BeginPackage["ConnorGray`NotebookWebsiteTools`Build`"]

makeAnchorContentSlug

notebookRelativeFileToURL

(*------------------------------------*)
(* State set during the build         *)
(*------------------------------------*)

$CurrentNotebook::usage = "$CurrentNotebook returns the Notebook expression of the notebook that is currently being processed."
$CurrentNotebookFile::usage = "$CurrentNotebookFile returns the absolute file path to the source notebook that is currently being built."
$CurrentNotebookWebsiteDirectory::usage = "$CurrentNotebookWebsiteDirectory returns the file path of the root directory of the notebook website that is currently being built."
$CurrentNotebookSupportFiles::usage = "$CurrentNotebookSupportFiles returns an Association containing the 'support files' that are used in the HTML generated for the notebook currently being built."

GeneralUtilities`SetUsage[AddSupportFile, "AddSupportFile[filename$, content$] adds content for a 'support' file that is used by the HTML for the current notebook being built."]

Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`"]
Needs["ConnorGray`NotebookWebsiteTools`LibraryLink`"]
Needs["ConnorGray`NotebookWebsiteTools`CurrentBuild`"]

Needs["ConnorGray`NotebookWebsiteTools`Utils`"]
Needs["ConnorGray`NotebookWebsiteTools`ErrorUtils`"]

(*====================================*)

$CurrentNotebook := RaiseError["Unexpected use of $CurrentNotebook: no notebook is currently being processed."]
$CurrentNotebookFile := RaiseError["Unexpected use of $CurrentNotebookFile: no notebook is currently being built."]
$CurrentNotebookWebsiteDirectory := RaiseError["Unexpected use of $CurrentNotebookWebsiteDirectory: no notebook website is currently being built."]
$CurrentNotebookSupportFiles := RaiseError["Unexpected use of $CurrentNotebookSupportFiles: no notebook is currently being built."]

(*====================================*)

NotebookWebsiteBuild[inputDir0: _?StringQ | File[_?StringQ]] := CatchRaised @ Module[{
	(* Note: Make sure inputDir is always StringQ, so that FileNameJoin works. *)
	inputDir = Replace[
		RaiseConfirm @ ExpandFileName[inputDir0],
		File[dir_?StringQ] :> dir
	],
	buildDir,
	contentDir,
	notebooks,
	htmlFiles
},
Block[{
	$CurrentNotebookWebsiteDirectory = Replace[inputDir, _?StringQ :> File[inputDir]]
},
	buildDir = FileNameJoin[{inputDir, "build"}];
	contentDir = FileNameJoin[{inputDir, "Content"}];

	RaiseAssert[StringQ[inputDir]];

	If[!DirectoryQ[contentDir],
		RaiseError["'Content' directory does not exist at expected location: ``",contentDir];
	];

	notebooks = FileNames["*.nb", contentDir, Infinity];

	(* TODO: Remove this check, clear the contents of the build directory if it
	         exists. (Note: clear the contents, don't delete the directory, as
			 a subtle UX choice )*)
	If[FileExistsQ[buildDir],
		If[DirectoryQ[buildDir],
			RaiseConfirm @ DeleteDirectory[buildDir, DeleteContents -> True];
			,
			RaiseError["File at build location is not a directory: ``", buildDir];
		];
	];

	RaiseConfirm @ CreateDirectory[buildDir];

	(*----------------------------*)
	(* Copy the common web assets *)
	(*----------------------------*)

	Module[{webAssetsSource},
		webAssetsSource = PacletObject["ConnorGray/NotebookWebsiteTools"]["AssetLocation", "web_assets"];

		RaiseAssert[
			DirectoryQ[webAssetsSource],
			"invalid NotebookWebsiteTools web_assets directory: ``",
			webAssetsSource
		];

		RaiseConfirm @ CopyDirectory[webAssetsSource, FileNameJoin[{buildDir, "web_assets"}]]
	];

	(*--------------------------*)
	(* Build the notebook files *)
	(*--------------------------*)

	htmlFiles = Map[
		nbFile |-> Module[{},
			WrapRaised["Error building notebook ``", nbFile][
				buildWebNotebook[nbFile, contentDir, buildDir]
			]
		],
		notebooks
	];

	RaiseAssert[MatchQ[htmlFiles, {File[_?StringQ]...}]];

	Success["NotebookWebsiteBuild", <|
		"ProcessedNotebooks" -> notebooks,
		(* NOTE(UX): These File[..] values are clickable in the FE, making it
			a quick and easy way for the caller of NotebookWebsiteBuild to open
			one of the built files. *)
		(* TODO: Sort this list using a heuristic for which is the most
			'general' type of document, so that the first is always most useful
			for the user to click, and is the best document for the `--open`
			CLI flag to open. See PacletDocumentationBuild[..] Success result
			sorting, which already has a similar heuristic. *)
		"OutputHTMLFiles" -> htmlFiles
	|>]
]]

(*======================================*)

buildWebNotebook[
	nbFile: _?StringQ,
	contentDir: _?StringQ,
	buildDir: _?StringQ
] := Module[{
	(* Relative path to the web_assets directory. *)
	(* TODO: Adjust this based on the URL of the notebook being processed. *)
	webAssetsLocation = "web_assets/",
	relativeWebAssetsLocation,
	nbFileRelative = RelativePath[contentDir, nbFile],
	nbUrl,
	htmlFile,
	metadata
},
Block[{
	$CurrentNotebook,
	(* Absolute file path to the source file of the notebook currently being
	   built. *)
	$CurrentNotebookFile = nbFile,
	(*
		An association of:

			File["relative/file/path.(png|gif|etc)"] -> (_?ImageQ | etc.)

		Do not modify this assocation directly. Instead use AddSupportFile.
	*)
	$CurrentNotebookSupportFiles = <||>
},
	RaiseAssert[StringQ[nbFileRelative]];

	nb = Replace[Get[File[nbFile]], {
		nb_Notebook :> nb,
		other_ :> RaiseError["Error importing notebook at ``: ``", nbFile, InputForm[other]]
	}];

	$CurrentNotebook = nb;

	metadata = Replace[Options[nb, TaggingRules], {
		KeyValuePattern[TaggingRules -> KeyValuePattern["ConnorGray/NotebookWebsiteTools" -> value_]] :> value,
		other_ :> RaiseError[
			"Notebook at `` does not have the expected TaggingRules needed to process web page notebook: ``",
			nbFileRelative,
			InputForm[other]
		]
	}];

	(* TODO: Use documentType? *)
	documentType = Replace[metadata, {
		KeyValuePattern["DocumentType" -> type_?StringQ] :> type,
		other_ :> RaiseError[
			"Notebook at `` does not have the expected metadata \"DocumentType\" field: ``",
			nbFileRelative,
			InputForm[other]
		]
	}];

	(*--------------------------------*)
	(* Convert the cells to HTML      *)
	(*--------------------------------*)

	elements = Flatten[convertToHtml[nb]];

	RaiseAssert[
		MatchQ[elements, {XMLElement[__]...}],
		"Unexpected elements structure: ``",
		InputForm[elements]
	];

	(*---------------------------------*)
	(* Generate the HTML output string *)
	(*---------------------------------*)

	nbUrl = notebookRelativeFileToURL[nbFileRelative];

	RaiseAssert[StringQ[nbUrl]];

	(* Determine the appropriate relative URL to point to the
	   web_assets directory. *)
	relativeWebAssetsLocation = URLBuild[{
		Replace[Length[URLParse[nbUrl, "Path"]], {
			(* The only component of the URL is the .nb file name itself, so
			   a relative path to web_assets doesn't need to go up any levels. *)
			1 -> Nothing,
			components_Integer :> StringRepeat["../", components - 1]
		}],
		webAssetsLocation
	}];

	html = XMLElement["html", {}, {
		XMLElement["head", {}, {
			XMLElement["link", {
				"rel" -> "stylesheet",
				"href" -> URLBuild[{relativeWebAssetsLocation, "notebook-website-default.css"}]
			}, {}]
		}],
		XMLElement["body", {}, {
			XMLElement["article", {"class" -> "Notebook"}, elements]
		}]
	}];

	htmlString = ExportString[html, "HTMLFragment"];

	htmlFile = FileNameJoin[{
		buildDir,
		StringReplace[nbFileRelative, ".nb" ~~ EndOfString -> ".html"]
	}];

	(*--------------------------------------------------------*)
	(* Create an appropriate parent directory for `htmlFile`. *)
	(*--------------------------------------------------------*)

	With[{parentDir = FileNameDrop[htmlFile]},
		Replace[FileType[parentDir], {
			(* Do nothing, we want this directory to exist. *)
			Directory -> {},
			None :> (
				RaiseConfirm @ CreateDirectory[
					parentDir,
					CreateIntermediateDirectories -> True
				];
			),
			type:File :> RaiseError[
				"Unable to export HTML file: path to parent directory of HTML file export file path is a non-directory type file: ``: type: ``",
				parentDir,
				type
			],
			other_ :> RaiseError[
				"Unexpected FileType during for expected HTML file parent directory: ``: ``",
				parentDir,
				other
			]
		}];
	];

	(*--------------------------------*)
	(* Write the support files        *)
	(*--------------------------------*)

	KeyValueMap[
		{relativeFilePath, value} |-> Module[{
			filePath
		},
			filePath = FileNameJoin[{
				FileNameDrop[htmlFile],
				Replace[relativeFilePath, File[s_?StringQ] :> s]
			}];

			RaiseAssert[StringQ[filePath]];

			RaiseConfirm @ Export[filePath, value];
		],
		$CurrentNotebookSupportFiles
	];

	(*--------------------------------*)
	(* Write the HTML to `htmlFile`   *)
	(*--------------------------------*)

	RaiseConfirm @ WriteString[htmlFile, htmlString];
	(* Close the file we just opened, to work around bug #348068. *)
	RaiseConfirm @ Close[File[htmlFile]];

	File[htmlFile]
]]

(*======================================*)

(*
	Title = h1
	Chapter = h2
	Section = h3
	Subsection = h4
	Subsubsection = h5
	Subsubsubsection = h6
	Subsubsubsubsection = ???
*)

convertToHtml[expr_] := Replace[expr, {
	Notebook[cells_?ListQ, options0___?OptionQ] :> (
		(* TODO: Handle relevant `options0`. *)
		Map[convertToHtml, cells]
	),

	(*================================*)
	(* Cells                          *)
	(*================================*)

	(* TODO(cleanup): Is this "class" -> "cell-group" used for anything? Is this
		<div> wrapper used for anything? Why not just flatten these inline? *)
	(* Cell[CellGroupData[cells_?ListQ, Open]] :> XMLElement["div", {"class" -> "cell-group"}, Map[convertToHtml, cells]], *)
	Cell[CellGroupData[cells_?ListQ, Open | Closed]] :> Map[convertToHtml, cells],

	(*--------------------------------*)
	(* Rasterized cell types          *)
	(*--------------------------------*)

	cell:Cell[_, "Input" | "Output", options0___?OptionQ] :> UsingFrontEnd @ Module[{
		$rasterResolution = 270,
		(* The native PPI resolution of the FrontEnd on this device. This is
		   typically 144 on HiDPI computers. *)
		$frontEndResolution,
		$frontEndScale,
		image,
		imageRelativeUrl,
		imageCSSPixelDimensions
	},
		{$frontEndResolution, $frontEndScale} = Replace[CurrentValue["ConnectedDisplays"], {
			{
				KeyValuePattern[{
					"Resolution" -> resolution_?NumberQ,
					"Scale" -> scale_?NumberQ
				}],
				___
			} :> {resolution, scale},
			other_ :> RaiseError[
				"unexpected \"ConnectedDisplays\" value: ``",
				InputForm[other]
			]
		}];

		RaiseAssert[NumberQ[$frontEndResolution]];

		RaiseAssert[
			$frontEndResolution == 144 || $frontEndResolution == 72,
			"unexpected FrontEnd resolution: ``", $frontEndResolution
		];

		image = Rasterize[
			cell,
			ImageResolution -> $rasterResolution,
			Background -> ColorConvert[Transparent, "RGB"]
		];

		RaiseAssert[ImageQ[image]];

		(*------------------------------------------------------------------*)
		(* Calculate the image dimensions in CSS pixels that will result in *)
		(* the cell image having the same physical on-screen size as when   *)
		(* viewed  in a notebook. When viewing the notebook next to the web *)
		(* page at the same magnification, the two should appear identical  *)
		(* in size.                                                         *)
		(*------------------------------------------------------------------*)

		(* These are the dimensions `image` would have if `image` was rasterized
		   at the default front end resolution. *)
		imageCSSPixelDimensions =
			ImageDimensions[image] / ($rasterResolution / $frontEndResolution);

		(* Account for the fact that HTML pixels are defined as 1/96th of an inch,
		   so they already compensate for the DPI scale; meaning we need to
		   divide the physical dimensions of the image by the scaling factor of
		   the FE they were rendered by. *)
		imageCSSPixelDimensions /= $frontEndScale;

		imageCSSPixelDimensions //= Round;

		RaiseAssert[MatchQ[imageCSSPixelDimensions, {_?IntegerQ, _?IntegerQ}]];

		(*-------------------------*)
		(* Return the HTML element *)
		(*-------------------------*)

		imageRelativeUrl = AddSupportFile[Automatic, image];

		XMLElement["img", {
			"src" -> imageRelativeUrl,
			"width" -> imageCSSPixelDimensions[[1]],
			"height" -> imageCSSPixelDimensions[[2]],
			"style" -> "display: block; padding: 4pt 0 4pt 0;"
		}, {}]
	],

	(*--------------------------------*)
	(* Converted cell types           *)
	(*--------------------------------*)

	Cell[content_, styles0___?StringQ, options0___?OptionQ] :> Module[{
		styles = {styles0},
		options = {options0},
		element
	},
		If[MemberQ[styles, "Excluded"],
			(* TODO: Better sentinel value for 'nothing' HTML? *)
			Return[{}, Module];
		];

		If[MemberQ[styles, "ComputedHTML"],
			Module[{context, heldExpr, xml},
				(*---------------------------------------------------------------*)
				(* Parse the typeset content of the cell into a held expression. *)
				(*---------------------------------------------------------------*)

				context = UniqueContext["NotebookWebsiteBuild"];

				heldExpr = Replace[content, {
					(* TODO: What if content is not StandardForm? *)
					BoxData[boxes_] :> (
						Block[{$Context = context, $ContextPath = {"System`"}},
							MakeExpression[boxes, StandardForm]
						]
					),
					(* TODO: What if content is not BoxData? *)
					TextData[_] :> RaiseError[
						"Unimplemented: evaluate \"ComputedHTML\" cells with TextData: ``",
						content
					],
					other_ :> RaiseError[
						"Malformed ComputedHTML cell: expected BoxData: ",
						InputForm[other]
					]
				}];

				xml = Block[{$Context = context, $ContextPath = {"System`"}},
					Replace[heldExpr, {
						(* The cell has multiple input expressions. Return the
							result of the last one. *)
						{__HoldComplete} :> Last[ReleaseHold[heldExpr]],
						_ :> ReleaseHold[heldExpr]
					}]
				];

				(*------------------------------------------------------------*)
				(* Validate the result of evaluating the "ComputedHTML" cell. *)
				(*------------------------------------------------------------*)

				Replace[xml, {
					XMLElement[_?StringQ, _?ListQ, _?ListQ] :> Null,
					_XMLElement :> RaiseError[
						"Malformed XMLElement returned from \"ComputedHTML\" cell: ``",
						InputForm[xml]
					],
					other_ :> RaiseError[
						"Expected evaluation of \"ComputedHTML\" to return XMLElement; got: ``",
						InputForm[other]
					]
				}];

				Return[xml];
			]
		];

		(*------------------------------------------------------------------------*)
		(* Assume this is a cell whose content can be converted directly to HTML. *)
		(*------------------------------------------------------------------------*)

		element = Fold[
			{html, style} |-> wrapHtmlForStyle[content, html, style],
			convertToHtml[content],
			styles
		];

		(* TODO: Handle the `options` as well. *)
		element
	],

	(*================================*)
	(* Cell data                      *)
	(*================================*)

	(*--------------------------------*)
	(* Text                           *)
	(*--------------------------------*)

	plainText_?StringQ :> HTMLEscape[plainText],

	TextData[inline_?ListQ] :> Map[convertToHtml, inline],
	TextData[content_] :> convertToHtml[content],

	StyleBox[content_, styles0___?StringQ, options0___?OptionQ] :> Module[{
		styles = {styles0},
		options = {options0},
		element
	},
		element = Fold[
			{elem, style} |-> Replace[style, {
				"Code" :> XMLElement["code", {}, {elem}],
				other_ :> RaiseError["unhandled StyleBox style: ``", InputForm[other]]
			}],
			convertToHtml[content],
			styles
		];

		element = Fold[
			{elem, option} |-> Replace[option, {
				(FontWeight -> weight_) :> Replace[weight, {
					"Bold" | Bold :> XMLElement["b", {}, {elem}],
					other_ :> RaiseError["unhandled FontWeight option value: ``", InputForm[weight]]
				}],
				(FontSlant -> slant_) :> Replace[slant, {
					"Italic" | Italic :> XMLElement["i", {}, {elem}],
					other_ :> RaiseError["unhandled FontSlant option value: ``", InputForm[slant]]
				}],
				(FontColor -> color_) :> Replace[color, {
					RGBColor[r_, g_, b_] :> XMLElement[
						"span",
						{"style" -> TemplateApply["color: rgb(``%, ``%, ``%)", IntegerPart[100 * {r, g, b}]]},
						{elem}
					],
					other_ :> RaiseError["unhandled FontColor option value: ``", InputForm[other]]
				}],
				(FontVariations -> {"StrikeThrough" -> True}) :> XMLElement[
					"span",
					{"style" -> "text-decoration: line-through"},
					{elem}
				],
				other_ :> RaiseError["unhandled StyleBox option value: ``", InputForm[other]]
			}],
			element,
			options
		];

		element
	],

	(*--------------------------------*)
	(* Boxes                          *)
	(*--------------------------------*)

	(* Handle hyperlinks. *)
	ButtonBox[
		content_,
		BaseStyle -> "Hyperlink",
		ButtonData -> {URL[url_?StringQ], None},
		ButtonNote -> _?StringQ
	] :> XMLElement["a", {"href" -> url}, {convertToHtml[content]}],

	other_ :> RaiseError["unhandled cell content: ``", InputForm[other]]
}]

AddUnmatchedArgumentsHandler[convertToHtml]

(*======================================*)

wrapHtmlForStyle[
	cellData_?CellDataQ,
	html_,
	style_?StringQ
] := Module[{},
	Replace[style, {
		(*===================================*)
		(* Headers with auto-anchor linking. *)
		(*===================================*)

		"Title" :> XMLElement["h1", {"class" -> "nb-Title"}, {makeAnchorLinkHtml[cellData, html]}],
		"Subtitle" :> XMLElement["p", {"class" -> "nb-Subtitle"}, {makeAnchorLinkHtml[cellData, html]}],
		"Chapter" :> XMLElement["h2", {"class" -> "nb-Chapter"}, {makeAnchorLinkHtml[cellData, html]}],
		"Section" :> XMLElement["h3", {"class" -> "nb-Section"}, {makeAnchorLinkHtml[cellData, html]}],
		"Subsection" :> XMLElement["h4", {"class" -> "nb-Subsection"}, {makeAnchorLinkHtml[cellData, html]}],
		"Subsubsection" :> XMLElement["h5", {"class" -> "nb-Subsubsection"}, {makeAnchorLinkHtml[cellData, html]}],
		"Subsubsubsection" :> XMLElement["h6", {"class" -> "nb-Subsubsubsection"}, {makeAnchorLinkHtml[cellData, html]}],

		(*===============*)
		(* Textual cells *)
		(*===============*)

		"Text" :> XMLElement["p", {}, {convertToHtml[cellData]}],

		"Item"
		| "ItemNumbered"
		| "ItemParagraph"
		| "Subitem"
		| "SubitemNumbered"
		| "SubitemParagraph"
		| "Subsubitem"
		| "SubsubitemNumbered"
		| "SubsubitemParagraph" :> XMLElement["div", {"class" -> StringJoin["nb-", style]}, {convertToHtml[cellData]}],

		(*============*)
		(* Code cells *)
		(*============*)

		"Program" :> XMLElement["pre", {"class" -> "nb-Program"}, {convertToHtml[cellData]}],

		(*===============*)
		(* Special cells *)
		(*===============*)

		"LiteralHTML" :> Module[{
			literalHTMLString,
			literalHTML
		},
			(* TODO: Option to ConvertToString that issues a warning if
				this contains non-plain-text content? *)
			literalHTMLString = ConvertToString[cellData];

			RaiseAssert[StringQ[literalHTMLString]];

			(* Parse the cell content into an XMLElement. This ensures
				that the resulting document doesn't have any syntax
				errors due to malformed HTML provided by the user. *)
			literalHTML = importHTMLFragment[literalHTMLString];

			literalHTML
		],

		"HighlightSyntax" :> Module[{
			syntaxString,
			highlightOptions,
			syntaxName,
			theme,
			syntaxHTMLString,
			syntaxHTML
		},
			(* TODO: Option to ConvertToString that issues a warning if
				this contains non-plain-text content? *)
			syntaxString = ConvertToString[cellData];

			RaiseAssert[StringQ[syntaxString]];

			highlightOptions = Replace[options, {
				KeyValuePattern[
					TaggingRules -> KeyValuePattern[
						"HighlightSyntaxOptions" -> value:(_?AssociationQ | {___?OptionQ})
					]
				] :> value,
				_ -> <||>
			}];

			(* TODO(polish): If using default for both syntax and theme,
				use a gray colored background instead of Solarized. Defaulting
				for both should look much like a Program cell. *)
			syntaxName = Lookup[highlightOptions, "Syntax", $DefaultSyntax];
			theme = Lookup[highlightOptions, "Theme", $DefaultTheme];
			lineNumbering = Lookup[highlightOptions, "LineNumbering", False];

			syntaxHTMLString = Replace[
				(* TODO(feature): Support theme argument here. *)
				$LibraryFunctions["highlight_to_html"][syntaxString, syntaxName, theme, lineNumbering],
				{
					highlightedHtml_?StringQ :> highlightedHtml,
					error_?FailureQ :> RaiseError[error],
					other_ :> RaiseError["Syntax highlighting returned unexpected result: ``", other]
				}
			];

			syntaxHTML = importHTMLFragment[syntaxHTMLString];

			syntaxHTML
		],

		other_ :> RaiseError["unhandled Cell style: ``: ``", InputForm[other], RawBoxes[cellData]]
	}]
]

AddUnmatchedArgumentsHandler[wrapHtmlForStyle]

(*======================================*)

(*
	Use this to add supporting files, like:

		* Images embedded in the notebook
		* Rasterized cells
		* TODO: Inline e.g. JS files written by the author

	The location of the support file in the built website is chosen
	automatically.

	This function will return a string containing the URL path to the support
	file that is relative to the URL of the notebook currently being built.
*)
AddSupportFile[
	name0: _?StringQ | Automatic,
	content: _?ImageQ
] := Module[{
	name = Replace[name0,
		Automatic :> ToString[Length[$CurrentNotebookSupportFiles]]
	],
	ext,
	filePath,
	urlPath
},
	RaiseAssert[
		!StringContainsQ[name, "/" | "\\"],
		"support file name contains file path separator character: ``", name
	];

	RaiseAssert[AssociationQ[$CurrentNotebookSupportFiles]];

	If[KeyMemberQ[$CurrentNotebookSupportFiles, name],
		RaiseError["support file with name `` has already been added.", InputForm[name]];
	];

	ext = Replace[content, {
		_?ImageQ :> ".png",
		other_ :> RaiseError["unsupported support file data: ``", other]
	}];

	(* FIXME: Validate name or encode so that `filePath` only contains URL-safe
		characters? *)
	(*
		Store support files in a directory that has the same name as the base
		name of the notebook they support.

		E.g. if the current notebook is kitchen-sink.nb, then the support file
		will be located at the file path `kitchen-sink/<name>.<ext>`
	*)
	filePath = FileNameJoin[{
		FileBaseName[$CurrentNotebookFile],
		name <> ext
	}];

	AssociateTo[$CurrentNotebookSupportFiles, File[filePath] -> content];

	(* Return a string containing the HTML relative path to this file. *)
	urlPath = URLBuild @ FileNameSplit[filePath];
	RaiseAssert[StringQ[urlPath]];
	urlPath
]

AddUnmatchedArgumentsHandler[AddSupportFile]

(*======================================*)

makeAnchorContentSlug[content_] := Module[{
	contentString = ConvertToString[content]
},
	RaiseAssert[StringQ[contentString]];

	StringReplace[contentString, {
		c:LetterCharacter :> ToLowerCase[c],
		WhitespaceCharacter.. -> "-",
		_ -> ""
	}]
]

AddUnmatchedArgumentsHandler[makeAnchorContentSlug]

(*======================================*)

makeAnchorLinkHtml[content_, html_] := Module[{
	contentString = ConvertToString[content],
	contentSlug
},
	RaiseAssert[StringQ[contentString]];

	contentSlug = makeAnchorContentSlug[contentString];

	RaiseAssert[StringQ[contentSlug]];

	RaiseAssert[
		MatchQ[html, _XMLElement | _?StringQ],
		"expected anchor link with label `` html value to be an XMLElement or string: ``",
		InputForm[contentString],
		InputForm[html]
	];

	XMLElement[
		"a",
		{
			(* TODO:
				Is there a way to make this work? GitHub prepends
				"user-content-" and anchor links work. How are they doing that?
				JavaScript?
			*)
			(* Note:
				Use a name that can't easily collide with other possible sources
				for element id's on this page.
			*)
			(* "id" -> "auto-anchor--" <> contentSlug, *)
			"id" -> contentSlug,
			"class" -> "anchor",
			"href" -> "#" <> contentSlug
		},
		{html}
	]
]

AddUnmatchedArgumentsHandler[makeAnchorLinkHtml]

(*======================================*)

importHTMLFragment[htmlString: _?StringQ] := Module[{},
	(* NOTE:
		Import using the {"HTML", "XMLObject"} format instead of "XML", because
		whitespace is not significant in generic XML, but it is in HTML. E.g.
		HTML with pre-formatted input (e.g. content in <pre> tags) is imported
		incorrectly as XML:

			ImportString["<pre>    leading spaces</pre>"]
				=> XMLElement["pre", {}, {"leading spaces"}]
	*)
	Replace[ImportString[htmlString, {"HTML", "XMLObject"}], {
		XMLObject["Document"][
			{XMLObject["Declaration"]["Version" -> "1.0", "Standalone" -> "yes"]},
			XMLElement[
				"html",
				{{"http://www.w3.org/2000/xmlns/", "xmlns"} -> "http://www.w3.org/1999/xhtml"},
				{
					XMLElement["body", {}, elements:{(_XMLElement | _String)...}]
				}
			],
			{}
		] :> Replace[elements, {
			(* TODO(polish): Support empty LiteralHTML cells. *)
			{} :> RaiseError["Unsupported empty LiteralHTML content: ``", InputForm[htmlString]],
			{one_} :> one,
			many:{__} :> RaiseError["Unsupported LiteralHTML cell with multiple top-level tags: ``", many]
		}],
		other_ :> RaiseError[
			"Imported HTML had unexpected format: ``: ``",
			InputForm @ Snippet[htmlString, 3],
			InputForm @ other
		]
	}]
]

AddUnmatchedArgumentsHandler[importHTMLFragment]


(*========================================================*)
(* URL Processing                                         *)
(*========================================================*)

notebookRelativeFileToURL[path_?StringQ] :=
	StringReplace[
		URLBuild[FileNameSplit[path]],
		".nb" ~~ EndOfString -> ".html"
	]

AddUnmatchedArgumentsHandler[notebookRelativeFileToURL]

(*========================================================*)


End[]

EndPackage[]