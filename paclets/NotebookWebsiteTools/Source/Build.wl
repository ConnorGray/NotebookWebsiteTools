BeginPackage["ConnorGray`NotebookWebsiteTools`Build`"]

makeAnchorContentSlug

notebookRelativeFileToURL

(*------------------------------------*)
(* State set during the build         *)
(*------------------------------------*)

$CurrentNotebook::usage = "$CurrentNotebook returns the Notebook expression of the notebook that is currently being processed."
$CurrentNotebookFile::usage = "$CurrentNotebookFile returns the absolute file path to the source notebook that is currently being built."
$CurrentNotebookRelativeURL::usage = "$CurrentNotebookRelativeURL returns the destination URL of the current source notebook relative to the destination URL of the notebook website content root."
$CurrentNotebookWebsiteDirectory::usage = "$CurrentNotebookWebsiteDirectory returns the file path of the root directory of the notebook website that is currently being built."
$CurrentNotebookSupportFiles::usage = "$CurrentNotebookSupportFiles returns an Association containing the 'support files' that are used in the HTML generated for the notebook currently being built."

GeneralUtilities`SetUsage[AddSupportFile, "AddSupportFile[filename$, content$] adds content for a 'support' file that is used by the HTML for the current notebook being built."]

GeneralUtilities`SetUsage[$BuildSettings, "
$BuildSettings is an association containing settings for the current build.
"]

$BuildCache
GetBuildValue

DetermineStatusAction

GeneralUtilities`SetUsage[ConvertToHtml, "\
ConvertToHtml[expr] converts a Notebook, Cell, or box expression into HTML.

The returned HTML expression will always be in one of the following forms:

	* XMLElement[...]
	* Splice[{___XMLElement}]
	* String
	* Nothing

such that the result of calling ConvertToHtml will always produce a
well-formed XMLElement when called in the 3rd argument list of XMLElement.

	VALID:

		XMLElement["p", {}, {ConvertToHtml[expr]}]

	INVALID:

		XMLElement["p", {}, ConvertToHtml[expr]]

	The latter is not a valid XMLElement.
"]

Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`"]
Needs["ConnorGray`NotebookWebsiteTools`LibraryLink`"]
Needs["ConnorGray`NotebookWebsiteTools`CurrentBuild`"]

Needs["ConnorGray`NotebookWebsiteTools`Utils`"]
Needs["ConnorGray`NotebookWebsiteTools`Errors`"]

Needs["ConnorGray`CacheUtils`"]

(*====================================*)

$CurrentNotebook := Raise[NotebookWebsiteError, "Unexpected use of $CurrentNotebook: no notebook is currently being processed."]
$CurrentNotebookFile := Raise[NotebookWebsiteError, "Unexpected use of $CurrentNotebookFile: no notebook is currently being built."]
$CurrentNotebookRelativeURL := Raise[NotebookWebsiteError, "Unexpected use of $CurrentNotebookRelativeURL: no notebook is currently being built."]
$CurrentNotebookWebsiteDirectory := Raise[NotebookWebsiteError, "Unexpected use of $CurrentNotebookWebsiteDirectory: no notebook website is currently being built."]
$CurrentNotebookSupportFiles := Raise[NotebookWebsiteError, "Unexpected use of $CurrentNotebookSupportFiles: no notebook is currently being built."]

$BuildSettings = <||>

$BuildCache := Raise[NotebookWebsiteError, "Unexpected use of $BuildCache: no build is currently in progress."]

(*====================================*)

Options[NotebookWebsiteBuild] = {
	"IncludeDrafts" -> False
}

NotebookWebsiteBuild[
	inputDir0 : _?StringQ | File[_?StringQ],
	buildDir0 : _?StringQ | Automatic : Automatic,
	OptionsPattern[]
] := Handle[_Failure] @ Module[{
	(* Note: Make sure inputDir is always StringQ, so that FileNameJoin works. *)
	inputDir = Replace[
		RaiseConfirm @ ExpandFileName[inputDir0],
		File[dir_?StringQ] :> dir
	],
	(* TODO: RaiseConfirmMatch[.., _?BooleanQ] this. *)
	includeDrafts = OptionValue["IncludeDrafts"],
	buildDir,
	contentDir,
	notebooks,
	htmlFiles
},
Block[{
	$BuildSettings = <|
		"IncludeDrafts" -> TrueQ[includeDrafts]
	|>,
	$BuildCache = CreateCache[],
	$CurrentNotebookWebsiteDirectory = Replace[inputDir, _?StringQ :> File[inputDir]]
},
	(* Build settings should not be mutable once the build has started. This
		ensures that cached values that depend on a particular setting don't
		become invalidated. *)
	Protect[$BuildSettings];

	populateBuildCacheHandlers[$BuildCache];

	(*--------------------------------*)

	buildDir = Replace[buildDir0, {
		s_?StringQ :> RaiseConfirm @ ExpandFileName[s],
		Automatic :> FileNameJoin[{inputDir, "build"}]
	}];

	(* TODO(cleanup): Use RaiseConfirm/RaiseConfirmMatch here. *)
	buildDir = Replace[CreateCacheDirectory[buildDir, DeleteContents -> True], {
		path_?StringQ :> path,
		err_Failure :> Return[err, Module],
		other_ :> Raise[NotebookWebsiteError, "Unexpected cache directory result: ``", InputForm[other]]
	}];

	contentDir = FileNameJoin[{inputDir, "Content"}];

	RaiseAssert[StringQ[inputDir]];

	If[!DirectoryQ[contentDir],
		Raise[NotebookWebsiteError, "'Content' directory does not exist at expected location: ``",contentDir];
	];

	notebooks = FileNames["*.nb", contentDir, Infinity];

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
			WrapRaised[NotebookWebsiteError, "Error building notebook ``", nbFile][
				buildWebNotebook[
					nbFile, contentDir, buildDir
				]
			]
		],
		notebooks
	];

	RaiseAssert[MatchQ[htmlFiles, {(File[_?StringQ] | Missing["Skipped", _])...}]];

	htmlFiles = DeleteCases[htmlFiles, Missing["Skipped", _]];

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

SetFallthroughError[populateBuildCacheHandlers]

populateBuildCacheHandlers[cache_CacheSpecifier] := Module[{},
	SetCacheHandler[cache, KeyPath[{file:File[_?StringQ], Notebook}] :> Handle[_Failure] @ Module[{
		result
	},
		result = Get[file];
		If[!MatchQ[result, _Notebook],
			Raise[
				NotebookWebsiteError,
				"Unexpected result getting Notebook from file ``: ``",
				InputForm[file],
				InputForm[result]
			]
		];

		result
	]];

	SetCacheHandler[cache, KeyPath[{file:File[_?StringQ], WebsiteNotebookStatus}] :> Handle[_Failure] @ Module[{
		nb = RaiseConfirm @ GetCacheValue[cache, {file, Notebook}]
	},
		WebsiteNotebookStatus[nb]
	]];

	SetCacheHandler[cache, KeyPath[{file:File[_?StringQ], WebsiteNotebookTitle}] :> Handle[_Failure] @ Module[{
		nb = RaiseConfirm @ GetCacheValue[cache, {file, Notebook}]
	},
		WebsiteNotebookTitle[nb]
	]];

	SetCacheHandler[cache, KeyPath[{file:File[_?StringQ], WebsiteNotebookSnippet}] :> Handle[_Failure] @ Module[{
		nb = RaiseConfirm @ GetCacheValue[cache, {file, Notebook}]
	},
		WebsiteNotebookSnippet[nb]
	]];
]

(*======================================*)

SetFallthroughError[buildWebNotebook]

buildWebNotebook[
	nbFile: _?StringQ,
	contentDir: _?StringQ,
	buildDir: _?StringQ
] := Module[{
	relativeWebAssetsLocation,
	nbFileRelative = RelativePath[contentDir, nbFile],
	nb,
	metadata,
	documentType,
	nbHtml,
	html,
	htmlString,
	htmlFile
},
Block[{
	$CurrentNotebook,
	(* Absolute file path to the source file of the notebook currently being
	   built. *)
	$CurrentNotebookFile = nbFile,
	$CurrentNotebookRelativeURL = notebookRelativeFileToURL[nbFileRelative],
	(*
		An association of:

			File["relative/file/path.(png|gif|etc)"] -> (_?ImageQ | etc.)

		Do not modify this assocation directly. Instead use AddSupportFile.
	*)
	$CurrentNotebookSupportFiles = <||>
},
	RaiseAssert[StringQ[nbFileRelative]];
	RaiseAssert[MatchQ[$CurrentNotebookRelativeURL, URL[_?StringQ]]];

	nb = Replace[GetBuildValue[{File[nbFile], Notebook}], {
		nb_Notebook :> nb,
		other_ :> Raise[NotebookWebsiteError, "Error importing notebook at ``: ``", nbFile, InputForm[other]]
	}];

	Replace[GetBuildValue[{File[nbFile], WebsiteNotebookStatus}], {
		status_?StringQ :> Replace[DetermineStatusAction[status], {
			(* Proceed normally. *)
			"Build" -> Null,
			(* Documents with this status should be skipped, so skip it. *)
			"Skip" :> Return[Missing["Skipped", status], Module],
			other_ :> Raise[NotebookWebsiteError, "Unhandled status action value: ``", InputForm[other]]
		}],
		(* All website notebooks should have their status set by their author. *)
		Missing["KeyAbsent", "DocumentStatus"] :> (
			Raise[NotebookWebsiteError, "Website notebook is missing a value for the DocumentStatus tagging rule."];
		),
		other_ :> Raise[NotebookWebsiteError, "Unexpected WebsiteNotebookStatus result: ``", InputForm[other]]
	}];

	$CurrentNotebook = nb;

	metadata = Replace[Options[nb, TaggingRules], {
		KeyValuePattern[TaggingRules -> KeyValuePattern["ConnorGray/NotebookWebsiteTools" -> value_]] :> value,
		other_ :> Raise[
			NotebookWebsiteError,
			"Notebook at `` does not have the expected TaggingRules needed to process web page notebook: ``",
			nbFileRelative,
			InputForm[other]
		]
	}];

	(* TODO: Use documentType? *)
	documentType = Replace[metadata, {
		KeyValuePattern["DocumentType" -> type_?StringQ] :> type,
		other_ :> Raise[
			NotebookWebsiteError,
			"Notebook at `` does not have the expected metadata \"DocumentType\" field: ``",
			nbFileRelative,
			InputForm[other]
		]
	}];

	(*--------------------------------*)
	(* Convert the cells to HTML      *)
	(*--------------------------------*)

	nbHtml = ConvertToHtml[nb];

	RaiseAssert[
		MatchQ[nbHtml, XMLElement["article", {"class" -> "Notebook"}, _List]],
		"Unexpected notebook HTML structure: ``",
		InputForm[nbHtml]
	];

	(*-----------------------------------*)
	(* Generate the symbolic HTML output *)
	(*-----------------------------------*)

	(* Determine the appropriate relative URL to point to the
	   web_assets directory. *)
	relativeWebAssetsLocation = notebookRelativeWebAssetsURL[$CurrentNotebookRelativeURL];

	html = XMLElement["html", {}, {
		XMLElement["head", {}, {
			XMLElement["link", {
				"rel" -> "stylesheet",
				"href" -> URLBuild[{relativeWebAssetsLocation, "notebook-website-default.css"}]
			}, {}],
			(* Make scaling work on mobile correctly. *)
			XML`RawXML["
				<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />
			"]
		}],
		XMLElement["body", {}, {nbHtml}]
	}];

	(*-------------------------------------------------------*)
	(* Verify that the symbolic XML structure is well-formed *)
	(*-------------------------------------------------------*)

	xmlErrors = XML`SymbolicXMLErrors[html];

	If[xmlErrors =!= {},
		(* TODO: Indicate this error in a better way. *)
		Scan[
			errorPos |-> (
				Print[
					"\tMalformed symbolic XML: ",
					InputForm[Extract[html, Drop[errorPos, -1]]]
				]
			),
			xmlErrors
		];

		(* TODO: Include XML object and error positions in failure metadata. *)
		Raise[NotebookWebsiteError, "Symbolic HTML contains errors."];
	];

	(*-----------------------------------------------*)
	(* Export the symbolic XML structure to a String *)
	(*-----------------------------------------------*)

	htmlString = ExportString[html, "XML"];

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
			type:File :> Raise[
				NotebookWebsiteError,
				"Unable to export HTML file: path to parent directory of HTML file export file path is a non-directory type file: ``: type: ``",
				parentDir,
				type
			],
			other_ :> Raise[
				NotebookWebsiteError,
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

SetFallthroughError[ConvertToHtml]

(*
*)
ConvertToHtml[expr_] := Replace[expr, {
	Notebook[cells_?ListQ, options0___?OptionQ] :> (
		(* TODO: Handle relevant `options0`. *)
		XMLElement[
			"article",
			{"class" -> "Notebook"},
			Map[ConvertToHtml, cells]
		]
	),

	(*================================*)
	(* Cells                          *)
	(*================================*)

	(* TODO(cleanup): Is this "class" -> "cell-group" used for anything? Is this
		<div> wrapper used for anything? Why not just flatten these inline? *)
	(* Cell[CellGroupData[cells_?ListQ, Open]] :> XMLElement["div", {"class" -> "cell-group"}, Map[convertToHtml, cells]], *)
	Cell[CellGroupData[cells_?ListQ, Open | Closed]] :> Splice @ Map[ConvertToHtml, cells],

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
			other_ :> Raise[
				NotebookWebsiteError,
				"Unexpected \"ConnectedDisplays\" value: ``",
				InputForm[other]
			]
		}];

		RaiseAssert[NumberQ[$frontEndResolution]];

		RaiseAssert[
			$frontEndResolution == 144 || $frontEndResolution == 72,
			"Unexpected FrontEnd resolution: ``", $frontEndResolution
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
			"width" -> ToString @ imageCSSPixelDimensions[[1]],
			"height" -> ToString @ imageCSSPixelDimensions[[2]],
			"style" -> "display: block; padding: 4pt 0 4pt 0;"
		}, {}]
	],

	(*--------------------------------*)
	(* Inline special link cells      *)
	(*--------------------------------*)

	Cell[
		BoxData @ TemplateBox[
			{label_, url_?StringQ},
			"ConnorGray/GitHubLink"
		],
		___?OptionQ
	] :> Module[{},
		XMLElement[
			"a",
			{"href" -> url, "class" -> "IconLink"},
			{
				XMLElement["img", {
					"src" -> URLBuild[{
						notebookRelativeWebAssetsURL[$CurrentNotebookRelativeURL],
						"github-mark.svg"
					}]
				}, {}],
				ConvertToHtml[label]
			}
		]
	],

	Cell[
		BoxData @ TemplateBox[
			{label_, url_?StringQ},
			"ConnorGray/PacletLink"
		],
		___?OptionQ
	] :> Module[{},
		XMLElement[
			"a",
			{"href" -> url, "class" -> "IconLink"},
			{
				XMLElement["img", {
					"src" -> URLBuild[{
						notebookRelativeWebAssetsURL[$CurrentNotebookRelativeURL],
						"paclet-icon.svg"
					}]
				}, {}],
				ConvertToHtml[label]
			}
		]
	],

	Cell[
		BoxData @ TemplateBox[
			{label_, url_?StringQ},
			"ConnorGray/RustCrateLink"
		],
		___?OptionQ
	] :> Module[{},
		XMLElement[
			"a",
			{"href" -> url, "class" -> "IconLink"},
			{
				XMLElement["img", {
					"src" -> URLBuild[{
						notebookRelativeWebAssetsURL[$CurrentNotebookRelativeURL],
						"rust-logo-blk.svg"
					}]
				}, {}],
				ConvertToHtml[label]
			}
		]
	],

	(*--------------------------------*)
	(* Converted cell types           *)
	(*--------------------------------*)

	Cell[content_, styles0___?StringQ, options0___?OptionQ] :> Module[{
		styles = {styles0},
		cellOptions = {options0},
		element
	},
		If[IntersectingQ[styles, {"Excluded", "HighlightSyntax", "LiteralHTML", "ComputedHTML"}],
			Raise[
				NotebookWebsiteError,
				"Cell has deprecated style: ``. Use cell style name prefixed with \"ConnorGray/\" instead.",
				InputForm[styles]
			];
		];

		If[MemberQ[styles, "ConnorGray/Excluded"],
			(* TODO: Better sentinel value for 'nothing' HTML? *)
			Return[Nothing, Module];
		];

		If[MemberQ[styles, "ConnorGray/Draft"] && !TrueQ[Lookup[$BuildSettings, "IncludeDrafts"]],
			(* TODO: Better sentinel value for 'nothing' HTML? *)
			Return[Nothing, Module];
		];

		If[MemberQ[styles, "ConnorGray/ComputedHTML"],
			Module[{inputLines, xml},
				(*---------------------------------------------------------------*)
				(* Parse the typeset content of the cell into a held expression. *)
				(*---------------------------------------------------------------*)

				inputLines = Replace[content, {
					BoxData[boxes0_] :> Replace[boxes0, b:Except[_?ListQ] :> {b}],
					(* TODO: What if content is not BoxData? *)
					TextData[_] :> Raise[
						NotebookWebsiteError,
						"Unimplemented: evaluate \"ComputedHTML\" cells with TextData: ``",
						content
					],
					other_ :> Raise[
						NotebookWebsiteError,
						"Malformed ComputedHTML cell: expected BoxData: ",
						InputForm[other]
					]
				}];

				RaiseAssert[ListQ[inputLines]];

				(* FIXME: Catch any raised exceptions from these ToExpression
					evaluation. *)
				xml = WrapRaised[
					NotebookWebsiteError,
					"Error evaluating ComputedHTML cell"
				] @ Block[{
					$Context = UniqueContext["NotebookWebsiteBuild"],
					$ContextPath = {"System`"}
				},
					Last @ Map[
						inputLine |-> ToExpression[inputLine],
						inputLines
					]
				];

				(*------------------------------------------------------------*)
				(* Validate the result of evaluating the "ComputedHTML" cell. *)
				(*------------------------------------------------------------*)

				Replace[xml, {
					XMLElement[_?StringQ, _?ListQ, _?ListQ] :> Null,
					_XMLElement :> Raise[
						NotebookWebsiteError,
						"Malformed XMLElement returned from \"ConnorGray/ComputedHTML\" cell: ``",
						InputForm[xml]
					],
					other_ :> Raise[
						NotebookWebsiteError,
						"Expected evaluation of \"ConnorGray/ComputedHTML\" to return XMLElement; got: ``",
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
			{html, style} |-> wrapHtmlForStyle[content, cellOptions, html, style],
			ConvertToHtml[content],
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

	plainText_?StringQ :> plainText,

	TextData[inline_?ListQ] :> Splice @ Map[ConvertToHtml, inline],
	TextData[content_] :> ConvertToHtml[content],

	StyleBox[content_, styles0___?StringQ, options0___?OptionQ] :> Module[{
		styles = {styles0},
		options = {options0},
		element
	},
		element = Fold[
			{elem, style} |-> Replace[style, {
				"Code" :> XMLElement["code", {}, {elem}],
				other_ :> Raise[NotebookWebsiteError, "Unhandled StyleBox style: ``", InputForm[other]]
			}],
			ConvertToHtml[content],
			styles
		];

		element = Fold[
			{elem, option} |-> Replace[option, {
				(FontWeight -> weight_) :> Replace[weight, {
					"Bold" | Bold :> XMLElement["b", {}, {elem}],
					other_ :> Raise[NotebookWebsiteError, "Unhandled FontWeight option value: ``", InputForm[weight]]
				}],
				(FontSlant -> slant_) :> Replace[slant, {
					"Italic" | Italic :> XMLElement["i", {}, {elem}],
					other_ :> Raise[NotebookWebsiteError, "Unhandled FontSlant option value: ``", InputForm[slant]]
				}],
				(FontColor -> color_) :> Replace[color, {
					RGBColor[r_, g_, b_] :> XMLElement[
						"span",
						{"style" -> TemplateApply["color: rgb(``%, ``%, ``%)", IntegerPart[100 * {r, g, b}]]},
						{elem}
					],
					other_ :> Raise[NotebookWebsiteError, "Unhandled FontColor option value: ``", InputForm[other]]
				}],
				(FontVariations -> {"StrikeThrough" -> True}) :> XMLElement[
					"span",
					{"style" -> "text-decoration: line-through"},
					{elem}
				],
				(Background -> color_) :> Replace[color, {
					RGBColor[r_, g_, b_] :> XMLElement[
						"span",
						{"style" -> TemplateApply["background: rgb(``%, ``%, ``%)", IntegerPart[100 * {r, g, b}]]},
						{elem}
					],
					other_ :> Raise[NotebookWebsiteError, "Unhandled Background option value: ``", InputForm[other]]
				}],
				other_ :> Raise[NotebookWebsiteError, "Unhandled StyleBox option value: ``", InputForm[other]]
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
	] :> XMLElement["a", {"href" -> url}, {ConvertToHtml[content]}],

	other_ :> Raise[NotebookWebsiteError, "Unhandled cell content: ``", InputForm[other]]
}]

(*======================================*)

wrapHtmlForStyle[
	cellData_?CellDataQ,
	cellOptions:{___?OptionQ},
	html_,
	style_?StringQ
] := Module[{},
	Replace[style, {
		(*===================================*)
		(* Headers with auto-anchor linking. *)
		(*===================================*)

		"Title" :> XMLElement["h1", {"class" -> "nb-Title"}, {makeAnchorLinkHtml[cellData, html]}],
		"Subtitle" :> XMLElement["p", {"class" -> "nb-Subtitle"}, {html}],
		"Chapter" :> XMLElement["h2", {"class" -> "nb-Chapter"}, {makeAnchorLinkHtml[cellData, html]}],
		"Section" :> XMLElement["h3", {"class" -> "nb-Section"}, {makeAnchorLinkHtml[cellData, html]}],
		"Subsection" :> XMLElement["h4", {"class" -> "nb-Subsection"}, {makeAnchorLinkHtml[cellData, html]}],
		"Subsubsection" :> XMLElement["h5", {"class" -> "nb-Subsubsection"}, {makeAnchorLinkHtml[cellData, html]}],
		"Subsubsubsection" :> XMLElement["h6", {"class" -> "nb-Subsubsubsection"}, {makeAnchorLinkHtml[cellData, html]}],

		(*===============*)
		(* Textual cells *)
		(*===============*)

		"Text" :> XMLElement["p", {}, {ConvertToHtml[cellData]}],

		"Item"
		| "ItemNumbered"
		| "ItemParagraph"
		| "Subitem"
		| "SubitemNumbered"
		| "SubitemParagraph"
		| "Subsubitem"
		| "SubsubitemNumbered"
		| "SubsubitemParagraph" :> XMLElement["div", {"class" -> StringJoin["nb-", style]}, {ConvertToHtml[cellData]}],

		(*============*)
		(* Code cells *)
		(*============*)

		"Program" :> XMLElement["pre", {"class" -> "nb-Program"}, {ConvertToHtml[cellData]}],

		(*===============*)
		(* Special cells *)
		(*===============*)

		"ConnorGray/LiteralHTML" :> Module[{
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

		"ConnorGray/HighlightSyntax" :> Module[{
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

			highlightOptions = Replace[cellOptions, {
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
					error_?FailureQ :> Raise[error],
					other_ :> Raise[NotebookWebsiteError, "Syntax highlighting returned unexpected result: ``", other]
				}
			];

			syntaxHTML = importHTMLFragment[syntaxHTMLString];

			syntaxHTML
		],

		"ConnorGray/Draft" :> Module[{},
			If[!TrueQ[Lookup[$BuildSettings, "IncludeDrafts"]],
				Raise[
					NotebookWebsiteError,
					"Uexpected attempt to convert cell marked as Draft: ``",
					cellData
				];
			];

			XMLElement["div", {"class" -> "nb-Draft"}, {html}]
		],

		other_ :> Raise[NotebookWebsiteError, "Unhandled Cell style: ``: ``", InputForm[other], RawBoxes[cellData]]
	}]
]

SetFallthroughError[wrapHtmlForStyle]

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
		Raise[NotebookWebsiteError, "Support file with name `` has already been added.", InputForm[name]];
	];

	ext = Replace[content, {
		_?ImageQ :> ".png",
		other_ :> Raise[NotebookWebsiteError, "Unsupported support file data: ``", other]
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

SetFallthroughError[AddSupportFile]

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

SetFallthroughError[makeAnchorContentSlug]

(*======================================*)

makeAnchorLinkHtml[content_, html_] := Module[{
	contentString = ConvertToString[content],
	contentSlug
},
	RaiseAssert[StringQ[contentString]];

	contentSlug = makeAnchorContentSlug[contentString];

	RaiseAssert[StringQ[contentSlug]];

	RaiseAssert[
		HTMLFragmentQ[html],
		"Anchor link with label `` has invalid HTML fragment: ``",
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

SetFallthroughError[makeAnchorLinkHtml]

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
					(* NOTE:
					    This is typically "body", but can be "head" when the
						parsed element is a custom <style>..</style> block *)
					XMLElement["body" | "head", {}, elements:{(_XMLElement | _String)...}]
				}
			],
			{}
		] :> ConfirmReplace[elements, {
			(* TODO(polish): Support empty LiteralHTML cells. *)
			{} :> Raise[NotebookWebsiteError, "Unsupported empty LiteralHTML content: ``", InputForm[htmlString]],
			{one_} :> one,
			many:{__} :> Raise[NotebookWebsiteError, "Unsupported LiteralHTML cell with multiple top-level tags: ``", many]
		}],
		other_ :> Raise[
			NotebookWebsiteError,
			"Imported HTML had unexpected format: ``: ``",
			InputForm @ Snippet[htmlString, 3],
			InputForm @ other
		]
	}]
]

SetFallthroughError[importHTMLFragment]

(*====================================*)

GeneralUtilities`SetUsage[DetermineStatusAction, "
	DetermineStatusAction[status$] returns the action to take for a given notebook
	'DocumentStatus' tagging rule value.

	This function returns one of a set of possible actions:

	* 'Build'
	* 'Skip'

	If the 'IncludeDrafts' option is set to True in $BuildSettings, then 'Draft'
	statuses will result in 'Build' instead of 'Skip'.
"]

SetFallthroughError[DetermineStatusAction]

Options[DetermineStatusAction] = {
	"IncludeDrafts" :> TrueQ[Lookup[$BuildSettings, "IncludeDrafts"]]
}

DetermineStatusAction[status_?StringQ, OptionsPattern[]] :=
	Replace[status, {
		"Published" -> "Build",

		(* If the document status is "Draft" and `"IncludeDrafts" -> True` option
			was set, then include this file. *)
		"Draft" /; TrueQ[OptionValue["IncludeDrafts"]] -> "Build",

		"Draft" | "Excluded" -> "Skip",
		other_ :> Raise[NotebookWebsiteError, "Unknown document status: ``", InputForm[other]]
	}]

(*========================================================*)

SetFallthroughError[GetBuildValue]

GetBuildValue[keyPath_List] := GetCacheValue[$BuildCache, KeyPath[keyPath]]


(*========================================================*)
(* URL Processing                                         *)
(*========================================================*)

notebookRelativeFileToURL[path_?StringQ] :=
	URL @ StringReplace[
		URLBuild[FileNameSplit[path]],
		".nb" ~~ EndOfString -> ".html"
	]

SetFallthroughError[notebookRelativeFileToURL]

(*====================================*)

SetFallthroughError[notebookRelativeWebAssetsURL]

notebookRelativeWebAssetsURL[nbUrl:URL[_?StringQ]] := Module[{},
	URL @ URLBuild[{
		Replace[Length[URLParse[nbUrl, "Path"]], {
			(* The only component of the URL is the .nb file name itself, so
			   a relative path to web_assets doesn't need to go up any levels. *)
			1 -> Nothing,
			components_Integer :> StringRepeat["../", components - 1]
		}],
		"web_assets"
	}]
]

(*========================================================*)


End[]

EndPackage[]