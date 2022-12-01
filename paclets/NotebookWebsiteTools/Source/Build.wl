BeginPackage["ConnorGray`NotebookWebsiteTools`Build`"]

Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`"]
Needs["ConnorGray`NotebookWebsiteTools`LibraryLink`"]
Needs["ConnorGray`NotebookWebsiteTools`Utils`"]
Needs["ConnorGray`NotebookWebsiteTools`ErrorUtils`"]

NotebookWebsiteBuild[inputDir0: _?StringQ | File[_?StringQ]] := CatchRaised @ Module[{
	inputDir = RaiseConfirm @ ExpandFileName[inputDir0],
	buildDir,
	contentDir,
	notebooks,
	htmlFiles
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
			buildWebNotebook[nbFile, contentDir, buildDir]
		],
		notebooks
	];

	RaiseAssert[MatchQ[htmlFiles, {File[_?StringQ]...}]];

	Success["NotebookWebsiteBuild", <|
		"ProcessedNotebooks" -> notebooks,
		(* NOTE(UX): These File[..] values are clickable in the FE, making it
			a quick and easy way for the caller of NotebookWebsiteBuild to open
			one of the built files. *)
		"OutputHTMLFiles" -> htmlFiles
	|>]
]

(*======================================*)

buildWebNotebook[
	nbFile: _?StringQ,
	contentDir: _?StringQ,
	buildDir: _?StringQ
] := Module[{
	(* Relative path to the web_assets directory. *)
	(* TODO: Adjust this based on the URL of the notebook being processed. *)
	webAssetsLocation = "web_assets/",
	nbFileRelative = RelativePath[contentDir, nbFile],
	htmlFile,
	metadata
},
	RaiseAssert[StringQ[nbFileRelative]];

	nb = Replace[Get[File[nbFile]], {
		nb_Notebook :> nb,
		other_ :> RaiseError["Error importing notebook at ``: ``", nbFile, InputForm[other]]
	}];

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

	elements = Flatten @ Map[
		convertToHtml,
		(* TODO: Validate this. *)
		nb[[1]]
	];

	RaiseAssert[
		MatchQ[elements, {XMLElement[__]...}],
		"Unexpected elements structure: ``",
		InputForm[elements]
	];

	(*--------------------------------*)
	(* Write out the HTML output      *)
	(*--------------------------------*)

	html = XMLElement["html", {}, {
		XMLElement["head", {}, {
			XMLElement["link", {
				"rel" -> "stylesheet",
				"href" -> URLBuild[{webAssetsLocation, "notebook-website-default.css"}]
			}, {}]
		}],
		XMLElement["body", {}, elements]
	}];

	htmlString = ExportString[html, "HTMLFragment"];

	htmlFile = FileNameJoin[{
		buildDir,
		StringReplace[nbFileRelative, ".nb" ~~ EndOfString -> ".html"]
	}];

	RaiseConfirm @ WriteString[htmlFile, htmlString];
	(* Close the file we just opened, to work around bug #348068. *)
	RaiseConfirm @ Close[File[htmlFile]];

	File[htmlFile]



	(* Construct an absolute URL equivalent of `nbFileRelative`. *)
	(* urlPath = "/" <> StringRiffle[
		URLEncode /@ FileNameSplit[nbFileRelative],
		"/"
	]; *)
	(* RaiseAssert[StringQ[urlPath]] *)
	(* Print["    URL: ", InputForm @ urlPath]; *)

]

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
	(*--------------------------------*)
	(* Cells                          *)
	(*--------------------------------*)

	(* TODO(cleanup): Is this "class" -> "cell-group" used for anything? Is this
		<div> wrapper used for anything? Why not just flatten these inline? *)
	(* Cell[CellGroupData[cells_?ListQ, Open]] :> XMLElement["div", {"class" -> "cell-group"}, Map[convertToHtml, cells]], *)
	Cell[CellGroupData[cells_?ListQ, Open]] :> Map[convertToHtml, cells],

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
					BoxData[_] :> (
						Block[{$Context = context, $ContextPath = {"System`"}},
							MakeExpression[content, StandardForm]
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
					ReleaseHold[heldExpr]
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
			{html, style} |-> Replace[style, {
				(*===================================*)
				(* Headers with auto-anchor linking. *)
				(*===================================*)

				"Title" :> XMLElement["h1", {}, {makeAnchorLinkHtml[content, html]}],
				"Subtitle" :> XMLElement["p", {"class" -> "nb-Subtitle"}, {makeAnchorLinkHtml[content, html]}],
				"Chapter" :> XMLElement["h2", {}, {makeAnchorLinkHtml[content, html]}],
				"Section" :> XMLElement["h3", {}, {makeAnchorLinkHtml[content, html]}],
				"Subsection" :> XMLElement["h4", {}, {makeAnchorLinkHtml[content, html]}],
				"Subsubsection" :> XMLElement["h5", {}, {makeAnchorLinkHtml[content, html]}],
				"Subsubsubsection" :> XMLElement["h6", {}, {makeAnchorLinkHtml[content, html]}],

				(*===============*)
				(* Textual cells *)
				(*===============*)

				"Text" :> XMLElement["p", {}, {convertToHtml[content]}],
				"Item" :> XMLElement["ul", {}, {XMLElement["li", {}, {convertToHtml[content]}]}],
				"ItemNumbered" :> XMLElement["ol", {}, {XMLElement["li", {}, {convertToHtml[content]}]}],
				"Subitem" :> XMLElement["ul", {}, {"\t", XMLElement["li", {}, {convertToHtml[content]}]}],

				(*============*)
				(* Code cells *)
				(*============*)

				"Program" :> XMLElement["pre", {"class" -> "nb-Program"}, {convertToHtml[content]}],

				(*===============*)
				(* Special cells *)
				(*===============*)

				"LiteralHTML" :> Module[{
					literalHTMLString,
					literalHTML
				},
					(* TODO: Option to ConvertToString that issues a warning if
						this contains non-plain-text content? *)
					literalHTMLString = ConvertToString[content];

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
					syntaxString = ConvertToString[content];

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
							html_?StringQ :> html,
							error_?FailureQ :> RaiseError[error],
							other_ :> RaiseError["Syntax highlighting returned unexpected result: ``", other]
						}
					];

					syntaxHTML = importHTMLFragment[syntaxHTMLString];

					syntaxHTML
				],

				other_ :> RaiseError["unhandled Cell style: ``: ``", InputForm[other], RawBoxes[content]]
			}],
			convertToHtml[content],
			styles
		];

		(* TODO: Handle the `options` as well. *)
		element
	],

	(*--------------------------------*)
	(* Text                           *)
	(*--------------------------------*)

	plainText_?StringQ :> plainText,
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

makeAnchorLinkHtml[content_, html_] := Module[{
	contentString = ConvertToString[content],
	contentSlug
},
	RaiseAssert[StringQ[contentString]];

	contentSlug = StringReplace[contentString, {
		c:LetterCharacter :> ToLowerCase[c],
		WhitespaceCharacter.. -> "-",
		_ -> ""
	}];

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


End[]

EndPackage[]