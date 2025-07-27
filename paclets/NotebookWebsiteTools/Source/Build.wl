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
FilteredCellQ

GeneralUtilities`SetUsage[ConvertToHTML, "
ConvertToHTML[expr] converts a Notebook, Cell, or box expression into HTML.

The returned HTML expression will always be in one of the following forms:

* XMLElement[...]
* Splice[{___XMLElement}]
* String
* Nothing

such that the result of calling ConvertToHTML will always produce a
well-formed XMLElement when called in the 3rd argument list of XMLElement.

VALID:

	XMLElement[\"p\", {}, {ConvertToHTML[expr]}]

INVALID:

	XMLElement[\"p\", {}, ConvertToHTML[expr]]

The latter is not a valid XMLElement.
"]

Begin["`Private`"]

Needs["ConnorGray`Utilities`"]
Needs["ConnorGray`Utilities`UI`" -> "CGUtilsUI`"]

Needs["ConnorGray`NotebookWebsiteTools`"]
Needs["ConnorGray`NotebookWebsiteTools`CurrentBuild`"]
Needs["ConnorGray`NotebookWebsiteTools`Configuration`"]

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
	(* The allowed build types are:

		* "Published" -- only includes files with the "Published" status

		* "Drafts" -- includes files and cells with the "Draft" status/style

		* "PreviewDraftsAsPublished" includes files marked as "Draft" but not
		  cells marked as Draft. Used by the 'Preview > Published' UI buttom.

		  Pretend that the document is in the "Published" status, so that
		  using the 'Preview /> Published' menu item shows the state of the
		  document "as if" it was Published (even if the document as a whole is
		  still in "Draft" mode) in the current state, with Draft cells not
		  included.
	*)
	"BuildType" -> "Published",

	"FileFilterFunction" -> None,

	(*
		Whether images derived form notebook content should be embedded
		directory in the HTML, or generated as separate files that are linked
		to.

		Embedding images makes it easier to distribute stand-alone HTML.
	*)
	"EmbedImages" -> False,
	"EmbedCSS" -> False
}

NotebookWebsiteBuild[
	inputDir0 : _?StringQ | File[_?StringQ],
	buildDir0 : _?StringQ | Automatic : Automatic,
	OptionsPattern[]
] := Handle[_Failure] @ Module[{
	(* Note: Make sure inputDir is always StringQ, so that FileNameJoin works. *)
	inputDir = Replace[
		RaiseConfirm @ ExpandFileName[inputDir0],
		File[dir: _?StringQ] :> dir
	],
	buildDir,
	contentDir,
	fileSystemTree,
	fileFilterFunction = OptionValue["FileFilterFunction"],
	notebooks,
	htmlFiles
},
Block[{
	$BuildSettings = <|
		"BuildType" -> OptionValue["BuildType"],
		(* TODO: RaiseConfirmMatch[.., _?BooleanQ] this. *)
		"EmbedImages" -> TrueQ[OptionValue["EmbedImages"]],
		"EmbedCSS" -> TrueQ[OptionValue["EmbedCSS"]],
		(* Initialized below if the notebook website has a valid
			NotebookWebsite.wl configuration file. *)
		"Configuration" -> <||>
	|>,
	$BuildCache = CreateCache[],
	$CurrentNotebookWebsiteDirectory = Replace[inputDir, _?StringQ :> File[inputDir]]
},
	(*================================*)
	(* Setup global build state       *)
	(*================================*)

	Module[{
		configFile = FileNameJoin[{inputDir, "NotebookWebsite.wl"}],
		config
	},
		If[FileExistsQ[configFile],
			If[FileType[configFile] =!= File,
				Raise[
					NotebookWebsiteError,
					"Expected configuration file with name `` to be a normal file, got ``",
					InputForm[configFile],
					FileType[configFile]
				];
			];

			config = WrapRaised[
				NotebookWebsiteError,
				"Unexpected error evaluating website configuration file at ``",
				InputForm[configFile]
			][
				(* Load the configuration file using a consistent symbol table
					environment. *)
				Block[{
					$Context = UniqueContext["NotebookWebsiteConfiguration"],
					$ContextPath = {"System`"}
				},
					Needs["ConnorGray`NotebookWebsiteTools`Configuration`"];

					Get[configFile]
				]
			];

			ConfirmReplace[config, {
				NotebookWebsite[assoc: _?AssociationQ] :> (
					$BuildSettings["Configuration"] = assoc;
				),
				other: _ :> Raise[
					NotebookWebsiteError,
					"Website configuration file must contain a "
					<> "NotebookWebsite[<| ... |>] object. Got: ``",
					InputForm[other]
				]
			}]
		];
	];

	(* Build settings should not be mutable once the build has started. This
		ensures that cached values that depend on a particular setting don't
		become invalidated. *)
	Protect[$BuildSettings];

	populateBuildCacheHandlers[$BuildCache];

	(*--------------------------------*)

	buildDir = Replace[buildDir0, {
		s: _?StringQ :> RaiseConfirm @ ExpandFileName[s],
		Automatic :> FileNameJoin[{inputDir, "build"}]
	}];

	(* TODO(cleanup): Use RaiseConfirm/RaiseConfirmMatch here. *)
	buildDir = Replace[CreateCacheDirectory[buildDir, DeleteContents -> True], {
		path: _?StringQ :> path,
		err: _Failure :> Return[err, Module],
		other: _ :> Raise[NotebookWebsiteError, "Unexpected cache directory result: ``", InputForm[other]]
	}];

	contentDir = FileNameJoin[{inputDir, "Content"}];

	RaiseAssert[StringQ[inputDir]];

	ConfirmFileType[
		contentDir,
		Directory,
		"'Content' directory does not exist at expected location"
	];

	fileSystemTree = FileSystemTree[contentDir];

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

	notebooks = {};
	htmlFiles = {};

	TreeMap[
		(* FIXME: Don't visit / ignore files that match .gitignore by default. *)
		filePath |-> Catch @ WrapRaised[
			NotebookWebsiteError,
			"Error processing file ``",
			filePath
		] @ Module[{
			relativePath = RelativePath[contentDir, filePath]
		},
			If[FileType[filePath] === Directory,
				Throw[Null];
			];

			If[fileFilterFunction =!= None, Module[{
				filterResult = fileFilterFunction[<|
					"RelativePath" -> relativePath
				|>]
			},
				ConfirmReplace[filterResult, {
					True -> None,
					(* Skip this file. *)
					False :> Throw[Null],
					other: _ :> Raise[
						NotebookWebsiteError,
						<|
							"FileFilterFunction" -> fileFilterFunction,
							"FilterResult" -> filterResult
						|>,
						"Function specified by 'FileFilterFunction' option did not return a boolean value."
					]
				}];
			]];

			ConfirmFileType[filePath, File];

			ConfirmReplace[FileExtension[filePath], {
				"nb" :> Module[{htmlFile},
					htmlFile = buildWebNotebook[
						filePath, contentDir, buildDir
					];
					AppendTo[notebooks, filePath];
					AppendTo[htmlFiles, htmlFile];
				],
				(* NOTE: Any other file without a recognized extension gets
					copied to the output directory unchanged. *)
				_?StringQ | None :> Catch @ Module[{
					destPath
				},
					(* TID:250330/1: Don't copy hidden files to build output. *)
					If[HiddenFileNameQ[relativePath],
						Throw[Null];
					];

					RaiseAssert[StringQ[relativePath]];
					destPath = FileNameJoin[{buildDir, relativePath}];
					(* FIXME: Test: Could happen if source is:
							foo.nb
							foo.html
						We should issue a descriptive error message explaining
						that a copied file conflicted with a converted file.
						(Similarly if a converted/copied file conflicts with
						a generated support file.)
					*)
					ConfirmFileType[
						destPath, None,
						"File already exists at destination path."
					];
					(* TID:250329/1: Create parent directory of copied file. *)
					ConfirmFileType[
						FileNameDrop[destPath], None | Directory,
						"Copied file destination parent path exists and is not a directory."
					];
					If[!FileExistsQ[FileNameDrop[destPath]],
						RaiseConfirm @ CreateDirectory[
							FileNameDrop[destPath],
							CreateIntermediateDirectories -> True
						];
					];
					RaiseConfirm @ CopyFile[filePath, destPath];
				],
				other: _ :> Raise[
					NotebookWebsiteError,
					<| "FilePath" -> filePath |>,
					"Unexpected file extension result `` for file in notebook website content directory.",
					other
				]
			}]
		],
		fileSystemTree
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

populateBuildCacheHandlers[cache: _CacheSpecifier] := Module[{},
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

	SetCacheHandler[cache, KeyPath[{file:File[_?StringQ], WebsiteNotebookTags}] :> Handle[_Failure] @ Module[{
		nb = RaiseConfirm @ GetCacheValue[cache, {file, Notebook}]
	},
		WebsiteNotebookTags[nb]
	]];
]

(*======================================*)

SetFallthroughError[buildWebNotebook]

buildWebNotebook[
	nbFile: _?StringQ,
	contentDir: _?StringQ,
	buildDir: _?StringQ
] := Module[{
	nbFileRelative = RelativePath[contentDir, nbFile],
	nb,
	metadata,
	documentType,
	nbHtml,
	htmlHead,
	htmlBody,
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
		nb: _Notebook :> nb,
		other: _ :> Raise[NotebookWebsiteError, "Error importing notebook at ``: ``", nbFile, InputForm[other]]
	}];

	Replace[GetBuildValue[{File[nbFile], WebsiteNotebookStatus}], {
		status: _?StringQ :> Replace[DetermineStatusAction[status], {
			(* Proceed normally. *)
			"Build" -> Null,
			(* Documents with this status should be skipped, so skip it. *)
			"Skip" :> Return[Missing["Skipped", status], Module],
			other: _ :> Raise[NotebookWebsiteError, "Unhandled status action value: ``", InputForm[other]]
		}],
		(* All website notebooks should have their status set by their author. *)
		Missing["KeyAbsent", "DocumentStatus"] :> (
			Raise[NotebookWebsiteError, "Website notebook is missing a value for the DocumentStatus tagging rule."];
		),
		other: _ :> Raise[NotebookWebsiteError, "Unexpected WebsiteNotebookStatus result: ``", InputForm[other]]
	}];

	$CurrentNotebook = nb;

	metadata = Replace[Options[nb, TaggingRules], {
		KeyValuePattern[TaggingRules -> KeyValuePattern[
			"ConnorGray/NotebookWebsiteTools" -> value: _
		]] :> value,
		other: _ :> Raise[
			NotebookWebsiteError,
			"Notebook at `` does not have the expected TaggingRules needed to process web page notebook: ``",
			nbFileRelative,
			InputForm[other]
		]
	}];

	(* TODO: Use documentType? *)
	documentType = Replace[metadata, {
		KeyValuePattern["DocumentType" -> type: _?StringQ] :> type,
		other: _ :> Raise[
			NotebookWebsiteError,
			"Notebook at `` does not have the expected metadata \"DocumentType\" field: ``",
			nbFileRelative,
			InputForm[other]
		]
	}];

	(*--------------------------------*)
	(* Convert the cells to HTML      *)
	(*--------------------------------*)

	nbHtml = ConvertToHTML[nb];

	RaiseAssert[
		MatchQ[nbHtml, XMLElement["article", {"class" -> "Notebook"}, _List]],
		"Unexpected notebook HTML structure: ``",
		InputForm[nbHtml]
	];

	(*-----------------------------------*)
	(* Generate the symbolic HTML output *)
	(*-----------------------------------*)

	(* Default HTML <head> and <body> content. *)
	htmlHead = {
		(* Make scaling work on mobile correctly. *)
		XML`RawXML["
			<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />
		"]
	};
	htmlBody = {
		nbHtml
	};

	ConfirmReplace[Lookup[$BuildSettings, "EmbedCSS"], {
		False :> Module[{
			(* Determine the appropriate relative URL to point to the
			web_assets directory. *)
			relativeWebAssetsLocation = notebookRelativeWebAssetsURL[
				$CurrentNotebookRelativeURL
			]
		},
			AppendTo[
				htmlHead,
				XMLElement["link", {
					"rel" -> "stylesheet",
					"href" -> URLBuild[{relativeWebAssetsLocation, "notebook-website-default.css"}]
				}, {}]
			];
		],
		True :> Module[{
			webAssetsSource = PacletObject["ConnorGray/NotebookWebsiteTools"][
				"AssetLocation",
				"web_assets"
			],
			cssContents
		},
			cssContents = RaiseConfirm @ Import[
				FileNameJoin[{webAssetsSource, "notebook-website-default.css"}],
				"String"
			];

			RaiseAssert[StringQ[cssContents]];

			PrependTo[htmlBody, XMLElement["style", {}, {XML`RawXML[cssContents]}]];
		]
	}];

	html = XMLElement["html", {}, {
		XMLElement["head", {}, htmlHead],
		XMLElement["body", {}, htmlBody]
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
			other: _ :> Raise[
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

	(* TODO: Try using FileSystemBundleExport here to simplfy this logic:
		RaiseConfirm @ FileSystemBundleExport[
			FileNameDrop[htmlFile],
			FileSystemBundle[$CurrentNotebookSupportFiles]
		];
	*)

	KeyValueMap[
		{relativeFilePath, value} |-> Module[{
			filePath
		},
			filePath = FileNameJoin[{
				FileNameDrop[htmlFile],
				Replace[relativeFilePath, File[s: _?StringQ] :> s]
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

SetFallthroughError[ConvertToHTML]

(*
*)
ConvertToHTML[expr0: _] := Replace[expr0, {
	Notebook[cells: _?ListQ, options0: ___?OptionQ] :> (
		(* TODO: Handle relevant `options0`. *)
		XMLElement[
			"article",
			{"class" -> "Notebook"},
			Map[ConvertToHTML, cells]
		]
	),

	(*================================*)
	(* Cells                          *)
	(*================================*)

	(*--------------------------------*)
	(* Cell Groups                    *)
	(*--------------------------------*)

	(* TID:240809/2: Handle ConnorGray/TabViewSection cell group. *)
	Cell @ CellGroupData[
		{
			(* NOTE: The main tab view header just kind of evaporates(?),
				the tab labels become descriptive. *)
			tabSectionHeader:Cell[
				_,
				stylesSeq: ___?StringQ /;
					MemberQ[{stylesSeq}, "ConnorGray/TabViewSection"],
				___?OptionQ
			],
			tabContentsSeq: __Cell
		},
		Open | Closed | {_?IntegerQ}
	] :> (
		(* Note: Warn about misleading filters applied to tab section header
			cells. The cells themselves are not converted anyway, and the header
			being excluded does not prevent the overall tab contents from being
			included. *)
		If[FilteredCellQ[tabSectionHeader],
			Print["warning: Applying filtered style to tab view section header "
				<> "cell does nothing."];
		];

		createTabViewSectionHTML[{tabContentsSeq}]
	),

	(* TODO(cleanup): Is this "class" -> "cell-group" used for anything? Is this
		<div> wrapper used for anything? Why not just flatten these inline? *)
	(* Cell[CellGroupData[cells_?ListQ, Open]] :> XMLElement["div", {"class" -> "cell-group"}, Map[convertToHtml, cells]], *)
	Cell[CellGroupData[
		cells: _?ListQ,
		Open | Closed | {_?IntegerQ}
	]] :> Splice @ Map[ConvertToHTML, cells],

	cell: Cell[_CellGroupData, ___] :> (
		Raise[NotebookWebsiteError, "Unhandled cell group: ``", InputForm[cell]]
	),

	(*--------------------------------*)
	(* Deprecated cells               *)
	(*--------------------------------*)

	Cell[
		_,
		stylesSeq: ___?StringQ,
		___?OptionQ
	] /; IntersectingQ[
		{stylesSeq},
		{"Excluded", "HighlightSyntax", "LiteralHTML", "ComputedHTML"}
	] :> (
		Raise[
			NotebookWebsiteError,
			"Cell has deprecated style: ``. Use cell style name prefixed with \"ConnorGray/\" instead.",
			InputForm[styles]
		];
	),

	(*--------------------------------*)
	(* Flagged cells                  *)
	(*--------------------------------*)

	(* TID:240601/3: Excluded applied to textual (converted) cell *)
	(* TID:240601/4: Excluded applied to box (rasterized) cell *)
	(* Always remove Excluded cells *)
	(* TID:240601/1: Draft applied to textual _converted_ cell *)
	(* TID:240601/2: Draft applied to box (rasterized) cell *)
	cell: _Cell /; FilteredCellQ[cell] :> (
		(* TODO: Better sentinel value for 'nothing' HTML? *)
		Nothing
	),

	(*--------------------------------*)
	(* Rasterized cell types          *)
	(*--------------------------------*)

	cell:Cell[
		_,
		primaryCellStyle:(
			"Input" | "Output"
		),
		(* FIXME: Handle these secondary styles *)
		secondaryStylesSeq: ___?StringQ,
		options0: ___?OptionQ
	] :> UsingFrontEnd @ Module[{
		image,
		imageCSSPixelDimensions
	},
		{image, imageCSSPixelDimensions} = Rasterize2[
			cell,
			{"Image", "CSSPixelSize"}
		];

		imageUrl = ConfirmReplace[Lookup[$BuildSettings, "EmbedImages", False], {
			False :> (
				AddSupportFile[primaryCellStyle, image]
			),
			True :> (
				StringJoin[
					"data:image/png;base64,",
					BaseEncode[ExportByteArray[image, "PNG"]]
				]
			)
		}];

		XMLElement["img", {
			"src" -> imageUrl,
			"width" -> ToString @ imageCSSPixelDimensions[[1]],
			"height" -> ToString @ imageCSSPixelDimensions[[2]],
			"style" -> "display: block; padding: 4pt 0 4pt 0;"
		}, {}]
	],

	(*--------------------------------*)
	(* Special conversion cell types  *)
	(*--------------------------------*)

	(* TID:250305/1: Basic conversion of ComputedHTML cells. *)
	cell: Cell[
		cdata: _,
		"ConnorGray/ComputedHTML",
		secondaryStylesSeq: ___?StringQ,
		options0: ___?OptionQ
	] :> Module[{
		inputLines, expr, xml
	},
		If[DeleteCases[{secondaryStylesSeq}, "ConnorGray/Draft"] =!= {},
			Raise[
				NotebookWebsiteError,
				<| "Cell" ->  cell |>,
				"Unimplemented: support secondary styles on ComputedHTML cells"
			];
		];

		(*------------------------------------------------*)
		(* Parse and evaluate the box content of the cell *)
		(*------------------------------------------------*)

		inputLines = CellDataInputLines[cdata];

		RaiseAssert[ListQ[inputLines]];

		(* FIXME: Catch any raised exceptions from these ToExpression
			evaluation. *)
		expr = WrapRaised[
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

		(*--------------------------------*)
		(* Convert the expression to HTML *)
		(*--------------------------------*)

		(* TID:250308/1: Convert ConnorGray/ComputedHTML cells using MakeHTML. *)
		xml = WrapRaised[
			NotebookWebsiteError,
			"Error using MakeHTML[..] to convert ComputedHTML style cell expression: ``",
			expr
		] @ ConfirmReplace[MakeHTML[expr], {
			frag: _?HTMLFragmentQ :> frag,
			other: _ :> Raise[
				NotebookWebsiteError,
				<|
					"Expression" -> InputForm[expr],
					"MakeHTMLResult" -> InputForm[other]
				|>,
				"Expected evaluation of MakeHTML[..] to return a symbolic HTML fragment."
			]
		}];

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
			other: _ :> Raise[
				NotebookWebsiteError,
				"Expected evaluation of \"ConnorGray/ComputedHTML\" to return XMLElement; got: ``",
				InputForm[other]
			]
		}];

		xml
	],

	(*--------------------------------*)
	(* Converted cell types           *)
	(*--------------------------------*)

	(* Assume this is a cell whose content can be converted directly to HTML. *)
	cell0: Cell[
		content: _,
		styles0: __?StringQ,
		options0: ___?OptionQ
	] :> WrapRaised[
		NotebookWebsiteError,
		"Error converting `` style cell: ``",
		InputForm[First[{styles0}]],
		cell0
	] @ Module[{
		styles = {styles0},
		cellOptions = {options0},
		element
	},
		wrapHtmlForStyles[
			content,
			ConvertToHTML[content],
			styles,
			cellOptions
		]
	],

	(*================================*)
	(* Cell data                      *)
	(*================================*)

	(*--------------------------------*)
	(* Text                           *)
	(*--------------------------------*)

	plainText: _?StringQ :> plainText,

	TextData[inline: _?ListQ] :> Splice @ Map[ConvertToHTML, inline],
	TextData[content: _] :> ConvertToHTML[content],

	StyleBox[content: _, styles0: ___?StringQ, options0: ___?OptionQ] :> Module[{
		styles = {styles0},
		options = {options0},
		element
	},
		element = Fold[
			{elem, style} |-> Replace[style, {
				(* TID:240602/1: Inline "Code" or "Program" StyleBox's *)
				"Code" | "Program" :> XMLElement["code", {}, {elem}],
				(* TID:240602/2: Unrecognized style in textual cell StyleBox. *)
				other: _ :> Raise[NotebookWebsiteError, "Unhandled StyleBox style: ``", InputForm[other]]
			}],
			ConvertToHTML[content],
			styles
		];

		element = Fold[
			{elem, option} |-> Replace[option, {
				(FontWeight -> weight: _) :> Replace[weight, {
					"Bold" | Bold :> XMLElement["b", {}, {elem}],
					other: _ :> Raise[NotebookWebsiteError, "Unhandled FontWeight option value: ``", InputForm[weight]]
				}],
				(FontSlant -> slant: _) :> Replace[slant, {
					"Italic" | Italic :> XMLElement["i", {}, {elem}],
					other: _ :> Raise[NotebookWebsiteError, "Unhandled FontSlant option value: ``", InputForm[slant]]
				}],
				(FontColor -> color: _) :> Replace[color, {
					RGBColor[r: _, g: _, b: _] :> XMLElement[
						"span",
						{"style" -> TemplateApply["color: rgb(``%, ``%, ``%)", IntegerPart[100 * {r, g, b}]]},
						{elem}
					],
					(* TID:240526/1: FontColor -> GrayLevel[..] handling. *)
					GrayLevel[value: _?NumberQ] :> XMLElement[
						"span",
						{"style" -> TemplateApply[
							"color: rgb(``%, ``%, ``%)",
							Table[100 * value, 3]
						]},
						{elem}
					],
					other: _ :> Raise[NotebookWebsiteError, "Unhandled FontColor option value: ``", InputForm[other]]
				}],
				(FontSize -> size: _) :> ConfirmReplace[size, {
					_?IntegerQ :> XMLElement[
						"span",
						{"style" -> TemplateApply["font-size: ``pt", size]},
						{elem}
					],
					other: _ :> Raise[
						NotebookWebsiteError,
						"Unhandled FontSize option value: ``",
						InputForm[other]
					]
				}],
				(FontVariations -> {"StrikeThrough" -> True}) :> XMLElement[
					"span",
					{"style" -> "text-decoration: line-through"},
					{elem}
				],
				(* TID:250612/1: FontVariations -> {"Underline" -> True} handling. *)
				(FontVariations -> {"Underline" -> True}) :> XMLElement[
					"u",
					{},
					{elem}
				],
				(Background -> color: _) :> Replace[color, {
					RGBColor[r: _, g: _, b: _] :> XMLElement[
						"span",
						{"style" -> TemplateApply["background: rgb(``%, ``%, ``%)", IntegerPart[100 * {r, g, b}]]},
						{elem}
					],
					other: _ :> Raise[NotebookWebsiteError, "Unhandled Background option value: ``", InputForm[other]]
				}],
				other: _ :> Raise[NotebookWebsiteError, "Unhandled StyleBox option value: ``", InputForm[other]]
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
		content: _,
		BaseStyle -> "Hyperlink",
		ButtonData -> {URL[url: _?StringQ], None},
		ButtonNote -> _?StringQ
	] :> XMLElement["a", {"href" -> url}, {ConvertToHTML[content]}],

	(*--------------------------------*)
	(* Inline special link cells      *)
	(*--------------------------------*)

	Cell[
		BoxData @ TemplateBox[
			{label: _, url: _?StringQ},
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
				ConvertToHTML[label]
			}
		]
	],

	(* TID:240602/3: Convert inline PacletLink special link *)
	Cell[
		BoxData @ TemplateBox[
			{label: _, url: _?StringQ},
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
				ConvertToHTML[label]
			}
		]
	],

	Cell[
		BoxData @ TemplateBox[
			{label: _, url: _?StringQ},
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
				ConvertToHTML[label]
			}
		]
	],

	other: _ :> Raise[
		NotebookWebsiteError,
		<| "Expression" -> InputForm[other] |>,
		"Unhandled cell style or content."
	]
}]

(*======================================*)

SetFallthroughError[wrapHtmlForStyles]

wrapHtmlForStyles[
	cellData: _?CellDataQ,
	initialHTML: _?HTMLFragmentQ,
	cellStyles:{___?StringQ},
	cellOptions:{___?OptionQ}
] := Module[{
	element
},
	element = Fold[
		{html, style} |-> wrapHtmlForStyle[cellData, cellOptions, html, style],
		initialHTML,
		cellStyles
	];

	(* TODO: Handle the `options` as well. *)
	element
]

(*======================================*)

wrapHtmlForStyle[
	cellData: _?CellDataQ,
	cellOptions:{___?OptionQ},
	html: _,
	style: _?StringQ
] := Module[{},
	Replace[style, {
		(*===================================*)
		(* Headers with auto-anchor linking. *)
		(*===================================*)

		"Title" :> (
			Splice[{
				makeBreadcrumbs[],
				XMLElement["h1", {"class" -> "nb-Title"}, {
					makeAnchorLinkHtml[cellData, html]
				}]
			}]
		),
		"Subtitle" :> XMLElement["p", {"class" -> "nb-Subtitle"}, {html}],
		"Chapter" :> XMLElement["h2", {"class" -> "nb-Chapter"}, {makeAnchorLinkHtml[cellData, html]}],
		"Section" :> XMLElement["h3", {"class" -> "nb-Section"}, {makeAnchorLinkHtml[cellData, html]}],
		"Subsection" :> XMLElement["h4", {"class" -> "nb-Subsection"}, {makeAnchorLinkHtml[cellData, html]}],
		"Subsubsection" :> XMLElement["h5", {"class" -> "nb-Subsubsection"}, {makeAnchorLinkHtml[cellData, html]}],
		"Subsubsubsection" :> XMLElement["h6", {"class" -> "nb-Subsubsubsection"}, {makeAnchorLinkHtml[cellData, html]}],

		(*===============*)
		(* Textual cells *)
		(*===============*)

		"Text" :> XMLElement["p", {}, {ConvertToHTML[cellData]}],

		(*-------*)
		(* Items *)
		(*-------*)
		"Item"
		| "ItemNumbered"
		| "ItemParagraph"
		| "Subitem"
		| "SubitemNumbered"
		| "SubitemParagraph"
		| "Subsubitem"
		| "SubsubitemNumbered"
		| "SubsubitemParagraph"
		(*-------*)
		(* Other *)
		(*-------*)
		(* TID:240527/1: "CodeText" cell handling. *)
		| "CodeText" :> (
			XMLElement[
				"div",
				{"class" -> StringJoin["nb-", style]},
				{ConvertToHTML[cellData]}
			]
		),

		(* TID:250713/1: "ConnorGray/BlockQuote" cell handling. *)
		"ConnorGray/BlockQuote" :> (
			XMLElement[
				"blockquote",
				{"class" -> "nb-BlockQuote"},
				{ConvertToHTML[cellData]}
			]
		),

		(*============*)
		(* Code cells *)
		(*============*)

		"Program" :> XMLElement["pre", {"class" -> "nb-Program"}, {ConvertToHTML[cellData]}],

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

			syntaxString = DeleteDelimitedLines[
				syntaxString,
				{"begin-hidden", "end-hidden"}
			];

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
			syntaxName = Lookup[highlightOptions, "Syntax", CGUtilsUI`$DefaultSyntax];
			theme = Lookup[highlightOptions, "Theme", CGUtilsUI`$DefaultTheme];
			lineNumbering = Lookup[highlightOptions, "LineNumbering", False];

			syntaxHTMLString = Replace[
				CodeSyntaxHighlightHTML[
					syntaxString, syntaxName, theme,
					"LineNumbering" -> lineNumbering
				],
				{
					highlightedHtml: _?StringQ :> highlightedHtml,
					error: _?FailureQ :> Raise[error],
					other: _ :> Raise[NotebookWebsiteError, "Syntax highlighting returned unexpected result: ``", other]
				}
			];

			syntaxHTML = importHTMLFragment[syntaxHTMLString];

			syntaxHTML
		],

		"ConnorGray/Draft" :> Module[{},
			(* Sanity check that we're in a build that that is allowed to
				include cells marked as Draft. *)
			RaiseConfirmMatch[
				Lookup[$BuildSettings, "BuildType"],
				"Drafts"
			];

			XMLElement["div", {"class" -> "nb-Draft"}, {html}]
		],

		other: _ :> Raise[
			NotebookWebsiteError,
			"Unhandled Cell style: ``: ``",
			InputForm[other],
			RawBoxes[cellData]
		]
	}]
]

SetFallthroughError[wrapHtmlForStyle]

(*======================================*)

SetFallthroughError[AddSupportFile]

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
] := WrapRaised[
	NotebookWebsiteError,
	"Error adding support file named: ``",
	InputForm[name0]
] @ Module[{
	name = ConfirmReplace[name0, {
		Automatic :> ToString[Length[$CurrentNotebookSupportFiles]],
		stem: _?StringQ :> (
			ToString[Length[$CurrentNotebookSupportFiles]] <> "-" <> stem
		),
		Verbatim[filename: _?StringQ] :> filename
	}],
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
		other: _ :> Raise[NotebookWebsiteError, "Unsupported support file data: ``", other]
	}];

	(* FIXME: Validate name or encode so that `filePath` only contains URL-safe
		characters? *)
	(*
		Store support files in a directory that has the same name as the base
		name of the notebook they support.

		E.g. if the current notebook is feature-overview.nb, then the support file
		will be located at the file path `feature-overview/<name>.<ext>`
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

(*======================================*)

makeAnchorContentSlug[content: _] := Module[{
	contentString = ConvertToString[content]
},
	RaiseAssert[
		StringQ[contentString],
		"expected anchor content String: ``",
		InputForm[contentString]
	];

	(*	Use just the date as the anchor in headers of the form
			"2024-01-01 — Some title"
		This is used by e.g. my Project Log on connorgray.com, but also useful
		for any other date-entry based page where you want anchors to be simple
		and consistent.
	*)
	If[
		StringStartsQ[
			contentString,
			RegularExpression["[0-9]{4}-[0-9]{2}-[0-9]{2} \[LongDash] "]
		],
		Return[StringTake[contentString, 10], Module];
	];

	StringReplace[contentString, {
		c:LetterCharacter :> ToLowerCase[c],
		WhitespaceCharacter.. -> "-",
		digit: DigitCharacter :> digit,
		(* Leave hyphens untouched. *)
		"-" -> "-",
		(* Remove everything else. *)
		_ -> ""
	}]
]

SetFallthroughError[makeAnchorContentSlug]

(*======================================*)

makeAnchorLinkHtml[content: _, html: _] := Module[{
	contentString = ConvertToString[content],
	contentSlug
},
	RaiseAssert[
		StringQ[contentString],
		"expected anchor link String: ``",
		InputForm[contentString]
	];

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
			{XMLObject["Declaration"][___?OptionQ]},
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
			{one: _} :> one,
			many:{__} :> Raise[NotebookWebsiteError, "Unsupported LiteralHTML cell with multiple top-level tags: ``", many]
		}],
		other: _ :> Raise[
			NotebookWebsiteError,
			"Imported HTML had unexpected format: ``: ``",
			InputForm @ Snippet[htmlString, 3],
			InputForm @ other
		]
	}]
]

SetFallthroughError[importHTMLFragment]

(*====================================*)

SetFallthroughError[makeBreadcrumbs]

(* If this site configures a breadcrumb function, use it to
	insert an HTML breadcrumb. *)
makeBreadcrumbs[] := Catch @ Module[{
	func = Lookup[$BuildSettings["Configuration"], "BreadcrumbFunction"],
	breadcrumbs
},
	If[MissingQ[func],
		Throw[Nothing];
	];

	breadcrumbs = WrapRaised[
		NotebookWebsiteError,
		"Error computing custom breadcrumbs"
	][
		func[$CurrentNotebookRelativeURL]
	];

	ConfirmReplace[breadcrumbs, {
		{{_?StringQ, _?StringQ}..} :> (
			XMLElement["div", {"class" -> "breadcrumbs"}, {
				Splice @ Riffle[#, XML`RawXML["&nbsp;"]]& @ Map[
					crumb |-> (
						XMLElement[
							"a",
							{
								"class" -> "breadcrumbs__crumb",
								"href" -> crumb[[2]]
							},
							{crumb[[1]] <> " />"}
						]
					),
					breadcrumbs
				]
			}]
		),
		Nothing -> Nothing,
		other: _ :> Raise[
			NotebookWebsiteError,
			"Invalid form for breadcrumb function result. Expected {{name, path}...}, got: ``",
			InputForm[other]
		]
	}]
]

(*====================================*)

SetFallthroughError[createTabViewSectionHTML]

createTabViewSectionHTML[tabContentsCells:{___Cell}] := WrapRaised[
	NotebookWebsiteError,
	"Error processing tabbed content"
] @ Module[{
	(* Avoid tab buttons from different tab groups from conflicting.
		Without this, only the first tab group in a document displays correctly.
		The tab content from subsequent groups is never visible. *)
	groupID = StringTake[IntegerString[Hash[tabContentsCells], 16], -8],
	getTabID,
	tabContents,
	tabLabels,
	tabCount
},
	SetFallthroughError[getTabID];

	getTabID[id: _?IntegerQ] :=
		StringJoin["tab-", ToString[id], "-", groupID];

	(*--------------------------------*)
	tabContents = MapIndexed[
		{tabCell, tabPosition} |-> WrapRaised[
			NotebookWebsiteError,
			"Error processing tab at position ``",
			InputForm[tabPosition]
		] @ ConfirmReplace[tabCell, {
			Cell @ CellGroupData[{
				headerCell: _,
				contentsSeq: ___Cell
			}, Open | Closed] :> Module[{
				label,
				contents
			},
				(* TID:240810/2: Tab with excluded header cell. *)
				If[FilteredCellQ[headerCell],
					Return[Nothing, Module];
				];

				label = ConfirmReplace[headerCell, {
					Cell[label0: _?StringQ, __] :> label0,
					(* TID:240810/3: Tab header with non-String cell data. *)
					other: _ :> Raise[
						NotebookWebsiteError,
						<| "TabHeaderCell" -> headerCell |>,
						"Tab header cell data expected to be simple String."
					]
				}];

				(* TID:240809/1: Multi-cell tab contents *)
				contents = ConfirmReplace[Map[ConvertToHTML, {contentsSeq}], {
					(* TID:240810/1: Tab with empty contents after filtering. *)
					{} :> Raise[
						NotebookWebsiteError,
						"Empty tab contents are not supported"
					],
					contents0:{__} :> contents0
				}];

				<|
					"Label" -> label,
					"Contents" -> contents
				|>
			],
			_ :> Raise[
				NotebookWebsiteError,
				"Unexpected structure for tab contents cell: ``",
				InputForm[tabCell]
			]
		}],
		tabContentsCells
	];

	RaiseConfirmMatch[tabContents, {
		Repeated @ <|
			"Label" -> _?HTMLFragmentQ,
			"Contents" -> {__?HTMLFragmentQ}
		|>
	}];

	tabCount = Length[tabContents];

	RaiseAssert[IntegerQ[tabCount]];

	tabLabels   = tabContents[[All, "Label"]];
	tabContents = tabContents[[All, "Contents"]];

	XMLElement["div", {"class" -> "tabbed"}, {
		(* Radio buttons *)
		Splice @ Table[
			XMLElement["input", {
				"type" -> "radio",
				"id" -> getTabID[tabIndex],
				(* NOTE: This name is required to be the same for all
					generated <input> elements, but the choice of name is
					arbitrary. *)
				"name" -> StringJoin["css-tabs-", groupID],
				If[tabIndex === 1,
					"checked" -> "true",
					Splice[{}]
				]
			}, {}],
			{tabIndex, tabCount}
		],

		(* Tab labels *)
		XMLElement[
			"ul",
			{"class" -> "tabs"},
			Table[
				XMLElement["li", {"class" -> "tab"}, {
					XMLElement[
						"label",
						{"for" -> getTabID[tabIndex]},
						{tabLabels[[tabIndex]]}
					]
				}],
				{tabIndex, tabCount}
			]
		],

		(* Tab contents *)
		Splice @ Table[
			XMLElement["div", {"class" -> "tab-content"}, {Splice @ tabContent}],
			{tabContent, tabContents}
		]
	}]
]

(*====================================*)

GeneralUtilities`SetUsage[FilteredCellQ, "
	FilteredCellQ[cell$] returns True if cell$ is marked with a style that
	should not be included in the current build configuration.

	Excluded cells are always filtered. Draft cells are filtered if the
	\"BuildType\" setting is not a build type that includes draft cells.
"]

SetFallthroughError[FilteredCellQ]

FilteredCellQ[cell: _] := Replace[cell, {
	Cell[
		_,
		stylesSeq: ___?StringQ,
		___?OptionQ
	] /; MemberQ[{stylesSeq}, "Excluded" | "ConnorGray/Excluded"] :> (
		True
	),

	Cell[
		_,
		stylesSeq: ___?StringQ,
		___?OptionQ
	] /; And[
		MemberQ[{stylesSeq}, "Draft" | "ConnorGray/Draft"],
		Lookup[$BuildSettings, "BuildType"] =!= "Drafts"
	] :> (
		True
	),

	_ -> False
}]

(*====================================*)

GeneralUtilities`SetUsage[DetermineStatusAction, "
	DetermineStatusAction[status$] returns the action to take for a given notebook
	'DocumentStatus' tagging rule value.

	This function returns one of a set of possible actions:

	* 'Build'
	* 'Skip'

	The action returned for notebooks with the 'Draft' status will depend on the
	'BuildType' in $BuildSettings:

	* 'Published' -> 'Skip'
	* 'Drafts' -> 'Build'
	* 'PreviewDraftsAsPublished' -> 'Build'
"]

SetFallthroughError[DetermineStatusAction]

Options[DetermineStatusAction] = {
	"BuildType" :> Lookup[$BuildSettings, "BuildType"]
}

DetermineStatusAction[status: _?StringQ, OptionsPattern[]] :=
	Replace[status, {
		"Published" -> "Build",

		(* Only include 'Draft' notebooks when the 'BuildType' setting is
			one of the two build types that includes drafts. *)
		"Draft" :> ConfirmReplace[OptionValue["BuildType"], {
			"Published" -> "Skip",
			"Drafts" | "PreviewDraftsAsPublished" -> "Build"
		}],

		"Excluded" -> "Skip",

		other: _ :> Raise[
			NotebookWebsiteError,
			"Unknown document status: ``",
			InputForm[other]
		]
	}]

(*========================================================*)

SetFallthroughError[GetBuildValue]

GetBuildValue[keyPath: _List] := GetCacheValue[$BuildCache, KeyPath[keyPath]]


(*========================================================*)
(* URL Processing                                         *)
(*========================================================*)

notebookRelativeFileToURL[path: _?StringQ] :=
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
			components: _Integer :> StringRepeat["../", components - 1]
		}],
		"web_assets"
	}]
]

(*========================================================*)


End[]

EndPackage[]