BeginPackage["ConnorGray`NotebookWebsiteTools`CurrentBuild`"]

$CurrentNotebook::usage = "$CurrentNotebook returns the Notebook expression of the notebook that is currently being processed."
$CurrentNotebookWebsiteDirectory = "$CurrentNotebookWebsiteDirectory returns the file path of the root directory of the notebook website that is currently being built."

TableOfContentsHtml::usage = "TableOfContentsHtml[] generates an XMLObject containing a table of contents for $CurrentNotebook."
PagesSummaryListHtml::usage = "PagesSummaryListHtml[] generates an XMLObject containing a site map table of contents for the current notebook website."

Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`"]
Needs["ConnorGray`NotebookWebsiteTools`Utils`"]
Needs["ConnorGray`NotebookWebsiteTools`ErrorUtils`"]

Needs["ConnorGray`NotebookWebsiteTools`Build`"]

$CurrentNotebook := RaiseError["Unexpected use of $CurrentNotebook: no notebook is currently being processed."]
$CurrentNotebookWebsiteDirectory := RaiseError["Unexpected use of $CurrentNotebookWebsiteDirectory: no notebook website is currently being built."]

(*========================================================*)

TableOfContentsHtml[] := TableOfContentsHtml[$CurrentNotebook]

TableOfContentsHtml[nb_Notebook] := Module[{
	cells,
	headings
},
	RaiseAssert[MatchQ[nb, Notebook[{Cell[___]...}, ___]]];

	cells = nb[[1]];

	headings = extractCellGroupHeadings[cells];

	headings = Replace[headings, {
		{Cell[_, "Title", ___] -> children_?ListQ} :> children
	}];

	XMLElement["nav", {"class" -> "TableOfContents"}, {makeTableOfContentsHtml[headings]}]
]

AddUnmatchedArgumentsHandler[TableOfContentsHtml]

(*------------------------------------*)

makeTableOfContentsHtml[
	headings: {(Cell[___] -> _?ListQ)...}
] := Module[{
	listItems
},
	listItems = Map[
		Replace[{
			(* Remove "Excluded" cells. *)
			Rule[Cell[_, ___, "Excluded", ___], _] -> Nothing,
			Rule[Cell[cellData_, ___], children_?ListQ] :> Module[{
				contentString = ConvertToString[cellData],
				contentSlug
			},
				contentSlug = makeAnchorContentSlug[contentString];

				XMLElement["li", {}, {
					XMLElement["a", {"href" -> "#" <> contentSlug}, {
						contentString,
						XMLElement["small", {}, {"↴"}]
					}],
					(* Now recurse to add sub-tables-of-contents. *)
					makeTableOfContentsHtml[children]
				}]
			],
			other_ :> RaiseError["Unexpected cell headings structure: ``", other]
		}],
		headings
	];

	XMLElement["ul", {}, listItems]
]

AddUnmatchedArgumentsHandler[makeTableOfContentsHtml]

(*====================================*)

(*
	{
		(heading -> {
			(heading -> {
				...
			})...
		})...
	}
*)

extractCellGroupHeadings[cells:{___Cell}] := Module[{},
	Map[
		Replace[{
			Cell[CellGroupData[inner:{heading_, ___Cell}, _]] :> (
				heading -> extractCellGroupHeadings[inner]
			),
			(* TODO: What about Chapter/Section/Subsection/etc. cell sections
				that are empty? *)
			Cell[Except[_CellGroupData], ___] -> Nothing,
			other_ :> RaiseError[
				"Error constructing table of contents: Unexpected Notebook cell structure: ``",
				InputForm[other]
			]
		}],
		cells
	]
]

AddUnmatchedArgumentsHandler[extractCellGroupHeadings]

(*========================================================*)

PagesSummaryListHtml[
	websiteDir0 : _?StringQ : Automatic
] := Module[{
	websiteDir = Replace[
		Replace[websiteDir0, Automatic :> $CurrentNotebookWebsiteDirectory],
		File[dir_?StringQ] :> dir
	],
	contentDir,
	notebooks,
	listItems
},
	contentDir = FileNameJoin[{websiteDir, "Content"}];

	RaiseAssert[DirectoryQ[contentDir], "bad contentDir: ``", contentDir];

	notebooks = FileNames["*.nb", contentDir, Infinity];

	listItems = Map[
		nbFile |-> Module[{
			nbFileRelative = RelativePath[contentDir, nbFile],
			nb,
			title,
			snippet
		},
			nb = RaiseConfirm @ Get[nbFile];
			RaiseAssert[MatchQ[nb, _Notebook]];

			title = RaiseConfirm @ WebsiteNotebookTitle[nb];
			snippet = RaiseConfirm @ Replace[
				WebsiteNotebookSnippet[nb],
				_?MissingQ -> None
			];

			RaiseAssert[StringQ[title], "bad title: ``", InputForm[title]];
			RaiseAssert[MatchQ[snippet, _?StringQ | None], "bad snippet: ``", InputForm[snippet]];

			XMLElement["li", {}, {
				XMLElement[
					"a",
					{"href" -> notebookRelativeFileToURL[nbFileRelative]},
					{XMLElement["h4", {}, {title}]}
				],
				If[StringQ[snippet],
					XMLElement["p", {}, {snippet}]
					,
					Nothing
				]
			}]
		],
		notebooks
	];

	XMLElement["nav", {"class" -> "PagesSummaryList"}, {
		XMLElement["ul", {}, listItems]
	}]
]

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