BeginPackage["ConnorGray`NotebookWebsiteTools`CurrentBuild`"]

TableOfContentsHtml::usage = "TableOfContentsHtml[] generates an XMLObject containing a table of contents for $CurrentNotebook."
PagesSummaryListHtml::usage = "PagesSummaryListHtml[] generates an XMLObject containing a site map table of contents for the current notebook website."

Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`"]
Needs["ConnorGray`NotebookWebsiteTools`Utils`"]
Needs["ConnorGray`NotebookWebsiteTools`ErrorUtils`"]

Needs["ConnorGray`NotebookWebsiteTools`Build`"]

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
			Rule[Cell[_, ___, "Excluded" | "ConnorGray/Excluded", ___], _] -> Nothing,

			(* Prevent Item cells from showing up in the table of contents. *)
			Rule[Cell[
				_,
				___,
				"Item" | "Subitem" | "SubsubItem"
				| "ItemNumbered" | "SubitemNumbered" | "SubsubitemNumbered"
				| "ItemParagraph" | "SubitemParagraph" | "SubsubitemParagraph",
				___
			], _] -> Nothing,

			(* Prevent Input/Output cell groups from showing up in the table of
			   contents. *)
			(Cell[___, "Input", ___] -> {}) -> Nothing,

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

	If[listItems === {},
		Nothing
		,
		XMLElement["ul", {}, listItems]
	]
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
	websiteDir0 : _?StringQ | Automatic : Automatic,
	filterFunc : _ : Automatic
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
			status,
			statusBadge,
			snippet,
			url
		},
			nb = RaiseConfirm @ Get[nbFile];

			RaiseAssert[MatchQ[nb, _Notebook]];

			(*--------------------------------------------------------*)
			(* Determine whether to include this notebook in the list *)
			(*--------------------------------------------------------*)

			status = Replace[WebsiteNotebookStatus[nb], {
				status_?StringQ :> status,
				(* Don't list non-website notebooks in the page list. *)
				Missing["KeyAbsent", "DocumentStatus"] :> Return[Nothing, Module],
				other_ :> RaiseError["unexpected WebsiteNotebookStatus result: ``", InputForm[other]]
			}];

			(* Don't include non-built files in the summary list. *)
			Replace[DetermineStatusAction[status], {
				"Build" -> Null,
				(* Documents with this status should be skipped, so skip it. *)
				"Skip" :> Return[Nothing, Module],
				other_ :> RaiseError["Unhandled status action value: ``", InputForm[other]]
			}];

			If[filterFunc =!= Automatic,
				If[!TrueQ[filterFunc[nbFileRelative]],
					Return[Nothing, Module];
				];
			];

			(*------------------------------*)
			(* Construct the HTML list item *)
			(*------------------------------*)

			url = notebookRelativeFileToURL[nbFileRelative];

			title = RaiseConfirm @ WebsiteNotebookTitle[nb];
			snippet = RaiseConfirm @ Replace[
				WebsiteNotebookSnippet[nb],
				_?MissingQ -> None
			];

			(* If this is a Draft page, show a pill-shaped badge next to the
				title so it is easy for the website author to know which pages
				are just drafts. *)
			statusBadge = Replace[status, {
				"Draft" -> XMLElement["span", {
					"class" -> "StatusBadge Status-Draft"
				}, {"DRAFT"}],
				_ -> Nothing
			}];

			RaiseAssert[StringQ[title], "bad title: ``", InputForm[title]];
			RaiseAssert[MatchQ[snippet, _?StringQ | None], "bad snippet: ``", InputForm[snippet]];
			RaiseAssert[MatchQ[url, URL[_?StringQ]]];

			XMLElement["li", {}, {
				XMLElement["h4", {}, {
					XMLElement[
						"a",
						{"href" -> url[[1]]},
						{title}
					],
					statusBadge
				}],
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


End[]

EndPackage[]