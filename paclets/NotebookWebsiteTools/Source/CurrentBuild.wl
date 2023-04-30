BeginPackage["ConnorGray`NotebookWebsiteTools`CurrentBuild`"]

TableOfContentsHtml::usage = "TableOfContentsHtml[] generates an XMLElement containing a table of contents for $CurrentNotebook."
PagesSummaryListHtml::usage = "PagesSummaryListHtml[] generates an XMLElement containing a table of contents for the current notebook website."
VisualSiteMapHtml::usage = "VisualSiteMapHtml[] generates an XMLElement containing a heirarchical listing of all documents in the current notebook website."

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
				createDocumentTitleLinkHtml[nb, url],
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

AddUnmatchedArgumentsHandler[VisualSiteMapHtml]

VisualSiteMapHtml[
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

	notebooks = Map[
		nbFile |-> Module[{
			nbFileRelative = RelativePath[contentDir, nbFile],
			nb,
			status,
			statusBadge,
			title,
			url
		},
			nb = RaiseConfirm @ Get[nbFile];

			RaiseAssert[MatchQ[nb, _Notebook]];

			(*--------------------------------------------------------*)
			(* Determine whether to include this notebook in the list *)
			(*--------------------------------------------------------*)

			status = Replace[WebsiteNotebookStatus[nb], {
				status_?StringQ :> status,
				(* Don't list non-website notebooks. *)
				Missing["KeyAbsent", "DocumentStatus"] :> Return[Nothing, Module],
				other_ :> RaiseError["unexpected WebsiteNotebookStatus result: ``", InputForm[other]]
			}];

			(* Don't include non-built files in the list. *)
			Replace[DetermineStatusAction[status], {
				"Build" -> Null,
				(* Documents with this status should be skipped, so skip it. *)
				"Skip" :> Return[Nothing, Module],
				other_ :> RaiseError["Unhandled status action value: ``", InputForm[other]]
			}];

			(*------------------------------------------------------------------*)
			(* Build association of useful metainformation about this notebook. *)
			(*------------------------------------------------------------------*)

			title = RaiseConfirm @ WebsiteNotebookTitle[nb];

			url = notebookRelativeFileToURL[nbFileRelative];

			<|
				"RelativePath" -> nbFileRelative,
				"Notebook" -> nb,
				"DocumentStatus" -> status,
				"Title" -> title,
				"URL" -> url
			|>
		],
		notebooks
	];

	notebooks = PrefixListsToRules[
		Map[Append[Most[FileNameSplit[#"RelativePath"]], #] &, notebooks]
	];

	notebooks = Sort[notebooks];

	RaiseAssert[MatchQ[notebooks, {(_?AssociationQ | (_?StringQ -> {___}))...}]];

	notebooks = RulesTree["Root" -> notebooks];

	RaiseAssert[TreeQ[notebooks]];

	XMLElement["nav", {"class" -> "VisualSiteMap"}, {
		XMLElement["ul", {}, Map[makeSiteMapHtml, TreeChildren[notebooks]]]
	}]
]

(*------------------------------------*)

AddUnmatchedArgumentsHandler[makeSiteMapHtml]

makeSiteMapHtml[node_] := Replace[node, {
	Tree[
		KeyValuePattern[{
			"Notebook" -> nb_Notebook,
			"DocumentStatus" -> status_?StringQ,
			"Title" -> title_?StringQ,
			"URL" -> url_URL
		}],
		None
	] :> Module[{},
		XMLElement["li", {}, {
			createDocumentTitleLinkHtml[nb, url]
		}]
	],
	Tree[component_?StringQ, children:{___Tree}] :> (
		XMLElement["li", {}, {
			component,
			XMLElement["ul", {}, Map[makeSiteMapHtml, children]]
		}]
	),
	other_ :> (
		RaiseError[
			"Unexpected site map HTML Tree[..] content: ``",
			InputForm[node]
		]
	)
}]

(*========================================================*)

AddUnmatchedArgumentsHandler[createDocumentTitleLinkHtml]

createDocumentTitleLinkHtml[nb_Notebook, URL[url_?StringQ]] := Module[{
	status,
	statusBadge
},
	status = Replace[WebsiteNotebookStatus[nb], {
		status_?StringQ :> status,
		(* Don't list non-website notebooks in the page list. *)
		missing:Missing["KeyAbsent", "DocumentStatus"] :> Return[missing, Module],
		other_ :> RaiseError["unexpected WebsiteNotebookStatus result: ``", InputForm[other]]
	}];

	(* If this is a Draft page, show a pill-shaped badge next to the
		title so it is easy for the website author to know which pages
		are just drafts. *)
	statusBadge = Replace[status, {
		"Draft" -> XMLElement[
			"span",
			{"class" -> "StatusBadge Status-Draft"},
			{"DRAFT"}
		],
		_ -> Nothing
	}];

	title = RaiseConfirm @ WebsiteNotebookTitle[nb];

	XMLElement["h4", {"class" -> "DocumentLink"}, {
		XMLElement[
			"a",
			{"href" -> url},
			{title}
		],
		statusBadge
	}]
]

(*========================================================*)

End[]

EndPackage[]