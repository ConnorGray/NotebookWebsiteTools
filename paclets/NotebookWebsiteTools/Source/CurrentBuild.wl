BeginPackage["ConnorGray`NotebookWebsiteTools`CurrentBuild`"]

TableOfContentsHtml::usage = "TableOfContentsHtml[] generates an XMLElement containing a table of contents for $CurrentNotebook."
PagesSummaryListHtml::usage = "PagesSummaryListHtml[] generates an XMLElement containing a table of contents for the current notebook website."
VisualSiteMapHtml::usage = "VisualSiteMapHtml[] generates an XMLElement containing a heirarchical listing of all documents in the current notebook website."

LinkDashboard::usage = "LinkDashboard[links] generates an XMLElement displaying the specified links in an easily clickable \"dashboard\" display"

Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`"]
Needs["ConnorGray`NotebookWebsiteTools`Utils`"]
Needs["ConnorGray`NotebookWebsiteTools`Errors`"]

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

SetFallthroughError[TableOfContentsHtml]

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
			other_ :> Raise[NotebookWebsiteError, "Unexpected cell headings structure: ``", other]
		}],
		headings
	];

	If[listItems === {},
		Nothing
		,
		XMLElement["ul", {}, listItems]
	]
]

SetFallthroughError[makeTableOfContentsHtml]

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
			other_ :> Raise[
				NotebookWebsiteError,
				"Error constructing table of contents: Unexpected Notebook cell structure: ``",
				InputForm[other]
			]
		}],
		cells
	]
]

SetFallthroughError[extractCellGroupHeadings]

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
		nbFile |-> WrapRaised[
			NotebookWebsiteError,
			"Error generating pages summary list entry for file at ``",
			nbFile
		] @ Module[{
			nbFileRelative = RelativePath[contentDir, nbFile],
			nb,
			title,
			status,
			statusBadge,
			snippet,
			url
		},
			nb = RaiseConfirm @ GetBuildValue[{File[nbFile], Notebook}];

			RaiseAssert[MatchQ[nb, _Notebook]];

			(*--------------------------------------------------------*)
			(* Determine whether to include this notebook in the list *)
			(*--------------------------------------------------------*)

			status = Replace[GetBuildValue[{File[nbFile], WebsiteNotebookStatus}], {
				status_?StringQ :> status,
				(* Don't list non-website notebooks in the page list. *)
				Missing["KeyAbsent", "DocumentStatus"] :> Return[Nothing, Module],
				other_ :> Raise[NotebookWebsiteError, "unexpected WebsiteNotebookStatus result: ``", InputForm[other]]
			}];

			(* Don't include non-built files in the summary list. *)
			Replace[DetermineStatusAction[status], {
				"Build" -> Null,
				(* Documents with this status should be skipped, so skip it. *)
				"Skip" :> Return[Nothing, Module],
				other_ :> Raise[NotebookWebsiteError, "Unhandled status action value: ``", InputForm[other]]
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

			title = RaiseConfirm @ GetBuildValue[{File[nbFile], WebsiteNotebookTitle}];

			snippet = RaiseConfirm @ Replace[
				GetBuildValue[{File[nbFile], WebsiteNotebookSnippet}],
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
				createDocumentTitleLinkHtml[File[nbFile], url],
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

SetFallthroughError[VisualSiteMapHtml]

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
			status,
			statusBadge,
			title,
			url
		},
			(*--------------------------------------------------------*)
			(* Determine whether to include this notebook in the list *)
			(*--------------------------------------------------------*)

			status = Replace[GetBuildValue[{File[nbFile], WebsiteNotebookStatus}], {
				status_?StringQ :> status,
				(* Don't list non-website notebooks. *)
				Missing["KeyAbsent", "DocumentStatus"] :> Return[Nothing, Module],
				other_ :> Raise[NotebookWebsiteError, "unexpected WebsiteNotebookStatus result: ``", InputForm[other]]
			}];

			(* Don't include non-built files in the list. *)
			Replace[DetermineStatusAction[status], {
				"Build" -> Null,
				(* Documents with this status should be skipped, so skip it. *)
				"Skip" :> Return[Nothing, Module],
				other_ :> Raise[NotebookWebsiteError, "Unhandled status action value: ``", InputForm[other]]
			}];

			(*------------------------------------------------------------------*)
			(* Build association of useful metainformation about this notebook. *)
			(*------------------------------------------------------------------*)

			title = RaiseConfirm @ GetBuildValue[{File[nbFile], WebsiteNotebookTitle}];

			url = notebookRelativeFileToURL[nbFileRelative];

			<|
				"RelativePath" -> nbFileRelative,
				"NotebookFile" -> File[nbFile],
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

SetFallthroughError[makeSiteMapHtml]

makeSiteMapHtml[node_] := Replace[node, {
	Tree[
		KeyValuePattern[{
			"NotebookFile" -> nbFile_File,
			"DocumentStatus" -> status_?StringQ,
			"Title" -> title_?StringQ,
			"URL" -> url_URL
		}],
		None
	] :> Module[{},
		XMLElement["li", {}, {
			createDocumentTitleLinkHtml[nbFile, url]
		}]
	],
	Tree[component_?StringQ, children:{___Tree}] :> (
		XMLElement["li", {}, {
			component,
			XMLElement["ul", {}, Map[makeSiteMapHtml, children]]
		}]
	),
	other_ :> (
		Raise[
			NotebookWebsiteError,
			"Unexpected site map HTML Tree[..] content: ``",
			InputForm[node]
		]
	)
}]

(*========================================================*)

SetFallthroughError[createDocumentTitleLinkHtml]

createDocumentTitleLinkHtml[nbFile_File, URL[url_?StringQ]] := Module[{
	status,
	statusBadge
},
	status = Replace[GetBuildValue[{nbFile, WebsiteNotebookStatus}], {
		status_?StringQ :> status,
		(* Don't list non-website notebooks in the page list. *)
		missing:Missing["KeyAbsent", "DocumentStatus"] :> Return[missing, Module],
		other_ :> Raise[NotebookWebsiteError, "unexpected WebsiteNotebookStatus result: ``", InputForm[other]]
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

	title = RaiseConfirm @ GetBuildValue[{nbFile, WebsiteNotebookTitle}];

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

SetFallthroughError[LinkDashboard]

LinkDashboard[linkGroups0_List] := Module[{
	linkGroups = linkGroups0
},
	linkGroups = Map[
		linkGroup |-> Module[{
			itemsHtml
		},
			If[!ListQ[linkGroup],
				Raise[
					NotebookWebsiteError,
					"Expected link display group to be a List, got: ``",
					InputForm[other]
				];
			];

			itemsHtml = Map[
				item0 |-> ConfirmReplace[item0, {
					heading_?StringQ :> (
						XMLElement["div", {"class" -> "header"}, {heading}]
					),
					Delimiter :> (
						XMLElement["hr", {}, {}]
					),
					Hyperlink[uri_?StringQ] :> (
						XMLElement["a", {"href" -> uri}, {uri}]
					),
					Hyperlink[label_, uri_?StringQ] :> (
						XMLElement["a", {"href" -> uri, "class" -> "IconLink"}, {
							(* FIXME: Only do for https links, e.g. not file: links *)
							Handle[
								getWebsiteFaviconAsEmbeddedImg[uri],
								f_Failure :> (
									Print["WARNING: ", f];
									Splice[{}]
								)
							],
							ConvertToHtml[label]
						}]
					),
					other_ :> Raise[
						NotebookWebsiteError,
						"Unexpected link display item: ``",
						InputForm[other]
					]
				}],
				linkGroup
			];

			itemsHtml //= Map[
				itemHtml |-> XMLElement["li", {}, {itemHtml}]
			];

			XMLElement[
				"ul",
				{"class" -> "link-group"},
				itemsHtml
			]
		],
		linkGroups
	];

	XMLElement[
		"div",
		{"class" -> "link-group-container"},
		linkGroups
	]
]

(*========================================================*)

getWebsiteFaviconAsEmbeddedImg[url_?StringQ] := WrapRaised[
	NotebookWebsiteError,
	"Error getting favicon for URL to embed in link: ``",
	InputForm[url]
] @ Module[{
	image,
	pngBase64
},
	(* TODO: We should cache this result based on the "Domain" of the URL. *)
	image = GetWebsiteFavicon[url];

	RaiseAssert[ImageQ[image], "expected favicon Image: ", InputForm[image]];

	pngBase64 = BaseEncode[
		ExportByteArray[image, "PNG"],
		"Base64"
	];

	RaiseAssert[StringQ[pngBase64]];

	XMLElement[
		"img",
		{
			"src" -> "data:image/png;base64," <> pngBase64,
			"width" -> "16",
			"height" -> "16"
		},
		{}
	]
]

(*========================================================*)

End[]

EndPackage[]