BeginPackage["ConnorGray`NotebookWebsiteTools`CurrentBuild`"]

$CurrentNotebook::usage = "$CurrentNotebook returns the Notebook expression of the notebook that is currently being processed."

TableOfContentsHtml::usage = "TableOfContentsHtml[] generates an XMLObject containing a table of contents for $CurrentNotebook."

Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`Utils`"]
Needs["ConnorGray`NotebookWebsiteTools`ErrorUtils`"]

Needs["ConnorGray`NotebookWebsiteTools`Build`"]

$CurrentNotebook := RaiseError["Unexpected use of $CurrentNotebook: no notebook is currently being processed."]

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

End[]

EndPackage[]