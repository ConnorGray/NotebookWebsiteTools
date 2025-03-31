BeginPackage["ConnorGray`NotebookWebsiteTools`Toolbar`"]

MakeStandardWebsiteNotebookToolbar


Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`Errors`"]

Needs["ConnorGray`NotebookWebsiteTools`UI`"]

MakeStandardWebsiteNotebookToolbar[] := Module[{},
	{
		(*
			This cell will display an error if the
			ConnorGray/NotebookWebsiteTools paclet is not installed, which
			implies that the NotebookWebsiteTools stylesheet and package code
			implementing functionality like buttons and syntax highlighting
			aren't available.
		*)
		Cell[
			TextData[{
				StyleBox[
					{
						StyleBox["Error: ", Bold, RGBColor[1, 0.25, 0.25]],
						"Unable to locate NotebookWebsiteTools paclet. ",
						"This notebook might not render or behave correctly. ",
						"Install ConnorGray/NotebookWebsiteTools and reopen this notebook to resolve this error."
					},
					FontFamily -> "Source Sans Pro",
					FontSize -> 15
				]
			}],
			Background -> RGBColor[1, 0.85, 0.85],
			(* Only show this error cell if NotebookWebsiteTools is not installed. *)
			CellOpen -> Dynamic[
				Not @ TrueQ[CurrentValue[
					EvaluationNotebook[],
					{TaggingRules, "NotebookWebsiteToolsIsInstalled"}
				]]
			]
		],
		(* Display DockedCells set in the NotebookWebsiteTools parent stylesheet. *)
		Inherited
	}
]

(*========================================================*)
(* Utilities                                              *)
(*========================================================*)

(*====================================*)

End[]

EndPackage[]