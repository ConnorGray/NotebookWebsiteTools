BeginPackage["ConnorGray`NotebookWebsiteTools`Notebook`BlogPost`"]

CreateBlogPostNotebook

MakeBlogPostDockedCells

Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`ErrorUtils`"]
Needs["ConnorGray`NotebookWebsiteTools`Notebook`"]
Needs["ConnorGray`NotebookWebsiteTools`Toolbar`"]

CreateBlogPostNotebook[title: _?StringQ] := Module[{nb},
	nb = Notebook[{
		Cell[title, "Title"],
		Cell[
			"Created " <> DateString[Now, {"DayName", " ", "MonthName", " ", "Day", ", ", "Year"}],
			"Subtitle"
		]
	},
		Initialization :> (
			(* Set a tagging rule to indicate if NotebookWebsiteTools is
				installed. This tagging rule is read dynamically by a docked
				cell that will display an error to the user if the paclet isn't
				installed. *)
			CurrentValue[
				EvaluationNotebook[],
				{TaggingRules, "NotebookWebsiteToolsIsInstalled"}
			] = !FailureQ[PacletObject["ConnorGray/NotebookWebsiteTools"]];
		),
		DockedCells -> MakeBlogPostDockedCells[],
		TaggingRules -> MakeNotebookTaggingRules["BlogPost"],
		StyleDefinitions -> MakeNotebookStyleDefinitions[]
	];

	nb
]

AddUnmatchedArgumentsHandler[CreateBlogPostNotebook]

(*====================================*)

MakeBlogPostDockedCells[] := MakeStandardWebsiteNotebookToolbar[]

AddUnmatchedArgumentsHandler[MakeBlogPostDockedCells]

End[]

EndPackage[]