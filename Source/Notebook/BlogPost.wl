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