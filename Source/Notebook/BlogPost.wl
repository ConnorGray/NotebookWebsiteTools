BeginPackage["ConnorGray`NotebookWebsiteTools`Notebook`BlogPost`"]

CreateBlogPostNotebook

Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`ErrorUtils`"]
Needs["ConnorGray`NotebookWebsiteTools`Notebook`"]

CreateBlogPostNotebook[title: _?StringQ] := Module[{nb},
	nb = Notebook[{
		Cell[title, "Title"],
		Cell[
			"Created " <> DateString[Now, {"DayName", " ", "MonthName", " ", "Day", ", ", "Year"}],
			"Subtitle"
		]
	},
		DockedCells -> makeBlogPostDockedCells[],
		TaggingRules -> MakeNotebookTaggingRules["BlogPost"]
	];

	nb
]

AddUnmatchedArgumentsHandler[CreateBlogPostNotebook]

(*====================================*)

makeBlogPostDockedCells[] := Module[{},
	{}
]

AddUnmatchedArgumentsHandler[makeBlogPostDockedCells]

End[]

EndPackage[]