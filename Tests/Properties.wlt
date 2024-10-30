Needs["ConnorGray`NotebookWebsiteTools`"]

(* Test initial content of a new website notebook. *)
VerificationTest[
	nb = UsingFrontEnd @ CreateWebsiteNotebook["BlogPost", "The Title"];

	UsingFrontEnd @ NotebookGet[nb]
	,
	Notebook[
		{
			Cell[CellGroupData[ {
				Cell["The Title", "Title"],
				Cell[s_?StringQ /; StringStartsQ[s, "Created "], "Subtitle"]
			}, Open]]
		},
		WindowSize -> _,
		WindowMargins -> _,
		DockedCells -> {
			Cell[___],
			Inherited
		},
		TaggingRules -> {
			"ConnorGray/NotebookWebsiteTools" -> {
				"DocumentType" -> "BlogPost",
				"CreatedByPacletVersion" -> "0.0.1"
			},
			"NotebookWebsiteToolsIsInstalled" -> True
		},
		Initialization :> _,
		FrontEndVersion -> _,
		StyleDefinitions -> FrontEnd`FileName[
			{"ConnorGray"},
			"NotebookWebsiteTools.nb",
			CharacterEncoding -> "UTF-8"
		]
	]
	,
	SameTest -> MatchQ
]
