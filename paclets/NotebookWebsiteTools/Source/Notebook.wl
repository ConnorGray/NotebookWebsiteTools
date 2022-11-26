BeginPackage["ConnorGray`NotebookWebsiteTools`Notebook`"]


MakeNotebookTaggingRules
MakeNotebookStyleDefinitions
MakeNotebookDockedCells

UpdateNotebook::usage = "UpdateNotebook[obj] updates the website notebook specified by the notebook object obj."

Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`ErrorUtils`"]
Needs["ConnorGray`NotebookWebsiteTools`Toolbar`"]
Needs["ConnorGray`NotebookWebsiteTools`LibraryLink`"]
Needs["ConnorGray`NotebookWebsiteTools`UI`"]

Needs["ConnorGray`NotebookWebsiteTools`Notebook`BlogPost`"]

(*====================================*)

UpdateNotebook[nb_NotebookObject] := Module[{
	metadata,
	documentType,
	createdByPacletVersion
},
	metadata = Replace[Options[nb, TaggingRules], {
		{TaggingRules -> KeyValuePattern["ConnorGray/NotebookWebsiteTools" -> metadata_]} :> metadata,
		other_ :> RaiseError[
			"Notebook does not have expected \"ConnorGray/NotebookWebsiteTools\" TaggingRules value"
		]
	}];

	{documentType, createdByPacletVersion} = Replace[metadata, {
		KeyValuePattern[{
			"DocumentType" -> type_?StringQ,
			"CreatedByPacletVersion" -> createdBy_?StringQ
		}] :> {type, createdBy},
		other_ :> RaiseError[
			"Notebook metadata does not have expected fields: ``",
			InputForm[other]
		]
	}];

	(* NOTE: documentType and createdByPacletVersion should be used to customize
		the update code below if and when backwards-incompatible changes are
		made to the website notebook format. *)

	SetOptions[nb, {
		DockedCells -> MakeNotebookDockedCells[documentType],
		StyleDefinitions -> MakeNotebookStyleDefinitions[]
	}]
]

AddUnmatchedArgumentsHandler[UpdateNotebook]

(*====================================*)

MakeNotebookTaggingRules[documentType: _?StringQ] := Module[{
	pacletVersion = PacletObject["ConnorGray/NotebookWebsiteTools"]["Version"]
},
	RaiseAssert[StringQ[pacletVersion] && StringMatchQ[
		pacletVersion,
		DigitCharacter.. ~~ "." ~~ DigitCharacter.. ~~ "." ~~ DigitCharacter..
	]];

	{
		"ConnorGray/NotebookWebsiteTools" -> {
			"DocumentType" -> documentType,
			(* Embed the paclet version of NotebookWebsiteTools that originally
			   created this notebook. This is potentially useful for improved
			   error reporting or automated fixes if backwards-incompatible
			   changes to NotebookWebsiteTools are ever necessary. *)
			"CreatedByPacletVersion" -> pacletVersion
		}
	}
]

AddUnmatchedArgumentsHandler[MakeNotebookTaggingRules]

(*====================================*)

MakeNotebookStyleDefinitions[] := Module[{},
	Notebook[{
		Cell[StyleData[StyleDefinitions -> "Default.nb"] ],

		(*====================*)
		(* Content Processing *)
		(*====================*)

		Cell[StyleData["Excluded"],
			Background -> $ExcludedColor,
			CellFrame -> {{3, False}, {False, False}},
			CellFrameColor -> $ExcludedAccentColor
		],

		(*===============*)
		(* Special Cells *)
		(*===============*)

		Cell[StyleData["LiteralHTML"],
			CellMargins->{{66, 10}, {8, 8}},
			Background -> GrayLevel[0.95],
			CellFrame -> {{3, False}, {False, False}},
			CellFrameColor -> GrayLevel[0.8],
			CellDingbat -> ToBoxes @ Style["</>", Bold, GrayLevel[0.6], ShowStringCharacters -> False],
			(* Copied from Default.nb "Program" cells: *)
			StripStyleOnPaste -> True,
			CodeAssistOptions -> {"AutoDetectHyperlinks" -> False},
			Hyphenation -> False,
			AutoQuoteCharacters -> {},
			PasteAutoQuoteCharacters -> {},
			LanguageCategory -> None,
			FontFamily -> Dynamic[
				AbsoluteCurrentValue[
					EvaluationCell[],
					{StyleHints, "CodeFont"}
				]
			]
		],

		Cell[StyleData["HighlightSyntax"],
			MenuSortingValue -> 1600,
			CellMargins->{{66, 10}, {8, 8}},
			Background :> Dynamic[HighlightSyntaxCellDefaultBackground[EvaluationCell[]]],
			CellFrame -> {{3, False}, {False, False}},
			CellFrameColor -> GrayLevel[0.8],
			CellDingbat -> ToBoxes @ Style["</>", Bold, GrayLevel[0.6], ShowStringCharacters -> False],
			(* Copied from Default.nb "Program" cells: *)
			StripStyleOnPaste -> True,
			CodeAssistOptions -> {"AutoDetectHyperlinks" -> False},
			Hyphenation -> False,
			AutoQuoteCharacters -> {},
			PasteAutoQuoteCharacters -> {},
			LanguageCategory -> None,
			FontFamily -> Dynamic[
				AbsoluteCurrentValue[
					EvaluationCell[],
					{StyleHints, "CodeFont"}
				]
			],
			CellEventActions -> {
				PassEventsDown -> True,
				(* Note: This undocumented option causes the cell event handler
					actions to be run AFTER the default FE behavior for the
					event runs (the default is that user-specified event handlers
					are run before the default FE ones).

					We want this because we need the character the user typed to
					be added to the content of the cell before we read the
					contents of the cell in our "KeyDown" event hander. *)
				EvaluationOrder -> After,
				"KeyDown" :> (
					HandleHighlightSyntaxCellEvent[EvaluationCell[], "KeyDown"]
				)
			}
		],

		(*====================================*)
		(* Toolbar button TemplateBox styles  *)
		(*====================================*)

		Cell[
			StyleData["NotebookWebsiteTools:IconAndLabelButtonTemplate"],
			TemplateBoxOptions -> {
				DisplayFunction -> $iconAndLabelButtonTemplate
			}
		]
	}]
]

AddUnmatchedArgumentsHandler[MakeNotebookStyleDefinitions]

(*------------------------------------*)

(*
	Parameters: {icon, label, tooltip, action, buttonMethod, buttonDefaultBackground, buttonAccentColor}

	(TemplateBox DisplayFunction's are 'evaluated' by the FrontEnd, whose
	very basic evaluator doesn't not support named Function parameters.)
*)
$iconAndLabelButtonTemplate = Function[
	DynamicModuleBox[{state = "default"},
		TagBox[
			ButtonBox[
				FrameBox[
					GridBox[
						{{
							StyleBox[
								#1,
								GraphicsBoxOptions -> {
									BaseStyle -> Dynamic[
										Switch[state,
											"hovered", White,
											_, #7
										]
									]
								}
							],
							PaneBox[#2]
						}},
						GridBoxAlignment -> {
							"Columns" -> {{Left}},
							"Rows" -> {{Center}}
						}
					],
					BaseStyle -> {
						FontSize -> 10,
						FontWeight -> Automatic,
						FontColor -> Dynamic[Switch[state,
							"hovered", White,
							_, #7
						]]
					},
					FrameMargins -> {{2, 2}, {2, 1}},
					FrameStyle -> Directive[Thickness[1], #7],
					Background -> Dynamic[Switch[state,
						"default",
							#6,
						"hovered",
							RGBColor[1, 0.5, 0],
						"pressed",
							Gray
					]],
					RoundingRadius -> 3
				],
				Appearance -> None,
				ButtonFunction :> #4,
				Method -> #5,
				Evaluator -> Automatic
			],
			EventHandlerTag[{
				"MouseEntered" :> (state = "hovered"),
				"MouseExited" :> (state = "default"),
				{"MouseDown", 1} :> (state = "pressed"),
				{"MouseUp", 1} :> (state = "hovered"),
				PassEventsDown -> True,
				PassEventsUp -> True,
				Method -> "Preemptive"
			}]
		],
		DynamicModuleValues :> {}
	]
]

(*====================================*)

MakeNotebookDockedCells[documentType: _?StringQ] := Replace[documentType, {
	"BlogPost" :> MakeBlogPostDockedCells[],
	other_ :> RaiseError["Unhandled document type in MakeNotebookDockedCells: ``", other]
}]

AddUnmatchedArgumentsHandler[MakeNotebookDockedCells]

(*====================================*)

End[]

EndPackage[]