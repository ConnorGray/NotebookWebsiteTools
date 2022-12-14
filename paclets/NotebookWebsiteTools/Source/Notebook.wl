BeginPackage["ConnorGray`NotebookWebsiteTools`Notebook`"]

(*----------------------------*)
(* Creating Website Notebooks *)
(*----------------------------*)

MakeNotebookTaggingRules
MakeNotebookStyleDefinitions
MakeNotebookDockedCells

UpdateNotebook::usage = "UpdateNotebook[obj] updates the website notebook specified by the notebook object obj."

Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`"]
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

$MenuSortingValueOffset = 2500;

MakeNotebookStyleDefinitions[] := Module[{},
	Notebook[{
		Cell[StyleData[StyleDefinitions -> "Default.nb"] ],

		(*====================*)
		(* Content Processing *)
		(*====================*)

		Cell[StyleData["Excluded"],
			MenuSortingValue -> $MenuSortingValueOffset,
			Background -> $ExcludedColor,
			CellFrame -> {{3, False}, {False, False}},
			CellFrameColor -> $ExcludedAccentColor
		],

		(*===============*)
		(* Special Cells *)
		(*===============*)

		Cell[StyleData["LiteralHTML"],
			MenuSortingValue -> $MenuSortingValueOffset + 25,
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

		(* TODO: Better name for this style. Template, plugin, etc? *)
		Cell[StyleData["ComputedHTML", StyleDefinitions -> StyleData["Input"]],
			MenuSortingValue -> $MenuSortingValueOffset + 50,
			CellDingbat -> ToBoxes @ Style["</>", Bold, GrayLevel[0.6], ShowStringCharacters -> False]
		],

		Cell[StyleData["HighlightSyntax"],
			MenuSortingValue -> $MenuSortingValueOffset + 75,
			CellMargins->{{66, 10}, {8, 8}},
			Background :> Dynamic[HighlightSyntaxCellDefaultBackground[EvaluationCell[]]],
			CellFrame -> {{3, False}, {False, False}},
			CellFrameColor -> GrayLevel[0.8],
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
			(* TODO: Uncomment this PageWidth setting after the bug with its
				interaction with attached cells is fixed. (The Right-aligned
				attached cell is not shown) *)
			(* Don't word wrap content in HighlightSyntax cells, which is
			   typically pre-formatted code input. *)
			(* PageWidth -> Infinity, *)
			Initialization :> (
				AttachCell[
					EvaluationCell[],
					Cell[
						BoxData @ ToBoxes @ PaneSelector[
							{
								True -> Row[{
									PopupMenu[
										Dynamic @ CurrentValue[
											ParentCell @ EvaluationCell[],
											{TaggingRules, "HighlightSyntaxOptions", "Syntax"}
										],
										KnownHighlightChoices[]["Syntaxes"],
										$DefaultSyntax,
										Framed[
											Style[
												Row[{
													Dynamic @ Replace[
														CurrentValue[
															ParentCell @ EvaluationCell[],
															{TaggingRules, "HighlightSyntaxOptions", "Syntax"}
														],
														Inherited -> $DefaultSyntax
													],
													"\[VeryThinSpace]\[RightAngleBracket]"
												}],
												FontSize -> 11,
												FontWeight -> "Bold",
												FontColor -> GrayLevel[0.5]
											],
											FrameMargins -> 4,
											FrameStyle -> Directive[
												RGBColor[0.8549, 0.83137, 0.72549],
												AbsoluteThickness[1]
											],
											ImageMargins -> {{0, 3}, {0, 0}},
											RoundingRadius -> 3,
											Background -> LightYellow
										]
									],
									PopupMenu[
										Dynamic @ CurrentValue[
											ParentCell @ EvaluationCell[],
											{TaggingRules, "HighlightSyntaxOptions", "Theme"}
										],
										KnownHighlightChoices[]["Themes"],
										$DefaultTheme,
										Framed[
											Style[
												Row[{
													Dynamic @ Replace[
														CurrentValue[
															ParentCell @ EvaluationCell[],
															{TaggingRules, "HighlightSyntaxOptions", "Theme"}
														],
														Inherited -> $DefaultTheme
													],
													"\[VeryThinSpace]\[RightAngleBracket]"
												}],
												FontSize -> 11,
												FontWeight -> "Bold",
												FontColor -> GrayLevel[0.5]
											],
											FrameMargins -> 4,
											FrameStyle -> Directive[
												RGBColor[0.8549, 0.83137, 0.72549],
												AbsoluteThickness[1]
											],
											ImageMargins -> {{0, 3}, {0, 0}},
											RoundingRadius -> 3,
											Background -> LightYellow
										]
									]
								}],
								False -> ""
							},
							(* Display the Syntax picker popup if the mouse is
							   over the parent HighlightSynax cell or this
							   attached cell. *)
							Dynamic[
								CurrentValue[
									ParentCell @ EvaluationCell[],
									{TaggingRules, "parent_cell_is_hovered"}
								] || CurrentValue["MouseOver"]
							]
						],
						CellEventActions -> None,
						Background -> Transparent
					],
					{Right, Top},
					-2,
					{Right, Top}
				]
			),
			CellDynamicExpression -> Dynamic[
				(* If this HighlightSyntax cell is hovered, set this tagging rule
					so that the attached settings cell knows to display itself.
					Use `Inherited` in the False case so that this tagging rule
					is removed when hovering ends. *)
				CurrentValue[EvaluationCell[], {TaggingRules, "parent_cell_is_hovered"}] =
					If[TrueQ[CurrentValue["MouseOver"]], True, Inherited];
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