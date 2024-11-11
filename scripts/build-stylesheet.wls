#!/usr/bin/env wolframscript

$pacletDirectory = FileNameJoin[{
	DirectoryName[$InputFileName, 2],
	"paclets",
	"NotebookWebsiteTools"
}]

PacletDirectoryLoad[$pacletDirectory]

Needs["ConnorGray`NotebookWebsiteTools`UI`"]
Needs["ConnorGray`NotebookWebsiteTools`UIUtils`"]

Print["INFO: Building using $InstallationDirectory == ", $InstallationDirectory]

(*========================================================*)
(* Define values used in Notebook stylesheet expression   *)
(*========================================================*)

(*====================================*)
(* Generic UI Utilities               *)
(*====================================*)

(*
	Parameters: {
		icon, label, tooltip, action,
		buttonMethod, buttonDefaultBackground, buttonAccentColor
	}

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

(*------------------------------------*)

(*
	Parameters: {
		icon, label, tooltip,
		choiceActions, buttonMethod,
		buttonDefaultBackground, buttonAccentColor
	}

	(TemplateBox DisplayFunction's are 'evaluated' by the FrontEnd, whose
	very basic evaluator doesn't not support named Function parameters.)
*)
$iconAndLabelDropdownTemplate = Function[
	DynamicModuleBox[{state = "default"},
		TagBox[
			ActionMenuBox[
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
							TemplateBox[
								{
									#2,
									"\"\[ThinSpace]\[ThinSpace]\[FilledDownTriangle]\""
								},
								"RowDefault"
							]
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
				#4,
				Appearance -> None,
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


(*========================================================*)
(* Create the stylesheet Notebook expression              *)
(*========================================================*)

notebookWebsiteToolsStylesheetNotebook = Notebook[{
	Cell[StyleData[StyleDefinitions -> "Default.nb"]],

	Cell[StyleData["Notebook"],
		DockedCells -> {
			Cell[
				BoxData @ GridBox[{{
					ItemBox[
						GridBox[{{
							TemplateBox[
								{
									GraphicsBox[{
										InsetBox[
											FormBox["\"TD\"", TraditionalForm],
											{0, 0}
										]
									}, {
										ImageSize -> 15,
										BaselinePosition -> Center
									}],
									"Exclude",
									"Toggle whether the selected cells are marked as Excluded.",
									Function[
										Needs["ConnorGray`NotebookWebsiteTools`UI`" -> None];
										ConnorGray`NotebookWebsiteTools`UI`ToggleExcluded[ButtonNotebook[]]
									],
									"Queued",
									RGBColor[1, 0.85, 0.85], (* $ExcludedColor = LightRed *)
									RGBColor[0.8, 0, 0] (* $ExcludedAccentColor = Darker[Red, 0.2] *)
								},
								"NotebookWebsiteTools:IconAndLabelButtonTemplate"
							],
							TemplateBox[
								{
									GraphicsBox[{
										InsetBox[
											FormBox["\"\"", TraditionalForm],
											{0, 0}
										]
									}, {
										ImageSize -> 15,
										BaselinePosition -> Center
									}],
									"Draft",
									"Toggle whether the selected cells are marked as Draft.",
									Function[
										Needs["ConnorGray`NotebookWebsiteTools`UI`" -> None];
										ConnorGray`NotebookWebsiteTools`UI`ToggleDraft[ButtonNotebook[]]
									],
									"Queued",
									RGBColor[1, 0.8, 0.6], (* Lighter draft color *)
									RGBColor[0.6, 0.4, 0.2] (* Draft color *)
								},
								"NotebookWebsiteTools:IconAndLabelButtonTemplate"
							],
							TemplateBox[
								{
									GraphicsBox[{
										InsetBox[
											FormBox["\"\"", TraditionalForm],
											{0, 0}
										]
									}, {
										ImageSize -> 15,
										BaselinePosition -> Center
									}],
									"Tab View",
									"Toggle whether the selected cell is a tab view section.",
									Function[
										Needs["ConnorGray`NotebookWebsiteTools`UI`" -> None];
										ConnorGray`NotebookWebsiteTools`UI`ToggleTabViewSection[ButtonNotebook[]]
									],
									"Queued",
									GrayLevel[0.9],
									GrayLevel[0.4]
								},
								"NotebookWebsiteTools:IconAndLabelButtonTemplate"
							]
						}}],
						(* Force this item to take up the maximum possible width. This
						makes items that come after this appear aligned to the right
						side of the notebook. *)
						ItemSize -> Fit
					],
					TemplateBox[
						{
							GraphicsBox[{
								InsetBox[
									FormBox["\"\"", TraditionalForm],
									{0, 0}
								]
							}, {
								ImageSize -> 15,
								BaselinePosition -> Center
							}],
							"Preview",
							"View a preview of the built webpage.",
							{
								ToBoxes["Published"] :> (
									Needs["ConnorGray`NotebookWebsiteTools`UI`" -> None];
									ConnorGray`NotebookWebsiteTools`UI`ShowPreview[
										ButtonNotebook[]
									];
								),
								ToBoxes["Draft"] :> (
									Needs["ConnorGray`NotebookWebsiteTools`UI`" -> None];
									ConnorGray`NotebookWebsiteTools`UI`ShowPreview[
										ButtonNotebook[],
										"IncludeDrafts" -> True
									];
								),
								Delimiter,
								ToBoxes["Published \[LongDash] Single File"] :> (
									Needs["ConnorGray`NotebookWebsiteTools`UI`" -> None];
									ConnorGray`NotebookWebsiteTools`UI`ShowPreview[
										ButtonNotebook[],
										"EmbedImages" -> True,
										"EmbedCSS" -> True
									];
								),
								ToBoxes["Draft \[LongDash] Single File"] :> (
									Needs["ConnorGray`NotebookWebsiteTools`UI`" -> None];
									ConnorGray`NotebookWebsiteTools`UI`ShowPreview[
										ButtonNotebook[],
										"IncludeDrafts" -> True,
										"EmbedImages" -> True,
										"EmbedCSS" -> True
									];
								)
							},
							"Queued",
							RGBColor["#8ecae6"],
							RGBColor["#023047"]
						},
						"NotebookWebsiteTools:IconAndLabelDropdownTemplate"
					],
					ToBoxes @ Spacer[5],
					StyleBox[
						"Status: ",
						"Text",
						GrayLevel[0.5]
					],
					PopupMenuBox[
						Dynamic[
							CurrentValue[
								EvaluationNotebook[],
								{TaggingRules, "ConnorGray/NotebookWebsiteTools", "DocumentStatus"}
							]
						],
						{
							"Draft" -> StyleBox["\"Draft\"", Bold, RGBColor[0.6, 0.4, 0.2]], (* Brown *)
							"Published" -> StyleBox["\"Published\"", Bold, RGBColor[0, Rational[2, 3], 0]], (* Darker[Green] *)
							"Excluded" -> StyleBox["\"Excluded\"", Bold, RGBColor[Rational[2, 3], 0, 0]] (* Darker[Red] *)
						},
						(* If no known document status value is selected, show
							a long dash to indicate that this value is missing. *)
						ItemBox[
							StyleBox["\[LongDash]", Bold, GrayLevel[0.4], Alignment -> Center],
							Alignment -> Center
						]
					]
				}},
					GridBoxAlignment -> {
						"Columns" -> {{Left}},
						"ColumnsIndexed" -> {-1 -> Right},
						"Rows" -> {{Center}}
					}
				],
				CellFrameMargins -> {{Inherited, Inherited}, {0, 0}}
			]
		}
	],

	(*================================*)
	(* Deprecated cell styles         *)
	(*================================*)

	Cell[
		StyleData[
			"Excluded",
			StyleDefinitions -> StyleData["ConnorGray/Excluded"]
		],
		MenuSortingValue -> None
	],

	Cell[
		StyleData[
			"LiteralHTML",
			StyleDefinitions -> StyleData["ConnorGray/LiteralHTML"]
		],
		MenuSortingValue -> None
	],

	Cell[
		StyleData[
			"ComputedHTML",
			StyleDefinitions -> StyleData["ConnorGray/ComputedHTML"]
		],
		MenuSortingValue -> None
	],

	Cell[
		StyleData[
			"HighlightSyntax",
			StyleDefinitions -> StyleData["ConnorGray/HighlightSyntax"]
		],
		MenuSortingValue -> None
	],

	(*====================*)
	(* Content Processing *)
	(*====================*)

	Cell[StyleData["ConnorGray/Excluded"],
		MenuSortingValue -> 2500,
		Background -> RGBColor[1, 0.85, 0.85], (* $ExcludedColor = LightRed *)
		CellFrame -> {{3, False}, {False, False}},
		CellFrameColor -> RGBColor[0.8, 0, 0] (* Darker[Red, 0.2] *)
	],

	Cell[StyleData["ConnorGray/Draft"],
		MenuSortingValue -> 2500,
		Background -> RGBColor[1, 0.8, 0.6], (* Lighter draft color *)
		CellFrame -> {{3, False}, {False, False}},
		CellFrameColor -> RGBColor[0.6, 0.4, 0.2] (* Draft color *)
	],

	(*======================*)
	(* Special Header Cells *)
	(*======================*)

	Cell[StyleData["ConnorGray/TabViewSection"],
		CellDingbat -> (
			Cell @ BoxData @ DynamicBox[
				ToBoxes[
					Refresh[
						Image[
							Import[
								PacletObject["ConnorGray/NotebookWebsiteTools"][
									"AssetLocation", "TabViewIcon"
								]
							],
							ImageSize -> 18,
							BaselinePosition -> Scaled[0]
						],
						None
					],
					StandardForm
				]
			]
		)
	],

	(*===============*)
	(* Special Cells *)
	(*===============*)

	Cell[StyleData["ConnorGray/LiteralHTML"],
		MenuSortingValue -> 2500 + 25,
		CellMargins->{{66, 10}, {8, 8}},
		Background -> GrayLevel[0.95],
		CellFrame -> {{3, False}, {False, False}},
		CellFrameColor -> GrayLevel[0.8],
		CellDingbat -> StyleBox["\"</>\"", Bold, GrayLevel[0.6], ShowStringCharacters -> False],
		(* Don't word wrap content in LiteralHTML cells, which is
			typically pre-formatted code input. *)
		PageWidth -> Infinity,
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
	Cell[StyleData["ConnorGray/ComputedHTML", StyleDefinitions -> StyleData["Input"]],
		MenuSortingValue -> 2500 + 50,
		CellDingbat -> StyleBox["\"</>\"", Bold, GrayLevel[0.6], ShowStringCharacters -> False]
	],

	Cell[StyleData["ConnorGray/HighlightSyntax"],
		MenuSortingValue -> 2500 + 75,
		CellMargins->{{66, 10}, {8, 8}},
		CellFrame -> {{3, False}, {False, False}},
		CellFrameColor -> GrayLevel[0.8],
		(* Overridden whenever the user changes the syntax coloring theme. *)
		Background -> HighlightSyntaxCellDefaultBackground[],
		CellDingbat -> Cell[
			BoxData @ ToBoxes @ MakeMenuCellDingbat[
				Graphics @ {
					Text @ Style["{}", FontSize -> 16, FontFamily -> "PT Mono"]
				},
				Function[
					Needs["ConnorGray`NotebookWebsiteTools`UI`" -> None];
					ConnorGray`NotebookWebsiteTools`UI`MakeHighlightSyntaxCellMenu[##]
				],
				AutoAction -> True
			],
			Background -> White
		],
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
		(* Don't word wrap content in HighlightSyntax cells, which is
			typically pre-formatted code input. *)
		PageWidth -> Infinity,
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
				ConnorGray`NotebookWebsiteTools`UI`HandleHighlightSyntaxCellEvent[EvaluationCell[], "KeyDown"]
			)
		}
	],

	(*=====================================*)
	(* Inline Icon Link TemplateBox styles *)
	(*=====================================*)

	Cell[
		StyleData["ConnorGray/DynamicIconLink"],
		TemplateBoxOptions -> {
			(*
				Parameters: {icon, label, URL}

				(TemplateBox DisplayFunction's are 'evaluated' by the FrontEnd, whose
				very basic evaluator doesn't not support named Function parameters.)
			*)
			DisplayFunction -> Function[
				Cell[
					BoxData @ TemplateBox[
						{
							GridBox[{{
								DynamicBox[
									ToBoxes[
										Refresh[
											#1,
											None
										],
										StandardForm
									]
								],
								#2
							}},
								GridBoxSpacings -> {"Columns" -> {{0.15}}}
							],
							{URL[#3], None},
							#3,
							"Hyperlink",
							{"HyperlinkActive"},
							BaseStyle -> {"Text"}
						},
						"HyperlinkTemplate"
					],
					Editable -> False
				]
			]
		}
	],

	Cell[
		StyleData["ConnorGray/GitHubLink"],
		TemplateBoxOptions -> {
			(*
				Parameters: {label, URL}

				(TemplateBox DisplayFunction's are 'evaluated' by the FrontEnd, whose
				very basic evaluator doesn't not support named Function parameters.)
			*)
			DisplayFunction -> Function[
				Cell @ BoxData @ TemplateBox[{
					Image[
						Import[
							PacletObject["ConnorGray/NotebookWebsiteTools"][
								"AssetLocation", "GitHubIcon"
							]
						],
						ImageSize -> 18,
						BaselinePosition -> Scaled[0.15]
					],
					#1,
					#2
				}, "ConnorGray/DynamicIconLink"]
			]
		}
	],

	Cell[
		StyleData["ConnorGray/PacletLink"],
		TemplateBoxOptions -> {
			(*
				Parameters: {label, URL}

				(TemplateBox DisplayFunction's are 'evaluated' by the FrontEnd, whose
				very basic evaluator doesn't not support named Function parameters.)
			*)
			DisplayFunction -> Function[
				Cell @ BoxData @ TemplateBox[{
					Show[
						Import[
							PacletObject["ConnorGray/NotebookWebsiteTools"][
								"AssetLocation", "PacletIcon"
							]
						],
						ImageSize -> 18,
						BaselinePosition -> Scaled[0.18]
					],
					#1,
					#2
				}, "ConnorGray/DynamicIconLink"]
			]
		}
	],

	Cell[
		StyleData["ConnorGray/RustCrateLink"],
		TemplateBoxOptions -> {
			(*
				Parameters: {label, URL}

				(TemplateBox DisplayFunction's are 'evaluated' by the FrontEnd, whose
				very basic evaluator doesn't not support named Function parameters.)
			*)
			DisplayFunction -> Function[
				Cell @ BoxData @ TemplateBox[{
					Image[
						Import[
							PacletObject["ConnorGray/NotebookWebsiteTools"][
								"AssetLocation", "RustIcon"
							]
						],
						ImageSize -> 18,
						BaselinePosition -> Scaled[0.15]
					],
					#1,
					#2
				}, "ConnorGray/DynamicIconLink"]
			]
		}
	],

	(*===========================================*)
	(* Generic toolbar button TemplateBox styles *)
	(*===========================================*)

	Cell[
		StyleData["NotebookWebsiteTools:IconAndLabelButtonTemplate"],
		TemplateBoxOptions -> {
			DisplayFunction -> $iconAndLabelButtonTemplate
		}
	],
	Cell[
		StyleData["NotebookWebsiteTools:IconAndLabelDropdownTemplate"],
		TemplateBoxOptions -> {
			DisplayFunction -> $iconAndLabelDropdownTemplate
		}
	],

	(*================================*)
	(* Generic UIUtils Styles         *)
	(*================================*)

	Splice[UIUtilsStyleDefinitions[]]
},
	StyleDefinitions -> "PrivateStylesheetFormatting.nb"
]

(*========================================================*)
(* Export the generated stylesheet notebook               *)
(*========================================================*)

$styleSheetTargetFile = FileNameJoin[{
	$pacletDirectory,
	"FrontEnd",
	"StyleSheets",
	"ConnorGray",
	"NotebookWebsiteTools.nb"
}]

Export[$styleSheetTargetFile, notebookWebsiteToolsStylesheetNotebook, "NB"]
