#!/usr/bin/env wolframscript

$pacletDirectory = FileNameJoin[{
	DirectoryName[$InputFileName, 2],
	"paclets",
	"NotebookWebsiteTools"
}]

PacletDirectoryLoad[$pacletDirectory]

Needs["ConnorGray`Utilities`" -> "CGUtils`"]

Needs["ConnorGray`NotebookWebsiteTools`UI`"]

Print["INFO: Building using $InstallationDirectory == ", $InstallationDirectory]

(*========================================================*)
(* Define values used in Notebook stylesheet expression   *)
(*========================================================*)

(*====================================*)
(* Generic UI Utilities               *)
(*====================================*)

(*========================================================*)
(* Create the stylesheet Notebook expression              *)
(*========================================================*)

notebookWebsiteToolsStylesheetNotebook = Notebook[{
	Cell[StyleData[StyleDefinitions -> "Default.nb"]],

	(* Depend on ConnorGray/Utilities styles. *)
	Cell[StyleData[StyleDefinitions -> FrontEnd`FileName[
		{"ConnorGray"},
		"Utilities.nb",
		CharacterEncoding -> "UTF-8"
	]]],

	Cell[StyleData["Notebook"],
		DockedCells -> {
			Cell[
				BoxData @ GridBox[{{
					ItemBox[
						GridBox[{{
							CGUtils`ToolbarButtonBoxes[
								"Exclude",
								Function[
									Needs["ConnorGray`NotebookWebsiteTools`UI`" -> None];
									ConnorGray`NotebookWebsiteTools`UI`ToggleExcluded[ButtonNotebook[]]
								],
								{
									RGBColor[0.8, 0, 0], (* $ExcludedAccentColor = Darker[Red, 0.2] *)
									RGBColor[1, 0.85, 0.85] (* $ExcludedColor = LightRed *)
								},
								Method -> "Queued",
								Tooltip -> "Toggle whether the selected cells are marked as Excluded."
							],
							CGUtils`ToolbarButtonBoxes[
								"Draft",
								Function[
									Needs["ConnorGray`NotebookWebsiteTools`UI`" -> None];
									ConnorGray`NotebookWebsiteTools`UI`ToggleDraft[ButtonNotebook[]]
								],
								{
									RGBColor[0.6, 0.4, 0.2], (* Draft color *)
									RGBColor[1, 0.8, 0.6] (* Lighter draft color *)
								},
								Method -> "Queued",
								Tooltip -> "Toggle whether the selected cells are marked as Draft."
							],
							CGUtils`ToolbarButtonBoxes[
								{
									Image[
										Import[
											PacletObject["ConnorGray/NotebookWebsiteTools"][
												"AssetLocation", "TabViewIcon"
											]
										],
										ImageSize -> 18,
										BaselinePosition -> Scaled[0]
									],
									"Tab View"
								},
								Function[
									Needs["ConnorGray`NotebookWebsiteTools`UI`" -> None];
									ConnorGray`NotebookWebsiteTools`UI`ToggleTabViewSection[ButtonNotebook[]]
								],
								{
									GrayLevel[0.4],
									GrayLevel[0.9]
								},
								Method -> "Queued",
								Tooltip -> "Toggle whether the selected cell is a tab view section."
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
									Needs["ConnorGray`Utilities`" -> None];
									Needs["ConnorGray`NotebookWebsiteTools`UI`" -> None];
									ConnorGray`Utilities`HandleUIFailure[
										ConnorGray`NotebookWebsiteTools`UI`ShowPreview[
											ButtonNotebook[],
											(* Ensure this notebook is built as if it
												was 'Published', even if it is 'Draft'. *)
											"BuildType" -> "PreviewDraftsAsPublished"
										]
									]
								),
								ToBoxes["Draft"] :> (
									Needs["ConnorGray`Utilities`" -> None];
									Needs["ConnorGray`NotebookWebsiteTools`UI`" -> None];
									ConnorGray`Utilities`HandleUIFailure[
										ConnorGray`NotebookWebsiteTools`UI`ShowPreview[
											ButtonNotebook[],
											"BuildType" -> "Drafts"
										]
									]
								),
								Delimiter,
								ToBoxes["Published \[LongDash] Single File"] :> (
									Needs["ConnorGray`Utilities`" -> None];
									Needs["ConnorGray`NotebookWebsiteTools`UI`" -> None];
									ConnorGray`Utilities`HandleUIFailure[
										ConnorGray`NotebookWebsiteTools`UI`ShowPreview[
											ButtonNotebook[],
											"EmbedImages" -> True,
											"EmbedCSS" -> True
										]
									]
								),
								ToBoxes["Draft \[LongDash] Single File"] :> (
									Needs["ConnorGray`Utilities`" -> None];
									Needs["ConnorGray`NotebookWebsiteTools`UI`" -> None];
									ConnorGray`Utilities`HandleUIFailure[
										ConnorGray`NotebookWebsiteTools`UI`ShowPreview[
											ButtonNotebook[],
											"BuildType" -> "Drafts",
											"EmbedImages" -> True,
											"EmbedCSS" -> True
										]
									]
								)
							},
							"Queued",
							RGBColor["#8ecae6"],
							RGBColor["#023047"]
						},
						"ConnorGray/IconAndLabelDropdownTemplate"
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
	(* Common Markup *)
	(*================================*)

	Cell[
		StyleData[
			"ConnorGray/BlockQuote",
			StyleDefinitions -> StyleData["Text"]
		],
		Background -> GrayLevel[0.9],
		MenuSortingValue -> 2500 + 75
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
	(* TODO: Maybe 'OutputHTML'? *)
	Cell[StyleData["ConnorGray/ComputedHTML", StyleDefinitions -> StyleData["Input"]],
		MenuSortingValue -> 2500 + 50,
		CellDingbat -> StyleBox["\"</>\"", Bold, GrayLevel[0.6], ShowStringCharacters -> False]
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
	]
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
