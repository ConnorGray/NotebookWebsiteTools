BeginPackage["ConnorGray`NotebookWebsiteTools`UIUtils`"]

MakeMenu
AttachSubmenu

(* Custom Menu Style Utilities *)
MakeMenuCellDingbat

(* Base Menu Style Definitions *)
UIUtilsStyleDefinitions


(*====================================*)

Begin["`Private`"]

Needs["GeneralUtilities`" -> "GU`"]

Needs["ConnorGray`NotebookWebsiteTools`Errors`"]

(*========================================================*)
(* Style Definitions                                      *)
(*========================================================*)

GU`SetUsage[UIUtilsStyleDefinitions, "
UIUtilsStyleDefinitions[] returns a list of style definition cells, suitable
for splicing into a stylesheet notebook.
"]

SetFallthroughError[UIUtilsStyleDefinitions]

(* These style definitions gets included in the built NotebookWebsiteTools.nb *)
UIUtilsStyleDefinitions[] := {
	Cell[
		StyleData["ConnorGray/MenuItemDelimiter"],
		TemplateBoxOptions -> {
			DisplayFunction -> Function[
				PaneBox[
					StyleBox[
						GraphicsBox[
							{
								CapForm["Round"],
								GrayLevel[0.9],
								AbsoluteThickness[2],
								LineBox @ {{-1, 0}, {1, 0}}
							},
							AspectRatio  -> Full,
							ImageMargins -> {{0, 0}, {2, 2}},
							ImagePadding -> {{5, 5}, {0, 0}},
							ImageSize    -> {Full, 2},
							PlotRange    -> {{-1, 1}, {-1, 1}}
						],
						LineIndent -> 0
					],
					BaselinePosition -> Baseline,
					FrameMargins     -> 0,
					ImageMargins     -> 0,
					ImageSize        -> Full
				]
			]
		}
	],

	Cell[
		StyleData["ConnorGray/ButtonMenuItem"],
		TemplateBoxOptions -> {
			DisplayFunction -> Function @ ButtonBox[
				TemplateBox[
					{
						TagBox[
							GridBox[
								{
									{
										#1,
										TemplateBox[{7}, "Spacer1"],
										PaneBox[
											StyleBox[#2, "ConnorGray/MenuLabel"],
											FrameMargins     -> 0,
											ImageMargins     -> 0,
											BaselinePosition -> Baseline,
											ImageSize        -> Full
										]
									}
								},
								GridBoxAlignment -> {
									"Columns" -> {{Left}},
									"Rows" -> {{Baseline}}
								},
								AutoDelete       -> False,
								GridBoxItemSize  -> {
									"Columns" -> {{Automatic}},
									"Rows" -> {{Automatic}}
								},
								GridBoxSpacings  -> {
									"Columns" -> {{0}},
									"Rows" -> {{0}}
								}
							],
							"Grid"
						],
						FrameStyle -> Dynamic @ If[
							CurrentValue["MouseOver"],
							GrayLevel[0.8],
							GrayLevel[0.98]
						],
						RoundingRadius -> 0,
						FrameMargins   -> {{5, 2}, {2, 2}},
						ImageSize      -> Full,
						ImageMargins   -> {{0, 0}, {0, 0}},
						Background     -> Dynamic @ If[
							CurrentValue["MouseOver"],
							GrayLevel[1],
							GrayLevel[0.98]
						]
					},
					"Highlighted"
				],
				ButtonFunction :> ReleaseHold @ #3,
				(* PRECOMMIT *)
				Appearance -> None,
				(* Appearance     -> $suppressButtonAppearance, *)
				Method         -> "Queued",
				Evaluator      -> Automatic
			]
		}
	],

	Cell[
		StyleData["ConnorGray/MenuSection"],
		TemplateBoxOptions -> {
			DisplayFunction -> Function @ TemplateBox[
				{
					TagBox[
						GridBox[
							{
								{
									PaneBox[
										StyleBox[#1, "ConnorGray/MenuSectionLabel"],
										FrameMargins     -> 0,
										ImageMargins     -> 0,
										BaselinePosition -> Baseline,
										ImageSize        -> Full
									]
								}
							},
							GridBoxAlignment -> {
								"Columns" -> {{Left}},
								"Rows" -> {{Baseline}}
							},
							AutoDelete       -> False,
							GridBoxItemSize  -> {
								"Columns" -> {{Automatic}},
								"Rows" -> {{Automatic}}},
							GridBoxSpacings  -> {
								"Columns" -> {{0}},
								"Rows" -> {{0}}
							}
						],
						"Grid"
					],
					Background     -> GrayLevel[ 0.937 ],
					FrameMargins   -> {{5, 2}, {2, 2}},
					FrameStyle     -> None,
					ImageMargins   -> {{0, 0}, {0, 0}},
					ImageSize      -> Full,
					RoundingRadius -> 0
				},
				"Highlighted"
			]
		}
	],

	Cell[
		StyleData["ConnorGray/MenuLabel"],
		FontColor       -> GrayLevel[0.2],
		FontFamily      -> "Source Sans Pro",
		FontSize        -> 13,
		FontWeight      -> Plain,
		LineBreakWithin -> False,
		LineIndent      -> 0
	],

	Cell[
		StyleData[
			"ConnorGray/MenuSectionLabel",
			StyleDefinitions -> StyleData["ConnorGray/MenuLabel"]
		],
		FontSize  -> 13,
		FontColor -> GrayLevel[0.35]
	]
}

(*========================================================*)
(* Programmatic Interface                                 *)
(*========================================================*)



GU`SetUsage[MakeMenuCellDingbat, "
PRECOMMIT"]

GU`SetUsage[MakeMenu, "
MakeMenu[items$$] returns an expression representing a menu of actions.

The generated menu expression may depend on styles from the NotebookWebsiteTools stylesheet.
"]

GU`SetUsage[AttachSubmenu, "
AttachSubmenu[parentMenu$, submenu$] attaches submenu$ to parentMenu$, taking
care to attach to the left or right side based on heuristic for available space.
"]

(*====================================*)

SetFallthroughError[MakeMenuCellDingbat]

MakeMenuCellDingbat[
	menuIcon: _,
	makeMenuCallback: _
] := Module[{
	button
},
	button = Button[
		Framed[
			Pane[
				menuIcon,
				Alignment -> {Center, Center},
				ImageSize -> {25, 25},
				ImageSizeAction -> "ShrinkToFit"
			],
			RoundingRadius -> 2,
			FrameStyle -> Dynamic[
				If[CurrentValue["MouseOver"], GrayLevel[0.74902], None]
			],
			Background -> Dynamic[
				If[CurrentValue["MouseOver"], GrayLevel[0.960784], None]
			],
			FrameMargins -> 0,
			ImageMargins -> 0,
			ContentPadding -> False
		],
		(
			AttachCell[
				EvaluationCell[],
				makeMenuCallback[
					ParentCell[EvaluationCell[]],
					EvaluationCell[]
				],
				{Left, Bottom},
				Offset[{0, 0}, {Left, Top}],
				{Left, Top},
				RemovalConditions -> {"EvaluatorQuit", "MouseClickOutside"}
			];
		),
		(* PRECOMMIT *)
		(* Appearance -> $suppressButtonAppearance, *)
		Appearance -> None,
		ImageMargins -> 0,
		FrameMargins -> 0,
		ContentPadding -> False
	];

	button
]

(*====================================*)

SetFallthroughError[MakeMenu]

MakeMenu[
	items: _List,
	frameColor0: _ : Automatic,
	width: _ : 240
] := Module[{
	frameColor = Replace[frameColor0, Automatic -> GrayLevel[0.85]]
},
	Pane[
		RawBoxes @ TemplateBox[
			{
				ToBoxes @ Column[
					menuItem /@ items,
					ItemSize -> Automatic,
					Spacings -> 0,
					Alignment -> Left
				],
				FrameMargins   -> 3,
				Background     -> GrayLevel[0.98],
				RoundingRadius -> 3,
				FrameStyle     -> Directive[AbsoluteThickness[1], frameColor],
				ImageMargins   -> 0
			},
			"Highlighted"
		],
		ImageSize -> {width, Automatic}
	]
]

(*====================================*)

SetFallthroughError[menuItem]

menuItem[item: _] := ConfirmReplace[item, {
	Delimiter :>
		RawBoxes @ TemplateBox[{}, "ConnorGray/MenuItemDelimiter"],

	RuleDelayed[display_, action_] :> Module[{
		selectedState, icon, label
	},
		{selectedState, icon, label} = ConfirmReplace[display, {
			{ss: _, i: _, l: _} :> {ss, i, l},
			{i: _, l: _} :> {None, i, l},
			l: _ :> {None, None, l}
		}];

		icon //= Replace[None -> Graphics[{}, ImageSize -> 0]];

		icon = ConfirmReplace[selectedState, {
			None | False :> icon,
			True | "SelectedInline" :> Row[{
				"\[Checkmark]",
				" ",
				icon
			}],
			"SelectedInherited" :> Row[{
				Style["\[Checkmark]", FontColor -> GrayLevel[0.75]],
				" ",
				icon
			}],
			other: _ :> Raise[
				NotebookWebsiteError,
				"Invalid specification for action menu item selected state: ``",
				InputForm[other]
			]
		}];

		RawBoxes @ TemplateBox[
			{ToBoxes @ icon, ToBoxes @ label, Hold[action]},
			"ConnorGray/ButtonMenuItem"
		]
	],

	{"ActionMenu", label: _, actions: _List} :> Module[{
		(* TODO: Handle lack of icon for these menus better. *)
		icon = Graphics[{}, ImageSize -> 0],
		label
	},

		RawBoxes @ TemplateBox[
			{ToBoxes @ icon, ToBoxes @ label, Hold[action]},
			"ConnorGray/ActionMenuItem"
		]
	],

	content: _Item :>
		content,

	section: _ :> (
		RawBoxes @ TemplateBox[{ToBoxes @ section}, "ConnorGray/MenuSection"]
	)
}]

(*====================================*)

AttachSubmenu[
	parentMenu: _CellObject,
	submenu: _
] := With[{
	mouseX = MousePosition["WindowScaled"][[1]]
}, {
	(* Note: Depending on the X coordinate of the users mouse
		when they click the 'Advanced Settings' button, either
		show the attached submenu to the left or right of the
		outer menu. This ensures that this submenu doesn't touch
		the right edge of the notebook window when it is opened
		from the 'Chat Settings' notebook toolbar. *)
	positions = If[
		TrueQ[mouseX < 0.5],
		{
			{Right, Bottom},
			(* PRECOMMIT: Bottom? *)
			{Left, Center}
		},
		{
			{Left, Bottom},
			(* PRECOMMIT: Bottom? *)
			{Right, Center}
		}
	]
},
	AttachCell[
		EvaluationCell[],
		submenu,
		positions[[1]],
		(* PRECOMMIT: 50? *)
		(* {100, 100}, *)
		0,
		positions[[2]],
		RemovalConditions -> "MouseExit"
	]
]


(*========================================================*)

End[]

EndPackage[]