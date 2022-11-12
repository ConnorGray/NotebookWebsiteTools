BeginPackage["ConnorGray`NotebookWebsiteTools`Toolbar`"]

MakeStandardWebsiteNotebookToolbar

MakeToolbarButtonBoxes

$ExcludedColor = LightRed;
$ExcludedAccentColor = Darker[Red, 0.2]


Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`ErrorUtils`"]

Needs["ConnorGray`NotebookWebsiteTools`UI`"]

MakeStandardWebsiteNotebookToolbar[] := Module[{
	toolbarRow,
	toolbarCell
},
	toolbarRow = GridBox[{{
		MakeToolbarButtonBoxes[
			Graphics[{Text["TD"]}],
			"Exclude",
			"Toggle whether the selected cells are marked as Excluded.",
			Function[
				ToggleExcluded[ButtonNotebook[]]
			],
			"Queued",
			$ExcludedColor,
			$ExcludedAccentColor
		]
	}}];

	toolbarCell = Cell[
		BoxData[toolbarRow],
		Background -> GrayLevel[0.9],
		CellFrameMargins -> {{Inherited, Inherited}, {0, 0}}
	];

	{toolbarCell}
]

(*========================================================*)
(* Utilities                                              *)
(*========================================================*)

MakeToolbarButtonBoxes[
	icon_Graphics,
	label_?StringQ,
	tooltip_?StringQ,
	buttonFunction_Function,
	buttonMethod0 : _?StringQ | Automatic : Automatic,
	buttonDefaultBackground : _ : White,
	buttonAccentColor : _ : RGBColor["#60993e"]
] := Module[{
	buttonMethod = Replace[buttonMethod0, Automatic -> "Preemptive"]
},
	TemplateBox[
		{
			ToBoxes @ Show[icon, ImageSize -> 15, BaselinePosition -> Center],
			label,
			tooltip,
			buttonFunction,
			buttonMethod,
			buttonDefaultBackground,
			buttonAccentColor
		},
		"NotebookWebsiteTools:IconAndLabelButtonTemplate"
	]
]

AddUnmatchedArgumentsHandler[MakeToolbarButtonBoxes]

(*====================================*)

End[]

EndPackage[]