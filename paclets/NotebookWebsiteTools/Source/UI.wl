(*
	The functions in this file implement the UI actions of website notebooks.

	Each UI action corresponds to a call to one of the functions in this file.

	This convention ensures that each GUI action has a programmatic equivalent.

	NOTE:
		The functions and symbols from this context are embedded in the
		notebook GUI cell expressions. Backwards-incompatible changes to the
		exported symbols or their argument structures will break UI elements
		from notebooks created by previous versions of NotebookWebsiteTools
		(unless an appropriate UpdateNotebook[..] call has been implemented and
		made).
*)

BeginPackage["ConnorGray`NotebookWebsiteTools`UI`"]

ToggleExcluded::usage = "ToggleExcluded toggles the Excluded status of selected cells."
ToggleDraft::usage = "ToggleDraft toggles the Draft status of selected cells."

$GitHubIcon :=
	$GitHubIcon = Import[
		PacletObject["ConnorGray/NotebookWebsiteTools"]["AssetLocation", "GitHubIcon"]
	]

(*------------------------------------*)
(* Used in Cell style implementations *)
(*------------------------------------*)

InitializeHighlightSyntaxCell
HandleHighlightSyntaxCellEvent::usage = "HandleHighlightSyntaxCellEvent[cellObj, event]"
HighlightSyntaxCellDefaultBackground
KnownHighlightChoices

Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`"]
Needs["ConnorGray`NotebookWebsiteTools`Errors`"]
Needs["ConnorGray`NotebookWebsiteTools`LibraryLink`"]
Needs["ConnorGray`NotebookWebsiteTools`Utils`"]

(*====================================*)

ToggleExcluded[nb_NotebookObject] := Module[{
	cells = SelectedCells[nb]
},
	RaiseAssert[MatchQ[cells, {___CellObject}]];

	Scan[cell |-> toggleCellStyle[cell, "ConnorGray/Excluded"], cells];

	(* Return the cells that we modified. *)
	cells
]

SetFallthroughError[ToggleExcluded]

(*====================================*)

SetFallthroughError[ToggleDraft]

ToggleDraft[nb_NotebookObject] := Module[{
	cells = SelectedCells[nb]
},
	RaiseAssert[MatchQ[cells, {___CellObject}]];

	(* FIXME: Setting the 'Draft' style should remove the 'Excluded' style,
		and vice versa. *)
	Scan[cell |-> toggleCellStyle[cell, "ConnorGray/Draft"], cells];

	(* Return the cells that we modified. *)
	cells
]

(*========================================================*)
(* Syntax Highlighting                                    *)
(*========================================================*)

InitializeHighlightSyntaxCell[cellObj: _CellObject] := Module[{
	cellHoveredPane,
	attachedCellData,
	attachedCell
},
	(*----------------------------------------------*)
	(* Construct the HighlightSyntax menu cell expr *)
	(*----------------------------------------------*)

	cellHoveredPane = Row[{
		PopupMenu[
			Dynamic @ CurrentValue[
				ParentCell @ EvaluationCell[],
				{TaggingRules, "HighlightSyntaxOptions", "Syntax"}
			],
			ConnorGray`NotebookWebsiteTools`UI`KnownHighlightChoices[]["Syntaxes"],
			ConnorGray`NotebookWebsiteTools`$DefaultSyntax,
			Framed[
				Style[
					Row[{
						Dynamic @ Replace[
							CurrentValue[
								ParentCell @ EvaluationCell[],
								{TaggingRules, "HighlightSyntaxOptions", "Syntax"}
							],
							Inherited -> ConnorGray`NotebookWebsiteTools`$DefaultSyntax
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
			ConnorGray`NotebookWebsiteTools`UI`KnownHighlightChoices[]["Themes"],
			ConnorGray`NotebookWebsiteTools`$DefaultTheme,
			Framed[
				Style[
					Row[{
						Dynamic @ Replace[
							CurrentValue[
								ParentCell @ EvaluationCell[],
								{TaggingRules, "HighlightSyntaxOptions", "Theme"}
							],
							Inherited -> ConnorGray`NotebookWebsiteTools`$DefaultTheme
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
	}];

	attachedCellData = PaneSelector[
		{
			True -> cellHoveredPane,
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
	];

	attachedCell = Cell[
		BoxData @ ToBoxes @ attachedCellData,
		CellEventActions -> None,
		Background -> Transparent
	];

	(*---------------------------------------------*)
	(* Attach the menu to the HighlightSyntax cell *)
	(*---------------------------------------------*)

	AttachCell[
		cellObj,
		attachedCell,
		{Right, Top},
		-2,
		{Right, Top}
	];
]

HandleHighlightSyntaxCellEvent[cell_CellObject, "KeyDown"] := Module[{
	cellObj,
	originalCell,
	position,
	syntax, theme,
	plainTextContent,
	highlightedContent,
	newCell
},
	(* Optimization: If the key the user pressed was an arrow key (moving the
		cursor position), don't reparse and rehighlight the content of the
		cell. *)
	Replace[CurrentValue["EventKey"], {
		(* Arrow keys: up, down, left, right *)
		"\:f700" | "\:f701" | "\:f702" | "\:f703" :> (
			Return[Null]
		)
	}];

	cellObj = EvaluationCell[];

	(*---------------------------------------------------*)
	(* Extract current cell content and cursor position. *)
	(*---------------------------------------------------*)

	position = Replace[Developer`CellInformation[cellObj], {
		KeyValuePattern[{"CursorPosition" -> {c_, c_}}] :> c,
		other_ :> Raise[NotebookWebsiteError, "Unexpected cell information: ``", InputForm@other]
	}];

	{syntax, theme} = getHighlightSyntaxCellSyntaxAndTheme[cellObj];

	originalCell = NotebookRead[cellObj];
	plainTextContent = Replace[originalCell, {
		Cell[content_, ___] :> ConvertToString[content],
		other_ :> Raise[NotebookWebsiteError, "Unexpected NotebookRead result: ``", InputForm@other]
	}];

	RaiseAssert[StringQ[plainTextContent]];

	(*----------------------------------------------*)
	(* Compute the updated syntax highlighting data *)
	(*----------------------------------------------*)

	(* FIXME: Do ToBoxes on this; fix ConvertToString parsing. *)
	{background, highlightedContent} = ReplaceAll[
		CodeSyntaxHighlight[plainTextContent, syntax, theme],
		Style -> StyleBox
	];

	If[FailureQ[highlightedContent],
		(* TODO(polish): Present syntax highlighting errors (e.g. unknown syntax)
			in a better way. *)
		Print["ERROR: ", highlightedContent];
		Return[Null, Module];
	];

	(* Flatten the nesting that indicates separate lines. *)
	highlightedContent = Flatten[highlightedContent, 1];

	newCell = Replace[originalCell, {
		Cell[_, args___] :> (
			Cell[
				TextData[highlightedContent],
				args
			]
		),
		other_ :> Raise[
			NotebookWebsiteError,
			"Unexpected HighlightSyntax cell structure: ``",
			InputForm[originalCell]
		]
	}];

	(*------------------------------------------------------------*)
	(* Replace the current contents of the cell with new content. *)
	(*------------------------------------------------------------*)

	(* TODO: `ShowSelection -> False` currently has a bug for TextData content,
		and the selection of cell content is still shown. If that bug is fixed,
		the commented code below is more efficient than the
		NotebookWrite[cellObj, ..], which has to competely recreate the cell on
		every keystroke. *)
	(* SetOptions[EvaluationNotebook[], ShowSelection -> False];
	SelectionMove[cellObj, All, CellContents];
	NotebookWrite[EvaluationNotebook[], TextData[highlightedContent]];
	SetOptions[EvaluationNotebook[], ShowSelection -> True]; *)

	NotebookWrite[cellObj, newCell, All, AutoScroll -> False];

	(* Re-position the input cursor/caret. *)
	SelectionMove[EvaluationNotebook[], Before, CellContents, AutoScroll -> False];
	SelectionMove[EvaluationNotebook[], Next, Character, position, AutoScroll -> False];
	SelectionMove[EvaluationNotebook[], After, Character, AutoScroll -> True]
]

SetFallthroughError[HandleHighlightSyntaxCellEvent]

(*====================================*)

HighlightSyntaxCellDefaultBackground[cellObj_CellObject] := Module[{
	theme, color
},
	theme = getHighlightSyntaxCellSyntaxAndTheme[cellObj][[2]];

	RaiseAssert[StringQ[theme]];

	color = RaiseConfirm[GetLibraryFunction["theme_default_background"][theme]];

	RaiseAssert[MatchQ[color, _RGBColor | None], "bad color: ``", InputForm@color];

	color
]

SetFallthroughError[HighlightSyntaxCellDefaultBackground]

(*====================================*)

KnownHighlightChoices[] := GetLibraryFunction["known_highlight_choices"][]

(*====================================*)

getHighlightSyntaxCellSyntaxAndTheme[cellObj_CellObject] := Module[{},
	syntaxOptions = Replace[AbsoluteCurrentValue[cellObj, {TaggingRules, "HighlightSyntaxOptions"}], {
		Inherited -> <||>,
		opts_?ListQ :> Association[opts],
		assoc_?AssociationQ :> assoc,
		other_ :> Raise[
			NotebookWebsiteError,
			"HighlightSyntax cell has invalid non-Association value for \"HighlightSyntaxOptions\": ``",
			InputForm[other]
		]
	}];

	{
		Lookup[syntaxOptions, "Syntax", $DefaultSyntax],
		Lookup[syntaxOptions, "Theme", $DefaultTheme]
	}
]

(*========================================================*)
(* Utilities                                              *)
(*========================================================*)

toggleCellStyle[cell : _CellObject, style : _?StringQ] := Module[{
	(* Note: StyleNames is an undocumented special option that returns all of
		the "secondary" styles of a cell.

		For example, given a cell with the structure:

			Cell["Some content", "Title", "ConnorGray/Excluded"]

		The the primary style is the first style, `"Title"` in this case, and
		the secondary styles are `{"ConnorGray/Excluded"}`.
	*)
	currentStyles = RaiseConfirm @ Lookup[Options[cell, StyleNames], StyleNames]
},
	currentStyles = Replace[currentStyles, s_?StringQ :> {s}];

	RaiseAssert[MatchQ[currentStyles, {___?StringQ}], "currentStyles: ``", InputForm @ currentStyles];

	(* Note: This requires two SetOptions calls because setting this property
		to any value other than Inherited can only *add* styles to the cell,
		not remove them. So we first clear the secondary styles by setting
		this to Inherited, and then we add back only the styles we want to
		keep. *)
	SetOptions[cell, StyleNames -> Inherited];

	If[MemberQ[currentStyles, style],
		(* Remove all occurences of `style` from `currentStyles`. *)
		SetOptions[cell, StyleNames -> DeleteCases[currentStyles, style]];
	,
		(* Note:
			Place the newly added style at the end, to ensure it always
			gets the last say on what the styling of the cell is. E.g. if
			the style being added is "ConnorGray/Excluded", then the cell will
			always have a red background, even if it is also e.g. a "Program"
			cell (which normally have gray backgrounds).
		*)
		SetOptions[cell, StyleNames -> Append[currentStyles, style]];
	];
]

SetFallthroughError[toggleCellStyle]

(*====================================*)


End[]

EndPackage[]