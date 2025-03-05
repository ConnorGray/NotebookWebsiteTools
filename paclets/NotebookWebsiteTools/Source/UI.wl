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
ToggleTabViewSection

ShowPreview

$GitHubIcon :=
	$GitHubIcon = Import[
		PacletObject["ConnorGray/NotebookWebsiteTools"]["AssetLocation", "GitHubIcon"]
	]

(*------------------------------------*)
(* Used in Cell style implementations *)
(*------------------------------------*)

MakeHighlightSyntaxCellMenu

HandleHighlightSyntaxCellEvent::usage = "HandleHighlightSyntaxCellEvent[cellObj, event]"
RedrawHighlightSyntaxCell
HighlightSyntaxCellDefaultBackground
KnownHighlightChoices

Begin["`Private`"]

Needs["ConnorGray`Utilities`"]

Needs["ConnorGray`NotebookWebsiteTools`"]
Needs["ConnorGray`NotebookWebsiteTools`Errors`"]
Needs["ConnorGray`NotebookWebsiteTools`LibraryLink`"]
Needs["ConnorGray`NotebookWebsiteTools`Utils`"]
Needs["ConnorGray`NotebookWebsiteTools`UIUtils`"]

(*====================================*)

ToggleExcluded[nb: _NotebookObject] := Module[{
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

ToggleDraft[nb: _NotebookObject] := Module[{
	cells = SelectedCells[nb]
},
	RaiseAssert[MatchQ[cells, {___CellObject}]];

	(* FIXME: Setting the 'Draft' style should remove the 'Excluded' style,
		and vice versa. *)
	Scan[cell |-> toggleCellStyle[cell, "ConnorGray/Draft"], cells];

	(* Return the cells that we modified. *)
	cells
]

(*====================================*)

SetFallthroughError[ToggleTabViewSection]

ToggleTabViewSection[nb: _NotebookObject] := Module[{
	cells = SelectedCells[nb]
},
	RaiseAssert[MatchQ[cells, {___CellObject}]];

	Scan[cell |-> toggleCellStyle[cell, "ConnorGray/TabViewSection"], cells];

	(* Return the cells that we modified. *)
	cells
]

(*====================================*)

Options[ShowPreview] = {
	"IncludeDrafts" -> False,
	"EmbedImages" -> False
}

SetFallthroughError[ShowPreview]

ShowPreview[
	nbObj0: _NotebookObject,
	opts:OptionsPattern[]
] := Module[{
	nbPath = NotebookFileName[nbObj0],
	nbObj,
	originalWebsiteDir,
	relativePath,
	configFile,
	tmpWebsiteDir,
	result
},
	{originalWebsiteDir, relativePath} = ConfirmReplace[FileNameSplit[nbPath], {
		{path: ___, "Content", rel: ___} :> {
			FileNameJoin[{path}],
			FileNameJoin[{"Content", rel}]
		}
	}];

	RaiseAssert[DirectoryQ[originalWebsiteDir]];

	configFile = FileNameJoin[{originalWebsiteDir, "NotebookWebsite.wl"}];

	(*---------------------------------------*)
	(* Populate temporary website directory. *)
	(*---------------------------------------*)

	tmpWebsiteDir = RaiseConfirm @ CreateDirectory[];

	(* Create parent directory of the saved temporary notebook. *)
	RaiseConfirm @ CreateDirectory[
		FileNameJoin[{tmpWebsiteDir, FileNameDrop @ relativePath}],
		CreateIntermediateDirectories -> True
	];

	(*
		Create an entirely indepedent NotebookObject. This has two advantages:

		1. We can modify this copy without modifying the original notebook.
		2. We preview the latest in-memory changes, which we wouldn't get if
			we used CopyFile to copy only the latest _saved_ changes to the
			temp build directory.
	*)
	nbObj = NotebookPut[NotebookGet[nbObj0], Visible -> False];

	RaiseAssert[MatchQ[nbObj, _NotebookObject]];

	(* Pretend that the document is in the "Published" status, so that
		using the 'Preview /> Published' menu item shows the state of the
		document "as if" it was Published (even if the document as a whole is
		still in "Draft" mode) in the current state, with Draft cells not
		included. *)
	CurrentValue[
		nbObj,
		{TaggingRules, "ConnorGray/NotebookWebsiteTools", "DocumentStatus"}
	] = "Published";

	(* Save the temporary notebook out to disk in the temporary directory. *)
	RaiseConfirm @ NotebookSave[
		nbObj,
		FileNameJoin[{tmpWebsiteDir, relativePath}]
	];
	NotebookClose[nbObj];

	If[FileExistsQ[configFile],
		RaiseConfirm @ CopyFile[
			configFile,
			FileNameJoin[{tmpWebsiteDir, "NotebookWebsite.wl"}]
		];
	];

	(* Print @ Diagrams`FileSystemTreeDiagram[
		tmpWebsiteDir,
		"ASCIIGraphics",
		ItemDisplayFunction -> FileNameTake @* Last
	]; *)

	(*--------------------------------*)
	(* Build the temporary website    *)
	(*--------------------------------*)

	result = NotebookWebsiteBuild[
		tmpWebsiteDir,
		ForwardOptions[opts]
	];

	(* Print[result]; *)

	ConfirmReplace[result, {
		Success["NotebookWebsiteBuild", KeyValuePattern[{
			"OutputHTMLFiles" -> {file: _File}
		}]] :> SystemOpen[file]
	}];
]

(*========================================================*)
(* Syntax Highlighting                                    *)
(*========================================================*)

SetFallthroughError[MakeHighlightSyntaxCellMenu]

MakeHighlightSyntaxCellMenu[
	syntaxCellObj: _CellObject,
	dingbatObj: _CellObject
] := With[{
	$syntaxSpec = {TaggingRules, "HighlightSyntaxOptions", "Syntax"},
	$themeSpec  = {TaggingRules, "HighlightSyntaxOptions", "Theme"}
},
	Module[{
	menuItems,
	currentSyntax,
	currentTheme,
	syntaxes,
	themes
},
	{currentSyntax, currentTheme} = getHighlightSyntaxCellSyntaxAndTheme[
		syntaxCellObj
	];

	{syntaxes, themes} = Lookup[
		KnownHighlightChoices[],
		{"Syntaxes", "Themes"}
	];

	syntaxes = Sort[syntaxes];
	themes = Sort[themes];

	menuItems = With[{
		syntaxChoices = Map[
			choice |-> (
				styleListItem[currentSyntax, choice] :> (
					CurrentValue[syntaxCellObj, $syntaxSpec] = choice;
					(* TODO: Better close menu action. *)
					NotebookDelete[EvaluationCell[]];

					RedrawHighlightSyntaxCell[syntaxCellObj];
				)
			),
			syntaxes
		],
		themeChoices = Map[
			choice |-> (
				styleListItem[currentTheme, choice] :> (
					CurrentValue[syntaxCellObj, $themeSpec] = choice;
					(* TODO: Better close menu action. *)
					NotebookDelete[EvaluationCell[]];

					RedrawHighlightSyntaxCell[syntaxCellObj];
				)
			),
			themes
		]
	},
		{
			{"ActionMenu",
				Row[{"Syntax: ", currentSyntax}],
				syntaxChoices
			},
			{"ActionMenu",
				Row[{"Theme: ", currentTheme}],
				themeChoices
			}
		}
	];

	MakeMenu[menuItems, Automatic, 240]
]]

(*------------------------------------*)

SetFallthroughError[styleListItem]

styleListItem[currentSelection: _, choice: _] :=
	If[currentSelection === choice,
		Row[{
			"\[Checkmark]",
			"  ",
			choice
		}],
		(* This possible value is not whatever the currently selected value is. *)
		(* Display a hidden checkmark purely so that this
			is offset by the same amount as list items that
			display a visible checkmark. *)
		Row[{
			Style[
				"\[Checkmark]",
				ShowContents -> False
			],
			"  ",
			choice
		}]
	]

(*====================================*)

HandleHighlightSyntaxCellEvent[
	cellObj: _CellObject,
	"KeyDown"
] := Module[{
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

	RedrawHighlightSyntaxCell[cellObj]
]

(*====================================*)

SetFallthroughError[RedrawHighlightSyntaxCell]

RedrawHighlightSyntaxCell[cellObj: _CellObject] := Module[{
	originalCell,
	position,
	syntax, theme,
	plainTextContent,
	highlightedContent,
	newCell
},
	(*---------------------------------------------------*)
	(* Extract current cell content and cursor position. *)
	(*---------------------------------------------------*)

	position = Replace[Developer`CellInformation[cellObj], {
		KeyValuePattern[{"CursorPosition" -> {c: _, c: _}}] :> c,
		KeyValuePattern[{
			"CursorPosition" -> None | "AboveCell" | "BelowCell"
		}] :> None,
		other: _ :> Raise[NotebookWebsiteError, "Unexpected cell information: ``", InputForm@other]
	}];

	{syntax, theme} = getHighlightSyntaxCellSyntaxAndTheme[cellObj];

	originalCell = NotebookRead[cellObj];
	plainTextContent = Replace[originalCell, {
		Cell[content: _, ___] :> ConvertToString[content],
		other: _ :> Raise[NotebookWebsiteError, "Unexpected NotebookRead result: ``", InputForm@other]
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
		Cell[_, args: ___] :> (
			Cell[
				TextData[highlightedContent],
				Background -> background,
				args
			]
		),
		other: _ :> Raise[
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


	(* Re-position the input cursor/caret. *)
	If[position =!= None,
		NotebookWrite[cellObj, newCell, All, AutoScroll -> False];
		SelectionMove[EvaluationNotebook[], Before, CellContents, AutoScroll -> False];
		SelectionMove[EvaluationNotebook[], Next, Character, position, AutoScroll -> False];
		SelectionMove[EvaluationNotebook[], After, Character, AutoScroll -> True]
		,
		(* The cursor was not inside the cell, so write without changing the
			selection. *)
		NotebookWrite[cellObj, newCell, AutoScroll -> False];
	];
]

SetFallthroughError[HandleHighlightSyntaxCellEvent]

(*====================================*)

SetFallthroughError[HighlightSyntaxCellDefaultBackground]

HighlightSyntaxCellDefaultBackground[] := Module[{
	theme = $DefaultTheme, color
},
	RaiseAssert[StringQ[theme]];

	color = RaiseConfirm[GetLibraryFunction["theme_default_background"][theme]];

	RaiseAssert[MatchQ[color, _RGBColor | None], "bad color: ``", InputForm@color];

	color
]

(*====================================*)

KnownHighlightChoices[] := GetLibraryFunction["known_highlight_choices"][]

(*====================================*)

SetFallthroughError[getHighlightSyntaxCellSyntaxAndTheme]

getHighlightSyntaxCellSyntaxAndTheme[
	cellObj: _CellObject
] := Module[{},
	syntaxOptions = Replace[AbsoluteCurrentValue[cellObj, {TaggingRules, "HighlightSyntaxOptions"}], {
		Inherited -> <||>,
		opts: _?ListQ :> Association[opts],
		assoc: _?AssociationQ :> assoc,
		other: _ :> Raise[
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
	currentStyles = Replace[currentStyles, s: _?StringQ :> {s}];

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