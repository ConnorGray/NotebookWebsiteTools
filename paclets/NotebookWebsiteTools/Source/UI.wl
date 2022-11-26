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

(*------------------------------------*)
(* Used in Cell style implementations *)
(*------------------------------------*)

HandleHighlightSyntaxCellEvent::usage = "HandleHighlightSyntaxCellEvent[cellObj, event]"
HighlightSyntaxCellDefaultBackground

Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`ErrorUtils`"]
Needs["ConnorGray`NotebookWebsiteTools`LibraryLink`"]
Needs["ConnorGray`NotebookWebsiteTools`Utils`"]

(*====================================*)

ToggleExcluded[nb_NotebookObject] := Module[{
	cells = SelectedCells[nb]
},
	RaiseAssert[MatchQ[cells, {___CellObject}]];

	Scan[cell |-> toggleCellStyle[cell, "Excluded"], cells];

	(* Return the cells that we modified. *)
	cells
]

AddUnmatchedArgumentsHandler[ToggleExcluded]

(*========================================================*)
(* Syntax Highlighting                                    *)
(*========================================================*)

HandleHighlightSyntaxCellEvent[cell_CellObject, "KeyDown"] := Module[{
	cellObj,
	originalCell,
	position,
	syntax, theme,
	plainTextContent,
	highlightedContent,
	newCell
},
	cellObj = EvaluationCell[];

	(*---------------------------------------------------*)
	(* Extract current cell content and cursor position. *)
	(*---------------------------------------------------*)

	position = Replace[Developer`CellInformation[cellObj], {
		KeyValuePattern[{"CursorPosition" -> {c_, c_}}] :> c,
		other_ :> RaiseError["Unexpected cell information: ``", InputForm@other]
	}];

	{syntax, theme} = getHighlightSyntaxCellSyntaxAndTheme[cellObj];

	originalCell = NotebookRead[cellObj];
	plainTextContent = Replace[originalCell, {
		Cell[content_, ___] :> ConvertToString[content],
		other_ :> RaiseError["Unexpected NotebookRead result: ``", InputForm@other]
	}];

	RaiseAssert[StringQ[plainTextContent]];

	(*----------------------------------------------*)
	(* Compute the updated syntax highlighting data *)
	(*----------------------------------------------*)

	(* FIXME: Do ToBoxes on this; fix ConvertToString parsing. *)
	highlightedContent = ReplaceAll[
		GetLibraryFunction["highlight_to_wolfram"][plainTextContent, syntax, theme],
		Style -> StyleBox
	];

	If[FailureQ[highlightedContent],
		(* TODO(polish): Present syntax highlighting errors (e.g. unknown syntax)
			in a better way. *)
		Print["ERROR: ", highlightedContent];
		Return[Null, Module];
	];

	newCell = Replace[originalCell, {
		Cell[_, args___] :> (
			Cell[
				TextData[highlightedContent],
				args
			]
		),
		other_ :> RaiseError[
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

AddUnmatchedArgumentsHandler[HandleHighlightSyntaxCellEvent]

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

AddUnmatchedArgumentsHandler[HighlightSyntaxCellDefaultBackground]

(*====================================*)

getHighlightSyntaxCellSyntaxAndTheme[cellObj_CellObject] := Module[{},
	syntaxOptions = Replace[AbsoluteCurrentValue[cellObj, {TaggingRules, "HighlightSyntaxOptions"}], {
		Inherited -> <||>,
		opts_?ListQ :> Association[opts],
		assoc_?AssociationQ :> assoc,
		other_ :> RaiseError[
			"HighlightSyntax cell has invalid non-Association value for \"HighlightSyntaxOptions\": ``",
			InputForm[other]
		]
	}];

	{
		Lookup[syntaxOptions, "Syntax", "Plain Text"],
		Lookup[syntaxOptions, "Theme", "Solarized (light)"]
	}
]

(*========================================================*)
(* Utilities                                              *)
(*========================================================*)

toggleCellStyle[cell : _CellObject, style : _?StringQ] := Module[{
	cellExpr = NotebookRead[cell]
},
	NotebookWrite[cell, Replace[cellExpr, {
		Cell[content_, a___String, style, b___String, opts___?OptionQ] :> (
			Cell[content, Sequence @@ DeleteCases[{a, b}, style]]
		),
		Cell[content_, styles___String, opts___?OptionQ] :> (
			(* Note:
				Place the newly added style at the end, to ensure it always
				gets the last say on what the styling of the cell is. E.g. if
				the style being added is "Excluded", then the cell will always
				have a red background, even if it is also e.g. a "Program" cell
				(which normally have gray backgrounds).
			*)
			Cell[content, styles, style, opts]
		),
		other_ :> RaiseError[
			"Unable to toggle cell style in malformed cell: ``: expr: ``",
			cell,
			cellExpr
		]
	}]]
]

AddUnmatchedArgumentsHandler[toggleCellStyle]

(*====================================*)


End[]

EndPackage[]