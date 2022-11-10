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

Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`ErrorUtils`"]

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