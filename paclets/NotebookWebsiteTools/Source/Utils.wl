BeginPackage["ConnorGray`NotebookWebsiteTools`Utils`"]

RelativePath

ConvertToString

UniqueContext::usage = "UniqueContext[stem] generates a unique context name beginning with stem."

NotebookCells::usage = "NotebookCells[notebook] returns a list of all top-level cells in notebook, after flattening out cell groups."

Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`ErrorUtils`"]

(*========================================================*)

(*
	Return the portion of `path` which is relative to `root`. If `path` is not relative to
	`root`, a failure is returned.

	TODO: What should this return if `root` and `path` are the same?
*)
RelativePath[
	root_?StringQ,
	path_?StringQ
] := Module[{
	rootParts = FileNameSplit[root],
	pathParts = FileNameSplit[path]
},
	(* Ensure that `path` is a subdirectory of `root`. If it is, return the component
	of the path which is relative to `root`. *)
	If[! MatchQ[pathParts, {Sequence @@ rootParts, ___}],
		$Failed,
		FileNameJoin[pathParts[[Length[rootParts] + 1 ;;]]]
	]
]

(*========================================================*)

ConvertToString[expr_] := Replace[expr, {
	string_?StringQ /; StringMatchQ[string, "\"" ~~ ___ ~~ "\""] :> ToExpression[string],
	string_?StringQ :> string,
	items_?ListQ :> StringJoin[ConvertToString /@ items],
	TextData[content_] :> ConvertToString[content],
	StyleBox[content_, ___] :> ConvertToString[content],
	BoxData[content_] :> ConvertToString[content],
	RowBox[items_?ListQ] :> StringJoin[ConvertToString /@ items],
	TemplateBox[items_?ListQ, "RowDefault"] :> StringJoin[ConvertToString /@ items],
	ButtonBox[content_, ___] :> ConvertToString[content],
	other_ :> RaiseError["no rule to convert form to string: ``", InputForm[other]]
}]

AddUnmatchedArgumentsHandler[ConvertToString]

(*========================================================*)

UniqueContext[stem_?StringQ] := Module[{ctx},
	If[!TrueQ[Internal`SymbolNameQ[stem]],
		Return[Failure["UniqueContext", <|
			"MessageTemplate" -> "Invalid non-Symbol stem: ``"|>,
			"MessageParameters" -> {stem}
		]];
	];

	ctx = stem <> "$" <> ToString[$ModuleNumber] <> "`";
	$ModuleNumber += 1;

	ctx
]

(*========================================================*)

NotebookCells[
	Notebook[cells:{___Cell}, ___?OptionQ]
] := flattenCellGroups[cells]

(*------------------------------------*)

flattenCellGroups[cells: {___Cell}] :=
	Flatten @ Map[
		Replace[{
			Cell[
				CellGroupData[groupCells:{___Cell}, ___],
				___
			] :> (
				flattenCellGroups[groupCells]
			),
			Cell[group_CellGroupData, ___] :> (
				RaiseError["Unexpected CellGroupData structure: ``", group]
			),
			normalCell_Cell :> normalCell,
			other_ :> RaiseError["Unexpected notebook structure: ``", InputForm[other]]
		}],
		cells
	]

AddUnmatchedArgumentsHandler[NotebookCells]


End[]

EndPackage[]