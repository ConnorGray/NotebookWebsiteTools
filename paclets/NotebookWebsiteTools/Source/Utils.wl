BeginPackage["ConnorGray`NotebookWebsiteTools`Utils`"]

RelativePath

ConvertToString

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
	other_ :> RaiseError["no rule to convert form to string: ``", InputForm[other]]
}]

AddUnmatchedArgumentsHandler[ConvertToString]

(*======================================*)


End[]

EndPackage[]