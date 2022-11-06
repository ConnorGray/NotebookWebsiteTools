BeginPackage["ConnorGray`NotebookWebsiteTools`Utils`"]

RelativePath

Begin["`Private`"]

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

End[]

EndPackage[]