BeginPackage["ConnorGray`NotebookWebsiteTools`CLI`"]

HandleNotebookWebsiteSubcommand::usage = "Handle the command `$ wolfram notebook-website build`."

Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`"]
Needs["ConnorGray`NotebookWebsiteTools`ErrorUtils`"]

Needs["ConnorGray`WolframCLI`" -> "CLI`"]

HandleNotebookWebsiteSubcommand[
	cliArgs: {___?StringQ}
] := CatchRaised @ Module[{
	inputDir,
	openFlag
},
	(* TODO: Provide a helper function in WolframCLI` for this? *)
	Replace[cliArgs, {
		{_, "notebook-website", "build",
			inputDir0 : _?ArgQ : Automatic,
			OrderlessPatternSequence[
				openFlag0 : "--open" : False
			]
		} :> (
			openFlag = StringQ[openFlag0];
			inputDir = Replace[inputDir0, Automatic :> Directory[]]
		),
		other_ :> RaiseError["Unexpected command line arguments: ``", other]
	}];

	RaiseAssert[StringQ[inputDir]];

	result = NotebookWebsiteBuild[inputDir];

	Replace[result, {
		success_Success :> (
			If[TrueQ[openFlag],
				(* Assume that the first file in "OutputHTMLFiles" is the most
				   interesting to open. *)
				Replace[success, {
					Success["NotebookWebsiteBuild", KeyValuePattern[{
						"OutputHTMLFiles" -> {first_, ___}
					}]] :> UsingFrontEnd @ SystemOpen[first]
				}]
			];
		),
		failure_Failure :> (
			Print[Format[failure, CLI`TerminalForm]];
		),
		other_ :> (
			RaiseError["Unexpected NotebookWebsiteBuild result: ``", InputForm[other]]
		)
	}]
]

(*======================================*)

(* TODO: Replace this with better declarative argument parsing. *)
ArgQ[expr_] := StringQ[expr] && !StringStartsQ[expr, "-"]


End[]

EndPackage[]