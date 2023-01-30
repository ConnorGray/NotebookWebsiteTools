BeginPackage["ConnorGray`NotebookWebsiteTools`CLI`"]

HandleNotebookWebsiteSubcommand::usage = "Handle the command `$ wolfram notebook-website build`."

Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`"]
Needs["ConnorGray`NotebookWebsiteTools`ErrorUtils`"]

Needs["ConnorGray`WolframCLI`" -> "CLI`"]

HandleNotebookWebsiteSubcommand[
	cliArgs: {___?StringQ}
] := CatchRaised @ Module[{
	inputDir
},
	(* TODO: Provide a helper function in WolframCLI` for this? *)
	Replace[cliArgs, {
		{_, "notebook-website", "build"} :> (
			inputDir = Directory[]
		),
		{_, "notebook-website", "build", inputDir0_} :> (
			inputDir = inputDir0
		),
		other_ :> RaiseError["Unexpected command line arguments: ``", other]
	}];

	RaiseAssert[StringQ[inputDir]];

	result = NotebookWebsiteBuild[inputDir];

	Replace[result, {
		_Success :> Null,
		failure_Failure :> (
			Print[Format[failure, CLI`TerminalForm]];
		),
		other_ :> (
			RaiseError["Unexpected NotebookWebsiteBuild result: ``", InputForm[other]]
		)
	}]
]


End[]

EndPackage[]