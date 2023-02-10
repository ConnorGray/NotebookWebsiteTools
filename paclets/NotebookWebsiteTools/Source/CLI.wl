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
			inputDir = Replace[inputDir0, Automatic :> Directory[]];

			handleBuild[inputDir, openFlag]
		),
		{_, "notebook-website", "new",
			fileName : _?ArgQ : Automatic,
			OrderlessPatternSequence[
				openFlag0 : "--open" : False
			]
		} :> (
			openFlag = StringQ[openFlag0];

			handleNew[fileName, openFlag]
		),
		other_ :> RaiseError["Unexpected command line arguments: ``", other]
	}]
]

AddUnmatchedArgumentsHandler[HandleNotebookWebsiteSubcommand]

(*======================================*)

handleBuild[
	inputDir: _?StringQ,
	openFlag: _?BooleanQ
] := Module[{
	result
},
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

AddUnmatchedArgumentsHandler[handleBuild]

(*======================================*)

handleNew[
	fileName0: _?StringQ,
	openFlag: _?BooleanQ
] := Module[{
	fileName = RaiseConfirm @ ExpandFileName[fileName0],
	nb
},
	Replace[FileExtension[fileName], {
		".nb" :> {},
		"" :> (
			fileName = fileName <> ".nb";
		),
		other_?StringQ :> RaiseError[
			"unsupported file extension specified for new website notebook file path: ``",
			fileName
		]
	}];

	UsingFrontEnd[
		nb = CreateWebsiteNotebook["BlogPost", "<Unnamed>"];

		RaiseAssert[MatchQ[nb, _NotebookObject], "unexpected CreateWebsiteNotebook result: ``", nb];

		If[FileExistsQ[fileName],
			RaiseError["file already exists at path: ``", fileName];
		];

		RaiseConfirm @ NotebookSave[nb, fileName];

		If[openFlag,
			SystemOpen[fileName];
		];
	];
]

AddUnmatchedArgumentsHandler[handleNew]

(*======================================*)

(* TODO: Replace this with better declarative argument parsing. *)
ArgQ[expr_] := StringQ[expr] && !StringStartsQ[expr, "-"]


End[]

EndPackage[]