BeginPackage["ConnorGray`NotebookWebsiteTools`Notebook`"]

MakeNotebookTaggingRules

Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`ErrorUtils`"]

(*====================================*)

MakeNotebookTaggingRules[documentType: _?StringQ] := Module[{
	pacletVersion = PacletObject["ConnorGray/NotebookWebsiteTools"]["Version"]
},
	RaiseAssert[StringQ[pacletVersion] && StringMatchQ[
		pacletVersion,
		DigitCharacter.. ~~ "." ~~ DigitCharacter.. ~~ "." ~~ DigitCharacter..
	]];

	{
		"ConnorGray/NotebookWebsiteTools" -> {
			"DocumentType" -> documentType,
			(* Embed the paclet version of NotebookWebsiteTools that originally
			   created this notebook. This is potentially useful for improved
			   error reporting or automated fixes if backwards-incompatible
			   changes to NotebookWebsiteTools are ever necessary. *)
			"CreatedByPacletVersion" -> pacletVersion
		}
	}
]

AddUnmatchedArgumentsHandler[MakeNotebookTaggingRules]

(*====================================*)

End[]

EndPackage[]