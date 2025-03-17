BeginPackage["ConnorGray`NotebookWebsiteTools`Notebook`"]

(*----------------------------*)
(* Creating Website Notebooks *)
(*----------------------------*)

MakeNotebookTaggingRules
MakeNotebookStyleDefinitions
MakeNotebookDockedCells

UpdateNotebook::usage = "UpdateNotebook[obj] updates the website notebook specified by the notebook object obj."

$NotebookWebsiteToolsStylesheet

Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`"]
Needs["ConnorGray`NotebookWebsiteTools`Errors`"]
Needs["ConnorGray`NotebookWebsiteTools`Toolbar`"]
Needs["ConnorGray`NotebookWebsiteTools`UI`"]

Needs["ConnorGray`NotebookWebsiteTools`Notebook`BlogPost`"]

(*====================================*)

UpdateNotebook[nb: _NotebookObject] := Module[{
	metadata,
	documentType,
	createdByPacletVersion
},
	metadata = Replace[Options[nb, TaggingRules], {
		{TaggingRules -> KeyValuePattern["ConnorGray/NotebookWebsiteTools" -> metadata: _]} :> metadata,
		other: _ :> Raise[
			NotebookWebsiteError,
			"Notebook does not have expected \"ConnorGray/NotebookWebsiteTools\" TaggingRules value"
		]
	}];

	{documentType, createdByPacletVersion} = Replace[metadata, {
		KeyValuePattern[{
			"DocumentType" -> type: _?StringQ,
			"CreatedByPacletVersion" -> createdBy: _?StringQ
		}] :> {type, createdBy},
		other: _ :> Raise[
			NotebookWebsiteError,
			"Notebook metadata does not have expected fields: ``",
			InputForm[other]
		]
	}];

	(* NOTE: documentType and createdByPacletVersion should be used to customize
		the update code below if and when backwards-incompatible changes are
		made to the website notebook format. *)

	SetOptions[nb, {
		DockedCells -> MakeNotebookDockedCells[documentType],
		StyleDefinitions -> MakeNotebookStyleDefinitions[]
	}]
]

SetFallthroughError[UpdateNotebook]

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

SetFallthroughError[MakeNotebookTaggingRules]

(*====================================*)

$NotebookWebsiteToolsStylesheet = FrontEnd`FileName[
	{"ConnorGray"},
	"NotebookWebsiteTools.nb",
	CharacterEncoding -> "UTF-8"
]

MakeNotebookStyleDefinitions[] :=
	$NotebookWebsiteToolsStylesheet

SetFallthroughError[MakeNotebookStyleDefinitions]


(*------------------------------------*)

(*====================================*)

MakeNotebookDockedCells[documentType: _?StringQ] := Replace[documentType, {
	"BlogPost" :> MakeBlogPostDockedCells[],
	other: _ :> Raise[NotebookWebsiteError, "Unhandled document type in MakeNotebookDockedCells: ``", other]
}]

SetFallthroughError[MakeNotebookDockedCells]

(*====================================*)

End[]

EndPackage[]