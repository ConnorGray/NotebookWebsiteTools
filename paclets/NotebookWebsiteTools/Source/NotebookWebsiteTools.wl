BeginPackage["ConnorGray`NotebookWebsiteTools`"]

PacletInstall /@ PacletObject["ConnorGray/NotebookWebsiteTools"]["Dependencies"]

CreateWebsiteNotebook::usage = "CreateWebsiteNotebook[type, title] creates a new web page source notebook of the specified type."

NotebookWebsiteBuild::usage = "NotebookWebsiteBuild[dir] builds the notebook website in dir."

(*----------------------------*)
(* Querying Website Notebooks *)
(*----------------------------*)

WebsiteNotebookTitle::usage = "WebsiteNotebookTitle[nb] returns the title of the specified website notebook.
	The title is defined as the textual content of the first cell with style \"Title\"."
WebsiteNotebookStatus::usage = "WebsiteNotebookStatus[nb] returns the value of the \"DocumentStatus\" metadata field for the specified notebook."
WebsiteNotebookSnippet::usage = "WebsiteNotebookSnippet[nb] returns a snippet of text that is intended to be a teaser or summary of the notebook content."


MakeHTML::usage = "MakeHTML[expr] is used to convert expressions in ConnorGray/ComputedHTML cells to HTML."

Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`Utils`"]
Needs["ConnorGray`NotebookWebsiteTools`Errors`"]

Needs["ConnorGray`NotebookWebsiteTools`Notebook`"]
Needs["ConnorGray`NotebookWebsiteTools`Notebook`BlogPost`"]
Needs["ConnorGray`NotebookWebsiteTools`Build`"]
Needs["ConnorGray`NotebookWebsiteTools`MakeHTML`"]

(*======================================*)

CreateWebsiteNotebook[type: _?StringQ, title: _?StringQ] := Handle[_Failure] @ Module[{nb},
	nb = Replace[type, {
		"BlogPost" :> CreateBlogPostNotebook[title],
		_ :> Raise[NotebookWebsiteError, "Unknown website notebook type: ``", type]
	}];

	RaiseAssert[MatchQ[nb, Notebook[{___}, ___?OptionQ]]];

	NotebookPut[nb]
]

SetFallthroughError[CreateWebsiteNotebook]

(*========================================================*)

WebsiteNotebookTitle[
	Notebook[cells: {___Cell}, ___?OptionQ]
] := Module[{
	cellData,
	title
},
	cellData = FirstCase[
		cells,
		Cell[data: _, ___, "Title", ___] :> data,
		Missing["NotFound"],
		Infinity
	];

	title = Replace[cellData, {
		Missing["NotFound"] :> Return[cellData, Module],
		_?StringQ | TextData[_] | BoxData[_] :> ConvertToString[cellData],
		other: _ :> Raise[
			NotebookWebsiteError,
			"Error getting website notebook title: unexpected \"Title\" cell data: ``",
			InputForm[other]
		]
	}];

	RaiseAssert[StringQ[title]];

	title
]

SetFallthroughError[WebsiteNotebookTitle]

(*========================================================*)

WebsiteNotebookStatus[
	Notebook[_?ListQ, options0: ___?OptionQ]
] := Replace[{options0}, {
	KeyValuePattern[{TaggingRules -> KeyValuePattern[{
		"ConnorGray/NotebookWebsiteTools" -> KeyValuePattern[{
			"DocumentStatus" -> status0: _
		}]
	}]}] :> Replace[status0, {
		_?StringQ :> status0,
		other: _ :> Raise[
			NotebookWebsiteError,
			"Invalid website notebook \"DocumentStatus\" value: ``. Expected string.",
			InputForm[other]
		]
	}],
	(* No document status was set. Callers of this function should treat
	   documents with this status as a "normal" document. *)
	_ :> Missing["KeyAbsent", "DocumentStatus"]
}]

SetFallthroughError[WebsiteNotebookStatus]

(*========================================================*)

SetFallthroughError[WebsiteNotebookSnippet]

WebsiteNotebookSnippet[
	nb:Notebook[{___Cell}, ___?OptionQ]
] := WrapRaised[
	NotebookWebsiteError,
	"Error generating website notebook snippet.",
	InputForm[configFile]
] @ Module[{
	cells = NotebookCells[nb],
	firstText
},
	cellData = FirstCase[
		cells,
		Cell[data: _, ___, "Text", ___] :> data
	];

	firstText = Replace[cellData, {
		Missing["NotFound"] :> Return[cellData, Module],
		_?StringQ | TextData[_] | BoxData[_] :> ConvertToString[cellData],
		other: _ :> Raise[
			NotebookWebsiteError,
			"Error getting website notebook snippet: unexpected \"Text\" cell data: ``",
			InputForm[other]
		]
	}];

	RaiseAssert[StringQ[firstText]];

	firstText
]

(*========================================================*)

End[] (* End `Private` *)

EndPackage[]
