BeginPackage["ConnorGray`NotebookWebsiteTools`"]

CreateWebsiteNotebook::usage = "CreateWebsiteNotebook[type, title] creates a new web page source notebook of the specified type."

NotebookWebsiteBuild::usage = "NotebookWebsiteBuild[dir] builds the notebook website in dir."


(* Defaults for syntax highlighting on HighlightSyntax cells that don't
	otherwise have explicit "Syntax" or "Theme" option values. *)
$DefaultSyntax = "Plain Text"
$DefaultTheme = "Solarized (light)"

Protect[{$DefaultSyntax, $DefaultTheme}]

(*----------------------------*)
(* Querying Website Notebooks *)
(*----------------------------*)

WebsiteNotebookTitle::usage = "WebsiteNotebookTitle[nb] returns the title of the specified website notebook.
	The title is defined as the textual content of the first cell with style \"Title\"."
WebsiteNotebookStatus::usage = "WebsiteNotebookStatus[nb] returns the value of the \"DocumentStatus\" metadata field for the specified notebook."
WebsiteNotebookSnippet::usage = "WebsiteNotebookSnippet[nb] returns a snippet of text that is intended to be a teaser or summary of the notebook content."


Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`Utils`"]
Needs["ConnorGray`NotebookWebsiteTools`ErrorUtils`"]

Needs["ConnorGray`NotebookWebsiteTools`Notebook`"]
Needs["ConnorGray`NotebookWebsiteTools`Notebook`BlogPost`"]
Needs["ConnorGray`NotebookWebsiteTools`Build`"]
Needs["ConnorGray`NotebookWebsiteTools`LibraryLink`"]

(*======================================*)

CreateWebsiteNotebook[type: _?StringQ, title: _?StringQ] := CatchRaised @ Module[{nb},
	nb = Replace[type, {
		"BlogPost" :> CreateBlogPostNotebook[title],
		_ :> RaiseError["Unknown website notebook type: ``", type]
	}];

	RaiseAssert[MatchQ[nb, Notebook[{___}, ___?OptionQ]]];

	NotebookPut[nb]
]

AddUnmatchedArgumentsHandler[CreateWebsiteNotebook]

(*========================================================*)

WebsiteNotebookTitle[
	Notebook[cells:{___Cell}, ___?OptionQ]
] := Module[{
	cellData,
	title
},
	cellData = FirstCase[
		cells,
		Cell[data_, ___, "Title", ___] :> data,
		Missing["NotFound"],
		Infinity
	];

	title = Replace[cellData, {
		Missing["NotFound"] :> Return[cellData, Module],
		_?StringQ | TextData[_] | BoxData[_] :> ConvertToString[cellData],
		other_ :> RaiseError[
			"Error getting website notebook title: unexpected \"Title\" cell data: ``",
			InputForm[other]
		]
	}];

	RaiseAssert[StringQ[title]];

	title
]

AddUnmatchedArgumentsHandler[WebsiteNotebookTitle]

(*========================================================*)

WebsiteNotebookStatus[
	Notebook[_?ListQ, options0___?OptionQ]
] := Replace[{options0}, {
	KeyValuePattern[{TaggingRules -> KeyValuePattern[{
		"ConnorGray/NotebookWebsiteTools" -> KeyValuePattern[{
			"DocumentStatus" -> status0_
		}]
	}]}] :> Replace[status0, {
		_?StringQ :> status0,
		other_ :> RaiseError[
			"Invalid website notebook \"DocumentStatus\" value: ``. Expected string.",
			InputForm[other]
		]
	}],
	(* No document status was set. Callers of this function should treat
	   documents with this status as a "normal" document. *)
	_ :> Missing["KeyAbsent", "DocumentStatus"]
}]

AddUnmatchedArgumentsHandler[WebsiteNotebookStatus]

(*========================================================*)

WebsiteNotebookSnippet[
	nb:Notebook[{___Cell}, ___?OptionQ]
] := Module[{
	cells = NotebookCells[nb],
	firstText
},
	cellData = FirstCase[
		cells,
		Cell[data_, ___, "Text", ___] :> data
	];

	firstText = Replace[cellData, {
		Missing["NotFound"] :> Return[cellData, Module],
		_?StringQ | TextData[_] | BoxData[_] :> ConvertToString[cellData],
		other_ :> RaiseError[
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
