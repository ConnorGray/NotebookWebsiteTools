BeginPackage["ConnorGray`NotebookWebsiteTools`"]

CreateWebsiteNotebook::usage = "CreateWebsiteNotebook[type, title] creates a new web page source notebook of the specified type."

NotebookWebsiteBuild::usage = "NotebookWebsiteBuild[dir] builds the notebook website in dir."

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


End[] (* End `Private` *)

EndPackage[]
