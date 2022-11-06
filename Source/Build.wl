BeginPackage["ConnorGray`NotebookWebsiteTools`Build`"]

Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`"]
Needs["ConnorGray`NotebookWebsiteTools`Utils`"]
Needs["ConnorGray`NotebookWebsiteTools`ErrorUtils`"]

NotebookWebsiteBuild[inputDir0: _?StringQ | File[_?StringQ]] := CatchRaised @ Module[{
	inputDir = RaiseConfirm @ ExpandFileName[inputDir0],
	buildDir,
	contentDir,
	notebooks
},
	buildDir = FileNameJoin[{inputDir, "build"}];
	contentDir = FileNameJoin[{inputDir, "Content"}];

	RaiseAssert[StringQ[inputDir]];

	If[!DirectoryQ[contentDir],
		RaiseError["'Content' directory does not exist at expected location: ``",contentDir];
	];

	notebooks = FileNames["*.nb", contentDir, Infinity];

	(* TODO: Remove this check, clear the contents of the build directory if it
	         exists. (Note: clear the contents, don't delete the directory, as
			 a subtle UX choice )*)
	If[FileExistsQ[buildDir],
		If[DirectoryQ[buildDir],
			RaiseConfirm @ DeleteDirectory[buildDir, DeleteContents -> True];
			,
			RaiseError["File at build location is not a directory: ``", buildDir];
		];
	];

	RaiseConfirm @ CreateDirectory[buildDir];

	Scan[
		nbFile |-> Module[{},
			buildWebNotebook[nbFile, contentDir, buildDir]
		],
		notebooks
	];
]

(*======================================*)

buildWebNotebook[
	nbFile: _?StringQ,
	contentDir: _?StringQ,
	buildDir: _?StringQ
] := Module[{
	nbFileRelative = RelativePath[contentDir, nbFile],
	htmlFile,
	metadata
},
	RaiseAssert[StringQ[nbFileRelative]];

	nb = Replace[Get[File[nbFile]], {
		nb_Notebook :> nb,
		other_ :> RaiseError["Error importing notebook at ``: ``", nbFile, InputForm[other]]
	}];

	metadata = Replace[Options[nb, TaggingRules], {
		KeyValuePattern[TaggingRules -> KeyValuePattern["ConnorGray/NotebookWebsiteTools" -> value_]] :> value,
		other_ :> RaiseError[
			"Notebook at `` does not have the expected TaggingRules needed to process web page notebook: ``",
			nbFileRelative,
			InputForm[other]
		]
	}];

	(* TODO: Use documentType? *)
	documentType = Replace[metadata, {
		KeyValuePattern["DocumentType" -> type_?StringQ] :> type,
		other_ :> RaiseError[
			"Notebook at `` does not have the expected metadata \"DocumentType\" field: ``",
			nbFileRelative,
			InputForm[other]
		]
	}];

	(*--------------------------------*)
	(* Convert the cells to HTML      *)
	(*--------------------------------*)

	elements = Flatten @ Map[
		convertToHtml,
		(* TODO: Validate this. *)
		nb[[1]]
	];

	RaiseAssert[
		MatchQ[elements, {XMLElement[__]...}],
		"Unexpected elements structure: ``",
		InputForm[elements]
	];

	(*--------------------------------*)
	(* Write out the HTML output      *)
	(*--------------------------------*)

	html = XMLElement["html", {}, {
		XMLElement["head", {}, {
			XMLElement["style", {}, {
				StringReplace["
				body {
					margin: 0 auto;
					max-width: 50em;

					font-family: \"Helvetica\", \"Arial\", sans-serif;

					line-height: 1.5;
					padding: 4em 1em;

					color: #555;
				}

				h2 {
					margin-top: 1em;
					padding-top: 1em;
				}

				h1, h2, h3, strong {
					color: #333;
				}
				", StartOfLine ~~ "\t\t\t\t" -> ""]
			}]
		}],
		XMLElement["body", {}, elements]
	}];

	htmlString = ExportString[html, "HTMLFragment"];

	htmlFile = FileNameJoin[{
		buildDir,
		StringReplace[nbFileRelative, ".nb" ~~ EndOfString -> ".html"]
	}];

	RaiseConfirm @ WriteString[htmlFile, htmlString];



	(* Construct an absolute URL equivalent of `nbFileRelative`. *)
	(* urlPath = "/" <> StringRiffle[
		URLEncode /@ FileNameSplit[nbFileRelative],
		"/"
	]; *)
	(* RaiseAssert[StringQ[urlPath]] *)
	(* Print["    URL: ", InputForm @ urlPath]; *)

]

(*======================================*)

(*
	Title = h1
	Chapter = h2
	Section = h3
	Subsection = h4
	Subsubsection = h5
	Subsubsubsection = h6
	Subsubsubsubsection = ???
*)

convertToHtml[expr_] := Replace[expr, {
	(* TODO(cleanup): Is this "class" -> "cell-group" used for anything? Is this
		<div> wrapper used for anything? Why not just flatten these inline? *)
	(* Cell[CellGroupData[cells_?ListQ, Open]] :> XMLElement["div", {"class" -> "cell-group"}, Map[convertToHtml, cells]], *)
	Cell[CellGroupData[cells_?ListQ, Open]] :> Map[convertToHtml, cells],
	Cell[text_?StringQ, "Title", ___?OptionQ] :> XMLElement["h1", {}, {text}],
	Cell[text_?StringQ, "Subtitle", ___?OptionQ] :> XMLElement["p", {}, {text}],
	Cell[text_?StringQ, "Chapter", ___?OptionQ] :> XMLElement["h2", {}, {text}],
	Cell[text_?StringQ, "Section", ___?OptionQ] :> XMLElement["h3", {}, {text}],
	Cell[text_?StringQ, "Subsection", ___?OptionQ] :> XMLElement["h4", {}, {text}],
	Cell[text_?StringQ, "Subsubsection", ___?OptionQ] :> XMLElement["h5", {}, {text}],
	Cell[text_?StringQ, "Item", ___?OptionQ] :>
		XMLElement["ul", {}, {XMLElement["li", {}, {text}]}],
	Cell[text_?StringQ, "Subitem", ___?OptionQ] :>
		XMLElement["ul", {}, {"\t", XMLElement["li", {}, {text}]}],
	other_ :> RaiseError["unhandled: ``", InputForm[other]]

}]

AddUnmatchedArgumentsHandler[convertToHtml]

End[]

EndPackage[]