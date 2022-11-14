BeginPackage["ConnorGray`NotebookWebsiteTools`LibraryLink`"]

GetLibraryFunction

$LibraryFunctions

Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`ErrorUtils`"]

(*====================================*)

$LibraryFunctions := Module[{
	loader,
	result
},
	(* TODO(refactor): Use paclet-based path instead of unqualified libname. *)
	loader = LibraryFunctionLoad["libnotebook_website_tools", "load_library_functions", LinkObject, LinkObject];

	If[FailureQ[loader],
		RaiseError["Error loading Wolfram LibraryLink loader function: ", loader];
	];

	result = loader[];

	Replace[result, {
		functions:Association[(_?StringQ -> _)...] :> (
			$LibraryFunctions = functions;
		),
		other_ :> (
			$LibraryFunctions := RaiseError["Library loader function returned unexpected result: ", other];
		)
	}];

	$LibraryFunctions
];

(*====================================*)

GetLibraryFunction[name: _?StringQ] := Lookup[
	$LibraryFunctions,
	name,
	RaiseError[
		"libnotebook_website_tools has no automatically exported function named ``",
		InputForm[name]
	]
]

AddUnmatchedArgumentsHandler[GetLibraryFunction]


End[]

EndPackage[]