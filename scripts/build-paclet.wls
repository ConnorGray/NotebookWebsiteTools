#!/usr/bin/env wolframscript

Needs["PacletTools`"]
Needs["CodeFormatter`"]

Print["Using $InstallationDirectory: ", $InstallationDirectory];

(*----------------------------------------------------------*)
(* Verify that Directory[] is the repository root directory *)
(*----------------------------------------------------------*)

$repositoryDir = Directory[];

If[FileNameTake[$repositoryDir] =!= "NotebookWebsiteTools",
	Throw[StringForm["Unexpected repository root directory path: ``", $repositoryDir]]
];

(*-------------------------------------------------------------------------------*)
(* Copy the compiled dynamic library into the paclet LibraryResources directory. *)
(*-------------------------------------------------------------------------------*)

(* FIXME: Make this switch between release and debug builds somehow. *)
Module[{
	source = FileNameJoin[{$repositoryDir, "target", "debug", "libnotebook_website_tools.dylib"}],
	dest   = FileNameJoin[{$repositoryDir, "paclets", "NotebookWebsiteTools", "LibraryResources", $SystemID, "libnotebook_website_tools.dylib"}]
},
	If[FileExistsQ[dest],
		DeleteFile[dest];
	];

	CopyFile[source, dest]
]


(*---------------------------------------------*)
(* Build the paclet into $repositoryDir/build/ *)
(*---------------------------------------------*)

result = PacletBuild[
	FileNameJoin[{$repositoryDir, "paclets", "NotebookWebsiteTools"}],
	FileNameJoin[{$repositoryDir, "build"}]
];

If[MatchQ[result, _Success],
	Print["RESULT: Success"]
	,
	Print["RESULT: ", CodeFormat @ ToString[result, InputForm]];
]

If[FailureQ[result],
	Exit[-1];
];

