Needs["ConnorGray`NotebookWebsiteTools`"]
Needs["ConnorGray`NotebookWebsiteTools`ErrorUtils`"]

$testsDir = FileNameDrop[$TestFileName];

$examplesDir = FileNameJoin[{FileNameDrop[$testsDir], "Examples"}];

VerificationTest[DirectoryQ[$testsDir]];
VerificationTest[DirectoryQ[$examplesDir]];

$buildDir = CreateDirectory[]

VerificationTest[
	NotebookWebsiteBuild[$examplesDir, $buildDir]
	,
	Success["NotebookWebsiteBuild", <|
		"ProcessedNotebooks" -> {
			FileNameJoin @ {$examplesDir, "Content", "kitchen-sink.nb"}
		},
		"OutputHTMLFiles" -> {
			File @ FileNameJoin @ {$buildDir, "kitchen-sink.html"}
		}
	|>]
]

(*====================================*)

(* That that NotebookWebsiteBuild generates an error if trying to build to
	a non-cache directory. *)

VerificationTest[
	NotebookWebsiteBuild[$examplesDir, $examplesDir]
	,
	Failure[ "NotebookWebsiteError", <|
		"MessageTemplate" -> "Unable to create cache directory at ``",
		"MessageParameters" -> {InputForm[$examplesDir]},
		"CausedBy" -> Failure["NotebookWebsiteError", <|
			"MessageTemplate" -> "Specified path is an existing non-empty directory without a CACHEDIR.TAG file. This operation may succeed if the existing directory contents are moved or deleted manually.\n\nNOTE: This file path is being used as a location to store cached data or generated files. Data loss WILL occur if you store non-recoverable files in this location.",
			"MessageParameters" -> {}
		|>]
	|>]
	,
	{RaiseError::error}
]

VerificationTest[
	NotebookWebsiteBuild[$examplesDir, FileNameJoin[{$examplesDir, "Content"}]]
	,
	Failure[ "NotebookWebsiteError", <|
		"MessageTemplate" -> "Unable to create cache directory at ``",
		"MessageParameters" -> {InputForm[FileNameJoin[{$examplesDir, "Content"}]]},
		"CausedBy" -> Failure["NotebookWebsiteError", <|
			"MessageTemplate" -> "Specified path is an existing non-empty directory without a CACHEDIR.TAG file. This operation may succeed if the existing directory contents are moved or deleted manually.\n\nNOTE: This file path is being used as a location to store cached data or generated files. Data loss WILL occur if you store non-recoverable files in this location.",
			"MessageParameters" -> {}
		|>]
	|>]
	,
	{RaiseError::error}
]

(*====================================*)