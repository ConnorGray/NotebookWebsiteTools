Needs["ConnorGray`NotebookWebsiteTools`"]

$testsDir = FileNameDrop[$TestFileName];

$examplesDir = FileNameJoin[{FileNameDrop[$testsDir], "Examples"}];

VerificationTest[DirectoryQ[$testsDir]];
VerificationTest[DirectoryQ[$examplesDir]];

$buildDir = CreateDirectory[]

VerificationTest[
	NotebookWebsiteBuild[
		$examplesDir
		(* PRECOMMIT: $buildDir *)
	]
	,
	Success["NotebookWebsiteBuild", <|
		"ProcessedNotebooks" -> {
			FileNameJoin @ {$examplesDir, "Content", "kitchen-sink.nb"}
		},
		"OutputHTMLFiles" -> {
			File @ FileNameJoin @ {$examplesDir, "build", "kitchen-sink.html"}
		}
	|>]
]