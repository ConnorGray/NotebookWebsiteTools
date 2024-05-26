Needs["ConnorGray`NotebookWebsiteTools`Utils`"]
Needs["ConnorGray`NotebookWebsiteTools`Errors`"]

$workDir = CreateDirectory[];
$buildDir = FileNameJoin[{$workDir, "build"}]

$tagFileContents = ConnorGray`NotebookWebsiteTools`Utils`Private`$tagFileContents;

VerificationTest[StringQ[$tagFileContents]]

(*-----------------------------------------------------------*)
(* Create an initial cache directory and verify its contents *)
(*-----------------------------------------------------------*)

VerificationTest[
	CreateCacheDirectory[$buildDir],
	$buildDir
]

VerificationTest[
	FileNames[All, $buildDir, Infinity]
	,
	FileNameJoin[{$buildDir, #}]& /@ {
		"CACHEDIR.TAG"
	}
]

VerificationTest[
	ReadString[FileNameJoin[{$buildDir, "CACHEDIR.TAG"}]]
	,
	$tagFileContents
]

(*----------------------------------------------------------*)
(* Test CreateCacheDirectory on an existing cache directory *)
(*----------------------------------------------------------*)

VerificationTest[
	CreateCacheDirectory[$buildDir]
	,
	$buildDir
]

VerificationTest[
	FileNames[All, $buildDir, Infinity]
	,
	FileNameJoin[{$buildDir, #}]& /@ {
		"CACHEDIR.TAG"
	}
]

VerificationTest[
	ReadString[FileNameJoin[{$buildDir, "CACHEDIR.TAG"}]]
	,
	$tagFileContents
]

(*-------------------------------------------------*)
(* Test CreateCacheDirectory on an empty directory *)
(*-------------------------------------------------*)

(* Delete the existing CACHEDIR.TAG file *)
DeleteFile[FileNameJoin[{$buildDir, "CACHEDIR.TAG"}]]

(* Verify that the cache directory is now empty. *)
VerificationTest[
	FileNames[All, $buildDir, Infinity]
	,
	{}
]

VerificationTest[
	CreateCacheDirectory[$buildDir]
	,
	$buildDir
]

VerificationTest[
	FileNames[All, $buildDir, Infinity]
	,
	FileNameJoin[{$buildDir, #}]& /@ {
		"CACHEDIR.TAG"
	}
]

VerificationTest[
	ReadString[FileNameJoin[{$buildDir, "CACHEDIR.TAG"}]]
	,
	$tagFileContents
]


(*-------------------------------------------------------*)
(* Test CreateCacheDirectory with DeleteContents -> True *)
(*-------------------------------------------------------*)

CreateFile[FileNameJoin[{$buildDir, "SomeCachedData.txt"}]]

VerificationTest[
	FileNames[All, $buildDir, Infinity]
	,
	FileNameJoin[{$buildDir, #}]& /@ {
		"CACHEDIR.TAG",
		"SomeCachedData.txt"
	}
]

VerificationTest[
	CreateCacheDirectory[$buildDir, DeleteContents -> True]
	,
	$buildDir
]

VerificationTest[
	FileNames[All, $buildDir, Infinity]
	,
	FileNameJoin[{$buildDir, #}]& /@ {
		"CACHEDIR.TAG"
	}
]

(*-------------------------------------------------------------------------*)
(* Test CreateCacheDirectory on a non-empty directory without CACHEDIR.TAG *)
(*-------------------------------------------------------------------------*)

DeleteFile[FileNameJoin[{$buildDir, "CACHEDIR.TAG"}]]

CreateFile[FileNameJoin[{$buildDir, "ImportantData.txt"}]]

VerificationTest[
	CreateCacheDirectory[$buildDir]
	,
	Failure[NotebookWebsiteError, <|
		"CausedBy" -> Failure[
			NotebookWebsiteError, <|
				"MessageTemplate" -> "Specified path is an existing non-empty directory without a CACHEDIR.TAG file. This operation may succeed if the existing directory contents are moved or deleted manually.\n\nNOTE: This file path is being used as a location to store cached data or generated files. Data loss WILL occur if you store non-recoverable files in this location.",
				"MessageParameters" -> {}
			|>
		],
		"MessageTemplate" -> "Unable to create cache directory at ``",
		"MessageParameters" -> {InputForm[$buildDir]}
	|>]
]

(*---------------------------------------------------------------*)
(* Test CreateCacheDirectory directory with invalid CACHEDIR.TAG *)
(*---------------------------------------------------------------*)

DeleteFile[FileNameJoin[{$buildDir, "ImportantData.txt"}]]

Put["Invalid", FileNameJoin[{$buildDir, "CACHEDIR.TAG"}]]

VerificationTest[
	CreateCacheDirectory[$buildDir]
	,
	Failure[NotebookWebsiteError, <|
		"CausedBy" -> Failure[NotebookWebsiteError, <|
			"MessageTemplate" -> "Existing CACHEDIR.TAG file contains invalid header.",
			"MessageParameters" -> {}
		|>],
		"MessageTemplate" -> "Unable to create cache directory at ``",
		"MessageParameters" -> {InputForm[$buildDir]}
	|>]
]
