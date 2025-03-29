Needs["Wolfram`ErrorTools`V0`"]

Needs["ConnorGray`Utilities`"]

Needs["ConnorGray`NotebookWebsiteTools`"]
Needs["ConnorGray`NotebookWebsiteTools`Build`"]


VerificationTest[
	Handle[_Failure] @ AddSupportFile[
		Automatic,
		Rasterize["2 + 2"]
	],
	Failure[
		ConnorGray`NotebookWebsiteTools`Errors`NotebookWebsiteError,
		<|
			"CausedBy" -> Failure[
				ConnorGray`NotebookWebsiteTools`Errors`NotebookWebsiteError,
				<|
					"MessageTemplate" -> "Unexpected use of $CurrentNotebookSupportFiles: no notebook is currently being built.",
					"MessageParameters" -> {}
				|>
			],
			"MessageTemplate" -> "Error adding support file named: ``",
			"MessageParameters" -> {InputForm[Automatic]}
		|>
	]
]

VerificationTest[
	makeAnchorContentSlug["This is a heading"],
	"this-is-a-heading"
]

(* Punctuation should be dropped. *)
VerificationTest[
	makeAnchorContentSlug["John Smith's Website."],
	"john-smiths-website"
]

(* Special case: Anchors starting with dates should anchor to just the date. *)
VerificationTest[
	makeAnchorContentSlug["2024-01-01 — This is a heading"],
	"2024-01-01"
]

(* TID:250329/1: Create parent directory of copied file. *)
Module[{
	dir = FileSystemBundleExport[FileSystemBundle @ <|
		"Content/foo/data1.txt" -> "1,2,3",
		(* Test that a second file in the same dir doesn't yield a
			CreateDirectory "directory already exists" error. *)
		"Content/foo/data2.txt" -> "a,b,c"
	|>],
	result
},
	VerificationTest[
		NotebookWebsiteBuild[dir],
		Success["NotebookWebsiteBuild", <|
			"ProcessedNotebooks" -> {},
			"OutputHTMLFiles" -> {}
		|>]
	];

	VerificationTest[
		Map[
			path |-> RelativePath[FileNameJoin[{dir, "build"}], path],
			FileNames[All, FileNameJoin[{dir, "build"}], Infinity]
		],
		{
			"CACHEDIR.TAG",
			"foo",
			"foo/data1.txt",
			"foo/data2.txt",
			Repeated @ PatternTest[_?StringQ, f |-> StringStartsQ[f, "web_assets"]]
		},
		SameTest -> MatchQ
	];
];
