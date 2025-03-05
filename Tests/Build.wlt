Needs["ConnorGray`NotebookWebsiteTools`"]
Needs["ConnorGray`NotebookWebsiteTools`Build`"]

Needs["Wolfram`ErrorTools`V0`"]

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
