Needs["ConnorGray`NotebookWebsiteTools`"]
Needs["ConnorGray`NotebookWebsiteTools`Build`"]

Needs["Wolfram`ErrorTools`"]

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