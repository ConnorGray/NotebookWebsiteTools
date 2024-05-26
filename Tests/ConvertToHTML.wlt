Needs["ConnorGray`NotebookWebsiteTools`"]
Needs["ConnorGray`NotebookWebsiteTools`Build`"]

VerificationTest[
	ConvertToHtml @ StyleBox[
		"Hello",
		FontSize -> 12,
		FontColor -> Red
	],
	XMLElement[
		"span",
		{"style" -> "color: rgb(100%, 0%, 0%)"},
		{XMLElement[
			"span",
			{"style" -> "font-size: 12pt"},
			{"Hello"}
		]}
	]
]