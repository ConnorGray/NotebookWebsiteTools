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

(* TID:240526/1: FontColor -> GrayLevel[..] handling. *)
VerificationTest[
	ConvertToHtml @ StyleBox[
		"Hello",
		FontColor -> GrayLevel[0.5]
	],
	XMLElement[
		"span",
		{"style" -> "color: rgb(50.0%, 50.0%, 50.0%)"},
		{"Hello"}
	]
]