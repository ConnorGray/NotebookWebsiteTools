Needs["ConnorGray`NotebookWebsiteTools`"]
Needs["ConnorGray`NotebookWebsiteTools`Build`"]

Needs["Wolfram`ErrorTools`"]

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

(* TID:240527/1: "CodeText" cell handling. *)
VerificationTest[
	ConvertToHtml @ Cell[
		"This is some text",
		"CodeText"
	],
	XMLElement[
		"div",
		{"class" -> "nb-CodeText"},
		{"This is some text"}
	]
]

VerificationTest[
	ConvertToHtml @ Notebook[{
		Cell @ CellGroupData[{
			Cell[TextData["Title"], "Section"],
			Cell[TextData["This is some content"], "Text"]
		}, Open]
	}],
	XMLElement[
		"article",
		{"class" -> "Notebook"},
		{
			XMLElement[
				"h3",
				{"class" -> "nb-Section"},
				{
					XMLElement[
						"a",
						{
							"id" -> "title",
							"class" -> "anchor",
							"href" -> "#title"
						},
						{"Title"}
					]
				}
			],
			XMLElement["p", {}, {"This is some content"}]
		}
	]
]

VerificationTest[
	(* TID:240602/1: Inline "Code" or "Program" StyleBox's *)
	ConvertToHtml @ Cell[
		TextData[{
			"Inline code ",
			StyleBox["code = 5", "Code"],
			" example. Inline program ",
			StyleBox["program = 10", "Program"],
			" example."
		}],
		"Text"
	],
	XMLElement["p", {}, {
		"Inline code ",
		XMLElement["code", {}, {
			"code = 5"
		}],
		" example. Inline program ",
		XMLElement["code", {}, {
			"program = 10"
		}],
		" example."
	}]
]

VerificationTest[
	(* TID:240602/2: Unrecognized style in textual cell StyleBox. *)
	Handle[_Failure] @ ConvertToHtml @ Cell[
		TextData[{
			"Inline code ",
			StyleBox["2 + 2", "NotAKnownStyle"],
			" example."
		}],
		"Text"
	],
	Failure[
		ConnorGray`NotebookWebsiteTools`Errors`NotebookWebsiteError,
		<|
			"CausedBy" -> Failure[
				ConnorGray`NotebookWebsiteTools`Errors`NotebookWebsiteError,
				<|
					"MessageTemplate" -> "Unhandled StyleBox style: ``",
					"MessageParameters" -> {InputForm["NotAKnownStyle"]}
				|>
			],
			"MessageTemplate" -> "Error converting `` style cell",
			"MessageParameters" -> {InputForm["Text"]}
		|>
	]
]

(*--------------------------------*)
(* Test conversion of Draft cells *)
(*--------------------------------*)

With[{
	example = Notebook[{
		Cell[
			TextData["Normal textual cell"],
			"Text"
		],
		Cell[
			BoxData[RowBox[{"normalBoxCell","=","10"}]],
			"Input"
		],

		(* TID:240601/1: Draft applied to textual (converted) cell *)
		Cell[
			TextData["Draft textual cell"],
			"Text",
			"ConnorGray/Draft"
		],
		(* TID:240601/2: Draft applied to box (rasterized) cell *)
		Cell[
			BoxData[RowBox[{"draftBoxCell","=","10"}]],
			"Input",
			"ConnorGray/Draft"
		],

		(* TID:240601/3: Excluded applied to textual (converted) cell *)
		Cell[
			TextData["Excluded textual cell"],
			"Text",
			"ConnorGray/Excluded"
		],
		(* TID:240601/4: Excluded applied to box (rasterized) cell *)
		Cell[
			BoxData[RowBox[{"excludedBoxCell","=","10"}]],
			"Input",
			"ConnorGray/Excluded"
		]
	}]
},
	(*-----------------------------*)
	(* With IncludeDrafts -> False *)
	(*-----------------------------*)

	Block[{
		$CurrentNotebookSupportFiles = <||>,
		$CurrentNotebookFile = "/tmp/FakeTestNotebook.nb"
	},
		VerificationTest[
			Block[{$BuildSettings = <| "IncludeDrafts" -> False |>},
				ConvertToHtml[example]
			],
			XMLElement[
				"article",
				{"class" -> "Notebook"},
				{
					XMLElement["p", {}, {"Normal textual cell"}],
					XMLElement["img", {
						"src" -> "FakeTestNotebook/0.png",
						"width" -> "133",
						"height" -> "17",
						"style" -> "display: block; padding: 4pt 0 4pt 0;"
					}, {}]
				}
			]
		];

		VerificationTest[
			$CurrentNotebookSupportFiles,
			<|
				(* FIXME: Encode the cell style in this so its more distinct
				 	for testing purposes. E.g. `/Input-0.png` or `0-Input.png`
				*)
				File["FakeTestNotebook/0.png"] -> _?ImageQ
			|>,
			SameTest -> MatchQ
		];
	];

	(*-----------------------------*)
	(* With IncludeDrafts -> True *)
	(*-----------------------------*)

	Block[{
		$CurrentNotebookSupportFiles = <||>,
		$CurrentNotebookFile = "/tmp/FakeTestNotebook.nb"
	},
		VerificationTest[
			Block[{$BuildSettings = <| "IncludeDrafts" -> True |>},
				ConvertToHtml[example]
			],
			XMLElement[
				"article",
				{"class" -> "Notebook"},
				{
					XMLElement["p", {}, {"Normal textual cell"}],
					XMLElement["img", {
						"src" -> "FakeTestNotebook/0.png",
						"width" -> "133",
						"height" -> "17",
						"style" -> "display: block; padding: 4pt 0 4pt 0;"
					}, {}],
					XMLElement[
						"div",
						{"class" -> "nb-Draft"},
						{XMLElement["p", {}, {"Draft textual cell"}]}
					],
					XMLElement["img", {
						"src" -> "FakeTestNotebook/1.png",
						"width" -> "126",
						"height" -> "17",
						"style" -> "display: block; padding: 4pt 0 4pt 0;"
					}, {}]
				}
			]
		];

		VerificationTest[
			$CurrentNotebookSupportFiles,
			<|
				File["FakeTestNotebook/0.png"] -> _?ImageQ,
				File["FakeTestNotebook/1.png"] -> _?ImageQ
			|>,
			SameTest -> MatchQ
		];
	];
]

VerificationTest[
	Block[{
		$CurrentNotebookRelativeURL = URL["content/test-file.html"]
	},
		(* TID:240602/3: Convert inline PacletLink special link *)
		ConvertToHtml @ Cell[
			BoxData @ TemplateBox[
				{"Foo", "https://example.com"},
				"ConnorGray/PacletLink"
			],
			FontWeight -> "Bold"
		]
	],
	XMLElement[
		"a",
		{"href" -> "https://example.com", "class" -> "IconLink"},
		{
			XMLElement[
				"img",
				{"src" -> "../web_assets/paclet-icon.svg"},
				{}
			],
			"Foo"
		}
	]
]