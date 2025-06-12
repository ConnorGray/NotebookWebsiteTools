Needs["ConnorGray`NotebookWebsiteTools`"]
Needs["ConnorGray`NotebookWebsiteTools`Build`"]
Needs["ConnorGray`NotebookWebsiteTools`Errors`"]

Needs["Wolfram`ErrorTools`V0`"]

(*============================================*)
(* Test text data cells and styles conversion *)
(*============================================*)

VerificationTest[
	ConvertToHTML @ StyleBox[
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
	ConvertToHTML @ StyleBox[
		"Hello",
		FontColor -> GrayLevel[0.5]
	],
	XMLElement[
		"span",
		{"style" -> "color: rgb(50.0%, 50.0%, 50.0%)"},
		{"Hello"}
	]
]

(* TID:250612/1: FontVariations -> {"Underline" -> True} handling. *)
VerificationTest[
	ConvertToHTML @ StyleBox[
		"Click & Drag",
		FontVariations -> {"Underline" -> True}
	],
	XMLElement["u", {}, {"Click & Drag"}]
]

(* TID:240527/1: "CodeText" cell handling. *)
VerificationTest[
	ConvertToHTML @ Cell[
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
	ConvertToHTML @ Notebook[{
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
	ConvertToHTML @ Cell[
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
	Handle[_Failure] @ ConvertToHTML @ Cell[
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
			"MessageTemplate" -> "Error converting `` style cell: ``",
			"MessageParameters" -> {
				InputForm["Text"],
				Cell[TextData[{
					"Inline code ",
					StyleBox["2 + 2", "NotAKnownStyle"],
					" example."
				}], "Text"]
			}
		|>
	]
]

(*====================================*)
(* Test conversion of Draft cells     *)
(*====================================*)

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
	(* With BuildType -> Published *)
	(*-----------------------------*)

	Block[{
		$CurrentNotebookSupportFiles = <||>,
		$CurrentNotebookFile = "/tmp/FakeTestNotebook.nb"
	},
		VerificationTest[
			Block[{$BuildSettings = <| "BuildType" -> "Published" |>},
				ConvertToHTML[example]
			],
			XMLElement[
				"article",
				{"class" -> "Notebook"},
				{
					XMLElement["p", {}, {"Normal textual cell"}],
					XMLElement["img", {
						"src" -> "FakeTestNotebook/0-Input.png",
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
				File["FakeTestNotebook/0-Input.png"] -> _?ImageQ
			|>,
			SameTest -> MatchQ
		];
	];

	(*-----------------------------*)
	(* With BuildType -> Drafts    *)
	(*-----------------------------*)

	Block[{
		$CurrentNotebookSupportFiles = <||>,
		$CurrentNotebookFile = "/tmp/FakeTestNotebook.nb"
	},
		VerificationTest[
			Block[{$BuildSettings = <| "BuildType" -> "Drafts" |>},
				ConvertToHTML[example]
			],
			XMLElement[
				"article",
				{"class" -> "Notebook"},
				{
					XMLElement["p", {}, {"Normal textual cell"}],
					XMLElement["img", {
						"src" -> "FakeTestNotebook/0-Input.png",
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
						"src" -> "FakeTestNotebook/1-Input.png",
						"width" -> "125",
						"height" -> "17",
						"style" -> "display: block; padding: 4pt 0 4pt 0;"
					}, {}]
				}
			]
		];

		VerificationTest[
			$CurrentNotebookSupportFiles,
			<|
				File["FakeTestNotebook/0-Input.png"] -> _?ImageQ,
				File["FakeTestNotebook/1-Input.png"] -> _?ImageQ
			|>,
			SameTest -> MatchQ
		];
	];
]

(*====================================*)
(* Test ComputedHTML cell conversion  *)
(*====================================*)

(* TID:250305/1: Basic conversion of ComputedHTML cells. *)
VerificationTest[
	ConvertToHTML @ Cell[
		(* XMLElement["p", {}, ToString[Today]] *)
		BoxData @ RowBox[{
			"XMLElement", "[",
			RowBox[{"\"p\"", ",", RowBox[{"{", "}"}], ",",
				RowBox[{"{", RowBox[{"DateString", "[", "Today", "]"}], "}"}]}],
			"]"
		}],
		"ConnorGray/ComputedHTML"
	],
	XMLElement["p", {}, {DateString[Today]}]
]

(* TID:250307/1: MakeHTML invalid Video argument error. *)
VerificationTest[
	Handle[_Failure] @ ConvertToHTML @ Notebook[{
		Cell[
			BoxData @ ToBoxes[Video[10]],
			"ConnorGray/ComputedHTML"
		]
	}],
	Failure[NotebookWebsiteError, <|
		"CausedBy" -> Failure[NotebookWebsiteError, <|
			"MessageTemplate" -> "Unrecognized form for Video source specification: ``. Expected String or URL expression.",
			"MessageParameters" -> {InputForm[10]}
		|>],
		"MessageTemplate" -> "Error using MakeHTML[..] to convert ComputedHTML style cell expression: ``",
		"MessageParameters" -> {Quiet @ Video[10]}
	|>],
	(* For the purposes of this example, this video file doesn't
		actually need to exist, so quite message about it not
		existing. *)
	{Video::badsource, Video::badsource}
]

(* TID:250308/1: Convert ConnorGray/ComputedHTML cells using MakeHTML. *)
(* TID:250308/2: Convert Video[..] in ComputedHTML cells. *)
VerificationTest[
	Block[{
		$CurrentNotebookFile = "/private/var/ExampleNotebook.mov"
	},
		ConvertToHTML @ Notebook[{
			Cell[
				BoxData @ ToBoxes[Video["/private/var/ExampleVideo.mov"]],
				"ConnorGray/ComputedHTML"
			]
		}]
	],
	XMLElement["article", {"class" -> "Notebook"}, {
		XMLElement["video", {
			"style" -> "max-width: 100%",
			"src" -> "ExampleVideo.mov",
			"loop" -> "true",
			"controls" -> "true",
			"autoplay" -> "true",
			"muted" -> "true"
		}, {""}]
	}],
	(* For the purposes of this example, this video file doesn't
		actually need to exist, so quite message about it not
		existing. *)
	{Video::fnfnd, Video::fnfnd}
]

(*====================================*)
(* Test special TemplateBox styles    *)
(*====================================*)

VerificationTest[
	Block[{
		$CurrentNotebookRelativeURL = URL["content/test-file.html"]
	},
		(* TID:240602/3: Convert inline PacletLink special link *)
		ConvertToHTML @ Cell[
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

(*====================================*)
(* Test conversion of tabbed content  *)
(*====================================*)

(* TID:240809/2: Handle ConnorGray/TabViewSection cell group. *)
VerificationTest[
	ConvertToHTML @ Cell @ CellGroupData[{
		Cell[
			"Tabbed Content",
			"Section",
			"ConnorGray/TabViewSection"
		],
		Cell @ CellGroupData[{
			Cell["Tab One Label", "Subsection"],
			Cell["This is some content in the first tab.", "Text"]
		}, Open],
		(* TID:240809/1: Multi-cell tab contents *)
		Cell @ CellGroupData[{
			Cell["Tab Two Label", "Subsection"],
			Cell["This is some content in the second tab.", "Text"],
			Cell["This tab has multiple cells", "Text"],
			Cell @ CellGroupData[{
				Cell["And Even an Inner Cell Group", "Subsubsection"],
				Cell["With its own content.", "Text"]
			}, Open]
		}, Open]
	}, Open]
	,
	XMLElement["div", {"class" -> "tabbed"}, {
		(*------------------*)
		(* Radio buttons    *)
		(*------------------*)

		XMLElement["input", {
			"type" -> "radio",
			"id" -> "tab-1-2edad004",
			"name" -> "css-tabs-2edad004",
			"checked" -> "true"
		}, {}],
		XMLElement["input", {
			"type" -> "radio",
			"id" -> "tab-2-2edad004",
			"name" -> "css-tabs-2edad004"
		}, {}],

		(*------------------*)
		(* Tab labels       *)
		(*------------------*)

		XMLElement["ul", {"class" -> "tabs"}, {
			XMLElement["li", {"class" -> "tab"}, {
				XMLElement["label", {"for" -> "tab-1-2edad004"}, {"Tab One Label"}]
			}],
			XMLElement["li", {"class" -> "tab"}, {
				XMLElement["label", {"for" -> "tab-2-2edad004"}, {"Tab Two Label"}]
			}]
		}],

		(*------------------*)
		(* Tab contents     *)
		(*------------------*)

		XMLElement["div", {"class" -> "tab-content"}, {
			XMLElement["p", {}, {"This is some content in the first tab."}]
		}],

		XMLElement["div", {"class" -> "tab-content"}, {
			XMLElement["p", {}, {"This is some content in the second tab."}],
			XMLElement["p", {}, {"This tab has multiple cells"}],
			XMLElement["h5", {"class" -> "nb-Subsubsection"}, {
				XMLElement["a", {
					"id" -> "and-even-an-inner-cell-group",
					"class" -> "anchor",
					"href" -> "#and-even-an-inner-cell-group"
				}, {
					"And Even an Inner Cell Group"
				}]
			}],
			XMLElement["p", {}, {"With its own content."}]
		}]
	}]
]

(* TID:240810/1: Tab with empty contents after filtering. *)
VerificationTest[
	Handle[_Failure] @ ConvertToHTML @ Cell @ CellGroupData[{
		Cell["Tabbed Content", "Section", "ConnorGray/TabViewSection"],
		Cell @ CellGroupData[{
			Cell["Tab One Label", "Subsection"],
			Cell[
				"This is some content in the first tab.",
				"Text",
				"ConnorGray/Excluded"
			]
		}, Open],
		Cell @ CellGroupData[{
			Cell["Tab Two Label", "Subsection"],
			Cell["This is some content in the second tab.", "Text"]
		}, Open]
	}, Open],
	Failure[NotebookWebsiteError, <|
		"CausedBy" -> Failure[NotebookWebsiteError, <|
			"CausedBy" -> Failure[NotebookWebsiteError, <|
				"MessageTemplate" -> "Empty tab contents are not supported",
				"MessageParameters" -> {}
			|>],
			"MessageTemplate" -> "Error processing tab at position ``",
			"MessageParameters" -> {InputForm[{1}]}
		|>],
		"MessageTemplate" -> "Error processing tabbed content",
		"MessageParameters" -> {}
	|>]
]

(* TID:240810/2: Tab with excluded header cell. *)
VerificationTest[
	Handle[_Failure] @ ConvertToHTML @ Cell @ CellGroupData[{
		Cell["Tabbed Content", "Section", "ConnorGray/TabViewSection"],
		Cell @ CellGroupData[{
			Cell["Tab One Label", "Subsection", "ConnorGray/Excluded"],
			Cell["This is some content in the first tab.", "Text"]
		}, Open],
		Cell @ CellGroupData[{
			Cell["Tab Two Label", "Subsection"],
			Cell["This is some content in the second tab.", "Text"]
		}, Open]
	}, Open],
	XMLElement["div", {"class" -> "tabbed"}, {
		(*------------------*)
		(* Radio buttons    *)
		(*------------------*)

		XMLElement["input", {
			"type" -> "radio",
			"id" -> "tab-1-df99fdb0",
			"name" -> "css-tabs-df99fdb0",
			"checked" -> "true"
		}, {}],

		(*------------------*)
		(* Tab labels       *)
		(*------------------*)

		XMLElement["ul", {"class" -> "tabs"}, {
			XMLElement["li", {"class" -> "tab"}, {
				XMLElement["label", {"for" -> "tab-1-df99fdb0"}, {"Tab Two Label"}]
			}]
		}],

		(*------------------*)
		(* Tab contents     *)
		(*------------------*)

		XMLElement["div", {"class" -> "tab-content"}, {
			XMLElement["p", {}, {"This is some content in the second tab."}]
		}]
	}]
]

(* TID:240810/3: Tab header with non-String cell data. *)
VerificationTest[
	Handle[_Failure] @ ConvertToHTML @ Cell @ CellGroupData[{
		Cell["Tabbed Content", "Section", "ConnorGray/TabViewSection"],

		Cell @ CellGroupData[{
			Cell["Tab One Header", "Subsection"],
			Cell["This is some content in the first tab.", "Text"]
		}, Open],

		Cell @ CellGroupData[{
			Cell[
				TextData[{"Tab Two ", StyleBox["Header", FontSlant->"Italic"]}],
				"Subsection"
			],
			Cell["This is some content in the second tab.", "Text"]
		}, Open]
	}, Open]
	,
	Failure[NotebookWebsiteError, <|
		"CausedBy" -> Failure[NotebookWebsiteError, <|
			"CausedBy" -> Failure[NotebookWebsiteError, <|
				"TabHeaderCell" -> Cell[
					TextData[{
						"Tab Two ",
						StyleBox["Header", FontSlant -> "Italic"]
					}],
					"Subsection"
				],
				"MessageTemplate" -> "Tab header cell data expected to be simple String.",
				"MessageParameters" -> {}
			|>],
			"MessageTemplate" -> "Error processing tab at position ``",
			"MessageParameters" -> {InputForm[{2}]}
		|>],
		"MessageTemplate" -> "Error processing tabbed content",
		"MessageParameters" -> {}
	|>]
]
