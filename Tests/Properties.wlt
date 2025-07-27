Needs["ConnorGray`Utilities`"]

Needs["ConnorGray`NotebookWebsiteTools`"]
Needs["ConnorGray`NotebookWebsiteTools`Build`"]
Needs["ConnorGray`NotebookWebsiteTools`CurrentBuild`"]


(* Test initial content of a new website notebook. *)
VerificationTest[
	nb = UsingFrontEnd @ CreateWebsiteNotebook["BlogPost", "The Title"];

	UsingFrontEnd @ NotebookGet[nb]
	,
	Notebook[
		{
			Cell[CellGroupData[ {
				Cell["The Title", "Title"],
				Cell[s_?StringQ /; StringStartsQ[s, "Created "], "Subtitle"]
			}, Open]]
		},
		WindowSize -> _,
		WindowMargins -> _,
		DockedCells -> {
			Cell[___],
			Inherited
		},
		TaggingRules -> {
			"ConnorGray/NotebookWebsiteTools" -> {
				"DocumentType" -> "BlogPost",
				"CreatedByPacletVersion" -> "0.0.1"
			},
			"NotebookWebsiteToolsIsInstalled" -> True
		},
		Initialization :> _,
		FrontEndVersion -> _,
		StyleDefinitions -> FrontEnd`FileName[
			{"ConnorGray"},
			"NotebookWebsiteTools.nb",
			CharacterEncoding -> "UTF-8"
		]
	]
	,
	SameTest -> MatchQ
]

(* TID:250723/1: WebsiteNotebookTags extraction of notebook tags. *)
VerificationTest[
	WebsiteNotebookTags @ Notebook[{

	},
		TaggingRules -> <|
			"ConnorGray/NotebookWebsiteTools" -> <|
				"DocumentType" -> "BlogPost",
				"CreatedByPacletVersion" -> "0.0.1",
				"ContentTags" -> {"Topic:ProgrammingLanguageDesign"}
			|>
		|>
	],
	{"Topic:ProgrammingLanguageDesign"}
]

(* TID:250723/2: Test pattern for listing documents matching content tag. *)
Module[{
	tmpDir, buildResult
},
	tmpDir = FileSystemBundleExport @ FileSystemBundle[<|
		"Content/Main.nb" -> ToString[#, InputForm]& @ Notebook[{
			Cell[
				(* Note: Block $ContextPath to force the generated boxes
				   to use fully-qualified symbol names. *)
				BoxData @ Block[{$ContextPath = {"System`"}}, MakeBoxes @ (
					Needs["ConnorGray`NotebookWebsiteTools`Build`"];
					Needs["ConnorGray`NotebookWebsiteTools`CurrentBuild`"];
					PagesSummaryListHtml[
						Automatic,
						{nbFileRelative, nbFileAbsolute} |-> Module[{
							tags = GetBuildValue[{File[nbFileAbsolute], WebsiteNotebookTags}]
						},
							MemberQ[tags, "Topic:ExamplePost"]
						]
					]
				)],
				"ConnorGray/ComputedHTML"
			]
		},
			TaggingRules -> <|
				"ConnorGray/NotebookWebsiteTools" -> <|
					"DocumentType" -> "BlogPost",
					"DocumentStatus" -> "Published"
				|>
			|>
		],
		"Content/Tagged.nb" -> ToString[#, InputForm]& @ Notebook[
			{Cell["Some Tagged Document", "Title"]},
			TaggingRules -> <|
				"ConnorGray/NotebookWebsiteTools" -> <|
					"DocumentType" -> "BlogPost",
					"DocumentStatus" -> "Published",
					"ContentTags" -> {"Topic:ExamplePost"}
				|>
			|>
		]
	|>];

	buildResult = NotebookWebsiteBuild[tmpDir];

	VerificationTest[
		buildResult,
		Success["NotebookWebsiteBuild", _?AssociationQ],
		SameTest -> MatchQ
	];

	(* Verify that the built HTML has summary list content for the page with
	   matching tag. *)
	VerificationTest[
		Import[buildResult[[2]]["OutputHTMLFiles"][[1]], "XML"]
		,
		XMLObject["Document"][
			{},
			XMLElement["html", {}, {
				XMLElement["head", {}, {
					XMLElement["meta", {"name" -> "viewport", "content" -> "width=device-width, initial-scale=1"}, {}],
					XMLElement["link", {"rel" -> "stylesheet", "href" -> "web_assets/notebook-website-default.css"}, {}]
				}],
				XMLElement["body", {}, {
					XMLElement["article", {"class" -> "Notebook"}, {
						XMLElement["nav", {"class" -> "PagesSummaryList"}, {
							XMLElement["ul", {}, {
								XMLElement["li", {}, {
									XMLElement["h4", {"class" -> "DocumentLink"}, {
										XMLElement["a", {"href" -> "Tagged.html"}, {"Some Tagged Document"}]}]
								}]
							}]
						}]
					}]
				}]
			}],
			{}
		]
	];
]