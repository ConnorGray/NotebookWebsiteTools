BeginPackage["ConnorGray`NotebookWebsiteTools`Utils`"]

Needs["GeneralUtilities`" -> "GU`"]

(*---------------------*)
(* FrontEnd Operations *)
(*---------------------*)
ConvertToString
NotebookCells
CellDataQ
Rasterize2

(*------*)
(* HTML *)
(*------*)
HTMLEscape
HTMLFragmentQ
GetWebsiteFavicon

(*------------------------------*)
(* Compuational Essay Authoring *)
(*------------------------------*)
DeleteDelimitedLines

Begin["`Private`"]

Needs["ConnorGray`Utilities`"]

Needs["ConnorGray`NotebookWebsiteTools`Errors`"]

(*========================================================*)
(* FrontEnd Operations                                    *)
(*========================================================*)

SetFallthroughError[ConvertToString]

ConvertToString[expr: _] := Replace[expr, {
	string: _?StringQ /; StringMatchQ[string, "\"" ~~ ___ ~~ "\""] :> ToExpression[string],
	string: _?StringQ :> string,
	items: _?ListQ :> StringJoin[ConvertToString /@ items],
	TextData[content: _] :> ConvertToString[content],
	StyleBox[content: _, ___] :> ConvertToString[content],
	BoxData[content: _] :> ConvertToString[content],
	RowBox[items: _?ListQ] :> StringJoin[ConvertToString /@ items],
	TemplateBox[items: _?ListQ, "RowDefault"] :> StringJoin[ConvertToString /@ items],
	ButtonBox[content: _, ___] :> ConvertToString[content],

	(*-------------------------------*)
	(* NotebookWebsiteTools specific *)
	(*-------------------------------*)

	(* FIXME:
		This should not be part of the general-purpose ConvertToString
		function. Instead make WebsiteNotebookSnippet smarter. E.g.
		WebsiteNotebookSnippet[nb, "PlainText" | "HTML"]. *)
	Cell[
		BoxData @ TemplateBox[
			{label: _?StringQ, url: _?StringQ},
			"ConnorGray/GitHubLink"
		],
		FontWeight -> "Bold",
		(* TODO: Handle options *)
		___?OptionQ
	] :> label,

	other: _ :> Raise[NotebookWebsiteError, "no rule to convert form to string: ``", InputForm[other]]
}]

(*========================================================*)

GU`SetUsage[NotebookCells, "
NotebookCells[notebook$] returns a list of all top-level cells in notebook,
after flattening out cell groups.
"]

SetFallthroughError[NotebookCells]

NotebookCells[
	Notebook[cells: {___Cell}, ___?OptionQ]
] := flattenCellGroups[cells]

(*------------------------------------*)

flattenCellGroups[cells: {___Cell}] :=
	Flatten @ Map[
		Replace[{
			Cell[
				CellGroupData[groupCells: {___Cell}, ___],
				___
			] :> (
				flattenCellGroups[groupCells]
			),
			Cell[group: _CellGroupData, ___] :> (
				Raise[NotebookWebsiteError, "Unexpected CellGroupData structure: ``", group]
			),
			normalCell: _Cell :> normalCell,
			other: _ :> Raise[NotebookWebsiteError, "Unexpected notebook structure: ``", InputForm[other]]
		}],
		cells
	]

(*========================================================*)

GU`SetUsage[CellDataQ, "
CellDataQ[expr$] returns True if expr$ is a valid cell content type, typically a
string, TextData[$$] or BoxData[$$].

* Box expressions are not considered valid cell data, as they cannot validly
  appear as the first argument of Cell[$$].
"]

SetFallthroughError[CellDataQ]

CellDataQ[expr: _] :=
	(* TODO: More obscure or deprecated forms. e.g. GraphicsData or OutputFormData? *)
	MatchQ[expr, Alternatives[
		_?StringQ,
		TextData[_],
		BoxData[_]
	]]

(*====================================*)

SetFallthroughError[Rasterize2]

Rasterize2[
	expr: _,
	outputElems: _?OutputElementsQ : Automatic
] := Module[{
	$rasterResolution = 270,
	(* The native PPI resolution of the FrontEnd on this device. This is
		typically 144 on HiDPI computers. *)
	$frontEndResolution,
	$frontEndScale,
	image,
	imageRelativeUrl,
	imageCSSPixelSize
},
	{$frontEndResolution, $frontEndScale} = Replace[
		CurrentValue["ConnectedDisplays"],
		{
			{
				KeyValuePattern[{
					"Resolution" -> resolution: _?NumberQ,
					"Scale" -> scale: _?NumberQ
				}],
				___
			} :> {resolution, scale},
			other: _ :> Raise[
				NotebookWebsiteError,
				"Unexpected \"ConnectedDisplays\" value: ``",
				InputForm[other]
			]
		}
	];

	RaiseAssert[NumberQ[$frontEndResolution]];

	RaiseAssert[
		$frontEndResolution == 144 || $frontEndResolution == 72,
		"Unexpected FrontEnd resolution: ``", $frontEndResolution
	];

	image = Rasterize[
		expr,
		ImageResolution -> $rasterResolution,
		Background -> ColorConvert[Transparent, "RGB"]
	];

	RaiseAssert[
		ImageQ[image],
		"expected cell Rasterize result to be Image, got: ``",
		InputForm[image]
	];

	(*------------------------------------------------------------------*)
	(* Calculate the image dimensions in CSS pixels that will result in *)
	(* the cell image having the same physical on-screen size as when   *)
	(* viewed in a notebook. When viewing the notebook next to the web  *)
	(* page at the same magnification, the two should appear identical  *)
	(* in size.                                                         *)
	(*------------------------------------------------------------------*)

	(* These are the dimensions `image` would have if `image` was rasterized
		at the default front end resolution. *)
	imageCSSPixelSize =
		ImageDimensions[image] / ($rasterResolution / $frontEndResolution);

	(* Account for the fact that HTML pixels are defined as 1/96th of an inch,
		so they already compensate for the DPI scale; meaning we need to
		divide the physical dimensions of the image by the scaling factor of
		the FE they were rendered by. *)
	imageCSSPixelSize /= $frontEndScale;

	imageCSSPixelSize //= Round;

	RaiseAssert[MatchQ[imageCSSPixelSize, {_?IntegerQ, _?IntegerQ}]];

	ConstructOutputElements[
		outputElems,
		"Image",
		{
			"Image" :> image,
			"CSSPixelSize" :> imageCSSPixelSize
		}
	]
]

(*========================================================*)
(* HTML                                                   *)
(*========================================================*)

GU`SetUsage[HTMLEscape, "
HTMLEscape[string$] escapes string so that it can be embedded in HTML as regular
text.
"]

SetFallthroughError[HTMLEscape]

(* NOTE: This function is required because exporting an XMLElement[..] using the
	"XML" format escapes '<' and '>' characters in `text`, but exporting as
	using the "HTMLFragment" format does not escape those characters. *)
HTMLEscape[text: _?StringQ] := StringReplace[
	ExportString[XMLElement["Text", {}, {text}], "XML"],
	StartOfString ~~ "<Text>" ~~ content: ___ ~~ "</Text>" ~~ EndOfString :> content
]

(*========================================================*)

GU`SetUsage[HTMLFragmentQ, "
HTMLFragmentQ[expr$] returns True if expr$ can validly appear as an element in a
list that is a 3rd argument of XMLElement.
"]

(* TODO: Include XML`RawXML["..."] here? *)
HTMLFragmentQ[expr: _] :=
	MatchQ[expr, _?StringQ | _XMLElement | Nothing | Splice[{___?HTMLFragmentQ}]]

(*========================================================*)

GU`SetUsage[GetWebsiteFavicon, "
GetWebsiteFavicon[url$] attempts to retrieve the favicon of a website as an
Image or Graphics expression.
"]

GetWebsiteFavicon[url: _?StringQ | URL[url: _?StringQ]] := Module[{
	domain,
	apiUrl,
	resp
},
	(* TODO:
		Is stripping the Path segment of the URL really required, and is
		it perhaps also invalid? I think its probably required as long as this
		function is using the Google-provided API, since otherwise there would
		be an ambiguity for any query parameters that appear in `url`. Perhaps
		though the URL parameter could be parameter encoded to fix that?

		I.e. so that:
			GetWebsiteFavicon["https://example.org/path?with_param=true"]
		doesn't do naive string concatenation to get:
			https://www.google.com/s2/favicons?domain=https://example.org/path?with_param=true
		which invalidly has two "?" in it. We could instead URLEncode or
		URLQueryEncode the domain parameter.
	*)
	domain = URLParse[url]["Domain"];

	If[!StringQ[domain],
		Raise[
			NotebookWebsiteError,
			"Unable to extract domain from URL ``: ``",
			InputForm[url], InputForm[domain]
		];
	];

	(* Per: https://dev.to/derlin/get-favicons-from-any-website-using-a-hidden-google-api-3p1e *)
	apiUrl = StringJoin[
		"https://www.google.com/s2/favicons?domain=",
		domain,
		"&sz=32"
	];

	resp = URLRead[apiUrl, TimeConstraint -> 10];

	RaiseAssert[MatchQ[resp, _HTTPResponse]];

	If[resp["StatusCode"] =!= 200,
		(* FIXME: Improve this error handling. Could be especially common and
		          flaky way for a website build to fail. *)
		Raise[
			NotebookWebsiteError,
			<| "Response" -> resp |>,
			"Favicon lookup returned non-success HTTP response (status=``)",
			InputForm[resp["StatusCode"]]
		];
	];

	favicon = Import[resp];

	ConfirmReplace[favicon, {
		_Image :> favicon,
		other: _ :> (
			Raise[
				NotebookWebsiteError,
				"Unexpected imported favicon data head: ``",
				Head[other]
			];
		)
	}]
]

(*========================================================*)
(* Compuational Essay Authoring *)
(*========================================================*)

(*========================================================*)

GU`SetUsage[DeleteDelimitedLines, "
	DeleteDelimitedLines[text$, {start$, end$}] returns a string where runs of lines
"]

SetFallthroughError[DeleteDelimitedLines]

DeleteDelimitedLines[
	text: _?StringQ,
	{startMarker: _?StringQ, endMarker: _?StringQ}
] := Module[{
	excludedPatt,
	lines
},
	excludedPatt = Shortest @ PatternSequence[
		start: _ /; StringContainsQ[start, startMarker],
		___,
		end: _ /; StringContainsQ[end, endMarker]
	];

	lines = StringSplit[text, "\n", All];

	RaiseAssert[MatchQ[lines, {___?StringQ}]];

	lines = ReplaceRepeated[lines, {
		(* TID/241027/1: Deleted interior section of hidden lines. *)
		{
			most: ___,
			"",
			excludedPatt,
			"",
			rest: ___
		} :> {most, "", rest},

		(* TID/241027/2: Deleted leading section of hidden lines. *)
		{
			excludedPatt,
			"",
			rest: ___
		} :> {rest},

		(* Handle any other cases *)
		{
			most: ___,
			excludedPatt,
			rest: ___
		} :> {most, rest}
	}];

	StringRiffle[lines, "\n"]
]

(*========================================================*)

End[]

EndPackage[]