BeginPackage["ConnorGray`NotebookWebsiteTools`MakeHTML`"]

Begin["`Private`"]

Needs["ConnorGray`Utilities`"]

Needs["ConnorGray`NotebookWebsiteTools`"]
Needs["ConnorGray`NotebookWebsiteTools`Errors`"]
Needs["ConnorGray`NotebookWebsiteTools`Build`"]
Needs["ConnorGray`NotebookWebsiteTools`Utils`"]

(*---------------------------*)
(* MakeHTML[_?HTMLFragmentQ] *)
(*---------------------------*)

MakeHTML[html: _?HTMLFragmentQ] :=
	html

(*----------------------*)
(* MakeHTML[Video[...]] *)
(*----------------------*)

MakeHTML[
	HoldPattern @ Video[
		source: _,
		___?OptionQ
	]
] := Module[{
	url,
	src
},
	url = ConfirmReplace[source, {
		url: _?StringQ | URL[url: _?StringQ] :> (
			url
		),
		(* TID:250307/1: MakeHTML invalid Video argument error. *)
		other: _ :> Raise[
			NotebookWebsiteError,
			"Unrecognized form for Video source specification: ``. Expected "
			<> "String or URL expression.",
			InputForm[other]
		]
	}];

	(* TID:250308/2: Convert Video[..] in ComputedHTML cells. *)
	src = ConfirmReplace[URLParse[url], {
		KeyValuePattern[{
			"Scheme" -> None | "file",
			"Path" -> pathComponents: _List
		}] :> Module[{
			path = FileNameJoin[pathComponents]
		},
			path = RelativePath[
				(* Parent directory of current notebook. *)
				FileNameDrop[$CurrentNotebookFile],
				(* Assume that the video file is within the same directory as
					(directory or a subdirectory) as the current notebook.
					TODO: Support video files that are elsewhere in the website,
					and generating the relative URL to them. Also write a blog
					post with a visualization of relative paths within the same
					directory tree or to outside directory trees.
				*)
				path
			];

			path
		],
		KeyValuePattern[{"Scheme" -> "http" | "https"}] :> (
			url
		),
		other: _ :> (
			Raise[
				NotebookWebsiteError,
				<| "VideoSource" -> source, "ParsedURL" -> other |>,
				"Unsupported URL or path source form for Video HTML."
			]
		)
	}];

	(* NOTE: Empty "" string as video element content is necessary because
		ExportString[xml, "XML"] gives a self-closing tag if the data list is
		empty, which is not valid HTML for most tags, causing the resulting
		HTML to render incorrectly in Google Chrome. We're forcing the
		generation of `<video ...></video>` instead of `<video ... />`.
	*)
	XMLElement["video", {
		"src" -> src,
		"loop" -> "true",
		"controls" -> "true",
		"autoplay" -> "true",
		"muted" -> "true"
	}, {""}]
]

End[]

EndPackage[]