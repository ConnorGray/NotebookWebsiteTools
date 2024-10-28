Needs["ConnorGray`NotebookWebsiteTools`Utils`"]

round[expr_] := (
	expr /. n_Real :> Round[n, 0.01]
)

VerificationTest[
	round @ CodeSyntaxHighlight[#, "Rust", "Solarized (light)"]& @ StringRiffle[{
		"fn main() {",
		"    let x = 2 + 2;",
		"}"
	}, "\n"],
	{RGBColor[0.99, 0.96, 0.89], {
		{
			Style["fn", FontColor -> RGBColor[0.15, 0.55, 0.82]],
			Style[" ", FontColor -> RGBColor[0.4, 0.48, 0.51]],
			Style["main", FontColor -> RGBColor[0.71, 0.54, 0.]],
			Style["(", FontColor -> RGBColor[0.4, 0.48, 0.51]],
			Style[")", FontColor -> RGBColor[0.4, 0.48, 0.51]],
			Style[" ", FontColor -> RGBColor[0.4, 0.48, 0.51]],
			Style["{", FontColor -> RGBColor[0.4, 0.48, 0.51]],
			Style["\n", FontColor -> RGBColor[0.4, 0.48, 0.51]]
		},
		{
			Style["    ", FontColor -> RGBColor[0.4, 0.48, 0.51]],
			Style["let", FontColor -> RGBColor[0.15, 0.55, 0.82]],
			Style[" x ", FontColor -> RGBColor[0.4, 0.48, 0.51]],
			Style["=", FontColor -> RGBColor[0.52, 0.6, 0.]],
			Style[" ", FontColor -> RGBColor[0.4, 0.48, 0.51]],
			Style["2", FontColor -> RGBColor[0.42, 0.44, 0.77]],
			Style[" ", FontColor -> RGBColor[0.4, 0.48, 0.51]],
			Style["+", FontColor -> RGBColor[0.52, 0.6, 0.]],
			Style[" ", FontColor -> RGBColor[0.4, 0.48, 0.51]],
			Style["2", FontColor -> RGBColor[0.42, 0.44, 0.77]],
			Style[";", FontColor -> RGBColor[0.4, 0.48, 0.51]],
			Style["\n", FontColor -> RGBColor[0.4, 0.48, 0.51]]
		},
		{
			Style["}", FontColor -> RGBColor[0.4, 0.48, 0.51]]
		}
	}}
]

VerificationTest[
	round @ CodeSyntaxHighlight[
		StringRiffle[{
			"fn main() {",
			"    let x = 2 + 2;",
			"}"
		}, "\n"],
		"Rust",
		"Solarized (light)",
		{
			{0, 3 ;; 7} -> (Background -> Yellow),
			{1, 4 ;; 7} -> Directive[
				FontColor -> Red,
				Background -> Yellow
			],
			{1, 12 ;; 17} -> Bold
		}
	],
	{RGBColor[0.99, 0.96, 0.89], {
		{
			Style["fn", FontColor -> RGBColor[0.15, 0.55, 0.82]],
			Style[" ", FontColor -> RGBColor[0.4, 0.48, 0.51]],
			Style["main",
				FontColor -> RGBColor[0.71, 0.54, 0.],
				Background -> RGBColor[1., 1., 0.]
			],
			Style["(", FontColor -> RGBColor[0.4, 0.48, 0.51]],
			Style[")", FontColor -> RGBColor[0.4, 0.48, 0.51]],
			Style[" ", FontColor -> RGBColor[0.4, 0.48, 0.51]],
			Style["{", FontColor -> RGBColor[0.4, 0.48, 0.51]],
			Style["\n", FontColor -> RGBColor[0.4, 0.48, 0.51]]
		},
		{
			Style["    ", FontColor -> RGBColor[0.4, 0.48, 0.51]],
			Style["let",
				FontColor -> RGBColor[1., 0., 0.],
				Background -> RGBColor[1., 1.0, 0.]
			],
			Style[" x ", FontColor -> RGBColor[0.4, 0.48, 0.51]],
			Style["=", FontColor -> RGBColor[0.52, 0.6, 0.]],
			Style[" ", FontColor -> RGBColor[0.4, 0.48, 0.51]],
			Style["2", FontColor -> RGBColor[0.42, 0.44, 0.77], Bold],
			Style[" ", FontColor -> RGBColor[0.4, 0.48, 0.51], Bold],
			Style["+", FontColor -> RGBColor[0.52, 0.6, 0.], Bold],
			Style[" ", FontColor -> RGBColor[0.4, 0.48, 0.51], Bold],
			Style["2", FontColor -> RGBColor[0.42, 0.44, 0.77], Bold],
			Style[";", FontColor -> RGBColor[0.4, 0.48, 0.51]],
			Style["\n", FontColor -> RGBColor[0.4, 0.48, 0.51]]
		},
		{
			Style["}", FontColor -> RGBColor[0.4, 0.48, 0.51]]
		}
	}}
]

(*====================================*)
(* DeleteDelimitedLines               *)
(*====================================*)

(* TID/241027/1: Deleted interior section of hidden lines. *)
(*
	Ensure only a single newline separates blocks in cases like:

		foo

		# start-exclude
		bar
		# end-exclude

		baz

	which should result in:

		foo

		baz

	and NOT:

		foo


		baz
*)
VerificationTest[
	DeleteDelimitedLines[#, {"begin-hidden", "end-hidden"}]& @
"
struct Before {}

# begin-hidden
impl Before { ..}
# end-hidden

fn main() {}
"
,
"
struct Before {}

fn main() {}
"
]

(* TID/241027/2: Deleted leading section of hidden lines. *)
VerificationTest[
	DeleteDelimitedLines[#, {"begin-hidden", "end-hidden"}]& @
"
# begin-hidden
struct Before {}
# end-hidden

fn main() {}
"
,
"
fn main() {}
"
]