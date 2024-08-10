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
		Style["fn", FontColor -> RGBColor[0.15, 0.55, 0.8200000000000001]],
		Style[" ", FontColor -> RGBColor[0.4, 0.48, 0.51]],
		Style["main", FontColor -> RGBColor[0.71, 0.54, 0.]],
		Style["(", FontColor -> RGBColor[0.4, 0.48, 0.51]],
		Style[")", FontColor -> RGBColor[0.4, 0.48, 0.51]],
		Style[" ", FontColor -> RGBColor[0.4, 0.48, 0.51]],
		Style["{", FontColor -> RGBColor[0.4, 0.48, 0.51]],
		Style["\n", FontColor -> RGBColor[0.4, 0.48, 0.51]],
		Style["    ", FontColor -> RGBColor[0.4, 0.48, 0.51]],
		Style["let", FontColor -> RGBColor[0.15, 0.55, 0.82]],
		Style[" x ", FontColor -> RGBColor[ 0.4, 0.48, 0.51]],
		Style["=", FontColor -> RGBColor[ 0.52, 0.6, 0.]],
		Style[" ", FontColor -> RGBColor[ 0.4, 0.48, 0.51]],
		Style["2", FontColor -> RGBColor[ 0.42, 0.44, 0.77]],
		Style[" ", FontColor -> RGBColor[ 0.4, 0.48, 0.51]],
		Style["+", FontColor -> RGBColor[ 0.52, 0.6, 0.]],
		Style[" ", FontColor -> RGBColor[ 0.4, 0.48, 0.51]],
		Style["2", FontColor -> RGBColor[ 0.42, 0.44, 0.77]],
		Style[";", FontColor -> RGBColor[ 0.4, 0.48, 0.51]],
		Style["\n", FontColor -> RGBColor[ 0.4, 0.48, 0.51]],
		Style["}", FontColor -> RGBColor[ 0.4, 0.48, 0.51]]
	}}
]
