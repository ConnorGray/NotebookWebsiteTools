(*
	A self-contained package for representing CSS symbolically, and generating
	CSS from builtin Wolfram notebook styles.
*)

BeginPackage["ConnorGray`NotebookWebsiteTools`CSS`"]

(* See Also: https://developer.mozilla.org/en-US/docs/Web/CSS/Syntax#css_rulesets *)
CSSRuleset::usage = "CSSRuleset[selectors, declarations]"

CSSToString::usage = "CSSToString[css] converts symbolic CSS into a CSS program string."

WolframStyleToCSS::usage = "WolframStyleToCSS[style, cellOptions]"

Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`Errors`"]

(*========================================================*)
(* Wolfram To Symbolic CSS                                *)
(*========================================================*)

WolframStyleToCSS[
	style: _?StringQ,
	options0 : {(_Symbol | _?StringQ)..} : Automatic
] := Module[{
	options = Replace[options0, Automatic :> $SupportedCellOptions],
	optionValues
},
	If[!TrueQ[$Notebooks],
		Raise[
			NotebookWebsiteError,
			"Unable to determine notebook styles for ``: no front end is available.",
			InputForm[style]
		];
	];

	optionValues = Normal @ AssociationMap[
		(*
			Use CurrentValue[{StyleDefinitions, ..}] to get the value for this
			option that is known to the current FE session.

			This is dependent on the FE session and specific notebook (if any)
			that the current evaluation was initiated from.
		*)
		option |-> CurrentValue[{StyleDefinitions, style, option}],
		options
	];

	RaiseAssert[MatchQ[optionValues, {___Rule}]];

	WolframStyleToCSS[style, optionValues]
]

(*------------------------------------*)

WolframStyleToCSS[
	style: _?StringQ,
	optionValues: {Rule[_, _]...}
] := Module[{
	declarations
},
	declarations = Flatten @ Map[cellOptionToCSSDeclarations, optionValues];

	CSSRuleset["." <> styleNameToCSS[style], declarations]
]

SetFallthroughError[WolframStyleToCSS]

(*------------------------------------*)

(*
	Cell options that can be automatically convered to equivalent CSS.

	Only cell options supported by cellOptionToCSSDeclaration should be listed
	here.
*)
$SupportedCellOptions = {
	CounterAssignments,
	CounterIncrements
}

Protect[$SupportedCellOptions]


cellOptionToCSSDeclarations[rule: Rule[_, _]] := Replace[rule, {
	Rule[CounterAssignments, {}] :> {},
	Rule[CounterAssignments, assignments: _?ListQ] :> (
		"counter-reset" -> StringRiffle[
			Map[
				Replace[{
					{name: _?StringQ, 0} :> (
						styleNameToCSS[name]
					),
					{name: _?StringQ, value: _?IntegerQ} :> (
						StringJoin[styleNameToCSS[name], " ", ToString[value]]
					),
					other: _ :> Raise[NotebookWebsiteError, "Unrecognized CounterAssignments value: ``", other]
				}],
				assignments
			],
			" "
		]
	),
	Rule[CounterIncrements, {}] :> {},
	Rule[CounterIncrements, name: _?StringQ] :> (
		"counter-increment" -> styleNameToCSS[name]
	),
	other: _ :> Raise[NotebookWebsiteError, "Unhandled cell style rule: ``", other]
}]

(*========================================================*)
(* Symbolic CSS To String                                 *)
(*========================================================*)

CSSToString[CSSRuleset[selectors0: _, declarations0: _?ListQ]] := Module[{
	selectors = Replace[selectors0, {
		value: _?StringQ :> {value},
		list: _?ListQ :> list,
		other: _ :> Raise[NotebookWebsiteError, "Invalid CSSRuleset selectors: ``", InputForm[other]]
	}],
	declarations = declarations0
},
	declarations = Map[
		Replace[{
			(property: _?StringQ -> value: _?StringQ) :> StringJoin[property, ": ", value],
			other: _ :> Raise[NotebookWebsiteError, "Invalid symbolic CSS declaration: ``", other]
		}],
		declarations
	];

	RaiseAssert[MatchQ[declarations, {___?StringQ}]];

	Scan[
		selector |-> If[!SelectorQ[selector],
			Raise[NotebookWebsiteError, "Invalid symbolic CSS selector: ``", selector];
		],
		selectors
	];

	declarations = Map[decl |-> StringJoin["\t", decl], declarations];

	StringJoin[
		Riffle[selectors, ", "],
		" {\n",
		Riffle[declarations, ";\n"],
		If[declarations =!= {}, ";", ""],
		"\n}"
	]
]

(*------------------------------------*)

CSSToString[items: {___CSSRuleset}] :=
	StringRiffle[Map[CSSToString, items], "\n\n"]

SetFallthroughError[CSSToString]

(*====================================*)

(* TODO: Make this return False for invalid CSS selector syntax. *)
SelectorQ[expr: _] := StringQ[expr]

SetFallthroughError[SelectorQ]

(*====================================*)

styleNameToCSS[style: _?StringQ] := StringJoin["nb-", style]


End[]

EndPackage[]