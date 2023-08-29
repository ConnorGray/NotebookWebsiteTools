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
	Rule[CounterAssignments, assignments_?ListQ] :> (
		"counter-reset" -> StringRiffle[
			Map[
				Replace[{
					{name_?StringQ, 0} :> (
						styleNameToCSS[name]
					),
					{name_?StringQ, value_?IntegerQ} :> (
						StringJoin[styleNameToCSS[name], " ", ToString[value]]
					),
					other_ :> Raise[NotebookWebsiteError, "Unrecognized CounterAssignments value: ``", other]
				}],
				assignments
			],
			" "
		]
	),
	Rule[CounterIncrements, {}] :> {},
	Rule[CounterIncrements, name_?StringQ] :> (
		"counter-increment" -> styleNameToCSS[name]
	),
	other_ :> Raise[NotebookWebsiteError, "Unhandled cell style rule: ``", other]
}]

(*========================================================*)
(* Symbolic CSS To String                                 *)
(*========================================================*)

CSSToString[CSSRuleset[selectors0_, declarations0_?ListQ]] := Module[{
	selectors = Replace[selectors0, {
		value_?StringQ :> {value},
		list_?ListQ :> list,
		other_ :> Raise[NotebookWebsiteError, "Invalid CSSRuleset selectors: ``", InputForm[other]]
	}],
	declarations = declarations0
},
	declarations = Map[
		Replace[{
			(property_?StringQ -> value_?StringQ) :> StringJoin[property, ": ", value],
			other_ :> Raise[NotebookWebsiteError, "Invalid symbolic CSS declaration: ``", other]
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

CSSToString[items:{___CSSRuleset}] :=
	StringRiffle[Map[CSSToString, items], "\n\n"]

SetFallthroughError[CSSToString]

(*====================================*)

(* TODO: Make this return False for invalid CSS selector syntax. *)
SelectorQ[expr_] := StringQ[expr]

SetFallthroughError[SelectorQ]

(*====================================*)

styleNameToCSS[style_?StringQ] := StringJoin["nb-", style]


End[]

EndPackage[]