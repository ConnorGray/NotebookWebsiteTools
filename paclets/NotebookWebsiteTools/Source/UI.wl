(*
	The functions in this file implement the UI actions of website notebooks.

	Each UI action corresponds to a call to one of the functions in this file.

	This convention ensures that each GUI action has a programmatic equivalent.

	NOTE:
		The functions and symbols from this context are embedded in the
		notebook GUI cell expressions. Backwards-incompatible changes to the
		exported symbols or their argument structures will break UI elements
		from notebooks created by previous versions of NotebookWebsiteTools
		(unless an appropriate UpdateNotebook[..] call has been implemented and
		made).
*)

BeginPackage["ConnorGray`NotebookWebsiteTools`UI`"]

ToggleExcluded::usage = "ToggleExcluded toggles the Excluded status of selected cells."
ToggleDraft::usage = "ToggleDraft toggles the Draft status of selected cells."
ToggleTabViewSection

ShowPreview

$GitHubIcon :=
	$GitHubIcon = Import[
		PacletObject["ConnorGray/NotebookWebsiteTools"]["AssetLocation", "GitHubIcon"]
	]

Begin["`Private`"]

Needs["ConnorGray`Utilities`"]

Needs["ConnorGray`NotebookWebsiteTools`"]
Needs["ConnorGray`NotebookWebsiteTools`Errors`"]
Needs["ConnorGray`NotebookWebsiteTools`Utils`"]

(*====================================*)

ToggleExcluded[nb: _NotebookObject] := Module[{
	cells = SelectedCells[nb]
},
	RaiseAssert[MatchQ[cells, {___CellObject}]];

	Scan[cell |-> toggleCellStyle[cell, "ConnorGray/Excluded"], cells];

	(* Return the cells that we modified. *)
	cells
]

SetFallthroughError[ToggleExcluded]

(*====================================*)

SetFallthroughError[ToggleDraft]

ToggleDraft[nb: _NotebookObject] := Module[{
	cells = SelectedCells[nb]
},
	RaiseAssert[MatchQ[cells, {___CellObject}]];

	(* FIXME: Setting the 'Draft' style should remove the 'Excluded' style,
		and vice versa. *)
	Scan[cell |-> toggleCellStyle[cell, "ConnorGray/Draft"], cells];

	(* Return the cells that we modified. *)
	cells
]

(*====================================*)

SetFallthroughError[ToggleTabViewSection]

ToggleTabViewSection[nb: _NotebookObject] := Module[{
	cells = SelectedCells[nb]
},
	RaiseAssert[MatchQ[cells, {___CellObject}]];

	Scan[cell |-> toggleCellStyle[cell, "ConnorGray/TabViewSection"], cells];

	(* Return the cells that we modified. *)
	cells
]

(*====================================*)

Options[ShowPreview] = {
	"BuildType" -> "PreviewDraftsAsPublished",
	"EmbedImages" -> False
}

SetFallthroughError[ShowPreview]

ShowPreview[
	nbObj0: _NotebookObject,
	opts:OptionsPattern[]
] := Module[{
	nbPath = NotebookFileName[nbObj0],
	nbObj,
	originalWebsiteDir,
	relativePath,
	tmpBuildDir,
	result
},
	{originalWebsiteDir, relativePath} = ConfirmReplace[FileNameSplit[nbPath], {
		{path: ___, "Content", rel: ___} :> {
			FileNameJoin[{path}],
			FileNameJoin[{rel}]
		}
	}];

	RaiseAssert[DirectoryQ[originalWebsiteDir]];

	(*--------------------------------*)
	(* Build the temporary website    *)
	(*--------------------------------*)

	tmpBuildDir = CreateDirectory[];

	result = RaiseConfirm @ NotebookWebsiteBuild[
		originalWebsiteDir,
		tmpBuildDir,
		"FileFilterFunction" -> Function[{assoc},
			(* Include all non-nb asset files, but make the only notebook
				we build be the current notebook. *)
			If[FileExtension[assoc["RelativePath"]] =!= "nb",
				True
				,
				assoc["RelativePath"] === relativePath
			]
		],
		ForwardOptions[opts]
	];

	(* Print[result]; *)

	ConfirmReplace[result, {
		Success["NotebookWebsiteBuild", KeyValuePattern[{
			"OutputHTMLFiles" -> {file: _File}
		}]] :> SystemOpen[file]
	}];
]

(*========================================================*)
(* Utilities                                              *)
(*========================================================*)

toggleCellStyle[cell : _CellObject, style : _?StringQ] := Module[{
	(* Note: StyleNames is an undocumented special option that returns all of
		the "secondary" styles of a cell.

		For example, given a cell with the structure:

			Cell["Some content", "Title", "ConnorGray/Excluded"]

		The the primary style is the first style, `"Title"` in this case, and
		the secondary styles are `{"ConnorGray/Excluded"}`.
	*)
	currentStyles = RaiseConfirm @ Lookup[Options[cell, StyleNames], StyleNames]
},
	currentStyles = Replace[currentStyles, s: _?StringQ :> {s}];

	RaiseAssert[MatchQ[currentStyles, {___?StringQ}], "currentStyles: ``", InputForm @ currentStyles];

	(* Note: This requires two SetOptions calls because setting this property
		to any value other than Inherited can only *add* styles to the cell,
		not remove them. So we first clear the secondary styles by setting
		this to Inherited, and then we add back only the styles we want to
		keep. *)
	SetOptions[cell, StyleNames -> Inherited];

	If[MemberQ[currentStyles, style],
		(* Remove all occurences of `style` from `currentStyles`. *)
		SetOptions[cell, StyleNames -> DeleteCases[currentStyles, style]];
	,
		(* Note:
			Place the newly added style at the end, to ensure it always
			gets the last say on what the styling of the cell is. E.g. if
			the style being added is "ConnorGray/Excluded", then the cell will
			always have a red background, even if it is also e.g. a "Program"
			cell (which normally have gray backgrounds).
		*)
		SetOptions[cell, StyleNames -> Append[currentStyles, style]];
	];
]

SetFallthroughError[toggleCellStyle]

(*====================================*)


End[]

EndPackage[]