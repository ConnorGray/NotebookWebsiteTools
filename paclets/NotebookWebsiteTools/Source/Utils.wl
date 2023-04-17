BeginPackage["ConnorGray`NotebookWebsiteTools`Utils`"]

Needs["GeneralUtilities`" -> "GU`"]

RelativePath

ConvertToString

UniqueContext::usage = "UniqueContext[stem] generates a unique context name beginning with stem."

NotebookCells::usage = "NotebookCells[notebook] returns a list of all top-level cells in notebook, after flattening out cell groups."

HTMLEscape::usage = "HTMLEscape[string] escapes string so that it can be embedded in HTML as regular text."

GU`SetUsage[CellDataQ, "CellDataQ[expr$] returns True if expr$ is a valid cell content type, typically a string, TextData[$$] or BoxData[$$].
* Box expressions are not considered valid cell data, as they cannot validly appear as the first\
  argument of Cell[$$].
"]

GU`SetUsage[CreateCacheDirectory, "
CreateCacheDirectory[path$] creates or confirms the existence of a cache directory at the specified path$.

This function is intended to aid in the creation of a directory containing
cached or built content that is recoverable or not important if deleted, while
being resistent to creating a cache directory at a location that contains
existing files that may not be easily recoverable if deleted.

This function follows the CACHEDIR.TAG (https://bford.info/cachedir/) specification.

If no file or directory exists at path$, a new directory will be created that
contains a valid CACHEDIR.TAG file.

If path$ is a non-empty directory containing a valid CACHEDIR.TAG file, path$
will be returned.

If path$ is an empty directory, a valid CACHEDIR.TAG file will be created and
path$ will be returned.

If path$ is an existing file, or non-empty directory that does not contain a
CACHEDIR.TAG value, an error will be returned.

If DeleteContents \[Rule] True is set and path$ is a valid cache directory, all
files other than CACHEDIR.TAG will be deleted. If path$ did not exist or was an
empty directory, it will be initialized as valid cache directory containing
only a CACHEDIR.TAG file.
"]

GU`SetUsage[HTMLFragmentQ, "
HTMLFragmentQ[expr$] returns True if expr$ can validlty appear as an element in a list that is a 3rd argument of XMLElement."]

Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`ErrorUtils`"]

(*========================================================*)

AddUnmatchedArgumentsHandler[CreateCacheDirectory]

Options[CreateCacheDirectory] = {
	DeleteContents -> False
}

(* TODO: Implement a scheme for locking cache directories to synchronize access
	to them? *)
CreateCacheDirectory[
	path0 : _?StringQ | File[_?StringQ],
	OptionsPattern[]
] := CatchRaised @ WrapRaised[
	"Unable to create cache directory at ``",
	InputForm[path0]
] @ Module[{
	path = RaiseConfirm @ ExpandFileName[Replace[path0, File[s_?StringQ] :> s]],
	result
},
	result = createCacheDirectory[path];

	(* Should throw an error or return `path`. *)
	RaiseAssert[result === path];

	If[TrueQ[OptionValue[DeleteContents]],
		deleteCacheDirectoryContentsUnchecked[path];
	];

	path
]

(*------------------------------------*)

AddUnmatchedArgumentsHandler[createCacheDirectory]

createCacheDirectory[
	path_?StringQ
] := Module[{
	tagPath = FileNameJoin[{path, "CACHEDIR.TAG"}]
},
	Replace[FileType[path], {
		None :> (
			(* The specified directory doesn't exist, so initialize a new
				empty cache directory. *)
			RaiseConfirm @ CreateDirectory[path];
			RaiseConfirm @ Export[tagPath, $tagFileContents, "Text"];

			Return[path, Module];
		),
		File :> RaiseError["Specified path is a non-directory file."],
		Directory :> Null,
		other_ :> RaiseError["Unexpected file type: ``", InputForm[other]]
	}];

	(*--------------------------------------*)
	(* If the tag file exists, validate it. *)
	(*--------------------------------------*)

	If[FileExistsQ[tagPath],
		(* TODO: Check that this file isn't a symbolic link either. *)
		RaiseAssert[FileType[tagPath] === File];

		Replace[ReadString[tagPath], {
			contents_?StringQ :> (
				If[!validTagFileContentsQ[contents],
					RaiseError["Existing CACHEDIR.TAG file contains invalid header."]
				];

				(* The tag file exists and is valid, so this is a valid cache
					directory. *)
				Return[path, Module];
			),
			other_ :> RaiseError["Error reading existing CACHEDIR.TAG file contents: ``", other]
		}];
	];

	(*-----------------------------------------------------------------*)
	(* `path` is an existing directory but doesn't contain a tag file. *)
	(*-----------------------------------------------------------------*)

	If[FileNames[All, path] =!= {},
		(* This directory isn't empty and it doesn't contain a tag file. It
			might be a directory containing non-recoverable user files! Don't
			risk deleting its contents. *)
		(* NOTE:
			This is the key user-facing error message of CreateCacheDirectory
			and a big part of the utility of this function. *)
		RaiseError[StringJoin[
			"Specified path is an existing non-empty directory without a CACHEDIR.TAG file. ",
			"This operation may succeed if the existing directory contents are moved or deleted manually.",
			"\n\nNOTE: This file path is being used as a location to store cached data or generated files. ",
			"Data loss WILL occur if you store non-recoverable files in this location."
		]]
	];

	(*----------------------------------------*)
	(* `path` is an existing empty directory. *)
	(*----------------------------------------*)

	RaiseConfirm @ Put[$tagFileContents, tagPath];
	RaiseConfirm @ Export[tagPath, $tagFileContents, "Text"];

	path
]

(*------------------------------------*)

AddUnmatchedArgumentsHandler[deleteCacheDirectoryContentsUnchecked]

deleteCacheDirectoryContentsUnchecked[path_?DirectoryQ] := Module[{
	files = FileNames[All, path]
},
	files = Select[files, FileNameTake[#] =!= "CACHEDIR.TAG" &];

	(*
		Delete all existing contents of the cache directory, except the
		CACHEDIR.TAG file.
	*)
	WrapRaised["Error deleting existing contents of cache directory"][
		Scan[
			entry |-> Replace[FileType[entry], {
				File :> DeleteFile[entry],
				Directory :> DeleteDirectory[entry, DeleteContents -> True]
			}],
			files
		];
	];
]

(*------------------------------------*)

(* TODO: Option to override the function name in the 'created by' message below. *)
$tagFileContents = "\
Signature: 8a477f597d28d172789f06886806bc55
# This file is a cache directory tag created by CreateCacheDirectory.
# For information about cache directory tags, see:
#	http://www.brynosaurus.com/cachedir/
"

(*====================================*)

AddUnmatchedArgumentsHandler[validTagFileContentsQ]

validTagFileContentsQ[contents_?StringQ] :=
	StringStartsQ[contents, "Signature: 8a477f597d28d172789f06886806bc55"]

(*========================================================*)

(*
	Return the portion of `path` which is relative to `root`. If `path` is not relative to
	`root`, a failure is returned.

	TODO: What should this return if `root` and `path` are the same?
*)
RelativePath[
	root_?StringQ,
	path_?StringQ
] := Module[{
	rootParts = FileNameSplit[root],
	pathParts = FileNameSplit[path]
},
	(* Ensure that `path` is a subdirectory of `root`. If it is, return the component
	of the path which is relative to `root`. *)
	If[! MatchQ[pathParts, {Sequence @@ rootParts, ___}],
		$Failed,
		FileNameJoin[pathParts[[Length[rootParts] + 1 ;;]]]
	]
]

(*========================================================*)

ConvertToString[expr_] := Replace[expr, {
	string_?StringQ /; StringMatchQ[string, "\"" ~~ ___ ~~ "\""] :> ToExpression[string],
	string_?StringQ :> string,
	items_?ListQ :> StringJoin[ConvertToString /@ items],
	TextData[content_] :> ConvertToString[content],
	StyleBox[content_, ___] :> ConvertToString[content],
	BoxData[content_] :> ConvertToString[content],
	RowBox[items_?ListQ] :> StringJoin[ConvertToString /@ items],
	TemplateBox[items_?ListQ, "RowDefault"] :> StringJoin[ConvertToString /@ items],
	ButtonBox[content_, ___] :> ConvertToString[content],
	other_ :> RaiseError["no rule to convert form to string: ``", InputForm[other]]
}]

AddUnmatchedArgumentsHandler[ConvertToString]

(*========================================================*)

UniqueContext[stem_?StringQ] := Module[{ctx},
	If[!TrueQ[Internal`SymbolNameQ[stem]],
		Return[Failure["UniqueContext", <|
			"MessageTemplate" -> "Invalid non-Symbol stem: ``"|>,
			"MessageParameters" -> {stem}
		]];
	];

	ctx = stem <> "$" <> ToString[$ModuleNumber] <> "`";
	$ModuleNumber += 1;

	ctx
]

(*========================================================*)

NotebookCells[
	Notebook[cells:{___Cell}, ___?OptionQ]
] := flattenCellGroups[cells]

(*------------------------------------*)

flattenCellGroups[cells: {___Cell}] :=
	Flatten @ Map[
		Replace[{
			Cell[
				CellGroupData[groupCells:{___Cell}, ___],
				___
			] :> (
				flattenCellGroups[groupCells]
			),
			Cell[group_CellGroupData, ___] :> (
				RaiseError["Unexpected CellGroupData structure: ``", group]
			),
			normalCell_Cell :> normalCell,
			other_ :> RaiseError["Unexpected notebook structure: ``", InputForm[other]]
		}],
		cells
	]

AddUnmatchedArgumentsHandler[NotebookCells]

(*========================================================*)

(* NOTE: This function is required because exporting an XMLElement[..] using the
	"XML" format escapes '<' and '>' characters in `text`, but exporting as
	using the "HTMLFragment" format does not escape those characters. *)
HTMLEscape[text_?StringQ] := StringReplace[
	ExportString[XMLElement["Text", {}, {text}], "XML"],
	StartOfString ~~ "<Text>" ~~ content___ ~~ "</Text>" ~~ EndOfString :> content
]

AddUnmatchedArgumentsHandler[HTMLEscape]

(*========================================================*)

CellDataQ[expr_] :=
	(* TODO: More obscure or deprecated forms. e.g. GraphicsData or OutputFormData? *)
	MatchQ[expr, Alternatives[
		_?StringQ,
		TextData[_],
		BoxData[_]
	]]

AddUnmatchedArgumentsHandler[CellDataQ]

(*========================================================*)

(* TODO: Include XML`RawXML["..."] here? *)
HTMLFragmentQ[expr_] :=
	MatchQ[expr, _?StringQ | _XMLElement | Nothing | Splice[{___?HTMLFragmentQ}]]

(*========================================================*)

End[]

EndPackage[]