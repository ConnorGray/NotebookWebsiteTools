BeginPackage["ConnorGray`CacheUtils`"]

Needs["GeneralUtilities`" -> None]

GeneralUtilities`SetUsage[CreateCache, ""]

GeneralUtilities`SetUsage[KeyPath, "
KeyPath[{components$$}] represents a lookup path
"]

GeneralUtilities`SetUsage[CacheSpecifier, "
CacheSpecifier[id$] represents a handle to a CacheObject.
"]

GeneralUtilities`SetUsage[CacheObject, "
CacheObject[values$, handlers$] represents an object that uses handlers$ to compute values and caches results in values$.
"]

CacheObject
GetCacheValue
SetCacheHandler
InvalidateCache

KeyPathQ
CacheObjectQ

Begin["`Private`"]

Needs["ConnorGray`NotebookWebsiteTools`Errors`"]

(*========================================================*)

$caches = <||>

(*====================================*)



SetFallthroughError[getCacheObject]

getCacheObject[CacheSpecifier[uuid_?StringQ]] := Module[{obj},
	RaiseAssert[AssociationQ[$caches]];

	obj = RaiseConfirm @ Lookup[$caches, uuid];

	obj
]

(*====================================*)

CacheObject[spec_CacheSpecifier] := Handle[_Failure] @ getCacheObject[spec]

(*========================================================*)

SetFallthroughError[CreateCache]

CreateCache[] := Module[{
	uuid = CreateUUID[],
	cache
},
	cache = CacheObject[<||>, {}];

	AssociateTo[$caches, uuid -> cache];

	CacheSpecifier[uuid]
]

(*========================================================*)

SetFallthroughError[GetCacheValue]

GetCacheValue[
	cache_CacheSpecifier,
	keyPath_List
] := GetCacheValue[cache, KeyPath[keyPath]]

GetCacheValue[
	cache_CacheSpecifier,
	key_?KeyPathQ
] := WithCacheObject[cache, obj0 |-> Module[{
	obj = obj0,
	$missing,
	result,
	values, handlers
},
	{values, handlers} = Replace[getCacheObject[cache], {
		CacheObject[values_?AssociationQ, handlers_?ListQ] :> {values, handlers},
		other_ :> Raise[NotebookWebsiteError, "Unexpected getCacheObject result: ``", InputForm[other]]
	}];

	result = Lookup[values, key, $missing];

	If[result =!= $missing,
		(* Print[" HIT: ", key]; *)
		Return[{CacheObject[values, handlers], result}, Module];
	];

	(* Print["MISS: ", key]; *)

	result = Replace[key, Append[handlers, _ -> $missing]];

	If[result === $missing,
		Return[
			{
				CacheObject[values, handlers],
				Missing["CacheMissingHandler", <| "Cache" -> cache, "KeyPath" -> key |>]
			},
			Module
		];
	];

	values[key] = result;

	{CacheObject[values, handlers], result}
]]

(*========================================================*)

SetFallthroughError[InvalidateCache]

InvalidateCache[
	cache:CacheSpecifier[uuid_?StringQ],
	key_?KeyPathQ
] := Module[{
	obj = getCacheObject[cache],
	values, handlers
},
	{values, handlers} = Replace[getCacheObject[cache], {
		CacheObject[values_?AssociationQ, handlers_?ListQ] :> {values, handlers},
		other_ :> Raise[NotebookWebsiteError, "Unexpected getCacheObject result: ``", InputForm[other]]
	}];

	values = AssociationMap[
		rule |-> If[MatchQ[rule[[1]], key],
			Nothing,
			rule
		],
		values
	];

	$caches[uuid] = CacheObject[values, handlers];

	cache
]

(*========================================================*)

SetFallthroughError[SetCacheHandler]

SetCacheHandler[
	spec:CacheSpecifier[uuid_?StringQ],
	handler:((Rule|RuleDelayed)[_?KeyPathQ, _])
] := Module[{
	obj = getCacheObject[spec]
},
	obj = Replace[obj, {
		CacheObject[values_?AssociationQ, handlers_?ListQ] :> (
			CacheObject[values, Append[handlers, handler]]
		),
		other_ :> Raise[NotebookWebsiteError, "Unexpected getCacheObject result: ``", InputForm[other]]
	}];

	RaiseAssert[
		CacheObjectQ[obj],
		"Invalid CacheObject: ``", InputForm[obj]
	];

	$caches[uuid] = obj;

	spec
]

(*========================================================*)

KeyPathQ[KeyPath[{___}]] := True

KeyPathQ[___] := False

(*====================================*)

CacheObjectQ[CacheObject[_?AssociationQ, _?ListQ]] := True

CacheObjectQ[___] := False

(*========================================================*)

SetFallthroughError[WithCacheObject]

WithCacheObject[spec:CacheSpecifier[uuid_?StringQ], func_] := Module[{
	obj,
	result
},
	obj = getCacheObject[spec];

	RaiseAssert[CacheObjectQ[obj]];

	result = func[obj];

	If[!MatchQ[result, {_?CacheObjectQ, _}],
		Raise[
			NotebookWebsiteError,
			"Invalid result returned from WithCacheObject callback: ``",
			InputForm[result]
		];
	];

	$caches[uuid] = result[[1]];

	result[[2]]
]

(*========================================================*)

End[]

EndPackage[]
