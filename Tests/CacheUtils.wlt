Needs["ConnorGray`CacheUtils`"]

cache = CreateCache[];

VerificationTest[
	cache,
	CacheSpecifier[_?StringQ],
	SameTest -> MatchQ
]

VerificationTest[
	CacheObject[cache],
	CacheObject[<||>, {}]
]

VerificationTest[
	SetCacheHandler[
		cache,
		KeyPath[{
			"Squared",
			value_Integer
		}] :> value * value
	],
	cache
]

VerificationTest[
	CacheObject[cache],
	CacheObject[<||>, {
		KeyPath[{"Squared", value_Integer}] :> value * value
	}]
]

(* Test getting a value that is not yet cached. *)
VerificationTest[GetCacheValue[cache, KeyPath[{"Squared", 2}]], 4]

VerificationTest[
	GetCacheValue[cache, KeyPath[{"Squared", 3}]]
	,
	9
]

VerificationTest[
	CacheObject[cache],
	CacheObject[
		<|
			KeyPath[{"Squared", 2}] -> 4,
			KeyPath[{"Squared", 3}] -> 9
		|>,
		{
			KeyPath[{"Squared", value_Integer}] :> value * value
		}
	]
]

(* Test getting a value that is cached. *)
VerificationTest[GetCacheValue[cache, KeyPath[{"Squared", 2}]], 4]

VerificationTest[
	InvalidateCache[cache, KeyPath[{"Squared", _?EvenQ}]],
	cache
]

VerificationTest[
	CacheObject[cache],
	CacheObject[
		<|
			KeyPath[{"Squared", 3}] -> 9
		|>,
		{
			KeyPath[{"Squared", value_Integer}] :> value * value
		}
	]
]