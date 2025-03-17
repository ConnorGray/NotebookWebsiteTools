Needs["ConnorGray`NotebookWebsiteTools`Utils`"]

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