(** Support for basic testing *)

[%%import "config.ml"]

[%%if TEST_ASSERT__ENABLED]

let _ = assert(Printf.printf "%s: assertions are enabled\n%!" __LOC__;true)

let assert_ (f:unit->unit) = f ()

[%%else]

let assert_ f = ()

[%%endif]


[%%if TEST_CHECK__ENABLED]

let _ = assert(Printf.printf "%s: checks are enabled\n%!" __LOC__;true)

let check (f:unit->unit) = f ()

[%%else]

let check (f:unit->unit) = ()

[%%endif]

