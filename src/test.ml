(** Support for basic testing, controlled by optcomp *)

[%%import "config.ml"]

[%%if TEST_ASSERT__ENABLED]

let _ = assert(Printf.printf "%s: assertions are enabled\n%!" "Tjr_fs_shared.Test";true)

let assert_ (f:unit->unit) = f ()

[%%else]

let assert_ (f:unit->unit) = ()

[%%endif]


[%%if TEST_CHECK__ENABLED]

let _ = assert(Printf.printf "%s: checks are enabled\n%!" "Tjr_fs_shared.Test";true)

let check (f:unit->unit) = f ()

[%%else]

let check (f:unit->unit) = ()

[%%endif]

