(lang dune 2.0)
(name tjr_fs_shared)

;; for opam
(authors              "Tom Ridge <tom.j.ridge@googlemail.com>")
(maintainers          "Tom Ridge <tom.j.ridge@googlemail.com>")
(source               (github tomjridge/tjr_fs_shared))
(homepage             https://github.com/tomjridge/tjr_fs_shared)
(documentation        https://tomjridge.github.io/tjr_fs_shared)
(generate_opam_files  true)


(package 
 (name tjr_fs_shared)
 (synopsis "Shared definitions for filesystem work (lib)")
 (depends 
   tjr_lib
   tjr_monad
   tjr_profile
   lwt ; for lwt block device
   lru ; for caching
   ke ; for functional queues
   alcotest ; for testing
))


(package 
 (name tjr_fs_shared_examples)
 (synopsis "Shared definitions for filesystem work (examples lib)")
 (depends tjr_fs_shared))

; (package 
;  (name tjr_fs_shared_test)
;  (synopsis "Shared definitions for filesystem work (test lib and bin)")
;  (depends tjr_fs_shared))

