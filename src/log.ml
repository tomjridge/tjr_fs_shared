let src = Logs.Src.create "tjr_fs_shared" ~doc:"log for tjr_fs_shared"

module Log = (val Logs.src_log src : Logs.LOG)

include Log

