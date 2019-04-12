# tjr_fs_shared: shared definitions for filesystem work

## Introduction

This library includes basic definitions that are common to much of the filesystem work.

## Installation

Usually this library is installed as part of the tjr_kv and imp_fs builds. To install directly from source, first install the dependencies (see below), then use `make` in this directory.

## Documentation

Individual .ml files have their own documentation in ocamldoc.

Html version of the ocamldoc can be found in the docs/ directory.

The html can be browsed online at <https://tomjridge.github.io/tjr_fs_shared/index.html>

There is a summary of the main types in module Tjr_fs_shared_doc,
available
[locally](docs/tjr_fs_shared/Tjr_fs_shared/Tjr_fs_shared_doc/index.html)
and
[online](https://tomjridge.github.io/tjr_fs_shared/tjr_fs_shared/Tjr_fs_shared/Tjr_fs_shared_doc/index.html).



## Dependencies 



| Dependency                                          | Comment                                                  |
| --------------------------------------------------- | -------------------------------------------------------- |
| bin_prot; ppx_bin_prot; yojson; ppx_deriving_yojson |                                                          |
| tjr_lib                                             |                                                          |
| tjr_monad                                           | So we don't have to explicit include in later libraries. |


