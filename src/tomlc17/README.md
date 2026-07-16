# tomlc17 — Vendored TOML Parser

## Overview

This directory contains a vendored copy of [tomlc17](https://github.com/cktan/tomlc17),
a lightweight TOML 1.0 parser written in C by CK Tan.  Only the two files
needed by the HDF5 filter configuration API are included.

## Upstream details

| Field         | Value                                          |
|---------------|------------------------------------------------|
| Upstream URL  | https://github.com/cktan/tomlc17               |
| License       | MIT (see `LICENSE` in this directory)          |
| Vendored on   | 2026-07-15                                     |
| Release / tag | `R260618`                                      |

Because tomlc17 does not include a version constant in its source, the
vendored files are identified by their SHA-256 checksums:

| File         | SHA-256                                                            |
|--------------|--------------------------------------------------------------------|
| `tomlc17.c`  | `f28f3742808505b5c55189b72cf62705e824276eca6533b36e8751a8433482aa` |
| `tomlc17.h`  | `eef9a891b93fc6235a9552db16286cefc684be8fae49018bedf1c6ab9ca55b87` |

Use these hashes to identify the exact upstream commit.

## HDF5-local modifications

**None.** The files in this directory are the exact upstream sources with no
local modifications.  They are intentionally excluded from the HDF5
clang-format pass (see `.github/workflows/clang-format-check.yml` and
`bin/format_source`) so that future upstream updates can be dropped in
without any re-formatting step.

Note: hex-float literal support (C99 `%a` output from `get_config` callbacks)
is handled in `src/H5Zconfig.c` via a pre-processing step that rewrites
hex-float tokens to their decimal equivalents before the parameter string is
handed to tomlc17.  This keeps these vendored sources unmodified.

## Files

| File         | Description              |
|--------------|--------------------------|
| `tomlc17.h`  | Public API header        |
| `tomlc17.c`  | Parser implementation    |
| `LICENSE`    | MIT license (CK Tan)     |

## Updating the vendored copy

**Only use tagged releases** from the upstream repository.

1. Download `tomlc17.h` and `tomlc17.c` from the desired upstream tag.
2. Copy them into this directory, replacing the existing files.
3. Record the new SHA-256 checksums and tag name in the table above.
4. Update the "Vendored on" date.
5. Do **not** run clang-format on these files.
6. Run the HDF5 test suite (`ctest -R tfilter2`) to verify compatibility.
