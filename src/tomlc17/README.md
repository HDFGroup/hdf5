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
| Vendored on   | 2026-08-28                                     |
| Release / tag | `R260821`                                      |

Because tomlc17 does not include a version constant in its source, the
vendored files are identified by their SHA-256 checksums:

| File         | SHA-256                                                            |
|--------------|--------------------------------------------------------------------|
| `tomlc17.c`  | `8df630f202c102c52a66027d8aa537e9006ea146bde744d6561adf0401517bc1` |
| `tomlc17.h`  | `281708fa05b805c32c117fc6033b0f4248257fce3440b1ac3391853bfc8f8bb5` |

Use these hashes to identify the exact upstream commit.  They are the
checksums of the **pristine** upstream files; `tomlc17.c` as it sits in this
directory carries one local change (see below) and hashes to
`89d3fe5ffe387360993c8b9df8f03eb13cd6df379b4482bcd039399f690a770f`.

## HDF5-local modifications

**One**, in `tomlc17.c` `scan_float()`.  They are otherwise the exact upstream
sources, and are intentionally excluded from the HDF5 clang-format pass (see
`.github/workflows/clang-format-check.yml` and `bin/format_source`) so that
future upstream updates can be dropped in without any re-formatting step.

### `scan_float()`: the subnormal fix, ahead of a release

`tomlc17.c` carries exactly one change from the `R260821` tag: upstream commit
`64a063b86`, *"Accept float literals that round to a subnormal double"*.  It is
applied **verbatim**, not reimplemented, so this file is byte-for-byte
identical to upstream `main`.

The bug: `scan_float()` rejected any `strtod()` that set `errno`.  C11 7.22.1.3
leaves it implementation-defined whether `strtod` sets `ERANGE` on underflow,
and glibc sets it for a tiny *inexact* result even though the conversion
succeeded and returned the correctly rounded value.  Every TOML float literal
rounding to a subnormal was therefore a syntax error -- `x = 5e-324` as surely
as `x = 2.2250738585072011e-308`.

For HDF5 this reached the filter configuration API through canonicalization:
`H5Z__rewrite_hexfloats()` rewrites hex-float literals to `%.16e` decimal, and
the decimal spelling of a subnormal is inexact even when the hex spelling was
exact, so `rate = 0x1p-1074` became a parse error on the way to disk.

Reported as <https://github.com/cktan/tomlc17/issues/48> and fixed upstream the
same week.  The fix landed two days after `R260821` was tagged, so it is in
`main` but not yet in any release; this directory tracks tagged releases only,
hence the one-commit delta.  **Drop it at the next update**: once a tag at or
after `64a063b86` exists, replacing these files with that tag leaves no local
change at all.

Covered by `canon-10` in `test/tfilter2.c`, which asserts value transparency
across the hex-to-decimal rewrite at exact powers of two from 2^-1074 to
2^1023.

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
4a. Check whether the new tag already contains upstream commit `64a063b86`
   (grep for `is_ok_subnormal`).  If it does, drop the local change entirely
   and delete the section below.  If not, re-apply it verbatim and record the
   new post-patch checksum.
5. Do **not** run clang-format on these files.
6. Run the HDF5 test suite (`ctest -R tfilter2`) to verify compatibility.
