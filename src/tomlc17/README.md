# tomlc17 — Vendored TOML Parser

## Overview

This directory contains a vendored copy of [tomlc17](https://github.com/cktan/tomlc17),
a lightweight TOML 1.0 parser written in C by CK Tan.  Only the two upstream
files needed by the HDF5 filter configuration API are included, plus the
HDF5-authored files listed under "Files".

## Upstream details

| Field         | Value                                          |
|---------------|------------------------------------------------|
| Upstream URL  | https://github.com/cktan/tomlc17               |
| License       | MIT (see `LICENSE` in this directory)          |
| Vendored on   | 2026-11-12                                     |
| Release / tag | `R261109` (commit `aa4ca1bc5`)                 |

Because tomlc17 does not include a version constant in its source, the
vendored files are identified by their SHA-256 checksums:

| File         | SHA-256                                                            |
|--------------|--------------------------------------------------------------------|
| `tomlc17.c`  | `7476923b6c9b8f0858b82b63ed6107f57448882983390e70c31327f7d3e2a362` |
| `tomlc17.h`  | `29c0909092f4b23c7edce705167064f4a90548db5334b4aa2cd95d7ad801adb9` |

## HDF5-local modifications

None.  Both files are byte-for-byte identical to upstream tag
`R261109`, and are intentionally excluded from the HDF5 clang-format pass (see
`.github/workflows/clang-format-check.yml` and `bin/format_source`) so that
future upstream updates can be dropped in without any re-formatting step.
Symbols are renamed in all builds by compiling `tomlc17.c` through
`h5_tomlc17.c`, which includes `h5_toml_prefix.h` first, not by editing the
sources.

`R261109` includes the upstream `scan_float()` fixes for subnormal float
literals (<https://github.com/cktan/tomlc17/issues/48>, commit `64a063b86`)
and for their rejection when denormals-are-zero is set, e.g. under Intel icx
or `-ffast-math` (<https://github.com/cktan/tomlc17/issues/49>, commit
`4d2a53f02`).

## Known limitations

### Subnormal literals where `strtod()` flushes them

On platforms whose libc flushes inside `strtod()` under ambient FTZ/DAZ --
observed on Windows Intel oneAPI and MSYS2 clangarm64, where the smallest
subnormal, `4.9406564584124654e-324`, converts to `0.0` -- the parsed value
truly is zero and tomlc17 rejects the literal as an underflow.  `tfilter2`
does not test subnormal literals.  Filter parameters that are exact subnormal
doubles (magnitude below ~2.2e-308) are not a realistic compression level,
tolerance, or scale factor, so this is a documented limitation rather than a
gap to close in the parser.

### Float parsing depends on `LC_NUMERIC`

`scan_float()` converts float literals with `strtod()`, which follows the
calling thread's `LC_NUMERIC` locale.  Since `R261109`
(<https://github.com/cktan/tomlc17/issues/57>), tomlc17 replaces the '.' with
the decimal point that `localeconv()` reports before calling `strtod()`, so a
locale such as `de_DE` no longer makes every float literal fail to parse.
HDF5 still parses in the "C" numeric locale for the calling thread, through
`H5Z__toml_parse_c_locale()` in `src/H5Zconfig.c` (`uselocale()` on POSIX, a
per-thread locale on Windows): that makes the upstream substitution a no-op, so
the result does not depend on the global locale, which another thread can
change.  Platforms with neither facility rely on the upstream substitution.

## Files

| File               | Description                                              |
|--------------------|----------------------------------------------------------|
| `tomlc17.h`        | Public API header (upstream)                             |
| `tomlc17.c`        | Parser implementation (upstream)                         |
| `LICENSE`          | MIT license (CK Tan)                                     |
| `h5_toml_prefix.h` | HDF5: renames the tomlc17 symbols                        |
| `h5_tomlc17.c`     | HDF5: compiles `tomlc17.c` with `h5_toml_prefix.h` first |
| `README.md`        | This file                                                |

## Updating the vendored copy

**Only use tagged releases** from the upstream repository.

1. Download `tomlc17.h` and `tomlc17.c` from the desired upstream tag.
2. Copy them into this directory, replacing the existing files.
3. Record the new SHA-256 checksums, tag name and commit in the table above.
4. Update the "Vendored on" date.
5. Check whether the new tag fixes anything under "Known limitations" and
   update that section.
6. Check whether the new tag adds any external symbols (`nm -g` on the
   object), and add them to `h5_toml_prefix.h`.
7. Do **not** run clang-format on these files.
8. Run the HDF5 test suite (`ctest -R tfilter2`) to verify compatibility.
