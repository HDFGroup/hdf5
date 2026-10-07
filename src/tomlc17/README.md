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
| Vendored on   | 2026-10-07                                     |
| Release / tag | `main` at commit `fba134d7d` (after `R261003`)  |

Because tomlc17 does not include a version constant in its source, the
vendored files are identified by their SHA-256 checksums:

| File         | SHA-256                                                            |
|--------------|--------------------------------------------------------------------|
| `tomlc17.c`  | `40de13f674959b120606ccee5064d07d7a7c5c0b0affe69d6b10b12248ca950e` |
| `tomlc17.h`  | `29c0909092f4b23c7edce705167064f4a90548db5334b4aa2cd95d7ad801adb9` |

## HDF5-local modifications

None.  Both files are byte-for-byte identical to upstream commit
`fba134d7d`, and are intentionally excluded from the HDF5 clang-format pass (see
`.github/workflows/clang-format-check.yml` and `bin/format_source`) so that
future upstream updates can be dropped in without any re-formatting step.
Symbols are renamed for static builds by force-including `h5_toml_prefix.h`
from `src/CMakeLists.txt`, not by editing the sources.

This commit is untagged; it is used instead of `R261003` for the
`page_create()` UBSan fix (<https://github.com/cktan/tomlc17/issues/56>).
Replace it with the next tagged release.

The vendored copy contains the two `scan_float()` fixes earlier copies carried
as local changes, for subnormal float literals:
<https://github.com/cktan/tomlc17/issues/48> (commit `64a063b86`) and their
rejection when denormals-are-zero is set, e.g. under Intel icx or
`-ffast-math` (<https://github.com/cktan/tomlc17/issues/49>, commit
`4d2a53f02`).

## Known limitations

### Subnormal literals where `strtod()` flushes them

On platforms whose libc flushes inside `strtod()` under ambient FTZ/DAZ --
observed on Windows Intel oneAPI and MSYS2 clangarm64, where the smallest
subnormal, `4.9406564584124654e-324`, converts to `0.0` -- the parsed value
truly is zero and tomlc17 rejects the literal as an underflow. HDF5 handles that case outside the
parser: `tfilter2` probes the platform's `strtod`/`snprintf` round-trip at
run time and skips only the two true-subnormal exponents when the probe shows
the libc does not preserve them (see `test/tfilter2.c`). Filter parameters
that are exact subnormal doubles (magnitude below ~2.2e-308) are not a
realistic compression level, tolerance, or scale factor, so this is a
documented limitation rather than a gap to close in the parser.

### Float parsing depends on `LC_NUMERIC`

`scan_float()` converts float literals with `strtod()`, which follows the
calling thread's `LC_NUMERIC` locale.  If an application switches to a locale
whose decimal point is not '.', such as `de_DE`, every float literal fails to
parse.  Reported as <https://github.com/cktan/tomlc17/issues/57>; not
patched here, so that the files stay identical to a tagged release.

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
3. Record the new SHA-256 checksums, tag name and commit in the table above.
4. Update the "Vendored on" date.
5. Check whether the new tag fixes anything under "Known limitations" and
   update that section.
6. Check whether the new tag adds any external symbols (`nm -g` on the
   object), and add them to `h5_toml_prefix.h`.
7. Do **not** run clang-format on these files.
8. Run the HDF5 test suite (`ctest -R tfilter2`) to verify compatibility.
