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
| Vendored on   | 2026-05-08                                     |
| Release / tag | `R20260501`                                    |

Because tomlc17 does not include a version constant in its source, the
vendored files are identified by their SHA-256 checksums:

| File         | SHA-256                                                            |
|--------------|--------------------------------------------------------------------|
| `tomlc17.c`  | `e5d61c4960a4ef7254abb4077ffaefd2ba8935459e52314ea67537b5e340ddd5` |
| `tomlc17.h`  | `9f6cae985cde199a64e0f63d4b395b2738455f9b62d501bb4385ca3a85e34728` |

Use these hashes to identify the exact upstream commit.  They are the
checksums of the **pristine** upstream files; `tomlc17.c` as it sits in this
directory carries one local change (see below) and hashes to
`8b4898b2e23046de778e5e989720b8d714bb105f337efce73fb56a827bb7a9f7`.

## HDF5-local modifications

**One**, in `tomlc17.c` `scan_float()`.  They are otherwise the exact upstream
sources, and are intentionally excluded from the HDF5 clang-format pass (see
`.github/workflows/clang-format-check.yml` and `bin/format_source`) so that
future upstream updates can be dropped in without any re-formatting step.

### `scan_float()`: `ERANGE` alone is not a parse failure

Upstream rejects any `strtod()` that sets `errno`:

```c
if (errno || *q || q == buffer) {
  return SETERROR(sp->ebuf, lineno, "error parsing float");
}
```

C11 7.22.1.3 leaves it implementation-defined whether `strtod` sets `ERANGE`
when the result underflows, and glibc sets it for a tiny *inexact* result even
though the conversion succeeded and returned the correctly rounded value.  The
effect is that every TOML float literal that rounds to a subnormal double is
rejected as a syntax error -- `x = 5e-324` as surely as
`x = 2.2250738585072011e-308`.  Python's `tomllib` accepts all of them.

For HDF5 this reached the filter configuration API through canonicalization:
`H5Z__rewrite_hexfloats()` rewrites a hex-float literal to `%.17e` decimal, and
the decimal spelling of a subnormal is inexact even when the hex spelling was
exact, so `rate = 0x1p-1074` became a parse error on the way to disk.

The local change accepts `ERANGE` when the result is finite and nonzero, and
still rejects overflow to `+/-HUGE_VAL` and underflow all the way to zero:

```c
if ((errno && !(errno == ERANGE && fp64 != 0.0 && isfinite(fp64))) || *q ||
    q == buffer) {
```

`<math.h>` is already included upstream, so `isfinite` needs no new header.
The `isfinite` term is load-bearing: `HUGE_VAL` is nonzero, so without it
overflow would start being accepted.

Covered by `canon-10` in `test/tfilter2.c`, which asserts value transparency
across the hex-to-decimal rewrite at exact powers of two from 2^-1074 to
2^1023.

Reported upstream as <https://github.com/cktan/tomlc17/issues/48>.  Drop this
delta once that is fixed and a release carrying the fix is vendored.

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
4a. Re-apply the local `scan_float()` change below, unless upstream has fixed
   it, and record the new post-patch checksum.
5. Do **not** run clang-format on these files.
6. Run the HDF5 test suite (`ctest -R tfilter2`) to verify compatibility.
