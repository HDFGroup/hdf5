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
directory carries two local changes (see below) and hashes to
`f3b41671ae5a99fc0d12807ce6b998dcb7139827333b29f668e9d6cdd9682d19`.

## HDF5-local modifications

**Two**, both in `tomlc17.c` `scan_float()`.  They are otherwise the exact
upstream sources, and are intentionally excluded from the HDF5 clang-format
pass (see `.github/workflows/clang-format-check.yml` and `bin/format_source`)
so that future upstream updates can be dropped in without any re-formatting
step.

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
`H5Z__rewrite_hexfloats()` rewrites hex-float literals to the shortest
bit-exact decimal (`H5Z__format_double_canonical()`), and the decimal
spelling of a subnormal is inexact even when the hex spelling was exact, so
`rate = 0x1p-1074` became a parse error on the way to disk.

Reported as <https://github.com/cktan/tomlc17/issues/48> and fixed upstream the
same week.  The fix landed two days after `R260821` was tagged, so it is in
`main` but not yet in any release; this directory tracks tagged releases only,
hence the one-commit delta.  **Drop it at the next update**: once a tag at or
after `64a063b86` exists, replacing these files with that tag leaves no local
change at all.

Covered by `canon-10` in `test/tfilter2.c`, which asserts value transparency
across the hex-to-decimal rewrite at exact powers of two from 2^-1074 to
2^1023.

### `scan_float()`: the fast-math fix, also ahead of a release

A second, independent change to the same `is_ok_subnormal` line, which decided
whether to forgive `ERANGE` using `fp64 != 0.0` and `isfinite(fp64)` -- two
floating-point operations a fast-math build is free to reinterpret. Intel's
icc/icx use `-fp-model=fast` by default at `-O2` and above (unless
`-fp-model=precise`/`-fp-model=strict` is given), under which the compiler may
assume no operand is subnormal and every operand is finite. `is_ok_subnormal`
then comes out false for a correctly-rounded subnormal, and the same class of
literal the `64a063b86` fix above was meant to accept (e.g. `x = 5e-324`) is
rejected again. This is why it surfaced as an Intel-only CI failure
(`tfilter2`'s `canon-10`/`test_config_canonicalization`) rather than on GCC or
Clang.

The flag responsible is DAZ (denormals-are-zero, `MXCSR` bit 6), not FTZ.
FTZ acts on the *results* of SSE arithmetic; DAZ acts on its *inputs*, which
is what makes a comparison read a subnormal operand as `0.0`. Measured
against the parser as of `64a063b86`, with the register read back to confirm
each setting took effect:

```
default   (FTZ=0 DAZ=0)   x = 5e-324 -> ACCEPT
FTZ only  (FTZ=1 DAZ=0)   x = 5e-324 -> ACCEPT
FTZ+DAZ   (FTZ=1 DAZ=1)   x = 5e-324 -> REJECT
```

The toolchains named above enable both together, which is why the original
diagnosis pointed at the right behaviour under the wrong name. Note that a
build need not use fast-math itself to be affected: DAZ is process-wide
state, so anything that sets it -- including a shared library elsewhere in
the process built with `-ffast-math` -- puts the parser in this mode.

The fix decides on the raw bit pattern, which no FP mode or fast-math
assumption can alter: mask off the sign bit, then require the magnitude to be
nonzero (rejecting an underflow to +-0.0) and below the infinity/NaN exponent
(rejecting an overflow). Testing the exponent bits directly also replaces
`isfinite()`, which gcc and clang fold to 1 under `-ffast-math` and
`-ffinite-math-only`.

Reported as <https://github.com/cktan/tomlc17/issues/49>, fix proposed as
<https://github.com/cktan/tomlc17/pull/50>. Not yet merged upstream at the time
of this vendoring, hence the second delta. **Drop it at the next update**:
once a tag containing that fix exists, replacing these files with that tag
leaves no local change for this issue.

Covered by the same `canon-10` test above; the failure is otherwise silent on
compilers that do not default to fast-math, so it will not reproduce locally on
a typical GCC/Clang build.

### What this patch does *not* fix

It only helps where `strtod()` itself returned a genuine subnormal and the
parser then misjudged it. On platforms whose libc flushes inside `strtod()`
under ambient FTZ/DAZ -- observed on Windows Intel oneAPI and MSYS2
clangarm64, where `0x1p-1074` round-trips to a literal `0.0` -- the bits truly
are zero, no bit-pattern test can recover the value, and tomlc17 still rejects
the literal. HDF5 handles that case outside the parser: `tfilter2` probes the
platform's `strtod`/`snprintf` round-trip at run time and skips only the two
true-subnormal exponents when the probe shows the libc does not preserve them
(see `test/tfilter2.c`). Filter parameters that are exact subnormal doubles
(magnitude below ~2.2e-308) are not a realistic compression level, tolerance,
or scale factor, so this is a documented limitation rather than a gap to close
in the parser.

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
   (grep for `is_ok_subnormal`).  If it does, drop that local change and
   delete its section above.  If not, re-apply it verbatim.
4b. Check whether the new tag already contains the fast-math fix from
   <https://github.com/cktan/tomlc17/pull/50> (grep for `fp64_mag`).  If it
   does, drop that local change too and delete its section above.  If not,
   re-apply it verbatim.
4c. Record the resulting file's new post-patch checksum in the table above.
5. Do **not** run clang-format on these files.
6. Run the HDF5 test suite (`ctest -R tfilter2`) to verify compatibility.
