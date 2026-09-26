# Ryu — Vendored Shortest-Round-Trip Float Formatter

## Overview

This directory contains a vendored copy of [Ryu](https://github.com/ulfjack/ryu),
Ulf Adams' reference C implementation of the algorithm from his PLDI 2018 paper
*"Ryū: fast float-to-string conversion"*.  Given an IEEE 754 double, Ryu
produces the **shortest decimal string that round-trips back to the identical
bit pattern**, computed directly rather than searched for.

HDF5 uses it in exactly one place: `H5Z__format_double_canonical()` in
`src/H5Zconfig.c`, which canonicalizes the C99 hex-float literals accepted by
the string-based filter configuration API into the decimal form persisted on
disk.  Writing that shortest-round-trip conversion by hand was considered and
rejected on <https://github.com/HDFGroup/hdf5/issues/6153>; a well-tested
library is the safer choice for a conversion whose output lands in the file
format.

Only the double-to-shortest-decimal subset is vendored.  Ryu's 32-bit float
(`f2s.c`), string-to-float (`s2d.c`, `s2f.c`), fixed/exponential-precision
(`d2fixed.c`) and 128-bit generic (`generic_128.c`) translation units are not
included, because nothing in HDF5 calls them.

## Upstream details

| Field         | Value                                                                     |
|---------------|---------------------------------------------------------------------------|
| Upstream URL  | https://github.com/ulfjack/ryu                                            |
| License       | Apache-2.0 **or** Boost-1.0, at your option (both in this directory)      |
| Vendored on   | 2026-09-19                                                                |
| Commit        | `4c0618b0e44f7ef027ebae05d2cc7812048f7c8f` (`master`, 2026-02-09)         |

Ryu's only tag, `v2.0`, long predates the current `master` and does not include
later correctness fixes to `d2s.c` (most recently `3a3e0f71c`, *"removed
subtracting i when zero"*), so this vendoring tracks a pinned `master` commit
rather than a tag.  The vendored files are additionally identified by their
SHA-256 checksums:

| File                 | SHA-256                                                            |
|----------------------|--------------------------------------------------------------------|
| `ryu.h`              | `b7feab0ba1df5e9ef3d602f386592ad152491405e49b408fb21c0e0e9e6bfb16` |
| `common.h`           | `0bbd71d26da6193e678d0776cf418f43f287c73d6fd6725353df0aadf70f2a19` |
| `digit_table.h`      | `8b782573abc0b8554d74163ae6c02f0beb5c30d1e63eaebdb0ddf2c98d817e01` |
| `d2s.c`              | `d24323c7eb77d63f1e52c415212b50060b776f0883525d907d04954fcd48cf64` |
| `d2s_full_table.h`   | `2618f6e5fae6c4443899b184efe3d08295dd267dc9f1a994c983c7caca59ebe6` |
| `d2s_intrinsics.h`   | `1d05702f2edacce428223d4b43dd3095c1bd1f3ad30128ce1761d84356dddc7d` |

## HDF5-local modifications

**None.**  Every file above is byte-for-byte upstream.  Renaming Ryu's three
public symbols out of the global namespace is done entirely from outside the
sources, by force-including the HDF5-authored `h5_ryu_prefix.h` (see that file
and `src/CMakeLists.txt`), which is why the hashes above stay valid.

These files are intentionally excluded from the HDF5 clang-format pass (see
`.github/workflows/clang-format-check.yml` and `bin/format_source`) so that
future upstream updates can be dropped in without a re-formatting step.

## How HDF5 consumes the output

`d2s_buffered_n()` always emits scientific notation with an uppercase
exponent marker and no zero padding -- `3.0` becomes `"3E0"`, `0.1` becomes
`"1E-1"`.  `H5Z__format_double_canonical()` therefore treats Ryu's output as a
*digits + decimal exponent* pair and re-lays it out `%g`-style, choosing
fixed-point notation for the human-scale exponent range and scientific
notation outside it, so the canonical form reads `rate = 3.5`, not
`rate = 3.5E0`.

That re-layout only moves the decimal point; it never rounds, so the
shortest-round-trip guarantee Ryu provides carries over unchanged.

Two behaviors of Ryu matter at the call site and are handled there:

* **Non-finite values.**  Ryu spells these `"NaN"`, `"Infinity"` and
  `"-Infinity"`, none of which are valid TOML.  `H5Z__format_double_canonical()`
  intercepts them ahead of the Ryu call and emits TOML's `nan`, `inf` and
  `-inf` instead.
* **Locale independence.**  Ryu writes ASCII digits and a literal `.` directly,
  never routing through the locale-sensitive `snprintf()`, so no
  `localeconv()`-based decimal-separator fixup is needed on the output.

## Files

| File                 | Description                                            |
|----------------------|--------------------------------------------------------|
| `ryu.h`              | Public API header (declares more than is vendored)     |
| `d2s.c`              | Double-to-shortest-decimal implementation              |
| `d2s_full_table.h`   | Full powers-of-5 lookup tables                         |
| `d2s_intrinsics.h`   | 64/128-bit multiply and shift helpers                  |
| `common.h`           | Shared bit-manipulation helpers                        |
| `digit_table.h`      | Two-digit lookup table for fast digit emission         |
| `h5_ryu_prefix.h`    | HDF5-authored symbol-prefixing shim (not upstream)     |
| `LICENSE-Apache2`    | Apache License 2.0 (Ulf Adams)                         |
| `LICENSE-Boost`      | Boost Software License 1.0 (Ulf Adams)                 |

`d2s_small_table.h` is deliberately not vendored: selecting it requires
`-DRYU_OPTIMIZE_SIZE`, which upstream documents as needing MSVC intrinsics, so
it is not portable across the compilers HDF5 supports.

## Updating the vendored copy

1. Download `ryu.h`, `common.h`, `digit_table.h`, `d2s.c`, `d2s_full_table.h`
   and `d2s_intrinsics.h` from the desired upstream commit.
2. Copy them into this directory, replacing the existing files.
3. Record the new commit hash, date and SHA-256 checksums in the tables above,
   and update the "Vendored on" date.
4. Diff the new `d2s.c`'s non-static definitions against the `#define` list in
   `h5_ryu_prefix.h` and add any new ones -- an unprefixed public symbol
   reintroduces the static-link collision that header prevents.
5. Do **not** run clang-format on these files.
6. Run `ctest -R tfilter2` to verify the canonical form is unchanged.
