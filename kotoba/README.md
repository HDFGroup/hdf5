# Kotoba v1 — HDF5 signature and superblock

This directory is a first-class language tree on the `kotoba-lang/hdf5` fork,
next to `src/` (C), `c++/`, `fortran/`, and `java/`. It is **not** part of
HDFGroup/hdf5 upstream.

## Honest scope

Kotoba binding **v1** parses only:

- the HDF5 format signature `\211HDF\r\n\032\n` (8 bytes)
- enough of a version-0 superblock to read **version** and **offsets**
  (`sizeof_addr`, `sizeof_size`, base address, end-of-file address)

from the vendored 56-byte fixture `fixtures/tiny-superblock.h5`.

This is **not** a replacement for the C library. It does not implement
datasets, groups, attributes, filters, chunking, virtual datasets, or a
file reader. It is **not robotics-ready**. Superblock versions 1/2/3,
checksums, and the root-group symbol table are out of scope.

The fixture is a hand-built superblock prefix (signature + version-0
header + four 8-byte addresses). It is enough to exercise those fields
and is not a complete HDF5 file.

## Language constraints

- Kotoba CLI **0.7.2**
- `kotoba compile --target wasm` → `wasm32-kotoba-v1`
- value profile **i64-v1** (no IEEE floats)
- no FFI / no host imports

`hdf5.kotoba` embeds the fixture as integer bytes and uses only `+`, `*`,
`if`, `=`, `<`, and `and`. `main` returns a packed i64 of the parsed fields.

## Checks

`checks.sh` compiles with Kotoba 0.7.2, runs the wasm32 module, and
compares the packed result to fields read from the fixture bytes. It does
not invent pass/fail.

```sh
# requires kotoba 0.7.2 on PATH (CI installs it)
./checks.sh
```
