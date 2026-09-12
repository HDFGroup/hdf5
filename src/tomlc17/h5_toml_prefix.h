/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * h5_toml_prefix.h - HDF5-authored symbol-prefixing shim for the vendored
 * tomlc17 library (src/tomlc17/tomlc17.c, tomlc17.h -- pristine, unmodified
 * copies; see src/tomlc17/README.md for provenance and the update procedure).
 *
 * -fvisibility=hidden (applied to tomlc17.c in src/CMakeLists.txt) only
 * strips these symbols from the dynamic symbol table of a *shared* libhdf5;
 * it does nothing for a static libhdf5.a, whose object files still export
 * ordinary global symbols. An application that statically links libhdf5.a
 * alongside its own copy of tomlc17 (same version or not) would then hit a
 * duplicate-symbol link error, or worse, silently link against whichever
 * copy the linker picks first.
 *
 * This header renames every public tomlc17 symbol to an H5Z__toml_-prefixed
 * name via plain object-like macros, force-included ahead of both tomlc17.c
 * itself (via a compiler flag in src/CMakeLists.txt, since editing an
 * #include into the pristine tomlc17.c would defeat the point) and every
 * HDF5 file that calls into it (H5Zconfig.c, via an explicit #include
 * immediately before "tomlc17/tomlc17.h"). Because a #define is a textual
 * substitution, this renames both tomlc17.c's function *definitions* and
 * every call site consistently, without editing a single byte of the
 * pristine vendored files -- their checked-in SHA-256 hashes in
 * src/tomlc17/README.md remain valid.
 *
 * When updating the vendored copy (see README.md's "Updating the vendored
 * copy" section), diff the new tomlc17.h's public API against the list
 * below and add/remove entries to match -- an unprefixed new public
 * function would silently reintroduce the exact collision this header
 * exists to prevent.
 */

#ifndef H5_TOML_PREFIX_H
#define H5_TOML_PREFIX_H

#define toml_parse             H5Z__toml_c17_parse
#define toml_parse_named       H5Z__toml_c17_parse_named
#define toml_parse_file        H5Z__toml_c17_parse_file
#define toml_parse_file_named  H5Z__toml_c17_parse_file_named
#define toml_parse_file_ex     H5Z__toml_c17_parse_file_ex
#define toml_free              H5Z__toml_c17_free
#define toml_get                H5Z__toml_c17_get
#define toml_seek               H5Z__toml_c17_seek
#define toml_merge              H5Z__toml_c17_merge
#define toml_equiv              H5Z__toml_c17_equiv
#define toml_default_option     H5Z__toml_c17_default_option
#define toml_set_option         H5Z__toml_c17_set_option

#endif /* H5_TOML_PREFIX_H */
