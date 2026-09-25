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
 * h5_ryu_prefix.h - HDF5-authored symbol-prefixing shim for the vendored Ryu
 * float-to-shortest-decimal library (src/ryu/d2s.c and its headers --
 * pristine, unmodified copies; see src/ryu/README.md for provenance and the
 * update procedure).
 *
 * Same rationale as src/tomlc17/h5_toml_prefix.h: -fvisibility=hidden
 * (applied to d2s.c in src/CMakeLists.txt) only strips these symbols from the
 * dynamic symbol table of a *shared* libhdf5, and does nothing for a static
 * libhdf5.a. `d2s`, `d2s_buffered` and `d2s_buffered_n` are short, unnamespaced
 * globals, so an application that statically links libhdf5.a alongside its own
 * copy of Ryu -- a realistic pairing, since Ryu is exactly the kind of library
 * a numerics-heavy application vendors too -- would hit a duplicate-symbol
 * link error, or silently bind to whichever copy the linker picks first.
 *
 * Renaming happens via plain object-like macros, force-included ahead of both
 * d2s.c itself (via a compiler flag in src/CMakeLists.txt, since editing an
 * #include into the pristine d2s.c would defeat the point) and every HDF5 file
 * that calls into it (H5Zconfig.c, via an explicit #include immediately before
 * "ryu/ryu.h"). No byte of the vendored files changes, so their checked-in
 * SHA-256 hashes in src/ryu/README.md remain valid.
 *
 * Only the three functions d2s.c *defines* need renaming. ryu.h also declares
 * f2s*, d2fixed* and d2exp*, whose translation units this directory
 * deliberately does not vendor; leaving them unprefixed is harmless because
 * nothing defines or calls them here.
 *
 * When updating the vendored copy (see README.md's "Updating the vendored
 * copy" section), diff the new d2s.c's non-static definitions against the list
 * below and add/remove entries to match -- an unprefixed new public function
 * would silently reintroduce the exact collision this header exists to
 * prevent.
 */

#ifndef H5_RYU_PREFIX_H
#define H5_RYU_PREFIX_H

#define d2s            H5Z__ryu_d2s
#define d2s_buffered   H5Z__ryu_d2s_buffered
#define d2s_buffered_n H5Z__ryu_d2s_buffered_n

#endif /* H5_RYU_PREFIX_H */
