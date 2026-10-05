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
 * Renames the public tomlc17 symbols so a static libhdf5 cannot collide with
 * an application's own tomlc17.  Force-included into tomlc17.c by
 * src/CMakeLists.txt, so the vendored files stay byte-identical, and included
 * by H5Zconfig.c.  Keep this list in sync with tomlc17.h when updating.
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
