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
 * Renames the functions d2s.c defines so a static libhdf5 cannot collide with
 * an application's own Ryu.  Force-included into d2s.c by src/CMakeLists.txt,
 * so the vendored files stay byte-identical, and included by H5Zconfig.c.
 * Keep this list in sync with d2s.c when updating.
 */

#ifndef H5_RYU_PREFIX_H
#define H5_RYU_PREFIX_H

#define d2s            H5Z__ryu_d2s
#define d2s_buffered   H5Z__ryu_d2s_buffered
#define d2s_buffered_n H5Z__ryu_d2s_buffered_n

#endif /* H5_RYU_PREFIX_H */
