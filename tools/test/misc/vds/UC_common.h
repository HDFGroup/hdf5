/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef USE_CASE_COMMON_H
#define USE_CASE_COMMON_H

/******************************************
 * Symbols used across multiple use cases *
 ******************************************/

/* All datasets are 3D */
#define RANK 3

/* Lengths of string identifiers (file, dataset names, etc.) */
#define NAME_LEN 32

/* Compression level */
#define COMPRESSION_LEVEL 7

/* Booleans */
#define TRUE  1
#define FALSE 0

/* Testing macros */
#define AT() printf("   at %s:%d in %s()...\n", __FILE__, __LINE__, __func__);
#define UC_ERROR                                                                                             \
    {                                                                                                        \
        puts("*ERROR*");                                                                                     \
        fflush(stdout);                                                                                      \
        AT();                                                                                                \
        goto error;                                                                                          \
    }

#endif /* USE_CASE_COMMON_H */
