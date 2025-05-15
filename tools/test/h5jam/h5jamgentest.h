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

#ifndef H5JAM_GENTEST_H
#define H5JAM_GENTEST_H

#include "hdf5.h"

/* not used yet
#define UBTXT1 "u0.txt"
*/
#define UBTXT2 "u10.txt"
#define UBTXT3 "u511.txt"
#define UBTXT4 "u512.txt"
#define UBTXT5 "u513.txt"

/* tall is same as dumper test */
#define H5JAM_FILE7 "tall.h5"
#define H5JAM_FILE8 "twithub.h5"
#define H5JAM_FILE9 "twithub513.h5"

herr_t gent_ub(const char *filename, size_t ub_size, size_t ub_fill);
herr_t create_textfile(const char *name, size_t size);

/*
 * This pattern is used to fill text files
 */
#define PATTERN_LEN 11
extern char pattern[PATTERN_LEN];

#endif /* H5JAM_GENTEST_H */