/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef H5DIFFCOMMON_H__
#define H5DIFFCOMMON_H__

#include "h5tools.h"
/* Name of tool */
#define PROGRAMNAME "h5diff"

#ifdef __cplusplus
extern "C" {
#endif

void usage(void);
void parse_command_line(int argc, const char* argv[], const char** fname1, const char** fname2, const char** objname1, const char** objname2, diff_opt_t* opts);
void h5diff_exit(int status);
void print_info(diff_opt_t* opts);

#ifdef __cplusplus
}
#endif

#endif /* H5DIFFCOMMON_H__ */
