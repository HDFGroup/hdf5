/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Common definitions for "remote" processes for the mirror VFD
 */

#include "hdf5.h"
#include "H5private.h"

#ifdef H5_HAVE_MIRROR_VFD

#include "H5FDmirror_priv.h" /* Private header for the mirror VFD */

#define V_NONE 0
#define V_ERR  1
#define V_WARN 2
#define V_INFO 3
#define V_ALL  4

#define MIRROR_LOG_DEFAULT_STREAM    stdout
#define MIRROR_LOG_DEFAULT_VERBOSITY V_WARN
#define MIRROR_LOG_PREFIX_MAX        79
#define MIRROR_LOG_INFO_MAGIC        0x569D589A

typedef struct mirror_log_info {
    uint32_t     magic;
    FILE        *stream;
    unsigned int verbosity;
    char         prefix[MIRROR_LOG_PREFIX_MAX + 1];
} loginfo_t;

void       mirror_log(loginfo_t *info, unsigned int level, const char *format, ...);
void       mirror_log_bytes(loginfo_t *info, unsigned int level, size_t n_bytes, const unsigned char *buf);
loginfo_t *mirror_log_init(char *path, const char *prefix, unsigned int verbosity);
int        mirror_log_term(loginfo_t *loginfo);

herr_t run_writer(int socketfd, H5FD_mirror_xmit_open_t *xmit_open);

#endif /* H5_HAVE_MIRROR_VFD */
