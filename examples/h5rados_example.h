/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* This source code was developed under the Mochi project
 * (https://www.mcs.anl.gov/research/projects/mochi), supported by the U.S.
 * Department of Energy, Office of Science, under contract DE-AC02-06CH11357.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <mpi.h>
#include <hdf5.h>
#include <H5VLrados_public.h>

/* Macros for printing standard messages and issuing errors */
#define AT()            printf ("        at %s:%d in %s()...\n", __FILE__, __LINE__, __FUNCTION__)
#define FAILED()        do {puts("*FAILED*");fflush(stdout);} while(0)
#define ERROR           do {FAILED(); AT(); goto error;} while(0)
#define PRINTF_ERROR(...) do {FAILED(); AT(); printf("        " __VA_ARGS__); printf("\n"); goto error;} while(0)

/* Config file */
#ifdef HDF5_USE_MOBJECT
#define CEPH_CONFIG_FILE "/tmp/mobject-cluster-test.gid"
#else
#define CEPH_CONFIG_FILE "ceph.conf"
#endif

