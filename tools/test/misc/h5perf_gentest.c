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
/*****************************************************************************
  This test generates attributes, groups, and datasets of many types. It
  creates a large number of attributes, groups, and datasets by specifying
  -a, -g, -d options respectively. Using "-h" option to see details.

 ****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "h5perf_gentest.h"

int
main(int argc, char *argv[])
{
    char fname[32];
    int  i, ngrps = NGROUPS, ndsets = NDSETS, nattrs = NATTRS, dim0 = DIM0, chunk = DIM0 / 10 + 1,
           nrows = NROWS, vlen = MAXVLEN, l = 0, z = 0;

    memset(fname, 0, 32);
    for (i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-f") == 0)
            strcpy(fname, argv[i + 1]);
        else if (strcmp(argv[i], "-g") == 0)
            ngrps = atoi(argv[i + 1]);
        else if (strcmp(argv[i], "-d") == 0)
            ndsets = atoi(argv[i + 1]);
        else if (strcmp(argv[i], "-a") == 0)
            nattrs = atoi(argv[i + 1]);
        else if (strcmp(argv[i], "-r") == 0)
            nrows = atoi(argv[i + 1]);
        else if (strcmp(argv[i], "-s") == 0)
            dim0 = atoi(argv[i + 1]);
        else if (strcmp(argv[i], "-c") == 0)
            chunk = atoi(argv[i + 1]);
        else if (strcmp(argv[i], "-v") == 0)
            vlen = atoi(argv[i + 1]);
        else if (strcmp(argv[i], "-l") == 0)
            l = 1;
        else if (strcmp(argv[i], "-z") == 0)
            z = 1;
        else if (strcmp(argv[i], "-h") == 0) {
            printf("\nOPTONS:\n");
            printf("\t-f F:\tname of the test file (default: %s).\n", FNAME);
            printf("\t-g N:\tnumber of top level groups (default: %d).\n", NGROUPS);
            printf("\t-d N:\tnumber of datasets (default: %d).\n", NDSETS);
            printf("\t-a N:\tnumber of attributes (default: %d).\n", NATTRS);
            printf("\t-r N:\tnumber of rows in the large compound dataset (default: %d).\n", NROWS);
            printf("\t-s N:\tsize of dim0 in datasets (default: %d).\n", DIM0);
            printf("\t-c N:\tchunk size of dim0 (default: %d).\n", (DIM0 / 10 + 1));
            printf("\t-v N:\tmax vlen size (default: %d).\n", MAXVLEN);
            printf("\t-l:\tuse latest format (default: no).\n");
            printf("\t-z:\tuse gzip compression (default: no).\n");
            printf("\t-h:\tthis help information.\n");
            printf("Example:\n");
            printf("\t./a.out -f test.h5 -g 10000 -d 5000 -a 500 -r 10000 -s 200 -c 20 -v 40 -l -z\n\n");
            exit(0);
        }
    }

    if (strlen(fname) <= 0)
        snprintf(fname, sizeof(fname), FNAME);

    create_perf_test_file(fname, ngrps, ndsets, nattrs, (hsize_t)nrows, (hsize_t)dim0, (hsize_t)chunk, vlen,
                          z, l);

    return EXIT_SUCCESS;
}
