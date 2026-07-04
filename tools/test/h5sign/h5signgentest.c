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
 * Purpose:  Generate test files for h5sign testing
 *
 *           Creates dummy plugin binaries and test keys for signature testing.
 */

#include "hdf5.h"
#include "H5private.h"

/*-------------------------------------------------------------------------
 * Function:    create_dummy_plugin
 *
 * Purpose:     Create a simple binary file that mimics a plugin
 *
 * Return:      Success: 0
 *              Failure: 1
 *-------------------------------------------------------------------------
 */
static int
create_dummy_plugin(const char *filename, size_t size)
{
    FILE  *fp;
    size_t i;

    if (NULL == (fp = fopen(filename, "wb"))) {
        fprintf(stderr, "Error: Cannot create file '%s'\n", filename);
        return 1;
    }

    /* Write simple binary pattern */
    for (i = 0; i < size; i++) {
        unsigned char byte = (unsigned char)(i % 256);
        if (1 != fwrite(&byte, 1, 1, fp)) {
            fprintf(stderr, "Error: Cannot write to file '%s'\n", filename);
            fclose(fp);
            return 1;
        }
    }

    fclose(fp);
    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Generate test files for h5sign
 *
 * Return:      Success: EXIT_SUCCESS
 *              Failure: EXIT_FAILURE
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    /* Create dummy plugin files of various sizes */
    if (create_dummy_plugin("plugin_small.so", 1024) != 0)
        return EXIT_FAILURE;

    if (create_dummy_plugin("plugin_medium.so", 64 * 1024) != 0)
        return EXIT_FAILURE;

    if (create_dummy_plugin("plugin_large.so", 1024 * 1024) != 0)
        return EXIT_FAILURE;

    printf("Test files created successfully\n");
    return EXIT_SUCCESS;
}
