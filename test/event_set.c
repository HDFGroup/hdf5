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

/*
 * Programmer:  Quincey Koziol <koziol@lbl.gov>
 *              Wednesday, April 8, 2020
 *
 * Purpose:    Tests event sets.
 */
#include "h5test.h"
#include "H5srcdir.h"


const char *FILENAME[] = {
    "eventset_1",
    NULL
};


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests event sets
 *
 * Return:      Success: EXIT_SUCCESS
 *              Failure: EXIT_FAILURE
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, April 8, 2020
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    int    nerrors=0, argno, test_contig=1, test_chunk=1, test_compact=1;
    hid_t    fapl = (-1), fapl2 = (-1);    /* File access property lists */

    /* Setup */
    h5_reset();
    fapl = h5_fileaccess();

    /* Tests */
#ifdef NOT_YET
    nerrors += test_create(fapl, FILENAME[0]);
#endif /* NOT_YET */

    /* Report status */
    if(nerrors)
        goto error;
    HDputs("All event set tests passed.");

    /* Cleanup */
    h5_cleanup(FILENAME, fapl);

    HDexit(EXIT_SUCCESS);

error:
    HDputs("***** EVENT SET TESTS FAILED *****");
    HDexit(EXIT_FAILURE);
} /* end main() */

