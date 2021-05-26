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

/*-------------------------------------------------------------------------
 *
 * Created:		H5FDtest.c
 *			    Fall 2014
 *
 * Purpose:		File driver testing routines.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#include "H5FDmodule.h" /* This source code file is part of the H5FD module */
#define H5FD_TESTING    /* Suppress warning about H5FD testing funcs    */

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions    */
#include "H5FDpkg.h"     /* File Drivers         */
#include "H5FLprivate.h" /* Free Lists           */
#include "H5Eprivate.h"  /* Error handling       */

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/

/********************/
/* Package Typedefs */
/********************/

/********************/
/* Local Prototypes */
/********************/

/*********************/
/* Package Variables */
/*********************/

/*****************************/
/* Library Private Variables */
/*****************************/
/* Declare external the free list for H5FD_vfd_swmr_idx_entry_t */
H5FL_SEQ_EXTERN(H5FD_vfd_swmr_idx_entry_t);

/*******************/
/* Local Variables */
/*******************/

/*-------------------------------------------------------------------------
 * Function:	H5FD__supports_swmr_test()
 *
 * Purpose:	    Determines if a VFD supports SWMR.
 *
 *              The function determines SWMR support by inspecting the
 *              HDF5_DRIVER environment variable, not by checking the
 *              VFD feature flags (which do not exist until the driver
 *              is instantiated).
 *
 *              See test/Makefile.am for a list of the VFD strings.
 *
 *              This function is only intended for use in the test code.
 *
 * Return:	    TRUE (1) if the VFD supports SWMR I/O or vfd_name is
 *              NULL or the empty string (which implies the default VFD).
 *
 *              FALSE (0) if it does not
 *
 *              This function cannot fail at this time so there is no
 *              error return value.
 *
 * Programmer:	Dana Robinson
 *              Fall 2014
 *
 *-------------------------------------------------------------------------
 */
hbool_t
H5FD__supports_swmr_test(const char *vfd_name)
{
    hbool_t ret_value = FALSE;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if (!vfd_name || !HDstrcmp(vfd_name, "") || !HDstrcmp(vfd_name, "nomatch"))
        ret_value = TRUE;
    else
        ret_value = !HDstrcmp(vfd_name, "log") || !HDstrcmp(vfd_name, "sec2");

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__supports_swmr_test() */

/*
 * Tests for VFD SWMR
 */
/*-------------------------------------------------------------------------
 * Function: H5FD__vfd_swmr_md_test
 *
 * Purpose: Verify the info obtained from the driver's local copy is as
 *          indicated by the parameter: num_entries and index
 *
 * Return:  SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD__vfd_swmr_reader_md_test(H5FD_t *file, unsigned num_entries, H5FD_vfd_swmr_idx_entry_t index[])
{
    unsigned                   vfd_num_entries = 0;
    H5FD_vfd_swmr_idx_entry_t *vfd_index       = NULL;
    unsigned                   i;
    herr_t                     ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Retrieve index from VFD SWMR driver */
    /* Initial call to get # of entries */
    if (H5FD_vfd_swmr_get_tick_and_idx(file, TRUE, NULL, &vfd_num_entries, vfd_index) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "Error in retrieving index from driver")

    /* Verify number of index entries */
    if (vfd_num_entries != num_entries)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "Error in retrieving index from driver")

    if (vfd_num_entries) {
        /* Allocate memory for index entries */
        if (NULL == (vfd_index = H5FL_SEQ_MALLOC(H5FD_vfd_swmr_idx_entry_t, vfd_num_entries)))
            HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL, "memory allocation failed for index entries")

        /* Second call to retrieve the index */
        if (H5FD_vfd_swmr_get_tick_and_idx(file, FALSE, NULL, &vfd_num_entries, vfd_index) < 0)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "Error in retrieving index from driver")

        /* Verify index entries */
        for (i = 0; i < vfd_num_entries; i++) {
            if (vfd_index[i].length != index[i].length)
                HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "incorrect length read from metadata file")

            if (vfd_index[i].hdf5_page_offset != index[i].hdf5_page_offset)
                HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL,
                            "incorrect hdf5_page_offset read from metadata file")

            if (vfd_index[i].md_file_page_offset != index[i].md_file_page_offset)
                HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL,
                            "incorrect md_file_page_offset read from metadata file")

            if (vfd_index[i].chksum != index[i].chksum)
                HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "incorrect chksum read from metadata file")
        }
    }

done:
    /* Free local copy of index entries */
    if (vfd_num_entries && vfd_index)
        vfd_index = (H5FD_vfd_swmr_idx_entry_t *)H5FL_SEQ_FREE(H5FD_vfd_swmr_idx_entry_t, vfd_index);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD__vfd_swmr_reader_md_test() */
