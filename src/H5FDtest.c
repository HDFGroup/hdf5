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
#include "H5private.h"  /* Generic Functions    */
#include "H5Eprivate.h" /* Error handling       */
#include "H5FDpkg.h"    /* File Drivers         */

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

/*******************/
/* Local Variables */
/*******************/

/*-------------------------------------------------------------------------
 * Function:	H5FDopen_test()
 *
 * Purpose:	Wrapper for using H5FDopen() in internal tests.  Pushes
 *              an API context, so that the internal routines will have
 *              one to get values from.
 *
 *              This function is only intended for use in the test code.
 *
 * Return:	Value from underlying routine
 *
 *-------------------------------------------------------------------------
 */
H5FD_t *
H5FDopen_test(const char *name, unsigned flags, hid_t fapl_id, haddr_t maxaddr)
{
    H5FD_t *ret_value;

    FUNC_ENTER_API(NULL)

    /* Call developer routine */
    ret_value = H5FDopen(name, flags, fapl_id, maxaddr);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5FDopen_test() */

/*-------------------------------------------------------------------------
 * Function:	H5FDclose_test()
 *
 * Purpose:	Wrapper for using H5FDclose() in internal tests.  Pushes
 *              an API context, so that the internal routines will have
 *              one to get values from.
 *
 *              This function is only intended for use in the test code.
 *
 * Return:	Value from underlying routine
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FDclose_test(H5FD_t *file)
{
    herr_t ret_value;

    FUNC_ENTER_API(FAIL)

    /* Call developer routine */
    ret_value = H5FDclose(file);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5FDclose_test() */

/*-------------------------------------------------------------------------
 * Function:	H5FDalloc_test()
 *
 * Purpose:	Wrapper for using H5FDalloc() in internal tests.  Pushes
 *              an API context, so that the internal routines will have
 *              one to get values from.
 *
 *              This function is only intended for use in the test code.
 *
 * Return:	Value from underlying routine
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5FDalloc_test(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, hsize_t size)
{
    haddr_t ret_value;

    FUNC_ENTER_API(HADDR_UNDEF)

    /* Call developer routine */
    ret_value = H5FDalloc(file, type, dxpl_id, size);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5FDalloc_test() */

/*-------------------------------------------------------------------------
 * Function:	H5FDget_eoa_test()
 *
 * Purpose:	Wrapper for using H5FDget_eoa() in internal tests.  Pushes
 *              an API context, so that the internal routines will have
 *              one to get values from.
 *
 *              This function is only intended for use in the test code.
 *
 * Return:	Value from underlying routine
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5FDget_eoa_test(H5FD_t *file, H5FD_mem_t type)
{
    haddr_t ret_value;

    FUNC_ENTER_API(HADDR_UNDEF)

    /* Call developer routine */
    ret_value = H5FDget_eoa(file, type);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5FDget_eoa_test() */

/*-------------------------------------------------------------------------
 * Function:	H5FDset_eoa_test()
 *
 * Purpose:	Wrapper for using H5FDset_eoa() in internal tests.  Pushes
 *              an API context, so that the internal routines will have
 *              one to get values from.
 *
 *              This function is only intended for use in the test code.
 *
 * Return:	Value from underlying routine
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FDset_eoa_test(H5FD_t *file, H5FD_mem_t type, haddr_t eoa)
{
    herr_t ret_value;

    FUNC_ENTER_API(FAIL)

    /* Call developer routine */
    ret_value = H5FDset_eoa(file, type, eoa);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5FDset_eoa_test() */

/*-------------------------------------------------------------------------
 * Function:	H5FDget_eof_test()
 *
 * Purpose:	Wrapper for using H5FDget_eof() in internal tests.  Pushes
 *              an API context, so that the internal routines will have
 *              one to get values from.
 *
 *              This function is only intended for use in the test code.
 *
 * Return:	Value from underlying routine
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5FDget_eof_test(H5FD_t *file, H5FD_mem_t type)
{
    haddr_t ret_value;

    FUNC_ENTER_API(HADDR_UNDEF)

    /* Call developer routine */
    ret_value = H5FDget_eof(file, type);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5FDget_eof_test() */

/*-------------------------------------------------------------------------
 * Function:	H5FDread_test()
 *
 * Purpose:	Wrapper for using H5FDread() in internal tests.  Pushes
 *              an API context, so that the internal routines will have
 *              one to get values from.
 *
 *              This function is only intended for use in the test code.
 *
 * Return:	Value from underlying routine
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FDread_test(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t size, void *buf)
{
    herr_t ret_value;

    FUNC_ENTER_API(FAIL)

    /* Call developer routine */
    ret_value = H5FDread(file, type, dxpl_id, addr, size, buf);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5FDread_test() */

/*-------------------------------------------------------------------------
 * Function:	H5FDwrite_test()
 *
 * Purpose:	Wrapper for using H5FDwrite() in internal tests.  Pushes
 *              an API context, so that the internal routines will have
 *              one to get values from.
 *
 *              This function is only intended for use in the test code.
 *
 * Return:	Value from underlying routine
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FDwrite_test(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t size, const void *buf)
{
    herr_t ret_value;

    FUNC_ENTER_API(FAIL)

    /* Call developer routine */
    ret_value = H5FDwrite(file, type, dxpl_id, addr, size, buf);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5FDwrite_test() */

/*-------------------------------------------------------------------------
 * Function:	H5FDread_vector_test()
 *
 * Purpose:	Wrapper for using H5FDread_vector() in internal tests.  Pushes
 *              an API context, so that the internal routines will have
 *              one to get values from.
 *
 *              This function is only intended for use in the test code.
 *
 * Return:	Value from underlying routine
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FDread_vector_test(H5FD_t *file, hid_t dxpl_id, uint32_t count, H5FD_mem_t types[], haddr_t addrs[],
                     size_t sizes[], void *bufs[])
{
    herr_t ret_value;

    FUNC_ENTER_API(FAIL)

    /* Call developer routine */
    ret_value = H5FDread_vector(file, dxpl_id, count, types, addrs, sizes, bufs);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5FDread_vector_test() */

/*-------------------------------------------------------------------------
 * Function:	H5FDwrite_vector_test()
 *
 * Purpose:	Wrapper for using H5FDwrite_vector() in internal tests.  Pushes
 *              an API context, so that the internal routines will have
 *              one to get values from.
 *
 *              This function is only intended for use in the test code.
 *
 * Return:	Value from underlying routine
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FDwrite_vector_test(H5FD_t *file, hid_t dxpl_id, uint32_t count, H5FD_mem_t types[], haddr_t addrs[],
                      size_t sizes[], const void *bufs[])
{
    herr_t ret_value;

    FUNC_ENTER_API(FAIL)

    /* Call developer routine */
    ret_value = H5FDwrite_vector(file, dxpl_id, count, types, addrs, sizes, bufs);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5FDwrite_vector_test() */

/*-------------------------------------------------------------------------
 * Function:	H5FDread_selection_test()
 *
 * Purpose:	Wrapper for using H5FDread_selection() in internal tests.  Pushes
 *              an API context, so that the internal routines will have
 *              one to get values from.
 *
 *              This function is only intended for use in the test code.
 *
 * Return:	Value from underlying routine
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FDread_selection_test(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, uint32_t count, hid_t mem_spaces[],
                        hid_t file_spaces[], haddr_t offsets[], size_t element_sizes[], void *bufs[])
{
    herr_t ret_value;

    FUNC_ENTER_API(FAIL)

    /* Call developer routine */
    ret_value =
        H5FDread_selection(file, type, dxpl_id, count, mem_spaces, file_spaces, offsets, element_sizes, bufs);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5FDread_selection_test() */

/*-------------------------------------------------------------------------
 * Function:	H5FDwrite_selection_test()
 *
 * Purpose:	Wrapper for using H5FDwrite_selection() in internal tests.  Pushes
 *              an API context, so that the internal routines will have
 *              one to get values from.
 *
 *              This function is only intended for use in the test code.
 *
 * Return:	Value from underlying routine
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FDwrite_selection_test(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, uint32_t count, hid_t mem_spaces[],
                         hid_t file_spaces[], haddr_t offsets[], size_t element_sizes[], const void *bufs[])
{
    herr_t ret_value;

    FUNC_ENTER_API(FAIL)

    /* Call developer routine */
    ret_value = H5FDwrite_selection(file, type, dxpl_id, count, mem_spaces, file_spaces, offsets,
                                    element_sizes, bufs);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5FDwrite_selection_test() */

/*-------------------------------------------------------------------------
 * Function:	H5FDtruncate_test()
 *
 * Purpose:	Wrapper for using H5FDtruncate() in internal tests.  Pushes
 *              an API context, so that the internal routines will have
 *              one to get values from.
 *
 *              This function is only intended for use in the test code.
 *
 * Return:	Value from underlying routine
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FDtruncate_test(H5FD_t *file, hid_t dxpl_id, hbool_t closing)
{
    herr_t ret_value;

    FUNC_ENTER_API(FAIL)

    /* Call developer routine */
    ret_value = H5FDtruncate(file, dxpl_id, closing);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5FDtruncate_test() */

/*-------------------------------------------------------------------------
 * Function:	H5FDctl_test()
 *
 * Purpose:	Wrapper for using H5FDctl() in internal tests.  Pushes
 *              an API context, so that the internal routines will have
 *              one to get values from.
 *
 *              This function is only intended for use in the test code.
 *
 * Return:	Value from underlying routine
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FDctl_test(H5FD_t *file, uint64_t op_code, uint64_t flags, const void *input, void **output)
{
    herr_t ret_value;

    FUNC_ENTER_API(FAIL)

    /* Call developer routine */
    ret_value = H5FDctl(file, op_code, flags, input, output);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5FDctl_test() */

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
 *              This function is only intended for use in the test code.
 *
 * Return:	    true (1) if the VFD supports SWMR I/O or vfd_name is
 *              NULL or the empty string (which implies the default VFD).
 *
 *              false (0) if it does not
 *
 *              This function cannot fail at this time so there is no
 *              error return value.
 *-------------------------------------------------------------------------
 */
bool
H5FD__supports_swmr_test(const char *vfd_name)
{
    bool ret_value = false;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if (!vfd_name || !strcmp(vfd_name, "") || !strcmp(vfd_name, "nomatch"))
        ret_value = true;
    else
        ret_value = !strcmp(vfd_name, "log") || !strcmp(vfd_name, "sec2");

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__supports_swmr_test() */
