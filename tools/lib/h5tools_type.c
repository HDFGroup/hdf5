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

#include "H5private.h"
#include "h5tools.h"

/*-------------------------------------------------------------------------
 * Function: h5tools_get_little_endian_type
 *
 * Purpose: Get a little endian type from a file type
 *
 * Return: Success:    datatype ID
 *         Failure:    FAIL
 *-------------------------------------------------------------------------
 */
hid_t
h5tools_get_little_endian_type(hid_t tid)
{
    hid_t       p_type=-1;
    H5T_class_t type_class;
    size_t      size;
    H5T_sign_t  sign;

    type_class = H5Tget_class(tid);
    size       = H5Tget_size(tid);
    sign       = H5Tget_sign(tid);

    switch(type_class) {
        case H5T_INTEGER:
            if (size == 1 && sign == H5T_SGN_2)
                p_type = H5Tcopy(H5T_STD_I8LE);
            else if (size == 2 && sign == H5T_SGN_2)
                p_type = H5Tcopy(H5T_STD_I16LE);
            else if (size == 4 && sign == H5T_SGN_2)
                p_type = H5Tcopy(H5T_STD_I32LE);
            else if (size == 8 && sign == H5T_SGN_2)
                p_type = H5Tcopy(H5T_STD_I64LE);
            else if (size == 1 && sign == H5T_SGN_NONE)
                p_type = H5Tcopy(H5T_STD_U8LE);
            else if (size == 2 && sign == H5T_SGN_NONE)
                p_type = H5Tcopy(H5T_STD_U16LE);
            else if (size == 4 && sign == H5T_SGN_NONE)
                p_type = H5Tcopy(H5T_STD_U32LE);
            else if (size == 8 && sign == H5T_SGN_NONE)
                p_type = H5Tcopy(H5T_STD_U64LE);
            break;

        case H5T_FLOAT:
            if (size == 4)
                p_type = H5Tcopy(H5T_IEEE_F32LE);
            else if (size == 8)
                p_type = H5Tcopy(H5T_IEEE_F64LE);
            break;

        case H5T_BITFIELD:
            if (size == 1)
                p_type = H5Tcopy(H5T_STD_B8LE);
            else if (size == 2)
                p_type = H5Tcopy(H5T_STD_B16LE);
            else if (size == 4)
                p_type = H5Tcopy(H5T_STD_B32LE);
            else if (size == 8)
                p_type = H5Tcopy(H5T_STD_B64LE);
            break;

        case H5T_TIME:
        case H5T_OPAQUE:
        case H5T_STRING:
        case H5T_COMPOUND:
        case H5T_REFERENCE:
        case H5T_ENUM:
        case H5T_VLEN:
        case H5T_ARRAY:
            break;

        case H5T_NO_CLASS:
        case H5T_NCLASSES:
        default:
            break;

    } /* end switch */

    return p_type;
} /* end h5tools_get_little_endian_type() */


/*-------------------------------------------------------------------------
 * Function: h5tools_get_big_endian_type
 *
 * Purpose: Get a big endian type from a file type
 *
 * Return: Success:    datatype ID
 *         Failure:    FAIL
 *-------------------------------------------------------------------------
 */
hid_t
h5tools_get_big_endian_type(hid_t tid)
{
    hid_t       p_type = -1;
    H5T_class_t type_class;
    size_t      size;
    H5T_sign_t  sign;

    type_class = H5Tget_class(tid);
    size       = H5Tget_size(tid);
    sign       = H5Tget_sign(tid);

    switch(type_class) {
        case H5T_INTEGER:
            if (size == 1 && sign == H5T_SGN_2)
                p_type = H5Tcopy(H5T_STD_I8BE);
            else if (size == 2 && sign == H5T_SGN_2)
                p_type = H5Tcopy(H5T_STD_I16BE);
            else if (size == 4 && sign == H5T_SGN_2)
                p_type = H5Tcopy(H5T_STD_I32BE);
            else if (size == 8 && sign == H5T_SGN_2)
                p_type = H5Tcopy(H5T_STD_I64BE);
            else if (size == 1 && sign == H5T_SGN_NONE)
                p_type = H5Tcopy(H5T_STD_U8BE);
            else if (size == 2 && sign == H5T_SGN_NONE)
                p_type = H5Tcopy(H5T_STD_U16BE);
            else if (size == 4 && sign == H5T_SGN_NONE)
                p_type = H5Tcopy(H5T_STD_U32BE);
            else if (size == 8 && sign == H5T_SGN_NONE)
                p_type = H5Tcopy(H5T_STD_U64BE);
            break;

        case H5T_FLOAT:
            if (size == 4)
                p_type = H5Tcopy(H5T_IEEE_F32BE);
            else if (size == 8)
                p_type = H5Tcopy(H5T_IEEE_F64BE);
            break;

        case H5T_BITFIELD:
            if (size == 1)
                p_type = H5Tcopy(H5T_STD_B8BE);
            else if (size == 2)
                p_type = H5Tcopy(H5T_STD_B16BE);
            else if (size == 4)
                p_type = H5Tcopy(H5T_STD_B32BE);
            else if (size == 8)
                p_type=H5Tcopy(H5T_STD_B64BE);
            break;

        case H5T_TIME:
        case H5T_OPAQUE:
        case H5T_STRING:
        case H5T_COMPOUND:
        case H5T_REFERENCE:
        case H5T_ENUM:
        case H5T_VLEN:
        case H5T_ARRAY:
            break;

        case H5T_NO_CLASS:
        case H5T_NCLASSES:
        default:
            break;
    } /* end switch */

    return p_type;
} /* end h5tools_get_big_endian_type() */

