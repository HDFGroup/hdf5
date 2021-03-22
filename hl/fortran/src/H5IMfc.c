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

/* This files contains C stubs for H5D Fortran APIs */

#include "H5IMcc.h"
#include "H5LTf90proto.h"
#include "H5Eprivate.h"

/*-------------------------------------------------------------------------
 * Function: h5immake_image_8bit_c
 *
 * Purpose: Call H5IMmake_image_8bit
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente
 *
 * Date: October 05, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
h5immake_image_8bit_c(hid_t_f *loc_id, size_t_f *namelen, _fcd name, hsize_t_f *width, hsize_t_f *height,
                      int_f *buf)
{
    int     ret_value = -1;
    herr_t  ret;
    hid_t   c_loc_id;
    char *  c_name = NULL;
    hsize_t w      = (hsize_t)*width;
    hsize_t h      = (hsize_t)*height;

    /*
     * convert FORTRAN name to C name
     */
    c_name = (char *)HD5f2cstring(name, (size_t)*namelen);
    if (c_name == NULL)
        goto done;

    /*
     * call H5IMmake_image_8bitf function.
     */
    c_loc_id = (hid_t)*loc_id;
    ret      = H5IMmake_image_8bitf(c_loc_id, c_name, w, h, buf);

    if (ret < 0)
        goto done;

    ret_value = 0;

done:
    if (c_name != NULL)
        HDfree(c_name);

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: h5imread_image_c
 *
 * Purpose: Call H5IMread_image
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente
 *
 * Date: October 05, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
h5imread_image_c(hid_t_f *loc_id, size_t_f *namelen, _fcd name, int_f *buf)
{
    int    ret_value = -1;
    herr_t ret;
    char * c_name = NULL;

    /*
     * convert FORTRAN name to C name
     */
    c_name = (char *)HD5f2cstring(name, (size_t)*namelen);
    if (c_name == NULL)
        goto done;

    /*
     * call H5IMread_image function.
     */
    ret = H5IMread_imagef((hid_t)*loc_id, c_name, buf);

    if (ret < 0)
        goto done;

    ret_value = 0;

done:
    if (c_name != NULL)
        HDfree(c_name);

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: h5immake_image_24bit_c
 *
 * Purpose: Call H5IMmake_image_24bit
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente
 *
 * Date: October 05, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
h5immake_image_24bit_c(hid_t_f *loc_id, size_t_f *namelen, _fcd name, size_t_f *ilen, _fcd il,
                       hsize_t_f *width, hsize_t_f *height, void *buf)
{
    int     ret_value = -1;
    herr_t  ret;
    hid_t   c_loc_id;
    char *  c_name = NULL;
    char *  c_il   = NULL;
    hsize_t w      = (hsize_t)*width;
    hsize_t h      = (hsize_t)*height;

    /*
     * convert FORTRAN name to C name
     */
    c_name = (char *)HD5f2cstring(name, (size_t)*namelen);
    if (c_name == NULL)
        goto done;

    c_il = (char *)HD5f2cstring(il, (size_t)*ilen);
    if (c_il == NULL)
        goto done;

    /*
     * call H5IMmake_image_24bitf function.
     */
    c_loc_id = (hid_t)*loc_id;

    ret = H5IMmake_image_24bitf(c_loc_id, c_name, w, h, c_il, (int_f *)buf);

    if (ret < 0)
        goto done;

    ret_value = 0;

done:
    if (c_name != NULL)
        HDfree(c_name);
    if (c_il != NULL)
        HDfree(c_il);

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: h5imget_image_info_c
 *
 * Purpose: Call H5IMget_image_info
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente
 *
 * Date: October 05, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
h5imget_image_info_c(hid_t_f *loc_id, size_t_f *namelen, _fcd name, hsize_t_f *width, hsize_t_f *height,
                     hsize_t_f *planes, hsize_t_f *npals, size_t_f *ilen, _fcd interlace)
{
    int      ret_value = -1;
    herr_t   ret;
    hid_t    c_loc_id;
    char *   c_name = NULL;
    hsize_t  c_width;
    hsize_t  c_height;
    hsize_t  c_planes;
    hssize_t c_npals;
    char *   c_buf = NULL; /* buffer to hold C string */

    /*
     * convert FORTRAN name to C name
     */
    c_name = (char *)HD5f2cstring(name, (size_t)*namelen);
    if (c_name == NULL)
        goto done;

    /*
     * allocate buffer to hold name of an attribute
     */
    if ((c_buf = (char *)HDmalloc((size_t)*ilen + 1)) == NULL)
        goto done;

    /*
     * call H5IMget_image_info function.
     */
    c_loc_id = (hid_t)*loc_id;

    ret = H5IMget_image_info(c_loc_id, c_name, &c_width, &c_height, &c_planes, c_buf, &c_npals);

    if (ret < 0)
        goto done;

    *width  = (hsize_t_f)c_width;
    *height = (hsize_t_f)c_height;
    *planes = (hsize_t_f)c_planes;
    *npals  = (hsize_t_f)c_npals;

    /*
     * convert C name to FORTRAN and place it in the given buffer
     */
    HD5packFstring(c_buf, _fcdtocp(interlace), (size_t)*ilen);

    ret_value = 0;

done:
    if (c_name != NULL)
        HDfree(c_name);
    if (c_buf != NULL)
        HDfree(c_buf);

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: h5imis_image_c
 *
 * Purpose: Call H5IMis_image
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente
 *
 * Date: October 06, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
h5imis_image_c(hid_t_f *loc_id, size_t_f *namelen, _fcd name)
{
    hid_t  c_loc_id;
    char * c_name = NULL;
    herr_t ret;

    /*
     * convert FORTRAN name to C name
     */
    c_name = (char *)HD5f2cstring(name, (size_t)*namelen);
    if (c_name == NULL)
        return -1;

    /*
     * call H5LTget_dataset_ndims function.
     */
    c_loc_id = (hid_t)*loc_id;

    ret = H5IMis_image(c_loc_id, c_name);

    if (c_name != NULL)
        HDfree(c_name);

    return ret;
}

/*-------------------------------------------------------------------------
 * Function: h5immake_palette_c
 *
 * Purpose: Call H5IMmake_palette
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente
 *
 * Date: October 06, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
h5immake_palette_c(hid_t_f *loc_id, size_t_f *namelen, _fcd name, hsize_t_f *dims, void *buf)
{
    char *  c_name = NULL;
    hsize_t c_dims[H5S_MAX_RANK];
    int     i;
    int     rank      = 2;
    int_f   ret_value = 0;

    /*
     * convert FORTRAN name to C name
     */
    if (NULL == (c_name = (char *)HD5f2cstring(name, (size_t)*namelen)))
        HGOTO_DONE(FAIL)

    for (i = 0; i < rank; i++)
        c_dims[i] = (hsize_t)dims[i];

    /*
     * call H5IMmake_palette function.
     */
    if (H5IMmake_palettef((hid_t)*loc_id, c_name, c_dims, (int_f *)buf) < 0)
        HGOTO_DONE(FAIL)

done:
    if (c_name)
        HDfree(c_name);

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: h5imlink_palette_c
 *
 * Purpose: Call H5IMlink_palette
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente
 *
 * Date: October 06, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
h5imlink_palette_c(hid_t_f *loc_id, size_t_f *namelen, _fcd name, size_t_f *ilen, _fcd pal_name)
{
    int    ret_value = -1;
    herr_t ret;
    hid_t  c_loc_id;
    char * c_name    = NULL;
    char * c_namepal = NULL;

    /*
     * convert FORTRAN name to C name
     */
    c_name = (char *)HD5f2cstring(name, (size_t)*namelen);
    if (c_name == NULL)
        goto done;

    c_namepal = (char *)HD5f2cstring(pal_name, (size_t)*ilen);
    if (c_namepal == NULL)
        goto done;

    /*
     * call H5IMlink_palette function.
     */
    c_loc_id = (hid_t)*loc_id;

    ret = H5IMlink_palette(c_loc_id, c_name, c_namepal);

    if (ret < 0)
        goto done;

    ret_value = 0;

done:
    if (c_name != NULL)
        HDfree(c_name);
    if (c_namepal != NULL)
        HDfree(c_namepal);

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: h5imunlink_palette_c
 *
 * Purpose: Call H5IMunlink_palette
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente
 *
 * Date: October 06, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
h5imunlink_palette_c(hid_t_f *loc_id, size_t_f *namelen, _fcd name, size_t_f *ilen, _fcd pal_name)
{
    int    ret_value = -1;
    herr_t ret;
    hid_t  c_loc_id;
    char * c_name    = NULL;
    char * c_namepal = NULL;

    /*
     * convert FORTRAN name to C name
     */
    c_name = (char *)HD5f2cstring(name, (size_t)*namelen);
    if (c_name == NULL)
        goto done;

    c_namepal = (char *)HD5f2cstring(pal_name, (size_t)*ilen);
    if (c_namepal == NULL)
        goto done;

    /*
     * call H5IMunlink_palette function.
     */
    c_loc_id = (hid_t)*loc_id;

    ret = H5IMunlink_palette(c_loc_id, c_name, c_namepal);

    if (ret < 0)
        goto done;

    ret_value = 0;

done:
    if (c_name != NULL)
        HDfree(c_name);
    if (c_namepal != NULL)
        HDfree(c_namepal);

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: h5imget_npalettes_c
 *
 * Purpose: Call H5IMget_npalettes
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente
 *
 * Date: October 06 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
h5imget_npalettes_c(hid_t_f *loc_id, size_t_f *namelen, _fcd name, hsize_t_f *npals)
{
    int      ret_value = -1;
    herr_t   ret;
    hid_t    c_loc_id;
    char *   c_name = NULL;
    hssize_t c_npals;

    /*
     * convert FORTRAN name to C name
     */
    c_name = (char *)HD5f2cstring(name, (size_t)*namelen);
    if (c_name == NULL)
        goto done;

    /*
     * call H5IMget_image_info function.
     */
    c_loc_id = (hid_t)*loc_id;

    ret = H5IMget_npalettes(c_loc_id, c_name, &c_npals);

    *npals = (hsize_t_f)c_npals;

    if (ret < 0)
        goto done;

    ret_value = 0;

done:
    if (c_name != NULL)
        HDfree(c_name);

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: h5imget_palette_info_c
 *
 * Purpose: Call H5IMget_palette_info
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente
 *
 * Date: October 06 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
h5imget_palette_info_c(hid_t_f *loc_id, size_t_f *namelen, _fcd name, int_f *pal_number, hsize_t_f *dims)
{
    int     ret_value = -1;
    herr_t  ret;
    hid_t   c_loc_id;
    char *  c_name = NULL;
    hsize_t c_dims[2];
    int     i;

    /*
     * convert FORTRAN name to C name
     */
    c_name = (char *)HD5f2cstring(name, (size_t)*namelen);
    if (c_name == NULL)
        goto done;

    /*
     * call H5IMget_image_info function.
     */
    c_loc_id = (hid_t)*loc_id;

    ret = H5IMget_palette_info(c_loc_id, c_name, *pal_number, c_dims);

    if (ret < 0)
        goto done;

    for (i = 0; i < 2; i++) {
        dims[i] = (hsize_t_f)c_dims[i];
    }

    ret_value = 0;

done:
    if (c_name != NULL)
        HDfree(c_name);

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: h5imget_palette_c
 *
 * Purpose: Call H5IMget_palette
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente
 *
 * Date: October 06 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
h5imget_palette_c(hid_t_f *loc_id, size_t_f *namelen, _fcd name, int_f *pal_number, void *buf)
{
    int    ret_value = -1;
    herr_t ret;
    hid_t  c_loc_id;
    char * c_name = NULL;

    /*
     * convert FORTRAN name to C name
     */
    c_name = (char *)HD5f2cstring(name, (size_t)*namelen);
    if (c_name == NULL)
        goto done;

    /*
     * call H5IMget_image_info function.
     */
    c_loc_id = (hid_t)*loc_id;

    ret = H5IMget_palettef(c_loc_id, c_name, *pal_number, (int_f *)buf);

    if (ret < 0)
        goto done;

    ret_value = 0;

done:
    if (c_name != NULL)
        HDfree(c_name);

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: h5imis_palette_c
 *
 * Purpose: Call H5IMis_palette
 *
 * Return: true, false, fail
 *
 * Programmer: Pedro Vicente
 *
 * Date: October 06, 2004
 *
 * Comments:
 *
 * Modifications:
 *
 *
 *-------------------------------------------------------------------------
 */

int_f
h5imis_palette_c(hid_t_f *loc_id, size_t_f *namelen, _fcd name)
{
    hid_t  c_loc_id;
    char * c_name;
    herr_t ret;

    /*
     * convert FORTRAN name to C name
     */
    c_name = (char *)HD5f2cstring(name, (size_t)*namelen);
    if (c_name == NULL)
        return -1;

    /*
     * call H5IMis_palette function.
     */
    c_loc_id = (hid_t)*loc_id;

    ret = H5IMis_palette(c_loc_id, c_name);

    if (c_name != NULL)
        HDfree(c_name);

    return ret;
}
