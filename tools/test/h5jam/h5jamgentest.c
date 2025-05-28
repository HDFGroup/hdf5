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

#include "hdf5.h"
#include "H5private.h"
#include "h5gentest.h"
#include "h5jamgentest.h"

/*-------------------------------------------------------------------------
 * prototypes
 *-------------------------------------------------------------------------
 */

#define BUF_SIZE 1024

char pattern[PATTERN_LEN] = "abcdefghij";

/* gent_ub
    with no ub, identical to gent_all from h5dumpgentest.c

    FILENAME is the name of the file to create
    UB_SIZE is the size the buffer should be
    UB_FILL characters will be set to the PATTERN array,
        the rest of the user block will be NULL.

/ : g1  g2  attr1  attr2
g1 : g1.1  g1.2
g1.1 : dset1.1.1(attr1, attr2)   dset1.1.2
g1.2 : g1.2.1 extlink
g1.2.1 : slink
g2 : dset2.1  dset2.2 udlink

*/
herr_t
gent_ub(const char *filename, size_t ub_size, size_t ub_fill)
{
    hid_t   fid          = H5I_INVALID_HID;
    hid_t   group        = H5I_INVALID_HID;
    hid_t   attr         = H5I_INVALID_HID;
    hid_t   dataset      = H5I_INVALID_HID;
    hid_t   space        = H5I_INVALID_HID;
    hid_t   create_plist = H5I_INVALID_HID;
    hsize_t dims[2];
    int     data[2][2], dset1[10][10], dset2[20];
    char    buf[BUF_SIZE];
    int     i, j;
    size_t  u;
    float   dset2_1[10], dset2_2[3][5];
    int     fd = -1;

    if (ub_size > 0) {
        if ((create_plist = H5Pcreate(H5P_FILE_CREATE)) < 0)
            goto error;
        if (H5Pset_userblock(create_plist, (hsize_t)ub_size) < 0)
            goto error;
        if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, create_plist, H5P_DEFAULT)) < 0)
            goto error;
    }
    else {
        if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto error;
    }

    /* Create groups */
    if ((group = H5Gcreate2(fid, "/g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;
    if (H5Gclose(group) < 0)
        goto error;

    if ((group = H5Gcreate2(fid, "/g2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;
    if (H5Gclose(group) < 0)
        goto error;

    if ((group = H5Gcreate2(fid, "/g1/g1.1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;
    if (H5Gclose(group) < 0)
        goto error;

    if ((group = H5Gcreate2(fid, "/g1/g1.2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;
    if (H5Gclose(group) < 0)
        goto error;

    if ((group = H5Gcreate2(fid, "/g1/g1.2/g1.2.1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;
    if (H5Gclose(group) < 0)
        goto error;

    /* Root attributes */
    if ((group = H5Gopen2(fid, "/", H5P_DEFAULT)) < 0)
        goto error;

    dims[0] = 10;
    if ((space = H5Screate_simple(1, dims, NULL)) < 0)
        goto error;
    if ((attr = H5Acreate2(group, "attr1", H5T_STD_I8BE, space, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;
    if (snprintf(buf, sizeof(buf), "abcdefghi") < 0)
        goto error;
    if (H5Awrite(attr, H5T_NATIVE_SCHAR, buf) < 0)
        goto error;
    if (H5Sclose(space) < 0)
        goto error;
    if (H5Aclose(attr) < 0)
        goto error;

    dims[0] = 2;
    dims[1] = 2;
    if ((space = H5Screate_simple(2, dims, NULL)) < 0)
        goto error;
    if ((attr = H5Acreate2(group, "attr2", H5T_STD_I32BE, space, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;
    data[0][0] = 0;
    data[0][1] = 1;
    data[1][0] = 2;
    data[1][1] = 3;
    if (H5Awrite(attr, H5T_NATIVE_INT, data) < 0)
        goto error;
    if (H5Sclose(space) < 0)
        goto error;
    if (H5Aclose(attr) < 0)
        goto error;

    if (H5Gclose(group) < 0)
        goto error;

    if ((group = H5Gopen2(fid, "/g1/g1.1", H5P_DEFAULT)) < 0)
        goto error;

    /* Dataset 1.1.1 */
    dims[0] = 10;
    dims[1] = 10;
    if ((space = H5Screate_simple(2, dims, NULL)) < 0)
        goto error;
    if ((dataset =
             H5Dcreate2(group, "dset1.1.1", H5T_STD_I32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;
    for (i = 0; i < 10; i++)
        for (j = 0; j < 10; j++)
            dset1[i][j] = j * i;
    if (H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset1) < 0)
        goto error;
    if (H5Sclose(space) < 0)
        goto error;

    /* Attributes of dset1.1.1 */
    dims[0] = 27;
    if ((space = H5Screate_simple(1, dims, NULL)) < 0)
        goto error;
    if ((attr = H5Acreate2(dataset, "attr1", H5T_STD_I8BE, space, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;
    if (snprintf(buf, sizeof(buf), "1st attribute of dset1.1.1") < 0)
        goto error;
    if (H5Awrite(attr, H5T_NATIVE_SCHAR, buf) < 0)
        goto error;
    if (H5Sclose(space) < 0)
        goto error;
    if (H5Aclose(attr) < 0)
        goto error;

    dims[0] = 27;
    if ((space = H5Screate_simple(1, dims, NULL)) < 0)
        goto error;
    if ((attr = H5Acreate2(dataset, "attr2", H5T_STD_I8BE, space, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;
    if (snprintf(buf, sizeof(buf), "2nd attribute of dset1.1.1") < 0)
        goto error;
    if (H5Awrite(attr, H5T_NATIVE_SCHAR, buf) < 0)
        goto error;
    if (H5Sclose(space) < 0)
        goto error;
    if (H5Aclose(attr) < 0)
        goto error;

    if (H5Dclose(dataset) < 0)
        goto error;

    /* Dataset 1.1.2 */
    dims[0] = 20;
    if ((space = H5Screate_simple(1, dims, NULL)) < 0)
        goto error;
    if ((dataset =
             H5Dcreate2(group, "dset1.1.2", H5T_STD_I32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;
    for (i = 0; i < 20; i++)
        dset2[i] = i;
    if (H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2) < 0)
        goto error;
    if (H5Sclose(space) < 0)
        goto error;
    if (H5Dclose(dataset) < 0)
        goto error;

    if (H5Gclose(group) < 0)
        goto error;

    /* External link */
    if (H5Lcreate_external("somefile", "somepath", fid, "/g1/g1.2/extlink", H5P_DEFAULT, H5P_DEFAULT) < 0)
        goto error;

    /* Soft link */
    if ((group = H5Gopen2(fid, "/g1/g1.2/g1.2.1", H5P_DEFAULT)) < 0)
        goto error;
    if (H5Lcreate_soft("somevalue", group, "slink", H5P_DEFAULT, H5P_DEFAULT) < 0)
        goto error;
    if (H5Gclose(group) < 0)
        goto error;

    if ((group = H5Gopen2(fid, "/g2", H5P_DEFAULT)) < 0)
        goto error;

    /* Dataset 2.1 */
    dims[0] = 10;
    if ((space = H5Screate_simple(1, dims, NULL)) < 0)
        goto error;
    if ((dataset =
             H5Dcreate2(group, "dset2.1", H5T_IEEE_F32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;
    for (i = 0; i < 10; i++)
        dset2_1[i] = (float)((float)i * 0.1F + 1.0F);
    if (H5Dwrite(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2_1) < 0)
        goto error;
    if (H5Sclose(space) < 0)
        goto error;
    if (H5Dclose(dataset) < 0)
        goto error;

    /* Dataset 2.2 */
    dims[0] = 3;
    dims[1] = 5;
    if ((space = H5Screate_simple(2, dims, NULL)) < 0)
        goto error;
    if ((dataset =
             H5Dcreate2(group, "dset2.2", H5T_IEEE_F32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;
    for (i = 0; i < 3; i++)
        for (j = 0; j < 5; j++)
            dset2_2[i][j] = (float)(((float)i + 1.0F) * (float)j * 0.1F);
    if (H5Dwrite(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset2_2) < 0)
        goto error;
    if (H5Sclose(space) < 0)
        goto error;
    if (H5Dclose(dataset) < 0)
        goto error;

    if (H5Gclose(group) < 0)
        goto error;

    /* User-defined link */
    if (H5Lregister(UD_link_class) < 0)
        goto error;
    if (H5Lcreate_ud(fid, "/g2/udlink", (H5L_type_t)MY_LINKCLASS, NULL, (size_t)0, H5P_DEFAULT, H5P_DEFAULT) <
        0)
        goto error;

    /* MUST close the file ID before the user block code or you risk tripping
     * over file locking issues.
     */
    if (H5Fclose(fid) < 0)
        goto error;

    /* If a user block is being used, write to it here */
    if (ub_size > 0) {
        char *bp;

        if (ub_size > BUF_SIZE)
            goto error;

        if ((fd = HDopen(filename, O_RDWR)) < 0)
            goto error;

        /* Fill buf with pattern */
        memset(buf, '\0', ub_size);
        bp = buf;
        for (u = 0; u < ub_fill; u++)
            *bp++ = pattern[u % 10];

        if (HDwrite(fd, buf, ub_size) < 0)
            goto error;

        if (HDclose(fd) < 0)
            goto error;
    }

    return SUCCEED;

error:
    if (fd >= 0)
        HDclose(fd);

    H5E_BEGIN_TRY
    {
        H5Fclose(fid);
        H5Gclose(group);
        H5Aclose(attr);
        H5Dclose(dataset);
        H5Sclose(space);
        H5Pclose(create_plist);
    }
    H5E_END_TRY

    return FAIL;
}

/* Creates a simple (i.e., not HDF5) text file and fills it with a pattern */
herr_t
create_textfile(const char *name, size_t size)
{
    char  *buf = NULL;
    int    fd  = -1;
    size_t i;
    char  *bp = NULL;

    if ((fd = HDcreat(name, 0777)) < 0)
        goto error;
    if (NULL == (buf = (char *)calloc(size, 1)))
        goto error;

    /* Fill buf with pattern */
    bp = buf;
    for (i = 0; i < size; i++)
        *bp++ = pattern[i % 10];

    if (HDwrite(fd, buf, size) < 0)
        goto error;

    free(buf);
    HDclose(fd);

    return SUCCEED;

error:
    free(buf);
    if (fd >= 0)
        HDclose(fd);

    return FAIL;
}
