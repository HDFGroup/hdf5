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

/*
 * Small program to illustrate the "misalignment" of members within a compound
 * datatype, in a datatype fixed by H5Tget_native_type().
 */

#include "hdf5.h"
#include "H5private.h"
#include "h5tools.h"

const char *fname   = "talign.h5";
const char *setname = "align";

/*
 * This program assumes that there is no extra space between the members 'Ok'
 * and 'Not Ok', (there shouldn't be because they are of the same atomic type
 * H5T_NATIVE_FLOAT, and they are placed within the compound next to one
 * another per construction)
 */

int
main(void)
{
    hid_t fil = H5I_INVALID_HID, spc = H5I_INVALID_HID, set = H5I_INVALID_HID;
    hid_t cs6 = H5I_INVALID_HID, cmp = H5I_INVALID_HID, fix = H5I_INVALID_HID;
    hid_t cmp1 = H5I_INVALID_HID, cmp2 = H5I_INVALID_HID, cmp3 = H5I_INVALID_HID;
    hid_t plist    = H5I_INVALID_HID;
    hid_t array_dt = H5I_INVALID_HID;

    hsize_t dim[2];
    hsize_t cdim[4];

    char   string5[5];
    float  fok[2]  = {1234.0f, 2341.0f};
    float  fnok[2] = {5678.0f, 6785.0f};
    float *fptr    = NULL;

    char *data = NULL;

    int    result = 0;
    herr_t error  = 1;

    HDprintf("%-70s", "Testing alignment in compound datatypes");

    HDstrcpy(string5, "Hi!");
    HDunlink(fname);
    fil = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    if (fil < 0) {
        HDputs("*FAILED*");
        return 1;
    }

    H5E_BEGIN_TRY
    {
        (void)H5Ldelete(fil, setname, H5P_DEFAULT);
    }
    H5E_END_TRY;

    cs6 = H5Tcopy(H5T_C_S1);
    H5Tset_size(cs6, sizeof(string5));
    H5Tset_strpad(cs6, H5T_STR_NULLPAD);

    cmp = H5Tcreate(H5T_COMPOUND, sizeof(fok) + sizeof(string5) + sizeof(fnok));
    H5Tinsert(cmp, "Awkward length", 0, cs6);

    cdim[0]  = sizeof(fok) / sizeof(float);
    array_dt = H5Tarray_create2(H5T_NATIVE_FLOAT, 1, cdim);
    H5Tinsert(cmp, "Ok", sizeof(string5), array_dt);
    H5Tclose(array_dt);

    cdim[0]  = sizeof(fnok) / sizeof(float);
    array_dt = H5Tarray_create2(H5T_NATIVE_FLOAT, 1, cdim);
    H5Tinsert(cmp, "Not Ok", sizeof(fok) + sizeof(string5), array_dt);
    H5Tclose(array_dt);

    fix  = H5Tget_native_type(cmp, H5T_DIR_DEFAULT);
    cmp1 = H5Tcreate(H5T_COMPOUND, sizeof(fok));

    cdim[0]  = sizeof(fok) / sizeof(float);
    array_dt = H5Tarray_create2(H5T_NATIVE_FLOAT, 1, cdim);
    H5Tinsert(cmp1, "Ok", 0, array_dt);
    H5Tclose(array_dt);

    cmp2 = H5Tcreate(H5T_COMPOUND, sizeof(string5));
    H5Tinsert(cmp2, "Awkward length", 0, cs6);

    cmp3 = H5Tcreate(H5T_COMPOUND, sizeof(fnok));

    cdim[0]  = sizeof(fnok) / sizeof(float);
    array_dt = H5Tarray_create2(H5T_NATIVE_FLOAT, 1, cdim);
    H5Tinsert(cmp3, "Not Ok", 0, array_dt);
    H5Tclose(array_dt);

    plist = H5Pcreate(H5P_DATASET_XFER);
    if ((error = H5Pset_preserve(plist, 1)) < 0)
        goto out;

    /*
     * Create a small dataset, and write data into it we write each field
     * in turn so that we are avoid alignment issues at this point
     */
    dim[0] = 1;
    spc    = H5Screate_simple(1, dim, NULL);
    set    = H5Dcreate2(fil, setname, cmp, spc, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    H5Dwrite(set, cmp1, spc, H5S_ALL, plist, fok);
    H5Dwrite(set, cmp2, spc, H5S_ALL, plist, string5);
    H5Dwrite(set, cmp3, spc, H5S_ALL, plist, fnok);

    H5Dclose(set);

    /* Now open the set, and read it back in */
    data = (char *)HDmalloc(H5Tget_size(fix));

    if (!data) {
        HDperror("malloc() failed");
        HDabort();
    }

    set = H5Dopen2(fil, setname, H5P_DEFAULT);

    H5Dread(set, fix, spc, H5S_ALL, H5P_DEFAULT, data);
    fptr = (float *)((void *)(data + H5Tget_member_offset(fix, 1)));
    H5Dclose(set);

out:
    if (error < 0) {
        result = 1;
        HDputs("*FAILED - HDF5 library error*");
    }
    else if (!(H5_FLT_ABS_EQUAL(fok[0], fptr[0])) || !(H5_FLT_ABS_EQUAL(fok[1], fptr[1])) ||
             !(H5_FLT_ABS_EQUAL(fnok[0], fptr[2])) || !(H5_FLT_ABS_EQUAL(fnok[1], fptr[3]))) {
        char *mname;

        result = 1;
        mname  = H5Tget_member_name(fix, 0);
        HDprintf("%14s (%2d) %6s = %s\n", mname ? mname : "(null)", (int)H5Tget_member_offset(fix, 0),
                 string5, (char *)(data + H5Tget_member_offset(fix, 0)));
        if (mname)
            H5free_memory(mname);

        fptr  = (float *)((void *)(data + H5Tget_member_offset(fix, 1)));
        mname = H5Tget_member_name(fix, 1);
        HDprintf("Data comparison:\n"
                 "%14s (%2d) %6f = %f\n"
                 "                    %6f = %f\n",
                 mname ? mname : "(null)", (int)H5Tget_member_offset(fix, 1), (double)fok[0], (double)fptr[0],
                 (double)fok[1], (double)fptr[1]);
        if (mname)
            H5free_memory(mname);

        fptr  = (float *)((void *)(data + H5Tget_member_offset(fix, 2)));
        mname = H5Tget_member_name(fix, 2);
        HDprintf("%14s (%2d) %6f = %f\n"
                 "                    %6f = %6f\n",
                 mname ? mname : "(null)", (int)H5Tget_member_offset(fix, 2), (double)fnok[0],
                 (double)fptr[0], (double)fnok[1], (double)fptr[1]);
        if (mname)
            H5free_memory(mname);

        fptr = (float *)((void *)(data + H5Tget_member_offset(fix, 1)));
        HDprintf("\n"
                 "Short circuit\n"
                 "                    %6f = %f\n"
                 "                    %6f = %f\n"
                 "                    %6f = %f\n"
                 "                    %6f = %f\n",
                 (double)fok[0], (double)fptr[0], (double)fok[1], (double)fptr[1], (double)fnok[0],
                 (double)fptr[2], (double)fnok[1], (double)fptr[3]);
        HDputs("*FAILED - compound type alignmnent problem*");
    }
    else {
        HDputs(" PASSED");
    }

    if (data)
        HDfree(data);
    H5Sclose(spc);
    H5Tclose(cs6);
    H5Tclose(cmp);
    H5Tclose(fix);
    H5Tclose(cmp1);
    H5Tclose(cmp2);
    H5Tclose(cmp3);
    H5Pclose(plist);
    H5Fclose(fil);
    HDunlink(fname);
    HDfflush(stdout);
    return result;
}
