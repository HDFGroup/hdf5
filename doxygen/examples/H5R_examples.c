/* -*- c-file-style: "stroustrup" -*- */

#include "hdf5.h"

#include <stdio.h>
#include <stdlib.h>

int
main(void)
{
    int ret_val = EXIT_SUCCESS;

    //! <!-- [create] -->
    {
        __label__ fail_file, fail_fspace, fail_dset, fail_sel, fail_aspace, fail_attr, fail_awrite;
        hid_t  file, fspace, dset, aspace, attr;
        H5R_ref_t ref;

        if ((file = H5Fcreate("reference.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }
        // create a region reference which selects all elements of the dataset at "/data"
        if ((fspace = H5Screate_simple(2, (hsize_t[]){10, 20}, NULL)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_fspace;
        }
        if ((dset = H5Dcreate(file, "data", H5T_STD_I32LE, fspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dset;
        }
        if (H5Sselect_all(fspace) < 0 ||
            H5Rcreate_region(file, "data", fspace, H5P_DEFAULT, &ref) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_sel;
        }
        // store the region reference in a scalar attribute of the root group called "region"
        if ((aspace = H5Screate(H5S_SCALAR))== H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_aspace;
        }
        if ((attr = H5Acreate(file, "region", H5T_STD_REF, aspace, H5P_DEFAULT, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_attr;
        }
        if (H5Awrite(attr, H5T_STD_REF, &ref) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_awrite;
        }

fail_awrite:
        H5Aclose(attr);
fail_attr:
        H5Sclose(aspace);
fail_aspace:
        H5Rdestroy(&ref);
fail_sel:
        H5Dclose(dset);
fail_dset:
        H5Sclose(fspace);
fail_fspace:
        H5Fclose(file);
fail_file:;
    }
    //! <!-- [create] -->

    //! <!-- [read] -->
    {
        // show how to query/de-reference a reference
    } //! <!-- [read] -->

    //! <!-- [update] -->
    {
        // show how to update a reference (?)
    } //! <!-- [update] -->

    //! <!-- [delete] -->
    {
        // show how to destroy a reference
    }
    //! <!-- [delete] -->

    return ret_val;
}
