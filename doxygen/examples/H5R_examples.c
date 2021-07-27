/* -*- c-file-style: "stroustrup" -*- */

#include "hdf5.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int
main(void)
{
    int ret_val = EXIT_SUCCESS;

    //! <!-- [create] -->
    {
        __label__ fail_file, fail_fspace, fail_dset, fail_sel, fail_aspace, fail_attr, fail_awrite;
        hid_t     file, fspace, dset, aspace, attr;
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
        if ((dset = H5Dcreate(file, "data", H5T_STD_I32LE, fspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) ==
            H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dset;
        }
        if (H5Sselect_all(fspace) < 0 || H5Rcreate_region(file, "data", fspace, H5P_DEFAULT, &ref) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_sel;
        }
        // store the region reference in a scalar attribute of the root group called "region"
        if ((aspace = H5Screate(H5S_SCALAR)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_aspace;
        }
        if ((attr = H5Acreate(file, "region", H5T_STD_REF, aspace, H5P_DEFAULT, H5P_DEFAULT)) ==
            H5I_INVALID_HID) {
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
        __label__ fail_file, fail_attr, fail_aread;
        hid_t     file, attr;
        H5R_ref_t ref;

        if ((file = H5Fopen("reference.h5", H5F_ACC_RDONLY, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }

        // read the dataset region reference from the attribute
        if ((attr = H5Aopen(file, "region", H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_attr;
        }
        if (H5Aread(attr, H5T_STD_REF, &ref) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_aread;
        }
        assert(H5Rget_type(&ref) == H5R_DATASET_REGION2);

        // get an HDF5 path name for the dataset of the region reference
        {
            char buf[255];
            if (H5Rget_obj_name(&ref, H5P_DEFAULT, buf, 255) < 0) {
                ret_val = EXIT_FAILURE;
            }
            printf("Object name: \"%s\"\n", buf);
        }

        H5Rdestroy(&ref);
fail_aread:
        H5Aclose(attr);
fail_attr:
        H5Fclose(file);
fail_file:;
    }
    //! <!-- [read] -->

    //! <!-- [update] -->
    {
        __label__ fail_file, fail_attr, fail_ref;
        hid_t     file, attr;
        H5R_ref_t ref;

        if ((file = H5Fopen("reference.h5", H5F_ACC_RDWR, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }

        // H5T_STD_REF is a generic reference type
        // we can "update" the attribute value to refer to the attribute itself
        if ((attr = H5Aopen(file, "region", H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_attr;
        }
        if (H5Rcreate_attr(file, "data", "region", H5P_DEFAULT, &ref) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_ref;
        }

        assert(H5Rget_type(&ref) == H5R_ATTR);

        if (H5Awrite(attr, H5T_STD_REF, &ref) < 0) {
            ret_val = EXIT_FAILURE;
        }

        H5Rdestroy(&ref);
fail_ref:
        H5Aclose(attr);
fail_attr:
        H5Fclose(file);
fail_file:;
    }
    //! <!-- [update] -->

    //! <!-- [delete] -->
    {
        __label__ fail_file, fail_ref;
        hid_t     file;
        H5R_ref_t ref;

        // create an HDF5 object reference to the root group
        if ((file = H5Fopen("reference.h5", H5F_ACC_RDONLY, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }
        if (H5Rcreate_object(file, ".", H5P_DEFAULT, &ref) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_ref;
        }

        // H5Rdestroy() releases all resources associated with an HDF5 reference
        H5Rdestroy(&ref);
fail_ref:
        H5Fclose(file);
fail_file:;
    }
    //! <!-- [delete] -->

    return ret_val;
}
