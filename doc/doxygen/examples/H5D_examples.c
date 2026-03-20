/* -*- c-file-style: "stroustrup" -*- */

#include "hdf5.h"

#include <stdio.h>
#include <stdlib.h>

//! <!-- [H5Dchunk_iter_cb] -->
int
chunk_cb(const hsize_t *offset, unsigned filter_mask, haddr_t addr, hsize_t size, void *op_data)
{
    // only print the allocated chunk size only
    printf("%" PRIuHSIZE "\n", size);
    return EXIT_SUCCESS;
}
//! <!-- [H5Dchunk_iter_cb] -->

//! <!-- [H5Ovisit_cb] -->
herr_t
H5Ovisit_cb(hid_t obj, const char *name, const H5O_info2_t *info, void *op_data)
{
    herr_t retval    = 0;
    char  *base_path = (char *)op_data;

    if (info->type == H5O_TYPE_DATASET) // current object is a dataset
    {
        hid_t dset, dcpl;
        if ((dset = H5Dopen(obj, name, H5P_DEFAULT)) == H5I_INVALID_HID) {
            retval = -1;
            goto func_leave;
        }
        if ((dcpl = H5Dget_create_plist(dset)) == H5I_INVALID_HID) {
            retval = -1;
            goto fail_dcpl;
        }
        if (H5Pget_layout(dcpl) == H5D_CHUNKED) // dataset is chunked
        {
            __label__ fail_dtype, fail_dspace, fail_shape;
            hid_t   dspace, dtype;
            size_t  size, i;
            int     rank;
            hsize_t cdims[H5S_MAX_RANK];

            // get resources
            if ((dtype = H5Dget_type(dset)) < 0) {
                retval = -1;
                goto fail_dtype;
            }
            if ((dspace = H5Dget_space(dset)) < 0) {
                retval = -1;
                goto fail_dspace;
            }
            // get the shape
            if ((size = H5Tget_size(dtype)) == 0 || (rank = H5Sget_simple_extent_ndims(dspace)) < 0 ||
                H5Pget_chunk(dcpl, H5S_MAX_RANK, cdims) < 0) {
                retval = -1;
                goto fail_shape;
            }
            // calculate the nominal chunk size
            size = 1;
            for (i = 0; i < (size_t)rank; ++i)
                size *= cdims[i];
            // print dataset info
            printf("%s%s : nominal chunk size %lu [B] \n", base_path, name, size);
            // get the allocated chunk sizes
            if (H5Dchunk_iter(dset, H5P_DEFAULT, &chunk_cb, NULL) < 0) {
                retval = -1;
                goto fail_fig;
            }
fail_fig:
fail_shape:
            H5Sclose(dspace);
fail_dspace:
            H5Tclose(dtype);
fail_dtype:;
        }

        H5Pclose(dcpl);
fail_dcpl:
        H5Dclose(dset);
    }

func_leave:
    return retval;
}
//! <!-- [H5Ovisit_cb] -->

int
main(void)
{
    int ret_val = EXIT_SUCCESS;

    //! <!-- [create] -->
    {
        __label__ fail_lcpl, fail_dset, fail_file;
        hid_t file, lcpl, fspace, dset;

        unsigned mode        = H5F_ACC_TRUNC;
        char     file_name[] = "d1.h5";
        // link names can be arbitrary Unicode strings
        char dset_name[] = "σύνολο/δεδομένων";

        if ((file = H5Fcreate(file_name, mode, H5P_DEFAULT, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }
        if ((lcpl = H5Pcreate(H5P_LINK_CREATE)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_lcpl;
        }
        // use UTF-8 encoding for link names
        if (H5Pset_char_encoding(lcpl, H5T_CSET_UTF8) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_fspace;
        }
        // create intermediate groups as needed
        if (H5Pset_create_intermediate_group(lcpl, 1) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_fspace;
        }
        // create a 1D dataspace
        if ((fspace = H5Screate_simple(1, (hsize_t[]){10}, NULL)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_fspace;
        }
        // create a 32-bit integer dataset
        if ((dset = H5Dcreate2(file, dset_name, H5T_STD_I32LE, fspace, lcpl, H5P_DEFAULT, H5P_DEFAULT)) ==
            H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dset;
        }

        H5Dclose(dset);
fail_dset:
        H5Sclose(fspace);
fail_fspace:
        H5Pclose(lcpl);
fail_lcpl:
        H5Fclose(file);
fail_file:;
    }
    //! <!-- [create] -->

    //! <!-- [read] -->
    {
        __label__ fail_dset, fail_file;
        hid_t file, dset;

        unsigned mode        = H5F_ACC_RDONLY;
        char     file_name[] = "d1.h5";
        // assume a priori knowledge of dataset name and size
        char dset_name[] = "σύνολο/δεδομένων";
        int  elts[10];

        if ((file = H5Fopen(file_name, mode, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }
        if ((dset = H5Dopen2(file, dset_name, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dset;
        }
        // read all dataset elements
        if (H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, elts) < 0)
            ret_val = EXIT_FAILURE;

        // do something w/ the dataset elements

        H5Dclose(dset);
fail_dset:
        H5Fclose(file);
fail_file:;
    }
    //! <!-- [read] -->

    //! <!-- [update] -->
    {
        __label__ fail_update, fail_fspace, fail_dset, fail_file;
        hid_t file, dset, fspace;

        unsigned mode           = H5F_ACC_RDWR;
        char     file_name[]    = "d1.h5";
        char     dset_name[]    = "σύνολο/δεδομένων";
        int      new_elts[6][2] = {{-1, 1}, {-2, 2}, {-3, 3}, {-4, 4}, {-5, 5}, {-6, 6}};

        if ((file = H5Fopen(file_name, mode, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }
        if ((dset = H5Dopen2(file, dset_name, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dset;
        }
        // get the dataset's dataspace
        if ((fspace = H5Dget_space(dset)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_fspace;
        }
        // select the first 5 elements in odd positions
        if (H5Sselect_hyperslab(fspace, H5S_SELECT_SET, (hsize_t[]){1}, (hsize_t[]){2}, (hsize_t[]){5},
                                NULL) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_update;
        }

        // (implicitly) select and write the first 5 elements of the second column of NEW_ELTS
        if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, fspace, H5P_DEFAULT, new_elts) < 0)
            ret_val = EXIT_FAILURE;

fail_update:
        H5Sclose(fspace);
fail_fspace:
        H5Dclose(dset);
fail_dset:
        H5Fclose(file);
fail_file:;
    }
    //! <!-- [update] -->

    //! <!-- [delete] -->
    {
        __label__ fail_delete, fail_file;
        hid_t file;

        unsigned mode         = H5F_ACC_RDWR;
        char     file_name[]  = "d1.h5";
        char     group_name[] = "σύνολο";
        char     dset_name[]  = "σύνολο/δεδομένων";

        if ((file = H5Fopen(file_name, mode, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }
        // delete (unlink) the dataset
        if (H5Ldelete(file, dset_name, H5P_DEFAULT) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_delete;
        }
        // the previous call deletes (unlinks) only the dataset
        if (H5Ldelete(file, group_name, H5P_DEFAULT) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_delete;
        }

fail_delete:
        H5Fclose(file);
fail_file:;
    }
    //! <!-- [delete] -->

    return ret_val;
}
