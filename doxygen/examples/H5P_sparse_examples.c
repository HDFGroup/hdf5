/* -*- c-file-style: "stroustrup" -*- */

#include "hdf5.h"

#include <stdio.h>
#include <stdlib.h>

int
main(void)
{
    int ret_val = EXIT_SUCCESS;

    //! <!-- [struct_chunk_filter] -->
    {
        __label__ fail_file, fail_space, fail_dcpl, fail_set_chunk, __label__ ail_set_layout,
            fail_set_filter2, fail_dcreate;

        hid_t   fid, sid, did, dcpl;
        char    file_name[]   = "sparse.h5";
        char    dset_name[]   = "sparse_filter_dset";
        hsize_t dims[2]       = {10, 10};
        hsize_t chunk_dims[2] = {5, 5};

        // Open the file
        if ((fid = H5Fopen(file_name, H5F_ACC_RDWR, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }

        // Create a 2D dataspace
        if ((sid = H5Screate_simple(2, dim, NULL)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_space;
        }

        // Create a dataset creation property list
        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dcpl;
        }

        // Set dataset layout to sparse chunk */
        if (H5Pset_layout(dcpl, H5D_SPARSE_CHUNK) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_set_layout;
        }

        // Set to sparse chunked dataset
        if (H5Pset_struct_chunk(dcpl, 1, chunk_dim, H5D_SPARSE_CHUNK) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_set_chunk;
        }

        // Adds a filter to section 0
        if (H5Pset_filter2(dcpl, H5Z_FLAG_SPARSE_SELECTION, H5Z_FILTER_DEFLATE, H5Z_FLAG_MANDATORY, 0, NULL) <
            0) {
            ret_val = EXIT_FAILURE;
            goto fail_set_filter2;
        }

        // Create an integer 2-d dataset with sparse chunk layout
        if ((did = H5Dcreate2(fid, dset_name, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) ==
            H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dcreate;
        }

        // Verify the number of filters is correct
        if ((nfilters = H5Pget_nfilters2(dcpl, H5Z_FLAG_SPARSE_SELECTION)) != 1)
            ret_val = EXIT_FAILURE;

        H5Dclose(did);

fail_dcreate:
fail_set_layout:
fail_set_chunk:
fail_set_filter2:
        H5Pclose(dcpl);

fail_dcpl:
        H5Sclose(sid);

fail_space:
        H5Fclose(fid);

fail_file:;
    }
    //! <!-- [struct_chunk_filter] -->

    //! <!-- [struct_chunk_filter2] -->
    {
        __label__ fail_file, fail_dopen, fail_dget_plist, fail_pget_filter3;
        __label__ fail_pget_mfilter2, fail_pget_filter_id3, fail_pget_remove_filter2;

        hid_t fid, sid, did, dcpl;
        char  file_name[] = "sparse.h5";
        char  dset_name[] = "sparse_filter_dset";

        // Open the file
        if ((fid = H5Fopen(file_name, H5F_ACC_RDWR, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }

        if ((did = H5Dopen2(fid, SPARSE_DSET, H5P_DEFAULT)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_dopen;
        }

        if ((dcpl = H5Dget_create_plist(did)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_dget_plist;
        }

        filtn = H5Pget_filter3(dcpl, H5Z_FLAG_SPARSE_SELECTION, 0, &flags, NULL, NULL, (size_t)0, NULL, NULL);
        if (filtn != H5Z_FILTER_DEFLATE && flags != H5Z_FLAG_MANDATORY) {
            ret_val = EXIT_FAILURE;
            goto fail_pget_filter3;
        }

        if (H5Pmodify_filter2(dcpl, H5Z_FLAG_SPARSE_SELECTION, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, 0,
                              NULL) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_pmod_filter2;
        }

        if (H5Pget_filter_by_id3(dcpl, H5Z_FLAG_SPARSE_SELECTION, H5Z_FILTER_DEFLATE, &flags, NULL, NULL, 0,
                                 NULL, NULL) < 0) {

            ret_val = EXIT_FAILURE;
            goto fail_pget_filter_id3;
        }

        if (flags != H5Z_FLAG_OPTIONAL) {
            ret_val = EXIT_FAILURE;
            goto fail_pget_filter_id3;
        }

        if (H5Premove_filter2(dcpl, H5Z_FLAG_SPARSE_SELECTION, H5Z_FILTER_DEFLATE) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_remove_filter2;
        }

        filtn = H5Pget_filter3(dcpl, H5Z_FLAG_SPARSE_SELECTION, 0, NULL, NULL, NULL, (size_t)0, NULL, NULL);
        if (filtn != H5Z_FILTER_NONE)
            ret_val = EXIT_FAILURE;

fail_pget_remove_filter2:
fail_pget_filter_id3:
fail_pget_mfilter2:
fail_pget_filter3:
        H5Pclose(dcpl);

fail_dget_plist:
        H5Dclose(did);

fail_dopen:
        H5Fclose(fid);

fail_file:;
    }
    //! <!-- [struct_chunk_filter2] -->

    return ret_val;
}
