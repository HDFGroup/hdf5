/************************************************************

  This example shows the "reference to another dataset" pattern
  for filter blob configuration data (RFC-HDFG-2026-003), as opposed
  to the "arbitrarily large opaque payload" pattern shown by
  h5ex_d_blob_libpressio.c.

  It models ROIBIN-SZ (https://doi.org/10.1080/08940886.2023.2245722),
  cited by LibPressio's author as a motivating case for this API in
  a comment on https://github.com/HDFGroup/hdf5/issues/6153:
  ROIBIN-SZ stores a binary spatial mask indicating which values use
  lossless compression. The preferred design keeps that mask as its
  own user-visible HDF5 dataset rather than folding it into the
  filter's own configuration, with only a small, fixed-size
  *reference* -- the mask dataset's path -- stored in the filter's
  blob. The filter dereferences the reference at open time to
  recover the mask.

  Unlike h5ex_d_blob_libpressio.c, this needs no custom write_blob/
  read_blob/close_blob callbacks: the reference itself is small, so
  the library's default global-heap blob storage handles persisting
  and recovering it with no extra code, via plain H5Z_class2_t (the
  same struct used for filters that predate this RFC entirely). The
  only new step is dereferencing the recovered path with an ordinary
  H5Dopen2() -- there is nothing filter-callback-specific about it.

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define FILENAME          "h5ex_d_blob_maskref.h5"
#define DATASET           "filtered"
#define MASK_DATASET      "mask"
#define MASK_PATH         "/" MASK_DATASET
#define DIM0              4
#define DIM1              4
#define CHUNK0            2
#define CHUNK1            2
#define MASK_LEN          4
#define MASKREF_FILTER_ID 33001

/* -----------------------------------------------------------------
 * The filter itself: a pass-through. A real compressor would use the
 * mask it dereferences below (see main()) to decide, per element,
 * whether to apply lossless or lossy compression.
 * ----------------------------------------------------------------- */
static size_t
maskref_filter(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values, size_t nbytes,
               size_t *buf_size, void **buf)
{
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    (void)buf_size;
    (void)buf;
    return nbytes;
}

int
main(void)
{
    hid_t   file_id       = H5I_INVALID_HID;
    hid_t   space_id      = H5I_INVALID_HID;
    hid_t   dset_id       = H5I_INVALID_HID;
    hid_t   dcpl_id       = H5I_INVALID_HID;
    hid_t   dcpl_out      = H5I_INVALID_HID;
    hid_t   mask_space_id = H5I_INVALID_HID;
    hid_t   mask_dset_id  = H5I_INVALID_HID;
    herr_t  status;
    hsize_t dims[2]      = {DIM0, DIM1};
    hsize_t chunk[2]     = {CHUNK0, CHUNK1};
    hsize_t mask_dims[1] = {MASK_LEN};
    int     wdata[DIM0][DIM1];
    int     rdata[DIM0][DIM1];
    int     mask_wdata[MASK_LEN] = {1, 0, 0, 1}; /* 1 = lossless, 0 = lossy */
    int     mask_rdata[MASK_LEN];
    char    path_buf[64];
    size_t  path_size;
    hsize_t i, j;
    int     ret_value = 1;

    const H5Z_class2_t maskref_cls = {
        H5Z_CLASS_T_VERS,  /* H5Z_class_t version */
        MASKREF_FILTER_ID, /* filter id number */
        1,                 /* encoder_present */
        1,                 /* decoder_present */
        "maskref_example", /* filter name for debugging */
        NULL,              /* can_apply callback */
        NULL,              /* set_local callback */
        maskref_filter,    /* filter function */
    };

    /*
     * Initialize data.
     */
    for (i = 0; i < DIM0; i++)
        for (j = 0; j < DIM1; j++)
            wdata[i][j] = (int)(i * DIM1 + j);

    status = H5Zregister(&maskref_cls);
    if (status < 0)
        goto done;

    /*
     * Create a new file using the default properties.
     */
    file_id = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (file_id < 0)
        goto done;

    /*
     * The mask lives as its own ordinary, user-visible dataset --
     * nothing filter-specific about it.
     */
    mask_space_id = H5Screate_simple(1, mask_dims, NULL);
    if (mask_space_id < 0)
        goto done;
    mask_dset_id = H5Dcreate2(file_id, MASK_DATASET, H5T_NATIVE_INT, mask_space_id, H5P_DEFAULT, H5P_DEFAULT,
                              H5P_DEFAULT);
    if (mask_dset_id < 0)
        goto done;
    status = H5Dwrite(mask_dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, mask_wdata);
    if (status < 0)
        goto done;
    H5Dclose(mask_dset_id);
    mask_dset_id = -1;

    /*
     * Create dataspace and DCPL, then attach the mask's *path* -- not
     * its data -- as the filter's blob. This is the whole migration:
     * a small, fixed-size reference travels in the pipeline message
     * instead of a copy of the mask itself.
     */
    space_id = H5Screate_simple(2, dims, NULL);
    if (space_id < 0)
        goto done;

    dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
    if (dcpl_id < 0)
        goto done;

    status = H5Pset_chunk(dcpl_id, 2, chunk);
    if (status < 0)
        goto done;

    printf("Attaching mask reference \"%s\" via H5Pappend_filter_blob...\n", MASK_PATH);
    status = H5Pappend_filter_blob(dcpl_id, MASKREF_FILTER_ID, 0, MASK_PATH, strlen(MASK_PATH) + 1);
    if (status < 0)
        goto done;

    dset_id = H5Dcreate2(file_id, DATASET, H5T_NATIVE_INT, space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    if (dset_id < 0)
        goto done;

    status = H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata[0]);
    if (status < 0)
        goto done;

    H5Dclose(dset_id);
    dset_id = -1;
    H5Pclose(dcpl_id);
    dcpl_id = -1;
    H5Sclose(space_id);
    space_id = -1;
    H5Sclose(mask_space_id);
    mask_space_id = -1;
    H5Fclose(file_id);
    file_id = -1;

    printf("....Close the file and reopen for reading ........\n");

    /*
     * Now we begin the read section of this example.
     */
    file_id = H5Fopen(FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT);
    if (file_id < 0)
        goto done;

    dset_id = H5Dopen(file_id, DATASET, H5P_DEFAULT);
    if (dset_id < 0)
        goto done;

    status = H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata[0]);
    if (status < 0) {
        printf("failed to read data.\n");
        goto done;
    }
    for (i = 0; i < DIM0; i++)
        for (j = 0; j < DIM1; j++)
            if (rdata[i][j] != wdata[i][j]) {
                printf("Data verification FAILED at [%llu][%llu]\n", (unsigned long long)i,
                       (unsigned long long)j);
                goto done;
            }
    printf("Data verification succeeded.\n");

    /*
     * Dereference: recover the reference via H5Pget_filter_blob() --
     * the same public getter used for opaque blobs, no special-casing
     * needed for the fact that this one happens to be a path -- then
     * open the mask by that path with an ordinary H5Dopen2(). This is
     * the entire "reference to another dataset" pattern; nothing
     * about it is filter-callback machinery.
     */
    dcpl_out = H5Dget_create_plist(dset_id);
    if (dcpl_out < 0)
        goto done;

    path_size = sizeof(path_buf);
    status    = H5Pget_filter_blob(dcpl_out, 0, 0, path_buf, &path_size);
    if (status < 0 || path_size >= sizeof(path_buf)) {
        printf("Failed to recover the mask reference.\n");
        goto done;
    }
    path_buf[path_size] = '\0';
    printf("Recovered mask reference: \"%s\"\n", path_buf);

    mask_dset_id = H5Dopen2(file_id, path_buf, H5P_DEFAULT);
    if (mask_dset_id < 0) {
        printf("Failed to open the referenced mask dataset.\n");
        goto done;
    }
    status = H5Dread(mask_dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, mask_rdata);
    if (status < 0)
        goto done;
    for (i = 0; i < MASK_LEN; i++)
        if (mask_rdata[i] != mask_wdata[i]) {
            printf("Mask verification FAILED at [%llu]\n", (unsigned long long)i);
            goto done;
        }
    printf("Dereferenced mask matches what was written -- Use Case B round-trip succeeded.\n");

    ret_value = 0;

done:
    /*
     * Close and release resources.
     */
    if (dcpl_out >= 0)
        H5Pclose(dcpl_out);
    if (dcpl_id >= 0)
        H5Pclose(dcpl_id);
    if (mask_dset_id >= 0)
        H5Dclose(mask_dset_id);
    if (dset_id >= 0)
        H5Dclose(dset_id);
    if (mask_space_id >= 0)
        H5Sclose(mask_space_id);
    if (space_id >= 0)
        H5Sclose(space_id);
    if (file_id >= 0)
        H5Fclose(file_id);

    return ret_value;
}
