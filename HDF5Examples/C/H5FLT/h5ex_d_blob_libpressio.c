/************************************************************

  This example shows how a filter with a LibPressio-shaped
  configuration -- a small, fixed-size summary plus an arbitrarily
  large "compressor options" blob -- can use H5Pappend_filter_blob()
  and a filter's write_blob/read_blob callbacks (RFC-HDFG-2026-003)
  instead of hand-packing everything into cd_values.

  It is modeled directly on LibPressio's actual production HDF5
  filter:

    https://github.com/robertu94/libpressio/blob/master/tools/hdf5_filter/src/libpressio_hdf5_filter.cc

  That filter's H5Z_libpressio_set_local() computes the dataset's
  datatype and chunk dimensions and hand-packs them into cd_values
  alongside the compressor's *entire* options bag, which it
  msgpack-serializes itself (get_cd_values_from_options()). Its
  author reported that this "cause[s] segfaults when attempting to
  store in CD values over a few KB" -- see
  https://github.com/HDFGroup/hdf5/issues/6153 (comment from
  @robertu94) -- citing two concrete cases that motivated
  RFC-HDFG-2026-003: ROIBIN-SZ's binary spatial mask, and SZ4's
  (still unreleased) JIT compiler needing to store pre-processed
  source. Both are multi-megabyte blobs, far past what cd_values can
  safely hold.

  This example keeps the part of that design that was never the
  problem -- set_local still packs the small, fixed-size datatype
  class + chunk dimensions into cd_values -- but moves the
  arbitrarily large "compressor options" onto the new blob
  mechanism: attached once via H5Pappend_filter_blob() before
  H5Dcreate, and recovered inside set_local() with
  H5Pget_filter_blob() instead of being serialized into cd_values by
  the filter itself.

  The filter's actual data transform is a no-op pass-through: the
  point of this example is the *configuration* path LibPressio's
  author flagged as broken, not a real compression algorithm.

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define FILENAME             "h5ex_d_blob_libpressio.h5"
#define DATASET              "DS1"
#define DIM0                 8
#define DIM1                 8
#define CHUNK0               4
#define CHUNK1               4
#define LIBPRESSIO_FILTER_ID 33000
/* Stands in for a msgpack-serialized compressor options bag (or, for
 * the SZ4-shaped case, pre-processed JIT source). 256 KiB is already
 * well past cd_values' practical ceiling; real options blobs for
 * these use cases run into the multiple megabytes. */
#define OPTIONS_BLOB_SIZE (256 * 1024)

/* -----------------------------------------------------------------
 * The filter itself: a pass-through. A real compressor would use
 * cd_values[0]/[1..] (datatype class, chunk dims) and its own
 * out-of-band copy of the options blob (parsed once, e.g. in
 * set_local, and cached by the plugin) to actually compress/
 * decompress here.
 * ----------------------------------------------------------------- */
static size_t
libpressio_filter(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values, size_t nbytes,
                  size_t *buf_size, void **buf)
{
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    (void)buf_size;
    (void)buf;
    return nbytes;
}

/* -----------------------------------------------------------------
 * cd_values-packing filters like the real LibPressio one know their
 * own filter ID at compile time but not their position in a
 * (possibly multi-filter) pipeline -- H5Pget_filter_blob() is
 * index-based, so set_local has to find itself first.
 * ----------------------------------------------------------------- */
static herr_t
libpressio_find_self(hid_t dcpl_id, unsigned *idx_out)
{
    int      nfilters = H5Pget_nfilters(dcpl_id);
    unsigned i;

    if (nfilters < 0)
        return -1;
    for (i = 0; i < (unsigned)nfilters; i++) {
        unsigned     flags;
        size_t       cd_nelmts = 0;
        H5Z_filter_t id        = H5Pget_filter2(dcpl_id, i, &flags, &cd_nelmts, NULL, 0, NULL, NULL);

        if (id == LIBPRESSIO_FILTER_ID) {
            *idx_out = i;
            return 0;
        }
    }
    return -1;
}

/* -----------------------------------------------------------------
 * Modeled on H5Z_libpressio_set_local(): computes the datatype class
 * and chunk dimensions from the chunk dataspace exactly as the real
 * filter does. Where the real filter would msgpack-serialize the
 * full compressor options and hand-pack them into cd_values here, it
 * instead just confirms the options blob attached via
 * H5Pappend_filter_blob is present -- a real filter would parse it
 * at this point (msgpack-decode it, or for a hypothetical
 * SZ4-shaped filter, JIT-compile the embedded source once per
 * dataset here rather than per chunk).
 * ----------------------------------------------------------------- */
static herr_t
libpressio_set_local(hid_t dcpl_id, hid_t type_id, hid_t chunk_space_id)
{
    unsigned    idx;
    unsigned    flags;
    size_t      cd_nelmts_cur = 0;
    int         chunk_ndims;
    hsize_t     dims[32];
    unsigned    cd_values[34]; /* datatype class + ndims + up to 32 dims */
    size_t      n         = 0;
    size_t      blob_size = 0;
    H5T_class_t dclass;
    int         i;

    if (libpressio_find_self(dcpl_id, &idx) < 0)
        return -1;

    /* The compressor-options blob attached at H5Pappend_filter_blob
     * time -- the multi-megabyte piece the real filter would have
     * tried to jam into cd_values. */
    if (H5Pget_filter_blob(dcpl_id, idx, 0, NULL, &blob_size) < 0)
        return -1;
    if (blob_size == 0)
        return -1; /* this filter requires options */

    if ((chunk_ndims = H5Sget_simple_extent_ndims(chunk_space_id)) < 0)
        return -1;
    if ((size_t)chunk_ndims > sizeof(dims) / sizeof(dims[0]))
        return -1;
    if (H5Sget_simple_extent_dims(chunk_space_id, dims, NULL) < 0)
        return -1;

    if ((dclass = H5Tget_class(type_id)) == H5T_NO_CLASS)
        return -1;

    cd_values[n++] = (unsigned)dclass;
    cd_values[n++] = (unsigned)chunk_ndims;
    for (i = 0; i < chunk_ndims; i++)
        cd_values[n++] = (unsigned)dims[i];

    if (H5Pget_filter_by_id2(dcpl_id, LIBPRESSIO_FILTER_ID, &flags, &cd_nelmts_cur, NULL, 0, NULL, NULL) < 0)
        return -1;
    if (H5Pmodify_filter(dcpl_id, LIBPRESSIO_FILTER_ID, flags, n, cd_values) < 0)
        return -1;

    return 0;
}

int
main(void)
{
    hid_t          file_id  = H5I_INVALID_HID;
    hid_t          space_id = H5I_INVALID_HID;
    hid_t          dset_id  = H5I_INVALID_HID;
    hid_t          dcpl_id  = H5I_INVALID_HID;
    hid_t          dcpl_out = H5I_INVALID_HID;
    herr_t         status;
    hsize_t        dims[2]  = {DIM0, DIM1};
    hsize_t        chunk[2] = {CHUNK0, CHUNK1};
    int            wdata[DIM0][DIM1];
    int            rdata[DIM0][DIM1];
    unsigned char *options_blob = NULL;
    unsigned char *blob_out     = NULL;
    unsigned       flags;
    size_t         cd_nelmts;
    unsigned       cd_values[34];
    size_t         got_size;
    hsize_t        i, j;
    int            ret_value = 1;

    const H5Z_class2_t libpressio_cls = {
        H5Z_CLASS_T_VERS,     /* H5Z_class_t version */
        LIBPRESSIO_FILTER_ID, /* filter id number */
        1,                    /* encoder_present */
        1,                    /* decoder_present */
        "libpressio_example", /* filter name for debugging */
        NULL,                 /* can_apply callback */
        libpressio_set_local, /* set_local callback */
        libpressio_filter,    /* filter function */
    };

    /*
     * Initialize data.
     */
    for (i = 0; i < DIM0; i++)
        for (j = 0; j < DIM1; j++)
            wdata[i][j] = (int)(i * DIM1 + j);

    options_blob = (unsigned char *)malloc(OPTIONS_BLOB_SIZE);
    if (!options_blob)
        goto done;
    for (i = 0; i < OPTIONS_BLOB_SIZE; i++)
        options_blob[i] = (unsigned char)(i * 7 + 3);

    status = H5Zregister(&libpressio_cls);
    if (status < 0)
        goto done;

    /*
     * Create a new file using the default properties.
     */
    file_id = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (file_id < 0)
        goto done;

    /*
     * Create dataspace and DCPL, then attach the (oversized) options
     * as a blob before creating the dataset -- this is the only
     * migration a real filter's public API needs to make.
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

    printf("Attaching a %d KiB options blob via H5Pappend_filter_blob...\n", OPTIONS_BLOB_SIZE / 1024);
    status = H5Pappend_filter_blob(dcpl_id, LIBPRESSIO_FILTER_ID, 0, options_blob, OPTIONS_BLOB_SIZE);
    if (status < 0)
        goto done;

    /*
     * H5Dcreate2 triggers set_local, which pulls the blob back out
     * and derives the small cd_values summary.
     */
    dset_id = H5Dcreate2(file_id, DATASET, H5T_NATIVE_INT, space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    if (dset_id < 0)
        goto done;

    /*
     * set_local ran against the dataset's own private DCPL copy, not
     * the "dcpl_id" template handle above -- H5Dcreate2 never mutates
     * the caller's original DCPL. H5Dget_create_plist(dset_id) is the
     * only way to see what set_local actually wrote.
     */
    dcpl_out  = H5Dget_create_plist(dset_id);
    cd_nelmts = sizeof(cd_values) / sizeof(cd_values[0]);
    status =
        H5Pget_filter_by_id2(dcpl_out, LIBPRESSIO_FILTER_ID, &flags, &cd_nelmts, cd_values, 0, NULL, NULL);
    if (status < 0)
        goto done;
    printf("set_local packed %lu cd_values: datatype class %u, chunk dims %ux%u\n", (unsigned long)cd_nelmts,
           cd_values[0], cd_values[2], cd_values[3]);
    H5Pclose(dcpl_out);
    dcpl_out = H5I_INVALID_HID;

    status = H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata[0]);
    if (status < 0)
        goto done;

    H5Dclose(dset_id);
    dset_id = -1;
    H5Pclose(dcpl_id);
    dcpl_id = -1;
    H5Sclose(space_id);
    space_id = -1;
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
     * The full oversized options blob -- the part that would have
     * segfaulted the real filter's cd_values-only approach -- round-
     * trips byte-for-byte via the reopened dataset's DCPL.
     */
    dcpl_out = H5Dget_create_plist(dset_id);
    if (dcpl_out < 0)
        goto done;

    status = H5Pget_filter_blob(dcpl_out, 0, 0, NULL, &got_size);
    if (status < 0 || got_size != OPTIONS_BLOB_SIZE) {
        printf("Blob size mismatch after reopen.\n");
        goto done;
    }

    blob_out = (unsigned char *)malloc(got_size);
    if (!blob_out)
        goto done;
    status = H5Pget_filter_blob(dcpl_out, 0, 0, blob_out, &got_size);
    if (status < 0 || memcmp(blob_out, options_blob, OPTIONS_BLOB_SIZE) != 0) {
        printf("Options blob did NOT round-trip correctly.\n");
        goto done;
    }
    printf("Options blob (%d KiB) round-tripped byte-for-byte via H5Pget_filter_blob.\n",
           OPTIONS_BLOB_SIZE / 1024);

    ret_value = 0;

done:
    /*
     * Close and release resources.
     */
    free(options_blob);
    free(blob_out);
    if (dcpl_out >= 0)
        H5Pclose(dcpl_out);
    if (dcpl_id >= 0)
        H5Pclose(dcpl_id);
    if (dset_id >= 0)
        H5Dclose(dset_id);
    if (space_id >= 0)
        H5Sclose(space_id);
    if (file_id >= 0)
        H5Fclose(file_id);

    return ret_value;
}
