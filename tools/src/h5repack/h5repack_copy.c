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

#include "h5repack.h"
#include "h5tools.h"
#include "h5tools_utils.h"

/*-------------------------------------------------------------------------
 * typedefs
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * globals
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * macros
 *-------------------------------------------------------------------------
 */

/* size of buffer/# of bytes to xfer at a time when copying userblock */
#define USERBLOCK_XFER_SIZE     512

/*-------------------------------------------------------------------------
 * local functions
 *-------------------------------------------------------------------------
 */
static int get_hyperslab(hid_t dcpl_id, int rank_dset, hsize_t dims_dset[],
        size_t size_datum, hsize_t dims_hslab[], hsize_t * hslab_nbytes_p);
static void print_dataset_info(hid_t dcpl_id, char *objname, double per, int pr);
static int do_copy_objects(hid_t fidin, hid_t fidout, trav_table_t *travt,
        pack_opt_t *options);
static int copy_user_block(const char *infile, const char *outfile,
        hsize_t size);
#if defined (H5REPACK_DEBUG_USER_BLOCK)
static void print_user_block(const char *filename, hid_t fid);
#endif


/*-------------------------------------------------------------------------
 * Function: copy_objects
 *
 * Purpose:  duplicate all HDF5 objects in the file
 *
 * Return:   0, ok,
 *          -1 no
 *-------------------------------------------------------------------------
 */
int
copy_objects(const char* fnamein, const char* fnameout, pack_opt_t *options)
{
    int           ret_value = 0;
    hid_t         fidin = -1;
    hid_t         fidout = -1;
    hid_t         fcpl_in = -1;  /* file creation property list ID for input file */
    hid_t         grp_in = -1;   /* group ID */
    hid_t         gcpl_in = -1;  /* group creation property list */
    hid_t         fcpl = H5P_DEFAULT;     /* file creation property list ID */
    hid_t         fapl = H5P_DEFAULT;     /* file access property list ID */
    trav_table_t *travt = NULL;
    hsize_t       ub_size = 0;            /* size of user block */
    H5F_fspace_strategy_t set_strategy;   /* Strategy to be set in outupt file */
    hbool_t       set_persist;            /* Persist free-space status to be set in output file */
    hsize_t       set_threshold;          /* Free-space section threshold to be set in output file */
    hsize_t       set_pagesize;           /* File space page size to be set in output file */
    H5F_fspace_strategy_t in_strategy;    /* Strategy from input file */
    hbool_t       in_persist;             /* Persist free-space status from input file */
    hsize_t       in_threshold;           /* Free-space section threshold from input file */
    hsize_t       in_pagesize;            /* File space page size from input file */
    unsigned      crt_order_flags;        /* group creation order flag */

    /*-------------------------------------------------------------------------
     * open input file
     *-------------------------------------------------------------------------
     */
    if ((fidin = h5tools_fopen(fnamein, H5F_ACC_RDONLY, H5P_DEFAULT, NULL, NULL, (size_t) 0)) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "h5tools_fopen failed <%s>: %s", fnamein, H5FOPENERROR);

    /* get user block size and file space strategy/persist/threshold */
    {
        if ((fcpl_in = H5Fget_create_plist(fidin)) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Fget_create_plist failed to retrieve file creation property list");

        if (H5Pget_userblock(fcpl_in, &ub_size) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pget_userblock failed to retrieve userblock size");

        /* If the -S option is not set, get "strategy" from the input file */
        if(H5Pget_file_space_strategy(fcpl_in, &in_strategy, &in_persist, &in_threshold) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pget_file_space_strategy failed to retrieve file space strategy");

        /* If the -G option is not set, get "pagesize" from the input file */
        if(H5Pget_file_space_page_size(fcpl_in, &in_pagesize) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pget_file_space_page_size failed to retrieve file space threshold");

        /* open root group */
        if ((grp_in = H5Gopen2(fidin, "/", H5P_DEFAULT)) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Gopen2 failed");

        /* get root group creation property list */
        if ((gcpl_in = H5Gget_create_plist(grp_in)) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Gget_create_plist failed");

        /* query and set the group creation properties */
        if (H5Pget_link_creation_order(gcpl_in, &crt_order_flags) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pget_link_creation_order failed");

        if (H5Pclose(fcpl_in) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pclose failed to close property list");
    }

    if(options->latest)
        options->low_bound = options->high_bound = H5F_LIBVER_LATEST;
    /* Create file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pcreate failed to create file access property list");

    /* It can be default, latest or other settings by users */
    if(H5Pset_libver_bounds(fapl, options->low_bound, options->high_bound) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_libver_bounds failed to set format version bounds");

    /* Check if we need to create a non-default file creation property list */
    if (options->low_bound >= H5F_LIBVER_V18 || ub_size > 0) {
        /* Create file creation property list */
        if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pcreate failed to create a file creation property list");

        if (ub_size > 0)
            if (H5Pset_userblock(fcpl, ub_size) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_userblock failed to set non-default userblock size");

        if (options->low_bound >= H5F_LIBVER_V18) {
            unsigned i = 0, nindex = 0, mesg_type_flags[5], min_mesg_sizes[5];

            /* Adjust group creation parameters for root group */
            /* (So that it is created in "dense storage" form) */
            if (H5Pset_link_phase_change(fcpl, (unsigned) options->grp_compact, (unsigned) options->grp_indexed) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_link_phase_change failed to adjust group creation parameters for root group");

            for (i = 0; i < 5; i++) {
                if (options->msg_size[i] > 0) {
                    switch (i) {
                        case 0:
                            mesg_type_flags[nindex] = H5O_SHMESG_SDSPACE_FLAG;
                            break;

                        case 1:
                            mesg_type_flags[nindex] = H5O_SHMESG_DTYPE_FLAG;
                            break;

                        case 2:
                            mesg_type_flags[nindex] = H5O_SHMESG_FILL_FLAG;
                            break;

                        case 3:
                            mesg_type_flags[nindex] = H5O_SHMESG_PLINE_FLAG;
                            break;

                        case 4:
                            mesg_type_flags[nindex] = H5O_SHMESG_ATTR_FLAG;
                            break;

                        default:
                            break;
                    } /* end switch */
                    min_mesg_sizes[nindex] = (unsigned) options->msg_size[i];

                    nindex++;
                } /* end if */
            } /* end for */

            if (nindex > 0) {
                if (H5Pset_shared_mesg_nindexes(fcpl, nindex) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_shared_mesg_nindexes failed to set the number of shared object header message indexes");

                /* msg_size[0]=dataspace, 1=datatype, 2=file value, 3=filter pipleline, 4=attribute */
                for (i = 0; i < (nindex - 1); i++)
                    if (H5Pset_shared_mesg_index(fcpl, i, mesg_type_flags[i], min_mesg_sizes[i]) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_shared_mesg_index failed to configure the specified shared object header message index");
            } /* if (nindex>0) */

        } /* end if */
    } /* end if */
#if defined (H5REPACK_DEBUG_USER_BLOCK)
print_user_block(fnamein, fidin);
#endif

    /*-------------------------------------------------------------------------
     * set the new user userblock options in the FCPL (before H5Fcreate )
     *-------------------------------------------------------------------------
     */
    if (options->ublock_size > 0) {
        /* either use the FCPL already created or create a new one */
        if (fcpl == H5P_DEFAULT)
            /* create a file creation property list */
            if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pcreate failed to create a file creation property list");

        /* set user block size */
        if (H5Pset_userblock(fcpl, options->ublock_size) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_userblock failed to set userblock size");
    }

    /*-------------------------------------------------------------------------
     * set alignment options
     *-------------------------------------------------------------------------
     */
    if (options->alignment > 0) {
        /* either use the FAPL already created or create a new one */
        if (fapl == H5P_DEFAULT)
            /* create a file access property list */
            if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pcreate failed to create file access property list");

        if (H5Pset_alignment(fapl, options->threshold, options->alignment) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_alignment failed to set alignment");
    }

    /*-------------------------------------------------------------------------
     * set metadata block size option
     *-------------------------------------------------------------------------
     */
    if (options->meta_block_size > 0) {
        /* either use the FAPL already created or create a new one */
        if (fapl == H5P_DEFAULT)
            /* create a file access property list */
            if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pcreate failed to create file access property list");

        if (H5Pset_meta_block_size(fapl, options->meta_block_size) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_meta_block_size failed to set metadata block size");
    }

    /*-------------------------------------------------------------------------
     * Set file space information
     *-------------------------------------------------------------------------
     */

    /* either use the FCPL already created or create a new one */
    if (fcpl == H5P_DEFAULT)
        /* create a file creation property list */
        if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pcreate failed to create a file creation property list");

    if(H5Pset_link_creation_order(fcpl, crt_order_flags ) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_link_creation_order failed");

    /* Set file space info to those from input file */
    set_strategy = in_strategy;
    set_persist = in_persist;
    set_threshold = in_threshold;
    set_pagesize = in_pagesize;

    if(options->fs_strategy == (H5F_fspace_strategy_t)-1) /* A default strategy is specified by user */
        set_strategy = FS_STRATEGY_DEF;
    else if(options->fs_strategy != (H5F_fspace_strategy_t)0) /* Set strategy as specified by user */
        set_strategy = options->fs_strategy;

    if(options->fs_persist == -1) /* A default "persist" is specified by user */
        set_persist = FS_PERSIST_DEF;
    else if(options->fs_persist != 0) /* Set "persist" as specified by user */
        set_persist = (hbool_t)options->fs_persist;

    if(options->fs_threshold == -1) /* A "0" threshold is specified by user */
        set_threshold = (hsize_t)0;
    else if(options->fs_threshold != 0) /* Set threshold as specified by user */
        set_threshold = (hsize_t)options->fs_threshold;

    /* Set file space information as specified */
    if(H5Pset_file_space_strategy(fcpl, set_strategy, set_persist, set_threshold) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_file_space_strategy failed to set file space strategy");

    if(options->fs_pagesize == -1) /* A "0" file space page size is specified by user */
        set_pagesize = (hsize_t)0;
    else if(options->fs_pagesize != 0) /* Set file space page size as specified by user */
        set_pagesize = (hsize_t)options->fs_pagesize;

    if(set_pagesize != FS_PAGESIZE_DEF) /* Set non-default file space page size as specified */
        if(H5Pset_file_space_page_size(fcpl, set_pagesize) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_file_space_page_size failed to set file space page size");

    /*-------------------------------------------------------------------------
     * create the output file
     *-------------------------------------------------------------------------
     */
    if (options->verbose)
        HDprintf("Making new file ...\n");

    if ((fidout = H5Fcreate(fnameout, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Fcreate could not create file <%s>:", fnameout);

    /*-------------------------------------------------------------------------
     * write a new user block if requested
     *-------------------------------------------------------------------------
     */
    if (options->ublock_size > 0)
        if (copy_user_block(options->ublock_filename, fnameout, options->ublock_size) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Could not copy user block. Exiting...");

    /*-------------------------------------------------------------------------
     * get list of objects
     *-------------------------------------------------------------------------
     */

    /* Initialize indexing options */
    h5trav_set_index(sort_by, sort_order);
    /* init table */
    trav_table_init(&travt);

    if (travt) {
        /* get the list of objects in the file */
        if (h5trav_gettable(fidin, travt) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "h5trav_gettable failed");

        /*-------------------------------------------------------------------------
        * do the copy
        *-------------------------------------------------------------------------
        */
        if (do_copy_objects(fidin, fidout, travt, options) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "do_copy_objects from <%s> could not copy data to <%s>", fnamein, fnameout);

        /*-------------------------------------------------------------------------
        * do the copy of referenced objects
        * and create hard links
        *-------------------------------------------------------------------------
        */
        if (do_copy_refobjs(fidin, fidout, travt, options) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "do_copy_refobjs from <%s> could not copy data to <%s>", fnamein, fnameout);
    }

    /*-------------------------------------------------------------------------
     * write only the input file user block if there is no user block file input
     *-------------------------------------------------------------------------
     */

    if (ub_size > 0 && options->ublock_size == 0)
        if (copy_user_block(fnamein, fnameout, ub_size) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Could not copy user block. Exiting...");

done:
    H5E_BEGIN_TRY {
        H5Pclose(fcpl_in);
        H5Pclose(gcpl_in);
        H5Pclose(fapl);
        H5Pclose(fcpl);
        H5Gclose(grp_in);
        H5Fclose(fidin);
        H5Fclose(fidout);
        H5Fclose(fidin);
        H5Fclose(fidout);
    } H5E_END_TRY;
    if (travt)
        trav_table_free(travt);

    return ret_value;
} /* end copy_objects() */

/*-------------------------------------------------------------------------
 * Function: get_hyperslab
 *
 * Purpose: Calulate a hyperslab from a dataset for higher performance.
 *          The size of hyperslab is limitted by H5TOOLS_BUFSIZE.
 *          Return the hyperslab dimentions and size in byte.
 *
 * Return:  0 - SUCCEED, -1 FAILED
 *
 * Parameters:
 *   dcpl_id : [IN] dataset creation property.
 *   rank_dset : [IN] dataset rank
 *   dims_dset[] : [IN] dataset dimentions
 *   size_datum : [IN] size of a data element in byte
 *   dims_hslab[] : [OUT] calculated hyperslab dimentions
 *   * hslab_nbytes_p : [OUT] total byte of the hyperslab
 *
 * Update:
 *   The hyperslab calucation would be depend on if the dataset is chunked
 *   or not.
 *
 *   There care 3 conditions to cover:
 *   1. If chunked and a chunk fits in buffer, each chunk would be a unit of
 *      collection and the boundary would be dataset's dims.
 *   2. If chunked but a chunk doesn't fit in buffer, each data element would
 *      be a unit of collection and the boundary would be the chunk itself.
 *   3. If not chunked, each data element would be a unit of collection and
 *      the boundary would be dataset's dims.
 *
 *   The calulation starts from the last dimention (h5dump dims output).
 *-----------------------------------------*/

int
get_hyperslab(hid_t dcpl_id, int rank_dset, hsize_t dims_dset[],
        size_t size_datum, hsize_t dims_hslab[], hsize_t * hslab_nbytes_p)
{
    int     ret_value = 0;
    int     k;
    H5D_layout_t dset_layout;
    int     rank_chunk;
    hsize_t dims_chunk[H5S_MAX_RANK];
    hsize_t size_chunk = 1;
    hsize_t nchunk_fit;                   /* number of chunks that fits in hyperslab buffer (H5TOOLS_BUFSIZE) */
    hsize_t ndatum_fit;                   /* number of dataum that fits in hyperslab buffer (H5TOOLS_BUFSIZE) */
    hsize_t chunk_dims_map[H5S_MAX_RANK]; /* mapped chunk dimentions */
    hsize_t hs_dims_map[H5S_MAX_RANK];    /* mapped hyperslab dimentions */
    hsize_t hslab_nbytes;                 /* size of hyperslab in byte */

    /* init to set as size of a data element */
    hslab_nbytes = size_datum;

    /* get layout of dataset */
    dset_layout = H5Pget_layout(dcpl_id);

    /* if dataset is chunked */
    if (dset_layout == H5D_CHUNKED) {
        /* get chunk dims */
        rank_chunk = H5Pget_chunk(dcpl_id, rank_dset, dims_chunk);
        if (rank_chunk < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pget_chunk failed");

        for (k = rank_dset; k > 0; --k)
            size_chunk *= dims_chunk[k - 1];

        /* figure out how many chunks can fit in the hyperslab buffer */
        nchunk_fit = (H5TOOLS_BUFSIZE / size_datum) / size_chunk;

        /* 1. if a chunk fit in hyperslab buffer */
        if (nchunk_fit >= 1) {
            /* Calulate a hyperslab that contains as many chunks that can fit
             * in hyperslab buffer. Hyperslab will be increased starting from
             * the last dimention of the dataset (see h5dump's dims output).
             * The calculation boundary is dataset dims.
             * In the loop, used mapping from a datum to a chunk to figure out
             * chunk based hyperslab.
             */
            for (k = rank_dset; k > 0; --k) {
                /* map dataset dimentions with a chunk dims */
                chunk_dims_map[k - 1] = dims_dset[k - 1] / dims_chunk[k - 1];

                /* if reminder exist, increse by 1 to cover partial edge chunks */
                if (dims_dset[k - 1] % dims_chunk[k - 1] > 0)
                    chunk_dims_map[k - 1]++;

                /* get mapped hyperslab dims */
                hs_dims_map[k - 1] = MIN (nchunk_fit, chunk_dims_map[k-1]);

                /* prepare next round */
                nchunk_fit = nchunk_fit / chunk_dims_map[k - 1];
                /* if a chunk is bigger than the rest of buffer */
                if (nchunk_fit == 0)
                    nchunk_fit = 1;

                /* get hyperslab dimentions as unmapping to actual size */
                dims_hslab[k - 1] = MIN( (hs_dims_map[k-1] * dims_chunk[k-1]), dims_dset[k-1]);

                /* calculate total size for the hyperslab */
                hslab_nbytes *= dims_hslab[k - 1];
            }
        }
        /* 2. if a chunk is bigger than hyperslab buffer */
        else {
            /* Calulate a hyperslab that contains as many data elements that
             * can fit in hyperslab buffer. Hyperslab will be increased
             * starting from the last dimention of the chunk (see h5dump's dims
             * output).
             * The calculation boundary is a chunk dims.
             */
            for (k = rank_dset; k > 0; --k) {
                ndatum_fit = H5TOOLS_BUFSIZE / hslab_nbytes;

                /* if a datum is bigger than rest of buffer */
                if (ndatum_fit == 0)
                    ndatum_fit = 1;
                /* get hyperslab dimentions within a chunk boundary */
                dims_hslab[k - 1] = MIN (dims_chunk[k-1], ndatum_fit);

                /* calculate total size for the hyperslab */
                hslab_nbytes *= dims_hslab[k - 1];

                if (hslab_nbytes <= 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "calculate total size for the hyperslab failed");
            }
        }
    }
    /* 3. if dataset is not chunked */
    else {
        /* Calulate a hyperslab that contains as many data elements that can
         * fit in hyperslab buffer. Hyperslab will be increased starting from
         * the last dimention of the dataset (see h5dump's dims output).
         * The calculation boundary is dataset dims.
         */
        for (k = rank_dset; k > 0; --k) {
            ndatum_fit = H5TOOLS_BUFSIZE / hslab_nbytes;

            /* if a datum is bigger than rest of buffer */
            if (ndatum_fit == 0)
                ndatum_fit = 1;
            /* get hyperslab dimentions within dataset boundary */
            dims_hslab[k - 1] = MIN(dims_dset[k - 1], ndatum_fit);

            /* calculate total size for the hyperslab */
            hslab_nbytes *= dims_hslab[k - 1];

            if (hslab_nbytes <= 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "calculate total size for the hyperslab failed");
        }
    }

    /* pass out the hyperslab size*/
    *hslab_nbytes_p = hslab_nbytes;

done:
    return ret_value;
} /* end get_hyperslab() */

/*-------------------------------------------------------------------------
 * Function: do_copy_objects
 *
 * Purpose:  duplicate all HDF5 objects in the file
 *
 * Return:   0, ok, -1 no
 *
 *  A threshold of H5TOOLS_MALLOCSIZE (128 MB) is the limit upon which I/O hyperslab is done
 *  i.e., if the memory needed to read a dataset is greater than this limit,
 *  then hyperslab I/O is done instead of one operation I/O
 *  For each dataset, the memory needed is calculated according to
 *
 *  memory needed = number of elements * size of each element
 *
 *  if the memory needed is lower than H5TOOLS_MALLOCSIZE, then the following operations
 *  are done
 *
 *  H5Dread( input_dataset1 )
 *  H5Dread( input_dataset2 )
 *
 *  with all elements in the datasets selected. If the memory needed is greater than
 *  H5TOOLS_MALLOCSIZE, then the following operations are done instead:
 *
 *  a strip mine is defined for each dimension k (a strip mine is defined as a
 *  hyperslab whose size is memory manageable) according to the formula
 *
 *  (1) strip_mine_size[k ] = MIN(dimension[k ], H5TOOLS_BUFSIZE / size of memory type)
 *
 *  where H5TOOLS_BUFSIZE is a constant currently defined as 1MB. This formula assures
 *  that for small datasets (small relative to the H5TOOLS_BUFSIZE constant), the strip
 *  mine size k is simply defined as its dimension k, but for larger datasets the
 *  hyperslab size is still memory manageable.
 *  a cycle is done until the number of elements in the dataset is reached. In each
 *  iteration, two parameters are defined for the function H5Sselect_hyperslab,
 *  the start and size of each hyperslab, according to
 *
 *  (2) hyperslab_size [k] = MIN(dimension[k] - hyperslab_offset[k], strip_mine_size [k])
 *
 *  where hyperslab_offset [k] is initially set to zero, and later incremented in
 *  hyperslab_size[k] offsets. The reason for the operation
 *
 *  dimension[k] - hyperslab_offset[k]
 *
 *  in (2) is that, when using the strip mine size, it assures that the "remaining" part
 *  of the dataset that does not fill an entire strip mine is processed.
 *
 *  1. figure out a hyperslab (dimentions) and size  (refer to get_hyperslab()).
 *  2. Calculate the hyperslab selections as the selection is moving forward.
 *     Selection would be same as the hyperslab except for the remaining edge portion
 *     of the dataset. The code take care of the remaining portion if exist.
 *
 *-------------------------------------------------------------------------
 */

int
do_copy_objects(hid_t fidin, hid_t fidout, trav_table_t *travt,
        pack_opt_t *options) /* repack options */
{
    int   ret_value = 0;
    hid_t grp_in = -1;   /* group ID */
    hid_t grp_out = -1;  /* group ID */
    hid_t dset_in = -1;  /* read dataset ID */
    hid_t dset_out = -1; /* write dataset ID */
    hid_t gcpl_in = -1;  /* group creation property list */
    hid_t gcpl_out = -1; /* group creation property list */
    hid_t type_in = -1;  /* named type ID */
    hid_t type_out = -1; /* named type ID */
    hid_t dcpl_in = -1;  /* dataset creation property list ID */
    hid_t dcpl_out = -1; /* dataset creation property list ID */
    hid_t f_space_id = -1; /* file space ID */
    hid_t ftype_id = -1; /* file type ID */
    hid_t wtype_id = -1; /* read/write type ID */
    named_dt_t *named_dt_head = NULL; /* Pointer to the stack of named datatypes copied */
    size_t msize;        /* size of type */
    hsize_t nelmts;      /* number of elements in dataset */
    H5D_space_status_t space_status; /* determines whether space has been allocated for the dataset  */
    int rank;            /* rank of dataset */
    hsize_t dims[H5S_MAX_RANK];/* dimensions of dataset */
    hsize_t dsize_in;    /* input dataset size before filter */
    hsize_t dsize_out;   /* output dataset size after filter */
    int apply_s;         /* flag for apply filter to small dataset sizes */
    int apply_f;         /* flag for apply filter to return error on H5Dcreate */
    void *buf = NULL;    /* buffer for raw data */
    void *hslab_buf = NULL; /* hyperslab buffer for raw data */
    int has_filter;      /* current object has a filter */
    int req_filter;      /* there was a request for a filter */
    int req_obj_layout = 0; /* request layout to current object */
    unsigned crt_order_flags; /* group creation order flag */
    unsigned i;
    unsigned u;
    int ifil;
    int is_ref = 0;
    htri_t is_named;
    hbool_t limit_maxdims;
    hsize_t size_dset;

    /*-------------------------------------------------------------------------
     * copy the supplied object list
     *-------------------------------------------------------------------------
     */

    if (options->verbose) {
        HDprintf("-----------------------------------------\n");
        HDprintf(" Type     Filter (Compression)     Name\n");
        HDprintf("-----------------------------------------\n");
    }

    if (travt->objs) {
        for (i = 0; i < travt->nobjs; i++) {
            /* init variables per obj */
            buf = NULL;
            limit_maxdims = FALSE;

            switch (travt->objs[i].type) {
            case H5TRAV_TYPE_UNKNOWN:
                break;

                /*-------------------------------------------------------------------------
                 * H5TRAV_TYPE_GROUP
                 *-------------------------------------------------------------------------
                 */
            case H5TRAV_TYPE_GROUP:
                if (options->verbose)
                    HDprintf(FORMAT_OBJ, "group", travt->objs[i].name);

                /* open input group */
                if ((grp_in = H5Gopen2(fidin, travt->objs[i].name, H5P_DEFAULT)) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Gopen2 failed");

                /* get input group creation property list */
                if ((gcpl_in = H5Gget_create_plist(grp_in)) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Gget_create_plist failed");

                /* query and set the group creation properties */
                if (H5Pget_link_creation_order(gcpl_in, &crt_order_flags) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pget_link_creation_order failed");

                /* set up group creation property list */
                if ((gcpl_out = H5Pcreate(H5P_GROUP_CREATE)) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pcreate failed");

                if (H5Pset_link_creation_order(gcpl_out, crt_order_flags) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_link_creation_order failed");

                /*-------------------------------------------------------------------------
                 * the root is a special case, we get an ID for the root group
                 * and copy its attributes using that ID
                 *-------------------------------------------------------------------------
                 */
                if (HDstrcmp(travt->objs[i].name, "/") == 0) {
                    if ((grp_out = H5Gopen2(fidout, "/", H5P_DEFAULT)) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Gopen2 failed");
                }
                else {
                    if (options->grp_compact > 0 || options->grp_indexed > 0)
                        if (H5Pset_link_phase_change(gcpl_out, (unsigned) options->grp_compact, (unsigned) options->grp_indexed) < 0)
                            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_link_phase_change failed");

                    if ((grp_out = H5Gcreate2(fidout, travt->objs[i].name, H5P_DEFAULT, gcpl_out, H5P_DEFAULT)) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Gcreate2 failed");
                }

                /*-------------------------------------------------------------------------
                 * copy attrs
                 *-------------------------------------------------------------------------
                 */
                if (copy_attr(grp_in, grp_out, &named_dt_head, travt, options) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "copy_attr failed");

                if (H5Pclose(gcpl_out) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pclose failed");
                if (H5Pclose(gcpl_in) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pclose failed");
                if (H5Gclose(grp_out) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Gclose failed");
                if (H5Gclose(grp_in) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Gclose failed");

                break;

                /*-------------------------------------------------------------------------
                 * H5TRAV_TYPE_DATASET
                 *-------------------------------------------------------------------------
                 */
            case H5TRAV_TYPE_DATASET:
                has_filter = 0;
                req_filter = 0;

                /* check if global filters were requested */
                if (options->n_filter_g)
                    req_filter = 1;

                /* check if filters were requested for individual objects */
                if (options->op_tbl->objs) {
                    for (u = 0; u < options->op_tbl->nelems; u++) {
                        if (HDstrcmp(travt->objs[i].name, options->op_tbl->objs[u].path) == 0)
                            for (ifil = 0; ifil < options->op_tbl->objs[ifil].nfilters; ifil++) {
                                if (options->op_tbl->objs[u].filter[ifil].filtn > 0)
                                    req_filter = 1;
                            }
                    }
                }

                /* check if layout change requested individual object */
                if (options->layout_g != H5D_LAYOUT_ERROR) {
                    pack_info_t *pckinfo;

                    /* any dataset is specified */
                    if (options->op_tbl->nelems > 0) {
                        /* check if object exist */
                        pckinfo = options_get_object(travt->objs[i].name, options->op_tbl);
                        if (pckinfo)
                            req_obj_layout = 1;
                    }
                }

                /* early detection of references */
                if ((dset_in = H5Dopen2(fidin, travt->objs[i].name, H5P_DEFAULT)) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dopen2 failed");
                if ((ftype_id = H5Dget_type(dset_in)) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dget_type failed");
                if (H5T_REFERENCE == H5Tget_class(ftype_id))
                    is_ref = 1;

                /* Check if the datatype is committed */
                if ((is_named = H5Tcommitted(ftype_id)) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Tcommitted failed");
                if (is_named)
                    if ((wtype_id = copy_named_datatype(ftype_id, fidout, &named_dt_head, travt, options)) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "copy_named_datatype failed");

                if (H5Tclose(ftype_id) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Tclose failed");
                if (H5Dclose(dset_in) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dclose failed");

                /*-------------------------------------------------------------------------
                 * check if we should use H5Ocopy or not
                 * if there is a request for filters/layout, we read/write the object
                 * otherwise we do a copy using H5Ocopy
                 *-------------------------------------------------------------------------
                 */
                if (options->op_tbl->nelems || options->all_filter == 1
                        || options->all_layout == 1 || is_ref || is_named) {

                    int j;

                    if ((dset_in = H5Dopen2(fidin, travt->objs[i].name, H5P_DEFAULT)) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dopen2 failed");
                    if ((f_space_id = H5Dget_space(dset_in)) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dget_space failed");
                    if ((ftype_id = H5Dget_type(dset_in)) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dget_type failed");
                    if ((dcpl_in = H5Dget_create_plist(dset_in)) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dget_create_plist failed");
                    if ((rank = H5Sget_simple_extent_ndims(f_space_id)) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Sget_simple_extent_ndims failed");
                    HDmemset(dims, 0, sizeof dims);
                    if (H5Sget_simple_extent_dims(f_space_id, dims, NULL) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Sget_simple_extent_dims failed");
                    if (H5Dget_space_status(dset_in, &space_status) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dget_space_status failed");

                    /* If the input dataset has external storage, it must be contiguous.
                     * Accordingly, there would be no filter or chunk properties to preserve,
                     * so create a new DCPL.
                     * Otherwise, copy dcpl_in.
                     */
                    if (H5Pget_external_count(dcpl_in)) {
                        if ((dcpl_out = H5Pcreate(H5P_DATASET_CREATE)) < 0)
                            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pcreate failed");
                    }
                    else if ((dcpl_out = H5Pcopy(dcpl_in)) < 0) {
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pcopy failed");
                    }

                    nelmts = 1;
                    for (j = 0; j < rank; j++)
                        nelmts *= dims[j];

                    /* wtype_id will have already been set if using a named dtype */
                    if (!is_named) {
                        if (options->use_native == 1)
                            wtype_id = H5Tget_native_type(ftype_id, H5T_DIR_DEFAULT);
                        else
                            wtype_id = H5Tcopy(ftype_id);
                    }

                    if ((msize = H5Tget_size(wtype_id)) == 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Tget_size failed");

                    /* size of current dset */
                    size_dset = nelmts * msize;

                    /*-------------------------------------------------------------------------
                     * check if the dataset creation property list has filters that
                     * are not registered in the current configuration
                     * 1) the external filters GZIP and SZIP might not be available
                     * 2) the internal filters might be turned off
                     *-------------------------------------------------------------------------
                     */
                    if (h5tools_canreadf((travt->objs[i].name), dcpl_in) == 1) {
                        apply_s = 1;
                        apply_f = 1;

                        /*-------------------------------------------------------------------------
                         * references are a special case
                         * we cannot just copy the buffers, but instead we recreate the reference
                         * in a second traversal of the output file
                         *-------------------------------------------------------------------------
                         */
                        if (H5T_REFERENCE != H5Tget_class(wtype_id)) {
                            /* get the storage size of the input dataset */
                            dsize_in = H5Dget_storage_size(dset_in);

                            /* check for small size datasets (less than 1k) except
                             * changing to COMPACT. For the reference, COMPACT is limited
                             * by size 64K by library.
                             */
                            if (options->layout_g != H5D_COMPACT)
                                if (size_dset < options->min_comp)
                                    apply_s = 0;

                            /* apply the filter */
                            if (apply_s)
                                if (apply_filters(travt->objs[i].name, rank, dims, msize, dcpl_out, options, &has_filter) < 0)
                                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "apply_filters failed");

                            /* only if layout change requested for entire file or
                             * individual obj */
                            if (options->all_layout > 0 || req_obj_layout == 1) {
                                /*-------------------------------------------------
                                 * Unset the unlimited max dims if convert to other
                                 * than chunk layouts, because unlimited max dims
                                 * only can be applied to chunk layout.
                                 * Also perform only for targeted dataset
                                 * Also check for size limit to convert to compact
                                 *-------------------------------------------------*/
                                if (options->layout_g != H5D_CHUNKED) {
                                    /* any dataset is specified */
                                    if (options->op_tbl->nelems > 0) {
                                        /* if current obj match specified obj */
                                        if (options_get_object(travt->objs[i].name, options->op_tbl))
                                            limit_maxdims = TRUE;
                                    }
                                    else  /* no dataset is specified */
                                        limit_maxdims = TRUE;

                                    /* if convert to COMPACT */
                                    if (options->layout_g == H5D_COMPACT)
                                        if (size_dset > MAX_COMPACT_DSIZE)
                                            limit_maxdims = FALSE;

                                    /* unset unlimited max dims */
                                    if (limit_maxdims)
                                        H5Sset_extent_simple(f_space_id, rank, dims, NULL);
                                } /* end if not chunked */
                            } /* end if layout change requested for entire file or individual object */

                            /*-------------------------------------------------------------------------
                             * create the output dataset;
                             * disable error checking in case the dataset cannot be created with the
                             * modified dcpl; in that case use the original instead
                             *-------------------------------------------------------------------------
                             */
                            dset_out = H5Dcreate2(fidout, travt->objs[i].name, wtype_id, f_space_id, H5P_DEFAULT, dcpl_out, H5P_DEFAULT);
                            if (dset_out == FAIL) {
                                H5Epush2(H5tools_ERR_STACK_g, __FILE__, FUNC, __LINE__, H5tools_ERR_CLS_g, H5E_tools_g, H5E_tools_min_id_g, "H5Dcreate2 failed");
                                if (options->verbose)
                                    HDprintf(" warning: could not create dataset <%s>. Applying original settings\n", travt->objs[i].name);

                                if ((dset_out = H5Dcreate2(fidout, travt->objs[i].name, wtype_id, f_space_id, H5P_DEFAULT, dcpl_in, H5P_DEFAULT)) < 0)
                                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dcreate2 failed");
                                apply_f = 0;
                            } /* end if retry dataset create */

                            /*-------------------------------------------------------------------------
                             * read/write
                             *-------------------------------------------------------------------------
                             */
                            if (nelmts > 0 && space_status != H5D_SPACE_STATUS_NOT_ALLOCATED) {
                                size_t need = (size_t)(nelmts * msize); /* bytes needed */

                                /* have to read the whole dataset if there is only one element in the dataset */
                                if (need < H5TOOLS_MALLOCSIZE)
                                    buf = HDmalloc(need);

                                if (buf != NULL) {
                                    if(H5Dread(dset_in, wtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
                                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dread failed");
                                    if(H5Dwrite(dset_out, wtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
                                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dwrite failed");

                                    /* Check if we have VL data in the dataset's
                                     * datatype that must be reclaimed */
                                    if (TRUE == H5Tdetect_class(wtype_id, H5T_VLEN))
                                        if (H5Dvlen_reclaim(wtype_id, f_space_id, H5P_DEFAULT, buf) < 0)
                                            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dvlen_reclaim failed");

                                    if (buf != NULL) { /* TODO: is buf potentially released by H5Dvlen_reclaim()? */
                                        HDfree(buf);
                                        buf = NULL;
                                    }
                                }
                                else { /* possibly not enough memory, read/write by hyperslabs */
                                    size_t p_type_nbytes = msize; /*size of memory type */
                                    hsize_t p_nelmts = nelmts; /*total elements */
                                    hsize_t elmtno; /*counter  */
                                    int carry; /*counter carry value */
                                    unsigned int vl_data = 0; /*contains VL datatypes */

                                    /* hyperslab info */
                                    hsize_t hslab_dims[H5S_MAX_RANK]; /*hyperslab dims */
                                    hsize_t hslab_nbytes; /*bytes per hyperslab */
                                    hsize_t hslab_nelmts; /*elements per hyperslab*/
                                    hid_t hslab_space; /*hyperslab data space */

                                    /* hyperslab selection info */
                                    hsize_t hs_sel_offset[H5S_MAX_RANK];/* selection offset */
                                    hsize_t hs_sel_count[H5S_MAX_RANK]; /* selection count */
                                    hsize_t hs_select_nelmts; /* selected elements */
                                    hsize_t zero[8]; /*vector of zeros */
                                    int k;
                                    H5D_layout_t dset_layout;
                                    hid_t dcpl_tmp = -1; /* dataset creation property list ID */

                                    /* check if we have VL data in the dataset's datatype */
                                    if (H5Tdetect_class(wtype_id, H5T_VLEN) == TRUE)
                                        vl_data = TRUE;

                                    /* check first if writing dataset is chunked,
                                     * if so use its chunk layout for better performance. */
                                    dset_layout = H5Pget_layout(dcpl_out);
                                    if (dset_layout == H5D_CHUNKED)
                                        dcpl_tmp = dcpl_out; /* writing dataset */
                                    else {
                                        dset_layout = H5Pget_layout(dcpl_in);
                                        if (dset_layout == H5D_CHUNKED)
                                            dcpl_tmp = dcpl_in; /* reading dataset */
                                    }

                                    /* get hyperslab dims and size in byte */
                                    if (get_hyperslab(dcpl_tmp, rank, dims, p_type_nbytes, hslab_dims, &hslab_nbytes) < 0)
                                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "get_hyperslab failed");

                                    hslab_buf = HDmalloc((size_t)hslab_nbytes);
                                    if (hslab_buf == NULL)
                                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "can't allocate space for hyperslab");

                                    hslab_nelmts = hslab_nbytes / p_type_nbytes;
                                    hslab_space = H5Screate_simple(1, &hslab_nelmts, NULL);

                                    /* the hyperslab selection loop */
                                    HDmemset(hs_sel_offset, 0, sizeof hs_sel_offset);
                                    HDmemset(zero, 0, sizeof zero);

                                    for (elmtno = 0; elmtno < p_nelmts; elmtno += hs_select_nelmts) {
                                        if (rank > 0) {
                                            /* calculate the hyperslab selections.
                                             * The selection would be same as the hyperslab
                                             * except for remaining edge portion of the dataset
                                             * which is smaller then the hyperslab.
                                             */
                                            for (k = 0, hs_select_nelmts = 1; k < rank; k++) {
                                                /* MIN() is used to get the remaining edge portion if exist.
                                                 * "dims[k] - hs_sel_offset[k]" is remaining edge portion that is smaller then the hyperslab.*/
                                                hs_sel_count[k] = MIN(dims[k] - hs_sel_offset[k], hslab_dims[k]);
                                                hs_select_nelmts *= hs_sel_count[k];
                                            }

                                            if (H5Sselect_hyperslab(f_space_id, H5S_SELECT_SET, hs_sel_offset, NULL, hs_sel_count, NULL) < 0)
                                                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Sselect_hyperslab failed");
                                            if (H5Sselect_hyperslab(hslab_space, H5S_SELECT_SET, zero, NULL, &hs_select_nelmts, NULL) < 0)
                                                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Sselect_hyperslab failed");
                                        } /* end if rank > 0 */
                                        else {
                                            H5Sselect_all(f_space_id);
                                            H5Sselect_all(hslab_space);
                                            hs_select_nelmts = 1;
                                        } /* end (else) rank  == 0 */

                                        if(H5Dread(dset_in, wtype_id, hslab_space, f_space_id, H5P_DEFAULT, hslab_buf) < 0)
                                            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dread failed");
                                        if(H5Dwrite(dset_out, wtype_id,  hslab_space, f_space_id, H5P_DEFAULT, hslab_buf) < 0)
                                            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dwrite failed");

                                        /* reclaim any VL memory, if necessary */
                                        if (vl_data)
                                            H5Dvlen_reclaim(wtype_id, hslab_space, H5P_DEFAULT, hslab_buf);

                                        /* calculate the next hyperslab offset */
                                        for (k = rank, carry = 1; k > 0 && carry; --k) {
                                            hs_sel_offset[k - 1] += hs_sel_count[k - 1];
                                            /* if reached the end of a dim */
                                            if (hs_sel_offset[k - 1] == dims[k - 1])
                                                hs_sel_offset[k - 1] = 0;
                                            else
                                                carry = 0;
                                        }
                                    } /* end for (hyperslab selection loop) */

                                    H5Sclose(hslab_space);
                                    if (hslab_buf != NULL) {
                                        HDfree(hslab_buf);
                                        hslab_buf = NULL;
                                    }
                                } /* end if reading/writing by hyperslab */
                            } /* end if (nelmts > 0 && space_status != H5D_SPACE_STATUS_NOT_ALLOCATED) */

                            /*-------------------------------------------------------------------------
                             * print amount of compression used
                             *-------------------------------------------------------------------------
                             */
                            if (options->verbose) {
                                double ratio = 0;

                                /* only print the compression ration if there was a filter request */
                                if (apply_s && apply_f && req_filter) {
                                    /* get the storage size of the output dataset */
                                    dsize_out = H5Dget_storage_size(dset_out);

                                    /* compression ratio = uncompressed size /  compressed size */
                                    if (dsize_out != 0)
                                        ratio = (double) dsize_in / (double) dsize_out;
                                    print_dataset_info(dcpl_out, travt->objs[i].name, ratio, 1);
                                }
                                else
                                    print_dataset_info(dcpl_in, travt->objs[i].name, ratio, 0);

                                /* print a message that the filter was not applied
                                 * (in case there was a filter)
                                 */
                                if (has_filter && apply_s == 0)
                                    HDprintf(" <warning: filter not applied to %s. dataset smaller than %d bytes>\n", travt->objs[i].name, (int) options->min_comp);

                                if (has_filter && apply_f == 0)
                                    HDprintf(" <warning: could not apply the filter to %s>\n", travt->objs[i].name);
                            } /* end if verbose (print compression) */

                            /*-------------------------------------------------------------------------
                             * copy attrs
                             *-------------------------------------------------------------------------
                             */
                            if (copy_attr(dset_in, dset_out, &named_dt_head, travt, options) < 0)
                                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "copy_attr failed");

                            if (H5Dclose(dset_out) < 0)
                                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dclose failed");
                        } /* end if not a reference */
                    } /* end if h5tools_canreadf (filter availability check) */

                    /*-------------------------------------------------------------------------
                     * Close
                     *-------------------------------------------------------------------------
                     */
                    if (H5Tclose(ftype_id) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Tclose failed");
                    if (H5Tclose(wtype_id) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Tclose failed");
                    if (H5Pclose(dcpl_in) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pclose failed");
                    if (H5Pclose(dcpl_out) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pclose failed");
                    if (H5Sclose(f_space_id) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Sclose failed");
                    if (H5Dclose(dset_in) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dclose failed");
                }
                /*-------------------------------------------------------------------------
                 * We do not have request for filter/chunking; use H5Ocopy instead
                 *-------------------------------------------------------------------------
                 */
                else {
                    hid_t pid = -1;

                    /* create property to pass copy options */
                    if ((pid = H5Pcreate(H5P_OBJECT_COPY)) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pcreate failed");

                    /* set options for object copy */
                    if (H5Pset_copy_object(pid, H5O_COPY_WITHOUT_ATTR_FLAG) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_copy_object failed");

                    if (H5Ocopy(fidin, /* Source file or group identifier */
                            travt->objs[i].name, /* Name of the source object to be copied */
                            fidout, /* Destination file or group identifier  */
                            travt->objs[i].name, /* Name of the destination object  */
                            pid, /* Properties which apply to the copy   */
                            H5P_DEFAULT) < 0) /* Properties which apply to the new hard link */
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Ocopy failed");

                    if (H5Pclose(pid) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pclose failed");

                    /*-------------------------------------------------------------------------
                     * Copy attrs manually
                     *-------------------------------------------------------------------------
                     */
                    if ((dset_in = H5Dopen2(fidin, travt->objs[i].name, H5P_DEFAULT)) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dopen2 failed");
                    if ((dset_out = H5Dopen2(fidout, travt->objs[i].name, H5P_DEFAULT)) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dopen2 failed");
                    if (copy_attr(dset_in, dset_out, &named_dt_head, travt, options) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "copy_attr failed");
                    if (H5Dclose(dset_in) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dclose failed");
                    if (H5Dclose(dset_out) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dclose failed");

                    if (options->verbose)
                        HDprintf(FORMAT_OBJ, "dset", travt->objs[i].name);

                } /* end whether we have request for filter/chunking */
                break;

            /*-------------------------------------------------------------------------
             * H5TRAV_TYPE_NAMED_DATATYPE
             *-------------------------------------------------------------------------
             */
            case H5TRAV_TYPE_NAMED_DATATYPE:
                if (options->verbose)
                    HDprintf(FORMAT_OBJ, "type", travt->objs[i].name);

                if ((type_in = H5Topen2(fidin, travt->objs[i].name, H5P_DEFAULT)) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Topen2 failed");

                /* Copy the datatype anonymously */
                if ((type_out = copy_named_datatype(type_in, fidout, &named_dt_head, travt, options)) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "copy_named_datatype failed");

                /* Link in to group structure */
                if (H5Lcreate_hard(type_out, ".", fidout, travt->objs[i].name, H5P_DEFAULT, H5P_DEFAULT) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Lcreate_hard failed");

                /*-------------------------------------------------------------------------
                 * copy attrs
                 *-------------------------------------------------------------------------
                 */
                if (copy_attr(type_in, type_out, &named_dt_head, travt, options) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "copy_attr failed");

                if (H5Tclose(type_in) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Tclose failed");
                if (H5Tclose(type_out) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Tclose failed");
                type_out = -1; /* named datatypes stack, named_dt_head, manages allocation */

                break;

            /*-------------------------------------------------------------------------
             * H5TRAV_TYPE_LINK
             * H5TRAV_TYPE_UDLINK
             *
             * Only handles external links; H5Lcopy will fail for other UD link types
             * since we don't have creation or copy callbacks for them.
             *-------------------------------------------------------------------------
             */
            case H5TRAV_TYPE_LINK:
            case H5TRAV_TYPE_UDLINK:
                if (options->verbose)
                    HDprintf(FORMAT_OBJ, "link", travt->objs[i].name);

                if (H5Lcopy(fidin, travt->objs[i].name, fidout, travt->objs[i].name, H5P_DEFAULT, H5P_DEFAULT) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Lcopy failed");

                if (options->verbose)
                    HDprintf(FORMAT_OBJ, "link", travt->objs[i].name);
                break;

            default:
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Object type not found");
            } /* switch */
        } /* end for each object to traverse */
    } /* end if there are objects */

done:

    /* Finalize (link) the stack of named datatypes (if any) first
     * because of reference counting */
    if (0 == ret_value && named_dt_head != NULL) {
        if (named_datatype_free(&named_dt_head, 0) < 0)
            H5TOOLS_INFO(H5E_tools_min_id_g, "named_datatype_free failed");
    }
    else
        H5E_BEGIN_TRY {
            named_datatype_free(&named_dt_head, 1);
        } H5E_END_TRY;

    H5E_BEGIN_TRY
    {
        H5Gclose(grp_in);
        H5Gclose(grp_out);
        H5Pclose(dcpl_in);
        H5Pclose(gcpl_in);
        H5Pclose(gcpl_out);
        H5Sclose(f_space_id);
        H5Dclose(dset_in);
        H5Dclose(dset_out);
        H5Tclose(ftype_id);
        H5Tclose(wtype_id);
        H5Tclose(type_in);
        H5Tclose(type_out);
    } H5E_END_TRY;

    /* free */
    if (buf != NULL)
        HDfree(buf);
    if (hslab_buf != NULL)
        HDfree(hslab_buf);

    return ret_value;
} /* end do_copy_objects() */

/*-------------------------------------------------------------------------
 * Function: print_dataset_info
 *
 * Purpose: print name, filters, percentage compression of a dataset
 *-------------------------------------------------------------------------
 */
static void
print_dataset_info(hid_t dcpl_id, char *objname, double ratio, int pr)
{
    char     strfilter[255];
#if defined (PRINT_DEBUG )
    char     temp[255];
#endif
    int      nfilters;       /* number of filters */
    unsigned filt_flags;     /* filter flags */
    H5Z_filter_t filtn;      /* filter identification number */
    unsigned cd_values[20];  /* filter client data values */
    size_t   cd_nelmts;      /* filter client number of values */
    char     f_objname[256]; /* filter objname */
    int      i;

    HDstrcpy(strfilter, "\0");

    /* get information about input filters */
    if ((nfilters = H5Pget_nfilters(dcpl_id)) < 0)
        return;

    for (i = 0; i < nfilters; i++) {
        cd_nelmts = NELMTS(cd_values);

        if ((filtn = H5Pget_filter2(dcpl_id, (unsigned) i, &filt_flags, &cd_nelmts,
                        cd_values, sizeof(f_objname), f_objname, NULL)) < 0) {
            HDstrcat(strfilter, "ERROR ");
            continue;
        }

        switch (filtn) {
            case H5Z_FILTER_NONE:
                HDstrcat(strfilter, "NONE ");
                break;

            case H5Z_FILTER_DEFLATE:
                HDstrcat(strfilter, "GZIP ");

#if defined (PRINT_DEBUG)
                {
                    unsigned level = cd_values[0];

                    HDsprintf(temp,"(%d)", level);
                    HDstrcat(strfilter, temp);
                }
#endif
                break;

            case H5Z_FILTER_SZIP:
                HDstrcat(strfilter, "SZIP ");

#if defined (PRINT_DEBUG)
                {
                    unsigned options_mask = cd_values[0]; /* from dcpl, not filt*/
                    unsigned ppb = cd_values[1];

                    HDsprintf(temp,"(%d,", ppb);
                    HDstrcat(strfilter, temp);
                    if (options_mask & H5_SZIP_EC_OPTION_MASK)
                        HDstrcpy(temp, "EC) ");
                    else if (options_mask & H5_SZIP_NN_OPTION_MASK)
                        HDstrcpy(temp, "NN) ");
                }
                HDstrcat(strfilter, temp);
#endif
                break;

            case H5Z_FILTER_SHUFFLE:
                HDstrcat(strfilter, "SHUF ");
                break;

            case H5Z_FILTER_FLETCHER32:
                HDstrcat(strfilter, "FLET ");
                break;

            case H5Z_FILTER_NBIT:
                HDstrcat(strfilter, "NBIT ");
                break;

            case H5Z_FILTER_SCALEOFFSET:
                HDstrcat(strfilter, "SCALEOFFSET ");
                break;

            default:
                HDstrcat(strfilter, "UD ");
                break;
        } /* end switch */
    } /* end for each filter */

    if (!pr)
        HDprintf(FORMAT_OBJ, "dset", objname);
    else {
        char str[512], temp[512];

        HDstrcpy(str, "dset     ");
        HDstrcat(str, strfilter);
        HDsprintf(temp, "  (%.3f:1)", ratio);
        HDstrcat(str, temp);
        HDprintf(FORMAT_OBJ, str, objname);
    }
} /* end print_dataset_info() */

/*-------------------------------------------------------------------------
 * Function: copy_user_block
 *
 * Purpose:  copy user block from one file to another
 *
 * Return:   0, ok, -1 no
 *-------------------------------------------------------------------------
 */
static int
copy_user_block(const char *infile, const char *outfile, hsize_t size)
{
    int ret_value = 0;
    int infid = -1, outfid = -1; /* File descriptors */

    /* User block must be any power of 2 equal to 512 or greater (512, 1024, 2048, etc.) */

    /* Open files */
    if ((infid = HDopen(infile, O_RDONLY)) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "HDopen failed input file <%s>", infile);
    if ((outfid = HDopen(outfile, O_WRONLY)) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "HDopen failed output file <%s>", outfile);

    /* Copy the userblock from the input file to the output file */
    while (size > 0) {
        ssize_t nread, nbytes; /* # of bytes transfered, etc. */
        char rbuf[USERBLOCK_XFER_SIZE]; /* Buffer for reading */
        const char *wbuf; /* Pointer into buffer, for writing */

        /* Read buffer from source file */
        if (size > USERBLOCK_XFER_SIZE)
            nread = HDread(infid, rbuf, (size_t)USERBLOCK_XFER_SIZE);
        else
            nread = HDread(infid, rbuf, (size_t)size);
        if (nread < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "HDread failed to read userblock");

        /* Write buffer to destination file */
        /* (compensating for interrupted writes & checking for errors, etc.) */
        nbytes = nread;
        wbuf = rbuf;
        while (nbytes > 0) {
            ssize_t nwritten; /* # of bytes written */

            do {
                nwritten = HDwrite(outfid, wbuf, (size_t)nbytes);
            } while (-1 == nwritten && EINTR == errno);
            if (-1 == nwritten)  /* error */
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "HDwrite failed");
            HDassert(nwritten > 0);
            HDassert(nwritten <= nbytes);

            /* Update # of bytes left & offset in buffer */
            nbytes -= nwritten;
            wbuf += nwritten;
            HDassert(nbytes == 0 || wbuf < (rbuf + USERBLOCK_XFER_SIZE));
        } /* end while */

        /* Update size of userblock left to transfer */
        size = size - (hsize_t) nread;
    } /* end while */

done:
    if (infid >= 0)
        HDclose(infid);
    if (outfid >= 0)
        HDclose(outfid);

    return ret_value;
} /* end copy_user_block() */

/*-------------------------------------------------------------------------
 * Function: print_user_block
 *
 * Purpose:  print user block
 *
 * Return:   0, ok, -1 no
 *-------------------------------------------------------------------------
 */
#if defined (H5REPACK_DEBUG_USER_BLOCK)
static
void
print_user_block(const char *filename, hid_t fid)
{
    int     ret_value = 0;
    int     fh = -1;   /* file handle */
    hsize_t ub_size;   /* user block size */
    hsize_t size;      /* size read */
    hid_t   fcpl = -1; /* file creation property list ID for HDF5 file */
    int     i;

    /* get user block size */
    if ((fcpl = H5Fget_create_plist(fid)) < 0) {
        HGOTO_ERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Fget_create_plist failed to retrieve file creation property list");
    }

    if (H5Pget_userblock(fcpl, &ub_size) < 0) {
        HGOTO_ERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Pget_userblock failed to retrieve userblock size");
    }

    if (H5Pclose(fcpl) < 0) {
        HGOTO_ERROR(H5E_tools_g, H5E_tools_min_id_g, "H5Pclose failed to close property list");
    }

    /* open file */
    if ((fh = HDopen(filename, O_RDONLY)) < 0) {
        HGOTO_ERROR(H5E_tools_g, H5E_tools_min_id_g, "HDopen failed to open file <%s>", filename);
    }

    size = ub_size;

    /* read file */
    while (size > 0) {
        ssize_t nread; /* # of bytes read */
        char rbuf[USERBLOCK_XFER_SIZE]; /* buffer for reading */

        /* read buffer */
        if (size > USERBLOCK_XFER_SIZE)
            nread = HDread(fh, rbuf, (size_t)USERBLOCK_XFER_SIZE);
        else
            nread = HDread(fh, rbuf, (size_t)size);

        for (i = 0; i < nread; i++) {

            HDprintf("%c ", rbuf[i]);

        }
        HDprintf("\n");

        if (nread < 0) {
            HGOTO_ERROR(H5E_tools_g, H5E_tools_min_id_g, "nread < 0");
        }

        /* update size of userblock left to transfer */
        size -= nread;
    }

done:
    if (fh >= 0)
        HDclose(fh);

    return;
} /* end print_user_block() */
#endif

