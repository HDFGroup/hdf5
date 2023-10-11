/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "H5private.h"
#include "h5tools.h"
#include "h5tools_utils.h"
#include "h5diff.h"
#include "ph5diff.h"

/*-------------------------------------------------------------------------
 * Function: diff_dataset
 *
 * Purpose: check for comparable datasets and read into a compatible
 *  memory type
 *
 * Return: Number of differences found
 *-------------------------------------------------------------------------
 */
hsize_t
diff_dataset(hid_t file1_id, hid_t file2_id, const char *obj1_name, const char *obj2_name, diff_opt_t *opts)
{
    int        status = -1;
    hid_t      did1   = H5I_INVALID_HID;
    hid_t      did2   = H5I_INVALID_HID;
    hid_t      dcpl1  = H5I_INVALID_HID;
    hid_t      dcpl2  = H5I_INVALID_HID;
    hsize_t    nfound = 0;
    diff_opt_t diff_opts;
    diff_err_t ret_value = opts->err_stat;

    H5TOOLS_START_DEBUG(" - errstat:%d", opts->err_stat);
    diff_opts             = *opts;
    diff_opts.obj_name[0] = NULL;
    diff_opts.obj_name[1] = NULL;

    H5TOOLS_DEBUG("obj_names: %s - %s", obj1_name, obj2_name);
    /*-------------------------------------------------------------------------
     * open the handles
     *-------------------------------------------------------------------------
     */
    /* Open the datasets */
    if ((did1 = H5Dopen2(file1_id, obj1_name, H5P_DEFAULT)) < 0) {
        parallel_print("Cannot open dataset <%s>\n", obj1_name);
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Dopen2 first dataset failed");
    }
    if ((did2 = H5Dopen2(file2_id, obj2_name, H5P_DEFAULT)) < 0) {
        parallel_print("Cannot open dataset <%s>\n", obj2_name);
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Dopen2 second dataset failed");
    }

    if ((dcpl1 = H5Dget_create_plist(did1)) < 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Dget_create_plist first dataset failed");
    if ((dcpl2 = H5Dget_create_plist(did2)) < 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Dget_create_plist second dataset failed");

    /*-------------------------------------------------------------------------
     * check if the dataset creation property list has filters that
     * are not registered in the current configuration
     * 1) the external filters GZIP and SZIP might not be available
     * 2) the internal filters might be turned off
     *-------------------------------------------------------------------------
     */
    H5TOOLS_DEBUG("h5tools_canreadf then diff_datasetid");
    if ((status = h5tools_canreadf((opts->mode_verbose ? obj1_name : NULL), dcpl1) == 1) &&
        (status = h5tools_canreadf((opts->mode_verbose ? obj2_name : NULL), dcpl2) == 1))
        nfound = diff_datasetid(did1, did2, obj1_name, obj2_name, &diff_opts);
    else if (status < 0) {
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "h5tools_canreadf failed");
    }
    else {
        ret_value         = 1;
        diff_opts.not_cmp = 1;
    }

done:
    opts->print_header = diff_opts.print_header;
    opts->not_cmp      = diff_opts.not_cmp;
    opts->err_stat     = diff_opts.err_stat | ret_value;

    /* disable error reporting */
    H5E_BEGIN_TRY
    {
        H5Pclose(dcpl1);
        H5Pclose(dcpl2);
        H5Dclose(did1);
        H5Dclose(did2);
        /* enable error reporting */
    }
    H5E_END_TRY

    H5TOOLS_ENDDEBUG(":%d - errstat:%d", nfound, opts->err_stat);
    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_datasetid
 *
 * Purpose: check for comparable datasets and read into a compatible
 *  memory type
 *
 * Return: Number of differences found
 *
 * October 2006:  Read by hyperslabs for big datasets.
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
 *-------------------------------------------------------------------------
 */
hsize_t
diff_datasetid(hid_t did1, hid_t did2, const char *obj1_name, const char *obj2_name, diff_opt_t *opts)
{
    hid_t        sid1   = H5I_INVALID_HID;
    hid_t        sid2   = H5I_INVALID_HID;
    hid_t        f_tid1 = H5I_INVALID_HID;
    hid_t        f_tid2 = H5I_INVALID_HID;
    hid_t        m_tid1 = H5I_INVALID_HID;
    hid_t        m_tid2 = H5I_INVALID_HID;
    hid_t        dcpl1  = H5I_INVALID_HID;
    hid_t        dcpl2  = H5I_INVALID_HID;
    H5D_layout_t stl1   = -1;
    H5D_layout_t stl2   = -1;
    size_t       m_size1;
    size_t       m_size2;
    H5T_sign_t   sign1;
    H5T_sign_t   sign2;
    int          rank1;
    int          rank2;
    hsize_t      nelmts1;
    hsize_t      nelmts2;
    hsize_t      dims1[H5S_MAX_RANK];
    hsize_t      dims2[H5S_MAX_RANK];
    hsize_t      maxdim1[H5S_MAX_RANK];
    hsize_t      maxdim2[H5S_MAX_RANK];
    hsize_t      storage_size1;
    hsize_t      storage_size2;
    hsize_t      nfound      = 0; /* number of differences found */
    int          can_compare = 1; /* do diff or not */
    void        *buf1        = NULL;
    void        *buf2        = NULL;
    void        *sm_buf1     = NULL;
    void        *sm_buf2     = NULL;
    hid_t        sm_space1   = H5I_INVALID_HID; /*stripmine data space */
    hid_t        sm_space2   = H5I_INVALID_HID; /*stripmine data space */
    size_t       need;                          /* bytes needed for malloc */
    int          i, j;
    unsigned int vl_data1  = 0; /*contains VL datatypes */
    unsigned int vl_data2  = 0; /*contains VL datatypes */
    diff_err_t   ret_value = opts->err_stat;

    H5TOOLS_START_DEBUG(" - errstat:%d", opts->err_stat);
    /* Get the dataspace handle */
    if ((sid1 = H5Dget_space(did1)) < 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Dget_space failed");

    /* Get rank */
    if ((rank1 = H5Sget_simple_extent_ndims(sid1)) < 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Sget_simple_extent_ndims failed");

    /* Get the dataspace handle */
    if ((sid2 = H5Dget_space(did2)) < 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Dget_space failed");

    /* Get rank */
    if ((rank2 = H5Sget_simple_extent_ndims(sid2)) < 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Sget_simple_extent_ndims failed");

    /* Get dimensions */
    if (H5Sget_simple_extent_dims(sid1, dims1, maxdim1) < 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Sget_simple_extent_dims failed");

    /* Get dimensions */
    if (H5Sget_simple_extent_dims(sid2, dims2, maxdim2) < 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Sget_simple_extent_dims failed");
    H5TOOLS_DEBUG("rank: %ld - %ld", rank1, rank2);

    /*-------------------------------------------------------------------------
     * get the file data type
     *-------------------------------------------------------------------------
     */

    /* Get the data type */
    if ((f_tid1 = H5Dget_type(did1)) < 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Dget_type failed");

    /* Get the data type */
    if ((f_tid2 = H5Dget_type(did2)) < 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Dget_type failed");

    /*-------------------------------------------------------------------------
     * get the storage layout type
     *-------------------------------------------------------------------------
     */
    if ((dcpl1 = H5Dget_create_plist(did1)) < 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Dget_create_plist failed");
    if ((stl1 = H5Pget_layout(dcpl1)) < 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Pget_layout failed");
    H5Pclose(dcpl1);

    if ((dcpl2 = H5Dget_create_plist(did2)) < 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Dget_create_plist failed");
    if ((stl2 = H5Pget_layout(dcpl2)) < 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Pget_layout failed");
    H5Pclose(dcpl2);

    /*-------------------------------------------------------------------------
     * check for empty datasets
     *-------------------------------------------------------------------------
     */
    H5TOOLS_DEBUG("check for empty datasets");

    storage_size1 = H5Dget_storage_size(did1);
    storage_size2 = H5Dget_storage_size(did2);
    H5TOOLS_DEBUG("storage size: %ld - %ld", storage_size1, storage_size2);

    if (storage_size1 == 0 || storage_size2 == 0) {
        if (stl1 == H5D_VIRTUAL || stl2 == H5D_VIRTUAL) {
            if ((opts->mode_verbose || opts->mode_list_not_cmp) && obj1_name && obj2_name)
                parallel_print("Warning: <%s> or <%s> is a virtual dataset\n", obj1_name, obj2_name);
        }
        else {
            if ((opts->mode_verbose || opts->mode_list_not_cmp) && obj1_name && obj2_name)
                parallel_print("Not comparable: <%s> or <%s> is an empty dataset\n", obj1_name, obj2_name);
            can_compare   = 0;
            opts->not_cmp = 1;
        }
    }

    H5TOOLS_DEBUG("obj_names: %s - %s", obj1_name, obj2_name);
    opts->obj_name[0] = NULL;
    if (obj1_name) {
        j = (int)strlen(obj1_name);
        H5TOOLS_DEBUG("obj1_name: %s - %d", obj1_name, j);
        if (j > 0) {
            opts->obj_name[0] = (char *)malloc((size_t)j + 1);
            strncpy(opts->obj_name[0], obj1_name, (size_t)j + 1);
        }
    }

    opts->obj_name[1] = NULL;
    if (obj2_name) {
        j = (int)strlen(obj2_name);
        H5TOOLS_DEBUG("obj2_name: %s - %d", obj2_name, j);
        if (j > 0) {
            opts->obj_name[1] = (char *)malloc((size_t)j + 1);
            strncpy(opts->obj_name[1], obj2_name, (size_t)j + 1);
        }
    }

    /*-------------------------------------------------------------------------
     * check for comparable TYPE and SPACE
     *-------------------------------------------------------------------------
     */
    if (diff_can_type(f_tid1, f_tid2, rank1, rank2, dims1, dims2, maxdim1, maxdim2, opts, 0) != 1)
        can_compare = 0;
    H5TOOLS_DEBUG("diff_can_type returned errstat:%d", opts->err_stat);

    /*-------------------------------------------------------------------------
     * memory type and sizes
     *-------------------------------------------------------------------------
     */
    H5TOOLS_DEBUG("check for memory type and sizes");
    if (H5Tget_class(f_tid1) == H5T_REFERENCE) {
        if ((m_tid1 = H5Tcopy(H5T_STD_REF)) < 0)
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Tcopy(H5T_STD_REF) first ftype failed");
    }
    else {
        if ((m_tid1 = H5Tget_native_type(f_tid1, H5T_DIR_DEFAULT)) < 0)
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Tget_native_type first ftype failed");
    }

    if (H5Tget_class(f_tid2) == H5T_REFERENCE) {
        if ((m_tid2 = H5Tcopy(H5T_STD_REF)) < 0)
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Tcopy(H5T_STD_REF) second ftype failed");
    }
    else {
        if ((m_tid2 = H5Tget_native_type(f_tid2, H5T_DIR_DEFAULT)) < 0)
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Tget_native_type second ftype failed");
    }

    m_size1 = H5Tget_size(m_tid1);
    m_size2 = H5Tget_size(m_tid2);
    H5TOOLS_DEBUG("type size: %ld - %ld", m_size1, m_size2);

    /*-------------------------------------------------------------------------
     * check for different signed/unsigned types
     *-------------------------------------------------------------------------
     */
    if (can_compare) {
        H5TOOLS_DEBUG("can_compare for sign");
        sign1 = H5Tget_sign(m_tid1);
        sign2 = H5Tget_sign(m_tid2);
        if (sign1 != sign2) {
            H5TOOLS_DEBUG("sign1 != sign2");
            if ((opts->mode_verbose || opts->mode_list_not_cmp) && obj1_name && obj2_name) {
                parallel_print("Not comparable: <%s> has sign %s ", obj1_name, get_sign(sign1));
                parallel_print("and <%s> has sign %s\n", obj2_name, get_sign(sign2));
            }

            can_compare   = 0;
            opts->not_cmp = 1;
        }
        H5TOOLS_DEBUG("can_compare for sign - can_compare=%d opts->not_cmp=%d", can_compare, opts->not_cmp);
    }

    /* Check if type is either VLEN-data or VLEN-string to reclaim any
     * VLEN memory buffer later
     */
    if (true == h5tools_detect_vlen(m_tid1))
        vl_data1 = true;
    if (true == h5tools_detect_vlen(m_tid2))
        vl_data2 = true;
    H5TOOLS_DEBUG("h5tools_detect_vlen %d:%d - errstat:%d", vl_data1, vl_data2, opts->err_stat);

    /*------------------------------------------------------------------------
     * only attempt to compare if possible
     *-------------------------------------------------------------------------
     */
    if (can_compare) { /* it is possible to compare */
        H5T_class_t tclass = H5Tget_class(f_tid1);
        H5TOOLS_DEBUG("can_compare attempt");

        /*-----------------------------------------------------------------
         * get number of elements
         *------------------------------------------------------------------
         */
        nelmts1 = 1;
        for (i = 0; i < rank1; i++)
            nelmts1 *= dims1[i];

        nelmts2 = 1;
        for (i = 0; i < rank2; i++)
            nelmts2 *= dims2[i];

        H5TOOLS_DEBUG("nelmts: %ld - %ld", nelmts1, nelmts2);

        if (tclass != H5T_ARRAY) {
            /*-----------------------------------------------------------------
             * "upgrade" the smaller memory size
             *------------------------------------------------------------------
             */
            H5TOOLS_DEBUG("NOT H5T_ARRAY, upgrade the smaller memory size?");
            if (FAIL == match_up_memsize(f_tid1, f_tid2, &m_tid1, &m_tid2, &m_size1, &m_size2))
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "match_up_memsize failed");
            H5TOOLS_DEBUG("m_size: %ld - %ld", m_size1, m_size2);
            opts->rank = rank1;
            for (i = 0; i < rank1; i++)
                opts->dims[i] = dims1[i];
            opts->m_size = m_size1;
            opts->m_tid  = m_tid1;
            opts->nelmts = nelmts1;
            need         = (size_t)(nelmts1 * m_size1); /* bytes needed */
        }
        else {
            H5TOOLS_DEBUG("Array dims: %d - %d", dims1[0], dims2[0]);
            /* Compare the smallest array, but create the largest buffer */
            if (m_size1 <= m_size2) {
                opts->rank = rank1;
                for (i = 0; i < rank1; i++)
                    opts->dims[i] = dims1[i];
                opts->m_size = m_size1;
                opts->m_tid  = m_tid1;
                opts->nelmts = nelmts1;
                need         = (size_t)(nelmts2 * m_size2); /* bytes needed */
            }
            else {
                opts->rank = rank2;
                for (i = 0; i < rank2; i++)
                    opts->dims[i] = dims2[i];
                opts->m_size = m_size2;
                opts->m_tid  = m_tid2;
                opts->nelmts = nelmts2;
                need         = (size_t)(nelmts1 * m_size1); /* bytes needed */
            }
        }
        opts->hs_nelmts = opts->nelmts;
        H5TOOLS_DEBUG("need: %ld", need);
        /* print names */
        H5TOOLS_DEBUG("obj_names: %s - %s", obj1_name, obj2_name);

        if (opts->obj_name[0] != NULL)
            free(opts->obj_name[0]);
        opts->obj_name[0] = NULL;
        if (opts->obj_name[1] != NULL)
            free(opts->obj_name[1]);
        opts->obj_name[1] = NULL;

        if (obj1_name)
            opts->obj_name[0] = strdup(diff_basename(obj1_name));
        if (obj2_name)
            opts->obj_name[1] = strdup(diff_basename(obj2_name));
        H5TOOLS_DEBUG("obj_names: %s - %s", opts->obj_name[0], opts->obj_name[1]);

        H5TOOLS_DEBUG("read/compare");
        /*----------------------------------------------------------------
         * read/compare
         *-----------------------------------------------------------------
         */
        if (need < H5TOOLS_MALLOCSIZE) {
            buf1 = malloc(need);
            buf2 = malloc(need);
        } /* end if */

        /* Assume entire data space to be printed */
        init_acc_pos((unsigned)opts->rank, opts->dims, opts->acc, opts->pos, opts->p_min_idx);

        for (i = 0; i < opts->rank; i++) {
            opts->p_max_idx[i] = opts->dims[i];
        }

        if (buf1 != NULL && buf2 != NULL && opts->sset[0] == NULL && opts->sset[1] == NULL) {
            H5TOOLS_DEBUG("buf1 != NULL && buf2 != NULL");
            H5TOOLS_DEBUG("H5Dread did1");
            if (H5Dread(did1, m_tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf1) < 0)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Dread failed");
            H5TOOLS_DEBUG("H5Dread did2");
            if (H5Dread(did2, m_tid2, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf2) < 0)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Dread failed");

            /* initialize the current stripmine position; this is necessary to print the array indices */
            for (j = 0; j < opts->rank; j++)
                opts->sm_pos[j] = (hsize_t)0;

            /* array diff */
            nfound = diff_array(buf1, buf2, opts, did1, did2);
            H5TOOLS_DEBUG("diff_array ret nfound:%d - errstat:%d", nfound, opts->err_stat);

            /* reclaim any VL memory, if necessary */
            H5TOOLS_DEBUG("check vl_data1:%d", vl_data1);
            if (vl_data1)
                H5Treclaim(m_tid1, sid1, H5P_DEFAULT, buf1);
            H5TOOLS_DEBUG("check vl_data2:%d", vl_data2);
            if (vl_data2)
                H5Treclaim(m_tid2, sid2, H5P_DEFAULT, buf2);
            if (buf1 != NULL) {
                free(buf1);
                buf1 = NULL;
            }
            if (buf2 != NULL) {
                free(buf2);
                buf2 = NULL;
            }
        }                   /* end if */
        else {              /* possibly not enough memory, read/compare by hyperslabs */
            hsize_t elmtno; /* counter  */
            int     carry;  /* counter carry value */

            /* stripmine info */
            hsize_t  sm_size[H5S_MAX_RANK];  /* stripmine size */
            hsize_t  sm_block[H5S_MAX_RANK]; /* stripmine block size  */
            hsize_t  sm_nbytes;              /* bytes per stripmine */
            hsize_t  sm_nelmts1;             /* elements per stripmine */
            hsize_t  sm_nelmts2;             /* elements per stripmine */
            hssize_t ssm_nelmts;             /* elements temp */

            /* hyperslab info */
            hsize_t hs_offset1[H5S_MAX_RANK]; /* starting offset */
            hsize_t hs_count1[H5S_MAX_RANK];  /* number of blocks */
            hsize_t hs_block1[H5S_MAX_RANK];  /* size of blocks */
            hsize_t hs_stride1[H5S_MAX_RANK]; /* stride */
            hsize_t hs_size1[H5S_MAX_RANK];   /* size this pass */
            hsize_t hs_offset2[H5S_MAX_RANK]; /* starting offset */
            hsize_t hs_count2[H5S_MAX_RANK];  /* number of blocks */
            hsize_t hs_block2[H5S_MAX_RANK];  /* size of blocks */
            hsize_t hs_stride2[H5S_MAX_RANK]; /* stride */
            hsize_t hs_size2[H5S_MAX_RANK];   /* size this pass */
            hsize_t hs_nelmts1 = 0;           /* elements in request */
            hsize_t hs_nelmts2 = 0;           /* elements in request */
            hsize_t zero[8];                  /* vector of zeros */
            hsize_t low[H5S_MAX_RANK];        /* low bound of hyperslab */
            hsize_t high[H5S_MAX_RANK];       /* higher bound of hyperslab */

            H5TOOLS_DEBUG("reclaim any VL memory and free unused buffers");
            if (buf1 != NULL) {
                /* reclaim any VL memory, if necessary */
                if (vl_data1)
                    H5Treclaim(m_tid1, sid1, H5P_DEFAULT, buf1);
                free(buf1);
                buf1 = NULL;
            }
            if (buf2 != NULL) {
                /* reclaim any VL memory, if necessary */
                if (vl_data2)
                    H5Treclaim(m_tid2, sid2, H5P_DEFAULT, buf2);
                free(buf2);
                buf2 = NULL;
            }

            /* the stripmine loop */
            memset(hs_offset1, 0, sizeof hs_offset1);
            memset(hs_stride1, 0, sizeof hs_stride1);
            memset(hs_count1, 0, sizeof hs_count1);
            memset(hs_block1, 0, sizeof hs_block1);
            memset(hs_size1, 0, sizeof hs_size1);
            memset(hs_offset2, 0, sizeof hs_offset2);
            memset(hs_stride2, 0, sizeof hs_stride2);
            memset(hs_count2, 0, sizeof hs_count2);
            memset(hs_block2, 0, sizeof hs_block2);
            memset(hs_size2, 0, sizeof hs_size2);
            memset(zero, 0, sizeof zero);

            /* if subsetting was requested - initialize the subsetting variables */
            H5TOOLS_DEBUG("compare by hyperslabs: opts->nelmts=%ld - opts->m_size=%ld", opts->nelmts,
                          opts->m_size);
            if (opts->sset[0] != NULL) {
                H5TOOLS_DEBUG("opts->sset[0] != NULL");

                /* Check for valid settings - default if not specified */
                if (!opts->sset[0]->start.data || !opts->sset[0]->stride.data || !opts->sset[0]->count.data ||
                    !opts->sset[0]->block.data) {
                    /* they didn't specify a ``stride'' or ``block''. default to 1 in all
                     * dimensions */
                    if (!opts->sset[0]->start.data) {
                        /* default to (0, 0, ...) for the start coord */
                        opts->sset[0]->start.data = (hsize_t *)calloc((size_t)rank1, sizeof(hsize_t));
                        opts->sset[0]->start.len  = (unsigned)rank1;
                    }

                    if (!opts->sset[0]->stride.data) {
                        opts->sset[0]->stride.data = (hsize_t *)calloc((size_t)rank1, sizeof(hsize_t));
                        opts->sset[0]->stride.len  = (unsigned)rank1;
                        for (i = 0; i < rank1; i++)
                            opts->sset[0]->stride.data[i] = 1;
                    }

                    if (!opts->sset[0]->count.data) {
                        opts->sset[0]->count.data = (hsize_t *)calloc((size_t)rank1, sizeof(hsize_t));
                        opts->sset[0]->count.len  = (unsigned)rank1;
                        for (i = 0; i < rank1; i++)
                            opts->sset[0]->count.data[i] = 1;
                    }

                    if (!opts->sset[0]->block.data) {
                        opts->sset[0]->block.data = (hsize_t *)calloc((size_t)rank1, sizeof(hsize_t));
                        opts->sset[0]->block.len  = (unsigned)rank1;
                        for (i = 0; i < rank1; i++)
                            opts->sset[0]->block.data[i] = 1;
                    }

                    /*-------------------------------------------------------------------------
                     * check for block overlap
                     *-------------------------------------------------------------------------
                     */
                    for (i = 0; i < rank1; i++) {
                        if (opts->sset[0]->count.data[i] > 1) {
                            if (opts->sset[0]->stride.data[i] < opts->sset[0]->block.data[i]) {
                                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "wrong subset selection[0]; blocks overlap");
                            } /* end if */
                        }     /* end if */
                    }         /* end for */
                }

                /* Reset the total number of elements to the subset from the command */
                opts->nelmts = 1;
                for (i = 0; i < rank1; i++) {
                    hs_offset1[i] = opts->sset[0]->start.data[i];
                    hs_stride1[i] = opts->sset[0]->stride.data[i];
                    hs_count1[i]  = opts->sset[0]->count.data[i];
                    hs_block1[i]  = opts->sset[0]->block.data[i];
                    opts->nelmts *= hs_count1[i] * hs_block1[i];
                    hs_size1[i] = 0;
                    H5TOOLS_DEBUG("[%d]hs_offset1:%ld, hs_stride1:%ld, hs_count1:%ld, hs_block1:%ld", i,
                                  hs_offset1[i], hs_stride1[i], hs_count1[i], hs_block1[i]);
                }
            }
            if (opts->sset[1] != NULL) {
                H5TOOLS_DEBUG("opts->sset[1] != NULL");

                /* Check for valid settings - default if not specified */
                if (!opts->sset[1]->start.data || !opts->sset[1]->stride.data || !opts->sset[1]->count.data ||
                    !opts->sset[1]->block.data) {
                    /* they didn't specify a ``stride'' or ``block''. default to 1 in all
                     * dimensions */
                    if (!opts->sset[1]->start.data) {
                        /* default to (0, 0, ...) for the start coord */
                        opts->sset[1]->start.data = (hsize_t *)calloc((size_t)rank2, sizeof(hsize_t));
                        opts->sset[1]->start.len  = (unsigned)rank2;
                    }

                    if (!opts->sset[1]->stride.data) {
                        opts->sset[1]->stride.data = (hsize_t *)calloc((size_t)rank2, sizeof(hsize_t));
                        opts->sset[1]->stride.len  = (unsigned)rank2;
                        for (i = 0; i < rank2; i++)
                            opts->sset[1]->stride.data[i] = 1;
                    }

                    if (!opts->sset[1]->count.data) {
                        opts->sset[1]->count.data = (hsize_t *)calloc((size_t)rank2, sizeof(hsize_t));
                        opts->sset[1]->count.len  = (unsigned)rank2;
                        for (i = 0; i < rank2; i++)
                            opts->sset[1]->count.data[i] = 1;
                    }

                    if (!opts->sset[1]->block.data) {
                        opts->sset[1]->block.data = (hsize_t *)calloc((size_t)rank2, sizeof(hsize_t));
                        opts->sset[1]->block.len  = (unsigned)rank2;
                        for (i = 0; i < rank2; i++)
                            opts->sset[1]->block.data[i] = 1;
                    }

                    /*-------------------------------------------------------------------------
                     * check for block overlap
                     *-------------------------------------------------------------------------
                     */
                    for (i = 0; i < rank2; i++) {
                        if (opts->sset[1]->count.data[i] > 1) {
                            if (opts->sset[1]->stride.data[i] < opts->sset[1]->block.data[i]) {
                                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "wrong subset selection[1]; blocks overlap");
                            } /* end if */
                        }     /* end if */
                    }         /* end for */
                }

                for (i = 0; i < rank2; i++) {
                    hs_offset2[i] = opts->sset[1]->start.data[i];
                    hs_stride2[i] = opts->sset[1]->stride.data[i];
                    hs_count2[i]  = opts->sset[1]->count.data[i];
                    hs_block2[i]  = opts->sset[1]->block.data[i];
                    hs_size2[i]   = 0;
                    H5TOOLS_DEBUG("[%d]hs_offset2:%ld, hs_stride2:%ld, hs_count2:%ld, hs_block2:%ld", i,
                                  hs_offset2[i], hs_stride2[i], hs_count2[i], hs_block2[i]);
                }
            }

            /*
             * determine the strip mine size and allocate a buffer. The strip mine is
             * a hyperslab whose size is manageable.
             */
            sm_nbytes = opts->m_size;
            if (opts->rank > 0) {
                for (i = opts->rank; i > 0; --i) {
                    hsize_t size = H5TOOLS_BUFSIZE / sm_nbytes;
                    if (size == 0) /* datum size > H5TOOLS_BUFSIZE */
                        size = 1;
                    H5TOOLS_DEBUG("opts->dims[%d]: %ld - size: %ld", i - 1, opts->dims[i - 1], size);
                    if (opts->sset[1] != NULL) {
                        sm_size[i - 1]  = MIN(hs_block1[i - 1] * hs_count1[i - 1], size);
                        sm_block[i - 1] = MIN(hs_block1[i - 1], sm_size[i - 1]);
                    }
                    else {
                        sm_size[i - 1]  = MIN(opts->dims[i - 1], size);
                        sm_block[i - 1] = sm_size[i - 1];
                    }
                    H5TOOLS_DEBUG("sm_size[%d]: %ld - sm_block:%ld", i - 1, sm_size[i - 1], sm_block[i - 1]);
                    sm_nbytes *= sm_size[i - 1];
                    H5TOOLS_DEBUG("sm_nbytes: %ld", sm_nbytes);
                }
            }

            H5TOOLS_DEBUG("opts->nelmts: %ld", opts->nelmts);
            for (elmtno = 0; elmtno < opts->nelmts; elmtno += opts->hs_nelmts) {
                H5TOOLS_DEBUG("elmtno: %ld - hs_nelmts1: %ld", elmtno, hs_nelmts1);

                if (NULL == (sm_buf1 = (unsigned char *)malloc((size_t)sm_nbytes)))
                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Could not allocate buffer for strip-mine");
                if (NULL == (sm_buf2 = (unsigned char *)malloc((size_t)sm_nbytes)))
                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Could not allocate buffer for strip-mine");

                /* calculate the hyperslab size */
                /* initialize subset */
                if (opts->rank > 0) {
                    if (opts->sset[0] != NULL) {
                        H5TOOLS_DEBUG("sset1 has data");
                        /* calculate the potential number of elements */
                        for (i = 0; i < rank1; i++) {
                            H5TOOLS_DEBUG("[%d]opts->dims: %ld - hs_offset1: %ld - sm_block: %ld", i,
                                          opts->dims[i], hs_offset1[i], sm_block[i]);
                            hs_size1[i] = MIN(opts->dims[i] - hs_offset1[i], sm_block[i]);
                            H5TOOLS_DEBUG("hs_size1[%d]: %ld", i, hs_size1[i]);
                        }
                        if (H5Sselect_hyperslab(sid1, H5S_SELECT_SET, hs_offset1, hs_stride1, hs_count1,
                                                hs_size1) < 0)
                            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Sselect_hyperslab sid1 failed");
                    }
                    else {
                        for (i = 0, hs_nelmts1 = 1; i < rank1; i++) {
                            H5TOOLS_DEBUG("[%d]opts->dims: %ld - hs_offset1: %ld - sm_block: %ld", i,
                                          opts->dims[i], hs_offset1[i], sm_block[i]);
                            hs_size1[i] = MIN(opts->dims[i] - hs_offset1[i], sm_block[i]);
                            H5TOOLS_DEBUG("hs_size1[%d]: %ld", i, hs_size1[i]);
                            hs_nelmts1 *= hs_size1[i];
                            H5TOOLS_DEBUG("hs_nelmts1:%ld *= hs_size1[%d]: %ld", hs_nelmts1, i, hs_size1[i]);
                        }
                        if (H5Sselect_hyperslab(sid1, H5S_SELECT_SET, hs_offset1, NULL, hs_size1, NULL) < 0)
                            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Sselect_hyperslab sid1 failed");
                    }

                    if ((ssm_nelmts = H5Sget_select_npoints(sid1)) < 0)
                        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Sget_select_npoints failed");
                    sm_nelmts1 = (hsize_t)ssm_nelmts;
                    H5TOOLS_DEBUG("sm_nelmts1: %ld", sm_nelmts1);
                    hs_nelmts1 = sm_nelmts1;

                    if ((sm_space1 = H5Screate_simple(1, &sm_nelmts1, NULL)) < 0)
                        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Screate_simple failed");

                    if (H5Sselect_hyperslab(sm_space1, H5S_SELECT_SET, zero, NULL, &sm_nelmts1, NULL) < 0)
                        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Sselect_hyperslab failed");

                    if (opts->sset[1] != NULL) {
                        H5TOOLS_DEBUG("sset2 has data");
                        for (i = 0; i < rank2; i++) {
                            H5TOOLS_DEBUG("[%d]opts->dims: %ld - hs_offset2: %ld - sm_block: %ld", i,
                                          opts->dims[i], hs_offset2[i], sm_block[i]);
                            hs_size2[i] = MIN(opts->dims[i] - hs_offset2[i], sm_block[i]);
                            H5TOOLS_DEBUG("hs_size2[%d]: %ld", i, hs_size2[i]);
                        }
                        if (H5Sselect_hyperslab(sid2, H5S_SELECT_SET, hs_offset2, hs_stride2, hs_count2,
                                                hs_size2) < 0)
                            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Sselect_hyperslab sid2 failed");
                    }
                    else {
                        for (i = 0, hs_nelmts2 = 1; i < rank2; i++) {
                            H5TOOLS_DEBUG("[%d]opts->dims: %ld - hs_offset2: %ld - sm_block: %ld", i,
                                          opts->dims[i], hs_offset2[i], sm_block[i]);
                            hs_size2[i] = MIN(opts->dims[i] - hs_offset2[i], sm_block[i]);
                            H5TOOLS_DEBUG("hs_size2[%d]: %ld", i, hs_size2[i]);
                            hs_nelmts2 *= hs_size2[i];
                            H5TOOLS_DEBUG("hs_nelmts2:%ld *= hs_size2[%d]: %ld", hs_nelmts2, i, hs_size2[i]);
                        }
                        if (H5Sselect_hyperslab(sid2, H5S_SELECT_SET, hs_offset2, NULL, hs_size2, NULL) < 0)
                            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Sselect_hyperslab sid2 failed");
                    }

                    if ((ssm_nelmts = H5Sget_select_npoints(sid2)) < 0)
                        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Sget_select_npoints failed");
                    sm_nelmts2 = (hsize_t)ssm_nelmts;
                    H5TOOLS_DEBUG("sm_nelmts2: %ld", sm_nelmts2);
                    hs_nelmts2 = sm_nelmts2;

                    if ((sm_space2 = H5Screate_simple(1, &sm_nelmts2, NULL)) < 0)
                        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Screate_simple failed");

                    if (H5Sselect_hyperslab(sm_space2, H5S_SELECT_SET, zero, NULL, &sm_nelmts2, NULL) < 0)
                        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Sselect_hyperslab failed");
                }
                else
                    hs_nelmts1 = 1;
                opts->hs_nelmts = hs_nelmts1;
                H5TOOLS_DEBUG("hs_nelmts: %ld", opts->hs_nelmts);

                /* read the data */
                if (H5Dread(did1, m_tid1, sm_space1, sid1, H5P_DEFAULT, sm_buf1) < 0)
                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Dread failed");
                if (H5Dread(did2, m_tid2, sm_space2, sid2, H5P_DEFAULT, sm_buf2) < 0)
                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Dread failed");

                /* print array indices. get the lower bound of the hyperslab and calculate
                   the element position at the start of hyperslab */
                if (H5Sget_select_bounds(sid1, low, high) < 0)
                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Sget_select_bounds failed");
                /* initialize the current stripmine position; this is necessary to print the array indices */
                for (j = 0; j < opts->rank; j++)
                    opts->sm_pos[j] = low[j];

                /* Assume entire data space to be printed */
                init_acc_pos((unsigned)opts->rank, opts->dims, opts->acc, opts->pos, opts->p_min_idx);

                /* get array differences. in the case of hyperslab read, increment the number of differences
                found in each hyperslab and pass the position at the beginning for printing */
                nfound += diff_array(sm_buf1, sm_buf2, opts, did1, did2);

                if (sm_buf1 != NULL) {
                    /* reclaim any VL memory, if necessary */
                    if (vl_data1)
                        H5Treclaim(m_tid1, sm_space1, H5P_DEFAULT, sm_buf1);
                    free(sm_buf1);
                    sm_buf1 = NULL;
                }
                if (sm_buf2 != NULL) {
                    /* reclaim any VL memory, if necessary */
                    if (vl_data2)
                        H5Treclaim(m_tid2, sm_space2, H5P_DEFAULT, sm_buf2);
                    free(sm_buf2);
                    sm_buf2 = NULL;
                }

                H5Sclose(sm_space1);
                H5Sclose(sm_space2);

                /* calculate the next hyperslab offset */
                for (i = opts->rank, carry = 1; i > 0 && carry; --i) {
                    if (opts->sset[0] != NULL) {
                        H5TOOLS_DEBUG("[%d]hs_size1:%ld - hs_block1:%ld - hs_stride1:%ld", i - 1,
                                      hs_size1[i - 1], hs_block1[i - 1], hs_stride1[i - 1]);
                        if (hs_size1[i - 1] >= hs_block1[i - 1]) {
                            hs_offset1[i - 1] += hs_size1[i - 1];
                        }
                        else {
                            hs_offset1[i - 1] += hs_stride1[i - 1];
                        }
                    }
                    else
                        hs_offset1[i - 1] += hs_size1[i - 1];
                    H5TOOLS_DEBUG("[%d]hs_offset1:%ld - opts->dims:%ld", i - 1, hs_offset1[i - 1],
                                  opts->dims[i - 1]);
                    if (hs_offset1[i - 1] >= opts->dims[i - 1])
                        hs_offset1[i - 1] = 0;
                    else
                        carry = 0;
                    H5TOOLS_DEBUG("[%d]hs_offset1:%ld", i - 1, hs_offset1[i - 1]);
                    if (opts->sset[1] != NULL) {
                        H5TOOLS_DEBUG("[%d]hs_size2:%ld - hs_block2:%ld - hs_stride2:%ld", i - 1,
                                      hs_size2[i - 1], hs_block2[i - 1], hs_stride2[i - 1]);
                        if (hs_size2[i - 1] >= hs_block2[i - 1]) {
                            hs_offset2[i - 1] += hs_size2[i - 1];
                        }
                        else {
                            hs_offset2[i - 1] += hs_stride2[i - 1];
                        }
                    }
                    else
                        hs_offset2[i - 1] += hs_size2[i - 1];
                    H5TOOLS_DEBUG("[%d]hs_offset2:%ld - opts->dims:%ld", i - 1, hs_offset2[i - 1],
                                  opts->dims[i - 1]);
                    if (hs_offset2[i - 1] >= opts->dims[i - 1])
                        hs_offset2[i - 1] = 0;
                    H5TOOLS_DEBUG("[%d]hs_offset2:%ld", i - 1, hs_offset2[i - 1]);
                }
            } /* elmtno for loop */
        }     /* hyperslab read */
        H5TOOLS_DEBUG("can compare complete");
    } /*can_compare*/

    /*-------------------------------------------------------------------------
     * close
     *-------------------------------------------------------------------------
     */

done:
    opts->err_stat = opts->err_stat | ret_value;

    H5TOOLS_DEBUG("free names - errstat:%d", opts->err_stat);
    /* free */
    if (opts->obj_name[0] != NULL)
        free(opts->obj_name[0]);
    opts->obj_name[0] = NULL;
    if (opts->obj_name[1] != NULL)
        free(opts->obj_name[1]);
    opts->obj_name[1] = NULL;

    H5TOOLS_DEBUG("reclaim any VL memory");
    if (buf1 != NULL) {
        /* reclaim any VL memory, if necessary */
        if (vl_data1)
            H5Treclaim(m_tid1, sid1, H5P_DEFAULT, buf1);
        free(buf1);
        buf1 = NULL;
    }
    if (buf2 != NULL) {
        /* reclaim any VL memory, if necessary */
        if (vl_data2)
            H5Treclaim(m_tid2, sid2, H5P_DEFAULT, buf2);
        free(buf2);
        buf2 = NULL;
    }
    H5TOOLS_DEBUG("reclaim any stripmine VL memory");
    if (sm_buf1 != NULL) {
        /* reclaim any VL memory, if necessary */
        if (vl_data1)
            H5Treclaim(m_tid1, sm_space1, H5P_DEFAULT, sm_buf1);
        free(sm_buf1);
        sm_buf1 = NULL;
    }
    if (sm_buf2 != NULL) {
        /* reclaim any VL memory, if necessary */
        if (vl_data2)
            H5Treclaim(m_tid2, sm_space2, H5P_DEFAULT, sm_buf2);
        free(sm_buf2);
        sm_buf2 = NULL;
    }

    H5TOOLS_DEBUG("close ids");
    /* disable error reporting */
    H5E_BEGIN_TRY
    {
        H5Sclose(sid1);
        H5Sclose(sid2);
        H5Sclose(sm_space1);
        H5Sclose(sm_space2);
        H5Pclose(dcpl1);
        H5Pclose(dcpl2);
        H5Tclose(f_tid1);
        H5Tclose(f_tid2);
        H5Tclose(m_tid1);
        H5Tclose(m_tid2);
        /* enable error reporting */
    }
    H5E_END_TRY

    H5TOOLS_ENDDEBUG(": %d with nfound:%d", ret_value, nfound);
    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_can_type
 *
 * Purpose:  check for comparable TYPE and SPACE
 *
 * Return:
 *           1, can compare
 *           0, cannot compare
 *          -1, error
 *-------------------------------------------------------------------------
 */

int
diff_can_type(hid_t f_tid1, hid_t f_tid2, int rank1, int rank2, hsize_t *dims1, hsize_t *dims2,
              hsize_t *maxdim1, hsize_t *maxdim2, diff_opt_t *opts, int is_compound)
{
    H5T_class_t tclass1;
    H5T_class_t tclass2;
    int         maxdim_diff = 0; /* maximum dimensions are different */
    int         dim_diff    = 0; /* current dimensions are different */
    int         i;
    int         ret_value = 1;

    H5TOOLS_START_DEBUG(" ");
    /*-------------------------------------------------------------------------
     * check for the same class
     *-------------------------------------------------------------------------
     */
    if ((tclass1 = H5Tget_class(f_tid1)) < 0)
        H5TOOLS_GOTO_ERROR(FAIL, "H5Tget_class first object failed");
    if ((tclass2 = H5Tget_class(f_tid2)) < 0)
        H5TOOLS_GOTO_ERROR(FAIL, "H5Tget_class second object failed");

    H5TOOLS_DEBUG("obj_names: %s - %s", opts->obj_name[0], opts->obj_name[1]);
    if (tclass1 != tclass2) {
        if ((opts->mode_verbose || opts->mode_list_not_cmp) && opts->obj_name[0] && opts->obj_name[1]) {
            if (is_compound) {
                parallel_print("Not comparable: <%s> has a class %s and <%s> has a class %s\n",
                               opts->obj_name[0], get_class(tclass1), opts->obj_name[1], get_class(tclass2));
            }
            else {
                parallel_print("Not comparable: <%s> is of class %s and <%s> is of class %s\n",
                               opts->obj_name[0], get_class(tclass1), opts->obj_name[1], get_class(tclass2));
            }
        }

        opts->not_cmp = 1;
        H5TOOLS_GOTO_DONE(0);
    }

    /*-------------------------------------------------------------------------
     * check for non supported classes
     *-------------------------------------------------------------------------
     */
    switch (tclass1) {
        case H5T_TIME:
            if ((opts->mode_verbose || opts->mode_list_not_cmp) && opts->obj_name[0] && opts->obj_name[1]) {
                parallel_print("Not comparable: <%s> and <%s> are of class %s\n", opts->obj_name[0],
                               opts->obj_name[1], get_class(tclass2));
            } /* end if */

            opts->not_cmp = 1;
            H5TOOLS_GOTO_DONE(0);
            break;

        case H5T_INTEGER:
        case H5T_FLOAT:
        case H5T_COMPOUND:
        case H5T_STRING:
        case H5T_ARRAY:
        case H5T_BITFIELD:
        case H5T_OPAQUE:
        case H5T_ENUM:
        case H5T_VLEN:
        case H5T_REFERENCE:
        case H5T_NO_CLASS:
        case H5T_NCLASSES:
        default:
            H5TOOLS_DEBUG("class - %s", get_class(tclass1));
            break;
    } /* end switch */

    /*-------------------------------------------------------------------------
     * check for equal file datatype; warning only
     *-------------------------------------------------------------------------
     */
    if ((H5Tequal(f_tid1, f_tid2) == 0) && (opts->mode_verbose) && opts->obj_name[0] && opts->obj_name[1]) {
        H5T_class_t cl = H5Tget_class(f_tid1);

        parallel_print("Warning: different storage datatype\n");
        if (cl == H5T_INTEGER || cl == H5T_FLOAT) {
            parallel_print("<%s> has file datatype ", opts->obj_name[0]);
            print_type(f_tid1);
            parallel_print("\n");
            parallel_print("<%s> has file datatype ", opts->obj_name[1]);
            print_type(f_tid2);
            parallel_print("\n");
        }
    }

    /*-------------------------------------------------------------------------
     * check for the same rank
     *-------------------------------------------------------------------------
     */
    if (rank1 != rank2) {
        if ((opts->mode_verbose || opts->mode_list_not_cmp) && opts->obj_name[0] && opts->obj_name[1]) {
            parallel_print("Not comparable: <%s> has rank %d, dimensions ", opts->obj_name[0], rank1);
            print_dimensions(rank1, dims1);
            parallel_print(", max dimensions ");
            print_dimensions(rank1, maxdim1);
            parallel_print("\n");
            parallel_print("and <%s> has rank %d, dimensions ", opts->obj_name[1], rank2);
            print_dimensions(rank2, dims2);
            parallel_print(", max dimensions ");
            print_dimensions(rank2, maxdim2);
            parallel_print("\n");
        }

        opts->not_cmp = 1;
        H5TOOLS_GOTO_DONE(0);
    }

    /*-------------------------------------------------------------------------
     * check for different dimensions
     *-------------------------------------------------------------------------
     */
    for (i = 0; i < rank1; i++) {
        if (maxdim1 && maxdim2) {
            if (maxdim1[i] != maxdim2[i])
                maxdim_diff = 1;
        }
        if (dims1[i] != dims2[i])
            dim_diff = 1;
    }

    /*-------------------------------------------------------------------------
     * current dimensions
     *-------------------------------------------------------------------------
     */
    if (dim_diff == 1) {
        if ((opts->mode_verbose || opts->mode_list_not_cmp) && opts->obj_name[0] && opts->obj_name[1]) {
            parallel_print("Not comparable: <%s> has rank %d, dimensions ", opts->obj_name[0], rank1);
            print_dimensions(rank1, dims1);
            if (maxdim1 && maxdim2) {
                parallel_print(", max dimensions ");
                print_dimensions(rank1, maxdim1);
                parallel_print("\n");
                parallel_print("and <%s> has rank %d, dimensions ", opts->obj_name[1], rank2);
                print_dimensions(rank2, dims2);
                parallel_print(", max dimensions ");
                print_dimensions(rank2, maxdim2);
                parallel_print("\n");
            }
        }

        opts->not_cmp = 1;
        H5TOOLS_GOTO_DONE(0);
    }

    /*-------------------------------------------------------------------------
     * maximum dimensions; just give a warning
     *-------------------------------------------------------------------------
     */
    if (maxdim1 && maxdim2 && maxdim_diff == 1 && opts->obj_name[0]) {
        if (opts->mode_verbose) {
            parallel_print("Warning: different maximum dimensions\n");
            parallel_print("<%s> has max dimensions ", opts->obj_name[0]);
            print_dimensions(rank1, maxdim1);
            parallel_print("\n");
            parallel_print("<%s> has max dimensions ", opts->obj_name[1]);
            print_dimensions(rank2, maxdim2);
            parallel_print("\n");
        }
    }

    if (tclass1 == H5T_STRING) {
        htri_t vstrtype1 = -1;
        htri_t vstrtype2 = -1;
        H5TOOLS_DEBUG(" - H5T_STRING");

        vstrtype1 = H5Tis_variable_str(f_tid1);
        vstrtype2 = H5Tis_variable_str(f_tid2);

        /* no compare if either one but not both are variable string type */
        if (vstrtype1 != vstrtype2) {
            if ((opts->mode_verbose || opts->mode_list_not_cmp) && opts->obj_name[0] && opts->obj_name[1])
                parallel_print("Not comparable: <%s> or <%s> is of mixed string type\n", opts->obj_name[0],
                               opts->obj_name[1]);

            opts->not_cmp = 1;
            H5TOOLS_GOTO_DONE(0);
        }
    }

    if (tclass1 == H5T_COMPOUND) {
        int   nmembs1;
        int   nmembs2;
        int   j;
        hid_t memb_type1 = H5I_INVALID_HID;
        hid_t memb_type2 = H5I_INVALID_HID;
        H5TOOLS_DEBUG(" - H5T_COMPOUND");

        nmembs1 = H5Tget_nmembers(f_tid1);
        nmembs2 = H5Tget_nmembers(f_tid2);

        if (nmembs1 != nmembs2) {
            if ((opts->mode_verbose || opts->mode_list_not_cmp) && opts->obj_name[0] && opts->obj_name[1]) {
                parallel_print("Not comparable: <%s> has %d members ", opts->obj_name[0], nmembs1);
                parallel_print("<%s> has %d members ", opts->obj_name[1], nmembs2);
                parallel_print("\n");
            }

            opts->not_cmp = 1;
            H5TOOLS_GOTO_DONE(0);
        }

        for (j = 0; j < nmembs1; j++) {
            memb_type1 = H5Tget_member_type(f_tid1, (unsigned)j);
            memb_type2 = H5Tget_member_type(f_tid2, (unsigned)j);

            if (diff_can_type(memb_type1, memb_type2, rank1, rank2, dims1, dims2, maxdim1, maxdim2, opts,
                              1) != 1) {
                opts->not_cmp = 1;
                H5Tclose(memb_type1);
                H5Tclose(memb_type2);
                H5TOOLS_GOTO_DONE(0);
            }
            H5Tclose(memb_type1);
            H5Tclose(memb_type2);
        }
    }
done:
    if (ret_value < 0)
        opts->err_stat = H5DIFF_ERR;

    H5TOOLS_ENDDEBUG(" - %d", ret_value);
    return ret_value;
}

#if defined(H5DIFF_DEBUG_UNUSED)
/* this function is not currently used, but could be useful */
/*-------------------------------------------------------------------------
 * Function: print_sizes
 *
 * Purpose: Print datatype sizes
 *-------------------------------------------------------------------------
 */
void print_sizes(const char *obj1, const char *obj2, hid_t f_tid1, hid_t f_tid2, hid_t m_tid1, hid_t m_tid2);

void
print_sizes(const char *obj1, const char *obj2, hid_t f_tid1, hid_t f_tid2, hid_t m_tid1, hid_t m_tid2)
{
    size_t f_size1, f_size2; /* size of type in file */
    size_t m_size1, m_size2; /* size of type in memory */

    f_size1 = H5Tget_size(f_tid1);
    f_size2 = H5Tget_size(f_tid2);
    m_size1 = H5Tget_size(m_tid1);
    m_size2 = H5Tget_size(m_tid2);

    parallel_print("\n");
    parallel_print("------------------\n");
    parallel_print("sizeof(char)   %u\n", sizeof(char));
    parallel_print("sizeof(short)  %u\n", sizeof(short));
    parallel_print("sizeof(int)    %u\n", sizeof(int));
    parallel_print("sizeof(long)   %u\n", sizeof(long));
    parallel_print("<%s> ------------------\n", obj1);
    parallel_print("type on file   ");
    print_type(f_tid1);
    parallel_print("\n");
    parallel_print("size on file   %u\n", f_size1);

    parallel_print("type on memory ");
    print_type(m_tid1);
    parallel_print("\n");
    parallel_print("size on memory %u\n", m_size1);

    parallel_print("<%s> ------------------\n", obj2);
    parallel_print("type on file   ");
    print_type(f_tid2);
    parallel_print("\n");
    parallel_print("size on file   %u\n", f_size2);

    parallel_print("type on memory ");
    print_type(m_tid2);
    parallel_print("\n");
    parallel_print("size on memory %u\n", m_size2);
    parallel_print("\n");
}
#endif /* H5DIFF_DEBUG */
