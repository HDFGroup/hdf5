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
hsize_t diff_dataset(hid_t file1_id,
                     hid_t file2_id,
                     const char *obj1_name,
                     const char *obj2_name,
                     diff_opt_t *opts)
{
    int     ret_value = opts->err_stat;
    int     status = -1;
    hid_t   did1 = -1;
    hid_t   did2 = -1;
    hid_t   dcpl1 = -1;
    hid_t   dcpl2 = -1;
    hsize_t nfound = 0;

    h5difftrace("diff_dataset start\n");
    /*-------------------------------------------------------------------------
     * open the handles
     *-------------------------------------------------------------------------
     */
    /* Open the datasets */
    if((did1 = H5Dopen2(file1_id, obj1_name, H5P_DEFAULT)) < 0) {
        parallel_print("Cannot open dataset <%s>\n", obj1_name);
        HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Dopen2 first dataset failed");
    }
    if((did2 = H5Dopen2(file2_id, obj2_name, H5P_DEFAULT)) < 0) {
        parallel_print("Cannot open dataset <%s>\n", obj2_name);
        HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Dopen2 second dataset failed");
    }

    if((dcpl1 = H5Dget_create_plist(did1)) < 0)
        HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Dget_create_plist first dataset failed");
    if((dcpl2 = H5Dget_create_plist(did2)) < 0)
        HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Dget_create_plist second dataset failed");

    /*-------------------------------------------------------------------------
     * check if the dataset creation property list has filters that
     * are not registered in the current configuration
     * 1) the external filters GZIP and SZIP might not be available
     * 2) the internal filters might be turned off
     *-------------------------------------------------------------------------
     */
    if ((status = h5tools_canreadf((opts->m_verbose ? obj1_name : NULL), dcpl1) == 1) &&
            (status = h5tools_canreadf((opts->m_verbose ? obj2_name : NULL), dcpl2) == 1))
        nfound = diff_datasetid(did1, did2, obj1_name, obj2_name, opts);
    else if (status < 0) {
        HGOTO_ERROR(1, H5E_tools_min_id_g, "h5tools_canreadf failed");
    }
    else {
        ret_value = 1;
        opts->not_cmp = 1;
    }

done:
    opts->err_stat = opts->err_stat | ret_value;

    /* disable error reporting */
    H5E_BEGIN_TRY {
        H5Pclose(dcpl1);
        H5Pclose(dcpl2);
        H5Dclose(did1);
        H5Dclose(did2);
        /* enable error reporting */
    } H5E_END_TRY;

    h5diffdebug3("diff_dataset finish:%d - errstat:%d\n", nfound, opts->err_stat);
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
hsize_t diff_datasetid(hid_t did1,
                       hid_t did2,
                       const char *obj1_name,
                       const char *obj2_name,
                       diff_opt_t *opts)
{
    int        ret_value = opts->err_stat;
    hid_t      sid1 = -1;
    hid_t      sid2 = -1;
    hid_t      f_tid1 = -1;
    hid_t      f_tid2 = -1;
    hid_t      dam_tid = -1;             /* m_tid for diff_array function */
    hid_t      m_tid1 = -1;
    hid_t      m_tid2 = -1;
    hid_t      dcpl1 = -1;
    hid_t      dcpl2 = -1;
    H5D_layout_t     stl1 = -1;
    H5D_layout_t     stl2 = -1;
    size_t     dam_size;                /* m_size for diff_array function */
    size_t     m_size1;
    size_t     m_size2;
    H5T_sign_t sign1;
    H5T_sign_t sign2;
    int        rank1;
    int        rank2;
    hsize_t    danelmts;                /* nelmts for diff_array function */
    hsize_t    nelmts1;
    hsize_t    nelmts2;
    hsize_t    *dadims;                 /* dims for diff_array function */
    hsize_t    dims1[H5S_MAX_RANK];
    hsize_t    dims2[H5S_MAX_RANK];
    hsize_t    maxdim1[H5S_MAX_RANK];
    hsize_t    maxdim2[H5S_MAX_RANK];
    const char *name1 = NULL;           /* relative names */
    const char *name2 = NULL;
    hsize_t    storage_size1;
    hsize_t    storage_size2;
    hsize_t    nfound = 0;              /* number of differences found */
    int        can_compare = 1;         /* do diff or not */
    void       *buf1 = NULL;
    void       *buf2 = NULL;
    void       *sm_buf1 = NULL;
    void       *sm_buf2 = NULL;
    hid_t      sm_space = -1;           /*stripmine data space */
    size_t     need;                    /* bytes needed for malloc */
    int        i;
    unsigned int  vl_data1 = 0;          /*contains VL datatypes */
    unsigned int  vl_data2 = 0;          /*contains VL datatypes */

    h5difftrace("diff_datasetid start\n");
    /* Get the dataspace handle */
    if((sid1 = H5Dget_space(did1)) < 0)
        HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Dget_space failed");

    /* Get rank */
    if((rank1 = H5Sget_simple_extent_ndims(sid1)) < 0)
        HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Sget_simple_extent_ndims failed");

    /* Get the dataspace handle */
    if((sid2 = H5Dget_space(did2)) < 0 )
        HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Dget_space failed");

    /* Get rank */
    if((rank2 = H5Sget_simple_extent_ndims(sid2)) < 0)
        HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Sget_simple_extent_ndims failed");

    /* Get dimensions */
    if(H5Sget_simple_extent_dims(sid1, dims1, maxdim1) < 0)
        HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Sget_simple_extent_dims failed");

    /* Get dimensions */
    if(H5Sget_simple_extent_dims(sid2, dims2, maxdim2) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Sget_simple_extent_dims failed");
    h5diffdebug3("rank: %ld - %ld\n", rank1, rank2);

    /*-------------------------------------------------------------------------
     * get the file data type
     *-------------------------------------------------------------------------
     */

    /* Get the data type */
    if((f_tid1 = H5Dget_type(did1)) < 0)
        HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Dget_type failed");

    /* Get the data type */
    if((f_tid2 = H5Dget_type(did2)) < 0)
        HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Dget_type failed");

    /*-------------------------------------------------------------------------
     * get the storage layout type
     *-------------------------------------------------------------------------
     */
    if((dcpl1 = H5Dget_create_plist(did1)) < 0)
        HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Dget_create_plist failed");
    if((dcpl2 = H5Dget_create_plist(did2)) < 0)
        HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Dget_create_plist failed");

    if((stl1 = H5Pget_layout(dcpl1)) < 0)
        HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Pget_layout failed");
    if((stl2 = H5Pget_layout(dcpl2)) < 0)
        HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Pget_layout failed");

    /*-------------------------------------------------------------------------
     * check for empty datasets
     *-------------------------------------------------------------------------
     */
    h5difftrace("check for empty datasets\n");

    storage_size1 = H5Dget_storage_size(did1);
    storage_size2 = H5Dget_storage_size(did2);
    h5diffdebug3("storage size: %ld - %ld\n", storage_size1, storage_size2);

    if(storage_size1 == 0 || storage_size2 == 0) {
        if(stl1 == H5D_VIRTUAL || stl2 == H5D_VIRTUAL) {
            if((opts->m_verbose||opts->m_list_not_cmp) && obj1_name && obj2_name)
                parallel_print("Warning: <%s> or <%s> is a virtual dataset\n", obj1_name, obj2_name);
        }
        else {
            if((opts->m_verbose || opts->m_list_not_cmp) && obj1_name && obj2_name)
                parallel_print("Not comparable: <%s> or <%s> is an empty dataset\n", obj1_name, obj2_name);
            can_compare = 0;
            opts->not_cmp = 1;
        }
    }

    /*-------------------------------------------------------------------------
     * check for comparable TYPE and SPACE
     *-------------------------------------------------------------------------
     */
    if (diff_can_type(f_tid1, f_tid2, rank1, rank2,
            dims1, dims2, maxdim1, maxdim2,
            obj1_name, obj2_name,
            opts, 0) != 1)
        can_compare = 0;
    h5diffdebug2("diff_can_type - errstat:%d\n", opts->err_stat);

    /*-------------------------------------------------------------------------
     * memory type and sizes
     *-------------------------------------------------------------------------
     */
    h5difftrace("check for memory type and sizes\n");
    if((m_tid1 = H5Tget_native_type(f_tid1, H5T_DIR_DEFAULT)) < 0)
        HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Tget_native_type failed");

    if((m_tid2 = H5Tget_native_type(f_tid2, H5T_DIR_DEFAULT)) < 0)
        HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Tget_native_type failed");

    m_size1 = H5Tget_size(m_tid1);
    m_size2 = H5Tget_size(m_tid2);
    h5diffdebug3("type size: %ld - %ld\n", m_size1, m_size2);

    /*-------------------------------------------------------------------------
     * check for different signed/unsigned types
     *-------------------------------------------------------------------------
     */
    if(can_compare) {
        h5difftrace("can_compare for sign\n");
        sign1 = H5Tget_sign(m_tid1);
        sign2 = H5Tget_sign(m_tid2);
        if(sign1 != sign2) {
            h5difftrace("sign1 != sign2\n");
            if((opts->m_verbose || opts->m_list_not_cmp) && obj1_name && obj2_name) {
                parallel_print("Not comparable: <%s> has sign %s ", obj1_name, get_sign(sign1));
                parallel_print("and <%s> has sign %s\n", obj2_name, get_sign(sign2));
            }

            can_compare = 0;
            opts->not_cmp = 1;
        }
    }

    /* Check if type is either VLEN-data or VLEN-string to reclaim any
     * VLEN memory buffer later
     */
    if(TRUE == h5tools_detect_vlen(m_tid1))
        vl_data1 = TRUE;
    if(TRUE == h5tools_detect_vlen(m_tid2))
        vl_data2 = TRUE;
    h5diffdebug2("h5tools_detect_vlen - errstat:%d\n", opts->err_stat);

    /*------------------------------------------------------------------------
     * only attempt to compare if possible
     *-------------------------------------------------------------------------
     */
    if(can_compare) { /* it is possible to compare */
        H5T_class_t  tclass = H5Tget_class(f_tid1);
        h5difftrace("can_compare attempt\n");

        /*-----------------------------------------------------------------
        * get number of elements
        *------------------------------------------------------------------
        */
        nelmts1 = 1;
        for(i = 0; i < rank1; i++)
            nelmts1 *= dims1[i];

        nelmts2 = 1;
        for(i = 0; i < rank2; i++)
            nelmts2 *= dims2[i];

        h5diffdebug3("nelmts: %ld - %ld\n", nelmts1, nelmts2);

        if(tclass != H5T_ARRAY) {
            /*-----------------------------------------------------------------
             * "upgrade" the smaller memory size
             *------------------------------------------------------------------
             */
            h5difftrace("upgrade the smaller memory size?\n");
            if (FAIL == match_up_memsize (f_tid1, f_tid2,
                                        &m_tid1, &m_tid2,
                                        &m_size1, &m_size2))
                HGOTO_ERROR(1, H5E_tools_min_id_g, "match_up_memsize failed");
            h5diffdebug3("m_size: %ld - %ld\n", m_size1, m_size2);
            dadims = dims1;
            dam_size = m_size1;
            dam_tid = m_tid1;
            danelmts = nelmts1;
            need = (size_t)(nelmts1 * m_size1);  /* bytes needed */
        }
        else {
            h5diffdebug3("Array dims: %d - %d\n", dims1[0], dims2[0]);
            /* Compare the smallest array, but create the largest buffer */
            if(m_size1 <= m_size2) {
                dadims = dims1;
                dam_size = m_size1;
                dam_tid = m_tid1;
                danelmts = nelmts1;
                need = (size_t)(nelmts2 * m_size2);  /* bytes needed */
            }
            else {
                dadims = dims2;
                dam_size = m_size2;
                dam_tid = m_tid2;
                danelmts = nelmts2;
                need = (size_t)(nelmts1 * m_size1);  /* bytes needed */
            }
        }
        /* print names */
        if(obj1_name)
            name1 = diff_basename(obj1_name);
        if(obj2_name)
            name2 = diff_basename(obj2_name);
        h5diffdebug3("obj_names: %s - %s\n", name1, name2);


        /*----------------------------------------------------------------
         * read/compare
         *-----------------------------------------------------------------
         */
        if(need < H5TOOLS_MALLOCSIZE) {
            buf1 = HDmalloc(need);
            buf2 = HDmalloc(need);
        } /* end if */

        if(buf1 != NULL && buf2 != NULL) {
            h5difftrace("buf1 != NULL && buf2 != NULL\n");
            if(H5Dread(did1, m_tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf1) < 0)
                HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Dread failed");
            h5difftrace("H5Dread did2\n");
            if(H5Dread(did2, m_tid2, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf2) < 0)
                HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Dread failed");

            /* array diff */
            nfound = diff_array(buf1, buf2, danelmts, (hsize_t)0, rank1, dadims,
                opts, name1, name2, dam_tid, did1, did2);
            h5diffdebug2("diff_array nfound:%d\n", nfound);

            /* reclaim any VL memory, if necessary */
            h5diffdebug2("check vl_data1:%d\n", vl_data1);
            if(vl_data1)
                H5Dvlen_reclaim(m_tid1, sid1, H5P_DEFAULT, buf1);
            h5diffdebug2("check vl_data2:%d\n", vl_data2);
            if(vl_data2)
                H5Dvlen_reclaim(m_tid2, sid2, H5P_DEFAULT, buf2);
            if(buf1 != NULL) {
                HDfree(buf1);
                buf1 = NULL;
            }
            if(buf2 != NULL) {
                HDfree(buf2);
                buf2 = NULL;
            }
        } /* end if */
        else { /* possibly not enough memory, read/compare by hyperslabs */
            size_t        p_type_nbytes = dam_size; /*size of memory type */
            hsize_t       p_nelmts = danelmts;      /*total selected elmts */
            hsize_t       elmtno;                  /*counter  */
            int           carry;                   /*counter carry value */

            /* stripmine info */
            hsize_t       sm_size[H5S_MAX_RANK];   /*stripmine size */
            hsize_t       sm_nbytes;               /*bytes per stripmine */
            hsize_t       sm_nelmts;               /*elements per stripmine*/

            /* hyperslab info */
            hsize_t       hs_offset[H5S_MAX_RANK]; /*starting offset */
            hsize_t       hs_size[H5S_MAX_RANK];   /*size this pass */
            hsize_t       hs_nelmts;               /*elements in request */
            hsize_t       zero[8];                 /*vector of zeros */

            /*
             * determine the strip mine size and allocate a buffer. The strip mine is
             * a hyperslab whose size is manageable.
             */
            sm_nbytes = p_type_nbytes;

            for(i = rank1; i > 0; --i) {
                hsize_t size = H5TOOLS_BUFSIZE / sm_nbytes;

                if(size == 0) /* datum size > H5TOOLS_BUFSIZE */
                    size = 1;
                sm_size[i - 1] = MIN(dadims[i - 1], size);
                sm_nbytes *= sm_size[i - 1];
                h5diffdebug2("sm_nbytes: %ld\n", sm_nbytes);
            } /* end for */

            /* malloc return code should be verified.
             * If fail, need to handle the error.
             * This else branch should be recoded as a separate function.
             * Note that there are many "goto error" within this branch
             * that fails to address freeing other objects created here.
             * E.g., sm_space.
             */
            if((sm_buf1 = HDmalloc((size_t)sm_nbytes)) == NULL)
                HGOTO_ERROR(1, H5E_tools_min_id_g, "HDmalloc failed");
            if((sm_buf2 = HDmalloc((size_t)sm_nbytes)) == NULL)
                HGOTO_ERROR(1, H5E_tools_min_id_g, "HDmalloc failed");

            sm_nelmts = sm_nbytes / p_type_nbytes;
            sm_space = H5Screate_simple(1, &sm_nelmts, NULL);

            /* the stripmine loop */
            HDmemset(hs_offset, 0, sizeof hs_offset);
            HDmemset(zero, 0, sizeof zero);

            for(elmtno = 0; elmtno < p_nelmts; elmtno += hs_nelmts) {
                /* calculate the hyperslab size */
                if(rank1 > 0) {
                    for(i = 0, hs_nelmts = 1; i < rank1; i++) {
                        hs_size[i] = MIN(dadims[i] - hs_offset[i], sm_size[i]);
                        hs_nelmts *= hs_size[i];
                    } /* end for */
                    if(H5Sselect_hyperslab(sid1, H5S_SELECT_SET, hs_offset, NULL, hs_size, NULL) < 0)
                        HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Sselect_hyperslab failed");
                    if(H5Sselect_hyperslab(sid2, H5S_SELECT_SET, hs_offset, NULL, hs_size, NULL) < 0)
                        HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Sselect_hyperslab failed");
                    if(H5Sselect_hyperslab(sm_space, H5S_SELECT_SET, zero, NULL, &hs_nelmts, NULL) < 0)
                        HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Sselect_hyperslab failed");
                } /* end if */
                else
                    hs_nelmts = 1;

                if(H5Dread(did1, m_tid1, sm_space, sid1, H5P_DEFAULT, sm_buf1) < 0)
                    HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Dread failed");
                if(H5Dread(did2, m_tid2, sm_space, sid2, H5P_DEFAULT, sm_buf2) < 0)
                    HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Dread failed");

                /* get array differences. in the case of hyperslab read, increment the number of differences
                found in each hyperslab and pass the position at the beginning for printing */
                nfound += diff_array(sm_buf1, sm_buf2, hs_nelmts, elmtno, rank1,
                        dadims, opts, name1, name2, dam_tid, did1, did2);

                /* reclaim any VL memory, if necessary */
                if(vl_data1)
                    H5Dvlen_reclaim(m_tid1, sm_space, H5P_DEFAULT, sm_buf1);
                if(vl_data2)
                    H5Dvlen_reclaim(m_tid2, sm_space, H5P_DEFAULT, sm_buf2);

                /* calculate the next hyperslab offset */
                for(i = rank1, carry = 1; i > 0 && carry; --i) {
                    hs_offset[i - 1] += hs_size[i - 1];
                    if(hs_offset[i - 1] == dadims[i - 1])
                        hs_offset[i - 1] = 0;
                    else
                        carry = 0;
                } /* i */
            } /* elmtno */
            if(sm_buf1 != NULL) {
                HDfree(sm_buf1);
                sm_buf1 = NULL;
            }
            if(sm_buf2 != NULL) {
                HDfree(sm_buf2);
                sm_buf2 = NULL;
            }

            H5Sclose(sm_space);
        } /* hyperslab read */
    } /*can_compare*/


    /*-------------------------------------------------------------------------
     * close
     *-------------------------------------------------------------------------
     */
    h5diffdebug2("reclaim any VL memory - errstat:%d\n", opts->err_stat);

done:
    opts->err_stat = opts->err_stat | ret_value;

    /* free */
    if(buf1 != NULL) {
        /* reclaim any VL memory, if necessary */
        if(vl_data1)
            H5Dvlen_reclaim(m_tid1, sid1, H5P_DEFAULT, buf1);
        HDfree(buf1);
        buf1 = NULL;
    }
    if(buf2 != NULL) {
        /* reclaim any VL memory, if necessary */
        if(vl_data2)
            H5Dvlen_reclaim(m_tid2, sid2, H5P_DEFAULT, buf2);
        HDfree(buf2);
        buf2 = NULL;
    }
    if(sm_buf1 != NULL) {
        /* reclaim any VL memory, if necessary */
        if(vl_data1)
            H5Dvlen_reclaim(m_tid1, sm_space, H5P_DEFAULT, sm_buf1);
        HDfree(sm_buf1);
        sm_buf1 = NULL;
    }
    if(sm_buf2 != NULL) {
        /* reclaim any VL memory, if necessary */
        if(vl_data2)
            H5Dvlen_reclaim(m_tid2, sm_space, H5P_DEFAULT, sm_buf2);
        HDfree(sm_buf2);
        sm_buf2 = NULL;
    }

    /* disable error reporting */
    H5E_BEGIN_TRY {
        H5Sclose(sid1);
        H5Sclose(sid2);
        H5Sclose(sm_space);
        H5Tclose(f_tid1);
        H5Tclose(f_tid2);
        H5Tclose(m_tid1);
        H5Tclose(m_tid2);
        /* enable error reporting */
    } H5E_END_TRY;

    h5diffdebug3("diff_datasetid return:%d with nfound:%d\n", ret_value, nfound);
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

int diff_can_type(hid_t       f_tid1, /* file data type */
                  hid_t       f_tid2, /* file data type */
                  int         rank1,
                  int         rank2,
                  hsize_t     *dims1,
                  hsize_t     *dims2,
                  hsize_t     *maxdim1,
                  hsize_t     *maxdim2,
                  const char  *obj1_name,
                  const char  *obj2_name,
                  diff_opt_t  *opts,
                  int         is_compound)
{
    int          ret_value = 1;            /* can_compare value */
    H5T_class_t  tclass1;
    H5T_class_t  tclass2;
    int          maxdim_diff = 0;          /* maximum dimensions are different */
    int          dim_diff = 0;             /* current dimensions are different */
    int          i;

    h5difftrace("diff_can_type start\n");
    /*-------------------------------------------------------------------------
     * check for the same class
     *-------------------------------------------------------------------------
     */
    if((tclass1 = H5Tget_class(f_tid1)) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Tget_class first object failed");
    if((tclass2 = H5Tget_class(f_tid2)) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Tget_class second object failed");

    if(tclass1 != tclass2) {
        if((opts->m_verbose || opts->m_list_not_cmp) && obj1_name && obj2_name) {
            if(is_compound) {
                parallel_print("Not comparable: <%s> has a class %s and <%s> has a class %s\n",
                        obj1_name, get_class(tclass1),
                        obj2_name, get_class(tclass2));
            }
            else {
                parallel_print("Not comparable: <%s> is of class %s and <%s> is of class %s\n",
                        obj1_name, get_class(tclass1),
                        obj2_name, get_class(tclass2));
            }
        }
        opts->not_cmp = 1;
        HGOTO_DONE(0);
    }

    /*-------------------------------------------------------------------------
     * check for non supported classes
     *-------------------------------------------------------------------------
     */
    switch (tclass1) {
        case H5T_TIME:
            if((opts->m_verbose || opts->m_list_not_cmp) && obj1_name && obj2_name) {
                parallel_print("Not comparable: <%s> and <%s> are of class %s\n",
                        obj1_name, obj2_name, get_class(tclass2));
            } /* end if */
            opts->not_cmp = 1;
            HGOTO_DONE(0);

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
            h5diffdebug2("diff_can_type class - %s\n", get_class(tclass1));
            break;
    } /* end switch */

    /*-------------------------------------------------------------------------
     * check for equal file datatype; warning only
     *-------------------------------------------------------------------------
     */
    if((H5Tequal(f_tid1, f_tid2) == 0) && (opts->m_verbose) && obj1_name && obj2_name) {
        H5T_class_t cl = H5Tget_class(f_tid1);

        parallel_print("Warning: different storage datatype\n");
        if(cl == H5T_INTEGER || cl == H5T_FLOAT) {
            parallel_print("<%s> has file datatype ", obj1_name);
            print_type(f_tid1);
            parallel_print("\n");
            parallel_print("<%s> has file datatype ", obj2_name);
            print_type(f_tid2);
            parallel_print("\n");
        }
    }

    /*-------------------------------------------------------------------------
     * check for the same rank
     *-------------------------------------------------------------------------
     */
    if(rank1 != rank2) {
        if((opts->m_verbose || opts->m_list_not_cmp) && obj1_name && obj2_name) {
            parallel_print("Not comparable: <%s> has rank %d, dimensions ", obj1_name, rank1);
            print_dimensions(rank1, dims1);
            parallel_print(", max dimensions ");
            print_dimensions(rank1, maxdim1);
            parallel_print("\n" );
            parallel_print("and <%s> has rank %d, dimensions ", obj2_name, rank2);
            print_dimensions(rank2, dims2);
            parallel_print(", max dimensions ");
            print_dimensions(rank2, maxdim2);
            parallel_print("\n");
        }
        opts->not_cmp = 1;
        HGOTO_DONE(0);
    }

    /*-------------------------------------------------------------------------
     * check for different dimensions
     *-------------------------------------------------------------------------
     */
    for(i = 0; i<rank1; i++) {
        if(maxdim1 && maxdim2) {
            if(maxdim1[i] != maxdim2[i])
                maxdim_diff = 1;
        }
        if(dims1[i] != dims2[i])
            dim_diff = 1;
    }

    /*-------------------------------------------------------------------------
     * current dimensions
     *-------------------------------------------------------------------------
     */
    if(dim_diff == 1) {
        if((opts->m_verbose || opts->m_list_not_cmp) && obj1_name && obj2_name) {
            parallel_print("Not comparable: <%s> has rank %d, dimensions ", obj1_name, rank1);
            print_dimensions(rank1, dims1);
            if(maxdim1 && maxdim2) {
                parallel_print(", max dimensions ");
                print_dimensions(rank1, maxdim1);
                parallel_print("\n" );
                parallel_print("and <%s> has rank %d, dimensions ", obj2_name, rank2);
                print_dimensions(rank2, dims2);
                parallel_print(", max dimensions ");
                print_dimensions(rank2, maxdim2);
                parallel_print("\n");
            }
        }
        opts->not_cmp = 1;
        HGOTO_DONE(0);
    }

    /*-------------------------------------------------------------------------
     * maximum dimensions; just give a warning
     *-------------------------------------------------------------------------
     */
    if(maxdim1 && maxdim2 && maxdim_diff == 1 && obj1_name) {
        if(opts->m_verbose) {
            parallel_print( "Warning: different maximum dimensions\n");
            parallel_print("<%s> has max dimensions ", obj1_name);
            print_dimensions(rank1, maxdim1);
            parallel_print("\n");
            parallel_print("<%s> has max dimensions ", obj2_name);
            print_dimensions(rank2, maxdim2);
            parallel_print("\n");
        }
    }

    if(tclass1 == H5T_STRING) {
        htri_t vstrtype1 = -1;
        htri_t vstrtype2 = -1;
        h5difftrace("diff_can_type end - H5T_STRING\n");

        vstrtype1 = H5Tis_variable_str(f_tid1);
        vstrtype2 = H5Tis_variable_str(f_tid2);

        /* no compare if either one but not both are variable string type */
        if (vstrtype1 != vstrtype2) {
            if((opts->m_verbose || opts->m_list_not_cmp) && obj1_name && obj2_name)
                parallel_print("Not comparable: <%s> or <%s> is of mixed string type\n",
                        obj1_name, obj2_name);
            opts->not_cmp = 1;
            HGOTO_DONE(0);
        }
    }

    if(tclass1 == H5T_COMPOUND) {
        int   nmembs1;
        int   nmembs2;
        int   j;
        hid_t memb_type1 = -1;
        hid_t memb_type2 = -1;
        h5difftrace("diff_can_type end - H5T_COMPOUND\n");

        nmembs1 = H5Tget_nmembers(f_tid1);
        nmembs2 = H5Tget_nmembers(f_tid2);

        if(nmembs1 != nmembs2) {
            if((opts->m_verbose || opts->m_list_not_cmp) && obj1_name && obj2_name) {
                parallel_print("Not comparable: <%s> has %d members ", obj1_name, nmembs1);
                parallel_print("<%s> has %d members ", obj2_name, nmembs2);
                parallel_print("\n");
            }
            opts->not_cmp = 1;
            HGOTO_DONE(0);
        }

        for (j = 0; j < nmembs1; j++) {
            memb_type1 = H5Tget_member_type(f_tid1, (unsigned)j);
            memb_type2 = H5Tget_member_type(f_tid2, (unsigned)j);

            if (diff_can_type(memb_type1, memb_type2, rank1, rank2,
                    dims1, dims2, maxdim1, maxdim2, obj1_name, obj2_name,
                    opts, 1) != 1) {
                opts->not_cmp = 1;
                H5Tclose(memb_type1);
                H5Tclose(memb_type2);
                HGOTO_DONE(0);
            }
            H5Tclose(memb_type1);
            H5Tclose(memb_type2);
        }
    }
done:
    if (ret_value < 0)
        opts->err_stat = 1;

    h5diffdebug2("diff_can_type end - %d\n", ret_value);
    return ret_value;
}


/*-------------------------------------------------------------------------
 * Function: print_sizes
 *
 * Purpose: Print datatype sizes
 *-------------------------------------------------------------------------
 */
#if defined (H5DIFF_DEBUG)
void print_sizes( const char *obj1,
                  const char *obj2,
                  hid_t f_tid1,
                  hid_t f_tid2,
                  hid_t m_tid1,
                  hid_t m_tid2 )
{
    size_t  f_size1, f_size2;       /* size of type in file */
    size_t  m_size1, m_size2;       /* size of type in memory */

    f_size1 = H5Tget_size( f_tid1 );
    f_size2 = H5Tget_size( f_tid2 );
    m_size1 = H5Tget_size( m_tid1 );
    m_size2 = H5Tget_size( m_tid2 );

    parallel_print("\n");
    parallel_print("------------------\n");
    parallel_print("sizeof(char)   %u\n", sizeof(char) );
    parallel_print("sizeof(short)  %u\n", sizeof(short) );
    parallel_print("sizeof(int)    %u\n", sizeof(int) );
    parallel_print("sizeof(long)   %u\n", sizeof(long) );
    parallel_print("<%s> ------------------\n", obj1);
    parallel_print("type on file   ");
    print_type(f_tid1);
    parallel_print("\n");
    parallel_print("size on file   %u\n", f_size1 );

    parallel_print("type on memory ");
    print_type(m_tid1);
    parallel_print("\n");
    parallel_print("size on memory %u\n", m_size1 );

    parallel_print("<%s> ------------------\n", obj2);
    parallel_print("type on file   ");
    print_type(f_tid2);
    parallel_print("\n");
    parallel_print("size on file   %u\n", f_size2 );

    parallel_print("type on memory ");
    print_type(m_tid2);
    parallel_print("\n");
    parallel_print("size on memory %u\n", m_size2 );
    parallel_print("\n");
}
#endif /* H5DIFF_DEBUG */
