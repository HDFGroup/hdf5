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

#define ATTR_NAME_MAX 255

typedef struct table_attr_t {
    char    *name;
    unsigned exist[2];
} match_attr_t;

typedef struct table_attrs_t {
    size_t        size;
    size_t        nattrs;
    size_t        nattrs_only1;
    size_t        nattrs_only2;
    match_attr_t *attrs;
} table_attrs_t;

/*-------------------------------------------------------------------------
 * Function: table_attrs_init
 *
 * Purpose: Initialize the table
 *
 * Parameter:
 *  - tbl [OUT]
 *
 *------------------------------------------------------------------------*/
static void
table_attrs_init(table_attrs_t **tbl)
{
    table_attrs_t *table_attrs = (table_attrs_t *)malloc(sizeof(table_attrs_t));

    table_attrs->size         = 0;
    table_attrs->nattrs       = 0;
    table_attrs->nattrs_only1 = 0;
    table_attrs->nattrs_only2 = 0;
    table_attrs->attrs        = NULL;

    *tbl = table_attrs;
}

/*-------------------------------------------------------------------------
 * Function: table_attrs_free
 *
 * Purpose: free given table
 *
 * Parameter:
 *  - table [IN]
 *
 *------------------------------------------------------------------------*/
static void
table_attrs_free(table_attrs_t *table)
{
    unsigned int i;

    if (table) {
        if (table->attrs) {
            for (i = 0; i < table->nattrs; i++) {
                if (table->attrs[i].name) {
                    free(table->attrs[i].name);
                }
            } /* end for */
            free(table->attrs);
            table->attrs = NULL;
        } /* end if */
        free(table);
        table = NULL;
    }
}

/*-------------------------------------------------------------------------
 * Function: table_attr_mark_exist
 *
 * Purpose: mark given attribute name to table as sign of exist
 *
 * Parameter:
 *  - exist [IN]
 *  - name [IN]  : attribute name
 *  - table [OUT]
 *
 *------------------------------------------------------------------------*/
static void
table_attr_mark_exist(const unsigned *exist, char *name, table_attrs_t *table)
{
    if (table->nattrs == table->size) {
        match_attr_t *new_attrs;

        table->size = MAX(1, table->size * 2);
        new_attrs   = (match_attr_t *)realloc(table->attrs, table->size * sizeof(match_attr_t));
        if (new_attrs)
            table->attrs = new_attrs;
    } /* end if */

    if (table->nattrs < table->size) {
        size_t curr_val;

        curr_val                        = table->nattrs;
        table->attrs[curr_val].exist[0] = exist[0];
        table->attrs[curr_val].exist[1] = exist[1];
        if (name)
            table->attrs[curr_val].name = (char *)strdup(name);
        table->nattrs++;
    }
}

/*-------------------------------------------------------------------------
 * Function: build_match_list_attrs
 *
 * Purpose: get list of matching attribute name from obj1 and obj2
 *
 * Note:
 *  Find common attribute; the algorithm for search is referred from
 *  build_match_list() in h5diff.c .
 *
 * Parameter:
 *  table_out [OUT] : return the list
 *------------------------------------------------------------------------*/
static herr_t
build_match_list_attrs(hid_t loc1_id, hid_t loc2_id, table_attrs_t **table_out, diff_opt_t *opts)
{
    table_attrs_t *table_lp = NULL;
    H5O_info2_t    oinfo1, oinfo2;             /* Object info */
    hid_t          attr1_id = H5I_INVALID_HID; /* attr ID */
    hid_t          attr2_id = H5I_INVALID_HID; /* attr ID */
    size_t         curr1    = 0;
    size_t         curr2    = 0;
    unsigned       infile[2];
    char           name1[ATTR_NAME_MAX];
    char           name2[ATTR_NAME_MAX];
    int            cmp;
    unsigned       i;
    herr_t         ret_value = SUCCEED;

    H5TOOLS_START_DEBUG(" - errstat:%d", opts->err_stat);

    if (H5Oget_info3(loc1_id, &oinfo1, H5O_INFO_NUM_ATTRS) < 0) {
        H5TOOLS_GOTO_ERROR(FAIL, "H5Oget_info first object failed");
    }
    H5TOOLS_DEBUG("H5Oget_info3 loc1id=%d", oinfo1.num_attrs);
    if (H5Oget_info3(loc2_id, &oinfo2, H5O_INFO_NUM_ATTRS) < 0) {
        H5TOOLS_GOTO_ERROR(FAIL, "H5Oget_info second object failed");
    }
    H5TOOLS_DEBUG("H5Oget_info3 loc2id=%d", oinfo2.num_attrs);

    table_attrs_init(&table_lp);
    if (table_lp == NULL)
        H5TOOLS_GOTO_ERROR(FAIL, "Table allocation failed");

    /*--------------------------------------------------
     * build the list
     */
    while (curr1 < oinfo1.num_attrs && curr2 < oinfo2.num_attrs) {
        H5TOOLS_DEBUG("list_attrs 1: %ld - %ld", curr1, oinfo1.num_attrs);
        H5TOOLS_DEBUG("list_attrs 2: %ld - %ld", curr2, oinfo2.num_attrs);

        /*------------------
         * open attribute1 */
        if ((attr1_id = H5Aopen_by_idx(loc1_id, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)curr1, H5P_DEFAULT,
                                       H5P_DEFAULT)) < 0)
            H5TOOLS_GOTO_ERROR(FAIL, "H5Aopen_by_idx first attribute failed");
        /* get name */
        if (H5Aget_name(attr1_id, (size_t)ATTR_NAME_MAX, name1) < 0)
            H5TOOLS_GOTO_ERROR(FAIL, "H5Aget_name first attribute failed");

        /*------------------
         * open attribute2 */
        if ((attr2_id = H5Aopen_by_idx(loc2_id, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)curr2, H5P_DEFAULT,
                                       H5P_DEFAULT)) < 0)
            H5TOOLS_GOTO_ERROR(FAIL, "H5Aopen_by_idx second attribute failed");
        /* get name */
        if (H5Aget_name(attr2_id, (size_t)ATTR_NAME_MAX, name2) < 0)
            H5TOOLS_GOTO_ERROR(FAIL, "H5Aget_name second attribute failed");

        /* criteria is string compare */
        cmp = strcmp(name1, name2);

        if (cmp == 0) {
            infile[0] = 1;
            infile[1] = 1;
            table_attr_mark_exist(infile, name1, table_lp);
            curr1++;
            curr2++;
        }
        else if (cmp < 0) {
            infile[0] = 1;
            infile[1] = 0;
            table_attr_mark_exist(infile, name1, table_lp);
            table_lp->nattrs_only1++;
            curr1++;
        }
        else {
            infile[0] = 0;
            infile[1] = 1;
            table_attr_mark_exist(infile, name2, table_lp);
            table_lp->nattrs_only2++;
            curr2++;
        }

        /* close for next turn */
        H5Aclose(attr1_id);
        attr1_id = H5I_INVALID_HID;
        H5Aclose(attr2_id);
        attr2_id = H5I_INVALID_HID;
    } /* end while */

    /* list1 did not end */
    infile[0] = 1;
    infile[1] = 0;
    while (curr1 < oinfo1.num_attrs) {
        H5TOOLS_DEBUG("list_attrs 1: %ld - %ld", curr1, oinfo1.num_attrs);

        /*------------------
         * open attribute1 */
        if ((attr1_id = H5Aopen_by_idx(loc1_id, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)curr1, H5P_DEFAULT,
                                       H5P_DEFAULT)) < 0)
            H5TOOLS_GOTO_ERROR(FAIL, "H5Aopen_by_idx first attribute failed");
        /* get name */
        if (H5Aget_name(attr1_id, (size_t)ATTR_NAME_MAX, name1) < 0)
            H5TOOLS_GOTO_ERROR(FAIL, "H5Aget_name first attribute failed");
        H5TOOLS_DEBUG("list_attrs 1 name - %s", name1);

        table_attr_mark_exist(infile, name1, table_lp);
        table_lp->nattrs_only1++;
        curr1++;

        /* close for next turn */
        H5Aclose(attr1_id);
        attr1_id = H5I_INVALID_HID;
    }

    /* list2 did not end */
    infile[0] = 0;
    infile[1] = 1;
    while (curr2 < oinfo2.num_attrs) {
        H5TOOLS_DEBUG("list_attrs 2: %ld - %ld", curr2, oinfo2.num_attrs);
        /*------------------
         * open attribute2 */
        if ((attr2_id = H5Aopen_by_idx(loc2_id, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)curr2, H5P_DEFAULT,
                                       H5P_DEFAULT)) < 0)
            H5TOOLS_GOTO_ERROR(FAIL, "H5Aopen_by_idx second attribute failed");
        /* get name */
        if (H5Aget_name(attr2_id, (size_t)ATTR_NAME_MAX, name2) < 0)
            H5TOOLS_GOTO_ERROR(FAIL, "H5Aget_name second attribute failed");
        H5TOOLS_DEBUG("list_attrs 2 name - %s", name2);

        table_attr_mark_exist(infile, name2, table_lp);
        table_lp->nattrs_only2++;
        curr2++;

        /* close for next turn */
        H5Aclose(attr2_id);
        attr2_id = H5I_INVALID_HID;
    }

    /*------------------------------------------------------
     * print the list
     */
    if (opts->mode_verbose_level == 2) {
        /* if '-v2' is detected */
        parallel_print("   obj1   obj2\n");
        parallel_print(" --------------------------------------\n");
        for (i = 0; i < (unsigned int)table_lp->nattrs; i++) {
            int c1, c2;
            c1 = (table_lp->attrs[i].exist[0]) ? 'x' : ' ';
            c2 = (table_lp->attrs[i].exist[1]) ? 'x' : ' ';
            parallel_print("%5c %6c    %-15s\n", c1, c2, table_lp->attrs[i].name);
        } /* end for */
    }

    if (opts->mode_verbose_level >= 1)
        parallel_print("Attributes status:  %zu common, %zu only in obj1, %zu only in obj2\n",
                       table_lp->nattrs - table_lp->nattrs_only1 - table_lp->nattrs_only2,
                       table_lp->nattrs_only1, table_lp->nattrs_only2);

done:
    *table_out = table_lp;

    /* disable error reporting */
    H5E_BEGIN_TRY
    {
        H5Aclose(attr1_id);
        H5Aclose(attr2_id);
    }
    H5E_END_TRY

    H5TOOLS_ENDDEBUG(" - errstat:%d", opts->err_stat);

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: diff_attr_data
 *
 * Purpose:  compare attribute data located in attr1_id and attr2_id, which are
 *           obtained from open attributes
 *
 * Return:   number of differences found
 *-------------------------------------------------------------------------
 */

hsize_t
diff_attr_data(hid_t attr1_id, hid_t attr2_id, const char *name1, const char *name2, const char *path1,
               const char *path2, diff_opt_t *opts)
{
    hid_t      space1_id = H5I_INVALID_HID; /* space ID */
    hid_t      space2_id = H5I_INVALID_HID; /* space ID */
    hid_t      ftype1_id = H5I_INVALID_HID; /* file data type ID */
    hid_t      ftype2_id = H5I_INVALID_HID; /* file data type ID */
    hid_t      mtype1_id = H5I_INVALID_HID; /* memory data type ID */
    hid_t      mtype2_id = H5I_INVALID_HID; /* memory data type ID */
    size_t     msize1;                      /* memory size of memory type */
    size_t     msize2;                      /* memory size of memory type */
    void      *buf1        = NULL;          /* data buffer */
    void      *buf2        = NULL;          /* data buffer */
    bool       buf1hasdata = false;         /* buffer has data */
    bool       buf2hasdata = false;         /* buffer has data */
    int        rank1;                       /* rank of dataset */
    int        rank2;                       /* rank of dataset */
    hsize_t    dims1[H5S_MAX_RANK];         /* dimensions of dataset */
    hsize_t    dims2[H5S_MAX_RANK];         /* dimensions of dataset */
    hsize_t    nfound = 0;
    size_t     sz;
    diff_err_t ret_value = opts->err_stat;

    H5TOOLS_START_DEBUG(" - errstat:%d", opts->err_stat);
    /* get the datatypes  */
    if ((ftype1_id = H5Aget_type(attr1_id)) < 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Aget_type first attribute failed");
    if ((ftype2_id = H5Aget_type(attr2_id)) < 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Aget_type second attribute failed");

    if (H5Tget_class(ftype1_id) == H5T_REFERENCE) {
        if ((mtype1_id = H5Tcopy(H5T_STD_REF)) < 0)
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Tcopy(H5T_STD_REF) first attribute ftype failed");
    }
    else {
        if ((mtype1_id = H5Tget_native_type(ftype1_id, H5T_DIR_DEFAULT)) < 0)
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Tget_native_type first attribute ftype failed");
    }
    if (H5Tget_class(ftype2_id) == H5T_REFERENCE) {
        if ((mtype2_id = H5Tcopy(H5T_STD_REF)) < 0)
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Tcopy(H5T_STD_REF) second attribute ftype failed");
    }
    else {
        if ((mtype2_id = H5Tget_native_type(ftype2_id, H5T_DIR_DEFAULT)) < 0)
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Tget_native_type second attribute ftype failed");
    }
    if ((msize1 = H5Tget_size(mtype1_id)) == 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Tget_size first attribute mtype failed");
    if ((msize2 = H5Tget_size(mtype2_id)) == 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Tget_size second attribute mtype failed");

    /* get the dataspace   */
    if ((space1_id = H5Aget_space(attr1_id)) < 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Aget_space first attribute failed");
    if ((space2_id = H5Aget_space(attr2_id)) < 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Aget_space second attribute failed");

    /* get dimensions  */
    if ((rank1 = H5Sget_simple_extent_dims(space1_id, dims1, NULL)) < 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Sget_simple_extent_dims first attribute failed");
    if ((rank2 = H5Sget_simple_extent_dims(space2_id, dims2, NULL)) < 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Sget_simple_extent_dims second attribute failed");

    /*----------------------------------------------------------------------
     * check for comparable TYPE and SPACE
     *----------------------------------------------------------------------
     */
    H5TOOLS_DEBUG("Check for comparable TYPE and SPACE");

    H5TOOLS_DEBUG("attr_names: %s - %s", name1, name2);
    if (name1) {
        sz = strlen(name1);
        H5TOOLS_DEBUG("attr1_name: %s - %d", name1, sz);
        if (sz > 0) {
            opts->obj_name[0] = (char *)malloc(sz + 1);
            strncpy(opts->obj_name[0], name1, sz + 1);
        }
    }
    if (name2) {
        sz = strlen(name2);
        H5TOOLS_DEBUG("attr2_name: %s - %d", name2, sz);
        if (sz > 0) {
            opts->obj_name[1] = (char *)malloc(sz + 1);
            strncpy(opts->obj_name[1], name2, sz + 1);
        }
    }
    H5TOOLS_DEBUG("attr_names: %s - %s", opts->obj_name[0], opts->obj_name[1]);

    /* pass dims1 and dims2 for maxdims as well since attribute's maxdims
     * are always same */
    if (diff_can_type(ftype1_id, ftype2_id, rank1, rank2, dims1, dims2, dims1, dims2, opts, 0) == 1) {

        int j;

        /*-----------------------------------------------------------------
         * "upgrade" the smaller memory size
         *------------------------------------------------------------------
         */
        if (FAIL == match_up_memsize(ftype1_id, ftype2_id, &mtype1_id, &mtype2_id, &msize1, &msize2))
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "match_up_memsize failed");

        H5TOOLS_DEBUG("initialize read");
        /*---------------------------------------------------------------------
         * initialize diff_opt_t structure for dimensions
         *----------------------------------------------------------------------
         */
        opts->nelmts = 1;
        for (j = 0; j < rank1; j++) {
            opts->dims[j] = dims1[j];
            opts->nelmts *= dims1[j];
        }
        opts->rank = rank1;
        init_acc_pos((unsigned)opts->rank, opts->dims, opts->acc, opts->pos, opts->p_min_idx);

        /*---------------------------------------------------------------------
         * read
         *----------------------------------------------------------------------
         */
        buf1 = (void *)calloc((size_t)(opts->nelmts), msize1);
        buf2 = (void *)calloc((size_t)(opts->nelmts), msize2);
        H5TOOLS_DEBUG("attr buffer size %ld * %ld", opts->nelmts, msize1);
        if (buf1 == NULL || buf2 == NULL) {
            parallel_print("cannot read into memory\n");
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "buffer allocation failed");
        }
        if (H5Aread(attr1_id, mtype1_id, buf1) < 0) {
            parallel_print("Failed reading attribute1 %s\n", name1);
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Aget_type first attribute failed");
        }
        else
            buf1hasdata = true;
        H5TOOLS_DEBUG("attr H5Aread 1");

        if (H5Aread(attr2_id, mtype2_id, buf2) < 0) {
            parallel_print("Failed reading attribute2 %s\n", name2);
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Aget_type second attribute failed");
        }
        else
            buf2hasdata = true;
        H5TOOLS_DEBUG("attr H5Aread 2");

        /* format output string */
        if (opts->obj_name[0] != NULL)
            free(opts->obj_name[0]);
        opts->obj_name[0] = NULL;
        if (opts->obj_name[1] != NULL)
            free(opts->obj_name[1]);
        opts->obj_name[1] = NULL;

        H5TOOLS_DEBUG("attr_names: %s - %s : %s - %s", name1, name2, path1, path2);
        if (name1) {
            sz = strlen(name1) + strlen(path1) + 7;
            H5TOOLS_DEBUG("attr1_name: %s - %d", name1, sz);
            opts->obj_name[0] = (char *)calloc(sz + 1, sizeof(char));
            snprintf(opts->obj_name[0], sz, "%s of <%s>", name1, path1);
            opts->obj_name[0][sz] = '\0';
        }
        if (name2) {
            sz = strlen(name2) + strlen(path2) + 7;
            H5TOOLS_DEBUG("attr2_name: %s - %d", name2, sz);
            opts->obj_name[1] = (char *)calloc(sz + 1, sizeof(char));
            snprintf(opts->obj_name[1], sz, "%s of <%s>", name2, path2);
            opts->obj_name[1][sz] = '\0';
        }

        /*---------------------------------------------------------------------
         * array compare
         *----------------------------------------------------------------------
         */
        H5TOOLS_DEBUG("array compare %s - %s", opts->obj_name[0], opts->obj_name[1]);

        opts->hs_nelmts = opts->nelmts;
        opts->m_tid     = mtype1_id;

        /* initialize the current stripmine position; this is necessary to print the array indices */
        for (j = 0; j < opts->rank; j++)
            opts->sm_pos[j] = (hsize_t)0;

        /* always print name */
        /* verbose (-v) and report (-r) mode */
        if (opts->mode_verbose || opts->mode_report) {
            do_print_attrname("attribute", opts->obj_name[0], opts->obj_name[1]);

            nfound = diff_array(buf1, buf2, opts, attr1_id, attr2_id);
            print_found(nfound);
        }
        /* quiet mode (-q), just count differences */
        else if (opts->mode_quiet) {
            nfound = diff_array(buf1, buf2, opts, attr1_id, attr2_id);
        }
        /* the rest (-c, none, ...) */
        else {
            nfound = diff_array(buf1, buf2, opts, attr1_id, attr2_id);

            /* print info if compatible and difference found */
            if (nfound) {
                do_print_attrname("attribute", opts->obj_name[0], opts->obj_name[1]);
                print_found(nfound);
            } /* end if */
        }     /* end else */
    }
    H5TOOLS_DEBUG("check for comparable TYPE and SPACE complete nfound:%d - errstat:%d", nfound,
                  opts->err_stat);

    /*----------------------------------------------------------------------
     * close
     *----------------------------------------------------------------------
     */
    if (opts->obj_name[0] != NULL)
        free(opts->obj_name[0]);
    opts->obj_name[0] = NULL;
    if (opts->obj_name[1] != NULL)
        free(opts->obj_name[1]);
    opts->obj_name[1] = NULL;

    /* Free buf1 and buf2, check both VLEN-data VLEN-string to reclaim any
     * VLEN memory first */
    if (true == h5tools_detect_vlen(mtype1_id))
        H5Treclaim(mtype1_id, space1_id, H5P_DEFAULT, buf1);
    free(buf1);
    buf1 = NULL;

    if (true == h5tools_detect_vlen(mtype2_id))
        H5Treclaim(mtype2_id, space2_id, H5P_DEFAULT, buf2);
    free(buf2);
    buf2 = NULL;

    if (H5Tclose(ftype1_id) < 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Aget_type first attribute failed");
    if (H5Tclose(ftype2_id) < 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Aget_type second attribute failed");
    if (H5Sclose(space1_id) < 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Aget_type first attribute failed");
    if (H5Sclose(space2_id) < 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Aget_type second attribute failed");
    if (H5Tclose(mtype1_id) < 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Tclose first attribute mtype failed");
    if (H5Tclose(mtype2_id) < 0)
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Tclose second attribute mtype failed");

done:
    opts->err_stat = opts->err_stat | ret_value;

    H5E_BEGIN_TRY
    {
        if (buf1) {
            if (buf1hasdata && true == h5tools_detect_vlen(mtype1_id))
                H5Treclaim(mtype1_id, space1_id, H5P_DEFAULT, buf1);
            free(buf1);
        } /* end if */
        if (buf2) {
            if (buf2hasdata && true == h5tools_detect_vlen(mtype2_id))
                H5Treclaim(mtype2_id, space2_id, H5P_DEFAULT, buf2);
            free(buf2);
        } /* end if */

        H5Tclose(ftype1_id);
        H5Tclose(ftype2_id);
        H5Tclose(mtype1_id);
        H5Tclose(mtype2_id);
        H5Sclose(space1_id);
        H5Sclose(space2_id);
    }
    H5E_END_TRY

    H5TOOLS_ENDDEBUG(" - errstat:%d", opts->err_stat);

    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_attr
 *
 * Purpose:  compare attributes located in LOC1_ID and LOC2_ID, which are
 *           obtained either from
 *               loc_id = H5Gopen2(fid, name, H5P_DEFAULT);
 *               loc_id = H5Dopen2(fid, name);
 *               loc_id = H5Topen2(fid, name, H5P_DEFAULT);
 *
 * Return:   number of differences found
 *-------------------------------------------------------------------------
 */

hsize_t
diff_attr(hid_t loc1_id, hid_t loc2_id, const char *path1, const char *path2, diff_opt_t *opts)
{
    table_attrs_t *match_list_attrs = NULL;
    hid_t          attr1_id         = H5I_INVALID_HID; /* attr ID */
    hid_t          attr2_id         = H5I_INVALID_HID; /* attr ID */
    char          *name1            = NULL;
    char          *name2            = NULL;
    unsigned       u; /* Local index variable */
    hsize_t        nfound       = 0;
    hsize_t        nfound_total = 0;
    diff_opt_t     attr_opts;
    diff_err_t     ret_value = opts->err_stat;

    H5TOOLS_START_DEBUG(" - errstat:%d", opts->err_stat);
    attr_opts             = *opts;
    attr_opts.obj_name[0] = NULL;
    attr_opts.obj_name[1] = NULL;

    if (build_match_list_attrs(loc1_id, loc2_id, &match_list_attrs, &attr_opts) < 0) {
        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "build_match_list_attrs failed");
    }
    H5TOOLS_DEBUG("check match_list_attrs - opts->contents:%d - errstat:%d", attr_opts.contents,
                  attr_opts.err_stat);

    /* if detect any unique extra attr */
    if (match_list_attrs->nattrs_only1 || match_list_attrs->nattrs_only2) {
        H5TOOLS_DEBUG("attributes only in one file");
        /* exit will be 1 */
        attr_opts.contents = 0;
    }
    H5TOOLS_DEBUG("match_list_attrs info - opts->contents:%d", attr_opts.contents);

    for (u = 0; u < (unsigned)match_list_attrs->nattrs; u++) {
        H5TOOLS_DEBUG("match_list_attrs loop[%d] - errstat:%d", u, attr_opts.err_stat);
        if ((match_list_attrs->attrs[u].exist[0]) && (match_list_attrs->attrs[u].exist[1])) {
            name1 = name2 = match_list_attrs->attrs[u].name;
            H5TOOLS_DEBUG("name - %s", name1);

            /*--------------
             * attribute 1 */
            if ((attr1_id = H5Aopen(loc1_id, name1, H5P_DEFAULT)) < 0)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Aopen first attribute failed");

            /*--------------
             * attribute 2 */
            if ((attr2_id = H5Aopen(loc2_id, name2, H5P_DEFAULT)) < 0)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Aopen second attribute failed");

            H5TOOLS_DEBUG("got attributes");
            nfound = diff_attr_data(attr1_id, attr2_id, name1, name2, path1, path2, &attr_opts);
            if (H5Aclose(attr1_id) < 0)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Aget_type first attribute failed");
            if (H5Aclose(attr2_id) < 0)
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Aget_type second attribute failed");

            nfound_total += nfound;
        }
    } /* u */

done:
    opts->print_header = attr_opts.print_header;
    opts->contents     = attr_opts.contents;
    opts->not_cmp      = attr_opts.not_cmp;
    opts->err_stat     = attr_opts.err_stat | ret_value;

    H5E_BEGIN_TRY
    {
        table_attrs_free(match_list_attrs);

        H5Aclose(attr1_id);
        H5Aclose(attr2_id);
    }
    H5E_END_TRY

    H5TOOLS_ENDDEBUG(" - errstat:%d", opts->err_stat);
    return nfound_total;
}
