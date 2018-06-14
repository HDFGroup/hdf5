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

#define ATTR_NAME_MAX 255

typedef struct table_attr_t {
    char      *name;
    unsigned   exist[2];
} match_attr_t;

typedef struct table_attrs_t {
    size_t      size;
    size_t      nattrs;
    size_t      nattrs_only1;
    size_t      nattrs_only2;
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
 * Programmer: Jonathan Kim
 *
 * Date: March 15, 2011
 *------------------------------------------------------------------------*/
static void table_attrs_init(table_attrs_t **tbl)
{
    table_attrs_t* table_attrs = (table_attrs_t*) HDmalloc(sizeof(table_attrs_t));

    table_attrs->size = 0;
    table_attrs->nattrs = 0;
    table_attrs->nattrs_only1 = 0;
    table_attrs->nattrs_only2 = 0;
    table_attrs->attrs = NULL;

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
 * Programmer: Jonathan Kim
 *
 * Date: March 15, 2011
 *------------------------------------------------------------------------*/
static void table_attrs_free( table_attrs_t *table )
{
    unsigned int i;

    if (table) {
        if (table->attrs) {
            for (i = 0; i < table->nattrs; i++) {
                if (table->attrs[i].name) {
                    HDfree(table->attrs[i].name);
                }
            } /* end for */
            HDfree(table->attrs);
            table->attrs = NULL;
        } /* end if */
        HDfree(table);
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
 * Programmer: Jonathan Kim
 *
 * Date: March 15, 2011
 *------------------------------------------------------------------------*/
static void table_attr_mark_exist(unsigned *exist, char *name, table_attrs_t *table)
{
    if(table->nattrs == table->size) {
        match_attr_t *new_attrs;

        table->size = MAX(1, table->size * 2);
        new_attrs = (match_attr_t *)HDrealloc(table->attrs, table->size * sizeof(match_attr_t));
        if(new_attrs)
            table->attrs = new_attrs;
    } /* end if */

    if(table->nattrs < table->size) {
        size_t curr_val;

        curr_val = table->nattrs;
        table->attrs[curr_val].exist[0] = exist[0];
        table->attrs[curr_val].exist[1] = exist[1];
        if(name)
            table->attrs[curr_val].name = (char *)HDstrdup(name);
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
static herr_t build_match_list_attrs(hid_t loc1_id, hid_t loc2_id, table_attrs_t ** table_out,  diff_opt_t *opts)
{
    int        ret_value = 0;
    H5O_info_t oinfo1, oinfo2;    /* Object info */
    hid_t      attr1_id = -1;     /* attr ID */
    hid_t      attr2_id = -1;     /* attr ID */
    size_t     curr1 = 0;
    size_t     curr2 = 0;
    unsigned   infile[2];
    char       name1[ATTR_NAME_MAX];
    char       name2[ATTR_NAME_MAX];
    int        cmp;
    unsigned   i;
    table_attrs_t *table_lp = NULL;

    h5difftrace("build_match_list_attrs start\n");

    if(H5Oget_info2(loc1_id, &oinfo1, H5O_INFO_NUM_ATTRS) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Oget_info first object failed");
    if(H5Oget_info2(loc2_id, &oinfo2, H5O_INFO_NUM_ATTRS) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Oget_info second object failed");

    table_attrs_init(&table_lp);
    if (table_lp == NULL)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "Table allocation failed");

    /*--------------------------------------------------
     * build the list
     */
    while(curr1 < oinfo1.num_attrs && curr2 < oinfo2.num_attrs) {
        h5diffdebug3("build_match_list_attrs 1: %ld - %ld\n", curr1, oinfo1.num_attrs);
        h5diffdebug3("build_match_list_attrs 2: %ld - %ld\n", curr2, oinfo2.num_attrs);

        /*------------------
        * open attribute1 */
        if((attr1_id = H5Aopen_by_idx(loc1_id, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)curr1, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Aopen_by_idx first attribute failed");
        /* get name */
        if(H5Aget_name(attr1_id, (size_t)ATTR_NAME_MAX, name1) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Aget_name first attribute failed");

        /*------------------
        * open attribute2 */
        if((attr2_id = H5Aopen_by_idx(loc2_id, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)curr2, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Aopen_by_idx second attribute failed");
        /* get name */
        if(H5Aget_name(attr2_id, (size_t)ATTR_NAME_MAX, name2) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Aget_name second attribute failed");

        /* criteria is string compare */
        cmp = HDstrcmp(name1, name2);

        if(cmp == 0) {
            infile[0] = 1;
            infile[1] = 1;
            table_attr_mark_exist(infile, name1, table_lp);
            curr1++;
            curr2++;
        }
        else if(cmp < 0) {
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
        attr1_id = -1;
        H5Aclose(attr2_id);
        attr2_id = -1;
    } /* end while */

    /* list1 did not end */
    infile[0] = 1;
    infile[1] = 0;
    while(curr1 < oinfo1.num_attrs) {
        h5diffdebug3("build_match_list_attrs 1: %ld - %ld\n", curr1, oinfo1.num_attrs);

        /*------------------
        * open attribute1 */
        if((attr1_id = H5Aopen_by_idx(loc1_id, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)curr1, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Aopen_by_idx first attribute failed");
        /* get name */
        if(H5Aget_name(attr1_id, (size_t)ATTR_NAME_MAX, name1) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Aget_name first attribute failed");
        h5diffdebug2("build_match_list_attrs #1 name - %s\n", name1);

        table_attr_mark_exist(infile, name1, table_lp);
        table_lp->nattrs_only1++;
        curr1++;

        /* close for next turn */
        H5Aclose(attr1_id);
        attr1_id = -1;
    }

    /* list2 did not end */
    infile[0] = 0;
    infile[1] = 1;
    while(curr2 < oinfo2.num_attrs) {
        h5diffdebug3("build_match_list_attrs 2: %ld - %ld\n", curr2, oinfo2.num_attrs);
        /*------------------
        * open attribute2 */
        if((attr2_id = H5Aopen_by_idx(loc2_id, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)curr2, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Aopen_by_idx second attribute failed");
        /* get name */
        if(H5Aget_name(attr2_id, (size_t)ATTR_NAME_MAX, name2) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Aget_name second attribute failed");
        h5diffdebug2("build_match_list_attrs #2 name - %s\n", name2);

        table_attr_mark_exist(infile, name2, table_lp);
        table_lp->nattrs_only2++;
        curr2++;

        /* close for next turn */
        H5Aclose(attr2_id);
        attr2_id = -1;
    }

    /*------------------------------------------------------
     * print the list
     */
    if(opts->m_verbose_level == 2) {
        /* if '-v2' is detected */
        parallel_print("   obj1   obj2\n");
        parallel_print(" --------------------------------------\n");
        for(i = 0; i < (unsigned int) table_lp->nattrs; i++) {
            char c1, c2;
            c1 = (table_lp->attrs[i].exist[0]) ? 'x' : ' ';
            c2 = (table_lp->attrs[i].exist[1]) ? 'x' : ' ';
            parallel_print("%5c %6c    %-15s\n", c1, c2, table_lp->attrs[i].name);
        } /* end for */
    }

    if(opts->m_verbose_level >= 1)
        parallel_print("Attributes status:  %d common, %d only in obj1, %d only in obj2\n",
                table_lp->nattrs - table_lp->nattrs_only1 - table_lp->nattrs_only2,
                table_lp->nattrs_only1, table_lp->nattrs_only2);

done:
    *table_out = table_lp;

    /* disable error reporting */
    H5E_BEGIN_TRY {
        H5Aclose(attr1_id);
        H5Aclose(attr2_id);
    } H5E_END_TRY;

    h5diffdebug2("build_match_list_attrs end - errstat:%d\n", opts->err_stat);

    return ret_value;
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

hsize_t diff_attr(hid_t loc1_id,
                  hid_t loc2_id,
                  const char *path1,
                  const char *path2,
                  diff_opt_t *opts)
{
    int        ret_value = opts->err_stat;
    hid_t      attr1_id = -1;     /* attr ID */
    hid_t      attr2_id = -1;     /* attr ID */
    hid_t      space1_id = -1;    /* space ID */
    hid_t      space2_id = -1;    /* space ID */
    hid_t      ftype1_id = -1;    /* file data type ID */
    hid_t      ftype2_id = -1;    /* file data type ID */
    hid_t      mtype1_id = -1;    /* memory data type ID */
    hid_t      mtype2_id = -1;    /* memory data type ID */
    size_t     msize1;            /* memory size of memory type */
    size_t     msize2;            /* memory size of memory type */
    void       *buf1 = NULL;      /* data buffer */
    void       *buf2 = NULL;      /* data buffer */
    hbool_t    buf1hasdata = FALSE;    /* buffer has data */
    hbool_t    buf2hasdata = FALSE;    /* buffer has data */
    hsize_t    nelmts1;           /* number of elements in dataset */
    int        rank1;             /* rank of dataset */
    int        rank2;             /* rank of dataset */
    hsize_t    dims1[H5S_MAX_RANK];    /* dimensions of dataset */
    hsize_t    dims2[H5S_MAX_RANK];    /* dimensions of dataset */
    char       *name1 = NULL;
    char       *name2 = NULL;
    char       np1[512];
    char       np2[512];
    unsigned   u;                 /* Local index variable */
    hsize_t    nfound = 0;
    hsize_t    nfound_total = 0;
    int       j;

    table_attrs_t *match_list_attrs = NULL;
    h5difftrace("diff_attr start\n");

    if(build_match_list_attrs(loc1_id, loc2_id, &match_list_attrs, opts) < 0) {
        HGOTO_ERROR(1, H5E_tools_min_id_g, "build_match_list_attrs failed");
    }
    h5diffdebug2("build_match_list_attrs - errstat:%d\n", opts->err_stat);

    /* if detect any unique extra attr */
    if(match_list_attrs->nattrs_only1 || match_list_attrs->nattrs_only2) {
        h5difftrace("diff_attr attributes only in one file\n");
        /* exit will be 1 */
        opts->contents = 0;
    }
    h5diffdebug2("match_list_attrs info - errstat:%d\n", opts->err_stat);

    for(u = 0; u < (unsigned)match_list_attrs->nattrs; u++) {
        h5diffdebug3("match_list_attrs loop[%d] - errstat:%d\n", u, opts->err_stat);
        if((match_list_attrs->attrs[u].exist[0]) && (match_list_attrs->attrs[u].exist[1])) {
            name1 = name2 = match_list_attrs->attrs[u].name;
            h5diffdebug2("diff_attr name - %s\n", name1);

            /*--------------
            * attribute 1 */
            if((attr1_id = H5Aopen(loc1_id, name1, H5P_DEFAULT)) < 0)
                HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Aopen first attribute failed");

            /*--------------
            * attribute 2 */
            if((attr2_id = H5Aopen(loc2_id, name2, H5P_DEFAULT)) < 0)
                HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Aopen second attribute failed");

            h5difftrace("diff_attr got attributes\n");
            /* get the datatypes  */
            if((ftype1_id = H5Aget_type(attr1_id)) < 0)
                HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Aget_type first attribute failed");
            if((ftype2_id = H5Aget_type(attr2_id)) < 0)
                HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Aget_type second attribute failed");

            if((mtype1_id = H5Tget_native_type(ftype1_id, H5T_DIR_DEFAULT)) < 0)
                HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Tget_native_type first attribute ftype failed");
            if((mtype2_id = H5Tget_native_type(ftype2_id, H5T_DIR_DEFAULT)) < 0)
                HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Tget_native_type second attribute ftype failed");
            if((msize1 = H5Tget_size(mtype1_id)) == 0)
                HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Tget_size first attribute mtype failed");
            if((msize2 = H5Tget_size(mtype2_id)) == 0)
                HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Tget_size second attribute mtype failed");

            /* get the dataspace   */
            if((space1_id = H5Aget_space(attr1_id)) < 0)
                HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Aget_space first attribute failed");
            if((space2_id = H5Aget_space(attr2_id)) < 0)
                HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Aget_space second attribute failed");

            /* get dimensions  */
            if((rank1 = H5Sget_simple_extent_dims(space1_id, dims1, NULL)) < 0)
                HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Sget_simple_extent_dims first attribute failed");
            if((rank2 = H5Sget_simple_extent_dims(space2_id, dims2, NULL)) < 0)
                HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Sget_simple_extent_dims second attribute failed");

            /*----------------------------------------------------------------------
            * check for comparable TYPE and SPACE
            *----------------------------------------------------------------------
            */

            /* pass dims1 and dims2 for maxdims as well since attribute's maxdims
            * are always same */
            if(diff_can_type(ftype1_id, ftype2_id, rank1, rank2, dims1, dims2,
                    dims1, dims2, name1, name2, opts, 0) != 1) {
                if(H5Tclose(ftype1_id) < 0)
                    HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Tclose first attribute ftype failed");
                if(H5Tclose(ftype2_id) < 0)
                    HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Tclose second attribute ftype failed");
                if(H5Sclose(space1_id) < 0)
                    HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Sclose first attribute failed");
                if(H5Sclose(space2_id) < 0)
                    HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Sclose second attribute failed");
                if(H5Aclose(attr1_id) < 0)
                    HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Aclose first attribute failed");
                if(H5Aclose(attr2_id) < 0)
                    HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Aclose second attribute failed");
                if(H5Tclose(mtype1_id) < 0)
                    HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Tclose first attribute mtype failed");
                if(H5Tclose(mtype2_id) < 0)
                    HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Tclose second attribute mtype failed");

                continue;
            }

            /*-----------------------------------------------------------------
            * "upgrade" the smaller memory size
            *------------------------------------------------------------------
            */
            if(FAIL == match_up_memsize(ftype1_id, ftype2_id, &mtype1_id,
                            &mtype2_id, &msize1, &msize2))
                HGOTO_ERROR(1, H5E_tools_min_id_g, "match_up_memsize failed");

            /*---------------------------------------------------------------------
            * read
            *----------------------------------------------------------------------
            */
            nelmts1 = 1;
            for(j = 0; j < rank1; j++)
                nelmts1 *= dims1[j];

            buf1 = (void *)HDcalloc((size_t)(nelmts1), msize1);
            buf2 = (void *)HDcalloc((size_t)(nelmts1), msize2);
            if(buf1 == NULL || buf2 == NULL) {
                parallel_print("cannot read into memory\n");
                HGOTO_ERROR(1, H5E_tools_min_id_g, "buffer allocation failed");
            }
            if(H5Aread(attr1_id, mtype1_id, buf1) < 0) {
                parallel_print("Failed reading attribute1 %s/%s\n", path1, name1);
                HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Aget_type first attribute failed");
            }
            else
                buf1hasdata = TRUE;

            if(H5Aread(attr2_id, mtype2_id, buf2) < 0) {
                parallel_print("Failed reading attribute2 %s/%s\n", path2, name2);
                HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Aget_type second attribute failed");
            }
            else
                buf2hasdata = TRUE;

            /* format output string */
            HDsnprintf(np1, sizeof(np1), "%s of <%s>", name1, path1);
            HDsnprintf(np2, sizeof(np1), "%s of <%s>", name2, path2);

            /*---------------------------------------------------------------------
            * array compare
            *----------------------------------------------------------------------
            */

            /* always print name */
            /* verbose (-v) and report (-r) mode */
            if(opts->m_verbose || opts->m_report) {
                do_print_attrname("attribute", np1, np2);

                nfound = diff_array(buf1, buf2, nelmts1, (hsize_t) 0, rank1,
                        dims1, opts, np1, np2, mtype1_id, attr1_id, attr2_id);
                print_found(nfound);
            }
            /* quiet mode (-q), just count differences */
            else if(opts->m_quiet) {
                nfound = diff_array(buf1, buf2, nelmts1, (hsize_t) 0, rank1,
                        dims1, opts, np1, np2, mtype1_id, attr1_id, attr2_id);
            }
            /* the rest (-c, none, ...) */
            else {
                nfound = diff_array(buf1, buf2, nelmts1, (hsize_t) 0, rank1,
                        dims1, opts, np1, np2, mtype1_id, attr1_id, attr2_id);

                /* print info if compatible and difference found */
                if (nfound) {
                    do_print_attrname("attribute", np1, np2);
                    print_found(nfound);
                } /* end if */
            } /* end else */

            /*----------------------------------------------------------------------
            * close
            *----------------------------------------------------------------------
            */

            /* Free buf1 and buf2, check both VLEN-data VLEN-string to reclaim any
            * VLEN memory first */
            if(TRUE == h5tools_detect_vlen(mtype1_id))
                H5Dvlen_reclaim(mtype1_id, space1_id, H5P_DEFAULT, buf1);
            HDfree(buf1);
            buf1 = NULL;

            if(TRUE == h5tools_detect_vlen(mtype2_id))
                H5Dvlen_reclaim(mtype2_id, space2_id, H5P_DEFAULT, buf2);
            HDfree(buf2);
            buf2 = NULL;

            if(H5Tclose(ftype1_id) < 0)
                HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Aget_type first attribute failed");
            if(H5Tclose(ftype2_id) < 0)
                HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Aget_type second attribute failed");
            if(H5Sclose(space1_id) < 0)
                HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Aget_type first attribute failed");
            if(H5Sclose(space2_id) < 0)
                HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Aget_type second attribute failed");
            if(H5Aclose(attr1_id) < 0)
                HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Aget_type first attribute failed");
            if(H5Aclose(attr2_id) < 0)
                HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Aget_type second attribute failed");
            if(H5Tclose(mtype1_id) < 0)
                HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Tclose first attribute mtype failed");
            if(H5Tclose(mtype2_id) < 0)
                HGOTO_ERROR(1, H5E_tools_min_id_g, "H5Tclose second attribute mtype failed");

            nfound_total += nfound;
        }
    } /* u */

done:
    opts->err_stat = opts->err_stat | ret_value;

    H5E_BEGIN_TRY {
        if(buf1) {
            if(buf1hasdata && TRUE == h5tools_detect_vlen(mtype1_id))
                H5Dvlen_reclaim(mtype1_id, space1_id, H5P_DEFAULT, buf1);
            HDfree(buf1);
        } /* end if */
        if(buf2) {
            if(buf2hasdata && TRUE == h5tools_detect_vlen(mtype2_id))
                H5Dvlen_reclaim(mtype2_id, space2_id, H5P_DEFAULT, buf2);
            HDfree(buf2);
        } /* end if */

        table_attrs_free(match_list_attrs);

        H5Tclose(ftype1_id);
        H5Tclose(ftype2_id);
        H5Tclose(mtype1_id);
        H5Tclose(mtype2_id);
        H5Sclose(space1_id);
        H5Sclose(space2_id);
        H5Aclose(attr1_id);
        H5Aclose(attr2_id);
    } H5E_END_TRY;

    h5diffdebug2("diff_attr end - errstat:%d\n", opts->err_stat);
    return nfound_total;
}

