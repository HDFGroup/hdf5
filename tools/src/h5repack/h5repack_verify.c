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

#include "h5repack.h"
#include "h5tools.h"
#include "h5tools_utils.h"

/* number of members in an array */
#ifndef NELMTS
#define NELMTS(X) (sizeof(X) / sizeof(X[0]))
#endif

static int verify_layout(hid_t pid, pack_info_t *obj);
static int verify_filters(hid_t pid, hid_t tid, int nfilters, filter_info_t *filter);

/*-------------------------------------------------------------------------
 * Function: h5repack_verify
 *
 * Purpose: verify if filters and layout in the input file match the output file
 *
 * Return:
 *  1 match
 *  0 do not match
 * -1 error
 *-------------------------------------------------------------------------
 */

int
h5repack_verify(const char *in_fname, const char *out_fname, pack_opt_t *options)
{
    hid_t                 fidin  = H5I_INVALID_HID; /* file ID for input file*/
    hid_t                 fidout = H5I_INVALID_HID; /* file ID for output file*/
    hid_t                 did    = H5I_INVALID_HID; /* dataset ID */
    hid_t                 pid    = H5I_INVALID_HID; /* dataset creation property list ID */
    hid_t                 sid    = H5I_INVALID_HID; /* space ID */
    hid_t                 tid    = H5I_INVALID_HID; /* type ID */
    int                   ok     = 1;               /* step results */
    unsigned int          i;
    trav_table_t         *travt    = NULL;
    hid_t                 fcpl_in  = H5I_INVALID_HID;  /* file creation property for input file */
    hid_t                 fcpl_out = H5I_INVALID_HID;  /* file creation property for output file */
    H5F_fspace_strategy_t in_strategy, out_strategy;   /* file space handling strategy for in/output file */
    bool                  in_persist, out_persist;     /* free-space persist status for in/output file */
    hsize_t               in_threshold, out_threshold; /* free-space section threshold for in/output file */
    hsize_t               in_pagesize, out_pagesize;   /* file space page size for input/output file */
    int                   ret_value = 0;

    /* open the output file */
    if ((fidout = H5Fopen(out_fname, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        H5TOOLS_GOTO_ERROR((-1), "H5Fopen failed on <%s>", out_fname);

    for (i = 0; i < options->op_tbl->nelems; i++) {
        char        *name = options->op_tbl->objs[i].path;
        pack_info_t *obj  = &options->op_tbl->objs[i];

        /*-------------------------------------------------------------------------
         * open
         *-------------------------------------------------------------------------
         */
        if ((did = H5Dopen2(fidout, name, H5P_DEFAULT)) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Dopen2 failed on <%s>", name);
        if ((sid = H5Dget_space(did)) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Dget_space failed");
        if ((pid = H5Dget_create_plist(did)) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Dget_create_plist failed");
        if ((tid = H5Dget_type(did)) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Dget_type failed");

        /*-------------------------------------------------------------------------
         * filter check
         *-------------------------------------------------------------------------
         */
        if (verify_filters(pid, tid, obj->nfilters, obj->filter) <= 0)
            ok = 0;

        /*-------------------------------------------------------------------------
         * layout check
         *-------------------------------------------------------------------------
         */
        if ((obj->layout != -1) && (verify_layout(pid, obj) == 0))
            ok = 0;

        /*-------------------------------------------------------------------------
         * close
         *-------------------------------------------------------------------------
         */
        if (H5Pclose(pid) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Pclose failed");
        if (H5Sclose(sid) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Sclose failed");
        if (H5Dclose(did) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Dclose failed");
        if (H5Tclose(tid) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Tclose failed");
    }

    /*-------------------------------------------------------------------------
     * check for the "all" objects option
     *-------------------------------------------------------------------------
     */

    if (options->all_filter == 1 || options->all_layout == 1) {
        /* Initialize indexing options */
        h5trav_set_index(sort_by, sort_order);
        /* init table */
        trav_table_init(fidout, &travt);

        /* get the list of objects in the file */
        if (h5trav_gettable(fidout, travt) < 0)
            H5TOOLS_GOTO_ERROR((-1), "h5trav_gettable failed");

        for (i = 0; i < travt->nobjs; i++) {
            char *name = travt->objs[i].name;

            if (travt->objs[i].type == H5TRAV_TYPE_DATASET) {
                /*-------------------------------------------------------------------------
                 * open
                 *-------------------------------------------------------------------------
                 */
                if ((did = H5Dopen2(fidout, name, H5P_DEFAULT)) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Dopen2 failed on <%s>", name);
                if ((sid = H5Dget_space(did)) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Dget_space failed");
                if ((pid = H5Dget_create_plist(did)) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Dget_create_plist failed");
                if ((tid = H5Dget_type(did)) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Dget_type failed");

                /*-------------------------------------------------------------------------
                 * filter check
                 *-------------------------------------------------------------------------
                 */
                if (options->all_filter == 1) {
                    if (verify_filters(pid, tid, options->n_filter_g, options->filter_g) <= 0)
                        ok = 0;
                }

                /*-------------------------------------------------------------------------
                 * layout check
                 *-------------------------------------------------------------------------
                 */
                if (options->all_layout == 1) {
                    pack_info_t pack;

                    init_packobject(&pack);
                    pack.layout = options->layout_g;
                    pack.chunk  = options->chunk_g;
                    if (verify_layout(pid, &pack) == 0)
                        ok = 0;
                }

                /*-------------------------------------------------------------------------
                 * close
                 *-------------------------------------------------------------------------
                 */
                if (H5Pclose(pid) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Pclose failed");
                if (H5Sclose(sid) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Sclose failed");
                if (H5Dclose(did) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Dclose failed");
                if (H5Tclose(tid) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Tclose failed");
            } /* if */
        }     /* i */

        /* free table */
        trav_table_free(travt);
        travt = NULL;
    }

    /*-------------------------------------------------------------------------
     * Verify that file space info are set as expected
     *-------------------------------------------------------------------------
     */

    /* open the input file */
    if ((fidin = H5Fopen(in_fname, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        H5TOOLS_GOTO_ERROR((-1), "H5Fopen failed on file <%s>", in_fname);

    /* Get file creation property list for input file */
    if ((fcpl_in = H5Fget_create_plist(fidin)) < 0)
        H5TOOLS_GOTO_ERROR((-1), "H5Fget_create_plist failed to retrieve file creation property list");

    /* Get file space info for input file */
    if (H5Pget_file_space_strategy(fcpl_in, &in_strategy, &in_persist, &in_threshold) < 0)
        H5TOOLS_GOTO_ERROR((-1),
                           "H5Pget_file_space_strategy failed to retrieve file space strategy & threshold");

    /* Get file space page size for input file */
    if (H5Pget_file_space_page_size(fcpl_in, &in_pagesize) < 0)
        H5TOOLS_GOTO_ERROR((-1), "H5Pget_file_space_page_size failed to retrieve file space page size");

    /* Output file is already opened */
    /* Get file creation property list for output file */
    if ((fcpl_out = H5Fget_create_plist(fidout)) < 0)
        H5TOOLS_GOTO_ERROR((-1), "H5Fget_create_plist failed to retrieve file creation property list");

    /* Get file space info for output file */
    if (H5Pget_file_space_strategy(fcpl_out, &out_strategy, &out_persist, &out_threshold) < 0)
        H5TOOLS_GOTO_ERROR((-1),
                           "H5Pget_file_space_strategy failed to retrieve file space strategy & threshold");

    /* Get file space page size for output file */
    if (H5Pget_file_space_page_size(fcpl_out, &out_pagesize) < 0)
        H5TOOLS_GOTO_ERROR((-1), "H5Pget_file_space_page_size failed to retrieve file space page size");

    /*
     * If -S option is set, the file space handling strategy should be set as specified.
     * If -S option is not set, the file space handling strategy should be
     * the same as the input file's strategy.
     */
    if (options->fs_strategy) {
        if (out_strategy != (options->fs_strategy == (H5F_fspace_strategy_t)-1 ? 0 : options->fs_strategy))
            H5TOOLS_GOTO_ERROR((-1), "file space strategy not set as unexpected");
    }
    else {
        if (out_strategy != in_strategy)
            H5TOOLS_GOTO_ERROR((-1), "file space strategy not set as unexpected");
    }

    /*
     * If the -P option is set, the free-space persist status should be set as specified.
     * If the -P option is not set, the free-space persist status should be
     * the same as the input file's free-space persist status
     */
    if (options->fs_persist) {
        if (out_persist != (bool)(options->fs_persist == (-1) ? false : options->fs_persist))
            H5TOOLS_GOTO_ERROR((-1), "free-space persist status not set as unexpected");
    }
    else {
        if (out_persist != in_persist)
            H5TOOLS_GOTO_ERROR((-1), "free-space persist status not set as unexpected");
    }

    /*
     * If the -T option is set, the threshold size should be set as specified.
     * If the -T option is not set, the threshold should be the same as the
     * input file's threshold size.
     */
    if (options->fs_threshold) {
        if (out_threshold != (hsize_t)(options->fs_threshold == (-1) ? 0 : options->fs_threshold))
            H5TOOLS_GOTO_ERROR((-1), "threshold not set as unexpected");
    }
    else {
        if (out_threshold != in_threshold)
            H5TOOLS_GOTO_ERROR((-1), "threshold not set as unexpected");
    }

    /*
     * If the -G option is set, the file space page size should be set as specified.
     * If the -G option is not set, the file space page size should be
     * the same as the input file's file space page size.
     */
    if (options->fs_pagesize) {
        if (out_pagesize != (hsize_t)(options->fs_pagesize == (-1) ? 0 : options->fs_pagesize))
            H5TOOLS_GOTO_ERROR((-1), "file space page size not set as unexpected");
    }
    else { /* "-G" is not set */
        if (out_pagesize != in_pagesize)
            H5TOOLS_GOTO_ERROR((-1), "file space page size not set as unexpected");
    }

    ret_value = ok;

done:
    H5E_BEGIN_TRY
    {
        H5Pclose(fcpl_in);
        H5Pclose(fcpl_out);
        H5Pclose(pid);
        H5Sclose(sid);
        H5Dclose(did);
        H5Tclose(tid);
        H5Fclose(fidin);
        H5Fclose(fidout);
        if (travt)
            trav_table_free(travt);
    }
    H5E_END_TRY

    return ret_value;
} /* h5repack_verify() */

/*-------------------------------------------------------------------------
 * Function: verify_layout
 *
 * Purpose:  verify which layout is present in the property list DCPL_ID
 *           H5D_COMPACT     = 0
 *           H5D_CONTIGUOUS  = 1
 *           H5D_CHUNKED     = 2
 *
 * Return:
 *           1 has,
 *           0 does not,
 *          -1 error
 *-------------------------------------------------------------------------
 */

int
verify_layout(hid_t pid, pack_info_t *obj)
{
    hsize_t      chsize[64]; /* chunk size in elements */
    H5D_layout_t layout;     /* layout */
    int          nfilters;   /* number of filters */
    int          rank;       /* rank */
    int          i;          /* index */

    /* check if we have filters in the input object */
    if ((nfilters = H5Pget_nfilters(pid)) < 0)
        return -1;

    /* a non chunked layout was requested on a filtered object */
    if (nfilters && obj->layout != H5D_CHUNKED)
        return 0;

    /* get layout */
    if ((layout = H5Pget_layout(pid)) < 0)
        return -1;

    if (obj->layout != layout)
        return 0;

    if (layout == H5D_CHUNKED) {
        if ((rank = H5Pget_chunk(pid, NELMTS(chsize), chsize /*out*/)) < 0)
            return -1;
        if (obj->chunk.rank != rank)
            return 0;
        for (i = 0; i < rank; i++)
            if (chsize[i] != obj->chunk.chunk_lengths[i])
                return 0;
    }

    return 1;
}

/*-------------------------------------------------------------------------
 * Function: h5repack_cmp_pl
 *
 * Purpose:  compare 2 files for identical property lists of all objects
 *
 * Return:
 *           1 = identical,
 *           0 = not identical,
 *          -1 = error
 *-------------------------------------------------------------------------
 */

int
h5repack_cmp_pl(const char *fname1, hid_t fname1_fapl, const char *fname2, hid_t fname2_fapl)
{
    hid_t         fid1   = H5I_INVALID_HID; /* file ID */
    hid_t         fid2   = H5I_INVALID_HID; /* file ID */
    hid_t         dset1  = H5I_INVALID_HID; /* dataset ID */
    hid_t         dset2  = H5I_INVALID_HID; /* dataset ID */
    hid_t         gid    = H5I_INVALID_HID; /* group ID */
    hid_t         dcpl1  = H5I_INVALID_HID; /* dataset creation property list ID */
    hid_t         dcpl2  = H5I_INVALID_HID; /* dataset creation property list ID */
    hid_t         gcplid = H5I_INVALID_HID; /* group creation property list */
    unsigned      crt_order_flag1;          /* group creation order flag */
    unsigned      crt_order_flag2;          /* group creation order flag */
    trav_table_t *trav = NULL;
    unsigned int  i;
    int           ret_value = 1;

    /*-------------------------------------------------------------------------
     * open the files
     *-------------------------------------------------------------------------
     */
    /* Open the files */
    if ((fid1 = h5tools_fopen(fname1, H5F_ACC_RDONLY, fname1_fapl, (fname1_fapl != H5P_DEFAULT), NULL, 0)) <
        0)
        H5TOOLS_GOTO_ERROR((-1), "h5tools_fopen failed <%s>: %s", fname1, H5FOPENERROR);
    if ((fid2 = h5tools_fopen(fname2, H5F_ACC_RDONLY, fname2_fapl, (fname2_fapl != H5P_DEFAULT), NULL, 0)) <
        0)
        H5TOOLS_GOTO_ERROR((-1), "h5tools_fopen failed <%s>: %s", fname2, H5FOPENERROR);

    /*-------------------------------------------------------------------------
     * get file table list of objects
     *-------------------------------------------------------------------------
     */
    /* Initialize indexing options */
    h5trav_set_index(sort_by, sort_order);
    /* init table */
    trav_table_init(fid1, &trav);
    if (h5trav_gettable(fid1, trav) < 0)
        H5TOOLS_GOTO_ERROR((-1), "h5trav_gettable failed");

    /*-------------------------------------------------------------------------
     * traverse the supplied object list
     *-------------------------------------------------------------------------
     */
    for (i = 0; i < trav->nobjs; i++) {
        if (trav->objs[i].type == H5TRAV_TYPE_GROUP) {
            if ((gid = H5Gopen2(fid1, trav->objs[i].name, H5P_DEFAULT)) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Gopen2 failed on first <%s>", trav->objs[i].name);
            if ((gcplid = H5Gget_create_plist(gid)) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Gget_create_plist failed");
            if (H5Pget_link_creation_order(gcplid, &crt_order_flag1) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Pget_link_creation_order failed");
            if (H5Pclose(gcplid) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Pclose failed");
            if (H5Gclose(gid) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Gclose failed");

            if ((gid = H5Gopen2(fid2, trav->objs[i].name, H5P_DEFAULT)) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Gopen2 failed on second <%s>", trav->objs[i].name);
            if ((gcplid = H5Gget_create_plist(gid)) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Gget_create_plist failed");
            if (H5Pget_link_creation_order(gcplid, &crt_order_flag2) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Pget_link_creation_order failed");
            if (H5Pclose(gcplid) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Pclose failed");
            if (H5Gclose(gid) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Gclose failed");

            if (crt_order_flag1 != crt_order_flag2)
                H5TOOLS_GOTO_ERROR(0, "property lists failed for <%s> are different", trav->objs[i].name);
        }
        else if (trav->objs[i].type == H5TRAV_TYPE_DATASET) {
            if ((dset1 = H5Dopen2(fid1, trav->objs[i].name, H5P_DEFAULT)) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Dopen2 failed on first <%s>", trav->objs[i].name);
            if ((dset2 = H5Dopen2(fid2, trav->objs[i].name, H5P_DEFAULT)) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Dopen2 failed on second <%s>", trav->objs[i].name);
            if ((dcpl1 = H5Dget_create_plist(dset1)) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Dget_create_plist failed");
            if ((dcpl2 = H5Dget_create_plist(dset2)) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Dget_create_plist failed");

            /*-------------------------------------------------------------------------
             * compare the property lists
             *-------------------------------------------------------------------------
             */
            if ((ret_value = H5Pequal(dcpl1, dcpl2)) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Pequal failed");

            if (ret_value == 0)
                H5TOOLS_GOTO_ERROR(0, "property lists failed for <%s> are different", trav->objs[i].name);

            /*-------------------------------------------------------------------------
             * close
             *-------------------------------------------------------------------------
             */
            if (H5Pclose(dcpl1) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Pclose failed");
            if (H5Pclose(dcpl2) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Pclose failed");
            if (H5Dclose(dset1) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Dclose failed");
            if (H5Dclose(dset2) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Dclose failed");
        } /*if*/
    }     /*for*/

done:
    H5E_BEGIN_TRY
    {
        H5Pclose(dcpl1);
        H5Pclose(dcpl2);
        H5Dclose(dset1);
        H5Dclose(dset2);
        H5Fclose(fid1);
        H5Fclose(fid2);
        H5Pclose(gcplid);
        H5Gclose(gid);
        if (trav)
            trav_table_free(trav);
    }
    H5E_END_TRY

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: verify_filters
 *
 * Purpose:  verify if all requested filters in the array FILTER obtained
 *           from user input are present in the property list PID obtained from
 *           the output file
 *
 * Return:
 *           1 match
 *           0 do not match
 *          -1 error
 *-------------------------------------------------------------------------
 */

static int
verify_filters(hid_t pid, hid_t tid, int nfilters, filter_info_t *filter)
{
    int          nfilters_dcpl; /* number of filters in DCPL*/
    unsigned     filt_flags;    /* filter flags */
    H5Z_filter_t filtn;         /* filter identification number */
    unsigned     cd_values[20]; /* filter client data values */
    size_t       cd_nelmts;     /* filter client number of values */
    char         f_name[256];   /* filter name */
    size_t       size;          /* type size */
    int          i;             /* index */
    unsigned     j;             /* index */

    /* get information about filters */
    if ((nfilters_dcpl = H5Pget_nfilters(pid)) < 0)
        return -1;

    /* if we do not have filters and the requested filter is NONE, return 1 */
    if (!nfilters_dcpl && nfilters == 1 && filter[0].filtn == H5Z_FILTER_NONE)
        return 1;

    /* else the numbers of filters must match */
    if (nfilters_dcpl != nfilters)
        return 0;

    /*-------------------------------------------------------------------------
     * build a list with DCPL filters
     *-------------------------------------------------------------------------
     */

    for (i = 0; i < nfilters_dcpl; i++) {
        cd_nelmts = NELMTS(cd_values);
        filtn = H5Pget_filter2(pid, (unsigned)i, &filt_flags, &cd_nelmts, cd_values, sizeof(f_name), f_name,
                               NULL);

        /* filter ID */
        if (filtn < 0)
            return -1;
        if (filtn != filter[i].filtn)
            return 0;

        /* compare client data values. some filters do return local values */
        switch (filtn) {
            case H5Z_FILTER_NONE:
                break;

            case H5Z_FILTER_SHUFFLE:
                /* 1 private client value is returned by DCPL */
                if (cd_nelmts != H5Z_SHUFFLE_TOTAL_NPARMS && filter[i].cd_nelmts != H5Z_SHUFFLE_USER_NPARMS)
                    return 0;

                /* get dataset's type size */
                if ((size = H5Tget_size(tid)) <= 0)
                    return -1;

                /* the private client value holds the dataset's type size */
                if (size != cd_values[0])
                    return 0;

                break;

            case H5Z_FILTER_SZIP:
                /* 4 private client values are returned by DCPL */
                if (cd_nelmts != H5Z_SZIP_TOTAL_NPARMS && filter[i].cd_nelmts != H5Z_SZIP_USER_NPARMS)
                    return 0;

                /* "User" parameter for pixels-per-block (index 1) */
                if (cd_values[H5Z_SZIP_PARM_PPB] != filter[i].cd_values[H5Z_SZIP_PARM_PPB])
                    return 0;

                break;

            case H5Z_FILTER_NBIT:
                /* only client data values number of values checked */
                if (H5Z_NBIT_USER_NPARMS != filter[i].cd_nelmts)
                    return 0;
                break;

            case H5Z_FILTER_SCALEOFFSET:
                /* only client data values checked */
                for (j = 0; j < H5Z_SCALEOFFSET_USER_NPARMS; j++)
                    if (cd_values[j] != filter[i].cd_values[j])
                        return 0;
                break;

            /* for these filters values must match, no local values set in DCPL */
            case H5Z_FILTER_FLETCHER32:
            case H5Z_FILTER_DEFLATE:

                if (cd_nelmts != filter[i].cd_nelmts)
                    return 0;

                for (j = 0; j < cd_nelmts; j++)
                    if (cd_values[j] != filter[i].cd_values[j])
                        return 0;

                break;

            default:
                if (cd_nelmts != filter[i].cd_nelmts)
                    return 0;

                for (j = 0; j < cd_nelmts; j++)
                    if (cd_values[j] != filter[i].cd_values[j])
                        return 0;
                break;

        } /* switch */
    }

    return 1;
}
