/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "h5repack.h"
#include "h5tools.h"
#include "h5tools_utils.h"

/* number of members in an array */
#ifndef NELMTS
#    define NELMTS(X)    (sizeof(X)/sizeof(X[0]))
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
 *
 * Programmer: Pedro Vicente, pvn@hdfgroup.org
 *
 * Date: December 19, 2003
 *  Modified: December, 19, 2007 (exactly 4 years later :-) )
 *  Separate into 3 cases
 *  1) no filter input, get all datasets and compare DCPLs. TO DO
 *  2) filter input on selected datasets, get each one trough OBJ and match
 *  3) filter input on all datasets, get all objects and match
 *
 *-------------------------------------------------------------------------
 */

int
h5repack_verify(const char *in_fname, const char *out_fname, pack_opt_t *options)
{
    int          ret_value = 0; /*no need to LEAVE() on ERROR: HERR_INIT(int, SUCCEED) */
    hid_t        fidin      = -1;	/* file ID for input file*/
    hid_t        fidout     = -1;   /* file ID for output file*/
    hid_t        did        = -1;   /* dataset ID */
    hid_t        pid        = -1;   /* dataset creation property list ID */
    hid_t        sid        = -1;   /* space ID */
    hid_t        tid        = -1;   /* type ID */
    unsigned int i;
    trav_table_t *travt = NULL;
    int          ok = 1;
    hid_t       fcpl_in     = -1;   /* file creation property for input file */
    hid_t   	fcpl_out    = -1;   /* file creation property for output file */
    H5F_file_space_type_t in_strat, out_strat;	/* file space handling strategy for in/output file */
    hsize_t	in_thresh, out_thresh;		/* free space section threshold for in/output file */

    /* open the output file */
    if((fidout = H5Fopen(out_fname, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0 )
        return -1;

    for(i = 0; i < options->op_tbl->nelems; i++)
    {
        char* name = options->op_tbl->objs[i].path;
        pack_info_t *obj = &options->op_tbl->objs[i];

       /*-------------------------------------------------------------------------
        * open
        *-------------------------------------------------------------------------
        */
        if((did = H5Dopen2(fidout, name, H5P_DEFAULT)) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dopen2 failed");
        if((sid = H5Dget_space(did)) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dget_space failed");
        if((pid = H5Dget_create_plist(did)) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dget_create_plist failed");
        if((tid = H5Dget_type(did)) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dget_type failed");

       /*-------------------------------------------------------------------------
        * filter check
        *-------------------------------------------------------------------------
        */
        if(verify_filters(pid, tid, obj->nfilters, obj->filter) <= 0)
            ok = 0;


       /*-------------------------------------------------------------------------
        * layout check
        *-------------------------------------------------------------------------
        */
        if((obj->layout != -1) && (verify_layout(pid, obj) == 0))
            ok = 0;

       /*-------------------------------------------------------------------------
        * close
        *-------------------------------------------------------------------------
        */
        if(H5Pclose(pid) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pclose failed");
        if (H5Sclose(sid) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Sclose failed");
        if (H5Dclose(did) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dclose failed");
        if (H5Tclose(tid) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Tclose failed");

    }


   /*-------------------------------------------------------------------------
    * check for the "all" objects option
    *-------------------------------------------------------------------------
    */

    if(options->all_filter == 1 || options->all_layout == 1)
    {

        /* init table */
        trav_table_init(&travt);

        /* get the list of objects in the file */
        if(h5trav_gettable(fidout, travt) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "h5trav_gettable failed");

        for(i = 0; i < travt->nobjs; i++)
        {
            char *name = travt->objs[i].name;

            if(travt->objs[i].type == H5TRAV_TYPE_DATASET)
            {

               /*-------------------------------------------------------------------------
                * open
                *-------------------------------------------------------------------------
                */
                if((did = H5Dopen2(fidout, name, H5P_DEFAULT)) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dopen2 failed");
                if((sid = H5Dget_space(did)) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dget_space failed");
                if((pid = H5Dget_create_plist(did)) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dget_create_plist failed");
                if((tid = H5Dget_type(did)) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dget_type failed");

               /*-------------------------------------------------------------------------
                * filter check
                *-------------------------------------------------------------------------
                */
                if(options->all_filter == 1)
                {
                    if(verify_filters(pid, tid, options->n_filter_g, options->filter_g) <= 0)
                        ok = 0;
                }

               /*-------------------------------------------------------------------------
                * layout check
                *-------------------------------------------------------------------------
                */
                if(options->all_layout == 1)
                {
                    pack_info_t pack;

                    init_packobject(&pack);
                    pack.layout = options->layout_g;
                    pack.chunk = options->chunk_g;
                    if(verify_layout(pid, &pack) == 0)
                        ok = 0;
                }


               /*-------------------------------------------------------------------------
                * close
                *-------------------------------------------------------------------------
                */
                if (H5Pclose(pid) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pclose failed");
                if (H5Sclose(sid) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Sclose failed");
                if (H5Dclose(did) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dclose failed");
                if (H5Tclose(tid) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Tclose failed");
            } /* if */

        } /* i */

        /* free table */
        trav_table_free(travt);
    }

   /*-------------------------------------------------------------------------
    * Verify that file space strategy and free space threshold
    * are set as expected
    *-------------------------------------------------------------------------
    */

    /* open the input file */
    if((fidin = H5Fopen(in_fname, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0 )
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Fopen failed");

    /* Get file creation property list for input file */
    if((fcpl_in = H5Fget_create_plist(fidin)) < 0) {
        error_msg("failed to retrieve file creation property list\n");
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Fget_create_plist failed");
    }

    /* Get file space management info for input file */
    if(H5Pget_file_space(fcpl_in, &in_strat, &in_thresh) < 0) {
        error_msg("failed to retrieve file space strategy & threshold\n");
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pget_file_space failed");
    }

    /* Output file is already opened */
    /* Get file creation property list for output file */
    if((fcpl_out = H5Fget_create_plist(fidout)) < 0) {
        error_msg("failed to retrieve file creation property list\n");
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Fget_create_plist failed");
    }

    /* Get file space management info for output file */
    if(H5Pget_file_space(fcpl_out, &out_strat, &out_thresh) < 0) {
        error_msg("failed to retrieve file space strategy & threshold\n");
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pget_file_space failed");
    }

    /*
     * If the strategy option is not set,
     * file space handling strategy should be the same for both
     * input & output files.
     * If the strategy option is set,
     * the output file's file space handling strategy should be the same
     * as what is set via the strategy option
     */
    if(!options->fs_strategy && out_strat != in_strat) {
        error_msg("file space strategy not set as unexpected\n");
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "file space strategy not set as unexpected");

    }
    else if(options->fs_strategy && out_strat!= options->fs_strategy)  {
        error_msg("file space strategy not set as unexpected\n");
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "file space strategy not set as unexpected");
    }

    /*
     * If the threshold option is not set,
     * the free space section threshold should be the same for both
     * input & output files.
     * If the threshold option is set,
     * the output file's free space section threshold should be the same
     * as what is set via the threshold option.
     */
    if(!options->fs_threshold && out_thresh != in_thresh) {
        error_msg("free space threshold not set as unexpected\n");
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "free space threshold not set as unexpected");
    }
    else if(options->fs_threshold && out_thresh != options->fs_threshold) {
        error_msg("free space threshold not set as unexpected\n");
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "free space threshold not set as unexpected");
    }

    /* Closing */
    if (H5Pclose(fcpl_in) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pclose failed");
    if (H5Pclose(fcpl_out) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pclose failed");
    if (H5Fclose(fidin) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Fclose failed");
    if (H5Fclose(fidout) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Fclose failed");

    return ok;

done:
    H5E_BEGIN_TRY {
        H5Pclose(fcpl_in);
        H5Pclose(fcpl_out);
        H5Pclose(pid);
        H5Sclose(sid);
        H5Dclose(did);
        H5Fclose(fidin);
        H5Fclose(fidout);
        if (travt)
            trav_table_free(travt);
    } H5E_END_TRY;

    return ret_value;
} /* h5repack_verify() */



/*-------------------------------------------------------------------------
 * Function: verify_layout
 *
 * Purpose: verify which layout is present in the property list DCPL_ID
 *
 *  H5D_COMPACT      = 0
 *  H5D_CONTIGUOUS  = 1
 *  H5D_CHUNKED    = 2
 *
 * Return: 1 has, 0 does not, -1 error
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December 30, 2003
 *
 *-------------------------------------------------------------------------
 */

int verify_layout(hid_t pid,
                  pack_info_t *obj)
{
    hsize_t      chsize[64];     /* chunk size in elements */
    H5D_layout_t layout;         /* layout */
    int          nfilters;       /* number of filters */
    int          rank;           /* rank */
    int          i;              /* index */

    /* check if we have filters in the input object */
    if ((nfilters = H5Pget_nfilters(pid)) < 0)
        return -1;

    /* a non chunked layout was requested on a filtered object */
    if (nfilters && obj->layout!=H5D_CHUNKED)
        return 0;

    /* get layout */
    if ((layout = H5Pget_layout(pid)) < 0)
        return -1;

    if (obj->layout != layout)
        return 0;

    if (layout==H5D_CHUNKED)
    {
        if ((rank = H5Pget_chunk(pid,NELMTS(chsize),chsize/*out*/)) < 0)
            return -1;
        if (obj->chunk.rank != rank)
            return 0;
        for ( i=0; i<rank; i++)
            if (chsize[i] != obj->chunk.chunk_lengths[i])
                return 0;
    }

    return 1;
}

/*-------------------------------------------------------------------------
 * Function: h5repack_cmp_pl
 *
 * Purpose: compare 2 files for identical property lists of all objects
 *
 * Return: 1=identical, 0=not identical, -1=error
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December 31, 2003
 *
 *-------------------------------------------------------------------------
 */

int h5repack_cmp_pl(const char *fname1,
                     const char *fname2)
{
    int           ret_value = 0; /*no need to LEAVE() on ERROR: HERR_INIT(int, SUCCEED) */
    hid_t         fid1=-1;         /* file ID */
    hid_t         fid2=-1;         /* file ID */
    hid_t         dset1=-1;        /* dataset ID */
    hid_t         dset2=-1;        /* dataset ID */
    hid_t         gid=-1;          /* group ID */
    hid_t         dcpl1=-1;        /* dataset creation property list ID */
    hid_t         dcpl2=-1;        /* dataset creation property list ID */
    hid_t         gcplid=-1;       /* group creation property list */
    unsigned      crt_order_flag1; /* group creation order flag */
    unsigned      crt_order_flag2; /* group creation order flag */
    trav_table_t  *trav=NULL;
    int           ret=1;
    unsigned int  i;

   /*-------------------------------------------------------------------------
    * open the files
    *-------------------------------------------------------------------------
    */

    /* disable error reporting */
    H5E_BEGIN_TRY
    {

        /* Open the files */
        if ((fid1 = H5Fopen(fname1,H5F_ACC_RDONLY,H5P_DEFAULT)) < 0 )
        {
            error_msg("<%s>: %s\n", fname1, H5FOPENERROR );
            return -1;
        }
        if ((fid2 = H5Fopen(fname2,H5F_ACC_RDONLY,H5P_DEFAULT)) < 0 )
        {
            error_msg("<%s>: %s\n", fname2, H5FOPENERROR );
            H5Fclose(fid1);
            return -1;
        }
        /* enable error reporting */
    } H5E_END_TRY;

   /*-------------------------------------------------------------------------
    * get file table list of objects
    *-------------------------------------------------------------------------
    */
    trav_table_init(&trav);
    if(h5trav_gettable(fid1, trav) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "h5trav_gettable failed");

   /*-------------------------------------------------------------------------
    * traverse the suppplied object list
    *-------------------------------------------------------------------------
    */
    for(i = 0; i < trav->nobjs; i++)
    {

        if(trav->objs[i].type == H5TRAV_TYPE_GROUP)
        {

            if ((gid = H5Gopen2(fid1, trav->objs[i].name, H5P_DEFAULT)) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Gopen2 failed");
            if ((gcplid = H5Gget_create_plist(gid)) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Gget_create_plist failed");
            if (H5Pget_link_creation_order(gcplid, &crt_order_flag1) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pget_link_creation_order failed");
            if (H5Pclose(gcplid) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pclose failed");
            if (H5Gclose(gid) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Gclose failed");

            if ((gid = H5Gopen2(fid2, trav->objs[i].name, H5P_DEFAULT)) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Gopen2 failed");
            if ((gcplid = H5Gget_create_plist(gid)) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Gget_create_plist failed");
            if (H5Pget_link_creation_order(gcplid, &crt_order_flag2) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pget_link_creation_order failed");
            if (H5Pclose(gcplid) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pclose failed");
            if (H5Gclose(gid) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Gclose failed");

            if (crt_order_flag1 != crt_order_flag2) {
                error_msg("property lists for <%s> are different\n",trav->objs[i].name);
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "property lists failed");
            }

        }



        else if(trav->objs[i].type == H5TRAV_TYPE_DATASET)
        {
            if((dset1 = H5Dopen2(fid1, trav->objs[i].name, H5P_DEFAULT)) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dopen2 failed");
            if((dset2 = H5Dopen2(fid2, trav->objs[i].name, H5P_DEFAULT)) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dopen2 failed");
            if((dcpl1 = H5Dget_create_plist(dset1)) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dget_create_plist failed");
            if((dcpl2 = H5Dget_create_plist(dset2)) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dget_create_plist failed");

           /*-------------------------------------------------------------------------
            * compare the property lists
            *-------------------------------------------------------------------------
            */
            if((ret = H5Pequal(dcpl1, dcpl2)) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pequal failed");

            if(ret == 0) {
                error_msg("property lists for <%s> are different\n",trav->objs[i].name);
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "property lists failed");
            }

           /*-------------------------------------------------------------------------
            * close
            *-------------------------------------------------------------------------
            */
            if(H5Pclose(dcpl1) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pclose failed");
            if(H5Pclose(dcpl2) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pclose failed");
            if(H5Dclose(dset1) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dclose failed");
            if(H5Dclose(dset2) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Dclose failed");
        } /*if*/
    } /*i*/

   /*-------------------------------------------------------------------------
    * free
    *-------------------------------------------------------------------------
    */

    trav_table_free(trav);

   /*-------------------------------------------------------------------------
    * close
    *-------------------------------------------------------------------------
    */

    H5Fclose(fid1);
    H5Fclose(fid2);

    return ret;

/*-------------------------------------------------------------------------
* error
*-------------------------------------------------------------------------
*/
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
        trav_table_free(trav);
    } H5E_END_TRY;

    return ret_value;
}


/*-------------------------------------------------------------------------
 * Function: verify_filters
 *
 * Purpose: verify if all requested filters in the array FILTER obtained
 *  from user input are present in the property list PID obtained from
 *  the output file
 *
 * Return:
 *  1 match
 *  0 do not match
 * -1 error
 *
 * Programmer: Pedro Vicente, pvn@hdfgroup.org
 *
 * Date: December 21, 2007
 *
 *-------------------------------------------------------------------------
 */

static
int verify_filters(hid_t pid, hid_t tid, int nfilters, filter_info_t *filter)
{
    int           nfilters_dcpl;  /* number of filters in DCPL*/
    unsigned      filt_flags;     /* filter flags */
    H5Z_filter_t  filtn;          /* filter identification number */
    unsigned      cd_values[20];  /* filter client data values */
    size_t        cd_nelmts;      /* filter client number of values */
    char          f_name[256];    /* filter name */
    size_t        size;           /* type size */
    int           i;              /* index */
    unsigned      j;              /* index */

    /* get information about filters */
    if((nfilters_dcpl = H5Pget_nfilters(pid)) < 0)
        return -1;

    /* if we do not have filters and the requested filter is NONE, return 1 */
    if(!nfilters_dcpl &&
        nfilters == 1 &&
        filter[0].filtn == H5Z_FILTER_NONE )
        return 1;

    /* else the numbers of filters must match */
    if (nfilters_dcpl != nfilters )
        return 0;

    /*-------------------------------------------------------------------------
     * build a list with DCPL filters
     *-------------------------------------------------------------------------
     */

    for( i = 0; i < nfilters_dcpl; i++)
    {
        cd_nelmts = NELMTS(cd_values);
        filtn = H5Pget_filter2(pid, (unsigned)i, &filt_flags, &cd_nelmts,
            cd_values, sizeof(f_name), f_name, NULL);

        /* filter ID */
        if (filtn != filter[i].filtn)
            return 0;

        /* compare client data values. some filters do return local values */
        switch (filtn)
        {

            case H5Z_FILTER_NONE:
        	break;

            case H5Z_FILTER_SHUFFLE:
                /* 1 private client value is returned by DCPL */
                if ( cd_nelmts != H5Z_SHUFFLE_TOTAL_NPARMS && filter[i].cd_nelmts != H5Z_SHUFFLE_USER_NPARMS )
                    return 0;

                /* get dataset's type size */
                if((size = H5Tget_size(tid)) <= 0)
                    return -1;

                /* the private client value holds the dataset's type size */
                if ( size != cd_values[0] )
                    return 0;

                break;

            case H5Z_FILTER_SZIP:
                /* 4 private client values are returned by DCPL */
                if ( cd_nelmts != H5Z_SZIP_TOTAL_NPARMS && filter[i].cd_nelmts != H5Z_SZIP_USER_NPARMS )
                    return 0;

                /* "User" parameter for pixels-per-block (index 1) */
                if ( cd_values[H5Z_SZIP_PARM_PPB] != filter[i].cd_values[H5Z_SZIP_PARM_PPB] )
                    return 0;

                break;

            case H5Z_FILTER_NBIT:
                /* only client data values number of values checked */
                if ( H5Z_NBIT_USER_NPARMS != filter[i].cd_nelmts)
                    return 0;
                break;

            case H5Z_FILTER_SCALEOFFSET:
                /* only client data values checked */
                for( j = 0; j < H5Z_SCALEOFFSET_USER_NPARMS; j++)
                    if (cd_values[j] != filter[i].cd_values[j])
                        return 0;
                break;

            /* for these filters values must match, no local values set in DCPL */
            case H5Z_FILTER_FLETCHER32:
            case H5Z_FILTER_DEFLATE:

                if ( cd_nelmts != filter[i].cd_nelmts)
                    return 0;

                for( j = 0; j < cd_nelmts; j++)
                    if (cd_values[j] != filter[i].cd_values[j])
                        return 0;

                break;

            default:
                if ( cd_nelmts != filter[i].cd_nelmts)
                    return 0;

                for( j = 0; j < cd_nelmts; j++)
                    if (cd_values[j] != filter[i].cd_values[j])
                        return 0;
                break;

        } /* switch */
    }

    return 1;
}

