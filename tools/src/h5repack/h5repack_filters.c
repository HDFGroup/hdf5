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

/* number of members in an array */
#ifndef NELMTS
#    define NELMTS(X)    (sizeof(X)/sizeof(X[0]))
#endif

/* minimum of two values */
#undef MIN
#define MIN(a,b)    (((a)<(b)) ? (a) : (b))

/*-------------------------------------------------------------------------
 * Function: aux_copy_obj
 *
 * Purpose: copy the object filters for object copy
 *
 * Return: 0 success, -1 failure
 *-------------------------------------------------------------------------
 */
static int aux_copy_obj(hid_t dcpl_id, /* dataset creation property list */
        const char* name,              /* object name from traverse list */
        pack_info_t *objout /*OUT*/)   /* info about object to filter */
{
    int          ret_value = 0;  /*no need to LEAVE() on ERROR: HERR_INIT(int, SUCCEED) */
    int          nfilters;       /* number of filters in DCPL */
    char         f_objname[256]; /* filter objname */
    H5D_layout_t layout;
    int          rank;           /* rank of dataset */
    hsize_t      chsize[64];     /* chunk size in elements */
    int i;
    unsigned u;

    /* get information about input filters */
    if ((nfilters = H5Pget_nfilters(dcpl_id)) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pget_nfilters failed");
    /* copy filter_info_t structure */
    for (i = 0; i < nfilters; i++) {
        if ((objout->filter[i].filtn = H5Pget_filter2(dcpl_id, (unsigned) i, &objout->filter[i].filt_flag, &objout->filter[i].cd_nelmts,
                objout->filter[i].cd_values, sizeof(f_objname), f_objname, NULL)) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pget_filter2 failed");
    }

    objout->nfilters = nfilters;
    HDstrcpy(objout->path, name);

    if ((layout = H5Pget_layout(dcpl_id)) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pget_layout failed");
    objout->layout = layout;

    if (layout == H5D_CHUNKED) {
        if ((rank = H5Pget_chunk(dcpl_id, NELMTS(chsize), chsize/*out*/)) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pget_chunk failed");
        objout->chunk.rank = rank;
        for (u = 0; u < (unsigned)rank; u++)
            objout->chunk.chunk_lengths[u] = chsize[u];
    }

done:
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: aux_find_obj
 *
 * Purpose: find the object name NAME (got from the traverse list)
 *  in the repack options list
 *-------------------------------------------------------------------------
 */
static int aux_find_obj(const char* name, /* object name from traverse list */
        pack_opt_t *options,              /* repack options */
        pack_info_t *obj                  /*OUT*/) /* info about object to filter */
{
    char         *pdest = NULL;
    const char   *pname = NULL;
    unsigned int i;

    for (i = 0; i < options->op_tbl->nelems; i++) {
        if (HDstrcmp(options->op_tbl->objs[i].path, name) == 0) {
            *obj = options->op_tbl->objs[i];
            return (int) i;
        }

        pdest = options->op_tbl->objs[i].path;
        if (pdest[0] == '/') pdest++;
        pname = name;
        if (pname[0] == '/') pname++;

        if (HDstrcmp(pdest, pname) == 0) {
            *obj = options->op_tbl->objs[i];
            return (int) i;
        }
    }/*i*/

    return -1;
}

/*-------------------------------------------------------------------------
 * Function: aux_assign_obj
 *
 * Purpose: find the object name NAME (got from the traverse list)
 *  in the repack options list; assign the filter information OBJ
 *
 * Return: 0 not found, 1 found
 *-------------------------------------------------------------------------
 */
static int aux_assign_obj(const char* name, /* object name from traverse list */
        pack_opt_t *options,                /* repack options */
        pack_info_t *obj /*OUT*/)           /* info about object to filter */
{
    int         idx, i;
    pack_info_t tmp;

    init_packobject(&tmp);

    idx = aux_find_obj(name, options, &tmp);

    /* name was on input */
    if (idx >= 0) {
        /* applying to all objects */
        if (options->all_layout) {
            /* assign the global layout info to the OBJ info */
            tmp.layout = options->layout_g;
            switch (options->layout_g) {
            case H5D_CHUNKED:
                tmp.chunk.rank = options->chunk_g.rank;
                for (i = 0; i < tmp.chunk.rank; i++)
                    tmp.chunk.chunk_lengths[i] = options->chunk_g.chunk_lengths[i];
                break;
            case H5D_LAYOUT_ERROR:
            case H5D_COMPACT:
            case H5D_CONTIGUOUS:
            case H5D_VIRTUAL:
            case H5D_NLAYOUTS:
                break;
            default:
                break;
            }/*switch*/
        }
        else {
            tmp.layout = options->op_tbl->objs[idx].layout;
            switch (tmp.layout) {
            case H5D_CHUNKED:
                tmp.chunk.rank = options->op_tbl->objs[idx].chunk.rank;
                for (i = 0; i < tmp.chunk.rank; i++)
                    tmp.chunk.chunk_lengths[i] = options->op_tbl->objs[idx].chunk.chunk_lengths[i];
                break;
            case H5D_LAYOUT_ERROR:
            case H5D_COMPACT:
            case H5D_CONTIGUOUS:
            case H5D_VIRTUAL:
            case H5D_NLAYOUTS:
                break;
            default:
                break;
            }/*switch*/
        }

        /* applying to all objects */
        if (options->all_filter) {
            /* assign the global filter */
            tmp.nfilters = 1;
            tmp.filter[0] = options->filter_g[0];
        } /* if all */
        else {
            tmp.nfilters = options->op_tbl->objs[idx].nfilters;
            for (i = 0; i < tmp.nfilters; i++) {
                tmp.filter[i] = options->op_tbl->objs[idx].filter[i];
            }
        }
    } /* if idx */
    /* no input name */
    else {
        if (options->all_filter) {
            int k;

            /* assign the global filters */
            tmp.nfilters = options->n_filter_g;
            for (k = 0; k < options->n_filter_g; k++)
                tmp.filter[k] = options->filter_g[k];
        }
        if (options->all_layout) {
            /* assign the global layout info to the OBJ info */
            tmp.layout = options->layout_g;
            switch (options->layout_g) {
            case H5D_CHUNKED:
                tmp.chunk.rank = options->chunk_g.rank;
                for (i = 0; i < tmp.chunk.rank; i++)
                    tmp.chunk.chunk_lengths[i] =
                            options->chunk_g.chunk_lengths[i];
                break;
            case H5D_LAYOUT_ERROR:
            case H5D_COMPACT:
            case H5D_CONTIGUOUS:
            case H5D_VIRTUAL:
            case H5D_NLAYOUTS:
                break;
            default:
                break;
            }/*switch*/
        }
    }

    *obj = tmp;
    return 1;
}

/*-------------------------------------------------------------------------
 * Function: apply_filters
 *
 * Purpose: apply the filters in the object to the property list;
 *  do extra checking in the case of SZIP; delete all filters in the case
 *  of H5Z_FILTER_NONE present in the PACK_INFO_T filter array
 *
 * Return: 0 success, -1 an error occured
 *-------------------------------------------------------------------------
 */

int apply_filters(const char* name, /* object name from traverse list */
        int rank,                   /* rank of dataset */
        hsize_t *dims,              /* dimensions of dataset */
        size_t msize,               /* size of type */
        hid_t dcpl_id,              /* dataset creation property list */
        pack_opt_t *options,        /* repack options */
        int *has_filter)            /* (OUT) object NAME has a filter */
{
    int         ret_value = 0; /*no need to LEAVE() on ERROR: HERR_INIT(int, SUCCEED) */
    int         nfilters;      /* number of filters in DCPL */
    hsize_t     chsize[64];    /* chunk size in elements */
    H5D_layout_t layout;
    int         i;
    pack_info_t obj;
    pack_info_t filtobj;

    *has_filter = 0;

    if (rank == 0) /* scalar dataset, do not apply */
        HGOTO_DONE(0);

    /*-------------------------------------------------------------------------
     * initialize the assigment object
     *-------------------------------------------------------------------------
     */
    init_packobject(&obj);
    init_packobject(&filtobj);

    /*-------------------------------------------------------------------------
     * find options
     *-------------------------------------------------------------------------
     */
    if (aux_assign_obj(name, options, &obj) == 0)
        HGOTO_DONE(0);

    /* get information about input filters */
    if ((nfilters = H5Pget_nfilters(dcpl_id)) < 0)
        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pget_nfilters failed");


    /*-------------------------------------------------------------------------
     * check if we have filters in the pipeline
     * we want to replace them with the input filters
     * only remove if we are inserting new ones
     *-------------------------------------------------------------------------
     */
    if (nfilters && obj.nfilters) {
        *has_filter = 1;
        if (H5Premove_filter(dcpl_id, H5Z_FILTER_ALL) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Premove_filter failed");
    }
    else if(nfilters) {
        *has_filter = 1;
        if (aux_copy_obj(dcpl_id, name, &filtobj) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "aux_copy_obj failed");
    }

    /*-------------------------------------------------------------------------
     * check if there is an existent chunk
     * read it only if there is not a requested layout
     *-------------------------------------------------------------------------
     */
    if (obj.layout == -1) {
        if ((layout = H5Pget_layout(dcpl_id)) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pget_layout failed");

        if (layout == H5D_CHUNKED) {
            if ((rank = H5Pget_chunk(dcpl_id, NELMTS(chsize), chsize/*out*/)) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pget_chunk failed");
            obj.layout = H5D_CHUNKED;
            obj.chunk.rank = rank;
            for (i = 0; i < rank; i++)
                obj.chunk.chunk_lengths[i] = chsize[i];
        }
    }

    /*-------------------------------------------------------------------------
     * the type of filter and additional parameter
     * type can be one of the filters
     * H5Z_FILTER_NONE        0 , uncompress if compressed
     * H5Z_FILTER_DEFLATE     1 , deflation like gzip
     * H5Z_FILTER_SHUFFLE     2 , shuffle the data
     * H5Z_FILTER_FLETCHER32  3 , fletcher32 checksum of EDC
     * H5Z_FILTER_SZIP        4 , szip compression
     * H5Z_FILTER_NBIT        5 , nbit compression
     * H5Z_FILTER_SCALEOFFSET 6 , scaleoffset compression
     *-------------------------------------------------------------------------
     */

    if (obj.nfilters) {
        /*-------------------------------------------------------------------------
         * filters require CHUNK layout; if we do not have one define a default
         *-------------------------------------------------------------------------
         */
        if (obj.layout == -1) {
            /* stripmine info */
            hsize_t sm_size[H5S_MAX_RANK]; /*stripmine size */
            hsize_t sm_nbytes; /*bytes per stripmine */

            obj.chunk.rank = rank;

            /*
             * determine the strip mine size. The strip mine is
             * a hyperslab whose size is manageable.
             */

            sm_nbytes = msize;
            for (i = rank; i > 0; --i) {
                hsize_t size = 0;
                if(sm_nbytes == 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "number of bytes per stripmine must be > 0");
                size = H5TOOLS_BUFSIZE / sm_nbytes;
                if (size == 0) /* datum size > H5TOOLS_BUFSIZE */
                    size = 1;
                sm_size[i - 1] = MIN(dims[i - 1], size);
                sm_nbytes *= sm_size[i - 1];
            }

            for (i = 0; i < rank; i++) {
                obj.chunk.chunk_lengths[i] = sm_size[i];
            }
        }

        for (i = 0; i < obj.nfilters; i++) {
            if (obj.filter[i].filtn < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "invalid filter");

            switch (obj.filter[i].filtn) {
            /*-------------------------------------------------------------------------
             * H5Z_FILTER_NONE       0 , uncompress if compressed
             *-------------------------------------------------------------------------
             */
            case H5Z_FILTER_NONE:
                break;

            /*-------------------------------------------------------------------------
             * H5Z_FILTER_DEFLATE       1 , deflation like gzip
             *-------------------------------------------------------------------------
             */
            case H5Z_FILTER_DEFLATE:
                {
                    unsigned aggression; /* the deflate level */

                    aggression = obj.filter[i].cd_values[0];
                    /* set up for deflated data */
                    if (H5Pset_chunk(dcpl_id, obj.chunk.rank, obj.chunk.chunk_lengths) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_chunk failed");
                    if (H5Pset_deflate(dcpl_id, aggression) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_deflate failed");
                }
                break;

            /*-------------------------------------------------------------------------
             * H5Z_FILTER_SZIP       4 , szip compression
             *-------------------------------------------------------------------------
             */
            case H5Z_FILTER_SZIP:
                {
                    unsigned options_mask;
                    unsigned pixels_per_block;

                    options_mask = obj.filter[i].cd_values[0];
                    pixels_per_block = obj.filter[i].cd_values[1];

                    /* set up for szip data */
                    if (H5Pset_chunk(dcpl_id, obj.chunk.rank, obj.chunk.chunk_lengths) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_chunk failed");
                    if (H5Pset_szip(dcpl_id, options_mask, pixels_per_block) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_szip failed");
                }
                break;

            /*-------------------------------------------------------------------------
             * H5Z_FILTER_SHUFFLE    2 , shuffle the data
             *-------------------------------------------------------------------------
             */
            case H5Z_FILTER_SHUFFLE:
                if (H5Pset_chunk(dcpl_id, obj.chunk.rank, obj.chunk.chunk_lengths) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_chunk failed");
                if (H5Pset_shuffle(dcpl_id) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_shuffle failed");
                break;

            /*-------------------------------------------------------------------------
             * H5Z_FILTER_FLETCHER32 3 , fletcher32 checksum of EDC
             *-------------------------------------------------------------------------
             */
            case H5Z_FILTER_FLETCHER32:
                if (H5Pset_chunk(dcpl_id, obj.chunk.rank, obj.chunk.chunk_lengths) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_chunk failed");
                if (H5Pset_fletcher32(dcpl_id) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_fletcher32 failed");
                break;
            /*----------- -------------------------------------------------------------
             * H5Z_FILTER_NBIT , NBIT compression
             *-------------------------------------------------------------------------
             */
            case H5Z_FILTER_NBIT:
                if (H5Pset_chunk(dcpl_id, obj.chunk.rank, obj.chunk.chunk_lengths) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_chunk failed");
                if (H5Pset_nbit(dcpl_id) < 0)
                    HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_nbit failed");
                break;
            /*----------- -------------------------------------------------------------
             * H5Z_FILTER_SCALEOFFSET , scale+offset compression
             *-------------------------------------------------------------------------
             */
            case H5Z_FILTER_SCALEOFFSET:
                {
                    H5Z_SO_scale_type_t scale_type;
                    int scale_factor;

                    scale_type = (H5Z_SO_scale_type_t) obj.filter[i].cd_values[0];
                    scale_factor = (int) obj.filter[i].cd_values[1];

                    if (H5Pset_chunk(dcpl_id, obj.chunk.rank, obj.chunk.chunk_lengths) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_chunk failed");
                    if (H5Pset_scaleoffset(dcpl_id, scale_type, scale_factor) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_scaleoffset failed");
                }
                break;
            default:
                {
                    if (H5Pset_chunk(dcpl_id, obj.chunk.rank, obj.chunk.chunk_lengths) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_chunk failed");
                    if (H5Pset_filter(dcpl_id, obj.filter[i].filtn,
                            obj.filter[i].filt_flag, obj.filter[i].cd_nelmts,
                            obj.filter[i].cd_values) < 0)
                        HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_filter failed");
                }
                break;
            } /* switch */
        }/*i*/
    }
    /*obj.nfilters*/

    if (filtobj.nfilters) {
        for (i = 0; i < filtobj.nfilters; i++) {
            if (filtobj.filter[i].filtn < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "invalid filter");

            if (H5Zfilter_avail(filtobj.filter[i].filtn) <= 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "%d filter unavailable", filtobj.filter[i].filtn);
        } /* for */
    } /* nfilters */

    /*-------------------------------------------------------------------------
     * layout
     *-------------------------------------------------------------------------
     */

    if (obj.layout >= 0) {
        /* a layout was defined */
        if (H5Pset_layout(dcpl_id, obj.layout) < 0)
            HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_layout failed");

        if (H5D_CHUNKED == obj.layout) {
            if (H5Pset_chunk(dcpl_id, obj.chunk.rank, obj.chunk.chunk_lengths) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_chunk failed");
        }
        else if (H5D_COMPACT == obj.layout) {
            if (H5Pset_alloc_time(dcpl_id, H5D_ALLOC_TIME_EARLY) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Pset_alloc_time failed");
        }
        /* remove filters for the H5D_CONTIGUOUS case */
        else if (H5D_CONTIGUOUS == obj.layout) {
            if (H5Premove_filter(dcpl_id, H5Z_FILTER_ALL) < 0)
                HGOTO_ERROR(FAIL, H5E_tools_min_id_g, "H5Premove_filter failed");
        }
    }

done:
    return ret_value;
}

