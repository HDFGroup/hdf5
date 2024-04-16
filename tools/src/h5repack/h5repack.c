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
#include "h5repack.h"
#include "h5tools.h"
#include "h5tools_utils.h"

/*-------------------------------------------------------------------------
 * File: h5repack.c
 * Purpose: Public API functions
 *-------------------------------------------------------------------------
 */

static int         check_options(pack_opt_t *options);
static int         check_objects(const char *fname, pack_opt_t *options);
static const char *get_sfilter(H5Z_filter_t filtn);
static int         have_request(pack_opt_t *options);

/*-------------------------------------------------------------------------
 * Function: h5repack
 *
 * Purpose: locate all high-level HDF5 objects in the file
 *  and compress/chunk them using options
 *
 * Algorithm: 2 traversals are made to the file; the 1st builds a list of
 *  the objects, the 2nd makes a copy of them, using the options;
 *  the reason for the 1st traversal is to check for invalid
 *  object name requests
 *
 * Return: 0, ok, -1, fail
 *-------------------------------------------------------------------------
 */
int
h5repack(const char *infile, const char *outfile, pack_opt_t *options)
{
    /* check input */
    if (check_options(options) < 0)
        return -1;

    /* check for objects in input that are in the file */
    if (check_objects(infile, options) < 0)
        return -1;

    /* copy the objects  */
    if (copy_objects(infile, outfile, options) < 0)
        return -1;

    return 0;
} /* end h5repack() */

/*-------------------------------------------------------------------------
 * Function: h5repack_init
 *
 * Purpose: initialize options
 *
 * Return: 0, ok, -1, fail
 *-------------------------------------------------------------------------
 */
int
h5repack_init(pack_opt_t *options, int verbose, bool latest)
{
    int k, n;

    memset(options, 0, sizeof(pack_opt_t));
    options->min_comp   = 0;
    options->verbose    = verbose;
    options->latest     = latest;
    options->layout_g   = H5D_LAYOUT_ERROR;
    options->low_bound  = H5F_LIBVER_EARLIEST;
    options->high_bound = H5F_LIBVER_LATEST;
    options->fin_fapl   = H5P_DEFAULT;
    options->fout_fapl  = H5P_DEFAULT;

    for (n = 0; n < H5_REPACK_MAX_NFILTERS; n++) {
        options->filter_g[n].filtn     = -1;
        options->filter_g[n].cd_nelmts = 0;
        for (k = 0; k < CD_VALUES; k++)
            options->filter_g[n].cd_values[k] = 0;
    }

    return (options_table_init(&(options->op_tbl)));
} /* end h5repack_init() */

/*-------------------------------------------------------------------------
 * Function: h5repack_end
 *
 * Purpose: free options table
 *-------------------------------------------------------------------------
 */

int
h5repack_end(pack_opt_t *options)
{
    return options_table_free(options->op_tbl);
} /* end h5repack_end() */

/*-------------------------------------------------------------------------
 * Function: h5repack_addfilter
 *
 * Purpose: add a compression -f option to table
 *   Example: -f dset:GZIP=6
 *
 * Return: 0, ok, -1, fail
 *-------------------------------------------------------------------------
 */
int
h5repack_addfilter(const char *str, pack_opt_t *options)
{
    obj_list_t   *obj_list = NULL; /* one object list for the -f and -l option entry */
    filter_info_t filter;          /* filter info for the current -f option entry */
    unsigned      n_objs;          /* number of objects in the current -f or -l option entry */
    int           is_glb;          /* is the filter global */

    /* parse the -f (--filter) option */
    if (NULL == (obj_list = parse_filter(str, &n_objs, &filter, options, &is_glb)))
        return -1;

    /* if it applies to all objects */
    if (is_glb) {
        int n;

        n = options->n_filter_g++; /* increase # of global filters */
        if (options->n_filter_g > H5_REPACK_MAX_NFILTERS) {
            error_msg("maximum number of filters exceeded for <%s>\n", str);
            free(obj_list);
            return -1;
        }

        options->filter_g[n] = filter;
    }
    else
        options_add_filter(obj_list, n_objs, filter, options->op_tbl);

    free(obj_list);
    return 0;
} /* end h5repack_addfilter() */

/*-------------------------------------------------------------------------
 * Function: h5repack_addlayout
 *
 * Purpose: add a layout option
 *
 * Return: 0, ok, -1, fail
 *-------------------------------------------------------------------------
 */
int
h5repack_addlayout(const char *str, pack_opt_t *options)
{
    obj_list_t *obj_list = NULL; /*one object list for the -t and -c option entry */
    unsigned    n_objs;          /*number of objects in the current -t or -c option entry */
    pack_info_t pack;            /*info about layout to extract from parse */
    int         j;
    int         ret_value = -1;

    init_packobject(&pack);

    if (options->all_layout == 1) {
        error_msg("invalid layout input: 'all' option is present with other objects <%s>\n", str);
        return ret_value;
    }

    /* parse the layout option */
    obj_list = parse_layout(str, &n_objs, &pack, options);
    if (obj_list) {
        /* set layout option */
        options->layout_g = pack.layout;

        /* no individual dataset specified */
        if (options->all_layout == 1) {
            if (pack.layout == H5D_CHUNKED) {
                /* -2 means the NONE option, remove chunking
                 and set the global layout to contiguous */
                if (pack.chunk.rank == -2) /* TODO: fix 'magic number' */
                    options->layout_g = H5D_CONTIGUOUS;
                /* otherwise set the global chunking type */
                else {
                    options->chunk_g.rank = pack.chunk.rank;
                    for (j = 0; j < pack.chunk.rank; j++)
                        options->chunk_g.chunk_lengths[j] = pack.chunk.chunk_lengths[j];
                }
            }
        }

        /* individual dataset specified */
        if (options->all_layout == 0)
            ret_value = options_add_layout(obj_list, n_objs, &pack, options->op_tbl);

        free(obj_list);
        ret_value = 0;
    } /* end if obj_list exists */

    return ret_value;
} /* end h5repack_addlayout() */

/* Note: The below copy_named_datatype(), named_datatype_free(), copy_attr()
 * were located in h5repack_copy.c as static prior to bugfix1726.
 * Made shared functions as copy_attr() was needed in h5repack_refs.c.
 * However copy_attr() may be obsoleted when H5Acopy is available and put back
 * others to static in h5repack_copy.c.
 */
/*-------------------------------------------------------------------------
 * Function: copy_named_datatype
 *
 * Purpose: Copies the specified datatype anonymously, and returns an open
 *          id for that datatype in the output file.  The first time this
 *          is called it scans every named datatype in travt into a
 *          private stack, afterwards it simply scans that stack.  The id
 *          returned must be closed after it is no longer needed.
 *          named_datatype_free must be called before the program exits
 *          to free the stack.
 *-------------------------------------------------------------------------
 */

hid_t
copy_named_datatype(hid_t type_in, hid_t fidout, named_dt_t **named_dt_head_p, trav_table_t *travt,
                    pack_opt_t *options)
{
    named_dt_t *dt     = NULL; /* Stack pointer */
    named_dt_t *dt_ret = NULL; /* Datatype to return */
    H5O_info2_t oinfo;         /* Object info of input dtype */
    int         token_cmp;
    hid_t       ret_value = H5I_INVALID_HID;

    if (H5Oget_info3(type_in, &oinfo, H5O_INFO_BASIC) < 0)
        H5TOOLS_GOTO_ERROR(H5I_INVALID_HID, "H5Oget_info failed");

    if (*named_dt_head_p) {
        /* Search the stack for the datatype. */
        for (dt = *named_dt_head_p; dt != NULL; dt = dt->next) {
            if (H5Otoken_cmp(type_in, &dt->obj_token, &oinfo.token, &token_cmp) < 0)
                H5TOOLS_GOTO_ERROR(H5I_INVALID_HID, "failed to compare object tokens");

            if (token_cmp == 0)
                break; // found it!
        }

        dt_ret = dt;
    }
    else {
        /* Create the stack */
        size_t i;

        for (i = 0; i < travt->nobjs; i++) {
            if (travt->objs[i].type == H5TRAV_TYPE_NAMED_DATATYPE) {
                /* Push onto the stack */
                if (NULL == (dt = (named_dt_t *)malloc(sizeof(named_dt_t))))
                    H5TOOLS_GOTO_ERROR(H5I_INVALID_HID, "buffer allocation failed failed");
                dt->next         = *named_dt_head_p;
                *named_dt_head_p = dt;

                /* Update the token/address and id */
                memcpy(&dt->obj_token, &travt->objs[i].obj_token, sizeof(H5O_token_t));
                dt->id_out = H5I_INVALID_HID;

                /* Check if this type is the one requested */
                if (H5Otoken_cmp(type_in, &oinfo.token, &dt->obj_token, &token_cmp) < 0)
                    H5TOOLS_GOTO_ERROR(H5I_INVALID_HID, "failed to compare object tokens");
                if (!token_cmp)
                    dt_ret = dt;
            } /* end if named datatype */
        }     /* end for each object in traversal table */
    }         /* end else (create the stack) */

    /* Handle the case that the requested datatype was not found.  This is
     * possible if the datatype was committed anonymously in the input file.
     */
    if (!dt_ret) {
        /* Push the new datatype onto the stack */
        if (NULL == (dt_ret = (named_dt_t *)malloc(sizeof(named_dt_t))))
            H5TOOLS_GOTO_ERROR(H5I_INVALID_HID, "buffer allocation failed failed");
        dt_ret->next     = *named_dt_head_p;
        *named_dt_head_p = dt_ret;

        /* Update the token/address and id */
        memcpy(&dt_ret->obj_token, &oinfo.token, sizeof(H5O_token_t));
        dt_ret->id_out = H5I_INVALID_HID;
    } /* end if requested datatype not found */

    /* If the requested datatype does not yet exist in the output file, copy it
     * anonymously
     */
    if (dt_ret->id_out < 0) {
        if (options->use_native == 1)
            dt_ret->id_out = H5Tget_native_type(type_in, H5T_DIR_DEFAULT);
        else
            dt_ret->id_out = H5Tcopy(type_in);
        if (dt_ret->id_out < 0)
            H5TOOLS_GOTO_ERROR(H5I_INVALID_HID, "H5Tget_native_type-H5Tcopy failed");
        if (H5Tcommit_anon(fidout, dt_ret->id_out, H5P_DEFAULT, H5P_DEFAULT) < 0)
            H5TOOLS_GOTO_ERROR(H5I_INVALID_HID, "H5Tcommit_anon failed");
    } /* end if named datatype not yet in output file */

    /* Set return value */
    ret_value = dt_ret->id_out;

    /* Increment the ref count on id_out, because the calling function will try
     * to close it. (TODO: fix scope envy)
     */
    if (H5Iinc_ref(ret_value) < 0)
        H5TOOLS_GOTO_ERROR(H5I_INVALID_HID, "H5Iinc_ref failed");

done:
    return ret_value;
} /* end copy_named_datatype() */

/*-------------------------------------------------------------------------
 * Function: named_datatype_free
 *
 * Purpose: Frees the stack of named datatypes.
 *-------------------------------------------------------------------------
 */
int
named_datatype_free(named_dt_t **named_dt_head_p, int ignore_err)
{
    named_dt_t *dt        = *named_dt_head_p;
    int         ret_value = -1;

    while (dt) {
        /* Pop the datatype off the stack and free it */
        if (H5Tclose(dt->id_out) < 0 && !ignore_err)
            H5TOOLS_GOTO_ERROR((-1), "H5Tclose failed");
        dt = dt->next;
        free(*named_dt_head_p);
        *named_dt_head_p = dt;
    }

    ret_value = 0;

done:
    return (ret_value);
} /* end named_datatype_free() */

/*-------------------------------------------------------------------------
 * Function: copy_attr
 *
 * Purpose: copy attributes located in LOC_IN, which is obtained either from
 * loc_id = H5Gopen2( fid, name);
 * loc_id = H5Dopen2( fid, name);
 * loc_id = H5Topen2( fid, name);
 *
 * Return: 0, ok, -1 no
 *-------------------------------------------------------------------------
 */
int
copy_attr(hid_t loc_in, hid_t loc_out, named_dt_t **named_dt_head_p, trav_table_t *travt, pack_opt_t *options)
{
    hid_t         attr_id  = H5I_INVALID_HID; /* attr ID */
    hid_t         attr_out = H5I_INVALID_HID; /* attr ID */
    hid_t         space_id = H5I_INVALID_HID; /* space ID */
    hid_t         ftype_id = H5I_INVALID_HID; /* file type ID */
    hid_t         wtype_id = H5I_INVALID_HID; /* read/write type ID */
    size_t        msize;                      /* size of type */
    void         *buf = NULL;                 /* data buffer */
    hsize_t       nelmts;                     /* number of elements in dataset */
    int           rank;                       /* rank of dataset */
    htri_t        is_named;                   /* Whether the datatype is named */
    hsize_t       dims[H5S_MAX_RANK];         /* dimensions of dataset */
    H5_timer_t    timer;                      /* Timer for read/write operations */
    H5_timevals_t times;                      /* Elapsed time for each operation */
    static double read_time  = 0;
    static double write_time = 0;
    char          name[255];
    H5O_info2_t   oinfo; /* object info */
    int           j;
    unsigned      u;
    bool          is_ref     = 0;
    H5T_class_t   type_class = -1;
    int           ret_value  = 0;

    if (H5Oget_info3(loc_in, &oinfo, H5O_INFO_NUM_ATTRS) < 0)
        H5TOOLS_GOTO_ERROR((-1), "H5Oget_info failed");

    /*-------------------------------------------------------------------------
     * copy all attributes
     *-------------------------------------------------------------------------
     */
    for (u = 0; u < (unsigned)oinfo.num_attrs; u++) {
        /* open attribute */
        if ((attr_id = H5Aopen_by_idx(loc_in, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)u, H5P_DEFAULT,
                                      H5P_DEFAULT)) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Aopen_by_idx failed");

        if (H5Aget_name(attr_id, (size_t)255, name) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Pclose failed");

        /* get the file datatype  */
        if ((ftype_id = H5Aget_type(attr_id)) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Aget_type failed");

        /* Check if the datatype is committed */
        if ((is_named = H5Tcommitted(ftype_id)) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Tcommitted failed");
        if (is_named && travt) {
            hid_t fidout = H5I_INVALID_HID;

            /* Create out file id */
            if ((fidout = H5Iget_file_id(loc_out)) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Iget_file_id failed");

            /* Copy named dt */
            if ((wtype_id = copy_named_datatype(ftype_id, fidout, named_dt_head_p, travt, options)) < 0) {
                H5Fclose(fidout);
                H5TOOLS_GOTO_ERROR((-1), "copy_named_datatype failed");
            }

            if (H5Fclose(fidout) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Fclose failed");
        } /* end if datatype is committed and we have a traversal table */
        else {
            if (options->use_native == 1)
                wtype_id = H5Tget_native_type(ftype_id, H5T_DIR_DEFAULT);
            else
                wtype_id = H5Tcopy(ftype_id);
        } /* end else: uncommitted datatype and/or no traversal table */

        /* get the dataspace handle  */
        if ((space_id = H5Aget_space(attr_id)) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Aget_space failed");

        /* get dimensions  */
        if ((rank = H5Sget_simple_extent_dims(space_id, dims, NULL)) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Sget_simple_extent_dims failed");

        nelmts = 1;
        for (j = 0; j < rank; j++)
            nelmts *= dims[j];

        if ((msize = H5Tget_size(wtype_id)) == 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Tget_size failed");

        /*---------------------------------------------------------------------
         * object references are a special case. We cannot just copy the
         * buffers, but instead we recreate the reference.
         * This is done on a second sweep of the file that just copies the
         * referenced objects at copy_refs_attr().
         *---------------------------------------------------------------------
         */
        type_class = H5Tget_class(wtype_id);
        is_ref     = (type_class == H5T_REFERENCE);
        if (type_class == H5T_VLEN || type_class == H5T_ARRAY) {
            hid_t base_type = H5I_INVALID_HID;

            base_type = H5Tget_super(ftype_id);
            is_ref    = (is_ref || (H5Tget_class(base_type) == H5T_REFERENCE));
            if (H5Tclose(base_type) < 0)
                H5TOOLS_ERROR((-1), "H5Tclose base_type failed");
        } /* end if type_class is variable length or array */

        if (type_class == H5T_COMPOUND) {
            int nmembers = H5Tget_nmembers(wtype_id);

            for (j = 0; j < nmembers; j++) {
                hid_t       mtid    = H5Tget_member_type(wtype_id, (unsigned)j);
                H5T_class_t mtclass = H5Tget_class(mtid);
                if (H5Tclose(mtid) < 0)
                    H5TOOLS_ERROR((-1), "H5Tclose mtid failed");

                if (mtclass == H5T_REFERENCE) {
                    is_ref = 1;
                    break;
                }
            } /* end for each member */
        }     /* end if type_class is H5T_COMPOUND */

        read_time  = 0;
        write_time = 0;

        if (!is_ref) {
            /*-----------------------------------------------------------------
             * read to memory
             *-----------------------------------------------------------------
             */

            buf = (void *)calloc(1, (size_t)(nelmts * msize));
            if (buf == NULL) {
                H5TOOLS_GOTO_ERROR((-1), "malloc failed");
            } /* end if */
            if (options->verbose == 2) {
                H5_timer_init(&timer);
                H5_timer_start(&timer);
            }
            if (H5Aread(attr_id, wtype_id, buf) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Aread failed");
            if (options->verbose == 2) {
                H5_timer_stop(&timer);
                H5_timer_get_times(timer, &times);
                read_time += times.elapsed;
            }

            /*-----------------------------------------------------------------
             * copy
             *-----------------------------------------------------------------
             */

            if ((attr_out = H5Acreate2(loc_out, name, wtype_id, space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Acreate2 failed on ,%s>", name);

            if (options->verbose == 2) {
                H5_timer_init(&timer);
                H5_timer_start(&timer);
            }
            if (H5Awrite(attr_out, wtype_id, buf) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Awrite failed");
            if (options->verbose == 2) {
                H5_timer_stop(&timer);
                H5_timer_get_times(timer, &times);
                write_time += times.elapsed;
            }

            /*close*/
            if (H5Aclose(attr_out) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Aclose failed");

            /* Check if we have VL data and string in the attribute's  datatype that must
             * be reclaimed */
            if (true == h5tools_detect_vlen(wtype_id))
                H5Treclaim(wtype_id, space_id, H5P_DEFAULT, buf);

            free(buf);
            buf = NULL;
        } /*H5T_REFERENCE*/

        if (options->verbose > 0) {
            if (options->verbose == 2)
                printf(FORMAT_OBJ_ATTR_TIME, "attr", read_time, write_time, name);
            else
                printf(FORMAT_OBJ_ATTR, "attr", name);
        }

        /*---------------------------------------------------------------------
         * close
         *---------------------------------------------------------------------
         */
        if (H5Sclose(space_id) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Sclose failed");
        space_id = H5I_INVALID_HID;
        if (H5Tclose(wtype_id) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Tclose failed");
        wtype_id = H5I_INVALID_HID;
        if (H5Tclose(ftype_id) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Tclose failed");
        ftype_id = H5I_INVALID_HID;
        if (H5Aclose(attr_id) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Aclose failed");
        attr_id = H5I_INVALID_HID;
    } /* for u (each attribute) */

done:
    H5E_BEGIN_TRY
    {
        if (buf) {
            /* Check if we have VL data and string in the attribute's
             * datatype that must be reclaimed
             */
            if (true == h5tools_detect_vlen(wtype_id))
                H5Treclaim(wtype_id, space_id, H5P_DEFAULT, buf);

            /* Free buf */
            free(buf);
        }

        H5Aclose(attr_out);
        H5Sclose(space_id);
        H5Tclose(wtype_id);
        H5Tclose(ftype_id);
        H5Aclose(attr_id);
    }
    H5E_END_TRY

    return ret_value;
} /* end copy_attr() */

/*-----------------------------------------------------------------------------
 * Function: check_options
 *
 * Purpose: print options, checks for invalid options
 *
 * Return: void, return -1 on error
 *-----------------------------------------------------------------------------
 */
static int
check_options(pack_opt_t *options)
{
    unsigned int i;
    int          k, j, has_cp = 0, has_ck = 0;
    char         slayout[30];
    int          ret_value = 0;

    /*-------------------------------------------------------------------------
     * Objects to layout
     *-------------------------------------------------------------------------
     */
    if (options->verbose > 0 && have_request(options)) {
        if (options->all_layout == 1) {
            printf("All objects to modify layout are...\n");
            switch (options->layout_g) {
                case H5D_COMPACT:
                    strcpy(slayout, "compact");
                    break;
                case H5D_CONTIGUOUS:
                    strcpy(slayout, "contiguous");
                    break;
                case H5D_CHUNKED:
                    strcpy(slayout, "chunked");
                    break;
                case H5D_VIRTUAL:
                    strcpy(slayout, "virtual");
                    break;
                case H5D_LAYOUT_ERROR:
                case H5D_NLAYOUTS:
                    H5TOOLS_GOTO_ERROR((-1), "invalid layout");
                    break;
                default:
                    strcpy(slayout, "invalid layout\n");
                    H5TOOLS_GOTO_DONE((-1));
            }
            printf(" Apply %s layout to all", slayout);
            if (H5D_CHUNKED == options->layout_g) {
                printf("with dimension [ ");
                for (j = 0; j < options->chunk_g.rank; j++)
                    printf("%d ", (int)options->chunk_g.chunk_lengths[j]);
                printf("]");
            }
            printf("\n");
        }
        else
            printf("No all objects to modify layout\n");
    } /* end if verbose */

    for (i = 0; i < options->op_tbl->nelems; i++) {
        char *name = options->op_tbl->objs[i].path;

        if (options->op_tbl->objs[i].chunk.rank > 0) {
            if (options->verbose > 0) {
                printf(" <%s> with chunk size ", name);
                for (k = 0; k < options->op_tbl->objs[i].chunk.rank; k++)
                    printf("%d ", (int)options->op_tbl->objs[i].chunk.chunk_lengths[k]);
                printf("\n");
            }
            has_ck = 1;
        }
        else if (options->op_tbl->objs[i].chunk.rank == -2) { /* TODO: replace 'magic number' */
            if (options->verbose > 0)
                printf(" <%s> %s\n", name, "NONE (contiguous)");
            has_ck = 1;
        }
    } /* end for each object in options */

    if (options->all_layout == 1 && has_ck)
        H5TOOLS_GOTO_ERROR((-1), "invalid chunking input: 'all' option is present with other objects");

    /*-------------------------------------------------------------------------
     * Objects to filter
     *-------------------------------------------------------------------------
     */

    if (options->verbose > 0 && have_request(options)) {
        if (options->all_filter == 1) {
            printf("All objects to apply filter are...\n");
            for (k = 0; k < options->n_filter_g; k++) {
                H5Z_filter_t filtn = options->filter_g[k].filtn;
                if (filtn < 0) {
                    printf(" Unknown\n");
                    continue;
                }
                switch (filtn) {
                    case H5Z_FILTER_NONE:
                        printf(" Uncompress all\n");
                        break;
                    case H5Z_FILTER_SHUFFLE:
                    case H5Z_FILTER_FLETCHER32:
                        printf(" All with %s\n", get_sfilter(filtn));
                        break;
                    case H5Z_FILTER_SZIP:
                    case H5Z_FILTER_DEFLATE:
                        printf(" All with %s, parameter %d\n", get_sfilter(filtn),
                               options->filter_g[k].cd_values[0]);
                        break;
                    default:
                        printf(" User Defined %d\n", filtn);
                        break;
                } /* end switch */
            }     /* end for each filter */
        }         /* end if options->all_filter == 1 (TODO: meaning) */
        else
            printf("No all objects to apply filter\n");
    } /* end if verbose */

    for (i = 0; i < options->op_tbl->nelems; i++) {
        pack_info_t pack = options->op_tbl->objs[i];
        char       *name = pack.path;

        for (j = 0; j < pack.nfilters; j++) {
            if (options->verbose > 0) {
                if (pack.filter[j].filtn >= 0) {
                    if (pack.filter[j].filtn > H5Z_FILTER_SCALEOFFSET) {
                        printf(" <%s> with %s filter %d\n", name, get_sfilter(pack.filter[j].filtn),
                               pack.filter[j].filtn);
                    }
                    else {
                        printf(" <%s> with %s filter\n", name, get_sfilter(pack.filter[j].filtn));
                    }
                }
            }
            has_cp = 1;
        } /* end for each filter */
    }     /* end for each object in options table */

    if (options->all_filter == 1 && has_cp)
        H5TOOLS_GOTO_ERROR((-1), "invalid compression input: 'all' option is present with other objects");

    /*-------------------------------------------------------------------------
     * Check options for the latest format
     *-------------------------------------------------------------------------
     */

    if (options->grp_compact < 0)
        H5TOOLS_GOTO_ERROR((-1), "invalid maximum number of links to store as header messages");
    if (options->grp_indexed < 0)
        H5TOOLS_GOTO_ERROR((-1), "invalid minimum number of links to store in the indexed format");
    if (options->grp_indexed > options->grp_compact)
        H5TOOLS_GOTO_ERROR((-1), "minimum indexed size is greater than the maximum compact size");
    for (i = 0; i < 8; i++)
        if (options->msg_size[i] < 0)
            H5TOOLS_GOTO_ERROR((-1), "invalid shared message size");

    /*------------------------------------------------------------------------
     * Verify new user userblock options; file name must be present
     *------------------------------------------------------------------------
     */
    if (options->ublock_filename != NULL && options->ublock_size == 0) {
        if (options->verbose > 0) {
            printf("Warning: user block size missing for file %s. Assigning a default size of 1024...\n",
                   options->ublock_filename);
            options->ublock_size = 1024;
        }
    }

    if (options->ublock_filename == NULL && options->ublock_size != 0)
        H5TOOLS_GOTO_ERROR((-1), "file name missing for user block");

    /*------------------------------------------------------------------------
     * Verify alignment options; threshold is zero default but alignment not
     *------------------------------------------------------------------------
     */

    if (options->alignment == 0 && options->threshold != 0)
        H5TOOLS_GOTO_ERROR((-1), "alignment for H5Pset_alignment missing");

done:
    return ret_value;
} /* end check_options() */

/*-------------------------------------------------------------------------
 * Function: check_objects
 *
 * Purpose: Locate all HDF5 objects in the file and compare with user-supplied
 *          list.
 *
 * Return: 0, ok, -1 no
 *-------------------------------------------------------------------------
 */
static int
check_objects(const char *fname, pack_opt_t *options)
{
    hid_t         fid = H5I_INVALID_HID;
    hid_t         did = H5I_INVALID_HID;
    hid_t         sid = H5I_INVALID_HID;
    unsigned int  i;
    int           ifil;
    trav_table_t *travt     = NULL;
    int           ret_value = 0;

    /* nothing to do */
    if (options->op_tbl->nelems == 0)
        H5TOOLS_GOTO_DONE(0);

    /*-------------------------------------------------------------------------
     * open the file
     *-------------------------------------------------------------------------
     */
    if ((fid = h5tools_fopen(fname, H5F_ACC_RDONLY, options->fin_fapl, (options->fin_fapl != H5P_DEFAULT),
                             NULL, 0)) < 0)
        H5TOOLS_GOTO_ERROR((-1), "h5tools_fopen failed <%s>: %s", fname, H5FOPENERROR);

    /*-------------------------------------------------------------------------
     * get the list of objects in the file
     *-------------------------------------------------------------------------
     */

    /* Initialize indexing options */
    h5trav_set_index(sort_by, sort_order);
    /* init table */
    trav_table_init(fid, &travt);

    /* get the list of objects in the file */
    if (h5trav_gettable(fid, travt) < 0)
        H5TOOLS_GOTO_ERROR((-1), "h5trav_gettable failed");

    /*-------------------------------------------------------------------------
     * compare with user supplied list
     *-------------------------------------------------------------------------
     */

    if (options->verbose > 0)
        printf("Opening file. Searching %zu objects to modify ...\n", travt->nobjs);

    for (i = 0; i < options->op_tbl->nelems; i++) {
        pack_info_t obj  = options->op_tbl->objs[i];
        char       *name = obj.path;

        if (options->verbose > 0)
            printf(" <%s>", name);

        /* the input object names are present in the file and are valid */
        if (h5trav_getindext(name, travt) < 0)
            H5TOOLS_GOTO_ERROR((-1), "%s Could not find <%s> in file <%s>. Exiting...\n",
                               (options->verbose > 0 ? "\n" : ""), name, fname);
        if (options->verbose > 0)
            printf("...Found\n");

        for (ifil = 0; ifil < obj.nfilters; ifil++) {
            if (obj.filter[ifil].filtn < 0)
                H5TOOLS_GOTO_ERROR((-1), "invalid filter");
            /* check for extra filter conditions */
            switch (obj.filter[ifil].filtn) {
                /* chunk size must be smaller than pixels per block */
                case H5Z_FILTER_SZIP: {
                    int      j;
                    hsize_t  csize = 1;
                    unsigned ppb   = obj.filter[ifil].cd_values[0];
                    hsize_t  dims[H5S_MAX_RANK];
                    int      rank;

                    if (obj.chunk.rank > 0) {
                        rank = obj.chunk.rank;
                        for (j = 0; j < rank; j++)
                            csize *= obj.chunk.chunk_lengths[j];
                    }
                    else {
                        if ((did = H5Dopen2(fid, name, H5P_DEFAULT)) < 0)
                            H5TOOLS_GOTO_ERROR((-1), "H5Dopen2 failed");
                        if ((sid = H5Dget_space(did)) < 0)
                            H5TOOLS_GOTO_ERROR((-1), "H5Dget_space failed");
                        if ((rank = H5Sget_simple_extent_ndims(sid)) < 0)
                            H5TOOLS_GOTO_ERROR((-1), "H5Sget_simple_extent_ndims failed");
                        memset(dims, 0, sizeof dims);
                        if (H5Sget_simple_extent_dims(sid, dims, NULL) < 0)
                            H5TOOLS_GOTO_ERROR((-1), "H5Sget_simple_extent_dims failed");
                        for (j = 0; j < rank; j++)
                            csize *= dims[j];
                        if (H5Sclose(sid) < 0)
                            H5TOOLS_GOTO_ERROR((-1), "H5Sclose failed");
                        if (H5Dclose(did) < 0)
                            H5TOOLS_GOTO_ERROR((-1), "H5Dclose failed");
                    }

                    if (csize < ppb) {
                        printf(" <warning: SZIP settings, chunk size is smaller than pixels per block>\n");
                        H5TOOLS_GOTO_DONE(0);
                    }
                } /* end case SZIP */
                break;
                default:
                    break;
            } /* end switch */
        }     /* for ifil (each user-defined filter) */
    }         /* for i (each object in options traversal table) */

done:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Dclose(did);
        H5Fclose(fid);
    }
    H5E_END_TRY
    if (travt)
        trav_table_free(travt);
    return ret_value;
} /* end check_objects() */

/*-------------------------------------------------------------------------
 * Function: have_request
 *
 * Purpose: check if a filter or layout was requested
 *
 * Return: 1 yes, 0 no
 *-------------------------------------------------------------------------
 */
static int
have_request(pack_opt_t *options)
{

    if (options->all_filter || options->all_layout || options->op_tbl->nelems)
        return 1;

    return 0;
} /* end have_request() */

/*-------------------------------------------------------------------------
 * Function: get_sfilter
 *
 * Purpose: return the filter as a string name
 *
 * Return: name of filter, exit on error
 *-------------------------------------------------------------------------
 */
static const char *
get_sfilter(H5Z_filter_t filtn)
{
    if (filtn < 0)
        return NULL;
    else if (filtn == H5Z_FILTER_NONE)
        return "NONE";
    else if (filtn == H5Z_FILTER_DEFLATE)
        return "GZIP";
    else if (filtn == H5Z_FILTER_SZIP)
        return "SZIP";
    else if (filtn == H5Z_FILTER_SHUFFLE)
        return "SHUFFLE";
    else if (filtn == H5Z_FILTER_FLETCHER32)
        return "FLETCHER32";
    else if (filtn == H5Z_FILTER_NBIT)
        return "NBIT";
    else if (filtn == H5Z_FILTER_SCALEOFFSET)
        return "SOFF";
    else
        return "UD";
} /* end get_sfilter() */
