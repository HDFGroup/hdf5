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
#include "h5diff.h"
#include "h5tools.h"

/*-------------------------------------------------------------------------
 * local functions
 *-------------------------------------------------------------------------
 */

static const char *MapIdToName(hid_t refobj_id, trav_table_t *travt);
static int         copy_refs_attr(hid_t loc_in, hid_t loc_out, trav_table_t *travt, hid_t fidout);
static herr_t update_ref_value(hid_t obj_id, H5R_type_t ref_type, void *ref_in, hid_t fid_out, void *ref_out,
                               trav_table_t *travt);

/*-------------------------------------------------------------------------
 * Function: do_copy_refobjs
 *
 * Purpose:  duplicate all referenced HDF5 objects in the file
 *           and create hard links
 *
 * Return:   0, ok, -1 no
 *-------------------------------------------------------------------------
 */

int
do_copy_refobjs(hid_t fidin, hid_t fidout, trav_table_t *travt, pack_opt_t *options) /* repack options */
{
    hid_t        grp_in   = H5I_INVALID_HID; /* read group ID */
    hid_t        grp_out  = H5I_INVALID_HID; /* write group ID */
    hid_t        dset_in  = H5I_INVALID_HID; /* read dataset ID */
    hid_t        dset_out = H5I_INVALID_HID; /* write dataset ID */
    hid_t        type_in  = H5I_INVALID_HID; /* named type ID */
    hid_t        dcpl_id  = H5I_INVALID_HID; /* dataset creation property list ID */
    hid_t        space_id = H5I_INVALID_HID; /* space ID */
    hid_t        ftype_id = H5I_INVALID_HID; /* file data type ID */
    hid_t        mtype_id = H5I_INVALID_HID; /* memory data type ID */
    size_t       msize;                      /* memory size of memory type */
    hsize_t      nelmts;                     /* number of elements in dataset */
    int          rank;                       /* rank of dataset */
    hsize_t      dims[H5S_MAX_RANK];         /* dimensions of dataset */
    unsigned int i, j;
    int          k;
    named_dt_t  *named_dt_head = NULL; /* Pointer to the stack of named datatypes copied */
    int          ret_value     = 0;

    /*-------------------------------------------------------------------------
     * browse
     *-------------------------------------------------------------------------
     */
    for (i = 0; i < travt->nobjs; i++) {
        switch (travt->objs[i].type) {
            /*-------------------------------------------------------------------------
             * H5TRAV_TYPE_GROUP
             *-------------------------------------------------------------------------
             */
            case H5TRAV_TYPE_GROUP:
                /*-------------------------------------------------------------------------
                 * copy referenced objects in attributes
                 *-------------------------------------------------------------------------
                 */
                if ((grp_out = H5Gopen2(fidout, travt->objs[i].name, H5P_DEFAULT)) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Gopen2 failed");

                if ((grp_in = H5Gopen2(fidin, travt->objs[i].name, H5P_DEFAULT)) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Gopen2 failed");

                if (copy_refs_attr(grp_in, grp_out, travt, fidout) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "copy_refs_attr failed");

                if (H5Gclose(grp_out) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Gclose failed");
                if (H5Gclose(grp_in) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Gclose failed");

                /*-------------------------------------------------------------------------
                 * check for hard links
                 *-------------------------------------------------------------------------
                 */
                if (travt->objs[i].nlinks)
                    for (j = 0; j < travt->objs[i].nlinks; j++)
                        H5Lcreate_hard(fidout, travt->objs[i].name, H5L_SAME_LOC,
                                       travt->objs[i].links[j].new_name, H5P_DEFAULT, H5P_DEFAULT);
                break;

            /*-------------------------------------------------------------------------
             * H5TRAV_TYPE_DATASET
             *-------------------------------------------------------------------------
             */
            case H5TRAV_TYPE_DATASET:
                if ((dset_in = H5Dopen2(fidin, travt->objs[i].name, H5P_DEFAULT)) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Dopen2 failed");
                if ((space_id = H5Dget_space(dset_in)) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Dget_space failed");
                if ((ftype_id = H5Dget_type(dset_in)) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Dget_type failed");
                if ((dcpl_id = H5Dget_create_plist(dset_in)) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Dget_create_plist failed");
                if ((rank = H5Sget_simple_extent_ndims(space_id)) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Sget_simple_extent_ndims failed");
                if (H5Sget_simple_extent_dims(space_id, dims, NULL) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Sget_simple_extent_dims failed");
                nelmts = 1;
                for (k = 0; k < rank; k++)
                    nelmts *= dims[k];

                if ((mtype_id = H5Tget_native_type(ftype_id, H5T_DIR_DEFAULT)) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Tget_native_type failed");

                if ((msize = H5Tget_size(mtype_id)) == 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Tget_size failed");

                /*-------------------------------------------------------------------------
                 * check if the dataset creation property list has filters that
                 * are not registered in the current configuration
                 * 1) the external filters GZIP and SZIP might not be available
                 * 2) the internal filters might be turned off
                 *-------------------------------------------------------------------------
                 */
                if (h5tools_canreadf(NULL, dcpl_id) == 1) {
                    /*-------------------------------------------------------------------------
                     * test for a valid output dataset
                     *-------------------------------------------------------------------------
                     */
                    dset_out = FAIL;

                    /*-------------------------------------------------------------------------
                     * object references are a special case
                     * we cannot just copy the buffers, but instead we recreate the reference
                     *-------------------------------------------------------------------------
                     */
                    if (H5Tequal(mtype_id, H5T_STD_REF_OBJ)) {
                        hid_t       refobj_id = H5I_INVALID_HID;
                        hobj_ref_t *refbuf    = NULL; /* buffer for object references */
                        hobj_ref_t *buf       = NULL;
                        const char *refname;
                        unsigned    u;

                        /*-------------------------------------------------------------------------
                         * read to memory
                         *-------------------------------------------------------------------------
                         */
                        if (nelmts) {
                            buf = (hobj_ref_t *)malloc((unsigned)(nelmts * msize));
                            if (buf == NULL) {
                                printf("cannot read into memory\n");
                                H5TOOLS_GOTO_ERROR((-1), "malloc failed");
                            }
                            if (H5Dread(dset_in, mtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
                                H5TOOLS_GOTO_ERROR((-1), "H5Dread failed");

                            refbuf = (hobj_ref_t *)calloc((unsigned)nelmts, msize);
                            if (refbuf == NULL) {
                                printf("cannot allocate memory\n");
                                H5TOOLS_GOTO_ERROR((-1), "calloc failed");
                            }
                            for (u = 0; u < nelmts; u++) {
                                H5E_BEGIN_TRY
                                {
                                    if ((refobj_id =
                                             H5Rdereference2(dset_in, H5P_DEFAULT, H5R_OBJECT, &buf[u])) < 0)
                                        continue;
                                }
                                H5E_END_TRY

                                /* get the name. a valid name could only occur
                                 * in the second traversal of the file
                                 */
                                if ((refname = MapIdToName(refobj_id, travt)) != NULL) {
                                    /* create the reference, -1 parameter for objects */
                                    if (H5Rcreate(&refbuf[u], fidout, refname, H5R_OBJECT, (hid_t)-1) < 0)
                                        H5TOOLS_GOTO_ERROR((-1), "H5Rcreate failed");
                                    if (options->verbose > 0) {
                                        if (options->verbose == 2)
                                            printf(FORMAT_OBJ_NOTIME, "dset", travt->objs[i].name);
                                        else
                                            printf(FORMAT_OBJ, "dset", travt->objs[i].name);
                                        printf("object <%s> object reference created to <%s>\n",
                                               travt->objs[i].name, refname);
                                    }
                                } /*refname*/
                                if (H5Oclose(refobj_id) < 0)
                                    H5TOOLS_ERROR((-1), "H5Oclose refob failed");
                            } /* u */
                        }     /*nelmts*/

                        /*-------------------------------------------------------------------------
                         * create/write dataset/close
                         *-------------------------------------------------------------------------
                         */
                        if ((dset_out = H5Dcreate2(fidout, travt->objs[i].name, mtype_id, space_id,
                                                   H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0)
                            H5TOOLS_GOTO_ERROR((-1), "H5Dcreate2 failed");
                        if (nelmts)
                            if (H5Dwrite(dset_out, mtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, refbuf) < 0)
                                H5TOOLS_GOTO_ERROR((-1), "H5Dwrite failed");

                        if (buf)
                            free(buf);
                        if (refbuf)
                            free(refbuf);

                        /*------------------------------------------------------
                         * copy attrs
                         *----------------------------------------------------*/
                        if (copy_attr(dset_in, dset_out, &named_dt_head, travt, options) < 0)
                            H5TOOLS_GOTO_ERROR((-1), "copy_attr failed");
                    } /*H5T_STD_REF_OBJ*/

                    /*-------------------------------------------------------------------------
                     * dataset region references
                     *-------------------------------------------------------------------------
                     */
                    else if (H5Tequal(mtype_id, H5T_STD_REF_DSETREG)) {
                        hid_t            refobj_id = H5I_INVALID_HID;
                        hdset_reg_ref_t *refbuf    = NULL; /* input buffer for region references */
                        hdset_reg_ref_t *buf       = NULL; /* output buffer */
                        const char      *refname;
                        unsigned         u;

                        /*-------------------------------------------------------------------------
                         * read input to memory
                         *-------------------------------------------------------------------------
                         */
                        if (nelmts) {
                            buf = (hdset_reg_ref_t *)malloc((unsigned)(nelmts * msize));
                            if (buf == NULL) {
                                printf("cannot read into memory\n");
                                H5TOOLS_GOTO_ERROR((-1), "malloc failed");
                            }
                            if (H5Dread(dset_in, mtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
                                H5TOOLS_GOTO_ERROR((-1), "H5Dread failed");

                            /*-------------------------------------------------------------------------
                             * create output
                             *-------------------------------------------------------------------------
                             */
                            refbuf = (hdset_reg_ref_t *)calloc(sizeof(hdset_reg_ref_t),
                                                               (size_t)nelmts); /*init to zero */
                            if (refbuf == NULL) {
                                printf("cannot allocate memory\n");
                                H5TOOLS_GOTO_ERROR((-1), "calloc failed");
                            }

                            for (u = 0; u < nelmts; u++) {
                                H5E_BEGIN_TRY
                                {
                                    if ((refobj_id = H5Rdereference2(dset_in, H5P_DEFAULT, H5R_DATASET_REGION,
                                                                     &buf[u])) < 0)
                                        continue;
                                }
                                H5E_END_TRY

                                /* get the name. a valid name could only occur
                                 * in the second traversal of the file
                                 */
                                if ((refname = MapIdToName(refobj_id, travt)) != NULL) {
                                    hid_t region_id =
                                        H5I_INVALID_HID; /* region id of the referenced dataset */

                                    if ((region_id = H5Rget_region(dset_in, H5R_DATASET_REGION, &buf[u])) < 0)
                                        H5TOOLS_GOTO_ERROR((-1), "H5Rget_region failed");

                                    /* create the reference, we need the space_id */
                                    if (H5Rcreate(&refbuf[u], fidout, refname, H5R_DATASET_REGION,
                                                  region_id) < 0)
                                        H5TOOLS_GOTO_ERROR((-1), "H5Rcreate failed");
                                    if (H5Sclose(region_id) < 0)
                                        H5TOOLS_GOTO_ERROR((-1), "H5Sclose failed");
                                    if (options->verbose > 0) {
                                        printf(FORMAT_OBJ, "dset", travt->objs[i].name);
                                        printf("object <%s> region reference created to <%s>\n",
                                               travt->objs[i].name, refname);
                                    }
                                } /*refname*/
                                if (H5Oclose(refobj_id) < 0)
                                    H5TOOLS_ERROR((-1), "H5Oclose refobj_id failed");
                            } /* u */
                        }     /*nelmts*/

                        /*-------------------------------------------------------------------------
                         * create/write dataset/close
                         *-------------------------------------------------------------------------
                         */
                        if ((dset_out = H5Dcreate2(fidout, travt->objs[i].name, mtype_id, space_id,
                                                   H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0)
                            H5TOOLS_GOTO_ERROR((-1), "H5Dcreate2 failed");
                        if (nelmts)
                            if (H5Dwrite(dset_out, mtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, refbuf) < 0)
                                H5TOOLS_GOTO_ERROR((-1), "H5Dwrite failed");

                        if (buf)
                            free(buf);
                        if (refbuf)
                            free(refbuf);

                        /*-----------------------------------------------------
                         * copy attrs
                         *----------------------------------------------------*/
                        if (copy_attr(dset_in, dset_out, &named_dt_head, travt, options) < 0)
                            H5TOOLS_GOTO_ERROR((-1), "copy_attr failed");
                    } /* H5T_STD_REF_DSETREG */
                    /*-------------------------------------------------------------------------
                     * not references, open previously created object in 1st traversal
                     *-------------------------------------------------------------------------
                     */
                    else {
                        if ((dset_out = H5Dopen2(fidout, travt->objs[i].name, H5P_DEFAULT)) < 0)
                            H5TOOLS_GOTO_ERROR((-1), "H5Dopen2 failed");
                    } /* end else */

                    /*-------------------------------------------------------------------------
                     * copy referenced objects in attributes
                     *-------------------------------------------------------------------------
                     */
                    if (copy_refs_attr(dset_in, dset_out, travt, fidout) < 0)
                        H5TOOLS_GOTO_ERROR((-1), "copy_refs_attr failed");

                    /*-------------------------------------------------------------------------
                     * check for hard links
                     *-------------------------------------------------------------------------
                     */
                    if (travt->objs[i].nlinks)
                        for (j = 0; j < travt->objs[i].nlinks; j++)
                            H5Lcreate_hard(fidout, travt->objs[i].name, H5L_SAME_LOC,
                                           travt->objs[i].links[j].new_name, H5P_DEFAULT, H5P_DEFAULT);

                    if (H5Dclose(dset_out) < 0)
                        H5TOOLS_GOTO_ERROR((-1), "H5Dclose failed");
                } /*can_read*/

                /*-------------------------------------------------------------------------
                 * close
                 *-------------------------------------------------------------------------
                 */
                if (H5Tclose(ftype_id) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Tclose failed");
                if (H5Tclose(mtype_id) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Tclose failed");
                if (H5Pclose(dcpl_id) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Pclose failed");
                if (H5Sclose(space_id) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Sclose failed");
                if (H5Dclose(dset_in) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Dclose failed");
                break;

            /*-------------------------------------------------------------------------
             * H5TRAV_TYPE_NAMED_DATATYPE
             *-------------------------------------------------------------------------
             */
            case H5TRAV_TYPE_NAMED_DATATYPE:
                if ((type_in = H5Topen2(fidin, travt->objs[i].name, H5P_DEFAULT)) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Topen2 failed");
                if (H5Tclose(type_in) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Tclose failed");
                break;

            /*-------------------------------------------------------------------------
             * H5TRAV_TYPE_LINK
             *-------------------------------------------------------------------------
             */
            case H5TRAV_TYPE_LINK:
                /*nothing to do */
                break;

            case H5TRAV_TYPE_UNKNOWN:
            case H5TRAV_TYPE_UDLINK:
                H5TOOLS_GOTO_ERROR((-1), "H5TRAV invalid type");
                break;

            default:
                break;
        } /* end switch */
    }     /* end for */

    /* Finalize (link) the stack of named datatypes (if any)
     * This function is paired with copy_named_datatype() which is called
     * in copy_attr(), so need to free.
     */
    if (named_datatype_free(&named_dt_head, 0) < 0)
        H5TOOLS_ERROR((-1), "named_datatype_free failed");

    return ret_value;

done:
    H5E_BEGIN_TRY
    {
        H5Gclose(grp_in);
        H5Gclose(grp_out);
        H5Pclose(dcpl_id);
        H5Sclose(space_id);
        H5Dclose(dset_in);
        H5Dclose(dset_out);
        H5Tclose(ftype_id);
        H5Tclose(mtype_id);
        H5Tclose(type_in);
        named_datatype_free(&named_dt_head, 1);
    }
    H5E_END_TRY

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: copy_refs_attr
 *
 * Purpose:  duplicate all referenced HDF5 located in attributes
 *           relative to LOC_IN, which is obtained either from
 *           loc_id = H5Gopen2(fid, name, H5P_DEFAULT);
 *           loc_id = H5Dopen2(fid, name, H5P_DEFAULT);
 *           loc_id = H5Topen2(fid, name, H5P_DEFAULT);
 *
 * Return:   0, ok, -1 no
 *
 * Modified:
 *           Update values of references(object and region) for the following types:
 *               1) References,
 *               2) ARRAY of reference,
 *               3) VLEN of references.
 *               4) COMPOUND of references.
 *           This function does not handle references in other complicated structures,
 *           such as references in nested compound datatypes.
 *-------------------------------------------------------------------------
 */

static int
copy_refs_attr(hid_t loc_in, hid_t loc_out, trav_table_t *travt, hid_t fidout) /* for saving references */
{
    hid_t       attr_id  = H5I_INVALID_HID; /* attr ID */
    hid_t       attr_out = H5I_INVALID_HID; /* attr ID */
    hid_t       space_id = H5I_INVALID_HID; /* space ID */
    hid_t       ftype_id = H5I_INVALID_HID; /* file data type ID */
    hid_t       mtype_id = H5I_INVALID_HID; /* memory data type ID */
    size_t      msize;                      /* memory size of type */
    hsize_t     nelmts;                     /* number of elements in dataset */
    hsize_t     dims[H5S_MAX_RANK];         /* dimensions of dataset */
    char        name[255];
    H5O_info2_t oinfo; /* Object info */
    unsigned    u, i, j;
    int         rank;
    H5T_class_t type_class = -1;
    bool        is_ref = 0, is_ref_vlen = 0, is_ref_array = 0, is_ref_comp = 0;
    void       *refbuf           = NULL;
    void       *buf              = NULL;
    unsigned   *ref_comp_index   = NULL;
    size_t     *ref_comp_size    = NULL;
    int         ref_comp_field_n = 0;
    int         ret_value        = 0;

    if (H5Oget_info3(loc_in, &oinfo, H5O_INFO_NUM_ATTRS) < 0)
        H5TOOLS_GOTO_ERROR((-1), "H5Oget_info failed");

    for (u = 0; u < (unsigned)oinfo.num_attrs; u++) {
        is_ref = is_ref_vlen = is_ref_array = is_ref_comp = 0;

        /* open attribute */
        if ((attr_id = H5Aopen_by_idx(loc_in, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)u, H5P_DEFAULT,
                                      H5P_DEFAULT)) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Aopen_by_idx failed");

        /* get the file datatype  */
        if ((ftype_id = H5Aget_type(attr_id)) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Aget_type failed");

        type_class = H5Tget_class(ftype_id);

        if ((mtype_id = H5Tget_native_type(ftype_id, H5T_DIR_DEFAULT)) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Tget_native_type failed");

        if ((msize = H5Tget_size(mtype_id)) == 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Tget_size failed");

        is_ref = (type_class == H5T_REFERENCE);

        if (type_class == H5T_VLEN) {
            hid_t base_type = H5Tget_super(ftype_id);

            is_ref_vlen = (H5Tget_class(base_type) == H5T_REFERENCE);
            msize       = H5Tget_size(base_type);
            if (H5Tclose(base_type) < 0)
                H5TOOLS_ERROR((-1), "H5Tclose base_type failed");
        }
        else if (type_class == H5T_ARRAY) {
            hid_t base_type = H5Tget_super(ftype_id);

            is_ref_array = (H5Tget_class(base_type) == H5T_REFERENCE);
            msize        = H5Tget_size(base_type);
            if (H5Tclose(base_type) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Tclose base_type failed");
        }
        else if (type_class == H5T_COMPOUND) {
            int nmembers = H5Tget_nmembers(ftype_id);

            if (nmembers < 1)
                H5TOOLS_GOTO_ERROR((-1), "H5Tget_nmembers failed");

            ref_comp_index   = (unsigned *)malloc((size_t)nmembers * sizeof(unsigned));
            ref_comp_size    = (size_t *)malloc((size_t)nmembers * sizeof(ref_comp_size));
            ref_comp_field_n = 0;

            for (i = 0; i < (unsigned)nmembers; i++) {
                hid_t mtid = H5Tget_member_type(ftype_id, i);

                if ((H5Tget_class(mtid) == H5T_REFERENCE)) {
                    ref_comp_index[ref_comp_field_n] = i;
                    ref_comp_size[ref_comp_field_n]  = H5Tget_size(mtid);
                    ref_comp_field_n++;
                }
                if (H5Tclose(mtid) < 0)
                    H5TOOLS_ERROR((-1), "H5Tclose mtid failed");
            }

            /* if compound don't contain reference type member, free the above
             * mallocs. Otherwise there can be memory leaks by the 'continue'
             * statement below. */
            if (!ref_comp_field_n) {
                if (ref_comp_index) {
                    free(ref_comp_index);
                    ref_comp_index = NULL;
                }

                if (ref_comp_size) {
                    free(ref_comp_size);
                    ref_comp_size = NULL;
                }
            }
            /* This line below needs to be moved in this loop instead of inserting outside. Otherwise,
               ref_comp_field_n may be >0 for the next attribute, which may not be
               the reference type and will be accidentally treated as the reference type.
               It will then cause the H5Acreate2 failed since that attribute is already created.
               KY 2020-02-07
            */
            is_ref_comp = (ref_comp_field_n > 0);
        }

        if (!(is_ref || is_ref_vlen || is_ref_array || is_ref_comp)) {
            if (H5Tclose(mtype_id) < 0)
                H5TOOLS_ERROR((-1), "H5Tclose mtype_id failed");
            if (H5Tclose(ftype_id) < 0)
                H5TOOLS_ERROR((-1), "H5Tclose ftype_id failed");
            if (H5Aclose(attr_id) < 0)
                H5TOOLS_ERROR((-1), "H5Aclose attr_id failed");
            continue;
        }

        /* get name */
        if (H5Aget_name(attr_id, 255, name) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Aget_name failed");

        /* get the dataspace handle  */
        if ((space_id = H5Aget_space(attr_id)) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Aget_space failed");

        /* get dimensions  */
        if ((rank = H5Sget_simple_extent_dims(space_id, dims, NULL)) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Sget_simple_extent_dims failed");

        /*-------------------------------------------------------------------------
         * elements
         *-------------------------------------------------------------------------
         */
        nelmts = 1;
        for (j = 0; j < (unsigned)rank; j++)
            nelmts *= dims[j];

        if (is_ref_array) {
            unsigned array_rank = 0;
            hsize_t  array_size = 1;
            hsize_t  array_dims[H5S_MAX_RANK];
            hid_t    base_type = H5Tget_super(ftype_id);

            msize = H5Tget_size(base_type);
            if (H5Tclose(base_type) < 0)
                H5TOOLS_ERROR((-1), "H5Tclose base_type failed");

            array_rank = (unsigned)H5Tget_array_ndims(mtype_id);
            H5Tget_array_dims2(mtype_id, array_dims);
            for (j = 0; j < array_rank; j++)
                array_size *= array_dims[j];
            nelmts *= array_size;
        }

        if ((attr_out = H5Acreate2(loc_out, name, ftype_id, space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Acreate2 failed");

        if (nelmts > 0) {
            /* handle object references */
            if ((is_ref || is_ref_array) && (H5R_OBJ_REF_BUF_SIZE == msize)) {
                buf = (hobj_ref_t *)malloc((unsigned)(nelmts * msize));
                if (buf == NULL) {
                    printf("cannot read into memory\n");
                    H5TOOLS_GOTO_ERROR((-1), "malloc failed");
                }
                if (H5Aread(attr_id, mtype_id, buf) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Aread failed");

                refbuf = (hobj_ref_t *)calloc((unsigned)nelmts, msize);
                if (refbuf == NULL) {
                    printf("cannot allocate memory\n");
                    H5TOOLS_GOTO_ERROR((-1), "calloc failed");
                }

                for (i = 0; i < (unsigned)nelmts; i++)
                    if (update_ref_value(attr_id, H5R_OBJECT, &((hobj_ref_t *)buf)[i], fidout,
                                         &((hobj_ref_t *)refbuf)[i], travt) < 0)
                        continue;
            } /* H5T_STD_REF_OBJ */
            /* handle region references */
            else if ((is_ref || is_ref_array) && (H5R_DSET_REG_REF_BUF_SIZE == msize)) {
                buf = (hdset_reg_ref_t *)malloc((unsigned)(nelmts * msize));

                if (buf == NULL) {
                    printf("cannot read into memory\n");
                    H5TOOLS_GOTO_ERROR((-1), "malloc failed");
                }
                if (H5Aread(attr_id, mtype_id, buf) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Aread failed");

                /*-------------------------------------------------------------------------
                 * create output
                 *-------------------------------------------------------------------------
                 */
                refbuf = (hdset_reg_ref_t *)calloc(sizeof(hdset_reg_ref_t), (size_t)nelmts); /*init to zero */
                if (refbuf == NULL) {
                    printf("cannot allocate memory\n");
                    H5TOOLS_GOTO_ERROR((-1), "calloc failed");
                }

                for (i = 0; i < (unsigned)nelmts; i++)
                    if (update_ref_value(attr_id, H5R_DATASET_REGION, &((hdset_reg_ref_t *)buf)[i], fidout,
                                         &((hdset_reg_ref_t *)refbuf)[i], travt) < 0)
                        continue;
            } /* H5T_STD_REF_DSETREG */
            else if (is_ref_vlen) {
                /* handle VLEN of references */

                buf    = (hvl_t *)malloc((unsigned)(nelmts * sizeof(hvl_t)));
                refbuf = buf; /* reuse the read buffer for write */

                if (buf == NULL) {
                    printf("cannot read into memory\n");
                    H5TOOLS_GOTO_ERROR((-1), "malloc failed");
                }

                if (H5Aread(attr_id, mtype_id, buf) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Aread failed");

                if (H5R_OBJ_REF_BUF_SIZE == msize) {
                    hobj_ref_t ref_out;

                    for (i = 0; i < (unsigned)nelmts; i++) {
                        hobj_ref_t *ptr = (hobj_ref_t *)((hvl_t *)buf)[i].p;

                        for (j = 0; j < ((hvl_t *)buf)[i].len; j++) {
                            if (update_ref_value(attr_id, H5R_OBJECT, &(ptr[j]), fidout, &ref_out, travt) < 0)
                                continue;
                            memcpy(&(ptr[j]), &ref_out, msize);
                        }
                    } /* for (i=0; i<nelems; i++) */
                }
                else if (H5R_DSET_REG_REF_BUF_SIZE == msize) {
                    hdset_reg_ref_t ref_out;

                    for (i = 0; i < (unsigned)nelmts; i++) {
                        hdset_reg_ref_t *ptr = (hdset_reg_ref_t *)((hvl_t *)buf)[i].p;

                        for (j = 0; j < ((hvl_t *)buf)[i].len; j++) {
                            if (update_ref_value(attr_id, H5R_DATASET_REGION, &(ptr[j]), fidout, &ref_out,
                                                 travt) < 0)
                                continue;
                            memcpy(&(ptr[j]), &ref_out, msize);
                        }
                    } /* for (i=0; i<nelems; i++) */
                }
            } /* else if (is_ref_vlen) */
            else if (is_ref_comp) {
                /* handle ref fields in a compound */

                buf    = malloc((unsigned)(nelmts * msize));
                refbuf = buf; /* reuse the read buffer for write */

                if (buf == NULL) {
                    printf("cannot read into memory\n");
                    H5TOOLS_GOTO_ERROR((-1), "malloc failed");
                }

                if (H5Aread(attr_id, mtype_id, buf) < 0)
                    H5TOOLS_GOTO_ERROR((-1), "H5Aread failed");

                for (i = 0; i < (unsigned)nelmts; i++) {
                    for (j = 0; j < (unsigned)ref_comp_field_n; j++) {
                        if (ref_comp_size[j] == H5R_OBJ_REF_BUF_SIZE) {
                            size_t     idx = (i * msize) + H5Tget_member_offset(mtype_id, ref_comp_index[j]);
                            hobj_ref_t ref_out;

                            if (update_ref_value(attr_id, H5R_OBJECT,
                                                 (hobj_ref_t *)((void *)(((char *)buf) + idx)), fidout,
                                                 &ref_out,
                                                 travt) < 0) /* Extra (void *) cast to quiet "cast to create
                                                                alignment" warning - 2019/07/05, QAK */
                                continue;
                            memcpy(((char *)buf) + idx, &ref_out, ref_comp_size[j]);
                        } /* if */
                        else if (ref_comp_size[j] == H5R_DSET_REG_REF_BUF_SIZE) {
                            size_t idx = i * msize + H5Tget_member_offset(mtype_id, ref_comp_index[j]);
                            hdset_reg_ref_t ref_out;

                            if (update_ref_value(attr_id, H5R_DATASET_REGION,
                                                 (hdset_reg_ref_t *)(((char *)buf) + idx), fidout, &ref_out,
                                                 travt) < 0)
                                continue;
                            memcpy(((char *)buf) + idx, &ref_out, ref_comp_size[j]);
                        } /* else if */
                    }     /* j */
                }         /* i */
            }             /* else if (is_ref_comp) */

            if (H5Awrite(attr_out, mtype_id, refbuf) < 0)
                H5TOOLS_GOTO_ERROR((-1), "H5Awrite failed");

            if (is_ref_vlen && buf)
                H5Treclaim(mtype_id, space_id, H5P_DEFAULT, buf);
        } /* if (nelmts) */

        if (refbuf == buf)
            refbuf = NULL; /* set it to NULL to avoid double free since buf and refbuf are the same. */

        if (buf) {
            free(buf);
            buf = NULL;
        }

        if (refbuf) {
            free(refbuf);
            refbuf = NULL;
        }

        if (ref_comp_index) {
            free(ref_comp_index);
            ref_comp_index = NULL;
        }

        if (ref_comp_size) {
            free(ref_comp_size);
            ref_comp_size = NULL;
        }

        if (H5Aclose(attr_out) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Aclose failed");

        /*-------------------------------------------------------------------------
         * close
         *-------------------------------------------------------------------------
         */
        if (H5Tclose(ftype_id) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Tclose failed");
        if (H5Tclose(mtype_id) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Tclose failed");
        if (H5Sclose(space_id) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Sclose failed");
        if (H5Aclose(attr_id) < 0)
            H5TOOLS_GOTO_ERROR((-1), "H5Aclose failed");
    } /* for(u = 0; u < (unsigned)oinfo.num_attrs; u++) */

done:
    if (refbuf)
        free(refbuf);
    if (buf)
        free(buf);

    if (ref_comp_index)
        free(ref_comp_index);

    if (ref_comp_size)
        free(ref_comp_size);

    H5E_BEGIN_TRY
    {
        H5Tclose(ftype_id);
        H5Tclose(mtype_id);
        H5Sclose(space_id);
        H5Aclose(attr_id);
        H5Aclose(attr_out);
    }
    H5E_END_TRY

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    MapIdToName
 *
 * Purpose:     map a ID from a reference to a dataset name
 *
 *-------------------------------------------------------------------------
 */
static const char *
MapIdToName(hid_t refobj_id, trav_table_t *travt)
{
    unsigned int u;
    const char  *ret = NULL;

    /* linear search */
    for (u = 0; u < travt->nobjs; u++) {
        if (travt->objs[u].type == (h5trav_type_t)H5O_TYPE_DATASET ||
            travt->objs[u].type == (h5trav_type_t)H5O_TYPE_GROUP ||
            travt->objs[u].type == (h5trav_type_t)H5O_TYPE_NAMED_DATATYPE) {
            H5O_info2_t ref_oinfo; /* Stat for the refobj id */
            int         token_cmp;

            /* obtain information to identify the referenced object uniquely */
            if (H5Oget_info3(refobj_id, &ref_oinfo, H5O_INFO_BASIC) < 0)
                goto out;

            if (H5Otoken_cmp(refobj_id, &ref_oinfo.token, &travt->objs[u].obj_token, &token_cmp) < 0)
                goto out;
            if (!token_cmp) {
                ret = travt->objs[u].name;
                goto out;
            }
        } /* end if */
    }     /* u */

out:
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    Update_Ref_value
 *
 * Purpose:     Update a reference value
 *-------------------------------------------------------------------------
 */
static herr_t
update_ref_value(hid_t obj_id, H5R_type_t ref_type, void *ref_in, hid_t fid_out, void *ref_out,
                 trav_table_t *travt)
{
    const char *ref_obj_name;
    hid_t       space_id   = H5I_INVALID_HID;
    hid_t       ref_obj_id = H5I_INVALID_HID;
    herr_t      ret_value  = SUCCEED;

    ref_obj_id = H5Rdereference2(obj_id, H5P_DEFAULT, ref_type, ref_in);
    if (ref_obj_id < 0)
        H5TOOLS_GOTO_ERROR(FAIL, "H5Rdereference2 failed");

    ref_obj_name = MapIdToName(ref_obj_id, travt);
    if (ref_obj_name == NULL)
        H5TOOLS_GOTO_ERROR(FAIL, "MapIdToName failed");

    if (ref_type == H5R_DATASET_REGION) {
        space_id = H5Rget_region(obj_id, H5R_DATASET_REGION, ref_in);
        if (space_id < 0)
            H5TOOLS_GOTO_ERROR(FAIL, "H5Rget_region failed");
    }

    if (H5Rcreate(ref_out, fid_out, ref_obj_name, ref_type, space_id) < 0)
        H5TOOLS_GOTO_ERROR(FAIL, "H5Rcreate failed");

done:
    H5E_BEGIN_TRY
    {
        H5Sclose(space_id);
        H5Oclose(ref_obj_id);
    }
    H5E_END_TRY

    return ret_value;
}
