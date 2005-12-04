/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer: 	Peter X. Cao
 *             	May 01, 2005
 *
 * Purpose:	Test H5Gcopy().
 */

#include <time.h>
#include "h5test.h"

const char *FILENAME[] = {
    "objcopy_src",
    "objcopy_dst",
    NULL
};

#define FILE_EXT 		"objcopy_ext.dat"

#define NAME_DATATYPE_SIMPLE 	"H5T_NATIVE_INT"
#define NAME_DATATYPE_SIMPLE2 	"H5T_NATIVE_INT-2"
#define NAME_DATATYPE_VL 	"vlen of int"
#define NAME_DATATYPE_VL_VL 	"vlen of vlen of int"
#define NAME_DATASET_SIMPLE 	"dataset_simple"
#define NAME_DATASET_COMPOUND 	"dataset_compound"
#define NAME_DATASET_CHUNKED 	"dataset_chunked"
#define NAME_DATASET_COMPACT 	"dataset_compact"
#define NAME_DATASET_EXTERNAL 	"dataset_ext"
#define NAME_DATASET_NAMED_DTYPE 	"dataset_named_dtype"
#define NAME_DATASET_NAMED_DTYPE2 	"dataset_named_dtype2"
#define NAME_DATASET_MULTI_OHDR 	"dataset_multi_ohdr"
#define NAME_DATASET_MULTI_OHDR2 	"dataset_multi_ohdr2"
#define NAME_DATASET_VL 	"dataset_vl"
#define NAME_DATASET_SUB_SUB 	"/g0/g00/g000/dataset_simple"
#define NAME_GROUP_UNCOPIED 	"/uncopied"
#define NAME_GROUP_EMPTY 	"/empty"
#define NAME_GROUP_TOP 		"/g0"
#define NAME_GROUP_SUB 		"/g0/g00"
#define NAME_GROUP_SUB_2	"/g0/g01"
#define NAME_GROUP_SUB_SUB 	"/g0/g00/g000"
#define NAME_GROUP_SUB_SUB2 	"g000"
#define NAME_GROUP_DATASET 	"/g0/dataset_simple"
#define NAME_GROUP_LINK		"/g_links"
#define NAME_GROUP_LOOP		"g_loop"
#define NAME_GROUP_LOOP2	"g_loop2"
#define NAME_GROUP_LOOP3	"g_loop3"
#define NAME_LINK_DATASET	"/g_links/dataset_simple"
#define NAME_LINK_HARD		"/g_links/hard_link_to_dataset_simple"
#define NAME_LINK_SOFT		"/g_links/soft_link_to_dataset_simple"
#define NAME_LINK_SOFT_DANGLE	"/g_links/soft_link_to_nowhere"

#define NAME_BUF_SIZE   1024
#define NUM_ATTRIBUTES 8
#define ATTR_NAME_LEN 40
#define DIM_SIZE_1 12
#define DIM_SIZE_2  6 
#define NUM_SUB_GROUPS  20 
#define NUM_WIDE_LOOP_GROUPS  10 
#define NUM_DATASETS  10 

/* Table containing object id and object name */
/* (Used for detecting duplicate objects when comparing groups */
static struct {
    size_t  nalloc;             /* number of slots allocated */
    size_t  nobjs;              /* number of objects */
    haddr_t *obj;               /* Addresses of objects seen */
} idtab_g;


/*-------------------------------------------------------------------------
 * Function: addr_insert
 *
 * Purpose: Add an address to the table.
 *
 * Return: void
 *
 * Programmer: Quincey Koziol
 *              Saturday, November  5, 2005
 *
 *-------------------------------------------------------------------------
 */
static void
addr_insert(H5G_stat_t *sb)
{
    size_t  n;

    /* Don't add it if the link count is 1 because such an object can only
     * be encountered once. */
    if(sb->u.obj.nlink < 2)
        return;

    /* Extend the table */
    if(idtab_g.nobjs >= idtab_g.nalloc) {
        idtab_g.nalloc = MAX(256, 2*idtab_g.nalloc);
        idtab_g.obj = HDrealloc(idtab_g.obj, idtab_g.nalloc * sizeof(idtab_g.obj[0]));
    } /* end if */

    /* Insert the entry */
    n = idtab_g.nobjs++;
    idtab_g.obj[n] = sb->u.obj.objno;
} /* end addr_insert() */


/*-------------------------------------------------------------------------
 * Function: addr_lookup
 *
 * Purpose: Check if address has already been encountered
 *
 * Return: Success: TRUE/FALSE
 *
 * Failure: (can't fail)
 *
 * Programmer: Quincey Koziol
 *              Saturday, November  5, 2005
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
addr_lookup(H5G_stat_t *sb)
{
    size_t  n;

    if (sb->u.obj.nlink<2) return FALSE; /*only one link possible*/

    for(n = 0; n < idtab_g.nobjs; n++)
        if(idtab_g.obj[n] == sb->u.obj.objno)
            return TRUE;
    return FALSE;
} /* end addr_lookup() */


/*-------------------------------------------------------------------------
 * Function: addr_reset
 *
 * Purpose: Reset the address tracking data structures
 *
 * Return: void
 *
 * Programmer: Quincey Koziol
 *              Saturday, November  5, 2005
 *
 *-------------------------------------------------------------------------
 */
static void
addr_reset(void)
{
    if(idtab_g.obj)
        HDfree(idtab_g.obj);
    idtab_g.obj = NULL;
    idtab_g.nalloc = idtab_g.nobjs = 0;
} /* end addr_reset() */


/*-------------------------------------------------------------------------
 * Function:    test_copy_attach_attributes
 *
 * Purpose:     Attach NUM_ATTRIBUTES attributes to the object to be copied
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Peter Cao 
 *              Friday, September 30, 2005 
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_attach_attributes(hid_t loc_id, hid_t type_id)
{
    hid_t aid = -1, sid = -1;
    char attr_name[ATTR_NAME_LEN];
    int  attr_data[2], i=0;
    hsize_t dim1=2;
    int ret_value = -1;

    if ( (sid = H5Screate_simple(1, &dim1, NULL)) < 0 )
        goto done;

    for (i=0; i<NUM_ATTRIBUTES; i++) {
        sprintf(attr_name, "%d attr", i);

        /* Set attribute data */
        attr_data[0] = 100 * i;
        attr_data[1] = 200 * i;

        if ( (aid = H5Acreate(loc_id, attr_name, type_id, sid, H5P_DEFAULT)) < 0)
            goto done;

        if ( H5Awrite(aid, H5T_NATIVE_INT, attr_data) < 0)
            goto done;

        if (aid > 0) 
            H5Aclose(aid); 

         aid = -1;
    }

    ret_value = 0;

done:
    if (sid > 0) 
        H5Sclose(sid);
    if (aid > 0) 
        H5Aclose(aid);

    return ret_value;
}


/*-------------------------------------------------------------------------
 * Function:    test_copy_attach_paired_attributes
 *
 * Purpose:     Attach NUM_ATTRIBUTES attributes to a pair of objects to be copied
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November 1, 2005 
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_attach_paired_attributes(hid_t loc_id, hid_t loc_id2, hid_t type_id)
{
    hid_t aid = -1, sid = -1;
    char attr_name[ATTR_NAME_LEN];
    int  attr_data[2], i;
    hsize_t dim1 = 2;

    if ( (sid = H5Screate_simple(1, &dim1, NULL)) < 0 ) goto done;

    for (i=0; i<NUM_ATTRIBUTES; i++) {
        sprintf(attr_name, "%d attr", i);

        /* Set attribute data */
        attr_data[0] = 100 * i;
        attr_data[1] = 200 * i;

        /* Add attribute to first object */
        if ( (aid = H5Acreate(loc_id, attr_name, type_id, sid, H5P_DEFAULT)) < 0) goto done;
        if ( H5Awrite(aid, H5T_NATIVE_INT, attr_data) < 0) goto done;
        if ( H5Aclose(aid) < 0) goto done;

        /* Add attribute to second object */
        if ( (aid = H5Acreate(loc_id2, attr_name, type_id, sid, H5P_DEFAULT)) < 0) goto done;
        if ( H5Awrite(aid, H5T_NATIVE_INT, attr_data) < 0) goto done;
        if ( H5Aclose(aid) < 0) goto done;
    }

    if ( H5Sclose(sid) < 0) goto done;

    return 0;

done:
    if (sid > 0) 
        H5Sclose(sid);
    if (aid > 0) 
        H5Aclose(aid);

    return -1;
} /* end test_copy_attach_paired_attributes() */


/*-------------------------------------------------------------------------
 * Function:    compare_attributes
 *
 * Purpose:     Compare attributes on two objects to check that they are equal
 *
 * Return:	TRUE if objects have same attributes/FALSE if they are different
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 31, 2005 
 *
 * Note:	This isn't very general, the attributes are assumed to be
 *              those written in test_copy_attach_attributes().
 *
 *-------------------------------------------------------------------------
 */
static int
compare_attributes(hid_t oid, hid_t oid2)
{
    hid_t sid = -1, sid2 = -1;                  /* Dataspace IDs */
    hid_t tid = -1, tid2 = -1;                  /* Datatype IDs */
    hid_t aid = -1, aid2 = -1;                  /* Attribute IDs */
    int num_attrs;                              /* Number of attributes */
    int num_attrs2;                             /* Number of attributes */
    char attr_name[ATTR_NAME_LEN];              /* Attribute name */
    int wattr_data[2];                          /* Attribute buffer for writing */
    int attr_data[2], attr_data2[2];            /* Attribute buffers for reading */
    htri_t is_committed;                        /* If the datatype is committed */
    htri_t is_committed2;                       /* If the datatype is committed */
    unsigned i;                                 /* Local index variable */

    /* Check the number of attributes on source dataset */
    if ( (num_attrs = H5Aget_num_attrs(oid)) < 0) TEST_ERROR;

    /* Check the number of attributes on destination dataset */
    if ( (num_attrs2 = H5Aget_num_attrs(oid2)) < 0) TEST_ERROR;

    /* Compare the number of attributes */
    if ( num_attrs != num_attrs2) TEST_ERROR;

    /* Check the attributes are equal */
    for(i = 0; i < (unsigned)num_attrs; i++) {
        sprintf(attr_name, "%d attr", i);

        /* Set up attribute data buffers */
        wattr_data[0] = 100 * i;
        wattr_data[1] = 200 * i;

        /* Open the attributes */
        if ( (aid = H5Aopen_name(oid, attr_name)) < 0) TEST_ERROR
        if ( (aid2 = H5Aopen_name(oid2, attr_name)) < 0) TEST_ERROR

        /* Check the datatypes are equal */

        /* Open the datatype for the source attribute */
        if ( (tid = H5Aget_type(aid)) < 0) TEST_ERROR;

        /* Open the datatype for the destination attribute */
        if ( (tid2 = H5Aget_type(aid2)) < 0) TEST_ERROR;

        /* Check that both datatypes are committed/not committed */
        if ( (is_committed = H5Tcommitted(tid)) < 0) TEST_ERROR;
        if ( (is_committed2 = H5Tcommitted(tid2)) < 0) TEST_ERROR;
        if ( is_committed != is_committed2) TEST_ERROR;

        /* Compare the datatypes */
        if ( H5Tequal(tid, tid2) != TRUE) TEST_ERROR;

        /* close the source datatype */
        if ( H5Tclose(tid) < 0) TEST_ERROR;

        /* close the destination datatype */
        if ( H5Tclose(tid2) < 0) TEST_ERROR;


        /* Check the dataspaces are equal */

        /* Open the dataspace for the source attribute */
        if ( (sid = H5Aget_space(aid)) < 0) TEST_ERROR;

        /* Open the dataspace for the destination attribute */
        if ( (sid2 = H5Aget_space(aid2)) < 0) TEST_ERROR;

        /* Compare the dataspaces */
        if ( H5Sextent_equal(sid, sid2) != TRUE) TEST_ERROR;

        /* close the source dataspace */
        if ( H5Sclose(sid) < 0) TEST_ERROR;

        /* close the destination dataspace */
        if ( H5Sclose(sid2) < 0) TEST_ERROR;


        /* Check the attribute data is equal */

        /* Read the attribute data */
        if ( (H5Aread(aid, H5T_NATIVE_INT, attr_data)) < 0) TEST_ERROR
        if ( (H5Aread(aid2, H5T_NATIVE_INT, attr_data2)) < 0) TEST_ERROR

        /* Check attribute data read in against data written out */
        if(wattr_data[0] != attr_data[0] || wattr_data[0] != attr_data2[0]) TEST_ERROR
        if(wattr_data[1] != attr_data[1] || wattr_data[1] != attr_data2[1]) TEST_ERROR

        /* Close the attributes */
        if ( H5Aclose(aid) < 0) TEST_ERROR
        if ( H5Aclose(aid2) < 0) TEST_ERROR
    } /* end for */

    /* Objects should be the same. :-) */
    return TRUE;

error:
    H5E_BEGIN_TRY {
    	H5Aclose(aid2);
    	H5Aclose(aid);
    	H5Sclose(sid2);
    	H5Sclose(sid);
    	H5Tclose(tid2);
    	H5Tclose(tid);
    } H5E_END_TRY;
    return FALSE;
} /* end compare_attributes() */


/*-------------------------------------------------------------------------
 * Function:    compare_data
 *
 * Purpose:     Compare two buffers of data to check that they are equal
 *
 * Return:	TRUE if buffer are equal/FALSE if they are different
 *
 * Programmer:  Quincey Koziol
 *              Monday, November 21, 2005 
 *
 *-------------------------------------------------------------------------
 */
static int
compare_data(hid_t tid, size_t elmt_size, size_t nelmts, void *buf1, void *buf2)
{
    /* Check for references, which aren't handled */
    if(H5Tdetect_class(tid, H5T_REFERENCE) == TRUE) TEST_ERROR

    /* Check for vlen datatype */
    if(H5Tdetect_class(tid, H5T_VLEN) == TRUE) {
        hvl_t *vl_buf1, *vl_buf2;       /* Aliases for buffers to compare */
        hid_t base_tid;                 /* Base type of vlen datatype */
        size_t base_size;               /* Size of base type */
        size_t u;                       /* Local index variable */

        /* Check for "simple" vlen datatype */
        if(H5Tget_class(tid) != H5T_VLEN) TEST_ERROR;

        /* Get base type of vlen datatype */
        if ( (base_tid = H5Tget_super(tid)) < 0) TEST_ERROR
        if ( (base_size = H5Tget_size(base_tid)) == 0) TEST_ERROR

        /* Loop over elements in buffers */
        vl_buf1 = buf1;
        vl_buf2 = buf2;
        for(u = 0; u < nelmts; u++, vl_buf1++, vl_buf2++) {
            /* Check vlen lengths */
            if(vl_buf1->len != vl_buf2->len) TEST_ERROR

            /* Check vlen data */
            if(!compare_data(base_tid, base_size, vl_buf1->len, vl_buf1->p, vl_buf2->p)) TEST_ERROR
        } /* end for */

        if(H5Tclose(base_tid) < 0) TEST_ERROR
    } /* end if */
    else
        if ( HDmemcmp(buf1, buf2, (size_t)(elmt_size * nelmts))) TEST_ERROR

    /* Data should be the same. :-) */
    return TRUE;

error:
    return FALSE;
} /* end compare_data() */


/*-------------------------------------------------------------------------
 * Function:    compare_datasets
 *
 * Purpose:     Compare two datasets to check that they are equal
 *
 * Return:	TRUE if datasets are equal/FALSE if they are different
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, October 25, 2005 
 *
 *-------------------------------------------------------------------------
 */
static int
compare_datasets(hid_t did, hid_t did2, void *wbuf)
{
    hid_t sid = -1, sid2 = -1;                  /* Dataspace IDs */
    hid_t tid = -1, tid2 = -1;                  /* Datatype IDs */
    hid_t dcpl = -1, dcpl2 = -1;                /* Dataset creation property list IDs */
    size_t elmt_size;                           /* Size of datatype */
    htri_t is_committed;                        /* If the datatype is committed */
    htri_t is_committed2;                       /* If the datatype is committed */
    hssize_t nelmts;                            /* # of elements in dataspace */
    void *rbuf = NULL;                          /* Buffer for reading raw data */
    void *rbuf2 = NULL;                         /* Buffer for reading raw data */
    H5D_space_status_t space_status;            /* Dataset's raw data space status */
    H5D_space_status_t space_status2;           /* Dataset's raw data space status */
    hsize_t storage_size;                       /* Dataset's raw data storage size */
    hsize_t storage_size2;                      /* Dataset's raw data storage size */

    /* Check the datatypes are equal */

    /* Open the datatype for the source dataset */
    if ( (tid = H5Dget_type(did)) < 0) TEST_ERROR;

    /* Open the datatype for the destination dataset */
    if ( (tid2 = H5Dget_type(did2)) < 0) TEST_ERROR;

    /* Check that both datatypes are committed/not committed */
    if ( (is_committed = H5Tcommitted(tid)) < 0) TEST_ERROR;
    if ( (is_committed2 = H5Tcommitted(tid2)) < 0) TEST_ERROR;
    if ( is_committed != is_committed2) TEST_ERROR;

    /* Compare the datatypes */
    if ( H5Tequal(tid, tid2) != TRUE) TEST_ERROR;

    /* Determine the size of datatype (for later) */
    if ( (elmt_size = H5Tget_size(tid)) == 0) TEST_ERROR


    /* Check the dataspaces are equal */

    /* Open the dataspace for the source dataset */
    if ( (sid = H5Dget_space(did)) < 0) TEST_ERROR;

    /* Open the dataspace for the destination dataset */
    if ( (sid2 = H5Dget_space(did2)) < 0) TEST_ERROR;

    /* Compare the dataspaces */
    if ( H5Sextent_equal(sid, sid2) != TRUE) TEST_ERROR;

    /* Determine the number of elements in dataspace (for later) */
    if ( (nelmts = H5Sget_simple_extent_npoints(sid)) < 0) TEST_ERROR


    /* Check the dataset creation property lists are equal */

    /* Open the dataset creation property list for the source dataset */
    if ( (dcpl = H5Dget_create_plist(did)) < 0) TEST_ERROR;

    /* Open the dataset creation property list for the destination dataset */
    if ( (dcpl2 = H5Dget_create_plist(did2)) < 0) TEST_ERROR;

    /* Compare the dataset creation property lists */
    if ( H5Pequal(dcpl, dcpl2) != TRUE) TEST_ERROR;

    /* close the source dataset creation property list */
    if ( H5Pclose(dcpl) < 0) TEST_ERROR;

    /* close the destination dataset creation property list */
    if ( H5Pclose(dcpl2) < 0) TEST_ERROR;


    /* Check the allocated storage is the same */

    /* Check that the space allocation status is the same */
    if(H5Dget_space_status(did, &space_status) < 0) TEST_ERROR;
    if(H5Dget_space_status(did2, &space_status2) < 0) TEST_ERROR;
    if(space_status != space_status2) TEST_ERROR;

    /* Check that the space used is the same */
    storage_size = H5Dget_storage_size(did);
    storage_size2 = H5Dget_storage_size(did2);
    if(storage_size != storage_size2) TEST_ERROR;


    /* Check the raw data is equal */

    /* Allocate & initialize space for the raw data buffers */
    if ( (rbuf = HDcalloc( elmt_size, (size_t)nelmts)) == NULL) TEST_ERROR;
    if ( (rbuf2 = HDcalloc( elmt_size, (size_t)nelmts)) == NULL) TEST_ERROR;

    /* Read data from datasets */
    if ( H5Dread(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0) TEST_ERROR;
    if ( H5Dread(did2, tid2, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf2) < 0) TEST_ERROR;

    /* Check raw data read in against data written out */
    if(wbuf) {
        if ( !compare_data(tid, elmt_size, (size_t)nelmts, wbuf, rbuf)) TEST_ERROR
        if ( !compare_data(tid2, elmt_size, (size_t)nelmts, wbuf, rbuf2)) TEST_ERROR
    } /* end if */
    /* Don't have written data, just compare data between the two datasets */
    else
        if ( !compare_data(tid, elmt_size, (size_t)nelmts, rbuf, rbuf2)) TEST_ERROR

    /* Reclaim vlen data, if necessary */
    if(H5Tdetect_class(tid, H5T_VLEN) == TRUE)
        if(H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, rbuf) < 0) TEST_ERROR
    if(H5Tdetect_class(tid2, H5T_VLEN) == TRUE)
        if(H5Dvlen_reclaim(tid2, sid2, H5P_DEFAULT, rbuf2) < 0) TEST_ERROR

    /* Release raw data buffers */
    HDfree(rbuf);
    HDfree(rbuf2);

    /* close the source dataspace */
    if ( H5Sclose(sid) < 0) TEST_ERROR;

    /* close the destination dataspace */
    if ( H5Sclose(sid2) < 0) TEST_ERROR;

    /* close the source datatype */
    if ( H5Tclose(tid) < 0) TEST_ERROR;

    /* close the destination datatype */
    if ( H5Tclose(tid2) < 0) TEST_ERROR;


    /* Check if the attributes are equal */
    if ( compare_attributes(did, did2) != TRUE) TEST_ERROR;


    /* Datasets should be the same. :-) */
    return TRUE;

error:
    H5E_BEGIN_TRY {
        if(rbuf)
            HDfree(rbuf);
        if(rbuf2)
            HDfree(rbuf2);
    	H5Pclose(dcpl2);
    	H5Pclose(dcpl);
    	H5Sclose(sid2);
    	H5Sclose(sid);
    	H5Tclose(tid2);
    	H5Tclose(tid);
    } H5E_END_TRY;
    return FALSE;
} /* end compare_datasets() */


/*-------------------------------------------------------------------------
 * Function:    compare_groups
 *
 * Purpose:     Compare two groups to check that they are "equal"
 *
 * Return:	TRUE if group are equal/FALSE if they are different
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 31, 2005 
 *
 *-------------------------------------------------------------------------
 */
static int
compare_groups(hid_t gid, hid_t gid2)
{
    hsize_t num_objs;           /* Number of objects in group */
    hsize_t num_objs2;          /* Number of objects in group */
    hsize_t idx;                /* Index over the objects in group */

    /* Check if both groups have the same # of objects */
    if(H5Gget_num_objs(gid, &num_objs) < 0) TEST_ERROR;
    if(H5Gget_num_objs(gid2, &num_objs2) < 0) TEST_ERROR;
    if(num_objs != num_objs2) TEST_ERROR;

    /* Check contents of groups */
    if(num_objs > 0) {
        char objname[NAME_BUF_SIZE];            /* Name of object in group */
        char objname2[NAME_BUF_SIZE];           /* Name of object in group */
        H5G_obj_t objtype;                      /* Type of object in group */
        H5G_obj_t objtype2;                     /* Type of object in group */
        H5G_stat_t objstat;                     /* Object info */
        H5G_stat_t objstat2;                    /* Object info */
        hid_t oid, oid2;                        /* IDs of objects within group */

        /* Loop over contents of groups */
        for(idx = 0; idx < num_objs; idx++) {
            /* Check name of objects */
            if(H5Gget_objname_by_idx(gid, idx, objname, NAME_BUF_SIZE) < 0) TEST_ERROR;
            if(H5Gget_objname_by_idx(gid2, idx, objname2, NAME_BUF_SIZE) < 0) TEST_ERROR;
            if(HDstrcmp(objname, objname2)) TEST_ERROR;

            /* Check type of objects */
            if((objtype = H5Gget_objtype_by_idx(gid, idx)) < 0) TEST_ERROR;
            if((objtype2 = H5Gget_objtype_by_idx(gid2, idx)) < 0) TEST_ERROR;
            if(objtype != objtype2) TEST_ERROR;

            /* Compare some pieces of the H5G_stat_t */
            if(H5Gget_objinfo(gid, objname, FALSE, &objstat) < 0) TEST_ERROR;
            if(H5Gget_objinfo(gid2, objname2, FALSE, &objstat2) < 0) TEST_ERROR;
            if(objstat.type != objstat2.type) TEST_ERROR;
            if(objstat.type != H5G_LINK) {
                if(objstat.u.obj.nlink != objstat2.u.obj.nlink) TEST_ERROR;
                if(objstat.u.obj.ohdr.nmesgs != objstat2.u.obj.ohdr.nmesgs) TEST_ERROR;
                if(objstat.u.obj.ohdr.nchunks != objstat2.u.obj.ohdr.nchunks) TEST_ERROR;
            } /* end if */

            /* Check for object already having been compared */
            if(addr_lookup(&objstat))
                continue;
            else
                addr_insert(&objstat);

            /* Compare objects within group */
            switch(objtype) {
                case H5G_LINK:
                    {
                        char linkname[NAME_BUF_SIZE];            /* Link value */
                        char linkname2[NAME_BUF_SIZE];           /* Link value */

                        /* Check link values */
                        if(H5Gget_linkval(gid, objname, NAME_BUF_SIZE, linkname) < 0) TEST_ERROR;
                        if(H5Gget_linkval(gid2, objname2, NAME_BUF_SIZE, linkname2) < 0) TEST_ERROR;
                        if(HDstrcmp(linkname, linkname2)) TEST_ERROR;
                    }
                    break;

                case H5G_GROUP:
                    /* Open groups */
                    if((oid = H5Gopen(gid, objname)) < 0) TEST_ERROR;
                    if((oid2 = H5Gopen(gid2, objname2)) < 0) TEST_ERROR;

                    /* Compare groups */
                    if(compare_groups(oid, oid2) != TRUE) TEST_ERROR;

                    /* Close groups */
                    if(H5Gclose(oid) < 0) TEST_ERROR;
                    if(H5Gclose(oid2) < 0) TEST_ERROR;
                    break;

                case H5G_DATASET:
                    /* Open datasets */
                    if((oid = H5Dopen(gid, objname)) < 0) TEST_ERROR;
                    if((oid2 = H5Dopen(gid2, objname2)) < 0) TEST_ERROR;

                    /* Compare datasets */
                    if(compare_datasets(oid, oid2, NULL) != TRUE) TEST_ERROR;

                    /* Close datasets */
                    if(H5Dclose(oid) < 0) TEST_ERROR;
                    if(H5Dclose(oid2) < 0) TEST_ERROR;
                    break;

                case H5G_TYPE:
                    /* Open datatypes */
                    if((oid = H5Topen(gid, objname)) < 0) TEST_ERROR;
                    if((oid2 = H5Topen(gid2, objname2)) < 0) TEST_ERROR;

                    /* Compare datatypes */
                    if(H5Tequal(oid, oid2) != TRUE) TEST_ERROR;

                    /* Close datatypes */
                    if(H5Tclose(oid) < 0) TEST_ERROR;
                    if(H5Tclose(oid2) < 0) TEST_ERROR;
                    break;

                default:
HDassert(0 && "Unknown type of object");
                    break;
            } /* end switch */
        } /* end for */
    } /* end if */

    /* Check if the attributes are equal */
    if ( compare_attributes(gid, gid2) != TRUE) TEST_ERROR;

    /* Groups should be the same. :-) */
    return TRUE;

error:
    H5E_BEGIN_TRY {
    } H5E_END_TRY;
    return FALSE;
} /* end compare_groups() */


/*-------------------------------------------------------------------------
 * Function:    test_copy_named_datatype
 *
 * Purpose:     Create name datatype in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao 
 *              Friday, September 30, 2005 
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_named_datatype(hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;   /* File IDs */
    hid_t tid = -1, tid2 = -1;          /* Datatype IDs */
    char		src_filename[NAME_BUF_SIZE];
    char		dst_filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): named datatype");

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* create datatype */
    if ( (tid = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR;

    /* create named datatype */
    if ( (H5Tcommit(fid_src, NAME_DATATYPE_SIMPLE, tid)) < 0) TEST_ERROR;

    /* close the datatype */
    if ( H5Tclose(tid) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the datatype for copy */ 
    if ( (tid = H5Topen(fid_src, NAME_DATATYPE_SIMPLE)) < 0) TEST_ERROR;

    /* copy the datatype from SRC to DST */
    if ( H5Gcopy(tid, fid_dst, NAME_DATATYPE_SIMPLE, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the copied datatype */ 
    if ( (tid2 = H5Topen(fid_dst, NAME_DATATYPE_SIMPLE)) < 0) TEST_ERROR;

    /* Compare the datatypes */
    if ( H5Tequal(tid, tid2) != TRUE) TEST_ERROR;

    /* close the destination datatype */
    if ( H5Tclose(tid2) < 0) TEST_ERROR;

    /* close the source datatype */
    if ( H5Tclose(tid) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Tclose(tid2);
    	H5Tclose(tid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_named_datatype */


/*-------------------------------------------------------------------------
 * Function:    test_copy_named_datatype_vl
 *
 * Purpose:     Create name vlen datatype in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November 22, 2005 
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_named_datatype_vl(hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;   /* File IDs */
    hid_t tid = -1, tid2 = -1;          /* Datatype IDs */
    char		src_filename[NAME_BUF_SIZE];
    char		dst_filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): named vlen datatype");

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* create datatype */
    if ( (tid = H5Tvlen_create(H5T_NATIVE_INT)) < 0) TEST_ERROR;

    /* create named datatype */
    if ( (H5Tcommit(fid_src, NAME_DATATYPE_VL, tid)) < 0) TEST_ERROR;

    /* close the datatype */
    if ( H5Tclose(tid) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the datatype for copy */ 
    if ( (tid = H5Topen(fid_src, NAME_DATATYPE_VL)) < 0) TEST_ERROR;

    /* copy the datatype from SRC to DST */
    if ( H5Gcopy(tid, fid_dst, NAME_DATATYPE_VL, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the copied datatype */ 
    if ( (tid2 = H5Topen(fid_dst, NAME_DATATYPE_VL)) < 0) TEST_ERROR;

    /* Compare the datatypes */
    if ( H5Tequal(tid, tid2) != TRUE) TEST_ERROR;

    /* close the destination datatype */
    if ( H5Tclose(tid2) < 0) TEST_ERROR;

    /* close the source datatype */
    if ( H5Tclose(tid) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Tclose(tid2);
    	H5Tclose(tid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_named_datatype_vl */


/*-------------------------------------------------------------------------
 * Function:    test_copy_named_datatype_vl_vl
 *
 * Purpose:     Create named vlen of vlen datatype in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November 22, 2005 
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_named_datatype_vl_vl(hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;   /* File IDs */
    hid_t tid = -1, tid2 = -1;          /* Datatype IDs */
    char		src_filename[NAME_BUF_SIZE];
    char		dst_filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): named nested vlen datatype");

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* create first vlen datatype */
    if ( (tid = H5Tvlen_create(H5T_NATIVE_INT)) < 0) TEST_ERROR;

    /* create second (nested) vlen datatype */
    if ( (tid2 = H5Tvlen_create(tid)) < 0) TEST_ERROR;

    /* create named datatype */
    if ( (H5Tcommit(fid_src, NAME_DATATYPE_VL_VL, tid2)) < 0) TEST_ERROR;

    /* close the first datatype */
    if ( H5Tclose(tid) < 0) TEST_ERROR;

    /* close the second datatype */
    if ( H5Tclose(tid2) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the datatype for copy */ 
    if ( (tid = H5Topen(fid_src, NAME_DATATYPE_VL_VL)) < 0) TEST_ERROR;

    /* copy the datatype from SRC to DST */
    if ( H5Gcopy(tid, fid_dst, NAME_DATATYPE_VL_VL, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the copied datatype */ 
    if ( (tid2 = H5Topen(fid_dst, NAME_DATATYPE_VL_VL)) < 0) TEST_ERROR;

    /* Compare the datatypes */
    if ( H5Tequal(tid, tid2) != TRUE) TEST_ERROR;

    /* close the destination datatype */
    if ( H5Tclose(tid2) < 0) TEST_ERROR;

    /* close the source datatype */
    if ( H5Tclose(tid) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Tclose(tid2);
    	H5Tclose(tid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_named_datatype_vl */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_simple
 *
 * Purpose:     Create a simple dataset in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao 
 *              Friday, September 30, 2005 
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_simple(hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    int buf[DIM_SIZE_1][DIM_SIZE_2];            /* Buffer for writing data */
    hsize_t dim2d[2];                           /* Dataset dimensions */
    int i, j;                                   /* local index variables */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): simple dataset");

    /* Initialize write buffer */
    for (i=0; i<DIM_SIZE_1; i++)
        for (j=0; j<DIM_SIZE_2; j++)
            buf[i][j] = 10000 + 100*i+j;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* Set dataspace dimensions */
    dim2d[0] = DIM_SIZE_1;
    dim2d[1] = DIM_SIZE_2;

    /* create 2D dataspace */
    if ( (sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR;
  
    /* create 2D int dataset at SRC file */
    if ( (did = H5Dcreate(fid_src, NAME_DATASET_SIMPLE, H5T_NATIVE_INT, sid, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* write data into file */
    if ( H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR;

    /* close dataspace */
    if ( H5Sclose(sid) < 0) TEST_ERROR;

    /* attach attributes to the dataset */
    if ( test_copy_attach_attributes(did, H5T_NATIVE_INT) < 0) TEST_ERROR;

    /* close the dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the dataset for copy */ 
    if ( (did = H5Dopen(fid_src, NAME_DATASET_SIMPLE)) < 0) TEST_ERROR;

    /* copy the dataset from SRC to DST */
    if ( H5Gcopy(did, fid_dst, NAME_DATASET_SIMPLE, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the destination dataset */ 
    if ( (did2 = H5Dopen(fid_dst, NAME_DATASET_SIMPLE)) < 0) TEST_ERROR;

    /* Check if the datasets are equal */
    if ( compare_datasets(did, did2, buf) != TRUE) TEST_ERROR;

    /* close the destination dataset */
    if ( H5Dclose(did2) < 0) TEST_ERROR;

    /* close the source dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_simple */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_simple_empty
 *
 * Purpose:     Create a simple dataset in SRC file and copy it to DST file
 *              (Note: dataset has no data)
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 31, 2005 
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_simple_empty(hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    hsize_t dim2d[2];                           /* Dataset dimensions */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): empty contiguous dataset");

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* Set dataspace dimensions */
    dim2d[0] = DIM_SIZE_1;
    dim2d[1] = DIM_SIZE_2;

    /* create 2D dataspace */
    if ( (sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR;
  
    /* create 2D int dataset at SRC file */
    if ( (did = H5Dcreate(fid_src, NAME_DATASET_SIMPLE, H5T_NATIVE_INT, sid, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* close dataspace */
    if ( H5Sclose(sid) < 0) TEST_ERROR;

    /* attach attributes to the dataset */
    if ( test_copy_attach_attributes(did, H5T_NATIVE_INT) < 0) TEST_ERROR;

    /* close the dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the dataset for copy */ 
    if ( (did = H5Dopen(fid_src, NAME_DATASET_SIMPLE)) < 0) TEST_ERROR;

    /* copy the dataset from SRC to DST */
    if ( H5Gcopy(did, fid_dst, NAME_DATASET_SIMPLE, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the destination dataset */ 
    if ( (did2 = H5Dopen(fid_dst, NAME_DATASET_SIMPLE)) < 0) TEST_ERROR;

    /* Check if the datasets are equal */
    if ( compare_datasets(did, did2, NULL) != TRUE) TEST_ERROR;

    /* close the destination dataset */
    if ( H5Dclose(did2) < 0) TEST_ERROR;

    /* close the source dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_simple_empty */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_compound
 *
 * Purpose:     Create a compound dataset in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao 
 *              Friday, September 30, 2005 
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_compound(hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t tid = -1;                             /* Datatype ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    hsize_t dim1d[1];                           /* Dataset dimensions */
    typedef struct comp_t {
        int a;
        double d;
    } comp_t;
    comp_t buf[DIM_SIZE_1];                     /* Buffer for writing data */
    int i;                                      /* Local index variable */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): compound dataset");

    for (i=0; i<DIM_SIZE_1; i++) {
        buf[i].a = i;
        buf[i].d = 1./(i+1);
    }

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* Set dataspace dimensions */
    dim1d[0] = DIM_SIZE_1;

    /* create dataspace */
    if ( (sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR;

    /* create datatype */
    if ( (tid = H5Tcreate (H5T_COMPOUND, sizeof(comp_t))) < 0) TEST_ERROR;
    if (H5Tinsert(tid, "int_name", HOFFSET(comp_t, a), H5T_NATIVE_INT) < 0) TEST_ERROR;
    if (H5Tinsert(tid, "double_name", HOFFSET(comp_t, d), H5T_NATIVE_DOUBLE) < 0) TEST_ERROR;

    /* create dataset */
    if ( (did = H5Dcreate(fid_src, NAME_DATASET_COMPOUND, tid, sid, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* write data into file */
    if ( H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR;

    /* close dataspace */
    if ( H5Sclose(sid) < 0) TEST_ERROR;

    /* close datatype */
    if ( H5Tclose(tid) < 0) TEST_ERROR;

    /* attach attributes to the dataset */
    if ( test_copy_attach_attributes(did, H5T_NATIVE_INT) < 0) TEST_ERROR;

    /* close the dataset */
    if (H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the dataset for copy */ 
    if ( (did = H5Dopen(fid_src, NAME_DATASET_COMPOUND)) < 0) TEST_ERROR;

    /* copy the dataset from SRC to DST */
    if ( H5Gcopy(did, fid_dst, NAME_DATASET_COMPOUND, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the destination dataset */ 
    if ( (did2 = H5Dopen(fid_dst, NAME_DATASET_COMPOUND)) < 0) TEST_ERROR;

    /* Check if the datasets are equal */
    if ( compare_datasets(did, did2, buf) != TRUE) TEST_ERROR;

    /* close the destination dataset */
    if ( H5Dclose(did2) < 0) TEST_ERROR;

    /* close the source dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Tclose(tid);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_compound */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_chunked
 *
 * Purpose:     Create a chunked dataset in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao 
 *              Friday, September 30, 2005 
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_chunked(hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t pid = -1;                             /* Dataset creation property list ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    hsize_t dim2d[2];                           /* Dataset dimensions */
    hsize_t chunk_dim2d[2] ={2, 3};             /* Chunk dimensions */
    float buf[DIM_SIZE_1][DIM_SIZE_2];          /* Buffer for writing data */
    int i, j;                                   /* Local index variables */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): chunked dataset");

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        for (j=0; j<DIM_SIZE_2; j++)
            buf[i][j] = (float)(i+j/100.0);

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* Set dataspace dimensions */
    dim2d[0]=DIM_SIZE_1;
    dim2d[1]=DIM_SIZE_2;

    /* create dataspace */
    if ( (sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR;

    /* create and set chunk plist */
    if ( (pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR;
    if ( H5Pset_chunk(pid, 2, chunk_dim2d) < 0) TEST_ERROR;

    /* create dataset */
    if ( (did = H5Dcreate(fid_src, NAME_DATASET_CHUNKED, H5T_NATIVE_FLOAT, sid, pid)) < 0) TEST_ERROR;

    /* close chunk plist */
    if ( H5Pclose(pid) < 0) TEST_ERROR;

    /* write data into file */
    if ( H5Dwrite(did, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR;

    /* close dataspace */
    if ( H5Sclose(sid) < 0) TEST_ERROR;

    /* attach attributes to the dataset */
    if ( test_copy_attach_attributes(did, H5T_NATIVE_INT) < 0) TEST_ERROR;

    /* close the dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the dataset for copy */ 
    if ( (did = H5Dopen(fid_src, NAME_DATASET_CHUNKED)) < 0) TEST_ERROR;

    /* copy the dataset from SRC to DST */
    if ( H5Gcopy(did, fid_dst, NAME_DATASET_CHUNKED, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the destination dataset */ 
    if ( (did2 = H5Dopen(fid_dst, NAME_DATASET_CHUNKED)) < 0) TEST_ERROR;

    /* Check if the datasets are equal */
    if ( compare_datasets(did, did2, buf) != TRUE) TEST_ERROR;

    /* close the destination dataset */
    if ( H5Dclose(did2) < 0) TEST_ERROR;

    /* close the source dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Pclose(pid);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_chunked */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_chunked_empty
 *
 * Purpose:     Create a chunked dataset in SRC file and copy it to DST file
 *              (Note: dataset has no data)
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 31, 2005 
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_chunked_empty(hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t pid = -1;                             /* Dataset creation property list ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    hsize_t dim2d[2];                           /* Dataset dimensions */
    hsize_t chunk_dim2d[2] ={2, 3};             /* Chunk dimensions */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): empty chunked dataset");

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* Set dataspace dimensions */
    dim2d[0]=DIM_SIZE_1;
    dim2d[1]=DIM_SIZE_2;

    /* create dataspace */
    if ( (sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR;

    /* create and set chunk plist */
    if ( (pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR;
    if ( H5Pset_chunk(pid, 2, chunk_dim2d) < 0) TEST_ERROR;

    /* create dataset */
    if ( (did = H5Dcreate(fid_src, NAME_DATASET_CHUNKED, H5T_NATIVE_FLOAT, sid, pid)) < 0) TEST_ERROR;

    /* close chunk plist */
    if ( H5Pclose(pid) < 0) TEST_ERROR;

    /* close dataspace */
    if ( H5Sclose(sid) < 0) TEST_ERROR;

    /* attach attributes to the dataset */
    if ( test_copy_attach_attributes(did, H5T_NATIVE_INT) < 0) TEST_ERROR;

    /* close the dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the dataset for copy */ 
    if ( (did = H5Dopen(fid_src, NAME_DATASET_CHUNKED)) < 0) TEST_ERROR;

    /* copy the dataset from SRC to DST */
    if ( H5Gcopy(did, fid_dst, NAME_DATASET_CHUNKED, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the destination dataset */ 
    if ( (did2 = H5Dopen(fid_dst, NAME_DATASET_CHUNKED)) < 0) TEST_ERROR;

    /* Check if the datasets are equal */
    if ( compare_datasets(did, did2, NULL) != TRUE) TEST_ERROR;

    /* close the destination dataset */
    if ( H5Dclose(did2) < 0) TEST_ERROR;

    /* close the source dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Pclose(pid);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_chunked_empty */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_chunked_sparse
 *
 * Purpose:     Create a chunked dataset with unlimited dimensions and un-written
 *              chunks in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 31, 2005 
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_chunked_sparse(hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t pid = -1;                             /* Dataset creation property list ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    hsize_t dim2d[2];                           /* Dataset dimensions */
    hsize_t new_dim2d[2];                       /* Dataset dimensions */
    hsize_t max_dim2d[2];                       /* Dataset max. dimensions */
    hsize_t chunk_dim2d[2] ={2, 3};             /* Chunk dimensions */
    float buf[DIM_SIZE_1][DIM_SIZE_2];          /* Buffer for writing data */
    int i, j;                                   /* Local index variables */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): sparse dataset");

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        for (j=0; j<DIM_SIZE_2; j++)
            buf[i][j] = (float)(i+j/100.0);

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* Set dataspace dimensions */
    dim2d[0]=DIM_SIZE_1;
    dim2d[1]=DIM_SIZE_2;
    max_dim2d[0]=H5S_UNLIMITED;
    max_dim2d[1]=H5S_UNLIMITED;

    /* create dataspace */
    if ( (sid = H5Screate_simple(2, dim2d, max_dim2d)) < 0) TEST_ERROR;

    /* create and set chunk plist */
    if ( (pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR;
    if ( H5Pset_chunk(pid, 2, chunk_dim2d) < 0) TEST_ERROR;

    /* create dataset */
    if ( (did = H5Dcreate(fid_src, NAME_DATASET_CHUNKED, H5T_NATIVE_FLOAT, sid, pid)) < 0) TEST_ERROR;

    /* close chunk plist */
    if ( H5Pclose(pid) < 0) TEST_ERROR;

    /* write data into file */
    if ( H5Dwrite(did, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR;

    /* Set extended dataset dimensions */
    new_dim2d[0] = DIM_SIZE_1 * 2;
    new_dim2d[1] = DIM_SIZE_2 * 2;

    /* Extend dataset's dimensions */
    if ( H5Dextend(did, new_dim2d) < 0) TEST_ERROR;

    /* close dataspace */
    if ( H5Sclose(sid) < 0) TEST_ERROR;

    /* attach attributes to the dataset */
    if ( test_copy_attach_attributes(did, H5T_NATIVE_INT) < 0) TEST_ERROR;

    /* close the dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the dataset for copy */ 
    if ( (did = H5Dopen(fid_src, NAME_DATASET_CHUNKED)) < 0) TEST_ERROR;

    /* copy the dataset from SRC to DST */
    if ( H5Gcopy(did, fid_dst, NAME_DATASET_CHUNKED, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the destination dataset */ 
    if ( (did2 = H5Dopen(fid_dst, NAME_DATASET_CHUNKED)) < 0) TEST_ERROR;

    /* Check if the datasets are equal */
    if ( compare_datasets(did, did2, NULL) != TRUE) TEST_ERROR;

    /* close the destination dataset */
    if ( H5Dclose(did2) < 0) TEST_ERROR;

    /* close the source dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Pclose(pid);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_chunked_sparse */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_compressed
 *
 * Purpose:     Create a compressed, chunked dataset in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 31, 2005 
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_compressed(hid_t fapl)
{
#ifdef H5_HAVE_FILTER_DEFLATE
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t pid = -1;                             /* Dataset creation property list ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    hsize_t dim2d[2];                           /* Dataset dimensions */
    hsize_t chunk_dim2d[2] ={2, 3};             /* Chunk dimensions */
    float buf[DIM_SIZE_1][DIM_SIZE_2];          /* Buffer for writing data */
    int i, j;                                   /* Local index variables */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];
#endif /* H5_HAVE_FILTER_DEFLATE */

    TESTING("H5Gcopy(): compressed dataset");

#ifndef H5_HAVE_FILTER_DEFLATE
    SKIPPED();
    puts("    Deflation filter not available");
#else /* H5_HAVE_FILTER_DEFLATE */
    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        for (j=0; j<DIM_SIZE_2; j++)
            buf[i][j] = (float)(100.0);         /* Something easy to compress */

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* Set dataspace dimensions */
    dim2d[0]=DIM_SIZE_1;
    dim2d[1]=DIM_SIZE_2;

    /* create dataspace */
    if ( (sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR;

    /* create and set comp & chunk plist */
    if ( (pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR;
    if ( H5Pset_chunk(pid, 2, chunk_dim2d) < 0) TEST_ERROR;
    if ( H5Pset_deflate(pid, 9) < 0) TEST_ERROR;

    /* create dataset */
    if ( (did = H5Dcreate(fid_src, NAME_DATASET_CHUNKED, H5T_NATIVE_FLOAT, sid, pid)) < 0) TEST_ERROR;

    /* close chunk plist */
    if ( H5Pclose(pid) < 0) TEST_ERROR;

    /* write data into file */
    if ( H5Dwrite(did, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR;

    /* close dataspace */
    if ( H5Sclose(sid) < 0) TEST_ERROR;

    /* attach attributes to the dataset */
    if ( test_copy_attach_attributes(did, H5T_NATIVE_INT) < 0) TEST_ERROR;

    /* close the dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the dataset for copy */ 
    if ( (did = H5Dopen(fid_src, NAME_DATASET_CHUNKED)) < 0) TEST_ERROR;

    /* copy the dataset from SRC to DST */
    if ( H5Gcopy(did, fid_dst, NAME_DATASET_CHUNKED, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the destination dataset */ 
    if ( (did2 = H5Dopen(fid_dst, NAME_DATASET_CHUNKED)) < 0) TEST_ERROR;

    /* Check if the datasets are equal */
    if ( compare_datasets(did, did2, NULL) != TRUE) TEST_ERROR;

    /* close the destination dataset */
    if ( H5Dclose(did2) < 0) TEST_ERROR;

    /* close the source dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;

    PASSED();
#endif /* H5_HAVE_FILTER_DEFLATE */
    return 0;

#ifdef H5_HAVE_FILTER_DEFLATE
error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Pclose(pid);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
#endif /* H5_HAVE_FILTER_DEFLATE */
} /* end test_copy_dataset_compressed */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_compact
 *
 * Purpose:     Create a compact dataset in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 31, 2005 
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_compact(hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t pid = -1;                             /* Dataset creation property list ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    hsize_t dim2d[2];                           /* Dataset dimensions */
    float buf[DIM_SIZE_1][DIM_SIZE_2];          /* Buffer for writing data */
    int i, j;                                   /* Local index variables */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): compact dataset");

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        for (j=0; j<DIM_SIZE_2; j++)
            buf[i][j] = (float)(i+j/100.0);

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* Set dataspace dimensions */
    dim2d[0]=DIM_SIZE_1;
    dim2d[1]=DIM_SIZE_2;

    /* create dataspace */
    if ( (sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR;

    /* create and set compact plist */
    if ( (pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR;
    if ( H5Pset_layout(pid, H5D_COMPACT) < 0) TEST_ERROR;

    /* create dataset */
    if ( (did = H5Dcreate(fid_src, NAME_DATASET_COMPACT, H5T_NATIVE_FLOAT, sid, pid)) < 0) TEST_ERROR;

    /* close chunk plist */
    if ( H5Pclose(pid) < 0) TEST_ERROR;

    /* write data into file */
    if ( H5Dwrite(did, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR;

    /* close dataspace */
    if ( H5Sclose(sid) < 0) TEST_ERROR;

    /* attach attributes to the dataset */
    if ( test_copy_attach_attributes(did, H5T_NATIVE_INT) < 0) TEST_ERROR;

    /* close the dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the dataset for copy */ 
    if ( (did = H5Dopen(fid_src, NAME_DATASET_COMPACT)) < 0) TEST_ERROR;

    /* copy the dataset from SRC to DST */
    if ( H5Gcopy(did, fid_dst, NAME_DATASET_COMPACT, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the destination dataset */ 
    if ( (did2 = H5Dopen(fid_dst, NAME_DATASET_COMPACT)) < 0) TEST_ERROR;

    /* Check if the datasets are equal */
    if ( compare_datasets(did, did2, buf) != TRUE) TEST_ERROR;

    /* close the destination dataset */
    if ( H5Dclose(did2) < 0) TEST_ERROR;

    /* close the source dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Pclose(pid);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_compact */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_external
 *
 * Purpose:     Create an external dataset in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao 
 *              Friday, September 30, 2005 
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_external(hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t pid = -1;                             /* Dataset creation property list ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    int i;
    hsize_t size;
    hsize_t dim1d[1];
    int buf[DIM_SIZE_1];
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): external dataset");

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        buf[i] = i;

    /* create an empty external file */
    HDfclose(HDfopen (FILE_EXT, "w"));

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if ( (sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR;
  
    /* set dataset creation plist */
    size = DIM_SIZE_1 * sizeof (int);
    if ( (pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR;
    if ( H5Pset_external(pid, FILE_EXT, (off_t)0, size) < 0) TEST_ERROR;

    /* create dataset at SRC file */
    if ( (did = H5Dcreate(fid_src, NAME_DATASET_EXTERNAL, H5T_NATIVE_INT, sid, pid)) < 0) TEST_ERROR;

    /* write data into file */
    if ( H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR;

    /* close dataspace */
    if ( H5Sclose(sid) < 0) TEST_ERROR;

    /* close external plist */
    if ( H5Pclose(pid) < 0) TEST_ERROR;

    /* close the dataset */
    if (H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
/* Don't change the address in the destination file for this test, it causes the
 * external file list's heap to be at a different location and generates a false
 * negative for this test.  The test is _slightly_ weaker because of this, but
 * I can't see any easy way around it. -QAK
 */
#if 0
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;
#endif /* 0 */

    /* open the dataset for copy */ 
    if ( (did = H5Dopen(fid_src, NAME_DATASET_EXTERNAL)) < 0) TEST_ERROR;

    /* copy the dataset from SRC to DST */
    if ( H5Gcopy(did, fid_dst, NAME_DATASET_EXTERNAL, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the destination dataset */ 
    if ( (did2 = H5Dopen(fid_dst, NAME_DATASET_EXTERNAL)) < 0) TEST_ERROR;

    /* Check if the datasets are equal */
    if ( compare_datasets(did, did2, buf) != TRUE) TEST_ERROR;

    /* close the destination dataset */
    if ( H5Dclose(did2) < 0) TEST_ERROR;

    /* close the source dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Pclose(pid);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_external */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_named_dtype
 *
 * Purpose:     Create a dataset that uses a named datatype in SRC file and
 *              copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 31, 2005 
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_named_dtype(hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t tid = -1;                             /* Datatype ID */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    int i;
    hsize_t dim1d[1];
    int buf[DIM_SIZE_1];
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): dataset that uses named datatype");

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        buf[i] = i;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if ( (sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR;

    /* create named datatype */
    if ( (tid = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR;
    if ( (H5Tcommit(fid_src, NAME_DATATYPE_SIMPLE, tid)) < 0) TEST_ERROR;
  
    /* create dataset at SRC file */
    if ( (did = H5Dcreate(fid_src, NAME_DATASET_NAMED_DTYPE, tid, sid, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* close the datatype */
    if ( H5Tclose(tid) < 0) TEST_ERROR;

    /* write data into file */
    if ( H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR;

    /* close dataspace */
    if ( H5Sclose(sid) < 0) TEST_ERROR;

    /* close the dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the dataset for copy */ 
    if ( (did = H5Dopen(fid_src, NAME_DATASET_NAMED_DTYPE)) < 0) TEST_ERROR;

    /* copy the dataset from SRC to DST */
    if ( H5Gcopy(did, fid_dst, NAME_DATASET_NAMED_DTYPE, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the destination dataset */ 
    if ( (did2 = H5Dopen(fid_dst, NAME_DATASET_NAMED_DTYPE)) < 0) TEST_ERROR;

    /* Check if the datasets are equal */
    if ( compare_datasets(did, did2, buf) != TRUE) TEST_ERROR;

    /* close the destination dataset */
    if ( H5Dclose(did2) < 0) TEST_ERROR;

    /* close the source dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Tclose(tid);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_named_dtype */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_named_dtype_hier
 *
 * Purpose:     Create a hierarchy of datasets that use a named datatype in
 *              SRC file and copy hierarchy to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, October 31, 2005 
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_named_dtype_hier(hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t tid = -1;                             /* Datatype ID */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1;                             /* Dataset ID */
    hid_t gid = -1, gid2 = -1;                  /* Group IDs */
    int i;
    hsize_t dim1d[1];
    int buf[DIM_SIZE_1];
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): hier. of datasets using named datatype inside hier.");

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        buf[i] = i;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* Create group to place all objects in */
    if ( (gid = H5Gcreate(fid_src, NAME_GROUP_TOP, (size_t)0)) < 0) TEST_ERROR;

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if ( (sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR;

    /* create named datatype _inside_ hierarchy to copy */
    if ( (tid = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR;
    if ( (H5Tcommit(gid, NAME_DATATYPE_SIMPLE, tid)) < 0) TEST_ERROR;
  
    /* create first dataset at SRC file */
    if ( (did = H5Dcreate(gid, NAME_DATASET_NAMED_DTYPE, tid, sid, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* write data into file */
    if ( H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR;

    /* close the dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* create second dataset at SRC file */
    if ( (did = H5Dcreate(gid, NAME_DATASET_NAMED_DTYPE2, tid, sid, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* write data into file */
    if ( H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR;

    /* close the dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the datatype */
    if ( H5Tclose(tid) < 0) TEST_ERROR;

    /* close dataspace */
    if ( H5Sclose(sid) < 0) TEST_ERROR;

    /* close group */
    if ( H5Gclose(gid) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the group for copy */ 
    if ( (gid = H5Gopen(fid_src, NAME_GROUP_TOP)) < 0) TEST_ERROR;

    /* copy the dataset from SRC to DST */
    if ( H5Gcopy(gid, fid_dst, NAME_GROUP_TOP, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the destination group */ 
    if ( (gid2 = H5Gopen(fid_dst, NAME_GROUP_TOP)) < 0) TEST_ERROR;

    /* Check if the groups are equal */
    if ( compare_groups(gid, gid2) != TRUE) TEST_ERROR;

    /* close the destination group */
    if ( H5Gclose(gid2) < 0) TEST_ERROR;

    /* close the source group */
    if ( H5Gclose(gid) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did);
    	H5Tclose(tid);
    	H5Sclose(sid);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_named_dtype_hier */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_named_dtype_hier_outside
 *
 * Purpose:     Create a hierarchy of datasets that use a named datatype that
 *              is outside of hierarchy in SRC file and copy hierarchy to DST
 *              file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, October 31, 2005 
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_named_dtype_hier_outside(hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t tid = -1;                             /* Datatype ID */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1;                             /* Dataset ID */
    hid_t gid = -1, gid2 = -1;                  /* Group IDs */
    int i;
    hsize_t dim1d[1];
    int buf[DIM_SIZE_1];
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): hier. of datasets using named datatype outside hier.");

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        buf[i] = i;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* Create group to place all objects in */
    if ( (gid = H5Gcreate(fid_src, NAME_GROUP_TOP, (size_t)0)) < 0) TEST_ERROR;

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if ( (sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR;

    /* create named datatype _outside_ hierarchy to copy */
    if ( (tid = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR;
    if ( (H5Tcommit(fid_src, NAME_DATATYPE_SIMPLE, tid)) < 0) TEST_ERROR;
  
    /* create first dataset at SRC file */
    if ( (did = H5Dcreate(gid, NAME_DATASET_NAMED_DTYPE, tid, sid, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* write data into file */
    if ( H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR;

    /* close the dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* create second dataset at SRC file */
    if ( (did = H5Dcreate(gid, NAME_DATASET_NAMED_DTYPE2, tid, sid, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* write data into file */
    if ( H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR;

    /* close the dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the datatype */
    if ( H5Tclose(tid) < 0) TEST_ERROR;

    /* close dataspace */
    if ( H5Sclose(sid) < 0) TEST_ERROR;

    /* close group */
    if ( H5Gclose(gid) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the group for copy */ 
    if ( (gid = H5Gopen(fid_src, NAME_GROUP_TOP)) < 0) TEST_ERROR;

    /* copy the dataset from SRC to DST */
    if ( H5Gcopy(gid, fid_dst, NAME_GROUP_TOP, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the destination group */ 
    if ( (gid2 = H5Gopen(fid_dst, NAME_GROUP_TOP)) < 0) TEST_ERROR;

    /* Check if the groups are equal */
    if ( compare_groups(gid, gid2) != TRUE) TEST_ERROR;

    /* close the destination group */
    if ( H5Gclose(gid2) < 0) TEST_ERROR;

    /* close the source group */
    if ( H5Gclose(gid) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did);
    	H5Tclose(tid);
    	H5Sclose(sid);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_named_dtype_hier_outside */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_multi_ohdr_chunks
 *
 * Purpose:     Create a pair of datasets that add attributes in a way that
 *              creates lots of object header chunks in SRC file and copy 
 *              datasets to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, October 31, 2005 
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_multi_ohdr_chunks(hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    hid_t gid = -1, gid2 = -1;                  /* Group IDs */
    int i;
    hsize_t dim1d[1];
    int buf[DIM_SIZE_1];
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): datasets that have multiple ohdr chunks");

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        buf[i] = i;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* Create group to place all objects in */
    if ( (gid = H5Gcreate(fid_src, NAME_GROUP_TOP, (size_t)0)) < 0) TEST_ERROR;

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if ( (sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR;

    /* create first dataset at SRC file */
    if ( (did = H5Dcreate(gid, NAME_DATASET_MULTI_OHDR, H5T_NATIVE_INT, sid, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* write data into file */
    if ( H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR;

    /* create second dataset at SRC file */
    if ( (did2 = H5Dcreate(gid, NAME_DATASET_MULTI_OHDR2, H5T_NATIVE_INT, sid, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* write data into file */
    if ( H5Dwrite(did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR;

    /* Add attributes to datasets in a way that creates lots of chunks */
    if ( test_copy_attach_paired_attributes(did, did2, H5T_NATIVE_INT) < 0) TEST_ERROR;

    /* close the first dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the second dataset */
    if ( H5Dclose(did2) < 0) TEST_ERROR;

    /* close dataspace */
    if ( H5Sclose(sid) < 0) TEST_ERROR;

    /* close group */
    if ( H5Gclose(gid) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the group for copy */ 
    if ( (gid = H5Gopen(fid_src, NAME_GROUP_TOP)) < 0) TEST_ERROR;

    /* copy the dataset from SRC to DST */
    if ( H5Gcopy(gid, fid_dst, NAME_GROUP_TOP, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the destination group */ 
    if ( (gid2 = H5Gopen(fid_dst, NAME_GROUP_TOP)) < 0) TEST_ERROR;

    /* Check if the groups are equal */
    if ( compare_groups(gid, gid2) != TRUE) TEST_ERROR;

    /* close the destination group */
    if ( H5Gclose(gid2) < 0) TEST_ERROR;

    /* close the source group */
    if ( H5Gclose(gid) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Sclose(sid);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_multi_ohdr_chunks */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_attr_named_dtype
 *
 * Purpose:     Create a pair of datasets that add attributes that use
 *              named datatypes in SRC file and copy datasets to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, October 31, 2005 
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_attr_named_dtype(hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t tid = -1;                             /* Datatype ID */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    hid_t gid = -1, gid2 = -1;                  /* Group IDs */
    int i;
    hsize_t dim1d[1];
    int buf[DIM_SIZE_1];
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): objects with attributes using named datatypes");

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        buf[i] = i;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* Create group to place all objects in */
    if ( (gid = H5Gcreate(fid_src, NAME_GROUP_TOP, (size_t)0)) < 0) TEST_ERROR;

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if ( (sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR;

    /* create named datatype _outside_ hierarchy to copy */
    if ( (tid = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR;
    if ( (H5Tcommit(fid_src, NAME_DATATYPE_SIMPLE, tid)) < 0) TEST_ERROR;
  
    /* create first dataset at SRC file */
    if ( (did = H5Dcreate(gid, NAME_DATASET_MULTI_OHDR, tid, sid, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* write data into file */
    if ( H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR;

    /* create second dataset at SRC file */
    if ( (did2 = H5Dcreate(gid, NAME_DATASET_MULTI_OHDR2, tid, sid, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* write data into file */
    if ( H5Dwrite(did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR;

    /* Add attributes to datasets in a way that creates lots of chunks */
    if ( test_copy_attach_paired_attributes(did, did2, tid) < 0) TEST_ERROR;

    /* close the datatype */
    if ( H5Tclose(tid) < 0) TEST_ERROR;

    /* close the first dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the second dataset */
    if ( H5Dclose(did2) < 0) TEST_ERROR;

    /* close dataspace */
    if ( H5Sclose(sid) < 0) TEST_ERROR;

    /* close group */
    if ( H5Gclose(gid) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the group for copy */ 
    if ( (gid = H5Gopen(fid_src, NAME_GROUP_TOP)) < 0) TEST_ERROR;

    /* copy the dataset from SRC to DST */
    if ( H5Gcopy(gid, fid_dst, NAME_GROUP_TOP, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the destination group */ 
    if ( (gid2 = H5Gopen(fid_dst, NAME_GROUP_TOP)) < 0) TEST_ERROR;

    /* Check if the groups are equal */
    if ( compare_groups(gid, gid2) != TRUE) TEST_ERROR;

    /* close the destination group */
    if ( H5Gclose(gid2) < 0) TEST_ERROR;

    /* close the source group */
    if ( H5Gclose(gid) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Tclose(tid);
    	H5Sclose(sid);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_attr_named_dtype */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_contig_vl
 *
 * Purpose:     Create a contiguous dataset w/variable-length datatype in SRC
 *              file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao 
 *              Friday, September 30, 2005 
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_contig_vl(hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t tid = -1;                             /* Datatype ID */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    unsigned int i, j;                          /* Local index variables */
    hsize_t dim1d[1];                           /* Dataset dimensions */
    hvl_t buf[DIM_SIZE_1];                      /* Buffer for writing data */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): contiguous dataset with variable-length datatype");

    /* set initial data values */
    for(i = 0; i < DIM_SIZE_1; i++) {
        buf[i].len = i+1;
        buf[i].p = (int *)HDmalloc(buf[i].len * sizeof(int));
        for(j = 0; j < buf[i].len; j++)
            ((int *)buf[i].p)[j] = i*10+j;
    } /* end for */

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if ( (sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR;

    /* create datatype */
    if ( (tid = H5Tvlen_create(H5T_NATIVE_INT)) < 0) TEST_ERROR;
  
    /* create dataset at SRC file */
    if ( (did = H5Dcreate(fid_src, NAME_DATASET_VL, tid, sid, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* write data into file */
    if ( H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR;

    /* close the dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the dataset for copy */ 
    if ( (did = H5Dopen(fid_src, NAME_DATASET_VL)) < 0) TEST_ERROR;

    /* copy the dataset from SRC to DST */
    if ( H5Gcopy(did, fid_dst, NAME_DATASET_VL, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the destination dataset */ 
    if ( (did2 = H5Dopen(fid_dst, NAME_DATASET_VL)) < 0) TEST_ERROR;

    /* Check if the datasets are equal */
    if ( compare_datasets(did, did2, buf) != TRUE) TEST_ERROR;

    /* close the destination dataset */
    if ( H5Dclose(did2) < 0) TEST_ERROR;

    /* close the source dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;


    /* Reclaim vlen buffer */
    if ( H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, buf) < 0) TEST_ERROR;

    /* close datatype */
    if ( H5Tclose(tid) < 0) TEST_ERROR;

    /* close dataspace */
    if ( H5Sclose(sid) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
        H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, buf);
    	H5Tclose(tid);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_dataset_contig_vl */


/*-------------------------------------------------------------------------
 * Function:    test_copy_dataset_chunked_vl
 *
 * Purpose:     Create a chunked dataset w/variable-length datatype in SRC
 *              file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao 
 *              Friday, September 30, 2005 
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_dataset_chunked_vl(hid_t fapl)
{
#ifdef NOT_YET
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t tid = -1;                             /* Datatype ID */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t pid = -1;                             /* Dataset creation property list ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    unsigned int i, j;                          /* Local index variables */
    hsize_t dim1d[1];                           /* Dataset dimensions */
    hsize_t chunk_dim1d[1] = {3};               /* Chunk dimensions */
    hvl_t buf[DIM_SIZE_1];                      /* Buffer for writing data */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];
#endif /* NOT_YET */

    TESTING("H5Gcopy(): chunked dataset with variable-length datatype");

#ifndef NOT_YET
    SKIPPED();
    puts("    Not supported yet!!");
#else /* NOT_YET */
    /* set initial data values */
    for(i = 0; i < DIM_SIZE_1; i++) {
        buf[i].len = i+1;
        buf[i].p = (int *)HDmalloc(buf[i].len * sizeof(int));
        for(j = 0; j < buf[i].len; j++)
            ((int *)buf[i].p)[j] = i*10+j;
    } /* end for */

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* Set dataspace dimensions */
    dim1d[0]=DIM_SIZE_1;

    /* create dataspace */
    if ( (sid = H5Screate_simple(1, dim1d, NULL)) < 0) TEST_ERROR;

    /* create datatype */
    if ( (tid = H5Tvlen_create(H5T_NATIVE_INT)) < 0) TEST_ERROR;
  
    /* create and set chunk plist */
    if ( (pid = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR;
    if ( H5Pset_chunk(pid, 1, chunk_dim1d) < 0) TEST_ERROR;

    /* create dataset at SRC file */
    if ( (did = H5Dcreate(fid_src, NAME_DATASET_VL, tid, sid, pid)) < 0) TEST_ERROR;

    /* close chunk plist */
    if ( H5Pclose(pid) < 0) TEST_ERROR;

    /* write data into file */
    if ( H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR;

    /* close the dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the dataset for copy */ 
    if ( (did = H5Dopen(fid_src, NAME_DATASET_VL)) < 0) TEST_ERROR;

    /* copy the dataset from SRC to DST */
    if ( H5Gcopy(did, fid_dst, NAME_DATASET_VL, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the destination dataset */ 
    if ( (did2 = H5Dopen(fid_dst, NAME_DATASET_VL)) < 0) TEST_ERROR;

    /* Check if the datasets are equal */
    if ( compare_datasets(did, did2, buf) != TRUE) TEST_ERROR;

    /* close the destination dataset */
    if ( H5Dclose(did2) < 0) TEST_ERROR;

    /* close the source dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;


    /* Reclaim vlen buffer */
    if ( H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, buf) < 0) TEST_ERROR;

    /* close datatype */
    if ( H5Tclose(tid) < 0) TEST_ERROR;

    /* close dataspace */
    if ( H5Sclose(sid) < 0) TEST_ERROR;

    PASSED();
#endif /* NOT_YET */
    return 0;

#ifdef NOT_YET
error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
        H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, buf);
    	H5Tclose(tid);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
#endif /* NOT_YET */
} /* end test_copy_dataset_chunked_vl */


/*-------------------------------------------------------------------------
 * Function:    test_copy_group_empty
 *
 * Purpose:     Create an empty group in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao 
 *              Friday, September 30, 2005 
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_group_empty(hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t gid = -1, gid2 = -1;                  /* Group IDs */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): empty group");

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* create group at the SRC file */
    if ( (gid = H5Gcreate(fid_src, NAME_GROUP_EMPTY, (size_t)0)) < 0) TEST_ERROR;

    /* attach attributes to the group */
    if ( test_copy_attach_attributes(gid, H5T_NATIVE_INT) < 0) TEST_ERROR;

    /* close the group */
    if ( H5Gclose(gid) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the group for copy */ 
    if ( (gid = H5Gopen(fid_src, NAME_GROUP_EMPTY)) < 0) TEST_ERROR;

    /* copy the group from SRC to DST */
    if ( H5Gcopy(gid, fid_dst, NAME_GROUP_EMPTY, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the destination group */ 
    if ( (gid2 = H5Gopen(fid_dst, NAME_GROUP_EMPTY)) < 0) TEST_ERROR;

    /* Check if the groups are equal */
    if ( compare_groups(gid, gid2) != TRUE) TEST_ERROR;

    /* close the destination group */
    if ( H5Gclose(gid2) < 0) TEST_ERROR;

    /* close the source group */
    if ( H5Gclose(gid) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_group_empty */


/*-------------------------------------------------------------------------
 * Function:    test_copy_group
 *
 * Purpose:     Create a group in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao 
 *              Friday, September 30, 2005 
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_group(hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1;                             /* Dataset ID */
    hid_t gid = -1, gid2 = -1;                  /* Group IDs */
    hid_t gid_sub = -1;                         /* Sub-group ID */
    hsize_t dim2d[2];
    int buf[DIM_SIZE_1][DIM_SIZE_2];
    int i, j;
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): simple nested groups");

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        for (j=0; j<DIM_SIZE_2; j++)
            buf[i][j] = 10000 + 100*i+j;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* create group at the SRC file */
    if ( (gid = H5Gcreate(fid_src, NAME_GROUP_TOP, (size_t)0)) < 0) TEST_ERROR;

    /* attach attributes to the group */
    if ( test_copy_attach_attributes(gid, H5T_NATIVE_INT) < 0) TEST_ERROR;

    /* Set dataspace dimensions */
    dim2d[0]=DIM_SIZE_1;
    dim2d[1]=DIM_SIZE_2;

    /* create dataspace */
    if ( (sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR;

    /* add a dataset to the group */
    if ( (did = H5Dcreate(fid_src, NAME_GROUP_DATASET, H5T_NATIVE_INT, sid, H5P_DEFAULT) ) < 0) TEST_ERROR;
    if ( H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR;
    
    /* close dataspace */
    if ( H5Sclose(sid) < 0) TEST_ERROR;

    /* close the dataset */
    if (H5Dclose(did) < 0) TEST_ERROR;

    /* create a sub-group */
    if ( (gid_sub = H5Gcreate(fid_src, NAME_GROUP_SUB, (size_t)0)) < 0) TEST_ERROR;
    if( H5Gclose(gid_sub) < 0) TEST_ERROR;

    /* create another  sub-group */
    if ( (gid_sub = H5Gcreate(fid_src, NAME_GROUP_SUB_2, (size_t)0)) < 0) TEST_ERROR;
    if( H5Gclose(gid_sub) < 0) TEST_ERROR;

    /* close the group */
    if ( H5Gclose(gid) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the group for copy */ 
    if ( (gid = H5Gopen(fid_src, NAME_GROUP_TOP)) < 0) TEST_ERROR;

    /* copy the group from SRC to DST */
    if ( H5Gcopy(gid, fid_dst, NAME_GROUP_TOP, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the destination group */ 
    if ( (gid2 = H5Gopen(fid_dst, NAME_GROUP_TOP)) < 0) TEST_ERROR;

    /* Check if the groups are equal */
    if ( compare_groups(gid, gid2) != TRUE) TEST_ERROR;

    /* close the destination group */
    if ( H5Gclose(gid2) < 0) TEST_ERROR;

    /* close the source group */
    if ( H5Gclose(gid) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Sclose(sid);
    	H5Dclose(did);
    	H5Gclose(gid_sub);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_group */


/*-------------------------------------------------------------------------
 * Function:    test_copy_group_deep
 *
 * Purpose:     Create a deep group hier. in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November 1, 2005 
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_group_deep(hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1;                             /* Dataset ID */
    hid_t gid = -1, gid2 = -1;                  /* Group IDs */
    hid_t gid_sub = -1, gid_sub2;               /* Sub-group IDs */
    hsize_t dim2d[2];
    int buf[DIM_SIZE_1][DIM_SIZE_2];
    int i, j, k;                                /* Local index variables */
    char objname[NAME_BUF_SIZE];                /* Sub-group & dataset name buffer */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): deep nested groups");

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        for (j=0; j<DIM_SIZE_2; j++)
            buf[i][j] = 10000 + 100*i+j;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* create group at the SRC file */
    if ( (gid = H5Gcreate(fid_src, NAME_GROUP_TOP, (size_t)0)) < 0) TEST_ERROR;

    /* attach attributes to the group */
    if ( test_copy_attach_attributes(gid, H5T_NATIVE_INT) < 0) TEST_ERROR;

    /* Set dataspace dimensions */
    dim2d[0]=DIM_SIZE_1;
    dim2d[1]=DIM_SIZE_2;

    /* create dataspace */
    if ( (sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR;

    /* create nested sub-groups & datasets */
    for(i = 0; i < NUM_SUB_GROUPS; i++) {
        sprintf(objname, "Group #%d", i);
        if ( (gid_sub = H5Gcreate(gid, objname, (size_t)0)) < 0) TEST_ERROR;

        for(j = 0; j < NUM_SUB_GROUPS; j++) {
            sprintf(objname, "Group #%d", j);
            if ( (gid_sub2 = H5Gcreate(gid_sub, objname, (size_t)0)) < 0) TEST_ERROR;

            for(k = 0; k < NUM_DATASETS; k++) {
                sprintf(objname, "Dataset #%d", k);

                /* add a dataset to the group */
                if ( (did = H5Dcreate(gid_sub2, objname, H5T_NATIVE_INT, sid, H5P_DEFAULT) ) < 0) TEST_ERROR;
                if ( H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR;
                if ( H5Dclose(did) < 0) TEST_ERROR;
            } /* end for */

            if( H5Gclose(gid_sub2) < 0) TEST_ERROR;
        } /* end for */

        if( H5Gclose(gid_sub) < 0) TEST_ERROR;
    } /* end for */

    /* close dataspace */
    if ( H5Sclose(sid) < 0) TEST_ERROR;

    /* close the group */
    if ( H5Gclose(gid) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the group for copy */ 
    if ( (gid = H5Gopen(fid_src, NAME_GROUP_TOP)) < 0) TEST_ERROR;

    /* copy the group from SRC to DST */
    if ( H5Gcopy(gid, fid_dst, NAME_GROUP_TOP, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the destination group */ 
    if ( (gid2 = H5Gopen(fid_dst, NAME_GROUP_TOP)) < 0) TEST_ERROR;

    /* Check if the groups are equal */
    if ( compare_groups(gid, gid2) != TRUE) TEST_ERROR;

    /* close the destination group */
    if ( H5Gclose(gid2) < 0) TEST_ERROR;

    /* close the source group */
    if ( H5Gclose(gid) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Sclose(sid);
    	H5Dclose(did);
    	H5Gclose(gid_sub);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_group_deep */


/*-------------------------------------------------------------------------
 * Function:    test_copy_group_loop
 *
 * Purpose:     Create a group hier. with loops in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November 1, 2005 
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_group_loop(hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t gid = -1, gid2 = -1;                  /* Group IDs */
    hid_t gid_sub = -1, gid_sub2;               /* Sub-group IDs */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): nested groups with loop");

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* create group at the SRC file */
    if ( (gid = H5Gcreate(fid_src, NAME_GROUP_TOP, (size_t)0)) < 0) TEST_ERROR;

    /* attach attributes to the group */
    if ( test_copy_attach_attributes(gid, H5T_NATIVE_INT) < 0) TEST_ERROR;

    /* create sub-groups */
    if ( (gid_sub = H5Gcreate(gid, NAME_GROUP_SUB, (size_t)0)) < 0) TEST_ERROR;

    if ( (gid_sub2 = H5Gcreate(gid, NAME_GROUP_SUB_SUB, (size_t)0)) < 0) TEST_ERROR;

    /* Create link to top group */
    if ( H5Glink2(gid, ".", H5G_LINK_HARD, gid_sub2, NAME_GROUP_LOOP) < 0) TEST_ERROR;

    /* close sub sub group */
    if( H5Gclose(gid_sub2) < 0) TEST_ERROR;

    /* close sub group */
    if( H5Gclose(gid_sub) < 0) TEST_ERROR;

    /* close the group */
    if ( H5Gclose(gid) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the group for copy */ 
    if ( (gid = H5Gopen(fid_src, NAME_GROUP_TOP)) < 0) TEST_ERROR;

    /* copy the group from SRC to DST */
    if ( H5Gcopy(gid, fid_dst, NAME_GROUP_TOP, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the destination group */ 
    if ( (gid2 = H5Gopen(fid_dst, NAME_GROUP_TOP)) < 0) TEST_ERROR;

    /* Check if the groups are equal */
    if ( compare_groups(gid, gid2) != TRUE) TEST_ERROR;

    /* close the destination group */
    if ( H5Gclose(gid2) < 0) TEST_ERROR;

    /* close the source group */
    if ( H5Gclose(gid) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid_sub2);
    	H5Gclose(gid_sub);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_group_loop */


/*-------------------------------------------------------------------------
 * Function:    test_copy_group_wide_loop
 *
 * Purpose:     Create a group hier. with loops in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November 1, 2005 
 *
 * Note:        Create groups w/lots of entries in each level, so that "dense"
 *              group form is used.
 *
 * Note:        Also tests multiple links to a locked group during copy.
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_group_wide_loop(hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t gid = -1, gid2 = -1;                  /* Group IDs */
    hid_t gid_sub = -1, gid_sub2;               /* Sub-group IDs */
    unsigned u, v;                              /* Local index variables */
    char objname[NAME_BUF_SIZE];                /* Object name buffer */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): wide nested groups with loop");

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* create group at the SRC file */
    if ( (gid = H5Gcreate(fid_src, NAME_GROUP_TOP, (size_t)0)) < 0) TEST_ERROR;

    /* attach attributes to the group */
    if ( test_copy_attach_attributes(gid, H5T_NATIVE_INT) < 0) TEST_ERROR;

    /* create wide sub-group hierarchy, with multiple links to higher groups */
    for(u = 0; u < NUM_WIDE_LOOP_GROUPS; u++) {
        sprintf(objname, "%s-%u", NAME_GROUP_SUB, u);
        if ( (gid_sub = H5Gcreate(gid, objname, (size_t)0)) < 0) TEST_ERROR;

        for(v = 0; v < NUM_WIDE_LOOP_GROUPS; v++) {
            sprintf(objname, "%s-%u", NAME_GROUP_SUB_SUB2, v);
            if ( (gid_sub2 = H5Gcreate(gid_sub, objname, (size_t)0)) < 0) TEST_ERROR;

            /* Create link to top group */
            if ( H5Glink2(gid, ".", H5G_LINK_HARD, gid_sub2, NAME_GROUP_LOOP) < 0) TEST_ERROR;

            /* Create link to sub-group */
            if ( H5Glink2(gid_sub, ".", H5G_LINK_HARD, gid_sub2, NAME_GROUP_LOOP2) < 0) TEST_ERROR;

            /* Create link to self :-) */
            if ( H5Glink2(gid_sub2, ".", H5G_LINK_HARD, gid_sub2, NAME_GROUP_LOOP3) < 0) TEST_ERROR;

            /* close sub sub group */
            if( H5Gclose(gid_sub2) < 0) TEST_ERROR;
        } /* end for */

        /* close sub group */
        if( H5Gclose(gid_sub) < 0) TEST_ERROR;
    } /* end for */

    /* close the group */
    if ( H5Gclose(gid) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the group for copy */ 
    if ( (gid = H5Gopen(fid_src, NAME_GROUP_TOP)) < 0) TEST_ERROR;

    /* copy the group from SRC to DST */
    if ( H5Gcopy(gid, fid_dst, NAME_GROUP_TOP, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the destination group */ 
    if ( (gid2 = H5Gopen(fid_dst, NAME_GROUP_TOP)) < 0) TEST_ERROR;

    /* Check if the groups are equal */
    if ( compare_groups(gid, gid2) != TRUE) TEST_ERROR;

    /* close the destination group */
    if ( H5Gclose(gid2) < 0) TEST_ERROR;

    /* close the source group */
    if ( H5Gclose(gid) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Gclose(gid_sub2);
    	H5Gclose(gid_sub);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_group_wide_loop */


/*-------------------------------------------------------------------------
 * Function:    test_copy_group_links
 *
 * Purpose:     Create a group and links in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Peter Cao 
 *              Friday, September 30, 2005 
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_group_links(hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1;                             /* Dataset ID */
    hid_t gid = -1, gid2 = -1;                  /* Group IDs */
    hsize_t dim2d[2];
    int buf[DIM_SIZE_1][DIM_SIZE_2];
    int i, j;
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): group with links");

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        for (j=0; j<DIM_SIZE_2; j++)
            buf[i][j] = 10000 + 100*i+j;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* create group at the SRC file */
    if ( (gid = H5Gcreate(fid_src, NAME_GROUP_LINK, (size_t)0)) < 0) TEST_ERROR;

    /* attach attributes to the group */
    if ( test_copy_attach_attributes(gid, H5T_NATIVE_INT) < 0) TEST_ERROR;

    /* Set dataspace dimensions */
    dim2d[0]=DIM_SIZE_1;
    dim2d[1]=DIM_SIZE_2;

    /* create dataspace */
    if ( (sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR;

    /* add a dataset to the group */
    if ( (did = H5Dcreate(fid_src, NAME_LINK_DATASET, H5T_NATIVE_INT, sid, H5P_DEFAULT) ) < 0) TEST_ERROR;
    if ( H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR;
    
    /* close dataspace */
    if ( H5Sclose(sid) < 0) TEST_ERROR;

    /* close the dataset */
    if (H5Dclose(did) < 0) TEST_ERROR;

    /* make a hard link to the dataset */
    if (H5Glink(fid_src, H5G_LINK_HARD, NAME_LINK_DATASET, NAME_LINK_HARD) < 0) TEST_ERROR;

    /* make a soft link to the dataset */
    if (H5Glink(fid_src, H5G_LINK_SOFT, NAME_LINK_DATASET, NAME_LINK_SOFT) < 0) TEST_ERROR;

    /* make a soft link to nowhere */
    if (H5Glink(fid_src, H5G_LINK_SOFT, "nowhere", NAME_LINK_SOFT_DANGLE) < 0) TEST_ERROR;

    /* close the group */
    if ( H5Gclose(gid) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the group for copy */ 
    if ( (gid = H5Gopen(fid_src, NAME_GROUP_LINK)) < 0) TEST_ERROR;

    /* copy the group from SRC to DST */
    if ( H5Gcopy(gid, fid_dst, NAME_GROUP_LINK, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the destination group */ 
    if ( (gid2 = H5Gopen(fid_dst, NAME_GROUP_LINK)) < 0) TEST_ERROR;

    /* Check if the groups are equal */
    if ( compare_groups(gid, gid2) != TRUE) TEST_ERROR;

    /* close the destination group */
    if ( H5Gclose(gid2) < 0) TEST_ERROR;

    /* close the source group */
    if ( H5Gclose(gid) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Sclose(sid);
    	H5Dclose(did);
    	H5Gclose(gid2);
    	H5Gclose(gid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_group_links */


/*-------------------------------------------------------------------------
 * Function:    test_copy_soft_link
 *
 * Purpose:     Create a soft link in SRC file and copy it to DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, September 30, 2005 
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_soft_link(hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    hid_t gid = -1;                             /* Group ID */
    hsize_t dim2d[2];
    int buf[DIM_SIZE_1][DIM_SIZE_2];
    int i, j;
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): object through soft link");

    /* set initial data values */
    for (i=0; i<DIM_SIZE_1; i++)
        for (j=0; j<DIM_SIZE_2; j++)
            buf[i][j] = 10000 + 100*i+j;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* create group at the SRC file */
    if ( (gid = H5Gcreate(fid_src, NAME_GROUP_LINK, (size_t)0)) < 0) TEST_ERROR;

    /* attach attributes to the group */
    if ( test_copy_attach_attributes(gid, H5T_NATIVE_INT) < 0) TEST_ERROR;

    /* Set dataspace dimensions */
    dim2d[0]=DIM_SIZE_1;
    dim2d[1]=DIM_SIZE_2;

    /* create dataspace */
    if ( (sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR;

    /* add a dataset to the group */
    if ( (did = H5Dcreate(fid_src, NAME_LINK_DATASET, H5T_NATIVE_INT, sid, H5P_DEFAULT) ) < 0) TEST_ERROR;
    if ( H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR;
    
    /* close dataspace */
    if ( H5Sclose(sid) < 0) TEST_ERROR;

    /* close the dataset */
    if (H5Dclose(did) < 0) TEST_ERROR;

    /* make a soft link to the dataset */
    if (H5Glink(fid_src, H5G_LINK_SOFT, NAME_LINK_DATASET, NAME_LINK_SOFT) < 0) TEST_ERROR;

    /* close the group */
    if ( H5Gclose(gid) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the dataset through the soft link for copy */ 
    if ( (did = H5Dopen(fid_src, NAME_LINK_SOFT)) < 0) TEST_ERROR;

    /* copy the dataset from SRC to DST */
    if ( H5Gcopy(did, fid_dst, NAME_DATASET_SIMPLE, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the destination dataset */ 
    if ( (did2 = H5Dopen(fid_dst, NAME_DATASET_SIMPLE)) < 0) TEST_ERROR;

    /* Check if the datasets are equal */
    if ( compare_datasets(did, did2, buf) != TRUE) TEST_ERROR;

    /* close the destination dataset */
    if ( H5Dclose(did2) < 0) TEST_ERROR;

    /* close the source dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Sclose(sid);
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Gclose(gid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_soft_link */


/*-------------------------------------------------------------------------
 * Function:    test_copy_exist
 *
 * Purpose:     Create a simple dataset in SRC file and copy it onto an
 *              existing object in DST file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November  8, 2005 
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_exist(hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1;                             /* Dataset IDs */
    int buf[DIM_SIZE_1][DIM_SIZE_2];            /* Buffer for writing data */
    hsize_t dim2d[2];                           /* Dataset dimensions */
    int i, j;                                   /* local index variables */
    herr_t ret;                                 /* Generic return value */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): existing object");

    /* Initialize write buffer */
    for (i=0; i<DIM_SIZE_1; i++)
        for (j=0; j<DIM_SIZE_2; j++)
            buf[i][j] = 10000 + 100*i+j;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* Set dataspace dimensions */
    dim2d[0] = DIM_SIZE_1;
    dim2d[1] = DIM_SIZE_2;

    /* create 2D dataspace */
    if ( (sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR;
  
    /* create 2D int dataset at SRC file */
    if ( (did = H5Dcreate(fid_src, NAME_DATASET_SIMPLE, H5T_NATIVE_INT, sid, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* write data into file */
    if ( H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR;

    /* close dataspace */
    if ( H5Sclose(sid) < 0) TEST_ERROR;

    /* attach attributes to the dataset */
    if ( test_copy_attach_attributes(did, H5T_NATIVE_INT) < 0) TEST_ERROR;

    /* close the dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the dataset for copy */ 
    if ( (did = H5Dopen(fid_src, NAME_DATASET_SIMPLE)) < 0) TEST_ERROR;

    /* copy the dataset from SRC to DST */
    if ( H5Gcopy(did, fid_dst, NAME_DATASET_SIMPLE, H5P_DEFAULT) < 0) TEST_ERROR;

    /* try to copy the dataset from SRC to DST again (should fail) */
    H5E_BEGIN_TRY {
        ret = H5Gcopy(did, fid_dst, NAME_DATASET_SIMPLE, H5P_DEFAULT);
    } H5E_END_TRY;
    if( ret >= 0) TEST_ERROR;

    /* close the source dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did);
    	H5Sclose(sid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_exist */


/*-------------------------------------------------------------------------
 * Function:    test_copy_path
 *
 * Purpose:     Create a simple dataset in SRC file and copy it to DST file
 *              using a full path name 
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November  8, 2005 
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_path(hid_t fapl)
{
    hid_t fid_src = -1, fid_dst = -1;           /* File IDs */
    hid_t sid = -1;                             /* Dataspace ID */
    hid_t did = -1, did2 = -1;                  /* Dataset IDs */
    hid_t gid = -1;                             /* Group ID */
    int buf[DIM_SIZE_1][DIM_SIZE_2];            /* Buffer for writing data */
    hsize_t dim2d[2];                           /* Dataset dimensions */
    int i, j;                                   /* local index variables */
    herr_t ret;                                 /* Generic return value */
    char src_filename[NAME_BUF_SIZE];
    char dst_filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): full path");

    /* Initialize write buffer */
    for (i=0; i<DIM_SIZE_1; i++)
        for (j=0; j<DIM_SIZE_2; j++)
            buf[i][j] = 10000 + 100*i+j;

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, src_filename, sizeof src_filename);
    h5_fixname(FILENAME[1], fapl, dst_filename, sizeof dst_filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid_src = H5Fcreate(src_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* Set dataspace dimensions */
    dim2d[0] = DIM_SIZE_1;
    dim2d[1] = DIM_SIZE_2;

    /* create 2D dataspace */
    if ( (sid = H5Screate_simple(2, dim2d, NULL)) < 0) TEST_ERROR;
  
    /* create 2D int dataset at SRC file */
    if ( (did = H5Dcreate(fid_src, NAME_DATASET_SIMPLE, H5T_NATIVE_INT, sid, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* write data into file */
    if ( H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR;

    /* close dataspace */
    if ( H5Sclose(sid) < 0) TEST_ERROR;

    /* attach attributes to the dataset */
    if ( test_copy_attach_attributes(did, H5T_NATIVE_INT) < 0) TEST_ERROR;

    /* close the dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;


    /* open the source file with read-only */
    if ( (fid_src = H5Fopen(src_filename, H5F_ACC_RDONLY, fapl)) < 0) TEST_ERROR;

    /* create destination file */
    if ( (fid_dst = H5Fcreate(dst_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
   
    /* Create an uncopied object in destination file so that addresses in source and destination files aren't the same */
    if ( H5Gclose(H5Gcreate(fid_dst, NAME_GROUP_UNCOPIED, (size_t)0)) < 0) TEST_ERROR;

    /* open the dataset for copy */ 
    if ( (did = H5Dopen(fid_src, NAME_DATASET_SIMPLE)) < 0) TEST_ERROR;

    /* copy the dataset from SRC to DST (should fail - intermediate groups not there) */
    H5E_BEGIN_TRY {
        ret = H5Gcopy(did, fid_dst, NAME_DATASET_SUB_SUB, H5P_DEFAULT);
    } H5E_END_TRY;
    if( ret >= 0) TEST_ERROR;

    /* Create the intermediate groups in destination file */
    if ( (gid = H5Gcreate(fid_dst, NAME_GROUP_TOP, (size_t)0)) < 0) TEST_ERROR;
    if ( H5Gclose(gid) < 0) TEST_ERROR;

    if ( (gid = H5Gcreate(fid_dst, NAME_GROUP_SUB, (size_t)0)) < 0) TEST_ERROR;
    if ( H5Gclose(gid) < 0) TEST_ERROR;

    if ( (gid = H5Gcreate(fid_dst, NAME_GROUP_SUB_SUB, (size_t)0)) < 0) TEST_ERROR;
    if ( H5Gclose(gid) < 0) TEST_ERROR;

    /* copy the dataset from SRC to DST, using full path */
    if ( H5Gcopy(did, fid_dst, NAME_DATASET_SUB_SUB, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the destination dataset */ 
    if ( (did2 = H5Dopen(fid_dst, NAME_DATASET_SUB_SUB)) < 0) TEST_ERROR;

    /* Check if the datasets are equal */
    if ( compare_datasets(did, did2, buf) != TRUE) TEST_ERROR;

    /* close the destination dataset */
    if ( H5Dclose(did2) < 0) TEST_ERROR;

    /* close the source dataset */
    if ( H5Dclose(did) < 0) TEST_ERROR;

    /* close the SRC file */
    if ( H5Fclose(fid_src) < 0) TEST_ERROR;

    /* close the DST file */
    if ( H5Fclose(fid_dst) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Dclose(did2);
    	H5Dclose(did);
    	H5Sclose(sid);
    	H5Gclose(gid);
    	H5Fclose(fid_dst);
    	H5Fclose(fid_src);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_path */


/*-------------------------------------------------------------------------
 * Function:    test_copy_same_file_named_datatype
 *
 * Purpose:     Create name datatype in SRC file and copy it to same file
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, November  8, 2005 
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_same_file_named_datatype(hid_t fapl)
{
    hid_t fid = -1;                     /* File ID */
    hid_t tid = -1, tid2 = -1;          /* Datatype IDs */
    char filename[NAME_BUF_SIZE];

    TESTING("H5Gcopy(): named datatype in same file");

    /* Initialize the filenames */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Reset file address checking info */
    addr_reset();

    /* create source file */
    if ( (fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* create datatype */
    if ( (tid = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR;

    /* create named datatype */
    if ( (H5Tcommit(fid, NAME_DATATYPE_SIMPLE, tid)) < 0) TEST_ERROR;


    /* copy the datatype from SRC to DST */
    if ( H5Gcopy(tid, fid, NAME_DATATYPE_SIMPLE2, H5P_DEFAULT) < 0) TEST_ERROR;

    /* open the copied datatype */ 
    if ( (tid2 = H5Topen(fid, NAME_DATATYPE_SIMPLE2)) < 0) TEST_ERROR;

    /* Compare the datatypes */
    if ( H5Tequal(tid, tid2) != TRUE) TEST_ERROR;

    /* close the destination datatype */
    if ( H5Tclose(tid2) < 0) TEST_ERROR;

    /* close the source datatype */
    if ( H5Tclose(tid) < 0) TEST_ERROR;

    /* close the file */
    if ( H5Fclose(fid) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    	H5Tclose(tid2);
    	H5Tclose(tid);
    	H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
} /* end test_copy_same_file_named_datatype */


/*-------------------------------------------------------------------------
 * Function:    test_copy_mount
 *
 * Purpose:     Test copying objects between mounted files
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 * Programmer:  Quincey Koziol
 *              Saturday, November  5, 2005 
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy_mount(hid_t fapl)
{
    TESTING("H5Gcopy(): objects between mounted files");

    SKIPPED();
    puts("    Not tested yet!!");
    return 0;
} /* end test_copy_mount */


/*-------------------------------------------------------------------------
 * Function:   	main 
 *
 * Purpose:     Test H5Gcopy() 
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Peter Cao 
 *              Friday, September 30, 2005 
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int 
main(void)
{
    int         nerrors = 0;
    hid_t	fapl;

    /* Setup */
    h5_reset();
    fapl = h5_fileaccess();

    /* The tests... */
    nerrors += test_copy_named_datatype(fapl);
    nerrors += test_copy_named_datatype_vl(fapl);
    nerrors += test_copy_named_datatype_vl_vl(fapl);
    nerrors += test_copy_dataset_simple(fapl);
    nerrors += test_copy_dataset_simple_empty(fapl);
    nerrors += test_copy_dataset_compound(fapl);
    nerrors += test_copy_dataset_chunked(fapl);
    nerrors += test_copy_dataset_chunked_empty(fapl);
    nerrors += test_copy_dataset_chunked_sparse(fapl);
    nerrors += test_copy_dataset_compressed(fapl);
    nerrors += test_copy_dataset_compact(fapl);
    nerrors += test_copy_dataset_external(fapl);
    nerrors += test_copy_dataset_named_dtype(fapl);
    nerrors += test_copy_dataset_named_dtype_hier(fapl);
    nerrors += test_copy_dataset_named_dtype_hier_outside(fapl);
    nerrors += test_copy_dataset_multi_ohdr_chunks(fapl);
    nerrors += test_copy_dataset_attr_named_dtype(fapl);
    nerrors += test_copy_dataset_contig_vl(fapl);
    nerrors += test_copy_dataset_chunked_vl(fapl);    	/* TODO */
/* TODO: Add more tests for copying vlen data */
    nerrors += test_copy_group_empty(fapl);
    nerrors += test_copy_group(fapl);
    nerrors += test_copy_group_deep(fapl);
    nerrors += test_copy_group_loop(fapl);
    nerrors += test_copy_group_wide_loop(fapl);  
    nerrors += test_copy_group_links(fapl);  
    nerrors += test_copy_soft_link(fapl);  
    nerrors += test_copy_exist(fapl);  
    nerrors += test_copy_path(fapl);  
    nerrors += test_copy_same_file_named_datatype(fapl);  
/* TODO: Add more tests for copying objects in same file */
    nerrors += test_copy_mount(fapl);           /* TODO */
/* TODO: Add more tests for copying objects in mounted files */

    /* Reset file address checking info */
    addr_reset();

    /* Results */
    if (nerrors) {
	printf("***** %d OBJECT COPY TEST%s FAILED! *****\n",
	       nerrors, (1 == nerrors ? "" : "S"));
        exit(1);
    }

    puts ("All object copying tests passed.");
    h5_cleanup(FILENAME, fapl);
    return 0;
} /* main */

