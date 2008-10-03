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

/*
 * Programmer:  Raymond Lu<slu@ncsa.uiuc.edu>
 *              Aug 27, 2007
 */

#define H5Z_PACKAGE		/*suppress error about including H5Zpkg	  */

#include "H5private.h"		/* Generic Functions			*/
#include "H5B2private.h"	/* v2 B-trees				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Dprivate.h"		/* Dataset                              */
#include "H5Fprivate.h"         /* File access                          */
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5SMprivate.h"	/* Shared Object Header Messages	*/
#include "H5Tprivate.h"	        /* Datatype                             */
#include "H5Zpkg.h"		/* Data filters				*/

#ifdef H5_HAVE_FILTER_DTYPE_MODIFY

/* Current version of the filter*/
#define H5Z_DTYPE_MODIFY_VERS (1)

/* Number of client data elements */
#define CD_NELMTS         16

#define H5Z_MDTYPE_DEC    0
#define H5Z_MDTYPE_INC    1

/* v2 B-tree creation macros for 'datatype' field index */
/* How to determine the node size? */
#define H5Z_DTYPE_BT2_NODE_SIZE          128 
#define H5Z_DTYPE_BT2_MERGE_PERC         40
#define H5Z_DTYPE_BT2_SPLIT_PERC         100

/* Local function prototypes */
static herr_t H5Z_set_local_dtype_modify(hid_t dcpl_id, hid_t dtype_id, hid_t space_id,
    hid_t file_id);
static herr_t H5Z_reset_local_dtype_modify(hid_t dcpl_id, hid_t dtype_id, hid_t space_id,
    hid_t file_id, hbool_t from_reopen);
static herr_t H5Z_change_local_dtype_modify(hid_t dcpl_id, hsize_t chunk_offset);
static herr_t H5Z_evict_local_dtype_modify(hid_t dcpl_id, hsize_t chunk_offset);
static herr_t H5Z_delete_local_dtype_modify(hid_t dcpl_id);
static herr_t H5Z_close_local_dtype_modify(hid_t dcpl_id);
static herr_t H5Z_free_slist_node(void *item, void UNUSED *key, void UNUSED *operator_data);
static herr_t H5Z_btree2_dtype_store(void *_nrecord, const void *_udata);
static herr_t H5Z_btree2_dtype_retrieve(void *udata, const void *nrecord);
static herr_t H5Z_btree2_dtype_encode(const H5F_t *f, uint8_t *raw, const void *_nrecord);
static herr_t H5Z_btree2_dtype_decode(const H5F_t *f, const uint8_t *raw, void *_nrecord);
static herr_t H5Z_btree2_dtype_compare(const void *_bt2_udata, const void *_bt2_rec);
static herr_t H5Z_btree2_dtype_find_cb(void *record, void *op_data);
static herr_t H5Z_btree2_dtype_remove_cb(void *record, void *op_data);
static herr_t H5Z_btree2_dtype_delete_cb(void *record, void *op_data);

static size_t H5Z_filter_dtype_modify (unsigned flags, hsize_t chunk_offset, size_t cd_nelmts,
    const unsigned cd_values[], size_t nbytes, size_t *buf_size, void **buf);

/* This message derives from H5Z */
H5Z_class_t H5Z_DTYPE_MODIFY[1] = {{
    H5Z_CLASS_T_VERS,           /* H5Z_class_t version          */
    H5Z_FILTER_DTYPE_MODIFY,	/* Filter id number		*/
    1,                    /* encoder_present flag (set to true) */
    1,                    /* decoder_present flag (set to true) */
    "dtype_modify",		/* Filter name for debugging	*/
    NULL,                       /* The "can apply" callback     */
    H5Z_set_local_dtype_modify, /* The "set local" callback     */
    H5Z_reset_local_dtype_modify, /* The "reset local" callback */
    H5Z_change_local_dtype_modify,/* The "change local" callback*/
    H5Z_evict_local_dtype_modify, /* The "evict local" callback */
    H5Z_delete_local_dtype_modify, /* The "delete local" callback */
    H5Z_close_local_dtype_modify, /* The "close local" callback */
    H5Z_filter_dtype_modify,	/* The actual filter function	*/
}};

typedef struct H5Z_slist_node_t {
    H5T_t    *dtype;
    hbool_t  type_modified;
    hsize_t  key; 
} H5Z_slist_node_t;

/* Typedef for native 'name' field index records in the v2 B-tree */
typedef struct H5Z_mdtype_bt2_rec_t {
    uint8_t       version;
    uint8_t       is_shared;
    uint64_t      numb_ref;
    H5T_t         *dtype;
    hbool_t       found;
    uint8_t       adjust;
} H5Z_mdtype_bt2_rec_t;

/* The v2 B-tree class for indexing 'datatype' field */
const H5B2_class_t H5Z_BT2_DTYPE[1]={{  /* B-tree class information */
    H5B2_MODIFY_TYPE_ID,                /* Type of B-tree */
    sizeof(H5Z_mdtype_bt2_rec_t),      /* Size of native record */
    H5Z_btree2_dtype_store,       /* Record storage callback */
    H5Z_btree2_dtype_retrieve,    /* Record retrieval callback */
    H5Z_btree2_dtype_compare,     /* Record comparison callback */
    H5Z_btree2_dtype_encode,      /* Record encoding callback */
    H5Z_btree2_dtype_decode,      /* Record decoding callback */
    NULL        /* Record debugging callback */
}};


/* Declare a free list to manage H5Z_slist_node_t objects */
H5FL_DEFINE_STATIC(H5Z_slist_node_t);


/*-------------------------------------------------------------------------
 * Function:	H5Z_set_local_dtype_modify
 *
 * Purpose:	Set the "local" dataset parameters for datatype modification.
 *              They are the ID of the dataset's datatype and the file
 *              pointer.
 *
 * Return:	Success: Non-negative
 *		Failure: Negative
 *
 * Programmer:	Raymond Lu
 *              30 Aug 2007
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Z_set_local_dtype_modify(hid_t dcpl_id, hid_t dtype_id, hid_t UNUSED space_id, 
    hid_t file_id)
{
    H5P_genplist_t  *dc_plist = NULL;       /* New Property list */
    H5F_t      *file=NULL;         /* File object for file ID */
    H5T_t      *dt = NULL;         /* Datatype object for datatype ID */
    H5SL_t     *chunk_slist=NULL;  /* Skip list for keeping record of chunks */
    size_t     dt_size;            /* the size of datatype */
    size_t     bt2_rrec_size;      /* v2 B-tree raw record size */
    haddr_t    baddr;              /* Address of B-tree to store dtype info  */
    size_t cd_nelmts = 0;          /* Number of filter parameters */
    unsigned count = 0;            /* Keeps track of pointer size */ 
    unsigned baddr_count = 0;      /* Keeps track of btree address size */ 
    unsigned cd_values[CD_NELMTS]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}; /* Filter parameters */
    uint8_t *pp = NULL;
    herr_t ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5Z_set_local_dtype_modify, FAIL)

    /* Get the dataset's property list object */
    if(NULL == (dc_plist = (H5P_genplist_t *)H5I_object(dcpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't get dataset creation property list")

    if(NULL == (file = H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")

    /* Check args */
    if (NULL == (dt = H5I_object_verify(dtype_id,H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");

    /* The first element of the client data is reserved for COUNT, which is 
     * copied toward the end of this function.  COUNT has two digits and 
     * keeps track of the number of elements in CD_VALUES except itself (first
     * digit) and the size of file pointer and skip list pointer (second digit).
     */
    cd_nelmts = 1;

    /* The second element of the client data is the ID of dataset's datatype. */
    cd_values[1] = dtype_id;
    cd_nelmts += 1;
    count = 10;

    pp = (uint8_t*)((unsigned *)cd_values + 2);

    /*
     * The third is the file pointer copied as unsigned integer.  The size of
     * the pointer can be 4 or 8 bytes.  Turn on the first digit of COUNT
     * if the pointer size is 8 bytes.  8-byte pointer should take 2 CD_NELMTS. 
     */  
    HDmemcpy(pp, &file, sizeof(H5F_t*));
    count +=10;
    if(4 == sizeof(H5F_t*)) {
        cd_nelmts += 1;
        pp += 4; 
    } else if(8 == sizeof(H5F_t*)) {
        cd_nelmts += 2;
        count +=1;
        pp += 8; 
    } else
	HGOTO_ERROR(H5E_PLINE, H5E_CANTENCODE, FAIL, "not implemented yet")

    /* Create skip list to keep track of visited chunk.  It's created here 
     * because the filter needs it when the dataset is just created and the 
     * data is being written to the file. But keep in mind that the client
     * data is saved in the file. Everything is invalid when the file is 
     * reopened and the client data is retrieved. */
    if((chunk_slist = H5SL_create(H5SL_TYPE_HSIZE, 0.5, (size_t)16)) == NULL)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTCREATE, FAIL, "can't create skip list for chunks")

    /* The fourth element is the skip list for chunks.  Again the second digit
     * indicates whether the pointer is 8 bytes. */  
    HDmemcpy(pp, &chunk_slist, sizeof(H5SL_t*));
    count += 10;
    if(4 == sizeof(H5SL_t*)) {
        cd_nelmts += 1;
        pp += 4; 
    } else if(8 == sizeof(H5SL_t*)) {
        count += 1;
        cd_nelmts += 2;
        pp += 8; 
    } else
        HGOTO_ERROR(H5E_PLINE, H5E_CANTENCODE, FAIL, "not implemented yet")

    /* Set the COUNT */
    cd_values[0] = count;

    /* Generally only support committed or shared datatype.  Check it 
     * here because this function can be called by H5D_create. We also allow 
     * integer or floating number because of their limited encoding size.
     */ 
    if(!H5T_committed(dt) && 
        !H5SM_can_share(file, H5AC_dxpl_id, NULL, NULL, H5O_DTYPE_ID, dt)) {
        H5T_class_t tclass;
        if((tclass = H5T_get_class(dt, TRUE)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "can't find datatype class")

        if(tclass != H5T_INTEGER && tclass != H5T_FLOAT)
            HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "violate datatype restriction")
    }

    /* Check if the datatype should be (or are already) shared in the SOHM table */
    if(H5SM_try_share(file, H5AC_dxpl_id, NULL, H5O_DTYPE_ID, dt, NULL) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADMESG, FAIL, "trying to share datatype failed")

    /* Get the encoding size of the dataset's datatype */
    if((dt_size = H5O_msg_raw_size(file, H5O_DTYPE_ID, FALSE, dt))==0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADSIZE, 0, "can't find datatype size")

    /* If the datatype is committed or shared, its size should be the address plus 
     * index of heap, which is 12.  If the datatype is predefined, the encoding
     * size should be at most 20 bytes (for floating number. See H5O_dtype_size in
     * H5Odtype.c).  Because the datatype may be changed later, we make it big 
     * enough now. 
     */ 
    if(dt_size < 20)  
        dt_size = 20;

    /* Create a B-tree to store the info of all the dtypes used by the chunks. */ 
    bt2_rrec_size = 1 +                /* version number */
                    1 +                /* flag for shared datatype */
                    dt_size +          /* datatype info */
                    8;                 /* Number of reference */

    if(H5B2_create(file, H5AC_dxpl_id, H5Z_BT2_DTYPE, (size_t)H5Z_DTYPE_BT2_NODE_SIZE, 
        bt2_rrec_size, H5Z_DTYPE_BT2_SPLIT_PERC, H5Z_DTYPE_BT2_MERGE_PERC, &baddr/*out*/) < 0)
	HGOTO_ERROR(H5E_PLINE, H5E_CANTINIT, FAIL, "can't create B-tree")

    /* the B-tree address can be 8 or 16 byte in size */    
    if(8 == sizeof(haddr_t)) {
        baddr_count = 2;
        cd_nelmts += 2;
    } else if(16 == sizeof(haddr_t)) {
        baddr_count = 4;
        cd_nelmts += 4;
    } else
        HGOTO_ERROR(H5E_PLINE, H5E_CANTENCODE, FAIL, "not implemented yet")

    HDmemcpy(pp, &baddr_count, sizeof(unsigned));
    pp += 4; 
    cd_nelmts += 1;

    HDmemcpy(pp, &baddr, sizeof(haddr_t));

    /* Modify the filter's parameters for this dataset */
    if(H5P_modify_filter(dc_plist, H5Z_FILTER_DTYPE_MODIFY, H5Z_FLAG_MANDATORY, 
        (size_t)cd_nelmts, cd_values) < 0)
	HGOTO_ERROR(H5E_PLINE, H5E_CANTSET, FAIL, "can't set local parameters")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Z_set_local_dtype_modify() */


/*-------------------------------------------------------------------------
 * Function:	H5Z_reset_local_dtype_modify
 *
 * Purpose:	Reset the "local" dataset parameters for datatype 
 *              modification.  This function is the same as 
 *              H5Z_set_local_dtype_modify.  But it's called after the 
 *              dataset has been created to update some info like file_id
 *              and datatype.  It's called by H5D_modify_dtype and H5D_open.
 *
 * Return:	Success: Non-negative
 *		Failure: Negative
 *
 * Programmer:	Raymond Lu
 *              16 Sept 2007
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Z_reset_local_dtype_modify(hid_t dcpl_id, hid_t dtype_id, hid_t UNUSED space_id, 
    hid_t file_id, hbool_t from_reopen)
{
    H5P_genplist_t  *dc_plist = NULL;       /* New Property list */
    H5F_t      *file=NULL;         /* File object for file ID */
    H5SL_t     *chunk_slist=NULL;  /* Skip list for keeping record of chunks */
    size_t cd_nelmts = 0;          /* Number of filter parameters */
    size_t orig_cd_nelmts = CD_NELMTS;     /* Number of filter parameters */
    unsigned count = 0;            /* Keeps track of pointer size */ 
    unsigned orig_count = 0;       /* Keeps track of pointer size */ 
    unsigned baddr_count = 0;            /* Keeps track of pointer size */ 
    unsigned orig_baddr_count = 0;       /* Keeps track of pointer size */ 
    unsigned cd_values[CD_NELMTS]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}; /* Filter parameters */
    unsigned orig_cd_values[CD_NELMTS]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}; /* Filter parameters */
    uint8_t *pp = NULL;
    uint8_t *orig_pp = NULL;
    herr_t ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5Z_reset_local_dtype_modify, FAIL)

    /* Get the dataset's property list object */
    if(NULL == (dc_plist = (H5P_genplist_t *)H5I_object(dcpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't get dataset creation property list")

    if(H5P_get_filter_by_id(dc_plist, H5Z_FILTER_DTYPE_MODIFY, NULL, 
        &orig_cd_nelmts, orig_cd_values, (size_t)0, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't get filter info")

    orig_count = orig_cd_values[0];

    if(NULL == (file = H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")

    /* The first element of the client data is reserved for COUNT, which is 
     * copied toward the end of this function.  COUNT has two digits and 
     * keeps track of the number of elements in CD_VALUES except itself (first
     * digit) and the size of file pointer and skip list pointer (second digit).
     */
    cd_nelmts = 1;

    /* The second element of the client data is the ID of dataset's datatype */
    cd_values[1] = dtype_id;
    cd_nelmts += 1;
    count = 10;

    pp = (uint8_t*)((unsigned *)cd_values + 2);

    /*
     * The third is the file pointer copied as unsigned integer.  Depending 
     * on the size of pointer, it may take the space of 1 - 2 elements.
     */  
    HDmemcpy(pp, &file, sizeof(H5F_t*));
    if(4 == sizeof(H5F_t*)) {
        cd_nelmts += 1;
        count += 10;
        pp += 4; 
    } else if(8 == sizeof(H5F_t*)) {
        cd_nelmts += 2;
        count += 10;
        count += 1;
        pp += 8;
    } else
	HGOTO_ERROR(H5E_PLINE, H5E_CANTENCODE, FAIL, "not implemented yet")

    /* Check whether the skip list for visited chunks has been created.  If the first
     * digit of original COUNT is 2, there're only 2 elements, and the skip list hasn't 
     * been created.  If this function is called by H5Dopen and FROM_REOPEN is set, the
     * pointer value is invalid because the client data was saved in the file.  Create
     * a new skip list in this case.  If it's 3 and FROM_REOPEN isn't set, the skip 
     * list has been created.  Simply copy the pointer over. 
     */  
    if(orig_count < 30 || from_reopen) {
        /* Create skip list to keep track of visited chunk */
        if((chunk_slist = H5SL_create(H5SL_TYPE_HSIZE, 0.5, (size_t)16)) == NULL)
            HGOTO_ERROR(H5E_PLINE, H5E_CANTCREATE, FAIL, "can't create skip list for chunks")

        /* The fourth element is the skip list for chunks.  Again the second digit
         * indicates whether the pointer is 8 bytes. */  
        HDmemcpy(pp, &chunk_slist, sizeof(H5SL_t*));
        count += 10;
        if(4 == sizeof(H5SL_t*)) {
            cd_nelmts += 1;
            pp += 4; 
        } else if(8 == sizeof(H5SL_t*)) {
            count += 1;
            cd_nelmts += 2;
            pp += 8; 
        } else
	    HGOTO_ERROR(H5E_PLINE, H5E_CANTENCODE, FAIL, "not implemented yet")

        if(orig_count == 20)
            orig_pp = (uint8_t*)((unsigned *)orig_cd_values + 3);
        else if(orig_count > 20 && orig_count <= 30) 
            orig_pp = (uint8_t*)((unsigned *)orig_cd_values + 4);
        else if(orig_count > 30)
            orig_pp = (uint8_t*)((unsigned *)orig_cd_values + 6);
    } else if (orig_count == 30 && !from_reopen) {
        /* Copy the skip list.  The size of pointer is 4 bytes. */
        orig_pp = (uint8_t*)((unsigned *)orig_cd_values + 3);
        HDmemcpy(pp, orig_pp, 4);
        count += 10;
        cd_nelmts += 1;
        pp += 4; 
        orig_pp += 4; 
    } else if (orig_count > 30 && !from_reopen) {
        /* Copy the skip list.  The size of pointer is 8 bytes. */
        orig_pp = (uint8_t*)((unsigned *)orig_cd_values + 4);
        HDmemcpy(pp, orig_pp, 8);
        count += 10;
        count += 1;
        cd_nelmts += 2;
        pp += 8; 
        orig_pp += 8; 
    } else
	HGOTO_ERROR(H5E_PLINE, H5E_CANTENCODE, FAIL, "not implemented yet")

    /* Set the COUNT */
    cd_values[0] = count;

    /* Copy B-tree address to the new client data */
    HDmemcpy(&orig_baddr_count, orig_pp, sizeof(unsigned));
    orig_pp += 4;

    /* the B-tree address can be 8 or 16 byte in size */    
    if(orig_baddr_count == 2) {
        baddr_count = 2;
        HDmemcpy(pp, &baddr_count, sizeof(unsigned));
        pp += 4;
        cd_nelmts += 1;
        HDmemcpy(pp, orig_pp, 8);
        cd_nelmts += 2;
    } else if(orig_baddr_count == 4) {
        baddr_count = 4;
        HDmemcpy(pp, &baddr_count, sizeof(unsigned));
        pp += 4;
        cd_nelmts += 1;
        HDmemcpy(pp, orig_pp, 16);
        cd_nelmts += 4;
    } else
	HGOTO_ERROR(H5E_PLINE, H5E_CANTCOPY, FAIL, "not implemented yet")

    /* Modify the filter's parameters for this dataset */
    if(H5P_modify_filter(dc_plist, H5Z_FILTER_DTYPE_MODIFY, H5Z_FLAG_MANDATORY, 
        (size_t)cd_nelmts, cd_values) < 0)
	HGOTO_ERROR(H5E_PLINE, H5E_CANTSET, FAIL, "can't set local parameters")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Z_reset_local_dtype_modify() */


/*-------------------------------------------------------------------------
 * Function:	H5Z_change_local_dtype_modify
 *
 * Purpose:	Search the node of the skip list for chunk info and update it.
 *              This function is called by H5D_istore_conv_dtype when the 
 *              datatype is changed and all the chunks in the cache have 
 *              to be converted.
 *
 * Return:	Success: Non-negative
 *		Failure: Negative
 *
 * Programmer:	Raymond Lu
 *              16 April 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Z_change_local_dtype_modify(hid_t dcpl_id, hsize_t chunk_offset)
{
    H5P_genplist_t  *dc_plist = NULL;       /* New Property list */
    H5SL_t          *chunk_slist=NULL;      /* Skip list for keeping record of chunks */
    H5Z_slist_node_t *node;
    size_t          cd_nelmts=CD_NELMTS;    /* Number of filter parameters */
    unsigned        count = 0;              /* Keeps track of pointer size */ 
    unsigned cd_values[CD_NELMTS]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}; /* Filter parameters */
    uint8_t         *pp = NULL;
    herr_t ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5Z_change_local_dtype_modify, FAIL)

    HDassert(H5I_GENPROP_LST==H5I_get_type(dcpl_id));

    /* Get the dataset's property list object */
    if(NULL == (dc_plist = (H5P_genplist_t *)H5I_object(dcpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't get dataset creation property list")

    if(H5P_get_filter_by_id(dc_plist, H5Z_FILTER_DTYPE_MODIFY, NULL, &cd_nelmts, cd_values, 
        (size_t)0, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't get filter info")

    /* If the skip list exists, update the dtype modification info for the chunk */
    count = cd_values[0];

    if(count >= 30) {
        pp = (uint8_t*)((unsigned *)cd_values + 2);

        /* Depending on the size of pointer, retrieve the value of the pointer to 
         * the skip list. */
        if (count == 30) {
            pp += 4;
            HDmemcpy(&chunk_slist, pp, 4);
        } else if (count > 30) {
            pp += 8;
            HDmemcpy(&chunk_slist, pp, 8);
        }

        if(!chunk_slist)
	    HGOTO_ERROR(H5E_SLIST, H5E_CANTGET, FAIL, "can't find skip list")

        /* Search the skip list and update the node */
        if(NULL != (node = H5SL_search(chunk_slist, &chunk_offset)))
            node->type_modified = TRUE;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Z_change_local_dtype_modify */


/*-------------------------------------------------------------------------
 * Function:	H5Z_evict_local_dtype_modify
 *
 * Purpose:	Search the node of the skip list for chunk and remove the
 *              node.  This function is called when the chunk is evicted
 *              from the cache.
 *
 * Return:	Success: Non-negative
 *		Failure: Negative
 *
 * Programmer:	Raymond Lu
 *              16 April 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Z_evict_local_dtype_modify(hid_t dcpl_id, hsize_t chunk_offset)
{
    H5P_genplist_t  *dc_plist = NULL;       /* New Property list */
    H5SL_t          *chunk_slist=NULL;      /* Skip list for keeping record of chunks */
    H5Z_slist_node_t *node;
    size_t          cd_nelmts=CD_NELMTS;            /* Number of filter parameters */
    unsigned        count = 0;              /* Keeps track of pointer size */ 
    unsigned cd_values[CD_NELMTS]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}; /* Filter parameters */
    uint8_t         *pp = NULL;
    herr_t ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5Z_evict_local_dtype_modify, FAIL)

    HDassert(H5I_GENPROP_LST==H5I_get_type(dcpl_id));

    /* Get the dataset's property list object */
    if(NULL == (dc_plist = (H5P_genplist_t *)H5I_object(dcpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't get dataset creation property list")

    if(H5P_get_filter_by_id(dc_plist, H5Z_FILTER_DTYPE_MODIFY, NULL, &cd_nelmts, cd_values, 
        (size_t)0, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't get filter info")

    /* If the skip list exists, remove the node for the chunk */
    count = cd_values[0];

    if(count >= 30 ) {
        pp = (uint8_t*)((unsigned *)cd_values + 2);

        /* Depending on the size of pointer, retrieve the value of the pointer to 
         * the skip list. */
        if (count == 30) {
            pp += 4;
            HDmemcpy(&chunk_slist, pp, 4);
        } else if (count > 30) {
            pp += 8;
            HDmemcpy(&chunk_slist, pp, 8);
        }

        if(!chunk_slist)
	    HGOTO_ERROR(H5E_SLIST, H5E_CANTGET, FAIL, "can't find skip list")

        /* Search the skip list and update the node */
        if(NULL != (node = H5SL_remove(chunk_slist, &chunk_offset))) {
            /* Check whether datatype is committed & decrement ref count
             * (to maintain ref. count incr/decr similarity with "shared message"
             * type of datatype sharing)
             */
            if(H5T_committed(node->dtype)) {
	        /* Decrement the reference count on the committed datatype */
	        if(H5T_link(node->dtype, -1, H5AC_dxpl_id) < 0)
	            HGOTO_ERROR(H5E_DATATYPE, H5E_LINKCOUNT, FAIL, "unable to adjust shared datatype link count")
            } /* end if */

            if(H5T_close(node->dtype) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CLOSEERROR, FAIL, "can't close data type")

            H5FL_FREE(H5Z_slist_node_t, node);
        }
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Z_evict_local_dtype_modify */


/*-------------------------------------------------------------------------
 * Function:	H5Z_delete_local_dtype_modify
 *
 * Purpose:	When the dataset is deleted, free the nodes of the skip list
 *              and delete it. Also decrement the reference counts of the 
 *              datatypes in B-tree nodes, free the nodes of the B-tree and 
 *              delete it. 
 *
 * Return:	Success: Non-negative
 *		Failure: Negative
 *
 * Programmer:	Raymond Lu
 *              11 August 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5Z_delete_local_dtype_modify(hid_t dcpl_id)
{
    H5P_genplist_t  *dc_plist = NULL;       /* New Property list */
    size_t          cd_nelmts=CD_NELMTS;    /* Number of filter parameters */
    unsigned        count = 0;              /* Keeps track of pointer size */ 
    unsigned cd_values[CD_NELMTS]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}; /* Filter parameters */
    uint8_t         *pp = NULL;
    H5F_t    *file = NULL;
    unsigned baddr_count = 0;       /* Keeps track of pointer size */ 
    haddr_t  baddr;                 /* B-tree address */
    herr_t ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5Z_delete_local_dtype_modify, FAIL)

    HDassert(H5I_GENPROP_LST==H5I_get_type(dcpl_id));

    /* Get the dataset's property list object */
    if(NULL == (dc_plist = (H5P_genplist_t *)H5I_object(dcpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't get dataset creation property list")

    if(H5P_get_filter_by_id(dc_plist, H5Z_FILTER_DTYPE_MODIFY, NULL, &cd_nelmts, cd_values, 
        (size_t)0, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't get filter info")

    count = cd_values[0];
    pp = (uint8_t*)((unsigned *)cd_values + 2);

    /* Depending on number of elements and the size of pointer, retrieve the value
     * of file pointer and skip list pointer. The first digit of CD_NELMTS indicates
     * the number of elements in CD_VALUES.  The second digit indicates whether the
     * pointers are 8 bytes.  0 stands for 4 bytes.  Greater than 0 means 8 bytes.
     */
    if (count == 20) {
        HDmemcpy(&file, pp, 4);
        pp += 4;
    } else if (20 < count && count < 30) {
        HDmemcpy(&file, pp, 8);
        pp += 8;
    } else if (count == 30) {
        HDmemcpy(&file, pp, 4);
        pp += 4;
        /* Advance over the skip list address */
        pp += 4;
    } else if (count > 30) {
        HDmemcpy(&file, pp, 8);
        pp += 8;
        /* Advance over the skip list address */
        pp += 8;
    } else
        HGOTO_ERROR(H5E_PLINE, H5E_CANTENCODE, 0, "not implemented yet")

    HDmemcpy(&baddr_count, pp, sizeof(unsigned));
    pp += 4;
    
    /* the B-tree address can be 8 or 16 byte in size */    
    if(baddr_count == 2) {
        HDmemcpy(&baddr, pp, 8);
    } else if(baddr_count == 4) {
        HDmemcpy(&baddr, pp, 16);
    }

    /* Delete the entire B-tree and decrement reference count of datatype */
    if(H5B2_delete(file, H5AC_dxpl_id, H5Z_BT2_DTYPE, baddr, H5Z_btree2_dtype_delete_cb, 
        NULL) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, FAIL, "can't delete B-tree")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Z_delete_local_dtype_modify() */


/*-------------------------------------------------------------------------
 * Function:	H5Z_close_local_dtype_modify
 *
 * Purpose:	When the dataset is closed, free the nodes of the skip list
 *              and delete it.  
 *
 * Return:	Success: Non-negative
 *		Failure: Negative
 *
 * Programmer:	Raymond Lu
 *              22 Feb 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Z_close_local_dtype_modify(hid_t dcpl_id)
{
    H5P_genplist_t  *dc_plist = NULL;       /* New Property list */
    H5SL_t          *chunk_slist=NULL;      /* Skip list for keeping record of chunks */
    size_t          cd_nelmts=CD_NELMTS;            /* Number of filter parameters */
    unsigned        count = 0;              /* Keeps track of pointer size */ 
    unsigned cd_values[CD_NELMTS]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}; /* Filter parameters */
    uint8_t         *pp = NULL;
    herr_t ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5Z_close_local_dtype_modify, FAIL)

    HDassert(H5I_GENPROP_LST==H5I_get_type(dcpl_id));

    /* Get the dataset's property list object */
    if(NULL == (dc_plist = (H5P_genplist_t *)H5I_object(dcpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't get dataset creation property list")

    if(H5P_get_filter_by_id(dc_plist, H5Z_FILTER_DTYPE_MODIFY, NULL, &cd_nelmts, cd_values, (size_t)0, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't get filter info")

    count = cd_values[0];
    pp = (uint8_t*)((unsigned *)cd_values + 2);

    /* Depending on the size of pointer, retrieve the value of the pointer to 
     * the skip list. */
    if (count == 30) {
        pp += 4;
        HDmemcpy(&chunk_slist, pp, 4);
    } else if (count > 30) {
        pp += 8;
        HDmemcpy(&chunk_slist, pp, 8);
    }

    /* Free the skip list */
    if(chunk_slist && H5SL_destroy(chunk_slist, H5Z_free_slist_node, NULL) < 0)
	HGOTO_ERROR(H5E_SLIST, H5E_CANTFREE, FAIL, "can't free skip list")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Z_close_local_dtype_modify() */

/*-------------------------------------------------------------------------
 * Function:    H5Z_free_slist_node
 *
 * Purpose:     Private function for H5SL_destroy.  Frees the nodes of 
 *              skip list.
 *
 * Return:      Non-negative on success, negative on failure
 *
 * Programmer:  Raymond Lu
 *              22 February 2008
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
static herr_t
H5Z_free_slist_node(void *item, void UNUSED *key, void UNUSED *operator_data/*in,out*/)
{
    H5T_t    *dtype = ((H5Z_slist_node_t*)item)->dtype;
    herr_t ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5Z_free_slist_node, FAIL)

    /* Check whether datatype is committed & decrement ref count
     * (to maintain ref. count incr/decr similarity with "shared message"
     * type of datatype sharing)
     */
    if(H5T_committed(dtype)) {
	/* Decrement the reference count on the committed datatype */
	if(H5T_link(dtype, -1, H5AC_dxpl_id) < 0)
	    HGOTO_ERROR(H5E_DATATYPE, H5E_LINKCOUNT, FAIL, "unable to adjust shared datatype link count")
    } /* end if */

    if(H5T_close(dtype) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CLOSEERROR, FAIL, "can't close data type")

    H5FL_FREE(H5Z_slist_node_t, item);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Z_free_slist_node */

/*-------------------------------------------------------------------------
 * Function:	H5Z_btree2_dtype_store
 *
 * Purpose:	Store user information into native record for v2 B-tree
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Raymond Lu
 *              5 August 2008
 *-------------------------------------------------------------------------
 */
static herr_t
H5Z_btree2_dtype_store(void *_nrecord, const void *_udata)
{
    const H5Z_mdtype_bt2_rec_t *udata = (const H5Z_mdtype_bt2_rec_t *)_udata;
    H5Z_mdtype_bt2_rec_t *nrecord = (H5Z_mdtype_bt2_rec_t *)_nrecord;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5Z_btree2_dtype_store, FAIL)

    /* Copy user information info native record */
    nrecord->version = udata->version;
    nrecord->is_shared = udata->is_shared;
    nrecord->numb_ref = udata->numb_ref;
   
    /* Make a copy of the datatype because Btree resides in the cache separately.
     * The dataset can be closed first. */ 
    if((nrecord->dtype = H5T_copy(udata->dtype, H5T_COPY_ALL)) == NULL)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTCOPY, FAIL, "can't copy datatype")

done:
    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5Z_btree2_dtype_store() */


/*-------------------------------------------------------------------------
 * Function:	H5Z_btree2_dtype_retrieve
 *
 * Purpose:	Retrieve native information from record for v2 B-tree
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Raymond Lu
 *              5 August 2008
 *-------------------------------------------------------------------------
 */
static herr_t
H5Z_btree2_dtype_retrieve(void *udata, const void *nrecord)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5Z_btree2_dtype_retrieve)

    *(H5Z_mdtype_bt2_rec_t *)udata = *(const H5Z_mdtype_bt2_rec_t *)nrecord;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5Z_btree2_dtype_retrieve() */


/*-------------------------------------------------------------------------
 * Function:	H5Z_btree2_dtype_encode
 *
 * Purpose:	Encode native information into raw form for storing on disk
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Raymond Lu
 *              5 August 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Z_btree2_dtype_encode(const H5F_t *f, uint8_t *raw, const void *_nrecord)
{
    const H5Z_mdtype_bt2_rec_t *nrecord = (const H5Z_mdtype_bt2_rec_t *)_nrecord;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5Z_btree2_dtype_encode, FAIL)

    /* Encode the record's fields */
    *raw++ = nrecord->version;

    /* Encode the flag indicating whether the dtype is shared in Btree node buffer */
    *raw++ = nrecord->is_shared;

    UINT64ENCODE(raw, nrecord->numb_ref);

    /* encode datatype info into Btree node buffer */
    if(H5O_msg_encode((H5F_t *)f, H5O_DTYPE_ID, FALSE, (unsigned char *)raw, 
        (const void *)nrecord->dtype)<0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTENCODE, FAIL, "can't encode object")

    if(H5T_close(nrecord->dtype) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CLOSEERROR, FAIL, "can't close datatype")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5Z_btree2_dtype_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5Z_btree2_dtype_decode
 *
 * Purpose:	Decode raw disk form of record into native form
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Raymond Lu
 *              5 August 2008
 *-------------------------------------------------------------------------
 */
static herr_t
H5Z_btree2_dtype_decode(const H5F_t *f, const uint8_t *raw, void *_nrecord)
{
    H5Z_mdtype_bt2_rec_t *nrecord = (H5Z_mdtype_bt2_rec_t *)_nrecord;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5Z_btree2_dtype_decode, FAIL)

    /* Decode the record's fields */
    nrecord->version = *raw++;
    nrecord->is_shared = *raw++;
    UINT64DECODE(raw, nrecord->numb_ref);

    /*
     * Decode the original datatype info from the chunk of data which might 
     * be shared or not shared.
     */
    if(nrecord->is_shared && (nrecord->dtype=(H5T_t *)H5O_msg_decode((H5F_t *)f, 
        H5AC_dxpl_id, H5O_DTYPE_ID, (const unsigned char *)raw, H5O_MSG_FLAG_SHARED))==NULL)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, FAIL, "can't decode object")
    else if(!nrecord->is_shared && (nrecord->dtype=(H5T_t *)H5O_msg_decode((H5F_t *)f, 
        H5AC_dxpl_id, H5O_DTYPE_ID, (const unsigned char *)raw, H5O_MSG_FLAG_WAS_UNKNOWN))==NULL)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, FAIL, "can't decode object")

done:
    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5Z_btree2_dtype_decode() */


/*-------------------------------------------------------------------------
 * Function:	H5Z_btree2_dtype_compare
 *
 * Purpose:	Compare two native information records, according to some key
 *
 * Return:	<0 if rec1 < rec2
 *              =0 if rec1 == rec2
 *              >0 if rec1 > rec2
 *
 * Programmer:	Raymond Lu
 *              5 August 2008
 *-------------------------------------------------------------------------
 */
static herr_t
H5Z_btree2_dtype_compare(const void *_bt2_udata, const void *_bt2_rec)
{
    const H5Z_mdtype_bt2_rec_t *bt2_udata = (const H5Z_mdtype_bt2_rec_t *)_bt2_udata;
    const H5Z_mdtype_bt2_rec_t *bt2_rec = (const H5Z_mdtype_bt2_rec_t *)_bt2_rec;
    herr_t ret_value;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5Z_btree2_dtype_compare)

    /* Sanity check */
    HDassert(bt2_udata);
    HDassert(bt2_rec);

    ret_value = H5T_cmp(bt2_udata->dtype, bt2_rec->dtype, FALSE);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5Z_btree2_dtype_compare() */


/*-------------------------------------------------------------------------
 * Function:	H5Z_btree2_dtype_find_cb
 *
 * Purpose:	Callback function for H5B2_find function.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Raymond Lu
 *              5 August 2008
 *-------------------------------------------------------------------------
 */
static herr_t 
H5Z_btree2_dtype_find_cb(void *record, void *op_data)
{
    H5Z_mdtype_bt2_rec_t *my_record = (H5Z_mdtype_bt2_rec_t *)record;
    H5Z_mdtype_bt2_rec_t *udata = (H5Z_mdtype_bt2_rec_t *)op_data;
    int compare;
    herr_t ret_value = FALSE;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5Z_btree2_dtype_find_cb)

    compare = H5T_cmp(my_record->dtype, udata->dtype, FALSE);

    if(compare == 0) {
        if(udata->adjust == H5Z_MDTYPE_INC)
            my_record->numb_ref++;
        else if(udata->adjust == H5Z_MDTYPE_DEC)
            my_record->numb_ref--;
       
        udata->numb_ref = my_record->numb_ref;
        udata->found = TRUE;
        ret_value = SUCCEED;
    }

    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Z_btree2_dtype_remove_cb
 *
 * Purpose:	Callback function for H5B2_remove function.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Raymond Lu
 *              5 August 2008
 *-------------------------------------------------------------------------
 */
static herr_t 
H5Z_btree2_dtype_remove_cb(void *record, void *op_data)
{
    H5Z_mdtype_bt2_rec_t *my_record = (H5Z_mdtype_bt2_rec_t *)record;
    herr_t ret_value = SUCCEED;           /* Return value */

    FUNC_ENTER_NOAPI(H5Z_btree2_dtype_remove_cb, FAIL)

    /* Close the datatype saved in Btree node */
    if(H5T_close(my_record->dtype) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CLOSEERROR, FAIL, "can't close datatype")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Z_btree2_dtype_delete_cb
 *
 * Purpose:	Callback function for H5B2_delete function.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Raymond Lu
 *              5 August 2008
 *-------------------------------------------------------------------------
 */
static herr_t 
H5Z_btree2_dtype_delete_cb(void *record, void *op_data)
{
    H5Z_mdtype_bt2_rec_t *my_record = (H5Z_mdtype_bt2_rec_t *)record;
    herr_t ret_value = SUCCEED;           /* Return value */

    FUNC_ENTER_NOAPI(H5Z_btree2_dtype_delete_cb, FAIL)

    /* Check whether datatype is committed & decrement ref count
     * (to maintain ref. count incr/decr similarity with "shared message"
     * type of datatype sharing)
     */
    if(H5T_committed(my_record->dtype)) {
	/* Decrement the reference count on the committed datatype */
	if(H5T_link(my_record->dtype, (int)-my_record->numb_ref, H5AC_dxpl_id) < 0)
	    HGOTO_ERROR(H5E_DATATYPE, H5E_LINKCOUNT, FAIL, "unable to adjust shared datatype link count")
    } /* end if */

    if(H5T_close(my_record->dtype) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CLOSEERROR, FAIL, "can't close datatype")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Z_filter_dtype_modify
 *
 * Purpose:	Implement the I/O filter of the datatype modification for 
 *              the dataset.  During the write, prepends the encoded current
 *              datatype info of the dataset to the chunked data.  During
 *              the read, decodes the datatype for the chunked data.  If 
 *              it's different from the current datatype of the dataset, 
 *              convert the data.
 *
 * Return:	Success: Size of buffer filtered
 *		Failure: 0
 *
 * Programmer:	Raymond Lu
 *              Aug 28, 2007
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
static size_t
H5Z_filter_dtype_modify (unsigned flags, hsize_t chunk_offset, size_t cd_nelmts, 
                     const unsigned cd_values[], size_t nbytes, size_t *buf_size, void **buf)
{
    unsigned char    *outbuf = NULL;     /* Pointer to new buffer */
    hid_t    dtype_id;
    H5T_t    *dtype = NULL, *orig_type = NULL;
    size_t   dtype_size, orig_size;
    size_t   type_msg_size;
    size_t   nelmts, dbuf_size;
    H5F_t    *file = NULL;
    H5SL_t   *chunk_slist=NULL;     /* Skip list for keeping record of chunks */
    H5Z_slist_node_t *node;
    uint8_t  *pp = NULL;
    unsigned char *dst=NULL;        /* Temporary pointer to destination buffer */
    unsigned count = 0;             /* Keeps track of pointer size */ 
    unsigned baddr_count = 0;       /* Keeps track of pointer size */ 
    haddr_t  baddr;                 /* B-tree address */
    int8_t   is_shared = FALSE;     /* Whether dtype is committed or shared */
    size_t   ret_value=nbytes;      /* Return value */

    FUNC_ENTER_NOAPI(H5Z_filter_dtype_modify, 0)

    count = cd_values[0];
   
    assert(cd_nelmts>=2 && cd_nelmts<=10); 
    assert(count>=20 && count<40);

    /* Get the dataset's datatype */
    dtype_id = cd_values[1];
    pp = (uint8_t*)((unsigned *)cd_values + 2);

    /* Depending on number of elements and the size of pointer, retrieve the value
     * of file pointer and skip list pointer. The first digit of CD_NELMTS indicates
     * the number of elements in CD_VALUES.  The second digit indicates whether the
     * pointers are 8 bytes.  0 stands for 4 bytes.  Greater than 0 means 8 bytes.
     */
    if (count == 20) {
        HDmemcpy(&file, pp, 4);
        pp += 4;
    } else if (20 < count && count < 30) {
        HDmemcpy(&file, pp, 8);
        pp += 8;
    } else if (count == 30) {
        HDmemcpy(&file, pp, 4);
        pp += 4;
        HDmemcpy(&chunk_slist, pp, 4);
        pp += 4;
    } else if (count > 30) {
        HDmemcpy(&file, pp, 8);
        pp += 8;
        HDmemcpy(&chunk_slist, pp, 8);
        pp += 8;
    } else
        HGOTO_ERROR(H5E_PLINE, H5E_CANTENCODE, 0, "not implemented yet")

    HDmemcpy(&baddr_count, pp, sizeof(unsigned));
    pp += 4;
    
    /* the B-tree address can be 8 or 16 byte in size */    
    if(baddr_count == 2) {
        HDmemcpy(&baddr, pp, 8);
    } else if(baddr_count == 4) {
        HDmemcpy(&baddr, pp, 16);
    }

    /* The current datatype of the dataset */
    if(NULL==(dtype=(H5T_t *)H5I_object_verify(dtype_id, H5I_DATATYPE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a datatype")

    if (flags & H5Z_FLAG_REVERSE) { /*Read*/
        uint8_t version = 0;
        hid_t   orig_type_id;
        size_t  info_size = 0;    /* the size of the encoded information except raw data */

        /* Retrieve and verify version number */
        HDmemcpy(&version, *buf, sizeof(uint8_t));

        if(version != H5Z_DTYPE_MODIFY_VERS)
	    HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, 0, "wrong version number")

        info_size = 1;

        /* Retrieve the flag indicating whether the dtype is committed or shared */
        outbuf = (unsigned char*)*buf + 1;
        HDmemcpy(&is_shared, outbuf, sizeof(int8_t));
        info_size += 1;
        outbuf += 1;

        /*
         * Decode the original datatype info from the chunk of data which might 
         * be shared or not shared.
         */
        if(is_shared && (orig_type=(H5T_t *)H5O_msg_decode(file, H5AC_dxpl_id, H5O_DTYPE_ID, 
            outbuf, H5O_MSG_FLAG_SHARED))==NULL)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, 0, "can't decode object")
        else if(!is_shared && (orig_type=(H5T_t *)H5O_msg_decode(file, H5AC_dxpl_id, 
            H5O_DTYPE_ID, outbuf, H5O_MSG_FLAG_WAS_UNKNOWN))==NULL)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, 0, "can't decode object")

        /* Get the encoding size of the original datatype */
        if((type_msg_size = H5O_msg_raw_size(file, H5O_DTYPE_ID, FALSE, orig_type))==0)
	    HGOTO_ERROR(H5E_DATATYPE, H5E_BADSIZE, 0, "can't find datatype size")

        info_size += type_msg_size;

        /* Calculate the size of buffer containing the new data */
        if(0 != H5T_cmp(dtype, orig_type, FALSE)) {
            if((dtype_size = H5T_get_size(dtype))==0)
	        HGOTO_ERROR(H5E_DATATYPE, H5E_BADSIZE, 0, "can't get datatype size")
            if((orig_size = H5T_get_size(orig_type))==0)
	        HGOTO_ERROR(H5E_DATATYPE, H5E_BADSIZE, 0, "can't get datatype size")

            nelmts = (nbytes-type_msg_size)/orig_size;
            if(dtype_size <= orig_size)
                dbuf_size = nbytes-info_size;
            else
                dbuf_size = nelmts*dtype_size;
        } else
            dbuf_size = nbytes-info_size;

        /* Allocate the buffer for the data */ 
	if (NULL==(dst=(unsigned char*)H5MM_malloc(dbuf_size)))
	    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "unable to allocate buffer")

        /* Copy the raw data */
        outbuf = (unsigned char*)*buf + info_size;
        HDmemcpy((void*)dst, (void*)outbuf, nbytes-info_size);

        /* Free input buffer */
 	H5MM_xfree(*buf);

        /* 
         * Convert the data if the current datatype is different from the 
         * original datatype.
         */
        if(0 != H5T_cmp(dtype, orig_type, FALSE)) {
            H5P_genplist_t *plist;              /* Property list pointer */
            htri_t      is_vlen = FALSE;        /* Flag to indicate VL type */
            hbool_t     vlen_conv = TRUE;       /* Transfer property to indicate no conversion for vlen */
            H5T_path_t		*tpath=NULL;	/*type conversion info	*/
            unsigned char       *bkg=NULL;      /* Pointer to background buffer */

	    /* If the datatype is or contains vlen, set the property to indicate no conversion 
	     * is needed for vlen. The file to memory conversion will take place at a 
             * higher level. */
	    if((is_vlen = H5T_detect_class(orig_type, H5T_VLEN)) == FAIL)
		HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, 0, "unable to detect dtatypes")

	    if(is_vlen ) {
		vlen_conv = FALSE;
		if(NULL == (plist = H5P_object_verify(H5AC_dxpl_id, H5P_DATASET_XFER)))
		    HGOTO_ERROR(H5E_PLIST, H5E_BADATOM, 0, "can't find object for ID")

		if(H5P_set(plist, H5D_XFER_VLEN_CONV_NAME, &vlen_conv) < 0)
		    HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, 0, "Error setting vlen conv flag")
	    }

            /* Find the conversion function */
            if (NULL==(tpath=H5T_path_find(orig_type, dtype, NULL, NULL, H5AC_dxpl_id, FALSE)))
	        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, 0, "unable to convert between src and dst data types");

            /* Register the original type and return the ID */
            if((orig_type_id = H5I_register(H5I_DATATYPE, orig_type)) < 0)
	        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, 0, "unable to register data type")

            if (H5T_path_bkg(tpath)) {
	        if (NULL==(bkg=(unsigned char*)H5MM_malloc(dbuf_size)))
	            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "unable to allocate buffer")
            }

            /* Convert the data */
            if (H5T_convert(tpath, orig_type_id, dtype_id, nelmts, (size_t)0, (size_t)0, dst, bkg, H5AC_dxpl_id)<0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, 0, "data type conversion failed");

	    /* Set the property of vlen conversion back to normal */
	    if(is_vlen ) {
		vlen_conv = TRUE;

		if(H5P_set(plist, H5D_XFER_VLEN_CONV_NAME, &vlen_conv) < 0)
		    HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, 0, "Error setting vlen conv flag")
	    }

            if(bkg)
                H5MM_xfree(bkg);
        } 

        /* Set return values */
        if(0 != H5T_cmp(dtype, orig_type, FALSE))
            *buf_size = nelmts*dtype_size;
        else
            *buf_size = nbytes - info_size;

	*buf = (void*)dst;
	dst = NULL;
        outbuf = NULL;
	ret_value = *buf_size;

        /* If the caller isn't from fractal heap (the flag H5Z_FLAG_SKIP_CRECORD isn't 
         * set) and the current chunk isn't in the skip list, put the current datatype 
         * for this chunk in.  When this datatype is being changed during the write 
         * call, the filter knows to decrement the reference count of the current type. 
         * The key of the node is the linear offset of the chunk in the dataspace.  This
         * linear offset is translated from the multi-dimensional offsets.
         */
        if(!(flags & H5Z_FLAG_SKIP_CRECORD) && chunk_slist) {
            /* Allocate space for the shared structure */
            if(NULL == (node = H5FL_MALLOC(H5Z_slist_node_t)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "memory allocation failed")
            node->key = chunk_offset;
 
            if((NULL == H5SL_search(chunk_slist, &(node->key))) && H5T_committed(orig_type)) {
 	        if ((NULL==(node->dtype=H5T_copy(orig_type, H5T_COPY_ALL))))
	            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, 0, "unable to copy data type");

                /* Check whether datatype is committed & increment ref count
                 * (to maintain ref. count incr/decr similarity with "shared message"
                 * type of datatype sharing)
                 */
                /* Increment the reference count on the committed datatype */
                if(H5T_link(node->dtype, 1, H5AC_dxpl_id) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_LINKCOUNT, 0, "unable to adjust shared datatype link count")
         
                /* set the flag for the later increment and decrement of reference count */ 
                if(0 != H5T_cmp(dtype, orig_type, FALSE))
                    node->type_modified = TRUE;
                else
                    node->type_modified = FALSE;

                if(H5SL_insert(chunk_slist, node, &(node->key)) < 0)
                    HGOTO_ERROR(H5E_SLIST, H5E_CANTINSERT, 0, "can't insert chunk node into skip list")
            }
        }

        /* Close the ID registered for doing data conversion */
        if(0 != H5T_cmp(dtype, orig_type, FALSE)) {
            if(H5Tclose(orig_type_id)<0)
	        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCLOSEOBJ, 0, "unable to close data type ID")
        }
    } else if (!(flags & H5Z_FLAG_INVMASK) || (flags & H5Z_FLAG_SKIP_CRECORD)) { /*Write*/
        uint8_t version = H5Z_DTYPE_MODIFY_VERS;
        size_t final_size = 0;
        htri_t is_shared_int;
        herr_t found = FAIL;
        H5Z_mdtype_bt2_rec_t udata;
        hsize_t bnode_numb = 0;
 
        /* Set the flag for committed or shared datatype */
        if(H5T_committed(dtype))
            is_shared = TRUE;

        /* Check if the datatype should be (or are already) shared in the SOHM table */
        if((is_shared_int = H5SM_try_share(file, H5AC_dxpl_id, NULL, H5O_DTYPE_ID, dtype, NULL)) < 0)
	    HGOTO_ERROR(H5E_DATATYPE, H5E_BADMESG, 0, "trying to share datatype failed")
        else if(is_shared_int)
            is_shared = TRUE;

        /* Get the encoding size of the dataset's datatype */
        if((type_msg_size = H5O_msg_raw_size(file, H5O_DTYPE_ID, FALSE, dtype))==0)
	    HGOTO_ERROR(H5E_DATATYPE, H5E_BADSIZE, 0, "can't find datatype size")

        /* Size of final chunk */
        final_size = 1                   /* Version number */
                   + 1                   /* Whether dtype is committed or shared */
                   + type_msg_size       /* Datatype message size */
                   + nbytes;             /* Raw data size */

        /* Allocate the buffer for the datatype info and the data */
	if (NULL==(dst=outbuf=(unsigned char*)H5MM_malloc(final_size)))
	    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "unable to allocate buffer")

        /* Encode version number */
        HDmemcpy(dst, &version, sizeof(uint8_t));
        dst += 1;

        /* Encode the flag indicating whether the dtype is shared in Btree node buffer */
        HDmemcpy(dst, &is_shared, sizeof(int8_t));
        dst += 1;

        /* prepend datatype info to raw data for storage as shared */
        if(H5O_msg_encode(file, H5O_DTYPE_ID, FALSE, dst, dtype)<0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, 0, "can't encode object")

        /* Copy raw data */
        dst += type_msg_size;
        HDmemcpy((void*)dst, (void*)(*buf), nbytes);

        /* Free input buffer */
 	H5MM_xfree(*buf);

        /* Set return values */
        *buf_size = final_size;
	*buf = (void*)outbuf;
	outbuf = NULL;
	ret_value = *buf_size;

        /* Adjust reference count for the old and new datatypes. */
        if(!(flags & H5Z_FLAG_SKIP_CRECORD) && chunk_slist) {
            /* Decrement the reference count of the old datatype and increment
             * the reference count of the new datatype if the chunk is in skip list. */
            if((NULL != (node = H5SL_search(chunk_slist, &chunk_offset)))
                && H5T_committed(node->dtype) && node->type_modified) {

		udata.version = H5Z_DTYPE_MODIFY_VERS;
		udata.is_shared = is_shared;
		udata.dtype = node->dtype;
		udata.numb_ref = 1;
		udata.found = FALSE;
                udata.adjust = H5Z_MDTYPE_DEC;

                /* Find out the number of nodes in the Btree */
                if(H5B2_get_nrec(file, H5AC_dxpl_id, H5Z_BT2_DTYPE, baddr, &bnode_numb) < 0)
                    HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, 0, "can't get number of nodes in Btree")

                /* Find the old datatype and decrement the number of references to the type */
                if(bnode_numb) 
                    H5B2_find(file, H5AC_dxpl_id, H5Z_BT2_DTYPE, baddr, &udata, 
                        H5Z_btree2_dtype_find_cb, &udata);

                /* If found the old type and no other chunk uses it, delete the node from the
                 * Btree */
                if(udata.found && !udata.numb_ref) {
                    /*delete the node*/
                    if(H5B2_remove(file, H5AC_dxpl_id, H5Z_BT2_DTYPE, baddr, &udata,
                        H5Z_btree2_dtype_remove_cb, NULL) < 0)
                        HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, 0, "can't remove node in Btree")

                    bnode_numb--;
                }

                /* Decrement the reference count of the old datatype in the skipped list.  
                 * 1 for being prepended to the chunk. 1 is for the copy in the skip list.*/
                if(H5T_link(node->dtype, -2, H5AC_dxpl_id) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_LINKCOUNT, 0, "unable to adjust shared datatype link count")

                if(H5T_close(node->dtype) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CLOSEERROR, 0, "can't close data type")

                /* Update the dtype in the skip list to the current dtype */
 	        if ((NULL==(node->dtype=H5T_copy(dtype, H5T_COPY_ALL))))
	            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, 0, "unable to copy data type");

                /* Increment the reference count on the new datatype. 1 is encoded with
                 * the chunk.  1 is the copy in the skip list */
                if(H5T_committed(node->dtype) && H5T_link(node->dtype, 2, H5AC_dxpl_id) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_LINKCOUNT, 0, "unable to adjust shared datatype link count")

                node->type_modified = FALSE;

                /* Update the node in the skip list? */

                /* If the dtype info isn't in Btree, insert it and its reference number 
                 * into Btree */
		udata.dtype = dtype;
		udata.numb_ref = 1;
		udata.found = FALSE;
                udata.adjust = H5Z_MDTYPE_INC;

                /* Find the new datatype in the Btree and increment the number of references 
                 * to the type */
                if(bnode_numb) 
                    H5B2_find(file, H5AC_dxpl_id, H5Z_BT2_DTYPE, baddr, &udata, 
                        H5Z_btree2_dtype_find_cb, &udata);

                if(udata.found == FALSE && H5B2_insert(file, H5AC_dxpl_id, H5Z_BT2_DTYPE, 
                    baddr, &udata) < 0)
	            HGOTO_ERROR(H5E_PLINE, H5E_CANTINSERT, 0, "can't insert B-tree")
            } else if(!node && H5T_committed(dtype)) {
                /* Insert the chunk in the skip list.  Increment type reference count. */
                /* Allocate space for the node for the skip list */
                if(NULL == (node = H5FL_MALLOC(H5Z_slist_node_t)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "memory allocation failed")
                node->key = chunk_offset;
                node->type_modified = FALSE;
 
 	        if ((NULL==(node->dtype=H5T_copy(dtype, H5T_COPY_ALL))))
	            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, 0, "unable to copy data type");

                /* Increment the reference count on the new datatype. 1 is encoded with
                 * the chunk.  1 is the copy in the skip list */
                if(H5T_link(dtype, 2, H5AC_dxpl_id) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_LINKCOUNT, 0, "unable to adjust shared datatype link count")

                /* Insert into the skip list */ 
                if(H5SL_insert(chunk_slist, node, &(node->key)) < 0)
                    HGOTO_ERROR(H5E_SLIST, H5E_CANTINSERT, 0, "can't insert chunk node into skip list")

		udata.version = H5Z_DTYPE_MODIFY_VERS;
		udata.is_shared = is_shared;
		udata.dtype = dtype;
		udata.numb_ref = 1;
		udata.found = FALSE;
                udata.adjust = H5Z_MDTYPE_INC;

                /* Find out the number of nodes in the Btree */
                if(H5B2_get_nrec(file, H5AC_dxpl_id, H5Z_BT2_DTYPE, baddr, &bnode_numb) < 0)
                    HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, 0, "can't get number of nodes in Btree")

                /* Find the datatype and increment the number of references to the type */
                if(bnode_numb) 
                    H5B2_find(file, H5AC_dxpl_id, H5Z_BT2_DTYPE, baddr, &udata, 
                        H5Z_btree2_dtype_find_cb, &udata);
                   /*HGOTO_ERROR(H5E_PLINE, H5E_NOTFOUND, 0, "can't locate dtype in Btree")*/

                /* If the dtype info isn't in Btree, insert it and its reference number 
                 * into Btree */
                if(udata.found == FALSE && H5B2_insert(file, H5AC_dxpl_id, H5Z_BT2_DTYPE, 
                    baddr, &udata) < 0)
	            HGOTO_ERROR(H5E_PLINE, H5E_CANTINSERT, 0, "can't insert B-tree")
            }
        } 
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)
}
#endif /* H5_HAVE_FILTER_DTYPE_MODIFY */
