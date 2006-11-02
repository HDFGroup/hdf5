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

/*-------------------------------------------------------------------------
 *
 * Created:		H5Pfcpl.c
 *			January  6 1998
 *			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		File creation property list class routines
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/
#define H5P_PACKAGE		/*suppress error about including H5Ppkg	  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Bprivate.h"		/* B-tree subclass names	  	*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"		/* Files		  	*/
#include "H5Ppkg.h"		/* Property lists		  	*/


/****************/
/* Local Macros */
/****************/

/* ========= File Creation properties ============ */
/* Definitions for the size of the file user block in bytes */
#define H5F_CRT_USER_BLOCK_SIZE      sizeof(hsize_t)
#define H5F_CRT_USER_BLOCK_DEF       0
/* Definitions for the 1/2 rank for symbol table leaf nodes */
#define H5F_CRT_SYM_LEAF_SIZE        sizeof(unsigned)
#define H5F_CRT_SYM_LEAF_DEF         4
/* Definitions for the 1/2 rank for btree internal nodes    */
#define H5F_CRT_BTREE_RANK_SIZE      sizeof(unsigned[H5B_NUM_BTREE_ID])
#define H5F_CRT_BTREE_RANK_DEF       {HDF5_BTREE_SNODE_IK_DEF,HDF5_BTREE_ISTORE_IK_DEF}
/* Definitions for byte number in an address                */
#define H5F_CRT_ADDR_BYTE_NUM_SIZE   sizeof(size_t)
#define H5F_CRT_ADDR_BYTE_NUM_DEF    H5F_OBJ_ADDR_SIZE
/* Definitions for byte number for object size              */
#define H5F_CRT_OBJ_BYTE_NUM_SIZE     sizeof(size_t)
#define H5F_CRT_OBJ_BYTE_NUM_DEF      H5F_OBJ_SIZE_SIZE
/* Definitions for version number of the superblock         */
#define H5F_CRT_SUPER_VERS_SIZE       sizeof(unsigned)
#define H5F_CRT_SUPER_VERS_DEF        HDF5_SUPERBLOCK_VERSION_DEF
/* Definitions for free-space version number                */
#define H5F_CRT_FREESPACE_VERS_SIZE   sizeof(unsigned)
#define H5F_CRT_FREESPACE_VERS_DEF    HDF5_FREESPACE_VERSION
/* Definitions for object directory version number          */
#define H5F_CRT_OBJ_DIR_VERS_SIZE     sizeof(unsigned)
#define H5F_CRT_OBJ_DIR_VERS_DEF      HDF5_OBJECTDIR_VERSION
/* Definitions for shared-header format version             */
#define H5F_CRT_SHARE_HEAD_VERS_SIZE  sizeof(unsigned)
#define H5F_CRT_SHARE_HEAD_VERS_DEF   HDF5_SHAREDHEADER_VERSION


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Property class callbacks */
static herr_t H5P_fcrt_reg_prop(H5P_genclass_t *pclass);

/*********************/
/* Package Variables */
/*********************/

/* File creation property list class library initialization object */
const H5P_libclass_t H5P_CLS_FCRT[1] = {{
    "file create",		/* Class name for debugging     */
    &H5P_CLS_GROUP_CREATE_g,	/* Parent class ID              */
    &H5P_CLS_FILE_CREATE_g,	/* Pointer to class ID          */
    &H5P_LST_FILE_CREATE_g,	/* Pointer to default property list ID */
    H5P_fcrt_reg_prop,		/* Default property registration routine */
    NULL,		        /* Class creation callback      */
    NULL,		        /* Class creation callback info */
    NULL,			/* Class copy callback          */
    NULL,		        /* Class copy callback info     */
    NULL,			/* Class close callback         */
    NULL 		        /* Class close callback info    */
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5P_fcrt_reg_prop
 *
 * Purpose:     Register the file creation property list class's properties
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              October 31, 2006
 *-------------------------------------------------------------------------
 */
static herr_t
H5P_fcrt_reg_prop(H5P_genclass_t *pclass)
{
    hsize_t userblock_size = H5F_CRT_USER_BLOCK_DEF;    /* Default userblock size */
    unsigned sym_leaf_k = H5F_CRT_SYM_LEAF_DEF;         /* Default size for symbol table leaf nodes */
    unsigned btree_k[H5B_NUM_BTREE_ID] = H5F_CRT_BTREE_RANK_DEF;    /* Default 'K' values for B-trees in file */
    size_t sizeof_addr = H5F_CRT_ADDR_BYTE_NUM_DEF;     /* Default size of addresses in the file */
    size_t sizeof_size = H5F_CRT_OBJ_BYTE_NUM_DEF;      /* Default size of sizes in the file */
    unsigned superblock_ver = H5F_CRT_SUPER_VERS_DEF;   /* Default superblock version # */
    unsigned freespace_ver = H5F_CRT_FREESPACE_VERS_DEF;/* Default free space version # */
    unsigned objectdir_ver = H5F_CRT_OBJ_DIR_VERS_DEF;  /* Default object directory version # */
    unsigned sharedheader_ver = H5F_CRT_SHARE_HEAD_VERS_DEF;    /* Default shared header message version # */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5P_fcrt_reg_prop)

    /* Register the user block size */
    if(H5P_register(pclass, H5F_CRT_USER_BLOCK_NAME, H5F_CRT_USER_BLOCK_SIZE, &userblock_size, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the 1/2 rank for symbol table leaf nodes */
    if(H5P_register(pclass, H5F_CRT_SYM_LEAF_NAME, H5F_CRT_SYM_LEAF_SIZE, &sym_leaf_k, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the 1/2 rank for btree internal nodes */
    if(H5P_register(pclass, H5F_CRT_BTREE_RANK_NAME, H5F_CRT_BTREE_RANK_SIZE, btree_k, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the byte number for an address */
    if(H5P_register(pclass, H5F_CRT_ADDR_BYTE_NUM_NAME, H5F_CRT_ADDR_BYTE_NUM_SIZE, &sizeof_addr, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the byte number for object size */
    if(H5P_register(pclass, H5F_CRT_OBJ_BYTE_NUM_NAME, H5F_CRT_OBJ_BYTE_NUM_SIZE, &sizeof_size, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the superblock version number */
    if(H5P_register(pclass, H5F_CRT_SUPER_VERS_NAME, H5F_CRT_SUPER_VERS_SIZE, &superblock_ver, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the free-space version number */
    if(H5P_register(pclass, H5F_CRT_FREESPACE_VERS_NAME, H5F_CRT_FREESPACE_VERS_SIZE, &freespace_ver, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the object directory version number */
    if(H5P_register(pclass, H5F_CRT_OBJ_DIR_VERS_NAME, H5F_CRT_OBJ_DIR_VERS_SIZE, &objectdir_ver, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the shared-header version number */
    if(H5P_register(pclass, H5F_CRT_SHARE_HEAD_VERS_NAME, H5F_CRT_SHARE_HEAD_VERS_SIZE, &sharedheader_ver, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_fcrt_reg_prop() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_version
 *
 * Purpose:	Retrieves version information for various parts of a file.
 *
 *		SUPER:		The file super block.
 *		HEAP:		The global heap.
 *		FREELIST:	The global free list.
 *		STAB:		The root symbol table entry.
 *		SHHDR:		Shared object headers.
 *
 *		Any (or even all) of the output arguments can be null
 *		pointers.
 *
 * Return:	Success:	Non-negative, version information is returned
 *				through the arguments.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 * 		Raymond Lu, Oct 14, 2001
 * 		Change to the new generic property list.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_version(hid_t plist_id, unsigned *super/*out*/, unsigned *freelist/*out*/,
	       unsigned *stab/*out*/, unsigned *shhdr/*out*/)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_API(H5Pget_version, FAIL);
    H5TRACE5("e","ixxxx",plist_id,super,freelist,stab,shhdr);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id,H5P_FILE_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Get values */
    if (super)
        if(H5P_get(plist, H5F_CRT_SUPER_VERS_NAME, super) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get superblock version");
    if (freelist)
        if(H5P_get(plist, H5F_CRT_FREESPACE_VERS_NAME, freelist) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get free-space version");
    if (stab)
        if(H5P_get(plist, H5F_CRT_OBJ_DIR_VERS_NAME, stab) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get object directory version");
    if (shhdr)
        if(H5P_get(plist, H5F_CRT_SHARE_HEAD_VERS_NAME, shhdr) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get shared-header version");

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_userblock
 *
 * Purpose:	Sets the userblock size field of a file creation property
 *		list.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Tuesday, January  6, 1998
 *
 * Modifications:
 *
 *		Raymond Lu, Oct 14, 2001
 * 		Changed to the new generic property list.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_userblock(hid_t plist_id, hsize_t size)
{
    unsigned		    i;
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER_API(H5Pset_userblock, FAIL);
    H5TRACE2("e","ih",plist_id,size);

    /* Check that the userblock size is a power of two */
    for (i=8; i<8*sizeof(hsize_t); i++) {
        hsize_t p2 = 8==i ? 0 : ((hsize_t)1<<i);

        if (size == p2)
            break;
    }
    if (i>=8*sizeof(hsize_t))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "userblock size is not valid");

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id,H5P_FILE_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Set value */
    if(H5P_set(plist, H5F_CRT_USER_BLOCK_NAME, &size) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set user block");

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_userblock
 *
 * Purpose:	Queries the size of a user block in a file creation property
 *		list.
 *
 * Return:	Success:	Non-negative, size returned through SIZE argument.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *		Raymond Lu, Oct 14, 2001
 *		Changed to the new generic property list.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_userblock(hid_t plist_id, hsize_t *size)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER_API(H5Pget_userblock, FAIL);
    H5TRACE2("e","i*h",plist_id,size);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id,H5P_FILE_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Get value */
    if (size)
        if(H5P_get(plist, H5F_CRT_USER_BLOCK_NAME, size) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL,"can't get user block");

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_sizes
 *
 * Purpose:	Sets file size-of addresses and sizes.	PLIST_ID should be a
 *		file creation property list.  A value of zero causes the
 *		property to not change.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Tuesday, January  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_sizes(hid_t plist_id, size_t sizeof_addr, size_t sizeof_size)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER_API(H5Pset_sizes, FAIL);
    H5TRACE3("e","izz",plist_id,sizeof_addr,sizeof_size);

    /* Check arguments */
    if (sizeof_addr) {
        if (sizeof_addr != 2 && sizeof_addr != 4 &&
                sizeof_addr != 8 && sizeof_addr != 16)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file haddr_t size is not valid");
    }
    if (sizeof_size) {
        if (sizeof_size != 2 && sizeof_size != 4 &&
                sizeof_size != 8 && sizeof_size != 16)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file size_t size is not valid");
    }

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id,H5P_FILE_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Set value */
    if (sizeof_addr)
        if(H5P_set(plist, H5F_CRT_ADDR_BYTE_NUM_NAME, &sizeof_addr) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set byte number for an address");
    if (sizeof_size)
        if(H5P_set(plist, H5F_CRT_OBJ_BYTE_NUM_NAME, &sizeof_size) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set byte number for object ");

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_sizes
 *
 * Purpose:	Returns the size of address and size quantities stored in a
 *		file according to a file creation property list.  Either (or
 *		even both) SIZEOF_ADDR and SIZEOF_SIZE may be null pointers.
 *
 * Return:	Success:	Non-negative, sizes returned through arguments.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_sizes(hid_t plist_id,
	     size_t *sizeof_addr /*out */ , size_t *sizeof_size /*out */ )
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER_API(H5Pget_sizes, FAIL);
    H5TRACE3("e","ixx",plist_id,sizeof_addr,sizeof_size);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id,H5P_FILE_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Get values */
    if (sizeof_addr)
        if(H5P_get(plist, H5F_CRT_ADDR_BYTE_NUM_NAME, sizeof_addr) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get byte number for an address");
    if (sizeof_size)
        if(H5P_get(plist, H5F_CRT_OBJ_BYTE_NUM_NAME, sizeof_size) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get byte number for object ");

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_sym_k
 *
 * Purpose:	IK is one half the rank of a tree that stores a symbol
 *		table for a group.  Internal nodes of the symbol table are on
 *		average 75% full.  That is, the average rank of the tree is
 *		1.5 times the value of IK.
 *
 *		LK is one half of the number of symbols that can be stored in
 *		a symbol table node.  A symbol table node is the leaf of a
 *		symbol table tree which is used to store a group.  When
 *		symbols are inserted randomly into a group, the group's
 *		symbol table nodes are 75% full on average.  That is, they
 *		contain 1.5 times the number of symbols specified by LK.
 *
 *		Either (or even both) of IK and LK can be zero in which case
 *		that value is left unchanged.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Tuesday, January  6, 1998
 *
 * Modifications:
 *
 *		Raymond Lu, Oct 14, 2001
 *         	Changed to the new generic property list.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_sym_k(hid_t plist_id, unsigned ik, unsigned lk)
{
    unsigned btree_k[H5B_NUM_BTREE_ID];
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Pset_sym_k, FAIL);
    H5TRACE3("e","iIuIu",plist_id,ik,lk);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id,H5P_FILE_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Set values */
    if (ik > 0) {
        if(H5P_get(plist, H5F_CRT_BTREE_RANK_NAME, btree_k) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get rank for btree interanl nodes");
        btree_k[H5B_SNODE_ID] = ik;
        if(H5P_set(plist, H5F_CRT_BTREE_RANK_NAME, btree_k) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set rank for btree nodes");
    }
    if (lk > 0)
        if(H5P_set(plist, H5F_CRT_SYM_LEAF_NAME, &lk) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set rank for symbol table leaf nodes");

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_sym_k
 *
 * Purpose:	Retrieves the symbol table B-tree 1/2 rank (IK) and the
 *		symbol table leaf node 1/2 size (LK).  See H5Pset_sym_k() for
 *		details. Either (or even both) IK and LK may be null
 *		pointers.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *		Raymond Lu
 *		Changed to the new generic property list.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_sym_k(hid_t plist_id, unsigned *ik /*out */ , unsigned *lk /*out */ )
{
    unsigned btree_k[H5B_NUM_BTREE_ID];
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Pget_sym_k, FAIL);
    H5TRACE3("e","ixx",plist_id,ik,lk);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id,H5P_FILE_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Get values */
    if (ik) {
        if(H5P_get(plist, H5F_CRT_BTREE_RANK_NAME, btree_k) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get rank for btree nodes");
        *ik = btree_k[H5B_SNODE_ID];
    }
    if (lk)
        if(H5P_get(plist, H5F_CRT_SYM_LEAF_NAME, lk) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get rank for symbol table leaf nodes");

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_istore_k
 *
 * Purpose:	IK is one half the rank of a tree that stores chunked raw
 *		data.  On average, such a tree will be 75% full, or have an
 *		average rank of 1.5 times the value of IK.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Tuesday, January  6, 1998
 *
 * Modifications:
 *
 *		Raymond Lu, Oct 14, 2001
 *		Changed to the new generic property list.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_istore_k(hid_t plist_id, unsigned ik)
{
    unsigned btree_k[H5B_NUM_BTREE_ID];
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Pset_istore_k, FAIL);
    H5TRACE2("e","iIu",plist_id,ik);

    /* Check arguments */
    if (ik == 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "istore IK value must be positive");

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id,H5P_FILE_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Set value */
    if(H5P_get(plist, H5F_CRT_BTREE_RANK_NAME, btree_k) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get rank for btree interanl nodes");
    btree_k[H5B_ISTORE_ID] = ik;
    if(H5P_set(plist, H5F_CRT_BTREE_RANK_NAME, btree_k) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set rank for btree interanl nodes");

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_istore_k
 *
 * Purpose:	Queries the 1/2 rank of an indexed storage B-tree.  See
 *		H5Pset_istore_k() for details.	The argument IK may be the
 *		null pointer.
 *
 * Return:	Success:	Non-negative, size returned through IK
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *		Raymond Lu, Oct 14, 2001
 *		Changed to the new generic property list.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_istore_k(hid_t plist_id, unsigned *ik /*out */ )
{
    unsigned btree_k[H5B_NUM_BTREE_ID];
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER_API(H5Pget_istore_k, FAIL);
    H5TRACE2("e","ix",plist_id,ik);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id,H5P_FILE_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Get value */
    if (ik) {
        if(H5P_get(plist, H5F_CRT_BTREE_RANK_NAME, btree_k) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get rank for btree interanl nodes");
        *ik = btree_k[H5B_ISTORE_ID];
    }

done:
    FUNC_LEAVE_API(ret_value);
}

