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

/*-------------------------------------------------------------------------
 *
 * Created:		H5Gcache.c
 *			Feb  5 2008
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Implement group metadata cache methods.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5G_PACKAGE		/*suppress error about including H5Gpkg   */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5WBprivate.h"        /* Wrapped Buffers                      */


/****************/
/* Local Macros */
/****************/

#define H5G_NODE_VERS           1       /* Symbol table node version number   */
#define H5G_NODE_BUF_SIZE       512     /* Size of stack buffer for serialized nodes */


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Metadata cache (H5AC) callbacks */
static H5G_node_t *H5G_node_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_udata1,
				 void *_udata2);
static herr_t H5G_node_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr,
			     H5G_node_t *sym, unsigned UNUSED * flags_ptr);
static herr_t H5G_node_dest(H5F_t *f, H5G_node_t *sym);
static herr_t H5G_node_clear(H5F_t *f, H5G_node_t *sym, hbool_t destroy);
static herr_t H5G_node_size(const H5F_t *f, const H5G_node_t *sym, size_t *size_ptr);


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Symbol table nodes inherit cache-like properties from H5AC */
const H5AC_class_t H5AC_SNODE[1] = {{
    H5AC_SNODE_ID,
    (H5AC_load_func_t)H5G_node_load,
    (H5AC_flush_func_t)H5G_node_flush,
    (H5AC_dest_func_t)H5G_node_dest,
    (H5AC_clear_func_t)H5G_node_clear,
    (H5AC_size_func_t)H5G_node_size,
}};


/* Declare extern the free list to manage the H5G_node_t struct */
H5FL_EXTERN(H5G_node_t);

/* Declare extern the free list to manage sequences of H5G_entry_t's */
H5FL_SEQ_EXTERN(H5G_entry_t);


/*-------------------------------------------------------------------------
 * Function:	H5G_node_load
 *
 * Purpose:	Loads a symbol table node from the file.
 *
 * Return:	Success:	Ptr to the new table.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 23 1997
 *
 * Modifications:
 *		Robb Matzke, 1999-07-28
 *		The ADDR argument is passed by value.
 *
 *	Quincey Koziol, 2002-7-180
 *	Added dxpl parameter to allow more control over I/O from metadata
 *      cache.
 *-------------------------------------------------------------------------
 */
static H5G_node_t *
H5G_node_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void UNUSED  *_udata1,
	      void UNUSED * _udata2)
{
    H5G_node_t		   *sym = NULL;
    size_t		    size;
    H5WB_t                 *wb = NULL;     /* Wrapped buffer for node data */
    uint8_t                 node_buf[H5G_NODE_BUF_SIZE]; /* Buffer for node */
    uint8_t		   *node;           /* Pointer to node buffer */
    const uint8_t	   *p;
    H5G_node_t		   *ret_value;	/*for error handling */

    FUNC_ENTER_NOAPI_NOINIT(H5G_node_load)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(!_udata1);
    HDassert(NULL == _udata2);

    /*
     * Initialize variables.
     */

    /* Wrap the local buffer for serialized node info */
    if(NULL == (wb = H5WB_wrap(node_buf, sizeof(node_buf))))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't wrap buffer")

    /* Compute the size of the serialized symbol table node on disk */
    size = H5G_node_size_real(f);

    /* Get a pointer to a buffer that's large enough for node */
    if(NULL == (node = (uint8_t *)H5WB_actual(wb, size)))
        HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, NULL, "can't get actual buffer")

    /* Read the serialized symbol table node. */
    if(H5F_block_read(f, H5FD_MEM_BTREE, addr, size, dxpl_id, node) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_READERROR, NULL, "unable to read symbol table node")

    /* Get temporary pointer to serialized node */
    p = node;

    /* magic */
    if(HDmemcmp(p, H5G_NODE_MAGIC, (size_t)H5_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_SYM, H5E_CANTLOAD, NULL, "bad symbol table node signature")
    p += 4;

    /* version */
    if(H5G_NODE_VERS != *p++)
	HGOTO_ERROR(H5E_SYM, H5E_CANTLOAD, NULL, "bad symbol table node version")

    /* reserved */
    p++;

    /* Allocate symbol table data structures */
    if(NULL == (sym = H5FL_CALLOC(H5G_node_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    if(NULL == (sym->entry = H5FL_SEQ_CALLOC(H5G_entry_t, (size_t)(2 * H5F_SYM_LEAF_K(f)))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* number of symbols */
    UINT16DECODE(p, sym->nsyms);

    /* entries */
    if(H5G_ent_decode_vec(f, &p, sym->entry, sym->nsyms) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTLOAD, NULL, "unable to decode symbol table entries")

    /* Set return value */
    ret_value = sym;

done:
    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, NULL, "can't close wrapped buffer")
    if(!ret_value)
        if(sym && H5G_node_dest(f, sym) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTFREE, NULL, "unable to destroy symbol table node")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_node_load() */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_flush
 *
 * Purpose:	Flush a symbol table node to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jun 23 1997
 *
 * Modifications:
 *              rky, 1998-08-28
 *		Only p0 writes metadata to disk.
 *
 * 		Robb Matzke, 1999-07-28
 *		The ADDR argument is passed by value.
 *
 *	Quincey Koziol, 2002-7-180
 *	Added dxpl parameter to allow more control over I/O from metadata
 *      cache.
 *
 *      Pedro Vicente, <pvn@ncsa.uiuc.edu> 18 Sep 2002
 *      Added `id to name' support.
 *
 *      JRM -- 8/21/06
 *      Added the flags_ptr parameter.  This parameter exists to
 *      allow the flush routine to report to the cache if the
 *      entry is resized or renamed as a result of the flush.
 *      *flags_ptr is set to H5C_CALLBACK__NO_FLAGS_SET on entry.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_node_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5G_node_t *sym, unsigned UNUSED * flags_ptr)
{
    H5WB_t     *wb = NULL;     /* Wrapped buffer for node data */
    uint8_t     node_buf[H5G_NODE_BUF_SIZE]; /* Buffer for node */
    unsigned	u;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_node_flush)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(sym);

    /*
     * Look for dirty entries and set the node dirty flag.
     */
    for(u = 0; u < sym->nsyms; u++)
	if(sym->entry[u].dirty) {
            /* Set the node's dirty flag */
            sym->cache_info.is_dirty = TRUE;

            /* Reset the entry's dirty flag */
            sym->entry[u].dirty = FALSE;
        } /* end if */

    /*
     * Write the symbol node to disk.
     */
    if(sym->cache_info.is_dirty) {
        uint8_t	*node;          /* Pointer to node buffer */
        uint8_t	*p;             /* Pointer into raw data buffer */
        size_t	size;

        /* Wrap the local buffer for serialized node info */
        if(NULL == (wb = H5WB_wrap(node_buf, sizeof(node_buf))))
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't wrap buffer")

        /* Compute the size of the serialized symbol table node on disk */
        size = H5G_node_size_real(f);

        /* Get a pointer to a buffer that's large enough for node */
        if(NULL == (node = (uint8_t *)H5WB_actual(wb, size)))
            HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "can't get actual buffer")

        /* Get temporary pointer to serialized symbol table node */
        p = node;

        /* magic number */
        HDmemcpy(p, H5G_NODE_MAGIC, (size_t)H5_SIZEOF_MAGIC);
        p += 4;

        /* version number */
        *p++ = H5G_NODE_VERS;

        /* reserved */
        *p++ = 0;

        /* number of symbols */
        UINT16ENCODE(p, sym->nsyms);

        /* entries */
        if(H5G_ent_encode_vec(f, &p, sym->entry, sym->nsyms) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "can't serialize")
        HDmemset(p, 0, size - (p - node));

	/* Write the serialized symbol table node. */
        if(H5F_block_write(f, H5FD_MEM_BTREE, addr, size, dxpl_id, node) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_WRITEERROR, FAIL, "unable to write symbol table node to the file")

        /* Reset the node's dirty flag */
        sym->cache_info.is_dirty = FALSE;
    } /* end if */

    /*
     * Destroy the symbol node?	 This might happen if the node is being
     * preempted from the cache.
     */
    if(destroy)
        if(H5G_node_dest(f, sym) < 0)
	    HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to destroy symbol table node")

done:
    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "can't close wrapped buffer")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_node_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_dest
 *
 * Purpose:	Destroy a symbol table node in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Jan 15 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_node_dest(H5F_t *f, H5G_node_t *sym)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_node_dest)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(sym);

    /* Verify that node is clean */
    HDassert(sym->cache_info.is_dirty == FALSE);

    /* If we're going to free the space on disk, the address must be valid */
    HDassert(!sym->cache_info.free_file_space_on_destroy || H5F_addr_defined(sym->cache_info.addr));

    /* Check for freeing file space for symbol table node */
    if(sym->cache_info.free_file_space_on_destroy) {
        /* Release the space on disk */
        /* (XXX: Nasty usage of internal DXPL value! -QAK) */
        if(H5MF_xfree(f, H5FD_MEM_BTREE, H5AC_dxpl_id, sym->cache_info.addr, (hsize_t)H5G_node_size_real(f)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to free symbol table node")
    } /* end if */

    /* Release resources */
    if(sym->entry)
        sym->entry = (H5G_entry_t *)H5FL_SEQ_FREE(H5G_entry_t, sym->entry);
    (void)H5FL_FREE(H5G_node_t, sym);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_node_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_clear
 *
 * Purpose:	Mark a symbol table node in memory as non-dirty.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 20 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_node_clear(H5F_t *f, H5G_node_t *sym, hbool_t destroy)
{
    unsigned u;              /* Local index variable */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5G_node_clear);

    /*
     * Check arguments.
     */
    assert(sym);

    /* Look for dirty entries and reset their dirty flag.  */
    for(u = 0; u < sym->nsyms; u++)
        sym->entry[u].dirty=FALSE;
    sym->cache_info.is_dirty = FALSE;

    /*
     * Destroy the symbol node?	 This might happen if the node is being
     * preempted from the cache.
     */
    if (destroy)
        if (H5G_node_dest(f, sym) < 0)
	    HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to destroy symbol table node");

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5G_node_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5G_node_size
 *
 * Purpose:	Compute the size in bytes of the specified instance of
 *		H5G_node_t on disk, and return it in *size_ptr.  On failure
 *		the value of size_ptr is undefined.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer
 *		5/13/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_node_size(const H5F_t *f, const H5G_node_t UNUSED *sym, size_t *size_ptr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_node_size);

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(size_ptr);

    *size_ptr = H5G_node_size_real(f);

    FUNC_LEAVE_NOAPI(SUCCEED);
} /* H5G_node_size() */

