/****************************************************************************
* NCSA HDF								   *
* Software Development Group						   *
* National Center for Supercomputing Applications			   *
* University of Illinois at Urbana-Champaign				   *
* 605 E. Springfield, Champaign IL 61820				   *
*									   *
* For conditions of distribution and use, see the accompanying		   *
* hdf/COPYING file.							   *
*									   *
****************************************************************************/

#ifdef RCSID
static char		RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

#include <stdarg.h>

/* Private header files */
#include <H5private.h>		/* Generic Functions			*/
#include <H5Iprivate.h>		/* IDs			  	*/
#include <H5Bprivate.h>		/* B-tree subclass names	  	*/
#include <H5Dprivate.h>		/* Datasets				*/
#include <H5Eprivate.h>		/* Error handling		  	*/
#include <H5MMprivate.h>	/* Memory management			*/
#include <H5Pprivate.h>		/* Property lists		  	*/

#define PABLO_MASK	H5P_mask

/* Is the interface initialized? */
static hbool_t		interface_initialize_g = FALSE;
#define INTERFACE_INIT H5P_init_interface
static herr_t		H5P_init_interface(void);

/* PRIVATE PROTOTYPES */
static void		H5P_term_interface(void);

/*--------------------------------------------------------------------------
NAME
   H5P_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5P_init_interface()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
static herr_t
H5P_init_interface(void)
{
    herr_t		    ret_value = SUCCEED;
    intn		    i;
    herr_t		    status;

    FUNC_ENTER(H5P_init_interface, FAIL);

    /*
     * Make sure the file creation and file access default property lists are
     * initialized since this might be done at run-time instead of compile
     * time.
     */
    if (H5F_init_interface ()<0) {
	HRETURN_ERROR (H5E_INTERNAL, H5E_CANTINIT, FAIL,
		       "unable to initialize H5F and H5P interfaces");
    }

    assert(H5P_NCLASSES <= H5_TEMPLATE_MAX - H5_TEMPLATE_0);

    /*
     * Initialize the mappings between property list classes and atom
     * groups. We keep the two separate because property list classes are
     * publicly visible but atom groups aren't.
     */
    for (i = 0; i < H5P_NCLASSES; i++) {
	status = H5I_init_group((H5I_group_t)(H5_TEMPLATE_0 +i),
				H5I_TEMPID_HASHSIZE, 0, NULL);
	if (status < 0) ret_value = FAIL;
    }
    if (ret_value < 0) {
	HRETURN_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL,
		      "unable to initialize atom group");
    }
    
    /*
     * Register cleanup function.
     */
    if (H5_add_exit(H5P_term_interface) < 0) {
	HRETURN_ERROR(H5E_INTERNAL, H5E_CANTINIT, FAIL,
		      "unable to install atexit function");
    }
    
    FUNC_LEAVE(ret_value);
}

/*--------------------------------------------------------------------------
 NAME
    H5P_term_interface
 PURPOSE
    Terminate various H5P objects
 USAGE
    void H5P_term_interface()
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Release the atom group and any other resources allocated.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static void
H5P_term_interface(void)
{
    intn		    i;

    for (i = 0; i < H5P_NCLASSES; i++) {
	H5I_destroy_group((H5I_group_t)(H5_TEMPLATE_0 + i));
    }
}

/*--------------------------------------------------------------------------
 NAME
    H5Pcreate
 PURPOSE
    Returns a copy of the default property list for some class of property
 *  lists.
 USAGE
    herr_t H5Pcreate (type)
	H5P_class_t type;	IN: Property list class whose default is
 *				    desired.
 RETURNS
    Property list ID or FAIL
 
 ERRORS
    ARGS      BADVALUE	    Unknown property list class. 
    ATOM      CANTINIT	    Can't register property list. 
    INTERNAL  UNSUPPORTED   Not implemented yet. 

 DESCRIPTION
    Returns a copy of the default property list for some class of property
 *  lists.
--------------------------------------------------------------------------*/
hid_t
H5Pcreate (H5P_class_t type)
{
    hid_t		    ret_value = FAIL;
    void		   *tmpl = NULL;

    FUNC_ENTER(H5Pcreate, FAIL);
    H5TRACE1("i","p",type);

    /* Allocate a new property list and initialize it with default values */
    switch (type) {
    case H5P_FILE_CREATE:
	tmpl = H5MM_xmalloc(sizeof(H5F_create_t));
	memcpy(tmpl, &H5F_create_dflt, sizeof(H5F_create_t));
	break;

    case H5P_FILE_ACCESS:
	tmpl = H5MM_xmalloc(sizeof(H5F_access_t));
	memcpy(tmpl, &H5F_access_dflt, sizeof(H5F_access_t));
	break;

    case H5P_DATASET_CREATE:
	tmpl = H5MM_xmalloc(sizeof(H5D_create_t));
	memcpy(tmpl, &H5D_create_dflt, sizeof(H5D_create_t));
	break;

    case H5P_DATASET_XFER:
	tmpl = H5MM_xmalloc(sizeof(H5D_xfer_t));
	memcpy(tmpl, &H5D_xfer_dflt, sizeof(H5D_xfer_t));
	break;

    default:
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		      "unknown property list class");
    }

    /* Atomize the new property list */
    if ((ret_value = H5P_create(type, tmpl)) < 0) {
	HRETURN_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL,
		      "unable to register property list");
    }
    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5P_create
 *
 * Purpose:	Given a pointer to some property list struct, atomize the
 *		property list and return its ID. The property list memory is
 *		not copied, so the caller should not free it; it will be
 *		freed by H5P_release().
 *
 * Return:	Success:	A new property list ID.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, December  3, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5P_create(H5P_class_t type, void *tmpl)
{
    hid_t	ret_value = FAIL;

    FUNC_ENTER(H5P_create, FAIL);

    /* check args */
    assert(type >= 0 && type < H5P_NCLASSES);
    assert(tmpl);

    /* Atomize the new property list */
    if ((ret_value=H5I_register((H5I_group_t)(H5_TEMPLATE_0+type), tmpl)) < 0) {
	HRETURN_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL,
		      "unable to register property list");
    }
    
    FUNC_LEAVE(ret_value);
}

/*--------------------------------------------------------------------------
 NAME
    H5Pclose
 PURPOSE
    Release access to a property list object.
 USAGE
    herr_t H5Pclose(oid)
	hid_t oid;	 IN: property list object to release access to
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
	This function releases access to a property list object
--------------------------------------------------------------------------*/
herr_t
H5Pclose (hid_t tid)
{
    H5P_class_t		type;
    void		*tmpl = NULL;

    FUNC_ENTER(H5Pclose, FAIL);
    H5TRACE1("e","i",tid);

    /* Check arguments */
    if ((type=H5Pget_class (tid))<0 ||
	NULL==(tmpl=H5I_object (tid))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
    }
	
    /*
     * Chuck the object!  When the reference count reaches zero then
     * H5I_dec_ref() removes it from the group and we should free it.  The
     * free function is not registered as part of the group because it takes
     * an extra argument.
     */
    if (0==H5I_dec_ref(tid)) H5P_close (type, tmpl);

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5P_close
 *
 * Purpose:	Closes a property list and frees the memory associated with
 *		the property list.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, February 18, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5P_close (H5P_class_t type, void *tmpl)
{
    H5F_access_t	*fa_list = (H5F_access_t*)tmpl;
    H5D_create_t	*dc_list = (H5D_create_t*)tmpl;
    
    FUNC_ENTER (H5P_close, FAIL);

    /* Check args */
    if (!tmpl) HRETURN (SUCCEED);

    /* Some property lists may need to do special things */
    switch (type) {
    case H5P_FILE_ACCESS:
	switch (fa_list->driver) {
	case H5F_LOW_ERROR:
	case H5F_LOW_SEC2:
	case H5F_LOW_STDIO:
	case H5F_LOW_CORE:
	    /* Nothing to do */
	    break;

	case H5F_LOW_MPIO:
#ifdef LATER
	    /* Need to free the COMM and INFO objects too. */
#endif
	    break;

	case H5F_LOW_SPLIT:
	    /* Free member info */
	    fa_list->driver = H5F_LOW_ERROR; /*prevent cycles*/
	    H5P_close (H5P_FILE_ACCESS, fa_list->u.split.meta_access);
	    H5P_close (H5P_FILE_ACCESS, fa_list->u.split.raw_access);
	    H5MM_xfree (fa_list->u.split.meta_ext);
	    H5MM_xfree (fa_list->u.split.raw_ext);
	    break;

	case H5F_LOW_FAMILY:
	    /* Free member info */
	    H5P_close (H5P_FILE_ACCESS, fa_list->u.fam.memb_access);
	    break;
	}
	break;
	
    case H5P_FILE_CREATE:
	/*nothing to do*/
	break;
	
    case H5P_DATASET_CREATE:
	H5O_reset (H5O_EFL, &(dc_list->efl));
	break;

    case H5P_DATASET_XFER:
	/*nothing to do*/
	break;

    default:
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "unknown property list class");
    }

    /* Free the property list struct and return */
    H5MM_xfree(tmpl);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_class
 *
 * Purpose:	Returns the class identifier for a property list.
 *
 * Return:	Success:	A property list class
 *
 *		Failure:	H5P_NO_CLASS (-1)
 *
 * Programmer:	Robb Matzke
 *		Wednesday, December  3, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5P_class_t
H5Pget_class(hid_t tid)
{
    H5I_group_t		    group;
    H5P_class_t		    ret_value = H5P_NO_CLASS;

    FUNC_ENTER(H5Pget_class, H5P_NO_CLASS);

    if ((group = H5I_group(tid)) < 0 ||
#ifndef NDEBUG
	group >= H5_TEMPLATE_MAX ||
#endif
	group < H5_TEMPLATE_0) {
	HRETURN_ERROR(H5E_ATOM, H5E_BADATOM, H5P_NO_CLASS,
		      "not a property list");
    }
    ret_value = (H5P_class_t)(group - H5_TEMPLATE_0);
    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5Pget_version
 *
 * Purpose:	Retrieves version information for various parts of a file.
 *
 *		BOOT:		The file boot block.
 *		HEAP:		The global heap.
 *		FREELIST:	The global free list.
 *		STAB:		The root symbol table entry.
 *		SHHDR:		Shared object headers.
 *
 *		Any (or even all) of the output arguments can be null
 *		pointers.
 *
 * Return:	Success:	SUCCEED, version information is returned
 *				through the arguments.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_version (hid_t tid, int *boot/*out*/, int *freelist/*out*/,
	       int *stab/*out*/, int *shhdr/*out*/)
{
    H5F_create_t	   *tmpl = NULL;

    FUNC_ENTER(H5Pget_version, FAIL);
    H5TRACE1("e","i",tid);

    /* Check arguments */
    if (H5P_FILE_CREATE != H5Pget_class(tid) ||
	NULL == (tmpl = H5I_object(tid))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file creation property list");
    }
    /* Get values */
    if (boot) *boot = tmpl->bootblock_ver;
    if (freelist) *freelist = tmpl->freespace_ver;
    if (stab) *stab = tmpl->objectdir_ver;
    if (shhdr) *shhdr = tmpl->sharedheader_ver;

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5Pset_userblock
 *
 * Purpose:	Sets the userblock size field of a file creation property
 *		list.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, January  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_userblock (hid_t tid, hsize_t size)
{
    uintn		    i;
    H5F_create_t	   *tmpl = NULL;

    FUNC_ENTER(H5Pset_userblock, FAIL);
    H5TRACE2("e","ih",tid,size);

    /* Check arguments */
    if (H5P_FILE_CREATE != H5Pget_class(tid) ||
	NULL == (tmpl = H5I_object(tid))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file creation property list");
    }
    for (i=8; i<8*sizeof(hsize_t); i++) {
	hsize_t p2 = 8==i ? 0 : ((hsize_t)1<<i);
	if (size == p2) break;
    }
    if (i>=8*sizeof(hsize_t)) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		      "userblock size is not valid");
    }
    /* Set value */
    tmpl->userblock_size = size;

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5Pget_userblock
 *
 * Purpose:	Queries the size of a user block in a file creation property
 *		list.
 *
 * Return:	Success:	SUCCEED, size returned through SIZE argument.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_userblock (hid_t tid, hsize_t *size)
{
    H5F_create_t	*tmpl = NULL;

    FUNC_ENTER(H5Pget_userblock, FAIL);
    H5TRACE2("e","i*h",tid,size);

    /* Check args */
    if (H5P_FILE_CREATE != H5Pget_class(tid) ||
	NULL == (tmpl = H5I_object(tid))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file creation property list");
    }
    /* Get value */
    if (size) *size = tmpl->userblock_size;

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_alignment
 *
 * Purpose:	Sets the alignment properties of a file access property list
 *		so that any file object >= THRESHOLD bytes will be aligned on
 *		an address which is a multiple of ALIGNMENT.  The addresses
 *		are relative to the end of the user block; the alignment is
 *		calculated by subtracting the user block size from the
 *		absolute file address and then adjusting the address to be a
 *		multiple of ALIGNMENT.
 *
 *		Default values for THRESHOLD and ALIGNMENT are one, implying
 *		no alignment.  Generally the default values will result in
 *		the best performance for single-process access to the file.
 *		For MPI-IO and other parallel systems, choose an alignment
 *		which is a multiple of the disk block size.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, June  9, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_alignment (hid_t fapl_id, hsize_t threshold, hsize_t alignment)
{
    H5F_access_t	*fapl = NULL;
    
    FUNC_ENTER (H5Pset_alignment, FAIL);
    H5TRACE3("e","ihh",fapl_id,threshold,alignment);

    /* Check args */
    if (H5P_FILE_ACCESS != H5Pget_class (fapl_id) ||
	NULL == (fapl = H5I_object (fapl_id))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "not a file access property list");
    }
    if (alignment<1) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "alignment must be positive");
    }

    /* Set values */
    fapl->threshold = threshold;
    fapl->alignment = alignment;

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_alignment
 *
 * Purpose:	Returns the current settings for alignment properties from a
 *		file access property list.  The THRESHOLD and/or ALIGNMENT
 *		pointers may be null pointers.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, June  9, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_alignment (hid_t fapl_id, hsize_t *threshold/*out*/,
		  hsize_t *alignment/*out*/)
{
    H5F_access_t	*fapl = NULL;

    FUNC_ENTER (H5Pget_alignment, FAIL);
    H5TRACE1("e","i",fapl_id);

    /* Check args */
    if (H5P_FILE_ACCESS != H5Pget_class (fapl_id) ||
	NULL == (fapl = H5I_object (fapl_id))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "not a file access property list");
    }

    /* Get values */
    if (threshold) *threshold = fapl->threshold;
    if (alignment) *alignment = fapl->alignment;

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_sizes
 *
 * Purpose:	Sets file size-of addresses and sizes.	TID should be a file
 *		creation property list.  A value of zero causes the property
 *		to not change.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, January  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_sizes (hid_t tid, size_t sizeof_addr, size_t sizeof_size)
{
    H5F_create_t	   *tmpl = NULL;

    FUNC_ENTER(H5Pset_sizeof_addr, FAIL);
    H5TRACE3("e","izz",tid,sizeof_addr,sizeof_size);

    /* Check arguments */
    if (H5P_FILE_CREATE != H5Pget_class(tid) ||
	NULL == (tmpl = H5I_object(tid))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file creation property list");
    }
    if (sizeof_addr) {
	if (sizeof_addr != 2 && sizeof_addr != 4 &&
	    sizeof_addr != 8 && sizeof_addr != 16) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
			  "file haddr_t size is not valid");
	}
    }
    if (sizeof_size) {
	if (sizeof_size != 2 && sizeof_size != 4 &&
	    sizeof_size != 8 && sizeof_size != 16) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
			  "file size_t size is not valid");
	}
    }
    /* Set value */
    if (sizeof_addr)
	tmpl->sizeof_addr = sizeof_addr;
    if (sizeof_size)
	tmpl->sizeof_size = sizeof_size;

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5Pget_sizes
 *
 * Purpose:	Returns the size of address and size quantities stored in a
 *		file according to a file creation property list.  Either (or
 *		even both) SIZEOF_ADDR and SIZEOF_SIZE may be null pointers.
 *
 * Return:	Success:	SUCCEED, sizes returned through arguments.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_sizes (hid_t tid,
	     size_t *sizeof_addr /*out */ , size_t *sizeof_size /*out */ )
{
    H5F_create_t	   *tmpl = NULL;

    FUNC_ENTER(H5Pget_sizes, FAIL);
    H5TRACE1("e","i",tid);

    /* Check args */
    if (H5P_FILE_CREATE != H5Pget_class(tid) ||
	NULL == (tmpl = H5I_object(tid))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file creation property list");
    }
    /* Get values */
    if (sizeof_addr)
	*sizeof_addr = tmpl->sizeof_addr;
    if (sizeof_size)
	*sizeof_size = tmpl->sizeof_size;

    FUNC_LEAVE(SUCCEED);
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
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, January  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_sym_k (hid_t tid, int ik, int lk)
{
    H5F_create_t	   *tmpl = NULL;

    FUNC_ENTER(H5Pset_sym_k, FAIL);
    H5TRACE3("e","iIsIs",tid,ik,lk);

    /* Check arguments */
    if (H5P_FILE_CREATE != H5Pget_class(tid) ||
	NULL == (tmpl = H5I_object(tid))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file creation property list");
    }
    /* Set values */
    if (ik > 0) {
	tmpl->btree_k[H5B_SNODE_ID] = ik;
    }
    if (lk > 0) {
	tmpl->sym_leaf_k = lk;
    }
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5Pget_sym_k
 *
 * Purpose:	Retrieves the symbol table B-tree 1/2 rank (IK) and the
 *		symbol table leaf node 1/2 size (LK).  See H5Pset_sym_k() for
 *		details. Either (or even both) IK and LK may be null
 *		pointers.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_sym_k (hid_t tid, int *ik /*out */ , int *lk /*out */ )
{
    H5F_create_t	   *tmpl = NULL;

    FUNC_ENTER(H5Pget_sym_k, FAIL);
    H5TRACE1("e","i",tid);

    /* Check arguments */
    if (H5P_FILE_CREATE != H5Pget_class(tid) ||
	NULL == (tmpl = H5I_object(tid))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file creation property list");
    }
    /* Get values */
    if (ik)
	*ik = tmpl->btree_k[H5B_SNODE_ID];
    if (lk)
	*lk = tmpl->sym_leaf_k;

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5Pset_istore_k
 *
 * Purpose:	IK is one half the rank of a tree that stores chunked raw
 *		data.  On average, such a tree will be 75% full, or have an
 *		average rank of 1.5 times the value of IK.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, January  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_istore_k (hid_t tid, int ik)
{
    H5F_create_t	   *tmpl = NULL;

    FUNC_ENTER(H5Pset_istore_k, FAIL);
    H5TRACE2("e","iIs",tid,ik);

    /* Check arguments */
    if (H5P_FILE_CREATE != H5Pget_class(tid) ||
	NULL == (tmpl = H5I_object(tid))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file creation property list");
    }
    if (ik <= 0) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		      "istore IK value must be positive");
    }
    /* Set value */
    tmpl->btree_k[H5B_ISTORE_ID] = ik;

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5Pget_istore_k
 *
 * Purpose:	Queries the 1/2 rank of an indexed storage B-tree.  See
 *		H5Pset_istore_k() for details.	The argument IK may be the
 *		null pointer.
 *
 * Return:	Success:	SUCCEED, size returned through IK
 *
 *		Failure:	
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_istore_k (hid_t tid, int *ik /*out */ )
{
    H5F_create_t	   *tmpl = NULL;

    FUNC_ENTER(H5Pget_istore_k, FAIL);
    H5TRACE1("e","i",tid);

    /* Check arguments */
    if (H5P_FILE_CREATE != H5Pget_class(tid) ||
	NULL == (tmpl = H5I_object(tid))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file creation property list");
    }
    /* Get value */
    if (ik)
	*ik = tmpl->btree_k[H5B_ISTORE_ID];

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5Pset_layout
 *
 * Purpose:	Sets the layout of raw data in the file.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, January  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_layout (hid_t tid, H5D_layout_t layout)
{
    H5D_create_t	   *tmpl = NULL;

    FUNC_ENTER(H5Pset_layout, FAIL);
    H5TRACE2("e","iDl",tid,layout);

    /* Check arguments */
    if (H5P_DATASET_CREATE != H5Pget_class(tid) ||
	NULL == (tmpl = H5I_object(tid))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a dataset creation property list");
    }
    if (layout < 0 || layout >= H5D_NLAYOUTS) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL,
		      "raw data layout method is not valid");
    }
    /* Set value */
    tmpl->layout = layout;

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5Pget_layout
 *
 * Purpose:	Retrieves layout type of a dataset creation property list.
 *
 * Return:	Success:	The layout type
 *
 *		Failure:	H5D_LAYOUT_ERROR (-1, same as FAIL)
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5D_layout_t
H5Pget_layout(hid_t tid)
{
    H5D_create_t	   *tmpl = NULL;

    FUNC_ENTER(H5Pget_layout, H5D_LAYOUT_ERROR);

    /* Check arguments */
    if (H5P_DATASET_CREATE != H5Pget_class(tid) ||
	NULL == (tmpl = H5I_object(tid))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, H5D_LAYOUT_ERROR,
		      "not a dataset creation property list");
    }
    FUNC_LEAVE(tmpl->layout);
}

/*-------------------------------------------------------------------------
 * Function:	H5Pset_chunk
 *
 * Purpose:	Sets the number of dimensions and the size of each chunk to
 *		the values specified.  The dimensionality of the chunk should
 *		match the dimensionality of the data space.
 *
 *		As a side effect, the layout method is changed to
 *		H5D_CHUNKED.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, January  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_chunk (hid_t tid, int ndims, const hsize_t dim[])
{
    int			    i;
    H5D_create_t	   *tmpl = NULL;

    FUNC_ENTER(H5Pset_chunk, FAIL);
    H5TRACE3("e","iIs*h",tid,ndims,dim);

    /* Check arguments */
    if (H5P_DATASET_CREATE != H5Pget_class(tid) ||
	NULL == (tmpl = H5I_object(tid))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a dataset creation property list");
    }
    if (ndims <= 0) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL,
		      "chunk dimensionality must be positive");
    }
    if ((size_t)ndims > NELMTS(tmpl->chunk_size)) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL,
		      "chunk dimensionality is too large");
    }
    if (!dim) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		      "no chunk dimensions specified");
    }
    for (i=0; i<ndims; i++) {
	if (dim[i] <= 0) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL,
			  "all chunk dimensions must be positive");
	}
    }

    /* Set value */
    tmpl->layout = H5D_CHUNKED;
    tmpl->chunk_ndims = ndims;
    for (i = 0; i < ndims; i++) {
	tmpl->chunk_size[i] = dim[i];
    }

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5Pget_chunk
 *
 * Purpose:	Retrieves the chunk size of chunked layout.  The chunk
 *		dimensionality is returned and the chunk size in each
 *		dimension is returned through the DIM argument.	 At most
 *		MAX_NDIMS elements of DIM will be initialized.
 *
 * Return:	Success:	Positive Chunk dimensionality.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5Pget_chunk (hid_t tid, int max_ndims, hsize_t dim[]/*out*/)
{
    int			i;
    H5D_create_t	*tmpl = NULL;

    FUNC_ENTER(H5Pget_chunk, FAIL);
    H5TRACE2("Is","iIs",tid,max_ndims);

    /* Check arguments */
    if (H5P_DATASET_CREATE != H5Pget_class(tid) ||
	NULL == (tmpl = H5I_object(tid))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a dataset creation property list");
    }
    if (H5D_CHUNKED != tmpl->layout) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		      "not a chunked storage layout");
    }
    for (i=0; i<tmpl->chunk_ndims && i<max_ndims && dim; i++) {
	dim[i] = tmpl->chunk_size[i];
    }

    FUNC_LEAVE(tmpl->chunk_ndims);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_external
 *
 * Purpose:	Adds an external file to the list of external files. PLIST_ID
 *		should be an object ID for a dataset creation property list.
 *		NAME is the name of an external file, OFFSET is the location
 *		where the data starts in that file, and SIZE is the number of
 *		bytes reserved in the file for the data.
 *
 *		If a dataset is split across multiple files then the files
 *		should be defined in order. The total size of the dataset is
 *		the sum of the SIZE arguments for all the external files.  If
 *		the total size is larger than the size of a dataset then the
 *		dataset can be extended (provided the data space also allows
 *		the extending).
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, March	3, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_external (hid_t plist_id, const char *name, off_t offset, hsize_t size)
{
    int			idx;
    size_t		total, tmp;
    H5D_create_t	*plist = NULL;

    FUNC_ENTER(H5Pset_external, FAIL);
    H5TRACE4("e","isoh",plist_id,name,offset,size);

    /* Check arguments */
    if (H5P_DATASET_CREATE != H5Pget_class(plist_id) ||
	NULL == (plist = H5I_object(plist_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a dataset creation property list");
    }
    if (!name || !*name) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "no name given");
    }
    if (offset<0) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "negative external file offset");
    }
    if (size<=0) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "zero size");
    }
    if (plist->efl.nused>0 &&
	H5O_EFL_UNLIMITED==plist->efl.slot[plist->efl.nused-1].size) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "previous file size is unlimited");
    }
    if (H5O_EFL_UNLIMITED!=size) {
	for (idx=0, total=size; idx<plist->efl.nused; idx++, total=tmp) {
	    tmp = total + plist->efl.slot[idx].size;
	    if (tmp <= total) {
		HRETURN_ERROR (H5E_EFL, H5E_OVERFLOW, FAIL,
			       "total external data size overflowed");
	    }
	}
    }
    
    /* Add to the list */
    if (plist->efl.nused>=plist->efl.nalloc) {
	plist->efl.nalloc += H5O_EFL_ALLOC;
	plist->efl.slot = H5MM_xrealloc (plist->efl.slot,
					 (plist->efl.nalloc *
					  sizeof(H5O_efl_entry_t)));
    }
    idx = plist->efl.nused;
    plist->efl.slot[idx].name_offset = 0; /*not entered into heap yet*/
    plist->efl.slot[idx].name = H5MM_xstrdup (name);
    plist->efl.slot[idx].offset = offset;
    plist->efl.slot[idx].size = size;
    plist->efl.nused++;

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_external_count
 *
 * Purpose:	Returns the number of external files for this dataset.
 *
 * Return:	Success:	Number of external files
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March  3, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5Pget_external_count (hid_t plist_id)
{
    H5D_create_t	*plist = NULL;
    
    FUNC_ENTER (H5Pget_external_count, FAIL);
    H5TRACE1("Is","i",plist_id);
    
    /* Check arguments */
    if (H5P_DATASET_CREATE != H5Pget_class(plist_id) ||
	NULL == (plist = H5I_object(plist_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a dataset creation property list");
    }

    /* Return */
    FUNC_LEAVE (plist->efl.nused);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_external
 *
 * Purpose:	Returns information about an external file.  External files
 *		are numbered from zero to N-1 where N is the value returned
 *		by H5Pget_external_count().  At most NAME_SIZE characters are
 *		copied into the NAME array.  If the external file name is
 *		longer than NAME_SIZE with the null terminator, then the
 *		return value is not null terminated (similar to strncpy()).
 *
 *		If NAME_SIZE is zero or NAME is the null pointer then the
 *		external file name is not returned.  If OFFSET or SIZE are
 *		null pointers then the corresponding information is not
 *		returned.
 *
 * See Also:	H5Pset_external()
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March  3, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_external (hid_t plist_id, int idx, size_t name_size, char *name/*out*/,
		 off_t *offset/*out*/, hsize_t *size/*out*/)
{
    H5D_create_t	*plist = NULL;
    
    FUNC_ENTER (H5Pget_external, FAIL);
    H5TRACE3("e","iIsz",plist_id,idx,name_size);
    
    /* Check arguments */
    if (H5P_DATASET_CREATE != H5Pget_class(plist_id) ||
	NULL == (plist = H5I_object(plist_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a dataset creation property list");
    }
    if (idx<0 || idx>=plist->efl.nused) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL,
		       "external file index is out of range");
    }

    /* Return values */
    if (name_size>0 && name) {
	strncpy (name, plist->efl.slot[idx].name, name_size);
    }
    if (offset) *offset = plist->efl.slot[idx].offset;
    if (size) *size = plist->efl.slot[idx].size;

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_driver
 *
 * Purpose:	Return the ID of the low-level file driver.  TID should be a
 *		file access property list.
 *
 * Return:	Success:	A low-level driver ID
 *
 *		Failure:	H5F_LOW_ERROR (a negative value)
 *
 * Programmer:	Robb Matzke
 *		Thursday, February 26, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5F_driver_t
H5Pget_driver (hid_t tid)
{
    H5F_access_t	*tmpl = NULL;

    FUNC_ENTER (H5Pget_driver, H5F_LOW_ERROR);

    /* Check arguments */
    if (H5P_FILE_ACCESS != H5Pget_class (tid) ||
	NULL == (tmpl = H5I_object (tid))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, H5F_LOW_ERROR,
		       "not a file access property list");
    }

    FUNC_LEAVE (tmpl->driver);
}
    

/*-------------------------------------------------------------------------
 * Function:	H5Pset_stdio
 *
 * Purpose:	Set the low level file driver to use the functions declared
 *		in the stdio.h file: fopen(), fseek() or fseek64(), fread(),
 *		fwrite(), and fclose().
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Thursday, February 19, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_stdio (hid_t tid)
{
    H5F_access_t	*tmpl = NULL;
    
    FUNC_ENTER (H5Pset_stdio, FAIL);
    H5TRACE1("e","i",tid);

    /* Check arguments */
    if (H5P_FILE_ACCESS != H5Pget_class(tid) ||
	NULL == (tmpl = H5I_object(tid))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file access property list");
    }

    /* Set driver */
    tmpl->driver = H5F_LOW_STDIO;

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_stdio
 *
 * Purpose:	If the file access property list is set to the stdio driver
 *		then this function returns zero; otherwise it returns a
 *		negative value.	 In the future, additional arguments may be
 *		added to this function to match those added to H5Pset_stdio().
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Thursday, February 26, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_stdio (hid_t tid)
{
    H5F_access_t	*tmpl = NULL;

    FUNC_ENTER (H5Pget_stdio, FAIL);
    H5TRACE1("e","i",tid);

    /* Check arguments */
    if (H5P_FILE_ACCESS != H5Pget_class (tid) ||
	NULL == (tmpl = H5I_object (tid))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "not a file access property list");
    }
    if (H5F_LOW_STDIO != tmpl->driver) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "the stdio driver is not set");
    }

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_sec2
 *
 * Purpose:	Set the low-level file driver to use the functions declared
 *		in the unistd.h file: open(), lseek() or lseek64(), read(),
 *		write(), and close().
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Thursday, February 19, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_sec2 (hid_t tid)
{
    H5F_access_t	*tmpl = NULL;
    
    FUNC_ENTER (H5Pset_sec2, FAIL);
    H5TRACE1("e","i",tid);

    /* Check arguments */
    if (H5P_FILE_ACCESS != H5Pget_class(tid) ||
	NULL == (tmpl = H5I_object(tid))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file access property list");
    }

    /* Set driver */
    tmpl->driver = H5F_LOW_SEC2;

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_sec2
 *
 * Purpose:	If the file access property list is set to the sec2 driver
 *		then this function returns zero; otherwise it returns a
 *		negative value.	 In the future, additional arguments may be
 *		added to this function to match those added to H5Pset_sec2().
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Thursday, February 26, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_sec2 (hid_t tid)
{
    H5F_access_t	*tmpl = NULL;

    FUNC_ENTER (H5Pget_sec2, FAIL);
    H5TRACE1("e","i",tid);

    /* Check arguments */
    if (H5P_FILE_ACCESS != H5Pget_class (tid) ||
	NULL == (tmpl = H5I_object (tid))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "not a file access property list");
    }
    if (H5F_LOW_SEC2 != tmpl->driver) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "the sec2 driver is not set");
    }

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_core
 *
 * Purpose:	Set the low-level file driver to use malloc() and free().
 *		This driver is restricted to temporary files which are not
 *		larger than the amount of virtual memory available. The
 *		INCREMENT argument determines the file block size and memory
 *		will be allocated in multiples of INCREMENT bytes. A liberal
 *		INCREMENT results in fewer calls to realloc() and probably
 *		less memory fragmentation.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Thursday, February 19, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_core (hid_t tid, size_t increment)
{
    H5F_access_t	*tmpl = NULL;
    
    FUNC_ENTER (H5Pset_core, FAIL);
    H5TRACE2("e","iz",tid,increment);

    /* Check arguments */
    if (H5P_FILE_ACCESS != H5Pget_class(tid) ||
	NULL == (tmpl = H5I_object(tid))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file access property list");
    }
    if (increment<1) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "increment must be positive");
    }

    /* Set driver */
    tmpl->driver = H5F_LOW_CORE;
    tmpl->u.core.increment = increment;

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_core
 *
 * Purpose:	If the file access property list is set to the core driver
 *		then this function returns zero; otherwise it returns a
 *		negative value.	 On success, the block size is returned
 *		through the INCREMENT argument if it isn't the null pointer.
 *		In the future, additional arguments may be added to this
 *		function to match those added to H5Pset_core().
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Thursday, February 26, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_core (hid_t tid, size_t *increment/*out*/)
{
    H5F_access_t	*tmpl = NULL;

    FUNC_ENTER (H5Pget_core, FAIL);
    H5TRACE1("e","i",tid);

    /* Check arguments */
    if (H5P_FILE_ACCESS != H5Pget_class (tid) ||
	NULL == (tmpl = H5I_object (tid))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "not a file access property list");
    }
    if (H5F_LOW_CORE != tmpl->driver) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "the core driver is not set");
    }

    /* Return values */
    if (increment) {
	*increment = tmpl->u.core.increment;
    }
    
    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_split
 *
 * Purpose:	Set the low-level driver to split meta data from raw data,
 *		storing meta data in one file and raw data in another file.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Thursday, February 19, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_split (hid_t tid, const char *meta_ext, hid_t meta_tid,
	      const char *raw_ext, hid_t raw_tid)
{
    H5F_access_t	*tmpl = NULL;
    H5F_access_t	*meta_tmpl = &H5F_access_dflt;
    H5F_access_t	*raw_tmpl = &H5F_access_dflt;
    
    FUNC_ENTER (H5Pset_split, FAIL);
    H5TRACE5("e","isisi",tid,meta_ext,meta_tid,raw_ext,raw_tid);

    /* Check arguments */
    if (H5P_FILE_ACCESS != H5Pget_class(tid) ||
	NULL == (tmpl = H5I_object(tid))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file access property list");
    }
    if (H5P_DEFAULT!=meta_tid &&
	(H5P_FILE_ACCESS != H5Pget_class(meta_tid) ||
	 NULL == (meta_tmpl = H5I_object(meta_tid)))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file access property list");
    }
    if (H5P_DEFAULT!=raw_tid &&
	(H5P_FILE_ACCESS != H5Pget_class(raw_tid) ||
	 NULL == (raw_tmpl = H5I_object(raw_tid)))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file access property list");
    }

    /* Set driver */
    tmpl->driver = H5F_LOW_SPLIT;
    tmpl->u.split.meta_access = H5P_copy (H5P_FILE_ACCESS, meta_tmpl);
    tmpl->u.split.raw_access = H5P_copy (H5P_FILE_ACCESS, raw_tmpl);
    tmpl->u.split.meta_ext = H5MM_xstrdup (meta_ext);
    tmpl->u.split.raw_ext = H5MM_xstrdup (raw_ext);

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_split
 *
 * Purpose:	If the file access property list is set to the sec2 driver
 *		then this function returns zero; otherwise it returns a
 *		negative value.	 On success, at most META_EXT_SIZE characters
 *		are copied to the META_EXT buffer if non-null and at most
 *		RAW_EXT_SIZE characters are copied to the RAW_EXT buffer if
 *		non-null.  If the actual extension is larger than the number
 *		of characters requested then the buffer will not be null
 *		terminated (that is, behavior like strncpy()).	In addition,
 *		if META_PROPERTIES and/or RAW_PROPERTIES are non-null then
 *		the file access property list of the meta file and/or raw
 *		file is copied and its OID returned through these arguments.
 *		In the future, additional arguments may be added to this
 *		function to match those added to H5Pset_sec2().
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Thursday, February 26, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_split (hid_t tid, size_t meta_ext_size, char *meta_ext/*out*/,
	      hid_t *meta_properties/*out*/, size_t raw_ext_size,
	      char *raw_ext/*out*/, hid_t *raw_properties/*out*/)
{
    H5F_access_t	*tmpl = NULL;

    FUNC_ENTER (H5Pget_split, FAIL);
    H5TRACE3("e","izz",tid,meta_ext_size,raw_ext_size);

    /* Check arguments */
    if (H5P_FILE_ACCESS != H5Pget_class (tid) ||
	NULL == (tmpl = H5I_object (tid))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "not a file access property list");
    }
    if (H5F_LOW_SPLIT != tmpl->driver) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "the split driver is not set");
    }

    /* Reset output args for error handling */
    if (meta_ext && meta_ext_size>0) *meta_ext = '\0';
    if (raw_ext && raw_ext_size>0) *raw_ext = '\0';
    if (meta_properties) *meta_properties = FAIL;
    if (raw_properties) *raw_properties = FAIL;

    /* Output arguments */
    if (meta_ext && meta_ext_size>0) {
	if (tmpl->u.split.meta_ext) {
	    strncpy (meta_ext, tmpl->u.split.meta_ext, meta_ext_size);
	} else {
	    strncpy (meta_ext, ".meta", meta_ext_size);
	}
    }
    if (raw_ext && raw_ext_size>0) {
	if (tmpl->u.split.raw_ext) {
	    strncpy (raw_ext, tmpl->u.split.raw_ext, raw_ext_size);
	} else {
	    strncpy (raw_ext, ".raw", raw_ext_size);
	}
    }
    if (meta_properties) {
	assert (tmpl->u.split.meta_access);
	*meta_properties = H5P_create (H5P_FILE_ACCESS,
				       H5P_copy (H5P_FILE_ACCESS,
						 tmpl->u.split.meta_access));
    }
    if (raw_properties) {
	assert (tmpl->u.split.raw_access);
	*raw_properties = H5P_create (H5P_FILE_ACCESS,
				      H5P_copy (H5P_FILE_ACCESS,
						tmpl->u.split.raw_access));
    }
    
    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_family
 *
 * Purpose:	Sets the low-level driver to stripe the hdf5 address space
 *		across a family of files.  The MEMB_SIZE argument indicates
 *		the size in bytes of each family member and is only
 *		meaningful when creating new files or opening families that
 *		have only one member.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Thursday, February 19, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_family (hid_t tid, hsize_t memb_size, hid_t memb_tid)
{
    
    H5F_access_t	*tmpl = NULL;
    H5F_access_t	*memb_tmpl = &H5F_access_dflt;
    
    FUNC_ENTER (H5Pset_family, FAIL);
    H5TRACE3("e","ihi",tid,memb_size,memb_tid);

    /* Check arguments */
    if (H5P_FILE_ACCESS != H5Pget_class(tid) ||
	NULL == (tmpl = H5I_object(tid))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file access property list");
    }
    if (memb_size && memb_size<1024) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL,
		       "family member size is too small");
    }
    if (H5P_DEFAULT!=memb_tid &&
	(H5P_FILE_ACCESS != H5Pget_class(memb_tid) ||
	 NULL == (tmpl = H5I_object(memb_tid)))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file access property list");
    }

    /* Set driver */
    tmpl->driver = H5F_LOW_FAMILY;
    H5F_addr_reset (&(tmpl->u.fam.memb_size));
    H5F_addr_inc (&(tmpl->u.fam.memb_size), memb_size);
    tmpl->u.fam.memb_access = H5P_copy (H5P_FILE_ACCESS, memb_tmpl);

    FUNC_LEAVE (SUCCEED);
}
    

/*-------------------------------------------------------------------------
 * Function:	H5Pget_family
 *
 * Purpose:	If the file access property list is set to the family driver
 *		then this function returns zero; otherwise it returns a
 *		negative value.	 On success, if MEMB_TID is a non-null
 *		pointer it will be initialized with the id of an open
 *		property list: the file access property list for the family
 *		members.  In the future, additional arguments may be added to
 *		this function to match those added to H5Pset_family().
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Thursday, February 26, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_family (hid_t tid, hsize_t *memb_size/*out*/, hid_t *memb_tid/*out*/)
{
    H5F_access_t	*tmpl = NULL;

    FUNC_ENTER (H5Pget_family, FAIL);
    H5TRACE1("e","i",tid);

    /* Check arguments */
    if (H5P_FILE_ACCESS != H5Pget_class (tid) ||
	NULL == (tmpl = H5I_object (tid))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "not a file access property list");
    }
    if (H5F_LOW_FAMILY != tmpl->driver) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "the family driver is not set");
    }

    /* Output args */
    if (memb_size) {
	*memb_size = tmpl->u.fam.memb_size.offset;
    }
    if (memb_tid) {
	assert (tmpl->u.fam.memb_access);
	*memb_tid = H5P_create (H5P_FILE_ACCESS,
				H5P_copy (H5P_FILE_ACCESS,
					  tmpl->u.fam.memb_access));
    }
	
    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_cache
 *
 * Purpose:	Set the number of objects in the meta data cache and the
 *		total number of bytes in the raw data chunk cache.
 *
 * 		The RDCC_W0 value should be between 0 and 1 inclusive and
 *		indicates how much chunks that have been fully read are
 *		favored for preemption.  A value of zero means fully read
 *		chunks are treated no differently than other chunks (the
 *		preemption is strictly LRU) while a value of one means fully
 *		read chunks are always preempted before other chunks.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, May 19, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_cache (hid_t tid, int mdc_nelmts, size_t rdcc_nbytes,
	      double rdcc_w0)
{
    H5F_access_t	*fapl = NULL;
    
    FUNC_ENTER (H5Pset_cache, FAIL);
    H5TRACE4("e","iIszd",tid,mdc_nelmts,rdcc_nbytes,rdcc_w0);

    /* Check arguments */
    if (H5P_FILE_ACCESS!=H5Pget_class (tid) ||
	NULL==(fapl=H5I_object (tid))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "not a file access property list");
    }
    if (mdc_nelmts<0) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "meta data cache size must be non-negative");
    }
    if (rdcc_w0<0.0 || rdcc_w0>1.0) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "raw data cache w0 value must be between 0.0 and 1.0 "
		       "inclusive");
    }

    /* Set sizes */
    fapl->mdc_nelmts = mdc_nelmts;
    fapl->rdcc_nbytes = rdcc_nbytes;

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_cache
 *
 * Purpose:	Retrieves the maximum possible number of elements in the meta
 *		data cache and the maximum possible number of bytes in the
 *		raw data chunk cache.  Any (or all) arguments may be null
 *		pointers in which case the corresponding datum is not
 *		returned.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, May 19, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_cache (hid_t tid, int *mdc_nelmts, size_t *rdcc_nbytes,
	      double *rdcc_w0)
{
    H5F_access_t	*fapl = NULL;
    
    FUNC_ENTER (H5Pget_cache, FAIL);
    H5TRACE4("e","i*Is*z*d",tid,mdc_nelmts,rdcc_nbytes,rdcc_w0);

    /* Check arguments */
    if (H5P_FILE_ACCESS!=H5Pget_class (tid) ||
	NULL==(fapl=H5I_object (tid))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "not a file access property list");
    }

    /* Get sizes */
    if (mdc_nelmts) *mdc_nelmts = fapl->mdc_nelmts;
    if (rdcc_nbytes) *rdcc_nbytes = fapl->rdcc_nbytes;
    if (rdcc_w0) *rdcc_w0 = fapl->rdcc_w0;

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_buffer
 *
 * Purpose:	Given a dataset transfer property list, set the maximum size
 *		for the type conversion buffer and background buffer and
 *		optionally supply pointers to application-allocated buffers.
 *		If the buffer size is smaller than the entire amount of data
 *		being transfered between application and file, and a type
 *		conversion buffer or background buffer is required then
 *		strip mining will be used.  However, certain restrictions
 *		apply for the size of buffer which can be used for strip
 *		mining.  For instance, when strip mining a 100x200x300
 *		hyperslab of a simple data space the buffer must be large
 *		enough to hold a 1x200x300 slab.
 *
 *		If TCONV and/or BKG are null pointers then buffers will be
 *		allocated and freed during the data transfer.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Monday, March 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_buffer (hid_t plist_id, size_t size, void *tconv, void *bkg)
{
    H5D_xfer_t		*plist = NULL;
    
    FUNC_ENTER (H5Pset_buffer, FAIL);
    H5TRACE4("e","izxx",plist_id,size,tconv,bkg);

    /* Check arguments */
    if (H5P_DATASET_XFER != H5Pget_class (plist_id) ||
	NULL == (plist = H5I_object (plist_id))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "not a dataset transfer property list");
    }
    if (size<=0) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "buffer size must not be zero");
    }

    /* Update property list */
    plist->buf_size = size;
    plist->tconv_buf = tconv;
    plist->bkg_buf = bkg;

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_buffer
 *
 * Purpose:	Reads values previously set with H5Pset_buffer().
 *
 * Return:	Success:	Buffer size.
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *              Monday, March 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5Pget_buffer (hid_t plist_id, void **tconv/*out*/, void **bkg/*out*/)
{
    H5D_xfer_t		*plist = NULL;
    
    FUNC_ENTER (H5Pget_buffer, 0);
    H5TRACE1("z","i",plist_id);

    /* Check arguments */
    if (H5P_DATASET_XFER != H5Pget_class (plist_id) ||
	NULL == (plist = H5I_object (plist_id))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, 0,
		       "not a dataset transfer property list");
    }

    /* Return values */
    if (tconv) *tconv = plist->tconv_buf;
    if (bkg) *bkg = plist->bkg_buf;

    FUNC_LEAVE (plist->buf_size);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_preserve
 *
 * Purpose:	When reading or writing compound data types and the
 *		destination is partially initialized and the read/write is
 *		intended to initialize the other members, one must set this
 *		property to TRUE.  Otherwise the I/O pipeline treats the
 *		destination datapoints as completely uninitialized.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March 17, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_preserve (hid_t plist_id, hbool_t status)
{
    H5D_xfer_t		*plist = NULL;
    
    FUNC_ENTER (H5Pset_preserve, FAIL);
    H5TRACE2("e","ib",plist_id,status);

    /* Check arguments */
    if (H5P_DATASET_XFER != H5Pget_class (plist_id) ||
	NULL == (plist = H5I_object (plist_id))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "not a dataset transfer property list");
    }

    /* Update property list */
    plist->need_bkg = status ? H5T_BKG_YES : H5T_BKG_NO;

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_preserve
 *
 * Purpose:	The inverse of H5Pset_preserve()
 *
 * Return:	Success:	TRUE or FALSE
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March 17, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5Pget_preserve (hid_t plist_id)
{
    H5D_xfer_t		*plist = NULL;
    
    FUNC_ENTER (H5Pset_preserve, FAIL);
    H5TRACE1("Is","i",plist_id);

    /* Check arguments */
    if (H5P_DATASET_XFER != H5Pget_class (plist_id) ||
	NULL == (plist = H5I_object (plist_id))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "not a dataset transfer property list");
    }

    FUNC_LEAVE (plist->need_bkg?TRUE:FALSE);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_compression
 *
 * Purpose:	Sets the compression method in a dataset creation property
 *		list.  This sets default values for the compression
 *		attributes by setting the flags to zero and supplying no
 *		compression client data.  It's probably better to use
 *		specific compression initialization functions like
 *		H5Pset_deflate().
 *
 *		The FLAGS, CD_SIZE, and CLIENT_DATA are copied to the
 *		property list and eventually to the file and passed to the
 *		compression functions.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April 15, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_compression (hid_t plist_id, H5Z_method_t method, unsigned int flags,
		    size_t cd_size, const void *client_data)
{
    H5D_create_t	*plist = NULL;
    
    FUNC_ENTER (H5Pset_compression, FAIL);
    H5TRACE5("e","iZmIuzx",plist_id,method,flags,cd_size,client_data);

    /* Check arguments */
    if (H5P_DATASET_CREATE!=H5Pget_class (plist_id) ||
	NULL==(plist=H5I_object (plist_id))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "not a dataset creation property list");
    }
    if (method<0 || method>H5Z_USERDEF_MAX) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "invalid compression method");
    }

    /* Clear any previous compression method info, then set new value */
    H5O_reset (H5O_COMPRESS, &(plist->compress));
    plist->compress.method = method;
    plist->compress.flags = flags;
    plist->compress.cd_size = cd_size;
    if (cd_size) {
	plist->compress.client_data = H5MM_xmalloc (cd_size);
	HDmemcpy (plist->compress.client_data, client_data, cd_size);
    }
    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_compression
 *
 * Purpose:	Gets the compression method information from a dataset
 *		creation property list.  The CLIENT_DATA buffer is initially
 *		CD_SIZE bytes.  On return, CLIENT_DATA will be initialized
 *		with at most that many bytes, and CD_SIZE will contain the
 *		actual size of the client data, which might be larger than
 *		its original value.
 *
 * Return:	Success:	Compression method.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April 15, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5Z_method_t
H5Pget_compression (hid_t plist_id, unsigned int *flags/*out*/,
		    size_t *cd_size/*in,out*/, void *client_data/*out*/)
{
    H5D_create_t	*plist = NULL;
    
    FUNC_ENTER (H5Pget_compression, FAIL);
    
    /* Check arguments */
    if (H5P_DATASET_CREATE!=H5Pget_class (plist_id) ||
	NULL==(plist=H5I_object (plist_id))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "not a dataset creation property list");
    }

    /* Output values */
    if (flags) *flags = plist->compress.flags;
    if (cd_size) {
	if (*cd_size>0 && client_data) {
	    HDmemcpy (client_data, plist->compress.client_data,
		      MIN(plist->compress.cd_size, *cd_size));
	}
	*cd_size = plist->compress.cd_size;
    }

    FUNC_LEAVE (plist->compress.method);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_deflate
 *
 * Purpose:	Sets the compression method for a dataset creation property
 *		list to H5D_COMPRESS_DEFLATE and the compression level to
 *		LEVEL which should be a value between zero and nine,
 *		inclusive.  Lower compression levels are faster but result in
 *		less compression.  This is the same algorithm as used by the
 *		GNU gzip program.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April 15, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_deflate (hid_t plist_id, int level)
{
    H5D_create_t	*plist = NULL;
    
    FUNC_ENTER (H5Pset_deflate, FAIL);
    H5TRACE2("e","iIs",plist_id,level);

    /* Check arguments */
    if (H5P_DATASET_CREATE!=H5Pget_class (plist_id) ||
	NULL==(plist=H5I_object (plist_id))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "not a dataset creation property list");
    }
    if (level<0 || level>9) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "invalid deflate level");
    }

    /* Clear previous compression parameters */
    H5O_reset (H5O_COMPRESS, &(plist->compress));
    plist->compress.method = H5Z_DEFLATE;
    plist->compress.flags = level;

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_deflate
 *
 * Purpose:	Returns the deflate compression level from a dataset creation
 *		property list that uses that method.
 *
 * Return:	Success:	A value between zero and nine, inclusive.
 *				Smaller values indicate faster compression
 *				while higher values indicate better
 *				compression ratios.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April 15, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5Pget_deflate (hid_t plist_id)
{
    H5D_create_t	*plist = NULL;
    
    FUNC_ENTER (H5Pget_deflate, FAIL);
    H5TRACE1("Is","i",plist_id);
    
    /* Check arguments */
    if (H5P_DATASET_CREATE!=H5Pget_class (plist_id) ||
	NULL==(plist=H5I_object (plist_id))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "not a dataset creation property list");
    }
    if (H5Z_DEFLATE!=plist->compress.method) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "deflate method is not being used");
    }

    FUNC_LEAVE (plist->compress.flags % 10);
}


#ifdef HAVE_PARALLEL
/*-------------------------------------------------------------------------
 * Function:	H5Pset_mpi
 *
 * Signature:	herr_t H5Pset_mpi(hid_t tid, MPI_Comm comm, MPI_Info info)
 *
 * Purpose:	Store the access mode for MPIO call and the user supplied
 *		communicator and info in the access property list which can
 *		then be used to open file.  This function is available only
 *		in the parallel HDF5 library and is not a collective
 *		function.
 *
 * Parameters:
 *		hid_t tid 
 *		    ID of property list to modify 
 *		MPI_Comm comm 
 *		    MPI communicator to be used for file open as defined in
 *		    MPI_FILE_OPEN of MPI-2.  This function  does not make a
 *		    duplicated communicator. Any modification to comm after
 *		    this function call returns may have undetermined effect
 *		    to the access property list.  Users should call this
 *		    function again to setup the property list.
 *		MPI_Info info 
 *		    MPI info object to be used for file open as defined in
 *		    MPI_FILE_OPEN of MPI-2.  This function  does not make a
 *		    duplicated info. Any modification to info after
 *		    this function call returns may have undetermined effect
 *		    to the access property list.  Users should call this
 *		    function again to setup the property list.
 *	     
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Albert Cheng
 *		Feb 3, 1998
 *
 * Modifications:
 *
 *	Robb Matzke, 18 Feb 1998
 *	Check all arguments before the property list is updated so we don't
 *	leave the property list in a bad state if something goes wrong.  Also,
 *	the property list data type changed to allow more generality so all
 *	the mpi-related stuff is in the `u.mpi' member.  The `access_mode'
 *	will contain only mpi-related flags defined in H5Fpublic.h.
 *
 *	Albert Cheng, Apr 16, 1998
 *	Removed the access_mode argument.  The access_mode is changed
 *	to be controlled by data transfer property list during data
 *	read/write calls.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_mpi (hid_t tid, MPI_Comm comm, MPI_Info info)
{
    H5F_access_t	   *tmpl = NULL;

    FUNC_ENTER(H5Pset_mpi, FAIL);
    H5TRACE3("e","iMcMi",tid,comm,info);

    /* Check arguments */
    if (H5P_FILE_ACCESS != H5Pget_class(tid) ||
	NULL == (tmpl = H5I_object(tid))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file access property list");
    }
    
#ifdef LATER
    /*
     * Need to verify comm and info contain sensible information.
     */
#endif

    tmpl->driver = H5F_LOW_MPIO;
    tmpl->u.mpio.comm = comm;
    tmpl->u.mpio.info = info;

    FUNC_LEAVE(SUCCEED);
}
#endif /*HAVE_PARALLEL*/


#ifdef HAVE_PARALLEL
/*-------------------------------------------------------------------------
 * Function:	H5Pget_mpi
 *
 * Purpose:	If the file access property list is set to the mpi driver
 *		then this function returns zero; otherwise it returns a
 *		negative value.	 In the future, additional arguments may be
 *		added to this function to match those added to H5Pset_mpi().
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Thursday, February 26, 1998
 *
 * Modifications:
 *
 *	Albert Cheng, Apr 16, 1998
 *	Removed the access_mode argument.  The access_mode is changed
 *	to be controlled by data transfer property list during data
 *	read/write calls.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_mpi (hid_t tid, MPI_Comm *comm, MPI_Info *info)
{
    H5F_access_t	*tmpl = NULL;

    FUNC_ENTER (H5Pget_mpi, FAIL);
    H5TRACE3("e","i*Mc*Mi",tid,comm,info);

    /* Check arguments */
    if (H5P_FILE_ACCESS != H5Pget_class (tid) ||
	NULL == (tmpl = H5I_object (tid))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "not a file access property list");
    }
    if (H5F_LOW_MPIO != tmpl->driver) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "the mpi driver is not set");
    }
    *comm = tmpl->u.mpio.comm;
    *info = tmpl->u.mpio.info;

    FUNC_LEAVE (SUCCEED);
}
#endif /*HAVE_PARALLEL*/


#ifdef HAVE_PARALLEL
/*-------------------------------------------------------------------------
 * Function:	H5Pset_xfer
 *
 * Signature:	herr_t H5Pset_xfer(hid_t tid, H5D_transfer_t data_xfer_mode)
 *
 * Purpose:	Set the transfer mode of the dataset transfer property list.
 *		The list can then be used to control the I/O transfer mode
 *		during dataset accesses.  This function is available only
 *		in the parallel HDF5 library and is not a collective function.
 *
 * Parameters:
 *		hid_t tid 
 *		    ID of a dataset transfer property list
 *		H5D_transfer_t data_xfer_mode
 *		    Data transfer modes: 
 *			H5D_XFER_INDEPENDENT 
 *			    Use independent I/O access. 
 *			H5D_XFER_COLLECTIVE 
 *			    Use MPI collective I/O access. 
 *			H5D_XFER_DFLT 
 *			    Use default I/O access.  Currently,
 *			    independent is the default mode.
 *			
 *	     
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Albert Cheng
 *		April 2, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_xfer (hid_t tid, H5D_transfer_t data_xfer_mode)
{
    H5D_xfer_t		*plist = NULL;

    FUNC_ENTER(H5Pset_xfer, FAIL);
    H5TRACE2("e","iDt",tid,data_xfer_mode);

    /* Check arguments */
    if (H5P_DATASET_XFER != H5Pget_class(tid) ||
	NULL == (plist = H5I_object(tid))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a dataset transfer property list");
    }
    
    switch (data_xfer_mode){
    case H5D_XFER_INDEPENDENT:
    case H5D_XFER_COLLECTIVE:
    case H5D_XFER_DFLT:
	plist->xfer_mode = data_xfer_mode;
	break;
    default:
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "invalid dataset transfer mode");
    }

    FUNC_LEAVE(SUCCEED);
}
#endif /*HAVE_PARALLEL*/


#ifdef HAVE_PARALLEL
/*-------------------------------------------------------------------------
 * Function:	H5Pget_xfer
 *
 * Purpose:	Reads the transfer mode current set in the property list.
 *		This function is available only in the parallel HDF5 library
 *		and is not a collective function.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Albert Cheng
 *		April 2, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_xfer (hid_t tid, H5D_transfer_t *data_xfer_mode)
{
    H5D_xfer_t		*plist = NULL;

    FUNC_ENTER (H5Pget_xfer, FAIL);
    H5TRACE2("e","i*Dt",tid,data_xfer_mode);

    /* Check arguments */
    if (H5P_DATASET_XFER != H5Pget_class(tid) ||
	NULL == (plist = H5I_object(tid))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a dataset transfer property list");
    }

    *data_xfer_mode = plist->xfer_mode;

    FUNC_LEAVE (SUCCEED);
}
#endif /*HAVE_PARALLEL*/


/*--------------------------------------------------------------------------
 NAME
    H5Pcopy
 PURPOSE
    Copy a property list
 USAGE
    hid_t H5P_copy(tid)
	hid_t tid;	  IN: property list object to copy
 RETURNS
    Returns property list ID (atom) on success, FAIL on failure

 ERRORS
    ARGS      BADRANGE	    Unknown property list class. 
    ATOM      BADATOM	    Can't unatomize property list. 
    ATOM      CANTREGISTER  Register the atom for the new property list. 
    INTERNAL  UNSUPPORTED   Dataset transfer properties are not implemented
			    yet. 
    INTERNAL  UNSUPPORTED   File access properties are not implemented yet. 

 DESCRIPTION
 * This function creates a new copy of a property list with all the same
 * parameter settings.
--------------------------------------------------------------------------*/
hid_t
H5Pcopy (hid_t tid)
{
    const void		   *tmpl = NULL;
    void		   *new_tmpl = NULL;
    H5P_class_t		    type;
    hid_t		    ret_value = FAIL;
    H5I_group_t		    group;

    FUNC_ENTER(H5Pcopy, FAIL);
    H5TRACE1("i","i",tid);

    /* Check args */
    if (NULL == (tmpl = H5I_object(tid)) ||
	(type = H5Pget_class(tid)) < 0 ||
	(group = H5I_group(tid)) < 0) {
	HRETURN_ERROR(H5E_ATOM, H5E_BADATOM, FAIL,
		      "unable to unatomize property list");
    }

    /* Copy it */
    if (NULL==(new_tmpl=H5P_copy (type, tmpl))) {
	HRETURN_ERROR (H5E_INTERNAL, H5E_CANTINIT, FAIL,
		       "unable to copy property list");
    }

    /* Register the atom for the new property list */
    if ((ret_value = H5I_register(group, new_tmpl)) < 0) {
	HRETURN_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL,
		      "unable to atomize property list pointer");
    }
    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5P_copy
 *
 * Purpose:	Creates a new property list and initializes it with some
 *		other property list.
 *
 * Return:	Success:	Ptr to new property list
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, February  3, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *
H5P_copy (H5P_class_t type, const void *src)
{
    size_t		size;
    void		*dst = NULL;
    int			i;
    const H5D_create_t	*dc_src = NULL;
    H5D_create_t	*dc_dst = NULL;
    const H5F_access_t	*fa_src = NULL;
    H5F_access_t	*fa_dst = NULL;
    
    FUNC_ENTER (H5P_copy, NULL);
    
    /* How big is the property list */
    switch (type) {
    case H5P_FILE_CREATE:
	size = sizeof(H5F_create_t);
	break;

    case H5P_FILE_ACCESS:
	size = sizeof(H5F_access_t);
	break;

    case H5P_DATASET_CREATE:
	size = sizeof(H5D_create_t);
	break;

    case H5P_DATASET_XFER:
	size = sizeof(H5D_xfer_t);
	break;

    default:
	HRETURN_ERROR(H5E_ARGS, H5E_BADRANGE, NULL,
		      "unknown property list class");
    }

    /* Create the new property list */
    dst = H5MM_xmalloc(size);
    HDmemcpy(dst, src, size);

    /* Deep-copy pointers */
    switch (type) {
    case H5P_FILE_CREATE:
	break;
	
    case H5P_FILE_ACCESS:
	fa_src = (const H5F_access_t*)src;
	fa_dst = (H5F_access_t*)dst;
	switch (fa_src->driver) {
	case H5F_LOW_ERROR:
	case H5F_LOW_SEC2:
	case H5F_LOW_STDIO:
	case H5F_LOW_CORE:
	case H5F_LOW_MPIO:
	    /* Nothing to do */
	    break;
	    
	case H5F_LOW_FAMILY:
	    fa_dst->u.fam.memb_access = H5P_copy (H5P_FILE_ACCESS,
						  fa_src->u.fam.memb_access);
	    break;

	case H5F_LOW_SPLIT:
	    fa_dst->u.split.meta_access=H5P_copy (H5P_FILE_ACCESS,
						  fa_src->u.split.meta_access);
	    fa_dst->u.split.raw_access = H5P_copy (H5P_FILE_ACCESS,
						   fa_src->u.split.raw_access);
	    break;
	}
	break;
	
    case H5P_DATASET_CREATE:
	dc_src = (const H5D_create_t*)src;
	dc_dst = (H5D_create_t*)dst;
	
	if (dc_src->efl.nalloc>0) {
	    dc_dst->efl.slot = H5MM_xmalloc (dc_dst->efl.nalloc *
					     sizeof(H5O_efl_entry_t));
	    for (i=0; i<dc_src->efl.nused; i++) {
		char *s = H5MM_xstrdup (dc_src->efl.slot[i].name);
		dc_dst->efl.slot[i] = dc_src->efl.slot[i];
		dc_dst->efl.slot[i].name = s;
	    }
	}
	break;
	
    case H5P_DATASET_XFER:
	break;

    default:
	HRETURN_ERROR(H5E_ARGS, H5E_BADRANGE, NULL,
		      "unknown property list class");
    }

    FUNC_LEAVE (dst);
}
