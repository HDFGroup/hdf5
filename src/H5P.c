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

/* Private header files */
#include <H5private.h>		/* Generic Functions			*/
#include <H5Iprivate.h>		/* IDs			  		*/
#include <H5Bprivate.h>		/* B-tree subclass names	  	*/
#include <H5Dprivate.h>		/* Datasets				*/
#include <H5Eprivate.h>		/* Error handling		  	*/
#include <H5FDprivate.h>	/* File drivers				*/
#include <H5FLprivate.h>	/* Free Lists	  */
#include <H5MMprivate.h>	/* Memory management			*/
#include <H5Pprivate.h>		/* Property lists		  	*/

/* Default file driver - see H5Pget_driver() */
#include <H5FDsec2.h>		/* Posix unbuffered I/O	file driver	*/

#define PABLO_MASK	H5P_mask

/* Is the interface initialized? */
static intn		interface_initialize_g = 0;
#define INTERFACE_INIT H5P_init_interface
static herr_t		H5P_init_interface(void);

/*
 * Predefined data types. These are initialized at runtime by 
 * H5P_init_interface() in this source file.
 */
hid_t H5P_NO_CLASS_g            = FAIL;
hid_t H5P_FILE_CREATE_g         = FAIL;
hid_t H5P_FILE_ACCESS_g         = FAIL;
hid_t H5P_DATASET_CREATE_g      = FAIL;
hid_t H5P_DATA_XFER_g           = FAIL;
hid_t H5P_MOUNT_g               = FAIL;

/* Declare a free list to manage the H5P_t struct */
H5FL_DEFINE_STATIC(H5P_t);


/*-------------------------------------------------------------------------
 * Function:	H5P_init
 *
 * Purpose:	Initialize the interface from some other layer.
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Saturday, March 4, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5P_init(void)
{
    FUNC_ENTER(H5P_init, FAIL);
    /* FUNC_ENTER() does all the work */
    FUNC_LEAVE(SUCCEED);
}

/*--------------------------------------------------------------------------
NAME
    H5P_xor_name -- Generate an xor'ed value for a string
USAGE
    uintn H5P_xor_name(s)
        const char *s;  IN: String to operate over
RETURNS
    Always returns valid value
DESCRIPTION
    Generates an xor'ed value for a string
--------------------------------------------------------------------------*/
static uintn
H5P_xor_name(const char *s)
{
    uintn ret=0;
    unsigned char temp;

    if(s!=NULL)
        while(*s!='\0') {
            temp=(ret>>24)&0xff;
            ret <<= 8;
            ret |= temp;
            ret ^= *s++;
        }

    return(ret);
}   /* end H5P_xor_name() */

/*--------------------------------------------------------------------------
NAME
    H5P_hash_name -- Generate a hash value for a string
USAGE
    uintn H5P_hash_name(s, hashsize)
        const char *s;  IN: String to operate over
        uintn;          IN: Size of hash table to clip against
RETURNS
    Always returns valid value
DESCRIPTION
    Generates an xor'ed value for a string
--------------------------------------------------------------------------*/
static uintn
H5P_hash_name(const char *s, uintn hashsize)
{
    return(H5P_xor_name(s)%hashsize);
}   /* end H5P_hash_name() */


/*--------------------------------------------------------------------------
NAME
   H5P_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5P_init_interface()
   
RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
static herr_t
H5P_init_interface(void)
{
    H5P_genclass_t  *root_class;    /* Pointer to root property list class created */
    H5P_genclass_t  *pclass;        /* Pointer to property list class to create */
    herr_t		    ret_value = SUCCEED;
    intn		    i;
    herr_t		    status;

    FUNC_ENTER(H5P_init_interface, FAIL);

    assert(H5P_NCLASSES <= H5I_TEMPLATE_MAX - H5I_TEMPLATE_0);

    /*
     * Initialize the mappings between property list classes and atom
     * groups. We keep the two separate because property list classes are
     * publicly visible but atom groups aren't.
     */
    for (i = 0; i < H5P_NCLASSES; i++) {
        status = H5I_init_group((H5I_type_t)(H5I_TEMPLATE_0 +i),
                    H5I_TEMPID_HASHSIZE, 0, (H5I_free_t)H5P_close);
        if (status < 0)
            ret_value = FAIL;
    }

    if (ret_value < 0) {
        HRETURN_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL,
		      "unable to initialize atom group");
    }
    
    /*
     * Initialize the Generic Property class & object groups.
     */
    if (H5I_init_group(H5I_GENPROP_CLS, H5I_GENPROPCLS_HASHSIZE, 0, NULL) < 0)
        HRETURN_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL,
		      "unable to initialize atom group");
    if (H5I_init_group(H5I_GENPROP_LST, H5I_GENPROPOBJ_HASHSIZE, 0, NULL) < 0)
        HRETURN_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL,
		      "unable to initialize atom group");

    /* Create root property list class */

    /* Allocate the root class */
    if (NULL==(root_class = H5P_create_class (NULL,"none",H5P_NO_CLASS_HASH_SIZE,1,NULL,NULL,NULL,NULL)))
        HRETURN_ERROR (H5E_PLIST, H5E_CANTINIT, FAIL, "class initialization failed");

    /* Register the root class */
    if ((H5P_NO_CLASS_g = H5I_register (H5I_GENPROP_CLS, root_class))<0)
        HRETURN_ERROR (H5E_PLIST, H5E_CANTREGISTER, FAIL, "can't register property list class");

    /* Register the file creation and file access property classes */

    /* Allocate the file creation class */
    if (NULL==(pclass = H5P_create_class (root_class,"file create",H5P_FILE_CREATE_HASH_SIZE,1,NULL,NULL,NULL,NULL)))
        HRETURN_ERROR (H5E_PLIST, H5E_CANTINIT, FAIL, "class initialization failed");

    /* Register the dataset creation class */
    if ((H5P_FILE_CREATE_g = H5I_register (H5I_GENPROP_CLS, pclass))<0)
        HRETURN_ERROR (H5E_PLIST, H5E_CANTREGISTER, FAIL, "can't register property list class");

    /* Allocate the file access class */
    if (NULL==(pclass = H5P_create_class (root_class,"file access",H5P_FILE_ACCESS_HASH_SIZE,1,NULL,NULL,NULL,NULL)))
        HRETURN_ERROR (H5E_PLIST, H5E_CANTINIT, FAIL, "class initialization failed");

    /* Register the file access class */
    if ((H5P_DATA_XFER_g = H5I_register (H5I_GENPROP_CLS, pclass))<0)
        HRETURN_ERROR (H5E_PLIST, H5E_CANTREGISTER, FAIL, "can't register property list class");

    /* Register the dataset creation and data xfer property classes */

    /* Allocate the dataset creation class */
    if (NULL==(pclass = H5P_create_class (root_class,"dataset create",H5P_DATASET_CREATE_HASH_SIZE,1,NULL,NULL,NULL,NULL)))
        HRETURN_ERROR (H5E_PLIST, H5E_CANTINIT, FAIL, "class initialization failed");

    /* Register the dataset creation class */
    if ((H5P_DATASET_CREATE_g = H5I_register (H5I_GENPROP_CLS, pclass))<0)
        HRETURN_ERROR (H5E_PLIST, H5E_CANTREGISTER, FAIL, "can't register property list class");

    /* Allocate the data xfer class */
    if (NULL==(pclass = H5P_create_class (root_class,"data xfer",H5P_DATA_XFER_HASH_SIZE,1,NULL,NULL,NULL,NULL)))
        HRETURN_ERROR (H5E_PLIST, H5E_CANTINIT, FAIL, "class initialization failed");

    /* Register the data xfer class */
    if ((H5P_DATA_XFER_g = H5I_register (H5I_GENPROP_CLS, pclass))<0)
        HRETURN_ERROR (H5E_PLIST, H5E_CANTREGISTER, FAIL, "can't register property list class");

/* When do the "basic" properties for each of the library classes get added? */
/* Who adds them? */

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
    Non-negative on success/Negative on failure
 DESCRIPTION
    Release the atom group and any other resources allocated.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
intn
H5P_term_interface(void)
{
    intn	i, n=0;

    if (interface_initialize_g) {
/* Destroy HDF5 library property classes */
        for (i=0; i<H5P_NCLASSES; i++)
            n += H5I_nmembers((H5I_type_t)(H5I_TEMPLATE_0+i));
        n += H5I_nmembers(H5I_GENPROP_CLS);
        n += H5I_nmembers(H5I_GENPROP_LST);
        if (n) {
            for (i=0; i<H5P_NCLASSES; i++)
                H5I_clear_group((H5I_type_t)(H5I_TEMPLATE_0+i), FALSE);
            H5I_clear_group(H5I_GENPROP_CLS, FALSE);
            H5I_clear_group(H5I_GENPROP_LST, FALSE);
        } else {
            for (i=0; i<H5P_NCLASSES; i++) {
                H5I_destroy_group((H5I_type_t)(H5I_TEMPLATE_0 + i));
                n++; /*H5I*/
            }

            H5I_destroy_group(H5I_GENPROP_CLS);
            n++; /*H5I*/
            H5I_destroy_group(H5I_GENPROP_LST);
            n++; /*H5I*/

            interface_initialize_g = 0;
        }
    }
    return n;
}


/*-------------------------------------------------------------------------
 * Function:	H5Pcreate
 *
 * Purpose:	Creates a new property list by copying a default property
 *		list.
 *
 * Return:	Success:	A new copy of a default property list.
 *
 *		Failure:	NULL
 *
 * Programmer:	Unknown
 *
 * Modifications:
 *		Robb Matzke, 1999-08-18
 *		Rewritten in terms of H5P_copy() to fix memory leaks.
 *-------------------------------------------------------------------------
 */
hid_t
H5Pcreate(H5P_class_t type)
{
    hid_t	ret_value = FAIL;
    const void	*src = NULL;
    H5P_t	*new_plist = NULL;

    FUNC_ENTER(H5Pcreate, FAIL);
    H5TRACE1("i","p",type);

    /* Allocate a new property list and initialize it with default values */
    switch (type) {
        case H5P_FILE_CREATE:
            src = &H5F_create_dflt;
            break;
        case H5P_FILE_ACCESS:
            src = &H5F_access_dflt;
            break;
        case H5P_DATASET_CREATE:
            src = &H5D_create_dflt;
            break;
        case H5P_DATA_XFER:
            src = &H5F_xfer_dflt;
            break;
        case H5P_MOUNT:
            src = &H5F_mount_dflt;
            break;
        default:
            HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                  "unknown property list class");
    }

    /* Copy the property list */
    if (NULL==(new_plist=H5P_copy(type, src))) {
        HRETURN_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL,
		      "unable to copy default property list");
    }
    
    /* Atomize the new property list */
    if ((ret_value = H5P_create(type, new_plist)) < 0) {
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
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Wednesday, December  3, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5P_create(H5P_class_t type, H5P_t *plist)
{
    hid_t	ret_value = FAIL;

    FUNC_ENTER(H5P_create, FAIL);

    /* check args */
    assert(type >= 0 && type < H5P_NCLASSES);
    assert(plist);

    /* Atomize the new property list */
    if ((ret_value=H5I_register((H5I_type_t)(H5I_TEMPLATE_0+type), plist))<0) {
        HRETURN_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL,
		      "unable to register property list");
    }
    
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pclose
 *
 * Purpose:	Release access to a property list object, PLIST_ID.
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Unknown
 *
 * Modifications:
 * 		Robb Matzke, 1999-08-03
 *		Attempting to close H5P_DEFAULT is no longer an error, but
 *		rather a no-op.
 *-------------------------------------------------------------------------
 */
herr_t
H5Pclose(hid_t plist_id)
{
    FUNC_ENTER(H5Pclose, FAIL);
    H5TRACE1("e","i",plist_id);

    /* Check arguments */
    if (plist_id==H5P_DEFAULT)
        HRETURN(SUCCEED);
    if (H5P_get_class (plist_id)<0) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
    }
	
    /* When the reference count reaches zero the resources are freed */
    if (H5I_dec_ref(plist_id) < 0)
        HRETURN_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "problem freeing property list");

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5P_close
 *
 * Purpose:	Closes a property list and frees the memory associated with
 *		the property list.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Wednesday, February 18, 1998
 *
 * Modifications:
 *		Robb Matzke, 1999-08-03
 *		Modified to work with the virtual file layer.
 *-------------------------------------------------------------------------
 */
herr_t
H5P_close(void *_plist)
{
    H5P_t           *plist=(H5P_t *)_plist;
    H5F_access_t	*fa_list = &(plist->u.faccess);
    H5F_xfer_t		*dx_list = &(plist->u.dxfer);
    H5D_create_t	*dc_list = &(plist->u.dcreate);
    
    FUNC_ENTER (H5P_close, FAIL);

    /* Check args */
    if (!plist)
        HRETURN (SUCCEED);

    /* Some property lists may need to do special things */
    switch (plist->class) {
        case H5P_FILE_ACCESS:
            if (fa_list->driver_id>=0) {
                H5FD_fapl_free(fa_list->driver_id, fa_list->driver_info);
                H5I_dec_ref(fa_list->driver_id);
                fa_list->driver_info = NULL;
                fa_list->driver_id = -1;
            }
            break;
        
        case H5P_FILE_CREATE:
            break;
        
        case H5P_DATASET_CREATE:
            H5O_reset(H5O_FILL, &(dc_list->fill));
            H5O_reset(H5O_EFL, &(dc_list->efl));
            H5O_reset(H5O_PLINE, &(dc_list->pline));
            break;

        case H5P_DATA_XFER:
            if (dx_list->driver_id>=0) {
                H5FD_dxpl_free(dx_list->driver_id, dx_list->driver_info);
                H5I_dec_ref(dx_list->driver_id);
                dx_list->driver_info = NULL;
                dx_list->driver_id = -1;
            }
            break;

        case H5P_MOUNT:
            break;

        default:
            HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
                   "unknown property list class");
    }

    /* Return the property list to the free list */
    H5FL_FREE(H5P_t,plist);

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
H5Pget_class(hid_t plist_id)
{
    H5I_type_t		    group;
    H5P_class_t		    ret_value = H5P_NO_CLASS;

    FUNC_ENTER(H5Pget_class, H5P_NO_CLASS);
    H5TRACE1("p","i",plist_id);

    if ((group = H5I_get_type(plist_id)) < 0 ||
#ifndef NDEBUG
            group >= H5I_TEMPLATE_MAX ||
#endif
            group < H5I_TEMPLATE_0) {
        HRETURN_ERROR(H5E_ATOM, H5E_BADATOM, H5P_NO_CLASS,
		      "not a property list");
    }

    ret_value = (H5P_class_t)(group - H5I_TEMPLATE_0);

    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5P_get_class
 *
 * Purpose:	Internal function for getting the property list class.
 *
 * Return:	Success:	A property list class
 *
 *		Failure:	H5P_NO_CLASS (-1)
 *
 * Programmer:	Robb Matzke
 *              Tuesday, July  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5P_class_t
H5P_get_class(hid_t plist_id)
{
    H5I_type_t		    group;
    H5P_class_t		    ret_value = H5P_NO_CLASS;

    FUNC_ENTER(H5P_get_class, H5P_NO_CLASS);

    if ((group = H5I_get_type(plist_id)) < 0 ||
#ifndef NDEBUG
            group >= H5I_TEMPLATE_MAX ||
#endif
            group < H5I_TEMPLATE_0) {
        HRETURN_ERROR(H5E_ATOM, H5E_BADATOM, H5P_NO_CLASS,
		      "not a property list");
    }

    ret_value = (H5P_class_t)(group - H5I_TEMPLATE_0);
    FUNC_LEAVE(ret_value);
}
    

/*-------------------------------------------------------------------------
 * Function:	H5Pcopy
 *
 * Purpose:	Deep-copies a property list PLIST_ID.
 *
 * Return:	Success:	The ID of the new copy of the property list.
 *				The ID will be different than the input ID
 *				since the new ID refers to a completely
 *				separate copy of the the structure that the
 *				original ID points to.
 *
 *		Failure:	Negative
 *
 * Programmer:	Unknown
 *
 * Modifications:
 *		Robb Matzke, 1999-08-03
 *		If PLIST_ID is H5P_DEFAULT then we return H5P_DEFAULT.
 *-------------------------------------------------------------------------
 */
hid_t
H5Pcopy(hid_t plist_id)
{
    const void		   *plist = NULL;
    void		   *new_plist = NULL;
    H5P_class_t		    type;
    hid_t		    ret_value = FAIL;
    H5I_type_t		    group;

    FUNC_ENTER(H5Pcopy, FAIL);
    H5TRACE1("i","i",plist_id);

    if (H5P_DEFAULT==plist_id)
        return H5P_DEFAULT;

    /* Check args */
    if (NULL == (plist = H5I_object(plist_id)) ||
	(type = H5P_get_class(plist_id)) < 0 ||
	(group = H5I_get_type(plist_id)) < 0) {
	HRETURN_ERROR(H5E_ATOM, H5E_BADATOM, FAIL,
		      "unable to unatomize property list");
    }

    /* Copy it */
    if (NULL==(new_plist=H5P_copy (type, plist))) {
	HRETURN_ERROR (H5E_INTERNAL, H5E_CANTINIT, FAIL,
		       "unable to copy property list");
    }

    /* Register the atom for the new property list */
    if ((ret_value = H5I_register(group, new_plist)) < 0) {
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
 *		Robb Matzke, 1999-08-03
 *		Modified to use the virtual file layer.
 *-------------------------------------------------------------------------
 */
void *
H5P_copy (H5P_class_t type, const void *src)
{
    size_t		size;
    H5P_t		*dst = NULL;
    const H5D_create_t	*dc_src = NULL;
    H5D_create_t	*dc_dst = NULL;
    H5F_access_t	*fa_dst = NULL;
    H5F_xfer_t		*dx_dst = NULL;
    
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

        case H5P_DATA_XFER:
            size = sizeof(H5F_xfer_t);
            break;

        case H5P_MOUNT:
            size = sizeof(H5F_mprop_t);
            break;

        default:
            HRETURN_ERROR(H5E_ARGS, H5E_BADRANGE, NULL,
                  "unknown property list class");
    }

    /* Create the new property list */
    if (NULL==(dst = H5FL_ALLOC(H5P_t,0))) {
        HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,
               "memory allocation failed");
    }

    /* Copy into new object */
    HDmemcpy(dst, src, size);

    /* Set the type of the property list */
    dst->class=type;

    /* Deep-copy pointers */
    switch (type) {
        case H5P_FILE_CREATE:
            break;
        
        case H5P_FILE_ACCESS:
            fa_dst = (H5F_access_t*)dst;

            if (fa_dst->driver_id>=0) {
                H5I_inc_ref(fa_dst->driver_id);
                fa_dst->driver_info = H5FD_fapl_copy(fa_dst->driver_id,
                                fa_dst->driver_info);
            }
            break;
        
        case H5P_DATASET_CREATE:
            dc_src = (const H5D_create_t*)src;
            dc_dst = (H5D_create_t*)dst;

            /* Copy the fill value */
            if (NULL==H5O_copy(H5O_FILL, &(dc_src->fill), &(dc_dst->fill))) {
                HRETURN_ERROR(H5E_PLIST, H5E_CANTINIT, NULL,
                      "unabe to copy fill value message");
            }
            
            /* Copy the external file list */
            HDmemset(&(dc_dst->efl), 0, sizeof(dc_dst->efl));
            if (NULL==H5O_copy(H5O_EFL, &(dc_src->efl), &(dc_dst->efl))) {
                HRETURN_ERROR(H5E_PLIST, H5E_CANTINIT, NULL,
                      "unable to copy external file list message");
            }

            /* Copy the filter pipeline */
            if (NULL==H5O_copy(H5O_PLINE, &(dc_src->pline), &(dc_dst->pline))) {
                HRETURN_ERROR(H5E_PLIST, H5E_CANTINIT, NULL,
                      "unable to copy filter pipeline message");
            }
            break;
        
        case H5P_DATA_XFER:
            dx_dst = (H5F_xfer_t*)dst;

            if (dx_dst->driver_id>=0) {
                H5I_inc_ref(dx_dst->driver_id);
                dx_dst->driver_info = H5FD_dxpl_copy(dx_dst->driver_id,
                                dx_dst->driver_info);
            }
            break;

        case H5P_MOUNT:
            /* Nothing to do */
            break;

        default:
            HRETURN_ERROR(H5E_ARGS, H5E_BADRANGE, NULL,
                  "unknown property list class");
    }

    FUNC_LEAVE (dst);
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
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_version(hid_t plist_id, int *boot/*out*/, int *freelist/*out*/,
	       int *stab/*out*/, int *shhdr/*out*/)
{
    H5F_create_t	   *plist = NULL;

    FUNC_ENTER(H5Pget_version, FAIL);
    H5TRACE5("e","ixxxx",plist_id,boot,freelist,stab,shhdr);

    /* Check arguments */
    if (H5P_FILE_CREATE != H5P_get_class(plist_id) ||
            NULL == (plist = H5I_object(plist_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file creation property list");
    }

    /* Get values */
    if (boot)
        *boot = plist->bootblock_ver;
    if (freelist)
        *freelist = plist->freespace_ver;
    if (stab)
        *stab = plist->objectdir_ver;
    if (shhdr)
        *shhdr = plist->sharedheader_ver;

    FUNC_LEAVE(SUCCEED);
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
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_userblock(hid_t plist_id, hsize_t size)
{
    uintn		    i;
    H5F_create_t	   *plist = NULL;

    FUNC_ENTER(H5Pset_userblock, FAIL);
    H5TRACE2("e","ih",plist_id,size);

    /* Check arguments */
    if (H5P_FILE_CREATE != H5P_get_class(plist_id) ||
            NULL == (plist = H5I_object(plist_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file creation property list");
    }
    for (i=8; i<8*sizeof(hsize_t); i++) {
        hsize_t p2 = 8==i ? 0 : ((hsize_t)1<<i);
        if (size == p2)
            break;
    }

    if (i>=8*sizeof(hsize_t)) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		      "userblock size is not valid");
    }

    /* Set value */
    plist->userblock_size = size;

    FUNC_LEAVE(SUCCEED);
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
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_userblock(hid_t plist_id, hsize_t *size)
{
    H5F_create_t	*plist = NULL;

    FUNC_ENTER(H5Pget_userblock, FAIL);
    H5TRACE2("e","i*h",plist_id,size);

    /* Check args */
    if (H5P_FILE_CREATE != H5P_get_class(plist_id) ||
            NULL == (plist = H5I_object(plist_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file creation property list");
    }

    /* Get value */
    if (size)
        *size = plist->userblock_size;

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
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Tuesday, June  9, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_alignment(hid_t fapl_id, hsize_t threshold, hsize_t alignment)
{
    H5F_access_t	*fapl = NULL;
    
    FUNC_ENTER (H5Pset_alignment, FAIL);
    H5TRACE3("e","ihh",fapl_id,threshold,alignment);

    /* Check args */
    if (H5P_FILE_ACCESS != H5P_get_class (fapl_id) ||
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
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Tuesday, June  9, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_alignment(hid_t fapl_id, hsize_t *threshold/*out*/,
		  hsize_t *alignment/*out*/)
{
    H5F_access_t	*fapl = NULL;

    FUNC_ENTER (H5Pget_alignment, FAIL);
    H5TRACE3("e","ixx",fapl_id,threshold,alignment);

    /* Check args */
    if (H5P_FILE_ACCESS != H5P_get_class (fapl_id) ||
            NULL == (fapl = H5I_object (fapl_id))) {
        HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "not a file access property list");
    }

    /* Get values */
    if (threshold)
        *threshold = fapl->threshold;
    if (alignment)
        *alignment = fapl->alignment;

    FUNC_LEAVE (SUCCEED);
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
    H5F_create_t	   *plist = NULL;

    FUNC_ENTER(H5Pset_sizes, FAIL);
    H5TRACE3("e","izz",plist_id,sizeof_addr,sizeof_size);

    /* Check arguments */
    if (H5P_FILE_CREATE != H5P_get_class(plist_id) ||
            NULL == (plist = H5I_object(plist_id))) {
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
        plist->sizeof_addr = sizeof_addr;
    if (sizeof_size)
        plist->sizeof_size = sizeof_size;

    FUNC_LEAVE(SUCCEED);
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
    H5F_create_t	   *plist = NULL;

    FUNC_ENTER(H5Pget_sizes, FAIL);
    H5TRACE3("e","ixx",plist_id,sizeof_addr,sizeof_size);

    /* Check args */
    if (H5P_FILE_CREATE != H5P_get_class(plist_id) ||
            NULL == (plist = H5I_object(plist_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file creation property list");
    }

    /* Get values */
    if (sizeof_addr)
        *sizeof_addr = plist->sizeof_addr;
    if (sizeof_size)
        *sizeof_size = plist->sizeof_size;

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
H5Pset_sym_k(hid_t plist_id, int ik, int lk)
{
    H5F_create_t	   *plist = NULL;

    FUNC_ENTER(H5Pset_sym_k, FAIL);
    H5TRACE3("e","iIsIs",plist_id,ik,lk);

    /* Check arguments */
    if (H5P_FILE_CREATE != H5P_get_class(plist_id) ||
            NULL == (plist = H5I_object(plist_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file creation property list");
    }

    /* Set values */
    if (ik > 0)
        plist->btree_k[H5B_SNODE_ID] = ik;
    if (lk > 0)
        plist->sym_leaf_k = lk;

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
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_sym_k(hid_t plist_id, int *ik /*out */ , int *lk /*out */ )
{
    H5F_create_t	   *plist = NULL;

    FUNC_ENTER(H5Pget_sym_k, FAIL);
    H5TRACE3("e","ixx",plist_id,ik,lk);

    /* Check arguments */
    if (H5P_FILE_CREATE != H5P_get_class(plist_id) ||
            NULL == (plist = H5I_object(plist_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file creation property list");
    }
    /* Get values */
    if (ik)
        *ik = plist->btree_k[H5B_SNODE_ID];
    if (lk)
        *lk = plist->sym_leaf_k;

    FUNC_LEAVE(SUCCEED);
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
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_istore_k(hid_t plist_id, int ik)
{
    H5F_create_t	   *plist = NULL;

    FUNC_ENTER(H5Pset_istore_k, FAIL);
    H5TRACE2("e","iIs",plist_id,ik);

    /* Check arguments */
    if (H5P_FILE_CREATE != H5P_get_class(plist_id) ||
            NULL == (plist = H5I_object(plist_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file creation property list");
    }
    if (ik <= 0) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		      "istore IK value must be positive");
    }

    /* Set value */
    plist->btree_k[H5B_ISTORE_ID] = ik;

    FUNC_LEAVE(SUCCEED);
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
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_istore_k(hid_t plist_id, int *ik /*out */ )
{
    H5F_create_t	   *plist = NULL;

    FUNC_ENTER(H5Pget_istore_k, FAIL);
    H5TRACE2("e","ix",plist_id,ik);

    /* Check arguments */
    if (H5P_FILE_CREATE != H5P_get_class(plist_id) ||
            NULL == (plist = H5I_object(plist_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file creation property list");
    }

    /* Get value */
    if (ik)
        *ik = plist->btree_k[H5B_ISTORE_ID];

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_layout
 *
 * Purpose:	Sets the layout of raw data in the file.
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
H5Pset_layout(hid_t plist_id, H5D_layout_t layout)
{
    H5D_create_t	   *plist = NULL;

    FUNC_ENTER(H5Pset_layout, FAIL);
    H5TRACE2("e","iDl",plist_id,layout);

    /* Check arguments */
    if (H5P_DATASET_CREATE != H5P_get_class(plist_id) ||
            NULL == (plist = H5I_object(plist_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a dataset creation property list");
    }
    if (layout < 0 || layout >= H5D_NLAYOUTS) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL,
		      "raw data layout method is not valid");
    }

    /* Set value */
    plist->layout = layout;

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5Pget_layout
 *
 * Purpose:	Retrieves layout type of a dataset creation property list.
 *
 * Return:	Success:	The layout type
 *
 *		Failure:	H5D_LAYOUT_ERROR (negative)
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5D_layout_t
H5Pget_layout(hid_t plist_id)
{
    H5D_create_t	   *plist = NULL;

    FUNC_ENTER(H5Pget_layout, H5D_LAYOUT_ERROR);
    H5TRACE1("Dl","i",plist_id);

    /* Check arguments */
    if (H5P_DATASET_CREATE != H5P_get_class(plist_id) ||
            NULL == (plist = H5I_object(plist_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, H5D_LAYOUT_ERROR,
		      "not a dataset creation property list");
    }

    FUNC_LEAVE(plist->layout);
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
H5Pset_chunk(hid_t plist_id, int ndims, const hsize_t dim[/*ndims*/])
{
    int			    i;
    H5D_create_t	   *plist = NULL;

    FUNC_ENTER(H5Pset_chunk, FAIL);
    H5TRACE3("e","iIs*[a1]h",plist_id,ndims,dim);

    /* Check arguments */
    if (H5P_DATASET_CREATE != H5P_get_class(plist_id) ||
            NULL == (plist = H5I_object(plist_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a dataset creation property list");
    }
    if (ndims <= 0) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL,
		      "chunk dimensionality must be positive");
    }
    if (ndims > H5S_MAX_RANK) {
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
    plist->layout = H5D_CHUNKED;
    plist->chunk_ndims = ndims;
    for (i = 0; i < ndims; i++)
        plist->chunk_size[i] = dim[i];

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
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5Pget_chunk(hid_t plist_id, int max_ndims, hsize_t dim[]/*out*/)
{
    int			i;
    H5D_create_t	*plist = NULL;

    FUNC_ENTER(H5Pget_chunk, FAIL);
    H5TRACE3("Is","iIsx",plist_id,max_ndims,dim);

    /* Check arguments */
    if (H5P_DATASET_CREATE != H5P_get_class(plist_id) ||
            NULL == (plist = H5I_object(plist_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a dataset creation property list");
    }
    if (H5D_CHUNKED != plist->layout) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		      "not a chunked storage layout");
    }

    for (i=0; i<plist->chunk_ndims && i<max_ndims && dim; i++)
        dim[i] = plist->chunk_size[i];

    FUNC_LEAVE(plist->chunk_ndims);
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
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Tuesday, March	3, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_external(hid_t plist_id, const char *name, off_t offset, hsize_t size)
{
    int			idx;
    size_t		total, tmp;
    H5D_create_t	*plist = NULL;

    FUNC_ENTER(H5Pset_external, FAIL);
    H5TRACE4("e","isoh",plist_id,name,offset,size);

    /* Check arguments */
    if (H5P_DATASET_CREATE != H5P_get_class(plist_id) ||
            NULL == (plist = H5I_object(plist_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a dataset creation property list");
    }
    if (!name || !*name) {
        HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "no name given");
    }
    if (offset<0) {
        HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "negative external file offset");
    }
    if (size<=0) {
        HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "zero size");
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
        intn na = plist->efl.nalloc + H5O_EFL_ALLOC;
        H5O_efl_entry_t *x = H5MM_realloc (plist->efl.slot,
                           na*sizeof(H5O_efl_entry_t));

        if (!x) {
            HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,
                   "memory allocation failed");
        }
        plist->efl.nalloc = na;
        plist->efl.slot = x;
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
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March  3, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5Pget_external_count(hid_t plist_id)
{
    H5D_create_t	*plist = NULL;
    
    FUNC_ENTER (H5Pget_external_count, FAIL);
    H5TRACE1("Is","i",plist_id);
    
    /* Check arguments */
    if (H5P_DATASET_CREATE != H5P_get_class(plist_id) ||
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
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March  3, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_external(hid_t plist_id, int idx, size_t name_size, char *name/*out*/,
		 off_t *offset/*out*/, hsize_t *size/*out*/)
{
    H5D_create_t	*plist = NULL;
    
    FUNC_ENTER (H5Pget_external, FAIL);
    H5TRACE6("e","iIszxxx",plist_id,idx,name_size,name,offset,size);
    
    /* Check arguments */
    if (H5P_DATASET_CREATE != H5P_get_class(plist_id) ||
            NULL == (plist = H5I_object(plist_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a dataset creation property list");
    }
    if (idx<0 || idx>=plist->efl.nused) {
        HRETURN_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL,
		       "external file index is out of range");
    }

    /* Return values */
    if (name_size>0 && name)
        HDstrncpy (name, plist->efl.slot[idx].name, name_size);
    if (offset)
        *offset = plist->efl.slot[idx].offset;
    if (size)
        *size = plist->efl.slot[idx].size;

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_driver
 *
 * Purpose:	Set the file driver (DRIVER_ID) for a file access or data
 *		transfer property list (PLIST_ID) and supply an optional
 *		struct containing the driver-specific properites
 *		(DRIVER_INFO).  The driver properties will be copied into the
 *		property list and the reference count on the driver will be
 *		incremented, allowing the caller to close the driver ID but
 *		still use the property list.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Tuesday, August  3, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_driver(hid_t plist_id, hid_t driver_id, const void *driver_info)
{
    H5F_access_t	*fapl=NULL;
    H5F_xfer_t		*dxpl=NULL;
    
    FUNC_ENTER(H5Pset_driver, FAIL);
    H5TRACE3("e","iix",plist_id,driver_id,driver_info);

    if (H5I_VFL!=H5I_get_type(driver_id) ||
            NULL==H5I_object(driver_id)) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file driver ID");
    }

    if (H5P_FILE_ACCESS==H5P_get_class(plist_id)) {
        if (NULL==(fapl=H5I_object(plist_id))) {
            HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
                  "not a file access property list");
        }
	
        /* Remove old driver */
        assert(fapl->driver_id>=0);
        H5FD_fapl_free(fapl->driver_id, fapl->driver_info);
        H5I_dec_ref(fapl->driver_id);

        /* Add new driver */
        H5I_inc_ref(driver_id);
        fapl->driver_id = driver_id;
        fapl->driver_info = H5FD_fapl_copy(driver_id, driver_info);
        
    } else if (H5P_DATA_XFER==H5P_get_class(plist_id)) {
        if (NULL==(dxpl=H5I_object(plist_id))) {
            HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
                  "not a file access property list");
        }

        /* Remove old driver */
        if (dxpl->driver_id>=0)
            H5FD_dxpl_free(dxpl->driver_id, dxpl->driver_info);

        /* Add new driver */
        H5I_inc_ref(driver_id);
        dxpl->driver_id = driver_id;
        dxpl->driver_info = H5FD_fapl_copy(driver_id, driver_info);
        
    } else {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file access or data transfer property list");
    }

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_driver
 *
 * Purpose:	Return the ID of the low-level file driver.  PLIST_ID should
 *		be a file access property list or data transfer propert list.
 *
 * Return:	Success:	A low-level driver ID which is the same ID
 *				used when the driver was set for the property
 *				list. The driver ID is only valid as long as
 *				the file driver remains registered.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Thursday, February 26, 1998
 *
 * Modifications:
 *		Robb Matzke, 1999-08-03
 *		Rewritten to use the virtual file layer.
 *
 * 		Robb Matzke, 1999-08-05
 *		If the driver ID is -2 then substitute the current value of
 *		H5FD_SEC2.
 *-------------------------------------------------------------------------
 */
hid_t
H5Pget_driver(hid_t plist_id)
{
    H5F_access_t	*fapl=NULL;
    H5F_xfer_t		*dxpl=NULL;
    hid_t		ret_value=-1;

    FUNC_ENTER (H5Pget_driver, FAIL);
    H5TRACE1("i","i",plist_id);

    if (H5P_FILE_ACCESS==H5P_get_class(plist_id) &&
            (fapl=H5I_object(plist_id))) {
        ret_value = fapl->driver_id;
	
    } else if (H5P_DATA_XFER==H5P_get_class(plist_id) &&
	       (dxpl=H5I_object(plist_id))) {
        ret_value = dxpl->driver_id;
	
    } else {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file access or data transfer property list");
    }

    if (-2==ret_value)
        ret_value = H5FD_SEC2;

    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_driver_info
 *
 * Purpose:	Returns a pointer directly to the file driver-specific
 *		information of a file access or data transfer property list.
 *
 * Return:	Success:	Ptr to *uncopied* driver specific data
 *				structure if any.
 *
 *		Failure:	NULL. Null is also returned if the driver has
 *				not registered any driver-specific properties
 *				although no error is pushed on the stack in
 *				this case.
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *
H5Pget_driver_info(hid_t plist_id)
{
    H5F_access_t	*fapl=NULL;
    H5F_xfer_t		*dxpl=NULL;
    void		*ret_value=NULL;

    FUNC_ENTER(H5Pget_driver_info, NULL);

    if (H5P_FILE_ACCESS==H5P_get_class(plist_id) &&
            (fapl=H5I_object(plist_id))) {
        ret_value = fapl->driver_info;
	
    } else if (H5P_DATA_XFER==H5P_get_class(plist_id) &&
	       (dxpl=H5I_object(plist_id))) {
        ret_value = dxpl->driver_info;
	
    } else {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, NULL,
		      "not a file access or data transfer property list");
    }
    
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_cache
 *
 * Purpose:	Set the number of objects in the meta data cache and the
 *		maximum number of chunks and bytes in the raw data chunk
 *		cache.
 *
 * 		The RDCC_W0 value should be between 0 and 1 inclusive and
 *		indicates how much chunks that have been fully read or fully
 *		written are favored for preemption.  A value of zero means
 *		fully read or written chunks are treated no differently than
 *		other chunks (the preemption is strictly LRU) while a value
 *		of one means fully read chunks are always preempted before
 *		other chunks.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Tuesday, May 19, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_cache(hid_t plist_id, int mdc_nelmts,
	     int rdcc_nelmts, size_t rdcc_nbytes, double rdcc_w0)
{
    H5F_access_t	*fapl = NULL;
    
    FUNC_ENTER (H5Pset_cache, FAIL);
    H5TRACE5("e","iIsIszd",plist_id,mdc_nelmts,rdcc_nelmts,rdcc_nbytes,
             rdcc_w0);

    /* Check arguments */
    if (H5P_FILE_ACCESS!=H5P_get_class (plist_id) ||
            NULL==(fapl=H5I_object (plist_id))) {
        HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "not a file access property list");
    }
    if (mdc_nelmts<0) {
        HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "meta data cache size must be non-negative");
    }
    if (rdcc_nelmts<0) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		      "raw data chunk cache nelmts must be non-negative");
    }
    if (rdcc_w0<0.0 || rdcc_w0>1.0) {
        HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "raw data cache w0 value must be between 0.0 and 1.0 "
		       "inclusive");
    }

    /* Set sizes */
    fapl->mdc_nelmts = mdc_nelmts;
    fapl->rdcc_nelmts = rdcc_nelmts;
    fapl->rdcc_nbytes = rdcc_nbytes;
    fapl->rdcc_w0 = rdcc_w0;

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_cache
 *
 * Purpose:	Retrieves the maximum possible number of elements in the meta
 *		data cache and the maximum possible number of elements and
 *		bytes and the RDCC_W0 value in the raw data chunk cache.  Any
 *		(or all) arguments may be null pointers in which case the
 *		corresponding datum is not returned.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Tuesday, May 19, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_cache(hid_t plist_id, int *mdc_nelmts,
	     int *rdcc_nelmts, size_t *rdcc_nbytes, double *rdcc_w0)
{
    H5F_access_t	*fapl = NULL;
    
    FUNC_ENTER (H5Pget_cache, FAIL);
    H5TRACE5("e","i*Is*Is*z*d",plist_id,mdc_nelmts,rdcc_nelmts,rdcc_nbytes,
             rdcc_w0);

    /* Check arguments */
    if (H5P_FILE_ACCESS!=H5P_get_class (plist_id) ||
            NULL==(fapl=H5I_object (plist_id))) {
        HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "not a file access property list");
    }

    /* Get sizes */
    if (mdc_nelmts)
        *mdc_nelmts = fapl->mdc_nelmts;
    if (rdcc_nelmts)
        *rdcc_nelmts = fapl->rdcc_nelmts;
    if (rdcc_nbytes)
        *rdcc_nbytes = fapl->rdcc_nbytes;
    if (rdcc_w0)
        *rdcc_w0 = fapl->rdcc_w0;

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
 *		strip mining will be used.
 *
 *		If TCONV and/or BKG are null pointers then buffers will be
 *		allocated and freed during the data transfer.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Monday, March 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_buffer(hid_t plist_id, size_t size, void *tconv, void *bkg)
{
    H5F_xfer_t		*plist = NULL;
    
    FUNC_ENTER (H5Pset_buffer, FAIL);
    H5TRACE4("e","izxx",plist_id,size,tconv,bkg);

    /* Check arguments */
    if (H5P_DATA_XFER != H5P_get_class (plist_id) ||
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
H5Pget_buffer(hid_t plist_id, void **tconv/*out*/, void **bkg/*out*/)
{
    H5F_xfer_t		*plist = NULL;
    
    FUNC_ENTER (H5Pget_buffer, 0);
    H5TRACE3("z","ixx",plist_id,tconv,bkg);

    /* Check arguments */
    if (H5P_DATA_XFER != H5P_get_class (plist_id) ||
            NULL == (plist = H5I_object (plist_id))) {
        HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, 0,
		       "not a dataset transfer property list");
    }

    /* Return values */
    if (tconv)
        *tconv = plist->tconv_buf;
    if (bkg)
        *bkg = plist->bkg_buf;

    FUNC_LEAVE (plist->buf_size);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_hyper_cache
 *
 * Purpose:	Given a dataset transfer property list, indicate whether to
 *		cache the hyperslab blocks during the I/O (which speeds
 *		things up) and the maximum size of the hyperslab block to
 *		cache.  If a block is smaller than to limit, it may still not
 *		be cached if no memory is available. Setting the limit to 0
 *		indicates no limitation on the size of block to attempt to
 *		cache.
 *
 *		The default is to cache blocks with no limit on block size
 *		for serial I/O and to not cache blocks for parallel I/O
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Monday, September 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_hyper_cache(hid_t plist_id, unsigned cache, unsigned limit)
{
    H5F_xfer_t		*plist = NULL;
    
    FUNC_ENTER (H5Pset_hyper_cache, FAIL);
    H5TRACE3("e","iIuIu",plist_id,cache,limit);

    /* Check arguments */
    if (H5P_DATA_XFER != H5P_get_class (plist_id) ||
            NULL == (plist = H5I_object (plist_id))) {
        HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "not a dataset transfer property list");
    }

    /* Update property list */
    plist->cache_hyper = (cache>0) ? 1 : 0;
    plist->block_limit = limit;

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_hyper_cache
 *
 * Purpose:	Reads values previously set with H5Pset_hyper_cache().
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Monday, September 21, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_hyper_cache(hid_t plist_id, unsigned *cache/*out*/,
		   unsigned *limit/*out*/)
{
    H5F_xfer_t		*plist = NULL;
    
    FUNC_ENTER (H5Pget_hyper_cache, FAIL);
    H5TRACE3("e","ixx",plist_id,cache,limit);

    /* Check arguments */
    if (H5P_DATA_XFER != H5P_get_class (plist_id) ||
            NULL == (plist = H5I_object (plist_id))) {
        HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "not a dataset transfer property list");
    }

    /* Return values */
    if (cache)
        *cache = plist->cache_hyper;
    if (limit)
        *limit = plist->block_limit;

    FUNC_LEAVE (SUCCEED);
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
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March 17, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_preserve(hid_t plist_id, hbool_t status)
{
    H5F_xfer_t		*plist = NULL;
    
    FUNC_ENTER (H5Pset_preserve, FAIL);
    H5TRACE2("e","ib",plist_id,status);

    /* Check arguments */
    if (H5P_DATA_XFER != H5P_get_class (plist_id) ||
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
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March 17, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5Pget_preserve(hid_t plist_id)
{
    H5F_xfer_t		*plist = NULL;
    
    FUNC_ENTER (H5Pget_preserve, FAIL);
    H5TRACE1("Is","i",plist_id);

    /* Check arguments */
    if (H5P_DATA_XFER != H5P_get_class (plist_id) ||
            NULL == (plist = H5I_object (plist_id))) {
        HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "not a dataset transfer property list");
    }

    FUNC_LEAVE (plist->need_bkg?TRUE:FALSE);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_filter
 *
 * Purpose:	Adds the specified FILTER and corresponding properties to the
 *		end of the transient or permanent output filter pipeline
 *		depending on whether PLIST is a dataset creation or dataset
 *		transfer property list.  The FLAGS argument specifies certain
 *		general properties of the filter and is documented below.
 *		The CD_VALUES is an array of CD_NELMTS integers which are
 *		auxiliary data for the filter.  The integer vlues will be
 *		stored in the dataset object header as part of the filter
 *		information.
 *
 * 		The FLAGS argument is a bit vector of the following fields:
 *
 * 		H5Z_FLAG_OPTIONAL(0x0001)
 *		If this bit is set then the filter is optional.  If the
 *		filter fails during an H5Dwrite() operation then the filter
 *		is just excluded from the pipeline for the chunk for which it
 *		failed; the filter will not participate in the pipeline
 *		during an H5Dread() of the chunk.  If this bit is clear and
 *		the filter fails then the entire I/O operation fails.
 *
 * Note:	This function currently supports only the permanent filter
 *		pipeline.  That is, PLIST_ID must be a dataset creation
 *		property list.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April 15, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_filter(hid_t plist_id, H5Z_filter_t filter, unsigned int flags,
	       size_t cd_nelmts, const unsigned int cd_values[/*cd_nelmts*/])
{
    H5D_create_t	*plist = NULL;
    
    FUNC_ENTER (H5Pset_filter, FAIL);
    H5TRACE5("e","iZfIuz*[a3]Iu",plist_id,filter,flags,cd_nelmts,cd_values);

    /* Check arguments */
    if (H5P_DATA_XFER==H5P_get_class(plist_id)) {
        HRETURN_ERROR(H5E_PLINE, H5E_UNSUPPORTED, FAIL,
		      "transient pipelines are not supported yet");
    }
    if (H5P_DATASET_CREATE!=H5P_get_class (plist_id) ||
            NULL==(plist=H5I_object (plist_id))) {
        HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "not a dataset creation property list");
    }
    if (filter<0 || filter>H5Z_FILTER_MAX) {
        HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "invalid filter identifier");
    }
    if (flags & ~H5Z_FLAG_DEFMASK) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		      "invalid flags");
    }
    if (cd_nelmts>0 && !cd_values) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		      "no client data values supplied");
    }

    /* Do it */
    if (H5Z_append(&(plist->pline), filter, flags, cd_nelmts, cd_values)<0) {
        HRETURN_ERROR(H5E_PLINE, H5E_CANTINIT, FAIL,
		      "unable to add filter to pipeline");
    }

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_nfilters
 *
 * Purpose:	Returns the number of filters in the permanent or transient
 *		pipeline depending on whether PLIST_ID is a dataset creation
 *		or dataset transfer property list.  In each pipeline the
 *		filters are numbered from zero through N-1 where N is the
 *		value returned by this function.  During output to the file
 *		the filters of a pipeline are applied in increasing order
 *		(the inverse is true for input).
 *
 * Note:	Only permanent filters are supported at this time.
 *
 * Return:	Success:	Number of filters or zero if there are none.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Tuesday, August  4, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5Pget_nfilters(hid_t plist_id)
{
    H5D_create_t	*plist = NULL;
    
    FUNC_ENTER(H5Pget_nfilters, FAIL);
    H5TRACE1("Is","i",plist_id);

    if (H5P_DATA_XFER==H5P_get_class(plist_id)) {
        HRETURN_ERROR(H5E_PLINE, H5E_UNSUPPORTED, FAIL,
		      "transient pipelines are not supported yet");
    }
    if (H5P_DATASET_CREATE!=H5P_get_class(plist_id) ||
            NULL==(plist=H5I_object(plist_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a dataset creation property list");
    }

    FUNC_LEAVE((int)(plist->pline.nfilters));
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_filter
 *
 * Purpose:	This is the query counterpart of H5Pset_filter() and returns
 *		information about a particular filter number in a permanent
 *		or transient pipeline depending on whether PLIST_ID is a
 *		dataset creation or transfer property list.  On input,
 *		CD_NELMTS indicates the number of entries in the CD_VALUES
 *		array allocated by the caller while on exit it contains the
 *		number of values defined by the filter.  The IDX should be a
 *		value between zero and N-1 as described for H5Pget_nfilters()
 *		and the function will return failure if the filter number is
 *		out or range.
 * 
 * Return:	Success:	Filter identification number.
 *
 *		Failure:	H5Z_FILTER_ERROR (Negative)
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April 15, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5Z_filter_t
H5Pget_filter(hid_t plist_id, int idx, unsigned int *flags/*out*/,
	       size_t *cd_nelmts/*in_out*/, unsigned cd_values[]/*out*/,
	       size_t namelen, char name[]/*out*/)
{
    H5D_create_t	*plist = NULL;
    size_t		i;
    
    FUNC_ENTER (H5Pget_filter, H5Z_FILTER_ERROR);
    H5TRACE7("Zf","iIsx*zxzx",plist_id,idx,flags,cd_nelmts,cd_values,namelen,
             name);
    
    /* Check arguments */
    if (H5P_DATA_XFER==H5P_get_class(plist_id)) {
        HRETURN_ERROR(H5E_PLINE, H5E_UNSUPPORTED, H5Z_FILTER_ERROR,
		      "transient filters are not supported yet");
    }
    if (H5P_DATASET_CREATE!=H5P_get_class (plist_id) ||
            NULL==(plist=H5I_object (plist_id))) {
        HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, H5Z_FILTER_ERROR,
		       "not a dataset creation property list");
    }
    if (idx<0 || (size_t)idx>=plist->pline.nfilters) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, H5Z_FILTER_ERROR,
		      "filter number is invalid");
    }
    if (cd_nelmts || cd_values) {
        if (cd_nelmts && *cd_nelmts>256) {
            /*
             * It's likely that users forget to initialize this on input, so
             * we'll check that it has a reasonable value.  The actual number
             * is unimportant because the H5O layer will detect when a message
             * is too large.
             */
            HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, H5Z_FILTER_ERROR,
                  "probable uninitialized *cd_nelmts argument");
        }
        if (cd_nelmts && *cd_nelmts>0 && !cd_values) {
            HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, H5Z_FILTER_ERROR,
                  "client data values not supplied");
        }

        /*
         * If cd_nelmts is null but cd_values is non-null then just ignore
         * cd_values
         */
        if (!cd_nelmts)
            cd_values = NULL;
    }

    /* Output values */
    if (flags)
        *flags = plist->pline.filter[idx].flags;
    if (cd_values) {
        for (i=0; i<plist->pline.filter[idx].cd_nelmts && i<*cd_nelmts; i++) {
            cd_values[i] = plist->pline.filter[idx].cd_values[i];
        }
    }
    if (cd_nelmts)
        *cd_nelmts = plist->pline.filter[idx].cd_nelmts;

    if (namelen>0 && name) {
        const char *s = plist->pline.filter[idx].name;
        if (!s) {
            H5Z_class_t *cls = H5Z_find(plist->pline.filter[idx].id);

            if (cls)
                s = cls->name;
        }
        if (s)
            HDstrncpy(name, s, namelen);
        else
            name[0] = '\0';
    }
    
    FUNC_LEAVE (plist->pline.filter[idx].id);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_deflate
 *
 * Purpose:	Sets the compression method for a permanent or transient
 *		filter pipeline (depending on whether PLIST_ID is a dataset
 *		creation or transfer property list) to H5Z_FILTER_DEFLATE
 *		and the compression level to LEVEL which should be a value
 *		between zero and nine, inclusive.  Lower compression levels
 *		are faster but result in less compression.  This is the same
 *		algorithm as used by the GNU gzip program.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April 15, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_deflate(hid_t plist_id, unsigned level)
{
    H5D_create_t	*plist = NULL;
    
    FUNC_ENTER (H5Pset_deflate, FAIL);
    H5TRACE2("e","iIu",plist_id,level);

    /* Check arguments */
    if (H5P_DATA_XFER==H5P_get_class(plist_id)) {
        HRETURN_ERROR(H5E_PLINE, H5E_UNSUPPORTED, FAIL,
		      "transient filter pipelines are not supported yet");
    }
    if (H5P_DATASET_CREATE!=H5P_get_class (plist_id) ||
            NULL==(plist=H5I_object (plist_id))) {
        HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "not a dataset creation property list");
    }
    if (level>9) {
        HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		       "invalid deflate level");
    }

    /* Add the filter */
    if (H5Z_append(&(plist->pline), H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL,
		   1, &level)<0) {
        HRETURN_ERROR(H5E_PLINE, H5E_CANTINIT, FAIL,
		      "unable to add deflate filter to pipeline");
    }

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_btree_ratios
 *
 * Purpose:	Queries B-tree split ratios.  See H5Pset_btree_ratios().
 *
 * Return:	Success:	Non-negative with split ratios returned through
 *				the non-null arguments.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Monday, September 28, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_btree_ratios(hid_t plist_id, double *left/*out*/, double *middle/*out*/,
		    double *right/*out*/)
{
    H5F_xfer_t		*plist = NULL;

    FUNC_ENTER(H5Pget_btree_ratios, FAIL);
    H5TRACE4("e","ixxx",plist_id,left,middle,right);

    /* Check arguments */
    if (H5P_DATA_XFER!=H5P_get_class(plist_id) ||
            NULL==(plist=H5I_object(plist_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a dataset transfer property list");
    }

    /* Get values */
    if (left)
        *left = plist->split_ratios[0];
    if (middle)
        *middle = plist->split_ratios[1];
    if (right)
        *right = plist->split_ratios[2];

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_btree_ratios
 *
 * Purpose:	Sets B-tree split ratios for a dataset transfer property
 *		list. The split ratios determine what percent of children go
 *		in the first node when a node splits.  The LEFT ratio is
 *		used when the splitting node is the left-most node at its
 *		level in the tree; the RIGHT ratio is when the splitting node
 *		is the right-most node at its level; and the MIDDLE ratio for
 *		all other cases.  A node which is the only node at its level
 *		in the tree uses the RIGHT ratio when it splits.  All ratios
 *		are real numbers between 0 and 1, inclusive.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Monday, September 28, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_btree_ratios(hid_t plist_id, double left, double middle,
		    double right)
{
    H5F_xfer_t		*plist = NULL;

    FUNC_ENTER(H5Pget_btree_ratios, FAIL);
    H5TRACE4("e","iddd",plist_id,left,middle,right);

    /* Check arguments */
    if (H5P_DATA_XFER!=H5P_get_class(plist_id) ||
            NULL==(plist=H5I_object(plist_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a dataset transfer property list");
    }
    if (left<0.0 || left>1.0 || middle<0.0 || middle>1.0 ||
            right<0.0 || right>1.0) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		      "split ratio must satisfy 0.0<=X<=1.0");
    }
    
    /* Set values */
    plist->split_ratios[0] = left;
    plist->split_ratios[1] = middle;
    plist->split_ratios[2] = right;

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_fill_value
 *
 * Purpose:	Set the fill value for a dataset creation property list. The
 *		VALUE is interpretted as being of type TYPE, which need not
 *		be the same type as the dataset but the library must be able
 *		to convert VALUE to the dataset type when the dataset is
 *		created.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  1, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fill_value(hid_t plist_id, hid_t type_id, const void *value)
{
    H5D_create_t	*plist = NULL;
    H5T_t		*type = NULL;
    
    FUNC_ENTER(H5Pset_fill_value, FAIL);
    H5TRACE3("e","iix",plist_id,type_id,value);

    /* Check arguments */
    if (H5P_DATASET_CREATE!=H5P_get_class(plist_id) ||
            NULL==(plist=H5I_object(plist_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a dataset creation property list");
    }
    if (H5I_DATATYPE!=H5I_get_type(type_id) ||
            NULL==(type=H5I_object(type_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    }
    if (!value) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no fill value specified");
    }

    /* Set the fill value */
    H5O_reset(H5O_FILL, &(plist->fill));
    if (NULL==(plist->fill.type=H5T_copy(type, H5T_COPY_TRANSIENT))) {
        HRETURN_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL,
		      "unable to copy data type");
    }
    plist->fill.size = H5T_get_size(type);
    if (NULL==(plist->fill.buf=H5MM_malloc(plist->fill.size))) {
        HRETURN_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL,
		      "memory allocation failed for fill value");
    }
    HDmemcpy(plist->fill.buf, value, plist->fill.size);

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_fill_value
 *
 * Purpose:	Queries the fill value property of a dataset creation
 *		property list.  The fill value is returned through the VALUE
 *		pointer and the memory is allocated by the caller.  The fill
 *		value will be converted from its current data type to the
 *		specified TYPE.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Thursday, October  1, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_fill_value(hid_t plist_id, hid_t type_id, void *value/*out*/)
{
    H5D_create_t	*plist = NULL;		/*property list		*/
    H5T_t		*type = NULL;		/*data type		*/
    H5T_path_t		*tpath = NULL;		/*type conversion info	*/
    void		*buf = NULL;		/*conversion buffer	*/
    void		*bkg = NULL;		/*conversion buffer	*/
    hid_t		src_id = -1;		/*source data type id	*/
    herr_t		ret_value = FAIL;	/*return value		*/
    
    FUNC_ENTER(H5Pget_fill_value, FAIL);
    H5TRACE3("e","iix",plist_id,type_id,value);

    /* Check arguments */
    if (H5P_DATASET_CREATE!=H5P_get_class(plist_id) ||
            NULL==(plist=H5I_object(plist_id))) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		    "not a dataset creation proprety list");
    }
    if (H5I_DATATYPE!=H5I_get_type(type_id) ||
            NULL==(type=H5I_object(type_id))) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    }
    if (!value) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		    "no fill value output buffer");
    }

    /*
     * If no fill value is defined then return an error.  We can't even
     * return zero because we don't know the data type of the dataset and
     * data type conversion might not have resulted in zero.
     */
    if (NULL==plist->fill.buf) {
        HGOTO_ERROR(H5E_PLIST, H5E_NOTFOUND, FAIL, "no fill value defined");
    }

    /*
     * Can we convert between the source and destination data types?
     */
    if (NULL==(tpath=H5T_path_find(plist->fill.type, type, NULL, NULL))) {
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL,
		    "unable to convert between src and dst data types");
    }
    src_id = H5I_register(H5I_DATATYPE,
			  H5T_copy (plist->fill.type, H5T_COPY_TRANSIENT));
    if (src_id<0) {
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL,
		    "unable to copy/register data type");
    }

    /*
     * Data type conversions are always done in place, so we need a buffer
     * other than the fill value buffer that is large enough for both source
     * and destination.  The app-supplied buffer might do okay.
     */
    if (H5T_get_size(type)>=H5T_get_size(plist->fill.type)) {
        buf = value;
        if (tpath->cdata.need_bkg>=H5T_BKG_TEMP &&
                NULL==(bkg=H5MM_malloc(H5T_get_size(type)))) {
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "memory allocation failed for type conversion");
        }
    } else {
        if (NULL==(buf=H5MM_malloc(H5T_get_size(plist->fill.type)))) {
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "memory allocation failed for type conversion");
        }
        if (tpath->cdata.need_bkg>=H5T_BKG_TEMP)
            bkg = value;
    }
    HDmemcpy(buf, plist->fill.buf, H5T_get_size(plist->fill.type));
        
    /* Do the conversion */
    if (H5T_convert(tpath, src_id, type_id, 1, 0, 0, buf, bkg, H5P_DEFAULT)<0) {
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL,
            "data type conversion failed");
    }
    if (buf!=value)
        HDmemcpy(value, buf, H5T_get_size(type));
    ret_value = SUCCEED;

done:
    if (buf!=value)
        H5MM_xfree(buf);
    if (bkg!=value)
        H5MM_xfree(bkg);
    if (src_id>=0)
        H5I_dec_ref(src_id);
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_gc_references
 *
 * Purpose:	Sets the flag for garbage collecting references for the file.
 *		Dataset region references (and other reference types
 *		probably) use space in the file heap.  If garbage collection
 *		is on and the user passes in an uninitialized value in a
 *		reference structure, the heap might get corrupted.  When
 *		garbage collection is off however and the user re-uses a
 *		reference, the previous heap block will be orphaned and not
 *		returned to the free heap space.  When garbage collection is
 *		on, the user must initialize the reference structures to 0 or
 *		risk heap corruption.
 *
 *		Default value for garbage collecting references is off, just
 *		to be on the safe side.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		June, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_gc_references(hid_t fapl_id, unsigned gc_ref)
{
    H5F_access_t	*fapl = NULL;
    
    FUNC_ENTER(H5Pset_gc_references, FAIL);
    H5TRACE2("e","iIu",fapl_id,gc_ref);

    /* Check args */
    if (H5P_FILE_ACCESS!=H5P_get_class(fapl_id) ||
        NULL==(fapl=H5I_object(fapl_id))) {
            HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file access property list");
    }

    /* Set values */
    fapl->gc_ref = (gc_ref!=0);

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_gc_references
 *
 * Purpose:	Returns the current setting for the garbage collection
 *		references property from a file access property list.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              June, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_gc_references(hid_t fapl_id, unsigned *gc_ref/*out*/)
{
    H5F_access_t	*fapl = NULL;

    FUNC_ENTER(H5Pget_gc_references, FAIL);
    H5TRACE2("e","ix",fapl_id,gc_ref);

    /* Check args */
    if (H5P_FILE_ACCESS!=H5P_get_class(fapl_id) ||
            NULL==(fapl=H5I_object(fapl_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file access property list");
    }

    /* Get values */
    if (gc_ref)
        *gc_ref = fapl->gc_ref;

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_vlen_mem_manager
 *
 * Purpose:	Sets the memory allocate/free pair for VL datatypes.  The
 *		allocation routine is called when data is read into a new
 *		array and the free routine is called when H5Dvlen_reclaim is
 *		called.  The alloc_info and free_info are user parameters
 *		which are passed to the allocation and freeing functions
 *		respectively.  To reset the allocate/free functions to the
 *		default setting of using the system's malloc/free functions,
 *		call this routine with alloc_func and free_func set to NULL.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, July 1, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_vlen_mem_manager(hid_t plist_id, H5MM_allocate_t alloc_func,
        void *alloc_info, H5MM_free_t free_func, void *free_info)
{
    H5F_xfer_t		*plist = NULL;
    
    FUNC_ENTER(H5Pset_vlen_mem_manager, FAIL);
    H5TRACE5("e","ixxxx",plist_id,alloc_func,alloc_info,free_func,free_info);

    /* Check arguments */
    if (H5P_DATA_XFER!=H5P_get_class(plist_id) ||
            NULL==(plist=H5I_object(plist_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a dataset transfer property list");
    }

    /* Update property list */
    plist->vlen_alloc = alloc_func;
    plist->alloc_info = alloc_info;
    plist->vlen_free = free_func;
    plist->free_info = free_info;

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_vlen_mem_manager
 *
 * Purpose:	The inverse of H5Pset_vlen_mem_manager()
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, July 1, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_vlen_mem_manager(hid_t plist_id, H5MM_allocate_t *alloc_func/*out*/,
			void **alloc_info/*out*/,
			H5MM_free_t *free_func/*out*/,
			void **free_info/*out*/)
{
    H5F_xfer_t		*plist = NULL;
    
    FUNC_ENTER(H5Pget_vlen_mem_manager, FAIL);
    H5TRACE5("e","ixxxx",plist_id,alloc_func,alloc_info,free_func,free_info);

    /* Check arguments */
    if (H5P_DATA_XFER!=H5P_get_class(plist_id) ||
            NULL==(plist=H5I_object(plist_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a dataset transfer property list");
    }

    if(alloc_func!=NULL)
        *alloc_func= plist->vlen_alloc;
    if(alloc_info!=NULL)
        *alloc_info= plist->alloc_info;
    if(free_func!=NULL)
        *free_func= plist->vlen_free;
    if(free_info!=NULL)
        *free_info= plist->free_info;

    FUNC_LEAVE (SUCCEED);
}


/*--------------------------------------------------------------------------
 NAME
    H5P_copy_prop
 PURPOSE
    Internal routine to copy a property
 USAGE
    H5P_genprop_t *H5P_copy_prop(oprop)
        H5P_genprop_t *oprop;   IN: Pointer to property to copy
 RETURNS
    Returns a pointer to the newly created property on success,
        NULL on failure.
 DESCRIPTION
    Allocates memory and copies property information into a new property object.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static H5P_genprop_t *
H5P_copy_prop(H5P_genprop_t *oprop)
{
    H5P_genprop_t *prop=NULL;        /* Pointer to new property copied */
    H5P_genprop_t *ret_value=NULL;   /* Return value */

    FUNC_ENTER (H5P_copy_prop, NULL);

    assert(oprop);

    /* Allocate the new property */
    if (NULL==(prop = H5MM_malloc (sizeof(H5P_genprop_t))))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Copy basic property information */
    HDmemcpy(prop,oprop,sizeof(H5P_genprop_t));

    /* Duplicate name */
    prop->name = HDstrdup(oprop->name);

    /* Duplicate current value, if it exists */
    if(oprop->value!=NULL) {
        if (NULL==(prop->value = H5MM_malloc (prop->size)))
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
        HDmemcpy(prop->value,oprop->value,prop->size);
    } /* end if */

    /* Duplicate default value, if it exists */
    if(oprop->def_value!=NULL) {
        if (NULL==(prop->def_value = H5MM_malloc (prop->size)))
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
        HDmemcpy(prop->def_value,oprop->def_value,prop->size);
    } /* end if */

    /* Reset the link to the next property */
    prop->next=NULL;

    /* Set return value */
    ret_value=prop;

done:
    /* Free any resources allocated */
    if(ret_value==NULL) {
        if(prop!=NULL) {
            if(prop->name!=NULL)
                H5MM_xfree(prop->name);
            if(prop->value!=NULL)
                H5MM_xfree(prop->value);
            if(prop->def_value!=NULL)
                H5MM_xfree(prop->def_value);
            H5MM_xfree(prop);
        } /* end if */
    } /* end if */

    FUNC_LEAVE (ret_value);
}   /* H5P_copy_prop() */


/*--------------------------------------------------------------------------
 NAME
    H5P_add_prop
 PURPOSE
    Internal routine to insert a property into a property hash table
 USAGE
    herr_t H5P_add_prop(hash, hashsize, prop)
        H5P_gen_prop_t *hash[]; IN/OUT: Pointer to array of properties for hash table
        uintn hashsize;         IN: Size of hash table
        H5P_genprop_t *prop;    IN: Pointer to property to insert
 RETURNS
    Returns non-negative on success, negative on failure.
 DESCRIPTION
    Inserts a property into a hash table of properties, using the hashed
    property name.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5P_add_prop(H5P_genprop_t *hash[], uintn hashsize, H5P_genprop_t *prop)
{
    uintn loc;                  /* Hash table location */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER (H5P_add_prop, FAIL);

    assert(hash);
    assert(hashsize>0);
    assert(prop);

    /* Get correct hash table location */
    loc=H5P_hash_name(prop->name,hashsize);
    
    /* Insert property into hash table */
    prop->next=hash[loc];
    hash[loc]=prop;

#ifdef LATER
done:
#endif /* LATER */
    FUNC_LEAVE (ret_value);
}   /* H5P_add_prop() */


/*--------------------------------------------------------------------------
 NAME
    H5P_find_prop
 PURPOSE
    Internal routine to check for a property in a hash table
 USAGE
    H5P_genprop_t *H5P_find_prop(hash, hashsize, name)
        H5P_genprop_t *hash[];  IN: Pointer to array of properties for hash table
        uintn hashsize;         IN: Size of hash table
        const char *name;       IN: Name of property to check for
 RETURNS
    Returns pointer to property on success, NULL on failure.
 DESCRIPTION
        Checks for a property in a hash table of properties.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static H5P_genprop_t *
H5P_find_prop(H5P_genprop_t *hash[], uintn hashsize, const char *name)
{
    H5P_genprop_t *ret_value;   /* Property pointer return value */
    uintn loc;                  /* Hash table location */

    FUNC_ENTER (H5P_add_prop, NULL);

    assert(hash);
    assert(hashsize>0);
    assert(name);

    /* Get correct hash table location */
    loc=H5P_hash_name(name,hashsize);
    
    /* Locate property in list */
    ret_value=hash[loc];
    while(ret_value!=NULL) {
        /* Check for name matching */
        if(HDstrcmp(ret_value->name,name)==0)
            break;
    } /* end while */

#ifdef LATER
done:
#endif /* LATER */
    FUNC_LEAVE (ret_value);
}   /* H5P_find_prop() */


/*--------------------------------------------------------------------------
 NAME
    H5P_free_prop
 PURPOSE
    Internal routine to destroy a property node
 USAGE
    herr_t H5P_free_prop(prop)
        H5P_genprop_t *prop;    IN: Pointer to property to destroy
 RETURNS
    Returns non-negative on success, negative on failure.
 DESCRIPTION
    Releases all the memory for a property list.  Does _not_ call the
    properties 'close' callback, that should already have been done.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5P_free_prop(H5P_genprop_t *prop)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER (H5P_free_prop, FAIL);

    assert(prop);

    /* Release the property value and default value if they exist */
    if(prop->size>0) {
        if(prop->value)
            H5MM_xfree(prop->value);
        if(prop->def_value)
            H5MM_xfree(prop->def_value);
    } /* end if */
    H5MM_xfree(prop->name);
    H5MM_xfree(prop);

#ifdef LATER
done:
#endif /* LATER */
    FUNC_LEAVE (ret_value);
}   /* H5P_free_prop() */


/*--------------------------------------------------------------------------
 NAME
    H5P_free_all_prop
 PURPOSE
    Internal routine to remove all properties from a property hash table
 USAGE
    herr_t H5P_free_all_prop(hash, hashsize)
        H5P_gen_prop_t *hash[]; IN/OUT: Pointer to array of properties for hash table
        uintn hashsize;         IN: Size of hash table
 RETURNS
    Returns non-negative on success, negative on failure.
 DESCRIPTION
        Remove all the properties from a property list.  Calls the property
    'close' callback for each property removed.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5P_free_all_prop(H5P_genprop_t *hash[], uintn hashsize)
{
    H5P_genprop_t *tprop, *next;/* Temporary pointer to properties */
    uintn u;                    /* Local index variable */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER (H5P_free_all_prop, FAIL);

    assert(hash);
    assert(hashsize>0);

    /* Work through all the properties... */
    for(u=0; u<hashsize; u++) {
        tprop=hash[u];
        while(tprop!=NULL) {
            /* Find next node right away, to avoid accessing the current node after it's been free'd */
            next=tprop->next;

            /* Call the close callback and ignore the return value, there's nothing we can do about it */
            if(tprop->close!=NULL)
                (tprop->close)(tprop->name,&(tprop->value));

            /* Free the property, ignoring return value, nothing we can do */
            H5P_free_prop(tprop);

            tprop=next;
        } /* end while */
    } /* end for */

#ifdef LATER
done:
#endif /* LATER */
    FUNC_LEAVE (ret_value);
}   /* H5P_free_all_prop() */


/*--------------------------------------------------------------------------
 NAME
    H5P_access_class
 PURPOSE
    Internal routine to increment or decrement list & class dependancies on a
        property list class
 USAGE
    herr_t H5P_access_class(pclass,mod)
        H5P_genclass_t *pclass;     IN: Pointer to class to modify
        H5P_class_mod_t mod;        IN: Type of modification to class
 RETURNS
    Returns non-negative on success, negative on failure.
 DESCRIPTION
        Increment/Decrement the class or list dependancies for a given class.
    This routine is the final arbiter on decisions about actually releasing a
    class in memory, such action is only taken when the reference counts for
    both dependent classes & lists reach zero.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5P_access_class(H5P_genclass_t *pclass, H5P_class_mod_t mod)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER (H5P_access_class, FAIL);

    assert(pclass);
    assert(mod>H5P_MOD_ERR && mod<H5P_MOD_MAX);

    switch(mod) {
        case H5P_MOD_INC_CLS:        /* Increment the dependant class count*/
            pclass->classes++;
            break;

        case H5P_MOD_DEC_CLS:        /* Decrement the dependant class count*/
            pclass->classes--;
            break;

        case H5P_MOD_INC_LST:        /* Increment the dependant list count*/
            pclass->plists++;
            break;

        case H5P_MOD_DEC_LST:        /* Decrement the dependant list count*/
            pclass->plists--;
            break;

        case H5P_MOD_CHECK:         /* NOOP, just check if we can delete the class */
            break;
        
        case H5P_MOD_ERR:
        case H5P_MOD_MAX:
            assert(0 && "Invalid H5P class modification");
    } /* end switch */

    /* Check if we can release the class information now */
    if(pclass->deleted && pclass->plists==0 && pclass->classes==0 ) {
        assert(pclass->name);
        H5MM_xfree(pclass->name);

/*!! Need to not make callbacks for these... */

        /* Make calls to any property close callbacks which exist */
/*      H5P_free_all_prop(plist->props,plist->pclass->hashsize); */
    } /* end if */

#ifdef LATER
done:
#endif /* LATER */
    FUNC_LEAVE (ret_value);
}   /* H5P_access_class() */


/*--------------------------------------------------------------------------
 NAME
    H5P_create_class
 PURPOSE
    Internal routine to create a new property list class.
 USAGE
    H5P_genclass_t H5P_create_class(par_class, name, hashsize, internal,
                cls_create, create_data, cls_close, close_data)
        H5P_genclass_t *par_class;  IN: Pointer to parent class
        const char *name;       IN: Name of class we are creating
        uintn hashsize; IN: Number of buckets in hash table
        uintn internal; IN: Whether this is an internal class or not
        H5P_cls_create_func_t;  IN: The callback function to call when each
                                    property list in this class is created.
        void *create_data;      IN: Pointer to user data to pass along to class
                                    creation callback.
        H5P_cls_close_func_t;   IN: The callback function to call when each
                                    property list in this class is closed.
        void *close_data;       IN: Pointer to user data to pass along to class
                                    close callback.
 RETURNS
    Returns a pointer to the newly created property list class on success,
        NULL on failure.
 DESCRIPTION
    Allocates memory and attaches a class to the property list class hierarchy.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
H5P_genclass_t *
H5P_create_class(H5P_genclass_t *par_class, const char *name, uintn hashsize, uintn internal,
    H5P_cls_create_func_t cls_create, void *create_data,
    H5P_cls_close_func_t cls_close, void *close_data
    )
{
    H5P_genclass_t *pclass;             /* Property list class created */
    H5P_genclass_t *ret_value=NULL;     /* return value */

    FUNC_ENTER (H5P_create_class, NULL);

    assert(name);
    /* Allow internal classes to break some rules */
    /* (This allows the root of the tree to be created with this routine -QAK) */
    if(!internal) {
        assert(par_class);
        assert(hashsize>0);
    }

    /* Allocate room for the class & it's hash table of properties */
    if (NULL==(pclass = H5MM_calloc (sizeof(H5P_genclass_t)+((hashsize-1)*sizeof(H5P_genprop_t *)))))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,"memory allocation failed");

    /* Set class state */
    pclass->parent = par_class;
    pclass->name = HDstrdup(name);
    pclass->nprops = 0;     /* Classes are created without properties initially */
    pclass->hashsize = hashsize;
    pclass->plists = 0;     /* No properties lists of this class yet */
    pclass->classes = 0;    /* No classes derived from this class yet */
    pclass->internal = internal;
    pclass->deleted = 0;    /* Not deleted yet... :-) */

    /* Set callback functions and pass-along data */
    pclass->create_func = cls_create;
    pclass->create_data = create_data;
    pclass->close_func = cls_close;
    pclass->close_data = close_data;

    /* Increment parent class's derived class value */
    if(par_class!=NULL)
        if(H5P_access_class(par_class,H5P_MOD_INC_CLS)<0)
            HGOTO_ERROR (H5E_PLIST, H5E_CANTINIT, NULL,"Can't increment parent class ref count");

    /* Set return value */
    ret_value=pclass;

done:
    /* Free any resources allocated */
    if(ret_value==NULL) {
        if(pclass!=NULL)
            H5MM_xfree(pclass);
    }

    FUNC_LEAVE (ret_value);
}   /* H5P_create_class() */


/*--------------------------------------------------------------------------
 NAME
    H5Pcreate_class
 PURPOSE
    Create a new property list class.
 USAGE
    hid_t H5Pcreate_class(parent, name, hashsize, cls_create, create_data,
                cls_close, close_data)
        hid_t parent;       IN: Property list class ID of parent class
        const char *name;   IN: Name of class we are creating
        uintn hashsize;     IN: Number of buckets in hash table
        H5P_cls_create_func_t cls_create;   IN: The callback function to call
                                    when each property list in this class is
                                    created.
        void *create_data;  IN: Pointer to user data to pass along to class
                                    creation callback.
        H5P_cls_close_func_t cls_close;     IN: The callback function to call
                                    when each property list in this class is
                                    closed.
        void *close_data;   IN: Pointer to user data to pass along to class
                                    close callback.
 RETURNS
    Returns a valid property list class ID on success, NULL on failure.
 DESCRIPTION
    Allocates memory and attaches a class to the property list class hierarchy.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hid_t
H5Pcreate_class(hid_t parent, const char *name, unsigned hashsize,
    H5P_cls_create_func_t cls_create, void *create_data,
    H5P_cls_close_func_t cls_close, void *close_data
    )
{
    H5P_genclass_t	*par_class = NULL;  /* Pointer to the parent class */
    H5P_genclass_t	*pclass = NULL;     /* Property list class created */
    hid_t	ret_value = FAIL;           /* Return value			*/

    FUNC_ENTER(H5Pcreate_class, FAIL);

    /* Check arguments. */
    if (H5P_DEFAULT!=parent && (H5I_GENPROP_CLS!=H5I_get_type(parent)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list class");
    if (!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid class name");
    if (hashsize==0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "hashsize too small");
    if ((create_data!=NULL && cls_create==NULL) || (close_data!=NULL && cls_close==NULL))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "data specified, but no callback provided");
    
    /* Get the pointer to the parent class */
    if(parent==H5P_DEFAULT)
        par_class=NULL;
    else {
        if (NULL == (par_class = H5I_object(parent)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "can't retrieve parent class");
    } /* end else */

    /* Create the new property list class */
    if (NULL==(pclass=H5P_create_class(par_class, name, hashsize, 0, cls_create, create_data, cls_close, close_data)))
        HGOTO_ERROR(H5E_PLIST, H5E_CANTCREATE, FAIL, "unable to create property list class");

    /* Get an atom for the class */
    if ((ret_value = H5I_register(H5I_GENPROP_CLS, pclass))<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to atomize property list class");

done:
    if (ret_value<0 && pclass)
        H5P_close_class(pclass);

    FUNC_LEAVE(ret_value);
}   /* H5Pcreate_class() */


/*--------------------------------------------------------------------------
 NAME
    H5P_create_list
 PURPOSE
    Internal routine to create a new property list of a property list class.
 USAGE
    H5P_genplist_t *H5P_create_list(class)
        H5P_genclass_t *class;  IN: Property list class create list from
 RETURNS
    Returns a pointer to the newly created property list on success,
        NULL on failure.
 DESCRIPTION
        Creates a property list of a given class.  If a 'create' callback
    exists for the property list class, it is called before the
    property list is passed back to the user.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
        If this routine is called from a library routine other than
    H5Pcreate_list, the calling routine is responsible for getting an ID for
    the property list and calling the class 'create' callback (if one exists)
    and also setting the "class_init" flag.
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
H5P_genplist_t *H5P_create_list(H5P_genclass_t *class)
{
    H5P_genclass_t *tclass=NULL;        /* Temporary class pointer */
    H5P_genplist_t *plist=NULL;         /* New property list created */
    H5P_genplist_t *ret_value=NULL;     /* return value */
    H5P_genprop_t *tmp;                 /* Temporary pointer to parent class properties */
    H5P_genprop_t *pcopy;               /* Copy of property to insert into class */
    uintn u;                            /* Local index variable */

    FUNC_ENTER (H5P_create_list, NULL);

    assert(class);

    /* 
     * Create new property list object
     */

    /* Allocate room for the property list & it's hash table of properties */
    if (NULL==(plist = H5MM_calloc (sizeof(H5P_genplist_t)+((class->hashsize-1)*sizeof(H5P_genprop_t *)))))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,"memory allocation failed");

    /* Set class state */
    plist->pclass = class;
    plist->nprops = 0;      /* Initially the plist has the same number of properties as the class */
    plist->class_init = 0;  /* Initially, wait until the class callback finishes to set */

    /*
     * Copy class properties (up through list of parent classes also),
     * initialize each with default value & make property 'create' callback.
     */
    tclass=class;
    while(tclass!=NULL) {
        if(tclass->nprops>0) {
            /* Walk through the hash table */
            for(u=0; u<tclass->hashsize; u++) {
                tmp=tclass->props[u];
                /* Walk through the list of properties at each hash location */
                while(tmp!=NULL) {
                    /* Check for property already existing in list */
                    if(H5P_find_prop(plist->props,class->hashsize,tmp->name)==NULL) {
                        /* Make a copy of the class's property */
                        if((pcopy=H5P_copy_prop(tmp))==NULL)
                            HGOTO_ERROR (H5E_PLIST, H5E_CANTCOPY, NULL,"Can't copy property");

                        /* Create initial value from default value for non-zero sized properties */
                        if(pcopy->size>0) {
                            /* Properties from the class should have any values yet, but should have a default */
                            assert(pcopy->value==NULL);
                            assert(pcopy->def_value);

                            /* Allocate space for the property value & copy default value */
                            if (NULL==(pcopy->value = H5MM_malloc (pcopy->size)))
                                HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

                            HDmemcpy(pcopy->value,pcopy->def_value,pcopy->size);
                        } /* end if */

                        /* Call property creation callback, if it exists */
                        if(pcopy->create) {
                            if((pcopy->create)(pcopy->name,&(pcopy->value))<0) {
                                H5P_free_prop(pcopy);
                                HGOTO_ERROR (H5E_PLIST, H5E_CANTINIT, NULL,"Can't initialize property");
                            } /* end if */
                        } /* end if */

                        /* Insert the initialized property into the property list */
                        if(H5P_add_prop(plist->props,class->hashsize,pcopy)<0)
                            HGOTO_ERROR (H5E_PLIST, H5E_CANTINSERT, NULL,"Can't insert property into class");
                    } /* end if */

                    /* Go to next registered property in class */
                    tmp=tmp->next;
                } /* end while */
            } /* end for */
        } /* end if */

        /* Go up to parent class */
        tclass=tclass->parent;
    } /* end while */

    /* Increment the number of property lists derived from class */
    if(H5P_access_class(plist->pclass,H5P_MOD_INC_LST)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTINIT, NULL,"Can't increment class ref count");

    /* Set return value */
    ret_value=plist;

done:
    /* Release resources allocated on failure */
    if(ret_value==NULL) {
        if(plist!=NULL) {
            /* Close & free all the properties */
            H5P_free_all_prop(plist->props,class->hashsize);

            /* Decrement the number of property lists derived from the class */
            class->plists--;
        } /* end if */
    } /* end if */

    FUNC_LEAVE (ret_value);
}   /* H5P_create_list() */


/*--------------------------------------------------------------------------
 NAME
    H5Pcreate_list
 PURPOSE
    Routine to create a new property list of a property list class.
 USAGE
    hid_t H5Pcreate_list(cls_id)
        hid_t cls_id;       IN: Property list class create list from
 RETURNS
    Returns a valid property list ID on success, NULL on failure.
 DESCRIPTION
        Creates a property list of a given class.  If a 'create' callback
    exists for the property list class, it is called before the
    property list is passed back to the user.  If 'create' callbacks exist for
    any individual properties in the property list, they are called before the
    class 'create' callback.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hid_t H5Pcreate_list(hid_t cls_id)
{
    H5P_genclass_t	*pclass;   /* Property list class to modify */
    H5P_genplist_t	*plist;    /* Property list created */
    hid_t plist_id=FAIL;       /* Property list ID */
    hid_t ret_value=FAIL;      /* return value */

    FUNC_ENTER (H5Pcreate_list, FAIL);

    /* Check arguments. */
    if (H5I_GENPROP_CLS != H5I_get_type(cls_id) || NULL == (pclass = H5I_object(cls_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a property list class");

    /* Create the new property list */
    if ((plist=H5P_create_list(pclass))==NULL)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTCREATE, NULL, "unable to create property list");

    /* Get an atom for the property list */
    if ((plist_id = H5I_register(H5I_GENPROP_LST, plist))<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to atomize property list");

    /* Call the class callback (if it exists) now that we have the property list ID */
    if(plist->pclass->create_func!=NULL) {
        if((plist->pclass->create_func)(plist_id,plist->pclass->create_data)<0) {
            /* Delete ID, ignore return value */
            H5I_remove(plist_id);
            HGOTO_ERROR (H5E_PLIST, H5E_CANTINIT, FAIL,"Can't initialize property");
        } /* end if */
    } /* end if */

    /* Set the class initialization flag */
    plist->class_init=1;

    /* Set the return value */
    ret_value=plist_id;

done:
    if (ret_value<0 && plist)
        H5P_close_list(plist);

    FUNC_LEAVE (ret_value);
}   /* H5Pcreate_list() */


/*--------------------------------------------------------------------------
 NAME
    H5P_register
 PURPOSE
    Internal routine to register a new property in a property list class.
 USAGE
    herr_t H5P_register(class, name, size, default, prp_create, prp_set, prp_get, prp_close)
        H5P_genclass_t *class;  IN: Property list class to close
        const char *name;       IN: Name of property to register
        size_t size;            IN: Size of property in bytes
        void *def_value;        IN: Pointer to buffer containing default value
                                    for property in newly created property lists
        H5P_prp_create_func_t prp_create;   IN: Function pointer to property
                                    creation callback
        H5P_prp_set_func_t prp_set; IN: Function pointer to property set callback
        H5P_prp_get_func_t prp_get; IN: Function pointer to property get callback
        H5P_prp_close_func_t prp_close; IN: Function pointer to property close
                                    callback
 RETURNS
    Returns non-negative on success, negative on failure.
 DESCRIPTION
        Registers a new property with a property list class.  The property will
    exist in all property list objects of that class after this routine is
    finished.  The name of the property must not already exist.  The default
    property value must be provided and all new property lists created with this
    property will have the property value set to the default provided.  Any of
    the callback routines may be set to NULL if they are not needed.

        Zero-sized properties are allowed and do not store any data in the
    property list.  These may be used as flags to indicate the presence or
    absence of a particular piece of information.  The 'default' pointer for a
    zero-sized property may be set to NULL.  The property 'create' & 'close'
    callbacks are called for zero-sized properties, but the 'set' and 'get'
    callbacks are never called.

        The 'create' callback is called when a new property list with this
    property is being created.  H5P_prp_create_func_t is defined as:
        typedef herr_t (*H5P_prp_create_func_t)(hid_t prop_id, const char *name,
                void **initial_value);
    where the parameters to the callback function are:
        hid_t prop_id;      IN: The ID of the property list being created.
        const char *name;   IN: The name of the property being modified.
        void **initial_value;   IN/OUT: The initial value for the property being created.
                                (The 'default' value passed to H5Pregister)
    The 'create' routine may modify the value to be set and those changes will
    be stored as the initial value of the property.  If the 'create' routine
    returns a negative value, the new property value is not copied into the
    property and the property list creation routine returns an error value.

        The 'set' callback is called before a new value is copied into the
    property.  H5P_prp_set_func_t is defined as:
        typedef herr_t (*H5P_prp_set_func_t)(hid_t prop_id, const char *name,
            void **value);
    where the parameters to the callback function are:
        hid_t prop_id;      IN: The ID of the property list being modified.
        const char *name;   IN: The name of the property being modified.
        void **new_value;   IN/OUT: The value being set for the property.
    The 'set' routine may modify the value to be set and those changes will be
    stored as the value of the property.  If the 'set' routine returns a
    negative value, the new property value is not copied into the property and
    the property list set routine returns an error value.

        The 'get' callback is called before a value is retrieved from the
    property.  H5P_prp_get_func_t is defined as:
        typedef herr_t (*H5P_prp_get_func_t)(hid_t prop_id, const char *name,
            void *value);
    where the parameters to the callback function are:
        hid_t prop_id;      IN: The ID of the property list being queried.
        const char *name;   IN: The name of the property being queried.
        void **value;       IN/OUT: The value being retrieved for the property.
    The 'get' routine may modify the value to be retrieved and those changes
    will be returned to the calling function.  If the 'get' routine returns a
    negative value, the property value is returned and the property list get
    routine returns an error value.

        The 'close' callback is called when a property list with this
    property is being destroyed.  H5P_prp_close_func_t is defined as:
        typedef herr_t (*H5P_prp_close_func_t)(const char *name, void *value);
    where the parameters to the callback function are:
        const char *name;   IN: The name of the property being closed.
        void *value;        IN: The value of the property being closed.
    The 'close' routine may modify the value passed in, but the value is not
    used by the library when the 'close' routine returns.  If the
    'close' routine returns a negative value, the property list close
    routine returns an error value but the property list is still closed.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
        The 'set' callback function may be useful to range check the value being
    set for the property or may perform some tranformation/translation of the
    value set.  The 'get' callback would then [probably] reverse the
    transformation, etc.  A single 'get' or 'set' callback could handle
    multiple properties by performing different actions based on the property
    name or other properties in the property list.

        I would like to say "the property list is not closed" when a 'close'
    routine fails, but I don't think that's possible due to other properties in
    the list being successfully closed & removed from the property list.  I
    suppose that it would be possible to just remove the properties which have
    successful 'close' callbacks, but I'm not happy with the ramifications
    of a mangled, un-closable property list hanging around...  Any comments? -QAK

 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5P_register(H5P_genclass_t *class, const char *name, size_t size,
    void *def_value, H5P_prp_create_func_t prp_create, H5P_prp_set_func_t prp_set,
    H5P_prp_get_func_t prp_get, H5P_prp_close_func_t prp_close)
{
    herr_t ret_value=FAIL;     /* return value */

    FUNC_ENTER (H5P_register, FAIL);

    assert(class);
    assert(name);
    assert((size>0 && def_value!=NULL) || (size==0));

/* Check for duplicate named properties */
/* Check if class needs to be split because property lists or classes have been */
/* created since the last modification was made to the class.  Insert property */
/* into property list class, increment modification made to class, reset */
/* information about property lists & classes created */

    /* Set return value */
    ret_value=SUCCEED;

done:
    FUNC_LEAVE (ret_value);
}   /* H5P_register() */


/*--------------------------------------------------------------------------
 NAME
    H5Pregister
 PURPOSE
    Routine to register a new property in a property list class.
 USAGE
    herr_t H5Pregister(class, name, size, default, prp_create, prp_set, prp_get, prp_close)
        hid_t class;            IN: Property list class to close
        const char *name;       IN: Name of property to register
        size_t size;            IN: Size of property in bytes
        void *def_value;        IN: Pointer to buffer containing default value
                                    for property in newly created property lists
        H5P_prp_create_func_t prp_create;   IN: Function pointer to property
                                    creation callback
        H5P_prp_set_func_t prp_set; IN: Function pointer to property set callback
        H5P_prp_get_func_t prp_get; IN: Function pointer to property get callback
        H5P_prp_close_func_t prp_close; IN: Function pointer to property close
                                    callback
 RETURNS
    Returns non-negative on success, negative on failure.
 DESCRIPTION
        Registers a new property with a property list class.  The property will
    exist in all property list objects of that class after this routine is
    finished.  The name of the property must not already exist.  The default
    property value must be provided and all new property lists created with this
    property will have the property value set to the default provided.  Any of
    the callback routines may be set to NULL if they are not needed.

        Zero-sized properties are allowed and do not store any data in the
    property list.  These may be used as flags to indicate the presence or
    absence of a particular piece of information.  The 'default' pointer for a
    zero-sized property may be set to NULL.  The property 'create' & 'close'
    callbacks are called for zero-sized properties, but the 'set' and 'get'
    callbacks are never called.

        The 'create' callback is called when a new property list with this
    property is being created.  H5P_prp_create_func_t is defined as:
        typedef herr_t (*H5P_prp_create_func_t)(hid_t prop_id, const char *name,
                void **initial_value);
    where the parameters to the callback function are:
        hid_t prop_id;      IN: The ID of the property list being created.
        const char *name;   IN: The name of the property being modified.
        void **initial_value;   IN/OUT: The initial value for the property being created.
                                (The 'default' value passed to H5Pregister)
    The 'create' routine may modify the value to be set and those changes will
    be stored as the initial value of the property.  If the 'create' routine
    returns a negative value, the new property value is not copied into the
    property and the property list creation routine returns an error value.

        The 'set' callback is called before a new value is copied into the
    property.  H5P_prp_set_func_t is defined as:
        typedef herr_t (*H5P_prp_set_func_t)(hid_t prop_id, const char *name,
            void **value);
    where the parameters to the callback function are:
        hid_t prop_id;      IN: The ID of the property list being modified.
        const char *name;   IN: The name of the property being modified.
        void **new_value;   IN/OUT: The value being set for the property.
    The 'set' routine may modify the value to be set and those changes will be
    stored as the value of the property.  If the 'set' routine returns a
    negative value, the new property value is not copied into the property and
    the property list set routine returns an error value.

        The 'get' callback is called before a value is retrieved from the
    property.  H5P_prp_get_func_t is defined as:
        typedef herr_t (*H5P_prp_get_func_t)(hid_t prop_id, const char *name,
            void *value);
    where the parameters to the callback function are:
        hid_t prop_id;      IN: The ID of the property list being queried.
        const char *name;   IN: The name of the property being queried.
        void **value;       IN/OUT: The value being retrieved for the property.
    The 'get' routine may modify the value to be retrieved and those changes
    will be returned to the calling function.  If the 'get' routine returns a
    negative value, the property value is returned and the property list get
    routine returns an error value.

        The 'close' callback is called when a property list with this
    property is being destroyed.  H5P_prp_close_func_t is defined as:
        typedef herr_t (*H5P_prp_close_func_t)(const char *name, void *value);
    where the parameters to the callback function are:
        const char *name;   IN: The name of the property being closed.
        void *value;        IN: The value of the property being closed.
    The 'close' routine may modify the value passed in, but the value is not
    used by the library when the 'close' routine returns.  If the
    'close' routine returns a negative value, the property list close
    routine returns an error value but the property list is still closed.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
        The 'set' callback function may be useful to range check the value being
    set for the property or may perform some tranformation/translation of the
    value set.  The 'get' callback would then [probably] reverse the
    transformation, etc.  A single 'get' or 'set' callback could handle
    multiple properties by performing different actions based on the property
    name or other properties in the property list.

        I would like to say "the property list is not closed" when a 'close'
    routine fails, but I don't think that's possible due to other properties in
    the list being successfully closed & removed from the property list.  I
    suppose that it would be possible to just remove the properties which have
    successful 'close' callbacks, but I'm not happy with the ramifications
    of a mangled, un-closable property list hanging around...  Any comments? -QAK

 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5Pregister(hid_t cls_id, const char *name, size_t size, void *def_value,
    H5P_prp_create_func_t prp_create, H5P_prp_set_func_t prp_set,
    H5P_prp_get_func_t prp_get, H5P_prp_close_func_t prp_close)
{
    H5P_genclass_t	*pclass;   /* Property list class to modify */
    herr_t ret_value=FAIL;     /* return value */

    FUNC_ENTER (H5Pregister, FAIL);

    /* Check arguments. */
    if (H5I_GENPROP_CLS != H5I_get_type(cls_id) || NULL == (pclass = H5I_object(cls_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list class");
    if (!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid class name");
    if (size>0 && def_value==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "properties >0 size must have default");

    /* Create the new property list class */
    if ((ret_value=H5P_register(pclass,name,size,def_value,prp_create,prp_set,prp_get,prp_close))<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to register property in class");

done:
    FUNC_LEAVE (ret_value);
}   /* H5Pregister() */


/*--------------------------------------------------------------------------
 NAME
    H5P_insert
 PURPOSE
    Internal routine to insert a new property in a property list.
 USAGE
    herr_t H5P_insert(plist, name, size, value, prp_set, prp_get, prp_close)
        H5P_genplist_t *plist;  IN: Property list to add property to
        const char *name;       IN: Name of property to add
        size_t size;            IN: Size of property in bytes
        void *value;            IN: Pointer to the value for the property
        H5P_prp_set_func_t prp_set; IN: Function pointer to property set callback
        H5P_prp_get_func_t prp_get; IN: Function pointer to property get callback
        H5P_prp_close_func_t prp_close; IN: Function pointer to property close
                                    callback
 RETURNS
    Returns non-negative on success, negative on failure.
 DESCRIPTION
        Inserts a temporary property into a property list.  The property will
    exist only in this property list object.  The name of the property must not
    already exist.  The value must be provided unless the property is zero-
    sized.  Any of the callback routines may be set to NULL if they are not
    needed.

        Zero-sized properties are allowed and do not store any data in the
    property list.  These may be used as flags to indicate the presence or
    absence of a particular piece of information.  The 'value' pointer for a
    zero-sized property may be set to NULL.  The property 'close' callback is
    called for zero-sized properties, but the 'set' and 'get' callbacks are
    never called.

        The 'set' callback is called before a new value is copied into the
    property.  H5P_prp_set_func_t is defined as:
        typedef herr_t (*H5P_prp_set_func_t)(hid_t prop_id, const char *name,
            void **value);
    where the parameters to the callback function are:
        hid_t prop_id;      IN: The ID of the property list being modified.
        const char *name;   IN: The name of the property being modified.
        void **new_value;   IN/OUT: The value being set for the property.
    The 'set' routine may modify the value to be set and those changes will be
    stored as the value of the property.  If the 'set' routine returns a
    negative value, the new property value is not copied into the property and
    the property list set routine returns an error value.

        The 'get' callback is called before a value is retrieved from the
    property.  H5P_prp_get_func_t is defined as:
        typedef herr_t (*H5P_prp_get_func_t)(hid_t prop_id, const char *name,
            void *value);
    where the parameters to the callback function are:
        hid_t prop_id;      IN: The ID of the property list being queried.
        const char *name;   IN: The name of the property being queried.
        void **value;       IN/OUT: The value being retrieved for the property.
    The 'get' routine may modify the value to be retrieved and those changes
    will be returned to the calling function.  If the 'get' routine returns a
    negative value, the property value is returned and the property list get
    routine returns an error value.

        The 'close' callback is called when a property list with this
    property is being destroyed.  H5P_prp_close_func_t is defined as:
        typedef herr_t (*H5P_prp_close_func_t)(const char *name,
            void *value);
    where the parameters to the callback function are:
        const char *name;   IN: The name of the property being closed.
        void *value;        IN: The value of the property being closed.
    The 'close' routine may modify the value passed in, but the value is not
    used by the library when the 'close' routine returns.  If the
    'close' routine returns a negative value, the property list close
    routine returns an error value but the property list is still closed.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
        The 'set' callback function may be useful to range check the value being
    set for the property or may perform some tranformation/translation of the
    value set.  The 'get' callback would then [probably] reverse the
    transformation, etc.  A single 'get' or 'set' callback could handle
    multiple properties by performing different actions based on the property
    name or other properties in the property list.
        
        There is no 'create' callback routine for temporary property list
    objects, the initial value is assumed to have any necessary setup already
    performed on it.

        I would like to say "the property list is not closed" when a 'close'
    routine fails, but I don't think that's possible due to other properties in
    the list being successfully closed & removed from the property list.  I
    suppose that it would be possible to just remove the properties which have
    successful 'close' callbacks, but I'm not happy with the ramifications
    of a mangled, un-closable property list hanging around...  Any comments? -QAK

 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5P_insert(H5P_genplist_t *plist, const char *name, size_t size,
    void *value, H5P_prp_set_func_t prp_set, H5P_prp_get_func_t prp_get,
    H5P_prp_close_func_t prp_close)
{
    herr_t ret_value=FAIL;     /* return value */

    FUNC_ENTER (H5P_insert, FAIL);

    assert(plist);
    assert(name);
    assert((size>0 && value!=NULL) || (size==0));

/* Check for duplicate named properties */
/* Insert property into property list */

    /* Set return value */
    ret_value=SUCCEED;

done:
    FUNC_LEAVE (ret_value);
}   /* H5P_insert() */


/*--------------------------------------------------------------------------
 NAME
    H5Pinsert
 PURPOSE
    Routine to insert a new property in a property list.
 USAGE
    herr_t H5Pinsert(plist, name, size, value, prp_set, prp_get, prp_close)
        hid_t plist;            IN: Property list to add property to
        const char *name;       IN: Name of property to add
        size_t size;            IN: Size of property in bytes
        void *value;            IN: Pointer to the value for the property
        H5P_prp_set_func_t prp_set; IN: Function pointer to property set callback
        H5P_prp_get_func_t prp_get; IN: Function pointer to property get callback
        H5P_prp_close_func_t prp_close; IN: Function pointer to property close
                                    callback
 RETURNS
    Returns non-negative on success, negative on failure.
 DESCRIPTION
        Inserts a temporary property into a property list.  The property will
    exist only in this property list object.  The name of the property must not
    already exist.  The value must be provided unless the property is zero-
    sized.  Any of the callback routines may be set to NULL if they are not
    needed.

        Zero-sized properties are allowed and do not store any data in the
    property list.  These may be used as flags to indicate the presence or
    absence of a particular piece of information.  The 'value' pointer for a
    zero-sized property may be set to NULL.  The property 'close' callback is
    called for zero-sized properties, but the 'set' and 'get' callbacks are
    never called.

        The 'set' callback is called before a new value is copied into the
    property.  H5P_prp_set_func_t is defined as:
        typedef herr_t (*H5P_prp_set_func_t)(hid_t prop_id, const char *name,
            void **value);
    where the parameters to the callback function are:
        hid_t prop_id;      IN: The ID of the property list being modified.
        const char *name;   IN: The name of the property being modified.
        void **new_value;   IN/OUT: The value being set for the property.
    The 'set' routine may modify the value to be set and those changes will be
    stored as the value of the property.  If the 'set' routine returns a
    negative value, the new property value is not copied into the property and
    the property list set routine returns an error value.

        The 'get' callback is called before a value is retrieved from the
    property.  H5P_prp_get_func_t is defined as:
        typedef herr_t (*H5P_prp_get_func_t)(hid_t prop_id, const char *name,
            void *value);
    where the parameters to the callback function are:
        hid_t prop_id;      IN: The ID of the property list being queried.
        const char *name;   IN: The name of the property being queried.
        void **value;       IN/OUT: The value being retrieved for the property.
    The 'get' routine may modify the value to be retrieved and those changes
    will be returned to the calling function.  If the 'get' routine returns a
    negative value, the property value is returned and the property list get
    routine returns an error value.

        The 'close' callback is called when a property list with this
    property is being destroyed.  H5P_prp_close_func_t is defined as:
        typedef herr_t (*H5P_prp_close_func_t)(const char *name, void *value);
    where the parameters to the callback function are:
        const char *name;   IN: The name of the property being closed.
        void *value;        IN: The value of the property being closed.
    The 'close' routine may modify the value passed in, but the value is not
    used by the library when the 'close' routine returns.  If the
    'close' routine returns a negative value, the property list close
    routine returns an error value but the property list is still closed.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
        The 'set' callback function may be useful to range check the value being
    set for the property or may perform some tranformation/translation of the
    value set.  The 'get' callback would then [probably] reverse the
    transformation, etc.  A single 'get' or 'set' callback could handle
    multiple properties by performing different actions based on the property
    name or other properties in the property list.
        
        There is no 'create' callback routine for temporary property list
    objects, the initial value is assumed to have any necessary setup already
    performed on it.

        I would like to say "the property list is not closed" when a 'close'
    routine fails, but I don't think that's possible due to other properties in
    the list being successfully closed & removed from the property list.  I
    suppose that it would be possible to just remove the properties which have
    successful 'close' callbacks, but I'm not happy with the ramifications
    of a mangled, un-closable property list hanging around...  Any comments? -QAK

 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5Pinsert(hid_t plist_id, const char *name, size_t size, void *value,
    H5P_prp_set_func_t prp_set, H5P_prp_get_func_t prp_get,
    H5P_prp_close_func_t prp_close)
{
    H5P_genplist_t	*plist;    /* Property list to modify */
    herr_t ret_value=FAIL;     /* return value */

    FUNC_ENTER (H5Pinsert, FAIL);

    /* Check arguments. */
    if (H5I_GENPROP_LST != H5I_get_type(plist_id) || NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
    if (!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid property name");
    if (size>0 && value==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "properties >0 size must have default");

    /* Create the new property list class */
    if ((ret_value=H5P_insert(plist,name,size,value,prp_set,prp_get,prp_close))<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to register property in plist");

done:
    FUNC_LEAVE (ret_value);
}   /* H5Pinsert() */


/*--------------------------------------------------------------------------
 NAME
    H5P_set
 PURPOSE
    Internal routine to set a property's value in a property list.
 USAGE
    herr_t H5P_set(plist, name, value)
        H5P_genplist_t *plist;  IN: Property list to find property in
        const char *name;       IN: Name of property to set
        void *value;            IN: Pointer to the value for the property
 RETURNS
    Returns non-negative on success, negative on failure.
 DESCRIPTION
        Sets a new value for a property in a property list.  The property name
    must exist or this routine will fail.  If there is a 'set' callback routine
    registered for this property, the 'value' will be passed to that routine and
    any changes to the 'value' will be used when setting the property value.
    The information pointed at by the 'value' pointer (possibly modified by the
    'set' callback) is copied into the property list value and may be changed
    by the application making the H5Pset call without affecting the property
    value.

        If the 'set' callback routine returns an error, the property value will
    not be modified.  This routine may not be called for zero-sized properties
    and will return an error in that case.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5P_set(H5P_genplist_t *plist, const char *name, void *value)
{
    herr_t ret_value=FAIL;     /* return value */

    FUNC_ENTER (H5P_set, FAIL);

    assert(plist);
    assert(name);
    assert(value);

/* Check for property size >0 */
/* Make a copy of the value */
/* Pass to 'set' callback if it exists */
/* Copy new [possibly changed] value into property value */

    /* Set return value */
    ret_value=SUCCEED;

done:
    FUNC_LEAVE (ret_value);
}   /* H5P_set() */


/*--------------------------------------------------------------------------
 NAME
    H5Pset
 PURPOSE
    Routine to set a property's value in a property list.
 USAGE
    herr_t H5P_set(plist_id, name, value)
        hid_t plist_id;         IN: Property list to find property in
        const char *name;       IN: Name of property to set
        void *value;            IN: Pointer to the value for the property
 RETURNS
    Returns non-negative on success, negative on failure.
 DESCRIPTION
        Sets a new value for a property in a property list.  The property name
    must exist or this routine will fail.  If there is a 'set' callback routine
    registered for this property, the 'value' will be passed to that routine and
    any changes to the 'value' will be used when setting the property value.
    The information pointed at by the 'value' pointer (possibly modified by the
    'set' callback) is copied into the property list value and may be changed
    by the application making the H5Pset call without affecting the property
    value.

        If the 'set' callback routine returns an error, the property value will
    not be modified.  This routine may not be called for zero-sized properties
    and will return an error in that case.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5Pset(hid_t plist_id, const char *name, void *value)
{
    H5P_genplist_t	*plist;    /* Property list to modify */
    herr_t ret_value=FAIL;     /* return value */

    FUNC_ENTER (H5Pset, FAIL);

    /* Check arguments. */
    if (H5I_GENPROP_LST != H5I_get_type(plist_id) || NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
    if (!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid property name");
    if (value==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalied property value");

    /* Create the new property list class */
    if ((ret_value=H5P_set(plist,name,value))<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to set value in plist");

done:
    FUNC_LEAVE (ret_value);
}   /* H5Pset() */


/*--------------------------------------------------------------------------
 NAME
    H5P_exist
 PURPOSE
    Internal routine to query the existance of a property in a property list.
 USAGE
    herr_t H5P_exist(plist, name)
        H5P_genplist_t *plist;  IN: Property list to check
        const char *name;       IN: Name of property to check for
 RETURNS
    Success: Positive if the property exists in the property list, zero
            if the property does not exist.
    Failure: negative value
 DESCRIPTION
        This routine checks if a property exists within a property list.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5P_exist(H5P_genplist_t *plist, const char *name)
{
    herr_t ret_value=FAIL;     /* return value */

    FUNC_ENTER (H5P_exist, FAIL);

    assert(plist);
    assert(name);

/* Check for property in property list */

    /* Set return value */
    ret_value=SUCCEED;

done:
    FUNC_LEAVE (ret_value);
}   /* H5P_exist() */


/*--------------------------------------------------------------------------
 NAME
    H5Pexist
 PURPOSE
    Routine to query the existance of a property in a property list.
 USAGE
    herr_t H5P_exist(plist_id, name)
        hid_t plist_id;         IN: Property list ID to check
        const char *name;       IN: Name of property to check for
 RETURNS
    Success: Positive if the property exists in the property list, zero
            if the property does not exist.
    Failure: negative value
 DESCRIPTION
        This routine checks if a property exists within a property list.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5Pexist(hid_t plist_id, const char *name)
{
    H5P_genplist_t	*plist;    /* Property list to modify */
    herr_t ret_value=FAIL;     /* return value */

    FUNC_ENTER (H5Pexist, FAIL);

    /* Check arguments. */
    if (H5I_GENPROP_LST != H5I_get_type(plist_id) || NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
    if (!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid property name");

    /* Create the new property list class */
    if ((ret_value=H5P_exist(plist,name))<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to set value in plist");

done:
    FUNC_LEAVE (ret_value);
}   /* H5Pexist() */


/*--------------------------------------------------------------------------
 NAME
    H5P_get_size
 PURPOSE
    Internal routine to query the size of a property in a property list.
 USAGE
    herr_t H5P_get_size(plist, name)
        H5P_genplist_t *plist;  IN: Property list to check
        const char *name;       IN: Name of property to query
 RETURNS
    Success: Size of property value in bytes.
    Failure: negative value
 DESCRIPTION
        This routine returns the size of a property's value in bytes.  Zero-
    sized properties are allowed and return 0.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5P_get_size(H5P_genplist_t *plist, const char *name)
{
    herr_t ret_value=FAIL;     /* return value */

    FUNC_ENTER (H5P_get_size, FAIL);

    assert(plist);
    assert(name);

/* Check for property in property list */
/* Query property size */

    /* Set return value */
    ret_value=SUCCEED;

done:
    FUNC_LEAVE (ret_value);
}   /* H5P_get_size() */


/*--------------------------------------------------------------------------
 NAME
    H5Pget_size
 PURPOSE
    Routine to query the size of a property in a property list.
 USAGE
    herr_t H5Pget_size(plist_id, name)
        hid_t plist_id;         IN: Property list to check
        const char *name;       IN: Name of property to query
 RETURNS
    Success: Size of property value in bytes.
    Failure: negative value
 DESCRIPTION
        This routine returns the size of a property's value in bytes.  Zero-
    sized properties are allowed and return 0.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5Pget_size(hid_t plist_id, const char *name)
{
    H5P_genplist_t	*plist;    /* Property list to modify */
    herr_t ret_value=FAIL;     /* return value */

    FUNC_ENTER (H5Pget_size, FAIL);

    /* Check arguments. */
    if (H5I_GENPROP_LST != H5I_get_type(plist_id) || NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
    if (!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid property name");

    /* Create the new property list class */
    if ((ret_value=H5P_get_size(plist,name))<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to query size in plist");

done:
    FUNC_LEAVE (ret_value);
}   /* H5Pget_size() */


/*--------------------------------------------------------------------------
 NAME
    H5P_get
 PURPOSE
    Internal routine to query the value of a property in a property list.
 USAGE
    herr_t H5P_get_size(plist, name, value)
        H5P_genplist_t *plist;  IN: Property list to check
        const char *name;       IN: Name of property to query
        void *value;            OUT: Pointer to the buffer for the property value
 RETURNS
    Returns non-negative on success, negative on failure.
 DESCRIPTION
        Retrieves a copy of the value for a property in a property list.  The
    property name must exist or this routine will fail.  If there is a
    'get' callback routine registered for this property, the copy of the
    value of the property will first be passed to that routine and any changes
    to the copy of the value will be used when returning the property value
    from this routine.
        If the 'get' callback routine returns an error, 'value' will not be
    modified and this routine will return an error.  This routine may not be
    called for zero-sized properties.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5P_get(H5P_genplist_t *plist, const char *name, void *value)
{
    herr_t ret_value=FAIL;     /* return value */

    FUNC_ENTER (H5P_get, FAIL);

    assert(plist);
    assert(name);
    assert(value);

/* Find the property in the property list */
/* Check for property size >0 */
/* Make a copy of the value */
/* Pass to 'get' callback if it exists */
/* Copy [possibly changed] value into user's buffer */

    /* Set return value */
    ret_value=SUCCEED;

done:
    FUNC_LEAVE (ret_value);
}   /* H5P_get() */


/*--------------------------------------------------------------------------
 NAME
    H5Pget
 PURPOSE
    Routine to query the value of a property in a property list.
 USAGE
    herr_t H5P_get_size(plist_id, name, value)
        hid_t plist_id;         IN: Property list to check
        const char *name;       IN: Name of property to query
        void *value;            OUT: Pointer to the buffer for the property value
 RETURNS
    Returns non-negative on success, negative on failure.
 DESCRIPTION
        Retrieves a copy of the value for a property in a property list.  The
    property name must exist or this routine will fail.  If there is a
    'get' callback routine registered for this property, the copy of the
    value of the property will first be passed to that routine and any changes
    to the copy of the value will be used when returning the property value
    from this routine.
        If the 'get' callback routine returns an error, 'value' will not be
    modified and this routine will return an error.  This routine may not be
    called for zero-sized properties.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5Pget(hid_t plist_id, const char *name, void * value)
{
    H5P_genplist_t	*plist;    /* Property list to modify */
    herr_t ret_value=FAIL;     /* return value */

    FUNC_ENTER (H5Pget, FAIL);

    /* Check arguments. */
    if (H5I_GENPROP_LST != H5I_get_type(plist_id) || NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
    if (!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid property name");
    if (value==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalied property value");

    /* Create the new property list class */
    if ((ret_value=H5P_get(plist,name,value))<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to query property value");

done:
    FUNC_LEAVE (ret_value);
}   /* H5Pget() */


/*--------------------------------------------------------------------------
 NAME
    H5P_remove
 PURPOSE
    Internal routine to remove a property from a property list.
 USAGE
    herr_t H5P_remove(plist, name)
        H5P_genplist_t *plist;  IN: Property list to modify
        const char *name;       IN: Name of property to remove
 RETURNS
    Returns non-negative on success, negative on failure.
 DESCRIPTION
        Removes a property from a property list.  Both properties which were
    in existance when the property list was created (i.e. properties registered
    with H5Pregister) and properties added to the list after it was created
    (i.e. added with H5Pinsert) may be removed from a property list.
    Properties do not need to be removed a property list before the list itself
    is closed, they will be released automatically when H5Pclose is called.
    The 'close' callback for this property is called before the property is
    release, if the callback exists.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5P_remove(H5P_genplist_t *plist, const char *name)
{
    herr_t ret_value=FAIL;     /* return value */

    FUNC_ENTER (H5P_remove, FAIL);

    assert(plist);
    assert(name);

/* Find the property in the property list */
/* Get value for property */
/* Pass to 'remove' callback if it exists */
/* Remove property from property list */

    /* Set return value */
    ret_value=SUCCEED;

done:
    FUNC_LEAVE (ret_value);
}   /* H5P_remove() */


/*--------------------------------------------------------------------------
 NAME
    H5Premove
 PURPOSE
    Routine to remove a property from a property list.
 USAGE
    herr_t H5Premove(plist_id, name)
        hid_t plist_id;         IN: Property list to modify
        const char *name;       IN: Name of property to remove
 RETURNS
    Returns non-negative on success, negative on failure.
 DESCRIPTION
        Removes a property from a property list.  Both properties which were
    in existance when the property list was created (i.e. properties registered
    with H5Pregister) and properties added to the list after it was created
    (i.e. added with H5Pinsert) may be removed from a property list.
    Properties do not need to be removed a property list before the list itself
    is closed, they will be released automatically when H5Pclose is called.
    The 'close' callback for this property is called before the property is
    release, if the callback exists.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5Premove(hid_t plist_id, const char *name)
{
    H5P_genplist_t	*plist;    /* Property list to modify */
    herr_t ret_value=FAIL;     /* return value */

    FUNC_ENTER (H5Premove, FAIL);

    /* Check arguments. */
    if (H5I_GENPROP_LST != H5I_get_type(plist_id) || NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
    if (!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid property name");

    /* Create the new property list class */
    if ((ret_value=H5P_remove(plist,name))<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to remove property");

done:
    FUNC_LEAVE (ret_value);
}   /* H5Premove() */


/*--------------------------------------------------------------------------
 NAME
    H5P_unregister
 PURPOSE
    Internal routine to remove a property from a property list class.
 USAGE
    herr_t H5P_unregister(pclass, name)
        H5P_genclass_t *pclass; IN: Property list class to modify
        const char *name;       IN: Name of property to remove
 RETURNS
    Returns non-negative on success, negative on failure.
 DESCRIPTION
        Removes a property from a property list class.  Future property lists
    created of that class will not contain this property.  Existing property
    lists containing this property are not affected.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5P_unregister(H5P_genclass_t *pclass, const char *name)
{
    herr_t ret_value=FAIL;     /* return value */

    FUNC_ENTER (H5P_unregister, FAIL);

    assert(pclass);
    assert(name);

/* Find the property in the property list class */
/* Remove property from property list class */

    /* Set return value */
    ret_value=SUCCEED;

done:
    FUNC_LEAVE (ret_value);
}   /* H5P_unregister() */


/*--------------------------------------------------------------------------
 NAME
    H5Punregister
 PURPOSE
    Routine to remove a property from a property list class.
 USAGE
    herr_t H5Punregister(pclass_id, name)
        hid_t pclass_id;         IN: Property list class to modify
        const char *name;       IN: Name of property to remove
 RETURNS
    Returns non-negative on success, negative on failure.
 DESCRIPTION
        Removes a property from a property list class.  Future property lists
    created of that class will not contain this property.  Existing property
    lists containing this property are not affected.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5Punregister(hid_t pclass_id, const char *name)
{
    H5P_genclass_t	*pclass;   /* Property list class to modify */
    herr_t ret_value=FAIL;     /* return value */

    FUNC_ENTER (H5Punregister, FAIL);

    /* Check arguments. */
    if (H5I_GENPROP_CLS != H5I_get_type(pclass_id) || NULL == (pclass = H5I_object(pclass_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list class");
    if (!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid property name");

    /* Remove the property list from class */
    if ((ret_value=H5P_unregister(pclass,name))<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to remove property from class");

done:
    FUNC_LEAVE (ret_value);
}   /* H5Punregister() */


/*--------------------------------------------------------------------------
 NAME
    H5P_close_list
 PURPOSE
    Internal routine to close a property list.
 USAGE
    herr_t H5P_close_list(plist)
        H5P_genplist_t *plist;  IN: Property list to close
 RETURNS
    Returns non-negative on success, negative on failure.
 DESCRIPTION
        Closes a property list.  If a 'close' callback exists for the property
    list class, it is called before the property list is destroyed.  If 'close'
    callbacks exist for any individual properties in the property list, they are
    called after the class 'close' callback.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
        The property list class 'close' callback routine is not called from
    here, it must have been check for and called properly prior to this routine
    being called
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5P_close_list(H5P_genplist_t *plist)
{
    herr_t ret_value=FAIL;     /* return value */

    FUNC_ENTER (H5P_close_list, FAIL);

    assert(plist);

    /* Decrement parent class's dependant property list value! */
    if(H5P_access_class(plist->pclass,H5P_MOD_DEC_LST)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTINIT, FAIL, "Can't decrement class ref count");

    /* Make calls to any property close callbacks which exist */
    H5P_free_all_prop(plist->props,plist->pclass->hashsize);

    /* Destroy property list object */
    H5MM_xfree(plist);

done:
    FUNC_LEAVE (ret_value);
}   /* H5P_close_list() */


/*--------------------------------------------------------------------------
 NAME
    H5Pclose_list
 PURPOSE
    Routine to close a property list.
 USAGE
    herr_t H5Pclose_list(plist_id)
        hid_t plist_id;       IN: Property list to close
 RETURNS
    Returns non-negative on success, negative on failure.
 DESCRIPTION
        Closes a property list.  If a 'close' callback exists for the property
    list class, it is called before the property list is destroyed.  If 'close'
    callbacks exist for any individual properties in the property list, they are
    called after the class 'close' callback.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5Pclose_list(hid_t plist_id)
{
    H5P_genplist_t	*plist;    /* Property list created */
    herr_t ret_value=FAIL;      /* return value */

    FUNC_ENTER (H5Pclose_list, FAIL);

    /* Check arguments. */
    if (H5I_GENPROP_LST != H5I_get_type(plist_id) || NULL == (plist = H5I_remove(plist_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

    /* Make call to property list class close callback, if needed */
    if(plist->class_init!=0 && plist->pclass->close_func!=NULL) {
        /* Call user's "close" callback function, ignoring return value */
        (plist->pclass->close_func)(plist_id,plist->pclass->close_data);
    } /* end if */

    /* Close the property list */
    if (H5P_close_list(plist) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close");

done:
    FUNC_LEAVE (ret_value);
}   /* H5Pclose_list() */


/*--------------------------------------------------------------------------
 NAME
    H5P_close_class
 PURPOSE
    Internal routine to close a property list class.
 USAGE
    herr_t H5P_create_class(class)
        H5P_genclass_t *class;  IN: Property list class to close
 RETURNS
    Returns non-negative on success, negative on failure.
 DESCRIPTION
    Releases memory and de-attach a class from the property list class hierarchy.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5P_close_class(H5P_genclass_t *class)
{
    herr_t ret_value=FAIL;     /* return value */

    FUNC_ENTER (H5P_close_class, FAIL);

    assert(class);

    /* Decrement parent class's dependant property class value! */
    if(H5P_access_class(class->parent,H5P_MOD_DEC_CLS)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTINIT, NULL,"Can't decrement class ref count");
    
    /* Mark class as deleted */
    class->deleted=1;

    /* Check dependancies on this class, deleting it if allowed */
    if(H5P_access_class(class,H5P_MOD_CHECK)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTINIT, NULL,"Can't check class ref count");

    /* Set return value */
    ret_value=SUCCEED;

done:
    FUNC_LEAVE (ret_value);
}   /* H5P_close_class() */


/*--------------------------------------------------------------------------
 NAME
    H5Pclose_class
 PURPOSE
    Close a property list class.
 USAGE
    herr_t H5Pclose_class(cls_id)
        hid_t cls_id;       IN: Property list class ID to class

 RETURNS
    Returns non-negative on success, negative on failure.
 DESCRIPTION
    Releases memory and de-attach a class from the property list class hierarchy.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5Pclose_class(hid_t cls_id)
{
    H5P_genclass_t	*pclass;        /* Property list class created */
    hid_t	ret_value = SUCCEED;    /* Return value			*/

    FUNC_ENTER(H5Pclose_class, FAIL);

    /* Check arguments */
    if (H5I_GENPROP_CLS != H5I_get_type(cls_id) || NULL == (pclass = H5I_remove(cls_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list class");

    /* Delete the property list class */
    if (H5P_close_class(pclass) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close");

done:
    FUNC_LEAVE(ret_value);
}   /* H5Pclose_class() */

