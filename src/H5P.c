/****************************************************************************
* NCSA HDF				                                    *
* Software Development Group			                            *
* National Center for Supercomputing Applications	                    *
* University of Illinois at Urbana-Champaign		                    *
* 605 E. Springfield, Champaign IL 61820		                    *
*							                    *
* For conditions of distribution and use, see the accompanying		    *
* hdf/COPYING file.							    *
*									    *
****************************************************************************/

/* $Id$ */

#define H5P_PACKAGE		/*suppress error about including H5Ppkg	  */

/* Private header files */
#include "H5private.h"		/* Generic Functions			*/
#include "H5Bprivate.h"		/* B-tree subclass names	  	*/
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"		/* Files		  	*/
#include "H5FDprivate.h"	/* File drivers				*/
#include "H5FLprivate.h"	/* Free Lists	  */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Ppkg.h"		/* Property lists		  	*/

/* Default file driver - see H5Pget_driver() */
#include "H5FDsec2.h"		/* Posix unbuffered I/O	file driver	*/

#define PABLO_MASK	H5P_mask

/* Is the interface initialized? */
static int		interface_initialize_g = 0;
#define INTERFACE_INIT H5P_init_interface
static herr_t		H5P_init_interface(void);

/*
 * Predefined property list classes. These are initialized at runtime by 
 * H5P_init_interface() in this source file.
 */
hid_t H5P_CLS_NO_CLASS_g            = FAIL;
hid_t H5P_CLS_FILE_CREATE_g         = FAIL;
hid_t H5P_CLS_FILE_ACCESS_g         = FAIL;
hid_t H5P_CLS_DATASET_CREATE_g      = FAIL;
hid_t H5P_CLS_DATASET_XFER_g        = FAIL;
hid_t H5P_CLS_MOUNT_g               = FAIL;

/*
 * Predefined property lists for each predefined class. These are initialized
 * at runtime by H5P_init_interface() in this source file.
 */
hid_t H5P_LST_NO_CLASS_g            = FAIL;
hid_t H5P_LST_FILE_CREATE_g         = FAIL;
hid_t H5P_LST_FILE_ACCESS_g         = FAIL;
hid_t H5P_LST_DATASET_CREATE_g      = FAIL;
hid_t H5P_LST_DATASET_XFER_g        = FAIL;
hid_t H5P_LST_MOUNT_g               = FAIL;

/* Local static functions */
static H5P_genclass_t *H5P_create_class(H5P_genclass_t *par_class,
     const char *name, unsigned hashsize, unsigned internal,
     H5P_cls_create_func_t cls_create, void *create_data,
     H5P_cls_copy_func_t cls_copy, void *copy_data,
     H5P_cls_close_func_t cls_close, void *close_data);
static herr_t H5P_close(void *_plist);
static herr_t H5P_close_class(void *_pclass);
static herr_t H5P_unregister(H5P_genclass_t *pclass, const char *name);
static H5P_genprop_t *H5P_dup_prop(H5P_genprop_t *oprop);
static herr_t H5P_access_class(H5P_genclass_t *pclass, H5P_class_mod_t mod);
static herr_t H5P_add_prop(H5P_genprop_t *hash[], unsigned hashsize, H5P_genprop_t *prop);
static herr_t H5P_free_prop(H5P_genprop_t *prop);


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
    unsigned H5P_xor_name(s)
        const char *s;  IN: String to operate over
RETURNS
    Always returns valid value
DESCRIPTION
    Generates an xor'ed value for a string
--------------------------------------------------------------------------*/
static unsigned
H5P_xor_name(const char *s)
{
    unsigned ret=0;
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
    unsigned H5P_hash_name(s, hashsize)
        const char *s;  IN: String to operate over
        unsigned;          IN: Size of hash table to clip against
RETURNS
    Always returns valid value
DESCRIPTION
    Generates a hash location based on an xor'ed value for a string
--------------------------------------------------------------------------*/
static unsigned
H5P_hash_name(const char *s, unsigned hashsize)
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
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER(H5P_init_interface, FAIL);

    /* Make certain IDs are initialized */
    if (ret_value < 0)
        HRETURN_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL, "unable to initialize atom group");
    
    /*
     * Initialize the Generic Property class & object groups.
     */
    if (H5I_init_group(H5I_GENPROP_CLS, H5I_GENPROPCLS_HASHSIZE, 0, (H5I_free_t)H5P_close_class) < 0)
        HRETURN_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL, "unable to initialize atom group");
    if (H5I_init_group(H5I_GENPROP_LST, H5I_GENPROPOBJ_HASHSIZE, 0, (H5I_free_t)H5P_close) < 0)
        HRETURN_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL, "unable to initialize atom group");

    /* Create root property list class */

    /* Allocate the root class */
    assert(H5P_CLS_NO_CLASS_g==(-1));
    if (NULL==(root_class = H5P_create_class (NULL,"none",H5P_NO_CLASS_HASH_SIZE,1,NULL,NULL,NULL,NULL,NULL,NULL)))
        HRETURN_ERROR (H5E_PLIST, H5E_CANTINIT, FAIL, "class initialization failed");

    /* Register the root class */
    if ((H5P_CLS_NO_CLASS_g = H5I_register (H5I_GENPROP_CLS, root_class))<0)
        HRETURN_ERROR (H5E_PLIST, H5E_CANTREGISTER, FAIL, "can't register property list class");

    /* Register the file creation and file access property classes */

    /* Allocate the file creation class */
    assert(H5P_CLS_FILE_CREATE_g==(-1));
    if (NULL==(pclass = H5P_create_class (root_class,"file create",H5P_FILE_CREATE_HASH_SIZE,1,NULL,NULL,NULL,NULL,NULL,NULL)))
        HRETURN_ERROR (H5E_PLIST, H5E_CANTINIT, FAIL, "class initialization failed");

    /* Register the file creation class */
    if ((H5P_CLS_FILE_CREATE_g = H5I_register (H5I_GENPROP_CLS, pclass))<0)
        HRETURN_ERROR (H5E_PLIST, H5E_CANTREGISTER, FAIL, "can't register property list class");

    /* Allocate the file access class */
    assert(H5P_CLS_FILE_ACCESS_g==(-1));
    if (NULL==(pclass = H5P_create_class (root_class,"file access",H5P_FILE_ACCESS_HASH_SIZE,1,H5F_acs_create,NULL,H5F_acs_copy,NULL,H5F_acs_close,NULL)))
        HRETURN_ERROR (H5E_PLIST, H5E_CANTINIT, FAIL, "class initialization failed");

    /* Register the file access class */
    if ((H5P_CLS_FILE_ACCESS_g = H5I_register (H5I_GENPROP_CLS, pclass))<0)
        HRETURN_ERROR (H5E_PLIST, H5E_CANTREGISTER, FAIL, "can't register property list class");

    /* Register the dataset creation and data xfer property classes */

    /* Allocate the dataset creation class */
    assert(H5P_CLS_DATASET_CREATE_g==(-1));
    if (NULL==(pclass = H5P_create_class (root_class,"dataset create",H5P_DATASET_CREATE_HASH_SIZE,1,NULL,NULL,H5D_crt_copy,NULL,H5D_crt_close,NULL)))
        HRETURN_ERROR (H5E_PLIST, H5E_CANTINIT, FAIL, "class initialization failed");

    /* Register the dataset creation class */
    if ((H5P_CLS_DATASET_CREATE_g = H5I_register (H5I_GENPROP_CLS, pclass))<0)
        HRETURN_ERROR (H5E_PLIST, H5E_CANTREGISTER, FAIL, "can't register property list class");

    /* Allocate the data xfer class */
    assert(H5P_CLS_DATASET_XFER_g==(-1));
    if (NULL==(pclass = H5P_create_class (root_class,"data xfer",H5P_DATASET_XFER_HASH_SIZE,1,H5D_xfer_create,NULL,H5D_xfer_copy,NULL,H5D_xfer_close,NULL)))
        HRETURN_ERROR (H5E_PLIST, H5E_CANTINIT, FAIL, "class initialization failed");

    /* Register the data xfer class */
    if ((H5P_CLS_DATASET_XFER_g = H5I_register (H5I_GENPROP_CLS, pclass))<0)
        HRETURN_ERROR (H5E_PLIST, H5E_CANTREGISTER, FAIL, "can't register property list class");

    /* Allocate the mount class */
    assert(H5P_CLS_MOUNT_g==(-1));
    if (NULL==(pclass = H5P_create_class (root_class,"file mount",H5P_MOUNT_HASH_SIZE,1,NULL,NULL,NULL,NULL,NULL,NULL)))
        HRETURN_ERROR (H5E_PLIST, H5E_CANTINIT, FAIL, "class initialization failed");

    /* Register the mount class */
    if ((H5P_CLS_MOUNT_g = H5I_register (H5I_GENPROP_CLS, pclass))<0)
        HRETURN_ERROR (H5E_PLIST, H5E_CANTREGISTER, FAIL, "can't register property list class");

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
int
H5P_term_interface(void)
{
    int	nlist=0;
    int	nclass=0;
    int	n=0;

    if (interface_initialize_g) {
        /* Destroy HDF5 library property classes & lists */

        /* Check if there are any open property list classes or lists */
        nclass = H5I_nmembers(H5I_GENPROP_CLS);
        nlist = H5I_nmembers(H5I_GENPROP_LST);
        n=nclass+nlist;

        /* If there are any open classes or groups, attempt to get rid of them. */
        if (n) {
            /* Clear the lists */
            if(nlist>0) {
                H5I_clear_group(H5I_GENPROP_LST, FALSE);

                /* Reset the default property lists, if they've been closed */
                if(H5I_nmembers(H5I_GENPROP_LST)==0) {
                    H5P_LST_NO_CLASS_g =
                        H5P_LST_FILE_CREATE_g =
                        H5P_LST_FILE_ACCESS_g =
                        H5P_LST_DATASET_CREATE_g =
                        H5P_LST_DATASET_XFER_g =
                        H5P_LST_MOUNT_g = (-1);
                } /* end if */
            } /* end if */

            /* Only attempt to close the classes after all the lists are closed */
            if(nlist==0 && nclass>0) {
                H5I_clear_group(H5I_GENPROP_CLS, FALSE);

                /* Reset the default property lists, if they've been closed */
                if(H5I_nmembers(H5I_GENPROP_CLS)==0) {
                    H5P_CLS_NO_CLASS_g =
                        H5P_CLS_FILE_CREATE_g =
                        H5P_CLS_FILE_ACCESS_g =
                        H5P_CLS_DATASET_CREATE_g =
                        H5P_CLS_DATASET_XFER_g =
                        H5P_CLS_MOUNT_g = (-1);
                } /* end if */
            } /* end if */
        } else {
            H5I_destroy_group(H5I_GENPROP_LST);
            n++; /*H5I*/
            H5I_destroy_group(H5I_GENPROP_CLS);
            n++; /*H5I*/

            interface_initialize_g = 0;
        }
    }
    return n;
}


/*--------------------------------------------------------------------------
 NAME
    H5P_copy_pclass
 PURPOSE
    Internal routine to copy a generic property class
 USAGE
    hid_t H5P_copy_pclass(pclass)
        H5P_genclass_t *pclass;      IN: Property class to copy
 RETURNS
    Success: valid property class ID on success (non-negative)
    Failure: negative
 DESCRIPTION
    Copy a property class and return the ID.  This routine does not make
    any callbacks.  (They are only make when operating on property lists).

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static hid_t H5P_copy_pclass(H5P_genclass_t *pclass)
{
    H5P_genclass_t *new_pclass = NULL;      /* Property list class copied */
    H5P_genprop_t *tmp;         /* Temporary pointer to parent class properties */
    H5P_genprop_t *pcopy;       /* Copy of property to insert into class */
    unsigned u;                    /* Local index variable */
    hid_t ret_value=FAIL;       /* return value */

    FUNC_ENTER (H5P_copy_pclass, FAIL);

    assert(pclass);

    /* 
     * Create new property class object
     */

    /* Create the new property list class */
    if (NULL==(new_pclass=H5P_create_class(pclass->parent, pclass->name, pclass->hashsize, 0, pclass->create_func, pclass->create_data, pclass->copy_func, pclass->copy_data, pclass->close_func, pclass->close_data)))
        HGOTO_ERROR(H5E_PLIST, H5E_CANTCREATE, FAIL, "unable to create property list class");

    /* Copy the properties registered for this class */
    if(pclass->nprops>0) {
        /* Walk through the hash table */
        for(u=0; u<pclass->hashsize; u++) {
            tmp=pclass->props[u];

            /* Walk through the list of properties at each hash location */
            while(tmp!=NULL) {
                /* Make a copy of the class's property */
                if((pcopy=H5P_dup_prop(tmp))==NULL)
                    HGOTO_ERROR (H5E_PLIST, H5E_CANTCOPY, FAIL,"Can't copy property");

                /* Insert the initialized property into the property list */
                if(H5P_add_prop(new_pclass->props,new_pclass->hashsize,pcopy)<0)
                    HGOTO_ERROR (H5E_PLIST, H5E_CANTINSERT, FAIL,"Can't insert property into class");

                /* Increment property count for class */
                new_pclass->nprops++;

                /* Go to next registered property in class */
                tmp=tmp->next;
            } /* end while */
        } /* end for */
    } /* end if */

    /* Increment parent class's derived class value */
    if(new_pclass->parent!=NULL)
        if(H5P_access_class(new_pclass->parent,H5P_MOD_INC_CLS)<0)
            HGOTO_ERROR (H5E_PLIST, H5E_CANTINIT, FAIL,"Can't increment parent class ref count");

    /* Get an atom for the class */
    if ((ret_value = H5I_register(H5I_GENPROP_CLS, new_pclass))<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to atomize property list class");

done:
    if (ret_value<0 && new_pclass)
        H5P_close_class(new_pclass);

    FUNC_LEAVE (ret_value);
}   /* H5P_copy_pclass() */


/*--------------------------------------------------------------------------
 NAME
    H5P_copy_plist
 PURPOSE
    Internal routine to copy a generic property list
 USAGE
        hid_t H5P_copy_plist(old_plist_id)
            hid_t old_plist_id;             IN: Property list ID to copy
 RETURNS
    Success: valid property list ID on success (non-negative)
    Failure: negative
 DESCRIPTION
    Copy a property list and return the ID.  This routine calls the
    class 'copy' callback after any property 'copy' callbacks are called
    (assuming all property 'copy' callbacks return successfully).

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hid_t H5P_copy_plist(H5P_genplist_t *old_plist)
{
    H5P_genplist_t *new_plist=NULL;  /* New property list generated from copy */
    H5P_genprop_t *tprop;       /* Temporary pointer to properties */
    H5P_genprop_t *new_prop;    /* New property created for copy */
    hid_t new_plist_id;         /* Property list ID of new list created */
    unsigned u;                 /* Local index variable */
    hid_t ret_value=FAIL;       /* return value */
 
    FUNC_ENTER (H5P_copy_plist, FAIL);

    assert(old_plist);

    /* 
     * Create new property list object
     */

    /* Allocate room for the property list & it's hash table of properties */
    if (NULL==(new_plist = H5MM_calloc (sizeof(H5P_genplist_t)+((old_plist->pclass->hashsize-1)*sizeof(H5P_genprop_t *)))))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,"memory allocation failed");

    /* Set class state */
    new_plist->pclass = old_plist->pclass;
    new_plist->nprops = 0;      /* Initially the plist has the same number of properties as the class */
    new_plist->class_init = 0;  /* Initially, wait until the class callback finishes to set */

    /* Cycle through the properties and copy them also */
    for(u=0; u<old_plist->pclass->hashsize; u++) {
        tprop=old_plist->props[u];

        /* Walk through the list of properties at each hash location */
        while(tprop!=NULL) {
            /* Make a copy of the list's property */
            if((new_prop=H5P_dup_prop(tprop))==NULL)
                HGOTO_ERROR (H5E_PLIST, H5E_CANTCOPY, FAIL,"Can't copy property");

            /* Call property copy callback, if it exists */
            if(new_prop->copy) {
                if((new_prop->copy)(new_prop->name,new_prop->size,new_prop->value)<0) {
                    H5P_free_prop(new_prop);
                    HGOTO_ERROR (H5E_PLIST, H5E_CANTCOPY, FAIL,"Can't copy property");
                } /* end if */
            } /* end if */

            /* Insert the initialized property into the property list */
            if(H5P_add_prop(new_plist->props,new_plist->pclass->hashsize,new_prop)<0)
                HGOTO_ERROR (H5E_PLIST, H5E_CANTINSERT, FAIL,"Can't insert property into list");

            /* Increment the number of properties in list */
            new_plist->nprops++;

            /* Go to next registered property in class */
            tprop=tprop->next;
        } /* end while */
    } /* end for */

    /* Increment the number of property lists derived from class */
    if(H5P_access_class(new_plist->pclass,H5P_MOD_INC_LST)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTINIT, FAIL,"Can't increment class ref count");

    /* Get an atom for the property list */
    if ((new_plist_id = H5I_register(H5I_GENPROP_LST, new_plist))<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to atomize property list");

    /* Save the property list ID in the property list struct, for use in the property class's 'close' callback */
    new_plist->plist_id=new_plist_id;

    /* Call the class callback (if it exists) now that we have the property list ID */
    if(new_plist->pclass->copy_func!=NULL) {
        if((new_plist->pclass->copy_func)(new_plist_id,old_plist->plist_id,old_plist->pclass->copy_data)<0) {
            /* Delete ID, ignore return value */
            H5I_remove(new_plist_id);
            HGOTO_ERROR (H5E_PLIST, H5E_CANTINIT, FAIL,"Can't initialize property");
        } /* end if */
    } /* end if */

    /* Set the class initialization flag */
    new_plist->class_init=1;

    /* Set the return value */
    ret_value=new_plist_id;

done:
    if (ret_value<0 && new_plist)
        H5P_close(new_plist);

    FUNC_LEAVE (ret_value);
}   /* H5P_copy_plist() */


/*--------------------------------------------------------------------------
 NAME
    H5Pcopy
 PURPOSE
    Routine to copy a property list or class
 USAGE
    hid_t H5Pcopy(id)
        hid_t id;           IN: Property list or class ID to copy
 RETURNS
    Success: valid property list ID on success (non-negative)
    Failure: negative
 DESCRIPTION
    Copy a property list or class and return the ID.  This routine calls the
    class 'copy' callback after any property 'copy' callbacks are called
    (assuming all property 'copy' callbacks return successfully).

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hid_t H5Pcopy(hid_t id)
{
    void *obj;                 /* Property object to copy */
    hid_t ret_value=FALSE;      /* return value */

    FUNC_ENTER (H5Pcopy, FAIL);

    if (H5P_DEFAULT==id)
        HGOTO_DONE(H5P_DEFAULT);

    /* Check arguments. */
    if (H5I_GENPROP_LST != H5I_get_type(id) && H5I_GENPROP_CLS != H5I_get_type(id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not property object");
    if(NULL == (obj = H5I_object(id)))
        HGOTO_ERROR(H5E_PLIST, H5E_NOTFOUND, FAIL, "property object doesn't exist");

    /* Compare property lists */
    if(H5I_GENPROP_LST == H5I_get_type(id)) {
        if((ret_value=H5P_copy_plist(obj))<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTCOPY, FAIL, "can't copy property list");
    } /* end if */
    /* Must be property classes */
    else {
        if((ret_value=H5P_copy_pclass(obj))<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTCOPY, FAIL, "can't copy property class");
    } /* end else */

done:
    FUNC_LEAVE (ret_value);
}   /* H5Pcopy() */


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
 * 		Raymond Lu, Oct 14, 2001
 * 		Change to the new generic property list.
 *	
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_version(hid_t plist_id, int *boot/*out*/, int *freelist/*out*/,
	       int *stab/*out*/, int *shhdr/*out*/)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=FAIL;      /* return value */

    FUNC_ENTER(H5Pget_version, FAIL);
    H5TRACE5("e","ixxxx",plist_id,boot,freelist,stab,shhdr);

    /* Check arguments */
    if(TRUE != H5P_isa_class(plist_id, H5P_FILE_CREATE))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Get values */
    if (boot)
        if(H5P_get(plist, H5F_CRT_BOOT_VERS_NAME, boot) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, 
                          "can't get boot version");
    if (freelist)
        if(H5P_get(plist, H5F_CRT_FREESPACE_VERS_NAME, freelist) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, 
                          "can't get free-space version");
    if (stab)
        if(H5P_get(plist, H5F_CRT_OBJ_DIR_VERS_NAME, stab) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, 
                          "can't get object directory version");
    if (shhdr)
        if(H5P_get(plist, H5F_CRT_SHARE_HEAD_VERS_NAME, shhdr) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, 
                          "can't get shared-header version");

    /* Set return value */
    ret_value=SUCCEED;

done:
    FUNC_LEAVE(ret_value);
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

    FUNC_ENTER(H5Pset_userblock, FAIL);
    H5TRACE2("e","ih",plist_id,size);

    /* Check arguments */
    if(TRUE != H5P_isa_class(plist_id, H5P_FILE_CREATE))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

    /* Check that the userblock size is a power of two */
    for (i=8; i<8*sizeof(hsize_t); i++) {
        hsize_t p2 = 8==i ? 0 : ((hsize_t)1<<i);

        if (size == p2)
            break;
    }
    if (i>=8*sizeof(hsize_t))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "userblock size is not valid");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Set value */
    if(H5P_set(plist, H5F_CRT_USER_BLOCK_NAME, &size) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set user block");

done:
    FUNC_LEAVE(ret_value);
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

    FUNC_ENTER(H5Pget_userblock, FAIL);
    H5TRACE2("e","i*h",plist_id,size);

    /* Check args */
    if(TRUE != H5P_isa_class(plist_id, H5P_FILE_CREATE))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Get value */
    if (size)
        if(H5P_get(plist, H5F_CRT_USER_BLOCK_NAME, size) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL,"can't get user block");

done:
    FUNC_LEAVE(ret_value);
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
 *		Raymond Lu
 *		Tuesday, Oct 23, 2001
 *		Changed file access property list mechanism to the new 
 *		generic property list.
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_alignment(hid_t fapl_id, hsize_t threshold, hsize_t alignment)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */
    
    FUNC_ENTER (H5Pset_alignment, FAIL);
    H5TRACE3("e","ihh",fapl_id,threshold,alignment);

    /* Check args */
    if(TRUE != H5P_isa_class(fapl_id, H5P_FILE_ACCESS))     
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list");
    if (alignment<1)
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "alignment must be positive");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Set values */
    if(H5P_set(plist, H5F_ACS_ALIGN_THRHD_NAME, &threshold) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set threshold");
    if(H5P_set(plist, H5F_ACS_ALIGN_NAME, &alignment) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set alignment");

done:
    FUNC_LEAVE(ret_value);
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
 *		Raymond Lu 
 *		Tuesday, Oct 23, 2001
 *		Changed the file access list design to the new generic 
 *		property list.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_alignment(hid_t fapl_id, hsize_t *threshold/*out*/,
		  hsize_t *alignment/*out*/)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=FAIL;      /* return value */

    FUNC_ENTER (H5Pget_alignment, FAIL);
    H5TRACE3("e","ixx",fapl_id,threshold,alignment);

    /* Check args */
    if(TRUE != H5P_isa_class(fapl_id, H5P_FILE_ACCESS))     
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Get values */
    if (threshold)
        if(H5P_get(plist, H5F_ACS_ALIGN_THRHD_NAME, threshold) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get threshold");
    if (alignment)
        if(H5P_get(plist, H5F_ACS_ALIGN_NAME, alignment) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get alignment");

    /* Set return value */
    ret_value=SUCCEED;

done:
    FUNC_LEAVE(ret_value);
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

    FUNC_ENTER(H5Pset_sizes, FAIL);
    H5TRACE3("e","izz",plist_id,sizeof_addr,sizeof_size);

    /* Check arguments */
    if(TRUE != H5P_isa_class(plist_id, H5P_FILE_CREATE))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

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
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Set value */
    if (sizeof_addr)
        if(H5P_set(plist, H5F_CRT_ADDR_BYTE_NUM_NAME, &sizeof_addr) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set byte number for an address");
    if (sizeof_size)
        if(H5P_set(plist, H5F_CRT_OBJ_BYTE_NUM_NAME, &sizeof_size) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set byte number for object ");

done:
    FUNC_LEAVE(ret_value);
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

    FUNC_ENTER(H5Pget_sizes, FAIL);
    H5TRACE3("e","ixx",plist_id,sizeof_addr,sizeof_size);

    /* Check args */
    if(TRUE != H5P_isa_class(plist_id, H5P_FILE_CREATE))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Get values */
    if (sizeof_addr)
        if(H5P_get(plist, H5F_CRT_ADDR_BYTE_NUM_NAME, sizeof_addr) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get byte number for an address");
    if (sizeof_size)
        if(H5P_get(plist, H5F_CRT_OBJ_BYTE_NUM_NAME, sizeof_size) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get byte number for object ");

done:
    FUNC_LEAVE(ret_value);
}

#ifdef H5_WANT_H5_V1_4_COMPAT

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
H5Pset_sym_k(hid_t plist_id, int ik, int lk)
{
    int btree_k[H5B_NUM_BTREE_ID];
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=FAIL;      /* return value */

    FUNC_ENTER(H5Pset_sym_k, FAIL);
    H5TRACE3("e","iIsIu",plist_id,ik,lk);

    /* Check arguments */
    if(TRUE != H5P_isa_class(plist_id, H5P_FILE_CREATE))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
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

    /* Set return value */
    ret_value=SUCCEED;

done:
    FUNC_LEAVE(ret_value);
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
H5Pget_sym_k(hid_t plist_id, int *ik /*out */ , int *lk /*out */ )
{
    int btree_k[H5B_NUM_BTREE_ID];
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=FAIL;      /* return value */

    FUNC_ENTER(H5Pget_sym_k, FAIL);
    H5TRACE3("e","ixx",plist_id,ik,lk);

    /* Check arguments */
    if(TRUE != H5P_isa_class(plist_id, H5P_FILE_CREATE))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
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

    /* Set return value */
    ret_value=SUCCEED;

done:
    FUNC_LEAVE(ret_value);
}
#else /* H5_WANT_H5_V1_4_COMPAT */

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
H5Pset_sym_k(hid_t plist_id, int ik, unsigned lk)
{
    int btree_k[H5B_NUM_BTREE_ID];
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=FAIL;      /* return value */

    FUNC_ENTER(H5Pset_sym_k, FAIL);
    H5TRACE3("e","iIsIu",plist_id,ik,lk);

    /* Check arguments */
    if(TRUE != H5P_isa_class(plist_id, H5P_FILE_CREATE))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
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

    /* Set return value */
    ret_value=SUCCEED;

done:
    FUNC_LEAVE(ret_value);
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
H5Pget_sym_k(hid_t plist_id, int *ik /*out */ , unsigned *lk /*out */ )
{
    int btree_k[H5B_NUM_BTREE_ID];
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=FAIL;      /* return value */

    FUNC_ENTER(H5Pget_sym_k, FAIL);
    H5TRACE3("e","ixx",plist_id,ik,lk);

    /* Check arguments */
    if(TRUE != H5P_isa_class(plist_id, H5P_FILE_CREATE))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
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

    /* Set return value */
    ret_value=SUCCEED;

done:
    FUNC_LEAVE(ret_value);
}
#endif /* H5_WANT_H5_V1_4_COMPAT */


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
H5Pset_istore_k(hid_t plist_id, int ik)
{
    int btree_k[H5B_NUM_BTREE_ID];
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=FAIL;      /* return value */

    FUNC_ENTER(H5Pset_istore_k, FAIL);
    H5TRACE2("e","iIs",plist_id,ik);

    /* Check arguments */
    if(TRUE != H5P_isa_class(plist_id, H5P_FILE_CREATE))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
    if (ik <= 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "istore IK value must be positive");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Set value */
    if(H5P_get(plist, H5F_CRT_BTREE_RANK_NAME, btree_k) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get rank for btree interanl nodes");
    btree_k[H5B_ISTORE_ID] = ik;
    if(H5P_set(plist, H5F_CRT_BTREE_RANK_NAME, btree_k) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set rank for btree interanl nodes");

    /* Set return value */
    ret_value=SUCCEED;

done:
    FUNC_LEAVE(ret_value);
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
H5Pget_istore_k(hid_t plist_id, int *ik /*out */ )
{
    int btree_k[H5B_NUM_BTREE_ID];
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER(H5Pget_istore_k, FAIL);
    H5TRACE2("e","ix",plist_id,ik);

    /* Check arguments */
    if(TRUE != H5P_isa_class(plist_id, H5P_FILE_CREATE))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Get value */
    if (ik) {
        if(H5P_get(plist, H5F_CRT_BTREE_RANK_NAME, btree_k) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get rank for btree interanl nodes");
        *ik = btree_k[H5B_ISTORE_ID];
    }

done:
    FUNC_LEAVE(ret_value);
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
 *              Raymond Lu
 *              Tuesday, October 2, 2001
 *              Changed the way to check parameter and set property for 
 *              generic property list.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_layout(hid_t plist_id, H5D_layout_t layout)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER(H5Pset_layout, FAIL);
    H5TRACE2("e","iDl",plist_id,layout);

    /* Check arguments */
    if(TRUE != H5P_isa_class(plist_id, H5P_DATASET_CREATE))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list");
    if (layout < 0 || layout >= H5D_NLAYOUTS)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "raw data layout method is not valid");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Set value */
    if(H5P_set(plist, H5D_CRT_LAYOUT_NAME, &layout) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "can't set layout");

done:
    FUNC_LEAVE(ret_value);
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
 *              Raymond Lu
 *              Tuesday, October 2, 2001
 *              Changed the way to check parameter and get property for 
 *              generic property list.
 *
 *-------------------------------------------------------------------------
 */
H5D_layout_t
H5Pget_layout(hid_t plist_id)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    H5D_layout_t   ret_value=H5D_LAYOUT_ERROR;

    FUNC_ENTER(H5Pget_layout, H5D_LAYOUT_ERROR);
    H5TRACE1("Dl","i",plist_id);

    /* Check arguments */
    if(TRUE != H5P_isa_class(plist_id, H5P_DATASET_CREATE))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5D_LAYOUT_ERROR, "not a dataset creation property list");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, H5D_LAYOUT_ERROR, "can't find object for ID");

    /* Get value */
    if(H5P_get(plist, H5D_CRT_LAYOUT_NAME, &ret_value) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, H5D_LAYOUT_ERROR, "can't get layout");

done:
    FUNC_LEAVE(ret_value);
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
 *              Raymond Lu
 *              Tuesday, October 2, 2001
 *              Changed the way to check parameter and set property for 
 *              generic property list.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_chunk(hid_t plist_id, int ndims, const hsize_t dim[/*ndims*/])
{
    int			    i;
    hsize_t real_dims[H5O_LAYOUT_NDIMS]; /* Full-sized array to hold chunk dims */
    H5D_layout_t           layout;
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER(H5Pset_chunk, FAIL);
    H5TRACE3("e","iIs*[a1]h",plist_id,ndims,dim);

    /* Check arguments */
    if(TRUE != H5P_isa_class(plist_id, H5P_DATASET_CREATE))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list");
    if (ndims <= 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "chunk dimensionality must be positive");
    if (ndims > H5S_MAX_RANK)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "chunk dimensionality is too large");
    if (!dim)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no chunk dimensions specified");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Initialize chunk dims to 0s */
    HDmemset(real_dims,0,H5O_LAYOUT_NDIMS*sizeof(hsize_t));
    for (i=0; i<ndims; i++) {
        if (dim[i] <= 0)
            HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "all chunk dimensions must be positive");
        real_dims[i]=dim[i]; /* Store user's chunk dimensions */
    }

    layout = H5D_CHUNKED;
    if(H5P_set(plist, H5D_CRT_LAYOUT_NAME, &layout) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "can't set layout");
    if(H5P_set(plist, H5D_CRT_CHUNK_DIM_NAME, &ndims) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "can't set chunk dimensionanlity");
    if(H5P_set(plist, H5D_CRT_CHUNK_SIZE_NAME, real_dims) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "can't set chunk size");

done:
    FUNC_LEAVE(ret_value);
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
 *              Raymond Lu
 *              Tuesday, October 2, 2001
 *              Changed the way to check parameter and set property for 
 *              generic property list.
 *
 *-------------------------------------------------------------------------
 */
int
H5Pget_chunk(hid_t plist_id, int max_ndims, hsize_t dim[]/*out*/)
{
    int			i;
    int                 ndims;
    H5D_layout_t        layout;
    hsize_t             chunk_size[32];
    H5P_genplist_t *plist;      /* Property list pointer */
    int                 ret_value;

    FUNC_ENTER(H5Pget_chunk, FAIL);
    H5TRACE3("Is","iIsx",plist_id,max_ndims,dim);

    /* Check arguments */
    if(TRUE != H5P_isa_class(plist_id, H5P_DATASET_CREATE))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list");
   
    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    if(H5P_get(plist, H5D_CRT_LAYOUT_NAME, &layout) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't get layout"); 
    if(H5D_CHUNKED != layout) 
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a chunked storage layout");
   
    if(H5P_get(plist, H5D_CRT_CHUNK_SIZE_NAME, chunk_size) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get chunk size");
    if(H5P_get(plist, H5D_CRT_CHUNK_DIM_NAME, &ndims) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get chunk dimensionality");

    /* Get the dimension sizes */
    for (i=0; i<ndims && i<max_ndims && dim; i++)
        dim[i] = chunk_size[i];

    /* Set the return value */
    ret_value=ndims;

done:
    FUNC_LEAVE(ret_value);
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
 *              Raymond Lu
 *              Tuesday, October 2, 2001
 *              Changed the way to check parameter and set property for 
 *              generic property list.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_external(hid_t plist_id, const char *name, off_t offset, hsize_t size)
{
    int			idx;
    hsize_t		total, tmp;
    H5O_efl_t           efl;
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=FAIL;      /* return value */

    FUNC_ENTER(H5Pset_external, FAIL);
    H5TRACE4("e","isoh",plist_id,name,offset,size);

    /* Check arguments */
    if(TRUE != H5P_isa_class(plist_id, H5P_DATASET_CREATE))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list");
    if (!name || !*name)
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "no name given");
    if (offset<0)
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "negative external file offset");
    if (size<=0)
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "zero size");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    if(H5P_get(plist, H5D_CRT_EXT_FILE_LIST_NAME, &efl) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get external file list");
    if(efl.nused > 0 && H5O_EFL_UNLIMITED==efl.slot[efl.nused-1].size)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "previous file size is unlimited");

    if (H5O_EFL_UNLIMITED!=size) {
        for (idx=0, total=size; idx<efl.nused; idx++, total=tmp) {
            tmp = total + efl.slot[idx].size;
            if (tmp <= total)
                HGOTO_ERROR (H5E_EFL, H5E_OVERFLOW, FAIL, "total external data size overflowed");
        } /* end for */
    } /* end if */
    

    /* Add to the list */
    if (efl.nused >= efl.nalloc) {
        int na = efl.nalloc + H5O_EFL_ALLOC;
        H5O_efl_entry_t *x = H5MM_realloc (efl.slot, na*sizeof(H5O_efl_entry_t));

        if (!x)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        efl.nalloc = na;
        efl.slot = x;
    }
    idx = efl.nused;
    efl.slot[idx].name_offset = 0; /*not entered into heap yet*/
    efl.slot[idx].name = H5MM_xstrdup (name);
    efl.slot[idx].offset = offset;
    efl.slot[idx].size = size;
    efl.nused++;
    
    if(H5P_set(plist, H5D_CRT_EXT_FILE_LIST_NAME, &efl) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "can't set external file list");

    /* Set return value */
    ret_value=SUCCEED;

done:
    FUNC_LEAVE(ret_value);
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
 *              Raymond Lu
 *              Tuesday, October 2, 2001
 *              Changed the way to check parameter and set property for 
 *              generic property list.
 *
 *-------------------------------------------------------------------------
 */
int
H5Pget_external_count(hid_t plist_id)
{
    H5O_efl_t           efl;
    H5P_genplist_t *plist;      /* Property list pointer */
    int ret_value=SUCCEED;      /* return value */

    FUNC_ENTER (H5Pget_external_count, FAIL);
    H5TRACE1("Is","i",plist_id);
    
    /* Check arguments */
    if(TRUE != H5P_isa_class(plist_id, H5P_DATASET_CREATE))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Get value */
    if(H5P_get(plist, H5D_CRT_EXT_FILE_LIST_NAME, &efl) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get external file list");
    
    /* Return value */
    ret_value=efl.nused;
    
done:
    FUNC_LEAVE(ret_value);
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
 *              Raymond Lu
 *              Tuesday, October 2, 2001
 *              Changed the way to check parameter and get property for 
 *              generic property list.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_external(hid_t plist_id, int idx, size_t name_size, char *name/*out*/,
		 off_t *offset/*out*/, hsize_t *size/*out*/)
{
    H5O_efl_t           efl;
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER (H5Pget_external, FAIL);
    H5TRACE6("e","iIszxxx",plist_id,idx,name_size,name,offset,size);
    
    /* Check arguments */
    if(TRUE != H5P_isa_class(plist_id, H5P_DATASET_CREATE))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list");
   
    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Get value */
    if(H5P_get(plist, H5D_CRT_EXT_FILE_LIST_NAME, &efl) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get external file list");
    
    if (idx<0 || idx>=efl.nused)
        HGOTO_ERROR (H5E_ARGS, H5E_BADRANGE, FAIL, "external file index is out of range");

    /* Return values */
    if (name_size>0 && name)
        HDstrncpy (name, efl.slot[idx].name, name_size);
    if (offset)
        *offset = efl.slot[idx].offset;
    if (size)
        *size = efl.slot[idx].size;

done:
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5P_set_driver
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
 * 		Raymond Lu
 * 		Tuesday, Oct 23, 2001
 *		Changed the file access list design to the new generic 
 *		property list.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5P_set_driver(H5P_genplist_t *plist, hid_t new_driver_id, const void *new_driver_info)
{
    hid_t driver_id;            /* VFL driver ID */
    void *driver_info;          /* VFL driver info */
    void *tmp_driver_info;      /* Temporary VFL driver info */
    herr_t ret_value=SUCCEED;   /* Return value */
    
    FUNC_ENTER(H5P_set_driver, FAIL);

    if (H5I_VFL!=H5I_get_type(new_driver_id) || NULL==H5I_object(new_driver_id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file driver ID");

    if( TRUE == H5P_isa_class(plist->plist_id, H5P_FILE_ACCESS) ) {
        /* Remove old driver */
        if(H5P_get(plist, H5F_ACS_FILE_DRV_ID_NAME, &driver_id) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get driver ID");
        if(H5P_get(plist, H5F_ACS_FILE_DRV_INFO_NAME, &driver_info) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,FAIL,"can't get driver info");
        assert(driver_id>=0);
        H5FD_fapl_free(driver_id, driver_info);
        H5I_dec_ref(driver_id);

        /* Add new driver */
        H5I_inc_ref(new_driver_id);
        driver_id   = new_driver_id;
        driver_info = H5FD_fapl_copy(new_driver_id, new_driver_info);
 
        if(H5P_set(plist, H5F_ACS_FILE_DRV_ID_NAME, &driver_id) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set driver ID");
        if(H5P_set(plist, H5F_ACS_FILE_DRV_INFO_NAME, &driver_info) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET,FAIL,"can't set driver info");
        
    } else if( TRUE == H5P_isa_class(plist->plist_id, H5P_DATASET_XFER) ) {
        /* Get the current driver information */
        if(H5P_get(plist, H5D_XFER_VFL_ID_NAME, &driver_id)<0)
            HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve VFL driver ID");
        if(H5P_get(plist, H5D_XFER_VFL_INFO_NAME, &driver_info)<0)
            HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve VFL driver info");

        /* Remove old driver */

        /* Double-check value... */
        assert(driver_id>=0);

        /* Free any driver information stored */
        H5FD_dxpl_free(driver_id, driver_info);

        /* Decrement reference count for old driver */
        H5I_dec_ref(driver_id);

        /* Add new driver */

        /* Increment reference count for new driver */
        H5I_inc_ref(new_driver_id);

        /* Make a copy of the driver information */
        if((tmp_driver_info = H5FD_dxpl_copy(new_driver_id, new_driver_info))==NULL)
            HGOTO_ERROR (H5E_DATASET, H5E_CANTCOPY, FAIL, "Can't copy VFL driver");

        /* Set the driver info for the property list */
        if(H5P_set(plist, H5D_XFER_VFL_ID_NAME, &new_driver_id)<0)
            HGOTO_ERROR (H5E_PLIST, H5E_CANTSET, FAIL, "Can't set VFL driver ID");
        if(H5P_set(plist, H5D_XFER_VFL_INFO_NAME, &tmp_driver_info) < 0)
            HGOTO_ERROR (H5E_PLIST, H5E_CANTSET, FAIL, "Can't set VFL driver info");
        
    } else {
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access or data transfer property list");
    }

done:
    FUNC_LEAVE(ret_value);
} /* end H5P_set_driver() */


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
 * 		Raymond Lu
 * 		Tuesday, Oct 23, 2001
 *		Changed the file access list design to the new generic 
 *		property list.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_driver(hid_t plist_id, hid_t new_driver_id, const void *new_driver_info)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* Return value */
    
    FUNC_ENTER(H5Pset_driver, FAIL);
    H5TRACE3("e","iix",plist_id,new_driver_id,new_driver_info);

    /* Check arguments */
    if(H5I_GENPROP_LST != H5I_get_type(plist_id) || NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
    if (H5I_VFL!=H5I_get_type(new_driver_id) || NULL==H5I_object(new_driver_id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file driver ID");

    /* Set the driver */
    if(H5P_set_driver(plist,new_driver_id,new_driver_info)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set driver info");

done:
    FUNC_LEAVE(ret_value);
} /* end H5Pset_driver() */


/*-------------------------------------------------------------------------
 * Function:	H5P_get_driver
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
 *		If the driver ID is H5FD_VFD_DEFAULT then substitute the 
 *              current value of H5FD_SEC2.
 *
 * 		Quincey Koziol 2000-11-28
 *		Added internal function..
 *
 *		Raymond Lu, 2001-10-23
 *		Changed the file access list design to the new generic 
 *		property list. 
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5P_get_driver(H5P_genplist_t *plist)
{
    hid_t	ret_value=-1;

    FUNC_ENTER (H5P_get_driver, FAIL);

    /* Get the current driver ID */
    if(TRUE == H5P_isa_class(plist->plist_id, H5P_FILE_ACCESS) ) {
        if(H5P_get(plist, H5F_ACS_FILE_DRV_ID_NAME, &ret_value) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get driver ID");
    } else if( TRUE == H5P_isa_class(plist->plist_id, H5P_DATASET_XFER) ) {
        if(H5P_get(plist, H5D_XFER_VFL_ID_NAME, &ret_value)<0)
            HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve VFL driver ID");
    } else {
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access or data transfer property list");
    }

    if (H5FD_VFD_DEFAULT==ret_value)
        ret_value = H5FD_SEC2;

done:
    FUNC_LEAVE(ret_value);
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
 *		If the driver ID is H5FD_VFD_DEFAULT then substitute the current value of
 *		H5FD_SEC2.
 *
 * 		Quincey Koziol 2000-11-28
 *		Added internal function..
 *-------------------------------------------------------------------------
 */
hid_t
H5Pget_driver(hid_t plist_id)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    hid_t	ret_value=-1;

    FUNC_ENTER (H5Pget_driver, FAIL);
    H5TRACE1("i","i",plist_id);

    if(H5I_GENPROP_LST != H5I_get_type(plist_id) || NULL == (plist = H5I_object(plist_id)))
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

    ret_value = H5P_get_driver(plist);

    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5P_get_driver_info
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
 *		Raymond Lu
 *		Tuesday, Oct 23, 2001
 *		Changed the file access list design to the new generic 
 *		property list.
 *	
 *-------------------------------------------------------------------------
 */
void *
H5P_get_driver_info(H5P_genplist_t *plist)
{
    void	*ret_value=NULL;

    FUNC_ENTER(H5P_get_driver_info, NULL);

    /* Get the current driver info */
    if( TRUE == H5P_isa_class(plist->plist_id, H5P_FILE_ACCESS) ) {
        if(H5P_get(plist, H5F_ACS_FILE_DRV_INFO_NAME, &ret_value) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,NULL,"can't get driver info");
    } else if( TRUE == H5P_isa_class(plist->plist_id, H5P_DATASET_XFER) ) {
        if(H5P_get(plist, H5D_XFER_VFL_INFO_NAME, &ret_value)<0)
            HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, NULL, "Can't retrieve VFL driver ID");
    } else {
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access or data transfer property list");
    }
    
done:
    FUNC_LEAVE(ret_value);
} /* end H5P_get_driver_info() */


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
 *		Raymond Lu
 *		Tuesday, Oct 23, 2001
 *		Changed the file access list design to the new generic 
 *		property list.
 *	
 *-------------------------------------------------------------------------
 */
void *
H5Pget_driver_info(hid_t plist_id)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    void		*ret_value=NULL;

    FUNC_ENTER(H5Pget_driver_info, NULL);

    if(H5I_GENPROP_LST != H5I_get_type(plist_id) || NULL == (plist = H5I_object(plist_id)))
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a property list");

    if((ret_value=H5P_get_driver_info(plist))==NULL)
        HRETURN_ERROR(H5E_PLIST,H5E_CANTGET,NULL,"can't get driver info");
    
    FUNC_LEAVE(ret_value);
} /* end H5Pget_driver_info() */

#ifdef H5_WANT_H5_V1_4_COMPAT

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
 *		Raymond Lu 
 *		Tuesday, Oct 23, 2001
 *		Changed the file access list to the new generic property list.
 *	
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_cache(hid_t plist_id, int mdc_nelmts,
	     int _rdcc_nelmts, size_t rdcc_nbytes, double rdcc_w0)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    size_t rdcc_nelmts=(size_t)_rdcc_nelmts;    /* Work around variable changing size */
    herr_t ret_value=SUCCEED;   /* return value */
    
    FUNC_ENTER (H5Pset_cache, FAIL);
    H5TRACE5("e","iIszzd",plist_id,mdc_nelmts,rdcc_nelmts,rdcc_nbytes,rdcc_w0);

    /* Check arguments */
    if(TRUE != H5P_isa_class(plist_id, H5P_FILE_ACCESS) )
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list");
    if (mdc_nelmts<0)
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "meta data cache size must be non-negative");
    if (rdcc_w0<0.0 || rdcc_w0>1.0)
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "raw data cache w0 value must be between 0.0 and 1.0 inclusive");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Set sizes */
    if(H5P_set(plist, H5F_ACS_META_CACHE_SIZE_NAME, &mdc_nelmts) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET,FAIL, "can't set meta data cache size");
    if(H5P_set(plist, H5F_ACS_DATA_CACHE_ELMT_SIZE_NAME, &rdcc_nelmts) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET,FAIL, "can't set data cache element size");
    if(H5P_set(plist, H5F_ACS_DATA_CACHE_BYTE_SIZE_NAME, &rdcc_nbytes) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET,FAIL, "can't set data cache byte size");
    if(H5P_set(plist, H5F_ACS_PREEMPT_READ_CHUNKS_NAME, &rdcc_w0) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET,FAIL, "can't set preempt read chunks");

done:
    FUNC_LEAVE(ret_value);
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
 *		Raymond Lu
 *		Tuesday, Oct 23, 2001
 *		Changed the file access list to the new generic property 
 *		list.
 *	
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_cache(hid_t plist_id, int *mdc_nelmts,
	     int *_rdcc_nelmts, size_t *rdcc_nbytes, double *rdcc_w0)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    size_t rdcc_nelmts;         /* Work around variable changing size */
    herr_t ret_value=SUCCEED;   /* return value */
    
    FUNC_ENTER (H5Pget_cache, FAIL);
    H5TRACE5("e","i*Is*z*z*d",plist_id,mdc_nelmts,rdcc_nelmts,rdcc_nbytes,rdcc_w0);

    /* Check arguments */
    if(TRUE != H5P_isa_class(plist_id, H5P_FILE_ACCESS))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Get sizes */
    if (mdc_nelmts)
        if(H5P_get(plist, H5F_ACS_META_CACHE_SIZE_NAME, mdc_nelmts) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,FAIL, "can't get meta data cache size");
    if (_rdcc_nelmts) {
        if(H5P_get(plist, H5F_ACS_DATA_CACHE_ELMT_SIZE_NAME, &rdcc_nelmts) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,FAIL, "can't get data cache element size");
        *_rdcc_nelmts=rdcc_nelmts;
    } /* end if */
    if (rdcc_nbytes)
        if(H5P_get(plist, H5F_ACS_DATA_CACHE_BYTE_SIZE_NAME, rdcc_nbytes) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,FAIL, "can't get data cache byte size");
    if (rdcc_w0)
        if(H5P_get(plist, H5F_ACS_PREEMPT_READ_CHUNKS_NAME, rdcc_w0) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,FAIL, "can't get preempt read chunks");

done:
    FUNC_LEAVE(ret_value);
}

#else /* H5_WANT_H5_V1_4_COMPAT */

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
 *		Raymond Lu 
 *		Tuesday, Oct 23, 2001
 *		Changed the file access list to the new generic property list.
 *	
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_cache(hid_t plist_id, int mdc_nelmts,
	     size_t rdcc_nelmts, size_t rdcc_nbytes, double rdcc_w0)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */
    
    FUNC_ENTER (H5Pset_cache, FAIL);
    H5TRACE5("e","iIszzd",plist_id,mdc_nelmts,rdcc_nelmts,rdcc_nbytes,rdcc_w0);

    /* Check arguments */
    if(TRUE != H5P_isa_class(plist_id, H5P_FILE_ACCESS) )
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list");
    if (mdc_nelmts<0)
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "meta data cache size must be non-negative");
    if (rdcc_w0<0.0 || rdcc_w0>1.0)
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "raw data cache w0 value must be between 0.0 and 1.0 inclusive");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Set sizes */
    if(H5P_set(plist, H5F_ACS_META_CACHE_SIZE_NAME, &mdc_nelmts) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET,FAIL, "can't set meta data cache size");
    if(H5P_set(plist, H5F_ACS_DATA_CACHE_ELMT_SIZE_NAME, &rdcc_nelmts) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET,FAIL, "can't set data cache element size");
    if(H5P_set(plist, H5F_ACS_DATA_CACHE_BYTE_SIZE_NAME, &rdcc_nbytes) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET,FAIL, "can't set data cache byte size");
    if(H5P_set(plist, H5F_ACS_PREEMPT_READ_CHUNKS_NAME, &rdcc_w0) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET,FAIL, "can't set preempt read chunks");

done:
    FUNC_LEAVE(ret_value);
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
 *		Raymond Lu
 *		Tuesday, Oct 23, 2001
 *		Changed the file access list to the new generic property 
 *		list.
 *	
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_cache(hid_t plist_id, int *mdc_nelmts,
	     size_t *rdcc_nelmts, size_t *rdcc_nbytes, double *rdcc_w0)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */
    
    FUNC_ENTER (H5Pget_cache, FAIL);
    H5TRACE5("e","i*Is*z*z*d",plist_id,mdc_nelmts,rdcc_nelmts,rdcc_nbytes,rdcc_w0);

    /* Check arguments */
    if(TRUE != H5P_isa_class(plist_id, H5P_FILE_ACCESS))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Get sizes */
    if (mdc_nelmts)
        if(H5P_get(plist, H5F_ACS_META_CACHE_SIZE_NAME, mdc_nelmts) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,FAIL, "can't get meta data cache size");
    if (rdcc_nelmts)
        if(H5P_get(plist, H5F_ACS_DATA_CACHE_ELMT_SIZE_NAME, rdcc_nelmts) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,FAIL, "can't get data cache element size");
    if (rdcc_nbytes)
        if(H5P_get(plist, H5F_ACS_DATA_CACHE_BYTE_SIZE_NAME, rdcc_nbytes) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,FAIL, "can't get data cache byte size");
    if (rdcc_w0)
        if(H5P_get(plist, H5F_ACS_PREEMPT_READ_CHUNKS_NAME, rdcc_w0) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,FAIL, "can't get preempt read chunks");

done:
    FUNC_LEAVE(ret_value);
}
#endif /* H5_WANT_H5_V1_4_COMPAT */


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
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER (H5Pset_buffer, FAIL);
    H5TRACE4("e","izxx",plist_id,size,tconv,bkg);

    /* Check arguments */
    if(TRUE != H5P_isa_class(plist_id, H5P_DATASET_XFER))
        HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list");
    if (size<=0)
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "buffer size must not be zero");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Update property list */
    if(H5P_set(plist, H5D_XFER_MAX_TEMP_BUF_NAME, &size)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTSET, FAIL, "Can't set transfer buffer size");
    if(H5P_set(plist, H5D_XFER_TCONV_BUF_NAME, &tconv)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTSET, FAIL, "Can't set transfer type conversion buffer");
    if(H5P_set(plist, H5D_XFER_BKGR_BUF_NAME, &bkg)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTSET, FAIL, "Can't set background type conversion buffer");

done:
    FUNC_LEAVE(ret_value);
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
    H5P_genplist_t *plist;      /* Property list pointer */
    size_t size;                /* Type conversion buffer size */
    size_t ret_value=0;         /* Type conversion buffer size */

    FUNC_ENTER (H5Pget_buffer, 0);
    H5TRACE3("z","ixx",plist_id,tconv,bkg);

    /* Check arguments */
    if (TRUE!=H5P_isa_class(plist_id,H5P_DATASET_XFER))
        HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, 0, "not a dataset transfer property list");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, 0, "can't find object for ID");

    /* Return values */
    if (tconv)
        if(H5P_get(plist, H5D_XFER_TCONV_BUF_NAME, tconv)<0)
            HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, 0, "Can't get transfer type conversion buffer");
    if (bkg)
        if(H5P_get(plist, H5D_XFER_BKGR_BUF_NAME, bkg)<0)
            HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, 0, "Can't get background type conversion buffer");

    /* Get the size */
    if(H5P_get(plist, H5D_XFER_MAX_TEMP_BUF_NAME, &size)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTSET, 0, "Can't set transfer buffer size");

    /* Set the return value */
    ret_value=size;

done:
    FUNC_LEAVE(ret_value);
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
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;

    FUNC_ENTER (H5Pset_hyper_cache, FAIL);
    H5TRACE3("e","iIuIu",plist_id,cache,limit);

    /* Check arguments */
    if (TRUE!=H5P_isa_class(plist_id,H5P_DATASET_XFER))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset tranxfer property list");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Update property list */
    cache = (cache>0) ? 1 : 0;
    if (H5P_set(plist,H5D_XFER_HYPER_CACHE_NAME,&cache)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to set value");
    if (H5P_set(plist,H5D_XFER_HYPER_CACHE_LIM_NAME,&limit)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to set value");

done:
    FUNC_LEAVE (ret_value);
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
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER (H5Pget_hyper_cache, FAIL);
    H5TRACE3("e","ixx",plist_id,cache,limit);

    /* Check arguments */
    if (TRUE!=H5P_isa_class(plist_id,H5P_DATASET_XFER))
        HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Return values */
    if (cache)
        if (H5P_get(plist,H5D_XFER_HYPER_CACHE_NAME,cache)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get value");
    if (limit)
        if (H5P_get(plist,H5D_XFER_HYPER_CACHE_LIM_NAME,limit)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get value");

done:
    FUNC_LEAVE(ret_value);
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
    H5T_bkg_t need_bkg;         /* Value for background buffer type */
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */
    
    FUNC_ENTER (H5Pset_preserve, FAIL);
    H5TRACE2("e","ib",plist_id,status);

    /* Check arguments */
    if (TRUE!=H5P_isa_class(plist_id,H5P_DATASET_XFER))
        HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Update property list */
    need_bkg = status ? H5T_BKG_YES : H5T_BKG_NO;
    if (H5P_set(plist,H5D_XFER_BKGR_BUF_TYPE_NAME,&need_bkg)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value");

done:
    FUNC_LEAVE(ret_value);
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
    H5T_bkg_t need_bkg;         /* Background value */
    H5P_genplist_t *plist;      /* Property list pointer */
    int ret_value;              /* return value */
    
    FUNC_ENTER (H5Pget_preserve, FAIL);
    H5TRACE1("Is","i",plist_id);

    /* Check arguments */
    if (TRUE!=H5P_isa_class(plist_id,H5P_DATASET_XFER))
        HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Get value */
    if (H5P_get(plist,H5D_XFER_BKGR_BUF_NAME,&need_bkg)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get value");

    /* Set return value */
    ret_value= need_bkg ? TRUE : FALSE;

done:
    FUNC_LEAVE(ret_value);
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
 *              Raymond Lu
 *              Tuesday, October 2, 2001
 *              Changed the way to check parameter and set property for  
 *              generic property list.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_filter(hid_t plist_id, H5Z_filter_t filter, unsigned int flags,
	       size_t cd_nelmts, const unsigned int cd_values[/*cd_nelmts*/])
{
    H5O_pline_t         pline;
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER (H5Pset_filter, FAIL);
    H5TRACE5("e","iZfIuz*[a3]Iu",plist_id,filter,flags,cd_nelmts,cd_values);

    /* Check arguments */
    if(TRUE != H5P_isa_class(plist_id, H5P_DATASET_CREATE))
        HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list");
   
    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    if(H5P_get(plist, H5D_CRT_DATA_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline");

    if (filter<0 || filter>H5Z_FILTER_MAX)
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "invalid filter identifier");
    if (flags & ~((unsigned)H5Z_FLAG_DEFMASK))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid flags");
    if (cd_nelmts>0 && !cd_values)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no client data values supplied");

    /* Do it */
    if(H5Z_append(&pline, filter, flags, cd_nelmts, cd_values) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTINIT, FAIL, "unable to add filter to pipeline");
    if(H5P_set(plist, H5D_CRT_DATA_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set pipeline");

done:
    FUNC_LEAVE(ret_value);
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
    H5O_pline_t         pline;
    H5P_genplist_t *plist;      /* Property list pointer */
    int ret_value;              /* return value */

    FUNC_ENTER(H5Pget_nfilters, FAIL);
    H5TRACE1("Is","i",plist_id);

    /* Check args */
    if(TRUE != H5P_isa_class(plist_id, H5P_DATASET_CREATE))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list");
   
    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Get value */
    if(H5P_get(plist, H5D_CRT_DATA_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline");

    /* Set return value */
    ret_value=(int)(pline.nfilters);

done:
    FUNC_LEAVE(ret_value);
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
 *              Raymond Lu
 *              Tuesday, October 2, 2001
 *              Changed the way to check paramter and set property for 
 *              generic property list. 
 *
 *-------------------------------------------------------------------------
 */
H5Z_filter_t
H5Pget_filter(hid_t plist_id, int idx, unsigned int *flags/*out*/,
	       size_t *cd_nelmts/*in_out*/, unsigned cd_values[]/*out*/,
	       size_t namelen, char name[]/*out*/)
{
    H5O_pline_t         pline;
    size_t		i;
    H5P_genplist_t *plist;      /* Property list pointer */
    H5Z_filter_t ret_value;     /* return value */
    
    FUNC_ENTER (H5Pget_filter, H5Z_FILTER_ERROR);
    H5TRACE7("Zf","iIsx*zxzx",plist_id,idx,flags,cd_nelmts,cd_values,namelen,
             name);
    
    /* Check arguments */
    if(TRUE != H5P_isa_class(plist_id, H5P_DATASET_CREATE))
        HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, H5Z_FILTER_ERROR, "not a dataset creation property list");
   
    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, H5Z_FILTER_ERROR, "can't find object for ID");

    /* Get pipeline info */
    if(H5P_get(plist, H5D_CRT_DATA_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, H5Z_FILTER_ERROR, "can't get pipeline");

    if (idx<0 || (size_t)idx>=pline.nfilters)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5Z_FILTER_ERROR, "filter number is invalid");

    if (cd_nelmts || cd_values) {
        if (cd_nelmts && *cd_nelmts>256)
            /*
             * It's likely that users forget to initialize this on input, so
             * we'll check that it has a reasonable value.  The actual number
             * is unimportant because the H5O layer will detect when a message
             * is too large.
             */
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5Z_FILTER_ERROR, "probable uninitialized *cd_nelmts argument");
        if (cd_nelmts && *cd_nelmts>0 && !cd_values)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, H5Z_FILTER_ERROR, "client data values not supplied");

        /*
         * If cd_nelmts is null but cd_values is non-null then just ignore
         * cd_values
         */
        if (!cd_nelmts)
            cd_values = NULL;
    }

    if (flags)
        *flags = pline.filter[idx].flags;
    if (cd_values) {
        for (i=0; i<pline.filter[idx].cd_nelmts && i<*cd_nelmts; i++)
            cd_values[i] = pline.filter[idx].cd_values[i];
    }
    if (cd_nelmts)
        *cd_nelmts = pline.filter[idx].cd_nelmts;

    if (namelen>0 && name) {
        const char *s = pline.filter[idx].name;

        if (!s) {
            H5Z_class_t *cls = H5Z_find(pline.filter[idx].id);

            if (cls)
                s = cls->name;
        }
        if (s)
            HDstrncpy(name, s, namelen);
        else
            name[0] = '\0';
    }
    
    /* Set return value */
    ret_value=pline.filter[idx].id;

done:
    FUNC_LEAVE(ret_value);
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
 *              Raymond Lu
 *              Tuesday, October 2, 2001
 *              Changed the way to check parameter and set property for 
 *              generic property list. 
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_deflate(hid_t plist_id, unsigned level)
{
    H5O_pline_t         pline;
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */
    
    FUNC_ENTER (H5Pset_deflate, FAIL);
    H5TRACE2("e","iIu",plist_id,level);

    /* Check arguments */
    if(TRUE != H5P_isa_class(plist_id, H5P_DATASET_CREATE))
        HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list");
    if (level>9)
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "invalid deflate level");
   
    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Add the filter */
    if(H5P_get(plist, H5D_CRT_DATA_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get pipeline");
    if (H5Z_append(&pline, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, 1, &level)<0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTINIT, FAIL, "unable to add deflate filter to pipeline");
    if(H5P_set(plist, H5D_CRT_DATA_PIPELINE_NAME, &pline) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTINIT, FAIL, "unable to set pipeline");

done:
    FUNC_LEAVE(ret_value);
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
    double btree_split_ratio[3];        /* B-tree node split ratios */
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER(H5Pget_btree_ratios, FAIL);
    H5TRACE4("e","ixxx",plist_id,left,middle,right);

    /* Check arguments */
    if (TRUE!=H5P_isa_class(plist_id,H5P_DATASET_XFER))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Get the split ratios */
    if (H5P_get(plist,H5D_XFER_BTREE_SPLIT_RATIO_NAME,&btree_split_ratio)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get value");

    /* Get values */
    if (left)
        *left = btree_split_ratio[0];
    if (middle)
        *middle = btree_split_ratio[1];
    if (right)
        *right = btree_split_ratio[2];

done:
    FUNC_LEAVE(ret_value);
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
    double split_ratio[3];        /* B-tree node split ratios */
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER(H5Pset_btree_ratios, FAIL);
    H5TRACE4("e","iddd",plist_id,left,middle,right);

    /* Check arguments */
    if (TRUE!=H5P_isa_class(plist_id,H5P_DATASET_XFER))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list");
    if (left<0.0 || left>1.0 || middle<0.0 || middle>1.0 ||
            right<0.0 || right>1.0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "split ratio must satisfy 0.0<=X<=1.0");
    
    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Set values */
    split_ratio[0] = left;
    split_ratio[1] = middle;
    split_ratio[2] = right;

    /* Set the split ratios */
    if (H5P_set(plist,H5D_XFER_BTREE_SPLIT_RATIO_NAME,&split_ratio)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value");

done:
    FUNC_LEAVE(ret_value);
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
 *              Raymond Lu
 *              Tuesday, October 2, 2001
 *              Changed the way to check parameter and set property for 
 *              generic property list. 
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fill_value(hid_t plist_id, hid_t type_id, const void *value)
{
    H5O_fill_t          fill;
    H5T_t		*type = NULL;
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */
    
    FUNC_ENTER(H5Pset_fill_value, FAIL);
    H5TRACE3("e","iix",plist_id,type_id,value);

    /* Check arguments */
    if(TRUE != H5P_isa_class(plist_id, H5P_DATASET_CREATE))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list");
   
    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    if(H5P_get(plist, H5D_CRT_FILL_VALUE_NAME, &fill) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get fill value");

    if (H5I_DATATYPE!=H5I_get_type(type_id) || NULL==(type=H5I_object(type_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    if (!value)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no fill value specified");

    /* Set the fill value */
    H5O_reset(H5O_FILL, &fill);
    if (NULL==(fill.type=H5T_copy(type, H5T_COPY_TRANSIENT)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to copy data type");
    fill.size = H5T_get_size(type);
    if (NULL==(fill.buf=H5MM_malloc(fill.size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "memory allocation failed for fill value");
    HDmemcpy(fill.buf, value, fill.size);

    if(H5P_set(plist, H5D_CRT_FILL_VALUE_NAME, &fill) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set fill value");

done:
    FUNC_LEAVE(ret_value);
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
 *              Raymond Lu
 *              Tuesday, October 2, 2001
 *              Changed the way to check parameter and get property for 
 *              generic property list.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_fill_value(hid_t plist_id, hid_t type_id, void *value/*out*/)
{
    H5O_fill_t          fill;
    H5T_t		*type = NULL;		/*data type		*/
    H5T_path_t		*tpath = NULL;		/*type conversion info	*/
    void		*buf = NULL;		/*conversion buffer	*/
    void		*bkg = NULL;		/*conversion buffer	*/
    hid_t		src_id = -1;		/*source data type id	*/
    herr_t		ret_value = FAIL;	/*return value		*/
    H5P_genplist_t *plist;      /* Property list pointer */
    
    FUNC_ENTER(H5Pget_fill_value, FAIL);
    H5TRACE3("e","iix",plist_id,type_id,value);

    /* Check arguments */
    if(TRUE != H5P_isa_class(plist_id, H5P_DATASET_CREATE))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation proprety list");
    if (H5I_DATATYPE!=H5I_get_type(type_id) || NULL==(type=H5I_object(type_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    if (!value)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no fill value output buffer");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /*
     * If no fill value is defined then return an error.  We can't even
     * return zero because we don't know the data type of the dataset and
     * data type conversion might not have resulted in zero.
     */
    if(H5P_get(plist, H5D_CRT_FILL_VALUE_NAME, &fill) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get fill value"); 
    if(NULL == fill.buf)
        HGOTO_ERROR(H5E_PLIST, H5E_NOTFOUND, FAIL, "no fill value defined");

    /*
     * Can we convert between the source and destination data types?
     */
    if(NULL==(tpath=H5T_path_find(fill.type, type, NULL, NULL)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to convert between src and dst data types");
    src_id = H5I_register(H5I_DATATYPE, H5T_copy (fill.type, H5T_COPY_TRANSIENT));
    if (src_id<0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to copy/register data type");

    /*
     * Data type conversions are always done in place, so we need a buffer
     * other than the fill value buffer that is large enough for both source
     * and destination.  The app-supplied buffer might do okay.
     */
    if (H5T_get_size(type)>=H5T_get_size(fill.type)) {    
        buf = value;
        if (tpath->cdata.need_bkg>=H5T_BKG_TEMP &&
                NULL==(bkg=H5MM_malloc(H5T_get_size(type))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for type conversion");
    } else {
        if (NULL==(buf=H5MM_malloc(H5T_get_size(fill.type))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for type conversion");
        if (tpath->cdata.need_bkg>=H5T_BKG_TEMP)
            bkg = value;
    }
    HDmemcpy(buf, fill.buf, H5T_get_size(fill.type));
        
    /* Do the conversion */
    if (H5T_convert(tpath, src_id, type_id, (hsize_t)1, 0, 0, buf, bkg, H5P_DEFAULT)<0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "data type conversion failed");
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
 *		Raymond Lu 
 * 		Tuesday, Oct 23, 2001
 *		Changed the file access list to the new generic property 
 *		list.
 *	
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_gc_references(hid_t plist_id, unsigned gc_ref)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER(H5Pset_gc_references, FAIL);
    H5TRACE2("e","iIu",plist_id,gc_ref);

    /* Check args */
    if(TRUE != H5P_isa_class(plist_id, H5P_FILE_ACCESS))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");   

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Set values */
    if(H5P_set(plist, H5F_ACS_GARBG_COLCT_REF_NAME, &gc_ref) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set garbage collect reference");

done:
    FUNC_LEAVE(ret_value);
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
 *		Raymond Lu
 *		Tuesday, Oct 23, 2001
 *		Changed the file access list to the new generic property 
 *		list.
 *	
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_gc_references(hid_t plist_id, unsigned *gc_ref/*out*/)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER(H5Pget_gc_references, FAIL);
    H5TRACE2("e","ix",plist_id,gc_ref);

    /* Check args */
    if(TRUE != H5P_isa_class(plist_id, H5P_FILE_ACCESS))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");  

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Get values */
    if (gc_ref)
        if(H5P_get(plist, H5F_ACS_GARBG_COLCT_REF_NAME, gc_ref) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get garbage collect reference");

done:
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5P_set_vlen_mem_manager
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
H5P_set_vlen_mem_manager(H5P_genplist_t *plist, H5MM_allocate_t alloc_func,
        void *alloc_info, H5MM_free_t free_func, void *free_info)
{
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER(H5P_set_vlen_mem_manager, FAIL);

    assert(plist);

    /* Update property list */
    if (H5P_set(plist,H5D_XFER_VLEN_ALLOC_NAME,&alloc_func)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value");
    if (H5P_set(plist,H5D_XFER_VLEN_ALLOC_INFO_NAME,&alloc_info)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value");
    if (H5P_set(plist,H5D_XFER_VLEN_FREE_NAME,&free_func)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value");
    if (H5P_set(plist,H5D_XFER_VLEN_FREE_INFO_NAME,&free_info)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value");

done:
    FUNC_LEAVE(ret_value);
} /* end H5P_set_vlen_mem_manager() */


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
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER(H5Pset_vlen_mem_manager, FAIL);
    H5TRACE5("e","ixxxx",plist_id,alloc_func,alloc_info,free_func,free_info);

    /* Check arguments */
    if(TRUE!=H5P_isa_class(plist_id,H5P_DATASET_XFER) || NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list");

    /* Update property list */
    if (H5P_set_vlen_mem_manager(plist,alloc_func,alloc_info,free_func,free_info)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set values");

done:
    FUNC_LEAVE(ret_value);
} /* end H5Pset_vlen_mem_manager() */


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
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER(H5Pget_vlen_mem_manager, FAIL);
    H5TRACE5("e","ixxxx",plist_id,alloc_func,alloc_info,free_func,free_info);

    /* Check arguments */
    if (TRUE!=H5P_isa_class(plist_id,H5P_DATASET_XFER))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    if(alloc_func!=NULL)
        if (H5P_get(plist,H5D_XFER_VLEN_ALLOC_NAME,alloc_func)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get value");
    if(alloc_info!=NULL)
        if (H5P_get(plist,H5D_XFER_VLEN_ALLOC_INFO_NAME,alloc_info)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get value");
    if(free_func!=NULL)
        if (H5P_get(plist,H5D_XFER_VLEN_FREE_NAME,free_func)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get value");
    if(free_info!=NULL)
        if (H5P_get(plist,H5D_XFER_VLEN_FREE_INFO_NAME,free_info)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get value");

done:
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_meta_block_size
 *
 * Purpose:	Sets the minimum size of metadata block allocations when
 *      the H5FD_FEAT_AGGREGATE_METADATA is set by a VFL driver.
 *      Each "raw" metadata block is allocated to be this size and then
 *      specific pieces of metadata (object headers, local heaps, B-trees, etc)
 *      are sub-allocated from this block.
 *      
 *		The default value is set to 2048 (bytes), indicating that metadata
 *      will be attempted to be bunched together in (at least) 2K blocks in
 *      the file.  Setting the value to 0 with this API function will
 *      turn off the metadata aggregation, even if the VFL driver attempts to
 *      use that strategy.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Friday, August 25, 2000
 *
 * Modifications:
 *
 *		Raymond Lu
 *		Tuesday, Oct 23, 2001
 *		Changed the file access list to the new generic property 
 *		list. 
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_meta_block_size(hid_t plist_id, size_t size)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER (H5Pset_meta_block_size, FAIL);
    H5TRACE2("e","iz",plist_id,size);

    /* Check args */
    if(TRUE != H5P_isa_class(plist_id, H5P_FILE_ACCESS))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");  

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Set values */
    if(H5P_set(plist, H5F_ACS_META_BLOCK_SIZE_NAME, &size) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set meta data block size");

done:
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_meta_block_size
 *
 * Purpose:	Returns the current settings for the metadata block allocation
 *      property from a file access property list.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Friday, August 29, 2000
 *
 * Modifications:
 *
 *		Raymond Lu 
 * 		Tuesday, Oct 23, 2001
 *		Changed the file access list to the new generic property 
 *		list.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_meta_block_size(hid_t plist_id, size_t *size/*out*/)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER (H5Pget_meta_block_size, FAIL);
    H5TRACE2("e","ix",plist_id,size);

    /* Check args */
    if(TRUE != H5P_isa_class(plist_id, H5P_FILE_ACCESS))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");  

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Get values */
    if (size)
        if(H5P_get(plist, H5F_ACS_META_BLOCK_SIZE_NAME, size) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get meta data block size");

done:
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_sieve_buf_size
 *
 * Purpose:	Sets the maximum size of the data seive buffer used for file
 *      drivers which are capable of using data sieving.  The data sieve
 *      buffer is used when performing I/O on datasets in the file.  Using a
 *      buffer which is large anough to hold several pieces of the dataset
 *      being read in for hyperslab selections boosts performance by quite a
 *      bit.
 *      
 *		The default value is set to 64KB, indicating that file I/O for raw data
 *      reads and writes will occur in at least 64KB blocks.
 *      Setting the value to 0 with this API function will turn off the
 *      data sieving, even if the VFL driver attempts to use that strategy.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, September 21, 2000
 *
 * Modifications:
 *
 *		Raymond Lu 
 * 		Tuesday, Oct 23, 2001
 *		Changed the file access list to the new generic property 
 *		list.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_sieve_buf_size(hid_t plist_id, size_t size)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */
    
    FUNC_ENTER (H5Pset_sieve_buf_size, FAIL);
    H5TRACE2("e","iz",plist_id,size);

    /* Check args */
    if(TRUE != H5P_isa_class(plist_id, H5P_FILE_ACCESS))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");  

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Set values */
    if(H5P_set(plist, H5F_ACS_SIEVE_BUF_SIZE_NAME, &size) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set sieve buffer size");

done:
    FUNC_LEAVE(ret_value);
} /* end H5Pset_sieve_buf_size() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_sieve_buf_size
 *
 * Purpose:	Returns the current settings for the data sieve buffer size
 *      property from a file access property list.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, September 21, 2000
 *
 * Modifications:
 *
 *		Raymond Lu 
 * 		Tuesday, Oct 23, 2001
 *		Changed the file access list to the new generic property 
 *		list.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_sieve_buf_size(hid_t plist_id, size_t *size/*out*/)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER (H5Pget_sieve_buf_size, FAIL);
    H5TRACE2("e","ix",plist_id,size);

    /* Check args */
    if(TRUE != H5P_isa_class(plist_id, H5P_FILE_ACCESS))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");  

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Get values */
    if (size)
        if(H5P_get(plist, H5F_ACS_SIEVE_BUF_SIZE_NAME, size) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get sieve buffer size");

done:
    FUNC_LEAVE(ret_value);
} /* end H5Pget_sieve_buf_size() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_hyper_vector_size
 *
 * Purpose:	Given a dataset transfer property list, set the number of
 *              "I/O vectors" (offset and length pairs) which are to be
 *              accumulated in memory before being issued to the lower levels
 *              of the library for reading or writing the actual data.
 *              Increasing the number should give better performance, but use
 *              more memory during hyperslab I/O.  The vector size must be
 *              greater than 1.
 *
 *		The default is to use 1024 vectors for I/O during hyperslab
 *              reading/writing.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Monday, July 9, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_hyper_vector_size(hid_t plist_id, size_t vector_size)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER (H5Pset_hyper_vector_size, FAIL);
    H5TRACE2("e","iz",plist_id,vector_size);

    /* Check arguments */
    if (TRUE!=H5P_isa_class(plist_id,H5P_DATASET_XFER))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list");
    if (vector_size<1)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "vector size too small");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Update property list */
    if (H5P_set(plist,H5D_XFER_HYPER_VECTOR_SIZE_NAME,&vector_size)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value");

done:
    FUNC_LEAVE(ret_value);
} /* end H5Pset_hyper_vector_size() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_hyper_vector_size
 *
 * Purpose:	Reads values previously set with H5Pset_hyper_vector_size().
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Monday, July 9, 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_hyper_vector_size(hid_t plist_id, size_t *vector_size/*out*/)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER (H5Pget_hyper_vector_size, FAIL);
    H5TRACE2("e","ix",plist_id,vector_size);

    /* Check arguments */
    if (TRUE!=H5P_isa_class(plist_id,H5P_DATASET_XFER))
        HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset transfer property list");

    /* Get the plist structure */
    if(NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Return values */
    if (vector_size)
        if (H5P_get(plist,H5D_XFER_HYPER_VECTOR_SIZE_NAME,vector_size)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get value");

done:
    FUNC_LEAVE(ret_value);
} /* end H5Pget_hyper_vector_size() */


/*--------------------------------------------------------------------------
 NAME
    H5P_dup_prop
 PURPOSE
    Internal routine to duplicate a property
 USAGE
    H5P_genprop_t *H5P_dup_prop(oprop)
        H5P_genprop_t *oprop;   IN: Pointer to property to copy
 RETURNS
    Returns a pointer to the newly created duplicate of a property on success,
        NULL on failure.
 DESCRIPTION
    Allocates memory and copies property information into a new property object.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static H5P_genprop_t *
H5P_dup_prop(H5P_genprop_t *oprop)
{
    H5P_genprop_t *prop=NULL;        /* Pointer to new property copied */
    H5P_genprop_t *ret_value=NULL;   /* Return value */

    FUNC_ENTER (H5P_dup_prop, NULL);

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
        assert(prop->size>0);
        if (NULL==(prop->value = H5MM_malloc (prop->size)))
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
        HDmemcpy(prop->value,oprop->value,prop->size);
    } /* end if */

    /* Duplicate default value, if it exists */
    if(oprop->def_value!=NULL) {
        assert(prop->size>0);
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
}   /* H5P_dup_prop() */


/*--------------------------------------------------------------------------
 NAME
    H5P_create_prop
 PURPOSE
    Internal routine to create a new property
 USAGE
    H5P_genprop_t *H5P_create_prop(name,size,def_value,prp_create,prp_set,
            prp_get,prp_delete,prp_close)
        const char *name;       IN: Name of property to register
        size_t size;            IN: Size of property in bytes
        void *def_value;        IN: Pointer to buffer containing default value
                                    for property in newly created property lists
        H5P_prp_create_func_t prp_create;   IN: Function pointer to property
                                    creation callback
        H5P_prp_set_func_t prp_set; IN: Function pointer to property set callback
        H5P_prp_get_func_t prp_get; IN: Function pointer to property get callback
        H5P_prp_delete_func_t prp_delete; IN: Function pointer to property delete callback
        H5P_prp_copy_func_t prp_copy; IN: Function pointer to property copy callback
        H5P_prp_close_func_t prp_close; IN: Function pointer to property close
                                    callback
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
H5P_create_prop(const char *name, size_t size, void *def_value, void *value,
    H5P_prp_create_func_t prp_create, H5P_prp_set_func_t prp_set,
    H5P_prp_get_func_t prp_get, H5P_prp_delete_func_t prp_delete,
    H5P_prp_copy_func_t prp_copy, H5P_prp_close_func_t prp_close)
{
    H5P_genprop_t *prop=NULL;        /* Pointer to new property copied */
    H5P_genprop_t *ret_value=NULL;   /* Return value */

    FUNC_ENTER (H5P_create_prop, NULL);

    assert(name);
    assert((size>0 && (def_value!=NULL || value!=NULL)) || (size==0));

    /* Allocate the new property */
    if (NULL==(prop = H5MM_malloc (sizeof(H5P_genprop_t))))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Set the property initial values */
    prop->xor_val = H5P_xor_name(name); /* Generate the XOR'd value for the name */
    prop->name = HDstrdup(name); /* Duplicate name */
    prop->size=size;

    /* Duplicate value, if it exists */
    if(value!=NULL) {
        if (NULL==(prop->value = H5MM_malloc (prop->size)))
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
        HDmemcpy(prop->value,value,prop->size);
    } /* end if */
    else
        prop->value=NULL;

    /* Duplicate default value, if it exists */
    if(def_value!=NULL) {
        if (NULL==(prop->def_value = H5MM_malloc (prop->size)))
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
        HDmemcpy(prop->def_value,def_value,prop->size);
    } /* end if */
    else
        prop->def_value=NULL;

    /* Set the function pointers */
    prop->create=prp_create;
    prop->set=prp_set;
    prop->get=prp_get;
    prop->del=prp_delete;
    prop->copy=prp_copy;
    prop->close=prp_close;

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
}   /* H5P_create_prop() */


/*--------------------------------------------------------------------------
 NAME
    H5P_add_prop
 PURPOSE
    Internal routine to insert a property into a property hash table
 USAGE
    herr_t H5P_add_prop(hash, hashsize, prop)
        H5P_gen_prop_t *hash[]; IN/OUT: Pointer to array of properties for hash table
        unsigned hashsize;         IN: Size of hash table
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
H5P_add_prop(H5P_genprop_t *hash[], unsigned hashsize, H5P_genprop_t *prop)
{
    unsigned loc;                  /* Hash table location */
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
        unsigned hashsize;         IN: Size of hash table
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
H5P_find_prop(H5P_genprop_t *hash[], unsigned hashsize, const char *name)
{
    H5P_genprop_t *ret_value;   /* Property pointer return value */
    unsigned loc;                  /* Hash table location */
    unsigned xor_val;              /* XOR'ed value of the name to look for */

    FUNC_ENTER (H5P_find_prop, NULL);

    assert(hash);
    assert(hashsize>0);
    assert(name);

    /* Get correct hash table location */
    loc=H5P_hash_name(name,hashsize);
    
    /* Get the XOR'ed value for the name to search for, to speed up comparisons */
    xor_val=H5P_xor_name(name);
    
    /* Locate property in list */
    ret_value=hash[loc];
    while(ret_value!=NULL) {
        /* Check for name matching */
        if(ret_value->xor_val==xor_val && HDstrcmp(ret_value->name,name)==0)
            break;
        
        /* Advance to the next property */
        ret_value=ret_value->next;
    } /* end while */

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
    herr_t H5P_free_all_prop(hash, hashsize, make_cb)
        H5P_gen_prop_t *hash[]; IN/OUT: Pointer to array of properties for hash table
        unsigned hashsize;         IN: Size of hash table
        unsigned make_cb;          IN: Whether to make property callbacks or not
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
H5P_free_all_prop(H5P_genprop_t *hash[], unsigned hashsize, unsigned make_cb)
{
    H5P_genprop_t *tprop, *next;/* Temporary pointer to properties */
    unsigned u;                    /* Local index variable */
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
            if(make_cb && tprop->close!=NULL)
                (tprop->close)(tprop->name,tprop->size,tprop->value);

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

        case H5P_MOD_INC_REF:        /* Increment the ID reference count*/
            pclass->ref_count++;
            break;

        case H5P_MOD_DEC_REF:        /* Decrement the ID reference count*/
            pclass->ref_count--;
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

        /* Free the class properties without making callbacks */
        H5P_free_all_prop(pclass->props,pclass->hashsize,0);

        H5MM_xfree(pclass);
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
        unsigned hashsize; IN: Number of buckets in hash table
        unsigned internal; IN: Whether this is an internal class or not
        H5P_cls_create_func_t;  IN: The callback function to call when each
                                    property list in this class is created.
        void *create_data;      IN: Pointer to user data to pass along to class
                                    creation callback.
        H5P_cls_copy_func_t;    IN: The callback function to call when each
                                    property list in this class is copied.
        void *copy_data;        IN: Pointer to user data to pass along to class
                                    copy callback.
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
static H5P_genclass_t *
H5P_create_class(H5P_genclass_t *par_class, const char *name, unsigned hashsize, unsigned internal,
    H5P_cls_create_func_t cls_create, void *create_data,
    H5P_cls_copy_func_t cls_copy, void *copy_data,
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
    pclass->ref_count = 1;  /* This is the first reference to the new class */
    pclass->internal = internal;
    pclass->deleted = 0;    /* Not deleted yet... :-) */

    /* Set callback functions and pass-along data */
    pclass->create_func = cls_create;
    pclass->create_data = create_data;
    pclass->copy_func = cls_copy;
    pclass->copy_data = copy_data;
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
        unsigned hashsize;     IN: Number of buckets in hash table
        H5P_cls_create_func_t cls_create;   IN: The callback function to call
                                    when each property list in this class is
                                    created.
        void *create_data;  IN: Pointer to user data to pass along to class
                                    creation callback.
        H5P_cls_copy_func_t cls_copy;   IN: The callback function to call
                                    when each property list in this class is
                                    copied.
        void *copy_data;  IN: Pointer to user data to pass along to class
                                    copy callback.
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
    H5P_cls_copy_func_t cls_copy, void *copy_data,
    H5P_cls_close_func_t cls_close, void *close_data
    )
{
    H5P_genclass_t	*par_class = NULL;  /* Pointer to the parent class */
    H5P_genclass_t	*pclass = NULL;     /* Property list class created */
    hid_t	ret_value = FAIL;           /* Return value		   */

    FUNC_ENTER(H5Pcreate_class, FAIL);

    /* Check arguments. */
    if (H5P_DEFAULT!=parent && (H5I_GENPROP_CLS!=H5I_get_type(parent)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list class");
    if (!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid class name");
    if (hashsize==0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "hashsize too small");
    if ((create_data!=NULL && cls_create==NULL)
        || (copy_data!=NULL && cls_copy==NULL)
        || (close_data!=NULL && cls_close==NULL))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "data specified, but no callback provided");
    
    /* Get the pointer to the parent class */
    if(parent==H5P_DEFAULT)
        par_class=NULL;
    else if (NULL == (par_class = H5I_object(parent)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't retrieve parent class");

    /* Create the new property list class */
    if (NULL==(pclass=H5P_create_class(par_class, name, hashsize, 0, cls_create, create_data, cls_copy, copy_data, cls_close, close_data)))
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
    H5P_create
 PURPOSE
    Internal routine to create a new property list of a property list class.
 USAGE
    H5P_genplist_t *H5P_create(class)
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
    H5P_c, the calling routine is responsible for getting an ID for
    the property list and calling the class 'create' callback (if one exists)
    and also setting the "class_init" flag.
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static H5P_genplist_t *H5P_create(H5P_genclass_t *pclass)
{
    H5P_genclass_t *tclass=NULL;        /* Temporary class pointer */
    H5P_genplist_t *plist=NULL;         /* New property list created */
    H5P_genplist_t *ret_value=NULL;     /* return value */
    H5P_genprop_t *tmp;                 /* Temporary pointer to parent class properties */
    H5P_genprop_t *pcopy;               /* Copy of property to insert into class */
    unsigned u;                            /* Local index variable */

    FUNC_ENTER (H5P_create, NULL);

    assert(pclass);

    /* 
     * Create new property list object
     */

    /* Allocate room for the property list & it's hash table of properties */
    if (NULL==(plist = H5MM_calloc (sizeof(H5P_genplist_t)+((pclass->hashsize-1)*sizeof(H5P_genprop_t *)))))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,"memory allocation failed");

    /* Set class state */
    plist->pclass = pclass;
    plist->nprops = 0;      /* Initially the plist has the same number of properties as the class */
    plist->class_init = 0;  /* Initially, wait until the class callback finishes to set */

    /*
     * Copy class properties (up through list of parent classes also),
     * initialize each with default value & make property 'create' callback.
     */
    tclass=pclass;
    while(tclass!=NULL) {
        if(tclass->nprops>0) {
            /* Walk through the hash table */
            for(u=0; u<tclass->hashsize; u++) {
                tmp=tclass->props[u];
                /* Walk through the list of properties at each hash location */
                while(tmp!=NULL) {
                    /* Check for property already existing in list */
                    if(H5P_find_prop(plist->props,tclass->hashsize,tmp->name)==NULL) {
                        /* Make a copy of the class's property */
                        if((pcopy=H5P_dup_prop(tmp))==NULL)
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
                            if((pcopy->create)(pcopy->name,pcopy->size,pcopy->value)<0) {
                                H5P_free_prop(pcopy);
                                HGOTO_ERROR (H5E_PLIST, H5E_CANTINIT, NULL,"Can't initialize property");
                            } /* end if */
                        } /* end if */

                        /* Insert the initialized property into the property list */
                        if(H5P_add_prop(plist->props,tclass->hashsize,pcopy)<0)
                            HGOTO_ERROR (H5E_PLIST, H5E_CANTINSERT, NULL,"Can't insert property into class");

                        /* Increment the number of properties in list */
                        plist->nprops++;
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
            H5P_free_all_prop(plist->props,pclass->hashsize,1);

            /* Decrement the number of property lists derived from the class */
            pclass->plists--;
        } /* end if */
    } /* end if */

    FUNC_LEAVE (ret_value);
}   /* H5P_create() */


/*--------------------------------------------------------------------------
 NAME
    H5P_create_id
 PURPOSE
    Internal routine to create a new property list of a property list class.
 USAGE
    hid_t H5P_create_id(pclass)
        H5P_genclass_t *pclass;       IN: Property list class create list from
 RETURNS
    Returns a valid property list ID on success, FAIL on failure.
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
hid_t H5P_create_id(H5P_genclass_t *pclass)
{
    H5P_genplist_t	*plist=NULL;    /* Property list created */
    hid_t plist_id=FAIL;       /* Property list ID */
    hid_t ret_value=FAIL;      /* return value */

    FUNC_ENTER (H5P_create_id, FAIL);

    assert(pclass);

    /* Create the new property list */
    if ((plist=H5P_create(pclass))==NULL)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTCREATE, FAIL, "unable to create property list");

    /* Get an atom for the property list */
    if ((plist_id = H5I_register(H5I_GENPROP_LST, plist))<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to atomize property list");

    /* Save the property list ID in the property list struct, for use in the property class's 'close' callback */
    plist->plist_id=plist_id;

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
        H5P_close(plist);

    FUNC_LEAVE (ret_value);
}   /* H5P_create_id() */


/*--------------------------------------------------------------------------
 NAME
    H5Pcreate
 PURPOSE
    Routine to create a new property list of a property list class.
 USAGE
    hid_t H5Pcreate(cls_id)
        hid_t cls_id;       IN: Property list class create list from
 RETURNS
    Returns a valid property list ID on success, FAIL on failure.
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
hid_t H5Pcreate(hid_t cls_id)
{
    H5P_genclass_t	*pclass;   /* Property list class to modify */
    hid_t ret_value=FAIL;      /* return value */

    FUNC_ENTER (H5Pcreate, FAIL);

    /* Check arguments. */
    if (H5I_GENPROP_CLS != H5I_get_type(cls_id) || NULL == (pclass = H5I_object(cls_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list class");

    /* Create the new property list */
    if((ret_value=H5P_create_id(pclass))<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTCREATE, FAIL, "unable to create property list");
done:
    FUNC_LEAVE (ret_value);
}   /* H5Pcreate() */


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
        H5P_prp_delete_func_t prp_delete; IN: Function pointer to property delete callback
        H5P_prp_copy_func_t prp_copy; IN: Function pointer to property copy callback
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
        void *new_value;    IN/OUT: The value being set for the property.
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
        void *value;        IN/OUT: The value being retrieved for the property.
    The 'get' routine may modify the value to be retrieved and those changes
    will be returned to the calling function.  If the 'get' routine returns a
    negative value, the property value is returned and the property list get
    routine returns an error value.

        The 'delete' callback is called when a property is deleted from a
    property list.  H5P_prp_del_func_t is defined as:
        typedef herr_t (*H5P_prp_del_func_t)(hid_t prop_id, const char *name,
            void *value);
    where the parameters to the callback function are:
        hid_t prop_id;      IN: The ID of the property list the property is deleted from.
        const char *name;   IN: The name of the property being deleted.
        void *value;        IN/OUT: The value of the property being deleted.
    The 'delete' routine may modify the value passed in, but the value is not
    used by the library when the 'delete' routine returns.  If the
    'delete' routine returns a negative value, the property list deletion
    routine returns an error value but the property is still deleted.

        The 'copy' callback is called when a property list with this
    property is copied.  H5P_prp_copy_func_t is defined as:
        typedef herr_t (*H5P_prp_copy_func_t)(const char *name, void *value);
    where the parameters to the callback function are:
        const char *name;   IN: The name of the property being closed.
        void *value;        IN: The value of the property being closed.
    The 'copy' routine may modify the value to be copied and those changes will be
    stored as the value of the property.  If the 'copy' routine returns a
    negative value, the new property value is not copied into the property and
    the property list copy routine returns an error value.

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
herr_t H5P_register(H5P_genclass_t *pclass, const char *name, size_t size,
    void *def_value, H5P_prp_create_func_t prp_create, H5P_prp_set_func_t prp_set,
    H5P_prp_get_func_t prp_get, H5P_prp_delete_func_t prp_delete,
    H5P_prp_copy_func_t prp_copy, H5P_prp_close_func_t prp_close)
{
    H5P_genclass_t *new_class; /* New class pointer */
    H5P_genprop_t *tmp_prop;   /* Temporary property pointer */
    H5P_genprop_t *new_prop=NULL;   /* Temporary property pointer */
    H5P_genprop_t *pcopy;      /* Property copy */
    unsigned u;                   /* Local index variable */
    herr_t ret_value=FAIL;     /* return value */

    FUNC_ENTER (H5P_register, FAIL);

    assert(pclass);
    assert(name);
    assert((size>0 && def_value!=NULL) || (size==0));

    /* Check for duplicate named properties */
    if((tmp_prop=H5P_find_prop(pclass->props,pclass->hashsize,name))!=NULL)
        HGOTO_ERROR(H5E_PLIST, H5E_EXISTS, FAIL, "property already exists");

    /* Check if class needs to be split because property lists or classes have
     *  been created since the last modification was made to the class.
     */
    if(pclass->plists>0 || pclass->classes>0) {
        if((new_class=H5P_create_class(pclass->parent,pclass->name,pclass->hashsize,
                pclass->internal,pclass->create_func,pclass->create_data,
                pclass->copy_func,pclass->copy_data,
                pclass->close_func,pclass->close_data))==NULL)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTCOPY, FAIL, "can't copy class");

        /* Walk through the hash table of the old class and copy properties */
        for(u=0; u<pclass->hashsize; u++) {
            tmp_prop=pclass->props[u];
            while(tmp_prop!=NULL) {
                /* Make a copy of the class's property */
                if((pcopy=H5P_dup_prop(tmp_prop))==NULL)
                    HGOTO_ERROR (H5E_PLIST, H5E_CANTCOPY, FAIL,"Can't copy property");

                /* Insert the property into the new property class */
                if(H5P_add_prop(new_class->props,pclass->hashsize,pcopy)<0)
                    HGOTO_ERROR (H5E_PLIST, H5E_CANTINSERT, FAIL,"Can't insert property into class");

                /* Go to next registered property in class */
                tmp_prop=tmp_prop->next;
            } /* end while */
        } /* end for */

        /* Use the new class instead of the old one */
        pclass=new_class;
    } /* end if */

    /* Create property object from parameters */
    if((new_prop=H5P_create_prop(name,size,def_value,NULL,prp_create,prp_set,prp_get,prp_delete,prp_copy,prp_close))==NULL)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTCREATE, FAIL,"Can't create property");

    /* Insert property into property list class */
    if(H5P_add_prop(pclass->props,pclass->hashsize,new_prop)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTINSERT, FAIL,"Can't insert property into class");

    /* Increment property count for class */
    pclass->nprops++;

    /* Set return value */
    ret_value=SUCCEED;

done:
    if(ret_value==FAIL) {
        if(new_prop!=NULL) {
            if(new_prop->name!=NULL)
                H5MM_xfree(new_prop->name);
            if(new_prop->value!=NULL)
                H5MM_xfree(new_prop->value);
            if(new_prop->def_value!=NULL)
                H5MM_xfree(new_prop->def_value);
            H5MM_xfree(new_prop);
        } /* end if */
    }  /* end if */
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
        H5P_prp_delete_func_t prp_delete; IN: Function pointer to property delete callback
        H5P_prp_copy_func_t prp_copy; IN: Function pointer to property copy callback
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

        The 'delete' callback is called when a property is deleted from a
    property list.  H5P_prp_del_func_t is defined as:
        typedef herr_t (*H5P_prp_del_func_t)(hid_t prop_id, const char *name,
            void *value);
    where the parameters to the callback function are:
        hid_t prop_id;      IN: The ID of the property list the property is deleted from.
        const char *name;   IN: The name of the property being deleted.
        void *value;        IN/OUT: The value of the property being deleted.
    The 'delete' routine may modify the value passed in, but the value is not
    used by the library when the 'delete' routine returns.  If the
    'delete' routine returns a negative value, the property list deletion
    routine returns an error value but the property is still deleted.

        The 'copy' callback is called when a property list with this
    property is copied.  H5P_prp_copy_func_t is defined as:
        typedef herr_t (*H5P_prp_copy_func_t)(const char *name, void *value);
    where the parameters to the callback function are:
        const char *name;   IN: The name of the property being closed.
        void *value;        IN: The value of the property being closed.
    The 'copy' routine may modify the value to be copied and those changes will be
    stored as the value of the property.  If the 'copy' routine returns a
    negative value, the new property value is not copied into the property and
    the property list copy routine returns an error value.

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
    H5P_prp_get_func_t prp_get, H5P_prp_delete_func_t prp_delete,
    H5P_prp_copy_func_t prp_copy, H5P_prp_close_func_t prp_close)
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
    if ((ret_value=H5P_register(pclass,name,size,def_value,prp_create,prp_set,prp_get,prp_delete,prp_copy,prp_close))<0)
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
        H5P_prp_delete_func_t prp_delete; IN: Function pointer to property delete callback
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

        The 'delete' callback is called when a property is deleted from a
    property list.  H5P_prp_del_func_t is defined as:
        typedef herr_t (*H5P_prp_del_func_t)(hid_t prop_id, const char *name,
            void *value);
    where the parameters to the callback function are:
        hid_t prop_id;      IN: The ID of the property list the property is deleted from.
        const char *name;   IN: The name of the property being deleted.
        void *value;        IN/OUT: The value of the property being deleted.
    The 'delete' routine may modify the value passed in, but the value is not
    used by the library when the 'delete' routine returns.  If the
    'delete' routine returns a negative value, the property list deletion
    routine returns an error value but the property is still deleted.

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
static herr_t H5P_insert(H5P_genplist_t *plist, const char *name, size_t size,
    void *value, H5P_prp_set_func_t prp_set, H5P_prp_get_func_t prp_get,
    H5P_prp_delete_func_t prp_delete, H5P_prp_copy_func_t prp_copy, 
    H5P_prp_close_func_t prp_close)
{
    H5P_genprop_t *new_prop=NULL;   /* Temporary property pointer */
    herr_t ret_value=FAIL;     /* return value */

    FUNC_ENTER (H5P_insert, FAIL);

    assert(plist);
    assert(name);
    assert((size>0 && value!=NULL) || (size==0));

    /* Check for duplicate named properties */
    if(H5P_find_prop(plist->props,plist->pclass->hashsize,name)!=NULL)
        HGOTO_ERROR(H5E_PLIST, H5E_EXISTS, FAIL, "property already exists");

    /* Create property object from parameters */
    if((new_prop=H5P_create_prop(name,size,NULL,value,NULL,prp_set,prp_get,prp_delete,prp_copy,prp_close))==NULL)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTCREATE, FAIL,"Can't create property");

    /* Insert property into property list class */
    if(H5P_add_prop(plist->props,plist->pclass->hashsize,new_prop)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTINSERT, FAIL,"Can't insert property into class");

    /* Increment property count for class */
    plist->nprops++;

    /* Set return value */
    ret_value=SUCCEED;

done:
    if(ret_value==FAIL) {
        if(new_prop!=NULL) {
            if(new_prop->name!=NULL)
                H5MM_xfree(new_prop->name);
            if(new_prop->value!=NULL)
                H5MM_xfree(new_prop->value);
            if(new_prop->def_value!=NULL)
                H5MM_xfree(new_prop->def_value);
            H5MM_xfree(new_prop);
        } /* end if */
    }  /* end if */
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
        H5P_prp_delete_func_t prp_delete; IN: Function pointer to property delete callback
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

        The 'delete' callback is called when a property is deleted from a
    property list.  H5P_prp_del_func_t is defined as:
        typedef herr_t (*H5P_prp_del_func_t)(hid_t prop_id, const char *name,
            void *value);
    where the parameters to the callback function are:
        hid_t prop_id;      IN: The ID of the property list the property is deleted from.
        const char *name;   IN: The name of the property being deleted.
        void *value;        IN/OUT: The value of the property being deleted.
    The 'delete' routine may modify the value passed in, but the value is not
    used by the library when the 'delete' routine returns.  If the
    'delete' routine returns a negative value, the property list deletion
    routine returns an error value but the property is still deleted.

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
    H5P_prp_delete_func_t prp_delete, H5P_prp_copy_func_t prp_copy, 
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
    if ((ret_value=H5P_insert(plist,name,size,value,prp_set,prp_get,prp_delete,prp_copy,prp_close))<0)
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
herr_t H5P_set(H5P_genplist_t *plist, const char *name, const void *value)
{
    H5P_genprop_t *prop;        /* Temporary property pointer */
    herr_t ret_value=FAIL;      /* return value */

    FUNC_ENTER (H5P_set, FAIL);

    assert(plist);
    assert(name);
    assert(value);

    /* Find property */
    if((prop=H5P_find_prop(plist->props,plist->pclass->hashsize,name))==NULL)
        HGOTO_ERROR(H5E_PLIST, H5E_NOTFOUND, FAIL, "property doesn't exist");

    /* Check for property size >0 */
    if(prop->size==0)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "property has zero size");

    /* Make a copy of the value and pass to 'set' callback */
    if(prop->set!=NULL) {
        void *tmp_value;            /* Temporary value for property */

        /* Make a copy of the current value, in case the callback fails */
        if (NULL==(tmp_value=H5MM_malloc(prop->size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed temporary property value");
        HDmemcpy(tmp_value,value,prop->size);

        /* Call user's callback */
        if((*(prop->set))(plist->plist_id,name,prop->size,tmp_value)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "can't set property value");

        /* Copy new [possibly unchanged] value into property value */
        HDmemcpy(prop->value,tmp_value,prop->size);

        /* Free the temporary value buffer */
        H5MM_xfree(tmp_value);
    } /* end if */
    /* No 'set' callback, just copy value */
    else
        HDmemcpy(prop->value,value,prop->size);

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
    H5P_genplist_t *plist;      /* Property list to modify */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER (H5Pset, FAIL);

    /* Check arguments. */
    if (H5I_GENPROP_LST != H5I_get_type(plist_id) || NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
    if (!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid property name");
    if (value==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalied property value");

    /* Go set the value */
    if(H5P_set(plist,name,value)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to set value in plist");

done:
    FUNC_LEAVE (ret_value);
}   /* H5Pset() */


/*--------------------------------------------------------------------------
 NAME
    H5P_exist_plist
 PURPOSE
    Internal routine to query the existance of a property in a property list.
 USAGE
    herr_t H5P_exist_plist(plist, name)
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
static htri_t H5P_exist_plist(H5P_genplist_t *plist, const char *name)
{
    htri_t ret_value=FAIL;     /* return value */

    FUNC_ENTER (H5P_exist_plist, FAIL);

    assert(plist);
    assert(name);

    /* Check for property in property list */
    if(H5P_find_prop(plist->props,plist->pclass->hashsize,name)==NULL)
        ret_value=0;
    else
        ret_value=1;

    FUNC_LEAVE (ret_value);
}   /* H5P_exist_plist() */


/*--------------------------------------------------------------------------
 NAME
    H5P_exist_pclass
 PURPOSE
    Internal routine to query the existance of a property in a property class.
 USAGE
    herr_t H5P_exist_pclass(pclass, name)
        H5P_genclass_t *pclass;  IN: Property class to check
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
static htri_t H5P_exist_pclass(H5P_genclass_t *pclass, const char *name)
{
    htri_t ret_value=FAIL;     /* return value */

    FUNC_ENTER (H5P_exist_pclass, FAIL);

    assert(pclass);
    assert(name);

    /* Check for property in property list */
    if(H5P_find_prop(pclass->props,pclass->hashsize,name)==NULL)
        ret_value=0;
    else
        ret_value=1;

    FUNC_LEAVE (ret_value);
}   /* H5P_exist_pclass() */


/*--------------------------------------------------------------------------
 NAME
    H5Pexist
 PURPOSE
    Routine to query the existance of a property in a property object.
 USAGE
    htri_t H5P_exist(id, name)
        hid_t id;           IN: Property object ID to check
        const char *name;   IN: Name of property to check for
 RETURNS
    Success: Positive if the property exists in the property object, zero
            if the property does not exist.
    Failure: negative value
 DESCRIPTION
        This routine checks if a property exists within a property list or
    class.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
htri_t H5Pexist(hid_t id, const char *name)
{
    H5P_genplist_t	*plist;    /* Property list to query */
    H5P_genclass_t	*pclass;   /* Property class to query */
    htri_t ret_value=FAIL;     /* return value */

    FUNC_ENTER (H5Pexist, FAIL);

    /* Check arguments. */
    if (H5I_GENPROP_LST != H5I_get_type(id) && H5I_GENPROP_CLS != H5I_get_type(id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property object");
    if (!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid property name");

    /* Check for the existance of the property in the list or class */
    if(H5I_GENPROP_LST == H5I_get_type(id)) {
        if (NULL == (plist = H5I_object(id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if ((ret_value=H5P_exist_plist(plist,name))<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "property does not exist in list");
    } /* end if */
    else 
        if(H5I_GENPROP_CLS == H5I_get_type(id)) {
            if (NULL == (pclass = H5I_object(id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property class");
            if ((ret_value=H5P_exist_pclass(pclass,name))<0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "property does not exist in class");
        } /* end if */
        else
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property object");

done:
    FUNC_LEAVE (ret_value);
}   /* H5Pexist() */


/*--------------------------------------------------------------------------
 NAME
    H5P_get_size_plist
 PURPOSE
    Internal routine to query the size of a property in a property list.
 USAGE
    herr_t H5P_get_size_plist(plist, name)
        H5P_genplist_t *plist;  IN: Property list to check
        const char *name;       IN: Name of property to query
        size_t *size;           OUT: Size of property
 RETURNS
    Success: non-negative value
    Failure: negative value
 DESCRIPTION
        This routine retrieves the size of a property's value in bytes.  Zero-
    sized properties are allowed and return a value of 0.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t H5P_get_size_plist(H5P_genplist_t *plist, const char *name, size_t *size)
{
    H5P_genprop_t *prop;        /* Temporary property pointer */
    herr_t ret_value=SUCCEED;      /* return value */

    FUNC_ENTER (H5P_get_size_plist, FAIL);

    assert(plist);
    assert(name);
    assert(size);

    /* Find property */
    if((prop=H5P_find_prop(plist->props,plist->pclass->hashsize,name))==NULL)
        HGOTO_ERROR(H5E_PLIST, H5E_NOTFOUND, FAIL, "property doesn't exist");

    /* Get property size */
    *size=prop->size;

done:
    FUNC_LEAVE (ret_value);
}   /* H5P_get_size_plist() */


/*--------------------------------------------------------------------------
 NAME
    H5P_get_size_pclass
 PURPOSE
    Internal routine to query the size of a property in a property class.
 USAGE
    herr_t H5P_get_size_pclass(pclass, name)
        H5P_genclass_t *pclass; IN: Property class to check
        const char *name;       IN: Name of property to query
        size_t *size;           OUT: Size of property
 RETURNS
    Success: non-negative value
    Failure: negative value
 DESCRIPTION
        This routine retrieves the size of a property's value in bytes.  Zero-
    sized properties are allowed and return a value of 0.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t H5P_get_size_pclass(H5P_genclass_t *pclass, const char *name, size_t *size)
{
    H5P_genprop_t *prop;        /* Temporary property pointer */
    herr_t ret_value=SUCCEED;      /* return value */

    FUNC_ENTER (H5P_get_size_pclass, FAIL);

    assert(pclass);
    assert(name);
    assert(size);

    /* Find property */
    if((prop=H5P_find_prop(pclass->props,pclass->hashsize,name))==NULL)
        HGOTO_ERROR(H5E_PLIST, H5E_NOTFOUND, FAIL, "property doesn't exist");

    /* Get property size */
    *size=prop->size;

done:
    FUNC_LEAVE (ret_value);
}   /* H5P_get_size_pclass() */


/*--------------------------------------------------------------------------
 NAME
    H5Pget_size
 PURPOSE
    Routine to query the size of a property in a property list or class.
 USAGE
    herr_t H5Pget_size(id, name)
        hid_t id;               IN: ID of property list or class to check
        const char *name;       IN: Name of property to query
        size_t *size;           OUT: Size of property
 RETURNS
    Success: non-negative value
    Failure: negative value
 DESCRIPTION
        This routine retrieves the size of a property's value in bytes.  Zero-
    sized properties are allowed and return a value of 0.  This function works
    for both property lists and classes.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5Pget_size(hid_t id, const char *name, size_t *size)
{
    H5P_genclass_t	*pclass;   /* Property class to query */
    H5P_genplist_t	*plist;    /* Property list to query */
    herr_t ret_value=FAIL;     /* return value */

    FUNC_ENTER (H5Pget_size, FAIL);

    /* Check arguments. */
    if (H5I_GENPROP_LST != H5I_get_type(id) && H5I_GENPROP_CLS != H5I_get_type(id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property object");
    if (!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid property name");
    if (size==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid property size");

    if (H5I_GENPROP_LST == H5I_get_type(id)) {
        if (NULL == (plist = H5I_object(id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

        /* Check the property size */
        if ((ret_value=H5P_get_size_plist(plist,name,size))<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to query size in plist");
    } /* end if */
    else 
        if (H5I_GENPROP_CLS == H5I_get_type(id)) {
            if (NULL == (pclass = H5I_object(id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

            /* Check the property size */
            if ((ret_value=H5P_get_size_pclass(pclass,name,size))<0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to query size in plist");
        } /* end if */
        else 
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property object");

done:
    FUNC_LEAVE (ret_value);
}   /* H5Pget_size() */


/*--------------------------------------------------------------------------
 NAME
    H5P_get_class
 PURPOSE
    Internal routine to query the class of a generic property list
 USAGE
    H5P_genclass_t *H5P_get_class(plist)
        H5P_genplist_t *plist;    IN: Property list to check
 RETURNS
    Success: Pointer to the class for a property list
    Failure: NULL
 DESCRIPTION
    This routine retrieves a pointer to the class for a property list.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static H5P_genclass_t *H5P_get_class(H5P_genplist_t *plist)
{
    H5P_genclass_t *ret_value=NULL;      /* return value */

    FUNC_ENTER (H5P_get_class, NULL);

    assert(plist);

    /* Get property size */
    ret_value=plist->pclass;

    FUNC_LEAVE (ret_value);
}   /* H5P_get_class() */


/*--------------------------------------------------------------------------
 NAME
    H5Pget_class
 PURPOSE
    Routine to query the class of a generic property list
 USAGE
    hid_t H5Pget_class(plist_id)
        hid_t plist_id;         IN: Property list to query
 RETURNS
    Success: ID of class object
    Failure: negative
 DESCRIPTION
    This routine retrieves the class of a property list.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    Change the name of this function to H5Pget_class (and remove old H5Pget_class)
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hid_t H5Pget_class(hid_t plist_id)
{
    H5P_genplist_t	*plist;         /* Property list to query */
    H5P_genclass_t	*pclass=NULL;   /* Property list class */
    hid_t ret_value=FAIL;           /* return value */

    FUNC_ENTER (H5Pget_class, FAIL);

    /* Check arguments. */
    if (H5I_GENPROP_LST != H5I_get_type(plist_id) || NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

    /* Retrieve the property list class */
    if ((pclass=H5P_get_class(plist))==NULL)
        HGOTO_ERROR(H5E_PLIST, H5E_NOTFOUND, FAIL, "unable to query class of property list");

    /* Increment the outstanding references to the class object */
    if(H5P_access_class(pclass,H5P_MOD_INC_REF)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTINIT, FAIL,"Can't increment class ID ref count");

    /* Get an atom for the class */
    if ((ret_value = H5I_register(H5I_GENPROP_CLS, pclass))<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to atomize property list class");

done:
    if (ret_value<0 && pclass)
        H5P_close_class(pclass);

    FUNC_LEAVE (ret_value);
}   /* H5Pget_class() */


/*--------------------------------------------------------------------------
 NAME
    H5P_get_nprops_plist
 PURPOSE
    Internal routine to query the number of properties in a property list
 USAGE
    herr_t H5P_get_nprops_plist(plist, nprops)
        H5P_genplist_t *plist;  IN: Property list to check
        size_t *nprops;         OUT: Number of properties in the property list
 RETURNS
    Success: non-negative value
    Failure: negative value
 DESCRIPTION
        This routine retrieves the number of a properties in a property list.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t H5P_get_nprops_plist(H5P_genplist_t *plist, size_t *nprops)
{
    herr_t ret_value=SUCCEED;      /* return value */

    FUNC_ENTER (H5P_get_nprops_plist, FAIL);

    assert(plist);
    assert(nprops);

    /* Get property size */
    *nprops=plist->nprops;

    FUNC_LEAVE (ret_value);
}   /* H5P_get_nprops_plist() */


/*--------------------------------------------------------------------------
 NAME
    H5P_get_nprops_pclass
 PURPOSE
    Internal routine to query the number of properties in a property class
 USAGE
    herr_t H5P_get_nprops_pclass(pclass, nprops)
        H5P_genclass_t *pclass;  IN: Property class to check
        size_t *nprops;         OUT: Number of properties in the property list
 RETURNS
    Success: non-negative value
    Failure: negative value
 DESCRIPTION
        This routine retrieves the number of a properties in a property class.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5P_get_nprops_pclass(H5P_genclass_t *pclass, size_t *nprops)
{
    herr_t ret_value=SUCCEED;      /* return value */

    FUNC_ENTER (H5P_get_nprops_pclass, FAIL);

    assert(pclass);
    assert(nprops);

    /* Get property size */
    *nprops=pclass->nprops;

    FUNC_LEAVE (ret_value);
}   /* H5P_get_nprops_pclass() */


/*--------------------------------------------------------------------------
 NAME
    H5Pget_nprops
 PURPOSE
    Routine to query the size of a property in a property list or class.
 USAGE
    herr_t H5Pget_nprops(id, nprops)
        hid_t id;               IN: ID of Property list or class to check
        size_t *nprops;         OUT: Number of properties in the property object
 RETURNS
    Success: non-negative value
    Failure: negative value
 DESCRIPTION
        This routine retrieves the number of properties in a property list or
    class.  If a property class ID is given, the number of registered properties
    in the class is returned in NPROPS.  If a property list ID is given, the
    current number of properties in the list is returned in NPROPS.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5Pget_nprops(hid_t id, size_t *nprops)
{
    H5P_genplist_t	*plist;    /* Property list to query */
    H5P_genclass_t	*pclass;   /* Property class to query */
    herr_t ret_value=SUCCEED;      /* return value */

    FUNC_ENTER (H5Pget_nprops, FAIL);

    /* Check arguments. */
    if (H5I_GENPROP_LST != H5I_get_type(id) && H5I_GENPROP_CLS != H5I_get_type(id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property object");
    if (nprops==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid property nprops pointer");

    if(H5I_GENPROP_LST == H5I_get_type(id)) {
        if (NULL == (plist = H5I_object(id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if (H5P_get_nprops_plist(plist,nprops)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to query # of properties in plist");
    } /* end if */
    else 
        if(H5I_GENPROP_CLS == H5I_get_type(id)) {
            if (NULL == (pclass = H5I_object(id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property class");
            if (H5P_get_nprops_pclass(pclass,nprops)<0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to query # of properties in pclass");
        } /* end if */
        else
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property object");

done:
    FUNC_LEAVE (ret_value);
}   /* H5Pget_nprops() */


/*--------------------------------------------------------------------------
 NAME
    H5P_cmp_prop
 PURPOSE
    Internal routine to compare two generic properties
 USAGE
    int H5P_cmp_prop(prop1, prop2)
        H5P_genprop_t *prop1;    IN: 1st property to compare
        H5P_genprop_t *prop1;    IN: 2nd property to compare
 RETURNS
    Success: negative if prop1 "less" than prop2, positive if prop1 "greater"
        than prop2, zero if prop1 is "equal" to prop2
    Failure: can't fail
 DESCRIPTION
        This function compares two generic properties together to see if
    they are the same property.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static int H5P_cmp_prop(H5P_genprop_t *prop1, H5P_genprop_t *prop2)
{
    int cmp_value;             /* Value from comparison */
    int ret_value=0;         /* return value */

    FUNC_ENTER (H5P_cmp_prop, FAIL);

    assert(prop1);
    assert(prop2);

    /* Check the name */
    if((cmp_value=HDstrcmp(prop1->name,prop2->name))!=0)
        HGOTO_DONE(cmp_value);

    /* Check the size of properties */
    if(prop1->size < prop2->size) HGOTO_DONE(-1);
    if(prop1->size > prop2->size) HGOTO_DONE(1);

    /* Check if they both have values allocated (or not allocated) */
    if(prop1->value==NULL && prop2->value!=NULL) HGOTO_DONE(-1);
    if(prop1->value!=NULL && prop2->value==NULL) HGOTO_DONE(1);
    if(prop1->value!=NULL)
        if((cmp_value=HDmemcmp(prop1->value,prop2->value,prop1->size))!=0)
            HGOTO_DONE(cmp_value);

    /* Check if they both have default values allocated (or not allocated) */
    if(prop1->def_value==NULL && prop2->def_value!=NULL) HGOTO_DONE(-1);
    if(prop1->def_value!=NULL && prop2->def_value==NULL) HGOTO_DONE(1);
    if(prop1->def_value!=NULL)
        if((cmp_value=HDmemcmp(prop1->def_value,prop2->def_value,prop1->size))!=0)
            HGOTO_DONE(cmp_value);

    /* Check if they both have the same 'create' callback */
    if(prop1->create==NULL && prop2->create!=NULL) HGOTO_DONE(-1);
    if(prop1->create!=NULL && prop2->create==NULL) HGOTO_DONE(1);
    if(prop1->create!=prop2->create) HGOTO_DONE(-1);

    /* Check if they both have the same 'set' callback */
    if(prop1->set==NULL && prop2->set!=NULL) HGOTO_DONE(-1);
    if(prop1->set!=NULL && prop2->set==NULL) HGOTO_DONE(1);
    if(prop1->set!=prop2->set) HGOTO_DONE(-1);

    /* Check if they both have the same 'get' callback */
    if(prop1->get==NULL && prop2->get!=NULL) HGOTO_DONE(-1);
    if(prop1->get!=NULL && prop2->get==NULL) HGOTO_DONE(1);
    if(prop1->get!=prop2->get) HGOTO_DONE(-1);

    /* Check if they both have the same 'delete' callback */
    if(prop1->del==NULL && prop2->del!=NULL) HGOTO_DONE(-1);
    if(prop1->del!=NULL && prop2->del==NULL) HGOTO_DONE(1);
    if(prop1->del!=prop2->del) HGOTO_DONE(-1);

    /* Check if they both have the same 'copy' callback */
    if(prop1->copy==NULL && prop2->copy!=NULL) HGOTO_DONE(-1);
    if(prop1->copy!=NULL && prop2->copy==NULL) HGOTO_DONE(1);
    if(prop1->copy!=prop2->copy) HGOTO_DONE(-1);

    /* Check if they both have the same 'close' callback */
    if(prop1->close==NULL && prop2->close!=NULL) HGOTO_DONE(-1);
    if(prop1->close!=NULL && prop2->close==NULL) HGOTO_DONE(1);
    if(prop1->close!=prop2->close) HGOTO_DONE(-1);

    /* Don't check the 'next' field, they must be equal by now */

done:
    FUNC_LEAVE (ret_value);
}   /* H5P_cmp_prop() */


/*--------------------------------------------------------------------------
 NAME
    H5P_cmp_class
 PURPOSE
    Internal routine to compare two generic property classes 
 USAGE
    int H5P_cmp_class(pclass1, pclass2)
        H5P_genclass_t *pclass1;    IN: 1st property class to compare
        H5P_genclass_t *pclass2;    IN: 2nd property class to compare
 RETURNS
    Success: negative if class1 "less" than class2, positive if class1 "greater"
        than class2, zero if class1 is "equal" to class2
    Failure: can't fail
 DESCRIPTION
        This function compares two generic property classes together to see if
    they are the same class.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static int H5P_cmp_class(H5P_genclass_t *pclass1, H5P_genclass_t *pclass2)
{
    H5P_genprop_t *tprop1, *tprop2;/* Temporary pointer to properties */
    unsigned u;                    /* Local index variable */
    int cmp_value;             /* Value from comparison */
    int ret_value=0;         /* return value */

    FUNC_ENTER (H5P_cmp_class, FAIL);

    assert(pclass1);
    assert(pclass2);

    /* Check the name */
    if((cmp_value=HDstrcmp(pclass1->name,pclass2->name))!=0)
        HGOTO_DONE(cmp_value);

    /* Check the number of properties */
    if(pclass1->nprops < pclass2->nprops) HGOTO_DONE(-1);
    if(pclass1->nprops > pclass2->nprops) HGOTO_DONE(1);

    /* Check the hashsize */
    if(pclass1->hashsize < pclass2->hashsize) HGOTO_DONE(-1);
    if(pclass1->hashsize > pclass2->hashsize) HGOTO_DONE(1);

    /* Check the number of property lists created from the class */
    if(pclass1->plists < pclass2->plists) HGOTO_DONE(-1);
    if(pclass1->plists > pclass2->plists) HGOTO_DONE(1);

    /* Check the number of classes derived from the class */
    if(pclass1->classes < pclass2->classes) HGOTO_DONE(-1);
    if(pclass1->classes > pclass2->classes) HGOTO_DONE(1);

    /* Check the number of ID references open on the class */
    if(pclass1->ref_count < pclass2->ref_count) HGOTO_DONE(-1);
    if(pclass1->ref_count > pclass2->ref_count) HGOTO_DONE(1);

    /* Check whether they are internal or not */
    if(pclass1->internal < pclass2->internal) HGOTO_DONE(-1);
    if(pclass1->internal > pclass2->internal) HGOTO_DONE(1);

    /* Check whether they are deleted or not */
    if(pclass1->deleted < pclass2->deleted) HGOTO_DONE(-1);
    if(pclass1->deleted > pclass2->deleted) HGOTO_DONE(1);

    /* Check whether they have creation callback functions & data */
    if(pclass1->create_func==NULL && pclass2->create_func!=NULL) HGOTO_DONE(-1);
    if(pclass1->create_func!=NULL && pclass2->create_func==NULL) HGOTO_DONE(1);
    if(pclass1->create_func!=pclass2->create_func) HGOTO_DONE(-1);
    if(pclass1->create_data < pclass2->create_data) HGOTO_DONE(-1);
    if(pclass1->create_data > pclass2->create_data) HGOTO_DONE(1);

    /* Check whether they have close callback functions & data */
    if(pclass1->close_func==NULL && pclass2->close_func!=NULL) HGOTO_DONE(-1);
    if(pclass1->close_func!=NULL && pclass2->close_func==NULL) HGOTO_DONE(1);
    if(pclass1->close_func!=pclass2->close_func) HGOTO_DONE(-1);
    if(pclass1->close_data < pclass2->close_data) HGOTO_DONE(-1);
    if(pclass1->close_data > pclass2->close_data) HGOTO_DONE(1);

    /* Cycle through the properties and compare them also */
    for(u=0; u<pclass1->hashsize; u++) {
        tprop1=pclass1->props[u];
        tprop2=pclass2->props[u];

        /* Check if they both have properties in this hash location */
        if(tprop1==NULL && tprop2!=NULL) HGOTO_DONE(-1);
        if(tprop1!=NULL && tprop2==NULL) HGOTO_DONE(1);

        /* Check the actual properties */
        while(tprop1!=NULL && tprop2!=NULL) {
            /* Compare the two properties */
            if((cmp_value=H5P_cmp_prop(tprop1,tprop2))!=0)
                HGOTO_DONE(cmp_value);

            /* Advance the pointers */
            tprop1=tprop1->next;
            tprop2=tprop2->next;

            /* Check if they both have properties in this location */
            if(tprop1==NULL && tprop2!=NULL) HGOTO_DONE(-1);
            if(tprop1!=NULL && tprop2==NULL) HGOTO_DONE(1);
        } /* end while */
    } /* end for */

done:
    FUNC_LEAVE (ret_value);
}   /* H5P_cmp_class() */


/*--------------------------------------------------------------------------
 NAME
    H5P_cmp_plist
 PURPOSE
    Internal routine to compare two generic property lists 
 USAGE
    int H5P_cmp_plist(plist1, plist2)
        H5P_genplist_t *plist1;    IN: 1st property list to compare
        H5P_genplist_t *plist2;    IN: 2nd property list to compare
 RETURNS
    Success: negative if list1 "less" than list2, positive if list1 "greater"
        than list2, zero if list1 is "equal" to list2
    Failure: can't fail
 DESCRIPTION
        This function compares two generic property lists together to see if
    they are the same list.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static int H5P_cmp_plist(H5P_genplist_t *plist1, H5P_genplist_t *plist2)
{
    H5P_genprop_t *tprop1, *tprop2;/* Temporary pointer to properties */
    unsigned u;                    /* Local index variable */
    int cmp_value;             /* Value from comparison */
    int ret_value=0;         /* return value */

    FUNC_ENTER (H5P_cmp_plist, FAIL);

    assert(plist1);
    assert(plist2);

    /* Check the parent class */
    if(plist1->pclass < plist2->pclass) HGOTO_DONE(-1);
    if(plist1->pclass > plist2->pclass) HGOTO_DONE(1);

    /* Check the number of properties */
    if(plist1->nprops < plist2->nprops) HGOTO_DONE(-1);
    if(plist1->nprops > plist2->nprops) HGOTO_DONE(1);

    /* Check whether they've been initialized */
    if(plist1->class_init < plist2->class_init) HGOTO_DONE(-1);
    if(plist1->class_init > plist2->class_init) HGOTO_DONE(1);

    /* Cycle through the properties and compare them also */
    for(u=0; u<plist1->pclass->hashsize; u++) {
        tprop1=plist1->props[u];
        tprop2=plist2->props[u];

        /* Check if they both have properties in this hash location */
        if(tprop1==NULL && tprop2!=NULL) HGOTO_DONE(-1);
        if(tprop1!=NULL && tprop2==NULL) HGOTO_DONE(1);

        /* Check the actual properties */
        while(tprop1!=NULL && tprop2!=NULL) {
            /* Compare the two properties */
            if((cmp_value=H5P_cmp_prop(tprop1,tprop2))!=0)
                HGOTO_DONE(cmp_value);

            /* Advance the pointers */
            tprop1=tprop1->next;
            tprop2=tprop2->next;

            /* Check if they both have properties in this location */
            if(tprop1==NULL && tprop2!=NULL) HGOTO_DONE(-1);
            if(tprop1!=NULL && tprop2==NULL) HGOTO_DONE(1);
        } /* end while */
    } /* end for */

done:
    FUNC_LEAVE (ret_value);
}   /* H5P_cmp_plist() */


/*--------------------------------------------------------------------------
 NAME
    H5Pequal
 PURPOSE
    Routine to query whether two property lists or two property classes are equal
 USAGE
    htri_t H5Pequal(id1, id2)
        hid_t id1;         IN: Property list or class ID to compare
        hid_t id2;         IN: Property list or class ID to compare
 RETURNS
    Success: TRUE if equal, FALSE if unequal
    Failure: negative
 DESCRIPTION
    Determines whether two property lists or two property classes are equal.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
htri_t H5Pequal(hid_t id1, hid_t id2)
{
    void *obj1, *obj2;          /* Property objects to compare */
    htri_t ret_value=FALSE;     /* return value */

    FUNC_ENTER (H5Pequal, FAIL);

    /* Check arguments. */
    if ((H5I_GENPROP_LST != H5I_get_type(id1) && H5I_GENPROP_CLS != H5I_get_type(id1))
            || (H5I_GENPROP_LST != H5I_get_type(id2) && H5I_GENPROP_CLS != H5I_get_type(id2)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not property objects");
    if (H5I_get_type(id1) != H5I_get_type(id2))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not the same kind of property objects");
    if(NULL == (obj1 = H5I_object(id1)) || NULL == (obj2 = H5I_object(id2)))
        HGOTO_ERROR(H5E_PLIST, H5E_NOTFOUND, FAIL, "property object doesn't exist");

    /* Compare property lists */
    if(H5I_GENPROP_LST == H5I_get_type(id1)) {
        if(H5P_cmp_plist(obj1,obj2)==0)
            ret_value=TRUE;
    } /* end if */
    /* Must be property classes */
    else {
        if(H5P_cmp_class(obj1,obj2)==0)
            ret_value=TRUE;
    } /* end else */

done:
    FUNC_LEAVE (ret_value);
}   /* H5Pequal() */


/*--------------------------------------------------------------------------
 NAME
    H5P_isa_class_real
 PURPOSE
    Internal routine to query whether a property list is a certain class
 USAGE
    htri_t H5P_isa_class_real(plist, pclass)
        H5P_genplist_t *plist;    IN: Property list to check
        H5P_genclass_t *pclass;   IN: Property class to compare with
 RETURNS
    Success: TRUE (1) or FALSE (0)
    Failure: negative value
 DESCRIPTION
    This routine queries whether a property list is a member of the property
    list class.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    What about returning a value indicating that the property class is further
    up the class hierarchy?
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static htri_t H5P_isa_class_real(H5P_genplist_t *plist, H5P_genclass_t *pclass)
{
    htri_t ret_value=FAIL;

    FUNC_ENTER (H5P_isa_class_real, FAIL);

    assert(plist);
    assert(pclass);

    /* Compare property classes */
    if(H5P_cmp_class(plist->pclass,pclass)==0) {
        HGOTO_DONE(TRUE);
    } else {
        HGOTO_DONE(FALSE);
    } /* end else */

done:
    FUNC_LEAVE (ret_value);
}   /* H5P_isa_class_real() */


/*--------------------------------------------------------------------------
 NAME
    H5P_isa_class
 PURPOSE
    Internal routine to query whether a property list is a certain class
 USAGE
    hid_t H5P_isa_class(plist_id, pclass_id)
        hid_t plist_id;         IN: Property list to query
        hid_t pclass_id;        IN: Property class to query
 RETURNS
    Success: TRUE (1) or FALSE (0)
    Failure: negative
 DESCRIPTION
    This routine queries whether a property list is a member of the property
    list class.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    What about returning a value indicating that the property class is further
    up the class hierarchy?

    This function is special in that it is an internal library function, but
    accepts hid_t's as parameters.  Since it is used in basically the same way
    as the H5I functions, this should be OK.  Don't make more library functions
    which accept hid_t's without thorough discussion. -QAK
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
htri_t H5P_isa_class(hid_t plist_id, hid_t pclass_id)
{
    H5P_genplist_t	*plist;         /* Property list to query */
    H5P_genclass_t	*pclass;        /* Property list class */
    htri_t ret_value=FAIL;              /* return value */

    FUNC_ENTER (H5P_isa_class, FAIL);

    /* Check arguments. */
    if (H5I_GENPROP_LST != H5I_get_type(plist_id) || NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
    if (H5I_GENPROP_CLS != H5I_get_type(pclass_id) || NULL == (pclass = H5I_object(pclass_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property class");

    /* Compare the property list's class against the other class */
    if ((ret_value = H5P_isa_class_real(plist, pclass))<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to compare property list classes");

done:
    FUNC_LEAVE (ret_value);
}   /* H5P_isa_class() */


/*--------------------------------------------------------------------------
 NAME
    H5Pisa_class
 PURPOSE
    Routine to query whether a property list is a certain class
 USAGE
    hid_t H5Pisa_class(plist_id, pclass_id)
        hid_t plist_id;         IN: Property list to query
        hid_t pclass_id;        IN: Property class to query
 RETURNS
    Success: TRUE (1) or FALSE (0)
    Failure: negative
 DESCRIPTION
    This routine queries whether a property list is a member of the property
    list class.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    What about returning a value indicating that the property class is further
    up the class hierarchy?
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
htri_t H5Pisa_class(hid_t plist_id, hid_t pclass_id)
{
    htri_t ret_value=FAIL;              /* return value */

    FUNC_ENTER (H5Pisa_class, FAIL);

    /* Check arguments. */
    if (H5I_GENPROP_LST != H5I_get_type(plist_id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
    if (H5I_GENPROP_CLS != H5I_get_type(pclass_id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property class");

    /* Compare the property list's class against the other class */
    if ((ret_value = H5P_isa_class(plist_id, pclass_id))<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to compare property list classes");

done:
    FUNC_LEAVE (ret_value);
}   /* H5Pisa_class() */


/*--------------------------------------------------------------------------
 NAME
    H5P_iterate_props
 PURPOSE
    Internal routine to iterate over a hashtable of properties 
 USAGE
    herr_t H5P_iterate_props(id, hash, hashsize, idx, iter_func, iter_data)
        hid_t id;                   IN: ID of property object iterating over
        H5P_gen_prop_t *hash[];     IN: Pointer to array of properties for hash table
        unsigned hashsize;             IN: Size of hash table
        int *idx;                   IN/OUT: Index of the property to begin with
        H5P_iterate_t iter_func;    IN: Function pointer to function to be
                                        called with each property iterated over.
        void *iter_data;            IN/OUT: Pointer to iteration data from user
 RETURNS
    Success: Returns the return value of the last call to ITER_FUNC if it was
                non-zero, or zero if all properties have been processed.
    Failure: negative value
 DESCRIPTION
    This routine iterates over the properties in the property object specified
with ID.  For each property in the object, the ITER_DATA and some
additional information, specified below, are passed to the ITER_FUNC function.
The iteration begins with the IDX property in the object and the next element
to be processed by the operator is returned in IDX.  If IDX is NULL, then the
iterator starts at the first property; since no stopping point is returned in
this case, the iterator cannot be restarted if one of the calls to its operator
returns non-zero. 

The prototype for H5P_iterate_t is: 
    typedef herr_t (*H5P_iterate_t)(hid_t id, const char *name, void *iter_data); 
The operation receives the property list or class identifier for the object
being iterated over, ID, the name of the current property within the object,
NAME, and the pointer to the operator data passed in to H5Piterate, ITER_DATA. 

The return values from an operator are: 
    Zero causes the iterator to continue, returning zero when all properties
        have been processed. 
    Positive causes the iterator to immediately return that positive value,
        indicating short-circuit success. The iterator can be restarted at the
        index of the next property. 
    Negative causes the iterator to immediately return that value, indicating
        failure. The iterator can be restarted at the index of the next
        property.

H5Piterate assumes that the properties in the object identified by ID remains
unchanged through the iteration.  If the membership changes during the
iteration, the function's behavior is undefined. 

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static int H5P_iterate_props(hid_t id, H5P_genprop_t *hash[], unsigned hashsize, int *idx, H5P_iterate_t iter_func, void *iter_data)
{
    H5P_genprop_t *prop;        /* Temporary property pointer */
    unsigned u;                    /* Local index variable */
    int curr_idx=0;             /* Current iteration index */
    int ret_value=0;            /* Return value */

    FUNC_ENTER (H5P_iterate_props, FAIL);

    assert(hash);
    assert(hashsize>0);
    assert(idx);
    assert(iter_func);

    /* Cycle through the properties and compare them also */
    for(u=0; u<hashsize && ret_value==0; u++) {
        prop=hash[u];

        /* Check the actual properties */
        while(prop!=NULL && ret_value==0) {
            /* Check if we are at the object to start iterating over */
            if(curr_idx>=*idx)
                ret_value=(*iter_func)(id,prop->name,iter_data);

            /* Increment the iteration index if iteration function succeeded */
            if(ret_value==0)
                curr_idx++;

            /* Advance the pointer */
            prop=prop->next;
        } /* end while */
    } /* end for */

    /* Set the index we stopped at */
    *idx=curr_idx;

    FUNC_LEAVE (ret_value);
}   /* H5P_iterate_props() */


/*--------------------------------------------------------------------------
 NAME
    H5P_iterate_plist
 PURPOSE
    Internal routine to iterate over the properties in a property list
 USAGE
    herr_t H5P_iterate_plist(plist_id, idx, iter_func, iter_data)
        hid_t plist_id;             IN: ID of property list to iterate over
        int *idx;                   IN/OUT: Index of the property to begin with
        H5P_iterate_t iter_func;    IN: Function pointer to function to be
                                        called with each property iterated over.
        void *iter_data;            IN/OUT: Pointer to iteration data from user
 RETURNS
    Success: Returns the return value of the last call to ITER_FUNC if it was
                non-zero, or zero if all properties have been processed.
    Failure: negative value
 DESCRIPTION
    This routine iterates over the properties in the property object specified
with PLIST_ID.  For each property in the object, the ITER_DATA and some
additional information, specified below, are passed to the ITER_FUNC function.
The iteration begins with the IDX property in the object and the next element
to be processed by the operator is returned in IDX.  If IDX is NULL, then the
iterator starts at the first property; since no stopping point is returned in
this case, the iterator cannot be restarted if one of the calls to its operator
returns non-zero. 

The prototype for H5P_iterate_t is: 
    typedef herr_t (*H5P_iterate_t)(hid_t id, const char *name, void *iter_data); 
The operation receives the property list or class identifier for the object
being iterated over, ID, the name of the current property within the object,
NAME, and the pointer to the operator data passed in to H5Piterate, ITER_DATA. 

The return values from an operator are: 
    Zero causes the iterator to continue, returning zero when all properties
        have been processed. 
    Positive causes the iterator to immediately return that positive value,
        indicating short-circuit success. The iterator can be restarted at the
        index of the next property. 
    Negative causes the iterator to immediately return that value, indicating
        failure. The iterator can be restarted at the index of the next
        property.

H5Piterate assumes that the properties in the object identified by ID remains
unchanged through the iteration.  If the membership changes during the
iteration, the function's behavior is undefined. 

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static int H5P_iterate_plist(hid_t plist_id, int *idx, H5P_iterate_t iter_func, void *iter_data)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    int ret_value=0;            /* Return value */

    FUNC_ENTER (H5P_iterate_plist, FAIL);

    assert(idx);
    assert(iter_func);

    /* Get the property list object */
    if (H5I_GENPROP_LST != H5I_get_type(plist_id) || NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

    /* Iterate through the properties in the property list */
    ret_value=H5P_iterate_props(plist_id, plist->props, plist->pclass->hashsize, idx, iter_func, iter_data);

done:
    FUNC_LEAVE (ret_value);
}   /* H5P_iterate_plist() */


/*--------------------------------------------------------------------------
 NAME
    H5P_iterate_pclass
 PURPOSE
    Internal routine to iterate over the properties in a property class
 USAGE
    herr_t H5P_iterate_pclass(pclass_id, idx, iter_func, iter_data)
        hid_t pclass_id;            IN: ID of property class to iterate over
        int *idx;                   IN/OUT: Index of the property to begin with
        H5P_iterate_t iter_func;    IN: Function pointer to function to be
                                        called with each property iterated over.
        void *iter_data;            IN/OUT: Pointer to iteration data from user
 RETURNS
    Success: Returns the return value of the last call to ITER_FUNC if it was
                non-zero, or zero if all properties have been processed.
    Failure: negative value
 DESCRIPTION
    This routine iterates over the properties in the property object specified
with PCLASS_ID.  For each property in the object, the ITER_DATA and some
additional information, specified below, are passed to the ITER_FUNC function.
The iteration begins with the IDX property in the object and the next element
to be processed by the operator is returned in IDX.  If IDX is NULL, then the
iterator starts at the first property; since no stopping point is returned in
this case, the iterator cannot be restarted if one of the calls to its operator
returns non-zero. 

The prototype for H5P_iterate_t is: 
    typedef herr_t (*H5P_iterate_t)(hid_t id, const char *name, void *iter_data); 
The operation receives the property list or class identifier for the object
being iterated over, ID, the name of the current property within the object,
NAME, and the pointer to the operator data passed in to H5Piterate, ITER_DATA. 

The return values from an operator are: 
    Zero causes the iterator to continue, returning zero when all properties
        have been processed. 
    Positive causes the iterator to immediately return that positive value,
        indicating short-circuit success. The iterator can be restarted at the
        index of the next property. 
    Negative causes the iterator to immediately return that value, indicating
        failure. The iterator can be restarted at the index of the next
        property.

H5Piterate assumes that the properties in the object identified by ID remains
unchanged through the iteration.  If the membership changes during the
iteration, the function's behavior is undefined. 

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static int H5P_iterate_pclass(hid_t pclass_id, int *idx, H5P_iterate_t iter_func, void *iter_data)
{
    H5P_genclass_t *pclass;     /* Property list pointer */
    int ret_value=0;            /* Return value */

    FUNC_ENTER (H5P_iterate_pclass, FAIL);

    assert(idx);
    assert(iter_func);

    /* Get the property list object */
    if (H5I_GENPROP_CLS != H5I_get_type(pclass_id) || NULL == (pclass = H5I_object(pclass_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property class");

    /* Iterate through the properties in the property list */
    ret_value=H5P_iterate_props(pclass_id, pclass->props, pclass->hashsize, idx, iter_func, iter_data);

done:
    FUNC_LEAVE (ret_value);
}   /* H5P_iterate_pclass() */


/*--------------------------------------------------------------------------
 NAME
    H5Piterate
 PURPOSE
    Routine to iterate over the properties in a property list or class
 USAGE
    int H5Piterate(pclass_id, idx, iter_func, iter_data)
        hid_t id;                   IN: ID of property object to iterate over
        int *idx;                   IN/OUT: Index of the property to begin with
        H5P_iterate_t iter_func;    IN: Function pointer to function to be
                                        called with each property iterated over.
        void *iter_data;            IN/OUT: Pointer to iteration data from user
 RETURNS
    Success: Returns the return value of the last call to ITER_FUNC if it was
                non-zero, or zero if all properties have been processed.
    Failure: negative value
 DESCRIPTION
    This routine iterates over the properties in the property object specified
with ID.  The properties in both property lists and classes may be iterated
over with this function.  For each property in the object, the ITER_DATA and
some additional information, specified below, are passed to the ITER_FUNC
function.  The iteration begins with the IDX property in the object and the
next element to be processed by the operator is returned in IDX.  If IDX is
NULL, then the iterator starts at the first property; since no stopping point
is returned in this case, the iterator cannot be restarted if one of the calls
to its operator returns non-zero. 

The prototype for H5P_iterate_t is: 
    typedef herr_t (*H5P_iterate_t)(hid_t id, const char *name, void *iter_data); 
The operation receives the property list or class identifier for the object
being iterated over, ID, the name of the current property within the object,
NAME, and the pointer to the operator data passed in to H5Piterate, ITER_DATA. 

The return values from an operator are: 
    Zero causes the iterator to continue, returning zero when all properties
        have been processed. 
    Positive causes the iterator to immediately return that positive value,
        indicating short-circuit success. The iterator can be restarted at the
        index of the next property. 
    Negative causes the iterator to immediately return that value, indicating
        failure. The iterator can be restarted at the index of the next
        property.

H5Piterate assumes that the properties in the object identified by ID remains
unchanged through the iteration.  If the membership changes during the
iteration, the function's behavior is undefined. 

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
int H5Piterate(hid_t id, int *idx, H5P_iterate_t iter_func, void *iter_data)
{
    int fake_idx=0;         /* Index when user doesn't provide one */
    int ret_value=FAIL;     /* return value */

    FUNC_ENTER (H5Piterate, FAIL);

    /* Check arguments. */
    if (H5I_GENPROP_LST != H5I_get_type(id) && H5I_GENPROP_CLS != H5I_get_type(id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property object");
    if (iter_func==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid iteration callback");

    if (H5I_GENPROP_LST == H5I_get_type(id)) {
        /* Iterate over a property list */
        if ((ret_value=H5P_iterate_plist(id,(idx ? idx : &fake_idx),iter_func,iter_data))<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to iterate over list");
    } /* end if */
    else 
        if (H5I_GENPROP_CLS == H5I_get_type(id)) {
            /* Iterate over a property class */
            if ((ret_value=H5P_iterate_pclass(id,(idx ? idx : &fake_idx),iter_func,iter_data))<0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to iterate over class");
        } /* end if */
        else 
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property object");

done:
    FUNC_LEAVE (ret_value);
}   /* H5Piterate() */


/*--------------------------------------------------------------------------
 NAME
    H5P_peek_unsigned
 PURPOSE
    Internal routine to quickly retrieve the value of a property in a property list.
 USAGE
    int H5P_peek_unsigned(plist, name)
        H5P_genplist_t *plist;  IN: Property list to check
        const char *name;       IN: Name of property to query
 RETURNS
    Directly returns the value of the property in the list
 DESCRIPTION
        This function directly returns the value of a property in a property
    list.  Because this function is only able to just copy a particular property
    value to the return value, there is no way to check for errors.  We attempt
    to make certain that bad things don't happen by validating that the size of
    the property is the same as the size of the return type, but that can't
    catch all errors.
        This function does call the user's 'get' callback routine still.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    No error checking!
    Use with caution!
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
unsigned H5P_peek_unsigned(H5P_genplist_t *plist, const char *name)
{
    unsigned ret_value;            /* return value */

    FUNC_ENTER (H5P_peek_unsigned, UFAIL);

    assert(plist);
    assert(name);

    /* Get the value to return, don't worry about the return value, we can't return it */
    H5P_get(plist,name,&ret_value);

    FUNC_LEAVE (ret_value);
}   /* H5P_peek_unsigned() */


/*--------------------------------------------------------------------------
 NAME
    H5P_peek_hid_t
 PURPOSE
    Internal routine to quickly retrieve the value of a property in a property list.
 USAGE
    hid_t H5P_peek_hid_t(plist, name)
        H5P_genplist_t *plist;  IN: Property list to check
        const char *name;       IN: Name of property to query
 RETURNS
    Directly returns the value of the property in the list
 DESCRIPTION
        This function directly returns the value of a property in a property
    list.  Because this function is only able to just copy a particular property
    value to the return value, there is no way to check for errors.  We attempt
    to make certain that bad things don't happen by validating that the size of
    the property is the same as the size of the return type, but that can't
    catch all errors.
        This function does call the user's 'get' callback routine still.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    No error checking!
    Use with caution!
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hid_t H5P_peek_hid_t(H5P_genplist_t *plist, const char *name)
{
    hid_t ret_value;            /* return value */

    FUNC_ENTER (H5P_peek_hid_t, FAIL);

    assert(plist);
    assert(name);

    /* Get the value to return, don't worry about the return value, we can't return it */
    H5P_get(plist,name,&ret_value);

    FUNC_LEAVE (ret_value);
}   /* H5P_peek_hid_t() */


/*--------------------------------------------------------------------------
 NAME
    H5P_peek_voidp
 PURPOSE
    Internal routine to quickly retrieve the value of a property in a property list.
 USAGE
    void *H5P_peek_voidp(plist, name)
        H5P_genplist_t *plist;  IN: Property list to check
        const char *name;       IN: Name of property to query
 RETURNS
    Directly returns the value of the property in the list
 DESCRIPTION
        This function directly returns the value of a property in a property
    list.  Because this function is only able to just copy a particular property
    value to the return value, there is no way to check for errors.  We attempt
    to make certain that bad things don't happen by validating that the size of
    the property is the same as the size of the return type, but that can't
    catch all errors.
        This function does call the user's 'get' callback routine still.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    No error checking!
    Use with caution!
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
void *H5P_peek_voidp(H5P_genplist_t *plist, const char *name)
{
    void * ret_value;            /* return value */

    FUNC_ENTER (H5P_peek_voidp_t, NULL);

    assert(plist);
    assert(name);

    /* Get the value to return, don't worry about the return value, we can't return it */
    H5P_get(plist,name,&ret_value);

    FUNC_LEAVE (ret_value);
}   /* H5P_peek_voidp() */


/*--------------------------------------------------------------------------
 NAME
    H5P_peek_hsize_t
 PURPOSE
    Internal routine to quickly retrieve the value of a property in a property list.
 USAGE
    hsize_t H5P_peek_hsize_t(plist, name)
        H5P_genplist_t *plist;  IN: Property list to check
        const char *name;       IN: Name of property to query
 RETURNS
    Directly returns the value of the property in the list
 DESCRIPTION
        This function directly returns the value of a property in a property
    list.  Because this function is only able to just copy a particular property
    value to the return value, there is no way to check for errors.  We attempt
    to make certain that bad things don't happen by validating that the size of
    the property is the same as the size of the return type, but that can't
    catch all errors.
        This function does call the user's 'get' callback routine still.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    No error checking!
    Use with caution!
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hsize_t H5P_peek_hsize_t(H5P_genplist_t *plist, const char *name)
{
    hsize_t ret_value;            /* return value */

    FUNC_ENTER (H5P_peek_hsize_t, UFAIL);

    assert(plist);
    assert(name);

    /* Get the value to return, don't worry about the return value, we can't return it */
    H5P_get(plist,name,&ret_value);

    FUNC_LEAVE (ret_value);
}   /* H5P_peek_hsize_t() */


/*--------------------------------------------------------------------------
 NAME
    H5P_peek_size_t
 PURPOSE
    Internal routine to quickly retrieve the value of a property in a property list.
 USAGE
    hsize_t H5P_peek_size_t(plist, name)
        H5P_genplist_t *plist;  IN: Property list to check
        const char *name;       IN: Name of property to query
 RETURNS
    Directly returns the value of the property in the list
 DESCRIPTION
        This function directly returns the value of a property in a property
    list.  Because this function is only able to just copy a particular property
    value to the return value, there is no way to check for errors.  We attempt
    to make certain that bad things don't happen by validating that the size of
    the property is the same as the size of the return type, but that can't
    catch all errors.
        This function does call the user's 'get' callback routine still.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    No error checking!
    Use with caution!
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
size_t H5P_peek_size_t(H5P_genplist_t *plist, const char *name)
{
    size_t ret_value;            /* return value */

    FUNC_ENTER (H5P_peek_size_t, UFAIL);

    assert(plist);
    assert(name);

    /* Get the value to return, don't worry about the return value, we can't return it */
    H5P_get(plist,name,&ret_value);

    FUNC_LEAVE (ret_value);
}   /* H5P_peek_size_t() */


/*--------------------------------------------------------------------------
 NAME
    H5P_get
 PURPOSE
    Internal routine to query the value of a property in a property list.
 USAGE
    herr_t H5P_get(plist, name, value)
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
    H5P_genprop_t *prop;        /* Temporary property pointer */
    herr_t ret_value=FAIL;      /* return value */

    FUNC_ENTER (H5P_get, FAIL);

    assert(plist);
    assert(name);
    assert(value);

    /* Find property */
    if((prop=H5P_find_prop(plist->props,plist->pclass->hashsize,name))==NULL)
        HGOTO_ERROR(H5E_PLIST, H5E_NOTFOUND, FAIL, "property doesn't exist");

    /* Check for property size >0 */
    if(prop->size==0)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "property has zero size");

    /* Make a copy of the value and pass to 'get' callback */
    if(prop->get!=NULL) {
        void *tmp_value;            /* Temporary value for property */

        /* Make a copy of the current value, in case the callback fails */
        if (NULL==(tmp_value=H5MM_malloc(prop->size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed temporary property value");
        HDmemcpy(tmp_value,prop->value,prop->size);

        /* Call user's callback */
        if((*(prop->get))(plist->plist_id,name,prop->size,tmp_value)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "can't get property value");

        /* Copy new [possibly unchanged] value into return value */
        HDmemcpy(value,tmp_value,prop->size);

        /* Free the temporary value buffer */
        H5MM_xfree(tmp_value);
    } /* end if */
    /* No 'get' callback, just copy value */
    else
        HDmemcpy(value,prop->value,prop->size);

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
    herr_t H5Pget(plist_id, name, value)
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
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER (H5Pget, FAIL);

    /* Check arguments. */
    if(H5I_GENPROP_LST != H5I_get_type(plist_id) || NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
    if (!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid property name");
    if (value==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalied property value");

    /* Go get the value */
    if(H5P_get(plist,name,value)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to query property value");

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
static herr_t H5P_remove(hid_t plist_id, H5P_genplist_t *plist, const char *name)
{
    H5P_genprop_t *prop;        /* Temporary property pointer */
    H5P_genprop_t *tprop, *prev;/* Temporary pointer to properties */
    unsigned loc;                  /* Hash table location */
    herr_t ret_value=FAIL;      /* return value */

    FUNC_ENTER (H5P_remove, FAIL);

    assert(plist);
    assert(name);

    /* Find the property in the property list */
    if((prop=H5P_find_prop(plist->props,plist->pclass->hashsize,name))==NULL)
        HGOTO_ERROR(H5E_PLIST, H5E_NOTFOUND, FAIL, "property doesn't exist");

    /* Pass value to 'close' callback, if it exists */
    if(prop->del!=NULL) {
        /* Call user's callback */
        if((*(prop->del))(plist_id,name,prop->size,prop->value)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "can't close property value");
    } /* end if */

    /* Get correct hash table location */
    loc=H5P_hash_name(name,plist->pclass->hashsize);
    
    /* Remove property from property list */
    /* Check if the property being removed is at the head of the list for a hash location */
    if(prop==plist->props[loc]) {
        /* Jump over the property we are deleting */
        plist->props[loc]=prop->next;

        /* Free the property, ignoring return value, nothing we can do */
        H5P_free_prop(prop);
    } /* end if */
    else {
        /* Set up initial pointers */
        prev=tprop=plist->props[loc];
        tprop=tprop->next;
        while(tprop!=NULL) {
            if(tprop==prop) {
                /* Jump over the property we are deleting */
                prev->next=prop->next;

                /* Free the property, ignoring return value, nothing we can do */
                H5P_free_prop(prop);

                /* Break out of while loop */
                break;
            } /* end if */

            /* Move to the next nodes */
            prev=tprop;
            tprop=tprop->next;
        } /* end while */
    } /* end else */

    /* Decrement the number of properties in list */
    plist->nprops--;

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
    if ((ret_value=H5P_remove(plist_id,plist,name))<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTDELETE, FAIL, "unable to remove property");

done:
    FUNC_LEAVE (ret_value);
}   /* H5Premove() */


/*--------------------------------------------------------------------------
 NAME
    H5P_copy_prop_plist
 PURPOSE
    Internal routine to copy a property from one list to another
 USAGE
    herr_t H5P_copy_prop_plist(dst_plist, src_plist, name)
        hid_t dst_id;               IN: ID of destination property list or class
        hid_t src_id;               IN: ID of source property list or class
        const char *name;           IN: Name of property to copy
 RETURNS
    Success: non-negative value.
    Failure: negative value.
 DESCRIPTION
    Copies a property from one property list to another.
    
    If a property is copied from one list to another, the property will be
    first deleted from the destination list (generating a call to the 'close'
    callback for the property, if one exists) and then the property is copied
    from the source list to the destination list (generating a call to the
    'copy' callback for the property, if one exists).

    If the property does not exist in the destination list, this call is
    equivalent to calling H5Pinsert and the 'create' callback will be called
    (if such a callback exists for the property).

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5P_copy_prop_plist(hid_t dst_id, hid_t src_id, const char *name)
{
    H5P_genplist_t *dst_plist;      /* Pointer to destination property list */
    H5P_genplist_t *src_plist;      /* Pointer to source property list */
    H5P_genprop_t *prop;            /* Temporary property pointer */
    H5P_genprop_t *new_prop=NULL;   /* Pointer to new property */
    herr_t ret_value=SUCCEED;       /* return value */

    FUNC_ENTER (H5P_copy_prop_plist, FAIL);

    assert(name);

    /* Get the objects to operate on */
    if(NULL == (src_plist = H5I_object(src_id)) || NULL == (dst_plist = H5I_object(dst_id)))
        HGOTO_ERROR(H5E_PLIST, H5E_NOTFOUND, FAIL, "property object doesn't exist");

    /* Check if the property exists in the destination */
    prop=H5P_find_prop(dst_plist->props,dst_plist->pclass->hashsize,name);

    /* If the property exists in the destination alread */
    if(prop!=NULL) {
        /* Delete the property from the destination list, calling the 'close' callback if necessary */
        if(H5P_remove(dst_id,dst_plist,name)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDELETE, FAIL, "unable to remove property");

        /* Get the pointer to the source property */
        prop=H5P_find_prop(src_plist->props,src_plist->pclass->hashsize,name);

        /* Make a copy of the source property */
        if((new_prop=H5P_dup_prop(prop))==NULL)
            HGOTO_ERROR (H5E_PLIST, H5E_CANTCOPY, FAIL,"Can't copy property");

        /* Call property copy callback, if it exists */
        if(new_prop->copy) {
            if((new_prop->copy)(new_prop->name,new_prop->size,new_prop->value)<0)
                HGOTO_ERROR (H5E_PLIST, H5E_CANTCOPY, FAIL,"Can't copy property");
        } /* end if */

        /* Insert the initialized property into the property list */
        if(H5P_add_prop(dst_plist->props,dst_plist->pclass->hashsize,new_prop)<0)
            HGOTO_ERROR (H5E_PLIST, H5E_CANTINSERT, FAIL,"Can't insert property into list");

        /* Increment the number of properties in list */
        dst_plist->nprops++;
    } /* end if */
    /* If not, get the information required to do an H5Pinsert with the property into the destination list */
    else {
        /* Get the pointer to the source property */
        prop=H5P_find_prop(src_plist->props,src_plist->pclass->hashsize,name);

        /* Create property object from parameters */
        if((new_prop=H5P_create_prop(prop->name,prop->size,prop->def_value,prop->value,prop->create,prop->set,prop->get,prop->del,prop->copy,prop->copy))==NULL)
            HGOTO_ERROR (H5E_PLIST, H5E_CANTCREATE, FAIL,"Can't create property");

        /* Call property creation callback, if it exists */
        if(new_prop->create) {
            if((new_prop->create)(new_prop->name,new_prop->size,new_prop->value)<0)
                HGOTO_ERROR (H5E_PLIST, H5E_CANTINIT, FAIL,"Can't initialize property");
        } /* end if */

        /* Insert property into property list class */
        if(H5P_add_prop(dst_plist->props,dst_plist->pclass->hashsize,new_prop)<0)
            HGOTO_ERROR (H5E_PLIST, H5E_CANTINSERT, FAIL,"Can't insert property into class");

        /* Increment property count for class */
        dst_plist->nprops++;

    } /* end else */

done:
    /* Cleanup, if necessary */
    if(ret_value<0) {
        if(new_prop!=NULL)
            H5P_free_prop(new_prop);
    } /* end if */

    FUNC_LEAVE (ret_value);
}   /* H5P_copy_prop_plist() */


/*--------------------------------------------------------------------------
 NAME
    H5P_copy_prop_pclass
 PURPOSE
    Internal routine to copy a property from one class to another
 USAGE
    herr_t H5P_copy_prop_pclass(dst_pclass, src_pclass, name)
        H5P_genclass_t	*dst_pclass;    IN: Pointer to destination class
        H5P_genclass_t	*src_pclass;    IN: Pointer to source class
        const char *name;               IN: Name of property to copy
 RETURNS
    Success: non-negative value.
    Failure: negative value.
 DESCRIPTION
    Copies a property from one property class to another.
    
    If a property is copied from one class to another, all the property
    information will be first deleted from the destination class and then the
    property information will be copied from the source class into the
    destination class.
    
    If the property does not exist in the destination class or list, this call
    is equivalent to calling H5Pregister.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5P_copy_prop_pclass(H5P_genclass_t *dst_pclass, H5P_genclass_t *src_pclass, const char *name)
{
    H5P_genprop_t *prop;            /* Temporary property pointer */
    herr_t ret_value=SUCCEED;       /* return value */

    FUNC_ENTER (H5P_copy_prop_pclass, FAIL);

    assert(dst_pclass);
    assert(src_pclass);
    assert(name);

    /* Check if the property exists in the destination */
    prop=H5P_find_prop(dst_pclass->props,dst_pclass->hashsize,name);

    /* If the property exists in the destination already */
    if(prop!=NULL) {
        /* Delete the old property from the destination class */
        if(H5P_unregister(dst_pclass,name)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDELETE, FAIL, "unable to remove property");
    } /* end if */

    /* Register the property into the destination */
    if(H5P_register(dst_pclass,name,prop->size,prop->def_value,prop->create,prop->set,prop->get,prop->del,prop->copy,prop->close)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTDELETE, FAIL, "unable to remove property");

done:
    /* Cleanup, if necessary */

    FUNC_LEAVE (ret_value);
}   /* H5P_copy_prop_pclass() */


/*--------------------------------------------------------------------------
 NAME
    H5Pcopy_prop
 PURPOSE
    Routine to copy a property from one list or class to another
 USAGE
    herr_t H5Pcopy_prop(dst_id, src_id, name)
        hid_t dst_id;               IN: ID of destination property list or class
        hid_t src_id;               IN: ID of source property list or class
        const char *name;           IN: Name of property to copy
 RETURNS
    Success: non-negative value.
    Failure: negative value.
 DESCRIPTION
    Copies a property from one property list or class to another.
    
    If a property is copied from one class to another, all the property
    information will be first deleted from the destination class and then the
    property information will be copied from the source class into the
    destination class.
    
    If a property is copied from one list to another, the property will be
    first deleted from the destination list (generating a call to the 'close'
    callback for the property, if one exists) and then the property is copied
    from the source list to the destination list (generating a call to the
    'copy' callback for the property, if one exists).

    If the property does not exist in the destination class or list, this call
    is equivalent to calling H5Pregister or H5Pinsert (for a class or list, as
    appropriate) and the 'create' callback will be called in the case of the
    property being copied into a list (if such a callback exists for the
    property).

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5Pcopy_prop(hid_t dst_id, hid_t src_id, const char *name)
{
    void *src_obj, *dst_obj;    /* Property objects to copy between */
    herr_t ret_value=SUCCEED;      /* return value */

    FUNC_ENTER (H5Pcopy_prop, FAIL);
    H5TRACE3("e","iis",dst_id,src_id,name);

    /* Check arguments. */
    if ((H5I_GENPROP_LST != H5I_get_type(src_id) && H5I_GENPROP_CLS != H5I_get_type(src_id))
            || (H5I_GENPROP_LST != H5I_get_type(dst_id) && H5I_GENPROP_CLS != H5I_get_type(dst_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not property objects");
    if (H5I_get_type(src_id) != H5I_get_type(dst_id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not the same kind of property objects");
    if (!name || !*name)
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "no name given");
    if(NULL == (src_obj = H5I_object(src_id)) || NULL == (dst_obj = H5I_object(dst_id)))
        HGOTO_ERROR(H5E_PLIST, H5E_NOTFOUND, FAIL, "property object doesn't exist");

    /* Compare property lists */
    if(H5I_GENPROP_LST == H5I_get_type(src_id)) {
        if(H5P_copy_prop_plist(dst_id,src_id,name)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTCOPY, FAIL, "can't copy property between lists");
    } /* end if */
    /* Must be property classes */
    else {
        if(H5P_copy_prop_pclass(dst_obj,src_obj,name)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTCOPY, FAIL, "can't copy property between classes");
    } /* end else */

done:
    FUNC_LEAVE (ret_value);
}   /* H5Pcopy_prop() */


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
static herr_t H5P_unregister(H5P_genclass_t *pclass, const char *name)
{
    H5P_genprop_t *prop;        /* Temporary property pointer */
    H5P_genprop_t *tprop, *prev;/* Temporary pointer to properties */
    unsigned loc;                  /* Hash table location */
    herr_t ret_value=FAIL;     /* return value */

    FUNC_ENTER (H5P_unregister, FAIL);

    assert(pclass);
    assert(name);

    /* Find the property in the property list */
    if((prop=H5P_find_prop(pclass->props,pclass->hashsize,name))==NULL)
        HGOTO_ERROR(H5E_PLIST, H5E_NOTFOUND, FAIL, "property doesn't exist");

    /* Get correct hash table location */
    loc=H5P_hash_name(name,pclass->hashsize);
    
    /* Remove property from property list */
    /* Check if the property being removed is at the head of the list for a hash location */
    if(prop==pclass->props[loc]) {
        pclass->props[loc]=prop->next;

        /* Free the property, ignoring return value, nothing we can do */
        H5P_free_prop(prop);
    } /* end if */
    else {
        /* Set up initial pointers */
        prev=tprop=pclass->props[loc];
        tprop=tprop->next;
        while(tprop!=NULL) {
            if(tprop==prop) {
                /* Jump over the property we are deleting */
                prev->next=prop->next;

                /* Free the property, ignoring return value, nothing we can do */
                H5P_free_prop(prop);

                /* Break out of while loop */
                break;
            } /* end if */

            /* Move to the next nodes */
            prev=tprop;
            tprop=tprop->next;
        } /* end while */
    } /* end else */

    /* Decrement the number of registered properties in class */
    pclass->nprops--;

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
    H5P_close
 PURPOSE
    Internal routine to close a property list.
 USAGE
    herr_t H5P_close(plist)
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
static herr_t H5P_close(void *_plist)
{
    H5P_genplist_t *plist=(H5P_genplist_t *)_plist;
    herr_t ret_value=SUCCEED;     /* return value */

    FUNC_ENTER (H5P_close, FAIL);

    assert(plist);

    /* Make call to property list class close callback, if needed */
    if(plist->class_init!=0 && plist->pclass->close_func!=NULL) {
        /* Call user's "close" callback function, ignoring return value */
        (plist->pclass->close_func)(plist->plist_id,plist->pclass->close_data);
    } /* end if */

    /* Make calls to any property close callbacks which exist */
    H5P_free_all_prop(plist->props,plist->pclass->hashsize,1);

    /* Decrement class's dependant property list value! */
    if(H5P_access_class(plist->pclass,H5P_MOD_DEC_LST)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTINIT, FAIL, "Can't decrement class ref count");

    /* Destroy property list object */
    H5MM_xfree(plist);

done:
    FUNC_LEAVE (ret_value);
}   /* H5P_close() */


/*--------------------------------------------------------------------------
 NAME
    H5Pclose
 PURPOSE
    Routine to close a property list.
 USAGE
    herr_t H5Pclose(plist_id)
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
herr_t H5Pclose(hid_t plist_id)
{
    H5P_genplist_t	*plist;    /* Property list created */
    herr_t ret_value=SUCCEED;      /* return value */

    FUNC_ENTER (H5Pclose, FAIL);

    if (plist_id==H5P_DEFAULT)
        HGOTO_DONE(SUCCEED);

    /* Check arguments. */
    if (H5I_GENPROP_LST != H5I_get_type(plist_id) || NULL == (plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

    /* Close the property list */
    if (H5P_close(plist) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close");

    /* Remove the property list from the ID manager now */
    if (NULL == H5I_remove(plist_id))
        HGOTO_ERROR(H5E_ARGS, H5E_CANTDELETE, FAIL, "can't delete property list");

done:
    FUNC_LEAVE (ret_value);
}   /* H5Pclose() */


/*--------------------------------------------------------------------------
 NAME
    H5P_get_class_name
 PURPOSE
    Internal routine to query the name of a generic property list class
 USAGE
    char *H5P_get_class_name(pclass)
        H5P_genclass_t *pclass;    IN: Property list to check
 RETURNS
    Success: Pointer to a malloc'ed string containing the class name
    Failure: NULL
 DESCRIPTION
        This routine retrieves the name of a generic property list class.
    The pointer to the name must be free'd by the user for successful calls.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
char *H5P_get_class_name(H5P_genclass_t *pclass)
{
    char *ret_value=NULL;      /* return value */

    FUNC_ENTER (H5P_get_class_name, NULL);

    assert(pclass);

    /* Get property size */
    ret_value=HDstrdup(pclass->name);

    FUNC_LEAVE (ret_value);
}   /* H5P_get_class_name() */


/*--------------------------------------------------------------------------
 NAME
    H5Pget_class_name
 PURPOSE
    Routine to query the name of a generic property list class
 USAGE
    char *H5Pget_class_name(pclass_id)
        hid_t pclass_id;         IN: Property class to query
 RETURNS
    Success: Pointer to a malloc'ed string containing the class name
    Failure: NULL
 DESCRIPTION
        This routine retrieves the name of a generic property list class.
    The pointer to the name must be free'd by the user for successful calls.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
char *H5Pget_class_name(hid_t pclass_id)
{
    H5P_genclass_t	*pclass;    /* Property class to query */
    char *ret_value=NULL;       /* return value */

    FUNC_ENTER (H5Pget_class_name, NULL);

    /* Check arguments. */
    if (H5I_GENPROP_CLS != H5I_get_type(pclass_id) || NULL == (pclass = H5I_object(pclass_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a property class");

    /* Get the property list class name */
    if ((ret_value=H5P_get_class_name(pclass))==NULL)
        HGOTO_ERROR(H5E_PLIST, H5E_NOTFOUND, NULL, "unable to query name of class");

done:
    FUNC_LEAVE (ret_value);
}   /* H5Pget_class_name() */


/*--------------------------------------------------------------------------
 NAME
    H5P_get_class_parent
 PURPOSE
    Internal routine to query the parent class of a generic property class
 USAGE
    H5P_genclass_t *H5P_get_class_parent(pclass)
        H5P_genclass_t *pclass;    IN: Property class to check
 RETURNS
    Success: Pointer to the parent class of a property class
    Failure: NULL
 DESCRIPTION
    This routine retrieves a pointer to the parent class for a property class.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static H5P_genclass_t *H5P_get_class_parent(H5P_genclass_t *pclass)
{
    H5P_genclass_t *ret_value=NULL;      /* return value */

    FUNC_ENTER (H5P_get_class_parent, NULL);

    assert(pclass);

    /* Get property size */
    ret_value=pclass->parent;

    FUNC_LEAVE (ret_value);
}   /* H5P_get_class_parent() */


/*--------------------------------------------------------------------------
 NAME
    H5Pget_class_parent
 PURPOSE
    routine to query the parent class of a generic property class
 USAGE
    hid_t H5Pget_class_parent(pclass_id)
        hid_t pclass_id;         IN: Property class to query
 RETURNS
    Success: ID of parent class object
    Failure: NULL
 DESCRIPTION
    This routine retrieves an ID for the parent class of a property class.

 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hid_t H5Pget_class_parent(hid_t pclass_id)
{
    H5P_genclass_t	*pclass;    /* Property class to query */
    H5P_genclass_t	*parent=NULL;   /* Parent's property class */
    hid_t ret_value=FAIL;       /* return value */

    FUNC_ENTER (H5Pget_class_parent, FAIL);

    /* Check arguments. */
    if (H5I_GENPROP_CLS != H5I_get_type(pclass_id) || NULL == (pclass = H5I_object(pclass_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property class");

    /* Retrieve the property class's parent */
    if ((parent=H5P_get_class_parent(pclass))==NULL)
        HGOTO_ERROR(H5E_PLIST, H5E_NOTFOUND, FAIL, "unable to query class of property list");

    /* Increment the outstanding references to the class object */
    if(H5P_access_class(parent,H5P_MOD_INC_REF)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_CANTINIT, FAIL,"Can't increment class ID ref count");

    /* Get an atom for the class */
    if ((ret_value = H5I_register(H5I_GENPROP_CLS, parent))<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to atomize property list class");

done:
    if (ret_value<0 && parent)
        H5P_close_class(parent);

    FUNC_LEAVE (ret_value);
}   /* H5Pget_class_parent() */


/*--------------------------------------------------------------------------
 NAME
    H5P_close_class
 PURPOSE
    Internal routine to close a property list class.
 USAGE
    herr_t H5P_close_class(class)
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
static herr_t H5P_close_class(void *_pclass)
{
    H5P_genclass_t *pclass=(H5P_genclass_t *)_pclass;
    herr_t ret_value = FAIL;     /* return value */

    FUNC_ENTER (H5P_close_class, FAIL);

    assert(pclass);

    /* Decrement the reference count & check if the object should go away */
    if(H5P_access_class(pclass,H5P_MOD_DEC_REF)<0)
        HGOTO_ERROR (H5E_PLIST, H5E_NOTFOUND, FAIL, "Can't decrement ID ref count");

    /* Check if the object should go away */
    if(pclass->ref_count==0) {
        /* Decrement parent class's dependant property class value! */
        if(pclass->parent)
            if (H5P_access_class(pclass->parent, H5P_MOD_DEC_CLS) < 0)
                HGOTO_ERROR (H5E_PLIST, H5E_NOTFOUND, FAIL,"Can't decrement class ref count");
        
        /* Mark class as deleted */
        pclass->deleted = 1;

        /* Check dependancies on this class, deleting it if allowed */
        if (H5P_access_class(pclass, H5P_MOD_CHECK) < 0)
            HGOTO_ERROR (H5E_PLIST, H5E_NOTFOUND, FAIL,"Can't check class ref count");
    } /* end if */

    /* Set return value */
    ret_value = SUCCEED;

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
    H5TRACE1("e","i",cls_id);

    /* Check arguments */
    if (H5I_GENPROP_CLS != H5I_get_type(cls_id) || NULL == (pclass = H5I_remove(cls_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list class");

    /* Delete the property list class */
    if (H5P_close_class(pclass) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't close");

done:
    FUNC_LEAVE(ret_value);
}   /* H5Pclose_class() */

