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
 * FILE:	H5I.c - Internal storage routines for handling "IDs"
 *
 * REMARKS:	ID's which allow objects (void *'s currently) to be bundled
 *		into "types" for more general storage.
 *
 * DESIGN:	The types are stored in an array of pointers to store each
 *		type in an element. Each "type" node contains a link to a
 *		hash table to manage the IDs in each type.  Allowed types are
 *		values within the range 1 to MAX_NUM_TYPES and are given out
 *		at run-time.  Types used by the library are stored in global
 *		variables defined in H5Ipublic.h.
 *
 * AUTHOR:	Quincey Koziol
 *
 * MODIFICATIONS:
 *	1/3/96	- Starting writing specs & coding prototype
 *	1/7/96	- Finished coding prototype
 *	6/10/97 - Moved into HDF5 library
 *	5/18/04 - Expanded to allow registration of new types at run-time
 */

#define H5I_PACKAGE		/*suppress error about including H5Ipkg	  */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5I_init_interface


#include "H5private.h"		/* Generic Functions			*/
#include "H5ACprivate.h"	/* Metadata cache			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5Ipkg.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Oprivate.h"		/* Object headers		  	*/
#include "H5AC2private.h"       /* Metadata cache                       */

/* Define this to compile in support for dumping ID information */
/* #define H5I_DEBUG_OUTPUT */
#ifndef H5I_DEBUG_OUTPUT
#include "H5Gprivate.h"		/* Groups				*/
#else /* H5I_DEBUG_OUTPUT */
#define H5G_PACKAGE /*suppress error message about including H5Gpkg.h */
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Tprivate.h"		/* Datatypes				*/
#endif /* H5I_DEBUG_OUTPUT */

/* Local Macros */

/*
 * Define the following macro for fast hash calculations (but limited
 * hash sizes)
 */
#define HASH_SIZE_POWER_2

#ifdef HASH_SIZE_POWER_2
/*
 * Map an ID to a hash location (assumes s is a power of 2 and smaller
 * than the ID_MASK constant).
 */
#  define H5I_LOC(a,s)		((hid_t)((size_t)(a)&((s)-1)))
#else
/*
 * Map an ID to a hash location.
 */
#  define H5I_LOC(a,s)	(((hid_t)(a)&ID_MASK)%(s))
#endif

/* Combine a Type number and an atom index into an atom */
#define H5I_MAKE(g,i)	((((hid_t)(g)&TYPE_MASK)<<ID_BITS)|	  \
			     ((hid_t)(i)&ID_MASK))

/* Local typedefs */

/* Atom information structure used */
typedef struct H5I_id_info_t {
    hid_t	id;		/* ID for this info			    */
    unsigned	count;		/* ref. count for this atom		    */
    void	*obj_ptr;	/* pointer associated with the atom	    */
    struct H5I_id_info_t *next;	/* link to next atom (in case of hash-clash)*/
} H5I_id_info_t;

/* ID type structure used */
typedef struct {
    unsigned	count;		/*# of times this type has been initialized*/
    unsigned	reserved;	/*# of IDs to reserve for constant IDs	    */
    unsigned	wrapped;	/*whether the id count has wrapped around   */
    size_t	hash_size;	/*sizeof the hash table to store the IDs in */
    unsigned	ids;		/*current number of IDs held		    */
    unsigned	nextid;		/*ID to use for the next atom		    */
    H5I_free_t	free_func;	/*release object method	    		    */
    H5I_id_info_t **id_list;	/*pointer to an array of ptrs to IDs	    */
} H5I_id_type_t;


/*-------------------- Locally scoped variables -----------------------------*/

/* Array of pointers to atomic types */
static H5I_id_type_t *H5I_id_type_list_g[MAX_NUM_TYPES];

/* Variable to keep track of the number of types allocated.  Its value is the */
/* next type ID to be handed out, so it is always one greater than the number */
/* of types. */
/* Starts at 1 instead of 0 because it makes trace output look nicer.  If more */
/* types (or IDs within a type) are needed, adjust TYPE_BITS in H5Ipkg.h       */
/* and/or increase size of hid_t */
static H5I_type_t H5I_next_type = (H5I_type_t) H5I_NTYPES;

/* Declare a free list to manage the H5I_id_info_t struct */
H5FL_DEFINE_STATIC(H5I_id_info_t);

/*--------------------- Local function prototypes ---------------------------*/
static H5I_id_info_t *H5I_find_id(hid_t id);
#ifdef H5I_DEBUG_OUTPUT
static herr_t H5I_debug(H5I_type_t type);
#endif /* H5I_DEBUG_OUTPUT */


/*--------------------------------------------------------------------------
NAME
   H5I_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5I_init_interface()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
static herr_t
H5I_init_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5I_init_interface);

    FUNC_LEAVE_NOAPI(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5I_term_interface
 *
 * Purpose:	Terminate the H5I interface: release all memory, reset all
 *		global variables to initial values. This only happens if all
 *		types have been destroyed from other interfaces.
 *
 * Return:	Success:	Positive if any action was taken that might
 *				affect some other interface; zero otherwise.
 *
 * 		Failure:	Negative.
 *
 * Programmer:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5I_term_interface(void)
{
    H5I_id_type_t	*type_ptr;
    H5I_type_t		type;
    int		n=0;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5I_term_interface);

    if (H5_interface_initialize_g) {
        /* How many types are still being used? */
        for (type=(H5I_type_t)0; type<H5I_next_type; H5_INC_ENUM(H5I_type_t,type)) {
            if ((type_ptr=H5I_id_type_list_g[type]) && type_ptr->id_list)
                n++;
        }

        /* If no types are used then clean up */
        if (0==n) {
            for (type=(H5I_type_t)0; type<H5I_next_type; H5_INC_ENUM(H5I_type_t,type)) {
                type_ptr = H5I_id_type_list_g[type];
                H5MM_xfree(type_ptr);
                H5I_id_type_list_g[type] = NULL;
            }
        }

        /* Mark interface closed */
        H5_interface_initialize_g = 0;
    }
    FUNC_LEAVE_NOAPI(n);
}


/*-------------------------------------------------------------------------
 * Function:	H5Iregister_type
 *
 * Purpose:	Public interface to H5I_register_type.  Creates a new type
 *		of ID's to give out.  A specific number (RESERVED) of type
 *		entries may be reserved to enable "constant" values to be handed
 *		out which are valid IDs in the type, but which do not map to any
 *		data structures and are not allocated dynamically later. HASH_SIZE is
 *		the minimum hash table size to use for the type. FREE_FUNC is
 *		called with an object pointer when the object is removed from
 *		the type.
 *
 * Return:	Success:	Type ID of the new type
 *
 *		Failure:	H5I_BADID
 *
 * Programmers:	Nathaniel Furrer
 *				James Laird
 *		Friday, April 30, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5I_type_t H5Iregister_type(size_t hash_size, unsigned reserved, H5I_free_t free_func)
{
	H5I_type_t ret_value;
	FUNC_ENTER_API(H5Iregister_type, H5I_BADID);

		/* Call H5I_register_type with a value of 0 to get a new type */
	ret_value = H5I_register_type((H5I_type_t)0, hash_size, reserved, free_func);

done:
	FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5I_register_type
 *
 * Purpose:	Creates a new type of ID's to give out.  A specific number
 *		(RESERVED) of type entries may be reserved to enable "constant"
 *		values to be handed out which are valid IDs in the type, but
 *		which do not map to any data structures and are not allocated
 *		dynamically later. TYPE_ID is the H5I_type_t value of the type
 *		to be initialized.  If this value is zero, a new type is created.
 *		If this value is one of the library types, that type is
 *		initialized or its reference count is incremented (if it is already
 *		initialized).  HASH_SIZE is the minimum hash table size to
 *		use for the type. FREE_FUNC is called with an object pointer
 *		when the object is removed from the type.
 *
 * Return:	Success:	Type ID of the new type
 *		Failure:	H5I_BADID
 *
 * Programmers:	Nathaniel Furrer
 *				James Laird
 *		Friday, April 30, 2004
 *
 * Modifications: The initialization section of this function was formerly
 *					H5I_init_type, programmed by Robb Matzke on February 19,
 *					1999.
 *
 * 		Bill Wendling, 2000-05-05
 * 		Instead of the ugly test of whether hash_size is a power of
 * 		two, I placed it in a macro POWER_OF_TWO which uses the fact
 * 		that a number that is a power of two has only 1 bit set.
 *
 * 		Bill Wendling, 2000-05-09
 * 		Changed POWER_OF_TWO macro to allow 1 as a valid power of two.
 * 		Changed test below accordingly.
 *
 *-------------------------------------------------------------------------
 */

H5I_type_t H5I_register_type(H5I_type_t type_id, size_t hash_size, unsigned reserved, H5I_free_t free_func)
{
	H5I_type_t ret_value=H5I_BADID;                   /* type ID to return */
        H5I_id_type_t	*type_ptr = NULL;		/*ptr to the atomic type*/
	int i;
	int done;

    FUNC_ENTER_NOAPI(H5I_register_type, H5I_BADID);

	/* Check that type_id is either a library type or zero */
	if(type_id < 0 || type_id >= H5I_NTYPES)
	{
		HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, H5I_BADID, "invalid type ID");
	}

	if(type_id == 0)	/* Generate a new H5I_type_t value */
	{
		/* Increment the number of types*/
		if (H5I_next_type < MAX_NUM_TYPES)
		{
			ret_value = H5I_next_type;
			H5_INC_ENUM(H5I_type_t, H5I_next_type);
		}
		else
		{
			done = 0;
			/* Look for a free type to give out */
			for(i = H5I_NTYPES; i < MAX_NUM_TYPES && done==0; i++)
			{
				if(H5I_id_type_list_g[i] == NULL)
				{
					/* Found a free type ID */
					ret_value = (H5I_type_t)i;
					done = 1;
				}
			}

			/* Verify that we found a type to give out */
			if(done == 0)
				HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, H5I_BADID, "Maximum number of ID types exceeded.");
		}
	}
	else	/* type_id is a library type; use this value. */
	{
		ret_value = type_id;
	}

	/* Initialize the type */
    /* Check arguments */
#ifdef HASH_SIZE_POWER_2
    if (!POWER_OF_TWO(hash_size) || hash_size == 1)
	HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, H5I_BADID, "invalid hash size");
#endif /* HASH_SIZE_POWER_2 */

    if (H5I_id_type_list_g[ret_value] == NULL) {
	/* Allocate the type information for new type */
	if (NULL==(type_ptr = H5MM_calloc(sizeof(H5I_id_type_t))))
	    HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, H5I_BADID, "memory allocation failed");
		H5I_id_type_list_g[ret_value] = type_ptr;
    } else {
		/* Get the pointer to the existing type */
		type_ptr = H5I_id_type_list_g[ret_value];
    }

    if (type_ptr->count == 0) {
		/* Initialize the ID type structure for new types */
		type_ptr->hash_size = hash_size;
		type_ptr->reserved = reserved;
		type_ptr->wrapped = 0;
		type_ptr->ids = 0;
		type_ptr->nextid = reserved;
		type_ptr->free_func = free_func;
		type_ptr->id_list = H5MM_calloc(hash_size*sizeof(H5I_id_info_t *));
		if (NULL==type_ptr->id_list)
			HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, H5I_BADID, "memory allocation failed");
    }

    /* Increment the count of the times this type has been initialized */
    type_ptr->count++;

	done:
	if(ret_value == H5I_BADID)	/* Clean up on error */
	{
		if (type_ptr != NULL)
		{
			H5MM_xfree(type_ptr->id_list);
			H5MM_xfree(type_ptr);
		}
	}

	FUNC_LEAVE_NOAPI(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5Itype_exists
 *
 * Purpose:     Query function to inform the user if a given type is
 *              currently registered with the library.
 *
 * Return:	Success:        1 if the type is registered, 0 if it is not
 *
 *		Failure:	Negative
 *
 * Programmer:	James Laird
 *		Nathaniel Furrer
 *              Tuesday, June 29, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
htri_t H5Itype_exists(H5I_type_t type)
{
    htri_t ret_value = TRUE;                      /* Return value */

    FUNC_ENTER_API(H5Itype_exists, FAIL);

    if (type<=H5I_BADID || type>=H5I_next_type)
	HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "invalid type number");

    if (H5I_id_type_list_g[type] == NULL)
        ret_value = FALSE;

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Inmembers
 *
 * Purpose:	Returns the number of members in a type.  Public interface to
 *		H5I_nmembers.  The public interface throws an error if the
 *              supplied type does not exist.  This is different than the
 *              private interface, which will just return 0.
 *
 * Return:	Success:	Zero
 *
 *		Failure:	Negative
 *
 * Programmer:	James Laird
 *		Nathaniel Furrer
 *              Friday, April 23, 2004
 *
 * Modifications:
 *              June 29, 2004
 *              Nat Furrer and James Laird
 *              Changed function signature to return the number of members
 *              by reference.
 *
 *-------------------------------------------------------------------------
 */
herr_t H5Inmembers(H5I_type_t type, hsize_t *num_members)
{
    int ret_value = SUCCEED;                      /* Return value */

    FUNC_ENTER_API(H5Inmembers, FAIL);

    if( H5I_IS_LIB_TYPE( type ) )
        HGOTO_ERROR(H5E_ATOM, H5E_BADGROUP, FAIL, "cannot call public function on library type");

    /* Validate parameters.  This needs to be done here, instead of letting
     * the private interface handle it, because the public interface throws
     * an error when the supplied type does not exist.
     */
    if (type<=H5I_BADID || type>=H5I_next_type)
	HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "invalid type number");
    if (NULL==H5I_id_type_list_g[type])
	HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "supplied type does not exist");

    if (num_members)
    {
        int members;

        if ((members = H5I_nmembers(type)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTCOUNT, FAIL, "can't compute number of members");

        *num_members=(hsize_t)members;
    }

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5I_nmembers
 *
 * Purpose:	Returns the number of members in a type.
 *
 * Return:	Success:	Number of members; zero if the type is empty
 *				or has been deleted.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Wednesday, March 24, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5I_nmembers(H5I_type_t type)
{
    H5I_id_type_t	*type_ptr = NULL;
    int		ret_value;

    FUNC_ENTER_NOAPI(H5I_nmembers, FAIL);

    if (type<=H5I_BADID || type>=H5I_next_type)
	HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "invalid type number");
    if (NULL==(type_ptr=H5I_id_type_list_g[type]) || type_ptr->count<=0)
	HGOTO_DONE(0);

    /* Set return value */
    H5_ASSIGN_OVERFLOW(ret_value, type_ptr->ids, unsigned, int);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Iclear_type
 *
 * Purpose:	Removes all objects from the type, calling the free
 *		function for each object regardless of the reference count.
 *		Public interface to H5I_clear_type.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	James Laird
 *				Nathaniel Furrer
 *              Friday, April 23, 2004
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
herr_t H5Iclear_type(H5I_type_t type, hbool_t force)
{
	herr_t ret_value;                      /* Return value */

    FUNC_ENTER_API(H5Iclear_type, FAIL);

	if( H5I_IS_LIB_TYPE( type ) )
	{
		HGOTO_ERROR(H5E_ATOM, H5E_BADGROUP, FAIL, "cannot call public function on library type");
	}

	ret_value = H5I_clear_type(type, force);

	done:
		FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5I_clear_type
 *
 * Purpose:	Removes all objects from the type, calling the free
 *		function for each object regardless of the reference count.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *              Wednesday, March 24, 1999
 *
 * Modifications:
 * 		Robb Matzke, 1999-04-27
 *		If FORCE is zero then any item for which the free callback
 *		failed is not removed.  This function returns failure if
 *		items could not be removed.
 *
 * 		Robb Matzke, 1999-08-17
 *		If the object reference count is larger than one then it must
 *		be because the library is using the object internally. This
 *		happens for instance for file driver ID's which are stored in
 *		things like property lists, files, etc.  Objects that have a
 *		reference count larger than one are not affected unless FORCE
 *		is non-zero.
 *-------------------------------------------------------------------------
 */
herr_t
H5I_clear_type(H5I_type_t type, hbool_t force)
{
    H5I_id_type_t  *type_ptr = NULL;    /* ptr to the atomic type */
    H5I_id_info_t   *cur=NULL;          /* Current node being worked with */
    H5I_id_info_t   *next=NULL;         /* Next node in list */
    H5I_id_info_t   *last=NULL;         /* Last node seen */
    H5I_id_info_t   *tmp=NULL;          /* Temporary node ptr */
    int		ret_value = SUCCEED;
    unsigned    delete_node;            /* Flag to indicate node should be removed from linked list */
    unsigned	i;

    FUNC_ENTER_NOAPI(H5I_clear_type, FAIL);

    if (type <= H5I_BADID || type >= H5I_next_type)
	HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "invalid type number");

    type_ptr = H5I_id_type_list_g[type];
    if (type_ptr == NULL || type_ptr->count <= 0)
	HGOTO_ERROR(H5E_ATOM, H5E_BADGROUP, FAIL, "invalid type");

    /*
     * Call free method for all objects in type regardless of their reference
     * counts. Ignore the return value from from the free method and remove
     * object from type regardless if FORCE is non-zero.
     */
    for (i=0; i<type_ptr->hash_size; i++) {
        for (cur=type_ptr->id_list[i]; cur; cur=next) {
            /*
             * Do nothing to the object if the reference count is larger than
             * one and forcing is off.
             */
            if (!force && cur->count>1) {
                next=cur->next;
                continue;
            } /* end if */

            /* Check for a 'free' function and call it, if it exists */
            if (type_ptr->free_func && (type_ptr->free_func)(cur->obj_ptr)<0) {
                if (force) {
#ifdef H5I_DEBUG
                    if (H5DEBUG(I)) {
                        fprintf(H5DEBUG(I), "H5I: free type=%d obj=0x%08lx "
                            "failure ignored\n", (int)type,
                            (unsigned long)(cur->obj_ptr));
                    } /* end if */
#endif /*H5I_DEBUG*/

                    /* Indicate node should be removed from list */
                    delete_node=1;
                } /* end if */
                else {
                    /* Indicate node should _NOT_ be remove from list */
                    delete_node=0;
                } /* end else */
            } /* end if */
            else {
                /* Indicate node should be removed from list */
                delete_node=1;
            } /* end else */

            /* Check if we should delete this node or not */
            if(delete_node) {
                /* Decrement the number of IDs in the type */
                (type_ptr->ids)--;

                /* Advance to next node */
                next = cur->next;

                /* Re-scan the list of nodes and remove the node from the list */
                /* (can't maintain static pointers to the previous node in the */
                /*      list, because the node's 'free' callback could have */
                /*      make an H5I call, which could potentially change the */
                /*      order of the nodes on the list - QAK) */
                last=NULL;
                tmp=type_ptr->id_list[i];
                while(tmp!=cur) {
                    assert(tmp!=NULL);
                    last=tmp;
                    tmp=tmp->next;
                } /* end while */

                /* Delete the node from the list */
                if(last==NULL) {
                    /* Node at head of list, just advance the list head to next node */
                    assert(type_ptr->id_list[i]==cur);
                    type_ptr->id_list[i] = next;
                } /* end if */
                else {
                    /* Node in middle of list, jump over it */
                    assert(last->next==cur);
                    last->next=next;
                } /* end else */

                /* Free the node */
                H5FL_FREE(H5I_id_info_t,cur);
            } /* end if */
            else {
                /* Advance to next node */
                next = cur->next;
            } /* end else */
        } /* end for */
    } /* end for */

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Idestroy_type
 *
 * Purpose:	Destroys a type along with all atoms in that type
 *		regardless of their reference counts. Destroying IDs
 *		involves calling the free-func for each ID's object and
 *		then adding the ID struct to the ID free list.  Public
 *		interface to H5I_destroy_type.
 *
 * Return:	Zero on success/Negative on failure
 *
 * Programmer:	Nathaniel Furrer
 *				James Laird
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t H5Idestroy_type(H5I_type_t type)
{
	herr_t ret_value;

	FUNC_ENTER_API(H5Idestroy_type, FAIL);

	if( H5I_IS_LIB_TYPE( type ) )
	{
		HGOTO_ERROR(H5E_ATOM, H5E_BADGROUP, FAIL, "cannot call public function on library type");
	}

	ret_value = H5I_destroy_type(type);

	done:
		FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5I_destroy_type
 *
 * Purpose:	Destroys a type along with all atoms in that type
 *		regardless of their reference counts. Destroying IDs
 *		involves calling the free-func for each ID's object and
 *		then adding the ID struct to the ID free list.
 *
 * Return:	Zero on success/Negative on failure
 *
 * Programmer:	Nathaniel Furrer
 *				James Laird
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t H5I_destroy_type(H5I_type_t type)
{
	herr_t ret_value = FAIL;
    H5I_id_type_t *type_ptr = NULL;	/* ptr to the atomic type */

	FUNC_ENTER_NOAPI(H5I_destroy_type, FAIL);

    if (type <= H5I_BADID || type >= H5I_next_type)
		HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "invalid type number");

    type_ptr = H5I_id_type_list_g[type];
    if (type_ptr == NULL || type_ptr->count <= 0)
		HGOTO_ERROR(H5E_ATOM, H5E_BADGROUP, FAIL, "invalid type");

	H5I_clear_type(type, TRUE);
	H5E_clear_stack(NULL); /*don't care about errors*/
	H5MM_xfree(type_ptr->id_list);

	H5MM_free(type_ptr);
	H5I_id_type_list_g[type] = NULL;
	ret_value = 0;

done:
	FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Iregister
 *
 * Purpose:	Public interface to H5I_register.
 *
 * Return:	Success:	New object id.
 *
 *		Failure:	Negative
 *
 * Programmer:	Nathaniel Furrer
 *				James Laird
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t H5Iregister(H5I_type_t type, void *object)
{
	hid_t ret_value;                      /* Return value */

    FUNC_ENTER_API(H5Iregister, H5I_INVALID_HID);

	if( H5I_IS_LIB_TYPE( type ) )
	{
		HGOTO_ERROR(H5E_ATOM, H5E_BADGROUP, FAIL, "cannot call public function on library type");
	}

	ret_value = H5I_register(type, object);

	done:
		FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5I_register
 *
 * Purpose:	Registers an OBJECT in a TYPE and returns an ID for it.
 *		This routine does _not_ check for unique-ness of the objects,
 *		if you register an object twice, you will get two different
 *		IDs for it.  This routine does make certain that each ID in a
 *		type is unique.  IDs are created by getting a unique number
 *		for the type the ID is in and incorporating the type into
 *		the ID which is returned to the user.
 *
 * Return:	Success:	New object id.
 *
 *		Failure:	Negative
 *
 * Programmer:	Unknown
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5I_register(H5I_type_t type, void *object)
{
    H5I_id_type_t	*type_ptr=NULL;	/*ptr to the type		*/
    H5I_id_info_t	*id_ptr=NULL;	/*ptr to the new ID information */
    hid_t		new_id;		/*new ID			*/
    unsigned		hash_loc;	/*new item's hash table location*/
    hid_t		next_id;	/*next ID to check		*/
    hid_t		ret_value=SUCCEED; /*return value		*/
    H5I_id_info_t	*curr_id;	/*ptr to the current atom	*/
    unsigned		i;		/*counter			*/

    FUNC_ENTER_NOAPI(H5I_register, FAIL);

    /* Check arguments */
    if (type <= H5I_BADID || type >= H5I_next_type)
	HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "invalid type number");
    type_ptr = H5I_id_type_list_g[type];
    if (type_ptr == NULL || type_ptr->count <= 0)
	HGOTO_ERROR(H5E_ATOM, H5E_BADGROUP, FAIL, "invalid type");
    if ((id_ptr = H5FL_MALLOC(H5I_id_info_t)) == NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* Create the struct & it's ID */
    new_id = H5I_MAKE(type, type_ptr->nextid);
    id_ptr->id = new_id;
    id_ptr->count = 1; /*initial reference count*/
    id_ptr->obj_ptr = object;
    id_ptr->next = NULL;

    /* hash bucket already full, prepend to front of chain */
    hash_loc = type_ptr->nextid % (unsigned) type_ptr->hash_size;
    if (type_ptr->id_list[hash_loc] != NULL)
	id_ptr->next = type_ptr->id_list[hash_loc];

    /* Insert into the type */
    type_ptr->id_list[hash_loc] = id_ptr;
    type_ptr->ids++;
    type_ptr->nextid++;

    /*
     * This next section of code checks for the 'nextid' getting too large and
     * wrapping around, thus necessitating checking for duplicate IDs being
     * handed out.
     */
    if (type_ptr->nextid > (unsigned)ID_MASK) {
	type_ptr->wrapped = 1;
	type_ptr->nextid = type_ptr->reserved;
    }

    /*
     * If we've wrapped around then we need to check for duplicate id's being
     * handed out.
     */
    if (type_ptr->wrapped) {
	/*
	 * Make sure we check all available ID's.  If we're about at the end
	 * of the range then wrap around and check the beginning values.  If
	 * we check all possible values and didn't find any free ones *then*
	 * we can fail.
	 */
	for (i=type_ptr->reserved; i<ID_MASK; i++) {
	    /* Handle end of range by wrapping to beginning */
	    if (type_ptr->nextid>(unsigned)ID_MASK)
		type_ptr->nextid = type_ptr->reserved;

	    /* new ID to check for */
	    next_id = H5I_MAKE(type, type_ptr->nextid);
	    hash_loc = H5I_LOC (type_ptr->nextid, type_ptr->hash_size);
	    curr_id = type_ptr->id_list[hash_loc];
	    if (curr_id == NULL)
                break; /* Ha! this is not likely... */

	    while (curr_id) {
		if (curr_id->id == next_id)
                    break;
		curr_id = curr_id->next;
	    }
	    if (!curr_id)
                break; /* must not have found a match */
	    type_ptr->nextid++;
	}

	if (i>=(unsigned)ID_MASK)
	    /* All the IDs are gone! */
            HGOTO_ERROR(H5E_ATOM, H5E_NOIDS, FAIL, "no IDs available in type");
    }
    ret_value = new_id;

  done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5I_object
 *
 * Purpose:	Find an object pointer for the specified ID.
 *
 * Return:	Success:	Non-null object pointer associated with the
 *				specified ID.
 *
 *		Failure:	NULL
 *
 * Programmer:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *
H5I_object(hid_t id)
{
    H5I_id_info_t	*id_ptr = NULL;		/*ptr to the new atom	*/
    void		*ret_value = NULL;	/*return value		*/

    FUNC_ENTER_NOAPI(H5I_object, NULL);

    /* General lookup of the ID */
    if (NULL!=(id_ptr = H5I_find_id(id))) {
        /* Get the object pointer to return */
        ret_value = id_ptr->obj_ptr;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Iobject_verify
 *
 * Purpose:	Find an object pointer for the specified ID, verifying that
 *                  its in a particular type.  Public interface to
 *					H5I_object_verify.
 *
 * Return:	Success:	Non-null object pointer associated with the
 *				specified ID.
 *
 *		Failure:	NULL
 *
 * Programmer:	Nathaniel Furrer
 *		James Laird
 *		Friday, April 23, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *H5Iobject_verify(hid_t id, H5I_type_t id_type)
{
  void * ret_value;                      /* Return value */

  FUNC_ENTER_API(H5Iobject_verify, NULL);

  if( H5I_IS_LIB_TYPE( id_type ) )
    HGOTO_ERROR(H5E_ATOM, H5E_BADGROUP, NULL, "cannot call public function on library type")

  if(id_type < 1 || id_type >= H5I_next_type)
    HGOTO_ERROR(H5E_ATOM, H5E_BADGROUP, NULL, "identifier has invalid type")

  ret_value = H5I_object_verify(id, id_type);

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5I_object_verify
 *
 * Purpose:	Find an object pointer for the specified ID, verifying that
 *                  its in a particular type.
 *
 * Return:	Success:	Non-null object pointer associated with the
 *				specified ID.
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, July 31, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *
H5I_object_verify(hid_t id, H5I_type_t id_type)
{
    H5I_id_info_t	*id_ptr = NULL;		/*ptr to the new atom	*/
    void		*ret_value = NULL;	/*return value		*/

    FUNC_ENTER_NOAPI(H5I_object_verify, NULL);

    assert(id_type>=1 && id_type<H5I_next_type);

    /* Verify that the type of the ID is correct & lookup the ID */
    if(id_type == H5I_TYPE(id) && NULL!=(id_ptr = H5I_find_id(id))) {
        /* Get the object pointer to return */
        ret_value = id_ptr->obj_ptr;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* H5I_object_verify() */


/*-------------------------------------------------------------------------
 * Function:	H5I_get_type
 *
 * Purpose:	Given an object ID return the type to which it
 *		belongs.  The ID need not be the ID of an object which
 *		currently exists because the type number is encoded
 *		in the object ID.
 *
 * Return:	Success:	A valid type number
 *
 *		Failure:	H5I_BADID, a negative value.
 *
 * Programmer:	Robb Matzke
 *		Friday, February 19, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5I_type_t
H5I_get_type(hid_t id)
{
    H5I_type_t		ret_value = H5I_BADID;

    FUNC_ENTER_NOAPI(H5I_get_type, H5I_BADID);

    if (id>0)
        ret_value = H5I_TYPE(id);

    assert(ret_value>=H5I_BADID && ret_value<H5I_next_type);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Iget_type
 *
 * Purpose:	The public version of H5I_get_type(), obtains a type number
 *		when given an ID.  The ID need not be the ID of an
 *		object which currently exists because the type number is
 *		encoded as part of the ID.
 *
 * Return:	Success:	Type number
 *
 *		Failure:	H5I_BADID, a negative value
 *
 * Programmer:
 *
 * Modifications:
 *		Robb Matzke, 1999-08-23
 *		Also fails if the ID has a valid type but no longer exists
 *		in the ID tables.
 *-------------------------------------------------------------------------
 */
H5I_type_t
H5Iget_type(hid_t id)
{
    H5I_type_t		ret_value = H5I_BADID;

    FUNC_ENTER_API(H5Iget_type, H5I_BADID);
    H5TRACE1("It", "i", id);

    ret_value = H5I_get_type(id);

    if (ret_value <= H5I_BADID || ret_value >= H5I_next_type || NULL==H5I_object(id))
	HGOTO_DONE(H5I_BADID);

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Iremove_verify
 *
 * Purpose:	Removes the specified ID from its type, first checking that the
 *			type of the ID and the type type are the same.  Public interface to
 *			H5I_remove_verify.
 *
 * Return:	Success:	A pointer to the object that was removed, the
 *				same pointer which would have been found by
 *				calling H5I_object().
 *
 *		Failure:	NULL
 *
 * Programmer:	James Laird
 *				Nathaniel Furrer
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *H5Iremove_verify(hid_t id, H5I_type_t id_type)
{
	void * ret_value;                      /* Return value */

    FUNC_ENTER_API(H5Iremove_verify, NULL);

	if( H5I_IS_LIB_TYPE( id_type ) )
		HGOTO_ERROR(H5E_ATOM, H5E_BADGROUP, NULL, "cannot call public function on library type")

	/* Remove the id */
	ret_value = H5I_remove_verify(id, id_type);

	done:
		FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5I_remove_verify
 *
 * Purpose:	Removes the specified ID from its type, first checking that
 *			the ID's type is the same as the ID type supplied as an argument
 *
 * Return:	Success:	A pointer to the object that was removed, the
 *				same pointer which would have been found by
 *				calling H5I_object().
 *
 *		Failure:	NULL
 *
 * Programmer:	James Laird
 *				Nat Furrer
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *
H5I_remove_verify(hid_t id, H5I_type_t id_type)
{
    void * ret_value = NULL;	/*return value			*/

    FUNC_ENTER_NOAPI(H5I_remove_verify, NULL);

	/* Argument checking will be performed by H5I_remove() */

    /* Verify that the type of the ID is correct */
    if(id_type == H5I_TYPE(id))
	{
        ret_value = H5I_remove(id);
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}



/*-------------------------------------------------------------------------
 * Function:	H5I_remove
 *
 * Purpose:	Removes the specified ID from its type.
 *
 * Return:	Success:	A pointer to the object that was removed, the
 *				same pointer which would have been found by
 *				calling H5I_object().
 *
 *		Failure:	NULL
 *
 * Programmer:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *
H5I_remove(hid_t id)
{
    H5I_id_type_t	*type_ptr = NULL;/*ptr to the atomic type	*/
    H5I_id_info_t	*curr_id;	/*ptr to the current atom	*/
    H5I_id_info_t	*last_id;	/*ptr to the last atom		*/
    H5I_type_t		type;		/*atom's atomic type		*/
    unsigned		hash_loc;	/*atom's hash table location	*/
    void *	      ret_value = NULL;	/*return value			*/

    FUNC_ENTER_NOAPI(H5I_remove, NULL);

    /* Check arguments */
    type = H5I_TYPE(id);
    if (type <= H5I_BADID || type >= H5I_next_type)
	HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, NULL, "invalid type number");
    type_ptr = H5I_id_type_list_g[type];
    if (type_ptr == NULL || type_ptr->count <= 0)
	HGOTO_ERROR(H5E_ATOM, H5E_BADGROUP, NULL, "invalid type");

    /* Get the bucket in which the ID is located */
    hash_loc = (unsigned) H5I_LOC(id, type_ptr->hash_size);
    curr_id = type_ptr->id_list[hash_loc];
    if (curr_id == NULL)
	HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "invalid ID");

    last_id = NULL;
    while (curr_id != NULL) {
        if (curr_id->id == id)
            break;
        last_id = curr_id;
        curr_id = curr_id->next;
    }

    if (curr_id != NULL) {
        if (last_id == NULL) {
            /* ID is the first in the chain */
            type_ptr->id_list[hash_loc] = curr_id->next;
        } else {
            last_id->next = curr_id->next;
        }
        ret_value = curr_id->obj_ptr;
        H5FL_FREE(H5I_id_info_t,curr_id);
    } else {
        /* couldn't find the ID in the proper place */
	HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, NULL, "invalid ID");
    }

    /* Decrement the number of IDs in the type */
    (type_ptr->ids)--;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Idec_ref
 *
 * Purpose:	Decrements the number of references outstanding for an ID.
 *              If the reference count for an ID reaches zero, the object
 *              will be closed.
 *
 * Return:	Success:	New reference count
 *		Failure:	Negative
 *
 * Programmer:  Quincey Koziol
 *              Dec  7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5Idec_ref(hid_t id)
{
    int ret_value;                      /* Return value */

    FUNC_ENTER_API_META(H5Idec_ref, id, H5AC_dxpl_id, FAIL);
    H5TRACE1("Is", "i", id);

    /* Check arguments */
    if (id<0)
	HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "invalid ID");

    /* Do actual decrement operation */
    if((ret_value = H5I_dec_ref(id))<0)
        HGOTO_ERROR (H5E_ATOM, H5E_CANTDEC, FAIL, "can't decrement ID ref count");

done:
    FUNC_LEAVE_API_META(ret_value);
} /* end H5Idec_ref() */


/*-------------------------------------------------------------------------
 * Function:	H5I_dec_ref
 *
 * Purpose:	Decrements the number of references outstanding for an ID.
 *		This will fail if the type is not a reference counted type.
 *		The ID type's 'free' function will be called for the ID
 *		if the reference count for the ID reaches 0 and a free
 *		function has been defined at type creation time.
 *
 * Return:	Success:	New reference count.
 *
 *		Failure:	Negative
 *
 * Programmer:	Unknown
 *
 * Modifications:
 *
 *	Robb Matzke, 19 Feb 1998
 *	It is no longer an error when the reference count of an item reaches
 *	zero and no `free' function has been defined.  The object is still
 *	removed from the list.
 *
 *	Robb Matzke, 30 Dec 1998
 *	Fixed a bug where the return value was always zero instead of the new
 *	reference count.
 *
 *	Robb Matzke, 19 Feb 1999
 *	If the free method is defined and fails then the object is not
 *	removed from the type and its reference count is not decremented.
 *	The type number is now passed to the free method.
 *
 *	Raymond, 11 Dec 2001
 *	If the freeing function fails, return failure instead of reference
 *	count 1.  This feature is needed by file close with H5F_CLOSE_SEMI
 *	value.
 *
 *-------------------------------------------------------------------------
 */
int
H5I_dec_ref(hid_t id)
{
    H5I_type_t		type;		/*type the object is in*/
    H5I_id_type_t	*type_ptr;	/*ptr to the type	*/
    H5I_id_info_t	*id_ptr;	/*ptr to the new ID	*/
    int ret_value;                      /* Return value */

    FUNC_ENTER_NOAPI(H5I_dec_ref, FAIL);

    /* Sanity check */
    assert(id>=0);

    /* Check arguments */
    type = H5I_TYPE(id);
    if (type <= H5I_BADID || type >= H5I_next_type)
	HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "invalid type number");
    type_ptr = H5I_id_type_list_g[type];
    if (type_ptr == NULL || type_ptr->count <= 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "invalid type number");

    /* General lookup of the ID */
    if ((id_ptr=H5I_find_id(id))==NULL)
	HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't locate ID");

    /*
     * If this is the last reference to the object then invoke the type's
     * free method on the object. If the free method is undefined or
     * successful then remove the object from the type; otherwise leave
     * the object in the type without decrementing the reference
     * count. If the reference count is more than one then decrement the
     * reference count without calling the free method.
     *
     * Beware: the free method may call other H5I functions.
     */
    if (1==id_ptr->count) {
        if (!type_ptr->free_func || (type_ptr->free_func)(id_ptr->obj_ptr)>=0) {
            H5I_remove(id);
            ret_value = 0;
        } else {
            ret_value = FAIL;
        }
    } else {
        ret_value = --(id_ptr->count);
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Iinc_ref
 *
 * Purpose:	Increments the number of references outstanding for an ID.
 *
 * Return:	Success:	New reference count
 *		Failure:	Negative
 *
 * Programmer:  Quincey Koziol
 *              Dec  7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5Iinc_ref(hid_t id)
{
    int ret_value;                      /* Return value */

    FUNC_ENTER_API_META(H5Iinc_ref, id, H5AC_dxpl_id, FAIL);
    H5TRACE1("Is", "i", id);

    /* Check arguments */
    if (id<0)
	HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "invalid ID");

    /* Do actual increment operation */
    if((ret_value = H5I_inc_ref(id))<0)
        HGOTO_ERROR (H5E_ATOM, H5E_CANTINC, FAIL, "can't increment ID ref count");

done:
    FUNC_LEAVE_API_META(ret_value);
} /* end H5Iinc_ref() */


/*-------------------------------------------------------------------------
 * Function:	H5I_inc_ref
 *
 * Purpose:	Increment the reference count for an object.
 *
 * Return:	Success:	The new reference count.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 29, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5I_inc_ref(hid_t id)
{
    H5I_type_t		type;		/*type the object is in*/
    H5I_id_type_t	*type_ptr;	/*ptr to the type	*/
    H5I_id_info_t	*id_ptr;	/*ptr to the ID		*/
    int ret_value;                      /* Return value */

    FUNC_ENTER_NOAPI(H5I_inc_ref, FAIL);

    /* Sanity check */
    assert(id>=0);

    /* Check arguments */
    type = H5I_TYPE(id);
    if (type <= H5I_BADID || type >= H5I_next_type)
	HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "invalid type number");
    type_ptr = H5I_id_type_list_g[type];
    if (!type_ptr || type_ptr->count<=0)
	HGOTO_ERROR(H5E_ATOM, H5E_BADGROUP, FAIL, "invalid type");

    /* General lookup of the ID */
    if (NULL==(id_ptr=H5I_find_id(id)))
	HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't locate ID");

    /* Set return value */
    ret_value=++(id_ptr->count);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Iget_ref
 *
 * Purpose:	Retrieves the number of references outstanding for an ID.
 *
 * Return:	Success:	Reference count
 *		Failure:	Negative
 *
 * Programmer:  Quincey Koziol
 *              Dec  7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5Iget_ref(hid_t id)
{
    int ret_value;                      /* Return value */

    FUNC_ENTER_API(H5Iget_ref, FAIL);
    H5TRACE1("Is", "i", id);

    /* Check arguments */
    if (id<0)
	HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "invalid ID");

    /* Do actual retrieve operation */
    if((ret_value = H5I_get_ref(id))<0)
        HGOTO_ERROR (H5E_ATOM, H5E_CANTGET, FAIL, "can't get ID ref count");

done:
    FUNC_LEAVE_API(ret_value);
} /* end H5Iget_ref() */


/*-------------------------------------------------------------------------
 * Function:	H5I_get_ref
 *
 * Purpose:	Retrieve the reference count for an object.
 *
 * Return:	Success:	The reference count.
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Saturday, Decemeber  6, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5I_get_ref(hid_t id)
{
    H5I_type_t		type;		/*type the object is in*/
    H5I_id_type_t	*type_ptr;	/*ptr to the type	*/
    H5I_id_info_t	*id_ptr;	/*ptr to the ID		*/
    int ret_value;                      /* Return value */

    FUNC_ENTER_NOAPI(H5I_get_ref, FAIL);

    /* Sanity check */
    assert(id>=0);

    /* Check arguments */
    type = H5I_TYPE(id);
    if (type <= H5I_BADID || type >= H5I_next_type)
	HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "invalid type number");
    type_ptr = H5I_id_type_list_g[type];
    if (!type_ptr || type_ptr->count<=0)
	HGOTO_ERROR(H5E_ATOM, H5E_BADGROUP, FAIL, "invalid type");

    /* General lookup of the ID */
    if (NULL==(id_ptr=H5I_find_id(id)))
	HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't locate ID");

    /* Set return value */
    ret_value=id_ptr->count;

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5I_get_ref() */


/*-------------------------------------------------------------------------
 * Function:	H5Iinc_type_ref
 *
 * Purpose:	Increments the number of references outstanding for an ID type.
 *
 * Return:	Success:	New reference count
 *		Failure:	Negative
 *
 * Programmer:  Nat Furrer
 *				James Laird
 *              April 30, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5Iinc_type_ref(H5I_type_t type)
{
    int ret_value;                      /* Return value */

    FUNC_ENTER_API(H5Iinc_type_ref, FAIL);
    H5TRACE1("Is", "It", type);

    /* Check arguments */
    if (type<=0 || type >= H5I_next_type)
		HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "invalid ID type");

	if( H5I_IS_LIB_TYPE( type ) )
	{
		HGOTO_ERROR(H5E_ATOM, H5E_BADGROUP, FAIL, "cannot call public function on library type");
	}

    /* Do actual increment operation */
    if((ret_value = H5I_inc_type_ref(type))<0)
        HGOTO_ERROR (H5E_ATOM, H5E_CANTINC, FAIL, "can't increment ID type ref count");

done:
    FUNC_LEAVE_API(ret_value);
} /* end H5Iinc_ref() */


/*-------------------------------------------------------------------------
 * Function:	H5I_inc_type_ref
 *
 * Purpose:	Increment the reference count for an ID type.
 *
 * Return:	Success:	The new reference count.
 *
 *		Failure:	Negative
 *
 * Programmer:	James Laird
 *				Nat Furrer
 *              Friday, April 30, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5I_inc_type_ref(H5I_type_t type)
{
    H5I_id_type_t	*type_ptr;	/* ptr to the type	*/
    int ret_value;                      /* Return value */

    FUNC_ENTER_NOAPI(H5I_inc_type_ref, FAIL);

    /* Sanity check */
    assert(type>0 && type < H5I_next_type);

    /* Check arguments */
    type_ptr = H5I_id_type_list_g[type];
    if (!type_ptr )
	HGOTO_ERROR(H5E_ATOM, H5E_BADGROUP, FAIL, "invalid type");

    /* Set return value */
    ret_value=++(type_ptr->count);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Idec_type_ref
 *
 * Purpose:	Decrements the reference count on an entire type of IDs.
 *		If the type reference count becomes zero then the type is
 *		destroyed along with all atoms in that type regardless of
 *		their reference counts.	 Destroying IDs involves calling
 *		the free-func for each ID's object and then adding the ID
 *		struct to the ID free list.  Public interface to
 *		H5I_dec_type_ref.
 *		Returns the number of references to the type on success; a
 *		return value of 0 means that the type will have to be
 *		re-initialized before it can be used again (and should probably
 *		be set to H5I_UNINIT).
 *
 * Return:	Number of references to type on success/Negative on failure
 *
 * Programmer:	Nathaniel Furrer
 *				James Laird
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t H5Idec_type_ref(H5I_type_t type)
{
	herr_t ret_value;

	FUNC_ENTER_API(H5Idec_type_ref, FAIL);

	if( H5I_IS_LIB_TYPE( type ) )
	{
		HGOTO_ERROR(H5E_ATOM, H5E_BADGROUP, FAIL, "cannot call public function on library type");
	}

	ret_value = H5I_dec_type_ref(type);

	done:
		FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5I_dec_type_ref
 *
 * Purpose:	Decrements the reference count on an entire type of IDs.
 *		If the type reference count becomes zero then the type is
 *		destroyed along with all atoms in that type regardless of
 *		their reference counts.	 Destroying IDs involves calling
 *		the free-func for each ID's object and then adding the ID
 *		struct to the ID free list.
 *		Returns the number of references to the type on success; a
 *		return value of 0 means that the type will have to be
 *		re-initialized before it can be used again (and should probably
 *		be set to H5I_UNINIT).
 *
 * Return:	Number of references to type on success/Negative on failure
 *
 * Programmer:	Unknown
 *
 * Modifications:
 *
 *	Robb Matzke, 25 Feb 1998
 *	IDs are freed when a type is destroyed.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5I_dec_type_ref(H5I_type_t type)
{
    H5I_id_type_t	*type_ptr = NULL;	/* ptr to the atomic type */
    herr_t		ret_value = FAIL;

    FUNC_ENTER_NOAPI(H5I_dec_type_ref, FAIL);

    if (type <= H5I_BADID || type >= H5I_next_type)
	HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "invalid type number");

    type_ptr = H5I_id_type_list_g[type];
    if (type_ptr == NULL || type_ptr->count <= 0)
	HGOTO_ERROR(H5E_ATOM, H5E_BADGROUP, FAIL, "invalid type");

    /*
     * Decrement the number of users of the atomic type.  If this is the
     * last user of the type then release all atoms from the type and
	 * free all memory it used.  The free function is invoked for each atom
	 * being freed.
     */
    if (1==type_ptr->count)
	{
		H5I_destroy_type(type);
		ret_value = 0;
    }
	else
	{
        --(type_ptr->count);
		ret_value = type_ptr->count;
    }

  done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Iget_type_ref
 *
 * Purpose:	Retrieves the number of references outstanding for a type.
 *
 * Return:	Success:	Reference count
 *		Failure:	Negative
 *
 * Programmer:  Nat Furrer
 *				James Laird
 *              April 30, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5Iget_type_ref(H5I_type_t type)
{
    int ret_value;                      /* Return value */

    FUNC_ENTER_API(H5Iget_type_ref, FAIL);
    H5TRACE1("Is", "It", type);

    /* Check arguments */
    if (type<=0 || type >= H5I_next_type)
		HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "invalid ID type");

	if( H5I_IS_LIB_TYPE( type ) )
	{
		HGOTO_ERROR(H5E_ATOM, H5E_BADGROUP, FAIL, "cannot call public function on library type");
	}

    /* Do actual retrieve operation */
    if((ret_value = H5I_get_type_ref(type))<0)
        HGOTO_ERROR (H5E_ATOM, H5E_CANTGET, FAIL, "can't get ID type ref count");

done:
    FUNC_LEAVE_API(ret_value);
} /* end H5Iget_ref() */


/*-------------------------------------------------------------------------
 * Function:	H5I_get_type_ref
 *
 * Purpose:	Retrieve the reference count for an ID type.
 *
 * Return:	Success:	The reference count.
 *
 *		Failure:	Negative
 *
 * Programmer:  Nat Furrer
 *				James Laird
 *              April 30, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5I_get_type_ref(H5I_type_t type)
{
    H5I_id_type_t	*type_ptr;	/*ptr to the type	*/
    int ret_value;                      /* Return value */

    FUNC_ENTER_NOAPI(H5I_get_type_ref, FAIL);

    /* Sanity check */
    assert(type>=0);

    /* Check arguments */
    type_ptr = H5I_id_type_list_g[type];
    if (!type_ptr )
		HGOTO_ERROR(H5E_ATOM, H5E_BADGROUP, FAIL, "invalid type");

    /* Set return value */
    ret_value=type_ptr->count;

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5I_get_ref() */


/*-------------------------------------------------------------------------
 * Function:	H5Isearch
 *
 * Purpose:	Apply function FUNC to each member of type TYPE and return a
 *		pointer to the first object for which FUNC returns non-zero.
 *		The FUNC should take a pointer to the object and the KEY as
 *		arguments and return non-zero to terminate the search (zero
 *		to continue).  Public interface to H5I_search.
 *
 * Limitation:	Currently there is no way to start searching from where a
 *		previous search left off.
 *
 * Return:	Success:	The first object in the type for which FUNC
 *				returns non-zero. NULL if FUNC returned zero
 *				for every object in the type.
 *
 *		Failure:	NULL
 *
 * Programmer:	James Laird
 *		Nathaniel Furrer
 *		Friday, April 23, 2004
 *
 *-------------------------------------------------------------------------
 */
void *H5Isearch(H5I_type_t type, H5I_search_func_t func, void *key)
{
    void * ret_value;                      /* Return value */

    FUNC_ENTER_API(H5Isearch, NULL)

    if( H5I_IS_LIB_TYPE( type ) )
        HGOTO_ERROR(H5E_ATOM, H5E_BADGROUP, NULL, "cannot call public function on library type")

    ret_value = H5I_search(type, func, key);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Isearch() */


/*-------------------------------------------------------------------------
 * Function:	H5I_search
 *
 * Purpose:	Apply function FUNC to each member of type TYPE and return a
 *		pointer to the first object for which FUNC returns non-zero.
 *		The FUNC should take a pointer to the object and the KEY as
 *		arguments and return non-zero to terminate the search (zero
 *		to continue).
 *
 * Limitation:	Currently there is no way to start searching from where a
 *		previous search left off.
 *
 * Return:	Success:	The first object in the type for which FUNC
 *				returns non-zero. NULL if FUNC returned zero
 *				for every object in the type.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Friday, February 19, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *
H5I_search(H5I_type_t type, H5I_search_func_t func, void *key)
{
    H5I_id_type_t	*type_ptr = NULL;	/*ptr to the type	*/
    H5I_id_info_t	*id_ptr = NULL;		/*ptr to the new ID	*/
    H5I_id_info_t	*next_id = NULL;	/*ptr to the next ID	*/
    unsigned		i;			/*counter		*/
    void		*ret_value = NULL;	/*return value		*/

    FUNC_ENTER_NOAPI(H5I_search, NULL);

    /* Check arguments */
    if (type <= H5I_BADID || type >= H5I_next_type)
	HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, NULL, "invalid type number");
    type_ptr = H5I_id_type_list_g[type];
    if (type_ptr == NULL || type_ptr->count <= 0)
	HGOTO_ERROR(H5E_ATOM, H5E_BADGROUP, NULL, "invalid type");

    /* Only iterate through hash table if there are IDs in group */
    if(type_ptr->ids > 0) {
        /* Start at the beginning of the array */
        for (i=0; i<type_ptr->hash_size; i++) {
            id_ptr = type_ptr->id_list[i];
            while (id_ptr) {
                next_id= id_ptr->next;      /* Protect against ID being deleted in callback */
                if ((*func)(id_ptr->obj_ptr, id_ptr->id, key))
                    HGOTO_DONE(id_ptr->obj_ptr);	/*found the item*/
                id_ptr = next_id;
            } /* end while */
        } /* end for */
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5I_search() */


/*-------------------------------------------------------------------------
 * Function:	H5I_find_id
 *
 * Purpose:	Given an object ID find the info struct that describes the
 *		object.
 *
 * Return:	Success:	Ptr to the object's info struct.
 *
 *		Failure:	NULL
 *
 * Programmer:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5I_id_info_t *
H5I_find_id(hid_t id)
{
    H5I_id_type_t	*type_ptr;		/*ptr to the type	*/
    H5I_id_info_t	*last_id;		/*ptr to the last ID	*/
    H5I_id_info_t	*id_ptr;		/*ptr to the new ID	*/
    H5I_type_t		type;			/*ID's type		*/
    unsigned		hash_loc;		/*bucket pointer	*/
    H5I_id_info_t	*ret_value = NULL;	/*return value		*/

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5I_find_id);

    /* Check arguments */
    type = H5I_TYPE(id);
    assert(type > H5I_BADID && type < H5I_next_type);
    type_ptr = H5I_id_type_list_g[type];

    assert(type_ptr && type_ptr->count > 0);

    /* Get the bucket in which the ID is located */
    hash_loc = (unsigned)H5I_LOC(id, type_ptr->hash_size);
    id_ptr = type_ptr->id_list[hash_loc];

    /* Scan the bucket's linked list for a match */
    last_id=NULL;
    while (id_ptr) {
	if (id_ptr->id == id) {
            /* If we found an object, move it to the front of the list, if it isn't there already */
            if(last_id!=NULL) {
                last_id->next=id_ptr->next;
                id_ptr->next=type_ptr->id_list[hash_loc];
                type_ptr->id_list[hash_loc]=id_ptr;
            } /* end if */
            break;
        } /* end if */
        last_id=id_ptr;
	id_ptr = id_ptr->next;
    } /* end while */

    /* Set the return value */
    ret_value = id_ptr;

    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function: H5Iget_name
 *
 * Purpose: Gets a name of an object from its ID.
 *
 * Return: Success: The length of name.
 *
 *         Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: July 26, 2002
 *
 * Comments: Public function
 *  If `name' is non-NULL then write up to `size' bytes into that
 *  buffer and always return the length of the entry name.
 *  Otherwise `size' is ignored and the function does not store the name,
 *  just returning the number of characters required to store the name.
 *  If an error occurs then the buffer pointed to by `name' (NULL or non-NULL)
 *  is unchanged and the function returns a negative value.
 *  If a zero is returned for the name's length, then there is no name
 *  associated with the ID.
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Iget_name(hid_t id, char *name/*out*/, size_t size)
{
    ssize_t       ret_value;

    FUNC_ENTER_API(H5Iget_name, FAIL)
    H5TRACE3("Zs", "ixz", id, name, size);

    /* Call internal group routine to retrieve object's name */
    if((ret_value = H5G_get_name(id, name, size, H5P_DEFAULT, H5AC_ind_dxpl_id)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTGET, FAIL, "can't retrieve object name")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Iget_name() */


/*-------------------------------------------------------------------------
 * Function:	H5Iget_file_id
 *
 * Purpose:	The public version of H5I_get_file_id(), obtains the file
 *              ID given an object ID.  User has to close this ID.
 *
 * Return:	Success:	file ID
 *
 *		Failure:	a negative value
 *
 * Programmer:  Raymond Lu
 *              Oct 27, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Iget_file_id(hid_t obj_id)
{
    hid_t		ret_value;

    FUNC_ENTER_API(H5Iget_file_id, FAIL);
    H5TRACE1("i", "i", obj_id);

    if((ret_value = H5I_get_file_id(obj_id))<0)
        HGOTO_ERROR (H5E_ATOM, H5E_CANTGET, FAIL, "can't retrieve file ID");

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5I_get_file_id
 *
 * Purpose:	The private version of H5Iget_file_id(), obtains the file
 *              ID given an object ID.
 *
 * Return:	Success:	file ID
 *
 *		Failure:	a negative value
 *
 * Programmer:  Raymond Lu
 *              Oct 27, 2003
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5I_get_file_id(hid_t obj_id)
{
    H5G_loc_t loc;              /* Location of object */
    H5I_type_t type;            /* ID type */
    hid_t ret_value;            /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5I_get_file_id)

    /* Get object type */
    type = H5I_TYPE(obj_id);
    if(type == H5I_FILE) {
        ret_value = obj_id;

        /* Increment reference count on atom. */
        if(H5I_inc_ref(ret_value) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTSET, FAIL, "incrementing file ID failed")
    }
    else if(type == H5I_DATATYPE || type == H5I_GROUP || type == H5I_DATASET || type == H5I_ATTR) {
        if(H5G_loc(obj_id, &loc) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTGET, FAIL, "can't get object location")
        if((ret_value = H5F_get_id(loc.oloc->file)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTGET, FAIL, "can't get file ID")
    }
    else
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "invalid object ID")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5I_get_file_id() */


/*-------------------------------------------------------------------------
 * Function: H5I_debug
 *
 * Purpose: Dump the contents of a type to stderr for debugging.
 *
 * Return: Success: Non-negative
 *
 *   Failure: Negative
 *
 * Programmer: Robb Matzke
 *  Friday, February 19, 1999
 *
 * Modifications:
 *
 *      Pedro Vicente, <pvn@ncsa.uiuc.edu> 22 Aug 2002
 *      Added `id to name' support.
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5I_DEBUG_OUTPUT
static herr_t
H5I_debug(H5I_type_t type)
{
    H5I_id_type_t *type_ptr;
    H5I_id_info_t *cur;
    H5G_name_t *path;
    int   is, js;
    unsigned int iu;
    herr_t ret_value;  /* Return value */

    FUNC_ENTER_NOAPI(H5I_debug, FAIL);

    fprintf(stderr, "Dumping ID type %d\n", (int)type);
    type_ptr = H5I_id_type_list_g[type];

    /* Header */
    fprintf(stderr, "	 count	   = %u\n", type_ptr->count);
    fprintf(stderr, "	 reserved  = %u\n", type_ptr->reserved);
    fprintf(stderr, "	 wrapped   = %u\n", type_ptr->wrapped);
    fprintf(stderr, "	 hash_size = %lu\n", (unsigned long)type_ptr->hash_size);
    fprintf(stderr, "	 ids	   = %u\n", type_ptr->ids);
    fprintf(stderr, "	 nextid	   = %u\n", type_ptr->nextid);

    /* Cache */
    fprintf(stderr, "	 Cache:\n");
    for (is=0; is<ID_CACHE_SIZE; is++) {
        if (H5I_cache_g[is] && H5I_TYPE(H5I_cache_g[is]->id)==type) {
            fprintf(stderr, "	     Entry-%d, ID=%lu\n",
                    is, (unsigned long)(H5I_cache_g[is]->id));
        }
    }

    /* List */
    fprintf(stderr, "	 List:\n");
    for (iu=0; iu<type_ptr->hash_size; iu++) {
        for (js=0, cur=type_ptr->id_list[iu]; cur; cur=cur->next, js++) {
            fprintf(stderr, "	     #%u.%d\n", iu, js);
            fprintf(stderr, "		 id = %lu\n", (unsigned long)(cur->id));
            fprintf(stderr, "		 count = %u\n", cur->count);
            fprintf(stderr, "		 obj   = 0x%08lx\n", (unsigned long)(cur->obj_ptr));

            /* Get the group location, so we get get the name */
            switch(type) {
                case H5I_GROUP:
                    path = H5G_nameof((H5G_t*)cur->obj_ptr);
                    break;
                case H5I_DATASET:
                    path = H5D_nameof((H5D_t*)cur->obj_ptr);
                    break;
                case H5I_DATATYPE:
                    path = H5T_nameof((H5T_t*)cur->obj_ptr);
                    break;
                default:
                    continue;   /* Other types of IDs are not stored in files */
            } /* end switch*/

            if(path) {
                if(path->user_path_r)
                    fprintf(stderr, "                user_path = %s\n", H5RS_get_str(path->user_path_r));
                if(ent->canon_path_r)
                    fprintf(stderr, "                canon_path = %s\n", H5RS_get_str(path->canon_path_r));
            } /* end if */
        } /* end for */
    } /* end for */

done:
    FUNC_LEAVE_NOAPI(SUCCEED);
}
#endif /* H5I_DEBUG_OUTPUT */

