/****************************************************************************
 * NCSA HDF								    *
 * Software Development Group						    *
 * National Center for Supercomputing Applications			    *
 * University of Illinois at Urbana-Champaign				    *
 * 605 E. Springfield, Champaign IL 61820				    *
 *									    *
 * For conditions of distribution and use, see the accompanying		    *
 * hdf/COPYING file.							    *
 *									    *
 ****************************************************************************/

#ifdef RCSID
static char		RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

/*
   FILE
   H5I.c - Internal storage routines for handling "IDs"

   REMARKS
   ID's which allow objects (void *'s currently) to be bundled into "groups"
   for more general storage.

   DESIGN
   The groups are stored in an array of pointers to store each group in an
   element. Each "group" node contains a link to a hash table to manage the IDs
   in each group.  The allowed "groups" are stored in an enum (called group_t)
   in H5Ipublic.h.

   BUGS/LIMITATIONS
   Can't interate over the IDs in a group.

   LOCAL ROUTINES
   H5I_find_id      - Returns a pointer to an HAI_id_info_t from an ID
   H5I_get_id_node  - Gets an ID node (uses the ID free list)
   H5I_release_id_node - Releases an ID node (uses the ID free list)
   EXPORTED ROUTINES
   ID Functions:
   H5I_register	      - Register an object in a group and get an ID for it
   H5I_object	      - Get the object for an ID
   H5I_get_type	      - Get the group for an ID
   H5I_remove	      - Remove an ID from a group
   H5I_search	      - Search a group for a particular object
   ID Group Functions:
   H5I_init_group      - Initialize a group to store IDs in
   H5I_destroy_group   - Destroy an ID group
   ID Group Cleanup:
   H5Ishutdown	      - Terminate various static buffers.

   AUTHOR
   Quincey Koziol

   MODIFICATION HISTORY
   1/3/96  - Starting writing specs & coding prototype
   1/7/96  - Finished coding prototype
   6/10/97 - Moved into HDF5 library
 */
#include <H5private.h>
#include <H5Iprivate.h>
#include <H5Eprivate.h>
#include <H5MMprivate.h>

/* Interface initialialization? */
#define PABLO_MASK	H5I_mask
static hbool_t interface_initialize_g = FALSE;
#define INTERFACE_INIT H5I_init_interface
static herr_t H5I_init_interface(void);

/*
 * Define the following macro for fast hash calculations (but limited
 * hash sizes)
 */
#define HASH_SIZE_POWER_2

/* Define the following macro for atom caching over all the atoms */
#define IDS_ARE_CACHED

/*-------------------- Locally scoped variables -----------------------------*/

#ifdef IDS_ARE_CACHED
#  define ID_CACHE_SIZE 4             /*# of previous atoms cached         */
#endif

/*
 * Number of bits to use for Group ID in each atom. Increase if H5I_MAXID
 * becomes too large (an assertion would fail in H5I_init_interface). This is
 * the only number that must be changed since all other bit field sizes and
 * masks are calculated from GROUP_BITS.
 */
#define GROUP_BITS	5
#define GROUP_MASK	((1<<GROUP_BITS)-1)

/*
 * Number of bits to use for the Atom index in each atom (assumes 8-bit
 * bytes). We don't use the sign bit.
 */
#define ID_BITS		((sizeof(hid_t)*8)-(GROUP_BITS+1))
#define ID_MASK		((1<<ID_BITS)-1)

/* Map an atom to a Group number */
#define H5I_GROUP(a)	((H5I_type_t)(((hid_t)(a)>>ID_BITS) & GROUP_MASK))


#ifdef HASH_SIZE_POWER_2
/*
 * Map an ID to a hash location (assumes s is a power of 2 and smaller
 * than the ID_MASK constant).
 */
#  define H5I_LOC(a,s)	((hid_t)((size_t)(a)&((s)-1)))
#else
/*
 * Map an ID to a hash location.
 */
#  define H5I_LOC(a,s)	(((hid_t)(a)&ID_MASK)%(s))
#endif

/* Combine a Group number and an atom index into an atom */
#define H5I_MAKE(g,i)	((((hid_t)(g)&GROUP_MASK)<<ID_BITS)|      \
                             ((hid_t)(i)&ID_MASK))

#ifdef IDS_ARE_CACHED
/* Array of pointers to ID groups */
static hid_t H5I_id_cache[ID_CACHE_SIZE] = {-1, -1, -1, -1};
static void *H5I_obj_cache[ID_CACHE_SIZE];
#endif

/* Array of pointers to atomic groups */
static H5I_id_group_t *id_group_list[H5I_MAXID];

/* Pointer to the atom node free list */
static H5I_id_info_t *id_free_list = NULL;

/*--------------------- Local function prototypes ---------------------------*/
static H5I_id_info_t *H5I_find_id(hid_t id);
static H5I_id_info_t *H5I_get_id_node(void);
static herr_t H5I_release_id_node(H5I_id_info_t *id);


/*--------------------------------------------------------------------------
NAME
   H5I_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5I_init_interface()
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any interface-specific data or routines.
--------------------------------------------------------------------------*/
static herr_t 
H5I_init_interface(void)
{
    herr_t		    ret_value = SUCCEED;
    FUNC_ENTER(H5I_init_interface, FAIL);

    /*
     * Make certain the ID types don't overflow the number of bits allocated
     * for them in an ID.
     */
    assert(H5I_MAXID<=(1<<GROUP_BITS));

    /* Registers the cleanup routine with the exit chain */
    ret_value = H5_add_exit(&H5I_term_interface);

    FUNC_LEAVE(ret_value);
}


/******************************************************************************
 NAME
     H5I_init_group - Initialize an ID group

 DESCRIPTION
    Creates a global ID group to store IDs in.  If the group has already
    been initialized, this routine just increments the count of # of
    initializations and returns without trying to change the size of the hash
    table.  A specific number of group entries may be reserved to enable
    "constant" values to be handed out which are valid IDs in the group, but
    which do not map to any data structures and are not allocated dynamicly
    later.

 RETURNS
    Returns SUCCEED if successful and FAIL otherwise

******************************************************************************/
intn 
H5I_init_group(H5I_type_t grp,	 /* IN: Group to initialize */
	       size_t hash_size, /* IN: Minimum hash table size to use for group */
	       uintn reserved,	 /* IN: Number of hash table entries to reserve */
	       herr_t (*free_func) (void *)	/* IN: Function to call when releasing ref counted objects */
)
{
    H5I_id_group_t	   *grp_ptr = NULL;	/* ptr to the atomic group */
    intn		    ret_value = SUCCEED;

    FUNC_ENTER(H5I_init_group, FAIL);

    if ((grp <= H5I_BADID || grp >= H5I_MAXID) && hash_size > 0) {
	HGOTO_DONE(FAIL);
    }

#ifdef HASH_SIZE_POWER_2
    /*
     * If anyone knows a faster test for a power of two, please change this
     * silly code -QAK
     */
    if (!(hash_size == 2 || hash_size == 4 || hash_size == 8 ||
	  hash_size == 16 || hash_size == 32 || hash_size == 64 ||
	  hash_size == 128 || hash_size == 256 || hash_size == 512 ||
	  hash_size == 1024 || hash_size == 2048 || hash_size == 4096 ||
	  hash_size == 8192 || hash_size == 16374 || hash_size == 32768 ||
	  hash_size == 65536 || hash_size == 131072 || hash_size == 262144 ||
	  hash_size == 524288 || hash_size == 1048576 ||
	  hash_size == 2097152 || hash_size == 4194304 ||
	  hash_size == 8388608 || hash_size == 16777216 ||
	  hash_size == 33554432 || hash_size == 67108864 ||
	  hash_size == 134217728 || hash_size == 268435456))
	HGOTO_DONE(FAIL);
#endif /* HASH_SIZE_POWER_2 */

    if (id_group_list[grp] == NULL) {
	/* Allocate the group information */
	if (NULL==(grp_ptr = H5MM_calloc(sizeof(H5I_id_group_t)))) {
	    HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,
			 "memory allocation failed");
	}
	id_group_list[grp] = grp_ptr;
    } else {
	/* Get the pointer to the existing group */
	grp_ptr = id_group_list[grp];
    }

    if (grp_ptr->count == 0) {
	/* Initialize the ID group structure */
	grp_ptr->hash_size = hash_size;
	grp_ptr->reserved = reserved;
	grp_ptr->wrapped = 0;
	grp_ptr->ids = 0;
	grp_ptr->nextid = reserved;
	grp_ptr->free_func = free_func;
	grp_ptr->id_list = H5MM_calloc(hash_size*sizeof(H5I_id_info_t *));
	if (NULL==grp_ptr->id_list) {
	    HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,
			 "memory allocation failed");
	}
    }
    
    /* Increment the count of the times this group has been initialized */
    grp_ptr->count++;

  done:
    if (ret_value == FAIL) {
	/* Error condition cleanup */
	if (grp_ptr != NULL) {
	    H5MM_xfree (grp_ptr->id_list);
	    H5MM_xfree (grp_ptr);
	}
    }
    
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5I_destroy_group
 *
 * Purpose:	Decrements the reference count on an entire group of IDs.
 *		If the group reference count becomes zero then the group is
 *		destroyed along with all atoms in that group regardless of
 *		their reference counts.  Destroying IDs involves calling
 *		the free-func for each ID's object and then adding the ID
 *		struct to the ID free list.
 *		
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Unknown
 *
 * Modifications:
 *
 * 	Robb Matzke, 25 Feb 1998
 *	IDs are freed when a group is destroyed.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5I_destroy_group(H5I_type_t grp)
{
    H5I_id_group_t	*grp_ptr = NULL;	/* ptr to the atomic group */
    H5I_id_info_t	*cur=NULL, *next=NULL;
    intn		ret_value = SUCCEED;
    uintn		i;

    FUNC_ENTER(H5I_destroy_group, FAIL);

    if (grp <= H5I_BADID || grp >= H5I_MAXID)
	HGOTO_DONE(FAIL);

    grp_ptr = id_group_list[grp];
    if (grp_ptr == NULL || grp_ptr->count <= 0)
	HGOTO_DONE(FAIL);

    /*
     * Decrement the number of users of the atomic group.  If this is the
     * last user of the group then release all atoms from the group.  The
     * free function is invoked for each atom being freed.
     */
    if ((--(grp_ptr->count)) == 0) {

#ifdef IDS_ARE_CACHED
	/*
	 * Remove atoms from the global atom cache.
	 */
	for (i=0; i<ID_CACHE_SIZE; i++) {
	    if (H5I_GROUP(H5I_id_cache[i]) == grp) {
		H5I_id_cache[i] = (-1);
		H5I_obj_cache[i] = NULL;
	    }
	}
#endif /* IDS_ARE_CACHED */

	/*
	 * Free all objects.
	 */
	if (grp_ptr->free_func) {
	    for (i=0; i<grp_ptr->hash_size; i++) {
            for (cur=grp_ptr->id_list[i]; cur; cur=next) {
                /* Free the object */
                (grp_ptr->free_func)(cur->obj_ptr);

                /* Add ID struct to free list */
                next = cur->next;
                cur->next = id_free_list;
                id_free_list = cur;
            }
	    }
	}

	/* Free local cache and reset group */
	H5MM_xfree(grp_ptr->id_list);
	HDmemset (grp_ptr, 0, sizeof(grp_ptr));
    }
    
  done:
    FUNC_LEAVE(ret_value);
}

/******************************************************************************
 NAME
     H5I_register - Register an object in a group and get an ID for it.

 DESCRIPTION
    Registers an object in a group and returns an ID for it.	This routine
    does _not_ check for unique-ness of the objects, if you register an object
    twice, you will get two different IDs for it.  This routine does make
    certain that each ID in a group is unique.  IDs are created by getting
    a unique number for the group the ID is in and incorporating the group
    into the ID which is returned to the user.

 RETURNS
    Returns ID if successful and FAIL otherwise

*******************************************************************************/
hid_t 
H5I_register(H5I_type_t grp,   /* IN: Group to register the object in */
	     void *object     /* IN: Object to attach to atom */
)
{
    H5I_id_group_t	   *grp_ptr = NULL;	/* ptr to the group */
    H5I_id_info_t	*id_ptr = NULL;	/* ptr to the new ID information */
    hid_t		    new_id;	/* new ID */
    uintn		    hash_loc;	/* new item's hash table location */
    hid_t		    ret_value = SUCCEED;

    FUNC_ENTER(H5I_register, FAIL);

    if (grp <= H5I_BADID || grp >= H5I_MAXID)
	HGOTO_DONE(FAIL);

    grp_ptr = id_group_list[grp];
    if (grp_ptr == NULL || grp_ptr->count <= 0)
	HGOTO_DONE(FAIL);

    if ((id_ptr = H5I_get_id_node()) == NULL)
	HGOTO_DONE(FAIL);

    /* Create the struct & it's ID */
    new_id = H5I_MAKE(grp, grp_ptr->nextid);
    id_ptr->id = new_id;
    id_ptr->count = 1; /*initial reference count*/
    id_ptr->obj_ptr = object;
    id_ptr->next = NULL;

    /* hash bucket already full, prepend to front of chain */
    hash_loc = grp_ptr->nextid % (uintn) grp_ptr->hash_size;
    if (grp_ptr->id_list[hash_loc] != NULL)
	id_ptr->next = grp_ptr->id_list[hash_loc];

    /* Insert into the group */
    grp_ptr->id_list[hash_loc] = id_ptr;
    grp_ptr->ids++;
    grp_ptr->nextid++;

    /*
     * This next section of code checks for the 'nextid' getting too large and
     * wrapping around, thus necessitating checking for duplicate IDs being
     * handed out.
     */
    if (grp_ptr->nextid > (uintn) ID_MASK || grp_ptr->wrapped != 0) {
	if (grp_ptr->wrapped == 0) {
	    /* set the "wrapped around" flag if it isn't already */
	    grp_ptr->wrapped = 1;
	    /* re-start the ID counter */
	    grp_ptr->nextid = grp_ptr->reserved;
	}
	
	do {
	    /* new ID to check for */
	    hid_t next_id = H5I_MAKE(grp, grp_ptr->nextid);
	    H5I_id_info_t *curr_id;   /* ptr to the current atom */
	    hash_loc = H5I_LOC (grp_ptr->nextid, grp_ptr->hash_size);

	    curr_id = grp_ptr->id_list[hash_loc];
	    if (curr_id == NULL) break; /* Ha! this is not likely... */

	    while (curr_id != NULL) {
		if (curr_id->id == next_id) break;
		curr_id = curr_id->next;
	    }
	    if (curr_id == NULL) break; /* must not have found a match */
	    grp_ptr->nextid++;
	} while (grp_ptr->nextid <= (uintn) ID_MASK);
	
	if (grp_ptr->nextid > (uintn) ID_MASK) {
	    /* All the IDs are gone! */
	    HGOTO_DONE(FAIL);
	}
    }
    ret_value = new_id;

  done:
    if (ret_value == FAIL) {
	/* Error condition cleanup */
    }
    
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}

/******************************************************************************
 NAME
     H5I_inc_ref - Adds a reference to a reference counted ID.
	IN: ID to increment reference count for
 DESCRIPTION
    Increments the number of references outstanding for an ID.  This will
    fail if the group is not a reference counted group.

 RETURNS
    ID/FAIL

*******************************************************************************/
hid_t
H5I_inc_ref(hid_t id)
{
    H5I_type_t		grp = H5I_GROUP(id); /* object's group	*/
    H5I_id_group_t	*grp_ptr = NULL;	/* ptr to the ID group*/
    H5I_id_info_t		*id_ptr = NULL;	/* ptr to the new ID 	*/
    hid_t		ret_value = FAIL;

    FUNC_ENTER(H5I_inc_ref, FAIL);

    grp_ptr = id_group_list[grp];
    if (grp_ptr == NULL || grp_ptr->count <= 0 || grp_ptr->free_func == NULL) {
	HRETURN(FAIL);
    }
    
    /* General lookup of the atom */
    if (NULL!=(id_ptr = H5I_find_id(id))) {
	id_ptr->count++;
	ret_value = id;
    }
    
    FUNC_LEAVE(ret_value);
}

/******************************************************************************
 NAME
     H5I_object - Returns to the object ptr for the ID 

 DESCRIPTION
    Retrieves the object ptr which is associated with the ID.

 RETURNS
    Returns object ptr if successful and NULL otherwise

*******************************************************************************/
void * 
H5I_object(hid_t id)
{
#ifdef IDS_ARE_CACHED
    uintn		i;	/* local counter */
#endif /* IDS_ARE_CACHED */
    H5I_id_info_t		*id_ptr = NULL;	/* ptr to the new atom */
    void 		*ret_value = NULL;

    FUNC_ENTER(H5I_object, NULL);

#ifdef IDS_ARE_CACHED
    /*
     * Look for the ID in the cache first. Implement a simple "move
     * forward" caching scheme by swapping the found cache item with the
     * previous cache item.  This gradually migrates used cache items toward
     * the front of the cache and unused items toward the end.  For instance,
     * finding `e' in the cache results in:
     *
     * Before: a b c d e f g h i j
     *	       | | |  X  | | | | |
     * After:  a b c e d f g h i j
     */
    for (i=0; i<ID_CACHE_SIZE; i++)
	if (H5I_id_cache[i] == id) {
	    ret_value = H5I_obj_cache[i];
	    if (i > 0) {
		hid_t t_id = H5I_id_cache[i-1];
		void *t_obj = H5I_obj_cache[i-1];
		H5I_id_cache[i-1] = H5I_id_cache[i];
		H5I_obj_cache[i-1] = H5I_obj_cache[i];
		H5I_id_cache[i] = t_id;
		H5I_obj_cache[i] = t_obj;
	    }
	    HGOTO_DONE(ret_value);
	}
#endif /* IDS_ARE_CACHED */

    /* General lookup of the ID */
    if ((id_ptr = H5I_find_id(id)) == NULL) HGOTO_DONE(NULL);

    /* Check if we've found the correct ID */
    if (id_ptr != NULL) ret_value = id_ptr->obj_ptr;

  done:
    FUNC_LEAVE(ret_value);
}

/******************************************************************************
 NAME
     H5I_get_type - Returns to the group for the ID 

 DESCRIPTION
    Retrieves the group which is associated with the ID.

 RETURNS
    Returns group if successful and H5I_BADID otherwise

*******************************************************************************/
H5I_type_t 
H5I_get_type(hid_t id)
{
    H5I_type_t		ret_value = H5I_BADID;

    FUNC_ENTER(H5I_get_type, H5I_BADID);

    ret_value = H5I_GROUP(id);
    assert(ret_value>H5I_BADID && ret_value<H5I_MAXID);

    FUNC_LEAVE(ret_value);
}

/******************************************************************************
 NAME
     H5Iget_type - Returns the type of an ID 

 DESCRIPTION
    Retrieves the type of an ID.

 RETURNS
    Returns group if successful and H5I_BADID otherwise

*******************************************************************************/
H5I_type_t
H5Iget_type(hid_t id)
{
    H5I_type_t		ret_value = H5I_BADID;

    FUNC_ENTER(H5Iget_type, H5I_BADID);
    H5TRACE1("It","i",id);

    if (ret_value <= H5I_BADID || ret_value >= H5I_MAXID)
        HGOTO_DONE(H5I_BADID);

    ret_value = H5I_get_type(id);

done:
    FUNC_LEAVE(ret_value);
}

/******************************************************************************
 NAME
     H5I_remove - Removes an ID from a group

 DESCRIPTION
    Removes an ID from a group.

 RETURNS
    Returns ID's object if successful and NULL otherwise

*******************************************************************************/
void * 
H5I_remove(hid_t id)
{
    H5I_id_group_t	*grp_ptr = NULL;/* ptr to the atomic group	*/
    H5I_id_info_t		*curr_id,	/* ptr to the current atom 	*/
			*last_id;	/* ptr to the last atom 	*/
    H5I_type_t		grp;		/* atom's atomic group 		*/
    uintn		hash_loc;	/* atom's hash table location 	*/
#ifdef IDS_ARE_CACHED
    uintn		i;		/* local counting variable 	*/
#endif
    void *	      ret_value = NULL;

    FUNC_ENTER(H5I_remove, NULL);

    grp = H5I_GROUP(id);
    if (grp <= H5I_BADID || grp >= H5I_MAXID) HGOTO_DONE(NULL);

    grp_ptr = id_group_list[grp];
    if (grp_ptr == NULL || grp_ptr->count <= 0) HGOTO_DONE(NULL);

    /* Get the location in which the ID is located */
    hash_loc = (uintn) H5I_LOC(id, grp_ptr->hash_size);
    curr_id = grp_ptr->id_list[hash_loc];
    if (curr_id == NULL) HGOTO_DONE(NULL);

    last_id = NULL;
    while (curr_id != NULL) {
	if (curr_id->id == id) break;
	last_id = curr_id;
	curr_id = curr_id->next;
    }

    if (curr_id != NULL) {
	if (last_id == NULL) {
	    /* ID is the first the chain */
	    grp_ptr->id_list[hash_loc] = curr_id->next;
	} else {
	    last_id->next = curr_id->next;
	}
	ret_value = curr_id->obj_ptr;
	H5I_release_id_node(curr_id);
    } else {
	/* couldn't find the ID in the proper place */
	HGOTO_DONE(NULL);
    }

#ifdef IDS_ARE_CACHED
    /* Delete object from cache */
    for (i = 0; i < ID_CACHE_SIZE; i++)
	if (H5I_id_cache[i] == id) {
	    H5I_id_cache[i] = (-1);
	    H5I_obj_cache[i] = NULL;
	    break; /* we assume there is only one instance in the cache */
	}
#endif /* IDS_ARE_CACHED */

    /* Decrement the number of IDs in the group */
    (grp_ptr->ids)--;

  done:
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5I_dec_ref
 *
 * Purpose:	Decrements the number of references outstanding for an ID.
 *		This will fail if the group is not a reference counted group.
 *		The ID group's 'free' function will be called for the ID
 *		if the reference count for the ID reaches 0 and a free
 *		function has been defined at group creation time.
 *
 * Return:	Success:	New reference count.
 *
 *		Failure:	FAIL
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
 *-------------------------------------------------------------------------
 */
intn
H5I_dec_ref(hid_t id)
{
    H5I_type_t		grp = H5I_GROUP(id); /* Group the object is in */
    H5I_id_group_t	*grp_ptr = NULL;     /* ptr to the group */
    H5I_id_info_t		*id_ptr = NULL;     /* ptr to the new ID */
    void *		obj;	    /* object to call 'free' function with */
    intn		ret_value = FAIL;

    FUNC_ENTER(H5I_dec_ref, FAIL);

    grp_ptr = id_group_list[grp];
    if (grp_ptr == NULL || grp_ptr->count <= 0) {
	HRETURN(FAIL);
    }
    
    /* General lookup of the ID */
    if ((id_ptr = H5I_find_id(id)) != NULL) {
	/* Decrement the reference count */
	ret_value = --(id_ptr->count);

	/* If the reference count is zero, remove the object from the group */
	if (0 == id_ptr->count && (obj = H5I_remove(id)) != NULL) {
	    /*
	     * call the user's 'free' function for the atom's information,
	     * otherwise just leak memory.
	     */
	    if (grp_ptr->free_func) {
		if ((grp_ptr->free_func)(obj)<0) {
		    HRETURN_ERROR (H5E_ATOM, H5E_CANTINIT, FAIL,
				   "unable to free atom");
		}
	    }
	}
	ret_value = SUCCEED;
    }
    FUNC_LEAVE(ret_value);
}

/******************************************************************************
 NAME
     H5I_search - Search for an object in a group and get it's pointer.

 DESCRIPTION
    Searchs for an object in a group and returns the pointer to it.
    This routine calls the function pointer passed in for each object in the
    group until it finds a match.  Currently there is no way to resume a
    search.

 RETURNS
    Returns pointer an ID's object if successful and NULL otherwise

*******************************************************************************/
void *
H5I_search(H5I_type_t grp,	    /* IN: Group to search for the object in */
	   H5I_search_func_t func,   /* IN: Ptr to the comparison function */
	   const void *key  /* IN: pointer to key to compare against */
)
{
    H5I_id_group_t	   *grp_ptr = NULL;	/* ptr to the group */
    H5I_id_info_t		   *id_ptr = NULL;	/* ptr to the new ID */
    uintn		    i;	/* local counting variable */
    void *	      ret_value = NULL;

    FUNC_ENTER(H5I_search, NULL);

    if (grp <= H5I_BADID || grp >= H5I_MAXID)
	HGOTO_DONE(NULL);

    grp_ptr = id_group_list[grp];
    if (grp_ptr == NULL || grp_ptr->count <= 0)
	HGOTO_DONE(NULL);

    /* Start at the beginning of the array */
    for (i = 0; i < grp_ptr->hash_size; i++) {
	id_ptr = grp_ptr->id_list[i];
	while (id_ptr != NULL) {
	    if ((*func) (id_ptr->obj_ptr, key))
		HGOTO_DONE(id_ptr->obj_ptr);	/* found the item we are looking for */
	    id_ptr = id_ptr->next;
	}
    }

  done:
    FUNC_LEAVE(ret_value);
}

/******************************************************************************
 NAME
     H5I_find_id - Finds a ID in a group

 DESCRIPTION
    Retrieves the ID ptr which is associated with the ID.

 RETURNS
    Returns ID ptr if successful and NULL otherwise

*******************************************************************************/
static H5I_id_info_t *
H5I_find_id(hid_t id)
{
    H5I_id_group_t	   *grp_ptr = NULL;	/* ptr to the group */
    H5I_id_info_t		   *id_ptr = NULL;	/* ptr to the new ID */
    H5I_type_t		    grp;	/* ID's group */
    uintn		    hash_loc;	/* ID's hash table location */
    H5I_id_info_t		   *ret_value = NULL;

    FUNC_ENTER(H5I_find_id, NULL);

    grp = H5I_GROUP(id);
    if (grp <= H5I_BADID || grp >= H5I_MAXID)
	HGOTO_DONE(NULL);

    grp_ptr = id_group_list[grp];
    if (grp_ptr == NULL || grp_ptr->count <= 0)
	HGOTO_DONE(NULL);

    /* Get the location in which the ID is located */
    hash_loc = (uintn) H5I_LOC(id, grp_ptr->hash_size);
    id_ptr = grp_ptr->id_list[hash_loc];
    if (id_ptr == NULL)
	HGOTO_DONE(NULL);

    while (id_ptr != NULL) {
	if (id_ptr->id == id) break;
	id_ptr = id_ptr->next;
    }
    ret_value = id_ptr;

#ifdef IDS_ARE_CACHED
    H5I_id_cache[ID_CACHE_SIZE-1] = id;
    H5I_obj_cache[ID_CACHE_SIZE-1] = id_ptr->obj_ptr;
#endif /* IDS_ARE_CACHED */

  done:
    FUNC_LEAVE(ret_value);
}

/******************************************************************************
 NAME
     H5I_get_idm_node - Gets an ID node

 DESCRIPTION
    Either gets an ID node from the free list (if there is one available)
    or allocate a node.

 RETURNS
    Returns ID ptr if successful and NULL otherwise

*******************************************************************************/
static H5I_id_info_t *
H5I_get_id_node(void)
{
    H5I_id_info_t		   *ret_value = NULL;

    FUNC_ENTER(H5I_get_id_node, NULL);

    if (id_free_list != NULL) {
	ret_value = id_free_list;
	id_free_list = id_free_list->next;
    } else if (NULL==(ret_value = H5MM_malloc(sizeof(H5I_id_info_t)))) {
	HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,
		       "memory allocation failed");
    }

    FUNC_LEAVE(ret_value);
}

/******************************************************************************
 NAME
     H5I_release_id_node - Releases an ID node

 DESCRIPTION
    Puts an ID node into the free list

 RETURNS
    SUCCEED

*******************************************************************************/
static herr_t
H5I_release_id_node(H5I_id_info_t *id)
{
    FUNC_ENTER(H5I_release_id_node, FAIL);

    /* Insert the ID at the beginning of the free list */
    id->next = id_free_list;
    id_free_list = id;

    FUNC_LEAVE(SUCCEED);
}

/*--------------------------------------------------------------------------
 NAME
    H5I_term_interface
 PURPOSE
    Terminate various static buffers.
 USAGE
    intn H5I_term_interface()
 RETURNS
    Returns SUCCEED/FAIL
 DESCRIPTION
    Free various buffers allocated in the H5I routines.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    Should only ever be called by the "atexit" function HDFend
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
void 
H5I_term_interface(void)
{
    H5I_id_info_t		   *curr;
    intn		    i;

    /* Release the free-list if it exists */
    if (id_free_list != NULL) {
	while (id_free_list != NULL) {
	    curr = id_free_list;
	    id_free_list = id_free_list->next;
	    HDfree(curr);
	}
    }

    /* Release all groups */
    for (i = 0; i < (intn) H5I_MAXID; i++) {
	if (id_group_list[i] != NULL) {
	    HDfree(id_group_list[i]);
	    id_group_list[i] = NULL;
	}
    }
}
