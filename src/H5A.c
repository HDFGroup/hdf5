/****************************************************************************
 * NCSA HDF                                                                 *
 * Software Development Group                                               *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/

#ifdef RCSID
static char             RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

/*
   FILE
   atom.c - Internal storage routines for handling "atoms"

   REMARKS
   Atoms are just ID's which allow objects (void *'s currently) to be
   bundled into "groups" for more general storage.

   DESIGN
   The groups are stored in an array of pointers to store each group in an
   element. Each "atomic group" node contains a link to a hash table to
   manage the atoms in each group.  The allowed "atomic groups" are stored
   in an enum (called group_t) in atom.h.

   BUGS/LIMITATIONS
   Can't interate over the atoms in a group.

   LOCAL ROUTINES
   H5A_find_atom      - Returns a pointer to an atom_info_t from a atom ID
   H5A_get_atom_node  - Gets an atom node (uses the atom free list)
   H5A_release_atom_node - Releases an atom node (uses the atom free list)
   EXPORTED ROUTINES
   Atom Functions:
   H5Aregister_atom   - Register an object in a group and get an atom for it
   H5Aatom_object     - Get the object for an atom
   H5Aatom_group      - Get the group for an atom
   H5Aremove_atom     - Remove an atom from a group
   H5Asearch_atom     - Search a group for a particular object
   H5Ais_reserved     - Check whether an atom is a reserved atom in a group
   Atom Group Functions:
   H5Ainit_group      - Initialize a group to store atoms in
   H5Adestroy_group   - Destroy an atomic group
   Atom Group Cleanup:
   H5Ashutdown        - Terminate various static buffers.

   AUTHOR
   Quincey Koziol

   MODIFICATION HISTORY
   1/3/96  - Starting writing specs & coding prototype
   1/7/96  - Finished coding prototype
   6/10/97 - Moved into HDF5 library
 */

#include <H5private.h>
#include <H5Aprivate.h>
#include <H5Eprivate.h>

#define PABLO_MASK      H5A_mask

/*-------------------- Locally scoped variables -----------------------------*/

#ifdef ATOMS_ARE_CACHED
/* Array of pointers to atomic groups */
static hid_t            atom_id_cache[ATOM_CACHE_SIZE] =
{-1, -1, -1, -1};
static VOIDP            atom_obj_cache[ATOM_CACHE_SIZE] =
{NULL};
#endif

/* Array of pointers to atomic groups */
static atom_group_t    *atom_group_list[MAXGROUP] =
{NULL};

/* Pointer to the atom node free list */
static atom_info_t     *atom_free_list = NULL;

/* Interface initialialization? */
static hbool_t          interface_initialize_g = FALSE;
#define INTERFACE_INIT H5A_init_interface
static herr_t           H5A_init_interface(void);

/*--------------------- Local function prototypes ---------------------------*/
static atom_info_t     *H5A_find_atom(hid_t atm);
static atom_info_t     *H5A_get_atom_node(void);
static herr_t           H5A_release_atom_node(atom_info_t *atm);

/*--------------------------------------------------------------------------
NAME
   H5A_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5A_init_interface()
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any interface-specific data or routines.
--------------------------------------------------------------------------*/
static herr_t 
H5A_init_interface(void)
{
    herr_t                  ret_value = SUCCEED;
    FUNC_ENTER(H5A_init_interface, FAIL);

    /* Registers the cleanup routine with the exit chain */
    ret_value = H5_add_exit(&H5A_term_interface);

    FUNC_LEAVE(ret_value);
}                               /* H5E_init_interface */

/******************************************************************************
 NAME
     H5Ainit_group - Initialize an atomic group

 DESCRIPTION
    Creates a global atomic group to store atoms in.  If the group has already
    been initialized, this routine just increments the count of # of
    initializations and returns without trying to change the size of the hash
    table.  A specific number of group entries may be reserved to enable
    "constant" values to be handed out which are valid atoms in the group, but
    which do not map to any data structures and are not allocated dynamicly
    later.

 RETURNS
    Returns SUCCEED if successful and FAIL otherwise

*******************************************************************************/
intn 
H5Ainit_group(group_t grp,      /* IN: Group to initialize */
              intn hash_size,   /* IN: Minimum hash table size to use for group */
              uintn reserved,   /* IN: Number of hash table entries to reserve */
              herr_t                  (*free_func) (void *)     /* IN: Function to call when releasing ref counted objects */
)
{
    atom_group_t           *grp_ptr = NULL;     /* ptr to the atomic group */
    intn                    ret_value = SUCCEED;

    FUNC_ENTER(H5Ainit_group, FAIL);

    if ((grp <= BADGROUP || grp >= MAXGROUP) && hash_size > 0)
        HGOTO_DONE(FAIL);

#ifdef HASH_SIZE_POWER_2
    /* If anyone knows a faster test for a power of two, please change this silly code -QAK */
    if (!(hash_size == 2 || hash_size == 4 || hash_size == 8 || hash_size == 16
          || hash_size == 32 || hash_size == 64 || hash_size == 128 || hash_size == 256
          || hash_size == 512 || hash_size == 1024 || hash_size == 2048
          || hash_size == 4096 || hash_size == 8192 || hash_size == 16374
          || hash_size == 32768 || hash_size == 65536 || hash_size == 131072
       || hash_size == 262144 || hash_size == 524288 || hash_size == 1048576
     || hash_size == 2097152 || hash_size == 4194304 || hash_size == 8388608
          || hash_size == 16777216 || hash_size == 33554432 || hash_size == 67108864
          || hash_size == 134217728 || hash_size == 268435456))
        HGOTO_DONE(FAIL);
#endif /* HASH_SIZE_POWER_2 */

    if (atom_group_list[grp] == NULL) {         /* Allocate the group information */
        grp_ptr = (atom_group_t *) HDcalloc(1, sizeof(atom_group_t));
        if (grp_ptr == NULL)
            HGOTO_DONE(FAIL);
        atom_group_list[grp] = grp_ptr;
    }
    /* end if */ 
    else                        /* Get the pointer to the existing group */
        grp_ptr = atom_group_list[grp];

    if (grp_ptr->count == 0) {  /* Initialize the atom group structure */
        grp_ptr->hash_size = hash_size;
        grp_ptr->reserved = reserved;
        grp_ptr->wrapped = 0;
        grp_ptr->atoms = 0;
        grp_ptr->nextid = reserved;
        grp_ptr->free_func = free_func;
        if ((grp_ptr->atom_list = (atom_info_t **) HDcalloc(hash_size, sizeof(atom_info_t *))) == NULL)
                                    HGOTO_DONE(FAIL);
    }                           /* end if */
    /* Increment the count of the times this group has been initialized */
    grp_ptr->count++;

#ifdef QAK
    printf("%s: group ID=%d, count=%d, current # of active atoms=%d\n", FUNC, grp, grp_ptr->count, grp_ptr->atoms);
#endif /* QAK */
  done:
    if (ret_value == FAIL) {    /* Error condition cleanup */
        if (grp_ptr != NULL) {
            if (grp_ptr->atom_list != NULL)
                HDfree(grp_ptr->atom_list);
            HDfree(grp_ptr);
        }                       /* end if */
    }                           /* end if */
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}                               /* end H5Ainit_group() */

/******************************************************************************
 NAME
     H5Adestroy_group - Destroy an atomic group

 DESCRIPTION
    Destroys an atomic group which atoms are stored in.  If the group still
    has atoms which are registered, this routine fails.  If there have been
    multiple initializations of the group, this routine just decrements the
    count of initializations and does not check the atoms out-standing.

 RETURNS
    Returns SUCCEED if successful and FAIL otherwise

*******************************************************************************/
intn 
H5Adestroy_group(group_t grp    /* IN: Group to destroy */
)
{
    atom_group_t           *grp_ptr = NULL;     /* ptr to the atomic group */
    intn                    ret_value = SUCCEED;

    FUNC_ENTER(H5Adestroy_group, FAIL);

    if (grp <= BADGROUP || grp >= MAXGROUP)
        HGOTO_DONE(FAIL);

    grp_ptr = atom_group_list[grp];
    if (grp_ptr == NULL || grp_ptr->count <= 0)
        HGOTO_DONE(FAIL);

#ifdef QAK
    printf("%s: group ID=%d, count=%d, current # of active atoms=%d\n", FUNC, grp, grp_ptr->count, grp_ptr->atoms);
#endif /* QAK */
    /* Decrement the number of users of the atomic group */
    if ((--(grp_ptr->count)) == 0) {
#ifdef ATOMS_ARE_CACHED
        {
            uintn                   i;

            for (i = 0; i < ATOM_CACHE_SIZE; i++)
                if (ATOM_TO_GROUP(atom_id_cache[i]) == grp) {
                    atom_id_cache[i] = (-1);
                    atom_obj_cache[i] = NULL;
                }               /* end if */
        }                       /* end block */
#endif /* ATOMS_ARE_CACHED */
        HDfree(grp_ptr->atom_list);
    }                           /* end if */
  done:
    if (ret_value == FAIL) {    /* Error condition cleanup */

    }                           /* end if */
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}                               /* end H5Adestroy_group() */

/******************************************************************************
 NAME
     H5Aregister_atom - Register an object in a group and get an atom for it.

 DESCRIPTION
    Registers an object in a group and returns an atom for it.  This routine
    does _not_ check for unique-ness of the objects, if you register an object
    twice, you will get two different atoms for it.  This routine does make
    certain that each atom in a group is unique.  Atoms are created by getting
    a unique number for the group the atom is in and incorporating the group
    into the atom which is returned to the user.

 RETURNS
    Returns atom if successful and FAIL otherwise

*******************************************************************************/
hid_t 
H5Aregister_atom(group_t grp,   /* IN: Group to register the object in */
                 const void *object     /* IN: Object to attach to atom */
)
{
    atom_group_t           *grp_ptr = NULL;     /* ptr to the atomic group */
    atom_info_t            *atm_ptr = NULL;     /* ptr to the new atom */
    hid_t                   atm_id;     /* new atom ID */
    uintn                   hash_loc;   /* new item's hash table location */
    hid_t                   ret_value = SUCCEED;

    FUNC_ENTER(H5Aregister_atom, FAIL);

    if (grp <= BADGROUP || grp >= MAXGROUP)
        HGOTO_DONE(FAIL);

    grp_ptr = atom_group_list[grp];
    if (grp_ptr == NULL || grp_ptr->count <= 0)
        HGOTO_DONE(FAIL);

    if ((atm_ptr = H5A_get_atom_node()) == NULL)
        HGOTO_DONE(FAIL);

    /* Create the atom & it's ID */
    atm_id = MAKE_ATOM(grp, grp_ptr->nextid);
    atm_ptr->id = atm_id;
    atm_ptr->count = 1;         /* reference count all objects (even if no 'free' function defined */
    atm_ptr->obj_ptr = object;
    atm_ptr->next = NULL;

    /* hash bucket already full, prepend to front of chain */
    hash_loc = grp_ptr->nextid % (uintn) grp_ptr->hash_size;
    if (grp_ptr->atom_list[hash_loc] != NULL)
        atm_ptr->next = grp_ptr->atom_list[hash_loc];

    /* Insert into the group */
    grp_ptr->atom_list[hash_loc] = atm_ptr;
    grp_ptr->atoms++;
    grp_ptr->nextid++;

    /*
     * This next section of code checks for the 'nextid' getting too large and
     * wrapping around, thus necessitating checking for duplicate atoms being
     * handed out.
     */
    if (grp_ptr->nextid > (uintn) ATOM_MASK || grp_ptr->wrapped != 0) {
        if (grp_ptr->wrapped == 0) {
            grp_ptr->wrapped = 1;       /* set the "wrapped around" flag if it isn't already */
            grp_ptr->nextid = grp_ptr->reserved;        /* re-start the ID counter */
        }                       /* end if */
        do {
            hid_t                   next_atom = MAKE_ATOM(grp, grp_ptr->nextid);        /* new atom to check for */
            atom_info_t            *curr_atm;   /* ptr to the current atom */

            curr_atm = grp_ptr->atom_list[(uintn) ATOM_TO_LOC(grp_ptr->nextid, grp_ptr->hash_size)];
            if (curr_atm == NULL)       /* Ha! this is not likely... */
                break;

            while (curr_atm != NULL) {
                if (curr_atm->id == next_atom)
                    break;
                curr_atm = curr_atm->next;
            }                   /* end while */
            if (curr_atm == NULL)       /* must not have found a match */
                break;
            grp_ptr->nextid++;
        } while (grp_ptr->nextid <= (uintn) ATOM_MASK);
        if (grp_ptr->nextid > (uintn) ATOM_MASK)        /* All the atoms are gone! */
            HGOTO_DONE(FAIL);
    }                           /* end if */
    ret_value = atm_id;

  done:
    if (ret_value == FAIL) {    /* Error condition cleanup */

    }                           /* end if */
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}                               /* end H5Aregister_atom() */

/******************************************************************************
 NAME
     H5A_inc_ref - Adds a reference to a reference counted atom.
        IN: Atom to increment reference count for
 DESCRIPTION
    Increments the number of references outstanding for an atom.  This will
    fail if the group is not a reference counted group.

 RETURNS
    atm/FAIL

*******************************************************************************/
hid_t
H5A_inc_ref(hid_t atm)
{
    group_t                 grp = ATOM_TO_GROUP(atm);   /* Group the object is in */
    atom_group_t           *grp_ptr = NULL;     /* ptr to the atomic group */
    atom_info_t            *atm_ptr = NULL;     /* ptr to the new atom */
    hid_t                   ret_value = FAIL;

    FUNC_ENTER(H5A_inc_ref, FAIL);

    grp_ptr = atom_group_list[grp];
    if (grp_ptr == NULL || grp_ptr->count <= 0 || grp_ptr->free_func == NULL) {
        HRETURN(FAIL);
    }
    /* General lookup of the atom */
    if ((atm_ptr = H5A_find_atom(atm)) != NULL) {
        atm_ptr->count++;
        ret_value = atm;
    }
    FUNC_LEAVE(ret_value);
}

/******************************************************************************
 NAME
     H5Aatom_object - Returns to the object ptr for the atom 

 DESCRIPTION
    Retrieves the object ptr which is associated with the atom.

 RETURNS
    Returns object ptr if successful and NULL otherwise

*******************************************************************************/
VOIDP 
H5Aatom_object(hid_t atm        /* IN: Atom to retrieve object for */
)
{
#ifdef ATOMS_ARE_CACHED
    uintn                   i;  /* local counter */
#endif /* ATOMS_ARE_CACHED */
    atom_info_t            *atm_ptr = NULL;     /* ptr to the new atom */
    VOIDP                   ret_value = NULL;

    FUNC_ENTER(H5Aatom_object, NULL);

#ifdef ATOMS_ARE_CACHED
    /* Look for the atom in the cache first */
    for (i = 0; i < ATOM_CACHE_SIZE; i++)
        if (atom_id_cache[i] == atm) {
            ret_value = atom_obj_cache[i];
            if (i > 0) {        /* Implement a simple "move forward" caching scheme */
                hid_t                   t_atom = atom_id_cache[i - 1];
                VOIDP                   t_obj = atom_obj_cache[i - 1];

                atom_id_cache[i - 1] = atom_id_cache[i];
                atom_obj_cache[i - 1] = atom_obj_cache[i];
                atom_id_cache[i] = t_atom;
                atom_obj_cache[i] = t_obj;
            }                   /* end if */
            HGOTO_DONE(ret_value);
        }                       /* end if */
#endif                       /* ATOMS_ARE_CACHED */

    /* General lookup of the atom */
    if ((atm_ptr = H5A_find_atom(atm)) == NULL)
        HGOTO_DONE(NULL);

    /* Check if we've found the correct atom */
    if (atm_ptr != NULL)
        ret_value = atm_ptr->obj_ptr;

  done:
    if (ret_value == NULL) {    /* Error condition cleanup */

    }                           /* end if */
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}                               /* end H5Aatom_object() */

/******************************************************************************
 NAME
     H5Aatom_group - Returns to the group for the atom 

 DESCRIPTION
    Retrieves the group which is associated with the atom.

 RETURNS
    Returns group if successful and BADGROUP otherwise

*******************************************************************************/
group_t 
H5Aatom_group(hid_t atm         /* IN: Atom to retrieve group for */
)
{
    group_t                 ret_value = BADGROUP;

    FUNC_ENTER(H5Aatom_group, FAIL);

    ret_value = ATOM_TO_GROUP(atm);
    if (ret_value <= BADGROUP || ret_value >= MAXGROUP)
        HGOTO_DONE(BADGROUP);

  done:
    if (ret_value == BADGROUP) {        /* Error condition cleanup */

    }                           /* end if */
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}                               /* end H5Aatom_group() */

/******************************************************************************
 NAME
     H5Aremove_atom - Removes an atom from a group

 DESCRIPTION
    Removes an atom from a group.

 RETURNS
    Returns atom's object if successful and NULL otherwise

*******************************************************************************/
VOIDP 
H5Aremove_atom(hid_t atm        /* IN: Atom to remove */
)
{
    atom_group_t           *grp_ptr = NULL;     /* ptr to the atomic group */
    atom_info_t            *curr_atm,   /* ptr to the current atom */
                           *last_atm;   /* ptr to the last atom */
    group_t                 grp;        /* atom's atomic group */
    uintn                   hash_loc;   /* atom's hash table location */
#ifdef ATOMS_ARE_CACHED
    uintn                   i;  /* local counting variable */
#endif /* ATOMS_ARE_CACHED */
    VOIDP                   ret_value = NULL;

    FUNC_ENTER(H5Aremove_atom, NULL);

    grp = ATOM_TO_GROUP(atm);
    if (grp <= BADGROUP || grp >= MAXGROUP)
        HGOTO_DONE(NULL);

    grp_ptr = atom_group_list[grp];
    if (grp_ptr == NULL || grp_ptr->count <= 0)
        HGOTO_DONE(NULL);

    /* Get the location in which the atom is located */
    hash_loc = (uintn) ATOM_TO_LOC(atm, grp_ptr->hash_size);
    curr_atm = grp_ptr->atom_list[hash_loc];
    if (curr_atm == NULL)
        HGOTO_DONE(NULL);

    last_atm = NULL;
    while (curr_atm != NULL) {
        if (curr_atm->id == atm)
            break;
        last_atm = curr_atm;
        curr_atm = curr_atm->next;
    }                           /* end while */

    if (curr_atm != NULL) {
        if (last_atm == NULL)   /* atom is the first the chain */
            grp_ptr->atom_list[hash_loc] = curr_atm->next;
        else
            last_atm->next = curr_atm->next;
        ret_value = curr_atm->obj_ptr;
        H5A_release_atom_node(curr_atm);
    }
    /* end if */ 
    else                        /* couldn't find the atom in the proper place */
        HGOTO_DONE(NULL);

#ifdef ATOMS_ARE_CACHED
    /* Delete object from cache */
    for (i = 0; i < ATOM_CACHE_SIZE; i++)
        if (atom_id_cache[i] == atm) {
            atom_id_cache[i] = (-1);
            atom_obj_cache[i] = NULL;
            break;              /* we assume there is only one instance in the cache */
        }                       /* end if */
#endif                      /* ATOMS_ARE_CACHED */

    /* Decrement the number of atoms in the group */
    (grp_ptr->atoms)--;

  done:
    if (ret_value == NULL) {    /* Error condition cleanup */

    }                           /* end if */
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}                               /* end H5Aremove_atom() */

/******************************************************************************
 NAME
     H5A_dec_ref - Decrements a reference to a reference counted atom.
          IN: Atom to decrement reference count for
 DESCRIPTION
    Decrements the number of references outstanding for an atom.  This will
    fail if the group is not a reference counted group.  The atom group's
    'free' function will be called for the atom if the reference count for the
    atom reaches 0.

 RETURNS
    SUCCEED/FAIL

*******************************************************************************/
intn
H5A_dec_ref(hid_t atm)
{
    group_t                 grp = ATOM_TO_GROUP(atm);   /* Group the object is in */
    atom_group_t           *grp_ptr = NULL;     /* ptr to the atomic group */
    atom_info_t            *atm_ptr = NULL;     /* ptr to the new atom */
    VOIDP                   obj;        /* object to call 'free' function with */
    intn                    ret_value = FAIL;

    FUNC_ENTER(H5A_dec_ref, FAIL);

    grp_ptr = atom_group_list[grp];
    if (grp_ptr == NULL || grp_ptr->count <= 0 || grp_ptr->free_func == NULL) {
        HRETURN(FAIL);
    }
    /* General lookup of the atom */
    if ((atm_ptr = H5A_find_atom(atm)) != NULL) {
        /* Decrement the reference count */
        atm_ptr->count--;

        /* If the reference count is zero, remove the object from the group */
        if (0 == atm_ptr->count && (obj = H5Aremove_atom(atm)) != NULL) {
            /* call the user's 'free' function for the atom's information */
            (*grp_ptr->free_func) (obj);
        }
        ret_value = SUCCEED;
    }
    FUNC_LEAVE(ret_value);
}

/******************************************************************************
 NAME
     H5Asearch_atom - Search for an object in a group and get it's pointer.

 DESCRIPTION
    Searchs for an object in a group and returns the pointer to it.
    This routine calls the function pointer passed in for each object in the
    group until it finds a match.  Currently there is no way to resume a
    search.

 RETURNS
    Returns pointer an atom's object if successful and NULL otherwise

*******************************************************************************/
VOIDP 
H5Asearch_atom(group_t grp,     /* IN: Group to search for the object in */
               H5Asearch_func_t func,   /* IN: Ptr to the comparison function */
               const VOIDP key  /* IN: pointer to key to compare against */
)
{
    atom_group_t           *grp_ptr = NULL;     /* ptr to the atomic group */
    atom_info_t            *atm_ptr = NULL;     /* ptr to the new atom */
    intn                    i;  /* local counting variable */
    VOIDP                   ret_value = NULL;

    FUNC_ENTER(H5Asearch_atom, NULL);

    if (grp <= BADGROUP || grp >= MAXGROUP)
        HGOTO_DONE(NULL);

    grp_ptr = atom_group_list[grp];
    if (grp_ptr == NULL || grp_ptr->count <= 0)
        HGOTO_DONE(NULL);

    /* Start at the beginning of the array */
    for (i = 0; i < grp_ptr->hash_size; i++) {
        atm_ptr = grp_ptr->atom_list[i];
        while (atm_ptr != NULL) {
            if ((*func) (atm_ptr->obj_ptr, key))
                HGOTO_DONE(atm_ptr->obj_ptr);   /* found the item we are looking for */
            atm_ptr = atm_ptr->next;
        }                       /* end while */
    }                           /* end for */

  done:
    if (ret_value == NULL) {    /* Error condition cleanup */

    }                           /* end if */
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}                               /* end H5Asearch_atom() */

/******************************************************************************
 NAME
     H5Ais_reserved - Check whether an atom is a reserved atom in a group

 DESCRIPTION
    Determines the group an atom belongs to and checks if the atom is a
    reserved atom in the group.

 RETURNS
    Returns BTRUE/BFALSE/BFAIL

*******************************************************************************/
intn 
H5Ais_reserved(hid_t atm        /* IN: Group to search for the object in */
)
{
    atom_group_t           *grp_ptr = NULL;     /* ptr to the atomic group */
    group_t                 grp;        /* atom's atomic group */
    hbool_t                 ret_value = BFAIL;

    FUNC_ENTER(H5Ais_reserved, FAIL);

    grp = ATOM_TO_GROUP(atm);
    if (grp <= BADGROUP || grp >= MAXGROUP)
        HGOTO_DONE(BFAIL);

    grp_ptr = atom_group_list[grp];
    if (grp_ptr == NULL)
        HGOTO_DONE(BFAIL);

    /* Get the location in which the atom is located */
    if ((atm & ATOM_MASK) < grp_ptr->reserved)
        ret_value = BTRUE;
    else
        ret_value = BFALSE;

  done:
    if (ret_value == BFAIL) {   /* Error condition cleanup */

    }                           /* end if */
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}                               /* end H5Ais_reserved() */

/******************************************************************************
 NAME
     H5A_find_atom - Finds a atom in a group

 DESCRIPTION
    Retrieves the atom ptr which is associated with the atom.

 RETURNS
    Returns atom ptr if successful and NULL otherwise

*******************************************************************************/
static atom_info_t     *
H5A_find_atom(hid_t atm         /* IN: Atom to retrieve atom for */
)
{
    atom_group_t           *grp_ptr = NULL;     /* ptr to the atomic group */
    atom_info_t            *atm_ptr = NULL;     /* ptr to the new atom */
    group_t                 grp;        /* atom's atomic group */
    uintn                   hash_loc;   /* atom's hash table location */
    atom_info_t            *ret_value = NULL;

    FUNC_ENTER(H5A_find_atom, NULL);

    grp = ATOM_TO_GROUP(atm);
    if (grp <= BADGROUP || grp >= MAXGROUP)
        HGOTO_DONE(NULL);

    grp_ptr = atom_group_list[grp];
    if (grp_ptr == NULL || grp_ptr->count <= 0)
        HGOTO_DONE(NULL);

    /* Get the location in which the atom is located */
    hash_loc = (uintn) ATOM_TO_LOC(atm, grp_ptr->hash_size);
    atm_ptr = grp_ptr->atom_list[hash_loc];
    if (atm_ptr == NULL)
        HGOTO_DONE(NULL);

    while (atm_ptr != NULL) {
        if (atm_ptr->id == atm)
            break;
        atm_ptr = atm_ptr->next;
    }                           /* end while */
    ret_value = atm_ptr;

#ifdef ATOMS_ARE_CACHED
    atom_id_cache[ATOM_CACHE_SIZE - 1] = atm;
    atom_obj_cache[ATOM_CACHE_SIZE - 1] = atm_ptr->obj_ptr;
#endif /* ATOMS_ARE_CACHED */

  done:
    if (ret_value == NULL) {    /* Error condition cleanup */

    }                           /* end if */
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}                               /* end H5A_find_atom() */

/******************************************************************************
 NAME
     H5A_get_atom_node - Gets an atom node

 DESCRIPTION
    Either gets an atom node from the free list (if there is one available)
    or allocate a node.

 RETURNS
    Returns atom ptr if successful and NULL otherwise

*******************************************************************************/
static atom_info_t     *
H5A_get_atom_node(void)
{
    atom_info_t            *ret_value = NULL;

    FUNC_ENTER(H5A_get_atom_node, NULL);

    if (atom_free_list != NULL) {
        ret_value = atom_free_list;
        atom_free_list = atom_free_list->next;
    }
    /* end if */ 
    else {
        if ((ret_value = (atom_info_t *) HDmalloc(sizeof(atom_info_t))) == NULL)
                                    HGOTO_DONE(NULL);
    }                           /* end else */

  done:
    if (ret_value == NULL) {    /* Error condition cleanup */

    }                           /* end if */
    /* Normal function cleanup */
    FUNC_LEAVE(ret_value);
}                               /* end H5A_get_atom_node() */

/******************************************************************************
 NAME
     H5A_release_atom_node - Releases an atom node

 DESCRIPTION
    Puts an atom node into the free list

 RETURNS
    SUCCEED

*******************************************************************************/
static herr_t
H5A_release_atom_node(atom_info_t *atm)
{
    FUNC_ENTER(H5A_release_atom_node, FAIL);

    /* Insert the atom at the beginning of the free list */
    atm->next = atom_free_list;
    atom_free_list = atm;

    FUNC_LEAVE(SUCCEED);
}                               /* end H5A_release_atom_node() */

/*--------------------------------------------------------------------------
 NAME
    H5A_term_interface
 PURPOSE
    Terminate various static buffers.
 USAGE
    intn H5A_term_interface()
 RETURNS
    Returns SUCCEED/FAIL
 DESCRIPTION
    Free various buffers allocated in the H5A routines.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    Should only ever be called by the "atexit" function HDFend
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
void 
H5A_term_interface(void)
{
    atom_info_t            *curr;
    intn                    i;

    /* Release the free-list if it exists */
    if (atom_free_list != NULL) {
        while (atom_free_list != NULL) {
            curr = atom_free_list;
            atom_free_list = atom_free_list->next;
            HDfree(curr);
        }                       /* end while */
    }                           /* end if */
    for (i = 0; i < (intn) MAXGROUP; i++)
        if (atom_group_list[i] != NULL) {
            HDfree(atom_group_list[i]);
            atom_group_list[i] = NULL;
        }                       /* end if */
}                               /* end H5A_term_interface() */
