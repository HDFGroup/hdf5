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

/* $Id$ */

/*
 * This file contains function prototypes for each exported function in
 * the H5A module.
 */
#ifndef _H5Apublic_H
#define _H5Apublic_H

/* Public headers needed by this file */
#include <H5public.h>

/* Group values allowed */
typedef enum {
    BADGROUP = (-1),            /* Invalid Group */
    H5_ERR = 0,                 /* Group ID for Error stack objects */
    H5_FILE,                    /* Group ID for File objects */
    H5_TEMPLATE_0,              /* Group ID for Template objects */
    H5_TEMPLATE_1,              /* Group ID for Template objects */
    H5_TEMPLATE_2,              /* Group ID for Template objects */
    H5_TEMPLATE_3,              /* Group ID for Template objects */
    H5_TEMPLATE_4,              /* Group ID for Template objects */
    H5_TEMPLATE_5,              /* Group ID for Template objects */
    H5_TEMPLATE_6,              /* Group ID for Template objects */
    H5_TEMPLATE_7,              /* Group ID for Template objects */
#ifndef NDEBUG
    H5_TEMPLATE_MAX,            /* Not really a group ID */
#endif
    H5_GROUP,                   /* Group ID for Group objects */
    H5_DATATYPE,                /* Group ID for Datatype objects */
    H5_DATASPACE,               /* Group ID for Dataspace objects */
    H5_DATASET,                 /* Group ID for Dataset objects */
    H5_DIRECTORY,               /* Group ID for Directory objects */
    MAXGROUP                    /* Highest group in group_t (Invalid as true group) */
} group_t;

/* Type of atoms to return to users */
typedef int32 hid_t;

/* Type of the function to compare objects & keys */
typedef intn            (*H5Asearch_func_t) (const VOIDP obj, const VOIDP key);

/* # of bits to use for Group ID in each atom (change if MAXGROUP>16) */
#define GROUP_BITS  8
#define GROUP_MASK  0xFF

/* # of bits to use for the Atom index in each atom (assumes 8-bit bytes) */
#define ATOM_BITS   ((sizeof(hid_t)*8)-GROUP_BITS)
#define ATOM_MASK   0x0FFFFFFF

/* Combine a Group number and an atom index into an atom */
#define MAKE_ATOM(g,i)      ((((hid_t)(g)&GROUP_MASK)<<ATOM_BITS)|      \
                             ((hid_t)(i)&ATOM_MASK))

#ifdef __cplusplus
extern                  "C" {
#endif

/* Functions in H5A.c */
/******************************************************************************
 NAME
     H5Ainit_group - Initialize an atomic group

 DESCRIPTION
    Creates an atomic group to store atoms in.  If the group has already been
    initialized, this routine just increments the count of # of initializations
    and returns without trying to change the size of the hash table.

 RETURNS
    Returns SUCCEED if successful and FAIL otherwise

*******************************************************************************/
    intn                    H5Ainit_group(group_t grp,  /* IN: Group to initialize */
                                          intn hash_size,       /* IN: Minimum hash table size to use for group */
                                          uintn reserved,       /* IN: Number of hash table entries to reserve */
                               herr_t                  (*free_func) (void *)    /* IN: Function to call when releasing ref counted objects */
    );

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
    intn                    H5Adestroy_group(group_t grp        /* IN: Group to destroy */
    );

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
    hid_t                   H5Aregister_atom(group_t grp,       /* IN: Group to register the object in */
                                             const void *object         /* IN: Object to attach to atom */
    );

/******************************************************************************
 NAME
     H5Aatom_object - Returns to the object ptr for the atom 

 DESCRIPTION
    Retrieves the object ptr which is associated with the atom.

 RETURNS
    Returns object ptr if successful and NULL otherwise

*******************************************************************************/
    VOIDP                   H5Aatom_object(hid_t atm    /* IN: Atom to retrieve object for */
    );

/******************************************************************************
 NAME
     H5Aatom_group - Returns to the group for the atom 

 DESCRIPTION
    Retrieves the group which is associated with the atom.

 RETURNS
    Returns group if successful and FAIL otherwise

*******************************************************************************/
    group_t                 H5Aatom_group(hid_t atm     /* IN: Atom to retrieve group for */
    );

/******************************************************************************
 NAME
     H5Aremove_atom - Removes an atom from a group

 DESCRIPTION
    Removes an atom from a group.

 RETURNS
    Returns atom's object if successful and FAIL otherwise

*******************************************************************************/
    VOIDP                   H5Aremove_atom(hid_t atm    /* IN: Atom to remove */
    );

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
    VOIDP                   H5Asearch_atom(group_t grp,         /* IN: Group to search for the object in */
                                           H5Asearch_func_t func,       /* IN: Ptr to the comparison function */
                                           const VOIDP key      /* IN: pointer to key to compare against */
    );

/******************************************************************************
 NAME
     H5Ais_reserved - Check whether an atom is a reserved atom in a group

 DESCRIPTION
    Determines the group an atom belongs to and checks if the atom is a
    reserved atom in the group.

 RETURNS
    Returns BTRUE/BFALSE/BFAIL

*******************************************************************************/
    intn                    H5Ais_reserved(hid_t atm    /* IN: Group to search for the object in */
    );

/******************************************************************************
 NAME
     H5A_term_interface - Terminate various static buffers.

 DESCRIPTION
    Free various buffers allocated in the H5A routines.

 RETURNS
    Returns SUCCEED/FAIL

*******************************************************************************/
    void                    H5A_term_interface(void);

#ifdef __cplusplus
}

#endif
#endif
