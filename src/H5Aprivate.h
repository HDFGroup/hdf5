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

/*-----------------------------------------------------------------------------
 * File:    atom.h
 * Purpose: header file for atom API
 *---------------------------------------------------------------------------*/

/* avoid re-inclusion */
#ifndef _H5Aprivate_H
#define _H5Aprivate_H

#include <H5Apublic.h>          /*include Public Definitions                 */

/* Private headers needed by this file */
#include <H5private.h>

/* Atom Features control */

/*
 * Define the following macro for fast hash calculations (but limited
 * hash sizes)
 */
#define HASH_SIZE_POWER_2

/* Define the following macro for atom caching over all the atoms */
#define ATOMS_ARE_CACHED

#ifdef ATOMS_ARE_CACHED
#  define ATOM_CACHE_SIZE 4             /*# of previous atoms cached         */
#endif

/* Map an atom to a Group number */
#define ATOM_TO_GROUP(a)    ((group_t)((((hid_t)(a))>>			     \
					((sizeof(hid_t)*8)-GROUP_BITS))	     \
				       &GROUP_MASK))

#ifdef HASH_SIZE_POWER_2

/*
 * Map an atom to a hash location (assumes s is a power of 2 and smaller
 * than the ATOM_MASK constant).
 */
#  define ATOM_TO_LOC(a,s)    ((hid_t)(a)&((s)-1))
#else

/*
 * Map an atom to a hash location.
 */
#  define ATOM_TO_LOC(a,s)    (((hid_t)(a)&ATOM_MASK)%(s))
#endif

/* Default sizes of the hash-tables for various atom groups */
#define H5A_ERRSTACK_HASHSIZE   	64
#define H5A_FILEID_HASHSIZE     	64
#define H5A_TEMPID_HASHSIZE     	64
#define H5A_DATATYPEID_HASHSIZE 	64
#define H5A_DATASPACEID_HASHSIZE 	64
#define H5A_DATASETID_HASHSIZE  	64
#define H5A_OID_HASHSIZE        	64
#define H5A_GROUPID_HASHSIZE    	64

/* Atom information structure used */
typedef struct atom_info_t {
    hid_t              id;       /*atom ID for this info                   */
    uintn              count;    /*ref. count for this atom                */
    void               *obj_ptr; /*pointer associated with the atom 	   */
    struct atom_info_t *next;	/*link to next atom (in case of hash-clash)*/
} atom_info_t;

/* Atom group structure used */
typedef struct atom_group_struct_tag {
    uintn       count;          /*# of times this group has been initialized */
    uintn       reserved;       /*# of atoms to reserve for constant atoms   */
    uintn       wrapped;	/*whether the id count has wrapped around    */
    intn        hash_size;      /*sizeof the hash table to store the atoms in*/
    uintn       atoms;          /*current number of atoms held               */
    uintn       nextid;         /*atom ID to use for the next atom           */
    herr_t      (*free_func)(void*);/*func to call to release object	     */
    atom_info_t **atom_list;    /*pointer to an array of ptrs to atoms       */
} atom_group_t;

/* Type of the function to compare objects & keys */
typedef intn (*H5A_search_func_t) (const void * obj, const void * key);

/* Private Functions in H5A.c */
intn H5A_init_group (group_t grp, intn hash_size, uintn reserved,
		     herr_t (*free_func)(void *));
herr_t H5A_destroy_group (group_t grp);
hid_t H5A_register (group_t grp, void *object);
void *H5A_object (hid_t atm);
group_t H5A_group (hid_t atm);
void *H5A_remove (hid_t atm);
void *H5A_search (group_t grp, H5A_search_func_t func, const void *key);
void H5A_term_interface (void);
intn H5A_dec_ref (hid_t atm);
hid_t H5A_inc_ref (hid_t atm);

#endif
