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

/*-----------------------------------------------------------------------------
 * File:    H5Iprivate.h
 * Purpose: header file for ID API
 *---------------------------------------------------------------------------*/

/* avoid re-inclusion */
#ifndef _H5Iprivate_H
#define _H5Iprivate_H

#include "H5Ipublic.h"		/*include Public Definitions		     */

/* Private headers needed by this file */
#include "H5private.h"

/* Default sizes of the hash-tables for various atom groups */
#define H5I_ERRSTACK_HASHSIZE		64
#define H5I_FILEID_HASHSIZE		64
#define H5I_TEMPID_HASHSIZE		64
#define H5I_DATATYPEID_HASHSIZE		64
#define H5I_DATASPACEID_HASHSIZE	64
#define H5I_DATASETID_HASHSIZE		64
#define H5I_OID_HASHSIZE		64
#define H5I_GROUPID_HASHSIZE		64
#define H5I_ATTRID_HASHSIZE		64
#define H5I_TEMPBUFID_HASHSIZE		64
#define H5I_REFID_HASHSIZE		64
#define H5I_VFL_HASHSIZE		64
#define H5I_GENPROPCLS_HASHSIZE		64
#define H5I_GENPROPOBJ_HASHSIZE		128

/*
 * Define the following macro for fast hash calculations (but limited
 * hash sizes)
 */
#define HASH_SIZE_POWER_2

/* Define the following macro for atom caching over all the atoms */
#define IDS_ARE_CACHED

#ifdef IDS_ARE_CACHED
#  define ID_CACHE_SIZE 4	      /*# of previous atoms cached	   */
#endif

/*
 * Number of bits to use for Group ID in each atom. Increase if H5I_NGROUPS
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
#  define H5I_LOC(a,s)		((hid_t)((size_t)(a)&((s)-1)))
#  define POWER_OF_TWO(n)	((((n) - 1) & (n)) == 0 && (n) > 0)
#else
/*
 * Map an ID to a hash location.
 */
#  define H5I_LOC(a,s)	(((hid_t)(a)&ID_MASK)%(s))
#endif

/* Combine a Group number and an atom index into an atom */
#define H5I_MAKE(g,i)	((((hid_t)(g)&GROUP_MASK)<<ID_BITS)|	  \
			     ((hid_t)(i)&ID_MASK))

/*
 * Function for freeing objects. This function will be called with an object
 * ID group number (object type) and a pointer to the object. The function
 * should free the object and return non-negative to indicate that the object
 * can be removed from the ID group. If the function returns negative
 * (failure) then the object will remain in the ID group.
 */
typedef herr_t (*H5I_free_t)(void*);

/* Type of the function to compare objects & keys */
typedef int (*H5I_search_func_t)(void *obj, hid_t id, const void *key);

/* Atom information structure used */
typedef struct H5I_id_info_t {
    hid_t	id;		/* ID for this info			    */
    unsigned	count;		/* ref. count for this atom		    */
    void	*obj_ptr;	/* pointer associated with the atom	    */
    struct H5I_id_info_t *next;	/* link to next atom (in case of hash-clash)*/
} H5I_id_info_t;

/* ID group structure used */
typedef struct {
    unsigned	count;		/*# of times this group has been initialized*/
    unsigned	reserved;	/*# of IDs to reserve for constant IDs	    */
    unsigned	wrapped;	/*whether the id count has wrapped around   */
    size_t	hash_size;	/*sizeof the hash table to store the IDs in */
    unsigned	ids;		/*current number of IDs held		    */
    unsigned	nextid;		/*ID to use for the next atom		    */
    H5I_free_t	free_func;	/*release object method	    		    */
    H5I_id_info_t **id_list;	/*pointer to an array of ptrs to IDs	    */
} H5I_id_group_t;

/* Private Functions in H5I.c */
__DLL__ int H5I_init_group(H5I_type_t grp, size_t hash_size, unsigned reserved,
			    H5I_free_t func);
__DLL__ int H5I_nmembers(H5I_type_t grp);
__DLL__ herr_t H5I_clear_group(H5I_type_t grp, hbool_t force);
__DLL__ herr_t H5I_destroy_group(H5I_type_t grp);
__DLL__ hid_t H5I_register(H5I_type_t grp, void *object);
__DLL__ void *H5I_object(hid_t id);
__DLL__ H5I_type_t H5I_get_type(hid_t id);
__DLL__ void *H5I_remove(hid_t id);
__DLL__ void *H5I_search(H5I_type_t grp, H5I_search_func_t func,
			 const void *key);
__DLL__ int H5I_inc_ref(hid_t id);
__DLL__ int H5I_dec_ref(hid_t id);
#endif
