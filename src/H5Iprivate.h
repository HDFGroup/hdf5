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
 * File:    H5Iprivate.h
 * Purpose: header file for ID API
 *---------------------------------------------------------------------------*/

/* avoid re-inclusion */
#ifndef _H5Iprivate_H
#define _H5Iprivate_H

#include <H5Ipublic.h>          /*include Public Definitions                 */

/* Private headers needed by this file */
#include <H5private.h>

/* Default sizes of the hash-tables for various atom groups */
#define H5I_ERRSTACK_HASHSIZE   	64
#define H5I_FILEID_HASHSIZE     	64
#define H5I_TEMPID_HASHSIZE     	64
#define H5I_DATATYPEID_HASHSIZE 	64
#define H5I_DATASPACEID_HASHSIZE 	64
#define H5I_DATASETID_HASHSIZE  	64
#define H5I_OID_HASHSIZE        	64
#define H5I_GROUPID_HASHSIZE    	64
#define H5I_ATTRID_HASHSIZE         64
#define H5I_TEMPBUFID_HASHSIZE      64
#define H5I_RAGGED_HASHSIZE         64
#define H5I_REFID_HASHSIZE          64

/* Atom information structure used */
typedef struct H5I_id_info_t {
    hid_t              id;       /* ID for this info                   */
    uintn              count;    /* ref. count for this atom                */
    void               *obj_ptr; /* pointer associated with the atom 	   */
    struct H5I_id_info_t *next;	 /* link to next atom (in case of hash-clash)*/
} H5I_id_info_t;

/* ID group structure used */
typedef struct {
    uintn       count;          /*# of times this group has been initialized */
    uintn       reserved;       /*# of IDs to reserve for constant IDs   */
    uintn       wrapped;	    /*whether the id count has wrapped around    */
    size_t      hash_size;      /*sizeof the hash table to store the IDs in*/
    uintn       ids;            /*current number of IDs held               */
    uintn       nextid;         /*ID to use for the next atom           */
    herr_t      (*free_func)(void*);/*func to call to release object	     */
    H5I_id_info_t **id_list;    /*pointer to an array of ptrs to IDs       */
} H5I_id_group_t;

/* Type of the function to compare objects & keys */
typedef intn (*H5I_search_func_t) (void * obj, const void * key);

/* Private Functions in H5I.c */
intn H5I_init_group (H5I_type_t grp, size_t hash_size, uintn reserved,
		     herr_t (*free_func)(void *));
herr_t H5I_destroy_group (H5I_type_t grp);
hid_t H5I_register (H5I_type_t grp, void *object);
void *H5I_object (hid_t id);
H5I_type_t H5I_get_type (hid_t id);
void *H5I_remove (hid_t id);
void *H5I_search (H5I_type_t grp, H5I_search_func_t func, const void *key);
void H5I_term_interface (void);
intn H5I_dec_ref (hid_t id);
hid_t H5I_inc_ref (hid_t id);

#endif
