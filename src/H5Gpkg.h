/*
 * Copyright (C) 1997 National Center for Supercomputing Applications.
 *                    All rights reserved.
 *
 * Programmer: Robb Matzke <matzke@llnl.gov>
 *             Thursday, September 18, 1997
 *
 * Purpose:	This file contains declarations which are visible
 *		only within the H5G package. Source files outside the
 *		H5G package should include H5Gprivate.h instead.
 */
#ifndef H5G_PACKAGE
#error "Do not include this file outside the H5G package!"
#endif

#ifndef _H5Gpkg_H
#define _H5Gpkg_H

#include <H5ACprivate.h>
#include <H5Gprivate.h>

#define H5G_NODE_VERS	1	/*symbol table node version number	*/
#define H5G_SIZE_HINT	1024	/*default root dir size hint		*/
#define H5G_NODE_K(F) ((F)->file_create_parms.sym_leaf_k)
#define H5G_NODE_SIZEOF_HDR(F) (H5G_NODE_SIZEOF_MAGIC + 4)
#define H5G_DEFAULT_ROOT_SIZE  32

/*
 * A symbol table entry.  The two important fields are `name_off' and
 * `header'.  The remaining fields are used for caching information that
 * also appears in the object header to which this symbol table entry
 * points.
 */
struct H5G_entry_t {
   hbool_t	dirty;		/*entry out-of-date?			*/
   off_t	name_off;	/*offset of name within name heap	*/
   haddr_t	header;		/*file address of object header		*/
   H5G_type_t	type;		/*type of information cached		*/
   H5G_cache_t	cache;		/*cached data from object header	*/
   H5G_shadow_t *shadow;	/*optional ptr to the shadow		*/
};

/*
 * A shadow is a copy of a symbol table entry which corresponds to an
 * `open' object.  Shadows are necessary because normal symbol table
 * entries can be preempted from the main cache.  The `shadow' field
 * of the `entry' points to the beginning of the shadow just like the
 * shadow field from symbol table entries in H5G_node_t.
 */
struct H5G_shadow_t {
   char		*name;		/*name for this entry			*/
   haddr_t	dir_addr;	/*hdr addr for dir containing shadow	*/
   uintn	nrefs;		/*reference counter			*/
   H5G_entry_t	entry;		/*local copy of symbol table entry	*/
   H5G_entry_t	*main;		/*main entry in stab node if cached	*/
   struct H5G_shadow_t *next;	/*next shadow for same symbol table	*/
   struct H5G_shadow_t *prev;	/*previous shadow for same symbol table	*/
};

/*
 * A symbol table node is a collection of symbol table entries.  It can
 * be thought of as the lowest level of the B-link tree that points to
 * a collection of symbol table entries that belong to a specific symbol
 * table or directory.
 */
typedef struct H5G_node_t {
   int		dirty;		/*has cache been modified?		*/
   int		nsyms;		/*number of symbols			*/
   H5G_entry_t	*entry;		/*array of symbol table entries		*/
} H5G_node_t;

/*
 * Each key field of the B-link tree that points to symbol table
 * nodes consists of this structure...
 */
typedef struct H5G_node_key_t {
   off_t	offset;		/*offset into heap for name		*/
} H5G_node_key_t;

/*
 * These operations can be passed down from the H5G_stab layer to the
 * H5G_node layer through the B-tree layer.
 */
typedef enum H5G_oper_t {
   H5G_OPER_FIND	=0,	/*find a symbol				*/
   H5G_OPER_INSERT	=1 	/*insert a new symbol			*/
} H5G_oper_t;

/*
 * Data exchange structure for symbol table nodes.  This structure is
 * passed through the B-link tree layer to the methods for the objects
 * to which the B-link tree points.
 */
typedef struct H5G_bt_ud1_t {

   /* downward */
   H5G_oper_t	operation;	/*what operation to perform		*/
   const char	*name;		/*points to temporary memory		*/
   haddr_t	dir_addr;	/*symbol table header address		*/
   haddr_t	heap_addr;	/*symbol table heap address		*/

   /* downward for INSERT */
   H5G_entry_t	entry;		/*entry to insert into table		*/

   /* upward for FIND and INSERT */
   haddr_t	node_addr;	/*address of node for this entry	*/
   H5G_node_t	*node_ptr;	/*ptr to the node containing the entry	*/
   H5G_entry_t	*entry_ptr;	/*ptr into cached symbol table node	*/

} H5G_bt_ud1_t;

/*
 * Data exchange structure to pass through the B-tree layer for the
 * H5B_list function.
 */
typedef struct H5G_bt_ud2_t {

   /* downward */
   H5G_entry_t	*entry;		/*array of entries, alloc'd by caller	*/
   char		**name;		/*array of string ptrs, allocd by caller*/
   intn		maxentries;	/*size of the ADDR and NAME arrays	*/
   haddr_t	dir_addr;	/*symbol table header address		*/
   haddr_t	heap_addr;	/*heap address				*/

   /* upward */
   intn		nsyms;		/*num. symbols processed		*/
   
} H5G_bt_ud2_t;

/*
 * This is the class identifier to give to the B-tree functions.
 */
extern H5B_class_t H5B_SNODE[1];

/*
 * This struct passes information through the H5AC layer.
 */
typedef struct H5G_ac_ud1_t {
   haddr_t	heap_addr;
   haddr_t	dir_addr;
} H5G_ac_ud1_t;

/* The cache subclass */
extern const H5AC_class_t H5AC_SNODE[1];

/*
 * Functions that understand symbol tables but not directories.  The
 * functions that understand directories are exported to the rest of
 * the library and appear in H5Gprivate.h.
 */
haddr_t H5G_stab_new (hdf5_file_t *f, H5G_entry_t *self, size_t init);
H5G_entry_t *H5G_stab_find (hdf5_file_t *f, haddr_t addr, H5G_entry_t *self,
			    const char *name);
H5G_entry_t *H5G_stab_insert (hdf5_file_t *f, H5G_entry_t *self,
			      const char *name, H5G_entry_t *ent);
intn H5G_stab_list (hdf5_file_t *f, H5G_entry_t *self, intn maxentries,
		    char *names[], H5G_entry_t entries[]);

/*
 * Functions that understand shadow entries.
 */
herr_t H5G_shadow_sync (H5G_entry_t *ent);
H5G_entry_t *H5G_shadow_open (hdf5_file_t *f, H5G_entry_t *dir,
			      H5G_entry_t *ent);
herr_t H5G_shadow_close (hdf5_file_t *f, H5G_entry_t *ent);
hbool_t H5G_shadow_p (H5G_entry_t *ent);
herr_t H5G_shadow_dissociate (H5G_entry_t *ent);
herr_t H5G_shadow_assoc_node (hdf5_file_t *f, H5G_node_t *sym,
			      H5G_ac_ud1_t *ac_udata);
H5G_shadow_t *H5G_shadow_list (hdf5_file_t *f, haddr_t stab_header_addr);
herr_t H5G_shadow_move (hdf5_file_t *f, H5G_shadow_t *shadow,
			const char *new_name, H5G_entry_t *new_entry,
			haddr_t dir_addr);

/*
 * Functions that understand symbol table entries.
 */
herr_t H5G_ent_decode_vec (hdf5_file_t *f, uint8 **pp, H5G_entry_t *ent,
			   intn n);
herr_t H5G_ent_encode_vec (hdf5_file_t *f, uint8 **pp, H5G_entry_t *ent,
			   intn n);

#endif
