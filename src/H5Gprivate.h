/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		H5Gprivate.h
 * 			Jul 11 1997
 * 			Robb Matzke <robb@maya.nuance.com>
 *
 * Purpose:		Private stuff for the H5G package (symbol tables).
 *
 * Modifications:	
 *
 *-------------------------------------------------------------------------
 */
#ifndef _H5Gprivate_H
#define _H5Gprivate_H

/*include public declarations		*/
#include "H5Gproto.h"

/*include packages needed by this header*/
#include "H5Bprivate.h"
#include "H5Fprivate.h"

#define H5G_NODE_MAGIC	"SNOD"	/*symbol table node magic number	*/
#define H5G_NODE_VERS	1	/*symbol table node version number	*/
#define H5G_NODE_K	64	/*min degree.  max degree is twice this	*/
#define H5G_HDR_SIZE(F)	8

/*
 * Various types of object header information can be cached in a symbol
 * table entry (it's normal home is the object header to which the entry
 * points).  This datatype determines what (if anything) is cached in the
 * symbol table entry.
 */
typedef enum H5G_type_t {
   H5G_NOTHING_CACHED		=0,	/*nothing is cached		*/
   H5G_CACHED_SDATA		=1,	/*simple dataset, `sdata'	*/
   H5G_CACHED_SYMTAB		=2 	/*symbol table, `symtab'	*/
} H5G_type_t;

/*
 * A symbol table entry.  The two important fields are `name_off' and
 * `header'.  The remaining fields are used for caching information that
 * also appears in the object header to which this symbol table entry
 * points.
 */
typedef struct H5G_entry_t {
   off_t	name_off;	/*offset of name within name heap	*/
   haddr_t	header;		/*file address of object header		*/
   H5G_type_t	type;		/*type of information cached		*/

   union {
      struct {
	 uint32	nt;		/*number type				*/
	 uint32 ndim;		/*number of dimensions			*/
	 uint32 dim[4];		/*dimension sizes			*/
      } sdata;

      struct {
	 haddr_t btree;		/*file address of symbol table B-tree	*/
	 haddr_t heap;		/*file address of symtab name heap	*/
      } symtab;
   } cache;			/*cached data from object header	*/
} H5G_entry_t;

/*
 * A symbol table node is a collection of symbol table entries.  It can
 * be thought of as the lowest level of the B-link tree that points to
 * a collection of symbol table entries that belong to a specific symbol
 * table or directory.
 */
typedef struct H5G_node_t {
   int		dirty;		/*has cache been modified?		*/
   int		nsyms;		/*number of symbols			*/
   H5G_entry_t	*entry;		/*symbol table entries			*/
} H5G_node_t;

/*
 * Each key field of the B-link tree that points to symbol table
 * nodes consists of this structure...
 */
typedef struct H5G_node_key_t {
   off_t	offset;		/*offset into heap for name		*/
} H5G_node_key_t;

typedef enum H5G_oper_t {
   H5G_OPER_FIND	=0,	/*find a symbol				*/
   H5G_OPER_MODIFY	=1	/*modify a symbol			*/
} H5G_oper_t;

/*
 * Data exchange structure for symbol table nodes.  This structure is
 * passed through the B-link tree layer to the methods for the objects
 * to which the B-link tree points.
 */
typedef struct H5G_node_ud1_t {

   /* downward */
   H5G_oper_t	operation;	/*what operation to perform		*/
   const char	*name;		/*points to temporary memory		*/
   haddr_t	heap;		/*symbol table heap address		*/

   /* upward for H5G_OPER_FIND, downward for H5G_OPER_MODIFY */
   H5G_entry_t	entry;		/*symbol table entry			*/

} H5G_node_ud1_t;

typedef struct H5G_node_list_t {

   /* downward */
   H5G_entry_t	*entry;		/*array of entries, alloc'd by caller	*/
   char		**name;		/*array of string ptrs, allocd by caller*/
   intn		maxentries;	/*size of the ADDR and NAME arrays	*/
   haddr_t	heap;		/*heap address				*/

   /* upward */
   intn		nsyms;		/*num. symbols processed		*/
   
} H5G_node_list_t;

extern const H5B_class_t H5B_SNODE[1];

/*
 * Library prototypes...
 */
haddr_t H5G_new (hdf5_file_t *f, size_t init);
haddr_t H5G_find (hdf5_file_t *f, haddr_t addr, const char *name,
		  H5G_entry_t *entry);
herr_t H5G_modify (hdf5_file_t *f, haddr_t addr, const char *name,
		   H5G_entry_t *entry);
herr_t H5G_insert (hdf5_file_t *f, haddr_t addr, const char *name,
		   H5G_entry_t *entry);
intn H5G_list (hdf5_file_t *f, haddr_t addr, int maxentries,
	       char *names[], H5G_entry_t entries[]);
herr_t H5G_decode (hdf5_file_t *f, uint8 **pp, H5G_entry_t *ent);
herr_t H5G_decode_vec (hdf5_file_t *f, uint8 **pp, H5G_entry_t *ent, intn n);
herr_t H5G_encode (hdf5_file_t *f, uint8 **pp, H5G_entry_t *ent);
herr_t H5G_encode_vec (hdf5_file_t *f, uint8 **pp, H5G_entry_t *ent, intn n);


#endif

