/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		H5Gprivate.h
 * 			Jul 11 1997
 * 			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		Library-visible declarations.
 *
 * Modifications:	
 *
 *-------------------------------------------------------------------------
 */
#ifndef _H5Gprivate_H
#define _H5Gprivate_H
#include <H5Gpublic.h>

/* Private headers needed by this file */
#include <H5private.h>
#include <H5Bprivate.h>
#include <H5Fprivate.h>

#define H5G_NODE_MAGIC	"SNOD"	/*symbol table node magic number	*/
#define H5G_NODE_SIZEOF_MAGIC 4 /*sizeof symbol node magic number	*/
#define H5G_new_entry() H5MM_xcalloc (1, sizeof(H5G_entry_t))

/*
 * The disk size for a symbol table entry...
 */
#define H5G_SIZEOF_ENTRY(F)						      \
   (H5F_SIZEOF_OFFSET(F) +	/*offset of name into heap     		*/    \
    H5F_SIZEOF_OFFSET(F) +	/*address of object header		*/    \
    4 +				/*entry type				*/    \
    24)				/*scratch pad space			*/
    
/*
 * Various types of object header information can be cached in a symbol
 * table entry (it's normal home is the object header to which the entry
 * points).  This datatype determines what (if anything) is cached in the
 * symbol table entry.
 */
typedef enum H5G_type_t {
   H5G_NOTHING_CACHED		=0,	/*nothing is cached, must be 0	*/
   H5G_CACHED_SDATA		=1,	/*simple dataset, `sdata'	*/
   H5G_CACHED_STAB		=2 	/*symbol table, `stab'		*/
} H5G_type_t;

/*
 * A symbol table entry caches these parameters from object header
 * messages...
 */
typedef union H5G_cache_t {
   struct {
      struct	{
	 uint8 length;
	 uint8 arch;
	 uint16 type;
      } nt ;			/*number type				*/
      uint32 ndim;		/*number of dimensions			*/
      uint32 dim[4];		/*dimension sizes			*/
   } sdata;

   struct {
      haddr_t btree_addr;	/*file address of symbol table B-tree	*/
      haddr_t heap_addr;	/*file address of stab name heap	*/
   } stab;
} H5G_cache_t;

/*
 * An H5G_shadow_t is the struct used to describe object headers that
 * are currently open for modification.  It's contents is not
 * important outside H5G.
 */
typedef struct H5G_shadow_t H5G_shadow_t;

/*
 * A symbol table entry.  The two important fields are `name_off' and
 * `header'.  The remaining fields are used for caching information that
 * also appears in the object header to which this symbol table entry
 * points.
 */
typedef struct H5G_entry_t {
   hbool_t	dirty;		/*entry out-of-date?			*/
   off_t	name_off;	/*offset of name within name heap	*/
   haddr_t	header;		/*file address of object header		*/
   H5G_type_t	type;		/*type of information cached		*/
   H5G_cache_t	cache;		/*cached data from object header	*/
   H5G_shadow_t *shadow;	/*optional ptr to the shadow		*/
} H5G_entry_t;

/*
 * Library prototypes...
 */
herr_t H5G_new (hdf5_file_t *f, H5G_entry_t *cwd, H5G_entry_t *dir_ent,
		const char *name, size_t size_hint, H5G_entry_t *ent);
H5G_entry_t *H5G_open (hdf5_file_t *f, H5G_entry_t *cwd, const char *name);
herr_t H5G_close (hdf5_file_t *f, H5G_entry_t *ent);
herr_t H5G_find (hdf5_file_t *f, H5G_entry_t *cwd, H5G_entry_t *dir_ent,
		 const char *name, H5G_entry_t *ent);
herr_t H5G_insert (hdf5_file_t *f, H5G_entry_t *cwd, H5G_entry_t *dir_ent,
		   const char *name, H5G_entry_t *ent);
herr_t H5G_set_root (hdf5_file_t *f, const char *name, H5G_entry_t *ent);
herr_t H5G_encode (hdf5_file_t *f, uint8 **pp, H5G_entry_t *ent);
herr_t H5G_decode (hdf5_file_t *f, uint8 **pp, H5G_entry_t *ent);
herr_t H5G_debug (hdf5_file_t *f, H5G_entry_t *ent, FILE *stream, intn indent,
		  intn fwidth);
herr_t H5G_node_debug (hdf5_file_t *f, haddr_t addr, FILE *stream, intn indent,
		       intn fwidth, haddr_t heap);


#endif

