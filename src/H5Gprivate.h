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
#define H5G_NO_CHANGE	(-1)	/*see H5G_ent_modified()		*/
#define H5G_NSHADOWS	10331	/*default size of shadow hash table	*/

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
typedef struct H5G_entry_t H5G_entry_t;

/*
 * Library prototypes...  These are the ones that other packages routinely
 * call.
 */
H5G_entry_t *H5G_new (H5F_t *f, const char *name, size_t size_hint);
herr_t H5G_set (H5F_t *f, const char *name);
herr_t H5G_push (H5F_t *f, const char *name);
herr_t H5G_pop (H5F_t *f);
H5G_entry_t *H5G_create (H5F_t *f, const char *name, size_t ohdr_hint);
H5G_entry_t *H5G_open (H5F_t *f, const char *name);
herr_t H5G_close (H5F_t *f, H5G_entry_t *ent);
herr_t H5G_find (H5F_t *f, const char *name, H5G_entry_t *grp_ent,
		 H5G_entry_t *ent);
herr_t H5G_ent_encode (H5F_t *f, uint8 **pp, H5G_entry_t *ent);
herr_t H5G_ent_decode (H5F_t *f, uint8 **pp, H5G_entry_t *ent);

/*
 * These functions operate on symbol table nodes.
 */
herr_t H5G_node_debug (H5F_t *f, haddr_t addr, FILE *stream, intn indent,
		       intn fwidth, haddr_t heap);

/*
 * These functions operate on shadow entries.
 */
herr_t H5G_shadow_flush (H5F_t *f, hbool_t invalidate);

/*
 * These functions operate on symbol table entries.  They're used primarily
 * in the H5O package where header messages are cached in symbol table
 * entries.  The subclasses of H5O probably don't need them though.
 */
H5G_entry_t *H5G_ent_calloc (void);
herr_t H5G_ent_invalidate (H5G_entry_t *ent);
haddr_t H5G_ent_addr (H5G_entry_t *ent);
H5G_cache_t *H5G_ent_cache (H5G_entry_t *ent, H5G_type_t *cache_type);
herr_t H5G_ent_modified (H5G_entry_t *ent, H5G_type_t cache_type);
herr_t H5G_ent_debug (H5F_t *f, H5G_entry_t *ent, FILE *stream,
		      intn indent, intn fwidth);

#endif

