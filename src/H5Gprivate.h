/*-------------------------------------------------------------------------
 * Copyright (C) 1997   National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:             H5Gprivate.h
 *                      Jul 11 1997
 *                      Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:             Library-visible declarations.
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

/*
 * Define this to enable debugging.
 */
#ifdef NDEBUG
#  undef H5G_DEBUG
#endif
#define H5G_NODE_MAGIC  "SNOD"          /*symbol table node magic number     */
#define H5G_NODE_SIZEOF_MAGIC 4         /*sizeof symbol node magic number    */
#define H5G_NO_CHANGE   (-1)            /*see H5G_ent_modified()             */

/*
 * The disk size for a symbol table entry...
 */
#define H5G_SIZEOF_SCRATCH      24
#define H5G_SIZEOF_ENTRY(F)                                                   \
   (H5F_SIZEOF_SIZE(F) +        /*offset of name into heap              */    \
    H5F_SIZEOF_ADDR(F) +        /*address of object header              */    \
    4 +                         /*entry type                            */    \
    H5G_SIZEOF_SCRATCH)         /*scratch pad space                       */

/*
 * Various types of object header information can be cached in a symbol
 * table entry (it's normal home is the object header to which the entry
 * points).  This datatype determines what (if anything) is cached in the
 * symbol table entry.
 */
typedef enum H5G_type_t {
    H5G_NOTHING_CACHED   = 0,   /*nothing is cached, must be 0               */
    H5G_CACHED_STAB      = 1,   /*symbol table, `stab'                       */

    H5G_NCACHED          = 2    /*THIS MUST BE LAST                          */
} H5G_type_t;

/*
 * A symbol table entry caches these parameters from object header
 * messages...  The values are entered into the symbol table when an object
 * header is created (by hand) and are extracted from the symbol table with a
 * callback function registered in H5O_init_interface().  Be sure to update
 * H5G_ent_decode(), H5G_ent_encode(), and H5G_ent_debug() as well.
 */
typedef union H5G_cache_t {
    struct {
        haddr_t btree_addr;             /*file address of symbol table B-tree*/
        haddr_t heap_addr;              /*file address of stab name heap     */
    } stab;
} H5G_cache_t;

/*
 * A symbol table entry.  The two important fields are `name_off' and
 * `header'.  The remaining fields are used for caching information that
 * also appears in the object header to which this symbol table entry
 * points.
 */
typedef struct H5G_entry_t {
    hbool_t     dirty;                  /*entry out-of-date?                 */
    size_t      name_off;               /*offset of name within name heap    */
    haddr_t     header;                 /*file address of object header      */
    H5G_type_t  type;                   /*type of information cached         */
    H5G_cache_t cache;                  /*cached data from object header     */
    H5F_t       *file;                  /*file to which this obj hdr belongs */
} H5G_entry_t;
typedef struct H5G_t H5G_t;

/*
 * Library prototypes...  These are the ones that other packages routinely
 * call.
 */
H5G_t *H5G_create (H5F_t *f, const char *name, size_t size_hint);
H5G_t *H5G_open (H5F_t *f, const char *name);
H5G_t *H5G_reopen (H5G_t *grp);
herr_t H5G_close (H5G_t *grp);
herr_t H5G_set (H5F_t *f, H5G_t *grp);
herr_t H5G_push (H5F_t *f, H5G_t *grp);
herr_t H5G_pop (H5F_t *f);
herr_t H5G_insert (const char *name, H5G_entry_t *ent);
herr_t H5G_find (H5F_t *f, const char *name, H5G_entry_t *grp_ent/*out*/,
                 H5G_entry_t *ent/*out*/);
herr_t H5G_ent_encode (H5F_t *f, uint8 **pp, H5G_entry_t *ent);
herr_t H5G_ent_decode (H5F_t *f, const uint8 **pp, H5G_entry_t *ent/*out*/);

/*
 * These functions operate on symbol table nodes.
 */
herr_t H5G_node_debug (H5F_t *f, const haddr_t *addr, FILE * stream,
                       intn indent, intn fwidth, const haddr_t *heap);

/*
 * These functions operate on symbol table entries.  They're used primarily
 * in the H5O package where header messages are cached in symbol table
 * entries.  The subclasses of H5O probably don't need them though.
 */
H5G_entry_t *H5G_ent_calloc (H5G_entry_t *init);
H5G_cache_t *H5G_ent_cache (H5G_entry_t *ent, H5G_type_t *cache_type);
herr_t H5G_ent_modified (H5G_entry_t *ent, H5G_type_t cache_type);
herr_t H5G_ent_debug (H5F_t *f, H5G_entry_t *ent, FILE * stream, intn indent,
                      intn fwidth);
#endif

