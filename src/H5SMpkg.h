/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  James Laird <jlaird@ncsa.uiuc.edu>
 *              Thursday, March 30, 2006
 *
 * Purpose:     This file contains declarations which are visible only within
 *              the H5SM shared object header messages package.  Source files
 *              outside the H5SM package should	include H5SMprivate.h instead.
 */
#ifndef _H5SMpkg_H
#define _H5SMpkg_H

#include "H5SMprivate.h"
#include "H5B2private.h"        /* B-trees                              */
#include "H5HFprivate.h"        /* Fractal heaps		  	*/


/****************************/
/* Package Macros           */
/****************************/
#define H5SM_LIST_MAGIC "SMLI"
#define H5SM_LIST_SIZEOF_MAGIC 4
#define H5SM_TABLE_MAGIC "SMTB"
#define H5SM_TABLE_SIZEOF_MAGIC 4

#define H5SM_MASTER_TABLE_VERSION	0	/* Version of the Shared Object Header Message Master Table*/

#define H5SM_SOHM_ENTRY_SIZE(f) (4  /* Hash value */                         \
         + 4                      /* reference count*/                       \
         + 8)    /* JAMES: size of heap ID on disk */

#define H5SM_TABLE_SIZE(f) ( H5SM_TABLE_SIZEOF_MAGIC                         \
         + 1                                   /* Table version */           \
         + 0/* JAMES checksum */)                                  /* Checksum */

#define H5SM_INDEX_HEADER_SIZE(f) (1 /* Whether index is a list or B-tree */ \
         + 2         /* Type of messages stored in the index */              \
         + 4         /* Minimum size of messages to share */                 \
         + (3 * 2)   /* B-tree cutoff, list cutoff, # of shared messages */  \
         + H5F_SIZEOF_ADDR(f) /* Location of list or B-tree */               \
         + H5F_SIZEOF_ADDR(f)) /* Address of heap */

/* JAMES: add checksum? */
#define H5SM_LIST_SIZE(f, num_mesg) H5SM_LIST_SIZEOF_MAGIC                   \
         + 1        /* List version */                                       \
         + (H5SM_SOHM_ENTRY_SIZE(f) * num_mesg)

#define H5SM_MAX_INDEXES 8
#define H5SM_MAX_LIST_ELEMS 1000

/****************************/
/* Package Typedefs         */
/****************************/

/* There are a number of Shared Object Header Message-specific structs here.
 *
 * The H5SM_master_table_t is pointed to by the file superblock.  Since a file
 * can have more than one SOHM index, this table collects all the indexes into
 * one place.  It holds an array of H5SM_index_header_t structs.
 *
 * An H5SM_index_header_t is actually the for a given index.  It holds
 * the number of messages in the index, the types of messages in the index,
 * etc.  It also records whether the index is a list or a b-tree, and has
 * the address of the list or b-tree.
 *
 * If the index is a list, the address in the index header should be given
 * to the cache, which can load it into a H5SM_list_t struct.  This is mostly
 * just a header for the cache information; it contains a pointer back to
 * the index header and an unsorted array of messages.
 *
 * These messages are H5SM_sohm_t structs.  They hold the actual SOHM's
 * address, hash value, and refcount.
 *
 * If the index is a b-tree, the H5SM_index_header_t struct holds the address
 * of the b-tree instead of the address of a H5SM_list_t.  The B-tree's nodes
 * are still 'H5SM_sohm_t's.
 *
 * H5SM_mesg_key_t structs are used to search lists and B-trees for a certain
 * message.  They correspond to a message that hasn't yet been written to
 * disk.
 */

/* Declare free lists to manage H5SM structs */
H5FL_ARR_EXTERN(H5SM_index_header_t);
H5FL_EXTERN(H5SM_list_t);
H5FL_ARR_EXTERN(H5SM_sohm_t);

typedef enum {
  H5SM_BADTYPE = -1,
  H5SM_LIST,    /* Index is an unsorted list */
  H5SM_BTREE    /* Index is a sorted B-tree */
} H5SM_index_type_t;

/* Typedef for searching an index (list or B-tree) */
typedef struct {
  uint32_t hash;    /* The hash value for this message */
  const void *encoding; /* The message encoded */
  hsize_t encoding_size; /* Size of the encoding */
  H5HF_t *fheap;    /* The heap for this message type, open. */
  H5SM_fheap_id_t mesg_heap_id; /* The heap_id for this message */
} H5SM_mesg_key_t;

/* Typedef for a SOHM index header */
typedef struct {
  unsigned mesg_types;          /* Bit flag vector of message types */
  size_t min_mesg_size;         /* number of messages being tracked */
  size_t list_to_btree;         /* >= this many messages, index with a B-tree */
  size_t btree_to_list;         /* <= this many messages, index with a list again */
  size_t num_messages;          /* number of messages being tracked */
  H5SM_index_type_t index_type; /* Is the index a list or a B-tree? */
  haddr_t index_addr;           /* Address of the actual index (list or B-tree) */
  haddr_t heap_addr;            /* Address of the fheap used to store shared messages */
} H5SM_index_header_t;

/* Typedef for a SOHM list */
typedef struct {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    H5SM_index_header_t *header;    /* Pointer to the corresponding index header */
    H5SM_sohm_t *messages;          /* Actual list, stored as an array */
} H5SM_list_t;


/* Typedef for shared object header message master table */
typedef struct {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    unsigned version;               /* Version of the table struct */
    uint8_t num_indexes;            /* Number of indexes */
    H5SM_index_header_t *indexes;   /* Array of num_indexes indexes */
} H5SM_master_table_t;


#define H5SM_B2_NODE_SIZE 512
#define H5SM_B2_SPLIT_PERCENT 100
#define H5SM_B2_MERGE_PERCENT 40

/****************************/
/* Package Variables        */
/****************************/
H5_DLLVAR const H5AC_class_t H5AC_SOHM_TABLE[1];
H5_DLLVAR const H5AC_class_t H5AC_SOHM_LIST[1];

H5_DLLVAR const H5B2_class_t H5SM_INDEX[1];

/****************************/
/* Package Prototypes       */
/****************************/
/* Encode and decode routines, used for B-tree and cache encoding/decoding */
H5_DLL herr_t H5SM_message_encode(const H5F_t *f, uint8_t *raw,
    const void *native);
H5_DLL herr_t H5SM_message_decode(const H5F_t *f, const uint8_t *raw,
    void *native);

/* Callbacks to give to B-tree traversals */
/* H5SM_message_compare is in H5SMbtree2.c, but is also used by list code
 * in H5SM.c.
 */
H5_DLL herr_t H5SM_message_compare(const H5SM_mesg_key_t *rec1,
                                   const H5SM_sohm_t *rec2);

/* H5B2_modify_t callbacks to adjust record's refcount. */
H5_DLL herr_t H5SM_incr_ref(void *record, void *op_data, hbool_t *changed);
H5_DLL herr_t H5SM_decr_ref(void *record, void *op_data, hbool_t *changed);

/* H5B2_remove_t callback to add messages to a list index */
H5_DLL herr_t H5SM_convert_to_list_op(void * record, void *op_data);

#endif /*_H5SMpkg_H*/
