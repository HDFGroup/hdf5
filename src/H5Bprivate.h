/*-------------------------------------------------------------------------
 * Copyright (C) 1997	National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:		H5Bprivate.h
 * 			Jul 10 1997
 * 			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		Private non-prototype header.
 *
 * Modifications:	
 *
 *-------------------------------------------------------------------------
 */
#ifndef _H5Bprivate_H
#define _H5Bprivate_H
#include <H5Bpublic.h>		/*API prototypes			*/

/* Private headers needed by this file */
#include <H5private.h>
#include <H5Fprivate.h>

#define H5B_MAGIC	"TREE"	/*tree node magic number		*/
#define H5B_SIZEOF_MAGIC 4	/*size of magic number			*/

#define H5B_SIZEOF_HDR(F)						      \
   (H5B_SIZEOF_MAGIC +		/*magic number				*/    \
    4 +				/*type, level, num entries		*/    \
    2*H5F_SIZEOF_OFFSET(F))	/*left and right sibling addresses	*/

#define H5B_K(F,TYPE)		/*K value given file and Btree subclass	*/    \
   ((F)->file_create_parms.btree_k[(TYPE)->id])

#define H5B_ANCHOR_LT	0	/* left node is anchored, right is new	*/
#define H5B_ANCHOR_RT	1	/* right node is anchored, left is new	*/

typedef enum H5B_subid_t {
   H5B_SNODE_ID		=0	/*B-tree is for symbol table nodes	*/
} H5B_subid_t;
   

/*
 * Each class of object that can be pointed to by a B-link tree has a
 * variable of this type that contains class variables and methods.  Each
 * tree has a K (1/2 rank) value on a per-file basis.  The file_create_parms
 * has an array of K values indexed by the `id' class field below.  The
 * array is initialized with the HDF5_BTREE_K_DEFAULT macro.
 */
typedef struct H5B_class_t {
   H5B_subid_t	id;		/*id as found in file			*/
   size_t	sizeof_nkey;	/*size of native (memory) key		*/
   size_t	(*get_sizeof_rkey)(hdf5_file_t*);
   haddr_t	(*new)(hdf5_file_t*,void*,void*,void*);
   intn		(*cmp)(hdf5_file_t*,void*,void*,void*);
   herr_t	(*found)(hdf5_file_t*,haddr_t,const void*,void*,const void*);
   haddr_t	(*insert)(hdf5_file_t*,haddr_t,int*,void*,hbool_t*,void*,void*,
			  void*,hbool_t*);
   herr_t	(*list)(hdf5_file_t*,haddr_t,void*);
   herr_t	(*decode)(hdf5_file_t*,uint8*,void*);
   herr_t	(*encode)(hdf5_file_t*,uint8*,void*);
} H5B_class_t;

/*
 * The B-tree node as stored in memory...
 */
typedef struct H5B_key_t {
   hbool_t	dirty;		/*native key is more recent than raw key*/
   uint8	*rkey;		/*ptr into node->page for raw key	*/
   void		*nkey;		/*null or ptr into node->native for key	*/
} H5B_key_t;

typedef struct H5B_t {
   const H5B_class_t *type;	/*type of tree				*/
   size_t	sizeof_rkey;	/*size of raw (disk) key		*/
   hbool_t	dirty;		/*something in the tree is dirty	*/
   intn		ndirty;		/*num child ptrs to emit		*/
   intn		level;		/*node level				*/
   haddr_t	left;		/*address of left sibling		*/
   haddr_t	right;		/*address of right sibling		*/
   intn		nchildren;	/*number of child pointers		*/
   uint8	*page;		/*disk page				*/
   uint8	*native;	/*array of keys in native format	*/
   H5B_key_t	*key;		/*2k+1 key entries			*/
   haddr_t	*child;		/*2k child pointers			*/
} H5B_t;


/*
 * Library prototypes.
 */
herr_t H5B_debug (hdf5_file_t *f, haddr_t addr, FILE *stream, intn indent,
		  intn fwidth, const H5B_class_t *type);
haddr_t H5B_new (hdf5_file_t *f, const H5B_class_t *type);
herr_t H5B_find (hdf5_file_t *f, const H5B_class_t *type, haddr_t addr,
		 void *udata);
haddr_t H5B_insert (hdf5_file_t *f, const H5B_class_t *type, haddr_t addr,
		    void *udata);
herr_t H5B_list (hdf5_file_t *f, const H5B_class_t *type, haddr_t addr,
		 void *udata);


#endif
