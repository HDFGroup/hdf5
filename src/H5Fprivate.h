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

/* $Id$ */

/*
 * This file contains macros & information for file access
 */

#ifndef _H5Fprivate_H
#define _H5Fprivate_H

#include "H5Fpublic.h"

/* This is a near top-level header! Try not to include much! */
#include "H5FDpublic.h"		/*file drivers				     */

typedef struct H5F_t H5F_t;

/*
 * Encode and decode macros for file meta-data.
 * Currently, all file meta-data is little-endian.
 */

#  define INT16ENCODE(p, i) {						      \
   *(p) = (uint8_t)( (unsigned)(i)	    & 0xff); (p)++;			      \
   *(p) = (uint8_t)(((unsigned)(i) >> 8) & 0xff); (p)++;			      \
}

#  define UINT16ENCODE(p, i) {						      \
   *(p) = (uint8_t)(	    (i)	    & 0xff); (p)++;			      \
   *(p) = (uint8_t)(((unsigned)(i) >> 8) & 0xff); (p)++;			      \
}

#  define INT32ENCODE(p, i) {						      \
   *(p) = (uint8_t)( (uint32_t)(i)	  & 0xff); (p)++;		      \
   *(p) = (uint8_t)(((uint32_t)(i) >>  8) & 0xff); (p)++;		      \
   *(p) = (uint8_t)(((uint32_t)(i) >> 16) & 0xff); (p)++;		      \
   *(p) = (uint8_t)(((uint32_t)(i) >> 24) & 0xff); (p)++;		      \
}

#  define UINT32ENCODE(p, i) {						      \
   *(p) = (uint8_t)( (i)        & 0xff); (p)++;				      \
   *(p) = (uint8_t)(((i) >>  8) & 0xff); (p)++;				      \
   *(p) = (uint8_t)(((i) >> 16) & 0xff); (p)++;				      \
   *(p) = (uint8_t)(((i) >> 24) & 0xff); (p)++;				      \
}

#  define INT64ENCODE(p, n) {						      \
   int64_t _n = (n);							      \
   size_t _i;								      \
   uint8_t *_p = (uint8_t*)(p);						      \
   for (_i=0; _i<sizeof(int64_t); _i++, _n>>=8) {			      \
      *_p++ = (uint8_t)(_n & 0xff);					      \
   }									      \
   for (/*void*/; _i<8; _i++) {						      \
      *_p++ = (n)<0 ? 0xff : 0;						      \
   }									      \
   (p) = (uint8_t*)(p)+8;						      \
}

#  define UINT64ENCODE(p, n) {						      \
   uint64_t _n = (n);							      \
   size_t _i;								      \
   uint8_t *_p = (uint8_t*)(p);						      \
   for (_i=0; _i<sizeof(uint64_t); _i++, _n>>=8) {			      \
      *_p++ = (uint8_t)(_n & 0xff);					      \
   }									      \
   for (/*void*/; _i<8; _i++) {						      \
      *_p++ = 0;							      \
   }									      \
   (p) = (uint8_t*)(p)+8;						      \
}

/* DECODE converts little endian bytes pointed by p to integer values and store
 * it in i.  For signed values, need to do sign-extension when converting
 * the last byte which carries the sign bit.
 * The macros does not require i be of a certain byte sizes.  It just requires
 * i be big enough to hold the intended value range.  E.g. INT16DECODE works
 * correctly even if i is actually a 64bit int like in a Cray.
 */

#  define INT16DECODE(p, i) {						      \
   (i)	= (int16_t)((*(p) & 0xff));      (p)++;				      \
   (i) |= (int16_t)(((*(p) & 0xff) << 8) |                                    \
                   ((*(p) & 0x80) ? ~0xffff : 0x0)); (p)++;		      \
}

#  define UINT16DECODE(p, i) {						      \
   (i)	= (uint16_t) (*(p) & 0xff);	  (p)++;			      \
   (i) |= (uint16_t)((*(p) & 0xff) << 8); (p)++;			      \
}

#  define INT32DECODE(p, i) {						      \
   (i)	= (	     *(p) & 0xff);	  (p)++;			      \
   (i) |= ((int32_t)(*(p) & 0xff) <<  8); (p)++;			      \
   (i) |= ((int32_t)(*(p) & 0xff) << 16); (p)++;			      \
   (i) |= ((int32_t)(((*(p) & 0xff) << 24) |                                  \
                   ((*(p) & 0x80) ? ~0xffffffff : 0x0))); (p)++;	      \
}

#  define UINT32DECODE(p, i) {						      \
   (i)	=  (uint32_t)(*(p) & 0xff);	   (p)++;			      \
   (i) |= ((uint32_t)(*(p) & 0xff) <<  8); (p)++;			      \
   (i) |= ((uint32_t)(*(p) & 0xff) << 16); (p)++;			      \
   (i) |= ((uint32_t)(*(p) & 0xff) << 24); (p)++;			      \
}

#  define INT64DECODE(p, n) {						      \
   /* WE DON'T CHECK FOR OVERFLOW! */					      \
   size_t _i;								      \
   n = 0;								      \
   (p) += 8;								      \
   for (_i=0; _i<sizeof(int64_t); _i++) {				      \
      n = (n<<8) | *(--p);						      \
   }									      \
   (p) += 8;								      \
}

#  define UINT64DECODE(p, n) {						      \
   /* WE DON'T CHECK FOR OVERFLOW! */					      \
   size_t _i;								      \
   n = 0;								      \
   (p) += 8;								      \
   for (_i=0; _i<sizeof(uint64_t); _i++) {				      \
      n = (n<<8) | *(--p);						      \
   }									      \
   (p) += 8;								      \
}

#define NBYTEENCODE(d, s, n) {	 HDmemcpy(d,s,n); p+=n }

/*
 * Note:  the NBYTEDECODE macro is backwards from the memcpy() routine, in
 *	  the spirit of the other DECODE macros.
 */
#define NBYTEDECODE(s, d, n) {	 HDmemcpy(d,s,n); p+=n }

/* Address-related macros */
#define H5F_addr_overflow(X,Z)	(HADDR_UNDEF==(X) ||			      \
				 HADDR_UNDEF==(X)+(haddr_t)(Z) ||	      \
				 (X)+(haddr_t)(Z)<(X))
#define H5F_addr_hash(X,M)	((unsigned)((X)%(M)))
#define H5F_addr_defined(X)	(X!=HADDR_UNDEF)
#define H5F_addr_eq(X,Y)	((X)!=HADDR_UNDEF &&			      \
				 (Y)!=HADDR_UNDEF &&			      \
				 (X)==(Y))
#define H5F_addr_ne(X,Y)	(!H5F_addr_eq((X),(Y)))
#define H5F_addr_lt(X,Y) 	((X)!=HADDR_UNDEF &&			      \
				 (Y)!=HADDR_UNDEF &&			      \
				 (X)<(Y))
#define H5F_addr_le(X,Y)	((X)!=HADDR_UNDEF &&			      \
				 (Y)!=HADDR_UNDEF &&			      \
				 (X)<=(Y))
#define H5F_addr_gt(X,Y)	((X)!=HADDR_UNDEF &&			      \
				 (Y)!=HADDR_UNDEF &&			      \
				 (X)>(Y))
#define H5F_addr_ge(X,Y)	((X)!=HADDR_UNDEF &&			      \
				 (Y)!=HADDR_UNDEF &&			      \
				 (X)>=(Y))
#define H5F_addr_cmp(X,Y)	(H5F_addr_eq(X,Y)?0:			      \
				 (H5F_addr_lt(X, Y)?-1:1))
#define H5F_addr_pow2(N)	((haddr_t)1<<(N))
#define H5F_addr_overlap(O1,L1,O2,L2) ((O1<O2 && (O1+L1)>O2) ||               \
                                 (O1>=O2 && O1<(O2+L2)))

/* size of size_t and off_t as they exist on disk */
#define H5F_SIZEOF_ADDR(F)      (H5F_sizeof_addr(F))
#define H5F_SIZEOF_SIZE(F)      (H5F_sizeof_size(F))

__DLL__ size_t H5F_sizeof_addr(const H5F_t *f);
__DLL__ size_t H5F_sizeof_size(const H5F_t *f);

/* Macros to encode/decode offset/length's for storing in the file */
#ifdef NOT_YET
#define H5F_ENCODE_OFFSET(f,p,o) (H5F_SIZEOF_ADDR(f)==4 ? UINT32ENCODE(p,o) \
    : H5F_SIZEOF_ADDR(f)==8 ? UINT64ENCODE(p,o) \
    : H5F_SIZEOF_ADDR(f)==2 ? UINT16ENCODE(p,o) \
    : H5FPencode_unusual_offset(f,&(p),(uint8_t*)&(o)))
#else /* NOT_YET */
#define H5F_ENCODE_OFFSET(f,p,o) switch(H5F_SIZEOF_ADDR(f)) {		      \
    case 4: UINT32ENCODE(p,o); break;					      \
    case 8: UINT64ENCODE(p,o); break;					      \
    case 2: UINT16ENCODE(p,o); break;					      \
}
#endif /* NOT_YET */

#define H5F_DECODE_OFFSET(f,p,o) switch (H5F_SIZEOF_ADDR (f)) {	\
   case 4: UINT32DECODE (p, o);	break;							\
   case 8: UINT64DECODE (p, o);	break;							\
   case 2: UINT16DECODE (p, o);	break;							\
}

#define H5F_ENCODE_LENGTH(f,p,l) switch(H5F_SIZEOF_SIZE(f)) {   \
   case 4: UINT32ENCODE(p,l); break;					      \
   case 8: UINT64ENCODE(p,l); break;					      \
   case 2: UINT16ENCODE(p,l); break;					      \
}

#define H5F_DECODE_LENGTH(f,p,l) switch(H5F_SIZEOF_SIZE(f)) {   \
   case 4: UINT32DECODE(p,l); break;					      \
   case 8: UINT64DECODE(p,l); break;					      \
   case 2: UINT16DECODE(p,l); break;					      \
}

/* ========= File Creation properties ============ */
/* Definitions for the size of the file user block in bytes */
#define H5F_CRT_USER_BLOCK_NAME      "block_size"
#define H5F_CRT_USER_BLOCK_SIZE      sizeof(hsize_t)
#define H5F_CRT_USER_BLOCK_DEF       0
/* Definitions for the 1/2 rank for symbol table leaf nodes */
#define H5F_CRT_SYM_LEAF_NAME        "symbol_leaf"
#define H5F_CRT_SYM_LEAF_SIZE        sizeof(unsigned)
#define H5F_CRT_SYM_LEAF_DEF         4
/* Definitions for the 1/2 rank for btree internal nodes    */
#define H5F_CRT_BTREE_RANK_NAME      "btree_rank"
#define H5F_CRT_BTREE_RANK_SIZE      sizeof(int[H5B_NUM_BTREE_ID])
#define H5F_CRT_BTREE_RANK_DEF       {16,32}
/* Definitions for byte number in an address                */
#define H5F_CRT_ADDR_BYTE_NUM_NAME   "addr_byte_num"
#define H5F_CRT_ADDR_BYTE_NUM_SIZE   sizeof(size_t)
#define H5F_CRT_ADDR_BYTE_NUM_DEF    sizeof(haddr_t)
/* Definitions for byte number for object size              */
#define H5F_CRT_OBJ_BYTE_NUM_NAME     "obj_byte_num"
#define H5F_CRT_OBJ_BYTE_NUM_SIZE     sizeof(size_t)
#define H5F_CRT_OBJ_BYTE_NUM_DEF      sizeof(hsize_t)
/* Definitions for version number of the bootblock          */
#define H5F_CRT_BOOT_VERS_NAME        "boot_version"
#define H5F_CRT_BOOT_VERS_SIZE        sizeof(int)
#define H5F_CRT_BOOT_VERS_DEF         HDF5_BOOTBLOCK_VERSION
/* Definitions for free-space version number                */
#define H5F_CRT_FREESPACE_VERS_NAME   "free_space_version"
#define H5F_CRT_FREESPACE_VERS_SIZE   sizeof(int)
#define H5F_CRT_FREESPACE_VERS_DEF    HDF5_FREESPACE_VERSION
/* Definitions for object directory version number          */
#define H5F_CRT_OBJ_DIR_VERS_NAME     "obj_dir_version"
#define H5F_CRT_OBJ_DIR_VERS_SIZE     sizeof(int)
#define H5F_CRT_OBJ_DIR_VERS_DEF      HDF5_OBJECTDIR_VERSION
/* Definitions for shared-header format version             */
#define H5F_CRT_SHARE_HEAD_VERS_NAME  "share_head_version"  
#define H5F_CRT_SHARE_HEAD_VERS_SIZE  sizeof(int)
#define H5F_CRT_SHARE_HEAD_VERS_DEF   HDF5_SHAREDHEADER_VERSION

/* ========= File Access properties ============ */
/* Definitions for size of meta data cache(elements) */
#define H5F_ACS_META_CACHE_SIZE_NAME           "mdc_nelmts"
#define H5F_ACS_META_CACHE_SIZE_SIZE           sizeof(int)
#define H5F_ACS_META_CACHE_SIZE_DEF            H5AC_NSLOTS

/* Definitions for size of raw data chunk cache(elements) */
#define H5F_ACS_DATA_CACHE_ELMT_SIZE_NAME      "rdcc_nelmts"
#define H5F_ACS_DATA_CACHE_ELMT_SIZE_SIZE      sizeof(size_t)
#define H5F_ACS_DATA_CACHE_ELMT_SIZE_DEF       521

/* Definition for size of raw data chunk cache(bytes) */
#define H5F_ACS_DATA_CACHE_BYTE_SIZE_NAME      "rdcc_nbytes"
#define H5F_ACS_DATA_CACHE_BYTE_SIZE_SIZE      sizeof(size_t)
#define H5F_ACS_DATA_CACHE_BYTE_SIZE_DEF       1024*1024

/* Definition for preemption read chunks first */
#define H5F_ACS_PREEMPT_READ_CHUNKS_NAME       "rdcc_w0"
#define H5F_ACS_PREEMPT_READ_CHUNKS_SIZE       sizeof(double)
#define H5F_ACS_PREEMPT_READ_CHUNKS_DEF        0.75  

/* Definition for threshold for alignment */
#define H5F_ACS_ALIGN_THRHD_NAME               "threshold"
#define H5F_ACS_ALIGN_THRHD_SIZE               sizeof(hsize_t)
#define H5F_ACS_ALIGN_THRHD_DEF                1

/* Definition for alignment */
#define H5F_ACS_ALIGN_NAME                     "align"
#define H5F_ACS_ALIGN_SIZE                     sizeof(hsize_t)
#define H5F_ACS_ALIGN_DEF                      1

/* Definition for minimum metadata allocation block size(when
   aggregating metadata allocations. */
#define H5F_ACS_META_BLOCK_SIZE_NAME           "meta_block_size"
#define H5F_ACS_META_BLOCK_SIZE_SIZE           sizeof(size_t)
#define H5F_ACS_META_BLOCK_SIZE_DEF            2048

/* Definition for maximum sieve buffer size (when data sieving 
   is allowed by file driver */
#define H5F_ACS_SIEVE_BUF_SIZE_NAME         "sieve_buf_size"
#define H5F_ACS_SIEVE_BUF_SIZE_SIZE         sizeof(size_t)
#define H5F_ACS_SIEVE_BUF_SIZE_DEF          64*1024

/* Definition for garbage-collect references */
#define H5F_ACS_GARBG_COLCT_REF_NAME           "gc_ref"
#define H5F_ACS_GARBG_COLCT_REF_SIZE           sizeof(unsigned)
#define H5F_ACS_GARBG_COLCT_REF_DEF            0

/* Definition for file driver ID */
#define H5F_ACS_FILE_DRV_ID_NAME               "driver_id"
#define H5F_ACS_FILE_DRV_ID_SIZE               sizeof(hid_t)
#define H5F_ACS_FILE_DRV_ID_DEF                H5FD_SEC2

/* Definition for file driver info */
#define H5F_ACS_FILE_DRV_INFO_NAME             "driver_info"
#define H5F_ACS_FILE_DRV_INFO_SIZE             sizeof(void*)
#define H5F_ACS_FILE_DRV_INFO_DEF              NULL

/* Definition for file close degree */
#define H5F_CLOSE_DEGREE_NAME		       "close_degree"
#define H5F_CLOSE_DEGREE_SIZE		       sizeof(H5F_close_degree_t)
#define H5F_CLOSE_DEGREE_DEF		       H5F_CLOSE_DEFAULT

/* ======================== File Mount properties ====================*/
/* Definition for whether absolute symlinks local to file. */
#define H5F_MNT_SYM_LOCAL_NAME 		"local"
#define H5F_MNT_SYM_LOCAL_SIZE		sizeof(hbool_t) 	
#define H5F_MNT_SYM_LOCAL_DEF	 	FALSE	

/* Forward declarations for prototypes arguments */
struct H5O_layout_t;
struct H5O_efl_t;
struct H5O_pline_t;
struct H5O_fill_t;
struct H5G_entry_t;
struct H5S_t;

/* Private functions, not part of the publicly documented API */
__DLL__ herr_t H5F_init(void);
__DLL__ unsigned H5F_get_intent(const H5F_t *f);
__DLL__ hid_t H5F_get_driver_id(const H5F_t *f);
__DLL__ herr_t H5F_get_fileno(const H5F_t *f, unsigned long *filenum);

/* Functions that operate on array storage */
__DLL__ herr_t H5F_arr_create(H5F_t *f,
			      struct H5O_layout_t *layout /*in,out*/);
__DLL__ herr_t H5F_arr_read (H5F_t *f, hid_t dxpl_id,
			     const struct H5O_layout_t *layout,
			     const struct H5O_pline_t *pline,
			     const struct H5O_fill_t *fill,
			     const struct H5O_efl_t *efl,
			     const hsize_t _hslab_size[],
			     const hsize_t mem_size[],
			     const hssize_t mem_offset[],
			     const hssize_t file_offset[], void *_buf/*out*/);
__DLL__ herr_t H5F_arr_write (H5F_t *f, hid_t dxpl_id,
			      const struct H5O_layout_t *layout,
			      const struct H5O_pline_t *pline,
			      const struct H5O_fill_t *fill,
			      const struct H5O_efl_t *efl,
			      const hsize_t _hslab_size[],
			      const hsize_t mem_size[],
			      const hssize_t mem_offset[],
			      const hssize_t file_offset[], const void *_buf);

/* Functions that operate on blocks of bytes wrt boot block */
__DLL__ herr_t H5F_block_read(H5F_t *f, H5FD_mem_t type, haddr_t addr,
                size_t size, hid_t dxpl_id, void *buf/*out*/);
__DLL__ herr_t H5F_block_write(H5F_t *f, H5FD_mem_t type, haddr_t addr,
                size_t size, hid_t dxpl_id, const void *buf);

/* Functions that operate on byte sequences */
__DLL__ herr_t H5F_seq_read(H5F_t *f, hid_t dxpl_id,
        const struct H5O_layout_t *layout, const struct H5O_pline_t *pline,
        const struct H5O_fill_t *fill, const struct H5O_efl_t *efl,
        const struct H5S_t *file_space, size_t elmt_size, size_t seq_len,
        hsize_t file_offset, void *_buf/*out*/);
__DLL__ herr_t H5F_seq_write (H5F_t *f, hid_t dxpl_id,
        const struct H5O_layout_t *layout, const struct H5O_pline_t *pline,
        const struct H5O_fill_t *fill, const struct H5O_efl_t *efl,
        const struct H5S_t *file_space, size_t elmt_size, size_t seq_len,
        hsize_t file_offset, const void *_buf);

/* Functions that operate on vectors of byte sequences */
__DLL__ herr_t H5F_seq_readv(H5F_t *f, hid_t dxpl_id,
        const struct H5O_layout_t *layout, const struct H5O_pline_t *pline,
        const struct H5O_fill_t *fill, const struct H5O_efl_t *efl,
        const struct H5S_t *file_space, size_t elmt_size, size_t nseq,
        size_t seq_len[], hsize_t file_offset[], void *_buf/*out*/);
__DLL__ herr_t H5F_seq_writev(H5F_t *f, hid_t dxpl_id,
        const struct H5O_layout_t *layout, const struct H5O_pline_t *pline,
        const struct H5O_fill_t *fill, const struct H5O_efl_t *efl,
        const struct H5S_t *file_space, size_t elmt_size, size_t nseq,
        size_t seq_len[], hsize_t file_offset[], const void *_buf);


/* Functions that operate on indexed storage */
__DLL__ hsize_t H5F_istore_allocated(H5F_t *f, unsigned ndims, haddr_t addr);
__DLL__ herr_t H5F_istore_dump_btree(H5F_t *f, FILE *stream, unsigned ndims,
				     haddr_t addr);

/* Functions for allocation/releasing chunks */
__DLL__ void * H5F_istore_chunk_alloc(size_t chunk_size);
__DLL__ void * H5F_istore_chunk_realloc(void *chunk, size_t new_size);
__DLL__ void * H5F_istore_chunk_free(void *chunk);

/* Address-related functions */
__DLL__ void H5F_addr_encode(H5F_t *, uint8_t** /*in,out*/, haddr_t);
__DLL__ void H5F_addr_decode(H5F_t *, const uint8_t** /*in,out*/,
			     haddr_t* /*out*/);
__DLL__ herr_t H5F_addr_pack(H5F_t *f, haddr_t *addr_p /*out*/,
			     const unsigned long objno[2]);

/* callback Functions for file access class */
__DLL__ herr_t H5F_acs_create(hid_t fapl_id, void *close_data);
__DLL__ herr_t H5F_acs_close(hid_t fapl_id, void *close_data);
__DLL__ herr_t H5F_acs_copy(hid_t new_fapl_id, hid_t old_fapl_id, 
                            void *close_data);

#endif
