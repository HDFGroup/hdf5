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
#include <H5Fpublic.h>

/* Private headers needed by this file */
#include <H5private.h>

/* Maximum size of boot-block buffer */
#define H5F_BOOTBLOCK_SIZE  1024

/* Define the HDF5 file signature */
#define H5F_SIGNATURE     "\211HDF\r\n\032\n"
#define H5F_SIGNATURE_LEN 8

/* size of size_t and off_t as they exist on disk */
#define H5F_SIZEOF_OFFSET(F) ((F)->shared->file_create_parms.offset_size)
#define H5F_SIZEOF_SIZE(F)	((F)->shared->file_create_parms.length_size)

/*
 * File open flags.
 */
#define H5F_ACC_WRITE	0x0001	/* Open file for read/write access	*/
#define H5F_ACC_CREAT	0x0002	/* Create non-existing files		*/
#define H5F_ACC_EXCL	0x0004	/* Fail if file exists			*/
#define H5F_ACC_TRUNC	0x0008	/* Truncate existing file		*/

/*
 * Define the low-level file interface.
 */
#if FILELIB == POSIXBUFIO
typedef FILE *hdf_file_t;
#  ifdef VMS
#     define H5F_OPEN(p, a)	(((a) & H5ACC_WRITE) ?			      \
				 fopen ((p), "r+", "mbc=64") :		      \
				 fopen ((p), "r", "mbc=64"))
#     define H5F_CREATE(p)	fopen ((p), "w+", "mbc=64")
#  elif defined SUN && defined (__GNUC__)
#     define H5F_OPEN(p, a)	(((a) & H5ACC_WRITE) ?			      \
				 fopen ((p), "r+") :			      \
				 fopen ((p), "r"))
#     define H5F_CREATE(p)	fopen ((p), "w+")
#  else
#     define H5F_OPEN(p, a)	(((a) & H5ACC_WRITE) ?			      \
				 fopen ((p), "rb+") :			      \
				 fopen((p), "rb"))
#     define H5F_CREATE(p)	fopen((p), "wb+")
#  endif

#  define H5F_READ(f, b, n)	(((size_t)(n) == (size_t)fread ((b), 1,	      \
								(size_t)(n),  \
								(f))) ?	      \
				 SUCCEED : FAIL)
#  define H5F_WRITE(f, b, n)	(((size_t)(n) == (size_t)fwrite ((b), 1,      \
								 (size_t)(n), \
								 (f))) ?      \
				 SUCCEED : FAIL)
#  define H5F_CLOSE(f) 	       	fclose (f)
#  define H5F_FLUSH(f)  	(0==fflush (f) ? SUCCEED : FAIL)
#  define H5F_SEEK(f,o)      	(0==fseek ((f), (long)(o), SEEK_SET) ?	      \
				 SUCCEED : FAIL)
#  define H5F_SEEK_CUR(f,o) 	(0==fseek ((f), (long)(o), SEEK_CUR) ?	      \
				 SUCCEED : FAIL)
#  define H5F_SEEKEND(f)	(0==fseek ((f), (long)0, SEEK_END) ?	      \
				 SUCCEED : FAIL)
#  define H5F_TELL(f)      	ftell (f)
#  define H5F_OPENERR(f)	(!f)
#  define H5F_INVALID_FILE    	((FILE *)NULL)

   
#elif FILELIB == POSIXUNBUFIO
typedef int hdf_file_t;
#  define H5F_OPEN(p, a)	(((a) & H5ACC_WRITE) ?			      \
				 open ((p), O_RDWR) :			      \
				 open ((p), O_RDONLY))
#  define H5F_CREATE(p)       	open ((p), O_RDWR | O_CREAT | O_TRUNC, 0666)
#  define H5F_CLOSE(f)        	close(f)
#  define H5F_FLUSH(f)        	SUCCEED
#  define H5F_READ(f, b, n)  	 (((n)==read ((f), (char*)(b), (n))) ?	      \
				  SUCCEED : FAIL)
#  define H5F_WRITE(f, b, n)  	(((n)==write ((f), (char*)(b), (n))) ?	      \
				 SUCCEED : FAIL)
#  define H5F_SEEK(f, o)      	(lseek ((f), (off_t)(o), SEEK_SET)<0 ?	      \
				 FAIL : SUCCEED)
#  define H5F_SEEKEND(f)      	(lseek ((f), (off_t)0, SEEK_END)<0 ?	      \
				 FAIL : SUCCEED)
#  define H5F_TELL(f)         	lseek ((f), (off_t)0, SEEK_CUR)
#  define H5F_OPENERR(f)      	((f) < 0)
#  define H5F_INVALID_FILE    	(-1)


#elif FILELIB == MACIO
typedef short hdf_file_t;
#  define H5F_OPEN(x,y)       	mopen (x, y)
#  define H5F_CREATE(name)    	mopen (name, H5ACC_CREATE)
#  define H5F_CLOSE(x)        	mclose (x)
#  define H5F_FLUSH(a)        	SUCCEED
#  define H5F_READ(a,b,c)     	mread (a, (char*)b, (int32)c)
#  define H5F_WRITE(a,b,c)    	mwrite (a, (char*)b, (int32)c)
#  define H5F_SEEK(x,y)       	mlseek (x, (int32)y, 0)
#  define H5F_SEEKEND(x)      	mlseek (x, 0L, 2)
#  define H5F_TELL(x)         	mlseek (x, 0L, 1)
#  define H5F_OPENERR(f)      	(f < 0)
#  define H5F_INVALID_FILE    	(-1)


#elif FILELIB == WINNTIO
typedef HFILE hdf_file_t;
#  define H5F_OPEN(p, a)	(((a) & H5ACC_WRITE) ?			      \
				 _lopen ((p), OF_READWRITE) :		      \
				 _lopen ((p), OF_READ))
#  define H5F_CREATE(p)       	_lcreat ((p), 0)
#  define H5F_READ(f, b, n)   	(((int32)(n) == _hread ((f), (b), (n))) ?     \
				 SUCCEED : FAIL)
#  define H5F_WRITE(f, b, n)  	(((int32)(n) == _hwrite ((f), (b), (n))) ?    \
				 SUCCEED : FAIL)
#  define H5F_CLOSE(f)        	(_lclose(f)==0 ? SUCCEED : FAIL)
#  define H5F_FLUSH(f)        	0
#  define H5F_SEEK(f, o)      	_llseek ((f), (long)(o), 0)
#  define H5F_SEEKEND(f)      	_llseek ((f), (long)0, 2)
#  define H5F_TELL(f)         	_llseek ((f), 0l, 1)
#  define H5F_OPENERR(f)      	((f) == (HFILE)HFILE_ERROR)
#  define H5F_INVALID_FILE    	((HFILE)HFILE_ERROR)


#elif FILELIB == PAGEBUFIO
#  include "fmpio.h"
typedef MPFILE *hdf_file_t;
#  define H5F_OPEN(p, a)      	MPopen ((p), (a))
#  define H5F_CREATE(p)         MPopen ((p), H5ACC_CREATE)
#  define H5F_CLOSE(f)          MPclose (f)
#  define H5F_FLUSH(f)          MPflush (f)
#  define H5F_READ(f, b, n)     MPread ((f), (char *)(b), (n))
#  define H5F_WRITE(f, b, n)    MPwrite ((f), (char *)(b), (n))
#  define H5F_SEEK(f, o)        MPseek ((f), (off_t)(o), SEEK_SET)
#  define H5F_SEEKEND(f)        MPseek ((f), (off_t)0, SEEK_END)
#  define H5F_TELL(f)           MPseek ((f), (off_t)0, SEEK_CUR)
#  define H5F_OPENERR(f)        ((f) == (MPFILE *)NULL)
#  define H5F_INVALID_FILE      ((MPFILE *)NULL)

#endif

/*
 * Encode and decode macros for file meta-data.
 * Currently, all file meta-data is little-endian.
 */

/* For non-little-endian platforms, encode each byte by itself */
#ifdef WORDS_BIGENDIAN
#  define INT16ENCODE(p, i) {						      \
   *(p) = (uint8)( (uintn)(i)       & 0xff); (p)++;			      \
   *(p) = (uint8)(((uintn)(i) >> 8) & 0xff); (p)++;			      \
}

#  define UINT16ENCODE(p, i) {						      \
   *(p) = (uint8)(        (i)       & 0xff); (p)++;			      \
   *(p) = (uint8)(((uintn)(i) >> 8) & 0xff); (p)++;			      \
}

#  define INT32ENCODE(p, i) {						      \
   *(p) = (uint8)( (uint32)(i)        & 0xff); (p)++;			      \
   *(p) = (uint8)(((uint32)(i) >>  8) & 0xff); (p)++;			      \
   *(p) = (uint8)(((uint32)(i) >> 16) & 0xff); (p)++;			      \
   *(p) = (uint8)(((uint32)(i) >> 24) & 0xff); (p)++;			      \
}

#  define UINT32ENCODE(p, i) {						      \
   *(p) = (uint8)( (i)        & 0xff); (p)++;				      \
   *(p) = (uint8)(((i) >>  8) & 0xff); (p)++;				      \
   *(p) = (uint8)(((i) >> 16) & 0xff); (p)++;				      \
   *(p) = (uint8)(((i) >> 24) & 0xff); (p)++;				      \
}

#  define INT64ENCODE(p, i) {						      \
   *(p) = (uint8)( (uint64)(i)        & 0xff); (p)++;			      \
   *(p) = (uint8)(((uint64)(i) >>  8) & 0xff); (p)++;			      \
   *(p) = (uint8)(((uint64)(i) >> 16) & 0xff); (p)++;			      \
   *(p) = (uint8)(((uint64)(i) >> 24) & 0xff); (p)++;			      \
   if (sizeof(int64)>4) {						      \
      *(p) = (uint8)(((uint64)(i) >> 32) & 0xff); (p)++;		      \
      *(p) = (uint8)(((uint64)(i) >> 40) & 0xff); (p)++;		      \
      *(p) = (uint8)(((uint64)(i) >> 48) & 0xff); (p)++;		      \
      *(p) = (uint8)(((uint64)(i) >> 56) & 0xff); (p)++;		      \
   } else if ((i)<0) {							      \
      *(p)++ = 0xff;							      \
      *(p)++ = 0xff;							      \
      *(p)++ = 0xff;							      \
      *(p)++ = 0xff;							      \
   } else {								      \
      *(p)++ = 0x00;							      \
      *(p)++ = 0x00;							      \
      *(p)++ = 0x00;							      \
      *(p)++ = 0x00;							      \
   }									      \
}

#  define UINT64ENCODE(p, i) {						      \
   *(p) = (uint8)( (i)        & 0xff); (p)++;				      \
   *(p) = (uint8)(((i) >>  8) & 0xff); (p)++;				      \
   *(p) = (uint8)(((i) >> 16) & 0xff); (p)++;				      \
   *(p) = (uint8)(((i) >> 24) & 0xff); (p)++;				      \
   if (sizeof(uint64)>4) {						      \
      *(p) = (uint8)(((i) >> 32) & 0xff); (p)++;			      \
      *(p) = (uint8)(((i) >> 40) & 0xff); (p)++;			      \
      *(p) = (uint8)(((i) >> 48) & 0xff); (p)++;			      \
      *(p) = (uint8)(((i) >> 56) & 0xff); (p)++;			      \
   } else {								      \
      *(p)++ = 0x00;							      \
      *(p)++ = 0x00;							      \
      *(p)++ = 0x00;							      \
      *(p)++ = 0x00;							      \
   }									      \
}

#  define INT16DECODE(p, i) {						      \
   (i)  = (int16)((*(p) & 0xff));      (p)++;				      \
   (i) |= (int16)((*(p) & 0xff) << 8); (p)++;				      \
}

#  define UINT16DECODE(p, i) {						      \
   (i)  = (uint16) (*(p) & 0xff);       (p)++;				      \
   (i) |= (uint16)((*(p) & 0xff) << 8); (p)++;				      \
}

#  define INT32DECODE(p, i) {						      \
   (i)  = (        *(p) & 0xff);        (p)++;				      \
   (i) |= ((int32)(*(p) & 0xff) <<  8); (p)++;				      \
   (i) |= ((int32)(*(p) & 0xff) << 16); (p)++;				      \
   (i) |= ((int32)(*(p) & 0xff) << 24); (p)++;				      \
}

#  define UINT32DECODE(p, i) {						      \
   (i)  =  (uint32)(*(p) & 0xff);        (p)++;				      \
   (i) |= ((uint32)(*(p) & 0xff) <<  8); (p)++;				      \
   (i) |= ((uint32)(*(p) & 0xff) << 16); (p)++;				      \
   (i) |= ((uint32)(*(p) & 0xff) << 24); (p)++;				      \
}

#  define INT64DECODE(p, i) {						      \
   (i)  = (        *(p) & 0xff);        (p)++;				      \
   (i) |= ((int64)(*(p) & 0xff) <<  8); (p)++;				      \
   (i) |= ((int64)(*(p) & 0xff) << 16); (p)++;				      \
   (i) |= ((int64)(*(p) & 0xff) << 24); (p)++;				      \
   if (sizeof(int64)>4) {						      \
      (i) |= ((int64)(*(p) & 0xff) << 32); (p)++;			      \
      (i) |= ((int64)(*(p) & 0xff) << 40); (p)++;			      \
      (i) |= ((int64)(*(p) & 0xff) << 48); (p)++;			      \
      (i) |= ((int64)(*(p) & 0xff) << 56); (p)++;			      \
   } else {								      \
      (p) += 4;								      \
   }									      \
}

#  define UINT64DECODE(p, i) {						      \
   (i)  =  (uint64)(*(p) & 0xff);        (p)++;				      \
   (i) |= ((uint64)(*(p) & 0xff) <<  8); (p)++;				      \
   (i) |= ((uint64)(*(p) & 0xff) << 16); (p)++;				      \
   (i) |= ((uint64)(*(p) & 0xff) << 24); (p)++;				      \
   if (sizeof(uint64)>4) {						      \
      (i) |= ((uint64)(*(p) & 0xff) << 32); (p)++;			      \
      (i) |= ((uint64)(*(p) & 0xff) << 40); (p)++;			      \
      (i) |= ((uint64)(*(p) & 0xff) << 48); (p)++;			      \
      (i) |= ((uint64)(*(p) & 0xff) << 56); (p)++;			      \
   } else {								      \
      (p) += 4;								      \
   }									      \
}

#else
   /* For little-endian platforms, make the compiler do the work */
#  define INT16ENCODE(p, i)  { *((int16 *)(p)) = (int16)(i); (p)+=2; }
#  define UINT16ENCODE(p, i) { *((uint16 *)(p)) = (uint16)(i); (p)+=2; }
#  define INT32ENCODE(p, i)  { *((int32 *)(p)) = (int32)(i); (p)+=4; }
#  define UINT32ENCODE(p, i) { *((uint32 *)(p)) = (uint32)(i); (p)+=4; }

#  define INT64ENCODE(p, i)  {						      \
   *((int64 *)(p)) = (int64)(i);					      \
   (p) += sizeof(int64);						      \
   if (4==sizeof(int64)) {						      \
      *(p)++ = (i)<0?0xff:0x00;						      \
      *(p)++ = (i)<0?0xff:0x00;						      \
      *(p)++ = (i)<0?0xff:0x00;						      \
      *(p)++ = (i)<0?0xff:0x00;						      \
   }									      \
}

#  define UINT64ENCODE(p, i) {						      \
   *((uint64 *)(p)) = (uint64)(i);					      \
   (p) += sizeof(uint64);						      \
   if (4==sizeof(uint64)) {						      \
      *(p)++ = 0x00;							      \
      *(p)++ = 0x00;							      \
      *(p)++ = 0x00;							      \
      *(p)++ = 0x00;							      \
   }									      \
}

#  define INT16DECODE(p, i)  { (i) = (int16)(*(const int16 *)(p)); (p)+=2; }
#  define UINT16DECODE(p, i) { (i) = (uint16)(*(const uint16 *)(p)); (p)+=2; }
#  define INT32DECODE(p, i)  { (i) = (int32)(*(const int32 *)(p)); (p)+=4; }
#  define UINT32DECODE(p, i) { (i) = (uint32)(*(const uint32 *)(p)); (p)+=4; }
#  define INT64DECODE(p, i)  { (i) = (int64)(*(const int64 *)(p)); (p)+=8; }
#  define UINT64DECODE(p, i) { (i) = (uint64)(*(const uint64 *)(p)); (p)+=8; }

#endif

#define NBYTEENCODE(d, s, n) {   HDmemcpy(d,s,n); p+=n }

/* Note! the NBYTEDECODE macro is backwards from the memcpy() routine, */
/*      in the spirit of the other DECODE macros */
#define NBYTEDECODE(s, d, n) {   HDmemcpy(d,s,n); p+=n }

/*
 * File-creation template information structure
 */
typedef struct {
   uintn userblock_size;	/* Size of the file user block in bytes */
   uintn sym_leaf_k;		/* 1/2 rank for symbol table leaf nodes */
   uintn btree_k[8];		/* 1/2 rank for btree internal nodes 	*/
   uint8 offset_size;          	/* Number of bytes for offsets 		*/
   uint8 length_size;          	/* Number of bytes for lengths 		*/
   uint8 bootblock_ver;        	/* Version # of the bootblock 		*/
   uint8 smallobject_ver;      	/* Version # of the small-object heap 	*/
   uint8 freespace_ver;        	/* Version # of the free-space information */
   uint8 objectdir_ver;        	/* Version # of the object directory format */
   uint8 sharedheader_ver;     	/* Version # of the shared header format */
} file_create_temp_t;

/*
 * These things make a file unique.
 */
typedef struct H5F_search_t {
   dev_t	dev;		/* Device number containing file	*/
   ino_t	ino;		/* Unique file number on device		*/
} H5F_search_t;

/* For determining what the last file operation was */
typedef enum
  {
      OP_UNKNOWN = 0,   /* Don't know what the last operation was (after fopen frex) */
      OP_SEEK,          /* Last operation was a seek */
      OP_WRITE,         /* Last operation was a write */
      OP_READ           /* Last operation was a read */
  }
H5F_fileop_t;

/*
 * Define the structure to store the file information for HDF5 files. One of
 * these structures is allocated per file, not per H5Fopen().
 */
typedef struct H5F_file_t {
   /* Seek caching info */
   haddr_t       f_cur_off; /* Current location in the file */
   H5F_fileop_t  last_op;   /* the last file operation performed */

   H5F_search_t	key;		/* The key for looking up files		*/
   uintn	flags;         	/* Access Permissions for file		*/
   hdf_file_t 	file_handle; 	/* File handle for actual I/O 		*/
   uintn	nrefs;        	/* Ref count for times file is opened 	*/
   uint32 	consist_flags; 	/* File Consistency Flags 		*/
   haddr_t 	smallobj_off;  	/* Offset of small-obj heap within the file */
   haddr_t 	freespace_off; 	/* Offset of free-space info within the file */
   size_t	logical_len;   	/* Logical length of file 		*/
   struct H5AC_t *cache; 	/* The object cache 			*/
   file_create_temp_t file_create_parms; /* File-creation template 	*/
#ifdef LATER
   file_access_temp_t file_access_parms; /* File-access template	*/
#endif
   struct H5G_entry_t *root_sym; /* Root symbol table entry		*/
   uintn nshadows;		/* Size of shadow hash table		*/
   struct H5G_hash_t **shadow;	/* The shadow hash table		*/
} H5F_file_t;

/*
 * This is the top-level file descriptor.  One of these structures is
 * allocated every time H5Fopen() is called although they may contain
 * pointers to shared H5F_file_t structs.
 */
typedef struct H5F_t {
   uintn	intent;		/* The flags passed to H5F_open()	*/
   char		*name;		/* Name used to open file		*/
   H5F_file_t	*shared;	/* The shared file info			*/
} H5F_t;




#ifdef NOT_YET
#define H5F_encode_offset(f,p,o) (H5F_SIZEOF_OFFSET(f)==4 ? UINT32ENCODE(p,o) \
    : H5F_SIZEOF_OFFSET(f)==8 ? UINT64ENCODE(p,o) \
    : H5F_SIZEOF_OFFSET(f)==2 ? UINT16ENCODE(p,o) \
    : H5FPencode_unusual_offset(f,&(p),(uint8 *)&(o)))
#else /* NOT_YET */
#define H5F_encode_offset(f,p,o) switch(H5F_SIZEOF_OFFSET(f)) { case 4: UINT32ENCODE(p,o); break;\
    case 8: UINT64ENCODE(p,o); break;\
    case 2: UINT16ENCODE(p,o); break;}
#endif /* NOT_YET */

#define H5F_decode_offset(f,p,o)					      \
   switch (H5F_SIZEOF_OFFSET (f)) {					      \
   case 4:								      \
      UINT32DECODE (p, o);						      \
      break;								      \
   case 8:								      \
      UINT64DECODE (p, o);						      \
      break;								      \
   case 2:								      \
      UINT16DECODE (p, o);						      \
      break;								      \
   }

#ifdef NOT_YET
#define H5F_encode_length(f,p,l) (H5F_SIZEOF_SIZE(f)==4 ? UINT32ENCODE(p,l) \
    : H5F_SIZEOF_SIZE(f)==8 ? UINT64ENCODE(p,l) \
    : H5F_SIZEOF_SIZE(f)==2 ? UINT16ENCODE(p,l) : H5FPencode_unusual_length(f,&(p),(uint8 *)&(l)))
#else
#define H5F_encode_length(f,p,l)					      \
   switch(H5F_SIZEOF_SIZE(f)) {						      \
   case 4: UINT32ENCODE(p,l); break;					      \
   case 8: UINT64ENCODE(p,l); break;					      \
   case 2: UINT16ENCODE(p,l); break;					      \
}
#endif

#define H5F_decode_length(f,p,l)					      \
   switch(H5F_SIZEOF_SIZE(f)) {						      \
   case 4: UINT32DECODE(p,l); break;					      \
   case 8: UINT64DECODE(p,l); break;					      \
   case 2: UINT16DECODE(p,l); break;					      \
}


/* Private functions, not part of the publicly documented API */
void H5F_encode_length_unusual(const H5F_t *f, uint8 **p, uint8 *l);
void H5F_encode_offset_unusual(const H5F_t *f, uint8 **p, uint8 *o);
herr_t H5F_block_read (H5F_t *f, haddr_t addr, size_t size, void *buf);
herr_t H5F_block_write (H5F_t *f, haddr_t addr, size_t size, void *buf);
herr_t H5F_debug (H5F_t *f, haddr_t addr, FILE *stream, intn indent,
		  intn fwidth);

#endif
