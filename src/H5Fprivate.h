/****************************************************************************
 * NCSA HDF								    *
 * Software Development Group						    *
 * National Center for Supercomputing Applications			    *
 * University of Illinois at Urbana-Champaign				    *
 * 605 E. Springfield, Champaign IL 61820				    *
 *									    *
 * For conditions of distribution and use, see the accompanying		    *
 * hdf/COPYING file.							    *
 *									    *
 ****************************************************************************/

/* $Id$ */

/*
 * This file contains macros & information for file access
 */

#ifndef _H5Fprivate_H
#define _H5Fprivate_H
#include <H5Fpublic.h>

/* This is a near top-level header! Try not to include much! */
#include <H5private.h>

/*
 * Feature: Define this constant to be non-zero if you want to enable code
 *	    that minimizes the number of calls to lseek().  This has a huge
 *	    performance benefit on some systems.  Set this constant to zero
 *	    on the compiler command line to disable that optimization.
 */
#ifndef H5F_OPT_SEEK
#  define H5F_OPT_SEEK 1
#endif

/*
 * Feature: Define this constant on the compiler command-line if you want to
 *	    see some debugging messages on stderr.
 */
#ifdef NDEBUG
#  undef H5F_DEBUG
#endif

/* Maximum size of boot-block buffer */
#define H5F_BOOTBLOCK_SIZE  1024

/* Define the HDF5 file signature */
#define H5F_SIGNATURE	  "\211HDF\r\n\032\n"
#define H5F_SIGNATURE_LEN 8

/* size of size_t and off_t as they exist on disk */
#define H5F_SIZEOF_ADDR(F)	((F)->shared->create_parms.sizeof_addr)
#define H5F_SIZEOF_SIZE(F)	((F)->shared->create_parms.sizeof_size)

/*
 * Private file open flags.
 */
#define H5F_ACC_PUBLIC_FLAGS 	0x00ffu

#define H5F_ACC_CREAT	0x0100u	/* Create non-existing files		  */

/*
 * Encode and decode macros for file meta-data.
 * Currently, all file meta-data is little-endian.
 */

/* For non-little-endian platforms, encode each byte by itself */
#ifdef WORDS_BIGENDIAN
#  define INT16ENCODE(p, i) {						      \
   *(p) = (uint8)( (uintn)(i)	    & 0xff); (p)++;			      \
   *(p) = (uint8)(((uintn)(i) >> 8) & 0xff); (p)++;			      \
}

#  define UINT16ENCODE(p, i) {						      \
   *(p) = (uint8)(	  (i)	    & 0xff); (p)++;			      \
   *(p) = (uint8)(((uintn)(i) >> 8) & 0xff); (p)++;			      \
}

#  define INT32ENCODE(p, i) {						      \
   *(p) = (uint8)( (uint32)(i)	      & 0xff); (p)++;			      \
   *(p) = (uint8)(((uint32)(i) >>  8) & 0xff); (p)++;			      \
   *(p) = (uint8)(((uint32)(i) >> 16) & 0xff); (p)++;			      \
   *(p) = (uint8)(((uint32)(i) >> 24) & 0xff); (p)++;			      \
}

#  define UINT32ENCODE(p, i) {						      \
   *(p) = (uint8)( (i)	      & 0xff); (p)++;				      \
   *(p) = (uint8)(((i) >>  8) & 0xff); (p)++;				      \
   *(p) = (uint8)(((i) >> 16) & 0xff); (p)++;				      \
   *(p) = (uint8)(((i) >> 24) & 0xff); (p)++;				      \
}

#  define INT64ENCODE(p, n) {						      \
   int64 _n = (n);							      \
   size_t _i;								      \
   uint8 *_p = (uint8*)(p);						      \
   for (_i=0; _i<sizeof(int64); _i++, _n>>=8) {				      \
      *_p++ = (uint8)(_n & 0xff);					      \
   }									      \
   for (/*void*/; _i<8; _i++) {						      \
      *_p++ = (n)<0 ? 0xff : 0;						      \
   }									      \
   (p) = (uint8*)(p)+8;							      \
}

#  define UINT64ENCODE(p, n) {						      \
   uint64 _n = (n);							      \
   size_t _i;								      \
   uint8 *_p = (uint8*)(p);						      \
   for (_i=0; _i<sizeof(uint64); _i++, _n>>=8) {			      \
      *_p++ = (uint8)(_n & 0xff);					      \
   }									      \
   for (/*void*/; _i<8; _i++) {						      \
      *_p++ = 0;							      \
   }									      \
   (p) = (uint8*)(p)+8;							      \
}

#  define INT16DECODE(p, i) {						      \
   (i)	= (int16)((*(p) & 0xff));      (p)++;				      \
   (i) |= (int16)((*(p) & 0xff) << 8); (p)++;				      \
}

#  define UINT16DECODE(p, i) {						      \
   (i)	= (uint16) (*(p) & 0xff);	(p)++;				      \
   (i) |= (uint16)((*(p) & 0xff) << 8); (p)++;				      \
}

#  define INT32DECODE(p, i) {						      \
   (i)	= (	   *(p) & 0xff);	(p)++;				      \
   (i) |= ((int32)(*(p) & 0xff) <<  8); (p)++;				      \
   (i) |= ((int32)(*(p) & 0xff) << 16); (p)++;				      \
   (i) |= ((int32)(*(p) & 0xff) << 24); (p)++;				      \
}

#  define UINT32DECODE(p, i) {						      \
   (i)	=  (uint32)(*(p) & 0xff);	 (p)++;				      \
   (i) |= ((uint32)(*(p) & 0xff) <<  8); (p)++;				      \
   (i) |= ((uint32)(*(p) & 0xff) << 16); (p)++;				      \
   (i) |= ((uint32)(*(p) & 0xff) << 24); (p)++;				      \
}

#  define INT64DECODE(p, n) {						      \
   /* WE DON'T CHECK FOR OVERFLOW! */					      \
   size_t _i;								      \
   n = 0;								      \
   (p) += 8;								      \
   for (_i=0; _i<sizeof(int64); _i++) {					      \
      n = (n<<8) | *(--p);						      \
   }									      \
   (p) += 8;								      \
}

#  define UINT64DECODE(p, n) {						      \
   /* WE DON'T CHECK FOR OVERFLOW! */					      \
   size_t _i;								      \
   n = 0;								      \
   (p) += 8;								      \
   for (_i=0; _i<sizeof(uint64); _i++) {				      \
      n = (n<<8) | *(--p);						      \
   }									      \
   (p) += 8;								      \
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

#define NBYTEENCODE(d, s, n) {	 HDmemcpy(d,s,n); p+=n }

/*
 * Note:  the NBYTEDECODE macro is backwards from the memcpy() routine, in
 *	  the spirit of the other DECODE macros.
 */
#define NBYTEDECODE(s, d, n) {	 HDmemcpy(d,s,n); p+=n }

/*
 * Macros that check for overflows.  These are somewhat dangerous to fiddle
 * with.
 */
#if (SIZEOF_SIZE_T >= SIZEOF_OFF_T)
#   define H5F_OVERFLOW_SIZET2OFFT(X)					      \
    ((size_t)(X)>=(size_t)((size_t)1<<(8*sizeof(off_t)-1)))
#else
#   define H5F_OVERFLOW_SIZET2OFFT(X) 0
#endif

/*
 * File-creation property list.
 */
typedef struct H5F_create_t {
    hsize_t	userblock_size;	/* Size of the file user block in bytes */
    intn	sym_leaf_k;	/* 1/2 rank for symbol table leaf nodes */
    intn	btree_k[8];	/* 1/2 rank for btree internal nodes	*/
    size_t	sizeof_addr;	/* Number of bytes in an address	*/
    size_t	sizeof_size;	/* Number of bytes for obj sizes	*/
    intn	bootblock_ver;	/* Version # of the bootblock		*/
    intn	freespace_ver;	/* Version # of the free-space information*/
    intn	objectdir_ver;	/* Version # of the object directory format*/
    intn	sharedheader_ver;/* Version # of the shared header format */
} H5F_create_t;

/*
 * File-access property list.
 */
typedef struct H5F_access_t {
    H5F_driver_t driver;	/* Low level file driver		*/
    union {

	/* Properties for in-core files */
	struct {
	    size_t increment;		/*amount by which to increment size*/
	} core;

	/* Properties for file families */
	struct {
	    struct H5F_access_t *memb_access; /*plist for the members	*/
	    size_t	offset_bits;	/*number of bits in offset	*/
	} fam;

	/* Properties for the split driver */
	struct {
	    char	*meta_ext;	/*name extension for meta file	*/
	    char	*raw_ext;	/*name extension for raw file	*/
	    struct H5F_access_t *meta_access; /*plist for meta file	*/
	    struct H5F_access_t *raw_access;  /*plist for raw data file	*/
	} split;
	
#ifdef HAVE_PARALLEL
	/* Properties for parallel I/O */
	struct {
	    uintn access_mode;	/* independent or collective variety?   */
	    MPI_Comm    comm;   /* communicator for file access         */
	    MPI_Info    info;   /* optional info for MPI-IO             */
	} mpio;
#endif
	
    } u;
} H5F_access_t;

/*
 * These things make a file unique.
 */
typedef struct H5F_search_t {
    dev_t	dev;		/* Device number containing file	*/
    ino_t	ino;		/* Unique file number on device		*/
} H5F_search_t;

/* For determining what the last file operation was */
typedef enum {
    H5F_OP_UNKNOWN,		/* Don't know what the last operation was*/
    H5F_OP_SEEK,		/* Last operation was a seek		*/
    H5F_OP_WRITE,		/* Last operation was a write		*/
    H5F_OP_READ			/* Last operation was a read		*/
} H5F_fileop_t;

/*
 * Define the low-level file interface.
 */
typedef struct H5F_low_class_t {
    hbool_t	(*access)(const char *name, const H5F_access_t *access_parms,
			  int mode, H5F_search_t *key/*out*/);
    struct H5F_low_t *(*open)(const char *name,
			      const H5F_access_t *access_parms, uintn flags,
			      H5F_search_t *key/*out*/);
    herr_t	(*close)(struct H5F_low_t *lf,
			 const H5F_access_t *access_parms);
    herr_t	(*read)(struct H5F_low_t *lf, const H5F_access_t *access_parms,
			const haddr_t *addr, size_t size, uint8 *buf);
    herr_t	(*write)(struct H5F_low_t *lf,
			 const H5F_access_t *access_parms,
			 const haddr_t *addr, size_t size, const uint8 *buf);
    herr_t	(*flush)(struct H5F_low_t *lf,
			 const H5F_access_t *access_parms);
    herr_t	(*extend)(struct H5F_low_t *lf,
			  const H5F_access_t *access_parms,
			  intn op, hsize_t size, haddr_t *addr);
} H5F_low_class_t;

typedef struct H5F_low_t {
    const H5F_low_class_t *type;/* What type of file is this?		*/
    haddr_t		eof;	/* Address of logical end-of-file	*/
    union {

	/* File families */
	struct {
	    char	*name;	/* Family name				*/
	    uintn	flags;	/* Flags for opening member files	*/
	    intn	nmemb;	/* Number of family members		*/
	    intn	nalloc;	/* Size of member table in elements	*/
	    struct H5F_low_t **memb; /* An array of family members	*/
	    size_t	offset_bits; /* Number of bits in a member offset*/
	} fam;

	/* Split meta/raw data */
	struct {
	    char	*name;	/* Base name w/o extension		*/
	    uint64	mask;	/* Bit that determines which file to use*/
	    struct H5F_low_t *meta; /* Meta data file			*/
	    struct H5F_low_t *raw; /* Raw data file			*/
	} split;

	/* Posix section 2 I/O */
	struct {
	    int		fd;	/* The unix file descriptor		*/
	    H5F_fileop_t op;	/* Previous file operation		*/
#ifdef HAVE_LSEEK64
	    off64_t	cur;	/* Current file position		*/
#else
	    off_t	cur;	/* Current file position		*/
#endif
	} sec2;

	/* Posix stdio */
	struct {
	    FILE	*f;	/* Posix stdio file			*/
	    H5F_fileop_t op;	/* Previous file operation		*/
#ifdef HAVE_FSEEK64
	    int64	cur;	/* Current file position		*/
#else
	    long	cur;	/* Current file position		*/
#endif
	} stdio;

	/* In-core temp file */
	struct {
	    uint8	*mem;	/* Mem image of the file		*/
	    size_t	size;	/* Current file size			*/
	    size_t	alloc;	/* Current size of MEM buffer		*/
	} core;

#ifdef HAVE_PARALLEL
	/* MPI-IO */
	struct {
	    MPI_File	f;	/* MPI-IO file handle			*/
	} mpio;
#endif

    } u;
} H5F_low_t;

/* What types of low-level files are there? */
#ifndef H5F_LOW_DFLT
#  define H5F_LOW_DFLT	H5F_LOW_STDIO	/* The default type	  */
#endif
extern const H5F_low_class_t H5F_LOW_SEC2_g[];	/* Posix section 2	*/
extern const H5F_low_class_t H5F_LOW_STDIO_g[];	/* Posix stdio		*/
extern const H5F_low_class_t H5F_LOW_CORE_g[];	/* In-core temp file	*/
extern const H5F_low_class_t H5F_LOW_FAMILY_g[];/* File family		*/
extern const H5F_low_class_t H5F_LOW_SPLIT_g[];	/* Split meta/raw data	*/
#ifdef HAVE_PARALLEL
extern const H5F_low_class_t H5F_LOW_MPIO_g[];	/* MPI-IO		*/
#endif

/*
 * Define the structure to store the file information for HDF5 files. One of
 * these structures is allocated per file, not per H5Fopen().
 */
typedef struct H5F_file_t {
    H5F_search_t key;		/* The key for looking up files		*/
    uintn	flags;		/* Access Permissions for file		*/
    H5F_low_t	*lf; 		/* Lower level file handle for I/O	*/
    uintn	nrefs;		/* Ref count for times file is opened	*/
    uint32	consist_flags;	/* File Consistency Flags		*/
    haddr_t	boot_addr;	/* Absolute address of boot block	*/
    haddr_t	base_addr;	/* Absolute base address for rel.addrs. */
    haddr_t	freespace_addr;	/* Relative address of free-space info	*/
    haddr_t	hdf5_eof;	/* Relative addr of end of all hdf5 data*/
    struct H5AC_t *cache;	/* The object cache			*/
    H5F_create_t create_parms;	/* File-creation property list		*/
    H5F_access_t access_parms;  /* File-access property list		*/
    struct H5G_t *root_grp;	/* Open root group			*/
    intn	ncwfs;		/* Num entries on cwfs list		*/
    struct H5HG_heap_t **cwfs;	/* Global heap cache			*/
} H5F_file_t;

/*
 * This is the top-level file descriptor.  One of these structures is
 * allocated every time H5Fopen() is called although they may contain
 * pointers to shared H5F_file_t structs.
 */
typedef struct H5F_t {
    uintn		intent;		/* The flags passed to H5F_open()*/
    char		*name;		/* Name used to open file	*/
    H5F_file_t		*shared;	/* The shared file info		*/
    struct H5G_cwgstk_t	*cwg_stack;	/* CWG stack for push/pop functions*/
    uintn		nopen;		/* Number of open object headers*/
    hbool_t		close_pending;	/* File close is pending	*/
} H5F_t;

#ifdef NOT_YET
#define H5F_ENCODE_OFFSET(f,p,o) (H5F_SIZEOF_ADDR(f)==4 ? UINT32ENCODE(p,o) \
    : H5F_SIZEOF_ADDR(f)==8 ? UINT64ENCODE(p,o) \
    : H5F_SIZEOF_ADDR(f)==2 ? UINT16ENCODE(p,o) \
    : H5FPencode_unusual_offset(f,&(p),(uint8 *)&(o)))
#else /* NOT_YET */
#define H5F_ENCODE_OFFSET(f,p,o) switch(H5F_SIZEOF_ADDR(f)) { case 4: UINT32ENCODE(p,o); break;\
    case 8: UINT64ENCODE(p,o); break;\
    case 2: UINT16ENCODE(p,o); break;}
#endif /* NOT_YET */

#define H5F_DECODE_OFFSET(f,p,o)					      \
   switch (H5F_SIZEOF_ADDR (f)) {					      \
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

#define H5F_encode_length(f,p,l)					      \
   switch(H5F_SIZEOF_SIZE(f)) {						      \
   case 4: UINT32ENCODE(p,l); break;					      \
   case 8: UINT64ENCODE(p,l); break;					      \
   case 2: UINT16ENCODE(p,l); break;					      \
}

#define H5F_decode_length(f,p,l)					      \
   switch(H5F_SIZEOF_SIZE(f)) {						      \
   case 4: UINT32DECODE(p,l); break;					      \
   case 8: UINT64DECODE(p,l); break;					      \
   case 2: UINT16DECODE(p,l); break;					      \
}

struct H5O_layout_t;		/*forward decl for prototype arguments */
struct H5O_efl_t;		/*forward decl for prototype arguments */
struct H5O_compress_t;		/*forward decl for prototype arguments */

/* library variables */
extern const H5F_create_t H5F_create_dflt;
extern H5F_access_t H5F_access_dflt;

/* Private functions, not part of the publicly documented API */
herr_t H5F_init_interface(void);
void H5F_encode_length_unusual(const H5F_t *f, uint8 **p, uint8 *l);
H5F_t *H5F_open(const char *name, uintn flags,
		const H5F_create_t *create_parms,
		const H5F_access_t *access_parms);
herr_t H5F_close(H5F_t *f);
herr_t H5F_debug(H5F_t *f, const haddr_t *addr, FILE * stream, intn indent,
		 intn fwidth);
herr_t H5F_istore_debug(H5F_t *f, const haddr_t *addr, FILE * stream,
			intn indent, intn fwidth, int ndims);

/* Functions that operate on array storage */
herr_t H5F_arr_create(H5F_t *f, struct H5O_layout_t *layout /*in,out*/);
herr_t H5F_arr_read (H5F_t *f, const struct H5O_layout_t *layout,
		     const struct H5O_compress_t *comp,
		     const struct H5O_efl_t *efl, const hsize_t _hslab_size[],
		     const hsize_t mem_size[], const hssize_t mem_offset[],
		     const hssize_t file_offset[], void *_buf/*out*/);
herr_t H5F_arr_write (H5F_t *f, const struct H5O_layout_t *layout,
		      const struct H5O_compress_t *comp,
		      const struct H5O_efl_t *efl, const hsize_t _hslab_size[],
		      const hsize_t mem_size[], const hssize_t mem_offset[],
		      const hssize_t file_offset[], const void *_buf);

/* Functions that operate on indexed storage */
herr_t H5F_istore_create(H5F_t *f, struct H5O_layout_t *layout /*in,out*/);
herr_t H5F_istore_read(H5F_t *f, const struct H5O_layout_t *layout,
		       const struct H5O_compress_t *comp,
		       const hssize_t offset[], const hsize_t size[],
		       void *buf /*out */ );
herr_t H5F_istore_write(H5F_t *f, const struct H5O_layout_t *layout,
			const struct H5O_compress_t *comp,
			const hssize_t offset[], const hsize_t size[],
			const void *buf);

/* Functions that operate on contiguous storage wrt boot block */
herr_t H5F_block_read(H5F_t *f, const haddr_t *addr, hsize_t size, void *buf);
herr_t H5F_block_write(H5F_t *f, const haddr_t *addr, hsize_t size,
		       const void *buf);

/* Functions that operate directly on low-level files */
const H5F_low_class_t *H5F_low_class (H5F_driver_t driver);
herr_t H5F_low_extend(H5F_low_t *lf, const H5F_access_t *access_parms,
		      intn op, hsize_t size, haddr_t *addr);
herr_t H5F_low_seteof(H5F_low_t *lf, const haddr_t *addr);
hbool_t H5F_low_access(const H5F_low_class_t *type, const char *name,
		       const H5F_access_t *access_parms, int mode,
		       H5F_search_t *key);
H5F_low_t *H5F_low_open(const H5F_low_class_t *type, const char *name,
			const H5F_access_t *access_parms, uintn flags,
			H5F_search_t *key);
H5F_low_t *H5F_low_close(H5F_low_t *lf, const H5F_access_t *access_parms);
hsize_t H5F_low_size(H5F_low_t *lf, haddr_t *addr);
herr_t H5F_low_read(H5F_low_t *lf, const H5F_access_t *access_parms,
		    const haddr_t *addr, size_t size, uint8 *buf);
herr_t H5F_low_write(H5F_low_t *lf, const H5F_access_t *access_parms,
		     const haddr_t *addr, size_t size, const uint8 *buf);
herr_t H5F_low_flush(H5F_low_t *lf, const H5F_access_t *access_parms);

/* Functions that operate on addresses */
#define H5F_addr_eq(A1,A2) (H5F_addr_cmp(A1,A2)==0)
#define H5F_addr_ne(A1,A2) (H5F_addr_cmp(A1,A2)!=0)
#define H5F_addr_lt(A1,A2) (H5F_addr_cmp(A1,A2)<0)
#define H5F_addr_le(A1,A2) (H5F_addr_cmp(A1,A2)<=0)
#define H5F_addr_gt(A1,A2) (H5F_addr_cmp(A1,A2)>0)
#define H5F_addr_ge(A1,A2) (H5F_addr_cmp(A1,A2)>=0)

intn H5F_addr_cmp(const haddr_t *, const haddr_t *);
hbool_t H5F_addr_defined(const haddr_t *);
void H5F_addr_undef(haddr_t *);
void H5F_addr_reset(haddr_t *);
hbool_t H5F_addr_zerop(const haddr_t *);
void H5F_addr_encode(H5F_t *, uint8 **, const haddr_t *);
void H5F_addr_decode(H5F_t *, const uint8 **, haddr_t *);
void H5F_addr_print(FILE *, const haddr_t *);
void H5F_addr_pow2(uintn, haddr_t *);
void H5F_addr_inc(haddr_t *addr/*in,out*/, hsize_t inc);
void H5F_addr_adj(haddr_t *addr/*in,out*/, hssize_t adj);
void H5F_addr_add(haddr_t *, const haddr_t *);
uintn H5F_addr_hash(const haddr_t *, uintn mod);

#endif
