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

/* This is a near top-level header! Try not to include much! */
#include <H5private.h>
#include <H5Dpublic.h>		/*for the H5D_transfer_t type  		     */

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
 *	    see some debugging messages on the debug stream.
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
#define H5F_SIZEOF_ADDR(F)	((F)->shared->create_parms->sizeof_addr)
#define H5F_SIZEOF_SIZE(F)	((F)->shared->create_parms->sizeof_size)

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
   *(p) = (uint8_t)( (uintn)(i)	    & 0xff); (p)++;			      \
   *(p) = (uint8_t)(((uintn)(i) >> 8) & 0xff); (p)++;			      \
}

#  define UINT16ENCODE(p, i) {						      \
   *(p) = (uint8_t)(	    (i)	    & 0xff); (p)++;			      \
   *(p) = (uint8_t)(((uintn)(i) >> 8) & 0xff); (p)++;			      \
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

#else
   /* For little-endian platforms, make the compiler do the work */
#  define INT16ENCODE(p, i) {*((int16_t*)(p))=(int16_t)(i);(p)+=2;}
#  define UINT16ENCODE(p, i) {*((uint16_t*)(p))=(uint16_t)(i);(p)+=2;}
#  define INT32ENCODE(p, i)  {*((int32_t*)(p))=(int32_t)(i);(p)+=4;}
#  define UINT32ENCODE(p, i) {*((uint32_t*)(p))=(uint32_t)(i);(p)+=4;}

#  define INT64ENCODE(p, i)  {						      \
   *((int64_t *)(p)) = (int64_t)(i);					      \
   (p) += sizeof(int64_t);						      \
   if (4==sizeof(int64_t)) {						      \
      *(p)++ = (i)<0?0xff:0x00;						      \
      *(p)++ = (i)<0?0xff:0x00;						      \
      *(p)++ = (i)<0?0xff:0x00;						      \
      *(p)++ = (i)<0?0xff:0x00;						      \
   }									      \
}

#  define UINT64ENCODE(p, i) {						      \
   *((uint64_t *)(p)) = (uint64_t)(i);					      \
   (p) += sizeof(uint64_t);						      \
   if (4==sizeof(uint64_t)) {						      \
      *(p)++ = 0x00;							      \
      *(p)++ = 0x00;							      \
      *(p)++ = 0x00;							      \
      *(p)++ = 0x00;							      \
   }									      \
}

#  define INT16DECODE(p, i)  {(i)=(int16_t)(*(const int16_t*)(p));(p)+=2;}
#  define UINT16DECODE(p, i) {(i)=(uint16_t)(*(const uint16_t*)(p));(p)+=2;}
#  define INT32DECODE(p, i)  {(i)=(int32_t)(*(const int32_t*)(p));(p)+=4;}
#  define UINT32DECODE(p, i) {(i)=(uint32_t)(*(const uint32_t*)(p));(p)+=4;}
#  define INT64DECODE(p, i)  {(i)=(int64_t)(*(const int64_t*)(p));(p)+=8;}
#  define UINT64DECODE(p, i) {(i)=(uint64_t)(*(const uint64_t*)(p));(p)+=8;}

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
    intn	mdc_nelmts;	/* Size of meta data cache (elements)	*/
    intn	rdcc_nelmts;	/* Size of raw data chunk cache (elmts)	*/
    size_t	rdcc_nbytes;	/* Size of raw data chunk cache	(bytes)	*/
    double	rdcc_w0;	/* Preempt read chunks first? [0.0..1.0]*/
    hsize_t	threshold;	/* Threshold for alignment		*/
    hsize_t	alignment;	/* Alignment				*/
    uintn	gc_ref;     /* Garbage-collect references? */
    H5F_driver_t driver;	/* Low level file driver		*/
    union {

	/* Properties for in-core files */
	struct {
	    size_t increment;		/*amount by which to increment size*/
	} core;

	/* Properties for file families */
	struct {
	    struct H5F_access_t *memb_access; /*plist for the members	*/
	    haddr_t	memb_size;	/*number of bits in offset	*/
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
	    MPI_Comm     comm;  /* communicator for file access         */
	    MPI_Info     info;  /* optional info for MPI-IO             */
	    MPI_Datatype btype;	/* buffer type for xfers		*/
	    MPI_Datatype ftype;	/* file type for xfers			*/
	    haddr_t	 disp;	/* displacement for set_view in xfers	*/
	    int		 use_types; /* if !0, use btype, ftype, disp.	*/
				    /* otherwise do simple byteblk xfer	*/
	    int	     old_use_types; /* remember value of use_types	*/
				    /* from last xfer			*/
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
#if WIN32
    /*
     * Specifies the low-order word of a unique identifier associated with the
     * file.  This identifier and the volume serial number uniquely identify a
     * file. This number may change when the system is restarted or when the
     * file is opened. After a process opens a file, the identifier is
     * constant until the file is closed. An application can use this
     * identifier and the volume serial number to determine whether two
     * handles refer to the same file.
     */
    int fileindexlo;
    int fileindexhi;
#endif
} H5F_search_t;

/* For determining what the last file operation was */
typedef enum {
    H5F_OP_UNKNOWN,		/* Don't know what the last operation was*/
    H5F_OP_SEEK,		/* Last operation was a seek		*/
    H5F_OP_WRITE,		/* Last operation was a write		*/
    H5F_OP_READ			/* Last operation was a read		*/
} H5F_fileop_t;

/* A free-list entry */
#define H5MF_NFREE 32		/*size of free block array		*/
typedef struct H5MF_free_t {
    haddr_t	addr;		/*file address				*/
    hsize_t	size;		/*size of free area			*/
} H5MF_free_t;

/* Dataset transfer property list */
typedef struct H5F_xfer_t {
    size_t		buf_size;	/*max temp buffer size		     */
    void		*tconv_buf;	/*type conversion buffer or null     */
    void		*bkg_buf;	/*background buffer or null	     */
    H5T_bkg_t		need_bkg;	/*type of background buffer needed   */
    double		split_ratios[3];/*B-tree node splitting ratios	     */
    uintn       	cache_hyper;    /*cache hyperslab blocks during I/O? */
    uintn       	block_limit;    /*largest hyperslab block to cache   */
    H5D_transfer_t	xfer_mode;	/*independent or collective transfer */
} H5F_xfer_t;

/*
 * Define the low-level file interface.
 */
typedef struct H5F_low_class_t {
    htri_t	(*access)(const char *name, const H5F_access_t *access_parms,
			  int mode, H5F_search_t *key/*out*/);
    struct H5F_low_t *(*open)(const char *name,
			      const H5F_access_t *access_parms, uintn flags,
			      H5F_search_t *key/*out*/);
    herr_t	(*close)(struct H5F_low_t *lf,
			 const H5F_access_t *access_parms);
    herr_t	(*read)(struct H5F_low_t *lf, const H5F_access_t *access_parms,
    			const H5F_xfer_t *xfer_parms, const haddr_t *addr,
			size_t size, uint8_t *buf);
    herr_t	(*write)(struct H5F_low_t *lf,
			 const H5F_access_t *access_parms,
			 const H5F_xfer_t *xfer_parms, const haddr_t *addr,
			 size_t size, const uint8_t *buf);
    herr_t	(*flush)(struct H5F_low_t *lf,
			 const H5F_access_t *access_parms);
    herr_t	(*extend)(struct H5F_low_t *lf,
			  const H5F_access_t *access_parms,
			  intn op, hsize_t size, haddr_t *addr/*out*/);
    intn        (*alloc)(struct H5F_low_t *lf, intn op, hsize_t alignment,
			 hsize_t threshold, hsize_t size, H5MF_free_t *blk,
			 haddr_t *addr/*out*/);
} H5F_low_class_t;

/*
 * One of these H5F_low_t structs is allocated for each H5F_file_t struct.
 * This struct describes how to access the storage for the hdf5 address space,
 * whether that storage is file, local memory, shared memory, network
 * distributed global memory, etc.
 */
#if defined WIN32
typedef UINT uint;
#endif
typedef struct H5F_low_t {
    const H5F_low_class_t *type;/* What type of file is this?		*/
    haddr_t		eof;	/* Address of logical end-of-file	*/
    uint 		eof_written; /* whether the last byte is written */
    union {

	/* File families */
	struct {
	    char	*name;	/* Family name				*/
	    uintn	flags;	/* Flags for opening member files	*/
	    intn	nmemb;	/* Number of family members		*/
	    intn	nalloc;	/* Size of member table in elements	*/
	    struct H5F_low_t **memb; /* An array of family members	*/
	    haddr_t	memb_size; /*Size of each family member		*/
	} fam;

	/* Split meta/raw data */
	struct {
	    char	*name;	/* Base name w/o extension		*/
	    uint64_t	mask;	/* Bit that determines which file to use*/
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
	    int64_t	cur;	/* Current file position		*/
#else
	    long	cur;	/* Current file position		*/
#endif
	} stdio;

	/* In-core temp file */
	struct {
	    uint8_t	*mem;	/* Mem image of the file		*/
	    size_t	size;	/* Current file size			*/
	    size_t	alloc;	/* Current size of MEM buffer		*/
	} core;

#ifdef HAVE_PARALLEL
	/* MPI-IO */
	struct {
	    MPI_File	f;	/* MPI-IO file handle			*/
	    hbool_t	allsame;/* all procs should write same data,    *
				 * so only p0 will do the actual write  */
	} mpio;
#endif

    } u;
} H5F_low_t;

/* What types of low-level files are there? */
#ifndef H5F_LOW_DFLT
#  define H5F_LOW_DFLT	H5F_LOW_SEC2	/* The default type	  */
#endif
__DLLVAR__ const H5F_low_class_t H5F_LOW_SEC2_g[];  /*Posix section 2	 */
__DLLVAR__ const H5F_low_class_t H5F_LOW_STDIO_g[]; /*Posix stdio	 */
__DLLVAR__ const H5F_low_class_t H5F_LOW_CORE_g[];  /*In-core temp file	 */
__DLLVAR__ const H5F_low_class_t H5F_LOW_FAMILY_g[];/*File family	 */
__DLLVAR__ const H5F_low_class_t H5F_LOW_SPLIT_g[]; /*Split meta/raw data*/
#ifdef HAVE_PARALLEL
__DLLVAR__ const H5F_low_class_t H5F_LOW_MPIO_g[];  /*MPI-IO		 */
#endif

/* The raw data chunk cache */
typedef struct H5F_rdcc_t {
    uintn		ninits;	/* Number of chunk creations		*/
    uintn		nhits;	/* Number of cache hits			*/
    uintn		nmisses;/* Number of cache misses		*/
    uintn		nflushes;/* Number of cache flushes		*/
    size_t		nbytes;	/* Current cached raw data in bytes	*/
    intn		nslots;	/* Number of chunk slots allocated	*/
    struct H5F_rdcc_ent_t *head; /* Head of doubly linked list		*/
    struct H5F_rdcc_ent_t *tail; /* Tail of doubly linked list		*/
    intn		nused;	/* Number of chunk slots in use		*/
    struct H5F_rdcc_ent_t **slot; /* Chunk slots, each points to a chunk*/
} H5F_rdcc_t;

/*
 * Define the structure to store the file information for HDF5 files. One of
 * these structures is allocated per file, not per H5Fopen(). That is, set of
 * H5F_t structs can all point to the same H5F_file_t struct. The `nrefs'
 * count in this struct indicates the number of H5F_t structs which are
 * pointing to this struct.
 */
typedef struct H5F_file_t {
    H5F_search_t key;		/* The key for looking up files		*/
    uintn	flags;		/* Access Permissions for file		*/
    H5F_low_t	*lf; 		/* Lower level file handle for I/O	*/
    uintn	nrefs;		/* Ref count for times file is opened	*/
    uint32_t	consist_flags;	/* File Consistency Flags		*/
    haddr_t	boot_addr;	/* Absolute address of boot block	*/
    haddr_t	base_addr;	/* Absolute base address for rel.addrs. */
    haddr_t	freespace_addr;	/* Relative address of free-space info	*/
    haddr_t	hdf5_eof;	/* Relative addr of end of all hdf5 data*/
    struct H5AC_t *cache;	/* The object cache			*/
    H5F_create_t *create_parms;	/* File-creation property list		*/
    H5F_access_t *access_parms;	/* File-access property list		*/
    struct H5G_t *root_grp;	/* Open root group			*/
    intn	ncwfs;		/* Num entries on cwfs list		*/
    struct H5HG_heap_t **cwfs;	/* Global heap cache			*/
    H5F_rdcc_t	rdcc;		/* Raw data chunk cache			*/
    intn	fl_nfree;	/*number of free blocks in array	*/
    H5MF_free_t fl_free[H5MF_NFREE]; /*free block array			*/
} H5F_file_t;

/* Mount property list */
typedef struct H5F_mprop_t {
    hbool_t		local;	/* Are absolute symlinks local to file?	*/
} H5F_mprop_t;

/* A record of the mount table */
typedef struct H5F_mount_t {
    struct H5G_t	*group;	/* Mount point group held open		*/
    struct H5F_t	*file;	/* File mounted at that point		*/
} H5F_mount_t;
    
/*
 * The mount table describes what files are attached to (mounted on) the file
 * to which this table belongs.
 */
typedef struct H5F_mtab_t {
    struct H5F_t	*parent;/* Parent file				*/
    uintn		nmounts;/* Number of children which are mounted	*/
    uintn		nalloc;	/* Number of mount slots allocated	*/
    H5F_mount_t		*child;	/* An array of mount records		*/
} H5F_mtab_t;

/*
 * This is the top-level file descriptor.  One of these structures is
 * allocated every time H5Fopen() is called although they may contain pointers
 * to shared H5F_file_t structs. The reference count (nrefs) indicates the
 * number of times the file has been opened (the application can only open a
 * file once explicitly, but the library can open the file a second time to
 * indicate that the file is mounted on some other file).
 */
typedef struct H5F_t {
    uintn		nrefs;		/* Reference count		*/
    uintn		intent;		/* The flags passed to H5F_open()*/
    char		*name;		/* Name used to open file	*/
    H5F_file_t		*shared;	/* The shared file info		*/
    uintn		nopen_objs;	/* Number of open object headers*/
    hid_t		closing;	/* H5I_FILE_CLOSING ID or zero	*/
    H5F_mtab_t		mtab;		/* File mount table		*/
} H5F_t;

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

/* Forward declarations for prototypes arguments */
struct H5O_layout_t;
struct H5O_efl_t;
struct H5O_pline_t;
struct H5F_xfer_t;
struct H5O_fill_t;
struct H5G_entry_t;

/* library variables */
__DLLVAR__ const H5F_create_t H5F_create_dflt;
__DLLVAR__ H5F_access_t H5F_access_dflt;
__DLLVAR__ const H5F_xfer_t H5F_xfer_dflt;
__DLLVAR__ const H5F_mprop_t H5F_mount_dflt;

#ifdef HAVE_PARALLEL
__DLLVAR__  hbool_t H5_mpi_1_metawrite_g;
#endif /* HAVE_PARALLEL */

/* Private functions, not part of the publicly documented API */
__DLL__ herr_t H5F_init(void);
__DLL__ void H5F_encode_length_unusual(const H5F_t *f, uint8_t **p,
				       uint8_t *l);
__DLL__ H5F_t *H5F_open(const char *name, uintn flags,
			const H5F_create_t *create_parms,
			const H5F_access_t *access_parms);
__DLL__ herr_t H5F_close(H5F_t *f);
__DLL__ herr_t H5F_close_all(void);
__DLL__ herr_t H5F_flush_all(hbool_t invalidate);
__DLL__ herr_t H5F_debug(H5F_t *f, const haddr_t *addr, FILE * stream,
			 intn indent, intn fwidth);
__DLL__ herr_t H5F_istore_debug(H5F_t *f, const haddr_t *addr, FILE * stream,
				intn indent, intn fwidth, int ndims);
__DLL__ herr_t H5F_mountpoint(struct H5G_entry_t *find/*in,out*/);

/* Functions that operate on array storage */
__DLL__ herr_t H5F_arr_create(H5F_t *f,
			      struct H5O_layout_t *layout /*in,out*/);
__DLL__ herr_t H5F_arr_read (H5F_t *f, const struct H5F_xfer_t *xfer,
			     const struct H5O_layout_t *layout,
			     const struct H5O_pline_t *pline,
			     const struct H5O_fill_t *fill,
			     const struct H5O_efl_t *efl,
			     const hsize_t _hslab_size[],
			     const hsize_t mem_size[],
			     const hssize_t mem_offset[],
			     const hssize_t file_offset[], void *_buf/*out*/);
__DLL__ herr_t H5F_arr_write (H5F_t *f, const struct H5F_xfer_t *xfer,
			      const struct H5O_layout_t *layout,
			      const struct H5O_pline_t *pline,
			      const struct H5O_fill_t *fill,
			      const struct H5O_efl_t *efl,
			      const hsize_t _hslab_size[],
			      const hsize_t mem_size[],
			      const hssize_t mem_offset[],
			      const hssize_t file_offset[], const void *_buf);

/* Functions that operate on indexed storage */
__DLL__ herr_t H5F_istore_init (H5F_t *f);
__DLL__ herr_t H5F_istore_flush (H5F_t *f, hbool_t preempt);
__DLL__ herr_t H5F_istore_dest (H5F_t *f);
__DLL__ hsize_t H5F_istore_allocated(H5F_t *f, int ndims, haddr_t *addr);
__DLL__ herr_t H5F_istore_stats (H5F_t *f, hbool_t headers);
__DLL__ herr_t H5F_istore_create(H5F_t *f,
				 struct H5O_layout_t *layout/*in,out*/);
__DLL__ herr_t H5F_istore_read(H5F_t *f, const struct H5F_xfer_t *xfer,
			       const struct H5O_layout_t *layout,
			       const struct H5O_pline_t *pline,
			       const struct H5O_fill_t *fill,
			       const hssize_t offset[], const hsize_t size[],
			       void *buf/*out*/);
__DLL__ herr_t H5F_istore_write(H5F_t *f, const struct H5F_xfer_t *xfer,
				const struct H5O_layout_t *layout,
				const struct H5O_pline_t *pline,
				const struct H5O_fill_t *fill,
				const hssize_t offset[], const hsize_t size[],
				const void *buf);
__DLL__ herr_t H5F_istore_allocate (H5F_t *f,
				    const struct H5O_layout_t *layout,
				    const hsize_t *space_dim,
				    const double split_ratios[], 
				    const struct H5O_pline_t *pline,
				    const struct H5O_fill_t *fill);
__DLL__ herr_t H5F_istore_dump_btree(H5F_t *f, FILE *stream, int ndims,
				     haddr_t *addr);

/* Functions that operate on contiguous storage wrt boot block */
__DLL__ herr_t H5F_block_read(H5F_t *f, const haddr_t *addr, hsize_t size,
			      const H5F_xfer_t *xfer_parms, void *buf);
__DLL__ herr_t H5F_block_write(H5F_t *f, const haddr_t *addr, hsize_t size,
			       const H5F_xfer_t *xfer_parms, const void *buf);

/* Functions that operate directly on low-level files */
__DLL__ const H5F_low_class_t *H5F_low_class (H5F_driver_t driver);
__DLL__ herr_t H5F_low_extend(H5F_low_t *lf, const H5F_access_t *access_parms,
			      intn op, hsize_t size, haddr_t *addr/*out*/);
__DLL__ herr_t H5F_low_seteof(H5F_low_t *lf, const haddr_t *addr);
__DLL__ intn H5F_low_alloc (H5F_low_t *lf, intn op, hsize_t alignment,
			    hsize_t threshold, hsize_t size, H5MF_free_t *blk,
			    haddr_t *addr/*out*/);
__DLL__ htri_t H5F_low_access(const H5F_low_class_t *type, const char *name,
			      const H5F_access_t *access_parms, int mode,
			      H5F_search_t *key);
__DLL__ H5F_low_t *H5F_low_open(const H5F_low_class_t *type, const char *name,
				const H5F_access_t *access_parms, uintn flags,
				H5F_search_t *key);
__DLL__ H5F_low_t *H5F_low_close(H5F_low_t *lf,
				 const H5F_access_t *access_parms);
__DLL__ hsize_t H5F_low_size(H5F_low_t *lf, haddr_t *addr);
__DLL__ herr_t H5F_low_read(H5F_low_t *lf, const H5F_access_t *access_parms,
			    const H5F_xfer_t *xfer_parms, const haddr_t *addr,
			    size_t size, uint8_t *buf);
__DLL__ herr_t H5F_low_write(H5F_low_t *lf, const H5F_access_t *access_parms,
			     const H5F_xfer_t *xfer_parms, const haddr_t *addr,
			     size_t size, const uint8_t *buf);
__DLL__ herr_t H5F_low_flush(H5F_low_t *lf, const H5F_access_t *access_parms);

/* Functions that operate on addresses */
#define H5F_addr_eq(A1,A2) (H5F_addr_cmp(A1,A2)==0)
#define H5F_addr_ne(A1,A2) (H5F_addr_cmp(A1,A2)!=0)
#define H5F_addr_lt(A1,A2) (H5F_addr_cmp(A1,A2)<0)
#define H5F_addr_le(A1,A2) (H5F_addr_cmp(A1,A2)<=0)
#define H5F_addr_gt(A1,A2) (H5F_addr_cmp(A1,A2)>0)
#define H5F_addr_ge(A1,A2) (H5F_addr_cmp(A1,A2)>=0)

__DLL__ intn H5F_addr_cmp(const haddr_t *, const haddr_t *);
__DLL__ htri_t H5F_addr_defined(const haddr_t *);
__DLL__ void H5F_addr_undef(haddr_t *);
__DLL__ void H5F_addr_reset(haddr_t *);
__DLL__ htri_t H5F_addr_zerop(const haddr_t *);
__DLL__ void H5F_addr_encode(H5F_t *, uint8_t **, const haddr_t *);
__DLL__ void H5F_addr_decode(H5F_t *, const uint8_t **, haddr_t *);
__DLL__ void H5F_addr_print(FILE *, const haddr_t *);
__DLL__ void H5F_addr_pow2(uintn, haddr_t *);
__DLL__ void H5F_addr_inc(haddr_t *addr/*in,out*/, hsize_t inc);
__DLL__ void H5F_addr_adj(haddr_t *addr/*in,out*/, hssize_t adj);
__DLL__ void H5F_addr_add(haddr_t *, const haddr_t *);
__DLL__ uintn H5F_addr_hash(const haddr_t *, uintn mod);
__DLL__ herr_t H5F_addr_pack(H5F_t *f, haddr_t *addr,
			     const unsigned long objno[2]);

/* Functions for MPI-IO */
#ifdef HAVE_PARALLEL
__DLL__ htri_t H5F_mpio_tas_allsame(H5F_low_t *lf, hbool_t newval);
__DLL__ herr_t H5PC_Wait_for_left_neighbor(MPI_Comm comm);
__DLL__ herr_t H5PC_Signal_right_neighbor(MPI_Comm comm);
#endif /* HAVE_PARALLEL */

#endif
