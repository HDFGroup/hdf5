/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Thursday, July 29, 1999
 *
 * Purpose:	The POSIX unbuffered file driver using only the HDF5 public
 *		API and with a few optimizations: the lseek() call is made
 *		only when the current file position is unknown or needs to be
 *		changed based on previous I/O through this driver (don't mix
 *		I/O from this driver with I/O from other parts of the
 *		application to the same file).
 */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5FD_sec2_init_interface

#include <strings.h>
#include <aio.h>

#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"		/* File access				*/
#include "H5FDprivate.h"	/* File drivers				*/
#include "H5FDsec2.h"		/* Sec2 file driver			*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"		/* Property lists			*/

/* The driver identification number, initialized at runtime */
static hid_t H5FD_SEC2_g = 0;

/*
 * structure used to store the instance of struct HDaiocb associated with
 * each asynchronous operation, along with such data about the operation
 * needed for its management.
 *
 * The magic field must always be set to H5FD_SEC2_AIO_CTLBLK_T__MAGIC,
 * the op field must be set to one values specified in the
 * H5FD_SEC2_AIO_OP #defines, the status field must be set to one of
 * the values specified in the H5FD_SEC2_AIO_STATUS #defines, and
 * retries should never exceed H5FD_SEC2_AIO__MAX_RETRIES.  The
 * errno field is used to store the error code returned by a failed
 * asynchronous operation until it can be reported.
 *
 */
#ifdef H5_HAVE_AIO
#define H5FD_SEC2_AIO_CTLBLK_T__MAGIC                   0x53324143 /* 'S2AC' */

#define H5FD_SEC2_AIO_OP__UNDEFINED                     0
#define H5FD_SEC2_AIO_OP__READ                          1
#define H5FD_SEC2_AIO_OP__WRITE                         2
#define H5FD_SEC2_AIO_OP__FSYNC                         3
#define H5FD_SEC2_AIO_OP__MAX_OP                        3

#define H5FD_SEC2_AIO_STATUS__UNDEFINED                 0
#define H5FD_SEC2_AIO_STATUS__READY                     1
#define H5FD_SEC2_AIO_STATUS__QUEUED                    2
#define H5FD_SEC2_AIO_STATUS__COMPLETE                  3
#define H5FD_SEC2_AIO_STATUS__CANT_QUEUE                4
#define H5FD_SEC2_AIO_STATUS__DO_USING_SIO_ON_FINISH    5
#define H5FD_SEC2_AIO_STATUS__CANCELED_IN_PROGRESS      6
#define H5FD_SEC2_AIO_STATUS__MAX_STATUS                6

#define H5FD_SEC2_AIO__MAX_RETRIES                      3

#define H5FD_SEC2_AIO__SC_VERBOSE			TRUE

typedef struct H5FD_sec2_aio_ctlblk_t {

    uint32_t magic;
    struct H5FD_sec2_t * file_ptr;
    int op;
    int status;
    int retries;
    int err_num;
    H5FD_mem_t type;
    hid_t dxpl_id;
    haddr_t addr;
    size_t size;
    void *buf;
    struct H5FD_sec2_aio_ctlblk_t * next_ptr;
    struct H5FD_sec2_aio_ctlblk_t * prev_ptr;
    struct HDaiocb ctlblk;

} H5FD_sec2_aio_ctlblk_t;

/* declare a free list to manage aio control blocks */
H5FL_DEFINE_STATIC(H5FD_sec2_aio_ctlblk_t);

/* The following macros are used to manage doubly linked lists of
 * H5FD_sec2_aio_ctlblk_t.
 */

#define H5FD_SEC2__DLL_APPEND(entry_ptr, head_ptr, tail_ptr, len)      \
        HDassert( (entry_ptr) != NULL );                               \
        HDassert( (entry_ptr)->next_ptr == NULL );                     \
        HDassert( (entry_ptr)->prev_ptr == NULL );                     \
        if ( (head_ptr) == NULL )                                      \
        {                                                              \
           HDassert( tail_ptr == NULL );                               \
           HDassert( len == 0 );                                       \
           (head_ptr) = (entry_ptr);                                   \
           (tail_ptr) = (entry_ptr);                                   \
        }                                                              \
        else                                                           \
        {                                                              \
           HDassert( tail_ptr != NULL );                               \
           HDassert( len >= 1 );                                       \
           HDassert( ( (len) == 1 ) || ( (head_ptr) != (tail_ptr) ) ); \
           (tail_ptr)->next_ptr = (entry_ptr);                         \
           (entry_ptr)->prev_ptr = (tail_ptr);                         \
           (tail_ptr) = (entry_ptr);                                   \
        }                                                              \
        (len)++;

#define H5FD_SEC2__DLL_PREPEND(entry_ptr, head_ptr, tail_ptr, len)     \
        HDassert( (entry_ptr) != NULL );                               \
        HDassert( (entry_ptr)->next_ptr == NULL );                     \
        HDassert( (entry_ptr)->prev_ptr == NULL );                     \
        if ( (head_ptr) == NULL )                                      \
        {                                                              \
           HDassert( (tail_ptr) == NULL );                             \
           HDassert( (len) == 0 );                                     \
           (head_ptr) = (entry_ptr);                                   \
           (tail_ptr) = (entry_ptr);                                   \
        }                                                              \
        else                                                           \
        {                                                              \
           HDassert( (tail_ptr) != NULL );                             \
           HDassert( (len) >= 1 );                                     \
           HDassert( ( (len) == 1 ) || ( (head_ptr) != (tail_ptr) ) ); \
           (head_ptr)->prev_ptr = (entry_ptr);                         \
           (entry_ptr)->next_ptr = (head_ptr);                         \
           (head_ptr) = (entry_ptr);                                   \
        }                                                              \
        (len)++;

#define H5FD_SEC2__DLL_REMOVE(entry_ptr, head_ptr, tail_ptr, len)      \
        HDassert( (entry_ptr) != NULL );                               \
        HDassert( (head_ptr) != NULL );                                \
        HDassert( (tail_ptr) != NULL );                                \
        HDassert( (len) >= 1 );                                        \
        {                                                              \
           if ( (head_ptr) == (entry_ptr) )                            \
           {                                                           \
              HDassert( (entry_ptr)->prev_ptr == NULL );               \
              (head_ptr) = (entry_ptr)->next_ptr;                      \
              if ( (head_ptr) != NULL )                                \
              {                                                        \
                 (head_ptr)->prev_ptr = NULL;                          \
              }                                                        \
           }                                                           \
           else                                                        \
           {                                                           \
              HDassert( (entry_ptr)->prev_ptr != NULL );               \
              HDassert( (len) >= 2 );                                  \
              (entry_ptr)->prev_ptr->next_ptr = (entry_ptr)->next_ptr; \
           }                                                           \
           if ( (tail_ptr) == (entry_ptr) )                            \
           {                                                           \
              HDassert( (entry_ptr)->next_ptr == NULL );               \
              (tail_ptr) = (entry_ptr)->prev_ptr;                      \
              if ( (tail_ptr) != NULL )                                \
              {                                                        \
                 (tail_ptr)->next_ptr = NULL;                          \
              }                                                        \
           }                                                           \
           else                                                        \
           {                                                           \
              HDassert( (entry_ptr)->next_ptr != NULL );               \
              HDassert( (len) >= 2 );                                  \
              (entry_ptr)->next_ptr->prev_ptr = (entry_ptr)->prev_ptr; \
           }                                                           \
           entry_ptr->next_ptr = NULL;                                 \
           entry_ptr->prev_ptr = NULL;                                 \
           (len)--;                                                    \
           HDassert( (len) >= 0 );                                     \
        }
#endif /* H5_HAVE_AIO */

/*
 * The description of a file belonging to this driver. The `eoa' and `eof'
 * determine the amount of hdf5 address space in use and the high-water mark
 * of the file (the current size of the underlying Unix file). The `pos'
 * value is used to eliminate file position updates when they would be a
 * no-op. Unfortunately we've found systems that use separate file position
 * indicators for reading and writing so the lseek can only be eliminated if
 * the current operation is the same as the previous operation.  When opening
 * a file the `eof' will be set to the current file size, `eoa' will be set
 * to zero, `pos' will be set to H5F_ADDR_UNDEF (as it is when an error
 * occurs), and `op' will be set to H5F_OP_UNKNOWN.
 */
typedef struct H5FD_sec2_t {
    H5FD_t	pub;			/*public stuff, must be first	*/
    int		fd;			/*the unix file			*/
    haddr_t	eoa;			/*end of allocated region	*/
    haddr_t	eof;			/*end of file; current file size*/
    haddr_t	pos;			/*current file I/O position	*/
    H5FD_file_op_t	op;		/*last operation		*/
    char	filename[H5FD_MAX_FILENAME_LEN];     /* Copy of file name from open operation */
#ifndef _WIN32
    /*
     * On most systems the combination of device and i-node number uniquely
     * identify a file.
     */
    dev_t	device;			/*file device number		*/
#ifdef H5_VMS
    ino_t	inode[3];		/*file i-node number		*/
#else
    ino_t	inode;			/*file i-node number		*/
#endif /*H5_VMS*/
#else
    /*
     * On _WIN32 the low-order word of a unique identifier associated with the
     * file and the volume serial number uniquely identify a file. This number
     * (which, both? -rpm) may change when the system is restarted or when the
     * file is opened. After a process opens a file, the identifier is
     * constant until the file is closed. An application can use this
     * identifier and the volume serial number to determine whether two
     * handles refer to the same file.
     */
    DWORD fileindexlo;
    DWORD fileindexhi;
#endif

    /* Information from properties set by 'h5repart' tool */
    hbool_t     fam_to_sec2;    /* Whether to eliminate the family driver info
                                 * and convert this file to a single file */
#ifdef H5_HAVE_AIO
    /* the sec2 driver needs to keep track of the outstanding asynchronous
     * read, write, and fsync operation.  The following linked lists
     * exist to serve this purpose.
     */
    int32_t aio_reads_count;
    H5FD_sec2_aio_ctlblk_t * aio_reads_head;
    H5FD_sec2_aio_ctlblk_t * aio_reads_tail;

    int32_t aio_writes_count;
    H5FD_sec2_aio_ctlblk_t * aio_writes_head;
    H5FD_sec2_aio_ctlblk_t * aio_writes_tail;

    int32_t aio_fsyncs_count;
    H5FD_sec2_aio_ctlblk_t * aio_fsyncs_head;
    H5FD_sec2_aio_ctlblk_t * aio_fsyncs_tail;

    int32_t aio_canceled_count;
    H5FD_sec2_aio_ctlblk_t * aio_canceled_head;
    H5FD_sec2_aio_ctlblk_t * aio_canceled_tail;
#endif /* H5_HAVE_AIO */
} H5FD_sec2_t;


/*
 * This driver supports systems that have the lseek64() function by defining
 * some macros here so we don't have to have conditional compilations later
 * throughout the code.
 *
 * HDoff_t:	The datatype for file offsets, the second argument of
 *		the lseek() or lseek64() call.
 *
 */

/*
 * These macros check for overflow of various quantities.  These macros
 * assume that HDoff_t is signed and haddr_t and size_t are unsigned.
 *
 * ADDR_OVERFLOW:	Checks whether a file address of type `haddr_t'
 *			is too large to be represented by the second argument
 *			of the file seek function.
 *
 * SIZE_OVERFLOW:	Checks whether a buffer size of type `hsize_t' is too
 *			large to be represented by the `size_t' type.
 *
 * REGION_OVERFLOW:	Checks whether an address and size pair describe data
 *			which can be addressed entirely by the second
 *			argument of the file seek function.
 */
#define MAXADDR (((haddr_t)1<<(8*sizeof(HDoff_t)-1))-1)
#define ADDR_OVERFLOW(A)	(HADDR_UNDEF==(A) ||			      \
				 ((A) & ~(haddr_t)MAXADDR))
#define SIZE_OVERFLOW(Z)	((Z) & ~(hsize_t)MAXADDR)
#define REGION_OVERFLOW(A,Z)	(ADDR_OVERFLOW(A) || SIZE_OVERFLOW(Z) ||      \
                                 HADDR_UNDEF==(A)+(Z) ||		      \
				 (HDoff_t)((A)+(Z))<(HDoff_t)(A))

/* Prototypes */
static H5FD_t *H5FD_sec2_open(const char *name, unsigned flags, hid_t fapl_id,
			      haddr_t maxaddr);
static herr_t H5FD_sec2_close(H5FD_t *_file);
static int H5FD_sec2_cmp(const H5FD_t *_f1, const H5FD_t *_f2);
static herr_t H5FD_sec2_query(const H5FD_t *_f1, unsigned long *flags);
static haddr_t H5FD_sec2_get_eoa(const H5FD_t *_file, H5FD_mem_t type);
static herr_t H5FD_sec2_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t addr);
static haddr_t H5FD_sec2_get_eof(const H5FD_t *_file);
static herr_t  H5FD_sec2_get_handle(H5FD_t *_file, hid_t fapl, void** file_handle);
static herr_t H5FD_sec2_read(H5FD_t *_file, H5FD_mem_t type, hid_t fapl_id, haddr_t addr,
			     size_t size, void *buf);
static herr_t H5FD_sec2_write(H5FD_t *_file, H5FD_mem_t type, hid_t fapl_id, haddr_t addr,
			      size_t size, const void *buf);
static herr_t H5FD_sec2_truncate(H5FD_t *_file, hid_t dxpl_id, hbool_t closing);
#ifdef H5_HAVE_AIO
static herr_t H5FD_sec2_aio_alloc_ctlblk(H5FD_sec2_aio_ctlblk_t **ctlblk_ptr_ptr);
static hbool_t H5FD_sec2_aio_ctlblk_sc(H5FD_t *file,
                                       H5FD_sec2_aio_ctlblk_t *ctlblk_ptr,
                                       int expected_op);
static herr_t H5FD_sec2_aio_discard_ctlblk(H5FD_sec2_aio_ctlblk_t *ctlblk_ptr);
static void H5FD_sec2_aio_dump_ctlblk(FILE * out_stream, 
                                      H5FD_sec2_aio_ctlblk_t *ctlblk_ptr);
static herr_t H5FD_sec2_aio_read(H5FD_t *file, H5FD_mem_t type, hid_t dxpl,
                                 haddr_t addr, size_t size, void *buffer,
                                 void **ctlblk_ptr_ptr);
static herr_t H5FD_sec2_aio_write(H5FD_t *file, H5FD_mem_t type, hid_t dxpl,
                                  haddr_t addr, size_t size, void *buffer,
                                  void **ctlblk_ptr_ptr);
static herr_t H5FD_sec2_aio_test(hbool_t *done_ptr, void *ctlblk_ptr);
static herr_t H5FD_sec2_aio_wait(void *ctlblk_ptr);
static herr_t H5FD_sec2_aio_finish(int *errno_ptr, void *ctlblk_ptr);
static herr_t H5FD_sec2_aio_fsync(H5FD_t *file, void **ctlblk_ptr_ptr);
static herr_t H5FD_sec2_aio_cancel(void *ctlblk_ptr);
static herr_t H5FD_sec2_aio_cancel__retire_canceled_in_progress(
                                                H5FD_sec2_t * file_ptr);
static herr_t H5FD_sec2_aio_sc(H5FD_t *file, 
	                       FILE * output_stream,
                               const char * tag,
                               hbool_t verbose);
#endif /* H5_HAVE_AIO */
static herr_t H5FD_sec2_fsync(H5FD_t *file, hid_t UNUSED dxpl);

static const H5FD_class_t H5FD_sec2_g = {
    "sec2",					/*name			*/
    MAXADDR,					/*maxaddr		*/
    H5F_CLOSE_WEAK,				/* fc_degree		*/
    NULL,					/*sb_size		*/
    NULL,					/*sb_encode		*/
    NULL,					/*sb_decode		*/
    0, 						/*fapl_size		*/
    NULL,					/*fapl_get		*/
    NULL,					/*fapl_copy		*/
    NULL, 					/*fapl_free		*/
    0,						/*dxpl_size		*/
    NULL,					/*dxpl_copy		*/
    NULL,					/*dxpl_free		*/
    H5FD_sec2_open,				/*open			*/
    H5FD_sec2_close,				/*close			*/
    H5FD_sec2_cmp,				/*cmp			*/
    H5FD_sec2_query,				/*query			*/
    NULL,					/*get_type_map		*/
    NULL,					/*alloc			*/
    NULL,					/*free			*/
    H5FD_sec2_get_eoa,				/*get_eoa		*/
    H5FD_sec2_set_eoa, 				/*set_eoa		*/
    H5FD_sec2_get_eof,				/*get_eof		*/
    H5FD_sec2_get_handle,                       /*get_handle            */
    H5FD_sec2_read,				/*read			*/
    H5FD_sec2_write,				/*write			*/
    NULL,					/*flush			*/
    H5FD_sec2_truncate,				/*truncate		*/
    NULL,                                       /*lock                  */
    NULL,                                       /*unlock                */
#ifdef H5_HAVE_AIO
    H5FD_sec2_aio_read,                         /*aio_read              */
    H5FD_sec2_aio_write,                        /*aio_write             */
    H5FD_sec2_aio_test,                         /*aio_test              */
    H5FD_sec2_aio_wait,                         /*aio_wait              */
    H5FD_sec2_aio_finish,                       /*aio_finish            */
    H5FD_sec2_aio_fsync,                        /*aio_fsync             */
    H5FD_sec2_aio_cancel,                       /*aio_cancel            */
#else /* H5_HAVE_AIO */
    NULL,                                       /*aio_read              */
    NULL,                                       /*aio_write             */
    NULL,                                       /*aio_test              */
    NULL,                                       /*aio_wait              */
    NULL,                                       /*aio_finish            */
    NULL,                                       /*aio_fsync             */
    NULL,                                       /*aio_cancel            */
#endif /* H5_HAVE_AIO */
    H5FD_sec2_fsync,                            /*fsync                 */
    H5FD_FLMAP_SINGLE 				/*fl_map		*/
};

/* Declare a free list to manage the H5FD_sec2_t struct */
H5FL_DEFINE_STATIC(H5FD_sec2_t);


/*--------------------------------------------------------------------------
NAME
   H5FD_sec2_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5FD_sec2_init_interface()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5FD_sec2_init currently).

--------------------------------------------------------------------------*/
static herr_t
H5FD_sec2_init_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_sec2_init_interface)

    FUNC_LEAVE_NOAPI(H5FD_sec2_init())
} /* H5FD_sec2_init_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_init
 *
 * Purpose:	Initialize this driver by registering the driver with the
 *		library.
 *
 * Return:	Success:	The driver ID for the sec2 driver.
 *		Failure:	Negative.
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 29, 1999
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5FD_sec2_init(void)
{
    hid_t ret_value;            /* Return value */

    FUNC_ENTER_NOAPI(H5FD_sec2_init, FAIL)

    if(H5I_VFL != H5I_get_type(H5FD_SEC2_g))
        H5FD_SEC2_g = H5FD_register(&H5FD_sec2_g, sizeof(H5FD_class_t), FALSE);

    /* Set return value */
    ret_value = H5FD_SEC2_g;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_sec2_init() */


/*---------------------------------------------------------------------------
 * Function:	H5FD_sec2_term
 *
 * Purpose:	Shut down the VFD
 *
 * Return:	<none>
 *
 * Programmer:  Quincey Koziol
 *              Friday, Jan 30, 2004
 *
 *---------------------------------------------------------------------------
 */
void
H5FD_sec2_term(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_sec2_term)

    /* Reset VFL ID */
    H5FD_SEC2_g = 0;

    FUNC_LEAVE_NOAPI_VOID
} /* end H5FD_sec2_term() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_fapl_sec2
 *
 * Purpose:	Modify the file access property list to use the H5FD_SEC2
 *		driver defined in this source file.  There are no driver
 *		specific properties.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Thursday, February 19, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_sec2(hid_t fapl_id)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value;

    FUNC_ENTER_API(H5Pset_fapl_sec2, FAIL)
    H5TRACE1("e", "i", fapl_id);

    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    ret_value = H5P_set_driver(plist, H5FD_SEC2, NULL);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fapl_sec2() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_open
 *
 * Purpose:	Create and/or opens a Unix file as an HDF5 file.
 *
 * Return:	Success:	A pointer to a new file data structure. The
 *				public fields will be initialized by the
 *				caller, which is always H5FD_open().
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 29, 1999
 *
 * Changes:	Added initialization of AIO related fields.  
 *
 *                                                  -- JRM - 3/14/11
 *
 *-------------------------------------------------------------------------
 */
static H5FD_t *
H5FD_sec2_open(const char *name, unsigned flags, hid_t fapl_id, haddr_t maxaddr)
{
    H5FD_sec2_t	*file = NULL;   /* sec2 VFD info */
    int		fd = (-1);      /* File descriptor */
    int		o_flags;        /* Flags for open() call */
#ifdef _WIN32
    HFILE filehandle;
    struct _BY_HANDLE_FILE_INFORMATION fileinfo;
#endif
    h5_stat_t	sb;
    H5FD_t	*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FD_sec2_open)

    /* Sanity check on file offsets */
    HDcompile_assert(sizeof(HDoff_t) >= sizeof(size_t));

    /* Check arguments */
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid file name")
    if(0 == maxaddr || HADDR_UNDEF == maxaddr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, NULL, "bogus maxaddr")
    if(ADDR_OVERFLOW(maxaddr))
        HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, NULL, "bogus maxaddr")

    /* Build the open flags */
    o_flags = (H5F_ACC_RDWR & flags) ? O_RDWR : O_RDONLY;
    if(H5F_ACC_TRUNC & flags)
        o_flags |= O_TRUNC;
    if(H5F_ACC_CREAT & flags)
        o_flags |= O_CREAT;
    if(H5F_ACC_EXCL & flags)
        o_flags |= O_EXCL;

    /* Open the file */
    if((fd = HDopen(name, o_flags, 0666)) < 0) {
        int myerrno = errno;

        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file: name = '%s', errno = %d, error message = '%s', flags = %x, o_flags = %x", name, myerrno, HDstrerror(myerrno), flags, (unsigned)o_flags);
    } /* end if */
    if(HDfstat(fd, &sb) < 0)
        HSYS_GOTO_ERROR(H5E_FILE, H5E_BADFILE, NULL, "unable to fstat file")

    /* Create the new file struct */
    if(NULL == (file = H5FL_CALLOC(H5FD_sec2_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "unable to allocate file struct")

    file->fd = fd;
    H5_ASSIGN_OVERFLOW(file->eof, sb.st_size, h5_stat_size_t, haddr_t);
    file->pos = HADDR_UNDEF;
    file->op = OP_UNKNOWN;
#ifdef _WIN32
    filehandle = _get_osfhandle(fd);
    (void)GetFileInformationByHandle((HANDLE)filehandle, &fileinfo);
    file->fileindexhi = fileinfo.nFileIndexHigh;
    file->fileindexlo = fileinfo.nFileIndexLow;
#else /* _WIN32 */
    file->device = sb.st_dev;
#ifdef H5_VMS
    file->inode[0] = sb.st_ino[0];
    file->inode[1] = sb.st_ino[1];
    file->inode[2] = sb.st_ino[2];
#else
    file->inode = sb.st_ino;
#endif /*H5_VMS*/

#endif /* _WIN32 */

#ifdef H5_HAVE_AIO
    /* initialize the fields supporting AIO */
    file->aio_reads_count  = 0;
    file->aio_reads_head   = NULL;
    file->aio_reads_tail   = NULL;

    file->aio_writes_count = 0;
    file->aio_writes_head  = NULL;
    file->aio_writes_tail  = NULL;

    file->aio_fsyncs_count = 0;
    file->aio_fsyncs_head  = NULL;
    file->aio_fsyncs_tail  = NULL;

    file->aio_canceled_count = 0;
    file->aio_canceled_head  = NULL;
    file->aio_canceled_tail  = NULL;
#endif /* H5_HAVE_AIO */

    /* Retain a copy of the name used to open the file, for possible error reporting */
    HDstrncpy(file->filename, name, sizeof(file->filename));
    file->filename[sizeof(file->filename) - 1] = '\0';

    /* Check for non-default FAPL */
    if(H5P_FILE_ACCESS_DEFAULT != fapl_id) {
        H5P_genplist_t      *plist;      /* Property list pointer */

        /* Get the FAPL */
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
            HGOTO_ERROR(H5E_VFL, H5E_BADTYPE, NULL, "not a file access property list")

        /* This step is for h5repart tool only. If user wants to change file driver from
         * family to sec2 while using h5repart, this private property should be set so that
         * in the later step, the library can ignore the family driver information saved
         * in the superblock.
         */
        if(H5P_exist_plist(plist, H5F_ACS_FAMILY_TO_SEC2_NAME) > 0)
            if(H5P_get(plist, H5F_ACS_FAMILY_TO_SEC2_NAME, &file->fam_to_sec2) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "can't get property of changing family to sec2")
    } /* end if */

    /* Set return value */
    ret_value = (H5FD_t*)file;

done:
    if(NULL == ret_value) {
        if(fd >= 0)
            HDclose(fd);
        if(file)
            file = H5FL_FREE(H5FD_sec2_t, file);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_sec2_open() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_close
 *
 * Purpose:	Closes a Unix file.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1, file not closed.
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 29, 1999
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_sec2_close(H5FD_t *_file)
{
    H5FD_sec2_t	*file = (H5FD_sec2_t *)_file;
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FD_sec2_close)

    /* Sanity check */
    HDassert(file);

    /* Close the underlying file */
    if(HDclose(file->fd) < 0)
        HSYS_GOTO_ERROR(H5E_IO, H5E_CANTCLOSEFILE, FAIL, "unable to close file")

    /* Release the file info */
    file = H5FL_FREE(H5FD_sec2_t, file);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_sec2_close() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_cmp
 *
 * Purpose:	Compares two files belonging to this driver using an
 *		arbitrary (but consistent) ordering.
 *
 * Return:	Success:	A value like strcmp()
 *
 *		Failure:	never fails (arguments were checked by the
 *				caller).
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 29, 1999
 *
 *-------------------------------------------------------------------------
 */
static int
H5FD_sec2_cmp(const H5FD_t *_f1, const H5FD_t *_f2)
{
    const H5FD_sec2_t	*f1 = (const H5FD_sec2_t *)_f1;
    const H5FD_sec2_t	*f2 = (const H5FD_sec2_t *)_f2;
    int ret_value = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_sec2_cmp)

#ifdef _WIN32
    if(f1->fileindexhi < f2->fileindexhi) HGOTO_DONE(-1)
    if(f1->fileindexhi > f2->fileindexhi) HGOTO_DONE(1)

    if(f1->fileindexlo < f2->fileindexlo) HGOTO_DONE(-1)
    if(f1->fileindexlo > f2->fileindexlo) HGOTO_DONE(1)

#else
#ifdef H5_DEV_T_IS_SCALAR
    if(f1->device < f2->device) HGOTO_DONE(-1)
    if(f1->device > f2->device) HGOTO_DONE(1)
#else /* H5_DEV_T_IS_SCALAR */
    /* If dev_t isn't a scalar value on this system, just use memcmp to
     * determine if the values are the same or not.  The actual return value
     * shouldn't really matter...
     */
    if(HDmemcmp(&(f1->device),&(f2->device),sizeof(dev_t)) < 0) HGOTO_DONE(-1)
    if(HDmemcmp(&(f1->device),&(f2->device),sizeof(dev_t)) > 0) HGOTO_DONE(1)
#endif /* H5_DEV_T_IS_SCALAR */

#ifndef H5_VMS
    if(f1->inode < f2->inode) HGOTO_DONE(-1)
    if(f1->inode > f2->inode) HGOTO_DONE(1)
#else
    if(HDmemcmp(&(f1->inode), &(f2->inode), 3 * sizeof(ino_t)) < 0) HGOTO_DONE(-1)
    if(HDmemcmp(&(f1->inode), &(f2->inode), 3 * sizeof(ino_t)) > 0) HGOTO_DONE(1)
#endif /*H5_VMS*/

#endif

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_sec2_cmp() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_query
 *
 * Purpose:	Set the flags that this VFL driver is capable of supporting.
 *              (listed in H5FDpublic.h)
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Friday, August 25, 2000
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_sec2_query(const H5FD_t *_file, unsigned long *flags /* out */)
{
    const H5FD_sec2_t	*file = (const H5FD_sec2_t *)_file;    /* sec2 VFD info */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_sec2_query)

    /* Set the VFL feature flags that this driver supports */
    if(flags) {
        *flags = 0;
        *flags |= H5FD_FEAT_AGGREGATE_METADATA; /* OK to aggregate metadata allocations */
        *flags |= H5FD_FEAT_ACCUMULATE_METADATA; /* OK to accumulate metadata for faster writes */
        *flags |= H5FD_FEAT_DATA_SIEVE;       /* OK to perform data sieving for faster raw data reads & writes */
        *flags |= H5FD_FEAT_AGGREGATE_SMALLDATA; /* OK to aggregate "small" raw data allocations */
        *flags |= H5FD_FEAT_POSIX_COMPAT_HANDLE; /* VFD handle is POSIX I/O call compatible */

        /* Check for flags that are set by h5repart */
        if(file->fam_to_sec2)
            *flags |= H5FD_FEAT_IGNORE_DRVRINFO; /* Ignore the driver info when file is opened (which eliminates it) */
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD_sec2_query() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_get_eoa
 *
 * Purpose:	Gets the end-of-address marker for the file. The EOA marker
 *		is the first address past the last byte allocated in the
 *		format address space.
 *
 * Return:	Success:	The end-of-address marker.
 *		Failure:	HADDR_UNDEF
 *
 * Programmer:	Robb Matzke
 *              Monday, August  2, 1999
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_sec2_get_eoa(const H5FD_t *_file, H5FD_mem_t UNUSED type)
{
    const H5FD_sec2_t	*file = (const H5FD_sec2_t *)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_sec2_get_eoa)

    FUNC_LEAVE_NOAPI(file->eoa)
} /* end H5FD_sec2_get_eoa() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_set_eoa
 *
 * Purpose:	Set the end-of-address marker for the file. This function is
 *		called shortly after an existing HDF5 file is opened in order
 *		to tell the driver where the end of the HDF5 data is located.
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 29, 1999
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_sec2_set_eoa(H5FD_t *_file, H5FD_mem_t UNUSED type, haddr_t addr)
{
    H5FD_sec2_t	*file = (H5FD_sec2_t *)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_sec2_set_eoa)

    file->eoa = addr;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD_sec2_set_eoa() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_get_eof
 *
 * Purpose:	Returns the end-of-file marker, which is the greater of
 *		either the Unix end-of-file or the HDF5 end-of-address
 *		markers.
 *
 * Return:	Success:	End of file address, the first address past
 *				the end of the "file", either the Unix file
 *				or the HDF5 file.
 *		Failure:	HADDR_UNDEF
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 29, 1999
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_sec2_get_eof(const H5FD_t *_file)
{
    const H5FD_sec2_t	*file = (const H5FD_sec2_t *)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_sec2_get_eof)

    FUNC_LEAVE_NOAPI(MAX(file->eof, file->eoa))
} /* end H5FD_sec2_get_eof() */


/*-------------------------------------------------------------------------
 * Function:       H5FD_sec2_get_handle
 *
 * Purpose:        Returns the file handle of sec2 file driver.
 *
 * Returns:        Non-negative if succeed or negative if fails.
 *
 * Programmer:     Raymond Lu
 *                 Sept. 16, 2002
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
static herr_t
H5FD_sec2_get_handle(H5FD_t *_file, hid_t UNUSED fapl, void **file_handle)
{
    H5FD_sec2_t         *file = (H5FD_sec2_t *)_file;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5FD_sec2_get_handle)

    if(!file_handle)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file handle not valid")

    *file_handle = &(file->fd);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_sec2_get_handle() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_read
 *
 * Purpose:	Reads SIZE bytes of data from FILE beginning at address ADDR
 *		into buffer BUF according to data transfer properties in
 *		DXPL_ID.
 *
 * Return:	Success:	Zero. Result is stored in caller-supplied
 *				buffer BUF.
 *		Failure:	-1, Contents of buffer BUF are undefined.
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 29, 1999
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
static herr_t
H5FD_sec2_read(H5FD_t *_file, H5FD_mem_t UNUSED type, hid_t UNUSED dxpl_id,
    haddr_t addr, size_t size, void *buf/*out*/)
{
    H5FD_sec2_t		*file = (H5FD_sec2_t *)_file;
    ssize_t		nbytes;
    herr_t              ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FD_sec2_read)

    HDassert(file && file->pub.cls);
    HDassert(buf);

    /* Check for overflow conditions */
    if(!H5F_addr_defined(addr))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "addr undefined, addr = %llu", (unsigned long long)addr)
    if(REGION_OVERFLOW(addr, size))
        HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow, addr = %llu", (unsigned long long)addr)
    if((addr + size) > file->eoa)
        HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow, addr = %llu", (unsigned long long)addr)

    /* Seek to the correct location */
    if(addr != file->pos || OP_READ != file->op) {
        if(HDlseek(file->fd, (HDoff_t)addr, SEEK_SET) < 0)
            HSYS_GOTO_ERROR(H5E_IO, H5E_SEEKERROR, FAIL, "unable to seek to proper position")
    } /* end if */

    /*
     * Read data, being careful of interrupted system calls, partial results,
     * and the end of the file.
     */
    while(size > 0) {
        do {
            nbytes = HDread(file->fd, buf, size);
        } while(-1 == nbytes && EINTR == errno);
        if(-1 == nbytes) { /* error */
            int myerrno = errno;
            time_t mytime = HDtime(NULL);
            HDoff_t myoffset = HDlseek(file->fd, (HDoff_t)0, SEEK_CUR);

            HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "file read failed: time = %s, filename = '%s', file descriptor = %d, errno = %d, error message = '%s', buf = %p, size = %lu, offset = %llu", HDctime(&mytime), file->filename, file->fd, myerrno, HDstrerror(myerrno), buf, (unsigned long)size, (unsigned long long)myoffset);
        } /* end if */
        if(0 == nbytes) {
            /* end of file but not end of format address space */
            HDmemset(buf, 0, size);
            break;
        } /* end if */
        HDassert(nbytes >= 0);
        HDassert((size_t)nbytes <= size);
        H5_CHECK_OVERFLOW(nbytes, ssize_t, size_t);
        size -= (size_t)nbytes;
        H5_CHECK_OVERFLOW(nbytes, ssize_t, haddr_t);
        addr += (haddr_t)nbytes;
        buf = (char *)buf + nbytes;
    } /* end while */

    /* Update current position */
    file->pos = addr;
    file->op = OP_READ;

done:
    if(ret_value < 0) {
        /* Reset last file I/O information */
        file->pos = HADDR_UNDEF;
        file->op = OP_UNKNOWN;
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_sec2_read() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_write
 *
 * Purpose:	Writes SIZE bytes of data to FILE beginning at address ADDR
 *		from buffer BUF according to data transfer properties in
 *		DXPL_ID.
 *
 * Return:	Success:	Zero
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 29, 1999
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
static herr_t
H5FD_sec2_write(H5FD_t *_file, H5FD_mem_t UNUSED type, hid_t UNUSED dxpl_id, haddr_t addr,
		size_t size, const void *buf)
{
    H5FD_sec2_t		*file = (H5FD_sec2_t *)_file;
    ssize_t		nbytes;
    herr_t              ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FD_sec2_write)

    HDassert(file && file->pub.cls);
    HDassert(buf);

    /* Check for overflow conditions */
    if(!H5F_addr_defined(addr))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "addr undefined, addr = %llu", (unsigned long long)addr)
    if(REGION_OVERFLOW(addr, size))
        HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow, addr = %llu, size = %llu", (unsigned long long)addr, (unsigned long long)size)
    if((addr + size) > file->eoa)
        HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow, addr = %llu, size = %llu, eoa = %llu", (unsigned long long)addr, (unsigned long long)size, (unsigned long long)file->eoa)

    /* Seek to the correct location */
    if(addr != file->pos || OP_WRITE != file->op) {
        if(HDlseek(file->fd, (HDoff_t)addr, SEEK_SET) < 0)
            HSYS_GOTO_ERROR(H5E_IO, H5E_SEEKERROR, FAIL, "unable to seek to proper position")
    } /* end if */

    /*
     * Write the data, being careful of interrupted system calls and partial
     * results
     */
    while(size > 0) {
        do {
            nbytes = HDwrite(file->fd, buf, size);
        } while(-1 == nbytes && EINTR == errno);
        if(-1 == nbytes) { /* error */
            int myerrno = errno;
            time_t mytime = HDtime(NULL);
            HDoff_t myoffset = HDlseek(file->fd, (HDoff_t)0, SEEK_CUR);

            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "file write failed: time = %s, filename = '%s', file descriptor = %d, errno = %d, error message = '%s', buf = %p, size = %lu, offset = %llu", HDctime(&mytime), file->filename, file->fd, myerrno, HDstrerror(myerrno), buf, (unsigned long)size, (unsigned long long)myoffset);
        } /* end if */
        HDassert(nbytes > 0);
        HDassert((size_t)nbytes <= size);
        H5_CHECK_OVERFLOW(nbytes, ssize_t, size_t);
        size -= (size_t)nbytes;
        H5_CHECK_OVERFLOW(nbytes, ssize_t, haddr_t);
        addr += (haddr_t)nbytes;
        buf = (const char *)buf + nbytes;
    } /* end while */

    /* Update current position and eof */
    file->pos = addr;
    file->op = OP_WRITE;
    if(file->pos > file->eof)
        file->eof = file->pos;

done:
    if(ret_value < 0) {
        /* Reset last file I/O information */
        file->pos = HADDR_UNDEF;
        file->op = OP_UNKNOWN;
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_sec2_write() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_truncate
 *
 * Purpose:	Makes sure that the true file size is the same (or larger)
 *		than the end-of-address.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
static herr_t
H5FD_sec2_truncate(H5FD_t *_file, hid_t UNUSED dxpl_id, hbool_t UNUSED closing)
{
    H5FD_sec2_t *file = (H5FD_sec2_t *)_file;
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FD_sec2_truncate)

    HDassert(file);

    /* Extend the file to make sure it's large enough */
    if(!H5F_addr_eq(file->eoa, file->eof)) {
#ifdef _WIN32
        HFILE filehandle;   /* Windows file handle */
        LARGE_INTEGER li;   /* 64-bit integer for SetFilePointer() call */

        /* Map the posix file handle to a Windows file handle */
        filehandle = _get_osfhandle(file->fd);

        /* Translate 64-bit integers into form Windows wants */
        /* [This algorithm is from the Windows documentation for SetFilePointer()] */
        li.QuadPart = (LONGLONG)file->eoa;
        (void)SetFilePointer((HANDLE)filehandle, li.LowPart, &li.HighPart, FILE_BEGIN);
        if(SetEndOfFile((HANDLE)filehandle) == 0)
            HGOTO_ERROR(H5E_IO, H5E_SEEKERROR, FAIL, "unable to extend file properly")
#else /* _WIN32 */
#ifdef H5_VMS
        /* Reset seek offset to the beginning of the file, so that the file isn't
         * re-extended later.  This may happen on Open VMS. */
        if(-1 == HDlseek(file->fd, (HDoff_t)0, SEEK_SET))
            HSYS_GOTO_ERROR(H5E_IO, H5E_SEEKERROR, FAIL, "unable to seek to proper position")
#endif

        if(-1 == HDftruncate(file->fd, (HDoff_t)file->eoa))
            HSYS_GOTO_ERROR(H5E_IO, H5E_SEEKERROR, FAIL, "unable to extend file properly")
#endif /* _WIN32 */

        /* Update the eof value */
        file->eof = file->eoa;

        /* Reset last file I/O information */
        file->pos = HADDR_UNDEF;
        file->op = OP_UNKNOWN;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_sec2_truncate() */

#ifdef H5_HAVE_AIO

/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_aio_alloc_ctlblk
 *
 * Purpose:	Allocate a control block for use in an asynchronous 
 *		read, write, or fsync.
 *
 *		If successful, return a pointer to the newly allocated
 *		and initialize control block in *ctlblk_ptr_ptr.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              5/20/10
 *
 *-------------------------------------------------------------------------
 */

static herr_t 
H5FD_sec2_aio_alloc_ctlblk(H5FD_sec2_aio_ctlblk_t **ctlblk_ptr_ptr)
{
    herr_t                   ret_value = SUCCEED;       /* Return value */
    H5FD_sec2_aio_ctlblk_t * ctlblk_ptr = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5FD_sec2_aio_alloc_ctlblk)

    HDassert( ctlblk_ptr_ptr != NULL );
    HDassert( *ctlblk_ptr_ptr == NULL );

    ctlblk_ptr = H5FL_CALLOC(H5FD_sec2_aio_ctlblk_t);

    if ( ctlblk_ptr == NULL ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "memory allocation failed")
    }

    ctlblk_ptr->magic    = H5FD_SEC2_AIO_CTLBLK_T__MAGIC;
    ctlblk_ptr->file_ptr = NULL;
    ctlblk_ptr->op       = H5FD_SEC2_AIO_OP__UNDEFINED;
    ctlblk_ptr->status   = H5FD_SEC2_AIO_STATUS__UNDEFINED;
    ctlblk_ptr->retries  = -1;
    ctlblk_ptr->err_num  = 0;
    ctlblk_ptr->type     = H5FD_MEM_NOLIST;
    ctlblk_ptr->dxpl_id  = H5I_INVALID_HID;
    ctlblk_ptr->addr     = HADDR_UNDEF;
    ctlblk_ptr->size     = 0;
    ctlblk_ptr->buf      = NULL;
    ctlblk_ptr->next_ptr = NULL;
    ctlblk_ptr->prev_ptr = NULL;

    /* zero the posix control block as recommended */
    HDmemset((void *)(&(ctlblk_ptr->ctlblk)), 0, sizeof(struct HDaiocb));

    *ctlblk_ptr_ptr = ctlblk_ptr;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5FD_sec2_aio_alloc_ctlblk */


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_aio_ctlblk_sc
 *
 * Purpose:	Run a sanity check on the indicated control block.  Return
 *		true if it passes, and false otherwise.
 *
 * Return:	Sanity check passes:	TRUE
 *
 *		Sanity check fails:	FALSE
 *
 * Programmer:	John Mainzer
 *              3/23/11
 *
 *-------------------------------------------------------------------------
 */

static hbool_t 
H5FD_sec2_aio_ctlblk_sc(H5FD_t *file,
                        H5FD_sec2_aio_ctlblk_t *ctlblk_ptr,
                        int expected_op)
{
    hbool_t sc_passes = TRUE;

    /* the following set of sanity checks could be much more detailed, but
     * it should suffice for now.
     */

    if ( ctlblk_ptr == NULL ) {

        sc_passes = FALSE;

    } else if ( ( ctlblk_ptr->magic != H5FD_SEC2_AIO_CTLBLK_T__MAGIC ) ||
                ( ctlblk_ptr->file_ptr != (H5FD_sec2_t *)file ) ||
                ( ctlblk_ptr->op != expected_op ) ||
                ( ctlblk_ptr->status <= H5FD_SEC2_AIO_OP__UNDEFINED ) ||
                ( ctlblk_ptr->status > H5FD_SEC2_AIO_STATUS__MAX_STATUS ) ||
                ( ctlblk_ptr->retries < -1 ) ||
                ( ctlblk_ptr->retries > H5FD_SEC2_AIO__MAX_RETRIES ) ) {

        sc_passes = FALSE;
    }

    return(sc_passes);

} /* H5FD_sec2_aio_ctlblk_sc */


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_aio_discard_ctlblk
 *
 * Purpose:	Free the control block pointed to by ctlblk_ptr, marking
 *		it as invalid in passing.  
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              5/20/10
 *
 *-------------------------------------------------------------------------
 */

static herr_t 
H5FD_sec2_aio_discard_ctlblk(H5FD_sec2_aio_ctlblk_t *ctlblk_ptr)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FD_sec2_aio_discard_ctlblk)

    HDassert( ctlblk_ptr != NULL );

    if ( ctlblk_ptr->magic != H5FD_SEC2_AIO_CTLBLK_T__MAGIC ) {

        HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, "bad ctlblk magic")
    }

    ctlblk_ptr->magic    = 0;
    ctlblk_ptr->file_ptr = NULL;
    ctlblk_ptr->op       = H5FD_SEC2_AIO_OP__UNDEFINED;
    ctlblk_ptr->status   = H5FD_SEC2_AIO_STATUS__UNDEFINED;
    ctlblk_ptr->retries  = -1;
    ctlblk_ptr->err_num  = 0;
    ctlblk_ptr->type     = H5FD_MEM_NOLIST;
    ctlblk_ptr->dxpl_id  = H5I_INVALID_HID;
    ctlblk_ptr->addr     = HADDR_UNDEF;
    ctlblk_ptr->size     = 0;
    ctlblk_ptr->buf      = NULL;
    ctlblk_ptr->next_ptr = NULL;
    ctlblk_ptr->prev_ptr = NULL;

    /* zero the posix control block */
    HDmemset((void *)(&(ctlblk_ptr->ctlblk)), 0, sizeof(struct HDaiocb));

    ctlblk_ptr = H5FL_FREE(H5FD_sec2_aio_ctlblk_t, ctlblk_ptr);

    if ( ctlblk_ptr != NULL ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                    "control block free failed")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5FD_sec2_aio_discard_ctlblk */


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_aio_dump_ctlblk
 *
 * Purpose:	Dump the indicated sec2 aio control block to the stream 
 *		indicated.
 *
 * Return:	Void.
 *
 * Programmer:	John Mainzer
 *              5/20/10
 *
 *-------------------------------------------------------------------------
 */

static void
H5FD_sec2_aio_dump_ctlblk(FILE * out_stream, 
                          H5FD_sec2_aio_ctlblk_t *ctlblk_ptr)
{
    HDassert( out_stream != NULL );

    if ( ctlblk_ptr == NULL ) {

        HDfprintf(out_stream, "dump: ctlblk_ptr == NULL.\n");

    } else {

        HDfprintf(out_stream, "dump: ctlblk_ptr = 0x%llx.\n", (long long)ctlblk_ptr);
        HDfprintf(out_stream, "dump: 	ctlblk_ptr->magic = 0x%llx.\n", 
                  (long long)(ctlblk_ptr->magic));
        HDfprintf(out_stream, "dump: 	ctlblk_ptr->file_ptr = 0x%llx.\n", 
                  (long long)(ctlblk_ptr->file_ptr));
        HDfprintf(out_stream, "dump: 	ctlblk_ptr->op = 0x%llx.\n", 
                  (long long)(ctlblk_ptr->op));
        HDfprintf(out_stream, "dump: 	ctlblk_ptr->status = 0x%llx.\n", 
                  (long long)(ctlblk_ptr->status));
        HDfprintf(out_stream, "dump: 	ctlblk_ptr->retries = 0x%llx.\n", 
                  (long long)(ctlblk_ptr->retries));
        HDfprintf(out_stream, "dump: 	ctlblk_ptr->err_num = 0x%llx.\n", 
                  (long long)(ctlblk_ptr->err_num));
        HDfprintf(out_stream, "dump: 	ctlblk_ptr->type = 0x%llx.\n", 
                  (long long)(ctlblk_ptr->type));
        HDfprintf(out_stream, "dump: 	ctlblk_ptr->dxpl_id = 0x%llx.\n", 
                  (long long)(ctlblk_ptr->dxpl_id));
        HDfprintf(out_stream, "dump: 	ctlblk_ptr->addr = 0x%llx.\n", 
                  (long long)(ctlblk_ptr->addr));
        HDfprintf(out_stream, "dump: 	ctlblk_ptr->size = 0x%llx.\n", 
                  (long long)(ctlblk_ptr->size));
        HDfprintf(out_stream, "dump: 	ctlblk_ptr->buff = 0x%llx.\n", 
                  (long long)(ctlblk_ptr->buf));
        HDfprintf(out_stream, "dump: 	ctlblk_ptr->next_ptr = 0x%llx.\n", 
                  (long long)(ctlblk_ptr->next_ptr));
        HDfprintf(out_stream, "dump: 	ctlblk_ptr->prev_ptr = 0x%llx.\n", 
                  (long long)(ctlblk_ptr->prev_ptr));
    }

    return;

} /* H5FD_sec2_aio_dump_ctlblk */



/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_aio_read
 *
 * Purpose:	Initiate an asynchronous read from the indicated file of 
 *		the specified number of bytes starting at the specified 
 *		offset and loading the data  into the provided buffer.
 *
 *              The buffer must be large enough to contain the requested 
 *		data, and is undefined and must not be read or modified 
 *		until the read completes successfully.  Completion is 
 *		determined via either an H5FD_sec2_aio_test() or an
 *		H5FD_sec2_aio_wait() call, and success via 
 *		H5FD_sec2_aio_finish().
 *
 *		If successful, the H5FD_sec2_aio_read routine will 
 *		allocate and return a pointer to an internal control block 
 *		in *ctlblk_ptr_ptr.  This pointer must be used in all 
 *		subsequent H5FD_sec2_aio_test() / H5FD_sec2_aio_wait() / 
 *		H5FD_sec2_aio_finish() calls referring to this request.
 *
 *		Note that a successful return from this function does not 
 *		imply a successful read -- simply that no errors were 
 *		immediately evident.  In particular, the initial attempt
 *		to queue the read may fail with an EAGAIN error, 
 *		resulting in the read not even being queued until 
 *		a later call to H5FD_SEC2_aio_test(), or possibly being
 *		replaced with a synchronous read if not successfully 
 *		queued before a call to H5FD_SEC2_aio_wait().
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              5/20/10
 *
 *-------------------------------------------------------------------------
 */

#define H5FD_SEC2_AIO_READ__DEBUG 0

static herr_t 
H5FD_sec2_aio_read(H5FD_t *file, 
                   H5FD_mem_t type, 
                   hid_t dxpl,
                   haddr_t addr, 
                   size_t size, 
                   void *buffer,
                   void **ctlblk_ptr_ptr)
{
    hbool_t			success = FALSE;
    herr_t      		ret_value = SUCCEED;       /* Return value */
    herr_t                      result;
    int				posix_result;
    H5FD_sec2_t               * file_ptr = NULL;
    H5FD_sec2_aio_ctlblk_t    * ctlblk_ptr = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5FD_sec2_aio_read)

    HDassert( file != NULL );
    HDassert( H5F_addr_defined(addr) );
    HDassert( size > 0 );
    HDassert( buffer != NULL );
    HDassert( ctlblk_ptr_ptr != NULL );
    HDassert( *ctlblk_ptr_ptr == NULL );

#if ( H5FD_SEC2_AIO_READ__DEBUG > 1 ) 
    HDfprintf(stdout, "%s: entering -- addr = 0x%llx, size = 0x%llx.\n", 
              FUNC, (long long)addr, (long long)size);
#endif /* ( H5FD_SEC2_AIO_READ__DEBUG > 1 ) */

#ifndef NDEBUG
    if ( H5FD_sec2_aio_sc(file, stdout, "beginning of H5FD_sec2_aio_read()", 
                          H5FD_SEC2_AIO__SC_VERBOSE) < 0 ) {
        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "AIO data structures sanity check failure")
    }
#endif /* NDEBUG */

    /* setup the file pointer */
    file_ptr = (H5FD_sec2_t *)file;

    /* Check for overflow conditions */
    if(!H5F_addr_defined(addr))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, \
                    "addr undefined, addr = %llu", (unsigned long long)addr)
    if(REGION_OVERFLOW(addr, size))
        HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, \
                    "addr overflow, addr = %llu", (unsigned long long)addr)
    if((addr + size) > file_ptr->eoa)
        HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, \
                    "addr overflow, addr = %llu", (unsigned long long)addr)

    /* allocate the control block */

    result = H5FD_sec2_aio_alloc_ctlblk(&ctlblk_ptr);

    if ( result != SUCCEED ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "can't allocate aio control block")

    } else if ( ( ctlblk_ptr == NULL ) ||
                ( ctlblk_ptr->magic != H5FD_SEC2_AIO_CTLBLK_T__MAGIC ) ) {

        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "NULL ctlblk_ptr or bad ctlblk magic")
    }

    /* setup the control block */
    ctlblk_ptr->file_ptr = (H5FD_sec2_t *)file;
    ctlblk_ptr->op       = H5FD_SEC2_AIO_OP__READ;
    ctlblk_ptr->status   = H5FD_SEC2_AIO_STATUS__READY;
    ctlblk_ptr->retries  = -1;
    ctlblk_ptr->err_num  = 0;

    /* we will need these if we have to do the read synchronously */
    ctlblk_ptr->type     = type;
    ctlblk_ptr->dxpl_id  = dxpl;
    ctlblk_ptr->addr     = addr;
    ctlblk_ptr->size     = size;
    ctlblk_ptr->buf      = buffer;

    /* setup the posix control block */
    ctlblk_ptr->ctlblk.aio_fildes = ctlblk_ptr->file_ptr->fd;
    ctlblk_ptr->ctlblk.aio_offset = (off_t)addr;
    ctlblk_ptr->ctlblk.aio_buf    = buffer;
    ctlblk_ptr->ctlblk.aio_nbytes = size;

    /* kick off the read.
     *
     * Don't worry if the read fails with EAGAIN -- if it does, and 
     * the retry limit is greater than 0, we will try to queue it again 
     * the next time we do a test, or do a synchronous read if we 
     * haven't succeeded in queueing the read by the time we do a 
     * wait on it.
     */

    posix_result = HDaio_read(&(ctlblk_ptr->ctlblk));

    if ( posix_result != 0 ) {

        if ( errno == EAGAIN ) {

            (ctlblk_ptr->retries)++;

            if ( ctlblk_ptr->retries >= H5FD_SEC2_AIO__MAX_RETRIES ) {
     
                ctlblk_ptr->status = 
			H5FD_SEC2_AIO_STATUS__DO_USING_SIO_ON_FINISH;
            }
        } else {

#if H5FD_SEC2_AIO_READ__DEBUG
            HDfprintf(stdout,
                      "%s: HDaio_read(ctlblk) failed. errno = %d (%s)\n",
                      FUNC, errno, strerror(errno));
            HDfprintf(stdout, "%s: offset/size = %lld/%d\n",
                      FUNC, (long long)addr, (int)size);
#endif /* H5FD_SEC2_AIO_READ__DEBUG */

            HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, \
                        "call to HDaio_read() failed.")
        }
    } else {

        ctlblk_ptr->status = H5FD_SEC2_AIO_STATUS__QUEUED;
    }

    if ( file_ptr->aio_canceled_count > 0 ) {

        result = H5FD_sec2_aio_cancel__retire_canceled_in_progress(file_ptr);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                        "H5FD_sec2_aio_cancel__retire_canceled_in_progress() failed.")
        }
    }

    /* add the read to the head of the read in progress list */
    H5FD_SEC2__DLL_PREPEND(ctlblk_ptr, \
                           file_ptr->aio_reads_head, \
                           file_ptr->aio_reads_tail, \
                           file_ptr->aio_reads_count);

    /* set ctlblk_ptr_ptr */
    *ctlblk_ptr_ptr = (void *)ctlblk_ptr;

    success = TRUE;

done:

    /* discard the control block if not successful */
    if ( ( ctlblk_ptr != NULL ) && ( ! success ) ) {
 
        result = H5FD_sec2_aio_discard_ctlblk(ctlblk_ptr);

        if ( result != SUCCEED ) {

            HDONE_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "ctlblk de-allocation failed")
        }

        ctlblk_ptr = NULL;
    }

#ifndef NDEBUG
    if ( H5FD_sec2_aio_sc(file, stdout, "end of H5FD_sec2_aio_read()",
                          H5FD_SEC2_AIO__SC_VERBOSE) < 0 ) {

        HDfprintf(stdout, "%s: H5FD_sec2_aio_sc() reports failure.\n", FUNC);
        HDONE_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                   "AIO data structures sanity check failure")
    }
#endif /* NDEBUG */

#if ( H5FD_SEC2_AIO_READ__DEBUG > 1 ) 
    HDfprintf(stdout, "%s: exiting.\n", FUNC);
#endif /* ( H5FD_SEC2_AIO_READ__DEBUG > 1 ) */


    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_sec2_aio_read() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_aio_write
 *
 * Purpose:	Initiate an asynchronous write to the indicated file of 
 *		the specified number of bytes from the supplied  buffer
 *		to the indicated location.
 *
 *              The buffer must not be discarded or modified until the 
 *		write completes successfully.  Completion is determined 
 *		via either H5FD_sec2_aio_test() orH5FD_sec2_aio_wait(), 
 *		and success via H5FD_sec2_aio_finish().
 *
 *		If successful, the H5FD_sec2_aio_write routine will 
 *		allocate and return a pointer to an internal control block 
 *		in *ctlblk_ptr_ptr.  This pointer must be used in all 
 *		subsequent H5FD_sec2_aio_test() / H5FD_sec2_aio_wait() / 
 *		H5FD_sec2_aio_finish() calls referring to this request.
 *
 *		Note that a successful return from this function does not 
 *		imply a successful write -- simply that no errors were 
 *		immediately evident.
 *
 *		In particular, the initial attempt to queue the write 
 *		may fail with an EAGAIN error, resulting in the write 
 *		not being queued until a later call to H5FD_SEC2_aio_test(),
 *		or possibly being replaced with a synchronous write if 
 *		not successfully queued before a call to 
 *		H5FD_SEC2_aio_wait().
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              5/20/10
 *
 *-------------------------------------------------------------------------
 */

#define H5FD_SEC2_AIO_WRITE__DEBUG 0

static herr_t 
H5FD_sec2_aio_write(H5FD_t *file, 
                    H5FD_mem_t type, 
                    hid_t dxpl,
                    haddr_t addr, 
                    size_t size, 
                    void *buffer,
                    void **ctlblk_ptr_ptr)
{
    hbool_t			success = FALSE;
    herr_t                      result;
    herr_t                      ret_value = SUCCEED;       /* Return value */
    int				posix_result;
    H5FD_sec2_t               * file_ptr = NULL;
    H5FD_sec2_aio_ctlblk_t    * ctlblk_ptr = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5FD_sec2_aio_write)

    HDassert( file != NULL );
    HDassert( H5F_addr_defined(addr) );
    HDassert( size > 0 );
    HDassert( buffer != NULL );
    HDassert( ctlblk_ptr_ptr != NULL );
    HDassert( *ctlblk_ptr_ptr == NULL );

#ifndef NDEBUG
    if ( H5FD_sec2_aio_sc(file, stdout, "beginning of H5FD_sec2_aio_write()",
                          H5FD_SEC2_AIO__SC_VERBOSE) < 0 ) {
        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "AIO data structures sanity check failure")
    }
#endif /* NDEBUG */

#if ( H5FD_SEC2_AIO_WRITE__DEBUG > 1 ) 
    HDfprintf(stdout, "%s: addr = 0x%llx, size = 0x%llx.\n", 
              FUNC, (long long)addr, (long long)size);
#endif /* ( H5FD_SEC2_AIO_WRITE__DEBUG > 1 ) */

    /* setup the file pointer */
    file_ptr = (H5FD_sec2_t *)file;

    /* Check for overflow conditions */
    if(!H5F_addr_defined(addr))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, \
                    "addr undefined, addr = %llu", (unsigned long long)addr)
    if(REGION_OVERFLOW(addr, size))
        HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, \
                    "addr overflow, addr = %llu, size = %llu", \
                    (unsigned long long)addr, (unsigned long long)size)
    if((addr + size) > file_ptr->eoa)
        HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, \
                    "addr overflow, addr = %llu, size = %llu, eoa = %llu", \
                    (unsigned long long)addr, (unsigned long long)size, (unsigned long long)file_ptr->eoa)

    /* allocate the control block */

#ifndef NDEBUG
    if ( H5FD_sec2_aio_sc(file, stdout, "H5FD_sec2_aio_write() -- before ctlblk alloc",
                          H5FD_SEC2_AIO__SC_VERBOSE) < 0 ) {
        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "AIO data structures sanity check failure")
    }
#endif /* NDEBUG */

    result = H5FD_sec2_aio_alloc_ctlblk(&ctlblk_ptr);

    if ( result != SUCCEED ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "can't allocate aio control block")

    } else if ( ( ctlblk_ptr == NULL ) ||
                ( ctlblk_ptr->magic != H5FD_SEC2_AIO_CTLBLK_T__MAGIC ) ) {

        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "NULL ctlblk_ptr or bad ctlblk magic")
    }

#ifndef NDEBUG
    if ( H5FD_sec2_aio_sc(file, stdout, "H5FD_sec2_aio_write() -- before ctlblk init",
                          H5FD_SEC2_AIO__SC_VERBOSE) < 0 ) {
        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "AIO data structures sanity check failure")
    }
#endif /* NDEBUG */

    /* setup the control block */
    ctlblk_ptr->file_ptr = (H5FD_sec2_t *)file;
    ctlblk_ptr->op       = H5FD_SEC2_AIO_OP__WRITE;
    ctlblk_ptr->status   = H5FD_SEC2_AIO_STATUS__READY;
    ctlblk_ptr->retries  = -1;

    /* we will need these if we have to do the write synchronously */
    ctlblk_ptr->type     = type;
    ctlblk_ptr->dxpl_id  = dxpl;
    ctlblk_ptr->addr     = addr;
    ctlblk_ptr->size     = size;
    ctlblk_ptr->buf      = buffer;

    /* setup the posix control block */
    ctlblk_ptr->ctlblk.aio_fildes = ctlblk_ptr->file_ptr->fd;
    ctlblk_ptr->ctlblk.aio_offset = (off_t)addr;
    ctlblk_ptr->ctlblk.aio_buf    = buffer;
    ctlblk_ptr->ctlblk.aio_nbytes = size;

#ifndef NDEBUG
    if ( H5FD_sec2_aio_sc(file, stdout, 
                          "H5FD_sec2_aio_write() -- before call to HDaio_write()",
                          H5FD_SEC2_AIO__SC_VERBOSE) < 0 ) {
        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "AIO data structures sanity check failure")
    }
#endif /* NDEBUG */

    /* kick off the write.
     *
     * Don't worry if the write failes with EAGAIN -- if it does, and 
     * the retry limit is greater than 0, we will try to queue it again 
     * the next time we do a test, or do a synchronous write if we 
     * haven't succeeded in queueing the write by the time we do a 
     * wait on it.
     */
    posix_result = HDaio_write(&(ctlblk_ptr->ctlblk));

    if ( posix_result != 0 ) {

        if ( errno == EAGAIN ) {

            (ctlblk_ptr->retries)++;

            if ( ctlblk_ptr->retries >= H5FD_SEC2_AIO__MAX_RETRIES ) {

                ctlblk_ptr->status = 
			H5FD_SEC2_AIO_STATUS__DO_USING_SIO_ON_FINISH;
            }
        } else {

#if H5FD_SEC2_AIO_WRITE__DEBUG
            HDfprintf(stdout,
                      "%s: HDaio_write(ctlblk) failed. errno = %d (%s)\n",
                      FUNC, errno, strerror(errno));
            HDfprintf(stdout, "%s: offset/size = %lld/%d\n",
                      FUNC, (long long)addr, (int)size);
#endif /* H5FD_SEC2_AIO_WRITE__DEBUG */

            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
                        "call to HDaio_write() failed.")
        }
    } else {

        ctlblk_ptr->status = H5FD_SEC2_AIO_STATUS__QUEUED;
    }

#ifndef NDEBUG
    if ( H5FD_sec2_aio_sc(file, stdout, "H5FD_sec2_aio_write() -- before retire canceled",
                          H5FD_SEC2_AIO__SC_VERBOSE) < 0 ) {
        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "AIO data structures sanity check failure")
    }
#endif /* NDEBUG */

    /* attempt to retire canceled operations if appropriate */
    if ( file_ptr->aio_canceled_count > 0 ) {

        result = H5FD_sec2_aio_cancel__retire_canceled_in_progress(file_ptr);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                        "H5FD_sec2_aio_cancel__retire_canceled_in_progress() failed.")
        }
    }

#ifndef NDEBUG
    if ( H5FD_sec2_aio_sc(file, stdout, "H5FD_sec2_aio_write() -- before prepend",
                          H5FD_SEC2_AIO__SC_VERBOSE) < 0 ) {
        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "AIO data structures sanity check failure")
    }
#endif /* NDEBUG */

    /* add the read to the head of the writes in progress list */
    H5FD_SEC2__DLL_PREPEND(ctlblk_ptr, \
                           file_ptr->aio_writes_head, \
                           file_ptr->aio_writes_tail, \
                           file_ptr->aio_writes_count);

    /* set ctlblk_ptr_ptr */
    *ctlblk_ptr_ptr = ctlblk_ptr;

    success = TRUE;

done:

    /* discard the control block if not successful */
    if ( ( ctlblk_ptr != NULL ) && ( ! success ) ) {
 
        result = H5FD_sec2_aio_discard_ctlblk(ctlblk_ptr);

        if ( result != SUCCEED ) {

            HDONE_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "ctlblk de-allocation failed")
        }

        ctlblk_ptr = NULL;
    }

#ifndef NDEBUG
    if ( H5FD_sec2_aio_sc(file, stdout, "end of H5FD_sec2_aio_write()",
                          H5FD_SEC2_AIO__SC_VERBOSE) < 0 ) {
        HDONE_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                   "AIO data structures sanity check failure")
    }
#endif /* NDEBUG */


    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_sec2_aio_write() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_aio_test
 *
 * Purpose:	If the asynchronous operation indicated by the supplied 
 *		control block has not been queued yet (i.e. status is 
 *		H5FD_SEC2_AIO_STATUS__READY), attempt to queue it again.  
 *
 *		If the attempt fails with anything other than EAGAIN,
 *		save the errno in the control block, set status to 
 *		H5FD_SEC2_AIO_STATUS__CANT_QUEUE, set *done_ptr to TRUE,
 *		and return success.
 *
 *		If the attempt fails with EAGAIN, and the retry limit is
 *		exceeded, set status to 
 *		H5FD_SEC2_AIO_STATUS__DO_USING_SIO_ON_FINISH, set 
 *		*done_ptr to TRUE, and return success.
 *
 *		If the retry fails with EAGAIN and the retry limit is 
 *		not exceeded, increment the retry counter, set *done_ptr
 *		to FALSE, and return success.
 *
 *		If the retry succeeds, set status to 
 *		H5FD_SEC2_AIO_STATUS__QUEUED, place the control block on
 *		the linked list indicated by its op field, set *done_ptr 
 *		to FALSE, and return success.
 *
 *		If the asynchronous operation indicated by the supplied
 *		control block has been queued and is not known to be 
 *		complete (i.e. status is H5FD_SEC2_AIO_STATUS__QUEUED), 
 *		test completion status with HDaio_error().  
 *
 *		If HDaio_error() returns either 0 or any error code other 
 *		than EINPROGRESS, load the error code into the control 
 *		block's err_num field, set the control block's status 
 *		field to H5FD_SEC2_AIO_STATUS__COMPLETE, set *done_ptr 
 *		to TRUE, and return success.
 *
 *		If HDaio_error() returns EAGAIN, simply set *done_ptr to 
 *		FALSE and return success.
 *
 *		If the supplied control block is invalid, or if status
 *		is not either H5FD_SEC2_AIO_STATUS__READY or 
 *		H5FD_SEC2_AIO_STATUS__QUEUED on entry, throw an error
 *		and return a FAIL.
 *
 *		After the operation is complete, a call to 
 *		H5FD_sec2_aio_finish() must be made to determine whether 
 *		the operation completed successfully and to allow the 
 *		driver to tidy its data structures.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              5/20/10
 *
 *-------------------------------------------------------------------------
 */

#define H5FD_SEC2_AIO_TEST__DEBUG 0

static herr_t 
H5FD_sec2_aio_test(hbool_t *done_ptr, 
                   void *ctlblk_ptr)
{
    herr_t			ret_value = SUCCEED;  /* Return value */
    herr_t			result;
    int				posix_result;
    H5FD_sec2_aio_ctlblk_t    * cb_ptr = NULL;
    H5FD_sec2_t               * file_ptr = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5FD_sec2_aio_test)

    if ( ( done_ptr == NULL ) ||
         ( ctlblk_ptr == NULL ) ||
         ( ((H5FD_sec2_aio_ctlblk_t *)ctlblk_ptr)->magic != 
           H5FD_SEC2_AIO_CTLBLK_T__MAGIC ) ) {

        HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, "bad arg(s) on entry.")
    }

    cb_ptr = (H5FD_sec2_aio_ctlblk_t *)ctlblk_ptr;

#ifndef NDEBUG
    if ( H5FD_sec2_aio_sc((H5FD_t *)(cb_ptr->file_ptr), stdout, 
                          "beginning of H5FD_sec2_aio_test()",
                          H5FD_SEC2_AIO__SC_VERBOSE) < 0 ) {
        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "AIO data structures sanity check failure")
    }
#endif /* NDEBUG */

    if ( ( cb_ptr->op != H5FD_SEC2_AIO_OP__READ ) &&
         ( cb_ptr->op != H5FD_SEC2_AIO_OP__WRITE ) &&
         ( cb_ptr->op != H5FD_SEC2_AIO_OP__FSYNC ) ) {

#if H5FD_SEC2_AIO_TEST__DEBUG
        HDfprintf(stdout, "%s: cb_ptr->op = %d.\n", FUNC, (int)(cb_ptr->op));
#endif /* H5FD_SEC2_AIO_TEST__DEBUG */

        HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                    "Undefined or unknown ctlblk op on entry.")
    } 

    file_ptr = cb_ptr->file_ptr;

    switch ( cb_ptr->status )
    {
	case H5FD_SEC2_AIO_STATUS__READY:
            /* we have already attempted to queue the operation at least
             * once -- give it another try.
             */
            switch ( cb_ptr->op )
            {
		case H5FD_SEC2_AIO_OP__READ:
                    posix_result = HDaio_read(&(cb_ptr->ctlblk));
		    break;

		case H5FD_SEC2_AIO_OP__WRITE:
                    posix_result = HDaio_write(&(cb_ptr->ctlblk));
		    break;

		case H5FD_SEC2_AIO_OP__FSYNC:
                    /* not sure we want to do this with HDaio_fsync() --
                     * revisit this issue at some point.
                     */
                    posix_result = HDaio_fsync(O_SYNC, &(cb_ptr->ctlblk));
		    break;

		default:
                    /* This case should be unreachable.  It is included
                     * to keep some compilers happy.
                     */
                    HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                                "Undefined or unknown ctlblk.")
		    break;
            }

            if ( posix_result == 0 ) { /* success */

                cb_ptr->status = H5FD_SEC2_AIO_STATUS__QUEUED;

                *done_ptr = FALSE;

            } else if ( errno == EAGAIN ) {

                cb_ptr->retries += 1;

                if ( cb_ptr->retries >= H5FD_SEC2_AIO__MAX_RETRIES ) {

                    cb_ptr->status = 
			H5FD_SEC2_AIO_STATUS__DO_USING_SIO_ON_FINISH;
                    *done_ptr = FALSE;

                } else {

                    *done_ptr = FALSE;
                }
            } else {

                cb_ptr->status = H5FD_SEC2_AIO_STATUS__CANT_QUEUE;
                cb_ptr->err_num = errno;
                *done_ptr = TRUE;
            }
	    break;

	case H5FD_SEC2_AIO_STATUS__QUEUED:
            posix_result = HDaio_error(&(cb_ptr->ctlblk));

            if ( posix_result == EINPROGRESS ) {

                *done_ptr = FALSE;

            } else if ( posix_result == 0 ) { /* successful completion */

                cb_ptr->err_num = 0;
                cb_ptr->status = H5FD_SEC2_AIO_STATUS__COMPLETE;
                *done_ptr = TRUE;

            } else if ( posix_result == ECANCELED ) {

                HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                            "aio op has been canceled?!?.")

            } else if ( posix_result == EINVAL ) {

                HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                            "invalid or already completed ctlblk.")

            } else {

                /* operation failed -- save the error code... */
                cb_ptr->err_num = posix_result;
                cb_ptr->status = H5FD_SEC2_AIO_STATUS__COMPLETE;
                *done_ptr = TRUE;
            }
	    break;

	case H5FD_SEC2_AIO_STATUS__UNDEFINED:
	case H5FD_SEC2_AIO_STATUS__COMPLETE:
	case H5FD_SEC2_AIO_STATUS__CANT_QUEUE:
        case H5FD_SEC2_AIO_STATUS__DO_USING_SIO_ON_FINISH:
        case H5FD_SEC2_AIO_STATUS__CANCELED_IN_PROGRESS:
            *done_ptr = TRUE;  /* just to set it to a known value */
            HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                        "ctlblk status must be either ready or queued.")
	    break;

	default:
#if H5FD_SEC2_AIO_TEST__DEBUG
            HDfprintf(stdout, "%s: cb_ptr->status = %d.\n", FUNC, (int)(cb_ptr->status));
#endif /* H5FD_SEC2_AIO_TEST__DEBUG */
            *done_ptr = TRUE;  /* just to set it to a known value */
            HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, "unknown ctlblk status.")
	    break;
    }

    if ( file_ptr->aio_canceled_count > 0 ) {

        result = H5FD_sec2_aio_cancel__retire_canceled_in_progress(file_ptr);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                        "retirement of cancel in progress failed.")
        }
    }

done:

#ifndef NDEBUG
    if ( ( ctlblk_ptr != NULL ) && 
         ( ((H5FD_sec2_aio_ctlblk_t *)ctlblk_ptr)->magic == 
            H5FD_SEC2_AIO_CTLBLK_T__MAGIC ) ) {

        if ( H5FD_sec2_aio_sc((H5FD_t *)(cb_ptr->file_ptr), stdout, 
                              "end of H5FD_sec2_aio_test()",
                              H5FD_SEC2_AIO__SC_VERBOSE) < 0 ) {
            HDONE_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                        "AIO data structures sanity check failure")
        }
    }
#endif /* NDEBUG */

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_sec2_aio_test() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_aio_wait
 *
 * Purpose:	Wait until the asynchronous read, write, or fsync operation 
 *		indicated by *ctlblk_ptr has completed (successfully or 
 *		otherwise).
 *
 *		Note that the error code returned refers only to the 
 *		operation of waiting until read/write/fsync is
 *		complete -- Success does not imply that the read, write,
 *		or fsync operation completed successfully, only that
 *		no error was encountered while waiting for the operation 
 *		to finish.
 *
 *		After H5FD_sec2_aio_wait() returns, a call to 
 *		H5FD_sec2_aio_finish() must be made to determine whether 
 *		the operation completed successfully and to allow the 
 *		driver to tidy its data structures.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              5/20/10
 *
 *-------------------------------------------------------------------------
 */

#define H5FD_SEC2_AIO_WAIT__DEBUG 0

static herr_t 
H5FD_sec2_aio_wait(void *ctlblk_ptr)
{
    herr_t      		ret_value = SUCCEED;       /* Return value */
    herr_t			result;
    int				posix_result;
    const struct HDaiocb      * aiocb_list[2] = { NULL, NULL };
    H5FD_sec2_aio_ctlblk_t    * cb_ptr = NULL;
    H5FD_sec2_t               * file_ptr = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5FD_sec2_aio_wait)

    if ( ( ctlblk_ptr == NULL ) ||
         ( ((H5FD_sec2_aio_ctlblk_t *)ctlblk_ptr)->magic != 
           H5FD_SEC2_AIO_CTLBLK_T__MAGIC ) ) {

        HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, "bad arg(s) on entry.")
    }

    cb_ptr = (H5FD_sec2_aio_ctlblk_t *)ctlblk_ptr;

#ifndef NDEBUG
    if ( H5FD_sec2_aio_sc((H5FD_t *)(cb_ptr->file_ptr), stdout, 
                          "beginning of H5FD_sec2_aio_wait()",
                          H5FD_SEC2_AIO__SC_VERBOSE) < 0 ) {
        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "AIO data structures sanity check failure")
    }
#endif /* NDEBUG */

    if ( ( cb_ptr->op != H5FD_SEC2_AIO_OP__READ ) &&
         ( cb_ptr->op != H5FD_SEC2_AIO_OP__WRITE ) &&
         ( cb_ptr->op != H5FD_SEC2_AIO_OP__FSYNC ) ) {

#if H5FD_SEC2_AIO_WAIT__DEBUG
    HDfprintf(stdout, "%s: cb_ptr->op == 0x%llx, cb_ptr->status == 0x%llx (c).\n", 
              FUNC, cb_ptr->op, cb_ptr->status);
#endif /* H5FD_SEC2_AIO_WAIT__DEBUG */

        HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                    "Undefined or unknown ctlblk op on entry.")
    } 

    file_ptr = cb_ptr->file_ptr;

    switch ( cb_ptr->status )
    {
	case H5FD_SEC2_AIO_STATUS__READY:
            /* One or more attempts to queue the operation have failed.
             *
             * Just set status to H5FD_SEC2_AIO_STATUS__DO_USING_SIO_ON_FINISH,
             * and return.  We will perform the operation synchronously
	     * in aio_finish(). 
             */
#if ( H5FD_SEC2_AIO_WAIT__DEBUG > 1 )
            HDfprintf(stdout, 
            "%s: setting cb_ptr->status = H5FD_SEC2_AIO_STATUS__DO_USING_SIO_ON_FINISH.\n",
            FUNC);
#endif /* ( H5FD_SEC2_AIO_WAIT__DEBUG > 1 ) */
            cb_ptr->status = H5FD_SEC2_AIO_STATUS__DO_USING_SIO_ON_FINISH;
	    break;

	case H5FD_SEC2_AIO_STATUS__QUEUED:
            switch ( cb_ptr->op )
            {
		case H5FD_SEC2_AIO_OP__READ:
		case H5FD_SEC2_AIO_OP__WRITE:
		    /* for an asynchronous read or write, we can use
                     * HDaio_suspend() to wait for completion of the operation.
                     */
                    aiocb_list[0] = &(cb_ptr->ctlblk);

#ifndef NDEBUG
    if ( H5FD_sec2_aio_sc((H5FD_t *)(cb_ptr->file_ptr), stdout, 
                          "H5FD_sec2_aio_wait() -- prior to HDaio_suspend()",
                          H5FD_SEC2_AIO__SC_VERBOSE) < 0 ) {
        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "AIO data structures sanity check failure")
    }
#endif /* NDEBUG */

                    posix_result = HDaio_suspend(aiocb_list, 1, NULL);

                    if ( posix_result != 0 ) {
#if H5FD_SEC2_AIO_WAIT__DEBUG
			HDfprintf(stdout, 
                            "%s: HDaio_suspend() failed with errno = %d (%s).\n", 
                            FUNC, errno, strerror(errno));
#endif /* H5FD_SEC2_AIO_WAIT__DEBUG */
                        HGOTO_ERROR(H5E_IO, H5E_FILE, FAIL, \
                                    "HDaio_suspend() failed.");
                    }

#ifndef NDEBUG
    if ( H5FD_sec2_aio_sc((H5FD_t *)(cb_ptr->file_ptr), stdout, 
                          "H5FD_sec2_aio_wait() -- after HDaio_suspend()",
                          H5FD_SEC2_AIO__SC_VERBOSE) < 0 ) {
        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "AIO data structures sanity check failure")
    }
#endif /* NDEBUG */
                    /* at this point, we know that the operation has
                     * completed, but we don't have its error return.
                     * Need to call HDaio_error() for this.  Rather than
                     * duplicate code, fall through to the next item
                     * in the case statement.
                     */
                    /***** INTENTIONAL FALL THROUGH *****/

		case H5FD_SEC2_AIO_OP__FSYNC:
                    /* can't use HDaio_suspend() to wait for the completion
		     * of an HDaio_fsync(), so busy wait instead using 
                     * repeated calls to HDaio_error().
                     *
                     * In the case of a fall through from the above,
                     * case, we should only make one pass through the 
                     * do loop.  Add a check to verify this?
                     */
                    do
    		    {

#ifndef NDEBUG
    if ( H5FD_sec2_aio_sc((H5FD_t *)(cb_ptr->file_ptr), stdout, 
                          "H5FD_sec2_aio_wait() -- prior to HDaio_error()", 
                          H5FD_SEC2_AIO__SC_VERBOSE) < 0 ) {
        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "AIO data structures sanity check failure")
    }
#endif /* NDEBUG */
        		posix_result = HDaio_error(&(cb_ptr->ctlblk));

                        if ( posix_result == 0 ) { /* successful completion */

#if ( H5FD_SEC2_AIO_WAIT__DEBUG > 1 )
                            HDfprintf(stdout, "%s: op succeeded.\n", FUNC);
#endif /* ( H5FD_SEC2_AIO_WAIT__DEBUG > 1 ) */

                            cb_ptr->err_num = 0;

                        } else if ( posix_result == ECANCELED ) {

#if ( H5FD_SEC2_AIO_WAIT__DEBUG > 1 )
                            HDfprintf(stdout, 
                                      "%s: HDaio_error() failed with ECANCELED.\n", FUNC);
#endif /* ( H5FD_SEC2_AIO_WAIT__DEBUG > 1 ) */

                            HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                                        "aio op has been canceled?!?.")

                        } else if ( posix_result == EINVAL ) {

#if ( H5FD_SEC2_AIO_WAIT__DEBUG > 1 )
                            HDfprintf(stdout, "%s: HDaio_error() failed with EINVAL.\n", 
                                      FUNC);
#endif /* ( H5FD_SEC2_AIO_WAIT__DEBUG > 1 ) */

                            HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                                        "invalid or already completed ctlblk.")

                        } else if ( posix_result != EINPROGRESS ) {

			    /* operation failed -- save the error code... */
                            cb_ptr->err_num = posix_result;

#if ( H5FD_SEC2_AIO_WAIT__DEBUG > 1 )
                            HDfprintf(stdout, 
                                      "%s: op failed with errno = %d (%s).\n", 
                                      FUNC, posix_result, strerror(posix_result));
#endif /* ( H5FD_SEC2_AIO_WAIT__DEBUG > 1 ) */
                           
                            /* ...and exit the loop */
                            posix_result = 0;

                        }

#ifndef NDEBUG
    if ( H5FD_sec2_aio_sc((H5FD_t *)(cb_ptr->file_ptr), stdout, 
                          "H5FD_sec2_aio_wait() -- after HDaio_error()",
                          H5FD_SEC2_AIO__SC_VERBOSE) < 0 ) {
        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "AIO data structures sanity check failure")
    }
#endif /* NDEBUG */

                    } while ( posix_result != 0 );
                    cb_ptr->status = H5FD_SEC2_AIO_STATUS__COMPLETE;
		    break;

		default:
                    /* This case should be unreachable.  It is included
                     * to keep some compilers happy.
                     */
                    HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                                "Undefined or unknown aio op.")
		    break;
            }
	    break;

	case H5FD_SEC2_AIO_STATUS__UNDEFINED:
	case H5FD_SEC2_AIO_STATUS__COMPLETE:
	case H5FD_SEC2_AIO_STATUS__CANT_QUEUE:
        case H5FD_SEC2_AIO_STATUS__DO_USING_SIO_ON_FINISH:
        case H5FD_SEC2_AIO_STATUS__CANCELED_IN_PROGRESS:
            HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                        "ctlblk status must be either ready or queued.")
	    break;

	default:
            HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, "unknown ctlblk status.")
	    break;
    }

#ifndef NDEBUG
    if ( H5FD_sec2_aio_sc((H5FD_t *)(cb_ptr->file_ptr), stdout, 
                          "H5FD_sec2_aio_wait() -- prior to attempt to retire canceled",
                          H5FD_SEC2_AIO__SC_VERBOSE) < 0 ) {
        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "AIO data structures sanity check failure")
    }
#endif /* NDEBUG */

    if ( file_ptr->aio_canceled_count > 0 ) {

        result = H5FD_sec2_aio_cancel__retire_canceled_in_progress(file_ptr);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                        "H5FD_sec2_aio_cancel__retire_canceled_in_progress() failed")
        }
    }

done:

#ifndef NDEBUG
    if ( ( ctlblk_ptr != NULL ) &&
         ( ((H5FD_sec2_aio_ctlblk_t *)ctlblk_ptr)->magic == 
           H5FD_SEC2_AIO_CTLBLK_T__MAGIC ) ) {
        if ( H5FD_sec2_aio_sc((H5FD_t *)(cb_ptr->file_ptr), stdout, 
                              "end of H5FD_sec2_aio_wait()",
                              H5FD_SEC2_AIO__SC_VERBOSE) < 0 ) {
            HDONE_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                        "AIO data structures sanity check failure")
        }
    }
#endif /* NDEBUG */

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_sec2_aio_wait() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_aio_finish
 *
 * Purpose:	Determine whether the read, write, or fsync operation 
 *		indicated by *ctlblk_ptr completed successfully.  If it 
 *		did, set *errno_ptr to 0.  If if didn't, set *errno_ptr 
 *		to the appropriate error code.
 *
 *		Free the supplied control block.
 *
 *		Return SUCCEED if successful, and the appropriate error 
 *		code if not.
 *
 *		Note that the returned error code only refers to the 
 *		success or failure of the finish operation.  The caller 
 *		must examine *errno_ptr to determine if the underlying 
 *		asynchronous operation succeeded.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              5/20/10
 *
 *-------------------------------------------------------------------------
 */

#define H5FD_SEC2_AIO_FINISH__DEBUG 0

static herr_t 
H5FD_sec2_aio_finish(int *errno_ptr, 
                     void *ctlblk_ptr)
{
    herr_t                      ret_value = SUCCEED;       /* Return value */
    herr_t                      result;
    hbool_t			ctlblk_valid = FALSE;
    ssize_t			posix_result;
    haddr_t			eow;
    H5FD_sec2_aio_ctlblk_t    * cb_ptr = NULL;
    H5FD_sec2_t               * file_ptr = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5FD_sec2_aio_finish)

    if ( ( ctlblk_ptr == NULL ) ||
         ( ((H5FD_sec2_aio_ctlblk_t *)ctlblk_ptr)->magic != 
           H5FD_SEC2_AIO_CTLBLK_T__MAGIC ) ) {

        HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, "bad control block on entry.")
    }

    cb_ptr = (H5FD_sec2_aio_ctlblk_t *)ctlblk_ptr;

    ctlblk_valid = TRUE;

    if ( errno_ptr == NULL ) {

        HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, "errno_ptr NULL on entry.")
    }

    if ( ( cb_ptr->op != H5FD_SEC2_AIO_OP__READ ) &&
         ( cb_ptr->op != H5FD_SEC2_AIO_OP__WRITE ) &&
         ( cb_ptr->op != H5FD_SEC2_AIO_OP__FSYNC ) ) {

        ctlblk_valid = FALSE;
        HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                    "Undefined or unknown ctlblk op on entry.")
    } 

    file_ptr = cb_ptr->file_ptr;

#ifndef NDEBUG
    if ( H5FD_sec2_aio_sc((H5FD_t *)file_ptr, stdout, 
                          "beginning of H5FD_sec2_aio_finish()",
                          H5FD_SEC2_AIO__SC_VERBOSE) < 0 ) {
        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "AIO data structures sanity check failure")
    }
#endif /* NDEBUG */

    switch ( cb_ptr->status )
    {
	case H5FD_SEC2_AIO_STATUS__COMPLETE:
            posix_result = HDaio_return(&(cb_ptr->ctlblk));

            if ( cb_ptr->err_num == 0 ) { /* operation succeeded */

                if ( posix_result == -1 ) {

                    *errno_ptr = errno;

#if H5FD_SEC2_AIO_FINISH__DEBUG
                    HDfprintf(stdout, 
                              "%s: op successful but HDaio_return returned -1.\n",
                              FUNC);
                    HDfprintf(stdout, "%s: errno = %d (%s).\n", FUNC,
                              errno, strerror(errno));
#endif /* H5FD_SEC2_AIO_FINISH__DEBUG */

                    HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                                "op successful, but HDaio_return returned -1!?!.")
                }

                if ( (haddr_t)posix_result > cb_ptr->size ) {

                    /* must set *errno_ptr to something other than 0 --
                     * this seems reasonable, but feel free to change it.
                     */
                    *errno_ptr = EIO;

                    HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                                "HDaio_return() val > cb_ptr->size!?!")
                }

                if ( (haddr_t)posix_result != cb_ptr->size ) {

#if H5FD_SEC2_AIO_FINISH__DEBUG
                    HDfprintf(stdout, 
                              "%s: read successful, but short %lld of %lld bytes.\n",
                              FUNC, 
                              (long long)(cb_ptr->size) - (long long)posix_result, 
                              (long long)(cb_ptr->size));
#endif /* H5FD_SEC2_AIO_FINISH__DEBUG */

                    if ( cb_ptr->op == H5FD_SEC2_AIO_OP__READ ) {
                    
                        /* if a read is short, but otherwise no error,
                         * check to see if we have reached the end of file.
                         * If so, fill in the rest of the buffer with nulls.
                         */

                        if ( ( (size_t)posix_result < cb_ptr->size ) &&
                             ( (cb_ptr->addr + (haddr_t)posix_result) >= file_ptr->eof ) ) {

                            size_t remaining_buffer_size;
                            void * remaining_buffer;
#if H5FD_SEC2_AIO_FINISH__DEBUG
                            HDfprintf(stdout, "%s: Filling in read past eof with nulls.\n",
                                      FUNC);
#endif /* H5FD_SEC2_AIO_FINISH__DEBUG */

                            remaining_buffer_size = 
                                    (size_t)(cb_ptr->size) - (size_t)posix_result;

                            remaining_buffer = (void *)
			              ((uint8_t *)(cb_ptr->buf) + remaining_buffer_size);

                            HDmemset(remaining_buffer, 0, remaining_buffer_size);

                        } else {

                            /* flag an error -- although if we get serious about error
                             * recovery, it would make sense to attempt an SIO read here
                             * instead.
                             *
                             * must set *errno_ptr to something other than 0 on failure.
                             * The following seems reasonable, but feel free to change it.
                             */
#if H5FD_SEC2_AIO_FINISH__DEBUG
                            HDfprintf(stdout, "%s: Incomplete read -- flagging error.\n", 
                                      FUNC);
#endif /* H5FD_SEC2_AIO_FINISH__DEBUG */

                             *errno_ptr = EIO;

                             HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, \
                                         "couldn't complete the read.")

                        }
                    } else if ( cb_ptr->op == H5FD_SEC2_AIO_OP__WRITE ) {

                        /* must set *errno_ptr to something other than 0
                         * on failure.  The following seems reasonable,
                         * but feel free to change it.
                         */
                        *errno_ptr = EIO;

#if H5FD_SEC2_AIO_FINISH__DEBUG
                        HDfprintf(stdout,
                          "%s: write successful, but bytes written = %lld, %lld expected.\n",
                                  FUNC,
                                  (long long)posix_result,
                                  (long long)(cb_ptr->size));
#endif /* H5FD_SEC2_AIO_FINISH__DEBUG */

                         HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
                                     "couldn't complete the write.")

                    } else {

                        /* note that we set cb_ptr->size to 0 in the 
                         * case of HDaio_fsync().
                         */
                        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, 
                                    "this should be unreachable");
                    }
                } else {

                    *errno_ptr = 0;
                }

            } else { /* operation failed */

                *errno_ptr = cb_ptr->err_num;

#if H5FD_SEC2_AIO_FINISH__DEBUG
                if ( errno != cb_ptr->err_num ) {

                    HDfprintf(stdout, 
                        "%s: unexpected errno = %d (%s). %d (%s) expected.\n",
                        FUNC, errno, strerror(errno), 
                        cb_ptr->err_num, strerror(cb_ptr->err_num));
                    HDfprintf(stdout, "%s: HDaio_return() returned %lld.\n",
                              FUNC, (long long)posix_result);
                }
#endif /* H5FD_SEC2_AIO_FINISH__DEBUG */


                if ( posix_result != -1 ) {

                    HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                        "op failed, but HDaio_return reports good status!?!.")
                }

                switch ( cb_ptr->op ) {

                    case H5FD_SEC2_AIO_OP__READ:
                         HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, \
                                     "asychronous read failed.")
                        break;

                    case H5FD_SEC2_AIO_OP__WRITE:
                         HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
                                     "asychronous write failed.")
                        break;

                    case H5FD_SEC2_AIO_OP__FSYNC:
                         HGOTO_ERROR(H5E_IO, H5E_SYNCFAIL, FAIL, \
                                     "asynchronous fsync failed.")
                        break;

                    default:
                        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, 
                                    "this should be unreachable");
                        break;
                }
            }
	    break;

        case H5FD_SEC2_AIO_STATUS__DO_USING_SIO_ON_FINISH:
            switch ( cb_ptr->op ) {
		case H5FD_SEC2_AIO_OP__READ:
                    /* If successful, H5FD_sec2_read() always reads the 
                     * requested number of bytes or fails-- although it 
                     * will fill with zeros if a read beyond the end of 
                     * file is requested.
                     */
                    result = H5FD_sec2_read((H5FD_t *)(cb_ptr->file_ptr),
                                            cb_ptr->type,
                                            cb_ptr->dxpl_id,
                                            cb_ptr->addr,
                                            cb_ptr->size,
                                            cb_ptr->buf);

                    if ( result == SUCCEED ) {

                        *errno_ptr = 0;

                    } else {

                        /* picking an error code out of the air -- feel
                         * free to change it if appropriate.
                         */
                        *errno_ptr = EIO;

                         HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, \
                             "fallback call to H5FD_sec2_read() failed.")
                    }
		    break;

		case H5FD_SEC2_AIO_OP__WRITE:
                    /* If successful, H5FD_sec2_write() always writes the 
                     * requested number of bytes or fails.
                     */
                    result = H5FD_sec2_write((H5FD_t *)(cb_ptr->file_ptr),
                                             cb_ptr->type,
                                             cb_ptr->dxpl_id,
                                             cb_ptr->addr,
                                             cb_ptr->size,
                                             cb_ptr->buf);

                    if ( result == SUCCEED ) {

                        *errno_ptr = 0;

                    } else {

                        /* picking an error code out of the air -- feel
                         * free to change it if appropriate.
                         */
                        *errno_ptr = EIO;

                         HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
                             "fallback call to H5FD_sec2_write() failed.")
                    }
		    break;

		case H5FD_SEC2_AIO_OP__FSYNC:
                    posix_result = HDfsync(cb_ptr->file_ptr->fd);

                    if ( posix_result == 0 ) { /* success */

                        *errno_ptr = 0;

                    } else { /* failure */

                        *errno_ptr = errno;
                         HGOTO_ERROR(H5E_IO, H5E_SYNCFAIL, FAIL, \
                                     "fallback call to HDfsync() failed.")
                    }
		    break;

		default:
                    /* This case should be unreachable.  It is included
                     * to keep some compilers happy.
                     */
                    HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                                "Undefined or unknown op.")
		    break;
            }
            break;

	case H5FD_SEC2_AIO_STATUS__CANT_QUEUE:
            *errno_ptr = cb_ptr->err_num;

            switch ( cb_ptr->op ) {

                case H5FD_SEC2_AIO_OP__READ:
                     HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, \
                                 "asychronous read couldn't be queued.")
                    break;

                case H5FD_SEC2_AIO_OP__WRITE:
                     HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
                                 "asychronous write couldn't be queued.")
                    break;

                case H5FD_SEC2_AIO_OP__FSYNC:
                     HGOTO_ERROR(H5E_IO, H5E_SYNCFAIL, FAIL, \
                                 "asynchronous fsync couldn't be queued.")
                    break;

                default:
                    HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, 
                                "this should be unreachable");
                    break;
            }
	    break;

	case H5FD_SEC2_AIO_STATUS__UNDEFINED:
        case H5FD_SEC2_AIO_STATUS__READY:
        case H5FD_SEC2_AIO_STATUS__QUEUED:
        case H5FD_SEC2_AIO_STATUS__CANCELED_IN_PROGRESS:
            HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                        "ctlblk status must be complete or \"do using SIO\".")
	    break;

	default:
            HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                        "unknown ctlblk status.")
	    break;
    }

    /* on a successful write, update the eof if necessary */
    if ( cb_ptr->op == H5FD_SEC2_AIO_OP__WRITE ) {

        eow = cb_ptr->addr + (haddr_t)(cb_ptr->size);

        if ( file_ptr->eof < eow ) {

            file_ptr->eof = eow;
        }
    }

    if ( file_ptr->aio_canceled_count > 0 ) {

        result = H5FD_sec2_aio_cancel__retire_canceled_in_progress(file_ptr);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                        "H5FD_sec2_aio_cancel__retire_canceled_in_progress() failed.")
        }
    }

done:

    if ( ctlblk_valid ) { 

        /* must remove the control block from the appropriate op list
         * and then release it.
         */

        switch ( cb_ptr->op ) {

            case H5FD_SEC2_AIO_OP__READ:
                H5FD_SEC2__DLL_REMOVE(cb_ptr, \
                                      file_ptr->aio_reads_head, \
                                      file_ptr->aio_reads_tail, \
                                      file_ptr->aio_reads_count);
                break;

            case H5FD_SEC2_AIO_OP__WRITE:
                H5FD_SEC2__DLL_REMOVE(cb_ptr, \
                                      file_ptr->aio_writes_head, \
                                      file_ptr->aio_writes_tail, \
                                      file_ptr->aio_writes_count);
                break;

            case H5FD_SEC2_AIO_OP__FSYNC:
                H5FD_SEC2__DLL_REMOVE(cb_ptr, \
                                      file_ptr->aio_fsyncs_head, \
                                      file_ptr->aio_fsyncs_tail, \
                                      file_ptr->aio_fsyncs_count);
                break;

            default:
                /* This should be unreachable. */
                HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                            "reach default case in unlink switch statement.")
                break;
        }

        result = H5FD_sec2_aio_discard_ctlblk(cb_ptr);

        if ( result != SUCCEED ) {

            HDONE_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "ctlblk de-allocation failed")
        }
    }

#ifndef NDEBUG
    if ( ctlblk_valid ) {

        if ( H5FD_sec2_aio_sc((H5FD_t *)file_ptr, stdout, "end of H5FD_sec2_aio_finish()",
                              H5FD_SEC2_AIO__SC_VERBOSE) < 0 ) {
            HDONE_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                        "AIO data structures sanity check failure")
        }
    }
#endif /* NDEBUG */

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_sec2_aio_finish() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_aio_fsync
 *
 * Purpose:	Queue a sync of all asynchronous writes outstanding as of 
 *		the time this function is called.  Return SUCCEED if no 
 *		errors are encountered, but note that a good error return 
 *		from H5FD_sec2_aio_fsync() does not imply a successful 
 *		operation, only that no immediate errors were detected.
 *
 *		The sync is not known to be successful until reported 
 *		complete by either H5FD_sec2_aio_test or H5FD_sec2_aio_wait, 
 *		and reported successful by H5FD_sec2_aio_finish.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              5/20/10
 *
 *-------------------------------------------------------------------------
 */

static herr_t 
H5FD_sec2_aio_fsync(H5FD_t *file, 
                    void **ctlblk_ptr_ptr)
{
    hbool_t			success = FALSE;
    herr_t      		ret_value = SUCCEED;       /* Return value */
    herr_t                      result;
    int				posix_result;
    H5FD_sec2_t               * file_ptr = NULL;
    H5FD_sec2_aio_ctlblk_t    * ctlblk_ptr = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5FD_sec2_aio_fsync)

    HDassert( file != NULL );
    HDassert( ctlblk_ptr_ptr != NULL );
    HDassert( *ctlblk_ptr_ptr == NULL );

#ifndef NDEBUG
    if ( H5FD_sec2_aio_sc(file, stdout, "beginning of H5FD_sec2_aio_fsync()",
                          H5FD_SEC2_AIO__SC_VERBOSE) < 0 ) {
        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "AIO data structures sanity check failure")
    }
#endif /* NDEBUG */

    /* allocate the control block */

    result = H5FD_sec2_aio_alloc_ctlblk(&ctlblk_ptr);

    if ( result != SUCCEED ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "can't allocate aio control block")

    } else if ( ( ctlblk_ptr == NULL ) ||
                ( ctlblk_ptr->magic != H5FD_SEC2_AIO_CTLBLK_T__MAGIC ) ) {

        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "NULL ctlblk_ptr or bad ctlblk magic")
    }

    /* setup the file pointer */
    file_ptr = (H5FD_sec2_t *)file;

    /* setup the control block */
    ctlblk_ptr->file_ptr = (H5FD_sec2_t *)file;
    ctlblk_ptr->op       = H5FD_SEC2_AIO_OP__FSYNC;
    ctlblk_ptr->status   = H5FD_SEC2_AIO_STATUS__READY;
    ctlblk_ptr->retries  = -1;

    /* set ctlblk_ptr->size to zero so as to simplify processing in
     * H5FD_sec2_aio_finish().  Needless to say, HDaio_fsync() doesn't
     * get passed a size, and thus doesn't care.
     */
    ctlblk_ptr->size     = 0; 

    /* setup the posix control block */
    ctlblk_ptr->ctlblk.aio_fildes = ctlblk_ptr->file_ptr->fd;

    /* kick off the fsync.
     *
     * Don't worry if the fsync fails with EAGAIN -- if it does, and 
     * the retry limit is greater than 0, we will try to queue it again 
     * the next time we do a test, or do a synchronous fsync if we 
     * haven't succeeded in queueing the fsync by the time we do a 
     * wait on it.
     */
    posix_result = HDaio_fsync(O_SYNC, &(ctlblk_ptr->ctlblk));

    if ( posix_result != 0 ) {

        if ( errno == EAGAIN ) {

            (ctlblk_ptr->retries)++;

            if ( ctlblk_ptr->retries >= H5FD_SEC2_AIO__MAX_RETRIES ) {

                ctlblk_ptr->status =
                        H5FD_SEC2_AIO_STATUS__DO_USING_SIO_ON_FINISH;
            }
        } else {

#if H5FD_SEC2_AIO_READ__DEBUG
            HDfprintf(stdout,
                "%s: HDaio_fsync(O_SYNC, ctlblk) failed. errno = %d (%s)\n",
                FUNC, errno, strerror(errno));
#endif /* H5FD_SEC2_AIO_READ__DEBUG */

            HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, \
                        "call to HDaio_fsync() failed.")
        }
    } else {

        ctlblk_ptr->status = H5FD_SEC2_AIO_STATUS__QUEUED;
    }

    if ( file_ptr->aio_canceled_count > 0 ) {

        result = H5FD_sec2_aio_cancel__retire_canceled_in_progress(file_ptr);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                        "can't ")
        }
    }

    /* add the fsync to the head of the fsync in progress list */
    H5FD_SEC2__DLL_PREPEND(ctlblk_ptr, \
                           file_ptr->aio_fsyncs_head, \
                           file_ptr->aio_fsyncs_tail, \
                           file_ptr->aio_fsyncs_count);

    /* set ctlblk_ptr_ptr */
    *ctlblk_ptr_ptr = ctlblk_ptr;

    success = TRUE;

done:

    /* discard the control block if not successful */
    if ( ( ctlblk_ptr != NULL ) && ( ! success ) ) {
 
        result = H5FD_sec2_aio_discard_ctlblk(ctlblk_ptr);

        if ( result != SUCCEED ) {

            HDONE_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "ctlblk de-allocation failed")
        }

        ctlblk_ptr = NULL;
    }

#ifndef NDEBUG
    if ( H5FD_sec2_aio_sc(file, stdout, "end of H5FD_sec2_aio_fsync()",
                          H5FD_SEC2_AIO__SC_VERBOSE) < 0 ) {
        HDONE_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                   "AIO data structures sanity check failure")
    }
#endif /* NDEBUG */

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_sec2_aio_fsync() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_aio_cancel
 *
 * Purpose:	Attempt to cancel the asynchronous operation associated 
 *		with the control block pointed to by ctlblk_ptr.  
 *
 *		Note that this target operation may not have started, may
 *		be in progress, or may have completed.  Note however, that
 *		it is an error to call H5FD_sec2_aio_cancel() on an operation
 *		if it has already been completed by a call to 
 *		H5FD_sec2_aio_finish().
 *
 *		As part of the cancel, tidy up the operation and free the 
 *		associated control block.
 *
 *		Note however, that "cancel and forget" feature of our 
 *		implementation comes at a cost.  While we can cheaply 
 *		cancel operation that have not yet started, or that 
 *		have already completed, our management of operations that
 *		are in progress are necessarily more time consuming -- at
 *		least in the case of AIO reads and writes.  Here we must
 *		await conclusion of AIO read/write operations that we 
 *		are not able to cancel -- as if we don't, we risk the 
 *		possiblity that the buffers used in these operations will
 *		be freed before the operations complese.  This in turns 
 *		opens the possibility of reads/writes to unallocated memory
 *		and thus heap corruption.
 *
 *		In general, this is what we should do anyway, and further,
 *		at least at this writing I would expect aio cancels to be
 *		rare.  However, if either of these assertions proves untrue,
 *		we will have to revisit the API for the aio cancel operation.
 *
 *		Return SUCCEED if successful, and the appropriate error 
 *		code otherwise.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              5/20/10
 *
 *-------------------------------------------------------------------------
 */

#define H5FD_SEC2_AIO_CANCEL__DEBUG 0

static herr_t 
H5FD_sec2_aio_cancel(void *ctlblk_ptr)
{
    herr_t      		ret_value = SUCCEED;       /* Return value */
    herr_t			result;
    hbool_t			cancel_succeeded = FALSE;
    ssize_t			posix_result;
    H5FD_sec2_aio_ctlblk_t    * cb_ptr = NULL;
    H5FD_sec2_t               * file_ptr = NULL;
    const struct HDaiocb      * aiocb_list[2] = { NULL, NULL };

    FUNC_ENTER_NOAPI_NOINIT(H5FD_sec2_aio_cancel)

    if ( ( ctlblk_ptr == NULL ) ||
         ( ((H5FD_sec2_aio_ctlblk_t *)ctlblk_ptr)->magic != 
           H5FD_SEC2_AIO_CTLBLK_T__MAGIC ) ) {

        HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, "bad arg on entry.")
    }

    cb_ptr = (H5FD_sec2_aio_ctlblk_t *)ctlblk_ptr;

#ifndef NDEBUG
    if ( H5FD_sec2_aio_sc((H5FD_t *)(cb_ptr->file_ptr), stdout, 
                          "beginning of H5FD_sec2_aio_cancel()",
                          H5FD_SEC2_AIO__SC_VERBOSE) < 0 ) {
        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "AIO data structures sanity check failure")
    }
#endif /* NDEBUG */

    if ( ( cb_ptr->op != H5FD_SEC2_AIO_OP__READ ) &&
         ( cb_ptr->op != H5FD_SEC2_AIO_OP__WRITE ) &&
         ( cb_ptr->op != H5FD_SEC2_AIO_OP__FSYNC ) ) {

        HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                    "Undefined or unknown ctlblk op on entry.")
    } 

    file_ptr = cb_ptr->file_ptr;

    switch ( cb_ptr->status ) {

	case H5FD_SEC2_AIO_STATUS__READY:
	case H5FD_SEC2_AIO_STATUS__DO_USING_SIO_ON_FINISH:
            /* we have never started the operation -- just remove the 
             * control block from the appropriate list, discard it and return.
             */
            cancel_succeeded = TRUE;
	    break;

	case H5FD_SEC2_AIO_STATUS__QUEUED:
            /* the asynchronous operation has been queued -- must attempt
             * to cancel it.
             */
	    posix_result = HDaio_cancel(cb_ptr->file_ptr->fd, &(cb_ptr->ctlblk));

            switch ( posix_result ) {

                case AIO_CANCELED: /* success */
#if H5FD_SEC2_AIO_CANCEL__DEBUG
		    HDfprintf(stdout, "%s: HDaio_cancel() reports AIO_CANCELED.\n", FUNC);
#endif /* H5FD_SEC2_AIO_CANCEL__DEBUG */
                    /* verify that the cancel succeeded */
                    posix_result = HDaio_error(&(cb_ptr->ctlblk));

                    if ( posix_result != ECANCELED ) {

#if H5FD_SEC2_AIO_CANCEL__DEBUG
		    HDfprintf(stdout, 
                      "%s: HDaio_error() returned %d after successful cancel.\n",
                      FUNC, posix_result);
                    HDfprintf(stdout, "%s: errno = %d (%s).\n",
                              FUNC, errno, strerror(errno));
#endif /* H5FD_SEC2_AIO_CANCEL__DEBUG */
                        HGOTO_ERROR(H5E_IO, H5E_AIOCANCELFAIL, FAIL, \
                            "HDaio_error() didn't return ECANCELED after cancel.")
                    }

		    /* call HDaio_return() to let the OS tidy up after the
                     * operation 
                     */
                    posix_result = HDaio_return(&(cb_ptr->ctlblk));

                    if ( ( posix_result != -1 ) &&
                         ( errno != ECANCELED ) ) {

#if H5FD_SEC2_AIO_CANCEL__DEBUG
		    HDfprintf(stdout, 
                      "%s: HDaio_return() returned %d after successful cancel.\n",
                      FUNC, posix_result);
                    HDfprintf(stdout, "%s: errno = %d (%s).\n",
                              FUNC, errno, strerror(errno));
#endif /* H5FD_SEC2_AIO_CANCEL__DEBUG */
                        HGOTO_ERROR(H5E_IO, H5E_AIOCANCELFAIL, FAIL, \
                            "unexpected HDaio_return() after successful cancel.")
                    }
                    cancel_succeeded = TRUE;
                    break;

            	case AIO_NOTCANCELED:
		    /* operation in progress when HDaio_cancel() was called.
                     *
                     * this is a bit sticky, as we can't just drop the 
		     * operation on the floor.  
                     *
                     * To make things more difficult, if the operation 
                     * is a read or write, it is possible that the operation 
                     * has a dynamically allocated buffer associated with it.
                     * Thus in the case of a read or a write, we must wait 
                     * until the operation completes, call HDaio_return(), and 
                     * then discard the control block.  
                     *
                     * In the case of an fsync, no buffer can be associated 
                     * with the operation.  In this case, we can mark the 
                     * operation as canceled, place it on a canceled op 
                     * queue, and check to see if any canceled ops have 
                     * completed whenever we do anything else.
                     *
                     * Rather than duplicate code, just set the control
                     * block status to 
                     *
                     *    H5FD_SEC2_AIO_STATUS__CANCELED_IN_PROGRESS 
                     *
                     * and deal with switching queues at the end of this
                     * function.
                     */
#if H5FD_SEC2_AIO_CANCEL__DEBUG
		    HDfprintf(stdout, "%s: HDaio_cancel() reports AIO_NOTCANCELED.\n", FUNC);
#endif /* H5FD_SEC2_AIO_CANCEL__DEBUG */

                    if ( ( cb_ptr->op == H5FD_SEC2_AIO_OP__READ ) ||
                         ( cb_ptr->op == H5FD_SEC2_AIO_OP__WRITE ) ) {
#if H5FD_SEC2_AIO_CANCEL__DEBUG
                        HDfprintf(stdout, "%s: completing read or write\n", FUNC);
#endif /* H5FD_SEC2_AIO_CANCEL__DEBUG */

                        /* await the end of the read or write */
			aiocb_list[0] = &(cb_ptr->ctlblk);
                        posix_result = HDaio_suspend(aiocb_list, 1, NULL);

                        if ( posix_result != 0 ) {
#if H5FD_SEC2_AIO_CANCEL__DEBUG
                            HDfprintf(stdout,
                                "%s: HDaio_suspend() failed with errno = %d (%s).\n",
                                FUNC, errno, strerror(errno));
#endif /* H5FD_SEC2_AIO_CANCEL__DEBUG */
                            HGOTO_ERROR(H5E_IO, H5E_AIOCANCELFAIL, FAIL, \
                                        "HDaio_suspend() failed after in progress.");
                        }

                        /* call HDaio_return() to let the OS tidy up after the
                         * operation 
                         */
                        posix_result = HDaio_return(&(cb_ptr->ctlblk));

                        /* as we are trying to cancel the operation, the 
                         * return value is typically of no interest.  However, if
                         * desired for debugging purposes, display result if they 
                         * indicate failure.
                         */
                        if ( posix_result == -1 ) {

#if H5FD_SEC2_AIO_CANCEL__DEBUG
		            HDfprintf(stdout, 
                         "%s: HDaio_return() returned %d after successful HDaio_suspend().\n",
                                      FUNC, posix_result);
                            HDfprintf(stdout, "%s: errno = %d (%s).\n",
                                      FUNC, errno, strerror(errno));
#endif /* H5FD_SEC2_AIO_CANCEL__DEBUG */
                        }

                        cancel_succeeded = TRUE;

                    } else {
#if H5FD_SEC2_AIO_CANCEL__DEBUG
                        HDfprintf(stdout, "%s: marked fsync canceled in progress\n", FUNC);
#endif /* H5FD_SEC2_AIO_CANCEL__DEBUG */

                        cb_ptr->status = H5FD_SEC2_AIO_STATUS__CANCELED_IN_PROGRESS;

                    } 
                    break;

                case AIO_ALLDONE: /* operation was alread completed */

#if H5FD_SEC2_AIO_CANCEL__DEBUG
		    HDfprintf(stdout, "%s: HDaio_cancel() reports AIO_ALLDONE.\n", FUNC);
#endif /* H5FD_SEC2_AIO_CANCEL__DEBUG */

                    /* On both Solaris and BSD, we run into problem if we 
		     * use a call to HDaio_error() to verify that the aio operation
                     * is, in fact, complete.
                     *
                     * On the one hand, given that the result of calling HDaio_return
                     * on an operation that is not complete is specifically undefined,
                     * it seems prudent to make this verification.
                     *
                     * On the other hand, the ommission doesn't seem to 
                     * cause any problems on any system tested so far.
                     *
                     * Thus I've decided to ommit the call -- even though the 
                     * standard does seem to indicate that it should succeed
                     * in this case.
                     */

		    /* call HDaio_return() to let the OS tidy up after the
                     * operation 
                     */
                    posix_result = HDaio_return(&(cb_ptr->ctlblk));

                    if ( posix_result == EINVAL ) {

                        HGOTO_ERROR(H5E_IO, H5E_AIOCANCELFAIL, FAIL, \
                          "HDaio_return() failed after cancel after completion.")
                    }
                    cancel_succeeded = TRUE;
                    break;

                case -1: /* error */ 
#if H5FD_SEC2_AIO_CANCEL__DEBUG
		    HDfprintf(stdout, 
                              "%s: HDaio_cancel() failed with errno = %d(%s).\n",
                              FUNC, errno, strerror(errno));
#endif /* H5FD_SEC2_AIO_CANCEL__DEBUG */
                    HGOTO_ERROR(H5E_IO, H5E_AIOCANCELFAIL, FAIL, \
                                "call to HDaio_cancel() failed.")
                    break;

                default:
#if H5FD_SEC2_AIO_CANCEL__DEBUG
		    HDfprintf(stdout, 
                            "%s: HDaio_cancel() returned unexpected value %d.\n",
                            FUNC, posix_result);
#endif /* H5FD_SEC2_AIO_CANCEL__DEBUG */
                    HGOTO_ERROR(H5E_IO, H5E_AIOCANCELFAIL, FAIL, \
                        "call to HDaio_cancel() returned unexpected value.")
		    break;
            }
            break;

	case H5FD_SEC2_AIO_STATUS__COMPLETE:
            /* the operation is already complete -- and the user knows it.
             * thow an error.
             */
            HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                 "op already reported complete  -- hence can't be canceled.")
            break;

 	case H5FD_SEC2_AIO_STATUS__CANT_QUEUE:
            /* We tried to queue the operation, but failed with error 
             * other than EAGAIN.  Throw an error.
             */
            HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                        "op couldn't be queued -- hence it can't be canceled.")
            break;

        case H5FD_SEC2_AIO_STATUS__CANCELED_IN_PROGRESS:
            HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                        "op already canceled.")
            break;

	case H5FD_SEC2_AIO_STATUS__UNDEFINED:
	    /* should never be passed back to user -- throw an error */
            HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                        "ctlblk status undefined??.")
            break;

        default:
            HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, "unknown ctlblk status.")
            break;
    }

    if ( file_ptr->aio_canceled_count > 0 ) {

        result = H5FD_sec2_aio_cancel__retire_canceled_in_progress(file_ptr);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                "H5FD_sec2_aio_cancel__retire_canceled_in_progress() failed.")
        }
    }

done:

    if ( ( cancel_succeeded ) ||
         ( ( cb_ptr != NULL ) &&
           ( cb_ptr->status == H5FD_SEC2_AIO_STATUS__CANCELED_IN_PROGRESS ) ) ) { 

        /* must remove the control block from the appropriate op list
         * and then release it.
         */

        switch ( cb_ptr->op ) {

            case H5FD_SEC2_AIO_OP__READ:
                H5FD_SEC2__DLL_REMOVE(cb_ptr, \
                                      file_ptr->aio_reads_head, \
                                      file_ptr->aio_reads_tail, \
                                      file_ptr->aio_reads_count);
                break;

            case H5FD_SEC2_AIO_OP__WRITE:
                H5FD_SEC2__DLL_REMOVE(cb_ptr, \
                                      file_ptr->aio_writes_head, \
                                      file_ptr->aio_writes_tail, \
                                      file_ptr->aio_writes_count);
                break;

            case H5FD_SEC2_AIO_OP__FSYNC:
                H5FD_SEC2__DLL_REMOVE(cb_ptr, \
                                      file_ptr->aio_fsyncs_head, \
                                      file_ptr->aio_fsyncs_tail, \
                                      file_ptr->aio_fsyncs_count);
                break;

            default:
                HDONE_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                            "Undefined or unknown ctlblk op.")
                break;
        }

        if ( cb_ptr->status == H5FD_SEC2_AIO_STATUS__CANCELED_IN_PROGRESS ) { 

            H5FD_SEC2__DLL_APPEND(cb_ptr, \
                                  file_ptr->aio_canceled_head, \
                                  file_ptr->aio_canceled_tail, \
                                  file_ptr->aio_canceled_count);

        } else {

            result = H5FD_sec2_aio_discard_ctlblk(cb_ptr);

            if ( result != SUCCEED ) {

                HDONE_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                            "ctlblk de-allocation failed")
            }
        }
    }

#ifndef NDEBUG
    if ( file_ptr != NULL ) {

        if ( H5FD_sec2_aio_sc((H5FD_t *)(file_ptr), stdout, 
                              "end of H5FD_sec2_aio_cancel()",
                              H5FD_SEC2_AIO__SC_VERBOSE) < 0 ) {
            HDONE_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                        "AIO data structures sanity check failure")
        }
    }
#endif /* NDEBUG */

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_sec2_aio_cancel() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_aio_cancel__retire_canceled_in_progress
 *
 * Purpose:	Check the head of the canceled list for operations that 
 *		were canceled while in progress that have since completed.
 *		If any are found, call HDaio_return() on them, and discard
 *		the control blocks.
 *
 *		Return SUCCEED if no errors are detected, and FAIL 
 *		otherwise.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	John Mainzer
 *              6/2/10
 *
 *-------------------------------------------------------------------------
 */

#define H5FD_SEC2_AIO_CANCEL__RETIRE_CANCELED_IN_PROGRESS__DEBUG 0

static herr_t 
H5FD_sec2_aio_cancel__retire_canceled_in_progress(H5FD_sec2_t * file_ptr)
{
    herr_t      		ret_value = SUCCEED;       /* Return value */
    herr_t			result;
    hbool_t			done = FALSE;
    ssize_t			posix_result;
    H5FD_sec2_aio_ctlblk_t    * cb_ptr = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5FD_sec2_aio_cancel__retire_canceled_in_progress)

    if ( file_ptr == NULL ) {

        HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, "bad arg on entry.")
    }

    cb_ptr = file_ptr->aio_canceled_head;

    while ( ( cb_ptr != NULL ) && ( ! done ) ) {

        if ( cb_ptr->status != H5FD_SEC2_AIO_STATUS__CANCELED_IN_PROGRESS ) {

            HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, 
                        "non-canceled op on canceled list")
        }

        /* call HDaio_error() to see if the op has completed */
        posix_result = HDaio_error(&(cb_ptr->ctlblk));

        if ( posix_result == EINPROGRESS ) {

            done = TRUE;

        } else {

	    /* call HDaio_return() to let the OS tidy up after the
             * operation 
             */
#if H5FD_SEC2_AIO_CANCEL__RETIRE_CANCELED_IN_PROGRESS__DEBUG
	    HDfprintf(stdout, "%s: retiring head of canceled list.\n", FUNC);
#endif /* H5FD_SEC2_AIO_CANCEL__RETIRE_CANCELED_IN_PROGRESS__DEBUG */
            posix_result = HDaio_return(&(cb_ptr->ctlblk));

            if ( posix_result == EINVAL ) {

                HGOTO_ERROR(H5E_IO, H5E_AIOCANCELFAIL, FAIL, \
                       "HDaio_return() failed after cancel after completion.")
            }

            H5FD_SEC2__DLL_REMOVE(cb_ptr, \
                                  file_ptr->aio_canceled_head, \
                                  file_ptr->aio_canceled_tail, \
                                  file_ptr->aio_canceled_count);

            result = H5FD_sec2_aio_discard_ctlblk(cb_ptr);

            if ( result != SUCCEED ) {

                HDONE_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                            "ctlblk de-allocation failed")
            }

            cb_ptr = file_ptr->aio_canceled_head;
        }
    } 

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_sec2_aio_cancel__retire_canceled_in_progress() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_aio_sc
 *
 * Purpose:	Run a sanity check on the aio related data structures 
 *		maintained by this instance of the file driver.  If 
 *		any errors are detected, print a diagnostic to the indicated
 *		stream if verbose is TRUE.
 *
 * Return:	No errors detected:		SUCCEED
 *
 *		One or more errors detected:	FAIL
 *
 * Programmer:	John Mainzer
 *              3/23/11
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5FD_sec2_aio_sc(H5FD_t *file, 
	         FILE * output_stream,
                 const char * tag,
                 hbool_t verbose)
{
    hbool_t 			dump;
    int				i;
    int				fp;
    int				error_count = 0;
    H5FD_sec2_t               * file_ptr = NULL;
    H5FD_sec2_aio_ctlblk_t    * ctlblk_ptr = NULL;
    herr_t 			ret_value = SUCCEED;	/* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FD_sec2_aio_sc)

    if ( ( file == NULL ) ||
         ( output_stream == NULL ) ||
         ( tag == NULL ) ) {

        HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, "bad arg(s) on entry.")
    }

    file_ptr = (H5FD_sec2_t *)file;

    /* scan the read aio ctl blks */
    i = 0;
    ctlblk_ptr = file_ptr->aio_reads_head;

    while ( ctlblk_ptr != NULL ) {

	dump = FALSE;

        if ( ( i > 0 ) && ( ctlblk_ptr->prev_ptr == NULL ) ) {

            fp = 1;
            dump = TRUE;
        }

        if ( i >= (file_ptr->aio_reads_count) ) {

            fp = 2;
            dump = TRUE;
        }

        if ( ( ctlblk_ptr->prev_ptr != NULL ) &&
             ( ctlblk_ptr != ctlblk_ptr->prev_ptr->next_ptr ) ) {

            fp = 3;
            dump = TRUE;
        }

        if ( ( ctlblk_ptr->next_ptr != NULL ) &&
             ( ctlblk_ptr != ctlblk_ptr->next_ptr->prev_ptr ) ) {

            fp = 4;
            dump = TRUE;
        }

        if ( ! H5FD_sec2_aio_ctlblk_sc(file, ctlblk_ptr, H5FD_SEC2_AIO_OP__READ) ) {

            fp = 5;
            dump = TRUE;
        }

        if ( dump ) {

            error_count++;
            HDfprintf(output_stream, "sanity check failure in read aio ctlblk %d/%d:\n", 
                      i, fp);
	    H5FD_sec2_aio_dump_ctlblk(output_stream, ctlblk_ptr);
        }

        ctlblk_ptr = ctlblk_ptr->next_ptr;
        i++;
    }

    /* scan the write aio ctl blks */
    i = 0;
    ctlblk_ptr = file_ptr->aio_writes_head;

    while ( ctlblk_ptr != NULL ) {

	dump = FALSE;

        if ( ( i > 0 ) && ( ctlblk_ptr->prev_ptr == NULL ) ) {

            fp = 1;
            dump = TRUE;
        }

        if ( i >= (file_ptr->aio_writes_count) ) {

            fp = 2;
            dump = TRUE;
        }

        if ( ( ctlblk_ptr->prev_ptr != NULL ) &&
             ( ctlblk_ptr != ctlblk_ptr->prev_ptr->next_ptr ) ) {

            fp = 3;
            dump = TRUE;
        }

        if ( ( ctlblk_ptr->next_ptr != NULL ) &&
             ( ctlblk_ptr != ctlblk_ptr->next_ptr->prev_ptr ) ) {

            fp = 4;
            dump = TRUE;
        }

        if ( ! H5FD_sec2_aio_ctlblk_sc(file, ctlblk_ptr, H5FD_SEC2_AIO_OP__WRITE) ) {

            fp = 5;
            dump = TRUE;
        }

        if ( dump ) {

            error_count++;
            HDfprintf(output_stream, "sanity check failure in write aio ctlblk %d/%d:\n", 
                      i, fp);
	    H5FD_sec2_aio_dump_ctlblk(output_stream, ctlblk_ptr);
        }

        ctlblk_ptr = ctlblk_ptr->next_ptr;
        i++;
    }

    /* scan the fsync aio ctl blks */
    i = 0;
    ctlblk_ptr = file_ptr->aio_fsyncs_head;

    while ( ctlblk_ptr != NULL ) {

	dump = FALSE;

        if ( ( i > 0 ) && ( ctlblk_ptr->prev_ptr == NULL ) ) {

            fp = 1;
            dump = TRUE;
        }

        if ( i >= (file_ptr->aio_fsyncs_count) ) {

            fp = 2;
            dump = TRUE;
        }

        if ( ( ctlblk_ptr->prev_ptr != NULL ) &&
             ( ctlblk_ptr != ctlblk_ptr->prev_ptr->next_ptr ) ) {

            fp = 3;
            dump = TRUE;
        }

        if ( ( ctlblk_ptr->next_ptr != NULL ) &&
             ( ctlblk_ptr != ctlblk_ptr->next_ptr->prev_ptr ) ) {

            fp = 4;
            dump = TRUE;
        }

        if ( ! H5FD_sec2_aio_ctlblk_sc(file, ctlblk_ptr, H5FD_SEC2_AIO_OP__FSYNC) ) {

            fp = 5;
            dump = TRUE;
        }

        if ( dump ) {

            error_count++;

            if ( verbose ) {
            
                HDfprintf(output_stream, 
                          "sanity check failure in fsync aio ctlblk %d/%d:\n", 
                          i, fp);
	        H5FD_sec2_aio_dump_ctlblk(output_stream, ctlblk_ptr);
            }
        }

        ctlblk_ptr = ctlblk_ptr->next_ptr;
        i++;
    }


    if ( error_count > 0 ) {

        if ( verbose ) {

            HDfprintf(output_stream, "%d sanity check failures at %s\n", error_count, tag);
            HDfflush(output_stream);
        }
        HGOTO_ERROR(H5E_INTERNAL, H5E_SYSTEM, FAIL, \
                    "aio data structures sanity check failed.")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_sec2_aio_sc() */

#endif /* H5_HAVE_AIO */


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_fsync
 *
 * Purpose:	Sync the file to disk.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              7/7/10
 *
 *-------------------------------------------------------------------------
 */

static herr_t 
H5FD_sec2_fsync(H5FD_t *file, 
                hid_t UNUSED dxpl)
{
    herr_t ret_value = SUCCEED;       /* Return value */
    int    result;
    H5FD_sec2_t               * sec2_file_ptr = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5FD_sec2_fsync)

    if ( file == NULL ) {

        HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, "bad arg(s) on entry.")
    }

    sec2_file_ptr = (H5FD_sec2_t *)file;

    result = HDfsync(sec2_file_ptr->fd);

    if ( result != 0 ) {

	HGOTO_ERROR(H5E_VFL, H5E_SYNCFAIL, FAIL, "fsync request failed")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_sec2_fsync() */

