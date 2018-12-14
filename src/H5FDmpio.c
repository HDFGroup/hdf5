/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Thursday, July 29, 1999
 *
 * Purpose:	This is the MPI-2 I/O driver.
 *
 */

#include "H5FDdrvr_module.h" /* This source code file is part of the H5FD driver module */

#include "H5private.h"		/* Generic Functions			*/
#include "H5CXprivate.h"        /* API Contexts                         */
#include "H5Dprivate.h"		/* Dataset functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"		/* File access				*/
#include "H5FDprivate.h"	/* File drivers				*/
#include "H5FDmpi.h"            /* MPI-based file drivers		*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"         /* Property lists                       */
#include "H5FDmpio_topology.h"  /* Topology API                         */
#include <pthread.h>

//#define topo_timing
//#define onesidedtrace
#ifdef H5_HAVE_PARALLEL
#ifdef BGQ
#define inline
#endif

/* optypes for ADIO Requests */
#define READ_CA                26
#define WRITE_CA               27

/*******************************/
/* CCIO Typedefs and Functions */
/*******************************/

typedef struct CustomAgg_FH_Struct_Data *CustomAgg_FH_Data;
typedef long ADIO_Offset_CA;

/*
 * Declaration of i/o thread data structure (bgmpio_pthreadwc)
 */
typedef struct wcThreadFuncData_CA {
    MPI_File fh;
    int io_kind;
    char *buf;
    MPI_Offset size;
    MPI_Offset offset;
    int error_code;
    int myrank;
} ThreadFuncData;

/*
 * FSLayout determines how aggregators will be mapped to the file
 * LUSTRE -> Aggregators will be mapped to specific LUSTRE-like stripes
 * GPFS   -> Aggregators will be each assigned to a contiguous file domain
 */
enum FSLayout{LUSTRE, GPFS};

/*
 * Structure holding important info for CCIO options
 * (Must be populated at the MPI_File_open)
 */
typedef struct CustomAgg_FH_Struct_Data {
    MPI_Comm comm;
    MPI_File fh;
    int io_buf_put_amounts;
    char *io_buf;
    MPI_Win io_buf_window; /* Window over the io_buf to support one-sided aggregation */
    MPI_Win io_buf_put_amounts_window; /* Window over the io_buf_put_amounts */
    int ccio_read;
    int ccio_write;
    int cb_nodes;
    int ppn; /* Only used in topology-aware cb selection if env var is set */
    int pps; /* Only used in topology-aware cb selection if env var is set */
    enum AGGSelect topo_cb_select;
    int cb_buffer_size;
    int fs_block_count;
    int fs_block_size;
    int onesided_always_rmw;
    int onesided_no_rmw;
    int onesided_inform_rmw;
    int onesided_write_aggmethod;
    int onesided_read_aggmethod;
    int *ranklist;
    int ranklist_populated;
    /* ------- Added for Async IO ------- */
    int async_io_outer; /* Assume H5FD_mpio_ccio_osagg_write calls will only require 1 "inner" round */
    int async_io_inner; /* Assume File-domain aggregation mapping */
    char *io_buf_d; /* Duplicate for "outer" async IO */
    int io_buf_put_amounts_d; /* Duplicate for "outer" async IO */
    MPI_Win io_buf_window_d; /* Duplicate for "outer" async IO */
    MPI_Win io_buf_put_amounts_window_d; /* Duplicate for "outer" async IO */
    MPIO_Request io_Request;
    MPIO_Request io_Request_d;
    int check_req;
    int check_req_d;
    int use_dup;
    int pthread_io;
    /* ---------------------------------- */
    enum FSLayout fslayout;
} CustomAgg_FH_Struct_Data;

/*
 * This data structure holds parameters related to regulating
 * the one-sided aggregation algorithm.
 */
typedef struct FS_Block_Parms {
    int stripeSize; /* size in bytes of the "striping" unit - a size of 0 indicates to the */
                    /* onesided algorithm that we are a non-striping file system         */
    ADIO_Offset_CA segmentLen; /* size in bytes of the segment (stripeSize*number of aggs) */
                            /* up to the size of the file)                              */
    int stripesPerAgg; /* the number of stripes to be packed into an agg cb for this segment */
    int segmentIter; /* segment number for the group of stripes currently being packed into  */
                     /* the agg cb - resets to 0 for each cb flush to the file system        */
    int flushCB; /* once we have fully packed the cb on an agg this flags */
                 /* tells us to now write to the file                     */
    ADIO_Offset_CA stripedLastFileOffset; /* since we are now just calling the onesided algorithm */
                                          /* with the offset range of segment, we still need to   */
                                          /* know the actual last offset of the file.             */
    int firstStripedIOCall; /* whether this is the first call in the first segement of the  */
                            /* onesided algorithm.                                          */
    int lastStripedIOCall;  /* whether this is the last call in the last segement of the  */
                            /* onesided algorithm.                                        */
    int iWasUsedStripingAgg;        /* whether this rank was ever a used agg for this striping segement */
    int numStripesUsed;             /* the number of stripes packed into an aggregator */
    /* These 2 elements are the offset and lengths in the file corresponding to the actual stripes */
    MPI_Offset *stripeIOoffsets;
    int *stripeIOLens;
    int amountOfStripedDataExpected;        /* used to determine holes in this segment thereby requiring a rmw */
    /* These 2 elements enable ADIOI_OneSidedWriteAggregation to be called multiple times but only */
    /* perform the potientially computationally costly flattening of the source buffer just once */
    hsize_t bufTypeExtent;
    /* These three elements track the state of the source buffer advancement through multiple calls */
    /* to ADIOI_OneSidedWriteAggregation */
    ADIO_Offset_CA lastDataTypeExtent;
    int lastFlatBufIndice;
    ADIO_Offset_CA lastIndiceOffset;
} FS_Block_Parms;

/*
 * This data structure holds the access state of the source buffer for target
 * file domains within aggregators corresponding to the target data blocks.
 * The validity of the usage of this structure relies on the requirement that
 * only 1 aggregator can write to agiven file domain.
 */
typedef struct FDSourceBufferState_CA {
    ADIO_Offset_CA indiceOffset;
    hsize_t bufTypeExtent;
    ADIO_Offset_CA dataTypeExtent;
    int flatBufIndice;
    ADIO_Offset_CA sourceBufferOffset;
} FDSourceBufferState_CA;

void calc_file_domains(ADIO_Offset_CA *st_offsets, ADIO_Offset_CA *end_offsets,
    int nprocs, int nprocs_for_coll, ADIO_Offset_CA *min_st_offset_ptr,
    ADIO_Offset_CA **fd_start_ptr, ADIO_Offset_CA **fd_end_ptr,
    ADIO_Offset_CA *fd_size_ptr, ADIO_Offset_CA blksize);

void H5FD_mpio_ccio_write_one_sided(CustomAgg_FH_Data ca_data, const void *buf,
    MPI_Offset mpi_off, H5S_flatbuf_t *memFlatBuf, H5S_flatbuf_t *fileFlatBuf, int *error_code);

void H5FD_mpio_ccio_read_one_sided(CustomAgg_FH_Data ca_data, void *buf, MPI_Offset mpi_off,
    H5S_flatbuf_t *memFlatBuf, H5S_flatbuf_t *fileFlatBuf, int *error_code);

void H5FD_mpio_ccio_iterate_write(CustomAgg_FH_Data ca_data, const void *buf,
    int *fs_block_info, ADIO_Offset_CA *offset_list, ADIO_Offset_CA *len_list,
    MPI_Offset mpi_off, int contig_access_count, int currentValidDataIndex,
    ADIO_Offset_CA start_offset, ADIO_Offset_CA end_offset,
    ADIO_Offset_CA firstFileOffset, ADIO_Offset_CA lastFileOffset,
    H5S_flatbuf_t *memFlatBuf, H5S_flatbuf_t *fileFlatBuf, int myrank, int *error_code);

void H5FD_mpio_ccio_iterate_read(CustomAgg_FH_Data ca_data, void *buf,
    int *fs_block_info, ADIO_Offset_CA *offset_list, ADIO_Offset_CA *len_list,
    MPI_Offset mpi_off, int contig_access_count, int currentValidDataIndex,
    ADIO_Offset_CA start_offset, ADIO_Offset_CA end_offset,
    ADIO_Offset_CA firstFileOffset, ADIO_Offset_CA lastFileOffset,
    H5S_flatbuf_t *memFlatBuf, H5S_flatbuf_t *fileFlatBuf, int myrank, int *error_code);

void H5FD_mpio_ccio_osagg_write(CustomAgg_FH_Data ca_data,
    ADIO_Offset_CA *offset_list,
    ADIO_Offset_CA *len_list,
    int contig_access_count,
    const void *buf,
    H5S_flatbuf_t *memFlatBuf,
    int *error_code,
    ADIO_Offset_CA firstFileOffset,
    ADIO_Offset_CA lastFileOffset,
    int numNonZeroDataOffsets,
    ADIO_Offset_CA *fd_start,
    ADIO_Offset_CA* fd_end,
    int hole_found,
    FS_Block_Parms *stripe_parms);

void H5FD_mpio_ccio_osagg_read(CustomAgg_FH_Data ca_data,
    ADIO_Offset_CA *offset_list,
    ADIO_Offset_CA *len_list,
    int contig_access_count,
    const void *buf,
    H5S_flatbuf_t *flatBuf,
    int *error_code,
    ADIO_Offset_CA firstFileOffset,
    ADIO_Offset_CA lastFileOffset,
    int numNonZeroDataOffsets,
    ADIO_Offset_CA *fd_start,
    ADIO_Offset_CA* fd_end,
    FS_Block_Parms *stripe_parms,
    int do_file_read);

void H5FD_mpio_ccio_file_read(CustomAgg_FH_Data ca_data, int *error_code,
    ADIO_Offset_CA firstFileOffset, ADIO_Offset_CA lastFileOffset,
    ADIO_Offset_CA *fd_start, ADIO_Offset_CA* fd_end);

void *IO_Thread_Func(void *vptr_args);

/***********************************/
/* END CCIO Typedefs and Functions */
/***********************************/

/*
 * The driver identification number, initialized at runtime if H5_HAVE_PARALLEL
 * is defined. This allows applications to still have the H5FD_MPIO
 * "constants" in their source code.
 */
static hid_t H5FD_MPIO_g = 0;

/* Whether to allow collective I/O operations */
/* (Value can be set from environment variable also) */
hbool_t H5FD_mpi_opt_types_g = TRUE;

/*
 * The view is set to this value
 */
static char H5FD_mpi_native_g[] = "native";

/*
 * The description of a file belonging to this driver.
 * The EOF value is only used just after the file is opened in order for the
 * library to determine whether the file is empty, truncated, or okay. The MPIO
 * driver doesn't bother to keep it updated since it's an expensive operation.
 */
typedef struct H5FD_mpio_t {
    H5FD_t	pub;		/*public stuff, must be first		*/
    MPI_File	f;		/*MPIO file handle			*/
    MPI_Comm	comm;		/*communicator				*/
    MPI_Info	info;		/*file information			*/
    int         mpi_rank;       /* This process's rank                  */
    int         mpi_size;       /* Total number of processes            */
    haddr_t	eof;		/*end-of-file marker			*/
    haddr_t	eoa;		/*end-of-address marker			*/
    haddr_t	last_eoa;	/* Last known end-of-address marker	*/
    haddr_t	local_eof;	/* Local end-of-file address for each process */
    CustomAgg_FH_Struct_Data custom_agg_data;
} H5FD_mpio_t;

/* Private Prototypes */

/* Callbacks */
static herr_t H5FD_mpio_term(void);
static void *H5FD_mpio_fapl_get(H5FD_t *_file);
static void *H5FD_mpio_fapl_copy(const void *_old_fa);
static herr_t H5FD_mpio_fapl_free(void *_fa);
static H5FD_t *H5FD_mpio_open(const char *name, unsigned flags, hid_t fapl_id,
			      haddr_t maxaddr);
static herr_t H5FD_mpio_close(H5FD_t *_file);
static herr_t H5FD_mpio_query(const H5FD_t *_f1, unsigned long *flags);
static haddr_t H5FD_mpio_get_eoa(const H5FD_t *_file, H5FD_mem_t type);
static herr_t H5FD_mpio_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t addr);
static haddr_t H5FD_mpio_get_eof(const H5FD_t *_file, H5FD_mem_t type);
static herr_t  H5FD_mpio_get_handle(H5FD_t *_file, hid_t fapl, void** file_handle);
static herr_t H5FD_mpio_read(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr,
            size_t size, void *buf);
static herr_t H5FD_mpio_write(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr,
            size_t size, const void *buf);
static herr_t H5FD_mpio_custom_read(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id,
            hid_t file_space, hid_t mem_space, size_t elmt_size, haddr_t addr, void *buf);
static herr_t H5FD_mpio_custom_write(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id,
            hid_t file_space, hid_t mem_space, size_t elmt_size, haddr_t addr, const void *buf);
static herr_t H5FD_mpio_flush(H5FD_t *_file, hid_t dxpl_id, hbool_t closing);
static herr_t H5FD_mpio_truncate(H5FD_t *_file, hid_t dxpl_id, hbool_t closing);
static int H5FD_mpio_mpi_rank(const H5FD_t *_file);
static int H5FD_mpio_mpi_size(const H5FD_t *_file);
static MPI_Comm H5FD_mpio_communicator(const H5FD_t *_file);
static herr_t  H5FD_mpio_get_info(H5FD_t *_file, void** mpi_info);
static herr_t  H5FD_mpio_ccio_setup(const char *name, H5FD_mpio_t *file, MPI_File fh);
static herr_t  H5FD_mpio_ccio_cleanup(const H5FD_mpio_t *file);
static herr_t H5FD_mpio_setup_flatbuf( H5S_sel_type space_sel_type, H5S_flatbuf_t *curflatbuf,
            H5S_sel_iter_t *sel_iter, H5S_t *space_stype, size_t elmt_size, hbool_t is_regular);

/* The MPIO file driver information */
static const H5FD_class_mpi_t H5FD_mpio_g = {
    {   /* Start of superclass information */
    "mpio",					/*name			*/
    HADDR_MAX,					/*maxaddr		*/
    H5F_CLOSE_SEMI,				/* fc_degree		*/
    H5FD_mpio_term,                             /*terminate             */
    NULL,					/*sb_size		*/
    NULL,					/*sb_encode		*/
    NULL,					/*sb_decode		*/
    sizeof(H5FD_mpio_fapl_t),			/*fapl_size		*/
    H5FD_mpio_fapl_get,				/*fapl_get		*/
    H5FD_mpio_fapl_copy,			/*fapl_copy		*/
    H5FD_mpio_fapl_free, 			/*fapl_free		*/
    0,		                		/*dxpl_size		*/
    NULL,					/*dxpl_copy		*/
    NULL,					/*dxpl_free		*/
    H5FD_mpio_open,				/*open			*/
    H5FD_mpio_close,				/*close			*/
    NULL,					/*cmp			*/
    H5FD_mpio_query,		                /*query			*/
    NULL,					/*get_type_map		*/
    NULL,					/*alloc			*/
    NULL,					/*free			*/
    H5FD_mpio_get_eoa,				/*get_eoa		*/
    H5FD_mpio_set_eoa, 				/*set_eoa		*/
    H5FD_mpio_get_eof,				/*get_eof		*/
    H5FD_mpio_get_handle,                       /*get_handle            */
    H5FD_mpio_read,				/*read			*/
    H5FD_mpio_write,				/*write			*/
    H5FD_mpio_custom_read,          /*select_read           */
    H5FD_mpio_custom_write,         /*select_write          */
    H5FD_mpio_flush,				/*flush			*/
    H5FD_mpio_truncate,				/*truncate		*/
    NULL,                                       /*lock                  */
    NULL,                                       /*unlock                */
    H5FD_FLMAP_DICHOTOMY                        /*fl_map                */
    },  /* End of superclass information */
    H5FD_mpio_mpi_rank,                         /*get_rank              */
    H5FD_mpio_mpi_size,                         /*get_size              */
    H5FD_mpio_communicator,                     /*get_comm              */
    H5FD_mpio_get_info                          /*get_info              */
};

#ifdef H5FDmpio_DEBUG
/* Flags to control debug actions in H5Fmpio.
 * Meant to be indexed by characters.
 *
 * 'c' show result of MPI_Get_count after read
 * 'r' show read offset and size
 * 't' trace function entry and exit
 * 'w' show write offset and size
 */
static int H5FD_mpio_Debug[256] =
        { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 };
#endif

/*--------------------------------------------------------------------------
NAME
   H5FD__init_package -- Initialize interface-specific information
USAGE
    herr_t H5FD__init_package()
RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5FD_mpio_init currently).

--------------------------------------------------------------------------*/
static herr_t
H5FD__init_package(void)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    if(H5FD_mpio_init() < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "unable to initialize mpio VFD")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD__init_package() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_init
 *
 * Purpose:	Initialize this driver by registering the driver with the
 *		library.
 *
 * Return:	Success:	The driver ID for the mpio driver.
 *		Failure:	Negative.
 *
 * Programmer:	Robb Matzke
 *              Thursday, August 5, 1999
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5FD_mpio_init(void)
{
#ifdef H5FDmpio_DEBUG
    static int H5FD_mpio_Debug_inited = 0;
#endif /* H5FDmpio_DEBUG */
    const char *s;              /* String for environment variables */
    hid_t ret_value;        	/* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Register the MPI-IO VFD, if it isn't already */
    if(H5I_VFL != H5I_get_type(H5FD_MPIO_g))
        H5FD_MPIO_g = H5FD_register((const H5FD_class_t *)&H5FD_mpio_g, sizeof(H5FD_class_mpi_t), FALSE);

    /* Allow MPI buf-and-file-type optimizations? */
    s = HDgetenv("HDF5_MPI_OPT_TYPES");
    if(s && HDisdigit(*s))
        H5FD_mpi_opt_types_g = (hbool_t)HDstrtol(s, NULL, 0);

#ifdef H5FDmpio_DEBUG
    if(!H5FD_mpio_Debug_inited) {
        /* Retrieve MPI-IO debugging environment variable */
        s = HDgetenv("H5FD_mpio_Debug");
        if(s) {
            /* Set debug mask */
	    while(*s) {
		H5FD_mpio_Debug[(int)*s]++;
		s++;
	    } /* end while */
        } /* end if */
	H5FD_mpio_Debug_inited++;
    } /* end if */
#endif /* H5FDmpio_DEBUG */

    /* Set return value */
    ret_value = H5FD_MPIO_g;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_mpio_init() */


/*---------------------------------------------------------------------------
 * Function:	H5FD_mpio_term
 *
 * Purpose:	Shut down the VFD
 *
 * Returns:     Non-negative on success or negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Friday, Jan 30, 2004
 *
 *---------------------------------------------------------------------------
 */
static herr_t
H5FD_mpio_term(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Reset VFL ID */
    H5FD_MPIO_g=0;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD_mpio_term() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_fapl_mpio
 *
 * Purpose:	Store the user supplied MPIO communicator comm and info in
 *		the file access property list FAPL_ID which can then be used
 *		to create and/or open the file.  This function is available
 *		only in the parallel HDF5 library and is not collective.
 *
 *		comm is the MPI communicator to be used for file open as
 *		defined in MPI_FILE_OPEN of MPI-2. This function makes a
 *		duplicate of comm. Any modification to comm after this function
 *		call returns has no effect on the access property list.
 *
 *		info is the MPI Info object to be used for file open as
 *		defined in MPI_FILE_OPEN of MPI-2. This function makes a
 *		duplicate of info. Any modification to info after this
 *		function call returns has no effect on the access property
 *		list.
 *
 *              If fapl_id has previously set comm and info values, they
 *              will be replaced and the old communicator and Info object
 *              are freed.
 *
 * Return:	Success:	Non-negative
 *
 * 		Failure:	Negative
 *
 * Programmer:	Albert Cheng
 *		Feb 3, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_mpio(hid_t fapl_id, MPI_Comm comm, MPI_Info info)
{
    H5FD_mpio_fapl_t	fa;
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "iMcMi", fapl_id, comm, info);

    if(fapl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a file access list")
    if(MPI_COMM_NULL == comm)
	HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a valid communicator")

    /* Initialize driver specific properties */
    fa.comm = comm;
    fa.info = info;

    /* duplication is done during driver setting. */
    ret_value = H5P_set_driver(plist, H5FD_MPIO, &fa);

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Pset_fapl_mpio() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_fapl_mpio
 *
 * Purpose:	If the file access property list is set to the H5FD_MPIO
 *		driver then this function returns duplicates of the MPI
 *		communicator and Info object stored through the comm and
 *		info pointers.  It is the responsibility of the application
 *		to free the returned communicator and Info object.
 *
 * Return:	Success:	Non-negative with the communicator and
 *				Info object returned through the comm and
 *				info arguments if non-null. Since they are
 *				duplicates of the stored objects, future
 *				modifications to the access property list do
 *				not affect them and it is the responsibility
 *				of the application to free them.
 *
 * 		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Thursday, February 26, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_fapl_mpio(hid_t fapl_id, MPI_Comm *comm/*out*/, MPI_Info *info/*out*/)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    const H5FD_mpio_fapl_t *fa;       /* MPIO fapl info */
    MPI_Comm	comm_tmp = MPI_COMM_NULL;
    hbool_t     comm_copied = FALSE;    /* MPI Comm has been duplicated */
    int		mpi_code;		/* MPI return code */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "ixx", fapl_id, comm, info);

    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a file access list")
    if(H5FD_MPIO != H5P_peek_driver(plist))
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "incorrect VFL driver")
    if(NULL == (fa = (const H5FD_mpio_fapl_t *)H5P_peek_driver_info(plist)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "bad VFL driver info")

    /* Store the duplicated communicator in a temporary variable for error */
    /* recovery in case the INFO duplication fails. */
    if(comm) {
	if(MPI_SUCCESS != (mpi_code = MPI_Comm_dup(fa->comm, &comm_tmp)))
	    HMPI_GOTO_ERROR(FAIL, "MPI_Comm_dup failed", mpi_code)
        comm_copied = TRUE;
    } /* end if */

    if(info) {
	if(MPI_INFO_NULL != fa->info) {
	    if(MPI_SUCCESS != (mpi_code = MPI_Info_dup(fa->info, info)))
		HMPI_GOTO_ERROR(FAIL, "MPI_Info_dup failed", mpi_code)
	} /* end if */
        else
	    /* do not dup it */
	    *info = MPI_INFO_NULL;
    } /* end if */

    /* Store the copied communicator, now that the Info object has been
     *  successfully copied.
     */
    if(comm)
        *comm = comm_tmp;

done:
    if(ret_value < 0)
	/* need to free anything created here */
	if(comm_copied)
	    MPI_Comm_free(&comm_tmp);

    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_fapl_mpio() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_dxpl_mpio
 *
 * Purpose:	Set the data transfer property list DXPL_ID to use transfer
 *		mode XFER_MODE. The property list can then be used to control
 *		the I/O transfer mode during data I/O operations. The valid
 *		transfer modes are:
 *
 * 		H5FD_MPIO_INDEPENDENT:
 *			Use independent I/O access (the default).
 *
 * 		H5FD_MPIO_COLLECTIVE:
 *			Use collective I/O access.
 *
 * Return:	Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:	Albert Cheng
 *		April 2, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_dxpl_mpio(hid_t dxpl_id, H5FD_mpio_xfer_t xfer_mode)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iDt", dxpl_id, xfer_mode);

    if(dxpl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")
    if(H5FD_MPIO_INDEPENDENT != xfer_mode && H5FD_MPIO_COLLECTIVE != xfer_mode)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "incorrect xfer_mode")

    /* Set the transfer mode */
    if(H5P_set(plist, H5D_XFER_IO_XFER_MODE_NAME, &xfer_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_dxpl_mpio() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_dxpl_mpio
 *
 * Purpose:	Queries the transfer mode current set in the data transfer
 *		property list DXPL_ID. This is not collective.
 *
 * Return:	Success:	Non-negative, with the transfer mode returned
 *				through the XFER_MODE argument if it is
 *				non-null.
 *
 * 		Failure:	Negative
 *
 * Programmer:	Albert Cheng
 *		April 2, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_dxpl_mpio(hid_t dxpl_id, H5FD_mpio_xfer_t *xfer_mode/*out*/)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ix", dxpl_id, xfer_mode);

    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Get the transfer mode */
    if(xfer_mode)
        if(H5P_get(plist, H5D_XFER_IO_XFER_MODE_NAME, xfer_mode) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_dxpl_mpio() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_dxpl_mpio_collective_opt
 *
 * Purpose:	To set a flag to choose linked chunk I/O or multi-chunk I/O
 *		without involving decision-making inside HDF5
 *
 * Note:	The library will do linked chunk I/O or multi-chunk I/O without
 *		involving communications for decision-making process.
 *		The library won't behave as it asks for only when we find
 *		that the low-level MPI-IO package doesn't support this.
 *
 * Return:	Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:	Kent Yang
 *		? ?, ?
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_dxpl_mpio_collective_opt(hid_t dxpl_id, H5FD_mpio_collective_opt_t opt_mode)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iDc", dxpl_id, opt_mode);

    if(dxpl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Set the transfer mode */
    if(H5P_set(plist, H5D_XFER_MPIO_COLLECTIVE_OPT_NAME, &opt_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_dxpl_mpio_collective_opt() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_dxpl_mpio_chunk_opt
 *
 * Purpose:	To set a flag to choose linked chunk I/O or multi-chunk I/O
 *		without involving decision-making inside HDF5
 *
 * Note:	The library will do linked chunk I/O or multi-chunk I/O without
 *		involving communications for decision-making process.
 *		The library won't behave as it asks for only when we find
 *		that the low-level MPI-IO package doesn't support this.
 *
 * Return:	Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:	Kent Yang
 *		? ?, ?
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_dxpl_mpio_chunk_opt(hid_t dxpl_id, H5FD_mpio_chunk_opt_t opt_mode)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iDh", dxpl_id, opt_mode);

    if(dxpl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Set the transfer mode */
    if(H5P_set(plist, H5D_XFER_MPIO_CHUNK_OPT_HARD_NAME, &opt_mode) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_dxpl_mpio_chunk_opt() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_dxpl_mpio_chunk_opt_num
 *
 * Purpose:	To set a threshold for doing linked chunk IO
 *
 * Note:	If the number is greater than the threshold set by the user,
 *		the library will do linked chunk I/O; otherwise, I/O will be
 *		done for every chunk.
 *
 * Return:	Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:	Kent Yang
 *		? ?, ?
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_dxpl_mpio_chunk_opt_num(hid_t dxpl_id, unsigned num_chunk_per_proc)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iIu", dxpl_id, num_chunk_per_proc);

    if(dxpl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Set the transfer mode */
    if(H5P_set(plist, H5D_XFER_MPIO_CHUNK_OPT_NUM_NAME, &num_chunk_per_proc) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_dxpl_mpio_chunk_opt_num() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_dxpl_mpio_chunk_opt_ratio
 *
 * Purpose:	To set a threshold for doing collective I/O for each chunk
 *
 * Note:	The library will calculate the percentage of the number of
 *		process holding selections at each chunk. If that percentage
 *		of number of process in the individual chunk is greater than
 *		the threshold set by the user, the library will do collective
 *		chunk I/O for this chunk; otherwise, independent I/O will be
 *		done for this chunk.
 *
 * Return:	Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:	Kent Yang
 *		? ?, ?
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_dxpl_mpio_chunk_opt_ratio(hid_t dxpl_id, unsigned percent_num_proc_per_chunk)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iIu", dxpl_id, percent_num_proc_per_chunk);

    if(dxpl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Set the transfer mode */
    if(H5P_set(plist, H5D_XFER_MPIO_CHUNK_OPT_RATIO_NAME, &percent_num_proc_per_chunk) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_dxpl_mpio_chunk_opt_ratio() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_fapl_get
 *
 * Purpose:	Returns a file access property list which could be used to
 *		create another file the same as this one.
 *
 * Return:	Success:	Ptr to new file access property list with all
 *				fields copied from the file pointer.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Friday, August 13, 1999
 *
 *-------------------------------------------------------------------------
 */
static void *
H5FD_mpio_fapl_get(H5FD_t *_file)
{
    H5FD_mpio_t		*file = (H5FD_mpio_t*)_file;
    H5FD_mpio_fapl_t	*fa = NULL;
    void      *ret_value;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);
    HDassert(H5FD_MPIO == file->pub.driver_id);

    if(NULL == (fa = (H5FD_mpio_fapl_t *)H5MM_calloc(sizeof(H5FD_mpio_fapl_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Duplicate communicator and Info object. */
    if(FAIL == H5FD_mpi_comm_info_dup(file->comm, file->info, &fa->comm, &fa->info))
	HGOTO_ERROR(H5E_INTERNAL, H5E_CANTCOPY, NULL, "Communicator/Info duplicate failed")

    /* Set return value */
    ret_value = fa;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_fapl_copy
 *
 * Purpose:	Copies the mpio-specific file access properties.
 *
 * Return:	Success:	Ptr to a new property list
 *
 *		Failure:	NULL
 *
 * Programmer:	Albert Cheng
 *              Jan  8, 2003
 *
 *-------------------------------------------------------------------------
 */
static void *
H5FD_mpio_fapl_copy(const void *_old_fa)
{
    void		*ret_value = NULL;
    const H5FD_mpio_fapl_t *old_fa = (const H5FD_mpio_fapl_t*)_old_fa;
    H5FD_mpio_fapl_t	*new_fa = NULL;

    FUNC_ENTER_NOAPI_NOINIT
#ifdef H5FDmpio_DEBUG
if (H5FD_mpio_Debug[(int)'t'])
fprintf(stderr, "enter H5FD_mpio_fapl_copy\n");
#endif

    if(NULL == (new_fa = (H5FD_mpio_fapl_t *)H5MM_malloc(sizeof(H5FD_mpio_fapl_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Copy the general information */
    HDmemcpy(new_fa, old_fa, sizeof(H5FD_mpio_fapl_t));

    /* Duplicate communicator and Info object. */
    if(FAIL == H5FD_mpi_comm_info_dup(old_fa->comm, old_fa->info, &new_fa->comm, &new_fa->info))
	HGOTO_ERROR(H5E_INTERNAL, H5E_CANTCOPY, NULL, "Communicator/Info duplicate failed")
    ret_value = new_fa;

done:
    if (NULL == ret_value){
	/* cleanup */
	if (new_fa)
	    H5MM_xfree(new_fa);
    }

#ifdef H5FDmpio_DEBUG
if (H5FD_mpio_Debug[(int)'t'])
fprintf(stderr, "leaving H5FD_mpio_fapl_copy\n");
#endif
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_mpio_fapl_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_fapl_free
 *
 * Purpose:	Frees the mpio-specific file access properties.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Albert Cheng
 *              Jan  8, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpio_fapl_free(void *_fa)
{
    herr_t		ret_value = SUCCEED;
    H5FD_mpio_fapl_t	*fa = (H5FD_mpio_fapl_t*)_fa;

    FUNC_ENTER_NOAPI_NOINIT_NOERR
#ifdef H5FDmpio_DEBUG
if (H5FD_mpio_Debug[(int)'t'])
fprintf(stderr, "in H5FD_mpio_fapl_free\n");
#endif
    HDassert(fa);

    /* Free the internal communicator and INFO object */
    HDassert(MPI_COMM_NULL!=fa->comm);
    H5FD_mpi_comm_info_free(&fa->comm, &fa->info);
    H5MM_xfree(fa);

#ifdef H5FDmpio_DEBUG
if (H5FD_mpio_Debug[(int)'t'])
fprintf(stderr, "leaving H5FD_mpio_fapl_free\n");
#endif
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_mpio_fapl_free() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_set_mpio_atomicity
 *
 * Purpose:	Sets the atomicity mode
 *
 * Return:	Success:	Non-negative
 *
 * 		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *		Feb 14, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_set_mpio_atomicity(H5FD_t *_file, hbool_t flag)
{
    H5FD_mpio_t *file = (H5FD_mpio_t*)_file;
    int          mpi_code;               /* MPI return code */
    int          temp_flag;
    herr_t       ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Entering H5FD_set_mpio_atomicity\n");
#endif

    if (FALSE == flag)
        temp_flag = 0;
    else
        temp_flag = 1;

    /* set atomicity value */
    if (MPI_SUCCESS != (mpi_code=MPI_File_set_atomicity(file->f, temp_flag)))
        HMPI_GOTO_ERROR(FAIL, "MPI_File_set_atomicity", mpi_code)

done:
#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Leaving H5FD_set_mpio_atomicity\n");
#endif
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_get_mpio_atomicity
 *
 * Purpose:	Returns the atomicity mode
 *
 * Return:	Success:	Non-negative
 *
 * 		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *		Feb 14, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_get_mpio_atomicity(H5FD_t *_file, hbool_t *flag)
{
    H5FD_mpio_t *file = (H5FD_mpio_t*)_file;
    int          mpi_code;               /* MPI return code */
    int          temp_flag;
    herr_t       ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Entering H5FD_get_mpio_atomicity\n");
#endif

    /* get atomicity value */
    if (MPI_SUCCESS != (mpi_code=MPI_File_get_atomicity(file->f, &temp_flag)))
        HMPI_GOTO_ERROR(FAIL, "MPI_File_get_atomicity", mpi_code)

    if (0 != temp_flag)
        *flag = TRUE;
    else
        *flag = FALSE;

done:
#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Leaving H5FD_get_mpio_atomicity\n");
#endif
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpio_open
 *
 * Purpose:     Opens a file with name NAME.  The FLAGS are a bit field with
 *		purpose similar to the second argument of open(2) and which
 *		are defined in H5Fpublic.h. The file access property list
 *		FAPL_ID contains the properties driver properties and MAXADDR
 *		is the largest address which this file will be expected to
 *		access.  This is collective.
 *
 * Return:      Success:        A new file pointer.
 *
 *              Failure:        NULL
 *
 * Programmer:
 *              January 30, 1998
 *
 *-------------------------------------------------------------------------
 */
static H5FD_t *
H5FD_mpio_open(const char *name, unsigned flags, hid_t fapl_id,
	       haddr_t H5_ATTR_UNUSED maxaddr)
{
    H5FD_mpio_t			*file=NULL;
    MPI_File			fh;
    unsigned                    file_opened=0;  /* Flag to indicate that the file was successfully opened */
    int				mpi_amode;
    int				mpi_rank;       /* MPI rank of this process */
    int				mpi_size;       /* Total number of MPI processes */
    int				mpi_code;	/* mpi return code */
    MPI_Offset			size;
    const H5FD_mpio_fapl_t	*fa = NULL;
    H5FD_mpio_fapl_t		_fa;
    H5P_genplist_t *plist;      /* Property list pointer */
    MPI_Comm                    comm_dup = MPI_COMM_NULL;
    MPI_Info                    info_dup = MPI_INFO_NULL;
    H5FD_t			*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t']) {
    	fprintf(stdout, "Entering H5FD_mpio_open(name=\"%s\", flags=0x%x, "
		"fapl_id=%d, maxaddr=%lu)\n", name, flags, (int)fapl_id, (unsigned long)maxaddr);
    }
#endif

    /* Obtain a pointer to mpio-specific file access properties */
    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
    if(H5P_FILE_ACCESS_DEFAULT == fapl_id || H5FD_MPIO != H5P_peek_driver(plist)) {
	_fa.comm = MPI_COMM_SELF; /*default*/
	_fa.info = MPI_INFO_NULL; /*default*/
	fa = &_fa;
    } /* end if */
    else {
        if(NULL == (fa = (const H5FD_mpio_fapl_t *)H5P_peek_driver_info(plist)))
	    HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, NULL, "bad VFL driver info")
    } /* end else */

    /* Duplicate communicator and Info object for use by this file. */
    if(FAIL == H5FD_mpi_comm_info_dup(fa->comm, fa->info, &comm_dup, &info_dup))
	HGOTO_ERROR(H5E_INTERNAL, H5E_CANTCOPY, NULL, "Communicator/Info duplicate failed")

    /* convert HDF5 flags to MPI-IO flags */
    /* some combinations are illegal; let MPI-IO figure it out */
    mpi_amode  = (flags & H5F_ACC_RDWR) ? MPI_MODE_RDWR : MPI_MODE_RDONLY;
    if(flags & H5F_ACC_CREAT)
        mpi_amode |= MPI_MODE_CREATE;
    if(flags & H5F_ACC_EXCL)
        mpi_amode |= MPI_MODE_EXCL;

#ifdef H5FDmpio_DEBUG
    /* Check for debug commands in the info parameter */
    {
        if(MPI_INFO_NULL != info_dup) {
            char debug_str[128];
            int flag;

            MPI_Info_get(fa->info, H5F_MPIO_DEBUG_KEY, sizeof(debug_str) - 1, debug_str, &flag);
            if(flag) {
                int i;

                fprintf(stdout, "H5FD_mpio debug flags = '%s'\n", debug_str);
                for(i = 0; debug_str[i]/*end of string*/ && i < 128/*just in case*/; ++i)
                    H5FD_mpio_Debug[(int)debug_str[i]] = 1;
            }
        }
    }
#endif

    if(MPI_SUCCESS != (mpi_code = MPI_File_open(comm_dup, name, mpi_amode, info_dup, &fh)))
        HMPI_GOTO_ERROR(NULL, "MPI_File_open failed", mpi_code)
    file_opened=1;

    /* Get the MPI rank of this process and the total number of processes */
    if (MPI_SUCCESS != (mpi_code=MPI_Comm_rank (comm_dup, &mpi_rank)))
        HMPI_GOTO_ERROR(NULL, "MPI_Comm_rank failed", mpi_code)
    if (MPI_SUCCESS != (mpi_code=MPI_Comm_size (comm_dup, &mpi_size)))
        HMPI_GOTO_ERROR(NULL, "MPI_Comm_size failed", mpi_code)

    /* Build the return value and initialize it */
    if(NULL == (file = (H5FD_mpio_t *)H5MM_calloc(sizeof(H5FD_mpio_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    file->f = fh;
    file->comm = comm_dup;
    file->info = info_dup;
    file->mpi_rank = mpi_rank;
    file->mpi_size = mpi_size;

    /* Setup structures for ccio optimizations            */
    /* (Optimizations used for select_<write,read> calls) */
    H5FD_mpio_ccio_setup(name, file, fh);

    /* Only processor p0 will get the filesize and broadcast it. */
    if (mpi_rank == 0) {
        if (MPI_SUCCESS != (mpi_code=MPI_File_get_size(fh, &size)))
            HMPI_GOTO_ERROR(NULL, "MPI_File_get_size failed", mpi_code)
    } /* end if */

    /* Broadcast file size */
    if(MPI_SUCCESS != (mpi_code = MPI_Bcast(&size, (int)sizeof(MPI_Offset), MPI_BYTE, 0, comm_dup)))
        HMPI_GOTO_ERROR(NULL, "MPI_Bcast failed", mpi_code)

    /* Determine if the file should be truncated */
    if(size && (flags & H5F_ACC_TRUNC)) {
        if (MPI_SUCCESS != (mpi_code=MPI_File_set_size(fh, (MPI_Offset)0)))
            HMPI_GOTO_ERROR(NULL, "MPI_File_set_size failed", mpi_code)

        /* Don't let any proc return until all have truncated the file. */
        if (MPI_SUCCESS!= (mpi_code=MPI_Barrier(comm_dup)))
            HMPI_GOTO_ERROR(NULL, "MPI_Barrier failed", mpi_code)

        /* File is zero size now */
        size = 0;
    } /* end if */

    /* Set the size of the file (from library's perspective) */
    file->eof = H5FD_mpi_MPIOff_to_haddr(size);
    file->local_eof = file->eof;

    /* Set return value */
    ret_value=(H5FD_t*)file;

done:
    if(ret_value==NULL) {
        if(file_opened)
            MPI_File_close(&fh);
	if (MPI_COMM_NULL != comm_dup)
	    MPI_Comm_free(&comm_dup);
	if (MPI_INFO_NULL != info_dup)
	    MPI_Info_free(&info_dup);
	if (file)
	    H5MM_xfree(file);
    } /* end if */

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
        fprintf(stdout, "Leaving H5FD_mpio_open\n" );
#endif
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_mpio_open() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpio_close
 *
 * Purpose:     Closes a file.  This is collective.
 *
 * Return:      Success:	Non-negative
 *
 * 		Failure:	Negative
 *
 * Programmer:  Unknown
 *              January 30, 1998
 *
 * Modifications:
 * 		Robb Matzke, 1998-02-18
 *		Added the ACCESS_PARMS argument.
 *
 * 		Robb Matzke, 1999-08-06
 *		Modified to work with the virtual file layer.
 *
 * 		Albert Cheng, 2003-04-17
 *		Free the communicator stored.
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpio_close(H5FD_t *_file)
{
    H5FD_mpio_t	*file = (H5FD_mpio_t*)_file;
    int		mpi_code;	        /* MPI return code */
    herr_t      ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Entering H5FD_mpio_close\n");
#endif
    HDassert(file);
    HDassert(H5FD_MPIO==file->pub.driver_id);

    /* MPI_File_close sets argument to MPI_FILE_NULL */
    if (MPI_SUCCESS != (mpi_code=MPI_File_close(&(file->f)/*in,out*/)))
        HMPI_GOTO_ERROR(FAIL, "MPI_File_close failed", mpi_code)

    /* Clean structures used for ccio optimizations */
    H5FD_mpio_ccio_cleanup( file );

    /* Clean up other stuff */
    H5FD_mpi_comm_info_free(&file->comm, &file->info);
    H5MM_xfree(file);

done:
#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Leaving H5FD_mpio_close\n");
#endif
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_query
 *
 * Purpose:	Set the flags that this VFL driver is capable of supporting.
 *              (listed in H5FDpublic.h)
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Friday, August 25, 2000
 *
 * Modifications:
 *
 *		John Mainzer -- 9/21/05
 *		Modified code to turn off the
 *		H5FD_FEAT_ACCUMULATE_METADATA_WRITE flag.
 *              With the movement of
 *		all cache writes to process 0, this flag has become
 *		problematic in PHDF5.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpio_query(const H5FD_t H5_ATTR_UNUSED *_file, unsigned long *flags /* out */)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Set the VFL feature flags that this driver supports */
    if(flags) {
        *flags=0;
        *flags |= H5FD_FEAT_AGGREGATE_METADATA;     /* OK to aggregate metadata allocations                             */
        *flags |= H5FD_FEAT_AGGREGATE_SMALLDATA;    /* OK to aggregate "small" raw data allocations                     */
        *flags |= H5FD_FEAT_HAS_MPI;                /* This driver uses MPI                                             */
        *flags |= H5FD_FEAT_ALLOCATE_EARLY;         /* Allocate space early instead of late                             */
        *flags |= H5FD_FEAT_DEFAULT_VFD_COMPATIBLE; /* VFD creates a file which can be opened with the default VFD      */
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_get_eoa
 *
 * Purpose:	Gets the end-of-address marker for the file. The EOA marker
 *		is the first address past the last byte allocated in the
 *		format address space.
 *
 * Return:	Success:	The end-of-address marker.
 *
 *		Failure:	HADDR_UNDEF
 *
 * Programmer:	Robb Matzke
 *              Friday, August  6, 1999
 *
 * Modifications:
 *              Raymond Lu
 *              21 Dec. 2006
 *              Added the parameter TYPE.  It's only used for MULTI driver.
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_mpio_get_eoa(const H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type)
{
    const H5FD_mpio_t	*file = (const H5FD_mpio_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(file);
    HDassert(H5FD_MPIO==file->pub.driver_id);

    FUNC_LEAVE_NOAPI(file->eoa)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_set_eoa
 *
 * Purpose:	Set the end-of-address marker for the file. This function is
 *		called shortly after an existing HDF5 file is opened in order
 *		to tell the driver where the end of the HDF5 data is located.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Friday, August 6, 1999
 *
 * Modifications:
 *              Raymond Lu
 *              21 Dec. 2006
 *              Added the parameter TYPE.  It's only used for MULTI driver.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpio_set_eoa(H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type, haddr_t addr)
{
    H5FD_mpio_t	*file = (H5FD_mpio_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(file);
    HDassert(H5FD_MPIO==file->pub.driver_id);

    file->eoa = addr;

    FUNC_LEAVE_NOAPI(SUCCEED)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_get_eof
 *
 * Purpose:	Gets the end-of-file marker for the file. The EOF marker
 *		is the real size of the file.
 *
 *		The MPIO driver doesn't bother keeping this field updated
 *		since that's a relatively expensive operation. Fortunately
 *		the library only needs the EOF just after the file is opened
 *		in order to determine whether the file is empty, truncated,
 *		or okay.  Therefore, any MPIO I/O function will set its value
 *		to HADDR_UNDEF which is the error return value of this
 *		function.
 *
 *              Keeping the EOF updated (during write calls) is expensive
 *              because any process may extend the physical end of the
 *              file. -QAK
 *
 * Return:	Success:	The end-of-address marker.
 *
 *		Failure:	HADDR_UNDEF
 *
 * Programmer:	Robb Matzke
 *              Friday, August  6, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_mpio_get_eof(const H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type)
{
    const H5FD_mpio_t	*file = (const H5FD_mpio_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(file);
    HDassert(H5FD_MPIO==file->pub.driver_id);

    FUNC_LEAVE_NOAPI(file->eof)
}


/*-------------------------------------------------------------------------
 * Function:       H5FD_mpio_get_handle
 *
 * Purpose:        Returns the file handle of MPIO file driver.
 *
 * Returns:        Non-negative if succeed or negative if fails.
 *
 * Programmer:     Raymond Lu
 *                 Sept. 16, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
*/
static herr_t
H5FD_mpio_get_handle(H5FD_t *_file, hid_t H5_ATTR_UNUSED fapl, void** file_handle)
{
    H5FD_mpio_t         *file = (H5FD_mpio_t *)_file;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(!file_handle)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file handle not valid")

    *file_handle = &(file->f);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:       H5FD_mpio_get_info
 *
 * Purpose:        Returns the file info of MPIO file driver.
 *
 * Returns:        Non-negative if succeed or negative if fails.
 *
 * Programmer:     John Mainzer
 *                 April 4, 2017
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
*/
static herr_t
H5FD_mpio_get_info(H5FD_t *_file, void** mpi_info)
{
    H5FD_mpio_t         *file = (H5FD_mpio_t *)_file;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(!mpi_info)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "mpi info not valid")

    *mpi_info = &(file->info);

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* H5FD_mpio_get_info() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_read
 *
 * Purpose:	Reads SIZE bytes of data from FILE beginning at address ADDR
 *		into buffer BUF according to data transfer properties in
 *		DXPL_ID using potentially complex file and buffer types to
 *		effect the transfer.
 *
 *		Reading past the end of the MPI file returns zeros instead of
 *		failing.  MPI is able to coalesce requests from different
 *		processes (collective or independent).
 *
 * Return:	Success:	Zero. Result is stored in caller-supplied
 *				buffer BUF.
 *
 *		Failure:	-1, Contents of buffer BUF are undefined.
 *
 * Programmer:	rky, 1998-01-30
 *
 * Modifications:
 * 		Robb Matzke, 1998-02-18
 *		Added the ACCESS_PARMS argument.
 *
 * 		rky, 1998-04-10
 *		Call independent or collective MPI read, based on
 *		ACCESS_PARMS.
 *
 * 		Albert Cheng, 1998-06-01
 *		Added XFER_MODE to control independent or collective MPI
 *		read.
 *
 * 		rky, 1998-08-16
 *		Use BTYPE, FTYPE, and DISP from access parms. The guts of
 *		H5FD_mpio_read and H5FD_mpio_write should be replaced by a
 *		single dual-purpose routine.
 *
 * 		Robb Matzke, 1999-04-21
 *		Changed XFER_MODE to XFER_PARMS for all H5F_*_read()
 *		callbacks.
 *
 * 		Robb Matzke, 1999-07-28
 *		The ADDR argument is passed by value.
 *
 * 		Robb Matzke, 1999-08-06
 *		Modified to work with the virtual file layer.
 *
 *		Quincey Koziol,  2002-05-14
 *		Only call MPI_Get_count if we can use MPI_BYTE for the MPI type
 *              for the I/O transfer.  Someday we might include code to decode
 *              the MPI type used for more complicated transfers and call
 *              MPI_Get_count all the time.
 *
 *              Quincey Koziol - 2002/06/17
 *              Removed 'disp' parameter from H5FD_mpio_setup routine and use
 *              the address of the dataset in MPI_File_set_view() calls, as
 *              necessary.
 *
 *              Quincey Koziol - 2002/06/24
 *              Removed "lazy" MPI_File_set_view() calls, since they would fail
 *              if the first I/O was a collective I/O using MPI derived types
 *              and the next I/O was an independent I/O.
 *
 *              Quincey Koziol - 2003/10/22-31
 *              Restructured code massively, straightening out logic and finally
 *              getting the bytes_read stuff working.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpio_read(H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type,
    hid_t H5_ATTR_UNUSED dxpl_id, haddr_t addr, size_t size, void *buf/*out*/)
{
    H5FD_mpio_t			*file = (H5FD_mpio_t*)_file;
    MPI_Offset			mpi_off;
    MPI_Status  		mpi_stat;       /* Status from I/O operation */
    int				mpi_code;	/* mpi return code */
    MPI_Datatype		buf_type = MPI_BYTE;      /* MPI description of the selection in memory */
    int         		size_i;         /* Integer copy of 'size' to read */
#if MPI_VERSION >= 3
    MPI_Count         		bytes_read;     /* Number of bytes read in */
    MPI_Count                   type_size;      /* MPI datatype used for I/O's size */
    MPI_Count                   io_size;        /* Actual number of bytes requested */
    MPI_Count                   n;
#else
    int                         bytes_read;     /* Number of bytes read in */
    int                         type_size;      /* MPI datatype used for I/O's size */
    int                         io_size;        /* Actual number of bytes requested */
    int         		n;
#endif
    hbool_t			use_view_this_time = FALSE;
    herr_t              	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Entering H5FD_mpio_read\n" );
#endif
    HDassert(file);
    HDassert(H5FD_MPIO==file->pub.driver_id);
    HDassert(buf);

    /* Portably initialize MPI status variable */
    HDmemset(&mpi_stat,0,sizeof(MPI_Status));

    /* some numeric conversions */
    if (H5FD_mpi_haddr_to_MPIOff(addr, &mpi_off/*out*/)<0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADRANGE, FAIL, "can't convert from haddr to MPI off")
    size_i = (int)size;
    if ((hsize_t)size_i != size)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADRANGE, FAIL, "can't convert from size to size_i")

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'r'])
        fprintf(stdout, "in H5FD_mpio_read  mpi_off=%ld  size_i=%d\n",
		(long)mpi_off, size_i );
#endif

    /* Only look for MPI views for raw data transfers */
    if(type == H5FD_MEM_DRAW) {
        H5FD_mpio_xfer_t            xfer_mode;   /* I/O transfer mode */

        /* Get the transfer mode from the API context */
        if(H5CX_get_io_xfer_mode(&xfer_mode) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "can't get MPI-I/O transfer mode")

        /*
         * Set up for a fancy xfer using complex types, or single byte block. We
         * wouldn't need to rely on the use_view field if MPI semantics allowed
         * us to test that btype=ftype=MPI_BYTE (or even MPI_TYPE_NULL, which
         * could mean "use MPI_BYTE" by convention).
         */
        if(xfer_mode==H5FD_MPIO_COLLECTIVE) {
            MPI_Datatype		file_type;

            /* Remember that views are used */
            use_view_this_time = TRUE;

            /* Prepare for a full-blown xfer using btype, ftype, and disp */
            if(H5CX_get_mpi_coll_datatypes(&buf_type, &file_type) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "can't get MPI-I/O datatypes")

            /*
             * Set the file view when we are using MPI derived types
             */
            if(MPI_SUCCESS != (mpi_code = MPI_File_set_view(file->f, mpi_off, MPI_BYTE, file_type, H5FD_mpi_native_g, file->info)))
                HMPI_GOTO_ERROR(FAIL, "MPI_File_set_view failed", mpi_code)

            /* When using types, use the address as the displacement for
             * MPI_File_set_view and reset the address for the read to zero
             */
            mpi_off = 0;
        } /* end if */
    } /* end if */

    /* Read the data. */
    if(use_view_this_time) {
        H5FD_mpio_collective_opt_t coll_opt_mode;

#ifdef H5FDmpio_DEBUG
	if (H5FD_mpio_Debug[(int)'t'])
	    fprintf(stdout, "H5FD_mpio_read: using MPIO collective mode\n");
#endif
        /* Get the collective_opt property to check whether the application wants to do IO individually. */
        if(H5CX_get_mpio_coll_opt(&coll_opt_mode) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "can't get MPI-I/O collective_op property")

        if(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO) {
#ifdef H5FDmpio_DEBUG
            if(H5FD_mpio_Debug[(int)'t'])
                fprintf(stdout, "H5FD_mpio_read: doing MPI collective IO\n");
#endif
            if(MPI_SUCCESS != (mpi_code = MPI_File_read_at_all(file->f, mpi_off, buf, size_i, buf_type, &mpi_stat)))
                HMPI_GOTO_ERROR(FAIL, "MPI_File_read_at_all failed", mpi_code)
        } /* end if */
        else {
#ifdef H5FDmpio_DEBUG
            if(H5FD_mpio_Debug[(int)'t'])
                fprintf(stdout, "H5FD_mpio_read: doing MPI independent IO\n");
#endif

            if(MPI_SUCCESS != (mpi_code = MPI_File_read_at(file->f, mpi_off, buf, size_i, buf_type, &mpi_stat)))
                HMPI_GOTO_ERROR(FAIL, "MPI_File_read_at failed", mpi_code)
        } /* end else */

        /*
         * Reset the file view when we used MPI derived types
         */
        if(MPI_SUCCESS != (mpi_code = MPI_File_set_view(file->f, (MPI_Offset)0, MPI_BYTE, MPI_BYTE, H5FD_mpi_native_g, file->info)))
            HMPI_GOTO_ERROR(FAIL, "MPI_File_set_view failed", mpi_code)
    } else {
        if(MPI_SUCCESS != (mpi_code = MPI_File_read_at(file->f, mpi_off, buf, size_i, buf_type, &mpi_stat)))
            HMPI_GOTO_ERROR(FAIL, "MPI_File_read_at failed", mpi_code)
    }

    /* How many bytes were actually read? */
#if MPI_VERSION >= 3
    if (MPI_SUCCESS != (mpi_code = MPI_Get_elements_x(&mpi_stat, buf_type, &bytes_read)))
#else
    if (MPI_SUCCESS != (mpi_code = MPI_Get_elements(&mpi_stat, MPI_BYTE, &bytes_read)))
#endif
        HMPI_GOTO_ERROR(FAIL, "MPI_Get_elements failed", mpi_code)

    /* Get the type's size */
#if MPI_VERSION >= 3
    if (MPI_SUCCESS != (mpi_code = MPI_Type_size_x(buf_type, &type_size)))
#else
    if (MPI_SUCCESS != (mpi_code = MPI_Type_size(buf_type, &type_size)))
#endif
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_size failed", mpi_code)

    /* Compute the actual number of bytes requested */
    io_size=type_size*size_i;

    /* Check for read failure */
    if (bytes_read<0 || bytes_read>io_size)
        HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "file read failed")

    /*
     * This gives us zeroes beyond end of physical MPI file.
     */
    if ((n=(io_size-bytes_read)) > 0)
        HDmemset((char*)buf+bytes_read, 0, (size_t)n);

done:
#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Leaving H5FD_mpio_read\n" );
#endif

    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_write
 *
 * Purpose:	Writes SIZE bytes of data to FILE beginning at address ADDR
 *		from buffer BUF according to data transfer properties in
 *		DXPL_ID using potentially complex file and buffer types to
 *		effect the transfer.
 *
 *		MPI is able to coalesce requests from different processes
 *		(collective and independent).
 *
 * Return:	Success:	Zero. USE_TYPES and OLD_USE_TYPES in the
 *				access params are altered.
 *
 *		Failure:	-1, USE_TYPES and OLD_USE_TYPES in the
 *				access params may be altered.
 *
 * Programmer:	Unknown
 *              January 30, 1998
 *
 * Modifications:
 *		rky, 1998-08-28
 *		If the file->allsame flag is set, we assume that all the
 *		procs in the relevant MPI communicator will write identical
 *		data at identical offsets in the file, so only proc 0 will
 *		write, and all other procs will wait for p0 to finish. This
 *		is useful for writing metadata, for example. Note that we
 *		don't _check_ that the data is identical. Also, the mechanism
 *		we use to eliminate the redundant writes is by requiring a
 *		call to H5FD_mpio_tas_allsame before the write, which is
 *		rather klugey. Would it be better to pass a parameter to
 *		low-level writes like H5F_block_write and H5F_low_write,
 *		instead?  Or...??? Also, when I created this mechanism I
 *		wanted to minimize the difference in behavior between the old
 *		way of doing things (i.e., all procs write) and the new way,
 *		so the writes are eliminated at the very lowest level, here
 *		in H5FD_mpio_write. It may be better to rethink that, and
 *		short-circuit the writes at a higher level (e.g., at the
 *		points in the code where H5FD_mpio_tas_allsame is called).
 *
 *
 * 		Robb Matzke, 1998-02-18
 *		Added the ACCESS_PARMS argument.
 *
 * 		rky, 1998-04-10
 *		Call independent or collective MPI write, based on
 *		ACCESS_PARMS.
 *
 * 		rky, 1998-04-24
 *		Removed redundant write from H5FD_mpio_write.
 *
 * 		Albert Cheng, 1998-06-01
 *		Added XFER_MODE to control independent or collective MPI
 *		write.
 *
 * 		rky, 1998-08-16
 *		Use BTYPE, FTYPE, and DISP from access parms. The guts of
 *		H5FD_mpio_read and H5FD_mpio_write should be replaced by a
 *		single dual-purpose routine.
 *
 * 		rky, 1998-08-28
 *		Added ALLSAME parameter to make all but proc 0 skip the
 *		actual write.
 *
 * 		Robb Matzke, 1999-04-21
 *		Changed XFER_MODE to XFER_PARMS for all H5FD_*_write()
 *		callbacks.
 *
 * 		Robb Matzke, 1999-07-28
 *		The ADDR argument is passed by value.
 *
 * 		Robb Matzke, 1999-08-06
 *		Modified to work with the virtual file layer.
 *
 *		Albert Cheng, 1999-12-19
 *		When only-p0-write-allsame-data, p0 Bcasts the
 *		ret_value to other processes.  This prevents
 *		a racing condition (that other processes try to
 *		read the file before p0 finishes writing) and also
 *		allows all processes to report the same ret_value.
 *
 *		Kim Yates, Pat Weidhaas,  2000-09-26
 *		Move block of coding where only p0 writes after the
 *              MPI_File_set_view call.
 *
 *		Quincey Koziol,  2002-05-10
 *		Instead of always writing metadata from process 0, spread the
 *              burden among all the processes by using a round-robin rotation
 *              scheme.
 *
 *		Quincey Koziol,  2002-05-10
 *		Removed allsame code, keying off the type parameter instead.
 *
 *		Quincey Koziol,  2002-05-14
 *		Only call MPI_Get_count if we can use MPI_BYTE for the MPI type
 *              for the I/O transfer.  Someday we might include code to decode
 *              the MPI type used for more complicated transfers and call
 *              MPI_Get_count all the time.
 *
 *              Quincey Koziol - 2002/06/17
 *              Removed 'disp' parameter from H5FD_mpio_setup routine and use
 *              the address of the dataset in MPI_File_set_view() calls, as
 *              necessary.
 *
 *              Quincey Koziol - 2002/06/24
 *              Removed "lazy" MPI_File_set_view() calls, since they would fail
 *              if the first I/O was a collective I/O using MPI derived types
 *              and the next I/O was an independent I/O.
 *
 *              Quincey Koziol - 2002/07/18
 *              Added "block_before_meta_write" dataset transfer flag, which
 *              is set during writes from a metadata cache flush and indicates
 *              that all the processes must sync up before (one of them)
 *              writing metadata.
 *
 *              Quincey Koziol - 2003/10/22-31
 *              Restructured code massively, straightening out logic and finally
 *              getting the bytes_written stuff working.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpio_write(H5FD_t *_file, H5FD_mem_t type, hid_t H5_ATTR_UNUSED dxpl_id,
    haddr_t addr, size_t size, const void *buf)
{
    H5FD_mpio_t			*file = (H5FD_mpio_t*)_file;
    MPI_Offset 		 	mpi_off;
    MPI_Status  		mpi_stat;       /* Status from I/O operation */
    MPI_Datatype		buf_type = MPI_BYTE;      /* MPI description of the selection in memory */
    int			        mpi_code;	/* MPI return code */
#if MPI_VERSION >= 3
    MPI_Count         		bytes_written;
    MPI_Count                   type_size;      /* MPI datatype used for I/O's size */
    MPI_Count                   io_size;        /* Actual number of bytes requested */
#else
    int                         bytes_written;
    int                         type_size;      /* MPI datatype used for I/O's size */
    int                         io_size;        /* Actual number of bytes requested */
#endif
    int                         size_i;
    hbool_t			use_view_this_time = FALSE;
    H5FD_mpio_xfer_t            xfer_mode;   /* I/O transfer mode */
    herr_t              	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Entering H5FD_mpio_write\n" );
#endif
    HDassert(file);
    HDassert(H5FD_MPIO==file->pub.driver_id);
    HDassert(buf);

    /* Verify that no data is written when between MPI_Barrier()s during file flush */
    HDassert(!H5CX_get_mpi_file_flushing());

    /* Portably initialize MPI status variable */
    HDmemset(&mpi_stat, 0, sizeof(MPI_Status));

    /* some numeric conversions */
    if(H5FD_mpi_haddr_to_MPIOff(addr, &mpi_off) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADRANGE, FAIL, "can't convert from haddr to MPI off")
    size_i = (int)size;
    if((hsize_t)size_i != size)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADRANGE, FAIL, "can't convert from size to size_i")

#ifdef H5FDmpio_DEBUG
    if(H5FD_mpio_Debug[(int)'w'])
        fprintf(stdout, "in H5FD_mpio_write  mpi_off=%ld  size_i=%d\n", (long)mpi_off, size_i);
#endif

    /* Get the transfer mode from the API context */
    if(H5CX_get_io_xfer_mode(&xfer_mode) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "can't get MPI-I/O transfer mode")

    /*
     * Set up for a fancy xfer using complex types, or single byte block. We
     * wouldn't need to rely on the use_view field if MPI semantics allowed
     * us to test that btype=ftype=MPI_BYTE (or even MPI_TYPE_NULL, which
     * could mean "use MPI_BYTE" by convention).
     */
    if(xfer_mode == H5FD_MPIO_COLLECTIVE) {
        MPI_Datatype		file_type;

        /* Remember that views are used */
        use_view_this_time = TRUE;

        /* Prepare for a full-blown xfer using btype, ftype, and disp */
        if(H5CX_get_mpi_coll_datatypes(&buf_type, &file_type) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "can't get MPI-I/O datatypes")

        /*
         * Set the file view when we are using MPI derived types
         */
        if(MPI_SUCCESS != (mpi_code = MPI_File_set_view(file->f, mpi_off, MPI_BYTE, file_type, H5FD_mpi_native_g, file->info)))
            HMPI_GOTO_ERROR(FAIL, "MPI_File_set_view failed", mpi_code)

        /* When using types, use the address as the displacement for
         * MPI_File_set_view and reset the address for the read to zero
         */
        mpi_off = 0;
    } /* end if */

    /* Write the data. */
    if(use_view_this_time) {
        H5FD_mpio_collective_opt_t coll_opt_mode;

#ifdef H5FDmpio_DEBUG
        if(H5FD_mpio_Debug[(int)'t'])
            fprintf(stdout, "H5FD_mpio_write: using MPIO collective mode\n");
#endif

        /* Get the collective_opt property to check whether the application wants to do IO individually. */
        if(H5CX_get_mpio_coll_opt(&coll_opt_mode) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "can't get MPI-I/O collective_op property")

        if(coll_opt_mode == H5FD_MPIO_COLLECTIVE_IO) {
#ifdef H5FDmpio_DEBUG
            if(H5FD_mpio_Debug[(int)'t'])
                fprintf(stdout, "H5FD_mpio_write: doing MPI collective IO\n");
#endif
            if(MPI_SUCCESS != (mpi_code = MPI_File_write_at_all(file->f, mpi_off, buf, size_i, buf_type, &mpi_stat)))
                HMPI_GOTO_ERROR(FAIL, "MPI_File_write_at_all failed", mpi_code)
        } /* end if */
        else {
            if(type != H5FD_MEM_DRAW)
                HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "Metadata Coll opt property should be collective at this point")
#ifdef H5FDmpio_DEBUG
            if(H5FD_mpio_Debug[(int)'t'])
                fprintf(stdout, "H5FD_mpio_write: doing MPI independent IO\n");
#endif
            if(MPI_SUCCESS != (mpi_code = MPI_File_write_at(file->f, mpi_off, buf, size_i, buf_type, &mpi_stat)))
                HMPI_GOTO_ERROR(FAIL, "MPI_File_write_at failed", mpi_code)
        } /* end else */

        /* Reset the file view when we used MPI derived types */
        if(MPI_SUCCESS != (mpi_code = MPI_File_set_view(file->f, (MPI_Offset)0, MPI_BYTE, MPI_BYTE, H5FD_mpi_native_g,  file->info)))
            HMPI_GOTO_ERROR(FAIL, "MPI_File_set_view failed", mpi_code)
    } else {
        if(MPI_SUCCESS != (mpi_code = MPI_File_write_at(file->f, mpi_off, buf, size_i, buf_type, &mpi_stat)))
            HMPI_GOTO_ERROR(FAIL, "MPI_File_write_at failed", mpi_code)
    }

    /* How many bytes were actually written? */
#if MPI_VERSION >= 3
    if(MPI_SUCCESS != (mpi_code = MPI_Get_elements_x(&mpi_stat, buf_type, &bytes_written)))
#else
    if(MPI_SUCCESS != (mpi_code = MPI_Get_elements(&mpi_stat, MPI_BYTE, &bytes_written)))
#endif
        HMPI_GOTO_ERROR(FAIL, "MPI_Get_elements failed", mpi_code)

    /* Get the type's size */
#if MPI_VERSION >= 3
    if(MPI_SUCCESS != (mpi_code = MPI_Type_size_x(buf_type, &type_size)))
#else
    if(MPI_SUCCESS != (mpi_code = MPI_Type_size(buf_type, &type_size)))
#endif
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_size failed", mpi_code)

    /* Compute the actual number of bytes requested */
    io_size = type_size * size_i;

    /* Check for write failure */
    if(bytes_written != io_size)
        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "file write failed")

    /* Each process will keep track of its perceived EOF value locally, and
     * ultimately we will reduce this value to the maximum amongst all
     * processes, but until then keep the actual eof at HADDR_UNDEF just in
     * case something bad happens before that point. (rather have a value
     * we know is wrong sitting around rather than one that could only
     * potentially be wrong.) */
    file->eof = HADDR_UNDEF;

    if(bytes_written && ((bytes_written + addr) > file->local_eof))
        file->local_eof = addr + bytes_written;

done:
#ifdef H5FDmpio_DEBUG
    if(H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "proc %d: Leaving H5FD_mpio_write with ret_value=%d\n",
	    file->mpi_rank, ret_value );
#endif
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_mpio_write() */

/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_setup_flatbuf
 *
 * Purpose:	Define the flatbuf structure needed by H5FD_mpio_custom_write.
 *		This is a helper function to avoid repeating code. The flatbuf can
 *		be a file or memory flatbuf -- and the structure depends on the type
 *		of selection.
 *
 * Return:	Success:	Non-negative
 *			Failure:	Negative
 *
 * Programmer:	Rick Zamora, 2018-07-03
 *				(Based on code originally in H5FD_mpio_custom_write)
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpio_setup_flatbuf( H5S_sel_type space_sel_type, H5S_flatbuf_t *curflatbuf,
    H5S_sel_iter_t *sel_iter, H5S_t *space_stype, size_t elmt_size, hbool_t is_regular)
{
    herr_t ret_value = SUCCEED;
    size_t sel_nseq, sel_nelem;
    hsize_t flatBufSize;
    unsigned null_flags = 0;
    hsize_t num_points;
    hssize_t snum_points;
    int numSpaceDims = 0;
    int numSelDims = 0;
    H5S_hyper_dim_t	*diminfo;
    hsize_t numBlockEntries = 1;
    hsize_t numElements = 1;
    herr_t rc = 0;

    if (space_sel_type == H5S_SEL_NONE) {
        curflatbuf->indices = NULL;
        curflatbuf->blocklens = NULL;
        curflatbuf->count = 0;
        curflatbuf->size = 0;
        curflatbuf->extent = 0;
#ifdef H5FDmpio_DEBUG
        if (H5FD_mpio_Debug[(int)'t']) {
            fprintf(stdout,"space_sel_type == H5S_SEL_NONE for flatbuf - setting everything to 0\n");
            fflush(stdout);
        }
#endif
    }
    else if (space_sel_type == H5S_SEL_ALL) {

        /* For H5S_SEL_ALL there is just 1 big block */
        curflatbuf->indices = (hsize_t *) H5MM_malloc(1 * sizeof(hsize_t));
        curflatbuf->blocklens = (size_t *) H5MM_malloc(1 * sizeof(size_t));
        curflatbuf->count = 1;
        curflatbuf->extent = curflatbuf->indices[0] + (hsize_t)curflatbuf->blocklens[0];

        if(H5S__all_get_seq_list(space_stype,null_flags,sel_iter,1,sel_iter->elmt_left,&sel_nseq,&sel_nelem,curflatbuf->indices,curflatbuf->blocklens) < 0)
        {
            fprintf(stdout,"ERROR: H5S__all_get_seq_list failed");
            ret_value = FAIL;
        }

        flatBufSize = 0;
        for (int j=0;j<curflatbuf->count;j++) {
            flatBufSize += curflatbuf->blocklens[j];
        }
        curflatbuf->size = flatBufSize;

#ifdef H5FDmpio_DEBUG
        if (H5FD_mpio_Debug[(int)'t']) {
            fprintf(stdout,"space_sel_type == H5S_SEL_ALL for flatbuf - curflatbuf->size is %ld curflatbuf->indices[0] is %ldcurflatbuf->blocklens[0] is %ld curflatbuf->extent is %ld\n",curflatbuf->size,curflatbuf->indices[0],curflatbuf->blocklens[0],curflatbuf->extent);
            fflush(stdout);
        }
#endif
    }
    else if (space_sel_type == H5S_SEL_POINTS) {

        if((snum_points = (hssize_t)H5S_GET_SELECT_NPOINTS(space_stype)) < 0)
        {
            fprintf(stdout,"ERROR: can't get number of elements selected");
            ret_value = FAIL;
        }
        num_points = (hsize_t)snum_points;

        curflatbuf->indices = (hsize_t *) H5MM_malloc(num_points * sizeof(hsize_t));
        curflatbuf->blocklens = (size_t *) H5MM_malloc(num_points * sizeof(size_t));
        curflatbuf->count = 1;

        /* Get the extent */
        hsize_t dims[H5O_LAYOUT_NDIMS];     /* Total size of memory buf */
        if((numSpaceDims = H5S_get_simple_extent_dims (space_stype, dims, NULL)) < 0){
            fprintf(stdout,"ERROR: unable to retrieve data space dimensions");
            ret_value = FAIL;
        }
        curflatbuf->extent = 1;
        for (int j=0;j<numSpaceDims;j++)
            curflatbuf->extent *= dims[j];

        if(H5S_point_get_seq_list(space_stype,null_flags,sel_iter,num_points,num_points,&sel_nseq,&sel_nelem,curflatbuf->indices,curflatbuf->blocklens) < 0)
        {
            fprintf(stdout,"ERROR: H5S__all_get_seq_list failed");
            ret_value = FAIL;
        }

        flatBufSize = 0;
        for (int j=0;j<curflatbuf->count;j++) {
            flatBufSize += curflatbuf->blocklens[j];
        }
        curflatbuf->size = flatBufSize;

#ifdef H5FDmpio_DEBUG
        if (H5FD_mpio_Debug[(int)'t']) {
            fprintf(stdout,"space_sel_type == H5S_SEL_POINTS called H5S_point_get_seq_list for file flatbuf - curflatbuf->count is %ld numSpaceDims is %d curflatbuf->extent is %ld curflatbuf->size is %ld returned sel_nseq %ld sel_nelem %ld offset/len pairs for curflatbuf->count entries are:\n",curflatbuf->count,numSpaceDims,curflatbuf->extent,curflatbuf->size,sel_nseq,sel_nelem);
            for (int j=0;j<curflatbuf->count;j++)
                fprintf(stdout, " %d offset: %ld len: %ld\n",j,curflatbuf->indices[j],curflatbuf->blocklens[j]);
            fflush(stdout);
        }
#endif
    }
    else if (space_sel_type == H5S_SEL_HYPERSLABS) {

        if (!is_regular){
            fprintf(stdout, "ERROR: irregular space selection not supported");
            // rjz - commenting this check for now:
            //ret_value = FAIL;
        }

        diminfo = sel_iter->u.hyp.diminfo;
        HDassert(diminfo);

        /* Here we need to use a function inside the space module since that is where the H5S_t structure is
         * actually defined.
         */
        rc = H5S_mpio_return_space_rank_and_extent(space_stype, &numSpaceDims, &(curflatbuf->extent));

        curflatbuf->extent *= elmt_size;

        /* Check for flattened selection, if so use the selection iter_rank for the number of
         * dimensions instead of the space rank.
         */
        if(sel_iter->u.hyp.iter_rank != 0 && sel_iter->u.hyp.iter_rank < numSpaceDims)
            numSelDims = sel_iter->u.hyp.iter_rank;
        else
            numSelDims = numSpaceDims;

#ifdef H5FDmpio_DEBUG
        if (H5FD_mpio_Debug[(int)'t']) {
            fprintf(stdout,"space_sel_type == H5S_SEL_HYPERSLABS computing numBlockEntries and numElements\n");
            fflush(stdout);
        }
#endif

        numBlockEntries = 1, numElements = 1;
        for(int u = 0; u < numSelDims; u++) {
#ifdef H5FDmpio_DEBUG
            if (H5FD_mpio_Debug[(int)'t']) {
                fprintf(stdout,"iter %d diminfo[u].count is %ld and diminfo[u].block is %ld\n",u,diminfo[u].count,diminfo[u].block);
                fflush(stdout);
            }
#endif
            if (u < (numSelDims-1)) {
                numBlockEntries *= (diminfo[u].count * diminfo[u].block);
                numElements *= (diminfo[u].count * diminfo[u].block);
            }
            else {
                numBlockEntries *= diminfo[u].count;
                numElements *= (diminfo[u].count * diminfo[u].block);
            }
        }

        curflatbuf->indices = (hsize_t *) H5MM_malloc(numElements * sizeof(hsize_t));
        curflatbuf->blocklens = (size_t *) H5MM_malloc(numElements * sizeof(size_t));
        curflatbuf->count = numBlockEntries;

#ifdef H5FDmpio_DEBUG
        if (H5FD_mpio_Debug[(int)'t']) {
            fprintf(stdout,"calling H5S__hyper_get_seq_list for file flatbuf - numSelDims is %d numElements is %ld curflatbuf->count is %ld curflatbuf->extent is %ld\n",numSelDims,numElements, curflatbuf->count, curflatbuf->extent);
            fflush(stdout);
        }
#endif

        if(H5S__hyper_get_seq_list(space_stype,null_flags,sel_iter,numElements,numElements,&sel_nseq,&sel_nelem,curflatbuf->indices,curflatbuf->blocklens) < 0)
        {
            fprintf(stdout,"ERROR: H5S__hyper_get_seq_list failed");
            ret_value = FAIL;
        }

        flatBufSize = 0;
        for (int j=0;j<curflatbuf->count;j++) {
            flatBufSize += curflatbuf->blocklens[j];
        }
        curflatbuf->size = flatBufSize;

#ifdef H5FDmpio_DEBUG
        if (H5FD_mpio_Debug[(int)'t']) {
            fprintf(stdout,"called H5S__hyper_get_seq_list for file flatbuf - numSelDims is %d numElements is %ld curflatbuf->count is %ld curflatbuf->extent is %ld curflatbuf->size is %ld returned sel_nseq %ld sel_nelem %ld offset/len pairs for curflatbuf->count entries are:\n",numSelDims,numElements, curflatbuf->count,curflatbuf->extent,curflatbuf->size,sel_nseq,sel_nelem);
            for (int j=0;j<curflatbuf->count;j++)
                fprintf(stdout, " %d offset: %ld len: %ld\n",j,curflatbuf->indices[j],curflatbuf->blocklens[j]);
            fflush(stdout);
        }
#endif

    }
    else {
        fprintf(stdout, "ERROR: In H5FD_mpio_setup_flatbuf, space selection type not recognized");
        ret_value = FAIL;
    }

    return ret_value;
} /* H5FD_mpio_setup_flatbuf */

/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_custom_write
 *
 * Purpose:	Writes data from a memory flatbuf into a file flatbuf. The memory
 *		and file flatbuf structures are defined using the H5S_<*>_get_seq_list
 *		functions, where <*> depends on the type of selection: all, points,
 *		hyperslab, or none.
 *		Note that this function is called from H5FD_select_write(), and is
 *		used to call optimized "write" routines defined in the "custom-collective
 *		IO virtual file layer" (CCIO) of the MPIO-VFD (see H5FDmpio_ccio.c).
 *
 * Return:
 *
 * Programmer:	Quincey Koziol and Paul Coffman
 *              Unknown (Winter), 2018
 *
 * Modifications:
 *		Rick Zamora, 2018-07-02
 *		cleanup and refactoring.
 *
 *-------------------------------------------------------------------------
 */
static herr_t H5FD_mpio_custom_write(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id,
    hid_t file_space, hid_t mem_space, size_t elmt_size, haddr_t addr, const void *buf)
{
    H5FD_mpio_t                 *file = (H5FD_mpio_t*)_file;
    MPI_Offset                  mpi_off;
    MPI_Status                  mpi_stat;       /* Status from I/O operation */
    H5S_t                       *file_space_stype;
    int                         file_space_ref_count;
    H5S_t                       *mem_space_stype;
    int                         mem_space_ref_count;
    int                         mpi_code;       /* MPI return code */
#if MPI_VERSION >= 3
    MPI_Count                   bytes_written;
    MPI_Count                   type_size;      /* MPI datatype used for I/O's size */
    MPI_Count                   io_size;        /* Actual number of bytes requested */
#else
    int                         bytes_written;
    int                         type_size;      /* MPI datatype used for I/O's size */
    int                         io_size;        /* Actual number of bytes requested */
#endif
    int                         size_i;
    H5P_genplist_t              *plist = NULL;  /* Property list pointer */
    H5FD_mpio_xfer_t            xfer_mode;      /* I/O tranfer mode */
    herr_t                      ret_value = SUCCEED;
    H5S_flatbuf_t               file_flatbuf;
    H5S_flatbuf_t               mem_flatbuf;
    hbool_t                     is_permuted = FALSE;
    hbool_t                     is_regular = TRUE;
    H5S_sel_iter_t              sel_iter;
    H5S_class_t                 file_space_extent_type;
    H5S_class_t                 mem_space_extent_type;
    H5S_sel_type                file_space_sel_type;
    H5S_sel_type                mem_space_sel_type;
    herr_t                      rc = 0;
    hsize_t                     *permute_map = NULL;

    /* Note: permute_map array holds the mapping from the old (out-of-order)
     * displacements to the in-order displacements of the H5S_flatbuf_t of the
     * point selection of the file space.
     */

    FUNC_ENTER_NOAPI_NOINIT

    /* File and memory space setup */
    file_space_stype = (H5S_t *) H5I_remove(file_space);
    file_space_ref_count = H5I_dec_ref(file_space);
    mem_space_stype = (H5S_t *) H5I_remove(mem_space);
    mem_space_ref_count = H5I_dec_ref(mem_space);

    /* some numeric conversions */
    if(H5FD_mpi_haddr_to_MPIOff(addr, &mpi_off) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADRANGE, FAIL, "can't convert from haddr to MPI off")

    size_i = (int)elmt_size;
    if((hsize_t)size_i != elmt_size)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADRANGE, FAIL, "can't convert from elmt_size to size_i")

    HDassert(file);
    HDassert(H5FD_MPIO==file->pub.driver_id);

    /* Make certain we have the correct type of property list */
    HDassert(H5I_GENPROP_LST==H5I_get_type(dxpl_id));
    HDassert(TRUE==H5P_isa_class(dxpl_id,H5P_DATASET_XFER));
    HDassert(buf);

    /* Portably initialize MPI status variable */
    HDmemset(&mpi_stat, 0, sizeof(MPI_Status));

    /*
     * Create flatbuf for FILE space selection
     */

    if(H5S_select_iter_init(&sel_iter, file_space_stype, elmt_size) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to initialize selection iterator")

    rc = H5S_mpio_return_space_extent_and_select_type(file_space_stype, &is_permuted, &is_regular, &file_space_extent_type, &file_space_sel_type);

    if(is_permuted)
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "permuted space selections not supported")

    /* Currently, file_space_extent_type must be H5S_NULL, H5S_SCALAR, or H5S_SIMPLE */
    if (!((file_space_extent_type == H5S_NULL) || (file_space_extent_type == H5S_SCALAR) || (file_space_extent_type == H5S_SIMPLE)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "file space extent type invalid")

    if(H5S_SELECT_ITER_RELEASE(&sel_iter) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release selection iterator")

    if ((file_space_sel_type == H5S_SEL_NONE) || (file_space_sel_type == H5S_SEL_ALL) ||
      (file_space_sel_type == H5S_SEL_POINTS) || (file_space_sel_type == H5S_SEL_HYPERSLABS)) {
        if( H5FD_mpio_setup_flatbuf( file_space_sel_type, &file_flatbuf, &sel_iter, file_space_stype, elmt_size, is_regular ) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "Call to H5FD_mpio_setup_flatbuf failed for FILE")
    }
    else {
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "Space selection type not recognized")
    }

#ifdef onesidedtrace
//
//    typedef struct H5S_flatbuf_t {
//        hsize_t count; /* number of contiguous blocks */
//        size_t *blocklens; /* array of contiguous block lengths (bytes)*/
//        hsize_t *indices; /*array of byte offsets of each block */
//        hsize_t extent; /* offset range for one instance of this flatbuf */
//        hsize_t size; /* number of bytes of block data */
//    } H5S_flatbuf_t;
//
    printf("_______ - file_flatbuf.count = %d, file_flatbuf.extent = %d, file_flatbuf.size = %d\n",file_flatbuf.count,file_flatbuf.extent,file_flatbuf.size);
    for (int i=0; i<file_flatbuf.count; i++) {
        printf("_______ - file_flatbuf.blocklens[%d] = %d, file_flatbuf.indices[%d] = %d\n",i,file_flatbuf.blocklens[i],i,file_flatbuf.indices[i]);
    }
#endif

    /*
     * Create flatbuf for MEMORY space selection
     */

    if(H5S_select_iter_init(&sel_iter, mem_space_stype, elmt_size) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to initialize selection iterator")

    rc = H5S_mpio_return_space_extent_and_select_type(mem_space_stype, &is_permuted, &is_regular, &mem_space_extent_type, &mem_space_sel_type);

    if(is_permuted)
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "permuted space selections not supported")

    /* Currently, mem_space_extent_type must be H5S_NULL, H5S_SCALAR, or H5S_SIMPLE */
    if (!((mem_space_extent_type == H5S_NULL) || (mem_space_extent_type == H5S_SCALAR) || (mem_space_extent_type == H5S_SIMPLE)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "space extent type invalid")

    if ((mem_space_sel_type == H5S_SEL_NONE) || (mem_space_sel_type == H5S_SEL_ALL) ||
      (mem_space_sel_type == H5S_SEL_POINTS) || (mem_space_sel_type == H5S_SEL_HYPERSLABS)) {
        if( H5FD_mpio_setup_flatbuf( mem_space_sel_type, &mem_flatbuf, &sel_iter, mem_space_stype, elmt_size, is_regular ) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "Call to H5FD_mpio_setup_flatbuf failed for MEM")
    }
    else {
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "Space selection type not recognized")
    }

#ifdef onesidedtrace
    printf("_______ - mem_flatbuf.count = %d, mem_flatbuf.extent = %d, mem_flatbuf.size = %d\n",mem_flatbuf.count,mem_flatbuf.extent,mem_flatbuf.size);
    for (int i=0; i<mem_flatbuf.count; i++) {
        printf("_______ - mem_flatbuf.blocklens[%d] = %d, mem_flatbuf.indices[%d] = %d\n",i,mem_flatbuf.blocklens[i],i,mem_flatbuf.indices[i]);
    }
#endif

    if(H5S_SELECT_ITER_RELEASE(&sel_iter) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release selection iterator")

    /* Obtain the data transfer properties */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    /* get the transfer mode from the dxpl */
    if(H5P_get(plist, H5D_XFER_IO_XFER_MODE_NAME, &xfer_mode)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get MPI-I/O transfer mode")

    /*
     * If using collective IO call the custom agggregation algorithm here.
     */
    if(xfer_mode == H5FD_MPIO_COLLECTIVE) {

        int error_code;
        H5FD_mpio_ccio_write_one_sided((CustomAgg_FH_Data)&(file->custom_agg_data), buf, mpi_off, &mem_flatbuf, &file_flatbuf, &error_code);
        if (file_flatbuf.indices) H5MM_free(file_flatbuf.indices);
        if (file_flatbuf.blocklens) H5MM_free(file_flatbuf.blocklens);
        if (mem_flatbuf.indices) H5MM_free(mem_flatbuf.indices);
        if (mem_flatbuf.blocklens) H5MM_free(mem_flatbuf.blocklens);

    }
    else {
        /*
         * Not collective IO, just do MPI_File_write_at - don't support this for now
         */
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "H5FD_MPIO_COLLECTIVE xfer mode required for custom aggregation")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_custom_read
 *
 * Purpose:	Reads data from a file flatbuf into a memory flatbuf. The memory
 *		and file flatbuf structures are defined using the H5S_<*>_get_seq_list
 *		functions, where <*> depends on the type of selection: all, points,
 *		hyperslab, or none.
 *		Note that this function is called from H5FD_select_read(), and is
 *		used to call optimized "read" routines defined in the "custom-collective
 *		IO virtual file layer" (CCIO) of the MPIO-VFD (see H5FDmpio_ccio.c).
 *
 * Return:
 *
 * Programmer:	Rick Zamora, 2018-07-10
 *
 *
 * Modifications:
 *		Rick Zamora, 2018-11-06
 *		cleanup and refactoring.
 *
 *-------------------------------------------------------------------------
 */
static herr_t H5FD_mpio_custom_read(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id,
        hid_t file_space, hid_t mem_space, size_t elmt_size, haddr_t addr, void *buf)
{

    H5FD_mpio_t                 *file = (H5FD_mpio_t*)_file;
    MPI_Offset                  mpi_off;
    MPI_Status                  mpi_stat;       /* Status from I/O operation */
    H5S_t                       *file_space_stype;
    int                         file_space_ref_count;
    H5S_t                       *mem_space_stype;
    int                         mem_space_ref_count;
    int                         size_i;
    H5P_genplist_t              *plist = NULL;  /* Property list pointer */
    H5FD_mpio_xfer_t            xfer_mode;      /* I/O tranfer mode */
    herr_t                      ret_value = SUCCEED;
    H5S_flatbuf_t               file_flatbuf;
    H5S_flatbuf_t               mem_flatbuf;
    hbool_t                     is_permuted = FALSE;
    hbool_t                     is_regular = TRUE;
    H5S_sel_iter_t              sel_iter;
    H5S_class_t                 file_space_extent_type;
    H5S_class_t                 mem_space_extent_type;
    H5S_sel_type                file_space_sel_type;
    H5S_sel_type                mem_space_sel_type;
    herr_t                      rc = 0;
    hsize_t                     *permute_map = NULL;

    /* Note: permute_map array holds the mapping from the old (out-of-order)
     * displacements to the in-order displacements of the H5S_flatbuf_t of the
     * point selection of the file space.
     */

    FUNC_ENTER_NOAPI_NOINIT

    /* File and memory space setup */
    file_space_stype = (H5S_t *) H5I_remove(file_space);
    file_space_ref_count = H5I_dec_ref(file_space);
    mem_space_stype = (H5S_t *) H5I_remove(mem_space);
    mem_space_ref_count = H5I_dec_ref(mem_space);

    /* some numeric conversions */
    if(H5FD_mpi_haddr_to_MPIOff(addr, &mpi_off) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADRANGE, FAIL, "can't convert from haddr to MPI off")

    size_i = (int)elmt_size;
    if((hsize_t)size_i != elmt_size)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADRANGE, FAIL, "can't convert from elmt_size to size_i")

    HDassert(file);
    HDassert(H5FD_MPIO==file->pub.driver_id);

    /* Make certain we have the correct type of property list */
    HDassert(H5I_GENPROP_LST==H5I_get_type(dxpl_id));
    HDassert(TRUE==H5P_isa_class(dxpl_id,H5P_DATASET_XFER));
    HDassert(buf);

    /* Portably initialize MPI status variable */
    HDmemset(&mpi_stat, 0, sizeof(MPI_Status));

    /*
     * Create flatbuf for FILE space selection
     */

    if(H5S_select_iter_init(&sel_iter, file_space_stype, elmt_size) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to initialize selection iterator")

    rc = H5S_mpio_return_space_extent_and_select_type(file_space_stype, &is_permuted, &is_regular, &file_space_extent_type, &file_space_sel_type);

    if(is_permuted)
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "permuted space selections not supported")

    /* Currently, file_space_extent_type must be H5S_NULL, H5S_SCALAR, or H5S_SIMPLE */
    if (!((file_space_extent_type == H5S_NULL) || (file_space_extent_type == H5S_SCALAR) || (file_space_extent_type == H5S_SIMPLE)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "file space extent type invalid")

    if(H5S_SELECT_ITER_RELEASE(&sel_iter) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release selection iterator")

    if ((file_space_sel_type == H5S_SEL_NONE) || (file_space_sel_type == H5S_SEL_ALL) ||
      (file_space_sel_type == H5S_SEL_POINTS) || (file_space_sel_type == H5S_SEL_HYPERSLABS)) {
        if( H5FD_mpio_setup_flatbuf( file_space_sel_type, &file_flatbuf, &sel_iter, file_space_stype, elmt_size, is_regular ) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "Call to H5FD_mpio_setup_flatbuf failed for FILE")
    }
    else {
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "Space selection type not recognized")
    }

    /*
     * Create flatbuf for MEMORY space selection
     */

    if(H5S_select_iter_init(&sel_iter, mem_space_stype, elmt_size) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to initialize selection iterator")

    rc = H5S_mpio_return_space_extent_and_select_type(mem_space_stype, &is_permuted, &is_regular, &mem_space_extent_type, &mem_space_sel_type);

    if(is_permuted)
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "permuted space selections not supported")

    /* Currently, mem_space_extent_type must be H5S_NULL, H5S_SCALAR, or H5S_SIMPLE */
    if (!((mem_space_extent_type == H5S_NULL) || (mem_space_extent_type == H5S_SCALAR) || (mem_space_extent_type == H5S_SIMPLE)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "space extent type invalid")

    if ((mem_space_sel_type == H5S_SEL_NONE) || (mem_space_sel_type == H5S_SEL_ALL) ||
      (mem_space_sel_type == H5S_SEL_POINTS) || (mem_space_sel_type == H5S_SEL_HYPERSLABS)) {
        if( H5FD_mpio_setup_flatbuf( mem_space_sel_type, &mem_flatbuf, &sel_iter, mem_space_stype, elmt_size, is_regular ) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "Call to H5FD_mpio_setup_flatbuf failed for MEM")
    }
    else {
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "Space selection type not recognized")
    }

    if(H5S_SELECT_ITER_RELEASE(&sel_iter) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release selection iterator")

    /* Obtain the data transfer properties */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    /* get the transfer mode from the dxpl */
    if(H5P_get(plist, H5D_XFER_IO_XFER_MODE_NAME, &xfer_mode)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get MPI-I/O transfer mode")

    /*
     * If using collective IO call the custom agggregation algorithm here.
     */
    if(xfer_mode == H5FD_MPIO_COLLECTIVE) {

      int error_code;
      H5FD_mpio_ccio_read_one_sided((CustomAgg_FH_Data)&(file->custom_agg_data), buf, mpi_off, &mem_flatbuf, &file_flatbuf, &error_code);
      if (file_flatbuf.indices) H5MM_free(file_flatbuf.indices);
      if (file_flatbuf.blocklens) H5MM_free(file_flatbuf.blocklens);
      if (mem_flatbuf.indices) H5MM_free(mem_flatbuf.indices);
      if (mem_flatbuf.blocklens) H5MM_free(mem_flatbuf.blocklens);

    }
    else {
      /*
       * Not collective IO, just do MPI_File_write_at - don't support this for now
       */
       HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "H5FD_MPIO_COLLECTIVE xfer mode required for custom aggregation")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5FD_mpio_flush
 *
 * Purpose:     Makes sure that all data is on disk.  This is collective.
 *
 * Return:      Success:	Non-negative
 *
 * 		Failure:	Negative
 *
 * Programmer:  Robb Matzke
 *              January 30, 1998
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpio_flush(H5FD_t *_file, hid_t H5_ATTR_UNUSED dxpl_id, hbool_t closing)
{
    H5FD_mpio_t		*file = (H5FD_mpio_t*)_file;
    int			mpi_code;	/* mpi return code */
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef H5FDmpio_DEBUG
    if(H5FD_mpio_Debug[(int)'t'])
    	HDfprintf(stdout, "Entering %s\n", FUNC);
#endif
    HDassert(file);
    HDassert(H5FD_MPIO == file->pub.driver_id);

    /* Only sync the file if we are not going to immediately close it */
    if(!closing)
        if(MPI_SUCCESS != (mpi_code = MPI_File_sync(file->f)))
            HMPI_GOTO_ERROR(FAIL, "MPI_File_sync failed", mpi_code)

done:
#ifdef H5FDmpio_DEBUG
    if(H5FD_mpio_Debug[(int)'t'])
    	HDfprintf(stdout, "Leaving %s\n", FUNC);
#endif

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_mpio_flush() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_mpio_truncate
 *
 * Purpose:     Make certain the file's size matches it's allocated size
 *
 *              This is a little sticky in the mpio case, as it is not
 *              easy for us to track the current EOF by extracting it from
 *              write calls.
 *
 *              Instead, we first check to see if the eoa has changed since
 *              the last call to this function.  If it has, we call
 *              MPI_File_get_size() to determine the current EOF, and
 *              only call MPI_File_set_size() if this value disagrees
 *              with the current eoa.
 *
 * Return:      Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:  Quincey Koziol
 *              January 31, 2008
 *
 * Changes:     Heavily reworked to avoid unnecessary MPI_File_set_size()
 *              calls.  The hope is that these calls are superfluous in the
 *              typical case, allowing us to avoid truncates most of the
 *              time.
 *
 *              The basic idea is to query the file system to get the
 *              current eof, and only truncate if the file systems
 *              conception of the eof disagrees with our eoa.
 *
 *                                                 JRM -- 10/27/17
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpio_truncate(H5FD_t *_file, hid_t H5_ATTR_UNUSED dxpl_id, hbool_t H5_ATTR_UNUSED closing)
{
    H5FD_mpio_t		*file = (H5FD_mpio_t*)_file;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

#ifdef H5FDmpio_DEBUG
    if(H5FD_mpio_Debug[(int)'t'])
    	HDfprintf(stdout, "Entering %s\n", FUNC);
#endif
    HDassert(file);
    HDassert(H5FD_MPIO == file->pub.driver_id);

    if(!H5F_addr_eq(file->eoa, file->last_eoa)) {
        int             mpi_code;       /* mpi return code */
        MPI_Offset      size;
        MPI_Offset      needed_eof;

        /* In principle, it is possible for the size returned by the
         * call to MPI_File_get_size() to depend on whether writes from
         * all proceeses have completed at the time process 0 makes the
         * call.
         *
         * In practice, most (all?) truncate calls will come after a barrier
         * and with no interviening writes to the file (with the possible
         * exception of sueprblock / superblock extension message updates).
         *
         * Check the "MPI file closing" flag in the API context to determine
         * if we can skip the barrier.
         */
        if(!H5CX_get_mpi_file_flushing())
            if(MPI_SUCCESS != (mpi_code = MPI_Barrier(file->comm)))
                HMPI_GOTO_ERROR(FAIL, "MPI_Barrier failed", mpi_code)

        /* Only processor p0 will get the filesize and broadcast it. */
        /* (Note that throwing an error here will cause non-rank 0 processes
         *      to hang in following Bcast.  -QAK, 3/17/2018)
         */
        if(0 == file->mpi_rank)
            if(MPI_SUCCESS != (mpi_code = MPI_File_get_size(file->f, &size)))
                HMPI_GOTO_ERROR(FAIL, "MPI_File_get_size failed", mpi_code)

        /* Broadcast file size */
        if(MPI_SUCCESS != (mpi_code = MPI_Bcast(&size, (int)sizeof(MPI_Offset), MPI_BYTE, 0, file->comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Bcast failed", mpi_code)

        if(H5FD_mpi_haddr_to_MPIOff(file->eoa, &needed_eof) < 0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_BADRANGE, FAIL, "cannot convert from haddr_t to MPI_Offset")

        /* eoa != eof.  Set eof to eoa */
        if(size != needed_eof) {
            /* Extend the file's size */
            if(MPI_SUCCESS != (mpi_code = MPI_File_set_size(file->f, needed_eof)))
                HMPI_GOTO_ERROR(FAIL, "MPI_File_set_size failed", mpi_code)

            /* In general, we must wait until all processes have finished
             * the truncate before any process can continue, since it is
             * possible that a process would write at the end of the
             * file, and this write would be discarded by the truncate.
             *
             * While this is an issue for a user initiated flush, it may
             * not be an issue at file close.  If so, we may be able to
             * optimize out the following barrier in that case.
             */
            if(MPI_SUCCESS != (mpi_code = MPI_Barrier(file->comm)))
                HMPI_GOTO_ERROR(FAIL, "MPI_Barrier failed", mpi_code)
        } /* end if */

        /* Update the 'last' eoa value */
        file->last_eoa = file->eoa;
    } /* end if */

done:
#ifdef H5FDmpio_DEBUG
    if(H5FD_mpio_Debug[(int)'t'])
    	HDfprintf(stdout, "Leaving %s\n", FUNC);
#endif

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_mpio_truncate() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_mpi_rank
 *
 * Purpose:	Returns the MPI rank for a process
 *
 * Return:	Success: non-negative
 *		Failure: negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, May 16, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
H5FD_mpio_mpi_rank(const H5FD_t *_file)
{
    const H5FD_mpio_t	*file = (const H5FD_mpio_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(file);
    HDassert(H5FD_MPIO==file->pub.driver_id);

    FUNC_LEAVE_NOAPI(file->mpi_rank)
} /* end H5FD_mpio_mpi_rank() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_mpi_size
 *
 * Purpose:	Returns the number of MPI processes
 *
 * Return:	Success: non-negative
 *		Failure: negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, May 16, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
H5FD_mpio_mpi_size(const H5FD_t *_file)
{
    const H5FD_mpio_t	*file = (const H5FD_mpio_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(file);
    HDassert(H5FD_MPIO==file->pub.driver_id);

    FUNC_LEAVE_NOAPI(file->mpi_size)
} /* end H5FD_mpio_mpi_size() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_communicator
 *
 * Purpose:	Returns the MPI communicator for the file.
 *
 * Return:	Success:	The communicator
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Monday, August  9, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static MPI_Comm
H5FD_mpio_communicator(const H5FD_t *_file)
{
    const H5FD_mpio_t	*file = (const H5FD_mpio_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(file);
    HDassert(H5FD_MPIO==file->pub.driver_id);

    FUNC_LEAVE_NOAPI(file->comm)
} /* end H5FD_mpio_communicator() */

/*-------------------------------------------------------------------------
 * Function:    HDF5_ccio_win_setup
 *
 * Purpose:     Function to setup one-sided communication structures.
 *
 * Return:      MPI_SUCCESS on success.
 *
 * Note:        This function must be called in mpio_open
 *
 *-------------------------------------------------------------------------
 */
int HDF5_ccio_win_setup(CustomAgg_FH_Data ca_data, int procs) {

    int ret = MPI_SUCCESS;
    ret = MPI_Win_create(ca_data->io_buf,ca_data->cb_buffer_size,1,MPI_INFO_NULL,ca_data->comm, &(ca_data->io_buf_window));
#ifdef onesidedtrace
    printf("CREATING ca_data->io_buf_window %016lx - ret = %d.\n",ca_data->io_buf_window,ret);
#endif
    if (ret != MPI_SUCCESS) goto fn_exit;
    ca_data->io_buf_put_amounts = 0;
    ret =MPI_Win_create(&(ca_data->io_buf_put_amounts),sizeof(int),sizeof(int),MPI_INFO_NULL,ca_data->comm, &(ca_data->io_buf_put_amounts_window));

    if (ca_data->async_io_outer == 1) {
        ret = MPI_Win_create(ca_data->io_buf_d,ca_data->cb_buffer_size,1,MPI_INFO_NULL,ca_data->comm, &(ca_data->io_buf_window_d));
        if (ret != MPI_SUCCESS) goto fn_exit;
        ca_data->io_buf_put_amounts_d = 0;
        ret = MPI_Win_create(&(ca_data->io_buf_put_amounts_d),sizeof(int),sizeof(int),MPI_INFO_NULL,ca_data->comm, &(ca_data->io_buf_put_amounts_window_d));
    }

fn_exit:
  return ret;
}

/*-------------------------------------------------------------------------
 * Function:    H5FD_mpio_ccio_setup
 *
 * Purpose:     Checks if CCIO VFD options are desired, and popluates
 *              necessary data structures.
 *
 * Return:      Success:
 *              Failure:        NULL
 *
 * Programmer:	Paul Coffman & Rick Zamora
 *              June 13, 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t H5FD_mpio_ccio_setup(const char *name, H5FD_mpio_t *file, MPI_File fh)
{
    char *do_custom_agg_rd = HDgetenv("HDF5_CCIO_RD");
    char *do_custom_agg_wr = HDgetenv("HDF5_CCIO_WR");
    char *cb_buffer_size = HDgetenv("HDF5_CCIO_CB_SIZE");
    char *cb_nodes = HDgetenv("HDF5_CCIO_CB_NODES");
    char *fs_block_size = HDgetenv("HDF5_CCIO_FS_BLOCK_SIZE");
    char *fs_block_count = HDgetenv("HDF5_CCIO_FS_BLOCK_COUNT");
    char *custom_agg_debug_str = HDgetenv("HDF5_CCIO_DEBUG");
    char *ccio_wr_method = HDgetenv("HDF5_CCIO_WR_METHOD");
    char *ccio_rd_method = HDgetenv("HDF5_CCIO_RD_METHOD");
    char *do_async_io = HDgetenv("HDF5_CCIO_ASYNC");
    char *set_cb_nodes_stride = HDgetenv("HDF5_CCIO_CB_STRIDE");
    char *use_file_system = HDgetenv("HDF5_CCIO_FS");
    char *do_topo_select = HDgetenv("HDF5_CCIO_TOPO_CB_SELECT");
    char *set_ppn = HDgetenv("HDF5_CCIO_TOPO_PPN");
    char *set_pps = HDgetenv("HDF5_CCIO_TOPO_PPS");
    char *use_fd_agg = HDgetenv("HDF5_CCIO_FD_AGG");
    int custom_agg_debug = 0;
    int mpi_rank = file->mpi_rank;       /* MPI rank of this process */
    int mpi_size = file->mpi_size;       /* Total number of MPI processes */
    int i, rc;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT
    {}

    if (custom_agg_debug_str && (strcmp(custom_agg_debug_str,"yes") == 0))
        custom_agg_debug = 1;

    if (use_fd_agg && (strcmp(use_fd_agg,"yes") == 0))
        file->custom_agg_data.fslayout = GPFS;
    else
        file->custom_agg_data.fslayout = LUSTRE;

    /* Set some defaults for the one-sided agg algorithm */
    file->custom_agg_data.ccio_read = 0;
    file->custom_agg_data.ccio_write = 0;
    file->custom_agg_data.cb_nodes = 1;
    file->custom_agg_data.ppn = 0;
    file->custom_agg_data.pps = 0;
    file->custom_agg_data.cb_buffer_size = 1048576;
    file->custom_agg_data.fs_block_count = 1;
    file->custom_agg_data.fs_block_size = 1048576;
    file->custom_agg_data.onesided_always_rmw = 0;
    file->custom_agg_data.onesided_no_rmw = 1;
    file->custom_agg_data.onesided_inform_rmw = 0;
    file->custom_agg_data.onesided_write_aggmethod = 1;
    file->custom_agg_data.onesided_read_aggmethod = 1;
    file->custom_agg_data.topo_cb_select = DEFAULT;
    file->custom_agg_data.ranklist_populated = 0;

    if (do_custom_agg_wr && (strcmp(do_custom_agg_wr,"yes") == 0)) {
        file->custom_agg_data.ccio_write = 1;
    }
    if (do_custom_agg_rd && (strcmp(do_custom_agg_rd,"yes") == 0)) {
        file->custom_agg_data.ccio_read = 1;
    }

    /* Check if we are using CCIO Options*/
    if ( (file->custom_agg_data.ccio_read) || (file->custom_agg_data.ccio_write) ) {

        /* By default, use env variables for agg settings */
        if ( cb_nodes ) {
            file->custom_agg_data.cb_nodes = atoi( cb_nodes );
        }
        if ( set_ppn ) {
            file->custom_agg_data.ppn = atoi( set_ppn );
        }
        if ( set_pps ) {
            file->custom_agg_data.pps = atoi( set_pps );
        }
        if ( cb_buffer_size ) {
            file->custom_agg_data.cb_buffer_size = atoi( cb_buffer_size );
        }
        if ( fs_block_count ) {
            file->custom_agg_data.fs_block_count = atoi( fs_block_count );
        }
        if ( fs_block_size ) {
            file->custom_agg_data.fs_block_size = atoi( fs_block_size );
        }
        file->custom_agg_data.comm = file->comm;
        file->custom_agg_data.fh = fh;

        /* TODO: Can we handle multiple stripes per aggregator?
         * For now, just pretend like the stripe size is the same as the buffer size...
         */
        int stripes_per_cb_buf = file->custom_agg_data.cb_buffer_size / file->custom_agg_data.fs_block_size;
        if ( stripes_per_cb_buf > 1 ) {
            file->custom_agg_data.fs_block_size = file->custom_agg_data.cb_buffer_size;
            file->custom_agg_data.fs_block_count /= stripes_per_cb_buf;
        }

        int tot_cb_bufsize = (int)(file->custom_agg_data.cb_buffer_size);
        file->custom_agg_data.io_buf_put_amounts = 0;
        file->custom_agg_data.io_buf_window = MPI_WIN_NULL;
        file->custom_agg_data.io_buf_put_amounts_window = MPI_WIN_NULL;

        /* Determine IF and HOW asynchronous I/O will be performed */
        file->custom_agg_data.async_io_inner = 0;
        file->custom_agg_data.async_io_outer = 0;
        file->custom_agg_data.check_req = 0;
        file->custom_agg_data.pthread_io = 0;
        if (do_async_io && (strcmp(do_async_io,"yes") == 0)) {
            /* Allow 'outer' pipelining if this is LUSTRE-like mapping */
            if(file->custom_agg_data.fslayout == LUSTRE) {
                file->custom_agg_data.async_io_outer = 1;
                file->custom_agg_data.io_buf_d = (char *) H5MM_malloc(tot_cb_bufsize*sizeof(char));
            }
            /* Allow 'inner' pipelining if this is GPFS-like mapping */
            else {
                file->custom_agg_data.cb_buffer_size *= 2;
                tot_cb_bufsize = (int)(file->custom_agg_data.cb_buffer_size);
                file->custom_agg_data.async_io_inner = 1;
                file->custom_agg_data.pthread_io = 1; /* pthreads needed for current 'inner' approach */
            }
        }
        file->custom_agg_data.io_buf = (char *) H5MM_malloc(tot_cb_bufsize*sizeof(char));
        file->custom_agg_data.io_buf_put_amounts_d = 0;
        file->custom_agg_data.io_buf_window_d = MPI_WIN_NULL;
        file->custom_agg_data.io_buf_put_amounts_window_d = MPI_WIN_NULL;
        file->custom_agg_data.use_dup = 0;

        if ( ccio_wr_method ) {
            file->custom_agg_data.onesided_write_aggmethod = atoi( ccio_wr_method );
            if (file->custom_agg_data.onesided_write_aggmethod < 1)
                file->custom_agg_data.onesided_write_aggmethod = 1;
            if (file->custom_agg_data.onesided_write_aggmethod < 2)
                file->custom_agg_data.onesided_write_aggmethod = 2;
        }
        if ( ccio_rd_method ) {
            file->custom_agg_data.onesided_read_aggmethod = atoi( ccio_rd_method );
            if (file->custom_agg_data.onesided_read_aggmethod < 1)
                file->custom_agg_data.onesided_read_aggmethod = 1;
            if (file->custom_agg_data.onesided_read_aggmethod < 2)
                file->custom_agg_data.onesided_read_aggmethod = 2;
        }

        if (custom_agg_debug && (mpi_rank == 0)) {
            fprintf(stdout,"Custom aggregation info on mpio_open: MPI_MAX_INFO_VAL is %d H5FD_mpio_open fh is %016lx cb_buffer_size is %d cb_nodes is %d fs_block_count is %d fs_block_size is %d\n",MPI_MAX_INFO_VAL,fh,file->custom_agg_data.cb_buffer_size,file->custom_agg_data.cb_nodes,file->custom_agg_data.fs_block_count,file->custom_agg_data.fs_block_size);
            fflush(stdout);
        }

        /* Generate the initial ranklist using a constant stride between ranks */
        file->custom_agg_data.ranklist = (int *) H5MM_malloc(mpi_size * sizeof(int));
        for (i=0;i<mpi_size;i++)
            file->custom_agg_data.ranklist[i] = i;
        int cb_nodes_stride = mpi_size / file->custom_agg_data.cb_nodes;

        /* If HDF5_CCIO_CB_STRIDE is set to a reasonable value, use it */
        if (set_cb_nodes_stride) {
          int set_stride_val = atoi( set_cb_nodes_stride );
          if ((set_stride_val > 0) && (set_stride_val <= cb_nodes_stride)) {
              cb_nodes_stride = set_stride_val;
          }
        }
        for (i=0;i<(file->custom_agg_data.cb_nodes);i++) {
            file->custom_agg_data.ranklist[i] = i*cb_nodes_stride;
        }

        /*
         * Here, we can check the HDF5_CCIO_TOPO_CB_SELECT env variable.
         * Use string to set AGGSelect custom_agg_data value...
         */
        if (do_topo_select) {
            if (strcmp(do_topo_select,"data") == 0) {
                file->custom_agg_data.topo_cb_select = DATA;
            } else if (strcmp(do_topo_select,"spread") == 0) {
                file->custom_agg_data.topo_cb_select = SPREAD;
            } else if (strcmp(do_topo_select,"strided") == 0) {
                /* Stride not really supported through topology API,
                 * Just use the strided rank list created above.
                 */
                file->custom_agg_data.topo_cb_select = DEFAULT;
            } else if (strcmp(do_topo_select,"random") == 0) {
                file->custom_agg_data.topo_cb_select = RANDOM;
            }
        }

        /* Show the aggregator ranks if we are in debug mode */
        if (custom_agg_debug && (mpi_rank == 0)) {
            fprintf(stdout,"DEBUG: file->custom_agg_data.cb_nodes is now set to %d romio_aggregator_list is:", file->custom_agg_data.cb_nodes);
            for (i=0;i<file->custom_agg_data.cb_nodes;i++)
                fprintf(stdout," %d",file->custom_agg_data.ranklist[i]);
            fprintf(stdout,"\n");
            fflush(stdout);
        }

    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD_mpio_ccio_setup */

/*-------------------------------------------------------------------------
 * Function:    H5FD_mpio_ccio_cleanup
 *
 * Purpose:     Cleans data structures used for CCIO VFD options
 *
 * Return:      Success:
 *              Failure:        NULL
 *
 * Programmer:	Rick Zamora
 *              October 25, 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t H5FD_mpio_ccio_cleanup(const H5FD_mpio_t *file)
{
    herr_t ret_value = SUCCEED;
    FUNC_ENTER_NOAPI_NOINIT
    {}

    /*
     * If doing custom aggregation, clean it up.
     */
    char *do_custom_agg_wr = HDgetenv("HDF5_CCIO_WR");
    char *do_custom_agg_rd = HDgetenv("HDF5_CCIO_RD");
    if ( (do_custom_agg_wr && (strcmp(do_custom_agg_wr,"yes") == 0)) ||
         (do_custom_agg_rd && (strcmp(do_custom_agg_rd,"yes") == 0)) ) {

        CustomAgg_FH_Data ca_data = (CustomAgg_FH_Data)&(file->custom_agg_data);
        if (ca_data->io_buf_window != MPI_WIN_NULL)
            ret_value = MPI_Win_free(&ca_data->io_buf_window);
        if (ca_data->io_buf_put_amounts_window != MPI_WIN_NULL)
            ret_value = MPI_Win_free(&ca_data->io_buf_put_amounts_window);
        if (ca_data->io_buf_window_d != MPI_WIN_NULL)
            ret_value = MPI_Win_free(&ca_data->io_buf_window_d);
        if (ca_data->io_buf_put_amounts_window_d != MPI_WIN_NULL)
            ret_value = MPI_Win_free(&ca_data->io_buf_put_amounts_window_d);

        H5MM_free(file->custom_agg_data.io_buf);
        H5MM_free(file->custom_agg_data.ranklist);
        if(file->custom_agg_data.async_io_outer)
            H5MM_free(file->custom_agg_data.io_buf_d);

    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD_mpio_ccio_cleanup */


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpio_calc_offset_list
 *
 * Purpose:     Function to get the offset list of absolute file offsets
 *              and associated lengths.
 *
 * Return:      Void.
 *
 *-------------------------------------------------------------------------
 */
void H5FD_mpio_calc_offset_list(ADIO_Offset_CA
    memFlatBufSize, H5S_flatbuf_t *fileFlatBuf, MPI_Offset mpi_off,
    ADIO_Offset_CA **offset_list_ptr, ADIO_Offset_CA
    **len_list_ptr, ADIO_Offset_CA *start_offset_ptr,
    ADIO_Offset_CA *end_offset_ptr, int
    *contig_access_count_ptr)
{
    int i, j, k;
    int st_index=0;
    int contig_access_count;
    ADIO_Offset_CA *len_list;
    ADIO_Offset_CA *offset_list;

    /* For the data in the memFlatBuf for this process, calculate the list of offsets and
    lengths in the file and determine the start and end offsets using the fileFlatBuf */

    if ( !(fileFlatBuf->size) || (memFlatBufSize == 0) ) {
        *contig_access_count_ptr = 0;
        *offset_list_ptr = (ADIO_Offset_CA *) H5MM_malloc(sizeof(ADIO_Offset_CA));
        *len_list_ptr = (ADIO_Offset_CA *) H5MM_malloc(sizeof(ADIO_Offset_CA));

        offset_list = *offset_list_ptr;
        len_list = *len_list_ptr;
        offset_list[0] = 0;
        len_list[0] = 0;
        *start_offset_ptr = 0;
        *end_offset_ptr = -1;
        return;
    }
    else {
        /* first count how many entries we will need to malloc correct amount of memory*/
        ADIO_Offset_CA bytesRemaining = memFlatBufSize;
        int fbindex = 0;
        int contig_access_count = 0;
        while (bytesRemaining > 0) {
            contig_access_count++;
            bytesRemaining -= fileFlatBuf->blocklens[fbindex++];
        }
#ifdef onesidedtrace
        printf("memFlatBufSize is %ld contig_access_count is %d\n",memFlatBufSize,contig_access_count);
        fflush(stdout);
#endif
        *offset_list_ptr = (ADIO_Offset_CA *) H5MM_malloc(contig_access_count*sizeof(ADIO_Offset_CA));
        *len_list_ptr = (ADIO_Offset_CA *) H5MM_malloc(contig_access_count*sizeof(ADIO_Offset_CA));
        offset_list = *offset_list_ptr;
        len_list = *len_list_ptr;

        /* now set the offset and len list */
        bytesRemaining = memFlatBufSize;
        fbindex = 0;
        int offlenindex = 0;
        while (bytesRemaining > 0) {
            if (fileFlatBuf->blocklens[fbindex] <= bytesRemaining) {
                offset_list[offlenindex] = fileFlatBuf->indices[fbindex] + mpi_off;
                len_list[offlenindex] = fileFlatBuf->blocklens[fbindex];
            }
            else {
                offset_list[offlenindex] = fileFlatBuf->indices[fbindex] + mpi_off;
                len_list[offlenindex] = bytesRemaining;
            }
            bytesRemaining -= fileFlatBuf->blocklens[fbindex];
            fbindex++;
            offlenindex++;

        }
        *contig_access_count_ptr = contig_access_count;
        *start_offset_ptr = offset_list[0];
        *end_offset_ptr = offset_list[offlenindex-1] + len_list[offlenindex-1] - (ADIO_Offset_CA)1;
    }
} /* H5FD_mpio_calc_offset_list */


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpio_ccio_write_one_sided
 *
 * Purpose:     Generic One-sided Collective Write Implementation
 *
 * Return:      Void.
 *
 *-------------------------------------------------------------------------
 */
void H5FD_mpio_ccio_write_one_sided(CustomAgg_FH_Data ca_data, const void *buf, MPI_Offset mpi_off,
    H5S_flatbuf_t *memFlatBuf, H5S_flatbuf_t *fileFlatBuf, int *error_code)
{
    /*
     * This function writes a memFlatBuf into a fileFlatBuf
     */

    int i, nprocs, myrank;
    int contig_access_count = 0;
    ADIO_Offset_CA start_offset, end_offset, fd_size, min_st_offset, off;
    ADIO_Offset_CA *offset_list = NULL, *st_offsets = NULL, *fd_start = NULL, *fd_end = NULL, *end_offsets = NULL;
    ADIO_Offset_CA *len_list = NULL;
    int *fs_block_info = NULL;
    ADIO_Offset_CA **buf_idx = NULL;
    int old_error, tmp_error;
    ADIO_Offset_CA *fs_offsets0, *fs_offsets, *count_sizes;

    MPI_Comm_size(ca_data->comm, &nprocs);
    MPI_Comm_rank(ca_data->comm, &myrank);

#ifdef topo_timing
    double endTimeTopo = 0.0;
    double startTimeTopo = 0.0;
    double endTime = 0.0;
    double startTime = 0.0;
    startTime = MPI_Wtime();
#endif

#ifdef onesidedtrace
    printf("Rank %d - H5FD_mpio_ccio_write_one_sided - ca_data->cb_nodes is %d\n",myrank,ca_data->cb_nodes);
    fflush(stdout);
    // dump the flatbufs
    printf("Rank %d - memFlatBuf->size is %ld fileFlatBuf->size is %ld mpi_off is %ld\n",myrank,memFlatBuf->size,fileFlatBuf->size,mpi_off);
    int flatbufCount = memFlatBuf->count;
    for (i=0;i<flatbufCount;i++) {
        printf("Rank %d - memFlatBuf->indices[%d] is %ld memFlatBuf->blocklens[%d] is %ld\n",myrank,i,memFlatBuf->indices[i],i,memFlatBuf->blocklens[i]);
    }
    flatbufCount = fileFlatBuf->count;
    for (i=0;i<flatbufCount;i++) {
        printf("Rank %d - fileFlatBuf->indices[%d] is %ld fileFlatBuf->blocklens[%d] is %ld\n",myrank,i,fileFlatBuf->indices[i],i,fileFlatBuf->blocklens[i]);
    }
    fflush(stdout);
#endif

    /* For this process's request, calculate the list of offsets and
     * lengths in the file and determine the start and end offsets.
     * Note: end_offset points to the last byte-offset that will be accessed.
     * e.g., if start_offset=0 and 100 bytes to be read, end_offset=99
     */

    H5FD_mpio_calc_offset_list((ADIO_Offset_CA)memFlatBuf->size, fileFlatBuf, mpi_off,
        &offset_list, &len_list, &start_offset, &end_offset, &contig_access_count);

#ifdef onesidedtrace
    printf("Rank %d - contig_access_count = %d\n",myrank,contig_access_count);
#endif

    /* each process communicates its start and end offsets to other
    * processes. The result is an array each of start and end offsets
    * stored in order of process rank.
    */
    st_offsets = (ADIO_Offset_CA *) H5MM_malloc(nprocs * sizeof(ADIO_Offset_CA));
    end_offsets = (ADIO_Offset_CA *) H5MM_malloc(nprocs * sizeof(ADIO_Offset_CA));

    /* One-sided aggregation needs the amount of data per rank as well because
    * the difference in starting and ending offsets for 1 byte is 0 the same
    * as 0 bytes so it cannot be distiguished.
    */
    count_sizes = (ADIO_Offset_CA *) H5MM_malloc(nprocs*sizeof(ADIO_Offset_CA));
    fs_offsets0 = (ADIO_Offset_CA *) H5MM_malloc(3*nprocs*sizeof(ADIO_Offset_CA));
    fs_offsets  = (ADIO_Offset_CA *) H5MM_malloc(3*nprocs*sizeof(ADIO_Offset_CA));
    for (i=0; i<nprocs; i++)  {
        fs_offsets0[i*3]   = 0;
        fs_offsets0[i*3+1] = 0;
        fs_offsets0[i*3+2] = 0;
    }
    fs_offsets0[myrank*3]   =   start_offset;
    fs_offsets0[myrank*3+1] =   end_offset;
    fs_offsets0[myrank*3+2] =   (ADIO_Offset_CA) memFlatBuf->size;
    MPI_Allreduce( fs_offsets0, fs_offsets, nprocs*3, MPI_LONG, MPI_MAX, ca_data->comm );
    for (i=0; i<nprocs; i++)  {
        st_offsets [i] = fs_offsets[i*3]  ;
        end_offsets[i] = fs_offsets[i*3+1];
        count_sizes[i] = fs_offsets[i*3+2];
    }

    H5MM_free( fs_offsets0 );
    H5MM_free( fs_offsets  );

    ADIO_Offset_CA lastFileOffset = 0, firstFileOffset = -1;
    int currentValidDataIndex = 0;
    /* Take out the 0-data offsets by shifting the indexes with data to the front
    * and keeping track of the valid data index for use as the length.
    */
    for (i=0; i<nprocs; i++) {
        if (count_sizes[i] > 0) {
            st_offsets[currentValidDataIndex] = st_offsets[i];
            end_offsets[currentValidDataIndex] = end_offsets[i];

            lastFileOffset = MAX(lastFileOffset,end_offsets[currentValidDataIndex]);
            if (firstFileOffset == -1)
            firstFileOffset = st_offsets[currentValidDataIndex];
            else
            firstFileOffset = MIN(firstFileOffset,st_offsets[currentValidDataIndex]);

            currentValidDataIndex++;
        }
    }
#ifdef onesidedtrace
    printf("Rank %d - H5FD_mpio_calc_offset_list results:\n",myrank);
    for (i=0;i<contig_access_count;i++) {
        printf("Rank %d - offset_list[%d] is %ld len_list[%d] is %ld start_offset is %ld end_offset is %ld firstFileOffset is %ld lastFileOffset is %ld\n",myrank,i,offset_list[i],i,len_list[i],start_offset,end_offset,firstFileOffset,lastFileOffset);
    }
#endif

    /* Select Topology-aware list of cb_nodes if desired */
    if ((ca_data->topo_cb_select != DEFAULT) && (ca_data->ranklist_populated==0)) {
#ifdef topo_timing
        startTimeTopo = MPI_Wtime();
#endif

        topology_aware_ranklist ( fileFlatBuf->blocklens, fileFlatBuf->indices, fileFlatBuf->count, &(ca_data->ranklist[0]), ca_data->cb_buffer_size, ca_data->cb_nodes, ca_data->ppn, ca_data->pps, 0, ca_data->comm, ca_data->topo_cb_select, (int)(ca_data->fslayout == GPFS) );

        /* Only populating ranklist when necessary */
        ca_data->ranklist_populated = 1;

#ifdef onesidedtrace
        if (myrank == 0) {
            fprintf(stdout,"Topology-aware CB Selection (type %d): ca_data->cb_nodes is %d, and ranklist is:", ca_data->topo_cb_select, ca_data->cb_nodes);
            for (i=0;i<ca_data->cb_nodes;i++)
                fprintf(stdout," %d",ca_data->ranklist[i]);
            fprintf(stdout,"\n");
        }
        MPI_Barrier(ca_data->comm);
#endif

#ifdef topo_timing
        endTimeTopo = MPI_Wtime();
#endif
    }

    /* Use GPFS-like mapping of aggregators to file data */
    if (ca_data->fslayout == GPFS) {

        calc_file_domains(st_offsets, end_offsets,
          currentValidDataIndex, ca_data->cb_nodes, &min_st_offset, &fd_start,
          &fd_end, &fd_size, ca_data->fs_block_size);

       /*
        * Pass this datastructure to indicate we are a non-striping filesystem
        * (by setting stripe size to 0).
        * That is, we are NOT using the LUSTRE approach here...
        */

        FS_Block_Parms noStripeParms;
        noStripeParms.stripeSize = 0;
        noStripeParms.segmentLen = 0;
        noStripeParms.stripesPerAgg = 0;
        noStripeParms.segmentIter = 0;
        noStripeParms.flushCB = 1;
        noStripeParms.stripedLastFileOffset = 0;
        noStripeParms.firstStripedIOCall = 0;
        noStripeParms.lastStripedIOCall = 0;
        noStripeParms.iWasUsedStripingAgg = 0;
        noStripeParms.numStripesUsed = 0;
        noStripeParms.amountOfStripedDataExpected = 0;
        noStripeParms.bufTypeExtent = 0;
        noStripeParms.lastDataTypeExtent = 0;
        noStripeParms.lastFlatBufIndice = 0;
        noStripeParms.lastIndiceOffset = 0;
        int holeFound = 0;

        H5FD_mpio_ccio_osagg_write(ca_data, offset_list, len_list, contig_access_count,
        buf, memFlatBuf, error_code, firstFileOffset, lastFileOffset,
        currentValidDataIndex, fd_start, fd_end, &holeFound, &noStripeParms);

        int anyHolesFound = 0;
        if (!(ca_data->onesided_no_rmw))
        MPI_Allreduce(&holeFound, &anyHolesFound, 1, MPI_INT, MPI_MAX, ca_data->comm);
        if (anyHolesFound == 0) {
            H5MM_free(offset_list);
            H5MM_free(len_list);
            H5MM_free(st_offsets);
            H5MM_free(end_offsets);
            H5MM_free(fd_start);
            H5MM_free(fd_end);
            H5MM_free(count_sizes);
        }
        else {
            /* Holes are found in the data and the user has not set
            * romio_onesided_no_rmw --- set romio_onesided_always_rmw to 1
            * and re-call ADIOI_OneSidedWriteAggregation and if the user has
            * romio_onesided_inform_rmw set then inform him of this condition
            * and behavior.
            */
            if (ca_data->onesided_inform_rmw && (myrank ==0)) {
                fprintf(stderr,"Information: Holes found during one-sided "
                "write aggregation algorithm --- re-running one-sided "
                "write aggregation with ROMIO_ONESIDED_ALWAYS_RMW set to 1.\n");
                ca_data->onesided_always_rmw = 1;
                int prev_onesided_no_rmw = ca_data->onesided_no_rmw;
                ca_data->onesided_no_rmw = 1;
                H5FD_mpio_ccio_osagg_write(ca_data, offset_list, len_list, contig_access_count,
                buf, memFlatBuf, error_code, firstFileOffset, lastFileOffset,
                currentValidDataIndex, fd_start, fd_end, &holeFound, &noStripeParms);
                ca_data->onesided_no_rmw = prev_onesided_no_rmw;
                H5MM_free(offset_list);
                H5MM_free(len_list);
                H5MM_free(st_offsets);
                H5MM_free(end_offsets);
                H5MM_free(fd_start);
                H5MM_free(fd_end);
                H5MM_free(count_sizes);
            }
        }

    }
    /* Use LUSTRE-like mapping of aggregators to file data */
    else {

        /* Rewriting the ca_data as 'fs_block_info' (probably NOT necessary) */
        fs_block_info = (int *) H5MM_malloc(3 * sizeof(int));
        fs_block_info[0] = ca_data->fs_block_size;
        fs_block_info[1] = ca_data->fs_block_count;
        fs_block_info[2] = ca_data->cb_nodes;
#ifdef onesidedtrace
        printf("Rank %d - ca_data->cb_buffer_size is %lu fs_block_info[0] is %d fs_block_info[1] is %d fs_block_info[2] is %d\n",myrank,ca_data->cb_buffer_size,fs_block_info[0],fs_block_info[1],fs_block_info[2]);
        fflush(stdout);
#endif

        /* Async I/O - Make sure we are starting with the main buffer */
        ca_data->use_dup = 0;

        /* Iterate over 1+ aggregation rounds and write to FS when buffers are full */
        H5FD_mpio_ccio_iterate_write(ca_data, buf, fs_block_info, offset_list, len_list, mpi_off, contig_access_count, currentValidDataIndex, start_offset, end_offset, firstFileOffset, lastFileOffset, memFlatBuf, fileFlatBuf, myrank, error_code);

        /* Async I/O - Wait for any outstanding requests (we are done with this I/O call) */
        ca_data->use_dup = 0;
        if (ca_data->check_req == 1) {
            MPIO_Wait(&ca_data->io_Request, error_code);
            ca_data->check_req = 0;
        }

        H5MM_free(offset_list);
        H5MM_free(len_list);
        H5MM_free(st_offsets);
        H5MM_free(end_offsets);
        H5MM_free(count_sizes);
        H5MM_free(fs_block_info);

    }

#ifdef topo_timing
    endTime = MPI_Wtime();
    double max_frac;
    double l_frac = (endTimeTopo - startTimeTopo) / (endTime - startTime);
    MPI_Allreduce ( &l_frac, &max_frac, 1, MPI_DOUBLE, MPI_MAX, ca_data->comm );
    if ((myrank == 0)&& (ca_data->topo_cb_select != DEFAULT)) {
        printf("WRITE: Aggregator Selection Fraction = %f\n", max_frac);
        fflush(stdout);
    }
    MPI_Barrier(ca_data->comm);
#endif

} /* H5FD_mpio_ccio_write_one_sided */

/*-------------------------------------------------------------------------
 * Function:    H5FD_mpio_ccio_read_one_sided
 *
 * Purpose:     Generic One-sided Collective Read Implementation
 *
 * Return:      Void.
 *
 *-------------------------------------------------------------------------
 */
 void H5FD_mpio_ccio_read_one_sided(CustomAgg_FH_Data ca_data, void *buf, MPI_Offset mpi_off,
    H5S_flatbuf_t *memFlatBuf, H5S_flatbuf_t *fileFlatBuf,
    int *error_code)
{
    /*
     * This function reads a fileFlatBuf into a memFlatBuf
     */

    int i, ii, nprocs, nprocs_for_coll, myrank;
    int contig_access_count=0;
    ADIO_Offset_CA start_offset, end_offset, fd_size, min_st_offset, off;
    ADIO_Offset_CA *offset_list = NULL, *st_offsets = NULL, *fd_start = NULL,
    *fd_end = NULL, *end_offsets = NULL;
    ADIO_Offset_CA *len_list = NULL;
    ADIO_Offset_CA *fs_offsets0 = NULL, *fs_offsets = NULL;
    ADIO_Offset_CA *count_sizes;
    int *fs_block_info = NULL;

    MPI_Comm_size(ca_data->comm, &nprocs);
    MPI_Comm_rank(ca_data->comm, &myrank);

#ifdef topo_timing
    double endTimeTopo = 0.0;
    double startTimeTopo = 0.0;
    double endTime = 0.0;
    double startTime = 0.0;
    startTime = MPI_Wtime();
#endif

#ifdef onesidedtrace
    printf("Rank %d - H5FD_mpio_ccio_read_one_sided - ca_data->cb_nodes is %d\n",myrank,ca_data->cb_nodes);
    fflush(stdout);

    /* dump the flatbufs */
    printf("Rank %d - memFlatBuf->size is %ld fileFlatBuf->size is %ld mpi_off is %ld\n",myrank,memFlatBuf->size,fileFlatBuf->size,mpi_off);
    int flatbufCount = memFlatBuf->count;
    for (i=0;i<flatbufCount;i++) {
        printf("Rank %d - memFlatBuf->indices[%d] is %ld memFlatBuf->blocklens[%d] is %ld\n",myrank,i,memFlatBuf->indices[i],i,memFlatBuf->blocklens[i]);
    }
    flatbufCount = fileFlatBuf->count;
    for (i=0;i<flatbufCount;i++) {
        printf("Rank %d - fileFlatBuf->indices[%d] is %ld fileFlatBuf->blocklens[%d] is %ld\n",myrank,i,fileFlatBuf->indices[i],i,fileFlatBuf->blocklens[i]);
    }
    fflush(stdout);
#endif

    /* For this process's request, calculate the list of offsets and
     * lengths in the file and determine the start and end offsets.
     * Note: end_offset points to the last byte-offset that will be accessed.
     * e.g., if start_offset=0 and 100 bytes to be read, end_offset=99
     */

    H5FD_mpio_calc_offset_list((ADIO_Offset_CA)memFlatBuf->size, fileFlatBuf, mpi_off,
        &offset_list, &len_list, &start_offset, &end_offset, &contig_access_count);

#ifdef onesidedtrace
    printf("Rank %d - contig_access_count = %d\n",myrank,contig_access_count);
#endif

    /* each process communicates its start and end offsets to other
    processes. The result is an array each of start and end offsets stored
    in order of process rank. */
    st_offsets = (ADIO_Offset_CA *) H5MM_malloc(nprocs*sizeof(ADIO_Offset_CA));
    end_offsets = (ADIO_Offset_CA *) H5MM_malloc(nprocs*sizeof(ADIO_Offset_CA));

    /* One-sided aggregation needs the amount of data per rank as well because
    * the difference in starting and ending offsets for 1 byte is 0 the same
    * as 0 bytes so it cannot be distiguished.
    */
    count_sizes = (ADIO_Offset_CA *) H5MM_malloc(nprocs*sizeof(ADIO_Offset_CA));
    fs_offsets0 = (ADIO_Offset_CA *) H5MM_malloc(3*nprocs*sizeof(ADIO_Offset_CA));
    fs_offsets  = (ADIO_Offset_CA *) H5MM_malloc(3*nprocs*sizeof(ADIO_Offset_CA));
    for (ii=0; ii<nprocs; ii++)  {
        fs_offsets0[ii*3]   = 0;
        fs_offsets0[ii*3+1] = 0;
        fs_offsets0[ii*3+2] = 0;
    }
    fs_offsets0[myrank*3]   = start_offset;
    fs_offsets0[myrank*3+1] =   end_offset;
    fs_offsets0[myrank*3+2] =   (ADIO_Offset_CA) memFlatBuf->size;
    MPI_Allreduce( fs_offsets0, fs_offsets, nprocs*3, MPI_LONG, MPI_MAX, ca_data->comm );
    for (ii=0; ii<nprocs; ii++)  {
        st_offsets [ii] = fs_offsets[ii*3]  ;
        end_offsets[ii] = fs_offsets[ii*3+1];
        count_sizes[ii] = fs_offsets[ii*3+2];
    }

    H5MM_free( fs_offsets0 );
    H5MM_free( fs_offsets  );

    ADIO_Offset_CA lastFileOffset = 0, firstFileOffset = -1;
    int currentNonZeroDataIndex = 0;
    /* Take out the 0-data offsets by shifting the indexes with data to the front
    * and keeping track of the valid data index for use as the length.
    */
    for (i=0; i<nprocs; i++) {
        if (count_sizes[i] > 0) {
            st_offsets[currentNonZeroDataIndex] = st_offsets[i];
            end_offsets[currentNonZeroDataIndex] = end_offsets[i];
            lastFileOffset = MAX(lastFileOffset,end_offsets[currentNonZeroDataIndex]);
            if (firstFileOffset == -1)
                firstFileOffset = st_offsets[currentNonZeroDataIndex];
            else
                firstFileOffset = MIN(firstFileOffset,st_offsets[currentNonZeroDataIndex]);
            currentNonZeroDataIndex++;
        }
    }

    /* Select Topology-aware list of cb_nodes if desired */
    if ((ca_data->topo_cb_select != DEFAULT) && (ca_data->ranklist_populated==0)) {
#ifdef topo_timing
    startTimeTopo = MPI_Wtime();
#endif

        topology_aware_ranklist ( fileFlatBuf->blocklens, fileFlatBuf->indices, fileFlatBuf->count, &(ca_data->ranklist[0]), ca_data->cb_buffer_size, ca_data->cb_nodes, ca_data->ppn, ca_data->pps, 0, ca_data->comm, ca_data->topo_cb_select, (int)(ca_data->fslayout == GPFS) );

        /* Only populating ranklist when necessary */
        ca_data->ranklist_populated = 1;

#ifdef onesidedtrace
        if (myrank == 0) {
            fprintf(stdout,"Topology-aware CB Selection: ca_data->cb_nodes is %d, and ranklist is:", ca_data->cb_nodes);
            for (i=0;i<ca_data->cb_nodes;i++)
                fprintf(stdout," %d",ca_data->ranklist[i]);
            fprintf(stdout,"\n");
        }
        MPI_Barrier(ca_data->comm);
#endif

#ifdef topo_timing
    endTimeTopo = MPI_Wtime();
#endif
    }

    /* Use LUSTRE-style data mapping to aggs */
    if (ca_data->fslayout == LUSTRE) {

        /* Rewriting the ca_data as 'fs_block_info' (probably NOT necessary) */
        fs_block_info = (int *) H5MM_malloc(3 * sizeof(int));
        fs_block_info[0] = ca_data->fs_block_size;
        fs_block_info[1] = ca_data->fs_block_count;
        fs_block_info[2] = ca_data->cb_nodes;
#ifdef onesidedtrace
        printf("Rank %d - ca_data->cb_buffer_size is %lu fs_block_info[0] is %d fs_block_info[1] is %d fs_block_info[2] is %d\n",myrank,ca_data->cb_buffer_size,fs_block_info[0],fs_block_info[1],fs_block_info[2]);
        fflush(stdout);
#endif

        /* Async I/O - Make sure we are starting with the main buffer */
        if (ca_data->check_req == 1) {
            MPIO_Wait(&ca_data->io_Request, error_code);
            ca_data->check_req = 0;
        }
        if (ca_data->check_req_d == 1) {
            MPIO_Wait(&ca_data->io_Request_d, error_code);
            ca_data->check_req_d = 0;
        }
        ca_data->use_dup = 0;

        /* Iterate over 1+ aggregation rounds and read to mem when buffers are full */
        H5FD_mpio_ccio_iterate_read(ca_data, buf, fs_block_info, offset_list, len_list, mpi_off, contig_access_count, currentNonZeroDataIndex, start_offset, end_offset, firstFileOffset, lastFileOffset, memFlatBuf, fileFlatBuf, myrank, error_code);

        /* Async I/O - Wait for any outstanding requests (we are done with this I/O call) */
        if (ca_data->check_req == 1) {
            MPIO_Wait(&ca_data->io_Request, error_code);
            ca_data->check_req = 0;
        }
        if (ca_data->check_req_d == 1) {
            MPIO_Wait(&ca_data->io_Request_d, error_code);
            ca_data->check_req_d = 0;
        }
        ca_data->use_dup = 0;

        H5MM_free(offset_list);
        H5MM_free(len_list);
        H5MM_free(st_offsets);
        H5MM_free(end_offsets);
        H5MM_free(count_sizes);
        H5MM_free(fs_block_info);

    }
    /* Use GPFS-style data mapping to aggs */
    else {

        calc_file_domains(st_offsets, end_offsets, currentNonZeroDataIndex, ca_data->cb_nodes, &min_st_offset, &fd_start, &fd_end, &fd_size, ca_data->fs_block_size);

        /* Indicate that this is NOT a striped file system.. */
        FS_Block_Parms noStripeParms;
        noStripeParms.stripeSize = 0;
        noStripeParms.segmentLen = 0;
        noStripeParms.stripesPerAgg = 0;
        noStripeParms.segmentIter = 0;
        noStripeParms.flushCB = 1;
        noStripeParms.stripedLastFileOffset = 0;
        noStripeParms.firstStripedIOCall = 0;
        noStripeParms.lastStripedIOCall = 0;
        noStripeParms.iWasUsedStripingAgg = 0;
        noStripeParms.numStripesUsed = 0;
        noStripeParms.amountOfStripedDataExpected = 0;
        noStripeParms.bufTypeExtent = 0;
        noStripeParms.lastDataTypeExtent = 0;
        noStripeParms.lastFlatBufIndice = 0;
        noStripeParms.lastIndiceOffset = 0;

        H5FD_mpio_ccio_osagg_read(ca_data, offset_list, len_list, contig_access_count, buf, memFlatBuf, error_code, firstFileOffset, lastFileOffset, currentNonZeroDataIndex, fd_start, fd_end, &noStripeParms, 1);
        // last '1' means you SHOULD be reading in H5FD_mpio_ccio_osagg_read.

        H5MM_free(offset_list);
        H5MM_free(len_list);
        H5MM_free(st_offsets);
        H5MM_free(end_offsets);
        H5MM_free(fd_start);
        H5MM_free(fd_end);
        H5MM_free(count_sizes);

    }

#ifdef topo_timing
    endTime = MPI_Wtime();
    double max_frac;
    double l_frac = (endTimeTopo - startTimeTopo)/(endTime - startTime);
    MPI_Allreduce ( &l_frac, &max_frac, 1, MPI_DOUBLE, MPI_MAX, ca_data->comm );
    if ((myrank == 0)&& (ca_data->topo_cb_select != DEFAULT)) {
        printf("READ: Aggregator Selection Fraction = %f\n", max_frac);
        fflush(stdout);
    }
    MPI_Barrier(ca_data->comm);
#endif

} /* H5FD_mpio_ccio_read_one_sided */

/*-------------------------------------------------------------------------
 * Function:    H5FD_mpio_ccio_iterate_write
 *
 * Purpose:     This function calls H5FD_mpio_ccio_osagg_write
 *              iteratively to essentially pack stripes of data into the
 *              collective buffer and then flushes the buffer to the file when
 *              fully packed (repeating this process until all the data is
 *              completely written to the file).
 *
 * Return:      Void.
 *
 *-------------------------------------------------------------------------
 */
void H5FD_mpio_ccio_iterate_write(CustomAgg_FH_Data ca_data, const void *buf,
    int *fs_block_info, ADIO_Offset_CA *offset_list, ADIO_Offset_CA *len_list,
    MPI_Offset mpi_off, int contig_access_count, int currentValidDataIndex,
    ADIO_Offset_CA start_offset, ADIO_Offset_CA end_offset,
    ADIO_Offset_CA firstFileOffset, ADIO_Offset_CA lastFileOffset,
    H5S_flatbuf_t *memFlatBuf, H5S_flatbuf_t *fileFlatBuf, int myrank, int *error_code)
{

    int i;
    int stripesPerAgg = ca_data->cb_buffer_size / fs_block_info[0];
    int numStripedAggs = ca_data->cb_nodes;
    if (stripesPerAgg == 0) {
        /* The striping unit is larger than the collective buffer size
        * therefore we must abort since the buffer has already been
        * allocated during the open.
        */
        fprintf(stderr,"Error: The collective buffer size %d is less "
        "than the fs_block_size %d - This collective I/O implementation "
        "cannot continue.\n",ca_data->cb_buffer_size,fs_block_info[0]);
        MPI_Abort(MPI_COMM_WORLD, 1);
    }
    MPI_Comm_rank(ca_data->comm, &myrank);

    /* Declare FS_Block_Parms here - these parameters will be locally managed
    * for this invokation of `iterate_one_sided`.  This will allow for concurrent
    * one-sided collective writes via multi-threading as well as multiple communicators.
    */
    FS_Block_Parms stripeParms;
    stripeParms.stripeSize = fs_block_info[0];
    stripeParms.stripedLastFileOffset = lastFileOffset;
    stripeParms.iWasUsedStripingAgg = 0;
    stripeParms.numStripesUsed = 0;
    stripeParms.amountOfStripedDataExpected = 0;
    stripeParms.bufTypeExtent = 0;
    stripeParms.lastDataTypeExtent = 0;
    stripeParms.lastFlatBufIndice = 0;
    stripeParms.lastIndiceOffset = 0;

    /* The general algorithm here is to divide the file up into segements, a segment
    * being defined as a contiguous region of the file which has up to one occurrence
    * of each stripe - the data for each stripe being written out by a particular
    * aggregator.  The segmentLen is the maximum size in bytes of each segment
    * (stripeSize*number of aggs).  Iteratively call H5FD_mpio_ccio_osagg_write
    * for each segment to aggregate the data to the collective buffers, but only do
    * the actual write (via flushCB stripe parm) once stripesPerAgg stripes
    * have been packed or the aggregation for all the data is complete, minimizing
    * synchronization.
    */
    stripeParms.segmentLen = ((ADIO_Offset_CA)numStripedAggs)*((ADIO_Offset_CA)(fs_block_info[0]));

    /* These arrays define the file offsets for the stripes for a given segment - similar
    * to the concept of file domains in GPFS, essentially file domeains for the segment.
    */
    ADIO_Offset_CA *segment_stripe_start = (ADIO_Offset_CA *) H5MM_malloc(numStripedAggs*sizeof(ADIO_Offset_CA));
    ADIO_Offset_CA *segment_stripe_end = (ADIO_Offset_CA *) H5MM_malloc(numStripedAggs*sizeof(ADIO_Offset_CA));

    /* Find the actual range of stripes in the file that have data in the offset
    * ranges being written -- skip holes at the front and back of the file.
    */
    int currentOffsetListIndex = 0;
    int fileSegmentIter = 0;
    int startingStripeWithData = 0;
    int foundStartingStripeWithData = 0;
    while (!foundStartingStripeWithData) {
        if ( ((startingStripeWithData+1) * (ADIO_Offset_CA)(fs_block_info[0])) > firstFileOffset)
            foundStartingStripeWithData = 1;
        else
            startingStripeWithData++;
    }

    ADIO_Offset_CA currentSegementOffset = (ADIO_Offset_CA)startingStripeWithData * (ADIO_Offset_CA)(fs_block_info[0]);

    int numSegments = (int) ((lastFileOffset+(ADIO_Offset_CA)1 - currentSegementOffset)/stripeParms.segmentLen);
    if ((lastFileOffset+(ADIO_Offset_CA)1 - currentSegementOffset)%stripeParms.segmentLen > 0)
        numSegments++;

#ifdef onesidedtrace
    printf("Rank %d - H5FD_mpio_ccio_iterate_write ca_data->cb_nodes is %d numStripedAggs is %d numSegments is %d start_offset is %ld end_offset is %ld firstFileOffset is %ld lastFileOffset is %ld\n",myrank,ca_data->cb_nodes,numStripedAggs,numSegments,start_offset,end_offset,firstFileOffset,lastFileOffset);
    fflush(stdout);
#endif

    /* To support read-modify-write use a while-loop to redo the aggregation if necessary
    * to fill in the holes.
    */
    int doAggregation = 1;
    int holeFound = 0;

    /* Remember onesided_no_rmw setting if we have to re-do
    * the aggregation if holes are found.
    */
    int prev_onesided_no_rmw = ca_data->onesided_no_rmw;

    while (doAggregation) {

        int totalDataWrittenLastRound = 0;

        /* This variable tracks how many segment stripes we have packed into the agg
        * buffers so we know when to flush to the file system.
        */
        stripeParms.segmentIter = 0;

        /* stripeParms.stripesPerAgg is the number of stripes to aggregate before doing a flush.
        */
        stripeParms.stripesPerAgg = stripesPerAgg;
        if (stripeParms.stripesPerAgg > numSegments)
            stripeParms.stripesPerAgg = numSegments;

        for (fileSegmentIter=0;fileSegmentIter < numSegments;fileSegmentIter++) {

            int dataWrittenThisRound = 0;

            /* Define the segment range in terms of file offsets.
            */
            ADIO_Offset_CA segmentFirstFileOffset = currentSegementOffset;
            if ((currentSegementOffset+stripeParms.segmentLen-(ADIO_Offset_CA)1) > lastFileOffset)
                currentSegementOffset = lastFileOffset;
            else
                currentSegementOffset += (stripeParms.segmentLen-(ADIO_Offset_CA)1);
            ADIO_Offset_CA segmentLastFileOffset = currentSegementOffset;
            currentSegementOffset++;

            ADIO_Offset_CA segment_stripe_offset = segmentFirstFileOffset;
            for (i=0;i<numStripedAggs;i++) {
                if (firstFileOffset > segment_stripe_offset)
                    segment_stripe_start[i] = firstFileOffset;
                else
                    segment_stripe_start[i] = segment_stripe_offset;
                if ((segment_stripe_offset + (ADIO_Offset_CA)(fs_block_info[0])) > lastFileOffset)
                    segment_stripe_end[i] = lastFileOffset;
                else
                    segment_stripe_end[i] = segment_stripe_offset + (ADIO_Offset_CA)(fs_block_info[0]) - (ADIO_Offset_CA)1;
                segment_stripe_offset += (ADIO_Offset_CA)(fs_block_info[0]);
            }

            /* In the interest of performance for non-contiguous data with large offset lists
            * essentially modify the given offset and length list appropriately for this segment
            * and then pass pointers to the sections of the lists being used for this segment
            * to H5FD_mpio_ccio_osagg_write.  Remember how we have modified the list for this
            * segment, and then restore it appropriately after processing for this segment has
            * concluded, so it is ready for the next segment.
            */
            int segmentContigAccessCount = 0;
            int startingOffsetListIndex = -1;
            int endingOffsetListIndex = -1;
            ADIO_Offset_CA startingOffsetAdvancement = 0;
            ADIO_Offset_CA startingLenTrim = 0;
            ADIO_Offset_CA endingLenTrim = 0;

            while (      ( ( offset_list[currentOffsetListIndex] + ((ADIO_Offset_CA)(len_list[currentOffsetListIndex]))-(ADIO_Offset_CA)1 ) < segmentFirstFileOffset)
                      && (currentOffsetListIndex < (contig_access_count-1) ) )
                currentOffsetListIndex++;
            startingOffsetListIndex = currentOffsetListIndex;
            endingOffsetListIndex = currentOffsetListIndex;
            int offsetInSegment = 0;
            ADIO_Offset_CA offsetStart = offset_list[currentOffsetListIndex];
            ADIO_Offset_CA offsetEnd = (offset_list[currentOffsetListIndex] + ((ADIO_Offset_CA)(len_list[currentOffsetListIndex]))-(ADIO_Offset_CA)1);

            if (len_list[currentOffsetListIndex] == 0)
                offsetInSegment = 0;
            else if ((offsetStart >= segmentFirstFileOffset) && (offsetStart <= segmentLastFileOffset)) {
                offsetInSegment = 1;
            }
            else if ((offsetEnd >= segmentFirstFileOffset) && (offsetEnd <= segmentLastFileOffset)) {
                offsetInSegment = 1;
            }
            else if ((offsetStart <= segmentFirstFileOffset) && (offsetEnd >= segmentLastFileOffset)) {
                offsetInSegment = 1;
            }

            if (!offsetInSegment) {
                segmentContigAccessCount = 0;
            }
            else {
                /* We are in the segment, advance currentOffsetListIndex until we are out of segment.
                */
                segmentContigAccessCount = 1;

                while ((offset_list[currentOffsetListIndex] <= segmentLastFileOffset) && (currentOffsetListIndex < contig_access_count)) {
                    dataWrittenThisRound += (int) len_list[currentOffsetListIndex];
                    currentOffsetListIndex++;
                }

                if (currentOffsetListIndex > startingOffsetListIndex) {
                    /* If we did advance, if we are at the end need to check if we are still in segment.
                    */
                    if (currentOffsetListIndex == contig_access_count) {
                        currentOffsetListIndex--;
                    }
                    else if (offset_list[currentOffsetListIndex] > segmentLastFileOffset) {
                        /* We advanced into the last one and it still in the segment.
                        */
                        currentOffsetListIndex--;
                    }
                    else {
                        dataWrittenThisRound += (int) len_list[currentOffsetListIndex];
                    }
                    segmentContigAccessCount += (currentOffsetListIndex-startingOffsetListIndex);
                    endingOffsetListIndex = currentOffsetListIndex;
                }
            }

            if (segmentContigAccessCount > 0) {
                /* Trim edges here so all data in the offset list range fits exactly in the segment.
                */
                if (offset_list[startingOffsetListIndex] < segmentFirstFileOffset) {
                    startingOffsetAdvancement = segmentFirstFileOffset-offset_list[startingOffsetListIndex];
                    offset_list[startingOffsetListIndex] += startingOffsetAdvancement;
                    dataWrittenThisRound -= (int) startingOffsetAdvancement;
                    startingLenTrim = startingOffsetAdvancement;
                    len_list[startingOffsetListIndex] -= startingLenTrim;
                }

                if ((offset_list[endingOffsetListIndex] + ((ADIO_Offset_CA)(len_list[endingOffsetListIndex]))-(ADIO_Offset_CA)1) > segmentLastFileOffset) {
                    endingLenTrim = offset_list[endingOffsetListIndex]+ ((ADIO_Offset_CA)(len_list[endingOffsetListIndex]))-(ADIO_Offset_CA)1 - segmentLastFileOffset;
                    len_list[endingOffsetListIndex] -= endingLenTrim;
                    dataWrittenThisRound -= (int) endingLenTrim;
                }
            }

            int holeFoundThisRound = 0;

            /* Once we have packed the collective buffers do the actual write.
            */
            if ((stripeParms.segmentIter == (stripeParms.stripesPerAgg-1)) || (fileSegmentIter == (numSegments-1))) {
                stripeParms.flushCB = 1;
            }
            else
                stripeParms.flushCB = 0;

            stripeParms.firstStripedIOCall = 0;
            stripeParms.lastStripedIOCall = 0;
            if (fileSegmentIter == 0) {
                stripeParms.firstStripedIOCall = 1;
            }
            else if (fileSegmentIter == (numSegments-1))
                stripeParms.lastStripedIOCall = 1;

            /* The difference in calls to H5FD_mpio_ccio_osagg_write is based on the whether the buftype is
            * contiguous.  The algorithm tracks the position in the source buffer when called
            * multiple times --  in the case of contiguous data this is simple and can be externalized with
            * a buffer offset, in the case of non-contiguous data this is complex and the state must be tracked
            * internally, therefore no external buffer offset.  Care was taken to minimize
            * H5FD_mpio_ccio_osagg_write changes at the expense of some added complexity to the caller.
            */

#ifdef onesidedtrace
            if (myrank == 0) {
                int j;
                printf("\n\nRank %d - Segment iteration %d stripeParms.flushCB is %d aggregator placement and assignment over %d aggs is:\n",myrank,fileSegmentIter,stripeParms.flushCB, ca_data->cb_nodes);
                for (j=0;j<ca_data->cb_nodes;j++)
                printf("Rank %d - agg rank %d writing to offset range %ld to %ld\n",myrank,ca_data->ranklist[j],segment_stripe_start[j],segment_stripe_end[j]);
                printf("\n\n");
            }
#endif

            if (memFlatBuf->count == 1) {
                H5FD_mpio_ccio_osagg_write(ca_data,(ADIO_Offset_CA*)&(offset_list[startingOffsetListIndex]), (ADIO_Offset_CA*)&(len_list[startingOffsetListIndex]), segmentContigAccessCount, buf+totalDataWrittenLastRound, memFlatBuf, error_code, segmentFirstFileOffset, segmentLastFileOffset, currentValidDataIndex, segment_stripe_start, segment_stripe_end, 0,&stripeParms);
            }
            else {
                H5FD_mpio_ccio_osagg_write(ca_data,(ADIO_Offset_CA*)&(offset_list[startingOffsetListIndex]), (ADIO_Offset_CA*)&(len_list[startingOffsetListIndex]), segmentContigAccessCount, buf, memFlatBuf, error_code, segmentFirstFileOffset, segmentLastFileOffset, currentValidDataIndex, segment_stripe_start, segment_stripe_end, 0,&stripeParms);
            }

            /* Async I/O - Switch between buffers */
            if(ca_data->async_io_outer) {
                ca_data->use_dup = (ca_data->use_dup + 1) % 2;
            }

            if (stripeParms.flushCB) {
                stripeParms.segmentIter = 0;
                if (stripesPerAgg > (numSegments-fileSegmentIter-1))
                stripeParms.stripesPerAgg = numSegments-fileSegmentIter-1;
                else
                stripeParms.stripesPerAgg = stripesPerAgg;
            }
            else
                stripeParms.segmentIter++;

            if (holeFoundThisRound)
                holeFound = 1;

            /* If we know we won't be doing a pre-read in a subsequent call to
            * H5FD_mpio_ccio_osagg_write which will have a barrier to keep
            * feeder ranks from doing rma to the collective buffer before the
            * write completes that we told it do with the stripeParms.flushCB
            * flag then we need to do a barrier here.
            */
            if (!ca_data->onesided_always_rmw && stripeParms.flushCB) {
                if (fileSegmentIter < (numSegments-1)) {
                    MPI_Barrier(ca_data->comm);
                }
            }

            /* Restore the offset_list and len_list to values that are ready for the
            * next iteration.
            */
            if (segmentContigAccessCount > 0) {
                offset_list[endingOffsetListIndex] += len_list[endingOffsetListIndex];
                len_list[endingOffsetListIndex] = endingLenTrim;
            }
            totalDataWrittenLastRound += dataWrittenThisRound;
        } /* fileSegmentIter for-loop */

        /* Check for holes in the data unless onesided_no_rmw is set.
        * If a hole is found redo the entire aggregation and write.
        */
        if (!ca_data->onesided_no_rmw) {
            int anyHolesFound = 0;
            MPI_Allreduce(&holeFound, &anyHolesFound, 1, MPI_INT, MPI_MAX, ca_data->comm);

            if (anyHolesFound) {
                H5MM_free(offset_list);
                H5MM_free(len_list);
                H5FD_mpio_calc_offset_list((ADIO_Offset_CA)memFlatBuf->size, fileFlatBuf, mpi_off,
                    &offset_list, &len_list, &start_offset, &end_offset, &contig_access_count);

                currentSegementOffset = (ADIO_Offset_CA)startingStripeWithData * (ADIO_Offset_CA)(fs_block_info[0]);
                ca_data->onesided_always_rmw = 1;
                ca_data->onesided_no_rmw = 1;

                /* Holes are found in the data and the user has not set
                * onesided_no_rmw --- set onesided_always_rmw to 1
                * and redo the entire aggregation and write and if the user has
                * onesided_inform_rmw set then inform him of this condition
                * and behavior.
                */
                if (ca_data->onesided_inform_rmw && (myrank ==0)) {
                    fprintf(stderr,"Information: Holes found during one-sided "
                    "write aggregation algorithm --- re-running one-sided "
                    "write aggregation with onesided_always_rmw set to 1.\n");
                }
            }
            else
                doAggregation = 0;
        }
        else
            doAggregation = 0;

    } // while doAggregation
    ca_data->onesided_no_rmw = prev_onesided_no_rmw;

    H5MM_free(segment_stripe_start);
    H5MM_free(segment_stripe_end);

} /* H5FD_mpio_ccio_iterate_write */

/*-------------------------------------------------------------------------
 * Function:    H5FD_mpio_ccio_iterate_read
 *
 * Purpose:     This function calls H5FD_mpio_ccio_osagg_read
 *              iteratively to perform "rounds" of one-sided collective
 *              data aggregation.
 *
 * Return:      Void.
 *
 *-------------------------------------------------------------------------
 */
 void H5FD_mpio_ccio_iterate_read(CustomAgg_FH_Data ca_data, void *buf,
    int *fs_block_info, ADIO_Offset_CA *offset_list, ADIO_Offset_CA *len_list,
    MPI_Offset mpi_off, int contig_access_count, int currentValidDataIndex,
    ADIO_Offset_CA start_offset, ADIO_Offset_CA end_offset,
    ADIO_Offset_CA firstFileOffset, ADIO_Offset_CA lastFileOffset,
    H5S_flatbuf_t *memFlatBuf, H5S_flatbuf_t *fileFlatBuf, int myrank, int *error_code)
{

    int i;
    int stripesPerAgg = ca_data->cb_buffer_size / fs_block_info[0];
    int numStripedAggs = ca_data->cb_nodes;
    if (stripesPerAgg == 0) {
        /* The striping unit is larger than the collective buffer size
        * therefore we must abort since the buffer has already been
        * allocated during the open.
        */
        fprintf(stderr,"Error: The collective buffer size %d is less "
        "than the fs_block_size %d - This collective I/O implementation "
        "cannot continue.\n",ca_data->cb_buffer_size,fs_block_info[0]);
        MPI_Abort(MPI_COMM_WORLD, 1);
    }
    MPI_Comm_rank(ca_data->comm, &myrank);

    /* Declare ADIOI_OneSidedStripeParms here - these parameters will be locally managed
    * for this invokation of H5FD_mpio_ccio_iterate_read.  This will allow for concurrent
    * one-sided collective writes via multi-threading as well as multiple communicators.
    */
    FS_Block_Parms stripeParms;
    stripeParms.stripeSize = fs_block_info[0]; /* stripe_size */
    stripeParms.stripedLastFileOffset = lastFileOffset;
    stripeParms.iWasUsedStripingAgg = 0;
    stripeParms.numStripesUsed = 0;
    stripeParms.amountOfStripedDataExpected = 0;
    stripeParms.bufTypeExtent = 0;
    stripeParms.lastDataTypeExtent = 0;
    stripeParms.lastFlatBufIndice = 0;
    stripeParms.lastIndiceOffset = 0;

    /* The general algorithm here is to divide the file up into segments, a segment
    * being defined as a contiguous region of the file which has up to `numStripedAggs`
    * occurrences of each stripe - the data for each stripe being READ by a particular
    * aggregator.  The segmentLen is the maximum size in bytes of each segment
    * (stripeSize * numStripedAggs).  Here, we iteratively call
    * H5FD_mpio_ccio_osagg_read for each segment to READ the data.
    */
    stripeParms.segmentLen = ((ADIO_Offset_CA)numStripedAggs)*((ADIO_Offset_CA)(fs_block_info[0]));

    /* These arrays define the file offsets for the stripes for a given segment - similar
    * to the concept of file domains in GPFS, essentially file domeains for the segment.
    */
    ADIO_Offset_CA *segment_stripe_start = (ADIO_Offset_CA *) H5MM_malloc(numStripedAggs*sizeof(ADIO_Offset_CA));
    ADIO_Offset_CA *segment_stripe_end = (ADIO_Offset_CA *) H5MM_malloc(numStripedAggs*sizeof(ADIO_Offset_CA));
    ADIO_Offset_CA *segment_stripe_start_next;
    ADIO_Offset_CA *segment_stripe_end_next;
    if (ca_data->async_io_outer) {
        segment_stripe_start_next = (ADIO_Offset_CA *) H5MM_malloc(numStripedAggs*sizeof(ADIO_Offset_CA));
        segment_stripe_end_next = (ADIO_Offset_CA *) H5MM_malloc(numStripedAggs*sizeof(ADIO_Offset_CA));
    }

    /* Find the actual range of stripes in the file that have data in the offset
    * ranges being written -- skip holes at the front and back of the file.
    */
    int currentOffsetListIndex = 0;
    int fileSegmentIter = 0;
    int startingStripeWithData = 0;
    int foundStartingStripeWithData = 0;
    while (!foundStartingStripeWithData) {
        if ( ((startingStripeWithData+1) * (ADIO_Offset_CA)(fs_block_info[0])) > firstFileOffset)
            foundStartingStripeWithData = 1;
        else
            startingStripeWithData++;
    }

    /* currentSegementOffset = Offset to beginning of first stripe with data to be read */
    ADIO_Offset_CA currentSegementOffset = (ADIO_Offset_CA)startingStripeWithData * (ADIO_Offset_CA)(fs_block_info[0]);

    /* How many "rounds" of segements will we need to iterate through here */
    int numSegments = (int) ((lastFileOffset+(ADIO_Offset_CA)1 - currentSegementOffset)/stripeParms.segmentLen);
    if ((lastFileOffset+(ADIO_Offset_CA)1 - currentSegementOffset)%stripeParms.segmentLen > 0)
        numSegments++;

#ifdef onesidedtrace
    printf("Rank %d - H5FD_mpio_ccio_iterate_read ca_data->cb_nodes is %d numStripedAggs is %d numSegments is %d start_offset is %ld end_offset is %ld firstFileOffset is %ld lastFileOffset is %ld\n",myrank,ca_data->cb_nodes,numStripedAggs,numSegments,start_offset,end_offset,firstFileOffset,lastFileOffset);
    fflush(stdout);
#endif

    /* This variable tracks how many segment stripes we have packed into the agg
     * buffers so we know when the buffers are full.
     */
    stripeParms.segmentIter = 0;

    /* stripeParms.stripesPerAgg is the number of stripes the aggregator must
     * read to fill it's buffer.
     */
    stripeParms.stripesPerAgg = stripesPerAgg;
    if (stripeParms.stripesPerAgg > numSegments)
        stripeParms.stripesPerAgg = numSegments;

    int totalDataReadLastRound = 0;

    /* Use 'next' read offsets for async I/O */
    ADIO_Offset_CA segmentFirstFileOffset_next, segmentLastFileOffset_next;

    /* Async I/O - Start with use_dup==0 */
    ca_data->use_dup = 0;

    /* Now, we iterate trhough all the segments that we want to read */
    for (fileSegmentIter=0;fileSegmentIter < numSegments;fileSegmentIter++) {

        int dataReadThisRound = 0;

        ADIO_Offset_CA segmentFirstFileOffset, segmentLastFileOffset;

        /* Define the segment range in terms of a file offsets.
        * Just increment the offset from the previous 'currentSegementOffset'
        */
        segmentFirstFileOffset = currentSegementOffset;
        if ((currentSegementOffset+stripeParms.segmentLen-(ADIO_Offset_CA)1) > lastFileOffset)
            currentSegementOffset = lastFileOffset;
        else
            currentSegementOffset += (stripeParms.segmentLen-(ADIO_Offset_CA)1);
        segmentLastFileOffset = currentSegementOffset;
        currentSegementOffset++; // shifting by one byte offset

        ADIO_Offset_CA segment_stripe_offset = segmentFirstFileOffset;
        for (i=0;i<numStripedAggs;i++) {
            if (firstFileOffset > segment_stripe_offset)
                segment_stripe_start[i] = firstFileOffset;
            else
                segment_stripe_start[i] = segment_stripe_offset;
            if ((segment_stripe_offset + (ADIO_Offset_CA)(fs_block_info[0])) > lastFileOffset)
                segment_stripe_end[i] = lastFileOffset;
            else
                segment_stripe_end[i] = segment_stripe_offset + (ADIO_Offset_CA)(fs_block_info[0]) - (ADIO_Offset_CA)1;
            segment_stripe_offset += (ADIO_Offset_CA)(fs_block_info[0]);
        }

        if ((ca_data->async_io_outer) && (fileSegmentIter<(numSegments-1)) && (numSegments>1)) {
            ADIO_Offset_CA cso_prev = currentSegementOffset;
            segmentFirstFileOffset_next = cso_prev;
            if ((cso_prev+stripeParms.segmentLen-(ADIO_Offset_CA)1) > lastFileOffset)
                cso_prev = lastFileOffset;
            else
                cso_prev += (stripeParms.segmentLen-(ADIO_Offset_CA)1);
            segmentLastFileOffset_next = cso_prev;

            ADIO_Offset_CA sso_next = segmentFirstFileOffset_next;
            for (i=0;i<numStripedAggs;i++) {
                if (firstFileOffset > sso_next)
                    segment_stripe_start_next[i] = firstFileOffset;
                else
                    segment_stripe_start_next[i] = sso_next;
                if ((sso_next + (ADIO_Offset_CA)(fs_block_info[0])) > lastFileOffset)
                    segment_stripe_end_next[i] = lastFileOffset;
                else
                    segment_stripe_end_next[i] = sso_next + (ADIO_Offset_CA)(fs_block_info[0]) - (ADIO_Offset_CA)1;
                sso_next += (ADIO_Offset_CA)(fs_block_info[0]);
            }

        }

        /* In the interest of performance for non-contiguous data with large offset lists
        * essentially modify the given offset and length list appropriately for this segment
        * and then pass pointers to the sections of the lists being used for this segment
        * to H5FD_mpio_ccio_osagg_read.  Remember how we have modified the list for this
        * segment, and then restore it appropriately after processing for this segment has
        * concluded, so it is ready for the next segment.
        */
        int segmentContigAccessCount = 0;
        int startingOffsetListIndex = -1;
        int endingOffsetListIndex = -1;
        ADIO_Offset_CA startingOffsetAdvancement = 0;
        ADIO_Offset_CA startingLenTrim = 0;
        ADIO_Offset_CA endingLenTrim = 0;

        while ( ((offset_list[currentOffsetListIndex] + ((ADIO_Offset_CA)(len_list[currentOffsetListIndex]))-(ADIO_Offset_CA)1) < segmentFirstFileOffset) && (currentOffsetListIndex < (contig_access_count-1)))
        {
            currentOffsetListIndex++;
        }

        startingOffsetListIndex = currentOffsetListIndex;
        endingOffsetListIndex = currentOffsetListIndex;
        int offsetInSegment = 0;
        ADIO_Offset_CA offsetStart = offset_list[currentOffsetListIndex];
        ADIO_Offset_CA offsetEnd = (offset_list[currentOffsetListIndex] + ((ADIO_Offset_CA)(len_list[currentOffsetListIndex]))-(ADIO_Offset_CA)1);

        if (len_list[currentOffsetListIndex] == 0)
            offsetInSegment = 0;
        else if ((offsetStart >= segmentFirstFileOffset) && (offsetStart <= segmentLastFileOffset)) {
            offsetInSegment = 1;
        }
        else if ((offsetEnd >= segmentFirstFileOffset) && (offsetEnd <= segmentLastFileOffset)) {
            offsetInSegment = 1;
        }
        else if ((offsetStart <= segmentFirstFileOffset) && (offsetEnd >= segmentLastFileOffset)) {
            offsetInSegment = 1;
        }

        if (!offsetInSegment) {
            segmentContigAccessCount = 0;

        }
        else {
            /* We are in the segment, advance currentOffsetListIndex until we are out of segment.
            */
            segmentContigAccessCount = 1;

            while ((offset_list[currentOffsetListIndex] <= segmentLastFileOffset) && (currentOffsetListIndex < contig_access_count)) {
                dataReadThisRound += (int) len_list[currentOffsetListIndex];
                currentOffsetListIndex++;
            }

            if (currentOffsetListIndex > startingOffsetListIndex) {
                /* If we did advance, if we are at the end need to check if we are still in segment.
                */
                if (currentOffsetListIndex == contig_access_count) {
                    currentOffsetListIndex--;
                }
                else if (offset_list[currentOffsetListIndex] > segmentLastFileOffset) {
                    /* We advanced into the last one and it still in the segment.
                    */
                    currentOffsetListIndex--;
                }
                else {
                    dataReadThisRound += (int) len_list[currentOffsetListIndex];
                }
                segmentContigAccessCount += (currentOffsetListIndex-startingOffsetListIndex);
                endingOffsetListIndex = currentOffsetListIndex;
            }
        }

        if (segmentContigAccessCount > 0) {
            /* Trim edges here so all data in the offset list range fits exactly in the segment.
            */
            if (offset_list[startingOffsetListIndex] < segmentFirstFileOffset) {
                startingOffsetAdvancement = segmentFirstFileOffset-offset_list[startingOffsetListIndex];
                offset_list[startingOffsetListIndex] += startingOffsetAdvancement;
                dataReadThisRound -= (int) startingOffsetAdvancement;
                startingLenTrim = startingOffsetAdvancement;
                len_list[startingOffsetListIndex] -= startingLenTrim;
            }

            if ((offset_list[endingOffsetListIndex] + ((ADIO_Offset_CA)(len_list[endingOffsetListIndex]))-(ADIO_Offset_CA)1) > segmentLastFileOffset) {
                endingLenTrim = offset_list[endingOffsetListIndex]+ ((ADIO_Offset_CA)(len_list[endingOffsetListIndex]))-(ADIO_Offset_CA)1 - segmentLastFileOffset;
                len_list[endingOffsetListIndex] -= endingLenTrim;
                dataReadThisRound -= (int) endingLenTrim;
            }
        }

        /* Once we have packed the collective buffers, set stripeParms.flushCB = 1
         * to signify this (note that stripeParms.flushCB does NOT control the actual I/O for reading)
         * That is, we are reading on every call, so 'flushCB' isn't really necessary for reads
         */
        if ((stripeParms.segmentIter == (stripeParms.stripesPerAgg-1)) || (fileSegmentIter == (numSegments-1))) {
            stripeParms.flushCB = 1;
        }
        else
            stripeParms.flushCB = 0;

        stripeParms.firstStripedIOCall = 0;
        stripeParms.lastStripedIOCall = 0;
        if (fileSegmentIter == 0) {
            stripeParms.firstStripedIOCall = 1;
        }
        else if (fileSegmentIter == (numSegments-1))
            stripeParms.lastStripedIOCall = 1;

        /* The difference in calls to H5FD_mpio_ccio_osagg_read is based on the whether the buftype is
        * contiguous.  The algorithm tracks the position in the target buffer when called
        * multiple times --  in the case of contiguous data this is simple and can be externalized with
        * a buffer offset, in the case of non-contiguous data this is complex and the state must be tracked
        * internally, therefore no external buffer offset.  Care was taken to minimize
        * H5FD_mpio_ccio_osagg_read changes at the expense of some added complexity to the caller.
        */

        /* Async I/O - Create a pipeline of 'reads' */
        if ((ca_data->async_io_outer) && (fileSegmentIter==0) && (numSegments>1)) {

            /* Read data from file into aggregator buffers */
            H5FD_mpio_ccio_file_read(ca_data, error_code, segmentFirstFileOffset, segmentLastFileOffset, segment_stripe_start, segment_stripe_end);

            /* Async I/O - Start prefetch of next iteration with duplite buffer */
            ca_data->use_dup = (ca_data->use_dup + 1) % 2;

            /* Read data from file into aggregator buffers for NEXT interation */
            H5FD_mpio_ccio_file_read(ca_data, error_code, segmentFirstFileOffset_next, segmentLastFileOffset_next, segment_stripe_start_next, segment_stripe_end_next);

            /* Async I/O - Switch back to current buffer */
            ca_data->use_dup = (ca_data->use_dup + 1) % 2;

        } else if ((ca_data->async_io_outer) && (fileSegmentIter<(numSegments-1)) && (numSegments>1)) {

            /* Async I/O - Start prefetch of next iteration with duplite buffer */
            ca_data->use_dup = (ca_data->use_dup + 1) % 2;

            /* Read data from file into aggregator buffers for NEXT interation */
            H5FD_mpio_ccio_file_read(ca_data, error_code, segmentFirstFileOffset_next, segmentLastFileOffset_next, segment_stripe_start_next, segment_stripe_end_next);

            /* Async I/O - Switch back to current buffer */
            ca_data->use_dup = (ca_data->use_dup + 1) % 2;

        } else if ((!ca_data->async_io_outer) || (numSegments<2)) {

            /* Read data from file into aggregator buffers */
            H5FD_mpio_ccio_file_read(ca_data, error_code, segmentFirstFileOffset, segmentLastFileOffset, segment_stripe_start, segment_stripe_end);

        }

        /* Async I/O - Wait for necessary buffer to be ready for RMA */
        if (ca_data->use_dup && ca_data->check_req_d) {
            MPIO_Wait(&ca_data->io_Request_d, error_code);
            ca_data->check_req_d = 0;
        } else if (!ca_data->use_dup && ca_data->check_req) {
            MPIO_Wait(&ca_data->io_Request, error_code);
            ca_data->check_req = 0;
        }

        if (memFlatBuf->count == 1) {

            /* Ranks perform one-sided read of data from collective buffers */
            H5FD_mpio_ccio_osagg_read(ca_data,(ADIO_Offset_CA*)&(offset_list[startingOffsetListIndex]), (ADIO_Offset_CA*)&(len_list[startingOffsetListIndex]), segmentContigAccessCount, buf+totalDataReadLastRound, memFlatBuf, error_code, segmentFirstFileOffset, segmentLastFileOffset, currentValidDataIndex, segment_stripe_start, segment_stripe_end, &stripeParms, 0); // Last '0' means the file read should be skipped in this call

        } else {

            /* Ranks perform one-sided read of data from collective buffers */
            H5FD_mpio_ccio_osagg_read(ca_data,(ADIO_Offset_CA*)&(offset_list[startingOffsetListIndex]), (ADIO_Offset_CA*)&(len_list[startingOffsetListIndex]), segmentContigAccessCount, buf, memFlatBuf, error_code, segmentFirstFileOffset, segmentLastFileOffset, currentValidDataIndex, segment_stripe_start, segment_stripe_end, &stripeParms, 0); // Last '0' means the file read should be skipped in this call

        }

        /* Async I/O - change 'current' buffer */
        if ((ca_data->async_io_outer) && (numSegments>1)) {
            ca_data->use_dup = (ca_data->use_dup + 1) % 2;
        }

        //if (stripeParms.flushCB) {
            stripeParms.segmentIter = 0;
            if (stripesPerAgg > (numSegments-fileSegmentIter-1))
            stripeParms.stripesPerAgg = numSegments-fileSegmentIter-1;
            else
            stripeParms.stripesPerAgg = stripesPerAgg;
        //}
        //else
        //    stripeParms.segmentIter++;

        /* Need barrier here.
        */
        if (fileSegmentIter < (numSegments-1)) {
            MPI_Barrier(ca_data->comm);
        }

        /* Restore the offset_list and len_list to values that are ready for the
        * next iteration.
        */
        if (segmentContigAccessCount > 0) {
            offset_list[endingOffsetListIndex] += len_list[endingOffsetListIndex];
            len_list[endingOffsetListIndex] = endingLenTrim;
        }
        totalDataReadLastRound += dataReadThisRound;

    } // fileSegmentIter for-loop

    H5MM_free(segment_stripe_start);
    H5MM_free(segment_stripe_end);
    if (ca_data->async_io_outer) {
        H5MM_free(segment_stripe_start_next);
        H5MM_free(segment_stripe_end_next);
    }

} /* End IterateOneSidedRead */

/*-------------------------------------------------------------------------
 * Function:    H5FD_mpio_nc_buffer_advance
 *
 * This funtion packs a contigous buffer of data from the non-contgious source
 * buffer for a specified chunk of data and advances the FDSourceBufferState
 * machinery, so subsequent calls with the FDSourceBufferState will return the
 * next linear chunk.
 * Parameters:
 * in:     sourceDataBuffer - pointer to source data buffer.
 * in:     flatBuf - pointer to flattened source data buffer
 * in:     targetNumBytes - number of bytes to return and advance.
 * in:     packing - whether data is being packed from the source buffer to the
 *         packed buffer (1) or unpacked from the packed buffer to the source
 *         buffer (0)
 * in/out: currentFDSourceBufferState - pointer to FDSourceBufferState structure, current
 *                                      data used as starting point, will be updated with
 *                                      the new state after targetNumBytes advance.
 * out:    packedDataBufer - pointer to the output packed data buffer.  If the
 *                           value is NULL then no data will be written.
 *
 *-------------------------------------------------------------------------
 */
inline static void H5FD_mpio_nc_buffer_advance(char *sourceDataBuffer,
  H5S_flatbuf_t *flatBuf, int targetNumBytes, int packing,
  FDSourceBufferState_CA *currentFDSourceBufferState, char *packedDataBufer)
{
    /*
     * Make currentDataTypeExtent and bufTypeExtent ADIO_Offset_CA since they are
     * used in offset calculations
     */
    ADIO_Offset_CA currentIndiceOffset = currentFDSourceBufferState->indiceOffset;
    ADIO_Offset_CA bufTypeExtent = (ADIO_Offset_CA)currentFDSourceBufferState->bufTypeExtent;
    ADIO_Offset_CA currentDataTypeExtent = currentFDSourceBufferState->dataTypeExtent;
    int currentFlatBufIndice = currentFDSourceBufferState->flatBufIndice;
    int targetSendDataIndex = 0;

#ifdef onesidedtrace
    printf("H5FD_mpio_nc_buffer_advance: currentFlatBufIndice is %d currentDataTypeExtent is %ld currentIndiceOffset is %ld\n",currentFlatBufIndice,currentDataTypeExtent,currentIndiceOffset);
#endif

    int remainingBytesToLoad = targetNumBytes;
    while (remainingBytesToLoad > 0) {
        if ((flatBuf->blocklens[currentFlatBufIndice] - currentIndiceOffset) >= remainingBytesToLoad) { // we can get the rest of our data from this indice
            ADIO_Offset_CA physicalSourceBufferOffset = (currentDataTypeExtent * bufTypeExtent) + flatBuf->indices[currentFlatBufIndice] + currentIndiceOffset;

#ifdef onesidedtrace
            printf("loading remainingBytesToLoad %d from src buffer offset %ld to targetSendDataIndex %d\n",remainingBytesToLoad,physicalSourceBufferOffset,targetSendDataIndex);
#endif

            if (packedDataBufer != NULL) {
                if (packing)
                    memcpy(&(packedDataBufer[targetSendDataIndex]),&(sourceDataBuffer[physicalSourceBufferOffset]),remainingBytesToLoad);
                else
                    memcpy(&(sourceDataBuffer[physicalSourceBufferOffset]),&(packedDataBufer[targetSendDataIndex]),remainingBytesToLoad);
            }

            targetSendDataIndex += remainingBytesToLoad;
            currentIndiceOffset += (ADIO_Offset_CA)remainingBytesToLoad;
            if (currentIndiceOffset >= flatBuf->blocklens[currentFlatBufIndice]) {
                currentIndiceOffset = (ADIO_Offset_CA)0;
                currentFlatBufIndice++;
                if (currentFlatBufIndice == flatBuf->count) {
                    currentFlatBufIndice = 0;
                    currentDataTypeExtent++;
                }
            }
            remainingBytesToLoad = 0;

        }
        else { // we can only get part of our data from this indice
            ADIO_Offset_CA amountDataToLoad = (flatBuf->blocklens[currentFlatBufIndice] - currentIndiceOffset);
            ADIO_Offset_CA physicalSourceBufferOffset = (currentDataTypeExtent * bufTypeExtent) + flatBuf->indices[currentFlatBufIndice] + currentIndiceOffset;

#ifdef onesidedtrace
            printf("loading amountDataToLoad %d from src buffer offset %ld to targetSendDataIndex %d\n",amountDataToLoad,physicalSourceBufferOffset,targetSendDataIndex);
#endif
            if (packedDataBufer != NULL) {
                if (packing)
                    memcpy(&(packedDataBufer[targetSendDataIndex]),&(sourceDataBuffer[physicalSourceBufferOffset]),amountDataToLoad);
                else
                    memcpy(&(sourceDataBuffer[physicalSourceBufferOffset]),&(packedDataBufer[targetSendDataIndex]),amountDataToLoad);
            }

            targetSendDataIndex += amountDataToLoad;
            currentIndiceOffset = (ADIO_Offset_CA)0;
            currentFlatBufIndice++;
            if (currentFlatBufIndice == flatBuf->count) {
                currentFlatBufIndice = 0;
                currentDataTypeExtent++;
            }
            remainingBytesToLoad -= amountDataToLoad;
        }
    } // while

    /*
     * Update machinery with new flatbuf position
     */
    currentFDSourceBufferState->indiceOffset = currentIndiceOffset;
    currentFDSourceBufferState->dataTypeExtent = currentDataTypeExtent;
    currentFDSourceBufferState->flatBufIndice = currentFlatBufIndice;
#ifdef onesidedtrace
    printf("source buf advanced to currentFlatBufIndice %d currentDataTypeExtent %ld currentIndiceOffset %ld\n",currentFlatBufIndice,currentDataTypeExtent,currentIndiceOffset);
#endif
}; /* H5FD_mpio_nc_buffer_advance */

/*-------------------------------------------------------------------------
 * Function:    H5FD_mpio_ccio_osagg_write
 *
 * Purpose:
 *
 * The H5FD_mpio_ccio_osagg_write algorithm is called once
 * for each segment of data, a segment being defined as a contiguous region of the file which
 * is the size of one striping unit times the number of aggregators.  For lustre the striping unit
 * corresponds with the actual file stripe, in the case of gpfs these are file domains.
 * Each call effectively packs one striping unit of data into the collective buffer on each agg,
 * with additional parameters which govern when to flush the collective buffer to the file.
 * Therefore in practice the collective write call for a file system such as
 * lustre on a dataset composed of multiple segments would call the algorithm several times without a
 * flush parameter to fill the collective buffers with multiple stripes of data, before calling it again to flush
 * the collective buffer to the file system.  In this fashion the synchronization can be minimized as that
 * only needs to occur during the actual read from or write to the file system.  In the case of gpfs
 * this function is called just once.  The FS_Block_Parms parameter is used to save the
 * state and re-use variables thru repetative calls to help in the case of lustre to avoid costly
 * recomputation, for consistency gpfs utilizes it as well but doesn't use some aspects of it.  This
 * function was originally first written for gpfs only and then modified to support lustre.
 *
 *-------------------------------------------------------------------------
 */
void H5FD_mpio_ccio_osagg_write(CustomAgg_FH_Data ca_data,
    ADIO_Offset_CA *offset_list,
    ADIO_Offset_CA *len_list,
    int contig_access_count,
    const void *buf,
    H5S_flatbuf_t *memFlatBuf,
    int *error_code,
    ADIO_Offset_CA firstFileOffset,
    ADIO_Offset_CA lastFileOffset,
    int numNonZeroDataOffsets,
    ADIO_Offset_CA *fd_start,
    ADIO_Offset_CA* fd_end,
    int hole_found,
    FS_Block_Parms *stripe_parms)

{
    int i,j; /* generic iterators */

    /*
     * Make local copy of certain ADIOI_OneSidedStripeParms elements for
     * faster access - pay for pointer dereference only once.
     */
    int stripeSize = stripe_parms->stripeSize;
    int segmentIter = stripe_parms->segmentIter;
    hsize_t bufTypeExtent = stripe_parms->bufTypeExtent;

    if ((stripeSize > 0) && stripe_parms->firstStripedIOCall)
        stripe_parms->iWasUsedStripingAgg = 0;

#ifdef onesidedtrace
    if (buf == NULL) {
        printf("H5FD_mpio_ccio_osagg_write - buf is NULL contig_access_count is %d\n",contig_access_count);
        for (i=0;i<contig_access_count;i++)
            printf("offset_list[%d] is %ld len_list[%d] is %ld\n",i,offset_list[i],i,len_list[i]);
    }
    if (contig_access_count < 0)
        printf("H5FD_mpio_ccio_osagg_write - contig_access_count "
            "of %d is less than 0\n",contig_access_count);
#endif

    int lenListOverZero = 0;
    for (i=0;((i<contig_access_count) && (!lenListOverZero));i++)
        if (len_list[i] > 0)
            lenListOverZero = 1;

    *error_code = MPI_SUCCESS; /* initialize to success */

    MPI_Status status;
    int nprocs,myrank;
    MPI_Comm_size(ca_data->comm, &nprocs);
    MPI_Comm_rank(ca_data->comm, &myrank);

#ifdef onesidedtrace
    printf("Rank %d - H5FD_mpio_ccio_osagg_write started\n",myrank);
#endif

    if (ca_data->io_buf_window == MPI_WIN_NULL || ca_data->io_buf_put_amounts_window == MPI_WIN_NULL)
    {
        HDF5_ccio_win_setup(ca_data, nprocs);
    }

    /*
     * This flag denotes whether the source datatype is contiguous, which is referenced throughout the algorithm
     * and defines how the source buffer offsets and data chunks are determined.  If the value is 1 (true - contiguous data)
     * things are profoundly simpler in that the source buffer offset for a given target offset simply linearly increases
     * by the chunk sizes being written.  If the value is 0 (non-contiguous) then these values are based on calculations
     * from the flattened source datatype.
     */
    int bufTypeIsContig;
    if (memFlatBuf->count == 1)
        bufTypeIsContig = 1;
    else
        bufTypeIsContig = 0;

    if (!bufTypeIsContig) {
        /* For a non-contiguous source buffer set the extent. */
        if ((stripeSize == 0) || stripe_parms->firstStripedIOCall) {
            bufTypeExtent = memFlatBuf->extent;
        }

#ifdef onesidedtrace
        printf("Rank %d - memFlatBuf->count is %d bufTypeExtent is %ld\n",myrank,memFlatBuf->count, bufTypeExtent);
        for (i=0;i<memFlatBuf->count;i++)
            printf("Rank %d - memFlatBuf->blocklens[%d] is %d memFlatBuf->indices[%d] is %ld\n",myrank,i,memFlatBuf->blocklens[i],i,memFlatBuf->indices[i]);
#endif
    }

    int naggs = ca_data->cb_nodes;

    /* Track the state of the source buffer for feeding the target data blocks.
     * For GPFS the number of file domains per agg is always 1 so we just need 1 agg
     * dimension to track the data, in the case of lustre we will need 2 dimensions
     * agg and file domain since aggs write to multiple file domains in the case of lustre.
     * This structure will be modified as the data is written to reflect the current state
     * of the offset.
     */

#ifdef onesidedtrace
    printf("Rank %d - sizeof(FDSourceBufferState_CA) is %d - make sure is 32 for 32-byte memalign optimal\n",myrank,sizeof(FDSourceBufferState_CA));
#endif

    FDSourceBufferState_CA *currentFDSourceBufferState = (FDSourceBufferState_CA *) H5MM_malloc(naggs * sizeof(FDSourceBufferState_CA));

    for (i=0;i<naggs;i++) {
        /* Initialize based on the bufType to indicate that it is unset.
         */
        if (bufTypeIsContig) {
            currentFDSourceBufferState[i].sourceBufferOffset = -1;
        }
        else {
            currentFDSourceBufferState[i].indiceOffset = -1;
        }
    }

#ifdef onesidedtrace
    printf("Rank %d - H5FD_mpio_ccio_osagg_write bufTypeIsContig is %d contig_access_count is %d\n",myrank,bufTypeIsContig,contig_access_count);
#endif

    /* MaxNumContigOperations keeps track of how many different chunks we will need to send
     * for the purpose of pre-allocating the data structures to hold them.
     */
    int maxNumContigOperations = contig_access_count;
    int myAggRank = -1; /* if I am an aggregor this is my index into ranklist */
    int iAmUsedAgg = 0; /* whether or not this rank is used as an aggregator. */

    /* Make coll_bufsize an ADIO_Offset_CA since it is used in calculations with offsets.
     */
    ADIO_Offset_CA coll_bufsize = (ADIO_Offset_CA)(ca_data->cb_buffer_size);

    /* This logic defines values that are used later to determine what offsets define the portion
     * of the file domain the agg is writing this round.
     */
    int greatestFileDomainAggRank = -1,smallestFileDomainAggRank = -1;
    ADIO_Offset_CA greatestFileDomainOffset = 0;
    ADIO_Offset_CA smallestFileDomainOffset = lastFileOffset;
    for (j=0;j<naggs;j++) {
        if (fd_end[j] > greatestFileDomainOffset) {
            greatestFileDomainOffset = fd_end[j];
            greatestFileDomainAggRank = j;
        }
        if (fd_start[j] < smallestFileDomainOffset) {
            smallestFileDomainOffset = fd_start[j];
            smallestFileDomainAggRank = j;
        }
        if (ca_data->ranklist[j] == myrank) {
            myAggRank = j;
            if (fd_end[j] > fd_start[j]) {
                iAmUsedAgg = 1;
                stripe_parms->iWasUsedStripingAgg = 1;
            }
        }
    }

#ifdef onesidedtrace
    printf("Rank %d - contig_access_count is %d lastFileOffset is %ld firstFileOffset is %ld\n",myrank,contig_access_count,lastFileOffset,firstFileOffset);
    for (j=0;j<contig_access_count;j++) {
        printf("Rank %d - offset_list[%d]: %ld , len_list[%d]: %ld\n",myrank,j,offset_list[j],j,len_list[j]);
    }
#endif

    /* Compute number of rounds.
     */
    int numberOfRounds = 0;
    for (j=0;j<naggs;j++) {
        int currentNumberOfRounds = (int)(((fd_end[j] - fd_start[j])+(ADIO_Offset_CA)1)/coll_bufsize);
        if (((ADIO_Offset_CA)currentNumberOfRounds*coll_bufsize) < ((fd_end[j] - fd_start[j])+(ADIO_Offset_CA)1))
            currentNumberOfRounds++;
        if (currentNumberOfRounds > numberOfRounds)
            numberOfRounds = currentNumberOfRounds;
    }

    /* Data structures to track what data this compute needs to send to whom.
     * For lustre they will all need another dimension for the file domain.
     */
    int *targetAggsForMyData = (int *)H5MM_malloc(naggs * sizeof(int));
    ADIO_Offset_CA *targetAggsForMyDataFDStart = (ADIO_Offset_CA *)H5MM_malloc(naggs * sizeof(ADIO_Offset_CA));
    ADIO_Offset_CA *targetAggsForMyDataFDEnd = (ADIO_Offset_CA *)H5MM_malloc(naggs * sizeof(ADIO_Offset_CA));
    int numTargetAggs = 0;

    /* This data structure holds the beginning offset and len list index for the range to be written
     * coresponding to the round and target agg.  Initialize to -1 to denote being unset.
     */
    int **targetAggsForMyDataFirstOffLenIndex = (int **)H5MM_malloc(numberOfRounds * sizeof(int *));
    for (i=0;i<numberOfRounds;i++) {
        targetAggsForMyDataFirstOffLenIndex[i] = (int *)H5MM_malloc(naggs * sizeof(int));
        for (j=0;j<naggs;j++)
            targetAggsForMyDataFirstOffLenIndex[i][j] = -1;
    }

    /* This data structure holds the ending offset and len list index for the range to be written
     * coresponding to the round and target agg.
     */
    int **targetAggsForMyDataLastOffLenIndex = (int **)H5MM_malloc(numberOfRounds * sizeof(int *));
    for (i=0;i<numberOfRounds;i++)
        targetAggsForMyDataLastOffLenIndex[i] = (int *)H5MM_malloc(naggs * sizeof(int));

#ifdef onesidedtrace
    printf("Rank %d - NumberOfRounds is %d\n",myrank,numberOfRounds);
    for (i=0;i<naggs;i++)
        printf("Rank %d - ca_data->ranklist[%d] is %d fd_start is %ld fd_end is %ld\n",myrank,i,ca_data->ranklist[i],fd_start[i],fd_end[i]);
    for (j=0;j<contig_access_count;j++)
        printf("Rank %d - offset_list[%d] is %ld len_list is %ld\n",myrank,j,offset_list[j],len_list[j]);
#endif

    int currentAggRankListIndex = 0;
    int maxNumNonContigSourceChunks = 0;

    ADIO_Offset_CA currentSourceBufferOffset = 0;
    ADIO_Offset_CA currentDataTypeExtent = 0;
    int currentFlatBufIndice=0;
    ADIO_Offset_CA currentIndiceOffset = 0;

    /* Remember where we left off in the source buffer when packing stripes. */
    if ((stripeSize > 0) && !stripe_parms->firstStripedIOCall) {
        currentDataTypeExtent = stripe_parms->lastDataTypeExtent;
        currentFlatBufIndice = stripe_parms->lastFlatBufIndice;
        currentIndiceOffset = stripe_parms->lastIndiceOffset;
#ifdef onesidedtrace
        printf("Rank %d - using stripe_parms->lastDataTypeExtent %ld stripe_parms->lastFlatBufIndice %d stripe_parms->lastIndiceOffset %ld\n",
             myrank,stripe_parms->lastDataTypeExtent,stripe_parms->lastFlatBufIndice,stripe_parms->lastIndiceOffset);
#endif
    }

    /* This denotes the coll_bufsize boundaries within the source buffer for writing for the same round.
     */
    ADIO_Offset_CA intraRoundCollBufsizeOffset = 0;

    /* This data structure tracks what target aggs need to be written to on what rounds.
     */
    int *targetAggsForMyDataCurrentRoundIter = (int *)H5MM_malloc(naggs * sizeof(int));
    for (i=0;i<naggs;i++)
        targetAggsForMyDataCurrentRoundIter[i] = 0;

    /* This is the first of the two main loops in this algorithm.  The purpose of this loop is essentially to populate
     * the data structures defined above for what source data blocks needs to go where (target agg and file domain) and when
     * (round iter).  For lustre essentially an additional layer of nesting will be required for the multiple file domains
     * within the target agg.
     */
    if ((contig_access_count > 0) && (buf != NULL) && lenListOverZero) {
        int blockIter;
        for (blockIter=0;blockIter<contig_access_count;blockIter++) {

            /* Determine the starting source buffer offset for this block - for iter 0 skip it since that value is 0.
             */
            if (blockIter>0) {
                if (bufTypeIsContig) {
                    currentSourceBufferOffset += len_list[blockIter-1];
                }
                else {

                    /* Non-contiguous source datatype, count up the extents and indices to this point
                    * in the blocks for use in computing the source starting buffer offset for target aggs
                    * and file domains.
                    */
                    ADIO_Offset_CA sourceBlockTotal = 0;
                    int lastIndiceUsed = currentFlatBufIndice;
                    int numNonContigSourceChunks = 0;

                    while (sourceBlockTotal < len_list[blockIter-1]) {
                        numNonContigSourceChunks++;
                        sourceBlockTotal += (memFlatBuf->blocklens[currentFlatBufIndice] - currentIndiceOffset);
                        lastIndiceUsed = currentFlatBufIndice;
                        currentFlatBufIndice++;
                        if (currentFlatBufIndice == memFlatBuf->count) {
                            currentFlatBufIndice = 0;
                            currentDataTypeExtent++;
                        }
                        currentIndiceOffset = (ADIO_Offset_CA)0;
                    }
                    if (sourceBlockTotal > len_list[blockIter-1]) {
                        currentFlatBufIndice--;
                        if (currentFlatBufIndice < 0 ) {
                            currentDataTypeExtent--;
                            currentFlatBufIndice = memFlatBuf->count-1;
                        }
                        currentIndiceOffset =  len_list[blockIter-1] - (sourceBlockTotal - memFlatBuf->blocklens[lastIndiceUsed]);
                    }
                    else
                    currentIndiceOffset = (ADIO_Offset_CA)0;
                    maxNumContigOperations += (numNonContigSourceChunks+2);
                    if (numNonContigSourceChunks > maxNumNonContigSourceChunks)
                        maxNumNonContigSourceChunks = numNonContigSourceChunks;

#ifdef onesidedtrace
                    printf("blockiter %d currentFlatBufIndice is now %d currentDataTypeExtent is now %ld currentIndiceOffset is now %ld maxNumContigOperations is now %d\n",blockIter,currentFlatBufIndice,currentDataTypeExtent,currentIndiceOffset,maxNumContigOperations);
#endif

                } // !bufTypeIsContig
            } // blockIter > 0

            /* For the last iteration we need to include these maxNumContigOperations and maxNumNonContigSourceChunks
             * for non-contig case even though we did not need to compute the next starting offset.
             */
            if ((blockIter == (contig_access_count-1)) && (!bufTypeIsContig)) {
                ADIO_Offset_CA sourceBlockTotal = 0;
                int tmpCurrentFlatBufIndice = currentFlatBufIndice;
                int  lastNumNonContigSourceChunks = 0;
                while (sourceBlockTotal < len_list[blockIter]) {
                    lastNumNonContigSourceChunks++;
                    sourceBlockTotal += memFlatBuf->blocklens[tmpCurrentFlatBufIndice];
                    tmpCurrentFlatBufIndice++;
                    if (tmpCurrentFlatBufIndice == memFlatBuf->count) {
                        tmpCurrentFlatBufIndice = 0;
                    }
                }
                maxNumContigOperations += (lastNumNonContigSourceChunks+2);
                if (lastNumNonContigSourceChunks > maxNumNonContigSourceChunks)
                maxNumNonContigSourceChunks = lastNumNonContigSourceChunks;

            }

            ADIO_Offset_CA blockStart = offset_list[blockIter], blockEnd = offset_list[blockIter]+len_list[blockIter]-(ADIO_Offset_CA)1;

            /* Find the starting target agg for this block - normally it will be the current agg so guard the expensive
             * while loop with a cheap if-check which for large numbers of small blocks will usually be false.
             */
            if (!((blockStart >= fd_start[currentAggRankListIndex]) && (blockStart <= fd_end[currentAggRankListIndex]))) {
                while (!((blockStart >= fd_start[currentAggRankListIndex]) && (blockStart <= fd_end[currentAggRankListIndex])))
                    currentAggRankListIndex++;
            };

#ifdef onesidedtrace
            printf("Rank %d - currentAggRankListIndex is %d blockStart %ld blockEnd %ld fd_start[currentAggRankListIndex] %ld fd_end[currentAggRankListIndex] %ld\n",myrank,currentAggRankListIndex,blockStart,blockEnd,fd_start[currentAggRankListIndex],fd_end[currentAggRankListIndex]);
#endif

            /* Determine if this is a new target agg.
             */
            if (blockIter>0) {
                if ((offset_list[blockIter-1]+len_list[blockIter-1]-(ADIO_Offset_CA)1) < fd_start[currentAggRankListIndex]) {
                    numTargetAggs++;
                }
            }

            /* Determine which round to start writing - data is written coll_bufsize per round from the aggregator
             * so if our starting offset in the file domain is multiple coll_bufsize that will correspond to the round.
             */
            if ((blockStart - fd_start[currentAggRankListIndex]) >= coll_bufsize) {
                ADIO_Offset_CA currentRoundBlockStart = fd_start[currentAggRankListIndex];
                int startingRound = 0;
                while (blockStart > (currentRoundBlockStart + coll_bufsize - (ADIO_Offset_CA)1)) {
                    currentRoundBlockStart+=coll_bufsize;
                    startingRound++;
                }
                targetAggsForMyDataCurrentRoundIter[numTargetAggs] = startingRound;
            }

            /* Initialize the data structures if this is the first offset in the round/target agg.
             */
            if (targetAggsForMyDataFirstOffLenIndex[targetAggsForMyDataCurrentRoundIter[numTargetAggs]][numTargetAggs] == -1) {
                targetAggsForMyData[numTargetAggs] = ca_data->ranklist[currentAggRankListIndex];
                targetAggsForMyDataFDStart[numTargetAggs] = fd_start[currentAggRankListIndex];
                /* Round up file domain to the first actual offset used if this is the first file domain.
                 */
                if (currentAggRankListIndex == smallestFileDomainAggRank) {
                    if (targetAggsForMyDataFDStart[numTargetAggs] < firstFileOffset)
                        targetAggsForMyDataFDStart[numTargetAggs] = firstFileOffset;
                }
                targetAggsForMyDataFDEnd[numTargetAggs] = fd_end[currentAggRankListIndex];
                /* Round down file domain to the last actual offset used if this is the last file domain.
                 */
                if (currentAggRankListIndex == greatestFileDomainAggRank) {
                    if (targetAggsForMyDataFDEnd[numTargetAggs] > lastFileOffset)
                        targetAggsForMyDataFDEnd[numTargetAggs] = lastFileOffset;
                }
                targetAggsForMyDataFirstOffLenIndex[targetAggsForMyDataCurrentRoundIter[numTargetAggs]][numTargetAggs] = blockIter;
                /* Set the source buffer state starting point for data access for this agg and file domain.  */

                if (bufTypeIsContig) {
                    if (currentFDSourceBufferState[numTargetAggs].sourceBufferOffset == -1) {
                        currentFDSourceBufferState[numTargetAggs].sourceBufferOffset = currentSourceBufferOffset;
#ifdef onesidedtrace
                        printf("Rank %d - For agg %d sourceBufferOffset initialized to %ld\n",myrank,currentAggRankListIndex,currentSourceBufferOffset);
#endif
                    }
                }
                else {
                    if (currentFDSourceBufferState[numTargetAggs].indiceOffset == -1) {
                        currentFDSourceBufferState[numTargetAggs].indiceOffset = currentIndiceOffset;
                        currentFDSourceBufferState[numTargetAggs].bufTypeExtent = bufTypeExtent;
                        currentFDSourceBufferState[numTargetAggs].dataTypeExtent = currentDataTypeExtent;
                        currentFDSourceBufferState[numTargetAggs].flatBufIndice = currentFlatBufIndice;
#ifdef onesidedtrace
                        printf("Rank %d - For agg %d dataTypeExtent initialized to %ld flatBufIndice to %d indiceOffset to %ld\n",myrank,numTargetAggs,currentDataTypeExtent,currentFlatBufIndice,currentIndiceOffset);
#endif
                    }
                }

                intraRoundCollBufsizeOffset = fd_start[currentAggRankListIndex] + ((ADIO_Offset_CA)(targetAggsForMyDataCurrentRoundIter[numTargetAggs]+1) * coll_bufsize);

#ifdef onesidedtrace
                printf("Rank %d - Initial settings numTargetAggs %d offset_list[%d] with value %ld past fd border %ld with len %ld currentSourceBufferOffset set to %ld intraRoundCollBufsizeOffset set to %ld\n",myrank,numTargetAggs,blockIter,offset_list[blockIter],fd_start[currentAggRankListIndex],len_list[blockIter],currentSourceBufferOffset,intraRoundCollBufsizeOffset);
#endif
            }

            /* Replace the last offset block iter with this one.
             */
            targetAggsForMyDataLastOffLenIndex[targetAggsForMyDataCurrentRoundIter[numTargetAggs]][numTargetAggs] = blockIter;

            /* If this blocks extends into the next file domain advance to the next target aggs and source buffer states.
             */
            if (blockEnd > fd_end[currentAggRankListIndex]) {

                ADIO_Offset_CA amountToAdvanceSBOffsetForFD = 0;
                int additionalFDCounter = 0;

                while (blockEnd > fd_end[currentAggRankListIndex]) {
#ifdef onesidedtrace
                    printf("Rank %d - block extends past current fd, blockEnd %ld >= fd_end[currentAggRankListIndex] %ld total block size is %ld blockStart was %ld\n",myrank,blockEnd,fd_end[currentAggRankListIndex], len_list[blockIter],blockStart);
#endif
                    ADIO_Offset_CA thisAggBlockEnd = fd_end[currentAggRankListIndex];
                    if (thisAggBlockEnd >= intraRoundCollBufsizeOffset) {
                        while (thisAggBlockEnd >= intraRoundCollBufsizeOffset) {
                            targetAggsForMyDataCurrentRoundIter[numTargetAggs]++;
                            intraRoundCollBufsizeOffset += coll_bufsize;
                            targetAggsForMyDataFirstOffLenIndex[targetAggsForMyDataCurrentRoundIter[numTargetAggs]][numTargetAggs] = blockIter;
                            targetAggsForMyDataLastOffLenIndex[targetAggsForMyDataCurrentRoundIter[numTargetAggs]][numTargetAggs] = blockIter;
#ifdef onesidedtrace
                            printf("Rank %d - targetAggsForMyDataCurrentRoundI%d] is now %d intraRoundCollBufsizeOffset is now %ld\n",myrank,numTargetAggs,targetAggsForMyDataCurrentRoundIter[numTargetAggs],intraRoundCollBufsizeOffset);
#endif
                        } // while (thisAggBlockEnd >= intraRoundCollBufsizeOffset)
                    } // if (thisAggBlockEnd >= intraRoundCollBufsizeOffset)

                    int prevAggRankListIndex = currentAggRankListIndex;
                    currentAggRankListIndex++;

                    /* Skip over unused aggs.
                     */
                    if (fd_start[currentAggRankListIndex] > fd_end[currentAggRankListIndex]) {
                        while (fd_start[currentAggRankListIndex] > fd_end[currentAggRankListIndex])
                            currentAggRankListIndex++;
                    } // (fd_start[currentAggRankListIndex] > fd_end[currentAggRankListIndex])

                    /* Start new target agg.
                     */
                    if (blockEnd >= fd_start[currentAggRankListIndex]) {
                        numTargetAggs++;
                        targetAggsForMyData[numTargetAggs] = ca_data->ranklist[currentAggRankListIndex];
                        targetAggsForMyDataFDStart[numTargetAggs] = fd_start[currentAggRankListIndex];
                        /* Round up file domain to the first actual offset used if this is the first file domain.
                         */
                        if (currentAggRankListIndex == smallestFileDomainAggRank) {
                            if (targetAggsForMyDataFDStart[numTargetAggs] < firstFileOffset)
                                targetAggsForMyDataFDStart[numTargetAggs] = firstFileOffset;
                        }
                        targetAggsForMyDataFDEnd[numTargetAggs] = fd_end[currentAggRankListIndex];
                        /* Round down file domain to the last actual offset used if this is the last file domain.
                         */
                        if (currentAggRankListIndex == greatestFileDomainAggRank) {
                            if (targetAggsForMyDataFDEnd[numTargetAggs] > lastFileOffset)
                                targetAggsForMyDataFDEnd[numTargetAggs] = lastFileOffset;
                        }
                        targetAggsForMyDataFirstOffLenIndex[targetAggsForMyDataCurrentRoundIter[numTargetAggs]][numTargetAggs] = blockIter;
                        /* For the first additonal file domain the source buffer offset
                         * will be incremented relative to the state of this first main
                         * loop but for subsequent full file domains the offset will be
                         * incremented by the size
                         * of the file domain.
                         */
                        if (additionalFDCounter == 0)
                            amountToAdvanceSBOffsetForFD = (fd_end[prevAggRankListIndex] - blockStart) + (ADIO_Offset_CA)1;
                        else
                            amountToAdvanceSBOffsetForFD = (fd_end[prevAggRankListIndex] - fd_start[prevAggRankListIndex]) +(ADIO_Offset_CA)1;

                        if (bufTypeIsContig) {
                            HDassert(numTargetAggs > 0);
                            if (currentFDSourceBufferState[numTargetAggs].sourceBufferOffset == -1) {
                                if (additionalFDCounter == 0) { // first file domain, still use the current data counter
                                    currentFDSourceBufferState[numTargetAggs].sourceBufferOffset =
                                    currentSourceBufferOffset+amountToAdvanceSBOffsetForFD;
                                }
                                else { // 2nd file domain, advance full file domain from last source buffer state
                                    currentFDSourceBufferState[numTargetAggs].sourceBufferOffset =
                                    currentFDSourceBufferState[numTargetAggs-1].sourceBufferOffset+amountToAdvanceSBOffsetForFD;
                                }
#ifdef onesidedtrace
                                printf("Rank %d - Crossed into new FD - for agg %d sourceBufferOffset initialized to %ld amountToAdvanceSBOffsetForFD is %ld\n",myrank,numTargetAggs,currentFDSourceBufferState[numTargetAggs].sourceBufferOffset,amountToAdvanceSBOffsetForFD);
#endif
                            }
                        }
                        else if (currentFDSourceBufferState[numTargetAggs].indiceOffset == -1) {
                            /* non-contiguos source buffer */
                            HDassert(numTargetAggs > 0);

                            /* Initialize the source buffer state appropriately and then
                             * advance it with the
                             * H5FD_mpio_nc_buffer_advance function.
                             */
                            if (additionalFDCounter == 0) {
                                // first file domain, still use the current data counter
                                currentFDSourceBufferState[numTargetAggs].indiceOffset =
                                currentIndiceOffset;
                                currentFDSourceBufferState[numTargetAggs].bufTypeExtent = bufTypeExtent;
                                currentFDSourceBufferState[numTargetAggs].dataTypeExtent =
                                currentDataTypeExtent;
                                currentFDSourceBufferState[numTargetAggs].flatBufIndice =
                                currentFlatBufIndice;
                            }
                            else {
                                // 2nd file domain, advance full file domain from last source buffer state
                                currentFDSourceBufferState[numTargetAggs].indiceOffset =
                                currentFDSourceBufferState[numTargetAggs-1].indiceOffset;
                                currentFDSourceBufferState[numTargetAggs].bufTypeExtent =
                                currentFDSourceBufferState[numTargetAggs-1].bufTypeExtent;
                                currentFDSourceBufferState[numTargetAggs].dataTypeExtent =
                                currentFDSourceBufferState[numTargetAggs-1].dataTypeExtent;
                                currentFDSourceBufferState[numTargetAggs].flatBufIndice =
                                currentFDSourceBufferState[numTargetAggs-1].flatBufIndice;
                            }
                            H5FD_mpio_nc_buffer_advance(((char*)buf), memFlatBuf,
                            (int)amountToAdvanceSBOffsetForFD, 1,
                            &currentFDSourceBufferState[numTargetAggs], NULL);
#ifdef onesidedtrace
                            printf("Rank %d - Crossed into new FD - for agg %d dataTypeExtent initialized to %ld flatBufIndice to %d indiceOffset to %ld amountToAdvanceSBOffsetForFD is %d\n",myrank,numTargetAggs,currentFDSourceBufferState[numTargetAggs].dataTypeExtent,currentFDSourceBufferState[numTargetAggs].flatBufIndice,currentFDSourceBufferState[numTargetAggs].indiceOffset,amountToAdvanceSBOffsetForFD);
#endif
                        }
                        additionalFDCounter++;

#ifdef onesidedtrace
                        printf("Rank %d - block extended beyond fd init settings numTargetAggs %d offset_list[%d] with value %ld past fd border %ld with len %ld\n",myrank,numTargetAggs,blockIter,offset_list[blockIter],fd_start[currentAggRankListIndex],len_list[blockIter]);
#endif
                        intraRoundCollBufsizeOffset = fd_start[currentAggRankListIndex] + coll_bufsize;
                        targetAggsForMyDataLastOffLenIndex[targetAggsForMyDataCurrentRoundIter[numTargetAggs]][numTargetAggs] = blockIter;

                    } // if (blockEnd >= fd_start[currentAggRankListIndex])
                } // while (blockEnd > fd_end[currentAggRankListIndex])
            } // if (blockEnd > fd_end[currentAggRankListIndex])

            /* If we are still in the same file domain / target agg but have gone
            * past the coll_bufsize and need to advance to the next round -
            * initialize tracking data appropriately.
            */
            if (blockEnd >= intraRoundCollBufsizeOffset) {
                ADIO_Offset_CA currentBlockEnd = blockEnd;
                while (currentBlockEnd >= intraRoundCollBufsizeOffset) {
                    targetAggsForMyDataCurrentRoundIter[numTargetAggs]++;
                    intraRoundCollBufsizeOffset += coll_bufsize;
                    targetAggsForMyDataFirstOffLenIndex[targetAggsForMyDataCurrentRoundIter[numTargetAggs]][numTargetAggs] = blockIter;
                    targetAggsForMyDataLastOffLenIndex[targetAggsForMyDataCurrentRoundIter[numTargetAggs]][numTargetAggs] = blockIter;
#ifdef onesidedtrace
                    printf("Rank %d - smaller than fd currentBlockEnd is now %ld intraRoundCollBufsizeOffset is now %ld targetAggsForMyDataCurrentRoundIter[%d] is now %d\n",myrank,currentBlockEnd, intraRoundCollBufsizeOffset, numTargetAggs,targetAggsForMyDataCurrentRoundIter[numTargetAggs]);
#endif
                } // while (currentBlockEnd >= intraRoundCollBufsizeOffset)
            } // if (blockEnd >= intraRoundCollBufsizeOffset)

            /* Need to advance numTargetAggs if this is the last target offset to
            * include this one.
            */
            if (blockIter == (contig_access_count-1))
                numTargetAggs++;
        }

#ifdef onesidedtrace
        printf("Rank %d - numTargetAggs is %d\n",myrank,numTargetAggs);
        for (i=0;i<numTargetAggs;i++) {
            printf("Rank %d - targetAggsForMyDataCurrentRoundIter[%d] = %d\n",myrank,i,targetAggsForMyDataCurrentRoundIter[i]);
            for (j=0;j<=targetAggsForMyDataCurrentRoundIter[i];j++) {
                printf("Rank %d - targetAggsForMyDataFirstOffLenIndex[round %d][target agg %d] is %ld\n",myrank,j,i,targetAggsForMyDataFirstOffLenIndex[j][i]);
                fflush(stdout);
                printf("Rank %d - targetAggsForMyDataLastOffLenIndex[round %d][target agg %d] is %ld\n",myrank,j,i,targetAggsForMyDataLastOffLenIndex[j][i]);
                fflush(stdout);
                //printf("Rank %d - targetAggsForMyData[%d] is %d targetAggsForMyDataFDStart[%d] is %ld targetAggsForMyDataFDEnd is %ld targetAggsForMyDataFirstOffLenIndex is %d with value %ld targetAggsForMyDataLastOffLenIndex is %d with value %ld\n",myrank,i,targetAggsForMyData[i],i,targetAggsForMyDataFDStart[i],targetAggsForMyDataFDEnd[i],targetAggsForMyDataFirstOffLenIndex[j][i],offset_list[targetAggsForMyDataFirstOffLenIndex[j][i]],targetAggsForMyDataLastOffLenIndex[j][i],offset_list[targetAggsForMyDataLastOffLenIndex[j][i]]);
            }
        }
        printf("Rank %d - About to leave (contig_access_count > 0) loop...\n",myrank);
        fflush(stdout);
#endif

    } // if ((contig_access_count > 0) && (buf != NULL) && lenListOverZero)

#ifdef onesidedtrace
    printf("Rank %d - Done with (contig_access_count > 0) loop.\n",myrank);
    fflush(stdout);
#endif

    H5MM_free(targetAggsForMyDataCurrentRoundIter);


#ifdef onesidedtrace
    printf("Rank %d - targetAggsForMyDataCurrentRoundIter freed.\n",myrank);
    fflush(stdout);
#endif

    /* use the write buffer allocated in the file_open */
    char *write_buf;
    MPI_Win write_buf_window;
    if(!ca_data->onesided_no_rmw) {
        hole_found = 0;
    }

    /* Async I/O - Adjust if this is the "duplicate" buffer */
    if (ca_data->use_dup) {
        write_buf = ca_data->io_buf_d;
        write_buf_window = ca_data->io_buf_window_d;
    } else {
        write_buf = ca_data->io_buf;
        write_buf_window = ca_data->io_buf_window;
    }

#ifdef onesidedtrace
    printf("Rank %d - write_buf and write_buf_window set.\n",myrank);
    fflush(stdout);
#endif

    /* Counters to track the offset range being written by the used aggs.
    */
    ADIO_Offset_CA currentRoundFDStart = 0;
    ADIO_Offset_CA currentRoundFDEnd = 0;

    if (iAmUsedAgg) {
        currentRoundFDStart = fd_start[myAggRank];
        currentRoundFDEnd = fd_end[myAggRank];
        if (myAggRank == smallestFileDomainAggRank) {
            if (currentRoundFDStart < firstFileOffset)
            currentRoundFDStart = firstFileOffset;
        }
        else if (myAggRank == greatestFileDomainAggRank) {
            if (currentRoundFDEnd > lastFileOffset)
            currentRoundFDEnd = lastFileOffset;
        }
#ifdef onesidedtrace
        printf("Rank %d - iAmUsedAgg - currentRoundFDStart initialized to %ld currentRoundFDEnd to %ld\n",myrank,currentRoundFDStart,currentRoundFDEnd);
#endif

        if ((stripeSize > 0) && (segmentIter == 0)) {
            stripe_parms->numStripesUsed = 0;
            stripe_parms->stripeIOoffsets = (MPI_Offset *) H5MM_malloc(stripe_parms->stripesPerAgg*sizeof(MPI_Offset));
            stripe_parms->stripeIOLens = (int *) H5MM_malloc(stripe_parms->stripesPerAgg*sizeof(int));
            stripe_parms->amountOfStripedDataExpected = 0;
            int stripeIter = 0;
            for (stripeIter=0;stripeIter<stripe_parms->stripesPerAgg;stripeIter++) {
                if (stripeIter == 0) {
                    stripe_parms->stripeIOoffsets[stripeIter] = currentRoundFDStart;
                    stripe_parms->stripeIOLens[stripeIter] = (int)(currentRoundFDEnd - currentRoundFDStart)+1;
                    stripe_parms->amountOfStripedDataExpected += (int)(currentRoundFDEnd - currentRoundFDStart)+1;
                    stripe_parms->numStripesUsed++;
                }
                else {
                    if (((currentRoundFDEnd + (ADIO_Offset_CA)1 + ((ADIO_Offset_CA)stripeIter * stripe_parms->segmentLen))) > stripe_parms->stripedLastFileOffset) {
                        if (((currentRoundFDEnd + (ADIO_Offset_CA)1 - (ADIO_Offset_CA)(stripe_parms->stripeSize) + ((ADIO_Offset_CA)stripeIter * stripe_parms->segmentLen))) <= stripe_parms->stripedLastFileOffset) {
                            stripe_parms->stripeIOoffsets[stripeIter] = (currentRoundFDEnd + (ADIO_Offset_CA)1) - (ADIO_Offset_CA)(stripe_parms->stripeSize) + ((ADIO_Offset_CA)stripeIter * stripe_parms->segmentLen);
                            stripe_parms->stripeIOLens[stripeIter] = (int)(stripe_parms->stripedLastFileOffset - (currentRoundFDEnd + (ADIO_Offset_CA)1 - (ADIO_Offset_CA)(stripe_parms->stripeSize) + ((ADIO_Offset_CA)stripeIter * stripe_parms->segmentLen)) + (ADIO_Offset_CA)1);
                            stripe_parms->amountOfStripedDataExpected += (int)(stripe_parms->stripedLastFileOffset - (currentRoundFDEnd + (ADIO_Offset_CA)1 - (ADIO_Offset_CA)(stripe_parms->stripeSize) + ((ADIO_Offset_CA)stripeIter * stripe_parms->segmentLen)) + (ADIO_Offset_CA)1);
                            stripe_parms->numStripesUsed++;
                        }
                    }
                    else {
                        stripe_parms->stripeIOoffsets[stripeIter] = (currentRoundFDEnd + (ADIO_Offset_CA)1) - (ADIO_Offset_CA)(stripe_parms->stripeSize) + ((ADIO_Offset_CA)stripeIter * stripe_parms->segmentLen);
                        stripe_parms->stripeIOLens[stripeIter] = stripe_parms->stripeSize;
                        stripe_parms->amountOfStripedDataExpected += stripe_parms->stripeSize;
                        stripe_parms->numStripesUsed++;
                    }
                }
            } // for-loop
#ifdef onesidedtrace
            printf("Rank %d - stripe_parms->amountOfStripedDataExpected is %d stripe_parms->numStripesUsed is %d offsets and lengths are ",myrank,stripe_parms->amountOfStripedDataExpected,stripe_parms->numStripesUsed);
            for (i=0;i<stripe_parms->numStripesUsed;i++) {
                printf("%ld %ld --",stripe_parms->stripeIOoffsets[i],stripe_parms->stripeIOLens[i]);
            }
            printf("\n");
#endif
        } // if ((stripe_parms->stripeSize>0) && (stripe_parms->segmentIter==0))

        if (ca_data->onesided_always_rmw && ((stripeSize==0) || (stripe_parms->segmentIter==0))) { // read in the first buffer
            ADIO_Offset_CA tmpCurrentRoundFDEnd = 0;
            if ((fd_end[myAggRank] - currentRoundFDStart) < coll_bufsize) {
                if (myAggRank == greatestFileDomainAggRank) {
                    if (fd_end[myAggRank] > lastFileOffset)
                    tmpCurrentRoundFDEnd = lastFileOffset;
                    else
                    tmpCurrentRoundFDEnd = fd_end[myAggRank];
                }
                else
                tmpCurrentRoundFDEnd = fd_end[myAggRank];
            }
            else
            tmpCurrentRoundFDEnd = currentRoundFDStart + coll_bufsize - (ADIO_Offset_CA)1;
#ifdef onesidedtrace
            printf("Rank %d - ca_data->onesided_always_rmw - first buffer pre-read for file offsets %ld to %ld total is %d\n",myrank,currentRoundFDStart,tmpCurrentRoundFDEnd,(int)(tmpCurrentRoundFDEnd - currentRoundFDStart)+1);
#endif
            if (stripeSize==0) {
                MPI_File_read_at(ca_data->fh, currentRoundFDStart, write_buf, (int)(tmpCurrentRoundFDEnd - currentRoundFDStart)+1,
                MPI_BYTE,  error_code);
            }
            else {
                /* pre-read the entire batch of stripes we will do before writing */
                int stripeIter = 0;
                for (stripeIter=0;stripeIter<stripe_parms->numStripesUsed;stripeIter++)
                    MPI_File_read_at(ca_data->fh, stripe_parms->stripeIOoffsets[stripeIter], (char*)write_buf + ((ADIO_Offset_CA)stripeIter * (ADIO_Offset_CA)stripeSize), stripe_parms->stripeIOLens[stripeIter], MPI_BYTE,  error_code);
            }
        }

    } // if iAmUsedAgg

    if (ca_data->onesided_always_rmw && ((stripeSize == 0) || (segmentIter == 0))) // wait until the first buffer is read
        MPI_Barrier(ca_data->comm);

#ifdef onesidedtrace
    MPI_Barrier(ca_data->comm);
    if(myrank==0) { printf("\n\n"); fflush(stdout); }
    MPI_Barrier(ca_data->comm);
    printf("Rank %d is waiting at barrier between main loops.\n", myrank);
    printf("Rank %d -- numberOfRounds = %d, contig_access_count = %d, numTargetAggs = %d\n", myrank, numberOfRounds, contig_access_count, numTargetAggs);
    fflush(stdout);
    MPI_Barrier(ca_data->comm);
    if(myrank==0) { printf("\n\n"); fflush(stdout); }
    MPI_Barrier(ca_data->comm);
#endif

    /* This is the second main loop of the algorithm, actually nested loop of target aggs within rounds.  There are 2 flavors of this.
    * For onesided_write_aggmethod of 1 each nested iteration for the target
    * agg does an mpi_put on a contiguous chunk using a primative datatype
    * determined using the data structures from the first main loop.  For
    * onesided_write_aggmethod of 2 each nested iteration for the target agg
    * builds up data to use in created a derived data type for 1 mpi_put that is done for the target agg for each round.
    * To support lustre there will need to be an additional layer of nesting
    * for the multiple file domains within target aggs.
    */
    int roundIter;

    for (roundIter=0;roundIter<numberOfRounds;roundIter++) {
        if ((contig_access_count > 0) && (buf != NULL) && lenListOverZero) {

            int aggIter;
            for (aggIter=0;aggIter<numTargetAggs;aggIter++) {

                int numBytesPutThisAggRound = 0;
                /* If we have data for the round/agg process it.
                */
                if (targetAggsForMyDataFirstOffLenIndex[roundIter][aggIter] != -1) {
                    ADIO_Offset_CA currentRoundFDStartForMyTargetAgg = (ADIO_Offset_CA)((ADIO_Offset_CA)targetAggsForMyDataFDStart[aggIter] + (ADIO_Offset_CA)((ADIO_Offset_CA)roundIter*coll_bufsize));
                    ADIO_Offset_CA currentRoundFDEndForMyTargetAgg = (ADIO_Offset_CA)((ADIO_Offset_CA)targetAggsForMyDataFDStart[aggIter] + (ADIO_Offset_CA)((ADIO_Offset_CA)(roundIter+1)*coll_bufsize) - (ADIO_Offset_CA)1);

                    int targetAggContigAccessCount = 0;

                    /* These data structures are used for the derived datatype mpi_put
                    * in the onesided_write_aggmethod of 2 case.
                    */
                    int *targetAggBlockLengths=NULL;
                    MPI_Aint *targetAggDisplacements=NULL, *sourceBufferDisplacements=NULL;
                    MPI_Datatype *targetAggDataTypes=NULL;

                    char *derivedTypePackedSourceBuffer=NULL;
                    int derivedTypePackedSourceBufferOffset = 0;
                    int allocatedDerivedTypeArrays = 0;
                    ADIO_Offset_CA amountOfDataWrittenThisRoundAgg = 0;

#ifdef onesidedtrace
                    printf("Rank %d - roundIter %d processing targetAggsForMyData %d \n",myrank,roundIter,targetAggsForMyData[aggIter]);
#endif

                    /* Process the range of offsets for this target agg.
                     */
                    int offsetIter;
                    int startingOffLenIndex = targetAggsForMyDataFirstOffLenIndex[roundIter][aggIter], endingOffLenIndex = targetAggsForMyDataLastOffLenIndex[roundIter][aggIter];
                    for (offsetIter=startingOffLenIndex;offsetIter<=endingOffLenIndex;offsetIter++) {
                        if (currentRoundFDEndForMyTargetAgg > targetAggsForMyDataFDEnd[aggIter])
                            currentRoundFDEndForMyTargetAgg = targetAggsForMyDataFDEnd[aggIter];

                        ADIO_Offset_CA offsetStart = offset_list[offsetIter], offsetEnd = (offset_list[offsetIter]+len_list[offsetIter]-(ADIO_Offset_CA)1);

#ifdef onesidedtrace
                        printf("Rank %d - roundIter %d target iter %d targetAggsForMyData is %d offset_list[%d] is %ld len_list[%d] is %ld targetAggsForMyDataFDStart is %ld targetAggsForMyDataFDEnd is %ld currentRoundFDStartForMyTargetAgg is %ld currentRoundFDEndForMyTargetAgg is %ld targetAggsForMyDataFirstOffLenIndex is %ld\n",
                        myrank,roundIter,aggIter,targetAggsForMyData[aggIter],offsetIter,offset_list[offsetIter],offsetIter,len_list[offsetIter],
                        targetAggsForMyDataFDStart[aggIter],targetAggsForMyDataFDEnd[aggIter],
                        currentRoundFDStartForMyTargetAgg,currentRoundFDEndForMyTargetAgg, targetAggsForMyDataFirstOffLenIndex[roundIter][aggIter]);
#endif

                        /* Determine the amount of data and exact source buffer offsets to use.
                         */
                        int bufferAmountToSend = 0;

                        if ((offsetStart >= currentRoundFDStartForMyTargetAgg) && (offsetStart <= currentRoundFDEndForMyTargetAgg)) {
                            if (offsetEnd > currentRoundFDEndForMyTargetAgg)
                                bufferAmountToSend = (currentRoundFDEndForMyTargetAgg - offsetStart) +1;
                            else
                                bufferAmountToSend = (offsetEnd - offsetStart) +1;
                        }
                        else if ((offsetEnd >= currentRoundFDStartForMyTargetAgg) && (offsetEnd <= currentRoundFDEndForMyTargetAgg)) {
                            if (offsetEnd > currentRoundFDEndForMyTargetAgg)
                                bufferAmountToSend = (currentRoundFDEndForMyTargetAgg - currentRoundFDStartForMyTargetAgg) +1;
                            else
                                bufferAmountToSend = (offsetEnd - currentRoundFDStartForMyTargetAgg) +1;
                            if (offsetStart < currentRoundFDStartForMyTargetAgg) {
                                offsetStart = currentRoundFDStartForMyTargetAgg;
                            }
                        }
                        else if ((offsetStart <= currentRoundFDStartForMyTargetAgg) && (offsetEnd >= currentRoundFDEndForMyTargetAgg)) {
                            bufferAmountToSend = (currentRoundFDEndForMyTargetAgg - currentRoundFDStartForMyTargetAgg) +1;
                            offsetStart = currentRoundFDStartForMyTargetAgg;
                        }

                        numBytesPutThisAggRound += bufferAmountToSend;
#ifdef onesidedtrace
                        printf("Rank %d - bufferAmountToSend is %d\n",myrank,bufferAmountToSend);
#endif
                        if (bufferAmountToSend > 0) { /* we have data to send this round */

                            if (ca_data->onesided_write_aggmethod == 2) {
                                /* Only allocate these arrays if we are using method 2 and only do it once for this round/target agg.
                                 */
                                if (!allocatedDerivedTypeArrays) {
                                    targetAggBlockLengths = (int *)H5MM_malloc(maxNumContigOperations * sizeof(int));
                                    targetAggDisplacements = (MPI_Aint *)H5MM_malloc(maxNumContigOperations * sizeof(MPI_Aint));
                                    sourceBufferDisplacements = (MPI_Aint *)H5MM_malloc(maxNumContigOperations * sizeof(MPI_Aint));
                                    targetAggDataTypes = (MPI_Datatype *)H5MM_malloc(maxNumContigOperations * sizeof(MPI_Datatype));
                                    if (!bufTypeIsContig) {
                                        int k;
                                        for (k=targetAggsForMyDataFirstOffLenIndex[roundIter][aggIter];k<=targetAggsForMyDataLastOffLenIndex[roundIter][aggIter];k++)
                                            amountOfDataWrittenThisRoundAgg += len_list[k];

#ifdef onesidedtrace
                                        printf("Rank %d - derivedTypePackedSourceBuffer mallocing %ld\n",myrank,amountOfDataWrittenThisRoundAgg);
#endif

                                        if (amountOfDataWrittenThisRoundAgg > 0)
                                            derivedTypePackedSourceBuffer = (char *)H5MM_malloc(amountOfDataWrittenThisRoundAgg * sizeof(char));
                                        else
                                            derivedTypePackedSourceBuffer = NULL;
                                    }
                                    allocatedDerivedTypeArrays = 1;
                                }
                            }

                            /* Determine the offset into the target window.
                             */
                            MPI_Aint targetDisplacementToUseThisRound = (MPI_Aint) (offsetStart - currentRoundFDStartForMyTargetAgg)  + ((MPI_Aint)(segmentIter)*(MPI_Aint)(stripeSize));


                            /* For onesided_write_aggmethod of 1 do the mpi_put using the primitive MPI_BYTE type for each contiguous
                             * chunk in the target, of source data is non-contiguous then pack the data first.
                             */
                            if (ca_data->onesided_write_aggmethod == 1) {

                                MPI_Win_lock(MPI_LOCK_SHARED, targetAggsForMyData[aggIter], MPI_MODE_NOCHECK, write_buf_window);

                                char *putSourceData;
                                if (bufTypeIsContig) {
#ifdef onesidedtrace
                                    printf("Rank %d - ca_data->onesided_write_aggmethod == 1 currentFDSourceBufferState[%d].sourceBufferOffset is %ld bufferAmountToSend is %d targetAggsForMyData[aggIter] is %d targetDisplacementToUseThisRound is %d write_buf_window is %016lx\n",myrank,aggIter,currentFDSourceBufferState[aggIter].sourceBufferOffset,bufferAmountToSend,targetAggsForMyData[aggIter],targetDisplacementToUseThisRound,write_buf_window);
                                    fflush(stdout);
#endif
                                    MPI_Put(((char*)buf) + currentFDSourceBufferState[aggIter].sourceBufferOffset,bufferAmountToSend, MPI_BYTE,targetAggsForMyData[aggIter],targetDisplacementToUseThisRound, bufferAmountToSend,MPI_BYTE,write_buf_window);
                                    currentFDSourceBufferState[aggIter].sourceBufferOffset += (ADIO_Offset_CA)bufferAmountToSend;
                                }
                                else {
                                    putSourceData = (char *) H5MM_malloc(bufferAmountToSend*sizeof(char));
                                    H5FD_mpio_nc_buffer_advance(((char*)buf), memFlatBuf, bufferAmountToSend, 1, &currentFDSourceBufferState[aggIter], putSourceData);

                                    MPI_Put(putSourceData,bufferAmountToSend, MPI_BYTE,targetAggsForMyData[aggIter],targetDisplacementToUseThisRound, bufferAmountToSend,MPI_BYTE,write_buf_window);
                                }

                                MPI_Win_unlock(targetAggsForMyData[aggIter], write_buf_window);

                                if (!bufTypeIsContig)
                                H5MM_free(putSourceData);
                            }


                            /* For aggmethod 2, populate the data structures for this round/agg for this offset iter
                             * to be used subsequently when building the derived type for 1 mpi_put for all the data for this
                             * round/agg.
                             */
                            else if (ca_data->onesided_write_aggmethod == 2) {

                                if (bufTypeIsContig) {
                                    targetAggBlockLengths[targetAggContigAccessCount]= bufferAmountToSend;
                                    targetAggDataTypes[targetAggContigAccessCount] = MPI_BYTE;
                                    targetAggDisplacements[targetAggContigAccessCount] = targetDisplacementToUseThisRound;
                                    sourceBufferDisplacements[targetAggContigAccessCount] = (MPI_Aint)currentFDSourceBufferState[aggIter].sourceBufferOffset;
                                    currentFDSourceBufferState[aggIter].sourceBufferOffset += (ADIO_Offset_CA)bufferAmountToSend;
                                    targetAggContigAccessCount++;
                                }
                                else {
                                    H5FD_mpio_nc_buffer_advance(((char*)buf), memFlatBuf, bufferAmountToSend, 1, &currentFDSourceBufferState[aggIter], &derivedTypePackedSourceBuffer[derivedTypePackedSourceBufferOffset]);
                                    targetAggBlockLengths[targetAggContigAccessCount]= bufferAmountToSend;
                                    targetAggDataTypes[targetAggContigAccessCount] = MPI_BYTE;
                                    targetAggDisplacements[targetAggContigAccessCount] = targetDisplacementToUseThisRound;
                                    sourceBufferDisplacements[targetAggContigAccessCount] = (MPI_Aint)derivedTypePackedSourceBufferOffset;
                                    targetAggContigAccessCount++;
                                    derivedTypePackedSourceBufferOffset += (ADIO_Offset_CA)bufferAmountToSend;
                                }
                            }

#ifdef onesidedtrace
                            printf("Rank %d - roundIter %d bufferAmountToSend is %d offsetStart is %ld currentRoundFDStartForMyTargetAgg is %ld currentRoundFDEndForMyTargetAgg is %ld targetDisplacementToUseThisRound is %ld targetAggsForMyDataFDStart[aggIter] is %ld\n",myrank,roundIter, bufferAmountToSend, offsetStart,currentRoundFDStartForMyTargetAgg,currentRoundFDEndForMyTargetAgg,targetDisplacementToUseThisRound,targetAggsForMyDataFDStart[aggIter]);
#endif

                        } // bufferAmountToSend > 0
                    } // contig list

                    /* For aggmethod 2, Now build the derived type using the data from this round/agg and do 1 single mpi_put.
                    */
                    if (ca_data->onesided_write_aggmethod == 2) {

                        MPI_Datatype sourceBufferDerivedDataType, targetBufferDerivedDataType;
                        MPI_Type_create_struct(targetAggContigAccessCount, targetAggBlockLengths, sourceBufferDisplacements, targetAggDataTypes, &sourceBufferDerivedDataType);
                        MPI_Type_commit(&sourceBufferDerivedDataType);
                        MPI_Type_create_struct(targetAggContigAccessCount, targetAggBlockLengths, targetAggDisplacements, targetAggDataTypes, &targetBufferDerivedDataType);
                        MPI_Type_commit(&targetBufferDerivedDataType);

#ifdef onesidedtrace
                        printf("Rank %d - mpi_put of derived type to agg %d targetAggContigAccessCount is %d\n",myrank,targetAggsForMyData[aggIter],targetAggContigAccessCount);
#endif

                        if (targetAggContigAccessCount > 0) {

#ifdef onesidedtrace
                            printf("Rank %d - Calling 1st MPI_Win_lock\n",myrank);
                            fflush(stdout);
#endif

                            MPI_Win_lock(MPI_LOCK_SHARED, targetAggsForMyData[aggIter], MPI_MODE_NOCHECK, write_buf_window);

                            if (bufTypeIsContig) {
#ifdef onesidedtrace
                                printf("Rank %d - Calling MPI_Put with bufTypeIsContig==TRUE, aggIter %ld, targetAggsForMyData[aggIter] is %ld\n",myrank,aggIter,targetAggsForMyData[aggIter]);
                                fflush(stdout);
#endif
                                MPI_Put(((char*)buf),1, sourceBufferDerivedDataType,targetAggsForMyData[aggIter],0, 1,targetBufferDerivedDataType,write_buf_window);
                            }
                            else {

#ifdef onesidedtrace
                                printf("Rank %d - Calling MPI_Put with bufTypeIsContig==FALSE, aggIter %ld, targetAggsForMyData[aggIter] is %ld\n",myrank,aggIter,targetAggsForMyData[aggIter]);
                                fflush(stdout);
#endif
                                MPI_Put(derivedTypePackedSourceBuffer,1, sourceBufferDerivedDataType,targetAggsForMyData[aggIter],0, 1,targetBufferDerivedDataType,write_buf_window);
                            }
#ifdef onesidedtrace
                            printf("Rank %d - Calling 1st MPI_Win_UNlock\n",myrank);
                            fflush(stdout);
#endif
                            MPI_Win_unlock(targetAggsForMyData[aggIter], write_buf_window);
                            //MPI_Win_fence(0, write_buf_window);
                        }

                        if (allocatedDerivedTypeArrays) {
                            H5MM_free(targetAggBlockLengths);
                            H5MM_free(targetAggDisplacements);
                            H5MM_free(targetAggDataTypes);
                            H5MM_free(sourceBufferDisplacements);
                            if (!bufTypeIsContig)
                            if (derivedTypePackedSourceBuffer != NULL)
                            H5MM_free(derivedTypePackedSourceBuffer);
                        }
                        if (targetAggContigAccessCount > 0) {
                            MPI_Type_free(&sourceBufferDerivedDataType);
                            MPI_Type_free(&targetBufferDerivedDataType);
                        }

                    }

                    if (!ca_data->onesided_no_rmw) {

                        MPI_Win io_buf_put_amounts_window_use = ca_data->io_buf_put_amounts_window;
                        if (ca_data->use_dup) {
                            io_buf_put_amounts_window_use = ca_data->io_buf_put_amounts_window_d;
                        }
#ifdef onesidedtrace
                        printf("Rank %d - Calling 2nd MPI_Win_lock\n",myrank);
                        fflush(stdout);
#endif
                        MPI_Win_lock(MPI_LOCK_SHARED, targetAggsForMyData[aggIter], MPI_MODE_NOCHECK, io_buf_put_amounts_window_use);
#ifdef onesidedtrace
                        printf("Rank %d - Calling MPI_Accumulate\n",myrank);
                        fflush(stdout);
#endif
                        MPI_Accumulate(&numBytesPutThisAggRound,1, MPI_INT,targetAggsForMyData[aggIter],0, 1, MPI_INT, MPI_SUM, io_buf_put_amounts_window_use);
#ifdef onesidedtrace
                        printf("Rank %d - Calling 2nd MPI_Win_UNlock\n",myrank);
                        fflush(stdout);
#endif
                        MPI_Win_unlock(targetAggsForMyData[aggIter], io_buf_put_amounts_window_use);
                    }

                } // baseoffset != -1
            } // target aggs

            if (stripeSize > 0) {
                stripe_parms->lastDataTypeExtent = currentFDSourceBufferState[numTargetAggs-1].dataTypeExtent;
                stripe_parms->lastFlatBufIndice = currentFDSourceBufferState[numTargetAggs-1].flatBufIndice;
                stripe_parms->lastIndiceOffset = currentFDSourceBufferState[numTargetAggs-1].indiceOffset;
#ifdef onesidedtrace
                printf("Rank %d - setting stripe_parms->lastDataTypeExtent %ld stripe_parms->lastFlatBufIndice %d stripe_parms->lastIndiceOffset %ld\n",myrank,stripe_parms->lastDataTypeExtent,stripe_parms->lastFlatBufIndice,stripe_parms->lastIndiceOffset);
#endif
            }

        } /// contig_access_count > 0

        /* Synchronize all procs before the file write */
        if ((stripeSize == 0) || (stripe_parms->flushCB)) {
#ifdef onesidedtrace
          printf("Rank %d - first barrier roundIter %d\n",myrank,roundIter);
#endif
            MPI_Barrier(ca_data->comm);
        }

        if ((iAmUsedAgg || stripe_parms->iWasUsedStripingAgg)  && ((stripeSize == 0) || (stripe_parms->flushCB))) {
            stripe_parms->iWasUsedStripingAgg = 0;
            /* Determine what offsets define the portion of the file domain the agg is writing this round.
            */
            if (iAmUsedAgg) {
                if ((fd_end[myAggRank] - currentRoundFDStart) < coll_bufsize) {
                    if (myAggRank == greatestFileDomainAggRank) {
                        if (fd_end[myAggRank] > lastFileOffset)
                        currentRoundFDEnd = lastFileOffset;
                        else
                        currentRoundFDEnd = fd_end[myAggRank];
                    }
                    else
                    currentRoundFDEnd = fd_end[myAggRank];
                }
                else
                currentRoundFDEnd = currentRoundFDStart + coll_bufsize - (ADIO_Offset_CA)1;
#ifdef onesidedtrace
                printf("current used agg about to writecontig - currentRoundFDStart is %ld currentRoundFDEnd is %ld within file domain %ld to %ld\n",currentRoundFDStart,currentRoundFDEnd,fd_start[myAggRank],fd_end[myAggRank]);
#endif
            }
#ifdef onesidedtrace
            else {
                printf("former used agg about to writecontig\n");
            }
#endif
            int doWriteContig = 1;
            int tmp_put_amt = ca_data->io_buf_put_amounts;
            if (ca_data->use_dup) tmp_put_amt = ca_data->io_buf_put_amounts_d;

            if (!ca_data->onesided_no_rmw) {
                if (stripeSize == 0) {
                    if (tmp_put_amt != ((int)(currentRoundFDEnd - currentRoundFDStart)+1)) {
                        doWriteContig = 0;
                        hole_found = 1;
#ifdef onesidedtrace
                          printf("hole found --- ca_data->io_buf_put_amounts is %d currentRoundFDEnd is %ld currentRoundFDStart is %ld on roundIter %d\n",tmp_put_amt,currentRoundFDEnd,currentRoundFDStart,roundIter);
#endif
                    }
                }
                else { // file striping
                    if (tmp_put_amt != stripe_parms->amountOfStripedDataExpected) {
                        doWriteContig = 0;
                        hole_found = 1;
#ifdef onesidedtrace
                        printf("striping hole found --- ca_data->io_buf_put_amounts is %d stripe_parms->amountOfStripedDataExpected is %d on roundIter %d\n",tmp_put_amt,stripe_parms->amountOfStripedDataExpected,roundIter);
#endif
                    }
                }
                if (ca_data->use_dup)
                    ca_data->io_buf_put_amounts_d = 0;
                else
                    ca_data->io_buf_put_amounts = 0;
            }

            if (doWriteContig) {
                if (stripeSize > 0) {
#ifdef onesidedtrace
                    printf("about to write out %d stripes\n",stripe_parms->numStripesUsed);
#endif

                    int stripeIter = 0;
                    for (stripeIter=0;stripeIter<stripe_parms->numStripesUsed;stripeIter++) {

#ifdef onesidedtrace
                        printf("writing write_buf offset %ld len %ld file offset %ld\n",((ADIO_Offset_CA)stripeIter * (ADIO_Offset_CA)(stripeSize)),stripe_parms->stripeIOLens[stripeIter],stripe_parms->stripeIOoffsets[stripeIter]);
#endif
                        if (ca_data->check_req) {
                            MPIO_Wait(&ca_data->io_Request, error_code);
                            ca_data->check_req = 0;
                        }

                        MPI_File_iwrite_at(ca_data->fh, stripe_parms->stripeIOoffsets[stripeIter], (char*)(write_buf + ((ADIO_Offset_CA)stripeIter * (ADIO_Offset_CA)(stripeSize))), stripe_parms->stripeIOLens[stripeIter], MPI_BYTE, &ca_data->io_Request);

                        if (ca_data->async_io_outer && 0) {
                            ca_data->check_req = 1;
                        } else {
                            MPIO_Wait(&ca_data->io_Request, error_code);
                            ca_data->check_req = 0;
                        }

                    }
                    H5MM_free(stripe_parms->stripeIOLens);
                    H5MM_free(stripe_parms->stripeIOoffsets);
                }
                else {

                    if (ca_data->check_req) {
                        MPIO_Wait(&ca_data->io_Request, error_code);
                        ca_data->check_req = 0;
                    }

                    MPI_File_iwrite_at(ca_data->fh, currentRoundFDStart, write_buf, (int)(currentRoundFDEnd - currentRoundFDStart)+1, MPI_BYTE, &ca_data->io_Request);

                    if (ca_data->async_io_outer) {
                        ca_data->check_req = 1;
                    } else {
                        MPIO_Wait(&ca_data->io_Request, error_code);
                        ca_data->check_req = 0;
                    }

                }
            }
        } // iAmUsedAgg

        if (iAmUsedAgg && stripeSize == 0) {
            currentRoundFDStart += coll_bufsize;

            if (ca_data->onesided_always_rmw && (roundIter<(numberOfRounds-1))) { // read in the buffer for the next round unless this is the last round
                ADIO_Offset_CA tmpCurrentRoundFDEnd = 0;
                if ((fd_end[myAggRank] - currentRoundFDStart) < coll_bufsize) {
                    if (myAggRank == greatestFileDomainAggRank) {
                        if (fd_end[myAggRank] > lastFileOffset)
                        tmpCurrentRoundFDEnd = lastFileOffset;
                        else
                        tmpCurrentRoundFDEnd = fd_end[myAggRank];
                    }
                    else
                    tmpCurrentRoundFDEnd = fd_end[myAggRank];
                }
                else
                tmpCurrentRoundFDEnd = currentRoundFDStart + coll_bufsize - (ADIO_Offset_CA)1;
#ifdef onesidedtrace
                printf("Rank %d - ca_data->onesided_always_rmw - round %d buffer pre-read for file offsets %ld to %ld total is %d\n",myrank,roundIter, currentRoundFDStart,tmpCurrentRoundFDEnd,(int)(tmpCurrentRoundFDEnd - currentRoundFDStart)+1);
#endif
                MPI_File_read_at(ca_data->fh, currentRoundFDStart, write_buf, (int)(tmpCurrentRoundFDEnd - currentRoundFDStart)+1,
                MPI_BYTE, error_code);
            }
        }

        if (roundIter<(numberOfRounds-1)) {
#ifdef onesidedtrace
            printf("second barrier roundIter %d --- waiting in loop this time\n",roundIter);
#endif
            MPI_Barrier(ca_data->comm);
        }

    } /* for-loop roundIter */

#ifdef onesidedtrace
    printf("freeing datastructures\n");
#endif
    H5MM_free(targetAggsForMyData);
    H5MM_free(targetAggsForMyDataFDStart);
    H5MM_free(targetAggsForMyDataFDEnd);

    for (i=0;i<numberOfRounds;i++) {
        H5MM_free(targetAggsForMyDataFirstOffLenIndex[i]);
        H5MM_free(targetAggsForMyDataLastOffLenIndex[i]);
    }
    H5MM_free(targetAggsForMyDataFirstOffLenIndex);
    H5MM_free(targetAggsForMyDataLastOffLenIndex);

    H5MM_free(currentFDSourceBufferState);

    return;
} /* H5FD_mpio_ccio_osagg_write */

/*-------------------------------------------------------------------------
 * Function:    H5FD_mpio_ccio_osagg_read
 *
 * Purpose:     One-sided collective READ implementation.
 *
 *-------------------------------------------------------------------------
 */
void H5FD_mpio_ccio_osagg_read(CustomAgg_FH_Data ca_data,
     ADIO_Offset_CA *offset_list,
     ADIO_Offset_CA *len_list,
     int contig_access_count,
     const void *buf,
     H5S_flatbuf_t *flatBuf,
     int *error_code,
     ADIO_Offset_CA firstFileOffset,
     ADIO_Offset_CA lastFileOffset,
     int numNonZeroDataOffsets,
     ADIO_Offset_CA *fd_start,
     ADIO_Offset_CA* fd_end,
     FS_Block_Parms *stripe_parms,
     int do_file_read)
{
    int i,j; /* generic iterators */

    /*
     * Make local copy of certain ADIOI_OneSidedStripeParms elements for
     * faster access - pay for pointer dereference only once.
     */
    int stripeSize = stripe_parms->stripeSize;
    int segmentIter = stripe_parms->segmentIter;
    hsize_t bufTypeExtent = stripe_parms->bufTypeExtent;

    if ((stripeSize > 0) && stripe_parms->firstStripedIOCall)
        stripe_parms->iWasUsedStripingAgg = 0;

#ifdef onesidedtrace
     if (buf == NULL) {
         printf("H5FD_mpio_ccio_osagg_read - buf is NULL contig_access_count is %d\n",contig_access_count);
         for (i=0;i<contig_access_count;i++)
         printf("offset_list[%d] is %ld len_list[%d] is %ld\n", i,offset_list[i],i,len_list[i]);
     }
     if (contig_access_count < 0) {
         printf("H5FD_mpio_ccio_osagg_read - contig_access_count of %d is less than 0\n",contig_access_count);
     }
#endif

    int lenListOverZero = 0;
    for (i=0;((i<contig_access_count) && (!lenListOverZero));i++) {
        if (len_list[i] > 0) lenListOverZero = 1;
    }

    *error_code = MPI_SUCCESS; /* initialize to success */

    MPI_Status status;

    pthread_t io_thread;
    void *thread_ret;
    ThreadFuncData io_thread_args;

    int nprocs,myrank;
    MPI_Comm_size(ca_data->comm, &nprocs);
    MPI_Comm_rank(ca_data->comm, &myrank);

#ifdef onesidedtrace
    printf("Rank %d - H5FD_mpio_ccio_osagg_read started\n",myrank);
#endif

    if (ca_data->io_buf_window == MPI_WIN_NULL || ca_data->io_buf_put_amounts_window == MPI_WIN_NULL)
    {
        HDF5_ccio_win_setup(ca_data, nprocs);
    }

     /* This flag denotes whether the source datatype is contiguous, which is referenced throughout the algorithm
     * and defines how the source buffer offsets and data chunks are determined.  If the value is 1 (true - contiguous data)
     * things are profoundly simpler in that the source buffer offset for a given target offset simply linearly increases
     * by the chunk sizes being written.  If the value is 0 (non-contiguous) then these values are based on calculations
     * from the flattened source datatype.
     */
     int bufTypeIsContig;
     if (flatBuf->count == 1)
        bufTypeIsContig = 1;
     else
        bufTypeIsContig = 0;

     if (!bufTypeIsContig) {
         /* For a non-contiguous source buffer set the extent. */
         if ((stripeSize == 0) || stripe_parms->firstStripedIOCall) {
             bufTypeExtent = flatBuf->extent;
         }

#ifdef onesidedtrace
        printf("Rank %d - memFlatBuf->count is %d bufTypeExtent is %ld\n",myrank,flatBuf->count, bufTypeExtent);
        for (i=0;i<flatBuf->count;i++)
            printf("Rank %d - flatBuf->blocklens[%d] is %d flatBuf->indices[%d] is %ld\n",myrank,i,flatBuf->blocklens[i],i,flatBuf->indices[i]);
#endif
     }

     int naggs = ca_data->cb_nodes;

     /* Track the state of the source buffer for feeding the target data blocks.
     * For GPFS the number of file domains per agg is always 1 so we just need 1 agg
     * dimension to track the data, in the case of lustre we will need 2 dimensions
     * agg and file domain since aggs write to multiple file domains in the case of lustre.
     * This structure will be modified as the data is written to reflect the current state
     * of the offset.
     */

#ifdef onesidedtrace
    printf("Rank %d - sizeof(FDSourceBufferState_CA) is %d - make sure is 32 for 32-byte memalign optimal\n",myrank,sizeof(FDSourceBufferState_CA));
#endif

     FDSourceBufferState_CA *currentFDSourceBufferState = (FDSourceBufferState_CA *) H5MM_malloc(naggs * sizeof(FDSourceBufferState_CA));
     for (i=0;i<naggs;i++) {
         /* initialize based on the bufType to indicate that it is unset.
         */
         if (bufTypeIsContig) {
             currentFDSourceBufferState[i].sourceBufferOffset = -1;
         }
         else {
             currentFDSourceBufferState[i].indiceOffset = -1;
         }
     }

#ifdef onesidedtrace
     printf("Rank %d - H5FD_mpio_ccio_osagg_read bufTypeIsContig is %d contig_access_count is %d\n",myrank,bufTypeIsContig,contig_access_count);
#endif

     /* maxNumContigOperations keeps track of how many different chunks we will
     * need to recv for the purpose of pre-allocating the data structures to
     * hold them.
     */
     int maxNumContigOperations = contig_access_count;
     int myAggRank = -1; /* if I am an aggregor this is my index into ranklist */
     int iAmUsedAgg = 0; /* whether or not this rank is used as an aggregator. */

     /* Make coll_bufsize an ADIO_Offset_CA since it is used in calculations with offsets.
     */
     ADIO_Offset_CA coll_bufsize = (ADIO_Offset_CA)(ca_data->cb_buffer_size);

    /* Check if the I/O is (inner) asynchronous */
    if (ca_data->async_io_inner == 1) {
        /* split buffer in half for asynchronous I/O */
        coll_bufsize = (ADIO_Offset_CA)(ca_data->cb_buffer_size/2);
    }

     /* This logic defines values that are used later to determine what offsets define the portion
     * of the file domain the agg is reading this round.
     */
     int greatestFileDomainAggRank = -1,smallestFileDomainAggRank = -1;
     ADIO_Offset_CA greatestFileDomainOffset = 0;
     ADIO_Offset_CA smallestFileDomainOffset = lastFileOffset;
     for (j=0;j<naggs;j++) {
         if (fd_end[j] > greatestFileDomainOffset) {
             greatestFileDomainOffset = fd_end[j];
             greatestFileDomainAggRank = j;
         }
         if (fd_start[j] < smallestFileDomainOffset) {
             smallestFileDomainOffset = fd_start[j];
             smallestFileDomainAggRank = j;
         }
         if (ca_data->ranklist[j] == myrank) {
             myAggRank = j;
             if (fd_end[j] > fd_start[j]) {
                 iAmUsedAgg = 1;
                 stripe_parms->iWasUsedStripingAgg = 1;
             }
         }
     }

#ifdef onesidedtrace
    printf("Rank %d - contig_access_count is %d lastFileOffset is %ld firstFileOffset is %ld\n",myrank,contig_access_count,lastFileOffset,firstFileOffset);
    for (j=0;j<contig_access_count;j++) {
        printf("Rank %d - offset_list[%d]: %ld , len_list[%d]: %ld\n",myrank,j,offset_list[j],j,len_list[j]);
    }
#endif

     /* Compute number of rounds.
     */
     int numberOfRounds = 0;
     for (j=0;j<naggs;j++) {
         int currentNumberOfRounds = (int)(((fd_end[j] - fd_start[j])+(ADIO_Offset_CA)1)/coll_bufsize);
         if ( ( (ADIO_Offset_CA)currentNumberOfRounds*coll_bufsize ) < ((fd_end[j] - fd_start[j])+(ADIO_Offset_CA)1))
             currentNumberOfRounds++;
         if (currentNumberOfRounds > numberOfRounds)
             numberOfRounds = currentNumberOfRounds;
     }

     /* Data structures to track what data this compute needs to receive from whom.
     * For lustre they will all need another dimension for the file domain.
     */
     int *sourceAggsForMyData = (int *) H5MM_malloc(naggs * sizeof(int));
     ADIO_Offset_CA *sourceAggsForMyDataFDStart = (ADIO_Offset_CA *)H5MM_malloc(naggs * sizeof(ADIO_Offset_CA));
     ADIO_Offset_CA *sourceAggsForMyDataFDEnd = (ADIO_Offset_CA *)H5MM_malloc(naggs * sizeof(ADIO_Offset_CA));
     int numSourceAggs = 0;

     /* This data structure holds the beginning offset and len list index for the range to be read
     * coresponding to the round and source agg. Initialize to -1 to denote being unset.
     */
     int **sourceAggsForMyDataFirstOffLenIndex = (int **) H5MM_malloc(numberOfRounds * sizeof(int *));
     for (i = 0; i < numberOfRounds; i++) {
         sourceAggsForMyDataFirstOffLenIndex[i] = (int *) H5MM_malloc(naggs * sizeof(int));
         for (j = 0; j < naggs; j++)
             sourceAggsForMyDataFirstOffLenIndex[i][j] = -1;
     }

     /* This data structure holds the ending offset and len list index for the range to be read
     * coresponding to the round and source agg.
     */
     int **sourceAggsForMyDataLastOffLenIndex = (int **) H5MM_malloc(numberOfRounds * sizeof(int *));
     for (i = 0; i < numberOfRounds; i++)
         sourceAggsForMyDataLastOffLenIndex[i] = (int *) H5MM_malloc(naggs * sizeof(int));

#ifdef onesidedtrace
    printf("Rank %d - NumberOfRounds is %d\n",myrank,numberOfRounds);
    for (i=0;i<naggs;i++)
        printf("Rank %d - ca_data->ranklist[%d] is %d fd_start is %ld fd_end is %ld\n",myrank,i,ca_data->ranklist[i],fd_start[i],fd_end[i]);
    for (j=0;j<contig_access_count;j++)
        printf("Rank %d - offset_list[%d] is %ld len_list is %ld\n",myrank,j,offset_list[j],len_list[j]);
#endif

     int currentAggRankListIndex = 0;
     int maxNumNonContigSourceChunks = 0;

     ADIO_Offset_CA currentRecvBufferOffset = 0;
     ADIO_Offset_CA currentDataTypeExtent = 0;
     int currentFlatBufIndice=0;
     ADIO_Offset_CA currentIndiceOffset = 0;

     /* Remember where we left off in the buffer when reading stripes. */
     if ((stripeSize > 0) && !stripe_parms->firstStripedIOCall) {
         currentDataTypeExtent = stripe_parms->lastDataTypeExtent;
         currentFlatBufIndice = stripe_parms->lastFlatBufIndice;
         currentIndiceOffset = stripe_parms->lastIndiceOffset;
     }

     /* This denotes the coll_bufsize boundaries within the source buffer for reading for 1 round.
     */
     ADIO_Offset_CA intraRoundCollBufsizeOffset = 0;

     /* This data structure tracks what source aggs need to be read to on what rounds.
     */
     int *sourceAggsForMyDataCurrentRoundIter = (int *) H5MM_malloc(naggs * sizeof(int));
     for (i = 0; i < naggs; i++)
         sourceAggsForMyDataCurrentRoundIter[i] = 0;


     /* This is the first of the two main loops in this algorithm.
     * The purpose of this loop is essentially to populate
     * the data structures defined above for what read data blocks
     * needs to go where (source agg and file domain) and when
     * (round iter).  For lustre essentially an additional layer of
     * nesting will be required for the multiple file domains
     * within the source agg.
     */
     if ((contig_access_count > 0) && (buf != NULL) && lenListOverZero) {
         int blockIter;
         for (blockIter = 0; blockIter < contig_access_count; blockIter++) {

             /* Determine the starting source buffer offset for this block - for iter 0 skip it since that value is 0.
             */
             if (blockIter > 0) {
                 if (bufTypeIsContig) {
                     currentRecvBufferOffset += len_list[blockIter - 1];
                 } else {
                     /* Non-contiguous source datatype, count up the extents and indices to this point
                     * in the blocks.
                     */
                     ADIO_Offset_CA sourceBlockTotal = 0;
                     int lastIndiceUsed = currentFlatBufIndice;
                     int numNonContigSourceChunks = 0;

                     while (sourceBlockTotal < len_list[blockIter - 1]) {
                         numNonContigSourceChunks++;
                         sourceBlockTotal += (flatBuf->blocklens[currentFlatBufIndice] - currentIndiceOffset);
                         lastIndiceUsed = currentFlatBufIndice;
                         currentFlatBufIndice++;
                         if (currentFlatBufIndice == flatBuf->count) {
                             currentFlatBufIndice = 0;
                             currentDataTypeExtent++;
                         }
                         currentIndiceOffset = (ADIO_Offset_CA) 0;
                     }
                     if (sourceBlockTotal > len_list[blockIter - 1]) {
                         currentFlatBufIndice--;
                         if (currentFlatBufIndice < 0) {
                             currentDataTypeExtent--;
                             currentFlatBufIndice = flatBuf->count - 1;
                         }
                         currentIndiceOffset = len_list[blockIter - 1] - (sourceBlockTotal - flatBuf->blocklens[lastIndiceUsed]);
                     } else
                     currentIndiceOffset = (ADIO_Offset_CA) 0;
                     maxNumContigOperations += (numNonContigSourceChunks + 2);
                     if (numNonContigSourceChunks > maxNumNonContigSourceChunks)
                         maxNumNonContigSourceChunks = numNonContigSourceChunks;

#ifdef onesidedtrace
                    printf("blockiter %d currentFlatBufIndice is now %d currentDataTypeExtent is now %ld currentIndiceOffset is now %ld maxNumContigOperations is now %d\n",blockIter,currentFlatBufIndice,currentDataTypeExtent,currentIndiceOffset,maxNumContigOperations);
#endif
                 } // !bufTypeIsContig
             } // blockIter > 0

             /* For the last iteration we need to include these maxNumContigOperations and maxNumNonContigSourceChunks
             * for non-contig case even though we did not need to compute the next starting offset.
             */
             if ((blockIter == (contig_access_count - 1)) && (!bufTypeIsContig)) {
                 ADIO_Offset_CA sourceBlockTotal = 0;
                 int tmpCurrentFlatBufIndice = currentFlatBufIndice;
                 int lastNumNonContigSourceChunks = 0;
                 while (sourceBlockTotal < len_list[blockIter]) {
                     lastNumNonContigSourceChunks++;
                     sourceBlockTotal += flatBuf->blocklens[tmpCurrentFlatBufIndice];
                     tmpCurrentFlatBufIndice++;
                     if (tmpCurrentFlatBufIndice == flatBuf->count) {
                         tmpCurrentFlatBufIndice = 0;
                     }
                 }
                 maxNumContigOperations += (lastNumNonContigSourceChunks + 2);
                 if (lastNumNonContigSourceChunks > maxNumNonContigSourceChunks)
                 maxNumNonContigSourceChunks = lastNumNonContigSourceChunks;
             }

             ADIO_Offset_CA blockStart = offset_list[blockIter];
             ADIO_Offset_CA blockEnd = offset_list[blockIter] + len_list[blockIter] - (ADIO_Offset_CA)1;

             /* Find the starting source agg for this block - normally it will be the current agg so guard the expensive
             * while loop with a cheap if-check which for large numbers of small blocks will usually be false.
             */
             if (!((blockStart >= fd_start[currentAggRankListIndex]) && (blockStart <= fd_end[currentAggRankListIndex]))) {
                 while (!((blockStart >= fd_start[currentAggRankListIndex]) && (blockStart <= fd_end[currentAggRankListIndex])))
                     currentAggRankListIndex++;
             };

#ifdef onesidedtrace
            printf("Rank %d - currentAggRankListIndex is %d blockStart %ld blockEnd %ld fd_start[currentAggRankListIndex] %ld fd_end[currentAggRankListIndex] %ld\n",myrank,currentAggRankListIndex,blockStart,blockEnd,fd_start[currentAggRankListIndex],fd_end[currentAggRankListIndex]);
#endif

             /* Determine if this is a new source agg.
             */
             if (blockIter > 0) {
                 if ((offset_list[blockIter - 1] + len_list[blockIter - 1] - (ADIO_Offset_CA) 1) < fd_start[currentAggRankListIndex]) {
                     numSourceAggs++;
                 }
             }

             /* Determine which round to start reading.
             */
             if ((blockStart - fd_start[currentAggRankListIndex]) >= coll_bufsize) {
                 ADIO_Offset_CA currentRoundBlockStart = fd_start[currentAggRankListIndex];
                 int startingRound = 0;
                 while (blockStart > (currentRoundBlockStart + coll_bufsize - (ADIO_Offset_CA) 1)) {
                     currentRoundBlockStart += coll_bufsize;
                     startingRound++;
                 }
                 sourceAggsForMyDataCurrentRoundIter[numSourceAggs] = startingRound;
             }

             /* Initialize the data structures if this is the first offset in the round/source agg.
             */
             if (sourceAggsForMyDataFirstOffLenIndex[sourceAggsForMyDataCurrentRoundIter[numSourceAggs]][numSourceAggs] == -1) {
                 sourceAggsForMyData[numSourceAggs] = ca_data->ranklist[currentAggRankListIndex];
                 sourceAggsForMyDataFDStart[numSourceAggs] = fd_start[currentAggRankListIndex];
                 /* Round up file domain to the first actual offset used if this is the first file domain.
                 */
                 if (currentAggRankListIndex == smallestFileDomainAggRank) {
                     if (sourceAggsForMyDataFDStart[numSourceAggs] < firstFileOffset)
                         sourceAggsForMyDataFDStart[numSourceAggs] = firstFileOffset;
                 }
                 sourceAggsForMyDataFDEnd[numSourceAggs] = fd_end[currentAggRankListIndex];
                 /* Round down file domain to the last actual offset used if this is the last file domain.
                 */
                 if (currentAggRankListIndex == greatestFileDomainAggRank) {
                     if (sourceAggsForMyDataFDEnd[numSourceAggs] > lastFileOffset)
                         sourceAggsForMyDataFDEnd[numSourceAggs] = lastFileOffset;
                 }
                 sourceAggsForMyDataFirstOffLenIndex[sourceAggsForMyDataCurrentRoundIter[numSourceAggs]][numSourceAggs] = blockIter;

                 /* Set the source buffer state starting point for data access for this agg and file domain.
                 */
                 if (bufTypeIsContig) {
                     if (currentFDSourceBufferState[numSourceAggs].sourceBufferOffset == -1) {
                         currentFDSourceBufferState[numSourceAggs].sourceBufferOffset = currentRecvBufferOffset;
#ifdef onesidedtrace
                        printf("Rank %d - For agg %d sourceBufferOffset initialized to %ld\n",myrank,currentAggRankListIndex,currentRecvBufferOffset);
#endif
                     }
                 } else {
                     if (currentFDSourceBufferState[numSourceAggs].indiceOffset == -1) {
                         currentFDSourceBufferState[numSourceAggs].indiceOffset = currentIndiceOffset;
                         currentFDSourceBufferState[numSourceAggs].bufTypeExtent = bufTypeExtent;
                         currentFDSourceBufferState[numSourceAggs].dataTypeExtent = currentDataTypeExtent;
                         currentFDSourceBufferState[numSourceAggs].flatBufIndice = currentFlatBufIndice;
#ifdef onesidedtrace
                         printf("Rank %d - For agg %d dataTypeExtent initialized to %d flatBufIndice to %d indiceOffset to %ld\n", myrank, numSourceAggs, currentDataTypeExtent, currentFlatBufIndice, currentIndiceOffset);
#endif
                     }
                 }

                 intraRoundCollBufsizeOffset = fd_start[currentAggRankListIndex] + ((ADIO_Offset_CA) (sourceAggsForMyDataCurrentRoundIter[numSourceAggs] + 1) * coll_bufsize);

#ifdef onesidedtrace
                 printf("Rank %d - init settings numSourceAggs %d offset_list[%d] with value %ld past fd border %ld with len %ld currentRecvBufferOffset set to %ld intraRoundCollBufsizeOffset set to %ld\n", myrank, numSourceAggs, blockIter, offset_list[blockIter], fd_start[currentAggRankListIndex], len_list[blockIter], currentRecvBufferOffset, intraRoundCollBufsizeOffset);
#endif

             }

             /* Replace the last offset block iter with this one.
             */
             sourceAggsForMyDataLastOffLenIndex[sourceAggsForMyDataCurrentRoundIter[numSourceAggs]][numSourceAggs] = blockIter;

             /* If this blocks extends into the next file domain advance to the next source aggs and source buffer states.
             */
             if (blockEnd > fd_end[currentAggRankListIndex]) {

                 ADIO_Offset_CA amountToAdvanceSBOffsetForFD = 0;
                 int additionalFDCounter = 0;

                 while (blockEnd > fd_end[currentAggRankListIndex]) {
#ifdef onesidedtrace
                     printf("Rank %d - block extends past current fd, blockEnd %ld >= fd_end[currentAggRankListIndex] %ld total block size is %ld blockStart was %ld\n", myrank, blockEnd, fd_end[currentAggRankListIndex], len_list[blockIter], blockStart);
                     printf("Rank %d - currentAggRankListIndex is now %d blockEnd %ld > fd_end[%d] %ld\n", myrank, currentAggRankListIndex, blockEnd, currentAggRankListIndex, fd_end[currentAggRankListIndex]);
#endif
                     ADIO_Offset_CA thisAggBlockEnd = fd_end[currentAggRankListIndex];
                     if (thisAggBlockEnd >= intraRoundCollBufsizeOffset) {
                         while (thisAggBlockEnd >= intraRoundCollBufsizeOffset) {
                             sourceAggsForMyDataCurrentRoundIter[numSourceAggs]++;
                             intraRoundCollBufsizeOffset += coll_bufsize;
                             sourceAggsForMyDataFirstOffLenIndex[sourceAggsForMyDataCurrentRoundIter[numSourceAggs]][numSourceAggs] = blockIter;
                             sourceAggsForMyDataLastOffLenIndex[sourceAggsForMyDataCurrentRoundIter[numSourceAggs]][numSourceAggs] = blockIter;
#ifdef onesidedtrace
                             printf("Rank %d - sourceAggsForMyDataCurrentRoundI%d] is now %d intraRoundCollBufsizeOffset is now %ld\n", myrank, numSourceAggs, sourceAggsForMyDataCurrentRoundIter[numSourceAggs], intraRoundCollBufsizeOffset);
#endif
                         } // while (thisAggBlockEnd >= intraRoundCollBufsizeOffset)
                     } // if (thisAggBlockEnd >= intraRoundCollBufsizeOffset)

                     int prevAggRankListIndex = currentAggRankListIndex;
                     currentAggRankListIndex++;

                     /* Skip over unused aggs.
                     */
                     if (fd_start[currentAggRankListIndex] > fd_end[currentAggRankListIndex]) {
                         while (fd_start[currentAggRankListIndex] > fd_end[currentAggRankListIndex])
                             currentAggRankListIndex++;
                     } // (fd_start[currentAggRankListIndex] > fd_end[currentAggRankListIndex])

                     /* Start new source agg.
                     */
                     if (blockEnd >= fd_start[currentAggRankListIndex]) {
                         numSourceAggs++;
                         sourceAggsForMyData[numSourceAggs] = ca_data->ranklist[currentAggRankListIndex];
                         sourceAggsForMyDataFDStart[numSourceAggs] = fd_start[currentAggRankListIndex];
                         /* Round up file domain to the first actual offset used if this is the first file domain.
                         */
                         if (currentAggRankListIndex == smallestFileDomainAggRank) {
                             if (sourceAggsForMyDataFDStart[numSourceAggs] < firstFileOffset)
                                 sourceAggsForMyDataFDStart[numSourceAggs] = firstFileOffset;
                         }
                         sourceAggsForMyDataFDEnd[numSourceAggs] = fd_end[currentAggRankListIndex];
                         /* Round down file domain to the last actual offset used if this is the last file domain.
                         */
                         if (currentAggRankListIndex == greatestFileDomainAggRank) {
                             if (sourceAggsForMyDataFDEnd[numSourceAggs] > lastFileOffset)
                                 sourceAggsForMyDataFDEnd[numSourceAggs] = lastFileOffset;
                         }
                         sourceAggsForMyDataFirstOffLenIndex[sourceAggsForMyDataCurrentRoundIter[numSourceAggs]][numSourceAggs] = blockIter;

                         /* For the first additonal file domain the source buffer offset
                         * will be incremented relative to the state of this first main
                         * loop but for subsequent full file domains the offset will be
                         * incremented by the size of the file domain.
                         */
                         if (additionalFDCounter == 0)
                             amountToAdvanceSBOffsetForFD = (fd_end[prevAggRankListIndex] - blockStart) + (ADIO_Offset_CA) 1;
                         else
                             amountToAdvanceSBOffsetForFD = (fd_end[prevAggRankListIndex] - fd_start[prevAggRankListIndex]) + (ADIO_Offset_CA) 1;

                         if (bufTypeIsContig) {
                             HDassert(numSourceAggs > 0);
                             if (currentFDSourceBufferState[numSourceAggs].sourceBufferOffset == -1) {
                                 if (additionalFDCounter == 0) { // first file domain, still use the current data counter
                                     currentFDSourceBufferState[numSourceAggs].sourceBufferOffset = currentRecvBufferOffset + amountToAdvanceSBOffsetForFD;
                                 } else { // 2nd file domain, advance full file domain from last source buffer state
                                     currentFDSourceBufferState[numSourceAggs].sourceBufferOffset = currentFDSourceBufferState[numSourceAggs - 1].sourceBufferOffset + amountToAdvanceSBOffsetForFD;
                                 }
#ifdef onesidedtrace
                                 printf("Rank %d - Crossed into new FD - for agg %d sourceBufferOffset initialized to %ld amountToAdvanceSBOffsetForFD is %ld\n", myrank, numSourceAggs, currentFDSourceBufferState[numSourceAggs].sourceBufferOffset, amountToAdvanceSBOffsetForFD);
#endif
                             }
                         } else if (currentFDSourceBufferState[numSourceAggs].indiceOffset == -1) {

                             /* non-contiguos source buffer */
                             HDassert(numSourceAggs > 0);

                             /* Initialize the source buffer state appropriately and then
                              * advance it with the nonContigSourceDataBufferAdvance function.
                              */
                             if (additionalFDCounter == 0) {
                                 // first file domain, still use the current data counter
                                 currentFDSourceBufferState[numSourceAggs].indiceOffset = currentIndiceOffset;
                                 currentFDSourceBufferState[numSourceAggs].bufTypeExtent = bufTypeExtent;
                                 currentFDSourceBufferState[numSourceAggs].dataTypeExtent = currentDataTypeExtent;
                                 currentFDSourceBufferState[numSourceAggs].flatBufIndice = currentFlatBufIndice;
                             } else {
                                 // 2nd file domain, advance full file domain from last source buffer state
                                 currentFDSourceBufferState[numSourceAggs].indiceOffset =
                                 currentFDSourceBufferState[numSourceAggs - 1].indiceOffset;
                                 currentFDSourceBufferState[numSourceAggs].bufTypeExtent =
                                 currentFDSourceBufferState[numSourceAggs - 1].bufTypeExtent;
                                 currentFDSourceBufferState[numSourceAggs].dataTypeExtent = currentFDSourceBufferState[numSourceAggs - 1].dataTypeExtent;
                                 currentFDSourceBufferState[numSourceAggs].flatBufIndice =
                                 currentFDSourceBufferState[numSourceAggs - 1].flatBufIndice;
                             }
                             H5FD_mpio_nc_buffer_advance(((char *) buf), flatBuf, (int) amountToAdvanceSBOffsetForFD, 0, &currentFDSourceBufferState[numSourceAggs], NULL);

#ifdef onesidedtrace
                             printf("Rank %d - Crossed into new FD - for agg %d dataTypeExtent initialized to %d flatBufIndice to %d indiceOffset to %ld amountToAdvanceSBOffsetForFD is %d\n", myrank, numSourceAggs, currentFDSourceBufferState[numSourceAggs].dataTypeExtent, currentFDSourceBufferState[numSourceAggs].flatBufIndice, currentFDSourceBufferState[numSourceAggs].indiceOffset, amountToAdvanceSBOffsetForFD);
#endif
                         }
                         additionalFDCounter++;

#ifdef onesidedtrace
                         printf("Rank %d - block extended beyond fd init settings numSourceAggs %d offset_list[%d] with value %ld past fd border %ld with len %ld\n", myrank, numSourceAggs, blockIter, offset_list[blockIter], fd_start[currentAggRankListIndex], len_list[blockIter]);
#endif
                         intraRoundCollBufsizeOffset = fd_start[currentAggRankListIndex] + coll_bufsize;
                         sourceAggsForMyDataLastOffLenIndex[sourceAggsForMyDataCurrentRoundIter[numSourceAggs]][numSourceAggs] = blockIter;

                     } // if (blockEnd >= fd_start[currentAggRankListIndex])
                 } // while (blockEnd > fd_end[currentAggRankListIndex])
             } // if (blockEnd > fd_end[currentAggRankListIndex])

             /* If we are still in the same file domain / source agg but have gone past the coll_bufsize and need
             * to advance to the next round handle this situation.
             */
             if (blockEnd >= intraRoundCollBufsizeOffset) {
                 ADIO_Offset_CA currentBlockEnd = blockEnd;
                 while (currentBlockEnd >= intraRoundCollBufsizeOffset) {
                     sourceAggsForMyDataCurrentRoundIter[numSourceAggs]++;
                     intraRoundCollBufsizeOffset += coll_bufsize;
                     sourceAggsForMyDataFirstOffLenIndex[sourceAggsForMyDataCurrentRoundIter[numSourceAggs]][numSourceAggs] = blockIter;
                     sourceAggsForMyDataLastOffLenIndex[sourceAggsForMyDataCurrentRoundIter[numSourceAggs]][numSourceAggs] = blockIter;
#ifdef onesidedtrace
                     printf("Rank %d - block less than fd currentBlockEnd is now %ld intraRoundCollBufsizeOffset is now %ld sourceAggsForMyDataCurrentRoundIter[%d] is now %d\n", myrank, currentBlockEnd, intraRoundCollBufsizeOffset, numSourceAggs, sourceAggsForMyDataCurrentRoundIter[numSourceAggs]);
#endif
                 } // while (currentBlockEnd >= intraRoundCollBufsizeOffset)
             } // if (blockEnd >= intraRoundCollBufsizeOffset)

             /* Need to advance numSourceAggs if this is the last source offset to
             * include this one.
             */
             if (blockIter == (contig_access_count - 1))
                 numSourceAggs++;
         }

#ifdef onesidedtrace
         printf("Rank %d - numSourceAggs is %d\n", myrank, numSourceAggs);
         /*for (i = 0; i < numSourceAggs; i++) {
             for (j = 0; j <= sourceAggsForMyDataCurrentRoundIter[i]; j++)
             printf("Rank %d - sourceAggsForMyData[%d] is %d sourceAggsForMyDataFDStart[%d] is %ld sourceAggsForMyDataFDEnd is %ld sourceAggsForMyDataFirstOffLenIndex is %d with value %ld sourceAggsForMyDataLastOffLenIndex is %d with value %ld\n", myrank, i, sourceAggsForMyData[i], i, sourceAggsForMyDataFDStart[i], sourceAggsForMyDataFDEnd[i], sourceAggsForMyDataFirstOffLenIndex[j][i], offset_list[sourceAggsForMyDataFirstOffLenIndex[j][i]], sourceAggsForMyDataLastOffLenIndex[j][i], offset_list[sourceAggsForMyDataLastOffLenIndex[j][i]]);
         }*/
#endif

     } // if ((contig_access_count > 0) && (buf != NULL) && lenListOverZero)

    H5MM_free(sourceAggsForMyDataCurrentRoundIter);

    int currentReadBuf = 0;
    int useIOBuffer = 0;

    /* Check if the I/O is asynchronous */
    if ((ca_data->async_io_inner == 1) && (numberOfRounds > 1)) {
        if (ca_data->pthread_io == 1) {
            useIOBuffer = 1;
            io_thread = pthread_self();
        } else {
            ca_data->async_io_inner = 0;
        }
    }

    /* use the two-phase buffer allocated in the file_open - no app should ever
     * be both reading and reading at the same time */
    char *read_buf0 = ca_data->io_buf;
    char *read_buf1 = ca_data->io_buf + coll_bufsize;

    /* Async I/O - Adjust if this is the "duplicate" buffer */
    if (ca_data->use_dup) {
        read_buf0 = ca_data->io_buf_d;
        read_buf1 = ca_data->io_buf_d + coll_bufsize;
    }

    /* use the two-phase buffer allocated in the file_open - no app should ever
     * be both reading and reading at the same time */
    char *read_buf = read_buf0;
    MPI_Win read_buf_window = ca_data->io_buf_window;

    /* Async I/O - Adjust if this is the "duplicate" buffer */
    if (ca_data->use_dup) {
        read_buf_window = ca_data->io_buf_window_d;
    }

    ADIO_Offset_CA currentRoundFDStart = 0, nextRoundFDStart = 0;
    ADIO_Offset_CA currentRoundFDEnd = 0, nextRoundFDEnd = 0;

     if (iAmUsedAgg) {
         currentRoundFDStart = fd_start[myAggRank];
         nextRoundFDStart = fd_start[myAggRank];
         if (myAggRank == smallestFileDomainAggRank) {
             if (currentRoundFDStart < firstFileOffset)
                 currentRoundFDStart = firstFileOffset;
             if (nextRoundFDStart < firstFileOffset)
                 nextRoundFDStart = firstFileOffset;
         } else if (myAggRank == greatestFileDomainAggRank) {
             if (currentRoundFDEnd > lastFileOffset)
                 currentRoundFDEnd = lastFileOffset;
             if (nextRoundFDEnd > lastFileOffset)
                 nextRoundFDEnd = lastFileOffset;
         }
#ifdef onesidedtrace
         printf("Rank %d - iAmUsedAgg - currentRoundFDStart initialized to %ld currentRoundFDEnd to %ld\n", myrank, currentRoundFDStart, currentRoundFDEnd);
#endif
     } // if iAmUsedAgg

#ifdef onesidedtrace
     MPI_Barrier(ca_data->comm);
     if(myrank==0) { printf("\n\n"); fflush(stdout); }
     MPI_Barrier(ca_data->comm);
     printf("Rank %d is waiting at barrier between main loops.\n", myrank);
     printf("Rank %d -- numberOfRounds = %d, contig_access_count = %d, numSourceAggs = %d\n", myrank, numberOfRounds, contig_access_count, numSourceAggs);
     fflush(stdout);
     MPI_Barrier(ca_data->comm);
     if(myrank==0) { printf("\n\n"); fflush(stdout); }
     MPI_Barrier(ca_data->comm);
#endif

     /* This is the second main loop of the algorithm, actually nested loop of
     * aggs within rounds. There are 2 flavors of this.
     * For onesided_read_aggmethod of 1 each nested iteration for the source agg
     * does an mpi_get on a contiguous chunk using a primative datatype
     * determined using the data structures from the first main loop.
     * For onesided_read_aggmethod of 2 each nested iteration for the source agg
     * builds up data to use in created a derived data type for 1 mpi_get that
     * is done for the target agg for each round.
     * To support lustre there will need to be an additional layer of nesting
     * for the multiple file domains within target aggs.
     */
     int roundIter;
     for (roundIter = 0; roundIter < numberOfRounds; roundIter++) {

         if (iAmUsedAgg || stripe_parms->iWasUsedStripingAgg) {
             stripe_parms->iWasUsedStripingAgg = 0;

#ifdef onesidedtrace
             printf("Rank %d - roundIter %ld of %ld - currentRoundFDEnd = %ld \n", myrank, roundIter, numberOfRounds, currentRoundFDEnd);
#endif

             /* determine what offsets define the portion of the file domain the agg is reading this round */
             if (iAmUsedAgg) {

                currentRoundFDStart = nextRoundFDStart;

                if (!useIOBuffer || (roundIter == 0)) {

                     ADIO_Offset_CA amountDataToReadThisRound;
                     if ((fd_end[myAggRank] - currentRoundFDStart) < coll_bufsize) {
                         currentRoundFDEnd = fd_end[myAggRank];
                         amountDataToReadThisRound = ((currentRoundFDEnd - currentRoundFDStart) + 1);
                     } else {
                         currentRoundFDEnd = currentRoundFDStart + coll_bufsize - (ADIO_Offset_CA) 1;
                         amountDataToReadThisRound = coll_bufsize;
                     }

#ifdef onesidedtrace
                     printf("Rank %d - amountDataToReadThisRound=%ld - myAggRank=%ld - fd_end[myAggRank]=%ld - currentRoundFDStart=%ld - currentRoundFDEnd=%ld - coll_bufsize=%ld\n", myrank, amountDataToReadThisRound, myAggRank, fd_end[myAggRank], currentRoundFDStart, currentRoundFDEnd, coll_bufsize);
#endif

                     /*
                      *  Don't actually do the read if it was already done
                      *  (asynchronously) outside this function call...
                      */
                     if (do_file_read && amountDataToReadThisRound>0) {
#ifdef onesidedtrace
                         printf("Rank %d - calling MPI_File_read_at\n", myrank);
#endif

                        if (ca_data->check_req) {
                            MPIO_Wait(&ca_data->io_Request, error_code);
                            ca_data->check_req = 0;
                        }

                        /* read currentRoundFDEnd bytes */
                        MPI_File_read_at(ca_data->fh, currentRoundFDStart, read_buf, amountDataToReadThisRound, MPI_BYTE, &status);

#ifdef onesidedtrace
                        printf("Rank %d - Finishing MPI_File_read_at (offset=%d,size=%d)\n", myrank, currentRoundFDStart, amountDataToReadThisRound);
                        fflush(stdout);
#endif
                     } /* if (do_file_read) */

                     currentReadBuf = 1;

                 } /* (!useIOBuffer || (roundIter == 0)) */
                if (useIOBuffer) {

                    /* use the thread reader for the next round */
                    /* switch back and forth between the read buffers so that the data aggregation code is diseminating 1 buffer while the thread is reading into the other */
                    if (roundIter > 0) currentRoundFDEnd = nextRoundFDEnd; // Does this do anything?

                    if (roundIter < (numberOfRounds - 1)) {

#ifdef onesidedtrace
                        printf("Rank %d - Calc amountDataToReadNextRound...\n", myrank);
                        fflush(stdout);
#endif
                        nextRoundFDStart += coll_bufsize;
                        ADIO_Offset_CA amountDataToReadNextRound;
                        if ((fd_end[myAggRank] - nextRoundFDStart) < coll_bufsize) {
                            nextRoundFDEnd = fd_end[myAggRank];
                            amountDataToReadNextRound = ((nextRoundFDEnd - nextRoundFDStart) + 1);
                        } else {
                            nextRoundFDEnd = nextRoundFDStart + coll_bufsize - (ADIO_Offset_CA) 1;
                            amountDataToReadNextRound = coll_bufsize;
                        }
#ifdef onesidedtrace
                        printf("Rank %d - nextRoundFDEnd = %ld, amountDataToReadNextRound = %ld.\n", myrank, nextRoundFDEnd, amountDataToReadNextRound);
                        fflush(stdout);
                        printf("Rank %d - myAggRank=%ld - fd_end[myAggRank]=%ld - nextRoundFDStart=%ld - nextRoundFDEnd=%ld - coll_bufsize=%ld\n", myrank, myAggRank, fd_end[myAggRank], nextRoundFDStart, nextRoundFDEnd, coll_bufsize);
#endif
                        if ( !pthread_equal(io_thread, pthread_self()) ) {

#ifdef onesidedtrace
                            printf("Rank %d - Need pthread join.\n", myrank);
                            fflush(stdout);
#endif
                            pthread_join(io_thread, &thread_ret);

                            int error_code_thread = *(int *) thread_ret;
                            if (error_code_thread != MPI_SUCCESS) {
                                printf("Rank %d - pthread_join FAILED!, error_code_thread = %d\n", myrank, error_code_thread);
                                fflush(stdout);
                                return;
                            }
                            io_thread = pthread_self();
                        }

                        /* do a little pointer shuffling: background I/O works from one
                         * buffer while two-phase machinery fills up another */
                        if (currentReadBuf == 0) {
                            read_buf = read_buf1;
                            currentReadBuf = 1;
                            io_thread_args.buf = read_buf0;
                        } else {
                            read_buf = read_buf0;
                            currentReadBuf = 0;
                            io_thread_args.buf = read_buf1;
                        }
                        io_thread_args.fh = ca_data->fh;
                        io_thread_args.myrank = myrank;
                        io_thread_args.io_kind = READ_CA;
                        io_thread_args.size = amountDataToReadNextRound;
                        io_thread_args.offset = nextRoundFDStart;
                        io_thread_args.error_code = *error_code;

                        if (amountDataToReadNextRound > 0) {
#ifdef onesidedtrace
                            printf("Rank %d - calling pthread_create (size=%ld,offset=%ld)\n", myrank, io_thread_args.size, io_thread_args.offset);
                            printf("Rank %d - (size=%ld,amountDataToReadNextRound=%ld)\n", myrank, io_thread_args.size, amountDataToReadNextRound);
                            fflush(stdout);
#endif
                            if ((pthread_create(&io_thread, NULL, IO_Thread_Func, &(io_thread_args))) != 0)
                                io_thread = pthread_self();
#ifdef onesidedtrace
                            printf("Rank %d - pthread_create DONE.\n", myrank);
#endif
                        }
                    } else {    /* last round */

                        if (!pthread_equal(io_thread, pthread_self())) {

                            pthread_join(io_thread, &thread_ret);
                            int error_code_thread = *(int *) thread_ret;
                            if (error_code_thread != MPI_SUCCESS) {
                                printf("Rank %d - Last pthread_join FAILED!, error_code_thread = %d\n", myrank, error_code_thread);
                                fflush(stdout);
                                return;
                            }
                            io_thread = pthread_self();

                        }
                        if (currentReadBuf == 0) {
                            read_buf = read_buf1;
                        } else {
                            read_buf = read_buf0;
                        }

                    }
                } /* useIOBuffer */

             } /* IAmUsedAgg */
              else if (useIOBuffer) {
                  if (roundIter < (numberOfRounds - 1)) {
                      if (currentReadBuf == 0) {
                          currentReadBuf = 1;
                          read_buf = read_buf1;
                      } else {
                          currentReadBuf = 0;
                          read_buf = read_buf0;
                      }
                  } else {
                      if (currentReadBuf == 0) {
                          read_buf = read_buf1;
                      } else {
                          read_buf = read_buf0;
                      }
                  }
              }

         } // (iAmUsedAgg || stripe_parms->iWasUsedStripingAgg)

#ifdef onesidedtrace
         printf("Rank %d - Hitting MPI_Barrier.\n", myrank);
#endif

         /* wait until the read buffers are full before we start pulling from the source procs */
         MPI_Barrier(ca_data->comm);

         if ((contig_access_count > 0) && (buf != NULL) && lenListOverZero) {

             int aggIter;
             for (aggIter = 0; aggIter < numSourceAggs; aggIter++) {

                 /* If we have data for the round/agg process it.
                 */
                 if (sourceAggsForMyDataFirstOffLenIndex[roundIter][aggIter] != -1) {

                     ADIO_Offset_CA currentRoundFDStartForMySourceAgg = (ADIO_Offset_CA) ((ADIO_Offset_CA) sourceAggsForMyDataFDStart[aggIter] + (ADIO_Offset_CA) ((ADIO_Offset_CA) roundIter * coll_bufsize));
                     ADIO_Offset_CA currentRoundFDEndForMySourceAgg = (ADIO_Offset_CA) ((ADIO_Offset_CA) sourceAggsForMyDataFDStart[aggIter] + (ADIO_Offset_CA) ((ADIO_Offset_CA) (roundIter + 1) * coll_bufsize) - (ADIO_Offset_CA) 1);

                     int sourceAggContigAccessCount = 0;

                     /* These data structures are used for the derived datatype mpi_get
                     * in the onesided_read_aggmethod of 2 case.
                     */
                     int *sourceAggBlockLengths = NULL;
                     MPI_Aint *sourceAggDisplacements = NULL, *recvBufferDisplacements = NULL;
                     MPI_Datatype *sourceAggDataTypes = NULL;
                     char *derivedTypePackedSourceBuffer = NULL;
                     int derivedTypePackedSourceBufferOffset = 0;
                     int allocatedDerivedTypeArrays = 0;
                     ADIO_Offset_CA amountOfDataReadThisRoundAgg = 0;

                     /* Process the range of offsets for this source agg.
                     */
                     int offsetIter;
                     int startingOffLenIndex = sourceAggsForMyDataFirstOffLenIndex[roundIter][aggIter];
                     int endingOffLenIndex = sourceAggsForMyDataLastOffLenIndex[roundIter][aggIter];
                     for (offsetIter = startingOffLenIndex; offsetIter <= endingOffLenIndex; offsetIter++) {

                         if (currentRoundFDEndForMySourceAgg > sourceAggsForMyDataFDEnd[aggIter])
                             currentRoundFDEndForMySourceAgg = sourceAggsForMyDataFDEnd[aggIter];

                         ADIO_Offset_CA offsetStart = offset_list[offsetIter], offsetEnd = (offset_list[offsetIter] + len_list[offsetIter] - (ADIO_Offset_CA) 1);

                         /* Determine the amount of data and exact source buffer offsets to use.
                         */
                         int bufferAmountToRecv = 0;

                         if ((offsetStart >= currentRoundFDStartForMySourceAgg) && (offsetStart <= currentRoundFDEndForMySourceAgg)) {
                             if (offsetEnd > currentRoundFDEndForMySourceAgg)
                                 bufferAmountToRecv = (currentRoundFDEndForMySourceAgg - offsetStart) + 1;
                             else
                                 bufferAmountToRecv = (offsetEnd - offsetStart) + 1;
                         } else if ((offsetEnd >= currentRoundFDStartForMySourceAgg) && (offsetEnd <= currentRoundFDEndForMySourceAgg)) {
                             if (offsetEnd > currentRoundFDEndForMySourceAgg)
                                 bufferAmountToRecv = (currentRoundFDEndForMySourceAgg - currentRoundFDStartForMySourceAgg) + 1;
                             else
                                 bufferAmountToRecv = (offsetEnd - currentRoundFDStartForMySourceAgg) + 1;
                             if (offsetStart < currentRoundFDStartForMySourceAgg) {
                                 offsetStart = currentRoundFDStartForMySourceAgg;
                             }
                         } else if ((offsetStart <= currentRoundFDStartForMySourceAgg) && (offsetEnd >= currentRoundFDEndForMySourceAgg)) {
                             bufferAmountToRecv = (currentRoundFDEndForMySourceAgg - currentRoundFDStartForMySourceAgg) + 1;
                             offsetStart = currentRoundFDStartForMySourceAgg;
                         }

                         if (bufferAmountToRecv > 0) {   /* we have data to recv this round */
                             if (ca_data->onesided_read_aggmethod == 2) {
                                 /* Only allocate these arrays if we are using method 2 and only do it once for this round/source agg.
                                 */
                                 if (!allocatedDerivedTypeArrays) {
                                     sourceAggBlockLengths = (int *) H5MM_malloc(maxNumContigOperations * sizeof(int));
                                     sourceAggDisplacements = (MPI_Aint *) H5MM_malloc(maxNumContigOperations * sizeof(MPI_Aint));
                                     recvBufferDisplacements = (MPI_Aint *) H5MM_malloc(maxNumContigOperations * sizeof(MPI_Aint));
                                     sourceAggDataTypes = (MPI_Datatype *) H5MM_malloc(maxNumContigOperations * sizeof(MPI_Datatype));
                                     if (!bufTypeIsContig) {
                                         int k;
                                         for (k = sourceAggsForMyDataFirstOffLenIndex[roundIter][aggIter]; k <= sourceAggsForMyDataLastOffLenIndex[roundIter][aggIter]; k++)
                                             amountOfDataReadThisRoundAgg += len_list[k];

#ifdef onesidedtrace
                                         printf("Rank %d - derivedTypePackedSourceBuffer mallocing %ld\n", myrank,amountOfDataReadThisRoundAgg);
#endif
                                         if (amountOfDataReadThisRoundAgg > 0)
                                             derivedTypePackedSourceBuffer = (char *) H5MM_malloc(amountOfDataReadThisRoundAgg * sizeof(char));
                                         else
                                             derivedTypePackedSourceBuffer = NULL;
                                     }
                                     allocatedDerivedTypeArrays = 1;
                                 }
                             }

                             /* Determine the offset into the source window.
                             */
                             ADIO_Offset_CA sourceDisplacementToUseThisRound = (ADIO_Offset_CA) (offsetStart - currentRoundFDStartForMySourceAgg);

                              /* If using the thread reader select the appropriate side of the split window.
                               */
                              if (useIOBuffer && (read_buf == read_buf1)) {
                                  sourceDisplacementToUseThisRound += (ADIO_Offset_CA) coll_bufsize;
                              }


                             /* For onesided_read_aggmethod of 1 do the mpi_get using the primitive MPI_BYTE type from each
                             * contiguous chunk from the target, if the source is non-contiguous then unpack the data after
                             * the MPI_Win_unlock is done to make sure the data has arrived first.
                             */
                             if (ca_data->onesided_read_aggmethod == 1) {

                                 MPI_Win_lock(MPI_LOCK_SHARED, sourceAggsForMyData[aggIter], 0, read_buf_window);

                                 char *getSourceData = NULL;
                                 if (bufTypeIsContig) {

                                     MPI_Get(((char *) buf) + currentFDSourceBufferState[aggIter].sourceBufferOffset, bufferAmountToRecv, MPI_BYTE, sourceAggsForMyData[aggIter], sourceDisplacementToUseThisRound, bufferAmountToRecv, MPI_BYTE, read_buf_window);
                                     currentFDSourceBufferState[aggIter].sourceBufferOffset += (ADIO_Offset_CA) bufferAmountToRecv;

                                 }
                                 else {

                                     getSourceData = (char *) H5MM_malloc(bufferAmountToRecv * sizeof(char));
                                     MPI_Get(getSourceData, bufferAmountToRecv, MPI_BYTE, sourceAggsForMyData[aggIter], sourceDisplacementToUseThisRound, bufferAmountToRecv, MPI_BYTE, read_buf_window);

                                 }

                                 MPI_Win_unlock(sourceAggsForMyData[aggIter], read_buf_window);

                                 if (!bufTypeIsContig) {
                                     H5FD_mpio_nc_buffer_advance(((char *) buf), flatBuf, bufferAmountToRecv, 0, &currentFDSourceBufferState[aggIter], getSourceData);
                                     H5MM_free(getSourceData);
                                 }
                             }

                             /* For onesided_read_aggmethod of 2 populate the data structures for this round/agg for this offset iter
                             * to be used subsequently when building the derived type for 1 mpi_get for all the data for this
                             * round/agg.
                             */
                             else if (ca_data->onesided_read_aggmethod == 2) {

                                 if (bufTypeIsContig) {
                                     sourceAggBlockLengths[sourceAggContigAccessCount] = bufferAmountToRecv;
                                     sourceAggDataTypes[sourceAggContigAccessCount] = MPI_BYTE;
                                     sourceAggDisplacements[sourceAggContigAccessCount] = sourceDisplacementToUseThisRound;
                                     recvBufferDisplacements[sourceAggContigAccessCount] = (MPI_Aint) currentFDSourceBufferState[aggIter].sourceBufferOffset;
                                     currentFDSourceBufferState[aggIter].sourceBufferOffset += (ADIO_Offset_CA) bufferAmountToRecv;
                                     sourceAggContigAccessCount++;
                                 }
                                 else {
                                     sourceAggBlockLengths[sourceAggContigAccessCount] = bufferAmountToRecv;
                                     sourceAggDataTypes[sourceAggContigAccessCount] = MPI_BYTE;
                                     sourceAggDisplacements[sourceAggContigAccessCount] = sourceDisplacementToUseThisRound;
                                     recvBufferDisplacements[sourceAggContigAccessCount] = (MPI_Aint) derivedTypePackedSourceBufferOffset;
                                     derivedTypePackedSourceBufferOffset += (ADIO_Offset_CA) bufferAmountToRecv;
                                     sourceAggContigAccessCount++;
                                 }
                             }
                         } // bufferAmountToRecv > 0
                     } // contig list

                     /* For onesided_read_aggmethod of 2 now build the derived type using
                     * the data from this round/agg and do 1 single mpi_get.
                     */
                     if (ca_data->onesided_read_aggmethod == 2) {
                         MPI_Datatype recvBufferDerivedDataType, sourceBufferDerivedDataType;

                         MPI_Type_create_struct(sourceAggContigAccessCount, sourceAggBlockLengths, recvBufferDisplacements, sourceAggDataTypes, &recvBufferDerivedDataType);
                         MPI_Type_commit(&recvBufferDerivedDataType);
                         MPI_Type_create_struct(sourceAggContigAccessCount, sourceAggBlockLengths, sourceAggDisplacements, sourceAggDataTypes, &sourceBufferDerivedDataType);
                         MPI_Type_commit(&sourceBufferDerivedDataType);

                         if (sourceAggContigAccessCount > 0) {

                             MPI_Win_lock(MPI_LOCK_SHARED, sourceAggsForMyData[aggIter], 0, read_buf_window);

                             if (bufTypeIsContig) {

                                 MPI_Get(((char *) buf), 1, recvBufferDerivedDataType, sourceAggsForMyData[aggIter], 0, 1, sourceBufferDerivedDataType, read_buf_window);

                             } else {

                                 MPI_Get(derivedTypePackedSourceBuffer, 1, recvBufferDerivedDataType, sourceAggsForMyData[aggIter], 0, 1, sourceBufferDerivedDataType, read_buf_window);

                             }

                             MPI_Win_unlock(sourceAggsForMyData[aggIter], read_buf_window);

                             if (!bufTypeIsContig) {

                                 H5FD_mpio_nc_buffer_advance(((char *) buf), flatBuf, derivedTypePackedSourceBufferOffset, 0, &currentFDSourceBufferState[aggIter], derivedTypePackedSourceBuffer);

                             }
                         }

                         if (allocatedDerivedTypeArrays) {
                             H5MM_free(sourceAggBlockLengths);
                             H5MM_free(sourceAggDisplacements);
                             H5MM_free(sourceAggDataTypes);
                             H5MM_free(recvBufferDisplacements);
                             if (!bufTypeIsContig) {
                                 if (derivedTypePackedSourceBuffer != NULL)
                                 H5MM_free(derivedTypePackedSourceBuffer);
                             }
                         }
                         if (sourceAggContigAccessCount > 0) {
                             MPI_Type_free(&recvBufferDerivedDataType);
                             MPI_Type_free(&sourceBufferDerivedDataType);
                         }
                     }
                 } // baseoffset != -1
             } // source aggs

             if (stripeSize > 0) {
                 stripe_parms->lastDataTypeExtent = currentFDSourceBufferState[numSourceAggs-1].dataTypeExtent;
                 stripe_parms->lastFlatBufIndice = currentFDSourceBufferState[numSourceAggs-1].flatBufIndice;
                 stripe_parms->lastIndiceOffset = currentFDSourceBufferState[numSourceAggs-1].indiceOffset;
             }

         } /* contig_access_count > 0 */

         /* the source procs recv the requested data to the aggs */

         /* Synchronize all procs */
         MPI_Barrier(ca_data->comm);

         nextRoundFDStart = currentRoundFDStart + coll_bufsize;

     }   /* for-loop roundIter */

    if (useIOBuffer) {  /* thread readr cleanup */
        if (!pthread_equal(io_thread, pthread_self())) {
            pthread_join(io_thread, &thread_ret);
            *error_code = *(int *) thread_ret;
        }
    }

     H5MM_free(sourceAggsForMyData);
     H5MM_free(sourceAggsForMyDataFDStart);
     H5MM_free(sourceAggsForMyDataFDEnd);

     for (i = 0; i < numberOfRounds; i++) {
         H5MM_free(sourceAggsForMyDataFirstOffLenIndex[i]);
         H5MM_free(sourceAggsForMyDataLastOffLenIndex[i]);
     }
     H5MM_free(sourceAggsForMyDataFirstOffLenIndex);
     H5MM_free(sourceAggsForMyDataLastOffLenIndex);
     H5MM_free(currentFDSourceBufferState);

     return;
 } /* H5FD_mpio_ccio_osagg_read */


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpio_ccio_file_read
 *
 * Purpose:     Read data from file into aggregators
 *
 *-------------------------------------------------------------------------
 */
void H5FD_mpio_ccio_file_read(CustomAgg_FH_Data ca_data, int *error_code,
    ADIO_Offset_CA firstFileOffset, ADIO_Offset_CA lastFileOffset,
    ADIO_Offset_CA *fd_start, ADIO_Offset_CA* fd_end)
 {
     int i,j; /* generic iterators */
     int naggs, iAmUsedAgg, myAggRank;
     MPI_Status status;
     int nprocs, myrank;
     int greatestFileDomainAggRank, smallestFileDomainAggRank;
     ADIO_Offset_CA greatestFileDomainOffset, smallestFileDomainOffset;
     ADIO_Offset_CA coll_bufsize;
     ADIO_Offset_CA readFDStart;
     ADIO_Offset_CA readFDEnd;

     *error_code = MPI_SUCCESS; /* initialize to success */
     MPI_Comm_size(ca_data->comm, &nprocs);
     MPI_Comm_rank(ca_data->comm, &myrank);

     naggs = ca_data->cb_nodes;
     iAmUsedAgg = 0; /* whether or not this rank is used as an aggregator. */
     myAggRank = -1; /* if I am an aggregor this is my index into ranklist */
     coll_bufsize = (ADIO_Offset_CA)(ca_data->cb_buffer_size);

     /*
      * Confirm that we are only dealing with ONE round here...
      */
     int numberOfRounds = 0;
     for (j=0;j<naggs;j++) {
         int currentNumberOfRounds = (int)(((fd_end[j] - fd_start[j])+(ADIO_Offset_CA)1)/coll_bufsize);
         if ( ( (ADIO_Offset_CA)currentNumberOfRounds*coll_bufsize ) < ((fd_end[j] - fd_start[j])+(ADIO_Offset_CA)1))
             currentNumberOfRounds++;
         if (currentNumberOfRounds > numberOfRounds)
             numberOfRounds = currentNumberOfRounds;
     }
     if (numberOfRounds > 1) {
         printf("ERROR -- Use of H5FD_mpio_ccio_file_read assumes there are is only ONE round for the current aggregation segment!\n");
     }

#ifdef onesidedtrace
    printf("Rank %d (use_dup == %d) called H5FD_mpio_ccio_file_read with segmentFirstFileOffset %d, segmentLastFileOffset %d, segment_stripe_start %d, segment_stripe_end %d. \n",myrank, ca_data->use_dup, (int)firstFileOffset, (int)lastFileOffset, (int)fd_start[0], (int)fd_end[0]);
#endif

     /* This logic defines values that are used later to determine what offsets define the portion
     * of the file domain the agg is reading this round.
     */
     greatestFileDomainAggRank = -1;
     smallestFileDomainAggRank = -1;
     greatestFileDomainOffset = 0;
     smallestFileDomainOffset = lastFileOffset;
     for (j=0;j<naggs;j++) {
         if (fd_end[j] > greatestFileDomainOffset) {
             greatestFileDomainOffset = fd_end[j];
             greatestFileDomainAggRank = j;
         }
         if (fd_start[j] < smallestFileDomainOffset) {
             smallestFileDomainOffset = fd_start[j];
             smallestFileDomainAggRank = j;
         }
         if (ca_data->ranklist[j] == myrank) {
             myAggRank = j;
             if (fd_end[j] > fd_start[j]) {
                 iAmUsedAgg = 1;
             }
         }
     }

     readFDStart = 0;
     readFDEnd = 0;
     if (iAmUsedAgg) {

         /* What offset to read from */
         readFDStart = fd_start[myAggRank];
         if (myAggRank == smallestFileDomainAggRank) {
             if (readFDStart < firstFileOffset)
                 readFDStart = firstFileOffset;
         } else if (myAggRank == greatestFileDomainAggRank) {
             if (readFDEnd > lastFileOffset)
                 readFDEnd = lastFileOffset;
         }

         /* How much data to read */
         int read_size;
         if ((fd_end[myAggRank] - readFDStart) < coll_bufsize) {
             readFDEnd = fd_end[myAggRank];
             read_size = ((readFDEnd - readFDStart) + 1);
         } else {
             readFDEnd = readFDStart + coll_bufsize - (ADIO_Offset_CA) 1;
             read_size = coll_bufsize;
         }

         /* Read 'read_size' bytes */
         if (ca_data->use_dup) {
             MPI_File_iread_at(ca_data->fh, readFDStart, ca_data->io_buf_d, read_size, MPI_BYTE, &ca_data->io_Request_d);
             ca_data->check_req_d = 1;
         }else {
             MPI_File_iread_at(ca_data->fh, readFDStart, ca_data->io_buf, read_size, MPI_BYTE, &ca_data->io_Request);
             ca_data->check_req = 1;
         }

     } // (iAmUsedAgg)

     /* Synchronize all procs */
     MPI_Barrier(ca_data->comm);

     return;
 } /* H5FD_mpio_ccio_file_read */

/*-------------------------------------------------------------------------
 * Function:    calc_file_domains
 *
 * Purpose:     Compute a dynamic access range based file domain partition
 *              among I/O aggregators, which align to the GPFS block size
 *              Divide the I/O workload among aggregation processes. This is
 *              done by (logically) dividing the file into file domains (FDs); each
 *              process may directly access only its own file domain.
 *              Additional effort is to make sure that each I/O aggregator gets
 *              a file domain that aligns to the GPFS block size.  So, there will
 *              not be any false sharing of GPFS file blocks among multiple I/O nodes.
 *
 * Return:      Void.
 *
 *-------------------------------------------------------------------------
 */
void calc_file_domains(ADIO_Offset_CA *st_offsets, ADIO_Offset_CA *end_offsets,
    int nprocs, int nprocs_for_coll, ADIO_Offset_CA *min_st_offset_ptr,
    ADIO_Offset_CA **fd_start_ptr, ADIO_Offset_CA **fd_end_ptr,
    ADIO_Offset_CA *fd_size_ptr, ADIO_Offset_CA blksize)
{
    ADIO_Offset_CA min_st_offset, max_end_offset, *fd_start, *fd_end, *fd_size;
    int i, aggr;

#ifdef onesidedtrace
    printf("calc_file_domains: Blocksize=%ld, nprocs=%ld, nprocs_for_coll=%ld\n",blksize,nprocs,nprocs_for_coll);
#endif
    /* find min of start offsets and max of end offsets of all processes */
    min_st_offset  = st_offsets [0];
    max_end_offset = end_offsets[0];
    for (i=1; i<nprocs; i++) {
        min_st_offset = MIN(min_st_offset, st_offsets[i]);
        max_end_offset = MAX(max_end_offset, end_offsets[i]);
    }

#ifdef onesidedtrace
    printf("calc_file_domains, min_st_offset, max_end_offset = %qd, %qd\n", min_st_offset, max_end_offset );
#endif

    /* determine the "file domain (FD)" of each process, i.e., the portion of
    the file that will be "owned" by each process */

    ADIO_Offset_CA gpfs_ub       = (max_end_offset +blksize-1) / blksize * blksize - 1;
    ADIO_Offset_CA gpfs_lb       = min_st_offset / blksize * blksize;
    ADIO_Offset_CA gpfs_ub_rdoff = (max_end_offset +blksize-1) / blksize * blksize - 1 - max_end_offset;
    ADIO_Offset_CA gpfs_lb_rdoff = min_st_offset - min_st_offset / blksize * blksize;
    ADIO_Offset_CA fd_gpfs_range = gpfs_ub - gpfs_lb + 1;

    int         naggs    = nprocs_for_coll;

    /* Tweak the file domains so that no fd is smaller than a threshold.  We
    * have to strike a balance between efficency and parallelism: somewhere
    * between 10k processes sending 32-byte requests and one process sending a
    * 320k request is a (system-dependent) sweet spot

    This is from the common code - the new min_fd_size parm that we didn't implement.
    (And common code uses a different declaration of fd_size so beware)

    if (fd_size < min_fd_size)
    fd_size = min_fd_size;
    */
    fd_size        = (ADIO_Offset_CA *) H5MM_malloc(nprocs_for_coll * sizeof(ADIO_Offset_CA));
    *fd_start_ptr  = (ADIO_Offset_CA *) H5MM_malloc(nprocs_for_coll * sizeof(ADIO_Offset_CA));
    *fd_end_ptr    = (ADIO_Offset_CA *) H5MM_malloc(nprocs_for_coll * sizeof(ADIO_Offset_CA));
    fd_start       = *fd_start_ptr;
    fd_end         = *fd_end_ptr;

    /* each process will have a file domain of some number of gpfs blocks, but
     * the division of blocks is not likely to be even.  Some file domains will
     * be "large" and others "small"
     *
     * Example: consider  17 blocks distributed over 3 aggregators.
     * nb_cn_small = 17/3 = 5
     * naggs_large = 17 - 3*(17/3) = 17 - 15  = 2
     * naggs_small = 3 - 2 = 1
     *
     * and you end up with file domains of {5-blocks, 6-blocks, 6-blocks}
     *
     * what about (relatively) small files?  say, a file of 1000 blocks
     * distributed over 2064 aggregators:
     * nb_cn_small = 1000/2064 = 0
     * naggs_large = 1000 - 2064*(1000/2064) = 1000
     * naggs_small = 2064 - 1000 = 1064
     * and you end up with domains of {0, 0, 0, ... 1, 1, 1 ...}
     *
     * it might be a good idea instead of having all the zeros up front, to
     * "mix" those zeros into the fd_size array.  that way, no pset/bridge-set
     * is left with zero work.  In fact, even if the small file domains aren't
     * zero, it's probably still a good idea to mix the "small" file domains
     * across the fd_size array to keep the io nodes in balance
     */

    ADIO_Offset_CA n_gpfs_blk  = fd_gpfs_range / blksize;
    ADIO_Offset_CA nb_cn_small = n_gpfs_blk/naggs;
    ADIO_Offset_CA naggs_large = n_gpfs_blk - naggs * (n_gpfs_blk/naggs);
    ADIO_Offset_CA naggs_small = naggs - naggs_large;

    /* simple allocation of file domins to each aggregator */
    for (i=0; i<naggs; i++) {
        if (i < naggs_large) {
            fd_size[i] = (nb_cn_small+1) * blksize;
        } else {
            fd_size[i] = nb_cn_small * blksize;
        }
    }

#ifdef onesidedtrace
    printf("gpfs_ub       %llu, gpfs_lb       %llu, gpfs_ub_rdoff %llu, gpfs_lb_rdoff %llu, fd_gpfs_range %llu, n_gpfs_blk    %llu, nb_cn_small   %llu, naggs_large   %llu, naggs_small   %llu\n",
    gpfs_ub      ,
    gpfs_lb      ,
    gpfs_ub_rdoff,
    gpfs_lb_rdoff,
    fd_gpfs_range,
    n_gpfs_blk   ,
    nb_cn_small  ,
    naggs_large  ,
    naggs_small
    );
    printf("File domains:\n");
    fflush(stdout);
#endif

    fd_size[0]       -= gpfs_lb_rdoff;
    fd_size[naggs-1] -= gpfs_ub_rdoff;

    /* compute the file domain for each aggr */
    ADIO_Offset_CA offset = min_st_offset;
    for (aggr=0; aggr<naggs; aggr++) {
        fd_start[aggr] = offset;
        fd_end  [aggr] = offset + fd_size[aggr] - 1;
        offset += fd_size[aggr];
#ifdef onesidedtrace
        printf("fd[%d]: start %ld end %ld\n",aggr,fd_start[aggr],fd_end[aggr]);
        fflush(stdout);
#endif
    }

    *fd_size_ptr = fd_size[0];
    *min_st_offset_ptr = min_st_offset;

    H5MM_free (fd_size);
}

/*-------------------------------------------------------------------------
 * Function:    IO_Thread_Func
 *
 * Purpose:     Function for running in another thread for doing the file
 *              reading while the main thread is doing data aggregation -
 *              useful only when multiple rounds are needed due to file size
 *              relative to the read buffer size and number of aggregators
 *
 * Return:      Void.
 *
 *-------------------------------------------------------------------------
 */
void *IO_Thread_Func(void *vptr_args) {
    ThreadFuncData *args = (ThreadFuncData*)vptr_args;
#ifdef onesidedtrace
        printf("Rank %d - In IO_Thread_Func.\n", args->myrank);
        fflush(stdout);
#endif
    if (args->size > 0) {
        if (args->io_kind == READ_CA) {
            args->error_code = MPI_File_read_at(args->fh, args->offset, args->buf, args->size, MPI_BYTE, &(args->error_code));
        } else {
            args->error_code = MPI_File_write_at(args->fh, args->offset, args->buf, args->size, MPI_BYTE, &(args->error_code));
        }
#ifdef onesidedtrace
        int eclass, len;
        char estring[MPI_MAX_ERROR_STRING];
        MPI_Error_string(args->error_code, estring, &len);
        printf("Rank %d - Leaving IO_Thread_Func with CODE %d: %s (int: %d) (offset=%d,size=%d)\n", args->myrank, eclass, estring, args->error_code, args->offset, args->size);
        fflush(stdout);
#endif
    } else {
        args->error_code = 0;
#ifdef onesidedtrace
        printf("Rank %d - WARNING: Leaving IO_Thread_Func WITHOUT doing IO OP (size = %d)\n", args->myrank, args->size);
        fflush(stdout);
#endif
    }
    pthread_exit(&(args->error_code));
    return NULL;
}

#endif /* H5_HAVE_PARALLEL */
