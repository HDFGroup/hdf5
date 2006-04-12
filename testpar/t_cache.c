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
 * Parallel metadata cache tests.
 *
 */

#include "h5test.h"
#include "testpar.h"
#include "H5Iprivate.h"
#include "H5ACprivate.h"

#define H5C_PACKAGE             /*suppress error about including H5Cpkg   */

#include "H5Cpkg.h"

#define H5F_PACKAGE             /*suppress error about including H5Fpkg   */

#include "H5Fpkg.h"


int     	nerrors = 0;
int		failures = 0;
hbool_t		verbose = TRUE; /* used to control error messages */
#if 0
/* So far we haven't needed this, but that may change.  
 * Keep it around for now 
 */
hid_t noblock_dxpl_id=(-1);
#endif

#define NFILENAME 2
#define PARATESTFILE filenames[0]
const char *FILENAME[NFILENAME]={"CacheTestDummy", NULL};
char    filenames[NFILENAME][PATH_MAX];
hid_t   fapl;                           /* file access property list */


int		world_mpi_size = -1;
int		world_mpi_rank = -1;
int		world_server_mpi_rank = -1;
MPI_Comm	world_mpi_comm = MPI_COMM_NULL;
int		file_mpi_size = -1;
int		file_mpi_rank = -1;
MPI_Comm	file_mpi_comm = MPI_COMM_NULL;


/*****************************************************************************
 * struct datum
 *
 *	Instances of struct datum are used to store information on entries
 *	that may be loaded into the cache.  The individual fields are 
 *	discussed below:
 *
 *	header:	Instance of H5C_cache_entry_t used by the for its data.
 *		This field is only used on the file processes, not on the 
 *		server process.
 *
 *		This field MUST be the first entry in this structure.
 *
 *	base_addr: Base address of the entry.
 *
 *	len:	Length of the entry.
 *
 *	ver:	Version number of the entry.  This number is initialize
 *		to zero, and incremented each time the entry is modified.
 *
 *	dirty:	Boolean flag indicating whether the entry is dirty.
 *
 *		For current purposes, an entry is clean until it is 
 *		modified, and dirty until written to the server (cache
 *		on process 0) or until it is marked clean (all other 
 *		caches).
 *
 *	valid:	Boolean flag indicating whether the entry contains
 *		valid data.  Attempts to read an entry whose valid
 *		flag is not set should trigger an error.
 *
 *	locked:	Boolean flag that is set to true iff the entry is in 
 *		the cache and locked.
 *
 *	index:	Index of this instance of datum in the data_index[] array
 *		discussed below.
 *
 *****************************************************************************/

struct datum 
{
    H5C_cache_entry_t	header;
    haddr_t		base_addr;
    size_t		len;
    int			ver;
    hbool_t		dirty;
    hbool_t		valid;
    hbool_t		locked;
    int			index;
};

/*****************************************************************************
 * data array
 *
 *	The data array is an array of instances of datum of size 
 *	NUM_DATA_ENTRIES that is used to track the particulars of all
 *	the entries that may be loaded into the cache.
 *
 *	It exists on all processes, although the master copy is maintained
 *	by the server process.  If the cache is performing correctly, all
 *	versions should be effectively identical.  By that I mean that
 *	the data received from the server should always match that in 
 *	the local version of the data array.
 *
 *****************************************************************************/

#ifndef H5_HAVE_MPE
#define NUM_DATA_ENTRIES	100000
#else
    /* Use a smaller test size to avoid creating huge MPE logfiles. */
#define NUM_DATA_ENTRIES	1000
#endif

struct datum data[NUM_DATA_ENTRIES];


/*****************************************************************************
 * data_index array
 *
 *	The data_index array is an array of integer used to maintain a list
 *	of instances of datum in the data array in increasing base_addr order.
 *
 *	This array is necessary, as rename operations can swap the values
 *	of the base_addr fields of two instances of datum.  Without this
 *	array, we would no longer be able to use a binary search on a sorted
 *	list to find the indexes of instances of datum given the values of 
 *	their base_addr fields.
 *	
 *****************************************************************************/

int data_index[NUM_DATA_ENTRIES];


/*****************************************************************************
 * struct mssg
 *
 *	The mssg structure is used as a generic container for messages to 
 *	and from the server.  Not all fields are used in all cases.
 *
 *	req:	Integer field containing the type of the message.
 *
 *	src:	World communicator MPI rank of the sending process.
 *
 *	dest:	World communicator MPI rank of the destination process.
 *
 *	mssg_num:	Serial number assigned to the message by the sender.
 *
 *	base_addr:	Base address of a datum.  Not used in all mssgs.
 *
 *	len:	Length of a datum (in bytes).  Not used in all mssgs.
 *
 *	ver:	Version number of a datum.  Not used in all mssgs.
 *
 *	magic:	Magic number for error detection.  Must be set to 
 *		MSSG_MAGIC.
 *
 *****************************************************************************/

#define	WRITE_REQ_CODE		0
#define READ_REQ_CODE		1
#define READ_REQ_REPLY_CODE	2
#define DONE_REQ_CODE		3
#define MAX_REQ_CODE		3

#define MSSG_MAGIC	0x1248

typedef struct mssg_t
{
    int		req;
    int		src;
    int		dest;
    long int	mssg_num;
    haddr_t	base_addr;
    int		len;
    int		ver;
    unsigned	magic;
};

MPI_Datatype mpi_mssg_t;	/* for MPI derived type created from mssg */


/*****************************************************************************/
/************************** function declarations ****************************/
/*****************************************************************************/

/* MPI setup functions */

hbool_t set_up_file_communicator(void);


/* data array manipulation functions */

int addr_to_datum_index(haddr_t base_addr);
void init_data(void);


/* mssg xfer related functions */

int get_max_nerrors(void);
hbool_t recv_mssg(struct mssg_t *mssg_ptr);
hbool_t send_mssg(struct mssg_t *mssg_ptr);
hbool_t setup_derived_types(void);
hbool_t takedown_derived_types(void);


/* server functions */

hbool_t server_main(void);
hbool_t serve_read_request(struct mssg_t * mssg_ptr);
hbool_t serve_write_request(struct mssg_t * mssg_ptr);


/* call back functions & related data structures */

herr_t clear_datum(H5F_t * f, void *  thing, hbool_t dest);
herr_t destroy_datum(H5F_t UNUSED * f, void * thing);
herr_t flush_datum(H5F_t *f, hid_t UNUSED dxpl_id, hbool_t dest, haddr_t addr, 
                   void *thing);
void * load_datum(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, haddr_t addr,
                  const void UNUSED *udata1, void UNUSED *udata2);
herr_t size_datum(H5F_t UNUSED * f, void * thing, size_t * size_ptr);

#define DATUM_ENTRY_TYPE	H5AC_TEST_ID

#define NUMBER_OF_ENTRY_TYPES	1

const H5C_class_t types[NUMBER_OF_ENTRY_TYPES] =
{
  {
    DATUM_ENTRY_TYPE,
    (H5C_load_func_t)load_datum,
    (H5C_flush_func_t)flush_datum,
    (H5C_dest_func_t)destroy_datum,
    (H5C_clear_func_t)clear_datum,
    (H5C_size_func_t)size_datum
  }
};


/* test utility functions */

void insert_entry(H5C_t * cache_ptr, H5F_t * file_ptr, 
                  int32_t idx, unsigned int flags);
void lock_and_unlock_random_entries(H5C_t * cache_ptr, H5F_t * file_ptr,
                                    int min_idx, int max_idx,
                                    int min_count, int max_count);
void lock_and_unlock_random_entry(H5C_t * cache_ptr, H5F_t * file_ptr,
                                  int min_idx, int max_idx);
void lock_entry(H5C_t * cache_ptr, H5F_t * file_ptr, int32_t idx);
void rename_entry(H5C_t * cache_ptr, H5F_t * file_ptr,
                  int32_t old_idx, int32_t new_idx);
hbool_t setup_cache_for_test(hid_t * fid_ptr, H5F_t ** file_ptr_ptr,
                             H5C_t ** cache_ptr_ptr);
void setup_rand(void);
hbool_t take_down_cache(hid_t fid);
void unlock_entry(H5C_t * cache_ptr, H5F_t * file_ptr, 
                  int32_t type, unsigned int flags);


/* test functions */

hbool_t server_smoke_check(void);
hbool_t smoke_check_1(void);
hbool_t smoke_check_2(void);
hbool_t smoke_check_3(void);
hbool_t smoke_check_4(void);


/*****************************************************************************/
/**************************** MPI setup functions ****************************/
/*****************************************************************************/

/*****************************************************************************
 *
 * Function:	set_up_file_communicator()
 *
 * Purpose:	Create the MPI communicator used to open a HDF5 file with.
 *		In passing, also initialize the file_mpi... globals.
 *
 * Return:	Success:	TRUE
 *
 *		Failure:	FALSE
 *
 * Programmer:	JRM -- 11/16/05
 *
 * Modifications:
 *
 *		None.
 *	
 *****************************************************************************/

hbool_t
set_up_file_communicator(void)
{
    const char * fcn_name = "set_up_file_communicator()";
    hbool_t success = TRUE;
    int mpi_result;
    int num_excluded_ranks;
    int excluded_ranks[1];
    MPI_Group file_group;
    MPI_Group world_group;

    if ( success ) {

        mpi_result = MPI_Comm_group(world_mpi_comm, &world_group);

        if ( mpi_result != MPI_SUCCESS ) {

            nerrors++;
            success = FALSE;
	    if ( verbose ) {
                fprintf(stdout, 
                        "%d:%s: MPI_Comm_group() failed with error %d.\n",
                        world_mpi_rank, fcn_name, mpi_result);
            }
        }
    }

    if ( success ) {
 
        num_excluded_ranks = 1;
        excluded_ranks[0] = world_server_mpi_rank;
        mpi_result = MPI_Group_excl(world_group, num_excluded_ranks, 
                                    excluded_ranks, &file_group);

        if ( mpi_result != MPI_SUCCESS ) {

            nerrors++;
            success = FALSE;
            if ( verbose ) {
                fprintf(stdout, 
                        "%d:%s: MPI_Group_excl() failed with error %d.\n",
                        world_mpi_rank, fcn_name, mpi_result);
            }
        }
    }

    if ( success ) {

        mpi_result = MPI_Comm_create(world_mpi_comm, file_group, 
                                     &file_mpi_comm);

        if ( mpi_result != MPI_SUCCESS ) {

            nerrors++;
            success = FALSE;
            if ( verbose ) {
                fprintf(stdout, 
                        "%d:%s: MPI_Comm_create() failed with error %d.\n",
                        world_mpi_rank, fcn_name, mpi_result);
            }

        } else {

            if ( world_mpi_rank != world_server_mpi_rank ) {

                if ( file_mpi_comm == MPI_COMM_NULL ) {

                    nerrors++;
                    success = FALSE;
                    if ( verbose ) {
                        fprintf(stdout, 
                                "%d:%s: file_mpi_comm == MPI_COMM_NULL.\n",
                                world_mpi_rank, fcn_name);
                    }
                }
            } else {

                file_mpi_size = world_mpi_size - 1; /* needed by the server */

                if ( file_mpi_comm != MPI_COMM_NULL ) {

                    nerrors++;
                    success = FALSE;
                    if ( verbose ) {
                        fprintf(stdout, 
                                "%d:%s: file_mpi_comm != MPI_COMM_NULL.\n",
                                world_mpi_rank, fcn_name);
                    }
                }
            }
        }
    }

    if ( ( success ) && ( world_mpi_rank != world_server_mpi_rank ) ) {

        mpi_result = MPI_Comm_size(file_mpi_comm, &file_mpi_size);

        if ( mpi_result != MPI_SUCCESS ) {

            nerrors++;
            success = FALSE;
            if ( verbose ) {
                fprintf(stdout, 
                        "%d:%s: MPI_Comm_size() failed with error %d.\n",
                        world_mpi_rank, fcn_name, mpi_result);
            }
        }
    }

    if ( ( success ) && ( world_mpi_rank != world_server_mpi_rank ) ) {

        mpi_result = MPI_Comm_rank(file_mpi_comm, &file_mpi_rank);

        if ( mpi_result != MPI_SUCCESS ) {

            nerrors++;
            success = FALSE;
            if ( verbose ) {
                fprintf(stdout, 
                        "%d:%s: MPI_Comm_rank() failed with error %d.\n",
                        world_mpi_rank, fcn_name, mpi_result);
            }
        }
    }

#if 0 /* Useful debuggging code -- lets keep it around for a while */
    if ( success ) {

        fprintf(stdout, "%d:%s: file mpi size = %d, file mpi rank = %d.\n",
                world_mpi_rank, fcn_name, file_mpi_size, file_mpi_rank);
    }
#endif /* JRM */

    return(success);

} /* set_up_file_communicator() */


/*****************************************************************************/
/******************** data array manipulation functions **********************/
/*****************************************************************************/

/*****************************************************************************
 *
 * Function:	addr_to_datum_index()
 *
 * Purpose:	Given the base address of a datum, find and return its index 
 *		in the data array.
 *
 * Return:	Success:	index of target datum.
 *
 *		Failure:	-1.
 *
 * Programmer:	JRM -- 12/20/05
 *
 * Modifications:
 *
 *		None.
 *	
 *****************************************************************************/

int
addr_to_datum_index(haddr_t base_addr)
{
    const char * fcn_name = "addr_to_datum_index()";
    int top = NUM_DATA_ENTRIES - 1;
    int bottom = 0;
    int middle = (NUM_DATA_ENTRIES - 1) / 2;
    int ret_value = -1;

    while ( top >= bottom )
    {
        if ( base_addr < data[data_index[middle]].base_addr ) {

            top = middle - 1;
            middle = (top + bottom) / 2;

        } else if ( base_addr > data[data_index[middle]].base_addr ) {

            bottom = middle + 1;
            middle = (top + bottom) / 2;

        } else /* ( base_addr == data[data_index[middle]].base_addr ) */ {

            ret_value = data_index[middle];
            bottom = top + 1; /* to force exit from while loop */

        } 
    }

    return(ret_value);

} /* addr_to_datum_index() */


/*****************************************************************************
 *
 * Function:	init_data_array()
 *
 * Purpose:	Initialize the data array, from which cache entries are 
 *		loaded.
 *
 * Return:	Success:	TRUE
 *
 *		Failure:	FALSE
 *
 * Programmer:	JRM -- 12/20/05
 *
 * Modifications:
 *
 *		None.
 *	
 *****************************************************************************/

void
init_data(void)
{
    const char * fcn_name = "init_data()";
    /* The set of address offsets is chosen so as to avoid allowing the 
     * base addresses to fall in a pattern of that will annoy the hash
     * table, and to give a good range of entry sizes.
     *
     * At present, I am using the first 20 entries of the Fibonacci 
     * sequence multiplied by 2.  We will see how it works.
     */
    const int num_addr_offsets = 20;
    const haddr_t addr_offsets[20] = {   2,    2,    4,    6,    10, 
                                        16,   26,   42,   68,   110, 
                                       178,  288,  466,  754,  1220, 
                                      1974, 3194, 5168, 8362, 13539};
    int i;
    int j = 0;
    haddr_t addr = 0;

    /* this must hold so renames don't change entry size. */
    HDassert( (NUM_DATA_ENTRIES / 2) % 20 == 0 );

    for ( i = 0; i < NUM_DATA_ENTRIES; i++ )
    {
        data[i].base_addr = addr;
        data[i].len       = (size_t)(addr_offsets[j]);
        data[i].ver       = 0;
        data[i].dirty     = FALSE;
        data[i].valid     = FALSE;
        data[i].locked    = FALSE;
	data[i].index     = i;

        data_index[i]     = i;

        addr += addr_offsets[j];
        HDassert( addr > data[i].base_addr );

        j = (j + 1) % num_addr_offsets;
    }
    
    return;

} /* init_data() */


/*****************************************************************************/
/************************ mssg xfer related functions ************************/
/*****************************************************************************/

/*****************************************************************************
 *
 * Function:	get_max_nerrors()
 *
 * Purpose:	Do an MPI_Allreduce to obtain the maximum value of nerrors 
 *		across all processes.  Return this value.
 *
 * Return:	Success:	Maximum of the nerrors global variables across
 *				all processes.
 *
 *		Failure:	-1
 *
 * Programmer:	JRM -- 1/3/06
 *
 * Modifications:
 *
 *		None.
 *	
 *****************************************************************************/

int
get_max_nerrors(void)
{
    const char * fcn_name = "get_max_nerrors()";
    int max_nerrors;
    int result;

    result = MPI_Allreduce((void *)&nerrors,
                           (void *)&max_nerrors,
                           1,
                           MPI_INT,
                           MPI_MAX,
                           world_mpi_comm);

    if ( result != MPI_SUCCESS ) {

        nerrors++;
        max_nerrors = -1;
        if ( verbose ) {
            HDfprintf(stdout, "%d:%s: MPI_Allreduce() failed.\n", 
                      world_mpi_rank, fcn_name );
        }
    } 

    return(max_nerrors);

} /* get_max_nerrors() */


/*****************************************************************************
 *
 * Function:	recv_mssg()
 *
 * Purpose:	Receive a message from any process in the provided instance 
 *		of struct mssg.
 *
 * Return:	Success:	TRUE
 *
 *		Failure:	FALSE
 *
 * Programmer:	JRM -- 12/22/05
 *
 * Modifications:
 *
 *		None.
 *	
 *****************************************************************************/

#define CACHE_TEST_TAG	99 /* different from any used by the library */

hbool_t
recv_mssg(struct mssg_t *mssg_ptr)
{
    const char * fcn_name = "recv_mssg()";
    hbool_t success = TRUE;
    int result;
    static long mssg_num = 0;
    MPI_Status status;

    if ( mssg_ptr == NULL ) {

        nerrors++;
        success = FALSE;
        if ( verbose ) {
            HDfprintf(stdout, "%d:%s: NULL mssg_ptr on entry.\n", 
                      world_mpi_rank, fcn_name);
        }
    }

    if ( success ) {

        result = MPI_Recv((void *)mssg_ptr, 1, mpi_mssg_t, MPI_ANY_SOURCE,
                          CACHE_TEST_TAG, world_mpi_comm, &status);

        if ( result != MPI_SUCCESS ) {

            nerrors++;
            success = FALSE;
            if ( verbose ) {
                HDfprintf(stdout, "%d:%s: MPI_Recv() failed.\n", 
                          world_mpi_rank, fcn_name );
            }
        } else if ( mssg_ptr->magic != MSSG_MAGIC ) {

            nerrors++;
            success = FALSE;
            if ( verbose ) {
                HDfprintf(stdout, "%d:%s: invalid magic.\n", world_mpi_rank,
                          fcn_name);
            }
        } else if ( mssg_ptr->src != status.MPI_SOURCE ) {

            nerrors++;
            success = FALSE;
            if ( verbose ) {
                HDfprintf(stdout, 
                          "%d:%s: mssg_ptr->src != status.MPI_SOURCE.\n", 
                          world_mpi_rank, fcn_name);
            }
        }
    }

    return(success);

} /* recv_mssg() */


/*****************************************************************************
 *
 * Function:	send_mssg()
 *
 * Purpose:	Send the provided instance of mssg to the indicated target.
 *
 *		Note that all source and destination ranks are in the 
 *		global communicator.
 *
 * Return:	Success:	TRUE
 *
 *		Failure:	FALSE
 *
 * Programmer:	JRM -- 12/22/05
 *
 * Modifications:
 *
 *		None.
 *	
 *****************************************************************************/

hbool_t
send_mssg(struct mssg_t *mssg_ptr)
{
    const char * fcn_name = "send_mssg()";
    hbool_t success = TRUE;
    int result;
    static long mssg_num = 0;

    if ( ( mssg_ptr == NULL ) ||
         ( mssg_ptr->src != world_mpi_rank ) ||
         ( mssg_ptr->dest < 0 ) ||
         ( mssg_ptr->dest == mssg_ptr->src ) ||
         ( mssg_ptr->dest >= world_mpi_size ) ||
         ( mssg_ptr->req < 0 ) ||
         ( mssg_ptr->req > MAX_REQ_CODE ) ||
         ( mssg_ptr->magic != MSSG_MAGIC ) ) {

        nerrors++;
        success = FALSE;
        if ( verbose ) {
            HDfprintf(stdout, "%d:%s: Invalid mssg on entry.\n", 
                      world_mpi_rank, fcn_name);
        }
    }

    if ( success ) {

        mssg_ptr->mssg_num = mssg_num++;

        result = MPI_Send((void *)mssg_ptr, 1, mpi_mssg_t,
                          mssg_ptr->dest, CACHE_TEST_TAG, world_mpi_comm);

        if ( result != MPI_SUCCESS ) {

            nerrors++;
            success = FALSE;
            if ( verbose ) {
                HDfprintf(stdout, "%d:%s: MPI_Send() failed.\n", 
                          world_mpi_rank, fcn_name);
            }
        }
    }

    return(success);

} /* send_mssg() */


/*****************************************************************************
 *
 * Function:	setup_derived_types()
 *
 * Purpose:	Set up the derived types used by the test bed.  At present,
 *		only the mpi_mssg derived type is needed.
 *
 * Return:	Success:	TRUE
 *
 *		Failure:	FALSE
 *
 * Programmer:	JRM -- 12/22/05
 *
 * Modifications:
 *
 *		None.
 *	
 *****************************************************************************/

hbool_t
setup_derived_types(void)
{
    const char * fcn_name = "setup_derived_types()";
    hbool_t success = TRUE;
    int i;
    int result;
    MPI_Datatype types[8] = {MPI_INT, MPI_INT, MPI_INT, MPI_LONG,
                             HADDR_AS_MPI_TYPE, MPI_INT, MPI_INT, 
                             MPI_UNSIGNED};
    int block_len[8] = {1, 1, 1, 1, 1, 1, 1, 1};
    MPI_Aint displs[8];
    struct mssg_t sample; /* used to compute displacements */

    /* setup the displacements array */
    if ( ( MPI_SUCCESS != MPI_Address(&sample.req, &displs[0]) ) ||
         ( MPI_SUCCESS != MPI_Address(&sample.src, &displs[1]) ) ||
         ( MPI_SUCCESS != MPI_Address(&sample.dest, &displs[2]) ) ||
         ( MPI_SUCCESS != MPI_Address(&sample.mssg_num, &displs[3]) ) ||
         ( MPI_SUCCESS != MPI_Address(&sample.base_addr, &displs[4]) ) ||
         ( MPI_SUCCESS != MPI_Address(&sample.len, &displs[5]) ) ||
         ( MPI_SUCCESS != MPI_Address(&sample.ver, &displs[6]) ) ||
         ( MPI_SUCCESS != MPI_Address(&sample.magic, &displs[7]) ) ) {

        nerrors++;
        success = FALSE;
        if ( verbose ) {
            HDfprintf(stdout, "%d:%s: MPI_Address() call failed.\n", 
                      world_mpi_rank, fcn_name);
        }

    } else {

        /* Now calculate the actual displacements */
        for ( i = 7; i >= 0; --i)
        {
            displs[i] -= displs[0];
        }
    }

    if ( success ) {

        result = MPI_Type_struct(8, block_len, displs, types, &mpi_mssg_t);

        if ( result != MPI_SUCCESS ) {

            nerrors++;
            success = FALSE;
            if ( verbose ) {
                HDfprintf(stdout, "%d:%s: MPI_Type_struct() call failed.\n", 
                          world_mpi_rank, fcn_name);
            }
        }
    }

    if ( success ) {

        result = MPI_Type_commit(&mpi_mssg_t);

        if ( result != MPI_SUCCESS) {

            nerrors++;
            success = FALSE;
            if ( verbose ) {
                HDfprintf(stdout, "%d:%s: MPI_Type_commit() call failed.\n", 
                          world_mpi_rank, fcn_name);
            }
        }
    }

    return(success);

} /* setup_derived_types */


/*****************************************************************************
 *
 * Function:	takedown_derived_types()
 *
 * Purpose:	take down the derived types used by the test bed.  At present,
 *		only the mpi_mssg derived type is needed.
 *
 * Return:	Success:	TRUE
 *
 *		Failure:	FALSE
 *
 * Programmer:	JRM -- 12/22/05
 *
 * Modifications:
 *
 *		None.
 *	
 *****************************************************************************/

hbool_t
takedown_derived_types(void)
{
    const char * fcn_name = "takedown_derived_types()";
    hbool_t success = TRUE;
    int i;
    int result;

    result = MPI_Type_free(&mpi_mssg_t);

    if ( result != MPI_SUCCESS ) {

        nerrors++;
        success = FALSE;
        if ( verbose ) {
            HDfprintf(stdout, "%d:%s: MPI_Type_free() call failed.\n", 
            world_mpi_rank, fcn_name);
        }
    }

    return(success);

} /* takedown_derived_types() */


/*****************************************************************************/
/***************************** server functions ******************************/
/*****************************************************************************/

/*****************************************************************************
 *
 * Function:	server_main()
 *
 * Purpose:	Main function for the server process.  This process exists
 *		to provide an independant view of the data array.
 *
 *		The function handles request from the other processes in
 *		the test until the count of done messages received equals
 *		the number of client processes.
 *
 * Return:	Success:	TRUE
 *
 *		Failure:	FALSE
 *
 * Programmer:	JRM -- 12/22/05
 *
 * Modifications:
 *
 *		None.
 *	
 *****************************************************************************/

hbool_t 
server_main(void)
{
    const char * fcn_name = "server_main()";
    hbool_t done = FALSE;
    hbool_t success = TRUE;
    int done_count = 0;
    struct mssg_t mssg;

    if ( world_mpi_rank != world_server_mpi_rank ) {

        nerrors++;
        success = FALSE;
        if ( verbose ) {
            HDfprintf(stdout, "%d:%s: This isn't the server process?!?!?\n", 
                      world_mpi_rank, fcn_name);
        }
    }


    while ( ( success ) && ( ! done ) )
    {
        success = recv_mssg(&mssg);

        if ( success ) {

            switch ( mssg.req )
            {
		case WRITE_REQ_CODE:
		    success = serve_write_request(&mssg);
		    break;

		case READ_REQ_CODE:
                    success = serve_read_request(&mssg);
		    break;

		case READ_REQ_REPLY_CODE:
                    success = FALSE;
		    HDfprintf(stdout, "%s: Received read req reply?!?.\n", 
			      fcn_name);
		    break;

		case DONE_REQ_CODE:
		    done_count++;
                    /* HDfprintf(stdout, "%d:%s: done_count = %d.\n",
                              world_mpi_rank, fcn_name, done_count); */
		    if ( done_count >= file_mpi_size ) {

			done = TRUE;
		    }
		    break;

		default:
                    nerrors++;
                    success = FALSE;
                    if ( verbose ) {
		        HDfprintf(stdout, "%d:%s: Unknown request code.\n", 
                                  world_mpi_rank, fcn_name);
                    }
		    break;
            }
        }
    }

    return(success);

} /* server_main() */


/*****************************************************************************
 *
 * Function:	serve_read_request()
 *
 * Purpose:	Serve a read request.
 *
 *		The function accepts a pointer to an instance of struct 
 *		mssg_t as input.  If all sanity checks pass, it sends
 *		a copy of the indicated datum from the data array to
 *		the requesting process.
 *
 * Return:	Success:	TRUE
 *
 *		Failure:	FALSE
 *
 * Programmer:	JRM -- 12/22/05
 *
 * Modifications:
 *
 *		None.
 *	
 *****************************************************************************/

hbool_t
serve_read_request(struct mssg_t * mssg_ptr)
{
    const char * fcn_name = "serve_read_request()";
    hbool_t success = TRUE;
    int target_index;
    haddr_t target_addr;
    struct mssg_t reply;

    if ( ( mssg_ptr == NULL ) || 
         ( mssg_ptr->req != READ_REQ_CODE ) ||
         ( mssg_ptr->magic != MSSG_MAGIC ) ) {

        nerrors++;
        success = FALSE;
        if ( verbose ) {
            HDfprintf(stdout, "%d:%s: Bad mssg on entry.\n", 
                      world_mpi_rank, fcn_name);
        }
    }

    if ( success ) {

        target_addr = mssg_ptr->base_addr;
        target_index = addr_to_datum_index(target_addr);

        if ( target_index < 0 ) {

            nerrors++;
            success = FALSE;
            if ( verbose ) {
                HDfprintf(stdout, "%d:%s: addr lookup failed for %a.\n", 
                          world_mpi_rank, fcn_name, target_addr);
            }
        } else if ( data[target_index].len != mssg_ptr->len ) {

            nerrors++;
            success = FALSE;
            if ( verbose ) {
                HDfprintf(stdout, 
                          "%d:%s: data[i].len = %d != mssg->len = %d.\n",
                           world_mpi_rank, fcn_name, 
                           data[target_index].len, mssg_ptr->len);
            }
        } else if ( ! (data[target_index].valid) ) {

            nerrors++;
            success = FALSE;
            if ( verbose ) {
                HDfprintf(stdout, 
                         "%d:%s: proc %d read invalid entry. base_addr = %a.\n",
                         world_mpi_rank, fcn_name, 
                         mssg_ptr->src, data[target_index].base_addr);
            }
        } else {

            /* compose the reply message */
            reply.req       = READ_REQ_REPLY_CODE;
            reply.src       = world_mpi_rank;
            reply.dest      = mssg_ptr->src;
            reply.mssg_num  = -1; /* set by send function */
            reply.base_addr = data[target_index].base_addr;
            reply.len       = data[target_index].len;
            reply.ver       = data[target_index].ver;
            reply.magic     = MSSG_MAGIC;
        }
    }

    if ( success ) {

        success = send_mssg(&reply);
    }
 
    return(success);

} /* serve_read_request() */


/*****************************************************************************
 *
 * Function:	serve_write_request()
 *
 * Purpose:	Serve a write request.
 *
 *		The function accepts a pointer to an instance of struct 
 *		mssg_t as input.  If all sanity checks pass, it updates
 *		the version number of the target data array entry as
 *		specified in the message.
 *
 * Return:	Success:	TRUE
 *
 *		Failure:	FALSE
 *
 * Programmer:	JRM -- 12/21/05
 *
 * Modifications:
 *
 *		None.
 *	
 *****************************************************************************/

hbool_t
serve_write_request(struct mssg_t * mssg_ptr)
{
    const char * fcn_name = "serve_write_request()";
    hbool_t success = TRUE;
    int target_index;
    int new_ver_num;
    haddr_t target_addr;

    if ( ( mssg_ptr == NULL ) || 
         ( mssg_ptr->req != WRITE_REQ_CODE ) ||
         ( mssg_ptr->magic != MSSG_MAGIC ) ) {

        nerrors++;
        success = FALSE;
        if ( verbose ) {
            HDfprintf(stdout, "%d:%s: Bad mssg on entry.\n", 
                      world_mpi_rank, fcn_name);
        }
    }

    if ( success ) {

        target_addr = mssg_ptr->base_addr;
        target_index = addr_to_datum_index(target_addr);

        if ( target_index < 0 ) {

            nerrors++;
            success = FALSE;
            if ( verbose ) {
                HDfprintf(stdout, "%d:%s: addr lookup failed for %a.\n", 
                          world_mpi_rank, fcn_name, target_addr);
            }
        } else if ( data[target_index].len != mssg_ptr->len ) {

            nerrors++;
            success = FALSE;
            if ( verbose ) {
                HDfprintf(stdout, 
                          "%d:%s: data[i].len = %d != mssg->len = %d.\n",
                          world_mpi_rank, fcn_name, 
                          data[target_index].len, mssg_ptr->len);
            }
        }
    }

    if ( success ) {

        new_ver_num = mssg_ptr->ver;

	if ( new_ver_num <= data[target_index].ver ) {

            nerrors++;
            success = FALSE;
            if ( verbose ) {
                HDfprintf(stdout, "%d:%s: new ver = %d <= old ver = %d.\n",
                          world_mpi_rank, fcn_name, 
                          new_ver_num, data[target_index].ver);
            }
        }
    }

    if ( success ) {

        data[target_index].ver = new_ver_num;
        data[target_index].valid = TRUE;
    }
 
    return(success);

} /* serve_write_request() */


/*****************************************************************************/
/**************************** Call back functions ****************************/
/*****************************************************************************/

/*-------------------------------------------------------------------------
 * Function:    clear_datum
 *
 * Purpose:     Mark the datum as clean and destroy it if requested.  
 *		Do not write it to the server, or increment the version.
 *
 * Return:      SUCCEED
 *
 * Programmer:  John Mainzer
 *              12/29/05
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t
clear_datum(H5F_t * f,
            void *  thing,
             hbool_t dest)
{
    int index;
    struct datum * entry_ptr;

    HDassert( thing );

    entry_ptr = (struct datum *)thing;

    index = addr_to_datum_index(entry_ptr->base_addr);

    HDassert( index >= 0 );
    HDassert( index < NUM_DATA_ENTRIES );
    HDassert( &(data[index]) == entry_ptr );

    HDassert( entry_ptr->header.addr == entry_ptr->base_addr );
    HDassert( entry_ptr->header.size == entry_ptr->len );

    entry_ptr->header.is_dirty = FALSE;
    entry_ptr->dirty = FALSE;

    if ( dest ) {

        destroy_datum(f, thing);

    }

    return(SUCCEED);

} /* clear_datum() */

/*-------------------------------------------------------------------------
 * Function:    destroy_datum()
 *
 * Purpose:     Destroy the entry.  At present, this means do nothing other
 *		than verify that the entry is clean.  In particular, do not 
 *		write it to the server process.
 *
 * Return:      SUCCEED
 *
 * Programmer:  John Mainzer
 *              12/29/05
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t
destroy_datum(H5F_t UNUSED * f,
              void *         thing)
{
    int index;
    struct datum * entry_ptr;

    HDassert( thing );

    entry_ptr = (struct datum *)thing;

    index = addr_to_datum_index(entry_ptr->base_addr);

    HDassert( index >= 0 );
    HDassert( index < NUM_DATA_ENTRIES );
    HDassert( &(data[index]) == entry_ptr );

    HDassert( entry_ptr->header.addr == entry_ptr->base_addr );
    HDassert( entry_ptr->header.size == entry_ptr->len );

    HDassert( !(entry_ptr->dirty) );
    HDassert( !(entry_ptr->header.is_dirty) );

    return(SUCCEED);

} /* destroy_datum() */

/*-------------------------------------------------------------------------
 * Function:    flush_datum
 *
 * Purpose:     Flush the entry to the server process and mark it as clean.  
 *		Then destroy the entry if requested.
 *
 * Return:      SUCCEED if successful, and FAIL otherwise.
 *
 * Programmer:  John Mainzer
 *              12/29/05
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t
flush_datum(H5F_t *f,
            hid_t UNUSED dxpl_id,
            hbool_t dest,
            haddr_t addr,
            void *thing)
{
    const char * fcn_name = "flush_datum()";
    herr_t ret_value = SUCCEED;
    int index;
    struct datum * entry_ptr;
    struct mssg_t mssg;

    HDassert( thing );

    entry_ptr = (struct datum *)thing;

    index = addr_to_datum_index(entry_ptr->base_addr);

    HDassert( index >= 0 );
    HDassert( index < NUM_DATA_ENTRIES );
    HDassert( &(data[index]) == entry_ptr );

    HDassert( entry_ptr->header.addr == entry_ptr->base_addr );
    HDassert( entry_ptr->header.size == entry_ptr->len );

    HDassert( entry_ptr->header.is_dirty == entry_ptr->dirty );

    if ( ( file_mpi_rank != 0 ) && ( entry_ptr->dirty ) ) {

        ret_value = FAIL;
        HDfprintf(stdout, 
                  "%d:%s: Flushed dirty entry from non-zero file process.",
                   world_mpi_rank, fcn_name);
    }

    if ( ret_value == SUCCEED ) {

        if ( entry_ptr->header.is_dirty ) {

            /* compose the message */
            mssg.req       = WRITE_REQ_CODE;
            mssg.src       = world_mpi_rank;
            mssg.dest      = world_server_mpi_rank;
            mssg.mssg_num  = -1; /* set by send function */
            mssg.base_addr = entry_ptr->base_addr;
            mssg.len       = entry_ptr->len;
            mssg.ver       = entry_ptr->ver;
            mssg.magic     = MSSG_MAGIC;

            if ( ! send_mssg(&mssg) ) {

                nerrors++;
                ret_value = FAIL;
                if ( verbose ) {
                    HDfprintf(stdout, "%d:%s: send_mssg() failed.\n",
                              world_mpi_rank, fcn_name);
                }
            }
            else
            {
                entry_ptr->header.is_dirty = FALSE;
                entry_ptr->dirty = FALSE;
            }
        }
    }

    if ( ret_value == SUCCEED ) {

        if ( dest ) {

            ret_value = destroy_datum(f, thing);
        }
    }

    return(ret_value);

} /* flush_datum() */

/*-------------------------------------------------------------------------
 * Function:    load_datum
 *
 * Purpose:     Read the requested entry from the server and mark it as 
 *              clean.
 *
 * Return:      SUCCEED if successful, FAIL otherwise.
 *
 * Programmer:  John Mainzer
 *              12/29/05
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

void *
load_datum(H5F_t UNUSED *f,
           hid_t UNUSED dxpl_id,
           haddr_t addr,
           const void UNUSED *udata1,
           void UNUSED *udata2)
{
    const char * fcn_name = "load_datum()";
    hbool_t success = TRUE;
    int index;
    struct datum * entry_ptr = NULL;
    struct mssg_t mssg;

    index = addr_to_datum_index(addr);

    HDassert( index >= 0 );
    HDassert( index < NUM_DATA_ENTRIES );

    entry_ptr = &(data[index]);

    HDassert( addr == entry_ptr->base_addr );

    /* compose the read message */
    mssg.req       = READ_REQ_CODE;
    mssg.src       = world_mpi_rank;
    mssg.dest      = world_server_mpi_rank;
    mssg.mssg_num  = -1; /* set by send function */
    mssg.base_addr = entry_ptr->base_addr;
    mssg.len       = entry_ptr->len;
    mssg.ver       = 0; /* bogus -- should be corrected by server */
    mssg.magic     = MSSG_MAGIC;

    if ( ! send_mssg(&mssg) ) {

        nerrors++;
        success = FALSE;
        if ( verbose ) {
            HDfprintf(stdout, "%d:%s: send_mssg() failed.\n",
                      world_mpi_rank, fcn_name);
        }
    }

    if ( success ) {

        if ( ! recv_mssg(&mssg) ) {

            nerrors++;
            success = FAIL;
            if ( verbose ) {
                HDfprintf(stdout, "%d:%s: recv_mssg() failed.\n",
                          world_mpi_rank, fcn_name);
            }
        }
    }

    if ( success ) {

        if ( ( mssg.req != READ_REQ_REPLY_CODE ) ||
             ( mssg.src != world_server_mpi_rank ) ||
             ( mssg.dest != world_mpi_rank ) ||
             ( mssg.base_addr != entry_ptr->base_addr ) ||
             ( mssg.len != entry_ptr->len ) ||
             ( mssg.ver < entry_ptr->ver ) ||
             ( mssg.magic != MSSG_MAGIC ) ) {

            nerrors++;
            success = FALSE;
            if ( verbose ) {
                HDfprintf(stdout, "%d:%s: Bad data in read req reply.\n",
                          world_mpi_rank, fcn_name);
            }
        } else {

            entry_ptr->ver = mssg.ver;
            entry_ptr->header.is_dirty = FALSE;
            entry_ptr->dirty = FALSE;
        }
    }

    if ( ! success ) {

        entry_ptr = NULL;

    }

    return(entry_ptr);

} /* load_datum() */

/*-------------------------------------------------------------------------
 * Function:    size_datum
 *
 * Purpose:     Get the size of the specified entry.  Just look at the 
 *		local copy, as size can't change.
 *
 * Return:      SUCCEED
 *
 * Programmer:  John Mainzer
 *              6/10/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t
size_datum(H5F_t UNUSED *  f,
           void *   thing,
           size_t * size_ptr)
{
    int index;
    struct datum * entry_ptr;

    HDassert( thing );
    HDassert( size_ptr );

    entry_ptr = (struct datum *)thing;

    index = addr_to_datum_index(entry_ptr->base_addr);

    HDassert( index >= 0 );
    HDassert( index < NUM_DATA_ENTRIES );
    HDassert( &(data[index]) == entry_ptr );

    HDassert( entry_ptr->header.addr == entry_ptr->base_addr );

    *size_ptr = entry_ptr->len;

    return(SUCCEED);

} /* size_datum() */


/*****************************************************************************/
/************************** test utility functions ***************************/
/*****************************************************************************/

/*****************************************************************************
 * Function:    insert_entry()
 *
 * Purpose:     Insert the entry indicated by the type and index, mark it
 *		as dirty, and increment its version number.
 *
 *		Do nothing if nerrors is non-zero on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              01/04/06
 *
 * Modifications:
 *
 *              None.
 *
 *****************************************************************************/

void
insert_entry(H5C_t * cache_ptr,
             H5F_t * file_ptr,
             int32_t idx,
             unsigned int flags)
{
    const char * fcn_name = "insert_entry()";
    herr_t result;
    struct datum * entry_ptr;

    HDassert( cache_ptr );
    HDassert( file_ptr );
    HDassert( ( 0 <= idx ) && ( idx < NUM_DATA_ENTRIES ) );

    entry_ptr = &(data[idx]);

    HDassert( !(entry_ptr->locked) );

    if ( nerrors == 0 ) {

        (entry_ptr->ver)++;
        entry_ptr->dirty = TRUE;

        result = H5AC_set(file_ptr, -1, &(types[0]), entry_ptr->base_addr, 
                          (void *)(&(entry_ptr->header)), flags);

        if ( ( result < 0 ) ||
             ( entry_ptr->header.type != &(types[0]) ) ||
             ( entry_ptr->len != entry_ptr->header.size ) ||
             ( entry_ptr->base_addr != entry_ptr->header.addr ) ) {

            nerrors++;
            if ( verbose ) {
	        HDfprintf(stdout, "%d:%s: Error in H5AC_set().\n",
	                  world_mpi_rank, fcn_name);
            }
        }

        if ( ! (entry_ptr->header.is_dirty) ) {

            nerrors++;
            if ( verbose ) {
	        HDfprintf(stdout, "%d:%s: data[%d].header.is_dirty = %d.\n",
	                  world_mpi_rank, fcn_name, idx, 
                          (int)(data[idx].header.is_dirty));
            }
        }

        /* HDassert( entry_ptr->header.is_dirty ); */
        HDassert( ((entry_ptr->header).type)->id == DATUM_ENTRY_TYPE );
    }

    return;

} /* insert_entry() */


/*****************************************************************************
 * Function:    lock_and_unlock_random_entries()
 *
 * Purpose:     Obtain a random number in the closed interval [min_count,
 *		max_count].  Then protect and unprotect that number of 
 *		random entries.
 *
 *              Do nothing if nerrors is non-zero on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              1/12/06
 *
 * Modifications:
 *
 *****************************************************************************/

void
lock_and_unlock_random_entries(H5C_t * cache_ptr,
                               H5F_t * file_ptr,
                               int min_idx,
                               int max_idx,
                               int min_count,
                               int max_count)
{
    const char * fcn_name = "lock_and_unlock_random_entries()";
    int count;
    int i;

    if ( nerrors == 0 ) {

        HDassert( cache_ptr );
        HDassert( file_ptr );
        HDassert( 0 <= min_count );
        HDassert( min_count < max_count );

        count = (HDrand() % (max_count - min_count)) + min_count;

        HDassert( min_count <= count );
        HDassert( count <= max_count );

        for ( i = 0; i < count; i++ )
        {
            lock_and_unlock_random_entry(cache_ptr, file_ptr, min_idx, max_idx);
        }
    }

    return;

} /* lock_and_unlock_random_entries() */


/*****************************************************************************
 * Function:    lock_and_unlock_random_entry()
 *
 * Purpose:     Protect and then unprotect a random entry with index in
 *		the data[] array in the close interval [min_idx, max_idx].
 *
 *              Do nothing if nerrors is non-zero on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              1/4/06
 *
 * Modifications:
 *
 *****************************************************************************/

void
lock_and_unlock_random_entry(H5C_t * cache_ptr,
                             H5F_t * file_ptr,
                             int min_idx,
                             int max_idx)
{
    const char * fcn_name = "lock_and_unlock_random_entry()";
    int index;

    if ( nerrors == 0 ) {

        HDassert( cache_ptr );
        HDassert( file_ptr );
        HDassert( 0 <= min_idx );
        HDassert( min_idx < max_idx );
        HDassert( max_idx < NUM_DATA_ENTRIES );

        index = (HDrand() % (max_idx - min_idx)) + min_idx;

        HDassert( min_idx <= index );
        HDassert( index <= max_idx );

	lock_entry(cache_ptr, file_ptr, index);
	unlock_entry(cache_ptr, file_ptr, index, H5AC__NO_FLAGS_SET);
    }

    return;

} /* lock_and_unlock_random_entry() */


/*****************************************************************************
 * Function:    lock_entry()
 *
 * Purpose:     Protect the entry indicated by the index.
 *
 *              Do nothing if nerrors is non-zero on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              1/4/06
 *
 * Modifications:
 *
 *****************************************************************************/

void
lock_entry(H5C_t * cache_ptr,
           H5F_t * file_ptr,
           int32_t idx)
{
    const char * fcn_name = "lock_entry()";
    struct datum * entry_ptr;
    H5C_cache_entry_t * cache_entry_ptr;

    if ( nerrors == 0 ) {

        HDassert( cache_ptr );
        HDassert( ( 0 <= idx ) && ( idx < NUM_DATA_ENTRIES ) );

        entry_ptr = &(data[idx]);

        cache_entry_ptr = H5AC_protect(file_ptr, -1, &(types[0]),
                                       entry_ptr->base_addr, 
                                       NULL, NULL, H5AC_WRITE);

        if ( ( cache_entry_ptr != (void *)(&(entry_ptr->header)) ) ||
             ( entry_ptr->header.type != &(types[0]) ) ||
             ( entry_ptr->len != entry_ptr->header.size ) ||
             ( entry_ptr->base_addr != entry_ptr->header.addr ) ) {

            nerrors++;
            if ( verbose ) {
		HDfprintf(stdout, "%d:%s: error in H5AC_protect().\n",
	                  world_mpi_rank, fcn_name);
            }
        } 

        HDassert( ((entry_ptr->header).type)->id == DATUM_ENTRY_TYPE );
    }

    return;

} /* lock_entry() */


/*****************************************************************************
 * Function:    rename_entry()
 *
 * Purpose:     Rename the entry indicated old_idx to the entry indicated
 *		by new_idex.  Touch up the data array so that flush will
 *		not choke.
 *
 *		Do nothing if nerrors isn't zero, or if old_idx equals
 *		new_idx.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              1/10/06
 *
 * Modifications:
 *
 *              None.
 *
 *****************************************************************************/

void
rename_entry(H5C_t * cache_ptr,
             H5F_t * file_ptr,
             int32_t old_idx,
             int32_t new_idx)
{
    const char   * fcn_name = "rename_entry()";
    herr_t         result;
    int		   tmp;
    hbool_t        done = TRUE; /* will set to FALSE if we have work to do */
    haddr_t        old_addr = HADDR_UNDEF;
    haddr_t        new_addr = HADDR_UNDEF;
    struct datum * old_entry_ptr;
    struct datum * new_entry_ptr;

    if ( ( nerrors == 0 ) && ( old_idx != new_idx ) ) {

        HDassert( cache_ptr );
        HDassert( file_ptr );
        HDassert( ( 0 <= old_idx ) && ( old_idx < NUM_DATA_ENTRIES ) );
        HDassert( ( 0 <= new_idx ) && ( new_idx < NUM_DATA_ENTRIES ) );

        old_entry_ptr = &(data[old_idx]);
        new_entry_ptr = &(data[new_idx]);

        HDassert( ((old_entry_ptr->header).type)->id == DATUM_ENTRY_TYPE );
        HDassert( !(old_entry_ptr->header.is_protected) );
        HDassert( !(old_entry_ptr->locked) );
        HDassert( old_entry_ptr->len == new_entry_ptr->len );

        old_addr = old_entry_ptr->base_addr;
        new_addr = new_entry_ptr->base_addr;

        result = H5AC_rename(file_ptr,  &(types[0]), old_addr, new_addr);

        if ( ( result < 0 ) || ( old_entry_ptr->header.addr != new_addr ) ) {

            nerrors++;
            if ( verbose ) {
		HDfprintf(stdout, "%d:%s: H5AC_rename() failed.\n",
	                  world_mpi_rank, fcn_name);
            }

        } else {

            HDassert( ((old_entry_ptr->header).type)->id == DATUM_ENTRY_TYPE );
            HDassert( old_entry_ptr->header.is_dirty );
            old_entry_ptr->dirty = TRUE;

            /* touch up versions, base_addrs, and data_index */

            if ( old_entry_ptr->ver < new_entry_ptr->ver ) {

		old_entry_ptr->ver = new_entry_ptr->ver;

            } else {

                (old_entry_ptr->ver)++;

            }

            old_entry_ptr->base_addr = new_addr;
            new_entry_ptr->base_addr = old_addr;

            data_index[old_entry_ptr->index] = new_idx;
            data_index[new_entry_ptr->index] = old_idx;

            tmp                  = old_entry_ptr->index;
            old_entry_ptr->index = new_entry_ptr->index;
            new_entry_ptr->index = tmp;
        }
    }

    return;

} /* rename_entry() */


/*****************************************************************************
 *
 * Function:	setup_cache_for_test()
 *
 * Purpose:	Setup the parallel cache for a test, and return the file id
 *		and a pointer to the cache's internal data structures.
 *
 *		To do this, we must create a file, flush it (so that we 
 *		don't have to worry about entries in the metadata cache),
 *		look up the address of the metadata cache, and then instruct 
 *		the cache to omit sanity checks on dxpl IDs.
 *
 * Return:	Success:	TRUE
 *
 *		Failure:	FALSE
 *
 * Programmer:	JRM -- 1/4/06
 *
 * Modifications:
 *
 *		None.
 *	
 *****************************************************************************/

hbool_t
setup_cache_for_test(hid_t * fid_ptr,
                     H5F_t ** file_ptr_ptr,
                     H5C_t ** cache_ptr_ptr)
{
    const char * fcn_name = "setup_cache_for_test()";
    hbool_t success = FALSE; /* will set to TRUE if appropriate. */
    hbool_t enable_rpt_fcn = FALSE;
    hid_t fid = -1;
    H5AC_cache_config_t config;
    H5F_t * file_ptr = NULL;
    H5C_t * cache_ptr = NULL;

    HDassert ( fid_ptr != NULL );
    HDassert ( file_ptr_ptr != NULL );
    HDassert ( cache_ptr_ptr != NULL );

    fid = H5Fcreate(filenames[0], H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

    if ( fid < 0 ) {
        nerrors++;
        if ( verbose ) {
	    HDfprintf(stdout, "%d:%s: H5Fcreate() failed.\n", 
                      world_mpi_rank, fcn_name);
        }
    } else if ( H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0 ) {
        nerrors++;
        if ( verbose ) {
	    HDfprintf(stdout, "%d:%s: H5Fflush() failed.\n", 
                      world_mpi_rank, fcn_name);
        }
    } else {
        file_ptr = H5I_object_verify(fid, H5I_FILE);
    }

    if ( file_ptr == NULL ) {
        nerrors++;
        if ( verbose ) {
	    HDfprintf(stdout, "%d:%s: Can't get file_ptr.\n", 
                      world_mpi_rank, fcn_name);
        }
    } else {
        cache_ptr = file_ptr->shared->cache;
    }

    if ( cache_ptr == NULL ) {
        nerrors++;
        if ( verbose ) {
	    HDfprintf(stdout, "%d:%s: Can't get cache_ptr.\n", 
                      world_mpi_rank, fcn_name);
        }
    } else if ( cache_ptr->magic != H5C__H5C_T_MAGIC ) {
        nerrors++;
        if ( verbose ) {
	    HDfprintf(stdout, "%d:%s: Bad cache_ptr magic.\n", 
                      world_mpi_rank, fcn_name);
        }
    } else {
        *fid_ptr = fid;
        *file_ptr_ptr = file_ptr;
        *cache_ptr_ptr = cache_ptr;
        H5C_set_skip_flags(cache_ptr, TRUE, TRUE);
        H5C_stats__reset(cache_ptr);
        success = TRUE;
    }

    if ( ( success ) && ( enable_rpt_fcn ) ) {

        config.version = H5AC__CURR_CACHE_CONFIG_VERSION;

        if ( H5AC_get_cache_auto_resize_config(cache_ptr, &config) 
             != SUCCEED ) {

	    HDfprintf(stdout, 
                      "%d:%s: H5AC_get_cache_auto_resize_config() failed.\n", 
                      world_mpi_rank, fcn_name);

        } else {

            config.rpt_fcn_enabled = TRUE;

            if ( H5AC_set_cache_auto_resize_config(cache_ptr, &config) 
                 != SUCCEED ) {

	        HDfprintf(stdout, 
                         "%d:%s: H5AC_set_cache_auto_resize_config() failed.\n",
                          world_mpi_rank, fcn_name);
            } else {

                HDfprintf(stdout, "%d:%s: rpt_fcn enabled.\n",
                          world_mpi_rank, fcn_name);
            }
        }
    }

    return(success);

} /* setup_cache_for_test() */


/*****************************************************************************
 *
 * Function:	setup_noblock_dxpl_id()
 *
 * Purpose:	Setup the noblock_dxpl_id global.  Increment nerrors if 
 *		errors are detected.  Do nothing if nerrors is non-zero
 *		on entry.
 *
 * Return:	void.
 *
 * Programmer:	JRM -- 1/5/06
 *
 * Modifications:
 *
 *		None.
 *	
 *****************************************************************************/
/* So far we haven't needed this, but that may change.  
 * Keep it around for now 
 */
#if 0
void
setup_noblock_dxpl_id(void)
{
    const char * fcn_name = "setup_noblock_dxpl_id()";
    H5P_genclass_t  *xfer_pclass;   /* Dataset transfer property list 
                                     * class object 
                                     */
    H5P_genplist_t  *xfer_plist;    /* Dataset transfer property list object */
    unsigned block_before_meta_write; /* "block before meta write" 
                                       * property value 
                                       */
    unsigned library_internal = 1;  /* "library internal" property value */
    H5FD_mpio_xfer_t xfer_mode;     /* I/O transfer mode property value */

    /* Sanity check */
    HDassert(H5P_CLS_DATASET_XFER_g!=(-1));

    /* Get the dataset transfer property list class object */
    if ( ( nerrors == 0 ) &&
         ( NULL == (xfer_pclass = H5I_object(H5P_CLS_DATASET_XFER_g)) ) ) {

        nerrors++;
        if ( verbose ) {
            HDfprintf(stdout, "%d:%s: can't get property list class.\n",
                      world_mpi_rank, fcn_name);
        }
    }

    /* Get an ID for the non-blocking, collective H5AC dxpl */
    if ( ( nerrors == 0 ) &&
         ( (noblock_dxpl_id = H5P_create_id(xfer_pclass)) < 0 ) ) {

        nerrors++;
        if ( verbose ) {
            HDfprintf(stdout, "%d:%s: can't register property list.\n",
                      world_mpi_rank, fcn_name);
        }
    }

    /* Get the property list object */
    if ( ( nerrors == 0 ) &&
         ( NULL == (xfer_plist = H5I_object(H5AC_noblock_dxpl_id)) ) ) {

        nerrors++;
        if ( verbose ) {
            HDfprintf(stdout, "%d:%s: can't get new property list object.\n",
                      world_mpi_rank, fcn_name);
        }
    }

    /* Insert 'block before metadata write' property */
    block_before_meta_write=0;
    if ( ( nerrors == 0 ) &&
         ( H5P_insert(xfer_plist, H5AC_BLOCK_BEFORE_META_WRITE_NAME,
                      H5AC_BLOCK_BEFORE_META_WRITE_SIZE, 
                      &block_before_meta_write,
                      NULL, NULL, NULL, NULL, NULL, NULL) < 0 ) ) {

        nerrors++;
        if ( verbose ) {
            HDfprintf(stdout, 
                      "%d:%s: can't insert metadata cache dxpl property 1.\n",
                      world_mpi_rank, fcn_name);
        }
    }

    /* Insert 'library internal' property */
    if ( ( nerrors == 0 ) &&
         ( H5P_insert(xfer_plist, H5AC_LIBRARY_INTERNAL_NAME,
                      H5AC_LIBRARY_INTERNAL_SIZE, &library_internal,
                      NULL, NULL, NULL, NULL, NULL, NULL ) < 0 ) ) {

        nerrors++;
        if ( verbose ) {
            HDfprintf(stdout, 
                      "%d:%s: can't insert metadata cache dxpl property 2.\n",
                      world_mpi_rank, fcn_name);
        }
    }

    /* Set the transfer mode */
    xfer_mode = H5FD_MPIO_COLLECTIVE;
    if ( ( nerrors == 0 ) &&
         ( H5P_set(xfer_plist, H5D_XFER_IO_XFER_MODE_NAME, &xfer_mode) < 0 ) ) {

        nerrors++;
        if ( verbose ) {
            HDfprintf(stdout, "%d:%s: unable to set value.\n", world_mpi_rank, 
                      fcn_name);
        }
    }

    return(success);

} /* setup_noblock_dxpl_id() */
#endif


/*****************************************************************************
 *
 * Function:	setup_rand()
 *
 * Purpose:	Use gettimeofday() to obtain a seed for rand(), print the 
 *		seed to stdout, and then pass it to srand().
 *
 *		Increment nerrors if any errors are detected.
 *
 * Return:	void.
 *
 * Programmer:	JRM -- 1/12/06
 *
 * Modifications:
 *
 *		None.
 *	
 *****************************************************************************/

void
setup_rand(void)
{
    const char * fcn_name = "setup_rand()";
    unsigned seed;
    struct timeval tv;
    struct timezone tz;

    if ( HDgettimeofday(&tv, &tz) != 0 ) {

	nerrors++;
        if ( verbose ) {
	    HDfprintf(stdout, "%d:%s: gettimeofday() failed.\n",
		      world_mpi_rank, fcn_name);
        }
    } else {
	seed = (unsigned)tv.tv_usec;
	HDfprintf(stdout, "%d:%s: seed = %d.\n", 
                  world_mpi_rank, fcn_name, seed);
        HDsrand(seed);
    }

    return;

} /* setup_rand() */


/*****************************************************************************
 *
 * Function:	take_down_cache()
 *
 * Purpose:	Take down the parallel cache after a test.
 *
 *		To do this, we must close the file, and delete if if 
 *		possible.
 *
 * Return:	Success:	TRUE
 *
 *		Failure:	FALSE
 *
 * Programmer:	JRM -- 1/4/06
 *
 * Modifications:
 *
 *		None.
 *	
 *****************************************************************************/

hbool_t
take_down_cache(hid_t fid)
{
    const char * fcn_name = "take_down_cache()";
    hbool_t success = FALSE; /* will set to TRUE if appropriate. */

    /* close the file and delete it */
    if ( H5Fclose(fid) < 0  ) {

        nerrors++;
        if ( verbose ) {
            HDfprintf(stdout, "%d:%s: H5Fclose() failed.\n", 
                      world_mpi_rank, fcn_name);
        }

    } else if ( world_mpi_rank == world_server_mpi_rank ) {

        if ( HDremove(filenames[0]) < 0 ) {

            nerrors++;
            if ( verbose ) {
                HDfprintf(stdout, "%d:%s: HDremove() failed.\n", 
                          world_mpi_rank, fcn_name);
            }
        } else {

	    success = TRUE;
        }
    } else {

        success = TRUE;
    }

    return(success);

} /* take_down_cache() */


/*****************************************************************************
 * Function:    unlock_entry()
 *
 * Purpose:     Unprotect the entry indicated by the index.
 *
 *              Do nothing if nerrors is non-zero on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              1/4/06
 *
 * Modifications:
 *
 *****************************************************************************/

void
unlock_entry(H5C_t * cache_ptr,
             H5F_t * file_ptr,
             int32_t idx,
             unsigned int flags)
{
    const char * fcn_name = "unlock_entry()";
    herr_t dirtied;
    herr_t result;
    struct datum * entry_ptr;

    if ( nerrors == 0 ) {

        HDassert( cache_ptr );
        HDassert( file_ptr );
        HDassert( ( 0 <= idx ) && ( idx < NUM_DATA_ENTRIES ) );

        entry_ptr = &(data[idx]);

        dirtied = ((flags & H5AC__DIRTIED_FLAG) == H5AC__DIRTIED_FLAG );

        if ( dirtied ) {

            (entry_ptr->ver)++;
            entry_ptr->dirty = TRUE;
        }

        result = H5AC_unprotect(file_ptr, -1, &(types[0]), entry_ptr->base_addr,
                                (void *)(&(entry_ptr->header)), flags);

        if ( ( result < 0 ) ||
             ( entry_ptr->header.type != &(types[0]) ) ||
             ( entry_ptr->len != entry_ptr->header.size ) ||
             ( entry_ptr->base_addr != entry_ptr->header.addr ) ) {

            nerrors++;
            if ( verbose ) {
                HDfprintf(stdout, "%d:%s: error in H5C_unprotect().\n",
                          world_mpi_rank, fcn_name);
            }
        }

        HDassert( ((entry_ptr->header).type)->id == DATUM_ENTRY_TYPE );

        if ( ( flags & H5AC__DIRTIED_FLAG ) != 0
                && ( (flags & H5C__DELETED_FLAG) == 0 ) ) {

            HDassert( entry_ptr->header.is_dirty );
            HDassert( entry_ptr->dirty );
        }
    }

    return;

} /* unlock_entry() */


/*****************************************************************************/
/****************************** test functions *******************************/
/*****************************************************************************/

/*****************************************************************************
 *
 * Function:	server_smoke_check()
 *
 * Purpose:	Quick smoke check for the server process.
 *
 * Return:	Success:	TRUE
 *
 *		Failure:	FALSE
 *
 * Programmer:	JRM -- 12/21/05
 *
 * Modifications:
 *
 *		None.
 *	
 *****************************************************************************/

hbool_t
server_smoke_check(void)
{
    const char * fcn_name = "server_smoke_check()";
    hbool_t success = TRUE;
    int max_nerrors;
    struct mssg_t mssg;

    if ( world_mpi_rank == 0 ) {

        TESTING("server smoke check");
    }

    nerrors = 0;
    init_data();

    if ( world_mpi_rank == world_server_mpi_rank ) {

	if ( ! server_main() ) {

            /* some error occured in the server -- report failure */
            nerrors++;
            if ( verbose ) {
		HDfprintf(stdout, "%d:%s: server_main() failed.\n",
                          world_mpi_rank, fcn_name);
            }
        }
    }
    else /* run the clients */
    {
        /* compose the write message */
        mssg.req       = WRITE_REQ_CODE;
        mssg.src       = world_mpi_rank;
        mssg.dest      = world_server_mpi_rank;
        mssg.mssg_num  = -1; /* set by send function */
        mssg.base_addr = data[world_mpi_rank].base_addr;
        mssg.len       = data[world_mpi_rank].len;
        mssg.ver       = ++(data[world_mpi_rank].ver);
        mssg.magic     = MSSG_MAGIC;

        if ( ! ( success = send_mssg(&mssg) ) ) {

            nerrors++;
            if ( verbose ) {
                HDfprintf(stdout, "%d:%s: send_mssg() failed on write.\n", 
                          world_mpi_rank, fcn_name);
            }
        }

        /* compose the read message */
        mssg.req       = READ_REQ_CODE;
        mssg.src       = world_mpi_rank;
        mssg.dest      = world_server_mpi_rank;
        mssg.mssg_num  = -1; /* set by send function */
        mssg.base_addr = data[world_mpi_rank].base_addr;
        mssg.len       = data[world_mpi_rank].len;
        mssg.ver       = 0; /* bogus -- should be corrected by server */
        mssg.magic     = MSSG_MAGIC;

        if ( success ) {

            success = send_mssg(&mssg);

            if ( ! success ) {

                nerrors++;
                if ( verbose ) {
                    HDfprintf(stdout, "%d:%s: send_mssg() failed on write.\n", 
                              world_mpi_rank, fcn_name);
                }
            }
        }

        /* try to receive the reply from the server */
        if ( success ) {

            success = recv_mssg(&mssg);

            if ( ! success ) {

                nerrors++;
                if ( verbose ) {
                    HDfprintf(stdout, "%d:%s: recv_mssg() failed.\n", 
                              world_mpi_rank, fcn_name);
                }
            }
        }

        /* verify that we got the expected result */
        if ( success ) {

            if ( ( mssg.req != READ_REQ_REPLY_CODE ) ||
                 ( mssg.src != world_server_mpi_rank ) ||
                 ( mssg.dest != world_mpi_rank ) ||
                 ( mssg.base_addr != data[world_mpi_rank].base_addr ) ||
                 ( mssg.len != data[world_mpi_rank].len ) ||
                 ( mssg.ver != data[world_mpi_rank].ver ) ||
                 ( mssg.magic != MSSG_MAGIC ) ) {

                success = FALSE;
                nerrors++;
                if ( verbose ) {
                    HDfprintf(stdout, "%d:%s: Bad data in read req reply.\n",
                              world_mpi_rank, fcn_name);
                }
            }
        }

        /* compose the done message */
        mssg.req       = DONE_REQ_CODE;
        mssg.src       = world_mpi_rank;
        mssg.dest      = world_server_mpi_rank;
        mssg.mssg_num  = -1; /* set by send function */
        mssg.base_addr = 0; /* not used */
        mssg.len       = 0; /* not used */
        mssg.ver       = 0; /* not used */
        mssg.magic     = MSSG_MAGIC;

        if ( success ) {

            success = send_mssg(&mssg);

            if ( ! success ) {

                nerrors++;
                if ( verbose ) {
                    HDfprintf(stdout, "%d:%s: send_mssg() failed on done.\n", 
                              world_mpi_rank, fcn_name);
                }
            }
        }
    }
    
    max_nerrors = get_max_nerrors();

    if ( world_mpi_rank == 0 ) {

	if ( max_nerrors == 0 ) {

	    PASSED();

        } else {

            failures++;
            H5_FAILED();
        }
    }

    success = ( ( success ) && ( max_nerrors == 0 ) );

    return(success);

} /* server_smoke_check() */

/*****************************************************************************
 *
 * Function:	smoke_check_1()
 *
 * Purpose:	First smoke check for the parallel cache.
 *
 * Return:	Success:	TRUE
 *
 *		Failure:	FALSE
 *
 * Programmer:	JRM -- 1/4/06
 *
 * Modifications:
 *
 *		None.
 *	
 *****************************************************************************/

hbool_t
smoke_check_1(void)
{
    const char * fcn_name = "smoke_check_1()";
    hbool_t success = TRUE;
    int i;
    int max_nerrors;
    hid_t fid = -1;
    H5F_t * file_ptr = NULL;
    H5C_t * cache_ptr = NULL;
    struct mssg_t mssg;

    if ( world_mpi_rank == 0 ) {

        TESTING("smoke check #1");
    }

    nerrors = 0;
    init_data();

    if ( world_mpi_rank == world_server_mpi_rank ) {

	if ( ! server_main() ) {

            /* some error occured in the server -- report failure */
            nerrors++;
            if ( verbose ) {
		HDfprintf(stdout, "%d:%s: server_main() failed.\n",
                          world_mpi_rank, fcn_name);
            }
        }
    }
    else /* run the clients */
    {
        if ( ! setup_cache_for_test(&fid, &file_ptr, &cache_ptr) ) {

            nerrors++;
            fid = -1;
            cache_ptr = NULL;
            if ( verbose ) {
		HDfprintf(stdout, "%d:%s: setup_cache_for_test() failed.\n",
                          world_mpi_rank, fcn_name);
            }
        }

        for ( i = 0; i < (NUM_DATA_ENTRIES / 2); i++ )
        {
            insert_entry(cache_ptr, file_ptr, i, H5AC__NO_FLAGS_SET);
        }

        for ( i = (NUM_DATA_ENTRIES / 2) - 1; i >= 0; i-- )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC__NO_FLAGS_SET);
        }

        /* rename the first half of the entries... */
        for ( i = 0; i < (NUM_DATA_ENTRIES / 2); i++ )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC__NO_FLAGS_SET);
	    rename_entry(cache_ptr, file_ptr, i, (i + (NUM_DATA_ENTRIES / 2)));
        }

        /* ...and then rename them back. */
        for ( i = (NUM_DATA_ENTRIES / 2) - 1; i >= 0; i-- )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC__NO_FLAGS_SET);
	    rename_entry(cache_ptr, file_ptr, i, (i + (NUM_DATA_ENTRIES / 2)));
        }

        if ( fid >= 0 ) {

            if ( ! take_down_cache(fid) ) {

                nerrors++;
                if ( verbose ) {
		    HDfprintf(stdout, "%d:%s: take_down_cache() failed.\n",
                              world_mpi_rank, fcn_name);
                }
            }
        }

        /* verify that all instance of datum are back where the started 
         * and are clean.
         */

        for ( i = 0; i < NUM_DATA_ENTRIES; i++ )
        {
            HDassert( data_index[i] == i );
            HDassert( ! (data[i].dirty) );
        }

        /* compose the done message */
        mssg.req       = DONE_REQ_CODE;
        mssg.src       = world_mpi_rank;
        mssg.dest      = world_server_mpi_rank;
        mssg.mssg_num  = -1; /* set by send function */
        mssg.base_addr = 0; /* not used */
        mssg.len       = 0; /* not used */
        mssg.ver       = 0; /* not used */
        mssg.magic     = MSSG_MAGIC;

        if ( success ) {


            success = send_mssg(&mssg);

            if ( ! success ) {

                nerrors++;
                if ( verbose ) {
                    HDfprintf(stdout, "%d:%s: send_mssg() failed on done.\n", 
                              world_mpi_rank, fcn_name);
                }
            }
        }
    }
    
    max_nerrors = get_max_nerrors();

    if ( world_mpi_rank == 0 ) {

	if ( max_nerrors == 0 ) {

	    PASSED();

        } else {

            failures++;
            H5_FAILED();
        }
    }

    success = ( ( success ) && ( max_nerrors == 0 ) );

    return(success);

} /* smoke_check_1() */


/*****************************************************************************
 *
 * Function:	smoke_check_2()
 *
 * Purpose:	Second smoke check for the parallel cache.
 *
 *		Introduce random reads, but keep all processes with roughly
 *		the same work load.
 *
 * Return:	Success:	TRUE
 *
 *		Failure:	FALSE
 *
 * Programmer:	JRM -- 1/12/06
 *
 * Modifications:
 *
 *		None.
 *	
 *****************************************************************************/

hbool_t
smoke_check_2(void)
{
    const char * fcn_name = "smoke_check_2()";
    hbool_t success = TRUE;
    int i;
    int max_nerrors;
    hid_t fid = -1;
    H5F_t * file_ptr = NULL;
    H5C_t * cache_ptr = NULL;
    struct mssg_t mssg;

    if ( world_mpi_rank == 0 ) {

        TESTING("smoke check #2");
    }

    nerrors = 0;
    init_data();

    if ( world_mpi_rank == world_server_mpi_rank ) {

	if ( ! server_main() ) {

            /* some error occured in the server -- report failure */
            nerrors++;
            if ( verbose ) {
		HDfprintf(stdout, "%d:%s: server_main() failed.\n",
                          world_mpi_rank, fcn_name);
            }
        }
    }
    else /* run the clients */
    {
        if ( ! setup_cache_for_test(&fid, &file_ptr, &cache_ptr) ) {

            nerrors++;
            fid = -1;
            cache_ptr = NULL;
            if ( verbose ) {
		HDfprintf(stdout, "%d:%s: setup_cache_for_test() failed.\n",
                          world_mpi_rank, fcn_name);
            }
        }

        for ( i = 0; i < (NUM_DATA_ENTRIES / 2); i++ )
        {
            insert_entry(cache_ptr, file_ptr, i, H5AC__NO_FLAGS_SET);

            if ( i > 100 ) {

		lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                               (i - 100), i, 0, 10);
            }
        }

        for ( i = (NUM_DATA_ENTRIES / 2) - 1; i >= 0; i-=2 )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC__NO_FLAGS_SET);
	    lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                           0, (NUM_DATA_ENTRIES / 20), 0, 100);
        }

        for ( i = 0; i < (NUM_DATA_ENTRIES / 2); i+=2 )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC__DIRTIED_FLAG);
	    lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                           0, (NUM_DATA_ENTRIES / 10), 0, 100);
        }

        /* rename the first half of the entries... */
        for ( i = 0; i < (NUM_DATA_ENTRIES / 2); i++ )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC__NO_FLAGS_SET);
	    rename_entry(cache_ptr, file_ptr, i, (i + (NUM_DATA_ENTRIES / 2)));
	    lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                           0, ((NUM_DATA_ENTRIES / 50) - 1), 
                                           0, 100);
        }

        /* ...and then rename them back. */
        for ( i = (NUM_DATA_ENTRIES / 2) - 1; i >= 0; i-- )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC__DIRTIED_FLAG);
	    rename_entry(cache_ptr, file_ptr, i, (i + (NUM_DATA_ENTRIES / 2)));
	    lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                           0, (NUM_DATA_ENTRIES / 100), 0, 100);
        }

        if ( fid >= 0 ) {

            if ( ! take_down_cache(fid) ) {

                nerrors++;
                if ( verbose ) {
		    HDfprintf(stdout, "%d:%s: take_down_cache() failed.\n",
                              world_mpi_rank, fcn_name);
                }
            }
        }

        /* verify that all instance of datum are back where the started 
         * and are clean.
         */

        for ( i = 0; i < NUM_DATA_ENTRIES; i++ )
        {
            HDassert( data_index[i] == i );
            HDassert( ! (data[i].dirty) );
        }

        /* compose the done message */
        mssg.req       = DONE_REQ_CODE;
        mssg.src       = world_mpi_rank;
        mssg.dest      = world_server_mpi_rank;
        mssg.mssg_num  = -1; /* set by send function */
        mssg.base_addr = 0; /* not used */
        mssg.len       = 0; /* not used */
        mssg.ver       = 0; /* not used */
        mssg.magic     = MSSG_MAGIC;

        if ( success ) {


            success = send_mssg(&mssg);

            if ( ! success ) {

                nerrors++;
                if ( verbose ) {
                    HDfprintf(stdout, "%d:%s: send_mssg() failed on done.\n", 
                              world_mpi_rank, fcn_name);
                }
            }
        }
    }
    
    max_nerrors = get_max_nerrors();

    if ( world_mpi_rank == 0 ) {

	if ( max_nerrors == 0 ) {

	    PASSED();

        } else {

            failures++;
            H5_FAILED();
        }
    }

    success = ( ( success ) && ( max_nerrors == 0 ) );

    return(success);

} /* smoke_check_2() */


/*****************************************************************************
 *
 * Function:	smoke_check_3()
 *
 * Purpose:	Third smoke check for the parallel cache.
 *
 *		Use random reads to vary the loads on the diffferent 
 *		processors.  Also force different cache size adjustments.
 *
 *		In this test, load process 0 heavily, and the other
 *		processes lightly.
 *
 * Return:	Success:	TRUE
 *
 *		Failure:	FALSE
 *
 * Programmer:	JRM -- 1/13/06
 *
 * Modifications:
 *
 *		Added code intended to ensure correct operation with large
 *		numbers of processors.
 *							JRM - 1/31/06
 *	
 *****************************************************************************/

hbool_t
smoke_check_3(void)
{
    const char * fcn_name = "smoke_check_3()";
    hbool_t success = TRUE;
    int i;
    int max_nerrors;
    int min_count;
    int max_count;
    int min_idx;
    int max_idx;
    hid_t fid = -1;
    H5F_t * file_ptr = NULL;
    H5C_t * cache_ptr = NULL;
    struct mssg_t mssg;

    if ( world_mpi_rank == 0 ) {

        TESTING("smoke check #3");
    }

    nerrors = 0;
    init_data();

    if ( world_mpi_rank == world_server_mpi_rank ) {

	if ( ! server_main() ) {

            /* some error occured in the server -- report failure */
            nerrors++;
            if ( verbose ) {
		HDfprintf(stdout, "%d:%s: server_main() failed.\n",
                          world_mpi_rank, fcn_name);
            }
        }
    }
    else /* run the clients */
    {
        if ( ! setup_cache_for_test(&fid, &file_ptr, &cache_ptr) ) {

            nerrors++;
            fid = -1;
            cache_ptr = NULL;
            if ( verbose ) {
		HDfprintf(stdout, "%d:%s: setup_cache_for_test() failed.\n",
                          world_mpi_rank, fcn_name);
            }
        }

        
        min_count = 100 / ((file_mpi_rank + 1) * (file_mpi_rank + 1));
        max_count = min_count + 50;

        for ( i = 0; i < (NUM_DATA_ENTRIES / 4); i++ )
        {
            insert_entry(cache_ptr, file_ptr, i, H5AC__NO_FLAGS_SET);

            if ( i > 100 ) {

		lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                               (i - 100), i, 
                                               min_count, max_count);
            }
        }

        min_count = 100 / ((file_mpi_rank + 2) * (file_mpi_rank + 2));
        max_count = min_count + 50;

        for ( i = (NUM_DATA_ENTRIES / 4); i < (NUM_DATA_ENTRIES / 2); i++ )
        {
            insert_entry(cache_ptr, file_ptr, i, H5AC__NO_FLAGS_SET);

            if ( i > 100 ) {

		lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                               (i - 100), i,
                                               min_count, max_count);
            }
        }

        min_idx = 0;
        max_idx = ((NUM_DATA_ENTRIES / 10) / 
                   ((file_mpi_rank + 1) * (file_mpi_rank + 1))) - 1;
        if ( max_idx <= min_idx ) {

            max_idx = min_idx + 10;
        }

        for ( i = (NUM_DATA_ENTRIES / 2) - 1; i >= 0; i-=2 )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC__NO_FLAGS_SET);
	    lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                           min_idx, max_idx, 0, 100);
        }

        min_idx = 0;
        max_idx = ((NUM_DATA_ENTRIES / 10) / 
                   ((file_mpi_rank + 3) * (file_mpi_rank + 3))) - 1;
        if ( max_idx <= min_idx ) {

            max_idx = min_idx + 10;
        }

        for ( i = 0; i < (NUM_DATA_ENTRIES / 2); i+=2 )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC__DIRTIED_FLAG);
	    lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                           min_idx, max_idx, 0, 100);
        }

        min_count = 10 / (file_mpi_rank + 1);
        max_count = min_count + 100;

        /* rename the first half of the entries... */
        for ( i = 0; i < (NUM_DATA_ENTRIES / 2); i++ )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC__NO_FLAGS_SET);
	    rename_entry(cache_ptr, file_ptr, i, (i + (NUM_DATA_ENTRIES / 2)));
	    lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                           0, (NUM_DATA_ENTRIES / 20), 
                                           min_count, max_count);
        }

        /* ...and then rename them back. */
        for ( i = (NUM_DATA_ENTRIES / 2) - 1; i >= 0; i-- )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC__DIRTIED_FLAG);
	    rename_entry(cache_ptr, file_ptr, i, (i + (NUM_DATA_ENTRIES / 2)));
	    lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                           0, (NUM_DATA_ENTRIES / 40),
                                           min_count, max_count);
        }

        /* finally, do some dirty lock/unlocks while we give the cache
         * a chance t reduce its size.
         */
        min_count = 200 / ((file_mpi_rank + 1) * (file_mpi_rank + 1));
        max_count = min_count + 100;

        for ( i = 0; i < (NUM_DATA_ENTRIES / 2); i+=2 )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC__DIRTIED_FLAG);

            if ( i > 100 ) {

		lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                               (i - 100), i, 
                                               min_count, max_count);
            }
        }

        if ( fid >= 0 ) {

            if ( ! take_down_cache(fid) ) {

                nerrors++;
                if ( verbose ) {
		    HDfprintf(stdout, "%d:%s: take_down_cache() failed.\n",
                              world_mpi_rank, fcn_name);
                }
            }
        }

        /* verify that all instances of datum are back where the started 
         * and are clean.
         */

        for ( i = 0; i < NUM_DATA_ENTRIES; i++ )
        {
            HDassert( data_index[i] == i );
            HDassert( ! (data[i].dirty) );
        }

        /* compose the done message */
        mssg.req       = DONE_REQ_CODE;
        mssg.src       = world_mpi_rank;
        mssg.dest      = world_server_mpi_rank;
        mssg.mssg_num  = -1; /* set by send function */
        mssg.base_addr = 0; /* not used */
        mssg.len       = 0; /* not used */
        mssg.ver       = 0; /* not used */
        mssg.magic     = MSSG_MAGIC;

        if ( success ) {


            success = send_mssg(&mssg);

            if ( ! success ) {

                nerrors++;
                if ( verbose ) {
                    HDfprintf(stdout, "%d:%s: send_mssg() failed on done.\n", 
                              world_mpi_rank, fcn_name);
                }
            }
        }
    }
    
    max_nerrors = get_max_nerrors();

    if ( world_mpi_rank == 0 ) {

	if ( max_nerrors == 0 ) {

	    PASSED();

        } else {

            failures++;
            H5_FAILED();
        }
    }

    success = ( ( success ) && ( max_nerrors == 0 ) );

    return(success);

} /* smoke_check_3() */


/*****************************************************************************
 *
 * Function:	smoke_check_4()
 *
 * Purpose:	Fourth smoke check for the parallel cache.
 *
 *		Use random reads to vary the loads on the diffferent 
 *		processors.  Also force different cache size adjustments.
 *
 *		In this test, load process 0 lightly, and the other
 *		processes heavily.
 *
 * Return:	Success:	TRUE
 *
 *		Failure:	FALSE
 *
 * Programmer:	JRM -- 1/13/06
 *
 * Modifications:
 *
 *		Added code intended to insure correct operation with large
 *		numbers of processors.
 *							JRM - 1/31/06
 *	
 *****************************************************************************/

hbool_t
smoke_check_4(void)
{
    const char * fcn_name = "smoke_check_4()";
    hbool_t success = TRUE;
    int i;
    int max_nerrors;
    int min_count;
    int max_count;
    int min_idx;
    int max_idx;
    hid_t fid = -1;
    H5F_t * file_ptr = NULL;
    H5C_t * cache_ptr = NULL;
    struct mssg_t mssg;

    if ( world_mpi_rank == 0 ) {

        TESTING("smoke check #4");
    }

    nerrors = 0;
    init_data();

    if ( world_mpi_rank == world_server_mpi_rank ) {

	if ( ! server_main() ) {

            /* some error occured in the server -- report failure */
            nerrors++;
            if ( verbose ) {
		HDfprintf(stdout, "%d:%s: server_main() failed.\n",
                          world_mpi_rank, fcn_name);
            }
        }
    }
    else /* run the clients */
    {
        if ( ! setup_cache_for_test(&fid, &file_ptr, &cache_ptr) ) {

            nerrors++;
            fid = -1;
            cache_ptr = NULL;
            if ( verbose ) {
		HDfprintf(stdout, "%d:%s: setup_cache_for_test() failed.\n",
                          world_mpi_rank, fcn_name);
            }
        }

        
        min_count = 100 * (file_mpi_rank % 4);
        max_count = min_count + 50;

        for ( i = 0; i < (NUM_DATA_ENTRIES / 4); i++ )
        {
            insert_entry(cache_ptr, file_ptr, i, H5AC__NO_FLAGS_SET);

            if ( i > 100 ) {

		lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                               (i - 100), i, 
                                               min_count, max_count);
            }
        }

        min_count = 10 * (file_mpi_rank % 4);
        max_count = min_count + 100;

        for ( i = (NUM_DATA_ENTRIES / 4); i < (NUM_DATA_ENTRIES / 2); i++ )
        {
            insert_entry(cache_ptr, file_ptr, i, H5AC__NO_FLAGS_SET);

            if ( i > 100 ) {

		lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                               (i - 100), i,
                                               min_count, max_count);
            }
        }

        min_idx = 0;
        max_idx = (((NUM_DATA_ENTRIES / 10) / 4) * 
                   ((file_mpi_rank % 4) + 1)) - 1;

        for ( i = (NUM_DATA_ENTRIES / 2) - 1; i >= 0; i-=2 )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC__NO_FLAGS_SET);
	    lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                           min_idx, max_idx, 0, 100);
        }

        min_idx = 0;
        max_idx = (((NUM_DATA_ENTRIES / 10) / 8) * 
                   ((file_mpi_rank % 4) + 1)) - 1;

        for ( i = 0; i < (NUM_DATA_ENTRIES / 2); i+=2 )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC__DIRTIED_FLAG);
	    lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                           min_idx, max_idx, 0, 100);
        }

        min_count = 10 * (file_mpi_rank % 4);
        max_count = min_count + 100;

        /* rename the first half of the entries... */
        for ( i = 0; i < (NUM_DATA_ENTRIES / 2); i++ )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC__NO_FLAGS_SET);
	    rename_entry(cache_ptr, file_ptr, i, (i + (NUM_DATA_ENTRIES / 2)));
	    lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                           0, (NUM_DATA_ENTRIES / 20), 
                                           min_count, max_count);
        }

        /* ...and then rename them back. */
        for ( i = (NUM_DATA_ENTRIES / 2) - 1; i >= 0; i-- )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC__DIRTIED_FLAG);
	    rename_entry(cache_ptr, file_ptr, i, (i + (NUM_DATA_ENTRIES / 2)));
	    lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                           0, (NUM_DATA_ENTRIES / 40),
                                           min_count, max_count);
        }

        /* finally, do some dirty lock/unlocks while we give the cache
         * a chance t reduce its size.
         */
        min_count = 100 * (file_mpi_rank % 4);
        max_count = min_count + 100;

        for ( i = 0; i < (NUM_DATA_ENTRIES / 2); i+=2 )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC__DIRTIED_FLAG);

            if ( i > 100 ) {

		lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                               (i - 100), i, 
                                               min_count, max_count);
            }
        }

        if ( fid >= 0 ) {

            if ( ! take_down_cache(fid) ) {

                nerrors++;
                if ( verbose ) {
		    HDfprintf(stdout, "%d:%s: take_down_cache() failed.\n",
                              world_mpi_rank, fcn_name);
                }
            }
        }

        /* verify that all instance of datum are back where the started 
         * and are clean.
         */

        for ( i = 0; i < NUM_DATA_ENTRIES; i++ )
        {
            HDassert( data_index[i] == i );
            HDassert( ! (data[i].dirty) );
        }

        /* compose the done message */
        mssg.req       = DONE_REQ_CODE;
        mssg.src       = world_mpi_rank;
        mssg.dest      = world_server_mpi_rank;
        mssg.mssg_num  = -1; /* set by send function */
        mssg.base_addr = 0; /* not used */
        mssg.len       = 0; /* not used */
        mssg.ver       = 0; /* not used */
        mssg.magic     = MSSG_MAGIC;

        if ( success ) {


            success = send_mssg(&mssg);

            if ( ! success ) {

                nerrors++;
                if ( verbose ) {
                    HDfprintf(stdout, "%d:%s: send_mssg() failed on done.\n", 
                              world_mpi_rank, fcn_name);
                }
            }
        }
    }
    
    max_nerrors = get_max_nerrors();

    if ( world_mpi_rank == 0 ) {

	if ( max_nerrors == 0 ) {

	    PASSED();

        } else {

            failures++;
            H5_FAILED();
        }
    }

    success = ( ( success ) && ( max_nerrors == 0 ) );

    return(success);

} /* smoke_check_4() */


/*****************************************************************************
 *
 * Function:	main()
 *
 * Purpose:	Main function for the parallel cache test.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	JRM -- 12/23/05
 *
 * Modifications:
 *
 *		None.
 *	
 *****************************************************************************/

int
main(int argc, char **argv)
{
    const char * fcn_name = "main()";
    int i;
    int mpi_size;
    int mpi_rank;
    int ret_code;
    int max_nerrors;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    world_mpi_size = mpi_size;
    world_mpi_rank = mpi_rank;
    world_server_mpi_rank = mpi_size - 1;
    world_mpi_comm = MPI_COMM_WORLD;

    H5open();

    if (MAINPROCESS){
	printf("===================================\n");
	printf("Parallel metadata cache tests\n");
	printf("	mpi_size = %d\n", mpi_size);
	printf("===================================\n");
    }

    if ( mpi_size < 3 ) {

        if ( MAINPROCESS ) {

            printf("	Need at least 3 processes.  Exiting.\n");
        }
        goto finish;
    }

    set_up_file_communicator();

    setup_derived_types();

    /* h5_fixname() will hang some processes don't participate.
     *
     * Thus we set up the fapl global with the world communicator, 
     * make our calls to h5_fixname(), discard the fapl, and then
     * create it again with the file communicator.
     */

    /* setup file access property list with the world communicator */
    if ( FAIL == (fapl = H5Pcreate(H5P_FILE_ACCESS)) ) {
        nerrors++;
	if ( verbose ) {
	    HDfprintf(stdout, "%d:%s: H5Pcreate() failed 1.\n", 
                      world_mpi_rank, fcn_name);
        }
    }

    if ( H5Pset_fapl_mpio(fapl, world_mpi_comm, MPI_INFO_NULL) < 0 ) {

        nerrors++;
        if ( verbose ) {
            HDfprintf(stdout, "%d:%s: H5Pset_fapl_mpio() failed 1.\n", 
                      world_mpi_rank, fcn_name);
        }
    }

    /* fix the file names */
    for ( i = 0; i < sizeof(FILENAME) / sizeof(FILENAME[0]) - 1; ++i ) 
    {
        if ( h5_fixname(FILENAME[i], fapl, filenames[i], 
                        sizeof(filenames[i])) == NULL ) {

            nerrors++;
            if ( verbose ) {
                HDfprintf(stdout, "%d:%s: h5_fixname() failed.\n", 
                          world_mpi_rank, fcn_name);
            }
            break;
        }
    }

    /* close the fapl before we set it up again */
    if ( H5Pclose(fapl) < 0 ) {
        nerrors++;
	if ( verbose ) {
	    HDfprintf(stdout, "%d:%s: H5Pclose() failed.\n", 
                      world_mpi_rank, fcn_name);
        }
    }

    /* now create the fapl again, excluding the server process. */
    if ( world_mpi_rank != world_server_mpi_rank ) {

        /* setup file access property list */
        if ( FAIL == (fapl = H5Pcreate(H5P_FILE_ACCESS)) ) {
	    nerrors++;
	    if ( verbose ) {
	        HDfprintf(stdout, "%d:%s: H5Pcreate() failed 2.\n", 
                          world_mpi_rank, fcn_name);
            }
        }

        if ( H5Pset_fapl_mpio(fapl, file_mpi_comm, MPI_INFO_NULL) < 0 ) {

            nerrors++;
	    if ( verbose ) {
	        HDfprintf(stdout, "%d:%s: H5Pset_fapl_mpio() failed 2.\n", 
                          world_mpi_rank, fcn_name);
            }
        }
    }

    setup_rand();
    
    max_nerrors = get_max_nerrors();

    if ( max_nerrors != 0 ) {

        /* errors in setup -- no point in continuing */

        if ( world_mpi_rank == 0 ) {

            HDfprintf(stdout, "Errors in test initialization.  Exiting.\n");
        }
	goto finish;
    }

    /* run the tests */
#if 1
    server_smoke_check();
    smoke_check_1();
#endif
#if 1
    smoke_check_2();
#endif
#if 1
    smoke_check_3();
#endif
#if 1
    smoke_check_4();
#endif   

finish:
    /* make sure all processes are finished before final report, cleanup
     * and exit.
     */
    MPI_Barrier(MPI_COMM_WORLD);
    if (MAINPROCESS){		/* only process 0 reports */
	printf("===================================\n");
	if (failures){
	    printf("***metadata cache tests detected %d failures***\n", 
                   failures);
	}
	else{
	    printf("metadata cache tests finished with no failures\n");
	}
	printf("===================================\n");
    }

    /* close HDF5 library */
    H5close();

    /* MPI_Finalize must be called AFTER H5close which may use MPI calls */
    MPI_Finalize();

    /* cannot just return (failures) because exit code is limited to 1byte */
    return(failures != 0);
}

