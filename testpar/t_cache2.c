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
 * Parallel metadata cache tests.
 *
 */

#include "h5test.h"
#include "testpar.h"
#include "H5Iprivate.h"
#include "H5MFprivate.h"
#include "H5AC2private.h"

#define H5C2_PACKAGE             /*suppress error about including H5C2pkg   */

#include "H5C2pkg.h"

#define H5AC2_PACKAGE            /*suppress error about including H5AC2pkg  */

#include "H5AC2pkg.h"

#define H5F_PACKAGE             /*suppress error about including H5Fpkg   */

#include "H5Fpkg.h"

#define BASE_ADDR               (haddr_t)1024


int     	nerrors = 0;
int		failures = 0;
hbool_t		verbose = TRUE; /* used to control error messages */
#if 1
/* So far we haven't needed this, but that may change.
 * Keep it around for now
 */
hid_t noblock_dxpl_id=(-1);
#endif

#define NFILENAME 2
#define PARATESTFILE filenames[0]
const char *FILENAME[NFILENAME]={"Cache2TestDummy", NULL};
char    filenames[NFILENAME][PATH_MAX];
hid_t   fapl;                           /* file access property list */
haddr_t max_addr = 0;			/* used to store the end of 
					 * the address space used by
					 * the data array (see below).
					 */
hbool_t callbacks_verbose = FALSE;	/* flag used to control whether 
					 * the callback functions are in
					 * verbose mode.
					 */


int		world_mpi_size = -1;
int		world_mpi_rank = -1;
int		world_server_mpi_rank = -1;
MPI_Comm	world_mpi_comm = MPI_COMM_NULL;
int		file_mpi_size = -1;
int		file_mpi_rank = -1;
MPI_Comm	file_mpi_comm = MPI_COMM_NULL;


/* the following globals are used to maintain rudementary statistics
 * to check the validity of the statistics maintained by H5C2.c
 */

long datum_clears          = 0;
long datum_pinned_clears   = 0;
long datum_destroys        = 0;
long datum_flushes         = 0;
long datum_pinned_flushes  = 0;
long datum_loads           = 0;
long global_pins	   = 0;
long global_dirty_pins	   = 0;
long local_pins		   = 0;


/*****************************************************************************
 * struct datum
 *
 *	Instances of struct datum are used to store information on entries
 *	that may be loaded into the cache.  The individual fields are
 *	discussed below:
 *
 *	header:	Instance of H5C2_cache_entry_t used by the for its data.
 *		This field is only used on the file processes, not on the
 *		server process.
 *
 *		This field MUST be the first entry in this structure.
 *
 *	base_addr: Base address of the entry.
 *
 *	len:	Length of the entry.
 *
 *	local_len:  Length of the entry according to the cache.  This
 *		value must be positive, and may not be larger than len.
 *
 *		The field exists to allow us change the sizes of entries
 *		in the cache without upsetting the server.  This value 
 *		is only used locally, and is never sent to the server.  
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
 *	global_pinned:	Boolean flag that is set to true iff the entry has
 *		been pinned collectively in all caches.  Since writes must
 *		be collective across all processes, only entries pinned
 *		in this fashion may be marked dirty.
 *
 *	local_pinned:	Boolean flag that is set to true iff the entry
 *		has been pinned in the local cache, but probably not all
 *		caches.  Such pins will typically not be consistant across
 *		processes, and thus cannot be marked as dirty unless they
 *		happen to overlap some collective operation.
 *
 *	index:	Index of this instance of datum in the data_index[] array
 *		discussed below.
 *
 *****************************************************************************/

struct datum
{
    H5C2_cache_entry_t	header;
    haddr_t		base_addr;
    size_t		len;
    size_t		local_len;
    int			ver;
    hbool_t		dirty;
    hbool_t		valid;
    hbool_t		locked;
    hbool_t		global_pinned;
    hbool_t		local_pinned;
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

#define NUM_DATA_ENTRIES	100000

struct datum data[NUM_DATA_ENTRIES];


/* Many tests use the size of data array as the size of test loops.
 * On some machines, this results in unacceptably long test runs.
 *
 * To deal with this issue, I have introduced the virt_num_data_entries
 * global, which can be set to a lower value to throtle the length of
 * tests.
 *
 * Note that this value must always be divisible by 40, and must be an
 * even divisor of NUM_DATA_ENTRIES.  So far, all tests have been with
 * powers of 10 that meet these criteria.
 *
 * Further, this value must be consistant across all processes.
 */

#define STD_VIRT_NUM_DATA_ENTRIES	NUM_DATA_ENTRIES
#define EXPRESS_VIRT_NUM_DATA_ENTRIES	(NUM_DATA_ENTRIES / 10)
/* Use a smaller test size to avoid creating huge MPE logfiles. */
#define MPE_VIRT_NUM_DATA_ENTIES	(NUM_DATA_ENTRIES / 100)

int virt_num_data_entries = NUM_DATA_ENTRIES;


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

#define DO_WRITE_REQ_ACK	FALSE
#define DO_SYNC_AFTER_WRITE	TRUE

#define	WRITE_REQ_CODE		0
#define	WRITE_REQ_ACK_CODE	1
#define READ_REQ_CODE		2
#define READ_REQ_REPLY_CODE	3
#define SYNC_REQ_CODE		4
#define SYNC_ACK_CODE		5
#define DONE_REQ_CODE		6
#define MAX_REQ_CODE		6

#define MSSG_MAGIC	0x1248

struct mssg_t
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

/* stats functions */

void print_stats(void);
void reset_stats(void);

/* MPI setup functions */

hbool_t set_up_file_communicator(void);


/* data array manipulation functions */

int addr_to_datum_index(haddr_t base_addr);
void init_data(void);


/* test coodination related functions */

int do_express_test(void);
void do_sync(void);
int get_max_nerrors(void);


/* mssg xfer related functions */

hbool_t recv_mssg(struct mssg_t *mssg_ptr, int mssg_tag_offset);
hbool_t send_mssg(struct mssg_t *mssg_ptr, hbool_t add_req_to_tag);
hbool_t setup_derived_types(void);
hbool_t takedown_derived_types(void);


/* server functions */

hbool_t server_main(void);
hbool_t serve_read_request(struct mssg_t * mssg_ptr);
hbool_t serve_sync_request(struct mssg_t * mssg_ptr);
hbool_t serve_write_request(struct mssg_t * mssg_ptr);


/* call back functions & related data structures */

static herr_t datum_clear_dirty_bits(haddr_t addr,
                                     size_t len,
                                     void * thing);

static void * datum_deserialize(haddr_t addr,
                                size_t len,
                                const void * image_ptr,
                                const void * udata_ptr,
                                hbool_t * dirty_ptr);

static herr_t datum_image_len(void *thing,
                              size_t *image_len_ptr);

static herr_t datum_serialize(haddr_t addr,
                              size_t len,
                              void * image_ptr,
                              void * thing,
                              unsigned * flags_ptr,
                              haddr_t * new_addr_ptr,
                              size_t * new_len_ptr,
                              void ** new_image_ptr_ptr);

static herr_t datum_free_icr(haddr_t addr,
                             size_t len,
                             void * thing);

#define DATUM_ENTRY_TYPE	H5AC2_TEST_ID

#define NUMBER_OF_ENTRY_TYPES	1

const H5C2_class_t types[NUMBER_OF_ENTRY_TYPES] =
{
  {
    DATUM_ENTRY_TYPE,
    "datum",
    H5FD_MEM_DEFAULT,
    (H5C2_deserialize_func_t)datum_deserialize,
    (H5C2_image_len_func_t)datum_image_len,
    (H5C2_serialize_func_t)datum_serialize,
    (H5C2_free_icr_func_t)datum_free_icr,
    (H5C2_clear_dirty_bits_func_t)datum_clear_dirty_bits
  }
};


/* test utility functions */

void expunge_entry(H5C2_t * cache_ptr, H5F_t * file_ptr, int32_t idx);
void insert_entry(H5C2_t * cache_ptr, H5F_t * file_ptr,
                  int32_t idx, unsigned int flags);
void local_pin_and_unpin_random_entries(H5C2_t * cache_ptr, H5F_t * file_ptr,
                                        int min_idx, int max_idx,
				        int min_count, int max_count);
void local_pin_random_entry(H5C2_t * cache_ptr, H5F_t * file_ptr,
                            int min_idx, int max_idx);
void local_unpin_all_entries(H5C2_t * cache_ptr, H5F_t * file_ptr,
			     hbool_t via_unprotect);
int local_unpin_next_pinned_entry(H5C2_t * cache_ptr, H5F_t * file_ptr,
                                  int start_idx, hbool_t via_unprotect);
void lock_and_unlock_random_entries(H5C2_t * cache_ptr, H5F_t * file_ptr,
                                    int min_idx, int max_idx,
                                    int min_count, int max_count);
void lock_and_unlock_random_entry(H5C2_t * cache_ptr, H5F_t * file_ptr,
                                  int min_idx, int max_idx);
void lock_entry(H5C2_t * cache_ptr, H5F_t * file_ptr, int32_t idx);
void mark_pinned_entry_dirty(H5C2_t * cache_ptr, H5F_t * file_ptr,
                int32_t idx, hbool_t size_changed, size_t new_size);
void mark_pinned_or_protected_entry_dirty(H5C2_t * cache_ptr,
                                          H5F_t * file_ptr,
                                          int32_t idx);
void pin_entry(H5C2_t * cache_ptr, H5F_t * file_ptr, int32_t idx,
	       hbool_t global, hbool_t dirty);
void pin_protected_entry(H5C2_t * cache_ptr, H5F_t * file_ptr,
                         int32_t idx, hbool_t global);
void rename_entry(H5C2_t * cache_ptr, H5F_t * file_ptr,
                  int32_t old_idx, int32_t new_idx);
void resize_entry(H5C2_t * cache_ptr, H5F_t * file_ptr, 
		  int32_t idx, size_t  new_size);
hbool_t setup_cache_for_test(hid_t * fid_ptr, H5F_t ** file_ptr_ptr,
                             H5C2_t ** cache_ptr_ptr);
void setup_rand(void);
hbool_t take_down_cache(hid_t fid, H5C2_t * cache_ptr);
void unlock_entry(H5C2_t * cache_ptr, H5F_t * file_ptr,
                  int32_t type, unsigned int flags);
void unpin_entry(H5C2_t * cache_ptr, H5F_t * file_ptr, int32_t idx,
	         hbool_t global, hbool_t dirty, hbool_t via_unprotect);


/* test functions */

hbool_t server_smoke_check(void);
hbool_t smoke_check_1(void);
hbool_t smoke_check_2(void);
hbool_t smoke_check_3(void);
hbool_t smoke_check_4(void);
hbool_t smoke_check_5(void);
hbool_t trace_file_check(void);


/*****************************************************************************/
/****************************** stats functions ******************************/
/*****************************************************************************/

/*****************************************************************************
 *
 * Function:	print_stats()
 *
 * Purpose:	Print the rudementary stats maintained by t_cache.
 *
 * 		This is a debugging function, which will not normally
 * 		be run as part of t_cache.
 *
 * Return:	void
 *
 * Programmer:	JRM -- 4/17/06
 *
 * Modifications:
 *
 *		None.
 *
 *****************************************************************************/

void
print_stats(void)
{
    HDfprintf(stdout,
	      "%d: datum clears / pinned clears / destroys = %ld / %ld / %ld\n",
              world_mpi_rank, datum_clears, datum_pinned_clears,
	      datum_destroys );
    HDfprintf(stdout,
	      "%d: datum flushes / pinned flushes / loads  = %ld / %ld / %ld\n",
              world_mpi_rank, datum_flushes, datum_pinned_flushes,
	      datum_loads );
    HDfprintf(stdout,
	      "%d: pins: global / global dirty / local = %ld / %ld / %ld\n",
	      world_mpi_rank, global_pins, global_dirty_pins, local_pins);
    HDfflush(stdout);

    return;

} /* print_stats() */

/*****************************************************************************
 *
 * Function:	reset_stats()
 *
 * Purpose:	Reset the rudementary stats maintained by t_cache.
 *
 * Return:	void
 *
 * Programmer:	JRM -- 4/17/06
 *
 * Modifications:
 *
 *		None.
 *
 *****************************************************************************/

void
reset_stats(void)
{
    datum_clears          = 0;
    datum_pinned_clears   = 0;
    datum_destroys        = 0;
    datum_flushes         = 0;
    datum_pinned_flushes  = 0;
    datum_loads           = 0;
    global_pins		  = 0;
    global_dirty_pins	  = 0;
    local_pins		  = 0;

    return;

} /* reset_stats() */


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
    /* const char * fcn_name = "addr_to_datum_index()"; */
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
 * Function:	init_data()
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
 *		JRM -- 7/11/06
 *		Added support for the local_len field.
 *
 *****************************************************************************/

void
init_data(void)
{
    /* const char * fcn_name = "init_data()"; */
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
    haddr_t addr = BASE_ADDR;

    /* this must hold so renames don't change entry size. */
    HDassert( (NUM_DATA_ENTRIES / 2) % 20 == 0 );
    HDassert( (virt_num_data_entries / 2) % 20 == 0 );

    for ( i = 0; i < NUM_DATA_ENTRIES; i++ )
    {
        data[i].base_addr     = addr;
        data[i].len           = (size_t)(addr_offsets[j]);
        data[i].local_len     = (size_t)(addr_offsets[j]);
        data[i].ver           = 0;
        data[i].dirty         = FALSE;
        data[i].valid         = FALSE;
        data[i].locked        = FALSE;
	data[i].global_pinned = FALSE;
	data[i].local_pinned  = FALSE;
	data[i].index         = i;

        data_index[i]         = i;

        addr += addr_offsets[j];
        HDassert( addr > data[i].base_addr );

        j = (j + 1) % num_addr_offsets;
    }

    /* save the end of the address space used by the data array */
    max_addr = addr;

    return;

} /* init_data() */


/*****************************************************************************/
/******************** test coodination related functions *********************/
/*****************************************************************************/

/*****************************************************************************
 *
 * Function:	do_express_test()
 *
 * Purpose:	Do an MPI_Allreduce to obtain the maximum value returned
 * 		by GetTestExpress() across all processes.  Return this
 * 		value.
 *
 * 		Envirmoment variables can be different across different
 * 		processes.  This function ensures that all processes agree
 * 		on whether to do an express test.
 *
 * Return:	Success:	Maximum of the values returned by
 * 				GetTestExpress() across	all processes.
 *
 *		Failure:	-1
 *
 * Programmer:	JRM -- 4/25/06
 *
 * Modifications:
 *
 *		None.
 *
 *****************************************************************************/

int
do_express_test(void)
{
    const char * fcn_name = "do_express_test()";
    int express_test;
    int max_express_test;
    int result;

    express_test = GetTestExpress();

    result = MPI_Allreduce((void *)&express_test,
                           (void *)&max_express_test,
                           1,
                           MPI_INT,
                           MPI_MAX,
                           world_mpi_comm);

    if ( result != MPI_SUCCESS ) {

        nerrors++;
        max_express_test = -1;
        if ( verbose ) {
            HDfprintf(stdout, "%d:%s: MPI_Allreduce() failed.\n",
                      world_mpi_rank, fcn_name );
        }
    }

    return(max_express_test);

} /* do_express_test() */


/*****************************************************************************
 *
 * Function:	do_sync()
 *
 * Purpose:	Ensure that all messages sent by this process have been
 * 		processed before proceeding.
 *
 * 		Do this by exchanging sync req / sync ack messages with
 * 		the server.
 *
 * 		Do nothing if nerrors is greater than zero.
 *
 * Return:	void
 *
 * Programmer:	JRM -- 5/10/06
 *
 * Modifications:
 *
 *		None.
 *
 *****************************************************************************/

void
do_sync(void)
{
    const char * fcn_name = "do_sync()";

    struct mssg_t mssg;

    if ( nerrors <= 0 ) {

        /* compose the message */
	mssg.req       = SYNC_REQ_CODE;
        mssg.src       = world_mpi_rank;
        mssg.dest      = world_server_mpi_rank;
        mssg.mssg_num  = -1; /* set by send function */
        mssg.base_addr = 0;
        mssg.len       = 0;
        mssg.ver       = 0;
        mssg.magic     = MSSG_MAGIC;

	if ( ! send_mssg(&mssg, FALSE) ) {

	    nerrors++;
	    if ( verbose ) {
                HDfprintf(stdout, "%d:%s: send_mssg() failed.\n",
                          world_mpi_rank, fcn_name);
            }
        }
    }

    if ( nerrors <= 0 ) {

	if ( ! recv_mssg(&mssg, SYNC_ACK_CODE) ) {

            nerrors++;
            if ( verbose ) {
                HDfprintf(stdout, "%d:%s: recv_mssg() failed.\n",
                          world_mpi_rank, fcn_name);
            }
	} else if ( ( mssg.req != SYNC_ACK_CODE ) ||
                    ( mssg.src != world_server_mpi_rank ) ||
                    ( mssg.dest != world_mpi_rank ) ||
		    ( mssg.magic != MSSG_MAGIC ) ) {

            nerrors++;
	    if ( verbose ) {
                HDfprintf(stdout, "%d:%s: Bad data in sync ack.\n",
                          world_mpi_rank, fcn_name);
            }
	}
    }

    return;

} /* do_sync() */


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


/*****************************************************************************/
/************************ mssg xfer related functions ************************/
/*****************************************************************************/

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
 * 		JRM -- 5/10/06
 *		Added mssg_tag_offset parameter and supporting code.
 *
 *****************************************************************************/

#define CACHE_TEST_TAG	99 /* different from any used by the library */

hbool_t
recv_mssg(struct mssg_t *mssg_ptr,
	  int mssg_tag_offset)
{
    const char * fcn_name = "recv_mssg()";
    hbool_t success = TRUE;
    int mssg_tag = CACHE_TEST_TAG;
    int result;
    MPI_Status status;

    if ( ( mssg_ptr == NULL ) ||
         ( mssg_tag_offset < 0 ) ||
         ( mssg_tag_offset> MAX_REQ_CODE ) ) {

        nerrors++;
        success = FALSE;
        if ( verbose ) {
            HDfprintf(stdout, "%d:%s: bad param(s) on entry.\n",
                      world_mpi_rank, fcn_name);
        }
    } else {

        mssg_tag += mssg_tag_offset;
    }

    if ( success ) {

        result = MPI_Recv((void *)mssg_ptr, 1, mpi_mssg_t, MPI_ANY_SOURCE,
                          mssg_tag, world_mpi_comm, &status);

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
 * 		JRM -- 5/10/06
 *		Added the add_req_to_tag parameter and supporting code.
 *
 *****************************************************************************/

hbool_t
send_mssg(struct mssg_t *mssg_ptr,
	  hbool_t add_req_to_tag)
{
    const char * fcn_name = "send_mssg()";
    hbool_t success = TRUE;
    int mssg_tag = CACHE_TEST_TAG;
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

	if ( add_req_to_tag ) {

	    mssg_tag += mssg_ptr->req;
	}

        result = MPI_Send((void *)mssg_ptr, 1, mpi_mssg_t,
                          mssg_ptr->dest, mssg_tag, world_mpi_comm);

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
    MPI_Datatype mpi_types[8] = {MPI_INT, MPI_INT, MPI_INT, MPI_LONG,
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

        result = MPI_Type_struct(8, block_len, displs, mpi_types, &mpi_mssg_t);

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
 * 		JRM -- 5/10/06
 *		Updated for sync message.
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
        success = recv_mssg(&mssg, 0);

        if ( success ) {

            switch ( mssg.req )
            {
		case WRITE_REQ_CODE:
		    success = serve_write_request(&mssg);
		    break;

		case WRITE_REQ_ACK_CODE:
                    success = FALSE;
		    HDfprintf(stdout, "%s: Received write ack?!?.\n", fcn_name);
		    break;

		case READ_REQ_CODE:
                    success = serve_read_request(&mssg);
		    break;

		case READ_REQ_REPLY_CODE:
                    success = FALSE;
		    HDfprintf(stdout, "%s: Received read req reply?!?.\n",
			      fcn_name);
		    break;

		case SYNC_REQ_CODE:
                    success = serve_sync_request(&mssg);
		    break;

		case SYNC_ACK_CODE:
                    success = FALSE;
		    HDfprintf(stdout, "%s: Received sync ack?!?.\n",
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
                  "%d:%s: proc %d read invalid entry. idx/base_addr = %d/%a.\n",
                         world_mpi_rank, fcn_name,
                         mssg_ptr->src, target_index,
			target_index,
			data[target_index].base_addr);
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

        success = send_mssg(&reply, TRUE);
    }

    return(success);

} /* serve_read_request() */


/*****************************************************************************
 *
 * Function:	serve_sync_request()
 *
 * Purpose:	Serve a sync request.
 *
 *		The function accepts a pointer to an instance of struct
 *		mssg_t as input.  If all sanity checks pass, it sends a
 *		sync ack to the requesting process.
 *
 *		This service exist to allow the sending process to ensure
 *		that all previous messages have been processed before
 *		proceeding.
 *
 * Return:	Success:	TRUE
 *
 *		Failure:	FALSE
 *
 * Programmer:	JRM -- 5/10/06
 *
 * Modifications:
 *
 *		None.
 *
 *****************************************************************************/

hbool_t
serve_sync_request(struct mssg_t * mssg_ptr)
{
    const char * fcn_name = "serve_sync_request()";
    hbool_t success = TRUE;
    struct mssg_t reply;

    if ( ( mssg_ptr == NULL ) ||
         ( mssg_ptr->req != SYNC_REQ_CODE ) ||
         ( mssg_ptr->magic != MSSG_MAGIC ) ) {

        nerrors++;
        success = FALSE;
        if ( verbose ) {
            HDfprintf(stdout, "%d:%s: Bad mssg on entry.\n",
                      world_mpi_rank, fcn_name);
        }
    }

    if ( success ) {

        /* compose the reply message */
        reply.req       = SYNC_ACK_CODE;
        reply.src       = world_mpi_rank;
        reply.dest      = mssg_ptr->src;
        reply.mssg_num  = -1; /* set by send function */
        reply.base_addr = 0;
        reply.len       = 0;
        reply.ver       = 0;
        reply.magic     = MSSG_MAGIC;
    }

    if ( success ) {

        success = send_mssg(&reply, TRUE);
    }

    return(success);

} /* serve_sync_request() */


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
 *		JRM -- 5/9/06
 *		Added code supporting a write ack message.  This is a
 *		speculative fix to a bug observed on Cobalt.  If it
 *		doesn't work, it will help narrow down the possibilities.
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
#if DO_WRITE_REQ_ACK
    struct mssg_t reply;
#endif /* DO_WRITE_REQ_ACK */

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

	/* process the write */
        data[target_index].ver = new_ver_num;
        data[target_index].valid = TRUE;

#if DO_WRITE_REQ_ACK

        /* compose the reply message */
        reply.req       = WRITE_REQ_ACK_CODE;
        reply.src       = world_mpi_rank;
        reply.dest      = mssg_ptr->src;
        reply.mssg_num  = -1; /* set by send function */
        reply.base_addr = data[target_index].base_addr;
        reply.len       = data[target_index].len;
        reply.ver       = data[target_index].ver;
        reply.magic     = MSSG_MAGIC;

	/* and send it */
        success = send_mssg(&reply, TRUE);

#endif /* DO_WRITE_REQ_ACK */

    }

    return(success);

} /* serve_write_request() */


/*****************************************************************************/
/**************************** Call back functions ****************************/
/*****************************************************************************/

/*-------------------------------------------------------------------------
 * Function:	datum_clear_dirty_bits
 *
 * Purpose:	Clear the dirty bits of the target entry.
 *
 * Return:	SUCCEED
 *
 * Programmer:	John Mainzer
 *              10/30/07
 *
 * Modifications:
 *
 * 		None
 *
 *-------------------------------------------------------------------------
 */

herr_t 
datum_clear_dirty_bits(UNUSED haddr_t addr,
                       UNUSED size_t len,
                       void * thing)
{
    int idx;
    struct datum * entry_ptr;

    HDassert( thing );

    entry_ptr = (struct datum *)thing;

    idx = addr_to_datum_index(entry_ptr->base_addr);

    if ( callbacks_verbose ) {

        HDfprintf(stdout, 
		  "%d: clear_dirty_bits() idx = %d, addr = %ld, len = %d.\n",
		  world_mpi_rank, idx, (long)addr, (int)len);
	fflush(stdout);
    }

    HDassert( idx >= 0 );
    HDassert( idx < NUM_DATA_ENTRIES );
    HDassert( idx < virt_num_data_entries );
    HDassert( &(data[idx]) == entry_ptr );

    HDassert( entry_ptr->header.addr == entry_ptr->base_addr );
    HDassert( ( entry_ptr->header.size == entry_ptr->len ) ||
              ( entry_ptr->header.size == entry_ptr->local_len ) );

    entry_ptr->dirty = FALSE;

    datum_clears++;

    if ( entry_ptr->header.is_pinned ) {

        datum_pinned_clears++;
        HDassert( entry_ptr->global_pinned || entry_ptr->local_pinned );
    }

    return(SUCCEED);

} /* datum_clear_dirty_bits() */


/*-------------------------------------------------------------------------
 * Function:	datum_deserialize
 *
 * Purpose:	deserialize the entry.
 *
 * Return:	void * (pointer to the in core representation of the entry)
 *
 * Programmer:	John Mainzer
 *              9/20/07
 *
 * Modifications:
 *
 * 		None
 *
 *-------------------------------------------------------------------------
 */

void *
datum_deserialize(haddr_t addr,
                  UNUSED size_t len,
                  const void * image_ptr,
                  const UNUSED void * udata_ptr,
                  hbool_t * dirty_ptr)
{
    const char * fcn_name = "load_datum()";
    hbool_t success = TRUE;
    int idx;
    struct datum * entry_ptr = NULL;
    struct mssg_t mssg;

    HDassert( image_ptr != NULL );

    idx = addr_to_datum_index(addr);

    HDassert( idx >= 0 );
    HDassert( idx < NUM_DATA_ENTRIES );
    HDassert( idx < virt_num_data_entries );

    entry_ptr = &(data[idx]);

    HDassert( addr == entry_ptr->base_addr );
    HDassert( ! entry_ptr->global_pinned );
    HDassert( ! entry_ptr->local_pinned );

    if ( callbacks_verbose ) {

        HDfprintf(stdout, 
	  "%d: deserialize() idx = %d, addr = %ld, len = %d, is_dirty = %d.\n",
	  world_mpi_rank, idx, (long)addr, (int)len, 
	  (int)(entry_ptr->header.is_dirty));
	fflush(stdout);
    }

    /* compose the read message */
    mssg.req       = READ_REQ_CODE;
    mssg.src       = world_mpi_rank;
    mssg.dest      = world_server_mpi_rank;
    mssg.mssg_num  = -1; /* set by send function */
    mssg.base_addr = entry_ptr->base_addr;
    mssg.len       = entry_ptr->len;
    mssg.ver       = 0; /* bogus -- should be corrected by server */
    mssg.magic     = MSSG_MAGIC;

    if ( ! send_mssg(&mssg, FALSE) ) {

        nerrors++;
        success = FALSE;
        if ( verbose ) {
            HDfprintf(stdout, "%d:%s: send_mssg() failed.\n",
                      world_mpi_rank, fcn_name);
        }
    }

    if ( success ) {

        if ( ! recv_mssg(&mssg, READ_REQ_REPLY_CODE) ) {

            nerrors++;
            success = FALSE;
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
#if 0 /* This has been useful debugging code -- keep it for now. */
        if ( mssg.req != READ_REQ_REPLY_CODE ) {

            HDfprintf(stdout, "%d:%s: mssg.req != READ_REQ_REPLY_CODE.\n",
                      world_mpi_rank, fcn_name);
            HDfprintf(stdout, "%d:%s: mssg.req = %d.\n",
                          world_mpi_rank, fcn_name, (int)(mssg.req));
        }

        if ( mssg.src != world_server_mpi_rank ) {

            HDfprintf(stdout, "%d:%s: mssg.src != world_server_mpi_rank.\n",
                      world_mpi_rank, fcn_name);
        }

        if ( mssg.dest != world_mpi_rank ) {

            HDfprintf(stdout, "%d:%s: mssg.dest != world_mpi_rank.\n",
                      world_mpi_rank, fcn_name);
        }

        if ( mssg.base_addr != entry_ptr->base_addr ) {

            HDfprintf(stdout,
                      "%d:%s: mssg.base_addr != entry_ptr->base_addr.\n",
                       world_mpi_rank, fcn_name);
            HDfprintf(stdout, "%d:%s: mssg.base_addr = %a.\n",
                      world_mpi_rank, fcn_name, mssg.base_addr);
            HDfprintf(stdout, "%d:%s: entry_ptr->base_addr = %a.\n",
                      world_mpi_rank, fcn_name, entry_ptr->base_addr);
        }

        if ( mssg.len != entry_ptr->len ) {

            HDfprintf(stdout, "%d:%s: mssg.len != entry_ptr->len.\n",
                      world_mpi_rank, fcn_name);
            HDfprintf(stdout, "%d:%s: mssg.len = %a.\n",
                      world_mpi_rank, fcn_name, mssg.len);
        }

        if ( mssg.ver < entry_ptr->ver ) {

            HDfprintf(stdout, "%d:%s: mssg.ver < entry_ptr->ver.\n",
                      world_mpi_rank, fcn_name);
        }

        if ( mssg.magic != MSSG_MAGIC ) {

            HDfprintf(stdout, "%d:%s: mssg.magic != MSSG_MAGIC.\n",
                      world_mpi_rank, fcn_name);
        }
#endif /* JRM */
        } else {

            entry_ptr->ver = mssg.ver;
            entry_ptr->dirty = FALSE;
	    *dirty_ptr = FALSE;
        }
    }

    if ( ! success ) {

        entry_ptr = NULL;

    }

    datum_loads++;

    return(entry_ptr);

} /* deserialize() */


/*-------------------------------------------------------------------------
 * Function:	datum_image_len
 *
 * Purpose:	Return the real (and possibly reduced) length of the image.
 * 		The helper functions verify that the correct version of 
 * 		deserialize is being called, and then call deserialize 
 * 		proper.
 *
 * Return:	SUCCEED
 *
 * Programmer:	John Mainzer
 *              9/19/07
 *
 * Modifications:
 *
 * 		None.
 *
 *-------------------------------------------------------------------------
 */

herr_t 
datum_image_len(void *thing,
                size_t *image_len_ptr)
{


    int idx;
    struct datum * entry_ptr;

    HDassert( thing );
    HDassert( image_len_ptr );

    entry_ptr = (struct datum *)thing;

    idx = addr_to_datum_index(entry_ptr->base_addr);

    HDassert( idx >= 0 );
    HDassert( idx < NUM_DATA_ENTRIES );
    HDassert( idx < virt_num_data_entries );
    HDassert( &(data[idx]) == entry_ptr );
    HDassert( entry_ptr->local_len > 0 );
    HDassert( entry_ptr->local_len <= entry_ptr->len );

    if ( callbacks_verbose ) {

        HDfprintf(stdout, 
		  "%d: image_len() idx = %d, addr = %ld, len = %d.\n",
		  world_mpi_rank, idx, (long)(entry_ptr->base_addr), 
		  (int)(entry_ptr->local_len));
	fflush(stdout);
    }

    HDassert( entry_ptr->header.addr == entry_ptr->base_addr );

    *image_len_ptr = entry_ptr->local_len;

    return(SUCCEED);

} /* datum_image_len() */


/*-------------------------------------------------------------------------
 * Function:	datum_serialize
 *
 * Purpose:	Serialize the supplied entry.
 *
 * Return:	SUCCEED if successful, FAIL otherwise.
 *
 * Programmer:	John Mainzer
 *              10/30/07
 *
 * Modifications:
 *
 * 		None.
 *
 *-------------------------------------------------------------------------
 */

herr_t 
datum_serialize(UNUSED haddr_t addr,
                UNUSED size_t len,
                void * image_ptr,
                void * thing,
                unsigned * flags_ptr,
                haddr_t * new_addr_ptr,
                size_t * new_len_ptr,
                void ** new_image_ptr_ptr)
{

    const char * fcn_name = "datum_serialize()";
    herr_t ret_value = SUCCEED;
    int idx;
    struct datum * entry_ptr;
    struct mssg_t mssg;

    HDassert( thing );
    HDassert( image_ptr );
    HDassert( flags_ptr );

    *flags_ptr = 0;

    HDassert( new_addr_ptr );
    HDassert( new_len_ptr );
    HDassert( new_image_ptr_ptr );

    entry_ptr = (struct datum *)thing;

    idx = addr_to_datum_index(entry_ptr->base_addr);

    HDassert( idx >= 0 );
    HDassert( idx < NUM_DATA_ENTRIES );
    HDassert( idx < virt_num_data_entries );
    HDassert( &(data[idx]) == entry_ptr );

    if ( callbacks_verbose ) {

        HDfprintf(stdout, 
		  "%d: serialize() idx = %d, addr = %ld, len = %d.\n",
		  world_mpi_rank, idx, (long)addr, (int)len);
	fflush(stdout);
    }

    HDassert( entry_ptr->header.addr == entry_ptr->base_addr );
    HDassert( ( entry_ptr->header.size == entry_ptr->len ) ||
              ( entry_ptr->header.size == entry_ptr->local_len ) );

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

                if ( ! send_mssg(&mssg, FALSE) ) {

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

#if DO_WRITE_REQ_ACK

        if ( ( ret_value == SUCCEED ) && ( entry_ptr->header.is_dirty ) ) {

            if ( ! recv_mssg(&mssg, WRITE_REQ_ACK_CODE) ) {

                nerrors++;
                ret_value = FAIL;
                if ( verbose ) {
                    HDfprintf(stdout, "%d:%s: recv_mssg() failed.\n",
                              world_mpi_rank, fcn_name);
                }
            } else if ( ( mssg.req != WRITE_REQ_ACK_CODE ) ||
	                ( mssg.src != world_server_mpi_rank ) ||
	                ( mssg.dest != world_mpi_rank ) ||
	                ( mssg.base_addr != entry_ptr->base_addr ) ||
	                ( mssg.len != entry_ptr->len ) ||
	                ( mssg.ver != entry_ptr->ver ) ||
	                ( mssg.magic != MSSG_MAGIC ) ) {

                nerrors++;
                ret_value = FAIL;
                if ( verbose ) {
                    HDfprintf(stdout, "%d:%s: Bad data in write req ack.\n",
                              world_mpi_rank, fcn_name);
                }
            }
        }

#endif /* DO_WRITE_REQ_ACK */

    datum_flushes++;

    if ( entry_ptr->header.is_pinned ) {

        datum_pinned_flushes++;
        HDassert( entry_ptr->global_pinned || entry_ptr->local_pinned );
    }

    return(ret_value);

} /* datum_serialize() */


/*-------------------------------------------------------------------------
 * Function:	datum_free_icr
 *
 * Purpose:	Nominally, this callback is supposed to free the 
 * 		in core representation of the entry.  
 *
 * 		In the context of this test bed, we use it to do 
 * 		do all the processing we used to do on a destroy.
 * 		
 * Return:	SUCCEED
 *
 * Programmer:	John Mainzer
 *              9/19/07
 *
 * Modifications:
 *
 * 		None.
 *
 *-------------------------------------------------------------------------
 */

herr_t 
datum_free_icr(UNUSED haddr_t addr,
               UNUSED size_t len,
               void * thing)
{
    int idx;
    struct datum * entry_ptr;

    HDassert( thing );

    entry_ptr = (struct datum *)thing;

    idx = addr_to_datum_index(entry_ptr->base_addr);

    HDassert( idx >= 0 );
    HDassert( idx < NUM_DATA_ENTRIES );
    HDassert( idx < virt_num_data_entries );
    HDassert( &(data[idx]) == entry_ptr );

    if ( callbacks_verbose ) {

        HDfprintf(stdout, 
	  "%d: free_icr() idx = %d, addr = %ld, len = %d, dirty = %d.\n",
		  world_mpi_rank, idx, (long)addr, (int)len, 
		  (int)(entry_ptr->dirty));
	fflush(stdout);
    }

    HDassert( entry_ptr->header.addr == entry_ptr->base_addr );
    HDassert( ( entry_ptr->header.size == entry_ptr->len ) ||
              ( entry_ptr->header.size == entry_ptr->local_len ) );

    HDassert( !(entry_ptr->dirty) );
    HDassert( !(entry_ptr->header.is_dirty) );
    HDassert( !(entry_ptr->global_pinned) );
    HDassert( !(entry_ptr->local_pinned) );
    HDassert( !(entry_ptr->header.is_pinned) );

    datum_destroys++;

    return(SUCCEED);

} /* datum_free_icr() */


/*****************************************************************************/
/************************** test utility functions ***************************/
/*****************************************************************************/

/*****************************************************************************
 * Function:    expunge_entry()
 *
 * Purpose:     Expunge the entry indicated by the type and index, mark it
 *		as clean, and don't increment its version number.
 *
 *		Do nothing if nerrors is non-zero on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              07/11/06
 *
 * Modifications:
 *
 *              None.
 *
 *****************************************************************************/

void
expunge_entry(H5C2_t * cache_ptr,
              H5F_t * file_ptr,
              int32_t idx)
{
    const char * fcn_name = "expunge_entry()";
    hbool_t in_cache;
    herr_t result;
    struct datum * entry_ptr;

    HDassert( cache_ptr );
    HDassert( file_ptr );
    HDassert( ( 0 <= idx ) && ( idx < NUM_DATA_ENTRIES ) );
    HDassert( idx < virt_num_data_entries );

    entry_ptr = &(data[idx]);

    HDassert( !(entry_ptr->locked) );
    HDassert( !(entry_ptr->global_pinned) );
    HDassert( !(entry_ptr->local_pinned) );

    entry_ptr->dirty = FALSE;

    if ( nerrors == 0 ) {

        result = H5AC2_expunge_entry(file_ptr, -1, &(types[0]), 
			            entry_ptr->header.addr);

        if ( result < 0 ) {

            nerrors++;
            if ( verbose ) {
	        HDfprintf(stdout, "%d:%s: Error in H5AC2_expunge_entry().\n",
	                  world_mpi_rank, fcn_name);
            }
        }

        HDassert( ((entry_ptr->header).type)->id == DATUM_ENTRY_TYPE );
	HDassert( ! ((entry_ptr->header).is_dirty) );

	result = H5C2_get_entry_status(cache_ptr, entry_ptr->base_addr,
				      NULL, &in_cache, NULL, NULL, NULL);

	if ( result < 0 ) {

            nerrors++;
            if ( verbose ) {
	        HDfprintf(stdout, "%d:%s: Error in H5C2_get_entry_status().\n",
	                  world_mpi_rank, fcn_name);
            }
        } else if ( in_cache ) {

            nerrors++;
            if ( verbose ) {
	        HDfprintf(stdout, "%d:%s: Expunged entry still in cache?!?\n",
	                  world_mpi_rank, fcn_name);
            }
        }
    }

    return;

} /* expunge_entry() */


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
 *              JRM -- 8/11/06
 *              Updated code to reflect the fact that entries can now be
 *              inserted pinned.  Note that since all inserts are dirty,
 *              any pins must be global pins.
 *
 *****************************************************************************/

void
insert_entry(H5C2_t * cache_ptr,
             H5F_t * file_ptr,
             int32_t idx,
             unsigned int flags)
{
    const char * fcn_name = "insert_entry()";
    hbool_t insert_pinned;
    herr_t result;
    struct datum * entry_ptr;

    HDassert( cache_ptr );
    HDassert( file_ptr );
    HDassert( ( 0 <= idx ) && ( idx < NUM_DATA_ENTRIES ) );
    HDassert( idx < virt_num_data_entries );

    entry_ptr = &(data[idx]);

    HDassert( !(entry_ptr->locked) );

    insert_pinned = ((flags & H5C2__PIN_ENTRY_FLAG) != 0 );

    if ( nerrors == 0 ) {

        (entry_ptr->ver)++;
        entry_ptr->dirty = TRUE;

        result = H5AC2_set(file_ptr, H5P_DATASET_XFER_DEFAULT, &(types[0]), 
                           entry_ptr->base_addr, entry_ptr->local_len,
                          (void *)(&(entry_ptr->header)), flags);

        if ( ( result < 0 ) ||
             ( entry_ptr->header.type != &(types[0]) ) ||
             ( entry_ptr->len != entry_ptr->header.size ) ||
             ( entry_ptr->base_addr != entry_ptr->header.addr ) ) {

            nerrors++;
            if ( verbose ) {
	        HDfprintf(stdout, "%d:%s: Error in H5AC2_set().\n",
	                  world_mpi_rank, fcn_name);
            }
        }

        if ( ! (entry_ptr->header.is_dirty) ) {

	    /* it is possible that we just exceeded the dirty bytes
	     * threshold, triggering a write of the newly inserted
	     * entry.  Test for this, and only flag an error if this
	     * is not the case.
	     */

	    struct H5AC2_aux_t * aux_ptr;

	    aux_ptr = ((H5AC2_aux_t *)(cache_ptr->aux_ptr));

	    if ( ! ( ( aux_ptr != NULL ) &&
		     ( aux_ptr->magic == H5AC2__H5AC2_AUX_T_MAGIC ) &&
		     ( aux_ptr->dirty_bytes == 0 ) ) ) {

                nerrors++;
                if ( verbose ) {
	            HDfprintf(stdout, "%d:%s: data[%d].header.is_dirty = %d.\n",
	                      world_mpi_rank, fcn_name, idx,
                              (int)(data[idx].header.is_dirty));
		}
            }
        }

        if ( insert_pinned ) {

            HDassert( entry_ptr->header.is_pinned );
            entry_ptr->global_pinned = TRUE;
	    global_pins++;

        } else {

            HDassert( ! ( entry_ptr->header.is_pinned ) );
            entry_ptr->global_pinned = FALSE;

        }

        /* HDassert( entry_ptr->header.is_dirty ); */
        HDassert( ((entry_ptr->header).type)->id == DATUM_ENTRY_TYPE );
    }

    return;

} /* insert_entry() */


/*****************************************************************************
 * Function:    local_pin_and_unpin_random_entries()
 *
 * Purpose:     Pin a random number of randomly selected entries in cache, and
 *              then unpin a random number of entries.
 *
 *              Do nothing if nerrors is non-zero on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              4/12/06
 *
 * Modifications:
 *
 *****************************************************************************/

void
local_pin_and_unpin_random_entries(H5C2_t * cache_ptr,
                                   H5F_t * file_ptr,
                                   int min_idx,
                                   int max_idx,
				   int min_count,
				   int max_count)
{
    /* const char * fcn_name = "local_pin_and_unpin_random_entries()"; */

    if ( nerrors == 0 ) {

        hbool_t via_unprotect;
        int count;
        int i;
        int idx;

        HDassert( cache_ptr );
        HDassert( file_ptr );
        HDassert( 0 <= min_idx );
        HDassert( min_idx < max_idx );
        HDassert( max_idx < NUM_DATA_ENTRIES );
        HDassert( max_idx < virt_num_data_entries );
	HDassert( 0 <= min_count );
	HDassert( min_count < max_count );

	count = (HDrand() % (max_count - min_count)) + min_count;

	HDassert( min_count <= count );
	HDassert( count <= max_count );

	for ( i = 0; i < count; i++ )
	{
            local_pin_random_entry(cache_ptr, file_ptr, min_idx, max_idx);
	}

	count = (HDrand() % (max_count - min_count)) + min_count;

	HDassert( min_count <= count );
	HDassert( count <= max_count );

        i = 0;
	idx = 0;

	while ( ( i < count ) && ( idx >= 0 ) )
	{
	    via_unprotect = ( (((unsigned)i) & 0x0001) == 0 );
	    idx = local_unpin_next_pinned_entry(cache_ptr, file_ptr,
			                        idx, via_unprotect);
	    i++;
	}
    }

    return;

} /* local_pin_and_unpin_random_entries() */


/*****************************************************************************
 * Function:    local_pin_random_entry()
 *
 * Purpose:     Pin a randomly selected entry in cache, and mark the entry
 *              as being locally pinned.  Since this entry will not in
 *              general be pinned in any other cache, we can't mark it
 *              dirty.
 *
 *              Do nothing if nerrors is non-zero on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              4/12/06
 *
 * Modifications:
 *
 *****************************************************************************/

void
local_pin_random_entry(H5C2_t * cache_ptr,
                       H5F_t * file_ptr,
                       int min_idx,
                       int max_idx)
{
    /* const char * fcn_name = "local_pin_random_entry()"; */
    int idx;

    if ( nerrors == 0 ) {

        HDassert( cache_ptr );
        HDassert( file_ptr );
        HDassert( 0 <= min_idx );
        HDassert( min_idx < max_idx );
        HDassert( max_idx < NUM_DATA_ENTRIES );
        HDassert( max_idx < virt_num_data_entries );

	do
	{
       	    idx = (HDrand() % (max_idx - min_idx)) + min_idx;
            HDassert( min_idx <= idx );
            HDassert( idx <= max_idx );
	}
	while ( data[idx].global_pinned || data[idx].local_pinned );

        pin_entry(cache_ptr, file_ptr, idx, FALSE, FALSE);
    }

    return;

} /* local_pin_random_entry() */


/*****************************************************************************
 * Function:    local_unpin_all_entries()
 *
 * Purpose:     Unpin all local pinned entries.
 *
 *              Do nothing if nerrors is non-zero on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              4/12/06
 *
 * Modifications:
 *
 *****************************************************************************/

void
local_unpin_all_entries(H5C2_t * cache_ptr,
                        H5F_t * file_ptr,
			hbool_t via_unprotect)
{
    /* const char * fcn_name = "local_unpin_all_entries()"; */

    if ( nerrors == 0 ) {

        int idx;

        HDassert( cache_ptr );
        HDassert( file_ptr );

	idx = 0;

	while ( idx >= 0 )
	{
	    idx = local_unpin_next_pinned_entry(cache_ptr, file_ptr,
			                        idx, via_unprotect);
	}
    }

    return;

} /* local_unpin_all_entries() */


/*****************************************************************************
 * Function:    local_unpin_next_pinned_entry()
 *
 * Purpose:     Find the next locally pinned entry after the specified
 * 		starting point, and unpin it.
 *
 *              Do nothing if nerrors is non-zero on entry.
 *
 * Return:      Index of the unpinned entry if there is one, or -1 if
 *              nerrors is non-zero on entry, or if there is no locally
 *              pinned entry.
 *
 * Programmer:  John Mainzer
 *              4/12/06
 *
 * Modifications:
 *
 *****************************************************************************/

int
local_unpin_next_pinned_entry(H5C2_t * cache_ptr,
                              H5F_t * file_ptr,
                              int start_idx,
			      hbool_t via_unprotect)
{
    /* const char * fcn_name = "local_unpin_next_pinned_entry()"; */
    int i = 0;
    int idx = -1;

    if ( nerrors == 0 ) {

        HDassert( cache_ptr );
        HDassert( file_ptr );
        HDassert( 0 <= start_idx );
        HDassert( start_idx < NUM_DATA_ENTRIES );
        HDassert( start_idx < virt_num_data_entries );

	idx = start_idx;

	while ( ( i < virt_num_data_entries ) &&
		( ! ( data[idx].local_pinned ) ) )
	{
	    i++;
	    idx++;
	    if ( idx >= virt_num_data_entries ) {
		idx = 0;
	    }
	}

	if ( data[idx].local_pinned ) {

	    unpin_entry(cache_ptr, file_ptr, idx, FALSE, FALSE, via_unprotect);

	} else {

	    idx = -1;
	}
    }

    return(idx);

} /* local_unpin_next_pinned_entry() */


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
lock_and_unlock_random_entries(H5C2_t * cache_ptr,
                               H5F_t * file_ptr,
                               int min_idx,
                               int max_idx,
                               int min_count,
                               int max_count)
{
    /* const char * fcn_name = "lock_and_unlock_random_entries()"; */
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
lock_and_unlock_random_entry(H5C2_t * cache_ptr,
                             H5F_t * file_ptr,
                             int min_idx,
                             int max_idx)
{
    /* const char * fcn_name = "lock_and_unlock_random_entry()"; */
    int idx;

    if ( nerrors == 0 ) {

        HDassert( cache_ptr );
        HDassert( file_ptr );
        HDassert( 0 <= min_idx );
        HDassert( min_idx < max_idx );
        HDassert( max_idx < NUM_DATA_ENTRIES );
        HDassert( max_idx < virt_num_data_entries );

        idx = (HDrand() % (max_idx - min_idx)) + min_idx;

        HDassert( min_idx <= idx );
        HDassert( idx <= max_idx );

	lock_entry(cache_ptr, file_ptr, idx);
	unlock_entry(cache_ptr, file_ptr, idx, H5AC2__NO_FLAGS_SET);
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
 * 		JRM -- 7/11/06
 * 		Modified asserts to handle the new local_len field in 
 * 		datum.
 *
 *****************************************************************************/

void
lock_entry(H5C2_t * cache_ptr,
           H5F_t * file_ptr,
           int32_t idx)
{
    const char * fcn_name = "lock_entry()";
    struct datum * entry_ptr;
    H5C2_cache_entry_t * cache_entry_ptr;

    if ( nerrors == 0 ) {

        HDassert( cache_ptr );
        HDassert( ( 0 <= idx ) && ( idx < NUM_DATA_ENTRIES ) );
        HDassert( idx < virt_num_data_entries );

        entry_ptr = &(data[idx]);

	HDassert( ! (entry_ptr->locked) );

        cache_entry_ptr = H5AC2_protect(file_ptr, H5P_DATASET_XFER_DEFAULT, 
                                        &(types[0]), entry_ptr->base_addr,
                                        entry_ptr->local_len, NULL, H5AC2_WRITE);

        if ( ( cache_entry_ptr != (void *)(&(entry_ptr->header)) ) ||
             ( entry_ptr->header.type != &(types[0]) ) ||
             ( ( entry_ptr->len != entry_ptr->header.size ) &&
	       ( entry_ptr->local_len != entry_ptr->header.size ) ) ||
             ( entry_ptr->base_addr != entry_ptr->header.addr ) ) {

            nerrors++;
            if ( verbose ) {
		HDfprintf(stdout, "%d:%s: error in H5AC2_protect().\n",
	                  world_mpi_rank, fcn_name);
            }
        } else {

	    entry_ptr->locked = TRUE;

	}

        HDassert( ((entry_ptr->header).type)->id == DATUM_ENTRY_TYPE );
    }

    return;

} /* lock_entry() */


/*****************************************************************************
 * Function:    mark_pinned_entry_dirty()
 *
 * Purpose:     Mark dirty the entry indicated by the index,
 *
 *              Do nothing if nerrors is non-zero on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              4/14/06
 *
 * Modifications:
 *
 *****************************************************************************/

void
mark_pinned_entry_dirty(H5C2_t * cache_ptr,
                        H5F_t * file_ptr,
                        int32_t idx,
	                hbool_t size_changed,
	                size_t new_size)
{
    const char * fcn_name = "mark_pinned_entry_dirty()";
    herr_t result;
    struct datum * entry_ptr;

    if ( nerrors == 0 ) {

        HDassert( file_ptr );
        HDassert( cache_ptr );
        HDassert( ( 0 <= idx ) && ( idx < NUM_DATA_ENTRIES ) );
        HDassert( idx < virt_num_data_entries );

        entry_ptr = &(data[idx]);

	HDassert ( entry_ptr->global_pinned );
	HDassert ( ! (entry_ptr->local_pinned) );

        (entry_ptr->ver)++;
        entry_ptr->dirty = TRUE;

	result = H5AC2_mark_pinned_entry_dirty(file_ptr,
	                                      (void *)entry_ptr,
                                              size_changed,
                                              new_size);

        if ( result < 0 ) {

            nerrors++;
            if ( verbose ) {
	        HDfprintf(stdout,
                          "%d:%s: error in H5AC2_mark_pinned_entry_dirty().\n",
                          world_mpi_rank, fcn_name);
            }
        }
	else
	{
	    global_dirty_pins++;
	}
    }

    return;

} /* mark_pinned_entry_dirty() */


/*****************************************************************************
 * Function:    mark_pinned_or_protected_entry_dirty()
 *
 * Purpose:     Use the H5AC2_mark_pinned_or_protected_entry_dirty() call to
 * 		mark dirty the entry indicated by the index,
 *
 *              Do nothing if nerrors is non-zero on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              5/18/06
 *
 * Modifications:
 *
 *****************************************************************************/

void
mark_pinned_or_protected_entry_dirty(H5C2_t * cache_ptr,
                                     H5F_t * file_ptr,
                                     int32_t idx)
{
    const char * fcn_name = "mark_pinned_or_protected_entry_dirty()";
    herr_t result;
    struct datum * entry_ptr;

    if ( nerrors == 0 ) {

        HDassert( file_ptr );
        HDassert( cache_ptr );
        HDassert( ( 0 <= idx ) && ( idx < NUM_DATA_ENTRIES ) );
        HDassert( idx < virt_num_data_entries );

        entry_ptr = &(data[idx]);

	HDassert ( entry_ptr->locked || entry_ptr->global_pinned );

        (entry_ptr->ver)++;
        entry_ptr->dirty = TRUE;

	result = H5AC2_mark_pinned_or_protected_entry_dirty(file_ptr,
	                                                   (void *)entry_ptr);

        if ( result < 0 ) {

            nerrors++;
            if ( verbose ) {
	        HDfprintf(stdout, "%d:%s: error in %s.\n",
                          world_mpi_rank, fcn_name,
			  "H5AC2_mark_pinned_or_protected_entry_dirty()");
            }
        }
	else if ( ! ( entry_ptr->locked ) )
	{
	    global_dirty_pins++;
	}
    }

    return;

} /* mark_pinned_or_protected_entry_dirty() */


/*****************************************************************************
 * Function:    pin_entry()
 *
 * Purpose:     Pin the entry indicated by the index.
 *
 *              Do nothing if nerrors is non-zero on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              4/11/06
 *
 * Modifications:
 *
 *****************************************************************************/

void
pin_entry(H5C2_t * cache_ptr,
          H5F_t * file_ptr,
          int32_t idx,
	  hbool_t global,
	  hbool_t dirty)
{
    /* const char * fcn_name = "pin_entry()"; */
    unsigned int flags = H5AC2__PIN_ENTRY_FLAG;
    struct datum * entry_ptr;

    if ( nerrors == 0 ) {

        HDassert( cache_ptr );
        HDassert( file_ptr );
        HDassert( ( 0 <= idx ) && ( idx < NUM_DATA_ENTRIES ) );
        HDassert( idx < virt_num_data_entries );

        entry_ptr = &(data[idx]);

	HDassert ( ! (entry_ptr->global_pinned) );
	HDassert ( ! (entry_ptr->local_pinned) );
	HDassert ( ! ( dirty && ( ! global ) ) );

	lock_entry(cache_ptr, file_ptr, idx);

	if ( dirty ) {

	    flags |= H5AC2__DIRTIED_FLAG;
	}

	unlock_entry(cache_ptr, file_ptr, idx, flags);

        HDassert( (entry_ptr->header).is_pinned );
	HDassert( ( ! dirty ) || ( (entry_ptr->header).is_dirty ) );

	if ( global ) {

	    entry_ptr->global_pinned = TRUE;

	    global_pins++;

	} else {

	    entry_ptr->local_pinned = TRUE;

	    local_pins++;

	}
    }

    return;

} /* pin_entry() */


/*****************************************************************************
 * Function:    pin_protected_entry()
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
pin_protected_entry(H5C2_t * cache_ptr,
                    H5F_t * file_ptr,
                    int32_t idx,
		    hbool_t global)
{
    const char * fcn_name = "pin_protected_entry()";
    herr_t result;
    struct datum * entry_ptr;

    HDassert( cache_ptr );
    HDassert( file_ptr );
    HDassert( ( 0 <= idx ) && ( idx < NUM_DATA_ENTRIES ) );
    HDassert( idx < virt_num_data_entries );

    entry_ptr = &(data[idx]);

    HDassert( entry_ptr->locked );

    if ( nerrors == 0 ) {

	result = H5AC2_pin_protected_entry(file_ptr, (void *)entry_ptr);

        if ( ( result < 0 ) ||
             ( entry_ptr->header.type != &(types[0]) ) ||
             ( ( entry_ptr->len != entry_ptr->header.size ) &&
	       ( entry_ptr->local_len != entry_ptr->header.size ) )||
             ( entry_ptr->base_addr != entry_ptr->header.addr ) ||
	     ( ! ( (entry_ptr->header).is_pinned ) ) ) {

            nerrors++;
            if ( verbose ) {
	        HDfprintf(stdout, 
			  "%d:%s: Error in H5AC2_pin_protected entry().\n",
	                  world_mpi_rank, fcn_name);
            }
        }

        if ( global ) {

	    entry_ptr->global_pinned = TRUE;

	    global_pins++;

	} else {

	    entry_ptr->local_pinned = TRUE;
 
	    local_pins++;

	}

        HDassert( ((entry_ptr->header).type)->id == DATUM_ENTRY_TYPE );
    }

    return;

} /* pin_protected_entry() */


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
 *              7/11/06 -- JRM 
 *              Added support for the phony_len field in datum.
 *
 *****************************************************************************/

void
rename_entry(H5C2_t * cache_ptr,
             H5F_t * file_ptr,
             int32_t old_idx,
             int32_t new_idx)
{
    const char   * fcn_name = "rename_entry()";
    herr_t         result;
    int		   tmp;
    size_t	   tmp_len;
    haddr_t        old_addr = HADDR_UNDEF;
    haddr_t        new_addr = HADDR_UNDEF;
    struct datum * old_entry_ptr;
    struct datum * new_entry_ptr;

    if ( ( nerrors == 0 ) && ( old_idx != new_idx ) ) {

        HDassert( cache_ptr );
        HDassert( file_ptr );
        HDassert( ( 0 <= old_idx ) && ( old_idx < NUM_DATA_ENTRIES ) );
        HDassert( old_idx < virt_num_data_entries );
        HDassert( ( 0 <= new_idx ) && ( new_idx < NUM_DATA_ENTRIES ) );
        HDassert( new_idx < virt_num_data_entries );

        old_entry_ptr = &(data[old_idx]);
        new_entry_ptr = &(data[new_idx]);

        HDassert( ((old_entry_ptr->header).type)->id == DATUM_ENTRY_TYPE );
        HDassert( !(old_entry_ptr->header.is_protected) );
        HDassert( !(old_entry_ptr->locked) );
        HDassert( old_entry_ptr->len == new_entry_ptr->len );

        old_addr = old_entry_ptr->base_addr;
        new_addr = new_entry_ptr->base_addr;

        result = H5AC2_rename(file_ptr,  &(types[0]), old_addr, new_addr);

        if ( ( result < 0 ) || ( old_entry_ptr->header.addr != new_addr ) ) {

            nerrors++;
            if ( verbose ) {
		HDfprintf(stdout, "%d:%s: H5AC2_rename() failed.\n",
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

	    if ( old_entry_ptr->local_len != new_entry_ptr->local_len ) {

	        tmp_len                  = old_entry_ptr->local_len;
	        old_entry_ptr->local_len = new_entry_ptr->local_len;
	        new_entry_ptr->local_len = tmp_len;
	    }
        }
    }

    return;

} /* rename_entry() */


/*****************************************************************************
 * Function:    resize_entry()
 *
 * Purpose:     Resize the pinned entry indicated by idx to the new_size.
 * 		Note that new_size must be greater than 0, and must be
 * 		less than or equal to the original size of the entry.
 *
 *		Do nothing if nerrors isn't zero.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              7/11/06
 *
 * Modifications:
 *
 *              None
 *
 *****************************************************************************/

void
resize_entry(H5C2_t * cache_ptr,
             H5F_t * file_ptr,
             int32_t idx,
	     size_t  new_size)
{
    const char   * fcn_name = "resize_entry()";
    herr_t         result;
    struct datum * entry_ptr;

    if ( nerrors == 0 ) {

        HDassert( cache_ptr );
        HDassert( file_ptr );
        HDassert( ( 0 <= idx ) && ( idx < NUM_DATA_ENTRIES ) );
        HDassert( idx < virt_num_data_entries );

        entry_ptr = &(data[idx]);

        HDassert( ((entry_ptr->header).type)->id == DATUM_ENTRY_TYPE );
        HDassert( !(entry_ptr->header.is_protected) );
        HDassert( !(entry_ptr->locked) );
	HDassert( ( entry_ptr->global_pinned ) && 
		  ( ! entry_ptr->local_pinned ) );
	HDassert( ( entry_ptr->header.size == entry_ptr->len ) ||
	          ( entry_ptr->header.size == entry_ptr->local_len ) );
	HDassert( new_size > 0 );
	HDassert( new_size <= entry_ptr->len );

	result = H5AC2_resize_pinned_entry(file_ptr, (void *)entry_ptr, 
			                  new_size);

        if ( result < 0 ) {

            nerrors++;
            if ( verbose ) {
		HDfprintf(stdout, "%d:%s: H5AC2_rename() failed.\n",
	                  world_mpi_rank, fcn_name);
            }

        } else {

            HDassert( ((entry_ptr->header).type)->id == DATUM_ENTRY_TYPE );
            HDassert( entry_ptr->header.is_dirty );
            HDassert( entry_ptr->header.size == new_size );

            entry_ptr->dirty = TRUE;
	    entry_ptr->local_len = new_size;

            /* touch up version. */

            (entry_ptr->ver)++;
        }
    }

    return;

} /* resize_entry() */


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
                     H5C2_t ** cache_ptr_ptr)
{
    const char * fcn_name = "setup_cache_for_test()";
    hbool_t success = FALSE; /* will set to TRUE if appropriate. */
    hbool_t enable_rpt_fcn = FALSE;
    hid_t fid = -1;
    H5AC2_cache_config_t config;
    H5F_t * file_ptr = NULL;
    H5C2_t * cache_ptr = NULL;
    haddr_t actual_base_addr;

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
        file_ptr = (H5F_t *)H5I_object_verify(fid, H5I_FILE);
    }

    if ( file_ptr == NULL ) {
        nerrors++;
        if ( verbose ) {
	    HDfprintf(stdout, "%d:%s: Can't get file_ptr.\n",
                      world_mpi_rank, fcn_name);
        }
    } else {
        cache_ptr = file_ptr->shared->cache2;
    }

    if ( cache_ptr == NULL ) {
        nerrors++;
        if ( verbose ) {
	    HDfprintf(stdout, "%d:%s: Can't get cache_ptr.\n",
                      world_mpi_rank, fcn_name);
        }
    } else if ( cache_ptr->magic != H5C2__H5C2_T_MAGIC ) {
        nerrors++;
        if ( verbose ) {
	    HDfprintf(stdout, "%d:%s: Bad cache_ptr magic.\n",
                      world_mpi_rank, fcn_name);
        }
    } else {
        *fid_ptr = fid;
        *file_ptr_ptr = file_ptr;
        *cache_ptr_ptr = cache_ptr;
#if 0 /* delete this eventually */
        H5C2_set_skip_flags(cache_ptr, TRUE, TRUE);
#endif /* JRM */
        H5C2_stats__reset(cache_ptr);
        success = TRUE;
    }

    if ( ( success ) && ( enable_rpt_fcn ) ) {

        config.version = H5AC2__CURR_CACHE_CONFIG_VERSION;

        if ( H5AC2_get_cache_auto_resize_config(cache_ptr, &config)
             != SUCCEED ) {

	    HDfprintf(stdout,
                      "%d:%s: H5AC2_get_cache_auto_resize_config() failed.\n",
                      world_mpi_rank, fcn_name);

        } else {

            config.rpt_fcn_enabled = TRUE;

            if ( H5AC2_set_cache_auto_resize_config(cache_ptr, &config)
                 != SUCCEED ) {

	        HDfprintf(stdout,
                         "%d:%s: H5AC2_set_cache_auto_resize_config() failed.\n",
                          world_mpi_rank, fcn_name);
            } else {

                HDfprintf(stdout, "%d:%s: rpt_fcn enabled.\n",
                          world_mpi_rank, fcn_name);
            }
        }
    }

#if DO_SYNC_AFTER_WRITE

    if ( success ) {

	if ( H5AC2_set_write_done_callback(cache_ptr, do_sync) != SUCCEED ) {

            nerrors++;
            if ( verbose ) {
	        HDfprintf(stdout,
			  "%d:%s: H5C2_set_write_done_callback failed.\n",
                          world_mpi_rank, fcn_name);
            }
	}
    }

#endif /* DO_SYNC_AFTER_WRITE */

    if ( success ) { /* allocate space for test entries */

        actual_base_addr = H5MF_alloc(file_ptr, H5FD_MEM_DEFAULT, H5P_DEFAULT,
                                      (hsize_t)(max_addr + BASE_ADDR));

        if ( actual_base_addr == HADDR_UNDEF ) {

            success = FALSE;
	    nerrors++;

            if ( verbose ) {
	        HDfprintf(stdout, "%d:%s: H5MF_alloc() failed.\n",
                          world_mpi_rank, fcn_name);
            }

        } else if ( actual_base_addr > BASE_ADDR ) {

            /* If this happens, must increase BASE_ADDR so that the
             * actual_base_addr is <= BASE_ADDR.  This should only happen
             * if the size of the superblock is increase.
             */
            success = FALSE;
	    nerrors++;

            if ( verbose ) {
	        HDfprintf(stdout, "%d:%s: actual_base_addr > BASE_ADDR.\n",
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

    /* Get an ID for the non-blocking, collective H5AC2 dxpl */
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
         ( NULL == (xfer_plist = H5I_object(H5AC2_noblock_dxpl_id)) ) ) {

        nerrors++;
        if ( verbose ) {
            HDfprintf(stdout, "%d:%s: can't get new property list object.\n",
                      world_mpi_rank, fcn_name);
        }
    }

    /* Insert 'block before metadata write' property */
    block_before_meta_write=0;
    if ( ( nerrors == 0 ) &&
         ( H5P_insert(xfer_plist, H5AC2_BLOCK_BEFORE_META_WRITE_NAME,
                      H5AC2_BLOCK_BEFORE_META_WRITE_SIZE,
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
         ( H5P_insert(xfer_plist, H5AC2_LIBRARY_INTERNAL_NAME,
                      H5AC2_LIBRARY_INTERNAL_SIZE, &library_internal,
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
 *		JRM -- 5/9/06
 *		Modified function to facilitate setting predefined seeds.
 *
 *****************************************************************************/

void
setup_rand(void)
{
    const char * fcn_name = "setup_rand()";
    hbool_t use_predefined_seeds = FALSE;
    int num_predefined_seeds = 3;
    unsigned predefined_seeds[3] = {18669, 89925, 12577};
    unsigned seed;
    struct timeval tv;
    struct timezone tz;

    if ( ( use_predefined_seeds ) &&
         ( world_mpi_size == num_predefined_seeds ) ) {

	    HDassert( world_mpi_rank >= 0 );
	    HDassert( world_mpi_rank < world_mpi_size );

            seed = predefined_seeds[world_mpi_rank];
	    HDfprintf(stdout, "%d:%s: predefined_seed = %d.\n",
                      world_mpi_rank, fcn_name, seed);
	    fflush(stdout);
            HDsrand(seed);

    } else {

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
	    fflush(stdout);
            HDsrand(seed);
        }
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
take_down_cache(hid_t fid,
		H5C2_t * cache_ptr)
{
    const char * fcn_name = "take_down_cache()";
    hbool_t show_progress = FALSE;
    hbool_t success = FALSE; /* will set to TRUE if appropriate. */
    int mile_stone = 1;

    if ( show_progress ) { /* 1 */
        HDfprintf(stdout, "%d:%s - %0d -- success = %d\n",
                  world_mpi_rank, fcn_name, mile_stone++, (int)success);
        fflush(stdout);
    }

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

    if ( show_progress ) { /* 2 */
        HDfprintf(stdout, "%d:%s - %0d -- success = %d\n",
                  world_mpi_rank, fcn_name, mile_stone++, (int)success);
        fflush(stdout);
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
 * 		7/11/06
 * 		Updated for the new local_len field in datum.
 *
 *****************************************************************************/

void
unlock_entry(H5C2_t * cache_ptr,
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
        HDassert( idx < virt_num_data_entries );

        entry_ptr = &(data[idx]);

	HDassert( entry_ptr->locked );

        dirtied = ((flags & H5AC2__DIRTIED_FLAG) == H5AC2__DIRTIED_FLAG );

        if ( dirtied ) {

            (entry_ptr->ver)++;
            entry_ptr->dirty = TRUE;
        }

        result = H5AC2_unprotect(file_ptr, H5P_DATASET_XFER_DEFAULT, &(types[0]),
			        entry_ptr->base_addr, entry_ptr->local_len,
                                (void *)(&(entry_ptr->header)), flags);

        if ( ( result < 0 ) ||
             ( entry_ptr->header.type != &(types[0]) ) ||
             ( ( entry_ptr->len != entry_ptr->header.size ) &&
	       ( entry_ptr->local_len != entry_ptr->header.size ) ) ||
             ( entry_ptr->base_addr != entry_ptr->header.addr ) ) {

            nerrors++;
            if ( verbose ) {
                HDfprintf(stdout, "%d:%s: error in H5C2_unprotect().\n",
                          world_mpi_rank, fcn_name);
            }
        } else {

            entry_ptr->locked = FALSE;

	}

        HDassert( ((entry_ptr->header).type)->id == DATUM_ENTRY_TYPE );

        if ( ( flags & H5AC2__DIRTIED_FLAG ) != 0
                && ( (flags & H5C2__DELETED_FLAG) == 0 ) ) {

            HDassert( entry_ptr->header.is_dirty );
            HDassert( entry_ptr->dirty );
        }
    }

    return;

} /* unlock_entry() */


/*****************************************************************************
 * Function:    unpin_entry()
 *
 * Purpose:     Unpin the entry indicated by the index.
 *
 *              Do nothing if nerrors is non-zero on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              4/12/06
 *
 * Modifications:
 *
 * 		JRM -- 8/15/06
 * 		Added assertion that entry is pinned on entry.
 *
 *****************************************************************************/

void
unpin_entry(H5C2_t * cache_ptr,
            H5F_t * file_ptr,
            int32_t idx,
            hbool_t global,
            hbool_t dirty,
            hbool_t via_unprotect)
{
    const char * fcn_name = "unpin_entry()";
    herr_t result;
    unsigned int flags = H5AC2__UNPIN_ENTRY_FLAG;
    struct datum * entry_ptr;

    if ( nerrors == 0 ) {

        HDassert( cache_ptr );
        HDassert( file_ptr );
        HDassert( ( 0 <= idx ) && ( idx < NUM_DATA_ENTRIES ) );
        HDassert( idx < virt_num_data_entries );

        entry_ptr = &(data[idx]);

	HDassert( (entry_ptr->header).is_pinned );
	HDassert ( ! ( entry_ptr->global_pinned && entry_ptr->local_pinned) );
	HDassert ( ( global && entry_ptr->global_pinned ) ||
		   ( ! global && entry_ptr->local_pinned ) );
	HDassert ( ! ( dirty && ( ! global ) ) );

	if ( via_unprotect ) {

	    lock_entry(cache_ptr, file_ptr, idx);

	    if ( dirty ) {

	        flags |= H5AC2__DIRTIED_FLAG;
	    }

	    unlock_entry(cache_ptr, file_ptr, idx, flags);

	} else {

	    if ( dirty ) {

		mark_pinned_entry_dirty(cache_ptr, file_ptr, idx, FALSE, 
				        (size_t)0);

	    }

	    result = H5AC2_unpin_entry(file_ptr, (void *)entry_ptr);

	    if ( result < 0 ) {

                nerrors++;
                if ( verbose ) {
                    HDfprintf(stdout, "%d:%s: error in H5AC2_unpin_entry().\n",
	                      world_mpi_rank, fcn_name);
                }
	    }
	}

        HDassert( ! ((entry_ptr->header).is_pinned) );

	if ( global ) {

	    entry_ptr->global_pinned = FALSE;

	} else {

	    entry_ptr->local_pinned = FALSE;

	}
    }

    return;

} /* unpin_entry() */


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
 *		JRM -- 5/9/06
 *		Added code supporting the write request ack message.  This
 *		message was added to eliminate one possible cause of a
 *		bug spotted on cobalt.  If this doesn't fix the problem,
 *		it will narrow things down a bit.
 *
 *		JRM -- 5/10/06
 *		Added call to do_sync().  This is part of an attempt to
 *		optimize out the slowdown caused by the addition of the
 *		write request ack message.
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
    reset_stats();

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

        if ( ! ( success = send_mssg(&mssg, FALSE) ) ) {

            nerrors++;
            if ( verbose ) {
                HDfprintf(stdout, "%d:%s: send_mssg() failed on write.\n",
                          world_mpi_rank, fcn_name);
            }
        }

#if DO_WRITE_REQ_ACK

        /* try to receive the write ack from the server */
        if ( success ) {

            success = recv_mssg(&mssg, WRITE_REQ_ACK_CODE);

            if ( ! success ) {

                nerrors++;
                if ( verbose ) {
                    HDfprintf(stdout, "%d:%s: recv_mssg() failed.\n",
                              world_mpi_rank, fcn_name);
                }
            }
        }

        /* verify that we received the expected ack message */
        if ( success ) {

            if ( ( mssg.req != WRITE_REQ_ACK_CODE ) ||
                 ( mssg.src != world_server_mpi_rank ) ||
                 ( mssg.dest != world_mpi_rank ) ||
                 ( mssg.base_addr != data[world_mpi_rank].base_addr ) ||
                 ( mssg.len != data[world_mpi_rank].len ) ||
                 ( mssg.ver != data[world_mpi_rank].ver ) ||
                 ( mssg.magic != MSSG_MAGIC ) ) {

                success = FALSE;
                nerrors++;
                if ( verbose ) {
                    HDfprintf(stdout, "%d:%s: Bad data in write req ack.\n",
                              world_mpi_rank, fcn_name);
                }
            }
        }

#endif /* DO_WRITE_REQ_ACK */

	do_sync();

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

            success = send_mssg(&mssg, FALSE);

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

            success = recv_mssg(&mssg, READ_REQ_REPLY_CODE);

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

            success = send_mssg(&mssg, FALSE);

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
    hbool_t show_progress = FALSE;
    int i;
    int max_nerrors;
    int mile_stone = 1;
    hid_t fid = -1;
    H5F_t * file_ptr = NULL;
    H5C2_t * cache_ptr = NULL;
    struct mssg_t mssg;

    if ( world_mpi_rank == 0 ) {

        TESTING("smoke check #1");
    }

    if ( show_progress ) { /* 1 */
        HDfprintf(stdout, "%d:%s - %0d -- success = %d\n",
                  world_mpi_rank, fcn_name, mile_stone++, (int)success);
	fflush(stdout);
    }

    nerrors = 0;
    init_data();
    reset_stats();

    if ( world_mpi_rank == world_server_mpi_rank ) {

        if ( show_progress ) { /* 2s */
            HDfprintf(stdout, "%d:%s - %0ds -- success = %d\n",
                      world_mpi_rank, fcn_name, mile_stone++, (int)success);
	    fflush(stdout);
        }

	if ( ! server_main() ) {

            /* some error occured in the server -- report failure */
            nerrors++;
            if ( verbose ) {
		HDfprintf(stdout, "%d:%s: server_main() failed.\n",
                          world_mpi_rank, fcn_name);
            }
        }

        if ( show_progress ) { /* 3s */
            HDfprintf(stdout, "%d:%s - %0ds -- success = %d\n",
                      world_mpi_rank, fcn_name, mile_stone++, (int)success);
	    fflush(stdout);
        }
    }
    else /* run the clients */
    {

        if ( show_progress ) { /* 2 */
            HDfprintf(stdout, "%d:%s - %0d -- success = %d\n",
                      world_mpi_rank, fcn_name, mile_stone++, (int)success);
	    fflush(stdout);
        }

        if ( ! setup_cache_for_test(&fid, &file_ptr, &cache_ptr) ) {

            nerrors++;
            fid = -1;
            cache_ptr = NULL;
            if ( verbose ) {
		HDfprintf(stdout, "%d:%s: setup_cache_for_test() failed.\n",
                          world_mpi_rank, fcn_name);
            }
        }

        if ( show_progress ) { /* 3 */
            HDfprintf(stdout, "%d:%s - %0d -- success = %d\n",
                      world_mpi_rank, fcn_name, mile_stone++, (int)success);
	    fflush(stdout);
        }

        for ( i = 0; i < (virt_num_data_entries / 2); i++ )
        {
            insert_entry(cache_ptr, file_ptr, i, H5AC2__NO_FLAGS_SET);
        }

        if ( show_progress ) { /* 4 */
            HDfprintf(stdout, "%d:%s - %0d -- success = %d\n",
                      world_mpi_rank, fcn_name, mile_stone++, (int)success);
	    fflush(stdout);
        }

        for ( i = (virt_num_data_entries / 2) - 1; i >= 0; i-- )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC2__NO_FLAGS_SET);
        }

        if ( show_progress ) { /* 5 */
            HDfprintf(stdout, "%d:%s - %0d -- success = %d\n",
                      world_mpi_rank, fcn_name, mile_stone++, (int)success);
	    fflush(stdout);
        }

        /* rename the first half of the entries... */
        for ( i = 0; i < (virt_num_data_entries / 2); i++ )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC2__NO_FLAGS_SET);
	    rename_entry(cache_ptr, file_ptr, i,
			 (i + (virt_num_data_entries / 2)));
        }

        if ( show_progress ) { /* 6 */
            HDfprintf(stdout, "%d:%s - %0d -- success = %d\n",
                      world_mpi_rank, fcn_name, mile_stone++, (int)success);
	    fflush(stdout);
        }

        /* ...and then rename them back. */
        for ( i = (virt_num_data_entries / 2) - 1; i >= 0; i-- )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC2__NO_FLAGS_SET);
	    rename_entry(cache_ptr, file_ptr, i,
			 (i + (virt_num_data_entries / 2)));
        }

        if ( show_progress ) { /* 7 */
            HDfprintf(stdout, "%d:%s - %0d -- success = %d\n",
                      world_mpi_rank, fcn_name, mile_stone++, (int)success);
	    fflush(stdout);
        }

        if ( fid >= 0 ) {

            if ( ! take_down_cache(fid, cache_ptr) ) {

                nerrors++;
                if ( verbose ) {
		    HDfprintf(stdout, "%d:%s: take_down_cache() failed.\n",
                              world_mpi_rank, fcn_name);
                }
            }
        }

        if ( show_progress ) { /* 8 */
            HDfprintf(stdout, "%d:%s - %0d -- success = %d\n",
                      world_mpi_rank, fcn_name, mile_stone++, (int)success);
	    fflush(stdout);
        }

        /* verify that all instance of datum are back where the started
         * and are clean.
         */

        for ( i = 0; i < NUM_DATA_ENTRIES; i++ )
        {
            HDassert( data_index[i] == i );
            HDassert( ! (data[i].dirty) );
        }

        if ( show_progress ) { /* 9 */
            HDfprintf(stdout, "%d:%s - %0d -- success = %d\n",
                      world_mpi_rank, fcn_name, mile_stone++, (int)success);
	    fflush(stdout);
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

            success = send_mssg(&mssg, FALSE);

            if ( ! success ) {

                nerrors++;
                if ( verbose ) {
                    HDfprintf(stdout, "%d:%s: send_mssg() failed on done.\n",
                              world_mpi_rank, fcn_name);
                }
            }
        }

        if ( show_progress ) { /* 10 */
            HDfprintf(stdout, "%d:%s - %0d -- success = %d\n",
                      world_mpi_rank, fcn_name, mile_stone++, (int)success);
	    fflush(stdout);
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
 *		JRM -- 4/13/06
 *		Added pinned entry tests.
 *
 *		JRM -- 4/28/06
 *		Modified test to rename pinned entries.
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
    H5C2_t * cache_ptr = NULL;
    struct mssg_t mssg;

    if ( world_mpi_rank == 0 ) {

        TESTING("smoke check #2");
    }

    nerrors = 0;
    init_data();
    reset_stats();

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

        for ( i = 0; i < (virt_num_data_entries / 2); i++ )
        {
            insert_entry(cache_ptr, file_ptr, i, H5AC2__NO_FLAGS_SET);

            if ( i > 100 ) {

		lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                               (i - 100), i, 0, 10);
            }
        }

	for ( i = 0; i < (virt_num_data_entries / 2); i+=61 )
	{
	    /* Make sure we don't step on any locally pinned entries */
	    if ( data[i].local_pinned ) {
		unpin_entry(cache_ptr, file_ptr, i, FALSE, FALSE, FALSE);
	    }

	    pin_entry(cache_ptr, file_ptr, i, TRUE, FALSE);
	}

        for ( i = (virt_num_data_entries / 2) - 1; i >= 0; i-=2 )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC2__NO_FLAGS_SET);
	    lock_and_unlock_random_entries(cache_ptr, file_ptr, 0,
			                   (virt_num_data_entries / 20),
					   0, 100);
	    local_pin_and_unpin_random_entries(cache_ptr, file_ptr, 0,
			                       (virt_num_data_entries / 4),
					       0, 3);
        }

        for ( i = 0; i < (virt_num_data_entries / 2); i+=2 )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC2__DIRTIED_FLAG);
	    lock_and_unlock_random_entries(cache_ptr, file_ptr, 0,
			                   (virt_num_data_entries / 10),
					   0, 100);
        }

	/* we can't rename pinned entries, so release any local pins now. */
	local_unpin_all_entries(cache_ptr, file_ptr, FALSE);

        /* rename the first half of the entries... */
        for ( i = 0; i < (virt_num_data_entries / 2); i++ )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC2__NO_FLAGS_SET);
	    rename_entry(cache_ptr, file_ptr, i,
			 (i + (virt_num_data_entries / 2)));
	    lock_and_unlock_random_entries(cache_ptr, file_ptr, 0,
			                   ((virt_num_data_entries / 50) - 1),
                                           0, 100);
        }

        /* ...and then rename them back. */
        for ( i = (virt_num_data_entries / 2) - 1; i >= 0; i-- )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC2__DIRTIED_FLAG);
	    rename_entry(cache_ptr, file_ptr, i,
			 (i + (virt_num_data_entries / 2)));
	    lock_and_unlock_random_entries(cache_ptr, file_ptr, 0,
			                   (virt_num_data_entries / 100),
					   0, 100);
        }

	for ( i = 0; i < (virt_num_data_entries / 2); i+=61 )
	{
	    hbool_t via_unprotect = ( (((unsigned)i) & 0x01) == 0 );
	    hbool_t dirty = ( (((unsigned)i) & 0x02) == 0 );

	    unpin_entry(cache_ptr, file_ptr, i, TRUE, dirty, via_unprotect);
	}

        if ( fid >= 0 ) {

            if ( ! take_down_cache(fid, cache_ptr) ) {

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

            success = send_mssg(&mssg, FALSE);

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
 *		Added pinned entry tests.		JRM - 4/14/06
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
    H5C2_t * cache_ptr = NULL;
    struct mssg_t mssg;

    if ( world_mpi_rank == 0 ) {

        TESTING("smoke check #3");
    }

    nerrors = 0;
    init_data();
    reset_stats();

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

        for ( i = 0; i < (virt_num_data_entries / 4); i++ )
        {
            insert_entry(cache_ptr, file_ptr, i, H5AC2__NO_FLAGS_SET);

            if ( i > 100 ) {

		lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                               (i - 100), i,
                                               min_count, max_count);
            }
        }


        min_count = 100 / ((file_mpi_rank + 2) * (file_mpi_rank + 2));
        max_count = min_count + 50;

        for ( i = (virt_num_data_entries / 4);
	      i < (virt_num_data_entries / 2);
	      i++ )
        {

            insert_entry(cache_ptr, file_ptr, i, H5AC2__NO_FLAGS_SET);

	    if ( i % 59 == 0 ) {

		hbool_t dirty = ( (i % 2) == 0);

		if ( data[i].local_pinned ) {
		    unpin_entry(cache_ptr, file_ptr, i, FALSE, FALSE, FALSE);
		}

		pin_entry(cache_ptr, file_ptr, i, TRUE, dirty);

	        HDassert( !dirty || data[i].header.is_dirty );
	        HDassert( data[i].header.is_pinned );
	        HDassert( data[i].global_pinned );
	        HDassert( ! data[i].local_pinned );
	    }

            if ( i > 100 ) {

		lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                               (i - 100), i,
                                               min_count, max_count);
            }

	    local_pin_and_unpin_random_entries(cache_ptr, file_ptr,
                                               0, virt_num_data_entries / 4,
					       0, (file_mpi_rank + 2));

	}


	/* flush the file to be sure that we have no problems flushing
	 * pinned entries
	 */
        if ( H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0 ) {
            nerrors++;
            if ( verbose ) {
	        HDfprintf(stdout, "%d:%s: H5Fflush() failed.\n",
                          world_mpi_rank, fcn_name);
            }
        }


        min_idx = 0;
        max_idx = ((virt_num_data_entries / 10) /
                   ((file_mpi_rank + 1) * (file_mpi_rank + 1))) - 1;
        if ( max_idx <= min_idx ) {

            max_idx = min_idx + 10;
        }

        for ( i = (virt_num_data_entries / 2) - 1; i >= 0; i-- )
        {
	    if ( ( i >= (virt_num_data_entries / 4) ) && ( i % 59 == 0 ) ) {

                hbool_t via_unprotect = ( (((unsigned)i) & 0x02) == 0 );
	        hbool_t dirty = ( (((unsigned)i) & 0x04) == 0 );

		HDassert( data[i].global_pinned );
		HDassert( ! data[i].local_pinned );

		unpin_entry(cache_ptr, file_ptr, i, TRUE, dirty,
			    via_unprotect);
	    }
	    if ( i % 2 == 0 ) {

	        lock_entry(cache_ptr, file_ptr, i);
	        unlock_entry(cache_ptr, file_ptr, i, H5AC2__NO_FLAGS_SET);
	        local_pin_and_unpin_random_entries(cache_ptr, file_ptr, 0,
				                   virt_num_data_entries / 2,
						   0, 2);
	        lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                               min_idx, max_idx, 0, 100);
	    }
        }

        min_idx = 0;
        max_idx = ((virt_num_data_entries / 10) /
                   ((file_mpi_rank + 3) * (file_mpi_rank + 3))) - 1;
        if ( max_idx <= min_idx ) {

            max_idx = min_idx + 10;
        }

        for ( i = 0; i < (virt_num_data_entries / 2); i+=2 )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC2__DIRTIED_FLAG);
	    lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                           min_idx, max_idx, 0, 100);
        }

        /* we can't rename pinned entries, so release any local pins now. */
        local_unpin_all_entries(cache_ptr, file_ptr, FALSE);

        min_count = 10 / (file_mpi_rank + 1);
        max_count = min_count + 100;

        /* rename the first half of the entries... */
        for ( i = 0; i < (virt_num_data_entries / 2); i++ )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC2__NO_FLAGS_SET);
	    rename_entry(cache_ptr, file_ptr, i,
			 (i + (virt_num_data_entries / 2)));
	    lock_and_unlock_random_entries(cache_ptr, file_ptr, 0,
			                   (virt_num_data_entries / 20),
                                           min_count, max_count);
        }

        /* ...and then rename them back. */
        for ( i = (virt_num_data_entries / 2) - 1; i >= 0; i-- )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC2__DIRTIED_FLAG);
	    rename_entry(cache_ptr, file_ptr, i,
			 (i + (virt_num_data_entries / 2)));
	    lock_and_unlock_random_entries(cache_ptr, file_ptr, 0,
			                   (virt_num_data_entries / 40),
                                           min_count, max_count);
        }

        /* finally, do some dirty lock/unlocks while we give the cache
         * a chance t reduce its size.
         */
        min_count = 200 / ((file_mpi_rank + 1) * (file_mpi_rank + 1));
        max_count = min_count + 100;

        for ( i = 0; i < (virt_num_data_entries / 2); i+=2 )
        {
	    local_pin_and_unpin_random_entries(cache_ptr, file_ptr, 0,
			                       (virt_num_data_entries / 2),
					       0, 5);

	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC2__DIRTIED_FLAG);

            if ( i > 100 ) {

		lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                               (i - 100), i,
                                               min_count, max_count);
            }
        }

        /* release any local pins before we take down the cache. */
        local_unpin_all_entries(cache_ptr, file_ptr, FALSE);

        if ( fid >= 0 ) {

            if ( ! take_down_cache(fid, cache_ptr) ) {

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


            success = send_mssg(&mssg, FALSE);

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
 *		Added code testing pinned insertion of entries.
 *
 *							JRM - 8/15/06
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
    H5C2_t * cache_ptr = NULL;
    struct mssg_t mssg;

    if ( world_mpi_rank == 0 ) {

        TESTING("smoke check #4");
    }

    nerrors = 0;
    init_data();
    reset_stats();

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

        for ( i = 0; i < (virt_num_data_entries / 4); i++ )
        {
            insert_entry(cache_ptr, file_ptr, i, H5AC2__NO_FLAGS_SET);

            if ( i > 100 ) {

		lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                               (i - 100), i,
                                               min_count, max_count);
            }
        }

        min_count = 10 * (file_mpi_rank % 4);
        max_count = min_count + 100;

        for ( i = (virt_num_data_entries / 4);
	      i < (virt_num_data_entries / 2);
	      i++ )
        {
	    if ( i % 2 == 0 ) {

                insert_entry(cache_ptr, file_ptr, i, H5AC2__NO_FLAGS_SET);

	    } else {

		/* Insert some entries pinned, and then unpin them
		 * immediately.  We have tested pinned entries elsewhere,
		 * so it should be sufficient to verify that the 
		 * entries are in fact pinned (which unpin_entry() should do).
		 */
                insert_entry(cache_ptr, file_ptr, i, H5C2__PIN_ENTRY_FLAG);
                unpin_entry(cache_ptr, file_ptr, i, TRUE, FALSE, FALSE);
	    }

            if ( i % 59 == 0 ) {

                hbool_t dirty = ( (i % 2) == 0);

                if ( data[i].local_pinned ) {
                    unpin_entry(cache_ptr, file_ptr, i, FALSE, FALSE, FALSE);
                }

                pin_entry(cache_ptr, file_ptr, i, TRUE, dirty);

                HDassert( !dirty || data[i].header.is_dirty );
                HDassert( data[i].header.is_pinned );
                HDassert( data[i].global_pinned );
                HDassert( ! data[i].local_pinned );
            }

            if ( i > 100 ) {

		lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                               (i - 100), i,
                                               min_count, max_count);
            }

            local_pin_and_unpin_random_entries(cache_ptr, file_ptr, 0,
			                       (virt_num_data_entries / 4),
                                               0, (file_mpi_rank + 2));
        }


        /* flush the file to be sure that we have no problems flushing
	 * pinned entries
	 */
        if ( H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0 ) {
            nerrors++;
            if ( verbose ) {
                HDfprintf(stdout, "%d:%s: H5Fflush() failed.\n",
                          world_mpi_rank, fcn_name);
            }
        }


        min_idx = 0;
        max_idx = (((virt_num_data_entries / 10) / 4) *
                   ((file_mpi_rank % 4) + 1)) - 1;

        for ( i = (virt_num_data_entries / 2) - 1; i >= 0; i-- )
        {
            if ( ( i >= (virt_num_data_entries / 4) ) && ( i % 59 == 0 ) ) {

                hbool_t via_unprotect = ( (((unsigned)i) & 0x02) == 0 );
                hbool_t dirty = ( (((unsigned)i) & 0x04) == 0 );

                HDassert( data[i].global_pinned );
                HDassert( ! data[i].local_pinned );

                unpin_entry(cache_ptr, file_ptr, i, TRUE, dirty, via_unprotect);
            }

	    if ( i % 2 == 0 ) {

	        lock_entry(cache_ptr, file_ptr, i);
	        unlock_entry(cache_ptr, file_ptr, i, H5AC2__NO_FLAGS_SET);
	        lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                               min_idx, max_idx, 0, 100);
	    }
        }

        min_idx = 0;
        max_idx = (((virt_num_data_entries / 10) / 8) *
                   ((file_mpi_rank % 4) + 1)) - 1;

        for ( i = 0; i < (virt_num_data_entries / 2); i+=2 )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC2__DIRTIED_FLAG);
	    lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                           min_idx, max_idx, 0, 100);
        }

	/* we can't rename pinned entries, so release any local pins now. */
	local_unpin_all_entries(cache_ptr, file_ptr, FALSE);

        min_count = 10 * (file_mpi_rank % 4);
        max_count = min_count + 100;

        /* rename the first half of the entries... */
        for ( i = 0; i < (virt_num_data_entries / 2); i++ )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC2__NO_FLAGS_SET);
	    rename_entry(cache_ptr, file_ptr, i,
			 (i + (virt_num_data_entries / 2)));
	    lock_and_unlock_random_entries(cache_ptr, file_ptr, 0,
			                   (virt_num_data_entries / 20),
                                           min_count, max_count);
        }

        /* ...and then rename them back. */
        for ( i = (virt_num_data_entries / 2) - 1; i >= 0; i-- )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC2__DIRTIED_FLAG);
	    rename_entry(cache_ptr, file_ptr, i,
			 (i + (virt_num_data_entries / 2)));
	    lock_and_unlock_random_entries(cache_ptr, file_ptr, 0,
			                   (virt_num_data_entries / 40),
                                           min_count, max_count);
        }

        /* finally, do some dirty lock/unlocks while we give the cache
         * a chance t reduce its size.
         */
        min_count = 100 * (file_mpi_rank % 4);
        max_count = min_count + 100;

        for ( i = 0; i < (virt_num_data_entries / 2); i+=2 )
        {
	    lock_entry(cache_ptr, file_ptr, i);
	    unlock_entry(cache_ptr, file_ptr, i, H5AC2__DIRTIED_FLAG);

            if ( i > 100 ) {

		lock_and_unlock_random_entries(cache_ptr, file_ptr,
                                               (i - 100), i,
                                               min_count, max_count);
            }
        }

        if ( fid >= 0 ) {

            if ( ! take_down_cache(fid, cache_ptr) ) {

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


            success = send_mssg(&mssg, FALSE);

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
 * Function:	smoke_check_5()
 *
 * Purpose:	Similar to smoke check 1, but modified to verify that
 * 		H5AC2_mark_pinned_or_protected_entry_dirty() works in
 * 		the parallel case.
 *
 * Return:	Success:	TRUE
 *
 *		Failure:	FALSE
 *
 * Programmer:	JRM -- 5/18/06
 *
 * Modifications:
 *
 *		JRM -- 7/12/06
 *		Added test code for H5AC2_expunge_entry() and 
 *		H5AC2_resize_pinned_entry().
 *
 *****************************************************************************/

hbool_t
smoke_check_5(void)
{
    const char * fcn_name = "smoke_check_5()";
    hbool_t show_progress = FALSE;
    hbool_t show_detailed_progress = FALSE;
    hbool_t success = TRUE;
    int i;
    int max_nerrors;
    int mile_stone = 1;
    hid_t fid = -1;
    H5F_t * file_ptr = NULL;
    H5C2_t * cache_ptr = NULL;
    struct mssg_t mssg;

    if ( world_mpi_rank == 0 ) {

        TESTING("smoke check #5");
    }

    nerrors = 0;
    init_data();
    reset_stats();

    if ( show_progress ) { /* 1 */
        HDfprintf(stdout, "%d:%s - %0d -- success = %d\n",
                  world_mpi_rank, fcn_name, mile_stone++, (int)success);
	fflush(stdout);
    }

    if ( world_mpi_rank == world_server_mpi_rank ) {

        if ( show_progress ) { /* 2 */
            HDfprintf(stdout, "%d:%s - %0ds -- success = %d\n",
                      world_mpi_rank, fcn_name, mile_stone++, (int)success);
	    fflush(stdout);
        }

	if ( ! server_main() ) {

            /* some error occured in the server -- report failure */
            nerrors++;
            if ( verbose ) {
		HDfprintf(stdout, "%d:%s: server_main() failed.\n",
                          world_mpi_rank, fcn_name);
            }
        }

        if ( show_progress ) { /* 3 */
            HDfprintf(stdout, "%d:%s - %0ds -- success = %d\n",
                      world_mpi_rank, fcn_name, mile_stone++, (int)success);
	    fflush(stdout);
        }
    }
    else /* run the clients */
    {
        if ( show_progress ) { /* 2 */
            HDfprintf(stdout, "%d:%s - %0d -- success = %d\n",
                      world_mpi_rank, fcn_name, mile_stone++, (int)success);
	    fflush(stdout);
        }

        if ( ! setup_cache_for_test(&fid, &file_ptr, &cache_ptr) ) {

            nerrors++;
            fid = -1;
            cache_ptr = NULL;
            if ( verbose ) {
		HDfprintf(stdout, "%d:%s: setup_cache_for_test() failed.\n",
                          world_mpi_rank, fcn_name);
            }
        }

        if ( show_progress ) { /* 3 */
            HDfprintf(stdout, "%d:%s - %0d -- success = %d\n",
                      world_mpi_rank, fcn_name, mile_stone++, (int)success);
	    fflush(stdout);
        }

        for ( i = 0; i < (virt_num_data_entries / 2); i++ )
        {
            insert_entry(cache_ptr, file_ptr, i, H5AC2__NO_FLAGS_SET);
        }

        if ( show_progress ) { /* 4 */
            HDfprintf(stdout, "%d:%s - %0d -- success = %d\n",
                      world_mpi_rank, fcn_name, mile_stone++, (int)success);
	    fflush(stdout);
        }

	/* flush the file so we can lock known clean entries. */
        if ( H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0 ) {
            nerrors++;
            if ( verbose ) {
	        HDfprintf(stdout, "%d:%s: H5Fflush() failed.\n",
                          world_mpi_rank, fcn_name);
            }
        }

        if ( show_progress ) { /* 5 */
            HDfprintf(stdout, "%d:%s - %0d -- success = %d\n",
                      world_mpi_rank, fcn_name, mile_stone++, (int)success);
	    fflush(stdout);
        }

        for ( i = 0; i < (virt_num_data_entries / 4); i++ )
        {
	    if ( show_detailed_progress )
            {
	        HDfprintf(stdout, "%d:(lock %d)\n", world_mpi_rank, i);
	        fflush(stdout);
            }
	    lock_entry(cache_ptr, file_ptr, i);

	    if ( i % 2 == 0 )
	    {
	        if ( show_detailed_progress )
                {
	            HDfprintf(stdout, "%d:(mpoped %d)\n", world_mpi_rank, i);
	            fflush(stdout);
                }
		mark_pinned_or_protected_entry_dirty(cache_ptr, file_ptr, i);
	    }

	    if ( show_detailed_progress )
            {
	        HDfprintf(stdout, "%d:(unlock %d)\n", world_mpi_rank, i);
	        fflush(stdout);
            }
	    unlock_entry(cache_ptr, file_ptr, i, H5AC2__NO_FLAGS_SET);

	    if ( i % 2 == 1 )
	    {
		if ( i % 4 == 1 ) {

	            if ( show_detailed_progress )
                    {
	                HDfprintf(stdout, "%d:(lock %d)\n", world_mpi_rank, i);
	                fflush(stdout);
                    }
	            lock_entry(cache_ptr, file_ptr, i);
	            if ( show_detailed_progress )
                    {
	                HDfprintf(stdout, "%d:(unlock %d)\n", world_mpi_rank, i);
	                fflush(stdout);
                    }
	            unlock_entry(cache_ptr, file_ptr, i, H5AC2__DIRTIED_FLAG);
		}

	        if ( show_detailed_progress )
                {
	           HDfprintf(stdout, "%d:(expunge %d)\n", world_mpi_rank, i);
	           fflush(stdout);
                }
	        expunge_entry(cache_ptr, file_ptr, i);
	    }
        }

        if ( show_progress ) { /* 6 */
            HDfprintf(stdout, "%d:%s - %0d -- success = %d\n",
                      world_mpi_rank, fcn_name, mile_stone++, (int)success);
	    fflush(stdout);
        }

        for ( i = (virt_num_data_entries / 2) - 1;
              i >= (virt_num_data_entries / 4);
	      i-- )
        {
	    pin_entry(cache_ptr, file_ptr, i, TRUE, FALSE);

	    if ( i % 2 == 0 )
	    {
		if ( i % 8 <= 4 ) {

		    resize_entry(cache_ptr, file_ptr, i, data[i].len / 2);
		}

		if ( i % 4 == 0 )
		{
		    mark_pinned_or_protected_entry_dirty(cache_ptr,
				                         file_ptr, i);
		}
		else
		{
		    mark_pinned_entry_dirty(cache_ptr, file_ptr, i,
				            FALSE, (size_t)0);
                }

		if ( i % 8 <= 4 ) {

		    resize_entry(cache_ptr, file_ptr, i, data[i].len);
		}
	    }

	    unpin_entry(cache_ptr, file_ptr, i, TRUE, FALSE, FALSE);
        }

        if ( show_progress ) { /* 7 */
            HDfprintf(stdout, "%d:%s - %0d -- success = %d\n",
                      world_mpi_rank, fcn_name, mile_stone++, (int)success);
	    fflush(stdout);
        }

        if ( fid >= 0 ) {

            if ( ! take_down_cache(fid, cache_ptr) ) {

                nerrors++;
                if ( verbose ) {
		    HDfprintf(stdout, "%d:%s: take_down_cache() failed.\n",
                              world_mpi_rank, fcn_name);
                }
            }
        }

        if ( show_progress ) { /* 8 */
            HDfprintf(stdout, "%d:%s - %0d -- success = %d\n",
                      world_mpi_rank, fcn_name, mile_stone++, (int)success);
	    fflush(stdout);
        }

        /* verify that all instance of datum are back where the started
         * and are clean.
         */

        for ( i = 0; i < NUM_DATA_ENTRIES; i++ )
        {
            HDassert( data_index[i] == i );
            HDassert( ! (data[i].dirty) );
        }

        if ( show_progress ) { /* 9 */
            HDfprintf(stdout, "%d:%s - %0d -- success = %d\n",
                      world_mpi_rank, fcn_name, mile_stone++, (int)success);
	    fflush(stdout);
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

            success = send_mssg(&mssg, FALSE);

            if ( ! success ) {

                nerrors++;
                if ( verbose ) {
                    HDfprintf(stdout, "%d:%s: send_mssg() failed on done.\n",
                              world_mpi_rank, fcn_name);
                }
            }
        }

        if ( show_progress ) { /* 10 */
            HDfprintf(stdout, "%d:%s - %0d -- success = %d\n",
                      world_mpi_rank, fcn_name, mile_stone++, (int)success);
	    fflush(stdout);
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

} /* smoke_check_5() */


/*****************************************************************************
 *
 * Function:	trace_file_check()
 *
 * Purpose:     A basic test of the trace file capability.  In essence,
 *              we invoke all operations that generate trace file output,
 *              and then verify that the expected output was generated.
 *
 *              Note that the trace file is currently implemented at the
 *              H5AC2 level, so all calls have to go through H5AC2.  Thus it
 *              is more convenient to test trace file capabilities in the
 *              parallel cache test which works at the H5AC2 level, instead
 *              of in the serial test code which does everything at the 
 *              H5C2 level.
 *
 *              The function must test trace file output in the following
 *              functions:
 *
 *                    - H5AC2_flush()
 *                    - H5AC2_set()
 *                    - H5AC2_mark_pinned_entry_dirty()
 *                    - H5AC2_mark_pinned_or_protected_entry_dirty()
 *                      H5AC2_rename()
 *                    - H5AC2_pin_protected_entry()
 *                    - H5AC2_protect()
 *                    - H5AC2_unpin_entry()
 *                    - H5AC2_unprotect()
 *                    - H5AC2_set_cache_auto_resize_config()
 *                    - H5AC2_expunge_entry()
 *                    - H5AC2_resize_pinned_entry()
 *
 *              This test is skipped if H5_METADATA_TRACE_FILE is undefined.
 *
 * Return:	Success:	TRUE
 *
 *		Failure:	FALSE
 *
 * Programmer:	JRM -- 6/13/06
 *
 * Modifications:
 *
 *		JRM -- 7/11/06
 *		Updated fro H5AC2_expunge_entry() and 
 *		H5AC2_resize_pinned_entry().
 *	
 *****************************************************************************/

hbool_t
trace_file_check(void)
{
    hbool_t success = TRUE;

#ifdef H5_METADATA_TRACE_FILE

    const char * fcn_name = "trace_file_check()";
    const char * expected_output[] =
    {
      "### HDF5 metadata cache trace file version 1 ###\n",
      "H5AC2_set_cache_auto_resize_config 1 0 1 0 \"t_cache2_trace.txt\" 1 0 1048576 0.500000 16777216 1048576 50000 1 0.900000 2.000000 1 4194304 3 0.999000 0.900000 1 1048576 3 1 0.100000 262144 0\n",
      "H5AC2_set 0x400 2 15 0x0 2 0\n",
      "H5AC2_set 0x402 2 15 0x0 2 0\n",
      "H5AC2_set 0x404 4 15 0x0 4 0\n",
      "H5AC2_set 0x408 6 15 0x0 6 0\n",
      "H5AC2_protect 0x400 2 15 H5AC2_WRITE 2 1\n",
      "H5AC2_mark_pinned_or_protected_entry_dirty 0x400 0\n",
      "H5AC2_unprotect 0x400 15 2 0 0\n",
      "H5AC2_protect 0x402 2 15 H5AC2_WRITE 2 1\n",
      "H5AC2_pin_protected_entry 0x402 0\n",
      "H5AC2_unprotect 0x402 15 2 0 0\n",
      "H5AC2_unpin_entry 0x402 0\n",
      "H5AC2_expunge_entry 0x402 15 0\n",
      "H5AC2_protect 0x404 4 15 H5AC2_WRITE 4 1\n",
      "H5AC2_pin_protected_entry 0x404 0\n",
      "H5AC2_unprotect 0x404 15 4 0 0\n",
      "H5AC2_mark_pinned_entry_dirty 0x404 0 0 0\n",
      "H5AC2_resize_pinned_entry 0x404 2 0\n",
      "H5AC2_resize_pinned_entry 0x404 4 0\n",
      "H5AC2_unpin_entry 0x404 0\n",
      "H5AC2_rename 0x400 0x8e65 15 0\n",
      "H5AC2_rename 0x8e65 0x400 15 0\n",
      "H5AC2_flush 0x0 0\n",
      NULL
    };
    char buffer[256];
    char trace_file_name[64];
    hbool_t done = FALSE;
    int i;
    int max_nerrors;
    int expected_line_len;
    int actual_line_len;
    hid_t fid = -1;
    H5F_t * file_ptr = NULL;
    H5C2_t * cache_ptr = NULL;
    FILE * trace_file_ptr = NULL;
    H5AC2_cache_config_t config;
    struct mssg_t mssg;

#endif /* H5_METADATA_TRACE_FILE */

    if ( world_mpi_rank == 0 ) {

        TESTING("trace file collection");
    }

#ifdef H5_METADATA_TRACE_FILE

    nerrors = 0;
    init_data();
    reset_stats();

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

        if ( nerrors == 0 ) {

            config.version = H5AC2__CURR_CACHE_CONFIG_VERSION;

            if ( H5AC2_get_cache_auto_resize_config(cache_ptr, &config) 
                 != SUCCEED ) {

		nerrors++;
	        HDfprintf(stdout, 
                        "%d:%s: H5AC2_get_cache_auto_resize_config() failed.\n", 
                        world_mpi_rank, fcn_name);

            } else {

                config.open_trace_file = TRUE;
		strcpy(config.trace_file_name, "t_cache2_trace.txt");

                if ( H5AC2_set_cache_auto_resize_config(cache_ptr, &config) 
                     != SUCCEED ) {

		    nerrors++;
	            HDfprintf(stdout, 
                         "%d:%s: H5AC2_set_cache_auto_resize_config() failed.\n",
                          world_mpi_rank, fcn_name);
                }
            }
        }

	insert_entry(cache_ptr, file_ptr, 0, H5AC2__NO_FLAGS_SET);
	insert_entry(cache_ptr, file_ptr, 1, H5AC2__NO_FLAGS_SET);
	insert_entry(cache_ptr, file_ptr, 2, H5AC2__NO_FLAGS_SET);
	insert_entry(cache_ptr, file_ptr, 3, H5AC2__NO_FLAGS_SET);

	lock_entry(cache_ptr, file_ptr, 0);
	mark_pinned_or_protected_entry_dirty(cache_ptr, file_ptr, 0);
	unlock_entry(cache_ptr, file_ptr, 0, H5AC2__NO_FLAGS_SET);

	lock_entry(cache_ptr, file_ptr, 1);
        pin_protected_entry(cache_ptr, file_ptr, 1, TRUE);
	unlock_entry(cache_ptr, file_ptr, 1, H5AC2__NO_FLAGS_SET);
        unpin_entry(cache_ptr, file_ptr, 1, TRUE, FALSE, FALSE);

        expunge_entry(cache_ptr,file_ptr, 1);

	lock_entry(cache_ptr, file_ptr, 2);
        pin_protected_entry(cache_ptr, file_ptr, 2, TRUE);
	unlock_entry(cache_ptr, file_ptr, 2, H5AC2__NO_FLAGS_SET);
	mark_pinned_entry_dirty(cache_ptr, file_ptr, 2, FALSE, 0);
        resize_entry(cache_ptr, file_ptr, 2, data[2].len / 2);
        resize_entry(cache_ptr, file_ptr, 2, data[2].len);
        unpin_entry(cache_ptr, file_ptr, 2, TRUE, FALSE, FALSE);

	rename_entry(cache_ptr, file_ptr, 0, 20);
	rename_entry(cache_ptr, file_ptr, 0, 20);

        if ( H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0 ) {
            nerrors++;
            if ( verbose ) {
	        HDfprintf(stdout, "%d:%s: H5Fflush() failed.\n", 
                          world_mpi_rank, fcn_name);
            }
        }

        if ( nerrors == 0 ) {

            config.version = H5AC2__CURR_CACHE_CONFIG_VERSION;

            if ( H5AC2_get_cache_auto_resize_config(cache_ptr, &config) 
                 != SUCCEED ) {

		nerrors++;
	        HDfprintf(stdout, 
                        "%d:%s: H5AC2_get_cache_auto_resize_config() failed.\n", 
                        world_mpi_rank, fcn_name);

            } else {

                config.open_trace_file = FALSE;
                config.close_trace_file = TRUE;
		config.trace_file_name[0] = '\0';

                if ( H5AC2_set_cache_auto_resize_config(cache_ptr, &config) 
                     != SUCCEED ) {

		    nerrors++;
	            HDfprintf(stdout, 
                        "%d:%s: H5AC2_set_cache_auto_resize_config() failed.\n",
                        world_mpi_rank, fcn_name);
                }
            }
        }

        if ( fid >= 0 ) {

            if ( ! take_down_cache(fid, cache_ptr) ) {

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

            success = send_mssg(&mssg, FALSE);

            if ( ! success ) {

                nerrors++;
                if ( verbose ) {
                    HDfprintf(stdout, "%d:%s: send_mssg() failed on done.\n", 
                              world_mpi_rank, fcn_name);
                }
            }
        }

        if ( nerrors == 0 ) {
	
	    sprintf(trace_file_name, "t_cache2_trace.txt.%d", 
		    (int)file_mpi_rank);

	    if ( (trace_file_ptr = HDfopen(trace_file_name, "r")) == NULL ) {

                nerrors++;
                if ( verbose ) {
                    HDfprintf(stdout, "%d:%s: HDfopen failed.\n", 
                              world_mpi_rank, fcn_name);
                }
            }
	}

	i = 0;
	while ( ( nerrors == 0 ) && ( ! done ) )
	{
	    if ( expected_output[i] == NULL ) {

		expected_line_len = 0;

	    } else {

		expected_line_len = HDstrlen(expected_output[i]);
	    }

	    if ( HDfgets(buffer, 255, trace_file_ptr) != NULL ) {

	        actual_line_len = strlen(buffer);

	    } else {

	        actual_line_len = 0;
	    }

	    if ( ( actual_line_len == 0 ) && ( expected_line_len == 0 ) ) {

	        done = TRUE;

	    } else if ( ( actual_line_len != expected_line_len ) ||
		        ( HDstrcmp(buffer, expected_output[i]) != 0 ) ) {

	        nerrors++;
                if ( verbose ) {
                    HDfprintf(stdout, 
			      "%d:%s: Unexpected data in trace file line %d.\n",
                              world_mpi_rank, fcn_name, i);
		    HDfprintf(stdout, "%d:%s: expected = \"%s\" %d\n",
			       world_mpi_rank, fcn_name, expected_output[i],
			       expected_line_len);
		    HDfprintf(stdout, "%d:%s: actual   = \"%s\" %d\n",
			       world_mpi_rank, fcn_name, buffer,
			       actual_line_len);
                }
	    } else {
	        i++;
	    }
	}

	if ( trace_file_ptr != NULL ) {

	    HDfclose(trace_file_ptr);
	    trace_file_ptr = NULL;
#if 1
	    HDremove(trace_file_name);
#endif
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

#else /* H5_METADATA_TRACE_FILE */

    if ( world_mpi_rank == 0 ) {

        SKIPPED();

        HDfprintf(stdout, " trace file support disabled.\n");
    }

#endif /* H5_METADATA_TRACE_FILE */

    return(success);

} /* trace_file_check() */


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
    int express_test;
    unsigned u;
    int mpi_size;
    int mpi_rank;
    int max_nerrors;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    world_mpi_size = mpi_size;
    world_mpi_rank = mpi_rank;
    world_server_mpi_rank = mpi_size - 1;
    world_mpi_comm = MPI_COMM_WORLD;

    H5open();

    express_test = do_express_test();
#if 0 /* JRM */
    express_test = 0;
#endif /* JRM */
    if ( express_test ) {

	virt_num_data_entries = EXPRESS_VIRT_NUM_DATA_ENTRIES;

    } else {

	virt_num_data_entries = STD_VIRT_NUM_DATA_ENTRIES;
    }

#ifdef H5_HAVE_MPE
    if ( MAINPROCESS ) { printf("	Tests compiled for MPE.\n"); }
    virt_num_data_entries = MPE_VIRT_NUM_DATA_ENTIES;
#endif /* H5_HAVE_MPE */


    if (MAINPROCESS){
	printf("===================================\n");
	printf("Parallel metadata cache tests\n");
	printf("	mpi_size     = %d\n", mpi_size);
	printf("	express_test = %d\n", express_test);
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
    for ( u = 0; u < sizeof(FILENAME) / sizeof(FILENAME[0]) - 1; ++u )
    {
        if ( h5_fixname(FILENAME[u], fapl, filenames[u],
                        sizeof(filenames[u])) == NULL ) {

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
#endif
#if 1
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
#if 1
    smoke_check_5();
#endif   
#if 1
    trace_file_check();
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

