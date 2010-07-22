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
 * Programmer:	Robb Matzke <matzke@llnl.gov>
 *		Monday, November 10, 1997
 *
 * Purpose:	Implements a file driver which dispatches I/O requests to
 *		other file drivers depending on the purpose of the address
 *		region being accessed. For instance, all meta-data could be
 *		place in one file while all raw data goes to some other file.
 *		This also serves as an example of coding a complex file driver,
 *		therefore, it should not use any non-public definitions.
 */
#include <assert.h>
#include <stdlib.h>
#include <string.h>

/* Disable certain warnings in PC-Lint: */
/*lint --emacro( {534, 830}, H5P_DEFAULT, H5P_FILE_ACCESS, H5P_DATASET_XFER) */
/*lint --emacro( {534, 830}, H5F_ACC_DEBUG, H5F_ACC_RDWR) */
/*lint --emacro( {534, 830}, H5FD_MULTI) */
/*lint -esym( 534, H5Eclear2, H5Epush2) */

#include "hdf5.h"

/*
 * Define H5FD_MULTI_DEBUG if you want the ability to print debugging
 * messages to the standard error stream. Messages are only printed if the
 * file is opened with the H5F_ACC_DEBUG flag.
 */
#define H5FD_MULTI_DEBUG

/* Our version of MAX */
#undef MAX
#define MAX(X,Y)	((X)>(Y)?(X):(Y))

#ifndef FALSE
#define FALSE		0
#endif
#ifndef TRUE
#define TRUE		1
#endif

/* Loop through all mapped files */
#define UNIQUE_MEMBERS(MAP,LOOPVAR) {					      \
    H5FD_mem_t _unmapped, LOOPVAR;					      \
    hbool_t _seen[H5FD_MEM_NTYPES];					      \
									      \
    memset(_seen, 0, sizeof _seen);					      \
    for (_unmapped=H5FD_MEM_SUPER; _unmapped<H5FD_MEM_NTYPES; _unmapped=(H5FD_mem_t)(_unmapped+1)) {  \
	LOOPVAR = MAP[_unmapped];					      \
	if (H5FD_MEM_DEFAULT==LOOPVAR) LOOPVAR=_unmapped;		      \
	assert(LOOPVAR>0 && LOOPVAR<H5FD_MEM_NTYPES);			      \
	if (_seen[LOOPVAR]++) continue;

#define ALL_MEMBERS(LOOPVAR) {						      \
    H5FD_mem_t LOOPVAR;							      \
    for (LOOPVAR=H5FD_MEM_DEFAULT; LOOPVAR<H5FD_MEM_NTYPES; LOOPVAR=(H5FD_mem_t)(LOOPVAR+1)) {


#define END_MEMBERS	}}

/* Type definitions of the structures used to store the aio control i
 * block(s) returned by any underlying file driver(s) that support AIO, 
 * along with the information needed to route the control block to the 
 * appropriate driver when it is passed down to the multi driver.
 *
 * The magic field must always be set to H5FD_MULTI_AIO_CTLBLK_T__MAGIC.
 *
 * While AIO reads and writes will only involve a single underlying 
 * file driver, an aio fsync request can (if all underlying drivers 
 * support AIO) involve all drivers associated with this instance of 
 * the multi file driver.
 *
 * To support this, for each underlying file driver we must maintain 
 * a pointer to the associated instance of H5FD_t, a pointer to the 
 * AIO control block, and booleans to store the done and finished status
 * of each operation.  Store this data in an instance of 
 * H5FD_multi_aio_subctlblk_t.
 *
 * Since the number possible underlying file drivers is strictly limited,
 * we use an array of struct H5FD_multi_aio_subctlblk_t, and store the 
 * number of active entries in this array in num_subctlblks.
 *
 * The driver, ctlblk, done, and finished fields of each entry 
 * in the array are set to NULL, NULL, FALSE, and FALSE respectively,
 * and retain those values when not in use.
 */

#define H5FD_MULTI_AIO_CTLBLK_T__MAGIC	0x4D414344	/* 'MACB' */

typedef struct H5FD_multi_aio_subctlblk_t {

    H5FD_t * driver;
    void   * ctlblk;
    hbool_t  done;
    hbool_t  finished;
} H5FD_multi_aio_subctlblk_t;

typedef struct H5FD_multi_aio_ctlblk_t {

    uint32_t                   magic;
    int                        num_subctlblks;
    H5FD_multi_aio_subctlblk_t sub_ctlblks[H5FD_MEM_NTYPES];

} H5FD_multi_aio_ctlblk_t;


/* The driver identification number, initialized at runtime */
static hid_t H5FD_MULTI_g = 0;

/* Driver-specific file access properties */
typedef struct H5FD_multi_fapl_t {
    H5FD_mem_t	memb_map[H5FD_MEM_NTYPES]; /*memory usage map		*/
    hid_t	memb_fapl[H5FD_MEM_NTYPES];/*member access properties	*/
    char	*memb_name[H5FD_MEM_NTYPES];/*name generators		*/
    haddr_t	memb_addr[H5FD_MEM_NTYPES];/*starting addr per member	*/
    hbool_t	relax;			/*less stringent error checking	*/
} H5FD_multi_fapl_t;

/*
 * The description of a file belonging to this driver. The file access
 * properties and member names do not have to be copied into this struct
 * since they will be held open by the file access property list which is
 * copied into the parent file struct in H5F_open().
 */
typedef struct H5FD_multi_t {
    H5FD_t	pub;		/*public stuff, must be first		*/
    H5FD_multi_fapl_t fa;	/*driver-specific file access properties*/
    haddr_t	memb_next[H5FD_MEM_NTYPES];/*addr of next member	*/
    H5FD_t	*memb[H5FD_MEM_NTYPES];	/*member pointers		*/
    /*haddr_t	eoa;*/		/*end of allocated addresses.  Took it out
                                 *because individual files have their own
                                 *eoa.                                  */
    unsigned	flags;		/*file open flags saved for debugging	*/
    char	*name;		/*name passed to H5Fopen or H5Fcreate	*/
} H5FD_multi_t;

/* Driver specific data transfer properties */
typedef struct H5FD_multi_dxpl_t {
    hid_t	memb_dxpl[H5FD_MEM_NTYPES];/*member data xfer properties*/
} H5FD_multi_dxpl_t;

/* Private functions */
static char *my_strdup(const char *s);
static int compute_next(H5FD_multi_t *file);
static int open_members(H5FD_multi_t *file);

/* Callback prototypes */
static hsize_t H5FD_multi_sb_size(H5FD_t *file);
static herr_t H5FD_multi_sb_encode(H5FD_t *file, char *name/*out*/,
				   unsigned char *buf/*out*/);
static herr_t H5FD_multi_sb_decode(H5FD_t *file, const char *name,
				   const unsigned char *buf);
static void *H5FD_multi_fapl_get(H5FD_t *file);
static void *H5FD_multi_fapl_copy(const void *_old_fa);
static herr_t H5FD_multi_fapl_free(void *_fa);
static void *H5FD_multi_dxpl_copy(const void *_old_dx);
static herr_t H5FD_multi_dxpl_free(void *_dx);
static H5FD_t *H5FD_multi_open(const char *name, unsigned flags,
			       hid_t fapl_id, haddr_t maxaddr);
static herr_t H5FD_multi_close(H5FD_t *_file);
static int H5FD_multi_cmp(const H5FD_t *_f1, const H5FD_t *_f2);
static herr_t H5FD_multi_query(const H5FD_t *_f1, unsigned long *flags);
static herr_t H5FD_multi_get_type_map(const H5FD_t *file, H5FD_mem_t *type_map);
static haddr_t H5FD_multi_get_eoa(const H5FD_t *_file, H5FD_mem_t type);
static herr_t H5FD_multi_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t eoa);
static haddr_t H5FD_multi_get_eof(const H5FD_t *_file);
static herr_t  H5FD_multi_get_handle(H5FD_t *_file, hid_t fapl, void** file_handle);
static haddr_t H5FD_multi_alloc(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, hsize_t size);
static herr_t H5FD_multi_free(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr,
			      hsize_t size);
static herr_t H5FD_multi_read(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr,
			      size_t size, void *_buf/*out*/);
static herr_t H5FD_multi_write(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr,
			       size_t size, const void *_buf);
static herr_t H5FD_multi_flush(H5FD_t *_file, hid_t dxpl_id, unsigned closing);
static herr_t H5FD_multi_truncate(H5FD_t *_file, hid_t dxpl_id, hbool_t closing);
static herr_t H5FD_multi_aio_alloc_ctlblk(H5FD_multi_aio_ctlblk_t **ctlblk_ptr_ptr);
static herr_t H5FD_multi_aio_discard_ctlblk(H5FD_multi_aio_ctlblk_t *ctlblk_ptr);
static herr_t H5FD_multi_aio_read(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id,
                                  haddr_t addr, size_t size, void *buffer,
                                  void **ctlblk_ptr_ptr);
static herr_t H5FD_multi_aio_write(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id,
                                   haddr_t addr, size_t size, void *buffer,
                                   void **ctlblk_ptr_ptr);
static herr_t H5FD_multi_aio_test(hbool_t *done_ptr, void *ctlblk_ptr);
static herr_t H5FD_multi_aio_wait(void *ctlblk_ptr);
static herr_t H5FD_multi_aio_finish(int *errno_ptr, void *ctlblk_ptr);
static herr_t H5FD_multi_aio_fsync(H5FD_t *file, void **ctlblk_ptr_ptr);
static herr_t H5FD_multi_aio_cancel(void *ctlblk_ptr);
static herr_t H5FD_multi_fsync(H5FD_t *file, hid_t dxpl_id);

/* The class struct */
static const H5FD_class_t H5FD_multi_g = {
    "multi",					/*name			*/
    HADDR_MAX,					/*maxaddr		*/
    H5F_CLOSE_WEAK,				/* fc_degree		*/
    H5FD_multi_sb_size,				/*sb_size		*/
    H5FD_multi_sb_encode,			/*sb_encode		*/
    H5FD_multi_sb_decode,			/*sb_decode		*/
    sizeof(H5FD_multi_fapl_t),			/*fapl_size		*/
    H5FD_multi_fapl_get,			/*fapl_get		*/
    H5FD_multi_fapl_copy,			/*fapl_copy		*/
    H5FD_multi_fapl_free,			/*fapl_free		*/
    sizeof(H5FD_multi_dxpl_t),			/*dxpl_size		*/
    H5FD_multi_dxpl_copy,			/*dxpl_copy		*/
    H5FD_multi_dxpl_free,			/*dxpl_free		*/
    H5FD_multi_open,				/*open			*/
    H5FD_multi_close,				/*close			*/
    H5FD_multi_cmp,				/*cmp			*/
    H5FD_multi_query,				/*query			*/
    H5FD_multi_get_type_map,			/*get_type_map		*/
    H5FD_multi_alloc,				/*alloc			*/
    H5FD_multi_free,				/*free			*/
    H5FD_multi_get_eoa,				/*get_eoa		*/
    H5FD_multi_set_eoa,				/*set_eoa		*/
    H5FD_multi_get_eof,				/*get_eof		*/
    H5FD_multi_get_handle,                      /*get_handle            */
    H5FD_multi_read,				/*read			*/
    H5FD_multi_write,				/*write			*/
    H5FD_multi_flush,				/*flush			*/
    H5FD_multi_truncate,			/*truncate		*/
    NULL,                                       /*lock                  */
    NULL,                                       /*unlock                */
    H5FD_multi_aio_read,                        /*aio_read              */
    H5FD_multi_aio_write,                       /*aio_write             */
    H5FD_multi_aio_test,                        /*aio_test              */
    H5FD_multi_aio_wait,                        /*aio_wait              */
    H5FD_multi_aio_finish,                      /*aio_finish            */
    H5FD_multi_aio_fsync,                       /*aio_fsync             */
    H5FD_multi_aio_cancel,                      /*aio_cancel            */
    H5FD_multi_fsync,				/*fsync			*/
    H5FD_FLMAP_DEFAULT 				/*fl_map		*/
};


/*-------------------------------------------------------------------------
 * Function:	my_strdup
 *
 * Purpose:	Private version of strdup()
 *
 * Return:	Success:	Ptr to new copy of string
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Friday, August 13, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static char *
my_strdup(const char *s)
{
    char *x;

    if(!s)
        return NULL;
    if(NULL == (x = (char *)malloc(strlen(s) + 1)))
        return NULL;
    strcpy(x, s);

    return x;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_init
 *
 * Purpose:	Initialize this driver by registering the driver with the
 *		library.
 *
 * Return:	Success:	The driver ID for the multi driver.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5FD_multi_init(void)
{
    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    if (H5I_VFL!=H5Iget_type(H5FD_MULTI_g)) {
	H5FD_MULTI_g = H5FDregister(&H5FD_multi_g);
    }
    return H5FD_MULTI_g;
}


/*---------------------------------------------------------------------------
 * Function:	H5FD_multi_term
 *
 * Purpose:	Shut down the VFD
 *
 * Return:	<none>
 *
 * Programmer:  Quincey Koziol
 *              Friday, Jan 30, 2004
 *
 * Modification:
 *
 *---------------------------------------------------------------------------
 */
void
H5FD_multi_term(void)
{
    /* Reset VFL ID */
    H5FD_MULTI_g=0;

} /* end H5FD_multi_term() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_fapl_split
 *
 * Purpose:	Compatability function. Makes the multi driver act like the
 *		old split driver which stored meta data in one file and raw
 *		data in another file.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August 11, 1999
 *
 * Modifications:
 *	Albert Cheng, Sep 17, 2001
 *	Added feature that if the raw or meta extension string contains
 *	a "%s", it will be substituted by the filename given for H5Fopen
 *	or H5Fcreate.  This is same as the multi-file syntax.  If no %s
 *	is found, one is inserted at the beginning.  This is the previous
 *	behavior.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_split(hid_t fapl, const char *meta_ext, hid_t meta_plist_id,
		  const char *raw_ext, hid_t raw_plist_id)
{
    H5FD_mem_t		memb_map[H5FD_MEM_NTYPES];
    hid_t		memb_fapl[H5FD_MEM_NTYPES];
    const char		*memb_name[H5FD_MEM_NTYPES];
    char		meta_name[1024], raw_name[1024];
    haddr_t		memb_addr[H5FD_MEM_NTYPES];

    /*NO TRACE*/

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    /* Initialize */
    ALL_MEMBERS(mt) {
	memb_map[mt] = (H5FD_MEM_DRAW==mt?mt:H5FD_MEM_SUPER);
	memb_fapl[mt] = -1;
	memb_name[mt] = NULL;
	memb_addr[mt] = HADDR_UNDEF;
    } END_MEMBERS;

    /* The file access properties */
    memb_fapl[H5FD_MEM_SUPER] = meta_plist_id;
    memb_fapl[H5FD_MEM_DRAW] = raw_plist_id;

    /* The names */
    /* process meta filename */
    if (meta_ext){
	if (strstr(meta_ext, "%s"))
	    strcpy(meta_name, meta_ext);
	else
	    sprintf(meta_name, "%%s%s", meta_ext);
    }
    else
	strcpy(meta_name, "%s.meta");
    memb_name[H5FD_MEM_SUPER] = meta_name;

    /* process raw filename */
    if (raw_ext){
	if (strstr(raw_ext, "%s"))
	    strcpy(raw_name, raw_ext);
	else
	    sprintf(raw_name, "%%s%s", raw_ext);
    }
    else
	strcpy(raw_name, "%s.raw");
    memb_name[H5FD_MEM_DRAW] = raw_name;

    /* The sizes */
    memb_addr[H5FD_MEM_SUPER] = 0;
    memb_addr[H5FD_MEM_DRAW] = HADDR_MAX/2;

    return H5Pset_fapl_multi(fapl, memb_map, memb_fapl, memb_name, memb_addr, TRUE);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_fapl_multi
 *
 * Purpose:	Sets the file access property list FAPL_ID to use the multi
 *		driver. The MEMB_MAP array maps memory usage types to other
 *		memory usage types and is the mechanism which allows the
 *		caller to specify how many files are created. The array
 *		contains H5FD_MEM_NTYPES entries which are either the value
 *		H5FD_MEM_DEFAULT or a memory usage type and the number of
 *		unique values determines the number of files which are
 *		opened.  For each memory usage type which will be associated
 *		with a file the MEMB_FAPL array should have a property list
 *		and the MEMB_NAME array should be a name generator (a
 *		printf-style format with a %s which will be replaced with the
 *		name passed to H5FDopen(), usually from H5Fcreate() or
 *		H5Fopen()).
 *
 *		If RELAX is set then opening an existing file for read-only
 *		access will not fail if some file members are missing.  This
 *		allows a file to be accessed in a limited sense if just the
 *		meta data is available.
 *
 * Defaults:	Default values for each of the optional arguments are:
 *
 *		memb_map:	The default member map has the value
 *				H5FD_MEM_DEFAULT for each element.
 *
 * 		memb_fapl:	The value H5P_DEFAULT for each element.
 *
 *		memb_name:	The string `%s-X.h5' where `X' is one of the
 *				letters `s' (H5FD_MEM_SUPER),
 *				`b' (H5FD_MEM_BTREE), `r' (H5FD_MEM_DRAW),
 * 				`g' (H5FD_MEM_GHEAP), 'l' (H5FD_MEM_LHEAP),
 * 				`o' (H5FD_MEM_OHDR).
 *
 * 		memb_addr:	The value HADDR_UNDEF for each element.
 *
 *
 * Example:	To set up a multi file access property list which partitions
 *		data into meta and raw files each being 1/2 of the address
 *		space one would say:
 *
 * 		    H5FD_mem_t mt, memb_map[H5FD_MEM_NTYPES];
 *		    hid_t memb_fapl[H5FD_MEM_NTYPES];
 *		    const char *memb[H5FD_MEM_NTYPES];
 *		    haddr_t memb_addr[H5FD_MEM_NTYPES];
 *
 * 		    // The mapping...
 * 		    for (mt=0; mt<H5FD_MEM_NTYPES; mt++) {
 *		        memb_map[mt] = H5FD_MEM_SUPER;
 *		    }
 * 		    memb_map[H5FD_MEM_DRAW] = H5FD_MEM_DRAW;
 *
 * 		    // Member information
 * 		    memb_fapl[H5FD_MEM_SUPER] = H5P_DEFAULT;
 *		    memb_name[H5FD_MEM_SUPER] = "%s.meta";
 *		    memb_addr[H5FD_MEM_SUPER] = 0;
 *
 *		    memb_fapl[H5FD_MEM_DRAW] = H5P_DEFAULT;
 *		    memb_name[H5FD_MEM_DRAW] = "%s.raw";
 *		    memb_addr[H5FD_MEM_DRAW] = HADDR_MAX/2;
 *
 * 		    hid_t fapl = H5Pcreate(H5P_FILE_ACCESS);
 *		    H5Pset_fapl_multi(fapl, memb_map, memb_fapl,
 *		                      memb_name, memb_addr, TRUE);
 *
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *		Raymond Lu, 2001-10-25
 *		Use new generic property list for argument checking.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_multi(hid_t fapl_id, const H5FD_mem_t *memb_map,
		  const hid_t *memb_fapl, const char * const *memb_name,
		  const haddr_t *memb_addr, hbool_t relax)
{
    H5FD_multi_fapl_t	fa;
    H5FD_mem_t		mt, mmt;
    H5FD_mem_t		_memb_map[H5FD_MEM_NTYPES];
    hid_t		_memb_fapl[H5FD_MEM_NTYPES];
    char		_memb_name[H5FD_MEM_NTYPES][16];
    const char		*_memb_name_ptrs[H5FD_MEM_NTYPES];
    haddr_t		_memb_addr[H5FD_MEM_NTYPES];
    static const char	*letters = "Xsbrglo";
    static const char *func="H5FDset_fapl_multi";  /* Function Name for error reporting */

    /*NO TRACE*/

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    /* Check arguments and supply default values */
    if(H5I_GENPROP_LST != H5Iget_type(fapl_id) ||
            TRUE != H5Pisa_class(fapl_id, H5P_FILE_ACCESS))
        H5Epush_ret(func, H5E_ERR_CLS, H5E_PLIST, H5E_BADVALUE, "not an access list", -1)
    if (!memb_map) {
	for (mt=H5FD_MEM_DEFAULT; mt<H5FD_MEM_NTYPES; mt=(H5FD_mem_t)(mt+1))
	    _memb_map[mt] = H5FD_MEM_DEFAULT;
	memb_map = _memb_map;
    }
    if (!memb_fapl) {
	for (mt=H5FD_MEM_DEFAULT; mt<H5FD_MEM_NTYPES; mt=(H5FD_mem_t)(mt+1))
	    _memb_fapl[mt] = H5Pcreate(H5P_FILE_ACCESS);
	memb_fapl = _memb_fapl;
    }
    if (!memb_name) {
	assert(strlen(letters)==H5FD_MEM_NTYPES);
	for (mt=H5FD_MEM_DEFAULT; mt<H5FD_MEM_NTYPES; mt=(H5FD_mem_t)(mt+1)) {
	    sprintf(_memb_name[mt], "%%s-%c.h5", letters[mt]);
	    _memb_name_ptrs[mt] = _memb_name[mt];
	}
	memb_name = _memb_name_ptrs;
    }
    if (!memb_addr) {
	for (mt=H5FD_MEM_DEFAULT; mt<H5FD_MEM_NTYPES; mt=(H5FD_mem_t)(mt+1))
	    _memb_addr[mt] = (hsize_t)(mt ? (mt - 1) : 0) * HADDR_MAX / H5FD_MEM_NTYPES;
	memb_addr = _memb_addr;
    }

    for (mt=H5FD_MEM_DEFAULT; mt<H5FD_MEM_NTYPES; mt=(H5FD_mem_t)(mt+1)) {
	/* Map usage type */
	mmt = memb_map[mt];
	if (mmt<0 || mmt>=H5FD_MEM_NTYPES)
            H5Epush_ret(func, H5E_ERR_CLS, H5E_INTERNAL, H5E_BADRANGE, "file resource type out of range", -1)
	if (H5FD_MEM_DEFAULT==mmt) mmt = mt;

	/*
	 * All members of MEMB_FAPL must be either defaults or actual file
	 * access property lists.
	 */
	if (H5P_DEFAULT!=memb_fapl[mmt] && TRUE!=H5Pisa_class(memb_fapl[mmt], H5P_FILE_ACCESS))
            H5Epush_ret(func, H5E_ERR_CLS, H5E_INTERNAL, H5E_BADVALUE, "file resource type incorrect", -1)

	/* All names must be defined */
	if (!memb_name[mmt] || !memb_name[mmt][0])
            H5Epush_ret(func, H5E_ERR_CLS, H5E_INTERNAL, H5E_BADVALUE, "file resource type not set", -1)
    }

    /*
     * Initialize driver specific information. No need to copy it into the FA
     * struct since all members will be copied by H5Pset_driver().
     */
    memcpy(fa.memb_map, memb_map, H5FD_MEM_NTYPES*sizeof(H5FD_mem_t));
    memcpy(fa.memb_fapl, memb_fapl, H5FD_MEM_NTYPES*sizeof(hid_t));
    memcpy(fa.memb_name, memb_name, H5FD_MEM_NTYPES*sizeof(char*));
    memcpy(fa.memb_addr, memb_addr, H5FD_MEM_NTYPES*sizeof(haddr_t));
    fa.relax = relax;

    /* Patch up H5P_DEFAULT property lists for members */
    for (mt=H5FD_MEM_DEFAULT; mt<H5FD_MEM_NTYPES; mt=(H5FD_mem_t)(mt+1)) {
        if(fa.memb_fapl[mt]==H5P_DEFAULT)
            fa.memb_fapl[mt] = H5Pcreate(H5P_FILE_ACCESS);
    }
    return H5Pset_driver(fapl_id, H5FD_MULTI, &fa);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_fapl_multi
 *
 * Purpose:	Returns information about the multi file access property
 *		list though the function arguments which are the same as for
 *		H5Pset_fapl_multi() above.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *              Raymond Lu, 2001-10-25
 *              Use new generic property list for argument checking.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_fapl_multi(hid_t fapl_id, H5FD_mem_t *memb_map/*out*/,
		  hid_t *memb_fapl/*out*/, char **memb_name/*out*/,
		  haddr_t *memb_addr/*out*/, hbool_t *relax)
{
    H5FD_multi_fapl_t	*fa;
    H5FD_mem_t		mt;
    static const char *func="H5FDget_fapl_multi";  /* Function Name for error reporting */

    /*NO TRACE*/

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    if(H5I_GENPROP_LST != H5Iget_type(fapl_id) ||
            TRUE != H5Pisa_class(fapl_id, H5P_FILE_ACCESS))
        H5Epush_ret(func, H5E_ERR_CLS, H5E_PLIST, H5E_BADTYPE, "not an access list", -1)
    if(H5FD_MULTI != H5Pget_driver(fapl_id))
        H5Epush_ret(func, H5E_ERR_CLS, H5E_PLIST, H5E_BADVALUE, "incorrect VFL driver", -1)
    if(NULL == (fa= (H5FD_multi_fapl_t *)H5Pget_driver_info(fapl_id)))
        H5Epush_ret(func, H5E_ERR_CLS, H5E_PLIST, H5E_BADVALUE, "bad VFL driver info", -1)

    if (memb_map)
        memcpy(memb_map, fa->memb_map, H5FD_MEM_NTYPES*sizeof(H5FD_mem_t));
    if (memb_fapl) {
	for (mt=H5FD_MEM_DEFAULT; mt<H5FD_MEM_NTYPES; mt=(H5FD_mem_t)(mt+1)) {
	    if (fa->memb_fapl[mt]>=0)
		memb_fapl[mt] = H5Pcopy(fa->memb_fapl[mt]);
	    else
		memb_fapl[mt] = fa->memb_fapl[mt]; /*default or bad ID*/
	}
    }
    if (memb_name) {
	for (mt=H5FD_MEM_DEFAULT; mt<H5FD_MEM_NTYPES; mt=(H5FD_mem_t)(mt+1)) {
	    if (fa->memb_name[mt]) {
		memb_name[mt] = (char *)malloc(strlen(fa->memb_name[mt])+1);
		strcpy(memb_name[mt], fa->memb_name[mt]);
	    } else
		memb_name[mt] = NULL;
	}
    }
    if (memb_addr)
	memcpy(memb_addr, fa->memb_addr, H5FD_MEM_NTYPES*sizeof(haddr_t));
    if (relax)
	*relax = fa->relax;

    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_dxpl_multi
 *
 * Purpose:	Set the data transfer property list DXPL_ID to use the multi
 *		driver with the specified data transfer properties for each
 *		memory usage type MEMB_DXPL[] (after the usage map is
 *		applied).
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Tuesday, August 10, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_dxpl_multi(hid_t dxpl_id, const hid_t *memb_dxpl)
{
    H5FD_multi_dxpl_t	dx;
    H5FD_mem_t		mt;
    static const char *func="H5FDset_dxpl_multi";  /* Function Name for error reporting */

    /*NO TRACE*/

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    /* Check arguments */
    if (TRUE!=H5Pisa_class(dxpl_id,  H5P_DATASET_XFER))
        H5Epush_ret(func, H5E_ERR_CLS, H5E_PLIST, H5E_BADTYPE, "not a data transfer property list", -1)
    if (!memb_dxpl)
        H5Epush_ret(func, H5E_ERR_CLS, H5E_INTERNAL, H5E_BADVALUE, "invalid pointer", -1)
    for (mt=H5FD_MEM_DEFAULT; mt<H5FD_MEM_NTYPES; mt=(H5FD_mem_t)(mt+1)) {
        if (memb_dxpl[mt]!=H5P_DEFAULT && TRUE!=H5Pisa_class(memb_dxpl[mt], H5P_DATASET_XFER))
            H5Epush_ret(func, H5E_ERR_CLS, H5E_PLIST, H5E_BADTYPE, "not a data transfer property list", -1)
    }

    /* Initialize the data transfer property list */
    memcpy(dx.memb_dxpl, memb_dxpl, H5FD_MEM_NTYPES*sizeof(hid_t));

    /* Convert "generic" default property lists into default dataset transfer property lists */
    for (mt=H5FD_MEM_DEFAULT; mt<H5FD_MEM_NTYPES; mt=(H5FD_mem_t)(mt+1)) {
        if (dx.memb_dxpl[mt]==H5P_DEFAULT)
            dx.memb_dxpl[mt]=H5P_DATASET_XFER_DEFAULT;
    }

    return H5Pset_driver(dxpl_id, H5FD_MULTI, &dx);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_dxpl_multi
 *
 * Purpose:	Returns information which was set with H5Pset_dxpl_multi()
 *		above.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Tuesday, August 10, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_dxpl_multi(hid_t dxpl_id, hid_t *memb_dxpl/*out*/)
{
    H5FD_multi_dxpl_t	*dx;
    H5FD_mem_t		mt;
    static const char *func="H5FDget_dxpl_multi";  /* Function Name for error reporting */

    /*NO TRACE*/

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    if (TRUE!=H5Pisa_class(dxpl_id, H5P_DATASET_XFER))
        H5Epush_ret(func, H5E_ERR_CLS, H5E_PLIST, H5E_BADTYPE, "not a file access property list", -1)
    if (H5FD_MULTI!=H5Pget_driver(dxpl_id))
        H5Epush_ret(func, H5E_ERR_CLS, H5E_PLIST, H5E_BADVALUE, "incorrect VFL driver", -1)
    if(NULL == (dx = (H5FD_multi_dxpl_t *)H5Pget_driver_info(dxpl_id)))
        H5Epush_ret(func, H5E_ERR_CLS, H5E_PLIST, H5E_BADVALUE, "bad VFL driver info", -1)

    if (memb_dxpl) {
	for (mt=H5FD_MEM_DEFAULT; mt<H5FD_MEM_NTYPES; mt=(H5FD_mem_t)(mt+1)) {
	    if (dx->memb_dxpl[mt]>=0)
		memb_dxpl[mt] = H5Pcopy(dx->memb_dxpl[mt]);
	    else
		memb_dxpl[mt] = dx->memb_dxpl[mt]; /*default or bad ID */
	}
    }

    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_sb_size
 *
 * Purpose:	Returns the size of the private information to be stored in
 *		the superblock.
 *
 * Return:	Success:	The super block driver data size.
 *
 *		Failure:	never fails
 *
 * Programmer:	Robb Matzke
 *              Monday, August 16, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hsize_t
H5FD_multi_sb_size(H5FD_t *_file)
{
    H5FD_multi_t	*file = (H5FD_multi_t*)_file;
    unsigned		nseen = 0;
    hsize_t		nbytes = 8; /*size of header*/

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    /* How many unique files? */
    UNIQUE_MEMBERS(file->fa.memb_map, mt) {
	nseen++;
    } END_MEMBERS;

    /* Addresses and EOA markers */
    nbytes += nseen * 2 * 8;

    /* Name templates */
    UNIQUE_MEMBERS(file->fa.memb_map, mt) {
        size_t n = strlen(file->fa.memb_name[mt])+1;
        nbytes += (n+7) & ~((size_t)0x0007);
    } END_MEMBERS;

    return nbytes;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_sb_encode
 *
 * Purpose:	Encode driver information for the superblock. The NAME
 *		argument is a nine-byte buffer which will be initialized with
 *		an eight-character name/version number and null termination.
 *
 *		The encoding is a six-byte member mapping followed two bytes
 *		which are unused. For each unique file in usage-type order
 *		encode all the starting addresses as unsigned 64-bit integers,
 *		then all the EOA values as unsigned 64-bit integers, then all
 *		the template names as null terminated strings which are
 *		multiples of 8 characters.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Monday, August 16, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_multi_sb_encode(H5FD_t *_file, char *name/*out*/,
		     unsigned char *buf/*out*/)
{
    H5FD_multi_t	*file = (H5FD_multi_t*)_file;
    haddr_t		memb_eoa;
    unsigned char	*p;
    size_t		nseen;
    size_t		i;
    H5FD_mem_t		m;
    static const char *func="H5FD_multi_sb_encode";  /* Function Name for error reporting */

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    /* Name and version number */
    strncpy(name, "NCSAmulti", (size_t)8);
    name[8] = '\0';

    assert(7==H5FD_MEM_NTYPES);

    for (m=H5FD_MEM_SUPER; m<H5FD_MEM_NTYPES; m=(H5FD_mem_t)(m+1)) {
        buf[m-1] = (unsigned char)file->fa.memb_map[m];
    }
    buf[6] = 0;
    buf[7] = 0;

    /*
     * Copy the starting addresses and EOA values into the buffer in order of
     * usage type but only for types which map to something unique.
     */

    /* Encode all starting addresses and EOA values */
    nseen = 0;
    p = buf+8;
    assert(sizeof(haddr_t)<=8);
    UNIQUE_MEMBERS(file->fa.memb_map, mt) {
        memcpy(p, &(file->fa.memb_addr[mt]), sizeof(haddr_t));
        p += sizeof(haddr_t);
        memb_eoa = H5FDget_eoa(file->memb[mt], mt);
        memcpy(p, &memb_eoa, sizeof(haddr_t));
        p += sizeof(haddr_t);
        nseen++;
    } END_MEMBERS;
    if (H5Tconvert(H5T_NATIVE_HADDR, H5T_STD_U64LE, nseen*2, buf+8, NULL,
		   H5P_DEFAULT)<0)
        H5Epush_ret(func, H5E_ERR_CLS, H5E_DATATYPE, H5E_CANTCONVERT, "can't convert superblock info", -1)

    /* Encode all name templates */
    p = buf + 8 + nseen*2*8;
    UNIQUE_MEMBERS(file->fa.memb_map, mt) {
        size_t n = strlen(file->fa.memb_name[mt]) + 1;
        strcpy((char *)p, file->fa.memb_name[mt]);
        p += n;
        for (i=n; i%8; i++) *p++ = '\0';
    } END_MEMBERS;

    return 0;
} /* end H5FD_multi_sb_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_sb_decode
 *
 * Purpose:	Decodes the superblock information for this driver. The NAME
 *		argument is the eight-character (plus null termination) name
 *		stored in the file.
 *
 *		The FILE argument is updated according to the information in
 *		the superblock. This may mean that some member files are
 *		closed and others are opened.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Monday, August 16, 1999
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_multi_sb_decode(H5FD_t *_file, const char *name, const unsigned char *buf)
{
    H5FD_multi_t	*file = (H5FD_multi_t*)_file;
    char		x[2*H5FD_MEM_NTYPES*8];
    H5FD_mem_t		map[H5FD_MEM_NTYPES];
    int			i;
    size_t		nseen=0;
    hbool_t		map_changed=FALSE;
    hbool_t		in_use[H5FD_MEM_NTYPES];
    const char		*memb_name[H5FD_MEM_NTYPES];
    haddr_t		memb_addr[H5FD_MEM_NTYPES];
    haddr_t		memb_eoa[H5FD_MEM_NTYPES];
    haddr_t		*ap;
    static const char *func="H5FD_multi_sb_decode";  /* Function Name for error reporting */

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    /* Make sure the name/version number is correct */
    if (strcmp(name, "NCSAmult"))
        H5Epush_ret(func, H5E_ERR_CLS, H5E_FILE, H5E_BADVALUE, "invalid multi superblock", -1)

    /* Set default values */
    ALL_MEMBERS(mt) {
        memb_addr[mt] = HADDR_UNDEF;
        memb_eoa[mt] = HADDR_UNDEF;
        memb_name[mt] = NULL;
    } END_MEMBERS;

    /*
     * Read the map and count the unique members.
     */
    memset(map, 0, sizeof map);

    for (i=0; i<6; i++) {
        map[i+1] = (H5FD_mem_t)buf[i];
        if (file->fa.memb_map[i+1]!=map[i+1])
            map_changed=TRUE;
    }

    UNIQUE_MEMBERS(map, mt) {
        nseen++;
    } END_MEMBERS;
    buf += 8;

    /* Decode Address and EOA values */
    assert(sizeof(haddr_t)<=8);
    memcpy(x, buf, (nseen*2*8));
    buf += nseen*2*8;
    if (H5Tconvert(H5T_STD_U64LE, H5T_NATIVE_HADDR, nseen*2, x, NULL, H5P_DEFAULT)<0)
        H5Epush_ret(func, H5E_ERR_CLS, H5E_DATATYPE, H5E_CANTCONVERT, "can't convert superblock info", -1)
    ap = (haddr_t*)x;
    UNIQUE_MEMBERS(map, mt) {
        memb_addr[_unmapped] = *ap++;
        memb_eoa[_unmapped] = *ap++;
    } END_MEMBERS;

    /* Decode name templates */
    UNIQUE_MEMBERS(map, mt) {
        size_t n = strlen((const char *)buf)+1;
        memb_name[_unmapped] = (const char *)buf;
        buf += (n+7) & ~((unsigned)0x0007);
    } END_MEMBERS;

    /*
     * Use the mapping saved in the superblock in preference to the one
     * already set for the file. Since we may have opened files which are no
     * longer needed we should close all those files. We'll open the new
     * files at the end.
     */
    if (map_changed) {
#ifdef H5FD_MULTI_DEBUG
        if (file->flags & H5F_ACC_DEBUG) {
            fprintf(stderr, "H5FD_MULTI: member map override\n");
            fprintf(stderr, "    old value: ");
            ALL_MEMBERS(mt) {
                fprintf(stderr, "%s%d", mt?", ":"", (int)(file->fa.memb_map[mt]));
            } END_MEMBERS;
            fprintf(stderr, "\n    new value: ");
            ALL_MEMBERS(mt) {
                fprintf(stderr, "%s%d", mt?", ":"", (int)(map[mt]));
            } END_MEMBERS;
        }
#endif
        /* Commit map */
        ALL_MEMBERS(mt) {
            file->fa.memb_map[mt] = map[mt];
        } END_MEMBERS;

        /* Close files which are unused now */
        memset(in_use, 0, sizeof in_use);
        UNIQUE_MEMBERS(map, mt) {
            in_use[mt] = TRUE;
        } END_MEMBERS;
        ALL_MEMBERS(mt) {
            if (!in_use[mt] && file->memb[mt]) {
#ifdef H5FD_MULTI_DEBUG
                if (file->flags & H5F_ACC_DEBUG) {
                    fprintf(stderr, "H5FD_MULTI: close member %d\n", (int)mt);
                }
#endif
                (void)H5FDclose(file->memb[mt]);
                file->memb[mt] = NULL;
            }
            file->fa.memb_map[mt] = map[mt];
        } END_MEMBERS;
    }

    /* Commit member starting addresses and name templates */
    ALL_MEMBERS(mt) {
        file->fa.memb_addr[mt] = memb_addr[mt];
        if (memb_name[mt]) {
            if (file->fa.memb_name[mt])
                free(file->fa.memb_name[mt]);
            file->fa.memb_name[mt] = my_strdup(memb_name[mt]);
        }
    } END_MEMBERS;
    if (compute_next(file)<0)
        H5Epush_ret(func, H5E_ERR_CLS, H5E_INTERNAL, H5E_BADVALUE, "compute_next() failed", -1)

    /* Open all necessary files */
    if (open_members(file)<0)
        H5Epush_ret(func, H5E_ERR_CLS, H5E_INTERNAL, H5E_BADVALUE, "open_members() failed", -1)

    /* Set the EOA marker for all open files */
    UNIQUE_MEMBERS(file->fa.memb_map, mt) {
        if (file->memb[mt])
            if(H5FDset_eoa(file->memb[mt], mt, memb_eoa[mt])<0)
                H5Epush_ret(func, H5E_ERR_CLS, H5E_INTERNAL, H5E_CANTSET, "set_eoa() failed", -1)
    } END_MEMBERS;

    return 0;
} /* end H5FD_multi_sb_decode() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_fapl_get
 *
 * Purpose:	Returns a file access property list which indicates how the
 *		specified file is being accessed. The return list could be
 *		used to access another file the same way.
 *
 * Return:	Success:	Ptr to new file access property list with all
 *				members copied from the file struct.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Friday, August 13, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5FD_multi_fapl_get(H5FD_t *_file)
{
    H5FD_multi_t	*file = (H5FD_multi_t*)_file;

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    return H5FD_multi_fapl_copy(&(file->fa));
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_fapl_copy
 *
 * Purpose:	Copies the multi-specific file access properties.
 *
 * Return:	Success:	Ptr to a new property list
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5FD_multi_fapl_copy(const void *_old_fa)
{
    const H5FD_multi_fapl_t *old_fa = (const H5FD_multi_fapl_t*)_old_fa;
    H5FD_multi_fapl_t *new_fa = (H5FD_multi_fapl_t *)malloc(sizeof(H5FD_multi_fapl_t));
    int nerrors = 0;
    static const char *func="H5FD_multi_fapl_copy";  /* Function Name for error reporting */

    assert(new_fa);

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    memcpy(new_fa, old_fa, sizeof(H5FD_multi_fapl_t));
    ALL_MEMBERS(mt) {
	if (old_fa->memb_fapl[mt]>=0) {
	    new_fa->memb_fapl[mt] = H5Pcopy(old_fa->memb_fapl[mt]);
	    if (new_fa->memb_fapl[mt]<0) nerrors++;
	}
	if (old_fa->memb_name[mt]) {
	    new_fa->memb_name[mt] = (char *)malloc(strlen(old_fa->memb_name[mt])+1);
	    assert(new_fa->memb_name[mt]);
	    strcpy(new_fa->memb_name[mt], old_fa->memb_name[mt]);
	}
    } END_MEMBERS;

    if (nerrors) {
        ALL_MEMBERS(mt) {
            if (new_fa->memb_fapl[mt]>=0) (void)H5Pclose(new_fa->memb_fapl[mt]);
            if (new_fa->memb_name[mt]) free(new_fa->memb_name[mt]);
        } END_MEMBERS;
        free(new_fa);
        H5Epush_ret(func, H5E_ERR_CLS, H5E_INTERNAL, H5E_BADVALUE, "invalid freespace objects", NULL)
    }
    return new_fa;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_fapl_free
 *
 * Purpose:	Frees the multi-specific file access properties.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_multi_fapl_free(void *_fa)
{
    H5FD_multi_fapl_t	*fa = (H5FD_multi_fapl_t*)_fa;
    static const char *func="H5FD_multi_fapl_free";  /* Function Name for error reporting */

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    ALL_MEMBERS(mt) {
	if (fa->memb_fapl[mt]>=0)
            if(H5Pclose(fa->memb_fapl[mt])<0)
                H5Epush_ret(func, H5E_ERR_CLS, H5E_FILE, H5E_CANTCLOSEOBJ, "can't close property list", -1)
	if (fa->memb_name[mt])
            free(fa->memb_name[mt]);
    } END_MEMBERS;
    free(fa);

    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_dxpl_copy
 *
 * Purpose:	Copes the multi-specific data transfer properties.
 *
 * Return:	Success:	Ptr to new property list
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5FD_multi_dxpl_copy(const void *_old_dx)
{
    const H5FD_multi_dxpl_t *old_dx = (const H5FD_multi_dxpl_t*)_old_dx;
    H5FD_multi_dxpl_t *new_dx = (H5FD_multi_dxpl_t *)malloc(sizeof(H5FD_multi_dxpl_t));
    int nerrors = 0;
    static const char *func="H5FD_multi_dxpl_copy";  /* Function Name for error reporting */

    assert(new_dx);

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    memcpy(new_dx, old_dx, sizeof(H5FD_multi_dxpl_t));
    ALL_MEMBERS(mt) {
	if (old_dx->memb_dxpl[mt]>=0) {
	    new_dx->memb_dxpl[mt] = H5Pcopy(old_dx->memb_dxpl[mt]);
	    if (new_dx->memb_dxpl[mt]<0) nerrors++;
	}
    } END_MEMBERS;

    if (nerrors) {
        ALL_MEMBERS(mt) {
            (void)H5Pclose(new_dx->memb_dxpl[mt]);
        } END_MEMBERS;
        free(new_dx);
        H5Epush_ret(func, H5E_ERR_CLS, H5E_INTERNAL, H5E_BADVALUE, "invalid freespace objects", NULL)
    }
    return new_dx;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_dxpl_free
 *
 * Purpose:	Frees the multi-specific data transfer properties.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_multi_dxpl_free(void *_dx)
{
    H5FD_multi_dxpl_t	*dx = (H5FD_multi_dxpl_t*)_dx;
    static const char *func="H5FD_multi_dxpl_free";  /* Function Name for error reporting */

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    ALL_MEMBERS(mt) {
        if (dx->memb_dxpl[mt]>=0)
            if(H5Pclose(dx->memb_dxpl[mt])<0)
                H5Epush_ret(func, H5E_ERR_CLS, H5E_FILE, H5E_CANTCLOSEOBJ, "can't close property list", -1)
    } END_MEMBERS;

    free(dx);
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_open
 *
 * Purpose:	Creates and/or opens a multi HDF5 file.
 *
 * Return:	Success:	A pointer to a new file data structure. The
 *				public fields will be initialized by the
 *				caller, which is always H5FD_open().
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5FD_t *
H5FD_multi_open(const char *name, unsigned flags, hid_t fapl_id,
		haddr_t maxaddr)
{
    H5FD_multi_t	*file=NULL;
    hid_t		close_fapl=-1;
    H5FD_multi_fapl_t	*fa;
    H5FD_mem_t		m;
    static const char *func="H5FD_multi_open";  /* Function Name for error reporting */

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    /* Check arguments */
    if (!name || !*name)
        H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_BADVALUE, "invalid file name", NULL)
    if (0==maxaddr || HADDR_UNDEF==maxaddr)
        H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_BADRANGE, "bogus maxaddr", NULL)

    /*
     * Initialize the file from the file access properties, using default
     * values if necessary.
     */
    if(NULL == (file = (H5FD_multi_t *)calloc((size_t)1, sizeof(H5FD_multi_t))))
        H5Epush_ret(func, H5E_ERR_CLS, H5E_RESOURCE, H5E_NOSPACE, "memory allocation failed", NULL)
    if(H5P_FILE_ACCESS_DEFAULT==fapl_id || H5FD_MULTI!=H5Pget_driver(fapl_id)) {
        close_fapl = fapl_id = H5Pcreate(H5P_FILE_ACCESS);
        if(H5Pset_fapl_multi(fapl_id, NULL, NULL, NULL, NULL, TRUE)<0)
            H5Epush_goto(func, H5E_ERR_CLS, H5E_FILE, H5E_CANTSET, "can't set property value", error)
    }
    fa = (H5FD_multi_fapl_t *)H5Pget_driver_info(fapl_id);
    assert(fa);
    ALL_MEMBERS(mt) {
	file->fa.memb_map[mt] = fa->memb_map[mt];
	file->fa.memb_addr[mt] = fa->memb_addr[mt];
	if (fa->memb_fapl[mt]>=0)
	    file->fa.memb_fapl[mt] = H5Pcopy(fa->memb_fapl[mt]);
	else
	    file->fa.memb_fapl[mt] = fa->memb_fapl[mt];
	if (fa->memb_name[mt])
	    file->fa.memb_name[mt] = my_strdup(fa->memb_name[mt]);
	else
	    file->fa.memb_name[mt] = NULL;
    } END_MEMBERS;
    file->fa.relax = fa->relax;
    file->flags = flags;
    file->name = my_strdup(name);
    if (close_fapl>=0)
        if(H5Pclose(close_fapl)<0)
            H5Epush_goto(func, H5E_ERR_CLS, H5E_FILE, H5E_CANTCLOSEOBJ, "can't close property list", error)

    /* Compute derived properties and open member files */
    if (compute_next(file)<0)
        H5Epush_goto(func, H5E_ERR_CLS, H5E_INTERNAL, H5E_BADVALUE, "compute_next() failed", error);
    if (open_members(file)<0)
        H5Epush_goto(func, H5E_ERR_CLS, H5E_INTERNAL, H5E_BADVALUE, "open_members() failed", error);

    /* We must have opened at least the superblock file */
    if (H5FD_MEM_DEFAULT==(m=file->fa.memb_map[H5FD_MEM_SUPER]))
        m = H5FD_MEM_SUPER;
    if (NULL==file->memb[m])
        goto error;

    return (H5FD_t*)file;

error:
    /* Cleanup and fail */
    if (file) {
	ALL_MEMBERS(mt) {
	    if (file->memb[mt]) (void)H5FDclose(file->memb[mt]);
	    if (file->fa.memb_fapl[mt]>=0) (void)H5Pclose(file->fa.memb_fapl[mt]);
	    if (file->fa.memb_name[mt]) free(file->fa.memb_name[mt]);
	} END_MEMBERS;
	if (file->name) free(file->name);
	free(file);
    }
    return NULL;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_close
 *
 * Purpose:	Closes a multi file.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative with as many members closed as
 *				possible. The only subsequent operation
 *				permitted on the file is a close operation.
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_multi_close(H5FD_t *_file)
{
    H5FD_multi_t	*file = (H5FD_multi_t*)_file;
    int			nerrors=0;
    static const char *func="H5FD_multi_close";  /* Function Name for error reporting */

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    /* Close as many members as possible */
    ALL_MEMBERS(mt) {
	if (file->memb[mt]) {
#ifdef H5FD_MULTI_DEBUG
	    if (file->flags & H5F_ACC_DEBUG) {
		fprintf(stderr, "H5FD_MULTI: closing member %d\n", (int)mt);
	    }
#endif
	    if (H5FDclose(file->memb[mt])<0) {
#ifdef H5FD_MULTI_DEBUG
		if (file->flags & H5F_ACC_DEBUG) {
		    fprintf(stderr, "H5FD_MULTI: close failed\n");
		}
#endif
		nerrors++;
	    } else {
		file->memb[mt] = NULL;
	    }
	}
    } END_MEMBERS;
    if (nerrors)
        H5Epush_ret(func, H5E_ERR_CLS, H5E_INTERNAL, H5E_BADVALUE, "error closing member files", -1)

    /* Clean up other stuff */
    ALL_MEMBERS(mt) {
	if (file->fa.memb_fapl[mt]>=0) (void)H5Pclose(file->fa.memb_fapl[mt]);
	if (file->fa.memb_name[mt]) free(file->fa.memb_name[mt]);
    } END_MEMBERS;
    free(file->name);
    free(file);
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_cmp
 *
 * Purpose:	Compares two file families to see if they are the same. It
 *		does this by comparing the first common member of the two
 *		families.  If the families have no members in common then the
 *		file with the earliest member is smaller than the other file.
 *		We abort if neither file has any members.
 *
 * Return:	Success:	like strcmp()
 *
 *		Failure:	never fails (arguments were checked by the
 *				caller).
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
H5FD_multi_cmp(const H5FD_t *_f1, const H5FD_t *_f2)
{
    const H5FD_multi_t	*f1 = (const H5FD_multi_t*)_f1;
    const H5FD_multi_t	*f2 = (const H5FD_multi_t*)_f2;
    H5FD_mem_t		out_mt = H5FD_MEM_DEFAULT;
    int			cmp=0;

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    ALL_MEMBERS(mt) {
        out_mt = mt;
	if (f1->memb[mt] && f2->memb[mt]) break;
	if (!cmp) {
	    if (f1->memb[mt]) cmp = -1;
	    else if (f2->memb[mt]) cmp = 1;
	}
    } END_MEMBERS;
    assert(cmp || out_mt<H5FD_MEM_NTYPES);
    if (out_mt>=H5FD_MEM_NTYPES) return cmp;

    return H5FDcmp(f1->memb[out_mt], f2->memb[out_mt]);
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_query
 *
 * Purpose:	Set the flags that this VFL driver is capable of supporting.
 *              (listed in H5FDpublic.h)
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, September 26, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_multi_query(const H5FD_t *_f, unsigned long *flags /* out */)
{
    /* Shut compiler up */
    _f=_f;

    /* Set the VFL feature flags that this driver supports */
    if(flags) {
        *flags = 0;
        *flags |= H5FD_FEAT_DATA_SIEVE;       /* OK to perform data sieving for faster raw data reads & writes */
        *flags |= H5FD_FEAT_AGGREGATE_SMALLDATA; /* OK to aggregate "small" raw data allocations */
    } /* end if */

    return(0);
} /* end H5FD_multi_query() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_get_type_map
 *
 * Purpose:	Retrieve the memory type mapping for this file
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, October  9, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_multi_get_type_map(const H5FD_t *_file, H5FD_mem_t *type_map)
{
    const H5FD_multi_t	*file = (const H5FD_multi_t*)_file;

    /* Copy file's free space type mapping */
    memcpy(type_map, file->fa.memb_map, sizeof(file->fa.memb_map));

    return(0);
} /* end H5FD_multi_get_type_map() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_get_eoa
 *
 * Purpose:	Returns the end-of-address marker for the file. The EOA
 *		marker is the first address past the last byte allocated in
 *		the format address space.
 *
 * Return:	Success:	The end-of-address-marker
 *
 *		Failure:	HADDR_UNDEF
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *              Raymond Lu
 *              21 Dec. 2006
 *              Added the parameter TYPE.  It's only used for MULTI driver.
 *              If the TYPE is H5FD_MEM_DEFAULT, simply find the biggest
 *              EOA of individual file because the EOA for the whole file
 *              is meaningless.
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_multi_get_eoa(const H5FD_t *_file, H5FD_mem_t type)
{
    const H5FD_multi_t	*file = (const H5FD_multi_t*)_file;
    haddr_t eoa = 0;
    haddr_t memb_eoa = 0;
    static const char *func="H5FD_multi_get_eoa";  /* Function Name for error reporting */

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    /* The library used to have EOA for the whole file.  But it's
     * taken out because it makes little sense for MULTI files.
     * However, the library sometimes queries it through H5F_get_eoa.
     * Here the code finds the biggest EOA for individual file if
     * the query is for TYPE == H5FD_MEM_DEFAULT.
     */
    if(H5FD_MEM_DEFAULT == type) {
        UNIQUE_MEMBERS(file->fa.memb_map, mt) {
	    if (file->memb[mt]) {
                /* Retrieve EOA */
	        H5E_BEGIN_TRY {
                    memb_eoa = H5FDget_eoa(file->memb[mt], mt);
	        } H5E_END_TRY;

	        if (HADDR_UNDEF==memb_eoa)
                    H5Epush_ret(func, H5E_ERR_CLS, H5E_INTERNAL, H5E_BADVALUE, "member file has unknown eoa", HADDR_UNDEF)
	    } else if (file->fa.relax) {
	        /*
	         * The member is not open yet (maybe it doesn't exist). Make the
	         * best guess about the end-of-file.
	         */
	        memb_eoa = file->memb_next[mt];
	        assert(HADDR_UNDEF!=memb_eoa);
	    } else {
                H5Epush_ret(func, H5E_ERR_CLS, H5E_INTERNAL, H5E_BADVALUE, "bad eoa", HADDR_UNDEF)
	    }

            if(memb_eoa > eoa)
                eoa = memb_eoa;
        } END_MEMBERS;
    } else {
        H5FD_mem_t mmt = file->fa.memb_map[type];
        if (H5FD_MEM_DEFAULT==mmt) mmt = type;

	if (file->memb[mmt]) {
            H5E_BEGIN_TRY {
	        eoa = H5FDget_eoa(file->memb[mmt], mmt);
            } H5E_END_TRY;

	    if (HADDR_UNDEF==eoa)
                H5Epush_ret(func, H5E_ERR_CLS, H5E_INTERNAL, H5E_BADVALUE, "member file has unknown eoa", HADDR_UNDEF)
	    if (eoa>0) eoa += file->fa.memb_addr[mmt];
	} else if (file->fa.relax) {
	    /*
	     * The member is not open yet (maybe it doesn't exist). Make the
	     * best guess about the end-of-file.
	     */
	    eoa = file->memb_next[mmt];
	    assert(HADDR_UNDEF!=eoa);
	 } else {
            H5Epush_ret(func, H5E_ERR_CLS, H5E_INTERNAL, H5E_BADVALUE, "bad eoa", HADDR_UNDEF)
	 }
    }

    return eoa;
} /* end H5FD_multi_get_eoa() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_set_eoa
 *
 * Purpose:	Set the end-of-address marker for the file by savig the new
 *		EOA value in the file struct. Also set the EOA marker for the
 *		subfile in which the new EOA value falls. We don't set the
 *		EOA values of any other subfiles.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *              Raymond Lu
 *              10 January 2007
 *              EOA for the whole file is discarded because it's meaningless
 *              for MULTI file.  This function only sets eoa for individual
 *              file.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_multi_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t eoa)
{
    H5FD_multi_t	*file = (H5FD_multi_t*)_file;
    H5FD_mem_t		mmt;
    herr_t		status;
    static const char *func="H5FD_multi_set_eoa";  /* Function Name for error reporting */

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    mmt = file->fa.memb_map[type];
    if(H5FD_MEM_DEFAULT == mmt)
        mmt = type;

    assert(eoa >= file->fa.memb_addr[mmt]);
    assert(eoa < file->memb_next[mmt]);

    H5E_BEGIN_TRY {
	status = H5FDset_eoa(file->memb[mmt], mmt, (eoa - file->fa.memb_addr[mmt]));
    } H5E_END_TRY;
    if (status<0)
        H5Epush_ret(func, H5E_ERR_CLS, H5E_FILE, H5E_BADVALUE, "member H5FDset_eoa failed", -1)

    return 0;
} /* end H5FD_multi_set_eoa() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_get_eof
 *
 * Purpose:	Returns the end-of-file marker, which is the greater of
 *		either the total multi size or the current EOA marker.
 *
 * Return:	Success:	End of file address, the first address past
 *				the end of the multi of files or the current
 *				EOA, whichever is larger.
 *
 *		Failure:      	HADDR_UNDEF
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *              Raymond Lu
 *              5 January 2007
 *              Multi driver no longer has EOA for the whole file.  Calculate
 *              it in the same way as EOF instead.
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_multi_get_eof(const H5FD_t *_file)
{
    const H5FD_multi_t	*file = (const H5FD_multi_t*)_file;
    haddr_t		eof=0, tmp_eof;
    haddr_t		eoa=0, tmp_eoa;
    static const char *func="H5FD_multi_get_eof";  /* Function Name for error reporting */

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    UNIQUE_MEMBERS(file->fa.memb_map, mt) {
	if (file->memb[mt]) {
            /* Retrieve EOF */
	    H5E_BEGIN_TRY {
		tmp_eof = H5FDget_eof(file->memb[mt]);
	    } H5E_END_TRY;

	    if (HADDR_UNDEF==tmp_eof)
                H5Epush_ret(func, H5E_ERR_CLS, H5E_INTERNAL, H5E_BADVALUE, "member file has unknown eof", HADDR_UNDEF)
	    if (tmp_eof>0) tmp_eof += file->fa.memb_addr[mt];

            /* Retrieve EOA */
	    H5E_BEGIN_TRY {
		tmp_eoa = H5FDget_eoa(file->memb[mt], mt);
	    } H5E_END_TRY;

	    if (HADDR_UNDEF==tmp_eoa)
                H5Epush_ret(func, H5E_ERR_CLS, H5E_INTERNAL, H5E_BADVALUE, "member file has unknown eoa", HADDR_UNDEF)
	    if (tmp_eoa>0) tmp_eoa += file->fa.memb_addr[mt];
	} else if (file->fa.relax) {
	    /*
	     * The member is not open yet (maybe it doesn't exist). Make the
	     * best guess about the end-of-file.
	     */
	    tmp_eof = file->memb_next[mt];
	    assert(HADDR_UNDEF!=tmp_eof);

	    tmp_eoa = file->memb_next[mt];
	    assert(HADDR_UNDEF!=tmp_eoa);
	} else {
            H5Epush_ret(func, H5E_ERR_CLS, H5E_INTERNAL, H5E_BADVALUE, "bad eof", HADDR_UNDEF)
	}

	if (tmp_eof>eof) eof = tmp_eof;
	if (tmp_eoa>eoa) eoa = tmp_eoa;
    } END_MEMBERS;

    return MAX(eoa, eof);
}


/*-------------------------------------------------------------------------
 * Function:       H5FD_multi_get_handle
 *
 * Purpose:        Returns the file handle of MULTI file driver.
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
H5FD_multi_get_handle(H5FD_t *_file, hid_t fapl, void** file_handle)
{
    H5FD_multi_t        *file = (H5FD_multi_t *)_file;
    H5FD_mem_t          type, mmt;
    static const char   *func="H5FD_multi_get_handle";  /* Function Name for error reporting */

    /* Get data type for multi driver */
    if(H5Pget_multi_type(fapl, &type) < 0)
        H5Epush_ret(func, H5E_ERR_CLS, H5E_INTERNAL, H5E_BADVALUE, "can't get data type for multi driver", -1)
    if(type<H5FD_MEM_DEFAULT || type>=H5FD_MEM_NTYPES)
        H5Epush_ret(func, H5E_ERR_CLS, H5E_INTERNAL, H5E_BADVALUE, "data type is out of range", -1)
    mmt = file->fa.memb_map[type];
    if(H5FD_MEM_DEFAULT==mmt) mmt = type;

    return (H5FDget_vfd_handle(file->memb[mmt], fapl, file_handle));
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_alloc
 *
 * Purpose:	Allocate file memory.
 *
 * Return:	Success:	Address of new memory
 *
 *		Failure:	HADDR_UNDEF
 *
 * Programmer:	Robb Matzke
 *              Thursday, August 12, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_multi_alloc(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, hsize_t size)
{
    H5FD_multi_t	*file = (H5FD_multi_t*)_file;
    H5FD_mem_t		mmt;
    haddr_t		addr;
    static const char *func="H5FD_multi_alloc";  /* Function Name for error reporting */

    mmt = file->fa.memb_map[type];
    if (H5FD_MEM_DEFAULT==mmt) mmt = type;

    if (HADDR_UNDEF==(addr=H5FDalloc(file->memb[mmt], mmt, dxpl_id, size)))
        H5Epush_ret(func, H5E_ERR_CLS, H5E_INTERNAL, H5E_BADVALUE, "member file can't alloc", HADDR_UNDEF)
    addr += file->fa.memb_addr[mmt];

/*#ifdef TMP
    if ( addr + size > file->eoa ) {

	if ( H5FD_multi_set_eoa(_file, addr + size) < 0 ) {

            H5Epush_ret(func, H5E_ERR_CLS, H5E_INTERNAL, H5E_BADVALUE, \
			"can't set eoa", HADDR_UNDEF)
	}
    }
#else
    if ( addr + size > file->eoa )
	file->eoa = addr + size;
#endif */

    return addr;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_free
 *
 * Purpose:	Frees memory
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, August 12, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_multi_free(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, hsize_t size)
{
    H5FD_multi_t	*file = (H5FD_multi_t*)_file;
    H5FD_mem_t		mmt;

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    mmt = file->fa.memb_map[type];
    if (H5FD_MEM_DEFAULT==mmt) mmt = type;

    assert(addr>=file->fa.memb_addr[mmt]);
    assert(addr+size<=file->memb_next[mmt]);
    return H5FDfree(file->memb[mmt], mmt, dxpl_id, addr-file->fa.memb_addr[mmt], size);
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_read
 *
 * Purpose:	Reads SIZE bytes of data from FILE beginning at address ADDR
 *		into buffer BUF according to data transfer properties in
 *		DXPL_ID.
 *
 * Return:	Success:	Zero. Result is stored in caller-supplied
 *				buffer BUF.
 *
 *		Failure:	-1, contents of buffer BUF are undefined.
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_multi_read(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t size,
		 void *_buf/*out*/)
{
    H5FD_multi_t	*file = (H5FD_multi_t*)_file;
    H5FD_multi_dxpl_t	*dx=NULL;
    H5FD_mem_t		mt, mmt, hi=H5FD_MEM_DEFAULT;
    haddr_t		start_addr=0;

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    /* Get the data transfer properties */
    if(H5P_FILE_ACCESS_DEFAULT != dxpl_id && H5FD_MULTI == H5Pget_driver(dxpl_id))
	dx = (H5FD_multi_dxpl_t *)H5Pget_driver_info(dxpl_id);

    /* Find the file to which this address belongs */
    for(mt = H5FD_MEM_SUPER; mt < H5FD_MEM_NTYPES; mt = (H5FD_mem_t)(mt + 1)) {
	mmt = file->fa.memb_map[mt];
	if(H5FD_MEM_DEFAULT == mmt)
            mmt = mt;
	assert(mmt > 0 && mmt < H5FD_MEM_NTYPES);

	if (file->fa.memb_addr[mmt]>addr) continue;
	if (file->fa.memb_addr[mmt]>=start_addr) {
	    start_addr = file->fa.memb_addr[mmt];
	    hi = mmt;
	}
    }
    assert(hi>0);

    /* Read from that member */
    return H5FDread(file->memb[hi], type, dx?dx->memb_dxpl[hi]:H5P_DEFAULT,
		    addr-start_addr, size, _buf);
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_write
 *
 * Purpose:	Writes SIZE bytes of data to FILE beginning at address ADDR
 *		from buffer BUF according to data transfer properties in
 *		DXPL_ID.
 *
 * Return:	Success:	Zero
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_multi_write(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t size,
		  const void *_buf)
{
    H5FD_multi_t	*file = (H5FD_multi_t*)_file;
    H5FD_multi_dxpl_t	*dx=NULL;
    H5FD_mem_t		mt, mmt, hi=H5FD_MEM_DEFAULT;
    haddr_t		start_addr=0;

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    /* Get the data transfer properties */
    if(H5P_FILE_ACCESS_DEFAULT != dxpl_id && H5FD_MULTI == H5Pget_driver(dxpl_id))
	dx = (H5FD_multi_dxpl_t *)H5Pget_driver_info(dxpl_id);

    /* Find the file to which this address belongs */
    for(mt = H5FD_MEM_SUPER; mt < H5FD_MEM_NTYPES; mt = (H5FD_mem_t)(mt + 1)) {
	mmt = file->fa.memb_map[mt];
	if(H5FD_MEM_DEFAULT == mmt)
            mmt = mt;
	assert(mmt>0 && mmt<H5FD_MEM_NTYPES);

	if (file->fa.memb_addr[mmt]>addr) continue;
	if (file->fa.memb_addr[mmt]>=start_addr) {
	    start_addr = file->fa.memb_addr[mmt];
	    hi = mmt;
	}
    }
    assert(hi>0);

    /* Write to that member */
    return H5FDwrite(file->memb[hi], type, dx?dx->memb_dxpl[hi]:H5P_DEFAULT,
		     addr-start_addr, size, _buf);
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_flush
 *
 * Purpose:	Flushes all multi members.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1, as many files flushed as possible.
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_multi_flush(H5FD_t *_file, hid_t dxpl_id, unsigned closing)
{
    H5FD_multi_t	*file = (H5FD_multi_t*)_file;
    H5FD_mem_t		mt;
    int			nerrors=0;
    static const char *func="H5FD_multi_flush";  /* Function Name for error reporting */

#if 0
    H5FD_mem_t		mmt;

    /* Debugging stuff... */
    fprintf(stderr, "multifile access information:\n");

    /* print the map */
    fprintf(stderr, "    map=");
    for (mt=1; mt<H5FD_MEM_NTYPES; mt++) {
	mmt = file->memb_map[mt];
	if (H5FD_MEM_DEFAULT==mmt) mmt = mt;
	fprintf(stderr, "%s%d", 1==mt?"":",", (int)mmt);
    }
    fprintf(stderr, "\n");

    /* print info about each file */
    fprintf(stderr, "      File             Starting            Allocated                 Next Member\n");
    fprintf(stderr, "    Number              Address                 Size              Address Name\n");
    fprintf(stderr, "    ------ -------------------- -------------------- -------------------- ------------------------------\n");

    for (mt=1; mt<H5FD_MEM_NTYPES; mt++) {
	if (HADDR_UNDEF!=file->memb_addr[mt]) {
	    haddr_t eoa = H5FDget_eoa(file->memb[mt], mt);
	    fprintf(stderr, "    %6d %20llu %20llu %20llu %s\n",
		    (int)mt, (unsigned long long)(file->memb_addr[mt]),
		    (unsigned long long)eoa,
		    (unsigned long long)(file->memb_next[mt]),
		    file->memb_name[mt]);
	}
    }
#endif

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    /* Flush each file */
    for (mt=H5FD_MEM_SUPER; mt<H5FD_MEM_NTYPES; mt=(H5FD_mem_t)(mt+1)) {
	if (file->memb[mt]) {
	    H5E_BEGIN_TRY {
		if (H5FDflush(file->memb[mt],dxpl_id,closing)<0) nerrors++;
	    } H5E_END_TRY;
	}
    }
    if (nerrors)
        H5Epush_ret(func, H5E_ERR_CLS, H5E_INTERNAL, H5E_BADVALUE, "error flushing member files", -1)

    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_truncate
 *
 * Purpose:	Truncates all multi members.
 *
 * Return:	Success:	0
 *		Failure:	-1, as many files truncated as possible.
 *
 * Programmer:	Quincey Koziol
 *              Thursday, January 31, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_multi_truncate(H5FD_t *_file, hid_t dxpl_id, hbool_t closing)
{
    H5FD_multi_t	*file = (H5FD_multi_t*)_file;
    H5FD_mem_t		mt;
    int			nerrors=0;
    static const char *func="H5FD_multi_truncate";  /* Function Name for error reporting */

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    /* Truncate each file */
    for(mt = H5FD_MEM_SUPER; mt < H5FD_MEM_NTYPES; mt = (H5FD_mem_t)(mt + 1)) {
	if(file->memb[mt]) {
	    H5E_BEGIN_TRY {
		if(H5FDtruncate(file->memb[mt], dxpl_id, closing) < 0)
                    nerrors++;
	    } H5E_END_TRY;
	}
    }
    if(nerrors)
        H5Epush_ret(func, H5E_ERR_CLS, H5E_INTERNAL, H5E_BADVALUE, "error truncating member files", -1)

    return 0;
} /* end H5FD_multi_truncate() */


/*-------------------------------------------------------------------------
 * Function:	compute_next
 *
 * Purpose:	Compute the memb_next[] values of the file based on the
 *		file's member map and the member starting addresses.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Monday, August 23, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
compute_next(H5FD_multi_t *file)
{
    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    ALL_MEMBERS(mt) {
	file->memb_next[mt] = HADDR_UNDEF;
    } END_MEMBERS;

    UNIQUE_MEMBERS(file->fa.memb_map, mt1) {
	UNIQUE_MEMBERS(file->fa.memb_map, mt2) {
	    if (file->fa.memb_addr[mt1]<file->fa.memb_addr[mt2] &&
		(HADDR_UNDEF==file->memb_next[mt1] ||
		 file->memb_next[mt1]>file->fa.memb_addr[mt2])) {
		file->memb_next[mt1] = file->fa.memb_addr[mt2];
	    }
	} END_MEMBERS;
	if (HADDR_UNDEF==file->memb_next[mt1]) {
	    file->memb_next[mt1] = HADDR_MAX; /*last member*/
	}
    } END_MEMBERS;

    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	open_members
 *
 * Purpose:	Opens all members which are not opened yet.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Monday, August 23, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
open_members(H5FD_multi_t *file)
{
    char	tmp[1024];
    int		nerrors=0;
    static const char *func="(H5FD_multi)open_members";  /* Function Name for error reporting */

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    UNIQUE_MEMBERS(file->fa.memb_map, mt) {
	if (file->memb[mt]) continue; /*already open*/
	assert(file->fa.memb_name[mt]);
	sprintf(tmp, file->fa.memb_name[mt], file->name);

#ifdef H5FD_MULTI_DEBUG
	if (file->flags & H5F_ACC_DEBUG) {
	    fprintf(stderr, "H5FD_MULTI: open member %d \"%s\"\n",
		    (int)mt, tmp);
	}
#endif
	H5E_BEGIN_TRY {
	    file->memb[mt] = H5FDopen(tmp, file->flags, file->fa.memb_fapl[mt],
				      HADDR_UNDEF);
	} H5E_END_TRY;
	if (!file->memb[mt]) {
#ifdef H5FD_MULTI_DEBUG
	    if (file->flags & H5F_ACC_DEBUG) {
		fprintf(stderr, "H5FD_MULTI: open failed for member %d\n",
			(int)mt);
	    }
#endif
	    if (!file->fa.relax || (file->flags & H5F_ACC_RDWR)) {
		nerrors++;
	    }
	}
    } END_MEMBERS;
    if (nerrors)
        H5Epush_ret(func, H5E_ERR_CLS, H5E_INTERNAL, H5E_BADVALUE, "error opening member files", -1)

    return 0;
}


/*-------------------------------------------------------------------------
 * Function:    H5FD_multi_aio_alloc_ctlblk
 *
 * Purpose:     Allocate a control block for use in an asynchronous
 *              read, write, or fsync.
 *
 *              If successful, return a pointer to the newly allocated
 *              and initialized control block in *ctlblk_ptr_ptr.
 *
 * Return:      Success:        zero
 *
 *              Failure:        Negative
 *
 * Programmer:  John Mainzer
 *              6/20/10
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5FD_multi_aio_alloc_ctlblk(H5FD_multi_aio_ctlblk_t **ctlblk_ptr_ptr)
{
    static const char       * func = "H5FD_multi_aio_alloc_ctlblk"; 
    int			      i;
    H5FD_multi_aio_ctlblk_t * ctlblk_ptr = NULL;

    if ( ( ctlblk_ptr_ptr == NULL ) ||
         ( *ctlblk_ptr_ptr != NULL ) ) {

        H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_BADVALUE, \
                    "bad ctlblk_ptr_ptr on entry", -1)
    }

    ctlblk_ptr = (H5FD_multi_aio_ctlblk_t *)
                 malloc(sizeof(H5FD_multi_aio_ctlblk_t));

    if ( ctlblk_ptr == NULL ) {

        H5Epush_ret(func, H5E_ERR_CLS, H5E_RESOURCE, H5E_NOSPACE, \
                    "memory allocation failed", -1)
    }

    ctlblk_ptr->magic          = H5FD_MULTI_AIO_CTLBLK_T__MAGIC;
    ctlblk_ptr->num_subctlblks = 0;

    for ( i = 0; i < H5FD_MEM_NTYPES; i++ ) 
    {
        (ctlblk_ptr->sub_ctlblks[i]).driver   = NULL;
        (ctlblk_ptr->sub_ctlblks[i]).ctlblk   = NULL;
        (ctlblk_ptr->sub_ctlblks[i]).done     = FALSE;
        (ctlblk_ptr->sub_ctlblks[i]).finished = FALSE;
    }

    return(0);

} /* H5FD_multi_aio_alloc_ctlblk() */


/*-------------------------------------------------------------------------
 *
 * Purpose:     Free the control block pointed to by ctlblk_ptr, marking
 *              it as invalid in passing.
 *
 * Return:      Success:        zero
 *
 *              Failure:        Negative
 *
 * Programmer:  John Mainzer
 *              6/20/10
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5FD_multi_aio_discard_ctlblk(H5FD_multi_aio_ctlblk_t *ctlblk_ptr)
{
    static const char * func = "H5FD_multi_aio_discard_ctlblk"; 
    int                 i;

    if ( ctlblk_ptr == NULL ) {

        H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_BADVALUE, \
                    "ctlblk_ptr NULL on entry", -1)
    }

    if ( ctlblk_ptr->magic != H5FD_MULTI_AIO_CTLBLK_T__MAGIC ) {

        H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_BADVALUE, \
                    "bad ctlblk magic", -1)
    }

    ctlblk_ptr->magic          = 0;
    ctlblk_ptr->num_subctlblks = 0;

    for ( i = 0; i < H5FD_MEM_NTYPES; i++ ) 
    {
        (ctlblk_ptr->sub_ctlblks[i]).driver   = NULL;
        (ctlblk_ptr->sub_ctlblks[i]).ctlblk   = NULL;
        (ctlblk_ptr->sub_ctlblks[i]).done     = FALSE;
        (ctlblk_ptr->sub_ctlblks[i]).finished = FALSE;
    }

    free((void *)ctlblk_ptr);

    return(0);

} /* H5FD_multi_aio_discard_ctlblk */


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_aio_read
 *
 * Purpose:     Initiate an asynchronous read from the indicated file of
 *              the specified number of bytes starting at the specified
 *              offset and loading the data  into the provided buffer.
 *
 *              The buffer must be large enough to contain the requested
 *              data, and is undefined and must not be read or modified
 *              until the read completes successfully.  Completion is
 *              determined via either a call to H5FD_multi_aio_test() or a
 *              call to H5FD_multi_aio_wait(), and success via a call to
 *              H5FD_multi_aio_finish().
 *
 *              If successful, the H5FD_multi_aio_read routine will return 
 *		a pointer to an internal control block in *ctlblk_ptr_ptr.  
 *		This pointer must be used in all subsequent 
 *		H5FD_multi_aio_test() / H5FD_multi_aio_wait() / 
 *		H5FD_multiaio_finish() calls referring to this request.
 *
 *              Note that a successful return from this function does not
 *              imply a successful read -- simply that no errors were
 *              immediately evident.  
 *
 *		As the multi file driver does no direct file I/O itself,
 *		it must pass the aio read call through to the appropriate 
 *		file driver if that driver supports AIO, or translate 
 *		the call into a synchronous read if it doesn't.  In either
 *		case, it must construct its own AIO control block, and 
 *		pass its address back to the caller if successful.
 *
 *		Note that all calls to the underlying files will be 
 *		through the public H5FD interface, which will fake 
 *		AIO if it is not supported.  Thus, this function 
 *		presumes that AIO is supported.
 *
 *		Determine the correct underlying driver, and pass the 
 *		call through to it.
 *
 *		If the call is successful, allocate an instance of 
 *		H5FD_sec2_multi_discard_ctlblk, load the address of 
 *		the target driver's instance of H5FD_t into it along with 
 *		address	of the control block returned by the driver, and 
 *		return the address of new instance of 
 *		H5FD_sec2_multi_discard_ctlblk to the caller.
 *
 *		If the call is not successful, simply fail.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              6/12/10
 *
 * Changes:	None.
 *
 *-------------------------------------------------------------------------
 */

static herr_t 
H5FD_multi_aio_read(H5FD_t *file, 
                    H5FD_mem_t type, 
                    hid_t dxpl_id,
                    haddr_t addr, 
                    size_t size, 
                    void *buffer,
                    void **ctlblk_ptr_ptr)
{
    static const char       * func = "H5FD_multi_aio_read"; 
    herr_t		      result;
    H5FD_multi_dxpl_t       * dx = NULL;
    H5FD_mem_t                mt;
    H5FD_mem_t                mmt;
    H5FD_mem_t                hi = H5FD_MEM_DEFAULT;
    haddr_t                   start_addr = 0;
    H5FD_multi_t            * multi_file;
    H5FD_t                  * tgt_file;
    void                    * subctlblk_ptr = NULL;
    H5FD_multi_aio_ctlblk_t * ctlblk_ptr;

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    if ( ( file == NULL ) ||
         ( file->cls == NULL ) ||
         ( addr == HADDR_UNDEF ) ||
         ( size <= 0 ) ||
         ( buffer == NULL ) ||
         ( ctlblk_ptr_ptr == NULL ) ||
         ( *ctlblk_ptr_ptr != NULL ) ) {

        H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                    "bad arg(s) on entry.", -1)
    }

    multi_file = (H5FD_multi_t *)file;

    /* Get the data transfer properties */
    if ( ( H5P_FILE_ACCESS_DEFAULT != dxpl_id ) && 
         ( H5FD_MULTI == H5Pget_driver(dxpl_id) ) ) {

        dx = (H5FD_multi_dxpl_t *)H5Pget_driver_info(dxpl_id);
    }


    /* find the file to which this address belongs */
    for ( mt = H5FD_MEM_SUPER; 
          mt < H5FD_MEM_NTYPES; 
          mt = (H5FD_mem_t)(mt + 1) ) {

        mmt = multi_file->fa.memb_map[mt];

        if ( H5FD_MEM_DEFAULT == mmt ) {

            mmt = mt;
        }

        assert ( ( mmt > 0 ) && ( mmt < H5FD_MEM_NTYPES ) );

        if ( multi_file->fa.memb_addr[mmt] > addr ) {

            continue;
        }

        if ( multi_file->fa.memb_addr[mmt] >= start_addr ) {

            start_addr = multi_file->fa.memb_addr[mmt];
            hi = mmt;
        }
    }

    assert ( hi > 0 );

    tgt_file = multi_file->memb[hi];

    /* pass on the aio read */
    result = H5FDaio_read(tgt_file, type, 
                          (dx ? dx->memb_dxpl[hi] : H5P_DEFAULT),
                          (addr - start_addr), size, buffer, &subctlblk_ptr);

    if ( result < 0 ) { /* failure */

        H5Epush_ret(func, H5E_ERR_CLS, H5E_VFL, H5E_AIOREADERROR, \
                    "aio read request failed", -1)
    }

    assert( subctlblk_ptr != NULL );

    /* allocate the control block */

    result = H5FD_multi_aio_alloc_ctlblk(&ctlblk_ptr);

    if ( result < 0 ) { /* failure */

        H5Epush_ret(func, H5E_ERR_CLS, H5E_RESOURCE, H5E_NOSPACE, \
                    "can't allocate aio control block", -1)
    }

    assert( ctlblk_ptr != NULL );
    assert( ctlblk_ptr->magic == H5FD_MULTI_AIO_CTLBLK_T__MAGIC );

    ctlblk_ptr->num_subctlblks = 1;

    (ctlblk_ptr->sub_ctlblks[0]).driver   = tgt_file;
    (ctlblk_ptr->sub_ctlblks[0]).ctlblk   = subctlblk_ptr;
    (ctlblk_ptr->sub_ctlblks[0]).done     = FALSE;
    (ctlblk_ptr->sub_ctlblks[0]).finished = FALSE;

    *ctlblk_ptr_ptr = ctlblk_ptr;

    return(0);

} /* end H5FD_multi_aio_read() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_aio_write
 *
 * Purpose:     Initiate an asynchronous write to the indicated file of
 *              the specified number of bytes from the supplied  buffer
 *              to the indicated location.
 *
 *              The buffer must not be discarded or modified until the
 *              write completes successfully.  Completion is determined
 *              via either H5FD_multi_aio_test() or H5FD_multi_aio_wait(), 
 *		and success via H5FD_multi_aio_finish().
 *
 *              If successful, the H5FD_aio_write routine will return a 
 *		pointer to an internal control block in *ctlblk_ptr_ptr.  
 *		This pointer must be used in all subsequent 
 *		H5FD_multi_aio_test() / H5FD_multi_aio_wait() / 
 *		H5FD_multi_aio_finish() calls referring to this request.
 *
 *              Note that a successful return from this function does not
 *              imply a successful write -- simply that no errors were
 *              immediately evident.
 *
 *		Note that all calls to the underlying files will be 
 *		through the public H5FD interface, which will fake 
 *		AIO if it is not supported.  Thus, this function 
 *		presumes that AIO is supported.
 *
 *		Determine the correct underlying driver, and pass the 
 *		call through to it.
 *
 *		If the call is successful, allocate an instance of 
 *		H5FD_sec2_multi_discard_ctlblk, load the address of 
 *		the target driver's instance of H5FD_t into it along with 
 *		address	of the control block returned by the driver, and 
 *		return the address of new instance of 
 *		H5FD_sec2_multi_discard_ctlblk to the caller.
 *
 *		If the call is not successful, simply fail.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              6/12/10
 *
 * Changes: 	None.
 *
 *-------------------------------------------------------------------------
 */

static herr_t 
H5FD_multi_aio_write(H5FD_t *file, 
                     H5FD_mem_t type, 
                     hid_t dxpl_id,
                     haddr_t addr, 
                     size_t size, 
                     void *buffer,
                     void **ctlblk_ptr_ptr)
{
    static const char       * func = "H5FD_multi_aio_write"; 
    herr_t		      result;
    H5FD_multi_dxpl_t       * dx = NULL;
    H5FD_mem_t                mt;
    H5FD_mem_t                mmt;
    H5FD_mem_t                hi = H5FD_MEM_DEFAULT;
    haddr_t                   start_addr = 0;
    H5FD_multi_t            * multi_file;
    H5FD_t                  * tgt_file;
    void                    * subctlblk_ptr = NULL;
    H5FD_multi_aio_ctlblk_t * ctlblk_ptr;

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    if ( ( file == NULL ) ||
         ( file->cls == NULL ) ||
         ( addr == HADDR_UNDEF ) ||
         ( size <= 0 ) ||
         ( buffer == NULL ) ||
         ( ctlblk_ptr_ptr == NULL ) ||
         ( *ctlblk_ptr_ptr != NULL ) ) {

        H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                    "bad arg(s) on entry.", -1)
    }

    multi_file = (H5FD_multi_t *)file;

    /* Get the data transfer properties */
    if ( ( H5P_FILE_ACCESS_DEFAULT != dxpl_id ) && 
         ( H5FD_MULTI == H5Pget_driver(dxpl_id) ) ) {

        dx = (H5FD_multi_dxpl_t *)H5Pget_driver_info(dxpl_id);
    }

    /* find the file to which this address belongs */
    for ( mt = H5FD_MEM_SUPER; 
          mt < H5FD_MEM_NTYPES; 
          mt = (H5FD_mem_t)(mt + 1) ) {

        mmt = multi_file->fa.memb_map[mt];

        if ( H5FD_MEM_DEFAULT == mmt ) {

            mmt = mt;
        }

        assert ( ( mmt > 0 ) && ( mmt < H5FD_MEM_NTYPES ) );

        if ( multi_file->fa.memb_addr[mmt] > addr ) {

            continue;
        }

        if ( multi_file->fa.memb_addr[mmt] >= start_addr ) {

            start_addr = multi_file->fa.memb_addr[mmt];
            hi = mmt;
        }
    }

    assert ( hi > 0 );

    tgt_file = multi_file->memb[hi];

    /* pass on the aio write */
    result = H5FDaio_write(tgt_file, type, 
                           (dx ? dx->memb_dxpl[hi] : H5P_DEFAULT),
                           (addr - start_addr), size, buffer, &subctlblk_ptr);

    if ( result < 0 ) { /* failure */

        H5Epush_ret(func, H5E_ERR_CLS, H5E_VFL, H5E_AIOWRITEERROR, \
                    "aio write request failed", -1)
    }

    assert( subctlblk_ptr != NULL );

    /* allocate the control block */
    result = H5FD_multi_aio_alloc_ctlblk(&ctlblk_ptr);

    if ( result < 0 ) { /* failure */

        H5Epush_ret(func, H5E_ERR_CLS, H5E_RESOURCE, H5E_NOSPACE, \
                    "can't allocate aio control block", -1)
    }

    assert( ctlblk_ptr != NULL );
    assert( ctlblk_ptr->magic == H5FD_MULTI_AIO_CTLBLK_T__MAGIC );

    ctlblk_ptr->num_subctlblks = 1;

    (ctlblk_ptr->sub_ctlblks[0]).driver   = tgt_file;
    (ctlblk_ptr->sub_ctlblks[0]).ctlblk   = subctlblk_ptr;
    (ctlblk_ptr->sub_ctlblks[0]).done     = FALSE;
    (ctlblk_ptr->sub_ctlblks[0]).finished = FALSE;

    *ctlblk_ptr_ptr = ctlblk_ptr;

    return(0);

} /* end H5FD_multi_aio_write() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_aio_test
 *
 * Purpose:	This function is used to determine if the asynchronous
 *		operation associated with the supplied control block 
 *		pointer is done.  If it is, *done_ptr should be set
 *		to TRUE, if it isn't, *done_ptr should be set to FALSE.
 *		In all cases, there function should return immediately.
 *
 *		Note that the return value only reflects errors in the 
 *		process of testing whether the operation is complete.
 *
 *		After the operation is done, a call to 
 *		H5FD_multi_aio_finish() must be made to determine whether 
 *		the operation completed successfully and to allow the 
 *		driver to tidy its data structures.
 *
 *		Note that all calls to the underlying files will be 
 *		through the public H5FD interface, which will fake 
 *		AIO if it is not supported.  Thus, this function 
 *		presumes that AIO is supported.
 *
 *		However, it is possible that multiple underlying 
 *		files may be involved (at present this can only happen
 *		with an aio fsync call).
 *
 *		For each underlying file / driver first check the 
 *		control block, and see if that driver is done with
 *		the operation.  If it is, go on to the next file / 
 *		driver pair.
 *
 *		If the file / driver pair is not marked as being 
 *		done, call H5FDaio_test with the target driver.  If the
 *		call fails, fail.
 *
 *		If the call indicates that the aio operation on the 
 *		target file/driver pair is still in progress, set *done_ptr
 *		to FALSE, and return.  Otherwise go onto the next 
 *		file / driver pair and repeat.  If all underlying 
 *		file / driver pairs report the operation complete,
 *		set *done_ptr to TRUE and then return.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              6/12/10
 *
 * Changes:	None.
 *
 *-------------------------------------------------------------------------
 */

static herr_t 
H5FD_multi_aio_test(hbool_t *done_ptr, 
                    void *ctlblk_ptr)
{
    static const char         * func = "H5FD_multi_aio_test"; 
    herr_t			result;
    hbool_t			done = TRUE;
    hbool_t			tgt_done;
    hbool_t			already_done = TRUE;
    int                         i = 0;
    H5FD_multi_aio_ctlblk_t   * multi_ctlblk_ptr;
    H5FD_t                    * tgt_file;
    void                      * subctlblk_ptr;

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    if ( ( done_ptr == NULL ) ||
         ( ctlblk_ptr == NULL ) ) {

        H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                    "bad arg(s) on entry.", -1)

    } 

    multi_ctlblk_ptr = (H5FD_multi_aio_ctlblk_t *)ctlblk_ptr;

    if ( multi_ctlblk_ptr->magic != H5FD_MULTI_AIO_CTLBLK_T__MAGIC ) {

        H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                    "bad ctlblk magic on entry.", -1)

    } else if ( multi_ctlblk_ptr->num_subctlblks < 1 ) {


        H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                    "empty control block on entry.", -1)
    }

    while ( ( done ) && ( i < multi_ctlblk_ptr->num_subctlblks ) ) {

        tgt_file      = (multi_ctlblk_ptr->sub_ctlblks[i]).driver;
        subctlblk_ptr = (multi_ctlblk_ptr->sub_ctlblks[i]).ctlblk;

        if ( ( tgt_file == NULL ) ||
             ( subctlblk_ptr == NULL ) ) {

            H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                        "NULL tgt file or sub ctl blk.", -1)
        }

        if ( (multi_ctlblk_ptr->sub_ctlblks[i]).finished ) {

            H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                        "sub op already finished?!?!", -1)
        }

        if ( ! ((multi_ctlblk_ptr->sub_ctlblks[i]).done) ) {

            already_done = FALSE;

            tgt_done = FALSE;

            result = H5FDaio_test(tgt_file, &tgt_done, subctlblk_ptr);

            if ( result < 0 ) {

                H5Epush_ret(func, H5E_ERR_CLS, H5E_VFL, H5E_AIOTESTFAIL, \
                            "aio test sub-request failed", -1)
            }

            if ( tgt_done ) {

                (multi_ctlblk_ptr->sub_ctlblks[i]).done = TRUE;
                
            } else {

                done = FALSE;

            }
        }

        i++;
    }

    if ( already_done ) {

        H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                    "operation already done?!?!?", -1)
    }

    *done_ptr = done;

    return(0);

} /* end H5FD_multi_aio_test() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_aio_wait
 *
 * Purpose:	Wait until the asynchronous read, write, or fsync operation 
 *		indicated by *ctlblk_ptr has completed (successfully or 
 *		otherwise).
 *
 *		Note that the error code returned refers only to the 
 *		operation of waiting until the read/write/fsync is
 *		complete -- Success does not imply that the read, write,
 *		or fsync operation completed successfully, only that
 *		no error was encountered while waiting for the operation 
 *		to finish.
 *
 *		Note that all calls to the underlying files will be 
 *		through the public H5FD interface, which will fake 
 *		AIO if it is not supported.  Thus, this function 
 *		presumes that AIO is supported.
 *
 *		However, it is possible that multiple underlying 
 *		files may be involved (at present this can only happen
 *		with an aio fsync call).
 *
 *		For each underlying file / driver listed in the control
 *		block, first check the control block particular to the 
 *		file / driver pair, and then wait until the associated
 *		operation is done -- if it is not done already.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              6/12/10
 *
 * Changes:	None.
 *
 *-------------------------------------------------------------------------
 */

static herr_t 
H5FD_multi_aio_wait(void *ctlblk_ptr)
{
    static const char         * func = "H5FD_multi_aio_wait"; 
    herr_t			result;
    hbool_t			already_done = TRUE;
    int                         i;
    H5FD_multi_aio_ctlblk_t   * multi_ctlblk_ptr;
    H5FD_t                    * tgt_file;
    void                      * subctlblk_ptr;

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    if ( ctlblk_ptr == NULL ) {

        H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                    "bad arg(s) on entry.", -1)

    } 

    multi_ctlblk_ptr = (H5FD_multi_aio_ctlblk_t *)ctlblk_ptr;

    if ( multi_ctlblk_ptr->magic != H5FD_MULTI_AIO_CTLBLK_T__MAGIC ) {

        H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                    "bad ctlblk magic on entry.", -1)

    } else if ( multi_ctlblk_ptr->num_subctlblks < 1 ) {


        H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                    "empty control block on entry.", -1)
    }

    i = 0;

    while ( i < multi_ctlblk_ptr->num_subctlblks ) {

        tgt_file      = (multi_ctlblk_ptr->sub_ctlblks[i]).driver;
        subctlblk_ptr = (multi_ctlblk_ptr->sub_ctlblks[i]).ctlblk;

        if ( ( tgt_file == NULL ) ||
             ( subctlblk_ptr == NULL ) ) {

            H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                        "NULL tgt file or sub ctl blk.", -1)
        }

        if ( (multi_ctlblk_ptr->sub_ctlblks[i]).finished ) {

            H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                        "sub op already finished?!?!", -1)
        }

        if ( ! ((multi_ctlblk_ptr->sub_ctlblks[i]).done) ) {

            already_done = FALSE;

            result = H5FDaio_wait(tgt_file, subctlblk_ptr);

            if ( result < 0 ) {

                H5Epush_ret(func, H5E_ERR_CLS, H5E_VFL, H5E_AIOTESTFAIL, \
                            "aio sub wait request failed", -1)
            }

            (multi_ctlblk_ptr->sub_ctlblks[i]).done = TRUE;
        }

        i++;
    }

    if ( already_done ) {

        H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                    "operation already done?!?!?", -1)
    }

    return(0);

} /* end H5FD_aio_wait() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_aio_finish
 *
 * Purpose:	Determine whether the read, write, or fsync operation 
 *		indicated by *ctlblk_ptr completed successfully.  If it 
 *		did, set *errno_ptr to 0.  If if didn't, set *errno_ptr 
 *		to the appropriate error code.
 *
 *		Return SUCCEED if successful, and the appropriate error 
 *		code if not.
 *
 *		Note that the returned error code only refers to the 
 *		success or failure of the finish operation.  The caller 
 *		must examine *errno_ptr to determine if the underlying 
 *		asynchronous operation succeeded.
 *
 *		Note that all calls to the underlying files will be 
 *		through the public H5FD interface, which will fake 
 *		AIO if it is not supported.  Thus, this function 
 *		presumes that AIO is supported.
 *
 *		However, it is possible that multiple underlying 
 *		files may be involved (at present this can only happen
 *		with an aio fsync call).
 *
 *		For each underlying file / driver listed in the control
 *		block, first check the control block particular to the 
 *		file / driver pair and verify that it is listed as being
 *		done but not finished.  Then call the associated aio 
 *		finish function for each file -- set *errno_ptr to 0
 *		if no errors are detected, and to the last non-zero
 *		errno reported if any error is reported.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              6/12/10
 *
 *-------------------------------------------------------------------------
 */

static herr_t 
H5FD_multi_aio_finish(int *errno_ptr, 
                      void *ctlblk_ptr)
{
    static const char         * func = "H5FD_multi_aio_finish"; 
    herr_t			result;
    int                         i;
    int				error_num = 0;
    int				sub_errno;
    H5FD_multi_aio_ctlblk_t   * multi_ctlblk_ptr;
    H5FD_t                    * tgt_file;
    void                      * subctlblk_ptr;

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    if ( ( errno_ptr == NULL ) ||
         ( ctlblk_ptr == NULL ) ) {

        H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                    "bad arg(s) on entry.", -1)

    } 

    multi_ctlblk_ptr = (H5FD_multi_aio_ctlblk_t *)ctlblk_ptr;

    if ( multi_ctlblk_ptr->magic != H5FD_MULTI_AIO_CTLBLK_T__MAGIC ) {

        H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                    "bad ctlblk magic on entry.", -1)

    } else if ( multi_ctlblk_ptr->num_subctlblks < 1 ) {


        H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                    "empty control block on entry.", -1)
    }

    /* finish all the sub operations */

    i = 0;
    while ( i < multi_ctlblk_ptr->num_subctlblks ) {

        tgt_file      = (multi_ctlblk_ptr->sub_ctlblks[i]).driver;
        subctlblk_ptr = (multi_ctlblk_ptr->sub_ctlblks[i]).ctlblk;

        if ( ( tgt_file == NULL ) ||
             ( subctlblk_ptr == NULL ) ) {

            H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                        "NULL tgt file or sub ctl blk.", -1)
        }

        if ( ! ((multi_ctlblk_ptr->sub_ctlblks[i]).done) ) {

            H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                        "sub op not done?!?!", -1)
        }

        if ( (multi_ctlblk_ptr->sub_ctlblks[i]).finished ) {

            H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                        "sub op already finished?!?!", -1)
        }

        sub_errno = 0;

        result = H5FDaio_finish(tgt_file, &sub_errno, subctlblk_ptr);

        if ( result < 0 ) {

            H5Epush_ret(func, H5E_ERR_CLS, H5E_VFL, H5E_AIOTESTFAIL, \
                        "aio finish sub-request failed", -1)
        }

        if ( sub_errno != 0 ) {

            error_num = sub_errno;
        }

        (multi_ctlblk_ptr->sub_ctlblks[i]).finished = TRUE;

        i++;
    }

    /* discard the control block */

    result = H5FD_multi_aio_discard_ctlblk(multi_ctlblk_ptr);

    if ( result < 0 ) {

        H5Epush_ret(func, H5E_ERR_CLS, H5E_RESOURCE, H5E_CANTFREE, \
                    "Attempt to discard control block failed", -1)
    }

    *errno_ptr = error_num;

    return(0);

} /* end H5FD_multi_aio_finish() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_aio_fsync
 *
 * Purpose:	Queue a sync of all asynchronous writes outstanding as of 
 *		the time this function is called.  Return zero if no 
 *		errors are encountered, but note that a good error return 
 *		from H5FD_multi_aio_fsync() does not imply a successful 
 *		operation, only that no immediate errors were detected.
 *
 *		The sync is not known to be successful until reported 
 *		complete by either H5FD_multi_aio_test or 
 *		H5FD_multi_aio_wait, and reported successful by 
 *		H5FD_multi_aio_finish.
 *
 *              Note that a successful return from this function does not
 *              imply a successful write -- simply that no errors were
 *              immediately evident.
 *
 *		Also note that all calls to the underlying files will be 
 *		through the public H5FD interface, which will fake 
 *		AIO if it is not supported.  Thus, this function 
 *		presumes that AIO is supported.
 *
 *		To implement the aio_fsync, we must send an aio fsync
 *		to each of the underlying files, construct a control
 *		block containing pointers to all the sub control block
 *		returned, and return the constructed control block 
 *		to the users.
 *
 *		To do this, first construct a list of all the unique,
 *		open, underlying files.
 *
 *		Then, pass the aio_fsync to each of them, making note
 *		of the returned control block.
 *
 *		If all calls complete without error, allocate an aio
 *		control block, load it with pointers to the drivers
 *		and to the returned control blocks, and return the 
 *		address of the control block in *ctlblk_ptr_ptr.
 *
 *		If any errors are detected, simply fail.
 *		
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              6/12/10
 *
 *-------------------------------------------------------------------------
 */

static herr_t 
H5FD_multi_aio_fsync(H5FD_t *file, 
                     void **ctlblk_ptr_ptr)
{
    static const char       * func = "H5FD_multi_aio_fsync"; 
    herr_t	 	      result;
    int                       i;
    int			      num_unique_open_files = 0;
    H5FD_multi_t            * multi_file;
    H5FD_t                  * drivers[H5FD_MEM_NTYPES];
    void                    * ctlblks[H5FD_MEM_NTYPES];
    H5FD_multi_aio_ctlblk_t * ctlblk_ptr;

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    if ( ( file == NULL ) ||
         ( file->cls == NULL ) ||
         ( ctlblk_ptr_ptr == NULL ) ||
         ( *ctlblk_ptr_ptr != NULL ) ) {

        H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                    "bad arg(s) on entry.", -1)

    } 

    multi_file = (H5FD_multi_t *)file;

    /* construct a list of the unique open files */ 
    UNIQUE_MEMBERS(multi_file->fa.memb_map, mt) { 

        /* if there are any unopened unique files, skip them */
        if ( multi_file->memb[mt] != NULL ) {

            drivers[num_unique_open_files] = multi_file->memb[mt];
            ctlblks[num_unique_open_files] = NULL;
            num_unique_open_files++;

        }
    } END_MEMBERS;

    /* pass the aio fsync on to each of the underlying files,
     * making note of the returned aio control blocks.
     */
    i = 0;
    while ( i < num_unique_open_files ) {

        result = H5FDaio_fsync(drivers[i], &(ctlblks[i]));

        if ( result < 0 ) {

            H5Epush_ret(func, H5E_ERR_CLS, H5E_VFL, H5E_AIOSYNCFAIL, \
                        "sub aio fsync request failed.", -1)
        }
    }

    /* if all goes well, allocate the control block and initialize it */

    result = H5FD_multi_aio_alloc_ctlblk(&ctlblk_ptr);

    if ( result < 0 ) { /* failure */

        H5Epush_ret(func, H5E_ERR_CLS, H5E_RESOURCE, H5E_NOSPACE, \
                    "can't allocate aio control block", -1)
    }

    assert( ctlblk_ptr != NULL );
    assert( ctlblk_ptr->magic == H5FD_MULTI_AIO_CTLBLK_T__MAGIC );

    ctlblk_ptr->num_subctlblks = num_unique_open_files;

    for ( i = 0; i < num_unique_open_files; i++ ) {

        (ctlblk_ptr->sub_ctlblks[i]).driver   = drivers[i];
        (ctlblk_ptr->sub_ctlblks[i]).ctlblk   = ctlblks[i];
        (ctlblk_ptr->sub_ctlblks[i]).done     = FALSE;
        (ctlblk_ptr->sub_ctlblks[i]).finished = FALSE;
    }

    *ctlblk_ptr_ptr = (void *)ctlblk_ptr;

    return(0);

} /* end H5FD_multi_aio_fsync() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_aio_cancel
 *
 * Purpose:	Attempt to cancel the asynchronous operation associated 
 *		with the control block pointed to by ctlblk_ptr.  
 *
 *		Note that this operation may have completed, but it is 
 *		an error if H5FD_aio_finish() has been called on it.
 *
 *		As part of the cancel, free the associated control blocks.
 *
 *		Return SUCCEED if successful, and the appropriate error 
 *		code otherwise.
 *
 *		Note that all calls to the underlying files will be 
 *		through the public H5FD interface, which will fake 
 *		AIO if it is not supported.  Thus, this function 
 *		presumes that AIO is supported.
 *
 *		However, it is possible that multiple underlying 
 *		files may be involved (at present this can only happen
 *		with an aio fsync call).
 *
 *		For each underlying file / driver listed in the control
 *		block, first check the control block particular to the 
 *		file / driver pair and verify that it is not listed as 
 *		being finished.  Then call the associated aio cancel 
 *		function for each file.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              6/12/10
 *
 *-------------------------------------------------------------------------
 */

static herr_t 
H5FD_multi_aio_cancel(void *ctlblk_ptr)
{
    static const char         * func = "H5FD_multi_aio_cancel"; 
    herr_t			result;
    int                         i;
    H5FD_multi_aio_ctlblk_t   * multi_ctlblk_ptr;
    H5FD_t                    * tgt_file;
    void                      * subctlblk_ptr;

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    if ( ctlblk_ptr == NULL ) {

        H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                    "bad arg(s) on entry.", -1)

    } 

    multi_ctlblk_ptr = (H5FD_multi_aio_ctlblk_t *)ctlblk_ptr;

    if ( multi_ctlblk_ptr->magic != H5FD_MULTI_AIO_CTLBLK_T__MAGIC ) {

        H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                    "bad ctlblk magic on entry.", -1)

    } else if ( multi_ctlblk_ptr->num_subctlblks < 1 ) {

        H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                    "empty control block on entry.", -1)
    }

    i = 0;

    while ( i < multi_ctlblk_ptr->num_subctlblks ) {

        tgt_file      = (multi_ctlblk_ptr->sub_ctlblks[i]).driver;
        subctlblk_ptr = (multi_ctlblk_ptr->sub_ctlblks[i]).ctlblk;

        if ( ( tgt_file == NULL ) ||
             ( subctlblk_ptr == NULL ) ) {

            H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                        "NULL tgt file or sub ctl blk.", -1)
        }

        if ( (multi_ctlblk_ptr->sub_ctlblks[i]).finished ) {

            H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                        "sub op already finished?!?!", -1)
        }

        result = H5FDaio_cancel(tgt_file, subctlblk_ptr);

        if ( result < 0 ) {

            H5Epush_ret(func, H5E_ERR_CLS, H5E_VFL, H5E_AIOTESTFAIL, \
                        "aio sub wait request failed", -1)
        }

	(multi_ctlblk_ptr->sub_ctlblks[i]).driver   = NULL;
	(multi_ctlblk_ptr->sub_ctlblks[i]).ctlblk   = NULL;
	(multi_ctlblk_ptr->sub_ctlblks[i]).done     = FALSE;
	(multi_ctlblk_ptr->sub_ctlblks[i]).finished = FALSE;

        i++;
    }

    /* discard the control block */

    result = H5FD_multi_aio_discard_ctlblk(multi_ctlblk_ptr);

    if ( result < 0 ) {

        H5Epush_ret(func, H5E_ERR_CLS, H5E_RESOURCE, H5E_CANTFREE, \
                    "Attempt to discard control block failed", -1)
    }

    return(0);

} /* end H5FD_multi_aio_cancel() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_multi_fsync
 *
 * Purpose:     Sync out all underlying files.
 *
 * Return:      Success:        Non-negative
 *
 *              Failure:        Negative
 *
 * Programmer:  John Mainzer
 *              7/7/10
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5FD_multi_fsync(H5FD_t *file,
                 hid_t dxpl_id)
{
    static const char        * func = "H5FD_multi_fsync"; 
    herr_t                     result;
    H5FD_multi_t             * multi_file_ptr;
    H5FD_t                   * tgt_file;

    /* Clear the error stack */
    H5Eclear2(H5E_DEFAULT);

    if ( file == NULL ) {

        H5Epush_ret(func, H5E_ERR_CLS, H5E_ARGS, H5E_SYSTEM, \
                    "bad arg(s) on entry.", -1)

    } 

    multi_file_ptr = (H5FD_multi_t *)file;

    UNIQUE_MEMBERS(multi_file_ptr->fa.memb_map, mt) { 

        /* if there are any unopened unique files, skip them */
        if ( multi_file_ptr->memb[mt] != NULL ) {

            tgt_file = multi_file_ptr->memb[mt];

            result = H5FDfsync(tgt_file, dxpl_id);

            if ( result < 0 ) {

                H5Epush_ret(func, H5E_ERR_CLS, H5E_VFL, H5E_SYNCFAIL, \
                            "sub fsync request failed", -1)
            }
        }
    } END_MEMBERS;

    return(0);

} /* end H5FD_multi_fsync() */


#ifdef _H5private_H
/*
 * This is not related to the functionality of the driver code.
 * It is added here to trigger warning if HDF5 private definitions are included
 * by mistake.  The code should use only HDF5 public API and definitions.
 */
#error "Do not use HDF5 private definitions"
#endif
