/*
 * Copyright (C) 1997 NCSA
 *		      All rights reserved.
 *
 * Programmer:	Robb Matzke <matzke@llnl.gov>
 *		Monday, November 10, 1997
 *
 * Purpose:	Implements a file driver which dispatches I/O requests to
 *		other file drivers depending on the purpose of the address
 *		region being accessed. For instance, all meta-data could be
 *		place in one file while all raw data goes to some other file.
 */
#include <assert.h>
#include <hdf5.h>
#include <stdlib.h>

/*
 * Define H5FD_MULTI_DEBUG if you want the ability to print debugging
 * messages to the standard error stream. Messages are only printed if the
 * file is opened with the H5F_ACC_DEBUG flag.
 */
#define H5FD_MULTI_DEBUG

/* Our versions of MIN and MAX */
#undef MAX
#define MAX(X,Y)	((X)>(Y)?(X):(Y))
#undef MIN
#define MIN(X,Y)	((X)<(Y)?(X):(Y))

/* The driver identification number, initialized at runtime */
static hid_t H5FD_MULTI_g = 0;

/*
 * The description of a file belonging to this driver. The file access
 * properties and member names do not have to be copied into this struct
 * since they will be held open by the file access property list which is
 * copied into the parent file struct in H5F_open().
 */
typedef struct H5FD_multi_t {
    H5FD_t	pub;		/*public stuff, must be first		*/
    H5FD_mem_t	memb_map[H5FD_MEM_NTYPES]; /*map from usage to file	*/
    haddr_t	memb_addr[H5FD_MEM_NTYPES];/*starting address per member*/
    haddr_t	memb_next[H5FD_MEM_NTYPES];/*addr of next member	*/
    H5FD_t	*memb[H5FD_MEM_NTYPES];	/*member pointers		*/
    hid_t	memb_fapl[H5FD_MEM_NTYPES];/*member file access props	*/
    char	*memb_name[H5FD_MEM_NTYPES];/*name generators		*/
    haddr_t	eoa;		/*end of allocated addresses		*/
    unsigned	flags;		/*file open flags saved for debugging	*/
} H5FD_multi_t;

/* Driver-specific file access properties */
typedef struct H5FD_multi_fapl_t {
    H5FD_mem_t	memb_map[H5FD_MEM_NTYPES]; /*memory usage map		*/
    hid_t	memb_fapl[H5FD_MEM_NTYPES];/*member access properties	*/
    char	*memb_name[H5FD_MEM_NTYPES];/*name generators		*/
    haddr_t	memb_addr[H5FD_MEM_NTYPES];/*starting addr per member	*/
} H5FD_multi_fapl_t;

/* Driver specific data transfer properties */
typedef struct H5FD_multi_dxpl_t {
    hid_t	memb_dxpl[H5FD_MEM_NTYPES];/*member data xfer properties*/
} H5FD_multi_dxpl_t;

/* Private functions */
static char *my_strdup(const char *s);

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
static haddr_t H5FD_multi_get_eoa(H5FD_t *_file);
static herr_t H5FD_multi_set_eoa(H5FD_t *_file, haddr_t eoa);
static haddr_t H5FD_multi_get_eof(H5FD_t *_file);
static haddr_t H5FD_multi_alloc(H5FD_t *_file, H5FD_mem_t type, hsize_t size);
static herr_t H5FD_multi_free(H5FD_t *_file, H5FD_mem_t type, haddr_t addr,
			      hsize_t size);
static herr_t H5FD_multi_read(H5FD_t *_file, hid_t dxpl_id, haddr_t addr,
			      hsize_t size, void *_buf/*out*/);
static herr_t H5FD_multi_write(H5FD_t *_file, hid_t dxpl_id, haddr_t addr,
			       hsize_t size, const void *_buf);
static herr_t H5FD_multi_flush(H5FD_t *_file);

/* The class struct */
static const H5FD_class_t H5FD_multi_g = {
    "multi",					/*name			*/
    HADDR_MAX,					/*maxaddr		*/
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
    H5FD_multi_alloc,				/*alloc			*/
    H5FD_multi_free,				/*free			*/
    H5FD_multi_get_eoa,				/*get_eoa		*/
    H5FD_multi_set_eoa,				/*set_eoa		*/
    H5FD_multi_get_eof,				/*get_eof		*/
    H5FD_multi_read,				/*read			*/
    H5FD_multi_write,				/*write			*/
    H5FD_multi_flush,				/*flush			*/
    H5FD_FLMAP_DEFAULT,				/*fl_map		*/
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
    if (!s) return NULL;
    if (NULL==(x=malloc(strlen(s)+1))) return NULL;
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
    if (!H5FD_MULTI_g) {
	H5FD_MULTI_g = H5FDregister(&H5FD_multi_g);
    }
    return H5FD_MULTI_g;
}


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
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_split(hid_t fapl, const char *meta_ext, hid_t meta_plist_id,
		  const char *raw_ext, hid_t raw_plist_id)
{
    H5FD_mem_t		mt, memb_map[H5FD_MEM_NTYPES];
    hid_t		memb_fapl[H5FD_MEM_NTYPES];
    const char		*memb_name[H5FD_MEM_NTYPES];
    char		meta_name[1024], raw_name[1024];
    haddr_t		memb_addr[H5FD_MEM_NTYPES];

    /*NO TRACE*/

    /* Initialize */
    for (mt=0; mt<H5FD_MEM_NTYPES; mt++) {
	memb_map[mt] = (H5FD_MEM_DRAW==mt?mt:H5FD_MEM_SUPER);
	memb_fapl[mt] = -1;
	memb_name[mt] = NULL;
	memb_addr[mt] = HADDR_UNDEF;
    }

    /* The file access properties */
    memb_fapl[H5FD_MEM_SUPER] = meta_plist_id;
    memb_fapl[H5FD_MEM_DRAW] = raw_plist_id;

    /* The names */
    sprintf(meta_name, "%%s%s", meta_ext?meta_ext:".meta");
    memb_name[H5FD_MEM_SUPER] = meta_name;
    sprintf(raw_name, "%%s%s", raw_ext?raw_ext:".raw");
    memb_name[H5FD_MEM_DRAW] = raw_name;

    /* The sizes */
    memb_addr[H5FD_MEM_SUPER] = 0;
    memb_addr[H5FD_MEM_DRAW] = HADDR_MAX/2;

    return H5Pset_fapl_multi(fapl, memb_map, memb_fapl, memb_name, memb_addr);
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
 *		                      memb_name, memb_addr);
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
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_multi(hid_t fapl_id, const H5FD_mem_t *memb_map,
		  const hid_t *memb_fapl, const char **memb_name,
		  const haddr_t *memb_addr)
{
    H5FD_multi_fapl_t	fa;
    H5FD_mem_t		mt, mmt;
    
    /*NO TRACE*/
    
    /* Check arguments */
    if (H5P_FILE_ACCESS!=H5Pget_class(fapl_id)) return -1;
    if (!memb_map) return -1;
    if (!memb_fapl) return -1;
    if (!memb_name) return -1;
    if (!memb_addr) return -1;
    
    for (mt=0; mt<H5FD_MEM_NTYPES; mt++) {
	/* Map usage type */
	mmt = memb_map[mt];
	if (mmt<0 || mmt>=H5FD_MEM_NTYPES) return -1;
	if (H5FD_MEM_DEFAULT==mmt) mmt = mt;

	/*
	 * All members of MEMB_FAPL must be either defaults or actual file
	 * access property lists.
	 */
	if (H5P_DEFAULT!=memb_fapl[mmt] &&
	    H5P_FILE_ACCESS!=H5Pget_class(memb_fapl[mmt])) return -1;

	/* All names must be defined */
	if (!memb_name[mmt] || !memb_name[mmt][0]) return -1;
    }
    
    /*
     * Initialize driver specific information. No need to copy it into the FA
     * struct since all members will be copied by H5Pset_driver().
     */
    memcpy(fa.memb_map, memb_map, H5FD_MEM_NTYPES*sizeof(H5FD_mem_t));
    memcpy(fa.memb_fapl, memb_fapl, H5FD_MEM_NTYPES*sizeof(hid_t));
    memcpy(fa.memb_name, memb_name, H5FD_MEM_NTYPES*sizeof(char*));
    memcpy(fa.memb_addr, memb_addr, H5FD_MEM_NTYPES*sizeof(haddr_t));
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
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_fapl_multi(hid_t fapl_id, H5FD_mem_t *memb_map/*out*/,
		  hid_t *memb_fapl/*out*/, char **memb_name/*out*/,
		  haddr_t *memb_addr/*out*/)
{
    H5FD_multi_fapl_t	*fa;
    H5FD_mem_t		mt;
    
    /*NO TRACE*/

    if (H5P_FILE_ACCESS!=H5Pget_class(fapl_id)) return -1;
    if (H5FD_MULTI!=H5Pget_driver(fapl_id)) return -1;
    if (NULL==(fa=H5Pget_driver_info(fapl_id))) return -1;

    if (memb_map) {
	memcpy(memb_map, fa->memb_map, H5FD_MEM_NTYPES*sizeof(H5FD_mem_t));
    }
    if (memb_fapl) {
	for (mt=0; mt<H5FD_MEM_NTYPES; mt++) {
	    if (fa->memb_fapl[mt]>=0) {
		memb_fapl[mt] = H5Pcopy(fa->memb_fapl[mt]);
	    } else {
		memb_fapl[mt] = fa->memb_fapl[mt]; /*default or bad ID*/
	    }
	}
    }
    if (memb_name) {
	for (mt=0; mt<H5FD_MEM_NTYPES; mt++) {
	    if (fa->memb_name[mt]) {
		memb_name[mt] = malloc(strlen(fa->memb_name[mt])+1);
		strcpy(memb_name[mt], fa->memb_name[mt]);
	    } else {
		memb_name[mt] = NULL;
	    }
	}
    }
    if (memb_addr) {
	memcpy(memb_addr, fa->memb_addr, H5FD_MEM_NTYPES*sizeof(haddr_t));
    }
    
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

    /*NO TRACE*/

    /* Check arguments */
    if (H5P_DATA_XFER!=H5Pget_class(dxpl_id)) return -1;
    if (!memb_dxpl) return -1;
    for (mt=0; mt<H5FD_MEM_NTYPES; mt++) {
	if (H5P_DEFAULT!=memb_dxpl[mt] &&
	    H5P_DATA_XFER!=H5Pget_class(memb_dxpl[mt])) return -1;
    }

    /* Initialize the data transfer property list */
    memcpy(dx.memb_dxpl, memb_dxpl, H5FD_MEM_NTYPES*sizeof(hid_t));
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

    /*NO TRACE*/

    if (H5P_FILE_ACCESS!=H5Pget_class(dxpl_id)) return -1;
    if (H5FD_MULTI!=H5Pget_driver(dxpl_id)) return -1;
    if (NULL==(dx=H5Pget_driver_info(dxpl_id))) return -1;

    if (memb_dxpl) {
	for (mt=0; mt<H5FD_MEM_NTYPES; mt++) {
	    if (dx->memb_dxpl[mt]>=0) {
		memb_dxpl[mt] = H5Pcopy(dx->memb_dxpl[mt]);
	    } else {
		memb_dxpl[mt] = dx->memb_dxpl[mt]; /*default or bad ID */
	    }
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
H5FD_multi_sb_size(H5FD_t *_file/*unused*/)
{
    return H5FD_MEM_NTYPES * 8;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_sb_encode
 *
 * Purpose:	Encode driver information for the superblock. The NAME
 *		argument is a nine-byte buffer which will be initialized with
 *		an eight-character name/version number and null termination.
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
    H5FD_mem_t		mt;
    haddr_t		memb_eoa;

    /* Name and version number */
    strcpy(name, "NCSAmulti");

    /* Copy EOA values into buffer */
    assert(sizeof(haddr_t)<=8);
    for (mt=0; mt<H5FD_MEM_NTYPES; mt++) {
	if (file->memb[mt]) {
	    memb_eoa = H5FDget_eoa(file->memb[mt]);
	} else {
	    memb_eoa = HADDR_UNDEF;
	}
	memcpy(buf+mt*sizeof(memb_eoa), &memb_eoa, sizeof(memb_eoa));
    }

    /* Convert to destination type */
    if (H5Tconvert(H5T_NATIVE_HADDR, H5T_STD_U64LE, H5FD_MEM_NTYPES,
		   buf, NULL, H5P_DEFAULT)<0) return -1;
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_multi_sb_decode
 *
 * Purpose:	Decodes the superblock information for this driver. The NAME
 *		argument is the eight-character (plus null termination) name
 *		stored in the file.
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
H5FD_multi_sb_decode(H5FD_t *_file, const char *name, const unsigned char *buf)
{
    H5FD_multi_t	*file = (H5FD_multi_t*)_file;
    H5FD_mem_t		mt;
    char		x[H5FD_MEM_NTYPES*8];

    /* Make sure the name/version number is correct */
    if (strcmp(name, "NCSAmult")) return -1;

    /* Decode EOA values */
    assert(sizeof(haddr_t)<=8);
    memcpy(x, buf, H5FD_MEM_NTYPES*8);
    if (H5Tconvert(H5T_STD_U64LE, H5T_NATIVE_HADDR, H5FD_MEM_NTYPES,
		   x, NULL, H5P_DEFAULT)<0) return -1;
    
    /* Set EOA values */
    for (mt=0; mt<H5FD_MEM_NTYPES; mt++) {
	if (file->memb[mt]) {
	    H5FDset_eoa(file->memb[mt], ((haddr_t*)x)[mt]);
	}
    }
    return 0;
}


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
    H5FD_multi_fapl_t	fa;

    memset(&fa, 0, sizeof fa);
    memcpy(fa.memb_map, file->memb_map, sizeof(fa.memb_map));
    memcpy(fa.memb_fapl, file->memb_fapl, sizeof(fa.memb_fapl));
    memcpy(fa.memb_name, file->memb_name, sizeof(fa.memb_name));
    memcpy(fa.memb_addr, file->memb_addr, sizeof(fa.memb_addr));
    return H5FD_multi_fapl_copy(&fa);
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
    H5FD_multi_fapl_t *new_fa = malloc(sizeof(H5FD_multi_fapl_t));
    H5FD_mem_t mt;
    int nerrors = 0;
    
    assert(new_fa);

    memcpy(new_fa, old_fa, sizeof(H5FD_multi_fapl_t));
    for (mt=0; mt<H5FD_MEM_NTYPES; mt++) {
	if (old_fa->memb_fapl[mt]>=0) {
	    new_fa->memb_fapl[mt] = H5Pcopy(old_fa->memb_fapl[mt]);
	    if (new_fa->memb_fapl[mt]<0) nerrors++;
	}
	if (old_fa->memb_name[mt]) {
	    new_fa->memb_name[mt] = malloc(strlen(old_fa->memb_name[mt])+1);
	    assert(new_fa->memb_name[mt]);
	    strcpy(new_fa->memb_name[mt], old_fa->memb_name[mt]);
	}
    }

    if (nerrors) {
	for (mt=0; mt<H5FD_MEM_NTYPES; mt++) {
	    if (new_fa->memb_fapl[mt]>=0) H5Pclose(new_fa->memb_fapl[mt]);
	    if (new_fa->memb_name[mt]) free(new_fa->memb_name[mt]);
	}
	free(new_fa);
	return NULL;
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
    H5FD_mem_t		mt;

    for (mt=0; mt<H5FD_MEM_NTYPES; mt++) {
	if (fa->memb_fapl[mt]>=0) H5Pclose(fa->memb_fapl[mt]);
	if (fa->memb_name[mt]) free(fa->memb_name[mt]);
    }
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
    H5FD_multi_dxpl_t *new_dx = malloc(sizeof(H5FD_multi_dxpl_t));
    H5FD_mem_t mt;
    int nerrors = 0;
    
    assert(new_dx);

    memcpy(new_dx, old_dx, sizeof(H5FD_multi_dxpl_t));
    for (mt=0; mt<H5FD_MEM_NTYPES; mt++) {
	if (old_dx->memb_dxpl[mt]>=0) {
	    new_dx->memb_dxpl[mt] = H5Pcopy(old_dx->memb_dxpl[mt]);
	    if (new_dx->memb_dxpl[mt]<0) nerrors++;
	}
    }

    if (nerrors) {
	for (mt=0; mt<H5FD_MEM_NTYPES; mt++) {
	    H5Pclose(new_dx->memb_dxpl[mt]);
	}
	free(new_dx);
	return NULL;
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
    H5FD_mem_t		mt;

    for (mt=0; mt<H5FD_MEM_NTYPES; mt++) {
	if (dx->memb_dxpl[mt]>=0) H5Pclose(dx->memb_dxpl[mt]);
    }
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
    H5FD_mem_t		mt, mmt, mt2, mmt2;
    char		tmp[4096];
    int			seen[H5FD_MEM_NTYPES];
    
    /* Check arguments */
    if (!name || !*name) return NULL;
    if (0==maxaddr || HADDR_UNDEF==maxaddr) return NULL;

    /*
     * Initialize file from file access properties. The default mapping
     * creates two files -- one for meta data and one for raw data. The
     * default file extensions are ".meta" and ".raw" accessed by default
     * file drivers. Half the address space is used for each.
     */
    if (NULL==(file=calloc(1, sizeof(H5FD_multi_t)))) return NULL;
    if (H5P_DEFAULT==fapl_id || H5FD_MULTI!=H5Pget_driver(fapl_id)) {
	for (mt=0; mt<H5FD_MEM_NTYPES; mt++) {
	    file->memb_map[mt] = H5FD_MEM_DRAW==mt?mt:H5FD_MEM_SUPER;
	    file->memb_addr[mt] = HADDR_UNDEF;
	    file->memb_fapl[mt] = H5P_DEFAULT;
	}
	file->memb_name[H5FD_MEM_SUPER] = my_strdup("%s.meta");
	file->memb_addr[H5FD_MEM_SUPER] = 0;
	
	file->memb_name[H5FD_MEM_DRAW] = my_strdup("%s.raw");
	file->memb_addr[H5FD_MEM_DRAW] = maxaddr/2;
    } else {
	H5FD_multi_fapl_t *fa = H5Pget_driver_info(fapl_id);
	assert(fa);
	for (mt=0; mt<H5FD_MEM_NTYPES; mt++) {
	    file->memb_map[mt] = fa->memb_map[mt];
	    file->memb_addr[mt] = fa->memb_addr[mt];
	    if (fa->memb_fapl[mt]>=0) {
		file->memb_fapl[mt] = H5Pcopy(fa->memb_fapl[mt]);
	    } else {
		file->memb_fapl[mt] = fa->memb_fapl[mt];
	    }
	    if (fa->memb_name[mt]) {
		file->memb_name[mt] = my_strdup(fa->memb_name[mt]);
	    } else {
		file->memb_name[mt] = NULL;
	    }
	}
    }
    file->flags = flags;

    /*
     * Figure out the memb_next[] values for each member. This is the
     * beginning address of the next member.
     */
    memset(seen, 0, sizeof seen);
    for (mt=1; mt<H5FD_MEM_NTYPES; mt++) {
	mmt = file->memb_map[mt];
	if (H5FD_MEM_DEFAULT==mmt) mmt = mt;
	assert(mmt>0 && mmt<H5FD_MEM_NTYPES);
	if (seen[mmt]++) continue;

	file->memb_next[mmt] = HADDR_UNDEF;
	for (mt2=1; mt2<H5FD_MEM_NTYPES; mt2++) {
	    mmt2 = file->memb_map[mt2];
	    if (H5FD_MEM_DEFAULT==mmt2) mmt2 = mt2;
	    assert(mmt2>0 && mmt2<H5FD_MEM_NTYPES);
	    if (mmt==mmt2) continue;

	    if (file->memb_addr[mmt]<file->memb_addr[mmt2] &&
		(HADDR_UNDEF==file->memb_next[mmt] ||
		 file->memb_next[mmt]>file->memb_addr[mmt2])) {
		file->memb_next[mmt] = file->memb_addr[mmt2];
	    }
	}
    }
    
    /*
     * Open all the multi members.
     */
    memset(seen, 0, sizeof seen);
    for (mt=1; mt<H5FD_MEM_NTYPES; mt++) {
	mmt = file->memb_map[mt];
	if (H5FD_MEM_DEFAULT==mmt) mmt = mt;
	assert(mmt>0 && mmt<H5FD_MEM_NTYPES);
	if (seen[mmt]++) continue;
	assert(file->memb_name[mmt]);
	sprintf(tmp, file->memb_name[mmt], name);

#ifdef H5FD_MULTI_DEBUG
	if (file->flags & H5F_ACC_DEBUG) {
	    fprintf(stderr, "H5FD_MULTI: opening \"%s\"\n", tmp);
	}
#endif
	H5E_BEGIN_TRY {
	    file->memb[mmt] = H5FDopen(tmp, flags, file->memb_fapl[mmt],
				       HADDR_UNDEF);
	} H5E_END_TRY;
	if (!file->memb[mmt]) {
#ifdef H5FD_MULTI_DEBUG
	    if (file->flags & H5F_ACC_DEBUG) {
		fprintf(stderr, "H5FD_MULTI: open failed\n");
	    }
#endif
	    goto error;
	}
    }

    /* Write index file */
    if ((flags & H5F_ACC_RDWR) &&
	H5FD_multi_flush((H5FD_t*)file)<0) goto error;
    return (H5FD_t*)file;

 error:
    /* Cleanup and fail */
    if (file) {
	for (mt=0; mt<H5FD_MEM_NTYPES; mt++) {
	    if (file->memb[mt]) H5FDclose(file->memb[mt]);
	    if (file->memb_fapl[mt]>=0) H5Pclose(file->memb_fapl[mt]);
	    if (file->memb_name[mt]) free(file->memb_name[mt]);
	}
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
    H5FD_mem_t		mt;
    int			nerrors=0;

    /* Flush our own data */
    if (H5FD_multi_flush(_file)<0) nerrors++;

    /* Close as many members as possible */
    for (mt=0; mt<H5FD_MEM_NTYPES; mt++) {
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
    }
    if (nerrors) return -1;

    /* Clean up other stuff */
    for (mt=0; mt<H5FD_MEM_NTYPES; mt++) {
	if (file->memb_fapl[mt]>=0) H5Pclose(file->memb_fapl[mt]);
	if (file->memb_name[mt]) free(file->memb_name[mt]);
    }
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
    H5FD_mem_t		mt;
    int			cmp=0;

    for (mt=0; mt<H5FD_MEM_NTYPES; mt++) {
	if (f1->memb[mt] && f2->memb[mt]) break;
	if (!cmp) {
	    if (f1->memb[mt]) cmp = -1;
	    else if (f2->memb[mt]) cmp = 1;
	}
    }
    assert(cmp || mt<H5FD_MEM_NTYPES);
    if (mt>=H5FD_MEM_NTYPES) return cmp;

    return H5FDcmp(f1->memb[mt], f2->memb[mt]);
}


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
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_multi_get_eoa(H5FD_t *_file)
{
    H5FD_multi_t	*file = (H5FD_multi_t*)_file;
    return file->eoa;
}


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
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_multi_set_eoa(H5FD_t *_file, haddr_t eoa)
{
    H5FD_multi_t	*file = (H5FD_multi_t*)_file;
    H5FD_mem_t		mt, mmt;
    herr_t		status;

    /* Find the subfile in which the new EOA value falls */
    for (mt=1; mt<H5FD_MEM_NTYPES; mt++) {
	mmt = file->memb_map[mt];
	if (H5FD_MEM_DEFAULT==mmt) mmt = mt;
	assert(mmt>0 && mmt<H5FD_MEM_NTYPES);

	if (eoa>=file->memb_addr[mmt] && eoa<file->memb_next[mmt]) {
	    break;
	}
    }
    assert(mt<H5FD_MEM_NTYPES);

    /* Set subfile eoa */
    H5E_BEGIN_TRY {
	status = H5FDset_eoa(file->memb[mmt], eoa-file->memb_addr[mmt]);
    } H5E_END_TRY;
    if (status<0) return -1;
    
    /* Save new eoa for return later */
    file->eoa = eoa;
    return 0;
}


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
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_multi_get_eof(H5FD_t *_file)
{
    H5FD_multi_t	*file = (H5FD_multi_t*)_file;
    haddr_t		eof=0, tmp;
    H5FD_mem_t		mt, mmt;
    int			seen[H5FD_MEM_NTYPES];

    memset(seen, 0, sizeof seen);
    for (mt=1; mt<H5FD_MEM_NTYPES; mt++) {
	mmt = file->memb_map[mt];
	if (H5FD_MEM_DEFAULT==mmt) mmt = mt;
	assert(mmt>0 && mmt<H5FD_MEM_NTYPES);
	if (seen[mmt]++) continue;

	H5E_BEGIN_TRY {
	    tmp = H5FDget_eof(file->memb[mmt]);
	} H5E_END_TRY;
	if (HADDR_UNDEF==tmp) return HADDR_UNDEF;
	if (tmp>0) tmp += file->memb_addr[mmt];
	if (tmp>eof) eof = tmp;
    }
    
    return MAX(file->eoa, eof);
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
H5FD_multi_alloc(H5FD_t *_file, H5FD_mem_t type, hsize_t size)
{
    H5FD_multi_t	*file = (H5FD_multi_t*)_file;
    H5FD_mem_t		mmt;
    haddr_t		addr;

    mmt = file->memb_map[type];
    if (H5FD_MEM_DEFAULT==mmt) mmt = type;
    
    if (HADDR_UNDEF==(addr=H5FDalloc(file->memb[mmt], type, size))) {
	return HADDR_UNDEF;
    }
    addr += file->memb_addr[mmt];
    if (addr+size>file->eoa) file->eoa = addr+size;
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
H5FD_multi_free(H5FD_t *_file, H5FD_mem_t type, haddr_t addr, hsize_t size)
{
    H5FD_multi_t	*file = (H5FD_multi_t*)_file;
    H5FD_mem_t		mmt;

    mmt = file->memb_map[type];
    if (H5FD_MEM_DEFAULT==mmt) mmt = type;
    
    assert(addr>=file->memb_addr[mmt]);
    assert(addr+size<=file->memb_next[mmt]);
    return H5FDfree(file->memb[mmt], type, addr-file->memb_addr[mmt], size);
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
H5FD_multi_read(H5FD_t *_file, hid_t dxpl_id, haddr_t addr, hsize_t size,
		 void *_buf/*out*/)
{
    H5FD_multi_t	*file = (H5FD_multi_t*)_file;
    H5FD_multi_dxpl_t	*dx=NULL;
    H5FD_mem_t		mt, mmt, hi=H5FD_MEM_DEFAULT;
    haddr_t		start_addr=0;

    /* Get the data transfer properties */
    if (H5P_DEFAULT!=dxpl_id && H5FD_MULTI==H5Pget_driver(dxpl_id)) {
	dx = H5Pget_driver_info(dxpl_id);
    }
    
    /* Find the file to which this address belongs */
    for (mt=1; mt<H5FD_MEM_NTYPES; mt++) {
	mmt = file->memb_map[mt];
	if (H5FD_MEM_DEFAULT==mmt) mmt = mt;
	assert(mmt>0 && mmt<H5FD_MEM_NTYPES);

	if (file->memb_addr[mmt]>addr) continue;
	if (file->memb_addr[mmt]>=start_addr) {
	    start_addr = file->memb_addr[mmt];
	    hi = mmt;
	}
    }
    assert(hi>0);

    /* Read from that member */
    return H5FDread(file->memb[hi], dx?dx->memb_dxpl[hi]:H5P_DEFAULT,
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
H5FD_multi_write(H5FD_t *_file, hid_t dxpl_id, haddr_t addr, hsize_t size,
		  const void *_buf)
{
    H5FD_multi_t	*file = (H5FD_multi_t*)_file;
    H5FD_multi_dxpl_t	*dx=NULL;
    H5FD_mem_t		mt, mmt, hi=H5FD_MEM_DEFAULT;
    haddr_t		start_addr=0;

    /* Get the data transfer properties */
    if (H5P_DEFAULT!=dxpl_id && H5FD_MULTI==H5Pget_driver(dxpl_id)) {
	dx = H5Pget_driver_info(dxpl_id);
    }
    
    /* Find the file to which this address belongs */
    for (mt=1; mt<H5FD_MEM_NTYPES; mt++) {
	mmt = file->memb_map[mt];
	if (H5FD_MEM_DEFAULT==mmt) mmt = mt;
	assert(mmt>0 && mmt<H5FD_MEM_NTYPES);

	if (file->memb_addr[mmt]>addr) continue;
	if (file->memb_addr[mmt]>=start_addr) {
	    start_addr = file->memb_addr[mmt];
	    hi = mmt;
	}
    }
    assert(hi>0);

    /* Write to that member */
    return H5FDwrite(file->memb[hi], dx?dx->memb_dxpl[hi]:H5P_DEFAULT,
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
H5FD_multi_flush(H5FD_t *_file)
{
    H5FD_multi_t	*file = (H5FD_multi_t*)_file;
    H5FD_mem_t		mt, mmt;
    int			nerrors=0;

#if 0
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
	    haddr_t eoa = H5FDget_eoa(file->memb[mt]);
	    fprintf(stderr, "    %6d %20llu %20llu %20llu %s\n",
		    (int)mt, (unsigned long long)(file->memb_addr[mt]),
		    (unsigned long long)eoa,
		    (unsigned long long)(file->memb_next[mt]),
		    file->memb_name[mt]);
	}
    }
#endif

    /* Flush each file */
    for (mt=1; mt<H5FD_MEM_NTYPES; mt++) {
	if (file->memb[mt]) {
	    H5E_BEGIN_TRY {
		if (H5FDflush(file->memb[mt])<0) nerrors++;
	    } H5E_END_TRY;
	}
    }
	
    return nerrors ? -1 : 0;
}

