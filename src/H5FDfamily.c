/*
 * Copyright (C) 1997 NCSA
 *		      All rights reserved.
 *
 * Programmer:	Robb Matzke <matzke@llnl.gov>
 *		Monday, November 10, 1997
 *
 * Purpose:	Implements a family of files that acts as a single hdf5
 *		file.  The purpose is to be able to split a huge file on a
 *		64-bit platform, transfer all the <2GB members to a 32-bit
 *		platform, and then access the entire huge file on the 32-bit
 *		platform.
 *
 *		All family members are logically the same size although their
 *		physical sizes may vary.  The logical member size is
 *		determined by looking at the physical size of the first member
 *		when the file is opened.  When creating a file family, the
 *		first member is created with a predefined physical size
 *		(actually, this happens when the file family is flushed, and
 *		can be quite time consuming on file systems that don't
 *		implement holes, like nfs).
 *		
 */
#include <assert.h>
#include <hdf5.h>
#include <stdlib.h>

#undef MAX
#define MAX(X,Y)	((X)>(Y)?(X):(Y))
#undef MIN
#define MIN(X,Y)	((X)<(Y)?(X):(Y))

/* The driver identification number, initialized at runtime */
static hid_t H5FD_FAMILY_g = 0;

/* The description of a file belonging to this driver. */
typedef struct H5FD_family_t {
    H5FD_t	pub;		/*public stuff, must be first		*/
    hid_t	memb_fapl_id;	/*file access property list for members	*/
    hsize_t	memb_size;	/*maximum size of each member file	*/
    int		nmembs;		/*number of family members		*/
    int		amembs;		/*number of member slots allocated	*/
    H5FD_t	**memb;		/*dynamic array of member pointers	*/
    haddr_t	eoa;		/*end of allocated addresses		*/
    char	*name;		/*name generator printf format		*/
    unsigned	flags;		/*flags for opening additional members	*/
} H5FD_family_t;

/* Driver-specific file access properties */
typedef struct H5FD_family_fapl_t {
    hsize_t	memb_size;	/*size of each member			*/
    hid_t	memb_fapl_id;	/*file access property list of each memb*/
} H5FD_family_fapl_t;

/* Driver specific data transfer properties */
typedef struct H5FD_family_dxpl_t {
    hid_t	memb_dxpl_id;	/*data xfer property list of each memb	*/
} H5FD_family_dxpl_t;

/* Callback prototypes */
static void *H5FD_family_fapl_copy(const void *_old_fa);
static herr_t H5FD_family_fapl_free(void *_fa);
static void *H5FD_family_dxpl_copy(const void *_old_dx);
static herr_t H5FD_family_dxpl_free(void *_dx);
static H5FD_t *H5FD_family_open(const char *name, unsigned flags,
				hid_t fapl_id, haddr_t maxaddr);
static herr_t H5FD_family_close(H5FD_t *_file);
static int H5FD_family_cmp(const H5FD_t *_f1, const H5FD_t *_f2);
static haddr_t H5FD_family_get_eoa(H5FD_t *_file);
static herr_t H5FD_family_set_eoa(H5FD_t *_file, haddr_t eoa);
static haddr_t H5FD_family_get_eof(H5FD_t *_file);
static herr_t H5FD_family_read(H5FD_t *_file, hid_t dxpl_id, haddr_t addr,
			       hsize_t size, void *_buf/*out*/);
static herr_t H5FD_family_write(H5FD_t *_file, hid_t dxpl_id, haddr_t addr,
				hsize_t size, const void *_buf);
static herr_t H5FD_family_flush(H5FD_t *_file);

/* The class struct */
static const H5FD_class_t H5FD_family_g = {
    "family",					/*name			*/
    HADDR_MAX,					/*maxaddr		*/
    sizeof(H5FD_family_fapl_t),			/*fapl_size		*/
    H5FD_family_fapl_copy,			/*fapl_copy		*/
    H5FD_family_fapl_free,			/*fapl_free		*/
    sizeof(H5FD_family_dxpl_t),			/*dxpl_size		*/
    H5FD_family_dxpl_copy,			/*dxpl_copy		*/
    H5FD_family_dxpl_free,			/*dxpl_free		*/
    H5FD_family_open,				/*open			*/
    H5FD_family_close,				/*close			*/
    H5FD_family_cmp,				/*cmp			*/
    NULL,					/*alloc			*/
    NULL,					/*free			*/
    H5FD_family_get_eoa,			/*get_eoa		*/
    H5FD_family_set_eoa,			/*set_eoa		*/
    H5FD_family_get_eof,			/*get_eof		*/
    H5FD_family_read,				/*read			*/
    H5FD_family_write,				/*write			*/
    H5FD_family_flush,				/*flush			*/
    H5FD_FLMAP_SINGLE,				/*fl_map		*/
};


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_init
 *
 * Purpose:	Initialize this driver by registering the driver with the
 *		library.
 *
 * Return:	Success:	The driver ID for the family driver.
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
H5FD_family_init(void)
{
    if (!H5FD_FAMILY_g) {
	H5FD_FAMILY_g = H5FDregister(&H5FD_family_g);
    }
    return H5FD_FAMILY_g;
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_fapl_family
 *
 * Purpose:	Sets the file access property list FAPL_ID to use the family
 *		driver. The MEMB_SIZE is the size in bytes of each file
 *		member (used only when creating a new file) and the
 *		MEMB_FAPL_ID is a file access property list to be used for
 *		each family member.
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
H5Pset_fapl_family(hid_t fapl_id, hsize_t memb_size, hid_t memb_fapl_id)
{
    H5FD_family_fapl_t	fa;
    
    /*NO TRACE*/
    
    /* Check arguments */
    if (H5P_FILE_ACCESS!=H5Pget_class(fapl_id)) return -1;
    if (H5P_DEFAULT!=memb_fapl_id &&
	H5P_FILE_ACCESS!=H5Pget_class(memb_fapl_id)) return -1;

    /* Initialize driver specific information */
    fa.memb_size = memb_size;
    fa.memb_fapl_id = memb_fapl_id;
    return H5Pset_driver(fapl_id, H5FD_FAMILY, &fa);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_fapl_family
 *
 * Purpose:	Returns information about the family file access property
 *		list though the function arguments.
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
H5Pget_fapl_family(hid_t fapl_id, hsize_t *memb_size/*out*/,
		   hid_t *memb_fapl_id/*out*/)
{
    H5FD_family_fapl_t	*fa;
    
    /*NO TRACE*/

    if (H5P_FILE_ACCESS!=H5Pget_class(fapl_id)) return -1;
    if (H5FD_FAMILY!=H5Pget_driver(fapl_id)) return -1;
    if (NULL==(fa=H5Pget_driver_info(fapl_id))) return -1;
    if (memb_size) *memb_size = fa->memb_size;
    if (memb_fapl_id) *memb_fapl_id = H5Pcopy(fa->memb_fapl_id);
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_fapl_copy
 *
 * Purpose:	Copies the family-specific file access properties.
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
H5FD_family_fapl_copy(const void *_old_fa)
{
    const H5FD_family_fapl_t *old_fa = (const H5FD_family_fapl_t*)_old_fa;
    H5FD_family_fapl_t *new_fa = malloc(sizeof(H5FD_family_fapl_t));
    assert(new_fa);

    memcpy(new_fa, old_fa, sizeof(H5FD_family_fapl_t));
    new_fa->memb_fapl_id = H5Pcopy(old_fa->memb_fapl_id);
    return new_fa;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_fapl_free
 *
 * Purpose:	Frees the family-specific file access properties.
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
H5FD_family_fapl_free(void *_fa)
{
    H5FD_family_fapl_t	*fa = (H5FD_family_fapl_t*)_fa;
    H5Pclose(fa->memb_fapl_id);
    free(fa);
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_dxpl_copy
 *
 * Purpose:	Copes the family-specific data transfer properties.
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
H5FD_family_dxpl_copy(const void *_old_dx)
{
    const H5FD_family_dxpl_t *old_dx = (const H5FD_family_dxpl_t*)_old_dx;
    H5FD_family_dxpl_t *new_dx = malloc(sizeof(H5FD_family_dxpl_t));
    assert(new_dx);

    memcpy(new_dx, old_dx, sizeof(H5FD_family_dxpl_t));
    new_dx->memb_dxpl_id = H5Pcopy(old_dx->memb_dxpl_id);
    return new_dx;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_dxpl_free
 *
 * Purpose:	Frees the family-specific data transfer properties.
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
H5FD_family_dxpl_free(void *_dx)
{
    H5FD_family_dxpl_t	*dx = (H5FD_family_dxpl_t*)_dx;
    H5Pclose(dx->memb_dxpl_id);
    free(dx);
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_open
 *
 * Purpose:	Creates and/or opens a family of files as an HDF5 file.
 *
 * Return:	Success:	A pointer to a new file dat structure. The
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
H5FD_family_open(const char *name, unsigned flags, hid_t fapl_id,
		 haddr_t maxaddr)
{
    H5FD_family_t	*file=NULL;
    char		memb_name[4096], temp[4096];
    hsize_t		eof;
    unsigned		t_flags = flags & ~H5F_ACC_CREAT;
    
    /* Check arguments */
    if (!name || !*name) return NULL;
    if (0==maxaddr || HADDR_UNDEF==maxaddr) return NULL;

    /* Initialize file from file access properties */
    if (NULL==(file=calloc(1, sizeof(H5FD_family_t)))) return NULL;
    if (H5P_DEFAULT==fapl_id) {
	file->memb_fapl_id = H5P_DEFAULT;
	file->memb_size = 1024*1024*1024; /*1GB*/
    } else {
	H5FD_family_fapl_t *fa = H5Pget_driver_info(fapl_id);
	file->memb_fapl_id = fa->memb_fapl_id;
	file->memb_size = fa->memb_size;
    }
    file->name = malloc(strlen(name)+1);
    strcpy(file->name, name);
    file->flags = flags;
    
    /* Check that names are unique */
    sprintf(memb_name, name, 0);
    sprintf(temp, name, 1);
    if (!strcmp(memb_name, temp)) return NULL;

    /* Open all the family members */
    while (1) {
	sprintf(memb_name, name, file->nmembs);

	/* Enlarge member array */
	if (file->nmembs>=file->amembs) {
	    int n = MAX(64, 2*file->amembs);
	    H5FD_t **x = realloc(file->memb, n*sizeof(H5FD_t*));
	    if (!x) goto error;
	    file->amembs = n;
	    file->memb = x;
	}
	
	/*
	 * Attempt to open file. If the first file cannot be opened then fail;
	 * otherwise an open failure means that we've reached the last member.
	 * Allow H5F_ACC_CREAT only on the first family member.
	 */
	H5E_BEGIN_TRY {
	    file->memb[file->nmembs] = H5FDopen(memb_name,
						0==file->nmembs?flags:t_flags,
						file->memb_fapl_id,
						HADDR_UNDEF);
	} H5E_END_TRY;
	if (!file->memb[file->nmembs]) {
	    if (0==file->nmembs) goto error;
	    H5Eclear();
	    break;
	}
	file->nmembs++;
    }
    
    /*
     * The size of the first member determines the size of all the members,
     * but if the size of the first member is zero then use the member size
     * from the file access property list.
     */
    if ((eof=H5FDget_eof(file->memb[0]))) file->memb_size = eof;

    return (H5FD_t*)file;

 error:
    /* Cleanup and fail */
    if (file) {
	int i;
	for (i=0; i<file->nmembs; i++) {
	    if (file->memb[i]) H5FDclose(file->memb[i]);
	}
	free(file);
    }
    return NULL;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_close
 *
 * Purpose:	Closes a family of files.
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
H5FD_family_close(H5FD_t *_file)
{
    H5FD_family_t	*file = (H5FD_family_t*)_file;
    int			i, nerrors=0;

    /* Close as many members as possible */
    for (i=0; i<file->nmembs; i++) {
	if (file->memb[i]) {
	    if (H5FDclose(file->memb[i])<0) {
		nerrors++;
	    } else {
		file->memb[i] = NULL;
	    }
	}
    }
    if (nerrors) return -1;

    /* Clean up other stuff */
    H5Pclose(file->memb_fapl_id);
    free(file->memb);
    free(file->name);
    free(file);
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_cmp
 *
 * Purpose:	Compares two file families to see if they are the same. It
 *		does this by comparing the first member of the two families.
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
H5FD_family_cmp(const H5FD_t *_f1, const H5FD_t *_f2)
{
    const H5FD_family_t	*f1 = (const H5FD_family_t*)_f1;
    const H5FD_family_t	*f2 = (const H5FD_family_t*)_f2;

    assert(f1->nmembs>=1 && f1->memb[0]);
    assert(f2->nmembs>=1 && f2->memb[0]);
    

    return H5FDcmp(f1->memb[0], f2->memb[0]);
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_get_eoa
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
H5FD_family_get_eoa(H5FD_t *_file)
{
    H5FD_family_t	*file = (H5FD_family_t*)_file;
    return file->eoa;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_set_eoa
 *
 * Purpose:	Set the end-of-address marker for the file.
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
H5FD_family_set_eoa(H5FD_t *_file, haddr_t eoa)
{
    H5FD_family_t	*file = (H5FD_family_t*)_file;
    haddr_t		addr=eoa;
    int			i;
    char		memb_name[4096];

    for (i=0; addr || i<file->nmembs; i++) {

	/* Enlarge member array */
	if (i>=file->amembs) {
	    int n = MAX(64, 2*file->amembs);
	    H5FD_t **x = realloc(file->memb, n*sizeof(H5FD_t*));
	    if (!x) return -1;
	    file->amembs = n;
	    file->memb = x;
	    file->nmembs = i;
	}

	/* Create another file if necessary */
	if (i>=file->nmembs || !file->memb[i]) {
	    file->nmembs = MAX(file->nmembs, i+1);
	    sprintf(memb_name, file->name, i);
	    H5E_BEGIN_TRY {
		file->memb[i] = H5FDopen(memb_name, file->flags|H5F_ACC_CREAT,
					 file->memb_fapl_id, file->memb_size);
	    } H5E_END_TRY;
	    if (NULL==file->memb[i]) return -1;
	}
	
	/* Set the EOA marker for the member */
	if (addr>file->memb_size) {
	    H5FDset_eoa(file->memb[i], file->memb_size);
	    addr -= file->memb_size;
	} else {
	    H5FDset_eoa(file->memb[i], addr);
	    addr = 0;
	}
    }

    file->eoa = eoa;
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_get_eof
 *
 * Purpose:	Returns the end-of-file marker, which is the greater of
 *		either the total family size or the current EOA marker.
 *
 * Return:	Success:	End of file address, the first address past
 *				the end of the family of files or the current
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
H5FD_family_get_eof(H5FD_t *_file)
{
    H5FD_family_t	*file = (H5FD_family_t*)_file;
    haddr_t		eof;
    int			i;

    /*
     * Find the last member that has a non-zero EOF and break out of the loop
     * with `i' equal to that member. If all members have zero EOF then exit
     * loop with i==0.
     */
    for (i=file->nmembs-1; i>=0; --i) {
	if ((eof=H5FDget_eof(file->memb[i]))) break;
	if (0==i) break;
    }

    /*
     * The file size is the number of members before the i'th member plus the
     * size of the i'th member.
     */
    eof += i*file->memb_size;
    return MAX(eof, file->eoa);
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_read
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
H5FD_family_read(H5FD_t *_file, hid_t dxpl_id, haddr_t addr, hsize_t size,
		 void *_buf/*out*/)
{
    H5FD_family_t	*file = (H5FD_family_t*)_file;
    unsigned char	*buf = (unsigned char*)_buf;
    hid_t		memb_dxpl_id = H5P_DEFAULT;
    int			i;
    haddr_t		sub;
    hsize_t		req;

    /*
     * Get the member data transfer property list. If the transfer property
     * list does not belong to this driver then assume defaults
     */
    if (H5P_DEFAULT!=dxpl_id && H5FD_FAMILY==H5Pget_driver(dxpl_id)) {
	H5FD_family_dxpl_t *dx = H5Pget_driver_info(dxpl_id);
	assert(H5P_DATA_XFER==H5Pget_class(dxpl_id));
	assert(dx);
	memb_dxpl_id = dx->memb_dxpl_id;
    }

    /* Read from each member */
    while (size>0) {
	i = addr / file->memb_size;
	sub = addr % file->memb_size;
	req = MIN(size, file->memb_size-sub);
	assert(i<file->nmembs);

	if (H5FDread(file->memb[i], memb_dxpl_id, sub, req, buf)<0) {
	    return -1;
	}

	addr += req;
	buf += req;
	size -= req;
    }

    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_write
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
H5FD_family_write(H5FD_t *_file, hid_t dxpl_id, haddr_t addr, hsize_t size,
		  const void *_buf)
{
    H5FD_family_t	*file = (H5FD_family_t*)_file;
    const unsigned char	*buf = (const unsigned char*)_buf;
    hid_t		memb_dxpl_id = H5P_DEFAULT;
    int			i;
    haddr_t		sub;
    hsize_t		req;

    /*
     * Get the member data transfer property list. If the transfer property
     * list does not belong to this driver then assume defaults.
     */
    if (H5P_DEFAULT!=dxpl_id && H5FD_FAMILY==H5Pget_driver(dxpl_id)) {
	H5FD_family_dxpl_t *dx = H5Pget_driver_info(dxpl_id);
	assert(H5P_DATA_XFER==H5Pget_class(dxpl_id));
	assert(dx);
	memb_dxpl_id = dx->memb_dxpl_id;
    }

    /* Write to each member */
    while (size>0) {
	i = addr / file->memb_size;
	sub = addr % file->memb_size;
	req = MIN(size, file->memb_size-sub);
	assert(i<file->nmembs);

	if (H5FDwrite(file->memb[i], memb_dxpl_id, sub, req, buf)<0) {
	    return -1;
	}

	addr += req;
	buf += req;
	size -= req;
    }

    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_family_flush
 *
 * Purpose:	Flushes all family members.
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
H5FD_family_flush(H5FD_t *_file)
{
    H5FD_family_t	*file = (H5FD_family_t*)_file;
    int			i, nerrors=0;

    for (i=0; i<file->nmembs; i++) {
	if (file->memb[i] && H5FDflush(file->memb[i])<0) {
	    nerrors++;
	}
    }

    return nerrors?-1:0;
}
