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
 * Programmer: Robb Matzke <matzke@llnl.gov>
 *	       Tuesday, November 25, 1997
 */

#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */
#define H5O_PACKAGE		/*suppress error about including H5Opkg	  */

/* Pablo information */
/* (Put before include files to avoid problems with inline functions) */
#define PABLO_MASK	H5O_efl_mask

#include "H5private.h"		/* Generic Functions			*/
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"             /* File access				*/
#include "H5HLprivate.h"	/* Local Heaps				*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Opkg.h"             /* Object headers			*/

/* PRIVATE PROTOTYPES */
static void *H5O_efl_decode(H5F_t *f, hid_t dxpl_id, const uint8_t *p, H5O_shared_t *sh);
static herr_t H5O_efl_encode(H5F_t *f, uint8_t *p, const void *_mesg);
static void *H5O_efl_copy(const void *_mesg, void *_dest);
static size_t H5O_efl_size(H5F_t *f, const void *_mesg);
static herr_t H5O_efl_reset(void *_mesg);
static herr_t H5O_efl_debug(H5F_t *f, hid_t dxpl_id, const void *_mesg, FILE * stream,
			    int indent, int fwidth);
static herr_t H5O_efl_read (const H5O_efl_t *efl, haddr_t addr, size_t size,
    uint8_t *buf);
static herr_t H5O_efl_write(const H5O_efl_t *efl, haddr_t addr, size_t size,
    const uint8_t *buf);

/* This message derives from H5O */
const H5O_class_t H5O_EFL[1] = {{
    H5O_EFL_ID,		    	/*message id number		*/
    "external file list",   	/*message name for debugging    */
    sizeof(H5O_efl_t),	    	/*native message size	    	*/
    H5O_efl_decode,	    	/*decode message		*/
    H5O_efl_encode,	    	/*encode message		*/
    H5O_efl_copy,	    	/*copy native value		*/
    H5O_efl_size,	    	/*size of message on disk	*/
    H5O_efl_reset,	    	/*reset method		    	*/
    NULL,		            /* free method			*/
    NULL,		        /* file delete method		*/
    NULL,			/* link method			*/
    NULL,	  	    	/*get share method		*/
    NULL,			/*set share method		*/
    H5O_efl_debug,	    	/*debug the message		*/
}};

#define H5O_EFL_VERSION		1


/*-------------------------------------------------------------------------
 * Function:	H5O_efl_decode
 *
 * Purpose:	Decode an external file list message and return a pointer to
 *		the message (and some other data).
 *
 * Return:	Success:	Ptr to a new message struct.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, November 25, 1997
 *
 * Modifications:
 *	Robb Matzke, 1998-07-20
 *	Rearranged the message to add a version number near the beginning.
 *	
 *-------------------------------------------------------------------------
 */
static void *
H5O_efl_decode(H5F_t *f, hid_t dxpl_id, const uint8_t *p, H5O_shared_t UNUSED *sh)
{
    H5O_efl_t		*mesg = NULL;
    int			version;
    const char		*s = NULL;
    const H5HL_t        *heap;
    size_t		u;      /* Local index variable */
    void *ret_value;            /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_efl_decode);

    /* Check args */
    assert(f);
    assert(p);
    assert (!sh);

    if (NULL==(mesg = H5MM_calloc(sizeof(H5O_efl_t))))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Version */
    version = *p++;
    if (version!=H5O_EFL_VERSION)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "bad version number for external file list message");

    /* Reserved */
    p += 3;

    /* Number of slots */
    UINT16DECODE(p, mesg->nalloc);
    assert(mesg->nalloc>0);
    UINT16DECODE(p, mesg->nused);
    assert(mesg->nused <= mesg->nalloc);

    /* Heap address */
    H5F_addr_decode(f, &p, &(mesg->heap_addr));

#ifndef NDEBUG
    assert (H5F_addr_defined(mesg->heap_addr));

    if (NULL == (heap = H5HL_protect(f, dxpl_id, mesg->heap_addr)))
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, NULL, "unable to read protect link value")
    
    s = H5HL_offset_into(f, heap, 0);

    assert (s && !*s);

    if (H5HL_unprotect(f, dxpl_id, heap, mesg->heap_addr) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, NULL, "unable to read unprotect link value")
#endif

    /* Decode the file list */
    mesg->slot = H5MM_calloc(mesg->nalloc*sizeof(H5O_efl_entry_t));
    if (NULL==mesg->slot)
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
    for (u=0; u<mesg->nused; u++) {
	/* Name */
	H5F_DECODE_LENGTH (f, p, mesg->slot[u].name_offset);

        if (NULL == (heap = H5HL_protect(f, dxpl_id, mesg->heap_addr)))
            HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, NULL, "unable to read protect link value")
    
        s = H5HL_offset_into(f, heap, mesg->slot[u].name_offset);
	assert (s && *s);
	mesg->slot[u].name = H5MM_xstrdup (s);
        assert(mesg->slot[u].name);

        if (H5HL_unprotect(f, dxpl_id, heap, mesg->heap_addr) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, NULL, "unable to read unprotect link value")
	
	/* File offset */
	H5F_DECODE_LENGTH (f, p, mesg->slot[u].offset);

	/* Size */
	H5F_DECODE_LENGTH (f, p, mesg->slot[u].size);
	assert (mesg->slot[u].size>0);
    }

    /* Set return value */
    ret_value=mesg;

done:
    if(ret_value==NULL) {
        if(mesg!=NULL)
            H5MM_xfree (mesg);
    } /* end if */
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_efl_encode
 *
 * Purpose:	Encodes a message.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Tuesday, November 25, 1997
 *
 * Modifications:
 *	Robb Matzke, 1998-07-20
 *	Rearranged the message to add a version number near the beginning.
 *
 * 	Robb Matzke, 1999-10-14
 *	Entering the name into the local heap happens when the dataset is
 *	created. Otherwise we could end up in infinite recursion if the heap
 *	happens to hash to the same cache slot as the object header.
 *	
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_efl_encode(H5F_t *f, uint8_t *p, const void *_mesg)
{
    const H5O_efl_t	*mesg = (const H5O_efl_t *)_mesg;
    size_t		u;      /* Local index variable */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_efl_encode);

    /* check args */
    assert(f);
    assert(mesg);
    assert(p);

    /* Version */
    *p++ = H5O_EFL_VERSION;

    /* Reserved */
    *p++ = 0;
    *p++ = 0;
    *p++ = 0;

    /* Number of slots */
    assert (mesg->nalloc>0);
    UINT16ENCODE(p, mesg->nused); /*yes, twice*/
    assert (mesg->nused>0 && mesg->nused<=mesg->nalloc);
    UINT16ENCODE(p, mesg->nused);

    /* Heap address */
    assert (H5F_addr_defined(mesg->heap_addr));
    H5F_addr_encode(f, &p, mesg->heap_addr);

    /* Encode file list */
    for (u=0; u<mesg->nused; u++) {
	/*
	 * The name should have been added to the heap when the dataset was
	 * created.
	 */
	assert(mesg->slot[u].name_offset);
	H5F_ENCODE_LENGTH (f, p, mesg->slot[u].name_offset);
	H5F_ENCODE_LENGTH (f, p, mesg->slot[u].offset);
	H5F_ENCODE_LENGTH (f, p, mesg->slot[u].size);
    }

    FUNC_LEAVE_NOAPI(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_efl_copy
 *
 * Purpose:	Copies a message from _MESG to _DEST, allocating _DEST if
 *		necessary.
 *
 * Return:	Success:	Ptr to _DEST
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, November 25, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_efl_copy(const void *_mesg, void *_dest)
{
    const H5O_efl_t	*mesg = (const H5O_efl_t *) _mesg;
    H5O_efl_t		*dest = (H5O_efl_t *) _dest;
    size_t		u;              /* Local index variable */
    void                *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_efl_copy);

    /* check args */
    assert(mesg);
    if (!dest) {
	if (NULL==(dest = H5MM_calloc(sizeof(H5O_efl_t))) ||
                NULL==(dest->slot=H5MM_malloc(mesg->nalloc* sizeof(H5O_efl_entry_t))))
	    HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
	
    } else if (dest->nalloc<mesg->nalloc) {
	H5MM_xfree(dest->slot);
	if (NULL==(dest->slot = H5MM_malloc(mesg->nalloc*
					    sizeof(H5O_efl_entry_t))))
	    HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
    }
    dest->heap_addr = mesg->heap_addr;
    dest->nalloc = mesg->nalloc;
    dest->nused = mesg->nused;

    for (u = 0; u < mesg->nused; u++) {
	dest->slot[u] = mesg->slot[u];
	dest->slot[u].name = H5MM_xstrdup (mesg->slot[u].name);
    }

    /* Set return value */
    ret_value=dest;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_efl_size
 *
 * Purpose:	Returns the size of the raw message in bytes not counting the
 *		message type or size fields, but only the data fields.	This
 *		function doesn't take into account message alignment. This
 *		function doesn't count unused slots.
 *
 * Return:	Success:	Message data size in bytes.
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *		Tuesday, November 25, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O_efl_size(H5F_t *f, const void *_mesg)
{
    const H5O_efl_t	*mesg = (const H5O_efl_t *) _mesg;
    size_t		ret_value = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_efl_size);

    /* check args */
    assert(f);
    assert(mesg);

    ret_value = H5F_SIZEOF_ADDR(f) +			/*heap address	*/
		2 +					/*slots allocated*/
		2 +					/*num slots used*/
		4 +					/*reserved	*/
		mesg->nused * (H5F_SIZEOF_SIZE(f) +	/*name offset	*/
			       H5F_SIZEOF_SIZE(f) +	/*file offset	*/
			       H5F_SIZEOF_SIZE(f));	/*file size	*/

    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_efl_reset
 *
 * Purpose:	Frees internal pointers and resets the message to an
 *		initialial state.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Tuesday, November 25, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_efl_reset(void *_mesg)
{
    H5O_efl_t	*mesg = (H5O_efl_t *) _mesg;
    size_t	u;              /* Local index variable */
    
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_efl_reset);

    /* check args */
    assert(mesg);

    /* reset */
    for (u=0; u<mesg->nused; u++)
	mesg->slot[u].name = H5MM_xfree (mesg->slot[u].name);
    mesg->heap_addr = HADDR_UNDEF;
    mesg->nused = mesg->nalloc = 0;
    if(mesg->slot)
        mesg->slot = H5MM_xfree(mesg->slot);

    FUNC_LEAVE_NOAPI(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_efl_total_size
 *
 * Purpose:	Return the total size of the external file list by summing
 *		the sizes of all of the files.
 *
 * Return:	Success:	Total reserved size for external data.
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March  3, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5O_efl_total_size (H5O_efl_t *efl)
{
    hsize_t	ret_value = 0, tmp;
    
    FUNC_ENTER_NOAPI_NOINIT(H5O_efl_total_size);

    if (efl->nused>0 &&
	H5O_EFL_UNLIMITED==efl->slot[efl->nused-1].size) {
	ret_value = H5O_EFL_UNLIMITED;
    } else {
        size_t		u;      /* Local index variable */

	for (u=0; u<efl->nused; u++, ret_value=tmp) {
	    tmp = ret_value + efl->slot[u].size;
	    if (tmp<=ret_value)
		HGOTO_ERROR (H5E_EFL, H5E_OVERFLOW, 0, "total external storage size overflowed");
	}
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_efl_read
 *
 * Purpose:	Reads data from an external file list.  It is an error to
 *		read past the logical end of file, but reading past the end
 *		of any particular member of the external file list results in
 *		zeros.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Wednesday, March  4, 1998
 *
 * Modifications:
 *		Robb Matzke, 1999-07-28
 *		The ADDR argument is passed by value.
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_efl_read (const H5O_efl_t *efl, haddr_t addr, size_t size, uint8_t *buf)
{
    int		fd=-1;
    size_t	to_read;
#ifndef NDEBUG
    hsize_t     tempto_read;
#endif /* NDEBUG */
    hsize_t     skip=0;
    haddr_t     cur;
    ssize_t	n;
    size_t      u;                      /* Local index variable */
    herr_t      ret_value=SUCCEED;       /* Return value */
    
    FUNC_ENTER_NOAPI_NOINIT(H5O_efl_read);

    /* Check args */
    assert (efl && efl->nused>0);
    assert (H5F_addr_defined (addr));
    assert (size < SIZET_MAX);
    assert (buf || 0==size);

    /* Find the first efl member from which to read */
    for (u=0, cur=0; u<efl->nused; u++) {
	if (H5O_EFL_UNLIMITED==efl->slot[u].size || addr < cur+efl->slot[u].size) {
	    skip = addr - cur;
	    break;
	}
  	cur += efl->slot[u].size;
    }
    
    /* Read the data */
    while (size) {
	if (u>=efl->nused)
	    HGOTO_ERROR (H5E_EFL, H5E_OVERFLOW, FAIL, "read past logical end of file");
	if (H5F_OVERFLOW_HSIZET2OFFT (efl->slot[u].offset+skip))
	    HGOTO_ERROR (H5E_EFL, H5E_OVERFLOW, FAIL, "external file address overflowed");
	if ((fd=HDopen (efl->slot[u].name, O_RDONLY, 0))<0)
	    HGOTO_ERROR (H5E_EFL, H5E_CANTOPENFILE, FAIL, "unable to open external raw data file");
	if (HDlseek (fd, (off_t)(efl->slot[u].offset+skip), SEEK_SET)<0)
	    HGOTO_ERROR (H5E_EFL, H5E_SEEKERROR, FAIL, "unable to seek in external raw data file");
#ifndef NDEBUG
	tempto_read = MIN(efl->slot[u].size-skip,(hsize_t)size);
        H5_CHECK_OVERFLOW(tempto_read,hsize_t,size_t);
	to_read = (size_t)tempto_read;
#else /* NDEBUG */
	to_read = MIN((size_t)(efl->slot[u].size-skip), size);
#endif /* NDEBUG */
	if ((n=HDread (fd, buf, to_read))<0) {
	    HGOTO_ERROR (H5E_EFL, H5E_READERROR, FAIL, "read error in external raw data file");
	} else if ((size_t)n<to_read) {
	    HDmemset (buf+n, 0, to_read-n);
	}
	HDclose (fd);
	fd = -1;
	size -= to_read;
	buf += to_read;
	skip = 0;
	u++;
    }
    
done:
    if (fd>=0)
        HDclose (fd);

    FUNC_LEAVE_NOAPI(ret_value);
}
	

/*-------------------------------------------------------------------------
 * Function:	H5O_efl_write
 *
 * Purpose:	Writes data to an external file list.  It is an error to
 *		write past the logical end of file, but writing past the end
 *		of any particular member of the external file list just
 *		extends that file.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Wednesday, March  4, 1998
 *
 * Modifications:
 *		Robb Matzke, 1999-07-28
 *		The ADDR argument is passed by value.
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_efl_write (const H5O_efl_t *efl, haddr_t addr, size_t size, const uint8_t *buf)
{
    int		fd=-1;
    size_t	to_write;
#ifndef NDEBUG
    hsize_t	tempto_write;
#endif /* NDEBUG */
    haddr_t     cur;
    hsize_t     skip=0;
    size_t	u;                      /* Local index variable */
    herr_t      ret_value=SUCCEED;       /* Return value */
    
    FUNC_ENTER_NOAPI_NOINIT(H5O_efl_write);

    /* Check args */
    assert (efl && efl->nused>0);
    assert (H5F_addr_defined (addr));
    assert (size < SIZET_MAX);
    assert (buf || 0==size);

    /* Find the first efl member in which to write */
    for (u=0, cur=0; u<efl->nused; u++) {
	if (H5O_EFL_UNLIMITED==efl->slot[u].size || addr < cur+efl->slot[u].size) {
	    skip = addr - cur;
	    break;
	}
	cur += efl->slot[u].size;
    }
    
    /* Write the data */
    while (size) {
	if (u>=efl->nused)
	    HGOTO_ERROR (H5E_EFL, H5E_OVERFLOW, FAIL, "write past logical end of file");
	if (H5F_OVERFLOW_HSIZET2OFFT (efl->slot[u].offset+skip))
	    HGOTO_ERROR (H5E_EFL, H5E_OVERFLOW, FAIL, "external file address overflowed");
	if ((fd=HDopen (efl->slot[u].name, O_CREAT|O_RDWR, 0666))<0) {
	    if (HDaccess (efl->slot[u].name, F_OK)<0) {
		HGOTO_ERROR (H5E_EFL, H5E_CANTOPENFILE, FAIL, "external raw data file does not exist");
	    } else {
		HGOTO_ERROR (H5E_EFL, H5E_CANTOPENFILE, FAIL, "unable to open external raw data file");
	    }
	}
	if (HDlseek (fd, (off_t)(efl->slot[u].offset+skip), SEEK_SET)<0)
	    HGOTO_ERROR (H5E_EFL, H5E_SEEKERROR, FAIL, "unable to seek in external raw data file");
#ifndef NDEBUG
	tempto_write = MIN(efl->slot[u].size-skip,(hsize_t)size);
        H5_CHECK_OVERFLOW(tempto_write,hsize_t,size_t);
        to_write = (size_t)tempto_write;
#else /* NDEBUG */
	to_write = MIN((size_t)(efl->slot[u].size-skip), size);
#endif /* NDEBUG */
	if ((size_t)HDwrite (fd, buf, to_write)!=to_write)
	    HGOTO_ERROR (H5E_EFL, H5E_READERROR, FAIL, "write error in external raw data file");
	HDclose (fd);
	fd = -1;
	size -= to_write;
	buf += to_write;
	skip = 0;
	u++;
    }
    
done:
    if (fd>=0)
        HDclose (fd);

    FUNC_LEAVE_NOAPI(ret_value);
}
	

/*-------------------------------------------------------------------------
 * Function:	H5O_efl_readvv
 *
 * Purpose:	Reads data from an external file list.  It is an error to
 *		read past the logical end of file, but reading past the end
 *		of any particular member of the external file list results in
 *		zeros.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, May  7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5O_efl_readvv(H5D_io_info_t *io_info,
    size_t dset_max_nseq, size_t *dset_curr_seq, size_t dset_len_arr[], hsize_t dset_offset_arr[],
    size_t mem_max_nseq, size_t *mem_curr_seq, size_t mem_len_arr[], hsize_t mem_offset_arr[],
    void *_buf)
{
    const H5O_efl_t *efl=&(io_info->store->efl); /* Pointer to efl info */
    unsigned char *buf;         /* Pointer to buffer to write */
    haddr_t addr;               /* Actual address to read */
    size_t size;                /* Size of sequence in bytes */
    size_t u;                   /* Counting variable */
    size_t v;                   /* Counting variable */
    ssize_t ret_value=0;        /* Return value */
    
    FUNC_ENTER_NOAPI(H5O_efl_readvv, FAIL);

    /* Check args */
    assert (efl && efl->nused>0);
    assert (_buf);

    /* Work through all the sequences */
    for(u=*dset_curr_seq, v=*mem_curr_seq; u<dset_max_nseq && v<mem_max_nseq; ) {
        /* Choose smallest buffer to write */
        if(mem_len_arr[v]<dset_len_arr[u])
            size=mem_len_arr[v];
        else
            size=dset_len_arr[u];

        /* Compute offset on disk */
        addr=dset_offset_arr[u];

        /* Compute offset in memory */
        buf = (unsigned char *)_buf + mem_offset_arr[v];

        /* Read data */
        if (H5O_efl_read(efl, addr, size, buf)<0)
            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "block write failed");

        /* Update memory information */
        mem_len_arr[v]-=size;
        mem_offset_arr[v]+=size;
        if(mem_len_arr[v]==0)
            v++;

        /* Update file information */
        dset_len_arr[u]-=size;
        dset_offset_arr[u]+=size;
        if(dset_len_arr[u]==0)
            u++;

        /* Increment number of bytes copied */
        ret_value+=size;
    } /* end for */

    /* Update current sequence vectors */
    *dset_curr_seq=u;
    *mem_curr_seq=v;

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5O_efl_readvv() */
	

/*-------------------------------------------------------------------------
 * Function:	H5O_efl_writevv
 *
 * Purpose:	Writes data to an external file list.  It is an error to
 *		write past the logical end of file, but writing past the end
 *		of any particular member of the external file list just
 *		extends that file.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Friday, May  2, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5O_efl_writevv(H5D_io_info_t *io_info,
    size_t dset_max_nseq, size_t *dset_curr_seq, size_t dset_len_arr[], hsize_t dset_offset_arr[],
    size_t mem_max_nseq, size_t *mem_curr_seq, size_t mem_len_arr[], hsize_t mem_offset_arr[],
    const void *_buf)
{
    const H5O_efl_t *efl=&(io_info->store->efl); /* Pointer to efl info */
    const unsigned char *buf;   /* Pointer to buffer to write */
    haddr_t addr;               /* Actual address to read */
    size_t size;                /* Size of sequence in bytes */
    size_t u;                   /* Counting variable */
    size_t v;                   /* Counting variable */
    ssize_t ret_value=0;        /* Return value */
    
    FUNC_ENTER_NOAPI(H5O_efl_writevv, FAIL);

    /* Check args */
    assert (efl && efl->nused>0);
    assert (_buf);

    /* Work through all the sequences */
    for(u=*dset_curr_seq, v=*mem_curr_seq; u<dset_max_nseq && v<mem_max_nseq; ) {
        /* Choose smallest buffer to write */
        if(mem_len_arr[v]<dset_len_arr[u])
            size=mem_len_arr[v];
        else
            size=dset_len_arr[u];

        /* Compute offset on disk */
        addr=dset_offset_arr[u];

        /* Compute offset in memory */
        buf = (const unsigned char *)_buf + mem_offset_arr[v];

        /* Write data */
        if (H5O_efl_write(efl, addr, size, buf)<0)
            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "block write failed");

        /* Update memory information */
        mem_len_arr[v]-=size;
        mem_offset_arr[v]+=size;
        if(mem_len_arr[v]==0)
            v++;

        /* Update file information */
        dset_len_arr[u]-=size;
        dset_offset_arr[u]+=size;
        if(dset_len_arr[u]==0)
            u++;

        /* Increment number of bytes copied */
        ret_value+=size;
    } /* end for */

    /* Update current sequence vectors */
    *dset_curr_seq=u;
    *mem_curr_seq=v;

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5O_efl_writevv() */
	

/*-------------------------------------------------------------------------
 * Function:	H5O_efl_debug
 *
 * Purpose:	Prints debugging info for a message.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Tuesday, November 25, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_efl_debug(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, const void *_mesg, FILE * stream,
	      int indent, int fwidth)
{
    const H5O_efl_t	   *mesg = (const H5O_efl_t *) _mesg;
    char		    buf[64];
    size_t		    u;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_efl_debug);

    /* check args */
    assert(f);
    assert(mesg);
    assert(stream);
    assert(indent >= 0);
    assert(fwidth >= 0);

    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
	      "Heap address:", mesg->heap_addr);

    HDfprintf(stream, "%*s%-*s %u/%u\n", indent, "", fwidth,
	      "Slots used/allocated:",
	      mesg->nused, mesg->nalloc);

    for (u = 0; u < mesg->nused; u++) {
	sprintf (buf, "File %u", (unsigned)u);
	HDfprintf (stream, "%*s%s:\n", indent, "", buf);
	
	HDfprintf(stream, "%*s%-*s \"%s\"\n", indent+3, "", MAX (fwidth-3, 0),
		  "Name:",
		  mesg->slot[u].name);
	
	HDfprintf(stream, "%*s%-*s %lu\n", indent+3, "", MAX (fwidth-3, 0),
		  "Name offset:",
		  (unsigned long)(mesg->slot[u].name_offset));

	HDfprintf (stream, "%*s%-*s %lu\n", indent+3, "", MAX (fwidth-3, 0),
		   "Offset of data in file:",
		   (unsigned long)(mesg->slot[u].offset));

	HDfprintf (stream, "%*s%-*s %lu\n", indent+3, "", MAX (fwidth-3, 0),
		   "Bytes reserved for data:",
		   (unsigned long)(mesg->slot[u].size));
    }

    FUNC_LEAVE_NOAPI(SUCCEED);
}
