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

/*-------------------------------------------------------------------------
 *
 * Created:		H5MM.c
 *			Jul 10 1997
 *			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		Memory management functions.
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */


#include "H5private.h"
#include "H5Dprivate.h"
#include "H5Eprivate.h"
#include "H5Iprivate.h"
#include "H5MMprivate.h"
#include "H5Pprivate.h"

#ifndef NDEBUG

/*-------------------------------------------------------------------------
 * Function:	H5MM_malloc
 *
 * Purpose:	Just like the POSIX version of malloc(3). This routine
 *		specifically checks for allocations of 0 bytes and fails
 *              in that case.  This routine is not called when NDEBUG is
 *		defined.
 *
 * Return:	Success:	Ptr to new memory
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Nov  8 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *
H5MM_malloc(size_t size)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5MM_malloc);

    assert(size);

    FUNC_LEAVE_NOAPI(HDmalloc(size));
} /* end H5MM_malloc() */


/*-------------------------------------------------------------------------
 * Function:	H5MM_calloc
 *
 * Purpose:	Similar to the POSIX version of calloc(3), except this routine
 *              just takes a 'size' parameter. This routine
 *		specifically checks for allocations of 0 bytes and fails
 *              in that case.  This routine is not called when NDEBUG is
 *		defined.
 *
 * Return:	Success:	Ptr to new memory
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Nov  8 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *
H5MM_calloc(size_t size)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5MM_calloc);

    assert(size);

    FUNC_LEAVE_NOAPI(HDcalloc(1,size));
} /* end H5MM_calloc() */
#endif /* NDEBUG */


/*-------------------------------------------------------------------------
 * Function:	H5MM_realloc
 *
 * Purpose:	Just like the POSIX version of realloc(3). Specifically, the
 *		following calls are equivalent
 *
 *		H5MM_realloc (NULL, size) <==> H5MM_malloc (size)
 *		H5MM_realloc (ptr, 0)	  <==> H5MM_xfree (ptr)
 *		H5MM_realloc (NULL, 0)	  <==> NULL
 *
 * Return:	Success:	Ptr to new memory or NULL if the memory
 *				was freed or HDrealloc couldn't allocate
 *				memory.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 10 1997
 *
 *-------------------------------------------------------------------------
 */
void *
H5MM_realloc(void *mem, size_t size)
{
    void *ret_value;

    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5MM_realloc)

    if(NULL == mem) {
	if(0 == size)
            ret_value = NULL;
        else
            ret_value = H5MM_malloc(size);
    } /* end if */
    else if(0 == size)
	ret_value = H5MM_xfree(mem);
    else
	ret_value = HDrealloc(mem, size);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MM_realloc() */


/*-------------------------------------------------------------------------
 * Function:	H5MM_xstrdup
 *
 * Purpose:	Duplicates a string.  If the string to be duplicated is the
 *		null pointer, then return null.	 If the string to be duplicated
 *		is the empty string then return a new empty string.
 *
 * Return:	Success:	Ptr to a new string (or null if no string).
 *
 *		Failure:	abort()
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 10 1997
 *
 *-------------------------------------------------------------------------
 */
char *
H5MM_xstrdup(const char *s)
{
    char	*ret_value = NULL;

    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5MM_xstrdup)

    if(s) {
        ret_value = (char *)H5MM_malloc(HDstrlen(s) + 1);
        HDassert(ret_value);
        HDstrcpy(ret_value, s);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MM_xstrdup() */


/*-------------------------------------------------------------------------
 * Function:	H5MM_strdup
 *
 * Purpose:	Duplicates a string.  If the string to be duplicated is the
 *		null pointer, then return null.	 If the string to be duplicated
 *		is the empty string then return a new empty string.
 *
 * Return:	Success:	Ptr to a new string (or null if no string).
 *
 *		Failure:	abort()
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 10 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
char *
H5MM_strdup(const char *s)
{
    char	*ret_value;

    FUNC_ENTER_NOAPI(H5MM_strdup, NULL)

    if(!s)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "null string")
    if(NULL == (ret_value = (char *)H5MM_malloc(HDstrlen(s) + 1)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDstrcpy(ret_value, s);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MM_strdup() */


/*-------------------------------------------------------------------------
 * Function:	H5MM_xfree
 *
 * Purpose:	Just like free(3) except null pointers are allowed as
 *		arguments, and the return value (always NULL) can be
 *		assigned to the pointer whose memory was just freed:
 *
 *			thing = H5MM_xfree (thing);
 *
 * Return:	Success:	NULL
 *
 *		Failure:	never fails
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 10 1997
 *
 *-------------------------------------------------------------------------
 */
void *
H5MM_xfree(void *mem)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5MM_xfree);

    if(mem)
        HDfree(mem);

    FUNC_LEAVE_NOAPI(NULL);
} /* end H5MM_xfree() */


/*-------------------------------------------------------------------------
 * Function:    H5MM_aligned_malloc
 *
 * Purpose:     Allocate a block of memory of at least size bytes,
 *              following the alignment restrictions specified by the file
 *              driver in lf.  Marks the buffer as aligned on dxpl_id.
 *
 * Return:      Success:        Ptr to new memory
 *
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              Sep  21 2011
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *
H5MM_aligned_malloc(size_t size, H5FD_t *lf)
{
    void                    *ret_value;         /* Return value */

    FUNC_ENTER_NOAPI(H5MM_aligned_malloc, NULL);

    HDassert(size);
    HDassert(lf);
    HDassert(lf->feature_flags & H5FD_FEAT_ALIGNED_MEM);
    HDassert(lf->must_align);

    /* Allocate the memory block */
    if(0 != HDposix_memalign(&ret_value, lf->mboundary,
            (((size - 1) / lf->mbsize) + 1) * lf->mbsize))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5MM_aligned_malloc() */


/*-------------------------------------------------------------------------
 * Function:    H5MM_aligned_realloc
 *
 * Purpose:     Reallocate a block of memory of at least size bytes,
 *              following the alignment restrictions specified by the file
 *              driver in lf.  Marks the buffer as aligned on dxpl_id.
 *
 * Return:      Success:        Ptr to new memory
 *
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              Sep  21 2011
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *
H5MM_aligned_realloc(void *mem, size_t old_size, size_t new_size, H5FD_t *lf)
{
    void *ret_value;  /* Return value */

    FUNC_ENTER_NOAPI(H5MM_aligned_realloc, NULL);

    HDassert(lf);
    HDassert(lf->feature_flags & H5FD_FEAT_ALIGNED_MEM);
    HDassert(lf->must_align);

    if(NULL == mem) {
        if(0 == new_size)
            ret_value = NULL;
        else
            ret_value = H5MM_aligned_malloc(new_size, lf);
    } /* end if */
    else if(0 == new_size) {
        H5MM_free(mem);
        ret_value = NULL;
    } /* end if */
    else if((((new_size - 1) / lf->mbsize) + 1) * lf->mbsize
            == (((old_size - 1) / lf->mbsize) + 1) * lf->mbsize)
        /* New allocation is in the same memory block size, no need to
         * realloc */
        ret_value = mem;
    else {
        /* Reallocate a block of a different size */
        /* Should we call standard H5MM_realloc with aligned size if
         * new_size < old_size?  Is realloc guaranteed to preserve the
         * aligned property of the original buffer in this case?  -NAF */
        if(NULL == (ret_value = H5MM_aligned_malloc(new_size, lf)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
        HDmemcpy(ret_value, mem, MIN(old_size, new_size));
        H5MM_free(mem);
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5MM_aligned_realloc */


/*-------------------------------------------------------------------------
 * Function:    H5MMaligned_malloc
 *
 * Purpose:     Allocate a block of memory of at least size bytes,
 *              following the alignment restrictions for the file
 *              containing loc_id.  Marks the buffer as aligned on
 *              dxpl_id.  The buffer must be freed by H5MMaligned_free,
 *              using the same dxpl_id.
 *
 * Return:      Success:        Ptr to new memory
 *
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              Sep  29 2011
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *
H5MMaligned_malloc(size_t size, hid_t loc_id, hid_t dxpl_id)
{
    H5G_loc_t    loc;           /* Location of object in file */
    void        *buf;           /* Allocated buffer */
    void        *ret_value;     /* Return value */

    FUNC_ENTER_API(H5MMaligned_malloc, NULL);
    H5TRACE3("*x", "zii", size, loc_id, dxpl_id);

    /* Check args */
    if(size == 0)
        HGOTO_DONE(NULL)
    if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a location")
    if(H5P_DEFAULT == dxpl_id)
        dxpl_id = -1;
    else
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not xfer parms")

    /* Call the internal routine, but only if the file driver uses aligned
     * buffers */
    if(H5F_LF(loc.oloc->file)->must_align) {
        if(NULL == (buf = H5MM_aligned_malloc(size, H5F_LF(loc.oloc->file))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

        /* Mark the dxpl as aligned */
        if(H5P_DEFAULT != dxpl_id) {
            H5P_genplist_t          *dx_plist = NULL;   /* Data transer property list */
            H5D_aligned_mem_t       aligned_mem;        /* Aligned memory property */

            if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not xfer parms")

            /* Get the dataset transfer property list */
            if(NULL == (dx_plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a dataset creation property list")

            /* Mark on the DXPL that the memory buffer is aligned */
            aligned_mem.aligned = TRUE;
            aligned_mem.buf = ret_value;
            aligned_mem.size = size;
            if(H5P_set(dx_plist, H5D_XFER_ALIGNED_MEM_NAME, &aligned_mem)
                    < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, NULL, "unable to set value")
        } /* end if */
    } /* end if */
    else
        if(NULL == (buf = H5MM_malloc(size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Set return value */
    ret_value = buf;

done:
    if(!ret_value)
        buf = H5MM_xfree(buf);

    FUNC_LEAVE_API(ret_value);
} /* end H5MMaligned_malloc() */


/*-------------------------------------------------------------------------
 * Function:    H5MMaligned_free
 *
 * Purpose:     Free a block of memory allocated with H5MMaligned_malloc.
 *              Removes the aligned buffer associated with dxpl_id.
 *
 * Return:      Success:        0
 *
 *              Failure:        Negative
 *
 * Programmer:  Neil Fortner
 *              Sep  29 2011
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MMaligned_free(void *mem, hid_t loc_id, hid_t dxpl_id)
{
    H5G_loc_t    loc;           /* Location of object in file */
    herr_t       ret_value = 0; /* Return value */

    FUNC_ENTER_API(H5MMaligned_free, FAIL);
    H5TRACE3("e", "*xii", mem, loc_id, dxpl_id);

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")

    /* Mark the dxpl as unaligned.  Do this first so if it fails mem is not
     * freed. */
    if(H5F_LF(loc.oloc->file)->must_align && H5P_DEFAULT != dxpl_id) {
        H5P_genplist_t          *dx_plist = NULL;   /* Data transer property list */
        H5D_aligned_mem_t       aligned_mem;        /* Aligned memory property */

        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms")

        /* Get the dataset transfer property list */
        if(NULL == (dx_plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list")

        /* Mark on the DXPL that the memory buffer is unaligned */
        aligned_mem.aligned = FALSE;
        aligned_mem.buf = NULL;
        aligned_mem.size = 0;
        if(H5P_set(dx_plist, H5D_XFER_ALIGNED_MEM_NAME, &aligned_mem)
                < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value")
    } /* end if */

    /* Free the memory */
    H5MM_free(mem);

done:
    FUNC_LEAVE_API(ret_value);
} /* end H5MMaligned_free() */

