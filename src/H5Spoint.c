/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Tuesday, June 16, 1998
 *
 * Purpose:	Point selection data space I/O functions.
 */
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5MMprivate.h>
#include <H5Sprivate.h>
#include <H5Vprivate.h>
#include <H5Dprivate.h>

/* Interface initialization */
#define PABLO_MASK      H5S_point_mask
#define INTERFACE_INIT  NULL
static intn             interface_initialize_g = FALSE;

static herr_t H5S_point_init (const struct H5O_layout_t *layout,
			      const H5S_t *space, H5S_sel_iter_t *iter);
static size_t H5S_point_favail (const H5S_t *space, const H5S_sel_iter_t *iter,
				size_t max);
static size_t H5S_point_fgath (H5F_t *f, const struct H5O_layout_t *layout,
			       const struct H5O_pline_t *pline,
			       const struct H5O_fill_t *fill,
			       const struct H5O_efl_t *efl, size_t elmt_size,
			       const H5S_t *file_space,
			       H5S_sel_iter_t *file_iter, size_t nelmts,
			       const H5D_xfer_t *xfer_parms,
			       void *buf/*out*/);
static herr_t H5S_point_fscat (H5F_t *f, const struct H5O_layout_t *layout,
			       const struct H5O_pline_t *pline,
			       const struct H5O_fill_t *fill,
			       const struct H5O_efl_t *efl, size_t elmt_size,
			       const H5S_t *file_space,
			       H5S_sel_iter_t *file_iter, size_t nelmts,
			       const H5D_xfer_t *xfer_parms,
			       const void *buf);
static size_t H5S_point_mgath (const void *_buf, size_t elmt_size,
			       const H5S_t *mem_space,
			       H5S_sel_iter_t *mem_iter, size_t nelmts,
			       void *_tconv_buf/*out*/);
static herr_t H5S_point_mscat (const void *_tconv_buf, size_t elmt_size,
			       const H5S_t *mem_space,
			       H5S_sel_iter_t *mem_iter, size_t nelmts,
			       void *_buf/*out*/);

const H5S_fconv_t	H5S_POINT_FCONV[1] = {{
    "point", 				/*name				*/
    H5S_SEL_POINTS,			/*selection type		*/
    H5S_point_init,			/*initialize			*/
    H5S_point_favail,			/*available			*/
    H5S_point_fgath,			/*gather			*/
    H5S_point_fscat,			/*scatter			*/
}};

const H5S_mconv_t	H5S_POINT_MCONV[1] = {{
    "point",				/*name				*/
    H5S_SEL_POINTS,			/*selection type		*/
    H5S_point_init,			/*initialize			*/
    H5S_point_mgath,			/*gather			*/
    H5S_point_mscat,			/*scatter			*/
}};

					      

/*-------------------------------------------------------------------------
 * Function:	H5S_point_init
 *
 * Purpose:	Initializes iteration information for point selection.
 *
 * Return:	non-negative on success, negative on failure.
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5S_point_init (const struct H5O_layout_t __unused__ *layout,
		const H5S_t *space, H5S_sel_iter_t *sel_iter)
{
    FUNC_ENTER (H5S_point_init, FAIL);

    /* Check args */
    assert (layout);
    assert (space && H5S_SEL_POINTS==space->select.type);
    assert (sel_iter);

#ifdef QAK
    printf("%s: check 1.0\n",FUNC);
#endif /* QAK */
    /* Initialize the number of points to iterate over */
    sel_iter->pnt.elmt_left=space->select.num_elem;

    /* Start at the head of the list of points */
    sel_iter->pnt.curr=space->select.sel_info.pnt_lst->head;
    
    FUNC_LEAVE (SUCCEED);
}


/*--------------------------------------------------------------------------
 NAME
    H5S_point_add
 PURPOSE
    Add a series of elements to a point selection
 USAGE
    herr_t H5S_point_add(space, num_elem, coord)
        H5S_t *space;           IN: Dataspace of selection to modify
        size_t num_elem;        IN: Number of elements in COORD array.
        const hssize_t *coord[];    IN: The location of each element selected
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    This function adds elements to the current point selection for a dataspace
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5S_point_add (H5S_t *space, size_t num_elem, const hssize_t **_coord)
{
    H5S_pnt_node_t *top, *curr, *new; /* Point selection nodes */
    const hssize_t *coord=(const hssize_t *)_coord;     /* Pointer to the actual coordinates */
    uintn i;                 /* Counter */
    herr_t ret_value=FAIL;  /* return value */

    FUNC_ENTER (H5S_point_add, FAIL);

    assert(space);
    assert(num_elem>0);
    assert(coord);

#ifdef QAK
    printf("%s: check 1.0\n",FUNC);
#endif /* QAK */
    top=curr=NULL;
    for(i=0; i<num_elem; i++) {
        /* Allocate space for the new node */
        if((new = H5MM_malloc(sizeof(H5S_pnt_node_t)))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "can't allocate point node");

#ifdef QAK
	printf("%s: check 1.1, rank=%d\n",
	       FUNC,(int)space->extent.u.simple.rank);
#endif /* QAK */
        if((new->pnt = H5MM_malloc(space->extent.u.simple.rank*sizeof(hssize_t)))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "can't allocate coordinate information");
#ifdef QAK
	printf("%s: check 1.2\n",FUNC);
#endif /* QAK */

        /* Copy over the coordinates */
        HDmemcpy(new->pnt,coord+(i*space->extent.u.simple.rank),(space->extent.u.simple.rank*sizeof(hssize_t)));
#ifdef QAK
	printf("%s: check 1.3\n",FUNC);
	{
	    intn j;

	    for(j=0; j<space->extent.u.simple.rank; j++) {
		printf("%s: pnt[%d]=%d\n",FUNC,(int)j,(int)new->pnt[j]);
		printf("%s: coord[%d][%d]=%d\n",
		       FUNC, (int)i, (int)j,
		       (int)*(coord+(i*space->extent.u.simple.rank)+j));
	    }
	}
#endif /* QAK */

        /* Link into list */
        new->next=NULL;
        if(top==NULL)
            top=new;
        else
            curr->next=new;
        curr=new;
    } /* end for */
#ifdef QAK
    printf("%s: check 2.0\n",FUNC);
#endif /* QAK */

    /* Append current list, if there is one */
    if(space->select.sel_info.pnt_lst->head!=NULL)
        curr->next=space->select.sel_info.pnt_lst->head;

    /* Put new list in point selection */
    space->select.sel_info.pnt_lst->head=top;

    /* Add the number of elements in the new selection */
    space->select.num_elem+=num_elem;

    ret_value=SUCCEED;
#ifdef QAK
    printf("%s: check 3.0\n",FUNC);
#endif /* QAK */
    
done:
    FUNC_LEAVE (ret_value);
}   /* H5S_point_add() */

/*-------------------------------------------------------------------------
 * Function:	H5S_point_favail
 *
 * Purpose:	Figure out the optimal number of elements to transfer to/from the file
 *
 * Return:	non-negative number of elements on success, negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5S_point_favail (const H5S_t __unused__ *space,
		  const H5S_sel_iter_t *sel_iter, size_t max)
{
    FUNC_ENTER (H5S_point_favail, FAIL);

    /* Check args */
    assert (space && H5S_SEL_POINTS==space->select.type);
    assert (sel_iter);

#ifdef QAK
    printf("%s: check 1.0, ret=%d\n",
	   FUNC,(int)MIN(sel_iter->pnt.elmt_left,max));
#endif /* QAK */
    FUNC_LEAVE (MIN(sel_iter->pnt.elmt_left,max));
}   /* H5S_point_favail() */

/*-------------------------------------------------------------------------
 * Function:	H5S_point_fgath
 *
 * Purpose:	Gathers data points from file F and accumulates them in the
 *		type conversion buffer BUF.  The LAYOUT argument describes
 *		how the data is stored on disk and EFL describes how the data
 *		is organized in external files.  ELMT_SIZE is the size in
 *		bytes of a datum which this function treats as opaque.
 *		FILE_SPACE describes the data space of the dataset on disk
 *		and the elements that have been selected for reading (via
 *		hyperslab, etc).  This function will copy at most NELMTS
 *		elements.
 *
 *  Notes: This could be optimized by gathering selected elements near (how
 *      near?) each other into one I/O request and then moving the correct
 *      elements into the return buffer
 *
 * Return:	Success:	Number of elements copied.
 *
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5S_point_fgath (H5F_t *f, const struct H5O_layout_t *layout,
		 const struct H5O_pline_t *pline,
		 const struct H5O_fill_t *fill, const struct H5O_efl_t *efl,
		 size_t elmt_size, const H5S_t *file_space,
		 H5S_sel_iter_t *file_iter, size_t nelmts,
		 const H5D_xfer_t *xfer_parms, void *_buf/*out*/)
{
    hssize_t	file_offset[H5O_LAYOUT_NDIMS];	/*offset of slab in file*/
    hsize_t	hsize[H5O_LAYOUT_NDIMS];	/*size of hyperslab	*/
    hssize_t	zero[H5O_LAYOUT_NDIMS];		/*zero			*/
    uint8 *buf=(uint8 *)_buf;   /* Alias for pointer arithmetic */
    uintn   ndims;          /* Number of dimensions of dataset */
    intn	i;				/*counters		*/
    size_t  num_read;       /* number of elements read into buffer */

    FUNC_ENTER (H5S_point_fgath, 0);

    /* Check args */
    assert (f);
    assert (layout);
    assert (elmt_size>0);
    assert (file_space);
    assert (file_iter);
    assert (nelmts>0);
    assert (buf);

#ifdef QAK
    printf("%s: check 1.0\n",FUNC);
#endif /* QAK */
    ndims=file_space->extent.u.simple.rank;
    /* initialize hyperslab size and offset in memory buffer */
    for(i=0; i<(int)(ndims+1); i++) {
        hsize[i]=1;     /* hyperslab size is 1, except for last element */
        zero[i]=0;      /* memory offset is 0 */
    } /* end for */
    hsize[ndims] = elmt_size;

    /*
     * Walk though and request each element we need and put it into the
     * buffer.
     */
    num_read=0;
    while(num_read<nelmts) {
        if(file_iter->pnt.elmt_left>0) {
            /* Copy the location of the point to get */
            HDmemcpy(file_offset, file_iter->pnt.curr->pnt,
		     ndims*sizeof(hssize_t));
            file_offset[ndims] = 0;

            /* Add in the offset */
            for(i=0; i<file_space->extent.u.simple.rank; i++)
                file_offset[i] += file_space->select.offset[i];

            /* Go read the point */
            if (H5F_arr_read (f, xfer_parms, layout, pline, fill, efl, hsize,
			      hsize, zero, file_offset, buf/*out*/)<0) {
                HRETURN_ERROR (H5E_DATASPACE, H5E_READERROR, 0, "read error");
            }

#ifdef QAK
	    printf("%s: check 3.0\n",FUNC);
	    {
		for(i=0; i<ndims; i++) {
		    printf("%s: %d - pnt=%d\n",
			   FUNC, (int)i, (int)file_iter->pnt.curr->pnt[i]);
		    printf("%s: %d - file_offset=%d\n",
			   FUNC, (int)i, (int)file_offset[i]);
		}
		printf("%s: *buf=%u\n",FUNC,(unsigned)*buf);
	    }
#endif /* QAK */
            /* Increment the offset of the buffer */
            buf+=elmt_size;

            /* Increment the count read */
            num_read++;

            /* Advance the point iterator */
            file_iter->pnt.elmt_left--;
            file_iter->pnt.curr=file_iter->pnt.curr->next;
        } else {
            break;      /* out of elements in the selection */
        } /* end else */
    } /* end while */
    
    FUNC_LEAVE (num_read);
} /* H5S_point_fgath() */

/*-------------------------------------------------------------------------
 * Function:	H5S_point_fscat
 *
 * Purpose:	Scatters dataset elements from the type conversion buffer BUF
 *		to the file F where the data points are arranged according to
 *		the file data space FILE_SPACE and stored according to
 *		LAYOUT and EFL. Each element is ELMT_SIZE bytes.
 *		The caller is requesting that NELMTS elements are copied.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5S_point_fscat (H5F_t *f, const struct H5O_layout_t *layout,
		 const struct H5O_pline_t *pline,
		 const struct H5O_fill_t *fill, const struct H5O_efl_t *efl,
		 size_t elmt_size, const H5S_t *file_space,
		 H5S_sel_iter_t *file_iter, size_t nelmts,
		 const H5D_xfer_t *xfer_parms, const void *_buf)
{
    hssize_t	file_offset[H5O_LAYOUT_NDIMS];	/*offset of hyperslab	*/
    hsize_t	hsize[H5O_LAYOUT_NDIMS];	/*size of hyperslab	*/
    hssize_t	zero[H5O_LAYOUT_NDIMS];		/*zero vector		*/
    const uint8 *buf=(const uint8 *)_buf;   /* Alias for pointer arithmetic */
    uintn   ndims;          /* Number of dimensions of dataset */
    intn	i;				/*counters		*/
    size_t  num_written;    /* number of elements written from buffer */

    FUNC_ENTER (H5S_point_fscat, FAIL);

    /* Check args */
    assert (f);
    assert (layout);
    assert (elmt_size>0);
    assert (file_space);
    assert (file_iter);
    assert (nelmts>0);
    assert (buf);

#ifdef QAK
    printf("%s: check 1.0, layout->ndims=%d\n",FUNC,(int)layout->ndims);
#endif /* QAK */
    ndims=file_space->extent.u.simple.rank;
    /* initialize hyperslab size and offset in memory buffer */
    for(i=0; i<(int)(ndims+1); i++) {
        hsize[i]=1;     /* hyperslab size is 1, except for last element */
        zero[i]=0;      /* memory offset is 0 */
    } /* end for */
    hsize[ndims] = elmt_size;

    /*
     * Walk though and request each element we need and put it into the
     * buffer.
     */
    num_written=0;
    while(num_written<nelmts && file_iter->pnt.elmt_left>0) {
#ifdef QAK
	printf("%s: check 2.0\n",FUNC);
	{
	    for(i=0; i<ndims; i++) {
		printf("%s: %d - pnt=%d\n",
		       FUNC, (int)i, (int)file_iter->pnt.curr->pnt[i]);
	    }
	}
#endif /* QAK */
        /* Copy the location of the point to get */
        HDmemcpy(file_offset,file_iter->pnt.curr->pnt,ndims*sizeof(hssize_t));
        file_offset[ndims] = 0;

        /* Add in the offset, if there is one */
        for(i=0; i<file_space->extent.u.simple.rank; i++)
            file_offset[i] += file_space->select.offset[i];

#ifdef QAK
	printf("%s: check 3.0\n",FUNC);
	{
	    for(i=0; i<ndims; i++) {
		printf("%s: %d - pnt=%d\n",
		       FUNC,(int)i,(int)file_iter->pnt.curr->pnt[i]);
		printf("%s: %d - file_offset=%d\n",
		       FUNC,(int)i,(int)file_offset[i]);
	    }
	    printf("%s: *buf=%u\n",FUNC,(unsigned)*buf);
	}
#endif /* QAK */
        /* Go write the point */
        if (H5F_arr_write (f, xfer_parms, layout, pline, fill, efl, hsize,
			   hsize, zero, file_offset, buf)<0) {
            HRETURN_ERROR (H5E_DATASPACE, H5E_WRITEERROR, 0, "write error");
        }

        /* Increment the offset of the buffer */
        buf+=elmt_size;

        /* Increment the count read */
        num_written++;

        /* Advance the point iterator */
        file_iter->pnt.elmt_left--;
        file_iter->pnt.curr=file_iter->pnt.curr->next;
#ifdef QAK
	printf("%s: check 5.0, file_iter->pnt.curr=%p\n",
	       FUNC,file_iter->pnt.curr);
#endif
    } /* end while */

    FUNC_LEAVE (num_written);
}   /* H5S_point_fscat() */

/*-------------------------------------------------------------------------
 * Function:	H5S_point_mgath
 *
 * Purpose:	Gathers dataset elements from application memory BUF and
 *		copies them into the data type conversion buffer TCONV_BUF.
 *		Each element is ELMT_SIZE bytes and arranged in application
 *		memory according to MEM_SPACE.  
 *		The caller is requesting that at most NELMTS be gathered.
 *
 * Return:	Success:	Number of elements copied.
 *
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5S_point_mgath (const void *_buf, size_t elmt_size,
		 const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
		 size_t nelmts, void *_tconv_buf/*out*/)
{
    hsize_t	mem_size[H5O_LAYOUT_NDIMS];	/*total size of app buf	*/
    const uint8 *buf=(const uint8 *)_buf;   /* Get local copies for address arithmetic */
    uint8 *tconv_buf=(uint8 *)_tconv_buf;
    hsize_t	acc;				/* coordinate accumulator */
    hsize_t	off;				/* coordinate offset */
    intn	space_ndims;			/*dimensionality of space*/
    intn	i;				/*counters		*/
    size_t num_gath;        /* number of elements gathered */

    FUNC_ENTER (H5S_point_mgath, 0);

    /* Check args */
    assert (buf);
    assert (elmt_size>0);
    assert (mem_space && H5S_SEL_POINTS==mem_space->select.type);
    assert (nelmts>0);
    assert (tconv_buf);

#ifdef QAK
    printf("%s: check 1.0\n",FUNC);
#endif /* QAK */
    if ((space_ndims=H5S_get_simple_extent_dims (mem_space, mem_size, NULL))<0) {
        HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, 0,
		       "unable to retrieve data space dimensions");
    }

    for(num_gath=0; num_gath<nelmts; num_gath++) {
        if(mem_iter->pnt.elmt_left>0) {
            /* Compute the location of the point to get */
            for(i=space_ndims-1,acc=1,off=0; i>=0; i--) {
                off+=(mem_iter->pnt.curr->pnt[i]+mem_space->select.offset[i])*acc;
                acc*=mem_size[i];
            } /* end for */

#ifdef QAK
	    printf("%s: check 2.0, acc=%d, off=%d\n",FUNC,(int)acc,(int)off);
#endif /* QAK */
            /* Copy the elements into the type conversion buffer */
            HDmemcpy(tconv_buf,buf+off,elmt_size);

            /* Increment the offset of the buffers */
            tconv_buf+=elmt_size;

            /* Advance the point iterator */
            mem_iter->pnt.elmt_left--;
            mem_iter->pnt.curr=mem_iter->pnt.curr->next;
        } else {
            break;      /* out of elements in the selection */
        } /* end else */
    } /* end for */

    FUNC_LEAVE (num_gath);
}   /* H5S_point_mgath() */

/*-------------------------------------------------------------------------
 * Function:	H5S_point_mscat
 *
 * Purpose:	Scatters NELMTS data points from the type conversion buffer
 *		TCONV_BUF to the application buffer BUF.  Each element is
 *		ELMT_SIZE bytes and they are organized in application memory
 *		according to MEM_SPACE.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, June 17, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5S_point_mscat (const void *_tconv_buf, size_t elmt_size,
		 const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
		 size_t nelmts, void *_buf/*out*/)
{
    hsize_t	mem_size[H5O_LAYOUT_NDIMS];	/*total size of app buf	*/
    uint8 *buf=(uint8 *)_buf;   /* Get local copies for address arithmetic */
    const uint8 *tconv_buf=(const uint8 *)_tconv_buf;
    hsize_t	acc;				/* coordinate accumulator */
    hsize_t	off;				/* coordinate offset */
    intn	space_ndims;		/*dimensionality of space*/
    intn	i;				/*counters		*/
    size_t num_scat;        /* Number of elements scattered */

    FUNC_ENTER (H5S_point_mscat, FAIL);

    /* Check args */
    assert (tconv_buf);
    assert (elmt_size>0);
    assert (mem_space && H5S_SEL_POINTS==mem_space->select.type);
    assert (nelmts>0);
    assert (buf);

#ifdef QAK
    printf("%s: check 1.0\n",FUNC);
#endif /* QAK */
    /*
     * Retrieve hyperslab information to determine what elements are being
     * selected (there might be other selection methods in the future).  We
     * only handle hyperslabs with unit sample because there's currently no
     * way to pass sample information to H5V_hyper_copy().
     */
    if ((space_ndims=H5S_get_simple_extent_dims (mem_space, mem_size, NULL))<0) {
        HRETURN_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL,
		       "unable to retrieve data space dimensions");
    }

    for(num_scat=0; num_scat<nelmts; num_scat++) {
        if(mem_iter->pnt.elmt_left>0) {
            /* Compute the location of the point to get */
            for(i=space_ndims-1,acc=1,off=0; i>=0; i--) {
                off+=(mem_iter->pnt.curr->pnt[i]+mem_space->select.offset[i])*acc;
                acc*=mem_size[i];
            } /* end for */

            /* Copy the elements into the type conversion buffer */
            HDmemcpy(buf+off,tconv_buf,elmt_size);

            /* Increment the offset of the buffers */
            tconv_buf+=elmt_size;

            /* Advance the point iterator */
            mem_iter->pnt.elmt_left--;
            mem_iter->pnt.curr=mem_iter->pnt.curr->next;
        } else {
            break;      /* out of elements in the selection */
        } /* end else */
    } /* end for */

    FUNC_LEAVE (SUCCEED);
}   /* H5S_point_mscat() */

/*--------------------------------------------------------------------------
 NAME
    H5S_point_release
 PURPOSE
    Release point selection information for a dataspace
 USAGE
    herr_t H5S_point_release(space)
        H5S_t *space;       IN: Pointer to dataspace
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Releases all point selection information for a dataspace
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_point_release (H5S_t *space)
{
    H5S_pnt_node_t *curr, *next;        /* Point selection nodes */
    FUNC_ENTER (H5S_point_release, FAIL);

    /* Check args */
    assert (space);

    /* Delete all the nodes from the list */
    curr=space->select.sel_info.pnt_lst->head;
    while(curr!=NULL) {
        next=curr->next;
        H5MM_xfree(curr->pnt);
        H5MM_xfree(curr);
        curr=next;
    } /* end while */
    
    /* Free & reset the point list header */
    H5MM_xfree(space->select.sel_info.pnt_lst);
    space->select.sel_info.pnt_lst=NULL;

    /* Reset the number of elements in the selection */
    space->select.num_elem=0;
    
    FUNC_LEAVE (SUCCEED);
}   /* H5S_point_release() */

/*--------------------------------------------------------------------------
 NAME
    H5S_point_npoints
 PURPOSE
    Compute number of elements in current selection
 USAGE
    hsize_t H5S_point_npoints(space)
        H5S_t *space;       IN: Pointer to dataspace
 RETURNS
    The number of elements in selection on success, 0 on failure
 DESCRIPTION
    Compute number of elements in current selection.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hsize_t
H5S_point_npoints (const H5S_t *space)
{
    FUNC_ENTER (H5S_point_npoints, 0);

    /* Check args */
    assert (space);

#ifdef QAK
    printf("%s: check 1.0, nelmts=%d\n",FUNC,(int)space->select.num_elem);
#endif /* QAK */
    FUNC_LEAVE (space->select.num_elem);
}   /* H5S_point_npoints() */

/*--------------------------------------------------------------------------
 NAME
    H5S_point_copy
 PURPOSE
    Copy a selection from one dataspace to another
 USAGE
    herr_t H5S_point_copy(dst, src)
        H5S_t *dst;  OUT: Pointer to the destination dataspace
        H5S_t *src;  IN: Pointer to the source dataspace
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Copies all the point selection information from the source
    dataspace to the destination dataspace.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_point_copy (H5S_t *dst, const H5S_t *src)
{
    H5S_pnt_node_t *curr, *new, *new_head;    /* Point information nodes */
    herr_t ret_value=SUCCEED;  /* return value */

    FUNC_ENTER (H5S_point_copy, FAIL);

    assert(src);
    assert(dst);

#ifdef QAK
 printf("%s: check 1.0\n",FUNC);
#endif /* QAK */
    /* Allocate room for the head of the point list */
    if((dst->select.sel_info.pnt_lst=H5MM_malloc(sizeof(H5S_pnt_list_t)))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
            "can't allocate point node");

    curr=src->select.sel_info.pnt_lst->head;
    new_head=NULL;
    while(curr!=NULL) {
        /* Create each point */
        if((new=H5MM_malloc(sizeof(H5S_pnt_node_t)))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "can't allocate point node");
        if((new->pnt = H5MM_malloc(src->extent.u.simple.rank*sizeof(hssize_t)))==NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                "can't allocate coordinate information");
        HDmemcpy(new->pnt,curr->pnt,(src->extent.u.simple.rank*sizeof(hssize_t)));
        new->next=NULL;

#ifdef QAK
 printf("%s: check 5.0\n",FUNC);
    {
        intn i;
        for(i=0; i<src->extent.u.simple.rank; i++)
            printf("%s: check 5.1, new->pnt[%d]=%d\n",FUNC,i,(int)new->pnt[i]);
    }
#endif /* QAK */

        /* Keep the order the same when copying */
        if(new_head==NULL)
            new_head=dst->select.sel_info.pnt_lst->head=new;
        else {
            new_head->next=new;
            new_head=new;
        } /* end else */

        curr=curr->next;
    } /* end while */
#ifdef QAK
 printf("%s: check 10.0 src->select.sel_info.pnt_lst=%p, dst->select.sel_info.pnt_lst=%p\n",FUNC,src->select.sel_info.pnt_lst,dst->select.sel_info.pnt_lst);
 printf("%s: check 10.0 src->select.sel_info.pnt_lst->head=%p, dst->select.sel_info.pnt_lst->head=%p\n",FUNC,src->select.sel_info.pnt_lst->head,dst->select.sel_info.pnt_lst->head);
#endif /* QAK */

done:
    FUNC_LEAVE (ret_value);
} /* end H5S_point_copy() */

/*--------------------------------------------------------------------------
 NAME
    H5S_point_select_valid
 PURPOSE
    Check whether the selection fits within the extent, with the current
    offset defined.
 USAGE
    hbool_t H5S_point_select_valid(space);
        H5S_t *space;             IN: Dataspace pointer to query
 RETURNS
    TRUE if the selection fits within the extent, FALSE if it does not and
        Negative on an error.
 DESCRIPTION
    Determines if the current selection at the current offet fits within the
    extent for the dataspace.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hbool_t
H5S_point_select_valid (const H5S_t *space)
{
    H5S_pnt_node_t *curr;       /* Point information nodes */
    intn i;                     /* Counter */
    hbool_t ret_value=TRUE;     /* return value */

    FUNC_ENTER (H5S_point_select_valid, FAIL);

    assert(space);

    /* Check each point to determine whether selection+offset is within extent */
    curr=space->select.sel_info.pnt_lst->head;
    while(curr!=NULL) {
        /* Check each dimension */
        for(i=0; i<space->extent.u.simple.rank; i++) {
            /* Check if an offset has been defined */
            /* Bounds check the selected point + offset against the extent */
            if(((curr->pnt[i]+space->select.offset[i])>(hssize_t)space->extent.u.simple.size[i])
                    || ((curr->pnt[i]+space->select.offset[i])<0)) {
                ret_value=FALSE;
                break;
            } /* end if */
        } /* end for */

        curr=curr->next;
    } /* end while */

    FUNC_LEAVE (ret_value);
} /* end H5S_point_select_valid() */

