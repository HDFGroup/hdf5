/*
 * Copyright (C) 1998-2001 NCSA
 *                         All rights reserved.
 *
 * Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Tuesday, June 16, 1998
 *
 * Purpose:	"All" selection data space I/O functions.
 */

#define H5S_PACKAGE		/*suppress error about including H5Spkg	  */

#include "H5private.h"
#include "H5Dprivate.h"
#include "H5Eprivate.h"
#include "H5Iprivate.h"
#include "H5Spkg.h"
#include "H5Tprivate.h"         /* Datatypes */
#include "H5Vprivate.h"

/* Interface initialization */
#define PABLO_MASK      H5Sall_mask
#define INTERFACE_INIT  NULL
static int             interface_initialize_g = 0;

static herr_t H5S_all_init (const H5S_t *space, size_t elmt_size, H5S_sel_iter_t *iter);
static hsize_t H5S_all_favail (const H5S_t *space, const H5S_sel_iter_t *iter,
			      hsize_t max);
static hsize_t H5S_all_fgath (H5F_t *f, const struct H5O_layout_t *layout,
			     const struct H5O_pline_t *pline,
			     const struct H5O_fill_t *fill,
			     const struct H5O_efl_t *efl, size_t elmt_size,
			     const H5S_t *file_space,
			     H5S_sel_iter_t *file_iter, hsize_t nelmts,
			     hid_t dxpl_id, void *buf/*out*/);
static herr_t H5S_all_fscat (H5F_t *f, const struct H5O_layout_t *layout,
			     const struct H5O_pline_t *pline,
			     const struct H5O_fill_t *fill,
			     const struct H5O_efl_t *efl, size_t elmt_size,
			     const H5S_t *file_space,
			     H5S_sel_iter_t *file_iter, hsize_t nelmts,
			     hid_t dxpl_id, const void *buf);
static hsize_t H5S_all_mgath (const void *_buf, size_t elmt_size,
			     const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
			     hsize_t nelmts, void *_tconv_buf/*out*/);
static herr_t H5S_all_mscat (const void *_tconv_buf, size_t elmt_size,
			     const H5S_t *mem_space, H5S_sel_iter_t *mem_iter,
			     hsize_t nelmts, void *_buf/*out*/);
static herr_t H5S_select_all(H5S_t *space);

const H5S_fconv_t	H5S_ALL_FCONV[1] = {{
    "all", 					/*name			*/
    H5S_SEL_ALL,				/*selection type	*/
    H5S_all_init,				/*initialize		*/
    H5S_all_favail,				/*available		*/
    H5S_all_fgath,				/*gather		*/
    H5S_all_fscat,				/*scatter		*/
}};

const H5S_mconv_t	H5S_ALL_MCONV[1] = {{
    "all", 					/*name			*/
    H5S_SEL_ALL,				/*selection type	*/
    H5S_all_init,				/*initialize		*/
    H5S_all_mgath,				/*gather		*/
    H5S_all_mscat, 				/*scatter		*/
}};


/*-------------------------------------------------------------------------
 * Function:	H5S_all_init
 *
 * Purpose:	Initializes iteration information for all selection.
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
H5S_all_init (const H5S_t *space, size_t UNUSED elmt_size, H5S_sel_iter_t *sel_iter)
{
    FUNC_ENTER (H5S_all_init, FAIL);

    /* Check args */
    assert (space && H5S_SEL_ALL==space->select.type);
    assert (sel_iter);

    /* Initialize the number of elements to iterate over */
    sel_iter->all.elmt_left=H5S_get_simple_extent_npoints(space);

    /* Start at the upper left location */
    sel_iter->all.offset=0;

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5S_all_favail
 *
 * Purpose:	Figure out the optimal number of elements to transfer to/from
 *		the file.
 *
 * Return:	non-negative number of elements on success, zero on
 *		failure.
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hsize_t
H5S_all_favail (const H5S_t * UNUSED space, const H5S_sel_iter_t *sel_iter, hsize_t max)
{
    FUNC_ENTER (H5S_all_favail, 0);

    /* Check args */
    assert (space && H5S_SEL_ALL==space->select.type);
    assert (sel_iter);

#ifdef QAK
    printf("%s: sel_iter->all.elmt_left=%u, max=%u\n",FUNC,(unsigned)sel_iter->all.elmt_left,(unsigned)max);
#endif /* QAK */
    FUNC_LEAVE (MIN(sel_iter->all.elmt_left,max));
}   /* H5S_all_favail() */


/*-------------------------------------------------------------------------
 * Function:	H5S_all_fgath
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
 * Return:	Success:	Number of elements copied.
 *
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June 16, 1998
 *
 * Modifications:
 *		Robb Matzke, 1999-08-03
 *		The data transfer properties are passed by ID since that's
 *		what the virtual file layer needs.
 *-------------------------------------------------------------------------
 */
static hsize_t
H5S_all_fgath (H5F_t *f, const struct H5O_layout_t *layout,
	       const struct H5O_pline_t *pline,
	       const struct H5O_fill_t *fill, const struct H5O_efl_t *efl,
	       size_t elmt_size, const H5S_t *file_space,
	       H5S_sel_iter_t *file_iter, hsize_t nelmts, hid_t dxpl_id,
	       void *buf/*out*/)
{
    hsize_t     actual_bytes; /* The actual number of bytes to read */
    hsize_t	buf_off;            /* Dataset offset for copying memory */

    FUNC_ENTER (H5S_all_fgath, 0);

    /* Check args */
    assert (f);
    assert (layout);
    assert (elmt_size>0);
    assert (file_space);
    assert (file_iter);
    assert (nelmts>0);
    assert (buf);

    /* Set the offset in the dataset and the number of bytes to read */
    buf_off=file_iter->all.offset*elmt_size;
    actual_bytes=elmt_size*nelmts;

    /*
     * Read piece from file.
     */
    H5_CHECK_OVERFLOW(actual_bytes,hsize_t,size_t);
    if (H5F_seq_read(f, dxpl_id, layout, pline, fill, efl, file_space,
            elmt_size, (size_t)actual_bytes, buf_off, buf/*out*/)<0) {
        HRETURN_ERROR(H5E_DATASPACE, H5E_READERROR, 0, "read error");
    }

    /* Advance iterator */
    file_iter->all.elmt_left-=nelmts;
    file_iter->all.offset+=nelmts;
    
    FUNC_LEAVE (nelmts);
} /* H5S_all_fgath() */


/*-------------------------------------------------------------------------
 * Function:	H5S_all_fscat
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
 *		Robb Matzke, 1999-08-03
 *		The data transfer properties are passed by ID since that's
 *		what the virtual file layer needs.
 *-------------------------------------------------------------------------
 */
static herr_t
H5S_all_fscat (H5F_t *f, const struct H5O_layout_t *layout,
	       const struct H5O_pline_t *pline, const struct H5O_fill_t *fill,
	       const struct H5O_efl_t *efl, size_t elmt_size,
	       const H5S_t *file_space, H5S_sel_iter_t *file_iter,
	       hsize_t nelmts, hid_t dxpl_id, const void *buf)
{
    hsize_t     actual_bytes;       /* The actual number of bytes to write */
    hsize_t	buf_off;            /* Dataset offset for copying memory */

    FUNC_ENTER (H5S_all_fscat, FAIL);

    /* Check args */
    assert (f);
    assert (layout);
    assert (elmt_size>0);
    assert (file_space);
    assert (file_iter);
    assert (nelmts>0);
    assert (buf);

    /* Set the offset in the dataset and the number of bytes to write */
    buf_off=file_iter->all.offset*elmt_size;
    actual_bytes=elmt_size*nelmts;

    /*
     * Write piece from file.
     */
    H5_CHECK_OVERFLOW(actual_bytes,hsize_t,size_t);
    if (H5F_seq_write(f, dxpl_id, layout, pline, fill, efl, file_space,
            elmt_size, (size_t)actual_bytes, buf_off, buf/*out*/)<0) {
        HRETURN_ERROR(H5E_DATASPACE, H5E_WRITEERROR, 0, "write error");
    }

    /* Advance iterator */
    file_iter->all.elmt_left-=nelmts;
    file_iter->all.offset+=nelmts;
    
    FUNC_LEAVE (SUCCEED);
}   /* H5S_all_fscat() */


/*-------------------------------------------------------------------------
 * Function:	H5S_all_mgath
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
static hsize_t
H5S_all_mgath (const void *_buf, size_t elmt_size,
	       const H5S_t UNUSED *mem_space, H5S_sel_iter_t *mem_iter,
	       hsize_t nelmts, void *tconv_buf/*out*/)
{
    const uint8_t *buf=(const uint8_t*)_buf;   /* Get local copies for address arithmetic */
    hsize_t      actual_bytes;       /* The actual number of bytes to read */

    FUNC_ENTER (H5S_all_mgath, 0);

    /* Check args */
    assert (buf);
    assert (elmt_size>0);
    assert (mem_space && H5S_SEL_ALL==mem_space->select.type);
    assert (mem_iter);
    assert (nelmts>0);
    assert (tconv_buf);

    /* Set the offset in the dataset and the number of bytes to read */
    buf += mem_iter->all.offset*elmt_size;
    actual_bytes=elmt_size*nelmts;

    /* "read" in the bytes from the source (buf) to the destination (tconv_buf) */
    H5_CHECK_OVERFLOW(actual_bytes,hsize_t,size_t);
    HDmemcpy(tconv_buf,buf,(size_t)actual_bytes);

    /* Advance iterator */
    mem_iter->all.elmt_left-=nelmts;
    mem_iter->all.offset+=nelmts;
    
    FUNC_LEAVE (nelmts);
}   /* H5S_all_mgath() */


/*-------------------------------------------------------------------------
 * Function:	H5S_all_mscat
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
H5S_all_mscat (const void *tconv_buf, size_t elmt_size,
	       const H5S_t UNUSED *mem_space, H5S_sel_iter_t *mem_iter,
	       hsize_t nelmts, void *_buf/*out*/)
{
    uint8_t *buf=(uint8_t *)_buf;
    hsize_t      actual_bytes;       /* The actual number of bytes to write */

    FUNC_ENTER (H5S_all_mscat, FAIL);

    /* Check args */
    assert (tconv_buf);
    assert (elmt_size>0);
    assert (mem_space && H5S_SEL_ALL==mem_space->select.type);
    assert (mem_iter);
    assert (nelmts>0);
    assert (buf);

    /* Set the offset in the dataset and the number of bytes to write */
    buf += mem_iter->all.offset*elmt_size;
    actual_bytes=elmt_size*nelmts;

    /* "write" the bytes from the source (tconv_buf) to the destination (buf) */
    H5_CHECK_OVERFLOW(actual_bytes,hsize_t,size_t);
    HDmemcpy(buf,tconv_buf,(size_t)actual_bytes);

    /* Advance iterator */
    mem_iter->all.elmt_left-=nelmts;
    mem_iter->all.offset+=nelmts;
    
    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5S_all_opt_possible
 *
 * Purpose:	Checks if an direct I/O transfer is possible between memory and
 *                  the file.
 *
 * Return:	Success:        Non-negative: TRUE or FALSE
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, April 3, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5S_all_opt_possible( const H5S_t *mem_space, const H5S_t *file_space, const unsigned UNUSED flags)
{
    htri_t c1,c2;               /* Flags whether a selection is optimizable */
    htri_t ret_value=TRUE;

    FUNC_ENTER(H5S_all_opt_possible, FAIL);

    /* Check args */
    assert(mem_space);
    assert(file_space);

    /* Check whether these are both simple dataspaces */
    if (H5S_SIMPLE!=mem_space->extent.type || H5S_SIMPLE!=file_space->extent.type)
        HGOTO_DONE(FALSE);

    /* Check whether both selections are single blocks */
    c1=H5S_select_single(file_space);
    c2=H5S_select_single(mem_space);
    if(c1==FAIL || c2==FAIL)
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "invalid check for single selection blocks");
    if(c1==FALSE || c2==FALSE)
        HGOTO_DONE(FALSE);

    /* Check whether the shape of each block is the same */
    c1=H5S_select_shape_same(mem_space,file_space);
    if(c1==FAIL)
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "invalid check for selection blocks same");
    if(c1==FALSE)
        HGOTO_DONE(FALSE);

done:
    FUNC_LEAVE(ret_value);
} /* H5S_all_opt_possible() */


/*-------------------------------------------------------------------------
 * Function:	H5S_all_read
 *
 * Purpose:	Reads directly from file into application memory if possible.
 *
 * Return:	Success:	Non-negative. If data was read directly then
 *				MUST_CONVERT is set to zero, otherwise
 *				MUST_CONVERT is set to non-zero.
 *
 *		Failure:	Negative. Return value of MUST_CONVERT is
 *				undefined.
 *
 * Programmer:	Robb Matzke
 *              Thursday, April 22, 1999
 *
 * Modifications:
 *    		Quincey Koziol, 1999-05-25
 *		Modified to allow contiguous hyperslabs to be written out.
 *
 *		Robb Matzke, 1999-08-03
 *		The data transfer properties are passed by ID since that's
 *		what the virtual file layer needs.
 *-------------------------------------------------------------------------
 */
herr_t
H5S_all_read(H5F_t *f, const H5O_layout_t *layout, const H5O_pline_t *pline,
             const struct H5O_fill_t *fill,
	     const H5O_efl_t *efl, size_t elmt_size, const H5S_t *file_space,
	     const H5S_t *mem_space, hid_t dxpl_id, void *_buf/*out*/)
{
    H5S_hyper_span_t *file_span=NULL,*mem_span=NULL;     /* Hyperslab span node */
    char       *buf=(char*)_buf;        /* Get pointer to buffer */
    hsize_t	file_elmts;             /* Number of elements in each dimension of selection */
    hssize_t	file_off,mem_off;       /* Offset (in elements) of selection */
    hsize_t	mem_size[H5O_LAYOUT_NDIMS];     /* Size of memory buffer */
    hsize_t	size[H5O_LAYOUT_NDIMS];         /* Size of selection */
    hssize_t	file_offset[H5O_LAYOUT_NDIMS];  /* Offset of selection in file */
    hssize_t	mem_offset[H5O_LAYOUT_NDIMS];   /* Offset of selection in memory */
    unsigned	u;                              /* Index variable */
    herr_t      ret_value=SUCCEED;

    FUNC_ENTER(H5S_all_read, FAIL);

#ifdef QAK
printf("%s: check 1.0\n",FUNC);
#endif /* QAK */
    /* Get information about memory and file */
    for (u=0; u<mem_space->extent.u.simple.rank; u++) {
        switch(mem_space->select.type) {
            case H5S_SEL_HYPERSLABS:
                /* Check for a "regular" hyperslab selection */
                if(mem_space->select.sel_info.hslab.diminfo != NULL) {
                    mem_off=mem_space->select.sel_info.hslab.diminfo[u].start;
                } /* end if */
                else {
                    mem_off=mem_span->low;
                    mem_span=mem_span->down->head;
                } /* end else */
                mem_off+=mem_space->select.offset[u];
                break;

            case H5S_SEL_ALL:
                mem_off=0;
                break;

            case H5S_SEL_POINTS:
                mem_off=mem_space->select.sel_info.pnt_lst->head->pnt[u]
                            +mem_space->select.offset[u];
                break;

            default:
                assert(0 && "Invalid selection type!");
        } /* end switch */

        switch(file_space->select.type) {
            case H5S_SEL_HYPERSLABS:
                /* Check for a "regular" hyperslab selection */
                if(file_space->select.sel_info.hslab.diminfo != NULL) {
                    file_elmts=file_space->select.sel_info.hslab.diminfo[u].block;
                    file_off=file_space->select.sel_info.hslab.diminfo[u].start;
                } /* end if */
                else {
                    file_elmts=(file_span->high-file_span->low)+1;
                    file_off=file_span->low;
                    file_span=file_span->down->head;
                } /* end else */
                file_off+=file_space->select.offset[u];
                break;

            case H5S_SEL_ALL:
                file_elmts=file_space->extent.u.simple.size[u];
                file_off=0;
                break;

            case H5S_SEL_POINTS:
                file_elmts=1;
                file_off=file_space->select.sel_info.pnt_lst->head->pnt[u]
                            +file_space->select.offset[u];
                break;

            default:
                assert(0 && "Invalid selection type!");
        } /* end switch */

        mem_size[u]=mem_space->extent.u.simple.size[u];
        size[u] = file_elmts;
        file_offset[u] = file_off;
        mem_offset[u] = mem_off;
    }
    mem_size[u]=elmt_size;
    size[u] = elmt_size;
    file_offset[u] = 0;
    mem_offset[u] = 0;

#ifdef QAK
printf("%s: check 2.0\n",FUNC);
for (u=0; u<mem_space->extent.u.simple.rank; u++)
    printf("size[%u]=%lu\n",u,(unsigned long)size[u]);
for (u=0; u<=mem_space->extent.u.simple.rank; u++)
    printf("mem_size[%u]=%lu\n",u,(unsigned long)mem_size[u]);
for (u=0; u<=mem_space->extent.u.simple.rank; u++)
    printf("mem_offset[%u]=%lu\n",u,(unsigned long)mem_offset[u]);
for (u=0; u<=mem_space->extent.u.simple.rank; u++)
    printf("file_offset[%u]=%lu\n",u,(unsigned long)file_offset[u]);
#endif /* QAK */
    /* Read data from the file */
    if (H5F_arr_read(f, dxpl_id, layout, pline, fill, efl, size,
            mem_size, mem_offset, file_offset, buf/*out*/)<0)
        HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "unable to read data from the file");

done:
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5S_all_write
 *
 * Purpose:	Writes data directly to the file if possible.
 *
 * Return:	Success:	Non-negative. If data was written directly
 *				then MUST_CONVERT is set to zero, otherwise
 *				MUST_CONVERT is set to non-zero.
 *
 *		Failure:	Negative. Return value of MUST_CONVERT is
 *				undefined.
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April 21, 1999
 *
 * Modifications:
 * 		Quincey Koziol, 1999-05-25
 *		Modified to allow contiguous hyperslabs to be written out.
 *
 *		Robb Matzke, 1999-08-03
 *		The data transfer properties are passed by ID since that's
 *		what the virtual file layer needs.
 *-------------------------------------------------------------------------
 */
herr_t
H5S_all_write(H5F_t *f, const struct H5O_layout_t *layout,
	     const H5O_pline_t *pline,
             const struct H5O_fill_t *fill,
             const H5O_efl_t *efl,
	     size_t elmt_size, const H5S_t *file_space,
	     const H5S_t *mem_space, hid_t dxpl_id, const void *_buf)
{
    H5S_hyper_span_t *file_span=NULL,*mem_span=NULL;     /* Hyperslab span node */
    const char *buf=(const char*)_buf;  /* Get pointer to buffer */
    hsize_t	file_elmts;   /* Number of elements in each dimension of selection */
    hssize_t	file_off,mem_off;       /* Offset (in elements) of selection */
    hsize_t	mem_size[H5O_LAYOUT_NDIMS];     /* Size of memory buffer */
    hsize_t	size[H5O_LAYOUT_NDIMS];         /* Size of selection */
    hssize_t	file_offset[H5O_LAYOUT_NDIMS];  /* Offset of selection in file */
    hssize_t	mem_offset[H5O_LAYOUT_NDIMS];   /* Offset of selection in memory */
    unsigned	u;                              /* Index variable */
    herr_t      ret_value=SUCCEED;
    
    FUNC_ENTER(H5S_all_write, FAIL);

#ifdef QAK
printf("%s: check 1.0\n",FUNC);
#endif /* QAK */

    /* Get information about memory and file */
    for (u=0; u<mem_space->extent.u.simple.rank; u++) {
        switch(mem_space->select.type) {
            case H5S_SEL_HYPERSLABS:
                /* Check for a "regular" hyperslab selection */
                if(mem_space->select.sel_info.hslab.diminfo != NULL) {
                    mem_off=mem_space->select.sel_info.hslab.diminfo[u].start;
                } /* end if */
                else {
                    mem_off=mem_span->low;
                    mem_span=mem_span->down->head;
                } /* end else */
                mem_off+=mem_space->select.offset[u];
                break;

            case H5S_SEL_ALL:
                mem_off=0;
                break;

            case H5S_SEL_POINTS:
                mem_off=mem_space->select.sel_info.pnt_lst->head->pnt[u]
                            +mem_space->select.offset[u];
                break;

            default:
                assert(0 && "Invalid selection type!");
        } /* end switch */

        switch(file_space->select.type) {
            case H5S_SEL_HYPERSLABS:
                /* Check for a "regular" hyperslab selection */
                if(file_space->select.sel_info.hslab.diminfo != NULL) {
                    file_elmts=file_space->select.sel_info.hslab.diminfo[u].block;
                    file_off=file_space->select.sel_info.hslab.diminfo[u].start;
                } /* end if */
                else {
                    file_elmts=(file_span->high-file_span->low)+1;
                    file_off=file_span->low;
                    file_span=file_span->down->head;
                } /* end else */
                file_off+=file_space->select.offset[u];
                break;

            case H5S_SEL_ALL:
                file_elmts=file_space->extent.u.simple.size[u];
                file_off=0;
                break;

            case H5S_SEL_POINTS:
                file_elmts=1;
                file_off=file_space->select.sel_info.pnt_lst->head->pnt[u]
                            +file_space->select.offset[u];
                break;

            default:
                assert(0 && "Invalid selection type!");
        } /* end switch */

        mem_size[u]=mem_space->extent.u.simple.size[u];
        size[u] = file_elmts;
        file_offset[u] = file_off;
        mem_offset[u] = mem_off;
    }
    mem_size[u]=elmt_size;
    size[u] = elmt_size;
    file_offset[u] = 0;
    mem_offset[u] = 0;

#ifdef QAK
printf("%s: check 2.0\n",FUNC);
for (u=0; u<mem_space->extent.u.simple.rank; u++)
    printf("size[%u]=%lu\n",u,(unsigned long)size[u]);
for (u=0; u<=mem_space->extent.u.simple.rank; u++)
    printf("mem_size[%u]=%lu\n",u,(unsigned long)mem_size[u]);
for (u=0; u<=mem_space->extent.u.simple.rank; u++)
    printf("mem_offset[%u]=%lu\n",u,(unsigned long)mem_offset[u]);
for (u=0; u<=mem_space->extent.u.simple.rank; u++)
    printf("file_offset[%u]=%lu\n",u,(unsigned long)file_offset[u]);
#endif /* QAK */
    /* Write data to the file */
    if (H5F_arr_write(f, dxpl_id, layout, pline, fill, efl, size,
            mem_size, mem_offset, file_offset, buf)<0)
        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "unable to write data to the file");

done:
    FUNC_LEAVE(ret_value);
}


/*--------------------------------------------------------------------------
 NAME
    H5S_all_release
 PURPOSE
    Release all selection information for a dataspace
 USAGE
    herr_t H5S_all_release(space)
        H5S_t *space;       IN: Pointer to dataspace
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Releases "all" selection information for a dataspace
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_all_release (H5S_t * UNUSED space)
{
    FUNC_ENTER (H5S_all_release, FAIL);

    /* Check args */
    assert (space);

    FUNC_LEAVE (SUCCEED);
}   /* H5S_all_release() */


/*--------------------------------------------------------------------------
 NAME
    H5S_all_npoints
 PURPOSE
    Compute number of elements in current selection
 USAGE
    hsize_t H5S_all_npoints(space)
        H5S_t *space;       IN: Pointer to dataspace
 RETURNS
    The number of elements in selection on success, 0 on failure
 DESCRIPTION
    Compute number of elements in current selection.  For "all" selections,
    this is the same as the number of points in the extent.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hsize_t
H5S_all_npoints (const H5S_t *space)
{
    unsigned u;     /* Counters */
    hsize_t ret_value;

    FUNC_ENTER (H5S_all_npoints, 0);

    /* Check args */
    assert (space);

    for(u=0, ret_value=1; u<space->extent.u.simple.rank; u++)
        ret_value*=space->extent.u.simple.size[u];
    
    FUNC_LEAVE (ret_value);
}   /* H5S_all_npoints() */


/*--------------------------------------------------------------------------
 NAME
    H5S_all_select_serialize
 PURPOSE
    Serialize the current selection into a user-provided buffer.
 USAGE
    herr_t H5S_all_select_serialize(space, buf)
        H5S_t *space;           IN: Dataspace pointer of selection to serialize
        uint8 *buf;             OUT: Buffer to put serialized selection into
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Serializes the current element selection into a buffer.  (Primarily for
    storing on disk).
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_all_select_serialize (const H5S_t *space, uint8_t *buf)
{
    herr_t ret_value=FAIL;  /* return value */

    FUNC_ENTER (H5S_all_select_serialize, FAIL);

    assert(space);

    /* Store the preamble information */
    UINT32ENCODE(buf, (uint32_t)space->select.type);  /* Store the type of selection */
    UINT32ENCODE(buf, (uint32_t)1);  /* Store the version number */
    UINT32ENCODE(buf, (uint32_t)0);  /* Store the un-used padding */
    UINT32ENCODE(buf, (uint32_t)0);  /* Store the additional information length */

    /* Set success */
    ret_value=SUCCEED;

    FUNC_LEAVE (ret_value);
}   /* H5S_all_select_serialize() */


/*--------------------------------------------------------------------------
 NAME
    H5S_all_select_deserialize
 PURPOSE
    Deserialize the current selection from a user-provided buffer.
 USAGE
    herr_t H5S_all_select_deserialize(space, buf)
        H5S_t *space;           IN/OUT: Dataspace pointer to place selection into
        uint8 *buf;             IN: Buffer to retrieve serialized selection from
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Deserializes the current selection into a buffer.  (Primarily for retrieving
    from disk).
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_all_select_deserialize (H5S_t *space, const uint8_t * UNUSED buf)
{
    herr_t ret_value=FAIL;  /* return value */

    FUNC_ENTER (H5S_all_select_deserialize, FAIL);

    assert(space);

    /* Change to "all" selection */
    if((ret_value=H5S_select_all(space))<0) {
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL, "can't change selection");
    } /* end if */

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_all_select_deserialize() */


/*--------------------------------------------------------------------------
 NAME
    H5S_all_bounds
 PURPOSE
    Gets the bounding box containing the selection.
 USAGE
    herr_t H5S_all_bounds(space, hsize_t *start, hsize_t *end)
        H5S_t *space;           IN: Dataspace pointer of selection to query
        hsize_t *start;         OUT: Starting coordinate of bounding box
        hsize_t *end;           OUT: Opposite coordinate of bounding box
 RETURNS
    Non-negative on success, negative on failure
 DESCRIPTION
    Retrieves the bounding box containing the current selection and places
    it into the user's buffers.  The start and end buffers must be large
    enough to hold the dataspace rank number of coordinates.  The bounding box
    exactly contains the selection, ie. if a 2-D element selection is currently
    defined with the following points: (4,5), (6,8) (10,7), the bounding box
    with be (4, 5), (10, 8).  Calling this function on a "none" selection
    returns fail.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_all_bounds(H5S_t *space, hsize_t *start, hsize_t *end)
{
    int rank;                  /* Dataspace rank */
    int i;                     /* index variable */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER (H5S_all_bounds, FAIL);

    assert(space);
    assert(start);
    assert(end);

    /* Get the dataspace extent rank */
    rank=space->extent.u.simple.rank;

    /* Just copy over the complete extent */
    for(i=0; i<rank; i++) {
        start[i]=0;
        end[i]=space->extent.u.simple.size[i]-1;
    } /* end for */

    FUNC_LEAVE (ret_value);
}   /* H5Sget_all_bounds() */


/*--------------------------------------------------------------------------
 NAME
    H5S_select_all
 PURPOSE
    Specify the the entire extent is selected
 USAGE
    herr_t H5S_select_all(dsid)
        hid_t dsid;             IN: Dataspace ID of selection to modify
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    This function selects the entire extent for a dataspace.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t H5S_select_all (H5S_t *space)
{
    herr_t ret_value=SUCCEED;  /* return value */

    FUNC_ENTER (H5S_select_all, FAIL);

    /* Check args */
    assert(space);

    /* Remove current selection first */
    if(H5S_select_release(space)<0) {
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL, "can't release selection");
    } /* end if */

    /* Set selection type */
    space->select.type=H5S_SEL_ALL;

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_select_all() */


/*--------------------------------------------------------------------------
 NAME
    H5Sselect_all
 PURPOSE
    Specify the the entire extent is selected
 USAGE
    herr_t H5Sselect_all(dsid)
        hid_t dsid;             IN: Dataspace ID of selection to modify
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    This function selects the entire extent for a dataspace.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t H5Sselect_all (hid_t spaceid)
{
    H5S_t	*space = NULL;  /* Dataspace to modify selection of */
    herr_t ret_value=SUCCEED;  /* return value */

    FUNC_ENTER (H5Sselect_all, FAIL);

    /* Check args */
    if (H5I_DATASPACE != H5I_get_type(spaceid) || NULL == (space=H5I_object(spaceid))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }

    /* Remove current selection first */
    if((ret_value=H5S_select_all(space))<0) {
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL, "can't change selection");
    } /* end if */

done:
    FUNC_LEAVE (ret_value);
}   /* H5Sselect_all() */


/*--------------------------------------------------------------------------
 NAME
    H5S_all_select_iterate
 PURPOSE
    Iterate over a "all" selection, calling a user's function for each
        element.
 USAGE
    herr_t H5S_all_select_iterate(buf, type_id, space, op, operator_data)
        void *buf;      IN/OUT: Buffer containing elements to iterate over
        hid_t type_id;  IN: Datatype ID of BUF array.
        H5S_t *space;   IN: Dataspace object containing selection to iterate over
        H5D_operator_t op; IN: Function pointer to the routine to be
                                called for each element in BUF iterated over.
        void *operator_data;    IN/OUT: Pointer to any user-defined data
                                associated with the operation.
 RETURNS
    Returns the return value of the last operator if it was non-zero, or zero
    if all elements were processed. Otherwise returns a negative value.
 DESCRIPTION
    Iterates over the selected elements in a memory buffer, calling the user's
    callback function for each element.  The selection in the dataspace is
    modified so that any elements already iterated over are removed from the
    selection if the iteration is interrupted (by the H5D_operator_t function
    returning non-zero) in the "middle" of the iteration and may be re-started
    by the user where it left off.

    NOTE: Until "subtracting" elements from a selection is implemented,
        the selection is not modified.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_all_select_iterate(void *buf, hid_t type_id, H5S_t *space, H5D_operator_t op,
        void *operator_data)
{
    hsize_t	mem_size[H5O_LAYOUT_NDIMS]; /* Dataspace size */
    hsize_t	mem_offset[H5O_LAYOUT_NDIMS]; /* current coordinates */
    hsize_t offset;             /* offset of region in buffer */
    hsize_t nelemts;            /* Number of elements to iterate through */
    void *tmp_buf;              /* temporary location of the element in the buffer */
    unsigned rank;              /* Dataspace rank */
    int indx;                   /* Index to increment */
    H5T_t *dt;                  /* Datatype structure */
    herr_t ret_value=0;         /* return value */

    FUNC_ENTER (H5S_all_select_iterate, 0);

    assert(buf);
    assert(space);
    assert(op);
    assert(H5I_DATATYPE == H5I_get_type(type_id));

    /* Get the dataspace extent rank */
    rank=space->extent.u.simple.rank;

    /* Set up the size of the memory space */
    HDmemcpy(mem_size, space->extent.u.simple.size, rank*sizeof(hsize_t));

    /* Set the size of the datatype */
    if (NULL==(dt=H5I_object(type_id)))
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an valid base datatype");
    mem_size[rank]=H5T_get_size(dt);

    /* Set the coordinates to zero */
    HDmemset(mem_offset, 0, (rank+1)*sizeof(hsize_t));

    /* Get the number of elements to iterate through */
    nelemts=H5S_get_simple_extent_npoints(space);

    /* Iterate through the entire dataset */
    while(nelemts>0 && ret_value==0) {
        /* Get the offset in the memory buffer */
        offset=H5V_array_offset(rank+1,mem_size,(const hssize_t *)mem_offset);
        tmp_buf=((char *)buf+offset);

        ret_value=(*op)(tmp_buf,type_id,(hsize_t)rank,(hssize_t *)mem_offset,operator_data);

        /* Decrement the number of elements to iterate through */
        nelemts--;

        /* Advance the coordinate (currently in C memory order) */
        indx=rank-1; /* Leave the byte offset in the element alone */
        while(indx>=0 && ++mem_offset[indx]==mem_size[indx]) {
            mem_offset[indx]=0;
            indx--;
          } /* end while */
      } /* end while */

    FUNC_LEAVE (ret_value);
}   /* H5S_all_select_iterate() */


/*--------------------------------------------------------------------------
 NAME
    H5S_all_select_fill
 PURPOSE
    Fill an "all" selection in memory with a value
 USAGE
    herr_t H5S_all_select_fill(fill,fill_size,space,buf)
        const void *fill;       IN: Pointer to fill value to use
        size_t fill_size;       IN: Size of elements in memory buffer & size of
                                    fill value
        H5S_t *space;           IN: Dataspace describing memory buffer &
                                    containing selection to use.
        void *buf;              IN/OUT: Memory buffer to fill selection in
 RETURNS
    Non-negative on success/Negative on failure.
 DESCRIPTION
    Use the selection in the dataspace to fill elements in a memory buffer.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    The memory buffer elements are assumed to have the same datatype as the
    fill value being placed into them.
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_all_select_fill(const void *fill, size_t fill_size, H5S_t *space, void *buf)
{
    hssize_t nelemts;          /* Number of elements in dataspace */
    herr_t ret_value=SUCCEED;  /* return value */

    FUNC_ENTER (H5S_all_select_fill, FAIL);

    /* Check args */
    assert(fill);
    assert(fill_size>0);
    assert(space);
    assert(buf);

    /* Fill the selection in the memory buffer */

    /* Get the number of elements to iterate through */
    if((nelemts=H5S_get_simple_extent_npoints(space))<0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOUNT, FAIL, "can't get number of elements");

    /* Fill the elements in the buffer */
    H5_CHECK_OVERFLOW(nelemts,hssize_t,size_t);
    H5V_array_fill(buf, fill, fill_size, (size_t)nelemts);

done:
    FUNC_LEAVE (ret_value);
}   /* H5S_all_select_fill() */

