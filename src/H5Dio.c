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

#define H5D_PACKAGE		/*suppress error about including H5Dpkg	  */

#include "H5private.h"		/* Generic Functions			*/
#include "H5Dpkg.h"		/* Dataset functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Sprivate.h"		/* Dataspace functions			*/
#include "H5Vprivate.h"		/* Vector and array functions		*/

/*#define H5D_DEBUG*/

/*
 * The MPIO, MPIPOSIX, & FPHDF5 drivers are needed because there are
 * file and places where we check for things that aren't handled by these
 * drivers.
 */
#include "H5FDfphdf5.h"
#include "H5FDmpio.h"
#include "H5FDmpiposix.h"

#ifdef H5_HAVE_PARALLEL
/* Remove this if H5R_DATASET_REGION is no longer used in this file */
#   include "H5Rpublic.h"
#endif /*H5_HAVE_PARALLEL*/

/* Pablo information */
#define PABLO_MASK	H5Dio_mask

/* Local typedefs */

/*information for mapping between file space and memory space*/
typedef struct fm_map {
    H5S_t **fspace;             /*file spaces for all chunks*/
    H5S_t **mspace;             /*memory spaces for all chunks*/
    hsize_t elmt_count;         /*indicate which element is being processed*/
    hsize_t nchunks;            /*total number of chunks*/
    H5S_sel_iter_t mem_iter;    /* Iterator for elements in memory selection */
    unsigned m_ndims;           /* Number of dimensions for memory dataspace */
    hsize_t chunks[H5O_LAYOUT_NDIMS];   /* Number of chunks in each dimension */
    hsize_t down_chunks[H5O_LAYOUT_NDIMS];      /* "down" size of number of chunks in each dimension */
    hssize_t *chunk_coords;     /*coordinates for all chunks*/
    H5O_layout_t *layout;       /*dataset layout information*/
} fm_map;

/* Interface initialization */
static int interface_initialize_g = 0;
#define INTERFACE_INIT NULL 

#ifdef H5_HAVE_PARALLEL
/* Global vars whose value can be set from environment variable also */
extern hbool_t H5S_mpi_opt_types_g;
#endif /* H5_HAVE_PARALLEL */

/* Local functions */
static herr_t H5D_read(H5D_t *dataset, const H5T_t *mem_type,
			const H5S_t *mem_space, const H5S_t *file_space,
			hid_t dset_xfer_plist, void *buf/*out*/);
static herr_t H5D_write(H5D_t *dataset, const H5T_t *mem_type,
			 const H5S_t *mem_space, const H5S_t *file_space,
			 hid_t dset_xfer_plist, const void *buf);
static herr_t 
H5D_contig_read(hsize_t nelmts, H5D_t *dataset, const H5T_t *mem_type, const H5S_t *mem_space,
            const H5S_t *file_space, H5T_path_t *tpath, H5S_conv_t *sconv, H5P_genplist_t *dc_plist, 
            H5P_genplist_t *dx_plist, hid_t dxpl_id, hbool_t doing_mpio, H5FD_mpio_xfer_t xfer_mode, 
            hid_t src_id, hid_t dst_id, void *buf/*out*/);
static herr_t
H5D_contig_write(hsize_t nelmts, H5D_t *dataset, const H5T_t *mem_type, const H5S_t *mem_space,
	    const H5S_t *file_space, H5T_path_t *tpath, H5S_conv_t *sconv, H5P_genplist_t *dc_plist,
            H5P_genplist_t *dx_plist, hid_t dxpl_id, hbool_t doing_mpio, H5FD_mpio_xfer_t xfer_mode, 
            hid_t src_id, hid_t dst_id, const void *buf);
static herr_t
H5D_chunk_read(hsize_t nelmts, H5D_t *dataset, const H5T_t *mem_type, const H5S_t *mem_space,
            const H5S_t *file_space, H5T_path_t *tpath, H5S_conv_t *sconv, H5P_genplist_t *dc_plist, 
            H5P_genplist_t *dx_plist, hid_t dxpl_id, hbool_t doing_mpio, H5FD_mpio_xfer_t xfer_mode, 
            hid_t src_id, hid_t dst_id, void *buf/*out*/);
static herr_t
H5D_chunk_write(hsize_t nelmts, H5D_t *dataset, const H5T_t *mem_type, const H5S_t *mem_space,
	    const H5S_t *file_space, H5T_path_t *tpath, H5S_conv_t *sconv, H5P_genplist_t *dc_plist,
            H5P_genplist_t *dx_plist, hid_t dxpl_id, hbool_t doing_mpio, H5FD_mpio_xfer_t xfer_mode, 
            hid_t src_id, hid_t dst_id, const void *buf);
#ifdef H5_HAVE_PARALLEL
static herr_t 
H5D_io_assist_mpio(H5P_genplist_t *dx_plist, hbool_t doing_mpio, H5FD_mpio_xfer_t xfer_mode, 
            hbool_t *xfer_mode_changed);
#endif /*H5_HAVE_PARALLEL*/
static herr_t
H5D_chunk_mem_file_map(H5D_t *dataset, const H5T_t *mem_type, const H5S_t *file_space, 
            const H5S_t *mem_space, fm_map *fm);
static herr_t H5D_chunk_coords_assist(hssize_t *coords, size_t ndims,
    hsize_t chunks[], hsize_t chunk_ptr);
static herr_t H5D_chunk_cb(void *elem, hid_t type_id, hsize_t ndims,
    hssize_t *coords, void *fm);
static herr_t H5D_fill(const void *fill, const H5T_t *fill_type, void *buf,
    const H5T_t *buf_type, const H5S_t *space, hid_t dxpl_id);


/* Declare a free list to manage blocks of single datatype element data */
H5FL_BLK_DEFINE(type_elem);

/* Declare a free list to manage blocks of type conversion data */
H5FL_BLK_DEFINE(type_conv);


/*--------------------------------------------------------------------------
 NAME
    H5Dfill
 PURPOSE
    Fill a selection in memory with a value
 USAGE
    herr_t H5Dfill(fill, fill_type, space, buf, buf_type)
        const void *fill;       IN: Pointer to fill value to use
        hid_t fill_type_id;     IN: Datatype of the fill value
        void *buf;              IN/OUT: Memory buffer to fill selection within
        hid_t buf_type_id;      IN: Datatype of the elements in buffer
        hid_t space_id;         IN: Dataspace describing memory buffer &
                                    containing selection to use.
 RETURNS
    Non-negative on success/Negative on failure.
 DESCRIPTION
    Use the selection in the dataspace to fill elements in a memory buffer.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    If "fill" parameter is NULL, use all zeros as fill value
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5Dfill(const void *fill, hid_t fill_type_id, void *buf, hid_t buf_type_id, hid_t space_id)
{
    H5S_t *space;               /* Dataspace */
    H5T_t *fill_type;           /* Fill-value datatype */
    H5T_t *buf_type;            /* Buffer datatype */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_API(H5Dfill, FAIL);
    H5TRACE5("e","xixii",fill,fill_type_id,buf,buf_type_id,space_id);

    /* Check args */
    if (buf==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid buffer");
    if (NULL == (space=H5I_object_verify(space_id, H5I_DATASPACE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a dataspace");
    if (NULL == (fill_type=H5I_object_verify(fill_type_id, H5I_DATATYPE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a datatype");
    if (NULL == (buf_type=H5I_object_verify(buf_type_id, H5I_DATATYPE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a datatype");

    /* Fill the selection in the memory buffer */
    if(H5D_fill(fill,fill_type,buf,buf_type,space, H5AC_dxpl_id)<0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTENCODE, FAIL, "filling selection failed");

done:
    FUNC_LEAVE_API(ret_value);
}   /* H5Dfill() */


/*--------------------------------------------------------------------------
 NAME
    H5D_fill
 PURPOSE
    Fill a selection in memory with a value (internal version)
 USAGE
    herr_t H5D_fill(fill, fill_type, buf, buf_type, space)
        const void *fill;       IN: Pointer to fill value to use
        H5T_t *fill_type;       IN: Datatype of the fill value
        void *buf;              IN/OUT: Memory buffer to fill selection within
        H5T_t *buf_type;        IN: Datatype of the elements in buffer
        H5S_t *space;           IN: Dataspace describing memory buffer &
                                    containing selection to use.
 RETURNS
    Non-negative on success/Negative on failure.
 DESCRIPTION
    Use the selection in the dataspace to fill elements in a memory buffer.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    If "fill" parameter is NULL, use all zeros as fill value.  If "fill_type"
    parameter is NULL, use "buf_type" for the fill value datatype.
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5D_fill(const void *fill, const H5T_t *fill_type, void *buf, const H5T_t *buf_type, const H5S_t *space, hid_t dxpl_id)
{
    H5T_path_t *tpath = NULL;   /* Conversion information*/
    uint8_t *tconv_buf = NULL;  /* Data type conv buffer */
    uint8_t *bkg_buf = NULL;    /* Temp conversion buffer */
    hid_t src_id = -1, dst_id = -1;     /* Temporary type IDs */
    size_t src_type_size;       /* Size of source type	*/
    size_t dst_type_size;       /* Size of destination type*/
    size_t buf_size;            /* Desired buffer size	*/
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOINIT(H5D_fill);

    /* Check args */
    assert(buf);
    assert(buf_type);
    assert(space);

    /* Check for "default" fill value */
    if(fill_type==NULL)
        fill_type=buf_type;

    /* Get the memory and file datatype sizes */
    src_type_size = H5T_get_size(fill_type);
    dst_type_size = H5T_get_size(buf_type);

    /* Get the maximum buffer size needed and allocate it */
    buf_size=MAX(src_type_size,dst_type_size);
    if (NULL==(tconv_buf = H5FL_BLK_MALLOC(type_elem,buf_size)) || NULL==(bkg_buf = H5FL_BLK_CALLOC(type_elem,buf_size)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* Copy the user's data into the buffer for conversion */
    if(fill==NULL)
        HDmemset(tconv_buf,0,src_type_size);
    else
        HDmemcpy(tconv_buf,fill,src_type_size);

    /* Convert memory buffer into disk buffer */
    /* Set up type conversion function */
    if (NULL == (tpath = H5T_path_find(fill_type, buf_type, NULL, NULL, dxpl_id))) {
        HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to convert between src and dest data types");
    } else if (!H5T_path_noop(tpath)) {
        if ((src_id = H5I_register(H5I_DATATYPE, H5T_copy(fill_type, H5T_COPY_ALL)))<0 ||
                (dst_id = H5I_register(H5I_DATATYPE, H5T_copy(buf_type, H5T_COPY_ALL)))<0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL, "unable to register types for conversion");
    }

    /* Perform data type conversion */
    if (H5T_convert(tpath, src_id, dst_id, (hsize_t)1, 0, 0, tconv_buf, bkg_buf, dxpl_id)<0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCONVERT, FAIL, "data type conversion failed");

    /* Fill the selection in the memory buffer */
    if(H5S_select_fill(tconv_buf, dst_type_size, space, buf)<0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTENCODE, FAIL, "filling selection failed");

done:
    if (tconv_buf)
        H5FL_BLK_FREE(type_elem,tconv_buf);
    if (bkg_buf)
        H5FL_BLK_FREE(type_elem,bkg_buf);
    FUNC_LEAVE_NOAPI(ret_value);
}   /* H5D_fill() */


/*-------------------------------------------------------------------------
 * Function:	H5Dread
 *
 * Purpose:	Reads (part of) a DSET from the file into application
 *		memory BUF. The part of the dataset to read is defined with
 *		MEM_SPACE_ID and FILE_SPACE_ID.	 The data points are
 *		converted from their file type to the MEM_TYPE_ID specified. 
 *		Additional miscellaneous data transfer properties can be
 *		passed to this function with the PLIST_ID argument.
 *
 *		The FILE_SPACE_ID can be the constant H5S_ALL which indicates
 *		that the entire file data space is to be referenced.
 *
 *		The MEM_SPACE_ID can be the constant H5S_ALL in which case
 *		the memory data space is the same as the file data space
 *		defined when the dataset was created.
 *
 *		The number of elements in the memory data space must match
 *		the number of elements in the file data space.
 *
 *		The PLIST_ID can be the constant H5P_DEFAULT in which
 *		case the default data transfer properties are used.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Errors:
 *		ARGS	  BADTYPE	Not a data space. 
 *		ARGS	  BADTYPE	Not a data type. 
 *		ARGS	  BADTYPE	Not a dataset. 
 *		ARGS	  BADTYPE	Not xfer parms. 
 *		ARGS	  BADVALUE	No output buffer. 
 *		DATASET	  READERROR	Can't read data. 
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dread(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
	hid_t file_space_id, hid_t plist_id, void *buf/*out*/)
{
    H5D_t		   *dset = NULL;
    const H5T_t		   *mem_type = NULL;
    const H5S_t		   *mem_space = NULL;
    const H5S_t		   *file_space = NULL;
    herr_t                  ret_value=SUCCEED;  /* Return value */

    FUNC_ENTER_API(H5Dread, FAIL);
    H5TRACE6("e","iiiiix",dset_id,mem_type_id,mem_space_id,file_space_id,
             plist_id,buf);

    /* check arguments */
    if (NULL == (dset = H5I_object_verify(dset_id, H5I_DATASET)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    if (NULL == dset->ent.file)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    if (NULL == (mem_type = H5I_object_verify(mem_type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    if (H5S_ALL != mem_space_id) {
	if (NULL == (mem_space = H5I_object_verify(mem_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");

	/* Check for valid selection */
	if(H5S_select_valid(mem_space)!=TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent");
    }
    if (H5S_ALL != file_space_id) {
	if (NULL == (file_space = H5I_object_verify(file_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");

	/* Check for valid selection */
	if(H5S_select_valid(file_space)!=TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent");
    }

    /* Get the default dataset transfer property list if the user didn't provide one */
    if (H5P_DEFAULT == plist_id)
        plist_id= H5P_DATASET_XFER_DEFAULT;
    else
        if (TRUE!=H5P_isa_class(plist_id,H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms");
    if (!buf)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer");

    /* read raw data */
    if (H5D_read(dset, mem_type, mem_space, file_space, plist_id, buf/*out*/) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data");

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Dwrite
 *
 * Purpose:	Writes (part of) a DSET from application memory BUF to the
 *		file.  The part of the dataset to write is defined with the
 *		MEM_SPACE_ID and FILE_SPACE_ID arguments. The data points
 *		are converted from their current type (MEM_TYPE_ID) to their
 *		file data type.	 Additional miscellaneous data transfer
 *		properties can be passed to this function with the
 *		PLIST_ID argument.
 *
 *		The FILE_SPACE_ID can be the constant H5S_ALL which indicates
 *		that the entire file data space is to be referenced.
 *
 *		The MEM_SPACE_ID can be the constant H5S_ALL in which case
 *		the memory data space is the same as the file data space
 *		defined when the dataset was created.
 *
 *		The number of elements in the memory data space must match
 *		the number of elements in the file data space.
 *
 *		The PLIST_ID can be the constant H5P_DEFAULT in which
 *		case the default data transfer properties are used.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Errors:
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dwrite(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
	 hid_t file_space_id, hid_t plist_id, const void *buf)
{
    H5D_t		   *dset = NULL;
    const H5T_t		   *mem_type = NULL;
    const H5S_t		   *mem_space = NULL;
    const H5S_t		   *file_space = NULL;
    herr_t                  ret_value=SUCCEED;  /* Return value */

    FUNC_ENTER_API(H5Dwrite, FAIL);
    H5TRACE6("e","iiiiix",dset_id,mem_type_id,mem_space_id,file_space_id,
             plist_id,buf);

    /* check arguments */
    if (NULL == (dset = H5I_object_verify(dset_id, H5I_DATASET)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    if (NULL == dset->ent.file)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    if (NULL == (mem_type = H5I_object_verify(mem_type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    if (H5S_ALL != mem_space_id) {
	if (NULL == (mem_space = H5I_object_verify(mem_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");

	/* Check for valid selection */
	if (H5S_select_valid(mem_space)!=TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent");
    }
    if (H5S_ALL != file_space_id) {
	if (NULL == (file_space = H5I_object_verify(file_space_id, H5I_DATASPACE)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");

	/* Check for valid selection */
	if (H5S_select_valid(file_space)!=TRUE)
	    HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection+offset not within extent");
    }

    /* Get the default dataset transfer property list if the user didn't provide one */
    if (H5P_DEFAULT == plist_id)
        plist_id= H5P_DATASET_XFER_DEFAULT;
    else
        if (TRUE!=H5P_isa_class(plist_id,H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms");
    if (!buf)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer");

    /* write raw data */
    if (H5D_write(dset, mem_type, mem_space, file_space, plist_id, buf) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data");

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5D_read
 *
 * Purpose:	Reads (part of) a DATASET into application memory BUF. See
 *		H5Dread() for complete details.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 *	Robb Matzke, 1998-06-09
 *	The data space is no longer cached in the dataset struct.
 *
 * 	Robb Matzke, 1998-08-11
 *	Added timing calls around all the data space I/O functions.
 *
 * 	rky, 1998-09-18
 *	Added must_convert to do non-optimized read when necessary.
 *
 *  	Quincey Koziol, 1999-07-02
 *	Changed xfer_parms parameter to xfer plist parameter, so it
 *	could be passed to H5T_convert.
 *
 *	Albert Cheng, 2000-11-21
 *	Added the code that when it detects it is not safe to process a
 *	COLLECTIVE read request without hanging, it changes it to
 *	INDEPENDENT calls.
 *
 *	Albert Cheng, 2000-11-27
 *	Changed to use the optimized MPIO transfer for Collective calls only.
 *
 *      Raymond Lu, 2001-10-2
 *      Changed the way to retrieve property for generic property list.
 *
 *	Raymond Lu, 2002-2-26
 *	For the new fill value design, data space can either be allocated 
 *	or not allocated at this stage.  Fill value or data from space is
 *	returned to outgoing buffer.
 *
 *      QAK - 2002/04/02
 *      Removed the must_convert parameter and move preconditions to
 *      H5S_<foo>_opt_possible() routine
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_read(H5D_t *dataset, const H5T_t *mem_type, const H5S_t *mem_space,
	 const H5S_t *file_space, hid_t dxpl_id, void *buf/*out*/)
{
    hssize_t	snelmts;                /*total number of elmts	(signed) */
    hsize_t	nelmts;                 /*total number of elmts	*/
    H5T_path_t	*tpath = NULL;		/*type conversion info	*/
    hid_t	src_id = -1, dst_id = -1;/*temporary type atoms */
    H5S_conv_t	*sconv=NULL;	        /*space conversion funcs*/
    H5FD_mpio_xfer_t xfer_mode=H5FD_MPIO_INDEPENDENT;	/*xfer_mode for this request */
    hbool_t	doing_mpio=0;		/*This is an MPIO access */
    H5P_genplist_t *dx_plist=NULL;      /* Data transfer property list */
    H5P_genplist_t *dc_plist;           /* Dataset creation roperty list */
    unsigned	sconv_flags=0;	        /* Flags for the space conversion */
    H5S_sel_type fsel_type;             /* Selection type on disk */
    H5S_sel_type msel_type;             /* Selection type in memory */
    herr_t	ret_value = SUCCEED;	/* Return value	*/

    FUNC_ENTER_NOINIT(H5D_read);

    /* check args */
    assert(dataset && dataset->ent.file);
    assert(mem_type);
    assert(buf);

    /* Get the dataset's creation property list */
    if (NULL == (dc_plist = H5I_object(dataset->dcpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list");

    /* Get the dataset transfer property list */
    if (NULL == (dx_plist = H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list");

    if (!file_space)
        file_space = dataset->space;
    if (!mem_space)
        mem_space = file_space;
    if((snelmts = H5S_get_select_npoints(mem_space))<0)
	HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "src dataspace has invalid selection");
    nelmts=snelmts;

#ifdef H5_HAVE_PARALLEL
    /* Collect Parallel I/O information for possible later use */
    if (H5FD_MPIO==H5P_peek_hid_t(dx_plist,H5D_XFER_VFL_ID_NAME)) {
	doing_mpio++;
        xfer_mode=H5P_peek_unsigned(dx_plist, H5D_XFER_IO_XFER_MODE_NAME);
    } /* end if */
    /* Collective access is not permissible without the MPIO or MPIPOSIX driver */
    if (doing_mpio && xfer_mode==H5FD_MPIO_COLLECTIVE &&
            !(IS_H5FD_MPIO(dataset->ent.file) || IS_H5FD_MPIPOSIX(dataset->ent.file) || IS_H5FD_FPHDF5(dataset->ent.file)))
        HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL, "collective access for MPIO & MPIPOSIX drivers only");

    /* Set the "parallel I/O possible" flag, for H5S_find() */
    if (H5S_mpi_opt_types_g && IS_H5FD_MPIO(dataset->ent.file)) {
	/* Only collective write should call this since it eventually
	 * calls MPI_File_set_view which is a collective call.
	 * See H5S_mpio_spaces_xfer() for details.
	 */
	if (doing_mpio && xfer_mode==H5FD_MPIO_COLLECTIVE)
            sconv_flags |= H5S_CONV_PAR_IO_POSSIBLE;
    } /* end if */
#endif /*H5_HAVE_PARALLEL*/

    /* Make certain that the number of elements in each selection is the same */
    if (nelmts!=(hsize_t)H5S_get_select_npoints(file_space))
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "src and dest data spaces have different sizes");

    /* Retrieve dataset properties */
    /* <none needed in the general case> */

    /* If space hasn't been allocated and not using external storage, 
     * return fill value to buffer if fill time is upon allocation, or
     * do nothing if fill time is never.  If the dataset is compact and 
     * fill time is NEVER, there is no way to tell whether part of data
     * has been overwritten.  So just proceed in reading.     
     */ 
    if(nelmts > 0 && dataset->efl.nused==0 && dataset->layout.type!=H5D_COMPACT 
            && dataset->layout.addr==HADDR_UNDEF) {
        H5O_fill_t  fill;                   /* Fill value info */
        H5D_fill_time_t	fill_time;      /* When to write the fill values */
        H5D_fill_value_t	fill_status;    /* Whether/How the fill value is defined */

        /* Retrieve dataset's fill-value properties */
        if(H5P_fill_value_defined(dc_plist, &fill_status)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't tell if fill value defined");
        if((fill_status==H5D_FILL_VALUE_DEFAULT || fill_status==H5D_FILL_VALUE_USER_DEFINED)
                && H5P_get(dc_plist, H5D_CRT_FILL_VALUE_NAME, &fill) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL,"can't retrieve fill value");
        if(H5P_get(dc_plist, H5D_CRT_FILL_TIME_NAME, &fill_time) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL,"can't retrieve fill time");

        /* Should be impossible, but check anyway... */
        if(fill_status == H5D_FILL_VALUE_UNDEFINED && fill_time == H5D_FILL_TIME_ALLOC)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "read failed: dataset doesn't exist, no data can be read");

        /* If we're never going to fill this dataset, just leave the junk in the user's buffer */
        if(fill_time == H5D_FILL_TIME_NEVER)
            HGOTO_DONE(SUCCEED);

        /* Go fill the user's selection with the dataset's fill value */
        if(H5D_fill(fill.buf,fill.type,buf,mem_type,mem_space, dxpl_id)<0) {
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "filling buf failed");
        } else
            HGOTO_DONE(SUCCEED);
    } /* end if */

    /*
     * Locate the type conversion function and data space conversion
     * functions, and set up the element numbering information. If a data
     * type conversion is necessary then register data type atoms. Data type
     * conversion is necessary if the user has set the `need_bkg' to a high
     * enough value in xfer_parms since turning off data type conversion also
     * turns off background preservation.
     */
    if (NULL==(tpath=H5T_path_find(dataset->type, mem_type, NULL, NULL, dxpl_id))) {
        HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to convert between src and dest data types");
    } else if (!H5T_path_noop(tpath)) {
        if ((src_id=H5I_register(H5I_DATATYPE, H5T_copy(dataset->type, H5T_COPY_ALL)))<0 ||
                (dst_id=H5I_register(H5I_DATATYPE, H5T_copy(mem_type, H5T_COPY_ALL)))<0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL, "unable to register types for conversion");
    } /* end if */

    /* Set the storage flags for the space conversion check */
    switch(dataset->layout.type) {
        case H5D_COMPACT:
            sconv_flags |= H5S_CONV_STORAGE_COMPACT;
            break;

        case H5D_CONTIGUOUS:
            sconv_flags |= H5S_CONV_STORAGE_CONTIGUOUS;
            break;

        case H5D_CHUNKED:
            sconv_flags |= H5S_CONV_STORAGE_CHUNKED;
            break;

        default:
            assert(0 && "Unhandled layout type!");
    } /* end switch */

    /* Get dataspace functions */
    if (NULL==(sconv=H5S_find(mem_space, file_space, sconv_flags)))
        HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to convert from file to memory data space");

    /* Get type of selection on disk & in memory */
    if((fsel_type=H5S_get_select_type(file_space))<0)
        HGOTO_ERROR (H5E_DATASET, H5E_BADSELECT, FAIL, "unable to convert from file to memory data space");
    if((msel_type=H5S_get_select_type(mem_space))<0)
        HGOTO_ERROR (H5E_DATASET, H5E_BADSELECT, FAIL, "unable to convert from file to memory data space");

    /* Determine correct I/O routine to invoke */
    if((fsel_type==H5S_SEL_POINTS || msel_type==H5S_SEL_POINTS) ||
            dataset->layout.type!=H5D_CHUNKED) {
        /* Must use "contiguous" code for point selections,
         * since order of I/O accesses can be different from that of the
         * other kinds of selections
         */
        if(H5D_contig_read(nelmts, dataset, mem_type, mem_space, file_space, tpath, sconv, dc_plist, 
                        dx_plist, dxpl_id, doing_mpio, xfer_mode, src_id, dst_id, buf)<0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data");
    } /* end if */
    else {
        if(H5D_chunk_read(nelmts, dataset, mem_type, mem_space, file_space, tpath, sconv, dc_plist, 
                        dx_plist, dxpl_id, doing_mpio, xfer_mode, src_id, dst_id, buf)<0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data");
    } /* end else */

done:
    if (src_id >= 0)
        H5I_dec_ref(src_id);
    if (dst_id >= 0)
        H5I_dec_ref(dst_id);

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5D_read() */


/*-------------------------------------------------------------------------
 * Function:	H5D_write
 *
 * Purpose:	Writes (part of) a DATASET to a file from application memory
 *		BUF. See H5Dwrite() for complete details.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 * 	Robb Matzke, 9 Jun 1998
 *	The data space is no longer cached in the dataset struct.
 *
 * 	rky 980918
 *	Added must_convert to do non-optimized read when necessary.
 *
 *      Quincey Koziol, 2 July 1999
 *      Changed xfer_parms parameter to xfer plist parameter, so it could
 *      be passed to H5T_convert
 *
 *	Albert Cheng, 2000-11-21
 *	Added the code that when it detects it is not safe to process a
 *	COLLECTIVE write request without hanging, it changes it to
 *	INDEPENDENT calls.
 *      
 *	Albert Cheng, 2000-11-27
 *	Changed to use the optimized MPIO transfer for Collective calls only.
 *
 *      Raymond Lu, 2001-10-2
 *      Changed the way to retrieve property for generic property list.
 *
 *	Raymond Lu, 2002-2-26
 *	For the new fill value design, space may not be allocated until 
 *	this function is called.  Allocate and initialize space if it 
 *	hasn't been.
 *
 *      QAK - 2002/04/02
 *      Removed the must_convert parameter and move preconditions to
 *      H5S_<foo>_opt_possible() routine
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_write(H5D_t *dataset, const H5T_t *mem_type, const H5S_t *mem_space,
	  const H5S_t *file_space, hid_t dxpl_id, const void *buf)
{
    hssize_t	snelmts;                /*total number of elmts	(signed) */
    hsize_t	nelmts;                 /*total number of elmts	*/
    H5T_path_t	*tpath = NULL;		/*type conversion info	*/
    hid_t	src_id = -1, dst_id = -1;/*temporary type atoms */
    H5S_conv_t	*sconv=NULL;		/*space conversion funcs*/
    H5FD_mpio_xfer_t xfer_mode=H5FD_MPIO_INDEPENDENT;	/*xfer_mode for this request */
    hbool_t	doing_mpio=0;		/*This is an MPIO access */
    H5P_genplist_t *dx_plist=NULL;      /* Data transfer property list */
    H5P_genplist_t *dc_plist;           /* Dataset creation roperty list */
    unsigned	sconv_flags=0;	        /* Flags for the space conversion */
    H5S_sel_type fsel_type;             /* Selection type on disk */
    H5S_sel_type msel_type;             /* Selection type in memory */
    herr_t	ret_value = SUCCEED;	/* Return value	*/

    FUNC_ENTER_NOINIT(H5D_write);

    /* check args */
    assert(dataset && dataset->ent.file);
    assert(mem_type);
    assert(buf);

    /* If MPIO, MPIPOSIX, or FPHDF5 is used, no VL datatype support yet. */
    /* This is because they use the global heap in the file and we don't */
    /* support parallel access of that yet */
    if ( (IS_H5FD_MPIO(dataset->ent.file) || IS_H5FD_MPIPOSIX(dataset->ent.file) || IS_H5FD_FPHDF5(dataset->ent.file)) &&
            H5T_get_class(mem_type)==H5T_VLEN)
        HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL, "Parallel IO does not support writing VL datatypes yet");
    /* If MPIO, MPIPOSIX, or FPHDF5 is used, no dataset region reference datatype support yet. */
    /* This is because they use the global heap in the file and we don't */
    /* support parallel access of that yet */
    if ((IS_H5FD_MPIO(dataset->ent.file) || IS_H5FD_MPIPOSIX(dataset->ent.file) || IS_H5FD_FPHDF5(dataset->ent.file)) &&
            H5T_get_class(mem_type)==H5T_REFERENCE &&
            H5T_get_ref_type(mem_type)==H5R_DATASET_REGION)
        HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL, "Parallel IO does not support writing region reference datatypes yet");

    /* Check if we are allowed to write to this file */
    if (0==(H5F_get_intent(dataset->ent.file) & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "no write intent on file");

    /* Get the dataset's creation property list */
    if (NULL == (dc_plist = H5I_object(dataset->dcpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list");

    /* Get the dataset transfer property list */
    if (NULL == (dx_plist = H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset creation property list");

    if (!file_space)
        file_space = dataset->space;
    if (!mem_space)                                                                                                                      
        mem_space = file_space;                                                                                                          
    if((snelmts = H5S_get_select_npoints(mem_space))<0)
	HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "src dataspace has invalid selection");
    nelmts=snelmts;

#ifdef H5_HAVE_PARALLEL
    /* Collect Parallel I/O information for possible later use */
    if (H5FD_MPIO==H5P_peek_hid_t(dx_plist,H5D_XFER_VFL_ID_NAME)) {
	doing_mpio++;
        xfer_mode=H5P_peek_unsigned(dx_plist, H5D_XFER_IO_XFER_MODE_NAME);
    } /* end if */
    
    /* Collective access is not permissible without the MPIO or MPIPOSIX driver */
    if (doing_mpio && xfer_mode==H5FD_MPIO_COLLECTIVE &&
            !(IS_H5FD_MPIO(dataset->ent.file) || IS_H5FD_MPIPOSIX(dataset->ent.file) || IS_H5FD_FPHDF5(dataset->ent.file)))
        HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL, "collective access for MPIO driver only");

    /* If dataset is compact, collective access is only allowed when file space
     * selection is H5S_ALL */
    if(doing_mpio && xfer_mode==H5FD_MPIO_COLLECTIVE
            && dataset->layout.type==H5D_COMPACT) { 
        if(H5S_get_select_type(file_space) != H5S_SEL_ALL)    
            HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL, "collective access to compact dataset doesn't support partial access");        
    }
    
    /* Set the "parallel I/O possible" flag, for H5S_find() */
    if (H5S_mpi_opt_types_g && IS_H5FD_MPIO(dataset->ent.file)) {
	/* Only collective write should call this since it eventually
	 * calls MPI_File_set_view which is a collective call.
	 * See H5S_mpio_spaces_xfer() for details.
	 */
	if (doing_mpio && xfer_mode==H5FD_MPIO_COLLECTIVE)
            sconv_flags |= H5S_CONV_PAR_IO_POSSIBLE;
    } /* end if */
#endif /*H5_HAVE_PARALLEL*/

    /* Make certain that the number of elements in each selection is the same */
    if (nelmts!=(hsize_t)H5S_get_select_npoints(file_space))
	HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "src and dest data spaces have different sizes");

    /* Retrieve dataset properties */
    /* <none needed currently> */

    /* Allocate data space and initialize it if it hasn't been. */
    if(nelmts > 0 && dataset->efl.nused==0 && dataset->layout.type!=H5D_COMPACT 
            && dataset->layout.addr==HADDR_UNDEF) {
        hssize_t file_nelmts;   /* Number of elements in file dataset's dataspace */

        /* Get the number of elements in file dataset's dataspace */
        if((file_nelmts=H5S_get_simple_extent_npoints(file_space))<0)
            HGOTO_ERROR (H5E_DATASET, H5E_BADVALUE, FAIL, "can't retrieve number of elements in file dataset");        

 	/* Allocate storage */
        if(H5D_alloc_storage(dataset->ent.file,dxpl_id,dataset,H5D_ALLOC_WRITE, TRUE, (hbool_t)((hsize_t)file_nelmts==nelmts ? TRUE : FALSE))<0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize storage");
    } /* end if */

    /*
     * Locate the type conversion function and data space conversion
     * functions, and set up the element numbering information. If a data
     * type conversion is necessary then register data type atoms. Data type
     * conversion is necessary if the user has set the `need_bkg' to a high
     * enough value in xfer_parms since turning off data type conversion also
     * turns off background preservation.
     */
    if (NULL==(tpath=H5T_path_find(mem_type, dataset->type, NULL, NULL, dxpl_id))) {
	HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to convert between src and dest data types");
    } else if (!H5T_path_noop(tpath)) {
	if ((src_id = H5I_register(H5I_DATATYPE, H5T_copy(mem_type, H5T_COPY_ALL)))<0 ||
                (dst_id = H5I_register(H5I_DATATYPE, H5T_copy(dataset->type, H5T_COPY_ALL)))<0)
	    HGOTO_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL, "unable to register types for conversion");
    } /* end if */

    /* Set the storage flags for the space conversion check */
    switch(dataset->layout.type) {
        case H5D_COMPACT:
            sconv_flags |= H5S_CONV_STORAGE_COMPACT;
            break;

        case H5D_CONTIGUOUS:
            sconv_flags |= H5S_CONV_STORAGE_CONTIGUOUS;
            break;

        case H5D_CHUNKED:
            sconv_flags |= H5S_CONV_STORAGE_CHUNKED;
            break;

        default:
            assert(0 && "Unhandled layout type!");
    } /* end switch */

    /* Get dataspace functions */
    if (NULL==(sconv=H5S_find(mem_space, file_space, sconv_flags)))
	HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to convert from memory to file data space");
        
    /* Get type of selection on disk & in memory */
    if((fsel_type=H5S_get_select_type(file_space))<0)
        HGOTO_ERROR (H5E_DATASET, H5E_BADSELECT, FAIL, "unable to convert from file to memory data space");
    if((msel_type=H5S_get_select_type(mem_space))<0)
        HGOTO_ERROR (H5E_DATASET, H5E_BADSELECT, FAIL, "unable to convert from file to memory data space");

    /* Determine correct I/O routine to invoke */
    if((fsel_type==H5S_SEL_POINTS || msel_type==H5S_SEL_POINTS) ||
            dataset->layout.type!=H5D_CHUNKED) {
        /* Must use "contiguous" code for point selections,
         * since order of I/O accesses can be different from that of the
         * other kinds of selections
         */
        if(H5D_contig_write(nelmts, dataset, mem_type, mem_space, file_space, tpath, sconv, dc_plist,
                        dx_plist, dxpl_id, doing_mpio, xfer_mode, src_id, dst_id, buf)<0)
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data");
    } /* end if */
    else {
        if(H5D_chunk_write(nelmts, dataset, mem_type, mem_space, file_space, tpath, sconv, dc_plist,
                        dx_plist, dxpl_id, doing_mpio, xfer_mode, src_id, dst_id, buf)<0)
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data");
    } /* end else */

    /*
     * Update modification time.  We have to do this explicitly because
     * writing to a dataset doesn't necessarily change the object header.
     */
    if (H5O_touch(&(dataset->ent), FALSE, dxpl_id)<0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to update modification time");

done:
    if (src_id >= 0)
        H5I_dec_ref(src_id);
    if (dst_id >= 0)
        H5I_dec_ref(dst_id);

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5D_write() */


/*-------------------------------------------------------------------------
 * Function:	H5D_contig_read
 *
 * Purpose:	Read from a contiguous dataset.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Thursday, April 10, 2003
 *
 * Modifications:
 *      QAK - 2003/04/17
 *      Hacked on it a lot. :-)
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5D_contig_read(hsize_t nelmts, H5D_t *dataset, const H5T_t *mem_type, const H5S_t *mem_space,
    const H5S_t *file_space, H5T_path_t *tpath, H5S_conv_t *sconv, H5P_genplist_t *dc_plist, 
    H5P_genplist_t *dx_plist, hid_t dxpl_id, hbool_t
#ifndef H5_HAVE_PARALLEL
    UNUSED
#endif /*H5_HAVE_PARALLEL*/
    doing_mpio, H5FD_mpio_xfer_t
#ifndef H5_HAVE_PARALLEL
    UNUSED
#endif /*H5_HAVE_PARALLEL*/
    xfer_mode, 
    hid_t src_id, hid_t dst_id, void *buf/*out*/)
{
    herr_t      status;                 /*function return status*/     
#ifdef H5S_DEBUG
    H5_timer_t	timer;
#endif
    size_t	src_type_size;		/*size of source type	*/
    size_t	dst_type_size;	        /*size of destination type*/
    size_t	target_size;		/*desired buffer size	*/
    hsize_t	request_nelmts;		/*requested strip mine	*/
    H5S_sel_iter_t mem_iter;            /*memory selection iteration info*/
    hbool_t	mem_iter_init=0;	/*memory selection iteration info has been initialized */
    H5S_sel_iter_t bkg_iter;            /*background iteration info*/
    hbool_t	bkg_iter_init=0;	/*background iteration info has been initialized */
    H5S_sel_iter_t file_iter;           /*file selection iteration info*/
    hbool_t	file_iter_init=0;	/*file selection iteration info has been initialized */
    H5T_bkg_t	need_bkg;		/*type of background buf*/
    uint8_t	*tconv_buf = NULL;	/*data type conv buffer	*/
    uint8_t	*bkg_buf = NULL;	/*background buffer	*/
    hsize_t	smine_start;		/*strip mine start loc	*/
    hsize_t	n, smine_nelmts;	/*elements per strip	*/
#ifdef H5_HAVE_PARALLEL
    hbool_t     xfer_mode_changed;      /* Whether the transfer mode was changed */
#endif /*H5_HAVE_PARALLEL*/
    herr_t	ret_value = SUCCEED;	/*return value		*/

    FUNC_ENTER_NOINIT(H5D_contig_read);
    
    /*
     * If there is no type conversion then read directly into the
     * application's buffer.  This saves at least one mem-to-mem copy.
     */
    if (H5T_path_noop(tpath)) {
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
  	/* Sanity check dataset, then read it */
        assert(dataset->layout.addr!=HADDR_UNDEF || dataset->efl.nused>0 || 
             dataset->layout.type==H5D_COMPACT);
        status = (sconv->read)(dataset->ent.file, &(dataset->layout), 
             dc_plist, (H5D_storage_t *)&(dataset->efl), H5T_get_size(dataset->type), 
             file_space, mem_space, dxpl_id, buf/*out*/);
#ifdef H5S_DEBUG
        H5_timer_end(&(sconv->stats[1].read_timer), &timer);
        sconv->stats[1].read_nbytes += nelmts * H5T_get_size(dataset->type);
        sconv->stats[1].read_ncalls++;
#endif

        /* Check return value from optimized read */
        if (status<0) {
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "optimized read failed");
        } else
	    /* direct xfer accomplished successfully */
            HGOTO_DONE(SUCCEED);
    } /* end if */
    
    /*
     * This is the general case(type conversion).
     */

#ifdef H5_HAVE_PARALLEL
    H5D_io_assist_mpio(dx_plist, doing_mpio, xfer_mode, &xfer_mode_changed);
#endif /*H5_HAVE_PARALLEL*/
   
    /* Compute element sizes and other parameters */
    src_type_size = H5T_get_size(dataset->type);
    dst_type_size = H5T_get_size(mem_type);
    target_size = H5P_peek_size_t(dx_plist,H5D_XFER_MAX_TEMP_BUF_NAME);
    request_nelmts = target_size / MAX(src_type_size, dst_type_size);

    /* Figure out the strip mine size. */
    if (H5S_select_iter_init(&file_iter, file_space, src_type_size)<0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize file selection information");
    file_iter_init=1;	/*file selection iteration info has been initialized */
    if (H5S_select_iter_init(&mem_iter, mem_space, dst_type_size)<0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize memory selection information");
    mem_iter_init=1;	/*file selection iteration info has been initialized */
    if (H5S_select_iter_init(&bkg_iter, mem_space, dst_type_size)<0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize background selection information");
    bkg_iter_init=1;	/*file selection iteration info has been initialized */

    /* Sanity check elements in temporary buffer */
    if (request_nelmts<=0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "temporary buffer max size is too small");

    /*
     * Get a temporary buffer for type conversion unless the app has already
     * supplied one through the xfer properties. Instead of allocating a
     * buffer which is the exact size, we allocate the target size.  The
     * malloc() is usually less resource-intensive if we allocate/free the
     * same size over and over.
     */
    if (H5T_path_bkg(tpath)) {
        /* Retrieve the bkgr buffer property */
        if(H5P_get(dx_plist, H5D_XFER_BKGR_BUF_TYPE_NAME, &need_bkg)<0)
            HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve background buffer type");
        need_bkg = MAX(H5T_path_bkg(tpath), need_bkg);
    } else {
        need_bkg = H5T_BKG_NO; /*never needed even if app says yes*/
    } /* end else */
    if (NULL==(tconv_buf=H5P_peek_voidp(dx_plist,H5D_XFER_TCONV_BUF_NAME))) {
        /* Allocate temporary buffer */
        if((tconv_buf=H5FL_BLK_MALLOC(type_conv,target_size))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for type conversion");
    } /* end if */
    if (need_bkg && NULL==(bkg_buf=H5P_peek_voidp(dx_plist,H5D_XFER_BKGR_BUF_NAME))) {
        /* Allocate background buffer */
        H5_CHECK_OVERFLOW((request_nelmts*dst_type_size),hsize_t,size_t);
        if((bkg_buf=H5FL_BLK_MALLOC(type_conv,(size_t)(request_nelmts*dst_type_size)))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for background conversion");
    } /* end if */

    /* Start strip mining... */
    for (smine_start=0; smine_start<nelmts; smine_start+=smine_nelmts) {
        /* Go figure out how many elements to read from the file */
        assert(H5S_select_iter_nelmts(&file_iter)==(nelmts-smine_start));
        smine_nelmts = MIN(request_nelmts, (nelmts-smine_start));
	
        /*
         * Gather the data from disk into the data type conversion
         * buffer. Also gather data from application to background buffer
         * if necessary.
         */
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
	/* Sanity check that space is allocated, then read data from it */ 
        assert(dataset->layout.addr!=HADDR_UNDEF || dataset->efl.nused>0 || 
             dataset->layout.type==H5D_COMPACT);
        n = H5S_select_fgath(dataset->ent.file, &(dataset->layout), 
                dc_plist, (H5D_storage_t *)&dataset->efl, src_type_size, file_space, 
                &file_iter, smine_nelmts, dxpl_id, tconv_buf/*out*/);

#ifdef H5S_DEBUG
	H5_timer_end(&(sconv->stats[1].gath_timer), &timer);
	sconv->stats[1].gath_nbytes += n * src_type_size;
	sconv->stats[1].gath_ncalls++;
#endif
	if (n!=smine_nelmts)
            HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "file gather failed");
	
        if (need_bkg) {
#ifdef H5S_DEBUG
            H5_timer_begin(&timer);
#endif
            n = H5S_select_mgath(buf, dst_type_size, mem_space, &bkg_iter,
				 smine_nelmts, dxpl_id, bkg_buf/*out*/);
#ifdef H5S_DEBUG
            H5_timer_end(&(sconv->stats[1].bkg_timer), &timer);
            sconv->stats[1].bkg_nbytes += n * dst_type_size;
            sconv->stats[1].bkg_ncalls++;
#endif
            if (n!=smine_nelmts)
                HGOTO_ERROR (H5E_IO, H5E_READERROR, FAIL, "mem gather failed");
        } /* end if */
	
        /*
         * Perform data type conversion.
         */
        if (H5T_convert(tpath, src_id, dst_id, smine_nelmts, 0, 0, tconv_buf, bkg_buf, dxpl_id)<0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "data type conversion failed");

        /*
         * Scatter the data into memory.
         */
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
        status = H5S_select_mscat(tconv_buf, dst_type_size, mem_space,
                          &mem_iter, smine_nelmts, dxpl_id, buf/*out*/);
#ifdef H5S_DEBUG
	H5_timer_end(&(sconv->stats[1].scat_timer), &timer);
	sconv->stats[1].scat_nbytes += smine_nelmts * dst_type_size;
	sconv->stats[1].scat_ncalls++;
#endif
	if (status<0)
            HGOTO_ERROR (H5E_DATASET, H5E_READERROR, FAIL, "scatter failed");
	
    } /* end for */

done:
#ifdef H5_HAVE_PARALLEL
    /* restore xfer_mode due to the kludge */
    if (doing_mpio && xfer_mode_changed) {
#ifdef H5D_DEBUG
	if (H5DEBUG(D))
	    fprintf (H5DEBUG(D), "H5D: dx->xfer_mode was COLLECTIVE, restored to INDEPENDENT\n");
#endif
	xfer_mode = H5FD_MPIO_COLLECTIVE;
        if(H5P_set (dx_plist, H5D_XFER_IO_XFER_MODE_NAME, &xfer_mode) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set transfer mode");
    } /* end if */
#endif /*H5_HAVE_PARALLEL*/
    /* Release selection iterators */
    if(file_iter_init)
        H5S_select_iter_release(&file_iter);
    if(mem_iter_init)
        H5S_select_iter_release(&mem_iter);
    if(bkg_iter_init)
        H5S_select_iter_release(&bkg_iter);

    assert(dx_plist);
    if (tconv_buf && NULL==H5P_peek_voidp(dx_plist,H5D_XFER_TCONV_BUF_NAME))
        H5FL_BLK_FREE(type_conv,tconv_buf);
    if (bkg_buf && NULL==H5P_peek_voidp(dx_plist,H5D_XFER_BKGR_BUF_NAME))
        H5FL_BLK_FREE(type_conv,bkg_buf);
        
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5D_contig_read() */


/*-------------------------------------------------------------------------
 * Function:	H5D_contig_write
 *
 * Purpose:	Write to a contiguous dataset.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Thursday, April 10, 2003
 *
 * Modifications:
 *      QAK - 2003/04/17
 *      Hacked on it a lot. :-)
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_contig_write(hsize_t nelmts, H5D_t *dataset, const H5T_t *mem_type, const H5S_t *mem_space,
    const H5S_t *file_space, H5T_path_t *tpath, H5S_conv_t *sconv, H5P_genplist_t *dc_plist,
    H5P_genplist_t *dx_plist, hid_t dxpl_id, hbool_t
#ifndef H5_HAVE_PARALLEL
    UNUSED
#endif /*H5_HAVE_PARALLEL*/
    doing_mpio, H5FD_mpio_xfer_t
#ifndef H5_HAVE_PARALLEL
    UNUSED
#endif /*H5_HAVE_PARALLEL*/
    xfer_mode, 
    hid_t src_id, hid_t dst_id, const void *buf)
{
    herr_t      status;                 /*function return status*/     
#ifdef H5S_DEBUG
    H5_timer_t	timer;
#endif
    size_t	src_type_size;		/*size of source type	*/
    size_t	dst_type_size;	        /*size of destination type*/
    size_t	target_size;		/*desired buffer size	*/
    hsize_t	request_nelmts;		/*requested strip mine	*/
    H5S_sel_iter_t mem_iter;            /*memory selection iteration info*/
    hbool_t	mem_iter_init=0;	/*memory selection iteration info has been initialized */
    H5S_sel_iter_t bkg_iter;            /*background iteration info*/
    hbool_t	bkg_iter_init=0;	/*background iteration info has been initialized */
    H5S_sel_iter_t file_iter;           /*file selection iteration info*/
    hbool_t	file_iter_init=0;	/*file selection iteration info has been initialized */
    H5T_bkg_t	need_bkg;		/*type of background buf*/
    uint8_t	*tconv_buf = NULL;	/*data type conv buffer	*/
    uint8_t	*bkg_buf = NULL;	/*background buffer	*/
    hsize_t	smine_start;		/*strip mine start loc	*/
    hsize_t	n, smine_nelmts;	/*elements per strip	*/
#ifdef H5_HAVE_PARALLEL
    hbool_t     xfer_mode_changed;      /* Whether the transfer mode was changed */
#endif /*H5_HAVE_PARALLEL*/
    herr_t	ret_value = SUCCEED;	/*return value		*/

    FUNC_ENTER_NOINIT(H5D_contig_write);
    
    /*
     * If there is no type conversion then write directly from the
     * application's buffer.  This saves at least one mem-to-mem copy.
     */
    if (H5T_path_noop(tpath)) {
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
        status = (sconv->write)(dataset->ent.file, &(dataset->layout),
                dc_plist, (H5D_storage_t *)&(dataset->efl), H5T_get_size(dataset->type),
                file_space, mem_space, dxpl_id, buf);
#ifdef H5S_DEBUG
	    H5_timer_end(&(sconv->stats[0].write_timer), &timer);
	    sconv->stats[0].write_nbytes += nelmts * H5T_get_size(mem_type);
	    sconv->stats[0].write_ncalls++;
#endif

        /* Check return value from optimized write */
        if (status<0) {
	    HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "optimized write failed");
	} else
	    /* direct xfer accomplished successfully */
	    HGOTO_DONE(SUCCEED);
    } /* end if */
    
    /*
     * This is the general case.
     */

#ifdef H5_HAVE_PARALLEL
    H5D_io_assist_mpio(dx_plist, doing_mpio, xfer_mode, &xfer_mode_changed);
#endif /*H5_HAVE_PARALLEL*/

    /* Compute element sizes and other parameters */
    src_type_size = H5T_get_size(mem_type);
    dst_type_size = H5T_get_size(dataset->type);
    target_size = H5P_peek_size_t(dx_plist,H5D_XFER_MAX_TEMP_BUF_NAME);
    request_nelmts = target_size / MAX (src_type_size, dst_type_size);

    /* Sanity check elements in temporary buffer */
    if (request_nelmts<=0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "temporary buffer max size is too small");

    /* Figure out the strip mine size. */
    if (H5S_select_iter_init(&file_iter, file_space, dst_type_size)<0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize file selection information");
    file_iter_init=1;	/*file selection iteration info has been initialized */
    if (H5S_select_iter_init(&mem_iter, mem_space, src_type_size)<0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize memory selection information");
    mem_iter_init=1;	/*file selection iteration info has been initialized */
    if (H5S_select_iter_init(&bkg_iter, file_space, dst_type_size)<0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize background selection information");
    bkg_iter_init=1;	/*file selection iteration info has been initialized */
     
    /*
     * Get a temporary buffer for type conversion unless the app has already
     * supplied one through the xfer properties.  Instead of allocating a
     * buffer which is the exact size, we allocate the target size. The
     * malloc() is usually less resource-intensive if we allocate/free the
     * same size over and over.
     */
    if (H5T_path_bkg(tpath)) {
        /* Retrieve the bkgr buffer property */
        if(H5P_get(dx_plist, H5D_XFER_BKGR_BUF_TYPE_NAME, &need_bkg)<0)
            HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve background buffer type");
        need_bkg = MAX (H5T_path_bkg(tpath), need_bkg);
    } else if(H5T_detect_class(dataset->type, H5T_VLEN)) {
	/* Old data is retrieved into background buffer for VL datatype.  The 
	 * data is used later for freeing heap objects. */
        need_bkg = H5T_BKG_YES;
    } else {
        need_bkg = H5T_BKG_NO; /*never needed even if app says yes*/
    } /* end else */
    if (NULL==(tconv_buf=H5P_peek_voidp(dx_plist,H5D_XFER_TCONV_BUF_NAME))) {
        /* Allocate temporary buffer */
        if((tconv_buf=H5FL_BLK_MALLOC(type_conv,target_size))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for type conversion");
    } /* end if */
    if (need_bkg && NULL==(bkg_buf=H5P_peek_voidp(dx_plist,H5D_XFER_BKGR_BUF_NAME))) {
        /* Allocate background buffer */
        H5_CHECK_OVERFLOW((request_nelmts*dst_type_size),hsize_t,size_t);
        if((bkg_buf=H5FL_BLK_CALLOC(type_conv,(size_t)(request_nelmts*dst_type_size)))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for background conversion");
    } /* end if */

    /* Start strip mining... */
    for (smine_start=0; smine_start<nelmts; smine_start+=smine_nelmts) {
        /* Go figure out how many elements to read from the file */
        assert(H5S_select_iter_nelmts(&file_iter)==(nelmts-smine_start));
        smine_nelmts = MIN(request_nelmts, (nelmts-smine_start));
	
        /*
         * Gather data from application buffer into the data type conversion
         * buffer. Also gather data from the file into the background buffer
         * if necessary.
         */
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
        n = H5S_select_mgath(buf, src_type_size, mem_space, &mem_iter,
			     smine_nelmts, dxpl_id, tconv_buf/*out*/);
#ifdef H5S_DEBUG
	H5_timer_end(&(sconv->stats[0].gath_timer), &timer);
	sconv->stats[0].gath_nbytes += n * src_type_size;
	sconv->stats[0].gath_ncalls++;
#endif
        if (n!=smine_nelmts)
            HGOTO_ERROR (H5E_IO, H5E_WRITEERROR, FAIL, "mem gather failed");

        if (need_bkg) {
#ifdef H5S_DEBUG
            H5_timer_begin(&timer);
#endif
            n = H5S_select_fgath(dataset->ent.file, &(dataset->layout), 
                 dc_plist, (H5D_storage_t *)&(dataset->efl), dst_type_size, file_space, 
                 &bkg_iter, smine_nelmts, dxpl_id, bkg_buf/*out*/); 

#ifdef H5S_DEBUG
            H5_timer_end(&(sconv->stats[0].bkg_timer), &timer);
            sconv->stats[0].bkg_nbytes += n * dst_type_size;
            sconv->stats[0].bkg_ncalls++;
#endif
            if (n!=smine_nelmts)
                HGOTO_ERROR (H5E_IO, H5E_WRITEERROR, FAIL, "file gather failed");
        } /* end if */
	
        /*
         * Perform data type conversion.
         */
        if (H5T_convert(tpath, src_id, dst_id, smine_nelmts, 0, 0, tconv_buf, bkg_buf, dxpl_id)<0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "data type conversion failed");

        /*
         * Scatter the data out to the file.
         */
#ifdef H5S_DEBUG
        H5_timer_begin(&timer);
#endif
        status = H5S_select_fscat(dataset->ent.file, &(dataset->layout), 
              dc_plist, (H5D_storage_t *)&(dataset->efl), dst_type_size, file_space, &file_iter,
              smine_nelmts, dxpl_id, tconv_buf);

#ifdef H5S_DEBUG
        H5_timer_end(&(sconv->stats[0].scat_timer), &timer);
        sconv->stats[0].scat_nbytes += smine_nelmts * dst_type_size;
        sconv->stats[0].scat_ncalls++;
#endif
        if (status<0)
            HGOTO_ERROR (H5E_IO, H5E_WRITEERROR, FAIL, "scatter failed");
    } /* end for */

done:
#ifdef H5_HAVE_PARALLEL
    /* restore xfer_mode due to the kludge */
    if (doing_mpio && xfer_mode_changed) {
#ifdef H5D_DEBUG
	if (H5DEBUG(D))
	    fprintf (H5DEBUG(D), "H5D: dx->xfer_mode was COLLECTIVE, restored to INDEPENDENT\n");
#endif
	xfer_mode = H5FD_MPIO_COLLECTIVE;
        if(H5P_set (dx_plist, H5D_XFER_IO_XFER_MODE_NAME, &xfer_mode) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set transfer mode");
    } /* end if */
#endif /*H5_HAVE_PARALLEL*/
    /* Release selection iterators */
    if(file_iter_init)
        H5S_select_iter_release(&file_iter);
    if(mem_iter_init)
        H5S_select_iter_release(&mem_iter);
    if(bkg_iter_init)
        H5S_select_iter_release(&bkg_iter);

    assert(dx_plist);
    if (tconv_buf && NULL==H5P_peek_voidp(dx_plist,H5D_XFER_TCONV_BUF_NAME))
        H5FL_BLK_FREE(type_conv,tconv_buf);
    if (bkg_buf && NULL==H5P_peek_voidp(dx_plist,H5D_XFER_BKGR_BUF_NAME))
        H5FL_BLK_FREE(type_conv,bkg_buf);

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5D_contig_write() */


/*-------------------------------------------------------------------------
 * Function:	H5D_chunk_read
 *
 * Purpose:	Read from a chunked dataset.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Thursday, April 10, 2003
 *
 * Modifications:
 *      QAK - 2003/04/17
 *      Hacked on it a lot. :-)
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_chunk_read(hsize_t nelmts, H5D_t *dataset, const H5T_t *mem_type, const H5S_t *mem_space,
    const H5S_t *file_space, H5T_path_t *tpath, H5S_conv_t *sconv, H5P_genplist_t *dc_plist, 
    H5P_genplist_t *dx_plist, hid_t dxpl_id, hbool_t
#ifndef H5_HAVE_PARALLEL
    UNUSED
#endif /*H5_HAVE_PARALLEL*/
    doing_mpio, H5FD_mpio_xfer_t
#ifndef H5_HAVE_PARALLEL
    UNUSED
#endif /*H5_HAVE_PARALLEL*/
    xfer_mode, hid_t src_id, hid_t dst_id, void *buf/*out*/)
{
    fm_map      fm;                     /* File<->memory mapping */
    herr_t      status;                 /*function return status*/     
#ifdef H5S_DEBUG
    H5_timer_t	timer;
#endif
    size_t	src_type_size;		/*size of source type	*/
    size_t	dst_type_size;	        /*size of destination type*/
    size_t	target_size;		/*desired buffer size	*/
    hsize_t	request_nelmts;		/*requested strip mine	*/
    hsize_t     smine_start;            /*strip mine start loc  */
    hsize_t     n, smine_nelmts;        /*elements per strip    */    
    H5S_sel_iter_t mem_iter;            /*memory selection iteration info*/
    hbool_t	mem_iter_init=0;	/*memory selection iteration info has been initialized */
    H5S_sel_iter_t bkg_iter;            /*background iteration info*/
    hbool_t	bkg_iter_init=0;	/*background iteration info has been initialized */
    H5S_sel_iter_t file_iter;           /*file selection iteration info*/
    hbool_t	file_iter_init=0;	/*file selection iteration info has been initialized */
    H5T_bkg_t	need_bkg;		/*type of background buf*/
    uint8_t	*tconv_buf = NULL;	/*data type conv buffer	*/
    uint8_t	*bkg_buf = NULL;	/*background buffer	*/
    H5D_storage_t store;                /*union of EFL and chunk pointer in file space */
    hsize_t     h;                      /* Local index variable */
#ifdef H5_HAVE_PARALLEL
    hbool_t     xfer_mode_changed;      /* Whether the transfer mode was changed */
#endif /*H5_HAVE_PARALLEL*/
    herr_t	ret_value = SUCCEED;	/*return value		*/

    FUNC_ENTER_NOINIT(H5D_chunk_read);
    
    /* Initialize fm_map*/
    HDmemset(&fm, 0, sizeof(fm_map));
    fm.layout = &(dataset->layout);
     
    /* Map elements between file and memory for each chunk*/
    H5D_chunk_mem_file_map(dataset, mem_type, file_space, mem_space, &fm);

    /*
     * If there is no type conversion then read directly into the
     * application's buffer.  This saves at least one mem-to-mem copy.
     */
    if (H5T_path_noop(tpath)) {
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
  	/* Sanity check dataset, then read it */
        assert(dataset->layout.addr!=HADDR_UNDEF || dataset->efl.nused>0 || 
             dataset->layout.type==H5D_COMPACT);

        /*loop through each chunk, read data*/
        for(h=0; h<fm.nchunks; h++) {
            if(H5S_get_select_npoints(fm.fspace[h]) > 0) {
                /*pass in chunk's coordinates in a union.  LAYOUT puts datatype size as an extra dimension,
                 * have to take it out.*/
                store.chunk_coords = fm.chunk_coords + h*dataset->layout.ndims;

                status = (sconv->read)(dataset->ent.file, &(dataset->layout), 
                        dc_plist, &store, H5T_get_size(dataset->type), 
                        fm.fspace[h], fm.mspace[h], dxpl_id, buf/*out*/);

                /* Check return value from optimized read */
                if (status<0)
                    HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "optimized read failed");
            } /* end if */
        } /* end for */
        
#ifdef H5S_DEBUG
        H5_timer_end(&(sconv->stats[1].read_timer), &timer);
        sconv->stats[1].read_nbytes += nelmts * H5T_get_size(dataset->type);
        sconv->stats[1].read_ncalls++;
#endif

	/* direct xfer accomplished successfully */
        HGOTO_DONE(SUCCEED);
    } /* end if */

    /*
     * This is the general case(type conversion).
     */

#ifdef H5_HAVE_PARALLEL
    H5D_io_assist_mpio(dx_plist, doing_mpio, xfer_mode, &xfer_mode_changed);
#endif /*H5_HAVE_PARALLEL*/

    /* Compute element sizes and other parameters */
    src_type_size = H5T_get_size(dataset->type);
    dst_type_size = H5T_get_size(mem_type);
    target_size = H5P_peek_size_t(dx_plist,H5D_XFER_MAX_TEMP_BUF_NAME);
    request_nelmts = target_size / MAX(src_type_size, dst_type_size);
    
    /* Sanity check elements in temporary buffer */
    if (request_nelmts<=0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "temporary buffer max size is too small");

    /*
     * Get a temporary buffer for type conversion unless the app has already
     * supplied one through the xfer properties. Instead of allocating a
     * buffer which is the exact size, we allocate the target size.  The
     * malloc() is usually less resource-intensive if we allocate/free the
     * same size over and over.
     */
    if (H5T_path_bkg(tpath)) {
        /* Retrieve the bkgr buffer property */
        if(H5P_get(dx_plist, H5D_XFER_BKGR_BUF_TYPE_NAME, &need_bkg)<0)
            HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve background buffer type");
        need_bkg = MAX(H5T_path_bkg(tpath), need_bkg);
    } else {
        need_bkg = H5T_BKG_NO; /*never needed even if app says yes*/
    } /* end else */
    if (NULL==(tconv_buf=H5P_peek_voidp(dx_plist,H5D_XFER_TCONV_BUF_NAME))) {
        /* Allocate temporary buffer */
        if((tconv_buf=H5FL_BLK_MALLOC(type_conv,target_size))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for type conversion");
    } /* end if */
    if (need_bkg && NULL==(bkg_buf=H5P_peek_voidp(dx_plist,H5D_XFER_BKGR_BUF_NAME))) {
        /* Allocate background buffer */
        H5_CHECK_OVERFLOW((request_nelmts*dst_type_size),hsize_t,size_t);
        if((bkg_buf=H5FL_BLK_MALLOC(type_conv,(size_t)(request_nelmts*dst_type_size)))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for background conversion");
    } /* end if */
    
    /* Loop over all the chunks, performing I/O on each */
    for(h=0; h<fm.nchunks; h++) {
        hsize_t chunk_nelmts;   /* Number of elements selected in current chunk */

        /* Get the number of elements selected in this chunk */
        chunk_nelmts=H5S_get_select_npoints(fm.fspace[h]);
        assert(chunk_nelmts<=nelmts);
        if(chunk_nelmts > 0) {
            /* initialize selection iterator */
            if (H5S_select_iter_init(&file_iter, fm.fspace[h], src_type_size)<0)
                HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize file selection information");
            file_iter_init=1;	/*file selection iteration info has been initialized */
            if (H5S_select_iter_init(&mem_iter, fm.mspace[h], dst_type_size)<0)
                HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize memory selection information");
            mem_iter_init=1;	/*file selection iteration info has been initialized */
            if (H5S_select_iter_init(&bkg_iter, fm.mspace[h], dst_type_size)<0)
                HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize background selection information");
            bkg_iter_init=1;	/*file selection iteration info has been initialized */
            
            /*pass in chunk's coordinates in a union*/
            store.chunk_coords = fm.chunk_coords + h*dataset->layout.ndims;
               
            for (smine_start=0; smine_start<chunk_nelmts; smine_start+=smine_nelmts) {
                /* Go figure out how many elements to read from the file */
                assert(H5S_select_iter_nelmts(&file_iter)==(chunk_nelmts-smine_start));
                smine_nelmts = MIN(request_nelmts, (chunk_nelmts-smine_start));
            
                /*
                 * Gather the data from disk into the data type conversion
                 * buffer. Also gather data from application to background buffer
                 * if necessary.
                 */
#ifdef H5S_DEBUG
                H5_timer_begin(&timer);
#endif
                /* Sanity check that space is allocated, then read data from it */ 
                assert(dataset->layout.addr!=HADDR_UNDEF || dataset->efl.nused>0 || 
                     dataset->layout.type==H5D_COMPACT);
                n = H5S_select_fgath(dataset->ent.file, &(dataset->layout), 
                        dc_plist, &store, src_type_size, fm.fspace[h], 
                        &file_iter, smine_nelmts, dxpl_id, tconv_buf/*out*/);

#ifdef H5S_DEBUG
                H5_timer_end(&(sconv->stats[1].gath_timer), &timer);
                sconv->stats[1].gath_nbytes += n * src_type_size;
                sconv->stats[1].gath_ncalls++;
#endif
                if (n!=smine_nelmts)
                    HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "file gather failed");

                if (need_bkg) {
#ifdef H5S_DEBUG
                    H5_timer_begin(&timer);
#endif
                    n = H5S_select_mgath(buf, dst_type_size, fm.mspace[h], &bkg_iter,
                                     smine_nelmts, dxpl_id, bkg_buf/*out*/);
#ifdef H5S_DEBUG
                    H5_timer_end(&(sconv->stats[1].bkg_timer), &timer);
                    sconv->stats[1].bkg_nbytes += n * dst_type_size;
                    sconv->stats[1].bkg_ncalls++;
#endif
                    if (n!=smine_nelmts)
                        HGOTO_ERROR (H5E_IO, H5E_READERROR, FAIL, "mem gather failed");
                } /* end if */
	
                /*
                 * Perform data type conversion.
                 */
                if (H5T_convert(tpath, src_id, dst_id, smine_nelmts, 0, 0, 
                        tconv_buf, bkg_buf, dxpl_id)<0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "data type conversion failed");

                /*
                 * Scatter the data into memory.
                 */
#ifdef H5S_DEBUG
                H5_timer_begin(&timer);
#endif
                status = H5S_select_mscat(tconv_buf, dst_type_size, fm.mspace[h],
                            &mem_iter, smine_nelmts, dxpl_id, buf/*out*/);
#ifdef H5S_DEBUG
                H5_timer_end(&(sconv->stats[1].scat_timer), &timer);
                sconv->stats[1].scat_nbytes += smine_nelmts * dst_type_size;
                sconv->stats[1].scat_ncalls++;
#endif
                if (status<0)
                    HGOTO_ERROR (H5E_DATASET, H5E_READERROR, FAIL, "scatter failed");
            } /* end for */

            /* Release selection iterators */
            if(file_iter_init) {
                H5S_select_iter_release(&file_iter);
                file_iter_init=0;
            } /* end if */
            if(mem_iter_init) {
                H5S_select_iter_release(&mem_iter);
                mem_iter_init=0;
            } /* end if */
            if(bkg_iter_init) {
                H5S_select_iter_release(&bkg_iter);
                bkg_iter_init=0;
            } /* end if */
        } /* end if */
    } /* end for */
     
done:
#ifdef H5_HAVE_PARALLEL
    /* restore xfer_mode due to the kludge */
    if (doing_mpio && xfer_mode_changed) {
#ifdef H5D_DEBUG
	if (H5DEBUG(D))
	    fprintf (H5DEBUG(D), "H5D: dx->xfer_mode was COLLECTIVE, restored to INDEPENDENT\n");
#endif
	xfer_mode = H5FD_MPIO_COLLECTIVE;
        if(H5P_set (dx_plist, H5D_XFER_IO_XFER_MODE_NAME, &xfer_mode) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set transfer mode");
    } /* end if */
#endif /*H5_HAVE_PARALLEL*/

    assert(dx_plist);
    if (tconv_buf && NULL==H5P_peek_voidp(dx_plist,H5D_XFER_TCONV_BUF_NAME))
        H5FL_BLK_FREE(type_conv,tconv_buf);
    if (bkg_buf && NULL==H5P_peek_voidp(dx_plist,H5D_XFER_BKGR_BUF_NAME))
        H5FL_BLK_FREE(type_conv,bkg_buf);
        
    /* Release selection iterators, if necessary */
    if(file_iter_init)
        H5S_select_iter_release(&file_iter);
    if(mem_iter_init)
        H5S_select_iter_release(&mem_iter);
    if(bkg_iter_init)
        H5S_select_iter_release(&bkg_iter);

    /* Close file space and memory space for each chunk*/
    for(h=0; h<fm.nchunks; h++) {
        if(fm.mspace[h])
            H5S_close(fm.mspace[h]);
        if(fm.fspace[h])
            H5S_close(fm.fspace[h]);
    }       
    if(fm.chunk_coords)
        H5MM_free(fm.chunk_coords);
    if(fm.fspace)
        H5MM_free(fm.fspace);
    if(fm.mspace)
        H5MM_free(fm.mspace);   
      
    FUNC_LEAVE_NOAPI(ret_value);
} /* H5D_chunk_read() */


/*-------------------------------------------------------------------------
 * Function:	H5D_chunk_write
 *
 * Purpose:	Writes to a chunked dataset.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Thursday, April 10, 2003
 *
 * Modifications:
 *      QAK - 2003/04/17
 *      Hacked on it a lot. :-)
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_chunk_write(hsize_t nelmts, H5D_t *dataset, const H5T_t *mem_type, const H5S_t *mem_space,
    const H5S_t *file_space, H5T_path_t *tpath, H5S_conv_t *sconv, H5P_genplist_t *dc_plist,
    H5P_genplist_t *dx_plist, hid_t dxpl_id, hbool_t
#ifndef H5_HAVE_PARALLEL
    UNUSED
#endif /*H5_HAVE_PARALLEL*/
    doing_mpio, H5FD_mpio_xfer_t
#ifndef H5_HAVE_PARALLEL
    UNUSED
#endif /*H5_HAVE_PARALLEL*/
    xfer_mode, hid_t src_id, hid_t dst_id, const void *buf)
{
    fm_map      fm;                     /* File<->memory mapping */
    herr_t      status;                 /*function return status*/     
#ifdef H5S_DEBUG
    H5_timer_t	timer;
#endif
    size_t	src_type_size;		/*size of source type	*/
    size_t	dst_type_size;	        /*size of destination type*/
    size_t	target_size;		/*desired buffer size	*/
    hsize_t	request_nelmts;		/*requested strip mine	*/
    hsize_t     smine_start;            /*strip mine start loc  */
    hsize_t     n, smine_nelmts;        /*elements per strip    */    
    H5S_sel_iter_t mem_iter;            /*memory selection iteration info*/
    hbool_t	mem_iter_init=0;	/*memory selection iteration info has been initialized */
    H5S_sel_iter_t bkg_iter;            /*background iteration info*/
    hbool_t	bkg_iter_init=0;	/*background iteration info has been initialized */
    H5S_sel_iter_t file_iter;           /*file selection iteration info*/
    hbool_t	file_iter_init=0;	/*file selection iteration info has been initialized */
    H5T_bkg_t	need_bkg;		/*type of background buf*/
    uint8_t	*tconv_buf = NULL;	/*data type conv buffer	*/
    uint8_t	*bkg_buf = NULL;	/*background buffer	*/
    H5D_storage_t store;                /*union of EFL and chunk pointer in file space */
    hsize_t     h;                      /* Local index variable */
#ifdef H5_HAVE_PARALLEL
    hbool_t     xfer_mode_changed;      /* Whether the transfer mode was changed */
#endif /*H5_HAVE_PARALLEL*/
    herr_t	ret_value = SUCCEED;	/*return value		*/

    FUNC_ENTER_NOINIT(H5D_chunk_write);
    
    /* Initialize fm_map*/
    HDmemset(&fm, 0, sizeof(fm_map));
    fm.layout = &(dataset->layout);

    /* Map elements between file and memory for each chunk*/
    H5D_chunk_mem_file_map(dataset, mem_type, file_space, mem_space, &fm);
   
    /*
     * If there is no type conversion then write directly from the
     * application's buffer.  This saves at least one mem-to-mem copy.
     */
    if (H5T_path_noop(tpath)) {
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
        /*loop through each chunk, write data*/
        for(h=0; h<fm.nchunks; h++) {
            if(H5S_get_select_npoints(fm.fspace[h]) > 0) {
                /*pass in chunk's coordinates in a union.  LAYOUT puts datatype size as an extra dimension,
                 * have to take it out.*/
                store.chunk_coords = fm.chunk_coords + h*dataset->layout.ndims;

                status = (sconv->write)(dataset->ent.file, &(dataset->layout),
                        dc_plist, &store, H5T_get_size(dataset->type),
                        fm.fspace[h], fm.mspace[h], dxpl_id, buf);
            
                /* Check return value from optimized write */
                if (status<0)
                    HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "optimized write failed");
            }
        }   
        
#ifdef H5S_DEBUG
	H5_timer_end(&(sconv->stats[0].write_timer), &timer);
	sconv->stats[0].write_nbytes += nelmts * H5T_get_size(mem_type);
	sconv->stats[0].write_ncalls++;
#endif

	/* direct xfer accomplished successfully */
	HGOTO_DONE(SUCCEED);
    } /* end if */
   
    /*
     * This is the general case(type conversion).
     */
     
#ifdef H5_HAVE_PARALLEL
    H5D_io_assist_mpio(dx_plist, doing_mpio, xfer_mode, &xfer_mode_changed);
#endif /*H5_HAVE_PARALLEL*/
    
    /* Compute element sizes and other parameters */
    src_type_size = H5T_get_size(mem_type);
    dst_type_size = H5T_get_size(dataset->type);
    target_size = H5P_peek_size_t(dx_plist,H5D_XFER_MAX_TEMP_BUF_NAME);
    request_nelmts = target_size / MAX (src_type_size, dst_type_size);

    /* Sanity check elements in temporary buffer */
    if (request_nelmts<=0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "temporary buffer max size is too small");

    /*
     * Get a temporary buffer for type conversion unless the app has already
     * supplied one through the xfer properties.  Instead of allocating a
     * buffer which is the exact size, we allocate the target size. The
     * malloc() is usually less resource-intensive if we allocate/free the
     * same size over and over.
     */
    if (H5T_path_bkg(tpath)) {
        /* Retrieve the bkgr buffer property */
        if(H5P_get(dx_plist, H5D_XFER_BKGR_BUF_TYPE_NAME, &need_bkg)<0)
            HGOTO_ERROR (H5E_PLIST, H5E_CANTGET, FAIL, "Can't retrieve background buffer type");
        need_bkg = MAX (H5T_path_bkg(tpath), need_bkg);
    } else if(H5T_detect_class(dataset->type, H5T_VLEN)) {
	/* Old data is retrieved into background buffer for VL datatype.  The 
	 * data is used later for freeing heap objects. */
        need_bkg = H5T_BKG_YES;
    } else {
        need_bkg = H5T_BKG_NO; /*never needed even if app says yes*/
    } /* end else */
    if (NULL==(tconv_buf=H5P_peek_voidp(dx_plist,H5D_XFER_TCONV_BUF_NAME))) {
        /* Allocate temporary buffer */
        if((tconv_buf=H5FL_BLK_MALLOC(type_conv,target_size))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for type conversion");
    } /* end if */
    if (need_bkg && NULL==(bkg_buf=H5P_peek_voidp(dx_plist,H5D_XFER_BKGR_BUF_NAME))) {
        /* Allocate background buffer */
        H5_CHECK_OVERFLOW((request_nelmts*dst_type_size),hsize_t,size_t);
        if((bkg_buf=H5FL_BLK_CALLOC(type_conv,(size_t)(request_nelmts*dst_type_size)))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for background conversion");
    } /* end if */

    /* Loop over all the chunks, performing I/O on each */
    for(h=0; h<fm.nchunks; h++) {
        hsize_t chunk_nelmts;   /* Number of elements selected in current chunk */

        /* Get the number of elements selected in this chunk */
        chunk_nelmts=H5S_get_select_npoints(fm.fspace[h]);
        assert(chunk_nelmts<=nelmts);
        if(chunk_nelmts > 0) {
            
            /* initialize selection iterator */
            if (H5S_select_iter_init(&file_iter, fm.fspace[h], dst_type_size)<0)
                HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize file selection information");
            file_iter_init=1;	/*file selection iteration info has been initialized */
            if (H5S_select_iter_init(&mem_iter, fm.mspace[h], src_type_size)<0)
                HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize memory selection information");
            mem_iter_init=1;	/*file selection iteration info has been initialized */
            if (H5S_select_iter_init(&bkg_iter, fm.fspace[h], dst_type_size)<0)
                HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize background selection information");
            bkg_iter_init=1;	/*file selection iteration info has been initialized */
            
            /*pass in chunk's coordinates in a union*/
            store.chunk_coords = fm.chunk_coords + h*dataset->layout.ndims;
           
            for (smine_start=0; smine_start<chunk_nelmts; smine_start+=smine_nelmts) {
                /* Go figure out how many elements to read from the file */
                assert(H5S_select_iter_nelmts(&file_iter)==(chunk_nelmts-smine_start));
                smine_nelmts = MIN(request_nelmts, (chunk_nelmts-smine_start));
                
                /*
                 * Gather the data from disk into the data type conversion
                 * buffer. Also gather data from application to background buffer
                 * if necessary.
                 */
#ifdef H5S_DEBUG
                H5_timer_begin(&timer);
#endif
                n = H5S_select_mgath(buf, src_type_size, fm.mspace[h], &mem_iter,
                                     smine_nelmts, dxpl_id, tconv_buf/*out*/);
        
#ifdef H5S_DEBUG
                H5_timer_end(&(sconv->stats[1].gath_timer), &timer);
                sconv->stats[1].gath_nbytes += n * src_type_size;
                sconv->stats[1].gath_ncalls++;
#endif
                if (n!=smine_nelmts)
                    HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "file gather failed");

                if (need_bkg) {
#ifdef H5S_DEBUG
                    H5_timer_begin(&timer);
#endif
                    n = H5S_select_fgath(dataset->ent.file, &(dataset->layout), 
                            dc_plist, &store, dst_type_size, fm.fspace[h], 
                            &bkg_iter, smine_nelmts, dxpl_id, bkg_buf/*out*/); 

#ifdef H5S_DEBUG
                    H5_timer_end(&(sconv->stats[0].bkg_timer), &timer);
                    sconv->stats[0].bkg_nbytes += n * dst_type_size;
                    sconv->stats[0].bkg_ncalls++;
#endif
                    if (n!=smine_nelmts)
                        HGOTO_ERROR (H5E_IO, H5E_WRITEERROR, FAIL, "file gather failed");
                } /* end if */
	
                /*
                 * Perform data type conversion.
                 */
                if (H5T_convert(tpath, src_id, dst_id, smine_nelmts, 0, 0, 
                        tconv_buf, bkg_buf, dxpl_id)<0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "data type conversion failed");

                /*
                 * Scatter the data out to the file.
                 */
#ifdef H5S_DEBUG
                H5_timer_begin(&timer);
#endif
                status = H5S_select_fscat(dataset->ent.file, &(dataset->layout), 
                            dc_plist, &store, dst_type_size, fm.fspace[h], 
                            &file_iter, smine_nelmts, dxpl_id, tconv_buf);

#ifdef H5S_DEBUG
                H5_timer_end(&(sconv->stats[0].scat_timer), &timer);
                sconv->stats[0].scat_nbytes += n * dst_type_size;
                sconv->stats[0].scat_ncalls++;
#endif
                if (status<0)
                    HGOTO_ERROR (H5E_IO, H5E_WRITEERROR, FAIL, "scatter failed");
            } /* end for */

            /* Release selection iterators */
            if(file_iter_init) {
                H5S_select_iter_release(&file_iter);
                file_iter_init=0;
            } /* end if */
            if(mem_iter_init) {
                H5S_select_iter_release(&mem_iter);
                mem_iter_init=0;
            } /* end if */
            if(bkg_iter_init) {
                H5S_select_iter_release(&bkg_iter);
                bkg_iter_init=0;
            } /* end if */
        } /* end if */
    } /* end for */

done:
#ifdef H5_HAVE_PARALLEL
    /* restore xfer_mode due to the kludge */
    if (doing_mpio && xfer_mode_changed) {
#ifdef H5D_DEBUG
	if (H5DEBUG(D))
	    fprintf (H5DEBUG(D), "H5D: dx->xfer_mode was COLLECTIVE, restored to INDEPENDENT\n");
#endif
	xfer_mode = H5FD_MPIO_COLLECTIVE;
        if(H5P_set (dx_plist, H5D_XFER_IO_XFER_MODE_NAME, &xfer_mode) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set transfer mode");
    } /* end if */
#endif /*H5_HAVE_PARALLEL*/

    assert(dx_plist);
    if (tconv_buf && NULL==H5P_peek_voidp(dx_plist,H5D_XFER_TCONV_BUF_NAME))
        H5FL_BLK_FREE(type_conv,tconv_buf);
    if (bkg_buf && NULL==H5P_peek_voidp(dx_plist,H5D_XFER_BKGR_BUF_NAME))
        H5FL_BLK_FREE(type_conv,bkg_buf);
        
    /* Release selection iterators, if necessary */
    if(file_iter_init)
        H5S_select_iter_release(&file_iter);
    if(mem_iter_init)
        H5S_select_iter_release(&mem_iter);
    if(bkg_iter_init)
        H5S_select_iter_release(&bkg_iter);
 
    /* Close file space and memory space for each chunk*/
    for(h=0; h<fm.nchunks; h++) {
        if(fm.mspace[h])
            H5S_close(fm.mspace[h]);
        if(fm.fspace[h])    
            H5S_close(fm.fspace[h]);
    }
    if(fm.chunk_coords)
        H5MM_free(fm.chunk_coords);
    if(fm.fspace)
        H5MM_free(fm.fspace);
    if(fm.mspace)
        H5MM_free(fm.mspace);

    FUNC_LEAVE_NOAPI(ret_value);
} /* H5D_chunk_write() */    

#ifdef H5_HAVE_PARALLEL

/*-------------------------------------------------------------------------
 * Function:	H5D_io_assist_mpio
 *
 * Purpose:	Common logic for determining if the MPI transfer mode should
 *              be changed.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Thursday, April 10, 2003
 *
 * Modifications:
 *      QAK - 2003/04/17
 *      Hacked on it a lot. :-)
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5D_io_assist_mpio(H5P_genplist_t *dx_plist, hbool_t doing_mpio, H5FD_mpio_xfer_t xfer_mode, 
            hbool_t *xfer_mode_changed)
{ 
    herr_t	ret_value = SUCCEED;	/*return value		*/

    FUNC_ENTER_NOINIT(H5D_io_assist_mpio);
    
    /* The following may not handle a collective call correctly
     * since it does not ensure all processes can handle the write
     * request according to the MPI collective specification.
     * Do the collective request via independent mode.
     */
    if (doing_mpio && xfer_mode==H5FD_MPIO_COLLECTIVE) {
	/* Kludge: change the xfer_mode to independent, handle the request,
	 * then xfer_mode before return.
	 * Better way is to get a temporary data_xfer property with
	 * INDEPENDENT xfer_mode and pass it downwards.
	 */
	xfer_mode = H5FD_MPIO_INDEPENDENT;
        if(H5P_set (dx_plist, H5D_XFER_IO_XFER_MODE_NAME, &xfer_mode) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set transfer mode");
	*xfer_mode_changed=TRUE;	/* restore it before return */
#ifdef H5D_DEBUG
	if (H5DEBUG(D))
	    fprintf(H5DEBUG(D), "H5D: Cannot handle this COLLECTIVE write request.  Do it via INDEPENDENT calls\n");
#endif
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value);
}
#endif /*H5_HAVE_PARALLEL*/


/*-------------------------------------------------------------------------
 * Function:	H5D_chunk_coords_assist
 *
 * Purpose:	Compute the coords for a particular chunk (in CHUNK_PTR),
 *              based on the size of the dataset's dataspace (given in
 *              NDIMS and CHUNKS), putting the resulting chunk's coordinate
 *              offsets in the COORDS array.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Thursday, April 10, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5D_chunk_coords_assist(hssize_t *coords, size_t ndims, hsize_t chunks[], hsize_t chunk_ptr)
{
    hsize_t tmp;                /* Size of "down elements" in each dimension */
    size_t i, j;                /* Local index variables */

    FUNC_ENTER_NOINIT(H5D_chunk_coords_assist);

    for(i=0; i<ndims; i++) {
        tmp=1;
        for(j=i+1; j<ndims; j++) 
            tmp *= chunks[j];    
        coords[i] = (hssize_t)(chunk_ptr / tmp);
        chunk_ptr = chunk_ptr % tmp;
    }
    coords[ndims] = 0;

    FUNC_LEAVE_NOAPI(SUCCEED);
}
        

/*-------------------------------------------------------------------------
 * Function:	H5D_chunk_mem_file_map
 *
 * Purpose:	Creates the mapping between elements selected in each chunk
 *              and the elements in the memory selection.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Thursday, April 10, 2003
 *
 * Modifications:
 *      QAK - 2003/04/17
 *      Hacked on it a lot. :-)
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_chunk_mem_file_map(H5D_t *dataset, const H5T_t *mem_type, const H5S_t *file_space, 
            const H5S_t *mem_space, fm_map *fm)
{
    H5S_t *tmp_fspace=NULL,     /* Temporary file dataspace */
        *tmp_mspace=NULL;       /* Temporary memory dataspace */
    hid_t f_tid=(-1);           /* Temporary copy of file datatype for iteration */
    size_t elmt_size;           /* Memory datatype size */
    hbool_t iter_init=0;        /* Selection iteration info has been initialized */
    unsigned f_ndims;           /* The number of dimensions of the file's dataspace */
    int sm_ndims;               /* The number of dimensions of the memory buffer's dataspace (signed) */
    unsigned m_ndims;           /* The number of dimensions of the memory buffer's dataspace */
    hsize_t f_dims[H5O_LAYOUT_NDIMS];   /* Dimensionality of file dataspace */
    char bogus;                         /* "bogus" buffer to pass to selection iterator */
    unsigned u;                         /* Local index variable */
    hsize_t j;                          /* Local index variable */
    herr_t	ret_value = SUCCEED;	/*return value		*/
     
    FUNC_ENTER_NOINIT(H5D_chunk_mem_file_map);
    
    /* Create a file space of a chunk's size, instead of whole file space*/
    if(NULL==(tmp_fspace=H5S_create_simple((dataset->layout.ndims-1),dataset->layout.dim,NULL)))
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTCREATE, FAIL, "can't create simple dataspace");

    /*make a copy of mem_space*/
    if((tmp_mspace = H5S_copy(mem_space))==NULL)
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTCOPY, FAIL, "unable to copy memory space");
 
    /*de-select the file space and mem space copies*/
    if(H5S_select_none(tmp_fspace)<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to de-select file space");
    if(H5S_select_none(tmp_mspace)<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to de-select memory space");

    /* Get dim number and dimensionality for each dataspace */
    f_ndims=dataset->layout.ndims-1;
    if((sm_ndims = H5S_get_simple_extent_ndims(tmp_mspace))<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTGET, FAIL, "unable to get dimension number");
    fm->m_ndims=m_ndims=sm_ndims;

    if(H5S_get_simple_extent_dims(file_space, f_dims, NULL)<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTGET, FAIL, "unable to get dimensionality");

    /* Decide the number of chunks in each dimension*/
    fm->nchunks = 1;
    for(u=0; u<f_ndims; u++) {
        fm->chunks[u] = ((f_dims[u]+dataset->layout.dim[u])-1) / dataset->layout.dim[u];
        fm->nchunks *= fm->chunks[u];
    }
        
    /* Compute the "down" size of 'chunks' information */
    if(H5V_array_down(f_ndims,fm->chunks,fm->down_chunks)<0)
        HGOTO_ERROR (H5E_INTERNAL, H5E_BADVALUE, FAIL, "can't compute 'down' sizes");

    /* Allocate arrays to hold each chunk's dataspace & selection, for both the
     * file and memory */
    H5_CHECK_OVERFLOW((sizeof(H5S_t*)*fm->nchunks),hsize_t,size_t);
    fm->fspace = (H5S_t**)H5MM_malloc((size_t)(sizeof(H5S_t*)*fm->nchunks));
    fm->mspace = (H5S_t**)H5MM_malloc((size_t)(sizeof(H5S_t*)*fm->nchunks));
    fm->elmt_count = 0;

    /* Allocate array to hold coordinates of each chunk in file's dataspace */
    H5_CHECK_OVERFLOW((sizeof(hsize_t)*fm->nchunks*(f_ndims+1)),hsize_t,size_t);
    fm->chunk_coords = (hssize_t*)H5MM_malloc((size_t)(sizeof(hsize_t)*fm->nchunks*(f_ndims+1)));
    
    /* Make per-chunk copies of memory and chunk dataspaces
     * (with each one's selection set to "none"
     */
    for(j=0; j<fm->nchunks; j++) {
        if((fm->mspace[j] = H5S_copy(tmp_mspace))==NULL)
            HGOTO_ERROR (H5E_DATASPACE, H5E_CANTCOPY, FAIL, "unable to copy memory space");
        if((fm->fspace[j] = H5S_copy(tmp_fspace))==NULL)
            HGOTO_ERROR (H5E_DATASPACE, H5E_CANTCOPY, FAIL, "unable to copy file space");
      
        /* Initialize chunk_coords for each chunk. each stride is the number of
         * file space dimensions, with fast-growing dimension at the last
         */
        H5D_chunk_coords_assist(fm->chunk_coords+j*(f_ndims+1), f_ndims, fm->chunks, j);
    }
   
    /* Create temporary datatypes for selection iteration */
    if((f_tid = H5I_register(H5I_DATATYPE, H5T_copy(dataset->type, H5T_COPY_ALL)))<0)
        HGOTO_ERROR (H5E_DATATYPE, H5E_CANTREGISTER, FAIL, "unable to register file datatype");
    
    /* Create selection iterator for memory selection */
    if((elmt_size=H5T_get_size(mem_type))==0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADSIZE, FAIL, "datatype size invalid");
    if (H5S_select_iter_init(&(fm->mem_iter), mem_space, elmt_size)<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to initialize selection iterator");
    iter_init=1;	/* Selection iteration info has been initialized */

    /* Build the file & memory selection for each chunk */
    if(H5S_select_iterate(&bogus, f_tid, file_space,  H5D_chunk_cb, fm)<0)
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "unable to iterate file space");

    /* Clean hyperslab span "scratch" information */
    for(j=0; j<fm->nchunks; j++) {
        if(H5S_hyper_reset_scratch(fm->fspace[j])<0)
            HGOTO_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "unable to reset span scratch info");
        if(H5S_hyper_reset_scratch(fm->mspace[j])<0)
            HGOTO_ERROR (H5E_DATASET, H5E_CANTFREE, FAIL, "unable to reset span scratch info");
    } /* end for */

done:
    if(iter_init) {
        if (H5S_select_iter_release(&(fm->mem_iter))<0)
            HDONE_ERROR (H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release selection iterator");
    }
    if(tmp_fspace)
        H5S_close(tmp_fspace);
    if(tmp_mspace)
        H5S_close(tmp_mspace);
    if(f_tid!=(-1))
        H5Tclose(f_tid);
   
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5D_chunk_mem_file_map() */


/*-------------------------------------------------------------------------
 * Function:	H5D_chunk_cb
 *
 * Purpose:	Callback routine for file selection iterator.  Used when
 *              creating selections in memory and each chunk for each chunk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		Thursday, April 10, 2003
 *
 * Modifications:
 *      QAK - 2003/04/17
 *      Hacked on it a lot. :-)
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5D_chunk_cb(void UNUSED *elem, hid_t UNUSED type_id, hsize_t ndims, hssize_t *coords, void *_fm)
{
    fm_map      *fm = (fm_map*)_fm;             /* File<->memory chunk mapping info */
    hssize_t    coords_in_chunk[H5O_LAYOUT_NDIMS];      /* Coordinates of element in chunk */
    hssize_t    coords_in_mem[H5O_LAYOUT_NDIMS];        /* Coordinates of element in memory */
    hsize_t     chunk_idx;                      /* Chunk index */
    hsize_t     u;                              /* Local index variables */
    herr_t	ret_value = SUCCEED;            /* Return value		*/
    
    FUNC_ENTER_NOINIT(H5D_chunk_cb);
#ifdef QAK
{
    unsigned u;
    HDfprintf(stderr,"%s: coords={",FUNC);
    for(u=0; u<ndims; u++)
        HDfprintf(stderr,"%Hd%s",coords[u],(u<(ndims-1)?", ":"}\n"));
}
#endif /* QAK */

    /* Calculate the index of this chunk */
    if(H5V_chunk_index((unsigned)ndims,coords,fm->layout->dim,fm->chunks,fm->down_chunks,&chunk_idx)<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_BADRANGE, FAIL, "can't get chunk index");
#ifdef QAK
HDfprintf(stderr,"%s: chunk_idx=%Hu\n",FUNC,chunk_idx);
#endif /* QAK */

    /*convert coords from relative to whole file space to relative to its chunk space, 
     *pass into H5S_select_hyperslab.*/
    for(u=0; u<ndims; u++) 
        coords_in_chunk[u] = coords[u] % fm->layout->dim[u];
    
#ifdef QAK
{
    unsigned u;
    HDfprintf(stderr,"%s: coords_in_chunk={",FUNC);
    for(u=0; u<ndims; u++)
        HDfprintf(stderr,"%Hd%s",coords_in_chunk[u],(u<(ndims-1)?", ":"}\n"));
}
#endif /* QAK */
    /* Add point to file selection for chunk */
    if(H5S_hyper_add_span_element(fm->fspace[chunk_idx], (unsigned)ndims, coords_in_chunk)<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTSELECT, FAIL, "unable to select element");

    /* Get coordinates of selection iterator for memory */
    if(H5S_select_iter_coords(&fm->mem_iter,coords_in_mem)<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTGET, FAIL, "unable to get iterator coordinates");

#ifdef QAK
{
    unsigned u;
    HDfprintf(stderr,"%s: coords_in_mem={",FUNC);
    for(u=0; u<fm->m_ndims; u++)
        HDfprintf(stderr,"%Hd%s",coords_in_mem[u],(u<(fm->m_ndims-1)?", ":"}\n"));
}
#endif /* QAK */
    /* Add point to memory selection for chunk */
    if(H5S_hyper_add_span_element(fm->mspace[chunk_idx], fm->m_ndims, coords_in_mem)<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTSELECT, FAIL, "unable to select element");

    /* Move memory selection iterator to next element in selection */
    if(H5S_select_iter_next(&fm->mem_iter,1)<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTNEXT, FAIL, "unable to move to next iterator location");

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5D_chunk_cb() */
