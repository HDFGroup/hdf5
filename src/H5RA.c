/*
 * Copyright © 1998 NCSA
 *                  All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Tuesday, August 25, 1998
 *
 * Purpose:	A ragged array package layered on top of other HDF5 objects.
 *		This was originally implemented on the HDF5 API but has been
 *		moved under the API so it meshes better with other objects,
 *		is somewhat faster, and has better debugging support.
 *
 * Note:	This file implementes a two dimensional array where each row
 *		of the array is a different length (a.k.a., ragged array). It
 *		is intended for applications where the distribution of row
 *		lengths is such that most rows near an average length with
 *		only a few rows that are significantly shorter or longer. The
 *		raw data is split among two datasets `raw' and `over': the
 *		`raw' dataset is a 2d chunked dataset whose width is large
 *		enough to hold most of the rows and the `over' dataset is a
 *		heap that stores the end of rows that overflow the first
 *		dataset.  A third dataset called `meta' contains one record
 *		for each row and describes what elements (if any) overflow
 *		the `raw' dataset and where they are stored in the `over'
 *		dataset.  All three datasets are contained in a single group
 *		whose name is the name of the ragged array.
 */
#include <H5RAprivate.h>

#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Iprivate.h>
#include <H5MMprivate.h>
#include <H5Pprivate.h>

typedef struct H5RA_meta_t {
    hsize_t	nelmts;		/*num elements in row			*/
    hssize_t	offset;		/*offset into overflow array		*/
    hsize_t	nover;		/*allocated size in overflow array	*/
} H5RA_meta_t;

struct H5RA_t {
    H5G_t	*group;		/*the group containing everything	*/
    H5D_t	*meta;		/*ragged meta data array		*/
    H5D_t	*raw;		/*fixed-width raw data			*/
    H5D_t	*over;		/*overflow data				*/
};

#define PABLO_MASK	H5RA_mask
static intn		interface_initialize_g = 0;
#define INTERFACE_INIT H5RA_init_interface
static herr_t H5RA_init_interface(void);
static H5T_t *H5RA_meta_type_g = NULL;

static herr_t H5RA_fix_overflow(H5RA_t *ra, H5T_t *type, H5RA_meta_t *meta,
			       hsize_t nelmts, void *buf);


/*-------------------------------------------------------------------------
 * Function:	H5RA_init_interface
 *
 * Purpose:	Initialize the interface.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Tuesday, August 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5RA_init_interface(void)
{
    H5T_t	*type = NULL;
    
    FUNC_ENTER(H5RA_init_interface, FAIL);

    /* The atom group */
    if (H5I_init_group(H5I_RAGGED, H5I_RAGGED_HASHSIZE, 0,
		       (H5I_free_t)H5RA_close)<0) {
	HRETURN_ERROR (H5E_RAGGED, H5E_CANTINIT, FAIL,
		       "unable to initialize interface");
    }

    /* The meta dataset type */
    if (NULL==(type=H5T_create(H5T_COMPOUND, sizeof(H5RA_meta_t))) ||
	H5T_struct_insert(type, "nelmts", HOFFSET(H5RA_meta_t, nelmts),
			  0, NULL, NULL, H5I_object(H5T_NATIVE_HSIZE_g))<0 ||
	H5T_struct_insert(type, "offset", HOFFSET(H5RA_meta_t, offset),
			  0, NULL, NULL, H5I_object(H5T_NATIVE_HSSIZE_g))<0 ||
	H5T_struct_insert(type, "nover", HOFFSET(H5RA_meta_t, nover),
			  0, NULL, NULL, H5I_object(H5T_NATIVE_HSIZE_g))) {
	HRETURN_ERROR(H5E_RAGGED, H5E_CANTINIT, FAIL,
		      "unable to define ragged array meta type");
    }
    H5RA_meta_type_g = type;
    
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5RA_term_interface
 *
 * Purpose:	Terminate the ragged array interface.
 *
 * Return:	Success:	Positive if anything was done that might
 *				affect some other interface. Zero otherwise.
 *
 * 		Failure:	Negaitive
 *
 * Programmer:	Robb Matzke
 *              Tuesday, August 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5RA_term_interface(void)
{
    intn	n=0;

    if (interface_initialize_g) {
	if ((n=H5I_nmembers(H5I_RAGGED))) {
	    H5I_clear_group(H5I_RAGGED, FALSE);
	} else {
	    H5T_close(H5RA_meta_type_g);
	    H5RA_meta_type_g = NULL;
	    H5I_destroy_group(H5I_RAGGED);
	    interface_initialize_g = 0;
	    n = 1; /*H5T,H5I*/
	}
    }
    return n;
}


/*-------------------------------------------------------------------------
 * Function:	H5RAcreate
 *
 * Purpose:	Create a new ragged array with the specified name.  A ragged
 *		array is implemented as a group containing three datasets.
 *		The dataset `raw' is a fixed width dataset which will hold
 *		the majority of the data.  The dataset `over' is a one
 *		dimensional heap which will hold the end of rows which are
 *		too long to fit in `raw'.  Finally, the `meta' dataset
 *		contains information about the `over' array.  All elements of
 *		the ragged array are stored with the same data type.
 *
 *		The property list PLIST_ID  should contain information about
 *		chunking. The chunk width will determine the width of the
 *		`raw' dataset while the chunk length should be such that the
 *		total chunk size is reasonably large (I/O will be performed
 *		in units of chunks).  If the PLIST_ID doesn't have a chunk
 *		size defined (e.g., H5P_DEFAULT) then this function will fail.
 *
 * Return:	Success:	A ragged array ID.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Tuesday, August 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5RAcreate(hid_t loc_id, const char *name, hid_t type_id, hid_t plist_id)
{
    H5RA_t		*ra=NULL;
    H5G_entry_t		*loc=NULL;
    H5T_t		*type=NULL;
    const H5D_create_t	*plist=NULL;
    hid_t		ret_value=FAIL;
    
    FUNC_ENTER(H5RAcreate, FAIL);
    H5TRACE4("i","isii",loc_id,name,type_id,plist_id);

    /* Check args */
    if (NULL==(loc=H5G_loc(loc_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location");
    }
    if (!name || !*name) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name given");
    }
    if (H5I_DATATYPE!=H5I_get_type(type_id) ||
	NULL==(type=H5I_object(type_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    }
    if (H5P_DEFAULT==plist_id) {
	plist = &H5D_create_dflt;
    } else if (H5P_DATASET_CREATE!=H5P_get_class(plist_id) ||
	       NULL==(plist=H5I_object(plist_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a dataset creation property list");
    }
    if (H5D_CHUNKED!=plist->layout) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		      "property list must define a chunked storage layout");
    }
    if (2!=plist->layout) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		      "chunked storage is not two dimensional");
    }
    
    /* Do the real work */
    if (NULL==(ra=H5RA_create(loc, name, type, plist))) {
	HRETURN_ERROR(H5E_RAGGED, H5E_CANTINIT, FAIL,
		      "unable to create ragged array");
    }

    /* Register the new dataset to get an ID for it */
    if ((ret_value=H5I_register(H5I_RAGGED, ra))<0) {
	H5RA_close(ra);
	HRETURN_ERROR(H5E_RAGGED, H5E_CANTREGISTER, FAIL,
		      "unable to register ragged array");
    }

    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5RA_create
 *
 * Purpose:	Create a new ragged array implemented as a group.
 *
 * Return:	Success:	Pointer to the new ragged array.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, August 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5RA_t *
H5RA_create(H5G_entry_t *loc, const char *name, H5T_t *type,
	   const H5D_create_t *dcpl)
{
    H5RA_t		*ra = NULL;
    H5S_t		*space = NULL;
    hsize_t		cur_dims[2];
    hsize_t		max_dims[2];
    H5RA_t		*ret_value=NULL;
    H5D_create_t	d1_dcpl;
    
    FUNC_ENTER(H5RA_create, NULL);

    /* Check args */
    assert(loc);
    assert(name && *name);
    assert(type);
    assert(dcpl);
    assert(H5D_CHUNKED==dcpl->layout);

    /* Create the data struct */
    if (NULL==(ra=H5MM_calloc(sizeof(H5RA_t)))) {
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL,
		    "memory allocation failed for ragged array struct");
    }

    /* Create the group to contain the arrays */
    if (NULL==(ra->group=H5G_create(loc, name, 0))) {
	HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, NULL,
		    "unable to create container group");
    }

    /* The raw data array */
    cur_dims[0] = 0;
    max_dims[0] = H5S_UNLIMITED;
    cur_dims[1] = max_dims[1] = dcpl->chunk_size[1];
    if (NULL==(space=H5S_create(H5S_SIMPLE)) ||
	H5S_set_extent_simple(space, 2, cur_dims, max_dims)<0) {
	HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, NULL,
		    "unable to define raw dataset extents");
    }
    if (NULL==(ra->raw=H5D_create(H5G_entof(ra->group), "raw", type, space,
				  dcpl))) {
	HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, NULL,
		    "unable to create raw dataset");
    }
    if (H5S_close(space)<0) {
	space = NULL;
	HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, NULL,
		    "unable to close raw dataset extents");
    }
    space = NULL;
    
    /* The overflow data array */
    cur_dims[0] = 0;
    max_dims[0] = H5S_UNLIMITED;
    if (NULL==(space=H5S_create(H5S_SIMPLE)) ||
	H5S_set_extent_simple(space, 1, cur_dims, max_dims)<0) {
	HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, NULL,
		    "unable to define overflow dataset extents");
    }
    d1_dcpl = *dcpl;
    d1_dcpl.chunk_ndims = 1;
    d1_dcpl.chunk_size[0] = dcpl->chunk_size[0]*dcpl->chunk_size[1];
    if (NULL==(ra->over=H5D_create(H5G_entof(ra->group), "over", type, space,
				   &d1_dcpl))) {
	HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, NULL,
		    "unable to create overflow dataset");
    }
    if (H5S_close(space)<0) {
	space = NULL;
	HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, NULL,
		    "unable to close overflow dataset extents");
    }
    space = NULL;
    
    /* The meta data array */
    cur_dims[0] = 0;
    max_dims[0] = H5S_UNLIMITED;
    if (NULL==(space=H5S_create(H5S_SIMPLE)) ||
	H5S_set_extent_simple(space, 1, cur_dims, max_dims)<0) {
	HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, NULL,
		    "unable to define meta dataset extents");
    }
    d1_dcpl.chunk_size[0] = MAX(1,
				(dcpl->chunk_size[0]*dcpl->chunk_size[1]*
				 H5T_get_size(type))/
				H5T_get_size(H5RA_meta_type_g));
    if (NULL==(ra->meta=H5D_create(H5G_entof(ra->group), "meta",
				   H5RA_meta_type_g, space, &d1_dcpl))) {
	HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, NULL,
		    "unable to create meta dataset");
    }
    if (H5S_close(space)<0) {
	space = NULL;
	HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, NULL,
		    "unable to close meta dataset extents");
    }
    space = NULL;

    ret_value = ra;

 done:
    if (!ret_value) {
	if (space) H5S_close(space);
	if (ra) {
	    if (ra->group) H5G_close(ra->group);
	    if (ra->raw) H5D_close(ra->raw);
	    if (ra->over) H5D_close(ra->over);
	    if (ra->meta) H5D_close(ra->meta);
	    H5MM_xfree(ra);
	}
    }
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5RAopen
 *
 * Purpose:	Opens an existing ragged array.  The name of the array should
 *		be the same that was used when the array was created; that is,
 *		the name of the group which implements the array.
 *		
 * Return:	Success:	An array ID
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Tuesday, August 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5RAopen(hid_t loc_id, const char *name)
{
    H5G_entry_t	*loc=NULL;
    H5RA_t	*ra=NULL;
    hid_t	ret_value=FAIL;
    
    FUNC_ENTER(H5RAopen, FAIL);
    H5TRACE2("i","is",loc_id,name);

    /* Check args */
    if (NULL==(loc=H5G_loc(loc_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location");
    }
    if (!name || !*name) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name given");
    }

    /* The real work */
    if (NULL==(ra=H5RA_open(loc, name))) {
	HRETURN_ERROR(H5E_RAGGED, H5E_CANTOPENOBJ, FAIL,
		      "unable to open ragged array");
    }

    /* Turn it into an atom */
    if ((ret_value=H5I_register(H5I_RAGGED, ra))<0) {
	H5RA_close(ra);
	HRETURN_ERROR(H5E_RAGGED, H5E_CANTREGISTER, FAIL,
		      "unable to register ragged array");
    }

    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5RA_isa
 *
 * Purpose:	Determines if an object is a ragged array.
 *
 * Return:	Success:	TRUE if the object is a ragged array; FALSE
 *				otherwise.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Wednesday, November  4, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5RA_isa(H5G_entry_t *ent)
{
    htri_t	exists;
    H5G_entry_t	d_ent;
    
    FUNC_ENTER(H5RA_isa, FAIL);

    /* Open the container group */
    if ((exists=H5G_isa(ent))<0) {
	HRETURN_ERROR(H5E_RAGGED, H5E_CANTINIT, FAIL,
		      "unable to read object header");
    } else if (!exists) {
	HRETURN(FALSE);
    }

    /* Is `raw' a dataset? */
    if (H5G_find(ent, "raw", NULL, &d_ent)<0) HRETURN(FALSE);
    if ((exists=H5D_isa(&d_ent))<0) {
	HRETURN_ERROR(H5E_DATASET, H5E_NOTFOUND, FAIL, "not found");
    } else if (!exists) {
	HRETURN(FALSE);
    }

    /* Is `over' a dataset? */
    if (H5G_find(ent, "over", NULL, &d_ent)<0 ||
	(exists=H5D_isa(&d_ent))<0) {
	HRETURN_ERROR(H5E_DATASET, H5E_NOTFOUND, FAIL, "not found");
    } else if (!exists) {
	HRETURN(FALSE);
    }

    /* Is `meta' a dataset? */
    if (H5G_find(ent, "meta", NULL, &d_ent)<0 ||
	(exists=H5D_isa(&d_ent))<0) {
	HRETURN_ERROR(H5E_DATASET, H5E_NOTFOUND, FAIL, "not found");
    } else if (!exists) {
	HRETURN(FALSE);
    }

    FUNC_LEAVE(TRUE);
}


/*-------------------------------------------------------------------------
 * Function:	H5RA_open
 *
 * Purpose:	Open a ragged array.  The name of the array is the same as
 *		the group that implements the array.
 *
 * Return:	Success:	Ptr to a new ragged array object.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, August 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5RA_t *
H5RA_open(H5G_entry_t *loc, const char *name)
{
    H5RA_t	*ra = NULL;
    H5RA_t	*ret_value = NULL;
    
    FUNC_ENTER(H5RA_open, NULL);

    /* Check arguments */
    assert(loc);
    assert(name && *name);

    /* Create the struct */
    if (NULL==(ra=H5MM_calloc(sizeof(H5RA_t)))) {
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL,
		    "memory allocation failed for ragged array struct");
    }

    /* Open the containing group */
    if (NULL==(ra->group=H5G_open(loc, name))) {
	HGOTO_ERROR(H5E_RAGGED, H5E_CANTOPENOBJ, NULL,
		    "unable to open container group");
    }

    /* Open the datasets */
    if (NULL==(ra->raw=H5D_open(H5G_entof(ra->group), "raw")) ||
	NULL==(ra->over=H5D_open(H5G_entof(ra->group), "over")) ||
	NULL==(ra->meta=H5D_open(H5G_entof(ra->group), "meta"))) {
	HGOTO_ERROR(H5E_RAGGED, H5E_CANTOPENOBJ, NULL,
		    "unable to open one or more component datasets");
    }

    ret_value = ra;
    
 done:
    if (!ret_value) {
	if (ra) {
	    if (ra->group) H5G_close(ra->group);
	    if (ra->raw) H5D_close(ra->raw);
	    if (ra->over) H5D_close(ra->over);
	    if (ra->meta) H5D_close(ra->meta);
	    H5MM_xfree(ra);
	}
    }
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5RAclose
 *
 * Purpose:	Close a ragged array.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Tuesday, August 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5RAclose(hid_t array_id)
{
    FUNC_ENTER(H5RAclose, FAIL);
    H5TRACE1("e","i",array_id);

    /* Check args */
    if (H5I_RAGGED!=H5I_get_type(array_id) ||
	NULL==H5I_object(array_id)) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a ragged array");
    }
    
    /*
     * Decrement the counter on the array.  It will be freed if the count
     * reaches zero.
     */
    if (H5I_dec_ref(array_id) < 0) {
	HRETURN_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to free");
    }
    
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5RA_close
 *
 * Purpose:	Close a ragged array
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Tuesday, August 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5RA_close(H5RA_t *ra)
{
    FUNC_ENTER(H5RA_close, FAIL);

    assert(ra);
    if ((ra->group && H5G_close(ra->group)<0) ||
	(ra->raw && H5D_close(ra->raw)<0) ||
	(ra->over && H5D_close(ra->over)<0) ||
	(ra->meta && H5D_close(ra->meta)<0)) {
	HRETURN_ERROR(H5E_RAGGED, H5E_CANTINIT, FAIL,
		      "unable to close one or more component datasets");
    }
    HDmemset(ra, 0, sizeof(H5RA_t));
    H5MM_xfree(ra);

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5RAwrite
 *
 * Purpose:	Write a contiguous set of rows to a ragged array beginning at
 *		row number START_ROW and continuing for NROWS rows.  Each row
 *		of the ragged array contains SIZE[] elements of type TYPE_ID
 *		and each row is stored in a buffer pointed to by BUF[].
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Tuesday, August 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5RAwrite(hid_t array_id, hssize_t start_row, hsize_t nrows,
	 hid_t type_id, hsize_t size[/*nrows*/], void *buf[/*nrows*/])
{
    H5RA_t	*ra=NULL;
    H5T_t	*type=NULL;
    hsize_t	i;
    
    FUNC_ENTER(H5RAwrite, FAIL);
    H5TRACE6("e","iHshi*[a2]h*[a2]x",array_id,start_row,nrows,type_id,size,
             buf);

    /* Check args */
    if (H5I_RAGGED!=H5I_get_type(array_id) ||
	NULL==(ra=H5I_object(array_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a ragged array");
    }
    if (H5I_DATATYPE!=H5I_get_type(type_id) ||
	NULL==(type=H5I_object(type_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    }
    if (nrows>0 && !size) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no size array");
    }
    if (nrows>0 && !buf) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer array");
    }
    for (i=0; i<nrows; i++) {
	if (size[i]>0 && !buf[i]) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
			  "one or more buffer points are null");
	}
    }
    
    /* Do the work */
    if (H5RA_write(ra, start_row, nrows, type, size, buf)<0) {
	HRETURN_ERROR(H5E_RAGGED, H5E_WRITEERROR, FAIL,
		      "unable to write to ragged array");
    }

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5RA_write
 *
 * Purpose:	Write a contiguous set of rows to a ragged array beginning at
 *		row number START_ROW and continuing for NROWS rows.  Each row
 *		of the ragged array contains SIZE[] elements of type TYPE_ID
 *		and each row is stored in a buffer pointed to by BUF[].
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Tuesday, August 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5RA_write(H5RA_t *ra, hssize_t start_row, hsize_t nrows, H5T_t *type,
	  hsize_t size[], void *buf[])
{
    herr_t	ret_value=FAIL;
    H5RA_meta_t	*meta = NULL;
    H5S_t	*mf_space=NULL;		/*meta file data space		*/
    H5S_t	*mm_space=NULL;		/*meta memory data space	*/
    H5S_t	*rf_space=NULL;		/*raw data file space		*/
    H5S_t	*rm_space=NULL;		/*raw data memory space		*/
    hsize_t	meta_cur_size;		/*current meta data nelmts	*/
    hsize_t	meta_read_size;		/*amount to read		*/
    hsize_t	raw_cur_size[2];	/*raw data current size		*/
    hssize_t	hs_offset[2];		/*hyperslab offset		*/
    hsize_t	hs_size[2];		/*hyperslab size		*/
    uint8_t	*raw_buf=NULL;		/*raw buffer			*/
    size_t	type_size;		/*size of the TYPE argument	*/
    hsize_t	i;
    
    FUNC_ENTER(H5RA_write, FAIL);

    /* Check args */
    assert(ra);
    assert(type);
    assert(0==nrows || size);
    assert(0==nrows || buf);
    if (0==nrows) HRETURN(SUCCEED);
    type_size = H5T_get_size(type);

    /* Get the meta data */
    if (NULL==(mf_space=H5D_get_space(ra->meta)) ||
	H5S_get_simple_extent_dims(mf_space, &meta_cur_size, NULL)<0) {
	HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, FAIL,
		    "unable to get current meta data extents");
    }
    if ((hsize_t)start_row>=meta_cur_size) {
	meta_read_size = 0;
    } else {
	meta_read_size = MIN(nrows, meta_cur_size-(hsize_t)start_row);
    }
    if (NULL==(mm_space=H5S_create(H5S_SIMPLE)) ||
	H5S_set_extent_simple(mm_space, 1, &meta_read_size, NULL)<0 ||
	H5S_select_hyperslab(mf_space, H5S_SELECT_SET, &start_row, NULL,
			     &meta_read_size, NULL)<0) {
	HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, FAIL,
		    "unable to set meta data selection");
    }
    if (NULL==(meta=H5MM_malloc(nrows*sizeof(H5RA_meta_t)))) {
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
		    "memory allocation failed for meta data");
    }
    if (H5D_read(ra->meta, H5RA_meta_type_g, mm_space, mf_space,
		 H5P_DEFAULT, meta)<0) {
	HGOTO_ERROR(H5E_RAGGED, H5E_READERROR, FAIL,
		    "unable to read meta data");
    }
    HDmemset(meta+meta_read_size, 0,
	     (nrows-meta_read_size)*sizeof(H5RA_meta_t));

    /* Write the part of the data that will fit in the raw dataset */
    if (NULL==(rf_space=H5D_get_space(ra->raw)) ||
	H5S_get_simple_extent_dims(rf_space, raw_cur_size, NULL)<0) {
	HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, FAIL,
		    "unable to determine current raw data extents");
    }
    if (NULL==(raw_buf=H5MM_malloc(nrows*raw_cur_size[1]*type_size))) {
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
		    "unable to allocate buffer for raw data");
    }
    for (i=0; i<nrows; i++) {
	size_t nbytes = MIN(size[i], raw_cur_size[1])*type_size;
	HDmemcpy(raw_buf+i*raw_cur_size[1]*type_size, buf[i], nbytes);
	HDmemset(raw_buf+i*raw_cur_size[1]*type_size+nbytes, 0,
		 raw_cur_size[1]*type_size-nbytes);
    }
    if ((hsize_t)start_row+nrows>raw_cur_size[0]) {
	raw_cur_size[0] = (hsize_t)start_row + nrows;
	if (H5D_extend(ra->raw, raw_cur_size)<0) {
	    HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, FAIL,
			"unable to extend raw dataset");
	}
    }
    hs_offset[0] = start_row;
    hs_offset[1] = 0;
    hs_size[0] = nrows;
    hs_size[1] = raw_cur_size[1];
    if (NULL==(rm_space=H5S_create(H5S_SIMPLE)) ||
	H5S_set_extent_simple(rm_space, 2, hs_size, NULL)<0 ||
	H5S_select_hyperslab(rf_space, H5S_SELECT_SET,
			     hs_offset, NULL, hs_size, NULL)<0) {
	HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, FAIL,
		    "unable to set meta data selection");
    }
    if (H5D_write(ra->raw, type, rm_space, rf_space, H5P_DEFAULT,
		  raw_buf)<0) {
	HGOTO_ERROR(H5E_RAGGED, H5E_WRITEERROR, FAIL,
		    "unable to write raw data");
    }
    
    /* Update the meta data */
    for (i=0; i<nrows; i++) {
	if (size[i]>raw_cur_size[1]) {
	    H5RA_fix_overflow(ra, type, meta+i, size[i]-raw_cur_size[1],
			     (uint8_t*)(buf[i])+raw_cur_size[1]*type_size);
	}
	meta[i].nelmts = size[i];
    }
    
    /* Extend and write the new meta data */
    if ((hsize_t)start_row+nrows>meta_cur_size) {
	meta_cur_size = (hsize_t)start_row+nrows;
	if (H5D_extend(ra->meta, &meta_cur_size)<0) {
	    HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, FAIL,
			"unable to extend meta dataset");
	}
    }
    if (H5S_set_extent_simple(mm_space, 1, &nrows, NULL)<0 ||
	H5S_select_hyperslab(mf_space, H5S_SELECT_SET, &start_row, NULL,
			     &nrows, NULL)<0) {
	HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, FAIL,
		    "unable to set meta data selection");
    }
    if (H5D_write(ra->meta, H5RA_meta_type_g, mm_space, mf_space,
		  H5P_DEFAULT, meta)<0) {
	HGOTO_ERROR(H5E_RAGGED, H5E_WRITEERROR, FAIL,
		    "unable to write meta data");
    }
    
    ret_value = SUCCEED;
 done:
    H5MM_xfree(meta);
    H5MM_xfree(raw_buf);
    if (mm_space) H5S_close(mm_space);
    if (mf_space) H5S_close(mf_space);
    if (rm_space) H5S_close(rm_space);
    if (rf_space) H5S_close(rf_space);
	
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5RA_fix_overflow
 *
 * Purpose:	Updates the overflow information for a row.  This is where
 *		the heap management comes into play.  The NELMTS is the
 *		number of elements that overflow into the heap and BUF is a
 *		pointer to those elements.  The first part of the row has
 *		already been written to the raw dataset.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August 26, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5RA_fix_overflow(H5RA_t *ra, H5T_t *type, H5RA_meta_t *meta, hsize_t nelmts,
		 void *buf)
{
    H5S_t	*of_space=NULL;		/*overflow file space		*/
    H5S_t	*om_space=NULL;		/*overflow memory space		*/
    hsize_t	cur_size;		/*num elmts in overflow dataset	*/
    herr_t	ret_value=FAIL;		/*return value			*/
    
    FUNC_ENTER(H5RA_fix_overflow, FAIL);
    
    assert(ra);
    assert(meta);
    assert(buf);

    if (nelmts==meta->nover) {
	/* The space is already allocated -- do nothing */
	
    } else if (nelmts<meta->nover) {
	/* Too much space is allocated -- free the extra */
#ifndef LATER
	/*
	 * For now we don't worry about reclaiming space.  But we remember
	 * the original amount allocated to allow the row to grow again up to
	 * that amount.
	 */
#endif
	
    } else if (meta->nover>0) {
	/* Space is allocated but we need more */
#ifndef LATER
	/*
	 * For now we'll just reallocate space at the end of the overflow
	 * dataset, but eventually we'll want to move the smallest of the
	 * current overflow, the previous overflow, or the following overflow
	 * region to the end of the file and be careful to reclaim space.
	 */
	if (NULL==(of_space=H5D_get_space(ra->over)) ||
	    H5S_get_simple_extent_dims(of_space, &cur_size, NULL)<0) {
	    HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, FAIL,
			"unable to get overflow dataset extents");
	}
	meta->offset = (hssize_t)cur_size;
	meta->nover = nelmts;
	cur_size += nelmts;
	if (H5D_extend(ra->over, &cur_size)<0) {
	    HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, FAIL,
			"unable to extend overflow dataset");
	}
#endif
	
    } else {
	/* No space is allocated */
	assert(nelmts>0);
	if (NULL==(of_space=H5D_get_space(ra->over)) ||
	    H5S_get_simple_extent_dims(of_space, &cur_size, NULL)<0) {
	    HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, FAIL,
			"unable to get overflow dataset extents");
	}
	meta->offset = (hssize_t)cur_size;
	meta->nover = nelmts;
	cur_size += nelmts;
	if (H5D_extend(ra->over, &cur_size)<0) {
	    HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, FAIL,
			"unable to extend overflow dataset");
	}
    }

    /* Write the data */
    if (nelmts>0) {
	if (!of_space && NULL==(of_space=H5D_get_space(ra->over))) {
	    HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, FAIL,
			"unable to get overflow dataset extents");
	}
	if (NULL==(om_space=H5S_create(H5S_SIMPLE)) ||
	    H5S_set_extent_simple(om_space, 1, &nelmts, NULL)<0 ||
	    H5S_select_hyperslab(of_space, H5S_SELECT_SET, &(meta->offset),
				 NULL, &nelmts, NULL)<0) {
	    HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, FAIL,
			"unable to set overflow selection");
	}
	if (H5D_write(ra->over, type, om_space, of_space, H5P_DEFAULT,
		      buf)<0) {
	    HGOTO_ERROR(H5E_RAGGED, H5E_WRITEERROR, FAIL,
			"unable to write to overflow dataset");
	}
    }
    
    ret_value = SUCCEED;
    
 done:
    if (of_space) H5S_close(of_space);
    if (om_space) H5S_close(om_space);
    if (ret_value<0) {
	meta->offset = 0;
	meta->nover = 0;
    }
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5RAread
 *
 * Purpose:	Reads the contents of one or more rows of a ragged array
 *		pointed to by ARRAY_ID.  The rows to read begin at row
 *		START_ROW and continue for NROWS rows with all raw data
 *		converted to type TYPE_ID.
 *
 *		The caller must allocate the SIZE[] and BUF[] arrays but
 *		memory for the data can be allocated by either the caller or
 *		the library.  In the former case the caller should initialize
 *		the BUF[] array with pointers to valid memory and the SIZE[]
 *		array with the lengths of the buffers.  In the latter case
 *		the caller should initialize BUF[] with null pointers (the
 *		input value of SIZE[] is irrelevant in this case) and the
 *		library will allocate memory for each row by calling malloc().
 *
 * Return:	Success:	Non-negative.  The values of the SIZE[] array will
 *				be the true length of each row.  If a row is
 *				longer than the caller-allocated length then
 *				SIZE[] will contain the true length of the
 *				row although not all elements of that row
 *				will be stored in the buffer.
 *
 *		Failure:	Negative.  The BUF[] array will contain it's
 *				original pointers (null or otherwise)
 *				although the caller-supplied buffers may have
 *				been modified. The SIZE[] array may also be
 *				modified.
 *
 * Programmer:	Robb Matzke
 *              Tuesday, August 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5RAread(hid_t array_id, hssize_t start_row, hsize_t nrows,
	hid_t type_id, hsize_t size[/*nrows*/], void *buf[/*nrows*/])
{
    H5RA_t	*ra=NULL;
    H5T_t	*type=NULL;
    
    FUNC_ENTER(H5RAread, FAIL);
    H5TRACE6("e","iHshi*[a2]h*[a2]x",array_id,start_row,nrows,type_id,size,
             buf);

    /* Check args */
    if (H5I_RAGGED!=H5I_get_type(array_id) ||
	NULL==(ra=H5I_object(array_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a ragged array");
    }
    if (H5I_DATATYPE!=H5I_get_type(type_id) ||
	NULL==(type=H5I_object(type_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    }
    if (nrows>0 && !size) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no size array");
    }
    if (nrows>0 && !buf) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer array");
    }
    
    /* Do the work */
    if (H5RA_read(ra, start_row, nrows, type, size, buf)<0) {
	HRETURN_ERROR(H5E_RAGGED, H5E_READERROR, FAIL,
		      "unable to read ragged array");
    }

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5RA_read
 *
 * Purpose:	Reads (part of) a ragged array.  See H5RAread() for details.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August 26, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5RA_read(H5RA_t *ra, hssize_t start_row, hsize_t nrows, H5T_t *type,
	 hsize_t size[], void *buf[])
{
    herr_t	ret_value=FAIL;
    H5RA_meta_t	*meta = NULL;
    H5S_t	*mf_space=NULL;		/*meta file data space		*/
    H5S_t	*mm_space=NULL;		/*meta memory data space	*/
    H5S_t	*rf_space=NULL;		/*raw data file space		*/
    H5S_t	*rm_space=NULL;		/*raw data memory space		*/
    H5S_t	*of_space=NULL;		/*overflow data file space	*/
    H5S_t	*om_space=NULL;		/*overflow data memory space	*/
    hsize_t	meta_cur_size;		/*current meta data nelmts	*/
    hsize_t	meta_read_size;		/*amount of meta data to read	*/
    hsize_t	raw_cur_size[2];	/*raw data current size		*/
    hsize_t	raw_read_size[2];	/*amount of raw data to read	*/
    hssize_t	hs_offset[2];		/*hyperslab offset		*/
    hsize_t	hs_size[2];		/*hyperslab size		*/
    uint8_t	*raw_buf=NULL;		/*raw buffer			*/
    size_t	type_size;		/*size of the TYPE argument	*/
    void	**buf_out=NULL;		/*output BUF values		*/
    hsize_t	i;			/*counter			*/

    FUNC_ENTER(H5RA_read, FAIL);

    /* Check args */
    assert(ra);
    assert(type);
    assert(0==nrows || size);
    assert(0==nrows || buf);
    if (0==nrows) HRETURN(SUCCEED);
    type_size = H5T_get_size(type);

    /*
     * Malloc `buf_out' to hold the output values for `buf'.  We have to do
     * this because if we return failure we want `buf' to have the original
     * values.
     */
    if (NULL==(buf_out=H5MM_calloc(nrows*sizeof(void*)))) {
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
		    "memory allocation failed for BUF output values");
    }
    
    /* Read from the raw dataset */
    if (NULL==(rf_space=H5D_get_space(ra->raw)) ||
	H5S_get_simple_extent_dims(rf_space, raw_cur_size, NULL)<0) {
	HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, FAIL,
		    "unable to determine current raw data extents");
    }
    if ((hsize_t)start_row>=raw_cur_size[0]) {
	raw_read_size[0] = 0;
	raw_read_size[1] = raw_cur_size[1];
    } else {
	raw_read_size[0] = MIN(nrows, raw_cur_size[0]-(hsize_t)start_row);
	raw_read_size[1] = raw_cur_size[1];
    }
    hs_offset[0] = start_row;
    hs_offset[1] = 0;
    if (NULL==(rm_space=H5S_create(H5S_SIMPLE)) ||
	H5S_set_extent_simple(rm_space, 2, raw_read_size, NULL)<0 ||
	H5S_select_hyperslab(rf_space, H5S_SELECT_SET, hs_offset, NULL,
			     raw_read_size, NULL)<0) {
	HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, FAIL,
		    "unable to set raw dataset selection");
    }
    if (NULL==(raw_buf=H5MM_malloc(nrows*raw_read_size[1]*type_size))) {
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
		    "memory allocation failed for raw dataset");
    }
    if (H5D_read(ra->raw, type, rm_space, rf_space, H5P_DEFAULT,
		 raw_buf)<0) {
	HGOTO_ERROR(H5E_RAGGED, H5E_READERROR, FAIL,
		    "unable to read raw dataset");
    }
    HDmemset(raw_buf+raw_read_size[0]*raw_read_size[1]*type_size, 0,
	     (nrows-raw_read_size[0])*raw_read_size[1]*type_size);
	
    /* Get the meta data */
    if (NULL==(meta=H5MM_malloc(nrows*sizeof(H5RA_meta_t)))) {
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
		    "memory allocation failed for meta data");
    }
    if (NULL==(mf_space=H5D_get_space(ra->meta)) ||
	H5S_get_simple_extent_dims(mf_space, &meta_cur_size, NULL)<0) {
	HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, FAIL,
		    "unable to get current meta data extents");
    }
    if ((hsize_t)start_row>=meta_cur_size) {
	meta_read_size = 0;
    } else {
	meta_read_size = MIN(nrows, meta_cur_size-(hsize_t)start_row);
    }
    if (NULL==(mm_space=H5S_create(H5S_SIMPLE)) ||
	H5S_set_extent_simple(mm_space, 1, &meta_read_size, NULL)<0 ||
	H5S_select_hyperslab(mf_space, H5S_SELECT_SET, &start_row, NULL,
			     &meta_read_size, NULL)<0) {
	HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, FAIL,
		    "unable to set meta data selection");
    }
    if (H5D_read(ra->meta, H5RA_meta_type_g, mm_space, mf_space,
		 H5P_DEFAULT, meta)<0) {
	HGOTO_ERROR(H5E_RAGGED, H5E_READERROR, FAIL,
		    "unable to read meta data");
    }
    HDmemset(meta+meta_read_size, 0,
	     (nrows-meta_read_size)*sizeof(H5RA_meta_t));

    /* Copy data into output buffers */
    for (i=0; i<nrows; i++) {
	/*
	 * If the caller didn't supply a buffer then allocate a buffer large
	 * enough to hold the entire row.  Ignore the input value for the
	 * size and request the entire row.
	 */
	if (NULL==(buf_out[i]=buf[i])) {
	    if (meta[i].nelmts>0 &&
		NULL==(buf_out[i]=H5MM_malloc(meta[i].nelmts*type_size))) {
		HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
			    "memory allocation failed for result");
	    }
	    size[i] = meta[i].nelmts;
	} else {
	    size[i] = MIN(size[i], meta[i].nelmts);
	}
	if (0==size[i]) continue;

	/* Copy the part of the row from the raw dataset */
	HDmemcpy(buf_out[i], raw_buf+i*raw_read_size[1]*type_size,
		 (size_t)(MIN(size[i], raw_read_size[1])*type_size));

	/* Copy the part of the row from the overflow dataset */
	if (size[i]>raw_read_size[1]) {
	    if (!of_space && NULL==(of_space=H5D_get_space(ra->over))) {
		HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, FAIL,
			    "unable to get overflow extents");
	    }
	    hs_size[0] = size[i]-raw_read_size[1];
	    if (NULL==(om_space=H5S_create(H5S_SIMPLE)) ||
		H5S_set_extent_simple(om_space, 1, size+i, NULL)<0 ||
		H5S_select_hyperslab(om_space, H5S_SELECT_SET,
				     (hssize_t*)(raw_read_size+1), NULL,
				     hs_size, NULL)<0 ||
		H5S_select_hyperslab(of_space, H5S_SELECT_SET,
				     &(meta[i].offset), NULL, hs_size,
				     NULL)<0) {
		HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, FAIL,
			    "unable to set overflow selection");
	    }
	    if (H5D_read(ra->over, type, om_space, of_space, H5P_DEFAULT,
			 buf_out[i])<0) {
		HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, FAIL,
			    "unable to read overflow dataset");
	    }
	    if (H5S_close(om_space)<0) {
		om_space = NULL;
		HGOTO_ERROR(H5E_RAGGED, H5E_CANTINIT, FAIL,
			    "unable to close overflow memory space");
	    }
	    om_space = NULL;
	}

	/* Actual row size */
	size[i] = meta[i].nelmts;
    }

    /* Copy output buffers into BUF argument */
    for (i=0; i<nrows; i++) buf[i] = buf_out[i];
    ret_value = SUCCEED;
    
 done:
    if (rf_space) H5S_close(rf_space);
    if (rm_space) H5S_close(rm_space);
    if (mf_space) H5S_close(mf_space);
    if (mm_space) H5S_close(mm_space);
    if (of_space) H5S_close(of_space);
    if (om_space) H5S_close(om_space);
    H5MM_xfree(meta);
    H5MM_xfree(raw_buf);
    if (buf_out) {
	for (i=0; i<nrows; i++) {
	    if (!buf[i]) H5MM_xfree(buf_out[i]);
	}
	H5MM_xfree(buf_out);
    }
    
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5RA_entof
 *
 * Purpose:	Return a pointer to the ragged arrays symbol table entry.
 *
 * Return:	Success:	Ptr to symbol table entry
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Friday, August 28, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5G_entry_t *
H5RA_entof(H5RA_t *ra)
{
    assert(ra);
    return H5G_entof(ra->group);
}
