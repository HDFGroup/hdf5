/*
 * Copyright (C) 1998 NCSA
 *		      All rights reserved.
 *
 * Programmer:	Robb Matzke <matzke@llnl.gov>
 *		Tuesday, March 31, 1998
 */
#ifdef RCSID
static char		RcsId[] = "@(#)$Revision$";
#endif

#define H5T_PACKAGE		/*suppress error about including H5Tpkg	  */

#include <H5private.h>		/*generic functions			  */
#include <H5Iprivate.h>		/*ID functions		   		  */
#include <H5Eprivate.h>		/*error handling			  */
#include <H5Gprivate.h>		/*groups				  */
#include <H5HGprivate.h>	/*global heap				  */
#include <H5MMprivate.h>	/*memory management			  */
#include <H5Sprivate.h>		/*data space				  */
#include <H5Tpkg.h>		/*data-type functions			  */

#define PABLO_MASK	H5T_mask

#define H5T_COMPND_INC	64	/*typical max numb of members per struct */

/* Interface initialization */
static intn interface_initialize_g = FALSE;
#define INTERFACE_INIT H5T_init_interface
static void H5T_term_interface(void);

/* Predefined types */
hid_t H5T_NATIVE_CHAR_g = FAIL;
hid_t H5T_NATIVE_UCHAR_g = FAIL;
hid_t H5T_NATIVE_SHORT_g = FAIL;
hid_t H5T_NATIVE_USHORT_g = FAIL;
hid_t H5T_NATIVE_INT_g = FAIL;
hid_t H5T_NATIVE_UINT_g = FAIL;
hid_t H5T_NATIVE_LONG_g = FAIL;
hid_t H5T_NATIVE_LLONG_g = FAIL;
hid_t H5T_NATIVE_ULLONG_g = FAIL;
hid_t H5T_NATIVE_HYPER_g = FAIL;
hid_t H5T_NATIVE_UHYPER_g = FAIL;
hid_t H5T_NATIVE_INT8_g = FAIL;
hid_t H5T_NATIVE_UINT8_g = FAIL;
hid_t H5T_NATIVE_INT16_g = FAIL;
hid_t H5T_NATIVE_UINT16_g = FAIL;
hid_t H5T_NATIVE_INT32_g = FAIL;
hid_t H5T_NATIVE_UINT32_g = FAIL;
hid_t H5T_NATIVE_INT64_g = FAIL;
hid_t H5T_NATIVE_UINT64_g = FAIL;
hid_t H5T_NATIVE_ULONG_g = FAIL;
hid_t H5T_NATIVE_FLOAT_g = FAIL;
hid_t H5T_NATIVE_DOUBLE_g = FAIL;
hid_t H5T_NATIVE_TIME_g = FAIL;
hid_t H5T_NATIVE_STRING_g = FAIL;
hid_t H5T_NATIVE_BITFIELD_g = FAIL;
hid_t H5T_NATIVE_OPAQUE_g = FAIL;

/* The path database */
static intn H5T_npath_g = 0;			/*num paths defined	*/
static intn H5T_apath_g = 0;			/*num slots allocated	*/
static H5T_path_t *H5T_path_g = NULL;		/*path array		*/

/* The soft conversion function master list */
static intn H5T_nsoft_g = 0;			/*num soft funcs defined */
static intn H5T_asoft_g = 0;			/*num slots allocated	*/
static H5T_soft_t *H5T_soft_g = NULL;		/*master soft list	*/

/*--------------------------------------------------------------------------
NAME
   H5T_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5T_init_interface()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
herr_t
H5T_init_interface(void)
{
    H5T_t	*dt = NULL;
    herr_t	ret_value = SUCCEED;

    interface_initialize_g = TRUE;
    FUNC_ENTER(H5T_init_interface, FAIL);

    /* Initialize the atom group for the file IDs */
    if ((ret_value = H5I_init_group(H5_DATATYPE, H5I_DATATYPEID_HASHSIZE,
				    H5T_RESERVED_ATOMS,
				    (herr_t (*)(void *)) H5T_close)) != FAIL) {
	ret_value = H5_add_exit(&H5T_term_interface);
    }
    /*
     * Initialize pre-defined data types that depend on the architecture.
     */
    ret_value = H5T_init();

    /*
     * Initialize pre-define data types that can be derived from
     * architecture-dependent types.
     */

    /* INT8 */
    H5T_NATIVE_INT8_g = H5Tcopy(H5T_NATIVE_INT_g);
    H5Tset_size(H5T_NATIVE_INT8_g, 1);
    H5Tset_precision(H5T_NATIVE_INT8_g, 8);
    H5Tlock(H5T_NATIVE_INT8_g);

    /* UINT8 */
    H5T_NATIVE_UINT8_g = H5Tcopy(H5T_NATIVE_UINT_g);
    H5Tset_size(H5T_NATIVE_UINT8_g, 1);
    H5Tset_precision(H5T_NATIVE_UINT8_g, 8);
    H5Tlock(H5T_NATIVE_UINT8_g);

    /* INT16 */
    H5T_NATIVE_INT16_g = H5Tcopy(H5T_NATIVE_INT_g);
    H5Tset_size(H5T_NATIVE_INT16_g, 2);
    H5Tset_precision(H5T_NATIVE_INT16_g, 16);
    H5Tlock(H5T_NATIVE_INT16_g);

    /* UINT16 */
    H5T_NATIVE_UINT16_g = H5Tcopy(H5T_NATIVE_UINT_g);
    H5Tset_size(H5T_NATIVE_UINT16_g, 2);
    H5Tset_precision(H5T_NATIVE_UINT16_g, 16);
    H5Tlock(H5T_NATIVE_UINT16_g);

    /* INT32 */
    H5T_NATIVE_INT32_g = H5Tcopy(H5T_NATIVE_INT_g);
    H5Tset_size(H5T_NATIVE_INT32_g, 4);
    H5Tset_precision(H5T_NATIVE_INT32_g, 32);
    H5Tlock(H5T_NATIVE_INT32_g);

    /* UINT32 */
    H5T_NATIVE_UINT32_g = H5Tcopy(H5T_NATIVE_UINT_g);
    H5Tset_size(H5T_NATIVE_UINT32_g, 4);
    H5Tset_precision(H5T_NATIVE_UINT32_g, 32);
    H5Tlock(H5T_NATIVE_UINT32_g);

    /* INT64 */
    H5T_NATIVE_INT64_g = H5Tcopy(H5T_NATIVE_INT_g);
    H5Tset_size(H5T_NATIVE_INT64_g, 8);
    H5Tset_precision(H5T_NATIVE_INT64_g, 64);
    H5Tlock(H5T_NATIVE_INT64_g);

    /* UINT64 */
    H5T_NATIVE_UINT64_g = H5Tcopy(H5T_NATIVE_UINT_g);
    H5Tset_size(H5T_NATIVE_UINT64_g, 8);
    H5Tset_precision(H5T_NATIVE_UINT64_g, 64);
    H5Tlock(H5T_NATIVE_UINT64_g);

    /*
     * Initialize pre-defined data types that don't depend on architecture.
     */

    /* TIME */
    dt = H5MM_xcalloc(1, sizeof(H5T_t));
    dt->locked = TRUE;
    dt->type = H5T_TIME;
    dt->size = 1;
    dt->u.atomic.order = H5Tget_order(H5T_NATIVE_INT_g);
    dt->u.atomic.offset = 0;
    dt->u.atomic.prec = 8 * dt->size;
    dt->u.atomic.lsb_pad = H5T_PAD_ZERO;
    dt->u.atomic.msb_pad = H5T_PAD_ZERO;
    if ((H5T_NATIVE_TIME_g = H5I_register(H5_DATATYPE, dt)) < 0) {
	HRETURN_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL,
		      "can't initialize H5T layer");
    }

    /* STRING */
    dt = H5MM_xcalloc(1, sizeof(H5T_t));
    dt->locked = TRUE;
    dt->type = H5T_STRING;
    dt->size = 1;
    dt->u.atomic.order = H5T_ORDER_NONE;
    dt->u.atomic.offset = 0;
    dt->u.atomic.prec = 8 * dt->size;
    dt->u.atomic.lsb_pad = H5T_PAD_ZERO;
    dt->u.atomic.msb_pad = H5T_PAD_ZERO;
    dt->u.atomic.u.s.cset = H5T_CSET_ASCII;
    dt->u.atomic.u.s.pad = H5T_STR_NULL;
    if ((H5T_NATIVE_STRING_g = H5I_register(H5_DATATYPE, dt)) < 0) {
	HRETURN_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL,
		      "can't initialize H5T layer");
    }

    /* BITFIELD */
    dt = H5MM_xcalloc(1, sizeof(H5T_t));
    dt->locked = TRUE;
    dt->type = H5T_BITFIELD;
    dt->size = 1;
    dt->u.atomic.order = H5Tget_order(H5T_NATIVE_INT_g);
    dt->u.atomic.offset = 0;
    dt->u.atomic.prec = 8 * dt->size;
    dt->u.atomic.lsb_pad = H5T_PAD_ZERO;
    dt->u.atomic.msb_pad = H5T_PAD_ZERO;
    if ((H5T_NATIVE_BITFIELD_g = H5I_register(H5_DATATYPE, dt)) < 0) {
	HRETURN_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL,
		      "unable to initialize H5T layer");
    }

    /* OPAQUE */
    dt = H5MM_xcalloc(1, sizeof(H5T_t));
    dt->locked = TRUE;
    dt->type = H5T_OPAQUE;
    dt->size = 1;
    dt->u.atomic.order = H5T_ORDER_NONE;
    dt->u.atomic.offset = 0;
    dt->u.atomic.prec = 8 * dt->size;
    dt->u.atomic.lsb_pad = H5T_PAD_ZERO;
    dt->u.atomic.msb_pad = H5T_PAD_ZERO;
    if ((H5T_NATIVE_OPAQUE_g = H5I_register(H5_DATATYPE, dt)) < 0) {
	HRETURN_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL,
		      "unable to initialize H5T layer");
    }

    /*
     * Define aliases.
     */
    H5T_NATIVE_HYPER_g = H5Tcopy(H5T_NATIVE_LLONG_g);
    H5Tlock(H5T_NATIVE_HYPER_g);

    H5T_NATIVE_UHYPER_g = H5Tcopy(H5T_NATIVE_ULLONG_g);
    H5Tlock(H5T_NATIVE_UHYPER_g);

    /*
     * Register conversion functions beginning with the most general and
     * ending with the most specific.
     */
    if (H5Tregister_soft("ibo", H5T_INTEGER, H5T_INTEGER,
			 H5T_conv_order) < 0) {
	HRETURN_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL,
		      "unable to register conversion function");
    }
    if (H5Tregister_soft("fbo", H5T_FLOAT, H5T_FLOAT, H5T_conv_order) < 0) {
	HRETURN_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL,
		      "unable to register conversion function");
    }
    if (H5Tregister_soft ("struct", H5T_COMPOUND, H5T_COMPOUND,
			  H5T_conv_struct)<0) {
	HRETURN_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,
		       "unable to register conversion function");
    }
    
    
    FUNC_LEAVE(ret_value);
}

/*--------------------------------------------------------------------------
 NAME
    H5T_term_interface
 PURPOSE
    Terminate various H5T objects
 USAGE
    void H5T_term_interface()
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Release the atom group and any other resources allocated.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static void
H5T_term_interface(void)
{
    intn	i;
    H5T_path_t	*path = NULL;
    H5T_cdata_t	*pcdata = NULL;
    H5T_conv_t	cfunc = NULL;
#ifdef H5T_DEBUG
    intn	nprint=0;
    hsize_t	nbytes;
#endif
    
    /* Unregister all conversion functions */
    for (i=0; i<H5T_npath_g; i++) {
	path = H5T_path_g + i;

	if (path->func) {
	    path->cdata.command = H5T_CONV_FREE;
	    if ((path->func)(FAIL, FAIL, &(path->cdata), 0, NULL, NULL)<0) {
#ifdef H5T_DEBUG
		fprintf (stderr, "H5T: conversion function failed "
			 "to free private data\n");
#endif
		H5E_clear(); /*ignore the error*/
	    }
#ifdef H5T_DEBUG
	    if (0==nprint++) {
		HDfprintf (stderr, "H5T: type conversion statistics "
			   "accumulated over life of library:\n");
		HDfprintf (stderr, "   %-*s %8s/%-5s %8s %8s %8s %15s\n",
			   H5T_NAMELEN-1, "Name", "Elmts", "Calls", "User",
			   "System", "Elapsed", "Bandwidth");
		HDfprintf (stderr, "   %-*s %8s-%-5s %8s %8s %8s %15s\n",
			   H5T_NAMELEN-1, "----", "-----", "-----", "----",
			   "------", "-------", "---------");
	    }
	    nbytes = MAX (H5T_get_size (path->src), H5T_get_size (path->dst));
	    nbytes *= path->cdata.stats->nelmts;
	    HDfprintf (stderr, "   %-*s %8Hd/%-5d %8.2f %8.2f %8.2f %15g\n",
		       H5T_NAMELEN-1, path->name,
		       path->cdata.stats->nelmts,
		       path->cdata.stats->ncalls,
		       path->cdata.stats->timer.utime, 
		       path->cdata.stats->timer.stime, 
		       path->cdata.stats->timer.etime,
		       nbytes / path->cdata.stats->timer.etime);
#endif
	    H5MM_xfree (path->cdata.stats);
	}
    }

    /* Clear conversion tables */
    H5T_apath_g = 0;
    H5T_npath_g = 0;
    H5T_path_g = H5MM_xfree (H5T_path_g);

    H5T_asoft_g = 0;
    H5T_nsoft_g = 0;
    H5T_soft_g = H5MM_xfree (H5T_soft_g);

    /* Clear noop function */
    if ((cfunc=H5T_find (NULL, NULL, H5T_BKG_NO, &pcdata))) {
	pcdata->command = H5T_CONV_FREE;
	(cfunc)(FAIL, FAIL, pcdata, 0, NULL, NULL);
    }

    H5I_destroy_group(H5_DATATYPE);
    H5T_NATIVE_CHAR_g = FAIL;
    H5T_NATIVE_UCHAR_g = FAIL;
    H5T_NATIVE_SHORT_g = FAIL;
    H5T_NATIVE_USHORT_g = FAIL;
    H5T_NATIVE_INT_g = FAIL;
    H5T_NATIVE_UINT_g = FAIL;
    H5T_NATIVE_LONG_g = FAIL;
    H5T_NATIVE_LLONG_g = FAIL;
    H5T_NATIVE_ULLONG_g = FAIL;
    H5T_NATIVE_HYPER_g = FAIL;
    H5T_NATIVE_UHYPER_g = FAIL;
    H5T_NATIVE_INT8_g = FAIL;
    H5T_NATIVE_UINT8_g = FAIL;
    H5T_NATIVE_INT16_g = FAIL;
    H5T_NATIVE_UINT16_g = FAIL;
    H5T_NATIVE_INT32_g = FAIL;
    H5T_NATIVE_UINT32_g = FAIL;
    H5T_NATIVE_INT64_g = FAIL;
    H5T_NATIVE_UINT64_g = FAIL;
    H5T_NATIVE_ULONG_g = FAIL;
    H5T_NATIVE_FLOAT_g = FAIL;
    H5T_NATIVE_DOUBLE_g = FAIL;
    H5T_NATIVE_TIME_g = FAIL;
    H5T_NATIVE_STRING_g = FAIL;
    H5T_NATIVE_BITFIELD_g = FAIL;
    H5T_NATIVE_OPAQUE_g = FAIL;
}


/*-------------------------------------------------------------------------
 * Function:	H5Tcreate
 *
 * Purpose:	Create a new type and initialize it to reasonable values.
 *		The type is a member of type class TYPE and is SIZE bytes.
 *
 * Return:	Success:	A new type identifier.
 *
 *		Failure:	FAIL
 *
 * Errors:
 *		ARGS	  BADVALUE	Invalid size. 
 *		DATATYPE  CANTINIT	Can't create type. 
 *		DATATYPE  CANTREGISTER	Can't register data type atom. 
 *
 * Programmer:	Robb Matzke
 *		Friday, December  5, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Tcreate(H5T_class_t type, size_t size)
{
    H5T_t	*dt = NULL;
    hid_t	ret_value = FAIL;

    FUNC_ENTER(H5Tcreate, FAIL);

    /* check args */
    if (size <= 0) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid size");
    }

    /* create the type */
    if (NULL == (dt = H5T_create(type, size))) {
	HRETURN_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "can't create type");
    }

    /* Make it an atom */
    if ((ret_value = H5I_register(H5_DATATYPE, dt)) < 0) {
	HRETURN_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL,
		      "can't register data type atom");
    }

    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tcopy
 *
 * Purpose:	Copies a data type.  The resulting data type is not locked.
 *		The data type should be closed when no longer needed by
 *		calling H5Tclose().
 *
 * Return:	Success:	The ID of a new data type.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Tcopy(hid_t type_id)
{
    H5T_t	*dt = NULL;
    H5T_t	*new_dt = NULL;
    hid_t	ret_value = FAIL;

    FUNC_ENTER(H5Tcopy, FAIL);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    }

    /* Copy */
    if (NULL == (new_dt = H5T_copy(dt))) {
	HRETURN_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "can't copy");
    }

    /* Atomize result */
    if ((ret_value = H5I_register(H5_DATATYPE, new_dt)) < 0) {
	H5T_close(new_dt);
	HRETURN_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL,
		      "can't register data type atom");
    }
    
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tclose
 *
 * Purpose:	Frees a data type and all associated memory.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tclose(hid_t type_id)
{
    H5T_t	*dt = NULL;

    FUNC_ENTER(H5Tclose, FAIL);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    }
    if (dt->locked) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "predefined data type");
    }

    /* When the reference count reaches zero the resources are freed */
    if (H5I_dec_ref(type_id) < 0) {
	HRETURN_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "problem freeing id");
    }
    
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tequal
 *
 * Purpose:	Determines if two data types are equal.
 *
 * Return:	Success:	TRUE if equal, FALSE if unequal
 *
 *		Failure:	FAIL
 *
 * Errors:
 *
 * Programmer:	Robb Matzke
 *		Wednesday, December 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hbool_t
H5Tequal(hid_t type1_id, hid_t type2_id)
{
    const H5T_t		*dt1 = NULL;
    const H5T_t		*dt2 = NULL;
    hbool_t		ret_value = FAIL;

    FUNC_ENTER(H5Tequal, FAIL);

    /* check args */
    if (H5_DATATYPE != H5I_group(type1_id) ||
	NULL == (dt1 = H5I_object(type1_id)) ||
	H5_DATATYPE != H5I_group(type2_id) ||
	NULL == (dt2 = H5I_object(type2_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    }
    ret_value = (0 == H5T_cmp(dt1, dt2)) ? TRUE : FALSE;

    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tlock
 *
 * Purpose:	Locks a type, making it read only and non-destructable.	 This
 *		is normally done by the library for predefined data types so
 *		the application doesn't inadvertently change or delete a
 *		predefined type.
 *
 *		Once a data type is locked it can never be unlocked.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Friday, January	 9, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tlock(hid_t type_id)
{
    H5T_t	*dt = NULL;

    FUNC_ENTER(H5Tlock, FAIL);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    }
    dt->locked = TRUE;
    
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tget_class
 *
 * Purpose:	Returns the data type class identifier for data type TYPE_ID.
 *
 * Return:	Success:	One of the non-negative data type class
 *				constants.
 *
 *		Failure:	H5T_NO_CLASS (-1, same as FAIL)
 *
 * Programmer:	Robb Matzke
 *		Monday, December  8, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5T_class_t
H5Tget_class(hid_t type_id)
{
    H5T_t	*dt = NULL;

    FUNC_ENTER(H5Tget_class, H5T_NO_CLASS);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, H5T_NO_CLASS, "not a data type");
    }
    
    FUNC_LEAVE(dt->type);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tget_size
 *
 * Purpose:	Determines the total size of a data type in bytes.
 *
 * Return:	Success:	Size of the data type in bytes.	 The size of
 *				data type is the size of an instance of that
 *				data type.
 *
 *		Failure:	0 (valid data types are never zero size)
 *
 * Programmer:	Robb Matzke
 *		Monday, December  8, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5Tget_size(hid_t type_id)
{
    H5T_t	*dt = NULL;
    size_t	size;

    FUNC_ENTER(H5Tget_size, 0);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a data type");
    }
    
    /* size */
    size = H5T_get_size(dt);

    FUNC_LEAVE(size);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tset_size
 *
 * Purpose:	Sets the total size in bytes for an atomic data type (this
 *		operation is not permitted on compound data types).  If the
 *		size is decreased so that the significant bits of the data
 *		type extend beyond the edge of the new size, then the
 *		`offset' property is decreased toward zero.  If the `offset'
 *		becomes zero and the significant bits of the data type still
 *		hang over the edge of the new size, then the number of
 *		significant bits is decreased.
 *
 *		Adjusting the size of an H5T_STRING automatically sets the
 *		precision to 8*size.
 *
 *		All data types have a positive size.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tset_size(hid_t type_id, size_t size)
{
    H5T_t	*dt = NULL;
    size_t	prec, offset;

    FUNC_ENTER(H5Tset_size, FAIL);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	!H5T_is_atomic(dt)) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an atomic data type");
    }
    if (dt->locked) {
	HRETURN_ERROR(H5E_ARGS, H5E_CANTINIT, FAIL, "data type is read-only");
    }
    if (size <= 0) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "size must be positive");
    }
    offset = dt->u.atomic.offset;
    prec = dt->u.atomic.prec;

    /* Decrement the offset and precision if necessary */
    if (prec > 8 * size)
	offset = 0;
    else if (offset + prec > 8 * size)
	offset = 8 * size - prec;
    if (prec > 8 * size)
	prec = 8 * size;

    /* Make sure that other values are still okay */
    switch (dt->type) {
    case H5T_INTEGER:
    case H5T_TIME:
    case H5T_BITFIELD:
	/* nothing to check */
	break;

    case H5T_STRING:
	prec = 8 * size;
	offset = 0;
	break;

    case H5T_FLOAT:
	/*
	 * The sign, mantissa, and exponent fields should be adjusted first
	 * when decreasing the size of a floating point type.
	 */
	if (dt->u.atomic.u.f.sign >= prec ||
	    dt->u.atomic.u.f.epos + dt->u.atomic.u.f.esize > prec ||
	    dt->u.atomic.u.f.mpos + dt->u.atomic.u.f.msize > prec) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
			"adjust sign, mantissa, and exponent fields first");
	}
	break;

    case H5T_OPAQUE:
	/*
	 * The significant bits of an opaque type are not allowed to change
	 * implicitly.
	 */
	if (prec != dt->u.atomic.prec) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
			  "unable to change precision of an opaque type");
	}
	break;

    default:
	assert("not implemented yet" && 0);
    }

    /* Commit */
    H5T_unshare (dt);
    dt->size = size;
    dt->u.atomic.offset = offset;
    dt->u.atomic.prec = prec;

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tget_order
 *
 * Purpose:	Returns the byte order of an atomic data type.
 *
 * Return:	Success:	A byte order constant
 *
 *		Failure:	H5T_ORDER_ERROR (-1, same as FAIL)
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5T_order_t
H5Tget_order(hid_t type_id)
{
    H5T_t		*dt = NULL;
    H5T_order_t		order;

    FUNC_ENTER(H5Tget_order, H5T_ORDER_ERROR);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	!H5T_is_atomic(dt)) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, H5T_ORDER_ERROR,
		      "not an atomic data type");
    }

    /* Order */
    order = dt->u.atomic.order;

    FUNC_LEAVE(order);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tset_order
 *
 * Purpose:	Sets the byte order for an atomic data type.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tset_order(hid_t type_id, H5T_order_t order)
{
    H5T_t	*dt = NULL;

    FUNC_ENTER(H5Tset_order, FAIL);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	!H5T_is_atomic(dt)) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an atomic data type");
    }
    if (dt->locked) {
	HRETURN_ERROR(H5E_ARGS, H5E_CANTINIT, FAIL, "data type is read-only");
    }
    if (order < 0 || order > H5T_ORDER_NONE) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "illegal byte order");
    }

    /* Commit */
    H5T_unshare (dt);
    dt->u.atomic.order = order;
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tget_precision
 *
 * Purpose:	Gets the precision of an atomic data type.  The precision is
 *		the number of significant bits which, unless padding is
 *		present, is 8 times larger than the value returned by
 *		H5Tget_size().
 *
 * Return:	Success:	Number of significant bits
 *
 *		Failure:	0 (all atomic types have at least one
 *				significant bit)
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5Tget_precision(hid_t type_id)
{
    H5T_t	*dt = NULL;
    size_t	prec;

    FUNC_ENTER(H5Tget_precision, 0);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	!H5T_is_atomic(dt)) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not an atomic data type");
    }
    
    /* Precision */
    prec = dt->u.atomic.prec;

    FUNC_LEAVE(prec);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tset_precision
 *
 * Purpose:	Sets the precision of an atomic data type.  The precision is
 *		the number of significant bits which, unless padding is
 *		present, is 8 times larger than the value returned by
 *		H5Tget_size().
 *
 *		If the precision is increased then the offset is decreased
 *		and then the size is increased to insure that significant
 *		bits do not "hang over" the edge of the data type.
 *
 *		Changing the precision of an H5T_STRING automatically changes
 *		the size as well.  The precision must be a multiple of 8.
 *
 *		When decreasing the precision of a floating point type, set
 *		the locations and sizes of the sign, mantissa, and exponent
 *		fields first.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tset_precision(hid_t type_id, size_t prec)
{
    H5T_t	*dt = NULL;
    size_t	offset, size;

    FUNC_ENTER(H5Tset_prec, FAIL);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	!H5T_is_atomic(dt)) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an atomic data type");
    }
    if (dt->locked) {
	HRETURN_ERROR(H5E_ARGS, H5E_CANTINIT, FAIL, "data type is read-only");
    }
    if (prec <= 0) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		      "precision must be positive");
    }
    
    /* Adjust the offset and size */
    offset = dt->u.atomic.offset;
    size = dt->size;
    if (prec > 8 * size)
	offset = 0;
    else if (offset + prec > 8 * size)
	offset = 8 * size - prec;
    if (prec > 8 * size)
	size = (prec + 7) / 8;

    /* Check that things are still kosher */
    switch (dt->type) {
    case H5T_INTEGER:
    case H5T_TIME:
    case H5T_BITFIELD:
    case H5T_OPAQUE:
	/* nothing to check */
	break;

    case H5T_STRING:
	if (prec % 8) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
			  "precision for this type must be a multiple of 8");
	}
	offset = 0;
	size = prec / 8;
	break;

    case H5T_FLOAT:
	/*
	 * The sign, mantissa, and exponent fields should be adjusted first
	 * when decreasing the precision of a floating point type.
	 */
	if (dt->u.atomic.u.f.sign >= prec ||
	    dt->u.atomic.u.f.epos + dt->u.atomic.u.f.esize > prec ||
	    dt->u.atomic.u.f.mpos + dt->u.atomic.u.f.msize > prec) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
			"adjust sign, mantissa, and exponent fields first");
	}
	break;

    default:
	assert("not implemented yet" && 0);
    }

    /* Commit */
    H5T_unshare (dt);
    dt->size = size;
    dt->u.atomic.offset = offset;
    dt->u.atomic.prec = prec;

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tget_offset
 *
 * Purpose:	Retrieves the bit offset of the first significant bit.	The
 *		signficant bits of an atomic datum can be offset from the
 *		beginning of the memory for that datum by an amount of
 *		padding. The `offset' property specifies the number of bits
 *		of padding that appear to the "right of" the value.  That is,
 *		if we have a 32-bit datum with 16-bits of precision having
 *		the value 0x1122 then it will be layed out in memory as (from
 *		small byte address toward larger byte addresses):
 *
 *		    Big	     Big       Little	Little
 *		    Endian   Endian    Endian	Endian
 *		    offset=0 offset=16 offset=0 offset=16
 *
 *		0:  [ pad]   [0x11]    [0x22]	[ pad]
 *		1:  [ pad]   [0x22]    [0x11]	[ pad]
 *		2:  [0x11]   [ pad]    [ pad]	[0x22]
 *		3:  [0x22]   [ pad]    [ pad]	[0x11]
 *
 * Return:	Success:	The offset
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5Tget_offset(hid_t type_id)
{
    H5T_t	*dt = NULL;
    size_t	offset;

    FUNC_ENTER(H5Tget_offset, 0);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	!H5T_is_atomic(dt)) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not an atomic data type");
    }
    
    /* Offset */
    offset = dt->u.atomic.offset;

    FUNC_LEAVE(offset);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tset_offset
 *
 * Purpose:	Sets the bit offset of the first significant bit.  The
 *		signficant bits of an atomic datum can be offset from the
 *		beginning of the memory for that datum by an amount of
 *		padding. The `offset' property specifies the number of bits
 *		of padding that appear to the "right of" the value.  That is,
 *		if we have a 32-bit datum with 16-bits of precision having
 *		the value 0x1122 then it will be layed out in memory as (from
 *		small byte address toward larger byte addresses):
 *
 *		    Big	     Big       Little	Little
 *		    Endian   Endian    Endian	Endian
 *		    offset=0 offset=16 offset=0 offset=16
 *
 *		0:  [ pad]   [0x11]    [0x22]	[ pad]
 *		1:  [ pad]   [0x22]    [0x11]	[ pad]
 *		2:  [0x11]   [ pad]    [ pad]	[0x22]
 *		3:  [0x22]   [ pad]    [ pad]	[0x11]
 *
 *		If the offset is incremented then the total size is
 *		incremented also if necessary to prevent significant bits of
 *		the value from hanging over the edge of the data type.
 *
 *		The offset of an H5T_STRING cannot be set to anything but
 *		zero. 
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tset_offset(hid_t type_id, size_t offset)
{
    H5T_t	*dt = NULL;

    FUNC_ENTER(H5Tset_offset, FAIL);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	!H5T_is_atomic(dt)) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an atomic data type");
    }
    if (dt->locked) {
	HRETURN_ERROR(H5E_ARGS, H5E_CANTINIT, FAIL, "data type is read-only");
    }
    if (H5T_STRING == dt->type && offset != 0) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		      "offset must be zero for this type");
    }
    
    /* Adjust the size */
    if (offset + dt->u.atomic.prec > 8 * dt->size) {
	dt->size = (offset + dt->u.atomic.prec + 7) / 8;
    }
    
    /* Commit */
    H5T_unshare (dt);
    dt->u.atomic.offset = offset;

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tget_pad
 *
 * Purpose:	Gets the least significant pad type and the most significant
 *		pad type and returns their values through the LSB and MSB
 *		arguments, either of which may be the null pointer.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Friday, January	 9, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tget_pad(hid_t type_id, H5T_pad_t *lsb/*out*/, H5T_pad_t *msb/*out*/)
{
    H5T_t	*dt = NULL;

    FUNC_ENTER(H5Tget_pad, FAIL);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	!H5T_is_atomic(dt)) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an atomic data type");
    }
    
    /* Get values */
    if (lsb) *lsb = dt->u.atomic.lsb_pad;
    if (msb) *msb = dt->u.atomic.msb_pad;

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tset_pad
 *
 * Purpose:	Sets the LSB and MSB pad types.
 *
 * Return:	Success:	
 *
 *		Failure:	
 *
 * Programmer:	Robb Matzke
 *		Friday, January	 9, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tset_pad(hid_t type_id, H5T_pad_t lsb, H5T_pad_t msb)
{
    H5T_t *dt = NULL;

    FUNC_ENTER(H5Tset_pad, FAIL);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	!H5T_is_atomic(dt)) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an atomic data type");
    }
    if (dt->locked) {
	HRETURN_ERROR(H5E_ARGS, H5E_CANTINIT, FAIL, "data type is read-only");
    }
    if (lsb < 0 || lsb >= H5T_NPAD || msb < 0 || msb >= H5T_NPAD) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid pad type");
    }

    /* Commit */
    H5T_unshare (dt);
    dt->u.atomic.lsb_pad = lsb;
    dt->u.atomic.msb_pad = msb;

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tget_sign
 *
 * Purpose:	Retrieves the sign type for an integer type.
 *
 * Return:	Success:	The sign type.
 *
 *		Failure:	H5T_SGN_ERROR (-1, same as FAIL)
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5T_sign_t
H5Tget_sign(hid_t type_id)
{
    H5T_t		*dt = NULL;
    H5T_sign_t		sign;

    FUNC_ENTER(H5Tget_sign, H5T_SGN_ERROR);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	H5T_INTEGER != dt->type) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, H5T_SGN_ERROR,
		      "not an integer data type");
    }
    
    /* Sign */
    sign = dt->u.atomic.u.i.sign;

    FUNC_LEAVE(sign);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tset_sign
 *
 * Purpose:	Sets the sign property for an integer.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tset_sign(hid_t type_id, H5T_sign_t sign)
{
    H5T_t	*dt = NULL;

    FUNC_ENTER(H5Tset_sign, FAIL);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	H5T_INTEGER != dt->type) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an integer data type");
    }
    if (dt->locked) {
	HRETURN_ERROR(H5E_ARGS, H5E_CANTINIT, FAIL, "data type is read-only");
    }
    if (sign < 0 || sign >= H5T_NSGN) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "illegal sign type");
    }
    
    /* Commit */
    H5T_unshare (dt);
    dt->u.atomic.u.i.sign = sign;
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tget_fields
 *
 * Purpose:	Returns information about the locations of the various bit
 *		fields of a floating point data type.  The field positions
 *		are bit positions in the significant region of the data type.
 *		Bits are numbered with the least significant bit number zero.
 *
 *		Any (or even all) of the arguments can be null pointers.
 *
 * Return:	Success:	SUCCEED, field locations and sizes are
 *				returned through the arguments.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tget_fields(hid_t type_id, size_t *spos/*out*/,
	      size_t *epos/*out*/, size_t *esize/*out*/,
	      size_t *mpos/*out*/, size_t *msize/*out*/)
{
    H5T_t	*dt = NULL;

    FUNC_ENTER(H5Tget_fields, FAIL);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	H5T_FLOAT != dt->type) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a floating-point data type");
    }
    
    /* Get values */
    if (spos) *spos = dt->u.atomic.u.f.sign;
    if (epos) *epos = dt->u.atomic.u.f.epos;
    if (esize) *esize = dt->u.atomic.u.f.esize;
    if (mpos) *mpos = dt->u.atomic.u.f.mpos;
    if (msize) *msize = dt->u.atomic.u.f.msize;

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tset_fields
 *
 * Purpose:	Sets the locations and sizes of the various floating point
 *		bit fields.  The field positions are bit positions in the
 *		significant region of the data type.  Bits are numbered with
 *		the least significant bit number zero.
 *
 *		Fields are not allowed to extend beyond the number of bits of
 *		precision, nor are they allowed to overlap with one another.
 *		
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tset_fields(hid_t type_id, size_t spos, size_t epos, size_t esize,
	      size_t mpos, size_t msize)
{
    H5T_t	*dt = NULL;

    FUNC_ENTER(H5Tset_fields, FAIL);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	H5T_FLOAT != dt->type) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a floating-point data type");
    }
    if (dt->locked) {
	HRETURN_ERROR(H5E_ARGS, H5E_CANTINIT, FAIL, "data type is read-only");
    }
    if (epos + esize > dt->u.atomic.prec) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		      "exponent bit field size/location is invalid");
    }
    if (mpos + msize > dt->u.atomic.prec) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		      "mantissa bit field size/location is invalid");
    }
    if (spos >= dt->u.atomic.prec) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		      "sign location is not valid");
    }
    
    /* Check for overlap */
    if (spos >= epos && spos < epos + esize) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		      "sign bit appears within exponent field");
    }
    if (spos >= mpos && spos < mpos + msize) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		      "sign bit appears within mantissa field");
    }
    if ((mpos < epos && mpos + msize > epos) ||
	(epos < mpos && epos + esize > mpos)) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		      "exponent and mantissa fields overlap");
    }
    
    /* Commit */
    H5T_unshare (dt);
    dt->u.atomic.u.f.sign = spos;
    dt->u.atomic.u.f.epos = epos;
    dt->u.atomic.u.f.mpos = mpos;
    dt->u.atomic.u.f.esize = esize;
    dt->u.atomic.u.f.msize = msize;

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tget_ebias
 *
 * Purpose:	Retrieves the exponent bias of a floating-point type.
 *
 * Return:	Success:	The bias
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5Tget_ebias(hid_t type_id)
{
    H5T_t	*dt = NULL;
    size_t	ebias;

    FUNC_ENTER(H5Tget_ebias, 0);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	H5T_FLOAT != dt->type) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, 0,
		      "not a floating-point data type");
    }
    
    /* bias */
    ebias = dt->u.atomic.u.f.ebias;

    FUNC_LEAVE(ebias);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tset_ebias
 *
 * Purpose:	Sets the exponent bias of a floating-point type.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tset_ebias(hid_t type_id, size_t ebias)
{
    H5T_t	*dt = NULL;

    FUNC_ENTER(H5Tset_ebias, FAIL);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	H5T_FLOAT != dt->type) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a floating-point data type");
    }
    if (dt->locked) {
	HRETURN_ERROR(H5E_ARGS, H5E_CANTINIT, FAIL, "data type is read-only");
    }

    /* Commit */
    H5T_unshare (dt);
    dt->u.atomic.u.f.ebias = ebias;

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tget_norm
 *
 * Purpose:	Returns the mantisssa normalization of a floating-point data
 *		type.
 *
 * Return:	Success:	Normalization ID
 *
 *		Failure:	H5T_NORM_ERROR (-1, same as FAIL)
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5T_norm_t
H5Tget_norm(hid_t type_id)
{
    H5T_t	*dt = NULL;
    H5T_norm_t	norm;

    FUNC_ENTER(H5Tget_norm, H5T_NORM_ERROR);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	H5T_FLOAT != dt->type) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, H5T_NORM_ERROR,
		      "not a floating-point data type");
    }
    
    /* norm */
    norm = dt->u.atomic.u.f.norm;

    FUNC_LEAVE(norm);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tset_norm
 *
 * Purpose:	Sets the mantissa normalization method for a floating point
 *		data type.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tset_norm(hid_t type_id, H5T_norm_t norm)
{
    H5T_t	*dt = NULL;

    FUNC_ENTER(H5Tset_norm, FAIL);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	H5T_FLOAT != dt->type) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a floating-point data type");
    }
    if (dt->locked) {
	HRETURN_ERROR(H5E_ARGS, H5E_CANTINIT, FAIL, "data type is read-only");
    }
    if (norm < 0 || norm > H5T_NORM_NONE) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "illegal normalization");
    }
    
    /* Commit */
    H5T_unshare (dt);
    dt->u.atomic.u.f.norm = norm;
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tget_inpad
 *
 * Purpose:	If any internal bits of a floating point type are unused
 *		(that is, those significant bits which are not part of the
 *		sign, exponent, or mantissa) then they will be filled
 *		according to the value of this property.
 *
 * Return:	Success:	The internal padding type.
 *
 *		Failure:	H5T_PAD_ERROR (-1, same as FAIL)
 *
 * Programmer:	Robb Matzke
 *		Friday, January	 9, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5T_pad_t
H5Tget_inpad(hid_t type_id)
{
    H5T_t	*dt = NULL;
    H5T_pad_t	pad;

    FUNC_ENTER(H5Tget_inpad, H5T_PAD_ERROR);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	H5T_FLOAT != dt->type) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, H5T_PAD_ERROR,
		      "not a floating-point data type");
    }
    
    /* pad */
    pad = dt->u.atomic.u.f.pad;

    FUNC_LEAVE(pad);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tset_inpad
 *
 * Purpose:	If any internal bits of a floating point type are unused
 *		(that is, those significant bits which are not part of the
 *		sign, exponent, or mantissa) then they will be filled
 *		according to the value of this property.
 *
 * Return:	Success:	
 *
 *		Failure:	
 *
 * Programmer:	Robb Matzke
 *		Friday, January	 9, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tset_inpad(hid_t type_id, H5T_pad_t pad)
{
    H5T_t	*dt = NULL;

    FUNC_ENTER(H5Tset_inpad, FAIL);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	H5T_FLOAT != dt->type) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a floating-point data type");
    }
    if (dt->locked) {
	HRETURN_ERROR(H5E_ARGS, H5E_CANTINIT, FAIL, "data type is read-only");
    }
    if (pad < 0 || pad >= H5T_NPAD) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		      "illegal internal pad type");
    }
    
    /* Commit */
    H5T_unshare (dt);
    dt->u.atomic.u.f.pad = pad;
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tget_cset
 *
 * Purpose:	HDF5 is able to distinguish between character sets of
 *		different nationalities and to convert between them to the
 *		extent possible.
 *		
 * Return:	Success:	The character set of an H5T_STRING type.
 *
 *		Failure:	H5T_CSET_ERROR (-1, same as FAIL)
 *
 * Programmer:	Robb Matzke
 *		Friday, January	 9, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5T_cset_t
H5Tget_cset(hid_t type_id)
{
    H5T_t	*dt = NULL;
    H5T_cset_t	cset;

    FUNC_ENTER(H5Tget_cset, H5T_CSET_ERROR);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	H5T_STRING != dt->type) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, H5T_CSET_ERROR,
		      "not a string data type");
    }
    
    /* result */
    cset = dt->u.atomic.u.s.cset;

    FUNC_LEAVE(cset);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tset_cset
 *
 * Purpose:	HDF5 is able to distinguish between character sets of
 *		different nationalities and to convert between them to the
 *		extent possible.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Friday, January	 9, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tset_cset(hid_t type_id, H5T_cset_t cset)
{
    H5T_t	*dt = NULL;

    FUNC_ENTER(H5Tset_cset, FAIL);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	H5T_STRING != dt->type) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a string data type");
    }
    if (dt->locked) {
	HRETURN_ERROR(H5E_ARGS, H5E_CANTINIT, FAIL, "data type is read-only");
    }
    if (cset < 0 || cset >= H5T_NCSET) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		      "illegal character set type");
    }
    
    /* Commit */
    H5T_unshare (dt);
    dt->u.atomic.u.s.cset = cset;
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tget_strpad
 *
 * Purpose:	The method used to store character strings differs with the
 *		programming language: C usually null terminates strings while
 *		Fortran left-justifies and space-pads strings.	This property
 *		defines the storage mechanism for the string.
 *		
 * Return:	Success:	The character set of an H5T_STRING type.
 *
 *		Failure:	H5T_STR_ERROR (-1, same as FAIL)
 *
 * Programmer:	Robb Matzke
 *		Friday, January	 9, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5T_str_t
H5Tget_strpad(hid_t type_id)
{
    H5T_t	*dt = NULL;
    H5T_str_t	strpad;

    FUNC_ENTER(H5Tget_strpad, H5T_STR_ERROR);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	H5T_STRING != dt->type) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, H5T_STR_ERROR,
		      "not a string data type");
    }
    
    /* result */
    strpad = dt->u.atomic.u.s.pad;

    FUNC_LEAVE(strpad);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tset_strpad
 *
 * Purpose:	The method used to store character strings differs with the
 *		programming language: C usually null terminates strings while
 *		Fortran left-justifies and space-pads strings.	This property
 *		defines the storage mechanism for the string.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Friday, January	 9, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tset_strpad(hid_t type_id, H5T_str_t strpad)
{
    H5T_t	*dt = NULL;

    FUNC_ENTER(H5Tset_strpad, FAIL);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	H5T_STRING != dt->type) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a string data type");
    }
    if (dt->locked) {
	HRETURN_ERROR(H5E_ARGS, H5E_CANTINIT, FAIL, "data type is read-only");
    }
    if (strpad < 0 || strpad >= H5T_NSTR) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "illegal string pad type");
    }
    
    /* Commit */
    H5T_unshare (dt);
    dt->u.atomic.u.s.pad = strpad;
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tget_nmembers
 *
 * Purpose:	Determines how many members compound data type TYPE_ID has.
 *
 * Return:	Success:	Number of members defined in a compound data
 *				type.
 *
 *		Failure:	FAIL
 *
 * Errors:
 *
 * Programmer:	Robb Matzke
 *		Monday, December  8, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5Tget_nmembers(hid_t type_id)
{

    H5T_t	*dt = NULL;

    FUNC_ENTER(H5Tget_num_members, FAIL);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	H5T_COMPOUND != dt->type) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a compound data type");
    }
    
    FUNC_LEAVE(dt->u.compnd.nmembs);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tget_member_name
 *
 * Purpose:	Returns the name of a member of a compound data type.
 *		Members are stored in no particular order with numbers 0
 *		through N-1 where N is the value returned by
 *		H5Tget_nmembers().
 *
 * Return:	Success:	Ptr to a string allocated with malloc().  The
 *				caller is responsible for freeing the string.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
char *
H5Tget_member_name(hid_t type_id, int membno)
{
    H5T_t	*dt = NULL;
    char	*s = NULL;

    FUNC_ENTER(H5Tget_member_name, NULL);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	H5T_COMPOUND != dt->type) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a compound data type");
    }
    if (membno < 0 || membno >= dt->u.compnd.nmembs) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid member number");
    }

    /* Value */
    s = H5MM_xstrdup(dt->u.compnd.memb[membno].name);
    FUNC_LEAVE(s);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tget_member_offset
 *
 * Purpose:	Returns the byte offset of the beginning of a member with
 *		respect to the beginning of the compound data type datum.
 *
 * Return:	Success:	Byte offset.
 *
 *		Failure:	Zero. Zero is a valid offset, but this
 *				function will fail only if a call to
 *				H5Tget_member_dims() fails with the same
 *				arguments.
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5Tget_member_offset(hid_t type_id, int membno)
{
    H5T_t	*dt = NULL;
    size_t	offset = 0;

    FUNC_ENTER(H5Tget_member_offset, 0);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	H5T_COMPOUND != dt->type) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a compound data type");
    }
    if (membno < 0 || membno >= dt->u.compnd.nmembs) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "invalid member number");
    }

    /* Value */
    offset = dt->u.compnd.memb[membno].offset;

    FUNC_LEAVE(offset);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tget_member_dims
 *
 * Purpose:	Returns the dimensionality of the member.  The dimensions and
 *		permuation vector are returned through arguments DIMS and
 *		PERM, both arrays of at least four elements.  Either (or even
 *		both) may be null pointers.
 *
 * Return:	Success:	A value between zero and four, inclusive.
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5Tget_member_dims(hid_t type_id, int membno,
		   size_t dims[]/*out*/, int perm[]/*out*/)
{
    H5T_t	*dt = NULL;
    intn	ndims, i;

    FUNC_ENTER(H5Tget_member_dims, FAIL);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	H5T_COMPOUND != dt->type) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a compound data type");
    }
    if (membno < 0 || membno >= dt->u.compnd.nmembs) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid member number");
    }

    /* Value */
    ndims = dt->u.compnd.memb[membno].ndims;
    for (i = 0; i < ndims; i++) {
	if (dims[i]) dims[i] = dt->u.compnd.memb[membno].dim[i];
	if (perm[i]) perm[i] = dt->u.compnd.memb[membno].perm[i];
    }

    FUNC_LEAVE(ndims);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tget_member_type
 *
 * Purpose:	Returns the data type of the specified member.	The caller
 *		should invoke H5Tclose() to release resources associated with
 *		the type.
 *
 * Return:	Success:	An OID of a copy of the member data type;
 *				modifying the returned data type does not
 *				modify the member type.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Tget_member_type(hid_t type_id, int membno)
{
    H5T_t	*dt = NULL, *memb_dt = NULL;
    hid_t	memb_type_id;

    FUNC_ENTER(H5Tget_member_type, FAIL);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	H5T_COMPOUND != dt->type) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a compound data type");
    }
    if (membno < 0 || membno >= dt->u.compnd.nmembs) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid member number");
    }
    
    /* Copy data type into an atom */
    if (NULL == (memb_dt = H5T_copy(dt->u.compnd.memb[membno].type))) {
	HRETURN_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL,
		      "unable to copy member data type");
    }
    if ((memb_type_id = H5I_register(H5_DATATYPE, memb_dt)) < 0) {
	H5T_close(memb_dt);
	HRETURN_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL,
		      "can't register data type atom");
    }
    
    FUNC_LEAVE(memb_type_id);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tinsert
 *
 * Purpose:	Adds another member to the compound data type PARENT_ID.  The
 *		new member has a NAME which must be unique within the
 *		compound data type. The OFFSET argument defines the start of
 *		the member in an instance of the compound data type, and
 *		MEMBER_ID is the type of the new member.
 *
 * Note:	All members of a compound data type must be atomic; a
 *		compound data type cannot have a member which is a compound
 *		data type.
 *
 * Return:	Success:	SUCCEED, the PARENT_ID compound data type is
 *				modified to include a copy of the member type
 *				MEMBER_ID.
 *
 *		Failure:	FAIL
 *
 * Errors:
 *
 * Programmer:	Robb Matzke
 *		Monday, December  8, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tinsert(hid_t parent_id, const char *name, size_t offset, hid_t member_id)
{
    H5T_t	*parent = NULL;		/*the compound parent data type */
    H5T_t	*member = NULL;		/*the atomic member type	*/

    FUNC_ENTER(H5Tinsert, FAIL);

    /* Check args */
    if (H5_DATATYPE != H5I_group(parent_id) ||
	NULL == (parent = H5I_object(parent_id)) ||
	H5T_COMPOUND != parent->type) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a compound data type");
    }
    if (parent->locked) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "parent type read-only");
    }
    if (!name || !*name) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no member name");
    }
    if (H5_DATATYPE != H5I_group(member_id) ||
	NULL == (member = H5I_object(member_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    }

    /* Insert */
    if (H5T_insert(parent, name, offset, member) < 0) {
	HRETURN_ERROR(H5E_DATATYPE, H5E_CANTINSERT, FAIL,
		      "can't insert member");
    }
    
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tpack
 *
 * Purpose:	Recursively removes padding from within a compound data type
 *		to make it more efficient (space-wise) to store that data.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tpack(hid_t type_id)
{
    H5T_t	*dt = NULL;

    FUNC_ENTER(H5Tpack, FAIL);

    /* Check args */
    if (H5_DATATYPE != H5I_group(type_id) ||
	NULL == (dt = H5I_object(type_id)) ||
	H5T_COMPOUND != dt->type) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a compound data type");
    }
    if (dt->locked) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "data type is read-only");
    }

    /* Pack */
    if (H5T_pack(dt) < 0) {
	HRETURN_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL,
		      "unable to pack compound data type");
    }
    
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tshare
 *
 * Purpose:	Marks a data type as sharable.  Using the type during the
 *		creation of a dataset will cause the dataset object header to
 *		point to the type in the global heap instead of containing
 *		the type directly in its object header.  Subsequent
 *		modifications to a shared type cause the type to become
 *		unshared.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March 31, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tshare (hid_t loc_id, hid_t type_id)
{
    H5G_t	*loc = NULL;
    H5T_t	*dt = NULL;
    
    FUNC_ENTER (H5Tshare, FAIL);

    /* Check arguments */
    if (NULL==(loc=H5G_loc (loc_id))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a location");
    }
    if (H5_DATATYPE!=H5I_group (type_id) ||
	NULL==(dt=H5I_object (type_id))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    }

    /* Make it sharable */
    if (H5T_share (H5G_fileof (loc), dt)<0) {
	HRETURN_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,
		       "unable to make data type sharable");
    }

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tis_shared
 *
 * Purpose:	Determines if a data type is shared in the specified file.
 *		The TYPE_ID is the type in question and LOC_ID is a file id
 *		or group id (a group id is used only to identify the file).
 *
 * Return:	Success:	TRUE or FALSE
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Friday, April  3, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hbool_t
H5Tis_shared (hid_t loc_id, hid_t type_id)
{
    H5G_t	*loc = NULL;
    H5T_t	*dt = NULL;
    hbool_t	ret_value = FAIL;
    
    FUNC_ENTER (H5Tis_shared, FAIL);

    /* Check arguments */
    if (NULL==(loc=H5G_loc (loc_id))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a location");
    }
    if (H5_DATATYPE!=H5I_group (type_id) ||
	NULL==(dt=H5I_object (type_id))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    }

    /* Is it sharable */
    ret_value = (H5HG_defined (&(dt->sh_heap)) &&
		 dt->sh_file->shared==H5G_fileof(loc)->shared) ? TRUE : FALSE;

    FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tregister_hard
 *
 * Purpose:	Register a hard conversion function for a data type
 *		conversion path.  The path is specified by the source and
 *		destination data types SRC_ID and DST_ID.  A conversion path
 *		can only have one hard function, so FUNC replaces any
 *		previous hard function.
 *
 *		If FUNC is the null pointer then any hard function registered
 *		for this path is removed from this path.  The soft functions
 *		are then used when determining which conversion function is
 *		appropriate for this path.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Friday, January	 9, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tregister_hard(const char *name, hid_t src_id, hid_t dst_id, H5T_conv_t func)
{
    H5T_t	*src = NULL;
    H5T_t	*dst = NULL;
    H5T_path_t	*path = NULL;
    intn	i;

    FUNC_ENTER(H5Tregister_hard, FAIL);

    /* Check args */
    if (!name || !*name) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "conversion must have a name for debugging");
    }
    if (H5_DATATYPE != H5I_group(src_id) ||
	NULL == (src = H5I_object(src_id)) ||
	H5_DATATYPE != H5I_group(dst_id) ||
	NULL == (dst = H5I_object(dst_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    }

    /* Locate or create a new conversion path */
    if (NULL == (path = H5T_path_find(name, src, dst, TRUE, func))) {
	HRETURN_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL,
		      "unable to locate/allocate conversion path");
    }

    /*
     * Notify all other functions to recalculate private data since some
     * functions might cache a list of conversion functions.  For instance,
     * the compound type converter caches a list of conversion functions for
     * the members, so adding a new function should cause the list to be
     * recalculated to use the new function.
     */
    for (i=0; i<H5T_npath_g; i++) {
	if (path != H5T_path_g+i) {
	    H5T_path_g[i].cdata.recalc = TRUE;
	}
    }

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5Tregister_soft
 *
 * Purpose:	Registers a soft conversion function by adding it to the end
 *		of the master soft list and replacing the soft function in
 *		all applicable existing conversion paths.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, January 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tregister_soft(const char *name, H5T_class_t src_cls, H5T_class_t dst_cls,
		 H5T_conv_t func)
{
    intn	i;
    hid_t	src_id, dst_id;
    H5T_cdata_t	cdata;

    FUNC_ENTER(H5Tregister_soft, FAIL);

    /* Check args */
    if (!name || !*name) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		       "conversion must have a name for debugging");
    }
    if (src_cls < 0 || src_cls >= H5T_NCLASSES ||
	dst_cls < 0 || dst_cls >= H5T_NCLASSES) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		      "illegal source or destination data type class");
    }
    if (!func) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
		      "no soft conversion function specified");
    }

    /* Add function to end of master list */
    if (H5T_nsoft_g >= H5T_asoft_g) {
	H5T_asoft_g = MAX(32, 2 * H5T_asoft_g);
	H5T_soft_g = H5MM_xrealloc(H5T_soft_g,
				   H5T_asoft_g * sizeof(H5T_soft_t));
    }
    HDstrncpy (H5T_soft_g[H5T_nsoft_g].name, name, H5T_NAMELEN);
    H5T_soft_g[H5T_nsoft_g].name[H5T_NAMELEN-1] = '\0';
    H5T_soft_g[H5T_nsoft_g].src = src_cls;
    H5T_soft_g[H5T_nsoft_g].dst = dst_cls;
    H5T_soft_g[H5T_nsoft_g].func = func;
    H5T_nsoft_g++;

    /* Replace soft functions of all appropriate paths */
    for (i=0; i<H5T_npath_g; i++) {
	H5T_path_t *path = H5T_path_g + i;
	path->cdata.recalc = TRUE;

	if (path->is_hard ||
	    path->src->type!=src_cls || path->dst->type!=dst_cls) {
	    continue;
	}

	/*
	 * Type conversion functions are app-level, so we need to convert the
	 * data type temporarily to an object id before we query the functions
	 * capabilities.
	 */
	if ((src_id = H5I_register(H5_DATATYPE, H5T_copy(path->src))) < 0 ||
	    (dst_id = H5I_register(H5_DATATYPE, H5T_copy(path->dst))) < 0) {
	    HRETURN_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL,
			  "unable to register data types for conv query");
	}

	HDmemset (&cdata, 0, sizeof cdata);
	cdata.command = H5T_CONV_INIT;
	cdata.stats = H5MM_xcalloc (1, sizeof(H5T_stats_t));
	if ((func) (src_id, dst_id, &cdata, 0, NULL, NULL) >= 0) {
	    /*
	     * Free resources used by the previous conversion function. We
	     * don't really care if this fails since at worst we'll just leak
	     * some memory.  Then initialize the path with new info.
	     */
	    if (path->func) {
		path->cdata.command = H5T_CONV_FREE;
		if ((path->func)(src_id, dst_id, &(path->cdata),
				 0, NULL, NULL)<0) {
#ifdef H5T_DEBUG
		    fprintf (stderr, "H5T: conversion function failed "
			     "to free private data.\n");
#endif
		    H5E_clear();
		}
		H5MM_xfree (path->cdata.stats);
	    }
	    HDstrncpy (path->name, name, H5T_NAMELEN);
	    path->name[H5T_NAMELEN-1] = '\0';
	    path->func = func;
	    path->cdata = cdata;
	} else {
	    H5MM_xfree (cdata.stats);
	}

	/* Release temporary atoms */
	H5I_dec_ref(src_id);
	H5I_dec_ref(dst_id);
	H5E_clear();
    }

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5Tunregister
 *
 * Purpose:	Removes FUNC from all conversion paths.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, January 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tunregister(H5T_conv_t func)
{
    intn	i, j;
    H5T_path_t	*path = NULL;
    hid_t	src_id, dst_id;

    FUNC_ENTER(H5Tunregister, FAIL);

    /* Check args */
    if (!func) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no conversion function");
    }

    /* Remove function from master soft list */
    for (i=H5T_nsoft_g-1; i>=0; --i) {
	if (H5T_soft_g[i].func == func) {
	    HDmemmove(H5T_soft_g+i, H5T_soft_g+i+1,
		      (H5T_nsoft_g - (i+1)) * sizeof(H5T_soft_t));
	    --H5T_nsoft_g;
	}
    }

    /* Remove function from all conversion paths */
    for (i=0; i<H5T_npath_g; i++) {
	path = H5T_path_g + i;

	if (path->func == func) {
	    path->func = NULL;
	    path->is_hard = FALSE;

	    /*
	     * Reset cdata.
	     */
	    path->cdata.command = H5T_CONV_FREE;
	    if ((func)(FAIL, FAIL, &(path->cdata), 0, NULL, NULL)<0) {
#ifdef H5T_DEBUG
		fprintf (stderr, "H5T: conversion function failed to "
			 "free private data.\n");
#endif
		H5E_clear();
	    }
	    H5MM_xfree (path->cdata.stats);
	    HDmemset (&(path->cdata), 0, sizeof(H5T_cdata_t));

	    /*
	     * Choose a new function.
	     */
	    for (j=H5T_nsoft_g-1; j>=0 && !path->func; --j) {
		
		if (path->src->type != H5T_soft_g[j].src ||
		    path->dst->type != H5T_soft_g[j].dst) {
		    continue;
		}

		/*
		 * Conversion functions are app-level, so temporarily create
		 * object id's for the data types.
		 */
		if ((src_id = H5I_register(H5_DATATYPE,
					   H5T_copy(path->src))) < 0 ||
		    (dst_id = H5I_register(H5_DATATYPE,
					   H5T_copy(path->dst))) < 0) {
		    HRETURN_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL,
				  "unable to register conv types for query");
		}

		path->cdata.command = H5T_CONV_INIT;
		path->cdata.stats = H5MM_xcalloc (1, sizeof(H5T_stats_t));
		if ((H5T_soft_g[j].func)(src_id, dst_id, &(path->cdata),
					 0, NULL, NULL) >= 0) {
		    HDstrcpy (path->name, H5T_soft_g[j].name);
		    path->func = H5T_soft_g[j].func;
		} else {
		    H5E_clear();
		    HDmemset (&(path->cdata), 0, sizeof(H5T_cdata_t));
		}
		H5I_dec_ref(src_id);
		H5I_dec_ref(dst_id);
	    }
	} else {
	    /*
	     * If the soft function didn't change then make sure it
	     * recalculates its private data at the next opportunity.
	     */
	    path->cdata.recalc = TRUE;
	}
    }

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5Tfind
 *
 * Purpose:	Finds a conversion function that can handle a conversion from
 *		type SRC_ID to type DST_ID.  The PCDATA argument is a pointer
 *		to a pointer to type conversion data which was created and
 *		initialized by the soft type conversion function of this path
 *		when the conversion function was installed on the path.
 *
 * Return:	Success:	A pointer to a suitable conversion function.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, January 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5T_conv_t
H5Tfind(hid_t src_id, hid_t dst_id, H5T_cdata_t **pcdata)
{
    H5T_conv_t	ret_value = NULL;
    H5T_t	*src = NULL, *dst = NULL;

    FUNC_ENTER(H5Tfind, NULL);

    /* Check args */
    if (H5_DATATYPE != H5I_group(src_id) ||
	NULL == (src = H5I_object(src_id)) ||
	H5_DATATYPE != H5I_group(dst_id) ||
	NULL == (dst = H5I_object(dst_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a data type");
    }
    if (!pcdata) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, NULL,
		       "no address to receive cdata pointer");
    }
    
    /* Find it */
    *pcdata = NULL;
    if (NULL == (ret_value = H5T_find(src, dst, H5T_BKG_NO, pcdata))) {
	HRETURN_ERROR(H5E_DATATYPE, H5E_NOTFOUND, NULL,
		      "conversion function not found");
    }
    
    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * API functions are above; library-private functions are below...
 *------------------------------------------------------------------------- 
 */

/*-------------------------------------------------------------------------
 * Function:	H5T_create
 *
 * Purpose:	Creates a new data type and initializes it to reasonable
 *		values.	 The new data type is SIZE bytes and an instance of
 *		the class TYPE.
 *
 * Return:	Success:	Pointer to the new type.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Friday, December  5, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5T_t *
H5T_create(H5T_class_t type, size_t size)
{
    H5T_t	*dt = NULL;

    FUNC_ENTER(H5T_create, NULL);

    assert(size > 0);

    switch (type) {
    case H5T_INTEGER:
    case H5T_FLOAT:
    case H5T_TIME:
    case H5T_STRING:
    case H5T_BITFIELD:
    case H5T_OPAQUE:
	HRETURN_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, NULL,
		      "type class is not appropriate - use H5Tcopy()");

    case H5T_COMPOUND:
	dt = H5MM_xcalloc(1, sizeof(H5T_t));
	dt->type = type;
	break;

    default:
	HRETURN_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, NULL,
		      "unknown data type class");
    }

    dt->size = size;
    FUNC_LEAVE(dt);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_copy
 *
 * Purpose:	Copies datatype OLD_DT.	 The resulting data type is not
 *		locked.
 *
 * Return:	Success:	Pointer to a new copy of the OLD_DT argument.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5T_t *
H5T_copy(const H5T_t *old_dt)
{
    H5T_t	*new_dt=NULL, *tmp=NULL;
    intn	i;
    char	*s;

    FUNC_ENTER(H5T_copy, NULL);

    /* check args */
    assert(old_dt);

    /* copy */
    new_dt = H5MM_xcalloc(1, sizeof(H5T_t));
    *new_dt = *old_dt;
    new_dt->locked = FALSE;

    if (H5T_COMPOUND == new_dt->type) {
	/*
	 * Copy all member fields to new type, then overwrite the
	 * name and type fields of each new member with copied values.
	 * That is, H5T_copy() is a deep copy.
	 */
	new_dt->u.compnd.memb = H5MM_xmalloc(new_dt->u.compnd.nmembs *
					     sizeof(H5T_member_t));
	HDmemcpy(new_dt->u.compnd.memb, old_dt->u.compnd.memb,
		 new_dt->u.compnd.nmembs * sizeof(H5T_member_t));
	
	for (i = 0; i < new_dt->u.compnd.nmembs; i++) {
	    s = new_dt->u.compnd.memb[i].name;
	    new_dt->u.compnd.memb[i].name = H5MM_xstrdup(s);
	    tmp = H5T_copy (old_dt->u.compnd.memb[i].type);
	    new_dt->u.compnd.memb[i].type = tmp;
	}
    }
    
    FUNC_LEAVE(new_dt);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_close
 *
 * Purpose:	Frees a data type and all associated memory.  If the data
 *		type is locked then nothing happens.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Monday, December  8, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_close(H5T_t *dt)
{
    intn	i;

    FUNC_ENTER(H5T_close, FAIL);

    assert(dt);

    if (!dt->locked) {
	if (dt && H5T_COMPOUND == dt->type) {
	    for (i = 0; i < dt->u.compnd.nmembs; i++) {
		H5MM_xfree(dt->u.compnd.memb[i].name);
	    }
	    H5MM_xfree(dt->u.compnd.memb);
	    H5MM_xfree(dt);

	} else if (dt) {
	    H5MM_xfree(dt);
	}
    }
    
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_share
 *
 * Purpose:	Causes a data type to be marked as sharable.  If the data
 *		type isn't already marked sharable in the specified file then
 *		it is written to the global heap of that file and the heap
 *		location information is added to the sh_file and sh_heap
 *		fields of the data type.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March 31, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_share (H5F_t *f, H5T_t *dt)
{
    FUNC_ENTER (H5T_share, FAIL);
    
    /* Check args */
    assert (f);
    assert (dt);

    /*
     * If the type is sharable in some other file then unshare it first. A
     * type can only be sharable in one file at a time.
     */
    if (H5HG_defined (&(dt->sh_heap)) && f->shared!=dt->sh_file->shared) {
	H5T_unshare (dt);
	H5E_clear (); /*don't really care if it fails*/
    }

    /*
     * Write the message to the global heap if it isn't already shared in
     * this file.
     */
    if (!H5HG_defined (&(dt->sh_heap))) {
	if (H5O_share (f, H5O_DTYPE, dt, &(dt->sh_heap))<0) {
	    HRETURN_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,
			   "unable to store data type message in global heap");
	}
	dt->sh_file = f;
    }
    
    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_unshare
 *
 * Purpose:	If a data type is in the global heap then this function
 *		removes that information from the H5T_t struct.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March 31, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_unshare (H5T_t *dt)
{
    FUNC_ENTER (H5T_unshare, FAIL);

    /* Check args */
    assert (dt);

    H5HG_undef (&(dt->sh_heap));
    dt->sh_file = NULL;

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_invalidate_cb
 *
 * Purpose:	This is a callback function for H5I_search(). When a file is
 *		closed we scan through the data type list and invalidate
 *		shared info for all types that have sharing enabled for the
 *		specified file.  This insures that we don't having dangling 
 *		pointers from data types to files.  We have to do this with
 *		data types but not datasets because a dataset_id always
 *		corresponds to an open object header which prevents the file
 *		from closing in the first place, but a data type can exist
 *		independent of a file and doesn't have an object header.
 *
 * Return:	Success:	0, this function never returns a non-zero
 *				value because that would terminate
 *				H5I_search().
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *              Friday, April  3, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5T_invalidate_cb (void *obj, const void *call_data)
{
    H5T_t	*dt = (H5T_t *)obj;
    const H5F_t	*f = (const H5F_t*)call_data; /*used only for comparison*/
    
    FUNC_ENTER (H5T_invalidate, 0);

    if (H5HG_defined (&(dt->sh_heap)) && dt->sh_file->shared==f->shared) {
	H5T_unshare (dt);
	H5E_clear ();
    }

    FUNC_LEAVE (0);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_is_atomic
 *
 * Purpose:	Determines if a data type is an atomic type.
 *
 * Return:	Success:	TRUE, FALSE
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hbool_t
H5T_is_atomic(const H5T_t *dt)
{
    FUNC_ENTER(H5T_is_atomic, FAIL);

    assert(dt);

    FUNC_LEAVE(H5T_COMPOUND == dt->type ? FALSE : TRUE);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_get_size
 *
 * Purpose:	Determines the total size of a data type in bytes.
 *
 * Return:	Success:	Size of the data type in bytes.	 The size of
 *				the data type is the size of an instance of
 *				that data type.
 *
 *		Failure:	0 (valid data types are never zero size)
 *
 * Programmer:	Robb Matzke
 *		Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5T_get_size(const H5T_t *dt)
{
    FUNC_ENTER(H5T_get_size, 0);

    /* check args */
    assert(dt);

    FUNC_LEAVE(dt->size);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_insert
 *
 * Purpose:	Adds a new MEMBER to the compound data type PARENT.  The new
 *		member will have a NAME that is unique within PARENT and an
 *		instance of PARENT will have the member begin at byte offset
 *		OFFSET from the beginning.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Monday, December  8, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_insert(H5T_t *parent, const char *name, size_t offset, const H5T_t *member)
{
    intn		    i;

    FUNC_ENTER(H5T_insert, FAIL);

    /* check args */
    assert(parent && H5T_COMPOUND == parent->type);
    assert(!parent->locked);
    assert(member);
    assert(name && *name);

    /* Does NAME already exist in PARENT? */
    for (i = 0; i < parent->u.compnd.nmembs; i++) {
	if (!HDstrcmp(parent->u.compnd.memb[i].name, name)) {
	    HRETURN_ERROR(H5E_DATATYPE, H5E_CANTINSERT, FAIL,
			  "member name is not unique");
	}
    }

    /* Does the new member overlap any existing member ? */
    for (i = 0; i < parent->u.compnd.nmembs; i++) {
	if ((offset < parent->u.compnd.memb[i].offset &&
	     offset + member->size > parent->u.compnd.memb[i].offset) ||
	    (parent->u.compnd.memb[i].offset < offset &&
	     parent->u.compnd.memb[i].offset +
	     parent->u.compnd.memb[i].type->size > offset)) {
	    HRETURN_ERROR(H5E_DATATYPE, H5E_CANTINSERT, FAIL,
			  "member overlaps with another member");
	}
    }

    /* Increase member array if necessary */
    if (parent->u.compnd.nmembs >= parent->u.compnd.nalloc) {
	parent->u.compnd.nalloc += H5T_COMPND_INC;
	parent->u.compnd.memb = H5MM_xrealloc(parent->u.compnd.memb,
					      (parent->u.compnd.nalloc *
					       sizeof(H5T_member_t)));
    }

    /* Add member to end of member array */
    H5T_unshare (parent);
    i = parent->u.compnd.nmembs;
    parent->u.compnd.memb[i].name = H5MM_xstrdup(name);
    parent->u.compnd.memb[i].offset = offset;
    parent->u.compnd.memb[i].ndims = 0;		/*defaults to scalar */
    parent->u.compnd.memb[i].type = H5T_copy (member);

    parent->u.compnd.nmembs++;
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_pack
 *
 * Purpose:	Recursively packs a compound data type by removing padding
 *		bytes. This is done in place (that is, destructively).
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_pack(H5T_t *dt)
{
    int		i;
    size_t	offset;

    FUNC_ENTER(H5T_pack, FAIL);

    assert(dt);
    assert(!dt->locked);

    H5T_unshare (dt);
    if (H5T_COMPOUND == dt->type) {
	/* Recursively pack the members */
	for (i = 0; i < dt->u.compnd.nmembs; i++) {
	    if (H5T_pack(dt->u.compnd.memb[i].type) < 0) {
		HRETURN_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL,
			      "unable to pack part of a compound data type");
	    }
	}

	/* Remove padding between members */
	H5T_sort_by_offset(dt);
	for (i = 0, offset = 0; i < dt->u.compnd.nmembs; i++) {
	    dt->u.compnd.memb[i].offset = offset;
	    offset += H5T_get_size (dt->u.compnd.memb[i].type);
	}

	/* Change total size */
	dt->size = MAX(1, offset);
    }
    
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_sort_by_offset
 *
 * Purpose:	Sorts the members of a compound data type by their offsets.
 *		This even works for locked data types since it doesn't change
 *		the value of the type.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_sort_by_offset(H5T_t *dt)
{
    int		i, j, nmembs;
    hbool_t	swapped;

    FUNC_ENTER(H5T_sort_by_offset, FAIL);

    /* Check args */
    assert(dt);
    assert(H5T_COMPOUND == dt->type);

    /* Use a bubble sort because we can short circuit */
    nmembs = dt->u.compnd.nmembs;
    for (i=nmembs-1, swapped=TRUE; i>0 && swapped; --i) {
	for (j=0, swapped=FALSE; j<i; j++) {
	    if (dt->u.compnd.memb[j].offset > dt->u.compnd.memb[j+1].offset) {
		H5T_member_t tmp = dt->u.compnd.memb[j];
		dt->u.compnd.memb[j] = dt->u.compnd.memb[j+1];
		dt->u.compnd.memb[j+1] = tmp;
		swapped = TRUE;
	    }
	}
    }

#ifndef NDEBUG
    /* I never trust a sort :-) */
    for (i = 0; i < dt->u.compnd.nmembs - 1; i++) {
	assert(dt->u.compnd.memb[i].offset < dt->u.compnd.memb[i + 1].offset);
    }
#endif

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_cmp
 *
 * Purpose:	Compares two data types.
 *
 * Return:	Success:	0 if DT1 and DT2 are equal.
 *				<0 if DT1 is less than DT2.
 *				>0 if DT1 is greater than DT2.
 *
 *		Failure:	0, never fails
 *
 * Programmer:	Robb Matzke
 *		Wednesday, December 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5T_cmp(const H5T_t *dt1, const H5T_t *dt2)
{
    intn	*idx1 = NULL, *idx2 = NULL;
    intn	ret_value = 0;
    intn	i, j, tmp;
    hbool_t	swapped;

    FUNC_ENTER(H5T_equal, 0);

    /* the easy case */
    if (dt1 == dt2) HGOTO_DONE(0);
    assert(dt1);
    assert(dt2);

    /* compare */
    if (dt1->type < dt2->type) HGOTO_DONE(-1);
    if (dt1->type > dt2->type) HGOTO_DONE(1);

    if (dt1->size < dt2->size) HGOTO_DONE(-1);
    if (dt1->size > dt2->size) HGOTO_DONE(1);

    if (H5T_COMPOUND == dt1->type) {
	/*
	 * Compound data types...
	 */
	if (dt1->u.compnd.nmembs < dt2->u.compnd.nmembs) HGOTO_DONE(-1);
	if (dt1->u.compnd.nmembs > dt2->u.compnd.nmembs) HGOTO_DONE(1);

	/* Build an index for each type so the names are sorted */
	idx1 = H5MM_xmalloc(dt1->u.compnd.nmembs * sizeof(intn));
	idx2 = H5MM_xmalloc(dt1->u.compnd.nmembs * sizeof(intn));
	for (i=0; i<dt1->u.compnd.nmembs; i++) idx1[i] = idx2[i] = i;
	for (i=dt1->u.compnd.nmembs-1, swapped=TRUE; swapped && i>=0; --i) {
	    for (j=0, swapped=FALSE; j<i; j++) {
		if (HDstrcmp(dt1->u.compnd.memb[idx1[j]].name,
			     dt1->u.compnd.memb[idx1[j+1]].name) > 0) {
		    tmp = idx1[j];
		    idx1[j] = idx1[j+1];
		    idx1[j+1] = tmp;
		    swapped = TRUE;
		}
	    }
	}
	for (i=dt2->u.compnd.nmembs-1, swapped=TRUE; swapped && i>=0; --i) {
	    for (j=0, swapped=FALSE; j<i; j++) {
		if (HDstrcmp(dt2->u.compnd.memb[idx2[j]].name,
			     dt2->u.compnd.memb[idx2[j+1]].name) > 0) {
		    tmp = idx2[j];
		    idx2[j] = idx2[j+1];
		    idx2[j+1] = tmp;
		    swapped = TRUE;
		}
	    }
	}

#ifdef H5T_DEBUG
	/* I don't quite trust the code above yet :-)  --RPM */
	for (i=0; i<dt1->u.compnd.nmembs-1; i++) {
	    assert(HDstrcmp(dt1->u.compnd.memb[idx1[i]].name,
			    dt1->u.compnd.memb[idx1[i + 1]].name));
	    assert(HDstrcmp(dt2->u.compnd.memb[idx2[i]].name,
			    dt2->u.compnd.memb[idx2[i + 1]].name));
	}
#endif

	/* Compare the members */
	for (i=0; i<dt1->u.compnd.nmembs; i++) {
	    tmp = HDstrcmp(dt1->u.compnd.memb[idx1[i]].name,
			   dt2->u.compnd.memb[idx2[i]].name);
	    if (tmp < 0) HGOTO_DONE(-1);
	    if (tmp > 0) HGOTO_DONE(1);

	    if (dt1->u.compnd.memb[idx1[i]].offset <
		dt2->u.compnd.memb[idx2[i]].offset) HGOTO_DONE(-1);
	    if (dt1->u.compnd.memb[idx1[i]].offset >
		dt2->u.compnd.memb[idx2[i]].offset) HGOTO_DONE(1);

	    if (dt1->u.compnd.memb[idx1[i]].ndims <
		dt2->u.compnd.memb[idx2[i]].ndims) HGOTO_DONE(-1);
	    if (dt1->u.compnd.memb[idx1[i]].ndims >
		dt2->u.compnd.memb[idx2[i]].ndims) HGOTO_DONE(1);

	    for (j=0; j<dt1->u.compnd.memb[idx1[i]].ndims; j++) {
		if (dt1->u.compnd.memb[idx1[i]].dim[j] <
		    dt2->u.compnd.memb[idx2[i]].dim[j]) HGOTO_DONE(-1);
		if (dt1->u.compnd.memb[idx1[i]].dim[j] >
		    dt2->u.compnd.memb[idx2[i]].dim[j]) HGOTO_DONE(1);
	    }

	    for (j=0; j<dt1->u.compnd.memb[idx1[i]].ndims; j++) {
		if (dt1->u.compnd.memb[idx1[i]].perm[j] <
		    dt2->u.compnd.memb[idx2[i]].perm[j]) HGOTO_DONE(-1);
		if (dt1->u.compnd.memb[idx1[i]].perm[j] >
		    dt2->u.compnd.memb[idx2[i]].perm[j]) HGOTO_DONE(1);
	    }

	    tmp = H5T_cmp(dt1->u.compnd.memb[idx1[i]].type,
			  dt2->u.compnd.memb[idx2[i]].type);
	    if (tmp < 0) HGOTO_DONE(-1);
	    if (tmp > 0) HGOTO_DONE(1);
	}

    } else {
	/*
	 * Atomic data types...
	 */
	if (dt1->u.atomic.order < dt2->u.atomic.order) HGOTO_DONE(-1);
	if (dt1->u.atomic.order > dt2->u.atomic.order) HGOTO_DONE(1);

	if (dt1->u.atomic.prec < dt2->u.atomic.prec) HGOTO_DONE(-1);
	if (dt1->u.atomic.prec > dt2->u.atomic.prec) HGOTO_DONE(1);

	if (dt1->u.atomic.offset < dt2->u.atomic.offset) HGOTO_DONE(-1);
	if (dt1->u.atomic.offset > dt2->u.atomic.offset) HGOTO_DONE(1);

	if (dt1->u.atomic.lsb_pad < dt2->u.atomic.lsb_pad) HGOTO_DONE(-1);
	if (dt1->u.atomic.lsb_pad > dt2->u.atomic.lsb_pad) HGOTO_DONE(1);

	if (dt1->u.atomic.msb_pad < dt2->u.atomic.msb_pad) HGOTO_DONE(-1);
	if (dt1->u.atomic.msb_pad > dt2->u.atomic.msb_pad) HGOTO_DONE(1);

	switch (dt1->type) {
	case H5T_INTEGER:
	    if (dt1->u.atomic.u.i.sign < dt2->u.atomic.u.i.sign) {
		HGOTO_DONE(-1);
	    }
	    if (dt1->u.atomic.u.i.sign > dt2->u.atomic.u.i.sign) {
		HGOTO_DONE(1);
	    }
	    break;

	case H5T_FLOAT:
	    if (dt1->u.atomic.u.f.sign < dt2->u.atomic.u.f.sign) {
		HGOTO_DONE(-1);
	    }
	    if (dt1->u.atomic.u.f.sign > dt2->u.atomic.u.f.sign) {
		HGOTO_DONE(1);
	    }

	    if (dt1->u.atomic.u.f.epos < dt2->u.atomic.u.f.epos) {
		HGOTO_DONE(-1);
	    }
	    if (dt1->u.atomic.u.f.epos > dt2->u.atomic.u.f.epos) {
		HGOTO_DONE(1);
	    }

	    if (dt1->u.atomic.u.f.esize <
		dt2->u.atomic.u.f.esize) HGOTO_DONE(-1);
	    if (dt1->u.atomic.u.f.esize >
		dt2->u.atomic.u.f.esize) HGOTO_DONE(1);

	    if (dt1->u.atomic.u.f.ebias <
		dt2->u.atomic.u.f.ebias) HGOTO_DONE(-1);
	    if (dt1->u.atomic.u.f.ebias >
		dt2->u.atomic.u.f.ebias) HGOTO_DONE(1);

	    if (dt1->u.atomic.u.f.mpos < dt2->u.atomic.u.f.mpos) {
		HGOTO_DONE(-1);
	    }
	    if (dt1->u.atomic.u.f.mpos > dt2->u.atomic.u.f.mpos) {
		HGOTO_DONE(1);
	    }

	    if (dt1->u.atomic.u.f.msize <
		dt2->u.atomic.u.f.msize) HGOTO_DONE(-1);
	    if (dt1->u.atomic.u.f.msize >
		dt2->u.atomic.u.f.msize) HGOTO_DONE(1);

	    if (dt1->u.atomic.u.f.norm < dt2->u.atomic.u.f.norm) {
		HGOTO_DONE(-1);
	    }
	    if (dt1->u.atomic.u.f.norm > dt2->u.atomic.u.f.norm) {
		HGOTO_DONE(1);
	    }

	    if (dt1->u.atomic.u.f.pad < dt2->u.atomic.u.f.pad) {
		HGOTO_DONE(-1);
	    }
	    if (dt1->u.atomic.u.f.pad > dt2->u.atomic.u.f.pad) {
		HGOTO_DONE(1);
	    }

	    break;

	case H5T_TIME:
	    /*void */
	    break;

	case H5T_STRING:
	    if (dt1->u.atomic.u.s.cset < dt1->u.atomic.u.s.cset) {
		HGOTO_DONE(-1);
	    }
	    if (dt1->u.atomic.u.s.cset > dt1->u.atomic.u.s.cset) {
		HGOTO_DONE(1);
	    }

	    if (dt1->u.atomic.u.s.pad < dt1->u.atomic.u.s.pad) {
		HGOTO_DONE(-1);
	    }
	    if (dt1->u.atomic.u.s.pad > dt1->u.atomic.u.s.pad) {
		HGOTO_DONE(1);
	    }

	    break;

	case H5T_BITFIELD:
	    /*void */
	    break;

	case H5T_OPAQUE:
	    /*void */
	    break;

	default:
	    assert("not implemented yet" && 0);
	}
    }

  done:
    H5MM_xfree(idx1);
    H5MM_xfree(idx2);

    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_find
 *
 * Purpose:	Finds a conversion function for the specified path.  If the
 *		source and destination types are the same and NEED_BKG is not
 *		H5T_BKG_YES then a pointer to the H5T_conv_noop() function is
 *		returned.
 *
 *		NAME is assigned to the conversion path if the path is
 *		created.  The name is only for debugging.
 *
 * Return:	Success:	A pointer to an appropriate conversion
 *				function.  The PCDATA argument is initialized
 *				to point to type conversion data which should
 *				be passed to the type conversion function.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January 14, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5T_conv_t
H5T_find(const H5T_t *src, const H5T_t *dst, H5T_bkg_t need_bkg,
	 H5T_cdata_t **pcdata/*out*/)
{
    H5T_path_t		*path = NULL;
    H5T_conv_t		ret_value = NULL;
    static H5T_cdata_t	noop_cdata;

    FUNC_ENTER(H5T_find, NULL);

    if (!noop_cdata.stats) {
	noop_cdata.stats = H5MM_xcalloc (1, sizeof(H5T_stats_t));
    }

    /* No-op case */
    if (need_bkg<H5T_BKG_YES && 0==H5T_cmp(src, dst)) {
	*pcdata = &noop_cdata;
	HRETURN(H5T_conv_noop);
    }
    
    /* Find it */
    if (NULL == (path = H5T_path_find(NULL, src, dst, TRUE, NULL))) {
	HRETURN_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL,
		      "unable to create conversion path");
    }

    if ((ret_value=path->func)) {
	*pcdata = &(path->cdata);
    } else {
	HRETURN_ERROR(H5E_DATATYPE, H5E_NOTFOUND, NULL,
		      "no conversion function for that path");
    }
    
    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5T_path_find
 *
 * Purpose:	Finds the path which converts type SRC_ID to type DST_ID.  If
 *		the path isn't found and CREATE is non-zero then a new path
 *		is created.  If FUNC is non-null then it is registered as the
 *		hard function for that path.
 *
 * 		If a path is created then NAME is used for debugging.
 *
 * Return:	Success:	Pointer to the path, valid until the path
 *				database is modified.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, January 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5T_path_t *
H5T_path_find(const char *name, const H5T_t *src, const H5T_t *dst,
	      hbool_t create, H5T_conv_t func)
{
    intn	lt = 0;			/*left edge (inclusive)		*/
    intn	rt = H5T_npath_g;	/*right edge (exclusive)	*/
    intn	md = 0;			/*middle			*/
    intn	cmp = -1;		/*comparison result		*/
    H5T_path_t	*path = NULL;		/*path found			*/
    int		i;
    hid_t	src_id, dst_id;

    FUNC_ENTER(H5T_path_find, NULL);

    /* Check args */
    assert(src);
    assert(dst);

    /* Binary search */
    while (lt < rt) {
	md = (lt + rt) / 2;

	cmp = H5T_cmp(src, H5T_path_g[md].src);
	if (0 == cmp) cmp = H5T_cmp(dst, H5T_path_g[md].dst);

	if (cmp < 0) {
	    rt = md;
	} else if (cmp > 0) {
	    lt = md + 1;
	} else {
	    HRETURN(H5T_path_g + md);
	}
    }

    /* Insert */
    if (create) {
	if (H5T_npath_g >= H5T_apath_g) {
	    H5T_apath_g = MAX(64, 2 * H5T_apath_g);
	    H5T_path_g = H5MM_xrealloc(H5T_path_g,
				       H5T_apath_g * sizeof(H5T_path_t));
	}
	if (cmp > 0) md++;

	/* make room */
	HDmemmove(H5T_path_g + md + 1, H5T_path_g + md,
		  (H5T_npath_g - md) * sizeof(H5T_path_t));
	H5T_npath_g++;

	/* insert */
	path = H5T_path_g + md;
	HDmemset(path, 0, sizeof(H5T_path_t));
	path->src = H5T_copy(src);
	path->dst = H5T_copy(dst);

	/* Associate a function with the path if possible */
	if (func) {
	    HDstrncpy (path->name, name, H5T_NAMELEN);
	    path->name[H5T_NAMELEN-1] = '\0';
	    path->func = func;
	    path->is_hard = TRUE;
	    path->cdata.command = H5T_CONV_INIT;
	    path->cdata.stats = H5MM_xcalloc (1, sizeof(H5T_stats_t));
	    if ((src_id=H5I_register(H5_DATATYPE, H5T_copy(path->src))) < 0 ||
		(dst_id=H5I_register(H5_DATATYPE, H5T_copy(path->dst))) < 0) {
		HRETURN_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, NULL,
			      "unable to register conv types for query");
	    }
	    if ((func)(src_id, dst_id, &(path->cdata), 0, NULL, NULL)<0) {
#ifdef H5T_DEBUG
		fprintf (stderr, "H5T: conversion function init "
			 "failed\n");
#endif
		H5E_clear(); /*ignore the failure*/
	    }
	    H5I_dec_ref(src_id);
	    H5I_dec_ref(dst_id);
	} else {
	    /* Locate a soft function */
	    for (i=H5T_nsoft_g-1; i>=0 && !path->func; --i) {
		if (src->type!=H5T_soft_g[i].src ||
		    dst->type!=H5T_soft_g[i].dst) {
		    continue;
		}
		if ((src_id=H5I_register(H5_DATATYPE,
					 H5T_copy(path->src))) < 0 ||
		    (dst_id=H5I_register(H5_DATATYPE,
					 H5T_copy(path->dst))) < 0) {
		    HRETURN_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, NULL,
				  "unable to register conv types for query");
		}
		path->cdata.command = H5T_CONV_INIT;
		path->cdata.stats = H5MM_xcalloc (1, sizeof(H5T_stats_t));
		if ((H5T_soft_g[i].func) (src_id, dst_id, &(path->cdata),
					  H5T_CONV_INIT, NULL, NULL) < 0) {
		    HDmemset (&(path->cdata), 0, sizeof(H5T_cdata_t));
		    H5E_clear(); /*ignore the error*/
		} else {
		    HDstrcpy (path->name, H5T_soft_g[i].name);
		    path->func = H5T_soft_g[i].func;
		}
		H5I_dec_ref(src_id);
		H5I_dec_ref(dst_id);
	    }
	}
    }
    FUNC_LEAVE(path);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_timer_begin
 *
 * Purpose:	Start a timer for a data type conversion.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Friday, April 17, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5T_timer_begin (H5_timer_t *timer, H5T_cdata_t *cdata)
{
    assert (timer);
    assert (cdata);
#ifdef H5T_DEBUG
    assert (cdata->stats);
    H5_timer_begin (timer);
#endif
}


/*-------------------------------------------------------------------------
 * Function:	H5T_timer_end
 *
 * Purpose:	Ends a timer for a data type conversion
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Friday, April 17, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5T_timer_end (H5_timer_t *timer, H5T_cdata_t *cdata, size_t nelmts)
{
    assert (timer);
    assert (cdata);
#ifdef H5T_DEBUG
    assert (cdata->stats);
    H5_timer_end (&(cdata->stats->timer), timer);
    cdata->stats->ncalls++;
    cdata->stats->nelmts += nelmts;
#endif
}


/*-------------------------------------------------------------------------
 * Function:	H5T_debug
 *
 * Purpose:	Prints information about a data type.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_debug(H5T_t *dt, FILE * stream)
{
    const char	*s = "";
    int		i, j;
    uint64	tmp;

    FUNC_ENTER(H5T_debug, FAIL);

    /* Check args */
    assert(dt);
    assert(stream);

    switch (dt->type) {
    case H5T_INTEGER:
	s = "int";
	break;
    case H5T_FLOAT:
	s = "float";
	break;
    case H5T_TIME:
	s = "time";
	break;
    case H5T_STRING:
	s = "str";
	break;
    case H5T_BITFIELD:
	s = "bits";
	break;
    case H5T_OPAQUE:
	s = "opaque";
	break;
    case H5T_COMPOUND:
	s = "struct";
	break;
    default:
	s = "";
	break;
    }

    fprintf(stream, "%s%s {nbytes=%lu",
	    s, dt->locked ? "[!]" : "", (unsigned long)(dt->size));

    if (H5T_is_atomic(dt)) {
	switch (dt->u.atomic.order) {
	case H5T_ORDER_BE:
	    s = "BE";
	    break;
	case H5T_ORDER_LE:
	    s = "LE";
	    break;
	case H5T_ORDER_VAX:
	    s = "VAX";
	    break;
	case H5T_ORDER_NONE:
	    s = "NONE";
	    break;
	default:
	    s = "order?";
	    break;
	}
	fprintf(stream, ", %s", s);

	if (dt->u.atomic.offset) {
	    fprintf(stream, ", offset=%lu",
		    (unsigned long) (dt->u.atomic.offset));
	}
	if (dt->u.atomic.prec != 8 * dt->size) {
	    fprintf(stream, ", prec=%lu",
		    (unsigned long) (dt->u.atomic.prec));
	}
	switch (dt->type) {
	case H5T_INTEGER:
	    switch (dt->u.atomic.u.i.sign) {
	    case H5T_SGN_NONE:
		s = "unsigned";
		break;
	    case H5T_SGN_2:
		s = NULL;
		break;
	    default:
		s = "sign?";
		break;
	    }
	    if (s)
		fprintf(stream, ", %s", s);
	    break;

	case H5T_FLOAT:
	    switch (dt->u.atomic.u.f.norm) {
	    case H5T_NORM_IMPLIED:
		s = "implied";
		break;
	    case H5T_NORM_MSBSET:
		s = "msbset";
		break;
	    case H5T_NORM_NONE:
		s = "no-norm";
		break;
	    default:
		s = "norm?";
		break;
	    }
	    fprintf(stream, ", sign=%lu+1",
		    (unsigned long) (dt->u.atomic.u.f.sign));
	    fprintf(stream, ", mant=%lu+%lu (%s)",
		    (unsigned long) (dt->u.atomic.u.f.mpos),
		    (unsigned long) (dt->u.atomic.u.f.msize), s);
	    fprintf(stream, ", exp=%lu+%lu",
		    (unsigned long) (dt->u.atomic.u.f.epos),
		    (unsigned long) (dt->u.atomic.u.f.esize));
	    tmp = dt->u.atomic.u.f.ebias >> 32;
	    if (tmp) {
		size_t hi = tmp;
		size_t lo = dt->u.atomic.u.f.ebias & 0xffffffff;
		fprintf(stream, " bias=0x%08lx%08lx",
			(unsigned long)hi, (unsigned long)lo);
	    } else {
		size_t lo = dt->u.atomic.u.f.ebias & 0xffffffff;
		fprintf(stream, " bias=0x%08lx", (unsigned long)lo);
	    }
	    break;

	default:
	    /* No additional info */
	    break;
	}
    } else {
	for (i = 0; i < dt->u.compnd.nmembs; i++) {
	    fprintf(stream, "\n\"%s\" @%lu",
		    dt->u.compnd.memb[i].name,
		    (unsigned long) (dt->u.compnd.memb[i].offset));
	    if (dt->u.compnd.memb[i].ndims) {
		fprintf(stream, "[");
		for (j = 0; j < dt->u.compnd.memb[i].ndims; j++) {
		    fprintf(stream, "%s%lu", j ? ", " : "",
			    (unsigned long)(dt->u.compnd.memb[i].dim[j]));
		}
		fprintf(stream, "]");
	    }
	    fprintf(stream, " ");
	    H5T_debug(dt->u.compnd.memb[i].type, stream);
	}
	fprintf(stream, "\n");
    }
    fprintf(stream, "}");

    FUNC_LEAVE(SUCCEED);
}
