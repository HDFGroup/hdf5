/****************************************************************************
* NCSA HDF                                                                 *
* Software Development Group                                               *
* National Center for Supercomputing Applications                          *
* University of Illinois at Urbana-Champaign                               *
* 605 E. Springfield, Champaign IL 61820                                   *
*                                                                          *
* For conditions of distribution and use, see the accompanying             *
* hdf/COPYING file.                                                        *
*                                                                          *
****************************************************************************/

#ifdef RCSID
static char RcsId[] = "@(#)$Revision$";
#endif

#define H5T_PACKAGE		/*suppress error about including H5Tpkg	*/

#include <H5private.h>		/*generic functions 			*/
#include <H5Aprivate.h>		/*atom functions 			*/
#include <H5Eprivate.h>		/*error handling 			*/
#include <H5Mprivate.h>		/*meta data 				*/
#include <H5MMprivate.h>	/*memory management			*/
#include <H5Pprivate.h>		/*data space 				*/
#include <H5Tpkg.h>		/*data-type functions 			*/

#define PABLO_MASK	H5T_mask

#define H5T_COMPND_INC	64	/*typical max numb of members per struct*/

/* Interface initialization */
static intn interface_initialize_g = FALSE;
#define INTERFACE_INIT H5T_init_interface
static void H5T_term_interface (void);

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
H5T_init_interface (void)
{
   herr_t ret_value = SUCCEED;
   FUNC_ENTER (H5T_init_interface, FAIL);

   /* Initialize the atom group for the file IDs */
   if ((ret_value=H5Ainit_group (H5_DATATYPE, H5A_DATATYPEID_HASHSIZE,
				 H5T_RESERVED_ATOMS,
				 (herr_t (*)(void*))H5T_close))!=FAIL) {
      ret_value=H5_add_exit (&H5T_term_interface);
   }

   /* Initialize pre-defined data types */
   ret_value = H5T_init ();
   
   FUNC_LEAVE (ret_value);
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
H5T_term_interface (void)
{
   H5Adestroy_group (H5_DATATYPE);
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
 *		ARGS      BADVALUE      Invalid size. 
 *		DATATYPE  CANTINIT      Can't create type. 
 *		DATATYPE  CANTREGISTER  Can't register data type atom. 
 *
 * Programmer:	Robb Matzke
 *              Friday, December  5, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Tcreate (H5T_class_t type, size_t size)
{
   H5T_t	*dt = NULL;
   hid_t	ret_value = FAIL;
   
   FUNC_ENTER (H5Tcreate, FAIL);

   /* check args */
   if (size<=0) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "invalid size");
   }
   
   /* create the type */
   if (NULL==(dt=H5T_create (type, size))) {
      HRETURN_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL, "can't create type");
   }
   
   /* Make it an atom */
   if ((ret_value=H5Aregister_atom (H5_DATATYPE, dt))<0) {
      HRETURN_ERROR (H5E_DATATYPE, H5E_CANTREGISTER, FAIL,
		     "can't register data type atom");
   }

   FUNC_LEAVE (ret_value);
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
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Tcopy (hid_t type_id)
{
   H5T_t	*dt = NULL;
   H5T_t	*new_dt = NULL;
   hid_t	ret_value = FAIL;
   
   FUNC_ENTER (H5Tcopy, FAIL);
   H5ECLEAR;

   /* check args */
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(dt=H5Aatom_object (type_id))) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
   }

   /* copy */
   if (NULL==(new_dt = H5T_copy (dt))) {
      HRETURN_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL, "can't copy");
   }

   /* atomize result */
   if ((ret_value=H5Aregister_atom (H5_DATATYPE, new_dt))<0) {
      H5T_close (new_dt);
      HRETURN_ERROR (H5E_DATATYPE, H5E_CANTREGISTER, FAIL,
		     "can't register data type atom");
   }

   FUNC_LEAVE (ret_value);
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
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tclose (hid_t type_id)
{
   H5T_t	*dt = NULL;
   
   FUNC_ENTER (H5Tclose, FAIL);
   H5ECLEAR;

   /* check args */
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(dt=H5Aatom_object (type_id))) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
   }
   if (dt->locked) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "predefined data type");
   }

   /* When the reference count reaches zero the resources are freed */
   if (H5A_dec_ref (type_id)<0) {
      HRETURN_ERROR (H5E_ATOM, H5E_BADATOM, FAIL, "problem freeing id");
   }

   FUNC_LEAVE (SUCCEED);
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
 *              Wednesday, December 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hbool_t
H5Tequal (hid_t type1_id, hid_t type2_id)
{
   const H5T_t	*dt1 = NULL;
   const H5T_t	*dt2 = NULL;
   hbool_t	ret_value = FAIL;
   
   FUNC_ENTER (H5Tequal, FAIL);

   /* check args */
   if (H5_DATATYPE!=H5Aatom_group (type1_id) ||
       NULL==(dt1=H5Aatom_object (type1_id)) ||
       H5_DATATYPE!=H5Aatom_group (type2_id) ||
       NULL==(dt2=H5Aatom_object (type2_id))) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
   }

   ret_value = (0==H5T_cmp (dt1, dt2));

   FUNC_LEAVE (ret_value);
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
 *              Monday, December  8, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5T_class_t
H5Tget_class (hid_t type_id)
{
   H5T_t	*dt = NULL;
   
   FUNC_ENTER (H5Tget_class, H5T_NO_CLASS);
   H5ECLEAR;

   /* Check args */
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(dt=H5Aatom_object (type_id))) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, H5T_NO_CLASS, "not a data type");
   }

   FUNC_LEAVE (dt->type);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tget_size
 *
 * Purpose:	Determines the total size of a data type in bytes.
 *
 * Return:	Success:	Size of the data type in bytes.  The size of
 *				data type is the size of an instance of that
 *				data type.
 *
 *		Failure:	0 (valid data types are never zero size)
 *
 * Programmer:	Robb Matzke
 *              Monday, December  8, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5Tget_size (hid_t type_id)
{
   H5T_t	*dt = NULL;
   size_t	size;
   
   FUNC_ENTER (H5Tget_size, 0);
   H5ECLEAR;

   /* Check args */
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(dt=H5Aatom_object (type_id))) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, 0, "not a data type");
   }

   /* size */
   size = H5T_get_size (dt);

   FUNC_LEAVE (size);
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
 * 		All data types have a positive size.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tset_size (hid_t type_id, size_t size)
{
   H5T_t	*dt = NULL;
   size_t	prec, offset;
   
   FUNC_ENTER (H5Tset_size, FAIL);
   H5ECLEAR;

   /* Check args */
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(dt=H5Aatom_object (type_id)) ||
       !H5T_is_atomic (dt)) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not an atomic data type");
   }
   if (dt->locked) {
      HRETURN_ERROR (H5E_ARGS, H5E_CANTINIT, FAIL, "data type is read-only");
   }
   if (size<=0) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "size must be positive");
   }

   offset = dt->u.atomic.offset;
   prec = dt->u.atomic.prec;
   
   /* Decrement the offset and precision if necessary */
   if (prec > 8*size) offset = 0;
   else if (offset + prec > 8*size) offset = 8*size - prec;
   if (prec > 8*size) prec = 8*size;

   /* Make sure that other values are still okay */
   switch (dt->type) {
   case H5T_INTEGER:
   case H5T_TIME:
   case H5T_STRING:
   case H5T_BITFIELD:
      /* nothing to check */
      break;
      
   case H5T_FLOAT:
      /*
       * The sign, mantissa, and exponent fields should be adjusted first
       * when decreasing the size of a floating point type.
       */
      if (dt->u.atomic.u.f.sign >= prec ||
	  dt->u.atomic.u.f.epos + dt->u.atomic.u.f.esize > prec ||
	  dt->u.atomic.u.f.mpos + dt->u.atomic.u.f.msize > prec) {
	 HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
			"adjust sign, mantissa, and exponent fields first");
      }
      break;

   case H5T_OPAQUE:
      /*
       * The significant bits of an opaque type are not allowed to change
       * implicitly.
       */
      if (prec != dt->u.atomic.prec) {
	 HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
			"unable to change precision of an opaque type");
      }
      break;

   default:
      assert ("not implemented yet" && 0);
   }

   /* Commit */
   dt->size = size;
   dt->u.atomic.offset = offset;
   dt->u.atomic.prec = prec;

   FUNC_LEAVE (SUCCEED);
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
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5T_order_t
H5Tget_order (hid_t type_id)
{
   H5T_t	*dt = NULL;
   H5T_order_t	order;
   
   FUNC_ENTER (H5Tget_order, H5T_ORDER_ERROR);
   H5ECLEAR;

   /* Check args */
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(dt=H5Aatom_object (type_id)) ||
       !H5T_is_atomic (dt)) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, H5T_ORDER_ERROR,
		     "not an atomic data type");
   }

   /* order */
   order = dt->u.atomic.order;

   FUNC_LEAVE (order);
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
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tset_order (hid_t type_id, H5T_order_t order)
{
   H5T_t	*dt = NULL;

   FUNC_ENTER (H5Tset_order, FAIL);
   H5ECLEAR;

   /* Check args */
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(dt=H5Aatom_object (type_id)) ||
       !H5T_is_atomic (dt)) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not an atomic data type");
   }
   if (dt->locked) {
      HRETURN_ERROR (H5E_ARGS, H5E_CANTINIT, FAIL, "data type is read-only");
   }
   if (order<0 || order>H5T_ORDER_NONE) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "illegal byte order");
   }
   
   /* order */
   dt->u.atomic.order = order;
   FUNC_LEAVE (SUCCEED);
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
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5Tget_precision (hid_t type_id)
{
   H5T_t	*dt = NULL;
   size_t	prec;
   
   FUNC_ENTER (H5Tget_precision, 0);
   H5ECLEAR;

   /* Check args */
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(dt=H5Aatom_object (type_id)) ||
       !H5T_is_atomic (dt)) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, 0, "not an atomic data type");
   }

   /* precision */
   prec = dt->u.atomic.prec;

   FUNC_LEAVE (prec);
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
 *		When decreasing the precision of a floating point type, set
 *		the locations and sizes of the sign, mantissa, and exponent
 *		fields first.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tset_precision (hid_t type_id, size_t prec)
{
   H5T_t	*dt = NULL;
   size_t	offset, size;

   FUNC_ENTER (H5Tset_prec, FAIL);
   H5ECLEAR;

   /* Check args */
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(dt=H5Aatom_object (type_id)) ||
       !H5T_is_atomic (dt)) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not an atomic data type");
   }
   if (dt->locked) {
      HRETURN_ERROR (H5E_ARGS, H5E_CANTINIT, FAIL, "data type is read-only");
   }
   if (prec<=0) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		     "precision must be positive");
   }

   /* Adjust the offset and size */
   offset = dt->u.atomic.offset;
   size = dt->size;
   if (prec > 8*size) offset = 0;
   else if (offset + prec > 8*size) offset = 8*size - prec;
   if (prec > 8*size) size = (prec+7)/8;

   /* Check that things are still kosher */
   switch (dt->type) {
   case H5T_INTEGER:
   case H5T_TIME:
   case H5T_STRING:
   case H5T_BITFIELD:
   case H5T_OPAQUE:
      /* nothing to check */
      break;
      
   case H5T_FLOAT:
      /*
       * The sign, mantissa, and exponent fields should be adjusted first
       * when decreasing the precision of a floating point type.
       */
      if (dt->u.atomic.u.f.sign >= prec ||
	  dt->u.atomic.u.f.epos + dt->u.atomic.u.f.esize > prec ||
	  dt->u.atomic.u.f.mpos + dt->u.atomic.u.f.msize > prec) {
	 HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
			"adjust sign, mantissa, and exponent fields first");
      }
      break;

   default:
      assert ("not implemented yet" && 0);
   }
   
   /* Commit */
   dt->size = size;
   dt->u.atomic.offset = offset;
   dt->u.atomic.prec = prec;
   
   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tget_offset
 *
 * Purpose:	Retrieves the bit offset of the first significant bit.  The
 *		signficant bits of an atomic datum can be offset from the
 *		beginning of the memory for that datum by an amount of
 *		padding. The `offset' property specifies the number of bits
 *		of padding that appear to the "right of" the value.  That is,
 *		if we have a 32-bit datum with 16-bits of precision having
 *		the value 0x1122 then it will be layed out in memory as (from
 *		small byte address toward larger byte addresses):
 *
 * 		    Big      Big       Little   Little
 *		    Endian   Endian    Endian   Endian
 *		    offset=0 offset=16 offset=0 offset=16
 *
 * 		0:  [ pad]   [0x11]    [0x22]   [ pad]
 *		1:  [ pad]   [0x22]    [0x11]   [ pad]
 *		2:  [0x11]   [ pad]    [ pad]   [0x22]
 *		3:  [0x22]   [ pad]    [ pad]   [0x11]
 *
 * Return:	Success:	The offset
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5Tget_offset (hid_t type_id)
{
   H5T_t	*dt = NULL;
   size_t	offset;
   
   FUNC_ENTER (H5Tget_offset, 0);
   H5ECLEAR;

   /* Check args */
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(dt=H5Aatom_object (type_id)) ||
       !H5T_is_atomic (dt)) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, 0, "not an atomic data type");
   }

   /* offset */
   offset = dt->u.atomic.offset;

   FUNC_LEAVE (offset);
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
 * 		    Big      Big       Little   Little
 *		    Endian   Endian    Endian   Endian
 *		    offset=0 offset=16 offset=0 offset=16
 *
 * 		0:  [ pad]   [0x11]    [0x22]   [ pad]
 *		1:  [ pad]   [0x22]    [0x11]   [ pad]
 *		2:  [0x11]   [ pad]    [ pad]   [0x22]
 *		3:  [0x22]   [ pad]    [ pad]   [0x11]
 *
 *		If the offset is incremented then the total size is
 *		incremented also if necessary to prevent significant bits of
 *		the value from hanging over the edge of the data type.
 *
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tset_offset (hid_t type_id, size_t offset)
{

   H5T_t	*dt = NULL;

   FUNC_ENTER (H5Tset_offset, FAIL);
   H5ECLEAR;

   /* Check args */
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(dt=H5Aatom_object (type_id)) ||
       !H5T_is_atomic (dt)) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not an atomic data type");
   }
   if (dt->locked) {
      HRETURN_ERROR (H5E_ARGS, H5E_CANTINIT, FAIL, "data type is read-only");
   }
   if (offset<=0) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		     "precision must be positive");
   }

   /* Adjust the size */
   if (offset + dt->u.atomic.prec > 8*dt->size) {
      dt->size = (offset + dt->u.atomic.prec + 7) / 8;
   }

   /* Commit */
   dt->u.atomic.offset = offset;
   
   FUNC_LEAVE (SUCCEED);
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
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5T_sign_t
H5Tget_sign (hid_t type_id)
{
   H5T_t	*dt = NULL;
   H5T_sign_t	sign;
   
   FUNC_ENTER (H5Tget_sign, H5T_SGN_ERROR);
   H5ECLEAR;

   /* Check args */
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(dt=H5Aatom_object (type_id)) ||
       H5T_INTEGER!=dt->type) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, H5T_SGN_ERROR,
		     "not an integer data type");
   }

   /* sign */
   sign = dt->u.atomic.u.i.sign;

   FUNC_LEAVE (sign);
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
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tset_sign (hid_t type_id, H5T_sign_t sign)
{
   H5T_t	*dt = NULL;

   FUNC_ENTER (H5Tset_sign, FAIL);
   H5ECLEAR;

   /* Check args */
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(dt=H5Aatom_object (type_id)) ||
       H5T_INTEGER!=dt->type) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not an integer data type");
   }
   if (dt->locked) {
      HRETURN_ERROR (H5E_ARGS, H5E_CANTINIT, FAIL, "data type is read-only");
   }
   if (sign<0 || sign>=H5T_SGN_N) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "illegal sign type");
   }
   
   /* sign */
   dt->u.atomic.u.i.sign = sign;
   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tget_fields
 *
 * Purpose:	Returns information about the locations of the various bit
 *		fields of a floating point data type.  The field positions
 *		are bit positions in the significant region of the data type.
 *		Bits are numbered with the least significant bit number zero.
 *
 * 		Any (or even all) of the arguments can be null pointers.
 *
 * Return:	Success:	SUCCEED, field locations and sizes are
 *				returned through the arguments.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tget_fields (hid_t type_id, size_t *spos/*out*/,
	       size_t *epos/*out*/, size_t *esize/*out*/,
	       size_t *mpos/*out*/, size_t *msize/*out*/)
{
   H5T_t	*dt = NULL;

   FUNC_ENTER (H5Tget_fields, FAIL);
   H5ECLEAR;

   /* Check args */
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(dt=H5Aatom_object (type_id)) ||
       H5T_FLOAT!=dt->type) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		     "not a floating-point data type");
   }

   /* Get values */
   if (spos) *spos = dt->u.atomic.u.f.sign;
   if (epos) *epos = dt->u.atomic.u.f.epos;
   if (esize) *esize = dt->u.atomic.u.f.esize;
   if (mpos) *mpos = dt->u.atomic.u.f.mpos;
   if (msize) *msize = dt->u.atomic.u.f.msize;

   FUNC_LEAVE (SUCCEED);
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
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tset_fields (hid_t type_id, size_t spos, size_t epos, size_t esize,
	       size_t mpos, size_t msize)
{
   H5T_t	*dt = NULL;

   FUNC_ENTER (H5Tset_fields, FAIL);
   H5ECLEAR;

   /* Check args */
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(dt=H5Aatom_object (type_id)) ||
       H5T_FLOAT!=dt->type) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		     "not a floating-point data type");
   }
   if (dt->locked) {
      HRETURN_ERROR (H5E_ARGS, H5E_CANTINIT, FAIL, "data type is read-only");
   }
   if (epos<0 || epos+esize>dt->u.atomic.prec) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		     "exponent bit field size/location is invalid");
   }
   if (mpos<0 || mpos+msize>dt->u.atomic.prec) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		     "mantissa bit field size/location is invalid");
   }
   if (spos<0 || spos>=dt->u.atomic.prec) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		     "sign location is not valid");
   }

   /* Check for overlap */
   if (spos>=epos && spos<epos+esize) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		     "sign bit appears within exponent field");
   }
   if (spos>=mpos && spos<mpos+msize) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		     "sign bit appears within mantissa field");
   }
   if ((mpos<epos && mpos+msize>epos) ||
       (epos<mpos && epos+esize>mpos)) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		     "exponent and mantissa fields overlap");
   }

   /* Commit */
   dt->u.atomic.u.f.sign = spos;
   dt->u.atomic.u.f.epos = epos;
   dt->u.atomic.u.f.mpos = mpos;
   dt->u.atomic.u.f.esize = esize;
   dt->u.atomic.u.f.msize = msize;

   FUNC_LEAVE (SUCCEED);
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
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5Tget_ebias (hid_t type_id)
{
   H5T_t	*dt = NULL;
   size_t	ebias;

   FUNC_ENTER (H5Tget_ebias, 0);
   H5ECLEAR;

   /* Check args */
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(dt=H5Aatom_object (type_id)) ||
       H5T_FLOAT!=dt->type) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, 0,
		     "not a floating-point data type");
   }

   /* bias */
   ebias = dt->u.atomic.u.f.ebias;

   FUNC_LEAVE (ebias);
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
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tset_ebias (hid_t type_id, size_t ebias)
{
   H5T_t	*dt = NULL;
   
   FUNC_ENTER (H5Tset_ebias, FAIL);
   H5ECLEAR;

   /* Check args */
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(dt=H5Aatom_object (type_id)) ||
       H5T_FLOAT!=dt->type) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		     "not a floating-point data type");
   }
   if (dt->locked) {
      HRETURN_ERROR (H5E_ARGS, H5E_CANTINIT, FAIL, "data type is read-only");
   }

   dt->u.atomic.u.f.ebias = ebias;

   FUNC_LEAVE (SUCCEED);
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
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5T_norm_t
H5Tget_norm (hid_t type_id)
{
   H5T_t	*dt = NULL;
   H5T_norm_t	norm;
   
   FUNC_ENTER (H5Tget_norm, H5T_NORM_ERROR);
   H5ECLEAR;

   /* Check args */
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(dt=H5Aatom_object (type_id)) ||
       H5T_FLOAT!=dt->type) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, H5T_NORM_ERROR,
		     "not a floating-point data type");
   }

   /* norm */
   norm = dt->u.atomic.u.f.norm;

   FUNC_LEAVE (norm);
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
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tset_norm (hid_t type_id, H5T_norm_t norm)
{
   H5T_t	*dt = NULL;

   FUNC_ENTER (H5Tset_norm, FAIL);
   H5ECLEAR;

   /* Check args */
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(dt=H5Aatom_object (type_id)) ||
       H5T_FLOAT!=dt->type) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
		     "not a floating-point data type");
   }
   if (dt->locked) {
      HRETURN_ERROR (H5E_ARGS, H5E_CANTINIT, FAIL, "data type is read-only");
   }
   if (norm<0 || norm>H5T_NORM_NONE) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "illegal normalization");
   }
   
   /* norm */
   dt->u.atomic.u.f.norm = norm;
   FUNC_LEAVE (SUCCEED);
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
 *              Monday, December  8, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5Tget_nmembers (hid_t type_id)
{
   
   H5T_t	*dt = NULL;

   FUNC_ENTER (H5Tget_num_members, FAIL);
   H5ECLEAR;

   /* Check args */
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(dt=H5Aatom_object (type_id)) ||
       H5T_COMPOUND!=dt->type) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a compound data type");
   }

   FUNC_LEAVE (dt->u.compnd.nmembs);
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
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
char *
H5Tget_member_name (hid_t type_id, int membno)
{
   H5T_t	*dt = NULL;
   char		*s = NULL;

   FUNC_ENTER (H5Tget_member_name, NULL);
   H5ECLEAR;

   /* Check args */
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(dt=H5Aatom_object (type_id)) ||
       H5T_COMPOUND!=dt->type) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, NULL, "not a compound data type");
   }
   if (membno<0 || membno>=dt->u.compnd.nmembs) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, NULL, "invalid member number");
   }

   s = H5MM_xstrdup (dt->u.compnd.memb[membno].name);
   FUNC_LEAVE (s);
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
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5Tget_member_offset (hid_t type_id, int membno)
{
   H5T_t	*dt = NULL;
   size_t	offset = 0;

   FUNC_ENTER (H5Tget_member_offset, 0);
   H5ECLEAR;

   /* Check args */
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(dt=H5Aatom_object (type_id)) ||
       H5T_COMPOUND!=dt->type) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, 0, "not a compound data type");
   }
   if (membno<0 || membno>=dt->u.compnd.nmembs) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, 0, "invalid member number");
   }

   offset = dt->u.compnd.memb[membno].offset;
   FUNC_LEAVE (offset);
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
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5Tget_member_dims (hid_t type_id, int membno,
		    int dims[]/*out*/, int perm[]/*out*/)
{
   H5T_t	*dt = NULL;
   intn		ndims, i;

   FUNC_ENTER (H5Tget_member_dims, FAIL);
   H5ECLEAR;

   /* Check args */
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(dt=H5Aatom_object (type_id)) ||
       H5T_COMPOUND!=dt->type) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a compound data type");
   }
   if (membno<0 || membno>=dt->u.compnd.nmembs) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "invalid member number");
   }

   ndims = dt->u.compnd.memb[membno].ndims;
   for (i=0; i<ndims; i++) {
      if (dims[i]) dims[i] = dt->u.compnd.memb[membno].dim[i];
      if (perm[i]) perm[i] = dt->u.compnd.memb[membno].perm[i];
   }
   
   FUNC_LEAVE (ndims);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tget_member_type
 *
 * Purpose:	Returns the data type of the specified member.  The caller
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
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Tget_member_type (hid_t type_id, int membno)
{
   H5T_t	*dt = NULL, *memb_dt = NULL;
   hid_t	memb_type_id;

   FUNC_ENTER (H5Tget_member_type, FAIL);
   H5ECLEAR;

   /* Check args */
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(dt=H5Aatom_object (type_id)) ||
       H5T_COMPOUND!=dt->type) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a compound data type");
   }
   if (membno<0 || membno>=dt->u.compnd.nmembs) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "invalid member number");
   }
   
   /* Copy data type into an atom */
   if (NULL==(memb_dt=H5T_copy (&(dt->u.compnd.memb[membno].type)))) {
      HRETURN_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,
		     "unable to copy member data type");
   }
   if ((memb_type_id=H5Aregister_atom (H5_DATATYPE, memb_dt))<0) {
      H5T_close (memb_dt);
      HRETURN_ERROR (H5E_DATATYPE, H5E_CANTREGISTER, FAIL,
		     "can't register data type atom");
   }

   FUNC_LEAVE (memb_type_id);
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
 *              Monday, December  8, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tinsert (hid_t parent_id, const char *name, off_t offset, hid_t member_id)
{
   H5T_t	*parent = NULL;		/*the compound parent data type	*/
   H5T_t	*member = NULL;		/*the atomic member type	*/
   
   FUNC_ENTER (H5Tinsert, FAIL);
   H5ECLEAR;

   /* Check args */
   if (H5_DATATYPE!=H5Aatom_group (parent_id) ||
       NULL==(parent=H5Aatom_object (parent_id)) ||
       H5T_COMPOUND!=parent->type) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a compound data type");
   }
   if (parent->locked) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "parent type read-only");
   }
   if (!name || !*name) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "no member name");
   }
   if (H5_DATATYPE!=H5Aatom_group (member_id) ||
       NULL==(member=H5Aatom_object (member_id))) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
   }

   if (H5T_insert (parent, name, offset, member)<0) {
      HRETURN_ERROR (H5E_DATATYPE, H5E_CANTINSERT, FAIL,
		     "can't insert member");
   }

   FUNC_LEAVE (SUCCEED);
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
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Tpack (hid_t type_id)
{
   H5T_t	*dt = NULL;
   
   FUNC_ENTER (H5Tpack, FAIL);
   H5ECLEAR;

   /* Check args */
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(dt=H5Aatom_object (type_id)) ||
       H5T_COMPOUND!=dt->type) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a compound data type");
   }
   if (dt->locked) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "data type is read-only");
   }

   if (H5T_pack (dt)<0) {
      HRETURN_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,
		     "unable to pack compound data type");
   }

   FUNC_LEAVE (SUCCEED);
}

   


/*-------------------------------------------------------------------------
 * API functions are above; library-private functions are below...
 *------------------------------------------------------------------------- 
 */







/*-------------------------------------------------------------------------
 * Function:	H5T_create
 *
 * Purpose:	Creates a new data type and initializes it to reasonable
 *		values.  The new data type is SIZE bytes and an instance of
 *		the class TYPE.
 *
 * Return:	Success:	Pointer to the new type.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Friday, December  5, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5T_t *
H5T_create (H5T_class_t type, size_t size)
{
   H5T_t	*dt = NULL;
   
   FUNC_ENTER (H5T_create, NULL);

   assert (size>0);

   switch (type) {
   case H5T_INTEGER:
      /* Default type is a native `int' */
      if (NULL==(dt=H5T_copy (H5Aatom_object (H5T_NATIVE_INT)))) {
	 HRETURN_ERROR (H5E_DATATYPE, H5E_CANTINIT, NULL,
			"can't derive type from native int");
      }
      break;

   case H5T_FLOAT:
      /* Default type is a native `double' */
      if (NULL==(dt=H5T_copy (H5Aatom_object (H5T_NATIVE_DOUBLE)))) {
	 HRETURN_ERROR (H5E_DATATYPE, H5E_CANTINIT, NULL,
			"can't derive type from native double");
      }
      break;

   case H5T_TIME:
   case H5T_STRING:
   case H5T_BITFIELD:
   case H5T_OPAQUE:
      assert ("not implemented yet" && 0);
      HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, NULL,
		     "not implemented yet");

   case H5T_COMPOUND:
      dt = H5MM_xcalloc (1, sizeof(H5T_t));
      dt->type = type;
      break;

   default:
      HRETURN_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, NULL,
		     "unknown data type class");
   }

   dt->size = size;
   FUNC_LEAVE (dt);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_copy
 *
 * Purpose:	Copies datatype OLD_DT.  The resulting data type is not
 *		locked.
 *
 * Return:	Success:	Pointer to a new copy of the OLD_DT argument.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5T_t *
H5T_copy (const H5T_t *old_dt)
{
   H5T_t	*new_dt = NULL;
   intn		i;
   char		*s;
   
   FUNC_ENTER (H5T_copy, NULL);

   /* check args */
   assert (old_dt);

   /* copy */
   new_dt = H5MM_xcalloc (1, sizeof(H5T_t));
   *new_dt = *old_dt;
   new_dt->locked = FALSE;
   
   if (H5T_COMPOUND==new_dt->type) {
      new_dt->u.compnd.memb = H5MM_xmalloc (new_dt->u.compnd.nmembs *
					    sizeof(H5T_member_t));
      HDmemcpy (new_dt->u.compnd.memb, old_dt->u.compnd.memb,
		new_dt->u.compnd.nmembs * sizeof(H5T_member_t));
      for (i=0; i<new_dt->u.compnd.nmembs; i++) {
	 s = new_dt->u.compnd.memb[i].name;
	 new_dt->u.compnd.memb[i].name = H5MM_xstrdup (s);
      }
   }
   
   FUNC_LEAVE (new_dt);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_close
 *
 * Purpose:	Frees a data type and all associated memory.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Monday, December  8, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_close (H5T_t *dt)
{
   intn		i;
   
   FUNC_ENTER (H5T_close, FAIL);

   assert (dt);
   assert (!dt->locked);

   if (dt && H5T_COMPOUND==dt->type) {
      for (i=0; i<dt->u.compnd.nmembs; i++) {
	 H5MM_xfree (dt->u.compnd.memb[i].name);
      }
      H5MM_xfree (dt->u.compnd.memb);
      H5MM_xfree (dt);
      
   } else if (dt) {
      H5MM_xfree (dt);
   }

   FUNC_LEAVE (SUCCEED);
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
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hbool_t
H5T_is_atomic (const H5T_t *dt)
{
   FUNC_ENTER (H5T_is_atomic, FAIL);

   assert (dt);

   FUNC_LEAVE (H5T_COMPOUND==dt->type ? FALSE : TRUE);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_get_size
 *
 * Purpose:	Determines the total size of a data type in bytes.
 *
 * Return:	Success:	Size of the data type in bytes.  The size of
 *				the data type is the size of an instance of
 *				that data type.
 *
 *		Failure:	0 (valid data types are never zero size)
 *
 * Programmer:	Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5T_get_size (const H5T_t *dt)
{
   FUNC_ENTER (H5T_get_size, 0);

   /* check args */
   assert (dt);

   FUNC_LEAVE (dt->size);
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
 *              Monday, December  8, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_insert (H5T_t *parent, const char *name, off_t offset, const H5T_t *member)
{
   intn		i;
   H5T_t	*tmp = NULL;
   
   FUNC_ENTER (H5T_insert, FAIL);

   /* check args */
   assert (parent && H5T_COMPOUND==parent->type);
   assert (!parent->locked);
   assert (member);
   assert (name && *name);

   /* Does NAME already exist in PARENT? */
   for (i=0; i<parent->u.compnd.nmembs; i++) {
      if (!HDstrcmp (parent->u.compnd.memb[i].name, name)) {
	 HRETURN_ERROR (H5E_DATATYPE, H5E_CANTINSERT, FAIL,
			"member name is not unique");
      }
   }
   
   /* Does the new member overlap any existing member ? */
   for (i=0; i<parent->u.compnd.nmembs; i++) {
      if ((offset<parent->u.compnd.memb[i].offset &&
	   offset+member->size>parent->u.compnd.memb[i].offset) ||
	  (parent->u.compnd.memb[i].offset<offset &&
	   parent->u.compnd.memb[i].offset+
	   parent->u.compnd.memb[i].type.size > offset)) {
	 HRETURN_ERROR (H5E_DATATYPE, H5E_CANTINSERT, FAIL,
			"member overlaps with another member");
      }
   }
   
   /* Increase member array if necessary */
   if (parent->u.compnd.nmembs>=parent->u.compnd.nalloc) {
      parent->u.compnd.nalloc += H5T_COMPND_INC;
      parent->u.compnd.memb = H5MM_xrealloc (parent->u.compnd.memb,
					     (parent->u.compnd.nalloc*
					      sizeof(H5T_member_t)));
   }

   /* Add member to end of member array */
   i = parent->u.compnd.nmembs;
   parent->u.compnd.memb[i].name = H5MM_xstrdup (name);
   parent->u.compnd.memb[i].offset = offset;
   parent->u.compnd.memb[i].ndims = 0; /*defaults to scalar*/
   
   tmp = H5T_copy (member);
   parent->u.compnd.memb[i].type = *tmp;
   H5MM_xfree (tmp);

   parent->u.compnd.nmembs++;
   FUNC_LEAVE (SUCCEED);
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
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_pack (H5T_t *dt)
{
   int		i;
   size_t	offset;
   
   FUNC_ENTER (H5T_pack, FAIL);

   assert (dt);
   assert (!dt->locked);

   if (H5T_COMPOUND==dt->type) {
      /* Recursively pack the members */
      for (i=0; i<dt->u.compnd.nmembs; i++) {
	 if (H5T_pack (&(dt->u.compnd.memb[i].type))<0) {
	    HRETURN_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,
			   "unable to pack part of a compound data type");
	 }
      }

      /* Remove padding between members */
      H5T_sort_by_offset (dt);
      for (i=0, offset=0; i<dt->u.compnd.nmembs; i++) {
	 dt->u.compnd.memb[i].offset = offset;
	 offset += dt->u.compnd.memb[i].type.size;
      }

      /* Change total size */
      dt->size = MAX (1, offset);
   }

   FUNC_LEAVE (SUCCEED);
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
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_sort_by_offset (H5T_t *dt)
{
   int		i, j, nmembs;
   hbool_t	swapped;
   
   FUNC_ENTER (H5T_sort_by_offset, FAIL);

   /* Check args */
   assert (dt);
   assert (H5T_COMPOUND==dt->type);

   /* Use a bubble sort because we can short circuit */
   nmembs = dt->u.compnd.nmembs;
   for (i=nmembs-1, swapped=TRUE; i>0 && swapped; --i) {
      for (j=0, swapped=FALSE; j<i; j++) {
	 if (dt->u.compnd.memb[j].offset>dt->u.compnd.memb[j+1].offset) {
	    H5T_member_t tmp = dt->u.compnd.memb[j];
	    dt->u.compnd.memb[j] = dt->u.compnd.memb[j+1];
	    dt->u.compnd.memb[j+1] = tmp;
	    swapped = 1;
	 }
      }
   }

#ifndef NDEBUG
   /* I never trust a sort :-) */
   for (i=0; i<dt->u.compnd.nmembs-1; i++) {
      assert (dt->u.compnd.memb[i].offset<dt->u.compnd.memb[i+1].offset);
   }
#endif

   FUNC_LEAVE (SUCCEED);
}



/*-------------------------------------------------------------------------
 * Function:	H5T_cmp
 *
 * Purpose:	Compares two data types.
 *
 * Return:	Success:	0 if DT1 and DT2 are equal.
 *				<0 if DT1 is less than DT2.
 * 				>0 if DT1 is greater than DT2.
 *
 *		Failure:	0, never fails
 *
 * Programmer:	Robb Matzke
 *              Wednesday, December 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5T_cmp (const H5T_t *dt1, const H5T_t *dt2)
{
   intn		*idx1=NULL, *idx2=NULL;
   intn		ret_value = 0;
   intn		i, j, tmp;
   hbool_t	swapped;
   
   FUNC_ENTER (H5T_equal, 0);

   /* check args */
   assert (dt1);
   assert (dt2);

   /* the easy case */
   if (dt1==dt2) HGOTO_DONE (0);

   /* compare */
   if (dt1->type < dt2->type) HGOTO_DONE (-1);
   if (dt1->type > dt2->type) HGOTO_DONE (1);

   if (dt1->size < dt2->size) HGOTO_DONE (-1);
   if (dt1->size > dt2->size) HGOTO_DONE (1);

   if (H5T_COMPOUND==dt1->type) {
      /*
       * Compound data types...
       */
      if (dt1->u.compnd.nmembs < dt2->u.compnd.nmembs) HGOTO_DONE (-1);
      if (dt1->u.compnd.nmembs > dt2->u.compnd.nmembs) HGOTO_DONE (1);

      /* Build an index for each type so the names are sorted */
      idx1 = H5MM_xmalloc (dt1->u.compnd.nmembs * sizeof(intn));
      idx2 = H5MM_xmalloc (dt1->u.compnd.nmembs * sizeof(intn));
      for (i=0; i<dt1->u.compnd.nmembs; i++) idx1[i] = idx2[i] = i;
      for (i=dt1->u.compnd.nmembs-1, swapped=TRUE; swapped && i>=0; --i) {
	 for (j=0, swapped=FALSE; j<i; j++) {
	    if (HDstrcmp (dt1->u.compnd.memb[idx1[j]].name,
			  dt1->u.compnd.memb[idx1[j+1]].name)>0) {
	       tmp = idx1[j];
	       idx1[j] = idx1[j+1];
	       idx1[j+1] = tmp;
	       swapped = TRUE;
	    }
	 }
      }
      for (i=dt1->u.compnd.nmembs-1, swapped=TRUE; swapped && i>=0; --i) {
	 for (j=0, swapped=FALSE; j<i; j++) {
	    if (HDstrcmp (dt2->u.compnd.memb[idx2[j]].name,
			  dt2->u.compnd.memb[idx2[j+1]].name)>0) {
	       tmp = idx2[j];
	       idx2[j] = idx2[j+1];
	       idx2[j+1] = tmp;
	       swapped = TRUE;
	    }
	 }
      }
      
#ifndef NDEBUG
      /* I don't quite trust the code above yet :-)  --RPM */
      for (i=0; i<dt1->u.compnd.nmembs; i++) {
	 assert (HDstrcmp (dt1->u.compnd.memb[idx1[i]].name,
			   dt1->u.compnd.memb[idx1[i+1]].name));
	 assert (HDstrcmp (dt2->u.compnd.memb[idx2[i]].name,
			   dt2->u.compnd.memb[idx2[i+1]].name));
      }
#endif

      /* Compare the members */
      for (i=0; i<dt1->u.compnd.nmembs; i++) {
	 tmp = HDstrcmp (dt1->u.compnd.memb[idx1[i]].name,
			 dt2->u.compnd.memb[idx2[i]].name);
	 if (tmp<0) HGOTO_DONE (-1);
	 if (tmp>0) HGOTO_DONE (1);

	 if (dt1->u.compnd.memb[idx1[i]].offset <
	     dt2->u.compnd.memb[idx2[i]].offset) HGOTO_DONE (-1);
	 if (dt1->u.compnd.memb[idx1[i]].offset >
	     dt2->u.compnd.memb[idx2[i]].offset) HGOTO_DONE (1);

	 if (dt1->u.compnd.memb[idx1[i]].ndims <
	     dt2->u.compnd.memb[idx2[i]].ndims) HGOTO_DONE (-1);
	 if (dt1->u.compnd.memb[idx1[i]].ndims >
	     dt2->u.compnd.memb[idx2[i]].ndims) HGOTO_DONE (1);

	 for (j=0; j<dt1->u.compnd.memb[idx1[i]].ndims; j++) {
	    if (dt1->u.compnd.memb[idx1[i]].dim[j] <
		dt2->u.compnd.memb[idx2[i]].dim[j]) HGOTO_DONE (-1);
	    if (dt1->u.compnd.memb[idx1[i]].dim[j] >
		dt2->u.compnd.memb[idx2[i]].dim[j]) HGOTO_DONE (1);
	 }
	 
	 for (j=0; j<dt1->u.compnd.memb[idx1[i]].ndims; j++) {
	    if (dt1->u.compnd.memb[idx1[i]].perm[j] <
		dt2->u.compnd.memb[idx2[i]].perm[j]) HGOTO_DONE (-1);
	    if (dt1->u.compnd.memb[idx1[i]].perm[j] >
		dt2->u.compnd.memb[idx2[i]].perm[j]) HGOTO_DONE (1);
	 }

	 tmp = H5T_cmp (&(dt1->u.compnd.memb[idx1[i]].type),
			&(dt2->u.compnd.memb[idx2[i]].type));
	 if (tmp<0) HGOTO_DONE (-1);
	 if (tmp>0) HGOTO_DONE (1);
      }
      
   } else {
      /*
       * Atomic data types...
       */
      if (dt1->u.atomic.order < dt2->u.atomic.order) HGOTO_DONE (-1);
      if (dt1->u.atomic.order > dt2->u.atomic.order) HGOTO_DONE (1);

      if (dt1->u.atomic.prec < dt2->u.atomic.prec) HGOTO_DONE (-1);
      if (dt1->u.atomic.prec > dt2->u.atomic.prec) HGOTO_DONE (1);

      if (dt1->u.atomic.offset < dt2->u.atomic.offset) HGOTO_DONE (-1);
      if (dt1->u.atomic.offset > dt2->u.atomic.offset) HGOTO_DONE (1);
      
      if (dt1->u.atomic.lo_pad < dt2->u.atomic.lo_pad) HGOTO_DONE (-1);
      if (dt1->u.atomic.lo_pad > dt2->u.atomic.lo_pad) HGOTO_DONE (1);
      
      if (dt1->u.atomic.hi_pad < dt2->u.atomic.hi_pad) HGOTO_DONE (-1);
      if (dt1->u.atomic.hi_pad > dt2->u.atomic.hi_pad) HGOTO_DONE (1);

      switch (dt1->type) {
      case H5T_INTEGER:
	 if (dt1->u.atomic.u.i.sign < dt2->u.atomic.u.i.sign) HGOTO_DONE (-1);
	 if (dt1->u.atomic.u.i.sign > dt2->u.atomic.u.i.sign) HGOTO_DONE (1);
	 break;

      case H5T_FLOAT:
	 if (dt1->u.atomic.u.f.sign < dt2->u.atomic.u.f.sign) HGOTO_DONE (-1);
	 if (dt1->u.atomic.u.f.sign > dt2->u.atomic.u.f.sign) HGOTO_DONE (1);
	 
	 if (dt1->u.atomic.u.f.epos < dt2->u.atomic.u.f.epos) HGOTO_DONE (-1);
	 if (dt1->u.atomic.u.f.epos > dt2->u.atomic.u.f.epos) HGOTO_DONE (1);
	 
	 if (dt1->u.atomic.u.f.esize <
	     dt2->u.atomic.u.f.esize) HGOTO_DONE (-1);
	 if (dt1->u.atomic.u.f.esize >
	     dt2->u.atomic.u.f.esize) HGOTO_DONE (1);
	 
	 if (dt1->u.atomic.u.f.ebias <
	     dt2->u.atomic.u.f.ebias) HGOTO_DONE (-1);
	 if (dt1->u.atomic.u.f.ebias >
	     dt2->u.atomic.u.f.ebias) HGOTO_DONE (1);
	 
	 if (dt1->u.atomic.u.f.mpos < dt2->u.atomic.u.f.mpos) HGOTO_DONE (-1);
	 if (dt1->u.atomic.u.f.mpos > dt2->u.atomic.u.f.mpos) HGOTO_DONE (1);
	 
	 if (dt1->u.atomic.u.f.msize <
	     dt2->u.atomic.u.f.msize) HGOTO_DONE (-1);
	 if (dt1->u.atomic.u.f.msize >
	     dt2->u.atomic.u.f.msize) HGOTO_DONE (1);
	 
	 if (dt1->u.atomic.u.f.norm < dt2->u.atomic.u.f.norm) HGOTO_DONE (-1);
	 if (dt1->u.atomic.u.f.norm > dt2->u.atomic.u.f.norm) HGOTO_DONE (1);
	 
	 if (dt1->u.atomic.u.f.pad < dt2->u.atomic.u.f.pad) HGOTO_DONE (-1);
	 if (dt1->u.atomic.u.f.pad > dt2->u.atomic.u.f.pad) HGOTO_DONE (1);

	 break;

      case H5T_TIME:
	 /*void*/
	 break;

      case H5T_STRING:
	 if (dt1->u.atomic.u.s.cset < dt1->u.atomic.u.s.cset) HGOTO_DONE (-1);
	 if (dt1->u.atomic.u.s.cset > dt1->u.atomic.u.s.cset) HGOTO_DONE (1);
	 
	 if (dt1->u.atomic.u.s.spad < dt1->u.atomic.u.s.spad) HGOTO_DONE (-1);
	 if (dt1->u.atomic.u.s.spad > dt1->u.atomic.u.s.spad) HGOTO_DONE (1);

	 break;
	 
      case H5T_BITFIELD:
	 /*void*/
	 break;

      case H5T_OPAQUE:
	 /*void*/
	 break;

      default:
	 assert ("not implemented yet" && 0);
      }
   }

 done:
   H5MM_xfree (idx1);
   H5MM_xfree (idx2);

   FUNC_LEAVE (ret_value);
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
 *              Wednesday, January  7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_debug (H5T_t *dt, FILE *stream)
{
   const char	*s = "";
   int		i, j;
   uint64	tmp;

   FUNC_ENTER (H5T_debug, FAIL);

   /* Check args */
   assert (dt);
   assert (stream);

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

   fprintf (stream, "%s%s {nbytes=%d",
	    s, dt->locked?"[!]":"", dt->size);

   if (H5T_is_atomic (dt)) {
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
      fprintf (stream, ", %s", s);

      if (dt->u.atomic.offset) {
	 fprintf (stream, ", offset=%lu",
		  (unsigned long)(dt->u.atomic.offset));
      }
      if (dt->u.atomic.prec!=8*dt->size) {
	 fprintf (stream, ", prec=%lu",
		  (unsigned long)(dt->u.atomic.prec));
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
	 if (s) fprintf (stream, ", %s", s);
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
	 fprintf (stream, ", sign=%lu+1",
		  (unsigned long)(dt->u.atomic.u.f.sign));
	 fprintf (stream, ", mant=%lu+%lu (%s)",
		  (unsigned long)(dt->u.atomic.u.f.mpos),
		  (unsigned long)(dt->u.atomic.u.f.msize), s);
	 fprintf (stream, ", exp=%lu+%lu",
		  (unsigned long)(dt->u.atomic.u.f.epos),
		  (unsigned long)(dt->u.atomic.u.f.esize));
	 tmp = dt->u.atomic.u.f.ebias >> 32;
	 if (tmp) {
	    uintn hi = tmp;
	    uintn lo = dt->u.atomic.u.f.ebias & 0xffffffff;
	    fprintf (stream, " bias=0x%08x%08x", hi, lo);
	 } else {
	    uintn lo = dt->u.atomic.u.f.ebias & 0xffffffff;
	    fprintf (stream, " bias=0x%08x", lo);
	 }
	 break;

      default:
	 /* No additional info */
	 break;
      }
   } else {
      for (i=0; i<dt->u.compnd.nmembs; i++) {
	 fprintf (stream, "\n\"%s\" @%lu",
		  dt->u.compnd.memb[i].name,
		  (unsigned long)(dt->u.compnd.memb[i].offset));
	 if (dt->u.compnd.memb[i].ndims) {
	    fprintf (stream, "[");
	    for (j=0; j<dt->u.compnd.memb[i].ndims; j++) {
	       fprintf (stream, "%s%d", j?", ":"",
			dt->u.compnd.memb[i].dim[j]);
	    }
	    fprintf (stream, "]");
	 }
	 fprintf (stream, " ");
	 H5T_debug (&(dt->u.compnd.memb[i].type), stream);
      }
      fprintf (stream, "\n");
   }
   fprintf (stream, "}");

   FUNC_LEAVE (SUCCEED);
}

	 

