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
 * Function:	H5Tget_class
 *
 * Purpose:	Returns the data type class identifier for data type TYPE_ID.
 *
 * Return:	Success:	One of the non-negative data type class
 *				constants.
 *
 *		Failure:	H5T_NO_CLASS (-1)
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
   
   FUNC_ENTER (H5Tget_class, FAIL);
   H5ECLEAR;

   /* Check args */
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(dt=H5Aatom_object (type_id))) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
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
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
   }

   /* size */
   size = H5T_get_size (dt);

   FUNC_LEAVE (size);
}


/*-------------------------------------------------------------------------
 * Function:	H5Tinsert_member
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
H5Tinsert_member (hid_t parent_id, const char *name, off_t offset,
		  hid_t member_id)
{
   H5T_t	*parent = NULL;		/*the compound parent data type	*/
   H5T_t	*member = NULL;		/*the atomic member type	*/
   
   FUNC_ENTER (H5Tinsert_member, FAIL);
   H5ECLEAR;

   /* Check args */
   if (H5_DATATYPE!=H5Aatom_group (parent_id) ||
       NULL==(parent=H5Aatom_object (parent_id)) ||
       H5T_COMPOUND!=parent->type) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a compound data type");
   }
   if (parent->locked) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "parent is locked");
   }
   if (!name || !*name) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "no member name");
   }
   if (H5_DATATYPE!=H5Aatom_group (member_id) ||
       NULL==(member=H5Aatom_object (member_id)) ||
       H5T_COMPOUND==member->type) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not an atomic data type");
   }

   if (H5T_insert_member (parent, name, offset, member)<0) {
      HRETURN_ERROR (H5E_DATATYPE, H5E_CANTINSERT, FAIL,
		     "can't insert member");
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
   case H5T_FIXED:
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

   case H5T_DATE:
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
 * Function:	H5T_insert_member
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
H5T_insert_member (H5T_t *parent, const char *name, off_t offset,
		   const H5T_t *member)
{
   intn		i;
   H5T_t	*tmp = NULL;
   
   FUNC_ENTER (H5T_insert_member, FAIL);

   /* check args */
   assert (parent && H5T_COMPOUND==parent->type);
   assert (!parent->locked);
   assert (member && H5T_COMPOUND!=member->type);
   assert (name && *name);

   /* Does NAME already exist in PARENT? */
   for (i=0; i<parent->u.compnd.nmembs; i++) {
      if (!HDstrcmp (parent->u.compnd.memb[i].name, name)) {
	 HRETURN_ERROR (H5E_DATATYPE, H5E_CANTINSERT, FAIL,
			"member name is not unique");
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
      case H5T_FIXED:
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

      case H5T_DATE:
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
      
