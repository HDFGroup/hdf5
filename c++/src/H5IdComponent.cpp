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

#include <string>

#include "H5Include.h"
#include "H5Exception.h"
#include "H5Library.h"
#include "H5IdComponent.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

//--------------------------------------------------------------------------
// Function:	IdComponent overloaded constructor
///\brief	Creates an IdComponent object using the id of an existing object.
///\param	h5_id - IN: Id of an existing object
///\exception	H5::DataTypeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
IdComponent::IdComponent(const hid_t h5_id) : id(h5_id) {}

//--------------------------------------------------------------------------
// Function:	IdComponent copy constructor
///\brief	Copy constructor: makes a copy of the original IdComponent object.
///\param	original - IN: IdComponent instance to copy
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
IdComponent::IdComponent( const IdComponent& original )
{
   id = original.id;
   H5Iinc_ref(id); // increment number of references to this id
}

//--------------------------------------------------------------------------
// Function:	IdComponent::incRefCount
///\brief	Increment id reference counter.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void IdComponent::incRefCount() { H5Iinc_ref(id); }

//--------------------------------------------------------------------------
// Function:	IdComponent::decRefCount
///\brief	Decrement id reference counter.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void IdComponent::decRefCount()
{
    if(id>0)
        if(H5Idec_ref(id)<0)
            throw IdComponentException("IdComponent::decRefCount", "decrementing object ref count failed");
}

//--------------------------------------------------------------------------
// Function:	IdComponent::getCounter
///\brief	Returns the reference counter to this identifier.
///\return	Reference count
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
int IdComponent::getCounter() { return( H5Iget_ref(id)); }

//--------------------------------------------------------------------------
// Function:	IdComponent::operator=
///\brief	Assignment operator.
///\param	rhs - IN: Reference to the existing object
///\return	Reference to IdComponent instance
///\exception	H5::IdComponentException when attempt to close the HDF5
///		object fails
// Description
// 		The underlaying reference counting in the C library ensures
// 		that the current valid id of this object is properly closed.
//		Copy the id from rhs to this object, then increment the 
//		reference counter of the id to indicate that another object 
//		is referencing it.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
IdComponent& IdComponent::operator=( const IdComponent& rhs )
{
   // handling references to this id
   decRefCount();

   // copy the data members from the rhs object
   id = rhs.id;

   // increment the reference counter
   H5Iinc_ref(id);

   return( *this );
}

//--------------------------------------------------------------------------
// Function:	IdComponent::setId
///\brief	Sets the identifier of this object to a new value.
///
///\exception	H5::IdComponentException when the attempt to close the HDF5
///		object fails
// Description:
// 		The underlaying reference counting in the C library ensures
// 		that the current valid id of this object is properly closed.
// 		Then the object's id is reset to the new id.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void IdComponent::setId( hid_t new_id )
{
   // handling references to this id
   decRefCount();

   // reset object's id to the given id
   id = new_id;
}

//--------------------------------------------------------------------------
// Function:	IdComponent::getId
///\brief	Returns the id of this object 
///\return	HDF5 id
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
hid_t IdComponent::getId () const
{
   return( id );
}

//--------------------------------------------------------------------------
// Function:	IdComponent destructor
///\brief	Noop destructor.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
IdComponent::~IdComponent() {

/* uncomment this block and complete it when deciding to use dontAtExit 
   unless the atexit/global destructor problem is fixed, then 
   remove it- BMR 11/14/00

   if( id == NOTATEXIT )
   {
      // Call H5Library::close to clean up - temporary solution to avoid the
      // trouble of atexit/global destructors
      try {
         if( H5Library::need_cleanup == true )
         {
            H5Library::close();
            H5Library::need_cleanup = false; // reset the boolean just in case
         }
      }
      // catch failure caused by the H5Library operations
      catch( LibraryIException error )
      {
         error.printError();
      }
   }
*/
}

//
// Implementation of protected functions for HDF5 Reference Interface.
// 

#ifndef DOXYGEN_SHOULD_SKIP_THIS
//--------------------------------------------------------------------------
// Function:	IdComponent default constructor - private
///\brief	Default constructor.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
IdComponent::IdComponent() : id(-1) {}

//--------------------------------------------------------------------------
// Function:	IdComponent::p_get_file_name
// Purpose:	Gets the name of the file, in which this object belongs.
// Exception:	H5::IdComponentException
// Description:
// 		This function is protected so that the user applications can
// 		only have access to its code via allowable classes, namely, 
// 		H5File and H5Object subclasses.
// Programmer	Binh-Minh Ribler - Jul, 2004
//--------------------------------------------------------------------------
string IdComponent::p_get_file_name() const
{
   // Preliminary call to H5Fget_name to get the length of the file name
   ssize_t name_size = H5Fget_name(id, NULL, 0);

   // If H5Aget_name returns a negative value, raise an exception,
   if( name_size < 0 )
   {
      throw IdComponentException("IdComponent::p_get_file_name", 
				"H5Fget_name failed");
   }

   // Call H5Fget_name again to get the actual file name
   char* name_C = new char[name_size+1];  // temporary C-string for C API
   name_size = H5Fget_name(id, name_C, name_size+1);

   // Check for failure again
   if( name_size < 0 )
   {
      throw IdComponentException("IdComponent::p_get_file_name", 
				"H5Fget_name failed");
   }

   // Convert the C file name and return
   string file_name(name_C);
   delete name_C;
   return(file_name);
}

//--------------------------------------------------------------------------
// Function:	IdComponent::p_reference (protected)
// Purpose	Creates a reference to an HDF5 object or a dataset region.
// Parameters
//		name - IN: Name of the object to be referenced
//		dataspace - IN: Dataspace with selection
//		ref_type - IN: Type of reference; default to \c H5R_DATASET_REGION
// Return	A reference
// Exception	H5::IdComponentException
// Programmer	Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
void* IdComponent::p_reference(const char* name, hid_t space_id, H5R_type_t ref_type) const
{
   void *ref=NULL;
   herr_t ret_value = H5Rcreate(ref, id, name, ref_type, space_id);
   if (ret_value < 0)
   {
      throw IdComponentException("IdComponent::p_reference",
                "H5Rcreate failed");
   }
   return(ref);
}

//--------------------------------------------------------------------------
// Function:	IdComponent::p_get_obj_type (protected)
// Purpose	Retrieves the type of object that an object reference points to.
// Parameters
//		ref      - IN: Reference to query
//		ref_type - IN: Type of reference to query
// Return	An object type, which can be one of the following:
//			H5G_LINK Object is a symbolic link.  
//			H5G_GROUP Object is a group.  
//			H5G_DATASET   Object is a dataset.  
//			H5G_TYPE Object is a named datatype 
// Exception	H5::IdComponentException
// Programmer	Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
H5G_obj_t IdComponent::p_get_obj_type(void *ref, H5R_type_t ref_type) const
{
   H5G_obj_t obj_type = H5Rget_obj_type(id, ref_type, ref);
   if (obj_type == H5G_UNKNOWN)
   {
      throw IdComponentException("IdComponent::p_get_obj_type",
                "H5R_get_obj_type failed");
   }
   return(obj_type);
}

//--------------------------------------------------------------------------
// Function:	IdComponent::p_get_region (protected)
// Purpose	Retrieves a dataspace with the region pointed to selected.
// Parameters
//		ref_type - IN: Type of reference to get region of - default
//				to H5R_DATASET_REGION
//		ref      - IN: Reference to get region of
// Return	Dataspace id
// Exception	H5::IdComponentException
// Programmer	Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
hid_t IdComponent::p_get_region(void *ref, H5R_type_t ref_type) const
{
   hid_t space_id = H5Rget_region(id, ref_type, ref);
   if (space_id < 0)
   {
      throw IdComponentException("IdComponent::p_get_region",
                "H5Rget_region failed");
   }
   return(space_id);
}

#endif // DOXYGEN_SHOULD_SKIP_THIS

#ifndef H5_NO_NAMESPACE
}
#endif
