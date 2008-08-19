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

#ifdef OLD_HEADER_FILENAME
#include <iostream.h>
#else
#include <iostream>
#endif
#include <string>

#include "H5Include.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5PropList.h"
#include "H5Object.h"
#include "H5AbstractDs.h"
#include "H5FaccProp.h"
#include "H5FcreatProp.h"
#include "H5DcreatProp.h"
#include "H5CommonFG.h"
#include "H5DataType.h"
#include "H5DataSpace.h"
#include "H5File.h"
#include "H5Attribute.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#ifndef H5_NO_STD
    using std::cerr;
    using std::endl;
#endif  // H5_NO_STD
#endif

//--------------------------------------------------------------------------
// Function:	Attribute default constructor
///\brief	Default constructor: Creates a stub attribute
// Programmer	Binh-Minh Ribler - May, 2004
// Modification
//	Jul, 08 No longer inherit data member 'id' from IdComponent.
//		- bugzilla 1068
//--------------------------------------------------------------------------
Attribute::Attribute() : AbstractDs(), IdComponent(), id(0) {}

//--------------------------------------------------------------------------
// Function:	Attribute copy constructor
///\brief	Copy constructor: makes a copy of the original Attribute object.
///\param	original  - IN: Original Attribute object to copy
// Programmer	Binh-Minh Ribler - 2000
// Modification
//	Jul, 08 No longer inherit data member 'id' from IdComponent.
//		- bugzilla 1068
//--------------------------------------------------------------------------
Attribute::Attribute(const Attribute& original) : AbstractDs(), IdComponent()
{
    id = original.getId();
    incRefCount(); // increment number of references to this id
}

//--------------------------------------------------------------------------
// Function:	Attribute overloaded constructor
///\brief	Creates an Attribute object using the id of an existing
///		attribute.
///\param	existing_id - IN: Id of an existing attribute
///\exception	H5::AttributeIException
// Programmer	Binh-Minh Ribler - 2000
// Modification
//	Jul, 08 No longer inherit data member 'id' from IdComponent.
//		- bugzilla 1068
//--------------------------------------------------------------------------
Attribute::Attribute(const hid_t existing_id) : AbstractDs(), IdComponent()
{
   id = existing_id;
}

//--------------------------------------------------------------------------
// Function:	Attribute::write
///\brief	Writes data to this attribute.
///\param	mem_type  - IN: Attribute datatype (in memory)
///\param	buf       - IN: Data to be written
///\exception	H5::AttributeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void Attribute::write( const DataType& mem_type, const void *buf ) const
{
   // Call C function H5Awrite to write data in 'buf' to the attribute
   herr_t ret_value = H5Awrite( id, mem_type.getId(), buf );
   if( ret_value < 0 )
   {
      throw AttributeIException("Attribute::write", "H5Awrite failed");
   }
}

//--------------------------------------------------------------------------
// Function:	Attribute::write
///\brief	This is an overloaded member function, provided for convenience.
///		It writes a \a H5std_string to this attribute.
///\param	mem_type  - IN: Attribute datatype (in memory)
///\param	strg      - IN: Data to be written
///\exception	H5::AttributeIException
// Programmer	Binh-Minh Ribler - Apr, 2003
//--------------------------------------------------------------------------
void Attribute::write( const DataType& mem_type, const H5std_string& strg ) const
{
   // Convert string to C-string
   const char* strg_C;
   strg_C = strg.c_str();  // strg_C refers to the contents of strg as a C-str

   // Call C function H5Awrite to write the character string to the attribute
   herr_t ret_value = H5Awrite( id, mem_type.getId(), &strg_C );
   if( ret_value < 0 )
   {
      throw AttributeIException("Attribute::write", "H5Awrite failed");
   }
}

//--------------------------------------------------------------------------
// Function:	Attribute::read
///\brief	Reads data from this attribute.
///\param	mem_type -  IN: Attribute datatype (in memory)
///\param	buf      - OUT: Buffer for read data
///\exception	H5::AttributeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void Attribute::read( const DataType& mem_type, void *buf ) const
{
   // Call C function H5Aread to read attribute data into 'buf'
   herr_t ret_value = H5Aread( id, mem_type.getId(), buf );
   if( ret_value < 0 )
   {
      throw AttributeIException("Attribute::read", "H5Aread failed");
   }
}

//--------------------------------------------------------------------------
// Function:	Attribute::read
///\brief	This is an overloaded member function, provided for convenience.
///		It reads a \a H5std_string from this attribute.
///\param	mem_type  - IN: Attribute datatype (in memory)
///\param	strg      - IN: Buffer for read string
///\exception	H5::AttributeIException
// Programmer	Binh-Minh Ribler - Apr, 2003
// Modification
//	Mar 2008
//		Corrected a misunderstanding that H5Aread would allocate
//		space for the buffer.  Obtained the attribute size and
//		allocated memory properly. - BMR
//--------------------------------------------------------------------------
void Attribute::read( const DataType& mem_type, H5std_string& strg ) const
{
   // Get the attribute size and allocate temporary C-string for C API
   // NOTE: will be changed to use dataspace information instead of this API
   hsize_t attr_size = H5Aget_storage_size(id);
   if (attr_size <= 0)
   {
      throw AttributeIException("Attribute::read", "Unable to get attribute size before reading");
   }
   char* strg_C = new char [attr_size+1];
   if (strg_C == NULL)
   {
      throw AttributeIException("Attribute::read", "Unable to allocate buffer to read the attribute");
   }

   // Call C API to get the attribute data, a string of chars
   herr_t ret_value = H5Aread(id, mem_type.getId(), &strg_C);
   if( ret_value < 0 )
   {
      throw AttributeIException("Attribute::read", "H5Aread failed");
   }

   // Get 'string' from the C char* and release resource
   strg_C[attr_size] = '\0';
   strg = strg_C;
   delete []strg_C;
}

//--------------------------------------------------------------------------
// Function:	Attribute::getSpace
///\brief	Gets a copy of the dataspace for this attribute.
///\return	Dataspace instance
///\exception	H5::AttributeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DataSpace Attribute::getSpace() const
{
   // Calls C function H5Aget_space to get the id of the dataspace
   hid_t dataspace_id = H5Aget_space( id );

   // If the dataspace id is valid, create and return the DataSpace object
   if( dataspace_id > 0 )
   {
      DataSpace dataspace( dataspace_id );
      return( dataspace );
   }
   else
   {
      throw AttributeIException("Attribute::getSpace", "H5Aget_space failed");
   }
}

//--------------------------------------------------------------------------
// Function:	Attribute::p_get_type (private)
// Purpose	Gets the datatype of this attribute.
// Return	Id of the datatype
// Exception	H5::AttributeIException
// Description
// 		This private function is used in AbstractDs.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
hid_t Attribute::p_get_type() const
{
   hid_t type_id = H5Aget_type( id );
   if( type_id > 0 )
      return( type_id );
   else
   {
      throw AttributeIException("", "H5Aget_type failed");
   }
}

//--------------------------------------------------------------------------
// Function:	Attribute::getFileName
///\brief	Gets the name of the file, in which this attribute belongs.
///\return	File name
///\exception	H5::IdComponentException
// Programmer	Binh-Minh Ribler - Jul, 2004
//--------------------------------------------------------------------------
H5std_string Attribute::getFileName() const
{
   try {
      return(p_get_file_name());
   }
   catch (IdComponentException E) {
      throw FileIException("Attribute::getFileName", E.getDetailMsg());
   }
}

//--------------------------------------------------------------------------
// Function:	Attribute::getName
///\brief	Gets the name of this attribute, returning its length.
///\param	buf_size  -  IN: Desired length of the name
///\param	attr_name - OUT: Buffer for the name string
///\return	Length of the attribute name
///\exception	H5::AttributeIException
// Programmer	Binh-Minh Ribler - Nov, 2001
//--------------------------------------------------------------------------
ssize_t Attribute::getName( size_t buf_size, H5std_string& attr_name ) const
{
   char* name_C = new char[buf_size+1];  // temporary C-string for C API

   // Calls C routine H5Aget_name to get the name of the attribute
   ssize_t name_size = H5Aget_name( id, buf_size, name_C );

   // If H5Aget_name returns a negative value, raise an exception,
   if( name_size < 0 )
   {
      throw AttributeIException("Attribute::getName", "H5Aget_name failed");
   }
   // otherwise, convert the C attribute name and return
   attr_name = name_C;
   delete []name_C;
   return( name_size );
}

//--------------------------------------------------------------------------
// Function:	Attribute::getName
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function in that it returns the
///		attribute's name, not the length.
///\return	Name of the attribute
///\param	buf_size  -  IN: Desired length of the name
///\exception	H5::AttributeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
H5std_string Attribute::getName( size_t buf_size ) const
{
   H5std_string attr_name;
   ssize_t name_size = getName( buf_size, attr_name );
   return( attr_name );
   // let caller catch exception if any
}

//--------------------------------------------------------------------------
// Function:	Attribute::getName
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above functions in that it doesn't take
///		any arguments and returns the attribute's name.
///\return	Name of the attribute
///\exception	H5::AttributeIException
// Programmer	Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
H5std_string Attribute::getName() const
{
   // Try with 0 and NULL to get the name size
   ssize_t name_size = H5Aget_name(id, 0, NULL);

   // If H5Aget_name returns a negative value, raise an exception,
   if (name_size < 0)
   {
      throw AttributeIException("Attribute::getName", "H5Aget_name failed with 0 and NULL");
   }

   // Allocate temporary C-string for C API
   char* name_C = new char[name_size+1];  // temporary C-string for C API

   // Calls C routine H5Aget_name again to get the name of the attribute
   name_size = H5Aget_name(id, name_size+1, name_C);

   // If H5Aget_name returns a negative value, raise an exception,
   if (name_size < 0)
   {
      throw AttributeIException("Attribute::getName", "H5Aget_name failed");
   }

   // otherwise, convert the C attribute name and return
   H5std_string attr_name = name_C;
   delete []name_C;
   return(attr_name);
}

//--------------------------------------------------------------------------
// Function:	Attribute::getRefObjType
///\brief	Retrieves the type of object that an object reference points to.
///\param	ref	 - IN: Reference to query
///\param	ref_type - IN: Type of reference to query, valid values are:
///		\li \c H5R_OBJECT \tReference is an object reference.
///		\li \c H5R_DATASET_REGION \tReference is a dataset region reference.
///\return	An object type, which can be one of the following:
///		\li \c H5G_LINK (0) \tObject is a symbolic link.
///		\li \c H5G_GROUP (1) \tObject is a group.
///		\li \c H5G_DATASET (2) \tObject is a dataset.
///		\li \c H5G_TYPE Object (3) \tis a named datatype
///\exception	H5::AttributeIException
// Programmer	Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
H5G_obj_t Attribute::getRefObjType(void *ref, H5R_type_t ref_type) const
{
   try {
      return(p_get_refobj_type(ref, ref_type));
   }
   catch (IdComponentException E) {
      throw AttributeIException("Attribute::getRefObjType", E.getDetailMsg());
   }
}

//--------------------------------------------------------------------------
// Function:    Attribute::getObjType
///\brief       This function was misnamed and will be deprecated in favor of
///             Attribute::getRefObjType; please use getRefObjType instead.
// Programmer   Binh-Minh Ribler - May, 2004
// Note:        Replaced by getRefObjType. - BMR - Jul, 2008
//--------------------------------------------------------------------------
H5G_obj_t Attribute::getObjType(void *ref, H5R_type_t ref_type) const
{
    return(getRefObjType(ref, ref_type));
}

//--------------------------------------------------------------------------
// Function:	Attribute::getStorageSize
///\brief	Returns the amount of storage size required for this attribute.
///\return	Size of the storage or 0, for no data
///\exception	H5::AttributeIException
// Note:	H5Dget_storage_size returns 0 when there is no data.  This
//		function should have no failure. (from SLU)
// Programmer	Binh-Minh Ribler - Mar, 2005
//--------------------------------------------------------------------------
hsize_t Attribute::getStorageSize() const
{
   hsize_t storage_size = H5Aget_storage_size(id);
   return (storage_size);
}

//--------------------------------------------------------------------------
// Function:	Attribute::getId
// Purpose:	Get the id of this group
// Modification:
//	May 2008 - BMR
//		Class hierarchy is revised to address bugzilla 1068.  Class
//		AbstractDS and Attribute are moved out of H5Object.  In
//		addition, member IdComponent::id is moved into subclasses, and
//		IdComponent::getId now becomes pure virtual function.
// Programmer	Binh-Minh Ribler - May, 2008
//--------------------------------------------------------------------------
hid_t Attribute::getId() const
{
   return(id);
}

//--------------------------------------------------------------------------
// Function:	Attribute::p_setId
///\brief	Sets the identifier of this group to a new value.
///
///\exception	H5::IdComponentException when the attempt to close the 
///		currently open group fails
// Description:
//		The underlaying reference counting in the C library ensures
//		that the current valid id of this object is properly closed.
//		Then the object's id is reset to the new id.
// Programmer	Binh-Minh Ribler - 2000
// Modification
//	Jul, 08 No longer inherit data member 'id' from IdComponent.
//		- bugzilla 1068
//--------------------------------------------------------------------------
void Attribute::p_setId(const hid_t new_id)
{
    // handling references to this old id
    try {
	close();
    }
    catch (Exception close_error) {
	throw AttributeIException("Attribute::p_setId", close_error.getDetailMsg());
    }
    // reset object's id to the given id
    id = new_id;
}

//--------------------------------------------------------------------------
// Function:	Attribute::close
///\brief	Closes this attribute.
///
///\exception	H5::AttributeIException
// Programmer	Binh-Minh Ribler - Mar 9, 2005
//--------------------------------------------------------------------------
void Attribute::close()
{
    if (p_valid_id(id))
    {
	herr_t ret_value = H5Aclose(id);
	if( ret_value < 0 )
	{
	    throw AttributeIException("Attribute::close", "H5Aclose failed");
	}
	// reset the id when the attribute that it represents is no longer
	// referenced
	if (getCounter() == 0)
	    id = 0;
    }
}

//--------------------------------------------------------------------------
// Function:	Attribute destructor
///\brief	Properly terminates access to this attribute.
// Programmer	Binh-Minh Ribler - 2000
// Modification
//		- Replaced resetIdComponent() with decRefCount() to use C
//		library ID reference counting mechanism - BMR, Feb 20, 2005
//		- Replaced decRefCount with close() to let the C library
//		handle the reference counting - BMR, Jun 1, 2006
//--------------------------------------------------------------------------
Attribute::~Attribute()
{
    try {
	close();
    }
    catch (Exception close_error) {
	cerr << "Attribute::~Attribute - " << close_error.getDetailMsg() << endl;
    }
}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
