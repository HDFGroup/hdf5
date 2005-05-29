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
#ifdef OLD_HEADER_FILENAME
#include <iostream.h>
#else
#include <iostream>
#endif

#include "H5Include.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5PropList.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

//--------------------------------------------------------------------------
///\brief	Constant for default property.
//--------------------------------------------------------------------------
const PropList PropList::DEFAULT( H5P_DEFAULT );

//--------------------------------------------------------------------------
// Function	Default constructor
///\brief	Default constructor: creates a stub property list object.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
PropList::PropList() : IdComponent( 0 ) {}

//--------------------------------------------------------------------------
// Function:	PropList copy constructor
///\brief	Copy constructor
///\param	original - IN: The original property list to copy
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
PropList::PropList( const PropList& original ) : IdComponent( original ) {}

//--------------------------------------------------------------------------
// Function:	PropList overloaded constructor
///\brief	Creates a property list using the id of an existing property.
///\param	plist_id - IN: Id of the existing property list
///\exception	H5::PropListIException
// Description
//		This function calls H5Pcreate to create a new property list 
//		if the given id, plist_id, is that of a property class.  If 
//		the given id is equal to H5P_NO_CLASS, then set this 
//		property's id to H5P_DEFAULT, otherwise, to the given id.  
//		Note: someone else added this code without comments and this 
//		description was what I came up with from reading the code.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
PropList::PropList( const hid_t plist_id ) : IdComponent(0)
{
    if (H5I_GENPROP_CLS == H5Iget_type(plist_id)) {
        // call C routine to create the new property
        id = H5Pcreate(plist_id);
        if( id <= 0 )
        {
            throw PropListIException("PropList constructor", "H5Pcreate failed");
        }
    }
    else {
        if(plist_id==H5P_NO_CLASS)
            id=H5P_DEFAULT;
        else
            id=plist_id;
    }
}

//--------------------------------------------------------------------------
// Function:	PropList::copy
///\brief	Makes a copy of an existing property list.
///\param	like_plist - IN: Reference to the existing property list
///\exception	H5::PropListIException
// Programmer	Binh-Minh Ribler - 2000
// Modification
//              Replaced resetIdComponent with decRefCount to use new ID
//              reference counting mechanisms by Quincey Koziol, June 1, 2004
//--------------------------------------------------------------------------
void PropList::copy( const PropList& like_plist )
{
    // If this object has a valid id, appropriately decrement reference
    // counter and close the id.
    try {
        decRefCount();
    }
    catch (Exception close_error) {
        throw PropListIException("PropList::copy", close_error.getDetailMsg());
    }

   // call C routine to copy the property list
   id = H5Pcopy( like_plist.getId() );

   if( id <= 0 )
      throw PropListIException("PropList::copy", "H5Pcopy failed");
}

//--------------------------------------------------------------------------
// Function:	PropList::operator=
///\brief	Assignment operator.
///\param	rhs - IN: Reference to the existing property list
///\exception	H5::PropListIException
// Description
//		Makes a copy of the property list on the right hand side 
//		and stores the new id in the left hand side object.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
PropList& PropList::operator=( const PropList& rhs )
{
   copy(rhs);
   return(*this);
}

//--------------------------------------------------------------------------
// Function:	PropList::copyProp
///\brief	Copies a property from one list or class to another
///\param	dest - IN: Destination property list or class
///\param	src  - IN: Source property list or class
///\param	name - IN: Name of the property to copy - \c char pointer
///\exception	H5::PropListIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void PropList::copyProp( PropList& dest, PropList& src, const char *name ) const
{
   hid_t dst_id = dest.getId();
   hid_t src_id = src.getId();
   herr_t ret_value = H5Pcopy_prop(dst_id, src_id, name);
   if( ret_value < 0 )
   {
      throw PropListIException("PropList::copyProp", "H5Pcopy_prop failed");
   }

}

//--------------------------------------------------------------------------
// Function:	PropList::copyProp
///\brief	This is an overloaded member function, provided for convenience.
/// 		It differs from the above function only in what arguments it 
///		accepts.
///\param	dest - IN: Destination property list or class
///\param	src  - IN: Source property list or class
///\param	name - IN: Name of the property to copy - \c std::string
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void PropList::copyProp( PropList& dest, PropList& src, const string& name ) const
{
   copyProp( dest, src, name.c_str());
}

//--------------------------------------------------------------------------
// Function:	PropList::close
///\brief	Closes the property list if it is not a default one.
///
///\exception	H5::PropListIException
// Programmer	Binh-Minh Ribler - Mar 9, 2005
//--------------------------------------------------------------------------
void PropList::close()
{
   if( id != H5P_NO_CLASS ) // not a constant, should call H5Pclose
   {
      herr_t ret_value = H5Pclose( id );
      if( ret_value < 0 )
      {
         throw PropListIException("PropList::close", "H5Pclose failed");
      }
      // reset the id because the property list that it represents is now closed
      id = 0;
   }
   else
      throw PropListIException("PropList::close", "Cannot close a constant");
}

//--------------------------------------------------------------------------
// Function:	PropList::getClass
///\brief	Returns the class of this property list, i.e. \c H5P_FILE_CREATE...
///\return	The property list class if it is not equal to \c H5P_NO_CLASS
///\exception	H5::PropListIException
// Programmer	Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
hid_t PropList::getClass() const
{
   hid_t plist_class = H5Pget_class( id );
   if( plist_class == H5P_NO_CLASS )
   {
      throw PropListIException("PropList::getClass", 
		"H5Pget_class failed - returned H5P_NO_CLASS");
   }
   return( plist_class );
}

//--------------------------------------------------------------------------
// Function:	PropList::propExist
///\brief	Query the existance of a property in a property object.
///\param	name - IN: Name of property to check for - \c char pointer
///\return	true if the property exists in the property object, and
///		false, otherwise.
///\exception	H5::PropListIException
///\par Description
///		This routine checks if a property exists within a property 
///		list or class.
// Programmer:  Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
bool PropList::propExist(const char* name ) const
{
   // Calls C routine H5Pexist to determine whether a property exists 
   // within a property list or class.  It returns a positive value, 0, 
   // or a negative value
   htri_t ret_value = H5Pexist(id, name);
   if( ret_value > 0 )
      return true;
   else if( ret_value == 0 )
      return false;
   else // Raise exception when H5Pexist returns a negative value
   {
      throw PropListIException("PropList::propExist", "H5Pexist failed");
   }
}
//--------------------------------------------------------------------------
// Function:	PropList::propExist
///\brief	This is an overloaded member function, provided for convenience.
/// 		It differs from the above function only in what arguments it 
///		accepts.
///\param	name - IN: Name of property to check for - \c std::string
// Programmer:  Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
bool PropList::propExist(const string& name ) const
{
   return( propExist( name.c_str()) );
}

//--------------------------------------------------------------------------
// Function:	PropList::closeClass
///\brief	Close a property list class.
///
///\exception	H5::PropListIException
///\par Description
///		Releases memory and detaches a class from the property 
///		list class hierarchy.
// Programmer:  Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
void PropList::closeClass() const
{
   herr_t ret_value = H5Pclose_class(id);
   if( ret_value < 0 )
   {
      throw PropListIException("PropList::closeClass", "H5Pclose_class failed");
   }
}

//--------------------------------------------------------------------------
// Function:	PropList::getProperty
///\brief	Query the value of a property in a property list.
///\param	name -   IN: Name of property to query - \c char pointer
///\param	value - OUT: Pointer to the buffer for the property value
///\exception	H5::PropListIException
///\par Description
///		Retrieves a copy of the value for a property in a property 
///		list.  The property name must exist or this routine will 
///		throw an exception.
// Programmer:  Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
void PropList::getProperty(const char* name, void* value) const
{
   herr_t ret_value = H5Pget(id, name, value);
   if (ret_value < 0)
   {
      throw PropListIException("PropList::getProperty", "H5Pget failed");
   }
}
//--------------------------------------------------------------------------
// Function:	PropList::getProperty
///\brief	This is an overloaded member function, provided for convenience.
///   		It differs from the above function only in what arguments it 
///		accepts.
///\param	name -  IN: Name of property to query - \c char pointer
///\return	The property that is a \c std::string.
// Programmer:  Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
string PropList::getProperty(const char* name) const
{
   size_t size = getPropSize(name);
   char* prop_strg_C = new char[size+1];  // temporary C-string for C API
   herr_t ret_value = H5Pget(id, name, prop_strg_C); // call C API

   // Throw exception if H5Pget returns failure
   if (ret_value < 0)
   {
      throw PropListIException("PropList::getProperty", "H5Pget failed");
   }

   // Return propety value as a string after deleting temp C-string
   string prop_strg = string(prop_strg_C);
   delete prop_strg_C;
   return (prop_strg);
}
//--------------------------------------------------------------------------
// Function:	PropList::getProperty
///\brief	This is an overloaded member function, provided for convenience.
///   		It differs from the above function only in what arguments it 
///		accepts.
///\param	name -   IN: Name of property to query - \c str::string
///\param	value - OUT: Pointer to the buffer for the property value
// Programmer:  Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
void PropList::getProperty(const string& name, void* value) const
{
   getProperty(name.c_str(), value);
}
//--------------------------------------------------------------------------
// Function:	PropList::getProperty
///\brief	This is an overloaded member function, provided for convenience.
///   		It differs from the above function only in what arguments it 
///		accepts.
///\param	name -  IN: Name of property to query - \c std::string
///\return	The property that is a \c std::string.
// Programmer:  Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
string PropList::getProperty(const string& name) const
{
   return (getProperty(name.c_str()));
}

//--------------------------------------------------------------------------
// Function:	PropList::getPropSize
///\brief	Query the size of a property in a property list or class.
///\param	name - IN: Name of property to query
///\return	Size of the property
///\exception	H5::PropListIException
///\par Description
///		This routine retrieves the size of a property's value 
///		in bytes.  Zero-sized properties are allowed and the return 
///		value will be of 0.  This function works for both property 
///		lists and classes.
// Programmer:  Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
size_t PropList::getPropSize(const char *name) const
{
   size_t prop_size;
   herr_t ret_value = H5Pget_size(id, name, &prop_size);
   if (ret_value < 0)
   {
      throw PropListIException("PropList::getPropSize", "H5Pget_size failed");
   }
   return(prop_size);
}
//--------------------------------------------------------------------------
// Function:	PropList::getPropSize
///\brief	This is an overloaded member function, provided for convenience.
/// 		It differs from the above function only in what arguments it 
///		accepts.
///\param	name - IN: Name of property to query - \c std::string
///
// Programmer:  Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
size_t PropList::getPropSize(const string& name) const
{
   return (getPropSize(name.c_str()));
}

//--------------------------------------------------------------------------
// Function:	PropList::getClassName
///\brief	Return the name of a generic property list class.
///\return	A string containing the class name, if success, otherwise,
///		a NULL string.
// Programmer:  Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
string PropList::getClassName() const
{
   char* temp_str;
   temp_str = H5Pget_class_name(id);

   if (temp_str != NULL)
   {
      string class_name = string(temp_str);
      free(temp_str);
      return(class_name);
   }
   else
      return 0;
}
//--------------------------------------------------------------------------
// Function:	PropList::getNumProps
///\brief	Returns the number of properties in this property list or class.
///\return	Size of the property.
///\exception	H5::PropListIException
// Programmer:  Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
size_t PropList::getNumProps() const
{
   size_t nprops;
   herr_t ret_value = H5Pget_nprops (id, &nprops);
   if (ret_value < 0)
   {
      throw PropListIException("PropList::getNumProps", "H5Pget_nprops failed");
   }
   return (nprops);
}

//--------------------------------------------------------------------------
// Function:	PropList::setProperty
///\brief	Set a property's value in a property list.
///\param	name  - IN: Name of property to set - \c char pointer
///\param	value - IN: Void pointer to the value for the property
///\exception	H5::PropListIException
// Programmer:  Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
void PropList::setProperty(const char* name, void* value) const
{
   herr_t ret_value = H5Pset(id, name, value);
   if (ret_value < 0)
   {
      throw PropListIException("PropList::setProperty", "H5Pset failed");
   }
}
//--------------------------------------------------------------------------
// Function:	PropList::setProperty
///\brief	This is an overloaded member function, provided for convenience.
/// 		It differs from the above function only in what arguments it 
///		accepts.
///\param	name    - IN: Name of property to set - \c char pointer
///\param	charptr - IN: Char pointer to the value for the property
// Programmer:  Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
void PropList::setProperty(const char* name, const char* charptr) const
{
   herr_t ret_value = H5Pset(id, name, (void*) charptr);
   if (ret_value < 0)
   {
      throw PropListIException("PropList::setProperty", "H5Pset failed");
   }
}
//--------------------------------------------------------------------------
// Function:	PropList::setProperty
///\brief	This is an overloaded member function, provided for convenience.
/// 		It differs from the above function only in what arguments it 
///		accepts.
///\param	name - IN: Name of property to set - \c char pointer
///\param	strg - IN: Value for the property is a \c std::string
// Programmer:  Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
void PropList::setProperty(const char* name, string& strg) const
{
   setProperty(name, strg.c_str());
}

//--------------------------------------------------------------------------
// Function:	PropList::setProperty
///\brief	This is an overloaded member function, provided for convenience.
/// 		It differs from the above function only in what arguments it 
///		accepts.
///\param	name  - IN: Name of property to set - \c std::string
///\param	value - IN: Void pointer to the value for the property
// Programmer:  Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
void PropList::setProperty(const string& name, void* value) const
{
   setProperty(name.c_str(), value);
}

//--------------------------------------------------------------------------
// Function:	PropList::setProperty
///\brief	This is an overloaded member function, provided for convenience.
/// 		It differs from the above function only in what arguments it 
///		accepts.
///\param	name - IN: Name of property to set - \c std::string
///\param	strg - IN: Value for the property is a \c std::string
// Programmer:  Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
void PropList::setProperty(const string& name, string& strg) const
{
   setProperty(name.c_str(), strg.c_str());
}

//--------------------------------------------------------------------------
// Function:	PropList::isAClass
///\brief	Determines whether a property list is a certain class.
///\param	prop_class - IN: Property class to query
///\return	true if the property list is a member of the property list 
///		class, and false, otherwise.
///\exception	H5::PropListIException
// Programmer:  Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
bool PropList::isAClass(const PropList& prop_class) const
{
   htri_t ret_value = H5Pisa_class(id, prop_class.getId());
   if( ret_value > 0 )
      return true;
   else if( ret_value == 0 )
      return false;
   else // Raise exception when H5Pisa_class returns a negative value
   {
      throw PropListIException("PropList::isAClass", "H5Pisa_class failed");
   }

}

//--------------------------------------------------------------------------
// Function:	PropList::removeProp
///\brief	Removes a property from a property list.
///\param	name - IN: Name of property to remove - \c char pointer
///\exception	H5::PropListIException
// Programmer:  Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
void PropList::removeProp(const char *name) const
{
   herr_t ret_value = H5Premove(id, name);
   if (ret_value < 0)
   {
      throw PropListIException("PropList::removeProp", "H5Premove failed");
   }
}

//--------------------------------------------------------------------------
// Function:	PropList::removeProp
///\brief	This is an overloaded member function, provided for convenience.
/// 		It differs from the above function only in what arguments it 
///		accepts.
///\param	name - IN: Name of property to remove - \c std::string
// Programmer:  Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
void PropList::removeProp(const string& name) const
{
   removeProp(name.c_str());
}

//--------------------------------------------------------------------------
// Function:	PropList::operator==
///\brief	Compares this property list or class against the given list or class.
///\param	rhs - IN: Reference to the property list to compare
///\return	true if the property lists or classes are equal, and 
///		false, otherwise.
///\exception	H5::PropListIException
// Programmer:  Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
bool PropList::operator==(const PropList& rhs) const
{
   htri_t ret_value = H5Pequal(id, rhs.getId());
   if( ret_value > 0 )
      return true;
   else if( ret_value == 0 )
      return false;
   else // Raise exception when H5Pequal returns a negative value
   {
      throw PropListIException("PropList::operator==", "H5Pequal failed");
   }
}

//--------------------------------------------------------------------------
// Function:	PropList::getClassParent
///\brief	Returns the parent class of a generic property class
///\return	The parent class of a property class
///\exception	H5::PropListIException
// Programmer:  Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
PropList PropList::getClassParent() const
{
   hid_t class_id = H5Pget_class_parent(id);
   if (class_id <= 0)
   {
      throw PropListIException("PropList::getClassParent", "H5Pget_class_parent failed");
   }
   PropList pclass(class_id);
   return(pclass);
}

//--------------------------------------------------------------------------
// Function:	PropList destructor
///\brief	Properly terminates access to this property list.
// Programmer	Binh-Minh Ribler - 2000
// Modification
//		Replaced resetIdComponent with decRefCount to use new ID 
//		reference counting mechanisms by Quincey Koziol, June 1, 2004
//--------------------------------------------------------------------------
PropList::~PropList()
{  
   // The property list id will be closed properly
    try {
        decRefCount();
    }
    catch (Exception close_error) {
        cerr << "PropList::~PropList - " << close_error.getDetailMsg() << endl;
    }
}  

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
