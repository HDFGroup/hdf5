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
#include "H5RefCounter.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5Idtemplates.h"
#include "H5PropList.h"
#include "H5Object.h"
#include "H5FaccProp.h"
#include "H5FcreatProp.h"
#include "H5DxferProp.h"
#include "H5DcreatProp.h"
#include "H5CommonFG.h"
#include "H5Group.h"
#include "H5AbstractDs.h"
#include "H5DataSpace.h"
#include "H5DataSet.h"
#include "H5File.h"
#include "H5Alltypes.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

// Default constructor
H5File::H5File() : IdComponent() {}

// Creates or opens an HDF5 file depending on the parameter flags.
H5File::H5File( const string& name, unsigned int flags, const FileCreatPropList& create_plist, const FileAccPropList& access_plist ) : IdComponent()
{
   getFile( name.c_str(), flags, create_plist, access_plist );
}

H5File::H5File( const char* name, unsigned int flags, const FileCreatPropList& create_plist, const FileAccPropList& access_plist ) : IdComponent()
{
   getFile( name, flags, create_plist, access_plist );
}

// This function is private and contains common code between the 
// constructors taking a string or a char*
void H5File::getFile( const char* name, unsigned int flags, const FileCreatPropList& create_plist, const FileAccPropList& access_plist )
{
    // These bits only set for creation, so if any of them are set,
    // create the file.
    if( flags & (H5F_ACC_EXCL|H5F_ACC_TRUNC|H5F_ACC_DEBUG ))
    {
	hid_t create_plist_id = create_plist.getId();
	hid_t access_plist_id = access_plist.getId();
	id = H5Fcreate( name, flags, create_plist_id, access_plist_id );
	if( id <= 0 )  // throw an exception when open/create fail
	{
	    throw FileIException("H5File constructor", "H5Fcreate failed");
	}
    }
    // Open the file if none of the bits above are set.
    else
    {
	// use create_plist for access plist because of the default argument
	hid_t access_plist_id = create_plist.getId();
	id = H5Fopen( name, flags, access_plist_id );
	if( id <= 0 )  // throw an exception when open/create fail
	{
	    throw FileIException("H5File constructor", "H5Fopen failed");
	}
    }
}

// Copy constructor: makes a copy of the original H5File object.
H5File::H5File( const H5File& original ) : IdComponent( original ) {}

// Determines whether a file specified by its name in HDF5 format
bool H5File::isHdf5(const string& name ) 
{
   return( isHdf5( name.c_str()) );
}
bool H5File::isHdf5(const char* name ) 
{
   // Calls C routine H5Fis_hdf5 to determine whether the file is in 
   // HDF5 format.  It returns positive value, 0, or negative value
   htri_t ret_value = H5Fis_hdf5( name );
   if( ret_value > 0 )
      return true;
   else if( ret_value == 0 )
      return false;
   else // Raise exception when H5Fis_hdf5 returns a negative value 
   {
      throw FileIException("H5File::isHdf5", "H5Fis_hdf5 returned negative value");
   }
}

// Get id of the location, which id the file id here; used by CommonFG
// member functions
hid_t H5File::getLocId() const
{
   return( getId() );
}

// Reopens this file
void H5File::reopen()
{
   // reset the identifier of this H5File - send 'this' in so that
   // H5Fclose can be called appropriately
    try {
        resetIdComponent( this ); }
    catch (Exception close_error) { // thrown by p_close
        throw FileIException("H5File::reopen", close_error.getDetailMsg());
    }

   // call C routine to reopen the file - Note: not sure about this
   // does id need to be closed later?  which id to be the parameter?
   id = H5Freopen( id );
   if( id <= 0 ) // Raise exception when H5Freopen returns a neg value
   {
      throw FileIException("H5File::reopen", "H5Freopen failed");
   }
}

// Returns the creation property list of this file
FileCreatPropList H5File::getCreatePlist() const
{
   hid_t create_plist_id = H5Fget_create_plist( id );

   // if H5Fget_create_plist returns a valid id, create and return
   // the FileCreatPropList object for this property list
   if( create_plist_id > 0 )
   {
      FileCreatPropList create_plist( create_plist_id );
      return( create_plist );
   }
   else
   {
      throw FileIException("H5File::getCreatePlist", "H5Fget_create_plist failed");
   }
}

// Returns the access property list of this file
FileAccPropList H5File::getAccessPlist() const
{
   hid_t access_plist_id = H5Fget_access_plist( id );

   // if H5Fget_access_plist returns a valid id, create and return
   // the FileAccPropList object for this property list
   if( access_plist_id > 0 )
   {
      FileAccPropList access_plist( access_plist_id );
      return access_plist;
   }
   else // Raise an exception
   {
      throw FileIException("H5File::getAccessPlist", "H5Fget_access_plist failed");
   }
}

// Calls the C API H5Fclose to close this file.  Used by IdComponent::reset
void H5File::p_close() const
{
   herr_t ret_value = H5Fclose( id );
   if( ret_value < 0 )
   {
      throw FileIException(0, "H5Fclose failed");
   }
}

// Throw file exception; used in CommonFG implementation so that proper
// exception can be thrown for file or group.  The func_name is a member
// function of CommonFG and "H5File::" will be inserted to indicate the
// function called is an implementation of H5File
void H5File::throwException(const string& func_name, const string& msg) const
{
   string full_name = func_name;
   full_name.insert(0, "H5File::"); 
   throw FileIException(full_name, msg);
}

// The destructor of this instance calls IdComponent::reset to
// reset its identifier - no longer true
// Older compilers (baldric) don't support template member functions
// and IdComponent::reset is one; so at this time, the resetId is not
// a member function so it can be template to work around that problem.
H5File::~H5File() 
{  
   // The HDF5 file id will be closed properly
    try {
        resetIdComponent( this ); }
    catch (Exception close_error) { // thrown by p_close
        throw FileIException("H5File::~H5File", close_error.getDetailMsg());
    }
}  

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
