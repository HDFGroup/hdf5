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
#include "H5PropList.h"
#include "H5Object.h"
#include "H5DataType.h"
#include "H5AtomType.h"
#include "H5AbstractDs.h"
#include "H5DxferProp.h"
#include "H5DataSpace.h"
#include "H5StrType.h"
#include "H5DataSet.h"
#include "H5PredType.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

// Default constructor
StrType::StrType() : AtomType() {}

// Creates a string type using a predefined type
StrType::StrType( const PredType& pred_type ) : AtomType()
{
   // use DataType::copy to make a copy of this predefined type
   copy( pred_type );
}

// Creates a string type with a specified length - 1st argument could
// have been skipped, but this constructor will collide with the one
// that takes an existing id below
StrType::StrType( const PredType& pred_type, const size_t size ) : AtomType()
{
   // use DataType::copy to make a copy of the string predefined type
   // then set its length
   copy(pred_type);
   setSize(size);
}

// Creates a string datatype using an existing id
StrType::StrType( const hid_t existing_id ) : AtomType( existing_id ) {}

// Copy constructor: makes copy of the original StrType object
StrType::StrType( const StrType& original ) : AtomType ( original ) {}

// Gets the string datatype of the specified dataset - will reimplement - BMR
StrType::StrType( const DataSet& dataset ) : AtomType ()
{
   // Calls C function H5Dget_type to get the id of the datatype
   id = H5Dget_type( dataset.getId() );

   if( id <= 0 )
   {
      throw DataSetIException("StrType constructor", "H5Dget_type failed");
   }
}

// Retrieves the character set type of a string datatype. 
H5T_cset_t StrType::getCset() const
{
   H5T_cset_t cset = H5Tget_cset( id );

   // Returns a valid character set type if successful
   if( cset == H5T_CSET_ERROR )
   {
      throw DataTypeIException("StrType::getCset", "H5Tget_cset failed");
   }
   return( cset );
}

// Sets character set to be used. 
void StrType::setCset( H5T_cset_t cset ) const
{
   herr_t ret_value = H5Tset_cset( id, cset );

   if( ret_value < 0 )
   {
      throw DataTypeIException("StrType::setCset", "H5Tset_cset failed");
   }
}

// Retrieves the string padding method for a string datatype. 
H5T_str_t StrType::getStrpad() const
{
   H5T_str_t strpad = H5Tget_strpad( id );

   // Returns a valid string padding type if successful
   if( strpad == H5T_STR_ERROR )
   {
      throw DataTypeIException("StrType::getStrpad", 
		"H5Tget_strpad failed - returned H5T_STR_ERROR");
   }
   return( strpad );
}

// Defines the storage mechanism for character strings. 
void StrType::setStrpad( H5T_str_t strpad ) const
{
   herr_t ret_value = H5Tset_strpad( id, strpad );

   if( ret_value < 0 )
   {
      throw DataTypeIException("StrType::setStrpad", "H5Tset_strpad failed");
   }
}

// This destructor terminates access to the datatype
StrType::~StrType() {}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
