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
#include "H5AbstractDs.h"
#include "H5DxferProp.h"
#include "H5DataSpace.h"
#include "H5AtomType.h"
#include "H5IntType.h"
#include "H5DataSet.h"
#include "H5PredType.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

// Default constructor
IntType::IntType() {}

// Copy constructor: makes copy of the original IntType object
IntType::IntType( const IntType& original ) : AtomType( original ) {}

// Creates a integer type using a predefined type
IntType::IntType( const PredType& pred_type ) : AtomType()
{
   // use DataType::copy to make a copy of this predefined type
   copy( pred_type );
}

/* BMR - may not keep
IntType& IntType::operator=( const PredType& rhs )
{
   copy(rhs);
   return(*this);
}
*/

// Creates a integer datatype using an existing id
IntType::IntType( const hid_t existing_id ) : AtomType( existing_id ) {}

// Gets the integer datatype of the specified dataset - may reimplement -BMR
IntType::IntType( const DataSet& dataset ) : AtomType()
{  
   // Calls C function H5Dget_type to get the id of the datatype
   id = H5Dget_type( dataset.getId() );

   if( id <= 0 )
   {
      throw DataSetIException("IntType constructor", "H5Dget_type failed");
   }
}  

// Retrieves the sign type for an integer type
H5T_sign_t IntType::getSign() const
{
   H5T_sign_t type_sign = H5Tget_sign( id );  // C routine
   // Returns a valid sign type if successful
   if( type_sign == H5T_SGN_ERROR )
   {
      throw DataTypeIException("IntType::getSign", 
		"H5Tget_sign failed - returned H5T_SGN_ERROR for the sign type");
   }
   return( type_sign );
}

// Sets the sign proprety for an integer type. 
void IntType::setSign( H5T_sign_t sign ) const
{
   // Call C routine to set the sign property
   herr_t ret_value = H5Tset_sign( id, sign );
   if( ret_value < 0 )
   {
      throw DataTypeIException("IntType::setSign", "H5Tset_sign failed");
   }
}

// This destructor terminates access to the datatype
IntType::~IntType() {}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
