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
#include "H5FloatType.h"
#include "H5DataSet.h"
#include "H5PredType.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

// Default constructor
FloatType::FloatType() {}

// Creates a floating-point type using a predefined type
FloatType::FloatType( const PredType& pred_type ) : AtomType()
{
   // use DataType::copy to make a copy of this predefined type
   copy( pred_type );
}

// Creates a floating-point datatype using an existing id
FloatType::FloatType( const hid_t existing_id ) : AtomType( existing_id ) {}

// Copy constructor: makes a copy of the original FloatType object
FloatType::FloatType( const FloatType&  original ) : AtomType( original ){}

// Gets the floating-point datatype of the specified dataset - will reimplement
FloatType::FloatType( const DataSet& dataset ) : AtomType()
{
   // Calls C function H5Dget_type to get the id of the datatype
   id = H5Dget_type( dataset.getId() );

   if( id <= 0 )
   {
      throw DataSetIException();
   }
}

// Retrieves floating point datatype bit field information. 
void FloatType::getFields( size_t& spos, size_t& epos, size_t& esize, size_t& mpos, size_t& msize ) const
{
   herr_t ret_value = H5Tget_fields( id, &spos, &epos, &esize, &mpos, &msize );
   if( ret_value < 0 )
   {
      throw DataTypeIException();
   }
}

// Sets locations and sizes of floating point bit fields. 
void FloatType::setFields( size_t spos, size_t epos, size_t esize, size_t mpos, size_t msize ) const
{
   herr_t ret_value = H5Tset_fields( id, spos, epos, esize, mpos, msize );
   if( ret_value < 0 )
   {
      throw DataTypeIException();
   }
}

// Retrieves the exponent bias of a floating-point type. 
size_t FloatType::getEbias() const
{
   size_t ebias = H5Tget_ebias( id );
   // Returns the bias if successful
   if( ebias == 0 )
   {
      throw DataTypeIException();
   }
   return( ebias );
}

// Sets the exponent bias of a floating-point type. 
void FloatType::setEbias( size_t ebias ) const
{
   herr_t ret_value = H5Tset_ebias( id, ebias );
   if( ret_value < 0 )
   {
      throw DataTypeIException();
   }
}

// Retrieves mantissa normalization of a floating-point datatype. 
H5T_norm_t FloatType::getNorm( string& norm_string ) const
{
   H5T_norm_t norm = H5Tget_norm( id );  // C routine
   // Returns a valid normalization type if successful
   if( norm == H5T_NORM_ERROR )
   {
      throw DataTypeIException();
   }
   if( norm == H5T_NORM_IMPLIED )
      norm_string = "H5T_NORM_IMPLIED (0)";
   else if( norm == H5T_NORM_MSBSET )
      norm_string = "H5T_NORM_MSBSET (1)";
   else if( norm == H5T_NORM_NONE )
      norm_string = "H5T_NORM_NONE (2)";
   return( norm );
}

// Sets the mantissa normalization of a floating-point datatype. 
void FloatType::setNorm( H5T_norm_t norm ) const
{
   herr_t ret_value = H5Tset_norm( id, norm );
   if( ret_value < 0 )
   {
      throw DataTypeIException();
   }
}

// Retrieves the internal padding type for unused bits in floating-point datatypes. 
H5T_pad_t FloatType::getInpad( string& pad_string ) const
{
   H5T_pad_t pad_type = H5Tget_inpad( id );
   // Returns a valid padding type if successful
   if( pad_type == H5T_PAD_ERROR )
   {
      throw DataTypeIException();
   }
   if( pad_type == H5T_PAD_ZERO )
      pad_string = "H5T_PAD_ZERO (0)";
   else if( pad_type == H5T_PAD_ONE )
      pad_string = "H5T_PAD_ONE (1)";
   else if( pad_type == H5T_PAD_BACKGROUND )
      pad_string = "H5T_PAD_BACKGROUD (2)";
   return( pad_type );
}

// Fills unused internal floating point bits. 
void FloatType::setInpad( H5T_pad_t inpad ) const
{
   herr_t ret_value = H5Tset_inpad( id, inpad );
   if( ret_value < 0 )
   {
      throw DataTypeIException();
   }
}

// Default destructor
FloatType::~FloatType() {}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
