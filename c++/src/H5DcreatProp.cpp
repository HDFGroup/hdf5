#include <string>

#include "H5Include.h"
#include "H5RefCounter.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5PropList.h"
#include "H5Object.h"
#include "H5DataType.h"
#include "H5DcreatProp.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

const DSetCreatPropList DSetCreatPropList::DEFAULT( H5P_DEFAULT );

// Copy constructor: makes a copy of the original DSetCreatPropList object;
DSetCreatPropList::DSetCreatPropList( const DSetCreatPropList& orig ) : PropList( orig ) {}

// Sets the size of the chunks used to store a chunked layout dataset.
void DSetCreatPropList::setChunk( int ndims, const hsize_t* dim ) const
{
   herr_t ret_value = H5Pset_chunk( id, ndims, dim );
   if( ret_value < 0 )
   {
      throw PropListIException();
   }
}

// Gets the layout of the raw data storage of the data that uses this
// property list
H5D_layout_t DSetCreatPropList::getLayout() const
{
   H5D_layout_t layout = H5Pget_layout( id );
   if( layout == H5D_LAYOUT_ERROR )
   {
      throw PropListIException();
   }
   return( layout );
}

// Retrieves the size of the chunks used to store a chunked layout dataset.
int DSetCreatPropList::getChunk( int max_ndims, hsize_t* dim ) const
{
   int chunk_size = H5Pget_chunk( id, max_ndims, dim );
   if( chunk_size < 0 )
   {
      throw PropListIException();
   }
   return( chunk_size );
}

// Sets the type of storage used store the raw data for a dataset.
void DSetCreatPropList::setLayout(hid_t plist, H5D_layout_t layout ) const
{
   herr_t ret_value = H5Pset_layout( id, layout );
   if( ret_value < 0 )
   {
      throw PropListIException();
   }
}

// Sets compression method and compression level
void DSetCreatPropList::setDeflate( int level ) const
{
   herr_t ret_value = H5Pset_deflate( id, level );
   if( ret_value < 0 )
   {
      throw PropListIException();
   }
}

// Sets a dataset fill value
void DSetCreatPropList::setFillValue( DataType& fvalue_type, const void* value ) const
{
   herr_t ret_value = H5Pset_fill_value( id, fvalue_type.getId(), value );
   if( ret_value < 0 )
   {
      throw PropListIException();
   }
}

// Retrieves a dataset fill value
void DSetCreatPropList::getFillValue( DataType& fvalue_type, void* value ) const
{
   herr_t ret_value = H5Pget_fill_value( id, fvalue_type.getId(), value );
   if( ret_value < 0 )
   {
      throw PropListIException();
   }
}

// Adds a filter to the filter pipeline
void DSetCreatPropList::setFilter( H5Z_filter_t filter, unsigned int flags, size_t cd_nelmts, const unsigned int cd_values[] ) const
{
   herr_t ret_value = H5Pset_filter( id, filter, flags, cd_nelmts, cd_values );
   if( ret_value < 0 )
   {
      throw PropListIException();
   }
}

// Returns the number of filters in the pipeline 
int DSetCreatPropList::getNfilters() const
{
   int num_filters = H5Pget_nfilters( id );
   if( num_filters < 0 )
   {
      throw PropListIException();
   }
   else
      return( num_filters );
}

// Returns information about a filter in a pipeline
H5Z_filter_t DSetCreatPropList::getFilter( int filter_number, unsigned int& flags, size_t& cd_nelmts, unsigned int* cd_values, size_t namelen, char name[] ) const
{
   H5Z_filter_t filter;
   filter = H5Pget_filter( id, filter_number, &flags, &cd_nelmts, cd_values, namelen, name );
   if( filter == H5Z_FILTER_ERROR )
   {
      throw PropListIException();
   }
   else
      return( filter );
}

// Adds an external file to the list of external files
void DSetCreatPropList::setExternal( const char* name, off_t offset, hsize_t size ) const
{
   herr_t ret_value = H5Pset_external( id, name, offset, size );
   if( ret_value < 0 )
   {
      throw PropListIException();
   }
}

// Returns the number of external files for a dataset 
int DSetCreatPropList::getExternalCount() const
{
   int num_ext_files = H5Pget_external_count( id );
   if( num_ext_files < 0 )
   {
      throw PropListIException();
   }
   else
      return( num_ext_files );
}

// Returns information about an external file
void DSetCreatPropList::getExternal( int idx, size_t name_size, char* name, off_t& offset, hsize_t& size ) const
{
   herr_t ret_value = H5Pget_external( id, idx, name_size, name, &offset, &size );
   if( ret_value < 0 )
   {
      throw PropListIException();
   }
}

// Default destructor
DSetCreatPropList::~DSetCreatPropList () {}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
