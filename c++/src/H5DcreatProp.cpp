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
      throw PropListIException("DSetCreatPropList::setChunk", "H5Pset_chunk failed");
   }
}

// Gets the layout of the raw data storage of the data that uses this
// property list
H5D_layout_t DSetCreatPropList::getLayout() const
{
   H5D_layout_t layout = H5Pget_layout( id );
   if( layout == H5D_LAYOUT_ERROR )
   {
      throw PropListIException("DSetCreatPropList::getLayout", 
		"H5Pget_layout returns H5D_LAYOUT_ERROR");
   }
   return( layout );
}

// Retrieves the size of the chunks used to store a chunked layout dataset.
int DSetCreatPropList::getChunk( int max_ndims, hsize_t* dim ) const
{
   int chunk_size = H5Pget_chunk( id, max_ndims, dim );
   if( chunk_size < 0 )
   {
      throw PropListIException("DSetCreatPropList::getChunk", 
		"H5Pget_chunk returns negative chunk size");
   }
   return( chunk_size );
}

// Sets the type of storage used store the raw data for a dataset.
void DSetCreatPropList::setLayout(hid_t plist, H5D_layout_t layout ) const
{
   herr_t ret_value = H5Pset_layout( id, layout );
   if( ret_value < 0 )
   {
      throw PropListIException("DSetCreatPropList::setLayout",
		"H5Pset_layout failed");
   }
}

// Sets compression method and compression level
void DSetCreatPropList::setDeflate( int level ) const
{
   herr_t ret_value = H5Pset_deflate( id, level );
   if( ret_value < 0 )
   {
      throw PropListIException("DSetCreatPropList::setDeflate",
		"H5Pset_deflate failed");
   }
}

// Sets a dataset fill value
void DSetCreatPropList::setFillValue( DataType& fvalue_type, const void* value ) const
{
   herr_t ret_value = H5Pset_fill_value( id, fvalue_type.getId(), value );
   if( ret_value < 0 )
   {
      throw PropListIException("DSetCreatPropList::setFillValue",
                "H5Pset_fill_value failed");
   }
}

// Retrieves a dataset fill value
void DSetCreatPropList::getFillValue( DataType& fvalue_type, void* value ) const
{
   herr_t ret_value = H5Pget_fill_value( id, fvalue_type.getId(), value );
   if( ret_value < 0 )
   {
      throw PropListIException("DSetCreatPropList::getFillValue",
                "H5Pget_fill_value failed");
   }
}

// Adds a filter to the filter pipeline
void DSetCreatPropList::setFilter( H5Z_filter_t filter, unsigned int flags, size_t cd_nelmts, const unsigned int cd_values[] ) const
{
   herr_t ret_value = H5Pset_filter( id, filter, flags, cd_nelmts, cd_values );
   if( ret_value < 0 )
   {
      throw PropListIException("DSetCreatPropList::setFilter",
                "H5Pset_filter failed");
   }
}

// Returns the number of filters in the pipeline 
int DSetCreatPropList::getNfilters() const
{
   int num_filters = H5Pget_nfilters( id );
   if( num_filters < 0 )
   {
      throw PropListIException("DSetCreatPropList::getNfilters",
                "H5Pget_nfilters returned negative number of filters");
   }
   else
      return( num_filters );
}

// Returns information about a filter in a pipeline
H5Z_filter_t DSetCreatPropList::getFilter( int filter_number, unsigned int& flags, size_t& cd_nelmts, unsigned int* cd_values, size_t namelen, char name[] ) const
{
   H5Z_filter_t filter;
   filter = H5Pget_filter( id, filter_number, &flags, &cd_nelmts, 
				cd_values, namelen, name );
   if( filter == H5Z_FILTER_ERROR )
   {
      throw PropListIException("DSetCreatPropList::getFilter",
                "H5Pget_filter returned H5Z_FILTER_ERROR");
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
      throw PropListIException("DSetCreatPropList::setExternal",
                "H5Pset_external failed");
   }
}

// Returns the number of external files for a dataset 
int DSetCreatPropList::getExternalCount() const
{
   int num_ext_files = H5Pget_external_count( id );
   if( num_ext_files < 0 )
   {
      throw PropListIException("DSetCreatPropList::getExternalCount",
                "H5Pget_external_count returns negative number of external files");
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
      throw PropListIException("DSetCreatPropList::getExternal",
                "H5Pget_external failed");
   }
}

// Default destructor
DSetCreatPropList::~DSetCreatPropList () {}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
