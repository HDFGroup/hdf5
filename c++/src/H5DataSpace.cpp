#include <string>

#include "H5Include.h"
#include "H5Exception.h"
#include "H5RefCounter.h"
#include "H5IdComponent.h"
#include "H5Idtemplates.h"
#include "H5DataSpace.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

const DataSpace DataSpace::ALL( H5S_ALL );

// Default constructor
DataSpace::DataSpace() : IdComponent() {}

// This constructor creates a DataSpace instance, given a dataspace type
DataSpace::DataSpace( H5S_class_t type ) : IdComponent()
{
   id = H5Screate( type );
   if( id <= 0 )
   {
      throw DataSpaceIException();
   }
}

// Creates a new simple data space and opens it for access.
DataSpace::DataSpace( int rank, const hsize_t * dims, const hsize_t * maxdims) : IdComponent()
{
   id = H5Screate_simple( rank, dims, maxdims );
   if( id <= 0 )
   {
      throw DataSpaceIException();
   }
}

/* Constructor that takes an existing dataspace id
Description:
	Uses an HDF5 id to create a DataSpace identifier instance.  This id can be either an existing dataspace id or a default dataspace id.  Design note: in the case of default dataspace, the identifier still has reference counter; the p_close function will take care of not to call H5Sclose on the default id.
*/
DataSpace::DataSpace( const hid_t space_id ) : IdComponent( space_id ) {}

// Copy constructor: makes a copy of the original DataSpace instance
DataSpace::DataSpace( const DataSpace& original ) : IdComponent( original ) {}

// Makes a copy of an existing dataspace
void DataSpace::copy( const DataSpace& like_space )
{
   // reset the identifier of this instance - send 'this' in so that
   // H5Sclose can be called appropriately
   resetIdComponent( this );

   // call C routine to copy the dataspace 
   id = H5Scopy( like_space.getId() );

   // points to the same ref counter
   ref_count = new RefCounter;

   if( id <= 0 )
   {
      throw DataSpaceIException();
   }
}

// Determines whether this dataspace is a simple dataspace.
bool DataSpace::isSimple () const
{
   htri_t simple = H5Sis_simple( id );
   if( simple > 0 )
      return true;
   else if( simple == 0 )
      return false;
   else
   {
      throw DataSpaceIException();
   }
}

// Sets the offset of this simple dataspace.
void DataSpace::offsetSimple ( const hssize_t* offset ) const
{
   herr_t ret_value = H5Soffset_simple( id, offset );
   if( ret_value < 0 )
   {
      throw DataSpaceIException();
   }
}

// Retrieves dataspace dimension size and maximum size
int DataSpace::getSimpleExtentDims ( hsize_t *dims, hsize_t *maxdims ) const
{
   int ndims = H5Sget_simple_extent_dims( id, dims, maxdims );
   if( ndims < 0 )
   {
      throw DataSpaceIException();
   }
   return( ndims );
}

// Determines the dimensionality of a dataspace
int DataSpace::getSimpleExtentNdims () const
{
   int ndims = H5Sget_simple_extent_ndims( id );
   if( ndims < 0 )
   {
      throw DataSpaceIException();
   }
   return( ndims );
}

// Determines the number of elements in a dataspace
// 12/05/00: due to C API change
//	return type hssize_t vs. hsize_t
//	num_elements = -1 when failure occurs vs. 0
hssize_t DataSpace::getSimpleExtentNpoints () const
{
   hssize_t num_elements = H5Sget_simple_extent_npoints( id );

   if( num_elements > -1 )
      return( num_elements );
   else
   {
      throw DataSpaceIException();
   }
}

// Determine the current class of a dataspace
H5S_class_t DataSpace::getSimpleExtentType () const
{
   H5S_class_t class_name = H5Sget_simple_extent_type( id );
   if( class_name == H5S_NO_CLASS )
   {
      throw DataSpaceIException();
   }
   return( class_name );
}

// Copies the extent of a dataspace
void DataSpace::extentCopy ( DataSpace& dest_space ) const
{
   hid_t dest_space_id = dest_space.getId();
   herr_t ret_value = H5Sextent_copy( dest_space_id, id );
   if( ret_value < 0 )
   {
      throw DataSpaceIException();
   }
}

// Sets or resets the size of an existing dataspace
void DataSpace::setExtentSimple( int rank, const hsize_t *current_size, const hsize_t *maximum_size ) const
{
   herr_t ret_value;
   ret_value = H5Sset_extent_simple( id, rank, current_size, maximum_size );
   if( ret_value < 0 )
   {
      throw DataSpaceIException();
   }
}

// Removes the extent from a dataspace
void DataSpace::setExtentNone () const
{
   herr_t ret_value = H5Sset_extent_none( id );
   if( ret_value < 0 )
   {
      throw DataSpaceIException();
   }
}

// Determines the number of elements in a dataspace selection
hssize_t DataSpace::getSelectNpoints () const
{
   hssize_t num_elements = H5Sget_select_npoints( id );
   if( num_elements < 0 )
   {
      throw DataSpaceIException();
   }
   return( num_elements );
}

// Get number of hyperslab blocks
hssize_t DataSpace::getSelectHyperNblocks () const
{
   hssize_t num_blocks = H5Sget_select_hyper_nblocks( id );
   if( num_blocks < 0 )
   {
      throw DataSpaceIException();
   }
   return( num_blocks );
}

// Gets the list of hyperslab blocks currently selected
void DataSpace::getSelectHyperBlocklist( hsize_t startblock, hsize_t numblocks, hsize_t *buf ) const
{
   herr_t ret_value;
   ret_value = H5Sget_select_hyper_blocklist( id, startblock, numblocks, buf );
   if( ret_value < 0 )
   {
      throw DataSpaceIException();
   }
}

// Gets the number of element points in the current selection
hssize_t DataSpace::getSelectElemNpoints () const
{
   hssize_t num_points = H5Sget_select_elem_npoints( id );
   if( num_points < 0 )
   {
      throw DataSpaceIException();
   }
   return( num_points );
}

// Gets the list of element points currently selected
void DataSpace::getSelectElemPointlist ( hsize_t startpoint, hsize_t numpoints, hsize_t *buf ) const
{
   herr_t ret_value;
   ret_value = H5Sget_select_elem_pointlist( id, startpoint, numpoints, buf );
   if( ret_value < 0 )
   {
      throw DataSpaceIException();
   }
}

// Gets the bounding box containing the current selection
void DataSpace::getSelectBounds ( hsize_t* start, hsize_t* end ) const
{
   herr_t ret_value = H5Sget_select_bounds( id, start, end );
   if( ret_value < 0 )
   {
      throw DataSpaceIException();
   }
}

// Selects array elements to be included in the selection for a dataspace
void DataSpace::selectElements ( H5S_seloper_t op, const size_t num_elements, const hssize_t *coord[ ] ) const
{
   herr_t ret_value;
   ret_value = H5Sselect_elements( id, op, num_elements, coord );
   if( ret_value < 0 )
   {
      throw DataSpaceIException();
   }
}

// Selects the entire dataspace
void DataSpace::selectAll () const
{
   herr_t ret_value = H5Sselect_all( id );
   if( ret_value < 0 )
   {
      throw DataSpaceIException();
   }
}

//Resets the selection region to include no elements
void DataSpace::selectNone () const
{
  herr_t ret_value = H5Sselect_none( id );
   if( ret_value < 0 )
   {
      throw DataSpaceIException();
   }
}

// Verifies that the selection is within the extent of the dataspace
bool DataSpace::selectValid () const
{
  htri_t ret_value = H5Sselect_valid( id );
   if( ret_value > 0 )
      return true;
   else if( ret_value == 0 )
      return false;
   else
   {
      throw DataSpaceIException();
   }
}

// Selects a hyperslab region to add to the current selected region
void DataSpace::selectHyperslab( H5S_seloper_t op, const hsize_t *count, const hssize_t *start, const hsize_t *stride, const hsize_t *block ) const
{
   herr_t ret_value;
   ret_value = H5Sselect_hyperslab( id, op, start, stride, count, block );
   if( ret_value < 0 )
   {
      throw DataSpaceIException();
   }
}

// Closes the dataspace if it is not a constant
void DataSpace::p_close() const
{
   hid_t space_id = id;
   if( space_id != H5S_ALL ) // not a constant, should call H5Sclose
   {
      herr_t ret_value = H5Sclose( space_id );
      if( ret_value < 0 )
      {
         throw DataSpaceIException();
      }
   }
}

// The destructor of this instance calls IdComponent::reset to
// reset its identifier - no longer true
// Older compilers (baldric) don't support template member functions
// and IdComponent::reset is one; so at this time, the resetId is not
// a member function so it can be template to work around that problem.
DataSpace::~DataSpace()
{  
   // The dataspace id will be closed properly
   resetIdComponent( this );
}  

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
