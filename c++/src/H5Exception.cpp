#ifdef OLD_HEADER_FILENAME
#include <iostream.h>
#else
#include <iostream>
#endif

#include "H5Include.h"
#include <string>

// Added this line for CC to work at this time.  Will remove it when 
// the problem is fixed. BMR - 10/30/00

#include "H5Exception.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

Exception::Exception() : detailMessage("") {}

// digital alpha (gondolin) produces compilation error at static_cast<string>
// so I replaced this constructor by the one below it
//Exception::Exception( const char* message) : detailMessage(static_cast<string>(message)) {}

Exception::Exception( const char* message)
{
   detailMessage = string( message ); 
}

Exception::Exception( const string& message ) : detailMessage( message ) {}

// copy constructor
Exception::Exception( const Exception& orig )
{
   detailMessage = orig.detailMessage;
}

// Returns the character string that describes an error specified by
// a major error number.
string Exception::getMajorString( H5E_major_t major_num ) const
{
   // calls the C API routine to get the major string - Note: in the
   // failure case, the string "Invalid major error number" will be returned.
   string major_str( H5Eget_major( major_num ));
   return( major_str );
}

// Returns the character string that describes an error specified by
// a minor error number.
string Exception::getMinorString( H5E_minor_t minor_num ) const
{
   // calls the C API routine to get the minor string - Note: in the
   // failure case, the string "Invalid minor error number" will be returned.
   string minor_str( H5Eget_minor( minor_num ));
   return( minor_str );
}

// Turns on the automatic error printing.
void Exception::setAutoPrint( H5E_auto_t func, void* client_data ) const
{
   // calls the C API routine H5Eset_auto to set the auto printing to
   // the specified function.
   herr_t ret_value = H5Eset_auto( func, client_data );
   if( ret_value < 0 )
      throw Exception( "setAutoPrint: H5Eset_auto fails" );
}

// Turns off the automatic error printing.
void Exception::dontPrint()
{
   // calls the C API routine H5Eset_auto with NULL parameters to turn
   // off the automatic error printing.
   herr_t ret_value = H5Eset_auto( NULL, NULL );
   if( ret_value < 0 )
      throw Exception( "dontPrint: H5Eset_auto fails" );
}

// Retrieves the current settings for the automatic error stack traversal
// function and its data.
void Exception::getAutoPrint( H5E_auto_t& func, void** client_data ) const
{
   // calls the C API routine H5Eget_auto to get the current setting of
   // the automatic error printing 
   herr_t ret_value = H5Eget_auto( &func, client_data );
   if( ret_value < 0 )
      throw Exception( "getAutoPrint: H5Eget_auto fails" );
}

// Clears the error stack for the current thread.
void Exception::clearErrorStack() const
{
   // calls the C API routine H5Eclear to clear the error stack
   herr_t ret_value = H5Eclear();
   if( ret_value < 0 )
      throw Exception( "clearErrorStack: H5Eclear fails" );
}

// Walks the error stack for the current thread, calling the specified function.
void Exception::walkErrorStack( H5E_direction_t direction, H5E_walk_t func, void* client_data ) const
{
   // calls the C API routine H5Ewalk to walk the error stack
   herr_t ret_value = H5Ewalk( direction, func, client_data );
   if( ret_value < 0 )
      throw Exception( "walkErrorStack: H5Ewalk fails" );
}

// Default error stack traversal callback function that prints error
// messages to the specified output stream.
void Exception::walkDefErrorStack( int n, H5E_error_t& err_desc, void* client_data ) const
{
   // calls the C API routine H5Ewalk_cb to walk the error stack
   herr_t ret_value = H5Ewalk_cb( n, &err_desc, client_data );
   if( ret_value < 0 )
      throw Exception( "walkDefErrorStack: H5Ewalk_cb fails" );
}

// Returns the detailed message set at the time the exception is thrown
string Exception::getDetailMesg() const
{
   return( detailMessage );
}

// Prints the error stack in a default manner.
void Exception::printError( FILE* stream ) const
{
   herr_t ret_value = H5Eprint( NULL ); // print to stderr
   if( ret_value < 0 )
      throw Exception( "printError: H5Eprint fails" );
}

Exception::~Exception()
{
   herr_t ret_value = H5Eprint( NULL ); // print to stderr
   if( ret_value < 0 )
      throw Exception( "printError: H5Eprint fails" );
}

FileIException::FileIException():Exception(){}
FileIException::FileIException( string message ): Exception( message ){}
FileIException::~FileIException() {}

GroupIException::GroupIException():Exception(){}
GroupIException::GroupIException( string message ): Exception( message ){}
GroupIException::~GroupIException() {}

DataSpaceIException::DataSpaceIException():Exception(){}
DataSpaceIException::DataSpaceIException( string message ): Exception( message ) {}
DataSpaceIException::~DataSpaceIException() {}

DataTypeIException::DataTypeIException():Exception(){}
DataTypeIException::DataTypeIException( string message ): Exception( message ) {}
DataTypeIException::~DataTypeIException() {}

PropListIException::PropListIException():Exception(){}
PropListIException::PropListIException( string message ): Exception( message ) {}
PropListIException::~PropListIException() {}

DataSetIException::DataSetIException():Exception(){}
DataSetIException::DataSetIException( string message ): Exception( message ) {}
DataSetIException::~DataSetIException() {}

AttributeIException::AttributeIException():Exception(){}
AttributeIException::AttributeIException( string message ): Exception( message ) {}
AttributeIException::~AttributeIException() {}

ReferenceException::ReferenceException():Exception(){}
ReferenceException::ReferenceException( string message ): Exception( message ) {}
ReferenceException::~ReferenceException() {}

LibraryIException::LibraryIException():Exception(){}
LibraryIException::LibraryIException( string message ): Exception( message ) {}
LibraryIException::~LibraryIException() {}

IdComponentException::IdComponentException(): Exception() {}
IdComponentException::IdComponentException( string message ): Exception( message ) {}
IdComponentException::~IdComponentException() {}

// The following are from Java API but not done here:
// AtomException, BtreeException, DataFiltersException, 
// ExternalFileListException, FunctionEntryExitException, 
// HeapException, InternalErrorException, LowLevelIOException, 
// MetaDataCacheException, ResourceUnavailableException, 
// SymbolTableException, ObjectHeaderException, FunctionArgumentException,
// DataStorageException

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
