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

#ifdef OLD_HEADER_FILENAME
#include <iostream.h>
#else
#include <iostream>
#endif

#include "H5Include.h"
#include "H5Exception.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
	using namespace std;
#endif

// Default constructor
Exception::Exception() : detailMessage(""), funcName("") {}

// Constructor taking only a detailed message as string object
//Exception::Exception(const string& message) : detailMessage(message) {}

// Constructor taking a function name and a detailed message as string objects
Exception::Exception(const string& func_name, const string& message) : detailMessage(message), funcName(func_name) {}

// Constructor taking a detailed message as character string
// digital alpha (gondolin) produces compilation error at static_cast<string>
// so I replaced this constructor by the one below it
//Exception::Exception( const char* message) : detailMessage(static_cast<string>(message)) {}
//Exception::Exception(const char* message)
//{
   //detailMessage = string(message); 
//}

// Constructor taking a function name and a detailed message as character
// strings
Exception::Exception(const char* func_name, const char* message)
{
    detailMessage = string(message);
    if (func_name != NULL)
	funcName = string(func_name);
}

// copy constructor
Exception::Exception( const Exception& orig )
{
   detailMessage = orig.detailMessage;
   funcName = orig.funcName;
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
void Exception::setAutoPrint( H5E_auto_t func, void* client_data )
{
   // calls the C API routine H5Eset_auto to set the auto printing to
   // the specified function.
   herr_t ret_value = H5Eset_auto( func, client_data );
   if( ret_value < 0 )
      throw Exception( "Exception::setAutoPrint", "H5Eset_auto failed" );
}

// Turns off the automatic error printing.
void Exception::dontPrint()
{
   // calls the C API routine H5Eset_auto with NULL parameters to turn
   // off the automatic error printing.
   herr_t ret_value = H5Eset_auto( NULL, NULL );
   if( ret_value < 0 )
      throw Exception( "Exception::dontPrint", "H5Eset_auto failed" );
}

// Retrieves the current settings for the automatic error stack traversal
// function and its data.
void Exception::getAutoPrint( H5E_auto_t& func, void** client_data )
{
   // calls the C API routine H5Eget_auto to get the current setting of
   // the automatic error printing 
   herr_t ret_value = H5Eget_auto( &func, client_data );
   if( ret_value < 0 )
      throw Exception( "Exception::getAutoPrint", "H5Eget_auto failed" );
}

// Clears the error stack for the current thread.
void Exception::clearErrorStack()
{
   // calls the C API routine H5Eclear to clear the error stack
   herr_t ret_value = H5Eclear();
   if( ret_value < 0 )
      throw Exception( "Exception::clearErrorStack", "H5Eclear failed" );
}

// Walks the error stack for the current thread, calling the specified function.
void Exception::walkErrorStack( H5E_direction_t direction, H5E_walk_t func, void* client_data )
{
   // calls the C API routine H5Ewalk to walk the error stack
   herr_t ret_value = H5Ewalk( direction, func, client_data );
   if( ret_value < 0 )
      throw Exception( "Exception::walkErrorStack", "H5Ewalk failed" );
}

// Default error stack traversal callback function that prints error
// messages to the specified output stream.
void Exception::walkDefErrorStack( int n, H5E_error_t& err_desc, void* client_data )
{
   // calls the C API routine H5Ewalk_cb to walk the error stack
   herr_t ret_value = H5Ewalk_cb( n, &err_desc, client_data );
   if( ret_value < 0 )
      throw Exception( "Exception::walkDefErrorStack", "H5Ewalk_cb failed" );
}

// Returns the detailed message set at the time the exception is thrown
string Exception::getDetailMsg() const
{
   return(detailMessage);
}

const char* Exception::getCDetailMsg() const
{
   return(detailMessage.c_str());
}

// Returns the function name where the exception is thrown
string Exception::getFuncName() const
{
   return(funcName);
}
const char* Exception::getCFuncName() const
{
   return(funcName.c_str());
}

// Prints the error stack in a default manner.
void Exception::printError( FILE* stream ) const
{
   herr_t ret_value = H5Eprint( NULL ); // print to stderr
   if( ret_value < 0 )
      throw Exception( "Exception::printError", "H5Eprint failed" );
}

Exception::~Exception() {}

FileIException::FileIException():Exception(){}
FileIException::FileIException(const string& func_name, const string& message) : Exception(func_name, message) {}
FileIException::FileIException(const char* func_name, const char* message) : Exception(func_name, message) {}
FileIException::~FileIException() {}

GroupIException::GroupIException():Exception(){}
GroupIException::GroupIException(const string& func_name, const string& message) : Exception(func_name, message) {}
GroupIException::GroupIException(const char* func_name, const char* message) : Exception(func_name, message) {}
GroupIException::~GroupIException() {}

DataSpaceIException::DataSpaceIException():Exception(){}
DataSpaceIException::DataSpaceIException(const string& func_name, const string& message) : Exception(func_name, message) {}
DataSpaceIException::DataSpaceIException(const char* func_name, const char* message) : Exception(func_name, message) {}
DataSpaceIException::~DataSpaceIException() {}

DataTypeIException::DataTypeIException():Exception(){}
DataTypeIException::DataTypeIException(const string& func_name, const string& message) : Exception(func_name, message) {}
DataTypeIException::DataTypeIException(const char* func_name, const char* message) : Exception(func_name, message) {}
DataTypeIException::~DataTypeIException() {}

PropListIException::PropListIException():Exception(){}
PropListIException::PropListIException(const string& func_name, const string& message) : Exception(func_name, message) {}
PropListIException::PropListIException(const char* func_name, const char* message) : Exception(func_name, message) {}
PropListIException::~PropListIException() {}

DataSetIException::DataSetIException():Exception(){}
DataSetIException::DataSetIException(const string& func_name, const string& message) : Exception(func_name, message) {}
DataSetIException::DataSetIException(const char* func_name, const char* message) : Exception(func_name, message) {}
DataSetIException::~DataSetIException() {}

AttributeIException::AttributeIException():Exception(){}
AttributeIException::AttributeIException(const string& func_name, const string& message) : Exception(func_name, message) {}
AttributeIException::AttributeIException(const char* func_name, const char* message) : Exception(func_name, message) {}
AttributeIException::~AttributeIException() {}

ReferenceException::ReferenceException():Exception(){}
ReferenceException::ReferenceException(const string& func_name, const string& message) : Exception(func_name, message) {}
ReferenceException::ReferenceException(const char* func_name, const char* message) : Exception(func_name, message) {}
ReferenceException::~ReferenceException() {}

LibraryIException::LibraryIException():Exception(){}
LibraryIException::LibraryIException(const string& func_name, const string& message) : Exception(func_name, message) {}
LibraryIException::LibraryIException(const char* func_name, const char* message) : Exception(func_name, message) {}
LibraryIException::~LibraryIException() {}

IdComponentException::IdComponentException(): Exception() {}
IdComponentException::IdComponentException(const string& func_name, const string& message) : Exception(func_name, message) {}
IdComponentException::IdComponentException(const char* func_name, const char* message) : Exception(func_name, message) {}
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
