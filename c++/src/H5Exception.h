// C++ informative line for the emacs editor: -*- C++ -*-
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

#ifndef _H5Exception_H
#define _H5Exception_H

#include <string>

#ifndef H5_NO_NAMESPACE
namespace H5 {
using namespace std;
#endif

class __DLLCPP__ Exception {
   public:
	// Default constructor
	Exception();

	// Creates an exception with a function name where the failure occurs
	// and an optional detailed message
	Exception( const string& func_name, const string& message = NULL);
	Exception( const char* func_name, const char* message = NULL);

	// copy constructor
	Exception( const Exception& orig);

	// Returns the character string that describes an error specified by
	// a major error number.
	string getMajorString( H5E_major_t major_num ) const;

	// Returns the character string that describes an error specified by
	// a minor error number.
	string getMinorString( H5E_minor_t minor_num ) const;

	// Returns the detailed message set at the time the exception is thrown
	string getDetailMsg() const;
	const char* getCDetailMsg() const;	// C string of detailed message
	string getFuncName() const;	// function name as a string object
	const char* getCFuncName() const;	// function name as a char string 

	// Turns on the automatic error printing.
	static void setAutoPrint( H5E_auto_t func, void* client_data);

	// Turns off the automatic error printing.
	static void dontPrint();

	// Retrieves the current settings for the automatic error stack 
	// traversal function and its data.
	static void getAutoPrint( H5E_auto_t& func, void** client_data);

	// Clears the error stack for the current thread.
	static void clearErrorStack();

	// Walks the error stack for the current thread, calling the 
	// specified function.
	static void walkErrorStack( H5E_direction_t direction, 
				H5E_walk_t func, void* client_data);

	// Default error stack traversal callback function that prints 
	// error messages to the specified output stream.
	static void walkDefErrorStack( int n, H5E_error_t& err_desc,
				void* client_data);

	// Prints the error stack in a default manner.
	virtual void printError( FILE* stream = NULL ) const;

	// virtual Destructor
	virtual ~Exception();

   private:
// Because 'string' is not instantiated at compilation time, this
// warning is displayed; but the class is exported so the warning
// is harmless
#pragma warning(disable: 4251)
	string detailMessage;
	string funcName;
};

class __DLLCPP__ FileIException : public Exception {
   public:
	FileIException();
	FileIException( const string& func_name, const string& message = NULL);
	FileIException( const char* func_name, const char* message = NULL);
	virtual ~FileIException();
};

class __DLLCPP__ GroupIException : public Exception {
   public:
	GroupIException();
	GroupIException( const string& func_name, const string& message=NULL);
	GroupIException( const char* func_name, const char* message = NULL);
	virtual ~GroupIException();
};

class __DLLCPP__ DataSpaceIException : public Exception {
   public:
	DataSpaceIException();
	DataSpaceIException(const string& func_name, const string& message=NULL);
	DataSpaceIException(const char* func_name, const char* message = NULL);
	virtual ~DataSpaceIException();
};

class __DLLCPP__ DataTypeIException : public Exception {
   public:
	DataTypeIException();
	DataTypeIException(const string& func_name, const string& message = NULL);
	DataTypeIException(const char* func_name, const char* message = NULL);
	virtual ~DataTypeIException();
};

class __DLLCPP__ PropListIException : public Exception {
   public:
	PropListIException();
	PropListIException(const string& func_name, const string& message=NULL);
	PropListIException(const char* func_name, const char* message = NULL);
	virtual ~PropListIException();
};

class __DLLCPP__ DataSetIException : public Exception {
   public:
	DataSetIException();
	DataSetIException(const string& func_name, const string& message=NULL);
	DataSetIException(const char* func_name, const char* message = NULL);
	virtual ~DataSetIException();
};

class __DLLCPP__ AttributeIException : public Exception {
   public:
	AttributeIException();
	AttributeIException(const string& func_name, const string& message=NULL);
	AttributeIException(const char* func_name, const char* message = NULL);
	virtual ~AttributeIException();
};

class __DLLCPP__ ReferenceException : public Exception {
   public:
	ReferenceException();
	ReferenceException(const string& func_name, const string& message=NULL);
	ReferenceException(const char* func_name, const char* message = NULL);
	virtual ~ReferenceException();
};

class __DLLCPP__ LibraryIException : public Exception {
   public:
	LibraryIException();
	LibraryIException(const string& func_name, const string& message=NULL);
	LibraryIException(const char* func_name, const char* message = NULL);
	virtual ~LibraryIException();
};

class __DLLCPP__ IdComponentException : public Exception {
   public:
	IdComponentException();
	IdComponentException(const string& func_name, const string& message=NULL);
	IdComponentException(const char* func_name, const char* message = NULL);
	virtual ~IdComponentException();
};

#ifndef H5_NO_NAMESPACE
}
#endif

#endif // _H5Exception_H
