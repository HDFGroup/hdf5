#ifndef _H5Exception_H
#define _H5Exception_H

#include <string>

#ifndef H5_NO_NAMESPACE
namespace H5 {
using namespace std;
#endif

class Exception {
   public:
	// Creates an exception with no message
	Exception();

	// Creates an exception with a detailed message
	Exception( const string& message );

	Exception( const char* message);

	// copy constructor
	Exception( const Exception& orig );

	// Returns the character string that describes an error specified by
	// a major error number.
	string getMajorString( H5E_major_t major_num ) const;

	// Returns the character string that describes an error specified by
	// a minor error number.
	string getMinorString( H5E_minor_t minor_num ) const;

	// Returns the detailed message set at the time the exception is thrown
	string getDetailMesg() const;

	// Turns on the automatic error printing.
	void setAutoPrint( H5E_auto_t func, 
				void* client_data ) const;

	// Turns off the automatic error printing.
	static void dontPrint();

	// Retrieves the current settings for the automatic error stack 
	// traversal function and its data.
	void getAutoPrint( H5E_auto_t& func, 
				void** client_data ) const;

	// Clears the error stack for the current thread.
	void clearErrorStack() const;

	// Walks the error stack for the current thread, calling the 
	// specified function.
	void walkErrorStack( H5E_direction_t direction, 
				H5E_walk_t func, void* client_data ) const;

	// Default error stack traversal callback function that prints 
	// error messages to the specified output stream.
	void walkDefErrorStack( int n, H5E_error_t& err_desc,
				void* client_data ) const;

	// Prints the error stack in a default manner.
	//void printError() const;
	virtual void printError( FILE* stream = NULL ) const;

	// virtual Destructor
	virtual ~Exception();

   private:
	string detailMessage;
};

class FileIException : public Exception {
   public:
	FileIException();
	FileIException( string message );
	virtual ~FileIException();
};

class GroupIException : public Exception {
   public:
	GroupIException();
	GroupIException( string message );
	virtual ~GroupIException();
};

class DataSpaceIException : public Exception {
   public:
	DataSpaceIException();
	DataSpaceIException( string message );
	virtual ~DataSpaceIException();
};

class DataTypeIException : public Exception {
   public:
	DataTypeIException();
	DataTypeIException( string message );
	virtual ~DataTypeIException();
};

class PropListIException : public Exception {
   public:
	PropListIException();
	PropListIException( string message );
	virtual ~PropListIException();
};

class DataSetIException : public Exception {
   public:
	DataSetIException();
	DataSetIException( string message );
	virtual ~DataSetIException();
};

class AttributeIException : public Exception {
   public:
	AttributeIException();
	AttributeIException( string message );
	virtual ~AttributeIException();
};

class ReferenceException : public Exception {
   public:
	ReferenceException();
	ReferenceException( string message );
	virtual ~ReferenceException();
};

class LibraryIException : public Exception {
   public:
	LibraryIException();
	LibraryIException( string message );
	virtual ~LibraryIException();
};

class IdComponentException : public Exception {
   public:
	IdComponentException();
	IdComponentException( string message );
	virtual ~IdComponentException();
};

// The following are from Java API but not done here:
// AtomException, BtreeException, DataFiltersException, 
// ExternalFilelistException, FunctionEntryExitException, 
// HeapException, InternalErrorException, LowLevelIOException, 
// MetaDataCacheException, ResourceUnavailableException, 
// SymbolTableException, ObjectHeaderException, FunctionArgumentException,
// DataStorageException
#ifndef H5_NO_NAMESPACE
}
#endif

#endif // _H5Exception_H
