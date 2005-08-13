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

// PredType holds the definition of all the HDF5 predefined datatypes.
// These types can only be made copy of, not created by H5Tcreate or
// closed by H5Tclose.  They are treated as constants.
/////////////////////////////////////////////////////////////////////

#ifndef _H5PredType_H
#define _H5PredType_H

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

class H5_DLLCPP PredType : public AtomType {
   public:
	// Makes a copy of the predefined type and stores the new
	// id in the left hand side object.
	PredType& operator=( const PredType& rhs );

	// Copy constructor - makes copy of the original object
	PredType( const PredType& original );

	// Returns the HDF5 predefined type id.
	virtual hid_t getId() const;

	// Noop destructor
	virtual ~PredType();

	// Declaration of predefined types; their definition is in H5PredType.cpp
	static const PredType STD_I8BE;
	static const PredType STD_I8LE;
	static const PredType STD_I16BE;
	static const PredType STD_I16LE;
	static const PredType STD_I32BE;
	static const PredType STD_I32LE;
	static const PredType STD_I64BE;
	static const PredType STD_I64LE;
	static const PredType STD_U8BE;
	static const PredType STD_U8LE;
	static const PredType STD_U16BE;
	static const PredType STD_U16LE;
	static const PredType STD_U32BE;
	static const PredType STD_U32LE;
	static const PredType STD_U64BE;
	static const PredType STD_U64LE;
	static const PredType STD_B8BE;
	static const PredType STD_B8LE;
	static const PredType STD_B16BE;
	static const PredType STD_B16LE;
	static const PredType STD_B32BE;
	static const PredType STD_B32LE;
	static const PredType STD_B64BE;
	static const PredType STD_B64LE;
	static const PredType STD_REF_OBJ;
	static const PredType STD_REF_DSETREG;

	static const PredType C_S1;
	static const PredType FORTRAN_S1;

	static const PredType IEEE_F32BE;
	static const PredType IEEE_F32LE;
	static const PredType IEEE_F64BE;
	static const PredType IEEE_F64LE;

	static const PredType UNIX_D32BE;
	static const PredType UNIX_D32LE;
	static const PredType UNIX_D64BE;
	static const PredType UNIX_D64LE;

	static const PredType INTEL_I8;
	static const PredType INTEL_I16;
	static const PredType INTEL_I32;
	static const PredType INTEL_I64;
	static const PredType INTEL_U8;
	static const PredType INTEL_U16;
	static const PredType INTEL_U32;
	static const PredType INTEL_U64;
	static const PredType INTEL_B8;
	static const PredType INTEL_B16;
	static const PredType INTEL_B32;
	static const PredType INTEL_B64;
	static const PredType INTEL_F32;
	static const PredType INTEL_F64;

	static const PredType ALPHA_I8;
	static const PredType ALPHA_I16;
	static const PredType ALPHA_I32;
	static const PredType ALPHA_I64;
	static const PredType ALPHA_U8;
	static const PredType ALPHA_U16;
	static const PredType ALPHA_U32;
	static const PredType ALPHA_U64;
	static const PredType ALPHA_B8;
	static const PredType ALPHA_B16;
	static const PredType ALPHA_B32;
	static const PredType ALPHA_B64;
	static const PredType ALPHA_F32;
	static const PredType ALPHA_F64;

	static const PredType MIPS_I8;
	static const PredType MIPS_I16;
	static const PredType MIPS_I32;
	static const PredType MIPS_I64;
	static const PredType MIPS_U8;
	static const PredType MIPS_U16;
	static const PredType MIPS_U32;
	static const PredType MIPS_U64;
	static const PredType MIPS_B8;
	static const PredType MIPS_B16;
	static const PredType MIPS_B32;
	static const PredType MIPS_B64;
	static const PredType MIPS_F32;
	static const PredType MIPS_F64;

	static const PredType NATIVE_CHAR;
	static const PredType NATIVE_SCHAR;
	static const PredType NATIVE_UCHAR;
	static const PredType NATIVE_SHORT;
	static const PredType NATIVE_USHORT;
	static const PredType NATIVE_INT;
	static const PredType NATIVE_UINT;
	static const PredType NATIVE_LONG;
	static const PredType NATIVE_ULONG;
	static const PredType NATIVE_LLONG;
	static const PredType NATIVE_ULLONG;
	static const PredType NATIVE_FLOAT;
	static const PredType NATIVE_DOUBLE;
	static const PredType NATIVE_LDOUBLE;
	static const PredType NATIVE_B8;
	static const PredType NATIVE_B16;
	static const PredType NATIVE_B32;
	static const PredType NATIVE_B64;
	static const PredType NATIVE_OPAQUE;
	static const PredType NATIVE_HSIZE;
	static const PredType NATIVE_HSSIZE;
	static const PredType NATIVE_HERR;
	static const PredType NATIVE_HBOOL;

	static const PredType NATIVE_INT8;
	static const PredType NATIVE_UINT8;
	static const PredType NATIVE_INT_LEAST8;
	static const PredType NATIVE_UINT_LEAST8;
	static const PredType NATIVE_INT_FAST8;
	static const PredType NATIVE_UINT_FAST8;

	static const PredType NATIVE_INT16;
	static const PredType NATIVE_UINT16;
	static const PredType NATIVE_INT_LEAST16;
	static const PredType NATIVE_UINT_LEAST16;
	static const PredType NATIVE_INT_FAST16;
	static const PredType NATIVE_UINT_FAST16;

	static const PredType NATIVE_INT32;
	static const PredType NATIVE_UINT32;
	static const PredType NATIVE_INT_LEAST32;
	static const PredType NATIVE_UINT_LEAST32;
	static const PredType NATIVE_INT_FAST32;
	static const PredType NATIVE_UINT_FAST32;

	static const PredType NATIVE_INT64;
	static const PredType NATIVE_UINT64;
	static const PredType NATIVE_INT_LEAST64;
	static const PredType NATIVE_UINT_LEAST64;
	static const PredType NATIVE_INT_FAST64;
	static const PredType NATIVE_UINT_FAST64;

#ifndef DOXYGEN_SHOULD_SKIP_THIS
	// These dummy functions do not inherit from DataType - they'll
	// throw a DataTypeIException if invoked.
	void commit( H5Object& loc, const string& name );
	void commit( H5Object& loc, const char* name );
	bool committed();
#endif // DOXYGEN_SHOULD_SKIP_THIS

   private:
	// added this to work around the atexit/global destructor problem
	// temporarily - it'll prevent the use of atexit to clean up
	static const PredType NotAtexit;	// not working yet

	// This enum type is used by this class only to handle the
	// global PredType objects.  These values will ensure that the
	// application receives an appropriate and uptodated id for an
	// HDF5 predefined type; particularly usefull when the application
	// closes and opens the library again.
	enum predefined_types {

	    INVALID = 0, E_C_S1 = 1, E_FORTRAN_S1,

	    E_STD_I8BE, E_STD_I8LE, E_STD_I16BE, E_STD_I16LE, E_STD_I32BE,
	    E_STD_I32LE, E_STD_I64BE, E_STD_I64LE, E_STD_U8BE, E_STD_U8LE,
	    E_STD_U16BE, E_STD_U16LE, E_STD_U32BE, E_STD_U32LE, E_STD_U64BE,
	    E_STD_U64LE, E_STD_B8BE, E_STD_B8LE, E_STD_B16BE, E_STD_B16LE,
	    E_STD_B32BE, E_STD_B32LE, E_STD_B64BE, E_STD_B64LE, E_STD_REF_OBJ,
	    E_STD_REF_DSETREG,

	    E_IEEE_F32BE, E_IEEE_F32LE, E_IEEE_F64BE, E_IEEE_F64LE,

	    E_UNIX_D32BE, E_UNIX_D32LE, E_UNIX_D64BE, E_UNIX_D64LE,

	    E_INTEL_I8, E_INTEL_I16, E_INTEL_I32, E_INTEL_I64, E_INTEL_U8,
	    E_INTEL_U16, E_INTEL_U32, E_INTEL_U64, E_INTEL_B8, E_INTEL_B16,
	    E_INTEL_B32, E_INTEL_B64, E_INTEL_F32, E_INTEL_F64,

	    E_ALPHA_I8, E_ALPHA_I16, E_ALPHA_I32, E_ALPHA_I64, E_ALPHA_U8,
	    E_ALPHA_U16, E_ALPHA_U32, E_ALPHA_U64, E_ALPHA_B8, E_ALPHA_B16,
	    E_ALPHA_B32, E_ALPHA_B64, E_ALPHA_F32, E_ALPHA_F64,

	    E_MIPS_I8, E_MIPS_I16, E_MIPS_I32, E_MIPS_I64, E_MIPS_U8,
	    E_MIPS_U16, E_MIPS_U32, E_MIPS_U64, E_MIPS_B8, E_MIPS_B16,
	    E_MIPS_B32, E_MIPS_B64, E_MIPS_F32, E_MIPS_F64,

	    E_NATIVE_CHAR, E_NATIVE_INT, E_NATIVE_FLOAT, E_NATIVE_SCHAR,
	    E_NATIVE_UCHAR, E_NATIVE_SHORT, E_NATIVE_USHORT, E_NATIVE_UINT,
	    E_NATIVE_LONG, E_NATIVE_ULONG, E_NATIVE_LLONG, E_NATIVE_ULLONG,
	    E_NATIVE_DOUBLE, E_NATIVE_LDOUBLE, E_NATIVE_B8, E_NATIVE_B16,
	    E_NATIVE_B32, E_NATIVE_B64, E_NATIVE_OPAQUE, E_NATIVE_HSIZE,
	    E_NATIVE_HSSIZE, E_NATIVE_HERR, E_NATIVE_HBOOL, E_NATIVE_INT8,
	    E_NATIVE_UINT8, E_NATIVE_INT_LEAST8, E_NATIVE_UINT_LEAST8,
	    E_NATIVE_INT_FAST8, E_NATIVE_UINT_FAST8, E_NATIVE_INT16,
	    E_NATIVE_UINT16, E_NATIVE_INT_LEAST16, E_NATIVE_UINT_LEAST16,
	    E_NATIVE_INT_FAST16, E_NATIVE_UINT_FAST16, E_NATIVE_INT32,
	    E_NATIVE_UINT32, E_NATIVE_INT_LEAST32, E_NATIVE_UINT_LEAST32,
	    E_NATIVE_INT_FAST32, E_NATIVE_UINT_FAST32, E_NATIVE_INT64,
	    E_NATIVE_UINT64, E_NATIVE_INT_LEAST64, E_NATIVE_UINT_LEAST64,
	    E_NATIVE_INT_FAST64, E_NATIVE_UINT_FAST64
	};

   protected:
#ifndef DOXYGEN_SHOULD_SKIP_THIS
	// Default constructor
	PredType();

	// Creates a pre-defined type using an HDF5 pre-defined constant
	PredType( const hid_t predtype_id );  // used by the library only

#endif // DOXYGEN_SHOULD_SKIP_THIS

};
#ifndef H5_NO_NAMESPACE
}
#endif
#endif
