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
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5PropList.h"
#include "H5Object.h"
#include "H5DcreatProp.h"
#include "H5CommonFG.h"
#include "H5DataType.h"
#include "H5AtomType.h"
#include "H5Library.h"
#include "H5PredType.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

#ifndef DOXYGEN_SHOULD_SKIP_THIS
//--------------------------------------------------------------------------
// Function:	PredType overloaded constructor
///\brief	Creates a PredType object using the id of an existing 
///		predefined datatype.
///\param	predtype_id - IN: Id of a predefined datatype
// Description
// 		This constructor creates a predefined datatype, so it sets 
// 		DataType::is_predtype to true.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
PredType::PredType( const hid_t predtype_id ) : AtomType( predtype_id )
{ 
   is_predtype = true; 
}

//--------------------------------------------------------------------------
// Function:	PredType default constructor
///\brief	Default constructor: Creates a stub predefined datatype
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
PredType::PredType() : AtomType() {}

//--------------------------------------------------------------------------
// Function:	PredType copy constructor
///\brief	Copy constructor: makes a copy of the original PredType object.
///\param	original - IN: PredType instance to copy
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
PredType::PredType( const PredType& original ) : AtomType( original ) {}

const PredType PredType::NotAtexit;	// only for atexit/global dest. problem

// Definition of pre-defined types
// Note: the parameter E_xxxx_yyyy is the enum value that represents 
// the corresponding HDF5 predefined type H5T_xxxx_yyyy.  This enum value
// is stored in PredType::id since it is a PredType object id, in a way.
// In addition, this storage makes things safely simpler.  When a PredType
// object is used, the virtual PredType::getId will base on the enum
// value to return the correct HDF5 predefined type id.

const PredType PredType::C_S1( E_C_S1 );
const PredType PredType::FORTRAN_S1( E_FORTRAN_S1 );

const PredType PredType::STD_I8BE( E_STD_I8BE );
const PredType PredType::STD_I8LE( E_STD_I8LE );
const PredType PredType::STD_I16BE( E_STD_I16BE );
const PredType PredType::STD_I16LE( E_STD_I16LE );
const PredType PredType::STD_I32BE( E_STD_I32BE );
const PredType PredType::STD_I32LE( E_STD_I32LE );
const PredType PredType::STD_I64BE( E_STD_I64BE );
const PredType PredType::STD_I64LE( E_STD_I64LE );
const PredType PredType::STD_U8BE( E_STD_U8BE );
const PredType PredType::STD_U8LE( E_STD_U8LE );
const PredType PredType::STD_U16BE( E_STD_U16BE );
const PredType PredType::STD_U16LE( E_STD_U16LE );
const PredType PredType::STD_U32BE( E_STD_U32BE );
const PredType PredType::STD_U32LE( E_STD_U32LE );
const PredType PredType::STD_U64BE( E_STD_U64BE );
const PredType PredType::STD_U64LE( E_STD_U64LE );
const PredType PredType::STD_B8BE( E_STD_B8BE );
const PredType PredType::STD_B8LE( E_STD_B8LE );

const PredType PredType::STD_B16BE( E_STD_B16BE );
const PredType PredType::STD_B16LE( E_STD_B16LE );
const PredType PredType::STD_B32BE( E_STD_B32BE );
const PredType PredType::STD_B32LE( E_STD_B32LE );
const PredType PredType::STD_B64BE( E_STD_B64BE );
const PredType PredType::STD_B64LE( E_STD_B64LE );
const PredType PredType::STD_REF_OBJ( E_STD_REF_OBJ );
const PredType PredType::STD_REF_DSETREG( E_STD_REF_DSETREG );

const PredType PredType::IEEE_F32BE( E_IEEE_F32BE );
const PredType PredType::IEEE_F32LE( E_IEEE_F32LE );
const PredType PredType::IEEE_F64BE( E_IEEE_F64BE );
const PredType PredType::IEEE_F64LE( E_IEEE_F64LE );

const PredType PredType::UNIX_D32BE( E_UNIX_D32BE );
const PredType PredType::UNIX_D32LE( E_UNIX_D32LE );
const PredType PredType::UNIX_D64BE( E_UNIX_D64BE );
const PredType PredType::UNIX_D64LE( E_UNIX_D64LE );

const PredType PredType::INTEL_I8( E_INTEL_I8 );
const PredType PredType::INTEL_I16( E_INTEL_I16 );
const PredType PredType::INTEL_I32( E_INTEL_I32 );
const PredType PredType::INTEL_I64( E_INTEL_I64 );
const PredType PredType::INTEL_U8( E_INTEL_U8 );
const PredType PredType::INTEL_U16( E_INTEL_U16 );
const PredType PredType::INTEL_U32( E_INTEL_U32 );
const PredType PredType::INTEL_U64( E_INTEL_U64 );
const PredType PredType::INTEL_B8( E_INTEL_B8 );
const PredType PredType::INTEL_B16( E_INTEL_B16 );
const PredType PredType::INTEL_B32( E_INTEL_B32 );
const PredType PredType::INTEL_B64( E_INTEL_B64 );
const PredType PredType::INTEL_F32( E_INTEL_F32 );
const PredType PredType::INTEL_F64( E_INTEL_F64 );

const PredType PredType::ALPHA_I8( E_ALPHA_I8 );
const PredType PredType::ALPHA_I16( E_ALPHA_I16 );
const PredType PredType::ALPHA_I32( E_ALPHA_I32 );
const PredType PredType::ALPHA_I64( E_ALPHA_I64 );
const PredType PredType::ALPHA_U8( E_ALPHA_U8 );
const PredType PredType::ALPHA_U16( E_ALPHA_U16 );
const PredType PredType::ALPHA_U32( E_ALPHA_U32 );
const PredType PredType::ALPHA_U64( E_ALPHA_U64 );
const PredType PredType::ALPHA_B8( E_ALPHA_B8 );
const PredType PredType::ALPHA_B16( E_ALPHA_B16 );
const PredType PredType::ALPHA_B32( E_ALPHA_B32 );
const PredType PredType::ALPHA_B64( E_ALPHA_B64 );
const PredType PredType::ALPHA_F32( E_ALPHA_F32 );
const PredType PredType::ALPHA_F64( E_ALPHA_F64 );

const PredType PredType::MIPS_I8( E_MIPS_I8 );
const PredType PredType::MIPS_I16( E_MIPS_I16 );
const PredType PredType::MIPS_I32( E_MIPS_I32 );
const PredType PredType::MIPS_I64( E_MIPS_I64 );
const PredType PredType::MIPS_U8( E_MIPS_U8 );
const PredType PredType::MIPS_U16( E_MIPS_U16 );
const PredType PredType::MIPS_U32( E_MIPS_U32 );
const PredType PredType::MIPS_U64( E_MIPS_U64 );
const PredType PredType::MIPS_B8( E_MIPS_B8 );
const PredType PredType::MIPS_B16( E_MIPS_B16 );
const PredType PredType::MIPS_B32( E_MIPS_B32 );
const PredType PredType::MIPS_B64( E_MIPS_B64 );
const PredType PredType::MIPS_F32( E_MIPS_F32 );
const PredType PredType::MIPS_F64( E_MIPS_F64 );

const PredType PredType::NATIVE_CHAR( E_NATIVE_CHAR );
const PredType PredType::NATIVE_INT( E_NATIVE_INT );
const PredType PredType::NATIVE_FLOAT( E_NATIVE_FLOAT );
const PredType PredType::NATIVE_SCHAR( E_NATIVE_SCHAR );
const PredType PredType::NATIVE_UCHAR( E_NATIVE_UCHAR );
const PredType PredType::NATIVE_SHORT( E_NATIVE_SHORT );
const PredType PredType::NATIVE_USHORT( E_NATIVE_USHORT );
const PredType PredType::NATIVE_UINT( E_NATIVE_UINT );
const PredType PredType::NATIVE_LONG( E_NATIVE_LONG );
const PredType PredType::NATIVE_ULONG( E_NATIVE_ULONG );
const PredType PredType::NATIVE_LLONG( E_NATIVE_LLONG );
const PredType PredType::NATIVE_ULLONG( E_NATIVE_ULLONG );
const PredType PredType::NATIVE_DOUBLE( E_NATIVE_DOUBLE );
#if H5_SIZEOF_LONG_DOUBLE !=0
const PredType PredType::NATIVE_LDOUBLE( E_NATIVE_LDOUBLE );
#endif
const PredType PredType::NATIVE_B8( E_NATIVE_B8 );
const PredType PredType::NATIVE_B16( E_NATIVE_B16 );
const PredType PredType::NATIVE_B32( E_NATIVE_B32 );
const PredType PredType::NATIVE_B64( E_NATIVE_B64 );
const PredType PredType::NATIVE_OPAQUE( E_NATIVE_OPAQUE );
const PredType PredType::NATIVE_HSIZE( E_NATIVE_HSIZE );
const PredType PredType::NATIVE_HSSIZE( E_NATIVE_HSSIZE );
const PredType PredType::NATIVE_HERR( E_NATIVE_HERR );
const PredType PredType::NATIVE_HBOOL( E_NATIVE_HBOOL );

const PredType PredType::NATIVE_INT8( E_NATIVE_INT8 );
const PredType PredType::NATIVE_UINT8( E_NATIVE_UINT8 );
const PredType PredType::NATIVE_INT_LEAST8( E_NATIVE_INT_LEAST8 );
const PredType PredType::NATIVE_UINT_LEAST8( E_NATIVE_UINT_LEAST8 );
const PredType PredType::NATIVE_INT_FAST8( E_NATIVE_INT_FAST8 );
const PredType PredType::NATIVE_UINT_FAST8( E_NATIVE_UINT_FAST8 );

const PredType PredType::NATIVE_INT16( E_NATIVE_INT16 );
const PredType PredType::NATIVE_UINT16( E_NATIVE_UINT16 );
const PredType PredType::NATIVE_INT_LEAST16( E_NATIVE_INT_LEAST16 );
const PredType PredType::NATIVE_UINT_LEAST16( E_NATIVE_UINT_LEAST16 );
const PredType PredType::NATIVE_INT_FAST16( E_NATIVE_INT_FAST16 );
const PredType PredType::NATIVE_UINT_FAST16( E_NATIVE_UINT_FAST16 );

const PredType PredType::NATIVE_INT32( E_NATIVE_INT32 );
const PredType PredType::NATIVE_UINT32( E_NATIVE_UINT32 );
const PredType PredType::NATIVE_INT_LEAST32( E_NATIVE_INT_LEAST32 );
const PredType PredType::NATIVE_UINT_LEAST32( E_NATIVE_UINT_LEAST32 );
const PredType PredType::NATIVE_INT_FAST32( E_NATIVE_INT_FAST32 );
const PredType PredType::NATIVE_UINT_FAST32( E_NATIVE_UINT_FAST32 );

const PredType PredType::NATIVE_INT64( E_NATIVE_INT64 );
const PredType PredType::NATIVE_UINT64( E_NATIVE_UINT64 );
const PredType PredType::NATIVE_INT_LEAST64( E_NATIVE_INT_LEAST64 );
const PredType PredType::NATIVE_UINT_LEAST64( E_NATIVE_UINT_LEAST64 );
const PredType PredType::NATIVE_INT_FAST64( E_NATIVE_INT_FAST64 );
const PredType PredType::NATIVE_UINT_FAST64( E_NATIVE_UINT_FAST64 );
#endif // DOXYGEN_SHOULD_SKIP_THIS

//--------------------------------------------------------------------------
// Function:	PredType::operator=
///\brief	Assignment operator.
///\param	rhs - IN: Reference to the predefined datatype
///\return	Reference to PredType instance
///\exception	H5::DataTypeIException
// Description
//		Makes a copy of the type on the right hand side and stores
//		the new id in the left hand side object.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
PredType& PredType::operator=( const PredType& rhs )
{
   copy(rhs);
   return(*this);
}

//--------------------------------------------------------------------------
// Function:	PredType::getId
///\brief	Returns the HDF5 predefined type id.
///\return	HDF5 predefined type id or INVALID
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
hid_t PredType::getId() const
{
    switch( id ) {
	case E_C_S1:
	    return( H5T_C_S1 );
	case E_FORTRAN_S1:
	    return( H5T_FORTRAN_S1 );

	case E_STD_I8BE:
	    return( H5T_STD_I8BE );
	case E_STD_I8LE:
	    return( H5T_STD_I8LE );
	case E_STD_I16BE:
	    return( H5T_STD_I16BE );
	case E_STD_I16LE:
	    return( H5T_STD_I16LE );
	case E_STD_I32BE:
	    return( H5T_STD_I32BE );
	case E_STD_I32LE:
	    return( H5T_STD_I32LE );
	case E_STD_I64BE:
	    return( H5T_STD_I64BE );
	case E_STD_I64LE:
	    return( H5T_STD_I64LE );
	case E_STD_U8BE:
	    return( H5T_STD_U8BE );
	case E_STD_U8LE:
	    return( H5T_STD_U8LE );
	case E_STD_U16BE:
	    return( H5T_STD_U16BE );
	case E_STD_U16LE:
	    return( H5T_STD_U16LE );
	case E_STD_U32BE:
	    return( H5T_STD_U32BE );
	case E_STD_U32LE:
	    return( H5T_STD_U32LE );
	case E_STD_U64BE:
	    return( H5T_STD_U64BE );
	case E_STD_U64LE:
	    return( H5T_STD_U64LE );
	case E_STD_B8BE:
	    return( H5T_STD_B8BE );
	case E_STD_B8LE:
	    return( H5T_STD_B8LE );

	case E_STD_B16BE:
	    return( H5T_STD_B16BE );
	case E_STD_B16LE:
	    return( H5T_STD_B16LE );
	case E_STD_B32BE:
	    return( H5T_STD_B32BE );
	case E_STD_B32LE:
	    return( H5T_STD_B32LE );
	case E_STD_B64BE:
	    return( H5T_STD_B64BE );
	case E_STD_B64LE:
	    return( H5T_STD_B64LE );
	case E_STD_REF_OBJ:
	    return( H5T_STD_REF_OBJ );
	case E_STD_REF_DSETREG:
	    return( H5T_STD_REF_DSETREG );

	case E_IEEE_F32BE:
	    return( H5T_IEEE_F32BE );
	case E_IEEE_F32LE:
	    return( H5T_IEEE_F32LE );
	case E_IEEE_F64BE:
	    return( H5T_IEEE_F64BE );
	case E_IEEE_F64LE:
	    return( H5T_IEEE_F64LE );

	case E_UNIX_D32BE:
	    return( H5T_UNIX_D32BE );
	case E_UNIX_D32LE:
	    return( H5T_UNIX_D32LE );
	case E_UNIX_D64BE:
	    return( H5T_UNIX_D64BE );
	case E_UNIX_D64LE:
	    return( H5T_UNIX_D64LE );

	case E_INTEL_I8:
	    return( H5T_INTEL_I8 );
	case E_INTEL_I16:
	    return( H5T_INTEL_I16 );
	case E_INTEL_I32:
	    return( H5T_INTEL_I32 );
	case E_INTEL_I64:
	    return( H5T_INTEL_I64 );
	case E_INTEL_U8:
	    return( H5T_INTEL_U8 );
	case E_INTEL_U16:
	    return( H5T_INTEL_U16 );
	case E_INTEL_U32:
	    return( H5T_INTEL_U32 );
	case E_INTEL_U64:
	    return( H5T_INTEL_U64 );
	case E_INTEL_B8:
	    return( H5T_INTEL_B8 );
	case E_INTEL_B16:
	    return( H5T_INTEL_B16 );
	case E_INTEL_B32:
	    return( H5T_INTEL_B32 );
	case E_INTEL_B64:
	    return( H5T_INTEL_B64 );
	case E_INTEL_F32:
	    return( H5T_INTEL_F32 );
	case E_INTEL_F64:
	    return( H5T_INTEL_F64 );

	case E_ALPHA_I8:
	    return( H5T_ALPHA_I8 );
	case E_ALPHA_I16:
	    return( H5T_ALPHA_I16 );
	case E_ALPHA_I32:
	    return( H5T_ALPHA_I32 );
	case E_ALPHA_I64:
	    return( H5T_ALPHA_I64 );
	case E_ALPHA_U8:
	    return( H5T_ALPHA_U8 );
	case E_ALPHA_U16:
	    return( H5T_ALPHA_U16 );
	case E_ALPHA_U32:
	    return( H5T_ALPHA_U32 );
	case E_ALPHA_U64:
	    return( H5T_ALPHA_U64 );
	case E_ALPHA_B8:
	    return( H5T_ALPHA_B8 );
	case E_ALPHA_B16:
	    return( H5T_ALPHA_B16 );
	case E_ALPHA_B32:
	    return( H5T_ALPHA_B32 );
	case E_ALPHA_B64:
	    return( H5T_ALPHA_B64 );
	case E_ALPHA_F32:
	    return( H5T_ALPHA_F32 );
	case E_ALPHA_F64:
	    return( H5T_ALPHA_F64 );

	case E_MIPS_I8:
	    return( H5T_MIPS_I8 );
	case E_MIPS_I16:
	    return( H5T_MIPS_I16 );
	case E_MIPS_I32:
	    return( H5T_MIPS_I32 );
	case E_MIPS_I64:
	    return( H5T_MIPS_I64 );
	case E_MIPS_U8:
	    return( H5T_MIPS_U8 );
	case E_MIPS_U16:
	    return( H5T_MIPS_U16 );
	case E_MIPS_U32:
	    return( H5T_MIPS_U32 );
	case E_MIPS_U64:
	    return( H5T_MIPS_U64 );
	case E_MIPS_B8:
	    return( H5T_MIPS_B8 );
	case E_MIPS_B16:
	    return( H5T_MIPS_B16 );
	case E_MIPS_B32:
	    return( H5T_MIPS_B32 );
	case E_MIPS_B64:
	    return( H5T_MIPS_B64 );
	case E_MIPS_F32:
	    return( H5T_MIPS_F32 );
	case E_MIPS_F64:
	    return( H5T_MIPS_F64 );

	case E_NATIVE_CHAR:
	    return( H5T_NATIVE_CHAR );
	case E_NATIVE_INT:
	    return( H5T_NATIVE_INT );
	case E_NATIVE_FLOAT:
	    return( H5T_NATIVE_FLOAT );
	case E_NATIVE_SCHAR:
	    return( H5T_NATIVE_SCHAR );
	case E_NATIVE_UCHAR:
	    return( H5T_NATIVE_UCHAR );
	case E_NATIVE_SHORT:
	    return( H5T_NATIVE_SHORT );
	case E_NATIVE_USHORT:
	    return( H5T_NATIVE_USHORT );
	case E_NATIVE_UINT:
	    return( H5T_NATIVE_UINT );
	case E_NATIVE_LONG:
	    return( H5T_NATIVE_LONG );
	case E_NATIVE_ULONG:
	    return( H5T_NATIVE_ULONG );
	case E_NATIVE_LLONG:
	    return( H5T_NATIVE_LLONG );
	case E_NATIVE_ULLONG:
	    return( H5T_NATIVE_ULLONG );
	case E_NATIVE_DOUBLE:
	    return( H5T_NATIVE_DOUBLE );
#if H5_SIZEOF_LONG_DOUBLE !=0
	case E_NATIVE_LDOUBLE:
	    return( H5T_NATIVE_LDOUBLE );
#endif
	case E_NATIVE_B8:
	    return( H5T_NATIVE_B8 );
	case E_NATIVE_B16:
	    return( H5T_NATIVE_B16 );
	case E_NATIVE_B32:
	    return( H5T_NATIVE_B32 );
	case E_NATIVE_B64:
	    return( H5T_NATIVE_B64 );
	case E_NATIVE_OPAQUE:
	    return( H5T_NATIVE_OPAQUE );
	case E_NATIVE_HSIZE:
	    return( H5T_NATIVE_HSIZE );
	case E_NATIVE_HSSIZE:
	    return( H5T_NATIVE_HSSIZE );
	case E_NATIVE_HERR:
	    return( H5T_NATIVE_HERR );
	case E_NATIVE_HBOOL:
	    return( H5T_NATIVE_HBOOL );

	case E_NATIVE_INT8:
	    return( H5T_NATIVE_INT8 );
	case E_NATIVE_UINT8:
	    return( H5T_NATIVE_UINT8 );
	case E_NATIVE_INT_LEAST8:
	    return( H5T_NATIVE_INT_LEAST8 );
	case E_NATIVE_UINT_LEAST8:
	    return( H5T_NATIVE_UINT_LEAST8 );
	case E_NATIVE_INT_FAST8:
	    return( H5T_NATIVE_INT_FAST8 );
	case E_NATIVE_UINT_FAST8:
	    return( H5T_NATIVE_UINT_FAST8 );

	case E_NATIVE_INT16:
	    return( H5T_NATIVE_INT16 );
	case E_NATIVE_UINT16:
	    return( H5T_NATIVE_UINT16 );
	case E_NATIVE_INT_LEAST16:
	    return( H5T_NATIVE_INT_LEAST16 );
	case E_NATIVE_UINT_LEAST16:
	    return( H5T_NATIVE_UINT_LEAST16 );
	case E_NATIVE_INT_FAST16:
	    return( H5T_NATIVE_INT_FAST16 );
	case E_NATIVE_UINT_FAST16:
	    return( H5T_NATIVE_UINT_FAST16 );

	case E_NATIVE_INT32:
	    return( H5T_NATIVE_INT32 );
	case E_NATIVE_UINT32:
	    return( H5T_NATIVE_UINT32 );
	case E_NATIVE_INT_LEAST32:
	    return( H5T_NATIVE_INT_LEAST32 );
	case E_NATIVE_UINT_LEAST32:
	    return( H5T_NATIVE_UINT_LEAST32 );
	case E_NATIVE_INT_FAST32:
	    return( H5T_NATIVE_INT_FAST32 );
	case E_NATIVE_UINT_FAST32:
	    return( H5T_NATIVE_UINT_FAST32 );

	case E_NATIVE_INT64:
	    return( H5T_NATIVE_INT64 );
	case E_NATIVE_UINT64:
	    return( H5T_NATIVE_UINT64 );
	case E_NATIVE_INT_LEAST64:
	    return( H5T_NATIVE_INT_LEAST64 );
	case E_NATIVE_UINT_LEAST64:
	    return( H5T_NATIVE_UINT_LEAST64 );
	case E_NATIVE_INT_FAST64:
	    return( H5T_NATIVE_INT_FAST64 );
	case E_NATIVE_UINT_FAST64:
	    return( H5T_NATIVE_UINT_FAST64 );

	default:
	    return( INVALID );
    }   // end switch
}   // end of getId()

#ifndef DOXYGEN_SHOULD_SKIP_THIS
// These dummy functions do not inherit from DataType - they'll
// throw an DataTypeIException if invoked.
void PredType::commit( H5Object& loc, const char* name )
{
   throw DataTypeIException("PredType::commit", "Attempting to commit a predefined datatype.  This operation is invalid" );
}  

void PredType::commit( H5Object& loc, const string& name )
{
   commit( loc, name.c_str());
}  

bool PredType::committed()
{
   throw DataTypeIException("PredType::committed", "Error: Attempting to check for commit status on a predefined datatype." );
   return (0);
}  
#endif // DOXYGEN_SHOULD_SKIP_THIS

// Default destructor
//--------------------------------------------------------------------------
// Function:	PredType destructor
///\brief	Noop destructor.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
PredType::~PredType() {}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
