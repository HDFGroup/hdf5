#include <string>

#include "H5Include.h"
#include "H5RefCounter.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5PropList.h"
#include "H5Object.h"
#include "H5DataType.h"
#include "H5AtomType.h"
#include "H5Library.h"
#include "H5PredType.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

// Default constructor 
PredType::PredType() : AtomType() {}

// creates predefined datatype, so set DataType::is_predtype to true by default
PredType::PredType( const hid_t predtype_id ) : AtomType( predtype_id )
{ 
   is_predtype = true; 
}

// Copy constructor: makes a copy of this PredType object.
PredType::PredType( const PredType& original ) : AtomType( original ) {}

const PredType PredType::NotAtexit;	// only for atexit/global dest. problem

// Definition of pre-defined types
const PredType PredType::C_S1( H5T_C_S1 );
const PredType PredType::FORTRAN_S1( H5T_FORTRAN_S1 );

const PredType PredType::STD_I8BE( H5T_STD_I8BE );
const PredType PredType::STD_I8LE( H5T_STD_I8LE );
const PredType PredType::STD_I16BE( H5T_STD_I16BE );
const PredType PredType::STD_I16LE( H5T_STD_I16LE );
const PredType PredType::STD_I32BE( H5T_STD_I32BE );
const PredType PredType::STD_I32LE( H5T_STD_I32LE );
const PredType PredType::STD_I64BE( H5T_STD_I64BE );
const PredType PredType::STD_I64LE( H5T_STD_I64LE );
const PredType PredType::STD_U8BE( H5T_STD_U8BE );
const PredType PredType::STD_U8LE( H5T_STD_U8LE );
const PredType PredType::STD_U16BE( H5T_STD_U16BE );
const PredType PredType::STD_U16LE( H5T_STD_U16LE );
const PredType PredType::STD_U32BE( H5T_STD_U32BE );
const PredType PredType::STD_U32LE( H5T_STD_U32LE );
const PredType PredType::STD_U64BE( H5T_STD_U64BE );
const PredType PredType::STD_U64LE( H5T_STD_U64LE );
const PredType PredType::STD_B8BE( H5T_STD_B8BE );
const PredType PredType::STD_B8LE( H5T_STD_B8LE );
const PredType PredType::STD_B16BE( H5T_STD_B16BE );
const PredType PredType::STD_B16LE( H5T_STD_B16LE );
const PredType PredType::STD_B32BE( H5T_STD_B32BE );
const PredType PredType::STD_B32LE( H5T_STD_B32LE );
const PredType PredType::STD_B64BE( H5T_STD_B64BE );
const PredType PredType::STD_B64LE( H5T_STD_B64LE );
const PredType PredType::STD_REF_OBJ( H5T_STD_REF_OBJ );
const PredType PredType::STD_REF_DSETREG( H5T_STD_REF_DSETREG );

const PredType PredType::IEEE_F32BE( H5T_IEEE_F32BE );
const PredType PredType::IEEE_F32LE( H5T_IEEE_F32LE );
const PredType PredType::IEEE_F64BE( H5T_IEEE_F64BE );
const PredType PredType::IEEE_F64LE( H5T_IEEE_F64LE );

const PredType PredType::UNIX_D32BE( H5T_UNIX_D32BE );
const PredType PredType::UNIX_D32LE( H5T_UNIX_D32LE );
const PredType PredType::UNIX_D64BE( H5T_UNIX_D64BE );
const PredType PredType::UNIX_D64LE( H5T_UNIX_D64LE );

const PredType PredType::INTEL_I8( H5T_INTEL_I8 );
const PredType PredType::INTEL_I16( H5T_INTEL_I16 );
const PredType PredType::INTEL_I32( H5T_INTEL_I32 );
const PredType PredType::INTEL_I64( H5T_INTEL_I64 );
const PredType PredType::INTEL_U8( H5T_INTEL_U8 );
const PredType PredType::INTEL_U16( H5T_INTEL_U16 );
const PredType PredType::INTEL_U32( H5T_INTEL_U32 );
const PredType PredType::INTEL_U64( H5T_INTEL_U64 );
const PredType PredType::INTEL_B8( H5T_INTEL_B8 );
const PredType PredType::INTEL_B16( H5T_INTEL_B16 );
const PredType PredType::INTEL_B32( H5T_INTEL_B32 );
const PredType PredType::INTEL_B64( H5T_INTEL_B64 );
const PredType PredType::INTEL_F32( H5T_INTEL_F32 );
const PredType PredType::INTEL_F64( H5T_INTEL_F64 );

const PredType PredType::ALPHA_I8( H5T_ALPHA_I8 );
const PredType PredType::ALPHA_I16( H5T_ALPHA_I16 );
const PredType PredType::ALPHA_I32( H5T_ALPHA_I32 );
const PredType PredType::ALPHA_I64( H5T_ALPHA_I64 );
const PredType PredType::ALPHA_U8( H5T_ALPHA_U8 );
const PredType PredType::ALPHA_U16( H5T_ALPHA_U16 );
const PredType PredType::ALPHA_U32( H5T_ALPHA_U32 );
const PredType PredType::ALPHA_U64( H5T_ALPHA_U64 );
const PredType PredType::ALPHA_B8( H5T_ALPHA_B8 );
const PredType PredType::ALPHA_B16( H5T_ALPHA_B16 );
const PredType PredType::ALPHA_B32( H5T_ALPHA_B32 );
const PredType PredType::ALPHA_B64( H5T_ALPHA_B64 );
const PredType PredType::ALPHA_F32( H5T_ALPHA_F32 );
const PredType PredType::ALPHA_F64( H5T_ALPHA_F64 );

const PredType PredType::MIPS_I8( H5T_MIPS_I8 );
const PredType PredType::MIPS_I16( H5T_MIPS_I16 );
const PredType PredType::MIPS_I32( H5T_MIPS_I32 );
const PredType PredType::MIPS_I64( H5T_MIPS_I64 );
const PredType PredType::MIPS_U8( H5T_MIPS_U8 );
const PredType PredType::MIPS_U16( H5T_MIPS_U16 );
const PredType PredType::MIPS_U32( H5T_MIPS_U32 );
const PredType PredType::MIPS_U64( H5T_MIPS_U64 );
const PredType PredType::MIPS_B8( H5T_MIPS_B8 );
const PredType PredType::MIPS_B16( H5T_MIPS_B16 );
const PredType PredType::MIPS_B32( H5T_MIPS_B32 );
const PredType PredType::MIPS_B64( H5T_MIPS_B64 );
const PredType PredType::MIPS_F32( H5T_MIPS_F32 );
const PredType PredType::MIPS_F64( H5T_MIPS_F64 );

const PredType PredType::NATIVE_CHAR( H5T_NATIVE_CHAR );
const PredType PredType::NATIVE_INT( H5T_NATIVE_INT );
const PredType PredType::NATIVE_FLOAT( H5T_NATIVE_FLOAT );
const PredType PredType::NATIVE_SCHAR( H5T_NATIVE_SCHAR );
const PredType PredType::NATIVE_UCHAR( H5T_NATIVE_UCHAR );
const PredType PredType::NATIVE_SHORT( H5T_NATIVE_SHORT );
const PredType PredType::NATIVE_USHORT( H5T_NATIVE_USHORT );
const PredType PredType::NATIVE_UINT( H5T_NATIVE_UINT );
const PredType PredType::NATIVE_LONG( H5T_NATIVE_LONG );
const PredType PredType::NATIVE_ULONG( H5T_NATIVE_ULONG );
const PredType PredType::NATIVE_LLONG( H5T_NATIVE_LLONG );
const PredType PredType::NATIVE_ULLONG( H5T_NATIVE_ULLONG );
const PredType PredType::NATIVE_DOUBLE( H5T_NATIVE_DOUBLE );
const PredType PredType::NATIVE_LDOUBLE( H5T_NATIVE_LDOUBLE );
const PredType PredType::NATIVE_B8( H5T_NATIVE_B8 );
const PredType PredType::NATIVE_B16( H5T_NATIVE_B16 );
const PredType PredType::NATIVE_B32( H5T_NATIVE_B32 );
const PredType PredType::NATIVE_B64( H5T_NATIVE_B64 );
const PredType PredType::NATIVE_OPAQUE( H5T_NATIVE_OPAQUE );
const PredType PredType::NATIVE_HSIZE( H5T_NATIVE_HSIZE );
const PredType PredType::NATIVE_HSSIZE( H5T_NATIVE_HSSIZE );
const PredType PredType::NATIVE_HERR( H5T_NATIVE_HERR );
const PredType PredType::NATIVE_HBOOL( H5T_NATIVE_HBOOL );

const PredType PredType::NATIVE_INT8( H5T_NATIVE_INT8 );
const PredType PredType::NATIVE_UINT8( H5T_NATIVE_UINT8 );
const PredType PredType::NATIVE_INT_LEAST8( H5T_NATIVE_INT_LEAST8 );
const PredType PredType::NATIVE_UINT_LEAST8( H5T_NATIVE_UINT_LEAST8 );
const PredType PredType::NATIVE_INT_FAST8( H5T_NATIVE_INT_FAST8 );
const PredType PredType::NATIVE_UINT_FAST8( H5T_NATIVE_UINT_FAST8 );

const PredType PredType::NATIVE_INT16( H5T_NATIVE_INT16 );
const PredType PredType::NATIVE_UINT16( H5T_NATIVE_UINT16 );
const PredType PredType::NATIVE_INT_LEAST16( H5T_NATIVE_INT_LEAST16 );
const PredType PredType::NATIVE_UINT_LEAST16( H5T_NATIVE_UINT_LEAST16 );
const PredType PredType::NATIVE_INT_FAST16( H5T_NATIVE_INT_FAST16 );
const PredType PredType::NATIVE_UINT_FAST16( H5T_NATIVE_UINT_FAST16 );

const PredType PredType::NATIVE_INT32( H5T_NATIVE_INT32 );
const PredType PredType::NATIVE_UINT32( H5T_NATIVE_UINT32 );
const PredType PredType::NATIVE_INT_LEAST32( H5T_NATIVE_INT_LEAST32 );
const PredType PredType::NATIVE_UINT_LEAST32( H5T_NATIVE_UINT_LEAST32 );
const PredType PredType::NATIVE_INT_FAST32( H5T_NATIVE_INT_FAST32 );
const PredType PredType::NATIVE_UINT_FAST32( H5T_NATIVE_UINT_FAST32 );

const PredType PredType::NATIVE_INT64( H5T_NATIVE_INT64 );
const PredType PredType::NATIVE_UINT64( H5T_NATIVE_UINT64 );
const PredType PredType::NATIVE_INT_LEAST64( H5T_NATIVE_INT_LEAST64 );
const PredType PredType::NATIVE_UINT_LEAST64( H5T_NATIVE_UINT_LEAST64 );
const PredType PredType::NATIVE_INT_FAST64( H5T_NATIVE_INT_FAST64 );
const PredType PredType::NATIVE_UINT_FAST64( H5T_NATIVE_UINT_FAST64 );

// These dummy functions do not inherit from DataType - they'll
// throw an DataTypeIException if invoked.
void PredType::commit( H5Object& loc, const char* name )
{
   throw DataTypeIException( "Attempting to commit a predefined datatype.  This operation is invalid" );
}  

void PredType::commit( H5Object& loc, const string& name )
{
   commit( loc, name.c_str());
}  

bool PredType::committed()
{
   throw DataTypeIException( "Error: Attempting to check for commit status on a predefined datatype." );
   return (-1);
}  

// Default destructor
PredType::~PredType() {}

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
