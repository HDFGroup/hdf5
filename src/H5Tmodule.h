/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:	Quincey Koziol
 *		Saturday, September 12, 2015
 *
 * Purpose:	This file contains declarations which define macros for the
 *		H5T package.  Including this header means that the source file
 *		is part of the H5T package.
 */
#ifndef H5Tmodule_H
#define H5Tmodule_H

/* Define the proper control macros for the generic FUNC_ENTER/LEAVE and error
 *      reporting macros.
 */
#define H5T_MODULE
#define H5_MY_PKG      H5T
#define H5_MY_PKG_ERR  H5E_DATATYPE
#define H5_MY_PKG_INIT YES

/** \page H5T_UG HDF5 Datatypes
 *
 * \section sec_datatype HDF5 Datatypes
 * \subsection subsec_datatype_intro Introduction and Definitions
 * \subsection subsec_datatype_model Datatype Model
 * \subsubsection subsubsec_datatype_model_class Datatype Classes and Properties
 * \subsubsection subsubsec_datatype_model_predefine Predefined Datatypes
 * \subsection subsec_datatype_usage How Datatypes are Used
 * \subsubsection subsubsec_datatype_usage_object The Datatype Object and the HDF5 Datatype API
 * \subsubsection subsubsec_datatype_usage_create Dataset Creation
 * \subsubsection subsubsec_datatype_usage_transfer Data Transfer (Read and Write)
 * \subsubsection subsubsec_datatype_usage_discover Discovery of Data Format
 * \subsubsection subsubsec_datatype_usage_user Creating and Using User‐defined Datatypes
 * \subsection subsec_datatype_function Datatype Function Summaries
 * \subsection subsec_datatype_program Programming Model for Datatypes
 * \subsubsection subsubsec_datatype_program_discover Discovery of Datatype Properties
 * \subsubsection subsubsec_datatype_program_define Definition of Datatypes
 * \subsection subsec_datatype_other Other Non-numeric Datatypes
 * \subsubsection subsubsec_datatype_other_strings Strings
 * \subsubsection subsubsec_datatype_other_refs Reference
 * \subsubsection subsubsec_datatype_other_enum ENUM
 * \subsubsection subsubsec_datatype_other_opaque Opaque
 * \subsubsection subsubsec_datatype_other_bitfield Bitfield
 * \subsection subsec_datatype_fill Fill Values
 * \subsection subsec_datatype_complex Complex Combinations of Datatypes
 * \subsubsection subsubsec_datatype_complex_create Creating a Complicated Compound Datatype
 * \subsubsection subsubsec_datatype_complex_analyze Analyzing and Navigating a Compound Datatype
 * \subsection subsec_datatype_life Life Cycle of the Datatype Object
 * \subsection subsec_datatype_transfer Data Transfer: Datatype Conversion and Selection
 * \subsection subsec_datatype_text Text Descriptions of Datatypes: Conversion to and from
 *
 */

/**\defgroup H5T H5T Datatypes
 *
 * Use the functions in this module to manage HDF5 datatypes.
 *
 * HDF5 datatypes describe the element type of HDF5 datasets and attributes.
 * There's a large set of predefined datatypes, but users may find it useful
 * to define new datatypes through a process called \Emph{derivation}.
 *
 * The element type is automatically persisted as part of the HDF5 metadata of
 * attributes and datasets. Additionally, datatype definitions can be persisted
 * to HDF5 files and linked to groups as HDF5 datatype objects or so-called
 * \Emph{committed datatypes}.
 *
 * <table>
 * <tr><th>Create</th><th>Read</th></tr>
 * <tr valign="top">
 *   <td>
 *   \snippet{lineno} H5T_examples.c create
 *   </td>
 *   <td>
 *   \snippet{lineno} H5T_examples.c read
 *   </td>
 * <tr><th>Update</th><th>Delete</th></tr>
 * <tr valign="top">
 *   <td>
 *   \snippet{lineno} H5T_examples.c update
 *   </td>
 *   <td>
 *   \snippet{lineno} H5T_examples.c delete
 *   </td>
 * </tr>
 * </table>
 *
 * \defgroup ARRAY Array Datatypes
 * \ingroup H5T
 * \defgroup ATOM Atomic Datatypes
 * \ingroup H5T
 * \defgroup CONV Conversion Function
 * \ingroup H5T
 * \defgroup OPAQUE Opaque Datatypes
 * \ingroup H5T
 * \defgroup VLEN Variable-length Sequence Datatypes
 * \ingroup H5T
 *
 * \defgroup COMPENUM Compound and Enumeration Datatypes
 * \ingroup H5T
 * \defgroup COMPOUND Compound Datatypes
 * \ingroup COMPENUM
 * \defgroup ENUM Enumeration Datatypes
 * \ingroup COMPENUM
 *
 * \defgroup PDT Predefined Datatypes
 * \ingroup H5T
 * \details What is a predefined HDF5 datatype?
 * \todo Fill in the blanks!
 *
 * \defgroup PDTCPU By CPU
 * \ingroup PDT
 * \details CPU-specific datatypes
 * \defgroup PDTALPHA DEC Alpha
 * \ingroup PDTCPU
 * \defgroup PDTX86 AMD & INTEL
 * \ingroup PDTCPU
 * \defgroup PDTMIPS SGI MIPS
 * \ingroup PDTCPU
 *
 * \defgroup PDTIEEE IEEE
 * \ingroup PDT
 * \details The IEEE floating point types in big- and little-endian byte orders.
 *
 * \defgroup PDTSTD Standard Datatypes
 * \ingroup PDT
 * \details These are "standard" types. For instance, signed (2's complement)
 *          and unsigned integers of various sizes in big- and little-endian
 *          byte orders.
 *
 * \defgroup PDTUNIX UNIX-specific Datatypes
 * \ingroup PDT
 * \details Types which are particular to Unix.
 * \todo Fill in the blanks!
 *
 * \defgroup PDTNAT Native Datatypes
 * \ingroup PDT
 * \details These are the datatypes detected during library \Emph{compilation}
 *          by \c H5detect(). Their names differ from other HDF5 datatype names
 *          as follows:
 *          \li Instead of a class name, precision and byte order as the last
 *              component, they have a C-like type name.
 *          \li If the type begins with \c U then it is the unsigned version of
 *              the integer type; other integer types are signed.
 *          \li The datatype \c LLONG corresponds C's \Code{long long} and
 *              \c LDOUBLE is \Code{long double}. These types might be the same
 *              as \c LONG and \c DOUBLE, respectively.
 * \defgroup PDTC9x C9x Integer Datatypes
 * \ingroup PDTNAT
 * \details C9x integer types
 * \todo Fill in the blanks!
 *
 * \defgroup PDTS Strings
 * \ingroup PDT
 *
 */

#endif /* H5Tmodule_H */
