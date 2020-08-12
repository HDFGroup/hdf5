/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:	Quincey Koziol <koziol@hdfgroup.org>
 *		Saturday, September 12, 2015
 *
 * Purpose:	This file contains declarations which define macros for the
 *		H5T package.  Including this header means that the source file
 *		is part of the H5T package.
 */
#ifndef _H5Tmodule_H
#define _H5Tmodule_H

/* Define the proper control macros for the generic FUNC_ENTER/LEAVE and error
 *      reporting macros.
 */
#define H5T_MODULE
#define H5_MY_PKG       H5T
#define H5_MY_PKG_ERR   H5E_DATATYPE
#define H5_MY_PKG_INIT  YES

/**\defgroup H5T H5T
 * \brief Datatype Interface
 * \todo Describe concisely what the functions in this module are about.
 *
 * \defgroup ARRAY Array Datatypes
 * \ingroup H5T
 * \defgroup ATOM Atomic Datatypes
 * \ingroup H5T
 * \defgroup COMPOUND Compound Datatypes
 * \ingroup H5T
 * \defgroup CONV Conversion Function
 * \ingroup H5T
 * \defgroup ENUM Enumeration Datatypes
 * \ingroup H5T
 * \defgroup GTO General Datatype Operations
 * \ingroup H5T
 * \defgroup OPAQUE Opaque Datatypes
 * \ingroup H5T
 * \defgroup PDT Predefined Datatypes
 * \ingroup H5T
 * \defgroup PDTB Bitfields
 * \ingroup PDT
 * \defgroup PDTF Floating-point Numbers
 * \ingroup PDT
 * \defgroup PDTI Signed Integers
 * \ingroup PDT
 * \defgroup PDTR References
 * \ingroup PDT
 * \defgroup PDTS Strings
 * \ingroup PDT
 * \defgroup PDTU Unsigned Integers
 * \ingroup PDT
 * \defgroup STRING String Datatypes
 * \ingroup H5T
 * \defgroup VLEN Variable-length Sequence Datatypes
 * \ingroup H5T

 */

#endif /* _H5Tmodule_H */
