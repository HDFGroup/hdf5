/*  This file is part of the Pablo Performance Analysis Environment
// 
//           (R)
//  The Pablo    Performance Analysis Environment software is NOT in
//  the public domain.  However, it is freely available without fee for
//  education, research, and non-profit purposes.  By obtaining copies
//  of this and other files that comprise the Pablo Performance Analysis
//  Environment, you, the Licensee, agree to abide by the following
//  conditions and understandings with respect to the copyrighted software:
//  
//  1.  The software is copyrighted in the name of the Board of Trustees
//      of the University of Illinois (UI), and ownership of the software
//      remains with the UI. 
// 
//  2.  Permission to use, copy, and modify this software and its documentation
//      for education, research, and non-profit purposes is hereby granted
//      to Licensee, provided that the copyright notice, the original author's
//      names and unit identification, and this permission notice appear on
//      all such copies, and that no charge be made for such copies.  Any
//      entity desiring permission to incorporate this software into commercial
//      products should contact:
// 
//           Professor Daniel A. Reed                 reed@cs.uiuc.edu
//           University of Illinois
//           Department of Computer Science
//           2413 Digital Computer Laboratory
//           1304 West Springfield Avenue
//           Urbana, Illinois  61801
//           USA
// 
//  3.  Licensee may not use the name, logo, or any other symbol of the UI
//      nor the names of any of its employees nor any adaptation thereof in
//      advertizing or publicity pertaining to the software without specific
//      prior written approval of the UI.
// 
//  4.  THE UI MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE
//      SOFTWARE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS
//      OR IMPLIED WARRANTY.
// 
//  5.  The UI shall not be liable for any damages suffered by Licensee from
//      the use of this software.
// 
//  6.  The software was developed under agreements between the UI and the
//      Federal Government which entitle the Government to certain rights.
// 
// *************************************************************************
// 
//  Developed by: The Pablo Research Group
//                University of Illinois at Urbana-Champaign
//                Department of Computer Science
//                1304 W. Springfield Avenue
//                Urbana, IL     61801
// 
//                http://www-pablo.cs.uiuc.edu
// 
//  Send comments to: pablo-feedback@guitar.cs.uiuc.edu
// 
//  Copyright (c) 1987-1998
//  The University of Illinois Board of Trustees.
//       All Rights Reserved.
// 
//  PABLO is a registered trademark of
//  The Board of Trustees of the University of Illinois
//  registered in the U.S. Patent and Trademark Office.
// 
//  Project Manager and Principal Investigator:
//       Daniel A. Reed (reed@cs.uiuc.edu)
// 
// Funded in part by the Defense Advanced Research Projects Agency under 
// DARPA contracts DABT63-94-C0049 (SIO Initiative), F30602-96-C-0161,
// and DABT63-96-C-0027 by the National Science Foundation under the PACI 
// program and grants NSF CDA 94-01124 and ASC 97-20202, and by the 
// Department of Energy under contracts DOE B-341494, W-7405-ENG-48, and 
// 1-B-333164.
//-------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------
 * File:  ProcTrace.h
 * Purpose: define entities for tracing HDF procedures
 *-------------------------------------------------------------------------*/

#ifndef PROCTRACE_H		/* avoid re-inclusion */
#define PROCTRACE_H
#include <stdarg.h>
/*======================================================================*
// By default, all HDF procedures are traced.  Tracing of individual	*
// procedures or all of the procedures in a particular source file in	*
// the HDF 5 library can be done by calling the procedure PabloHDF5trace*
// with the appropriate argument.  The call must be made prior to	*
// calling HDF5initTrace.  As many calls as necessary may be made prior	*
// to calling HDF5initTrace so several specific procedures can be 	*
// traced. 								*
// PabloHDF5trace has the following syntax.				*
//   #include "ProcTrace.h"						*
//   void PabloHDF5trace( int traceID );				*
// where								*
//   traceID specifies the procedure or procedures within an HDF 5 file	*
//   that are to be traced.  If a single procedure named <proc> is to 	*
//   be traced, then traceID should have the value ID_<proc>.  If all 	*
//   of the procedures within the HDF 5 library routine <file>.c are to *
//   be traced, then the value of traceID should be FID_<file>.  The	*
//   constants ID_<proc> and FID_<file> are declared for all possible	*
//   values of <proc> and <file> below.                         	*
//      								*
//   Example:								*
//     To enable tracing of the individual procedures H5I_register and	*
//     H5Topen and all of the procedures in the HDF 5 library source 	*
//     files H5A.c and H5Gent.c the following code segements could be 	*
//     used:								*
//     									*
//     #include "ProcTrace.h"						*
//	 ...								*
//     PabloHDF5trace( ID_H5I_register );				*
//     PabloHDF5trace( ID_H5Topenr );					*
//     PabloHDF5trace( FID_H5A );					*
//     PabloHDF5trace( FID_H5Gent );					*
//	...								*
//     HDF5initTrace( ... );   						*
//     									*
// See the document PabloHDF5.doc for further information		*
//======================================================================*/
/*======================================================================*/
/* Assign HDF identifier routine tags					*/
/*======================================================================*/
#ifdef RUNTIME_TRACE
#undef RUNTIME_TRACE
#endif
enum HDF_IDS {
RUNTIME_TRACE,
SUMMARY_TRACE,
MPI_RUNTIME_TRACE,
MPI_SUMMARY_TRACE,
NO_TRACE,
#include "HDFidList.h"
ID_HDF_Last_Entry,
AllHDF5 = ID_HDF_Last_Entry,
ID_H5_c,
ID_H5A_c,
ID_H5AC_c,
ID_H5B_c,
ID_H5D_c,
ID_H5E_c,
ID_H5F_c,
ID_H5Farray_c,
ID_H5Fcore_c,
ID_H5Ffamily_c,
ID_H5Fistore_c,
ID_H5Flow_c,
ID_H5Fmpio_c,
ID_H5Fsec2_c,
ID_H5Fsplit_c,
ID_H5Fstdio_c,
ID_H5G_c,
ID_H5Gent_c,
ID_H5Gnode_c,
ID_H5Gstab_c,
ID_H5HG_c,
ID_H5HL_c,
ID_H5I_c,
ID_H5MF_c,
ID_H5MM_c,
ID_H5O_c,
ID_H5Oattr_c,
ID_H5Ocomp_c,
ID_H5Ocont_c,
ID_H5Odtype_c,
ID_H5Oefl_c,
ID_H5Ofill_c,
ID_H5Olayout_c,
ID_H5Omtime_c,
ID_H5Oname_c,
ID_H5Onull_c,
ID_H5Osdspace_c,
ID_H5Oshared_c,
ID_H5Ostab_c,
ID_H5P_c,
ID_H5R_c,
ID_H5RA_c,
ID_H5S_c,
ID_H5Sall_c,
ID_H5Shyper_c,
ID_H5Smpio_c,
ID_H5Snone_c,
ID_H5Spoint_c,
ID_H5Sselect_c,
ID_H5T_c,
ID_H5TB_c,
ID_H5Tbit_c,
ID_H5Tconv_c,
ID_H5Tinit_c,
ID_H5V_c,
ID_H5Z_c,
NUM_HDF5_IDS
} ;

void PabloHDF5Trace( int ) ;
void HDF5initTrace( const char *, int trace_id, ... ); 
void HDF5endTrace( void ); 
#endif /* PROCTRACE_H */
