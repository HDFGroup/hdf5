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
 * File:  ProcIDs.h
 * Purpose: define IDs for identifying procedures in traces
 *-------------------------------------------------------------------------*/

#ifndef PROCIDS_H		/* avoid re-inclusion */
#define PROCIDS_H

#include "ProcMasks.h"
extern unsigned procTrace;
/*
 * Define the event IDs that will be used for the various I/O events
 */

/*======================================================================*/
/* Assign HDF identifier routine tags					*/
/*======================================================================*/
enum HDF_IDS {
	ID_HDFprocName=9996,
	ID_malloc=9997,
	ID_free=9998,
	ID_timeStamp=9999,
	DUMMY_HDF = 10000,
/*======================================================================*/
/* the following part of the enumeration is only needed when the 	*/
/* instrumented HDF library is built.  It is created in the pablo 	*/
/* subdirectory of HDF.							*/
/*======================================================================*/
#include "HDFidList.h"
ID_HDF_Last_Entry
} ;

#define BEGIN_HDF (DUMMY_HDF + 1)
#define END_HDF ID_HDF_Last_Entry
#define NumHDFProcs ( END_HDF - BEGIN_HDF )
/*======================================================================*/
/* HDF attribute, file, data_set, etc ID codes.	May not be used.	*/
/*======================================================================*/
#define NoDSid                     0            /* no data set id       */
#define HDF_NULL_ID 		   0
#define HDF_File_ID 		   1
#define HDF_SDS_ID 		   2
#define HDF_Dim_ID 		   3
#define HDF_Attribute_ID 	   4
#define HDF_Label_ID 		   5
#define HDF_Access_ID 		   6
#define HDF_Directory_ID 	   7
#define HDF_Annotation_ID          8
#define HDF_Gen_Raster_ID          9
#define HDF_Look_Up_Table_ID      10
#define HDF_Vdata_ID 		  11
#define BEGIN_MPIO            900800
#define END_MPIO              900899
/*======================================================================*/
/* Macros to tell if the ID is that of an HDF Entry or Exit             */
/*======================================================================*/
#define isBeginHDFEvent( ID ) ( BEGIN_HDF <= (ID) && (ID) < END_HDF )
#define isEndHDFEvent( ID )   isBeginHDFEvent(-(ID))
#define isBeginMPIOEvent( ID ) \
            ( BEGIN_MPIO <= (ID) && (ID) <= END_MPIO && (ID)%2 == 0 )

#define isEndMPIOEvent( ID ) \
            ( BEGIN_MPIO <= (ID) && (ID) <= END_MPIO && (ID)%2 == 1 )
#define isBeginIOEvent( ID )  \
        ( IOerrorID < (ID) && (ID) <= fsetposEndID && (ID)%2 == 1 )
#define isEndIOEvent( ID ) \
        ( IOerrorID < (ID) && (ID) <= fsetposEndID && (ID)%2 == 0 )
#define ProcIndex( ID ) ( (ID) - BEGIN_HDF )
#define ProcIndexForHDFEntry( ID ) ( (ID) - BEGIN_HDF )
#define ProcIndexForHDFExit( ID )  ProcIndexForHDFEntry(-ID)
#define HDFProcIXtoEventID( ID ) ( (ID) + BEGIN_HDF )

#define TRACE_ON(mask, eventID) \
        if (procTrace & mask) startHDFtraceEvent(eventID)
#define TRACE_OFF(mask, eventID ) \
        if (procTrace & mask)  endHDFtraceEvent(-eventID, 0, NULL, 0 )
 
#endif /* PROCIDS_H */
