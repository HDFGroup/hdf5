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
//======================================================================*/
#ifndef HDFTRACE_H
#define HDFTRACE_H
/************************************************************************/
/* A shared char * pointer (defined in HDF_Descriptors.c) and Macro 	*/
/* definitions								*/
/************************************************************************/
extern char *hdfRecordPointer;
/************************************************************************/
/* The following are used in tagging HDF packets.  Avoid using FAMILY   */
/* tags in the range 0100000 to 0200000					*/
/************************************************************************/
#define HDF_FAMILY 	 	0100000
#define HDF_SUMMARY_FAMILY  	0140000
/************************************************************************/
/* Number of fields in the packets.					*/
/************************************************************************/
#define NUM_FIELDS	17
/* 
 * "WRITE_HDF_ATTR" macro to output an attribute pair. 
 */
#ifndef WRITE_HDF_ATTR
#define WRITE_HDF_ATTR( attrKey, attrValue ) \
    sddfWriteString( &hdfRecordPointer, attrKey ); \
    sddfWriteString( &hdfRecordPointer, attrValue ); 
#endif /* WRITE_HDF_ATTR */

/* 
 * "WRITE_HDF_FIELD" macro to output a field with a single attribute pair. 
 */
#ifndef WRITE_HDF_FIELD
#define WRITE_HDF_FIELD( name, attrKey, attrValue, type, dimension ) \
    sddfWriteString( &hdfRecordPointer, name ); \
    sddfWriteInteger( &hdfRecordPointer, 1 ); \
    sddfWriteString( &hdfRecordPointer, attrKey ); \
    sddfWriteString( &hdfRecordPointer, attrValue ); \
    sddfWriteInteger( &hdfRecordPointer, type ); \
    sddfWriteInteger( &hdfRecordPointer, dimension ); 
#endif /* WRITE_HDF_FIELD */

/* 
 * "WRITE_HDF_FIELD2" to output a field with two attribute pairs. 
 */
#ifndef WRITE_HDF_FIELD2
#define WRITE_HDF_FIELD2( name, aKey1, aValue1, aKey2, aValue2, type, dimension ) \
    sddfWriteString( &hdfRecordPointer, name ); \
    sddfWriteInteger( &hdfRecordPointer, 2 ); \
    sddfWriteString( &hdfRecordPointer, aKey1 ); \
    sddfWriteString( &hdfRecordPointer, aValue1 ); \
    sddfWriteString( &hdfRecordPointer, aKey2 ); \
    sddfWriteString( &hdfRecordPointer, aValue2 ); \
    sddfWriteInteger( &hdfRecordPointer, type ); \
    sddfWriteInteger( &hdfRecordPointer, dimension ); 
#endif /* WRITE_HDF_FIELD2*/

#define FAMILY_NAME         	0260            /* Data Set Name Record */
#define FAMILY_MISC      	0270
#define FAMILY_HDFPROCNAME     	0300

typedef struct {
        long setID;
        char *setName;
} HDFsetInfo ;

#endif /* HDFTRACE_H */
