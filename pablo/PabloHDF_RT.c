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
// Funded in part by the Defense Advanced Research Projects Agency 
// under DARPA contracts DABT63-94-C0049 (SIO Initiative), 
// F30602-96-C-0161, and DABT63-96-C-0027 by the National Science 
// Foundation under the PACI program and grants NSF CDA 94-01124 and
// ASC 97-20202, and by the Department of Energy under contracts
// DOE B-341494, W-7405-ENG-48, and 1-B-333164.
*/ 
/*======================================================================*
// File:  PabloHDF_RT							*
// Purpose: support use of Pablo trace library to analyze HDF 		*
//	    performance							*
// Contents:								*
//  HDFinitTrace_RT        : initialize real-time tracing		*
//  HDFendTrace_RT         : complete trace 				*
//  initHDFProcTrace_RT    : sets up data structures at init time.	*
//  initproctracert_()	   : fortran interface				*
//  HDFtraceEvent_RT	   : called to record event information		*
//  HDFrecordSum 	   : adds fields of one record to those of 	*
//			     another					*
//  HDFnodeInit 	   : initializes linked list node		*
//  HDFrecordFileName	   : records named HDF identifiers 		*
//  BeginIOEventRecord     : initialize before I/O call			*
//  EndIOEventRecord 	   : finalize after I/O call			*
//  BeginMPIOEventRecord   : initialize before MPI-I/O call		*
//  EndMPIOEventRecord 	   : finalize after MPI-I/O call		*
//  BeginHDFEventRecord    : initialize before HDF call			*
//  EndHDFEventRecord 	   : finalizie after HDF call			*
//  HDFrecordFileName	   : record named identifier information	*
//  HDFassignPabloIDs	   : assigns a number to named identifiers	*
//  writeHDFNamePacketsRT  : write SDDF packets for identifier names	*
//  HDFupdateProcLists     : adds records in queue to entries in 	*
//			     tables					*
//  HDFupdateProcs	   : called by HDFupdateProcLists to do 	*
//			     addition					*
//  HDFSummarySDDF	   : write SDDF event summary packets		*
//  HDFnodeInit 	   : initialize event node			*
//  HDFrecordSum 	   : add one event record to another		*
//  getHDFFieldIndex	   : get Field Index for counts and times	*
//  getHDFByteFieldIndex   : get field index for bytes 			*
//  writeHDFRecDescrptrsRT : write HDF Record Descriptor packets	*
//  printFileMappingsRT	   : print map of named identifiers		*
//  _hdfNameDescriptor()   : writes SDDF descriptor packet for names	*
//======================================================================*/
#include "H5config.h"
#include "SystemDepend.h"
#include "Trace.h"
#include "TraceParam.h"
#include "ProcIDs.h"
#include "HDF5Trace.h"
#include "SDDFparam.h"
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
/*======================================================================* 
// on ipsc/860 don't include this or you'll get multiply defined SEEK_  *
//======================================================================*/
#ifndef __NX
#include <unistd.h>
#endif
 
#ifndef SUCCESS
#define SUCCESS 0
#define FAILURE	1
#endif

#ifndef TRUE
#define TRUE 1
#define FALSE	0
#endif
#define NEG_THREAD_ID -999

#include "HDF5record_RT.h"

#ifdef HAVE_PARALLEL
#include "mpio.h"
#include "MPIO_Init.h"
#include "MPIO_EventArgs.h"
#include "MPIO_TraceParams.h"
#endif

#ifndef TRgetThreadID
#define TRgetThreadID  TRgetNode
#endif

#ifndef TRnumNodes
#define TRnumNodes 1
#endif

#define AllThreads -1

#define set_c_mappedID( fd ) (fd)
#define c_mappedID( fd ) (fd)
/*======================================================================*
//  User output file pointer.						*
//======================================================================*/
FILE *outP;
/*======================================================================*
// Data Structures:							*
//									*
// HDFQueues:   an array of linked list.  Each list corresponds to an	*
//              HDF event and contains an entry for each different 	*
//		thread and data set referenced by a call to that event  *
//									*
// CallStack: 	a stack of HDFnode_t objects.  At any given time, the   *
//		stack represents the calling stack of the HDF routines 	*
//									*
// HDFfileList: a linked list of named identifiers and identifier 	*
//		numbers.  This is processed later to assign a single 	*
//		numbers to identifiers with the same name.		*
//======================================================================*/
HDFnode_t **HDFQueues;
HDFnode_t *CallStack;
HDFnode_t *TagQueue;
fileRec_t *HDFfileList;
/*======================================================================*
// Internal Function prototypes						*
//======================================================================*/
void HDFinitTrace_RT( char *, unsigned );
void HDFendTrace_RT( void );
int initproctracert_( void );
int initHDFProcTrace_RT( void );
void HDFtraceEvent_RT( int , char *, unsigned ) ;
void BeginIOEventRecord ( int, double , void * );
void EndIOEventRecord ( int , double , void * );
void BeginMPIOEventRecord ( int, double, void *, int ); 
void EndMPIOEventRecord ( int , double , void *, int);
void BeginHDFEventRecord( int , double );
void EndHDFEventRecord ( int , double ,void *);
void HDFrecordFileName( HDFsetInfo * );
void HDFassignPabloIDs( int *, char *** );
void writeHDFNamePacketsRT( char **, int );
void HDFupdateProcLists( void );
void HDFupdateProcs( HDFnode_t * );
void HDFSummarySDDF( HDFnode_t *, int );
void HDFnodeInit ( HDFnode_t * ) ;
void HDFrecordSum ( HDFrec_t *, HDFrec_t * );
int getHDFFieldIndex( int );
int getHDFByteFieldIndex( int );
void writeHDFRecDescrptrsRT( void );
void printFileMappingsRT( char *, char **, int );
void _hdfNameDescriptor( void );
void _hdfDescriptorRT( char *, char *, int );
void HDFfinalTimeStamp( void );
/*======================================================================*
// Global variables           						*
//======================================================================*/
HDFnode_t InitNode;		/* Node used in initialization		*/
HDFrec_t Tally;			/* Node used to get total		*/
char *FileName;			/* Name of Trace file			*/
HDFsetInfo openInfo;		/* Info about file opened		*/
char openName[256];		/* Name of opened file			*/
extern char *hdfRecordPointer;
/*======================================================================*
// NAME									*
//     HDFinitTrace_RT-- initialize HDF real-time tracing		*
// USAGE								*
//     VOID HDFinitTrace_RT( fileName, procTraceMask)			*
//									*
//     char *fileName;		IN: name of output file			*
//     unsigned procTraceMask;	IN: families of procedures to trace	*
// RETURNS								*
//     None.								*
//======================================================================*/
void HDFinitTrace_RT( char *fileName, unsigned procTraceMask )
{
#ifdef  HAVE_PARALLEL
	int myNode;
#endif
	int error;
	TR_LOCK	criticalSection;
	TRgetClock( &epoch );
	criticalSection = TRlock();
	error = initHDFProcTrace_RT() ;
    	procTrace = procTraceMask;
	TRunlock( criticalSection );
	if ( error != SUCCESS ) {
	   fprintf (stderr,"Unable to Initialize properly.  Exiting program\n");
	   exit(-1);
	}
	FileName = ( char * ) malloc ( strlen( fileName ) + 10 );
#ifdef  HAVE_PARALLEL
        MPI_Comm_rank( MPI_COMM_WORLD, &myNode );
        setTraceProcessorNumber( myNode );
	sprintf(FileName,"%s.nd%d",fileName,myNode);
	/*==============================================================*
	// In the parallel case, initialize MPI-IO tracing.  This will	*
	// set the trace file name.					*
	//==============================================================*/
#else
	/*==============================================================*
	// In the non-parallel case, set the trace file name and 	*
	// initialize the trace library.				*
	//==============================================================*/
	strcpy( FileName, fileName ) ;
#endif	/* HAVE_PARALLEL */
        setTraceFileName(FileName);
        basicLibraryInit( );         
}
/*======================================================================*
// NAME									*
//     HDFendTrace-- end HDF tracing					*
// USAGE								*
//     VOID HDFendTrace_RT(void)					*
// RETURNS								*
//     None.								*
//======================================================================*/
void HDFendTrace_RT( void )
{
	int j, numSetIDs;
	HDFnode_t *P;
	char **Names;
	char* mapFile;

	HDFfinalTimeStamp();
	/*==============================================================*
	//  Assing pablo ids to named identifiers and tag records	*
	//==============================================================*/
	HDFassignPabloIDs( &numSetIDs, &Names );
	/*==============================================================*
	//  Create a file name for the File map file.			*
	//==============================================================*/
	mapFile = (char *)malloc( strlen(FileName) + 4 );
	strcpy(mapFile,FileName);
	strcat(mapFile,".map");
	/*==============================================================*
	//  print the file mappings.					*
	//==============================================================*/
        printFileMappingsRT( mapFile, Names, numSetIDs ); 
	/*==============================================================*
	// Print SDDF summary records					*
	//==============================================================*/
	writeHDFRecDescrptrsRT();
	writeHDFNamePacketsRT( Names, numSetIDs );
     	for ( j = 0; j < NumHDFProcs; ++j ) {
	   HDFSummarySDDF( HDFQueues[j], j );
	}  
	endTracing();
	/*==============================================================*
	// Clean up storage						*
	//==============================================================*/
	free( (void *)mapFile );
	for ( j = 0; j < numSetIDs; ++j ) {
	    if ( Names[j] != NULL ) {
               free((void *)Names[j]);
	    }
	}
	free( (void *)Names );
        P = CallStack;
        if ( P->ptr != NULL ) {
	   fprintf(stderr,"CallStack not empty at termination\n");
        } else {
           free((void *)P);
	}
	free((void *)HDFQueues) ; 
}
/*======================================================================*
// initHFDProcTrace_RT							*
//	This function initializes data structures specific to		* 
//	the HDF real-time procedure entry/exit tracing extensions of 	*
//      the Pablo instrumentation library.  				*
//======================================================================*/
int initproctracert_( void )

{
	return initHDFProcTrace_RT();
}

int initHDFProcTrace_RT( void )

{
	int i, j, size;
	int numProcs = NumHDFProcs;

        if ( traceProcessorNumber == -1 ) {
            traceProcessorNumber = TRgetDefaultProcessor();
        }
	/*==============================================================*
	// Initialize InitNode used for node initialization.		*
	//==============================================================*/
	InitNode.ptr = NULL;
        InitNode.eventID = 0;             
        InitNode.lastIOtime = 0;           
        InitNode.record.nCalls = 0;             
        InitNode.record.lastCall = 0;
        InitNode.record.incDur = 0;              
        InitNode.record.excDur = 0;              
        InitNode.record.hdfID = 0;             
        InitNode.record.xRef = 0;             
	for ( j = 0; j < nTallyFields; ++j ) {
           InitNode.record.times[j] = 0; 
	}
	for ( j = 0; j < nTallyFields; ++j ) {
           InitNode.record.counts[j] = 0; 
	}
	for ( j = 0; j < nByteFields; ++j ) {
           InitNode.record.bytes[j] = 0; 
	}
	for ( i = 0; i < nByteFields; ++i ) {
	   for ( j = 0; j < nBkts; ++j ) {
	      InitNode.record.Hists[i][j] = 0;
	   }
	}
	/*==============================================================*
	// initialize linked list used to keep track of named hdf 	*
	// identifiers.							*
	//==============================================================*/
	HDFfileList = NULL;
	/*==============================================================*
	// Allocate a one dimensional array of pointers to queues of 	*
	// HDFnodes.  There is one queue for each thread and one for 	*
	// each HDF procedure.  Each queue will be a list of summary 	*
	// records distinquished by file type and 			*
	//==============================================================*/
	size = (int)(numProcs*sizeof( HDFnode_t * ));
	HDFQueues = (HDFnode_t **)malloc( size );
	if ( HDFQueues == NULL ) {
	   fprintf(stderr,"Failed to allocate HDFQueues in initHDFProcTrace\n");
	   return FAILURE;
	}
	for ( j = 0; j < numProcs; ++j ) {
	   HDFQueues[j] = NULL;
	}
	/*==============================================================*
	// Initialize call stack to a dummy node and TagQueue to NULL   *
	//==============================================================*/
	CallStack = (HDFnode_t *)malloc( sizeof(HDFnode_t) );
	*CallStack = InitNode;
	TagQueue = NULL ;
	return SUCCESS;
}
/*======================================================================*
// This is called from the HDF and I/O routines when real-time summary	*
// tracing is used.  It sets up a call stack for the specific thread in *
// use if no stack is yet set up.  It then calls calls a routine to 	*
// handle the event based on whether it is an I/O or HDF call.		*
//======================================================================*/
void HDFtraceEvent_RT( int eventType, char *dataPtr, unsigned dataLen ) 
{
	TR_LOCK	criticalSection;
	CLOCK	currentTime;
	double  seconds;

	criticalSection = TRlock();
	currentTime = getClock();
	seconds = clockToSeconds( currentTime );

	if ( isBeginIOEvent ( eventType ) || eventType == ID_malloc ) {
	   BeginIOEventRecord ( eventType, seconds, dataPtr ) ;
	} else if ( isEndIOEvent( eventType )  || eventType == -ID_malloc) {
	   EndIOEventRecord ( eventType, seconds, dataPtr );
	} else if ( isBeginHDFEvent( eventType ) ) { 
	   BeginHDFEventRecord ( eventType , seconds ) ;
	} else if ( isEndHDFEvent( eventType ) ) {
	   EndHDFEventRecord ( eventType, seconds, dataPtr );
#ifdef  HAVE_PARALLEL
	} else if ( isBeginMPIOEvent( eventType ) ) { 
	   BeginMPIOEventRecord ( eventType, seconds, dataPtr, dataLen ) ;
	} else if ( isEndMPIOEvent( eventType ) ) {
	   EndMPIOEventRecord ( eventType, seconds, dataPtr, dataLen );
#endif  /* HAVE_PARALLEL */
	} else {
	   fprintf(stderr,"eventType %d, dataLen = %u\n",eventType,dataLen);
	} 
	TRunlock( criticalSection );
}
/*======================================================================* 
// BeginIOEventRecord:                                               	*
//  This routine simply records the time in the record on the top of 	*
//  the stack.								*
//======================================================================*/ 
void BeginIOEventRecord ( int eventType, double seconds, void *dataPtr  )
{
	char *name;
	/*==============================================================*
	// save the time value temporarily in top of stack		*
	// When the end record is received, the duration can be 	*
	// computed.							*
	//==============================================================*/
	CallStack->lastIOtime = seconds;
	/*==============================================================*
	// get the ID or name of the file accessed from the structure	*
	// passed as dataPtr.  						*
	//==============================================================*/
	switch ( eventType )
	{
		case fopenBeginID:
		case openBeginID:
		   name = (char *)(dataPtr) + 2*sizeof(int);
  	   	   strcpy( openName, name );
		   break;
		case fcloseBeginID:
		case closeBeginID:
	   	   CallStack->record.hdfID = *( long *)dataPtr;
		   break;
		case readBeginID:
		case freadBeginID:
		case writeBeginID:
		case fwriteBeginID:
		case lseekBeginID:
		case fseekBeginID:
		case fsetposBeginID:
		case rewindBeginID:
	   	   CallStack->record.hdfID = *(int *)dataPtr;
		   break;
		default:
		   break;
	}
}
/*======================================================================* 
// EndIOEventRecord:							*
//  This routine retrieves the entry time saved on the top of the stack *
//  and computes the duration of the I/O event.  This is added to the   *
//  record field corresponding to this type of I/O.  The Bytes field in *
//  the record is updated if this is a read or write operation.		*
//======================================================================*/ 
void EndIOEventRecord ( int eventType, double secs, void *dataPtr )
{
	double incDur;
	int i, Field, ByteField, bytes;

	incDur = secs - CallStack->lastIOtime;
	Field = getHDFFieldIndex( eventType ) ;
        CallStack->record.times[Field] += incDur;
        ++CallStack->record.counts[Field];
	ByteField = getHDFByteFieldIndex( Field ) ;
	switch ( eventType ) 
	{
		case readEndID:
		case freadEndID:
		case writeEndID:
		case fwriteEndID:
		case -ID_malloc:
	   	   bytes = *((int *)dataPtr);
	      	   CallStack->record.bytes[ByteField] += bytes;
	           /*====================================================
		   // update histogram					*
	           //===================================================*/
                   i = -1;
                   while ( bytes >= BktLim[i+1] ) ++i  ;
                   if ( i >= 0 ) ++CallStack->record.Hists[ByteField][i];
		   break;
		case fopenEndID:
		case openEndID:
		   openInfo.setName = openName;
		   openInfo.setID = (int)(*((long *)dataPtr));
		   CallStack->record.hdfID = openInfo.setID;
	           HDFrecordFileName ( &openInfo );
		   break;
		default:
		   break;
	}
			
}
#ifdef HAVE_PARALLEL
/*======================================================================*
// BeginMPIOEventRecord:                                               	*
//  This routine simply records the time in the record on the top of 	*
//  the stack.								*
//======================================================================*/ 
void BeginMPIOEventRecord( int eventType, 
	                   double seconds, 
	                   void *dataPtr,
	                   int dataLen )
{
	/*==============================================================*
	// save the time value temporarily in top of stack		*
	// When the end record is received, the duration can be 	*
	// computed.							*
	//==============================================================*/
	CallStack->lastIOtime = seconds;
	/*==============================================================*
	// get useful info from the structure pointed to by dataPtr.	*
	// in most cases, this is the file ID.  For mpiOpen, it is the	*
	// name of the file.  For mpiDelete, no information is of any	*
	// use.								*
	//==============================================================*/
        if ( dataLen == 0 ) return;
	switch ( eventType ) 
	{
	   case mpiGetSizeBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiGetSizeBeginArgs *)dataPtr)->fileID;
	      break;
	   case mpiGetGroupBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiGetGroupBeginArgs *)dataPtr)->fileID;
	      break;
	   case mpiGetAmodeBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiGetAmodeBeginArgs *)dataPtr)->fileID;
	      break;
	   case mpiGetViewBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiGetViewBeginArgs *)dataPtr)->fileID;
	      break;
	   case mpiGetPositionBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiGetPositionBeginArgs *)dataPtr)->fileID;
	      break;
	   case mpiGetByteOffsetBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiGetByteOffsetBeginArgs *)dataPtr)->fileID;
	      break;
	   case mpiGetTypeExtentBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiGetTypeExtentBeginArgs *)dataPtr)->fileID;
	      break;
	   case mpiGetAtomicityBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiGetAtomicityBeginArgs *)dataPtr)->fileID;
	      break;
	   case mpiOpenBeginID:
	      strcpy( openName,
		     ((struct mpiOpenBeginArgs *)dataPtr)->fileName);
	      break;
	   case mpiCloseBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiCloseBeginArgs *)dataPtr)->fileID;
	      break;
	   case mpiDeleteBeginID:
	      break;
	   case mpiSetSizeBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiSetSizeBeginArgs *)dataPtr)->fileID;
	      break;
	   case mpiPreallocateBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiPreallocateBeginArgs *)dataPtr)->fileID;
	      break;
	   case mpiSetViewBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiSetViewBeginArgs *)dataPtr)->fileID;
	      break;
	   case mpiReadAtBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiReadAtBeginArgs *)dataPtr)->fileID;
	      break;
	   case mpiReadAtAllBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiReadAtAllBeginArgs *)dataPtr)->fileID;
	      break;
	   case mpiWriteAtBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiWriteAtBeginArgs *)dataPtr)->fileID;
	      break;
	   case mpiWriteAtAllBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiWriteAtAllBeginArgs *)dataPtr)->fileID;
	      break;
	   case mpiIreadAtBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiIreadAtBeginArgs *)dataPtr)->fileID;
	      break;
	   case mpiIwriteAtBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiIwriteAtBeginArgs *)dataPtr)->fileID;
	      break;
	   case mpiReadBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiReadBeginArgs *)dataPtr)->fileID;
	      break;
	   case mpiReadAllBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiReadAllBeginArgs *)dataPtr)->fileID;
	      break;
	   case mpiWriteBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiWriteBeginArgs *)dataPtr)->fileID;
	      break;
	   case mpiWriteAllBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiWriteAllBeginArgs *)dataPtr)->fileID;
	      break;
	   case mpiIreadBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiIreadBeginArgs *)dataPtr)->fileID;
	      break;
	   case mpiIwriteBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiIwriteBeginArgs *)dataPtr)->fileID;
	      break;
	   case mpiSeekBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiSeekBeginArgs *)dataPtr)->fileID;
	      break;
	   case mpiSetAtomicityBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiSetAtomicityBeginArgs *)dataPtr)->fileID;
	      break;
	   case mpiSyncBeginID:
	      CallStack->record.hdfID 
		 = ((struct mpiSyncBeginArgs *)dataPtr)->fileID;
	      break;
	   default:
	      break;
	}
}
/*======================================================================*
// EndMPIOEventRecord:							*
//  This routine retrieves the entry time saved on the top of the stack *
//  and computes the duration of the MPI-I/O event.  This is added to   *
//  the record field corresponding MPI-I/O.  				*
//======================================================================*/ 
void EndMPIOEventRecord ( int eventType, 
	                  double seconds, 
	                  void *dataPtr,
	                  int dataLen )
{
	double incDur;

	incDur = seconds - CallStack->lastIOtime;
        CallStack->record.times[MPI] += incDur;
        ++CallStack->record.counts[MPI];
	if ( eventType == mpiOpenEndID && dataLen != 0 ) {
	   /*===========================================================*
	   // complete the file information for the case of a file 	*
	   // open and record the information.				*
	   //===========================================================*/
	   openInfo.setName = openName;
	   openInfo.setID = ((struct mpiOpenEndArgs *)dataPtr)->fileID;
	   CallStack->record.hdfID = openInfo.setID;
	   HDFrecordFileName ( &openInfo );
	}
}
#endif /* HAVE_PARALLEL */
/*======================================================================*
//   BeginHDFEventRecord:						* 
// 	This function puts a trace record on the stack corresponding to	*
//   	this thread.  If no stack exists, one is created.  If no record	* 
//   	exist, a record is created.                                   	* 
//======================================================================*/ 
void BeginHDFEventRecord( int eventID, double secs )
{
	HDFnode_t *HDFrec;
	/*==============================================================*
	// Create a record. Push it onto the call stack.                *
	//==============================================================*/
        HDFrec = (HDFnode_t *)malloc( sizeof(HDFnode_t) );
        HDFnodeInit( HDFrec ) ;
	HDFrec->eventID = eventID;
	HDFrec->ptr = CallStack;
	CallStack = HDFrec ;
	/*==============================================================*
	// save time stamp in record.					*
	//==============================================================*/
	HDFrec->record.lastCall = secs;
}
/*======================================================================* 
// EndHDFEventRecord:							*
//  This routine pops the HDF record from the top of the stack 		*
//  corresponding to this thread and computes the inclusive duration    *
//  and adds it to the inclusive duration field of this record and to   *
//  the HDF time field of the calling routines record.			*
//======================================================================*/ 
void EndHDFEventRecord ( int eventID, double seconds, void *dataPtr )
{
        HDFsetInfo 	*info;
	HDFnode_t	*HDFrec;
	double 		incSecs;
	static int	dummyIDs = -4;
	/*==============================================================*
	// pop record from top of the stack, compute inclusive duration	*
	// and set the corresponding record field and increment nCalls.	*
	//==============================================================*/
	HDFrec = CallStack;
	CallStack = CallStack->ptr;
	if ( CallStack == NULL ) {
	   fprintf(stderr,">>> EndHDFEventRecord: Call Stack is empty. <<<\n");
	   return;
	}
	incSecs = seconds - HDFrec->record.lastCall;
	HDFrec->record.incDur = +incSecs;
	++HDFrec->record.nCalls;
   	/*==============================================================*
	// add old record to chain to have its xRef field tagged.	*
	//==============================================================*/
	HDFrec->ptr = TagQueue;
	TagQueue = HDFrec;
	/*==============================================================*
	// Add set ID information.					*
	//==============================================================*/
	if ( dataPtr != NULL ) {
	   info = (HDFsetInfo *)dataPtr;
	   if ( info->setName != NULL ) {
	      if ( info->setID == 0 ) {
	         info->setID = dummyIDs--;
	      }
	      HDFrecordFileName ( info );
	   }
	   HDFrec->record.hdfID = info->setID;
	}
	/*==============================================================*
	// Update the HDF totals for the calling program.		*
	//==============================================================*/
        CallStack->record.times[ HDF_ ] += incSecs ;
        ++CallStack->record.counts[ HDF_ ] ;
	/*==============================================================*
	// If the stack has only one record it represents the main 	* 
	// program.  Tag all of the records on the TagQueue and tally   * 
	// them up.							* 
	//==============================================================*/
        if ( CallStack->ptr == NULL ) {
           HDFupdateProcLists( );
	}
}
/*======================================================================* 
// This routine keeps track of the identifier names and tags.  Some	*
// names may be associated with more than one tag.  This will be 	*
// rectified when final tallies are done.				*
//======================================================================*/
void HDFrecordFileName( HDFsetInfo *info )
{
	fileRec_t *P;
	char *t;
	int match; 
	long id;
	P = HDFfileList;
	match = FALSE;
	id = info->setID;
	while ( P != NULL && match == FALSE ) {
	   if ( strcmp( P->fileName, info->setName ) != 0 && P->hdfID == id ) {
	      match = TRUE;
	   } else {
	      P = P->ptr;
	   }
	}
	if ( match == FALSE ) {
	   P = ( fileRec_t *) malloc( sizeof( fileRec_t ) );
	   P->ptr = HDFfileList;
	   HDFfileList = P;
	   t = (char *)malloc( strlen( info->setName ) + 1 );
	   strcpy ( t, info->setName ) ;
	   P->fileName = t;
	   P->hdfID = info->setID;
	   P->PabloID = 0;
	} 
}  
/*======================================================================* 
// This routine assigns a unique Pablo ID to each unique name 		*
// regardless of the HDF tag.						*
// It then goes through the HDFRecordQueue and marks each record with   *
// the PabloID corresponding to the hdfID and xRef fields or 0.		*
//======================================================================*/
void HDFassignPabloIDs( int *nSetIDs, char ***Names )
{
	fileRec_t *F, *G;
	HDFnode_t *P;
	int j; 
	long PabloID = 1;
	long hdfID, xRef;
	char *fName, **T;

	F = HDFfileList;
        /*==============================================================*
        // Assign the same ID to identical names.			*
        //==============================================================*/
	while ( F != NULL ) {
	   if ( F->PabloID == 0 ) {
	      F->PabloID = PabloID++;
	      fName = F->fileName;
	      G = F->ptr;
	      while ( G != NULL ) {
	         if ( strcmp( G->fileName , fName ) == 0 ) {
	            G->PabloID = F->PabloID;
	         }
	         G = G->ptr;
	      }
	   }
	   F = F->ptr;
	}
	*nSetIDs = (int)(PabloID - 1);
        if ( *nSetIDs <= 0 ) return;
        /*==============================================================*
	// Repace hdfID and xRef fields with corresponding Pablo ID	*
        //==============================================================*/
   	for ( j = 0; j < NumHDFProcs; ++j ) {
   	   P = HDFQueues[j] ;
   	   while ( P != NULL ) {
   	      hdfID = P->record.hdfID;
   	      if ( hdfID != 0 ) {
   	         PabloID = 0;
                 F = HDFfileList;
   	         while ( F != NULL && PabloID == 0 ) {
                    if ( hdfID == F->hdfID ) {
   	               PabloID = F->PabloID;
   	            }
   	            F = F->ptr;
   	         }
   	         P->record.hdfID = PabloID;
   	      }
   	      xRef = P->record.xRef;
   	      if ( xRef != 0 ) {
   	         PabloID = 0;
                 F = HDFfileList;
   	         while ( F != NULL && PabloID == 0 ) {
                    if ( xRef == F->hdfID ) {
   	               PabloID = F->PabloID;
   	            }
   	            F = F->ptr;
   	         }
   	         P->record.xRef = PabloID;
   	      }
              P = P->ptr;
   	   } /* end while ( P != NULL ) */
        } /* end for */
	/*==============================================================*
	// get a list of all the unique names and order them according  *
	// to their Pablo IDs.						*
	//==============================================================*/
	T = ( char ** )malloc( (*nSetIDs+1) * sizeof( char * ) );
	for ( j = 0; j <= *nSetIDs; ++j ) {
	   T[j] = NULL;
	}
	F = HDFfileList;
	while ( F != NULL ) {
	   PabloID = F->PabloID  ;
	   if ( T[PabloID] == NULL ) {
	      T[PabloID] = ( char * )malloc( strlen( F->fileName ) + 1 );
	      strcpy( T[PabloID], F->fileName ) ;
	   }
	   free((void *)(F->fileName));
	   G = F;
	   F = F->ptr;
	   free ( (void *)G );
	}
	*Names = T;
}
/*======================================================================* 
// This routine writes SDDF packets to SDDF file containing information	*
// about the named identifiers found in the program.			*
//======================================================================*/
void writeHDFNamePacketsRT( char **Names, int numSetIDs )
{
	int j;
	HDFNamePacket_t NamePkt;
	char *BUFF, *fName;
	int buffSize;
	/*==============================================================*
	// Allocate a buffer to hold the packet.  Allow 80 chars for 	*
	// identifier name.						*
	//==============================================================*/
	buffSize = sizeof(HDFNamePacket_t) + 80;
	BUFF = (char *)malloc(buffSize);
	/*==============================================================*
	// Fill in constant information					*
	//==============================================================*/
	NamePkt.packetType = PKT_DATA;
	NamePkt.packetTag = FAMILY_NAME;
	/*==============================================================*
	// Fill in named identifier information and write to SDDF file	*
	//==============================================================*/
	for ( j = 1; j <= numSetIDs; ++j ) {
	   fName = Names[j];
	   NamePkt.packetLength = (int)(sizeof(NamePkt) + strlen(fName));
	   NamePkt.fileType = 0;		/* not currently used	*/
	   NamePkt.fileID = j;
	   NamePkt.nameLen = (int)strlen(fName) ;
	   if ( buffSize < NamePkt.packetLength ) {
	      free((void *)BUFF) ;
	      buffSize = NamePkt.packetLength + 80;
	      BUFF = (char *)malloc( buffSize ) ;
	   }
	   /*===========================================================*
	   // Copy packet data and tack on identifier name		*
	   //===========================================================*/
	   memcpy( BUFF, &NamePkt, sizeof(NamePkt) );
	   memcpy( BUFF + sizeof(NamePkt) , fName, strlen(fName) );
	   putBytes( BUFF , NamePkt.packetLength ) ;
	}
	free((void *)BUFF);
}
/*======================================================================*
// Tag xRef field of all records in this queue with the hdfID of the 	*
// highest level caller. Also						*
// This routine takes the records after they have been tagged and adds  *
// their fields to the apporopriate position in the HDFQueues structure *
//======================================================================*/
void HDFupdateProcLists( void )
{
	HDFnode_t *P, *Q;
	long hdfID;

	hdfID = TagQueue->record.hdfID;
	P = TagQueue;
	while ( P != NULL ) {
	   P->record.xRef = hdfID;
	   Q = P->ptr;
	   HDFupdateProcs( P );
	   P = Q;
	}
        TagQueue = NULL;
}
/*======================================================================*
// This routine takes as input a node pointer P  and looks for a Total  *
// record with this same eventID, hdfID and xRef.  If such a record     *
// exists, P is added to the record, otherwise a record is created and  *
// its values are set to P's.						*
//======================================================================*/
void HDFupdateProcs( HDFnode_t *P )
{
	int procIndex, eventID;
	long hdfID, xRef;
	HDFnode_t *Q;
	eventID = P->eventID;
	procIndex = ProcIndexForHDFEntry( eventID );
        hdfID = P->record.hdfID;
        xRef = P->record.xRef;
	Q = HDFQueues[ procIndex ];
	/*==============================================================*
	// First determine if a tally node exists that matches the     	*
	// eventID, hdfID and xRef of P.				*
	//==============================================================*/
	while ( Q != NULL && 
            (( Q->record.hdfID != hdfID ) || ( Q->record.xRef != xRef )) ) {
           Q = Q->ptr;
	}
	if ( Q == NULL ) {
	   /*===========================================================*
	   // No tally record matches the hdfID and xRef so put P in    *
	   // the queue.                 				*
	   //===========================================================*/
	   P->ptr = HDFQueues[ procIndex ];
	   HDFQueues[ procIndex ] = P;
	} else {
	   /*===========================================================*
	   // add P to the exiting record and free it.			*
	   //===========================================================*/
           HDFrecordSum ( &Q->record , &P->record );
	   free((void *)P);
	}
}
/*======================================================================*
// Print SDDF records for all records in this linked list.		*
//======================================================================*/
void HDFSummarySDDF( HDFnode_t *P, int procIndex )
{
	int i, j, arrayLen;
	int allIOCount;
	double allIOTime;
	char buff[1024];
	char *Packet;
	HDFnode_t *Q;
	struct {
		int packetLen,
		    packetType,
		    packetTag,
	            eventID,
		    threadID,
	            nCalls;
                 double Seconds, 
                        IncDur,
                        ExcDur;
	         long HDFid,
	              XREFid;
	} Header;

	Header.packetLen = sizeof(Header) 
	                 + sizeof(int)			 /* array len   */
			 + nTallyFields*sizeof(double)	 /* times array */
	                 + sizeof(int)			 /* array len   */
	                 + nTallyFields*sizeof(int) 	 /* count array */
	                 + sizeof(int)			 /* array len   */
	                 + nByteFields*sizeof(int) 	 /* bytes array */
	                 + nByteFields*sizeof(int)	 /* array lens  */
	                 + nByteFields*nBkts*sizeof(int) /* byte hist   */
	                 + sizeof(int) ;                 /* Name len    */
	Header.packetTag = HDF_SUMMARY_FAMILY +
			   ( procIndex + 1 )*8 + RECORD_TRACE ;
	Header.packetType = PKT_DATA;
	Header.threadID = TRgetNode();
        while ( P != NULL ) {
	   Q = P->ptr;
	   Header.eventID = P->eventID;
	   Header.nCalls  = P->record.nCalls;
           Header.Seconds = P->record.lastCall;
	   Header.IncDur  = P->record.incDur;
	   Header.ExcDur  = P->record.excDur;
	   Header.HDFid   = P->record.hdfID;
	   Header.XREFid  = P->record.xRef;
	   Packet = buff;
	   memcpy( Packet, &Header, sizeof(Header) );
	   Packet += sizeof(Header);
	   /*===========================================================*
	   // Total the I/O time and counts                        	*
	   //===========================================================*/
           allIOTime = 0;
           for ( j = FirstIO; j <= LastIO; ++j ) {
              allIOTime += P->record.times[j];
           }
           P->record.times[AllIO] = allIOTime;
 
           allIOCount = 0;
           for ( j = FirstIO; j <= LastIO; ++j ) {
              allIOCount += P->record.counts[j];
           }
           P->record.counts[AllIO] = allIOCount;
	   /*===========================================================*
	   // copy length of times array and times array to Packet.	*
	   //===========================================================*/
	   arrayLen = nTallyFields;
	   memcpy( Packet, &arrayLen, sizeof(int) );
	   Packet += sizeof(int);
	   memcpy( Packet, P->record.times, nTallyFields*sizeof(double) );
	   Packet += nTallyFields*sizeof(double);
	   /*===========================================================*
	   // copy length of counts array and counts array to Packet.	*
	   //===========================================================*/
	   arrayLen = nTallyFields;
	   memcpy( Packet, &arrayLen, sizeof(int) );
	   Packet += sizeof(int);
	   memcpy( Packet, P->record.counts, nTallyFields*sizeof(int) );
	   Packet += nTallyFields*sizeof(int);
	   /*===========================================================*
	   // copy length of bytes array and bytes array to Packet.	*
	   //===========================================================*/
	   arrayLen = nByteFields;
	   memcpy( Packet, &arrayLen, sizeof(int) );
	   Packet += sizeof(int);
	   memcpy( Packet, P->record.bytes, nByteFields*sizeof(int) );
	   Packet += nByteFields*sizeof(int);
	   /*===========================================================*
	   // copy length of historgram arrays and arrays to Packet.	*
	   //===========================================================*/
	   arrayLen = nBkts;
	   for ( i = 0; i < nByteFields; ++i ) {
	      memcpy( Packet, &arrayLen, sizeof(int) );
	      Packet += sizeof(int);
	      memcpy( Packet, P->record.Hists[i], nBkts*sizeof(int) );
	      Packet += nBkts*sizeof(int);
	   }
	   arrayLen = 0;	/* name length */
	   memcpy( Packet, &arrayLen, sizeof(int) );
	   putBytes( buff, Header.packetLen ); 
           free((void *)P);
           P = Q;
	} 
}
/*======================================================================*
// Initialize a node.							*
//======================================================================*/
void HDFnodeInit ( HDFnode_t *S ) 
{
	*S = InitNode;
}
/*======================================================================*
//      Compute IO totals, exclusive durations of the input record T    *
//      then add the fields of T to that of S.                          *
//======================================================================*/
void HDFrecordSum ( HDFrec_t *S, HDFrec_t *T )
{
        int i, j;
 
        T->excDur = T->incDur - ( T->times[HDF_] + T->times[MPI]
                                                 + T->times[AllIO] );
 
        S->nCalls    += T->nCalls;
        S->lastCall  =  max( S->lastCall, T->lastCall );
        S->incDur    += T->incDur;
        S->excDur    += T->excDur;
        for ( j = 0; j < nTallyFields; ++j ) {
           S->times[j] += T->times[j] ;
        }
        for ( j = 0; j < nTallyFields; ++j ) {
           S->counts[j] += T->counts[j] ;
        }
        for ( j = 0; j < nByteFields; ++j ) {
           S->bytes[j] += T->bytes[j] ;
        }
        for ( j = 0; j < nByteFields; ++j ) {
           for ( i = 0; i < nBkts; ++i ) {
              S->Hists[j][i] += T->Hists[j][i] ;
           }
        }
}
/*======================================================================*
// Return the field index corresponding to an IO event ID.  The fields  *
// are specified in an enum statement in an include file.		*
//======================================================================*/
int getHDFFieldIndex( int eventID )
{
	int result = -1;
	switch ( eventID )
	{
	        case ID_malloc:
	        case -ID_malloc:
			result = Malloc;
			break;
		case openBeginID:
		case openEndID:
		case fopenBeginID:
		case fopenEndID:
			result = Open;
			break;
		case closeBeginID:
		case closeEndID:
		case fcloseBeginID:
		case fcloseEndID:
			result = Close;
			break;
		case readBeginID:
		case readEndID:
		case freadBeginID:
		case freadEndID:
			result = Read;
			break;
		case lseekBeginID:
		case lseekEndID:
		case fseekBeginID:
		case fseekEndID:
			result = Seek;
			break;
		case writeBeginID:
		case writeEndID:
		case fwriteBeginID:
		case fwriteEndID:
			result = Write;
			break;
		case fflushBeginID:
		case fflushEndID:
		case flushBeginID:
		case flushEndID:
			result = Misc;
			break;
		case rewindBeginID:
		case rewindEndID:
		case fsetposBeginID:
		case fsetposEndID:
			result = Misc;
			break;
#ifdef	creadBeginID
		case creadBeginID:
		case creadEndID:
		case creadvBeginID:
		case creadvEndID:
			result = Read;
 			break;
		case cwriteBeginID:
		case cwriteEndID:
		case cwritevBeginID:
		case cwritevEndID:
			result = Write;
 			break;
		case ireadBeginID:
		case ireadEndID:
		case ireadvBeginID:
		case ireadvEndID:
			result = ARead;
 			break;
		case iwriteBeginID:
		case iwriteEndID:
		case iwritevBeginID:
		case iwritevEndID:
			result = AWrite;
 			break;
		case iowaitBeginID:
		case iowaitEndID:
			result = Wait;
 			break;
		case iodoneBeginID:
		case iodoneEndID:
			result = Misc;
 			break;
		case gopenBeginID:
		case gopenEndID:
			result = Open;
 			break;
		case iomodeBeginID:
		case iomodeEndID:
		case setiomodeBeginID:
		case setiomodeEndID:
		case lsizeBeginID:
		case lsizeEndID:
		case forflushBeginID:
		case forflushEndID:
			result = Misc;
 			break;
#endif	
	}
	return result;
}
/*======================================================================*
// This routine determines the field index in the bytes array of the 	*
// HDF records which correspond to a given IO operation.  If the  	*
// operation does not transfer bytes, (e.g., open operation), -1 is	*
// returned.								*
//======================================================================*/
int getHDFByteFieldIndex( int Operation ) 
{
	int result;
	switch ( Operation )
	{
		case Malloc:
	 		result = MallocBytes;
			break;
		case Read:
	 		result = ReadBytes;
			break;
		case Write:
	 		result = WriteBytes;
			break;
		case ARead:
	 		result = AReadBytes;
			break;
		case AWrite:
	 		result = AWriteBytes;
			break;
		default:
			result = -1;
			break;
	}
	return result;
}
/*======================================================================*
// This routine writes the SDDF packet descriptors for the HDF summary	*
// records to the output file.						*
//======================================================================*/
void _hdfDescriptorRT( char *recordName, char *recordDescription, 
                                                       int recordFamily )
{
    static char	recordBuffer[ 4096 ];
    int		recordLength;

    hdfRecordPointer = recordBuffer;
    /*==================================================================*
    // Allow space at the beginning of the record for the packet 	*
    //length which will be computed after the packet is complete.	*
    //==================================================================*/
    sddfWriteInteger( &hdfRecordPointer, 0 );
    /*==================================================================* 
    // The record type, tag, and name 					* 
    //==================================================================*/
    sddfWriteInteger( &hdfRecordPointer, PKT_DESCRIPTOR );
    sddfWriteInteger( &hdfRecordPointer, ( recordFamily | RECORD_TRACE ) );
    sddfWriteString( &hdfRecordPointer, recordName );
    /*==================================================================* 
    // The record attribute count and string pair 			* 
    //==================================================================*/
    sddfWriteInteger( &hdfRecordPointer, 1 );
    sddfWriteString( &hdfRecordPointer, "description" );
    sddfWriteString( &hdfRecordPointer, recordDescription );
    /*==================================================================*
    // The record field count 						*
    //==================================================================*/
    sddfWriteInteger( &hdfRecordPointer, 17 );
    WRITE_HDF_FIELD( "Event Identifier", 
	             "Event ID",
                     "Corresponding Event",
	             INTEGER, 0 );
    WRITE_HDF_FIELD( "Processor Number", 
		     "Node", 
		     "Processor number", 
		     INTEGER, 0 );
    WRITE_HDF_FIELD( "N Calls", 
		     "N Calls", 
		     "Number of Calls to this Proc", 
		     INTEGER, 0 );
    WRITE_HDF_FIELD( "Seconds", 
	   	     "Seconds", 
                     "Floating Point Timestamp", 
		      DOUBLE, 0 );
    WRITE_HDF_FIELD( "Inclusive Duration", 
  	             "Inclusive Duration", 
	             "Inclusive Duration of this Procedure",
		      DOUBLE, 0 );
    WRITE_HDF_FIELD( "Exclusive Duration", 
  	             "Exclusive Duration", 
	             "Excludes IO, MPI-IO and other HDF calls",
		      DOUBLE, 0 );
    WRITE_HDF_FIELD2("HDF ID",
                     "HDF ID", "Identifier number",
                     "0", "No HDF ID specified",
                     LONG, 0 );
    WRITE_HDF_FIELD( "Xref ID",
                     "Cross Reference", 
                     "Index of related HDF ID or 0 if none",
		     LONG, 0 );
    WRITE_HDF_FIELD( "Times Array",
                     "Times Array",
	             "Array of Total Operation Times",
		     DOUBLE, 1 );
    WRITE_HDF_FIELD( "Counts Array",
                     "Counts Array",
	             "Array of Total Operation Counts",
		     INTEGER, 1 );
    WRITE_HDF_FIELD( "Bytes Array",
                     "Bytes Array",
	             "Array of Total Bytes Transferred",
		     INTEGER, 1 );
    WRITE_HDF_FIELD( "Malloc Histogram",
                     "Malloc Histogram",
	             "Historgram of size Malloc Requests",
		     INTEGER, 1 );
    WRITE_HDF_FIELD( "Read Histogram",
                     "Read Histogram",
	             "Historgram of size Read Requests",
		     INTEGER, 1 );
    WRITE_HDF_FIELD( "Write Histogram",
                     "Write Histogram",
	             "Historgram of size Write Requests",
		     INTEGER, 1 );
    WRITE_HDF_FIELD( "ARead Histogram",
                     "ARead Histogram",
	             "Historgram of size Asynch Read Requests",
		     INTEGER, 1 );
    WRITE_HDF_FIELD( "AWrite Histogram",
                     "AWrite Histogram",
	             "Historgram of size Asynch Write Requests",
		     INTEGER, 1 );
    WRITE_HDF_FIELD( "HDF Name",
                     "HDF Name", 
	             "Name of File,Data Set or Dim accessed",
                     CHARACTER, 1 ); 
    /*=================================================================== 
    // The entire record descriptor packet has been written.		* 
    // Compute and update the record length.				* 
    // Write the completed record.					* 
    //==================================================================*/
    recordLength = (int)(hdfRecordPointer - recordBuffer);

    hdfRecordPointer = recordBuffer;
    sddfWriteInteger( &hdfRecordPointer, recordLength );

    putBytes( recordBuffer, (unsigned) recordLength );
}

/*======================================================================* 
//   Internal Routine:  writeHDFRecDescrptrsRT	                        * 
//                      Writes record descriptors for the HDF events.   * 
//======================================================================*/
void writeHDFRecDescrptrsRT( void ) 
{
	char HDFProcNames[][40] = {
#	include "HDFentryNames.h"
	"HDF_Last_Entry"
	};
	int j, FAMILY;
        char BUF1[256], BUF2[256] ;
	_hdfNameDescriptor();	/* Descriptor for named identifiers	*/
        for ( j = 0; j < NumHDFProcs; ++j ) {
           if ( HDFQueues[j] != NULL ) {
              strcpy( BUF2, "HDF ");
              strcat( BUF2, HDFProcNames[j] );
              strcat( BUF2, " Procedure Summary");
              strcpy( BUF1, BUF2 );
              strcat( BUF1, " Trace");
	      FAMILY = HDF_SUMMARY_FAMILY + (j + 1)*8;
              _hdfDescriptorRT( BUF1, BUF2, FAMILY );
           }
        }
        return;
}
/*======================================================================*
// This routine prints the Pablo IDs assigned to named HDF identifiers  *
//======================================================================*/
void printFileMappingsRT( char *mapFile, char **Names, int nPabloIDs )
{
	int i;
	FILE *ptr;
	ptr = fopen( mapFile, "w" );
 
        if ( ptr == NULL ) {
	    fprintf(stderr,
                    "Couldn't open map file %s - none created.\n",mapFile);
            return;
        }
 
	fprintf(ptr,"\n\nPablo ID to HDF Name mappings:\n");
	fprintf(ptr,"------------------------------\n");
        for ( i = 1; i <= nPabloIDs; i++ ) {
	     fprintf(ptr,"%4d %s\n",i,Names[i] );
	}
	fprintf(ptr,"\n\n");
	fclose( ptr );
}
/************************************************************************/
/*	_hdfNameDescriptor	     					*/
/*	   Generate a SDDF binary format record descriptor for the	*/
/*	   named identifiers used during execution.              	*/
/************************************************************************/
void _hdfNameDescriptor( void )
{
    static char recordBuffer[ 4096 ];
    int         recordLength;

#ifdef DEBUG
	fprintf( debugFile, "_hdfExitTraceDescriptor entered\n" );
	fflush( debugFile );
#endif /* DEBUG */
    hdfRecordPointer = recordBuffer;
    /********************************************************************/
    /* Allow space at the beginning of the record for the packet        */
    /*length which will be computed after the packet is complete.       */
    /********************************************************************/
    sddfWriteInteger( &hdfRecordPointer, 0 );
    /********************************************************************/
    /* The record type, tag, and name                                   */
    /********************************************************************/
    sddfWriteInteger( &hdfRecordPointer, PKT_DESCRIPTOR );
    sddfWriteInteger( &hdfRecordPointer, ( FAMILY_NAME ) );
    sddfWriteString( &hdfRecordPointer, "HDF Name Identifier Record" );
    /********************************************************************/
    /* The record attribute count and string pair                       */
    /********************************************************************/
    sddfWriteInteger( &hdfRecordPointer, 1 );
    sddfWriteString( &hdfRecordPointer, "description" );
    sddfWriteString( &hdfRecordPointer, "HDF Name Identifier Record" );
    /********************************************************************/
    /* The record field count                                           */
    /********************************************************************/
    sddfWriteInteger( &hdfRecordPointer, 3);
    /********************************************************************/
    /* Create fields                                               	*/
    /********************************************************************/
    WRITE_HDF_FIELD(  "Identifier Type", 
		      "Data Set Type", 
		      "Data Set Identifier Type", 
		      INTEGER, 0 );
    WRITE_HDF_FIELD2( "HDF ID",
                      "HDF ID", "File, Data Set or Dim Identifier number",
                      "0", "No HDF ID specified",
                      INTEGER, 0 ); 
    WRITE_HDF_FIELD( "HDF Name",
                     "HDF Name", "Name of File, Data Set or Dim",
                      CHARACTER, 1 );

    recordLength = (int)(hdfRecordPointer - recordBuffer);

    hdfRecordPointer = recordBuffer;
    sddfWriteInteger( &hdfRecordPointer, recordLength );

    putBytes( recordBuffer, (unsigned) recordLength );
}
