/*
 * This file is an extension to NCSA HDF to enable the use of the
 * Pablo trace library.
 *
 * Developed by: The TAPESTRY Parallel Computing Laboratory
 *		 University of Illinois at Urbana-Champaign
 *		 Department of Computer Science
 *		 1304 W. Springfield Avenue
 *		 Urbana, IL	61801
 *
 * Copyright (c) 1995
 * The University of Illinois Board of Trustees.
 *      All Rights Reserved.
 *
 * PABLO is a registered trademark of
 * The Board of Trustees of the University of Illinois
 * registered in the U.S. Patent and Trademark Office.
 *
 * Author: George Xin Zhou (xzhou@cs.uiuc.edu)
 * Contributing Author: Jonathan M. Reid (jreid@cs.uiuc.edu)
 *
 * Project Manager and Principal Investigator:
 *	Daniel A. Reed (reed@cs.uiuc.edu)
 *
 * Funded by: National Aeronautics and Space Administration under NASA
 * Contracts NAG-1-613 and USRA 5555-22 and by the Advanced Research
 * Projects Agency under ARPA contracts DAVT63-91-C-0029 and
 * DABT63-93-C-0040.
 *
 */
/*======================================================================*
// File:  PabloHDF_SDDF.c						*
// Purpose: support use of Pablo trace library to analyze HDF 		*
// performance								*
// Contents:							 	*
// HDFinitTrace_SDDF:   initialize SDDF tracing	 	 		*
// HDFendTrace_SDDF:    end SDDF tracing		 		*	
// startHDFtraceEvent:  record start of HDF procedure    		*
// endHDFtraceEvent:    record end of HDF proc	 			*
// preInitHDFProcTrace: called by HDFinitTrace_SDDF to set up SDDF 	*
//                      interface function calls			*
// initHDFProcTrace:    called by HDFinitTrace_SDDF to initialize data 	*
//		        structures used in tracing HDF procedures.	*
// writeHDFProcRecordDescriptors:       				*
//	                generates the record descriptors for the HDF	*
//	                procedure entry/exit event families.  		*
// HDFprocEventRecord:  generates trace records for events which are	*
//   	                to produce procedure entry or exit event family *
//			trace records.					*
// findHDFProcEvent:    retruns procedure entry/exit index		*
// _hdfTraceEntryDescriptor:						* 
//	                Generate a SDDF binary format record descriptor *
//                      for HDF procedure entries 			*
// _hdfTraceExitDescriptor:	     					* 
//	                Generate a SDDF binary format record descriptor *
//                      for the	HDF procedure exits			*
//======================================================================*/
#include <stdio.h>

#ifdef HAVE_PARALLEL
#include "mpi.h"
#endif

#undef HAVE_PABLO
#include "H5private.h" 
#define HAVE_PABLO
#include "H5config.h"
#include "ProcIDs.h"

#include "SystemDepend.h"
#include "SDDFparam.h"
#include "TraceParam.h"
#include "Trace.h"
#include "HDFTrace.h"
void HDFendTrace_SDDF(void);
void startHDFtraceEvent(int eventID);
void endHDFtraceEvent(int , int , char *, int );
int preInitHDFProcTrace( void );
int initHDFProcTrace( int , int * );
int  writeHDFProcRecordDescriptors( void );
int findHDFProcEvent( int ) ;
TR_RECORD *HDFprocEventRecord( int, TR_EVENT *, CLOCK, HDFsetInfo *, unsigned );
TR_RECORD *miscEventRecord( int , TR_EVENT *, CLOCK, void *, unsigned );
void _hdfTraceEntryDescriptor( void );
void _hdfTraceExitDescriptor( void );
void _hdfMiscDescriptor( void );
void _hdfProcNameDescriptor( void );
int setEventRecordFunction( int, void * );
void HDFtraceIOEvent( int, void *, unsigned );
void initIOTrace( void );
void enableIOdetail( void );
void disableLifetimeSummaries( void );
void disableTimeWindowSummaries( void );
void disableFileRegionSummaries( void );

void initIOTrace( void );
void endIOTrace( void );
#define PABLO 1
/* on the ipsc/860 we don't include unistd.h */
#ifndef __NX
#include <unistd.h>
#endif

#define returnRecord(x)    return x;

#ifdef HAVE_MPIOTRACE
	int initMPIOTrace( char *, int );
	void endMPIOTrace( void ) ;
#endif
extern char *hdfRecordPointer;
/*======================================================================*
// Prototypes of functions in this file.				*
//======================================================================*/
void HDFinitTrace_SDDF( char *, uint32 );
/*======================================================================* 
// Each procedure being traced has associated with it a distinct pair 	* 
// of entry and exit event IDs.  This code maintains a vector of such  	* 
// matchings which permits the ready identification of an event ID as  	* 
// being either an entry event or an exit event and for which procedure.* 
//======================================================================*/
typedef struct procEventMatch {
	int			entryID;  /* procedure entry event ID 	*/
	int			exitID;	  /* procedure exit event ID  	*/
} PROC_EVENTS;

static PROC_EVENTS	*procEvents =	   /* array of event ID pairs 	*/
			(PROC_EVENTS *) 0;
/*======================================================================* 
// For each procedure being traced this code maintains a stack of	* 
// procedure entry times.  Each procedure entry event causes the	* 
// corresponding procedure's stack to be pushed, each procedure exit	* 
// event causes the corresponding procedure's stack to be popped, and	* 
// from the difference in time between entry and exit the procedure	* 
// duration may be calculated in a very straightforward subtraction.  	* 
// The above procedure entry-exit event ID matching is used to map 	* 
// events to their corresponding procedures.  In addition, the 		* 
// cumulative total of these procedure durations is maintained for all 	* 
// traced subprocedures	of each traced procedure.  That is, when a 	* 
// traced procedure exits, it increases this duration sum for its most 	* 
// immediate traced ancestor procedure.  By subtracting this 		* 
// subprocedure duration sum from the traced procedure's inclusive 	* 
// duration, we arrive at the exclusive duration of the procedure.	* 
//======================================================================*/
typedef struct procEntryTime {
	CLOCK			entryTime;	/* when proc entered 	*/
	CLOCK			subProcTime;	/* subproc duration    	*/
	struct procEntryTime	*nextTime;	/* stack pointer down	*/
	struct procEntryTime	*ancestorProc;	/* traced ancestor	*/
} PROC_ENTRY;

/*
static PROC_ENTRY	**procEntryStack =*/	/* array of pointers to	*/
/*	(PROC_ENTRY **) 0;*/	/* stack top elements	*/
/*======================================================================*
// Define data structure types for procedure entry and exit trace 	* 
// records, similarly to record data structures in Trace.h	 	* 
//======================================================================*/

/*======================================================================*
// FAMILY_PROCENTRY family Record Data packets:				* 
//======================================================================*/
struct procEntryTraceRecordData {
	int	packetLength;	/* bytes in packet		    	*/
	int	packetType;	/* == PKT_DATA			    	*/
	int	packetTag;	/* FAMILY_PROCENTRY | RECORD_TRACE  	*/
	int	eventID;	/* ID of corresponding event	    	*/
	double	seconds;	/* floating-point timestamp	    	*/
	long	sourceByte;	/* source code byte offset in file  	*/
	int	sourceLine;	/* source code line number in file  	*/
	int	nodeNumber;	/* occurred on which node	    	*/
};
#define procEntryTraceLen 6*sizeof(int) + sizeof(long) + sizeof(double)
/*======================================================================* 
// FAMILY_PROCEXIT family Record Data packets:				* 
//======================================================================*/
struct procExitTraceRecordData {
	int	packetLength;	   /* bytes in packet		    	*/
	int	packetType;	   /* == PKT_DATA		    	*/
	int	packetTag;	   /* FAMILY_PROCEXIT | RECORD_TRACE   	*/
	int	eventID;	   /* ID of corresponding event	    	*/
	double	seconds;	   /* floating-point timestamp	    	*/
	long	setID;	           /* index of file | Data Set accessed	*/
	int	nodeNumber;	   /* occurred on which node	    	*/
	int	nameLen;	   /* Length of file or data set name	*/
	/* name comes next, but no space is allocated	*/
};
#define procExitTraceLen 6*sizeof(int) + 3*sizeof(double) +sizeof(long)
/*======================================================================*
// misc Record Data packets:						* 
//======================================================================*/
struct miscTraceRecordData {
	int	packetLength;	/* bytes in packet		    	*/
	int	packetType;	/* == PKT_DATA			    	*/
	int	packetTag;	/* FAMILY_MISC | RECORD_TRACE  		*/
	int	eventID;	/* ID of corresponding event	    	*/
	double	seconds;	/* floating-point timestamp	    	*/
	double	duration;	/* floating-point operation duration	*/
	unsigned long	bytes;	/* number of bytes requested        	*/
	int	nodeNumber;	/* occurred on which node	    	*/
};
#define miscTraceLen 5*sizeof(int) + 2*sizeof(double) +sizeof(long)
/*======================================================================*
// HDFprocName Record Data packets:					* 
// These are used to pass information about the names of the traced	*
// routine in the trace file to the post processing utilities.		*
//======================================================================*/
struct HDFprocNameRecordData {
	int	packetLength;	/* bytes in packet		    	*/
	int	packetType;	/* == PKT_DATA			    	*/
	int	packetTag;	/* FAMILY_HDFPROCNAME | RECORD_TRACE	*/
	int	eventID;	/* ID of corresponding event	    	*/
	int	HDFeventID;	/* ID of HDF proc               	*/
	int	procIndex;	/* Index of HDF proc            	*/
	int	numProcs;	/* Number of HDF procs          	*/
	int	NameLen;	/* length of HDF proc Name	    	*/
	char   *Name;
};
#define HDFprocNameRecLen 8*sizeof(int)
/*======================================================================*
// Define data structures used to contain source code location data for *
// Pablo instrumenting parser-generated code.				*
//======================================================================*/
static long	procByteOffset = -1;	/* source code byte offset    	*/

static int	procLineNumber = -1;	/* source code line number 	*/

/*======================================================================*
// The procEntries array specifies the event IDs of procedure entry 	*
// events. 								*
//======================================================================*/
int procEntries[] = {
#include "HDFidList.h"
ID_HDF_Last_Entry
};
/*======================================================================*
// The procEntryCalled array indicates whether or not a procedure entry *
// was called.					*
//======================================================================*/
int *procEntryCalled;
/*======================================================================*
// The HDFProcNames array holds the names of the HDF entries.       	*
//======================================================================*/
static char HDFProcNames[][40] = {
#include "HDFentryNames.h"
"HDF_LAST_ENTRY"
};
/*=======================================================================
// NAME									*
//     	HDFinitTrace_SDDF -- initalize HDF tracing with SDDF records	*
// USAGE								*
//     	HDFinitTrace_SDDF( traceFileName, procTraceMask )		*
// PARAMETERS								*
//	char *traceFileName  -- name of trace file to hold output	*
//	uint32 prcoTraceMask -- mask indicating classes of HDF procs to *
//				be traced				*
// RETURNS								*
//     	None 								*
//======================================================================*/
void HDFinitTrace_SDDF( char *traceFileName, uint32 procTraceMask )
{
	/*===============================================================
        // set traceFileName and set IO tracing switches.  If MPIO 	*
	// tracing is available, this will be initialized also.  	*
	//==============================================================*/
#ifdef HAVE_PARALLEL
	/*===============================================================
	// in the parallel case, initialize MPI-IO tracing.  This will	*
	// initialize the traceFileName and set the I/O tracing 	*
	// switches.							*
	//==============================================================*/
        MPI_Comm_rank( MPI_COMM_WORLD, &myNode );
	setTraceProcessorNumber( myNode );
#ifdef HAVE_MPIOTRACE
        initMPIOTrace( traceFileName, 0 ); 
#else 
	buff = (char *)malloc( strlen(traceFileName)+12);
	sprintf( buff, "%s.nd%.4d\0",traceFileName,myNode);
	setTraceFileName( buff );
	free( buff );
#endif
#else 
	/*===============================================================
	// in the non-parallel case, set the trace file name and the 	*
	// I/O tracing switches.					*
	//==============================================================*/
	setTraceFileName(traceFileName);
    	initIOTrace();
    	enableIOdetail();
    	disableLifetimeSummaries();
        disableTimeWindowSummaries();
	disableFileRegionSummaries();
#endif /* HAVE_PARALLEL */
	/*===============================================================
	// complete HDF initiailization.				*
	//==============================================================*/
	preInitHDFProcTrace();
        initHDFProcTrace( sizeof(procEntries)/sizeof(int), procEntries );
    	procTrace = procTraceMask;
}
/*=======================================================================
// NAME									*
//     HDFendTrace_SDDF -- end HDF tracing				*
// USAGE								*
//     HDFendTrace_SDDF()      						*
// RETURNS								*
//     None.								*
//======================================================================*/
void HDFendTrace_SDDF(void)
{
	HDFtraceIOEvent( -ID_timeStamp, 0, 0 ); 
#ifdef HAVE_MPIOTRACE
	/*===============================================================
	// termintate MPI-IO tracing in the parallel case.  This will	*
	// terminate the I/O tracing and close tracing as well.		*
	//==============================================================*/
	endMPIOTrace(); 
#else
	/*===============================================================
	// terminate tracing 						*
	//==============================================================*/
       	endIOTrace();
       	endTracing();
#endif
}
/*=======================================================================
// NAME									*
//   initHDFProcTrace:							*
//     This function initializes data structures specific to		* 
//     the procedure entry/exit tracing extensions of the Pablo		* 
//     instrumentation library.  The argument numProcs specifies	* 
//     how many procedures are to be traced.  The argument procEntryID	* 
//     is a vector specifying the event IDs to be use as entry events	*
//     for each of the procedures.  The negative value is used for 	*
//     the exit event ID.  						*
// USAGE								*
//   result = initHDFProcTrace(numProcs,procEntryID)			*
// PARAMETERS								*
//   int numProcs     -- number of Procedures to be initialized		*
//   int *procEntryID -- array of id entry codes for these procedures	*
// RETURNS								*
//   SUCCESS or FAILURE							*
//======================================================================*/
int initHDFProcTrace( int numProcs, int *procEntryID )
{
	int			procIndex;

  	if (( numProcs <= 0 ) || ( procEntryID == (int *) 0 )  )
		return FAILURE; 
	/*===============================================================
	// Allocate space to store a copy of the procedure entry-exit	*
	// event ID matchings and also the procedure entry stacks.	*
	//==============================================================*/
	procEvents = (PROC_EVENTS *) TRgetBuffer(
					   (numProcs+4)*sizeof(PROC_EVENTS) );

	if ( procEvents == (PROC_EVENTS *) 0 )
		TRfailure( "cannot allocate procedure events matching" );

	procEntryCalled = ( int *)malloc( numProcs*sizeof(int) );
	if ( procEvents == NULL )
	   TRfailure( "cannot allocate procedure Called indicators" );
	/*===============================================================
	// Initialize the procedure events matching from the arguments	*
	// passed.  Configure the trace record-generating function for  *
	// these events.  Initialize the flags indicating whether or	*
	// not the procedure was called.				*
	//==============================================================*/
	for ( procIndex = 0; procIndex < numProcs; procIndex++ ) {

		procEvents[ procIndex ].entryID = procEntryID[ procIndex ];
		procEvents[ procIndex ].exitID = -procEntryID[ procIndex ];

		setEventRecordFunction( procEntryID[ procIndex ],
					HDFprocEventRecord );
		setEventRecordFunction( -procEntryID[ procIndex ],
					HDFprocEventRecord );
		procEntryCalled[ procIndex ] = 0;

	}

	/*===============================================================
	// Initialize the procedure events for malloc.  		*
	// Configure the trace record-generating function for this  	*
	// event.							*
	//==============================================================*/
	procEvents[ numProcs ].entryID = ID_malloc;
	procEvents[ numProcs ].exitID = -ID_malloc;
	setEventRecordFunction( ID_malloc, miscEventRecord );
	setEventRecordFunction( -ID_malloc, miscEventRecord );
	procEvents[ numProcs+1 ].entryID = ID_free;
	procEvents[ numProcs+1 ].exitID = -ID_free;
	setEventRecordFunction( ID_free, miscEventRecord );
	setEventRecordFunction( -ID_free, miscEventRecord );
	procEvents[ numProcs+2 ].entryID = ID_timeStamp;
	procEvents[ numProcs+2 ].exitID = -ID_timeStamp;
	setEventRecordFunction( ID_timeStamp, miscEventRecord );
	setEventRecordFunction( -ID_timeStamp, miscEventRecord );

	return SUCCESS;
}
/*=======================================================================
// NAME									*
//   preInitHDFProcTrace:						*
//   	This function calls the trace library interface function	* 
//	setRecordDescriptor, which records the address of the		* 
//	procedure that generates the record descriptors for the		* 
//	procedure trace event families.  It is automatically		* 
//	invoked by HDFinitTrace_SDDF. 					*
// USAGE								*
//   result = preInitHDFProcTrace();					*
// RESULT								*
//   SUCCESS or FAILURE							*
/=======================================================================*/
int preInitHDFProcTrace( void )
{
	static int	preInitDone = FALSE;

	if ( preInitDone == TRUE )
		return SUCCESS;
	/*===============================================================
	// Give the instrumentation library a pointer to the functions	*
	// in which we output the specialized record descriptors for	*
	// procedure entry/exit.					*
        //==============================================================*/
	setRecordDescriptor( writeHDFProcRecordDescriptors );

	preInitDone = TRUE;
	return SUCCESS;
}
/*=======================================================================
// NAME									*
//   writeHDFProcRecordDescriptors:					*
//	   This function generates the record descriptors for the HDF	*
//	   procedure entry/exit event families.  It will be invoked	*
//	   by the instrumentation library initialization routines.	*
//	   Patterned after instrumentation library internal function	*
//	   writeRecordDescriptors.					*
// USAGE								*
//   result = writeHDFProcRecordDescriptors();				*
// RESULT								*
//   SUCCESS 								*
/=======================================================================*/
int  writeHDFProcRecordDescriptors( void )
{
#ifdef DEBUG
	fprintf( debugFile, "writeHDFProcRecordDescriptors\n" );
	fflush( debugFile );
#endif /* DEBUG */

	_hdfMiscDescriptor();
	_hdfTraceEntryDescriptor() ;
	_hdfTraceExitDescriptor()  ;
	_hdfProcNameDescriptor();

#ifdef DEBUG
	fprintf( debugFile, "writeHDFProcRecordDescriptors done\n" );
	fflush( debugFile );
#endif /* DEBUG */
	return SUCCESS;
}
/*=======================================================================
// NAME									*
//   HDFprocEventRecord:						*
// 	This function generates trace records for events which are	*
//   	to produce procedure entry or exit event family trace records.	*
//   	Patterned after the instrumentation library internal functions	*
//	externalEventRecord and sddfRecord.				*
// USAGE								*
//   REC = HDFprocEventRecord( recordType, eventPointer, timeStamp,	*
//                             dataPointer, dataLength)			*
// PARAMETERS								*
//   int recordType 	    -- type of event record			*
//   TR_EVENT eventPointer  -- pointer to event data structure		*
//   CLOCK timeStamp	    -- time event is recorded			*
//   HDFsetInfo dataPointer -- information about HDF data set accessed	*
//   unsigned dataLength    -- dummy for compatability			*
// RETURNS								*
//   pointer to trace record for this event 				*
//======================================================================*/
TR_RECORD *
HDFprocEventRecord( int recordType, TR_EVENT *eventPointer, CLOCK timeStamp,
		    HDFsetInfo *dataPointer, unsigned dataLength )
{
	static TR_RECORD		traceRecord;
	static void			*recordBuffer = NULL;
	static int			bufferLength = 0;
	struct procEntryTraceRecordData	*entryTraceRecordHeader;
	struct procExitTraceRecordData	*exitTraceRecordHeader;
	struct HDFprocNameRecordData 	*nameRecord;
	int				procIndex;
	int				recordFamily;
	char				*namePtr;
	int				NameLen;
	int				NameRecLen;
	
#ifdef DEBUG
	fprintf( debugFile, "HDFprocEventRecord\n" );
	fflush( debugFile );
#endif /* DEBUG */

	/*==============================================================* 
	// Find the index in the tables for the procedure corresponding *
	// to this eventID.						*
	//==============================================================*/
	procIndex = findHDFProcEvent( eventPointer->eventID );
	if ( procIndex < 0 ) {
	   return nullRecordFunction( recordType, eventPointer,
				timeStamp, (char *)dataPointer, dataLength );
	}
	/*==============================================================* 
	// Produce a packet for the name of the procedure if one has 	*
	// not already been produced.					*
	//==============================================================*/
	if ( procEntryCalled[procIndex] == 0 ) {
	   NameLen = strlen( HDFProcNames[procIndex] );
	   NameRecLen = HDFprocNameRecLen + NameLen;
	   nameRecord = ( struct HDFprocNameRecordData *)malloc( NameRecLen );
	   nameRecord->packetLength = NameRecLen;
	   nameRecord->packetType  = PKT_DATA;
	   nameRecord->packetTag   = FAMILY_HDFPROCNAME | RECORD_TRACE;
	   nameRecord->eventID     = ID_HDFprocName;
	   nameRecord->HDFeventID  = abs(eventPointer->eventID);
	   nameRecord->procIndex   = procIndex;
	   nameRecord->numProcs    = NumHDFProcs;
	   nameRecord->NameLen     = NameLen;
	   /*===========================================================*
	   // copy procedure name into the packet, write out the packet	*
	   // and set ProcEntryCalled[procIndex] to indicate the name	*
	   // packet was produced.					*
	   //===========================================================*/
	   memcpy( &(nameRecord->Name), HDFProcNames[procIndex], NameLen );
	   putBytes( (char *)nameRecord , NameRecLen );
	   free( nameRecord );
	   procEntryCalled[procIndex] = 1;
	}
	/*==============================================================* 
	// Determine whether this is a procedure entry or a procedure 	* 
	// exit family event by lookup in the procedure event ID 	* 
	// matching.  							* 
	//==============================================================*/
	if ( procEvents[ procIndex ].entryID == eventPointer->eventID ) {
	   recordFamily = FAMILY_PROCENTRY;
	} else {
	   recordFamily = FAMILY_PROCEXIT;
	}
	/*==============================================================* 
	// The time stamp stored in the event descriptor will be used	*  
	// unless one is specified in the timeStamp parameter.	    	*  
	//==============================================================*/
	if ( clockCompare( timeStamp, noSuchClock ) == 0 ) {
	   timeStamp = eventPointer->eventLast;
	}
	/*==============================================================* 
	// Determine how many bytes of storage will be needed for the 	*  
	// contents of the trace record.			    	* 
	//==============================================================*/
	switch (( recordFamily | recordType )) {

	   case FAMILY_PROCENTRY | RECORD_TRACE:
		traceRecord.recordLength = sizeof *entryTraceRecordHeader;
		break;

	   case FAMILY_PROCEXIT | RECORD_TRACE:
		traceRecord.recordLength = sizeof *exitTraceRecordHeader;
		break;
	}
        if ( dataPointer != NULL && dataPointer->setName != NULL ) {
	   traceRecord.recordLength += strlen( dataPointer->setName );
        }
	/*==============================================================* 
	// If there is a previously-allocated buffer and its size will	* 
	// hold this record, re-use the buffer.  Otherwise, deallocate	* 
	// the buffer (if allocated) and allocate a bigger one.	    	* 
	//==============================================================*/
	if ( bufferLength < traceRecord.recordLength ) {

	   if ( recordBuffer != NULL ) {
	      TRfreeBuffer( recordBuffer );
	   }

	   recordBuffer = (char *)TRgetBuffer( traceRecord.recordLength );

	   if ( recordBuffer == NULL ) {
	      TRfailure( "cannot allocate storage for trace record" );
	   }
	   bufferLength = traceRecord.recordLength;
	}

	traceRecord.recordContents = recordBuffer;
	/*==============================================================* 
	// Load the trace record fields into the allocated buffer 	* 
	//==============================================================*/
	switch (( recordFamily | recordType )) {

	   case FAMILY_PROCENTRY | RECORD_TRACE:
		entryTraceRecordHeader = (struct procEntryTraceRecordData *)
							recordBuffer;
		entryTraceRecordHeader->packetLength =
				traceRecord.recordLength;
		entryTraceRecordHeader->packetType = PKT_DATA;
		entryTraceRecordHeader->packetTag = recordFamily | recordType;
		entryTraceRecordHeader->seconds = clockToSeconds( timeStamp );
		entryTraceRecordHeader->eventID = eventPointer->eventID;
		entryTraceRecordHeader->nodeNumber = TRgetNode();
		entryTraceRecordHeader->sourceByte = procByteOffset;
		entryTraceRecordHeader->sourceLine = procLineNumber;
		break;

	   case FAMILY_PROCEXIT | RECORD_TRACE:
		exitTraceRecordHeader = (struct procExitTraceRecordData *)
							recordBuffer;
		exitTraceRecordHeader->packetLength =
				                  traceRecord.recordLength;
		exitTraceRecordHeader->packetType = PKT_DATA;
		exitTraceRecordHeader->packetTag = recordFamily | recordType;
		exitTraceRecordHeader->seconds = clockToSeconds( timeStamp );
		exitTraceRecordHeader->eventID = eventPointer->eventID;
		exitTraceRecordHeader->nodeNumber = TRgetNode();

		if ( dataPointer != 0 ) {
	           exitTraceRecordHeader->setID = dataPointer->setID;
		   if (dataPointer->setName != NULL ) {
		      exitTraceRecordHeader->nameLen 
					= (int)strlen( dataPointer->setName );
		      /*================================================* 
		      // copy name directly into the end of the buffer.	*
		      //================================================*/
		      namePtr = (char *)exitTraceRecordHeader 
                                                  + procExitTraceLen;
	              memcpy( namePtr, dataPointer->setName,
		              exitTraceRecordHeader->nameLen );
	           } else {
		      exitTraceRecordHeader->nameLen = 0;
	           }   
		} else {
	           exitTraceRecordHeader->setID = NoDSid;
		   exitTraceRecordHeader->nameLen = 0;
		} 
		break;
	}

#ifdef DEBUG
	fprintf( debugFile, "HDFprocEventRecord done\n" );
	fflush( debugFile );
#endif /* DEBUG */
	returnRecord(&traceRecord);
}
/*======================================================================*
// Internal Routine:  miscEventRecord					*
//	  Called for misc start and end events.		       		*
//======================================================================*/
TR_RECORD *miscEventRecord( int recordType, 
			      TR_EVENT *eventPointer, 
                              CLOCK timeStamp, 
			      void *dataPointer, 
			      unsigned dataLength )
{
    static TR_RECORD traceRecord;
    static struct miscTraceRecordData miscRecord;
    static int initialized = FALSE;
    int eventID;

    if ( clockCompare( timeStamp, noSuchClock ) == 0 ) {
        timeStamp = eventPointer->eventLast;
    }

    eventID =  eventPointer->eventID;
    if ( ! initialized ) {
       miscRecord.packetLength = sizeof( miscRecord );
       miscRecord.packetType = PKT_DATA;
       miscRecord.packetTag = FAMILY_MISC | RECORD_TRACE;
       miscRecord.nodeNumber = traceProcessorNumber;

       traceRecord.recordLength = miscRecord.packetLength;
       traceRecord.recordContents = (char *) &miscRecord;
       initialized = TRUE;
    }

    switch ( eventID ) {
       case ID_malloc:
       case ID_free:
	  miscRecord.seconds = clockToSeconds( timeStamp ) ;
	  miscRecord.bytes = *(size_t *)dataPointer;
          miscRecord.eventID = eventID ;
	  break;
       case -ID_malloc:
       case -ID_free:
          miscRecord.duration = clockToSeconds( timeStamp) 
                                      - miscRecord.seconds;
          return &traceRecord;		/* generate trace record */
	  break;
       case ID_timeStamp:
       case -ID_timeStamp:
	  miscRecord.seconds = clockToSeconds( timeStamp ) ;
	  miscRecord.bytes = 0;
          miscRecord.eventID = eventID ;
          miscRecord.duration = 0;
          return &traceRecord;	
	  break;
       default:
          fprintf( stderr, "miscEventRecord: unknown eventID %d\n", eventID );
	  break;
    }
    /*==================================================================*
    // If we get here then no trace record generated.  Normally we 	*	
    // should get here if this is an entry call.			*
    //==================================================================*/
    return( nullRecordFunction( recordType, eventPointer, timeStamp,
				            dataPointer, dataLength ) );
}
/*======================================================================*
// NAME									*
//   findHDFProcEvent:							*
//	   Search the procedure entry/exit event ID matching data	*
//	   structure for an event ID (either entry or exit) which is	*
//	   the same as the argument eventID.  If found, return the	*
//	   index from that table, which will be between 0 and		*
//	   numberProcedures - 1, inclusive.  If not found, return -1;	*
// USAGE								*
//   index = findHDFProcEvent						*
// RETURNS								*
//   index of the procedure corresponding to this ID			*
//======================================================================*/
int findHDFProcEvent( int eventID )
{
	int	procIndex;

#ifdef DEBUG
	fprintf( debugFile, "findHDFProcEvent\n" );
	fflush( debugFile );
#endif /* DEBUG */
        if ( isBeginHDFEvent(eventID) ) {
	   procIndex = eventID - BEGIN_HDF;
        } else if ( isEndHDFEvent( eventID ) ) {
	   procIndex = -eventID - BEGIN_HDF;
	} else {
	   procIndex = -1 ;
	}
	return procIndex;
}
/*======================================================================*
// NAME									*
//	_hdfTraceEntryDescriptor					* 
//	   Generate a SDDF binary format record descriptor for the	* 
//	   full trace class of events in the HDF procedure entry 	* 
// USAGE								*
//      _hdfTraceEntryDescriptro()					*
// RETURNS								*
//      void								*
//======================================================================*/
void _hdfTraceEntryDescriptor( void )
{
    static char recordBuffer[ 4096 ];
    int         recordLength;

#ifdef DEBUG
	fprintf( debugFile, "_hdfTraceEntryDescriptor entered\n" );
	fflush( debugFile );
#endif /* DEBUG */
    hdfRecordPointer = recordBuffer;
    /*==================================================================* 
    // Allow space at the beginning of the record for the packet        *
    //length which will be computed after the packet is complete.       *
    //==================================================================*/
    sddfWriteInteger( &hdfRecordPointer, 0 );
    /*==================================================================* 
    // The record type, tag, and name                                   * 
    //==================================================================*/
    sddfWriteInteger( &hdfRecordPointer, PKT_DESCRIPTOR );
    sddfWriteInteger( &hdfRecordPointer, ( FAMILY_PROCENTRY | RECORD_TRACE ) );
    sddfWriteString( &hdfRecordPointer, "HDF Procedure Entry Trace" );
    /*==================================================================*
    // The record attribute count and string pair                       *
    //==================================================================*/
    sddfWriteInteger( &hdfRecordPointer, 1 );
    sddfWriteString( &hdfRecordPointer, "description" );
    sddfWriteString( &hdfRecordPointer, "HDF Procedure Entry Trace Record" );
    /*==================================================================*
    // The record field count                                           * 
    //==================================================================*/
    sddfWriteInteger( &hdfRecordPointer, 5);
    /*==================================================================* 
    // Create fields                                               	*
    //==================================================================*/
    WRITE_HDF_FIELD( "Event Identifier", 
		     "Event ID", 
		     "Event Identifier Number", 
		     INTEGER, 0 );
    WRITE_HDF_FIELD( "Seconds", 
		     "Seconds", 
		     "Floating Point Timestamp", 
		     DOUBLE, 0 );
    WRITE_HDF_FIELD( "Source Byte", 
		     "Byte", 
		     "Source Byte Offset", 
		     LONG, 0 );
    WRITE_HDF_FIELD( "Source Line", 
		     "Line", 
		     "Source Line Number",
		     INTEGER, 0 );
    WRITE_HDF_FIELD( "Processor Number", 
		     "Node", 
		     "Processor number", 
		     INTEGER, 0 );

    recordLength = (int)(hdfRecordPointer - recordBuffer);

    hdfRecordPointer = recordBuffer;
    sddfWriteInteger( &hdfRecordPointer, recordLength );

    putBytes( recordBuffer, (unsigned) recordLength );
}
/*======================================================================*
// NAME									*
//	_hdfTraceExitDescriptor	     					* 
//	   Generate a SDDF binary format record descriptor for the	* 
//	   full trace class of events in the HDF procedure exit  	* 
// USAGE								*
//	_hdfTraceExitDescriptor()					*	
// RETURNS								*
//  	void								*
//======================================================================*/
void _hdfTraceExitDescriptor( void )
{
    static char recordBuffer[ 4096 ];
    int         recordLength;

#ifdef DEBUG
	fprintf( debugFile, "_hdfExitTraceDescriptor entered\n" );
	fflush( debugFile );
#endif /* DEBUG */
    hdfRecordPointer = recordBuffer;
    /*==================================================================* 
    // Allow space at the beginning of the record for the packet        * 
    // length which will be computed after the packet is complete.      * 
    //==================================================================*/
    sddfWriteInteger( &hdfRecordPointer, 0 );
    /*==================================================================* 
    // The record type, tag, and name                                   * 
    /===================================================================*/
    sddfWriteInteger( &hdfRecordPointer, PKT_DESCRIPTOR );
    sddfWriteInteger( &hdfRecordPointer, ( FAMILY_PROCEXIT | RECORD_TRACE ) );
    sddfWriteString( &hdfRecordPointer, "HDF Procedure Exit Trace" );
    /*==================================================================*
    // The record attribute count and string pair                       *
    //==================================================================*/
    sddfWriteInteger( &hdfRecordPointer, 1 );
    sddfWriteString( &hdfRecordPointer, "description" );
    sddfWriteString( &hdfRecordPointer, "HDF Procedure Exit Trace Record" );
    /*==================================================================* 
    // The record field count                                           *
    //==================================================================*/
    sddfWriteInteger( &hdfRecordPointer, 5);
    /*==================================================================* 
    // Create fields                                               	* 
    //==================================================================*/
    WRITE_HDF_FIELD(  "Event Identifier", 
		      "Event ID", 
		      "Event Identifier Number", 
		      INTEGER, 0 );
    WRITE_HDF_FIELD(  "Seconds", 
		      "Seconds", 
		      "Floating Point Timestamp", 
		      DOUBLE, 0 );
    WRITE_HDF_FIELD2( "HDF ID",
                      "HDF ID", "File, Data Set or Dim Identifier number",
                      "0", "No HDF ID specified",
                      LONG, 0 ); 
    WRITE_HDF_FIELD(  "Processor Number", 
		      "Node", 
		      "Processor number", 
		      INTEGER, 0 );
    WRITE_HDF_FIELD( "HDF Name",
                     "HDF Name", "Name of File, Data Set or Dim",
                      CHARACTER, 1 );

    recordLength = (int)(hdfRecordPointer - recordBuffer);

    hdfRecordPointer = recordBuffer;
    sddfWriteInteger( &hdfRecordPointer, recordLength );

    putBytes( recordBuffer, (unsigned) recordLength );
}

/*======================================================================*
// NAME									*
//	_hdfMiscDescriptor						* 
//	   Generate a SDDF binary format record descriptor for the	* 
//	   misc procedure                                        	* 
// USAGE								*
//      _hdfMiscDescriptor()					*
// RETURNS								*
//      void								*
//======================================================================*/
void _hdfMiscDescriptor( void )
{
    static char recordBuffer[ 4096 ];
    int         recordLength;

#ifdef DEBUG
	fprintf( debugFile, "_hdfMiscDescriptor entered\n" );
	fflush( debugFile );
#endif /* DEBUG */
    hdfRecordPointer = recordBuffer;
    /*==================================================================* 
    // Allow space at the beginning of the record for the packet        *
    //length which will be computed after the packet is complete.       *
    //==================================================================*/
    sddfWriteInteger( &hdfRecordPointer, 0 );
    /*==================================================================* 
    // The record type, tag, and name                                   * 
    //==================================================================*/
    sddfWriteInteger( &hdfRecordPointer, PKT_DESCRIPTOR );
    sddfWriteInteger( &hdfRecordPointer, ( FAMILY_MISC | RECORD_TRACE ) );
    sddfWriteString( &hdfRecordPointer, "Misc Trace" );
    /*==================================================================*
    // The record attribute count and string pair                       *
    //==================================================================*/
    sddfWriteInteger( &hdfRecordPointer, 1 );
    sddfWriteString( &hdfRecordPointer, "description" );
    sddfWriteString( &hdfRecordPointer, "Misc Trace Record" );
    /*==================================================================*
    // The record field count                                           * 
    //==================================================================*/
    sddfWriteInteger( &hdfRecordPointer, 5);
    /*==================================================================* 
    // Create fields                                               	*
    //==================================================================*/
    WRITE_HDF_FIELD( "Event Identifier", 
		     "Event ID", 
		     "Event Identifier Number", 
		     INTEGER, 0 );
    WRITE_HDF_FIELD( "Seconds", 
		     "Seconds", 
		     "Floating Point Timestamp", 
		     DOUBLE, 0 );
    WRITE_HDF_FIELD( "Duration", 
		     "Duration", 
		     "Operation Duration", 
		     DOUBLE, 0 );
    WRITE_HDF_FIELD( "Bytes", 
		     "Bytes", 
		     "Bytes Requested", 
		     LONG, 0 );
    WRITE_HDF_FIELD( "Processor Number", 
		     "Node", 
		     "Processor number", 
		     INTEGER, 0 );

    recordLength = (int)(hdfRecordPointer - recordBuffer);

    hdfRecordPointer = recordBuffer;
    sddfWriteInteger( &hdfRecordPointer, recordLength );

    putBytes( recordBuffer, (unsigned) recordLength );
}
/*======================================================================*
// NAME									*
//	_hdfProcNameDescriptor						* 
//	   Generate a SDDF binary format record descriptor for the	* 
//	   HDFProcName Records                                   	* 
// USAGE								*
//      _hdfProcNameDescriptor()					*
// RETURNS								*
//      void								*
//======================================================================*/
void _hdfProcNameDescriptor( void )
{
    static char recordBuffer[ 4096 ];
    int         recordLength;

#ifdef DEBUG
	fprintf( debugFile, "_hdfProcNameDescriptor entered\n" );
	fflush( debugFile );
#endif /* DEBUG */
    hdfRecordPointer = recordBuffer;
    /*==================================================================* 
    // Allow space at the beginning of the record for the packet        *
    //length which will be computed after the packet is complete.       *
    //==================================================================*/
    sddfWriteInteger( &hdfRecordPointer, 0 );
    /*==================================================================* 
    // The record type, tag, and name                                   * 
    //==================================================================*/
    sddfWriteInteger( &hdfRecordPointer, PKT_DESCRIPTOR );
    sddfWriteInteger( &hdfRecordPointer, ( FAMILY_HDFPROCNAME| RECORD_TRACE ) );
    sddfWriteString( &hdfRecordPointer, "HDF Procedure Information" );
    /*==================================================================*
    // The record attribute count and string pair                       *
    //==================================================================*/
    sddfWriteInteger( &hdfRecordPointer, 1 );
    sddfWriteString( &hdfRecordPointer, "description" );
    sddfWriteString( &hdfRecordPointer, "HDF Proc Info Record" );
    /*==================================================================*
    // The record field count                                           * 
    //==================================================================*/
    sddfWriteInteger( &hdfRecordPointer, 5);
    /*==================================================================* 
    // Create fields                                               	*
    //==================================================================*/
    WRITE_HDF_FIELD( "Event Identifier", 
		     "Event ID", 
		     "Event Identifier Number", 
		     INTEGER, 0 );
    WRITE_HDF_FIELD( "HDF Proc Event Id", 
		     "HDF Proc Event Identifier", 
		     "HDF Proc Event Identifier Number", 
		     INTEGER, 0 );
    WRITE_HDF_FIELD( "HDF Proc Index", 
		     "HDF Proc Index", 
		     "Index of HDF Proc in Tables", 
		     INTEGER, 0 );
    WRITE_HDF_FIELD( "Num HDF Procs", 
		     "Num HDF Procs", 
		     "Number of HDF Procedures", 
		     INTEGER, 0 );
    WRITE_HDF_FIELD( "HDF Proc Name",
                     "HDF Proc Name", 
		     "Name of HDF Procedure",
                     CHARACTER, 1 );
    recordLength = (int)(hdfRecordPointer - recordBuffer);

    hdfRecordPointer = recordBuffer;
    sddfWriteInteger( &hdfRecordPointer, recordLength );

    putBytes( recordBuffer, (unsigned) recordLength );
}
/*#endif */ /* HAVE_PABLO */ 
