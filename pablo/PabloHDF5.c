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
//========================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <stdarg.h>
#ifndef fileno
int fileno ( FILE * );
#endif
/* on ipsc/860 don't include this or you'll get multiply defined SEEK_* */
#ifndef __NX
#include <unistd.h>
#endif


#define HDFtrace3OPEN__
int HDFtrace3OPEN( const char *, int, mode_t );

#include "SDDFparam.h"
#include "TraceParam.h"

#include "SystemDepend.h"
#include "Trace.h"

#include "IO_TraceParams.h"
#include "HDFIOTrace.h"

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

/* mode_t is not defined on the ipsc/860 so we define it here */
#ifdef __NX
typedef unsigned int mode_t;
#endif

int OUTPUT_SWITCH = 1;
int *procTrace;
extern void preInitIOTrace( void ); 

#include "ProcTrace.h"
#include "HDF5Trace.h"
#include "IOTrace.h"

#define	ID_HDFprocName		9996
#define	ID_malloc		9997
#define	ID_free			9998
#define	ID_timeStamp		9999
#define	DUMMY_HDF		10000

#ifdef HAVE_PARALLEL
#include "mpio.h"
#include "MPIO_TraceParams.h"
#endif /* HAVE_PARALLEL*/

#ifdef HAVE_MPIOTRACE
#include "MPIO_Init.h"
#include "MPIO_EventArgs.h"
#include "HDFmpioProtos.h"
#endif /* HAVE_MPIOTRACE */

void HDFinitTrace_RT ( const char *, int );
void HDFinitTrace_SDDF ( const char *, int );
void h5inittracex_ ( int [], int *, int[], int *,unsigned * );
void hdf5endtrace_ ( void ) ;
void HDFendTrace_RT (int);
void HDFendTrace_SDDF(int);
void HDFfinalTimeStamp( void );
void startHDFtraceEvent (int );
int computeProcMask (int eventID);
int computePacketTag(int eventID);
void endHDFtraceEvent (int , int , char *, int );
void traceEvent ( int , char *, unsigned );
void HDFtraceEvent_RT ( int , HDFsetInfo *, unsigned );
void HDFtraceIOEvent( int , void *, unsigned );
extern int IOtracingEnabled;
char *hdfRecordPointer;
double WriteTotals = 0.0;
double ReadTotals = 0.0;
/*======================================================================*
// NAME									*
//     HDF5initTrace -- initialize HDF tracing				*
// USAGE								*
//     VOID HDFinitTrace( traceFileName, out_sw )			*
//     char    *traceFileName;	IN: name of the generated trace output  *
//				    file				*
//     int     ...   		IN: indicates which routines to trace	*
//				    The list is terminated by the 	*
//				    OUTPUT_SWITCH value indicating	*
//				    whether to do RunTime or Summary	*
//				    tracing.				*
// RETURNS								*
//     None.								*
//======================================================================*/
/*======================================================================*
// fortran to C interface.  To insure portability, the character array  *
// passed in Fortran is converted to an integer array using the ICHAR	*
// function.  This program converts it from integer to char, then 	*
// passes it to the C initialization routine.				*
//======================================================================*/
void h5inittracex_( int *file, int *len, int flags[], int *nflags,
                                                       unsigned *out_sw )
{
        char *traceFileName;
	int i;
	traceFileName = (char *)malloc(*len+1);
	for ( i = 0; i < *len; ++i ) {
	   traceFileName[i] = file[i];
	}
	traceFileName[*len+1] = 0;
	/*==============================================================*
	// Allocate space for trace indicators.				*
	//==============================================================*/
        procTrace = ( int * ) malloc( NUM_HDF5_IDS*sizeof(int) );
        if ( procTrace == NULL ) {
           fprintf(stderr,">> Error: Unable to allocate procTrace ");
           fprintf(stderr,"array in program HDF5initTrace. <<<\n");
           fprintf(stderr,">>> Exiting program! <<<\n");
           exit (-1);
	}
	/*==============================================================*
	// Initialize to 0.						*
	//==============================================================*/
	for ( i = 0; i <= NUM_HDF5_IDS; ++i ) {
	   procTrace[i] = 0;      
	}
	/*==============================================================*
	// Read in the flags indicating which procedures to trace.	*
	// The last parameter passed is an indicator of the type of 	*
	// tracing to do.  This indicator has a value larger than any   *
	// of the flags.						*
	//==============================================================*/
	for ( i = 0; i < *nflags; ++i ) {
	   procTrace[flags[i]] = 1;
	}
	OUTPUT_SWITCH = *out_sw;
	/*==============================================================*
	// if no flags were passed, the default is to trace all of the  *
	// procedures.							*
	//==============================================================*/
	if ( *nflags == 0 || procTrace[AllHDF5] ) {
	   for ( i = ID_HDF_Last_Entry + 1; i < NUM_HDF5_IDS; ++i ) {
	      procTrace[i] = 1;      
	   }
   	}
	if ( OUTPUT_SWITCH == RUNTIME_TRACE 
	                     || OUTPUT_SWITCH == MPI_RUNTIME_TRACE ) {
	   HDFinitTrace_SDDF( traceFileName, OUTPUT_SWITCH );
	   IOtracingEnabled = 1;
	} else if ( OUTPUT_SWITCH == SUMMARY_TRACE 
	                     || OUTPUT_SWITCH == MPI_SUMMARY_TRACE ) {
	   HDFinitTrace_RT( traceFileName, OUTPUT_SWITCH );
	   IOtracingEnabled = 1;
	} else if ( OUTPUT_SWITCH == NO_TRACE ) {
	   IOtracingEnabled = 0;
	} else {
	   fprintf(stderr,">> Error in HDF5initTrace: the third argument ");
           fprintf(stderr,"must have a value between %4d<<\n",RUNTIME_TRACE);
	   fprintf(stderr,">> and %4d, inclusive.",NO_TRACE);
	   fprintf(stderr,"  The value received was %4u.", OUTPUT_SWITCH);
	   fprintf(stderr," Exiting Program.     <<\n");
	   exit (-1);
	}
}
void HDF5initTrace( const char *traceFileName, int id_flag, ... )
{
	int i, nIDs;
	va_list ap;

	/*==============================================================*
	// Allocate space for trace indicators.				*
	//==============================================================*/
        procTrace = ( int * ) malloc( NUM_HDF5_IDS*sizeof(int) );
        if ( procTrace == NULL ) {
           fprintf(stderr,">> Error: Unable to allocate procTrace ");
           fprintf(stderr,"array in program HDF5initTrace. <<<\n");
           fprintf(stderr,">>> Exiting program! <<<\n");
           exit (-1);
	}
	/*==============================================================*
	// Initialize to 0.						*
	//==============================================================*/
	for ( i = 0; i < NUM_HDF5_IDS; ++i ) {
	   procTrace[i] = 0;      
	}
	/*==============================================================*
	// Read in the flags indicating which procedures to trace.	*
	// The last parameter passed is an indicator of the type of 	*
	// tracing to do.  This indicator has a value larger than any   *
	// of the flags.						*
	//==============================================================*/
	nIDs = 0;
	va_start( ap, id_flag );
	while ( id_flag > NO_TRACE ) {
	   procTrace[id_flag] = 1;
	   ++nIDs;
           id_flag = va_arg ( ap, int );
	}
	OUTPUT_SWITCH = id_flag;
	/*==============================================================*
	// if no flags were passed, the default is to trace all of the  *
	// procedures.							*
	//==============================================================*/
	if ( nIDs == 0 || procTrace[AllHDF5] ) {
	   for ( i = ID_HDF_Last_Entry + 1; i < NUM_HDF5_IDS; ++i ) {
	      procTrace[i] = 1;      
	   }
   	}
	if ( OUTPUT_SWITCH == RUNTIME_TRACE 
	                     || OUTPUT_SWITCH == MPI_RUNTIME_TRACE ) {
	   HDFinitTrace_SDDF( traceFileName, OUTPUT_SWITCH );
	   IOtracingEnabled = 1;
	} else if ( OUTPUT_SWITCH == SUMMARY_TRACE 
	                     || OUTPUT_SWITCH == MPI_SUMMARY_TRACE ) {
	   HDFinitTrace_RT( traceFileName, OUTPUT_SWITCH );
	   IOtracingEnabled = 1;
        } else if ( OUTPUT_SWITCH == NO_TRACE ) {
           IOtracingEnabled = 0;
        } else {
           fprintf(stderr,">> Error in HDF5initTrace: the third argument ");
           fprintf(stderr,"must have a value between %4d<<\n",RUNTIME_TRACE);
           fprintf(stderr,">> and %4d, inclusive.",NO_TRACE);
           fprintf(stderr,"  The value received was %4u.", OUTPUT_SWITCH);
           fprintf(stderr," Exiting Program.     <<\n");
           exit (-1);
        }
}
/*======================================================================*
// NAME									*
//     HDF5endTrace -- end HDF tracing					*
// USAGE								*
//     VOID HDF5endTrace(VOID)						*
// RETURNS								*
//     None.								*
//======================================================================*/
void hdf5endtrace_( void ) 
{
	HDF5endTrace ();
}
void HDF5endTrace(void)
{
	if ( OUTPUT_SWITCH == RUNTIME_TRACE 
	                     || OUTPUT_SWITCH == MPI_RUNTIME_TRACE ) {
	   HDFendTrace_SDDF( OUTPUT_SWITCH );
	} else if ( OUTPUT_SWITCH == SUMMARY_TRACE 
	                     || OUTPUT_SWITCH == MPI_SUMMARY_TRACE ) {
	   HDFendTrace_RT( OUTPUT_SWITCH );
	}
}
void startHDFtraceEvent(int eventID)
{
	if ( OUTPUT_SWITCH == RUNTIME_TRACE 
	                     || OUTPUT_SWITCH == MPI_RUNTIME_TRACE ) {
	   traceEvent( eventID, NULL, 0 ) ;
	} else {
	   HDFtraceEvent_RT( eventID, NULL, 0 ) ;
	} 
}
void endHDFtraceEvent(int eventID, int setID, char *setName, int IDtype )
{
	HDFsetInfo info;
	info.setID = setID;
	info.setName = setName;
	if ( OUTPUT_SWITCH == RUNTIME_TRACE 
	                     || OUTPUT_SWITCH == MPI_RUNTIME_TRACE ) {
	   traceEvent( eventID, (char *)&info, 0 ) ;
	} else if ( OUTPUT_SWITCH == SUMMARY_TRACE 
	                     || OUTPUT_SWITCH == MPI_SUMMARY_TRACE ) {
	   HDFtraceEvent_RT( eventID, &info, 0 ) ;
	} else if ( OUTPUT_SWITCH != NO_TRACE ) {
	   fprintf(stderr,"endHDFtraceEvent: ");
	   fprintf(stderr,"invalid OUTPUT_SWITCH %d, IDtype = %d\n",
                                                  OUTPUT_SWITCH, IDtype ) ;
	}
}
/******************************************************************************/

/*+	Open routines      			                             +*/
/*+     -------------            				             +*/
/*+	                                                                     +*/
/*+ Routine:  FILE *HDFtraceFOPEN( const char *filename, const char *type )  +*/
/*+		  substitute for fopen()                                     +*/
/*+     	  generates fopenBeginID, fopenEndID		             +*/
/*+		  record Open (fopenBegin)                                   +*/
/*+			Mode = -1                                            +*/
/*+									     +*/
FILE *HDFtraceFOPEN( const char *filename, const char *type )
{
    FILE *fp;
    int fd, id;
    int flags = 0;
    struct open_args openArgs;
    size_t typeLen;
	
    if ( IOtracingEnabled ) {
        strcpy( openArgs.filename, filename );
	 
	/* check for 'b' - usually if 2 chars, second is '+' */
	typeLen = strlen( type );
	if ( ( typeLen == 2 ) && ( type [1] == 'b' ) ) {
	    typeLen = 1;
	}

        if ( typeLen == 1 ) {
            switch( type[0] ) {
              case 'r':
                  flags = flags | O_RDONLY;
                  break;
              case 'w':
                  flags = O_TRUNC | O_CREAT | O_WRONLY;
                  break;
              case 'a':
                  flags = flags | O_APPEND | O_CREAT | O_WRONLY;
                  break;
            }
        } else {
            switch( type[0] ) {
              case 'r':
                  flags = O_RDWR;
                  break;
              case 'w':
                  flags = O_TRUNC | O_CREAT | O_RDWR;
                  break;
              case 'a':
                  flags = O_APPEND | O_CREAT | O_RDWR;
                  break;    
            }
        }
        openArgs.flags = flags;
        openArgs.mode= -1;

        HDFtraceIOEvent( fopenBeginID, (void *)&openArgs, sizeof(openArgs) );
    }

    fp = fopen( filename, type );
    if ( fp != NULL ) {
	fd = fileno( fp );
        id = set_c_mappedID( fd );
    } else {
        id = -1;
    }

    if ( IOtracingEnabled ) {
        HDFtraceIOEvent( fopenEndID, (void *) &id, int_SIZE );   
    }

    return( fp );
}

/*+	Routine:  int HDFtraceCREAT( const char *path, mode_t mode )         +*/
/*+		  substitute for creat()                                     +*/
/*+     	  generates openBeginID, openEndID		             +*/
/*+		  record Open (openBeginID)                                  +*/
/*+									     +*/
int HDFtraceCREAT( const char *path, mode_t mode )
{
    struct open_args openArgs;
    int fd;
    int id;

    if ( IOtracingEnabled ) {
        strcpy( openArgs.filename, path );
        openArgs.flags = O_WRONLY | O_CREAT | O_TRUNC;
        openArgs.mode = (int) mode;

        HDFtraceIOEvent( openBeginID, (void *)&openArgs, sizeof(openArgs) );
    }

    fd = creat( path, mode );
    id = set_c_mappedID( fd );

    if ( IOtracingEnabled ) {
        HDFtraceIOEvent( openEndID, (void *) &id, int_SIZE );
    }

    return( fd );
}

/******************************************************************************/

/*+	Flush routines				                             +*/
/*+     --------------				                             +*/
/*+	                                                                     +*/
/*+	Routine:  int HDFtraceFFLUSH( FILE *stream )                         +*/
/*+		  substitute for fflush()                                    +*/
/*+               generates fflushBeginID, fflushEndID                       +*/
/*+	          record Flush (fflushBeginID)                               +*/
/*+									     +*/
int HDFtraceFFLUSH( FILE *stream )
{
    int ret;
    int id;
    int fd;

    if ( IOtracingEnabled ) {
	/*
	 * If stream is NULL, all files open for write are flushed.
	 * We show this with a -2 in the trace record as too much overhead
	 * to try and tell what files those are and generate individual
	 * trace records.
	 */
        if ( stream == NULL ) {   
	    id = -2;
        } else {
            fd = fileno( stream );
	    id = c_mappedID( fd );
            HDFtraceIOEvent( fflushBeginID, (void *) 0, int_SIZE );
        }
    }

    ret = fflush( stream );

    if ( IOtracingEnabled ) {
        HDFtraceIOEvent( fflushEndID, (void *) &id, 0 );
    }

    /* 
     * Note that if fflush called on stream open for reading, the file pointer
     * is moved to EOF if it isn't there already.  We don't account for that
     * in our file positioning information.
     */

    return( ret );
}

/******************************************************************************/
/*+	Close routines				                             +*/
/*+     --------------				                             +*/
/*+	                                                                     +*/
/*+	Routine:  int HDFtraceFCLOSE( FILE *stream )                         +*/
/*+		  substitute for fclose()                                    +*/
/*+               generates fcloseBeginID, fcloseEndID                       +*/
/*+	          record Close (fcloseBeginID)                               +*/
/*+									     +*/
int HDFtraceFCLOSE( FILE *stream )
{
    int ret;
    int id;
    int fd = fileno( stream );

    if ( IOtracingEnabled ) {
	id = c_mappedID( fd );
        HDFtraceIOEvent( fcloseBeginID, (void *) &id, int_SIZE );
    }

    ret = fclose( stream );

    if ( IOtracingEnabled ) {
        HDFtraceIOEvent( fcloseEndID, (void *) 0, 0 );
    }

    return( ret );
}

/*+     Routine:  int HDFtrace3OPEN( char *path, int flags, mode_t mode )    +*/
/*+               substitute for open() when called with 3 arguments         +*/
/*+               generates openBeginID, openEndID                           +*/
/*+               record Open (openBeginID)                                  +*/
/*+                                                                          +*/
int HDFtrace3OPEN( const char *path, int flags, mode_t mode )
{
    struct open_args openArgs;
    int fd;
    int id;

    if ( IOtracingEnabled ) {
        strcpy( openArgs.filename, path );
        openArgs.flags = flags;
        openArgs.mode = (int) mode;

        HDFtraceIOEvent( openBeginID, (char *)&openArgs, sizeof(openArgs) );
    }

    fd = open( path, flags, mode );
    id = set_c_mappedID( fd );

    if ( IOtracingEnabled ) {
        HDFtraceIOEvent( openEndID, (char *) &id, int_SIZE );
    }

    return( fd );
}

/*+	Routine:  int HDFtraceCLOSE( int fd )	                             +*/
/*+		  substitute for close()              			     +*/
/*+               generates closeBeginID, closeEndID 		             +*/
/*+		  record Close (closeBeginID)                                +*/
/*+									     +*/
int HDFtraceCLOSE( int fd )
{
    int ret;
    int id;

    if ( IOtracingEnabled ) {
	id = c_mappedID( fd );
        HDFtraceIOEvent( closeBeginID, (void *) &id, int_SIZE );
    }

    ret = close( fd );

    if ( IOtracingEnabled ) {
        HDFtraceIOEvent( closeEndID, (void *) 0, 0 );
    }

    return( ret );
}

/******************************************************************************/
/*+	Read routines	            			                     +*/
/*+     -------------			            	                     +*/
/*+	                                                                     +*/
/*+	Routine:  int HDFtraceREAD( int fd, char *buf, int nbyte )           +*/
/*+		  substitute for read()                                      +*/
/*+               generates readBeginID, readEndID                           +*/
/*+	          record Read (readBeginID)                                  +*/
/*+                      Number Variables = 1                                +*/
/*+			 Cause = -1                                          +*/
/*+									     +*/
size_t HDFtraceREAD( int fd, char *buf, int nbyte )
{
    struct read_write_args readArgs;  
    size_t ret;
    int bytes;
    CLOCK t1, t2, incDur;

    if ( IOtracingEnabled ) {
        readArgs.fileID = c_mappedID( fd );
        readArgs.numVariables = 1;
        readArgs.cause = -1;   

        HDFtraceIOEvent( readBeginID, (void *) &readArgs, sizeof(readArgs) );
    }

    t1 = getClock();
    ret = read( fd, buf, nbyte );
    t2 = getClock();
    incDur = clockSubtract(t2,t1);
    ReadTotals += clockToSeconds( incDur );

    if ( IOtracingEnabled ) {
        if ( ret > 0 ) {
           bytes = (int)ret;
	} else { 
           bytes = 0;
	} 
        HDFtraceIOEvent( readEndID, (void *) &bytes, int_SIZE );
    }

    return( ret );
}
       
/*+	Routine:  int HDFtraceFREAD( char *ptr, int size, int nitems,        +*/
/*+			          FILE *stream)                              +*/
/*+		  substitute for fread()                                     +*/
/*+               generates freadBeginID, freadEndID                         +*/
/*+	          record Read (freadBeginID)                                 +*/
/*+                      Number Variables = nitems                           +*/
/*+			 Cause = -1                                          +*/
/*+									     +*/
size_t HDFtraceFREAD( void *ptr, int size, int nitems, FILE *stream )
{
    struct read_write_args readArgs;  
    size_t ret;
    int nbytes;
    int fd = fileno( stream );
	CLOCK t1, t2, incDur;

    if ( IOtracingEnabled ) {
        readArgs.fileID = c_mappedID( fd );
        readArgs.numVariables = nitems;
        readArgs.cause = -1;
        HDFtraceIOEvent( freadBeginID, (void *) &readArgs, sizeof(readArgs) );
    }

    t1 = getClock();
    ret = fread( ptr, size, nitems, stream );
    t2 = getClock();
    incDur = clockSubtract(t2,t1);
    ReadTotals += clockToSeconds( incDur );

    if ( IOtracingEnabled ) {
        if ( ret > 0 ) {
           nbytes = (int)ret * size ;
	} else {
           nbytes = 0;
	}
        HDFtraceIOEvent( freadEndID, (void *) &nbytes, int_SIZE );
    }

    return( ret );
}

       
/******************************************************************************/
/*+	Seek routines            			                     +*/
/*+     -------------			            	                     +*/
/*+	                                                                     +*/
/*+	Routine:  off_t HDFtraceLSEEK( int fd, off_t offset, int whence )    +*/
/*+		  substitute for lseek()                                     +*/
/*+               generates lseekBeginID, lseekEndID                         +*/
/*+	          record Seek (lseekBeginID)                                 +*/
/*+									     +*/
off_t HDFtraceLSEEK( int fd, off_t offset, int whence )
{
    struct seek_args seekArgs;
    off_t ret;
    long  arg;

    if ( IOtracingEnabled ) {
        seekArgs.fileID = c_mappedID( fd );
        seekArgs.offset = (int) offset;
        seekArgs.whence = whence;

        HDFtraceIOEvent( lseekBeginID, (void *) &seekArgs, sizeof(seekArgs) );
    }

    ret = lseek( fd, offset, whence );

    if ( IOtracingEnabled ) {
	arg = (long) ret;
        HDFtraceIOEvent( lseekEndID, (void *)&arg, long_SIZE );
    }

    return( ret );
}

/*+ routine:  int HDF traceFSEEK( FILE *stream, long offset, int whence )    +*/
/*+		  substitute for fseek()                                     +*/
/*+               generates fseekBeginID, fseekEndID                         +*/
/*+	          record Seek (fseekBeginID)                                 +*/
/*+									     +*/
int HDFtraceFSEEK( FILE *stream, long offset, int whence )
{
    struct seek_args seekArgs;
    int ret;
    long arg;
    int fd = fileno( stream );

    if ( IOtracingEnabled ) {
        seekArgs.fileID = c_mappedID( fd );;
        seekArgs.offset = (int) offset;
        seekArgs.whence = whence;

        HDFtraceIOEvent( fseekBeginID, (void *) &seekArgs, sizeof(seekArgs) );
    }

    ret = fseek( stream, offset, whence );

    if ( IOtracingEnabled ) {
	arg = ftell( stream );
        HDFtraceIOEvent( fseekEndID, (void *)&arg, long_SIZE );
    }

    return( ret );
}

#ifdef fpos_t
/*+ Routine:  int HDFtraceFSETPOS( FILE *stream, const fpos_t *position )   +*/
/*+		  substitute for fsetpos()                                   +*/
/*+               generates fsetposBeginID, fsetposEndID                     +*/
/*+	          record Seek (fsetposBeginID)                               +*/
/*+									     +*/
int HDFtraceFSETPOS( FILE stream, const fpos_t *position )
{
    struct seek_args seekArgs;
    int ret;
    long arg;
    int fd = fileno( stream );

    if ( IOtracingEnabled ) {
        seekArgs.fileID = c_mappedID( fd );;
        seekArgs.offset = (int) *position;
        seekArgs.whence = SEEK_SET;

        HDFtraceIOEvent( fsetposBeginID, (void *) &seekArgs, sizeof(seekArgs) );
    }

    ret = fsetpos( stream, position );

    if ( IOtracingEnabled ) {
	arg = (long) *position;
        HDFtraceIOEvent( fsetposEndID, (void *)&arg, long_SIZE );
    }

    return( ret );
}
#endif /* fpos_t */

/*+	Routine:  void HDFtraceREWIND ( FILE *stream )                       +*/
/*+		  substitute for rewind()                                    +*/
/*+               generates rewindBeginID, rewindEndID                       +*/
/*+	          record Seek (rewindBeginID)                                +*/
/*+                    	 Offset = 0                                          +*/
/*+			 Whence = SEEK_SET                                   +*/
/*+									     +*/
void HDFtraceREWIND( FILE *stream )
{
    struct seek_args seekArgs;
    long arg;
    int fd = fileno( stream );

    if ( IOtracingEnabled ) {
        seekArgs.fileID = c_mappedID( fd );
        seekArgs.offset = 0;
        seekArgs.whence = SEEK_SET;

        HDFtraceIOEvent( rewindBeginID, (void *) &seekArgs, sizeof(seekArgs) );
    }

    rewind( stream );

    if ( IOtracingEnabled ) {
	arg = 0;
        HDFtraceIOEvent( rewindEndID, (void *)&arg, long_SIZE );
    }

    return;
}

/******************************************************************************/
/*+	Write routines            			                     +*/
/*+     --------------			            	                     +*/
/*+	                                                                     +*/
/*+  Routine:  int HDFtraceWRITE( int fd, char *buf, int nbyte )             +*/
/*+		  substitute for write()                                     +*/
/*+               generates writeBeginID, writeEndID                         +*/
/*+	          record Write (writeBeginID)                                +*/
/*+                    	 Number Variables = 1                                +*/
/*+			 Cause = -1                                          +*/
/*+									     +*/
size_t HDFtraceWRITE( int fd, const char *buf, int nbyte )
{
    struct read_write_args writeArgs;
    size_t ret;
    int bytes;
    CLOCK t1, t2, incDur;

    if ( IOtracingEnabled ) {
        writeArgs.fileID = c_mappedID( fd );
        writeArgs.numVariables = 1;
        writeArgs.cause = -1;

        HDFtraceIOEvent( writeBeginID, (void *) &writeArgs, sizeof(writeArgs) );
    }

    t1 = getClock();
    ret = (size_t)write( fd, buf, nbyte );
    t2 = getClock();
    incDur = clockSubtract(t2,t1);
    WriteTotals += clockToSeconds( incDur );

    if ( IOtracingEnabled ) {
        if ( ret > 0 ) {
           bytes =  (int)ret;
	} else {
	   bytes = 0;
	}
        HDFtraceIOEvent( writeEndID, (void *) &bytes, int_SIZE );
    }
    return( ret );
}  

/*+  Routine:  size_t HDFtraceFWRITE( const char *ptr, int size, int nitems, +*/
/*+                                FILE *stream )                            +*/
/*+		  substitute for fwrite()                                    +*/
/*+               generates fwriteBeginID, fwriteEndID                       +*/
/*+	          record Write (fwriteBeginID)                               +*/
/*+                    	 Number Variables = nitems                           +*/
/*+			 Cause = -1                                          +*/
/*+									     +*/
size_t HDFtraceFWRITE( const char *ptr, int size, int nitems, FILE *stream )
{
    struct read_write_args writeArgs;
    size_t ret;
    int nbytes;
    int fd = fileno( stream );
    CLOCK t1, t2, incDur;

    if ( IOtracingEnabled ) {
        writeArgs.fileID = c_mappedID( fd );
        writeArgs.numVariables = nitems;
        writeArgs.cause = -1;

      HDFtraceIOEvent( fwriteBeginID, (void *) &writeArgs, sizeof(writeArgs) );
    }

    t1 = getClock();
    ret = fwrite( ptr, size, nitems, stream );
    t2 = getClock();
    incDur = clockSubtract(t2,t1);
    WriteTotals += clockToSeconds( incDur );


    if ( IOtracingEnabled ) {
        if ( ret > 0 ) {
           nbytes = (int)ret * size ;
	} else {
           nbytes = 0;
	} 
        HDFtraceIOEvent( fwriteEndID, (void *) &nbytes, int_SIZE );
    }

    return( ret );
}

/*+  Routine:  int HDFtracePUTS( char *s )                                   +*/
/*+		  substitute for puts()                                      +*/
/*+               generates fwriteBeginID, fwriteEndID                       +*/
/*+	          record Write (fwriteBeginID)                               +*/
/*+                    	 Number Variables = 1                                +*/
/*+			 Cause = -1                                          +*/
/*+									     +*/
int HDFtracePUTS( char *s )
{
    struct read_write_args writeArgs;
    int ret;
    int fd = fileno( stdout );

    if ( IOtracingEnabled ) {
        writeArgs.fileID = c_mappedID( fd );
        writeArgs.numVariables = 1;
        writeArgs.cause = -1;

        HDFtraceIOEvent( fwriteBeginID, (void *) &writeArgs, sizeof(writeArgs) );
    }

    ret = puts( s );

    if ( IOtracingEnabled ) {
        HDFtraceIOEvent( fwriteEndID, (void *) &ret, int_SIZE );
    }

    return( ret );
}
       
/*+	Routine:  int HDFtraceFPUTC( int c, FILE *stream )                   +*/
/*+		  substitute for fputc()                                     +*/
/*+               generates fwriteBeginID, fwriteEndID                       +*/
/*+	          record Write (fwriteBeginID)                               +*/
/*+                    	 Number Variables = 1                                +*/
/*+			 Cause = -1                                          +*/
/*+									     +*/
int HDFtraceFPUTC( int c, FILE *stream )
{
    struct read_write_args writeArgs;
    int ret; 
    int nbytes = char_SIZE;
    int fd = fileno( stream );

    if ( IOtracingEnabled ) {
        writeArgs.fileID = c_mappedID( fd );
        writeArgs.numVariables = 1;
        writeArgs.cause = -1;

        HDFtraceIOEvent( fwriteBeginID, (void *) &writeArgs, sizeof(writeArgs) );
    }

    ret = fputc( c, stream );

    if ( IOtracingEnabled ) {
	if ( ret == EOF ) {
	    nbytes = 0;
            HDFtraceIOEvent( fwriteEndID, (void *) &nbytes, int_SIZE );
	}
    }

    return( ret );
}
/*+  Routine:  int HDFtraceFPUTS( char *s, FILE *stream )                    +*/
/*+		  substitute for fputs()                                     +*/
/*+               generates fwriteBeginID, fwriteEndID                       +*/
/*+	          record Write (fwriteBeginID)                               +*/
/*+                    	 Number Variables = 1                                +*/
/*+			 Cause = -1                                          +*/
/*+									     +*/
int HDFtraceFPUTS( const char *s, FILE *stream )
{
    struct read_write_args writeArgs;
    int ret;
    int fd = fileno( stream );

    if ( IOtracingEnabled ) {
        writeArgs.fileID = c_mappedID( fd );
        writeArgs.numVariables = 1;
        writeArgs.cause = -1;

        HDFtraceIOEvent(fwriteBeginID, (void *)&writeArgs, sizeof(writeArgs));
    }

    ret = fputs( s, stream );

    if ( IOtracingEnabled ) {
        HDFtraceIOEvent( fwriteEndID, (void *) &ret, int_SIZE );
    }

    return( ret );
}
void *HDFtraceMALLOC(size_t bytes )
{
	void *ptr;
	int byte_req;
	byte_req = (int)bytes;
	if ( IOtracingEnabled ) {
	   HDFtraceIOEvent ( ID_malloc, NULL, 0 );
	}
	
	ptr = malloc( bytes );

	if ( IOtracingEnabled ) {
	   HDFtraceIOEvent ( -ID_malloc, &byte_req, sizeof(int) );
	}
	
	return ptr ;

}
	
void HDFtraceIOEvent( int eventType, void *dataPtr, unsigned dataLen )
{
	if ( OUTPUT_SWITCH == RUNTIME_TRACE 
	                     || OUTPUT_SWITCH == MPI_RUNTIME_TRACE ) {
           traceEvent( eventType, dataPtr, dataLen );
        } else {
           HDFtraceEvent_RT( eventType, (HDFsetInfo *)dataPtr, dataLen );
        }
}
/*======================================================================*
// record the final time stamp                                          *
//======================================================================*/
void HDFfinalTimeStamp( void )
{
        CLOCK   currentTime;
        double  seconds;
        struct {
                int packetLength,
                    packetType,
                    packetTag,
                    timeDim;
                double Seconds;
                int eventID,
                    node,
                    dataLen;
        } Packet;

        currentTime = getClock();
        seconds = clockToSeconds( currentTime );

        Packet.packetLength = sizeof(Packet);
        Packet.packetType   = PKT_DATA;
        Packet.packetTag    = FAMILY_EXTERNAL | RECORD_TRACE;
        Packet.timeDim      = 0;        /* use fp time stamp only */
        Packet.Seconds      = seconds;  /* fp time stamp          */
        Packet.eventID      = ID_timeStamp;
        Packet.node         = TRgetNode();
        Packet.dataLen      = 0;
        putBytes( (void *)&Packet , sizeof(Packet) );
}
/*======================================================================*
// This Program is called to specify which routines are to be traced.	*
// On first entry, the program allocates storage for and initializes a  *
// global array procTrace.  The array has one element for each possible *
// HDF5 procedure and HDF5 library file.  If a procedure or all of the  *
// procedure in an HDF file are to be traced, then the elemen in the 	*
// array corresponding to the procedure or file is turned on.  This is  *
// used by the macros TRACE_ON and TRACE_OFF to enable tracing.  If     *
// this procedure is not called prior to initialization, then all of    *
// the elements of procTrace corresponding to HDF 5 files will be 	*
// turned on, in which case all HDF 5 procedures will be traced.	*
//======================================================================*/
void PabloHDF5Trace( int ID ) 
{
	int i;
	if ( procTrace == NULL ) {
	   procTrace = ( int * ) malloc( NUM_HDF5_IDS*sizeof(int) );
	   if ( procTrace == NULL ) {
              fprintf(stderr,">> Error: Unable to allocate procTrace ");
              fprintf(stderr,"array in program PabloHDF5Trace. <<<\n");
              fprintf(stderr," Exiting program.                  <<<\n");
              exit (-1);
	   }
	   for ( i = 0; i < NUM_HDF5_IDS; ++i ) {
	       procTrace[i] = 0;      
	   }
	}
	if ( ID >= 0 && ID < NUM_HDF5_IDS ) {
	   procTrace[ID] = 1;      
	} else {
           fprintf(stderr,">> Error: Value passed to PabloHDF5Trace, ");
           fprintf(stderr,"%d, is out of range. <<<\n",ID);
           fprintf(stderr," Exiting program.                  <<<\n");
           exit (-1);
	}
}
#ifdef HAVE_PARALLEL
int HDF_XMPI_File_open( MPI_Comm comm, char *filename, int amode, 
			MPI_Info info, MPI_File *fh );
int HDF_XMPI_File_close( MPI_File *fh );
int HDF_XMPI_File_delete( char *filename, MPI_Info info );
int HDF_XMPI_File_set_size( MPI_File fh, MPI_Offset size );
int HDF_XMPI_File_preallocate( MPI_File fh, MPI_Offset size);
int HDF_XMPI_File_get_size( MPI_File fh, MPI_Offset *size );
int HDF_XMPI_File_get_group( MPI_File fh, MPI_Group *group );
int HDF_XMPI_File_get_amode( MPI_File fh, int *amode );
int HDF_XMPI_File_set_view( MPI_File fh, MPI_Offset disp, MPI_Datatype etype, 
			    MPI_Datatype filetype, char *datarep, 
			    MPI_Info info );
int HDF_XMPI_File_get_view( MPI_File fh, MPI_Offset *disp, 
			    MPI_Datatype *etype, MPI_Datatype *filetype, 
			    char *datarep );
int HDF_XMPI_File_read_at( MPI_File fh, MPI_Offset offset, void *buf, 
			    int count, MPI_Datatype datatype, 
			    MPI_Status *status );
int HDF_XMPI_File_read_at_all( MPI_File fh, MPI_Offset offset, void *buf, 
			       int count, MPI_Datatype datatype, 
			       MPI_Status *status );
int HDF_XMPI_File_write_at( MPI_File fh, MPI_Offset offset, void *buf, 
                            int count, MPI_Datatype datatype, 
                            MPI_Status *status );
int HDF_XMPI_File_write_at_all( MPI_File fh, MPI_Offset offset, void *buf, 
                                int count, MPI_Datatype datatype, 
                                MPI_Status *status );

int HDF_XMPI_File_iread_at( MPI_File fh, MPI_Offset offset, void *buf, 
                            int count, MPI_Datatype datatype, 
                            MPIO_Request *request );
int HDF_XMPI_File_iwrite_at( MPI_File fh, MPI_Offset offset, void *buf, 
                             int count, MPI_Datatype datatype, 
                             MPIO_Request *request );
int HDF_XMPI_File_read( MPI_File fh, void *buf, int count, 
                             MPI_Datatype datatype, MPI_Status *status );
int HDF_XMPI_File_read_all( MPI_File fh, void *buf, int count, 
                            MPI_Datatype datatype, MPI_Status *status );
int HDF_XMPI_File_write( MPI_File fh, void *buf, int count, 
                         MPI_Datatype datatype, MPI_Status *status );
int HDF_XMPI_File_write_all( MPI_File fh, void *buf, int count, 
                             MPI_Datatype datatype, MPI_Status *status );
int HDF_XMPI_File_iread( MPI_File fh, void *buf, int count, 
			 MPI_Datatype datatype, MPIO_Request *request );
int HDF_XMPI_File_iwrite( MPI_File fh, void *buf, int count, 
			  MPI_Datatype datatype, MPIO_Request *request );
int HDF_XMPI_File_seek( MPI_File fh, 
		        MPI_Offset offset, int whence ) ;
int HDF_XMPI_File_get_position( MPI_File fh, MPI_Offset *offset );
int HDF_XMPI_File_get_byte_offset( MPI_File fh, MPI_Offset offset, 
                                   MPI_Offset *disp) ;
int HDF_XMPI_File_get_type_extent( MPI_File fh, MPI_Datatype datatype, 
			           MPI_Aint *extent );
int HDF_XMPI_File_set_atomicity( MPI_File fh, int flag );
int HDF_XMPI_File_get_atomicity( MPI_File fh, int *flag );
int HDF_XMPI_File_sync( MPI_File fh );
/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_open( MPI_Comm comm, char *filename, int amode, 
                                    MPI_Info info, MPI_File *fh )
{
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;
   
        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
           returnVal = HDF_XMPI_File_open( comm, filename, amode, info, fh );
        } else {
	   dataLen = sizeof( HDFsetInfo );
	   dataPtr.setID = (long)fh;
	   dataPtr.setName = (char *)malloc( strlen(filename) + 1);
	   strcpy( dataPtr.setName , filename );
           HDFtraceEvent_RT( mpiOpenBeginID, &dataPtr, dataLen );
           returnVal = PMPI_File_open( comm, filename, amode, info, fh );
           HDFtraceEvent_RT( mpiOpenEndID, &dataPtr, dataLen );
        }
   	return returnVal;
}

   

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_close( MPI_File *fh )
{
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;
   
        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
           returnVal = HDF_XMPI_File_close( fh );
        } else {
	   dataLen = sizeof( HDFsetInfo );
	   dataPtr.setID = (long)fh;
	   dataPtr.setName = NULL;
           HDFtraceEvent_RT( mpiCloseBeginID, &dataPtr, dataLen );
           returnVal = PMPI_File_close( fh );
           HDFtraceEvent_RT( mpiCloseEndID, &dataPtr, dataLen );
	   free( dataPtr.setName );
        }
   	return returnVal;
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_delete( char *filename, MPI_Info info )
{
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;
   
        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
   	   returnVal = HDF_XMPI_File_delete( filename, info );
        } else {
	   dataLen = sizeof( HDFsetInfo );
	   dataPtr.setID = 0;
	   dataPtr.setName = (char *)malloc( sizeof(filename) );
	   strcpy( dataPtr.setName , filename );
           HDFtraceEvent_RT( mpiDeleteBeginID, &dataPtr, dataLen );
   	   returnVal = PMPI_File_delete( filename, info );
           HDFtraceEvent_RT( mpiDeleteEndID, &dataPtr, dataLen );
	   free( dataPtr.setName );
        }
   	return returnVal;
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_set_size( MPI_File fh, MPI_Offset size )
{
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;
   
        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
   	   returnVal = HDF_XMPI_File_set_size( fh, size );
        } else {
	   dataLen = 0;
           HDFtraceEvent_RT( mpiSetSizeBeginID,&dataPtr,dataLen );
   	   returnVal = PMPI_File_set_size( fh, size );
           HDFtraceEvent_RT( mpiSetSizeEndID, &dataPtr, dataLen );
        }
   	return returnVal;
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_preallocate( MPI_File fh, MPI_Offset size)
{
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;
   
        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
   	   returnVal = HDF_XMPI_File_preallocate( fh, size);
        } else {
	   dataLen = 0;
           HDFtraceEvent_RT( mpiPreallocateBeginID,
                             &dataPtr,dataLen );
   	   returnVal = PMPI_File_preallocate( fh, size);
           HDFtraceEvent_RT( mpiPreallocateEndID, 
                             &dataPtr, dataLen );
        }
   	return returnVal;
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_get_size( MPI_File fh, MPI_Offset *size )
{
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
           returnVal = HDF_XMPI_File_get_size( fh, size);
	} else {
	   dataLen = 0;
           HDFtraceEvent_RT( mpiGetSizeBeginID,
                             &dataPtr,dataLen );
           returnVal = PMPI_File_get_size( fh, size);
           HDFtraceEvent_RT( mpiGetSizeEndID,
                             &dataPtr,dataLen );
	}
   	return returnVal;
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_get_group( MPI_File fh, MPI_Group *group )
{
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;
   
        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
   	   returnVal = HDF_XMPI_File_get_group( fh, group);
	} else {
	   dataLen = 0;
           HDFtraceEvent_RT( mpiGetGroupBeginID,
                             &dataPtr,dataLen );
   	   returnVal = PMPI_File_get_group( fh, group);
           HDFtraceEvent_RT( mpiGetGroupEndID,
                             &dataPtr,dataLen );
	}
   	return returnVal;
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_get_amode( MPI_File fh, int *amode )
{
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
   	   returnVal = HDF_XMPI_File_get_amode( fh, amode);
	} else {
	   dataLen = 0;
           HDFtraceEvent_RT( mpiGetAmodeBeginID,
                             &dataPtr,dataLen );
   	   returnVal = PMPI_File_get_amode( fh, amode);
           HDFtraceEvent_RT( mpiGetAmodeEndID,
                             &dataPtr,dataLen );
	}
   	return returnVal;
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_set_view( MPI_File fh, MPI_Offset disp, MPI_Datatype etype, 
                           MPI_Datatype filetype, char *datarep, MPI_Info info )
{
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
   	   returnVal = HDF_XMPI_File_set_view( fh, disp, etype, filetype, 
                                                      datarep, info );
	} else {
	   dataLen = 0;
           HDFtraceEvent_RT( mpiSetViewBeginID,
                             &dataPtr,dataLen );
   	   returnVal = PMPI_File_set_view( fh, disp, etype, filetype, 
                                                      datarep, info );
           HDFtraceEvent_RT( mpiSetViewEndID,
                             &dataPtr,dataLen );
	}
   	return returnVal;
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_get_view( MPI_File fh, MPI_Offset *disp, MPI_Datatype *etype, 
                           MPI_Datatype *filetype, char *datarep )
{
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
   	   returnVal = HDF_XMPI_File_get_view(fh, disp, etype, filetype, datarep);
	} else {
	   dataLen = 0;
           HDFtraceEvent_RT( mpiSetViewBeginID,
                             &dataPtr,dataLen );
   	   returnVal = PMPI_File_get_view(fh, disp, etype, filetype, datarep);
           HDFtraceEvent_RT( mpiSetViewEndID,
                             &dataPtr,dataLen );
     	   returnVal = PMPI_File_get_view(fh, disp, etype, filetype, datarep);

	}
   	return returnVal;
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_read_at( MPI_File fh, MPI_Offset offset, void *buf,
		         int count, MPI_Datatype datatype, MPI_Status *status )
{
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
   	   returnVal = HDF_XMPI_File_read_at( fh, offset, buf, count, datatype, 
	                                                            status );
	} else {
	   dataLen = 0;
           HDFtraceEvent_RT( mpiReadAtBeginID,
                             &dataPtr,dataLen );
   	   returnVal = PMPI_File_read_at( fh, offset, buf, count, datatype, 
	                                                            status );
           HDFtraceEvent_RT( mpiReadAtEndID,
                             &dataPtr,dataLen );
	}
   	return returnVal;
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_read_at_all( MPI_File fh, MPI_Offset offset, void *buf,
   			int count, MPI_Datatype datatype, MPI_Status *status )
{
   
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
   	   returnVal = HDF_XMPI_File_read_at_all( fh, offset, buf, 
				      count, datatype, status );
	} else {
	   dataLen = 0;
           HDFtraceEvent_RT( mpiReadAtAllBeginID,
                             &dataPtr,dataLen );
   	   returnVal = PMPI_File_read_at_all( fh, offset, buf, 
				      count, datatype, status );
           HDFtraceEvent_RT( mpiReadAtAllEndID,
                             &dataPtr,dataLen );
	}
   	return returnVal;
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_write_at( MPI_File fh, MPI_Offset offset, void *buf,
                      int count, MPI_Datatype datatype, MPI_Status *status )
{
   
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
   	   returnVal = HDF_XMPI_File_write_at( fh, offset, buf, count, datatype, 
	                                                           status );
	} else {
	   dataLen = 0;
           HDFtraceEvent_RT( mpiWriteAtBeginID,
                             &dataPtr,dataLen );
   	   returnVal = PMPI_File_write_at( fh, offset, buf, count, datatype, 
	                                                            status );
           HDFtraceEvent_RT( mpiWriteAtEndID,
                             &dataPtr,dataLen );
	}
   	return returnVal;
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_write_at_all( MPI_File fh, MPI_Offset offset, void *buf,
                  int count, MPI_Datatype datatype, MPI_Status *status )
{
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
   	   returnVal = HDF_XMPI_File_write_at_all( fh, offset, buf, 
	 	 	       		count, datatype, status );
	} else {
	   dataLen = 0;
           HDFtraceEvent_RT( mpiWriteAtAllBeginID,
                             &dataPtr,dataLen );
   	   returnVal = PMPI_File_write_at_all( fh, offset, buf, 
	 			       count, datatype, status );
           HDFtraceEvent_RT( mpiWriteAtAllEndID,
                             &dataPtr,dataLen );
	}
   	return returnVal;
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_iread_at( MPI_File fh, MPI_Offset offset, void *buf,
   int count, MPI_Datatype datatype, MPIO_Request *request )
{
   return HDF_XMPI_File_iread_at( fh, offset, buf, count, datatype, request );
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_iwrite_at( MPI_File fh, MPI_Offset offset, void *buf, 
		int count, MPI_Datatype datatype, MPIO_Request *request)
{
   return HDF_XMPI_File_iwrite_at( fh, offset, buf, count, datatype, request );
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_read( MPI_File fh, void *buf, int count, 
                       MPI_Datatype datatype, MPI_Status *status)
{
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
   	   returnVal = HDF_XMPI_File_read( fh, buf, count, datatype, status );
	} else {
	   dataLen = 0;
           HDFtraceEvent_RT( mpiReadBeginID,
                             &dataPtr,dataLen );
   	   returnVal = PMPI_File_read( fh, buf, count, datatype, status );
           HDFtraceEvent_RT( mpiReadEndID,
                             &dataPtr,dataLen );
	}
   	return returnVal;
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_read_all( MPI_File fh, void *buf, int count, 
                           MPI_Datatype datatype, MPI_Status *status)
{
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
   	   returnVal = HDF_XMPI_File_read_all( fh, buf, count, datatype, status );
	} else {
	   dataLen = 0;
           HDFtraceEvent_RT( mpiReadAllBeginID,
                             &dataPtr,dataLen );
   	   returnVal = PMPI_File_read_all( fh, buf, count, datatype, status );
           HDFtraceEvent_RT( mpiReadAllEndID,
                             &dataPtr,dataLen );
	}
   	return returnVal;
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_write( MPI_File fh, void *buf, int count, 
                        MPI_Datatype datatype, MPI_Status *status )
{
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
           returnVal = HDF_XMPI_File_write( fh, buf, count, datatype, status );
	} else {
	   dataLen = 0;
           HDFtraceEvent_RT( mpiWriteBeginID,
                             &dataPtr,dataLen );
           returnVal = PMPI_File_write( fh, buf, count, datatype, status );
           HDFtraceEvent_RT( mpiWriteEndID,
                             &dataPtr,dataLen );
	}
   	return returnVal;
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_write_all( MPI_File fh, void *buf, int count, 
                            MPI_Datatype datatype, MPI_Status *status )
{
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
   	   returnVal =HDF_XMPI_File_write_all( fh, buf, count, datatype, status );
	} else {
	   dataLen = 0;
           HDFtraceEvent_RT( mpiWriteAllBeginID,
                             &dataPtr,dataLen );
   	   returnVal = PMPI_File_write_all( fh, buf, count, datatype, status );
           HDFtraceEvent_RT( mpiWriteAllEndID,
                             &dataPtr,dataLen );
	}
   	return returnVal;
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_iread( MPI_File fh, void *buf, int count, 
                        MPI_Datatype datatype, MPIO_Request *request )
{
	return HDF_XMPI_File_iread( fh, buf, count, datatype, request );
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_iwrite( MPI_File fh, void *buf, int count, 
	                 MPI_Datatype datatype, MPIO_Request *request )
{
   return HDF_XMPI_File_iwrite( fh, buf, count, datatype, request );
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_seek( MPI_File fh, MPI_Offset offset, int whence )
{
   
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
   	   returnVal = HDF_XMPI_File_seek( fh, offset, whence );
	} else {
	   dataLen = 0;
           HDFtraceEvent_RT( mpiSeekBeginID,
                             &dataPtr,dataLen );
   	   returnVal = PMPI_File_seek( fh, offset, whence );
           HDFtraceEvent_RT( mpiSeekEndID,
                             &dataPtr,dataLen );
	}
   	return returnVal;
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_get_position( MPI_File fh, MPI_Offset *offset )
{
   
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
   	   returnVal = HDF_XMPI_File_get_position( fh, offset );
	} else {
	   dataLen = 0;
           HDFtraceEvent_RT( mpiGetPositionBeginID,
                             &dataPtr,dataLen );
   	   returnVal = PMPI_File_get_position( fh, offset );
           HDFtraceEvent_RT( mpiGetPositionEndID,
                             &dataPtr,dataLen );
	}
   	return returnVal;
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_get_byte_offset( MPI_File fh, MPI_Offset offset, 
                                             MPI_Offset *disp )
{
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
   	   returnVal = HDF_XMPI_File_get_byte_offset( fh, offset, disp );
	} else {
	   dataLen = 0;
           HDFtraceEvent_RT( mpiGetByteOffsetBeginID,
                             &dataPtr,dataLen );
   	   returnVal = PMPI_File_get_byte_offset( fh, offset, disp );
           HDFtraceEvent_RT( mpiGetByteOffsetEndID,
                             &dataPtr,dataLen );
	}
   	return returnVal;
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_get_type_extent( MPI_File fh, MPI_Datatype datatype, 
                                               MPI_Aint *extent )
{
   
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
   	   returnVal = HDF_XMPI_File_get_type_extent( fh, datatype, extent );
	} else {
	   dataLen = 0;
           HDFtraceEvent_RT( mpiGetTypeExtentBeginID,
                             &dataPtr,dataLen );
   	   returnVal = PMPI_File_get_type_extent( fh, datatype, extent );
           HDFtraceEvent_RT( mpiGetTypeExtentEndID,
                             &dataPtr,dataLen );
	}
   	return returnVal;
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_set_atomicity( MPI_File fh, int flag )
{
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
   	   returnVal = HDF_XMPI_File_set_atomicity( fh, flag );
	} else {
	   dataLen = 0;
           HDFtraceEvent_RT( mpiSetAtomicityBeginID,
                             &dataPtr,dataLen );
   	   returnVal = PMPI_File_set_atomicity( fh, flag );
           HDFtraceEvent_RT( mpiSetAtomicityEndID,
                             &dataPtr,dataLen );
	}
   	return returnVal;
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_get_atomicity( MPI_File fh, int *flag )
{
   
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
   	   returnVal = HDF_XMPI_File_get_atomicity( fh, flag );
	} else {
	   dataLen = 0;
           HDFtraceEvent_RT( mpiGetAtomicityBeginID,
                             &dataPtr,dataLen );
   	   returnVal = PMPI_File_get_atomicity( fh, flag );
           HDFtraceEvent_RT( mpiGetAtomicityEndID,
                             &dataPtr,dataLen );
	}
   	return returnVal;
}

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int HDF_MPI_File_sync( MPI_File fh )
{
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) { 
   	   returnVal = HDF_XMPI_File_sync ( fh );
	} else {
	   dataLen = 0;
           HDFtraceEvent_RT( mpiSyncBeginID,
                             &dataPtr,dataLen );
   	   returnVal = PMPI_File_sync ( fh );
           HDFtraceEvent_RT( mpiSyncEndID,
                             &dataPtr,dataLen );
	} 
   	return returnVal;
}

#endif /* HAVE_PARALLEL */
