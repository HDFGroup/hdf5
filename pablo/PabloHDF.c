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

int OUTPUT_SWITCH;
extern void preInitIOTrace( void ); 

#include "ProcIDs.h"
#include "HDF5Trace.h"
#include "IOTrace.h"
#define NO_OUTPUT 0
#define SDDF_OUTPUT 1
#define RT_OUTPUT 2
void HDFinitTrace_RT ( const char *, unsigned int );
void HDFinitTrace_SDDF ( const char *, unsigned int );
void hinittracex_ ( int [], int *, unsigned *, int * );
void hdfendtrace_ ( void ) ;
void HDFendTrace (void);
void HDFendTrace_RT (void);
void HDFendTrace_SDDF(void);
void HDFfinalTimeStamp( void );
void startHDFtraceEvent (int );
int computeProcMask (int eventID);
int computePacketTag(int eventID);
void endHDFtraceEvent (int , int , char *, int );
void traceEvent ( int , char *, unsigned );
void HDFtraceEvent_RT ( int , HDFsetInfo *, unsigned );
void HDFtraceIOEvent( int , void *, unsigned );
uint procTrace;
extern int IOtracingEnabled;
char *hdfRecordPointer;
/*======================================================================*
// NAME									*
//     HDFinitTrace -- initialize HDF tracing				*
// USAGE								*
//     VOID HDFinitTrace( traceFileName, procTraceMask, out_sw )	*
//     char    *traceFileName;	IN: name of the generated trace output  *
//				    file				*
//     uint32  procTraceMask;	IN: families of procedures to trace	*
//     int     out_sw		IN: indicates whether to produce SDDF	*
//				    file or Final Summary		*
// RETURNS								*
//     None.								*
//======================================================================*/
/*======================================================================*
// fortran to C interface.  To insure portability, the character array  *
// passed in Fortran is converted to an integer array using the ICHAR	*
// function.  This program converts it from integer to char, then 	*
// passes it to the C initialization routine.				*
//======================================================================*/
void hinittracex_( int *file, int *len, unsigned *procMask, int *out_sw )
{
        char *fileName;
	int i;
	fileName = (char *)malloc(*len+1);
	for ( i = 0; i < *len; ++i ) {
	   fileName[i] = file[i];
	}
	fileName[*len+1] = 0;
        HDFinitTrace ( fileName, *procMask, *out_sw );
}
void HDFinitTrace( const char *traceFileName, unsigned  procTraceMask, 
	                                      unsigned  out_sw )
{
	OUTPUT_SWITCH = out_sw;
	if ( out_sw == SDDF_OUTPUT ) {
	   HDFinitTrace_SDDF( traceFileName, procTraceMask );
	   IOtracingEnabled = 1;
	} else if ( out_sw == RT_OUTPUT ) {
	   HDFinitTrace_RT( traceFileName, procTraceMask );
	   IOtracingEnabled = 1;
	} else if ( out_sw == NO_OUTPUT ) {
	   procTrace = 0;
	   IOtracingEnabled = 0;
	} else {
	   fprintf(stderr,">> Error in HDFinitTrace: the third argument ");
           fprintf(stderr,"must have the value 0, 1, <<<\n");
	   fprintf(stderr,">> or 2.  The value received was %u.", out_sw);
	   fprintf(stderr," Exiting program.                  <<<\n");
	   exit (-1);
	}
}
/*======================================================================*
// NAME									*
//     HDFendTrace -- end HDF tracing					*
// USAGE								*
//     VOID HDFendTrace(VOID)						*
// RETURNS								*
//     None.								*
//======================================================================*/
void hdfendtrace_( void ) 
{
	HDFendTrace ();
}
void HDFendTrace(void)
{
	if ( OUTPUT_SWITCH == SDDF_OUTPUT ) {
	   HDFendTrace_SDDF( );
	} else if ( OUTPUT_SWITCH == RT_OUTPUT ) {
	   HDFendTrace_RT( );
	}
	procTrace = 0;
}
void startHDFtraceEvent(int eventID)
{
	if ( OUTPUT_SWITCH == SDDF_OUTPUT ) {
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
	if ( OUTPUT_SWITCH == SDDF_OUTPUT ) {
	   traceEvent( eventID, (char *)&info, 0 ) ;
	} else if (OUTPUT_SWITCH == RT_OUTPUT ) {
	   HDFtraceEvent_RT( eventID, &info, 0 ) ;
	} else if ( OUTPUT_SWITCH < 0 ) {
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

    if ( IOtracingEnabled ) {
        readArgs.fileID = c_mappedID( fd );
        readArgs.numVariables = 1;
        readArgs.cause = -1;   

        HDFtraceIOEvent( readBeginID, (void *) &readArgs, sizeof(readArgs) );
    }

    ret = read( fd, buf, nbyte );

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

    if ( IOtracingEnabled ) {
        readArgs.fileID = c_mappedID( fd );
        readArgs.numVariables = nitems;
        readArgs.cause = -1;
        HDFtraceIOEvent( freadBeginID, (void *) &readArgs, sizeof(readArgs) );
    }

    ret = fread( ptr, size, nitems, stream );

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

    if ( IOtracingEnabled ) {
        writeArgs.fileID = c_mappedID( fd );
        writeArgs.numVariables = 1;
        writeArgs.cause = -1;

        HDFtraceIOEvent( writeBeginID, (void *) &writeArgs, sizeof(writeArgs) );
    }

    ret = write( fd, buf, nbyte );

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

    if ( IOtracingEnabled ) {
        writeArgs.fileID = c_mappedID( fd );
        writeArgs.numVariables = nitems;
        writeArgs.cause = -1;

      HDFtraceIOEvent( fwriteBeginID, (void *) &writeArgs, sizeof(writeArgs) );
    }

    ret = fwrite( ptr, size, nitems, stream );

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
	size_t byte_req;
	byte_req = bytes;
	if ( IOtracingEnabled ) {
	   HDFtraceIOEvent ( ID_malloc, NULL, 0 );
	}
	
	ptr = malloc( bytes );

	if ( IOtracingEnabled ) {
	   HDFtraceIOEvent ( -ID_malloc, &byte_req, sizeof(size_t) );
	}
	
	return ptr ;

}
	
void HDFtraceIOEvent( int eventType, void *dataPtr, unsigned dataLen )
{
        if ( OUTPUT_SWITCH == 1 ) {
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
        TR_LOCK criticalSection;
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

        criticalSection = TRlock();
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
