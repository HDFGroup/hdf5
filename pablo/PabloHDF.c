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

#include "ProcIDs.h"
#include "HDFTrace.h"
#include "IOTrace.h"

#define	ID_HDFprocName		9996
#define	ID_malloc		9997
#define	ID_free			9998
#define	ID_timeStamp		9999
#define	DUMMY_HDF		10000

#ifdef H5_HAVE_PARALLEL
#include "mpi.h"
int HDF_get_Bytes( MPI_Datatype datatype, int count );
#endif /* H5_HAVE_PARALLEL*/

void HDFinitTrace_RT ( const char *, int );
void HDFinitTrace_SDDF ( const char *, int );
void hinittracex_ ( char [], int *, int[], int *,unsigned * );
void hdfendtrace_ ( void ) ;
void HDFendTrace_RT (int);
void HDFendTrace_SDDF(int);
void HDFfinalTimeStamp( void );
void startHDFtraceEvent (int );
int computeProcMask (int eventID);
int computePacketTag(int eventID);
void endHDFtraceEvent (int , int , char *, int );
void HDFtraceEvent_RT ( int , HDFsetInfo *, unsigned );
void HDFtraceIOEvent( int , void *, unsigned );
extern int IOtracingEnabled;
extern int suppressMPIOtrace;
char *hdfRecordPointer;
double WriteTotals = 0.0;
double ReadTotals = 0.0;
/*======================================================================*
// NAME									*
//     HDFinitTrace -- initialize HDF tracing				*
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
// fortran to C interface.                                              *
// This is called from hdfinittracef_                                   *
//======================================================================*/
void 
hinittracex_( char *file, 
              int *len, 
              int flags[], 
              int *nflags, 
              unsigned *out_sw )
{
   char *traceFileName;
   int i;
   traceFileName = (char *)malloc(*len+1);
   for ( i = 0; i < *len; ++i ) 
   {
      traceFileName[i] = file[i];
   }
   traceFileName[*len+1] = 0;
   /*===================================================================*
   // Allocate space for trace indicators.				*
   //===================================================================*/
   procTrace = ( int * ) malloc( NUM_HDF_IDS*sizeof(int) );
   if ( procTrace == NULL ) 
   {
       fprintf(stderr,">> Error: Unable to allocate procTrace ");
       fprintf(stderr,"array in program HDFinitTrace. <<<\n");
       fprintf(stderr,">>> Exiting program! <<<\n");
       exit (-1);
   }
   /*===================================================================*
   // Initialize to 0.							*
   //===================================================================*/
   for ( i = 0; i <= NUM_HDF_IDS; ++i ) 
   {
      procTrace[i] = 0;      
   }
   /*===================================================================*
   // Read in the flags indicating which procedures to trace.		*
   // The last parameter passed is an indicator of the type of 		*
   // tracing to do.  This indicator has a value larger than any   	*
   // of the flags.							*
   //===================================================================*/
   for ( i = 0; i < *nflags; ++i ) 
   {
      procTrace[flags[i]] = 1;
   }
   OUTPUT_SWITCH = *out_sw;
   /*===================================================================*
   // if no flags were passed, the default is to trace all of the  	*
   // procedures.							*
   //===================================================================*/
   if ( *nflags == 0 || procTrace[ID_ALLHDF] ) 
   {
      for ( i = 0; i < NUM_HDF_IDS; ++i ) 
      {
         procTrace[i] = 1;      
      }
   }
   if ( OUTPUT_SWITCH == RUNTIME_TRACE 
                        || OUTPUT_SWITCH == MPI_RUNTIME_TRACE ) 
   {
      HDFinitTrace_SDDF( traceFileName, OUTPUT_SWITCH );
      IOtracingEnabled = 1;
   } 
   else if ( OUTPUT_SWITCH == SUMMARY_TRACE 
                        || OUTPUT_SWITCH == MPI_SUMMARY_TRACE ) 
   {
      HDFinitTrace_RT( traceFileName, OUTPUT_SWITCH );
      IOtracingEnabled = 1;
   } 
   else if ( OUTPUT_SWITCH == NO_TRACE ) 
   {
      IOtracingEnabled = 0;
   } 
   else 
   {
      fprintf(stderr,">> Error in HDFinitTrace: the third argument ");
      fprintf(stderr,"must have a value between %4d<<\n",RUNTIME_TRACE);
      fprintf(stderr,">> and %4d, inclusive.",NO_TRACE);
      fprintf(stderr,"  The value received was %4u.", OUTPUT_SWITCH);
      fprintf(stderr," Exiting Program.     <<\n");
      exit (-1);
   }
}
void HDFinitTrace( const char *traceFileName, int id_flag, ... )
{
   int i, nIDs;
   va_list ap;

   /*===================================================================*
   // Allocate space for trace indicators.				*
   //===================================================================*/
   procTrace = ( int * ) malloc( NUM_HDF_IDS*sizeof(int) );
   if ( procTrace == NULL ) 
   {
      fprintf(stderr,">> Error: Unable to allocate procTrace ");
      fprintf(stderr,"array in program HDFinitTrace. <<<\n");
      fprintf(stderr,">>> Exiting program! <<<\n");
      exit (-1);
   }
   /*===================================================================*
   // Initialize to 0.							*
   //===================================================================*/
   for ( i = 0; i < NUM_HDF_IDS; ++i ) 
   {
      procTrace[i] = 0;      
   }
   /*===================================================================*
   // Read in the flags indicating which procedures to trace.		*
   // The last parameter passed is an indicator of the type of 		*
   // tracing to do.  This indicator has a value larger than any   	*
   // of the flags.							*
   //===================================================================*/
   nIDs = 0;
   va_start( ap, id_flag );
   while ( id_flag > NO_TRACE ) 
   {
      procTrace[id_flag] = 1;
      ++nIDs;
      id_flag = va_arg ( ap, int );
   }
   OUTPUT_SWITCH = id_flag;
   /*===================================================================*
   // if no flags were passed, the default is to trace all of the  	*
   // procedures.							*
   //===================================================================*/
   if ( nIDs == 0 || procTrace[ID_ALLHDF] ) 
   {
      for ( i = 0; i < NUM_HDF_IDS; ++i ) 
      {
         procTrace[i] = 1;      
      }
   }
   suppressMPIOtrace = TRUE;
   if ( OUTPUT_SWITCH == RUNTIME_TRACE 
                        || OUTPUT_SWITCH == MPI_RUNTIME_TRACE ) 
   {
      HDFinitTrace_SDDF( traceFileName, OUTPUT_SWITCH );
      IOtracingEnabled = 1;
   } 
   else if ( OUTPUT_SWITCH == SUMMARY_TRACE 
                        || OUTPUT_SWITCH == MPI_SUMMARY_TRACE ) 
   {
      HDFinitTrace_RT( traceFileName, OUTPUT_SWITCH );
      IOtracingEnabled = 1;
   } 
   else if ( OUTPUT_SWITCH == NO_TRACE ) 
   {
       IOtracingEnabled = 0;
   } 
   else 
   {
       fprintf(stderr,">> Error in HDFinitTrace: the third argument ");
       fprintf(stderr,"must have a value between %4d<<\n",RUNTIME_TRACE);
       fprintf(stderr,">> and %4d, inclusive.",NO_TRACE);
       fprintf(stderr,"  The value received was %4u.", OUTPUT_SWITCH);
       fprintf(stderr," Exiting Program.     <<\n");
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
   if ( OUTPUT_SWITCH == RUNTIME_TRACE 
	                     || OUTPUT_SWITCH == MPI_RUNTIME_TRACE ) 
   {
      HDFendTrace_SDDF( OUTPUT_SWITCH );
   } 
   else if ( OUTPUT_SWITCH == SUMMARY_TRACE 
                   || OUTPUT_SWITCH == MPI_SUMMARY_TRACE ) 
   {
      HDFendTrace_RT( OUTPUT_SWITCH );
   }
}
void startHDFtraceEvent(int eventID)
{
   if ( OUTPUT_SWITCH == RUNTIME_TRACE 
	                     || OUTPUT_SWITCH == MPI_RUNTIME_TRACE ) 
   {
      traceEvent( eventID, NULL, 0 ) ;
   } 
   else 
   {
      HDFtraceEvent_RT( eventID, NULL, 0 ) ;
   } 
}
void endHDFtraceEvent(int eventID, int setID, char *setName, int IDtype )
{
   HDFsetInfo info;
   info.setID = setID;
   info.setName = setName;
   if ( OUTPUT_SWITCH == RUNTIME_TRACE 
	                     || OUTPUT_SWITCH == MPI_RUNTIME_TRACE ) 
   {
      traceEvent( eventID, (char *)&info, 0 ) ;
   } 
   else if ( OUTPUT_SWITCH == SUMMARY_TRACE 
	                     || OUTPUT_SWITCH == MPI_SUMMARY_TRACE ) 
   {
      HDFtraceEvent_RT( eventID, &info, 0 ) ;
   } 
   else if ( OUTPUT_SWITCH != NO_TRACE ) 
   {
      fprintf(stderr,"endHDFtraceEvent: ");
      fprintf(stderr,"invalid OUTPUT_SWITCH %d, IDtype = %d\n",
                                                  OUTPUT_SWITCH, IDtype ) ;
   }
}
/*****************************************************************************/
/* The HDFtraceXXXX routines are substitutes for the standard I/O routines.  */
/* When libhdf5-inst.a is compiled, macros in HDFTrace.h substitute the name */
/* HDFtraceWrite for write, HDFtraceRead for Read, etc.  These routines are  */
/* then called when standard I/O is done.				     */
/*****************************************************************************/
/*****************************************************************************/
/*+	Open routines      	   	                                    +*/
/*+     -------------            				            +*/
/*+	                                                                    +*/
/*+ Routine:  FILE *HDFtraceFOPEN( const char *filename, const char *type ) +*/
/*+		  substitute for fopen()                                    +*/
/*+     	  generates fopenBeginID, fopenEndID		            +*/
/*+		  record Open (fopenBegin)                                  +*/
/*+			Mode = -1                                           +*/
/*+									    +*/
/*****************************************************************************/
FILE*HDFtraceFOPEN( const char *filename, const char *type )
{
   FILE *fp;
   int fd, id;
   int flags = 0;
   struct open_args openArgs;
   size_t typeLen;
	
   if ( IOtracingEnabled ) 
   {
      strcpy( openArgs.filename, filename );
	 
      /* check for 'b' - usually if 2 chars, second is '+' */
      typeLen = strlen( type );
      if ( ( typeLen == 2 ) && ( type [1] == 'b' ) ) 
      {
         typeLen = 1;
      }

      if ( typeLen == 1 ) 
      {
         switch( type[0] ) 
         {
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
      } 
      else 
      {
         switch( type[0] ) 
         {
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
   if ( fp != NULL ) 
   {
      fd = fileno( fp );
      id = set_c_mappedID( fd );
   } 
   else 
   {
      id = -1;
   }

   if ( IOtracingEnabled ) 
   {
     HDFtraceIOEvent( fopenEndID, (void *) &id, int_SIZE );   
   }

   return( fp );
}

/*****************************************************************************/
/*+	Routine:  int HDFtraceCREAT( const char *path, mode_t mode )        +*/
/*+		  substitute for creat()                                    +*/
/*+     	  generates openBeginID, openEndID		            +*/
/*+		  record Open (openBeginID)                                 +*/
/*+									    +*/
/*****************************************************************************/
int 
HDFtraceCREAT( const char *path, mode_t mode )
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

/*****************************************************************************/
/*+	Flush routines				                            +*/
/*+     --------------				                            +*/
/*+	                                                                    +*/
/*+	Routine:  int HDFtraceFFLUSH( FILE *stream )                        +*/
/*+		  substitute for fflush()                                   +*/
/*+               generates fflushBeginID, fflushEndID                      +*/
/*+	          record Flush (fflushBeginID)                              +*/
/*+		   							    +*/
/*****************************************************************************/
int HDFtraceFFLUSH( FILE *stream )
{
   int ret;
   int id;
   int fd;

   if ( IOtracingEnabled ) 
   {
      /*
       * If stream is NULL, all files open for write are flushed.
       * We show this with a -2 in the trace record as too much overhead
       * to try and tell what files those are and generate individual
       * trace records.
       */
      if ( stream == NULL ) 
      {   
         id = -2;
      } 
      else 
      {
         fd = fileno( stream );
         id = c_mappedID( fd );
         HDFtraceIOEvent( fflushBeginID, (void *) 0, int_SIZE );
     }
   }

   ret = fflush( stream );

   if ( IOtracingEnabled ) 
   {
      HDFtraceIOEvent( fflushEndID, (void *) &id, 0 );
   }

   /* 
    * Note that if fflush called on stream open for reading, the file pointer
    * is moved to EOF if it isn't there already.  We don't account for that
    * in our file positioning information.
    */

   return( ret );
}

/*****************************************************************************/
/*+	Close routines				                            +*/
/*+     --------------				                            +*/
/*+	                                                                    +*/
/*+	Routine:  int HDFtraceFCLOSE( FILE *stream )                        +*/
/*+		  substitute for fclose()                                   +*/
/*+               generates fcloseBeginID, fcloseEndID                      +*/
/*+	          record Close (fcloseBeginID)                              +*/
/*+									    +*/
/*****************************************************************************/
int 
HDFtraceFCLOSE( FILE *stream )
{
   int ret;
   int id;
   int fd = fileno( stream );

   if ( IOtracingEnabled ) 
   {
      id = c_mappedID( fd );
      HDFtraceIOEvent( fcloseBeginID, (void *) &id, int_SIZE );
   }

   ret = fclose( stream );

   if ( IOtracingEnabled ) 
   {
      HDFtraceIOEvent( fcloseEndID, (void *) 0, 0 );
   }

   return( ret );
}

/*****************************************************************************/
/*+     Routine:  int HDFtrace3OPEN( char *path, int flags, mode_t mode )   +*/
/*+               substitute for open() when called with 3 arguments        +*/
/*+               generates openBeginID, openEndID                          +*/
/*+               record Open (openBeginID)                                 +*/
/*+                                                                         +*/
/*****************************************************************************/
int 
HDFtrace3OPEN( const char *path, int flags, mode_t mode )
{
   struct open_args openArgs;
   int fd;
   int id;

   if ( IOtracingEnabled ) 
   {
      strcpy( openArgs.filename, path );
      openArgs.flags = flags;
      openArgs.mode = (int) mode;

      HDFtraceIOEvent( openBeginID, (char *)&openArgs, sizeof(openArgs) );
   }

   fd = open( path, flags, mode );
   id = set_c_mappedID( fd );

   if ( IOtracingEnabled ) 
   {
      HDFtraceIOEvent( openEndID, (char *) &id, int_SIZE );
   }

   return( fd );
}

/*****************************************************************************/
/*+	Routine:  int HDFtraceCLOSE( int fd )	                            +*/
/*+		  substitute for close()              			    +*/
/*+               generates closeBeginID, closeEndID 		            +*/
/*+		  record Close (closeBeginID)                               +*/
/*+									    +*/
/*****************************************************************************/
int 
HDFtraceCLOSE( int fd )
{
   int ret;
   int id;

   if ( IOtracingEnabled ) 
   {
      id = c_mappedID( fd );
      HDFtraceIOEvent( closeBeginID, (void *) &id, int_SIZE );
   }

   ret = close( fd );

   if ( IOtracingEnabled ) 
   {
      HDFtraceIOEvent( closeEndID, (void *) 0, 0 );
   }

   return( ret );
}

/*****************************************************************************/
/*+	Read routines	            			                    +*/
/*+     -------------			            	                    +*/
/*+	                                                                    +*/
/*+	Routine:  int HDFtraceREAD( int fd, char *buf, int nbyte )          +*/
/*+		  substitute for read()                                     +*/
/*+               generates readBeginID, readEndID                          +*/
/*+	          record Read (readBeginID)                                 +*/
/*+                      Number Variables = 1                               +*/
/*+			 Cause = -1                                         +*/
/*+									    +*/
/*****************************************************************************/
ssize_t 
HDFtraceREAD( int fd, void *buf, size_t nbyte )
{
   struct read_write_args readArgs;  
   ssize_t ret;
   int bytes;
   CLOCK t1, t2, incDur;

   if ( IOtracingEnabled ) 
   {
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

   if ( IOtracingEnabled ) 
   {
      if ( ret > 0 ) 
      {
         bytes = (int)ret;
      } 
      else 
      { 
         bytes = 0;
      } 
      HDFtraceIOEvent( readEndID, (void *) &bytes, int_SIZE );
   }

   return( ret );
}
        
/*****************************************************************************/
/*+	Routine:  int HDFtraceFREAD( char *ptr, int size, int nitems,       +*/
/*+			          FILE *stream)                             +*/
/*+		  substitute for fread()                                    +*/
/*+               generates freadBeginID, freadEndID                        +*/
/*+	          record Read (freadBeginID)                                +*/
/*+                      Number Variables = nitems                          +*/
/*+			 Cause = -1                                         +*/
/*+									    +*/
/*****************************************************************************/
size_t 
HDFtraceFREAD( void *ptr, size_t size, size_t nitems, FILE *stream )
{
   struct read_write_args readArgs;  
   size_t ret;
   int nbytes;
   int fd = fileno( stream );
   CLOCK t1, t2, incDur;

   if ( IOtracingEnabled ) 
   {
      readArgs.fileID = c_mappedID( fd );
      readArgs.numVariables = (int)nitems;
      readArgs.cause = -1;
      HDFtraceIOEvent( freadBeginID, (void *) &readArgs, sizeof(readArgs) );
   }

   t1 = getClock();
   ret = fread( ptr, size, nitems, stream );
   t2 = getClock();
   incDur = clockSubtract(t2,t1);
   ReadTotals += clockToSeconds( incDur );

   if ( IOtracingEnabled ) 
   {
      if ( ret > 0 ) 
      {
         nbytes = (int)(ret * size) ;
      } 
      else 
      {
         nbytes = 0;
      }
      HDFtraceIOEvent( freadEndID, (void *) &nbytes, int_SIZE );
   }

   return( ret );
}

       
/*****************************************************************************/
/*+	Seek routines            			                    +*/
/*+     -------------			            	                    +*/
/*+	                                                                    +*/
/*+	Routine:  off_t HDFtraceLSEEK( int fd, off_t offset, int whence )   +*/
/*+		  substitute for lseek()                                    +*/
/*+               generates lseekBeginID, lseekEndID                        +*/
/*+	          record Seek (lseekBeginID)                                +*/
/*+									     +*/
/*****************************************************************************/
off_t 
HDFtraceLSEEK( int fd, off_t offset, int whence )
{
   struct seek_args seekArgs;
   off_t ret;
   long  arg;

   if ( IOtracingEnabled ) 
   {
      seekArgs.fileID = c_mappedID( fd );
      seekArgs.offset = (int) offset;
      seekArgs.whence = whence;

      HDFtraceIOEvent( lseekBeginID, (void *) &seekArgs, sizeof(seekArgs) );
   }

   ret = lseek( fd, offset, whence );

   if ( IOtracingEnabled ) 
   {
      arg = (long) ret;
      HDFtraceIOEvent( lseekEndID, (void *)&arg, long_SIZE );
   }

   return( ret );
}

/*****************************************************************************/
/*+ routine:  int HDF traceFSEEK( FILE *stream, long offset, int whence )   +*/
/*+		  substitute for fseek()                                    +*/
/*+               generates fseekBeginID, fseekEndID                        +*/
/*+	          record Seek (fseekBeginID)                                +*/
/*+									    +*/
/*****************************************************************************/
int 
HDFtraceFSEEK( FILE *stream, long offset, int whence )
{
   struct seek_args seekArgs;
   int ret;
   long arg;
   int fd = fileno( stream );

   if ( IOtracingEnabled ) 
   {
      seekArgs.fileID = c_mappedID( fd );;
      seekArgs.offset = (int) offset;
      seekArgs.whence = whence;

      HDFtraceIOEvent( fseekBeginID, (void *) &seekArgs, sizeof(seekArgs) );
   }

   ret = fseek( stream, offset, whence );

   if ( IOtracingEnabled ) 
   {
      arg = ftell( stream );
      HDFtraceIOEvent( fseekEndID, (void *)&arg, long_SIZE );
   }

   return( ret );
}

#ifdef fpos_t
/*****************************************************************************/
/*+ Routine:  int HDFtraceFSETPOS( FILE *stream, const fpos_t *position )   +*/
/*+		  substitute for fsetpos()                                  +*/
/*+               generates fsetposBeginID, fsetposEndID                    +*/
/*+	          record Seek (fsetposBeginID)                              +*/
/*+									    +*/
/*****************************************************************************/
int 
HDFtraceFSETPOS( FILE stream, const fpos_t *position )
{
   struct seek_args seekArgs;
   int ret;
   long arg;
   int fd = fileno( stream );

   if ( IOtracingEnabled ) 
   {
      seekArgs.fileID = c_mappedID( fd );;
      seekArgs.offset = (int) *position;
      seekArgs.whence = SEEK_SET;

      HDFtraceIOEvent( fsetposBeginID, (void *) &seekArgs, sizeof(seekArgs) );
   }

   ret = fsetpos( stream, position );

   if ( IOtracingEnabled ) 
   {
      arg = (long) *position;
      HDFtraceIOEvent( fsetposEndID, (void *)&arg, long_SIZE );
   }

   return( ret );
}
#endif /* fpos_t */

/*****************************************************************************/
/*+	Routine:  void HDFtraceREWIND ( FILE *stream )                      +*/
/*+		  substitute for rewind()                                   +*/
/*+               generates rewindBeginID, rewindEndID                      +*/
/*+	          record Seek (rewindBeginID)                               +*/
/*+                    	 Offset = 0                                         +*/
/*+			 Whence = SEEK_SET                                  +*/
/*+									    +*/
/*****************************************************************************/
void HDFtraceREWIND( FILE *stream )
{
   struct seek_args seekArgs;
   long arg;
   int fd = fileno( stream );

   if ( IOtracingEnabled ) 
   {
      seekArgs.fileID = c_mappedID( fd );
      seekArgs.offset = 0;
      seekArgs.whence = SEEK_SET;

      HDFtraceIOEvent( rewindBeginID, (void *) &seekArgs, sizeof(seekArgs) );
   }

   rewind( stream );

   if ( IOtracingEnabled ) 
   {
      arg = 0;
      HDFtraceIOEvent( rewindEndID, (void *)&arg, long_SIZE );
   }

   return;
}

/*****************************************************************************/
/*+	Write routines            			                    +*/
/*+     --------------			            	                    +*/
/*+	                                                                    +*/
/*+  Routine:  int HDFtraceWRITE( int fd, char *buf, int nbyte )            +*/
/*+		  substitute for write()                                    +*/
/*+               generates writeBeginID, writeEndID                        +*/
/*+	          record Write (writeBeginID)                               +*/
/*+                    	 Number Variables = 1                               +*/
/*+			 Cause = -1                                         +*/
/*+									    +*/
/*****************************************************************************/
ssize_t HDFtraceWRITE( int fd, const void *buf, size_t nbyte )
{
   struct read_write_args writeArgs;
   ssize_t ret;
   int bytes;
   CLOCK t1, t2, incDur;

   if ( IOtracingEnabled ) 
   {
      writeArgs.fileID = c_mappedID( fd );
      writeArgs.numVariables = 1;
      writeArgs.cause = -1;

      HDFtraceIOEvent( writeBeginID, (void *) &writeArgs, sizeof(writeArgs) );
   }

   t1 = getClock();
   ret = write( fd, buf, nbyte );
   t2 = getClock();
   incDur = clockSubtract(t2,t1);
   WriteTotals += clockToSeconds( incDur );

   if ( IOtracingEnabled ) 
   {
      if ( ret > 0 ) 
      {
         bytes =  (int)ret;
      } 
      else 
      {
         bytes = 0;
      }
      HDFtraceIOEvent( writeEndID, (void *) &bytes, int_SIZE );
   }
   return( ret );
}  

/*****************************************************************************/
/*+  Routine:  size_t HDFtraceFWRITE( const char *ptr, int size, int nitems,+*/
/*+                                FILE *stream )                           +*/
/*+		  substitute for fwrite()                                   +*/
/*+               generates fwriteBeginID, fwriteEndID                      +*/
/*+	          record Write (fwriteBeginID)                              +*/
/*+                    	 Number Variables = nitems                          +*/
/*+			 Cause = -1                                         +*/
/*+									    +*/
/*****************************************************************************/
size_t 
HDFtraceFWRITE(const void *ptr,size_t size,size_t nitems,FILE *stream )

{
   struct read_write_args writeArgs;
   size_t ret;
   int nbytes;
   int fd = fileno( stream );
   CLOCK t1, t2, incDur;

   if ( IOtracingEnabled ) 
   {
      writeArgs.fileID = c_mappedID( fd );
      writeArgs.numVariables = (int)nitems;
      writeArgs.cause = -1;

      HDFtraceIOEvent(fwriteBeginID, (void *)&writeArgs, sizeof(writeArgs));
   }

   t1 = getClock();
   ret = fwrite( ptr, size, nitems, stream );
   t2 = getClock();
   incDur = clockSubtract(t2,t1);
   WriteTotals += clockToSeconds( incDur );


   if ( IOtracingEnabled ) 
   {
      if ( ret > 0 ) 
      {
         nbytes = (int)(ret * size) ;
      } 
      else 
      {
         nbytes = 0;
      } 
      HDFtraceIOEvent( fwriteEndID, (void *) &nbytes, int_SIZE );
   }

   return( ret );
}

/*****************************************************************************/
/*+  Routine:  int HDFtracePUTS( char *s )                                  +*/
/*+		  substitute for puts()                                     +*/
/*+               generates fwriteBeginID, fwriteEndID                      +*/
/*+	          record Write (fwriteBeginID)                              +*/
/*+                    	 Number Variables = 1                               +*/
/*+			 Cause = -1                                         +*/
/*+									    +*/
/*****************************************************************************/
int 
HDFtracePUTS( char *s )
{
   struct read_write_args writeArgs;
   int ret;
   int fd = fileno( stdout );

   if ( IOtracingEnabled ) 
   {
      writeArgs.fileID = c_mappedID( fd );
      writeArgs.numVariables = 1;
      writeArgs.cause = -1;

      HDFtraceIOEvent( fwriteBeginID, (void *) &writeArgs, sizeof(writeArgs) );
   }

   ret = puts( s );

   if ( IOtracingEnabled ) 
   {
      HDFtraceIOEvent( fwriteEndID, (void *) &ret, int_SIZE );
   }

   return( ret );
}
       
/*****************************************************************************/
/*+	Routine:  int HDFtraceFPUTC( int c, FILE *stream )                  +*/
/*+		  substitute for fputc()                                    +*/
/*+               generates fwriteBeginID, fwriteEndID                      +*/
/*+	          record Write (fwriteBeginID)                              +*/
/*+                    	 Number Variables = 1                               +*/
/*+			 Cause = -1                                         +*/
/*+									    +*/
/*****************************************************************************/
int 
HDFtraceFPUTC( int c, FILE *stream )
{
   struct read_write_args writeArgs;
   int ret; 
   int nbytes = char_SIZE;
   int fd = fileno( stream );

   if ( IOtracingEnabled ) 
   {
      writeArgs.fileID = c_mappedID( fd );
      writeArgs.numVariables = 1;
      writeArgs.cause = -1;

      HDFtraceIOEvent( fwriteBeginID, (void *) &writeArgs, sizeof(writeArgs) );
   }

   ret = fputc( c, stream );

   if ( IOtracingEnabled ) 
   {
      if ( ret == EOF ) 
      {
         nbytes = 0;
         HDFtraceIOEvent( fwriteEndID, (void *) &nbytes, int_SIZE );
      }
   }

    return( ret );
}
/*****************************************************************************/
/*+  Routine:  int HDFtraceFPUTS( char *s, FILE *stream )                   +*/
/*+		  substitute for fputs()                                    +*/
/*+               generates fwriteBeginID, fwriteEndID                      +*/
/*+	          record Write (fwriteBeginID)                              +*/
/*+                    	 Number Variables = 1                               +*/
/*+			 Cause = -1                                         +*/
/*+									    +*/
/*****************************************************************************/
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
// HDF procedure and HDF library file.  If a procedure or all of the  *
// procedure in an HDF file are to be traced, then the elemen in the 	*
// array corresponding to the procedure or file is turned on.  This is  *
// used by the macros TRACE_ON and TRACE_OFF to enable tracing.  If     *
// this procedure is not called prior to initialization, then all of    *
// the elements of procTrace corresponding to HDF files will be 	*
// turned on, in which case all HDF procedures will be traced.	*
//======================================================================*/
void PabloHDFTrace( int ID ) 
{
	int i;
	if ( procTrace == NULL ) {
	   procTrace = ( int * ) malloc( NUM_HDF_IDS*sizeof(int) );
	   if ( procTrace == NULL ) {
              fprintf(stderr,">> Error: Unable to allocate procTrace ");
              fprintf(stderr,"array in program PabloHDFTrace. <<<\n");
              fprintf(stderr," Exiting program.                  <<<\n");
              exit (-1);
	   }
	   for ( i = 0; i < NUM_HDF_IDS; ++i ) {
	       procTrace[i] = 0;      
	   }
	}
	if ( ID >= 0 && ID < NUM_HDF_IDS ) {
	   procTrace[ID] = 1;      
	} else {
           fprintf(stderr,">> Error: Value passed to PabloHDFTrace, ");
           fprintf(stderr,"%d, is out of range. <<<\n",ID);
           fprintf(stderr," Exiting program.                  <<<\n");
           exit (-1);
	}
}
#ifdef H5_HAVE_PARALLEL
#include "MPIO_Trace.h"
#include "MPIO_EventArgs.h"
#include "MPIO_Data.h"

/* Global variable declarations and definitions. */
static int HDFlocalNode = 0;
int HDFmyNode;
int myHDFid = 3;
/* Function prototypes */
int HDF_get_mode( int );
int HDF_get_source( int );
int HDF_get_comm( MPI_Comm );
int HDF_get_Datatype( MPI_Datatype );
int HDF_get_DataRep( char* );
int HDF_get_Bytes( MPI_Datatype, int );

/* Get the mode at the file openning */
int HDF_get_mode( int amode )
{
   if( amode == MPI_MODE_RDONLY || amode == MPI_MODE_RDWR ||
       amode == MPI_MODE_WRONLY || amode == MPI_MODE_CREATE ||
       amode == MPI_MODE_EXCL || amode == MPI_MODE_DELETE_ON_CLOSE ||
       amode == MPI_MODE_UNIQUE_OPEN || 
       /* amode == MPI_MODE_SEQUENTIAL || */
       amode == MPI_MODE_APPEND )
      return amode;
   else
      return PABLO_MPI_MODE_NULL;
}

/* Get the node number */
int HDF_get_source( int source )
{
   if ( source == MPI_ANY_SOURCE ) {
      return PABLO_MPI_ANYSOURCE;
   }
   
   if ( source == MPI_PROC_NULL ) {
      return PABLO_MPI_PROCNULL;
   }
   
   else {
      return  source;
   }
}

/* get the communicator ID                            */
/* this is dummy for compatability with MPIO Traceing */
int HDF_get_comm( MPI_Comm in_comm )
{
   return 0;
}
/* Get the MPI_Datatype (mapped to a integer) */
int HDF_get_Datatype( MPI_Datatype datatype )
{
   /*  elementary  datatypes (C) */
   if ( datatype ==  MPI_CHAR )
      return PABLO_MPI_CHAR;
   else if ( datatype ==  MPI_SHORT )
      return PABLO_MPI_SHORT; 
   else if ( datatype == MPI_INT )
      return PABLO_MPI_INT;
   else if ( datatype ==  MPI_LONG ) 
      return PABLO_MPI_LONG;  
   else if ( datatype ==  MPI_UNSIGNED_CHAR )
      return PABLO_MPI_UNSIGNED_CHAR; 
   else if ( datatype ==  MPI_UNSIGNED_SHORT )
      return PABLO_MPI_UNSIGNED_SHORT;
   else if ( datatype ==  MPI_UNSIGNED )
      return PABLO_MPI_UNSIGNED;  
   else if ( datatype ==  MPI_UNSIGNED_LONG )
      return PABLO_MPI_UNSIGNED_LONG;
  else if ( datatype ==  MPI_FLOAT )
     return PABLO_MPI_FLOAT;  
   else if ( datatype ==  MPI_DOUBLE )
      return PABLO_MPI_DOUBLE; 
   else if ( datatype ==  MPI_LONG_DOUBLE )
      return PABLO_MPI_LONG_DOUBLE; 
   
   /* elementary  datatypes (FORTRAN) */
#ifdef MPI_INTEGER
   else if ( datatype ==  MPI_INTEGER )	
      return PABLO_MPI_INTEGER;   
#endif
#ifdef MPI_REAL       	
   else if ( datatype ==  MPI_REAL )
      return PABLO_MPI_REAL; 	
#endif
#ifdef MPI_DOUBLE_PRECISION
   else if ( datatype ==  MPI_DOUBLE_PRECISION )
      return PABLO_MPI_DOUBLE_PRECISION; 
#endif
#ifdef MPI_COMPLEX
   else if ( datatype ==  MPI_COMPLEX )   
      return PABLO_MPI_COMPLEX; 
#endif
#ifdef MPI_DOUBLE_COMPLEX		
   else if ( datatype ==  MPI_DOUBLE_COMPLEX )	
      return PABLO_MPI_DOUBLE_COMPLEX; 	
#endif
#ifdef MPI_LOGICAL
   else if ( datatype ==  MPI_LOGICAL )	
      return PABLO_MPI_LOGICAL;
#endif
#ifdef MPI_CHARACTER
   else if ( datatype ==  MPI_CHARACTER )	
      return PABLO_MPI_CHARACTER;
#endif 	
      
   /*  other  datatypes (C, Fortran).*/
   else if ( datatype ==  MPI_BYTE )
      return PABLO_MPI_BYTE;         
   else if ( datatype ==  MPI_PACKED )
      return PABLO_MPI_PACKED;  
   else if ( datatype ==  MPI_LB )
      return PABLO_MPI_LB;         
   else if ( datatype ==  MPI_UB )
      return PABLO_MPI_UB;  
   
   
   /*  reduction  datatypes (C).	*/
   else if ( datatype ==  MPI_FLOAT_INT )
      return PABLO_MPI_FLOAT_INT; 	
   else if ( datatype ==  MPI_DOUBLE_INT )
      return PABLO_MPI_DOUBLE_INT; 	
   else if ( datatype ==  MPI_LONG_INT )
      return PABLO_MPI_LONG_INT; 	
   else if ( datatype ==  MPI_2INT )
      return PABLO_MPI_2INT; 		
   else if ( datatype ==  MPI_SHORT_INT )
      return PABLO_MPI_SHORT_INT; 	
   else if ( datatype ==  MPI_LONG_DOUBLE_INT )
      return PABLO_MPI_LONG_DOUBLE_INT;
   
   /* Reduction datatypes (FORTRAN) */
#ifdef MPI_2REAL
   else if ( datatype ==  MPI_2REAL )
      return PABLO_MPI_2REAL; 	
#endif
#ifdef MPI_2DOUBLE_PRECISION	
   else if ( datatype ==  MPI_2DOUBLE_PRECISION )
      return PABLO_MPI_2DOUBLE_PRECISION;
#endif
#ifdef MPI_2INTEGER 		
   else if ( datatype ==  MPI_2INTEGER )
      return PABLO_MPI_2INTEGER; 		
#endif

#ifdef MPI_2COMPLEX
   else if ( datatype ==  MPI_2COMPLEX )
      return PABLO_MPI_2COMPLEX; 
#endif
#ifdef 	MPI_2DOUBLE_COMPLEX
   else if ( datatype ==  MPI_2DOUBLE_COMPLEX )
      return PABLO_MPI_2DOUBLE_COMPLEX; 
#endif		
   
/*  optional  datatypes (C).*/
#ifdef MPI_LONG_LONG_INT
   else if ( datatype ==  MPI_LONG_LONG_INT )
      return PABLO_MPI_LONG_LONG_INT;
#endif
   else if ( datatype ==  MPI_DATATYPE_NULL ) 	
      return PABLO_MPI_DATATYPE_NULL; 		
   else
      return PABLO_MPI_USER_DEF;  
}

/* data representations */
int HDF_get_DataRep( char* datarep )
{
   if ( !strcmp( datarep, "native" ) )
      return PABLO_MPI_NATIVE;
   else if ( !strcmp( datarep, "internal" ) )
      return PABLO_MPI_INTERNAL;
   else if ( !strcmp( datarep, "external32" ) )
      return PABLO_MPI_EXTERNAL32;
   else
      return (-1);
}

/*****************************************************************************/
/* The routines below are there to bypass the MPI I/O Tracing.  When MPI I/O */
/* tracing is done with a nonstandard MPI I/O implementation, problems can   */
/* occur in linking and in data recording.				     */
/* For each of the MPI I/O routines MPI_File_xxx used in HDF, there are two  */
/* entries: HDF_MPI_File_xxx and PabloMPI_File_xxx.	Macros replace the   */
/* MPI_File_xxx call with HDF_MPI_File_xxx.  	                             */
/* If SUMMARY Tracing is used                                                */
/* HDF_MPI_File_xxx routine will record entry data in a table, call the      */
/* IF RUNTIME Tracing is used						     */
/* HDF_MPI_File_xxx routine calls PabloMPI_File_xxx.  This routine writes    */
/* data to the trace file, calls the standard MPI_File_xxx routine, then     */
/* records exit data to a trace file.					     */
/* The PabloMPI_File_xxx functionality could have been incorporated into the */
/* HDF_MPI_File_xxx routine, but was not done so for expediency.	     */
/*****************************************************************************/
int PabloMPI_File_open( MPI_Comm comm,
                   char *filename,
                   int amode,
                   MPI_Info info,
                   MPI_File *fh ) ;

int PabloMPI_File_close( MPI_File *fh ) ;

int PabloMPI_File_set_size( MPI_File fh, MPI_Offset size ) ;

int PabloMPI_File_get_size( MPI_File fh, MPI_Offset *size ) ;

int PabloMPI_File_set_view( MPI_File fh,
                       MPI_Offset disp,
                       MPI_Datatype etype,
                       MPI_Datatype filetype,
                       char *datarep,
                       MPI_Info info ) ;

int PabloMPI_File_get_view( MPI_File fh,
                       MPI_Offset *disp,
                       MPI_Datatype *etype,
                       MPI_Datatype *filetype,
                       char *datarep ) ;

int PabloMPI_File_read_at( MPI_File fh,
                      MPI_Offset offset,
                      void *buf,
                      int count,
                      MPI_Datatype datatype,
                      MPI_Status *status ) ;

int PabloMPI_File_read_at_all( MPI_File fh,
                          MPI_Offset offset,
                          void *buf,
                          int count,
                          MPI_Datatype datatype,
                          MPI_Status *status ) ;

int PabloMPI_File_write_at( MPI_File fh,
                       MPI_Offset offset,
                       void *buf,
                       int count,
                       MPI_Datatype datatype,
                       MPI_Status *status ) ;

int PabloMPI_File_write_at_all( MPI_File fh,
                           MPI_Offset offset,
                           void *buf,
                           int count,
                           MPI_Datatype datatype,
                           MPI_Status *status ) ;

int 
PabloMPI_File_sync( MPI_File fh ) ; 

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
   
   if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) 
   {
      returnVal = PabloMPI_File_open( comm, filename, amode, info, fh );
   } 
   else 
   {
      dataLen = sizeof( HDFsetInfo );
      dataPtr.setID = 0;
      dataPtr.setName = (char *)malloc( strlen(filename) + 1);
      strcpy( dataPtr.setName , filename );
      HDFtraceEvent_RT( HDFmpiOpenID, &dataPtr, dataLen );
      returnVal = MPI_File_open( comm, filename, amode, info, fh );
      dataPtr.setID = (long)fh;
      HDFtraceEvent_RT( -HDFmpiOpenID, &dataPtr, dataLen );
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
   
   if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) 
   {
      returnVal = PabloMPI_File_close( fh );
   } 
   else 
   {
      dataLen = sizeof( HDFsetInfo );
      dataPtr.setID = (long)fh;
      dataPtr.setName = NULL;
      HDFtraceEvent_RT( HDFmpiCloseID, &dataPtr, dataLen );
      returnVal = MPI_File_close( fh );
      HDFtraceEvent_RT( -HDFmpiCloseID, &dataPtr, dataLen );
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
   
   if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) 
   {
      returnVal = PabloMPI_File_set_size( fh, size );
   } 
   else 
   {
      dataLen = 1;
      dataPtr.setID = (long)fh;
      HDFtraceEvent_RT( HDFmpiSetSizeID,&dataPtr,dataLen );
      returnVal = MPI_File_set_size( fh, size );
      HDFtraceEvent_RT( -HDFmpiSetSizeID, &dataPtr, dataLen );
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
           returnVal = PabloMPI_File_get_size( fh, size);
	} else {
	   dataLen = 1;
           dataPtr.setID = (long)fh;
           HDFtraceEvent_RT( HDFmpiGetSizeID,
                             &dataPtr,dataLen );
           returnVal = MPI_File_get_size( fh, size);
           HDFtraceEvent_RT( -HDFmpiGetSizeID,
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
   	   returnVal = PabloMPI_File_set_view( fh, disp, etype, filetype, 
                                                      datarep, info );
	} else {
	   dataLen = 1;
           dataPtr.setID = (long)fh;
           HDFtraceEvent_RT( HDFmpiSetViewID,
                             &dataPtr,dataLen );
   	   returnVal = MPI_File_set_view( fh, disp, etype, filetype, 
                                                      datarep, info );
           HDFtraceEvent_RT( -HDFmpiSetViewID,
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

   if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) 
   {
      returnVal = PabloMPI_File_get_view(fh, disp, etype, filetype, datarep);
   } 
   else 
   {
      dataLen = 1;
      dataPtr.setID = (long)fh;
      HDFtraceEvent_RT( HDFmpiSetViewID,
                             &dataPtr,dataLen );
      returnVal = MPI_File_get_view(fh, disp, etype, filetype, datarep);
      HDFtraceEvent_RT( -HDFmpiSetViewID, &dataPtr,dataLen );
      returnVal = MPI_File_get_view(fh, disp, etype, filetype, datarep);

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
        int rCount;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
   	   returnVal = PabloMPI_File_read_at( fh, offset, buf, count, datatype, 
	                                                            status );
	} else {
	   dataLen = sizeof(dataPtr);
           dataPtr.setID = (long)fh;
           dataPtr.numBytes = HDF_get_Bytes( datatype, count );
           HDFtraceEvent_RT( HDFmpiReadAtID,
                             &dataPtr,dataLen );
   	   returnVal = MPI_File_read_at( fh, offset, buf, count, datatype, 
	                                                            status );
           MPI_Get_count(status,datatype,&rCount);
           if ( rCount < 0 || rCount > count ) 
           {
              dataPtr.numBytes = -1;
           }
           else
           {
              dataPtr.numBytes = HDF_get_Bytes( datatype, rCount );
           }
           HDFtraceEvent_RT( -HDFmpiReadAtID,
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
        int rCount;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
   	   returnVal = PabloMPI_File_read_at_all( fh, offset, buf, 
				      count, datatype, status );
	} else {
	   dataLen = sizeof(dataPtr);
           dataPtr.setID = (long)fh;
           dataPtr.numBytes = HDF_get_Bytes( datatype, count );
           HDFtraceEvent_RT( HDFmpiReadAtAllID,
                             &dataPtr,dataLen );
   	   returnVal = MPI_File_read_at_all( fh, offset, buf, 
				      count, datatype, status );
           MPI_Get_count(status,datatype,&rCount);
           if ( rCount < 0 || rCount > count ) 
           {
              dataPtr.numBytes = -1;
           }
           else
           {
              dataPtr.numBytes = HDF_get_Bytes( datatype, rCount );
           }
           HDFtraceEvent_RT( -HDFmpiReadAtAllID,
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
   int rCount;

   if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) 
   {
      returnVal = PabloMPI_File_write_at( fh, offset, buf, count, datatype, 
                                                              status );
   } 
   else 
   {
      dataLen = sizeof(dataPtr);
      dataPtr.setID = (long)fh;
      dataPtr.numBytes = HDF_get_Bytes( datatype, count );
      HDFtraceEvent_RT( HDFmpiWriteAtID, &dataPtr,dataLen );
      returnVal = MPI_File_write_at( fh, offset, buf, count, datatype, 
                                                               status );
      MPI_Get_count(status,datatype,&rCount);
      if ( rCount < 0 || rCount > count ) 
      {
         dataPtr.numBytes = -1;
      }
      else
      {
         dataPtr.numBytes = HDF_get_Bytes( datatype, rCount );
      }
      HDFtraceEvent_RT( -HDFmpiWriteAtID, &dataPtr,dataLen );
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
        int numBytes;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) {
   	   returnVal = PabloMPI_File_write_at_all( fh, offset, buf, 
	 	 	       		count, datatype, status );
	} else {
	   dataLen = sizeof(dataPtr);
           dataPtr.setID = (long)fh;
           dataPtr.numBytes = HDF_get_Bytes( datatype, count );
           HDFtraceEvent_RT( HDFmpiWriteAtAllID,
                             &dataPtr,dataLen );
   	   returnVal = MPI_File_write_at_all( fh, offset, buf, 
	 			       count, datatype, status );
           if ( returnVal == MPI_SUCCESS ) 
           {
              numBytes = HDF_get_Bytes( datatype, count );
           }
           else
           {
              numBytes = -1;
           }
           dataPtr.numBytes = numBytes;
           HDFtraceEvent_RT( -HDFmpiWriteAtAllID,
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
   	   returnVal = PabloMPI_File_sync ( fh );
	} else {
	   dataLen = 1;
           dataPtr.setID = (long)fh;
           HDFtraceEvent_RT( HDFmpiSyncID,
                             &dataPtr,dataLen );
   	   returnVal = MPI_File_sync ( fh );
           HDFtraceEvent_RT( -HDFmpiSyncID,
                             &dataPtr,dataLen );
	} 
   	return returnVal;
}

int HDF_get_Bytes( MPI_Datatype datatype, int count )
{
   int nm_bytes;
   
   MPI_Type_size( datatype, &nm_bytes );
   return( nm_bytes * count );
}

int 
PabloMPI_File_open( MPI_Comm comm,
                    char *filename,
                    int amode,
                    MPI_Info info,
                    MPI_File *fh )
{
   int returnVal;
   
   struct mpiOpenBeginArgs mpiOpenBeginArguments;
   struct mpiOpenEndArgs mpiOpenEndArguments;
   
#ifdef DEBUG
   fprintf( debugFile, "MPI_File_open\n" );
   fflush( debugFile );
#endif /* DEBUG */
   
   MPI_Comm_rank( comm, &HDFlocalNode );
   MPI_Comm_rank( MPI_COMM_WORLD, &HDFmyNode );

   mpiOpenBeginArguments.localNode = HDF_get_source( HDFlocalNode );
   mpiOpenBeginArguments.globalNode = HDFmyNode;
   mpiOpenBeginArguments.communicatorID = HDF_get_comm( comm );
   mpiOpenBeginArguments.mode = amode;
   strcpy( mpiOpenBeginArguments.fileName, filename );
   
   /* Generate entry record */
   HDFtraceIOEvent( mpiOpenBeginID, (char *) &mpiOpenBeginArguments, 
                 sizeof( mpiOpenBeginArguments ) );
   
   returnVal = MPI_File_open( comm, filename, amode, info, fh );
   
   mpiOpenEndArguments.localNode = HDF_get_source( HDFlocalNode );
   mpiOpenEndArguments.globalNode = HDFmyNode;
   /* the fileID is mapped to the fp's address */
   myHDFid++;
   mpiOpenEndArguments.fileID = myHDFid;
   
   /* Generate exit record */
   HDFtraceIOEvent( mpiOpenEndID, (char *) &mpiOpenEndArguments, 
                 sizeof( mpiOpenEndArguments ) );
   
   return returnVal;
}

int 
PabloMPI_File_close( MPI_File *fh )
{
   int returnVal;
   
   struct mpiCloseBeginArgs mpiCloseBeginArguments;
   struct mpiCloseEndArgs mpiCloseEndArguments;
   
#ifdef DEBUG
   fprintf( debugFile, "MPI_File_close\n" );
   fflush( debugFile );
#endif /* DEBUG */
   
   mpiCloseBeginArguments.localNode = HDFlocalNode;
   mpiCloseBeginArguments.globalNode = HDFmyNode;
   mpiCloseBeginArguments.fileID = myHDFid;
   
   /* Generate entry record */
   HDFtraceIOEvent( mpiCloseBeginID, (char *) &mpiCloseBeginArguments, 
		 sizeof( mpiCloseBeginArguments ) );
   
   returnVal = MPI_File_close( fh );
   
   mpiCloseEndArguments.localNode = HDFlocalNode;
   mpiCloseEndArguments.globalNode = HDFmyNode;
   mpiCloseEndArguments.fileID = myHDFid;
   
   /* Generate exit record */
   HDFtraceIOEvent( mpiCloseEndID, (char *) &mpiCloseEndArguments, 
	      sizeof( mpiCloseEndArguments ) );
   
   return returnVal;
}

int 
PabloMPI_File_set_size( MPI_File fh, MPI_Offset size )
{
   int returnVal;
   
   struct mpiSetSizeBeginArgs mpiSetSizeBeginArguments;
   struct mpiSetSizeEndArgs mpiSetSizeEndArguments;
   
#ifdef DEBUG
   fprintf( debugFile, "MPI_File_set_size\n" );
   fflush( debugFile );
#endif /* DEBUG */
   
   mpiSetSizeBeginArguments.localNode = HDFlocalNode;
   mpiSetSizeBeginArguments.globalNode = HDFmyNode;
   /* mpiSetSizeBeginArguments.fileID = (long) (&fh); */
   mpiSetSizeBeginArguments.fileID = myHDFid;
   
   mpiSetSizeBeginArguments.fileSize = (long) size;
   
   /* Generate entry record */
   HDFtraceIOEvent( mpiSetSizeBeginID, (char *) &mpiSetSizeBeginArguments, 
		 sizeof( mpiSetSizeBeginArguments ) );
   
   returnVal = MPI_File_set_size( fh, size );
   
   mpiSetSizeEndArguments.localNode = HDFlocalNode;
   mpiSetSizeEndArguments.globalNode = HDFmyNode;
   /* mpiSetSizeEndArguments.fileID = (long) ( &fh ); */
   mpiSetSizeEndArguments.fileID = myHDFid;
      
   /* Generate entry record */
   HDFtraceIOEvent( mpiSetSizeEndID, (char *) &mpiSetSizeEndArguments, 
		 sizeof( mpiSetSizeEndArguments ) );
   
   return returnVal; 
}

int 
PabloMPI_File_get_size( MPI_File fh, MPI_Offset *size )
{
   int returnVal;
  
   struct mpiGetSizeBeginArgs mpiGetSizeBeginArguments;
   struct mpiGetSizeEndArgs mpiGetSizeEndArguments;
   
#ifdef DEBUG
   fprintf( debugFile, "MPI_File_get_size\n" );
   fflush( debugFile );
#endif /* DEBUG */
   
   mpiGetSizeBeginArguments.localNode = HDFlocalNode;
   mpiGetSizeBeginArguments.globalNode = HDFmyNode;
   /* mpiGetSizeBeginArguments.fileID = (long) (&fh); */
   mpiGetSizeBeginArguments.fileID = myHDFid;
   
   /* Generate entry record */
   HDFtraceIOEvent( mpiGetSizeBeginID, (char *) &mpiGetSizeBeginArguments, 
		 sizeof( mpiGetSizeBeginArguments ) );
   
   returnVal = MPI_File_get_size( fh, size);
   
   mpiGetSizeEndArguments.localNode = HDFlocalNode;
   mpiGetSizeEndArguments.globalNode = HDFmyNode;
   /* mpiGetSizeEndArguments.fileID = (long) ( &fh ); */
   mpiGetSizeEndArguments.fileID = myHDFid;
   
   mpiGetSizeEndArguments.fileSize = (long) (*size);
   
   /* Generate entry record */
   HDFtraceIOEvent( mpiGetSizeEndID, (char *) &mpiGetSizeEndArguments, 
		 sizeof( mpiGetSizeEndArguments ) );
   
   return returnVal;
}

int 
PabloMPI_File_set_view( MPI_File fh,
                        MPI_Offset disp,
                        MPI_Datatype etype,
                        MPI_Datatype filetype,
                        char *datarep,
                        MPI_Info info )
{
   int returnVal;
   
   struct mpiSetViewBeginArgs mpiSetViewBeginArguments;
   struct mpiSetViewEndArgs mpiSetViewEndArguments;
   
#ifdef DEBUG
   fprintf( debugFile, "MPI_File_set_view\n" );
   fflush( debugFile );
#endif /* DEBUG */
   
   mpiSetViewBeginArguments.localNode = HDFlocalNode;
   mpiSetViewBeginArguments.globalNode = HDFmyNode;
   /* mpiSetViewBeginArguments.fileID = (long) ( &fh ); */
   mpiSetViewBeginArguments.fileID = myHDFid;
   
   mpiSetViewBeginArguments.disp = (long) ( disp );
   mpiSetViewBeginArguments.etype = HDF_get_Datatype( etype );
   mpiSetViewBeginArguments.fileType = HDF_get_Datatype( filetype );
   mpiSetViewBeginArguments.dataRep = HDF_get_DataRep( datarep );
   
   /* Generate entry record */
   HDFtraceIOEvent( mpiSetViewBeginID, (char *) &mpiSetViewBeginArguments, 
		 sizeof( mpiSetViewBeginArguments ) );
   
   returnVal = MPI_File_set_view( fh, disp, etype, filetype, 
                                                    datarep, info );
   
   mpiSetViewEndArguments.localNode = HDFlocalNode;
   mpiSetViewEndArguments.globalNode = HDFmyNode;
   /* mpiSetViewEndArguments.fileID = (long) ( &fh ); */
   mpiSetViewEndArguments.fileID = myHDFid;
   
   /* Generate entry record */
   HDFtraceIOEvent( mpiSetViewEndID, (char *) &mpiSetViewEndArguments, 
		 sizeof( mpiSetViewEndArguments ) );
   
   return returnVal;
}

int 
PabloMPI_File_get_view( MPI_File fh,
                        MPI_Offset *disp,
                        MPI_Datatype *etype,
                        MPI_Datatype *filetype,
                        char *datarep )
{
   int returnVal;
   
   struct mpiGetViewBeginArgs mpiGetViewBeginArguments;
   struct mpiGetViewEndArgs mpiGetViewEndArguments;

#ifdef DEBUG
   fprintf( debugFile, "MPI_File_get_view\n" );
   fflush( debugFile );
#endif /* DEBUG */
   
   mpiGetViewBeginArguments.localNode = HDFlocalNode;
   mpiGetViewBeginArguments.globalNode = HDFmyNode;
   /* mpiGetViewBeginArguments.fileID = (long) ( &fh ); */
   mpiGetViewBeginArguments.fileID = myHDFid;
     
   /* Generate entry record */
   HDFtraceIOEvent( mpiGetViewBeginID, (char *) &mpiGetViewBeginArguments, 
		 sizeof( mpiGetViewBeginArguments ) );
   
   returnVal = MPI_File_get_view( fh, disp, etype, filetype, datarep );
   
   mpiGetViewEndArguments.localNode = HDFlocalNode;
   mpiGetViewEndArguments.globalNode = HDFmyNode;
   /* mpiGetViewEndArguments.fileID = (long) ( &fh ); */
   mpiGetViewEndArguments.fileID = myHDFid;
   
   mpiGetViewEndArguments.disp = (long) ( *disp );
   mpiGetViewEndArguments.etype = HDF_get_Datatype( *etype );
   mpiGetViewEndArguments.fileType = HDF_get_Datatype( *filetype );
   mpiGetViewEndArguments.dataRep = HDF_get_DataRep( datarep );
   
   /* Generate entry record */
   HDFtraceIOEvent( mpiGetViewEndID, (char *) &mpiGetViewEndArguments, 
		 sizeof( mpiGetViewEndArguments ) );
   
   return returnVal;  
}

int 
PabloMPI_File_read_at( MPI_File fh,
                       MPI_Offset offset,
                       void *buf,
                       int count,
                       MPI_Datatype datatype,
                       MPI_Status *status )
{
   int returnVal, bcount;
   
   struct mpiReadAtBeginArgs mpiReadAtBeginArguments;
   struct mpiReadAtEndArgs mpiReadAtEndArguments;
   
#ifdef DEBUG
   fprintf( debugFile, "MPI_File_read_at\n" );
   fflush( debugFile );
#endif /* DEBUG */
   
   mpiReadAtBeginArguments.localNode = HDFlocalNode;
   mpiReadAtBeginArguments.globalNode = HDFmyNode;
   /* mpiReadAtBeginArguments.fileID = (long) ( &fh ); */
   mpiReadAtBeginArguments.fileID = myHDFid;
   
   mpiReadAtBeginArguments.offset = (long) ( offset );
   mpiReadAtBeginArguments.count = count;
   mpiReadAtBeginArguments.dataType = HDF_get_Datatype( datatype );
   mpiReadAtBeginArguments.numBytes = HDF_get_Bytes( datatype, count );
    
  
   /* Generate entry record */
   HDFtraceIOEvent( mpiReadAtBeginID, (char *) &mpiReadAtBeginArguments, 
		 sizeof( mpiReadAtBeginArguments ) );
   
   returnVal = MPI_File_read_at( fh, offset, buf, count, datatype, status );
   
   mpiReadAtEndArguments.localNode = HDFlocalNode;
   mpiReadAtEndArguments.globalNode = HDFmyNode;
   /* mpiReadAtEndArguments.fileID = (long) ( &fh ); */
   mpiReadAtEndArguments.fileID = myHDFid;
   
   MPI_Get_count( status, datatype, &bcount );
   if ( bcount < 0 || bcount > count ) 
   {
      mpiReadAtEndArguments.rCount = -1;
      mpiReadAtEndArguments.numBytes = -1;
   }   
   else
   {
      mpiReadAtEndArguments.rCount = bcount;
      mpiReadAtEndArguments.numBytes = HDF_get_Bytes( datatype, bcount );
   }   
   /* Generate entry record */
   HDFtraceIOEvent( mpiReadAtEndID, (char *) &mpiReadAtEndArguments, 
		 sizeof( mpiReadAtEndArguments ) );
   
   return returnVal;
}

int 
PabloMPI_File_read_at_all( MPI_File fh,
                           MPI_Offset offset,
                           void *buf,
                           int count,
                           MPI_Datatype datatype,
                           MPI_Status *status )
{
   int returnVal, bcount;
   
   struct mpiReadAtAllBeginArgs mpiReadAtAllBeginArguments;
   struct mpiReadAtAllEndArgs mpiReadAtAllEndArguments;
   
#ifdef DEBUG
   fprintf( debugFile, "MPI_File_read_at_all\n" );
   fflush( debugFile );
#endif /* DEBUG */
   
   mpiReadAtAllBeginArguments.localNode = HDFlocalNode;
   mpiReadAtAllBeginArguments.globalNode = HDFmyNode;
   /* mpiReadAtAllBeginArguments.fileID = (long) ( &fh ); */
   mpiReadAtAllBeginArguments.fileID = myHDFid;
   
   mpiReadAtAllBeginArguments.offset = (long) ( offset );
   mpiReadAtAllBeginArguments.count = count;
   mpiReadAtAllBeginArguments.dataType = HDF_get_Datatype( datatype );
   mpiReadAtAllBeginArguments.numBytes = HDF_get_Bytes( datatype, count );
   
   /* Generate entry record */
   HDFtraceIOEvent( mpiReadAtAllBeginID, (char *) &mpiReadAtAllBeginArguments, 
		 sizeof( mpiReadAtAllBeginArguments ) );
   
   returnVal = MPI_File_read_at_all( fh, offset, buf, 
				      count, datatype, status );
   
   mpiReadAtAllEndArguments.localNode = HDFlocalNode;
   mpiReadAtAllEndArguments.globalNode = HDFmyNode;
   /* mpiReadAtAllEndArguments.fileID = (long) ( &fh ); */
   mpiReadAtAllEndArguments.fileID = myHDFid;
   
   MPI_Get_count( status, datatype, &bcount );
   if ( bcount < 0 || bcount > count )
   {
      mpiReadAtAllEndArguments.rCount = -1;
      mpiReadAtAllEndArguments.numBytes = -1;
   }
   else
   {
      mpiReadAtAllEndArguments.rCount = bcount;
      mpiReadAtAllEndArguments.numBytes = HDF_get_Bytes( datatype, bcount );
   }
   
   /* Generate entry record */
   HDFtraceIOEvent( mpiReadAtAllEndID, (char *) &mpiReadAtAllEndArguments, 
		 sizeof( mpiReadAtAllEndArguments ) );
   
   return returnVal;
}

int 
PabloMPI_File_write_at( MPI_File fh,
                        MPI_Offset offset,
                        void *buf,
                        int count,
                        MPI_Datatype datatype,
                        MPI_Status *status )
{
   int returnVal, bcount;
   
   struct mpiWriteAtBeginArgs mpiWriteAtBeginArguments;
   struct mpiWriteAtEndArgs mpiWriteAtEndArguments;
   
#ifdef DEBUG
   fprintf( debugFile, "MPI_File_write_at\n" );
   fflush( debugFile );
#endif /* DEBUG */
   
   mpiWriteAtBeginArguments.localNode = HDFlocalNode;
   mpiWriteAtBeginArguments.globalNode = HDFmyNode;
   /* mpiWriteAtBeginArguments.fileID = (long) ( &fh ); */
   mpiWriteAtBeginArguments.fileID = myHDFid;
   
   mpiWriteAtBeginArguments.offset = (long) ( offset );
   mpiWriteAtBeginArguments.count = count;
   mpiWriteAtBeginArguments.dataType = HDF_get_Datatype( datatype );
   mpiWriteAtBeginArguments.numBytes = HDF_get_Bytes( datatype, count );
   
   /* Generate entry record */
   HDFtraceIOEvent( mpiWriteAtBeginID, (char *) &mpiWriteAtBeginArguments, 
		 sizeof( mpiWriteAtBeginArguments ) );
  
   returnVal = MPI_File_write_at( fh, offset, buf, count, 
                                            datatype, status );
   
   mpiWriteAtEndArguments.localNode = HDFlocalNode;
   mpiWriteAtEndArguments.globalNode = HDFmyNode;
   /* mpiWriteAtEndArguments.fileID = (long) ( &fh ); */
   mpiWriteAtEndArguments.fileID = myHDFid;
   
   MPI_Get_count( status, datatype, &bcount );
   mpiWriteAtEndArguments.wCount = bcount;
   mpiWriteAtEndArguments.numBytes = HDF_get_Bytes( datatype, bcount );
   
   /* Generate entry record */
   HDFtraceIOEvent( mpiWriteAtEndID, (char *) &mpiWriteAtEndArguments, 
		 sizeof( mpiWriteAtEndArguments ) );
   
   return returnVal;  
}

int 
PabloMPI_File_write_at_all( MPI_File fh,
                            MPI_Offset offset,
                            void *buf,
                            int count,
                            MPI_Datatype datatype,
                            MPI_Status *status )
{
   int returnVal, bcount;
   int numBytes;
   
   struct mpiWriteAtAllBeginArgs mpiWriteAtAllBeginArguments;
   struct mpiWriteAtAllEndArgs mpiWriteAtAllEndArguments;
   
#ifdef DEBUG
   fprintf( debugFile, "MPI_File_write_at\n" );
   fflush( debugFile );
#endif /* DEBUG */
   
   mpiWriteAtAllBeginArguments.localNode = HDFlocalNode;
   mpiWriteAtAllBeginArguments.globalNode = HDFmyNode;
   /* mpiWriteAtAllBeginArguments.fileID = (long) ( &fh ); */
   mpiWriteAtAllBeginArguments.fileID = myHDFid;
   
   mpiWriteAtAllBeginArguments.offset = (long) ( offset );
   mpiWriteAtAllBeginArguments.count = count;
   mpiWriteAtAllBeginArguments.dataType = HDF_get_Datatype( datatype );
   mpiWriteAtAllBeginArguments.numBytes = HDF_get_Bytes( datatype, count );
      
   /* Generate entry record */
   HDFtraceIOEvent( mpiWriteAtAllBeginID, (char *) &mpiWriteAtAllBeginArguments, 
		 sizeof( mpiWriteAtAllBeginArguments ) );
   
   returnVal = MPI_File_write_at_all( fh, offset, buf, 
				       count, datatype, status );
   
   mpiWriteAtAllEndArguments.localNode = HDFlocalNode;
   mpiWriteAtAllEndArguments.globalNode = HDFmyNode;
   /* mpiWriteAtAllEndArguments.fileID = (long) ( &fh ); */
   mpiWriteAtAllEndArguments.fileID = myHDFid;
   
   if ( returnVal == MPI_SUCCESS ) 
   {
      bcount = count;
      numBytes = HDF_get_Bytes( datatype, count );
   }
   else
   {
      bcount = -1;
      numBytes = -1;
   }
   mpiWriteAtAllEndArguments.wCount = bcount;
   mpiWriteAtAllEndArguments.numBytes = numBytes;

   /* Generate entry record */
   HDFtraceIOEvent( mpiWriteAtAllEndID, (char *) &mpiWriteAtAllEndArguments, 
		 sizeof( mpiWriteAtAllEndArguments ) );
   
   return returnVal;  
}

int 
PabloMPI_File_sync( MPI_File fh ) 
{
   int returnVal;
   
   struct mpiSyncBeginArgs mpiSyncBeginArguments;
   struct mpiSyncEndArgs mpiSyncEndArguments;
   
#ifdef DEBUG
   fprintf( debugFile, "MPI_File_sync\n" );
   fflush( debugFile );
#endif /* DEBUG */
   
   mpiSyncBeginArguments.localNode = HDFlocalNode;
   mpiSyncBeginArguments.globalNode = HDFmyNode;
   /* mpiSyncBeginArguments.fileID = (long) ( &fh ); */
   mpiSyncBeginArguments.fileID = myHDFid;
      
   /* Generate entry record */
   HDFtraceIOEvent( mpiSyncBeginID, 
		 (char *) &mpiSyncBeginArguments, 
		 sizeof( mpiSyncBeginArguments ) );
   
   returnVal = MPI_File_sync ( fh );
   
   mpiSyncEndArguments.localNode = HDFlocalNode;
   mpiSyncEndArguments.globalNode = HDFmyNode;
   /* mpiSyncEndArguments.fileID = (long) ( &fh ); */
   mpiSyncEndArguments.fileID = myHDFid;
   
   /* Generate entry record */
   HDFtraceIOEvent( mpiSyncEndID, (char *) &mpiSyncEndArguments, 
		 sizeof( mpiSyncEndArguments ) );
   
   return returnVal;
}

#endif /* H5_HAVE_PARALLEL */
