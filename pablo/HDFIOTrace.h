/*
 * This file is part of the Pablo Performance Analysis Environment
 *
 *          (R)
 * The Pablo    Performance Analysis Environment software is NOT in
 * the public domain.  However, it is freely available without fee for
 * education, research, and non-profit purposes.  By obtaining copies
 * of this and other files that comprise the Pablo Performance Analysis
 * Environment, you, the Licensee, agree to abide by the following
 * conditions and understandings with respect to the copyrighted software:
 * 
 * 1.  The software is copyrighted in the name of the Board of Trustees
 *     of the University of Illinois (UI), and ownership of the software
 *     remains with the UI. 
 *
 * 2.  Permission to use, copy, and modify this software and its documentation
 *     for education, research, and non-profit purposes is hereby granted
 *     to Licensee, provided that the copyright notice, the original author's
 *     names and unit identification, and this permission notice appear on
 *     all such copies, and that no charge be made for such copies.  Any
 *     entity desiring permission to incorporate this software into commercial
 *     products should contact:
 *
 *          Professor Daniel A. Reed                 reed@cs.uiuc.edu
 *          University of Illinois
 *          Department of Computer Science
 *          2413 Digital Computer Laboratory
 *          1304 West Springfield Avenue
 *          Urbana, Illinois  61801
 *          USA
 *
 * 3.  Licensee may not use the name, logo, or any other symbol of the UI
 *     nor the names of any of its employees nor any adaptation thereof in
 *     advertizing or publicity pertaining to the software without specific
 *     prior written approval of the UI.
 *
 * 4.  THE UI MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE
 *     SOFTWARE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS
 *     OR IMPLIED WARRANTY.
 *
 * 5.  The UI shall not be liable for any damages suffered by Licensee from
 *     the use of this software.
 *
 * 6.  The software was developed under agreements between the UI and the
 *     Federal Government which entitle the Government to certain rights.
 *
 **************************************************************************
 *
 * Developed by: The Pablo Research Group
 *               University of Illinois at Urbana-Champaign
 *               Department of Computer Science
 *               1304 W. Springfield Avenue
 *               Urbana, IL     61801
 *
 *               http://www-pablo.cs.uiuc.edu
 *
 * Send comments to: pablo-feedback@guitar.cs.uiuc.edu
 *
 * Copyright (c) 1991-1996
 * The University of Illinois Board of Trustees.
 *      All Rights Reserved.
 *
 * PABLO is a registered trademark of
 * The Board of Trustees of the University of Illinois
 * registered in the U.S. Patent and Trademark Office.
 *
 * Author:  Ruth A. Aydt (aydt@cs.uiuc.edu)
 * Author:  Tara M. Madhyastha (tara@cs.uiuc.edu)
 *
 * Project Manager and Principal Investigator:
 *      Daniel A. Reed (reed@cs.uiuc.edu)
 *
 * Funded in part by National Science Foundation grants NSF CCR87-06653
 * and NSF CDA87-22836 (Tapestry), DARPA contracts DABT63-91-K-0004,
 * DABT63-93-C-0040, DABT63-94-C-0049 (SIO), and F30602-96-C-0161, NASA
 * contracts NAG-1-613 (ICLASS), USRA 5555-22, and NGT-51023, and a
 * collaborative research agreement with the Intel Supercomputer
 * Systems Division
 */

/*
 * HDFIOTrace.h:  This header file can be included in c source files to
 *	       automatically redefine the I/O function calls to the
 *	       tracing versions when "IOTRACE" is defined.   
 *
 *	       It also contains function declarations for the I/O trace
 *	       routines called from user code and constant values that may
 *	       be needed by the user.
 *
 */
#ifndef HDFIOTrace_h
#define HDFIOTrace_h

 
/************************************************************************/
/* These defines and the external variable OUTPUT_SWITCH are used in    */
/* for HDF and MPI-IO tracing to govern the type of output produced.    */
/************************************************************************/
extern int OUTPUT_SWITCH;        /* default is SDDF records      */
 
#if defined(__STDC__) || defined(__cplusplus)
#define PROTO(x) x
#else
#define PROTO(x) ()
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* #include <stdio.h> */
#include <sys/types.h>

void startHDFtraceEvent (int );
void endHDFtraceEvent (int , int , char *, int );

FILE *HDFtraceFOPEN( const char *filename, const char *type ) ;
#ifndef HDFtrace3OPEN__
	int HDFtrace3OPEN( const char *path, int flags, ... ) ;
#endif
int HDFtraceCREAT( const char *path, mode_t mode );
int HDFtraceFFLUSH( FILE *stream ) ;
int HDFtraceFCLOSE( FILE *stream ) ;
int HDFtraceCLOSE( int fd ) ;
ssize_t HDFtraceREAD( int fd, void *buf, size_t nbyte );
size_t HDFtraceFREAD( void *ptr, size_t size, size_t nitems, FILE *stream );
void *HDFtraceMALLOC( size_t );
off_t HDFtraceLSEEK( int fd, off_t offset, int whence ) ;
int HDFtraceFSEEK( FILE *stream, long offset, int whence ) ;
int HDFtraceFSETPOS( FILE *stream, const fpos_t *position ) ;
void HDFtraceREWIND( FILE *stream ) ;
ssize_t HDFtraceWRITE( int fd, const void *buf, size_t nbytes );
size_t HDFtraceFWRITE( const void *, size_t , size_t , FILE * );
int HDFtracePUTS( char *s ) ;
int HDFtraceFPUTC( int c, FILE *stream ) ;
int HDFtraceFPUTS( const char *s, FILE *stream ) ;

#ifdef HDFIOTRACE
/*
 * If IOTRACE is defined, then redefine standard I/O routines to tracing
 * versions.  Also include the appropriate .h files so the function
 * declarations from them will be redefined to traced versions.
 */

#ifdef fopen 
#   undef fopen
#endif
#ifdef open 
#   undef open
#endif
#ifdef creat 
#   undef creat
#endif
 
#ifdef fflush 
#   undef fflush
#endif
#ifdef fclose 
#   undef fclose
#endif
#ifdef close 
#   undef close
#endif
 
#ifdef read 
#   undef read
#endif
#ifdef fread 
#   undef fread
#endif
#ifdef fgetc 
#   undef fgetc
#endif
#ifdef fgets 
#   undef fgets
#endif
#ifdef gets 
#   undef gets
#endif
#ifdef getw 
#   undef getw
#endif
 
#ifdef lseek 
#   undef lseek
#endif
#ifdef fseek 
#   undef fseek
#endif
#ifdef rewind 
#   undef rewind
#endif
#ifdef fsetpos 
#   undef fsetpos
#endif
 
#ifdef write 
#   undef write
#endif
#ifdef fwrite 
#   undef fwrite
#endif
#ifdef fputc 
#   undef fputc
#endif
#ifdef fputs 
#   undef fputs
#endif
#ifdef puts 
#   undef puts
#endif
#ifdef putw 
#   undef putw
#endif
 
#ifdef malloc 
#   undef malloc
#endif

#define fopen		(FILE *)HDFtraceFOPEN
#define open		HDFtrace3OPEN
#define creat		HDFtraceCREAT

#define fflush		HDFtraceFFLUSH
#define fclose		HDFtraceFCLOSE
#define close		HDFtraceCLOSE

#define read		HDFtraceREAD
#define fread		HDFtraceFREAD
#define fgetc		HDFtraceFGETC
#define fgets		HDFtraceFGETS
#define gets		HDFtraceGETS
#define getw		HDFtraceGETW

#define lseek		HDFtraceLSEEK
#define fseek		HDFtraceFSEEK
#define rewind		HDFtraceREWIND
#define fsetpos		HDFtraceFSETPOS

#define write		HDFtraceWRITE
#define fwrite		HDFtraceFWRITE
#define fputc		HDFtraceFPUTC
#define fputs		HDFtraceFPUTS
#define puts		HDFtracePUTS
#define putw		HDFtracePUTW

#define malloc		HDFtraceMALLOC

#include <stdio.h>
#include <fcntl.h>

/*
 * On the iPSC/860 we don't include unistd.h or we get warnings about
 * SEEK_* multiply defined.
 */
#ifndef __NX
#include <unistd.h>
#endif


#include <stdio.h>
/*
 * Function declarations for routines that can be called from user code.
 */
void  enableIOtracing( void );
void  disableIOtracing( void );

void  enableIOdetail( void );
void  disableIOdetail( void );

void  enableLifetimeSummaries( void );
void  disableLifetimeSummaries( void );

void  enableTimeWindowSummaries ( double );
void  disableTimeWindowSummaries( void );
void  setTimeWindowSize ( double );
void  outputTimeWindowSummaries( void );

void  enableFileRegionSummaries ( int );
void  disableFileRegionSummaries( void );
void  setFileRegionSize ( int );
void  outputFileRegionSummaries( void );

int   HDFtrace2OPEN ( char*, int );
void  HDFtraceReadBegin ( int, int, int );
void  HDFtraceReadEnd ( int );
void  HDFtraceWriteBegin ( int, int, int );
void  HDFtraceWriteEnd ( int );
void  HDFtraceIOBegin ( int, int );
void  HDFtraceIOEnd ( int, double, char * );

#ifdef H5_HAVE_PARALLEL

#include "HDFmpioProtos.h"

#ifdef MPI_File_open 		
#undef MPI_File_open 		
#endif
#ifdef MPI_File_close 		
#undef MPI_File_close 		
#endif
#ifdef MPI_File_delete 	
#undef MPI_File_delete 	
#endif
#ifdef MPI_File_set_size 	
#undef MPI_File_set_size 	
#endif
#ifdef MPI_File_preallocate 	
#undef MPI_File_preallocate 	
#endif
#ifdef MPI_File_get_size 	
#undef MPI_File_get_size 	
#endif
#ifdef MPI_File_get_group 	
#undef MPI_File_get_group 	
#endif
#ifdef MPI_File_get_amode 	
#undef MPI_File_get_amode 	
#endif
#ifdef MPI_File_set_view 	
#undef MPI_File_set_view 	
#endif
#ifdef MPI_File_get_view 	
#undef MPI_File_get_view 	
#endif
#ifdef MPI_File_read_at 	
#undef MPI_File_read_at 	
#endif
#ifdef MPI_File_read_at_all 	
#undef MPI_File_read_at_all 	
#endif
#ifdef MPI_File_write_at 	
#undef MPI_File_write_at 	
#endif
#ifdef MPI_File_write_at_all 	
#undef MPI_File_write_at_all 	
#endif
#ifdef MPI_File_iread_at 	
#undef MPI_File_iread_at 	
#endif
#ifdef MPI_File_iwrite_at 	
#undef MPI_File_iwrite_at 	
#endif
#ifdef MPI_File_read 		
#undef MPI_File_read 		
#endif
#ifdef MPI_File_read_all 	
#undef MPI_File_read_all 	
#endif
#ifdef MPI_File_write 		
#undef MPI_File_write 		
#endif
#ifdef MPI_File_write_all 	
#undef MPI_File_write_all 	
#endif
#ifdef MPI_File_iread 		
#undef MPI_File_iread 		
#endif
#ifdef MPI_File_iwrite 	
#undef MPI_File_iwrite 	
#endif
#ifdef MPI_File_seek 		
#undef MPI_File_seek 		
#endif
#ifdef MPI_File_get_position 	
#undef MPI_File_get_position 	
#endif
#ifdef MPI_File_get_byte_offset
#undef MPI_File_get_byte_offset
#endif
#ifdef MPI_File_get_type_extent
#undef MPI_File_get_type_extent
#endif
#ifdef MPI_File_set_atomicity 	
#undef MPI_File_set_atomicity 	
#endif
#ifdef MPI_File_get_atomicity 	
#undef MPI_File_get_atomicity 	
#endif
#ifdef MPI_File_sync 		
#undef MPI_File_sync 		
#endif

#define MPI_File_open 		 HDF_MPI_File_open
#define MPI_File_close 		 HDF_MPI_File_close
#define MPI_File_delete 	 HDF_MPI_File_delete
#define MPI_File_set_size 	 HDF_MPI_File_set_size
#define MPI_File_preallocate 	 HDF_MPI_File_preallocate
#define MPI_File_get_size 	 HDF_MPI_File_get_size
#define MPI_File_get_group 	 HDF_MPI_File_get_group
#define MPI_File_get_amode 	 HDF_MPI_File_get_amode
#define MPI_File_set_view 	 HDF_MPI_File_set_view
#define MPI_File_get_view 	 HDF_MPI_File_get_view
#define MPI_File_read_at 	 HDF_MPI_File_read_at
#define MPI_File_read_at_all 	 HDF_MPI_File_read_at_all
#define MPI_File_write_at 	 HDF_MPI_File_write_at
#define MPI_File_write_at_all 	 HDF_MPI_File_write_at_all
#define MPI_File_iread_at 	 HDF_MPI_File_iread_at
#define MPI_File_iwrite_at 	 HDF_MPI_File_iwrite_at
#define MPI_File_read 		 HDF_MPI_File_read
#define MPI_File_read_all 	 HDF_MPI_File_read_all
#define MPI_File_write 		 HDF_MPI_File_write
#define MPI_File_write_all 	 HDF_MPI_File_write_all
#define MPI_File_iread 		 HDF_MPI_File_iread
#define MPI_File_iwrite 	 HDF_MPI_File_iwrite
#define MPI_File_seek 		 HDF_MPI_File_seek
#define MPI_File_get_position 	 HDF_MPI_File_get_position
#define MPI_File_get_byte_offset HDF_MPI_File_get_byte_offset
#define MPI_File_get_type_extent HDF_MPI_File_get_type_extent
#define MPI_File_set_atomicity 	 HDF_MPI_File_set_atomicity
#define MPI_File_get_atomicity 	 HDF_MPI_File_get_atomicity
#define MPI_File_sync 		 HDF_MPI_File_sync
#endif /* H5_HAVE_PARALLEL */
#endif /* HDFIOTRACE */
#ifdef __cplusplus
}
#endif

#endif		/* HDFIOTRACE conditional */
