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
// File:  PabloHDF_MPIO.c						*
// Purpose: support use of Pablo trace library to analyze MPIO calls	*
// within HDF calls 							*
// Most of the code is conditionally compiled dependent on the compiler *
// flag HAVE_H5_PARALLEL which is set in the Makefile in thie hdf/pablo *
// directory.								*
// Contents
//    HDF_get_NodeNum                					*
// 
//    HDF_get_mode         				*
//   returns the node number						*
// 
//    HDF_get_source        					*
// 
//    HDF_get_comm
// 
//    HDF_get_Datatype
// 
//    HDF_get_DataRep
// 
// int HDF_get_Bytes
// 
// int PabloMPI_File_open
//                       
// int PabloMPI_File_close
// 
// int PabloMPI_File_set_size
// 
// int PabloMPI_File_get_size
// 
// int PabloMPI_File_set_view
// 
// int PabloMPI_File_get_view
// 
// int PabloMPI_File_read_at
// 
// int PabloMPI_File_read_at_all
// 
// int PabloMPI_File_write_at
// 
// int PabloMPI_File_write_at_all
// 
// int PabloMPI_File_sync
// 
// int HDF_MPI_File_open
// 
// int HDF_MPI_File_close
// 
// int HDF_MPI_File_set_size
// 
// int HDF_MPI_File_get_size
// 
// int HDF_MPI_File_set_view
// 
// int HDF_MPI_File_get_view
// 
// int HDF_MPI_File_read_at
// 
// int HDF_MPI_File_read_at_all
// 
// int HDF_MPI_File_write_at
//
// int HDF_MPI_File_write_at_all
// 
// int HDF_MPI_File_sync
// 
//======================================================================*/
#ifdef H5_HAVE_PARALLEL
#include "mpi.h"
/************************************************************************/
/* Return the node number.						*/
/************************************************************************/
void HDF_get_NodeNum( int* nodeNum )
{
   MPI_Comm_rank( MPI_COMM_WORLD, nodeNum );
}
#ifdef _BUILD
#include "HDFTrace.h"
#include "ProcTrace.h"
#include "ProcIDs.h"
#include "MPIO_Trace.h"
#include "MPIO_EventArgs.h"
#include "MPIO_Data.h"

extern int OUTPUT_SWITCH;
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
int 
HDF_get_mode( int amode )
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
int 
HDF_get_source( int source )
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
int 
HDF_get_comm( MPI_Comm in_comm )
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

int PabloMPI_File_sync( MPI_File fh ) ; 

/*======================================================================* 
// Pass call through to regular MPIO entry except in case of Real Time	* 
// tracing.  								* 
// Note: The regular MPIO entry may or may not be instrumented.		*
//======================================================================*/
int 
HDF_MPI_File_open( MPI_Comm comm, 
                   char *filename, 
                   int amode, 
                   MPI_Info info, 
                   MPI_File *fh )
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
int 
HDF_MPI_File_close( MPI_File *fh )
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
int 
HDF_MPI_File_set_size( MPI_File fh, MPI_Offset size )
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
int 
HDF_MPI_File_get_size( MPI_File fh, MPI_Offset *size )
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
int 
HDF_MPI_File_set_view( MPI_File fh, 
                       MPI_Offset disp, 
                       MPI_Datatype etype, 
                       MPI_Datatype filetype, 
                       char *datarep, 
                       MPI_Info info )
{
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) 
        {
   	   returnVal = PabloMPI_File_set_view( fh, disp, etype, filetype, 
                                                      datarep, info );
	} 
        else 
        {
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
int 
HDF_MPI_File_get_view( MPI_File fh, 
                       MPI_Offset *disp, 
                       MPI_Datatype *etype, 
                       MPI_Datatype *filetype, 
                       char *datarep )
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
      HDFtraceEvent_RT( HDFmpiSetViewID, &dataPtr,dataLen );
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
int HDF_MPI_File_read_at( MPI_File fh, 
                          MPI_Offset offset, 
                          void *buf,
		          int count, 
                          MPI_Datatype datatype, 
                          MPI_Status *status )
{
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;
        int rCount;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) 
        {
   	   returnVal = PabloMPI_File_read_at( fh, 
                                              offset, 
                                              buf, 
                                              count, 
                                              datatype, 
                                              status );
	} 
        else 
        {
	   dataLen = sizeof(dataPtr);
           dataPtr.setID = (long)fh;
           dataPtr.numBytes = HDF_get_Bytes( datatype, count );
           HDFtraceEvent_RT( HDFmpiReadAtID,
                             &dataPtr,dataLen );
   	   returnVal = MPI_File_read_at( fh, 
                                         offset, 
                                         buf, 
                                         count, 
                                         datatype, 
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
int 
HDF_MPI_File_read_at_all( MPI_File fh, 
                          MPI_Offset offset, 
                          void *buf,
                          int count, 
                          MPI_Datatype datatype, 
                          MPI_Status *status )
{
   
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;
        int rCount;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) 
        {
   	   returnVal = PabloMPI_File_read_at_all( fh, 
                                                  offset, 
                                                  buf, 
                                                  count, 
                                                  datatype, 
                                                  status );
	} 
        else 
        {
	   dataLen = sizeof(dataPtr);
           dataPtr.setID = (long)fh;
           dataPtr.numBytes = HDF_get_Bytes( datatype, count );
           HDFtraceEvent_RT( HDFmpiReadAtAllID,
                             &dataPtr,dataLen );
   	   returnVal = MPI_File_read_at_all( fh, 
                                             offset, 
                                             buf, 
                                             count, 
                                             datatype, 
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
int 
HDF_MPI_File_write_at( MPI_File fh, 
                       MPI_Offset offset, 
                       void *buf,
                       int count, 
                       MPI_Datatype datatype, 
                       MPI_Status *status )
{
   
   int returnVal;
   HDFsetInfo dataPtr;
   int dataLen;
   int rCount;

   if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) 
   {
      returnVal = PabloMPI_File_write_at( fh, 
                                          offset, 
                                          buf, 
                                          count, 
                                          datatype, 
                                          status );
   } 
   else 
   {
      dataLen = sizeof(dataPtr);
      dataPtr.setID = (long)fh;
      dataPtr.numBytes = HDF_get_Bytes( datatype, count );
      HDFtraceEvent_RT( HDFmpiWriteAtID, &dataPtr,dataLen );
      returnVal = MPI_File_write_at( fh, 
                                     offset, 
                                     buf, 
                                     count, 
                                     datatype, 
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
int 
HDF_MPI_File_write_at_all( MPI_File fh, 
                           MPI_Offset offset, 
                           void *buf,
                           int count, 
                           MPI_Datatype datatype, 
                           MPI_Status *status )
{
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;
        int numBytes;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) 
        {
   	   returnVal = PabloMPI_File_write_at_all( fh, 
                                                   offset, 
                                                   buf, 
                                                   count, 
                                                   datatype, 
                                                   status );
	} 
        else 
        {
	   dataLen = sizeof(dataPtr);
           dataPtr.setID = (long)fh;
           dataPtr.numBytes = HDF_get_Bytes( datatype, count );
           HDFtraceEvent_RT( HDFmpiWriteAtAllID, &dataPtr, dataLen );
   	   returnVal = MPI_File_write_at_all( fh, 
                                              offset, 
                                              buf, 
                                              count, 
                                              datatype, 
                                              status );
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
int 
HDF_MPI_File_sync( MPI_File fh )
{
   	int returnVal;
   	HDFsetInfo dataPtr;
   	int dataLen;

        if ( OUTPUT_SWITCH != MPI_SUMMARY_TRACE ) 
        { 
   	   returnVal = PabloMPI_File_sync ( fh );
	} 
        else 
        {
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

int 
HDF_get_Bytes( MPI_Datatype datatype, int count )
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
   fprintf( debugFile, "PabloMPI_File_open\n" );
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
   HDFtraceIOEvent( mpiOpenBeginID, 
                    (char *) &mpiOpenBeginArguments, 
                    sizeof( mpiOpenBeginArguments ) );
   
   returnVal = MPI_File_open( comm, filename, amode, info, fh );
   
   mpiOpenEndArguments.localNode = HDF_get_source( HDFlocalNode );
   mpiOpenEndArguments.globalNode = HDFmyNode;
   /* the fileID is mapped to the fp's address */
   myHDFid++;
   mpiOpenEndArguments.fileID = myHDFid;
   
   /* Generate exit record */
   HDFtraceIOEvent( mpiOpenEndID,    
                    (char *) &mpiOpenEndArguments, 
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
   HDFtraceIOEvent( mpiCloseBeginID, 
                    (char *) &mpiCloseBeginArguments, 
		    sizeof( mpiCloseBeginArguments ) );
   
   returnVal = MPI_File_close( fh );
   
   mpiCloseEndArguments.localNode = HDFlocalNode;
   mpiCloseEndArguments.globalNode = HDFmyNode;
   mpiCloseEndArguments.fileID = myHDFid;
   
   /* Generate exit record */
   HDFtraceIOEvent( mpiCloseEndID, 
                    (char *) &mpiCloseEndArguments, 
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
   HDFtraceIOEvent( mpiSetSizeBeginID, 
                    (char *) &mpiSetSizeBeginArguments, 
		    sizeof( mpiSetSizeBeginArguments ) );
   
   returnVal = MPI_File_set_size( fh, size );
   
   mpiSetSizeEndArguments.localNode = HDFlocalNode;
   mpiSetSizeEndArguments.globalNode = HDFmyNode;
   mpiSetSizeEndArguments.fileID = myHDFid;
      
   /* Generate entry record */
   HDFtraceIOEvent( mpiSetSizeEndID, 
                    (char *) &mpiSetSizeEndArguments, 
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
   HDFtraceIOEvent( mpiGetSizeBeginID, 
                    (char *) &mpiGetSizeBeginArguments, 
		    sizeof( mpiGetSizeBeginArguments ) );
   
   returnVal = MPI_File_get_size( fh, size);
   
   mpiGetSizeEndArguments.localNode = HDFlocalNode;
   mpiGetSizeEndArguments.globalNode = HDFmyNode;
   /* mpiGetSizeEndArguments.fileID = (long) ( &fh ); */
   mpiGetSizeEndArguments.fileID = myHDFid;
   
   mpiGetSizeEndArguments.fileSize = (long) (*size);
   
   /* Generate entry record */
   HDFtraceIOEvent( mpiGetSizeEndID, 
                    (char *) &mpiGetSizeEndArguments, 
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
   HDFtraceIOEvent( mpiSetViewBeginID, 
                    (char *) &mpiSetViewBeginArguments, 
		    sizeof( mpiSetViewBeginArguments ) );
   
   returnVal = MPI_File_set_view( fh, 
                                  disp, 
                                  etype, 
                                  filetype, 
                                  datarep, 
                                  info );
   
   mpiSetViewEndArguments.localNode = HDFlocalNode;
   mpiSetViewEndArguments.globalNode = HDFmyNode;
   /* mpiSetViewEndArguments.fileID = (long) ( &fh ); */
   mpiSetViewEndArguments.fileID = myHDFid;
   
   /* Generate entry record */
   HDFtraceIOEvent( mpiSetViewEndID, 
                    (char *) &mpiSetViewEndArguments, 
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
   mpiGetViewBeginArguments.fileID = myHDFid;
     
   /* Generate entry record */
   HDFtraceIOEvent( mpiGetViewBeginID, 
                    (char *) &mpiGetViewBeginArguments, 
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
   HDFtraceIOEvent( mpiGetViewEndID, 
                    (char *) &mpiGetViewEndArguments, 
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
   HDFtraceIOEvent( mpiReadAtBeginID, 
                    (char *) &mpiReadAtBeginArguments, 
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
   HDFtraceIOEvent( mpiReadAtEndID, 
                    (char *) &mpiReadAtEndArguments, 
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
   HDFtraceIOEvent( mpiReadAtAllBeginID, 
                    (char *) &mpiReadAtAllBeginArguments, 
		    sizeof( mpiReadAtAllBeginArguments ) );
   
   returnVal = MPI_File_read_at_all( fh, 
                                     offset, 
                                     buf, 
                                     count, 
                                     datatype, 
                                     status );
   
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
   HDFtraceIOEvent( mpiReadAtAllEndID, 
                    (char *) &mpiReadAtAllEndArguments, 
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
   HDFtraceIOEvent( mpiWriteAtBeginID, 
                    (char *) &mpiWriteAtBeginArguments, 
		    sizeof( mpiWriteAtBeginArguments ) );
  
   returnVal = MPI_File_write_at( fh, 
                                  offset, 
                                  buf, 
                                  count, 
                                  datatype, 
                                  status );
   
   mpiWriteAtEndArguments.localNode = HDFlocalNode;
   mpiWriteAtEndArguments.globalNode = HDFmyNode;
   /* mpiWriteAtEndArguments.fileID = (long) ( &fh ); */
   mpiWriteAtEndArguments.fileID = myHDFid;
   
   MPI_Get_count( status, datatype, &bcount );
   mpiWriteAtEndArguments.wCount = bcount;
   mpiWriteAtEndArguments.numBytes = HDF_get_Bytes( datatype, bcount );
   
   /* Generate entry record */
   HDFtraceIOEvent( mpiWriteAtEndID, 
                    (char *) &mpiWriteAtEndArguments, 
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
   HDFtraceIOEvent( mpiWriteAtAllBeginID, 
                    (char *) &mpiWriteAtAllBeginArguments, 
		    sizeof( mpiWriteAtAllBeginArguments ) );
   
   returnVal = MPI_File_write_at_all( fh, 
                                      offset, 
                                      buf, 
				      count, 
                                      datatype, 
                                      status );
   
   mpiWriteAtAllEndArguments.localNode = HDFlocalNode;
   mpiWriteAtAllEndArguments.globalNode = HDFmyNode;
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
   HDFtraceIOEvent( mpiWriteAtAllEndID, 
                    (char *) &mpiWriteAtAllEndArguments, 
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
   HDFtraceIOEvent( mpiSyncEndID, 
                    (char *) &mpiSyncEndArguments, 
		    sizeof( mpiSyncEndArguments ) );
   
   return returnVal;
}
#endif /* _BUILD */
#else /* H5_HAVE_PARALLEL */
void HDF_get_NodeNum( int* nodeNum )
{
   *nodeNum = 0;
}
#endif /* H5_HAVE_PARALLEL */
