C *
C * This file is an extension to NCSA HDF to enable the use of the
C * Pablo trace library.
C *
C * Developed by: The TAPESTRY Parallel Computing Laboratory
C *		  University of Illinois at Urbana-Champaign
C *		  Department of Computer Science
C *		  1304 W. Springfield Avenue
C *		  Urbana, IL	61801
C *
C * Copyright (c) 1995
C * The University of Illinois Board of Trustees.
C *      All Rights Reserved.
C *
C * PABLO is a registered trademark of
C * The Board of Trustees of the University of Illinois
C * registered in the U.S. Patent and Trademark Office.
C *
C * Author: Jonathan M. Reid (jreid@cs.uiuc.edu)
C *
C * Project Manager and Principal Investigator:
C *	Daniel A. Reed (reed@cs.uiuc.edu)
C *
C * Funded by: National Aeronautics and Space Administration under NASA
C * Contracts NAG-1-613 and USRA 5555-22 and by the Advanced Research
C * Projects Agency under ARPA contracts DAVT63-91-C-0029 and
C * DABT63-93-C-0040.
C *

C-----------------------------------------------------------------------------
C File:     PabloHDFff.f
C Purpose:  Fortran stubs for Pablo routines
C Invokes:  PabloHDFf.c 
C Contents: 
C   hinitiotrace :   Call ihinitiotrace to initialize Pablo tracing
C Remarks: none
C-----------------------------------------------------------------------------

C-----------------------------------------------------------------------------
C Name: hdfinittrace
C Purpose:  call hdfinittracex to initialize tracing
C Inputs:   tracefn: Trace file name
C           proctmask:
C Returns: 0 on success, FAIL on failure 
C Users:   
C Invokes: hinittracex
C-----------------------------------------------------------------------------

      subroutine hdfinittracef(tracefn,traceids,nids,out_sw)

      character*(*) tracefn
      integer       traceids(*), out_sw, nids
      character filename(1024)
      integer  i, length

      length = len(tracefn)
      do i = 1, length
	 filename(i) = tracefn(i:i) 
      end do
      call  hinittracex(filename,length,traceids,nids,out_sw)
      return
      end
