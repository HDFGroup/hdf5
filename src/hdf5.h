/****************************************************************************
 * NCSA HDF                                                                 *
 * Software Development Group                                               *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/

/* $Id$ */

/*
 * This is the main HDF5 include file.  Put further information in a particular
 * header file and include that here, don't fill this file with lots of gunk...
 */

#ifndef HDF5_H
#define HDF5_H

/* Standard header files needed all the time */
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>

/* PABLO support files */
#ifdef HAVE_PABLO
#define IOTRACE
#include "IOTrace.h"
#include "ProcIDs.h"
#endif  /* HAVE_PABLO */


/* Determine the system and set up basic info. */
#include "hdf5plat.h"   /* Platform/OS/CPU detection header file (should be first header included) */

/* Generic Data-Structure/Algorithm include files */
#include "H5Aproto.h"   /* Atom management routines */

/* Major HDF5 Include files */
#include "hdf5meta.h"   /* File Meta-data conversion macros, etc. */
#include "hdf5fort.h"   /* Fortran macros, etc. */
#include "hdf5port.h"   /* Portability macros for easier development */
#include "hdf5gen.h"    /* General global macros */
#include "hdf5type.h"   /* General global typedefs (uses basic types defined in hdf5plat.h) */
#include "hdf5lims.h"   /* Various global limits & version numbers on things */
#include "H5Fproto.h"   /* File access information and macros */
#include "hdf5pabl.h"   /* Pablo porting macros */
#include "H5Eproto.h"   /* Error reporting information */
#include "H5Cproto.h"   /* Template information */
#include "H5Mproto.h"   /* Meta-Object information */
#include "H5Tproto.h"   /* Datatype information */
#include "H5Pproto.h"   /* Dataspace information */
#include "H5proto.h"    /* Generic Interface information */
#include "hdf5glob.h"   /* Global & thread-specific variables */

#endif /* HDF5_H */

