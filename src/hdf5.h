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

/*
 * This is the main public HDF5 include file.  Put further information in
 * a particular header file and include that here, don't fill this file with
 * lots of gunk...
 */
#ifndef _HDF5_H
#define _HDF5_H

#include <H5public.h>
#include <H5Ipublic.h>      /* IDs (this has to come near the top, to define hid_t) */
#include <H5Apublic.h>      /* Attributes */
#include <H5ACpublic.h>     /* Metadata cache */
#include <H5Bpublic.h>      /* B-trees */
#include <H5Dpublic.h>      /* Datasets */
#include <H5Epublic.h>      /* Errors */
#include <H5Fpublic.h>      /* Files */
#include <H5Gpublic.h>      /* Groups */
#include <H5HGpublic.h>	    /* Global heaps */
#include <H5HLpublic.h>	    /* Local heaps */
#include <H5MFpublic.h>     /* File memory management */
#include <H5MMpublic.h>     /* Core memory management */
#include <H5Opublic.h>      /* Object headers */
#include <H5Ppublic.h>      /* Property lists */
#include <H5Rpublic.h>	    /* References */
#include <H5RApublic.h>	    /* Ragged arrays */
#include <H5Spublic.h>      /* Dataspaces */
#include <H5Tpublic.h>      /* Datatypes */
#include <H5Zpublic.h>	    /* Data filters */

#endif
