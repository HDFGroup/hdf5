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
 * This file contains macros & private information for general HDF5 functions
 */

#ifndef H5PRIVATE_H
#define H5PRIVATE_H

#include "H5proto.h"   /* Include Public Definitions */

/* Private functions, not part of the publicly documented API */
herr_t H5_init_library(void);
void H5_term_library(void);
herr_t H5_init_thread(void);

#endif /* H5PRIVATE_H */

