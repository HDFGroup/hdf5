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
 *  Header file for Pablo compatibility
 */

#ifndef HDF5PABL_H
#define HDF5PABL_H

#ifdef HAVE_PABLO
#define PABLO_SAVE(func_id) int pablo_func_id = func_id
#define PABLO_TRACE_ON(m, f) TRACE_ON(m,f)
#define PABLO_TRACE_OFF(m, f) TRACE_OFF(m,f)
#else /* no Pablo tracing enabled */
#define PABLO_SAVE(func_id) /*void*/
#define PABLO_TRACE_ON(m, f) /*void*/
#define PABLO_TRACE_OFF(m, f) /*void*/
#endif /* HAVE_PABLO */

#endif /* HDF5PABL_H */


