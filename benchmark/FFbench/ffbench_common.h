/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef _FFBENCH_COMMON_H
#define _FFBENCH_COMMON_H

#include <stdlib.h>
#include <sys/time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define FFB_SUCCESS 0
#define FFB_FAIL -1
#define DEBUG

#define READ 290
#define WRITE 390


typedef struct timeval timer;
typedef unsigned long long length_t;


#ifdef DEBUG
#define LOC fprintf (stderr,"in %s:%d ", __FILE__, __LINE__);
#define DEBUG_PRINT(fmt, ...) LOC fprintf (stderr, fmt,__VA_ARGS__);
#define DEBUG_LINE(text) LOC fprintf (stderr, text); 
#else
#define DEBUG_PRINT(fmt,...)
#define DEBUG_LINE(text)
#endif



#endif /*FFBENCH_COMMON*/
