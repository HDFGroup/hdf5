/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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

/*  
 * crasher HDF5 API module of the trecover test program.
 */
 
#include "trecover.h"

/* Crasher
 * Two crash modes:
 *    SyncCrash: will now at once;
 *    AsyncCrash: schedule a crash to occur in the future in the asynchronous
 *       fashion.
 * Crash_param: used by AsyncCrash mode and ignored by SyncCrash mode.
 */
void
crasher(int crash_mode, int crash_param)
{
    switch (crash_mode) {
	case SyncCrash:
	    _exit(0);
	    break;
	case AsyncCrash:
	    printf("AsyncCrash not implemented yet\n");
	    break;
	otherwise:
	    print("Unknown Crash Mode (%d)\n", crash_mode);
	    break;
    }
}     
