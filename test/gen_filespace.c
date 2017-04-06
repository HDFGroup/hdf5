/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "hdf5.h"
#include <assert.h>

#define NELMTS(X)    	(sizeof(X)/sizeof(X[0]))	/* # of elements */

const char *FILENAMES[] = {
    "fsm_aggr_nopersist.h5",	/* H5F_FSPACE_STRATEGY_FSM_AGGR + not persisting free-space */
    "fsm_aggr_persist.h5",		/* H5F_FSPACE_STRATEGY_FSM_AGGR + persisting free-space */
    "paged_nopersist.h5",	    /* H5F_FSPACE_STRATEGY_PAGE + not persisting free-space */
    "paged_persist.h5",		    /* H5F_FSPACE_STRATEGY_PAGE + persisting free-space */
    "aggr.h5",	                /* H5F_FSPACE_STRATEGY_AGGR */
    "none.h5"		            /* H5F_FSPACE_STRATEGY_NONE */
};

#define DATASET		"dset"
#define NUM_ELMTS	100
#define FALSE		0
#define TRUE		1
#define INC_ENUM(TYPE,VAR) (VAR)=((TYPE)((VAR)+1))

/*
 * Compile and run this program in the trunk to generate
 * HDF5 files with combinations of 4 file space strategies
 * and persist/not persist free-space.
 * The library creates the file space info message with "mark if unknown"
 * in these files.
 *
 * Move these files to 1.8 branch for compatibility testing:
 * test_filespace_compatible() in test/tfile.c will use these files.
 *
 * Copy these files from the 1.8 branch back to the trunk for 
 * compatibility testing via test_filespace_round_compatible() in test/tfile.c.
 *
 */
int main(void)
{
    hid_t fid = -1;         /* File ID */
    hid_t fcpl = -1;        /* File creation property list */
    hid_t did = -1;         /* Dataset ID */
    hid_t sid = -1;		    /* Dataspace ID */
    hsize_t dim[1];			/* Dimension sizes */
    int data[NUM_ELMTS];	/* Buffer for data */
    int i, j;			    /* Local index variables */
    H5F_fspace_strategy_t fs_strategy;	/* File space handling strategy */
    unsigned fs_persist;     /* Persisting free-space or not */

    j = 0;
    for(fs_strategy = H5F_FSPACE_STRATEGY_FSM_AGGR; fs_strategy < H5F_FSPACE_STRATEGY_NTYPES; INC_ENUM(H5F_fspace_strategy_t, fs_strategy)) {
        for(fs_persist = FALSE; fs_persist <= TRUE; fs_persist++) {

            if(fs_persist && fs_strategy >= H5F_FSPACE_STRATEGY_AGGR)
                continue;

            /* Get a copy of the default file creation property */
            if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
                goto error;

            if(H5Pset_file_space_strategy(fcpl, fs_strategy, fs_persist, (hsize_t)1) < 0)
                goto error;

            /* Create the file with the file space info */
            if((fid = H5Fcreate(FILENAMES[j], H5F_ACC_TRUNC, fcpl, H5P_DEFAULT)) < 0)
                goto error;

            /* Create the dataset */
            dim[0] = NUM_ELMTS;
            if((sid = H5Screate_simple(1, dim, NULL)) < 0)
                goto error;
            if((did = H5Dcreate2(fid, DATASET, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
                goto error;

            for(i = 0; i < NUM_ELMTS; i++)
                data[i] = i;

            /* Write the dataset */
            if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
                goto error;

            /* Closing */
            if(H5Dclose(did) < 0)
                goto error;
            if(H5Sclose(sid) < 0)
                goto error;
            if(H5Fclose(fid) < 0)
                goto error;
            if(H5Pclose(fcpl) < 0)
                goto error;
            ++j;
        }
    }
    assert(j == NELMTS(FILENAMES));

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Sclose(sid);
        H5Sclose(did);
        H5Pclose(fcpl);
        H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
}
