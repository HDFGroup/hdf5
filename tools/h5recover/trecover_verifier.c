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
 * This file contains a set of routines intended to verify that the crashed
 * file has been recovered correctly.
 *
 * We say that a crashed file is correctly recovered iff:
 *
 * 1) The file is readable.
 *
 * 2) If the crash file contains a dataset of the same name as a dataset
 *    that appears in the architype, 
 *
 *    a) it is of the same type and configuration,
 *
 *    b) it is the same size or smaller,
 *
 *    c) While metadata journaling does nothing about raw data, we should
 *       look at the raw data and comment if it is either not all garbage
 *       or filler value, or matching data for a while followed by garbage
 *       or filler falue
 *
 * 3) The crash file contains no dataset that are not in the architype 
 *    file.
 *
 * If the crash file is clearly not correctly recoverd, write a message 
 * to this effect to stderr, and return non-zero.
 *
 * If the raw data looks odd, write a message to stdout, and return zero.
 *
 * If all is OK, return 0
 *
 * Note that this code make a lot of assumptions about the structure
 * of the control and crash files.  I expect that this file will have 
 * to be reworked whenever the test file generation code is changed.
 *
 * 					JRM -- 12/8/08
 *
 * WARNING: The above is only partially implemented.  Specifically,
 * at present we only check for the chunked integer dataset type, and 
 * do not check for extraneous dataset.
 */

#include "trecover.h"

#define FALSE 0
#define TRUE  1

int
open_files(const char *filename, const char *ctl_filename)
{
    int ret_val = 0;

    ctl_file = H5Fopen(ctl_filename, H5F_ACC_RDONLY, H5P_DEFAULT);

    if ( ctl_file < 0 ) {

	ret_val = 1;
	fprintf(stderr, "Can't open control file \"%s\".\n", ctl_filename);
    }

    if ( ret_val == 0 ) {

        datafile = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT);

        if ( datafile < 0 ) {

            ret_val = 1;
	    fprintf(stderr, "Can't open crash file \"%s\".\n", ctl_filename);
	    H5Fclose(ctl_file);
	    ctl_file = -1;
        
        }
    }

    return(ret_val);

}

int 
verify_chunked_dset(void)
{
    hbool_t done = FALSE;
    hbool_t dump_ctl_dset = FALSE;
    hbool_t dump_crash_dset = FALSE;
    hbool_t good_data;
    hbool_t odd_streak;
    int streak_len;
    int transitions;
    int ret_val = 0;
    int ctl_rank = -1;
    int crash_rank = -1;
    int ctl_data[NX * 4][NY];
    int crash_data[NX * 4][NY];
    unsigned int i, j;
    herr_t ctl_result;
    herr_t crash_result;
    hid_t ctl_dset = -1;
    hid_t crash_dset = -1;
    hid_t ctl_dspace = -1;
    hid_t crash_dspace = -1;
    hid_t ctl_dtype = -1;
    hid_t crash_dtype = -1;
    hid_t ctl_plist = -1;
    hid_t crash_plist = -1;
    hid_t ctl_memspace = -1;
    hid_t crash_memspace = -1;
    hsize_t zero_offset[RANK] = {0, 0};
    hsize_t ctl_dims[RANK];
    hsize_t crash_dims[RANK];
    hsize_t ctl_maxdims[RANK];
    hsize_t crash_maxdims[RANK];
    hsize_t ctl_chnkdims[RANK];
    hsize_t crash_chnkdims[RANK];
    hsize_t ctl_memspace_dims[RANK];
    hsize_t crash_memspace_dims[RANK];

    /* Check to see if the dataset appears in both the ctl and crash 
     * files.
     */
    if ( ret_val == 0 )
    {
        ctl_dset = H5Dopen2(ctl_file, CHUNKDATASETNAME, H5P_DEFAULT);

	if ( ctl_dset >= 0 ) {

	    crash_dset = H5Dopen2(datafile, CHUNKDATASETNAME, H5P_DEFAULT);

	    if ( crash_dset < 0 ) {

	        /* Dataset doesn't appear in the crash file, so we are
                 * done.
		 */

                done = TRUE;

	    }
	} else {

	    crash_dset = H5Dopen2(datafile, DATASETNAME, H5P_DEFAULT);

	    if ( crash_dset >= 0 ) {

	        ret_val = 1;
		fprintf(stderr, 
		    "dataset %s appears in crash file but not in ctl file.\n",
		    DATASETNAME);
	    }

	}
    }


    /* Get the dataspaces of the data sets */
    if ( ( ret_val == 0 ) && ( ! done ) ) {

	ctl_dspace = H5Dget_space(ctl_dset);
	crash_dspace = H5Dget_space(crash_dset);

	if ( ( ctl_dspace < 0 ) || ( crash_dspace < 0 ) ) {

	    ret_val = 1;
	    fprintf(stderr, "Can't get one or more data spaces.\n");

	}
    }


    /* verify that ranks are as expected */
    if ( ( ret_val == 0 ) && ( ! done ) ) {

        ctl_rank = H5Sget_simple_extent_ndims(ctl_dspace);
        crash_rank = H5Sget_simple_extent_ndims(crash_dspace);

        if ( ( ctl_rank < 0 ) || ( crash_rank < 0 ) ) {

            ret_val = 1;
            fprintf(stderr, "Can't get one or more data space ranks.\n");

        } else if ( ctl_rank != crash_rank ) {

	    ret_val = 1;
            fprintf(stderr, "Contig dspace rank mismatch.\n");

        } else if ( ctl_rank != RANK ) {

	    ret_val = 1;
            fprintf(stderr, "Unexpected chunk dspace rank(1).\n");

	}
    }


    /* get the extents */
    if ( ( ret_val == 0 ) && ( ! done ) ) {

        ctl_rank = H5Sget_simple_extent_dims(ctl_dspace, 
                                             ctl_dims, 
                                             ctl_maxdims );
        if ( ctl_rank != RANK ) {

	    ret_val = 1;
            fprintf(stderr, "Unexpected chunk ctl dspace rank.\n");

	}

        crash_rank =  H5Sget_simple_extent_dims(crash_dspace, 
                                                crash_dims, 
                                                crash_maxdims);
        if ( ctl_rank != RANK ) {

	    ret_val = 1;
            fprintf(stderr, "Unexpected chunk crash dspace rank.\n");

	}
    }


    /* verify that the dims contain the expected data */
    if ( ( ret_val == 0 ) && ( ! done ) ) {

        if ( crash_dims[0] > ctl_dims[0] ) {

	    ret_val = 1;
            fprintf(stderr, "chunk dspace crash_dims[0] > ctl_dims[0].\n");
            fprintf(stderr, "ctl_dims[0] = %d, crash_dims[0] = %d.\n",
                    (int)(ctl_dims[0]), (int)(crash_dims[0]));

        } else if ( (ctl_dims[0] % NX) != 0 ) {

	    ret_val = 1;
            fprintf(stderr, "chunk dspace (ctl_dims[0] mod NX) != 0.\n");

        } else if ( (crash_dims[0] % NX) != 0 ) {

	    ret_val = 1;
            fprintf(stderr, "chunk dspace (crash_dims[0] mod NX) != 0.\n");

        } else if ( crash_dims[1] != ctl_dims[1] ) {

	    ret_val = 1;
            fprintf(stderr, "crash_dims[1] != ctl_dims[1].\n");

        } else if ( (ctl_dims[1] % NY) != 0 ) {

	    ret_val = 1;
            fprintf(stderr, "chunk dspace (ctl_dims[1] mod NY) != 0.\n");

        } else if ( (crash_dims[1] % NY) != 0 ) {

	    ret_val = 1;
            fprintf(stderr, "chunk dspace (crash_dims[1] mod NY) != 0.\n");

        } else if ( ( ctl_dims[0] > 4 * NX ) || ( ctl_dims[1] > NY ) ) {

	    ret_val = 1;
            fprintf(stderr, "chunk dspace ctl dims too big.\n");

        } else if ( ( crash_dims[0] > 4 * NX ) || ( crash_dims[1] > NY ) ) {

	    ret_val = 1;
            fprintf(stderr, "chunk dspace crash dims too big.\n");

        }
    }


    /* verify that the maxdims contain the expected data */
    if ( ( ret_val == 0 ) && ( ! done ) ) {

        if ( ctl_maxdims[0] != H5S_UNLIMITED ) {

	    ret_val = 1;
            fprintf(stderr, 
                      "chunk dspace ctl_maxdims[0] != H5S_UNLIMITED.\n");

	} else if ( ctl_maxdims[1] != NY ) {

	    ret_val = 1;
            fprintf(stderr, "chunk dspace ctl_maxdims[1] != NY.\n");

        } else if ( crash_maxdims[0] != H5S_UNLIMITED ) {

	    ret_val = 1;
            fprintf(stderr, 
                      "chunk dspace crash_maxdims[0] != H5S_UNLIMITED.\n");

	} else if ( crash_maxdims[1] != NY ) {

	    ret_val = 1;
            fprintf(stderr, "chunk dspace crash_maxdims[1] != NY.\n");
        }
    }


    /* get the underlying type of the data sets, and verify that it 
     * is as expected 
     */
    if ( ( ret_val == 0 ) && ( ! done ) ) {

        ctl_dtype = H5Dget_type(ctl_dset);
        crash_dtype = H5Dget_type(crash_dset);

        if ( H5Tequal(ctl_dtype, H5T_STD_I32LE) <= 0 ) {

	    ret_val = 1;
            fprintf(stderr, "unexpected chunked ctl_dset dtype.\n");

        } else if ( H5Tequal( crash_dtype, H5T_STD_I32LE ) <= 0 ) {

	    ret_val = 1;
            fprintf(stderr, "unexpected chunked crash_dset dtype.\n");

        }
    }


    /* get the creation property lists of the datasets, and verify
     * that they contain the expected data.
     */
    if ( ( ret_val == 0 ) && ( ! done ) ) {

        ctl_plist = H5Dget_create_plist(ctl_dset);
        crash_plist = H5Dget_create_plist(crash_dset);

        if ( ( ctl_plist < 0 ) || ( crash_plist < 0 ) ) {

	    ret_val = 1;
            fprintf(stderr, "Failure obtaining  chnked dset plist(s).\n");

        }
    }


    /* verify that the data sets are chunked as expected */
    if ( ( ret_val == 0 ) && ( ! done ) ) {

        if ( ( H5Pget_layout(ctl_plist) != H5D_CHUNKED ) ||
             ( H5Pget_layout(crash_plist) != H5D_CHUNKED ) ) {

	    ret_val = 1;
            fprintf(stderr, "One or more chnked dsets not chnked?.\n");

        }
    }


    /* verify that the data sets have the expected chunk sizes */
    if ( ( ret_val == 0 ) && ( ! done ) ) {

        if ( ( H5Pget_chunk(ctl_plist, ctl_rank, ctl_chnkdims) < 0 ) ||
             ( H5Pget_chunk(crash_plist, crash_rank, crash_chnkdims) < 0 ) ) {

            ret_val = 1;
            fprintf(stderr, "Failure obtaining chnk dimss).\n");
        }
        else if ( ( ctl_chnkdims[0] != ChunkX ) ||
                  ( ctl_chnkdims[1] != ChunkY ) ||
                  ( crash_chnkdims[0] != ChunkX ) ||
                  ( crash_chnkdims[1] != ChunkY ) ) {

            ret_val = 1;
            fprintf(stderr, "unexpected chnk dimss.\n");
        }
    }

    /* finally, read in the contents of the dsets and compare them.
     *
     * Since we are not journaling raw data, it is hard to say if 
     * any particular set of value is right or wrong, but we will
     * try to spot anything odd and comment.
     */

    /* start by setting up the memory spaces and mem space selections */
    if ( ( ret_val == 0 ) && ( ! done ) ) {

        ctl_memspace_dims[0] = ctl_dims[0];
        ctl_memspace_dims[1] = ctl_dims[1];
    
	ctl_memspace = H5Screate_simple(RANK, ctl_memspace_dims, NULL);

        crash_memspace_dims[0] = crash_dims[0];
        crash_memspace_dims[1] = crash_dims[1];
    
	crash_memspace = H5Screate_simple(RANK, crash_memspace_dims, NULL);

	if ( ( ctl_memspace < 0 ) || ( crash_memspace < 0 ) ) {

            ret_val = 1;
            fprintf(stderr, "memspace allocation failed.\n");

        } else {

	    ctl_result = H5Sselect_hyperslab(ctl_memspace, H5S_SELECT_SET,
			                     zero_offset, NULL, 
					     ctl_memspace_dims, NULL);

	    crash_result = H5Sselect_hyperslab(crash_memspace, H5S_SELECT_SET,
			                       zero_offset, NULL, 
					       crash_memspace_dims, NULL);

	    if ( ( ctl_result < 0 ) || ( crash_result < 0 ) ) {

                ret_val = 1;
                fprintf(stderr, "memspace hyperslab selection(s) failed.\n");

	    }
	}
    }


    /* set up the file space selections */
    if ( ( ret_val == 0 ) && ( ! done ) ) {


        ctl_result = H5Sselect_hyperslab(ctl_dspace, H5S_SELECT_SET, 
			                 zero_offset, NULL, ctl_dims, NULL);

        crash_result = H5Sselect_hyperslab(crash_dspace, H5S_SELECT_SET, 
			                   zero_offset, NULL, crash_dims, NULL);

        if ( ( ctl_result < 0 ) || ( crash_result < 0 ) ) {

            ret_val = 1;
            fprintf(stderr, "file hyperslab selection(s) failed.\n");
        }
    }


    /* read in the contents of the data sets */
    if ( ( ret_val == 0 ) && ( ! done ) ) {

        ctl_result = H5Dread(ctl_dset, H5T_NATIVE_INT, ctl_memspace,
			     ctl_dspace, H5P_DEFAULT, ctl_data);

        crash_result = H5Dread(crash_dset, H5T_NATIVE_INT, crash_memspace,
			       crash_dspace, H5P_DEFAULT, crash_data);


        if ( ( ctl_result < 0 ) || ( crash_result < 0 ) ) {

            ret_val = 1;
            fprintf(stderr, "chunked dset read(s) failed.\n");
        }
    }

    /* scan the data:
     *
     * Since we are only doing metadata journaling, not raw data journaling,
     * it is hard to say that any particular raw data is wrong.  However,
     * typically, we should see one of the following cases:
     *
     * 1) All good data.
     *
     * 2) All bad data.
     *
     * 3) Good data at the beginning, turning to gargbage somewhere along 
     *    the way.  This should be the typical case.
     *    Further, since we this is a chunked data set, we would expect
     *    each chunk to be all good or all bad.
     *
     * The following code is an attempt to check this.  We will not flag
     * any errors, but if we see anything odd, we will dump the contents 
     * of the data set.
     */

    if ( ( ret_val == 0 ) && ( ! done ) ) {

        /* do a raster scan of the dataset and cont the number of 
         * transitions from good data to bad data and vise versa.
         * 
         * If there are "too many" transitions, make note and request
         * a dump of the data sets.
         *
         * Similarly, if any streak of good or bad data is not a 
         * multiple of the chunk height, make note and request a 
         * dump of the data sets.
         */

        good_data = TRUE;
        odd_streak = FALSE;
        streak_len = 0;
        transitions = 0;

        for ( i = 0; i < crash_dims[0]; i++ )
        {
	    for ( j = 0; j < crash_dims[1]; j++ )
            {
                if ( good_data ) {

                    if ( ctl_data[i][j] != crash_data[i][j] ) {

                        good_data = FALSE;
                        transitions++;

                        if ( (streak_len % ChunkY) != 0 ) {

                            odd_streak = TRUE;
                        }
                        streak_len = 0;

                    } else {

                        streak_len++;

                    }
                } else {

                    if ( ctl_data[i][j] == crash_data[i][j] ) {

                        good_data = TRUE;
                        transitions++;

                        if ( (streak_len % ChunkY) != 0 ) {

                            odd_streak = TRUE;
                        }
                        streak_len = 0;

                    } else {

                        streak_len++;

                    }
                }
            }
        }

        if ( odd_streak ) {

            fprintf(stdout, "\nFound odd length streak of good or bad data.\n");
            fprintf(stdout, "Dumping the ctl and crash chunked data sets.\n");
            dump_ctl_dset = TRUE;
            dump_crash_dset = TRUE;
        }

        if ( transitions > (2 * ChunkX) ) {

            fprintf(stdout, 
                    "\nToo many transitions between good and bad data.\n");
            fprintf(stdout, "Dumping the ctl and crash chunked data sets.\n");
            dump_ctl_dset = TRUE;
            dump_crash_dset = TRUE;
        }
    }


    /* dump ctl and/or crash data set if requested */
    if ( ( ret_val == 0 ) && ( ! done ) ) {

        if ( dump_ctl_dset ) {

	    fprintf(stdout, "\nctl data:\n");

	    for ( i = 0; i < ctl_dims[0]; i++ )
	    {
		fprintf(stdout, "(%d,0): ", i);

                for ( j = 0; j < ctl_dims[1]; j++ )
		{
                    fprintf(stdout, "%d, ", ctl_data[i][j]);
		}
		fprintf(stdout, "\n");
	    }

        }

        if ( dump_crash_dset ) {

	    fprintf(stdout, "\ncrash data:\n");

	    for ( i = 0; i < crash_dims[0]; i++ )
	    {
		fprintf(stdout, "(%d,0): ", i);

                for ( j = 0; j < crash_dims[1]; j++ )
		{
                    fprintf(stdout, "%d, ", crash_data[i][j]);
		}
		fprintf(stdout, "\n");
	    }
        }
    }

    /* tidy up */

    if ( ctl_memspace >= 0 ) {
	H5Sclose(ctl_memspace);
	ctl_memspace = -1;
    }

    if ( crash_memspace >= 0 ) {
	H5Sclose(crash_memspace);
	crash_memspace = -1;
    }


    if ( ctl_dspace >= 0 ) {
	H5Sclose(ctl_dspace);
	ctl_dspace = -1;
    }

    if ( crash_dspace >= 0 ) {
	H5Sclose(crash_dspace);
	crash_dspace = -1;
    }

    if ( ctl_plist >= 0 ) {
	H5Pclose(ctl_plist);
	ctl_plist = -1;
    }

    if ( crash_plist >= 0 ) {
	H5Pclose(crash_plist);
	crash_plist = -1;
    }

    if ( ctl_dtype >= 0 ) {
        H5Tclose(ctl_dtype);
	ctl_dtype = -1;
    }

    if ( crash_dtype >= 0 ) {
        H5Tclose(crash_dtype);
	crash_dtype = -1;
    }

    if ( ctl_dset >= 0 ) {
	H5Dclose(ctl_dset);
	ctl_dset = -1;
    }

    if ( crash_dset >= 0 ) {
	H5Dclose(crash_dset);
	crash_dset = -1;
    }

    return(ret_val);

} /* verify_chunked_dset() */


int 
verify_recovery(void)
{
    int ret_val = 0;

    ret_val = open_files(H5FILE_NAME, CTL_H5FILE_NAME);

    if ( ret_val == 0 ) {

        ret_val = verify_chunked_dset();
    }

    if ( ctl_file >= 0 ) {

	H5Fclose(ctl_file);
	ctl_file = -1;
    }

    if ( datafile >= 0 ) {

	H5Fclose(datafile);
	datafile = -1;
    }

    return(ret_val);
}


