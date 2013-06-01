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

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Use Case 1.7	Appending a single chunk
 * Description:
 *     Appending a single chunk of raw data to a dataset along an unlimited
 *     dimension within a pre-created file and reading the new data back.
 * Goal:
 *     Read data appended by the Writer to a pre-existing dataset in a
 *     file. The dataset has one or more unlimited dimensions. The data is
 *     appended by a hyperslab that is contained in one chunk (for example,
 *     appending 2-dim planes along the slowest changing dimension in the
 *     3-dim dataset).
 * Level:
 *     User Level
 * Guarantees:
 *     o	Readers will see the modified dimension sizes after the Writer
 * 	finishes HDF5 metadata updates and issues H5Fflush or H5Oflush calls.
 *     o	Readers will see newly appended data after the Writer finishes
 * 	the flush operation.
 * 
 * Preconditions:
 *     o	Readers are not allowed to modify the file.  o	 All datasets
 * 	that are modified by the Writer exist when the Writer opens the file.
 *     o	All datasets that are modified by the Writer exist when a Reader
 * 	opens the file.  o	 Data is written by a hyperslab contained in
 * 	one chunk.
 * 
 * Main Success Scenario:
 *     1.	An application creates a file with required objects (groups,
 * 	datasets, and attributes).
 *     2.	The Writer application opens the file and datasets in the file
 * 	and starts adding data along the unlimited dimension using a hyperslab
 * 	selection that corresponds to an HDF5 chunk.
 *     3.	A Reader opens the file and a dataset in a file, and queries
 * 	the sizes of the dataset; if the extent of the dataset has changed,
 * 	reads the appended data back.
 * 
 * Discussion points:
 *     1.	Since the new data is written to the file, and metadata update
 * 	operation of adding pointer to the newly written chunk is atomic and
 * 	happens after the chunk is on the disk, only two things may happen
 * 	to the Reader:
 * 	    o	The Reader will not see new data.
 * 	    o	The Reader will see all new data written by Writer.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Created: Albert Cheng, 2013/5/28.
 * Modified:
 */

#include "use.h"

/* Global Variable definitions */
options_t UC_opts;	/* Use Case Options */
const char *progname_g="use_append_chunk";	/* program name */

/* Create the skeleton use case file for testing.
 * It has one 3d dataset using chunked storage.
 * The dataset is (unlimited, chunksize, chunksize).
 * Dataset type is 2 bytes integer.
 * It starts out "empty", i.e., first dimension is 0.
 *
 * Return: 0 succeed; -1 fail.
 */
int create_uc_file(void)
{
    hsize_t dims[3];		/* Dataset starting dimensions */
    hsize_t max_dims[3];	/* Dataset maximum dimensions */
    hsize_t chunk_dims[3];	/* Chunk dimensions */
    hid_t fid;          /* File ID for new HDF5 file */
    hid_t dcpl;         /* Dataset creation property list */
    hid_t sid;          /* Dataspace ID */
    hid_t dsid;         /* Dataset ID */

    /* Create the file */
    if((fid = H5Fcreate(UC_opts.filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        return -1;

    /* Set up dimension sizes */
    chunk_dims[0] = 1;
    dims[0] = 0;
    max_dims[0] = H5S_UNLIMITED;
    max_dims[1] = max_dims[2] = dims[1] = dims[2] = chunk_dims[1] = chunk_dims[2] = UC_opts.chunksize;

    /* Create dataspace for creating datasets */
    if((sid = H5Screate_simple(3, dims, max_dims)) < 0)
        return -1;

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        return -1;
    if(H5Pset_chunk(dcpl, 3, chunk_dims) < 0)
        return -1;

    /* create dataset of progname */
    if((dsid = H5Dcreate2(fid, progname_g, UC_DATATYPE, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
	return -1;

    /* Close everythign */
    if(H5Dclose(dsid) < 0)
	return -1;

    if(H5Sclose(sid) < 0)
        return -1;
    if(H5Fclose(fid) < 0)
        return -1;

    return 0;
}


/* Write planes to the dataset in the use case file.
 * Fill each plan with plan numbers and then write it to the nth plane.
 * Increase the plane number and repeat till end of dataset.
 *
 * Return: 0 succeed; -1 fail.
 */
int write_uc_file(void)
{
    hid_t	fid;          /* File ID for new HDF5 file */
    hid_t	dsid;         /* dataset ID */
    char	*name;
    UC_CTYPE	*buffer, *bufptr;	/* data buffer */
    int		cz=UC_opts.chunksize;		/* Chunk size */
    hid_t	f_sid;	    /* dataset file space id */
    hid_t	m_sid;	    /* memory space id */
    int		rank;	    /* rank */
    hsize_t	dims[3];    /* Dataspace dimensions */
    hsize_t	memdims[3]; /* Memory space dimensions */
    hsize_t	start[3] = {0,0,0}, count[3];    /* Hyperslab selection values */
    int		i, j, k;

    name = UC_opts.filename;

    /* Open the file */
    if((fid = H5Fopen(name, H5F_ACC_RDWR | (UC_opts.use_swmr ? H5F_ACC_SWMR_WRITE : 0), H5P_DEFAULT)) < 0){
	fprintf(stderr, "H5Fopen failed\n");
        return -1;
    }

    /* Open the dataset of the program name */
    if((dsid = H5Dopen2(fid, progname_g, H5P_DEFAULT)) < 0){
	fprintf(stderr, "H5Dopen2 failed\n");
	return -1;
    }

    /* allocate space for data buffer chunksize X chunksize of UC_CTYPE */
    /*cz = UC_opts.chunksize;*/
    memdims[0]=1;
    memdims[1] = memdims[2] = cz;
    if ((buffer=(UC_CTYPE*)HDmalloc(cz*cz*sizeof(UC_CTYPE)))==NULL) {
	fprintf(stderr, "malloc: failed\n");
	return -1;
    };

    /*
     * Get dataset rank and dimension.
     */
    f_sid = H5Dget_space(dsid);    /* Get filespace handle first. */
    rank  = H5Sget_simple_extent_ndims(f_sid);
    if (rank != 3){
	fprintf(stderr, "rank(%d) of dataset does not match\n", rank);
	return -1;
    }
    if (H5Sget_simple_extent_dims(f_sid, dims, NULL) < 0){
	fprintf(stderr, "H5Sget_simple_extent_dims got error\n");
	return -1;
    }
    printf("dataset rank %d, dimensions %lu x %lu x %lu\n",
	   rank, (unsigned long)(dims[0]), (unsigned long)(dims[1]), (unsigned long)(dims[2]));
    /* verify that file space dims are as expected and are consistent with memory space dims */
    if (dims[0] != 0 || dims[1] != memdims[1] || dims[2] != memdims[2]){
	fprintf(stderr, "dataset is not empty. Got dims=(%ld,%ld,%ld)\n",
	    (long)dims[0], (long)dims[1], (long)dims[2]);
	return -1;
    }
    
    /* setup mem-space for buffer */
    if ((m_sid=H5Screate_simple(rank, memdims, NULL))<0){
	fprintf(stderr, "H5Screate_simple for memory failed\n");
	return -1;
    };

    /* write planes */
    count[0]=1;
    count[1]=count[2]=cz;
    for (i=0; i<UC_opts.nplanes; i++){
	/* fill buffer with value i+1 */
	/* HDmemset(buffer, i+1, cz*cz*sizeof(UC_CTYPE));*/
	bufptr = buffer;
	for (j=0; j<cz; j++)
	    for (k=0; k<cz; k++)
		*bufptr++ = i;

	/* extend the dataset by one for new plane */
	dims[0]=i+1;
        if(H5Dset_extent(dsid, dims) < 0){
	    fprintf(stderr, "H5Dset_extent failed\n");
            return -1;
	}

        /* Get the dataset's dataspace */
        if((f_sid = H5Dget_space(dsid)) < 0){
	    fprintf(stderr, "H5Dset_extent failed\n");
            return -1;
	}

	start[0]=i;
        /* Choose the next plane to write */
        if(H5Sselect_hyperslab(f_sid, H5S_SELECT_SET, start, NULL, count, NULL) < 0){
	    fprintf(stderr, "Failed H5Sselect_hyperslab\n");
            return -1;
	}

        /* Write plane to the dataset */
        if(H5Dwrite(dsid, UC_DATATYPE, m_sid, f_sid, H5P_DEFAULT, buffer) < 0){
	    fprintf(stderr, "Failed H5Dwrite\n");
            return -1;
	}
	/* flush file to make the just written plane available. */
	if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0){
	    fprintf(stderr, "Failed to H5Fflush file\n");
	    return -1;
	}
    }

    /* Done writing. Free/Close all resources including data file */
    HDfree(buffer);
    if (H5Dclose(dsid) < 0){
	fprintf(stderr, "Failed to close datasete\n");
	return -1;
    }
    if (H5Sclose(m_sid) < 0){
	fprintf(stderr, "Failed to close memory space\n");
	return -1;
    }
    if (H5Sclose(f_sid) < 0){
	fprintf(stderr, "Failed to close file space\n");
	return -1;
    }
    if (H5Fclose(fid) < 0){
	fprintf(stderr, "Failed to close file id\n");
	return -1;
    }

    return 0;
}


/* Read planes from the dataset in the use case file.
 * It expects the dataset is being changed (growing).
 * It checks the unlimited dimension (1st one). When it increases,
 * it will try read the new plane and verify the data correctness.
 * (The nth plan should contain all "n".)
 * When the unlimited dimension grows to the chunksize (it becomes
 * a cube), that is the end of data. It will then return.
 *
 * Return: 0 succeed; -1 fail.
 */
int read_uc_file(void)
{
    hid_t	fid;          /* File ID for new HDF5 file */
    hid_t	dsid;         /* dataset ID */
    char	*name;
    UC_CTYPE	*buffer, *bufptr;	/* read data buffer */
    int		cz=UC_opts.chunksize;		/* Chunk size */
    hid_t	f_sid;	    /* dataset file space id */
    hid_t	m_sid;	    /* memory space id */
    int		rank;	    /* rank */
    hsize_t	dims[3];    /* Dataspace dimensions */
    hsize_t	memdims[3]; /* Memory space dimensions */
    int		nplane=0, nplane_old=0;	/* nth plane, last nth plane */
    hsize_t	start[3] = {0,0,0}, count[3];    /* Hyperslab selection values */
    int		j, k;
    int		nreadererr=0;
    int		nerrs;
    int		nonewplane;

    name = UC_opts.filename;

    /* Open the file */
    if((fid = H5Fopen(name, H5F_ACC_RDONLY | (UC_opts.use_swmr ? H5F_ACC_SWMR_READ : 0), H5P_DEFAULT)) < 0){
	fprintf(stderr, "H5Fopen failed\n");
        return -1;
    }

    /* Open the dataset of the program name */
    if((dsid = H5Dopen2(fid, progname_g, H5P_DEFAULT)) < 0){
	fprintf(stderr, "H5Dopen2 failed\n");
	return -1;
    }

    /* allocate space for data buffer chunksize X chunksize of UC_CTYPE */
    /*cz = UC_opts.chunksize;*/
    memdims[0]=1;
    memdims[1] = memdims[2] = cz;
    if ((buffer=(UC_CTYPE*)HDmalloc(cz*cz*sizeof(UC_CTYPE)))==NULL) {
	fprintf(stderr, "malloc: failed\n");
	return -1;
    };

    /*
     * Get dataset rank and dimension.
     * Verify dimension is as expected (unlimited,chunksize,chunksize).
     */
    f_sid = H5Dget_space(dsid);    /* Get filespace handle first. */
    rank  = H5Sget_simple_extent_ndims(f_sid);
    if (rank != 3){
	fprintf(stderr, "rank(%d) of dataset does not match\n", rank);
	return -1;
    }
    if (H5Sget_simple_extent_dims(f_sid, dims, NULL) < 0){
	fprintf(stderr, "H5Sget_simple_extent_dims got error\n");
	return -1;
    }
    printf("dataset rank %d, dimensions %lu x %lu x %lu\n",
	   rank, (unsigned long)(dims[0]), (unsigned long)(dims[1]), (unsigned long)(dims[2]));
    /* verify that file space dims are as expected and are consistent with memory space dims */
    if (dims[1] != memdims[1] || dims[2] != memdims[2]){
	fprintf(stderr, "dataset dimension is not as expected. Got dims=(%ld,%ld,%ld)\n",
	    (long)dims[0], (long)dims[1], (long)dims[2]);
	fprintf(stderr, "But memdims=(%ld,%ld,%ld)\n",
	    (long)memdims[0], (long)memdims[1], (long)memdims[2]);
	return -1;
    }
    
    /* setup mem-space for buffer */
    if ((m_sid=H5Screate_simple(rank, memdims, NULL))<0){
	fprintf(stderr, "H5Screate_simple for memory failed\n");
	return -1;
    };

    /* Read 1 plane at a time whenever the dataset grows larger
     * (along dim[0]) */
    count[0]=1;
    count[1]=count[2]=cz;
    /* quit when all nplanes, default cz, have been read */
    nonewplane=0;
    while (nplane_old < UC_opts.nplanes ){
	/* print progress message according to if new planes are availalbe */
	if (nplane_old < dims[0]) {
	    if (nonewplane){
		/* end the previous message */
		printf("\n");
		nonewplane=0;
	    }
	    printf("reading planes %d to %d\n", nplane_old, (int)dims[0]);
	}else{
	    if (nonewplane){
		printf(".");
		if (nonewplane>=30){
		    fprintf(stderr, "waited too long for new plane, quit.\n");
		    return -1;
		}
	    }else{
		/* print mesg only the first time; dots still no new plane */
		printf("no new planes to read ");
	    }
	    nonewplane++;
	    /* pause for a second */
	    sleep(1);
	}
	for (nplane=nplane_old; nplane < dims[0]; nplane++){
	    /* read planes between last old nplanes and current extent */
	    /* Get the dataset's dataspace */
	    if((f_sid = H5Dget_space(dsid)) < 0){
		fprintf(stderr, "H5Dget_space failed\n");
		return -1;
	    }

	    start[0]=nplane;
	    /* Choose the next plane to read */
	    if(H5Sselect_hyperslab(f_sid, H5S_SELECT_SET, start, NULL, count, NULL) < 0){
		fprintf(stderr, "H5Sselect_hyperslab failed\n");
		return -1;
	    }

	    /* Read the plane from the dataset */
	    if(H5Dread(dsid, UC_DATATYPE, m_sid, f_sid, H5P_DEFAULT, buffer) < 0){
		fprintf(stderr, "H5Dread failed\n");
		return -1;
	    }

	    /* compare read data with expected data value which is nplane */
	    bufptr = buffer;
	    nerrs=0;
	    for (j=0; j<cz; j++){
		for (k=0; k<cz; k++){
		    if (*bufptr++ != nplane){
			if (++nerrs < ErrorReportMax){
			    fprintf(stderr,
				"found error %d plane(%d,%d), expected %d, got %d\n",
				nplane, j, k, nplane, (int)*(bufptr-1));
			}
		    }
		}
	    }
	    if (nerrs){
		nreadererr++;
		fprintf(stderr, "found %d unexpected values in plane %d\n", nerrs, nplane);
	    }
	}
	/* Have read all current planes */
	nplane_old=dims[0];

	/* check if dataset has grown since last time */
	/* close dsid and file, then reopen them */
	if (H5Dclose(dsid) < 0){
	    fprintf(stderr, "H5Dclose failed\n");
	    return -1;
	}
	if (H5Fclose(fid) < 0){
	    fprintf(stderr, "H5Fclose failed\n");
	    return -1;
	}
	if((fid = H5Fopen(name, H5F_ACC_RDONLY | (UC_opts.use_swmr ? H5F_ACC_SWMR_READ : 0), H5P_DEFAULT)) < 0){
	    fprintf(stderr, "H5Fopen failed\n");
	    return -1;
	}
	if((dsid = H5Dopen2(fid, progname_g, H5P_DEFAULT)) < 0){
	    fprintf(stderr, "H5Dopen2 failed\n");
	    return -1;
	}
	f_sid = H5Dget_space(dsid);    /* Get filespace handle first. */
	if (H5Sget_simple_extent_dims(f_sid, dims, NULL) < 0){
	    fprintf(stderr, "H5Sget_simple_extent_dims got error\n");
	    return -1;
	}
    }

    if (nreadererr)
	return -1;
    else
	return 0;
}


/* Overall Algorithm: 
 * Parse options from user;
 * Generate/pre-created test files needed and close it;
 * fork: child process becomes the reader process;
 *       while parent process continues as the writer process;
 * both run till ending conditions are met.
 */
int
main(int argc, char *argv[])
{
    pid_t childpid=0;
    pid_t mypid, tmppid;
    int	child_status;
    int child_wait_option=0;
    int ret_value = 0;
    int child_ret_value;

    /* initialization */
    /* use case defaults */
    HDmemset(&UC_opts, 0, sizeof(options_t));
    UC_opts.h5_use_chunks = 1;	/* use chunked datasets */
    UC_opts.chunksize = Chunksize_DFT;
    UC_opts.use_swmr = 1;	/* use swmr open */

    /* parse options */
    if (parse_option(argc, argv) < 0){
	Hgoto_error(1);
    }

    /* ==============================================================*/
    /* UC_READWRITE: create datafile, launch both reader and writer. */
    /* UC_WRITER:    create datafile, skip reader, launch writer.    */
    /* UC_READER:    skip create, launch reader, exit.               */
    /* ==============================================================*/
    /* ============*/
    /* Create file */
    /* ============*/
    if (UC_opts.launch != UC_READER){
	printf("Creating skeleton data file for test...\n");
	if (create_uc_file() < 0){
	    fprintf(stderr, "***encounter error\n");
	    Hgoto_error(1);
	}else
	    printf("File created.\n");
    }

    if (UC_opts.launch==UC_READWRITE){
	/* fork process */
	if((childpid = fork()) < 0) {
	    perror("fork");
	    Hgoto_error(1);
	};
    };
    mypid = getpid();

    /* ============= */
    /* launch reader */
    /* ============= */
    if (UC_opts.launch != UC_WRITER){
	/* child process launch the reader */
	if(0 == childpid) {
	    printf("%d: launch reader process\n", mypid);
	    if (read_uc_file() < 0){
		fprintf(stderr, "read_uc_file encountered error\n");
		exit(1);
	    }
	    exit(0);
	}
    }

    /* ============= */
    /* launch writer */
    /* ============= */
    /* this process continues to launch the writer */
    printf("%d: continue as the writer process\n", mypid);
    if (write_uc_file() < 0){
	fprintf(stderr, "write_uc_file encountered error\n");
	Hgoto_error(1);
    }

    /* ================================================ */
    /* If readwrite, collect exit code of child process */
    /* ================================================ */
    if (UC_opts.launch == UC_READWRITE){
	if ((tmppid = waitpid(childpid, &child_status, child_wait_option)) < 0){
	    perror("waitpid");
	    Hgoto_error(1);
	}
	if (WIFEXITED(child_status)){
	    if ((child_ret_value=WEXITSTATUS(child_status)) != 0){
		printf("%d: child process exited with non-zero code (%d)\n",
		    mypid, child_ret_value);
		Hgoto_error(2);
	    }
	} else {
	    printf("%d: child process terminated abnormally\n", mypid);
	    Hgoto_error(2);
	}
    }
    
done:
    /* Print result and exit */
    if (ret_value != 0){
	printf("Error(s) encountered\n");
    }else{
	printf("All passed\n");
    }

    return(ret_value);
}
