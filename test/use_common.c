/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "h5test.h"

/* This test uses many POSIX things that are not available on
 * Windows. We're using a check for fork(2) here as a proxy for
 * all POSIX/Unix/Linux things until this test can be made
 * more platform-independent.
 */
#ifdef H5_HAVE_FORK

#include "use.h"

#define H5D_FRIEND        /*suppress error about including H5Dpkg      */
#define H5D_TESTING
#include "H5Dpkg.h"

void
usage(const char *prog)
{
    HDfprintf(stderr, "usage: %s [OPTIONS]\n", prog);
    HDfprintf(stderr, "  OPTIONS\n");
    HDfprintf(stderr, "     -h, --help            Print a usage message and exit\n");
    HDfprintf(stderr, "     -f FN                 Test file name [default: %s.h5]\n", prog);
    HDfprintf(stderr, "     -i N, --iteration=N   Number of iterations to repeat the whole thing. [default: 1]\n");
    HDfprintf(stderr, "     -l w|r                launch writer or reader only. [default: launch both]\n");
    HDfprintf(stderr, "     -n N, --nplanes=N     Number of planes to write/read. [default: 1000]\n");
    HDfprintf(stderr, "     -s N, --swmr=N        Use SWMR mode (0: no, non-0: yes) default is yes\n");
    HDfprintf(stderr, "     -z N, --chunksize=N   Chunk size [default: %d]\n", Chunksize_DFT);
    HDfprintf(stderr, "     -y N, --chunkplanes=N Number of planes per chunk [default: 1]\n");
    HDfprintf(stderr, "\n");
} /* end usage() */

/* Setup Use Case parameters by parsing command line options.
* Setup default values if not set by options. */
int
parse_option(int argc, char * const argv[])
{
    int ret_value=0;
    int c;
    int use_swmr;       /* Need an int to detect errors */

    /* command line options: See function usage for a description */
    const char *nagg_options = "f:hi:l:n:s:y:z:";

    /* suppress getopt from printing error */
    opterr = 0;

    while (1){
    c = getopt (argc, argv, nagg_options);
    if (-1 == c)
        break;
    switch (c) {
    case 'h':
        usage(progname_g);
        HDexit(EXIT_SUCCESS);
        break;
    case 'f':    /* usecase data file name */
        UC_opts.filename = optarg;
        break;
    case 'i':    /* iterations */
        if ((UC_opts.iterations = HDatoi(optarg)) <= 0) {
            HDfprintf(stderr, "bad iterations number %s, must be a positive integer\n", optarg);
        usage(progname_g);
        Hgoto_error(-1);
        };
        break;
    case 'l':    /* launch reader or writer only */
        switch (*optarg) {
        case 'r':    /* reader only */
        UC_opts.launch = UC_READER;
        break;
        case 'w': /* writer only */
        UC_opts.launch = UC_WRITER;
        break;
        default:
            HDfprintf(stderr, "launch value(%c) should be w or r only.\n", *optarg);
        usage(progname_g);
        Hgoto_error(-1);
        break;
        }
        break;
    case 'n':    /* number of planes to write/read */
        if ((UC_opts.nplanes = HDstrtoul(optarg, NULL, 0)) <= 0) {
            HDfprintf(stderr, "bad number of planes %s, must be a positive integer\n", optarg);
        usage(progname_g);
        Hgoto_error(-1);
        };
        break;
    case 's':    /* use swmr file open mode */
        use_swmr = HDatoi(optarg);
        if (use_swmr != 0 && use_swmr != 1) {
            HDfprintf(stderr, "swmr value should be 0(no) or 1(yes)\n");
            usage(progname_g);
            Hgoto_error(-1);
        }
        UC_opts.use_swmr = (hbool_t)use_swmr;
        break;
    case 'y':    /* Number of planes per chunk */
        if ((UC_opts.chunkplanes = HDstrtoul(optarg, NULL, 0)) <= 0) {
            HDfprintf(stderr, "bad number of planes per chunk %s, must be a positive integer\n", optarg);
        usage(progname_g);
        Hgoto_error(-1);
        };
        break;
    case 'z':    /* size of chunk=(z,z) */
        if ((UC_opts.chunksize = HDstrtoull(optarg, NULL, 0)) <= 0) {
            HDfprintf(stderr, "bad chunksize %s, must be a positive integer\n", optarg);
        usage(progname_g);
        Hgoto_error(-1);
        };
        break;
    case '?':
        HDfprintf(stderr, "getopt returned '%c'.\n", c);
        Hgoto_error(-1);
    default:
        HDfprintf(stderr, "getopt returned unexpected value.\n");
        HDfprintf(stderr, "Unexpected value is %d\n", c);
        Hgoto_error(-1);
    }
    }

    /* set test file name if not given */
    if (!UC_opts.filename){
    /* default data file name is <progname>.h5 */
    if ((UC_opts.filename=(char*)HDmalloc(HDstrlen(progname_g)+4))==NULL) {
        HDfprintf(stderr, "malloc: failed\n");
        Hgoto_error(-1);
    };
    HDstrcpy(UC_opts.filename, progname_g);
    HDstrcat(UC_opts.filename, ".h5");
    }

done:
    /* All done. */
    return(ret_value);
}

/* Show parameters used for this use case */
void show_parameters(void){
    HDprintf("===Parameters used:===\n");
    printf("chunk dims=(%llu, %llu, %llu)\n", (unsigned long long)UC_opts.chunkdims[0],
        (unsigned long long)UC_opts.chunkdims[1], (unsigned long long)UC_opts.chunkdims[2]);
    printf("dataset max dims=(%llu, %llu, %llu)\n", (unsigned long long)UC_opts.max_dims[0],
        (unsigned long long)UC_opts.max_dims[1], (unsigned long long)UC_opts.max_dims[2]);
    HDprintf("number of planes to write=%llu\n", (unsigned long long)UC_opts.nplanes);
    HDprintf("using SWMR mode=%s\n", UC_opts.use_swmr ? "yes(1)" : "no(0)");
    HDprintf("data filename=%s\n", UC_opts.filename);
    HDprintf("launch part=");
    switch (UC_opts.launch){
    case UC_READWRITE:
        printf("Reader/Writer\n");
        break;
    case UC_WRITER:
        printf("Writer\n");
        break;
    case UC_READER:
        printf("Reader\n");
        break;
    default:
        /* should not happen */
        printf("Illegal part(%d)\n", UC_opts.launch);
    };
    HDprintf("number of iterations=%d (not used yet)\n", UC_opts.iterations);
    HDprintf("===Parameters shown===\n");
}

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
    hsize_t dims[3];        /* Dataset starting dimensions */
    hid_t fid;          /* File ID for new HDF5 file */
    hid_t dcpl;         /* Dataset creation property list */
    hid_t sid;          /* Dataspace ID */
    hid_t dsid;         /* Dataset ID */
    hid_t fapl;         /* File access property list */
    H5D_chunk_index_t idx_type; /* Chunk index type */

    /* Create the file */
    if((fapl = h5_fileaccess()) < 0)
        return -1;
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        return -1;
    if((fid = H5Fcreate(UC_opts.filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        return -1;

    /* Set up dimension sizes */
    dims[0] = 0;
    dims[1] = dims[2] = UC_opts.max_dims[1];

    /* Create dataspace for creating datasets */
    if((sid = H5Screate_simple(3, dims, UC_opts.max_dims)) < 0)
        return -1;

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        return -1;
    if(H5Pset_chunk(dcpl, 3, UC_opts.chunkdims) < 0)
        return -1;

    /* create dataset of progname */
    if((dsid = H5Dcreate2(fid, progname_g, UC_DATATYPE, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
    return -1;

    /* Check that the chunk index type is not version 1 B-tree.
     * Version 1 B-trees are not supported under SWMR.
     */
    if(H5D__layout_idx_type_test(dsid, &idx_type) < 0)
        return -1;
    if(idx_type == H5D_CHUNK_IDX_BTREE) {
        HDfprintf(stderr, "ERROR: Chunk index is version 1 B-tree: aborting.\n");
        return -1;
    }

    /* Close everything */
    if(H5Dclose(dsid) < 0)
    return -1;
    if(H5Pclose(fapl) < 0)
        return -1;
    if(H5Pclose(dcpl) < 0)
        return -1;
    if(H5Sclose(sid) < 0)
        return -1;
    if(H5Fclose(fid) < 0)
        return -1;

    return 0;
}

/* Append planes, each of (1,2*chunksize,2*chunksize) to the dataset.
 * In other words, 4 chunks are appended to the dataset at a time.
 * Fill each plan with the plane number and then write it at the nth plane.
 * Increase the plane number and repeat till the end of dataset, when it
 * reaches chunksize long. End product is a (2*chunksize)^3 cube.
 *
 * Return: 0 succeed; -1 fail.
 */
int write_uc_file(hbool_t tosend, hid_t fid)
{
    hid_t    dsid;         /* dataset ID */
    hid_t    dcpl;         /* Dataset creation property list */
    UC_CTYPE    *buffer, *bufptr;    /* data buffer */
    hsize_t    cz=UC_opts.chunksize;        /* Chunk size */
    hid_t    f_sid;        /* dataset file space id */
    hid_t    m_sid;        /* memory space id */
    int        rank;        /* rank */
    hsize_t     chunk_dims[3];    /* Chunk dimensions */
    hsize_t    dims[3];    /* Dataspace dimensions */
    hsize_t    memdims[3]; /* Memory space dimensions */
    hsize_t    start[3] = {0,0,0}, count[3];    /* Hyperslab selection values */
    hsize_t     i, j, k;

    if(tosend)
        /* Send a message that H5Fopen is complete--releasing the file lock */
        h5_send_message(WRITER_MESSAGE, NULL, NULL);

    /* Open the dataset of the program name */
    if((dsid = H5Dopen2(fid, progname_g, H5P_DEFAULT)) < 0){
        HDfprintf(stderr, "H5Dopen2 failed\n");
        return -1;
    }

    /* Find chunksize used */
    if ((dcpl = H5Dget_create_plist(dsid)) < 0){
        HDfprintf(stderr, "H5Dget_create_plist failed\n");
        return -1;
    }
    if (H5D_CHUNKED != H5Pget_layout(dcpl)){
        HDfprintf(stderr, "storage layout is not chunked\n");
        return -1;
    }
    if ((rank = H5Pget_chunk(dcpl, 3, chunk_dims)) != 3){
        HDfprintf(stderr, "storage rank is not 3\n");
        return -1;
    }

    /* verify chunk_dims against set paramenters */
    if (chunk_dims[0]!=UC_opts.chunkdims[0] || chunk_dims[1] != cz || chunk_dims[2] != cz){
        HDfprintf(stderr, "chunk size is not as expected. Got dims=(%llu,%llu,%llu)\n",
        (unsigned long long)chunk_dims[0], (unsigned long long)chunk_dims[1],
            (unsigned long long)chunk_dims[2]);
        return -1;
    }

    /* allocate space for data buffer 1 X dims[1] X dims[2] of UC_CTYPE */
    memdims[0]=1;
    memdims[1] = UC_opts.dims[1];
    memdims[2] = UC_opts.dims[2];
    if ((buffer=(UC_CTYPE*)HDmalloc((size_t)memdims[1]*(size_t)memdims[2]*sizeof(UC_CTYPE)))==NULL) {
        HDfprintf(stderr, "malloc: failed\n");
        return -1;
    };

    /*
     * Get dataset rank and dimension.
     */
    f_sid = H5Dget_space(dsid);    /* Get filespace handle first. */
    rank  = H5Sget_simple_extent_ndims(f_sid);
    if (rank != UC_RANK){
        HDfprintf(stderr, "rank(%d) of dataset does not match\n", rank);
        return -1;
    }
    if (H5Sget_simple_extent_dims(f_sid, dims, NULL) < 0){
        HDfprintf(stderr, "H5Sget_simple_extent_dims got error\n");
        return -1;
    }
    HDprintf("dataset rank %d, dimensions %llu x %llu x %llu\n",
    rank, (unsigned long long)(dims[0]), (unsigned long long)(dims[1]),
           (unsigned long long)(dims[2]));
    /* verify that file space dims are as expected and are consistent with memory space dims */
    if (dims[0] != 0 || dims[1] != memdims[1] || dims[2] != memdims[2]){
        HDfprintf(stderr, "dataset is not empty. Got dims=(%llu,%llu,%llu)\n",
        (unsigned long long)dims[0], (unsigned long long)dims[1],
            (unsigned long long)dims[2]);
        return -1;
    }

    /* setup mem-space for buffer */
    if ((m_sid=H5Screate_simple(rank, memdims, NULL))<0){
        HDfprintf(stderr, "H5Screate_simple for memory failed\n");
        return -1;
    };

    /* write planes */
    count[0]=1;
    count[1]=dims[1];
    count[2]=dims[2];
    for (i=0; i<UC_opts.nplanes; i++){
    /* fill buffer with value i+1 */
    bufptr = buffer;
    for (j=0; j<dims[1]; j++)
        for (k=0; k<dims[2]; k++)
        *bufptr++ = (UC_CTYPE)i;

        /* Cork the dataset's metadata in the cache, if SWMR is enabled */
        if(UC_opts.use_swmr)
            if(H5Odisable_mdc_flushes(dsid) < 0) {
                HDfprintf(stderr, "H5Odisable_mdc_flushes failed\n");
                return -1;
            }

    /* extend the dataset by one for new plane */
    dims[0]=i+1;
        if(H5Dset_extent(dsid, dims) < 0){
            HDfprintf(stderr, "H5Dset_extent failed\n");
            return -1;
    }

        /* Get the dataset's dataspace */
        if((f_sid = H5Dget_space(dsid)) < 0){
            HDfprintf(stderr, "H5Dset_extent failed\n");
            return -1;
    }

    start[0]=i;
        /* Choose the next plane to write */
        if(H5Sselect_hyperslab(f_sid, H5S_SELECT_SET, start, NULL, count, NULL) < 0){
            HDfprintf(stderr, "Failed H5Sselect_hyperslab\n");
            return -1;
    }

        /* Write plane to the dataset */
        if(H5Dwrite(dsid, UC_DATATYPE, m_sid, f_sid, H5P_DEFAULT, buffer) < 0){
            HDfprintf(stderr, "Failed H5Dwrite\n");
            return -1;
    }

        /* Uncork the dataset's metadata from the cache, if SWMR is enabled */
        if(UC_opts.use_swmr)
            if(H5Oenable_mdc_flushes(dsid) < 0) {
                HDfprintf(stderr, "H5Oenable_mdc_flushes failed\n");
                return -1;
            }

    /* flush file to make the just written plane available. */
    if(H5Dflush(dsid) < 0)
    {
        HDfprintf(stderr, "Failed to H5Fflush file\n");
        return -1;
    }
    }

    /* Done writing. Free/Close all resources including data file */
    HDfree(buffer);
    if (H5Dclose(dsid) < 0){
        HDfprintf(stderr, "Failed to close datasete\n");
        return -1;
    }
    if (H5Sclose(m_sid) < 0){
        HDfprintf(stderr, "Failed to close memory space\n");
        return -1;
    }
    if (H5Sclose(f_sid) < 0){
        HDfprintf(stderr, "Failed to close file space\n");
        return -1;
    }

    return 0;
}


/* Read planes from the dataset.
 * It expects the dataset is being changed (growing).
 * It checks the unlimited dimension (1st one). When it increases,
 * it will read in the new planes, one by one, and verify the data correctness.
 * (The nth plan should contain all "n".)
 * When the unlimited dimension grows to the chunksize (it becomes a cube),
 * that is the expected end of data, the reader exits.
 *
 * Return: 0 succeed; -1 fail.
 */
int read_uc_file(hbool_t towait)
{
    hid_t   fapl;         /* file access property list ID */
    hid_t    fid;          /* File ID for new HDF5 file */
    hid_t    dsid;         /* dataset ID */
    char    *name;
    UC_CTYPE    *buffer, *bufptr;    /* read data buffer */
    hid_t    f_sid;        /* dataset file space id */
    hid_t    m_sid;        /* memory space id */
    int        rank;        /* rank */
    hsize_t    dims[3];    /* Dataspace dimensions */
    hsize_t    memdims[3]; /* Memory space dimensions */
    hsize_t     nplane=0, nplane_old=0;    /* nth plane, last nth plane */
    hsize_t    start[3] = {0,0,0}, count[3];    /* Hyperslab selection values */
    hsize_t    j, k;
    int        nreadererr=0;
    int        nerrs;
    int        nonewplane;

    /* Before reading, wait for the message that H5Fopen is complete--file lock is released */
    if(towait && h5_wait_message(WRITER_MESSAGE) < 0) {
        HDfprintf(stderr, "Cannot find writer message file...failed\n");
        return -1;
    }

    name = UC_opts.filename;

    /* Open the file */
    if((fapl = h5_fileaccess()) < 0)
        return -1;
    if((fid = H5Fopen(name, H5F_ACC_RDONLY | (UC_opts.use_swmr ? H5F_ACC_SWMR_READ : 0), fapl)) < 0){
        HDfprintf(stderr, "H5Fopen failed\n");
        return -1;
    }
    if (H5Pclose(fapl) < 0){
        HDfprintf(stderr, "Failed to property list\n");
        return -1;
    }


    /* Open the dataset of the program name */
    if((dsid = H5Dopen2(fid, progname_g, H5P_DEFAULT)) < 0){
        HDfprintf(stderr, "H5Dopen2 failed\n");
        return -1;
    }

    /* allocate space for data buffer 1 X dims[1] X dims[2] of UC_CTYPE */
    memdims[0]=1;
    memdims[1] = UC_opts.dims[1];
    memdims[2] = UC_opts.dims[2];
    if ((buffer=(UC_CTYPE*)HDmalloc((size_t)memdims[1]*(size_t)memdims[2]*sizeof(UC_CTYPE)))==NULL) {
        HDfprintf(stderr, "malloc: failed\n");
        return -1;
    };

    /*
     * Get dataset rank and dimension.
     * Verify dimension is as expected (unlimited,2*chunksize,2*chunksize).
     */
    f_sid = H5Dget_space(dsid);    /* Get filespace handle first. */
    rank  = H5Sget_simple_extent_ndims(f_sid);
    if (rank != UC_RANK){
        HDfprintf(stderr, "rank(%d) of dataset does not match\n", rank);
        return -1;
    }
    if (H5Sget_simple_extent_dims(f_sid, dims, NULL) < 0){
        HDfprintf(stderr, "H5Sget_simple_extent_dims got error\n");
        return -1;
    }
    HDprintf("dataset rank %d, dimensions %llu x %llu x %llu\n",
            rank, (unsigned long long)(dims[0]), (unsigned long long)(dims[1]),
            (unsigned long long)(dims[2]));
    /* verify that file space dims are as expected and are consistent with memory space dims */
    if (dims[1] != memdims[1] || dims[2] != memdims[2]){
        HDfprintf(stderr, "dataset dimension is not as expected. Got dims=(%llu,%llu,%llu)\n",
            (unsigned long long)dims[0], (unsigned long long)dims[1],
            (unsigned long long)dims[2]);
        HDfprintf(stderr, "But memdims=(%llu,%llu,%llu)\n",
            (unsigned long long)memdims[0], (unsigned long long)memdims[1],
            (unsigned long long)memdims[2]);
        return -1;
    }

    /* setup mem-space for buffer */
    if ((m_sid=H5Screate_simple(rank, memdims, NULL))<0){
        HDfprintf(stderr, "H5Screate_simple for memory failed\n");
        return -1;
    };

    /* Read 1 plane at a time whenever the dataset grows larger
     * (along dim[0]) */
    count[0]=1;
    count[1]=dims[1];
    count[2]=dims[2];
    /* quit when all nplanes  have been read */
    nonewplane=0;
    while (nplane_old < UC_opts.nplanes ){
    /* print progress message according to if new planes are availalbe */
    if (nplane_old < dims[0]) {
        if (nonewplane){
            /* end the previous message */
            HDprintf("\n");
            nonewplane=0;
        }
        HDprintf("reading planes %llu to %llu\n", (unsigned long long)nplane_old,
                (unsigned long long)dims[0]);
    }else{
        if (nonewplane){
            HDprintf(".");
        if (nonewplane>=30){
            HDfprintf(stderr, "waited too long for new plane, quit.\n");
            return -1;
        }
        }else{
            /* print mesg only the first time; dots still no new plane */
            HDprintf("no new planes to read ");
        }
        nonewplane++;
        /* pause for a second */
        HDsleep(1);
    }
    for (nplane=nplane_old; nplane < dims[0]; nplane++){
        /* read planes between last old nplanes and current extent */
        /* Get the dataset's dataspace */
        if((f_sid = H5Dget_space(dsid)) < 0){
            HDfprintf(stderr, "H5Dget_space failed\n");
            return -1;
        }

        start[0]=nplane;
        /* Choose the next plane to read */
        if(H5Sselect_hyperslab(f_sid, H5S_SELECT_SET, start, NULL, count, NULL) < 0){
            HDfprintf(stderr, "H5Sselect_hyperslab failed\n");
            return -1;
        }

        /* Read the plane from the dataset */
        if(H5Dread(dsid, UC_DATATYPE, m_sid, f_sid, H5P_DEFAULT, buffer) < 0){
            HDfprintf(stderr, "H5Dread failed\n");
            return -1;
        }

        /* compare read data with expected data value which is nplane */
        bufptr = buffer;
        nerrs=0;
        for (j=0; j<dims[1]; j++){
            for (k=0; k<dims[2]; k++){
                if ((hsize_t)*bufptr++ != nplane){
                    if (++nerrs < ErrorReportMax){
                        HDfprintf(stderr,
                            "found error %llu plane(%llu,%llu), expected %llu, got %d\n",
                            (unsigned long long)nplane, (unsigned long long)j,
                            (unsigned long long)k, (unsigned long long)nplane, (int)*(bufptr-1));
                    }
                }
            }
        }
        if (nerrs){
            nreadererr++;
            HDfprintf(stderr, "found %d unexpected values in plane %llu\n", nerrs,
                    (unsigned long long)nplane);
        }
    }
    /* Have read all current planes */
    nplane_old=dims[0];

    /* check if dataset has grown since last time */
#if 0
    /* close dsid and file, then reopen them */
    if (H5Dclose(dsid) < 0){
        HDfprintf(stderr, "H5Dclose failed\n");
        return -1;
    }
    if (H5Fclose(fid) < 0){
        HDfprintf(stderr, "H5Fclose failed\n");
        return -1;
    }
    if((fid = H5Fopen(name, H5F_ACC_RDONLY | (UC_opts.use_swmr ? H5F_ACC_SWMR_READ : 0), H5P_DEFAULT)) < 0){
        HDfprintf(stderr, "H5Fopen failed\n");
        return -1;
    }
    if((dsid = H5Dopen2(fid, progname_g, H5P_DEFAULT)) < 0){
        HDfprintf(stderr, "H5Dopen2 failed\n");
        return -1;
    }
#else
    H5Drefresh(dsid);
#endif
    f_sid = H5Dget_space(dsid);    /* Get filespace handle first. */
    if (H5Sget_simple_extent_dims(f_sid, dims, NULL) < 0){
        HDfprintf(stderr, "H5Sget_simple_extent_dims got error\n");
        return -1;
    }
    }

    /* Close the file */
    if(H5Fclose(fid) < 0) {
        HDfprintf(stderr, "H5Fclose failed\n");
        return -1;
    }

    if (nreadererr)
        return -1;
    else
        return 0;
} /* read_uc_file() */

#endif /* H5_HAVE_FORK */

