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
#include "H5private.h"

/* The HDF5 test files */
const char *FILENAME[] = {
    "h5clear_sec2_v3.h5",        /* 0 -- sec2 file with superblock version 3 */
    "h5clear_log_v3.h5",        /* 1 -- log file with superblock veresion 3 */
    "h5clear_sec2_v0.h5",        /* 2 -- sec2 file with superblock version 0 */
    "h5clear_sec2_v2.h5"        /* 3 -- sec2 file with superblock version 2 */
};

const char *FILENAME_ENHANCE[] = {
    "h5clear_fsm_persist_equal.h5",         /* 0: persisting free-space, stored EOA = actual EOF */
    "h5clear_fsm_persist_greater.h5",       /* 1: persisting free-space, stored EOA > actual EOF */
    "h5clear_fsm_persist_less.h5",          /* 2: persisting free-space, stored EOA < actual EOF */
    "h5clear_fsm_persist_user_equal.h5",    /* 3: user block, persisting free-space, stored EOA = actual EOF */
    "h5clear_fsm_persist_user_greater.h5",  /* 4: user block, persisting free-space, stored EOA > actual EOF */
    "h5clear_fsm_persist_user_less.h5",     /* 5: user block, persisting free-space, stored EOA < actual EOF */
    "h5clear_status_noclose.h5",            /* 6 -- v3 superblock, nonzero status_flags, no flush, exit,
                                               stored EOA < actual EOF */
    "h5clear_fsm_persist_noclose.h5"        /* 7 -- persisting free-space, no flush, exit, stored EOA < actual EOF */
};

#define KB         1024U

#define CACHE_IMAGE_FILE    "h5clear_mdc_image.h5"
#define DSET                "DSET"
#define DATASET             "dset"
#define NUM_ELMTS           100
#define USERBLOCK           512

/*-------------------------------------------------------------------------
 * Function:    gen_cache_image_file
 *
 * Purpose:        To create a file with cache image feature enabled.
 *
 * Return:      Success:    0
 *              Failure:    1
 *
 * Programmer:    Vailin Choi; March 2017
 *
 *-------------------------------------------------------------------------
 */
static int
gen_cache_image_file(const char *fname)
{
    hid_t fid = -1;                 /* File ID */
    hid_t did = -1, sid = -1;       /* Dataset ID, dataspace ID */
    hid_t fapl = -1;                /* File access property list */
    hid_t dcpl = -1;                /* Dataset creation property list */
    hsize_t dims[2];                /* Dimension sizes */
    hsize_t chunks[2];              /* Chunked dimension sizes */
    int buf[50][100];               /* Buffer for data to write */
    int i, j;                       /* Local index variables */
    H5AC_cache_image_config_t cache_image_config =  /* Cache image input configuration */
                            { H5AC__CURR_CACHE_IMAGE_CONFIG_VERSION,
                              TRUE, FALSE,
                              H5AC__CACHE_IMAGE__ENTRY_AGEOUT__NONE};

    /* Create a copy of file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;

    /* Enable latest format in fapl */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        goto error;

    /* Enable metadata cache image in fapl */
    if(H5Pset_mdc_image_config(fapl, &cache_image_config) < 0)
        goto error;

    /* Create the file */
    if((fid = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        goto error;

    /* Create dataspace */
    dims[0] = 50;
    dims[1] = 100;
    if((sid = H5Screate_simple(2, dims, NULL)) < 0)
        goto error;

    /* Initialize buffer for writing to dataset */
    for(i = 0; i < 50; i++)
        for(j = 0; j < 100; j++)
            buf[i][j] = i * j;

    /* Set up to create a chunked dataset */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    chunks[0] = 5;
    chunks[1] = 10;
    if(H5Pset_chunk(dcpl, 2, chunks) < 0)
        goto error;
    if((did = H5Dcreate2(fid, DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Write to the dataset */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        goto error;

    /* Closing */
    if(H5Dclose(did) < 0)
        goto error;
    if(H5Pclose(dcpl) < 0)
        goto error;
    if(H5Pclose(fapl) < 0)
        goto error;
    if(H5Sclose(sid) < 0)
        goto error;
    if(H5Fclose(fid) < 0)
        goto error;
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Sclose(sid);
        H5Dclose(did);
        H5Fclose(fid);
        H5Pclose(fapl);
        H5Pclose(dcpl);
    } H5E_END_TRY;
    return 1;
} /* gen_cache_image_file() */

/*-------------------------------------------------------------------------
 * Function:    gen_enhance_files
 *
 * Purpose:        To create the first 6 files in FILENAME_ENHANCE[]:
 *                  (0) FILENAME_ENHANCE[0]: "h5clear_fsm_persist_equal.h5"
 *                  (1) FILENAME_ENHANCE[1]: "h5clear_fsm_persist_greater.h5"
 *                  (2) FILENAME_ENHANCE[2]: "h5clear_fsm_persist_less.h5"
 *                  (3) FILENAME_ENHANCE[3]: "h5clear_user_fsm_persist_equal.h5"
 *                  (4) FILENAME_ENHANCE[4]: "h5clear_user_fsm_persist_greater.h5"
 *                  (5) FILENAME_ENHANCE[5]: "h5clear_user_fsm_persist_less.h5"
 *              After creating the files for #1, #2, #4 #5, write invalid EOA
 *              value to the location where the EOA is stored in the superblock.
 *              Also modify the chksum in the superblock due to this change.
 *
 *              The first call to this routine (without user block) will generate
 *              the first 3 files.
 *              The second call to this routine (with user block) will generate
 *              the last 3 files.
 *
 * Return:      Success:    0
 *              Failure:    1
 *
 * Programmer:    Vailin Choi; March 2017
 *
 *-------------------------------------------------------------------------
 */
static int
gen_enhance_files(hbool_t user)
{
    hid_t fid = -1;         /* File ID */
    hid_t fcpl = -1;        /* File creation property list */
    hid_t sid = -1;         /* Dataspace ID */
    hid_t did = -1;         /* Dataset ID */
    hsize_t dim[1];         /* Dimension sizes */
    int data[NUM_ELMTS];    /* Buffer for data */
    int fd = -1;            /* The file descriptor ID */
    int64_t eoa;            /* The EOA value */
    uint32_t chksum;        /* The chksum value */
    int i = 0 , j = 0, u = 0;   /* Local index variable */

    /* Get a copy of the default file creation property */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        goto error;

    /* Check to see if user block will be added */
    if(user) {
        if(H5Pset_userblock(fcpl, (hsize_t)USERBLOCK) < 0)
            goto error;
        u = 3;
    }

    /* Set file space strategy and persisting free-space */
    if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_FSM_AGGR, TRUE, (hsize_t)1) < 0)
        goto error;

    /*
     * Create the file, then write invalid EOA to the file.
     */
    for(i = 0+u; i < 3+u; i++) {

        /* Create the file with the file space strategy and persisting free-space */
        if((fid = H5Fcreate(FILENAME_ENHANCE[i], H5F_ACC_TRUNC, fcpl, H5P_DEFAULT)) < 0)
            goto error;

        /* Create the dataset */
        dim[0] = NUM_ELMTS;
        if((sid = H5Screate_simple(1, dim, NULL)) < 0)
           goto error;
        if((did = H5Dcreate2(fid, DATASET, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto error;

        for(j = 0; j < NUM_ELMTS; j++)
            data[j] = j;

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

        /*
         * No further action for:
         *      --FILENAME_ENHANCE[0]: "h5clear_fsm_persist_equal.h5"
         *      --FILENAME_ENHANCE[3]: "h5clear_fsm_persist_user_equal.h5",
         */
        if(!(i % 3))
            continue;
        /*
         * For the following files:
         *      --FILENAME_ENHANCE[1]: "h5clear_fsm_persist_greater.h5"
         *      --FILENAME_ENHANCE[2]: "h5clear_fsm_persist_less.h5"
         *      --FILENAME_ENHANCE[4]: "h5clear_fsm_persist_greater.h5"
         *      --FILENAME_ENHANCE[5]: "h5clear_fsm_persist_less.h5"
         *
         *  Write invalid value to the location for stored eoa and
         *  update the chksum value.
         */
        /* Open the file */
        if((fd = open(FILENAME_ENHANCE[i], O_RDWR, 0663)) < 0)
            goto error;

        switch(i) {
            case 1: /* stored EOA is > EOF */
                eoa = 3048;
                chksum = 268376587;
                break;

            case 2: /* stored EOA is < EOF */
                eoa = 512;
                chksum = 372920305;
                break;

            case 4: /* with userblock, stored EOA > EOF */
                eoa = 4000;
                chksum = 4168810027;
                break;

            case 5: /* with userblock, stored EOA < EOF */
                eoa = 3000;
                chksum = 3716054346;
                break;

            default:
                break;
        }

        /* location of "end of file address" */
        if(lseek(fd, (off_t)(28+(user?USERBLOCK:0)), SEEK_SET) < 0)
            goto error;

        /* Write the bad eoa value to the file */
        if(write(fd, &eoa, sizeof(eoa)) < 0)
            goto error;

        /* location of "superblock checksum" */
        if(lseek(fd, (off_t)(44+(user?USERBLOCK:0)), SEEK_SET) < 0)
            goto error;

        /* Write the chksum value to the file */
        if(write(fd, &chksum, sizeof(chksum)) < 0)
            goto error;

        /* Close the file */
        if(close(fd) < 0)
            goto error;

    } /* end for */

    /* Close the property list */
    if(H5Pclose(fcpl) < 0)
        goto error;

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Sclose(sid);
        H5Dclose(did);
        H5Fclose(fid);
        H5Pclose(fcpl);
    } H5E_END_TRY;
    return 1;
} /* gen_enhance_files() */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Generate test files used by h5clear.
 *
 *      (A) gen_cache_image_file():
 *          --generate a file with cache image feature
 *          --"h5clear_mdc_image.h5"
 *      (B) gen_enhance_files():
 *          --generate the first 6 files in FILENAME_ENHANCE[]:
 *              (0) "h5clear_fsm_persist_equal.h5"
 *              (1) "h5clear_fsm_persist_greater.h5"
 *              (2) "h5clear_fsm_persist_less.h5"
 *              (3) "h5clear_fsm_persist_user_equal.h5"
 *              (4) "h5clear_fsm_persist_user_greater.h5"
 *              (5) "h5clear_fsm_persist_user_less.h5"
 *
 *      (C) Generate the following FILENAME[] files in main():
 *              (0a) "h5clear_sec2_v3.h5"
 *              (0b) "latest_h5clear_sec2_v3.h5"
 *              (1a) "h5clear_log_v3.h5",
 *              (1b) "latest_h5clear_log_v3.h5"
 *              (2) "h5clear_sec2_v0.h5"
 *              (3) "h5clear_sec2_v2.h5"
 *
 *          These HDF5 files are created with non-zero status_flags in
 *          the superblock via flushing and exiting without closing the
 *          library.
 *            Due to file locking, status_flags in the superblock will be
 *            nonzero after H5Fcreate.  The library will clear status_flags
 *            on file closing.
 *          This program, after "H5Fcreate" the files, exits without
 *            going through library closing. Thus, status_flags for these
 *            files are not cleared.
 *            The library will check consistency of status_flags when
 *            opening a file with superblock >= v3 and will return error
 *          accordingly.
 *            The library will not check status_flags when opening a file
 *            with < v3 superblock.
 *            These files are used by "h5clear" to see if the tool clears
 *            status_flags properly so users can open the files afterwards.
 *
 *      (D) Generate the last two files in FILENAME_ENHANCE[] in main():
 *              (6) "h5clear_status_noclose.h5",
 *              (7) "h5clear_fsm_persist_noclose.h5"
 *
 * Return:    Success:    0
 *            Failure:    1
 *
 * Programmer:    Vailin Choi; July 2013
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t fid = -1;            /* File ID */
    hid_t fcpl = -1;        /* File creation property list */
    hid_t fapl = -1, new_fapl = -1;    /* File access property lists */
    char fname[512];        /* File name */
    unsigned new_format;    /* To use latest library format or not */
    hid_t sid = -1;         /* Dataspace ID */
    hid_t did = -1;         /* Dataset ID */
    hsize_t dim[1];         /* Dimension sizes */
    int data[NUM_ELMTS];    /* Buffer for data */
    int i;                  /* Local index variables */

    /* Generate a file with cache image feature enabled */
    if(gen_cache_image_file(CACHE_IMAGE_FILE) < 0)
        goto error;

    /* Generate the first 6 files in FILENAME_ENHANCE[]  */
    if(gen_enhance_files(FALSE) < 0)
        goto error;
    if(gen_enhance_files(TRUE) < 0)
        goto error;

    /*
     * Generate files in FILENAME[]
     */
    /* Create a copy of the file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;

    /* Copy the file access property list */
    if((new_fapl = H5Pcopy(fapl)) < 0)
        goto error;
    /* Set to latest library format */
    if(H5Pset_libver_bounds(new_fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        goto error;

    /*
     * Files created within this for loop will have v3 superblock and nonzero status_flags
     *      --FILENAME[0]: "h5clear_sec2_v3.h5", "latest_h5clear_sec2_v3.h5"
     *      --FILENAME[1]: "h5clear_log_v3.h5", "latest_h5clear_log_v3.h5"
     */
    for(new_format = FALSE; new_format <= TRUE; new_format++) {
        hid_t fapl2, my_fapl;    /* File access property lists */

        /* Set to use the appropriate file access property list */
        if(new_format)
            fapl2 = new_fapl;
        else
            fapl2 = fapl;
        /*
         * Create a sec2 file
         */
        if((my_fapl = H5Pcopy(fapl2)) < 0)
            goto error;
        /* Create the file */
        HDsprintf(fname, "%s%s", new_format? "latest_":"", FILENAME[0]);
        if((fid = H5Fcreate(fname, H5F_ACC_TRUNC | (new_format ? 0 : H5F_ACC_SWMR_WRITE), H5P_DEFAULT, my_fapl)) < 0)
            goto error;

        /* Flush the file */
        if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
            goto error;

        /* Close the property list */
        if(H5Pclose(my_fapl) < 0)
            goto error;

        /*
         * Create a log file
         */
        /* Create a copy of file access property list */
        if((my_fapl = H5Pcopy(fapl2)) < 0)
            goto  error;

        /* Setup the fapl for the log driver */
        if(H5Pset_fapl_log(my_fapl, "append.log", (unsigned long long)H5FD_LOG_ALL, (size_t)(4 * KB)) < 0)
            goto error;

        /* Create the file */
        HDsprintf(fname, "%s%s", new_format? "latest_":"", FILENAME[1]);
        if((fid = H5Fcreate(fname, H5F_ACC_TRUNC | (new_format ? 0 : H5F_ACC_SWMR_WRITE), H5P_DEFAULT, my_fapl)) < 0)
            goto error;

        /* Flush the file */
        if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
            goto error;

        /* Close the property list */
        if(H5Pclose(my_fapl) < 0)
            goto error;

    } /* end for */

    /*
     * Create a sec2 file with v0 superblock but nonzero status_flags:
     *      FILENAME[2]: "h5clear_sec2_v0.h5"
     */
    if((fid = H5Fcreate(FILENAME[2], H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        goto error;

    /* Flush the file */
    if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
        goto error;


    /*
     * Create a sec2 file with v2 superblock but nonzero status_flags:
     *      FILENAME[3]: "h5clear_sec2_v2.h5"
     */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        goto error;
    if(H5Pset_shared_mesg_nindexes(fcpl, 1) < 0)
        goto error;
    if(H5Pset_shared_mesg_index(fcpl, 0, H5O_SHMESG_DTYPE_FLAG, 50) < 0)
        goto error;

    if((fid = H5Fcreate(FILENAME[3], H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        goto error;

    /* Flush the file */
    if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
        goto error;


    /* Close the property lists */
    if(H5Pclose(fapl) < 0)
        goto error;
    if(H5Pclose(new_fapl) < 0)
        goto error;
    if(H5Pclose(fcpl) < 0)
        goto error;

    /*
     * Create the last two files in FILENAME_ENHANCE[]:
     * --FILENAME_ENHANCE[6]: h5clear_status_noclose.h5
     * --FILENAME_ENHANCE[7]: h5clear_fsm_persist_noclose.h5
     */
    /*
     * FILENAME_ENHANCE[6]: h5clear_status_noclose.h5
     *  --stored EOA < actual EOF
     *  --version 3 superblock
     *  --nonzero status_flags
     *  --does not persist free-space
     *  --does not flush the file, just exit without closing file:
     *  --this file is similar to the user-suppplied test file attached with HDFFV-10347
     */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
       goto error;

    /* Set to latest format */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
       goto error;

    /* Create file with SWMR-write access */
    if((fid = H5Fcreate(FILENAME_ENHANCE[6], H5F_ACC_TRUNC|H5F_ACC_SWMR_WRITE, H5P_DEFAULT, fapl)) < 0)
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
    if(H5Pclose(fapl) < 0)
        goto error;

    /* Does not flush and does not close the file */


    /*
     * FILENAME_ENHANCE[7]: h5clear_fsm_persist_noclose.h5
     *  --stored EOA < actual EOF
     *  --persisting free-space
     *  --undefined fsinfo.eoa_pre_fsm_fsalloc
     *  --undefined fsinfo.fs_addr
     *  --does not flush the file, just exit without closing
     */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        goto error;

    /* Set file space strategy and persisting free-space */
    if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_FSM_AGGR, TRUE, (hsize_t)1) < 0)
       goto error;

    /* Create the file with the set file space info */
    if((fid = H5Fcreate(FILENAME_ENHANCE[7], H5F_ACC_TRUNC, fcpl, H5P_DEFAULT)) < 0)
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
    if(H5Pclose(fcpl) < 0)
        goto error;

    /* Does not flush and does not close the file */


    fflush(stdout);
    fflush(stderr);

    /* Not going through library closing by calling _exit(0) with success */
    HD_exit(0);

error:

    /* Exit with failure */
    HD_exit(1);
}
