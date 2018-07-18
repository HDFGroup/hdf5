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

/************************************************************

  This example illustrates the concept of virtual dataset.
  The program  creates three 1-dim source datasets and writes
  data to them. Then it creates a 2-dim virtual dataset and
  maps the first three rows of the virtual dataset to the data
  in the source datasets. Elements of a row are mapped to all
  elements of the corresponding source dataset.
  The fourth row is not mapped and will be filled with the fill
  values when virtual dataset is read back.

  The program closes all datasets, and then reopens the virtual
  dataset, and finds and prints its creation properties.
  Then it reads the values.

  This file is intended for use with HDF5 Library version 1.10

 ************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "hdf5.h"

int mpi_rank;
int mpi_size;
int nerrors = 0;

/* Set some minimum values to work without actually calling MPI */
int mpi_global_rank = 0;
int mpi_global_size = 1;

#include "testpar.h"

#define NFILENAMES 4

const char *FILENAMES[NFILENAMES + 1]={"subfile_a",
                                       "subfile_b",
                                       "subfile_c",
                                       "subfile_d",
                                       NULL};

const char *DSETNAMES[NFILENAMES + 1]={"A",
                                       "B",
                                       "C",
                                       "D",
                                       NULL};


#define FILENAME_BUF_SIZE 1024

#define RANK_ELEMENTS   100
#define SUBFILE_ELEMENTS (RANK_ELEMENTS * 2)

#define FILE         "subfile_vds.h5"
#define DATASET      "VDS"
#define HALFGROUP       100
#define VDSDIM1         200
#define VDSDIM0         4
#define DIM0            200
#define RANK1           1
#define RANK2           2

/* The following can be used for file cleanup
 * after running the test.
 */
const char *SRC_FILE[] = {
    "subfile_a.h5",
    "subfile_b.h5",
    "subfile_c.h5",
    "subfile_d.h5",
    "subfile_vds.h5"
};

const char *SRC_DATASET[] = {
    "A",
    "B",
    "C"
};


/* File_Access_type bits */
#define FACC_DEFAULT    0x0     /* default */
#define FACC_MPIO       0x1     /* MPIO */
#define FACC_SPLIT      0x2     /* Split File */

/*
 * Create the appropriate File access property list
 */
static hid_t
create_faccess_plist(MPI_Comm comm, MPI_Info info, int l_facc_type)
{
    hid_t ret_pl = -1;
    herr_t ret;                 /* generic return value */

    ret_pl = H5Pcreate (H5P_FILE_ACCESS);
    VRFY((ret_pl >= 0), "H5P_FILE_ACCESS");

    if (l_facc_type == FACC_DEFAULT)
	return (ret_pl);

    if (l_facc_type == FACC_MPIO){
	/* set Parallel access with communicator */
	ret = H5Pset_fapl_mpio(ret_pl, comm, info);
	VRFY((ret >= 0), "");
        ret = H5Pset_all_coll_metadata_ops(ret_pl, TRUE);
	VRFY((ret >= 0), "");
        ret = H5Pset_coll_metadata_write(ret_pl, TRUE);
	VRFY((ret >= 0), "");
	return(ret_pl);
    }

    if (l_facc_type == (FACC_MPIO | FACC_SPLIT)){
	hid_t mpio_pl;

	mpio_pl = H5Pcreate (H5P_FILE_ACCESS);
	VRFY((mpio_pl >= 0), "");
	/* set Parallel access with communicator */
	ret = H5Pset_fapl_mpio(mpio_pl, comm, info);
	VRFY((ret >= 0), "");

	/* setup file access template */
	ret_pl = H5Pcreate (H5P_FILE_ACCESS);
	VRFY((ret_pl >= 0), "");
	/* set Parallel access with communicator */
	ret = H5Pset_fapl_split(ret_pl, ".meta", mpio_pl, ".raw", mpio_pl);
	VRFY((ret >= 0), "H5Pset_fapl_split succeeded");
	H5Pclose(mpio_pl);
	return(ret_pl);
    }

    /* unknown file access types */
    return (ret_pl);
}



/*-------------------------------------------------------------------------
 * Function:    generate_test_files
 *
 * Purpose:     This function is called to produce HDF5 dataset files
 *              which will eventually be used as the 'src' files in a
 *              containing Virtual Data Set (VDS) file.
 *
 *              Since data will be read back and validated, we generate
 *              data in a predictable manner rather than randomly.
 *              For now, we simply use the global mpi_rank of the writing
 *              process as a starting component for the data generation.
 *              Subsequent writes are increments from the initial start
 *              value.
 *
 * Return:      Success: 0
 *
 *              Failure: 1
 *
 * Programmer:  Richard Warren
 *              10/1/17
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
generate_test_files( MPI_Comm comm, int g_rank, int group_id )
{
    const char *fcn_name = "generate_test_files()";
    const char *failure_mssg = NULL;
    const char *dset_name = NULL;

    char data_filename[FILENAME_BUF_SIZE];

    int i, status;
    int group_size;
    int group_rank;
    int local_failure = 0;
    int global_failures = 0;
    int start_value;
    int   written[RANK_ELEMENTS],          /* Data to write */
        retrieved[RANK_ELEMENTS];          /* Data read in */
// hsize_t chunk_size = 16384;	           /* chunk size */

    hsize_t orig_size=RANK_ELEMENTS;   	   /* Original dataset dim size */
    hsize_t group_count = SUBFILE_ELEMENTS;
    hsize_t count = RANK_ELEMENTS;
// hsize_t max_size = H5S_UNLIMITED;	   /* dataset maximum dim size */
    hsize_t offset;
    hid_t fid   = -1;
    hid_t fs;   		/* File dataspace ID */
    hid_t ms;   		/* Memory dataspace ID */
    hid_t dataset   = -1;
    hid_t fapl   = -1;
    hid_t dcpl   = -1;
    hbool_t pass = true;
    float *data_slice = NULL;
    herr_t ret;

    HDassert(comm != MPI_COMM_NULL);
    status = MPI_Comm_rank(comm, &group_rank);
    VRFY((status == MPI_SUCCESS), "MPI_Comm_rank succeeded");
    status = MPI_Comm_size(comm, &group_size);
    VRFY((status == MPI_SUCCESS), "MPI_Comm_size succeeded");

    /* setup file access template */
    fapl = create_faccess_plist(comm, MPI_INFO_NULL, FACC_MPIO);
    VRFY((fapl >= 0), "create_faccess_plist succeeded");

    h5_fixname(FILENAMES[group_id], fapl, data_filename, sizeof data_filename);
    dset_name =DSETNAMES[group_id];
    /* -------------------
     * START AN HDF5 FILE
     * -------------------*/
    /* create the file collectively */
    fid = H5Fcreate(data_filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    VRFY((fid >= 0), "H5Fcreate succeeded");

    /* Release file-access template */
    ret = H5Pclose(fapl);
    VRFY((ret >= 0), "H5Pclose succeeded");

    /* --------------------------------------------------------------
     * Define the dimensions of the overall datasets and create them.
     * ------------------------------------------------------------- */

    /* set up dataset storage chunk sizes and creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((dcpl >= 0), "H5Pcreate succeeded");

    /* setup dimensionality object */
    /* File space is the global view */
    fs = H5Screate_simple (1, &group_count, NULL);
    VRFY((fs >= 0), "H5Screate_simple succeeded");

    /* create an extendible dataset collectively */
    dataset = H5Dcreate2(fid, dset_name, H5T_NATIVE_INT, fs, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dcreate2 succeeded");

    /* release resource */
    ret = H5Pclose(dcpl);
    VRFY((ret >= 0), "H5Pclose succeeded");

    /* -------------------------
     * Test writing to dataset
     * -------------------------*/
    /* create a memory dataspace independently */
    /* Memory space is a local view */
    ms = H5Screate_simple(1, &orig_size, NULL);
    VRFY((ms >= 0), "H5Screate_simple succeeded");
    offset = (hsize_t)((hsize_t)group_rank * orig_size);
    ret = H5Sselect_hyperslab(fs, H5S_SELECT_SET, &offset, NULL, &count, NULL);
    VRFY((ret >= 0), "H5Sselect_hyperslab succeeded");

    /* put some trivial (rank specific) data in the data_array */
    start_value = ((int)(orig_size) * g_rank);
    for(i = 0; i < (int)(orig_size); i++)
        written[i] = start_value + i;
    MESG("data array initialized");
    if(VERBOSE_MED) {
	MESG("writing at offset zero: ");
        for(i = 0; i < (int)orig_size; i++)
            printf("%s%d", i?", ":"", written[i]);
        printf("\n");
    }
    ret = H5Dwrite(dataset, H5T_NATIVE_INT, ms, fs, H5P_DEFAULT, written);
    VRFY((ret >= 0), "H5Dwrite succeeded");

    /* -------------------------
     * Read initial data from dataset.
     * -------------------------*/
    ret = H5Dread(dataset, H5T_NATIVE_INT, ms, fs, H5P_DEFAULT, retrieved);
    VRFY((ret >= 0), "H5Dread succeeded");
    for (i=0; i<(int)orig_size; i++)
        if(written[i]!=retrieved[i]) {
            printf("Line #%d: written!=retrieved: written[%d]=%d, retrieved[%d]=%d\n",__LINE__,
                i,written[i], i,retrieved[i]);
            nerrors++;
        }
    if(VERBOSE_MED){
	MESG("read at offset zero: ");
        for (i=0; i<(int)orig_size; i++)
            printf("%s%d", i?", ":"", retrieved[i]);
        printf("\n");
    }

    ret = H5Dclose(dataset);
    VRFY((ret >= 0), "H5Dclose succeeded");

    ret = H5Fclose(fid);
    VRFY((ret >= 0), "H5Fclose succeeded");

    /* collect results from other processes.
     * Only overwrite the failure message if no previous error
     * has been detected
     */
    local_failure = ( nerrors > 0 ? 1 : 0 );

    /* This is a global all reduce (NOT group specific) */
    if ( MPI_Allreduce(&local_failure, &global_failures, 1,
                       MPI_INT, MPI_SUM, MPI_COMM_WORLD) != MPI_SUCCESS ) {
        if ( pass ) {
            pass = FALSE;
            failure_mssg = "MPI_Allreduce() failed.\n";
        }
    } else if ( global_failures > 0 ) {
        pass = FALSE;
        failure_mssg = "One or more processes report failure.\n";
    }



    /* report results */
    if ( g_rank == 0 ) {
        if ( pass ) {
            HDfprintf(stdout, "Done.\n");
        } else {
            HDfprintf(stdout, "FAILED.\n");
            HDfprintf(stdout, "%s: failure_mssg = \"%s\"\n",
                      fcn_name, failure_mssg);
        }
    }

    /* free data_slice if it has been allocated */
    if ( data_slice != NULL ) {
        HDfree(data_slice);
        data_slice = NULL;
    }

    return(! pass);

} /* generate_test_file() */


/*-------------------------------------------------------------------------
 * Function:    generate_vds_container
 *
 * Purpose:     Create a parallel VDS container using the source files
 *              previously created in generate_test_files().
 *
 * Return:      Success: 0
 *
 *              Failure: 1
 *
 * Programmer:  Richard Warren
 *              10/1/17
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
generate_vds_container(MPI_Comm comm)
{
    hid_t        file, space, src_space, vspace, dset; /* Handles */ 
    hid_t        fapl;
    hid_t        dcpl;
    herr_t       status;
    hsize_t      vdsdims[2] = {VDSDIM0, VDSDIM1},      /* Virtual datasets dimension */
                 dims[1] = {DIM0},                     /* Source datasets dimensions */
                 start[2],                             /* Hyperslab parameters */
                 count[2],
                 block[2];
    hsize_t      start_out[2],
                 stride_out[2],
                 count_out[2],
                 block_out[2];
    int          expected = 0;
    int          rdata[VDSDIM0][VDSDIM1],    /* Read buffer for virtual dataset */
                 i, j, k, l;
    int          fill_value = -1;            /* Fill value for VDS */
    int          local_failure = 0;
    int          global_failures = 0;
    int          group_size;
    int          group_rank;
    int          n_groups;

    H5D_layout_t layout;                     /* Storage layout */
    size_t       num_map;                    /* Number of mappings */
    size_t       len;                        /* Length of the string; also a return value */
    char         *filename;
    char         *dsetname;
    hssize_t     nblocks;
    hsize_t      *buf;                       /* Buffer to hold hyperslab coordinates */

    HDassert(comm != MPI_COMM_NULL);
    status = MPI_Comm_rank(comm, &group_rank);
    VRFY((status == MPI_SUCCESS), "MPI_Comm_rank succeeded");
    status = MPI_Comm_size(comm, &group_size);
    VRFY((status == MPI_SUCCESS), "MPI_Comm_size succeeded");

    n_groups = group_size/2;

    /* setup file access template */
    fapl = create_faccess_plist(comm, MPI_INFO_NULL, FACC_MPIO);
    VRFY((fapl >= 0), "create_faccess_plist succeeded");

    /* Create file in which virtual dataset will be stored. */
    file = H5Fcreate (FILE, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    VRFY((file >= 0), "H5Fcreate succeeded");

    /* Create VDS dataspace.  */
    space = H5Screate_simple (RANK2, vdsdims, NULL);
    VRFY((space >= 0), "H5Screate_simple succeeded");

    /* Set VDS creation property. */
    dcpl = H5Pcreate (H5P_DATASET_CREATE);
    VRFY((dcpl >= 0), "H5Pcreate succeeded");

    status = H5Pset_fill_value (dcpl, H5T_NATIVE_INT, &fill_value);
    VRFY((status >= 0), "H5Pset_fill_value succeeded");

    /* Initialize hyperslab values. */
    start[0] = 0;
    start[1] = 0;
    count[0] = 1;
    count[1] = 1;
    block[0] = 1;
    block[1] = VDSDIM1;

    /*
     * Build the mappings.
     * Selections in the source datasets are H5S_ALL.
     * In the virtual dataset we select the first, the second and the third rows 
     * and map each row to the data in the corresponding source dataset. 
     */
    src_space = H5Screate_simple (RANK1, dims, NULL);
    VRFY((src_space >= 0), "H5Screate_simple succeeded");

    for (i = 0; i < n_groups; i++) {
        start[0] = (hsize_t)i;
        /* Select i-th row in the virtual dataset; selection in the source datasets is the same. */
        status = H5Sselect_hyperslab (space, H5S_SELECT_SET, start, NULL, count, block);
        VRFY((status >= 0), "H5Sselect_hyperslab succeeded");
        status = H5Pset_virtual (dcpl, space, SRC_FILE[i], SRC_DATASET[i], src_space);
        VRFY((status >= 0), "H5Pset_virtual succeeded");
    }
    /* Final virtual group (with only 1 writer) ONLY IF REQUIRED */
    if (group_size % 2) {
        start[0] = (hsize_t)i;
        status = H5Sselect_hyperslab (space, H5S_SELECT_SET, start, NULL, count, block);
        VRFY((status >= 0), "H5Sselect_hyperslab succeeded");
        status = H5Pset_virtual (dcpl, space, SRC_FILE[i], SRC_DATASET[i], src_space);
        VRFY((status >= 0), "H5Pset_virtual succeeded");
    }

    /* Create a virtual dataset. */
    dset = H5Dcreate2 (file, DATASET, H5T_NATIVE_INT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    VRFY((dset >= 0), "H5Dcreate2 succeeded");
    status = H5Sclose (space);
    VRFY((status >= 0), "H5Sclose succeeded");
    status = H5Sclose (src_space);
    VRFY((status >= 0), "H5Sclose succeeded");
    status = H5Dclose (dset);
    VRFY((status >= 0), "H5Dclose succeeded");
    status = H5Fclose (file);
    VRFY((status >= 0), "H5Fclose succeeded");

    /*
     * Now we begin the read section of this example.
     */

    /*
     * Open the file and virtual dataset.
     */
    file = H5Fopen (FILE, H5F_ACC_RDWR, fapl);
    VRFY((file >= 0), "H5Fopen succeeded");
    dset = H5Dopen2 (file, DATASET, H5P_DEFAULT);
    VRFY((dset >= 0), "H5Dopen2 succeeded");

    /*
     * Get creation property list and mapping properties.
     */
    dcpl = H5Dget_create_plist (dset);
    VRFY((dcpl >= 0), "H5Dget_create_plist succeeded");

    /*
     * Get storage layout.
     */
    layout = H5Pget_layout (dcpl);
    if (group_rank == 0) {
        if (H5D_VIRTUAL == layout)
            printf(" Dataset has a virtual layout \n");
        else
            printf(" Wrong layout found \n");
    }

     /*
      * Find the number of mappings.
      */
    status = H5Pget_virtual_count (dcpl, &num_map);
    VRFY((status >= 0), "H5Pget_virtual_count succeeded");
    if (group_rank == 0)
        printf(" Number of mappings is %lu\n", (unsigned long)num_map);

     /*
      * Get mapping parameters for each mapping.
      */
    for (i = 0; i < (int)num_map; i++) {
        if (group_rank == 0) {
            printf(" Mapping %d \n", i);
            printf("         Selection in the virtual dataset ");
	}
        /* Get selection in the virttual  dataset */
        vspace = H5Pget_virtual_vspace (dcpl, (size_t)i);
        VRFY((vspace >= 0), "H5Pget_virtual_vspace succeeded");
        /* Make sure that this is a hyperslab selection and then print information. */
        if (H5Sget_select_type(vspace) == H5S_SEL_HYPERSLABS) { 
            nblocks = H5Sget_select_hyper_nblocks (vspace);
            buf = (hsize_t *)malloc(sizeof(hsize_t)*2*RANK2*(hsize_t)nblocks);
            status = H5Sget_select_hyper_blocklist (vspace, (hsize_t)0, (hsize_t)nblocks, buf);
            if (group_rank == 0) {
                for (l=0; l<nblocks; l++) {
                    printf("(");
                    for (k=0; k<RANK2-1; k++)
	                printf("%d,", (int)buf[k]);
                    printf("%d ) - (", (int)buf[k]);
                    for (k=0; k<RANK2-1; k++) 
                        printf("%d,", (int)buf[RANK2+k]);
                    printf("%d)\n", (int)buf[RANK2+k]);
                }
            }
        /* We also can use new APIs to get start, stride, count and block */
            if (H5Sis_regular_hyperslab(vspace)) {
                status = H5Sget_regular_hyperslab (vspace, start_out, stride_out, count_out, block_out);
                if (group_rank == 0) {
                    printf("         start  = [%llu, %llu] \n", (unsigned long long)start_out[0], (unsigned long long)start_out[1]);
                    printf("         stride = [%llu, %llu] \n", (unsigned long long)stride_out[0], (unsigned long long)stride_out[1]);
                    printf("         count  = [%llu, %llu] \n", (unsigned long long)count_out[0], (unsigned long long)count_out[1]);
                    printf("         block  = [%llu, %llu] \n", (unsigned long long)block_out[0], (unsigned long long)block_out[1]);
                }
            }
        }
        /* Get source file name. */
        len = (size_t)H5Pget_virtual_filename (dcpl, (size_t)i, NULL, 0);
        filename = (char *)malloc(len*sizeof(char)+1);
        H5Pget_virtual_filename (dcpl, (size_t)i, filename, len+1);
        if (group_rank == 0)
            printf("         Source filename %s\n", filename);

        /* Get source dataset name. */
        len = (size_t)H5Pget_virtual_dsetname (dcpl, (size_t)i, NULL, 0);
        dsetname = (char *)malloc((size_t)len*sizeof(char)+1);
        H5Pget_virtual_dsetname (dcpl, (size_t)i, dsetname, len+1);
        if (group_rank == 0)
            printf("         Source dataset name %s\n", dsetname);

        /* Get selection in the source dataset. */
        if (group_rank == 0)
            printf("         Selection in the source dataset ");
        src_space = H5Pget_virtual_srcspace (dcpl, (size_t)i);

        /* Make sure it is ALL selection and then print the coordinates. */
        if(H5Sget_select_type(src_space) == H5S_SEL_ALL) {
            if (group_rank == 0)
                printf("(0) - (%d) \n", DIM0-1);
        }
        H5Sclose(vspace);
        H5Sclose(src_space);
        free(filename);
        free(dsetname);
        free(buf);
    }

    /*
     * Read the data using the default properties.
     */
    status = H5Dread (dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata[0]);
    VRFY((status >= 0), "H5Dread succeeded");
    /*
     * Output the data to the screen.
     */

    if (group_rank == 0) {
        for (i=0; i<VDSDIM0; i++) {
	  if (i < n_groups) {
              for (j=0; j<VDSDIM1; j++) {
                  if (rdata[i][j] != expected++)
                      local_failure++;
	      }
	  }
          else if (expected == fill_value) {
              for (j=0; j<VDSDIM1; j++) {
	          if (rdata[i][j] != expected)
                      local_failure++;
              }
	  }
	  else if (group_size % 2) {
              for (j=0; j<HALFGROUP; j++) {
	          if (rdata[i][j] != expected++)
                      local_failure++;
              }
              expected = fill_value;
          }
        }
    }

    /*
     * Close and release resources.
     */
    status = H5Pclose (dcpl);
    status = H5Dclose (dset);
    status = H5Fclose (file);

    /* collect results from other processes.
     * Only overwrite the failure message if no previous error
     * has been detected
     */

    status = MPI_Allreduce( &local_failure, &global_failures, 1,
			    MPI_INT, MPI_SUM, MPI_COMM_WORLD);

    VRFY((status == MPI_SUCCESS), "MPI_Allreduce succeeded");
    /* report results and finish cleanup */
    if ( group_rank == 0 ) {
        if ( global_failures == 0 ) {
            PASSED();
        } else {
            H5_FAILED();
        }
    }

    return global_failures;
}


int
main (int argc, char **argv)
{
    int nerrs = 0;
    int which_group = 0;
    MPI_Comm group_comm = MPI_COMM_NULL;

    if ( (MPI_Init(&argc, &argv)) != MPI_SUCCESS) {
       HDfprintf(stderr, "FATAL: Unable to initialize MPI\n");
       HDexit(EXIT_FAILURE);
    }

    if ( (MPI_Comm_rank(MPI_COMM_WORLD, &mpi_global_rank)) != MPI_SUCCESS) {
        HDfprintf(stderr, "FATAL: MPI_Comm_rank returned an error\n");
        HDexit(EXIT_FAILURE);
    }

    if ( (MPI_Comm_size(MPI_COMM_WORLD, &mpi_global_size)) != MPI_SUCCESS) {
        HDfprintf(stderr, "FATAL: MPI_Comm_size returned an error\n");
        HDexit(EXIT_FAILURE);
    }

    H5open();

    mpi_size = mpi_global_size;
    if ((mpi_size < 4) || (mpi_size > 6)) {
        nerrs++;

        if ( mpi_global_rank == 0 ) {
            HDprintf("MPI size = %d, need at least 4 processes, max = 6.  Exiting.\n", mpi_size);
        }
        goto finish;
    }

    if ((mpi_rank = mpi_global_rank) == 0 ) {
        HDfprintf(stdout, "============================================\n");
        HDfprintf(stdout, "Subfiling functionality (parallel VDS) tests\n");
        HDfprintf(stdout, "        mpi_size     = %d\n", mpi_size);
        HDfprintf(stdout, "============================================\n");
    }


    /* ------  Create MPI groups of 2 ------
     *
     * We split MPI_COMM_WORLD into n groups of size 2.
     * The resulting communicators will be used to generate
     * HDF dataset files which in turn will be opened in parallel and the
     * contents verified in the second read test below.
     */

    which_group = mpi_rank / 2;

    if ( (MPI_Comm_split(MPI_COMM_WORLD,
                         which_group,
                         0,
                         &group_comm)) != MPI_SUCCESS) {

        HDfprintf(stderr, "FATAL: MPI_Comm_split returned an error\n");
        HDexit(EXIT_FAILURE);
    }

    /* ------  Generate all source files ------ */
    nerrs += generate_test_files( group_comm, mpi_rank, which_group );

    if ( nerrs > 0 ) {
        if ( mpi_rank == 0 ) {
            HDprintf("    SubFile construction failed -- skipping tests.\n");
        }
        goto finish;
    }

    /* We generate a containing VDS file and read the data 
     * from the multiple containers produced in 'generate_test_files'.
     */
    nerrs += generate_vds_container( MPI_COMM_WORLD );

    if ( nerrs > 0 ) {
        if ( mpi_rank == 0 ) {
            HDprintf("    VDS file construction failed -- skipping tests.\n");
        }
        goto finish;
    }
finish:

    if ((group_comm != MPI_COMM_NULL) &&
        (MPI_Comm_free(&group_comm)) != MPI_SUCCESS) {
        HDfprintf(stderr, "MPI_Comm_free failed!\n");
    }

    /* make sure all processes are finished before final report, cleanup
     * and exit.
     */
    MPI_Barrier(MPI_COMM_WORLD);

    if ( mpi_rank == 0 ) {           /* only process 0 reports */
        int i;
        const char *header = "Subfiling validation tests";

        HDfprintf(stdout, "===================================\n");
        if ( nerrs > 0 ) {
            HDfprintf(stdout, "***%s detected %d failures***\n", header, nerrs);
        }
        else {
            HDfprintf(stdout, "%s finished with no failures\n", header);
        }
        HDfprintf(stdout, "===================================\n");

        for(i=0; i<NFILENAMES; i++) {
            HDremove(SRC_FILE[i]);
        }

    }

    /* close HDF5 library */
    if (H5close() != SUCCEED) {
        HDfprintf(stdout, "H5close() failed. (Ignoring)\n");
    }

    /* MPI_Finalize must be called AFTER H5close which may use MPI calls */
    MPI_Finalize();

    /* cannot just return (nerrs) because exit code is limited to 1byte */
    return((nerrs > 0) ? EXIT_FAILURE : EXIT_SUCCESS );
}
