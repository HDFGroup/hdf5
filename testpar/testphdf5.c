/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* $Id$ */

/*
 * Main driver of the Parallel HDF5 tests
 */

#include "testphdf5.h"

#ifndef PATH_MAX
#define PATH_MAX    512
#endif  /* !PATH_MAX */

/* global variables */
int dim0 = DIM0;
int dim1 = DIM1;
int chunkdim0;
int chunkdim1;
int nerrors = 0;			/* errors count */
int ndatasets = 300;			/* number of datasets to create*/
int ngroups = 512;                      /* number of groups to create in root
                                         * group. */
int facc_type = FACC_MPIO;		/*Test file access type */

H5E_auto_t old_func;		        /* previous error handler */
void *old_client_data;			/* previous error handler arg.*/

/* other option flags */
int doread=1;				/* read test */
int dowrite=1;				/* write test */
int docompact=1;                        /* compact dataset test */
int doindependent=1;			/* independent test */
unsigned dobig=0;                       /* "big" dataset tests */

/* FILENAME and filenames must have the same number of names */
const char *FILENAME[10]={
	    "ParaEg1",
	    "ParaEg2",
	    "ParaEg3",
	    "ParaMdset",
            "ParaMgroup",
            "ParaCompact",
            "ParaIndividual",
            "ParaBig",
            "ParaFill",
	    NULL};
char	filenames[10][PATH_MAX];
hid_t	fapl;				/* file access property list */

#ifdef USE_PAUSE
/* pause the process for a moment to allow debugger to attach if desired. */
/* Will pause more if greenlight file is not persent but will eventually */
/* continue. */
#include <sys/types.h>
#include <sys/stat.h>

void pause_proc(void)
{

    int pid;
    h5_stat_t	statbuf;
    char greenlight[] = "go";
    int maxloop = 10;
    int loops = 0;
    int time_int = 10;

    /* mpi variables */
    int  mpi_size, mpi_rank;
    int  mpi_namelen;		
    char mpi_name[MPI_MAX_PROCESSOR_NAME];

    pid = getpid();
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Get_processor_name(mpi_name, &mpi_namelen);

    if (MAINPROCESS)
	while ((stat(greenlight, &statbuf) == -1) && loops < maxloop){
	    if (!loops++){
		printf("Proc %d (%*s, %d): to debug, attach %d\n",
		    mpi_rank, mpi_namelen, mpi_name, pid, pid);
	    }
	    printf("waiting(%ds) for file %s ...\n", time_int, greenlight);
	    fflush(stdout);
	    sleep(time_int);
	}
    MPI_Barrier(MPI_COMM_WORLD);
}

/* Use the Profile feature of MPI to call the pause_proc() */
int MPI_Init(int *argc, char ***argv)
{
    int ret_code;
    ret_code=PMPI_Init(argc, argv);
    pause_proc();
    return (ret_code);
}
#endif	/* USE_PAUSE */


/*
 * Show command usage
 */
static void
usage(void)
{
    printf("Usage: testphdf5 [-r] [-w] [-v<verbosity>] [-m<n_datasets>] [-n<n_groups>] "
	"[-o] [-f <prefix>] [-d <dim0> <dim1>]\n");
    printf("\t-r\t\tno read test\n");
    printf("\t-w\t\tno write test\n");
    printf("\t-m<n_datasets>"
	"\tset number of datasets for the multiple dataset test\n");
    printf("\t-n<n_groups>"
        "\tset number of groups for the multiple group test\n");  
    printf("\t-o\t\tno compact dataset test\n");
    printf("\t-i\t\tno independent read test\n");
    printf("\t-b\t\trun big dataset test\n");
    printf("\t-v<verbosity>\tset verbose level (0-9,l,m,h)\n");
    printf("\t-f <prefix>\tfilename prefix\n");
    printf("\t-s\t\tuse Split-file together with MPIO\n");
    printf("\t-p\t\tuse combo MPI-POSIX driver\n");
    printf("\t-d <dim0> <dim1>\tdataset dimensions\n");
    printf("\t-c <dim0> <dim1>\tdataset chunk dimensions\n");
    printf("\tDefault: do write then read with dimensions %dx%d\n",
	DIM0, DIM1);
    printf("\n");
}


/*
 * parse the command line options
 */
static int
parse_options(int argc, char **argv)
{
    int mpi_size, mpi_rank;				/* mpi variables */

    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    /* setup default chunk-size. Make sure sizes are > 0 */
    chunkdim0 = (dim0+9)/10;
    chunkdim1 = (dim1+9)/10;

    while (--argc){
	if (**(++argv) != '-'){
	    break;
	}else{
	    switch(*(*argv+1)){
		case 'r':   doread = 0;
			    break;
		case 'w':   dowrite = 0;
			    break;
		case 'm':   ndatasets = atoi((*argv+1)+1);
			    if (ndatasets < 0){
				nerrors++;
				return(1);
			    }
			    break;
	        case 'n':   ngroups = atoi((*argv+1)+1);
		            if (ngroups < 0){
                                nerrors++;
                                return(1);
			    }
                            break;
                case 'o':   docompact = 0;
                            break;
                case 'i':   doindependent = 0;
                            break;
                case 'b':   dobig = 1;
                            break;
		case 'v':   if (*((*argv+1)+1))
				ParseTestVerbosity((*argv+1)+1);
			    else
				SetTestVerbosity(VERBO_MED);
			    break;
		case 'f':   if (--argc < 1) {
				nerrors++;
				return(1);
			    }
			    if (**(++argv) == '-') {
				nerrors++;
				return(1);
			    }
			    paraprefix = *argv;
			    break;
		case 'p':   /* Use the MPI-POSIX driver access */
			    facc_type = FACC_MPIPOSIX;
			    break;
		case 's':   /* Use the split-file driver with MPIO access */
			    /* Can use $HDF5_METAPREFIX to define the */
			    /* meta-file-prefix. */
			    facc_type = FACC_MPIO | FACC_SPLIT;
			    break;
		case 'd':   /* dimensizes */
			    if (--argc < 2){
				nerrors++;
				return(1);
			    }
			    dim0 = atoi(*(++argv));
			    argc--;
			    dim1 = atoi(*(++argv));
			    /* set default chunkdim sizes too */
			    chunkdim0 = (dim0+9)/10;
			    chunkdim1 = (dim1+9)/10;
			    break;
		case 'c':   /* chunk dimensions */
			    if (--argc < 2){
				nerrors++;
				return(1);
			    }
			    chunkdim0 = atoi(*(++argv));
			    argc--;
			    chunkdim1 = atoi(*(++argv));
			    break;
		case 'h':   /* print help message--return with nerrors set */
			    return(1);
		default:    nerrors++;
			    return(1);
	    }
	}
    } /*while*/

    /* check validity of dimension and chunk sizes */
    if (dim0 <= 0 || dim1 <= 0){
	printf("Illegal dim sizes (%d, %d)\n", dim0, dim1);
	nerrors++;
	return(1);
    }
    if (chunkdim0 <= 0 || chunkdim1 <= 0){
	printf("Illegal chunkdim sizes (%d, %d)\n", chunkdim0, chunkdim1);
	nerrors++;
	return(1);
    }

    /* Make sure datasets can be divided into equal portions by the processes */
    if ((dim0 % mpi_size) || (dim1 % mpi_size)){
	if (MAINPROCESS)
	    printf("dim0(%d) and dim1(%d) must be multiples of processes(%d)\n",
		    dim0, dim1, mpi_size);
	nerrors++;
	return(1);
    }

    /* compose the test filenames */
    {
	int i, n;

	n = sizeof(FILENAME)/sizeof(FILENAME[0]) - 1;	/* exclude the NULL */

	for (i=0; i < n; i++)
	    if (h5_fixname(FILENAME[i],fapl,filenames[i],sizeof(filenames[i]))
		== NULL){
		printf("h5_fixname failed\n");
		nerrors++;
		return(1);
	    }
	printf("Test filenames are:\n");
	for (i=0; i < n; i++)
	    printf("    %s\n", filenames[i]);
    }

    return(0);
}


/*
 * Create the appropriate File access property list
 */
hid_t
create_faccess_plist(MPI_Comm comm, MPI_Info info, int l_facc_type,
                     hbool_t use_gpfs)
{
    hid_t ret_pl = -1;
    herr_t ret;                 /* generic return value */
    int mpi_rank;		/* mpi variables */

    /* need the rank for error checking macros */
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    ret_pl = H5Pcreate (H5P_FILE_ACCESS);
    VRFY((ret_pl >= 0), "H5P_FILE_ACCESS");

    if (l_facc_type == FACC_DEFAULT)
	return (ret_pl);

    if (l_facc_type == FACC_MPIO){
	/* set Parallel access with communicator */
	ret = H5Pset_fapl_mpio(ret_pl, comm, info);     
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

    if (l_facc_type == FACC_MPIPOSIX) {
	/* set Parallel access with communicator */
#ifdef H5_WANT_H5_V1_4_COMPAT
	ret = H5Pset_fapl_mpiposix(ret_pl, comm);
#else /* H5_WANT_H5_V1_4_COMPAT */
	ret = H5Pset_fapl_mpiposix(ret_pl, comm, use_gpfs);
#endif /* H5_WANT_H5_V1_4_COMPAT */
	VRFY((ret >= 0), "H5Pset_fapl_mpiposix succeeded");
	return(ret_pl);
    }

    /* unknown file access types */
    return (ret_pl);
}

/*
 * Check the size of a file using MPI routines
 */
MPI_Offset
h5_mpi_get_file_size(const char *filename, MPI_Comm comm, MPI_Info info)
{
    MPI_File	fh;             /* MPI file handle */
    MPI_Offset	size=0;         /* File size to return */

    if (MPI_SUCCESS != MPI_File_open(comm, (char*)filename, MPI_MODE_RDONLY, info, &fh))
        goto done;

    if (MPI_SUCCESS != (MPI_File_get_size(fh, &size)))
        goto done;

    if (MPI_SUCCESS != MPI_File_close(&fh))
        size=0;

done:
    return(size);
}

int main(int argc, char **argv)
{
    int mpi_size, mpi_rank;				/* mpi variables */

    /* Un-buffer the stdout and stderr */
    setbuf(stderr, NULL);
    setbuf(stdout, NULL);

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    if (MAINPROCESS){
	printf("===================================\n");
	printf("PHDF5 TESTS START\n");
	printf("===================================\n");
    }
    H5open();
    h5_show_hostname();

    fapl = H5Pcreate (H5P_FILE_ACCESS);
    H5Pset_fapl_mpio(fapl, MPI_COMM_WORLD, MPI_INFO_NULL);

    if (parse_options(argc, argv) != 0){
	if (MAINPROCESS)
	    usage();
	goto finish;
    }

    if (facc_type == FACC_MPIPOSIX && MAINPROCESS){
	printf("===================================\n"
	       "   Using MPIPOSIX driver\n"
	       "===================================\n");
    }

    MPI_BANNER("test_fapl_mpio_dup...");
    test_fapl_mpio_dup();

    MPI_BANNER("test_fapl_mpiposix_dup...");
    test_fapl_mpiposix_dup();

    if (ndatasets){
	MPI_BANNER("multiple datasets write ...");
        multiple_dset_write(filenames[3], ndatasets);
    }
    else{
        MPI_BANNER("Multiple datasets test skipped");
    }

    if (ngroups){
	MPI_BANNER("multiple groups write ...");
        multiple_group_write(filenames[4], ngroups);
        if (doread) {
       	    MPI_BANNER("multiple groups read ...");
            multiple_group_read(filenames[4], ngroups);
        }
    }
    else{
        MPI_BANNER("Multiple groups test skipped");
    }

    if (dowrite){
	MPI_BANNER("dataset using split communicators...");
	test_split_comm_access(filenames[0]);

	MPI_BANNER("dataset independent write...");
	dataset_writeInd(filenames[0]);

	MPI_BANNER("dataset collective write...");
	dataset_writeAll(filenames[1]);

	MPI_BANNER("extendible dataset independent write...");
	extend_writeInd(filenames[2]);

	MPI_BANNER("extendible dataset collective write...");
	extend_writeAll(filenames[2]);
    }
    else{
	MPI_BANNER("write tests skipped");
    }

    if (doread){
	MPI_BANNER("dataset independent read...");
	dataset_readInd(filenames[0]);

	MPI_BANNER("dataset collective read...");
	dataset_readAll(filenames[1]);

	MPI_BANNER("extendible dataset independent read...");
	extend_readInd(filenames[2]);

	MPI_BANNER("extendible dataset collective read...");
	extend_readAll(filenames[2]);
    }
    else{
	MPI_BANNER("read tests skipped");
    }

    if (docompact){
        MPI_BANNER("compact dataset test...");
        compact_dataset(filenames[5]); 
    }
    else {
        MPI_BANNER("compact dataset test skipped");
    }
    
    if (doindependent){
	MPI_BANNER("collective group and dataset write ...");
        collective_group_write(filenames[6], ngroups);
        if (doread) {
       	    MPI_BANNER("independent group and dataset read ...");
            independent_group_read(filenames[6], ngroups);
        }
    }
    else{
        MPI_BANNER("Independent test skipped");
    }
        
    if (dobig && sizeof(MPI_Offset)>4){
        MPI_BANNER("big dataset test...");
        big_dataset(filenames[7]); 
    }
    else {
        MPI_BANNER("big dataset test skipped");
    }
    
    MPI_BANNER("dataset fill value test...");
    dataset_fillvalue(filenames[8]); 
    
    if (!(dowrite || doread || ndatasets || ngroups || docompact || doindependent || dobig )){
	usage();
	nerrors++;
    }

finish:
    /* make sure all processes are finished before final report, cleanup
     * and exit.
     */
    MPI_Barrier(MPI_COMM_WORLD);
    if (MAINPROCESS){		/* only process 0 reports */
	printf("===================================\n");
	if (nerrors){
	    printf("***PHDF5 tests detected %d errors***\n", nerrors);
	}
	else{
	    printf("PHDF5 tests finished with no errors\n");
	}
	printf("===================================\n");
    }
    if (dowrite){
	h5_cleanup(FILENAME, fapl);
    } else {
	/* h5_cleanup would have closed fapl.  Now must do it explicitedly */
	H5Pclose(fapl);
    }

    /* close HDF5 library */
    H5close();

    /* MPI_Finalize must be called AFTER H5close which may use MPI calls */
    MPI_Finalize();
    return(nerrors);
}

