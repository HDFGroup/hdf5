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

/* FILENAME and filenames must have the same number of names.
 * Use PARATESTFILE in general and use a separated filename only if the file
 * created in one test is accessed by a different test.
 * filenames[0] is reserved as the file name for PARATESTFILE.
 */
#define NFILENAME 2
#define PARATESTFILE filenames[0]
const char *FILENAME[NFILENAME]={
	    "ParaTest",
	    NULL};
char	filenames[NFILENAME][PATH_MAX];
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
    printf("    [-r] [-w] [-m<n_datasets>] [-n<n_groups>] "
	"[-o] [-f <prefix>] [-d <dim0> <dim1>]\n");
    printf("\t-m<n_datasets>"
	"\tset number of datasets for the multiple dataset test\n");
    printf("\t-n<n_groups>"
        "\tset number of groups for the multiple group test\n");
    printf("\t-f <prefix>\tfilename prefix\n");
    printf("\t-2\t\tuse Split-file together with MPIO\n");
    printf("\t-p\t\tuse combo MPI-POSIX driver\n");
    printf("\t-d <dim0> <dim1>\tdataset dimensions. Defaults (%d,%d)\n",
	DIM0, DIM1);
    printf("\t-c <dim0> <dim1>\tdataset chunk dimensions. Defaults (dim0/10,dim1/10)\n");
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
		case '2':   /* Use the split-file driver with MPIO access */
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
		default:    printf("Illegal option(%s)\n", *argv);
			    nerrors++;
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
	ret = H5Pset_fapl_mpiposix(ret_pl, comm, use_gpfs);
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
    H5Ptest_param_t ndsets_params, ngroups_params;
    H5Ptest_param_t collngroups_params;
    H5Ptest_param_t io_mode_confusion_params;

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

    /* Initialize testing framework */
    TestInit(argv[0], usage, parse_options);

    /* Tests are generally arranged from least to most complexity... */
    AddTest("mpiodup", test_fapl_mpio_dup, NULL,
	    "fapl_mpio duplicate", NULL);
    AddTest("posixdup", test_fapl_mpiposix_dup, NULL,
	    "fapl_mpiposix duplicate", NULL);

    AddTest("split", test_split_comm_access, NULL,
	    "dataset using split communicators", PARATESTFILE);

    AddTest("idsetw", dataset_writeInd, NULL,
	    "dataset independent write", PARATESTFILE);
    AddTest("idsetr", dataset_readInd, NULL,
	    "dataset independent read", PARATESTFILE);

    AddTest("cdsetw", dataset_writeAll, NULL,
	    "dataset collective write", PARATESTFILE);
    AddTest("cdsetr", dataset_readAll, NULL,
	    "dataset collective read", PARATESTFILE);

    AddTest("eidsetw", extend_writeInd, NULL,
	    "extendible dataset independent write", PARATESTFILE);
    AddTest("eidsetr", extend_readInd, NULL,
	    "extendible dataset independent read", PARATESTFILE);
    AddTest("ecdsetw", extend_writeAll, NULL,
	    "extendible dataset collective write", PARATESTFILE);
    AddTest("ecdsetr", extend_readAll, NULL,
	    "extendible dataset collective read", PARATESTFILE);
    AddTest("eidsetw2", extend_writeInd2, NULL,
	    "extendible dataset independent write #2", PARATESTFILE);
    AddTest("calloc", test_chunk_alloc, NULL,
	    "parallel extend Chunked allocation on serial file", PARATESTFILE);

#ifdef H5_HAVE_FILTER_DEFLATE
    AddTest("cmpdsetr", compress_readAll, NULL,
	    "compressed dataset collective read", PARATESTFILE);
#endif /* H5_HAVE_FILTER_DEFLATE */

    ndsets_params.name = PARATESTFILE;
    ndsets_params.count = ndatasets;
    AddTest("ndsetw", multiple_dset_write, NULL,
	    "multiple datasets write", &ndsets_params);

    ngroups_params.name = PARATESTFILE;
    ngroups_params.count = ngroups;
    AddTest("ngrpw", multiple_group_write, NULL,
	    "multiple groups write", &ngroups_params);
    AddTest("ngrpr", multiple_group_read, NULL,
	    "multiple groups read", &ngroups_params);

    AddTest("compact", compact_dataset, NULL,
	    "compact dataset test", PARATESTFILE);

    collngroups_params.name = PARATESTFILE;
    collngroups_params.count = ngroups;
    AddTest("cngrpw", collective_group_write, NULL,
	    "collective group and dataset write", &collngroups_params);
    AddTest("ingrpr", independent_group_read, NULL,
	    "independent group and dataset read", &collngroups_params);
 /* By default, do not run big dataset on WIN32. */
#ifdef WIN32
    AddTest("-bigdset", big_dataset, NULL,
            "big dataset test", PARATESTFILE);
#else
				 AddTest("bigdset", big_dataset, NULL,
            "big dataset test", PARATESTFILE);
#endif
    AddTest("fill", dataset_fillvalue, NULL,
	    "dataset fill value", PARATESTFILE);

    AddTest("cchunk1",
	coll_chunk1,NULL, "simple collective chunk io",PARATESTFILE);
    AddTest("cchunk2",
	coll_chunk2,NULL, "noncontiguous collective chunk io",PARATESTFILE);
    AddTest("cchunk3",
	coll_chunk3,NULL, "multi-chunk collective chunk io",PARATESTFILE);
    AddTest("cchunk4",
        coll_chunk4,NULL, "collective chunk io with partial non-selection ",PARATESTFILE);

    if((mpi_size < 3)&& MAINPROCESS ) {
	printf("Collective chunk IO optimization APIs ");
	printf("needs at least 3 processes to participate\n");
	printf("Collective chunk IO API tests will be skipped \n");
    }
    AddTest((mpi_size <3)? "-cchunk5":"cchunk5" ,
        coll_chunk5,NULL,
	"linked chunk collective IO without optimization",PARATESTFILE);
    AddTest((mpi_size < 3)? "-cchunk6" : "cchunk6",
	coll_chunk6,NULL,
	"multi-chunk collective IO without optimization",PARATESTFILE);
    AddTest((mpi_size < 3)? "-cchunk7" : "cchunk7",
	coll_chunk7,NULL,
	"linked chunk collective IO with optimization",PARATESTFILE);
    AddTest((mpi_size < 3)? "-cchunk8" : "cchunk8",
	coll_chunk8,NULL,
	"linked chunk collective IO transferring to multi-chunk",PARATESTFILE);
    AddTest((mpi_size < 3)? "-cchunk9" : "cchunk9",
	coll_chunk9,NULL,
	"multiple chunk collective IO with optimization",PARATESTFILE);
    AddTest((mpi_size < 3)? "-cchunk10" : "cchunk10",
	coll_chunk10,NULL,
	"multiple chunk collective IO transferring to independent IO",PARATESTFILE);



/* irregular collective IO tests*/
    AddTest("ccontw",
	coll_irregular_cont_write,NULL,
	"collective irregular contiguous write",PARATESTFILE);
    AddTest("ccontr",
	coll_irregular_cont_read,NULL,
	"collective irregular contiguous read",PARATESTFILE);
    AddTest("cschunkw",
	coll_irregular_simple_chunk_write,NULL,
	"collective irregular simple chunk write",PARATESTFILE);
    AddTest("cschunkr",
	coll_irregular_simple_chunk_read,NULL,
	"collective irregular simple chunk read",PARATESTFILE);
    AddTest("ccchunkw",
	coll_irregular_complex_chunk_write,NULL,
	"collective irregular complex chunk write",PARATESTFILE);
    AddTest("ccchunkr",
	coll_irregular_complex_chunk_read,NULL,
	"collective irregular complex chunk read",PARATESTFILE);


#if 0
    if((mpi_size > 3) && MAINPROCESS) {
	printf("Collective irregular chunk IO tests haven't been tested \n");
	printf("  for the number of process greater than 3.\n");
	printf("Please try with the number of process \n");
	printf("  no greater than 3 for collective irregular chunk IO test.\n");
	printf("Collective irregular chunk tests will be skipped \n");
    }
    AddTest((mpi_size > 3) ? "-ccontw" : "ccontw",
	coll_irregular_cont_write,NULL,
	"collective irregular contiguous write",PARATESTFILE);
    AddTest((mpi_size > 3) ? "-ccontr" : "ccontr",
	coll_irregular_cont_read,NULL,
	"collective irregular contiguous read",PARATESTFILE);
    AddTest((mpi_size > 3) ? "-cschunkw" : "cschunkw",
	coll_irregular_simple_chunk_write,NULL,
	"collective irregular simple chunk write",PARATESTFILE);
    AddTest((mpi_size > 3) ? "-cschunkr" : "cschunkr",
	coll_irregular_simple_chunk_read,NULL,
	"collective irregular simple chunk read",PARATESTFILE);
    AddTest((mpi_size > 3) ? "-ccchunkw" : "ccchunkw",
	coll_irregular_complex_chunk_write,NULL,
	"collective irregular complex chunk write",PARATESTFILE);
    AddTest((mpi_size > 3) ? "-ccchunkr" : "ccchunkr",
	coll_irregular_complex_chunk_read,NULL,
	"collective irregular complex chunk read",PARATESTFILE);
#endif


    AddTest("null", null_dataset, NULL,
	    "null dataset test", PARATESTFILE);

    io_mode_confusion_params.name  = PARATESTFILE;
    io_mode_confusion_params.count = 0; /* value not used */

    AddTest("I/Omodeconf", io_mode_confusion, NULL,
	    "I/O mode confusion test -- hangs quickly on failure",
            &io_mode_confusion_params);

    /* Display testing information */
    TestInfo(argv[0]);

    /* setup file access property list */
    fapl = H5Pcreate (H5P_FILE_ACCESS);
    H5Pset_fapl_mpio(fapl, MPI_COMM_WORLD, MPI_INFO_NULL);

    /* Parse command line arguments */
    TestParseCmdLine(argc, argv);

    if (facc_type == FACC_MPIPOSIX && MAINPROCESS){
	printf("===================================\n"
	       "   Using MPIPOSIX driver\n"
	       "===================================\n");
    }

    /* Perform requested testing */
    PerformTests();

    /* make sure all processes are finished before final report, cleanup
     * and exit.
     */
    MPI_Barrier(MPI_COMM_WORLD);

    /* Display test summary, if requested */
    if (MAINPROCESS && GetTestSummary())
        TestSummary();

    /* Clean up test files */
    h5_cleanup(FILENAME, fapl);

    nerrors += GetTestNumErrs();

    /* Gather errors from all processes */
    {
        int temp;
        MPI_Allreduce(&nerrors, &temp, 1, MPI_INT, MPI_MAX, MPI_COMM_WORLD);
	nerrors=temp;
    }

    if (MAINPROCESS){		/* only process 0 reports */
	printf("===================================\n");
	if (nerrors)
	    printf("***PHDF5 tests detected %d errors***\n", nerrors);
	else
	    printf("PHDF5 tests finished with no errors\n");
	printf("===================================\n");
    }
    /* close HDF5 library */
    H5close();

    /* MPI_Finalize must be called AFTER H5close which may use MPI calls */
    MPI_Finalize();

    /* cannot just return (nerrors) because exit code is limited to 1byte */
    return(nerrors!=0);
}

