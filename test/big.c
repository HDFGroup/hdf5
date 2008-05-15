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

/*
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Wednesday, April  8, 1998
 */
#include "h5test.h"

const char *FILENAME[] = {
    "big",
    "sec2",
    "stdio",
    NULL
};

#define DNAME		"big.data"

#define WRT_N		50
#define WRT_SIZE	4*1024
#define FAMILY_SIZE	1024*1024*1024

/* Define big file as 2GB */
#define BIG_FILE 0x80000000UL
 
#define MAX_TRIES       100

#if H5_SIZEOF_LONG_LONG >= 8
#   define GB8LL	((unsigned long_long)8*1024*1024*1024)
#else
#   define GB8LL	0	/*cannot do the test*/
#endif

/* Protocols */
static void usage(void);

static hsize_t values_used[WRT_N];

/*-------------------------------------------------------------------------
 * Function:	randll
 *
 * Purpose:	Create a random long_long value.
 *
 * Return:	Success:	Random value
 *
 *		Failure:	never fails
 *
 * Programmer:	Robb Matzke
 *              Tuesday, November 24, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hsize_t
randll(hsize_t limit, int current_index)
{
    hsize_t     acc;
    int         overlap = 1;
    int         i;
    int         tries = 0;

    /* Generate up to MAX_TRIES random numbers until one of them */
    /* does not overlap with any previous writes */
    while(overlap != 0 && tries < MAX_TRIES)
    {
        acc = rand ();
        acc *= rand ();
        acc = acc % limit;
        overlap = 0;

        for(i = 0; i < current_index; i++)
        {
            if((acc >= values_used[i]) && (acc < values_used[i]+WRT_SIZE))
                overlap = 1;
            if((acc+WRT_SIZE >= values_used[i]) && (acc+WRT_SIZE < values_used[i
]+WRT_SIZE))
                overlap = 1;
        }
        tries++;
    }

    values_used[current_index]=acc;

    return acc;
}


/*-------------------------------------------------------------------------
 * Function:	is_sparse
 *
 * Purpose:	Determines if the file system of the current working
 *		directory supports holes.
 *
 * Return:	Success:	Non-zero if holes are supported; zero
 *				otherwise.
 *
 *		Failure:	zero
 *
 * Programmer:	Robb Matzke
 *              Wednesday, July 15, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
is_sparse(void)
{
    int		fd;
    h5_stat_t	sb;

    if ((fd=HDopen("x.h5", O_RDWR|O_TRUNC|O_CREAT, 0666))<0) return 0;
    if (HDlseek(fd, (off_t)(1024*1024), SEEK_SET)!=1024*1024) return 0;
    if (5!=HDwrite(fd, "hello", (size_t)5)) return 0;
    if (HDclose(fd)<0) return 0;
    if (HDstat("x.h5", &sb)<0) return 0;
    if (HDunlink("x.h5")<0) return 0;
#ifdef H5_HAVE_STAT_ST_BLOCKS
    return ((unsigned long)sb.st_blocks*512 < (unsigned long)sb.st_size);
#else
    return (0);
#endif
}


/*-------------------------------------------------------------------------
 * Function:	supports_big
 *
 * Purpose:	Determines if the file system of the current working
 *		directory supports big files.
 *
 * Return:	Success:	Non-zero if big files are supported; zero
 *				otherwise.
 *
 *		Failure:	zero
 *
 * Programmer:	Raymond Lu
 *              Wednesday, April 18, 2007
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
supports_big(void)
{
    int		fd;

    if ((fd=HDopen("y.h5", O_RDWR|O_TRUNC|O_CREAT, 0666))<0) return 0;

    /* Write a few bytes at 2GB */
    if (HDlseek(fd, BIG_FILE, SEEK_SET)!=BIG_FILE) return 0;
    if (5!=HDwrite(fd, "hello", (size_t)5)) return 0;

    /* Write a few bytes at 4GB */
    if (HDlseek(fd, 2*BIG_FILE, SEEK_SET) != 2*BIG_FILE) return 0;
    if (5!=HDwrite(fd, "hello", (size_t)5)) return 0;

    if (HDclose(fd)<0) return 0;
    if (HDunlink("y.h5")<0) return 0;

    return (1);
}



/*-------------------------------------------------------------------------
 * Function:	enough_room
 *
 * Purpose:	Tries to create a bunch of sparse files to see if quotas will
 *		get in the way.  Some systems also have problems opening
 *		enough files and we'll check that too.
 *
 * Return:	Success:	Non-zero
 *
 *		Failure:	zero
 *
 * Programmer:	Robb Matzke
 *              Thursday, August  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
enough_room(hid_t fapl)
{
    int		ret_value=0;
    int		fd[68];
    size_t	i, size = (size_t)1 << 30;
    char	filename[1024], name[1024];

    /* Initialize file descriptors */
    for (i=0; i<NELMTS(fd); i++) fd[i] = -1;

    /* Get file name template */
    assert(H5FD_FAMILY==H5Pget_driver(fapl));
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create files */
    for (i=0; i<NELMTS(fd); i++) {
	HDsnprintf(name, sizeof name, filename, i);
	if ((fd[i]=HDopen(name, O_RDWR|O_CREAT|O_TRUNC, 0666))<0) {
	    goto done;
	}
	if ((off_t)size != HDlseek(fd[i], (off_t)size, SEEK_SET)) {
	    goto done;
	}
	if (1!=HDwrite(fd[i], "X", (size_t)1)) {
	    goto done;
	}
    }
    ret_value = 1;

 done:
    for (i=0; i<NELMTS(fd) && fd[i]>=0; i++) {
	HDsnprintf(name, sizeof name, filename, i);
	if(HDclose(fd[i])<0)
            ret_value=0;
	HDunlink(name);
    }

    return ret_value;
}


/*-------------------------------------------------------------------------
 * Function:	writer
 *
 * Purpose:	Creates a *big* dataset.
 *
 * Return:	Success:	0
 *
 *		Failure:	>0
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April  8, 1998
 *
 * Modifications:
 * 	Robb Matzke, 15 Jul 1998
 *	Addresses are written to the file DNAME instead of stdout.
 *
 *-------------------------------------------------------------------------
 */
static int
writer(char* filename, hid_t fapl, int wrt_n)
{
    hsize_t	size1[4] = {8, 1024, 1024, 1024};
    hsize_t	size2[1] = {GB8LL};
    hsize_t	hs_start[1];
    hsize_t	hs_size[1];
    hid_t	file=-1, space1=-1, space2=-1, mem_space=-1, d1=-1, d2=-1;
    int		*buf = (int*)malloc (sizeof(int) * WRT_SIZE);
    int		i, j;
    FILE	*out = fopen(DNAME, "w");
    hid_t       dcpl;

    TESTING("large dataset write");

    /*
     * We might be on a machine that has 32-bit files, so create an HDF5 file
     * which is a family of files.  Each member of the family will be 1GB
     */
    if ((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl))<0) {
	goto error;
    }

    /* Create simple data spaces according to the size specified above. */
    if ((space1 = H5Screate_simple (4, size1, size1))<0 ||
	(space2 = H5Screate_simple (1, size2, size2))<0) {
	goto error;
    }

    /* Create the datasets */
/*
 *  The fix below is provided for bug#921
 *  H5Dcreate with H5P_DEFAULT creation properties
 *  will create a set of solid 1GB files; test will crash if quotas are enforced
 *  or it will take some time to write a file.
 *  We should create a dataset allocating space late and never writing fill values.
 *  EIP 4/8/03

    if ((d1=H5Dcreate (file, "d1", H5T_NATIVE_INT, space1, H5P_DEFAULT))<0 ||
	(d2=H5Dcreate (file, "d2", H5T_NATIVE_INT, space2, H5P_DEFAULT))<0) {
	goto error;
    }
*/
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_LATE);
    H5Pset_fill_time(dcpl, H5D_FILL_TIME_NEVER);
    if ((d1=H5Dcreate (file, "d1", H5T_NATIVE_INT, space1, dcpl))<0 ||
	(d2=H5Dcreate (file, "d2", H5T_NATIVE_INT, space2, dcpl))<0) {
	goto error;
    }


    /* Write some things to them randomly */
    hs_size[0] = WRT_SIZE;
    if ((mem_space = H5Screate_simple (1, hs_size, hs_size))<0) goto error;
    for (i=0; i<wrt_n; i++) {
	hs_start[0] = randll (size2[0], i);
	HDfprintf (out, "#%03d 0x%016Hx\n", i, hs_start[0]);
	if (H5Sselect_hyperslab (space2, H5S_SELECT_SET, hs_start, NULL,
				 hs_size, NULL)<0) goto error;
	for (j=0; j<WRT_SIZE; j++) {
	    buf[j] = i+1;
	}
	if (H5Dwrite (d2, H5T_NATIVE_INT, mem_space, space2,
		      H5P_DEFAULT, buf)<0) goto error;
    }

    if (H5Dclose (d1)<0) goto error;
    if (H5Dclose (d2)<0) goto error;
    if (H5Sclose (mem_space)<0) goto error;
    if (H5Sclose (space1)<0) goto error;
    if (H5Sclose (space2)<0) goto error;
    if (H5Fclose (file)<0) goto error;
    free (buf);
    fclose(out);
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Dclose(d1);
	H5Dclose(d2);
	H5Sclose(space1);
	H5Sclose(space2);
	H5Sclose(mem_space);
	H5Fclose(file);
    } H5E_END_TRY;
    if (buf) free(buf);
    if (out) fclose(out);
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	reader
 *
 * Purpose:	Reads some data from random locations in the dataset.
 *
 * Return:	Success:	0
 *
 * 		Failure:	>0
 *
 * Programmer:	Robb Matzke
 *              Friday, April 10, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
reader (char *filename, hid_t fapl)
{
    FILE	*script = NULL;
    hid_t	file=-1, mspace=-1, fspace=-1, d2=-1;
    char	ln[128], *s;
    hsize_t	hs_offset[1];
    hsize_t	hs_size[1] = {WRT_SIZE};
    int		*buf = (int*)malloc (sizeof(int) * WRT_SIZE);
    int		i, j, zero, wrong, nerrors=0;

    /* Open script file */
    script = fopen (DNAME, "r");

    /* Open HDF5 file */
    if ((file=H5Fopen(filename, H5F_ACC_RDONLY, fapl))<0) goto error;

    /* Open the dataset */
    if ((d2 = H5Dopen (file, "d2"))<0) goto error;
    if ((fspace = H5Dget_space (d2))<0) goto error;

    /* Describe `buf' */
    if ((mspace = H5Screate_simple (1, hs_size, hs_size))<0) goto error;

    /* Read each region */
    while (fgets (ln, (int)sizeof(ln), script)) {
	if ('#'!=ln[0]) break;
	i = (int)strtol (ln+1, &s, 10);
	hs_offset[0] = HDstrtoll (s, NULL, 0);
	HDfprintf (stdout, "#%03d 0x%016Hx%47s", i, hs_offset[0], "");
	fflush (stdout);

	if (H5Sselect_hyperslab (fspace, H5S_SELECT_SET, hs_offset, NULL,
				 hs_size, NULL)<0) goto error;
	if (H5Dread (d2, H5T_NATIVE_INT, mspace, fspace, H5P_DEFAULT, buf)<0) {
	    goto error;
	}

	/* Check */
	for (j=zero=wrong=0; j<WRT_SIZE; j++) {
	    if (0==buf[j]) zero++;
	    else if (buf[j]!=i+1) wrong++;
	}
	if (zero) {
	    H5_FAILED();
	    printf("    %d zero%s\n", zero, 1==zero?"":"s");
	} else if (wrong) {
	    SKIPPED();
	    puts("    Possible overlap with another region.");
	    nerrors++;
	} else {
	    PASSED();
	}
    }

    if (H5Dclose (d2)<0) goto error;
    if (H5Sclose (mspace)<0) goto error;
    if (H5Sclose (fspace)<0) goto error;
    if (H5Fclose (file)<0) goto error;
    free (buf);
    fclose (script);
    return nerrors;

 error:
    H5E_BEGIN_TRY {
	H5Dclose(d2);
	H5Sclose(mspace);
	H5Sclose(fspace);
	H5Fclose(file);
    } H5E_END_TRY;
    if (buf) free(buf);
    if (script) fclose(script);
    return 1;
}



/*-------------------------------------------------------------------------
 * Function:	usage
 *
 * Purpose:	Print command usage
 *
 * Return:	void
 *
 * Programmer:	Albert Chent
 *              Mar 28, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
usage(void)
{
    HDfprintf(stdout,
	"Usage: big [-h] [-c] [-fsize <fsize>}\n"
	"\t-h\tPrint the help page\n"
	"\t-c\tFile system Checking skipped.  Caution: this test generates\n"
	"\t\tmany big files and may fill up the file system.\n"
	"\t-fsize\tChange family size default to <fsize> where <fsize> is\n"
	"\t\ta positive float point number.  Default value is %Hu.\n"
	"Examples:\n"
	"\tbig -fsize 2.1e9 \t# test with file size just under 2GB\n"
	"\tbig -fsize 2.2e9 \t# test with file size just above 2GB\n"
	"\tBe sure the file system can support the file size requested\n"
	, (hsize_t)FAMILY_SIZE);
}



/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:
 *
 * Return:	Success:
 *
 *		Failure:
 *
 * Programmer:	Robb Matzke
 *              Friday, April 10, 1998
 *
 * Modifications:
 *		Albert Cheng, 2002/03/28
 *		Added command option -fsize.
 *		Albert Cheng, 2002/04/19
 *		Added command option -c.
 *
 *-------------------------------------------------------------------------
 */
int
main (int ac, char **av)
{
    hid_t	fapl=-1;
    hsize_t	family_size;
    hsize_t	family_size_def;	/* default family file size */
    double	family_size_def_dbl;	/* default family file size */
    int		cflag=1;		/* check file system before test */
    char	filename[1024];

    /* parameters setup */
    family_size_def = FAMILY_SIZE;

    while (--ac > 0){
	av++;
	if (strcmp("-fsize", *av)==0){
	    /* specify a different family file size */
	    ac--; av++;
	    if (ac > 0){
		family_size_def_dbl = atof(*av);
                H5_ASSIGN_OVERFLOW(family_size_def,family_size_def_dbl,double,hsize_t);
		if (family_size_def <= 0)
		    family_size_def = (hsize_t)FAMILY_SIZE;
	    }
	    else{
		printf("***Missing fsize value***\n");
		usage();
		return 1;
	    }
	}
	else if (strcmp("-c", *av)==0){
	    /* turn off file system check before test */
	    cflag=0;
	}
	else if (strcmp("-h", *av)==0){
	    usage();
	    return 0;
	}else{
	    usage();
	    return 1;
	}
    }

    /* Reset library */
    h5_reset();
    fapl = h5_fileaccess();

    /* Test big file with the family driver */
    puts("Testing big file with the Family Driver ");
    if (H5FD_FAMILY!=H5Pget_driver(fapl)) {
	HDfprintf(stdout,
	   "Changing file drivers to the family driver, %Hu bytes each\n",
	   family_size_def);
	if (H5Pset_fapl_family(fapl, family_size_def, H5P_DEFAULT)<0) goto error;
    } else if (H5Pget_fapl_family(fapl, &family_size, NULL)<0) {
	goto error;
    } else if (family_size!=family_size_def) {
	HDfprintf(stdout, "Changing family member size from %Hu to %Hu\n",
	       family_size, family_size_def);
	if (H5Pset_fapl_family(fapl, family_size_def, H5P_DEFAULT)<0)
	    goto error;
    }

    if (cflag){
	/*
	 * We shouldn't run this test if the file system doesn't support holes
	 * because we would generate multi-gigabyte files.
	 */
	puts("Checking if file system is adequate for this test...");
	if (sizeof(long_long)<8 || 0==GB8LL) {
	    puts("Test skipped because sizeof(long_long) is too small. This");
	    puts("hardware apparently doesn't support 64-bit integer types.");
	    usage();
	    goto quit;
	}
	if (!is_sparse()) {
	    puts("Test skipped because file system does not support holes.");
	    usage();
	    goto quit;
	}
	if (!enough_room(fapl)) {
	    puts("Test skipped because of quota (file size or num open files).");
	    usage();
	    goto quit;
	}
    }

    /* Do the test with the Family Driver */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if (writer(filename, fapl, WRT_N)) goto error;
    if (reader(filename, fapl)) goto error;

    puts("Test passed with the Family Driver.");

    /*
     * We shouldn't run this test if the file system doesn't support big files 
     * because we would generate multi-gigabyte files.
     */
    puts("\nChecking if file system supports big files...");
    if (!supports_big()) {
        puts("Tests for sec2 and stdio are skipped because file system does not support big files.");
        usage();
        goto quit;
    }

    /* Clean up the test file */
    if (h5_cleanup(FILENAME, fapl)) remove(DNAME);

    /* Test big file with the SEC2 driver */
    puts("Testing big file with the SEC2 Driver ");

    fapl = h5_fileaccess();
    if(H5Pset_fapl_sec2(fapl)<0)

    HDmemset(filename, 0, sizeof(filename));
    h5_fixname(FILENAME[2], fapl, filename, sizeof filename);

    if (writer(filename, fapl, WRT_N)) goto error;
    if (reader(filename, fapl)) goto error;

    puts("Test passed with the SEC2 Driver.");

#ifdef H5_HAVE_FSEEKO
    /* Clean up the test file */
    if (h5_cleanup(FILENAME, fapl)) remove(DNAME);

    /* Test big file with the STDIO driver only if fseeko is supported,
     * because the OFFSET parameter of fseek has the type LONG, not big
     * enough to support big files. */
    puts("\nTesting big file with the STDIO Driver ");

    fapl = h5_fileaccess();
    if(H5Pset_fapl_stdio(fapl)<0)

    HDmemset(filename, 0, sizeof(filename));
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);

    if (writer(filename, fapl, WRT_N)) goto error;
    if (reader(filename, fapl)) goto error;
    puts("Test passed with the STDIO Driver.");
#endif

quit:
    /* End with normal exit code */
    if (h5_cleanup(FILENAME, fapl)) remove(DNAME);
    return 0;

error:
    if (fapl>=0) H5Pclose(fapl);
    puts("*** TEST FAILED ***");
    return 1;
}
