/*****************************************************************************
  This test generates attributes, groups, and datasets of many types. It 
  creates a large number of attributes, groups, and datasets by specifying 
  -a, -g, -d options respectively. Using "-h" option to see details.

  Programmer:  Peter Cao <xcao@hdfgroup.org>, Jan. 2013
 ****************************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FNAME       "test_perf.h5"
#define NGROUPS     20
#define NDSETS      20
#define NATTRS      20
#define DIM0        40
#define NROWS       100
#define NTYPES      9
#define MAXVLEN     10
#define FIXED_LEN   8

typedef enum { SOLID=0, LIQUID, GAS, PLASMA } phase_t;                                  

typedef struct {
    int                 i;
    unsigned long long  l;
    float               f;
    double              d;
    char                s[FIXED_LEN];
    phase_t             e;
    float               f_array[FIXED_LEN];
    hvl_t               i_vlen;
    char                *s_vlen;
} test_comp_t;    

typedef struct {
    int     zipcode;
    char    *city;
} zipcode_t;                               

int add_attrs(hid_t oid, int idx);
int add_attr(hid_t oid, const char *name, hid_t tid, hid_t sid, void *buf) ;
herr_t create_perf_test_file(const char *fname, int ngrps, int ndsets, 
       int nattrs, hsize_t nrows, hsize_t dim0, hsize_t chunk, int vlen, 
	   int compressed, int latest);

int main (int argc, char *argv[])
{
    char fname[32];
    int i, ngrps=NGROUPS, ndsets=NDSETS, nattrs=NATTRS, dim0=DIM0, 
	    chunk=DIM0/10+1, nrows=NROWS, vlen=MAXVLEN, l=0, z=0;

    memset(fname, 0, 32);
    for (i=1; i<argc; i++) {
        if (strcmp(argv[i], "-f")==0)
            strcpy(fname, argv[i+1]);
        else if (strcmp(argv[i], "-g")==0)
            ngrps = atoi(argv[i+1]);
        else if (strcmp(argv[i], "-d")==0)
            ndsets = atoi(argv[i+1]);
        else if (strcmp(argv[i], "-a")==0)
            nattrs = atoi(argv[i+1]);
        else if (strcmp(argv[i], "-r")==0)
            nrows = atoi(argv[i+1]);
        else if (strcmp(argv[i], "-s")==0)
            dim0 = atoi(argv[i+1]);
        else if (strcmp(argv[i], "-c")==0)
            chunk = atoi(argv[i+1]);
        else if (strcmp(argv[i], "-v")==0)
            vlen = atoi(argv[i+1]);
        else if (strcmp(argv[i], "-l")==0)
            l = 1;
        else if (strcmp(argv[i], "-z")==0)
            z = 1; 
        else if (strcmp(argv[i], "-h")==0) {
            printf("\nOPTONS:\n");
            printf("\t-f F:\tname of the test file (default: %s).\n", FNAME);
            printf("\t-g N:\tnumber of top level groups (default: %d).\n", NGROUPS);
            printf("\t-d N:\tnumber of datasets (default: %d).\n", NDSETS);
            printf("\t-a N:\tnumber of attributes (default: %d).\n", NATTRS);
            printf("\t-r N:\tnumber of rows in the large compound dataset (default: %d).\n", NROWS);
            printf("\t-s N:\tsize of dim0 in datasets (default: %d).\n", DIM0);
            printf("\t-c N:\tchunk size of dim0 (default: %d).\n", (DIM0/10+1));
            printf("\t-v N:\tmax vlen size (default: %d).\n", MAXVLEN);
            printf("\t-l:\tuse latest format (default: no).\n");
            printf("\t-z:\tuse gzip compression (default: no).\n");
            printf("\t-h:\tthis help information.\n");
            printf("Example:\n");
            printf("\t./a.out -f test.h5 -g 10000 -d 5000 -a 500 -r 10000 -s 200 -c 20 -v 40 -l -z\n\n");
            exit(0);
        }
    }
	
    if (strlen(fname)<=0)
        sprintf(fname, FNAME);

	create_perf_test_file(fname, ngrps, ndsets, nattrs, (hsize_t)nrows, 
	                     (hsize_t)dim0, (hsize_t)chunk, vlen, z, l);
    
    return 0;
}

/*****************************************************************************
  This function generates attributes, groups, and datasets of many types. 

  Parameters:
            fname:	file_name.
            ngrps:	number of top level groups.
            ndsets:	number of datasets.
            attrs:	number of attributes.
            nrow:	number of rows in a dataset.
            chunk:	chunk size (single number).
            vlen:	max vlen size.
            comp:	use latest format.
            latest:	use gzip comnpression.
			
  Return:  Non-negative on success/Negative on failure
  
  Programmer:  Peter Cao <xcao@hdfgroup.org>, Jan. 2013
 ****************************************************************************/
herr_t create_perf_test_file(const char *fname, int ngrps, int ndsets, 
       int nattrs, hsize_t nrows, hsize_t dim0, hsize_t chunk, int vlen, 
	   int compressed, int latest)
{
    int         i, j, k;
    hid_t       fid, sid_null, sid_scalar, sid_1d, sid_2d, did, aid, sid_2, sid_large, 
	            fapl=H5P_DEFAULT, dcpl=H5P_DEFAULT, gid1, gid2, cmp_tid, tid_str, 
				tid_enum, tid_array_f, tid_vlen_i, tid_vlen_s;
    char        name[32], tmp_name1[32], tmp_name2[32], tmp_name3[32];
    hsize_t     dims[1]={dim0}, dims2d[2]={dim0, (dim0/4+1)}, dims_array[1]={FIXED_LEN}, 
	            dim1[1]={2};
    char        *enum_names[4] = {"SOLID", "LIQUID", "GAS", "PLASMA"};
    test_comp_t *buf_comp=NULL, *buf_comp_large=NULL;
    int         *buf_int=NULL;
    float       (*buf_float_a)[FIXED_LEN]=NULL;
    double      **buf_double2d=NULL;
    hvl_t       *buf_vlen_i=NULL;
	char        (*buf_str)[FIXED_LEN];
	char        **buf_vlen_s=NULL;
	hobj_ref_t  buf_ref[2];
	hdset_reg_ref_t buf_reg_ref[2];
    size_t      offset, len;
    herr_t      status;
    char        *names[NTYPES] = { "int", "ulong", "float", "double", "fixed string", 
	            "enum", "fixed float array", "vlen int array", "vlen strings"};
    hid_t       types[NTYPES] = { H5T_NATIVE_INT, H5T_NATIVE_UINT64, H5T_NATIVE_FLOAT, 
                H5T_NATIVE_DOUBLE, tid_str, tid_enum, tid_array_f, tid_vlen_i, tid_vlen_s};
	hsize_t     coords[4][2] = { {0,  1}, {3, 5}, {1,  0}, {2,  4}}, start=0, stride=1, count=1;
	
	if (nrows < NROWS) nrows = NROWS; 
    if (ngrps<NGROUPS) ngrps=NGROUPS;
	if (ndsets<NDSETS) ndsets=NDSETS;
	if (nattrs<NATTRS) nattrs=NATTRS;
	if (dim0<DIM0) dim0=DIM0;
    if (chunk>dim0) chunk=dim0/4;
    if (chunk<1) chunk = 1;
	if (vlen<1) vlen = MAXVLEN;

    /* create fixed string datatype */                                   
    types[4] = tid_str =  H5Tcopy (H5T_C_S1);
    H5Tset_size (tid_str, FIXED_LEN);

    /* create enum datatype */
    types[5] = tid_enum = H5Tenum_create(H5T_NATIVE_INT);
    for (i = (int) SOLID; i <= (int) PLASMA; i++) {
        phase_t val = (phase_t) i;
        status = H5Tenum_insert (tid_enum, enum_names[i], &val);
    }

    /* create float array datatype */
    types[6] = tid_array_f = H5Tarray_create (H5T_NATIVE_FLOAT, 1, dims_array);
 
    /* create variable length integer datatypes */
    types[7] = tid_vlen_i = H5Tvlen_create (H5T_NATIVE_INT);
    	
    /* create variable length string datatype */
    types[8] = tid_vlen_s =  H5Tcopy (H5T_C_S1);
    H5Tset_size (tid_vlen_s, H5T_VARIABLE);
                   
    /* create compound datatypes */    
    cmp_tid = H5Tcreate (H5T_COMPOUND, sizeof (test_comp_t));
    offset = 0;
    for (i=0; i<NTYPES-2; i++) {
        H5Tinsert(cmp_tid, names[i], offset, types[i]);
        offset += H5Tget_size(types[i]);
    }

	H5Tinsert(cmp_tid, names[7], offset, types[7]);
	offset += sizeof (hvl_t);
	H5Tinsert(cmp_tid, names[8], offset, types[8]);

	/* create dataspace */
    sid_1d = H5Screate_simple (1, dims, NULL);
    sid_2d = H5Screate_simple (2, dims2d, NULL);
    sid_2 = H5Screate_simple  (1, dim1, NULL);
	sid_large = H5Screate_simple  (1, &nrows, NULL);
    sid_null = H5Screate (H5S_NULL);	
	sid_scalar = H5Screate (H5S_SCALAR);
	
	/* create fid access property */
	fapl = H5Pcreate (H5P_FILE_ACCESS);
    H5Pset_libver_bounds (fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST);

	/* create dataset creation property */
    dcpl = H5Pcreate (H5P_DATASET_CREATE);

	/* set dataset chunk */
    if (chunk>0) {
        H5Pset_chunk (dcpl, 1, &chunk);
    }

	/* set dataset compression */
    if (compressed) {
        if (chunk<=0) {
            chunk = dim0/10+1;;
            H5Pset_chunk (dcpl, 1, &chunk);
        }
        H5Pset_shuffle (dcpl);
        H5Pset_deflate (dcpl, 6);
    }	

	/* allocate buffers */
    buf_comp   = (test_comp_t *)calloc(dim0, sizeof(test_comp_t));
    buf_comp_large   = (test_comp_t *)calloc(nrows, sizeof(test_comp_t));
    buf_int    = (int *)calloc(dim0, sizeof(int));
    buf_float_a  = malloc(dim0*sizeof(*buf_float_a));
	buf_vlen_i = (hvl_t *)calloc(dim0, sizeof (hvl_t));
    buf_vlen_s = (char **)calloc(dim0, sizeof(char *));
	buf_str    =  malloc(dim0*sizeof (*buf_str));

	/* allocate array of doulbe pointers */
	buf_double2d  = (double **)calloc(dims2d[0],sizeof(double *));
	/* allocate a contigous chunk of memory for the data */
	buf_double2d[0] = (double *)calloc( dims2d[0]*dims2d[1],sizeof(double) );
	/* assign memory city to pointer array */
	for (i=1; i <dims2d[0]; i++) buf_double2d[i] = buf_double2d[0]+i*dims2d[1];

	/* fill buffer values */
	len = 1;
    for (i=0; i<dims[0]; i++) {
        buf_comp[i].i = buf_int[i] = i-2147483648;
        buf_comp[i].l = 0xffffffffffffffff-i;
        buf_comp[i].f = 1.0/(i+1.0);
        buf_comp[i].d = 987654321.0*i+1.0/(i+1.0);
        buf_comp[i].e = (phase_t) (i % (int) (PLASMA + 1));
		
		for (j=0; j<FIXED_LEN; j++) {
		    buf_comp[i].f_array[j] = buf_float_a[i][j] = i*100+j;
			buf_str[i][j] = 'a' + (i%26);
		}
		buf_str[i][FIXED_LEN-1] = 0;
        strcpy(buf_comp[i].s, buf_str[i]);
		
		len = (1-cos(i/8.0))/2*vlen+1;
		if (!i) len = vlen;
		buf_vlen_i[i].len = len;
		buf_vlen_i[i].p = (int *)calloc(len, sizeof(int));
		for (j=0; j<len; j++) ((int*)(buf_vlen_i[i].p))[j] = i*100+j;
		buf_comp[i].i_vlen = buf_vlen_i[i];
		
		buf_vlen_s[i] = (char *)calloc(len, sizeof(char));
		for (j=0; j<len-1; j++)
		    buf_vlen_s[i][j] = j%26+'A';
		buf_comp[i].s_vlen = buf_vlen_s[i];
		
		for (j=0; j<dims2d[1]; j++)
		    buf_double2d[i][j] = i+j/10000.0;
    }

    for (i=0; i<nrows; i++) {
        buf_comp_large[i].i = i-2147483648;
        buf_comp_large[i].l = 0xffffffffffffffff-i;
        buf_comp_large[i].f = 1.0/(i+1.0);
        buf_comp_large[i].d = 987654321.0*i+1.0/(i+1.0);
        buf_comp_large[i].e = (phase_t) (i % (int) (PLASMA + 1));
        for (j=0; j<FIXED_LEN-1; j++) {
            buf_comp_large[i].f_array[j] = i*100+j;
            buf_comp_large[i].s[j] = 'a' + (i%26);
        }
		len = i%vlen+1;
        buf_comp_large[i].i_vlen.len = len;
        buf_comp_large[i].i_vlen.p = (int *)calloc(len, sizeof(int));
        for (j=0; j<len; j++) ((int*)(buf_comp_large[i].i_vlen.p))[j] = i*100+j;
        buf_comp_large[i].s_vlen = (char *)calloc(i+2, sizeof(char));
        for (j=0; j<i+1; j++) (buf_comp_large[i].s_vlen)[j] = j%26+'A';
    }
	
	/* create file */
    if (latest)
        fid = H5Fcreate (fname, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    else
        fid = H5Fcreate (fname, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
	add_attrs(fid, 0);

	sprintf(name, "a cmp ds of %d rows", nrows);
	did = H5Dcreate (fid, name, cmp_tid, sid_large, H5P_DEFAULT, dcpl, H5P_DEFAULT);
	H5Dwrite (did, cmp_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_comp_large);
	add_attrs(did, 0); 
	H5Dclose(did);

	// /* add attributes*/
    gid1 = H5Gcreate (fid, "attributes", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	if (nattrs<1) nattrs = 1;
	i=0;
	while (i<nattrs) i += add_attrs(gid1, i);
	H5Gclose(gid1);
		
	/* add many sub groups to a group*/
    gid1 = H5Gcreate (fid, "groups", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	add_attrs(gid1, 0);
    for (i=0; i<ngrps; i++) {
	    /* create sub groups */
        sprintf(name, "g%02d", i);
        gid2 = H5Gcreate (gid1, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		if (i<10) add_attrs(gid2, 0);
		H5Gclose(gid2);
	}
	H5Gclose(gid1);

	/* add many datasets to a group */
	gid1 = H5Gcreate (fid, "datasets", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	add_attrs(gid1, 0);
    for (j=0; j<ndsets; j+=12) {
		/* 1 add a null dataset */
		sprintf(name, "%05d null dataset", j);
        did = H5Dcreate (gid1, name, H5T_STD_I32LE, sid_null, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		if (!j) add_attrs(did, j); 
        H5Dclose(did);	

		/* 2 add scalar int point */
	    sprintf(name, "%05d scalar int point", j);
        did = H5Dcreate (gid1, name, H5T_NATIVE_INT, sid_scalar, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        H5Dwrite (did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &j);		
		if (!j) add_attrs(did, j); 
		H5Dclose(did);		
		
		/* 3 scalar vlen string */
	    sprintf(name, "%05d scalar vlen string", j);
        did = H5Dcreate (gid1, name, tid_vlen_s, sid_scalar, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        H5Dwrite (did, tid_vlen_s, H5S_ALL, H5S_ALL, H5P_DEFAULT, &buf_vlen_s[0]);		
		if (!j) add_attrs(did, j); 
		H5Dclose(did);			
	
	    /* 4 add fixed-length float array */
		sprintf(name, "%05d fixed-length float array", j);
		did = H5Dcreate (gid1, name, tid_array_f, sid_1d, H5P_DEFAULT, dcpl, H5P_DEFAULT);
		H5Dwrite (did, tid_array_f, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_float_a);
		if (!j) add_attrs(did, j); 
		H5Dclose(did);
	
		/* 5 add fixed-length strings */
		sprintf(name, "%05d fixed-length strings", j);
		did = H5Dcreate (gid1, name, tid_str, sid_1d, H5P_DEFAULT, dcpl, H5P_DEFAULT);
		H5Dwrite (did, tid_str, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_str);
		if (!j) add_attrs(did, j); 
		H5Dclose(did);
		
		/* 6 add compound data */
	    sprintf(name, "%05d compund data", j);
	    did = H5Dcreate (gid1, name, cmp_tid, sid_1d, H5P_DEFAULT, dcpl, H5P_DEFAULT);
	    H5Dwrite (did, cmp_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_comp);
		if (!j) add_attrs(did, j); 
		H5Dclose(did);

		/* 7 add 2D double */
	    sprintf(name, "%05d 2D double", j);
		strcpy (tmp_name1, name);
	    did = H5Dcreate (gid1, name, H5T_NATIVE_DOUBLE, sid_2d, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	    H5Dwrite (did, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_double2d[0]);
		if (!j) add_attrs(did, j); 
		H5Dclose(did);
		
		/* 8 add 1D int array */
	    sprintf(name, "%05d 1D int array", j);
        did = H5Dcreate (gid1, name, H5T_NATIVE_INT, sid_1d, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        H5Dwrite (did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_int);		
		if (!j) add_attrs(did, j); 
		H5Dclose(did);
		
		/* 9 add vlen int array */
	    sprintf(name, "%05d vlen int array", j);
		strcpy (tmp_name2, name);
        did = H5Dcreate (gid1, name, tid_vlen_i, sid_1d, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        H5Dwrite (did, tid_vlen_i, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_vlen_i);		
		if (!j) add_attrs(did, j); 
		H5Dclose(did);	

		/* 10 add vlen strings */
	    sprintf(name, "%05d vlen strings", j);
		strcpy (tmp_name3, name);
        did = H5Dcreate (gid1, name, tid_vlen_s, sid_1d, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        H5Dwrite (did, tid_vlen_s, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_vlen_s);		
		if (!j) add_attrs(did, j); 
		H5Dclose(did);	
		
		/* 11 add object refs */
		H5Rcreate(&buf_ref[0],gid1, ".", H5R_OBJECT, (hid_t)-1); 
		H5Rcreate(&buf_ref[1],gid1, tmp_name3, H5R_OBJECT, (hid_t)-1); 
	    sprintf(name, "%05d obj refs", j);
        did = H5Dcreate (gid1, name, H5T_STD_REF_OBJ, sid_2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        H5Dwrite (did, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_ref);		
		if (!j) add_attrs(did, j); 
		H5Dclose(did);

		/* 12 add region refs */
		H5Sselect_elements (sid_2d, H5S_SELECT_SET, 4, coords[0]);
		H5Rcreate(&buf_reg_ref[0],gid1, tmp_name1, H5R_DATASET_REGION, sid_2d); 
		H5Sselect_none(sid_2d);
		count = dims[0]/2+1;
		H5Sselect_hyperslab (sid_1d, H5S_SELECT_SET, &start, &stride, &count,NULL);
		H5Rcreate(&buf_reg_ref[1],gid1, tmp_name2, H5R_DATASET_REGION, sid_1d); 
		H5Sselect_none(sid_1d);
	    sprintf(name, "%05d region refs", j);
        did = H5Dcreate (gid1, name, H5T_STD_REF_DSETREG, sid_2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        H5Dwrite (did, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_reg_ref);		
		if (!j) add_attrs(did, j); 
		H5Dclose(did);
	}
	H5Gclose(gid1);			
	
    H5Tclose (tid_array_f);
    H5Tclose (tid_vlen_i);
    H5Tclose (tid_vlen_s);
    H5Tclose (tid_enum);
    H5Tclose (tid_str);
    H5Tclose (cmp_tid);
    H5Pclose (dcpl);
    H5Pclose (fapl);
    H5Sclose (sid_1d);
    H5Sclose (sid_2d);
    H5Sclose (sid_2);
    H5Sclose (sid_large);
    H5Sclose (sid_null);
    H5Sclose (sid_scalar);
    H5Fclose (fid);

    for (i=0; i<dims[0]; i++) {
		if (buf_vlen_i[i].p) free(buf_vlen_i[i].p);
		if (buf_vlen_s[i]) free(buf_vlen_s[i]);
	}

    for (i=0; i<nrows; i++) {
	    if (buf_comp_large[i].i_vlen.p)  free(buf_comp_large[i].i_vlen.p);
		if (buf_comp_large[i].s_vlen) free(buf_comp_large[i].s_vlen);
	}
	
    free (buf_comp);
    free (buf_comp_large);
    free (buf_int);
    free (buf_float_a);
    free (buf_double2d[0]);
    free (buf_double2d);
	free (buf_str);
    free(buf_vlen_i);
    free(buf_vlen_s);

    return 0;
}

/* add a single attribute */
int add_attr(hid_t oid, const char *name, hid_t tid, hid_t sid, void *buf) 
{
    hid_t aid;

    aid = H5Acreate (oid, name, tid, sid, H5P_DEFAULT, H5P_DEFAULT);
	if (aid <0)
	return 0;
	
    H5Awrite(aid, tid, buf); 

    H5Aclose(aid);
	
	return 1;
}

/* 
    adds different types of attributes to an object.
	
	returns the number of attributes added to the objects.
 */
int add_attrs(hid_t oid, int idx) 
{
    char name[32];
    int i0, i1, i2, j, nattrs=0;
	hid_t aid, tid, tid1, sid;
    hvl_t               i_vlen[4];
	hobj_ref_t          ref;
	zipcode_t            cmp_data[4];
    unsigned int        i = 0xffffffff;
    long long           l = -2147483647;
    float               f = 123456789.987654321;
    double              d = 987654321.123456789;
    char                *s[7] = {"Parting", "is such", "sweeter", "sorrow."};
    float               f_array[4] = {1.0, 2.22, 3.333, 4.444};
    char                *s_vlen[4] = {"Parting", "is such", "sweet", "sorrow."};
	hsize_t             dims1[1]={1}, dims2[1]={4}, dims3[2]={3,5};
	int                 int3d[4][3][5];
	 size_t             offset = 0;
	
	for (i0=0; i0<4; i0++) {
	    i_vlen[i0].len = (i0+1);
		i_vlen[i0].p = (int *)calloc(i_vlen[i0].len, sizeof(int));
		for (j=0; j<i_vlen[i0].len; j++)
		    ((int *)i_vlen[i0].p)[j] = i0*100+j;
	    for (i1=0; i1<3; i1++) {
	        for (i2=0; i2<5; i2++)
	            int3d[i0][i1][i2] = i0*i1-i1*i2+i0*i2;
		}
    }

	cmp_data[0].zipcode = 01001;
    cmp_data[0].city = "Agawam, Massachusetts";
    cmp_data[1].zipcode = 99950;
    cmp_data[1].city = "Ketchikan, Alaska";
    cmp_data[2].zipcode = 00501;
    cmp_data[2].city = "Holtsville, New York";
    cmp_data[3].zipcode = 61820;
    cmp_data[3].city = "Champaign, Illinois";
	
	/* 1 scalar point */
	sid = H5Screate (H5S_SCALAR);
	sprintf(name, "%05d scalar int", idx);
    nattrs += add_attr(oid, name, H5T_NATIVE_UINT, sid, &i);	
	sprintf(name, "%05d scalar ulong", idx);
    nattrs += add_attr(oid, name, H5T_NATIVE_INT64, sid, &l);	
	sprintf(name, "%05d scalar str", idx);
	tid =  H5Tcopy (H5T_C_S1);
    H5Tset_size (tid, H5T_VARIABLE);
    nattrs += add_attr(oid, name, tid, sid, &s[2]);	
	H5Tclose(tid);
	H5Sclose(sid);

	/* 4 single point */
	sid = H5Screate_simple (1, dims1, NULL);
    H5Rcreate(&ref, oid, ".", H5R_OBJECT, (hid_t)-1);
	sprintf(name, "%05d single float", idx);
    nattrs += add_attr(oid, name, H5T_NATIVE_FLOAT, sid, &f);	
	sprintf(name, "%05d single double", idx);
    nattrs += add_attr(oid, name, H5T_NATIVE_DOUBLE, sid, &d);	
	sprintf(name, "%05d single obj_ref", idx);
    nattrs += add_attr(oid, name, H5T_STD_REF_OBJ, sid, &ref);	
	H5Sclose(sid);
	
	/* 7 fixed length 1D array */
	sid = H5Screate_simple (1, dims1, NULL);
	tid = H5Tarray_create (H5T_NATIVE_FLOAT, 1, dims2);
	sprintf(name, "%05d array float", idx);
    nattrs += add_attr(oid, name, tid, sid, &f_array[0]);
	H5Tclose(tid);
	tid =  H5Tcopy (H5T_C_S1);
    H5Tset_size (tid, strlen(s[0])+1);	
	tid1 = H5Tarray_create (tid, 1, dims2);
	sprintf(name, "%05d array str", idx);
    nattrs += add_attr(oid, name, tid1, sid, s);	
	H5Tclose(tid1);	
	H5Tclose(tid);	
	H5Sclose(sid);

	/* 9 fixed length 2D int arrays */
	sid = H5Screate_simple (1, dims2, NULL);
	tid = H5Tarray_create (H5T_NATIVE_INT, 2, dims3);
	sprintf(name, "%05d array int 2D", idx);
    nattrs += add_attr(oid, name, tid, sid, int3d[0][0]);
	H5Tclose(tid);
	H5Sclose(sid);
	
	/* 10 variable length arrays */
	sid = H5Screate_simple (1, dims2, NULL);
	tid = H5Tcopy (H5T_C_S1);
	H5Tset_size (tid, H5T_VARIABLE);
	sprintf(name, "%05d vlen strings", idx);
    nattrs += add_attr(oid, name, tid, sid, s_vlen);
	H5Tclose(tid);	
	tid = H5Tvlen_create (H5T_NATIVE_INT);;
	sprintf(name, "%05d vlen int array", idx);
    nattrs += add_attr(oid, name, tid, sid, i_vlen);
	H5Tclose(tid);
	H5Sclose(sid);

	/* 12 compound data */
	sid = H5Screate_simple (1, dims2, NULL);
	tid = H5Tcreate (H5T_COMPOUND, sizeof (zipcode_t));
	tid1 = H5Tcopy (H5T_C_S1);
	H5Tset_size (tid1, H5T_VARIABLE);	
    H5Tinsert (tid, "zip code", 0, H5T_NATIVE_INT); offset += sizeof(H5T_NATIVE_INT);
    H5Tinsert (tid, "City", offset, tid1); offset += sizeof(char *);
	sprintf(name, "%05d compound data", idx);
    nattrs += add_attr(oid, name, tid, sid, cmp_data);
	H5Tclose(tid1);	
	H5Tclose(tid);	
	H5Sclose(sid);

	for (i0=0; i0<4; i0++) 
	    free(i_vlen[i0].p);
		
	return nattrs;
}
