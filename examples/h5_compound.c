/*
 * This example shows how to create a compound data type,
 * write an array which has the compound data type to the file,
 * and read back fields' subsets.
 */

#include "hdf5.h"

#define FILE          "SDScompound.h5"
#define DATASETNAME   "ArrayOfStructures"
#define LENGTH        10
#define RANK          1

main()

{

   
/* First structure  and dataset*/
typedef struct s1_t {
    int    a;
    float  b;
    double c; 
} s1_t;
s1_t       s1[LENGTH];
hid_t      s1_tid;     /* File datatype hadle */

/* Second structure (subset of s1_t)  and dataset*/
typedef struct s2_t {
    double c;
    int    a;
} s2_t;
s2_t       s2[LENGTH];
hid_t      s2_tid;    /* Memory datatype handle */

/* Third "structure" ( will be used to read float field of s1) */
hid_t      s3_tid;   /* Memory datatype handle */
float      s3[LENGTH];

int        i;
hid_t      file, datatype, dataset, space; /* Handles */
herr_t     status;
size_t     dim[] = {LENGTH};   /* Dataspace dimensions */

H5T_class_t class;
size_t     size;

/*
 * Initialize the data
 */
   for (i = 0; i< LENGTH; i++) {
        s1[i].a = i;
        s1[i].b = i*i;
        s1[i].c = 1./(i+1);
}

/*
 * Create the data space.
 */
space = H5Screate_simple(RANK, dim, NULL);

/*
 * Create the file.
 */
file = H5Fcreate(FILE, H5F_ACC_TRUNC, H5C_DEFAULT, H5C_DEFAULT);

/*
 * Create the memory data type. 
 */
s1_tid = H5Tcreate (H5T_COMPOUND, sizeof(s1_t));
status = H5Tinsert(s1_tid, "a_name", HPOFFSET(s1, a), H5T_NATIVE_INT);
status = H5Tinsert(s1_tid, "c_name", HPOFFSET(s1, c), H5T_NATIVE_DOUBLE);
status = H5Tinsert(s1_tid, "b_name", HPOFFSET(s1, b), H5T_NATIVE_FLOAT);

/* 
 * Create the dataset.
 */
dataset = H5Dcreate(file, DATASETNAME, s1_tid, space, H5C_DEFAULT);

/*
 * Wtite data to the dataset; 
 */
status = H5Dwrite(dataset, s1_tid, H5S_ALL, H5S_ALL, H5C_DEFAULT, s1);

/*
 * Release resources
 */
H5Tclose(s1_tid);
H5Sclose(space);
H5Dclose(dataset);
H5Fclose(file);
 
/*
 * Open the file and the dataset.
 */
file = H5Fopen(FILE, H5F_ACC_RDONLY, H5C_DEFAULT);
 
dataset = H5Dopen(file, DATASETNAME);

/* 
 * Create a data type for s2
 */
s2_tid = H5Tcreate(H5T_COMPOUND, sizeof(s2_t));

status = H5Tinsert(s2_tid, "c_name", HPOFFSET(s2, c), H5T_NATIVE_DOUBLE);
status = H5Tinsert(s2_tid, "a_name", HPOFFSET(s2, a), H5T_NATIVE_INT);

/*
 * Read two fields c and a from s1 dataset. Fields iin the file
 * are found by their names "c_name" and "a_name".
 */
status = H5Dread(dataset, s2_tid, H5S_ALL, H5S_ALL, H5C_DEFAULT, s2);

/*
 * Display the fields
 */
printf("\n");
printf("Field c : \n");
for( i = 0; i < LENGTH; i++) printf("%.4f ", s2[i].c);
printf("\n");

printf("\n");
printf("Field a : \n");
for( i = 0; i < LENGTH; i++) printf("%d ", s2[i].a);
printf("\n");

/* 
 * Create a data type for s3.
 */
s3_tid = H5Tcreate(H5T_COMPOUND, sizeof(float));

status = H5Tinsert(s3_tid, "b_name", 0, H5T_NATIVE_FLOAT);

/*
 * Read field b from s1 dataset. Field in the file is found by its name.
 */
status = H5Dread(dataset, s3_tid, H5S_ALL, H5S_ALL, H5C_DEFAULT, s3);

/*
 * Display the field
 */
printf("\n");
printf("Field b : \n");
for( i = 0; i < LENGTH; i++) printf("%.4f ", s3[i]);
printf("\n");

/*
 * Release resources
 */
H5Tclose(s2_tid);
H5Tclose(s3_tid);
H5Dclose(dataset);
H5Sclose(space);
H5Fclose(file);
}
