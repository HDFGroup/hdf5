Appendix A: OpenMP-HDF5 Programs
-------------------------------------------------------------------------
                Program 1
-------------------------------------------------------------------------
/*  
 *  This example writes 64 datasets to a HDF5 file, using multiple threads
 *  (OpenMP).  Each thread grab the lock while it tries to call HDF5 functions
 *  to write out dataset.  In this way, the HDF5 calls are serialized, while
 *  the calculation part is in parallel.   This is one of the ways to do 
 *  OpenMP computation with HDF.  As long as not to do HDF I/O in parallel, 
 *  it is safe to use HDF.
 */
 
#include <hdf5.h>
#include <omp.h>
#include <math.h>

#define NUM_THREADS 4
#define NUM_MDSET   16
#define FILE        "SDS.h5"
#define NX          5                  /* dataset dimensions */
#define NY          18
#define RANK        2

void CalcWriteData(hid_t, hid_t, hid_t);

/*Global variable, OpenMP lock*/
omp_lock_t  lock;


int
main (void)
{
    hid_t       fid;                  /* file and dataset handles */
    hid_t       datatype, dataspace;   /* handles */
    hsize_t     dimsf[2];              /* dataset dimensions */
    herr_t      status;                             
    int         i;

    /*
     * Create a new file using H5F_ACC_TRUNC access,
     * default file creation properties, and default file
     * access properties.
     */
    fid = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Describe the size of the array and create the data space for fixed
     * size dataset. 
     */
    dimsf[0] = NX;
    dimsf[1] = NY;
    dataspace = H5Screate_simple(RANK, dimsf, NULL); 

    /* 
     * Define datatype for the data in the file.
     * We will store little endian INT numbers.
     */
    datatype = H5Tcopy(H5T_NATIVE_DOUBLE);
    status = H5Tset_order(datatype, H5T_ORDER_LE);

    /*Disable dynamic allocation of threads*/
    omp_set_dynamic(0);

    /*Allocate threads*/
    omp_set_num_threads(NUM_THREADS);

    /*Initialize lock*/
    omp_init_lock(&lock);

    /*Each thread grab one iteration in the for loop and call function 
     * CaclWriteData*/
    #pragma omp parallel default(shared)
    {
        #pragma omp for
             for(i=0; i<NUM_THREADS; i++) {
                  CalcWriteData(fid, dataspace, datatype);
             }
    }

    /*Finished lock mechanism, destroy it*/
    omp_destroy_lock(&lock);

    /*
     * Close/release resources.
     */
    H5Sclose(dataspace);
    H5Tclose(datatype);
    H5Fclose(fid);
 
    return 0;
}

/*Each thread will call this function independantly.  They calculate dataset
 *and then write it out to hdf, for NUM_MDSET times */
void CalcWriteData(hid_t fid, hid_t dataspace, hid_t datatype)
{
    double   data[NX][NY];
    hid_t    dataset;
    char     dname[16];
    int      tid;
    int      i, j, k;

    tid = omp_get_thread_num();

    for(i=0; i<NUM_MDSET; i++) {
         /*Weird calculation to extend computing time*/
         for(j=0; j<NX; j++) {
              for(k=0; k<NY; k++) 
                   data[j][k] = ( pow(sin(tid), 2.0) + pow(cos(tid), 2.0) ) * 
                            ( tid + i ) +
                            ( pow(123456789.0, 8.0) - pow(123456789.0, 8.0) );
   
	 }
         sprintf(dname, "tid%d-dataset%d", tid, i);

         /*Serialize HDF dataset writing using OpenMP lock*/
         omp_set_lock(&lock);

         dataset = H5Dcreate(fid, dname, datatype, dataspace, H5P_DEFAULT);
         H5Dwrite(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, 
                  data);
         H5Dclose(dataset);

         /*Release lock*/
         omp_unset_lock(&lock);
    }

}




-------------------------------------------------------------------------
		Program 2
-------------------------------------------------------------------------
/*  
 *  This example compute the element values of an array in parallel 
 *  by two threads.  Then write this dataset into HDF file.  This is
 *  one of the ways to do OpenMP computation with HDF.  As long as 
 *  not to do HDF I/O in parallel, it is safe to use HDF.
 *  
 */
 
#include <hdf5.h>
#include <omp.h>

#define FILE        "SDS.h5"
#define DATASETNAME "IntArray" 
#define NX     5                      /* dataset dimensions */
#define NY     6
#define RANK   2

int
main (void)
{
    hid_t       file, dataset;         /* file and dataset handles */
    hid_t       datatype, dataspace;   /* handles */
    hsize_t     dimsf[2];              /* dataset dimensions */
    herr_t      status;                             
    int         data[NX][NY];          /* data to write */
    int         i, j;

    /*
     * Create a new file using H5F_ACC_TRUNC access,
     * default file creation properties, and default file
     * access properties.
     */
    file = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Describe the size of the array and create the data space for fixed
     * size dataset. 
     */
    dimsf[0] = NX;
    dimsf[1] = NY;
    dataspace = H5Screate_simple(RANK, dimsf, NULL); 

    /* 
     * Define datatype for the data in the file.
     * We will store little endian INT numbers.
     */
    datatype = H5Tcopy(H5T_NATIVE_INT);
    status = H5Tset_order(datatype, H5T_ORDER_LE);

    /* Disable dynamic allocation of threads. */
    omp_set_dynamic(0);
    
    /* Allocate 2 threads */
    omp_set_num_threads(2);


    /* Parallel computation.  Let 2 threads handle this nested for loops
     * in parallel.  Only one data array is computed.  */  
    #pragma omp parallel default(shared) 
    {
        #pragma omp for 
            for (j = 0; j < NX; j++) {
                #pragma omp parallel shared(j, NY) 
                {
                    #pragma omp for 
                        for (i = 0; i < NY; i++)
                            data[j][i] = i + j; 
                }
            }
    }

    /* Write this dataset into HDF file */
    dataset = H5Dcreate(file, DATASETNAME, datatype, dataspace,
                        H5P_DEFAULT); 
    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
                        H5P_DEFAULT, data);
    H5Dclose(dataset);

    /*
     * Close/release resources.
     */
    H5Sclose(dataspace);
    H5Tclose(datatype);
    H5Fclose(file);
 
    return 0;
}     



-------------------------------------------------------------------------
		Program 3
-------------------------------------------------------------------------
/*  
 *  This example create two threads.  Each thread writes  a dataset to 
 *  the HDF5 file in parallel.  This program only works occasionally.
 */
 
#include <hdf5.h>
#include <omp.h>

#define FILE   "SDS.h5"
#define NX     5                      /* dataset dimensions */
#define NY     6
#define RANK   2

void writeData(int, hid_t, hid_t, hid_t, char*);

int main (void)
{
    hid_t       file;                  /* file and dataset handles */
    hid_t       datatype, dataspace;   /* handles */
    hsize_t     dimsf[2];              /* dataset dimensions */
    herr_t      status;                             
    int         id;
    char        dname[2][10] = {"Array1", "Array2"};

    /*
     * Create a new file using H5F_ACC_TRUNC access,
     * default file creation properties, and default file
     * access properties.
     */
    file = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Describe the size of the array and create the data space for fixed
     * size dataset. 
     */
    dimsf[0] = NX;
    dimsf[1] = NY;
    dataspace = H5Screate_simple(RANK, dimsf, NULL); 

    /* 
     * Define datatype for the data in the file.
     * We will store little endian INT numbers.
     */
    datatype = H5Tcopy(H5T_NATIVE_INT);
    status = H5Tset_order(datatype, H5T_ORDER_LE);

    /*Disable dynamic allocation of threads*/
    omp_set_dynamic(0);
 
    /*Allocate 2 threads*/
    omp_set_num_threads(2);

    /*Parallel Part:  each thread call function writeData; id is private to 
     *                thread while others are shared                     */
    #pragma omp parallel shared(file, dataspace, datatype, dname) private(id)
    {
         id = omp_get_thread_num();
         writeData(id, file, dataspace, datatype, dname[id]);           
    }


    /*
     * Close/release resources.
     */
    H5Sclose(dataspace);
    H5Tclose(datatype);
    H5Fclose(file);
 
    return 0;
}   


/*Each thread call this function to write a dataset into HDF5 file*/
 
void writeData(int id, hid_t file, hid_t dataspace, hid_t datatype, char *dname)
{
    int    data[NX][NY];
    hid_t  dataset;
    int    i, j;

    for (j = 0; j < NX; j++) {
        for (i = 0; i < NY; i++)
            data[j][i] = i + j + id;
    }

    dataset = H5Dcreate(file, dname, datatype, dataspace,
                       H5P_DEFAULT);
    H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
                       H5P_DEFAULT, data);
    H5Dclose(dataset);
}


-------------------------------------------------------------------------
		Program 4
-------------------------------------------------------------------------
/*  
 * This example compute and write two datasets into HDF file in
 * parallel.  It also only works occasionally.      
 */
 
#include <hdf5.h>
#include <omp.h>

#define FILE        "SDS.h5"
#define DATASETNAME "IntArray" 
#define NX     5                      /* dataset dimensions */
#define NY     6
#define RANK   2

int
main (void)
{
    hid_t       file, dataset;         /* file and dataset handles */
    hid_t       datatype, dataspace;   /* handles */
    hsize_t     dimsf[2];              /* dataset dimensions */
    herr_t      status;                             
    int         data[NX][NY];          /* data to write */
    int         i, j, id;
    char        dname[2][10] = {"intArray1", "intArray2"};

    /*
     * Create a new file using H5F_ACC_TRUNC access,
     * default file creation properties, and default file
     * access properties.
     */
    file = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Describe the size of the array and create the data space for fixed
     * size dataset. 
     */
    dimsf[0] = NX;
    dimsf[1] = NY;
    dataspace = H5Screate_simple(RANK, dimsf, NULL); 

    /* 
     * Define datatype for the data in the file.
     * We will store little endian INT numbers.
     */
    datatype = H5Tcopy(H5T_NATIVE_INT);
    status = H5Tset_order(datatype, H5T_ORDER_LE);

    omp_set_dynamic(0);
    omp_set_num_threads(2);
    
    
    /* This part of program compute and write two datasets in parallel. */
    #pragma omp parallel shared(file, datatype, dataspace, dname) private(id, j, i, data, dataset)
    {
        id = omp_get_thread_num();
        for (j = 0; j < NX; j++) {
            for (i = 0; i < NY; i++)
                data[j][i] = i + j + id;
        }

        dataset = H5Dcreate(file, dname[id], datatype, dataspace,
                            H5P_DEFAULT); 
        H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
                              H5P_DEFAULT, data);
        H5Dclose(dataset);
    }


    /*
     * Close/release resources.
     */
    H5Sclose(dataspace);
    H5Tclose(datatype);
    H5Fclose(file);
 
    return 0;
}     

