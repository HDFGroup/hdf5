
#include "hdf5.h"
#include "testphdf5.h"
#include "H5Dprivate.h"                /* For Chunk tests */

/* FILENAME and filenames must have the same number of names */
const char *FILENAME[2]={ "bigio_test.h5",
                           NULL
                        };

/* Constants definitions */
#define MAX_ERR_REPORT  10      /* Maximum number of errors reported */

/* Define some handy debugging shorthands, routines, ... */
/* debugging tools */

#define MAINPROCESS     (!mpi_rank) /* define process 0 as main process */

/* Constants definitions */
#define RANK        2

#define IN_ORDER     1
#define OUT_OF_ORDER 2

#define DATASET1 "DSET1"
#define DATASET2 "DSET2"
#define DATASET3 "DSET3"
#define DATASET4 "DSET4"
#define DATASET5 "DSET5"
#define DXFER_COLLECTIVE_IO 0x1  /* Collective IO*/
#define DXFER_INDEPENDENT_IO 0x2 /* Independent IO collectively */
#define DXFER_BIGCOUNT 536870916

#define HYPER 1
#define POINT 2
#define ALL 3

/* Dataset data type.  Int's can be easily octo dumped. */
typedef hsize_t B_DATATYPE;

int facc_type = FACC_MPIO;        /*Test file access type */
int dxfer_coll_type = DXFER_COLLECTIVE_IO;
size_t bigcount = DXFER_BIGCOUNT;
int nerrors = 0;
int mpi_size, mpi_rank;

hsize_t space_dim1 = SPACE_DIM1 * 256; // 4096
hsize_t space_dim2 = SPACE_DIM2;

static void coll_chunktest(const char* filename, int chunk_factor, int select_factor,
                           int api_option, int file_selection, int mem_selection, int mode);
hid_t create_faccess_plist(MPI_Comm comm, MPI_Info info, int l_facc_type);

/*
 * Setup the coordinates for point selection.
 */
static void
set_coords(hsize_t start[],
          hsize_t count[],
          hsize_t stride[],
          hsize_t block[],
          size_t num_points,
          hsize_t coords[],
          int order)
{
    hsize_t i,j, k = 0, m ,n, s1 ,s2;

    if(OUT_OF_ORDER == order)
        k = (num_points * RANK) - 1;
    else if(IN_ORDER == order)
        k = 0;

    s1 = start[0];
    s2 = start[1];

    for(i = 0 ; i < count[0]; i++)
        for(j = 0 ; j < count[1]; j++)
            for(m = 0 ; m < block[0]; m++)
                for(n = 0 ; n < block[1]; n++)
                    if(OUT_OF_ORDER == order) {
                        coords[k--] = s2 + (stride[1] * j) + n;
                        coords[k--] = s1 + (stride[0] * i) + m;
                    }
                    else if(IN_ORDER == order) {
                        coords[k++] = s1 + stride[0] * i + m;
                        coords[k++] = s2 + stride[1] * j + n;
                    }
}

/*
 * Fill the dataset with trivial data for testing.
 * Assume dimension rank is 2 and data is stored contiguous.
 */
static void
fill_datasets(hsize_t start[], hsize_t block[], B_DATATYPE * dataset)
{
    B_DATATYPE *dataptr = dataset;
    hsize_t i, j;

    /* put some trivial data in the data_array */
    for (i=0; i < block[0]; i++){
    for (j=0; j < block[1]; j++){
        *dataptr = (B_DATATYPE)((i+start[0])*100 + (j+start[1]+1));
        dataptr++;
    }
    }
}

/*
 * Setup the coordinates for point selection.
 */
void point_set(hsize_t start[],
               hsize_t count[],
               hsize_t stride[],
               hsize_t block[],
               size_t num_points,
               hsize_t coords[],
               int order)
{
    hsize_t i,j, k = 0, m ,n, s1 ,s2;

    HDcompile_assert(RANK == 2);

    if(OUT_OF_ORDER == order)
        k = (num_points * RANK) - 1;
    else if(IN_ORDER == order)
        k = 0;

    s1 = start[0];
    s2 = start[1];

    for(i = 0 ; i < count[0]; i++)
        for(j = 0 ; j < count[1]; j++)
            for(m = 0 ; m < block[0]; m++)
                for(n = 0 ; n < block[1]; n++)
                    if(OUT_OF_ORDER == order) {
                        coords[k--] = s2 + (stride[1] * j) + n;
                        coords[k--] = s1 + (stride[0] * i) + m;
                    }
                    else if(IN_ORDER == order) {
                        coords[k++] = s1 + stride[0] * i + m;
                        coords[k++] = s2 + stride[1] * j + n;
                    }

    if(VERBOSE_MED) {
        HDprintf("start[]=(%lu, %lu), count[]=(%lu, %lu), stride[]=(%lu, %lu), block[]=(%lu, %lu), total datapoints=%lu\n",
               (unsigned long)start[0], (unsigned long)start[1], (unsigned long)count[0], (unsigned long)count[1],
               (unsigned long)stride[0], (unsigned long)stride[1], (unsigned long)block[0], (unsigned long)block[1],
               (unsigned long)(block[0] * block[1] * count[0] * count[1]));
        k = 0;
        for(i = 0; i < num_points ; i++) {
            HDprintf("(%d, %d)\n", (int)coords[k], (int)coords[k + 1]);
            k += 2;
        }
    }
}

/*
 * Print the content of the dataset.
 */
static void
dataset_print(hsize_t start[], hsize_t block[], B_DATATYPE * dataset)
{
    B_DATATYPE *dataptr = dataset;
    hsize_t i, j;

    /* print the column heading */
    HDprintf("%-8s", "Cols:");
    for (j=0; j < block[1]; j++){
        HDprintf("%3lu ", (unsigned long)(start[1]+j));
    }
    HDprintf("\n");

    /* print the slab data */
    for (i=0; i < block[0]; i++){
    HDprintf("Row %2lu: ", (unsigned long)(i+start[0]));
    for (j=0; j < block[1]; j++){
        HDprintf("%llu ", *dataptr++);
    }
    HDprintf("\n");
    }
}


/*
 * Print the content of the dataset.
 */
static int
verify_data(hsize_t start[], hsize_t count[], hsize_t stride[], hsize_t block[], B_DATATYPE *dataset, B_DATATYPE *original)
{
    hsize_t i, j;
    int vrfyerrs;

    /* print it if VERBOSE_MED */
    if(VERBOSE_MED) {
        HDprintf("verify_data dumping:::\n");
        HDprintf("start(%lu, %lu), count(%lu, %lu), stride(%lu, %lu), block(%lu, %lu)\n",
            (unsigned long)start[0], (unsigned long)start[1], (unsigned long)count[0], (unsigned long)count[1],
            (unsigned long)stride[0], (unsigned long)stride[1], (unsigned long)block[0], (unsigned long)block[1]);
        HDprintf("original values:\n");
        dataset_print(start, block, original);
        HDprintf("compared values:\n");
        dataset_print(start, block, dataset);
    }

    vrfyerrs = 0;
    for (i=0; i < block[0]; i++){
    for (j=0; j < block[1]; j++){
        if(*dataset != *original){
        if(vrfyerrs++ < MAX_ERR_REPORT || VERBOSE_MED){
            HDprintf("Dataset Verify failed at [%lu][%lu](row %lu, col %lu): expect %llu, got %llu\n",
                           (unsigned long)i, (unsigned long)j,
                           (unsigned long)(i+start[0]), (unsigned long)(j+start[1]),
                           *(original), *(dataset));
        }
        dataset++;
        original++;
        }
    }
    }
    if(vrfyerrs > MAX_ERR_REPORT && !VERBOSE_MED)
        HDprintf("[more errors ...]\n");
    if(vrfyerrs)
        HDprintf("%d errors found in verify_data\n", vrfyerrs);
    return(vrfyerrs);
}

/* Set up the selection */
static void
ccslab_set(int mpi_rank,
    int mpi_size,
    hsize_t start[],
    hsize_t count[],
    hsize_t stride[],
    hsize_t block[],
    int mode)
{

    switch (mode){

    case BYROW_CONT:
    /* Each process takes a slabs of rows. */
    block[0]  =  1;
    block[1]  =  1;
    stride[0] =  1;
    stride[1] =  1;
    count[0]  =  space_dim1;
    count[1]  =  space_dim2;
    start[0]  =  mpi_rank*count[0];
    start[1]  =  0;

    break;

    case BYROW_DISCONT:
    /* Each process takes several disjoint blocks. */
    block[0]  =  1;
    block[1]  =  1;
        stride[0] =  3;
        stride[1] =  3;
        count[0]  =  space_dim1/(stride[0]*block[0]);
        count[1]  =  (space_dim2)/(stride[1]*block[1]);
    start[0]  =  space_dim1*mpi_rank;
    start[1]  =  0;

    break;

    case BYROW_SELECTNONE:
    /* Each process takes a slabs of rows, there are
           no selections for the last process. */
    block[0]  =  1;
    block[1]  =  1;
    stride[0] =  1;
    stride[1] =  1;
    count[0]  =  ((mpi_rank >= MAX(1,(mpi_size-2)))?0:space_dim1);
    count[1]  =  space_dim2;
    start[0]  =  mpi_rank*count[0];
    start[1]  =  0;

    break;

    case BYROW_SELECTUNBALANCE:
      /* The first one-third of the number of processes only
         select top half of the domain, The rest will select the bottom
         half of the domain. */

        block[0]  = 1;
    count[0]  = 2;
        stride[0] = space_dim1*mpi_size/4+1;
        block[1]  = space_dim2;
        count[1]  = 1;
        start[1]  = 0;
        stride[1] = 1;
    if((mpi_rank *3)<(mpi_size*2)) start[0]  = mpi_rank;
    else start[0] = 1 + space_dim1*mpi_size/2 + (mpi_rank-2*mpi_size/3);
        break;

    case BYROW_SELECTINCHUNK:
      /* Each process will only select one chunk */

        block[0] = 1;
        count[0] = 1;
    start[0] = mpi_rank*space_dim1;
        stride[0]= 1;
    block[1] = space_dim2;
    count[1] = 1;
    stride[1]= 1;
    start[1] = 0;

        break;

    default:
    /* Unknown mode.  Set it to cover the whole dataset. */
    block[0]  = space_dim1*mpi_size;
    block[1]  = space_dim2;
    stride[0] = block[0];
    stride[1] = block[1];
    count[0]  = 1;
    count[1]  = 1;
    start[0]  = 0;
    start[1]  = 0;

    break;
    }
    if (VERBOSE_MED){
      HDprintf("start[]=(%lu,%lu), count[]=(%lu,%lu), stride[]=(%lu,%lu), block[]=(%lu,%lu), total datapoints=%lu\n",
    (unsigned long)start[0], (unsigned long)start[1], (unsigned long)count[0], (unsigned long)count[1],
    (unsigned long)stride[0], (unsigned long)stride[1], (unsigned long)block[0], (unsigned long)block[1],
    (unsigned long)(block[0]*block[1]*count[0]*count[1]));
    }
}


/*
 * Fill the dataset with trivial data for testing.
 * Assume dimension rank is 2.
 */
static void
ccdataset_fill(hsize_t start[],
        hsize_t stride[],
        hsize_t count[],
        hsize_t block[],
        DATATYPE * dataset,
               int mem_selection)
{
    DATATYPE *dataptr = dataset;
    DATATYPE *tmptr;
    hsize_t   i,j,k1,k2,k=0;
    /* put some trivial data in the data_array */
    tmptr = dataptr;

    /* assign the disjoint block (two-dimensional)data array value
       through the pointer */

    for (k1 = 0; k1 < count[0]; k1++) {
      for(i = 0;  i < block[0]; i++) {
        for(k2 = 0; k2 < count[1]; k2++) {
          for(j = 0;j < block[1]; j++) {

            if (ALL != mem_selection) {
                dataptr  =  tmptr + ((start[0]+k1*stride[0]+i)*space_dim2+
                                     start[1]+k2*stride[1]+j);
            }
            else {
                dataptr = tmptr + k;
                k++;
            }

            *dataptr = (DATATYPE)(k1+k2+i+j);
          }
        }
      }
    }
}

/*
 * Print the first block of the content of the dataset.
 */
static void
ccdataset_print(hsize_t start[],
        hsize_t block[],
        DATATYPE * dataset)

{
    DATATYPE *dataptr = dataset;
    hsize_t i, j;

    /* print the column heading */
    HDprintf("Print only the first block of the dataset\n");
    HDprintf("%-8s", "Cols:");
    for (j=0; j < block[1]; j++){
        HDprintf("%3lu ", (unsigned long)(start[1]+j));
    }
    HDprintf("\n");

    /* print the slab data */
    for (i=0; i < block[0]; i++){
        HDprintf("Row %2lu: ", (unsigned long)(i+start[0]));
        for (j=0; j < block[1]; j++){
            HDprintf("%03d ", *dataptr++);
        }
        HDprintf("\n");
    }
}

/*
 * Print the content of the dataset.
 */
static int
ccdataset_vrfy(hsize_t start[],
        hsize_t count[],
        hsize_t stride[],
        hsize_t block[],
        DATATYPE *dataset,
        DATATYPE *original,
               int mem_selection)
{
    hsize_t i, j,k1,k2,k=0;
    int vrfyerrs;
    DATATYPE *dataptr,*oriptr;

    /* print it if VERBOSE_MED */
    if (VERBOSE_MED) {
        HDprintf("dataset_vrfy dumping:::\n");
        HDprintf("start(%lu, %lu), count(%lu, %lu), stride(%lu, %lu), block(%lu, %lu)\n",
            (unsigned long)start[0], (unsigned long)start[1], (unsigned long)count[0], (unsigned long)count[1],
            (unsigned long)stride[0], (unsigned long)stride[1], (unsigned long)block[0], (unsigned long)block[1]);
        HDprintf("original values:\n");
        ccdataset_print(start, block, original);
        HDprintf("compared values:\n");
        ccdataset_print(start, block, dataset);
    }

    vrfyerrs = 0;

    for (k1=0;k1<count[0];k1++) {
        for(i=0;i<block[0];i++) {
            for(k2=0; k2<count[1];k2++) {
                for(j=0;j<block[1];j++) {
                    if (ALL != mem_selection) {
                        dataptr = dataset + ((start[0]+k1*stride[0]+i)*space_dim2+
                                             start[1]+k2*stride[1]+j);
                        oriptr =  original + ((start[0]+k1*stride[0]+i)*space_dim2+
                                              start[1]+k2*stride[1]+j);
                    }
                    else {
                        dataptr = dataset + k;
                        oriptr = original + k;
                        k++;
                    }
                    if (*dataptr != *oriptr){
                        if (vrfyerrs++ < MAX_ERR_REPORT || VERBOSE_MED){
                            HDprintf("Dataset Verify failed at [%lu][%lu]: expect %d, got %d\n",
                                   (unsigned long)i, (unsigned long)j,
                                   *(oriptr), *(dataptr));
                        }
                    }
                }
            }
        }
    }
    if (vrfyerrs > MAX_ERR_REPORT && !VERBOSE_MED)
        HDprintf("[more errors ...]\n");
    if (vrfyerrs)
        HDprintf("%d errors found in ccdataset_vrfy\n", vrfyerrs);
    return(vrfyerrs);
}

/*
 * Example of using the parallel HDF5 library to create two datasets
 * in one HDF5 file with collective parallel access support.
 * The Datasets are of sizes (number-of-mpi-processes x dim0) x dim1.
 * Each process controls only a slab of size dim0 x dim1 within each
 * dataset. [Note: not so yet.  Datasets are of sizes dim0xdim1 and
 * each process controls a hyperslab within.]
 */

static void
dataset_big_write(void)
{

    hid_t xfer_plist;        /* Dataset transfer properties list */
    hid_t sid;           /* Dataspace ID */
    hid_t file_dataspace;    /* File dataspace ID */
    hid_t mem_dataspace;    /* memory dataspace ID */
    hid_t dataset;
    hid_t datatype;        /* Datatype ID */
    hsize_t dims[RANK];       /* dataset dim sizes */
    hsize_t start[RANK];            /* for hyperslab setting */
    hsize_t count[RANK], stride[RANK];        /* for hyperslab setting */
    hsize_t block[RANK];            /* for hyperslab setting */
    hsize_t *coords = NULL;
    int i;
    herr_t ret;             /* Generic return value */
    hid_t fid;                  /* HDF5 file ID */
    hid_t acc_tpl;        /* File access templates */
    hsize_t h;
    size_t num_points;
    B_DATATYPE * wdata;


    /* allocate memory for data buffer */
    wdata = (B_DATATYPE *)HDmalloc(bigcount*sizeof(B_DATATYPE));
    VRFY((wdata != NULL), "wdata malloc succeeded");

    /* setup file access template */
    acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
    VRFY((acc_tpl >= 0), "H5P_FILE_ACCESS");
    H5Pset_fapl_mpio(acc_tpl, MPI_COMM_WORLD, MPI_INFO_NULL);

    /* create the file collectively */
    fid = H5Fcreate(FILENAME[0], H5F_ACC_TRUNC, H5P_DEFAULT, acc_tpl);
    VRFY((fid >= 0), "H5Fcreate succeeded");

    /* Release file-access template */
    ret = H5Pclose(acc_tpl);
    VRFY((ret >= 0), "");


    /* Each process takes a slabs of rows. */
    if (mpi_rank == 0)
        HDprintf("\nTesting Dataset1 write by ROW\n");
    /* Create a large dataset */
    dims[0] = bigcount;
    dims[1] = mpi_size;

    sid = H5Screate_simple (RANK, dims, NULL);
    VRFY((sid >= 0), "H5Screate_simple succeeded");
    dataset = H5Dcreate2(fid, DATASET1, H5T_NATIVE_LLONG, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dcreate2 succeeded");
    H5Sclose(sid);

    block[0] = dims[0]/mpi_size;
    block[1] = dims[1];
    stride[0] = block[0];
    stride[1] = block[1];
    count[0] = 1;
    count[1] = 1;
    start[0] = mpi_rank*block[0];
    start[1] = 0;

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill the local slab with some trivial data */
    fill_datasets(start, block, wdata);
    MESG("data_array initialized");
    if(VERBOSE_MED){
        MESG("data_array created");
        dataset_print(start, block, wdata);
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "H5Pcreate xfer succeeded");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");
    if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
        ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist,H5FD_MPIO_INDIVIDUAL_IO);
        VRFY((ret>= 0),"set independent IO collectively succeeded");
    }

    ret = H5Dwrite(dataset, H5T_NATIVE_LLONG, mem_dataspace, file_dataspace,
                   xfer_plist, wdata);
    VRFY((ret >= 0), "H5Dwrite dataset1 succeeded");

    /* release all temporary handles. */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    ret = H5Dclose(dataset);
    VRFY((ret >= 0), "H5Dclose1 succeeded");


    /* Each process takes a slabs of cols. */
    if (mpi_rank == 0)
        HDprintf("\nTesting Dataset2 write by COL\n");
    /* Create a large dataset */
    dims[0] = bigcount;
    dims[1] = mpi_size;

    sid = H5Screate_simple (RANK, dims, NULL);
    VRFY((sid >= 0), "H5Screate_simple succeeded");
    dataset = H5Dcreate2(fid, DATASET2, H5T_NATIVE_LLONG, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dcreate2 succeeded");
    H5Sclose(sid);

    block[0] = dims[0];
    block[1] = dims[1]/mpi_size;
    stride[0] = block[0];
    stride[1] = block[1];
    count[0] = 1;
    count[1] = 1;
    start[0] = 0;
    start[1] = mpi_rank*block[1];

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill the local slab with some trivial data */
    fill_datasets(start, block, wdata);
    MESG("data_array initialized");
    if(VERBOSE_MED){
        MESG("data_array created");
        dataset_print(start, block, wdata);
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "H5Pcreate xfer succeeded");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");
    if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
        ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist,H5FD_MPIO_INDIVIDUAL_IO);
        VRFY((ret>= 0),"set independent IO collectively succeeded");
    }

    ret = H5Dwrite(dataset, H5T_NATIVE_LLONG, mem_dataspace, file_dataspace,
                   xfer_plist, wdata);
    VRFY((ret >= 0), "H5Dwrite dataset1 succeeded");

    /* release all temporary handles. */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    ret = H5Dclose(dataset);
    VRFY((ret >= 0), "H5Dclose1 succeeded");



    /* ALL selection */
    if (mpi_rank == 0)
        HDprintf("\nTesting Dataset3 write select ALL proc 0, NONE others\n");
    /* Create a large dataset */
    dims[0] = bigcount;
    dims[1] = 1;

    sid = H5Screate_simple (RANK, dims, NULL);
    VRFY((sid >= 0), "H5Screate_simple succeeded");
    dataset = H5Dcreate2(fid, DATASET3, H5T_NATIVE_LLONG, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dcreate2 succeeded");
    H5Sclose(sid);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    if(mpi_rank == 0) {
        ret = H5Sselect_all(file_dataspace);
        VRFY((ret >= 0), "H5Sset_all succeeded");
    }
    else {
        ret = H5Sselect_none(file_dataspace);
        VRFY((ret >= 0), "H5Sset_none succeeded");
    }

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, dims, NULL);
    VRFY((mem_dataspace >= 0), "");
    if(mpi_rank != 0) {
        ret = H5Sselect_none(mem_dataspace);
        VRFY((ret >= 0), "H5Sset_none succeeded");
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "H5Pcreate xfer succeeded");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");
    if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
        ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist,H5FD_MPIO_INDIVIDUAL_IO);
        VRFY((ret>= 0),"set independent IO collectively succeeded");
    }

    /* fill the local slab with some trivial data */
    fill_datasets(start, dims, wdata);
    MESG("data_array initialized");
    if(VERBOSE_MED){
        MESG("data_array created");
    }

    ret = H5Dwrite(dataset, H5T_NATIVE_LLONG, mem_dataspace, file_dataspace,
                   xfer_plist, wdata);
    VRFY((ret >= 0), "H5Dwrite dataset1 succeeded");

    /* release all temporary handles. */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    ret = H5Dclose(dataset);
    VRFY((ret >= 0), "H5Dclose1 succeeded");

    /* Point selection */
    if (mpi_rank == 0)
        HDprintf("\nTesting Dataset4 write point selection\n");
    /* Create a large dataset */
    dims[0] = bigcount;
    dims[1] = mpi_size * 4;

    sid = H5Screate_simple (RANK, dims, NULL);
    VRFY((sid >= 0), "H5Screate_simple succeeded");
    dataset = H5Dcreate2(fid, DATASET4, H5T_NATIVE_LLONG, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dcreate2 succeeded");
    H5Sclose(sid);

    block[0] = dims[0]/2;
    block[1] = 2;
    stride[0] = dims[0]/2;
    stride[1] = 2;
    count[0] = 1;
    count[1] = 1;
    start[0] = 0;
    start[1] = dims[1]/mpi_size * mpi_rank;

    num_points = bigcount;

    coords = (hsize_t *)HDmalloc(num_points * RANK * sizeof(hsize_t));
    VRFY((coords != NULL), "coords malloc succeeded");

    set_coords (start, count, stride, block, num_points, coords, IN_ORDER);
    /* create a file dataspace */
    file_dataspace = H5Dget_space (dataset);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_elements(file_dataspace, H5S_SELECT_SET, num_points, coords);
    VRFY((ret >= 0), "H5Sselect_elements succeeded");

    if(coords) free(coords);

    fill_datasets(start, block, wdata);
    MESG("data_array initialized");
    if(VERBOSE_MED){
        MESG("data_array created");
        dataset_print(start, block, wdata);
    }

    /* create a memory dataspace */
    /* Warning: H5Screate_simple requires an array of hsize_t elements
     * even if we only pass only a single value.  Attempting anything else
     * appears to cause problems with 32 bit compilers.
     */
    mem_dataspace = H5Screate_simple (1, dims, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "H5Pcreate xfer succeeded");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");
    if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
        ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist,H5FD_MPIO_INDIVIDUAL_IO);
        VRFY((ret>= 0),"set independent IO collectively succeeded");
    }

    ret = H5Dwrite(dataset, H5T_NATIVE_LLONG, mem_dataspace, file_dataspace,
                   xfer_plist, wdata);
    VRFY((ret >= 0), "H5Dwrite dataset1 succeeded");

    /* release all temporary handles. */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    ret = H5Dclose(dataset);
    VRFY((ret >= 0), "H5Dclose1 succeeded");

    HDfree(wdata);
    H5Fclose(fid);
}

/*
 * Example of using the parallel HDF5 library to read two datasets
 * in one HDF5 file with collective parallel access support.
 * The Datasets are of sizes (number-of-mpi-processes x dim0) x dim1.
 * Each process controls only a slab of size dim0 x dim1 within each
 * dataset. [Note: not so yet.  Datasets are of sizes dim0xdim1 and
 * each process controls a hyperslab within.]
 */

static void
dataset_big_read(void)
{
    hid_t fid;                  /* HDF5 file ID */
    hid_t acc_tpl;        /* File access templates */
    hid_t xfer_plist;        /* Dataset transfer properties list */
    hid_t file_dataspace;    /* File dataspace ID */
    hid_t mem_dataspace;    /* memory dataspace ID */
    hid_t dataset;
    B_DATATYPE *rdata = NULL;    /* data buffer */
    B_DATATYPE *wdata = NULL;     /* expected data buffer */
    hsize_t dims[RANK];       /* dataset dim sizes */
    hsize_t start[RANK];            /* for hyperslab setting */
    hsize_t count[RANK], stride[RANK];        /* for hyperslab setting */
    hsize_t block[RANK];            /* for hyperslab setting */
    int i,j,k;
    hsize_t h;
    size_t num_points;
    hsize_t *coords = NULL;
    herr_t ret;             /* Generic return value */

    /* allocate memory for data buffer */
    rdata = (B_DATATYPE *)HDmalloc(bigcount*sizeof(B_DATATYPE));
    VRFY((rdata != NULL), "rdata malloc succeeded");
    wdata = (B_DATATYPE *)HDmalloc(bigcount*sizeof(B_DATATYPE));
    VRFY((wdata != NULL), "wdata malloc succeeded");

    HDmemset(rdata, 0, bigcount*sizeof(B_DATATYPE));

    /* setup file access template */
    acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
    VRFY((acc_tpl >= 0), "H5P_FILE_ACCESS");
    H5Pset_fapl_mpio(acc_tpl, MPI_COMM_WORLD, MPI_INFO_NULL);

    /* open the file collectively */
    fid=H5Fopen(FILENAME[0],H5F_ACC_RDONLY,acc_tpl);
    VRFY((fid >= 0), "H5Fopen succeeded");

    /* Release file-access template */
    ret = H5Pclose(acc_tpl);
    VRFY((ret >= 0), "");

    if (mpi_rank == 0)
        HDprintf("\nRead Testing Dataset1 by COL\n");

    dataset = H5Dopen2(fid, DATASET1, H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dopen2 succeeded");

    dims[0] = bigcount;
    dims[1] = mpi_size;
    /* Each process takes a slabs of cols. */
    block[0] = dims[0];
    block[1] = dims[1]/mpi_size;
    stride[0] = block[0];
    stride[1] = block[1];
    count[0] = 1;
    count[1] = 1;
    start[0] = 0;
    start[1] = mpi_rank*block[1];

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill dataset with test data */
    fill_datasets(start, block, wdata);
    MESG("data_array initialized");
    if(VERBOSE_MED){
    MESG("data_array created");
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");
    if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
        ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist,H5FD_MPIO_INDIVIDUAL_IO);
        VRFY((ret>= 0),"set independent IO collectively succeeded");
    }

    /* read data collectively */
    ret = H5Dread(dataset, H5T_NATIVE_LLONG, mem_dataspace, file_dataspace,
                  xfer_plist, rdata);
    VRFY((ret >= 0), "H5Dread dataset1 succeeded");

    /* verify the read data with original expected data */
    ret = verify_data(start, count, stride, block, rdata, wdata);
    if(ret) {HDfprintf(stderr, "verify failed\n"); exit(1);}

    /* release all temporary handles. */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);
    ret = H5Dclose(dataset);
    VRFY((ret >= 0), "H5Dclose1 succeeded");


    if (mpi_rank == 0)
        HDprintf("\nRead Testing Dataset2 by ROW\n");
    HDmemset(rdata, 0, bigcount*sizeof(B_DATATYPE));
    dataset = H5Dopen2(fid, DATASET2, H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dopen2 succeeded");

    dims[0] = bigcount;
    dims[1] = mpi_size;
    /* Each process takes a slabs of rows. */
    block[0] = dims[0]/mpi_size;
    block[1] = dims[1];
    stride[0] = block[0];
    stride[1] = block[1];
    count[0] = 1;
    count[1] = 1;
    start[0] = mpi_rank*block[0];
    start[1] = 0;

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill dataset with test data */
    fill_datasets(start, block, wdata);
    MESG("data_array initialized");
    if(VERBOSE_MED){
        MESG("data_array created");
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");
    if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
        ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist,H5FD_MPIO_INDIVIDUAL_IO);
        VRFY((ret>= 0),"set independent IO collectively succeeded");
    }

    /* read data collectively */
    ret = H5Dread(dataset, H5T_NATIVE_LLONG, mem_dataspace, file_dataspace,
                  xfer_plist, rdata);
    VRFY((ret >= 0), "H5Dread dataset2 succeeded");

    /* verify the read data with original expected data */
    ret = verify_data(start, count, stride, block, rdata, wdata);
    if(ret) {HDfprintf(stderr, "verify failed\n"); exit(1);}

    /* release all temporary handles. */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);
    ret = H5Dclose(dataset);
    VRFY((ret >= 0), "H5Dclose1 succeeded");

    if (mpi_rank == 0)
        HDprintf("\nRead Testing Dataset3 read select ALL proc 0, NONE others\n");
    HDmemset(rdata, 0, bigcount*sizeof(B_DATATYPE));
    dataset = H5Dopen2(fid, DATASET3, H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dopen2 succeeded");

    dims[0] = bigcount;
    dims[1] = 1;

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    if(mpi_rank == 0) {
        ret = H5Sselect_all(file_dataspace);
        VRFY((ret >= 0), "H5Sset_all succeeded");
    }
    else {
        ret = H5Sselect_none(file_dataspace);
        VRFY((ret >= 0), "H5Sset_none succeeded");
    }

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, dims, NULL);
    VRFY((mem_dataspace >= 0), "");
    if(mpi_rank != 0) {
        ret = H5Sselect_none(mem_dataspace);
        VRFY((ret >= 0), "H5Sset_none succeeded");
    }

    /* fill dataset with test data */
    fill_datasets(start, dims, wdata);
    MESG("data_array initialized");
    if(VERBOSE_MED){
        MESG("data_array created");
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");
    if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
        ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist,H5FD_MPIO_INDIVIDUAL_IO);
        VRFY((ret>= 0),"set independent IO collectively succeeded");
    }

    /* read data collectively */
    ret = H5Dread(dataset, H5T_NATIVE_LLONG, mem_dataspace, file_dataspace,
                  xfer_plist, rdata);
    VRFY((ret >= 0), "H5Dread dataset3 succeeded");

    if(mpi_rank == 0) {
        /* verify the read data with original expected data */
        ret = verify_data(start, count, stride, block, rdata, wdata);
        if(ret) {HDfprintf(stderr, "verify failed\n"); exit(1);}
    }

    /* release all temporary handles. */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);
    ret = H5Dclose(dataset);
    VRFY((ret >= 0), "H5Dclose1 succeeded");

    if (mpi_rank == 0)
        HDprintf("\nRead Testing Dataset4 with Point selection\n");
    dataset = H5Dopen2(fid, DATASET4, H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dopen2 succeeded");

    dims[0] = bigcount;
    dims[1] = mpi_size * 4;

    block[0] = dims[0]/2;
    block[1] = 2;
    stride[0] = dims[0]/2;
    stride[1] = 2;
    count[0] = 1;
    count[1] = 1;
    start[0] = 0;
    start[1] = dims[1]/mpi_size * mpi_rank;

    fill_datasets(start, block, wdata);
    MESG("data_array initialized");
    if(VERBOSE_MED){
        MESG("data_array created");
        dataset_print(start, block, wdata);
    }

    num_points = bigcount;

    coords = (hsize_t *)HDmalloc(num_points * RANK * sizeof(hsize_t));
    VRFY((coords != NULL), "coords malloc succeeded");

    set_coords (start, count, stride, block, num_points, coords, IN_ORDER);
    /* create a file dataspace */
    file_dataspace = H5Dget_space (dataset);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_elements(file_dataspace, H5S_SELECT_SET, num_points, coords);
    VRFY((ret >= 0), "H5Sselect_elements succeeded");

    if(coords) HDfree(coords);

    /* create a memory dataspace */
    /* Warning: H5Screate_simple requires an array of hsize_t elements
     * even if we only pass only a single value.  Attempting anything else
     * appears to cause problems with 32 bit compilers.
     */
    mem_dataspace = H5Screate_simple (1, dims, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");
    if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
        ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist,H5FD_MPIO_INDIVIDUAL_IO);
        VRFY((ret>= 0),"set independent IO collectively succeeded");
    }

    /* read data collectively */
    ret = H5Dread(dataset, H5T_NATIVE_LLONG, mem_dataspace, file_dataspace,
                  xfer_plist, rdata);
    VRFY((ret >= 0), "H5Dread dataset1 succeeded");

    ret = verify_data(start, count, stride, block, rdata, wdata);
    if(ret) {HDfprintf(stderr, "verify failed\n"); exit(1);}

    /* release all temporary handles. */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);
    ret = H5Dclose(dataset);
    VRFY((ret >= 0), "H5Dclose1 succeeded");

    HDfree(wdata);
    HDfree(rdata);

    wdata = NULL;
    rdata = NULL;
    /* We never wrote Dataset5 in the write section, so we can't
     * expect to read it...
     */
    file_dataspace = -1;
    mem_dataspace = -1;
    xfer_plist = -1;
    dataset = -1;

    /* release all temporary handles. */
    if (file_dataspace != -1) H5Sclose(file_dataspace);
    if (mem_dataspace != -1) H5Sclose(mem_dataspace);
    if (xfer_plist != -1) H5Pclose(xfer_plist);
    if (dataset != -1) {
        ret = H5Dclose(dataset);
        VRFY((ret >= 0), "H5Dclose1 succeeded");
    }
    H5Fclose(fid);

    /* release data buffers */
    if(rdata) HDfree(rdata);
    if(wdata) HDfree(wdata);

} /* dataset_large_readAll */


/*
 * Create the appropriate File access property list
 */
hid_t
create_faccess_plist(MPI_Comm comm, MPI_Info info, int l_facc_type)
{
    hid_t ret_pl = -1;
    herr_t ret;                 /* generic return value */
    int mpi_rank;        /* mpi variables */

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
 * Function:    coll_chunk1
 *
 * Purpose:    Wrapper to test the collective chunk IO for regular JOINT
                selection with a single chunk
 *
 * Return:    Success:    0
 *
 *        Failure:    -1
 *
 * Programmer:    Unknown
 *        July 12th, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

/* ------------------------------------------------------------------------
 *  Descriptions for the selection: One big singluar selection inside one chunk
 *  Two dimensions,
 *
 *  dim1       = space_dim1(5760)*mpi_size
 *  dim2       = space_dim2(3)
 *  chunk_dim1 = dim1
 *  chunk_dim2 = dim2
 *  block      = 1 for all dimensions
 *  stride     = 1 for all dimensions
 *  count0     = space_dim1(5760)
 *  count1     = space_dim2(3)
 *  start0     = mpi_rank*space_dim1
 *  start1     = 0
 * ------------------------------------------------------------------------
 */

void
coll_chunk1(void)
{
    const char *filename = FILENAME[0];
    if (mpi_rank == 0)
        HDprintf("coll_chunk1\n");

    coll_chunktest(filename, 1, BYROW_CONT, API_NONE, HYPER, HYPER, OUT_OF_ORDER);
    coll_chunktest(filename, 1, BYROW_CONT, API_NONE, HYPER, POINT, OUT_OF_ORDER);
    coll_chunktest(filename, 1, BYROW_CONT, API_NONE, POINT, ALL, OUT_OF_ORDER);
    coll_chunktest(filename, 1, BYROW_CONT, API_NONE, POINT, POINT, OUT_OF_ORDER);
    coll_chunktest(filename, 1, BYROW_CONT, API_NONE, POINT, HYPER, OUT_OF_ORDER);

    coll_chunktest(filename, 1, BYROW_CONT, API_NONE, POINT, ALL, IN_ORDER);
    coll_chunktest(filename, 1, BYROW_CONT, API_NONE, POINT, POINT, IN_ORDER);
    coll_chunktest(filename, 1, BYROW_CONT, API_NONE, POINT, HYPER, IN_ORDER);
}


/*-------------------------------------------------------------------------
 * Function:    coll_chunk2
 *
 * Purpose:    Wrapper to test the collective chunk IO for regular DISJOINT
                selection with a single chunk
 *
 * Return:    Success:    0
 *
 *        Failure:    -1
 *
 * Programmer:    Unknown
 *        July 12th, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

 /* ------------------------------------------------------------------------
 *  Descriptions for the selection: many disjoint selections inside one chunk
 *  Two dimensions,
 *
 *  dim1       = space_dim1*mpi_size(5760)
 *  dim2       = space_dim2(3)
 *  chunk_dim1 = dim1
 *  chunk_dim2 = dim2
 *  block      = 1 for all dimensions
 *  stride     = 3 for all dimensions
 *  count0     = space_dim1/stride0(5760/3)
 *  count1     = space_dim2/stride(3/3 = 1)
 *  start0     = mpi_rank*space_dim1
 *  start1     = 0
 *
 * ------------------------------------------------------------------------
 */
void
coll_chunk2(void)
{
    const char *filename = FILENAME[0];
    if (mpi_rank == 0)
        HDprintf("coll_chunk2\n");

    coll_chunktest(filename, 1, BYROW_DISCONT, API_NONE, HYPER, HYPER, OUT_OF_ORDER);
    coll_chunktest(filename, 1, BYROW_DISCONT, API_NONE, HYPER, POINT, OUT_OF_ORDER);
    coll_chunktest(filename, 1, BYROW_DISCONT, API_NONE, POINT, ALL, OUT_OF_ORDER);
    coll_chunktest(filename, 1, BYROW_DISCONT, API_NONE, POINT, POINT, OUT_OF_ORDER);
    coll_chunktest(filename, 1, BYROW_DISCONT, API_NONE, POINT, HYPER, OUT_OF_ORDER);

    coll_chunktest(filename, 1, BYROW_DISCONT, API_NONE, POINT, ALL, IN_ORDER);
    coll_chunktest(filename, 1, BYROW_DISCONT, API_NONE, POINT, POINT, IN_ORDER);
    coll_chunktest(filename, 1, BYROW_DISCONT, API_NONE, POINT, HYPER, IN_ORDER);
}


/*-------------------------------------------------------------------------
 * Function:    coll_chunk3
 *
 * Purpose:    Wrapper to test the collective chunk IO for regular JOINT
                selection with at least number of 2*mpi_size chunks
 *
 * Return:    Success:    0
 *
 *        Failure:    -1
 *
 * Programmer:    Unknown
 *        July 12th, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

/* ------------------------------------------------------------------------
 *  Descriptions for the selection: one singular selection accross many chunks
 *  Two dimensions, Num of chunks = 2* mpi_size
 *
 *  dim1       = space_dim1*mpi_size
 *  dim2       = space_dim2(3)
 *  chunk_dim1 = space_dim1
 *  chunk_dim2 = dim2/2
 *  block      = 1 for all dimensions
 *  stride     = 1 for all dimensions
 *  count0     = space_dim1
 *  count1     = space_dim2(3)
 *  start0     = mpi_rank*space_dim1
 *  start1     = 0
 *
 * ------------------------------------------------------------------------
 */

void
coll_chunk3(void)
{
    const char *filename = FILENAME[0];
    if (mpi_rank == 0)
        HDprintf("coll_chunk3\n");

    coll_chunktest(filename, mpi_size, BYROW_CONT, API_NONE, HYPER, HYPER, OUT_OF_ORDER);
    coll_chunktest(filename, mpi_size, BYROW_CONT, API_NONE, HYPER, POINT, OUT_OF_ORDER);
    coll_chunktest(filename, mpi_size, BYROW_CONT, API_NONE, POINT, ALL, OUT_OF_ORDER);
    coll_chunktest(filename, mpi_size, BYROW_CONT, API_NONE, POINT, POINT, OUT_OF_ORDER);
    coll_chunktest(filename, mpi_size, BYROW_CONT, API_NONE, POINT, HYPER, OUT_OF_ORDER);

    coll_chunktest(filename, mpi_size, BYROW_CONT, API_NONE, POINT, ALL, IN_ORDER);
    coll_chunktest(filename, mpi_size, BYROW_CONT, API_NONE, POINT, POINT, IN_ORDER);
    coll_chunktest(filename, mpi_size, BYROW_CONT, API_NONE, POINT, HYPER, IN_ORDER);
}


//-------------------------------------------------------------------------
// Borrowed/Modified (slightly) from t_coll_chunk.c
/*-------------------------------------------------------------------------
 * Function:    coll_chunktest
 *
 * Purpose:     The real testing routine for regular selection of collective
                chunking storage
                testing both write and read,
        If anything fails, it may be read or write. There is no
        separation test between read and write.
 *
 * Return:    Success:    0
 *
 *        Failure:    -1
 *
 * Modifications:
 *   Remove invalid temporary property checkings for API_LINK_HARD and
 *   API_LINK_TRUE cases.
 * Programmer: Jonathan Kim
 * Date: 2012-10-10
 *
 * Programmer:    Unknown
 *        July 12th, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
coll_chunktest(const char* filename,
        int chunk_factor,
        int select_factor,
               int api_option,
               int file_selection,
               int mem_selection,
               int mode)
{
  hid_t       file, dataset, file_dataspace, mem_dataspace;
  hid_t    acc_plist,xfer_plist,crp_plist;

  hsize_t  dims[RANK], chunk_dims[RANK];
  int*     data_array1  = NULL;
  int*     data_origin1 = NULL;

  hsize_t  start[RANK],count[RANK],stride[RANK],block[RANK];

#ifdef H5_HAVE_INSTRUMENTED_LIBRARY
  unsigned prop_value;
#endif /* H5_HAVE_INSTRUMENTED_LIBRARY */

  herr_t   status;
  MPI_Comm comm = MPI_COMM_WORLD;
  MPI_Info info = MPI_INFO_NULL;

  size_t  num_points;           /* for point selection */
  hsize_t *coords = NULL;       /* for point selection */
  int i;

  /* Create the data space */

  acc_plist = create_faccess_plist(comm,info,facc_type);
  VRFY((acc_plist >= 0),"");

  file = H5Fcreate(filename,H5F_ACC_TRUNC,H5P_DEFAULT,acc_plist);
  VRFY((file >= 0),"H5Fcreate succeeded");

  status = H5Pclose(acc_plist);
  VRFY((status >= 0),"");

  /* setup dimensionality object */
  dims[0] = space_dim1*mpi_size;
  dims[1] = space_dim2;

  /* allocate memory for data buffer */
  data_array1 = (int *)HDmalloc(dims[0] * dims[1] * sizeof(int));
  VRFY((data_array1 != NULL), "data_array1 malloc succeeded");

  /* set up dimensions of the slab this process accesses */
  ccslab_set(mpi_rank, mpi_size, start, count, stride, block, select_factor);

  /* set up the coords array selection */
  num_points = block[0] * block[1] * count[0] * count[1];
  coords = (hsize_t *)HDmalloc(num_points * RANK * sizeof(hsize_t));
  VRFY((coords != NULL), "coords malloc succeeded");
  point_set(start, count, stride, block, num_points, coords, mode);

  /* Warning: H5Screate_simple requires an array of hsize_t elements
   * even if we only pass only a single value.  Attempting anything else
   * appears to cause problems with 32 bit compilers.
   */
  file_dataspace = H5Screate_simple(2, dims, NULL);
  VRFY((file_dataspace >= 0), "file dataspace created succeeded");

  if(ALL != mem_selection) {
      mem_dataspace = H5Screate_simple(2, dims, NULL);
      VRFY((mem_dataspace >= 0), "mem dataspace created succeeded");
  }
  else {
      /* Putting the warning about H5Screate_simple (above) into practice... */
      hsize_t dsdims[1] = {num_points};
      mem_dataspace = H5Screate_simple (1, dsdims, NULL);
      VRFY((mem_dataspace >= 0), "mem_dataspace create succeeded");
  }

  crp_plist = H5Pcreate(H5P_DATASET_CREATE);
  VRFY((crp_plist >= 0),"");

  /* Set up chunk information.  */
  chunk_dims[0] = dims[0]/chunk_factor;

  /* to decrease the testing time, maintain bigger chunk size */
  (chunk_factor == 1) ? (chunk_dims[1] = space_dim2) : (chunk_dims[1] = space_dim2/2);
  status = H5Pset_chunk(crp_plist, 2, chunk_dims);
  VRFY((status >= 0),"chunk creation property list succeeded");

  dataset = H5Dcreate2(file, DSET_COLLECTIVE_CHUNK_NAME, H5T_NATIVE_INT,
                       file_dataspace, H5P_DEFAULT, crp_plist, H5P_DEFAULT);
  VRFY((dataset >= 0),"dataset created succeeded");

  status = H5Pclose(crp_plist);
  VRFY((status >= 0), "");

  /*put some trivial data in the data array */
  ccdataset_fill(start, stride, count,block, data_array1, mem_selection);

  MESG("data_array initialized");

  switch (file_selection) {
      case HYPER:
          status = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
          VRFY((status >= 0),"hyperslab selection succeeded");
          break;

      case POINT:
          if (num_points) {
              status = H5Sselect_elements(file_dataspace, H5S_SELECT_SET, num_points, coords);
              VRFY((status >= 0),"Element selection succeeded");
          }
          else {
              status = H5Sselect_none(file_dataspace);
              VRFY((status >= 0),"none selection succeeded");
          }
          break;

      case ALL:
          status = H5Sselect_all(file_dataspace);
          VRFY((status >= 0), "H5Sselect_all succeeded");
          break;
  }

  switch (mem_selection) {
      case HYPER:
          status = H5Sselect_hyperslab(mem_dataspace, H5S_SELECT_SET, start, stride, count, block);
          VRFY((status >= 0),"hyperslab selection succeeded");
          break;

      case POINT:
          if (num_points) {
              status = H5Sselect_elements(mem_dataspace, H5S_SELECT_SET, num_points, coords);
              VRFY((status >= 0),"Element selection succeeded");
          }
          else {
              status = H5Sselect_none(mem_dataspace);
              VRFY((status >= 0),"none selection succeeded");
          }
          break;

      case ALL:
          status = H5Sselect_all(mem_dataspace);
          VRFY((status >= 0), "H5Sselect_all succeeded");
          break;
  }

  /* set up the collective transfer property list */
  xfer_plist = H5Pcreate(H5P_DATASET_XFER);
  VRFY((xfer_plist >= 0), "");

  status = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
  VRFY((status>= 0),"MPIO collective transfer property succeeded");
  if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
     status = H5Pset_dxpl_mpio_collective_opt(xfer_plist, H5FD_MPIO_INDIVIDUAL_IO);
     VRFY((status>= 0),"set independent IO collectively succeeded");
  }

  switch(api_option){
    case API_LINK_HARD:
    status = H5Pset_dxpl_mpio_chunk_opt(xfer_plist,H5FD_MPIO_CHUNK_ONE_IO);
           VRFY((status>= 0),"collective chunk optimization succeeded");
           break;

    case API_MULTI_HARD:
    status = H5Pset_dxpl_mpio_chunk_opt(xfer_plist,H5FD_MPIO_CHUNK_MULTI_IO);
    VRFY((status>= 0),"collective chunk optimization succeeded ");
           break;

    case API_LINK_TRUE:
           status = H5Pset_dxpl_mpio_chunk_opt_num(xfer_plist,2);
    VRFY((status>= 0),"collective chunk optimization set chunk number succeeded");
           break;

    case API_LINK_FALSE:
           status = H5Pset_dxpl_mpio_chunk_opt_num(xfer_plist,6);
           VRFY((status>= 0),"collective chunk optimization set chunk number succeeded");
           break;

    case API_MULTI_COLL:
           status = H5Pset_dxpl_mpio_chunk_opt_num(xfer_plist,8);/* make sure it is using multi-chunk IO */
           VRFY((status>= 0),"collective chunk optimization set chunk number succeeded");
    status = H5Pset_dxpl_mpio_chunk_opt_ratio(xfer_plist,50);
           VRFY((status>= 0),"collective chunk optimization set chunk ratio succeeded");
           break;

    case API_MULTI_IND:
           status = H5Pset_dxpl_mpio_chunk_opt_num(xfer_plist,8);/* make sure it is using multi-chunk IO */
           VRFY((status>= 0),"collective chunk optimization set chunk number succeeded");
    status = H5Pset_dxpl_mpio_chunk_opt_ratio(xfer_plist,100);
           VRFY((status>= 0),"collective chunk optimization set chunk ratio succeeded");
           break;

    default:
            ;
   }

#ifdef H5_HAVE_INSTRUMENTED_LIBRARY
  if(facc_type == FACC_MPIO) {
      switch(api_option) {
            case API_LINK_HARD:
               prop_value = H5D_XFER_COLL_CHUNK_DEF;
               status = H5Pinsert2(xfer_plist, H5D_XFER_COLL_CHUNK_LINK_HARD_NAME, H5D_XFER_COLL_CHUNK_SIZE, &prop_value,
                           NULL, NULL, NULL, NULL, NULL, NULL);
               VRFY((status >= 0),"testing property list inserted succeeded");
               break;

            case API_MULTI_HARD:
               prop_value = H5D_XFER_COLL_CHUNK_DEF;
               status = H5Pinsert2(xfer_plist, H5D_XFER_COLL_CHUNK_MULTI_HARD_NAME, H5D_XFER_COLL_CHUNK_SIZE, &prop_value,
                           NULL, NULL, NULL, NULL, NULL, NULL);
               VRFY((status >= 0),"testing property list inserted succeeded");
               break;

            case API_LINK_TRUE:
               prop_value = H5D_XFER_COLL_CHUNK_DEF;
               status = H5Pinsert2(xfer_plist, H5D_XFER_COLL_CHUNK_LINK_NUM_TRUE_NAME, H5D_XFER_COLL_CHUNK_SIZE, &prop_value,
                           NULL, NULL, NULL, NULL, NULL, NULL);
               VRFY((status >= 0),"testing property list inserted succeeded");
               break;

            case API_LINK_FALSE:
               prop_value = H5D_XFER_COLL_CHUNK_DEF;
               status = H5Pinsert2(xfer_plist, H5D_XFER_COLL_CHUNK_LINK_NUM_FALSE_NAME, H5D_XFER_COLL_CHUNK_SIZE, &prop_value,
                           NULL, NULL, NULL, NULL, NULL, NULL);
               VRFY((status >= 0),"testing property list inserted succeeded");
               break;

            case API_MULTI_COLL:
               prop_value = H5D_XFER_COLL_CHUNK_DEF;
               status = H5Pinsert2(xfer_plist, H5D_XFER_COLL_CHUNK_MULTI_RATIO_COLL_NAME, H5D_XFER_COLL_CHUNK_SIZE, &prop_value,
                           NULL, NULL, NULL, NULL, NULL, NULL);
               VRFY((status >= 0),"testing property list inserted succeeded");
               break;

            case API_MULTI_IND:
               prop_value = H5D_XFER_COLL_CHUNK_DEF;
               status = H5Pinsert2(xfer_plist, H5D_XFER_COLL_CHUNK_MULTI_RATIO_IND_NAME, H5D_XFER_COLL_CHUNK_SIZE, &prop_value,
                           NULL, NULL, NULL, NULL, NULL, NULL);
               VRFY((status >= 0),"testing property list inserted succeeded");
               break;

            default:
                ;
       }
   }
#endif

  /* write data collectively */
  status = H5Dwrite(dataset, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
            xfer_plist, data_array1);
  VRFY((status >= 0),"dataset write succeeded");

#ifdef H5_HAVE_INSTRUMENTED_LIBRARY
  if(facc_type == FACC_MPIO) {
      switch(api_option){
            case API_LINK_HARD:
               status = H5Pget(xfer_plist,H5D_XFER_COLL_CHUNK_LINK_HARD_NAME,&prop_value);
               VRFY((status >= 0),"testing property list get succeeded");
               VRFY((prop_value == 0),"API to set LINK COLLECTIVE IO directly succeeded");
               break;

            case API_MULTI_HARD:
               status = H5Pget(xfer_plist,H5D_XFER_COLL_CHUNK_MULTI_HARD_NAME,&prop_value);
               VRFY((status >= 0),"testing property list get succeeded");
               VRFY((prop_value == 0),"API to set MULTI-CHUNK COLLECTIVE IO optimization succeeded");
               break;

            case API_LINK_TRUE:
               status = H5Pget(xfer_plist,H5D_XFER_COLL_CHUNK_LINK_NUM_TRUE_NAME,&prop_value);
               VRFY((status >= 0),"testing property list get succeeded");
               VRFY((prop_value == 0),"API to set LINK COLLECTIVE IO succeeded");
               break;

            case API_LINK_FALSE:
               status = H5Pget(xfer_plist,H5D_XFER_COLL_CHUNK_LINK_NUM_FALSE_NAME,&prop_value);
               VRFY((status >= 0),"testing property list get succeeded");
               VRFY((prop_value == 0),"API to set LINK IO transferring to multi-chunk IO succeeded");
               break;

            case API_MULTI_COLL:
               status = H5Pget(xfer_plist,H5D_XFER_COLL_CHUNK_MULTI_RATIO_COLL_NAME,&prop_value);
               VRFY((status >= 0),"testing property list get succeeded");
               VRFY((prop_value == 0),"API to set MULTI-CHUNK COLLECTIVE IO with optimization succeeded");
               break;

            case API_MULTI_IND:
               status = H5Pget(xfer_plist,H5D_XFER_COLL_CHUNK_MULTI_RATIO_IND_NAME,&prop_value);
               VRFY((status >= 0),"testing property list get succeeded");
               VRFY((prop_value == 0),"API to set MULTI-CHUNK IO transferring to independent IO  succeeded");
               break;

            default:
            ;
       }
   }
#endif

  status = H5Dclose(dataset);
  VRFY((status >= 0),"");

  status = H5Pclose(xfer_plist);
  VRFY((status >= 0),"property list closed");

  status = H5Sclose(file_dataspace);
  VRFY((status >= 0),"");

  status = H5Sclose(mem_dataspace);
  VRFY((status >= 0),"");


  status = H5Fclose(file);
  VRFY((status >= 0),"");

  if (data_array1) HDfree(data_array1);

  /* Use collective read to verify the correctness of collective write. */

  /* allocate memory for data buffer */
  data_array1 = (int *)HDmalloc(dims[0]*dims[1]*sizeof(int));
  VRFY((data_array1 != NULL), "data_array1 malloc succeeded");

  /* allocate memory for data buffer */
  data_origin1 = (int *)HDmalloc(dims[0]*dims[1]*sizeof(int));
  VRFY((data_origin1 != NULL), "data_origin1 malloc succeeded");

  acc_plist = create_faccess_plist(comm, info, facc_type);
  VRFY((acc_plist >= 0),"MPIO creation property list succeeded");

  file = H5Fopen(FILENAME[0],H5F_ACC_RDONLY,acc_plist);
  VRFY((file >= 0),"H5Fcreate succeeded");

  status = H5Pclose(acc_plist);
  VRFY((status >= 0),"");

  /* open the collective dataset*/
  dataset = H5Dopen2(file, DSET_COLLECTIVE_CHUNK_NAME, H5P_DEFAULT);
  VRFY((dataset >= 0), "");

  /* set up dimensions of the slab this process accesses */
  ccslab_set(mpi_rank, mpi_size, start, count, stride, block, select_factor);

  /* obtain the file and mem dataspace*/
  file_dataspace = H5Dget_space (dataset);
  VRFY((file_dataspace >= 0), "");

  if (ALL != mem_selection) {
      mem_dataspace = H5Dget_space (dataset);
      VRFY((mem_dataspace >= 0), "");
  }
  else {
      /* Warning: H5Screate_simple requires an array of hsize_t elements
       * even if we only pass only a single value.  Attempting anything else
       * appears to cause problems with 32 bit compilers.
       */
      hsize_t dsdims[1] = {num_points};
      mem_dataspace = H5Screate_simple (1, dsdims, NULL);
      VRFY((mem_dataspace >= 0), "mem_dataspace create succeeded");
  }

  switch (file_selection) {
      case HYPER:
          status = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
          VRFY((status >= 0),"hyperslab selection succeeded");
          break;

      case POINT:
          if (num_points) {
              status = H5Sselect_elements(file_dataspace, H5S_SELECT_SET, num_points, coords);
              VRFY((status >= 0),"Element selection succeeded");
          }
          else {
              status = H5Sselect_none(file_dataspace);
              VRFY((status >= 0),"none selection succeeded");
          }
          break;

      case ALL:
          status = H5Sselect_all(file_dataspace);
          VRFY((status >= 0), "H5Sselect_all succeeded");
          break;
  }

  switch (mem_selection) {
      case HYPER:
          status = H5Sselect_hyperslab(mem_dataspace, H5S_SELECT_SET, start, stride, count, block);
          VRFY((status >= 0),"hyperslab selection succeeded");
          break;

      case POINT:
          if (num_points) {
              status = H5Sselect_elements(mem_dataspace, H5S_SELECT_SET, num_points, coords);
              VRFY((status >= 0),"Element selection succeeded");
          }
          else {
              status = H5Sselect_none(mem_dataspace);
              VRFY((status >= 0),"none selection succeeded");
          }
          break;

      case ALL:
          status = H5Sselect_all(mem_dataspace);
          VRFY((status >= 0), "H5Sselect_all succeeded");
          break;
  }

  /* fill dataset with test data */
  ccdataset_fill(start, stride,count,block, data_origin1, mem_selection);
  xfer_plist = H5Pcreate (H5P_DATASET_XFER);
  VRFY((xfer_plist >= 0),"");

  status = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
  VRFY((status>= 0),"MPIO collective transfer property succeeded");
  if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
     status = H5Pset_dxpl_mpio_collective_opt(xfer_plist,H5FD_MPIO_INDIVIDUAL_IO);
     VRFY((status>= 0),"set independent IO collectively succeeded");
  }

  status = H5Dread(dataset, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
                   xfer_plist, data_array1);
  VRFY((status >=0),"dataset read succeeded");

  /* verify the read data with original expected data */
  status = ccdataset_vrfy(start, count, stride, block, data_array1, data_origin1, mem_selection);
  if (status) nerrors++;

  status = H5Pclose(xfer_plist);
  VRFY((status >= 0),"property list closed");

  /* close dataset collectively */
  status=H5Dclose(dataset);
  VRFY((status >= 0), "H5Dclose");

  /* release all IDs created */
  status = H5Sclose(file_dataspace);
  VRFY((status >= 0),"H5Sclose");

  status = H5Sclose(mem_dataspace);
  VRFY((status >= 0),"H5Sclose");

  /* close the file collectively */
  status = H5Fclose(file);
  VRFY((status >= 0),"H5Fclose");

  /* release data buffers */
  if(coords) HDfree(coords);
  if(data_array1) HDfree(data_array1);
  if(data_origin1) HDfree(data_origin1);

}



/*****************************************************************************
 *
 * Function:    do_express_test()
 *
 * Purpose:    Do an MPI_Allreduce to obtain the maximum value returned
 *         by GetTestExpress() across all processes.  Return this
 *         value.
 *
 *         Envirmoment variables can be different across different
 *         processes.  This function ensures that all processes agree
 *         on whether to do an express test.
 *
 * Return:    Success:    Maximum of the values returned by
 *                 GetTestExpress() across    all processes.
 *
 *        Failure:    -1
 *
 * Programmer:    JRM -- 4/25/06
 *
 *****************************************************************************/
static int
do_express_test(int world_mpi_rank)
{
    int express_test;
    int max_express_test;
    int result;

    express_test = GetTestExpress();

    result = MPI_Allreduce((void *)&express_test,
                           (void *)&max_express_test,
                           1,
                           MPI_INT,
                           MPI_MAX,
                           MPI_COMM_WORLD);

    if ( result != MPI_SUCCESS ) {
        nerrors++;
        max_express_test = -1;
        if ( VERBOSE_MED && (world_mpi_rank == 0)) {
            HDfprintf(stdout, "%d:%s: MPI_Allreduce() failed.\n",
                      world_mpi_rank, FUNC );
        }
    }

    return(max_express_test);

} /* do_express_test() */


int main(int argc, char **argv)
{
    int ExpressMode = 0;
    hsize_t newsize = 1048576;
    /* Set the bigio processing limit to be 'newsize' bytes */
    hsize_t oldsize = H5S_mpio_set_bigio_count(newsize);

    /* Having set the bigio handling to a size that is managable,
     * we'll set our 'bigcount' variable to be 2X that limit so
     * that we try to ensure that our bigio handling is actually
     * envoked and tested.
     */
    if (newsize != oldsize) {
       bigcount = newsize * 2;
    }

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);

    /* Attempt to turn off atexit post processing so that in case errors
     * happen during the test and the process is aborted, it will not get
     * hang in the atexit post processing in which it may try to make MPI
     * calls.  By then, MPI calls may not work.
     */
    if (H5dont_atexit() < 0){
    HDprintf("Failed to turn off atexit processing. Continue.\n");
    };

    /* set alarm. */
    ALARM_ON;

    ExpressMode = do_express_test(mpi_rank);

    dataset_big_write();
    MPI_Barrier(MPI_COMM_WORLD);

    dataset_big_read();
    MPI_Barrier(MPI_COMM_WORLD);

    if (ExpressMode > 0) {
      if (mpi_rank == 0)
          HDprintf("***Express test mode on.  Several tests are skipped\n");
    }
    else {
      coll_chunk1();
      MPI_Barrier(MPI_COMM_WORLD);
      coll_chunk2();
      MPI_Barrier(MPI_COMM_WORLD);
      coll_chunk3();
    }

    /* turn off alarm */
    ALARM_OFF;

    if (mpi_rank == 0)
        HDremove(FILENAME[0]);

    /* close HDF5 library */
    H5close();

    MPI_Finalize();

    return 0;
}

