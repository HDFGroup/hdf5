/* 
 * h5ff_client_analysis.c: Client side test for analysis shipping routines.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include <mpi.h>
#include <hdf5.h>

/* define filename for this app, and max size after username prepended */
#define FILENAME_APP "eff_analysis.h5"
#define NAME_SIZE 64

/* macros related to error reporting */
#define STATUS (ret >= 0) ? " " : " - FAILED"
#define ASSERT_RET assert( ret >= 0 )

#define NTUPLES 512

#define DATASET_POSITION    "Position"
#define DATASET_VELOCITY    "Velocity"
#define DATASET_PRESSURE    "Pressure"
#define DATASET_TEMPERATURE "Temperature"

static int my_rank = 0, my_size = 1;

struct analysis_data {
    size_t ntuples;
    hid_t type_id;
    double *position;
    double *velocity;
    double *pressure;
    double *temperature;
};

/* Local sum and global sum */
const char *split_script =
        "import numpy as np\n"
        "def split(array):\n"
        "  print '--------------------'\n"
        "  print 'Split sum: ' + str(array.sum())\n"
        "  print 'Split average: ' + str(np.average(array))\n"
        "  print '--------------------'\n"
        "  return np.array([array.sum(), np.average(array)])\n";

const char *combine_script =
        "import numpy as np\n"
        "def combine(arrays):\n"
        "  global_sum = 0\n"
        "  global_average = 0\n"
        "  for a in arrays:\n"
        "    global_sum += a[0]\n"
        "    global_average += a[1]\n"
        "  global_average /= len(arrays)\n"
        "  print '--------------------'\n"
        "  print 'Combined sum: ' + str(global_sum)\n"
        "  print 'Combined average: ' + str(global_average)\n"
        "  print '--------------------'\n"
        "  return np.array([global_sum, global_average])\n";

const char *integrate_script =
        "import numpy as np\n"
        "def integrate(arrays):\n"
        "  global_sum = 0\n"
        "  global_average = 0\n"
        "  for a in arrays:\n"
        "    global_sum += a[0]\n"
        "    global_average += a[1]\n"
        "  global_average /= len(arrays)\n"
        "  print '--------------------'\n"
        "  print 'Integrate sum: ' + str(global_sum)\n"
        "  print 'Integrate average: ' + str(global_average)\n"
        "  print '--------------------'\n"
        "  return np.array([global_sum, global_average])\n";

static void
write_dataset(hid_t group_id, const char *dataset_name,
        hsize_t ntuples, hsize_t ncomponents, hid_t datatype_id,
        void *buf, hid_t trans_id, hid_t estack_id)
{
    hid_t       dataset_id;
    hid_t       space_id;
    hsize_t     dims[2] = {ntuples, ncomponents};
    int         rank = (ncomponents == 1) ? 1 : 2;
    herr_t      ret;

    /* Create the data space for the first dataset. */
    space_id = H5Screate_simple(rank, dims, NULL);
    assert(space_id);

    /* Create a dataset. */
    dataset_id = H5Dcreate_ff(group_id, dataset_name, datatype_id, space_id,
            H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, trans_id, estack_id);
    assert(dataset_id);

    /* Write the first dataset. */
    ret = H5Dwrite_ff(dataset_id, datatype_id, H5S_ALL, H5S_ALL,
            H5P_DEFAULT, buf, trans_id, estack_id);
    assert(0 == ret);

    /* Close the first dataset. */
    ret = H5Dclose_ff(dataset_id, estack_id);
    assert(0 == ret);

    /* Close dataspace */
    ret = H5Sclose(space_id);
    assert(0 == ret);
}


static void
init_data(size_t ntuples, struct analysis_data *data)
{
    size_t i,j;
    size_t ncomponents;

    data->type_id = H5T_NATIVE_DOUBLE;
    data->ntuples = ntuples;

    /* Initialize position */
    ncomponents = 3;
    data->position = (double *) malloc(sizeof(double) * ncomponents * ntuples);
    for (i = 0; i < ntuples; i++) {
       for (j = 0; j < ncomponents; j++) {
           data->position[ncomponents * i + j] =
                   (double) (((hsize_t) my_rank) * ntuples + i);
       }
    }

    /* Initialize velocity */
    data->velocity = (double *) malloc(sizeof(double) * ncomponents * ntuples);
    for (i = 0; i < ntuples; i++) {
       for (j = 0; j < ncomponents; j++) {
           data->velocity[ncomponents * i + j] =
                   (double) (((hsize_t) my_rank) * ntuples + i);
       }
    }

    /* Initialize pressure */
    ncomponents = 1;
    data->pressure = (double *) malloc(sizeof(double) * ncomponents * ntuples);
    for (i = 0; i < ntuples; i++) {
       for (j = 0; j < ncomponents; j++) {
           data->pressure[ncomponents * i + j] =
                   (double) (((hsize_t) my_rank) * ntuples + i);
       }
    }

    /* Initialize temperature */
    data->temperature = (double *) malloc(sizeof(double) * ncomponents * ntuples);
    for (i = 0; i < ntuples; i++) {
       for (j = 0; j < ncomponents; j++) {
           data->temperature[ncomponents * i + j] = i;
           //(double) (((hsize_t) my_rank) * ntuples + i);
       }
    }
}

static void
free_data(struct analysis_data *data)
{
    free(data->position);
    free(data->velocity);
    free(data->pressure);
    free(data->temperature);
}

static void
write_data(hid_t file_id, struct analysis_data data, hid_t estack_id)
{
    hid_t       group_id;
    hid_t       trans_id, rcxt_id, trspl_id;
    uint64_t    version = 1;
    herr_t      ret;
    char group_name[NAME_SIZE];
    hsize_t ncomponents;

    /* Every rank creates Group_"my_rank" */
    sprintf(group_name, "Group_%d", my_rank);

    /* acquire container version 1 - EXACT. */
    if(0 == my_rank) {
        rcxt_id = H5RCacquire(file_id, &version, H5P_DEFAULT, estack_id);
    } else {
        rcxt_id = H5RCcreate(file_id, version);
    }
    assert(1 == version);

    /* create transaction object */
    trans_id = H5TRcreate(file_id, rcxt_id, version + 1);
    assert(trans_id);

    trspl_id = H5Pcreate(H5P_TR_START);
    ret = H5Pset_trspl_num_peers(trspl_id, (unsigned int) my_size);
    assert(0 == ret);
    ret = H5TRstart(trans_id, trspl_id, estack_id);
    assert(0 == ret);
    ret = H5Pclose(trspl_id);
    assert(0 == ret);

    group_id = H5Gcreate_ff(file_id, group_name, H5P_DEFAULT, H5P_DEFAULT,
            H5P_DEFAULT, trans_id, estack_id);
    assert(group_id > 0);

    /* Write datasets */
    ncomponents = 3;
    write_dataset(group_id, DATASET_POSITION, data.ntuples, ncomponents,
            data.type_id, data.position, trans_id, estack_id);
    write_dataset(group_id, DATASET_VELOCITY, data.ntuples, ncomponents,
            data.type_id, data.velocity, trans_id, estack_id);

    ncomponents = 1;
    write_dataset(group_id, DATASET_PRESSURE, data.ntuples, ncomponents,
            data.type_id, data.pressure, trans_id, estack_id);
    write_dataset(group_id, DATASET_TEMPERATURE, data.ntuples, ncomponents,
            data.type_id, data.temperature, trans_id, estack_id);

    ret = H5Gclose_ff(group_id, estack_id);
    assert(0 == ret);

    /* Finish transaction 0. */
    ret = H5TRfinish(trans_id, H5P_DEFAULT, NULL, estack_id);
    assert(0 == ret);

    /* release container version 0. */
    if (my_rank == 0) {
        ret = H5RCrelease(rcxt_id, estack_id);
        assert(0 == ret);
    }

    ret = H5RCclose(rcxt_id);
    assert(0 == ret);

    ret = H5TRclose(trans_id);
    assert(0 == ret);
}

static void
ship_analysis(const char *file_name)
{
    double lb = 39.1, ub = 42.1;
    hid_t  query_id1, query_id2, query_id3, query_id4;
    hid_t query_id;
    herr_t ret;

    /* Create a simple query */
    /* query = (39.1 < x < 42.1) AND (name = "Temperature") */
    query_id1 = H5Qcreate(H5Q_TYPE_DATA_ELEM, H5Q_MATCH_GREATER_THAN,
            H5T_NATIVE_DOUBLE, &lb);
    assert(query_id1);

    query_id2 = H5Qcreate(H5Q_TYPE_DATA_ELEM, H5Q_MATCH_LESS_THAN,
            H5T_NATIVE_DOUBLE, &ub);
    assert(query_id2);

    query_id3 = H5Qcombine(query_id1, H5Q_COMBINE_AND, query_id2);
    assert(query_id3);

    query_id4 = H5Qcreate(H5Q_TYPE_LINK_NAME, H5Q_MATCH_EQUAL,
            DATASET_TEMPERATURE);
    assert(query_id4);

    query_id = H5Qcombine(query_id3, H5Q_COMBINE_AND, query_id4);
    assert(query_id);

    /* Issue an anlysis shipping request */
    ret = H5ASinvoke(file_name, query_id, split_script, combine_script,
            integrate_script, H5_EVENT_STACK_NULL);
    assert(0 == ret);

    H5Qclose(query_id);
    H5Qclose(query_id4);
    H5Qclose(query_id3);
    H5Qclose(query_id2);
    H5Qclose(query_id1);
}

int
main(int argc, char **argv)
{
    const char *user_name;
    char file_name[NAME_SIZE];
    hid_t file_id, fapl_id;
    hid_t estack_id = H5_EVENT_STACK_NULL;
    herr_t ret;
    H5ES_status_t status;
    size_t num_events = 0;
    int c;
    extern char *optarg;
    struct analysis_data data;
    size_t ntuples = NTUPLES;
    int only_analysis = 0;

    getchar();
    /* Prepend user name */
    user_name = getenv("USER");
    snprintf(file_name, NAME_SIZE, "%s_%s", user_name, FILENAME_APP);

    MPI_Init(&argc, &argv);

    /* Call EFF_init to initialize the EFF stack. */
    EFF_init(MPI_COMM_WORLD, MPI_INFO_NULL);

    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &my_size);
    fprintf(stderr, "APP processes = %d, my rank is %d\n", my_size, my_rank);

    while ((c = getopt (argc, argv, "a")) != -1) {
        switch (c) {
        case 'a':
            only_analysis = 1;
            break;
        default:
            abort ();
        }
    }

    MPI_Barrier(MPI_COMM_WORLD);

    if(only_analysis) {
        goto analysis;
    }

    /* Choose the IOD VOL plugin to use with this file. */
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_fapl_iod(fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL);

    /* create an event Queue for managing asynchronous requests. */
    estack_id = H5EScreate();
    assert(estack_id);

    /* Open an existing file. */
    file_id = H5Fcreate_ff(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id,
            H5_EVENT_STACK_NULL);

    ret = H5Pclose(fapl_id);
    assert(0 == ret);

    init_data(ntuples, &data);

    write_data(file_id, data, estack_id);

    H5ESget_count(estack_id, &num_events);
    H5ESwait_all(estack_id, &status);
    H5ESclear(estack_id);
    printf("%zu events in event stack. Completion status = %d\n", num_events, status);
    assert(status == H5ES_STATUS_SUCCEED);

    MPI_Barrier(MPI_COMM_WORLD);

    free_data(&data);
    ret = H5ESclose(estack_id);
    assert(ret == 0);

    /* Close the file. */
    ret = H5Fclose_ff(file_id, 1, H5_EVENT_STACK_NULL);
    assert(0 == ret);
    MPI_Barrier(MPI_COMM_WORLD);

analysis:
    if(0 == my_rank) {
        ship_analysis(file_name);
    }

    MPI_Barrier(MPI_COMM_WORLD);
    EFF_finalize();
    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Finalize();
    return 0;
}
