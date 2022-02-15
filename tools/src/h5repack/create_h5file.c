#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>
#include <time.h>
#include <hdf5.h>

#define NAME_LENGTH 1024
#define ATTR_RANK   1
#define ONE_RANK    1
#define DSET_RANK   2
#define FILENAME    "test_h5repack.h5"

typedef struct {
    unsigned int ngroups;
    unsigned int nattrs;
    unsigned int ndsets;
    bool         empty_dsets;
    unsigned int dset_rows, dset_cols;
    unsigned int multiply;
    unsigned int attr_dim;
    char *       filename;
} handler_t;

static handler_t hand;
int *            attr_write;

/* Show command usage */
static void
usage(void)
{
    printf("    [-h] [-a] [-c] [-d] [-D] [-e] [-g] [-l] [-m] [-n] [-r]\n\n");

    printf("    [-h]: this help page\n");
    printf("    [-a]: number of attributes for each group (default is 0)\n");
    printf("    [-c]: number of columns of the smaller datasets (default is 4)\n");
    printf("    [-d]: number of smaller datasets (integer type) under the rot group (default is 0) of which "
           "the row & column are set through -r & -c\n");
    printf("    [-e]: empty datasets (no data in the datasets)\n");
    printf("    [-g]: number of groups under the root group (default is 0)\n");
    printf("    [-l]: number of elements (single dimension only) for each attribute (default is 1)\n");
    printf(
        "    [-m]: multiplication factor for dataset dimensions which is applied randomly (default is 1)\n");
    printf("    [-n]: name of the file\n");
    printf("    [-r]: number of rows of the smaller datasets (default is 4)\n");

    printf("\n");
    printf("    example of usage: ./a.out -c 10 -r 20 -g 4 -d 1 -a 2 -l 4\n");
    printf("\n");
}

static bool
state_init(int argc, char **argv)
{
    int opt;
    int i;

    hand.ngroups     = 0;
    hand.nattrs      = 0;
    hand.ndsets      = 0;
    hand.empty_dsets = false;
    hand.dset_rows   = 4;
    hand.dset_cols   = 4;
    hand.multiply    = 1;
    hand.attr_dim    = 1;
    hand.filename    = strdup(FILENAME);

    while ((opt = getopt(argc, argv, "a:c:d:eg:hl:m:n:r:")) != -1) {
        switch (opt) {
            case 'a':
                if (optarg) {
                    hand.nattrs = atoi(optarg);
                    fprintf(stdout, "number of attributes for each group:\t\t%u\n", hand.nattrs);
                }
                else
                    printf("optarg is null\n");

                break;
            case 'c':
                if (optarg) {
                    hand.dset_cols = atoi(optarg);
                    fprintf(stdout, "dataset columns:\t\t\t\t%u\n", hand.dset_cols);
                }
                else
                    printf("optarg is null\n");

                break;
            case 'd':
                if (optarg) {
                    hand.ndsets = atoi(optarg);
                    fprintf(stdout, "number of datasets:\t\t\t\t%u\n", hand.ndsets);
                }
                else
                    printf("optarg is null\n");

                break;
            case 'e':
                hand.empty_dsets = true;
                fprintf(stdout, "empty datasets:\t\t\t\t\ttrue\n");

                break;
            case 'g':
                if (optarg) {
                    hand.ngroups = atoi(optarg);
                    fprintf(stdout, "number of groups:\t\t\t\t%u\n", hand.ngroups);
                }
                else
                    printf("optarg is null\n");

                break;
            case 'h':
                usage();
                exit(0);

                break;
            case 'l':
                if (optarg) {
                    hand.attr_dim = atoi(optarg);
                    fprintf(stdout, "dimension of attributes:\t\t\t%u\n", hand.attr_dim);
                }
                else
                    printf("optarg is null\n");

                break;
            case 'm':
                if (optarg) {
                    hand.multiply = atoi(optarg);
                    fprintf(stdout, "random multiply factor for dataset dimensions:\t%u\n", hand.multiply);
                }
                else
                    printf("optarg is null\n");

                break;
            case 'n':
                if (optarg) {
                    if (hand.filename)
                        free(hand.filename);
                    hand.filename = strdup(optarg);
                }
                else
                    printf("optarg is null\n");

                break;
            case 'r':
                if (optarg) {
                    hand.dset_rows = atoi(optarg);
                    fprintf(stdout, "dataset rows:\t\t\t\t\t%u\n", hand.dset_rows);
                }
                else
                    printf("optarg is null\n");

                break;
            case '?':
            default:
                usage();
                exit(0);

                break;
        }
    }

    /* Allocate the memory for the attribute write */
    if (hand.attr_dim) {
        attr_write = (int *)malloc((size_t)hand.attr_dim * sizeof(int));

        /* Initialize the data for the attribute */
        for (i = 0; i < hand.attr_dim; i++)
            attr_write[i] = i;
    }

    return true;

error:
    return false;
}

static bool
create_attributes(hid_t group_id)
{
    char    attr_name[NAME_LENGTH];
    hsize_t attr_dim[ATTR_RANK];
    hid_t   attr_space, attr_id;
    int     i;

    attr_dim[0] = (hsize_t)hand.attr_dim;

    /* This step could happen earlier for better performance */
    if ((attr_space = H5Screate_simple(ATTR_RANK, attr_dim, NULL)) < 0) {
        fprintf(stderr, "failed to create memory space for attributes\n");
        goto error;
    }

    for (i = 0; i < hand.nattrs; i++) {
        sprintf(attr_name, "attr_%d", i + 1);

        if ((attr_id =
                 H5Acreate2(group_id, attr_name, H5T_NATIVE_INT, attr_space, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
            fprintf(stderr, "failed to create attribute '%s'\n", attr_name);
            goto error;
        }

        if (H5Awrite(attr_id, H5T_NATIVE_INT, attr_write) < 0) {
            fprintf(stderr, "failed to write data to the attribute: %s\n", attr_name);
            goto error;
        }

        if (H5Aclose(attr_id) < 0) {
            fprintf(stderr, "failed to close the attribure\n");
            goto error;
        }
    }

    if (H5Sclose(attr_space) < 0) {
        fprintf(stderr, "failed to close the attribure space\n");
        goto error;
    }

    return true;

error:
    return false;
}

static bool
create_groups(hid_t file_id)
{
    int     i, j;
    hid_t   gid;
    char    gname[NAME_LENGTH];
    hsize_t attr_dim[ATTR_RANK];

    for (i = 0; i < hand.ngroups; i++) {
        sprintf(gname, "group_%d", i + 1);

        if ((gid = H5Gcreate2(file_id, gname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
            fprintf(stderr, "couldn't create group '%s'\n", gname);
            goto error;
        }

        if (hand.nattrs && !create_attributes(gid)) {
            fprintf(stderr, "couldn't create attributes for group '%s'\n", gname);
            goto error;
        }

        if (H5Gclose(gid) < 0) {
            fprintf(stderr, "failed to close the file\n");
            goto error;
        }
    }

    return true;

error:
    return false;
}

static bool
create_dsets(hid_t file_id)
{
    int          i, j, k;
    hid_t        dset_id, dset_space, dcpl_id;
    char         dset_name[NAME_LENGTH];
    hsize_t      dimsf[DSET_RANK]; /* dataset dimensions */
    int *        dset_data      = NULL;
    unsigned int multiplication = 1;

    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
        fprintf(stderr, "failed to create the dataset creation property list\n");
        goto error;
    }

    /* Allocate space early for testing the parallel h5repack */
    if ((H5Pset_alloc_time(dcpl_id, H5D_ALLOC_TIME_EARLY) < 0)) {
        fprintf(stderr, "failed to create the dataset creation property list\n");
        goto error;
    }

    for (k = 0; k < hand.ndsets; k++) {
        sprintf(dset_name, "dset_%d", k + 1);

        multiplication = rand() % 2 ? hand.multiply : 1;

        if (hand.dset_rows == 1) {
            dimsf[0] = (hsize_t)hand.dset_cols * multiplication;

            if ((dset_space = H5Screate_simple(ONE_RANK, dimsf, NULL)) < 0) {
                fprintf(stderr, "failed to create the data space\n");
                goto error;
            }
        }
        else {
            dimsf[0] = (hsize_t)hand.dset_rows * multiplication;
            dimsf[1] = (hsize_t)hand.dset_cols * multiplication;

            if ((dset_space = H5Screate_simple(DSET_RANK, dimsf, NULL)) < 0) {
                fprintf(stderr, "failed to create the data space\n");
                goto error;
            }
        }

        dset_data = (int *)malloc(dimsf[0] * dimsf[1] * sizeof(int));

        if (hand.dset_rows == 1) {
            for (i = 0; i < dimsf[0]; i++)
                *(dset_data + i) = i;
        }
        else {
            for (i = 0; i < dimsf[0]; i++)
                for (j = 0; j < dimsf[1]; j++)
                    *(dset_data + i * dimsf[1] + j) = i + j;
        }

        if ((dset_id = H5Dcreate2(file_id, dset_name, H5T_NATIVE_INT, dset_space, H5P_DEFAULT, dcpl_id,
                                  H5P_DEFAULT)) < 0) {
            fprintf(stderr, "couldn't create dataset '%s'\n", dset_name);
            goto error;
        }

        if (!hand.empty_dsets &&
            H5Dwrite(dset_id, H5T_NATIVE_INT, dset_space, dset_space, H5P_DEFAULT, dset_data) < 0) {
            fprintf(stderr, "couldn't write data for dataset '%s'\n", dset_name);
            goto error;
        }

        if (H5Dclose(dset_id) < 0) {
            fprintf(stderr, "failed to close the dataset '%s'\n", dset_name);
            goto error;
        }

        if (H5Sclose(dset_space) < 0) {
            fprintf(stderr, "failed to close the dataset space\n");
            goto error;
        }

        if (dset_data)
            free(dset_data);
    }

    if (H5Pclose(dcpl_id) < 0) {
        fprintf(stderr, "failed to close the dataset creation property list\n");
        goto error;
    }

    return true;

error:
    return false;
}

static void
release_resources()
{
    if (hand.nattrs && attr_write)
        free(attr_write);

    if (hand.filename)
        free(hand.filename);
}

int
main(int argc, char **argv)
{
    hid_t file_id;

    /* seed for random generator */
    srand(time(NULL));

    if (!state_init(argc, argv)) {
        fprintf(stderr, "state_init failed\n");
        goto error;
    }

    if ((file_id = H5Fcreate(hand.filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        fprintf(stderr, "failed to create the file '%s'\n", hand.filename);
        goto error;
    }

    if (hand.ngroups && !create_groups(file_id)) {
        fprintf(stderr, "create_groups failed\n");
        goto error;
    }

    if (hand.ndsets && !create_dsets(file_id)) {
        fprintf(stderr, "create_dsets failed\n");
        goto error;
    }

    if (H5Fclose(file_id) < 0) {
        fprintf(stderr, "failed to close the file\n");
        goto error;
    }

    release_resources();

    return 0;

error:
    return 1;
}
