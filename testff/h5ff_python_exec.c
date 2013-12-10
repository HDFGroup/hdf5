#include "hdf5.h"
#include <Python.h>

#include <stdio.h>
#include <assert.h>

#define SPLIT_COUNT 5
#define ARRAY_SIZE 10

/* Keep that for debug */
const char *split_script =
        "def split(in_array):\n"
        "  print 'executing split'\n"
        "  print type(in_array)\n"
        "  print in_array.shape\n"
        "  print in_array.dtype\n"
        "  print in_array.size\n"
        "  print in_array.nbytes\n"
        "  print in_array[0]\n"
        "  print in_array[9]\n"
        "  return in_array.copy()\n";

const char *combine_script =
        "def combine(arrays):\n"
        "  print 'executing combine'\n"
        "  print 'size of list ' + str(len(arrays))\n"
        "  for a in enumerate(arrays):\n"
        "    print a[0]\n"
        "  return arrays[0].copy()\n";

/* Local sum and global sum */
const char *split_sum_script =
        "import numpy as np\n"
        "def split(array):\n"
        "  print 'Split sum: ' + str(array.sum())\n"
        "  return np.array([array.sum(), array.size])\n";

const char *combine_sum_script =
        "import numpy as np\n"
        "def combine(arrays):\n"
        "  global_sum = 0\n"
        "  for a in arrays:\n"
        "    global_sum += a[0]\n"
        "  print 'Combined sum: ' + str(global_sum)\n"
        "  return np.array([global_sum, len(arrays)])\n";

/*
 * Checks that analysis python scripts can be executed
 */
int
main(int argc, char *argv[])
{
    int *split_data[SPLIT_COUNT];
    size_t split_data_size[SPLIT_COUNT];
    hid_t *split_data_type_id[SPLIT_COUNT];
    int *combine_data;
    size_t combine_data_size;
    hid_t *combine_data_type_id;
    int i;
    int ret = EXIT_SUCCESS;

    Py_Initialize();

    for (i = 0; i < SPLIT_COUNT; i++) {
        int array[ARRAY_SIZE];
        int j;

        for (j = 0; j < ARRAY_SIZE; j++) {
            array[j] = i + 1;
        }

        if (0 != H5VL__iod_split(split_sum_script, array, ARRAY_SIZE, H5T_NATIVE_INT,
                &split_data[i], &split_data_size[i], &split_data_type_id[i])) {
            ret = EXIT_FAILURE;
            goto done;
        }
    }

    if (0 != H5VL__iod_combine(combine_sum_script, split_data, split_data_size,
            SPLIT_COUNT, split_data_type_id[0], &combine_data, &combine_data_size,
            &combine_data_type_id)) {
        ret = EXIT_FAILURE;
        goto done;
    }

done:
    Py_Finalize();
    return ret;
}
