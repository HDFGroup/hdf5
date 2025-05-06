/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Generate the binary hdf5 files for the h5dump tests.
 * Usage: just execute the program without any arguments will
 * generate all the binary hdf5 files in the local directory.
 *
 * If you regenerate the test files (e.g., changing some code,
 * trying it on a new platform, ...), you need to verify the correctness
 * of the expected output and update the corresponding *.ddl files.
 */
#include "hdf5.h"
#include "H5private.h"
#include "h5dumpgentest.h"

/*-------------------------------------------------------------------------
 * Function: main
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    gent_group();
    gent_attribute();
    gent_softlink();
    gent_softlink2();
    gent_dataset();
    gent_hardlink();
    gent_extlink();
    gent_udlink();
    gent_compound_dt();
    gent_all();
    gent_loop();
    gent_dataset2();
    gent_compound_dt2();
    gent_loop2();
    gent_many();
    gent_str();
    gent_str2();
    gent_enum();
    gent_objref();
    gent_datareg();
    gent_attrreg();
    gent_nestcomp();
    gent_opaque();
    gent_bitfields();
    gent_vldatatypes();
    gent_vldatatypes2();
    gent_vldatatypes3();
    gent_vldatatypes4();
    gent_vldatatypes5();
    gent_array1_big();
    gent_array1();
    gent_array2();
    gent_array3();
    gent_array4();
    gent_array5();
    gent_array6();
    gent_array7();
    gent_array8();
    gent_empty();
    gent_group_comments();
    gent_split_file();
    gent_family();
    gent_multi();
    gent_large_objname();
    gent_vlstr();
    gent_vlenstr_array();
    gent_char();
    gent_attr_all();
    gent_compound_complex();
    gent_compound_complex2();
    gent_named_dtype_attr();
    gent_null_space();
    gent_zero_dim_size();

    gent_filters();
    gent_fvalues();
    gent_fcontents();
    gent_string();
    gent_aindices();
    gent_longlinks();
    gent_ldouble();
    gent_ldouble_scalar();
    gent_binary();
    gent_bigdims();
    gent_hyperslab();
    gent_group_creation_order();
    gent_attr_creation_order();
    gent_fpformat();
    gent_extlinks();
    gent_fs_strategy_threshold();
    gent_packedbits();
    gent_dataset_idx();
    gent_attr_intsize();
    gent_charsets();
    gent_compound_intsizes();
    gent_compound_attr_intsizes();

    gent_nested_compound_dt();
    gent_intscalars();
    gent_attr_intscalars();
    gent_string_scalars();
    gent_compound_int_array();
    gent_compound_ints();
    gent_intattrscalars();
    gent_intsattrs();
    gent_floatsattrs();
    gent_bitnopaquefields();
    gent_nodata();

    gent_intsfourdims();
    gent_null_space_group();

    gent_udfilter();

    gent_err_attr_dspace();

    /* Generate the files for testing Onion VFD */
    gent_onion_1d_dset();
    gent_onion_create_delete_objects();
    gent_onion_dset_extension();

#ifdef H5_HAVE__FLOAT16
    gent_float16();
    gent_float16_be();
#endif

#ifdef H5_HAVE_COMPLEX_NUMBERS
    gent_complex();
    gent_complex_be();
#endif

    return EXIT_SUCCESS;
}
