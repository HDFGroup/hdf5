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

package test;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.exceptions.HDF5LibraryException;

import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5PL {
    @Rule public TestName testname = new TestName();
    private static String FILENAME = "h5_dlopenChunk.h5";
    private static String DATASETNAME = "DS1";
    private static final int DIM_X = 6;
    private static final int DIM_Y = 8;
    private static final int CHUNK_X = 4;
    private static final int CHUNK_Y = 4;
    private static final int RANK = 2;
    private static final int NDIMS = 2;
    private static final int H5Z_FILTER_DYNLIB4 = 260;

    @Before
    public void checkOpenIDs() {
        assertTrue("H5 open ids is 0",H5.getOpenIDCount()==0);
        System.out.print(testname.getMethodName());
    }
    @After
    public void nextTestName() {
        System.out.println();
    }

    @Test
    public void TestH5PLplugins() {
        try {
            int plugin_flags = H5.H5PLget_loading_state();
            assertTrue("H5.H5PLget_loading_state: "+plugin_flags, plugin_flags == HDF5Constants.H5PL_ALL_PLUGIN);
            int new_setting = plugin_flags & ~HDF5Constants.H5PL_FILTER_PLUGIN;
            H5.H5PLset_loading_state (new_setting);
            int changed_flags = H5.H5PLget_loading_state();
            assertTrue("H5.H5PLget_loading_state: "+changed_flags, changed_flags == new_setting);
            H5.H5PLset_loading_state (plugin_flags);
            changed_flags = H5.H5PLget_loading_state();
            assertTrue("H5.H5PLget_loading_state: "+changed_flags, changed_flags == HDF5Constants.H5PL_ALL_PLUGIN);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5PLplugins " + err);
        }
    }

    @Ignore
    public void TestH5PLdlopen() {
        long file_id = -1;
        long filespace_id = -1;
        long dataset_id = -1;
        long fapl_id = -1;
        long dcpl_id = -1;
        try {
            int[]  cd_values = {9, 0, 0, 0};
            int[] libversion = {0, 0, 0};
            long[] dims = { DIM_X, DIM_Y };
            long[] chunk_dims = { CHUNK_X, CHUNK_Y };
            int[][] dset_data = new int[DIM_X][DIM_Y];
            int[] mdc_nelmts = {0};
            long[] rdcc_nelmts = {0};
            long[] rdcc_nbytes = {0};
            double[] rdcc_w0 = {0};

            // Initialize data to "1", to make it easier to see the selections.
            for (int indx = 0; indx < DIM_X; indx++)
                for (int jndx = 0; jndx < DIM_Y; jndx++)
                    dset_data[indx][jndx] = 1;

            // Create a new file using default properties.
            try {
                file_id = H5.H5Fcreate(FILENAME, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                        HDF5Constants.H5P_DEFAULT);
            }
            catch (Exception e) {
                e.printStackTrace();
                fail("TestH5PLdlopen H5Fcreate:" + e);
            }

            // Create dataspace. Setting maximum size to NULL sets the maximum
            // size to be the current size.
            try {
                filespace_id = H5.H5Screate_simple(RANK, dims, null);
            }
            catch (Exception e) {
                e.printStackTrace();
                fail("TestH5PLdlopen H5Screate_simple:" + e);
            }

            // Create the dataset creation property list.
            try {
                dcpl_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE);
            }
            catch (Exception e) {
                e.printStackTrace();
                fail("TestH5PLdlopen H5Pcreate:" + e);
            }

            // Set the chunk size.
            try {
                if (dcpl_id >= 0)
                    H5.H5Pset_chunk(dcpl_id, NDIMS, chunk_dims);
            }
            catch (Exception e) {
                e.printStackTrace();
                fail("TestH5PLdlopen H5Pset_chunk:" + e);
            }

            try {
                H5.H5get_libversion(libversion);
                cd_values[1] = libversion[0];
                cd_values[2] = libversion[1];
                cd_values[3] = libversion[2];
                if (dcpl_id >= 0)
                    H5.H5Pset_filter(dcpl_id, H5Z_FILTER_DYNLIB4, HDF5Constants.H5Z_FLAG_MANDATORY, 4, cd_values);
            }
            catch (Exception e) {
                e.printStackTrace();
                fail("TestH5PLdlopen H5Pset_filter:" + e);
            }

            // Create the chunked dataset.
            try {
                if ((file_id >= 0) && (filespace_id >= 0) && (dcpl_id >= 0))
                    dataset_id = H5.H5Dcreate(file_id, DATASETNAME, HDF5Constants.H5T_NATIVE_INT, filespace_id,
                            HDF5Constants.H5P_DEFAULT, dcpl_id, HDF5Constants.H5P_DEFAULT);
            }
            catch (Exception e) {
                e.printStackTrace();
                fail("TestH5PLdlopen H5Dcreate:" + e);
            }

            try {
                if (dataset_id >= 0)
                    H5.H5Dwrite(dataset_id, HDF5Constants.H5T_NATIVE_INT, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                            HDF5Constants.H5S_ALL, dset_data);
            }
            catch (Exception e) {
                e.printStackTrace();
                fail("TestH5PLdlopen H5Dwrite:" + e);
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5PLdlopen " + err);
        }
        finally {
            // End access to the dataset and release resources used by it.
            if (dcpl_id >= 0)
                try {H5.H5Pclose_class(dcpl_id);} catch (Throwable err) {}
            if (dataset_id >= 0)
                try {H5.H5Dclose(dataset_id);} catch (Throwable err) {}
            if (filespace_id >= 0)
                try {H5.H5Sclose(filespace_id);} catch (Throwable err) {}
            if (file_id >= 0)
                try {H5.H5Fclose(file_id);} catch (Throwable err) {}
        }
    }
}
