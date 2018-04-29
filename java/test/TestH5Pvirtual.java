/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

package test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.exceptions.HDF5Exception;
import hdf.hdf5lib.exceptions.HDF5LibraryException;

import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5Pvirtual {
    @Rule public TestName testname = new TestName();

    private static final String H5_FILE = "vds.h5";
    private static final String SRC_FILE[] = {
            "v-0.h5",
            "v-1.h5",
            "v-2.h5"
        };
    private static final String SRC_DATASET[] = {
            "A",
            "B",
            "C"
        };
    private static final int DIM_Y = 6;
    private static final int VDSDIM_X = 4;
    private static final int VDSDIM_Y = 6;
    private static final int fill_value = -1;
    long[] H5dims = { DIM_Y };
    long[] VDSH5dims = { VDSDIM_X, VDSDIM_Y };
    long H5fid = -1;
    long H5dsid = -1;
    long H5dssid = -1;
    long H5dvsid = -1;
    long H5did = -1;
    long H5dcplid = -1;
    long H5dapl_id = -1;

    private final void _deleteFile(String filename) {
        File file = new File(filename);

        if (file.exists()) {
            try {file.delete();} catch (SecurityException e) {}
        }
    }

    private final long _createDataset(long fid, long dsid, String name, long dcpl, long dapl) {
        long did = -1;
        long space_id = -1;
        long[] start = {0, 0};
        long[] stride = null;
        long[] count = {1, 1};
        long[] block = {1, VDSDIM_Y};

        try {
            H5dssid = H5.H5Screate_simple(1, H5dims, null);
            for (int i = 0; i < 3; i++) {
                start[0] = i;
                /* Select i-th row in the virtual dataset; selection in the source datasets is the same. */
                H5.H5Sselect_hyperslab(dsid, HDF5Constants.H5S_SELECT_SET, start, stride, count, block);
                H5.H5Pset_virtual(dcpl, dsid, SRC_FILE[i], SRC_DATASET[i], H5dssid);
            }
            did = H5.H5Dcreate(fid, name, HDF5Constants.H5T_NATIVE_INT, dsid,
                    HDF5Constants.H5P_DEFAULT, dcpl, dapl);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Dcreate: " + err);
        }
        assertTrue("TestH5Pvirtual._createDataset: ", did > 0);

        return did;
    }

    private final void _createH5File(long fcpl, long fapl) {
        int[] dset_data = new int[DIM_Y];
        // Create source files and datasets
        for (int i=0; i < 3; i++) {
            long space_id = -1;
            long dset_id = -1;
            long file_id = -1;
            for (int j = 0; j < DIM_Y; j++) dset_data[j] = i+1;

            try {
                file_id = H5.H5Fcreate(SRC_FILE[i], HDF5Constants.H5F_ACC_TRUNC,
                        HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
                space_id = H5.H5Screate_simple(1, H5dims, null);
                dset_id = H5.H5Dcreate(file_id, SRC_DATASET[i], HDF5Constants.H5T_NATIVE_INT, space_id,
                        HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
                H5.H5Dwrite (dset_id, HDF5Constants.H5T_NATIVE_INT, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL, HDF5Constants.H5P_DEFAULT,
                        dset_data);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("TestH5Pvirtual.createH5file: " + err);
            }
            finally {
                if (dset_id > 0)
                    try {H5.H5Dclose(dset_id);} catch (Exception ex) {}
                if (space_id > 0)
                    try {H5.H5Sclose(space_id);} catch (Exception ex) {}
                if (file_id > 0)
                    try {H5.H5Fclose(file_id);} catch (Exception ex) {}
            }
        }

        try {
            int[] fill_value = {-1};
            H5fid = H5.H5Fcreate(H5_FILE, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
            H5dsid = H5.H5Screate_simple(2, VDSH5dims, null);
            H5dcplid = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE);
            H5.H5Pset_fill_value(H5dcplid, HDF5Constants.H5T_NATIVE_INT, fill_value);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5Pvirtual.createH5file: " + err);
        }
        assertTrue("TestH5Pvirtual.createH5file: H5.H5Fcreate: ", H5fid > 0);
        assertTrue("TestH5Pvirtual.createH5file: H5.H5Screate_simple: ", H5dsid > 0);
        assertTrue("TestH5Pvirtual.createH5file: H5.H5Pcreate: ", H5dcplid > 0);

        try {
            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
        }
        catch (Throwable err) {
            err.printStackTrace();
        }
    }

    @Before
    public void createH5file()
            throws NullPointerException, HDF5Exception {
        assertTrue("H5 open ids is 0",H5.getOpenIDCount()==0);
        System.out.print(testname.getMethodName());
        _createH5File(HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        H5dapl_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_ACCESS);
        assertTrue("TestH5Pvirtual.createH5file: H5.H5Pcreate: ", H5dapl_id > 0);
  }

    @After
    public void deleteH5file() throws HDF5LibraryException {
        if (H5dapl_id > 0)
            try {H5.H5Pclose(H5dapl_id);} catch (Exception ex) {}
        if (H5dcplid > 0)
            try {H5.H5Pclose(H5dcplid);} catch (Exception ex) {}
        if (H5dsid > 0)
            try {H5.H5Sclose(H5dsid);} catch (Exception ex) {}
        if (H5fid > 0)
            try {H5.H5Fclose(H5fid);} catch (Exception ex) {}
        for (int i = 0; i < 3; i++) {
            _deleteFile(SRC_FILE[i]);
        }
        _deleteFile(H5_FILE);
        System.out.println();
    }

    @Test
    public void testH5Pvirtual_storage() {
        int layout = -1;

        H5did = _createDataset(H5fid, H5dsid, "VDS", H5dcplid, H5dapl_id);
        try {
            layout = H5.H5Pget_layout (H5dcplid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Pget_layout: " + err);
        }
        finally {
            if (H5dssid > 0)
                try {H5.H5Sclose(H5dssid);} catch (Exception ex) {}
            if (H5did > 0)
                try {H5.H5Dclose(H5did);} catch (Exception ex) {}
        }
        assertTrue("testH5Pvirtual_storage", HDF5Constants.H5D_VIRTUAL == layout);
    }

    @Test
    public void testH5Pget_virtual_count() {
        long num_map = -1;

        H5did = _createDataset(H5fid, H5dsid, "VDS", H5dcplid, H5dapl_id);
        try {
            num_map = H5.H5Pget_virtual_count(H5dcplid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Pget_virtual_count: " + err);
        }
        finally {
            if (H5dssid > 0)
                try {H5.H5Sclose(H5dssid);} catch (Exception ex) {}
            if (H5did > 0)
                try {H5.H5Dclose(H5did);} catch (Exception ex) {}
        }
        assertTrue("testH5Pget_virtual_count: "+num_map, num_map >= 0);
        assertEquals(3, num_map);
    }

    @Test
    public void testH5Pget_source_filename() throws Throwable {
        String filename = null;

        H5did = _createDataset(H5fid, H5dsid, "VDS", H5dcplid, H5dapl_id);
        try {
            filename = (H5.H5Pget_virtual_filename (H5dcplid, 2));
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Pget_virtual_filename: " + err);
        }
        finally {
            if (H5dssid > 0)
                try {H5.H5Sclose(H5dssid);} catch (Exception ex) {}
            if (H5did > 0)
                try {H5.H5Dclose(H5did);} catch (Exception ex) {}
        }
        assertTrue("testH5Pget_source_filename: "+filename, filename.compareTo("v-2.h5") == 0);
    }

    @Test
    public void testH5Pget_source_datasetname() throws Throwable {
        String datasetname = null;

        H5did = _createDataset(H5fid, H5dsid, "VDS", H5dcplid, H5dapl_id);
        try {
            datasetname = H5.H5Pget_virtual_dsetname (H5dcplid, 1);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Pget_virtual_dsetname: " + err);
        }
        finally {
            if (H5dssid > 0)
                try {H5.H5Sclose(H5dssid);} catch (Exception ex) {}
            if (H5did > 0)
                try {H5.H5Dclose(H5did);} catch (Exception ex) {}
        }
        assertTrue("testH5Pget_source_datasetname: "+datasetname, datasetname.compareTo("B") == 0);
    }

    @Test
    public void testH5Pget_selection_source_dataset() throws Throwable {
        long src_space = -1;
        long src_selection = -1;

        H5did = _createDataset(H5fid, H5dsid, "VDS", H5dcplid, H5dapl_id);
        try {
            src_space = H5.H5Pget_virtual_srcspace (H5dcplid, 0);
            src_selection = H5.H5Sget_select_type(src_space);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Pget_selection_source_dataset: " + err);
        }
        finally {
            if (src_space > 0)
                try {H5.H5Sclose(src_space);} catch (Exception ex) {}
            if (H5dssid > 0)
                try {H5.H5Sclose(H5dssid);} catch (Exception ex) {}
            if (H5did > 0)
                try {H5.H5Dclose(H5did);} catch (Exception ex) {}
        }
        assertTrue("testH5Pget_selection_source_dataset", src_selection == HDF5Constants.H5S_SEL_ALL);
    }

    @Test
    public void testH5Pget_mapping_parameters() {
        long num_map = -1;

        H5did = _createDataset(H5fid, H5dsid, "VDS", H5dcplid, H5dapl_id);
        try {
            try {
                num_map = H5.H5Pget_virtual_count(H5dcplid);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("H5.H5Pget_virtual_count: " + err);
            }
            for (int i = 0; i < num_map; i++) {
                int vselection = -1;
                long vspace = -1;
                long nblocks;   // Number of hyperslab blocks
                long blocks[] = {-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1};    // List of blocks
                long[] start = {i, 0};
                long[] stride = {1, 1};
                long[] count = {1, 1};
                long[] block = {1, VDSDIM_Y};
                long q_start[] = new long[2];
                long q_stride[] = new long[2];
                long q_count[] = new long[2];
                long q_block[] = new long[2];
                boolean is_regular = false;

                try {
                    try {
                        vspace = H5.H5Pget_virtual_vspace (H5dcplid, i);
                    }
                    catch (Throwable err) {
                        err.printStackTrace();
                        fail("H5.H5Pget_virtual_vspace: " + err);
                    }
                    try {
                        vselection = H5.H5Sget_select_type(vspace);
                    }
                    catch (Throwable err) {
                        err.printStackTrace();
                        fail("H5.H5Sget_select_type: " + err);
                    }
                    assertTrue("testH5Pget_mapping_parameters["+i+"]", vselection == HDF5Constants.H5S_SEL_HYPERSLABS);

                    // Verify that there is only one block
                    nblocks = H5.H5Sget_select_hyper_nblocks(vspace);
                    assertTrue("H5Sget_select_hyper_nblocks", nblocks == 1);

                    // Retrieve the block defined
                    H5.H5Sget_select_hyper_blocklist(vspace, 0, nblocks, blocks);

                    // Verify that the correct block is defined
                    assertTrue("H5.H5Sget_select_hyper_blocklist["+i+"] [0]: "+blocks[0], start[0] == blocks[0]);
                    assertTrue("H5.H5Sget_select_hyper_blocklist["+i+"] [1]: "+blocks[1], start[1] == blocks[1]);
                    assertTrue("H5.H5Sget_select_hyper_blocklist["+i+"] [2]: "+blocks[2], (block[0]-1+i) == blocks[2]);
                    assertTrue("H5.H5Sget_select_hyper_blocklist["+i+"] [3]: "+blocks[3], (block[1]-1) == blocks[3]);
                    // We also can use new APIs to get start, stride, count and block
                    is_regular = H5.H5Sis_regular_hyperslab(vspace);
                    assertTrue("H5.H5Sis_regular_hyperslab", is_regular);
                    H5.H5Sget_regular_hyperslab (vspace, q_start, q_stride, q_count, q_block);

                    // Verify the hyperslab parameters
                    for(int u = 0; u < 2; u++) {
                        assertTrue("H5Sget_regular_hyperslab, start", start[u] == q_start[u]);
                        assertTrue("H5Sget_regular_hyperslab, stride", stride[u] == q_stride[u]);
                        assertTrue("H5Sget_regular_hyperslab, count", count[u] == q_count[u]);
                        assertTrue("H5Sget_regular_hyperslab, block", block[u] == q_block[u]);
                    }
               }
                catch (Throwable err) {
                    err.printStackTrace();
                    fail("H5.testH5Pget_mapping_parameters: " + err);
                }
                finally {
                    if (vspace > 0)
                        try {H5.H5Sclose(vspace);} catch (Exception ex) {}
                }
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Pget_mapping_parameters: " + err);
        }
        finally {
            if (H5dssid > 0)
                try {H5.H5Sclose(H5dssid);} catch (Exception ex) {}
            if (H5did > 0)
                try {H5.H5Dclose(H5did);} catch (Exception ex) {}
        }
    }

    @Test
    public void testH5Pset_get_virtual_view() {
        int ret_val = -1;
        H5did = _createDataset(H5fid, H5dsid, "VDS", H5dcplid, H5dapl_id);
        try {
            ret_val = H5.H5Pget_virtual_view(H5dapl_id);
            assertTrue("H5Pget_virtual_view", ret_val >= 0);
            assertEquals(HDF5Constants.H5D_VDS_LAST_AVAILABLE, ret_val);
            H5.H5Pset_virtual_view(H5dapl_id, HDF5Constants.H5D_VDS_FIRST_MISSING);
            ret_val = H5.H5Pget_virtual_view(H5dapl_id);
            assertTrue("H5Pget_virtual_view", ret_val >= 0);
            assertEquals(HDF5Constants.H5D_VDS_FIRST_MISSING, ret_val);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Pset_get_virtual_view: " + err);
        }
        finally {
            if (H5dssid > 0)
                try {H5.H5Sclose(H5dssid);} catch (Exception ex) {}
            if (H5did > 0)
                try {H5.H5Dclose(H5did);} catch (Exception ex) {}
        }
    }

    @Test
    public void  testH5Pset_get_virtual_printf_gap() {
        long ret_val = -1;
        H5did = _createDataset(H5fid, H5dsid, "VDS", H5dcplid, H5dapl_id);
        try {
            ret_val = H5.H5Pget_virtual_printf_gap(H5dapl_id);
            assertTrue("H5Pget_virtual_printf_gap", ret_val >= 0);
            assertEquals(0, ret_val);
            H5.H5Pset_virtual_printf_gap(H5dapl_id, 2);
            ret_val = H5.H5Pget_virtual_printf_gap(H5dapl_id);
            assertTrue("H5Pget_virtual_printf_gap", ret_val >= 0);
            assertEquals(2, ret_val);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_get_virtual_printf_gap: " + err);
        }
        finally {
            if (H5dssid > 0)
                try {H5.H5Sclose(H5dssid);} catch (Exception ex) {}
            if (H5did > 0)
                try {H5.H5Dclose(H5did);} catch (Exception ex) {}
        }
    }

    @Test
    public void testH5Pset_virtual_prefix() {
        String prefix = "tmp";
        try {
            H5.H5Pset_virtual_prefix(H5dapl_id, prefix);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_virtual_prefix: " + err);
        }
    }

    @Test(expected = NullPointerException.class)
    public void testH5Pset_virtual_prefix_null() throws Throwable{
        H5.H5Pset_virtual_prefix(H5dapl_id, null);
    }

    @Test
    public void testH5Pget_virtual_prefix() {
        String prefix = "tmp";
        String pre = "";

        try {
            H5.H5Pset_virtual_prefix(H5dapl_id, prefix);
            pre = H5.H5Pget_virtual_prefix(H5dapl_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_virtual_prefix: " + err);
        }
        assertTrue("The prefix: ", prefix.equals(pre));
    }
}
