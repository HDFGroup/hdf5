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
import java.text.DecimalFormat;
import java.text.NumberFormat;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.exceptions.HDF5Exception;
import hdf.hdf5lib.exceptions.HDF5LibraryException;
import hdf.hdf5lib.exceptions.HDF5PropertyListInterfaceException;
import hdf.hdf5lib.structs.H5AC_cache_config_t;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5Pfapl {
    @Rule public TestName testname = new TestName();

    private static final String H5_FILE = "testPf.h5";
    private static final String H5_LOG_FILE = "testPf.log";
    private static final String H5_FAMILY_FILE = "testPf%05d";
    private static final String H5_MULTI_FILE = "testPfmulti";
    private static char  MULTI_LETTERS[] = {'X','s','b','r','g','l','o'};
    private static final int DIM_X = 4;
    private static final int DIM_Y = 6;
    private static final int DIMF_X = 12;
    private static final int DIMF_Y = 18;
    long H5fid = -1;
    long H5dsid = -1;
    long H5did = -1;
    long H5Fdsid = -1;
    long H5Fdid = -1;
    long[] H5dims = { DIM_X, DIM_Y };
    long fapl_id = -1;
    long plapl_id = -1;
    long dapl_id = -1;
    long plist_id = -1;
    long btplist_id = -1;
    long[] H5Fdims = { DIMF_X, DIMF_Y };
    double windchillF[][] =
    {{36.0, 31.0, 25.0, 19.0, 13.0, 7.0, 1.0, -5.0, -11.0, -16.0, -22.0, -28.0, -34.0, -40.0, -46.0, -52.0, -57.0, -63.0},
     {34.0, 27.0, 21.0, 15.0, 9.0, 3.0, -4.0, -10.0, -16.0, -22.0, -28.0, -35.0, -41.0, -47.0, -53.0, -59.0, -66.0, -72.0},
     {32.0, 25.0, 19.0, 13.0, 6.0, 0.0, -7.0, -13.0, -19.0, -26.0, -32.0, -39.0, -45.0, -51.0, -58.0, -64.0, -71.0, -77.0},
     {30.0, 24.0, 17.0, 11.0, 4.0, -2.0, -9.0, -15.0, -22.0, -29.0, -35.0, -42.0, -48.0, -55.0, -61.0, -68.0, -74.0, -81.0},
     {29.0, 23.0, 16.0, 9.0, 3.0, -4.0, -11.0, -17.0, -24.0, -31.0, -37.0, -44.0, -51.0, -58.0, -64.0, -71.0, -78.0, -84.0},
     {28.0, 22.0, 15.0, 8.0, 1.0, -5.0, -12.0, -19.0, -26.0, -33.0, -39.0, -46.0, -53.0, -60.0, -67.0, -73.0, -80.0, -87.0},
     {28.0, 21.0, 14.0, 7.0, 0.0, -7.0, -14.0, -21.0, -27.0, -34.0, -41.0, -48.0, -55.0, -62.0, -69.0, -76.0, -82.0, -89.0},
     {27.0, 20.0, 13.0, 6.0, -1.0, -8.0, -15.0, -22.0, -29.0, -36.0, -43.0, -50.0, -57.0, -64.0, -71.0, -78.0, -84.0, -91.0},
     {26.0, 19.0, 12.0, 5.0, -2.0, -9.0, -16.0, -23.0, -30.0, -37.0, -44.0, -51.0, -58.0, -65.0, -72.0, -79.0, -86.0, -93.0},
     {26.0, 19.0, 12.0, 4.0, -3.0, -10.0, -17.0, -24.0, -31.0, -38.0, -45.0, -52.0, -60.0, -67.0, -74.0, -81.0, -88.0, -95.0},
     {25.0, 18.0, 11.0, 4.0, -3.0, -11.0, -18.0, -25.0, -32.0, -39.0, -46.0, -54.0, -61.0, -68.0, -75.0, -82.0, -89.0, -97.0},
     {25.0, 17.0, 10.0, 3.0, -4.0, -11.0, -19.0, -26.0, -33.0, -40.0, -48.0, -55.0, -62.0, -69.0, -76.0, -84.0, -91.0, -98.0}
    };

    private final void _deleteFile(String filename) {
        File file = null;
        try {
            file = new File(filename);
        }
        catch (Throwable err) {}

        if (file.exists()) {
            try {file.delete();} catch (SecurityException e) {}
        }
    }

    private final void _deleteLogFile() {
        File file = null;
        try {
            file = new File(H5_LOG_FILE);
        }
        catch (Throwable err) {}

        if (file.exists()) {
            try {file.delete();} catch (SecurityException e) {}
        }
    }

    private final void _deleteFamilyFile() {
        File file = null;
        for(int indx = 0; ;indx++) {
            java.text.DecimalFormat myFormat = new java.text.DecimalFormat("00000");
            try {
                file = new File("test"+myFormat.format(new Integer(indx))+".h5");
            }
            catch (Throwable err) {}

            if (file.exists()) {
                try {file.delete();} catch (SecurityException e) {}
            }
            else
                return;
        }
    }

    private final void _deleteMultiFile() {
        File file = null;
        for(int indx = 1;indx<7;indx++) {
            try {
                file = new File(H5_MULTI_FILE+"-"+MULTI_LETTERS[indx]+".h5");
            }
            catch (Throwable err) {}

            if (file.exists()) {
                try {file.delete();} catch (SecurityException e) {}
            }
        }
    }

    private final long _createDataset(long fid, long dsid, String name, long dapl) {
        long did = -1;
        try {
            did = H5.H5Dcreate(fid, name, HDF5Constants.H5T_STD_I32BE, dsid,
                    HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, dapl);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Dcreate: " + err);
        }
        assertTrue("TestH5Pfapl._createDataset: ", did > 0);

        return did;
    }

    private final void _createFloatDataset() {
        try {
            H5Fdsid = H5.H5Screate_simple(2, H5Fdims, null);
            H5Fdid = H5.H5Dcreate(H5fid, "dsfloat", HDF5Constants.H5T_NATIVE_FLOAT, H5Fdsid,
                    HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Dcreate: " + err);
        }
        assertTrue("TestH5Pfapl._createFloatDataset: ", H5Fdid > 0);

        try {
            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
        }
        catch (Throwable err) {
            err.printStackTrace();
        }
    }

    private final void _createH5multiFileDS() {
        try {
            H5did = _createDataset(H5fid, H5dsid, "dset", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5Pfapl.createH5file: " + err);
        }
        assertTrue("TestH5Pfapl.createH5file: _createDataset: ", H5did > 0);

        try {
            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
        }
        catch (Throwable err) {
            err.printStackTrace();
        }
    }

    private final void _createH5File(long fapl) {
        try {
            H5fid = H5.H5Fcreate(H5_FILE, HDF5Constants.H5F_ACC_TRUNC,
                    HDF5Constants.H5P_DEFAULT, fapl);
            H5dsid = H5.H5Screate_simple(2, H5dims, null);
            H5did = _createDataset(H5fid, H5dsid, "dset", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5Pfapl.createH5file: " + err);
        }
        assertTrue("TestH5Pfapl.createH5file: H5.H5Fcreate: ", H5fid > 0);
        assertTrue("TestH5Pfapl.createH5file: H5.H5Screate_simple: ", H5dsid > 0);
        assertTrue("TestH5Pfapl.createH5file: _createDataset: ", H5did > 0);

        try {
            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
        }
        catch (Throwable err) {
            err.printStackTrace();
        }
    }

    private final void _createH5familyFile(long fapl) {
        try {
            H5fid = H5.H5Fcreate(H5_FAMILY_FILE+".h5", HDF5Constants.H5F_ACC_TRUNC,
                    HDF5Constants.H5P_DEFAULT, fapl);
            H5dsid = H5.H5Screate_simple(2, H5dims, null);
            H5did = _createDataset(H5fid, H5dsid, "dset", HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5Pfapl.createH5file: " + err);
        }
        assertTrue("TestH5Pfapl.createH5file: H5.H5Fcreate: ", H5fid > 0);
        assertTrue("TestH5Pfapl.createH5file: H5.H5Screate_simple: ", H5dsid > 0);
        assertTrue("TestH5Pfapl.createH5file: _createDataset: ", H5did > 0);

        try {
            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
        }
        catch (Throwable err) {
            err.printStackTrace();
        }
    }

    private final void _createH5multiFile(long fapl) {
        try {
            H5fid = H5.H5Fcreate(H5_MULTI_FILE, HDF5Constants.H5F_ACC_TRUNC,
                    HDF5Constants.H5P_DEFAULT, fapl);
            H5dsid = H5.H5Screate_simple(2, H5dims, null);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5Pfapl.createH5file: " + err);
        }
        assertTrue("TestH5Pfapl.createH5file: H5.H5Fcreate: ", H5fid > 0);
        assertTrue("TestH5Pfapl.createH5file: H5.H5Screate_simple: ", H5dsid > 0);

        try {
            H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
        }
        catch (Throwable err) {
            err.printStackTrace();
        }
    }

    public void deleteH5file() {
        _deleteFile(H5_FILE);
    }

    public void deleteH5familyfile() {
        _deleteFamilyFile();
    }

    public void deleteH5multifile() {
        _deleteMultiFile();
    }

    @Before
    public void createFileAccess()
            throws NullPointerException, HDF5Exception {
        assertTrue("H5 open ids is 0",H5.getOpenIDCount()==0);
        System.out.print(testname.getMethodName());

        try {
            fapl_id = H5.H5Pcreate(HDF5Constants.H5P_FILE_ACCESS);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5Pfapl.createFileAccess: " + err);
        }
        assertTrue(fapl_id > 0);
        try {
            plapl_id = H5.H5Pcreate(HDF5Constants.H5P_LINK_ACCESS);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5Pfapl.createFileAccess: " + err);
        }
        assertTrue(plapl_id > 0);
        try {
            plist_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_XFER);
            btplist_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_XFER);
            dapl_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_ACCESS);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5Pfapl.createFileAccess: " + err);
        }
        assertTrue(plist_id > 0);
        assertTrue(btplist_id > 0);
        assertTrue(dapl_id > 0);
    }

    @After
    public void deleteFileAccess() throws HDF5LibraryException {
        if (fapl_id > 0)
            try {H5.H5Pclose(fapl_id);} catch (Exception ex) {}
        if (plapl_id > 0)
            try {H5.H5Pclose(plapl_id);} catch (Exception ex) {}
        if (dapl_id > 0)
            try {H5.H5Pclose(dapl_id);} catch (Exception ex) {}
        if (plist_id > 0)
            try {H5.H5Pclose(plist_id);} catch (Exception ex) {}
        if (btplist_id > 0)
            try {H5.H5Pclose(btplist_id);} catch (Exception ex) {}

        if (H5Fdsid > 0)
            try {H5.H5Sclose(H5Fdsid);} catch (Exception ex) {}
        if (H5Fdid > 0)
            try {H5.H5Dclose(H5Fdid);} catch (Exception ex) {}
        if (H5dsid > 0)
            try {H5.H5Sclose(H5dsid);} catch (Exception ex) {}
        if (H5did > 0)
            try {H5.H5Dclose(H5did);} catch (Exception ex) {}
        if (H5fid > 0)
            try {H5.H5Fclose(H5fid);} catch (Exception ex) {}
        System.out.println();
    }

    @Test
    public void testH5Pget_libver_bounds() {
        int ret_val = -1;
        int[] libver = new int[2];

        try {
            ret_val = H5.H5Pget_libver_bounds(fapl_id, libver);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_libver_bounds: " + err);
        }
        assertTrue("testH5Pget_libver_bounds", ret_val >= 0);
        // Check the Earliest Version if the library
        assertEquals(HDF5Constants.H5F_LIBVER_EARLIEST, libver[0]);
        // Check the Latest Version if the library
        assertEquals(HDF5Constants.H5F_LIBVER_LATEST, libver[1]);
    }

    @Test
    public void testH5Pset_libver_bounds() {

        int ret_val = -1;
        int low = HDF5Constants.H5F_LIBVER_EARLIEST;
        int high = HDF5Constants.H5F_LIBVER_LATEST;
        int[] libver = new int[2];

        try {
            ret_val = H5.H5Pset_libver_bounds(fapl_id, low, high);
            H5.H5Pget_libver_bounds(fapl_id, libver);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_libver_bounds: " + err);
        }
        assertTrue("testH5Pset_libver_bounds", ret_val >= 0);
        // Check the Earliest Version if the library
        assertEquals(HDF5Constants.H5F_LIBVER_EARLIEST, libver[0]);
        // Check the Latest Version if the library
        assertEquals(HDF5Constants.H5F_LIBVER_LATEST, libver[1]);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Pset_elink_fapl_NegativeID() throws Throwable {
        H5.H5Pset_elink_fapl(-1, fapl_id );
    }

    @Test
    public void testH5Pset_elink_fapl() {
        int ret_val = -1;
        try {
            ret_val = H5.H5Pset_elink_fapl(plapl_id, fapl_id );
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_elink_fapl: " + err);
        }
        assertTrue("H5Pset_elink_fapl", ret_val >= 0);
    }

    @Test
    public void testH5Pget_elink_fapl() {
        long ret_val_id = -1;
        try {
            ret_val_id = H5.H5Pget_elink_fapl(plapl_id);
            assertTrue("H5Pget_elink_fapl", ret_val_id >= 0);
            assertEquals(HDF5Constants.H5P_DEFAULT, ret_val_id );
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_elink_fapl: " + err);
        }
        finally {
            if (ret_val_id > 0)
                try {H5.H5Pclose(ret_val_id);} catch (Exception ex) {}
        }
    }

    @Test
    public void testH5P_elink_fapl() {
        long ret_val_id = -1;
        try {
            H5.H5Pset_elink_fapl(plapl_id, fapl_id );
            ret_val_id = H5.H5Pget_elink_fapl(plapl_id);
            assertTrue("H5P_elink_fapl", ret_val_id >= 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5P_elink_fapl: " + err);
        }
        finally {
            if (ret_val_id > 0)
                try {H5.H5Pclose(ret_val_id);} catch (Exception ex) {}
        }
    }

    @Test
    public void testH5P_elink_file_cache_size() {
        long elink_fapl_id = -1;
        int efc_size = 0;
        try {
            H5.H5Pset_elink_fapl(plapl_id, fapl_id );
            elink_fapl_id = H5.H5Pget_elink_fapl(plapl_id);
            assertTrue("H5P_elink_file_cache_size", elink_fapl_id >= 0);
            try {
                efc_size = H5.H5Pget_elink_file_cache_size(elink_fapl_id);
                assertTrue("H5P_elink_file_cache_size default", efc_size == 0);
            }
            catch (UnsupportedOperationException err) {
                System.out.println(err.getMessage());
            }
            try {
                efc_size = 8;
                H5.H5Pset_elink_file_cache_size(elink_fapl_id, efc_size);
                efc_size = H5.H5Pget_elink_file_cache_size(elink_fapl_id);
                assertTrue("H5P_elink_file_cache_size 8", efc_size == 8);
            }
            catch (UnsupportedOperationException err) {
                System.out.println(err.getMessage());
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5P_elink_file_cache_size: " + err);
        }
        finally {
            if (elink_fapl_id > 0)
                try {H5.H5Pclose(elink_fapl_id);} catch (Exception ex) {}
        }
    }

    @Test
    public void testH5P_btree_ratios() {
        double[] left = {0.1};
        double[] middle = {0.5};
        double[] right = {0.7};
        try {
            H5.H5Pset_btree_ratios(plist_id, left[0], middle[0], right[0]);
            H5.H5Pget_btree_ratios(plist_id, left, middle, right);
            assertTrue("H5P_btree_ratios", left[0] == 0.1);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5P_btree_ratios: " + err);
        }
    }

    @Test
    public void testH5P_edc_check() {
        int ret_val_id = -1;
        try {
            ret_val_id = H5.H5Pget_edc_check(plist_id);
            assertTrue("H5P_edc_check", ret_val_id == HDF5Constants.H5Z_ENABLE_EDC);
            H5.H5Pset_edc_check(plist_id, HDF5Constants.H5Z_DISABLE_EDC);
            ret_val_id = H5.H5Pget_edc_check(plist_id);
            assertTrue("H5P_edc_check", ret_val_id == HDF5Constants.H5Z_DISABLE_EDC);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5P_edc_check: " + err);
        }
    }

    @Test
    public void testH5P_fclose_degree() {
        int ret_val_id = -1;
        try {
            ret_val_id = H5.H5Pget_fclose_degree(fapl_id);
            assertTrue("H5Pget_fclose_degree default", ret_val_id == HDF5Constants.H5F_CLOSE_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5P_fclose_degree: default " + err);
        }
        try {
            H5.H5Pset_fclose_degree(fapl_id, HDF5Constants.H5F_CLOSE_STRONG);
            ret_val_id = H5.H5Pget_fclose_degree(fapl_id);
            assertTrue("H5Pget_fclose_degree", ret_val_id == HDF5Constants.H5F_CLOSE_STRONG);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5P_fclose_degree: H5F_CLOSE_STRONG " + err);
        }
        try {
            H5.H5Pset_fclose_degree(fapl_id, HDF5Constants.H5F_CLOSE_SEMI);
            ret_val_id = H5.H5Pget_fclose_degree(fapl_id);
            assertTrue("H5Pget_fclose_degree", ret_val_id == HDF5Constants.H5F_CLOSE_SEMI);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5P_fclose_degree: H5F_CLOSE_SEMI " + err);
        }
    }

    @Test
    public void testH5P_alignment() {
        long[] align = {0,0};
        try {
            H5.H5Pget_alignment(fapl_id, align);
            assertTrue("H5P_alignment threshold default", align[0] == 1);
            assertTrue("H5P_alignment alignment default", align[1] == 1);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5P_alignment: default " + err);
        }
        try {
            align[0] = 1024;
            align[1] = 2048;
            H5.H5Pset_alignment(fapl_id, align[0], align[1]);
            H5.H5Pget_alignment(fapl_id, align);
            assertTrue("H5P_alignment threshold", align[0] == 1024);
            assertTrue("H5P_alignment alignment", align[1] == 2048);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5P_alignment: " + err);
        }
    }

    @Test
    public void testH5P_meta_block_size() {
        long meta_size = 0;
        try {
            meta_size = H5.H5Pget_meta_block_size(fapl_id);
            assertTrue("H5P_meta_block_size default", meta_size == 2048);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5P_meta_block_size: default " + err);
        }
        try {
            meta_size = 4096;
            H5.H5Pset_meta_block_size(fapl_id, meta_size);
            meta_size = H5.H5Pget_meta_block_size(fapl_id);
            assertTrue("H5P_meta_block_size 4096", meta_size == 4096);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5P_meta_block_size: " + err);
        }
    }

    @Test
    public void testH5P_small_data_block_size() {
        long align = 0;
        try {
            align = H5.H5Pget_small_data_block_size(fapl_id);
            assertTrue("H5P_small_data_block_size default", align == 2048);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5P_small_data_block_size: default " + err);
        }
        try {
            align = 4096;
            H5.H5Pset_small_data_block_size(fapl_id, align);
            align = H5.H5Pget_small_data_block_size(fapl_id);
            assertTrue("H5P_small_data_block_size 4096", align == 4096);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5P_small_data_block_size: " + err);
        }
    }

    @Test
    public void testH5P_hyper_vector_size() {
        long[] align = {0};
        try {
            H5.H5Pget_hyper_vector_size(plist_id, align);
            assertTrue("H5P_hyper_vector_size default", align[0] == 1024);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5P_hyper_vector_size: default " + err);
        }
        try {
            align[0] = 4096;
            H5.H5Pset_hyper_vector_size(plist_id, align[0]);
            H5.H5Pget_hyper_vector_size(plist_id, align);
            assertTrue("H5P_hyper_vector_size 4096", align[0] == 4096);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5P_hyper_vector_size: " + err);
        }
    }

    @Test
    public void testH5P_cache() {
        long[] rdcc_nelmts = {0};
        long[] rdcc_nbytes = {0};
        double[] rdcc_w0 = {0};
        try {
            H5.H5Pget_cache(fapl_id, null, rdcc_nelmts, rdcc_nbytes, rdcc_w0);
            assertTrue("H5P_cache default", rdcc_nelmts[0] == 521);
            assertTrue("H5P_cache default", rdcc_nbytes[0] == (1024*1024));
            assertTrue("H5P_cache default", rdcc_w0[0] == 0.75);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5P_cache: default " + err);
        }
        try {
            rdcc_nelmts[0] = 4096;
            H5.H5Pset_cache(fapl_id, 0, rdcc_nelmts[0], rdcc_nbytes[0], rdcc_w0[0]);
            H5.H5Pget_cache(fapl_id, null, rdcc_nelmts, rdcc_nbytes, rdcc_w0);
            assertTrue("H5P_cache 4096", rdcc_nelmts[0] == 4096);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5P_cache: " + err);
        }
    }

    @Test
    public void testH5P_chunk_cache() {
        long[] rdcc_nslots = {0};
        long[] rdcc_nbytes = {0};
        double[] rdcc_w0 = {0};
        try {
            H5.H5Pget_chunk_cache(dapl_id, rdcc_nslots, rdcc_nbytes, rdcc_w0);
            assertTrue("H5P_chunk_cache default", rdcc_nslots[0] == 521);
            assertTrue("H5P_chunk_cache default", rdcc_nbytes[0] == (1024*1024));
            assertTrue("H5P_chunk_cache default", rdcc_w0[0] == 0.75);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5P_chunk_cache: default " + err);
        }
        try {
            rdcc_nslots[0] = 4096;
            H5.H5Pset_chunk_cache(dapl_id, rdcc_nslots[0], rdcc_nbytes[0], rdcc_w0[0]);
            H5.H5Pget_chunk_cache(dapl_id, rdcc_nslots, rdcc_nbytes, rdcc_w0);
            assertTrue("H5P_chunk_cache 4096", rdcc_nslots[0] == 4096);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5P_chunk_cache: " + err);
        }
    }

    @Test
    public void testH5P_sieve_buf_size() {
        long buf_size = 0;
        try {
            buf_size = H5.H5Pget_sieve_buf_size(fapl_id);
            assertTrue("H5P_sieve_buf_size default", buf_size == (64*1024));
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5P_sieve_buf_size: default " + err);
        }
        try {
            buf_size = 4096;
            H5.H5Pset_sieve_buf_size(fapl_id, buf_size);
            buf_size = H5.H5Pget_sieve_buf_size(fapl_id);
            assertTrue("H5P_sieve_buf_size 4096", buf_size == 4096);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5P_sieve_buf_size: " + err);
        }
    }

    @Test
    public void testH5P_gc_references() {
        boolean ret_val_id = false;
        try {
            H5.H5Pset_gc_references(fapl_id, true);
            ret_val_id = H5.H5Pget_gc_references(fapl_id);
            assertTrue("H5P_gc_references", ret_val_id);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5P_gc_references: " + err);
        }
    }

    @Test
    public void testH5Pget_mdc_config() {
        H5AC_cache_config_t cache_config = null;
        try {
            cache_config = H5.H5Pget_mdc_config(fapl_id);
            assertTrue("H5Pget_mdc_config", cache_config.version==HDF5Constants.H5AC_CURR_CACHE_CONFIG_VERSION);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_mdc_config: " + err);
        }
    }

    @Test
    public void testH5Pset_mdc_config() {
        H5AC_cache_config_t cache_config = null;
        try {
            cache_config = H5.H5Pget_mdc_config(fapl_id);
            assertTrue("H5Pset_mdc_config", cache_config.version==HDF5Constants.H5AC_CURR_CACHE_CONFIG_VERSION);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_mdc_config: " + err);
        }
        try {
            cache_config.decr_mode = HDF5Constants.H5C_decr_off;
            H5.H5Pset_mdc_config(fapl_id, cache_config);
            cache_config = H5.H5Pget_mdc_config(fapl_id);
            assertTrue("H5Pset_mdc_config", cache_config.version==HDF5Constants.H5AC_CURR_CACHE_CONFIG_VERSION);
            assertTrue("H5Pset_mdc_config", cache_config.decr_mode==HDF5Constants.H5C_decr_off);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_mdc_config: " + err);
        }
    }

    @Test
    public void testH5P_fapl_core() {
        if (HDF5Constants.H5FD_CORE < 0)
            return;
        try {
            H5.H5Pset_fapl_core(fapl_id, 4096, false);
            long driver_type = H5.H5Pget_driver(fapl_id);
            assertTrue("H5Pget_driver: core = "+ driver_type, HDF5Constants.H5FD_CORE==driver_type);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_fapl_core: " + err);
        }
        try {
            long[] increment = {-1};
            boolean[] backingstore = {true};
            H5.H5Pget_fapl_core(fapl_id, increment, backingstore);
            assertTrue("H5Pget_fapl_core: increment="+increment[0], increment[0]==4096);
            assertTrue("H5Pget_fapl_core: backingstore="+backingstore[0], !backingstore[0]);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_fapl_core: " + err);
        }
    }

    @Test
    public void testH5P_fapl_family() {
        if (HDF5Constants.H5FD_FAMILY < 0)
            return;
        try {
            H5.H5Pset_fapl_family(fapl_id, 1024, HDF5Constants.H5P_DEFAULT);
            long driver_type = H5.H5Pget_driver(fapl_id);
            assertTrue("H5Pget_driver: family = "+ driver_type, HDF5Constants.H5FD_FAMILY==driver_type);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_fapl_family: " + err);
        }
        try {
            long[] member_size = {0};
            long[] member_fapl = {-1};
            H5.H5Pget_fapl_family(fapl_id, member_size, member_fapl);
            assertTrue("H5Pget_fapl_family: member_size="+member_size[0], member_size[0]==1024);
            assertTrue("H5Pget_fapl_family: member_fapl ", H5.H5P_equal(member_fapl[0], HDF5Constants.H5P_FILE_ACCESS_DEFAULT));
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_fapl_family: " + err);
        }
        _createH5familyFile(fapl_id);
        deleteH5familyfile();
    }

    @Test
    public void testH5P_family_offset() {
        if (HDF5Constants.H5FD_FAMILY < 0)
            return;
        try {
            H5.H5Pset_fapl_family(fapl_id, 1024, HDF5Constants.H5P_DEFAULT);
            long driver_type = H5.H5Pget_driver(fapl_id);
            assertTrue("H5Pget_driver: family = "+ driver_type, HDF5Constants.H5FD_FAMILY==driver_type);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_fapl_family: " + err);
        }
        _createH5familyFile(fapl_id);
        long family_offset = 512;
        try {
            H5.H5Pset_family_offset(fapl_id, family_offset);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_fapl_family: " + err);
        }
        try {
            long offset = H5.H5Pget_family_offset(fapl_id);
            assertTrue("H5Pget_fapl_family: offset="+offset, offset==family_offset);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_fapl_family: " + err);
        }
        deleteH5familyfile();
    }

    @Test
    public void testH5Pset_fapl_sec2() {
        if (HDF5Constants.H5FD_SEC2 < 0)
            return;
        try {
            H5.H5Pset_fapl_sec2(fapl_id);
            long driver_type = H5.H5Pget_driver(fapl_id);
            assertTrue("H5Pget_driver: sec2 = "+ driver_type, HDF5Constants.H5FD_SEC2==driver_type);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_fapl_sec2: " + err);
        }
        _createH5File(fapl_id);
        deleteH5file();
    }

    @Test
    public void testH5Pset_fapl_stdio() {
        if (HDF5Constants.H5FD_STDIO < 0)
            return;
        try {
            H5.H5Pset_fapl_stdio(fapl_id);
            long driver_type = H5.H5Pget_driver(fapl_id);
            assertTrue("H5Pget_driver: stdio = "+ driver_type, HDF5Constants.H5FD_STDIO==driver_type);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_fapl_stdio: " + err);
        }
        _createH5File(fapl_id);
        deleteH5file();
    }

    @Test
    public void testH5Pset_fapl_log() {
        if (HDF5Constants.H5FD_LOG < 0)
            return;
        try {
            long log_flags = HDF5Constants.H5FD_LOG_LOC_IO;
            H5.H5Pset_fapl_log(fapl_id, H5_LOG_FILE, log_flags, 1024);
            long driver_type = H5.H5Pget_driver(fapl_id);
            assertTrue("H5Pget_driver: log = "+ driver_type, HDF5Constants.H5FD_LOG==driver_type);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_fapl_log: " + err);
        }
        _createH5File(fapl_id);
        deleteH5file();
        _deleteLogFile();
    }

    @Test
    public void testH5P_fapl_muti_nulls() {
        if (HDF5Constants.H5FD_MULTI < 0)
            return;

        int[] member_map = null;
        long[] member_fapl = null;
        String[] member_name = null;
        long[] member_addr = null;

        try {
            H5.H5Pset_fapl_multi(fapl_id, member_map, member_fapl, member_name, member_addr, true);
            long driver_type = H5.H5Pget_driver(fapl_id);
            assertTrue("H5Pget_driver: muti = "+ driver_type, HDF5Constants.H5FD_MULTI==driver_type);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_fapl_muti: " + err);
        }
        try {
            boolean relax = H5.H5Pget_fapl_multi(fapl_id, member_map, member_fapl, member_name, member_addr);
            assertTrue("H5Pget_fapl_muti: relax ", relax);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_fapl_muti: " + err);
        }
        _createH5multiFile(fapl_id);
        deleteH5multifile();
    }

    @Test
    public void testH5P_fapl_muti_defaults() {
        if (HDF5Constants.H5FD_MULTI < 0)
            return;
        int H5FD_MEM_NTYPES = HDF5Constants.H5FD_MEM_NTYPES; // 7

        long sH5FD_MEM_DEFAULT_HADDR = HDF5Constants.H5FD_DEFAULT_HADDR_SIZE;
        long sH5FD_MEM_HADDR = HDF5Constants.H5FD_MEM_DEFAULT_SIZE;
        long sH5FD_MEM_SUPER_HADDR = HDF5Constants.H5FD_MEM_DEFAULT_SUPER_SIZE;
        long sH5FD_MEM_BTREE_HADDR = HDF5Constants.H5FD_MEM_DEFAULT_BTREE_SIZE;
        long sH5FD_MEM_DRAW_HADDR = HDF5Constants.H5FD_MEM_DEFAULT_DRAW_SIZE;
        long sH5FD_MEM_GHEAP_HADDR = HDF5Constants.H5FD_MEM_DEFAULT_GHEAP_SIZE;
        long sH5FD_MEM_LHEAP_HADDR = HDF5Constants.H5FD_MEM_DEFAULT_LHEAP_SIZE;
        long sH5FD_MEM_OHDR_HADDR = HDF5Constants.H5FD_MEM_DEFAULT_OHDR_SIZE;
        int[] member_map = null;
        long[] member_fapl = null;
        String[] member_name = null;
        long[] member_addr = null;

        try {
            H5.H5Pset_fapl_multi(fapl_id, member_map, member_fapl, member_name, member_addr, true);
            long driver_type = H5.H5Pget_driver(fapl_id);
            assertTrue("H5Pget_driver: muti = "+ driver_type, HDF5Constants.H5FD_MULTI==driver_type);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_fapl_muti: " + err);
        }
        try {
            member_map = new int[HDF5Constants.H5FD_MEM_NTYPES];
            member_fapl = new long[HDF5Constants.H5FD_MEM_NTYPES];
            member_name = new String[HDF5Constants.H5FD_MEM_NTYPES];
            member_addr = new long[HDF5Constants.H5FD_MEM_NTYPES];
            boolean relax = H5.H5Pget_fapl_multi(fapl_id, member_map, member_fapl, member_name, member_addr);
            assertTrue("H5Pget_fapl_muti: relax ", relax);
            assertTrue("H5Pget_fapl_muti: member_map="+member_map[HDF5Constants.H5FD_MEM_DEFAULT], member_map[HDF5Constants.H5FD_MEM_DEFAULT] == HDF5Constants.H5FD_MEM_DEFAULT);
            assertTrue("H5Pget_fapl_muti: member_fapl ", H5.H5P_equal(member_fapl[HDF5Constants.H5FD_MEM_DEFAULT], HDF5Constants.H5P_FILE_ACCESS_DEFAULT));
            assertTrue("H5Pget_fapl_muti: member_name="+member_name[HDF5Constants.H5FD_MEM_DEFAULT], member_name[HDF5Constants.H5FD_MEM_DEFAULT].compareTo("%s-X.h5")==0);
            assertTrue("H5Pget_fapl_muti: member_name="+member_name[HDF5Constants.H5FD_MEM_SUPER], member_name[HDF5Constants.H5FD_MEM_SUPER].compareTo("%s-s.h5")==0);
            assertTrue("H5Pget_fapl_muti: member_name="+member_name[HDF5Constants.H5FD_MEM_BTREE], member_name[HDF5Constants.H5FD_MEM_BTREE].compareTo("%s-b.h5")==0);
            assertTrue("H5Pget_fapl_muti: member_name="+member_name[HDF5Constants.H5FD_MEM_DRAW], member_name[HDF5Constants.H5FD_MEM_DRAW].compareTo("%s-r.h5")==0);
            assertTrue("H5Pget_fapl_muti: member_addr="+member_addr[HDF5Constants.H5FD_MEM_DEFAULT], member_addr[HDF5Constants.H5FD_MEM_DEFAULT] == sH5FD_MEM_HADDR);
            assertTrue("H5Pget_fapl_muti: member_addr="+member_addr[HDF5Constants.H5FD_MEM_SUPER], member_addr[HDF5Constants.H5FD_MEM_SUPER] == sH5FD_MEM_SUPER_HADDR);
            assertTrue("H5Pget_fapl_muti: member_addr="+member_addr[HDF5Constants.H5FD_MEM_BTREE], member_addr[HDF5Constants.H5FD_MEM_BTREE] == sH5FD_MEM_BTREE_HADDR);
            assertTrue("H5Pget_fapl_muti: member_addr="+member_addr[HDF5Constants.H5FD_MEM_DRAW], member_addr[HDF5Constants.H5FD_MEM_DRAW] == sH5FD_MEM_DRAW_HADDR);
            assertTrue("H5Pget_fapl_muti: member_addr="+member_addr[HDF5Constants.H5FD_MEM_GHEAP], member_addr[HDF5Constants.H5FD_MEM_GHEAP] == sH5FD_MEM_GHEAP_HADDR);
            assertTrue("H5Pget_fapl_muti: member_addr="+member_addr[HDF5Constants.H5FD_MEM_LHEAP], member_addr[HDF5Constants.H5FD_MEM_LHEAP] == sH5FD_MEM_LHEAP_HADDR);
            assertTrue("H5Pget_fapl_muti: member_addr="+member_addr[HDF5Constants.H5FD_MEM_OHDR], member_addr[HDF5Constants.H5FD_MEM_OHDR] == sH5FD_MEM_OHDR_HADDR);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_fapl_muti: " + err);
        }
        _createH5multiFile(fapl_id);
        _createH5multiFileDS();
        deleteH5multifile();
    }

    @Test
    public void testH5P_fapl_muti() {
        if (HDF5Constants.H5FD_MULTI < 0)
            return;
        long HADDR_DEFAULT_SIZE = HDF5Constants.H5FD_DEFAULT_HADDR_SIZE;
        int[] member_map = new int[HDF5Constants.H5FD_MEM_NTYPES];
        long[] member_fapl = new long[HDF5Constants.H5FD_MEM_NTYPES];
        String[] member_name = new String[HDF5Constants.H5FD_MEM_NTYPES];
        long[] member_addr = new long[HDF5Constants.H5FD_MEM_NTYPES];

        for(int mt=HDF5Constants.H5FD_MEM_DEFAULT; mt<HDF5Constants.H5FD_MEM_NTYPES; mt++) {
            member_fapl[mt] = HDF5Constants.H5P_DEFAULT;
            member_map[mt] = HDF5Constants.H5FD_MEM_SUPER;
        }
        member_map[HDF5Constants.H5FD_MEM_DRAW] = HDF5Constants.H5FD_MEM_DRAW;
        member_map[HDF5Constants.H5FD_MEM_BTREE] = HDF5Constants.H5FD_MEM_BTREE;
        member_map[HDF5Constants.H5FD_MEM_GHEAP] = HDF5Constants.H5FD_MEM_GHEAP;

        member_name[HDF5Constants.H5FD_MEM_SUPER] = new String("%s-super.h5");
        member_addr[HDF5Constants.H5FD_MEM_SUPER] = 0;

        member_name[HDF5Constants.H5FD_MEM_BTREE] = new String("%s-btree.h5");
        member_addr[HDF5Constants.H5FD_MEM_BTREE] = HADDR_DEFAULT_SIZE/4;

        member_name[HDF5Constants.H5FD_MEM_DRAW] = new String("%s-draw.h5");
        member_addr[HDF5Constants.H5FD_MEM_DRAW] = HADDR_DEFAULT_SIZE/2;

        member_name[HDF5Constants.H5FD_MEM_GHEAP] = new String("%s-gheap.h5");
        member_addr[HDF5Constants.H5FD_MEM_GHEAP] = (HADDR_DEFAULT_SIZE/4)*3;

        try {
            H5.H5Pset_fapl_multi(fapl_id, member_map, member_fapl, member_name, member_addr, true);
            long driver_type = H5.H5Pget_driver(fapl_id);
            assertTrue("H5Pget_driver: muti = "+ driver_type, HDF5Constants.H5FD_MULTI==driver_type);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_fapl_muti: " + err);
        }
        try {
            boolean relax = H5.H5Pget_fapl_multi(fapl_id, member_map, member_fapl, member_name, member_addr);
            assertTrue("H5Pget_fapl_muti: relax ", relax);
            assertTrue("H5Pget_fapl_muti: member_map="+member_map[HDF5Constants.H5FD_MEM_DEFAULT], member_map[HDF5Constants.H5FD_MEM_DEFAULT] == HDF5Constants.H5FD_MEM_SUPER);
            assertTrue("H5Pget_fapl_muti: member_map="+member_map[HDF5Constants.H5FD_MEM_DRAW], member_map[HDF5Constants.H5FD_MEM_DRAW] == HDF5Constants.H5FD_MEM_DRAW);
            assertTrue("H5Pget_fapl_muti: member_map="+member_map[HDF5Constants.H5FD_MEM_BTREE], member_map[HDF5Constants.H5FD_MEM_BTREE] == HDF5Constants.H5FD_MEM_BTREE);
            assertTrue("H5Pget_fapl_muti: member_map="+member_map[HDF5Constants.H5FD_MEM_GHEAP], member_map[HDF5Constants.H5FD_MEM_GHEAP] == HDF5Constants.H5FD_MEM_GHEAP);

            assertTrue("H5Pget_fapl_muti: member_fapl ", H5.H5P_equal(member_fapl[HDF5Constants.H5FD_MEM_DEFAULT], HDF5Constants.H5P_FILE_ACCESS_DEFAULT));
            assertTrue("H5Pget_fapl_muti: member_addr="+member_addr[HDF5Constants.H5FD_MEM_DEFAULT], member_addr[HDF5Constants.H5FD_MEM_DEFAULT] == 0);
            assertTrue("H5Pget_fapl_muti: member_name="+member_name[HDF5Constants.H5FD_MEM_SUPER], member_name[HDF5Constants.H5FD_MEM_SUPER].compareTo("%s-super.h5")==0);

            assertTrue("H5Pget_fapl_muti: member_name="+member_name[HDF5Constants.H5FD_MEM_BTREE], member_name[HDF5Constants.H5FD_MEM_BTREE].compareTo("%s-btree.h5")==0);
            assertTrue("H5Pget_fapl_muti: member_addr="+member_addr[HDF5Constants.H5FD_MEM_BTREE], member_addr[HDF5Constants.H5FD_MEM_BTREE] == HADDR_DEFAULT_SIZE/4);

            assertTrue("H5Pget_fapl_muti: member_name="+member_name[HDF5Constants.H5FD_MEM_DRAW], member_name[HDF5Constants.H5FD_MEM_DRAW].compareTo("%s-draw.h5")==0);
            assertTrue("H5Pget_fapl_muti: member_addr="+member_addr[HDF5Constants.H5FD_MEM_DRAW], member_addr[HDF5Constants.H5FD_MEM_DRAW] == HADDR_DEFAULT_SIZE/2);

            assertTrue("H5Pget_fapl_muti: member_name="+member_name[HDF5Constants.H5FD_MEM_GHEAP], member_name[HDF5Constants.H5FD_MEM_GHEAP].compareTo("%s-gheap.h5")==0);
            assertTrue("H5Pget_fapl_muti: member_addr="+member_addr[HDF5Constants.H5FD_MEM_GHEAP], member_addr[HDF5Constants.H5FD_MEM_GHEAP] == (HADDR_DEFAULT_SIZE/4)*3);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_fapl_muti: " + err);
        }
        _createH5multiFile(fapl_id);
        try {
            long file_size = H5.H5Fget_filesize(H5fid);
            assertTrue("H5Pget_fapl_muti: file_size ", file_size >= HADDR_DEFAULT_SIZE/4 || file_size <= HADDR_DEFAULT_SIZE/2);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_fapl_muti:H5Fget_filesize " + err);
        }
        _createH5multiFileDS();
        deleteH5multifile();
        File file = new File(H5_MULTI_FILE+"-super.h5");
        if (file.exists()) {
            try {
                file.delete();
            }
            catch (SecurityException e) {
                ;// e.printStackTrace();
            }
        }
        file = new File(H5_MULTI_FILE+"-btree.h5");
        if (file.exists()) {
            try {
                file.delete();
            }
            catch (SecurityException e) {
                ;// e.printStackTrace();
            }
        }
        file = new File(H5_MULTI_FILE+"-draw.h5");
        if (file.exists()) {
            try {
                file.delete();
            }
            catch (SecurityException e) {
                ;// e.printStackTrace();
            }
        }
        file = new File(H5_MULTI_FILE+"-gheap.h5");
        if (file.exists()) {
            try {
                file.delete();
            }
            catch (SecurityException e) {
                ;// e.printStackTrace();
            }
        }
    }

    @Test
    public void testH5P_fapl_split() {
        if (HDF5Constants.H5FD_MULTI < 0)
            return;

        try {
            H5.H5Pset_fapl_split(fapl_id, "-meta.h5", HDF5Constants.H5P_DEFAULT, "-raw.h5", HDF5Constants.H5P_DEFAULT);
            long driver_type = H5.H5Pget_driver(fapl_id);
            assertTrue("H5Pget_driver: split = "+ driver_type, HDF5Constants.H5FD_MULTI==driver_type);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_fapl_split: " + err);
        }
        try {
            int[] member_map = new int[HDF5Constants.H5FD_MEM_NTYPES];
            long[] member_fapl = new long[HDF5Constants.H5FD_MEM_NTYPES];
            String[] member_name = new String[HDF5Constants.H5FD_MEM_NTYPES];
            long[] member_addr = new long[HDF5Constants.H5FD_MEM_NTYPES];
            boolean relax = H5.H5Pget_fapl_multi(fapl_id, member_map, member_fapl, member_name, member_addr);
            assertTrue("H5Pget_fapl_multi: relax ", relax);
            assertTrue("H5Pget_fapl_multi: member_name="+member_name[HDF5Constants.H5FD_MEM_SUPER], member_name[HDF5Constants.H5FD_MEM_SUPER].compareTo("%s-meta.h5")==0);
            assertTrue("H5Pget_fapl_multi: member_name="+member_name[HDF5Constants.H5FD_MEM_DRAW], member_name[HDF5Constants.H5FD_MEM_DRAW].compareTo("%s-raw.h5")==0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_fapl_split: " + err);
        }
        _createH5multiFile(fapl_id);
        deleteH5multifile();
        File file = new File(H5_MULTI_FILE+"-meta.h5");
        if (file.exists()) {
            try {
                file.delete();
            }
            catch (SecurityException e) {
                ;// e.printStackTrace();
            }
        }
        file = new File(H5_MULTI_FILE+"-raw.h5");
        if (file.exists()) {
            try {
                file.delete();
            }
            catch (SecurityException e) {
                ;// e.printStackTrace();
            }
        }
    }

    @Test
    public void testH5P_fapl_direct() {
        if (HDF5Constants.H5FD_DIRECT < 0)
            return;
        try {
            H5.H5Pset_fapl_direct(fapl_id, 1024, 4096, 8*4096);
            long driver_type = H5.H5Pget_driver(fapl_id);
            assertTrue("H5Pget_driver: direct = "+ driver_type, HDF5Constants.H5FD_DIRECT==driver_type);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_fapl_direct: " + err);
        }
        try {
            long[] params = {-1, -1, -1};
            H5.H5Pget_fapl_direct(fapl_id, params);
            assertTrue("H5Pget_fapl_direct: alignment="+params[0], params[0]==1024);
            assertTrue("H5Pget_fapl_direct: block_size="+params[1], params[1]==4096);
            assertTrue("H5Pget_fapl_direct: cbuf_size="+params[2], params[2]==8*4096);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_fapl_direct: " + err);
        }
        _createH5File(fapl_id);
        deleteH5file();
    }

    @Test
    public void testH5Pset_fapl_windows() {
        if (HDF5Constants.H5FD_WINDOWS < 0)
            return;
        try {
            H5.H5Pset_fapl_windows(fapl_id);
            long driver_type = H5.H5Pget_driver(fapl_id);
            assertTrue("H5Pget_driver: windows = "+ driver_type, HDF5Constants.H5FD_WINDOWS==driver_type);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_fapl_windows: " + err);
        }
        _createH5File(fapl_id);
        deleteH5file();
    }

    @Test
    public void testH5Pmulti_transform() {
        if (HDF5Constants.H5FD_MULTI < 0)
            return;
        String f_to_c = "(5/9.0)*(x-32)";
        double windchillFread[][] = new double[DIMF_X][DIMF_Y];
        double windchillC;
        NumberFormat formatter = new DecimalFormat("#0.000");
        long HADDRMAX = HDF5Constants.H5FD_DEFAULT_HADDR_SIZE;

        int[] member_map = new int[HDF5Constants.H5FD_MEM_NTYPES];
        long[] member_fapl = new long[HDF5Constants.H5FD_MEM_NTYPES];
        String[] member_name = new String[HDF5Constants.H5FD_MEM_NTYPES];
        long[] member_addr = new long[HDF5Constants.H5FD_MEM_NTYPES];

        try {
            H5.H5Pset_data_transform(plist_id, f_to_c);
            H5.H5Pset_btree_ratios(btplist_id, 0.1, 0.5, 0.7);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pdata_transform: " + err);
        }

        for(int mt=HDF5Constants.H5FD_MEM_DEFAULT; mt<HDF5Constants.H5FD_MEM_NTYPES; mt++) {
            member_fapl[mt] = HDF5Constants.H5P_DEFAULT;
            member_map[mt] = HDF5Constants.H5FD_MEM_SUPER;
        }
        member_map[HDF5Constants.H5FD_MEM_DRAW] = HDF5Constants.H5FD_MEM_DRAW;
        member_map[HDF5Constants.H5FD_MEM_BTREE] = HDF5Constants.H5FD_MEM_BTREE;
        member_map[HDF5Constants.H5FD_MEM_GHEAP] = HDF5Constants.H5FD_MEM_GHEAP;

        member_name[HDF5Constants.H5FD_MEM_SUPER] = new String("%s-super.h5");
        member_addr[HDF5Constants.H5FD_MEM_SUPER] = 0;

        member_name[HDF5Constants.H5FD_MEM_BTREE] = new String("%s-btree.h5");
        member_addr[HDF5Constants.H5FD_MEM_BTREE] = HADDRMAX/4;

        member_name[HDF5Constants.H5FD_MEM_DRAW] = new String("%s-draw.h5");
        member_addr[HDF5Constants.H5FD_MEM_DRAW] = HADDRMAX/2;

        member_name[HDF5Constants.H5FD_MEM_GHEAP] = new String("%s-gheap.h5");
        member_addr[HDF5Constants.H5FD_MEM_GHEAP] = (HADDRMAX/4)*3;

        try {
            H5.H5Pset_fapl_multi(fapl_id, member_map, member_fapl, member_name, member_addr, true);
            long driver_type = H5.H5Pget_driver(fapl_id);
            assertTrue("H5Pget_driver: muti = "+ driver_type, HDF5Constants.H5FD_MULTI==driver_type);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pset_fapl_muti: " + err);
        }
        try {
            boolean relax = H5.H5Pget_fapl_multi(fapl_id, member_map, member_fapl, member_name, member_addr);
            assertTrue("H5Pget_fapl_muti: relax ", relax);
            assertTrue("H5Pget_fapl_muti: member_map="+member_map[HDF5Constants.H5FD_MEM_DEFAULT], member_map[HDF5Constants.H5FD_MEM_DEFAULT] == HDF5Constants.H5FD_MEM_SUPER);
            assertTrue("H5Pget_fapl_muti: member_map="+member_map[HDF5Constants.H5FD_MEM_DRAW], member_map[HDF5Constants.H5FD_MEM_DRAW] == HDF5Constants.H5FD_MEM_DRAW);
            assertTrue("H5Pget_fapl_muti: member_map="+member_map[HDF5Constants.H5FD_MEM_BTREE], member_map[HDF5Constants.H5FD_MEM_BTREE] == HDF5Constants.H5FD_MEM_BTREE);
            assertTrue("H5Pget_fapl_muti: member_map="+member_map[HDF5Constants.H5FD_MEM_GHEAP], member_map[HDF5Constants.H5FD_MEM_GHEAP] == HDF5Constants.H5FD_MEM_GHEAP);

            assertTrue("H5Pget_fapl_muti: member_fapl ", H5.H5P_equal(member_fapl[HDF5Constants.H5FD_MEM_DEFAULT], HDF5Constants.H5P_FILE_ACCESS_DEFAULT));
            assertTrue("H5Pget_fapl_muti: member_addr="+member_addr[HDF5Constants.H5FD_MEM_DEFAULT], member_addr[HDF5Constants.H5FD_MEM_DEFAULT] == 0);
            assertTrue("H5Pget_fapl_muti: member_name="+member_name[HDF5Constants.H5FD_MEM_SUPER], member_name[HDF5Constants.H5FD_MEM_SUPER].compareTo("%s-super.h5")==0);

            assertTrue("H5Pget_fapl_muti: member_name="+member_name[HDF5Constants.H5FD_MEM_BTREE], member_name[HDF5Constants.H5FD_MEM_BTREE].compareTo("%s-btree.h5")==0);
            assertTrue("H5Pget_fapl_muti: member_addr="+member_addr[HDF5Constants.H5FD_MEM_BTREE], member_addr[HDF5Constants.H5FD_MEM_BTREE] == HADDRMAX/4);

            assertTrue("H5Pget_fapl_muti: member_name="+member_name[HDF5Constants.H5FD_MEM_DRAW], member_name[HDF5Constants.H5FD_MEM_DRAW].compareTo("%s-draw.h5")==0);
            assertTrue("H5Pget_fapl_muti: member_addr="+member_addr[HDF5Constants.H5FD_MEM_DRAW], member_addr[HDF5Constants.H5FD_MEM_DRAW] == HADDRMAX/2);

            assertTrue("H5Pget_fapl_muti: member_name="+member_name[HDF5Constants.H5FD_MEM_GHEAP], member_name[HDF5Constants.H5FD_MEM_GHEAP].compareTo("%s-gheap.h5")==0);
            assertTrue("H5Pget_fapl_muti: member_addr="+member_addr[HDF5Constants.H5FD_MEM_GHEAP], member_addr[HDF5Constants.H5FD_MEM_GHEAP] == (HADDRMAX/4)*3);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pget_fapl_muti: " + err);
        }
        try {
            _createH5multiFile(fapl_id);
            long file_size = H5.H5Fget_filesize(H5fid);
            assertTrue("H5Pget_fapl_muti: file_size ", file_size >= HADDRMAX/4 || file_size <= HADDRMAX/2);
            _createH5multiFileDS();
            _createFloatDataset();
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pdata_transform: " + err);
        }
        try {
            H5.H5Dwrite(H5Fdid, HDF5Constants.H5T_NATIVE_DOUBLE, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                    plist_id, windchillF);
            H5.H5Dread(H5Fdid, HDF5Constants.H5T_NATIVE_DOUBLE, HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                    HDF5Constants.H5P_DEFAULT, windchillFread);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Pdata_transform: " + err);
        }
        for(int row = 0; row < DIMF_X; row++) {
            for(int col = 0; col < DIMF_Y; col++) {
                windchillC = (5/9.0)*(windchillF[row][col]-32);
                String Cstr = formatter.format(windchillC);
                String Fread = formatter.format(windchillFread[row][col]);
                assertTrue("H5Pdata_transform: <"+row+","+col+">"+Fread+"="+Cstr, Fread.compareTo(Cstr)==0);
            }
        }
        deleteH5multifile();
        File file = new File(H5_MULTI_FILE+"-super.h5");
        if (file.exists()) {
            try {
                file.delete();
            }
            catch (SecurityException e) {
                ;// e.printStackTrace();
            }
        }
        file = new File(H5_MULTI_FILE+"-btree.h5");
        if (file.exists()) {
            try {
                file.delete();
            }
            catch (SecurityException e) {
                ;// e.printStackTrace();
            }
        }
        file = new File(H5_MULTI_FILE+"-draw.h5");
        if (file.exists()) {
            try {
                file.delete();
            }
            catch (SecurityException e) {
                ;// e.printStackTrace();
            }
        }
        file = new File(H5_MULTI_FILE+"-gheap.h5");
        if (file.exists()) {
            try {
                file.delete();
            }
            catch (SecurityException e) {
                ;// e.printStackTrace();
            }
        }
    }

    @Test
    public void testH5Fmdc_logging() {
        boolean[] mdc_logging_status = {false, false};
        boolean[] mdc_log_options = {false, false};

        try {
            boolean is_enabled = true;
            boolean start_on_access = false;
            H5.H5Pset_mdc_log_options(fapl_id, is_enabled, H5_LOG_FILE, start_on_access);

            String location = H5.H5Pget_mdc_log_options(fapl_id, mdc_log_options);
            assertTrue("H5.H5Pget_mdc_log_options: is_enabled", mdc_log_options[0]);
            assertFalse("H5.H5Pget_mdc_log_options: start_on_access_out", mdc_log_options[1]);

            H5.H5Pset_libver_bounds(fapl_id, HDF5Constants.H5F_LIBVER_LATEST, HDF5Constants.H5F_LIBVER_LATEST);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("mdc_log_option: " + err);
        }
        _createH5File(fapl_id);

        try {
            H5.H5Fget_mdc_logging_status(H5fid, mdc_logging_status);
        }
        catch (Throwable err) {
            fail("H5.H5Fget_mdc_logging_status: " + err);
        }
        assertTrue("initial: is_enabled", mdc_logging_status[0]);
        assertFalse("initial: is_currently_logging", mdc_logging_status[1]);

        try {
            H5.H5Fstart_mdc_logging(H5fid);
            H5.H5Fget_mdc_logging_status(H5fid, mdc_logging_status);
        }
        catch (Throwable err) {
            fail("start H5.H5Fget_mdc_logging_status: " + err);
        }
        assertTrue("start: is_enabled", mdc_logging_status[0]);
        assertTrue("start: is_currently_logging", mdc_logging_status[1]);

        try {
            H5.H5Fstop_mdc_logging(H5fid);
            H5.H5Fget_mdc_logging_status(H5fid, mdc_logging_status);
        }
        catch (Throwable err) {
            fail("stop H5.H5Fget_mdc_logging_status: " + err);
        }
//        assertFalse("stop: is_enabled", mdc_logging_status[0]);
        assertFalse("stop: is_currently_logging", mdc_logging_status[1]);

        deleteH5file();
        _deleteLogFile();
    }

    @Test
    public void testH5P_evict_on_close() {
        boolean ret_val_id = false;
        try {
            H5.H5Pset_evict_on_close(fapl_id, true);
            ret_val_id = H5.H5Pget_evict_on_close(fapl_id);
            assertTrue("H5P_evict_on_close", ret_val_id);
        }
        catch (HDF5PropertyListInterfaceException err) {
            // parallel is not supported
            if (err.getMinorErrorNumber() != HDF5Constants.H5E_UNSUPPORTED) {
                err.printStackTrace();
                fail("H5P_evict_on_close: " + err);
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5P_evict_on_close: " + err);
        }
    }
}
