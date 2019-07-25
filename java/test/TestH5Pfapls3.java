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
import static org.junit.Assert.assertNull;
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
import hdf.hdf5lib.structs.H5FD_hdfs_fapl_t;
import hdf.hdf5lib.structs.H5FD_ros3_fapl_t;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5Pfapls3 {
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
    public void testH5Pset_fapl_ros3()
    throws Exception
    {
        if (HDF5Constants.H5FD_ROS3 < 0)
            return;

        final H5FD_ros3_fapl_t config = new H5FD_ros3_fapl_t();
        assertEquals("Default fapl has unexpected contents",
                new H5FD_ros3_fapl_t("", "", ""),
                config);

        H5.H5Pset_fapl_ros3(fapl_id, config);

        assertEquals("driver types don't match",
                HDF5Constants.H5FD_ROS3,
                H5.H5Pget_driver(fapl_id));

        /* get_fapl_ros3 can throw exception in error cases */
        H5FD_ros3_fapl_t copy = H5.H5Pget_fapl_ros3(fapl_id);
        assertEquals("contents of fapl set and get don't match",
                new H5FD_ros3_fapl_t("", "", ""),
                copy);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Pget_fapl_ros3_invalid_fapl_id()
    throws Exception
    {
        if (HDF5Constants.H5FD_ROS3 < 0)
            throw new HDF5LibraryException("skip");
        H5FD_ros3_fapl_t fails = H5.H5Pget_fapl_ros3(-1);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Pget_fapl_ros3_fapl_id_of_wrong_driver_type()
    throws Exception
    {
        if (HDF5Constants.H5FD_ROS3 < 0)
            throw new HDF5LibraryException("skip");
        if (HDF5Constants.H5FD_SEC2 < 0 )
            throw new HDF5LibraryException("skip");
            /* TODO: for now, test against a sec2 fapl only */

        H5.H5Pset_fapl_sec2(fapl_id);
        assertEquals("fapl_id was not set properly",
                HDF5Constants.H5FD_SEC2,
                H5.H5Pget_driver(fapl_id));
        H5FD_ros3_fapl_t fails = H5.H5Pget_fapl_ros3(fapl_id);
    }

    @Test
    public void testH5Pset_fapl_ros3_specified()
    throws Exception
    {
        if (HDF5Constants.H5FD_ROS3 < 0)
            return;

        String region  = "us-east-1";
        String acc_id  = "my_access_id";
        String acc_key = "my_access_key";

        final H5FD_ros3_fapl_t config = new H5FD_ros3_fapl_t(
                region,
                acc_id,
                acc_key);
        H5.H5Pset_fapl_ros3(fapl_id, config);
        assertEquals("driver types don't match",
                HDF5Constants.H5FD_ROS3,
                H5.H5Pget_driver(fapl_id));

        H5FD_ros3_fapl_t copy = H5.H5Pget_fapl_ros3(fapl_id);
        assertEquals("contents of fapl set and get don't match",
                new H5FD_ros3_fapl_t(region, acc_id, acc_key),
                copy);
    }

}
