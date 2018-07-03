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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.exceptions.HDF5LibraryException;
import hdf.hdf5lib.structs.H5G_info_t;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5G {
    @Rule public TestName testname = new TestName();
    private static final String H5_FILE = "testG.h5";
    private static final String H5_FILE2 = "testG2.h5";
    private static final String[] GROUPS = { "/G1", "/G1/G11", "/G1/G12",
            "/G1/G11/G111", "/G1/G11/G112", "/G1/G11/G113", "/G1/G11/G114" };
    private static final String[] GROUPS2 = { "/G1", "/G1/G14", "/G1/G12", "/G1/G13", "/G1/G11"};
    long H5fid = -1;
    long H5fid2 = -1;

    private final long _createGroup(long fid, String name) {
        long gid = -1;
        try {
            gid = H5.H5Gcreate(fid, name, HDF5Constants.H5P_DEFAULT,
                        HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Gcreate: " + err);
        }
        assertTrue("TestH5G._createGroup: ", gid > 0);

        return gid;
    }

    private final long _createGroup2(long fid, String name) {
        long gid = -1;
        long gcpl = -1;
        try {
            gcpl = H5.H5Pcreate(HDF5Constants.H5P_GROUP_CREATE); //create gcpl
        }
        catch (final Exception ex) {
            fail("H5.H5Pcreate(): " + ex);
        }
        assertTrue("TestH5G._createGroup2: ", gcpl >= 0);
        try {
            H5.H5Pset_link_creation_order(gcpl, HDF5Constants.H5P_CRT_ORDER_TRACKED
                    + HDF5Constants.H5P_CRT_ORDER_INDEXED); // Set link creation order
        }
        catch (final Exception ex) {
            try {H5.H5Pclose(gcpl);} catch (final Exception exx) {}
            fail("H5.H5Pset_link_creation_order: " + ex);
        }
        try {
            gid = H5.H5Gcreate(fid, name, HDF5Constants.H5P_DEFAULT,
                    gcpl, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Gcreate: " + err);
        }
        finally {
            try {H5.H5Pclose(gcpl);} catch (final Exception ex) {}
        }
        assertTrue("TestH5G._createGroup2: ", gid > 0);

        return gid;
    }

    private final long _openGroup(long fid, String name) {
        long gid = -1;
        try {
            gid = H5.H5Gopen(fid, name, HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            gid = -1;
            err.printStackTrace();
            fail("H5.H5Gopen: " + err);
        }
        assertTrue("TestH5G._openGroup: ", gid > 0);

        return gid;
    }

    private final void _deleteFile(String filename) {
        File file = new File(filename);

        if (file.exists()) {
            try {file.delete();} catch (SecurityException e) {}
        }
    }

    @Before
    public void createH5file()
            throws HDF5LibraryException, NullPointerException {
        assertTrue("H5 open ids is 0",H5.getOpenIDCount()==0);
        System.out.print(testname.getMethodName());

        try {
            H5fid = H5.H5Fcreate(H5_FILE, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                    HDF5Constants.H5P_DEFAULT);

            H5fid2 = H5.H5Fcreate(H5_FILE2, HDF5Constants.H5F_ACC_TRUNC, HDF5Constants.H5P_DEFAULT,
                    HDF5Constants.H5P_DEFAULT);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5G.createH5file: " + err);
        }
        assertTrue("TestH5G.createH5file: H5.H5Fcreate: ", H5fid > 0);
        assertTrue("TestH5G.createH5file: H5.H5Fcreate: ", H5fid2 > 0);

        long gid = -1;

        for (int i = 0; i < GROUPS.length; i++) {
            gid = _createGroup(H5fid, GROUPS[i]);
            try {H5.H5Gclose(gid);} catch (Exception ex) {}
        }

        for (int i = 0; i < GROUPS2.length; i++) {
            gid = _createGroup2(H5fid2, GROUPS2[i]);
            try {H5.H5Gclose(gid);} catch (Exception ex) {}
        }

        H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
        H5.H5Fflush(H5fid2, HDF5Constants.H5F_SCOPE_LOCAL);
    }

    @After
    public void deleteH5file() throws HDF5LibraryException {
        if (H5fid > 0) {
            try {H5.H5Fclose(H5fid);} catch (Exception ex) {}
        }
        if (H5fid2 > 0) {
            try {H5.H5Fclose(H5fid2);} catch (Exception ex) {}
        }
       _deleteFile(H5_FILE);
       _deleteFile(H5_FILE2);
       System.out.println();
    }

    @Test
    public void testH5Gopen() {
        long gid = -1;
        for (int i = 0; i < GROUPS.length; i++) {
            try {
                gid = H5.H5Gopen(H5fid, GROUPS[i], HDF5Constants.H5P_DEFAULT);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("TestH5G.testH5Gopen: H5.H5Gopen: " + err);
            }
            assertTrue("TestH5G.testH5Gopen: ", gid > 0);
            try {
                H5.H5Gclose(gid);
            }
            catch (Exception ex) {
            }
        }
    }

    @Test
    public void testH5Gget_create_plist() {
        long gid = -1;
        long pid = -1;

        for (int i = 0; i < GROUPS.length; i++) {
            try {
                gid = H5.H5Gopen(H5fid, GROUPS[i], HDF5Constants.H5P_DEFAULT);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("TestH5G.testH5Gget_create_plist: H5.H5Gopen: " + err);
            }
            assertTrue("TestH5G.testH5Gget_create_plist: ", gid > 0);

            try {
                pid = H5.H5Gget_create_plist(gid);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("TestH5G.testH5Gget_create_plist: H5.H5Gget_create_plist: " + err);
            }
            assertTrue("TestH5G.testH5Gget_create_plist: ", pid > 0);

            try {
                H5.H5Gclose(gid);
            }
            catch (Exception ex) {
            }
        }
    }

    @Test
    public void testH5Gget_info() {
        H5G_info_t info = null;

        for (int i = 0; i < GROUPS.length; i++) {

            try {
                info = H5.H5Gget_info(H5fid);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("TestH5G.testH5Gget_info: H5.H5Gget_info: " + err);
            }
            assertNotNull("TestH5G.testH5Gget_info: ", info);
        }
    }

    @Test
    public void testH5Gget_info_by_name() {
        H5G_info_t info = null;

        for (int i = 0; i < GROUPS.length; i++) {
            try {
                info = H5.H5Gget_info_by_name(H5fid, GROUPS[i],
                        HDF5Constants.H5P_DEFAULT);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("TestH5G.testH5Gget_info_by_name: H5.H5Gget_info_by_name: " + err);
            }
            assertNotNull("TestH5G.testH5Gget_info_by_name: ", info);
        }
    }

    @Test
    public void testH5Gget_info_by_idx() {
        H5G_info_t info = null;
        for (int i = 0; i < 2; i++) {
            try {
                info = H5.H5Gget_info_by_idx(H5fid, "/G1",
                        HDF5Constants.H5_INDEX_NAME, HDF5Constants.H5_ITER_INC,
                        i, HDF5Constants.H5P_DEFAULT);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("TestH5G.testH5Gget_info_by_idx: H5.H5Gget_info_by_idx: " + err);
            }
            assertNotNull("TestH5G.testH5Gget_info_by_idx: ", info);
        }
    }

    @Test
    public void testH5Gget_obj_info_all() {
        H5G_info_t info = null;

        long gid = _openGroup(H5fid, GROUPS[0]);

        try {
            info = H5.H5Gget_info(gid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5G.testH5Gget_obj_info_all: H5.H5Gget_info: " + err);
        }
        finally {
            try {H5.H5Gclose(gid);} catch (Exception ex) { }
        }
        assertNotNull("TestH5G.testH5Gget_obj_info_all: ", info);
        assertTrue("TestH5G.testH5Gget_obj_info_all: number of links is empty", info.nlinks > 0);
        String objNames[] = new String[(int) info.nlinks];
        int objTypes[] = new int[(int) info.nlinks];
        int lnkTypes[] = new int[(int) info.nlinks];
        long objRefs[] = new long[(int) info.nlinks];

        int names_found = 0;
        try {
            names_found = H5.H5Gget_obj_info_all(H5fid, GROUPS[0], objNames,
                    objTypes, lnkTypes, objRefs, HDF5Constants.H5_INDEX_NAME);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5G.testH5Gget_obj_info_all: H5.H5Gget_obj_info_all: " + err);
        }

        assertTrue("number found[" + names_found + "] different than expected["
                + objNames.length + "]", names_found == objNames.length);
        for (int i = 0; i < objNames.length; i++) {
            assertNotNull("name #" + i + " does not exist", objNames[i]);
            assertTrue("TestH5G.testH5Gget_obj_info_all: ", objNames[i].length() > 0);
        }
    }

    @Test
    public void testH5Gget_obj_info_all_gid() {
        H5G_info_t info = null;

        long gid = _openGroup(H5fid, GROUPS[0]);

        try {
            info = H5.H5Gget_info(gid);
            assertNotNull("TestH5G.testH5Gget_obj_info_all_gid: ", info);
            assertTrue("TestH5G.testH5Gget_obj_info_all_gid: number of links is empty", info.nlinks > 0);
            String objNames[] = new String[(int) info.nlinks];
            long objRefs[] = new long[(int) info.nlinks];
            int lnkTypes[] = new int[(int) info.nlinks];
            int objTypes[] = new int[(int) info.nlinks];

            int names_found = 0;
            try {
                names_found = H5.H5Gget_obj_info_all(gid, null, objNames, objTypes, lnkTypes,
                        objRefs, HDF5Constants.H5_INDEX_NAME);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("TestH5G.testH5Gget_obj_info_all_gid: H5.H5Gget_obj_info_all: " + err);
            }

            assertTrue("TestH5G.testH5Gget_obj_info_all_gid: number found[" + names_found + "] different than expected["
                    + objNames.length + "]", names_found == objNames.length);
            for (int i = 0; i < objNames.length; i++) {
                assertNotNull("TestH5G.testH5Gget_obj_info_all_gid: name #" + i + " does not exist", objNames[i]);
                assertTrue("TestH5G.testH5Gget_obj_info_all_gid: ", objNames[i].length() > 0);
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5G.testH5Gget_obj_info_all_gid: H5.H5Gget_info: " + err);
        }
        finally {
            try {H5.H5Gclose(gid);} catch (Exception ex) { }
        }
    }

    @Test
    public void testH5Gget_obj_info_all_gid2() {
        H5G_info_t info = null;

        long gid = _openGroup(H5fid, GROUPS[1]);

        try {
            info = H5.H5Gget_info(gid);
            assertNotNull("TestH5G.testH5Gget_obj_info_all_gid2: ", info);
            assertTrue("TestH5G.testH5Gget_obj_info_all_gid2: number of links is empty", info.nlinks > 0);
            String objNames[] = new String[(int) info.nlinks];
            long objRefs[] = new long[(int) info.nlinks];
            int lnkTypes[] = new int[(int) info.nlinks];
            int objTypes[] = new int[(int) info.nlinks];

            int names_found = 0;
            try {
                names_found = H5.H5Gget_obj_info_all(gid, null, objNames, objTypes, lnkTypes,
                        objRefs, HDF5Constants.H5_INDEX_NAME);
            }
            catch (Throwable err) {
                err.printStackTrace();
                fail("TestH5G.testH5Gget_obj_info_all_gid2: H5.H5Gget_obj_info_all: " + err);
            }

            assertTrue("TestH5G.testH5Gget_obj_info_all_gid2: number found[" + names_found + "] different than expected["
                    + objNames.length + "]", names_found == objNames.length);
            for (int i = 0; i < objNames.length; i++) {
                assertNotNull("TestH5G.testH5Gget_obj_info_all_gid2: name #" + i + " does not exist", objNames[i]);
                assertTrue("TestH5G.testH5Gget_obj_info_all_gid2: ", objNames[i].length() > 0);
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5G.testH5Gget_obj_info_all_gid2: H5.H5Gget_info: " + err);
        }
        finally {
            try {H5.H5Gclose(gid);} catch (Exception ex) { }
        }
    }

    @Test
    public void testH5Gget_obj_info_max() {
        long gid = _openGroup(H5fid, GROUPS[0]);
        long groups_max_size = GROUPS.length + 1;
        String objNames[] = new String[(int)groups_max_size];
        int objTypes[] = new int[(int)groups_max_size];
        int lnkTypes[] = new int[(int)groups_max_size];
        long objRefs[] = new long[(int)groups_max_size];

        int names_found = 0;
        try {
            names_found = H5.H5Gget_obj_info_max(gid, objNames, objTypes, lnkTypes,
                    objRefs, groups_max_size);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5G.testH5Gget_obj_info_max: H5.H5Gget_obj_info_max: " + err);
        }
        finally {
            try {H5.H5Gclose(gid);} catch (Exception ex) { }
        }

        // expected number does not include root group
        assertTrue("TestH5G.testH5Gget_obj_info_max: number found[" + names_found + "] different than expected["
                + (GROUPS.length - 1) + "]", names_found == (GROUPS.length - 1));
        for (int i = 0; i < GROUPS.length-1; i++) {
            assertNotNull("TestH5G.testH5Gget_obj_info_max: name #"+i+" does not exist",objNames[i]);
            assertTrue("TestH5G.testH5Gget_obj_info_max: ", objNames[i].length()>0);
        }
    }

    @Test
    public void testH5Gget_obj_info_max_limit() {
        long gid = _openGroup(H5fid, GROUPS[0]);
        long groups_max_size = GROUPS.length - 3;
        String objNames[] = new String[(int)groups_max_size];
        int objTypes[] = new int[(int)groups_max_size];
        int lnkTypes[] = new int[(int)groups_max_size];
        long objRefs[] = new long[(int)groups_max_size];

        int names_found = 0;
        try {
            names_found = H5.H5Gget_obj_info_max(gid, objNames, objTypes, lnkTypes,
                    objRefs, groups_max_size);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5G.testH5Gget_obj_info_max_limit: H5.H5Gget_obj_info_max: " + err);
        }
        finally {
            try {H5.H5Gclose(gid);} catch (Exception ex) { }
        }

        assertTrue("TestH5G.testH5Gget_obj_info_max_limit: number found[" + names_found + "] different than expected["
                + groups_max_size + "]", names_found == groups_max_size);
        for (int i = 0; i < objNames.length; i++) {
            assertNotNull("TestH5G.testH5Gget_obj_info_max_limit: name #" + i + " does not exist", objNames[i]);
            assertTrue("TestH5G.testH5Gget_obj_info_max_limit: ", objNames[i].length() > 0);
        }
    }

    @Test
    public void testH5Gget_obj_info_all_byIndexType() {
        H5G_info_t info = null;

        long gid = _openGroup(H5fid2, GROUPS2[0]);

        try {
            info = H5.H5Gget_info(gid);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5G.testH5Gget_obj_info_all_byIndexType: H5.H5Gget_info: " + err);
        }
        finally {
            try {H5.H5Gclose(gid);} catch (Exception ex) { }
        }

        assertNotNull("TestH5G.testH5Gget_obj_info_all_byIndexType: ", info);
        assertTrue("TestH5G.testH5Gget_obj_info_all_byIndexType: number of links is empty", info.nlinks > 0);
        String objNames[] = new String[(int) info.nlinks];
        int objTypes[] = new int[(int) info.nlinks];
        int lnkTypes[] = new int[(int) info.nlinks];
        long objRefs[] = new long[(int) info.nlinks];

        try {
            H5.H5Gget_obj_info_all(H5fid2, GROUPS2[0], objNames,
                    objTypes, lnkTypes, objRefs, HDF5Constants.H5_INDEX_CRT_ORDER);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5G.testH5Gget_obj_info_all_byIndexType: H5.H5Gget_obj_info_all: " + err);
        }

        assertEquals("G12",objNames[1]);
        assertEquals("G13", objNames[2] );
        assertEquals("G11", objNames[3] );

        try {
           H5.H5Gget_obj_info_all(H5fid2, GROUPS2[0], objNames,
                    objTypes, lnkTypes, objRefs, HDF5Constants.H5_INDEX_NAME);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5G.testH5Gget_obj_info_all_byIndexType: H5.H5Gget_obj_info_all: " + err);
        }

        assertEquals("G12",objNames[1]);
        assertEquals("G13", objNames[2] );
        assertEquals("G14", objNames[3] );
    }

}
