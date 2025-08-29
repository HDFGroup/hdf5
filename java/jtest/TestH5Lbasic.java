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

package test;

import static org.hdfgroup.javahdf5.hdf5_h.*;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.exceptions.HDF5LibraryException;

import org.hdfgroup.javahdf5.*;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5Lbasic {
    @Rule
    public TestName testname            = new TestName();
    private static final String H5_FILE = "h5ex_g_iterateL1.hdf";
    long H5fid                          = H5I_INVALID_HID();

    @Before
    public void openH5file() throws HDF5LibraryException, NullPointerException
    {
        System.out.print(testname.getMethodName());

        try {
            H5fid = H5Fopen(H5_FILE, H5F_ACC_RDONLY(), H5P_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Fopen: openH5file: " + err);
        }
    }

    @After
    public void closeH5file() throws HDF5LibraryException
    {
        if (H5fid > 0) {
            try {
                H5Fclose(H5fid);
            }
            catch (Exception ex) {
            }
        }
        System.out.println();
    }

    @Test
    public void testH5Lexists()
    {
        boolean link_exists = false;
        try {
            link_exists = H5Lexists(H5fid, "None", H5P_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Lexists: " + err);
        }
        assertFalse("H5Lexists ", link_exists);
        try {
            link_exists = H5Lexists(H5fid, "DS1", H5P_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Lexists: " + err);
        }
        assertTrue("H5Lexists ", link_exists);
        try {
            link_exists = H5Lexists(H5fid, "G1/DS2", H5P_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Lexists: " + err);
        }
        assertTrue("H5Lexists ", link_exists);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lget_info_not_exist() throws Throwable
    {
        H5Lget_info(H5fid, "None", H5P_DEFAULT());
    }

    @Test
    public void testH5Lget_info_dataset()
    {
        H5L_info_t link_info = null;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment  = arena.allocateFrom("DS1");
            MemorySegment linfo_segment = arena.allocate(H5L_info2_t.sizeof());
            if (H5Lget_info2(H5fid, name_segment, linfo_segment, H5P_DEFAULT()) < 0) {
                throw new HDF5LibraryException("H5Lget_info2 failed");
            }
            // Unpack the H5L_info2_t from the MemorySegment
            if (H5L_info2_t.type(linfo_segment) == H5L_TYPE_HARD) {
                H5O_token_t token = new H5O_token_t(H5L_info2_t.u(linfo_segment).toByteArray());
                link_info =
                    new H5L_info_t(H5L_info2_t.type(linfo_segment), H5L_info2_t.corder_valid(linfo_segment),
                                   H5L_info2_t.corder(linfo_segment), H5L_info2_t.cset(linfo_segment), token);
            }
            else
                link_info =
                    new H5L_info_t(H5L_info2_t.type(linfo_segment), H5L_info2_t.corder_valid(linfo_segment),
                                   H5L_info2_t.corder(linfo_segment), H5L_info2_t.cset(linfo_segment),
                                   H5L_info2_t.u.val_size(linfo_segment));
            log.trace("H5Lget_info2: type={}", info.type);
            if (link_info.type == HDF5Constants.H5L_TYPE_ERROR) {
                throw new HDF5LibraryException("H5Lget_info2: Invalid link type");
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("Arena: " + err);
        }
        assertFalse("H5Lget_info ", link_info == null);
        assertTrue("H5Lget_info link type", link_info.type == H5L_TYPE_HARD());
    }

    @Test
    public void testH5Lget_info_hardlink()
    {
        H5L_info_t link_info = null;
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment  = arena.allocateFrom("L1");
            MemorySegment linfo_segment = arena.allocate(H5L_info2_t.sizeof());
            if (H5Lget_info2(H5fid, name_segment, linfo_segment, H5P_DEFAULT()) < 0) {
                throw new HDF5LibraryException("H5Lget_info2 failed");
            }
            // Unpack the H5L_info2_t from the MemorySegment
            if (H5L_info2_t.type(linfo_segment) == H5L_TYPE_HARD) {
                H5O_token_t token = new H5O_token_t(H5L_info2_t.u(linfo_segment).toByteArray());
                link_info =
                    new H5L_info_t(H5L_info2_t.type(linfo_segment), H5L_info2_t.corder_valid(linfo_segment),
                                   H5L_info2_t.corder(linfo_segment), H5L_info2_t.cset(linfo_segment), token);
            }
            else
                link_info =
                    new H5L_info_t(H5L_info2_t.type(linfo_segment), H5L_info2_t.corder_valid(linfo_segment),
                                   H5L_info2_t.corder(linfo_segment), H5L_info2_t.cset(linfo_segment),
                                   H5L_info2_t.u.val_size(linfo_segment));
            log.trace("H5Lget_info2: type={}", info.type);
            if (link_info.type == HDF5Constants.H5L_TYPE_ERROR) {
                throw new HDF5LibraryException("H5Lget_info2: Invalid link type");
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("Arena: " + err);
        }
        assertFalse("H5Lget_info", link_info == null);
        assertTrue("H5Lget_info link type", link_info.type == H5L_TYPE_HARD());
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lget_info_by_idx2_name_not_exist_name() throws Throwable
    {
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment  = arena.allocateFrom("None");
            MemorySegment linfo_segment = arena.allocate(H5L_info2_t.sizeof());
            if (H5Lget_info_by_idx2(H5fid, name_segment, H5_INDEX_NAME(), H5_ITER_INC(), 0, linfo_segment,
                                    H5P_DEFAULT()) < 0) {
                throw new HDF5LibraryException("H5Lget_info_by_idx2 failed");
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("Arena: " + err);
        }
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lget_info_by_idx2_name_not_exist_create() throws Throwable
    {
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment  = arena.allocateFrom("None");
            MemorySegment linfo_segment = arena.allocate(H5L_info2_t.sizeof());
            if (H5Lget_info_by_idx2(H5fid, name_segment, H5_INDEX_CRT_ORDER(), H5_ITER_INC(), 0,
                                    linfo_segment, H5P_DEFAULT()) < 0) {
                throw new HDF5LibraryException("H5Lget_info_by_idx2 failed");
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("Arena: " + err);
        }
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lget_info_by_idx2_not_exist_name() throws Throwable
    {
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment  = arena.allocateFrom("/");
            MemorySegment linfo_segment = arena.allocate(H5L_info2_t.sizeof());
            if (H5Lget_info_by_idx2(H5fid, name_segment, H5_INDEX_NAME(), H5_ITER_INC(), 5, linfo_segment,
                                    H5P_DEFAULT()) < 0) {
                throw new HDF5LibraryException("H5Lget_info_by_idx2 failed");
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("Arena: " + err);
        }
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lget_info_by_idx2_not_exist_create() throws Throwable
    {
        try (Arena arena = Arena.ofConfined()) {
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment  = arena.allocateFrom("/");
            MemorySegment linfo_segment = arena.allocate(H5L_info2_t.sizeof());
            if (H5Lget_info_by_idx2(H5fid, name_segment, H5_INDEX_CRT_ORDER(), H5_ITER_INC(), 5,
                                    linfo_segment, H5P_DEFAULT()) < 0) {
                throw new HDF5LibraryException("H5Lget_info_by_idx2 failed");
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("Arena: " + err);
        }
    }

    @Test
    public void testH5Lget_info_by_idx2_n0()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment linfo_segment = arena.allocate(H5L_info2_t.sizeof());
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom("/");
            if (H5Lget_info_by_idx2(H5fid, name_segment, H5_INDEX_NAME(), H5_ITER_INC(), 0, linfo_segment,
                                    H5P_DEFAULT()) < 0) {
                throw new HDF5LibraryException("H5Lget_info_by_idx2 failed");
            }
            log.trace("H5Lget_info_by_idx2: type={}", H5L_info2_t.type(linfo_segment));
            if (H5L_info2_t.type(linfo_segment) == H5L_TYPE_ERROR()) {
                throw new HDF5LibraryException("H5Lget_info_by_idx2: Invalid link type");
            }
            assertTrue("H5Lget_info_by_idx2 link type", H5L_info2_t.type(linfo_segment) == H5L_TYPE_HARD());

            MemorySegment linfo2_segment = arena.allocate(H5L_info2_t.sizeof());
            MemorySegment name_segment   = arena.allocateFrom("DS1");
            if (H5Lget_info2(H5fid, name_segment, linfo2_segment, H5P_DEFAULT()) < 0) {
                throw new HDF5LibraryException("H5Lget_info2 failed");
            }
            assertTrue("Link Value Size",
                       H5L_info2_t.u.val_size(linfo_segment) == H5L_info2_t.u.val_size(linfo2_segment));
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Lget_info_by_idx2: " + err);
        }
    }

    @Test
    public void testH5Lget_info_by_idx_n3()
    {
        try (Arena arena = Arena.ofConfined()) {
            MemorySegment linfo_segment = arena.allocate(H5L_info2_t.sizeof());
            // Allocate a MemorySegment to hold the string bytes
            MemorySegment name_segment = arena.allocateFrom("/");
            if (H5Lget_info_by_idx2(H5fid, name_segment, H5_INDEX_NAME(), H5_ITER_INC(), 3, linfo_segment,
                                    H5P_DEFAULT()) < 0) {
                throw new HDF5LibraryException("H5Lget_info_by_idx2 failed");
            }
            log.trace("H5Lget_info_by_idx2: type={}", H5L_info2_t.type(linfo_segment));
            assertTrue("H5Lget_info_by_idx link type", H5L_info2_t.type(linfo_segment) == H5L_TYPE_HARD());

            MemorySegment linfo2_segment = arena.allocate(H5L_info2_t.sizeof());
            MemorySegment name_segment   = arena.allocateFrom("L1");
            if (H5Lget_info2(H5fid, name_segment, linfo2_segment, H5P_DEFAULT()) < 0) {
                throw new HDF5LibraryException("H5Lget_info2 failed");
            }
            assertTrue("Link Value Size",
                       H5L_info2_t.u.val_size(linfo_segment) == H5L_info2_t.u.val_size(linfo2_segment));
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("Arena: " + err);
        }
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Lget_name_by_idx_not_exist() throws Throwable
    {
        H5Lget_name_by_idx(H5fid, "None", H5_INDEX_CRT_ORDER(), H5_ITER_INC(), 0, H5P_DEFAULT());
    }

    @Test
    public void testH5Lget_name_by_idx_n0()
    {
        String link_name = null;
        try {
            link_name = H5Lget_name_by_idx(H5fid, "/", H5_INDEX_NAME(), H5_ITER_INC(), 0, H5P_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Lget_name_by_idx: " + err);
        }
        assertFalse("H5Lget_name_by_idx ", link_name == null);
        assertTrue("Link Name ", link_name.compareTo("DS1") == 0);
    }

    @Test
    public void testH5Lget_name_by_idx_n3()
    {
        String link_name = null;
        try {
            link_name = H5Lget_name_by_idx(H5fid, "/", H5_INDEX_NAME(), H5_ITER_INC(), 3, H5P_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Lget_name_by_idx: " + err);
        }
        assertFalse("H5Lget_name_by_idx ", link_name == null);
        assertTrue("Link Name ", link_name.compareTo("L1") == 0);
    }

    @Test
    public void testH5Lvisit()
    {
        class idata {
            public String link_name = null;
            public int link_type    = -1;
            idata(String name, int type)
            {
                this.link_name = name;
                this.link_type = type;
            }
        }
        class H5L_iter_data extends H5L_iterate2_t {
            public ArrayList<idata> iterdata = new ArrayList<idata>();
        }
        H5L_iterate2_t iter_data = new H5L_iter_data();
        class H5L_iter_callback implements H5L_iterate2_t.Function {
            public int apply(long group, String name, H5L_info2_t info, H5L_iterate2_t op_data)
            {
                idata id = new idata(name, info.type);
                ((H5L_iter_data)op_data).iterdata.add(id);
                return 0;
            }
        }
        H5L_iterate2_t iter_cb = new H5L_iter_callback();
        try {
            H5Lvisit(H5fid, H5_INDEX_NAME(), H5_ITER_INC(), iter_cb, iter_data);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Lvisit: " + err);
        }
        assertFalse("H5Lvisit ", ((H5L_iter_data)iter_data).iterdata.isEmpty());
        assertTrue("H5Lvisit " + ((H5L_iter_data)iter_data).iterdata.size(),
                   ((H5L_iter_data)iter_data).iterdata.size() == 5);
        assertTrue("H5Lvisit " + (((H5L_iter_data)iter_data).iterdata.get(0)).link_name,
                   (((H5L_iter_data)iter_data).iterdata.get(0)).link_name.compareToIgnoreCase("DS1") == 0);
        assertTrue("H5Lvisit " + (((H5L_iter_data)iter_data).iterdata.get(1)).link_name,
                   (((H5L_iter_data)iter_data).iterdata.get(1)).link_name.compareToIgnoreCase("DT1") == 0);
        assertTrue("H5Lvisit " + (((H5L_iter_data)iter_data).iterdata.get(2)).link_name,
                   (((H5L_iter_data)iter_data).iterdata.get(2)).link_name.compareToIgnoreCase("G1") == 0);
        assertTrue("H5Lvisit " + (((H5L_iter_data)iter_data).iterdata.get(3)).link_name,
                   (((H5L_iter_data)iter_data).iterdata.get(3)).link_name.compareToIgnoreCase("G1/DS2") == 0);
        assertTrue("H5Lvisit " + (((H5L_iter_data)iter_data).iterdata.get(4)).link_name,
                   (((H5L_iter_data)iter_data).iterdata.get(4)).link_name.compareToIgnoreCase("L1") == 0);
    }

    @Test
    public void testH5Lvisit_by_name()
    {
        class idata {
            public String link_name = null;
            public int link_type    = -1;
            idata(String name, int type)
            {
                this.link_name = name;
                this.link_type = type;
            }
        }
        class H5L_iter_data extends H5L_iterate2_t {
            public ArrayList<idata> iterdata = new ArrayList<idata>();
        }
        H5L_iterate2_t iter_data = new H5L_iter_data();
        class H5L_iter_callback implements H5L_iterate2_t.Function {
            public int apply(long group, String name, H5L_info2_t info, H5L_iterate2_t op_data)
            {
                idata id = new idata(name, info.type);
                ((H5L_iter_data)op_data).iterdata.add(id);
                return 0;
            }
        }
        H5L_iterate2_t iter_cb = new H5L_iter_callback();
        try {
            H5Lvisit_by_name(H5fid, "G1", H5_INDEX_NAME(), H5_ITER_INC(), iter_cb, iter_data, H5P_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Lvisit_by_name: " + err);
        }
        assertFalse("H5Lvisit_by_name ", ((H5L_iter_data)iter_data).iterdata.isEmpty());
        assertTrue("H5Lvisit_by_name " + ((H5L_iter_data)iter_data).iterdata.size(),
                   ((H5L_iter_data)iter_data).iterdata.size() == 1);
        assertTrue("H5Lvisit_by_name " + (((H5L_iter_data)iter_data).iterdata.get(0)).link_name,
                   (((H5L_iter_data)iter_data).iterdata.get(0)).link_name.compareToIgnoreCase("DS2") == 0);
    }

    @Test
    public void testH5Literate()
    {
        class idata {
            public String link_name = null;
            public int link_type    = -1;
            idata(String name, int type)
            {
                this.link_name = name;
                this.link_type = type;
            }
        }
        class H5L_iter_data extends H5L_iterate2_t {
            public ArrayList<idata> iterdata = new ArrayList<idata>();
        }
        H5L_iterate2_t iter_data = new H5L_iter_data();
        class H5L_iter_callback implements H5L_iterate2_t.Function {
            public int apply(long group, String name, H5L_info2_t info, H5L_iterate2_t op_data)
            {
                idata id = new idata(name, info.type);
                ((H5L_iter_data)op_data).iterdata.add(id);
                return 0;
            }
        }
        H5L_iterate2_t iter_cb = new H5L_iter_callback();
        try {
            H5Literate(H5fid, H5_INDEX_NAME(), H5_ITER_INC(), 0L, iter_cb, iter_data);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Literate: " + err);
        }
        assertFalse("H5Literate ", ((H5L_iter_data)iter_data).iterdata.isEmpty());
        assertTrue("H5Literate " + ((H5L_iter_data)iter_data).iterdata.size(),
                   ((H5L_iter_data)iter_data).iterdata.size() == 4);
        assertTrue("H5Literate " + (((H5L_iter_data)iter_data).iterdata.get(0)).link_name,
                   (((H5L_iter_data)iter_data).iterdata.get(0)).link_name.compareToIgnoreCase("DS1") == 0);
        assertTrue("H5Literate " + (((H5L_iter_data)iter_data).iterdata.get(1)).link_name,
                   (((H5L_iter_data)iter_data).iterdata.get(1)).link_name.compareToIgnoreCase("DT1") == 0);
        assertTrue("H5Literate " + ((idata)((H5L_iter_data)iter_data).iterdata.get(2)).link_name,
                   (((H5L_iter_data)iter_data).iterdata.get(2)).link_name.compareToIgnoreCase("G1") == 0);
        assertTrue("H5Literate " + ((idata)((H5L_iter_data)iter_data).iterdata.get(3)).link_name,
                   ((idata)((H5L_iter_data)iter_data).iterdata.get(3)).link_name.compareToIgnoreCase("L1") ==
                       0);
    }

    @Test
    public void testH5Literate_by_name()
    {
        class idata {
            public String link_name = null;
            public int link_type    = -1;
            idata(String name, int type)
            {
                this.link_name = name;
                this.link_type = type;
            }
        }
        class H5L_iter_data extends H5L_iterate2_t {
            public ArrayList<idata> iterdata = new ArrayList<idata>();
        }
        H5L_iterate2_t iter_data = new H5L_iter_data();
        class H5L_iter_callback implements H5L_iterate2_t.Function {
            public int apply(long group, String name, H5L_info2_t info, H5L_iterate2_t op_data)
            {
                idata id = new idata(name, info.type);
                ((H5L_iter_data)op_data).iterdata.add(id);
                return 0;
            }
        }
        H5L_iterate2_t iter_cb = new H5L_iter_callback();
        try {
            H5Literate_by_name(H5fid, "G1", H5_INDEX_NAME(), H5_ITER_INC(), 0L, iter_cb, iter_data,
                               H5P_DEFAULT());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Literate_by_name: " + err);
        }
        assertFalse("H5Literate_by_name ", ((H5L_iter_data)iter_data).iterdata.isEmpty());
        assertTrue("H5Literate_by_name " + ((H5L_iter_data)iter_data).iterdata.size(),
                   ((H5L_iter_data)iter_data).iterdata.size() == 1);
        assertTrue("H5Literate_by_name " + ((idata)((H5L_iter_data)iter_data).iterdata.get(0)).link_name,
                   ((idata)((H5L_iter_data)iter_data).iterdata.get(0)).link_name.compareToIgnoreCase("DS2") ==
                       0);
    }
}
