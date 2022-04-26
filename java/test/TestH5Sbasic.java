/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

package test;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.exceptions.HDF5LibraryException;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5Sbasic {
    @Rule
    public TestName testname = new TestName();

    @Before
    public void checkOpenIDs()
    {
        assertTrue("H5 open ids is 0", H5.getOpenIDCount() == 0);
        System.out.print(testname.getMethodName());
    }
    @After
    public void nextTestName()
    {
        System.out.println();
    }

    @Test //(expected = HDF5LibraryException.class)
    public void testH5Sclose_invalid() throws Throwable
    {
        long sid = H5.H5Sclose(-1);
        assertTrue(sid == 0);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Screate_invalid() throws Throwable
    {
        H5.H5Screate(-1);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Sget_simple_extent_type_invalid() throws Throwable
    {
        H5.H5Sget_simple_extent_type(-1);
    }

    @Test
    public void testH5Screate_scalar()
    {
        long sid       = HDF5Constants.H5I_INVALID_HID;
        int class_type = -1;
        try {
            sid = H5.H5Screate(HDF5Constants.H5S_SCALAR);
            assertTrue("H5.H5Screate_scalar", sid > 0);
            class_type = H5.H5Sget_simple_extent_type(sid);
            assertTrue("H5.H5Screate_scalar: type", class_type == HDF5Constants.H5S_SCALAR);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Screate: " + err);
        }
        finally {
            try {
                H5.H5Sclose(sid);
            }
            catch (Exception ex) {
            }
        }
    }

    @Test
    public void testH5Screate_null()
    {
        long sid       = HDF5Constants.H5I_INVALID_HID;
        int class_type = -1;
        try {
            sid = H5.H5Screate(HDF5Constants.H5S_NULL);
            assertTrue("H5.H5Screate_null", sid > 0);
            class_type = H5.H5Sget_simple_extent_type(sid);
            assertTrue("H5.H5Screate_null: type", class_type == HDF5Constants.H5S_NULL);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Screate: " + err);
        }
        finally {
            try {
                H5.H5Sclose(sid);
            }
            catch (Exception ex) {
            }
        }
    }

    @Test(expected = NullPointerException.class)
    public void testH5Screate_simple_dims_null() throws Throwable
    {
        H5.H5Screate_simple(2, (long[])null, null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Screate_simple_rank_invalid() throws Throwable
    {
        long dims[] = {5, 5};
        H5.H5Screate_simple(-1, dims, null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Screate_simple_dims_invalid() throws Throwable
    {
        long dims[] = {2, 2};
        H5.H5Screate_simple(5, dims, null);
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Screate_simple_dims_exceed() throws Throwable
    {
        long dims[] = {0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14, 15, 16, 17,
                       18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 35};
        H5.H5Screate_simple(35, dims, null);
    }

    // H5Screate_simple was changed to allow a dim of 0
    //     @Ignore(expected = HDF5LibraryException.class)
    //     public void testH5Screate_simple_dims_zero() {
    //         long dims[] = {0, 0};
    //         H5.H5Screate_simple(2, dims, null);
    //     }

    @Test
    public void testH5Screate_simple()
    {
        long sid       = HDF5Constants.H5I_INVALID_HID;
        int class_type = -1;
        int rank       = 2;
        long dims[]    = {5, 5};
        long maxdims[] = {10, 10};

        try {
            sid = H5.H5Screate_simple(rank, dims, maxdims);
            assertTrue("H5.H5Screate_simple", sid > 0);
            class_type = H5.H5Sget_simple_extent_type(sid);
            assertTrue("H5.H5Screate_simple: type", class_type == HDF5Constants.H5S_SIMPLE);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Screate_simple: " + err);
        }
        finally {
            try {
                H5.H5Sclose(sid);
            }
            catch (Exception ex) {
            }
        }
    }

    @Test
    public void testH5Screate_simple_unlimted()
    {
        long sid       = HDF5Constants.H5I_INVALID_HID;
        int class_type = -1;
        int rank       = 2;
        long dims[]    = {5, 5};
        long maxdims[] = {HDF5Constants.H5S_UNLIMITED, HDF5Constants.H5S_UNLIMITED};

        try {
            sid = H5.H5Screate_simple(rank, dims, maxdims);
            assertTrue("H5.H5Screate_simple", sid > 0);
            class_type = H5.H5Sget_simple_extent_type(sid);
            assertTrue("H5.H5Screate_simple: type", class_type == HDF5Constants.H5S_SIMPLE);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Screate_simple: " + err);
        }
        finally {
            try {
                H5.H5Sclose(sid);
            }
            catch (Exception ex) {
            }
        }
    }

    @Test
    public void testH5Screate_simple_unlimted_1d()
    {
        long sid       = HDF5Constants.H5I_INVALID_HID;
        int class_type = -1;
        int rank       = 1;
        long dims[]    = {5};
        long maxdims[] = {HDF5Constants.H5S_UNLIMITED};

        try {
            sid = H5.H5Screate_simple(rank, dims, maxdims);
            assertTrue("H5.H5Screate_simple", sid > 0);
            class_type = H5.H5Sget_simple_extent_type(sid);
            assertTrue("H5.H5Screate_simple: type", class_type == HDF5Constants.H5S_SIMPLE);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Screate_simple: " + err);
        }
        finally {
            try {
                H5.H5Sclose(sid);
            }
            catch (Exception ex) {
            }
        }
    }

    @Test
    public void testH5Screate_simple_max_default()
    {
        long sid    = HDF5Constants.H5I_INVALID_HID;
        int rank    = 2;
        long dims[] = {5, 5};

        try {
            sid = H5.H5Screate_simple(rank, dims, null);
            assertTrue("H5.H5Screate_simple_max_default", sid > 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Screate_simple: " + err);
        }
        finally {
            try {
                H5.H5Sclose(sid);
            }
            catch (Exception ex) {
            }
        }
    }

    @Test
    public void testH5Screate_simple_extent()
    {
        long sid       = HDF5Constants.H5I_INVALID_HID;
        int rank       = 2;
        long dims[]    = {5, 5};
        long maxdims[] = {10, 10};

        try {
            sid = H5.H5Screate(HDF5Constants.H5S_SIMPLE);
            assertTrue("H5.H5Screate_simple_extent", sid > 0);
            H5.H5Sset_extent_simple(sid, rank, dims, maxdims);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Screate: " + err);
        }
        finally {
            try {
                H5.H5Sclose(sid);
            }
            catch (Exception ex) {
            }
        }
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Sencode_invalid() throws Throwable
    {
        H5.H5Sencode(-1);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Sdecode_null() throws Throwable
    {
        H5.H5Sdecode(null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Sget_regular_hyperslab_invalid() throws Throwable
    {
        long q_start[]  = new long[2];
        long q_stride[] = new long[2];
        long q_count[]  = new long[2];
        long q_block[]  = new long[2];

        H5.H5Sget_regular_hyperslab(-1, q_start, q_stride, q_count, q_block);
    }

    @Test(expected = hdf.hdf5lib.exceptions.HDF5FunctionArgumentException.class)
    public void testH5Sselect_copy_invalid() throws Throwable
    {
        H5.H5Sselect_copy(-1, -1);
    }

    @Test(expected = hdf.hdf5lib.exceptions.HDF5DataspaceInterfaceException.class)
    public void testH5Sselect_shape_same_invalid() throws Throwable
    {
        H5.H5Sselect_shape_same(-1, -1);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Sselect_adjust_invalid() throws Throwable
    {
        long offset[][] = {{0, 1}, {2, 4}, {5, 6}};
        H5.H5Sselect_adjust(-1, offset);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Sselect_adjust_rank_offset() throws Throwable
    {
        long sid        = HDF5Constants.H5I_INVALID_HID;
        long offset[][] = {{0, 1}, {2, 4}, {5, 6}};

        try {
            sid = H5.H5Screate(HDF5Constants.H5S_SIMPLE);
            assertTrue("H5.H5Screate_simple_extent", sid > 0);
            H5.H5Sselect_adjust(sid, offset);
        }
        finally {
            try {
                H5.H5Sclose(sid);
            }
            catch (Exception ex) {
            }
        }
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Sselect_intersect_block_invalid() throws Throwable
    {
        long start[] = new long[2];
        long end[]   = new long[2];
        H5.H5Sselect_intersect_block(-1, start, end);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Sselect_intersect_block_rank_start() throws Throwable
    {
        long sid     = HDF5Constants.H5I_INVALID_HID;
        long start[] = new long[2];
        long end[]   = null;

        try {
            sid = H5.H5Screate(HDF5Constants.H5S_SIMPLE);
            assertTrue("H5.H5Screate_simple_extent", sid > 0);
            H5.H5Sselect_intersect_block(sid, start, end);
        }
        finally {
            try {
                H5.H5Sclose(sid);
            }
            catch (Exception ex) {
            }
        }
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Sselect_intersect_block_rank_end() throws Throwable
    {
        long sid     = HDF5Constants.H5I_INVALID_HID;
        long start[] = null;
        long end[]   = new long[2];

        try {
            sid = H5.H5Screate(HDF5Constants.H5S_SIMPLE);
            assertTrue("H5.H5Screate_simple_extent", sid > 0);
            H5.H5Sselect_intersect_block(sid, start, end);
        }
        finally {
            try {
                H5.H5Sclose(sid);
            }
            catch (Exception ex) {
            }
        }
    }

    @Test(expected = hdf.hdf5lib.exceptions.HDF5DataspaceInterfaceException.class)
    public void testH5Sselect_project_intersection_invalid() throws Throwable
    {
        H5.H5Sselect_project_intersection(-1, -1, -1);
    }

    @Test(expected = hdf.hdf5lib.exceptions.HDF5FunctionArgumentException.class)
    public void testH5Scombine_hyperslab_invalid() throws Throwable
    {
        long start[] = new long[2];
        long count[] = new long[2];
        H5.H5Scombine_hyperslab(-1, 0, start, null, count, null);
    }

    @Test(expected = NullPointerException.class)
    public void testH5Scombine_hyperslab_null_start() throws Throwable
    {
        long sid      = HDF5Constants.H5I_INVALID_HID;
        long start[]  = null;
        long stride[] = null;
        long count[]  = new long[2];
        long block[]  = null;

        try {
            sid = H5.H5Screate(HDF5Constants.H5S_SIMPLE);
            assertTrue("H5.H5Screate_simple_extent", sid > 0);
            H5.H5Scombine_hyperslab(sid, 0, start, stride, count, block);
        }
        finally {
            try {
                H5.H5Sclose(sid);
            }
            catch (Exception ex) {
            }
        }
    }

    @Test(expected = NullPointerException.class)
    public void testH5Scombine_hyperslab_null_count() throws Throwable
    {
        long sid      = HDF5Constants.H5I_INVALID_HID;
        long start[]  = new long[2];
        long stride[] = null;
        long count[]  = null;
        long block[]  = null;

        try {
            sid = H5.H5Screate(HDF5Constants.H5S_SIMPLE);
            assertTrue("H5.H5Screate_simple_extent", sid > 0);
            H5.H5Scombine_hyperslab(sid, 0, start, stride, count, block);
        }
        finally {
            try {
                H5.H5Sclose(sid);
            }
            catch (Exception ex) {
            }
        }
    }

    @Test(expected = hdf.hdf5lib.exceptions.HDF5FunctionArgumentException.class)
    public void testH5Smodify_select_invalid() throws Throwable
    {
        H5.H5Smodify_select(-1, 0, -1);
    }

    @Test(expected = hdf.hdf5lib.exceptions.HDF5FunctionArgumentException.class)
    public void testH5Scombine_select_invalid() throws Throwable
    {
        H5.H5Scombine_select(-1, 0, -1);
    }
}
