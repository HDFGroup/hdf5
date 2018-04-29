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


import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.exceptions.HDF5Exception;
import hdf.hdf5lib.exceptions.HDF5LibraryException;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5S {
    @Rule public TestName testname = new TestName();
    long H5sid = -1;
    int H5rank = 2;
    long H5dims[] = {5, 5};
    long H5maxdims[] = {10, 10};

    @Before
    public void createH5file()
            throws NullPointerException, HDF5Exception {
        assertTrue("H5 open ids is 0", H5.getOpenIDCount()==0);
        System.out.print(testname.getMethodName());

        H5sid = H5.H5Screate_simple(H5rank, H5dims, H5maxdims);
        assertTrue("H5.H5Screate_simple_extent", H5sid > 0);
    }

    @After
    public void deleteH5file() throws HDF5LibraryException {
        if (H5sid > 0) {
            try {H5.H5Sclose(H5sid);} catch (Exception ex) {}
        }
        System.out.println();
    }

    @Test
    public void testH5Sget_simple_extent_ndims() {
        int read_rank = -1;
        try {
            read_rank = H5.H5Sget_simple_extent_ndims(H5sid);
            assertTrue("H5.H5Sget_simple_extent_ndims", H5rank == read_rank);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Sget_simple_extent_ndims: " + err);
        }
    }

    @Test
    public void testH5Sget_simple_extent_dims_null() {
        int read_rank = -1;

        try {
            read_rank = H5.H5Sget_simple_extent_dims(H5sid, null, null);
            assertTrue("H5.H5Sget_simple_extent_dims", H5rank == read_rank);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Sget_simple_extent_dims: " + err);
        }
    }

    @Test
    public void testH5Sget_simple_extent_dims() {
        int read_rank = -1;
        long dims[] = {5, 5};
        long maxdims[] = {10, 10};

        try {
            read_rank = H5.H5Sget_simple_extent_dims(H5sid, dims, maxdims);
            assertTrue("H5.H5Sget_simple_extent_dims", H5rank == read_rank);
            assertTrue("H5.H5Sget_simple_extent_dims:dims", H5dims[0] == dims[0]);
            assertTrue("H5.H5Sget_simple_extent_dims:maxdims", H5maxdims[0] == maxdims[0]);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Sget_simple_extent_dims: " + err);
        }
    }

    @Test
    public void testH5Sget_simple_extent_npoints() {
        long num_elements = -1;
        try {
            num_elements = H5.H5Sget_simple_extent_npoints(H5sid);
            assertTrue("H5.H5Sget_simple_extent_npoints", (H5dims[0]*H5dims[1]) == num_elements);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Sget_simple_extent_npoints: " + err);
        }
    }

    @Test
    public void testH5Sget_simple_extent_type() {
        int read_type = -1;
        try {
            read_type = H5.H5Sget_simple_extent_type(H5sid);
            assertTrue("H5.H5Sget_simple_extent_type", HDF5Constants.H5S_SIMPLE == read_type);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Sget_simple_extent_type: " + err);
        }
    }

    @Test
    public void testH5Sis_simple() {
        boolean result = false;

        try {
            result = H5.H5Sis_simple(H5sid);
            assertTrue("H5.H5Sis_simple", result);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Sis_simple: " + err);
        }
    }

    @Test
    public void testH5Sset_extent_simple() {
        long num_elements = -1;
        try {
            H5.H5Sset_extent_simple(H5sid, H5rank, H5maxdims, H5maxdims);
            num_elements = H5.H5Sget_simple_extent_npoints(H5sid);
            assertTrue("H5.H5Sget_simple_extent_npoints", (H5maxdims[0]*H5maxdims[1]) == num_elements);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Sset_extent_simple: " + err);
        }
    }

    @Test
    public void testH5Sget_select_type() {
        int read_type = -1;
        try {
            read_type = H5.H5Sget_select_type(H5sid);
            assertTrue("H5.H5Sget_select_type", HDF5Constants.H5S_SEL_ALL == read_type);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Sset_extent_none: " + err);
        }
    }

    @Test
    public void testH5Sset_extent_none() {
        int read_type = -1;
        try {
            H5.H5Sset_extent_none(H5sid);
            read_type = H5.H5Sget_simple_extent_type(H5sid);
            assertTrue("H5.H5Sget_simple_extent_type: "+read_type, HDF5Constants.H5S_NO_CLASS == read_type);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Sset_extent_none: " + err);
        }
    }

    @Test
    public void testH5Scopy() {
        long sid = -1;
        int read_rank = -1;

        try {
            sid = H5.H5Scopy(H5sid);
            assertTrue("H5.H5Sis_simple", sid > 0);
            read_rank = H5.H5Sget_simple_extent_ndims(sid);
            assertTrue("H5.H5Screate_simple_extent_ndims", H5rank == read_rank);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Scopy: " + err);
        }
        finally {
            try {H5.H5Sclose(sid);} catch (Exception ex) {}
        }
    }

    @Test
    public void testH5Sextent_copy() {
        long sid = -1;
        int class_type = -1;

        try {
            sid = H5.H5Screate(HDF5Constants.H5S_NULL);
            assertTrue("H5.H5Screate_null", sid > 0);
            H5.H5Sextent_copy(sid, H5sid);
            class_type = H5.H5Sget_simple_extent_type(sid);
            assertTrue("H5.H5Screate_null: type", class_type == HDF5Constants.H5S_SIMPLE);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Sextent_copy: " + err);
        }
        finally {
            try {H5.H5Sclose(sid);} catch (Exception ex) {}
        }
    }

    @Test
    public void testH5Sextent_equal() {
        long sid = -1;
        boolean result = false;

        try {
            sid = H5.H5Screate(HDF5Constants.H5S_NULL);
            assertTrue("H5.H5Screate_null",sid > 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Screate: null " + err);
        }

        try {
            result = H5.H5Sextent_equal(sid, H5sid);
            assertFalse("H5.testH5Sextent_equal",result);
            H5.H5Sextent_copy(sid, H5sid);
            result = H5.H5Sextent_equal(sid, H5sid);
            assertTrue("H5.testH5Sextent_equal", result);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Sextent_copy " + err);
        }
        finally {
            try {H5.H5Sclose(sid);} catch (Exception ex) {}
        }
    }

    @Test
    public void testH5Sencode_decode_null_dataspace() {
        long sid = -1;
        long decoded_sid = -1;
        byte[] null_sbuf = null;
        boolean result = false;

        try {
            sid = H5.H5Screate(HDF5Constants.H5S_NULL);
            assertTrue("H5.H5Screate_null", sid > 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Screate: null " + err);
        }

        try {
            null_sbuf = H5.H5Sencode(sid);
            assertFalse("H5.testH5Sencode", null_sbuf==null);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Sencode " + err);
        }
        finally {
            if(null_sbuf == null) {
                try {H5.H5Sclose(sid);} catch (Exception ex) {}
            }
        }

        try {
            decoded_sid = H5.H5Sdecode(null_sbuf);
            assertTrue("H5.testH5Sdecode", decoded_sid>0);

            result = H5.H5Sextent_equal(sid, decoded_sid);
            assertTrue("H5.testH5Sextent_equal", result);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Sdecode " + err);
        }
        finally {
            try {H5.H5Sclose(decoded_sid);} catch (Exception ex) {}
            try {H5.H5Sclose(sid);} catch (Exception ex) {}
        }
    }

    @Test
    public void testH5Sencode_decode_scalar_dataspace() {
        long sid = -1;
        long decoded_sid = -1;
        byte[] scalar_sbuf = null;
        boolean result = false;
        int iresult = -1;
        long lresult = -1;

        try {
            sid = H5.H5Screate(HDF5Constants.H5S_SCALAR);
            assertTrue("H5.H5Screate_null", sid > 0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Screate: null " + err);
        }

        try {
            scalar_sbuf = H5.H5Sencode(sid);
            assertFalse("H5.testH5Sencode", scalar_sbuf==null);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Sencode " + err);
        }
        finally {
            if(scalar_sbuf == null) {
                try {H5.H5Sclose(sid);} catch (Exception ex) {}
            }
        }

        try {
            decoded_sid = H5.H5Sdecode(scalar_sbuf);
            assertTrue("H5.testH5Sdecode", decoded_sid>0);

            result = H5.H5Sextent_equal(sid, decoded_sid);
            assertTrue("H5.testH5Sextent_equal", result);

            /* Verify decoded dataspace */
            lresult = H5.H5Sget_simple_extent_npoints(decoded_sid);
            assertTrue("H5.testH5Sget_simple_extent_npoints", lresult==1);

            iresult = H5.H5Sget_simple_extent_ndims(decoded_sid);
            assertTrue("H5.testH5Sget_simple_extent_ndims", iresult==0);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Sdecode " + err);
        }
        finally {
            try {H5.H5Sclose(decoded_sid);} catch (Exception ex) {}
            try {H5.H5Sclose(sid);} catch (Exception ex) {}
        }
    }

    @Test
    public void testH5Sselect_none() {
        int read_type = -1;
        try {
            H5.H5Sselect_none(H5sid);
            read_type = H5.H5Sget_select_type(H5sid);
            assertTrue("H5.H5Sget_select_type: "+read_type, HDF5Constants.H5S_SEL_NONE == read_type);
            H5.H5Sselect_all(H5sid);
            read_type = H5.H5Sget_select_type(H5sid);
            assertTrue("H5.H5Sget_select_type: "+read_type, HDF5Constants.H5S_SEL_ALL == read_type);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Sset_extent_none: " + err);
        }
    }

    @Test
    public void testH5Sget_select_npoints() {
        long coord[][] = {{0,1},{2,4},{5,6}}; /* Coordinates for point selection */
        long num_elements = -1;
        try {
            H5.H5Sselect_elements(H5sid, HDF5Constants.H5S_SELECT_SET, 3, coord);
            num_elements = H5.H5Sget_select_npoints(H5sid);
            assertTrue("H5.H5Sget_select_npoints: "+num_elements, 3 == num_elements);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Sget_select_npoints: " + err);
        }
    }

    @Test(expected = IllegalArgumentException.class)
    public void testH5Sget_select_elem_pointlist_invalid() throws Throwable {
        long coord[][] = {{0,1},{2,4},{5,6}}; /* Coordinates for point selection */
        long getcoord[] = {-1,-1}; /* Coordinates for get point selection */
        try {
            H5.H5Sselect_elements(H5sid, HDF5Constants.H5S_SELECT_SET, 3, coord);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Sget_select_elem_pointlist: " + err);
        }
        H5.H5Sget_select_elem_pointlist(H5sid, 0, 3, getcoord);
    }

    @Test
    public void testH5Sget_select_elem_pointlist() {
        long coord[][] = {{0,1},{2,3},{4,5}}; /* Coordinates for point selection */
        long getcoord[] = {-1,-1,-1,-1,-1,-1}; /* Coordinates for get point selection */
        try {
            H5.H5Sselect_elements(H5sid, HDF5Constants.H5S_SELECT_SET, 3, coord);
            H5.H5Sget_select_elem_pointlist(H5sid, 0, 3, getcoord);
            assertTrue("H5.H5Sget_select_elem_pointlist", coord[0][0] == getcoord[0]);
            assertTrue("H5.H5Sget_select_elem_pointlist", coord[0][1] == getcoord[1]);
            assertTrue("H5.H5Sget_select_elem_pointlist", coord[1][0] == getcoord[2]);
            assertTrue("H5.H5Sget_select_elem_pointlist", coord[1][1] == getcoord[3]);
            assertTrue("H5.H5Sget_select_elem_pointlist", coord[2][0] == getcoord[4]);
            assertTrue("H5.H5Sget_select_elem_pointlist", coord[2][1] == getcoord[5]);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Sget_select_elem_pointlist: " + err);
        }
    }

    @Test
    public void testH5Sget_select_bounds() {
        long lowbounds[] = {-1,-1};
        long hibounds[] = {-1,-1};
        try {
            H5.H5Sget_select_bounds(H5sid, lowbounds, hibounds);
            assertTrue("H5.H5Sget_select_bounds", 0 == lowbounds[0]);
            assertTrue("H5.H5Sget_select_bounds", 0 == lowbounds[1]);
            assertTrue("H5.H5Sget_select_bounds", (H5dims[0]-1) == hibounds[0]);
            assertTrue("H5.H5Sget_select_bounds", (H5dims[1]-1) == hibounds[1]);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Sget_select_bounds: " + err);
        }
    }

    @Test
    public void testH5Soffset_simple() {
        long coord[][] = {{2,2},{2,4},{4,2},{4,4}}; /* Coordinates for point selection */
        long lowbounds[] = {-1,-1};
        long hibounds[] = {-1,-1};
        try {
            H5.H5Sselect_elements(H5sid, HDF5Constants.H5S_SELECT_SET, 4, coord);
            H5.H5Sget_select_bounds(H5sid, lowbounds, hibounds);
            assertTrue("H5.H5Sget_select_bounds", 2 == lowbounds[0]);
            assertTrue("H5.H5Sget_select_bounds", 2 == lowbounds[1]);
            assertTrue("H5.H5Sget_select_bounds", (H5dims[0]-1) == hibounds[0]);
            assertTrue("H5.H5Sget_select_bounds", (H5dims[1]-1) == hibounds[1]);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Sget_select_bounds: " + err);
        }
        try {
            long offset[] = {-1,-1};
            H5.H5Soffset_simple(H5sid, offset);
            H5.H5Sget_select_bounds(H5sid, lowbounds, hibounds);
            assertTrue("H5.H5Sget_select_bounds", 1 == lowbounds[0]);
            assertTrue("H5.H5Sget_select_bounds", 1 == lowbounds[1]);
            assertTrue("H5.H5Sget_select_bounds", (H5dims[0]-2) == hibounds[0]);
            assertTrue("H5.H5Sget_select_bounds", (H5dims[1]-2) == hibounds[1]);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Soffset_simple: " + err);
        }
    }

    @Test
    public void testH5Sget_select_hyper() {
        long space1 = -1;
        long start[] = {0,0};
        long stride[] = {1,1};
        long count[] = {1,1};
        long block[] = {4,4};
        long nblocks;   // Number of hyperslab blocks
        long blocks[] = {-1, -1, -1, -1, -1, -1, -1, -1};    // List of blocks
        try {
            // Copy "all" selection & space
            space1 = H5.H5Scopy(H5sid);
            assertTrue("H5.H5Scopy", H5sid > 0);
            // 'AND' "all" selection with another hyperslab
            H5.H5Sselect_hyperslab(space1, HDF5Constants.H5S_SELECT_AND, start, stride, count, block);

            // Verify that there is only one block
            nblocks = H5.H5Sget_select_hyper_nblocks(space1);
            assertTrue("H5Sget_select_hyper_nblocks", nblocks == 1);

            // Retrieve the block defined
            H5.H5Sget_select_hyper_blocklist(space1, 0, nblocks, blocks);

            // Verify that the correct block is defined
            assertTrue("H5.H5Sget_select_hyper_blocklist", start[0] == blocks[0]);
            assertTrue("H5.H5Sget_select_hyper_blocklist", start[1] == blocks[1]);
            assertTrue("H5.H5Sget_select_hyper_blocklist", (block[0]-1) == blocks[2]);
            assertTrue("H5.H5Sget_select_hyper_blocklist", (block[1]-1) == blocks[3]);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Sget_select_bounds: " + err);
        }
        finally {
            try {H5.H5Sclose(space1);} catch (Exception ex) {}
        }
    }

    @Test
    public void testH5Sget_select_valid() {
        long space1 = -1;
        long start[] = {1,0};
        long stride[] = {1,1};
        long count[] = {2,3};
        long block[] = {1,1};
        long offset[] = {0,0};    // Offset of selection

        try {
            // Copy "all" selection & space
            space1 = H5.H5Scopy(H5sid);
            assertTrue("H5.H5Scopy", H5sid > 0);
            // 'AND' "all" selection with another hyperslab
            H5.H5Sselect_hyperslab(space1, HDF5Constants.H5S_SELECT_SET, start, stride, count, block);

            // Check a valid offset
            offset[0]=-1;
            offset[1]=0;
            H5.H5Soffset_simple(space1, offset);
            assertTrue("H5Sselect_valid", H5.H5Sselect_valid(space1));

            // Check an invalid offset
            offset[0]=10;
            offset[1]=0;
            H5.H5Soffset_simple(space1, offset);
            assertFalse("H5Sselect_valid", H5.H5Sselect_valid(space1));

            /* Reset offset */
            offset[0]=0;
            offset[1]=0;
            H5.H5Soffset_simple(space1, offset);
            assertTrue("H5Sselect_valid", H5.H5Sselect_valid(space1));
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Sget_select_valid: " + err);
        }
        finally {
            try {H5.H5Sclose(space1);} catch (Exception ex) {}
        }
    }

    @Test
    public void testH5Shyper_regular() {
        long start[] = {1,0};
        long stride[] = {1,1};
        long count[] = {2,3};
        long block[] = {1,1};
        long q_start[] = new long[2];
        long q_stride[] = new long[2];
        long q_count[] = new long[2];
        long q_block[] = new long[2];
        boolean is_regular = false;

        try {
            // Set "regular" hyperslab selection
            H5.H5Sselect_hyperslab(H5sid, HDF5Constants.H5S_SELECT_SET, start, stride, count, block);

            // Query if 'hyperslab' selection is regular hyperslab (should be TRUE)
            is_regular = H5.H5Sis_regular_hyperslab(H5sid);
            assertTrue("H5.H5Sis_regular_hyperslab", is_regular);

            // Retrieve the hyperslab parameters
            H5.H5Sget_regular_hyperslab(H5sid, q_start, q_stride, q_count, q_block);

            /* Verify the hyperslab parameters */
            for(int u = 0; u < H5rank; u++) {
                assertTrue("H5Sget_regular_hyperslab, start", start[u] == q_start[u]);
                assertTrue("H5Sget_regular_hyperslab, stride", stride[u] == q_stride[u]);
                assertTrue("H5Sget_regular_hyperslab, count", count[u] == q_count[u]);
                assertTrue("H5Sget_regular_hyperslab, block", block[u] == q_block[u]);
            } /* end for */
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Sget_select_valid: " + err);
        }
    }
}
