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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.exceptions.HDF5LibraryException;

import org.junit.After;
import org.junit.Before;
import org.junit.FixMethodOrder;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;
import org.junit.runners.MethodSorters;

@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class TestH5Z {
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

    @Test
    public void testH5Zfilter_avail()
    {
        try {
            int filter_found;

            filter_found = H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_FLETCHER32);
            assertTrue("H5.H5Zfilter_avail_FLETCHER32", filter_found > 0);
            filter_found = H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_NBIT);
            assertTrue("H5.H5Zfilter_avail_NBIT", filter_found > 0);
            filter_found = H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_SCALEOFFSET);
            assertTrue("H5.H5Zfilter_avail_SCALEOFFSET", filter_found > 0);
            filter_found = H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_SHUFFLE);
            assertTrue("H5.H5Zfilter_avail_SHUFFLE", filter_found > 0);

            // Just make sure H5Zfilter_avail() doesn't fail with szip/zlib
            // since there is no way for us to determine if they should be present
            // or not.
            filter_found = H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_DEFLATE);
            filter_found = H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_SZIP);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Zfilter_avail " + err);
        }
    }

    @Test
    public void testH5Zget_filter_info()
    {
        try {
            int filter_flag;

            filter_flag = H5.H5Zget_filter_info(HDF5Constants.H5Z_FILTER_FLETCHER32);
            assertTrue("H5.H5Zget_filter_info_FLETCHER32_DECODE_ENABLED",
                       (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_DECODE_ENABLED) > 0);
            assertTrue("H5.H5Zget_filter_info_FLETCHER32_ENCODE_ENABLED",
                       (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_ENCODE_ENABLED) > 0);
            filter_flag = H5.H5Zget_filter_info(HDF5Constants.H5Z_FILTER_NBIT);
            assertTrue("H5.H5Zget_filter_info_NBIT_DECODE_ENABLED",
                       (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_DECODE_ENABLED) > 0);
            assertTrue("H5.H5Zget_filter_info_NBIT_ENCODE_ENABLED",
                       (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_ENCODE_ENABLED) > 0);
            filter_flag = H5.H5Zget_filter_info(HDF5Constants.H5Z_FILTER_SCALEOFFSET);
            assertTrue("H5.H5Zget_filter_info_SCALEOFFSET_DECODE_ENABLED",
                       (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_DECODE_ENABLED) > 0);
            assertTrue("H5.H5Zget_filter_info_SCALEOFFSET_ENCODE_ENABLED",
                       (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_ENCODE_ENABLED) > 0);
            filter_flag = H5.H5Zget_filter_info(HDF5Constants.H5Z_FILTER_SHUFFLE);
            assertTrue("H5.H5Zget_filter_info_DECODE_SHUFFLE_ENABLED",
                       (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_DECODE_ENABLED) > 0);
            assertTrue("H5.H5Zget_filter_info_ENCODE_SHUFFLE_ENABLED",
                       (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_ENCODE_ENABLED) > 0);

            if (1 == H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_DEFLATE)) {
                filter_flag = H5.H5Zget_filter_info(HDF5Constants.H5Z_FILTER_DEFLATE);
                assertTrue("H5.H5Zget_filter_info_DEFLATE_DECODE_ENABLED",
                           (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_DECODE_ENABLED) > 0);
                assertTrue("H5.H5Zget_filter_info_DEFLATE_ENCODE_ENABLED",
                           (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_ENCODE_ENABLED) > 0);
            }

            if (1 == H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_SZIP)) {
                filter_flag = H5.H5Zget_filter_info(HDF5Constants.H5Z_FILTER_SZIP);
                // Decode should always be available, but we have no way of determining
                // if encode is so don't assert on that.
                assertTrue("H5.H5Zget_filter_info_DECODE_SZIP_ENABLED",
                           (filter_flag & HDF5Constants.H5Z_FILTER_CONFIG_DECODE_ENABLED) > 0);
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Zget_filter_info " + err);
        }
    }

    @Test(expected = HDF5LibraryException.class)
    public void testH5Zunregister_predefined() throws Throwable
    {
        int filter_found = H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_SHUFFLE);
        assertTrue("H5.H5Zfilter_avail", filter_found > 0);

        H5.H5Zunregister(HDF5Constants.H5Z_FILTER_SHUFFLE);
    }

    @Test
    public void testH5Pappend_filter_string()
    {
        long dcpl_id = HDF5Constants.H5I_INVALID_HID;
        try {
            dcpl_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE);
            assertTrue("H5Pcreate", dcpl_id >= 0);

            int ret = H5.H5Pappend_filter(dcpl_id, HDF5Constants.H5Z_FILTER_SHUFFLE, 0, "");
            assertTrue("H5Pappend_filter (string, shuffle)", ret >= 0);

            int nfilters = H5.H5Pget_nfilters(dcpl_id);
            assertEquals("nfilters after H5Pappend_filter", 1, nfilters);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Pappend_filter_string: " + err);
        }
        finally {
            if (dcpl_id != HDF5Constants.H5I_INVALID_HID)
                try {
                    H5.H5Pclose(dcpl_id);
                }
                catch (Exception e) { /* ignore */
                }
        }
    }

    @Test
    public void testH5Pappend_filter_cdvalues()
    {
        long dcpl_id = HDF5Constants.H5I_INVALID_HID;
        try {
            dcpl_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE);
            assertTrue("H5Pcreate", dcpl_id >= 0);

            int[] cd_values = new int[] {9};
            if (1 == H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_DEFLATE)) {
                int ret = H5.H5Pappend_filter(dcpl_id, HDF5Constants.H5Z_FILTER_DEFLATE, 0, cd_values);
                assertTrue("H5Pappend_filter (cd_values, deflate)", ret >= 0);

                int nfilters = H5.H5Pget_nfilters(dcpl_id);
                assertEquals("nfilters after H5Pappend_filter", 1, nfilters);

                // Verify the cd_values round-trip via H5Pget_filter.
                int[] cd_out      = new int[1];
                int[] flags_out   = new int[1];
                long[] cd_nelmts  = new long[] {1};
                String[] name_out = new String[] {""};
                int filter_id =
                    H5.H5Pget_filter(dcpl_id, 0, flags_out, cd_nelmts, cd_out, 256, name_out, new int[1]);
                assertEquals("filter id from H5Pget_filter", HDF5Constants.H5Z_FILTER_DEFLATE, filter_id);
                assertEquals("cd_nelmts", 1L, cd_nelmts[0]);
                assertEquals("cd_values[0] (deflate level)", 9, cd_out[0]);
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Pappend_filter_cdvalues: " + err);
        }
        finally {
            if (dcpl_id != HDF5Constants.H5I_INVALID_HID)
                try {
                    H5.H5Pclose(dcpl_id);
                }
                catch (Exception e) { /* ignore */
                }
        }
    }

    @Test
    public void testH5Pmodify_filter_by_idx_string()
    {
        long dcpl_id = HDF5Constants.H5I_INVALID_HID;
        try {
            dcpl_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE);
            assertTrue("H5Pcreate", dcpl_id >= 0);

            if (1 == H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_DEFLATE)) {
                int ret = H5.H5Pappend_filter(dcpl_id, HDF5Constants.H5Z_FILTER_DEFLATE, 0, "level=1");
                assertTrue("H5Pappend_filter (string, deflate)", ret >= 0);

                // Replace the configuration in place; position and ID are unchanged.
                ret = H5.H5Pmodify_filter_by_idx(dcpl_id, 0, 0, "level=9");
                assertTrue("H5Pmodify_filter_by_idx (string)", ret >= 0);

                int nfilters = H5.H5Pget_nfilters(dcpl_id);
                assertEquals("nfilters after H5Pmodify_filter_by_idx", 1, nfilters);

                // The entry keeps a stored string, which reports the new value.
                String params = H5.H5Pget_filter_params_by_idx(dcpl_id, 0);
                assertNotNull("params is non-null", params);
                assertTrue("stored string reflects the modify: " + params, params.contains("level=9"));
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Pmodify_filter_by_idx_string: " + err);
        }
        finally {
            if (dcpl_id != HDF5Constants.H5I_INVALID_HID)
                try {
                    H5.H5Pclose(dcpl_id);
                }
                catch (Exception e) { /* ignore */
                }
        }
    }

    @Test
    public void testH5Pmodify_filter_by_idx_cdvalues()
    {
        long dcpl_id = HDF5Constants.H5I_INVALID_HID;
        try {
            dcpl_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE);
            assertTrue("H5Pcreate", dcpl_id >= 0);

            if (1 == H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_DEFLATE)) {
                int ret = H5.H5Pappend_filter(dcpl_id, HDF5Constants.H5Z_FILTER_DEFLATE, 0, "level=1");
                assertTrue("H5Pappend_filter (string, deflate)", ret >= 0);

                int[] cd_values = new int[] {6};
                ret             = H5.H5Pmodify_filter_by_idx(dcpl_id, 0, 0, cd_values);
                assertTrue("H5Pmodify_filter_by_idx (cd_values)", ret >= 0);

                int nfilters = H5.H5Pget_nfilters(dcpl_id);
                assertEquals("nfilters after H5Pmodify_filter_by_idx", 1, nfilters);

                // cd_values were replaced.
                int[] cd_out      = new int[1];
                int[] flags_out   = new int[1];
                long[] cd_nelmts  = new long[] {1};
                String[] name_out = new String[] {""};
                int filter_id =
                    H5.H5Pget_filter(dcpl_id, 0, flags_out, cd_nelmts, cd_out, 256, name_out, new int[1]);
                assertEquals("filter id unchanged by modify", HDF5Constants.H5Z_FILTER_DEFLATE, filter_id);
                assertEquals("cd_values[0] after modify", 6, cd_out[0]);
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Pmodify_filter_by_idx_cdvalues: " + err);
        }
        finally {
            if (dcpl_id != HDF5Constants.H5I_INVALID_HID)
                try {
                    H5.H5Pclose(dcpl_id);
                }
                catch (Exception e) { /* ignore */
                }
        }
    }

    @Test
    public void testH5Pget_filter_params_by_idx()
    {
        long dcpl_id = HDF5Constants.H5I_INVALID_HID;
        try {
            dcpl_id = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE);
            assertTrue("H5Pcreate", dcpl_id >= 0);

            int ret = H5.H5Pappend_filter(dcpl_id, HDF5Constants.H5Z_FILTER_SHUFFLE, 0, "");
            assertTrue("H5Pappend_filter shuffle", ret >= 0);

            String params = H5.H5Pget_filter_params_by_idx(dcpl_id, 0);
            assertNotNull("params is non-null", params);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("testH5Pget_filter_params_by_idx: " + err);
        }
        finally {
            if (dcpl_id != HDF5Constants.H5I_INVALID_HID)
                try {
                    H5.H5Pclose(dcpl_id);
                }
                catch (Exception e) { /* ignore */
                }
        }
    }
}
