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
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;

import org.hdfgroup.javahdf5.*;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5Z {
    @Rule
    public TestName testname = new TestName();

    @Before
    public void checkOpenIDs()
    {
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

            filter_found = H5Zfilter_avail(H5Z_FILTER_FLETCHER32());
            assertTrue("H5Zfilter_avail_FLETCHER32", filter_found > 0);
            filter_found = H5Zfilter_avail(H5Z_FILTER_NBIT());
            assertTrue("H5Zfilter_avail_NBIT", filter_found > 0);
            filter_found = H5Zfilter_avail(H5Z_FILTER_SCALEOFFSET());
            assertTrue("H5Zfilter_avail_SCALEOFFSET", filter_found > 0);
            filter_found = H5Zfilter_avail(H5Z_FILTER_SHUFFLE());
            assertTrue("H5Zfilter_avail_SHUFFLE", filter_found > 0);

            // Just make sure H5Zfilter_avail() doesn't fail with szip/zlib
            // since there is no way for us to determine if they should be present
            // or not.
            filter_found = H5Zfilter_avail(H5Z_FILTER_DEFLATE());
            filter_found = H5Zfilter_avail(H5Z_FILTER_SZIP());
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Zfilter_avail " + err);
        }
    }

    @Test
    public void testH5Zget_filter_info()
    {
        try {
            int filter_flag; // Allocate on-heap memory
            // Use try-with-resources to manage the lifetime of off-heap memory
            try (Arena offHeap = Arena.ofConfined()) {
                // Allocate memory to store the integers
                MemorySegment filter_flag_ptr = offHeap.allocate(ValueLayout.JAVA_INT);

                H5Zget_filter_info(H5Z_FILTER_FLETCHER32(), filter_flag_ptr);
                filter_flag = filter_flag_ptr.get(ValueLayout.JAVA_INT, 0);
                assertTrue("H5Zget_filter_info_FLETCHER32_DECODE_ENABLED",
                           (filter_flag & H5Z_FILTER_CONFIG_DECODE_ENABLED()) > 0);
                assertTrue("H5Zget_filter_info_FLETCHER32_ENCODE_ENABLED",
                           (filter_flag & H5Z_FILTER_CONFIG_ENCODE_ENABLED()) > 0);
                H5Zget_filter_info(H5Z_FILTER_NBIT(), filter_flag_ptr);
                filter_flag = filter_flag_ptr.get(ValueLayout.JAVA_INT, 0);
                assertTrue("H5Zget_filter_info_NBIT_DECODE_ENABLED",
                           (filter_flag & H5Z_FILTER_CONFIG_DECODE_ENABLED()) > 0);
                assertTrue("H5Zget_filter_info_NBIT_ENCODE_ENABLED",
                           (filter_flag & H5Z_FILTER_CONFIG_ENCODE_ENABLED()) > 0);
                H5Zget_filter_info(H5Z_FILTER_SCALEOFFSET(), filter_flag_ptr);
                filter_flag = filter_flag_ptr.get(ValueLayout.JAVA_INT, 0);
                assertTrue("H5Zget_filter_info_SCALEOFFSET_DECODE_ENABLED",
                           (filter_flag & H5Z_FILTER_CONFIG_DECODE_ENABLED()) > 0);
                assertTrue("H5Zget_filter_info_SCALEOFFSET_ENCODE_ENABLED",
                           (filter_flag & H5Z_FILTER_CONFIG_ENCODE_ENABLED()) > 0);
                H5Zget_filter_info(H5Z_FILTER_SHUFFLE(), filter_flag_ptr);
                filter_flag = filter_flag_ptr.get(ValueLayout.JAVA_INT, 0);
                assertTrue("H5Zget_filter_info_DECODE_SHUFFLE_ENABLED",
                           (filter_flag & H5Z_FILTER_CONFIG_DECODE_ENABLED()) > 0);
                assertTrue("H5Zget_filter_info_ENCODE_SHUFFLE_ENABLED",
                           (filter_flag & H5Z_FILTER_CONFIG_ENCODE_ENABLED()) > 0);

                if (1 == H5Zfilter_avail(H5Z_FILTER_DEFLATE())) {
                    H5Zget_filter_info(H5Z_FILTER_DEFLATE(), filter_flag_ptr);
                    filter_flag = filter_flag_ptr.get(ValueLayout.JAVA_INT, 0);
                    assertTrue("H5Zget_filter_info_DEFLATE_DECODE_ENABLED",
                               (filter_flag & H5Z_FILTER_CONFIG_DECODE_ENABLED()) > 0);
                    assertTrue("H5Zget_filter_info_DEFLATE_ENCODE_ENABLED",
                               (filter_flag & H5Z_FILTER_CONFIG_ENCODE_ENABLED()) > 0);
                }

                if (1 == H5Zfilter_avail(H5Z_FILTER_SZIP())) {
                    H5Zget_filter_info(H5Z_FILTER_SZIP(), filter_flag_ptr);
                    filter_flag = filter_flag_ptr.get(ValueLayout.JAVA_INT, 0);
                    // Decode should always be available, but we have no way of determining
                    // if encode is so don't assert on that.
                    assertTrue("H5Zget_filter_info_DECODE_SZIP_ENABLED",
                               (filter_flag & H5Z_FILTER_CONFIG_DECODE_ENABLED()) > 0);
                }
            }
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5Zget_filter_info " + err);
        }
    }

    @Test
    public void testH5Zunregister_predefined() throws Throwable
    {
        int filter_found = H5Zfilter_avail(H5Z_FILTER_SHUFFLE());
        assertTrue("H5Zfilter_avail", filter_found > 0);

        int status = H5Zunregister(H5Z_FILTER_SHUFFLE());
        assertTrue("H5Zfilter_avail", status < 0);
    }
}
