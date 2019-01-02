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

import java.io.File;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;
import hdf.hdf5lib.exceptions.HDF5Exception;
import hdf.hdf5lib.exceptions.HDF5LibraryException;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

public class TestH5Dplist {
    @Rule public TestName testname = new TestName();
    private static final String H5_FILE = "testDp.h5";
    private static final int DIM_X = 4;
    private static final int DIM_Y = 7;
    private static final int EDIM_X = 6;
    private static final int EDIM_Y = 10;
    private static final int CHUNK_X = 4;
    private static final int CHUNK_Y = 4;
    private static final int NDIMS = 2;
    private static final int FILLVAL = 99;
    private static final int RANK = 2;
    long H5fid = -1;
    long H5dsid = -1;
    long H5did = -1;
    long H5dcpl_id = -1;
    long[] H5dims = { DIM_X, DIM_Y };
    long[] H5extdims = { EDIM_X, EDIM_Y };
    long[] H5chunk_dims = { CHUNK_X, CHUNK_Y };
    long[] H5maxdims = { HDF5Constants.H5S_UNLIMITED, HDF5Constants.H5S_UNLIMITED };

    private final void _deleteFile(String filename) {
        File file = new File(filename);

        if (file.exists()) {
            try {file.delete();} catch (SecurityException e) {}
        }
    }

    private final void _createPDataset(long fid, long dsid, String name, long dcpl_val) {
        try {
            H5dcpl_id = H5.H5Pcreate(dcpl_val);
        }
        catch (Exception err) {
            err.printStackTrace();
            fail("H5.H5Pcreate: " + err);
        }
        assertTrue("TestH5Dplist._createPDataset: ", H5dcpl_id > 0);

        // Set the chunk size.
        try {
            H5.H5Pset_chunk(H5dcpl_id, NDIMS, H5chunk_dims);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Set the fill value for the dataset
        try {
            int[] fill_value = { FILLVAL };
            H5.H5Pset_fill_value(H5dcpl_id, HDF5Constants.H5T_NATIVE_INT, fill_value);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Set the allocation time to "early". This way we can be sure
        // that reading from the dataset immediately after creation will
        // return the fill value.
        try {
            H5.H5Pset_alloc_time(H5dcpl_id, HDF5Constants.H5D_ALLOC_TIME_EARLY);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        _createDataset(H5fid, H5dsid, "dset", H5dcpl_id, HDF5Constants.H5P_DEFAULT);
    }

    private final void _createDataset(long fid, long dsid, String name, long dcpl, long dapl) {
        try {
            H5did = H5.H5Dcreate(fid, name,
                        HDF5Constants.H5T_STD_I32BE, dsid,
                        HDF5Constants.H5P_DEFAULT, dcpl, dapl);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("H5.H5Dcreate: " + err);
        }
        assertTrue("TestH5Dplist._createDataset: ",H5did > 0);
    }

    @Before
    public void createH5file() throws NullPointerException, HDF5Exception {
        assertTrue("H5 open ids is 0",H5.getOpenIDCount()==0);
        System.out.print(testname.getMethodName());
        try {
            H5fid = H5.H5Fcreate(H5_FILE, HDF5Constants.H5F_ACC_TRUNC,
                    HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
            H5dsid = H5.H5Screate_simple(RANK, H5dims, H5maxdims);
        }
        catch (Throwable err) {
            err.printStackTrace();
            fail("TestH5Dplist.createH5file: " + err);
        }
        assertTrue("TestH5Dplist.createH5file: H5.H5Fcreate: ",H5fid > 0);
        assertTrue("TestH5Dplist.createH5file: H5.H5Screate_simple: ",H5dsid > 0);

        H5.H5Fflush(H5fid, HDF5Constants.H5F_SCOPE_LOCAL);
    }

    @After
    public void deleteH5file() throws HDF5LibraryException {
        if (H5dcpl_id >= 0)
            try {H5.H5Pclose(H5dcpl_id);} catch (Exception ex) {}
        if (H5did > 0)
            try {H5.H5Dclose(H5did);} catch (Exception ex) {}
        if (H5dsid > 0)
            try {H5.H5Sclose(H5dsid);} catch (Exception ex) {}
        if (H5fid > 0)
            try {H5.H5Fclose(H5fid);} catch (Exception ex) {}

        _deleteFile(H5_FILE);
        System.out.println();
    }

  @Test
  public void testH5Dset_extent() {
      int[][] write_dset_data = new int[DIM_X][DIM_Y];
      int[][] read_dset_data = new int[DIM_X][DIM_Y];
      int[][] extend_dset_data = new int[EDIM_X][EDIM_Y];

      // Initialize the dataset.
      for (int indx = 0; indx < DIM_X; indx++)
          for (int jndx = 0; jndx < DIM_Y; jndx++)
              write_dset_data[indx][jndx] = indx * jndx - jndx;

      _createPDataset(H5fid, H5dsid, "dset", HDF5Constants.H5P_DATASET_CREATE);

      // Read values from the dataset, which has not been written to yet.
      try {
          H5.H5Dread(H5did, HDF5Constants.H5T_NATIVE_INT,
                      HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                      HDF5Constants.H5P_DEFAULT, read_dset_data);
      }
      catch (Exception e) {
          e.printStackTrace();
      }
      assertTrue("testH5Dset_extent - H5.H5Dread: ", read_dset_data[0][0] == 99);

      // Write the data to the dataset.
      try {
          H5.H5Dwrite(H5did, HDF5Constants.H5T_NATIVE_INT,
                      HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                      HDF5Constants.H5P_DEFAULT, write_dset_data);
      }
      catch (Exception e) {
          e.printStackTrace();
      }

      // Read the data back.
      try {
          H5.H5Dread(H5did, HDF5Constants.H5T_NATIVE_INT,
                  HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                  HDF5Constants.H5P_DEFAULT, read_dset_data);
      }
      catch (Exception e) {
          e.printStackTrace();
      }
      assertTrue("testH5Dset_extent - H5.H5Dread: ", read_dset_data[3][6] == 12);

      // Extend the dataset.
      try {
          H5.H5Dset_extent(H5did, H5extdims);
      }
      catch (Exception e) {
          e.printStackTrace();
      }

      // Read from the extended dataset.
      try {
          H5.H5Dread(H5did, HDF5Constants.H5T_NATIVE_INT,
                  HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                  HDF5Constants.H5P_DEFAULT, extend_dset_data);
      }
      catch (Exception e) {
          e.printStackTrace();
      }
      assertTrue("testH5Dset_extent - H5.H5Dread: ", extend_dset_data[3][6] == 12);
      assertTrue("testH5Dset_extent - H5.H5Dread: ", extend_dset_data[4][8] == 99);
  }

  @Test
  public void testH5P_dset_no_attrs_hint() {
      boolean ret_val_id = true;

      _createPDataset(H5fid, H5dsid, "dset", HDF5Constants.H5P_DATASET_CREATE);

      try {
          ret_val_id = H5.H5Pget_dset_no_attrs_hint(H5dcpl_id);
          assertFalse("H5P_dset_no_attrs_hint", ret_val_id);
          H5.H5Pset_dset_no_attrs_hint(H5dcpl_id, true);
          ret_val_id = H5.H5Pget_dset_no_attrs_hint(H5dcpl_id);
          assertTrue("H5P_dset_no_attrs_hint", ret_val_id);
      }
      catch (Throwable err) {
          err.printStackTrace();
          fail("H5P_dset_no_attrs_hint: " + err);
      }
  }

}
