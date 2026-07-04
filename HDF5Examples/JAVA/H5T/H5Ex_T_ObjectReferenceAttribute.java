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

/************************************************************
  This example shows how to read and write object references
  to an attribute.  The program first creates objects in the
  file and writes references to those objects to an
  attribute with a dataspace of DIM0, then closes the file.
  Next, it reopens the file, dereferences the references,
  and outputs the names of their targets to the screen.
 ************************************************************/

import static org.hdfgroup.javahdf5.hdf5_h.*;

import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

public class H5Ex_T_ObjectReferenceAttribute {
    private static String FILENAME      = "H5Ex_T_ObjectReferenceAttribute.h5";
    private static String DATASETNAME   = "DS1";
    private static String ATTRIBUTENAME = "A1";
    private static String DATASETNAME2  = "DS2";
    private static String GROUPNAME     = "G1";
    private static final int DIM0       = 2;
    private static final int RANK       = 1;

    // Values for the status of space allocation
    enum H5G_obj {
        H5G_UNKNOWN(H5O_TYPE_UNKNOWN()),     /* Unknown object type */
        H5G_GROUP(H5O_TYPE_GROUP()),         /* Object is a group */
        H5G_DATASET(H5O_TYPE_DATASET()),     /* Object is a dataset */
        H5G_TYPE(H5O_TYPE_NAMED_DATATYPE()); /* Object is a named data type */
        private static final Map<Integer, H5G_obj> lookup = new HashMap<Integer, H5G_obj>();

        static
        {
            for (H5G_obj s : EnumSet.allOf(H5G_obj.class))
                lookup.put(s.getCode(), s);
        }

        private int code;

        H5G_obj(int layout_type) { this.code = layout_type; }

        public int getCode() { return this.code; }

        public static H5G_obj get(int code) { return lookup.get(code); }
    }

    private static void CreateDataset(Arena arena)
    {
        long file_id      = H5I_INVALID_HID();
        long dataspace_id = H5I_INVALID_HID();
        long group_id     = H5I_INVALID_HID();
        long dataset_id   = H5I_INVALID_HID();
        long attribute_id = H5I_INVALID_HID();
        long[] dims       = {DIM0};

        // Allocate MemorySegments for references
        MemorySegment[] refs = new MemorySegment[DIM0];
        for (int i = 0; i < DIM0; i++) {
            refs[i] = arena.allocate(H5R_REF_BUF_SIZE());
        }

        // Create a new file using default properties.
        try {
            file_id = H5Fcreate(arena.allocateFrom(FILENAME), H5F_ACC_TRUNC(), H5P_DEFAULT(), H5P_DEFAULT());
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create dataset with a scalar dataspace.
        try {
            dataspace_id = H5Screate(H5S_SCALAR());
            if ((file_id >= 0) && (dataspace_id >= 0)) {
                dataset_id = H5Dcreate2(file_id, arena.allocateFrom(DATASETNAME2), H5T_STD_I32LE_g(),
                                        dataspace_id, H5P_DEFAULT(), H5P_DEFAULT(), H5P_DEFAULT());
                if (dataset_id >= 0)
                    H5Dclose(dataset_id);
                dataset_id = H5I_INVALID_HID();
                H5Sclose(dataspace_id);
                dataspace_id = H5I_INVALID_HID();
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create a group in the file.
        try {
            if (file_id >= 0)
                group_id = H5Gcreate2(file_id, arena.allocateFrom(GROUPNAME), H5P_DEFAULT(), H5P_DEFAULT(),
                                      H5P_DEFAULT());
            if (group_id >= 0)
                H5Gclose(group_id);
            group_id = H5I_INVALID_HID();
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            if (file_id >= 0) {
                try {
                    H5Rcreate_object(file_id, arena.allocateFrom(GROUPNAME), H5P_DEFAULT(), refs[0]);
                }
                catch (Throwable err) {
                    err.printStackTrace();
                }

                try {
                    H5Rcreate_object(file_id, arena.allocateFrom(DATASETNAME2), H5P_DEFAULT(), refs[1]);
                }
                catch (Throwable err) {
                    err.printStackTrace();
                }
            }

            // Create dataset with a scalar dataspace to serve as the parent
            // for the attribute.
            try {
                dataspace_id = H5Screate(H5S_SCALAR());
                if (dataspace_id >= 0) {
                    dataset_id = H5Dcreate2(file_id, arena.allocateFrom(DATASETNAME), H5T_STD_I32LE_g(),
                                            dataspace_id, H5P_DEFAULT(), H5P_DEFAULT(), H5P_DEFAULT());
                    H5Sclose(dataspace_id);
                    dataspace_id = H5I_INVALID_HID();
                }
            }
            catch (Exception e) {
                e.printStackTrace();
            }

            // Create dataspace. Setting maximum size to NULL sets the maximum
            // size to be the current size.
            try {
                dataspace_id = H5Screate_simple(RANK, arena.allocateFrom(ValueLayout.JAVA_LONG, dims),
                                                MemorySegment.NULL);
            }
            catch (Exception e) {
                e.printStackTrace();
            }

            // Create the attribute and write the array data to it.
            try {
                if ((dataset_id >= 0) && (dataspace_id >= 0))
                    attribute_id = H5Acreate2(dataset_id, arena.allocateFrom(ATTRIBUTENAME), H5T_STD_REF_g(),
                                              dataspace_id, H5P_DEFAULT(), H5P_DEFAULT());
            }
            catch (Exception e) {
                e.printStackTrace();
            }

            // Write the attribute.
            try {
                if (attribute_id >= 0) {
                    // Pack references into contiguous MemorySegment
                    int refSize           = H5R_REF_BUF_SIZE();
                    MemorySegment refData = arena.allocate(refSize * DIM0);
                    for (int i = 0; i < DIM0; i++) {
                        MemorySegment.copy(refs[i], 0, refData, i * refSize, refSize);
                    }
                    H5Awrite(attribute_id, H5T_STD_REF_g(), refData);
                }
            }
            catch (Exception e) {
                e.printStackTrace();
            }
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }
        finally {
            try {
                H5Rdestroy(refs[1]);
            }
            catch (Exception ex) {
            }
            try {
                H5Rdestroy(refs[0]);
            }
            catch (Exception ex) {
            }
        }

        // End access to the dataset and release resources used by it.
        try {
            if (attribute_id >= 0)
                H5Aclose(attribute_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            if (dataset_id >= 0)
                H5Dclose(dataset_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            if (dataspace_id >= 0)
                H5Sclose(dataspace_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the file.
        try {
            if (file_id >= 0)
                H5Fclose(file_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static void ReadDataset(Arena arena)
    {
        long file_id      = H5I_INVALID_HID();
        long dataspace_id = H5I_INVALID_HID();
        long dataset_id   = H5I_INVALID_HID();
        long attribute_id = H5I_INVALID_HID();
        int object_type   = -1;
        long object_id    = H5I_INVALID_HID();
        long[] dims       = {DIM0};

        // Allocate MemorySegments for references
        MemorySegment[] refs = new MemorySegment[DIM0];
        for (int i = 0; i < DIM0; i++) {
            refs[i] = arena.allocate(H5R_REF_BUF_SIZE());
        }

        // Open an existing file.
        try {
            file_id = H5Fopen(arena.allocateFrom(FILENAME), H5F_ACC_RDONLY(), H5P_DEFAULT());

            // Open an existing dataset.
            try {
                dataset_id = H5Dopen2(file_id, arena.allocateFrom(DATASETNAME), H5P_DEFAULT());

                try {
                    attribute_id =
                        H5Aopen_by_name(dataset_id, arena.allocateFrom("."),
                                        arena.allocateFrom(ATTRIBUTENAME), H5P_DEFAULT(), H5P_DEFAULT());

                    // Get dataspace and allocate memory for read buffer.
                    try {
                        dataspace_id          = H5Aget_space(attribute_id);
                        MemorySegment dimsSeg = arena.allocateFrom(ValueLayout.JAVA_LONG, dims);
                        H5Sget_simple_extent_dims(dataspace_id, dimsSeg, MemorySegment.NULL);
                        // Read back the dimensions
                        for (int i = 0; i < dims.length; i++) {
                            dims[i] = dimsSeg.getAtIndex(ValueLayout.JAVA_LONG, i);
                        }

                        // Read data.
                        // Read data into contiguous MemorySegment

                        int refSize = H5R_REF_BUF_SIZE();

                        MemorySegment refData = arena.allocate(refSize * dims[0]);

                        H5Aread(attribute_id, H5T_STD_REF_g(), refData);

                        // Unpack references from contiguous MemorySegment

                        for (int i = 0; i < dims[0]; i++) {

                            MemorySegment.copy(refData, i * refSize, refs[i], 0, refSize);
                        }

                        // Output the data to the screen.
                        for (int indx = 0; indx < dims[0]; indx++) {
                            System.out.println(ATTRIBUTENAME + "[" + indx + "]:");
                            System.out.print("  ->");
                            // Open the referenced object, get its name and type.
                            try {
                                object_id = H5Ropen_object(refs[indx], H5P_DEFAULT(), H5P_DEFAULT());
                                try {
                                    // Get object type
                                    MemorySegment objTypeSeg = arena.allocate(ValueLayout.JAVA_INT);
                                    H5Rget_obj_type3(refs[indx], H5P_DEFAULT(), objTypeSeg);
                                    object_type     = objTypeSeg.get(ValueLayout.JAVA_INT, 0);
                                    String obj_name = null;
                                    if (object_type >= 0) {
                                        // Get the name.
                                        // Get the name - first query size
                                        long name_size = H5Iget_name(object_id, MemorySegment.NULL, 0);
                                        if (name_size > 0) {
                                            MemorySegment nameBuffer = arena.allocate(name_size + 1);
                                            H5Iget_name(object_id, nameBuffer, name_size + 1);
                                            obj_name = nameBuffer.getString(0);
                                        }
                                    }
                                    if ((object_id >= 0) && (object_type >= -1)) {
                                        switch (H5G_obj.get(object_type)) {
                                        case H5G_GROUP:
                                            System.out.print("H5G_GROUP");
                                            break;
                                        case H5G_DATASET:
                                            System.out.print("H5G_DATASET");
                                            break;
                                        case H5G_TYPE:
                                            System.out.print("H5G_TYPE");
                                            break;
                                        default:
                                            System.out.print("UNHANDLED");
                                        }
                                    }
                                    // Print the name.
                                    System.out.println(": " + obj_name);
                                }
                                catch (Exception e) {
                                    e.printStackTrace();
                                }
                                finally {
                                    try {
                                        H5Oclose(object_id);
                                    }
                                    catch (Exception e) {
                                    }
                                }
                            }
                            catch (Exception e5) {
                                e5.printStackTrace();
                            }
                            finally {
                                try {
                                    H5Rdestroy(refs[indx]);
                                }
                                catch (Exception e5) {
                                }
                            }
                        } // end for
                    }
                    catch (Exception e4) {
                        e4.printStackTrace();
                    }
                    finally {
                        try {
                            H5Sclose(dataspace_id);
                        }
                        catch (Exception e3) {
                        }
                    }
                }
                catch (Exception e3) {
                    e3.printStackTrace();
                }
                finally {
                    try {
                        H5Aclose(attribute_id);
                    }
                    catch (Exception e4) {
                    }
                }
            }
            catch (Exception e2) {
                e2.printStackTrace();
            }
            finally {
                try {
                    H5Dclose(dataset_id);
                }
                catch (Exception e2) {
                }
            }
        }
        catch (Exception e1) {
            e1.printStackTrace();
        }
        finally {
            try {
                H5Fclose(file_id);
            }
            catch (Exception e1) {
            }
        }
    }

    public static void main(String[] args)
    {

        try (Arena arena = Arena.ofConfined()) {
            H5Ex_T_ObjectReferenceAttribute.CreateDataset(arena);
            H5Ex_T_ObjectReferenceAttribute.ReadDataset(arena);
        }
    }
}
