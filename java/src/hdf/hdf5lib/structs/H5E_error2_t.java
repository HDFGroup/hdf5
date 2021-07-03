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

package hdf.hdf5lib.structs;

import java.io.Serializable;

/**
 * Information struct for Attribute (For H5Ewalk)
 *
 */
public class H5E_error2_t implements Serializable{
    private static final long serialVersionUID = 279144359041667613L;
    /** class ID */
    public long   cls_id;
    /** major error ID */
    public long   maj_num;
    /** minor error number */
    public long   min_num;
    /** line in file where error occurs */
    public int    line;
    /** function in which error occurred */
    public String func_name;
    /** file in which error occurred */
    public String file_name;
    /** optional supplied description */
    public String desc;

    H5E_error2_t(long cls_id, long maj_num, long min_num, int line, String func_name, String file_name, String desc) {
        this.cls_id = cls_id;
        this.maj_num = maj_num;
        this.min_num = min_num;
        this.line = line;
        this.func_name = func_name;
        this.file_name = file_name;
        this.desc = desc;
    }
}
