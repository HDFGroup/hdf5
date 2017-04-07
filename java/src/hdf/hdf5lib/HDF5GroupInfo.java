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


package hdf.hdf5lib;

/**
 * <p>
 * This class is a container for the information reported about an HDF5 Object
 * from the H5Gget_obj_info() method.
 * <p>
 * The fileno and objno fields contain four values which uniquely identify an
 * object among those HDF5 files which are open: if all four values are the same
 * between two objects, then the two objects are the same (provided both files
 * are still open). The nlink field is the number of hard links to the object or
 * zero when information is being returned about a symbolic link (symbolic links
 * do not have hard links but all other objects always have at least one). The
 * type field contains the type of the object, one of H5G_GROUP, H5G_DATASET, or
 * H5G_LINK. The mtime field contains the modification time. If information is
 * being returned about a symbolic link then linklen will be the length of the
 * link value (the name of the pointed-to object with the null terminator);
 * otherwise linklen will be zero. Other fields may be added to this structure
 * in the future.
 */

public class HDF5GroupInfo {
    long[] fileno;
    long[] objno;
    int nlink;
    int type;
    long mtime;
    int linklen;

    public HDF5GroupInfo() {
        fileno = new long[2];
        objno = new long[2];
        nlink = -1;
        type = -1;
        mtime = 0;
        linklen = 0;
    }

    /**
     * Sets the HDF5 group information. Used by the JHI5.
     *
     * @param fn
     *            File id number
     * @param on
     *            Object id number
     * @param nl
     *            Number of links
     * @param t
     *            Type of the object
     * @param mt
     *            Modification time
     * @param len
     *            Length of link
     **/
    public void setGroupInfo(long[] fn, long[] on, int nl, int t, long mt,
            int len) {
        fileno = fn;
        objno = on;
        nlink = nl;
        type = t;
        mtime = mt;
        linklen = len;
    }

    /** Resets all the group information to defaults. */
    public void reset() {
        fileno[0] = 0;
        fileno[1] = 0;
        objno[0] = 0;
        objno[1] = 0;
        nlink = -1;
        type = -1;
        mtime = 0;
        linklen = 0;
    }

    /* accessors */
    public long[] getFileno() {
        return fileno;
    }

    public long[] getObjno() {
        return objno;
    }

    public int getType() {
        return type;
    }

    public int getNlink() {
        return nlink;
    }

    public long getMtime() {
        return mtime;
    }

    public int getLinklen() {
        return linklen;
    }

    /**
     * The fileno and objno fields contain four values which uniquely identify
     * an object among those HDF5 files.
     */
    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof HDF5GroupInfo)) {
            return false;
        }

        HDF5GroupInfo target = (HDF5GroupInfo) obj;
        if ((fileno[0] == target.fileno[0]) && (fileno[1] == target.fileno[1])
                && (objno[0] == target.objno[0])
                && (objno[1] == target.objno[1])) {
            return true;
        }
        else {
            return false;
        }
    }

    /**
     * Returns the object id.
     *
     * @return the object id
     */
    public long getOID() {
        return objno[0];
    }

    /**
     * /** Converts this object to a String representation.
     *
     * @return a string representation of this object
     */
    @Override
    public String toString() {
        String fileStr = "fileno=null";
        String objStr = "objno=null";

        if (fileno != null) {
            fileStr = "fileno[0]=" + fileno[0] + ",fileno[1]=" + fileno[1];
        }

        if (objno != null) {
            objStr = "objno[0]=" + objno[0] + ",objno[1]=" + objno[1];
        }

        return getClass().getName() + "[" + fileStr + "," + objStr + ",type="
                + type + ",nlink=" + nlink + ",mtime=" + mtime + ",linklen="
                + linklen + "]";
    }

}
