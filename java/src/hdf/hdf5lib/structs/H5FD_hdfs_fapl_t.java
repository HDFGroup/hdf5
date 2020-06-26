/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Read-Only HDFS Virtual File Driver (VFD)                                  *
 * Copyright (c) 2018, The HDF Group.                                        *
 *                                                                           *
 * All rights reserved.                                                      *
 *                                                                           *
 * NOTICE:                                                                   *
 * All information contained herein is, and remains, the property of The HDF *
 * Group. The intellectual and technical concepts contained herein are       *
 * proprietary to The HDF Group. Dissemination of this information or        *
 * reproduction of this material is strictly forbidden unless prior written  *
 * permission is obtained from The HDF Group.                                *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

package hdf.hdf5lib.structs;

import java.io.Serializable;

/*
 * Java representation of the HDFS VFD file access property list (fapl)
 * structure.
 *
 * Used for the access of files hosted on the Hadoop Distributed File System.
 */

public class H5FD_hdfs_fapl_t implements Serializable {
    private static final long serialVersionUID = 2072473407027648309L;

    private int    version;
    private String namenode_name;
    private int    namenode_port;
    private String user_name;
    private String kerberos_ticket_cache;
    private int    stream_buffer_size;

    /*
     * Create a fapl_t structure with the specified components.
     */
    public H5FD_hdfs_fapl_t(
            String namenode_name,
            int    namenode_port,
            String user_name,
            String kerberos_ticket_cache,
            int    stream_buffer_size)
    {
        this.version                = 1;
        this.namenode_name          = namenode_name;
        this.namenode_port          = namenode_port;
        this.user_name              = user_name;
        this.kerberos_ticket_cache  = kerberos_ticket_cache;
        this.stream_buffer_size     = stream_buffer_size;
    }

    @Override
    public boolean equals(Object o) {
        if (o == null)
            return false;
        if (!(o instanceof H5FD_hdfs_fapl_t))
            return false;

        H5FD_hdfs_fapl_t other = (H5FD_hdfs_fapl_t)o;
        if (this.version != other.version)
            return false;
        if (!this.namenode_name.equals(other.namenode_name))
            return false;
        if (this.namenode_port != other.namenode_port)
            return false;
        if (!this.user_name.equals(other.user_name))
            return false;
        if (!this.kerberos_ticket_cache.equals(other.kerberos_ticket_cache))
            return false;
        if (this.stream_buffer_size != other.stream_buffer_size)
            return false;
        return true;
    }

    @Override
    public int hashCode() {
        /* this is a _very bad_ hash algorithm for purposes of hashing! */
        /* implemented to satisfy the "contract" regarding equality     */
        int k = (int)this.version;
        k += this.namenode_name.length();
        k += this.user_name.length();
        k += this.kerberos_ticket_cache.length();
        k += namenode_port;
        k += stream_buffer_size;
        return k;
    }

    @Override
    public String toString() {
    return "H5FD_hdfs_fapl_t (Version: " + this.version + ") {" +
           "\n    namenode_name: '" + this.namenode_name +
           "'\n    namenode_port: " + this.namenode_port +
           "\n    user_name: '" + this.user_name +
           "'\n    kerberos_ticket_cache: '" + this.kerberos_ticket_cache +
           "'\n    stream_buffer_size: " + this.stream_buffer_size +
           "\n}\n";
    }
}


