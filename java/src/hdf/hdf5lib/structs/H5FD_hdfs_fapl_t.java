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
 * Java representation of the HDFS VFD file access property list (fapl)
 * structure.
 *
 * Used for the access of files hosted on the Hadoop Distributed File System.
 */

public class H5FD_hdfs_fapl_t implements Serializable {
    private static final long serialVersionUID = 2072473407027648309L;

    /** Version number of the H5FD_hdfs_fapl_t structure. */
    private int version;
    /** Name of "Name Node" to access as the HDFS server. */
    private String namenode_name;
    /** Port number to use to connect with Name Node. */
    private int namenode_port;
    /** Username to use when accessing file. */
    private String user_name;
    /** Path to the location of the Kerberos authentication cache. */
    private String kerberos_ticket_cache;
    /** Size (in bytes) of the file read stream buffer. */
    private int stream_buffer_size;

    /**
     * Create a fapl_t structure with the specified components.
     * @param namenode_name
     *     Name of "Name Node" to access as the HDFS server.
     * @param namenode_port
     *     Port number to use to connect with Name Node.
     * @param user_name
     *     Username to use when accessing file.
     * @param kerberos_ticket_cache
     *     Path to the location of the Kerberos authentication cache.
     * @param stream_buffer_size
     *     Size (in bytes) of the file read stream buffer.
     */
    public H5FD_hdfs_fapl_t(String namenode_name, int namenode_port, String user_name,
                            String kerberos_ticket_cache, int stream_buffer_size)
    {
        this.version               = 1;
        this.namenode_name         = namenode_name;
        this.namenode_port         = namenode_port;
        this.user_name             = user_name;
        this.kerberos_ticket_cache = kerberos_ticket_cache;
        this.stream_buffer_size    = stream_buffer_size;
    }

    @Override
    public boolean equals(Object o)
    {
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
    public int hashCode()
    {
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
    public String toString()
    {
        return "H5FD_hdfs_fapl_t (Version: " + this.version + ") {"
            + "\n    namenode_name: '" + this.namenode_name + "'\n    namenode_port: " + this.namenode_port +
            "\n    user_name: '" + this.user_name + "'\n    kerberos_ticket_cache: '" +
            this.kerberos_ticket_cache + "'\n    stream_buffer_size: " + this.stream_buffer_size + "\n}\n";
    }
}
