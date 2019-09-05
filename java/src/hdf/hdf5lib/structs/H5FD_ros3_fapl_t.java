/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Read-Only S3 Virtual File Driver (VFD)                                    *
 * Copyright (c) 2017-2018, The HDF Group.                                   *
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
 * Java representation of the ROS3 VFD file access property list (fapl)
 * structure.
 *
 * Used for the access of files hosted remotely on S3 by Amazon.
 *
 * For simplicity, implemented assuming that all ROS3 fapls have components:
 * - version
 * - authenticate
 * - aws_region
 * - secret_id
 * - secret_key
 *
 * Future implementations may be created to enable different fapl "shapes"
 * depending on provided version.
 *
 * proposed:
 *
 *     H5FD_ros3_fapl_t (super class, has only version field)
 *     H5FD_ros3_fapl_v1_t (extends super with Version 1 components)
 *     H5FD_ros3_fapl_v2_t (extends super with Version 2 components)
 *     and so on, for each version
 *
 *     "super" is passed around, and is version-checked and re-cast as
 *     appropriate
 */

public class H5FD_ros3_fapl_t implements Serializable {
    private static final long serialVersionUID = 8985533001471224030L;

    private int   version;
    private boolean authenticate;
    private String aws_region;
    private String secret_id;
    private String secret_key;

    /**
     * Create a "default" fapl_t structure, for anonymous access.
     */
    public H5FD_ros3_fapl_t () {
        /* H5FD_ros3_fapl_t("", "", ""); */ /* defer */
        this.version = 1;
        this.aws_region = "";
        this.secret_id = "";
        this.secret_key = "";
    }

    /**
     * Create a fapl_t structure with the specified components.
     * If all are the empty string, is anonymous (non-authenticating).
     * Region and ID must both be supplied for authentication.
     *
     * @param region "aws region" for authenticating request
     * @param id "secret id" or "access id" for authenticating request
     * @param key "secret key" or "access key" for authenticating request
     */
    public H5FD_ros3_fapl_t (String region, String id, String key) {
        this.version    = 1; /* must equal H5FD_CURR_ROS3_FAPL_T_VERSION */
                             /* as found in H5FDros3.h                    */
        this.aws_region = region;
        this.secret_id  = id;
        this.secret_key = key;
    }

    @Override
    public boolean equals(Object o) {
        if (o == null)
            return false;
        if (!(o instanceof H5FD_ros3_fapl_t))
            return false;

        H5FD_ros3_fapl_t other = (H5FD_ros3_fapl_t)o;
        if (this.version != other.version)
            return false;
        if (!this.aws_region.equals(other.aws_region))
            return false;
        if (!this.secret_key.equals(other.secret_key))
            return false;
        if (!this.secret_id.equals(other.secret_id))
            return false;
        return true;
    }

    @Override
    public int hashCode() {
        /* this is a _very bad_ hash algorithm for purposes of hashing! */
        /* implemented to satisfy the "contract" regarding equality     */
        int k = (int)this.version;
        k += this.aws_region.length();
        k += this.secret_id.length();
        k += this.secret_key.length();
        return k;
    }

    @Override
    public String toString() {
    return "H5FD_ros3_fapl_t (Version:" + this.version + ") {" +
            "\n    aws_region : " + this.aws_region +
            "\n    secret_id  : " + this.secret_id +
            "\n    secret_key : " + this.secret_key +
            "\n}\n";
    }
}


