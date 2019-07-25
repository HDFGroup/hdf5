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

/*
 * Programmer:  John Mainzer
 *              2017-10-10
 *
 * Purpose:	The public header file for the ros3 driver.
 */
#ifndef H5FDros3_H
#define H5FDros3_H

#define H5FD_ROS3 (H5FD_ros3_init())

#ifdef __cplusplus
extern "C" {
#endif

/****************************************************************************
 *
 * Structure: H5FD_ros3_fapl_t
 *
 * Purpose:
 *
 *     H5FD_ros3_fapl_t is a public structure that is used to pass S3 
 *     authentication data to the appropriate S3 VFD via the FAPL.  A pointer 
 *     to an instance of this structure is a parameter to H5Pset_fapl_ros3() 
 *     and H5Pget_fapl_ros3().
 *
 *
 *
 * `version` (int32_t)
 *
 *     Version number of the H5FD_ros3_fapl_t structure.  Any instance passed 
 *     to the above calls must have a recognized version number, or an error
 *     will be flagged.
 *
 *     This field should be set to H5FD__CURR_ROS3_FAPL_T_VERSION.
 *
 * `authenticate` (hbool_t)
 *
 *     Flag TRUE or FALSE whether or not requests are to be authenticated
 *     with the AWS4 algorithm. 
 *     If TRUE, `aws_region`, `secret_id`, and `secret_key` must be populated. 
 *     If FALSE, those three components are unused.
 *
 * `aws_region` (char[])
 *
 *     String: name of the AWS "region" of the host, e.g. "us-east-1".
 *
 * `secret_id` (char[])
 *
 *     String: "Access ID" for the resource.
 *
 * `secret_key` (char[])
 *
 *     String: "Secret Access Key" associated with the ID and resource.
 *
 *
 *
 * Programmer: John Mainzer
 *
 * Changes:
 *
 *     - Add documentation of fields (except `version`)
 *     --- Jacob Smith 2017-12-04
 *
 ****************************************************************************/

#define H5FD__CURR_ROS3_FAPL_T_VERSION     1

#define H5FD__ROS3_MAX_REGION_LEN         32
#define H5FD__ROS3_MAX_SECRET_ID_LEN     128
#define H5FD__ROS3_MAX_SECRET_KEY_LEN    128

typedef struct H5FD_ros3_fapl_t {
    int32_t version;
    hbool_t authenticate;
    char    aws_region[H5FD__ROS3_MAX_REGION_LEN + 1];
    char    secret_id[H5FD__ROS3_MAX_SECRET_ID_LEN + 1];
    char    secret_key[H5FD__ROS3_MAX_SECRET_KEY_LEN + 1];
} H5FD_ros3_fapl_t;

H5_DLL hid_t H5FD_ros3_init(void);
H5_DLL herr_t H5Pget_fapl_ros3(hid_t fapl_id, H5FD_ros3_fapl_t * fa_out);
H5_DLL herr_t H5Pset_fapl_ros3(hid_t fapl_id, H5FD_ros3_fapl_t * fa);

#ifdef __cplusplus
}
#endif

#endif /* ifndef H5FDros3_H */


