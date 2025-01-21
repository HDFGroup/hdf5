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

/*
 *        This file contains a heavily edited and functionally reduce
 *        version of the test code first written by Quincey in a file
 *        of the same name.
 */

#include "cache_common.h"
#include "genall5.h"

#define DSET_DIMS         (1024 * 1024)
#define DSET_SMALL_DIMS   (64 * 1024)
#define DSET_CHUNK_DIMS   1024
#define DSET_COMPACT_DIMS 4096

/*-------------------------------------------------------------------------
 * Function:    ns_grp_0
 *
 * Purpose:     Create an empty "new style" group at the specified location
 *              in the specified file.
 *
 *              If pass is false on entry, do nothing.
 *
 *              If an error is detected, set pass to false, and set
 *              failure_mssg to point to an appropriate error message.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */

void
ns_grp_0(hid_t fid, const char *group_name)
{
    hid_t  gid  = H5I_INVALID_HID;
    hid_t  gcpl = H5I_INVALID_HID;
    herr_t ret;

    if (pass) {
        gcpl = H5Pcreate(H5P_GROUP_CREATE);

        if (gcpl <= 0) {
            pass         = false;
            failure_mssg = "ns_grp_0: H5Pcreate() failed";
        }
        assert(gcpl > 0);
    }

    if (pass) {
        ret = H5Pset_link_creation_order(gcpl, H5P_CRT_ORDER_TRACKED);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "ns_grp_0: H5Pset_link_creation_order() failed";
        }
        assert(ret >= 0);
    }

    if (pass) {
        gid = H5Gcreate2(fid, group_name, H5P_DEFAULT, gcpl, H5P_DEFAULT);

        if (gid <= 0) {
            pass         = false;
            failure_mssg = "ns_grp_0: H5Gcreate2() failed";
        }
        assert(gid > 0);
    }

    if (pass) {
        ret = H5Pclose(gcpl);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "ns_grp_0: H5Pclose(gcpl) failed";
        }
        assert(ret >= 0);
    }

    if (pass) {
        ret = H5Gclose(gid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "ns_grp_0: H5Gclose(gid) failed";
        }
        assert(ret >= 0);
    }

} /* ns_grp_0 */

/*-------------------------------------------------------------------------
 * Function:    vrfy_ns_grp_0
 *
 * Purpose:     verify an empty "new style" group at the specified location
 *              in the specified file.
 *
 *              If pass is false on entry, do nothing.
 *
 *              If an error is detected, set pass to false, and set
 *              failure_mssg to point to an appropriate error message.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */

void
vrfy_ns_grp_0(hid_t fid, const char *group_name)
{
    hid_t      gid  = H5I_INVALID_HID;
    hid_t      gcpl = H5I_INVALID_HID;
    H5G_info_t grp_info;
    unsigned   crt_order_flags = 0;
    herr_t     ret;

    if (pass) {
        gid = H5Gopen2(fid, group_name, H5P_DEFAULT);

        if (gid <= 0) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_0: H5Gopen2() failed";
        }
        assert(gid > 0);
    }

    if (pass) {
        gcpl = H5Gget_create_plist(gid);

        if (gcpl <= 0) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_0: H5Gget_create_plist() failed";
        }
        assert(gcpl > 0);
    }

    if (pass) {
        ret = H5Pget_link_creation_order(gcpl, &crt_order_flags);

        if (gcpl <= 0) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_0: H5Pget_link_creation_order() failed";
        }
        else if (H5P_CRT_ORDER_TRACKED != crt_order_flags) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_0: H5P_CRT_ORDER_TRACKED != crt_order_flags";
        }
        assert(ret >= 0);
        assert(H5P_CRT_ORDER_TRACKED == crt_order_flags);
    }

    if (pass) {
        ret = H5Pclose(gcpl);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_0: H5Pclose() failed";
        }
        assert(ret >= 0);
    }

    if (pass) {
        memset(&grp_info, 0, sizeof(grp_info));
        ret = H5Gget_info(gid, &grp_info);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_0: H5Gget_info() failed";
        }
        else if (H5G_STORAGE_TYPE_COMPACT != grp_info.storage_type) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_0: H5G_STORAGE_TYPE_COMPACT != grp_info.storage_type";
        }
        else if (0 != grp_info.nlinks) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_0: 0 != grp_info.nlinks";
        }
        else if (0 != grp_info.max_corder) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_0: 0 != grp_info.max_corder";
        }
        else if (false != grp_info.mounted) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_0: false != grp_info.mounted";
        }

        assert(ret >= 0);
        assert(H5G_STORAGE_TYPE_COMPACT == grp_info.storage_type);
        assert(0 == grp_info.nlinks);
        assert(0 == grp_info.max_corder);
        assert(false == grp_info.mounted);
    }

    if (pass) {
        ret = H5Gclose(gid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_0: H5Gclose() failed";
        }
        assert(ret >= 0);
    }

} /* vrfy_ns_grp_0() */

/*-------------------------------------------------------------------------
 * Function:    ns_grp_c
 *
 * Purpose:     Create a compact "new style" group, with 'nlinks'
 *              soft/hard/external links in it in the specified file.
 *
 *              If pass is false on entry, do nothing.
 *
 *              If an error is detected, set pass to false, and set
 *              failure_mssg to point to an appropriate error message.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */

void
ns_grp_c(hid_t fid, const char *group_name, unsigned nlinks)
{
    hid_t    gid  = H5I_INVALID_HID;
    hid_t    gcpl = H5I_INVALID_HID;
    unsigned max_compact;
    unsigned u;
    herr_t   ret;

    if (pass) {
        gcpl = H5Pcreate(H5P_GROUP_CREATE);

        if (gcpl <= 0) {
            pass         = false;
            failure_mssg = "ns_grp_c: H5Pcreate(H5P_GROUP_CREATE) failed";
        }
        assert(gcpl > 0);
    }

    if (pass) {
        ret = H5Pset_link_creation_order(gcpl, H5P_CRT_ORDER_TRACKED);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "ns_grp_c:  H5Pset_link_creation_order() failed";
        }
        assert(ret >= 0);
    }

    if (pass) {
        gid = H5Gcreate2(fid, group_name, H5P_DEFAULT, gcpl, H5P_DEFAULT);

        if (gid <= 0) {
            pass         = false;
            failure_mssg = "ns_grp_c: H5Gcreate2() failed";
        }
        assert(gid > 0);
    }

    if (pass) {
        max_compact = 0;
        ret         = H5Pget_link_phase_change(gcpl, &max_compact, NULL);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "ns_grp_c: H5Pget_link_phase_change() failed";
        }
        else if (nlinks <= 0) {
            pass         = false;
            failure_mssg = "ns_grp_c: nlinks <= 0";
        }
        else if (nlinks >= max_compact) {
            pass         = false;
            failure_mssg = "ns_grp_c: nlinks >= max_compact";
        }

        assert(ret >= 0);
        assert(nlinks > 0);
        assert(nlinks < max_compact);
    }

    u = 0;
    while ((pass) && (u < nlinks)) {
        char linkname[16];

        snprintf(linkname, sizeof(linkname), "%u", u);

        if (0 == (u % 3)) {
            ret = H5Lcreate_soft(group_name, gid, linkname, H5P_DEFAULT, H5P_DEFAULT);

            if (ret < 0) {
                pass         = false;
                failure_mssg = "ns_grp_c: H5Lcreate_soft() failed";
            }
            assert(ret >= 0);
        } /* end if */
        else if (1 == (u % 3)) {
            ret = H5Lcreate_hard(fid, "/", gid, linkname, H5P_DEFAULT, H5P_DEFAULT);

            if (ret < 0) {
                pass         = false;
                failure_mssg = "ns_grp_c: H5Lcreate_hard() failed";
            }
            assert(ret >= 0);
        } /* end else-if */
        else {
            assert(2 == (u % 3));
            ret = H5Lcreate_external("external.h5", "/ext", gid, linkname, H5P_DEFAULT, H5P_DEFAULT);

            if (ret < 0) {
                pass         = false;
                failure_mssg = "ns_grp_c: H5Lcreate_external() failed";
            }
            assert(ret >= 0);
        } /* end else */

        u++;
    } /* end while() */

    if (pass) {
        ret = H5Pclose(gcpl);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "ns_grp_c: H5Pclose(gcpl) failed";
        }
        assert(ret >= 0);
    }

    if (pass) {
        ret = H5Gclose(gid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "ns_grp_c: H5Gclose(gid) failed";
        }
        assert(ret >= 0);
    }

} /* ns_grp_c() */

/*-------------------------------------------------------------------------
 * Function:    vrfy_ns_grp_c
 *
 * Purpose:     Verify a compact "new style" group, with 'nlinks'
 *              soft/hard/external links in it in the specified file.
 *
 *              If pass is false on entry, do nothing.
 *
 *              If an error is detected, set pass to false, and set
 *              failure_mssg to point to an appropriate error message.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */

void
vrfy_ns_grp_c(hid_t fid, const char *group_name, unsigned nlinks)
{
    hid_t      gid  = H5I_INVALID_HID;
    hid_t      gcpl = H5I_INVALID_HID;
    H5G_info_t grp_info;
    unsigned   crt_order_flags = 0;
    unsigned   u;
    herr_t     ret;

    if (pass) {
        gid = H5Gopen2(fid, group_name, H5P_DEFAULT);

        if (gid <= 0) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_c: H5Gopen2() failed";
        }
        assert(gid > 0);
    }

    if (pass) {
        gcpl = H5Gget_create_plist(gid);

        if (gcpl <= 0) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_c: H5Gget_create_plist(gid) failed";
        }
        assert(gcpl > 0);
    }

    if (pass) {
        ret = H5Pget_link_creation_order(gcpl, &crt_order_flags);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_c: H5Pget_link_creation_order() failed";
        }
        else if (H5P_CRT_ORDER_TRACKED != crt_order_flags) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_c: H5P_CRT_ORDER_TRACKED != crt_order_flags";
        }
        assert(ret >= 0);
        assert(H5P_CRT_ORDER_TRACKED == crt_order_flags);
    }

    if (pass) {
        ret = H5Pclose(gcpl);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_c: H5Pclose() failed";
        }
        assert(ret >= 0);
    }

    if (pass) {
        memset(&grp_info, 0, sizeof(grp_info));
        ret = H5Gget_info(gid, &grp_info);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_c: H5Gget_info() failed";
        }
        else if (H5G_STORAGE_TYPE_COMPACT != grp_info.storage_type) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_c: H5G_STORAGE_TYPE_COMPACT != grp_info.storage_type";
        }
        else if (nlinks != grp_info.nlinks) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_c: nlinks != grp_info.nlinks";
        }
        else if (nlinks != grp_info.max_corder) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_c: nlinks != grp_info.max_corder";
        }
        else if (false != grp_info.mounted) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_c: false != grp_info.mounted";
        }

        assert(ret >= 0);
        assert(H5G_STORAGE_TYPE_COMPACT == grp_info.storage_type);
        assert(nlinks == grp_info.nlinks);
        assert(nlinks == grp_info.max_corder);
        assert(false == grp_info.mounted);
    }

    u = 0;
    while ((pass) && (u < nlinks)) {
        H5L_info2_t lnk_info;
        char        linkname[16];
        htri_t      link_exists;

        snprintf(linkname, sizeof(linkname), "%u", u);
        link_exists = H5Lexists(gid, linkname, H5P_DEFAULT);

        if (link_exists < 0) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_c: H5Lexists() failed";
        }
        assert(link_exists >= 0);

        memset(&lnk_info, 0, sizeof(grp_info));
        ret = H5Lget_info2(gid, linkname, &lnk_info, H5P_DEFAULT);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_c: H5Lget_info() failed";
        }
        else if (true != lnk_info.corder_valid) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_c: true != lnk_info.corder_valid";
        }
        else if (u != lnk_info.corder) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_c: u != lnk_info.corder";
        }
        else if (H5T_CSET_ASCII != lnk_info.cset) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_c: H5T_CSET_ASCII != lnk_info.cset";
        }
        assert(ret >= 0);
        assert(true == lnk_info.corder_valid);
        assert(u == lnk_info.corder);
        assert(H5T_CSET_ASCII == lnk_info.cset);

        if (0 == (u % 3)) {
            char *slinkval;

            if (H5L_TYPE_SOFT != lnk_info.type) {
                pass         = false;
                failure_mssg = "vrfy_ns_grp_c: H5L_TYPE_SOFT != lnk_info.type";
            }
            else if ((strlen(group_name) + 1) != lnk_info.u.val_size) {
                pass         = false;
                failure_mssg = "vrfy_ns_grp_c: (strlen(group_name) + 1) != lnk_info.u.val_size";
            }
            assert(H5L_TYPE_SOFT == lnk_info.type);
            assert((strlen(group_name) + 1) == lnk_info.u.val_size);

            slinkval = (char *)malloc(lnk_info.u.val_size);

            if (!slinkval) {
                pass         = false;
                failure_mssg = "vrfy_ns_grp_c: malloc of slinkval failed";
            }
            assert(slinkval);

            ret = H5Lget_val(gid, linkname, slinkval, lnk_info.u.val_size, H5P_DEFAULT);
            if (ret < 0) {
                pass         = false;
                failure_mssg = "vrfy_ns_grp_c: H5Lget_val() failed";
            }
            else if (0 != strcmp(slinkval, group_name)) {
                pass         = false;
                failure_mssg = "vrfy_ns_grp_c: 0 != strcmp(slinkval, group_name)";
            }
            assert(ret >= 0);
            assert(0 == strcmp(slinkval, group_name));

            free(slinkval);
        } /* end if */
        else if (1 == (u % 3)) {
            H5O_info2_t root_oinfo;
            int         token_cmp = 0;

            if (H5L_TYPE_HARD != lnk_info.type) {
                pass         = false;
                failure_mssg = "vrfy_ns_grp_c: H5L_TYPE_HARD != lnk_info.type";
            }
            assert(H5L_TYPE_HARD == lnk_info.type);

            memset(&root_oinfo, 0, sizeof(root_oinfo));
            ret = H5Oget_info3(fid, &root_oinfo, H5O_INFO_BASIC);

            if (ret < 0) {
                pass         = false;
                failure_mssg = "vrfy_ns_grp_c: H5Oget_info() failed.";
            }
            else {
                if (H5Otoken_cmp(fid, &root_oinfo.token, &lnk_info.u.token, &token_cmp) < 0) {
                    pass         = false;
                    failure_mssg = "vrfy_ns_grp_c: H5Otoken_cmp() failed.";
                }

                if (token_cmp) {
                    pass         = false;
                    failure_mssg = "vrfy_ns_grp_c: root_oinfo.token != lnk_info.u.token";
                }
            }
            assert(ret >= 0);
            assert(!token_cmp);
        } /* end else-if */
        else {
            void       *elinkval;
            const char *file = NULL;
            const char *path = NULL;

            assert(2 == (u % 3));

            if (H5L_TYPE_EXTERNAL != lnk_info.type) {
                pass         = false;
                failure_mssg = "vrfy_ns_grp_c: H5L_TYPE_EXTERNAL != lnk_info.type";
            }
            assert(H5L_TYPE_EXTERNAL == lnk_info.type);

            elinkval = malloc(lnk_info.u.val_size);

            if (!elinkval) {
                pass         = false;
                failure_mssg = "vrfy_ns_grp_c: malloc of elinkval failed.";
            }
            assert(elinkval);

            ret = H5Lget_val(gid, linkname, elinkval, lnk_info.u.val_size, H5P_DEFAULT);
            if (ret < 0) {
                pass         = false;
                failure_mssg = "vrfy_ns_grp_c: H5Lget_val() failed.";
            }
            assert(ret >= 0);

            ret = H5Lunpack_elink_val(elinkval, lnk_info.u.val_size, NULL, &file, &path);
            if (ret < 0) {
                pass         = false;
                failure_mssg = "vrfy_ns_grp_c: H5Lunpack_elink_val() failed.";
            }
            else if (0 != strcmp(file, "external.h5")) {
                pass         = false;
                failure_mssg = "vrfy_ns_grp_c: 0 != strcmp(file, \"external.h5\")";
            }
            else if (0 != strcmp(path, "/ext")) {
                pass         = false;
                failure_mssg = "vrfy_ns_grp_c: 0 != strcmp(path, \"/ext\")";
            }
            assert(ret >= 0);
            assert(0 == strcmp(file, "external.h5"));
            assert(0 == strcmp(path, "/ext"));

            free(elinkval);
        } /* end else */

        u++;
    } /* end while */

    if (pass) {
        ret = H5Gclose(gid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_c: H5Gclose() failed.";
        }
        assert(ret >= 0);
    }

} /* vrfy_ns_grp_c() */

/*-------------------------------------------------------------------------
 * Function:    ns_grp_d
 *
 * Purpose:     Create a dense "new style" group, with 'nlinks'
 *              (soft/hard/external) links in it in the specified file.
 *
 *              If pass is false on entry, do nothing.
 *
 *              If an error is detected, set pass to false, and set
 *              failure_mssg to point to an appropriate error message.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */

void
ns_grp_d(hid_t fid, const char *group_name, unsigned nlinks)
{
    hid_t    gid  = H5I_INVALID_HID;
    hid_t    gcpl = H5I_INVALID_HID;
    unsigned max_compact;
    unsigned u;
    herr_t   ret;

    if (pass) {
        gcpl = H5Pcreate(H5P_GROUP_CREATE);

        if (gcpl <= 0) {
            pass         = false;
            failure_mssg = "ns_grp_d: H5Pcreate() failed.";
        }
        assert(gcpl > 0);
    }

    if (pass) {
        ret = H5Pset_link_creation_order(gcpl, H5P_CRT_ORDER_TRACKED);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "ns_grp_d: H5Pset_link_creation_order() failed.";
        }
        assert(ret >= 0);
    }

    if (pass) {
        gid = H5Gcreate2(fid, group_name, H5P_DEFAULT, gcpl, H5P_DEFAULT);

        if (gid <= 0) {
            pass         = false;
            failure_mssg = "ns_grp_d: H5Gcreate2() failed.";
        }
        assert(gid > 0);
    }

    if (pass) {
        max_compact = 0;
        ret         = H5Pget_link_phase_change(gcpl, &max_compact, NULL);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "ns_grp_d: H5Pget_link_phase_change() failed.";
        }
        else if (nlinks <= max_compact) {
            pass         = false;
            failure_mssg = "ns_grp_d: nlinks <= max_compact";
        }
        assert(ret >= 0);
        assert(nlinks > max_compact);
    }

    u = 0;
    while ((pass) && (u < nlinks)) {
        char linkname[16];

        snprintf(linkname, sizeof(linkname), "%u", u);

        if (0 == (u % 3)) {
            ret = H5Lcreate_soft(group_name, gid, linkname, H5P_DEFAULT, H5P_DEFAULT);

            if (ret < 0) {
                pass         = false;
                failure_mssg = "ns_grp_d: H5Lcreate_soft() failed.";
            }
            assert(ret >= 0);
        } /* end if */
        else if (1 == (u % 3)) {
            ret = H5Lcreate_hard(fid, "/", gid, linkname, H5P_DEFAULT, H5P_DEFAULT);

            if (ret < 0) {
                pass         = false;
                failure_mssg = "ns_grp_d: H5Lcreate_hard() failed.";
            }
            assert(ret >= 0);
        } /* end else-if */
        else {
            assert(2 == (u % 3));

            ret = H5Lcreate_external("external.h5", "/ext", gid, linkname, H5P_DEFAULT, H5P_DEFAULT);

            if (ret < 0) {
                pass         = false;
                failure_mssg = "ns_grp_d: H5Lcreate_external() failed.";
            }
            assert(ret >= 0);
        } /* end else */

        u++;
    } /* end while */

    if (pass) {
        ret = H5Pclose(gcpl);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "ns_grp_d: H5Pclose() failed.";
        }
        assert(ret >= 0);
    }

    if (pass) {
        ret = H5Gclose(gid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "ns_grp_d: H5Gclose() failed.";
        }
        assert(ret >= 0);
    }

} /* ns_grp_d() */

/*-------------------------------------------------------------------------
 * Function:    vrfy_ns_grp_d
 *
 * Purpose:     Verify a dense "new style" group, with 'nlinks'
 *              soft/hard/external links in it in the specified file.
 *
 *              If pass is false on entry, do nothing.
 *
 *              If an error is detected, set pass to false, and set
 *              failure_mssg to point to an appropriate error message.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */

void
vrfy_ns_grp_d(hid_t fid, const char *group_name, unsigned nlinks)
{
    hid_t      gid  = H5I_INVALID_HID;
    hid_t      gcpl = H5I_INVALID_HID;
    H5G_info_t grp_info;
    unsigned   crt_order_flags = 0;
    unsigned   u;
    herr_t     ret;

    if (pass) {
        gid = H5Gopen2(fid, group_name, H5P_DEFAULT);

        if (gid <= 0) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_d: H5Gopen2() failed.";
        }
        assert(gid > 0);
    }

    if (pass) {
        gcpl = H5Gget_create_plist(gid);

        if (gcpl <= 0) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_d: H5Gget_create_plist() failed.";
        }
        assert(gcpl > 0);
    }

    if (pass) {
        ret = H5Pget_link_creation_order(gcpl, &crt_order_flags);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_d: H5Pget_link_creation_order() failed.";
        }
        else if (H5P_CRT_ORDER_TRACKED != crt_order_flags) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_d: H5P_CRT_ORDER_TRACKED != crt_order_flags";
        }
        assert(ret >= 0);
        assert(H5P_CRT_ORDER_TRACKED == crt_order_flags);
    }

    if (pass) {
        ret = H5Pclose(gcpl);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_d: H5Pclose() failed.";
        }
        assert(ret >= 0);
    }

    if (pass) {
        memset(&grp_info, 0, sizeof(grp_info));
        ret = H5Gget_info(gid, &grp_info);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_d: H5Gget_info() failed.";
        }
        else if (H5G_STORAGE_TYPE_DENSE != grp_info.storage_type) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_d: H5G_STORAGE_TYPE_DENSE != grp_info.storage_type";
        }
        else if (nlinks != grp_info.nlinks) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_d: nlinks != grp_info.nlinks";
        }
        else if (nlinks != grp_info.max_corder) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_d: nlinks != grp_info.max_corder";
        }
        else if (false != grp_info.mounted) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_d: false != grp_info.mounted";
        }
        assert(ret >= 0);
        assert(H5G_STORAGE_TYPE_DENSE == grp_info.storage_type);
        assert(nlinks == grp_info.nlinks);
        assert(nlinks == grp_info.max_corder);
        assert(false == grp_info.mounted);
    }

    u = 0;
    while ((pass) && (u < nlinks)) {
        H5L_info2_t lnk_info;
        char        linkname[16];
        htri_t      link_exists;

        snprintf(linkname, sizeof(linkname), "%u", u);
        link_exists = H5Lexists(gid, linkname, H5P_DEFAULT);

        if (link_exists < 0) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_d: H5Lexists() failed.";
        }
        assert(link_exists >= 0);

        memset(&lnk_info, 0, sizeof(grp_info));
        ret = H5Lget_info2(gid, linkname, &lnk_info, H5P_DEFAULT);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_d: H5Lget_info() failed.";
        }
        else if (true != lnk_info.corder_valid) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_d: true != lnk_info.corder_valid";
        }
        else if (u != lnk_info.corder) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_d: u != lnk_info.corder";
        }
        else if (H5T_CSET_ASCII != lnk_info.cset) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_d: H5T_CSET_ASCII != lnk_info.cset";
        }
        assert(ret >= 0);
        assert(true == lnk_info.corder_valid);
        assert(u == lnk_info.corder);
        assert(H5T_CSET_ASCII == lnk_info.cset);

        if (0 == (u % 3)) {
            char *slinkval;

            if (H5L_TYPE_SOFT != lnk_info.type) {
                pass         = false;
                failure_mssg = "vrfy_ns_grp_d: H5L_TYPE_SOFT != lnk_info.type";
            }
            else if ((strlen(group_name) + 1) != lnk_info.u.val_size) {
                pass         = false;
                failure_mssg = "vrfy_ns_grp_d: H5L_TYPE_SOFT != lnk_info.type";
            }
            assert(H5L_TYPE_SOFT == lnk_info.type);
            assert((strlen(group_name) + 1) == lnk_info.u.val_size);

            slinkval = (char *)malloc(lnk_info.u.val_size);

            if (!slinkval) {
                pass         = false;
                failure_mssg = "vrfy_ns_grp_d: malloc of slinkval failed";
            }
            assert(slinkval);

            ret = H5Lget_val(gid, linkname, slinkval, lnk_info.u.val_size, H5P_DEFAULT);
            if (ret < 0) {
                pass         = false;
                failure_mssg = "vrfy_ns_grp_d: H5Lget_val() failed";
            }
            else if (0 != strcmp(slinkval, group_name)) {
                pass         = false;
                failure_mssg = "vrfy_ns_grp_d: 0 != strcmp(slinkval, group_name)";
            }
            assert(ret >= 0);
            assert(0 == strcmp(slinkval, group_name));

            free(slinkval);
        } /* end if */
        else if (1 == (u % 3)) {
            H5O_info2_t root_oinfo;
            int         token_cmp = 0;

            if (H5L_TYPE_HARD != lnk_info.type) {
                pass         = false;
                failure_mssg = "vrfy_ns_grp_d: H5L_TYPE_HARD != lnk_info.type";
            }
            assert(H5L_TYPE_HARD == lnk_info.type);

            memset(&root_oinfo, 0, sizeof(root_oinfo));
            ret = H5Oget_info3(fid, &root_oinfo, H5O_INFO_BASIC);
            if (ret < 0) {
                pass         = false;
                failure_mssg = "vrfy_ns_grp_d: H5Oget_info() failed.";
            }
            else {
                if (H5Otoken_cmp(fid, &root_oinfo.token, &lnk_info.u.token, &token_cmp) < 0) {
                    pass         = false;
                    failure_mssg = "vrfy_ns_grp_d: H5Otoken_cmp() failed.";
                }

                if (token_cmp) {
                    pass         = false;
                    failure_mssg = "vrfy_ns_grp_d: root_oinfo.token != lnk_info.u.token";
                }
            }
            assert(ret >= 0);
            assert(!token_cmp);
        } /* end else-if */
        else {
            void       *elinkval;
            const char *file = NULL;
            const char *path = NULL;

            assert(2 == (u % 3));

            if (H5L_TYPE_EXTERNAL != lnk_info.type) {
                pass         = false;
                failure_mssg = "vrfy_ns_grp_d: H5L_TYPE_EXTERNAL != lnk_info.type";
            }
            assert(H5L_TYPE_EXTERNAL == lnk_info.type);

            elinkval = malloc(lnk_info.u.val_size);

            if (!elinkval) {
                pass         = false;
                failure_mssg = "vrfy_ns_grp_d: malloc of elinkval failed.";
            }
            assert(elinkval);

            ret = H5Lget_val(gid, linkname, elinkval, lnk_info.u.val_size, H5P_DEFAULT);
            if (ret < 0) {
                pass         = false;
                failure_mssg = "vrfy_ns_grp_d: H5Lget_val failed.";
            }
            assert(ret >= 0);

            ret = H5Lunpack_elink_val(elinkval, lnk_info.u.val_size, NULL, &file, &path);
            if (ret < 0) {
                pass         = false;
                failure_mssg = "vrfy_ns_grp_d: H5Lunpack_elink_val failed.";
            }
            else if (0 != strcmp(file, "external.h5")) {
                pass         = false;
                failure_mssg = "vrfy_ns_grp_d: 0 != strcmp(file, \"external.h5\").";
            }
            else if (0 != strcmp(path, "/ext")) {
                pass         = false;
                failure_mssg = "vrfy_ns_grp_d: 0 != strcmp(path, \"/ext\")";
            }
            assert(ret >= 0);
            assert(0 == strcmp(file, "external.h5"));
            assert(0 == strcmp(path, "/ext"));

            free(elinkval);
        } /* end else */

        u++;
    } /* end while() */

    if (pass) {
        ret = H5Gclose(gid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ns_grp_d: H5Gclose() failed.";
        }
        assert(ret >= 0);
    }

} /* vrfy_ns_grp_d() */

/*-------------------------------------------------------------------------
 * Function:    os_grp_0
 *
 * Purpose:     Create an empty "old style" group.
 *
 *              If pass is false on entry, do nothing.
 *
 *              If an error is detected, set pass to false, and set
 *              failure_mssg to point to an appropriate error message.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */

void
os_grp_0(hid_t fid, const char *group_name)
{
    hid_t        gid  = H5I_INVALID_HID;
    hid_t        fapl = H5I_INVALID_HID;
    H5F_libver_t low = H5F_LIBVER_ERROR, high = H5F_LIBVER_ERROR;

    herr_t ret;

    if (pass) { /* get the file's file access property list */
        fapl = H5Fget_access_plist(fid);
        if (fapl <= 0) {
            pass         = false;
            failure_mssg = "os_grp_0: H5Fget_access_plist() failed.";
        }
        assert(fapl > 0);
    }

    if (pass) { /* get low and high bounds from fapl */
        ret = H5Pget_libver_bounds(fapl, &low, &high);
        if (ret < 0) {
            pass         = false;
            failure_mssg = "os_grp_0: H5Pget_libver_bounds() failed(1).";
        }
        assert(ret >= 0);
    }

    if (pass) { /* turn file format latest off */
        if (low >= H5F_LIBVER_V18) {
            ret = H5Fset_libver_bounds(fid, H5F_LIBVER_EARLIEST, high);
            if (ret < 0) {
                pass         = false;
                failure_mssg = "os_grp_0: H5Fset_libver_bounds() failed(1).";
            }
            assert(ret >= 0);
        }
    }

    if (pass) {
        gid = H5Gcreate2(fid, group_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        if (gid <= 0) {
            pass         = false;
            failure_mssg = "os_grp_0: H5Gcreate2() failed.";
        }
        assert(gid > 0);
    }

    if (pass) {
        ret = H5Gclose(gid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "os_grp_0: H5Gclose() failed.";
        }
        assert(ret >= 0);
    }

    if (pass) { /* restore low and high bounds */
        if (low >= H5F_LIBVER_V18) {
            ret = H5Fset_libver_bounds(fid, low, high);
            if (ret < 0) {
                pass         = false;
                failure_mssg = "os_grp_0: H5Fset_libver_bounds() failed(1).";
            }
            assert(ret >= 0);
        }
    }

} /* os_grp_0() */

/*-------------------------------------------------------------------------
 * Function:    vrfy_os_grp_0
 *
 * Purpose:     Validate an empty "old style" group.
 *
 *              If pass is false on entry, do nothing.
 *
 *              If an error is detected, set pass to false, and set
 *              failure_mssg to point to an appropriate error message.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */

void
vrfy_os_grp_0(hid_t fid, const char *group_name)
{
    hid_t      gid  = H5I_INVALID_HID;
    hid_t      gcpl = H5I_INVALID_HID;
    H5G_info_t grp_info;
    unsigned   crt_order_flags = 0;
    herr_t     ret;

    if (pass) {
        gid = H5Gopen2(fid, group_name, H5P_DEFAULT);

        if (gid <= 0) {
            pass         = false;
            failure_mssg = "vrfy_os_grp_0: H5Gopen2() failed.";
        }
        assert(gid > 0);
    }

    if (pass) {
        gcpl = H5Gget_create_plist(gid);

        if (gcpl <= 0) {
            pass         = false;
            failure_mssg = "vrfy_os_grp_0: H5Gget_create_plist() failed.";
        }
        assert(gcpl > 0);
    }

    if (pass) {

        ret = H5Pget_link_creation_order(gcpl, &crt_order_flags);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_os_grp_0: H5Pget_link_creation_order() failed";
        }
        else if (0 != crt_order_flags) {
            pass         = false;
            failure_mssg = "vrfy_os_grp_0: 0 != crt_order_flags";
        }
        assert(ret >= 0);
        assert(0 == crt_order_flags);
    }

    if (pass) {
        ret = H5Pclose(gcpl);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_os_grp_0: H5Pclose() failed.";
        }
        assert(ret >= 0);
    }

    if (pass) {
        memset(&grp_info, 0, sizeof(grp_info));
        ret = H5Gget_info(gid, &grp_info);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_os_grp_0: H5Gget_info() failed.";
        }
        else if (H5G_STORAGE_TYPE_SYMBOL_TABLE != grp_info.storage_type) {
            pass         = false;
            failure_mssg = "vrfy_os_grp_0: H5G_STORAGE_TYPE_SYMBOL_TABLE != grp_info.storage_type";
        }
        else if (0 != grp_info.nlinks) {
            pass         = false;
            failure_mssg = "vrfy_os_grp_0: 0 != grp_info.nlinks";
        }
        else if (0 != grp_info.max_corder) {
            pass         = false;
            failure_mssg = "vrfy_os_grp_0: 0 != grp_info.max_corder";
        }
        else if (false != grp_info.mounted) {
            pass         = false;
            failure_mssg = "vrfy_os_grp_0: false != grp_info.mounted";
        }
        assert(ret >= 0);
        assert(H5G_STORAGE_TYPE_SYMBOL_TABLE == grp_info.storage_type);
        assert(0 == grp_info.nlinks);
        assert(0 == grp_info.max_corder);
        assert(false == grp_info.mounted);
    }

    if (pass) {
        ret = H5Gclose(gid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_os_grp_0: H5Gclose() failed.";
        }
        assert(ret >= 0);
    }

} /* vrfy_os_grp_0() */

/*-------------------------------------------------------------------------
 * Function:    os_grp_n
 *
 * Purpose:     Create an "old style" group, with 'nlinks' soft/hard
 *              links in it.
 *
 *              If pass is false on entry, do nothing.
 *
 *              If an error is detected, set pass to false, and set
 *              failure_mssg to point to an appropriate error message.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */

void
os_grp_n(hid_t fid, const char *group_name, int proc_num, unsigned nlinks)
{
    hid_t        gid = H5I_INVALID_HID;
    unsigned     u;
    hid_t        fapl = H5I_INVALID_HID;
    H5F_libver_t low = H5F_LIBVER_ERROR, high = H5F_LIBVER_ERROR;
    herr_t       ret;

    if (pass) { /* get the file's file access property list */
        fapl = H5Fget_access_plist(fid);
        if (fapl <= 0) {
            pass         = false;
            failure_mssg = "os_grp_n: H5Fget_access_plist() failed.";
        }
        assert(fapl > 0);
    }

    if (pass) { /* get low and high bounds from fapl */
        ret = H5Pget_libver_bounds(fapl, &low, &high);
        if (ret < 0) {
            pass         = false;
            failure_mssg = "os_grp_0: H5Pget_libver_bounds() failed(1).";
        }
        assert(ret >= 0);
    }

    if (pass) { /* turn file format latest off */
        if (low >= H5F_LIBVER_V18) {
            ret = H5Fset_libver_bounds(fid, H5F_LIBVER_EARLIEST, high);
            if (ret < 0) {
                pass         = false;
                failure_mssg = "os_grp_0: H5Fset_libver_bounds() failed(1).";
            }
            assert(ret >= 0);
        }
    }

    if (pass) {
        gid = H5Gcreate2(fid, group_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        if (gid <= 0) {
            pass         = false;
            failure_mssg = "os_grp_n: H5Gcreate2() failed.";
        }
        assert(gid > 0);
    }

    assert(nlinks > 0);

    u = 0;
    while ((pass) && (u < nlinks)) {
        char linkname[32];

        snprintf(linkname, sizeof(linkname), "ln%d_%u", proc_num, u);

        if (0 == (u % 2)) {
            ret = H5Lcreate_soft(group_name, gid, linkname, H5P_DEFAULT, H5P_DEFAULT);
            if (ret < 0) {
                pass         = false;
                failure_mssg = "os_grp_n: H5Lcreate_soft() failed.";
            }
            assert(ret >= 0);
        } /* end if */
        else {
            assert(1 == (u % 2));

            ret = H5Lcreate_hard(fid, "/", gid, linkname, H5P_DEFAULT, H5P_DEFAULT);
            if (ret < 0) {
                pass         = false;
                failure_mssg = "os_grp_n: H5Lcreate_hard() failed.";
            }
            assert(ret >= 0);
        } /* end else */

        u++;
    } /* end while */

    if (pass) {
        ret = H5Gclose(gid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "os_grp_n: H5Gclose() failed.";
        }
        assert(ret >= 0);
    }

    if (pass) { /* restore low and high bounds */
        if (low >= H5F_LIBVER_V18) {
            ret = H5Fset_libver_bounds(fid, low, high);
            if (ret < 0) {
                pass         = false;
                failure_mssg = "os_grp_n: H5Fset_libver_bounds() failed(2).";
            }
            assert(ret >= 0);
        }
    }

} /* os_grp_n() */

/*-------------------------------------------------------------------------
 * Function:    vrfy_os_grp_n
 *
 * Purpose:     Validate an "old style" group with 'nlinks' soft/hard
 *              links in it.
 *
 *              If pass is false on entry, do nothing.
 *
 *              If an error is detected, set pass to false, and set
 *              failure_mssg to point to an appropriate error message.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
void
vrfy_os_grp_n(hid_t fid, const char *group_name, int proc_num, unsigned nlinks)
{
    hid_t      gid  = H5I_INVALID_HID;
    hid_t      gcpl = H5I_INVALID_HID;
    H5G_info_t grp_info;
    unsigned   crt_order_flags = 0;
    unsigned   u;
    herr_t     ret;

    if (pass) {
        gid = H5Gopen2(fid, group_name, H5P_DEFAULT);

        if (gid <= 0) {
            pass         = false;
            failure_mssg = "vrfy_os_grp_n: H5Gopen2() failed";
        }
        assert(gid > 0);
    }

    if (pass) {
        gcpl = H5Gget_create_plist(gid);

        if (gcpl <= 0) {
            pass         = false;
            failure_mssg = "vrfy_os_grp_n: H5Gget_create_plist() failed";
        }
        assert(gcpl > 0);
    }

    if (pass) {
        ret = H5Pget_link_creation_order(gcpl, &crt_order_flags);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_os_grp_n: H5Pget_link_creation_order";
        }
        else if (0 != crt_order_flags) {
            pass         = false;
            failure_mssg = "vrfy_os_grp_n: 0 != crt_order_flags";
        }
        assert(ret >= 0);
        assert(0 == crt_order_flags);
    }

    if (pass) {
        ret = H5Pclose(gcpl);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_os_grp_n: H5Pclose() failed";
        }
        assert(ret >= 0);
    }

    if (pass) {
        memset(&grp_info, 0, sizeof(grp_info));

        ret = H5Gget_info(gid, &grp_info);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_os_grp_n: H5Gget_info() failed";
        }
        else if (H5G_STORAGE_TYPE_SYMBOL_TABLE != grp_info.storage_type) {
            pass         = false;
            failure_mssg = "vrfy_os_grp_n: H5G_STORAGE_TYPE_SYMBOL_TABLE != grp_info.storage_type";
        }
        else if (nlinks != grp_info.nlinks) {
            pass         = false;
            failure_mssg = "vrfy_os_grp_n: nlinks != grp_info.nlinks";
        }
        else if (0 != grp_info.max_corder) {
            pass         = false;
            failure_mssg = "vrfy_os_grp_n: 0 != grp_info.max_corder";
        }
        else if (false != grp_info.mounted) {
            pass         = false;
            failure_mssg = "vrfy_os_grp_n: false != grp_info.mounted";
        }
        assert(ret >= 0);
        assert(H5G_STORAGE_TYPE_SYMBOL_TABLE == grp_info.storage_type);
        assert(nlinks == grp_info.nlinks);
        assert(0 == grp_info.max_corder);
        assert(false == grp_info.mounted);
    }

    u = 0;
    while ((pass) && (u < nlinks)) {
        H5L_info2_t lnk_info;
        char        linkname[32];
        htri_t      link_exists;

        snprintf(linkname, sizeof(linkname), "ln%d_%u", proc_num, u);
        link_exists = H5Lexists(gid, linkname, H5P_DEFAULT);

        if (link_exists < 0) {
            pass         = false;
            failure_mssg = "vrfy_os_grp_n: H5Lexists() failed";
        }
        assert(link_exists >= 0);

        memset(&lnk_info, 0, sizeof(grp_info));
        ret = H5Lget_info2(gid, linkname, &lnk_info, H5P_DEFAULT);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_os_grp_n: H5Lget_info() failed";
        }
        else if (false != lnk_info.corder_valid) {
            pass         = false;
            failure_mssg = "vrfy_os_grp_n: false != lnk_info.corder_valid";
        }
        else if (H5T_CSET_ASCII != lnk_info.cset) {
            pass         = false;
            failure_mssg = "vrfy_os_grp_n: H5T_CSET_ASCII != lnk_info.cset";
        }
        assert(ret >= 0);
        assert(false == lnk_info.corder_valid);
        assert(H5T_CSET_ASCII == lnk_info.cset);

        if (0 == (u % 2)) {
            char *slinkval;

            if (H5L_TYPE_SOFT != lnk_info.type) {
                pass         = false;
                failure_mssg = "vrfy_os_grp_n: H5L_TYPE_SOFT != lnk_info.type";
            }
            else if ((strlen(group_name) + 1) != lnk_info.u.val_size) {
                pass         = false;
                failure_mssg = "vrfy_os_grp_n: (strlen(group_name) + 1) != lnk_info.u.val_size";
            }
            assert(H5L_TYPE_SOFT == lnk_info.type);
            assert((strlen(group_name) + 1) == lnk_info.u.val_size);

            slinkval = (char *)malloc(lnk_info.u.val_size);

            if (!slinkval) {
                pass         = false;
                failure_mssg = "vrfy_os_grp_n: malloc of slinkval failed";
            }
            assert(slinkval);

            ret = H5Lget_val(gid, linkname, slinkval, lnk_info.u.val_size, H5P_DEFAULT);

            if (ret < 0) {
                pass         = false;
                failure_mssg = "vrfy_os_grp_n: H5Lget_val() failed";
            }
            else if (0 != strcmp(slinkval, group_name)) {
                pass         = false;
                failure_mssg = "vrfy_os_grp_n: 0 != strcmp(slinkval, group_name)";
            }
            assert(ret >= 0);
            assert(0 == strcmp(slinkval, group_name));

            free(slinkval);
        } /* end if */
        else {
            H5O_info2_t root_oinfo;
            int         token_cmp = 0;

            assert(1 == (u % 2));

            if (H5L_TYPE_HARD != lnk_info.type) {
                pass         = false;
                failure_mssg = "vrfy_os_grp_n: H5L_TYPE_HARD != lnk_info.type";
            }
            assert(H5L_TYPE_HARD == lnk_info.type);

            memset(&root_oinfo, 0, sizeof(root_oinfo));
            ret = H5Oget_info3(fid, &root_oinfo, H5O_INFO_BASIC);

            if (ret < 0) {
                pass         = false;
                failure_mssg = "vrfy_os_grp_n: H5Oget_info() failed.";
            }
            else {
                if (H5Otoken_cmp(fid, &root_oinfo.token, &lnk_info.u.token, &token_cmp) < 0) {
                    pass         = false;
                    failure_mssg = "vrfy_os_grp_n: H5Otoken_cmp() failed.";
                }

                if (token_cmp) {
                    pass         = false;
                    failure_mssg = "vrfy_os_grp_n: root_oinfo.token != lnk_info.u.token";
                }
            }
            assert(ret >= 0);
            assert(!token_cmp);
        } /* end else */

        u++;
    } /* end while */

    if (pass) {
        ret = H5Gclose(gid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_os_grp_n: H5Gclose() failed.";
        }
        assert(ret >= 0);
    }

} /* vrfy_os_grp_n() */

/*-------------------------------------------------------------------------
 * Function:    ds_ctg_i
 *
 * Purpose:     Create a contiguous dataset w/int datatype.  Write data
 *              to the data set or not as indicated by the write_data
 *              parameter.
 *
 *              If pass is false on entry, do nothing.
 *
 *              If an error is detected, set pass to false, and set
 *              failure_mssg to point to an appropriate error message.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
void
ds_ctg_i(hid_t fid, const char *dset_name, bool write_data)
{
    int     *wdata = NULL;
    unsigned u;
    hid_t    dsid    = H5I_INVALID_HID;
    hid_t    sid     = H5I_INVALID_HID;
    hsize_t  dims[1] = {DSET_DIMS};
    herr_t   ret;

    if (pass) {
        sid = H5Screate_simple(1, dims, NULL);

        if (sid <= 0) {
            pass         = false;
            failure_mssg = "ds_ctg_i: H5Screate_simple() failed";
        }
        assert(sid > 0);
    }

    if (pass) {
        dsid = H5Dcreate2(fid, dset_name, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

        if (dsid <= 0) {
            pass         = false;
            failure_mssg = "ds_ctg_i: H5Dcreate2() failed";
        }
        assert(dsid > 0);
    }

    if (pass) {
        ret = H5Sclose(sid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "ds_ctg_i: H5Sclose() failed";
        }
        assert(ret >= 0);
    }

    if ((pass) && (write_data)) {
        wdata = (int *)malloc(sizeof(int) * DSET_DIMS);

        if (!wdata) {
            pass         = false;
            failure_mssg = "ds_ctg_i: malloc of wdata failed.";
        }
        assert(wdata);
    }

    if ((pass) && (write_data)) {
        for (u = 0; u < DSET_DIMS; u++)
            wdata[u] = (int)u;

        ret = H5Dwrite(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "ds_ctg_i: H5Dwrite() failed.";
        }
        assert(ret >= 0);
    }

    free(wdata);

    if (pass) {
        ret = H5Dclose(dsid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "ds_ctg_i: H5Dclose() failed";
        }
        assert(ret >= 0);
    }

} /* ds_ctg_i */

/*-------------------------------------------------------------------------
 * Function:    vrfy_ds_ctg_i
 *
 * Purpose:     Validate a contiguous datasets w/int datatypes. Validate
 *              data if indicated via the write_data parameter.
 *
 *              If pass is false on entry, do nothing.
 *
 *              If an error is detected, set pass to false, and set
 *              failure_mssg to point to an appropriate error message.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
void
vrfy_ds_ctg_i(hid_t fid, const char *dset_name, bool write_data)
{
    int               *rdata = NULL;
    unsigned           u;
    hid_t              dsid = H5I_INVALID_HID;
    hid_t              sid  = H5I_INVALID_HID;
    hid_t              tid  = H5I_INVALID_HID;
    hid_t              dcpl = H5I_INVALID_HID;
    H5D_space_status_t allocation;
    H5D_layout_t       layout;
    int                ndims;
    hsize_t            dims[1], max_dims[1];
    htri_t             type_equal;
    herr_t             ret;

    if (pass) {
        dsid = H5Dopen2(fid, dset_name, H5P_DEFAULT);

        if (dsid <= 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_i: H5Dopen2() failed.";
        }
        assert(dsid > 0);
    }

    if (pass) {
        sid = H5Dget_space(dsid);

        if (sid <= 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_i: H5Dget_space() failed.";
        }
        assert(sid > 0);
    }

    if (pass) {
        ndims = H5Sget_simple_extent_ndims(sid);

        if (1 != ndims) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_i: 1 != ndims";
        }
        assert(1 == ndims);
    }

    if (pass) {
        ret = H5Sget_simple_extent_dims(sid, dims, max_dims);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_i: H5Sget_simple_extent_dims() failed";
        }
        else if (DSET_DIMS != dims[0]) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_i: DSET_DIMS != dims[0]";
        }
        else if (DSET_DIMS != max_dims[0]) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_i: DSET_DIMS != max_dims[0]";
        }
        assert(ret >= 0);
        assert(DSET_DIMS == dims[0]);
        assert(DSET_DIMS == max_dims[0]);
    }

    if (pass) {
        ret = H5Sclose(sid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_i: H5Sclose() failed.";
        }
        assert(ret >= 0);
    }

    if (pass) {
        tid = H5Dget_type(dsid);

        if (tid <= 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_i: H5Dget_type() failed.";
        }
        assert(tid > 0);
    }

    if (pass) {
        type_equal = H5Tequal(tid, H5T_NATIVE_INT);

        if (1 != type_equal) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_i: type not H5T_NATIVE_INT";
        }
        assert(1 == type_equal);
    }

    if (pass) {
        ret = H5Tclose(tid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_i: H5Tclose() failed.";
        }
        assert(ret >= 0);
    }

    if (pass) {
        ret = H5Dget_space_status(dsid, &allocation);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_i: H5Dget_space_status() failed.";
        }
        else if (write_data && (allocation != H5D_SPACE_STATUS_ALLOCATED)) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_i: write_data && allocation != H5D_SPACE_STATUS_ALLOCATED";
        }
        else if (!write_data && (allocation != H5D_SPACE_STATUS_NOT_ALLOCATED)) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_i: !write_data && allocation != H5D_SPACE_STATUS_NOT_ALLOCATED";
        }
        assert(ret >= 0);
        assert((write_data && allocation == H5D_SPACE_STATUS_ALLOCATED) ||
               (!write_data && allocation == H5D_SPACE_STATUS_NOT_ALLOCATED));
    }

    if (pass) {
        dcpl = H5Dget_create_plist(dsid);

        if (dcpl <= 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_i: H5Dget_create_plist() failed.";
        }
        assert(dcpl > 0);
    }

    if (pass) {
        layout = H5Pget_layout(dcpl);

        if (H5D_CONTIGUOUS != layout) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_i: H5D_CONTIGUOUS != layout";
        }
        assert(H5D_CONTIGUOUS == layout);
    }

    if (pass) {
        ret = H5Pclose(dcpl);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_i: H5Pclose() failed.";
        }
        assert(ret >= 0);
    }

    if ((pass) && (write_data)) {
        rdata = (int *)malloc(sizeof(int) * DSET_DIMS);

        if (!rdata) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_i: malloc of rdata failed.";
        }
        assert(rdata);
    }

    if ((pass) && (write_data)) {
        ret = H5Dread(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_i: H5Dread() failed.";
        }
        assert(ret >= 0);
    }

    if ((pass) && (write_data)) {
        for (u = 0; u < DSET_DIMS; u++) {
            if ((int)u != rdata[u]) {
                pass         = false;
                failure_mssg = "vrfy_ds_ctg_i: u != rdata[u].";
                break;
            }
            assert((int)u == rdata[u]);
        }
    } /* end if */

    free(rdata);

    if (pass) {
        ret = H5Dclose(dsid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_i: H5Dclose() failed";
        }
        assert(ret >= 0);
    }

} /* vrfy_ds_ctg_i() */

/*-------------------------------------------------------------------------
 * Function:    ds_chk_i
 *
 * Purpose:     Create a chunked dataset w/int datatype.  Write data
 *              to the data set or not as indicated by the write_data
 *              parameter.
 *
 *              If pass is false on entry, do nothing.
 *
 *              If an error is detected, set pass to false, and set
 *              failure_mssg to point to an appropriate error message.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
void
ds_chk_i(hid_t fid, const char *dset_name, bool write_data)
{
    int     *wdata = NULL;
    unsigned u;
    hid_t    dsid          = H5I_INVALID_HID;
    hid_t    dcpl          = H5I_INVALID_HID;
    hid_t    sid           = H5I_INVALID_HID;
    hsize_t  dims[1]       = {DSET_DIMS};
    hsize_t  chunk_dims[1] = {DSET_CHUNK_DIMS};
    herr_t   ret;

    if (pass) {
        sid = H5Screate_simple(1, dims, NULL);

        if (sid <= 0) {
            pass         = false;
            failure_mssg = "ds_chk_i: H5Screate_simple() failed.";
        }
        assert(sid > 0);
    }

    if (pass) {
        dcpl = H5Pcreate(H5P_DATASET_CREATE);

        if (dcpl <= 0) {
            pass         = false;
            failure_mssg = "ds_chk_i: H5Pcreate() failed.";
        }
        assert(dcpl > 0);
    }

    if (pass) {
        ret = H5Pset_chunk(dcpl, 1, chunk_dims);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "ds_chk_i: H5Pset_chunk() failed.";
        }
        assert(ret >= 0);
    }

    if (pass) {
        dsid = H5Dcreate2(fid, dset_name, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);

        if (dsid <= 0) {
            pass         = false;
            failure_mssg = "ds_chk_i: H5Dcreate2() failed";
        }
        assert(dsid > 0);
    }

    if (pass) {
        ret = H5Pclose(dcpl);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "ds_chk_i: H5Pclose() failed.";
        }
        assert(ret >= 0);
    }

    if (pass) {
        ret = H5Sclose(sid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "ds_chk_i: H5Sclose() failed.";
        }
        assert(ret >= 0);
    }

    if ((pass) && (write_data)) {
        wdata = (int *)malloc(sizeof(int) * DSET_DIMS);

        if (!wdata) {
            pass         = false;
            failure_mssg = "ds_chk_i: malloc of wdata failed.";
        }
        assert(wdata);
    }

    if ((pass) && (write_data)) {
        for (u = 0; u < DSET_DIMS; u++)
            wdata[u] = (int)u;

        ret = H5Dwrite(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
        if (ret < 0) {
            pass         = false;
            failure_mssg = "ds_chk_i: H5Dwrite() failed.";
        }
        assert(ret >= 0);
        free(wdata);
    } /* end if */

    if (pass) {
        ret = H5Dclose(dsid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "ds_chk_i: H5Dclose() failed.";
        }
        assert(ret >= 0);
    }

} /* ds_chk_i */

/*-------------------------------------------------------------------------
 * Function:    vrfy_ds_chk_i
 *
 * Purpose:     Validate a chunked datasets w/int datatypes. Validate
 *              data if indicated via the write_data parameter.
 *
 *              If pass is false on entry, do nothing.
 *
 *              If an error is detected, set pass to false, and set
 *              failure_mssg to point to an appropriate error message.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
void
vrfy_ds_chk_i(hid_t fid, const char *dset_name, bool write_data)
{
    int               *rdata = NULL;
    unsigned           u;
    hid_t              dsid = H5I_INVALID_HID;
    hid_t              sid  = H5I_INVALID_HID;
    hid_t              tid  = H5I_INVALID_HID;
    hid_t              dcpl = H5I_INVALID_HID;
    H5D_space_status_t allocation;
    H5D_layout_t       layout;
    int                ndims;
    hsize_t            dims[1], max_dims[1], chunk_dims[1];
    htri_t             type_equal;
    herr_t             ret;

    if (pass) {
        dsid = H5Dopen2(fid, dset_name, H5P_DEFAULT);

        if (dsid <= 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_chk_i: H5Dopen2() failed.";
        }
        assert(dsid > 0);
    }

    if (pass) {
        sid = H5Dget_space(dsid);

        if (sid <= 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_chk_i: H5Dget_space() failed.";
        }
        assert(sid > 0);
    }

    if (pass) {
        ndims = H5Sget_simple_extent_ndims(sid);

        if (1 != ndims) {
            pass         = false;
            failure_mssg = "vrfy_ds_chk_i: 1 != ndims";
        }
        assert(1 == ndims);
    }

    if (pass) {
        ret = H5Sget_simple_extent_dims(sid, dims, max_dims);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_chk_i: H5Sget_simple_extent_dims() failed";
        }
        else if (DSET_DIMS != dims[0]) {
            pass         = false;
            failure_mssg = "vrfy_ds_chk_i: DSET_DIMS != dims[0]";
        }
        else if (DSET_DIMS != max_dims[0]) {
            pass         = false;
            failure_mssg = "vrfy_ds_chk_i: DSET_DIMS != max_dims[0]";
        }
        assert(ret >= 0);
        assert(DSET_DIMS == dims[0]);
        assert(DSET_DIMS == max_dims[0]);
    }

    if (pass) {
        ret = H5Sclose(sid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_chk_i: H5Sclose() failed.";
        }
        assert(ret >= 0);
    }

    if (pass) {
        tid = H5Dget_type(dsid);

        if (tid <= 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_chk_i: H5Dget_type() failed.";
        }
        assert(tid > 0);
    }

    if (pass) {
        type_equal = H5Tequal(tid, H5T_NATIVE_INT);

        if (1 != type_equal) {
            pass         = false;
            failure_mssg = "vrfy_ds_chk_i: tid != H5T_NATIVE_INT";
        }
        assert(1 == type_equal);
    }

    if (pass) {
        ret = H5Tclose(tid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_chk_i: H5Tclose() failed.";
        }
        assert(ret >= 0);
    }

    if (pass) {
        ret = H5Dget_space_status(dsid, &allocation);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_chk_i: H5Dget_space_status() failed.";
        }
        else if (write_data && (allocation != H5D_SPACE_STATUS_ALLOCATED)) {
            pass         = false;
            failure_mssg = "vrfy_ds_chk_i: write_data && allocation != H5D_SPACE_STATUS_ALLOCATED";
        }
        else if (!write_data && (allocation != H5D_SPACE_STATUS_NOT_ALLOCATED)) {
            pass         = false;
            failure_mssg = "vrfy_ds_chk_i: !write_data && allocation != H5D_SPACE_STATUS_NOT_ALLOCATED";
        }
        assert(ret >= 0);
        assert((write_data && allocation == H5D_SPACE_STATUS_ALLOCATED) ||
               (!write_data && allocation == H5D_SPACE_STATUS_NOT_ALLOCATED));
    }

    if (pass) {
        dcpl = H5Dget_create_plist(dsid);

        if (dcpl <= 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_chk_i: H5Dget_create_plist() failed.";
        }
        assert(dcpl > 0);
    }

    if (pass) {
        layout = H5Pget_layout(dcpl);

        if (H5D_CHUNKED != layout) {
            pass         = false;
            failure_mssg = "vrfy_ds_chk_i: H5D_CHUNKED != layout";
        }
        assert(H5D_CHUNKED == layout);
    }

    if (pass) {
        ret = H5Pget_chunk(dcpl, 1, chunk_dims);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_chk_i: H5Pget_chunk";
        }
        else if (DSET_CHUNK_DIMS != chunk_dims[0]) {
            pass         = false;
            failure_mssg = "vrfy_ds_chk_i: ";
        }
        assert(ret >= 0);
        assert(DSET_CHUNK_DIMS == chunk_dims[0]);
    }

    if (pass) {
        ret = H5Pclose(dcpl);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_chk_i: H5Pclose() failed.";
        }
        assert(ret >= 0);
    }

    if ((pass) && (write_data)) {
        rdata = (int *)malloc(sizeof(int) * DSET_DIMS);

        if (!rdata) {
            pass         = false;
            failure_mssg = "vrfy_ds_chk_i: malloc of rdata failed.";
        }
        assert(rdata);
    }

    if ((pass) && (write_data)) {
        ret = H5Dread(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_chk_i: H5Dread() failed.";
        }
        assert(ret >= 0);
    }

    if ((pass) && (write_data)) {
        for (u = 0; u < DSET_DIMS; u++) {
            if ((int)u != rdata[u]) {
                pass         = false;
                failure_mssg = "vrfy_ds_chk_i: u != rdata[u]";
                break;
            }
            assert((int)u == rdata[u]);
        }
    } /* end if */

    free(rdata);

    if (pass) {
        ret = H5Dclose(dsid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_chk_i: H5Dclose() failed.";
        }
        assert(ret >= 0);
    }

} /* vrfy_ds_chk_i() */

/*-------------------------------------------------------------------------
 * Function:    ds_cpt_i
 *
 * Purpose:     Create a compact dataset w/int datatype.  Write data
 *              to the data set or not as indicated by the write_data
 *              parameter.
 *
 *              If pass is false on entry, do nothing.
 *
 *              If an error is detected, set pass to false, and set
 *              failure_mssg to point to an appropriate error message.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
void
ds_cpt_i(hid_t fid, const char *dset_name, bool write_data)
{
    int     *wdata = NULL;
    unsigned u;
    hid_t    dsid    = H5I_INVALID_HID;
    hid_t    dcpl    = H5I_INVALID_HID;
    hid_t    sid     = H5I_INVALID_HID;
    hsize_t  dims[1] = {DSET_COMPACT_DIMS};
    herr_t   ret;

    if (pass) {
        sid = H5Screate_simple(1, dims, NULL);

        if (sid <= 0) {
            pass         = false;
            failure_mssg = "ds_cpt_i: H5Screate_simple() failed.";
        }
        assert(sid > 0);
    }

    if (pass) {
        dcpl = H5Pcreate(H5P_DATASET_CREATE);

        if (dcpl <= 0) {
            pass         = false;
            failure_mssg = "ds_cpt_i: H5Pcreate() failed.";
        }
        assert(dcpl > 0);
    }

    if (pass) {
        ret = H5Pset_layout(dcpl, H5D_COMPACT);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "ds_cpt_i: H5Pset_layout() failed.";
        }
        assert(ret >= 0);
    }

    if (pass) {
        dsid = H5Dcreate2(fid, dset_name, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);

        if (dsid <= 0) {
            pass         = false;
            failure_mssg = "ds_cpt_i: H5Dcreate2() failed.";
        }
        assert(dsid > 0);
    }

    if (pass) {
        ret = H5Pclose(dcpl);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "ds_cpt_i: H5Pclose() failed.";
        }
        assert(ret >= 0);
    }

    if (pass) {
        ret = H5Sclose(sid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "ds_cpt_i: H5Sclose() failed.";
        }
        assert(ret >= 0);
    }

    if ((pass) && (write_data)) {
        wdata = (int *)malloc(sizeof(int) * DSET_COMPACT_DIMS);

        if (!wdata) {
            pass         = false;
            failure_mssg = "ds_cpt_i: malloc of wdata failed.";
        }
        assert(wdata);
    }

    if ((pass) && (write_data)) {
        for (u = 0; u < DSET_COMPACT_DIMS; u++)
            wdata[u] = (int)u;

        ret = H5Dwrite(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "ds_cpt_i: H5Dwrite() failed.";
        }
        assert(ret >= 0);

        free(wdata);
    } /* end if */

    if (pass) {
        ret = H5Dclose(dsid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "ds_cpt_i: H5Dclose() failed.";
        }
        assert(ret >= 0);
    }

} /* ds_cpt_i() */

/*-------------------------------------------------------------------------
 * Function:    vrfy_ds_cpt_i
 *
 * Purpose:     Validate a compact datasets w/int datatypes. Validate
 *              data if indicated via the write_data parameter.
 *
 *              If pass is false on entry, do nothing.
 *
 *              If an error is detected, set pass to false, and set
 *              failure_mssg to point to an appropriate error message.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
void
vrfy_ds_cpt_i(hid_t fid, const char *dset_name, bool write_data)
{
    int               *rdata = NULL;
    unsigned           u;
    hid_t              dsid = H5I_INVALID_HID;
    hid_t              sid  = H5I_INVALID_HID;
    hid_t              tid  = H5I_INVALID_HID;
    hid_t              dcpl = H5I_INVALID_HID;
    H5D_space_status_t allocation;
    H5D_layout_t       layout;
    int                ndims;
    hsize_t            dims[1], max_dims[1];
    htri_t             type_equal;
    herr_t             ret;

    if (pass) {
        dsid = H5Dopen2(fid, dset_name, H5P_DEFAULT);

        if (dsid <= 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_cpt_i: H5Dopen2() failed.";
        }
        assert(dsid > 0);
    }

    if (pass) {
        sid = H5Dget_space(dsid);

        if (sid <= 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_cpt_i: H5Dget_space() failed.";
        }
        assert(sid > 0);
    }

    if (pass) {
        ndims = H5Sget_simple_extent_ndims(sid);

        if (1 != ndims) {
            pass         = false;
            failure_mssg = "vrfy_ds_cpt_i: 1 != ndims";
        }
        assert(1 == ndims);
    }

    if (pass) {
        ret = H5Sget_simple_extent_dims(sid, dims, max_dims);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_cpt_i: H5Sget_simple_extent_dims() failed";
        }
        else if (DSET_COMPACT_DIMS != dims[0]) {
            pass         = false;
            failure_mssg = "vrfy_ds_cpt_i: DSET_COMPACT_DIMS != dims[0]";
        }
        else if (DSET_COMPACT_DIMS != max_dims[0]) {
            pass         = false;
            failure_mssg = "vrfy_ds_cpt_i: DSET_COMPACT_DIMS != max_dims[0]";
        }
        assert(ret >= 0);
        assert(DSET_COMPACT_DIMS == dims[0]);
        assert(DSET_COMPACT_DIMS == max_dims[0]);
    }

    if (pass) {
        ret = H5Sclose(sid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_cpt_i: H5Sclose() failed.";
        }
        assert(ret >= 0);
    }

    if (pass) {
        tid = H5Dget_type(dsid);

        if (tid <= 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_cpt_i: H5Dget_type() failed.";
        }
        assert(tid > 0);
    }

    if (pass) {
        type_equal = H5Tequal(tid, H5T_NATIVE_INT);

        if (1 != type_equal) {
            pass         = false;
            failure_mssg = "vrfy_ds_cpt_i: type != H5T_NATIVE_INT";
        }
        assert(1 == type_equal);
    }

    if (pass) {
        ret = H5Tclose(tid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_cpt_i: H5Tclose() failed.";
        }
        assert(ret >= 0);
    }

    if (pass) {
        ret = H5Dget_space_status(dsid, &allocation);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_cpt_i: H5Dget_space_status() failed.";
        }
        else if (H5D_SPACE_STATUS_ALLOCATED != allocation) {
            pass         = false;
            failure_mssg = "vrfy_ds_cpt_i: H5D_SPACE_STATUS_ALLOCATED != allocation";
        }
        assert(ret >= 0);
        assert(H5D_SPACE_STATUS_ALLOCATED == allocation);
    }

    if (pass) {
        dcpl = H5Dget_create_plist(dsid);

        if (dcpl <= 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_cpt_i: H5Dget_create_plist() failed.";
        }
        assert(dcpl > 0);
    }

    if (pass) {
        layout = H5Pget_layout(dcpl);

        if (H5D_COMPACT != layout) {
            pass         = false;
            failure_mssg = "vrfy_ds_cpt_i: H5D_COMPACT != layout";
        }
        assert(H5D_COMPACT == layout);
    }

    if (pass) {
        ret = H5Pclose(dcpl);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_cpt_i: H5Pclose() failed.";
        }
        assert(ret >= 0);
    }

    if ((pass) && (write_data)) {
        rdata = (int *)malloc(sizeof(int) * DSET_COMPACT_DIMS);

        if (!rdata) {
            pass         = false;
            failure_mssg = "vrfy_ds_cpt_i: malloc of rdata failed.";
        }
        assert(rdata);
    }

    if ((pass) && (write_data)) {
        ret = H5Dread(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_cpt_i: H5Dread() failed.";
        }
        assert(ret >= 0);
    }

    if ((pass) && (write_data)) {
        for (u = 0; u < DSET_COMPACT_DIMS; u++) {
            if ((int)u != rdata[u]) {
                pass         = false;
                failure_mssg = "vrfy_ds_cpt_i: (int)u != rdata[u]";
                break;
            }
            assert((int)u == rdata[u]);
        }
    } /* end if */

    free(rdata);

    if (pass) {
        ret = H5Dclose(dsid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_cpt_i: H5Dclose() failed.";
        }
        assert(ret >= 0);
    }

} /* vrfy_ds_cpt_i() */

/*-------------------------------------------------------------------------
 * Function:    ds_ctg_v
 *
 * Purpose:     Create a contiguous dataset w/variable-length datatype.
 *              Write data to the data set or not as indicated by the
 *              write_data parameter.
 *
 *              If pass is false on entry, do nothing.
 *
 *              If an error is detected, set pass to false, and set
 *              failure_mssg to point to an appropriate error message.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
void
ds_ctg_v(hid_t fid, const char *dset_name, bool write_data)
{
    hid_t    dsid    = H5I_INVALID_HID;
    hid_t    sid     = H5I_INVALID_HID;
    hid_t    tid     = H5I_INVALID_HID;
    hsize_t  dims[1] = {DSET_SMALL_DIMS};
    herr_t   ret;
    hvl_t   *wdata = NULL;
    unsigned u;

    if (pass) {
        sid = H5Screate_simple(1, dims, NULL);

        if (sid <= 0) {
            pass         = false;
            failure_mssg = "ds_ctg_v: H5Screate_simple";
        }
        assert(sid > 0);
    }

    if (pass) {
        tid = H5Tvlen_create(H5T_NATIVE_INT);

        if (tid <= 0) {
            pass         = false;
            failure_mssg = "ds_ctg_v: H5Tvlen_create() failed.";
        }
        assert(tid > 0);
    }

    if (pass) {
        dsid = H5Dcreate2(fid, dset_name, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

        if (dsid <= 0) {
            pass         = false;
            failure_mssg = "ds_ctg_v: H5Dcreate2() failed.";
        }
        assert(dsid > 0);
    }

    if ((pass) && (write_data)) {
        wdata = (hvl_t *)malloc(sizeof(hvl_t) * DSET_SMALL_DIMS);

        if (!wdata) {
            pass         = false;
            failure_mssg = "ds_ctg_v: malloc of wdata failed.";
        }
        assert(wdata);
    }

    if ((pass) && (write_data)) {
        for (u = 0; u < DSET_SMALL_DIMS; u++) {
            int     *tdata;
            unsigned len;
            unsigned v;

            len   = (u % 10) + 1;
            tdata = (int *)malloc(sizeof(int) * len);

            if (!tdata) {
                pass         = false;
                failure_mssg = "ds_ctg_v: malloc of tdata failed.";
                break;
            }
            assert(tdata);

            for (v = 0; v < len; v++)
                tdata[v] = (int)(u + v);

            wdata[u].len = len;
            wdata[u].p   = tdata;
        } /* end for */
    }

    if ((pass) && (write_data)) {
        ret = H5Dwrite(dsid, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "ds_ctg_v: H5Dwrite() failed.";
        }
        assert(ret >= 0);
    }

    if ((pass) && (write_data)) {
        ret = H5Treclaim(tid, sid, H5P_DEFAULT, wdata);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "ds_ctg_v: H5Treclaim() failed.";
        }
        assert(ret >= 0);

        free(wdata);
    } /* end if */

    if (pass) {
        ret = H5Sclose(sid);

        if (sid < 0) {
            pass         = false;
            failure_mssg = "ds_ctg_v: H5Sclose() failed.";
        }
        assert(ret >= 0);
    }

    if (pass) {
        ret = H5Tclose(tid);

        if (tid < 0) {
            pass         = false;
            failure_mssg = "ds_ctg_v: H5Tclose() failed.";
        }
        assert(ret >= 0);
    }

    if (pass) {
        ret = H5Dclose(dsid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "ds_ctg_v: H5Dclose() failed.";
        }
        assert(ret >= 0);
    }

} /* ds_ctg_v() */

/*-------------------------------------------------------------------------
 * Function:    vrfy_ds_ctg_v
 *
 * Purpose:     Validate a contiguous datasets w/variable-length datatypes.
 *              Validate data if indicated via the write_data parameter.
 *
 *              If pass is false on entry, do nothing.
 *
 *              If an error is detected, set pass to false, and set
 *              failure_mssg to point to an appropriate error message.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
void
vrfy_ds_ctg_v(hid_t fid, const char *dset_name, bool write_data)
{
    hid_t              dsid    = H5I_INVALID_HID;
    hid_t              sid     = H5I_INVALID_HID;
    hid_t              tid     = H5I_INVALID_HID;
    hid_t              tmp_tid = H5I_INVALID_HID;
    hid_t              dcpl    = H5I_INVALID_HID;
    H5D_space_status_t allocation;
    H5D_layout_t       layout;
    int                ndims;
    hsize_t            dims[1], max_dims[1];
    htri_t             type_equal;
    hvl_t             *rdata = NULL;
    unsigned           u;
    herr_t             ret;

    if (pass) {
        dsid = H5Dopen2(fid, dset_name, H5P_DEFAULT);

        if (dsid <= 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_v: H5Dopen2() failed.";
        }
        assert(dsid > 0);
    }

    if (pass) {
        sid = H5Dget_space(dsid);

        if (sid <= 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_v: H5Dget_space() failed";
        }
        assert(sid > 0);
    }

    if (pass) {
        ndims = H5Sget_simple_extent_ndims(sid);

        if (1 != ndims) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_v: 1 != ndims";
        }
        assert(1 == ndims);
    }

    if (pass) {
        ret = H5Sget_simple_extent_dims(sid, dims, max_dims);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_v: H5Sget_simple_extent_dims() failed.";
        }
        else if (DSET_SMALL_DIMS != dims[0]) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_v: DSET_SMALL_DIMS != dims[0]";
        }
        else if (DSET_SMALL_DIMS != max_dims[0]) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_v: DSET_SMALL_DIMS != max_dims[0]";
        }
        assert(ret >= 0);
        assert(DSET_SMALL_DIMS == dims[0]);
        assert(DSET_SMALL_DIMS == max_dims[0]);
    }

    if (pass) {
        tid = H5Dget_type(dsid);

        if (tid <= 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_v: H5Dget_type() failed.";
        }
        assert(tid > 0);
    }

    if (pass) {
        tmp_tid = H5Tvlen_create(H5T_NATIVE_INT);

        if (tmp_tid <= 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_v: H5Tvlen_create() failed.";
        }
        assert(tmp_tid > 0);
    }

    if (pass) {
        type_equal = H5Tequal(tid, tmp_tid);

        if (1 != type_equal) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_v: type != vlen H5T_NATIVE_INT";
        }
        assert(1 == type_equal);
    }

    if (pass) {
        ret = H5Tclose(tmp_tid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_v: H5Tclose() failed.";
        }
        assert(ret >= 0);
    }

    if (pass) {
        ret = H5Dget_space_status(dsid, &allocation);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_v: H5Dget_space_status() failed";
        }
        else if (write_data && (allocation != H5D_SPACE_STATUS_ALLOCATED)) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_v: write_data && allocation != H5D_SPACE_STATUS_ALLOCATED";
        }
        else if (!write_data && (allocation != H5D_SPACE_STATUS_NOT_ALLOCATED)) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_v: !write_data && allocation != H5D_SPACE_STATUS_NOT_ALLOCATED";
        }
        assert(ret >= 0);
        assert((write_data && allocation == H5D_SPACE_STATUS_ALLOCATED) ||
               (!write_data && allocation == H5D_SPACE_STATUS_NOT_ALLOCATED));
    }

    if (pass) {
        dcpl = H5Dget_create_plist(dsid);

        if (dcpl <= 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_v: H5Dget_create_plist() failed.";
        }
        assert(dcpl > 0);
    }

    if (pass) {
        layout = H5Pget_layout(dcpl);

        if (H5D_CONTIGUOUS != layout) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_v: H5D_CONTIGUOUS != layout";
        }
        assert(H5D_CONTIGUOUS == layout);
    }

    if (pass) {
        ret = H5Pclose(dcpl);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_v: H5Pclose() failed.";
        }
        assert(ret >= 0);
    }

    if ((pass) && (write_data)) {
        rdata = (hvl_t *)malloc(sizeof(hvl_t) * DSET_SMALL_DIMS);

        if (!rdata) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_v: malloc of rdata failed.";
        }
        assert(rdata);
    }

    if ((pass) && (write_data)) {
        ret = H5Dread(dsid, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_v: H5Dread() failed.";
        }
        assert(ret >= 0);
    }

    if ((pass) && (write_data)) {
        for (u = 0; u < DSET_SMALL_DIMS; u++) {
            unsigned len;
            unsigned v;

            len = (unsigned)rdata[u].len;
            for (v = 0; v < len; v++) {
                int *tdata = (int *)rdata[u].p;

                if (!tdata) {
                    pass         = false;
                    failure_mssg = "vrfy_ds_ctg_v: !tdata";
                    break;
                }
                else if ((int)(u + v) != tdata[v]) {
                    pass         = false;
                    failure_mssg = "vrfy_ds_ctg_v: (int)(u + v) != tdata[v]";
                    break;
                }
                assert(tdata);
                assert((int)(u + v) == tdata[v]);
            } /* end for */
        }     /* end for */
    }

    if ((pass) && (write_data)) {
        ret = H5Treclaim(tid, sid, H5P_DEFAULT, rdata);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_v: H5Treclaim() failed.";
        }
        assert(ret >= 0);
    } /* end if */

    free(rdata);

    if (pass) {
        ret = H5Sclose(sid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_v: H5Sclose() failed.";
        }
        assert(ret >= 0);
    }

    if (pass) {
        ret = H5Tclose(tid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_v: H5Tclose() failed.";
        }
        assert(ret >= 0);
    }

    if (pass) {
        ret = H5Dclose(dsid);

        if (ret < 0) {
            pass         = false;
            failure_mssg = "vrfy_ds_ctg_v: H5Dclose() failed.";
        }
        assert(ret >= 0);
    }

} /* vrfy_ds_ctg_v() */

/*-------------------------------------------------------------------------
 * Function:    create_zoo
 *
 * Purpose:     Given the path to a group, construct a variety of HDF5
 *              data sets, groups, and other objects selected so as to
 *              include instances of all on disk data structures used
 *              in the HDF5 library.
 *
 *              If pass is false on entry, do nothing.
 *
 *              If an error is detected, set pass to false, and set
 *              failure_mssg to point to an appropriate error message.
 *
 *              This function was initially created to assist in testing
 *              the cache image feature of the metadata cache.  Thus, it
 *              only concerns itself with the version 2 superblock, and
 *              on disk structures that can occur with this version of
 *              the superblock.
 *
 *              Note the associated validate_zoo() function.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */

void
create_zoo(hid_t fid, const char *base_path, int proc_num)
{
    char full_path[1024];

    assert(base_path);

    /* Add & verify an empty "new style" group */
    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/A", base_path);
        assert(strlen(full_path) < 1024);
        ns_grp_0(fid, full_path);
    }

    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/A", base_path);
        assert(strlen(full_path) < 1024);
        vrfy_ns_grp_0(fid, full_path);
    }

    /* Add & verify a compact "new style" group (3 link messages) */
    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/B", base_path);
        assert(strlen(full_path) < 1024);
        ns_grp_c(fid, full_path, 3);
    }

    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/B", base_path);
        assert(strlen(full_path) < 1024);
        vrfy_ns_grp_c(fid, full_path, 3);
    }

    /* Add & verify a dense "new style" group (w/300 links, in v2 B-tree &
     * fractal heap)
     */
    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/C", base_path);
        assert(strlen(full_path) < 1024);
        ns_grp_d(fid, full_path, 300);
    }

    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/C", base_path);
        assert(strlen(full_path) < 1024);
        vrfy_ns_grp_d(fid, full_path, 300);
    }

    /* Add & verify an empty "old style" group to file */
    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/D", base_path);
        assert(strlen(full_path) < 1024);
        os_grp_0(fid, full_path);
    }

    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/D", base_path);
        assert(strlen(full_path) < 1024);
        vrfy_os_grp_0(fid, full_path);
    }

    /* Add & verify an "old style" group (w/300 links, in v1 B-tree &
     * local heap) to file
     */
    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/E", base_path);
        assert(strlen(full_path) < 1024);
        os_grp_n(fid, full_path, proc_num, 300);
    }

    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/E", base_path);
        assert(strlen(full_path) < 1024);
        vrfy_os_grp_n(fid, full_path, proc_num, 300);
    }

    /* Add & verify a contiguous dataset w/integer datatype (but no data)
     * to file
     */
    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/F", base_path);
        assert(strlen(full_path) < 1024);
        ds_ctg_i(fid, full_path, false);
    }

    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/F", base_path);
        assert(strlen(full_path) < 1024);
        vrfy_ds_ctg_i(fid, full_path, false);
    }

    /* Add & verify a contiguous dataset w/integer datatype (with data)
     * to file
     */
    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/G", base_path);
        assert(strlen(full_path) < 1024);
        ds_ctg_i(fid, full_path, true);
    }

    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/G", base_path);
        assert(strlen(full_path) < 1024);
        vrfy_ds_ctg_i(fid, full_path, true);
    }

    /* Add & verify a chunked dataset w/integer datatype (but no data)
     * to file
     */
    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/H", base_path);
        assert(strlen(full_path) < 1024);
        ds_chk_i(fid, full_path, false);
    }

    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/H", base_path);
        assert(strlen(full_path) < 1024);
        vrfy_ds_chk_i(fid, full_path, false);
    }

    /* Add & verify a chunked dataset w/integer datatype (and data)
     * to file
     */
    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/I", base_path);
        assert(strlen(full_path) < 1024);
        ds_chk_i(fid, full_path, true);
    }

    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/I", base_path);
        assert(strlen(full_path) < 1024);
        vrfy_ds_chk_i(fid, full_path, true);
    }

    /* Add & verify a compact dataset w/integer datatype (but no data)
     * to file
     */
    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/J", base_path);
        assert(strlen(full_path) < 1024);
        ds_cpt_i(fid, full_path, false);
    }

    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/J", base_path);
        assert(strlen(full_path) < 1024);
        vrfy_ds_cpt_i(fid, full_path, false);
    }

    /* Add & verify a compact dataset w/integer datatype (and data)
     * to file
     */
    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/K", base_path);
        assert(strlen(full_path) < 1024);
        ds_cpt_i(fid, full_path, true);
    }

    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/K", base_path);
        assert(strlen(full_path) < 1024);
        vrfy_ds_cpt_i(fid, full_path, true);
    }

    /* Add & verify a contiguous dataset w/variable-length datatype
     * (but no data) to file
     */
    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/L", base_path);
        assert(strlen(full_path) < 1024);
        ds_ctg_v(fid, full_path, false);
    }

    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/L", base_path);
        assert(strlen(full_path) < 1024);
        vrfy_ds_ctg_v(fid, full_path, false);
    }

    /* Add & verify a contiguous dataset w/variable-length datatype
     * (and data) to file
     */
    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/M", base_path);
        assert(strlen(full_path) < 1024);
        ds_ctg_v(fid, full_path, true);
    }

    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/M", base_path);
        assert(strlen(full_path) < 1024);
        vrfy_ds_ctg_v(fid, full_path, true);
    }

} /* create_zoo() */

/*-------------------------------------------------------------------------
 * Function:    validate_zoo
 *
 * Purpose:     Given the path to a group in which a "zoo" has been
 *              constructed, validate the objects in the "zoo".
 *
 *              If pass is false on entry, do nothing.
 *
 *              If an error is detected, set pass to false, and set
 *              failure_mssg to point to an appropriate error message.
 *
 *              This function was initially created to assist in testing
 *              the cache image feature of the metadata cache.  Thus, it
 *              only concerns itself with the version 2 superblock, and
 *              on disk structures that can occur with this version of
 *              the superblock.
 *
 *              Note the associated validate_zoo() function.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */

void
validate_zoo(hid_t fid, const char *base_path, int proc_num)
{
    char full_path[1024];

    assert(base_path);

    /* validate an empty "new style" group */
    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/A", base_path);
        assert(strlen(full_path) < 1024);
        vrfy_ns_grp_0(fid, full_path);
    }

    /* validate a compact "new style" group (3 link messages) */
    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/B", base_path);
        assert(strlen(full_path) < 1024);
        vrfy_ns_grp_c(fid, full_path, 3);
    }

    /* validate a dense "new style" group (w/300 links, in v2 B-tree &
     * fractal heap)
     */
    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/C", base_path);
        assert(strlen(full_path) < 1024);
        vrfy_ns_grp_d(fid, full_path, 300);
    }

    /* validate an empty "old style" group in file */
    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/D", base_path);
        assert(strlen(full_path) < 1024);
        vrfy_os_grp_0(fid, full_path);
    }

    /* validate an "old style" group (w/300 links, in v1 B-tree &
     * local heap)
     */
    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/E", base_path);
        assert(strlen(full_path) < 1024);
        vrfy_os_grp_n(fid, full_path, proc_num, 300);
    }

    /* validate a contiguous dataset w/integer datatype (but no data)
     * in file.
     */
    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/F", base_path);
        assert(strlen(full_path) < 1024);
        vrfy_ds_ctg_i(fid, full_path, false);
    }

    /* validate a contiguous dataset w/integer datatype (with data)
     * in file.
     */
    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/G", base_path);
        assert(strlen(full_path) < 1024);
        vrfy_ds_ctg_i(fid, full_path, true);
    }

    /* validate a chunked dataset w/integer datatype (but no data)
     * in file
     */
    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/H", base_path);
        assert(strlen(full_path) < 1024);
        vrfy_ds_chk_i(fid, full_path, false);
    }

    /* validate a chunked dataset w/integer datatype (and data)
     * in file
     */
    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/I", base_path);
        assert(strlen(full_path) < 1024);
        vrfy_ds_chk_i(fid, full_path, true);
    }

    /* Validate a compact dataset w/integer datatype (but no data)
     * in file
     */
    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/J", base_path);
        assert(strlen(full_path) < 1024);
        vrfy_ds_cpt_i(fid, full_path, false);
    }

    /* validate a compact dataset w/integer datatype (and data)
     * in file
     */
    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/K", base_path);
        assert(strlen(full_path) < 1024);
        vrfy_ds_cpt_i(fid, full_path, true);
    }

    /* validate a contiguous dataset w/variable-length datatype
     * (but no data) to file
     */
    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/L", base_path);
        assert(strlen(full_path) < 1024);
        vrfy_ds_ctg_v(fid, full_path, false);
    }

    /* validate a contiguous dataset w/variable-length datatype
     * (and data) to file
     */
    if (pass) {
        snprintf(full_path, sizeof(full_path), "%s/M", base_path);
        assert(strlen(full_path) < 1024);
        vrfy_ds_ctg_v(fid, full_path, true);
    }

} /* validate_zoo() */
