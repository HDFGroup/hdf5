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

/* Regression tests for table builders with inconsistent stored counts. */
#include "h5test.h"
#define H5A_FRIEND
#define H5B2_FRIEND
#define H5G_FRIEND
#include "H5ACprivate.h"
#include "H5Apkg.h"
#include "H5B2pkg.h"
#include "H5Gpkg.h"
#include "H5CXprivate.h"
#include "H5VLnative_private.h"

#define NRECORDS 40

static herr_t
check_link(const H5O_link_t *lnk, void *op_data)
{
    unsigned *seen = (unsigned *)op_data;
    char      expected[32];

    snprintf(expected, sizeof(expected), "link%02u", *seen);
    if (strcmp(lnk->name, expected))
        return FAIL;
    (*seen)++;
    return SUCCEED;
}

/* Alter only in-memory counts and restore them before closing the file. This
 * reaches the builders directly, including compact counts normally derived
 * from the object header, without relying on a particular disk encoding.
 */
static int
test_table_counts(bool attributes, bool dense)
{
    hid_t            file = H5I_INVALID_HID, group = H5I_INVALID_HID;
    hid_t            gcpl = H5I_INVALID_HID, space = H5I_INVALID_HID, attr = H5I_INVALID_HID;
    H5G_t           *grp;
    H5O_loc_t       *oloc;
    H5O_linfo_t      linfo;
    H5O_ainfo_t      ainfo;
    H5B2_t          *bt2              = NULL;
    H5G_link_table_t ltable           = {0, NULL};
    H5A_attr_table_t atable           = {0, 0, NULL};
    H5CX_node_t      api_ctx          = {{0}, NULL};
    bool             pushed           = false;
    hsize_t          saved_count      = 0;
    unsigned         saved_node_count = 0;
    const hsize_t    counts[]         = {
        0, 1, NRECORDS - 1, NRECORDS, NRECORDS + 1, (hsize_t)0x3ffffffd, (hsize_t)0x3fffffff, HSIZET_MAX};
    char     name[32];
    herr_t   status;
    unsigned seen;

    TESTING(attributes ? "dense attribute tables with fabricated counts"
            : dense ? "dense link tables with fabricated counts"
                    : "compact link tables with fabricated counts");

    if ((file = H5Fcreate("table_counts.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if ((gcpl = H5Pcreate(H5P_GROUP_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_link_creation_order(gcpl, H5P_CRT_ORDER_TRACKED) < 0)
        TEST_ERROR;
    if (H5Pset_link_phase_change(gcpl, dense ? 0 : NRECORDS + 1, 0) < 0)
        TEST_ERROR;
    if (H5Pset_attr_phase_change(gcpl, 0, 0) < 0)
        TEST_ERROR;
    if ((group = H5Gcreate2(file, "group", H5P_DEFAULT, gcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if ((space = H5Screate(H5S_SCALAR)) < 0)
        TEST_ERROR;
    for (unsigned i = 0; i < NRECORDS; i++) {
        snprintf(name, sizeof(name), attributes ? "attr%02u" : "link%02u", i);
        if (attributes) {
            if ((attr = H5Acreate2(group, name, H5T_NATIVE_INT, space, H5P_DEFAULT, H5P_DEFAULT)) < 0)
                TEST_ERROR;
            if (H5Aclose(attr) < 0)
                TEST_ERROR;
            attr = H5I_INVALID_HID;
        }
        else if (H5Lcreate_soft("/target", group, name, H5P_DEFAULT, H5P_DEFAULT) < 0)
            TEST_ERROR;
    }

    if (H5CX_push(&api_ctx) < 0)
        TEST_ERROR;
    pushed = true;
    if (NULL == (grp = (H5G_t *)H5VL_object_verify(group, H5I_GROUP)))
        TEST_ERROR;
    oloc = H5G_oloc(grp);
    H5AC_tag(oloc->addr, NULL);
    if (attributes) {
        if (NULL == H5O_msg_read(oloc, H5O_AINFO_ID, &ainfo))
            TEST_ERROR;
        if (NULL == (bt2 = H5B2_open(oloc->file, ainfo.name_bt2_addr, NULL)))
            TEST_ERROR;
    }
    else {
        if (H5G__obj_get_linfo(oloc, &linfo) <= 0)
            TEST_ERROR;
        if (dense && NULL == (bt2 = H5B2_open(oloc->file, linfo.name_bt2_addr, NULL)))
            TEST_ERROR;
    }
    if (bt2) {
        saved_count      = bt2->hdr->root.all_nrec;
        saved_node_count = bt2->hdr->root.node_nrec;
    }

    /* Also exercise an empty tree with a nonzero total count: no records
     * are copied, but malformed metadata must still fail without leaking.
     */
    for (unsigned empty = 0; empty < (bt2 ? 2u : 1u); empty++) {
        unsigned actual = empty ? 0 : NRECORDS;

        if (bt2)
            bt2->hdr->root.node_nrec = empty ? 0 : (uint16_t)saved_node_count;
        for (size_t i = 0; i < sizeof(counts) / sizeof(counts[0]); i++) {
            if (bt2)
                bt2->hdr->root.all_nrec = counts[i];
            if (!attributes)
                linfo.nlinks = counts[i];
            seen = 0;
            H5E_BEGIN_TRY
            {
                if (attributes)
                    status = H5A__dense_build_table(oloc->file, &ainfo, H5_INDEX_NAME, H5_ITER_INC, &atable);
                else if (dense)
                    status = H5G__dense_build_table(oloc->file, &linfo, H5_INDEX_NAME, H5_ITER_INC, &ltable);
                else
                    status = H5G__compact_iterate(oloc, &linfo, H5_INDEX_NAME, H5_ITER_INC, 0, NULL,
                                                  check_link, &seen);
            }
            H5E_END_TRY
            if (counts[i] != actual) {
                if (status >= 0 || atable.attrs || ltable.lnks || seen || atable.num_attrs || ltable.nlinks)
                    TEST_ERROR;
            }
            else {
                if (status < 0)
                    TEST_ERROR;
                if (attributes) {
                    if (atable.num_attrs != actual || atable.max_attrs > 2 * actual)
                        TEST_ERROR;
                    for (unsigned j = 0; j < actual; j++) {
                        snprintf(name, sizeof(name), "attr%02u", j);
                        if (strcmp(atable.attrs[j]->shared->name, name))
                            TEST_ERROR;
                    }
                    if (H5A__attr_release_table(&atable) < 0)
                        TEST_ERROR;
                }
                else if (dense) {
                    if (ltable.nlinks != actual)
                        TEST_ERROR;
                    for (unsigned j = 0; j < actual; j++)
                        if (check_link(&ltable.lnks[j], &seen) < 0)
                            TEST_ERROR;
                    if (H5G__link_release_table(&ltable) < 0)
                        TEST_ERROR;
                }
                else if (seen != actual)
                    TEST_ERROR;
            }
        }
    }

    if (bt2) {
        bt2->hdr->root.all_nrec  = saved_count;
        bt2->hdr->root.node_nrec = (uint16_t)saved_node_count;
        if (H5B2_close(bt2) < 0)
            TEST_ERROR;
        bt2 = NULL;
    }
    if (H5CX_pop(false) < 0)
        TEST_ERROR;
    pushed = false;
    if (H5Sclose(space) < 0 || H5Pclose(gcpl) < 0 || H5Gclose(group) < 0 || H5Fclose(file) < 0)
        TEST_ERROR;
    PASSED();
    return 0;

error:
    H5Eprint2(H5E_DEFAULT, stdout);
    H5E_BEGIN_TRY
    {
        if (bt2) {
            bt2->hdr->root.all_nrec  = saved_count;
            bt2->hdr->root.node_nrec = (uint16_t)saved_node_count;
            H5B2_close(bt2);
        }
        if (atable.attrs)
            H5A__attr_release_table(&atable);
        if (ltable.lnks)
            H5G__link_release_table(&ltable);
        if (pushed)
            H5CX_pop(false);
        H5Aclose(attr);
        H5Sclose(space);
        H5Pclose(gcpl);
        H5Gclose(group);
        H5Fclose(file);
    }
    H5E_END_TRY
    return 1;
}

int
main(void)
{
    int nerrors = 0;

    h5_test_init();
    nerrors += test_table_counts(false, false);
    nerrors += test_table_counts(false, true);
    nerrors += test_table_counts(true, true);
    if (nerrors)
        return EXIT_FAILURE;
    if (remove("table_counts.h5") < 0)
        return EXIT_FAILURE;
    return EXIT_SUCCESS;
}
