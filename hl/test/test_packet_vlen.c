/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "h5hltest.h"

/*-------------------------------------------------------------------------
 * Packet Table with Variable-Length test
 *
 *-------------------------------------------------------------------------
 */

#define NRECORDS 5
#define TEST_FILE_NAME "test_packet_table_vlen.h5"
#define TESTFL_FILE_NAME "testfl_packet_table_vlen.h5"
#define PT_VLEN_ATOMIC "Dataset with VL of Atomic types"
#define PT_VLEN_COMP "Dataset with VL of Compound Types"
#define PT_COMP_VLEN "Dataset with Compound Type of VL types"
#define PT_VLEN_VLEN "Dataset with VL of VL types"
#define PT_FIXED_LEN "Fixed-length Packet Table"
#define L1_INCM         16
#define L2_INCM         8
#define NAME_BUF_SIZE   80

/*-------------------------------------------------------------------------
 * Local functions
 *-------------------------------------------------------------------------
 */

/* Verifies that the packet table is a variable- or fixed-length */
static int verify_ptlengthtype(hid_t fid, const char *table_name, herr_t checked_value);

/* Adds an attribute to the named packet table */
static int adding_attribute(hid_t fid, const char *table_name, const char *attr_name);

/* Verifies that the named attribute was written and is read correctly */
static int verify_attribute(hid_t fid, const char *table_name, const char *attr_name);

/*-------------------------------------------------------------------------
 * test_VLof_atomic(): Test that a packet table with VL datatypes of atomic
 *    datatypes can be created and written correctly. (HDFFV-442)
 *
 * 2016/01/27 -BMR
 *-------------------------------------------------------------------------
 */
static int test_VLof_atomic(void)
{
    hid_t   fid=H5I_INVALID_HID;    /* Test file identifier */
    hid_t   ptable=H5I_INVALID_HID;    /* Packet table identifier */
    hid_t   vltype=H5I_INVALID_HID;    /* Variable length datatype */
    hsize_t count;        /* Number of records in the table */
    unsigned uu, vv;        /* Loop variables */
    hvl_t   writeBuf[NRECORDS];    /* Buffer to hold data to be written */
    hvl_t   readBuf[NRECORDS];    /* Buffer to hold read data */
    char    msg[80];        /* For error message */
    herr_t  ret;        /* Returned status from a callee */

    HL_TESTING3("        with vlen of atomic");

    /* Allocate and initialize VL data to write (copied from C test) */
    for (uu = 0; uu < NRECORDS; uu++) {
        writeBuf[uu].p = HDmalloc((uu + 1) * sizeof(unsigned int));
        if (writeBuf[uu].p == NULL) {
            HDfprintf(stderr, "Cannot allocate memory for VL data! uu=%u\n", uu);
            goto error;
    }
        writeBuf[uu].len = uu + 1;
        for (vv = 0; vv < (uu + 1); vv++)
        ((unsigned int *)writeBuf[uu].p)[vv] = uu * 10 + vv;
    } /* end for */

    /* Open the file */
    fid = H5Fopen(TEST_FILE_NAME, H5F_ACC_RDWR, H5P_DEFAULT);
    if (fid < 0)
    goto error;

    /* Create a vlen type that uses an atomic datatype as its base type */
    vltype = H5Tvlen_create (H5T_NATIVE_UINT);
    if (vltype < 0)
    goto error;

    /* Create a packet table that uses a vlen datatype of an atomic type */
    ptable = H5PTcreate(fid, PT_VLEN_ATOMIC, vltype, (hsize_t)1, H5P_DEFAULT);

    /* Ensure that PT is created successfully */
    if (ptable == H5I_INVALID_HID)
    goto error;

    /* Close the vlen datatype */
    if (H5Tclose(vltype) < 0)
    goto error;

    /* Write the entire buffer to the packet table */
    ret = H5PTappend(ptable, (size_t)NRECORDS, writeBuf);
    if (ret < 0)
    goto error;

    /* Get the number of packets in the packet table, should be NRECORDS. */
    ret = H5PTget_num_packets(ptable, &count);
    if (ret < 0)
    goto error;

    HDsprintf(msg, "The number of packets in the packet table must be %u\n", NRECORDS);
    VERIFY(count == NRECORDS, msg);

    /* Read all five packets back */
    ret = H5PTread_packets(ptable, (hsize_t)0, (size_t)NRECORDS, (void*)readBuf );
    if (ret < 0)
    goto error;

    for (uu = 0; uu < NRECORDS; uu++)
        for (vv = 0; vv < (uu + 1); vv++)
        {
        if (((unsigned int *)readBuf[uu].p)[vv] != ((unsigned int *)writeBuf[uu].p)[vv]) {
        HDprintf("Packet %u's value should be %d\n", uu, ((unsigned int *)writeBuf[uu].p)[vv]);
        HDprintf("Packet %u's value in readBuf is %d\n", uu, ((unsigned int *)readBuf[uu].p)[vv]);
        }
        }

    /* Free the buffers */
    ret = H5PTfree_vlen_buff(ptable, NRECORDS, readBuf );
    if (ret < 0)
    goto error;
    ret = H5PTfree_vlen_buff(ptable, NRECORDS, writeBuf);
    if (ret < 0)
    goto error;

    /* Close the packet table */
    ret = H5PTclose(ptable);
    if (ret < 0)
    goto error;

    /* Close the file */
    if (H5Fclose(fid) < 0)
    goto error;

    PASSED();
    return SUCCEED;

error: /* An error has occurred.  Clean up and exit. */
    if (vltype > 0) H5Tclose(vltype);
    if (H5PTis_valid(ptable) > 0) H5PTclose(ptable);
    if (fid > 0) H5Fclose(fid);
    H5PTfree_vlen_buff(ptable, NRECORDS, readBuf);
    H5PTfree_vlen_buff(ptable, NRECORDS, writeBuf);
    H5_FAILED();
    return FAIL;
} /* test_VLof_atomic */

/*-------------------------------------------------------------------------
 * test_VLof_comptype(): Test that a packet table with VL datatypes of
 *    compound datatypes can be created and written correctly. (HDFFV-442)
 *
 * 2016/01/27 -BMR
 *-------------------------------------------------------------------------
 */
static int test_VLof_comptype(void)
{
    /* Struct that the VL sequences are composed of */
    typedef struct {
        unsigned u;
        float f;
    } VLcomp_t;
    hid_t   fid=H5I_INVALID_HID;    /* Test file identifier */
    hid_t   ptable=H5I_INVALID_HID;    /* Packet table identifier */
    hid_t   vltype=H5I_INVALID_HID;    /* Variable length datatype */
    hid_t   cmptype=H5I_INVALID_HID;    /* Compound datatype */
    hvl_t   writeBuf[NRECORDS];    /* Buffer to hold data to be written */
    hvl_t   readBuf[NRECORDS];    /* Buffer to hold read data */
    hsize_t count;        /* Number of records in the table */
    unsigned uu, vv;        /* Loop variables */
    char    msg[80];        /* For error message */
    herr_t ret;

    HL_TESTING3("        with vlen of compound datatypes");

    /* Allocate and initialize VL data to write (copied from C test) */
    for (uu = 0; uu < NRECORDS; uu++) {
        writeBuf[uu].p = HDmalloc((uu + 1) * sizeof(VLcomp_t));
        if(writeBuf[uu].p == NULL) {
            HDfprintf(stderr, "Cannot allocate memory for VL data! uu=%u\n", uu);
            goto error;
    }
        writeBuf[uu].len = uu + 1;
        for (vv = 0; vv < (uu + 1); vv++) {
            ((VLcomp_t *)writeBuf[uu].p)[vv].u = uu + vv;
            ((VLcomp_t *)writeBuf[uu].p)[vv].f = (float)(uu + vv) / 3.0F;
          } /* end for */
    } /* end for */

    /* Open the file */
    fid = H5Fopen(TEST_FILE_NAME, H5F_ACC_RDWR, H5P_DEFAULT);
    if (fid < 0)
    goto error;

    /* Create the base compound type */
    cmptype = H5Tcreate(H5T_COMPOUND, sizeof(VLcomp_t));
    if (cmptype < 0)
    goto error;

    /* Insert fields */
    ret = H5Tinsert(cmptype, "u", HOFFSET(VLcomp_t, u), H5T_NATIVE_UINT);
    if (ret < 0)
    goto error;
    ret = H5Tinsert(cmptype, "f", HOFFSET(VLcomp_t, f), H5T_NATIVE_FLOAT);
    if (ret < 0)
    goto error;

    /* Create a variable length type that uses the VLcomp_t as its base type */
    vltype = H5Tvlen_create(cmptype);
    if (vltype < 0)
    goto error;

    /* Create a packet table that uses a vlen datatype of compound datatype */
    ptable = H5PTcreate(fid, PT_VLEN_COMP, vltype, (hsize_t)1, H5P_DEFAULT);

    /* Ensure that PT is created successfully */
    if (ptable == H5I_INVALID_HID)
    goto error;

    /* Release the datatypes */
    if (H5Tclose(cmptype) < 0)
    goto error;
    if (H5Tclose(vltype) < 0)
    goto error;

    /* Write the entire buffer to the packet table */
    ret = H5PTappend(ptable, (size_t)5, writeBuf );
    if (ret < 0)
    goto error;

    /* Get the number of packets in the packet table, should be NRECORDS. */
    ret = H5PTget_num_packets(ptable, &count);
    if (ret < 0)
    goto error;

    HDsprintf(msg, "The number of packets in the packet table must be %u\n", NRECORDS);
    VERIFY(count == NRECORDS, msg);

    /* Read all five packets back */
    ret = H5PTread_packets(ptable, (hsize_t)0, (size_t)5, (void*)readBuf );
    if (ret < 0)
    goto error;

    /* Compare data read in */
    for (uu = 0; uu < NRECORDS; uu++) {
        if (writeBuf[uu].len != readBuf[uu].len) {
            HDfprintf(stderr, "%d: VL data length don't match!, writeBuf[%u].len=%d, readBuf[%u].len=%d\n", __LINE__, uu, (int)writeBuf[uu].len, uu, (int)readBuf[uu].len);
        continue;
    } /* write len != read len */

        for (vv = 0; vv < (uu + 1); vv++) {
            if (((unsigned int *)writeBuf[uu].p)[vv] != ((unsigned int *)readBuf[uu].p)[vv] ) {
                HDfprintf(stderr, "VL data values don't match!, writeBuf[uu].p[%d]=%d, readBuf[uu].p[%d]=%d\n", vv, (int)((unsigned int *)writeBuf[uu].p)[vv], vv, (int)((unsigned int *)readBuf[uu].p)[vv]);
                continue;
        } /* write value != read value */
    }
    } /* end for */

    /* Free the buffers */
    ret = H5PTfree_vlen_buff(ptable, NRECORDS, readBuf);
    if (ret < 0)
    goto error;
    ret = H5PTfree_vlen_buff(ptable, NRECORDS, writeBuf);
    if (ret < 0)
    goto error;

    /* Close the packet table */
    ret = H5PTclose(ptable);
    if (ret < 0)
    goto error;

    /* Close the file */
    if (H5Fclose(fid) < 0)
    goto error;

    PASSED();
    return SUCCEED;

error: /* An error has occurred.  Clean up and exit. */
    if (cmptype > 0) H5Tclose(cmptype);
    if (vltype > 0) H5Tclose(vltype);
    if (H5PTis_valid(ptable) > 0) H5PTclose(ptable);
    if (fid > 0) H5Fclose(fid);
    H5PTfree_vlen_buff(ptable, NRECORDS, readBuf);
    H5PTfree_vlen_buff(ptable, NRECORDS, writeBuf);
    H5_FAILED();
    return FAIL;
} /* test_VLof_comptype */

/*-------------------------------------------------------------------------
 * test_compound_VL_VL(): Test that a packet table of compound datatypes
 *    containing VL datatypes can be created and written
 *    correctly. (HDFFV-442)
 *
 * 2016/01/27 -BMR
 *-------------------------------------------------------------------------
 */
static int test_compound_VL_VLtype(void)
{
    /* Struct that the VL sequences are composed of */
    typedef struct {
        unsigned u;
        float f;
    hvl_t v;
    } compVLVL_t;
    hid_t   fid=H5I_INVALID_HID;    /* Test file identifier */
    hid_t   ptable=H5I_INVALID_HID;    /* Packet table identifier */
    hid_t   vlatomic=H5I_INVALID_HID;    /* Variable length datatype */
    hid_t   vlofvl=H5I_INVALID_HID;    /* Variable length datatype */
    hid_t   comp_vlvl=H5I_INVALID_HID;    /* ID of a compound datatype containing
                    a VL of VL of atomic datatype */
    hsize_t count;        /* Number of records in the table */
    compVLVL_t writeBuf[NRECORDS];/* Buffer to hold data to be written */
    compVLVL_t readBuf[NRECORDS]; /* Buffer to hold read data */
    hvl_t   *t1, *t2;
    unsigned uu, vv, ww;    /* Loop variables */
    char    msg[80];        /* For error message */
    herr_t  ret;        /* Returned status from a callee */

    HL_TESTING3("        with compound datatype containing vlen datatype");

    /* Allocate and initialize VL data to write (copied from C test) */
    for (uu = 0; uu < NRECORDS; uu++) {
        writeBuf[uu].u = uu * 10;
        writeBuf[uu].f = (float)(uu * 20) / 3.0F;
        writeBuf[uu].v.p = HDmalloc((uu + L1_INCM) * sizeof(hvl_t));
        if (writeBuf[uu].v.p == NULL) {
            HDfprintf(stderr, "Cannot allocate memory for VL data! uu=%u\n", uu);
            goto error;
    }
        writeBuf[uu].v.len = uu + L1_INCM;
        for (t1 = (hvl_t *)((writeBuf[uu].v).p), vv = 0; vv < (uu + L1_INCM); vv++, t1++)
    {
            t1->p = HDmalloc((vv + L2_INCM) * sizeof(unsigned int));
        if (t1->p == NULL) {
        HDfprintf(stderr, "Cannot allocate memory for VL data! uu=%u\n", uu);
        goto error;
        }
            t1->len = vv + L2_INCM;
            for (ww = 0; ww < vv + L2_INCM; ww++)
                ((unsigned int *)t1->p)[ww] = uu * 100 + vv * 10 + ww;
        }
    } /* end for */

    /* Open the file */
    fid = H5Fopen(TEST_FILE_NAME, H5F_ACC_RDWR, H5P_DEFAULT);
    if (fid < 0)
    goto error;

    /* Create a VL datatype of an atomic type */
    vlatomic = H5Tvlen_create (H5T_NATIVE_UINT);
    if (vlatomic < 0)
    goto error;

    /* Create a VL datatype of the VL of atomic datatype */
    vlofvl = H5Tvlen_create (vlatomic);
    if (vlofvl < 0)
    goto error;

    /* Create the base compound type */
    comp_vlvl = H5Tcreate(H5T_COMPOUND, sizeof(compVLVL_t));
    if (comp_vlvl < 0)
    goto error;

    /* Insert fields: atomic, atomic, vlen */
    ret = H5Tinsert(comp_vlvl, "u", HOFFSET(compVLVL_t, u), H5T_NATIVE_UINT);
    if (ret < 0)
    goto error;
    ret = H5Tinsert(comp_vlvl, "f", HOFFSET(compVLVL_t, f), H5T_NATIVE_FLOAT);
    if (ret < 0)
    goto error;
    ret = H5Tinsert(comp_vlvl, "v", HOFFSET(compVLVL_t, v), vlofvl);
    if (ret < 0)
    goto error;

    /* Create a packet table that uses a compound datatype of vlen datatype */
    ptable = H5PTcreate(fid, PT_COMP_VLEN, comp_vlvl, (hsize_t)1, H5P_DEFAULT);

    /* Ensure that PT is created successfully */
    if (ptable == H5I_INVALID_HID)
    goto error;

    /* Release datatypes */
    if (H5Tclose(vlatomic) < 0)
    goto error;
    if (H5Tclose(vlofvl) < 0)
    goto error;
    if (H5Tclose(comp_vlvl) < 0)
    goto error;

    /* Write the entire buffer to the packet table */
    ret = H5PTappend(ptable, (size_t)NRECORDS, writeBuf );
    if (ret < 0)
    goto error;

    /* Get the number of packets in the packet table, should be NRECORDS. */
    ret = H5PTget_num_packets(ptable, &count);
    if (ret < 0)
    goto error;

    HDsprintf(msg, "The number of packets in the packet table must be %u\n", NRECORDS);
    VERIFY(count == NRECORDS, msg);

    /* Read all five packets back */
    ret = H5PTread_packets(ptable, (hsize_t)0, (size_t)NRECORDS, (void*)readBuf );
    if (ret < 0)
    goto error;

    /* Compare data read in */
    for (uu = 0; uu < NRECORDS; uu++) {
        if (writeBuf[uu].u != readBuf[uu].u) {
            HDfprintf(stderr, "Integer components don't match!, writeBuf[%u].u=%u, readBuf[%u].u=%u\n", uu, writeBuf[uu].u, uu, readBuf[uu].u);
            continue;
        } /* end if */
        if (!H5_FLT_ABS_EQUAL(writeBuf[uu].f,readBuf[uu].f)) {
            HDfprintf(stderr, "Float components don't match!, writeBuf[%u].f=%f, readBuf[%u].f=%f\n", uu, (double)writeBuf[uu].f, uu, (double)readBuf[uu].f);
            continue;
        } /* end if */

        if (writeBuf[uu].v.len != readBuf[uu].v.len) {
            HDfprintf(stderr, "%d: VL data length don't match!, writeBuf[%d].v.len=%zu, readBuf[%d].v.len=%zu\n", __LINE__, uu, writeBuf[uu].v.len, uu, readBuf[uu].v.len);
            continue;
        } /* end if */

        for (t1 = (hvl_t *)(writeBuf[uu].v.p), t2 = (hvl_t *)(readBuf[uu].v.p), vv = 0; (size_t)vv < readBuf[uu].v.len; vv++, t1++, t2++) {
            if (t1->len != t2->len) {
                HDfprintf(stderr, "%d: VL data length don't match!, uu=%u, vv=%u, t1->len=%zu, t2->len=%zu\n", __LINE__, uu, vv, t1->len, t2->len);
                continue;
            } /* end if */
            for (ww = 0; (size_t)ww < t2->len; ww++) {
                if (((unsigned int *)t1->p)[ww] != ((unsigned int *)t2->p)[ww] ) {
                    HDfprintf(stderr, "VL data values don't match!, t1->p[%u]=%u, t2->p[%u]=%u\n", ww, ((unsigned int *)t1->p)[ww], ww, ((unsigned int *)t2->p)[ww]);
                    continue;
                } /* end if */
            } /* end for */
        } /* end for */
    } /* end for */

    /* Free the buffers */
    ret = H5PTfree_vlen_buff(ptable, NRECORDS, readBuf);
    if (ret < 0)
    goto error;
    ret = H5PTfree_vlen_buff(ptable, NRECORDS, writeBuf);
    if (ret < 0)
    goto error;

    /* Close the packet table */
    ret = H5PTclose(ptable);
    if (ret < 0)
    goto error;

    /* Close the file */
    if (H5Fclose(fid) < 0)
    goto error;

    PASSED();
    return SUCCEED;

error: /* An error has occurred.  Clean up and exit. */
    if (vlatomic > 0) H5Tclose(vlatomic);
    if (vlofvl > 0) H5Tclose(vlofvl);
    if (comp_vlvl > 0) H5Tclose(comp_vlvl);
    if (H5PTis_valid(ptable) > 0) H5PTclose(ptable);
    if (fid > 0) H5Fclose(fid);
    H5PTfree_vlen_buff(ptable, NRECORDS, readBuf);
    H5PTfree_vlen_buff(ptable, NRECORDS, writeBuf);
    H5_FAILED();
    return FAIL;
} /* test_compound_VL_VLtype */

/*-------------------------------------------------------------------------
 * test_VLof_VLtype(): Test that a packet table of VL datatype with VL
 *    datatypes of atomic datatypes can be created and written
 *    correctly. (HDFFV-442)
 *
 * 2016/01/27 -BMR
 *-------------------------------------------------------------------------
 */
static int test_VLof_VLtype(void)
{
    hid_t   fid=H5I_INVALID_HID;    /* Test file identifier */
    hid_t   ptable=H5I_INVALID_HID;    /* Packet table identifier */
    hid_t   vlatomic=H5I_INVALID_HID;    /* Variable length datatype */
    hid_t   vlofvl=H5I_INVALID_HID;    /* VL datatype of VL datatypes */
    hsize_t count;        /* Number of records in the table */
    hvl_t   *t1;        /* pointer to advance */
    unsigned uu, vv, ww;    /* Loop variables */
    hvl_t   writeBuf[NRECORDS];    /* Buffer to hold data to be written */
    hvl_t   readBuf[NRECORDS];    /* Buffer to hold read data */
    char    msg[80];        /* For error message */
    herr_t  ret;        /* Returned status from a callee */

    HL_TESTING3("        with vlen datatype of vlen datatype");

    /* Allocate and initialize VL data to write (copied from C test) */
    for (uu = 0; uu < NRECORDS; uu++) {
        writeBuf[uu].p = HDmalloc((uu + 1) * sizeof(hvl_t));
        if (writeBuf[uu].p == NULL) {
            HDfprintf(stderr, "Cannot allocate memory for VL data! uu=%u\n", uu);
            goto error;
        } /* end if */
        writeBuf[uu].len = uu + 1;
        for (t1=(hvl_t *)(writeBuf[uu].p), vv = 0; vv < (uu + 1); vv++, t1++)
    {
            t1->p = HDmalloc((vv + 1) * sizeof(unsigned int));
        if (t1->p == NULL) {
        HDfprintf(stderr, "Cannot allocate memory for VL data! uu=%u\n", uu);
        goto error;
        }
            t1->len = vv * 1;
            for (ww = 0; ww < (vv * 1); ww++)
                ((unsigned int *)t1->p)[ww] = uu * 100 + vv * 10 + ww;
        } /* end for */
    } /* end for */

    /* Open the file */
    fid = H5Fopen(TEST_FILE_NAME, H5F_ACC_RDWR, H5P_DEFAULT);
    if (fid < 0)
    goto error;

    /* Create a VL datatype of an atomic type */
    vlatomic = H5Tvlen_create (H5T_NATIVE_UINT);
    if (vlatomic < 0)
    goto error;

    vlofvl = H5Tvlen_create (vlatomic);
    if (vlofvl < 0)
    goto error;

    /* Create a packet table that uses a vlen datatype of vlen datatype */
    ptable = H5PTcreate(fid, PT_VLEN_VLEN, vlofvl, (hsize_t)1, H5P_DEFAULT);

    /* Ensure that PT is created successfully */
    if (ptable == H5I_INVALID_HID)
    goto error;

    /* Release datatypes */
    if (H5Tclose(vlatomic) < 0)
    goto error;
    if (H5Tclose(vlofvl) < 0)
    goto error;

    /* Write the entire buffer to the packet table */
    ret = H5PTappend(ptable, (size_t)5, writeBuf );
    if (ret < 0)
    goto error;

    /* Get the number of packets in the packet table, should be NRECORDS. */
    ret = H5PTget_num_packets(ptable, &count);
    if (ret < 0)
    goto error;

    HDsprintf(msg, "The number of packets in the packet table must be %u\n", NRECORDS);
    VERIFY(count == NRECORDS, msg);

    /* Read all five packets back */
    ret = H5PTread_packets(ptable, (hsize_t)0, (size_t)5, (void*)readBuf );
    if (ret < 0)
    goto error;

    /* Free the buffers */
    ret = H5PTfree_vlen_buff(ptable, NRECORDS, readBuf);
    if (ret < 0)
    goto error;
    ret = H5PTfree_vlen_buff(ptable, NRECORDS, writeBuf);
    if (ret < 0)
    goto error;

    /* Close the packet table */
    ret = H5PTclose(ptable);
    if (ret < 0)
    goto error;

    /* Close the file */
    if (H5Fclose(fid) < 0)
    goto error;

    PASSED();
    return SUCCEED;

error: /* An error has occurred.  Clean up and exit. */
    if (vlatomic > 0) H5Tclose(vlatomic);
    if (vlofvl > 0) H5Tclose(vlofvl);
    if (H5PTis_valid(ptable) > 0) H5PTclose(ptable);
    if (fid > 0) H5Fclose(fid);
    H5PTfree_vlen_buff(ptable, NRECORDS, readBuf);
    H5PTfree_vlen_buff(ptable, NRECORDS, writeBuf);
    H5_FAILED();
    return FAIL;
} /* test_VLof_VLtype */

/*-------------------------------------------------------------------------
 * verify_ptlengthtype() - helper function, verifies that the named packet
 *    table is a fixed-length or variable-length as indicated by the last
 *    argument.
 *
 * 2016/01/27 -BMR
 *-------------------------------------------------------------------------
 */
static int verify_ptlengthtype(hid_t fid, const char *table_name, herr_t expected_value)
{
    hid_t   ptable = H5I_INVALID_HID;    /* Packet table identifier */
    herr_t  is_varlen = 0;
    herr_t  ret = FAIL;

    /* Open the named packet table */
    if( (ptable = H5PTopen(fid, table_name)) < 0)
    goto error;

    /* Verify the value returned from H5PTis_varlen is as expected */
    is_varlen = H5PTis_varlen(ptable);
    if (is_varlen == FAIL)
    goto error;
    else if (is_varlen == expected_value)
    ret = SUCCEED;
    else
    {
    char lenthtype[20];
    HDstrcpy(lenthtype, "fixed-length");
    if (expected_value == 1)
        HDstrcpy(lenthtype, "variable-length");
    HDfprintf(stderr, "\nPacket table '%s' should be %s but is not\n", table_name, lenthtype);
    ret = FAIL;
    }

    /* Close the packet table */
    if (H5PTclose(ptable) < 0)
    goto error;

    return ret;

error: /* An error has occurred.  Clean up and exit. */
    if (H5PTis_valid(ptable) > 0) H5PTclose(ptable);
    return ret;
} /* verify_ptlengthtype */

/*-------------------------------------------------------------------------
 * test_H5PTis_varlen(): Test that H5PTis_varlen works correctly on both
 *    fixed- and variable-length packet tables.
 *
 * Description:
 *    - Added a fixed-length packet table to the file for variety
 *    - Use the helper funtion verify_ptlengthtype to test H5PTis_varlen
 *      on each packet table.
 *
 * 2016/01/27 -BMR
 *-------------------------------------------------------------------------
 */
static int test_H5PTis_varlen(void)
{
    hid_t   fid=H5I_INVALID_HID;    /* Test file identifier */
    hid_t   ptable=H5I_INVALID_HID;    /* Packet table identifier */
    herr_t  ret;        /* Returned status from a callee */

    HL_TESTING2("H5PTis_varlen");

    /* Open the file */
    fid = H5Fopen(TEST_FILE_NAME, H5F_ACC_RDWR, H5P_DEFAULT);
    if (fid < 0)
    goto error;

    /* Create a new table */
    ptable = H5PTcreate(fid, PT_FIXED_LEN, H5T_STD_I32BE, (hsize_t)100, H5P_DEFAULT);

    /* Ensure that PT is created successfully */
    if (ptable == H5I_INVALID_HID)
    goto error;

    /* Close the packet table */
    ret = H5PTclose(ptable);
    if (ret < 0)
    goto error;

    /* Close the file */
    if (H5Fclose(fid) < 0)
    goto error;

    /* Open the file */
    fid = H5Fopen(TEST_FILE_NAME, H5F_ACC_RDWR, H5P_DEFAULT);
    if (fid < 0)
    goto error;

    /* Open each packet table, and verify that H5PTis_varlen returns correct
       type for each table */
    ret = verify_ptlengthtype(fid, PT_VLEN_ATOMIC, 1); /* vlen of atomic */
    if (ret < 0)
    goto error;
    ret = verify_ptlengthtype(fid, PT_VLEN_COMP, 1); /* vlen of compound */
    if (ret < 0)
    goto error;
    ret = verify_ptlengthtype(fid, PT_COMP_VLEN, 0); /* compound of vlen, no vlen */
    if (ret < 0)
    goto error;
    ret = verify_ptlengthtype(fid, PT_VLEN_VLEN, 1); /* vlen of vlen */
    if (ret < 0)
    goto error;
    ret = verify_ptlengthtype(fid, PT_FIXED_LEN, 0); /* no vlen */
    if (ret < 0)
    goto error;

    /* Close the file */
    if (H5Fclose(fid) < 0)
    goto error;

    PASSED();
    return SUCCEED;

error: /* An error has occurred.  Clean up and exit. */
    if (fid > 0) H5Fclose(fid);
    H5_FAILED();
    return FAIL;
} /* test_H5PTis_varlen */

/*-------------------------------------------------------------------------
 * adding_attribute() - helper function, adds an attribute to the named
 *    packet table.
 * Note:
 *    For simplicity, the attributes that are added to the packet tables
 *    have the same characteristics except their names.  They have the
 *    same type, space, and values.
 *
 * 2016/01/27 -BMR
 *-------------------------------------------------------------------------
 */
#define ATTR_RANK      1
#define ATTR_DIM       3
int attr_data[ATTR_DIM]={256,11945,-22107}; /* values to be written to attr */

static int adding_attribute(hid_t fid, const char *table_name, const char *attr_name)
{
    hid_t ptable = H5I_INVALID_HID;    /* Packet table identifier */
    hid_t space_id = H5I_INVALID_HID;    /* Dataspace for the attribute */
    hid_t attr_id = H5I_INVALID_HID;    /* Attribute identifier */
    hid_t dset_id = H5I_INVALID_HID;    /* Dataset identifier */
    hsize_t dims[] = {ATTR_DIM};    /* Dimension for dataspace */
    int ret = FAIL;        /* Returned status from a callee */

    /* Create dataspace for attribute */
    space_id = H5Screate_simple(ATTR_RANK, dims, NULL);
    if (space_id < 0)
    goto error;

    /* Open the named packet table */
    if( (ptable = H5PTopen(fid, table_name)) < 0)
    goto error;

    dset_id = H5PTget_dataset(ptable);
    if (dset_id < 0)
    goto error;

    /* Add the specified attribute to it */
    attr_id = H5Acreate2(dset_id, attr_name, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT);
    if (attr_id < 0)
    goto error;

    /* Write attribute values */
    ret = H5Awrite(attr_id, H5T_NATIVE_INT, attr_data);
    if (ret < 0)
    goto error;

    /* Close the attribute */
    if (H5Aclose(attr_id) < 0)
    goto error;

    /* Close the dataspace */
    if (H5Sclose(space_id) < 0)
    goto error;

    /* Close the packet table */
    if (H5PTclose(ptable) < 0)
    goto error;

    return SUCCEED;

error: /* An error has occurred.  Clean up and exit. */
    if (attr_id > 0) H5Aclose(attr_id);
    if (space_id > 0) H5Sclose(space_id);
    if (H5PTis_valid(ptable) > 0) H5PTclose(ptable);
    return ret;
} /* adding_attribute */

/*-------------------------------------------------------------------------
 * verify_attribute() - helper function, verifies the named attribute can
 *    be read correctly.
 *
 * 2016/01/27 -BMR
 *-------------------------------------------------------------------------
 */
static herr_t verify_attribute(hid_t fid, const char *table_name, const char *attr_name)
{
    hid_t ptable=H5I_INVALID_HID;    /* Packet table identifier */
    hid_t attr_id=H5I_INVALID_HID;    /* Attribute identifier */
    hid_t dset_id=H5I_INVALID_HID;    /* Dataset associated with the pt */
    int read_data[ATTR_DIM];        /* Output buffer */
    int ii;
    herr_t ret = FAIL;        /* Returned status from a callee */

    /* Open the named packet table */
    ptable = H5PTopen(fid, table_name);
    if (ptable < 0)
    goto error;

    /* Get the dataset id of this packet table */
    dset_id = H5PTget_dataset(ptable);
    if (dset_id < 0)
    goto error;

    /* Open first attribute for the dataset */
    attr_id = H5Aopen(dset_id, attr_name, H5P_DEFAULT);
    if (attr_id < 0)
    goto error;

    /* Read attribute values */
    ret = H5Aread(attr_id, H5T_NATIVE_INT, read_data);
    if (ret < 0)
    goto error;

    /* Verify values read in */
    for (ii = 0; ii < ATTR_DIM; ii++)
        if (attr_data[ii] != read_data[ii])
            TestErrPrintf("%d: attribute data different: attr_data[%d]=%d, read_data[%d]=%d\n", __LINE__, ii, attr_data[ii], ii, read_data[ii]);

    /* Close the attribute */
    if (H5Aclose(attr_id) < 0)
    goto error;

    /* Close the packet table */
    if (H5PTclose(ptable) < 0)
    goto error;

    return SUCCEED;

error: /* An error has occurred.  Clean up and exit. */
    if (attr_id > 0) H5Aclose(attr_id);
    if (H5PTis_valid(ptable) > 0) H5PTclose(ptable);
    return ret;
} /* verify_attribute */

/*-------------------------------------------------------------------------
 * test_attributes(): Test adding attributes to packet tables
 *
 * Description:
 *    Added attributes to some random packet tables in the file.
 *
 * 2016/01/27 -BMR
 *-------------------------------------------------------------------------
 */
static int test_attributes(void)
{
    hid_t fid=H5I_INVALID_HID;        /* File identifier */
    hid_t attr_id=H5I_INVALID_HID;    /* Attribute identifier */
    herr_t ret = FAIL;        /* Returned status from a callee */

    HL_TESTING2("adding attributes to packet tables");

    /* Open the file */
    fid = H5Fopen(TEST_FILE_NAME, H5F_ACC_RDWR, H5P_DEFAULT);
    if (fid < 0)
    goto error;

    /* Add an arbitrary attribute to a few packet tables, using helper func */
    attr_id = adding_attribute(fid, PT_VLEN_ATOMIC, "Attribute 1");
    if (attr_id < 0)
    goto error;
    attr_id = adding_attribute(fid, PT_VLEN_COMP, "Attribute 2");
    if (attr_id < 0)
    goto error;
    attr_id = adding_attribute(fid, PT_COMP_VLEN, "Attribute 3");
    if (attr_id < 0)
    goto error;

    /* Close the file */
    if (H5Fclose(fid) < 0)
    goto error;

    /* Open the file again */
    fid = H5Fopen(TEST_FILE_NAME, H5F_ACC_RDWR, H5P_DEFAULT);
    if (fid < 0)
    goto error;

    /* Read each attribute and verify the values, using helper function */
    ret = verify_attribute(fid, PT_VLEN_ATOMIC, "Attribute 1");
    if (ret < 0)
    goto error;
    ret = verify_attribute(fid, PT_VLEN_COMP, "Attribute 2");
    if (ret < 0)
    goto error;
    ret = verify_attribute(fid, PT_COMP_VLEN, "Attribute 3");
    if (ret < 0)
    goto error;

    /* Close the file */
    if (H5Fclose(fid) < 0)
    goto error;

    PASSED();
    return(ret);

error: /* An error has occurred.  Clean up and exit. */
    if (fid > 0) H5Fclose(fid);
    H5_FAILED();
    return FAIL;
} /* test_attributes */

/*-------------------------------------------------------------------------
 * verify_accessors() - helper function, verifies that various info can be
 *    retrieved correctly using the info returned by the accessor functions.
 *
 * Description:
 *    Testing functions H5PTget_dataset and H5PTget_type
 *
 *    - Opens the named packet table
 *    - Gets its associated dataset ID then calls a C function on that ID
 *      to verify the dataset name
 *    - Gets its associated datatype ID then calls a C function on that ID
 *      to verify that the packet table is variable- or fixed-length as
 *      indicated by the expected_value argument
 *
 * 2016/01/27 -BMR
 *-------------------------------------------------------------------------
 */
static herr_t verify_accessors(hid_t fid, const char *table_name, hbool_t uses_vlen_type)
{
    hid_t ptable = H5I_INVALID_HID;     /* Packet table identifier */
    hid_t dset_id = H5I_INVALID_HID;    /* Dataset associated with the pt */
    hid_t dtype_id = H5I_INVALID_HID;   /* Dataset identifier */
    char buf[NAME_BUF_SIZE];
    ssize_t name_size;
    htri_t vlen_check_result = -1;

    /* Open the named packet table. */
    if((ptable = H5PTopen(fid, table_name)) < 0)
        goto error;

    /* Get the associated dataset ID. */
    if((dset_id = H5PTget_dataset(ptable)) < 0)
        goto error;

    /* Check if the packet table's name matches its associated dataset's. */
    *buf = '\0';
    if((name_size = H5Iget_name(dset_id, (char*)buf, NAME_BUF_SIZE)) < 0)
        goto error;
    VERIFY(HDstrcmp(buf, table_name), "Names of dataset and packet table don't match");

    /* Get the packet table's datatype ID */
    if((dtype_id = H5PTget_type(ptable)) < 0)
        goto error;

    /* Check if the type class matches that of the packet table. */
    if((vlen_check_result = H5Tdetect_class(dtype_id, H5T_VLEN)) < 0)
        goto error;

    /* Check if length types match */
    if (vlen_check_result != (htri_t)uses_vlen_type) {
        /* Give lengthtype "fixed-length" or "variable-length" depending on the
         * expected_value passed in, then print the error message.
         */
        char lenthtype[20];
        if (uses_vlen_type == TRUE)
            HDstrcpy(lenthtype, "variable-length");
        else
            HDstrcpy(lenthtype, "fixed-length");
        HDfprintf(stderr, "\nThe dataset '%s' should be %s but is not\n", table_name, lenthtype);
        goto error;
    }

    /* Close the packet table */
    if (H5PTclose(ptable) < 0)
        goto error;

    return SUCCEED;

error: /* An error has occurred.  Clean up and exit. */
    if (H5PTis_valid(ptable) > 0)
        H5PTclose(ptable);
    H5_FAILED();
    return FAIL;
} /* verify_accessors */

/*-------------------------------------------------------------------------
 * test_accessors(): Test the accessor functions
 *
 * Description:
 *    Retrieves the dataset and datatype IDs and verifies various info
 *    to ensure these IDs are correct.
 *
 * 2016/01/27 -BMR
 *-------------------------------------------------------------------------
 */
static int test_accessors(void)
{
    hid_t fid=H5I_INVALID_HID;        /* File identifier */
    herr_t ret = FAIL;        /* Returned status from a callee */

    HL_TESTING2("accessor functions");

    /* Open the file */
    fid = H5Fopen(TEST_FILE_NAME, H5F_ACC_RDWR, H5P_DEFAULT);
    if (fid < 0)
        goto error;

    ret = verify_accessors(fid, PT_VLEN_ATOMIC, TRUE);
    if (ret < 0)
        goto error;

    ret = verify_accessors(fid, PT_FIXED_LEN, FALSE);
    if (ret < 0)
        goto error;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        goto error;

    PASSED();
    return SUCCEED;

error: /* An error has occurred.  Clean up and exit. */
    if (fid > 0)
        H5Fclose(fid);
    H5_FAILED();
    return FAIL;
} /* test_accessors */

/**************************************************************************
    Test set for deprecated function H5PTcreate_fl
    Each test in this set is the same as the corresponding one in the
    set for H5PTcreate, as of Mar 2016

**************************************************************************/

/*-------------------------------------------------------------------------
 * testfl_VLof_atomic(): Test that a packet table with VL datatypes of atomic
 *    datatypes can be created and written correctly. (HDFFV-442)
 *
 * 2016/01/27 -BMR
 *-------------------------------------------------------------------------
 */
static int testfl_VLof_atomic(void)
{
    hid_t   fid=H5I_INVALID_HID;    /* Test file identifier */
    hid_t   ptable=H5I_INVALID_HID;    /* Packet table identifier */
    hid_t   vltype=H5I_INVALID_HID;    /* Variable length datatype */
    hsize_t count;        /* Number of records in the table */
    unsigned uu, vv;        /* Loop variables */
    hvl_t   writeBuf[NRECORDS];    /* Buffer to hold data to be written */
    hvl_t   readBuf[NRECORDS];    /* Buffer to hold read data */
    char    msg[80];        /* For error message */
    herr_t  ret;        /* Returned status from a callee */

    HL_TESTING3("        with vlen of atomic");

    /* Allocate and initialize VL data to write (copied from C test) */
    for (uu = 0; uu < NRECORDS; uu++) {
        writeBuf[uu].p = HDmalloc((uu + 1) * sizeof(unsigned int));
        if (writeBuf[uu].p == NULL) {
            HDfprintf(stderr, "Cannot allocate memory for VL data! uu=%u\n", uu);
            goto error;
    }
        writeBuf[uu].len = uu + 1;
        for (vv = 0; vv < (uu + 1); vv++)
        ((unsigned int *)writeBuf[uu].p)[vv] = uu * 10 + vv;
    } /* end for */

    /* Open the file */
    fid = H5Fopen(TESTFL_FILE_NAME, H5F_ACC_RDWR, H5P_DEFAULT);
    if (fid < 0)
    goto error;

    /* Create a vlen type that uses an atomic datatype as its base type */
    vltype = H5Tvlen_create (H5T_NATIVE_UINT);
    if (vltype < 0)
    goto error;

    /* Create a packet table that uses a vlen datatype of an atomic type */
    ptable = H5PTcreate_fl(fid, PT_VLEN_ATOMIC, vltype, (hsize_t)1, 0);

    /* Ensure that PT is created successfully */
    if (ptable == H5I_INVALID_HID)
    goto error;

    /* Close the vlen datatype */
    if (H5Tclose(vltype) < 0)
    goto error;

    /* Write the entire buffer to the packet table */
    ret = H5PTappend(ptable, (size_t)NRECORDS, writeBuf);
    if (ret < 0)
    goto error;

    /* Get the number of packets in the packet table, should be NRECORDS. */
    ret = H5PTget_num_packets(ptable, &count);
    if (ret < 0)
    goto error;

    HDsprintf(msg, "The number of packets in the packet table must be %u\n", NRECORDS);
    VERIFY(count == NRECORDS, msg);

    /* Read all five packets back */
    ret = H5PTread_packets(ptable, (hsize_t)0, (size_t)NRECORDS, (void*)readBuf );
    if (ret < 0)
    goto error;

    for (uu = 0; uu < NRECORDS; uu++)
        for (vv = 0; vv < (uu + 1); vv++)
        {
        if (((unsigned int *)readBuf[uu].p)[vv] != ((unsigned int *)writeBuf[uu].p)[vv]) {
        HDprintf("Packet %d's value should be %d\n", uu, ((unsigned int *)writeBuf[uu].p)[vv]);
        HDprintf("Packet %d's value in readBuf is %d\n", uu, ((unsigned int *)readBuf[uu].p)[vv]);
        }
        }

    /* Free the buffers */
    ret = H5PTfree_vlen_buff(ptable, NRECORDS, readBuf );
    if (ret < 0)
    goto error;
    ret = H5PTfree_vlen_buff(ptable, NRECORDS, writeBuf);
    if (ret < 0)
    goto error;

    /* Close the packet table */
    ret = H5PTclose(ptable);
    if (ret < 0)
    goto error;

    /* Close the file */
    if (H5Fclose(fid) < 0)
    goto error;

    PASSED();
    return SUCCEED;

error: /* An error has occurred.  Clean up and exit. */
    if (vltype > 0) H5Tclose(vltype);
    if (H5PTis_valid(ptable) > 0) H5PTclose(ptable);
    if (fid > 0) H5Fclose(fid);
    H5PTfree_vlen_buff(ptable, NRECORDS, readBuf);
    H5PTfree_vlen_buff(ptable, NRECORDS, writeBuf);
    H5_FAILED();
    return FAIL;
} /* testfl_VLof_atomic */

/*-------------------------------------------------------------------------
 * testfl_VLof_comptype(): Test that a packet table with VL datatypes of
 *    compound datatypes can be created and written correctly. (HDFFV-442)
 *
 * 2016/01/27 -BMR
 *-------------------------------------------------------------------------
 */
static int testfl_VLof_comptype(void)
{
    /* Struct that the VL sequences are composed of */
    typedef struct {
        unsigned u;
        float f;
    } VLcomp_t;
    hid_t   fid=H5I_INVALID_HID;    /* Test file identifier */
    hid_t   ptable=H5I_INVALID_HID;    /* Packet table identifier */
    hid_t   vltype=H5I_INVALID_HID;    /* Variable length datatype */
    hid_t   cmptype=H5I_INVALID_HID;    /* Compound datatype */
    hvl_t   writeBuf[NRECORDS];    /* Buffer to hold data to be written */
    hvl_t   readBuf[NRECORDS];    /* Buffer to hold read data */
    hsize_t count;        /* Number of records in the table */
    unsigned uu, vv;        /* Loop variables */
    char    msg[80];        /* For error message */
    herr_t ret;

    HL_TESTING3("        with vlen of compound datatypes");

    /* Allocate and initialize VL data to write (copied from C test) */
    for (uu = 0; uu < NRECORDS; uu++) {
        writeBuf[uu].p = HDmalloc((uu + 1) * sizeof(VLcomp_t));
        if(writeBuf[uu].p == NULL) {
            HDfprintf(stderr, "Cannot allocate memory for VL data! uu=%u\n", uu);
            goto error;
    }
        writeBuf[uu].len = uu + 1;
        for (vv = 0; vv < (uu + 1); vv++) {
            ((VLcomp_t *)writeBuf[uu].p)[vv].u = uu + vv;
            ((VLcomp_t *)writeBuf[uu].p)[vv].f = (float)(uu + vv) / 3.0F;
          } /* end for */
    } /* end for */

    /* Open the file */
    fid = H5Fopen(TESTFL_FILE_NAME, H5F_ACC_RDWR, H5P_DEFAULT);
    if (fid < 0)
    goto error;

    /* Create the base compound type */
    cmptype = H5Tcreate(H5T_COMPOUND, sizeof(VLcomp_t));
    if (cmptype < 0)
    goto error;

    /* Insert fields */
    ret = H5Tinsert(cmptype, "u", HOFFSET(VLcomp_t, u), H5T_NATIVE_UINT);
    if (ret < 0)
    goto error;
    ret = H5Tinsert(cmptype, "f", HOFFSET(VLcomp_t, f), H5T_NATIVE_FLOAT);
    if (ret < 0)
    goto error;

    /* Create a variable length type that uses the VLcomp_t as its base type */
    vltype = H5Tvlen_create(cmptype);
    if (vltype < 0)
    goto error;

    /* Create a packet table that uses a vlen datatype of compound datatype */
    ptable = H5PTcreate_fl(fid, PT_VLEN_COMP, vltype, (hsize_t)1, 0);

    /* Ensure that PT is created successfully */
    if (ptable == H5I_INVALID_HID)
    goto error;

    /* Release the datatypes */
    if (H5Tclose(cmptype) < 0)
    goto error;
    if (H5Tclose(vltype) < 0)
    goto error;

    /* Write the entire buffer to the packet table */
    ret = H5PTappend(ptable, (size_t)5, writeBuf );
    if (ret < 0)
    goto error;

    /* Get the number of packets in the packet table, should be NRECORDS. */
    ret = H5PTget_num_packets(ptable, &count);
    if (ret < 0)
    goto error;

    HDsprintf(msg, "The number of packets in the packet table must be %u\n", NRECORDS);
    VERIFY(count == NRECORDS, msg);

    /* Read all five packets back */
    ret = H5PTread_packets(ptable, (hsize_t)0, (size_t)5, (void*)readBuf );
    if (ret < 0)
    goto error;

    /* Compare data read in */
    for (uu = 0; uu < NRECORDS; uu++) {
        if (writeBuf[uu].len != readBuf[uu].len) {
            HDfprintf(stderr, "%d: VL data length don't match!, writeBuf[%u].len=%zu, readBuf[%u].len=%zu\n",__LINE__, uu, writeBuf[uu].len, uu, readBuf[uu].len);
        continue;
    } /* write len != read len */

        for (vv = 0; vv < (uu + 1); vv++) {
            if (((unsigned int *)writeBuf[uu].p)[vv] != ((unsigned int *)readBuf[uu].p)[vv] ) {
                HDfprintf(stderr, "VL data values don't match!, writeBuf[uu].p[%u]=%u, readBuf[uu].p[%u]=%u\n", vv, ((unsigned int *)writeBuf[uu].p)[vv], vv, ((unsigned int *)readBuf[uu].p)[vv]);
                continue;
        } /* write value != read value */
    }
    } /* end for */

    /* Free the buffers */
    ret = H5PTfree_vlen_buff(ptable, NRECORDS, readBuf);
    if (ret < 0)
    goto error;
    ret = H5PTfree_vlen_buff(ptable, NRECORDS, writeBuf);
    if (ret < 0)
    goto error;

    /* Close the packet table */
    ret = H5PTclose(ptable);
    if (ret < 0)
    goto error;

    /* Close the file */
    if (H5Fclose(fid) < 0)
    goto error;

    PASSED();
    return SUCCEED;

error: /* An error has occurred.  Clean up and exit. */
    if (cmptype > 0) H5Tclose(cmptype);
    if (vltype > 0) H5Tclose(vltype);
    if (H5PTis_valid(ptable) > 0) H5PTclose(ptable);
    if (fid > 0) H5Fclose(fid);
    H5PTfree_vlen_buff(ptable, NRECORDS, readBuf);
    H5PTfree_vlen_buff(ptable, NRECORDS, writeBuf);
    H5_FAILED();
    return FAIL;
} /* testfl_VLof_comptype */

/*-------------------------------------------------------------------------
 * testfl_compound_VL_VL(): Test that a packet table of compound datatypes
 *    containing VL datatypes can be created and written
 *    correctly. (HDFFV-442)
 *
 * 2016/01/27 -BMR
 *-------------------------------------------------------------------------
 */
static int testfl_compound_VL_VLtype(void)
{
    /* Struct that the VL sequences are composed of */
    typedef struct {
        unsigned u;
        float f;
    hvl_t v;
    } compVLVL_t;
    hid_t   fid=H5I_INVALID_HID;    /* Test file identifier */
    hid_t   ptable=H5I_INVALID_HID;    /* Packet table identifier */
    hid_t   vlatomic=H5I_INVALID_HID;    /* Variable length datatype */
    hid_t   vlofvl=H5I_INVALID_HID;    /* Variable length datatype */
    hid_t   comp_vlvl=H5I_INVALID_HID;    /* ID of a compound datatype containing
                    a VL of VL of atomic datatype */
    hsize_t count;        /* Number of records in the table */
    compVLVL_t writeBuf[NRECORDS];/* Buffer to hold data to be written */
    compVLVL_t readBuf[NRECORDS]; /* Buffer to hold read data */
    hvl_t   *t1, *t2;
    unsigned uu, vv, ww;    /* Loop variables */
    char    msg[80];        /* For error message */
    herr_t  ret;        /* Returned status from a callee */

    HL_TESTING3("        with compound datatype containing vlen datatype");

    /* Allocate and initialize VL data to write (copied from C test) */
    for (uu = 0; uu < NRECORDS; uu++) {
        writeBuf[uu].u = uu * 10;
        writeBuf[uu].f = (float)(uu * 20) / 3.0F;
        writeBuf[uu].v.p = HDmalloc((uu + L1_INCM) * sizeof(hvl_t));
        if (writeBuf[uu].v.p == NULL) {
            HDfprintf(stderr, "Cannot allocate memory for VL data! uu=%u\n", uu);
            goto error;
    }
        writeBuf[uu].v.len = uu + L1_INCM;
        for (t1 = (hvl_t *)((writeBuf[uu].v).p), vv = 0; vv < (uu + L1_INCM); vv++, t1++)
    {
            t1->p = HDmalloc((vv + L2_INCM) * sizeof(unsigned int));
        if (t1->p == NULL) {
        HDfprintf(stderr, "Cannot allocate memory for VL data! uu=%u\n", uu);
        goto error;
        }
            t1->len = vv + L2_INCM;
            for (ww = 0; ww < vv + L2_INCM; ww++)
                ((unsigned int*)t1->p)[ww] = uu * 100 + vv * 10 + ww;
        }
    } /* end for */

    /* Open the file */
    fid = H5Fopen(TESTFL_FILE_NAME, H5F_ACC_RDWR, H5P_DEFAULT);
    if (fid < 0)
    goto error;

    /* Create a VL datatype of an atomic type */
    vlatomic = H5Tvlen_create (H5T_NATIVE_UINT);
    if (vlatomic < 0)
    goto error;

    /* Create a VL datatype of the VL of atomic datatype */
    vlofvl = H5Tvlen_create (vlatomic);
    if (vlofvl < 0)
    goto error;

    /* Create the base compound type */
    comp_vlvl = H5Tcreate(H5T_COMPOUND, sizeof(compVLVL_t));
    if (comp_vlvl < 0)
    goto error;

    /* Insert fields: atomic, atomic, vlen */
    ret = H5Tinsert(comp_vlvl, "u", HOFFSET(compVLVL_t, u), H5T_NATIVE_UINT);
    if (ret < 0)
    goto error;
    ret = H5Tinsert(comp_vlvl, "f", HOFFSET(compVLVL_t, f), H5T_NATIVE_FLOAT);
    if (ret < 0)
    goto error;
    ret = H5Tinsert(comp_vlvl, "v", HOFFSET(compVLVL_t, v), vlofvl);
    if (ret < 0)
    goto error;

    /* Create a packet table that uses a compound datatype of vlen datatype */
    ptable = H5PTcreate_fl(fid, PT_COMP_VLEN, comp_vlvl, (hsize_t)1, 0);

    /* Ensure that PT is created successfully */
    if (ptable == H5I_INVALID_HID)
    goto error;

    /* Release datatypes */
    if (H5Tclose(vlatomic) < 0)
    goto error;
    if (H5Tclose(vlofvl) < 0)
    goto error;
    if (H5Tclose(comp_vlvl) < 0)
    goto error;

    /* Write the entire buffer to the packet table */
    ret = H5PTappend(ptable, (size_t)NRECORDS, writeBuf );
    if (ret < 0)
    goto error;

    /* Get the number of packets in the packet table, should be NRECORDS. */
    ret = H5PTget_num_packets(ptable, &count);
    if (ret < 0)
    goto error;

    HDsprintf(msg, "The number of packets in the packet table must be %u\n", NRECORDS);
    VERIFY(count == NRECORDS, msg);

    /* Read all five packets back */
    ret = H5PTread_packets(ptable, (hsize_t)0, (size_t)NRECORDS, (void*)readBuf );
    if (ret < 0)
    goto error;

    /* Compare data read in */
    for (uu = 0; uu < NRECORDS; uu++) {
        if (writeBuf[uu].u != readBuf[uu].u) {
            HDfprintf(stderr, "Integer components don't match!, writeBuf[%u].u=%u, readBuf[%u].u=%u\n", uu, writeBuf[uu].u, uu, readBuf[uu].u);
            continue;
        } /* end if */
        if (!H5_FLT_ABS_EQUAL(writeBuf[uu].f, readBuf[uu].f)) {
            HDfprintf(stderr, "Float components don't match!, writeBuf[%u].f=%f, readBuf[%u].f=%f\n", uu, (double)writeBuf[uu].f, uu, (double)readBuf[uu].f);
            continue;
        } /* end if */

        if (writeBuf[uu].v.len != readBuf[uu].v.len) {
            HDfprintf(stderr, "%d: VL data length don't match!, writeBuf[%u].v.len=%zu, readBuf[%u].v.len=%zu\n", __LINE__, uu, writeBuf[uu].v.len, uu, readBuf[uu].v.len);
            continue;
        } /* end if */

        for (t1 = (hvl_t *)(writeBuf[uu].v.p), t2 = (hvl_t *)(readBuf[uu].v.p), vv = 0; (size_t)vv < readBuf[uu].v.len; vv++, t1++, t2++) {
            if (t1->len != t2->len) {
                HDfprintf(stderr, "%d: VL data length don't match!, uu=%u, vv=%u, t1->len=%zu, t2->len=%zu\n", __LINE__, uu, vv, t1->len, t2->len);
                continue;
            } /* end if */
            for (ww = 0; (size_t)ww < t2->len; ww++) {
                if (((unsigned int *)t1->p)[ww] != ((unsigned int *)t2->p)[ww] ) {
                    HDfprintf(stderr, "VL data values don't match!, t1->p[%u]=%u, t2->p[%u]=%u\n", ww, ((unsigned int *)t1->p)[ww], ww, ((unsigned int *)t2->p)[ww]);
                    continue;
                } /* end if */
            } /* end for */
        } /* end for */
    } /* end for */

    /* Free the buffers */
    ret = H5PTfree_vlen_buff(ptable, NRECORDS, readBuf);
    if (ret < 0)
    goto error;
    ret = H5PTfree_vlen_buff(ptable, NRECORDS, writeBuf);
    if (ret < 0)
    goto error;

    /* Close the packet table */
    ret = H5PTclose(ptable);
    if (ret < 0)
    goto error;

    /* Close the file */
    if (H5Fclose(fid) < 0)
    goto error;

    PASSED();
    return SUCCEED;

error: /* An error has occurred.  Clean up and exit. */
    if (vlatomic > 0) H5Tclose(vlatomic);
    if (vlofvl > 0) H5Tclose(vlofvl);
    if (comp_vlvl > 0) H5Tclose(comp_vlvl);
    if (H5PTis_valid(ptable) > 0) H5PTclose(ptable);
    if (fid > 0) H5Fclose(fid);
    H5PTfree_vlen_buff(ptable, NRECORDS, readBuf);
    H5PTfree_vlen_buff(ptable, NRECORDS, writeBuf);
    H5_FAILED();
    return FAIL;
} /* testfl_compound_VL_VLtype */

/*-------------------------------------------------------------------------
 * testfl_VLof_VLtype(): Test that a packet table of VL datatype with VL
 *    datatypes of atomic datatypes can be created and written
 *    correctly. (HDFFV-442)
 *
 * 2016/01/27 -BMR
 *-------------------------------------------------------------------------
 */
static int testfl_VLof_VLtype(void)
{
    hid_t   fid=H5I_INVALID_HID;    /* Test file identifier */
    hid_t   ptable=H5I_INVALID_HID;    /* Packet table identifier */
    hid_t   vlatomic=H5I_INVALID_HID;    /* Variable length datatype */
    hid_t   vlofvl=H5I_INVALID_HID;    /* VL datatype of VL datatypes */
    hsize_t count;        /* Number of records in the table */
    hvl_t   *t1;        /* pointer to advance */
    unsigned uu, vv, ww;    /* Loop variables */
    hvl_t   writeBuf[NRECORDS];    /* Buffer to hold data to be written */
    hvl_t   readBuf[NRECORDS];    /* Buffer to hold read data */
    char    msg[80];        /* For error message */
    herr_t  ret;        /* Returned status from a callee */

    HL_TESTING3("        with vlen datatype of vlen datatype");

    /* Allocate and initialize VL data to write (copied from C test) */
    for (uu = 0; uu < NRECORDS; uu++) {
        writeBuf[uu].p = HDmalloc((uu + 1) * sizeof(hvl_t));
        if (writeBuf[uu].p == NULL) {
            HDfprintf(stderr, "Cannot allocate memory for VL data! uu=%u\n", uu);
            goto error;
        } /* end if */
        writeBuf[uu].len = uu + 1;
        for (t1 = (hvl_t *)(writeBuf[uu].p), vv = 0; vv < (uu + 1); vv++, t1++)
    {
            t1->p = HDmalloc((vv + 1) * sizeof(unsigned int));
        if (t1->p == NULL) {
        HDfprintf(stderr, "Cannot allocate memory for VL data! uu=%u\n", uu);
        goto error;
        }
            t1->len = vv + 1;
            for (ww = 0; ww < (vv + 1); ww++)
                ((unsigned int *)t1->p)[ww] = uu * 100 + vv * 10 + ww;
        } /* end for */
    } /* end for */

    /* Open the file */
    fid = H5Fopen(TESTFL_FILE_NAME, H5F_ACC_RDWR, H5P_DEFAULT);
    if (fid < 0)
    goto error;

    /* Create a VL datatype of an atomic type */
    vlatomic = H5Tvlen_create (H5T_NATIVE_UINT);
    if (vlatomic < 0)
    goto error;

    vlofvl = H5Tvlen_create (vlatomic);
    if (vlofvl < 0)
    goto error;

    /* Create a packet table that uses a vlen datatype of vlen datatype */
    ptable = H5PTcreate_fl(fid, PT_VLEN_VLEN, vlofvl, (hsize_t)1, 0);

    /* Ensure that PT is created successfully */
    if (ptable == H5I_INVALID_HID)
    goto error;

    /* Release datatypes */
    if (H5Tclose(vlatomic) < 0)
    goto error;
    if (H5Tclose(vlofvl) < 0)
    goto error;

    /* Write the entire buffer to the packet table */
    ret = H5PTappend(ptable, (size_t)5, writeBuf );
    if (ret < 0)
    goto error;

    /* Get the number of packets in the packet table, should be NRECORDS. */
    ret = H5PTget_num_packets(ptable, &count);
    if (ret < 0)
    goto error;

    HDsprintf(msg, "The number of packets in the packet table must be %u\n", NRECORDS);
    VERIFY(count == NRECORDS, msg);

    /* Read all five packets back */
    ret = H5PTread_packets(ptable, (hsize_t)0, (size_t)5, (void*)readBuf );
    if (ret < 0)
    goto error;

    /* Free the buffers */
    ret = H5PTfree_vlen_buff(ptable, NRECORDS, readBuf);
    if (ret < 0)
    goto error;
    ret = H5PTfree_vlen_buff(ptable, NRECORDS, writeBuf);
    if (ret < 0)
    goto error;

    /* Close the packet table */
    ret = H5PTclose(ptable);
    if (ret < 0)
    goto error;

    /* Close the file */
    if (H5Fclose(fid) < 0)
    goto error;

    PASSED();
    return SUCCEED;

error: /* An error has occurred.  Clean up and exit. */
    if (vlatomic > 0) H5Tclose(vlatomic);
    if (vlofvl > 0) H5Tclose(vlofvl);
    if (H5PTis_valid(ptable) > 0) H5PTclose(ptable);
    if (fid > 0) H5Fclose(fid);
    H5PTfree_vlen_buff(ptable, NRECORDS, readBuf);
    H5PTfree_vlen_buff(ptable, NRECORDS, writeBuf);
    H5_FAILED();
    return FAIL;
} /* testfl_VLof_VLtype */

/*-------------------------------------------------------------------------
 * test_packet_table_with_varlen(): Invokes individual tests to ensure that
 *    packet tables with variable length are created and written correctly
 *    without the specific VL PT functionality. (HDFFV-442)
 *
 * 2016/01/27 -BMR
 *-------------------------------------------------------------------------
 */
int test_packet_table_with_varlen(void)
{
    hid_t fid=H5I_INVALID_HID;        /* File identifier */
    int status = SUCCEED;

    /* Create a file using default properties */
    fid = H5Fcreate(TEST_FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid < 0)
    return FAIL;

    /* Close the file.  The file will be opened by each test function below */
    if (H5Fclose(fid) < 0)
    return FAIL;

    HDputs("Testing packet table with various variable-length datatypes");

    /* If any test fails, move on to subsequent test, but status will indicate
       there is a failure. */

    /* Test variable length of a simple type */
    if (test_VLof_atomic() < 0)
    status = FAIL;

    /* Test variable length of a compound type */
    if (test_VLof_comptype() < 0)
    status = FAIL;

    /* Test compound type with variable length */
    if (test_compound_VL_VLtype() < 0)
    status = FAIL;

    /* Test variable length of a variable length */
    if (test_VLof_VLtype() < 0)
    status = FAIL;

    /* Test variable length of a variable length */
    if (test_H5PTis_varlen() < 0)
    status = FAIL;

    /* Test adding attributes to packet table */
    if (test_attributes() < 0)
    status = FAIL;

    /* Test accessor functions */
    if (test_accessors() < 0)
    status = FAIL;

/**************************************************************************
    Calling test functions for deprecated function H5PTcreate_fl
    Mar 2016, -BMR

**************************************************************************/

    /* Create a file using default properties */
    fid = H5Fcreate(TESTFL_FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fid < 0)
    return FAIL;

    /* Close the file.  The file will be opened by each test function below */
    if (H5Fclose(fid) < 0)
    return FAIL;

    HDputs("Testing packet table with various variable-length datatypes - H5PTcreate_fl");

    /* If any test fails, move on to subsequent test, but status will indicate
       there is a failure. */

    /* Test variable length of a simple type */
    if (testfl_VLof_atomic() < 0)
    status = FAIL;

    /* Test variable length of a compound type */
    if (testfl_VLof_comptype() < 0)
    status = FAIL;

    /* Test compound type with variable length */
    if (testfl_compound_VL_VLtype() < 0)
    status = FAIL;

    /* Test variable length of a variable length */
    if (testfl_VLof_VLtype() < 0)
    status = FAIL;

    return(status);
}
