/*-------------------------------------------------------------------------
 * Copyright (C) 1997   National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:             theap.c
 *                      Jul 17 1997
 *                      Robb Matzke <robb@maya.nuance.com>
 *
 * Purpose:             
 *
 * Modifications:       
 *
 *-------------------------------------------------------------------------
 */
#include <testhdf5.h>

#include <H5private.h>
#include <H5Iprivate.h>
#include <H5ACprivate.h>
#include <H5Pprivate.h>
#include <H5Fprivate.h>
#include <H5Hprivate.h>

#define NOBJS   40

/*-------------------------------------------------------------------------
 * Function:    test_heap
 *
 * Purpose:     Test name and object heaps.
 *
 * Return:      void
 *
 * Programmer:  Robb Matzke
 *              robb@maya.nuance.com
 *              Jul 17 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
test_heap(void)
{
    int                     i, j;
    hid_t                   fid;
    H5F_t                  *f;
    haddr_t                 heap_addr;
    char                    buf[NOBJS + 8];
    const char             *s;
    size_t                  obj[NOBJS];
    herr_t                  status;

    MESSAGE(5, ("Testing Heaps\n"));

    /* Create the file */
    fid = H5Fcreate("theap.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");
    f = H5I_object(fid);
    CHECK(f, NULL, "H5I_object");

    /* Create a new heap */
    status = H5H_create(f, H5H_LOCAL, 0, &heap_addr /*out */ );
    CHECK_I(status, "H5H_new");

    /* Add stuff to the heap */
    for (i = 0; i < NOBJS; i++) {
        sprintf(buf, "%03d-", i);
        for (j = 4; j < i; j++)
            buf[j] = '0' + j % 10;
        if (j > 4)
            buf[j] = '\0';

        obj[i] = H5H_insert(f, &heap_addr, strlen(buf) + 1, buf);
	assert ((size_t)(-1)!=obj[i]);
    }

    /* Flush the cache and invalidate everything */
    H5AC_flush(f, NULL, 0, TRUE);

    /* Read the objects back out */
    for (i = 0; i < NOBJS; i++) {
        s = H5H_peek(f, &heap_addr, obj[i]);
        MESSAGE(8, ("object is `%s'\n", s));
    }

    /* Close the file */
    H5Fclose(fid);
}
