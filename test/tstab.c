/*-------------------------------------------------------------------------
 * Copyright (C) 1997   National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:             tstab.c
 *                      Aug  7 1997
 *                      Robb Matzke <matzke@llnl.gov>
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
#include <H5Gprivate.h>
#include <H5Oprivate.h>

/*
 * This file needs to access private datatypes from the H5G package.
 */
#define H5G_PACKAGE
#include <H5Gpkg.h>

/*-------------------------------------------------------------------------
 * Function:    test_2
 *
 * Purpose:     Creates a really large directory.
 *
 * Return:      void
 *
 * Programmer:  Robb Matzke
 *              robb@maya.nuance.com
 *              Aug 29 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
test_2(void)
{
    hid_t                   fid, create_plist, access_plist, dir;
    H5F_t                  *f;
    int                     i;
    char                    name[256];
    herr_t                  status;
    int                     nsyms = 5000;

    MESSAGE(2, ("........large directories\n"));

    /*
     * Use larger symbol table data structures to be more efficient, use
     * defaults to bang harder on the library for testing.
     */
    create_plist = H5Pcreate(H5P_FILE_CREATE);
    H5Pset_sym_k(create_plist, 16, 16);

    /*
     * File access property list.
     */
    access_plist = H5Pcreate (H5P_FILE_ACCESS);

#if 0
    /* Try temporary core files */
    H5Cset_core (access_plist, 3000000);
#elif 0
    /* Try a default split file but with our own name extensions */
    H5Cset_split (access_plist, ".XX1", H5C_DEFAULT, ".XX2", H5C_DEFAULT);
#elif 0
    {
	/* Try a split file with an in-core meta data part */
	hid_t meta_access = H5Ccreate (H5C_FILE_ACCESS);
	H5Cset_core (meta_access, 1024*1024);
	H5Cset_split (access_plist, NULL, meta_access, NULL, H5C_DEFAULT);
    }
#elif 0
    {
	/* Try a split file with an in-core raw data part */
	hid_t raw_access = H5Ccreate (H5C_FILE_ACCESS);
	H5Cset_core (raw_access, 1024*1024);
	H5Cset_split (access_plist, NULL, H5C_DEFAULT, NULL, raw_access);
    }
#endif

    /* create the file */
    fid = H5Fcreate("tstab2.h5", H5F_ACC_TRUNC, create_plist, access_plist);
    CHECK(fid, FAIL, "H5Fcreate");
    f = H5I_object(fid);
    CHECK(f, NULL, "H5I_object");
    f->intent |= H5F_ACC_DEBUG;

    /*
     * Create a directory that has so many entries that the root
     * of the B-tree ends up splitting.
     */
    dir = H5Gcreate(fid, "/big", (size_t)nsyms*16+2);
    CHECK_I(dir, "H5Gcreate");
    status = H5Gclose(dir);
    CHECK_I(status, "H5Gclose");
    status = H5Gset(fid, "/big");
    CHECK_I(status, "H5Gset");

    for (i = 0; i < nsyms; i++) {
        sprintf(name, "%05d%05d", rand() % 100000, i);
        MESSAGE(8, ("%s\n", name));
        dir = H5Gcreate(fid, name, 0);
        CHECK_I(dir, "H5Gcreate");
        status = H5Gclose(dir);
        CHECK_I(status, "H5Gclose");
    }

    /* close the file */
    status = H5Fclose(fid);
    CHECK_I(status, "H5Fclose");
}

/*-------------------------------------------------------------------------
 * Function:    test_stab
 *
 * Purpose:     Test symbol tables
 *
 * Return:      void
 *
 * Programmer:  Robb Matzke
 *              matzke@viper.llnl.gov
 *              Aug  7 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
test_stab(void)
{
    test_2();
}
