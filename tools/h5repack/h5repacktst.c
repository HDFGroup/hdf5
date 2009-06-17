/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* Copyright by The HDF Group.                                               *
* Copyright by the Board of Trustees of the University of Illinois.         *
* All rights reserved.                                                      *
*                                                                           *
* This file is part of HDF5.  The full HDF5 copyright notice, including     *
* terms governing use, modification, and redistribution, is contained in    *
* the files COPYING and Copyright.html.  COPYING can be found at the root   *
* of the source code distribution tree; Copyright.html can be found at the  *
* root level of an installed copy of the electronic HDF5 document set and   *
* is linked from the top-level documents page.  It can also be found at     *
* http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
* access to either file, you may request a copy from help@hdfgroup.org.     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "h5repack.h"
#include "h5test.h"
#include "h5diff.h"
#include "h5tools.h"

#define GOERROR  {H5_FAILED(); goto error;}


/* fill value test */
#define FNAME0     "h5repack_fill.h5"
#define FNAME0OUT  "h5repack_fill_out.h5"
/* HDF5 objects and all dataset datatypes */
#define FNAME1     "h5repack_objs.h5"
#define FNAME1OUT  "h5repack_objs_out.h5"
/* attributes, all datatypes  */
#define FNAME2     "h5repack_attr.h5"
#define FNAME2OUT  "h5repack_attr_out.h5"
/* hard links  */
#define FNAME3     "h5repack_hlink.h5"
#define FNAME3OUT  "h5repack_hlink_out.h5"
/* layout  */
#define FNAME4     "h5repack_layout.h5"
#define FNAME4OUT  "h5repack_layout_out.h5"
/* H5D_ALLOC_TIME_EARLY  */
#define FNAME5     "h5repack_early.h5"
#define FNAME5OUT  "h5repack_early_out.h5"
#define FNAME6     "h5repack_early2.h5"
/* SZIP filter  */
#define FNAME7     "h5repack_szip.h5"
#define FNAME7OUT  "h5repack_szip_out.h5"
/* GZIP filter  */
#define FNAME8     "h5repack_deflate.h5"
#define FNAME8OUT  "h5repack_deflate_out.h5"
/* GZIP filter  */
#define FNAME9     "h5repack_shuffle.h5"
#define FNAME9OUT  "h5repack_shuffle_out.h5"
/* Fletcher filter  */
#define FNAME10    "h5repack_fletcher.h5"
#define FNAME10OUT "h5repack_fletcher_out.h5"
/* All filters  */
#define FNAME11    "h5repack_filters.h5"
#define FNAME11OUT "h5repack_filters_out.h5"

/* Big file to test read by hyperslabs  */
#define FNAME14    "h5repack_big.h5"
#define FNAME14OUT "h5repack_big_out.h5"

/* external file  */
#define FNAME15    "h5repack_ext.h5"
#define FNAME15OUT "h5repack_ext_out.h5"

/* File w/userblock */
#define FNAME16    "h5repack_ub.h5"
#define FNAME16OUT "h5repack_ub_out.h5"

#define FNAME_UB   "ublock.bin"


const char *H5REPACK_FILENAMES[] = {
    "h5repack_big_out",
    NULL
};

#define H5REPACK_EXTFILE "h5repack_ext.bin"

const char *progname = "h5repacktst";
int d_status = EXIT_SUCCESS;


#define DIM1    40
#define DIM2    20
#define CDIM1   DIM1/2
#define CDIM2   DIM2/2
#define RANK    2

/* Size of userblock (for userblock test) */
#define USERBLOCK_SIZE  2048


/*-------------------------------------------------------------------------
* prototypes
*-------------------------------------------------------------------------
*/
int make_all_objects(hid_t loc_id);
int make_attributes(hid_t loc_id);
int make_hlinks(hid_t loc_id);
int make_early(void);
int make_layout(hid_t loc_id);
#ifdef H5_HAVE_FILTER_SZIP
int make_szip(hid_t loc_id);
#endif /* H5_HAVE_FILTER_SZIP */
int make_deflate(hid_t loc_id);
int make_shuffle(hid_t loc_id);
int make_fletcher32(hid_t loc_id);

int make_all(hid_t loc_id);
int make_fill(hid_t loc_id);
int make_big(hid_t loc_id);
int make_testfiles(void);
void write_dset_in(hid_t loc_id,const char* dset_name,hid_t file_id,int make_diffs );
void write_attr_in(hid_t loc_id,const char* dset_name,hid_t fid,int make_diffs );
int write_dset(hid_t loc_id,int rank,hsize_t *dims,const char *dset_name,hid_t type_id,void *buf );
int make_dset(hid_t loc_id,const char *name,hid_t sid,hid_t dcpl,void *buf);
int make_attr(hid_t loc_id,int rank,hsize_t *dims,const char *attr_name,hid_t type_id,void *buf);
void make_dset_reg_ref(hid_t loc_id);
int make_external(hid_t loc_id);
static int make_userblock(void);
static int verify_userblock( const char* filename);
static int make_userblock_file(void);



/*-------------------------------------------------------------------------
* Function: main
*
* Purpose: Executes h5repack tests
*
* Return: Success: zero
*  Failure: 1
*
* Programmer:  Pedro Vicente <pvn@ncsa.uiuc.edu>
*             January, 6, 2004
*
*-------------------------------------------------------------------------
*/

int main (void)
{
    pack_opt_t  pack_options;
    diff_opt_t  diff_options;
#if defined (H5_HAVE_FILTER_SZIP)
    int szip_can_encode = 0;
#endif

    /* initialize */
    memset(&diff_options, 0, sizeof (diff_opt_t));
    memset(&pack_options, 0, sizeof (pack_opt_t));

    /* run tests  */
    puts("Testing h5repack:");

    /* make the test files */
    TESTING("    generating datasets");
    if (make_testfiles()<0)
        GOERROR;
    PASSED();

    /*-------------------------------------------------------------------------
    * Format of the tests:
    *
    * 1) make a copy of the file with h5repack
    * 2) use the h5diff utility to compare the input and output file;
    *     it returns RET==0 if the objects have the same data
    *-------------------------------------------------------------------------
    */


    /*-------------------------------------------------------------------------
    * file with fill values
    *-------------------------------------------------------------------------
    */
    TESTING("    copy of datasets (fill values)");
    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack(FNAME0,FNAME0OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME0,FNAME0OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME0OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_cmpdcpl(FNAME0,FNAME0OUT)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;
    PASSED();

    /*-------------------------------------------------------------------------
    * file with all kinds of dataset datatypes
    *-------------------------------------------------------------------------
    */
    TESTING("    copy of datasets (all datatypes)");
    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack(FNAME1,FNAME1OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME1,FNAME1OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME1OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_cmpdcpl(FNAME1,FNAME1OUT)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;
    PASSED();


    /*-------------------------------------------------------------------------
    * file with attributes
    *-------------------------------------------------------------------------
    */
    TESTING("    copy of datasets (attributes)");
    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack(FNAME2,FNAME2OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME2,FNAME2OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME2OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_cmpdcpl(FNAME2,FNAME2OUT)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;
    PASSED();

    /*-------------------------------------------------------------------------
    * file with hardlinks
    *-------------------------------------------------------------------------
    */
    TESTING("    copy of datasets (hardlinks)");
    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack(FNAME3,FNAME3OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME3,FNAME3OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME3OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_cmpdcpl(FNAME3,FNAME3OUT)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;

    PASSED();

    /*-------------------------------------------------------------------------
    * alloc early test
    *-------------------------------------------------------------------------
    */
    TESTING("    copy of allocation early file");
    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack(FNAME5,FNAME5OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME5,FNAME5OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME5OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;
    PASSED();

    /*-------------------------------------------------------------------------
    * the remaining files differ in the dcpl's
    *-------------------------------------------------------------------------
    */

    /*-------------------------------------------------------------------------
    * deflate
    *-------------------------------------------------------------------------
    */
    TESTING("    adding deflate filter");

#ifdef H5_HAVE_FILTER_DEFLATE

    /*-------------------------------------------------------------------------
    * test an individual object option
    *-------------------------------------------------------------------------
    */

    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack_addfilter("dset1:GZIP=9",&pack_options)<0)
        GOERROR;
    if (h5repack_addlayout("dset1:CHUNK=20x10",&pack_options)<0)
        GOERROR;
    if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;
    PASSED();
#else
    SKIPPED();
#endif

    /*-------------------------------------------------------------------------
    * test all objects option
    *-------------------------------------------------------------------------
    */

    TESTING("    adding deflate filter to all");

#ifdef H5_HAVE_FILTER_DEFLATE

    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack_addfilter("GZIP=1",&pack_options)<0)
        GOERROR;
    if (h5repack_addlayout("CHUNK=20x10",&pack_options)<0)
        GOERROR;
    if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;

    PASSED();
#else
    SKIPPED();
#endif

    /*-------------------------------------------------------------------------
    * SZIP
    *-------------------------------------------------------------------------
    */

    TESTING("    adding szip filter");

#if defined (H5_HAVE_FILTER_SZIP)
    if (h5tools_can_encode(H5Z_FILTER_SZIP) == 1) {
        szip_can_encode = 1;
    }

    /*-------------------------------------------------------------------------
    * test an individual object option
    *-------------------------------------------------------------------------
    */

    if (szip_can_encode) {
        if (h5repack_init (&pack_options, 0)<0)
            GOERROR;
        if (h5repack_addfilter("dset2:SZIP=8,EC",&pack_options)<0)
            GOERROR;
        if (h5repack_addlayout("dset2:CHUNK=20x10",&pack_options)<0)
            GOERROR;
        if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
            GOERROR;
        if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) > 0)
            GOERROR;
        if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
            GOERROR;
        if (h5repack_end (&pack_options)<0)
            GOERROR;

        PASSED();
    } else {
        SKIPPED();
    }
#else
    SKIPPED();
#endif


    /*-------------------------------------------------------------------------
    * test all objects option
    *-------------------------------------------------------------------------
    */
    TESTING("    adding szip filter to all");

#if defined (H5_HAVE_FILTER_SZIP)
    if (szip_can_encode) {
        if (h5repack_init (&pack_options, 0)<0)
            GOERROR;
        if (h5repack_addfilter("SZIP=8,NN",&pack_options)<0)
            GOERROR;
        if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
            GOERROR;
        if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) > 0)
            GOERROR;
        if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
            GOERROR;
        if (h5repack_end (&pack_options)<0)
            GOERROR;

        PASSED();
    } else {
        SKIPPED();
    }
#else
    SKIPPED();
#endif


    TESTING("    addding shuffle filter");

#ifdef H5_HAVE_FILTER_SHUFFLE

    /*-------------------------------------------------------------------------
    * test an individual object option
    *-------------------------------------------------------------------------
    */

    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack_addfilter("dset1:SHUF",&pack_options)<0)
        GOERROR;
    if (h5repack_addlayout("dset1:CHUNK=20x10",&pack_options)<0)
        GOERROR;
    if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;

    PASSED();
#else
    SKIPPED();
#endif

    /*-------------------------------------------------------------------------
    * test all objects option
    *-------------------------------------------------------------------------
    */

    TESTING("    addding shuffle filter to all");

#ifdef H5_HAVE_FILTER_SHUFFLE

    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack_addfilter("SHUF",&pack_options)<0)
        GOERROR;
    if (h5repack_addlayout("CHUNK=20x10",&pack_options)<0)
        GOERROR;
    if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;

    PASSED();
#else
    SKIPPED();
#endif

    TESTING("    adding checksum filter");

#ifdef H5_HAVE_FILTER_FLETCHER32

    /*-------------------------------------------------------------------------
    * test an individual object option
    *-------------------------------------------------------------------------
    */

    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack_addfilter("dset1:FLET",&pack_options)<0)
        GOERROR;
    if (h5repack_addlayout("dset1:CHUNK=20x10",&pack_options)<0)
        GOERROR;
    if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;

    PASSED();
#else
    SKIPPED();
#endif

    /*-------------------------------------------------------------------------
    * test all objects option
    *-------------------------------------------------------------------------
    */


    TESTING("    adding checksum filter to all");

#ifdef H5_HAVE_FILTER_FLETCHER32

    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack_addfilter("FLET",&pack_options)<0)
        GOERROR;
    if (h5repack_addlayout("CHUNK=20x10",&pack_options)<0)
        GOERROR;
    if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;

    PASSED();
#else
    SKIPPED();
#endif


    TESTING("    filter queue fletcher, shuffle, deflate, szip");

    /*-------------------------------------------------------------------------
    * add some filters
    *-------------------------------------------------------------------------
    */

    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack_addlayout("dset1:CHUNK 20x10",&pack_options)<0)
        GOERROR;

#if defined (H5_HAVE_FILTER_FLETCHER32)
    if (h5repack_addfilter("dset1:FLET",&pack_options)<0)
        GOERROR;
#endif

#ifdef H5_HAVE_FILTER_SHUFFLE
    if (h5repack_addfilter("dset1:SHUF",&pack_options)<0)
        GOERROR;
#endif

#if defined (H5_HAVE_FILTER_SZIP)
    if (szip_can_encode) {
        if (h5repack_addfilter("dset1:SZIP=8,NN",&pack_options)<0)
            GOERROR;
    }
#endif

#ifdef H5_HAVE_FILTER_DEFLATE
    if (h5repack_addfilter("dset1:GZIP=1",&pack_options)<0)
        GOERROR;
#endif

    if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;

    PASSED();


    TESTING("    adding layout chunked");

    /*-------------------------------------------------------------------------
    * test an individual object option
    *-------------------------------------------------------------------------
    */

    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack_addlayout("dset1:CHUNK=20x10",&pack_options)<0)
        GOERROR;
    if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;
    PASSED();

    /*-------------------------------------------------------------------------
    * test all objects option
    *-------------------------------------------------------------------------
    */
    TESTING("    adding layout chunked to all");

    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack_addlayout("CHUNK=20x10",&pack_options)<0)
        GOERROR;
    if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;

    PASSED();

    TESTING("    adding layout contiguous");

    /*-------------------------------------------------------------------------
    * test an individual object option
    *-------------------------------------------------------------------------
    */
    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack_addlayout("dset1:CONTI",&pack_options)<0)
        GOERROR;
    if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;

    PASSED();

    TESTING("    adding layout contiguous to all");

    /*-------------------------------------------------------------------------
    * test all objects option
    *-------------------------------------------------------------------------
    */
    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack_addlayout("CONTI",&pack_options)<0)
        GOERROR;
    if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;

    PASSED();

    /*-------------------------------------------------------------------------
    * do the same test for a file with filters (chunked)
    *-------------------------------------------------------------------------
    */
    if (h5repack_init (&pack_options, 0) < 0)
        GOERROR;
    if (h5repack_addlayout("CONTI",&pack_options) < 0)
        GOERROR;
    if (h5repack(FNAME8,FNAME8OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME8,FNAME8OUT,NULL,NULL,&diff_options) >0)
        GOERROR;
    if (h5repack_verify(FNAME8OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options) < 0)
        GOERROR;





    TESTING("    adding layout compact");

    /*-------------------------------------------------------------------------
    * test an individual object option
    *-------------------------------------------------------------------------
    */

    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack_addlayout("dset1:COMPA",&pack_options)<0)
        GOERROR;
    if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;
    PASSED();

    TESTING("    adding layout compact to all");

    /*-------------------------------------------------------------------------
    * test all objects option
    *-------------------------------------------------------------------------
    */

    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack_addlayout("COMPA",&pack_options)<0)
        GOERROR;
    if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;

    PASSED();


    TESTING("    layout compact to contiguous conversion");

    /*-------------------------------------------------------------------------
    * layout compact to contiguous conversion
    *-------------------------------------------------------------------------
    */
    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack_addlayout("dset_compact:CONTI",&pack_options)<0)
        GOERROR;
    if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;
    PASSED();

    TESTING("    layout compact to chunk conversion");

    /*-------------------------------------------------------------------------
    * layout compact to chunk conversion
    *-------------------------------------------------------------------------
    */
    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack_addlayout("dset_compact:CHUNK=2x5",&pack_options)<0)
        GOERROR;
    if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;
    PASSED();

    TESTING("    layout compact to compact conversion");

    /*-------------------------------------------------------------------------
    * layout compact to compact conversion
    *-------------------------------------------------------------------------
    */
    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack_addlayout("dset_compact:COMPA",&pack_options)<0)
        GOERROR;
    if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;
    PASSED();

    TESTING("    layout contiguous to compact conversion");
    /*-------------------------------------------------------------------------
    * layout contiguous to compact conversion
    *-------------------------------------------------------------------------
    */
    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack_addlayout("dset_contiguous:COMPA",&pack_options)<0)
        GOERROR;
    if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;
    PASSED();

    TESTING("    layout contiguous to chunk conversion");
    /*-------------------------------------------------------------------------
    * layout contiguous to chunk conversion
    *-------------------------------------------------------------------------
    */
    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack_addlayout("dset_contiguous:CHUNK=3x6",&pack_options)<0)
        GOERROR;
    if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;
    PASSED();

    TESTING("    layout contiguous to contiguous conversion");

    /*-------------------------------------------------------------------------
    * layout contiguous to contiguous conversion
    *-------------------------------------------------------------------------
    */
    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack_addlayout("dset_contiguous:CONTI",&pack_options)<0)
        GOERROR;
    if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;
    PASSED();

    TESTING("    layout chunked to compact conversion");
    /*-------------------------------------------------------------------------
    * layout chunked to compact conversion
    *-------------------------------------------------------------------------
    */
    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack_addlayout("dset_chunk:COMPA",&pack_options)<0)
        GOERROR;
    if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;
    PASSED();

    TESTING("    layout chunked to contiguous conversion");

    /*-------------------------------------------------------------------------
    * layout chunked to contiguous conversion
    *-------------------------------------------------------------------------
    */
    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack_addlayout("dset_chunk:CONTI",&pack_options)<0)
        GOERROR;
    if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;
    PASSED();

    TESTING("    layout chunked to chunk conversion");
    /*-------------------------------------------------------------------------
    * layout chunked to chunked conversion
    *-------------------------------------------------------------------------
    */
    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack_addlayout("dset_chunk:CHUNK=18x13",&pack_options)<0)
        GOERROR;
    if (h5repack(FNAME4,FNAME4OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME4,FNAME4OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME4OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;
    PASSED();


    /*-------------------------------------------------------------------------
    * the following tests assume the input files have filters
    * FNAME7     
    * FNAME8     
    * FNAME9     
    * FNAME10    
    * FNAME11    
    *-------------------------------------------------------------------------
    */


    TESTING("    copy of szip filter");

#if defined (H5_HAVE_FILTER_SZIP)
    if (szip_can_encode) {
        if (h5repack_init (&pack_options, 0)<0)
            GOERROR;
        if (h5repack(FNAME7,FNAME7OUT,&pack_options) < 0)
            GOERROR;
        if (h5diff(FNAME7,FNAME7OUT,NULL,NULL,&diff_options) > 0)
            GOERROR;
        if (h5repack_verify(FNAME7OUT,&pack_options)<=0)
            GOERROR;
        if (h5repack_end (&pack_options)<0)
            GOERROR;

        PASSED();
    } else {
        SKIPPED();
    }
#else
    SKIPPED();
#endif

    TESTING("    removing szip filter");

#if defined (H5_HAVE_FILTER_SZIP)
    if (szip_can_encode) {
        if (h5repack_init (&pack_options, 0)<0)
            GOERROR;
        if (h5repack_addfilter("dset_szip:NONE",&pack_options)<0)
            GOERROR;
        if (h5repack(FNAME7,FNAME7OUT,&pack_options) < 0)
            GOERROR;
        if (h5diff(FNAME7,FNAME7OUT,NULL,NULL,&diff_options) > 0)
            GOERROR;
        if (h5repack_verify(FNAME7OUT,&pack_options)<=0)
            GOERROR;
        if (h5repack_end (&pack_options)<0)
            GOERROR;

        PASSED();
    } else {
        SKIPPED();
    }
#else
    SKIPPED();
#endif


    TESTING("    copy of deflate filter");

#ifdef H5_HAVE_FILTER_DEFLATE
    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack(FNAME8,FNAME8OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME8,FNAME8OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME8OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;

    PASSED();
#else
    SKIPPED();
#endif


    TESTING("    removing deflate filter");

#ifdef H5_HAVE_FILTER_DEFLATE
    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack_addfilter("dset_deflate:NONE",&pack_options)<0)
        GOERROR;
    if (h5repack(FNAME8,FNAME8OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME8,FNAME8OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME8OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;

    PASSED();
#else
    SKIPPED();
#endif



    TESTING("    copy of shuffle filter");

#ifdef H5_HAVE_FILTER_SHUFFLE
    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack(FNAME9,FNAME9OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME9,FNAME9OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME9OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;

    PASSED();
#else
    SKIPPED();
#endif

    TESTING("    removing shuffle filter");

#ifdef H5_HAVE_FILTER_SHUFFLE
    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack_addfilter("dset_shuffle:NONE",&pack_options)<0)
        GOERROR;
    if (h5repack(FNAME9,FNAME9OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME9,FNAME9OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME9OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;

    PASSED();
#else
    SKIPPED();
#endif

    TESTING("    copy of fletcher filter");

#ifdef H5_HAVE_FILTER_FLETCHER32
    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack(FNAME10,FNAME10OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME10,FNAME10OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME10OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;

    PASSED();
#else
    SKIPPED();
#endif

    TESTING("    removing fletcher filter");

#ifdef H5_HAVE_FILTER_FLETCHER32
    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack_addfilter("dset_fletcher32:NONE",&pack_options)<0)
        GOERROR;
    if (h5repack(FNAME10,FNAME10OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME10,FNAME10OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME10OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;

    PASSED();
#else
    SKIPPED();
#endif



    /*-------------------------------------------------------------------------
    * file with all filters
    *  dset_all
    *  dset_deflate
    *  dset_szip
    *  dset_shuffle
    *  dset_fletcher32
    *-------------------------------------------------------------------------
    */


    TESTING("    filter conversion from deflate to szip");

#if defined (H5_HAVE_FILTER_SZIP) \
    && defined (H5_HAVE_FILTER_DEFLATE) \
    && defined (H5_HAVE_FILTER_FLETCHER32) && defined (H5_HAVE_FILTER_SHUFFLE)

    if (szip_can_encode) {
        if (h5repack_init (&pack_options, 0)<0)
            GOERROR;
        if (h5repack_addfilter("dset_deflate:SZIP=8,NN",&pack_options)<0)
            GOERROR;
        if (h5repack(FNAME11,FNAME11OUT,&pack_options) < 0)
            GOERROR;
        if (h5diff(FNAME11,FNAME11OUT,NULL,NULL,&diff_options) > 0)
            GOERROR;
        if (h5repack_verify(FNAME11OUT,&pack_options)<=0)
            GOERROR;
        if (h5repack_end (&pack_options)<0)
            GOERROR;

        PASSED();
    } else {
        SKIPPED();
    }
#else
    SKIPPED();
#endif

    TESTING("    filter conversion from szip to deflate");

#if defined (H5_HAVE_FILTER_SZIP) \
    && defined (H5_HAVE_FILTER_DEFLATE) \
    && defined (H5_HAVE_FILTER_FLETCHER32) && defined (H5_HAVE_FILTER_SHUFFLE)

    if (szip_can_encode) {
        if (h5repack_init (&pack_options, 0)<0)
            GOERROR;
        if (h5repack_addfilter("dset_szip:GZIP=1",&pack_options)<0)
            GOERROR;
        if (h5repack(FNAME11,FNAME11OUT,&pack_options) < 0)
            GOERROR;
        if (h5diff(FNAME11,FNAME11OUT,NULL,NULL,&diff_options) > 0)
            GOERROR;
        if (h5repack_verify(FNAME11OUT,&pack_options)<=0)
            GOERROR;
        if (h5repack_end (&pack_options)<0)
            GOERROR;

        PASSED();
    } else {
        SKIPPED();
    }
#else
    SKIPPED();
#endif


    /*-------------------------------------------------------------------------
    * test the NONE global option
    *-------------------------------------------------------------------------
    */

    TESTING("    removing all filters");

#if defined (H5_HAVE_FILTER_SZIP) && defined (H5_HAVE_FILTER_DEFLATE) \
    && defined (H5_HAVE_FILTER_FLETCHER32) && defined (H5_HAVE_FILTER_SHUFFLE)

    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack_addfilter("NONE",&pack_options)<0)
        GOERROR;
    if (h5repack(FNAME11,FNAME11OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME11,FNAME11OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME11OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;

    PASSED();
#else
    SKIPPED();
#endif

    /*-------------------------------------------------------------------------
    * test a big file
    *-------------------------------------------------------------------------
    */
    TESTING("    big file");
    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack(FNAME14,FNAME14OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME14,FNAME14OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME14OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;
    PASSED();

    /*-------------------------------------------------------------------------
    * test external dataset
    *-------------------------------------------------------------------------
    */
    TESTING("    external datasets");
    if (h5repack_init (&pack_options, 0)<0)
        GOERROR;
    if (h5repack(FNAME15,FNAME15OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME15,FNAME15OUT,NULL,NULL,&diff_options) > 0)
        GOERROR;
    if (h5repack_verify(FNAME15OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options)<0)
        GOERROR;
    PASSED();

    /*-------------------------------------------------------------------------
    * test several global filters
    *-------------------------------------------------------------------------
    */

    TESTING("    several global filters");

#if defined (H5_HAVE_FILTER_DEFLATE) && defined (H5_HAVE_FILTER_SHUFFLE) 

    if (h5repack_init (&pack_options, 0) < 0)
        GOERROR;
    if (h5repack_addfilter("GZIP=1",&pack_options) < 0)
        GOERROR;
    if (h5repack_addfilter("SHUF",&pack_options) < 0)
        GOERROR;
    if (h5repack(FNAME11,FNAME11OUT,&pack_options) < 0)
        GOERROR;
    if (h5diff(FNAME11,FNAME11OUT,NULL,NULL,&diff_options) >0)
        GOERROR;
    if (h5repack_verify(FNAME11OUT,&pack_options)<=0)
        GOERROR;
    if (h5repack_end (&pack_options) < 0)
        GOERROR;

    PASSED();
#else
    SKIPPED();
#endif

    /*-------------------------------------------------------------------------
    * test file with userblock
    *-------------------------------------------------------------------------
    */
    TESTING("    file with userblock");
    if(h5repack_init(&pack_options, 0) < 0)
        GOERROR;
    if(h5repack(FNAME16, FNAME16OUT, &pack_options) < 0)
        GOERROR;
    if(h5diff(FNAME16, FNAME16OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if(h5repack_verify(FNAME16OUT, &pack_options) <= 0)
        GOERROR;
    if(verify_userblock(FNAME16OUT) < 0)
        GOERROR;
    if(h5repack_end(&pack_options) < 0)
        GOERROR;
    PASSED();

    /*-------------------------------------------------------------------------
    * test file with userblock
    *-------------------------------------------------------------------------
    */
    TESTING("    file with added userblock");
    if(h5repack_init(&pack_options, 0) < 0)
        GOERROR;

    /* add the options for a user block size and user block filename */
    pack_options.ublock_size = USERBLOCK_SIZE;
    pack_options.ublock_filename = FNAME_UB;

    if(h5repack(FNAME8, FNAME8OUT, &pack_options) < 0)
        GOERROR;
    if(h5diff(FNAME8, FNAME8OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if(h5repack_verify(FNAME8OUT, &pack_options) <= 0)
        GOERROR;
    if(verify_userblock(FNAME8OUT) < 0)
        GOERROR;
    if(h5repack_end(&pack_options) < 0)
        GOERROR;
    PASSED();

    /*-------------------------------------------------------------------------
    * test file with aligment
    *-------------------------------------------------------------------------
    */
    TESTING("    file with aligment");

#ifdef H5_HAVE_FILTER_DEFLATE

    if(h5repack_init(&pack_options, 0) < 0)
        GOERROR;

    /* add the options for aligment */
    pack_options.alignment = 1;
    pack_options.threshold = 1;

    if(h5repack(FNAME8, FNAME8OUT, &pack_options) < 0)
        GOERROR;
    if(h5diff(FNAME8, FNAME8OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if(h5repack_verify(FNAME8OUT, &pack_options) <= 0)
        GOERROR;


    /* verify aligment */
    {
        hsize_t threshold;
        hsize_t alignment;
        hid_t fapl;
        hid_t fid;

        if (( fid = H5Fopen(FNAME8OUT, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0 )
            GOERROR;
        if ((fapl = H5Fget_access_plist(fid)) < 0)
            GOERROR;
        if ( H5Pget_alignment(fapl, &threshold, &alignment  )  < 0)
            GOERROR;
        if ( threshold != 1 )
            GOERROR;
        if ( alignment != 1 )
            GOERROR;
        if ( H5Pclose(fapl) < 0)
            GOERROR;
        if (H5Fclose(fid) < 0)
            GOERROR;

    }


    if(h5repack_end(&pack_options) < 0)
        GOERROR;


    PASSED();
#else
    SKIPPED();
#endif




    /*-------------------------------------------------------------------------
    * clean temporary test files
    *-------------------------------------------------------------------------
    */
    {
        hid_t       fapl;

        /* setup */
        h5_reset();
        fapl = h5_fileaccess();
        h5_cleanup(H5REPACK_FILENAMES, fapl); 

    }

    puts("All h5repack tests passed.");

    return 0;

error:
    puts("***** H5REPACK TESTS FAILED *****");
    return 1;

}


/*-------------------------------------------------------------------------
* Function: make_testfiles
*
* Purpose: make a test file with all types of HDF5 objects,
*   datatypes and filters
*
*-------------------------------------------------------------------------
*/
int make_testfiles(void)
{
    hid_t  loc_id;  /* file ID */


    /*-------------------------------------------------------------------------
    * create a file for general copy test
    *-------------------------------------------------------------------------
    */
    if((loc_id = H5Fcreate(FNAME0,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
        return -1;
    if (make_fill(loc_id)<0)
        goto out;
    if(H5Fclose(loc_id)<0)
        return -1;

    /*-------------------------------------------------------------------------
    * create another file for general copy test (all datatypes)
    *-------------------------------------------------------------------------
    */
    if((loc_id = H5Fcreate(FNAME1,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
        return -1;
    if (make_all_objects(loc_id)<0)
        goto out;
    if(H5Fclose(loc_id)<0)
        return -1;

    /*-------------------------------------------------------------------------
    * create a file for attributes copy test
    *-------------------------------------------------------------------------
    */
    if((loc_id = H5Fcreate(FNAME2,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
        return -1;
    if (make_attributes(loc_id)<0)
        goto out;
    if(H5Fclose(loc_id)<0)
        return -1;
    /*-------------------------------------------------------------------------
    * create a file for hard links test
    *-------------------------------------------------------------------------
    */
    if((loc_id = H5Fcreate(FNAME3,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
        return -1;
    if (make_hlinks(loc_id)<0)
        goto out;
    if(H5Fclose(loc_id)<0)
        return -1;
    /*-------------------------------------------------------------------------
    * create a file for layouts test
    *-------------------------------------------------------------------------
    */
    if((loc_id = H5Fcreate(FNAME4,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
        return -1;
    if (make_layout(loc_id)<0)
        goto out;
    if(H5Fclose(loc_id)<0)
        return -1;

    /*-------------------------------------------------------------------------
    * create a file for the H5D_ALLOC_TIME_EARLY test
    *-------------------------------------------------------------------------
    */
    if (make_early()<0)
        goto out;

    /*-------------------------------------------------------------------------
    * create a file with the SZIP filter
    *-------------------------------------------------------------------------
    */
#ifdef H5_HAVE_FILTER_SZIP
    if((loc_id = H5Fcreate(FNAME7,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
        return -1;
    if (make_szip(loc_id)<0)
        goto out;
    if(H5Fclose(loc_id)<0)
        return -1;
#endif /* H5_HAVE_FILTER_SZIP */

    /*-------------------------------------------------------------------------
    * create a file with the deflate filter
    *-------------------------------------------------------------------------
    */
    if((loc_id = H5Fcreate(FNAME8,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
        return -1;
    if (make_deflate(loc_id)<0)
        goto out;
    if(H5Fclose(loc_id)<0)
        return -1;

    /*-------------------------------------------------------------------------
    * create a file with the shuffle filter
    *-------------------------------------------------------------------------
    */
    if((loc_id = H5Fcreate(FNAME9,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
        return -1;
    if (make_shuffle(loc_id)<0)
        goto out;
    if(H5Fclose(loc_id)<0)
        return -1;

    /*-------------------------------------------------------------------------
    * create a file with the fletcher32 filter
    *-------------------------------------------------------------------------
    */
    if((loc_id = H5Fcreate(FNAME10,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
        return -1;
    if (make_fletcher32(loc_id)<0)
        goto out;
    if(H5Fclose(loc_id)<0)
        return -1;

    /*-------------------------------------------------------------------------
    * create a file with all the filters
    *-------------------------------------------------------------------------
    */
    if((loc_id = H5Fcreate(FNAME11,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
        return -1;
    if (make_all(loc_id)<0)
        goto out;
    if(H5Fclose(loc_id)<0)
        return -1;


    /*-------------------------------------------------------------------------
    * create a big dataset
    *-------------------------------------------------------------------------
    */
    if((loc_id = H5Fcreate(FNAME14,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
        return -1;
    if (make_big(loc_id)<0)
        goto out;
    if(H5Fclose(loc_id)<0)
        return -1;

    /*-------------------------------------------------------------------------
    * create a file with external dataset
    *-------------------------------------------------------------------------
    */
    if((loc_id = H5Fcreate(FNAME15,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
        return -1;
    if (make_external(loc_id)<0)
        goto out;
    if(H5Fclose(loc_id)<0)
        return -1;

    /*-------------------------------------------------------------------------
    * create a file with userblock
    *-------------------------------------------------------------------------
    */
    if(make_userblock() < 0)
        goto out;

    /*-------------------------------------------------------------------------
    * create a userblock file 
    *-------------------------------------------------------------------------
    */
    if(make_userblock_file() < 0)
        goto out;


    return 0;

out:
    H5Fclose(loc_id);
    return -1;
}



/*-------------------------------------------------------------------------
* Function: make_all_objects
*
* Purpose: make a test file with all types of HDF5 objects
*
*-------------------------------------------------------------------------
*/
int make_all_objects(hid_t loc_id)
{
    hid_t   dset_id;
    hid_t   group_id;
    hid_t   type_id;
    hid_t   root_id;
    hid_t   space_id;
    hsize_t dims[1]={2};
    /* Compound datatype */
    typedef struct s_t
    {
        int    a;
        float  b;
    } s_t;

    /*-------------------------------------------------------------------------
    * H5G_DATASET
    *-------------------------------------------------------------------------
    */
    space_id = H5Screate_simple(1,dims,NULL);
    dset_id  = H5Dcreate(loc_id,"dset_referenced",H5T_NATIVE_INT,space_id,H5P_DEFAULT);
    H5Sclose(space_id);

    /*-------------------------------------------------------------------------
    * H5G_GROUP
    *-------------------------------------------------------------------------
    */
    group_id  = H5Gcreate(loc_id,"g1",0);
    root_id   = H5Gopen(loc_id, "/");

    /*-------------------------------------------------------------------------
    * H5G_TYPE
    *-------------------------------------------------------------------------
    */

    /* Create a memory compound datatype */
    type_id = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
    H5Tinsert(type_id, "a", HOFFSET(s_t, a), H5T_NATIVE_INT);
    H5Tinsert(type_id, "b", HOFFSET(s_t, b), H5T_NATIVE_FLOAT);
    /* Commit compound datatype and close it */
    H5Tcommit(loc_id, "type", type_id);
    H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5G_LINK
    *-------------------------------------------------------------------------
    */

    H5Glink(loc_id, H5G_LINK_SOFT, "dset", "link");

    /*-------------------------------------------------------------------------
    * write a series of datasetes
    *-------------------------------------------------------------------------
    */

    write_dset_in(root_id,"dset_referenced",loc_id,0);

    /* Close */
    H5Dclose(dset_id);
    H5Gclose(group_id);
    H5Gclose(root_id);

    return 0;

}


/*-------------------------------------------------------------------------
* Function: make_attributes
*
* Purpose: make a test file with all types of attributes
*
*-------------------------------------------------------------------------
*/
int make_attributes(hid_t loc_id)
{
    hid_t   dset_id;
    hid_t   group_id;
    hid_t   root_id;
    hid_t   space_id;
    hsize_t dims[1]={2};


    /*-------------------------------------------------------------------------
    * H5G_DATASET
    *-------------------------------------------------------------------------
    */

    space_id = H5Screate_simple(1,dims,NULL);
    dset_id  = H5Dcreate(loc_id,"dset",H5T_NATIVE_INT,space_id,H5P_DEFAULT);
    H5Sclose(space_id);

    /*-------------------------------------------------------------------------
    * H5G_GROUP
    *-------------------------------------------------------------------------
    */
    group_id  = H5Gcreate(loc_id,"g1",0);
    root_id   = H5Gopen(loc_id, "/");

    /*-------------------------------------------------------------------------
    * write a series of attributes on the dataset, group, and root group
    *-------------------------------------------------------------------------
    */

    write_attr_in(dset_id,"dset",loc_id,0);
    write_attr_in(group_id,"dset",loc_id,0);
    write_attr_in(root_id,"dset",loc_id,0);

    /* Close */
    H5Dclose(dset_id);
    H5Gclose(group_id);
    H5Gclose(root_id);

    return 0;

}

/*-------------------------------------------------------------------------
* Function: make_hlinks
*
* Purpose: make a test file with hard links
*
*-------------------------------------------------------------------------
*/
int make_hlinks(hid_t loc_id)
{
    hid_t   group1_id;
    hid_t   group2_id;
    hid_t   group3_id;
    hsize_t dims[2]={3,2};
    int     buf[3][2]= {{1,1},{1,2},{2,2}};

    /*-------------------------------------------------------------------------
    * create a dataset and some hard links to it
    *-------------------------------------------------------------------------
    */

    if (write_dset(loc_id,2,dims,"dset",H5T_NATIVE_INT,buf)<0)
        return -1;
    if (H5Glink(loc_id, H5G_LINK_HARD, "dset", "link1 to dset")<0)
        return -1;
    if (H5Glink(loc_id, H5G_LINK_HARD, "dset", "link2 to dset")<0)
        return -1;
    if (H5Glink(loc_id, H5G_LINK_HARD, "dset", "link3 to dset")<0)
        return -1;


    /*-------------------------------------------------------------------------
    * create a group and some hard links to it
    *-------------------------------------------------------------------------
    */

    if ((group1_id = H5Gcreate(loc_id,"g1",0))<0)
        return -1;
    if ((group2_id = H5Gcreate(group1_id,"g2",0))<0)
        return -1;
    if ((group3_id = H5Gcreate(group2_id,"g3",0))<0)
        return -1;

    if (H5Glink2(loc_id, "g1", H5G_LINK_HARD, group2_id, "link1 to g1")<0)
        return -1;
    if (H5Glink2(group1_id, "g2", H5G_LINK_HARD, group3_id, "link1 to g2")<0)
        return -1;

    H5Gclose(group1_id);
    H5Gclose(group2_id);
    H5Gclose(group3_id);

    return 0;

}


/*-------------------------------------------------------------------------
* Function: make_szip
*
* Purpose: make a dataset with the SZIP filter
*
*-------------------------------------------------------------------------
*/
#ifdef H5_HAVE_FILTER_SZIP
int make_szip(hid_t loc_id)
{
    hid_t    dcpl; /* dataset creation property list */
    hid_t    sid;  /* dataspace ID */
    unsigned szip_options_mask=H5_SZIP_ALLOW_K13_OPTION_MASK|H5_SZIP_NN_OPTION_MASK;
    unsigned szip_pixels_per_block=8;
    hsize_t  dims[RANK]={DIM1,DIM2};
    hsize_t  chunk_dims[RANK]={CDIM1,CDIM2};
    int      buf[DIM1][DIM2];
    int      i, j, n;
    int szip_can_encode = 0;

    for (i=n=0; i<DIM1; i++){
        for (j=0; j<DIM2; j++){
            buf[i][j]=n++;
        }
    }
    /* create a space */
    if((sid = H5Screate_simple(RANK, dims, NULL))<0)
        return -1;
    /* create a dcpl */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
        goto out;
    /* set up chunk */
    if(H5Pset_chunk(dcpl, RANK, chunk_dims)<0)
        goto out;

    /*-------------------------------------------------------------------------
    * SZIP
    *-------------------------------------------------------------------------
    */
    /* Make sure encoding is enabled */
    if (h5tools_can_encode(H5Z_FILTER_SZIP) == 1) {
        szip_can_encode = 1;
    }
    if (szip_can_encode) {
        /* set szip data */
        if(H5Pset_szip (dcpl,szip_options_mask,szip_pixels_per_block)<0)
            goto out;
        if (make_dset(loc_id,"dset_szip",sid,dcpl,buf)<0)
            goto out;
    } else {
        /* WARNING? SZIP is decoder only, can't generate test files */
    }

    if(H5Sclose(sid)<0)
        goto out;
    if(H5Pclose(dcpl)<0)
        goto out;

    return 0;

out:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Sclose(sid);
    } H5E_END_TRY;
    return -1;
}
#endif /* H5_HAVE_FILTER_SZIP */



/*-------------------------------------------------------------------------
* Function: make_deflate
*
* Purpose: make a dataset with the deflate filter
*
*-------------------------------------------------------------------------
*/
int make_deflate(hid_t loc_id)
{
    hid_t      dcpl; /* dataset creation property list */
    hid_t      sid;  /* dataspace ID */
    hsize_t    dims[RANK]={DIM1,DIM2};
    hsize_t    chunk_dims[RANK]={CDIM1,CDIM2};
    int        buf[DIM1][DIM2];
    hobj_ref_t bufref[1]; /* reference */
    hsize_t    dims1r[1]={1};
    int        i, j, n;

    for (i=n=0; i<DIM1; i++){
        for (j=0; j<DIM2; j++){
            buf[i][j]=n++;
        }
    }

    /* create a space */
    if((sid = H5Screate_simple(RANK, dims, NULL))<0)
        return -1;
    /* create a dcpl */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
        goto out;
    /* set up chunk */
    if(H5Pset_chunk(dcpl, RANK, chunk_dims)<0)
        goto out;
    /*-------------------------------------------------------------------------
    * GZIP
    *-------------------------------------------------------------------------
    */
#if defined (H5_HAVE_FILTER_DEFLATE)
    /* set deflate data */
    if(H5Pset_deflate(dcpl, 9)<0)
        goto out;
    if (make_dset(loc_id,"dset_deflate",sid,dcpl,buf)<0)
        goto out;

    /* create a reference to the dataset, test second seeep of file for references */

    if (H5Rcreate(&bufref[0],loc_id,"dset_deflate",H5R_OBJECT,-1)<0)
        goto out;
    if (write_dset(loc_id,1,dims1r,"ref",H5T_STD_REF_OBJ,bufref)<0)
        goto out;
#endif

    /*-------------------------------------------------------------------------
    * close space and dcpl
    *-------------------------------------------------------------------------
    */
    if(H5Sclose(sid)<0)
        goto out;
    if(H5Pclose(dcpl)<0)
        goto out;

    return 0;

out:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Sclose(sid);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
* Function: make_shuffle
*
* Purpose: make a dataset with the shuffle filter
*
*-------------------------------------------------------------------------
*/
int make_shuffle(hid_t loc_id)
{
    hid_t    dcpl; /* dataset creation property list */
    hid_t    sid;  /* dataspace ID */
    hsize_t  dims[RANK]={DIM1,DIM2};
    hsize_t  chunk_dims[RANK]={CDIM1,CDIM2};
    int      buf[DIM1][DIM2];
    int      i, j, n;

    for (i=n=0; i<DIM1; i++){
        for (j=0; j<DIM2; j++){
            buf[i][j]=n++;
        }
    }
    /* create a space */
    if((sid = H5Screate_simple(RANK, dims, NULL))<0)
        return -1;
    /* create a dcpl */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
        goto out;
    /* set up chunk */
    if(H5Pset_chunk(dcpl, RANK, chunk_dims)<0)
        goto out;

    /*-------------------------------------------------------------------------
    * shuffle
    *-------------------------------------------------------------------------
    */
#if defined (H5_HAVE_FILTER_SHUFFLE)
    /* set the shuffle filter */
    if (H5Pset_shuffle(dcpl)<0)
        goto out;
    if (make_dset(loc_id,"dset_shuffle",sid,dcpl,buf)<0)
        goto out;
#endif

    /*-------------------------------------------------------------------------
    * close space and dcpl
    *-------------------------------------------------------------------------
    */
    if(H5Sclose(sid)<0)
        goto out;
    if(H5Pclose(dcpl)<0)
        goto out;

    return 0;

out:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Sclose(sid);
    } H5E_END_TRY;
    return -1;
}

/*-------------------------------------------------------------------------
* Function: make_fletcher32
*
* Purpose: make a dataset with the fletcher32 filter
*
*-------------------------------------------------------------------------
*/
int make_fletcher32(hid_t loc_id)
{
    hid_t    dcpl; /* dataset creation property list */
    hid_t    sid;  /* dataspace ID */
    hsize_t  dims[RANK]={DIM1,DIM2};
    hsize_t  chunk_dims[RANK]={CDIM1,CDIM2};
    int      buf[DIM1][DIM2];
    int      i, j, n;

    for (i=n=0; i<DIM1; i++){
        for (j=0; j<DIM2; j++){
            buf[i][j]=n++;
        }
    }
    /* create a space */
    if((sid = H5Screate_simple(RANK, dims, NULL))<0)
        return -1;
    /* create a dataset creation property list; the same DCPL is used for all dsets */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
        goto out;
    /* set up chunk */
    if(H5Pset_chunk(dcpl, RANK, chunk_dims)<0)
        goto out;


    /*-------------------------------------------------------------------------
    * fletcher32
    *-------------------------------------------------------------------------
    */
#if defined (H5_HAVE_FILTER_FLETCHER32)
    /* remove the filters from the dcpl */
    if (H5Premove_filter(dcpl,H5Z_FILTER_ALL)<0)
        goto out;
    /* set the checksum filter */
    if (H5Pset_fletcher32(dcpl)<0)
        goto out;
    if (make_dset(loc_id,"dset_fletcher32",sid,dcpl,buf)<0)
        goto out;
#endif

    /*-------------------------------------------------------------------------
    * close space and dcpl
    *-------------------------------------------------------------------------
    */
    if(H5Sclose(sid)<0)
        goto out;
    if(H5Pclose(dcpl)<0)
        goto out;

    return 0;

out:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Sclose(sid);
    } H5E_END_TRY;
    return -1;
}



/*-------------------------------------------------------------------------
* Function: make_all
*
* Purpose: make a file with all filters
*
*-------------------------------------------------------------------------
*/
int make_all(hid_t loc_id)
{
    hid_t    dcpl; /* dataset creation property list */
    hid_t    sid;  /* dataspace ID */
#if defined (H5_HAVE_FILTER_NBIT)
    hid_t    dtid;
    hid_t    dsid;
#endif /* H5_HAVE_FILTER_NBIT */
#if defined (H5_HAVE_FILTER_SZIP)
    unsigned szip_options_mask=H5_SZIP_ALLOW_K13_OPTION_MASK|H5_SZIP_NN_OPTION_MASK;
    unsigned szip_pixels_per_block=8;
#endif /* H5_HAVE_FILTER_SZIP */
    hsize_t  dims[RANK]={DIM1,DIM2};
    hsize_t  chunk_dims[RANK]={CDIM1,CDIM2};
    int      buf[DIM1][DIM2];
    int      i, j, n;
#if defined (H5_HAVE_FILTER_SZIP)
    int szip_can_encode = 0;
#endif

    for (i=n=0; i<DIM1; i++){
        for (j=0; j<DIM2; j++){
            buf[i][j]=n++;
        }
    }
    /* create a space */
    if((sid = H5Screate_simple(RANK, dims, NULL))<0)
        return -1;
    /* create a dataset creation property list; the same DCPL is used for all dsets */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
        goto out;
    /* set up chunk */
    if(H5Pset_chunk(dcpl, RANK, chunk_dims)<0)
        goto out;

#if defined (H5_HAVE_FILTER_SHUFFLE)
    /* set the shuffle filter */
    if (H5Pset_shuffle(dcpl)<0)
        goto out;
#endif

#if defined (H5_HAVE_FILTER_FLETCHER32)
    /* set the checksum filter */
    if (H5Pset_fletcher32(dcpl)<0)
        goto out;
#endif

#if defined (H5_HAVE_FILTER_SZIP)
    if (h5tools_can_encode(H5Z_FILTER_SZIP) == 1) {
        szip_can_encode = 1;
    }
    if (szip_can_encode) {
        /* set szip data */
        if(H5Pset_szip (dcpl,szip_options_mask,szip_pixels_per_block)<0)
            goto out;
    } else {
        /* WARNING? SZIP is decoder only, can't generate test data using szip */
    }
#endif

#if defined (H5_HAVE_FILTER_DEFLATE)
    /* set deflate data */
    if(H5Pset_deflate(dcpl, 9)<0)
        goto out;
#endif

    if (make_dset(loc_id,"dset_all",sid,dcpl,buf)<0)
        goto out;

    /*-------------------------------------------------------------------------
    * fletcher32
    *-------------------------------------------------------------------------
    */
#if defined (H5_HAVE_FILTER_FLETCHER32)
    /* remove the filters from the dcpl */
    if (H5Premove_filter(dcpl,H5Z_FILTER_ALL)<0)
        goto out;
    /* set the checksum filter */
    if (H5Pset_fletcher32(dcpl)<0)
        goto out;
    if (make_dset(loc_id,"dset_fletcher32",sid,dcpl,buf)<0)
        goto out;
#endif

    /*-------------------------------------------------------------------------
    * SZIP
    *-------------------------------------------------------------------------
    */
    /* Make sure encoding is enabled */
#if defined (H5_HAVE_FILTER_SZIP)
    if (szip_can_encode) {
        /* remove the filters from the dcpl */
        if (H5Premove_filter(dcpl,H5Z_FILTER_ALL)<0)
            goto out;
        /* set szip data */
        if(H5Pset_szip (dcpl,szip_options_mask,szip_pixels_per_block)<0)
            goto out;
        if (make_dset(loc_id,"dset_szip",sid,dcpl,buf)<0)
            goto out;
    } else {
        /* WARNING? SZIP is decoder only, can't generate test dataset */
    }
#endif

    /*-------------------------------------------------------------------------
    * shuffle
    *-------------------------------------------------------------------------
    */
#if defined (H5_HAVE_FILTER_SHUFFLE)
    /* remove the filters from the dcpl */
    if (H5Premove_filter(dcpl,H5Z_FILTER_ALL)<0)
        goto out;
    /* set the shuffle filter */
    if (H5Pset_shuffle(dcpl)<0)
        goto out;
    if (make_dset(loc_id,"dset_shuffle",sid,dcpl,buf)<0)
        goto out;
#endif

    /*-------------------------------------------------------------------------
    * GZIP
    *-------------------------------------------------------------------------
    */
#if defined (H5_HAVE_FILTER_DEFLATE)
    /* remove the filters from the dcpl */
    if (H5Premove_filter(dcpl,H5Z_FILTER_ALL)<0)
        goto out;
    /* set deflate data */
    if(H5Pset_deflate(dcpl, 1)<0)
        goto out;
    if (make_dset(loc_id,"dset_deflate",sid,dcpl,buf)<0)
        goto out;
#endif


    /*-------------------------------------------------------------------------
    * close space and dcpl
    *-------------------------------------------------------------------------
    */
    if(H5Sclose(sid)<0)
        goto out;
    if(H5Pclose(dcpl)<0)
        goto out;

    return 0;

out:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Sclose(sid);
    } H5E_END_TRY;
    return -1;
}



/*-------------------------------------------------------------------------
* Function: make_early
*
* Purpose: create a file for the H5D_ALLOC_TIME_EARLY test
*
*-------------------------------------------------------------------------
*/
int make_early(void)
{
    hsize_t dims[1] ={3000};
    hsize_t cdims[1]={30};
    hid_t   fid=-1;
    hid_t   dset_id=-1;
    hid_t   sid=-1;
    hid_t   tid=-1;
    hid_t   dcpl=-1;
    int     i;
    char    name[10];
    int     iter=100;

    if ((fid = H5Fcreate(FNAME5,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
        return -1;
    if (H5Fclose(fid)<0)
        goto out;

    if ((sid = H5Screate_simple(1, dims, NULL))<0)
        goto out;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
        goto out;
    if (H5Pset_chunk(dcpl,1,cdims)<0)
        goto out;
    if (H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY)<0)
        goto out;

    for (i=0; i<iter; i++)
    {
        if ((fid = H5Fopen(FNAME5,H5F_ACC_RDWR,H5P_DEFAULT))<0)
            goto out;
        if ((dset_id = H5Dcreate(fid,"early",H5T_NATIVE_DOUBLE,sid,dcpl))<0)
            goto out;
        if ((tid = H5Tcopy(H5T_NATIVE_DOUBLE))<0)
            goto out;
        sprintf(name,"%d", i);
        if ((H5Tcommit(fid,name,tid))<0)
            goto out;
        if (H5Tclose(tid)<0)
            goto out;
        if (H5Dclose(dset_id)<0)
            goto out;
        if (H5Gunlink(fid,"early")<0)
            goto out;
        if (H5Fclose(fid)<0)
            goto out;
    }

    /*-------------------------------------------------------------------------
    * do the same without close/opening the file and creating the dataset
    *-------------------------------------------------------------------------
    */

    if ((fid = H5Fcreate(FNAME6,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
        return -1;

    for (i=0; i<iter; i++)
    {
        if ((tid = H5Tcopy(H5T_NATIVE_DOUBLE))<0)
            goto out;
        sprintf(name,"%d", i);
        if ((H5Tcommit(fid,name,tid))<0)
            goto out;
        if (H5Tclose(tid)<0)
            goto out;
    }

    if (H5Sclose(sid)<0)
        goto out;
    if (H5Pclose(dcpl)<0)
        goto out;
    if (H5Fclose(fid)<0)
        goto out;


    return 0;

out:
    H5E_BEGIN_TRY {
        H5Tclose(tid);
        H5Pclose(dcpl);
        H5Sclose(sid);
        H5Dclose(dset_id);
        H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
}



/*-------------------------------------------------------------------------
* Function: make_layout
*
* Purpose: make several datasets with several layouts in location LOC_ID
*
*-------------------------------------------------------------------------
*/
int make_layout(hid_t loc_id)
{
    hid_t    dcpl; /* dataset creation property list */
    hid_t    sid;  /* dataspace ID */
    hsize_t  dims[RANK]={DIM1,DIM2};
    hsize_t  chunk_dims[RANK]={CDIM1,CDIM2};
    int      buf[DIM1][DIM2];
    int      i, j, n;
    char     name[6];


    for (i=n=0; i<DIM1; i++){
        for (j=0; j<DIM2; j++){
            buf[i][j]=n++;
        }
    }

    /*-------------------------------------------------------------------------
    * make several dataset with no filters
    *-------------------------------------------------------------------------
    */
    for (i=0; i<4; i++)
    {
        sprintf(name,"dset%d",i+1);
        if (write_dset(loc_id,RANK,dims,name,H5T_NATIVE_INT,buf)<0)
            return -1;
    }


    /*-------------------------------------------------------------------------
    * make several dataset with several layout options
    *-------------------------------------------------------------------------
    */
    /* create a space */
    if((sid = H5Screate_simple(RANK, dims, NULL))<0)
        return -1;
    /* create a dataset creation property list; the same DCPL is used for all dsets */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
        goto out;

    /*-------------------------------------------------------------------------
    * H5D_COMPACT
    *-------------------------------------------------------------------------
    */
    if(H5Pset_layout (dcpl,H5D_COMPACT)<0)
        goto out;
    if (make_dset(loc_id,"dset_compact",sid,dcpl,buf)<0)
        goto out;

    /*-------------------------------------------------------------------------
    * H5D_CONTIGUOUS
    *-------------------------------------------------------------------------
    */
    if(H5Pset_layout (dcpl,H5D_CONTIGUOUS)<0)
        goto out;
    if (make_dset(loc_id,"dset_contiguous",sid,dcpl,buf)<0)
        goto out;

    /*-------------------------------------------------------------------------
    * H5D_CHUNKED
    *-------------------------------------------------------------------------
    */
    if(H5Pset_chunk(dcpl, RANK, chunk_dims)<0)
        goto out;
    if (make_dset(loc_id,"dset_chunk",sid,dcpl,buf)<0)
        goto out;

    /*-------------------------------------------------------------------------
    * close space and dcpl
    *-------------------------------------------------------------------------
    */
    if(H5Sclose(sid)<0)
        goto out;
    if(H5Pclose(dcpl)<0)
        goto out;

    return 0;

out:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Sclose(sid);
    } H5E_END_TRY;
    return -1;
}

/*-------------------------------------------------------------------------
* Function: make a file with an integer dataset with a fill value
*
* Purpose: test copy of fill values
*
*-------------------------------------------------------------------------
*/
int make_fill(hid_t loc_id)
{
    hid_t   did=-1;
    hid_t   sid=-1;
    hid_t   dcpl;
    hsize_t dims[2]={3,2};
    int     buf[3][2]= {{1,1},{1,2},{2,2}};
    int     fillvalue=2;

    /*-------------------------------------------------------------------------
    * H5T_INTEGER, write a fill value
    *-------------------------------------------------------------------------
    */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
        goto out;
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillvalue)<0)
        goto out;
    if ((sid = H5Screate_simple(2,dims,NULL))<0)
        goto out;
    if ((did = H5Dcreate(loc_id,"dset_fill",H5T_NATIVE_INT,sid,dcpl))<0)
        goto out;
    if (H5Dwrite(did,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf)<0)
        goto out;

    /* close */
    if(H5Sclose(sid)<0)
        goto out;
    if(H5Pclose(dcpl)<0)
        goto out;
    if(H5Dclose(did)<0)
        goto out;

    return 0;

out:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Sclose(sid);
        H5Dclose(did);
    } H5E_END_TRY;
    return -1;

}

/*-------------------------------------------------------------------------
* Function: make_big 
*
* Purpose: used in test read by hyperslabs. Creates a 128MB dataset.
*  Only 1 1024Kb hyperslab is written.
*
*-------------------------------------------------------------------------
*/

int make_big(hid_t loc_id)
{
    hid_t   did=-1;
    hid_t   f_sid=-1;
    hid_t   m_sid=-1;
    hid_t   tid;
    hid_t   dcpl;
    hsize_t dims[1]={ H5TOOLS_MALLOCSIZE + 1}; /* dataset dimensions */
    hsize_t hs_size[1];                     /* hyperslab dimensions */
    hsize_t hs_start[1];                    /* hyperslab start */
    hsize_t chunk_dims[1]={1024};           /* chunk dimensions */
    size_t  size;
    size_t  nelmts=(size_t)1024;
    signed  char fillvalue=-1;
    signed  char *buf=NULL;

    /* write one 1024 byte hyperslab */
    hs_start[0] = 0;
    hs_size[0]  = 1024;

    /* create */ 
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
        goto out;
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_SCHAR, &fillvalue)<0)
        goto out;
    if(H5Pset_chunk(dcpl, 1, chunk_dims)<0)
        goto out;
    if ((f_sid = H5Screate_simple(1,dims,NULL))<0)
        goto out;
    if ((did = H5Dcreate(loc_id,"dset",H5T_NATIVE_SCHAR,f_sid,dcpl))<0)
        goto out;
    if ((m_sid = H5Screate_simple(1, hs_size, hs_size))<0) 
        goto out;
    if ((tid = H5Dget_type(did))<0) 
        goto out;
    if ((size = H5Tget_size(tid))<=0)
        goto out;

    /* initialize buffer to 0  */
    buf=(signed  char *) calloc( nelmts, size);

    if (H5Sselect_hyperslab (f_sid,H5S_SELECT_SET,hs_start,NULL,hs_size, NULL)<0) 
        goto out;
    if (H5Dwrite (did,H5T_NATIVE_SCHAR,m_sid,f_sid,H5P_DEFAULT,buf)<0) 
        goto out;

    free(buf);
    buf=NULL;

    /* close */
    if(H5Sclose(f_sid)<0)
        goto out;
    if(H5Sclose(m_sid)<0)
        goto out;
    if(H5Pclose(dcpl)<0)
        goto out;
    if(H5Dclose(did)<0)
        goto out;

    return 0;

out:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Sclose(f_sid);
        H5Sclose(m_sid);
        H5Dclose(did);
    } H5E_END_TRY;
    return -1;

}

/*-------------------------------------------------------------------------
* Function: make_external 
*
* Purpose: create a external dataset
*
*-------------------------------------------------------------------------
*/

int make_external(hid_t loc_id)
{
    hid_t   did=-1;
    hid_t   sid=-1;
    hid_t   dcpl;
    int     buf[2]={1,2};
    hsize_t cur_size[1];		/* data space current size	*/
    hsize_t max_size[1];		/* data space maximum size	*/
    hsize_t size;

    cur_size[0] = max_size[0] = 2;
    size = max_size[0] * sizeof(int);

    /* create */ 
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
        goto out;
    if (H5Pset_external(dcpl, H5REPACK_EXTFILE, (off_t)0, size)<0) 
        goto out;
    if ((sid = H5Screate_simple(1,cur_size, max_size))<0)
        goto out;
    if ((did = H5Dcreate(loc_id,"external",H5T_NATIVE_INT,sid,dcpl))<0)
        goto out;
    if (H5Dwrite(did,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf)<0)
        goto out;

    /* close */
    if(H5Sclose(sid)<0)
        goto out;
    if(H5Pclose(dcpl)<0)
        goto out;
    if(H5Dclose(did)<0)
        goto out;

    return 0;

out:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Sclose(sid);
        H5Dclose(did);
    } H5E_END_TRY;
    return -1;

}

/*-------------------------------------------------------------------------
* Function: make_userblock
*
* Purpose: create a file for the userblock copying test
*
*-------------------------------------------------------------------------
*/
static int
make_userblock(void)
{
    hid_t   fid = -1;
    hid_t   fcpl = -1;
    int     fd = -1;            /* File descriptor for writing userblock */
    char    ub[USERBLOCK_SIZE]; /* User block data */
    ssize_t nwritten;           /* # of bytes written */
    size_t  u;                  /* Local index variable */

    /* Create file creation property list with userblock set */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        goto out;
    if(H5Pset_userblock(fcpl, (hsize_t)USERBLOCK_SIZE) < 0)
        goto out;

    /* Create file with userblock */
    if((fid = H5Fcreate(FNAME16, H5F_ACC_TRUNC, fcpl, H5P_DEFAULT)) < 0)
        goto out;
    if(H5Fclose(fid) < 0)
        goto out;

    /* Close file creation property list */
    if(H5Pclose(fcpl) < 0)
        goto out;


    /* Initialize userblock data */
    for(u = 0; u < USERBLOCK_SIZE; u++)
        ub[u] = 'a' + (u % 26);

    /* Re-open HDF5 file, as "plain" file */
    if((fd = HDopen(FNAME16, O_WRONLY, 0644)) < 0)
        goto out;

    /* Write userblock data */
    nwritten = HDwrite(fd, ub, (size_t)USERBLOCK_SIZE);
    assert(nwritten == USERBLOCK_SIZE);

    /* Close file */
    HDclose(fd);

    return 0;

out:
    H5E_BEGIN_TRY {
        H5Pclose(fcpl);
        H5Fclose(fid);
    } H5E_END_TRY;
    if(fd > 0)
        HDclose(fd);

    return -1;
} /* end make_userblock() */

/*-------------------------------------------------------------------------
* Function: verify_userblock
*
* Purpose: Verify that the userblock was copied correctly
*
*-------------------------------------------------------------------------
*/
static int
verify_userblock( const char* filename)
{
    hid_t   fid = -1;
    hid_t   fcpl = -1;
    int     fd = -1;            /* File descriptor for writing userblock */
    char    ub[USERBLOCK_SIZE]; /* User block data */
    hsize_t ub_size = 0;        /* User block size */
    ssize_t nread;              /* # of bytes read */
    size_t  u;                  /* Local index variable */

    /* Open file with userblock */
    if((fid = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        goto out;

    /* Retrieve file creation property list & userblock size */
    if((fcpl = H5Fget_create_plist(fid)) < 0)
        goto out;
    if(H5Pget_userblock(fcpl, &ub_size) < 0)
        goto out;

    /* Verify userblock size is correct */
    if(ub_size != USERBLOCK_SIZE)
        goto out;

    /* Close file creation property list */
    if(H5Pclose(fcpl) < 0)
        goto out;

    if(H5Fclose(fid) < 0)
        goto out;


    /* Re-open HDF5 file, as "plain" file */
    if((fd = HDopen(filename, O_RDONLY, 0)) < 0)
        goto out;

    /* Read userblock data */
    nread = HDread(fd, ub, (size_t)USERBLOCK_SIZE);
    assert(nread == USERBLOCK_SIZE);

    /* Verify userblock data */
    for(u = 0; u < USERBLOCK_SIZE; u++)
        if(ub[u] != (char)('a' + (u % 26)))
            goto out;

    /* Close file */
    HDclose(fd);

    return 0;

out:
    H5E_BEGIN_TRY {
        H5Pclose(fcpl);
        H5Fclose(fid);
    } H5E_END_TRY;
    if(fd > 0)
        HDclose(fd);

    return -1;
} /* end verify_userblock() */


/*-------------------------------------------------------------------------
* Function: make_userblock_file
*
* Purpose: create a file for the userblock add test
*
*-------------------------------------------------------------------------
*/
static int
make_userblock_file(void)
{
    hid_t   fid = -1;
    hid_t   fcpl = -1;
    int     fd = -1;            /* File descriptor for writing userblock */
    char    ub[USERBLOCK_SIZE]; /* User block data */
    ssize_t nwritten;           /* # of bytes written */
    size_t  u;                  /* Local index variable */

    /* initialize userblock data */
    for(u = 0; u < USERBLOCK_SIZE; u++)
        ub[u] = 'a' + (u % 26);

    /* open file */
    if((fd = HDopen(FNAME_UB,O_WRONLY|O_CREAT|O_TRUNC, 0644 )) < 0)
        goto out;

    /* write userblock data */
    nwritten = HDwrite(fd, ub, (size_t)USERBLOCK_SIZE);
    assert(nwritten == USERBLOCK_SIZE);

    /* close file */
    HDclose(fd);

    return 0;

out:

    if(fd > 0)
        HDclose(fd);

    return -1;
} 

/*-------------------------------------------------------------------------
* Function: write_dset_in
*
* Purpose: write datasets in LOC_ID
*
* Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
*
* Date: November 12, 2003
*
*-------------------------------------------------------------------------
*/


void write_dset_in(hid_t loc_id,
                   const char* dset_name, /* for saving reference to dataset*/
                   hid_t file_id,
                   int make_diffs /* flag to modify data buffers */)
{
    /* Compound datatype */
    typedef struct s_t
    {
        char   a;
        double b;
    } s_t;

    typedef enum
    {
        RED,
        GREEN
    } e_t;

    hid_t   dset_id;
    hid_t   space_id;
    hid_t   type_id;
    hid_t   plist_id;
    herr_t  status;
    int     val, i, j, k, n;
    float   f;

    /* create 1D attributes with dimension [2], 2 elements */
    hsize_t    dims[1]={2};
    hsize_t    dims1r[1]={2};
    char       buf1[2][2]= {"ab","de"};        /* string */
    char       buf2[2]= {1,2};                 /* bitfield, opaque */
    s_t        buf3[2]= {{1,2},{3,4}};         /* compound */
    hobj_ref_t buf4[2];                        /* reference */
    e_t        buf45[2]= {RED,GREEN};          /* enum */
    hvl_t      buf5[2];                        /* vlen */
    hsize_t    dimarray[1]={3};                /* array dimension */
    int        buf6[2][3]= {{1,2,3},{4,5,6}};  /* array */
    int        buf7[2]= {1,2};                 /* integer */
    float      buf8[2]= {1,2};                 /* float */

    /* create 2D attributes with dimension [3][2], 6 elements */
    hsize_t    dims2[2]={3,2};
    hsize_t    dims2r[2]={1,1};
    char       buf12[6][2]= {"ab","cd","ef","gh","ij","kl"};         /* string */
    char       buf22[3][2]= {{1,2},{3,4},{5,6}};                     /* bitfield, opaque */
    s_t        buf32[6]= {{1,2},{3,4},{5,6},{7,8},{9,10},{11,12}};   /* compound */
    hobj_ref_t buf42[1][1];                                          /* reference */
    hvl_t      buf52[3][2];                                          /* vlen */
    int        buf62[6][3]= {{1,2,3},{4,5,6},{7,8,9},{10,11,12},{13,14,15},{16,17,18}};  /* array */
    int        buf72[3][2]= {{1,2},{3,4},{5,6}};                     /* integer */
    float      buf82[3][2]= {{1,2},{3,4},{5,6}};                     /* float */

    /* create 3D attributes with dimension [4][3][2], 24 elements */
    hsize_t    dims3[3]={4,3,2};
    hsize_t    dims3r[3]={1,1,1};
    char       buf13[24][2]= {"ab","cd","ef","gh","ij","kl","mn","pq",
        "rs","tu","vw","xz","AB","CD","EF","GH",
        "IJ","KL","MN","PQ","RS","TU","VW","XZ"};  /* string */
    char       buf23[4][3][2];    /* bitfield, opaque */
    s_t        buf33[4][3][2];    /* compound */
    hobj_ref_t buf43[1][1][1];    /* reference */
    hvl_t      buf53[4][3][2];    /* vlen */
    int        buf63[24][3];      /* array */
    int        buf73[4][3][2];    /* integer */
    float      buf83[4][3][2];    /* float */


    /*-------------------------------------------------------------------------
    * 1D
    *-------------------------------------------------------------------------
    */

    /*-------------------------------------------------------------------------
    * H5T_STRING
    *-------------------------------------------------------------------------
    */


    if (make_diffs)
    {
        for (i=0; i<2; i++)
            for (j=0; j<2; j++)
            {
                buf1[i][j]='z';
            }
    }


    type_id = H5Tcopy(H5T_C_S1);
    status  = H5Tset_size(type_id, 2);
    write_dset(loc_id,1,dims,"string",type_id,buf1);
    status = H5Tclose(type_id);


    /* create hard link */
    status = H5Glink(loc_id, H5G_LINK_HARD, "string", "string_link");

    /*-------------------------------------------------------------------------
    * H5T_BITFIELD
    *-------------------------------------------------------------------------
    */

    if (make_diffs)
    {
        for (i=0; i<2; i++)
            buf2[i]=buf2[1]=0;
    }

    type_id = H5Tcopy(H5T_STD_B8LE);
    write_dset(loc_id,1,dims,"bitfield",type_id,buf2);
    status = H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5T_OPAQUE
    *-------------------------------------------------------------------------
    */

    if (make_diffs)
    {
        for (i=0; i<2; i++)
        {
            buf3[i].a=0; buf3[i].b=0;
        }
    }

    type_id = H5Tcreate(H5T_OPAQUE, 1);
    status = H5Tset_tag(type_id, "1-byte opaque type"); /* must set this */
    write_dset(loc_id,1,dims,"opaque",type_id,buf2);
    status = H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5T_COMPOUND
    *-------------------------------------------------------------------------
    */


    if (make_diffs)
    {
        for (i=0; i<2; i++)
        {
            buf45[i]=GREEN;
        }
    }

    type_id = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
    H5Tinsert(type_id, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
    H5Tinsert(type_id, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
    write_dset(loc_id,1,dims,"compound",type_id,buf3);
    status = H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5T_REFERENCE
    *-------------------------------------------------------------------------
    */
    /* object references ( H5R_OBJECT ) */
    buf4[0]=0;
    buf4[1]=0;
    if (dset_name)
    {
        status=H5Rcreate(&buf4[0],file_id,dset_name,H5R_OBJECT,-1);
        write_dset(loc_id,1,dims1r,"refobj",H5T_STD_REF_OBJ,buf4);
    }

    /* Dataset region reference ( H5R_DATASET_REGION  )  */
    make_dset_reg_ref(loc_id);


    /*-------------------------------------------------------------------------
    * H5T_ENUM
    *-------------------------------------------------------------------------
    */
    type_id = H5Tcreate(H5T_ENUM, sizeof(e_t));
    H5Tenum_insert(type_id, "RED",   (val = 0, &val));
    H5Tenum_insert(type_id, "GREEN", (val = 1, &val));
    write_dset(loc_id,1,dims,"enum",type_id,buf45);
    status = H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5T_VLEN
    *-------------------------------------------------------------------------
    */

    /* Allocate and initialize VL dataset to write */

    buf5[0].len = 1;
    buf5[0].p = malloc( 1 * sizeof(int));
    ((int *)buf5[0].p)[0]=1;
    buf5[1].len = 2;
    buf5[1].p = malloc( 2 * sizeof(int));
    ((int *)buf5[1].p)[0]=2;
    ((int *)buf5[1].p)[1]=3;

    if (make_diffs)
    {
        ((int *)buf5[0].p)[0]=0;
        ((int *)buf5[1].p)[0]=0;
        ((int *)buf5[1].p)[1]=0;
    }

    space_id = H5Screate_simple(1,dims,NULL);
    type_id = H5Tvlen_create(H5T_NATIVE_INT);
    dset_id = H5Dcreate(loc_id,"vlen",type_id,space_id,H5P_DEFAULT);
    status = H5Dwrite(dset_id,type_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf5);
    assert(status>=0);
    status = H5Dvlen_reclaim(type_id,space_id,H5P_DEFAULT,buf5);
    assert(status>=0);
    status = H5Dclose(dset_id);
    status = H5Tclose(type_id);
    status = H5Sclose(space_id);

    /*-------------------------------------------------------------------------
    * H5T_ARRAY
    *-------------------------------------------------------------------------
    */

    if (make_diffs)
    {
        for (i=0; i<2; i++)
            for (j=0; j<3; j++)
            {
                buf6[i][j]=0;
            }
    }

    type_id = H5Tarray_create(H5T_NATIVE_INT, 1, dimarray, NULL);
    write_dset(loc_id,1,dims,"array",type_id,buf6);
    status = H5Tclose(type_id);

    {

        double  *dbuf;                           /* information to write */
        hid_t   did;                             /* dataset ID   */
        hid_t   sid;                             /* dataspace ID   */
        hid_t   tid;                             /* datatype ID   */
        size_t  size;
        hsize_t sdims[] = {1};
        hsize_t tdims[] = {H5TOOLS_MALLOCSIZE / sizeof(double) + 1};
        int     j;

        /* allocate and initialize array data to write */
        size = ( H5TOOLS_MALLOCSIZE / sizeof(double) + 1 ) * sizeof(double);
        dbuf = malloc( size );

        for( j = 0; j < H5TOOLS_MALLOCSIZE / sizeof(double) + 1; j++)
            dbuf[j] = j;

        if (make_diffs)
        {
            dbuf[5] = 0;
            dbuf[6] = 0;
        }

        /* create a type larger than H5TOOLS_MALLOCSIZE */
        tid = H5Tarray_create(H5T_NATIVE_DOUBLE, 1, tdims, NULL);
        size = H5Tget_size(tid);
        sid = H5Screate_simple(1, sdims, NULL);
        did = H5Dcreate(loc_id, "arrayd", tid, sid, H5P_DEFAULT);
#if defined(WRITE_ARRAY)
        H5Dwrite(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, dbuf);
#endif

        /* close */
        H5Dclose(did);
        H5Tclose(tid);
        H5Sclose(sid);
        free( dbuf );
    }


    /*-------------------------------------------------------------------------
    * H5T_INTEGER and H5T_FLOAT
    *-------------------------------------------------------------------------
    */

    if (make_diffs)
    {
        for (i=0; i<2; i++)
        {
            buf7[i]=0;
            buf8[i]=0;
        }
    }

    write_dset(loc_id,1,dims,"integer",H5T_NATIVE_INT,buf7);
    write_dset(loc_id,1,dims,"float",H5T_NATIVE_FLOAT,buf8);


    /*-------------------------------------------------------------------------
    * 2D
    *-------------------------------------------------------------------------
    */

    /*-------------------------------------------------------------------------
    * H5T_STRING
    *-------------------------------------------------------------------------
    */

    if (make_diffs)
    {
        memset(buf12, 'z', sizeof buf12);
    }


    type_id = H5Tcopy(H5T_C_S1);
    status  = H5Tset_size(type_id, 2);
    write_dset(loc_id,2,dims2,"string2D",type_id,buf12);
    status = H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5T_BITFIELD
    *-------------------------------------------------------------------------
    */


    if (make_diffs)
    {
        memset(buf22,0,sizeof buf22);
    }

    type_id = H5Tcopy(H5T_STD_B8LE);
    write_dset(loc_id,2,dims2,"bitfield2D",type_id,buf22);
    status = H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5T_OPAQUE
    *-------------------------------------------------------------------------
    */
    type_id = H5Tcreate(H5T_OPAQUE, 1);
    status = H5Tset_tag(type_id, "1-byte opaque type"); /* must set this */
    write_dset(loc_id,2,dims2,"opaque2D",type_id,buf22);
    status = H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5T_COMPOUND
    *-------------------------------------------------------------------------
    */

    if (make_diffs)
    {
        memset(buf32,0,sizeof buf32);
    }

    type_id = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
    H5Tinsert(type_id, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
    H5Tinsert(type_id, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
    write_dset(loc_id,2,dims2,"compound2D",type_id,buf32);
    status = H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5T_REFERENCE
    *-------------------------------------------------------------------------
    */
    /* Create references to dataset */
    if (dset_name)
    {
        for (i = 0; i < 1; i++) {
            for (j = 0; j < 1; j++) {
                status=H5Rcreate(&buf42[i][j],file_id,dset_name,H5R_OBJECT,-1);
            }
        }
        write_dset(loc_id,2,dims2r,"refobj2D",H5T_STD_REF_OBJ,buf42);
    }

    /*-------------------------------------------------------------------------
    * H5T_ENUM
    *-------------------------------------------------------------------------
    */

    type_id = H5Tcreate(H5T_ENUM, sizeof(e_t));
    H5Tenum_insert(type_id, "RED",   (val = 0, &val));
    H5Tenum_insert(type_id, "GREEN", (val = 1, &val));
    write_dset(loc_id,2,dims2,"enum2D",type_id,0);
    status = H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5T_VLEN
    *-------------------------------------------------------------------------
    */

    /* Allocate and initialize VL dataset to write */
    n=0;
    for (i = 0; i < 3; i++) {
        for (j = 0; j < 2; j++) {
            int l;
            buf52[i][j].p = malloc((i + 1) * sizeof(int));
            buf52[i][j].len = i + 1;
            for (l = 0; l < i + 1; l++)
                if (make_diffs)((int *)buf52[i][j].p)[l] = 0;
                else ((int *)buf52[i][j].p)[l] = n++;
        }
    }

    space_id = H5Screate_simple(2,dims2,NULL);
    type_id = H5Tvlen_create(H5T_NATIVE_INT);
    dset_id = H5Dcreate(loc_id,"vlen2D",type_id,space_id,H5P_DEFAULT);
    status = H5Dwrite(dset_id,type_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf52);
    assert(status>=0);
    status = H5Dvlen_reclaim(type_id,space_id,H5P_DEFAULT,buf52);
    assert(status>=0);
    status = H5Dclose(dset_id);
    status = H5Tclose(type_id);
    status = H5Sclose(space_id);

    /*-------------------------------------------------------------------------
    * H5T_ARRAY
    *-------------------------------------------------------------------------
    */

    if (make_diffs)
    {
        memset(buf62,0,sizeof buf62);
    }


    type_id = H5Tarray_create(H5T_NATIVE_INT, 1, dimarray, NULL);
    write_dset(loc_id,2,dims2,"array2D",type_id,buf62);
    status = H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5T_INTEGER, write a fill value
    *-------------------------------------------------------------------------
    */


    if (make_diffs)
    {
        memset(buf72,0,sizeof buf72);
        memset(buf82,0,sizeof buf82);
    }


    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    space_id = H5Screate_simple(2,dims2,NULL);
    dset_id = H5Dcreate(loc_id,"integer2D",H5T_NATIVE_INT,space_id,plist_id);
    status = H5Dwrite(dset_id,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf72);
    status = H5Pclose(plist_id);
    status = H5Dclose(dset_id);
    status = H5Sclose(space_id);

    /*-------------------------------------------------------------------------
    * H5T_FLOAT
    *-------------------------------------------------------------------------
    */

    write_dset(loc_id,2,dims2,"float2D",H5T_NATIVE_FLOAT,buf82);


    /*-------------------------------------------------------------------------
    * 3D
    *-------------------------------------------------------------------------
    */

    /*-------------------------------------------------------------------------
    * H5T_STRING
    *-------------------------------------------------------------------------
    */

    if (make_diffs)
    {
        memset(buf13,'z',sizeof buf13);
    }

    type_id = H5Tcopy(H5T_C_S1);
    status  = H5Tset_size(type_id, 2);
    write_dset(loc_id,3,dims3,"string3D",type_id,buf13);
    status = H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5T_BITFIELD
    *-------------------------------------------------------------------------
    */


    n=1;
    for (i = 0; i < 4; i++) {
        for (j = 0; j < 3; j++) {
            for (k = 0; k < 2; k++) {
                if (make_diffs) buf23[i][j][k]=0;
                else buf23[i][j][k]=n++;
            }
        }
    }


    type_id = H5Tcopy(H5T_STD_B8LE);
    write_dset(loc_id,3,dims3,"bitfield3D",type_id,buf23);
    status = H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5T_OPAQUE
    *-------------------------------------------------------------------------
    */
    type_id = H5Tcreate(H5T_OPAQUE, 1);
    status = H5Tset_tag(type_id, "1-byte opaque type"); /* must set this */
    write_dset(loc_id,3,dims3,"opaque3D",type_id,buf23);
    status = H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5T_COMPOUND
    *-------------------------------------------------------------------------
    */

    n=1;
    for (i = 0; i < 4; i++) {
        for (j = 0; j < 3; j++) {
            for (k = 0; k < 2; k++) {
                if (make_diffs) {
                    buf33[i][j][k].a=0;
                    buf33[i][j][k].b=0;
                }
                else {
                    buf33[i][j][k].a=n++;
                    buf33[i][j][k].b=n++;
                }
            }
        }
    }


    type_id = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
    H5Tinsert(type_id, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
    H5Tinsert(type_id, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
    write_dset(loc_id,3,dims3,"compound3D",type_id,buf33);
    status = H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5T_REFERENCE
    *-------------------------------------------------------------------------
    */
    /* Create references to dataset */
    if (dset_name)
    {
        for (i = 0; i < 1; i++) {
            for (j = 0; j < 1; j++) {
                for (k = 0; k < 1; k++)
                    status=H5Rcreate(&buf43[i][j][k],file_id,dset_name,H5R_OBJECT,-1);
            }
        }
        write_dset(loc_id,3,dims3r,"refobj3D",H5T_STD_REF_OBJ,buf43);
    }

    /*-------------------------------------------------------------------------
    * H5T_ENUM
    *-------------------------------------------------------------------------
    */

    type_id = H5Tcreate(H5T_ENUM, sizeof(e_t));
    H5Tenum_insert(type_id, "RED",   (val = 0, &val));
    H5Tenum_insert(type_id, "GREEN", (val = 1, &val));
    write_dset(loc_id,3,dims3,"enum3D",type_id,0);
    status = H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5T_VLEN
    *-------------------------------------------------------------------------
    */

    /* Allocate and initialize VL dataset to write */
    n=0;
    for (i = 0; i < 4; i++) {
        for (j = 0; j < 3; j++) {
            for (k = 0; k < 2; k++) {
                int l;
                buf53[i][j][k].p = malloc((i + 1) * sizeof(int));
                buf53[i][j][k].len = i + 1;
                for (l = 0; l < i + 1; l++)
                    if (make_diffs)((int *)buf53[i][j][k].p)[l] = 0;
                    else ((int *)buf53[i][j][k].p)[l] = n++;
            }
        }
    }

    space_id = H5Screate_simple(3,dims3,NULL);
    type_id = H5Tvlen_create(H5T_NATIVE_INT);
    dset_id = H5Dcreate(loc_id,"vlen3D",type_id,space_id,H5P_DEFAULT);
    status = H5Dwrite(dset_id,type_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf53);
    assert(status>=0);
    status = H5Dvlen_reclaim(type_id,space_id,H5P_DEFAULT,buf53);
    assert(status>=0);
    status = H5Dclose(dset_id);
    status = H5Tclose(type_id);
    status = H5Sclose(space_id);

    /*-------------------------------------------------------------------------
    * H5T_ARRAY
    *-------------------------------------------------------------------------
    */


    n=1;
    for (i = 0; i < 24; i++) {
        for (j = 0; j < (int)dimarray[0]; j++) {
            if (make_diffs) buf63[i][j]=0;
            else buf63[i][j]=n++;
        }
    }

    type_id = H5Tarray_create(H5T_NATIVE_INT, 1, dimarray, NULL);
    write_dset(loc_id,3,dims3,"array3D",type_id,buf63);
    status = H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5T_INTEGER and H5T_FLOAT
    *-------------------------------------------------------------------------
    */
    n=1; f=1;
    for (i = 0; i < 4; i++) {
        for (j = 0; j < 3; j++) {
            for (k = 0; k < 2; k++) {
                if (make_diffs) {
                    buf73[i][j][k]=0;
                    buf83[i][j][k]=0;
                }
                else {
                    buf73[i][j][k]=n++;
                    buf83[i][j][k]=f++;
                }
            }
        }
    }
    write_dset(loc_id,3,dims3,"integer3D",H5T_NATIVE_INT,buf73);
    write_dset(loc_id,3,dims3,"float3D",H5T_NATIVE_FLOAT,buf83);
}



/*-------------------------------------------------------------------------
* Function: make_dset_reg_ref
*
* Purpose: write dataset region references
*
*-------------------------------------------------------------------------
*/

#define SPACE1_RANK 1
#define SPACE1_DIM1 1
#define SPACE2_RANK 2
#define SPACE2_DIM1 10
#define SPACE2_DIM2 10
#define NPOINTS 10

void make_dset_reg_ref(hid_t loc_id)
{
    hid_t           dset1; /* Dataset ID   */
    hid_t           dset2; /* Dereferenced dataset ID */
    hid_t           sid1;  /* Dataspace ID #1  */
    hid_t           sid2;  /* Dataspace ID #2  */
    hsize_t         dims1[] = {SPACE1_DIM1};
    hsize_t         dims2[] = {SPACE2_DIM1, SPACE2_DIM2};
    hsize_t         start[SPACE2_RANK];     /* Starting location of hyperslab */
    hsize_t         stride[SPACE2_RANK];    /* Stride of hyperslab */
    hsize_t         count[SPACE2_RANK];     /* Element count of hyperslab */
    hsize_t         block[SPACE2_RANK];     /* Block size of hyperslab */
    hdset_reg_ref_t *wbuf;  /* buffer to write to disk */
    int             *dwbuf; /* Buffer for writing numeric data to disk */
    int             i;      /* counting variables */
    herr_t          ret;    /* Generic return value  */

    /* Allocate write & read buffers */
    wbuf=calloc(sizeof(hdset_reg_ref_t), SPACE1_DIM1);
    dwbuf=malloc(sizeof(int)*SPACE2_DIM1*SPACE2_DIM2);

    /* Create dataspace for datasets */
    sid2 = H5Screate_simple(SPACE2_RANK, dims2, NULL);

    /* Create a dataset */
    dset2=H5Dcreate(loc_id,"dsetreg",H5T_NATIVE_UCHAR,sid2,H5P_DEFAULT);

    for(i=0; i<SPACE2_DIM1*SPACE2_DIM2; i++)
        dwbuf[i]=i*3;

    /* Write selection to disk */
    ret=H5Dwrite(dset2,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,dwbuf);

    /* Close Dataset */
    ret = H5Dclose(dset2);

    /* Create dataspace for the reference dataset */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);

    /* Create a dataset */
    dset1=H5Dcreate(loc_id,"refreg",H5T_STD_REF_DSETREG,sid1,H5P_DEFAULT);

    /* Create references */

    /* Select 6x6 hyperslab for first reference */
    start[0]=2; start[1]=2;
    stride[0]=1; stride[1]=1;
    count[0]=6; count[1]=6;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);

    /* Store dataset region */
    ret = H5Rcreate(&wbuf[0],loc_id,"dsetreg",H5R_DATASET_REGION,sid2);

    /* Write selection to disk */
    ret=H5Dwrite(dset1,H5T_STD_REF_DSETREG,H5S_ALL,H5S_ALL,H5P_DEFAULT,wbuf);

    /* Close all objects */
    ret = H5Sclose(sid1);
    ret = H5Dclose(dset1);
    ret = H5Sclose(sid2);

    free(wbuf);
    free(dwbuf);
}

/*-------------------------------------------------------------------------
* Function: write_attr_in
*
* Purpose: write attributes in LOC_ID (dataset, group, named datatype)
*
* Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
*
* Date: November 12, 2003
*
*-------------------------------------------------------------------------
*/


void write_attr_in(hid_t loc_id,
                   const char* dset_name, /* for saving reference to dataset*/
                   hid_t fid, /* for reference create */
                   int make_diffs /* flag to modify data buffers */)
{
    /* Compound datatype */
    typedef struct s_t
    {
        char   a;
        double b;
    } s_t;

    typedef enum
    {
        RED,
        GREEN
    } e_t;

    hid_t   attr_id;
    hid_t   space_id;
    hid_t   type_id;
    herr_t  status;
    int     val, i, j, k, n;
    float   f;

    /* create 1D attributes with dimension [2], 2 elements */
    hsize_t    dims[1]={2};
    char       buf1[2][2]= {"ab","de"};        /* string */
    char       buf2[2]= {1,2};                 /* bitfield, opaque */
    s_t        buf3[2]= {{1,2},{3,4}};         /* compound */
    hobj_ref_t buf4[2];                        /* reference */
    e_t        buf45[2]= {RED,RED};            /* enum */
    hvl_t      buf5[2];                        /* vlen */
    hsize_t    dimarray[1]={3};                /* array dimension */
    int        buf6[2][3]= {{1,2,3},{4,5,6}};  /* array */
    int        buf7[2]= {1,2};                 /* integer */
    float      buf8[2]= {1,2};                 /* float */

    /* create 2D attributes with dimension [3][2], 6 elements */
    hsize_t    dims2[2]={3,2};
    char       buf12[6][2]= {"ab","cd","ef","gh","ij","kl"};         /* string */
    char       buf22[3][2]= {{1,2},{3,4},{5,6}};                     /* bitfield, opaque */
    s_t        buf32[6]= {{1,2},{3,4},{5,6},{7,8},{9,10},{11,12}};   /* compound */
    hobj_ref_t buf42[3][2];                                          /* reference */
    e_t        buf452[3][2];                                         /* enum */
    hvl_t      buf52[3][2];                                          /* vlen */
    int        buf62[6][3]= {{1,2,3},{4,5,6},{7,8,9},{10,11,12},{13,14,15},{16,17,18}};  /* array */
    int        buf72[3][2]= {{1,2},{3,4},{5,6}};                     /* integer */
    float      buf82[3][2]= {{1,2},{3,4},{5,6}};                     /* float */

    /* create 3D attributes with dimension [4][3][2], 24 elements */
    hsize_t    dims3[3]={4,3,2};
    char       buf13[24][2]= {"ab","cd","ef","gh","ij","kl","mn","pq",
        "rs","tu","vw","xz","AB","CD","EF","GH",
        "IJ","KL","MN","PQ","RS","TU","VW","XZ"};  /* string */
    char       buf23[4][3][2];    /* bitfield, opaque */
    s_t        buf33[4][3][2];    /* compound */
    hobj_ref_t buf43[4][3][2];    /* reference */
    e_t        buf453[4][3][2];   /* enum */
    hvl_t      buf53[4][3][2];    /* vlen */
    int        buf63[24][3];      /* array */
    int        buf73[4][3][2];    /* integer */
    float      buf83[4][3][2];    /* float */


    /*-------------------------------------------------------------------------
    * 1D attributes
    *-------------------------------------------------------------------------
    */

    /*-------------------------------------------------------------------------
    * H5T_STRING
    *-------------------------------------------------------------------------
    */

    if (make_diffs)
    {
        for (i=0; i<2; i++)
            for (j=0; j<2; j++)
            {
                buf1[i][j]='z';
            }
    }
    /*
    buf1[2][2]= {"ab","de"};
    $h5diff file7.h5 file6.h5 g1 g1 -v
    Group:       </g1> and </g1>
    Attribute:   <string> and <string>
    position      string of </g1>  string of </g1> difference
    ------------------------------------------------------------
    [ 0 ]          a                z
    [ 0 ]          b                z
    [ 1 ]          d                z
    [ 1 ]          e                z
    */
    type_id = H5Tcopy(H5T_C_S1);
    status  = H5Tset_size(type_id, 2);
    make_attr(loc_id,1,dims,"string",type_id,buf1);
    status = H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5T_BITFIELD
    *-------------------------------------------------------------------------
    */

    if (make_diffs)
    {
        for (i=0; i<2; i++)
            buf2[i]=buf2[1]=0;
    }
    /*
    buf2[2]= {1,2};
    $h5diff file7.h5 file6.h5 g1 g1 -v
    Group:       </g1> and </g1>
    Attribute:   <bitfield> and <bitfield>
    position      bitfield of </g1> bitfield of </g1> difference
    position        opaque of </g1> opaque of </g1> difference
    ------------------------------------------------------------
    [ 0 ]          1               0               1
    [ 1 ]          2               0               2
    */

    type_id = H5Tcopy(H5T_STD_B8LE);
    make_attr(loc_id,1,dims,"bitfield",type_id,buf2);
    status = H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5T_OPAQUE
    *-------------------------------------------------------------------------
    */

    /*
    buf2[2]= {1,2};
    $h5diff file7.h5 file6.h5 g1 g1 -v
    Group:       </g1> and </g1>
    Attribute:   <opaque> and <opaque>
    position     opaque of </g1> opaque of </g1> difference
    position        opaque of </g1> opaque of </g1> difference
    ------------------------------------------------------------
    [ 0 ]          1               0               1
    [ 1 ]          2               0               2
    */

    type_id = H5Tcreate(H5T_OPAQUE, 1);
    status = H5Tset_tag(type_id, "1-byte opaque type"); /* must set this */
    make_attr(loc_id,1,dims,"opaque",type_id,buf2);
    status = H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5T_COMPOUND
    *-------------------------------------------------------------------------
    */

    if (make_diffs)
    {
        for (i=0; i<2; i++)
        {
            buf3[i].a=0; buf3[i].b=0;
        }
    }

    /*
    buf3[2]= {{1,2},{3,4}};
    $h5diff file7.h5 file6.h5 g1 g1 -v
    Group:       </g1> and </g1>
    Attribute:   <compound> and <compound>
    position        compound of </g1> compound of </g1> difference
    ------------------------------------------------------------
    [ 0 ]          1               5               4
    [ 0 ]          2               5               3
    [ 1 ]          3               5               2
    [ 1 ]          4               5               1
    */

    type_id = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
    H5Tinsert(type_id, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
    H5Tinsert(type_id, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
    make_attr(loc_id,1,dims,"compound",type_id,buf3);
    status = H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5T_REFERENCE
    *-------------------------------------------------------------------------
    */
    /* object references ( H5R_OBJECT  */
    if (dset_name)
    {
        status=H5Rcreate(&buf4[0],fid,dset_name,H5R_OBJECT,-1);
        status=H5Rcreate(&buf4[1],fid,dset_name,H5R_OBJECT,-1);
        make_attr(loc_id,1,dims,"reference",H5T_STD_REF_OBJ,buf4);
    }


    /*-------------------------------------------------------------------------
    * H5T_ENUM
    *-------------------------------------------------------------------------
    */
    if (make_diffs)
    {
        for (i=0; i<2; i++)
        {
            buf45[i]=GREEN;
        }
    }
    /*
    buf45[2]= {RED,RED};
    $h5diff file7.h5 file6.h5 g1 g1 -v
    Group:       </g1> and </g1>
    Attribute:   <enum> and <enum>
    position     enum of </g1>   enum of </g1>   difference
    ------------------------------------------------------------
    [ 0 ]          RED              GREEN
    [ 1 ]          RED              GREEN
    */
    type_id = H5Tcreate(H5T_ENUM, sizeof(e_t));
    H5Tenum_insert(type_id, "RED",   (val = 0, &val));
    H5Tenum_insert(type_id, "GREEN", (val = 1, &val));
    make_attr(loc_id,1,dims,"enum",type_id,buf45);
    status = H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5T_VLEN
    *-------------------------------------------------------------------------
    */

    /* Allocate and initialize VL dataset to write */

    buf5[0].len = 1;
    buf5[0].p = malloc( 1 * sizeof(int));
    ((int *)buf5[0].p)[0]=1;
    buf5[1].len = 2;
    buf5[1].p = malloc( 2 * sizeof(int));
    ((int *)buf5[1].p)[0]=2;
    ((int *)buf5[1].p)[1]=3;

    if (make_diffs)
    {
        ((int *)buf5[0].p)[0]=0;
        ((int *)buf5[1].p)[0]=0;
        ((int *)buf5[1].p)[1]=0;
    }
    /*
    $h5diff file7.h5 file6.h5 g1 g1 -v
    Group:       </g1> and </g1>
    position        vlen of </g1>   vlen of </g1>   difference
    ------------------------------------------------------------
    [ 0 ]          1               0               1
    [ 1 ]          2               0               2
    [ 1 ]          3               0               3
    */

    space_id = H5Screate_simple(1,dims,NULL);
    type_id = H5Tvlen_create(H5T_NATIVE_INT);
    attr_id = H5Acreate(loc_id,"vlen",type_id,space_id,H5P_DEFAULT);
    status = H5Awrite(attr_id,type_id,buf5);
    assert(status>=0);
    status = H5Dvlen_reclaim(type_id,space_id,H5P_DEFAULT,buf5);
    assert(status>=0);
    status = H5Aclose(attr_id);
    status = H5Tclose(type_id);
    status = H5Sclose(space_id);

    /*-------------------------------------------------------------------------
    * H5T_ARRAY
    *-------------------------------------------------------------------------
    */

    if (make_diffs)
    {
        for (i=0; i<2; i++)
            for (j=0; j<3; j++)
            {
                buf6[i][j]=0;
            }
    }
    /*
    buf6[2][3]= {{1,2,3},{4,5,6}};
    $h5diff file7.h5 file6.h5 g1 g1 -v
    Group:       </g1> and </g1>
    Attribute:   <array> and <array>
    position        array of </g1>  array of </g1>  difference
    ------------------------------------------------------------
    [ 0 ]          1               0               1
    [ 0 ]          2               0               2
    [ 0 ]          3               0               3
    [ 1 ]          4               0               4
    [ 1 ]          5               0               5
    [ 1 ]          6               0               6
    */
    type_id = H5Tarray_create(H5T_NATIVE_INT, 1, dimarray, NULL);
    make_attr(loc_id,1,dims,"array",type_id,buf6);
    status = H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5T_INTEGER and H5T_FLOAT
    *-------------------------------------------------------------------------
    */

    if (make_diffs)
    {
        for (i=0; i<2; i++)
        {
            buf7[i]=0;
            buf8[i]=0;
        }
    }
    /*
    buf7[2]= {1,2};
    buf8[2]= {1,2};
    $h5diff file7.h5 file6.h5 g1 g1 -v
    Group:       </g1> and </g1>
    position        integer of </g1> integer of </g1> difference
    ------------------------------------------------------------
    [ 0 ]          1               0               1
    [ 1 ]          2               0               2
    position        float of </g1>  float of </g1>  difference
    ------------------------------------------------------------
    [ 0 ]          1               0               1
    [ 1 ]          2               0               2
    */
    make_attr(loc_id,1,dims,"integer",H5T_NATIVE_INT,buf7);
    make_attr(loc_id,1,dims,"float",H5T_NATIVE_FLOAT,buf8);


    /*-------------------------------------------------------------------------
    * 2D attributes
    *-------------------------------------------------------------------------
    */

    /*-------------------------------------------------------------------------
    * H5T_STRING
    *-------------------------------------------------------------------------
    */
    if (make_diffs)
    {
        memset(buf12, 'z', sizeof buf12);
    }

    /*
    buf12[6][2]= {"ab","cd","ef","gh","ij","kl"};
    $h5diff file7.h5 file6.h5 g1 g1 -v
    Attribute:   <string2D> and <string2D>
    position        string2D of </g1> string2D of </g1> difference
    ------------------------------------------------------------
    [ 0 0 ]          a                z
    [ 0 0 ]          b                z
    [ 0 1 ]          c                z
    [ 0 1 ]          d                z
    [ 1 0 ]          e                z
    [ 1 0 ]          f                z
    [ 1 1 ]          g                z
    [ 1 1 ]          h                z
    [ 2 0 ]          i                z
    [ 2 0 ]          j                z
    [ 2 1 ]          k                z
    [ 2 1 ]          l                z
    */

    type_id = H5Tcopy(H5T_C_S1);
    status  = H5Tset_size(type_id, 2);
    make_attr(loc_id,2,dims2,"string2D",type_id,buf12);
    status = H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5T_BITFIELD
    *-------------------------------------------------------------------------
    */

    if (make_diffs)
    {
        memset(buf22,0,sizeof buf22);
    }

    /*
    buf22[3][2]= {{1,2},{3,4},{5,6}};
    $h5diff file7.h5 file6.h5 g1 g1 -v
    Attribute:   <bitfield2D> and <bitfield2D>
    position        bitfield2D of </g1> bitfield2D of </g1> difference
    ------------------------------------------------------------
    [ 0 0 ]          1               0               1
    [ 0 1 ]          2               0               2
    [ 1 0 ]          3               0               3
    [ 1 1 ]          4               0               4
    [ 2 0 ]          5               0               5
    [ 2 1 ]          6               0               6
    */


    type_id = H5Tcopy(H5T_STD_B8LE);
    make_attr(loc_id,2,dims2,"bitfield2D",type_id,buf22);
    status = H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5T_OPAQUE
    *-------------------------------------------------------------------------
    */

    /*
    buf22[3][2]= {{1,2},{3,4},{5,6}};
    $h5diff file7.h5 file6.h5 g1 g1 -v
    Attribute:   <opaque2D> and <opaque2D>
    position        opaque2D of </g1> opaque2D of </g1> difference
    ------------------------------------------------------------
    [ 0 0 ]          1               0               1
    [ 0 1 ]          2               0               2
    [ 1 0 ]          3               0               3
    [ 1 1 ]          4               0               4
    [ 2 0 ]          5               0               5
    [ 2 1 ]          6               0               6
    */
    type_id = H5Tcreate(H5T_OPAQUE, 1);
    status = H5Tset_tag(type_id, "1-byte opaque type"); /* must set this */
    make_attr(loc_id,2,dims2,"opaque2D",type_id,buf22);
    status = H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5T_COMPOUND
    *-------------------------------------------------------------------------
    */
    if (make_diffs)
    {
        memset(buf32,0,sizeof buf32);
    }

    /*
    buf32[6]= {{1,2},{3,4},{5,6},{7,8},{9,10},{11,12}};
    $h5diff file7.h5 file6.h5 g1 g1 -v
    Attribute:   <opaque2D> and <opaque2D>
    position        opaque2D of </g1> opaque2D of </g1> difference
    ------------------------------------------------------------
    [ 0 0 ]          1               0               1
    [ 0 1 ]          2               0               2
    [ 1 0 ]          3               0               3
    [ 1 1 ]          4               0               4
    [ 2 0 ]          5               0               5
    [ 2 1 ]          6               0               6
    */


    type_id = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
    H5Tinsert(type_id, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
    H5Tinsert(type_id, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
    make_attr(loc_id,2,dims2,"compound2D",type_id,buf32);
    status = H5Tclose(type_id);

    /*-------------------------------------------------------------------------
    * H5T_REFERENCE
    *-------------------------------------------------------------------------
    */
    /* Create references to dataset */
    if (dset_name)
    {
        for (i = 0; i < 3; i++) {
            for (j = 0; j < 2; j++) {
                status=H5Rcreate(&buf42[i][j],fid,dset_name,H5R_OBJECT,-1);
            }
        }
        make_attr(loc_id,2,dims2,"reference2D",H5T_STD_REF_OBJ,buf42);
    }

    /*-------------------------------------------------------------------------
    * H5T_ENUM
    *-------------------------------------------------------------------------
    */
    for (i=0; i<3; i++)
        for (j=0; j<2; j++)
        {
            if (make_diffs) buf452[i][j]=GREEN; else buf452[i][j]=RED;
        }

        /*
        Attribute:   <enum2D> and <enum2D>
        position        enum2D of </g1> enum2D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 ]          RED              GREEN
        [ 0 1 ]          RED              GREEN
        [ 1 0 ]          RED              GREEN
        [ 1 1 ]          RED              GREEN
        [ 2 0 ]          RED              GREEN
        [ 2 1 ]          RED              GREEN
        */

        type_id = H5Tcreate(H5T_ENUM, sizeof(e_t));
        H5Tenum_insert(type_id, "RED",   (val = 0, &val));
        H5Tenum_insert(type_id, "GREEN", (val = 1, &val));
        make_attr(loc_id,2,dims2,"enum2D",type_id,buf452);
        status = H5Tclose(type_id);

        /*-------------------------------------------------------------------------
        * H5T_VLEN
        *-------------------------------------------------------------------------
        */

        /* Allocate and initialize VL dataset to write */
        n=0;
        for (i = 0; i < 3; i++) {
            for (j = 0; j < 2; j++) {
                int l;
                buf52[i][j].p = malloc((i + 1) * sizeof(int));
                buf52[i][j].len = i + 1;
                for (l = 0; l < i + 1; l++)
                    if (make_diffs)((int *)buf52[i][j].p)[l] = 0;
                    else ((int *)buf52[i][j].p)[l] = n++;
            }
        }

        /*
        position        vlen2D of </g1> vlen2D of </g1> difference
        ------------------------------------------------------------
        [ 0 1 ]          1               0               1
        [ 1 0 ]          2               0               2
        [ 1 0 ]          3               0               3
        [ 1 1 ]          4               0               4
        [ 1 1 ]          5               0               5
        [ 2 0 ]          6               0               6
        [ 2 0 ]          7               0               7
        [ 2 0 ]          8               0               8
        [ 2 1 ]          9               0               9
        [ 2 1 ]          10              0               10
        [ 2 1 ]          11              0               11
        */

        space_id = H5Screate_simple(2,dims2,NULL);
        type_id = H5Tvlen_create(H5T_NATIVE_INT);
        attr_id = H5Acreate(loc_id,"vlen2D",type_id,space_id,H5P_DEFAULT);
        status = H5Awrite(attr_id,type_id,buf52);
        assert(status>=0);
        status = H5Dvlen_reclaim(type_id,space_id,H5P_DEFAULT,buf52);
        assert(status>=0);
        status = H5Aclose(attr_id);
        status = H5Tclose(type_id);
        status = H5Sclose(space_id);

        /*-------------------------------------------------------------------------
        * H5T_ARRAY
        *-------------------------------------------------------------------------
        */

        if (make_diffs)
        {
            memset(buf62,0,sizeof buf62);
        }
        /*
        buf62[6][3]= {{1,2,3},{4,5,6},{7,8,9},{10,11,12},{13,14,15},{16,17,18}};
        $h5diff file7.h5 file6.h5 g1 g1 -v
        Group:       </g1> and </g1>
        Attribute:   <array2D> and <array2D>
        position        array2D of </g1> array2D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 ]          1               0               1
        [ 0 0 ]          2               0               2
        [ 0 0 ]          3               0               3
        [ 0 1 ]          4               0               4
        [ 0 1 ]          5               0               5
        [ 0 1 ]          6               0               6
        [ 1 0 ]          7               0               7
        [ 1 0 ]          8               0               8
        [ 1 0 ]          9               0               9
        [ 1 1 ]          10              0               10
        [ 1 1 ]          11              0               11
        [ 1 1 ]          12              0               12
        [ 2 0 ]          13              0               13
        [ 2 0 ]          14              0               14
        [ 2 0 ]          15              0               15
        [ 2 1 ]          16              0               16
        [ 2 1 ]          17              0               17
        [ 2 1 ]          18              0               18
        */
        type_id = H5Tarray_create(H5T_NATIVE_INT, 1, dimarray, NULL);
        make_attr(loc_id,2,dims2,"array2D",type_id,buf62);
        status = H5Tclose(type_id);

        /*-------------------------------------------------------------------------
        * H5T_INTEGER and H5T_FLOAT
        *-------------------------------------------------------------------------
        */

        if (make_diffs)
        {
            memset(buf72,0,sizeof buf72);
            memset(buf82,0,sizeof buf82);
        }
        /*
        Attribute:   <integer2D> and <integer2D>
        position        integer2D of </g1> integer2D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 ]          1               0               1
        [ 0 1 ]          2               0               2
        [ 1 0 ]          3               0               3
        [ 1 1 ]          4               0               4
        [ 2 0 ]          5               0               5
        [ 2 1 ]          6               0               6
        6 differences found
        Attribute:   <float2D> and <float2D>
        position        float2D of </g1> float2D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 ]          1               0               1
        [ 0 1 ]          2               0               2
        [ 1 0 ]          3               0               3
        [ 1 1 ]          4               0               4
        [ 2 0 ]          5               0               5
        [ 2 1 ]          6               0               6
        */

        make_attr(loc_id,2,dims2,"integer2D",H5T_NATIVE_INT,buf72);
        make_attr(loc_id,2,dims2,"float2D",H5T_NATIVE_FLOAT,buf82);


        /*-------------------------------------------------------------------------
        * 3D attributes
        *-------------------------------------------------------------------------
        */

        /*-------------------------------------------------------------------------
        * H5T_STRING
        *-------------------------------------------------------------------------
        */

        if (make_diffs)
        {
            memset(buf13,'z',sizeof buf13);
        }

        /*
        buf13[24][2]= {"ab","cd","ef","gh","ij","kl","mn","pq",
        "rs","tu","vw","xz","AB","CD","EF","GH",
        "IJ","KL","MN","PQ","RS","TU","VW","XZ"};

        Attribute:   <string3D> and <string3D>
        position        string3D of </g1> string3D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 0 ]          a                z
        [ 0 0 0 ]          b                z
        [ 0 0 1 ]          c                z
        [ 0 0 1 ]          d                z
        [ 0 1 0 ]          e                z
        [ 0 1 0 ]          f                z
        [ 0 1 1 ]          g                z
        [ 0 1 1 ]          h                z
        [ 0 2 0 ]          i                z
        [ 0 2 0 ]          j                z
        [ 0 2 1 ]          k                z
        [ 0 2 1 ]          l                z
        [ 1 0 0 ]          m                z
        [ 1 0 0 ]          n                z
        [ 1 0 1 ]          p                z
        [ 1 0 1 ]          q                z
        [ 1 1 0 ]          r                z
        [ 1 1 0 ]          s                z
        [ 1 1 1 ]          t                z
        [ 1 1 1 ]          u                z
        [ 1 2 0 ]          v                z
        [ 1 2 0 ]          w                z
        [ 1 2 1 ]          x                z
        [ 2 0 0 ]          A                z
        [ 2 0 0 ]          B                z
        [ 2 0 1 ]          C                z
        [ 2 0 1 ]          D                z
        [ 2 1 0 ]          E                z
        [ 2 1 0 ]          F                z
        [ 2 1 1 ]          G                z
        [ 2 1 1 ]          H                z
        [ 2 2 0 ]          I                z
        [ 2 2 0 ]          J                z
        [ 2 2 1 ]          K                z
        [ 2 2 1 ]          L                z
        [ 3 0 0 ]          M                z
        [ 3 0 0 ]          N                z
        [ 3 0 1 ]          P                z
        [ 3 0 1 ]          Q                z
        [ 3 1 0 ]          R                z
        [ 3 1 0 ]          S                z
        [ 3 1 1 ]          T                z
        [ 3 1 1 ]          U                z
        [ 3 2 0 ]          V                z
        [ 3 2 0 ]          W                z
        [ 3 2 1 ]          X                z
        [ 3 2 1 ]          Z                z
        */

        type_id = H5Tcopy(H5T_C_S1);
        status  = H5Tset_size(type_id, 2);
        make_attr(loc_id,3,dims3,"string3D",type_id,buf13);
        status = H5Tclose(type_id);

        /*-------------------------------------------------------------------------
        * H5T_BITFIELD
        *-------------------------------------------------------------------------
        */

        n=1;
        for (i = 0; i < 4; i++) {
            for (j = 0; j < 3; j++) {
                for (k = 0; k < 2; k++) {
                    if (make_diffs) buf23[i][j][k]=0;
                    else buf23[i][j][k]=n++;
                }
            }
        }

        /*
        position        bitfield3D of </g1> bitfield3D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 0 ]          1               0               1
        [ 0 0 1 ]          2               0               2
        [ 0 1 0 ]          3               0               3
        [ 0 1 1 ]          4               0               4
        [ 0 2 0 ]          5               0               5
        [ 0 2 1 ]          6               0               6
        [ 1 0 0 ]          7               0               7
        [ 1 0 1 ]          8               0               8
        [ 1 1 0 ]          9               0               9
        [ 1 1 1 ]          10              0               10
        [ 1 2 0 ]          11              0               11
        [ 1 2 1 ]          12              0               12
        [ 2 0 0 ]          13              0               13
        [ 2 0 1 ]          14              0               14
        [ 2 1 0 ]          15              0               15
        [ 2 1 1 ]          16              0               16
        [ 2 2 0 ]          17              0               17
        [ 2 2 1 ]          18              0               18
        [ 3 0 0 ]          19              0               19
        [ 3 0 1 ]          20              0               20
        [ 3 1 0 ]          21              0               21
        [ 3 1 1 ]          22              0               22
        [ 3 2 0 ]          23              0               23
        [ 3 2 1 ]          24              0               24
        */

        type_id = H5Tcopy(H5T_STD_B8LE);
        make_attr(loc_id,3,dims3,"bitfield3D",type_id,buf23);
        status = H5Tclose(type_id);

        /*-------------------------------------------------------------------------
        * H5T_OPAQUE
        *-------------------------------------------------------------------------
        */
        type_id = H5Tcreate(H5T_OPAQUE, 1);
        status = H5Tset_tag(type_id, "1-byte opaque type"); /* must set this */
        make_attr(loc_id,3,dims3,"opaque3D",type_id,buf23);
        status = H5Tclose(type_id);

        /*-------------------------------------------------------------------------
        * H5T_COMPOUND
        *-------------------------------------------------------------------------
        */

        n=1;
        for (i = 0; i < 4; i++) {
            for (j = 0; j < 3; j++) {
                for (k = 0; k < 2; k++) {
                    if (make_diffs) {
                        buf33[i][j][k].a=0;
                        buf33[i][j][k].b=0;
                    }
                    else {
                        buf33[i][j][k].a=n++;
                        buf33[i][j][k].b=n++;
                    }
                }
            }
        }
        /*position        compound3D of </g1> compound3D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 0 ]          1               0               1
        [ 0 0 0 ]          2               0               2
        [ 0 0 1 ]          3               0               3
        [ 0 0 1 ]          4               0               4
        [ 0 1 0 ]          5               0               5
        [ 0 1 0 ]          6               0               6
        [ 0 1 1 ]          7               0               7
        [ 0 1 1 ]          8               0               8
        [ 0 2 0 ]          9               0               9
        [ 0 2 0 ]          10              0               10
        [ 0 2 1 ]          11              0               11
        [ 0 2 1 ]          12              0               12
        [ 1 0 0 ]          13              0               13
        [ 1 0 0 ]          14              0               14
        [ 1 0 1 ]          15              0               15
        [ 1 0 1 ]          16              0               16
        [ 1 1 0 ]          17              0               17
        [ 1 1 0 ]          18              0               18
        [ 1 1 1 ]          19              0               19
        [ 1 1 1 ]          20              0               20
        [ 1 2 0 ]          21              0               21
        [ 1 2 0 ]          22              0               22
        [ 1 2 1 ]          23              0               23
        [ 1 2 1 ]          24              0               24
        [ 2 0 0 ]          25              0               25
        [ 2 0 0 ]          26              0               26
        [ 2 0 1 ]          27              0               27
        [ 2 0 1 ]          28              0               28
        [ 2 1 0 ]          29              0               29
        [ 2 1 0 ]          30              0               30
        [ 2 1 1 ]          31              0               31
        [ 2 1 1 ]          32              0               32
        [ 2 2 0 ]          33              0               33
        [ 2 2 0 ]          34              0               34
        [ 2 2 1 ]          35              0               35
        [ 2 2 1 ]          36              0               36
        [ 3 0 0 ]          37              0               37
        [ 3 0 0 ]          38              0               38
        [ 3 0 1 ]          39              0               39
        [ 3 0 1 ]          40              0               40
        [ 3 1 0 ]          41              0               41
        [ 3 1 0 ]          42              0               42
        [ 3 1 1 ]          43              0               43
        [ 3 1 1 ]          44              0               44
        [ 3 2 0 ]          45              0               45
        [ 3 2 0 ]          46              0               46
        [ 3 2 1 ]          47              0               47
        [ 3 2 1 ]          48              0               48
        */



        type_id = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
        H5Tinsert(type_id, "a", HOFFSET(s_t, a), H5T_NATIVE_CHAR);
        H5Tinsert(type_id, "b", HOFFSET(s_t, b), H5T_NATIVE_DOUBLE);
        make_attr(loc_id,3,dims3,"compound3D",type_id,buf33);
        status = H5Tclose(type_id);

        /*-------------------------------------------------------------------------
        * H5T_REFERENCE
        *-------------------------------------------------------------------------
        */
        /* Create references to dataset */
        if (dset_name)
        {
            for (i = 0; i < 4; i++) {
                for (j = 0; j < 3; j++) {
                    for (k = 0; k < 2; k++)
                        status=H5Rcreate(&buf43[i][j][k],fid,dset_name,H5R_OBJECT,-1);
                }
            }
            make_attr(loc_id,3,dims3,"reference3D",H5T_STD_REF_OBJ,buf43);
        }

        /*-------------------------------------------------------------------------
        * H5T_ENUM
        *-------------------------------------------------------------------------
        */

        for (i = 0; i < 4; i++) {
            for (j = 0; j < 3; j++) {
                for (k = 0; k < 2; k++) {
                    if (make_diffs) buf453[i][j][k]=RED; else buf453[i][j][k]=GREEN;
                }
            }
        }

        /*
        position        enum3D of </g1> enum3D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 0 ]          GREEN            RED
        [ 0 0 1 ]          GREEN            RED
        [ 0 1 0 ]          GREEN            RED
        [ 0 1 1 ]          GREEN            RED
        [ 0 2 0 ]          GREEN            RED
        [ 0 2 1 ]          GREEN            RED
        [ 1 0 0 ]          GREEN            RED
        [ 1 0 1 ]          GREEN            RED
        [ 1 1 0 ]          GREEN            RED
        [ 1 1 1 ]          GREEN            RED
        [ 1 2 0 ]          GREEN            RED
        [ 1 2 1 ]          GREEN            RED
        [ 2 0 0 ]          GREEN            RED
        [ 2 0 1 ]          GREEN            RED
        [ 2 1 0 ]          GREEN            RED
        [ 2 1 1 ]          GREEN            RED
        [ 2 2 0 ]          GREEN            RED
        [ 2 2 1 ]          GREEN            RED
        [ 3 0 0 ]          GREEN            RED
        [ 3 0 1 ]          GREEN            RED
        [ 3 1 0 ]          GREEN            RED
        [ 3 1 1 ]          GREEN            RED
        [ 3 2 0 ]          GREEN            RED
        [ 3 2 1 ]          GREEN            RED
        */


        type_id = H5Tcreate(H5T_ENUM, sizeof(e_t));
        H5Tenum_insert(type_id, "RED",   (val = 0, &val));
        H5Tenum_insert(type_id, "GREEN", (val = 1, &val));
        make_attr(loc_id,3,dims3,"enum3D",type_id,buf453);
        status = H5Tclose(type_id);

        /*-------------------------------------------------------------------------
        * H5T_VLEN
        *-------------------------------------------------------------------------
        */

        /* Allocate and initialize VL dataset to write */
        n=0;
        for (i = 0; i < 4; i++) {
            for (j = 0; j < 3; j++) {
                for (k = 0; k < 2; k++) {
                    int l;
                    buf53[i][j][k].p = malloc((i + 1) * sizeof(int));
                    buf53[i][j][k].len = i + 1;
                    for (l = 0; l < i + 1; l++)
                        if (make_diffs)((int *)buf53[i][j][k].p)[l] = 0;
                        else ((int *)buf53[i][j][k].p)[l] = n++;
                }
            }
        }
        /*
        position        vlen3D of </g1> vlen3D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 1 ]          1               0               1
        [ 0 1 0 ]          2               0               2
        [ 0 1 1 ]          3               0               3
        [ 0 2 0 ]          4               0               4
        [ 0 2 1 ]          5               0               5
        [ 1 0 0 ]          6               0               6
        [ 1 0 0 ]          7               0               7
        [ 1 0 1 ]          8               0               8
        [ 1 0 1 ]          9               0               9
        [ 1 1 0 ]          10              0               10
        etc
        */
        space_id = H5Screate_simple(3,dims3,NULL);
        type_id = H5Tvlen_create(H5T_NATIVE_INT);
        attr_id = H5Acreate(loc_id,"vlen3D",type_id,space_id,H5P_DEFAULT);
        status = H5Awrite(attr_id,type_id,buf53);
        assert(status>=0);
        status = H5Dvlen_reclaim(type_id,space_id,H5P_DEFAULT,buf53);
        assert(status>=0);
        status = H5Aclose(attr_id);
        status = H5Tclose(type_id);
        status = H5Sclose(space_id);

        /*-------------------------------------------------------------------------
        * H5T_ARRAY
        *-------------------------------------------------------------------------
        */
        n=1;
        for (i = 0; i < 24; i++) {
            for (j = 0; j < (int)dimarray[0]; j++) {
                if (make_diffs) buf63[i][j]=0;
                else buf63[i][j]=n++;
            }
        }
        /*
        position        array3D of </g1> array3D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 0 ]          1               0               1
        [ 0 0 0 ]          2               0               2
        [ 0 0 0 ]          3               0               3
        [ 0 0 1 ]          4               0               4
        [ 0 0 1 ]          5               0               5
        [ 0 0 1 ]          6               0               6
        [ 0 1 0 ]          7               0               7
        etc
        */

        type_id = H5Tarray_create(H5T_NATIVE_INT, 1, dimarray, NULL);
        make_attr(loc_id,3,dims3,"array3D",type_id,buf63);
        status = H5Tclose(type_id);

        /*-------------------------------------------------------------------------
        * H5T_INTEGER and H5T_FLOAT
        *-------------------------------------------------------------------------
        */
        n=1; f=1;
        for (i = 0; i < 4; i++) {
            for (j = 0; j < 3; j++) {
                for (k = 0; k < 2; k++) {
                    if (make_diffs) {
                        buf73[i][j][k]=0;
                        buf83[i][j][k]=0;
                    }
                    else {
                        buf73[i][j][k]=n++;
                        buf83[i][j][k]=f++;
                    }
                }
            }
        }

        /*
        position        integer3D of </g1> integer3D of </g1> difference
        ------------------------------------------------------------
        [ 0 0 0 ]          1               0               1
        [ 0 0 1 ]          2               0               2
        [ 0 1 0 ]          3               0               3
        [ 0 1 1 ]          4               0               4
        [ 0 2 0 ]          5               0               5
        [ 0 2 1 ]          6               0               6
        [ 1 0 0 ]          7               0               7
        [ 1 0 1 ]          8               0               8
        [ 1 1 0 ]          9               0               9
        [ 1 1 1 ]          10              0               10
        etc
        */
        make_attr(loc_id,3,dims3,"integer3D",H5T_NATIVE_INT,buf73);
        make_attr(loc_id,3,dims3,"float3D",H5T_NATIVE_FLOAT,buf83);
}



/*-------------------------------------------------------------------------
* Function: make_dset
*
* Purpose: utility function to create and write a dataset in LOC_ID
*
* Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
*
* Date: November 12, 2003
*
*-------------------------------------------------------------------------
*/
int make_dset(hid_t loc_id,
              const char *name,
              hid_t sid,
              hid_t dcpl,
              void *buf)
{
    hid_t   dsid;

    /* create the dataset */
    if((dsid = H5Dcreate (loc_id,name,H5T_NATIVE_INT,sid,dcpl))<0)
        return -1;

    /* write */
    if(H5Dwrite(dsid,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf)<0)
        goto out;

    /* close */
    if(H5Dclose(dsid)<0)
        return -1;

    return 0;
out:
    H5E_BEGIN_TRY {
        H5Dclose(dsid);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
* Function: write_dset
*
* Purpose: utility function to create and write a dataset in LOC_ID
*
* Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
*
* Date: November 12, 2003
*
*-------------------------------------------------------------------------
*/

int write_dset( hid_t loc_id,
               int rank,
               hsize_t *dims,
               const char *dset_name,
               hid_t type_id,
               void *buf )
{
    hid_t   dset_id;
    hid_t   space_id;

    /* Create a buf space  */
    if ((space_id = H5Screate_simple(rank,dims,NULL))<0)
        return -1;

    /* Create a dataset */
    if ((dset_id = H5Dcreate(loc_id,dset_name,type_id,space_id,H5P_DEFAULT))<0)
        return -1;

    /* Write the buf */
    if ( buf )
        if (H5Dwrite(dset_id,type_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf)<0)
            return -1;

    /* Close */
    if (H5Dclose(dset_id)<0)
        return -1;
    if (H5Sclose(space_id)<0)
        return -1;

    return 0;

}


/*-------------------------------------------------------------------------
* Function: write_attr
*
* Purpose: utility function to write an attribute in LOC_ID
*
* Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
*
* Date: November 12, 2003
*
*-------------------------------------------------------------------------
*/

int make_attr(hid_t loc_id,
              int rank,
              hsize_t *dims,
              const char *attr_name,
              hid_t type_id,
              void *buf)
{
    hid_t   attr_id;
    hid_t   space_id;

    /* create a space  */
    if ((space_id = H5Screate_simple(rank,dims,NULL))<0)
        return -1;

    /* create the attribute */
    if ((attr_id = H5Acreate(loc_id,attr_name,type_id,space_id,H5P_DEFAULT))<0)
        goto out;

    /* write the buffer */
    if ( buf )
    {
        if (H5Awrite(attr_id,type_id,buf)<0)
            goto out;
    }

    /* close */
    H5Aclose(attr_id);
    H5Sclose(space_id);
    return 0;

out:
    H5E_BEGIN_TRY {
        H5Aclose(attr_id);
        H5Sclose(space_id);
    } H5E_END_TRY;
    return -1;
}



