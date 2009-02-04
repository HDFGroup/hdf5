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

/*
 * Programmer:  Pedro Vicente <pvn@ncsa.uiuc.edu>
 *              April 12, 2002
 *
 * Purpose:     Tests the H5Dset_extent call
 */

#include "hdf5.h"
#include "h5test.h"

/*-------------------------------------------------------------------------
 *
 * Tests the function H5Dset_extent.
 *
 *-------------------------------------------------------------------------
 */


const char *FILENAME[] = {
    "set_extent1",
    "set_extent2",
    "set_extent3",
    "set_extent4",
    "set_extent5",
    NULL
};

#define NAME_BUF_SIZE   1024
#define EXT_FILE_NAME1 "ext1.bin"
#define EXT_FILE_NAME2 "ext2.bin"


#define RANK1 1
#define RANK2 2
#define RANK3 3
#define DIM0  4
#define DIM1  4
#define DIM2  4
#define DIMS0 2
#define DIMS1 2
#define DIMS2 2
#define DIME0 7
#define DIME1 7
#define DIME2 7
#define ISTORE_IK  64

static int do_ranks( hid_t fapl );
static int do_layouts( hid_t fapl );

static int test_rank1( hbool_t do_compress, 
                       hbool_t do_fill_value, 
                       hbool_t set_istore_k,
                       H5D_fill_time_t fill_time,
                       hid_t fapl);
static int test_rank2( hbool_t do_compress, 
                       hbool_t do_fill_value, 
                       hbool_t set_istore_k,
                       H5D_fill_time_t fill_time,
                       hid_t fapl);
static int test_rank3( hbool_t do_compress, 
                       hbool_t do_fill_value, 
                       hbool_t set_istore_k,
                       H5D_fill_time_t fill_time,
                       hid_t fapl);

static int test_external( hid_t fapl );
static int test_layouts( H5D_layout_t layout, hid_t fapl );

/*-------------------------------------------------------------------------
 * main
 *-------------------------------------------------------------------------
 */

int main( void )
{

    hid_t fapl;         /* file access property list */
    int	  nerrors = 0;

    h5_reset();
    fapl = h5_fileaccess();

    nerrors += do_ranks( fapl ) < 0 ? 1 : 0;
    nerrors += test_external( fapl ) < 0 ? 1 : 0;
    nerrors += do_layouts( fapl ) < 0 ? 1 : 0;
   
    h5_cleanup(FILENAME, fapl);
    HDremove(EXT_FILE_NAME1);
    HDremove(EXT_FILE_NAME2);

    if(nerrors) 
    {
        printf("***** %d H5Dset_extent TEST%s FAILED! *****\n",
            nerrors, 1 == nerrors ? "" : "S");
        exit(1);
    }

    puts("All H5Dset_extent tests passed.");

    return 0;

}



/*-------------------------------------------------------------------------
* test with several ranks
*-------------------------------------------------------------------------
*/
static int do_ranks( hid_t fapl )
{

    hbool_t do_compress = 0;
    hbool_t do_fillvalue = 0;
    hbool_t set_istore_k = 0;
      
      
    TESTING("with fill value, no compression");

    do_fillvalue = 1;

    if (test_rank1( do_compress, do_fillvalue, set_istore_k, H5D_FILL_TIME_ALLOC, fapl ) < 0)
    {
        goto error;
    } 
    if (test_rank1( do_compress, do_fillvalue, set_istore_k, H5D_FILL_TIME_IFSET, fapl ) < 0)
    {
        goto error;
    } 
    if (test_rank2( do_compress, do_fillvalue, set_istore_k, H5D_FILL_TIME_ALLOC, fapl ) < 0)
    {
        goto error;
    } 
    if (test_rank2( do_compress, do_fillvalue, set_istore_k, H5D_FILL_TIME_IFSET, fapl ) < 0)
    {
        goto error;
    } 
    if (test_rank3( do_compress, do_fillvalue, set_istore_k, H5D_FILL_TIME_ALLOC, fapl ) < 0)
    {
        goto error;
    } 
    if (test_rank3( do_compress, do_fillvalue, set_istore_k, H5D_FILL_TIME_IFSET, fapl ) < 0)
    {
        goto error;
    } 
   


    PASSED();


    TESTING("no fill value, no compression");

    do_fillvalue = 0;

    if (test_rank1( do_compress, do_fillvalue, set_istore_k, H5D_FILL_TIME_ERROR, fapl  ) < 0)
    {
        goto error;
    }
    if (test_rank2( do_compress, do_fillvalue, set_istore_k, H5D_FILL_TIME_ERROR, fapl  ) < 0)
    {
        goto error;
    }
    if (test_rank3( do_compress, do_fillvalue, set_istore_k, H5D_FILL_TIME_ERROR, fapl  ) < 0)
    {
        goto error;
    }
   
   

    PASSED();
    
    TESTING("with fill value, with compression");

#ifdef H5_HAVE_FILTER_DEFLATE

    do_compress = 1;
    do_fillvalue = 1;

    if (test_rank1( do_compress, do_fillvalue, set_istore_k, H5D_FILL_TIME_ALLOC, fapl ) < 0)
    {
        goto error;
    }
    if (test_rank1( do_compress, do_fillvalue, set_istore_k, H5D_FILL_TIME_IFSET, fapl ) < 0)
    {
        goto error;
    }
    if (test_rank2( do_compress, do_fillvalue, set_istore_k, H5D_FILL_TIME_ALLOC, fapl ) < 0)
    {
        goto error;
    }
    if (test_rank2( do_compress, do_fillvalue, set_istore_k, H5D_FILL_TIME_IFSET, fapl ) < 0)
    {
        goto error;
    }
    if (test_rank3( do_compress, do_fillvalue, set_istore_k, H5D_FILL_TIME_ALLOC, fapl ) < 0)
    {
        goto error;
    }
    if (test_rank3( do_compress, do_fillvalue, set_istore_k, H5D_FILL_TIME_IFSET, fapl ) < 0)
    {
        goto error;
    }
  
     
    PASSED();
#else
    SKIPPED();
#endif

    TESTING("no fill value, with compression");

#ifdef H5_HAVE_FILTER_DEFLATE

    do_fillvalue = 0;

    if (test_rank1( do_compress, do_fillvalue, set_istore_k, H5D_FILL_TIME_ERROR, fapl ) < 0)
    {
        goto error;
    }
    if (test_rank2( do_compress, do_fillvalue, set_istore_k, H5D_FILL_TIME_ERROR, fapl ) < 0)
    {
        goto error;
    }
    if (test_rank3( do_compress, do_fillvalue, set_istore_k, H5D_FILL_TIME_ERROR, fapl ) < 0)
    {
        goto error;
    }
     
    PASSED();
#else
    SKIPPED();
#endif

    TESTING("with non-default indexed storage B-tree");

    do_fillvalue = 1;
    set_istore_k = 1;

    if (test_rank2( do_compress, do_fillvalue, set_istore_k, H5D_FILL_TIME_ALLOC, fapl ) < 0)
    {
        goto error;
    }
    

    PASSED();
   
    
    return 0;
    
    
error:
    return -1;
}


/*-------------------------------------------------------------------------
* test with different storage layouts
*-------------------------------------------------------------------------
*/
static int do_layouts( hid_t fapl )
{
    
    TESTING("storage layout use");
 
    if (test_layouts( H5D_COMPACT, fapl ) < 0)
    {
        goto error;
    } 
    
    if (test_layouts( H5D_CONTIGUOUS, fapl ) < 0)
    {
        goto error;
    } 
    
    PASSED();
    
    return 0;
    
error:
    return -1;
}

/*-------------------------------------------------------------------------
 * test usage with a 1D rank
 *-------------------------------------------------------------------------
 */

static int test_rank1( hbool_t do_compress, 
                       hbool_t do_fill_value, 
                       hbool_t set_istore_k,
                       H5D_fill_time_t fill_time,
                       hid_t fapl)
{

    hid_t   fid=-1;          
    hid_t   did=-1;
    hid_t   sid=-1;
    hid_t   dcpl=-1;
    hid_t   fcpl;
    hsize_t dims_o[RANK1] = {DIM0};   /* original dimensions */ 
    hsize_t dims_s[RANK1] = {DIMS0};  /* shrinking dimensions */ 
    hsize_t dims_e[RANK1] = {DIME0};  /* extended dimensions */ 
    hsize_t dims_c[RANK1] = {2};      /* chunk dimensions */ 
    hsize_t dims_r[RANK1];            /* read dimensions */ 
    hsize_t maxdims[RANK1] = {H5S_UNLIMITED};
    int     buf_o[DIM0];
    int     buf_s[DIMS0];
    int     buf_e[DIME0];
    int     buf_r[DIM0];
    int     i;
    int     fillvalue = 1; 
    int     comp_value;
    char    filename[NAME_BUF_SIZE];

       
    if ( do_fill_value )
    {
        comp_value = fillvalue;
    }
    else
    {
        comp_value = 0;
    }

        
    for( i = 0; i < DIM0; i++ )
    {
        
        buf_o[i] = 2;
    }

    /* create a file creation property list */
    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0) 
    {
        goto error;
    }
    
    if  ( set_istore_k )
    {
        /* set non-default indexed storage B-tree internal 'K' value */
        if (H5Pset_istore_k(fcpl,ISTORE_IK) < 0) 
        {
            goto error;
        }
        
    }
    /* create a new file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0) 
    {
        goto error;
    }

    /* close property list */
    if(H5Pclose(fcpl) < 0) 
    {
        goto error;
    }
    
    /* create the data space with unlimited dimensions. */
    if ((sid = H5Screate_simple(RANK1, dims_o, maxdims)) < 0) 
    {
        goto error;
    }
    
    /* modify dataset creation properties, i.e. enable chunking. */
    if ((dcpl = H5Pcreate (H5P_DATASET_CREATE)) < 0)
    {
        goto error;
    }
    if (H5Pset_chunk(dcpl, RANK1, dims_c) < 0)
    {
        goto error;
    }
    if ( do_fill_value )
    {
        if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillvalue) < 0)
        {
            goto error;
        }

        if(H5Pset_fill_time(dcpl, fill_time) < 0) 
        {
            goto error;
        }
    }
    else
    {
        
        if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) 
        {
            goto error;
        }
        
    }
    if (do_compress)
    {
        if(H5Pset_deflate(dcpl, 9) < 0) 
        {
            goto error;
        }
    }
    
    /*-------------------------------------------------------------------------
    * create, write dataset
    *-------------------------------------------------------------------------
    */
    
    /* create a dataset */
    if ((did = H5Dcreate(fid , "dset1", H5T_NATIVE_INT, sid, dcpl)) < 0) 
    {
        goto error;
    }
    
    /* write */
    if (H5Dwrite(did , H5T_NATIVE_INT, sid, H5S_ALL, H5P_DEFAULT, buf_o) < 0) 
    {
        goto error;
    }
    
    
#if defined (H5_SET_EXTENT_DEBUG)
    printf("\n");
    for (i = 0; i < (int)dims_o[0]; i++ )
    {
        
        printf("%d ", buf_o[i]);
        
    }
    printf("\n");
#endif  


    
    if (H5Sclose(sid) < 0)
    {
        goto error;
    }
    
    /*-------------------------------------------------------------------------
    * set new dimensions for the array; expand it
    *-------------------------------------------------------------------------
    */
    
    /* set new dimensions for the array. */
    if (H5Dset_extent(did , dims_e) < 0)
    {
        goto error;
    }
    
    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        goto error;
    }
    
    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0) 
    {
        goto error;
    }
    
    if (H5Sclose(sid) < 0)
    {
        goto error;
    }
    
    
    /* check dimensions */
    for( i = 0; i < RANK1; i++ )
    {
        if (dims_r[i] != dims_e[i]) 
            goto error;
    }
    
    /* read */
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_e) < 0) 
        goto error;

   
    
#if defined (H5_SET_EXTENT_DEBUG)
    printf("\n");
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        
        printf("%d ", buf_e[i]);
        
    }
    printf("\n");
#endif  

    
    
    
    /* compare the read array with the expanded array */
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        
        if ( i >= DIM0  ) 
        {
            if(buf_e[i] != comp_value) 
            {
                printf("buf_e[%d] = %d\n", i, buf_e[i]);
                printf("value = %d\n", comp_value);
                goto error;
            } 
        } 
        else 
        {
            if(buf_e[i] != buf_o[i]) 
                goto error;
        }
    }
    
    
  
    
    /*-------------------------------------------------------------------------
    * shrink 
    *
    *-------------------------------------------------------------------------
    */
    
    /* set new dimensions for the array. */
    if (H5Dset_extent(did , dims_s) < 0)
    {
        goto error;
    }
    
    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        goto error;
    }
    
    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0) 
    {
        goto error;
    }
    
    if (H5Sclose(sid) < 0)
    {
        goto error;
    }
    
    /* check dimensions */
    for( i = 0; i < RANK1; i++ )
    {
        if (dims_r[i] != dims_s[i]) 
            goto error;
    }



    /* for this case we close and reopen file */
    if ( set_istore_k )
    {
        
        if (H5Dclose(did) < 0) 
        {
            goto error;
        }
        if (H5Fclose(fid) < 0)
        {
            goto error;
        }

        if ((fid = H5Fopen( filename, H5F_ACC_RDWR, fapl ))<0) 
        {
            goto error;
        }
        
        if ((did = H5Dopen( fid , "dset1" ))<0) 
        {
            goto error;
        }
        
        
        
    }
    
    
    /*-------------------------------------------------------------------------
    * read
    *-------------------------------------------------------------------------
    */
    
    /* read */
    if (H5Dread( did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_s ) < 0) 
    {
        goto error;
    }
    
#if defined (H5_SET_EXTENT_DEBUG)
    printf("\n");
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        
        printf("%d ", buf_s[i]);
    }
    printf("\n");

#endif  


    
    
    /* compare the read array with the shrinked array */
    for( i = 0; i < (int)dims_r[0]; i++ )
    {
        
        if (  buf_s[i] != buf_o[i] ) 
        {
            printf("buf_s[%d] = %d\n", i, buf_s[i]);
            printf("buf_o[%d] = %d\n", i, buf_o[i]);
            goto error;
        } 
    }
    
    
    /*-------------------------------------------------------------------------
    * expand it back to original size
    *-------------------------------------------------------------------------
    */
    
    /* set new dimensions for the array */
    if (H5Dset_extent(did, dims_o) < 0)
    {
        goto error;
    }
    
    /* get the space */
    if ((sid = H5Dget_space(did)) < 0) 
    {
        goto error;
    }
    
    /* get dimensions. */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0)
    {
        goto error;
    }

    if (H5Sclose(sid) < 0)
    {
        goto error;
    }
    
    
    /* check dimensions */
    for( i = 0; i < RANK1; i++ )
    {
        if (dims_r[i] != dims_o[i]) 
            goto error;
    }
    
       
    /* read */
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_r) < 0) 
        goto error;
    
#if defined (H5_SET_EXTENT_DEBUG)
    printf("\n");
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        
        printf("%d ", buf_r[i]);
        
    }
    printf("\n");
#endif  


    
    /* compare the read array with the original array */
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        
        if (i >= DIMS0 ) 
        {
            if(buf_r[i] != comp_value) 
            {
                printf("buf_r[%d] = %d\n", i, buf_r[i] );
                printf("value = %d\n", comp_value);
                goto error;
            } 
        } 
        else 
        {
            if(buf_r[i] != buf_o[i]) 
                goto error;
        }
    }
    

    /*-------------------------------------------------------------------------
    * shrink to 0
    *
    *-------------------------------------------------------------------------
    */

    dims_s[0] = 0;
    
    /* set new dimensions for the array. */
    if (H5Dset_extent(did , dims_s) < 0)
    {
        goto error;
    }
    
    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        goto error;
    }
    
    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0) 
    {
        goto error;
    }
    
    if (H5Sclose(sid) < 0)
    {
        goto error;
    }
    
    /* check dimensions */
    for( i = 0; i < RANK1; i++ )
    {
        if (dims_r[i] != dims_s[i]) 
            goto error;
    }    
    
    /*-------------------------------------------------------------------------
    * close dataset
    *-------------------------------------------------------------------------
    */
    
    if (H5Dclose(did) < 0) 
    {
        goto error;
    }
   
    
   
    
    
     /*-------------------------------------------------------------------------
    * test a dataset with non initialized chunks
    *-------------------------------------------------------------------------
    */
    
      
    if ((sid = H5Screate_simple(RANK1, dims_o, maxdims)) < 0) 
    {
        goto error;
    }
    if ((did = H5Dcreate(fid , "dset3", H5T_NATIVE_INT, sid, dcpl)) < 0) 
    {
        goto error;
    }
    /* set new dimensions for the array */
    dims_o[ 0 ] = 0;
    if (H5Dset_extent( did , dims_o ) < 0) 
    {
        goto error;
    }



    if (H5Dclose(did) < 0) 
    {
        goto error;
    }
    if (H5Sclose(sid) < 0)
    {
        goto error;
    }

    
    
    
    /*-------------------------------------------------------------------------
    * close property list
    *-------------------------------------------------------------------------
    */
    
    
    if (H5Pclose(dcpl) < 0) 
    {
        goto error;
    }

    if (H5Fclose( fid ) < 0)
    {
        goto error;
    }


    return 0;
    
    
    
error:
    
    H5E_BEGIN_TRY 
    {
        H5Dclose( did );
        H5Sclose( sid );
        H5Pclose( dcpl  );
        H5Pclose( fcpl  );
        H5Fclose( fid );
    } H5E_END_TRY;
    return -1;
    
}

/*-------------------------------------------------------------------------
 * test usage with a 2D rank
 *-------------------------------------------------------------------------
 */

static int test_rank2( hbool_t do_compress, 
                       hbool_t do_fill_value, 
                       hbool_t set_istore_k,
                       H5D_fill_time_t fill_time,
                       hid_t fapl)
{

    hid_t   fid=-1;          
    hid_t   did=-1;
    hid_t   sid=-1;
    hid_t   dcpl=-1;
    hid_t   fcpl;
    hsize_t dims_o[RANK2] = {DIM0,DIM1};    /* original dimensions */ 
    hsize_t dims_s[RANK2] = {DIMS0,DIMS1};  /* shrinking dimensions */ 
    hsize_t dims_e[RANK2] = {DIME0,DIME1};  /* extended dimensions */ 
    hsize_t dims_c[RANK2] = {2,2};          /* chunk dimensions */ 
    hsize_t dims_r[RANK2];                  /* read dimensions */ 
    hsize_t maxdims[RANK2] = {H5S_UNLIMITED,H5S_UNLIMITED};
    int     buf_o[DIM0][DIM1];
    int     buf_s[DIMS0][DIMS1];
    int     buf_e[DIME0][DIME1];
    int     buf_r[DIM0][DIM1];
    int     i, j;
    int     fillvalue = 1; 
    int     comp_value; 
    char    filename[NAME_BUF_SIZE];
       
    if ( do_fill_value )
    {
        comp_value = fillvalue;
    }
    else
    {
        comp_value = 0;
    }

        
    for( i = 0; i < DIM0; i++ )
    {
        for( j = 0; j < DIM1; j++ )
        {
            buf_o[i][j] = 2;
        }
    }

    /* create a file creation property list */
    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0) 
    {
        goto error;
    }
    
    if  ( set_istore_k )
    {
        /* set non-default indexed storage B-tree internal 'K' value */
        if (H5Pset_istore_k(fcpl,ISTORE_IK) < 0) 
        {
            goto error;
        }
        
    }
    /* create a new file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0) 
    {
        goto error;
    }

    /* close property list */
    if(H5Pclose(fcpl) < 0) 
    {
        goto error;
    }
    
    /* create the data space with unlimited dimensions. */
    if ((sid = H5Screate_simple(RANK2, dims_o, maxdims)) < 0) 
    {
        goto error;
    }
    
    /* modify dataset creation properties, i.e. enable chunking. */
    if ((dcpl = H5Pcreate (H5P_DATASET_CREATE)) < 0)
    {
        goto error;
    }
    if (H5Pset_chunk(dcpl, RANK2, dims_c) < 0)
    {
        goto error;
    }
    if ( do_fill_value )
    {
        if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillvalue) < 0)
        {
            goto error;
        }

        if(H5Pset_fill_time(dcpl, fill_time) < 0) 
        {
            goto error;
        }
    }
    else
    {
        
        if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) 
        {
            goto error;
        }
        
    }
    if (do_compress)
    {
        if(H5Pset_deflate(dcpl, 9) < 0) 
        {
            goto error;
        }
    }
    
    /*-------------------------------------------------------------------------
    * Procedure 1
    * a.	Write an array AxB. These are the dimensions for creating the dataset
    * b.	Define a greater array CxD where C > A and D > B
    * c.	Read data back 
    * d.	Verify if new dimensions are C and D
    * e.	Verify if data from A to C and B to D is what it is to be expected
    *
    * original data is
    *
    *  2 2 2 2
    *  2 2 2 2
    *  2 2 2 2
    *  2 2 2 2
    *
    *-------------------------------------------------------------------------
    */
    
    /* create a dataset */
    if ((did = H5Dcreate(fid , "dset1", H5T_NATIVE_INT, sid, dcpl)) < 0) 
    {
        goto error;
    }
    
    /* write */
    if (H5Dwrite(did , H5T_NATIVE_INT, sid, H5S_ALL, H5P_DEFAULT, buf_o) < 0) 
    {
        goto error;
    }
    
    
#if defined (H5_SET_EXTENT_DEBUG2)
    printf("\n");
    for (i = 0; i < (int)dims_o[0]; i++ )
    {
        for (j = 0; j < (int)dims_o[1]; j++ )
        {
            printf("%d ", buf_o[i][j]);
        }
        printf("\n");
    }
#endif  


    
    if (H5Sclose(sid) < 0)
    {
        goto error;
    }
    
    /*-------------------------------------------------------------------------
    * set new dimensions for the array; expand it
    * data is now, extended space was initialized with fill value or default value
    *
    *  2 2 2 2 1 1 1
    *  2 2 2 2 1 1 1
    *  2 2 2 2 1 1 1
    *  2 2 2 2 1 1 1
    *  1 1 1 1 1 1 1
    *  1 1 1 1 1 1 1
    *  1 1 1 1 1 1 1
    *
    *-------------------------------------------------------------------------
    */
    
    /* set new dimensions for the array. */
    if (H5Dset_extent(did , dims_e) < 0)
    {
        goto error;
    }
    
    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        goto error;
    }
    
    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0) 
    {
        goto error;
    }
    
    if (H5Sclose(sid) < 0)
    {
        goto error;
    }
    
    
    /* check dimensions */
    for( i = 0; i < RANK2; i++ )
    {
        if (dims_r[i] != dims_e[i]) 
            goto error;
    }
    
    /* read */
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_e) < 0) 
        goto error;

   
    
#if defined (H5_SET_EXTENT_DEBUG2)
    printf("\n");
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            printf("%d ", buf_e[i][j]);
        }
        printf("\n");
    }
#endif  

    
    
    
    /* compare the read array with the expanded array */
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            if ( i >= DIM0 || j >= DIM1 ) 
            {
                if(buf_e[i][j] != comp_value) 
                {
                    printf("buf_e[%d][%d] = %d\n", i, j, buf_e[i][j]);
                    printf("value = %d\n", comp_value);
                    goto error;
                } 
            } 
            else 
            {
                if(buf_e[i][j] != buf_o[i][j]) 
                    goto error;
            }
        }
    }
    
    
  
    
    /*-------------------------------------------------------------------------
    *
    * Procedure 2
    * a.	Define a smaller array ExF where E < A and F < B
    * b.	Read data back 
    * c.	Verify if new dimensions are E and F
    * d.	Verify if data up until E and F is what to be expected
    *
    * data is now
    *
    *  2 2
    *  2 2
    *
    *-------------------------------------------------------------------------
    */
    
    /* set new dimensions for the array. */
    if (H5Dset_extent(did , dims_s) < 0)
    {
        goto error;
    }
    
    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        goto error;
    }
    
    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0) 
    {
        goto error;
    }
    
    if (H5Sclose(sid) < 0)
    {
        goto error;
    }
    
    /* check dimensions */
    for( i = 0; i < RANK2; i++ )
    {
        if (dims_r[i] != dims_s[i]) 
            goto error;
    }



    /* for this case we close and reopen file */
    if ( set_istore_k )
    {
        
        if (H5Dclose(did) < 0) 
        {
            goto error;
        }
        if (H5Fclose(fid) < 0)
        {
            goto error;
        }

        if ((fid = H5Fopen( filename, H5F_ACC_RDWR, fapl ))<0) 
        {
            goto error;
        }
        
        if ((did = H5Dopen( fid , "dset1" ))<0) 
        {
            goto error;
        }
        
        
        
    }
    
    
    /*-------------------------------------------------------------------------
    * read
    *-------------------------------------------------------------------------
    */
    
    /* read */
    if (H5Dread( did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_s ) < 0) 
    {
        goto error;
    }
    
#if defined (H5_SET_EXTENT_DEBUG2)
    printf("\n");
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            printf("%d ", buf_s[i][j]);
        }
        printf("\n");
    }
#endif  


    
    
    /* compare the read array with the shrinked array */
    for( i = 0; i < (int)dims_r[0]; i++ )
    {
        for( j = 0; j < (int)dims_r[1]; j++ )
        {
            if (  buf_s[i][j] != buf_o[i][j] ) 
            {
                printf("buf_s[%d][%d] = %d\n", i, j, buf_s[i][j]);
                printf("buf_o[%d][%d] = %d\n", i, j, buf_o[i][j]);
                goto error;
            } 
        }
    }
    
    
    /*-------------------------------------------------------------------------
    * set new dimensions for the array; expand it back to original size
    * data is now, extended space was initialized with fill value or default value
    *
    *  2 2 1 1
    *  2 2 1 1
    *  1 1 1 1
    *  1 1 1 1
    *
    *-------------------------------------------------------------------------
    */
    
    /* set new dimensions for the array */
    if (H5Dset_extent(did, dims_o) < 0)
    {
        goto error;
    }
    
    /* get the space */
    if ((sid = H5Dget_space(did)) < 0) 
    {
        goto error;
    }
    
    /* get dimensions. */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0)
    {
        goto error;
    }

    if (H5Sclose(sid) < 0)
    {
        goto error;
    }
    
    /* check dimensions */
    for( i = 0; i < RANK2; i++ )
    {
        if (dims_r[i] != dims_o[i]) 
            goto error;
    }
    
    
    /*-------------------------------------------------------------------------
    * read
    *-------------------------------------------------------------------------
    */
    
    /* read */
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_r) < 0) 
        goto error;
    
#if defined (H5_SET_EXTENT_DEBUG2)
    printf("\n");
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            printf("%d ", buf_r[i][j]);
        }
        printf("\n");
    }
#endif  


    
    /* compare the read array with the original array */
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            if (i >= DIMS0 || j >= DIMS1) 
            {
                if(buf_r[i][j] != comp_value) 
                {
                    printf("buf_r[%d][%d] = %d\n", i, j, buf_r[i][j]);
                    printf("value = %d\n", comp_value);
                    goto error;
                } 
            } 
            else 
            {
                if(buf_r[i][j] != buf_o[i][j]) 
                    goto error;
            }
        }
    }

    /*-------------------------------------------------------------------------
    * shrink to 0
    *
    *-------------------------------------------------------------------------
    */

    dims_s[0] = 0;
    dims_s[1] = 0;
    
    /* set new dimensions for the array. */
    if (H5Dset_extent(did , dims_s) < 0)
    {
        goto error;
    }
    
    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        goto error;
    }
    
    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0) 
    {
        goto error;
    }
    
    if (H5Sclose(sid) < 0)
    {
        goto error;
    }
    
    /* check dimensions */
    for( i = 0; i < RANK2; i++ )
    {
        if (dims_r[i] != dims_s[i]) 
            goto error;
    }    
    
    
    /*-------------------------------------------------------------------------
    * close dataset 
    *-------------------------------------------------------------------------
    */
    
    if (H5Dclose(did) < 0) 
    {
        goto error;
    }
       
    
     /*-------------------------------------------------------------------------
    * test a dataset with non initialized chunks
    *-------------------------------------------------------------------------
    */
    
      
    if ((sid = H5Screate_simple(RANK2, dims_o, maxdims)) < 0) 
    {
        goto error;
    }
    if ((did = H5Dcreate(fid , "dset3", H5T_NATIVE_INT, sid, dcpl)) < 0) 
    {
        goto error;
    }
    /* set new dimensions for the array */
    dims_o[ 0 ] = 0;
    dims_o[ 1 ] = 0;
    if (H5Dset_extent( did , dims_o ) < 0) 
    {
        goto error;
    }



    if (H5Dclose(did) < 0) 
    {
        goto error;
    }
    if (H5Sclose(sid) < 0)
    {
        goto error;
    }

    
    
    
    /*-------------------------------------------------------------------------
    * close property list
    *-------------------------------------------------------------------------
    */
    
    
    if (H5Pclose(dcpl) < 0) 
    {
        goto error;
    }

    if (H5Fclose( fid ) < 0)
    {
        goto error;
    }


    return 0;
    
    
    
error:
    
    H5E_BEGIN_TRY 
    {
        H5Dclose( did );
        H5Sclose( sid );
        H5Pclose( dcpl  );
        H5Pclose( fcpl  );
        H5Fclose( fid );
    } H5E_END_TRY;
    return -1;
    
}




/*-------------------------------------------------------------------------
 * test usage with a 3D rank
 *-------------------------------------------------------------------------
 */

static int test_rank3( hbool_t do_compress, 
                       hbool_t do_fill_value, 
                       hbool_t set_istore_k,
                       H5D_fill_time_t fill_time,
                       hid_t fapl)
{

    hid_t   fid=-1;          
    hid_t   did=-1;
    hid_t   sid=-1;
    hid_t   dcpl=-1;
    hid_t   fcpl;
    hsize_t dims_o[RANK3] = {DIM0,DIM1,DIM2};    /* original dimensions */ 
    hsize_t dims_s[RANK3] = {DIMS0,DIMS1,DIMS2}; /* shrinking dimensions */ 
    hsize_t dims_e[RANK3] = {DIME0,DIME1,DIME2}; /* extended dimensions */ 
    hsize_t dims_c[RANK3] = {2,2,2};             /* chunk dimensions */ 
    hsize_t dims_r[RANK3];                       /* read dimensions */ 
    hsize_t maxdims[RANK3] = {H5S_UNLIMITED,H5S_UNLIMITED,H5S_UNLIMITED};
    int     buf_o[DIM0][DIM1][DIM2];
    int     buf_s[DIMS0][DIMS1][DIMS2];
    int     buf_e[DIME0][DIME1][DIME2];
    int     buf_r[DIM0][DIM1][DIM2];
    int     i, j, k;
    int     fillvalue = 1; 
    int     comp_value; 
    char    filename[NAME_BUF_SIZE];
       
    if ( do_fill_value )
    {
        comp_value = fillvalue;
    }
    else
    {
        comp_value = 0;
    }

        
    for( i = 0; i < DIM0; i++ )
    {
        for( j = 0; j < DIM1; j++ )
        {
            for( k = 0; k < DIM2; k++ )
            {
                buf_o[i][j][k] = 2;
            }
        }
    }

    /* create a file creation property list */
    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0) 
    {
        goto error;
    }
    
    if  ( set_istore_k )
    {
        /* set non-default indexed storage B-tree internal 'K' value */
        if (H5Pset_istore_k(fcpl,ISTORE_IK) < 0) 
        {
            goto error;
        }
        
    }
    /* create a new file */
    h5_fixname(FILENAME[2], fapl, filename, sizeof filename);
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0) 
    {
        goto error;
    }

    /* close property list */
    if(H5Pclose(fcpl) < 0) 
    {
        goto error;
    }
    
    /* create the data space with unlimited dimensions. */
    if ((sid = H5Screate_simple(RANK3, dims_o, maxdims)) < 0) 
    {
        goto error;
    }
    
    /* modify dataset creation properties, i.e. enable chunking. */
    if ((dcpl = H5Pcreate (H5P_DATASET_CREATE)) < 0)
    {
        goto error;
    }
    if (H5Pset_chunk(dcpl, RANK3, dims_c) < 0)
    {
        goto error;
    }
    if ( do_fill_value )
    {
        if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillvalue) < 0)
        {
            goto error;
        }

        if(H5Pset_fill_time(dcpl, fill_time) < 0) 
        {
            goto error;
        }
    }
    else
    {
        
        if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) 
        {
            goto error;
        }
        
    }
    if (do_compress)
    {
        if(H5Pset_deflate(dcpl, 9) < 0) 
        {
            goto error;
        }
    }
    
    /*-------------------------------------------------------------------------
    * create, write array
    *-------------------------------------------------------------------------
    */
    
    /* create a dataset */
    if ((did = H5Dcreate(fid , "dset1", H5T_NATIVE_INT, sid, dcpl)) < 0) 
    {
        goto error;
    }
    
    /* write */
    if (H5Dwrite(did , H5T_NATIVE_INT, sid, H5S_ALL, H5P_DEFAULT, buf_o) < 0) 
    {
        goto error;
    }
    
    
#if defined (H5_SET_EXTENT_DEBUG3)
    printf("\n");
    for (i = 0; i < (int)dims_o[0]; i++ )
    {
        for (j = 0; j < (int)dims_o[1]; j++ )
        {
            for( k = 0; k < (int)dims_o[2]; k++ )
            {
                printf("%d ", buf_o[i][j][k]);
            }
            printf("[%d] ", j);
        }
        printf("\n");
        
    }
    printf("\n");
#endif  


    
    if (H5Sclose(sid) < 0)
    {
        goto error;
    }
    
    /*-------------------------------------------------------------------------
    * set new dimensions for the array; expand it
    *
    *-------------------------------------------------------------------------
    */
    
    /* set new dimensions for the array. */
    if (H5Dset_extent(did , dims_e) < 0)
    {
        goto error;
    }
    
    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        goto error;
    }
    
    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0) 
    {
        goto error;
    }
    
    if (H5Sclose(sid) < 0)
    {
        goto error;
    }
    
    
    /* check dimensions */
    for( i = 0; i < RANK3; i++ )
    {
        if (dims_r[i] != dims_e[i]) 
            goto error;
    }
    
    /* read */
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_e) < 0) 
        goto error;

   
    
#if defined (H5_SET_EXTENT_DEBUG3)
    printf("\n");
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            for( k = 0; k < (int)dims_r[2]; k++ )
            {
                printf("%d ", buf_e[i][j][k]);
            }
            printf("[%d] ", j);
        }
        printf("\n");
        
    }
    printf("\n");
#endif  

    
    
    
    /* compare the read array with the expanded array */
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            for( k = 0; k < (int)dims_r[2]; k++ )
            {
                if ( i >= DIM0 || j >= DIM1 || k >= DIM2 ) 
                {
                    if(buf_e[i][j][k] != comp_value) 
                    {
                        printf("buf_e[%d][%d][%d] = %d\n", i, j, k, buf_e[i][j][k] );
                        printf("value = %d\n", comp_value);
                        goto error;
                    } 
                } 
                else 
                {
                    if(buf_e[i][j][k] != buf_o[i][j][k] ) 
                        goto error;
                }
            }
        }
    }
    
    
  
    
    /*-------------------------------------------------------------------------
    * shrink
    *-------------------------------------------------------------------------
    */
    
    /* set new dimensions for the array. */
    if (H5Dset_extent(did , dims_s) < 0)
    {
        goto error;
    }
    
    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        goto error;
    }
    
    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0) 
    {
        goto error;
    }
    
    if (H5Sclose(sid) < 0)
    {
        goto error;
    }
    
    /* check dimensions */
    for( i = 0; i < RANK3; i++ )
    {
        if (dims_r[i] != dims_s[i]) 
            goto error;
    }



    /* for this case we close and reopen file */
    if ( set_istore_k )
    {
        
        if (H5Dclose(did) < 0) 
        {
            goto error;
        }
        if (H5Fclose(fid) < 0)
        {
            goto error;
        }

        if ((fid = H5Fopen( filename, H5F_ACC_RDWR, fapl ))<0) 
        {
            goto error;
        }
        
        if ((did = H5Dopen( fid , "dset1"))<0) 
        {
            goto error;
        }
        
        
        
    }
    
    
    /*-------------------------------------------------------------------------
    * read
    *-------------------------------------------------------------------------
    */
    
    /* read */
    if (H5Dread( did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_s ) < 0) 
    {
        goto error;
    }
    
#if defined (H5_SET_EXTENT_DEBUG3)
    printf("\n");
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            for( k = 0; k < (int)dims_r[2]; k++ )
            {
                printf("%d ", buf_s[i][j][k]);
            }
             printf("[%d] ", j);
        }
        printf("\n");
        
    }
    printf("\n");
#endif  


    
    
    /* compare the read array with the shrinked array */
    for( i = 0; i < (int)dims_r[0]; i++ )
    {
        for( j = 0; j < (int)dims_r[1]; j++ )
        {
            for( k = 0; k < (int)dims_r[2]; k++ )
            {
                if (  buf_s[i][j][k] != buf_o[i][j][k] ) 
                {
                    printf("buf_s[%d][%d][%d] = %d\n", i, j, k, buf_s[i][j][k] );
                    printf("buf_o[%d][%d][%d] = %d\n", i, j, k, buf_o[i][j][k] );
                    goto error;
                } 
            }
        }
    }
    
    
    /*-------------------------------------------------------------------------
    * set new dimensions for the array; expand it back to original size
    *-------------------------------------------------------------------------
    */
    
    /* set new dimensions for the array */
    if (H5Dset_extent(did, dims_o) < 0)
    {
        goto error;
    }
    
    /* get the space */
    if ((sid = H5Dget_space(did)) < 0) 
    {
        goto error;
    }
    
    /* get dimensions. */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0)
    {
        goto error;
    }

    if (H5Sclose(sid) < 0)
    {
        goto error;
    }
    
    /* check dimensions */
    for( i = 0; i < RANK3; i++ )
    {
        if (dims_r[i] != dims_o[i]) 
            goto error;
    }
    
    /* read */
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_r) < 0) 
        goto error;
    
#if defined (H5_SET_EXTENT_DEBUG3)
    printf("\n");
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            for( k = 0; k < (int)dims_r[2]; k++ )
            {
                
                printf("%d ", buf_r[i][j][k]);
            }
             printf("[%d] ", j);
        }
        printf("\n");
        
    }
    printf("\n");
#endif  


    
    /* compare the read array with the original array */
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            for( k = 0; k < (int)dims_r[2]; k++ )
            {
                if (i >= DIMS0 || j >= DIMS1 || k >= DIMS2 ) 
                {
                    if( buf_r[i][j][k] != comp_value ) 
                    {
                        printf("buf_r[%d][%d][%d] = %d\n", i, j, k, buf_r[i][j][k] );
                        printf("value = %d\n", comp_value);
                        goto error;
                    } 
                } 
                else 
                {
                    if(buf_r[i][j][k] != buf_o[i][j][k]) 
                        goto error;
                }
            }
        }
    }
    

    /*-------------------------------------------------------------------------
    * shrink to 0
    *
    *-------------------------------------------------------------------------
    */

    dims_s[0] = 0;
    dims_s[1] = 0;
    dims_s[2] = 0;
    
    /* set new dimensions for the array. */
    if (H5Dset_extent(did , dims_s) < 0)
    {
        goto error;
    }
    
    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        goto error;
    }
    
    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0) 
    {
        goto error;
    }
    
    if (H5Sclose(sid) < 0)
    {
        goto error;
    }
    
    /* check dimensions */
    for( i = 0; i < RANK3; i++ )
    {
        if (dims_r[i] != dims_s[i]) 
            goto error;
    }    
    
    
    
    /*-------------------------------------------------------------------------
    * close dataset 
    *-------------------------------------------------------------------------
    */
    
    if (H5Dclose(did) < 0) 
    {
        goto error;
    }
       
    
    /*-------------------------------------------------------------------------
    * test a dataset with non initialized chunks
    *-------------------------------------------------------------------------
    */
    
      
    if ((sid = H5Screate_simple(RANK3, dims_o, maxdims)) < 0) 
    {
        goto error;
    }
    if ((did = H5Dcreate(fid , "dset3", H5T_NATIVE_INT, sid, dcpl)) < 0) 
    {
        goto error;
    }
    /* set new dimensions for the array */
    dims_o[ 0 ] = 0;
    dims_o[ 1 ] = 0;
    dims_o[ 2 ] = 0;
    if (H5Dset_extent( did , dims_o ) < 0) 
    {
        goto error;
    }



    if (H5Dclose(did) < 0) 
    {
        goto error;
    }
    if (H5Sclose(sid) < 0)
    {
        goto error;
    }

    
    
    
    /*-------------------------------------------------------------------------
    * close property list
    *-------------------------------------------------------------------------
    */
    
    
    if (H5Pclose(dcpl) < 0) 
    {
        goto error;
    }

    if (H5Fclose( fid ) < 0)
    {
        goto error;
    }


    return 0;
    
    
    
error:
    
    H5E_BEGIN_TRY 
    {
        H5Dclose( did );
        H5Sclose( sid );
        H5Pclose( dcpl  );
        H5Pclose( fcpl  );
        H5Fclose( fid );
    } H5E_END_TRY;
    return -1;
    
}


/*-------------------------------------------------------------------------
 * test usage with external storage
 *-------------------------------------------------------------------------
 */
static int test_external( hid_t fapl  )
{

    hid_t   fid=-1;       
    hid_t   did=-1;
    hid_t   sid=-1;
    hid_t   dcpl=-1;
    hsize_t dims_o[RANK2] = {DIM0,DIM1};    /* original dimensions */ 
    hsize_t dims_s[RANK2] = {DIMS0,DIMS1};  /* shrinking dimensions */ 
    hsize_t dims_e[RANK2] = {DIME0,DIM1};   /* extended dimensions, dimension 1 is the original */ 
    hsize_t dims_r[RANK2];                  /* read dimensions */ 
    hsize_t maxdims[RANK2] = {DIME0,DIM1};  /* only the first dimension can be extendible */
    int     buf_o[DIM0][DIM1];              /* original buffer, for writing */  
    int     buf_s[DIMS0][DIMS1];            /* shrinked buffer, for reading */  
    int     buf_e[DIME0][DIM1];             /* extended buffer, for writing, dimension 1 is the original */    
    int     buf_ro[DIM0][DIM1];             /* original buffer for reading */    
    int     i, j;
    int     comp_value = 0;
    char    filename[NAME_BUF_SIZE];
   

    hsize_t size;        /* number of bytes reserved in the file for the data */
    hsize_t max_size[2];	
    
    max_size[0] = dims_e[0];
    max_size[1] = dims_e[1];
    size = max_size[0] * max_size[1] * sizeof(int) / 2;
    
      
    for( i = 0; i < DIM0; i++ )
    {
        for( j = 0; j < DIM1; j++ )
        {
            buf_o[i][j] = 2;
        }
    }

    TESTING("external file use");
  
    /* create a new file */
    h5_fixname(FILENAME[3], fapl, filename, sizeof filename);
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) 
    {
        goto error;
    }
  
    /* modify dataset creation properties */
    if ((dcpl = H5Pcreate (H5P_DATASET_CREATE)) < 0)
    {
        goto error;
    }
    
    if(H5Pset_external(dcpl, EXT_FILE_NAME1, (off_t)0, size) < 0)
    {
        goto error;
    }

    if(H5Pset_external(dcpl, EXT_FILE_NAME2, (off_t)0, size) < 0)
    {
        goto error;
    }
    
    {
        
        char	name[256];		/*external file name		*/
        off_t	file_offset;    /*external file offset		*/
        hsize_t	file_size;		/*sizeof external file segment	*/
        
        if(H5Pget_external(dcpl, 0, sizeof(name), name, &file_offset,
            &file_size) < 0) goto error;
        
    }



    /*-------------------------------------------------------------------------
    * Write an array AxB. These are the dimensions for creating the dataset
    *
    * original data is
    *
    *  2 2 2 2
    *  2 2 2 2
    *  2 2 2 2
    *  2 2 2 2
    *
    *-------------------------------------------------------------------------
    */


    /* create the data space with unlimited dimensions. */
    if ((sid = H5Screate_simple(RANK2, dims_o, maxdims)) < 0) 
    {
        goto error;
    }
    if ((did = H5Dcreate(fid , "dset1", H5T_NATIVE_INT, sid, dcpl)) < 0) 
    {
        goto error;
    }
    if (H5Dwrite(did , H5T_NATIVE_INT, sid, H5S_ALL, H5P_DEFAULT, buf_o) < 0) 
    {
        goto error;
    }
    if (H5Sclose(sid) < 0)
    {
        goto error;
    }


    /*-------------------------------------------------------------------------
    * read
    *-------------------------------------------------------------------------
    */

     /* read */
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_ro) < 0) 
        goto error;
    
#if defined (H5_SET_EXTENT_DEBUG)
    printf("\n");
    for (i = 0; i < (int)dims_o[0]; i++ )
    {
        for (j = 0; j < (int)dims_o[1]; j++ )
        {
            printf("%d ", buf_ro[i][j]);
        }
        printf("\n");
    }
#endif  


    
    /*-------------------------------------------------------------------------
    * expand
    *-------------------------------------------------------------------------
    */

     /*-------------------------------------------------------------------------
    * set new dimensions for the array; expand it
    * data is now, extended space was initialized with default value
    *
    *  2 2 2 2 
    *  2 2 2 2 
    *  2 2 2 2 
    *  2 2 2 2 
    *  0 0 0 0 
    *  0 0 0 0 
    *  0 0 0 0 
    *
    *-------------------------------------------------------------------------
    */

    /* set new dimensions for the array. */
    if (H5Dset_extent(did , dims_e) < 0)
    {
        goto error;
    }

     /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        goto error;
    }
    
    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0) 
    {
        goto error;
    }
    
    if (H5Sclose(sid) < 0)
    {
        goto error;
    }
    
    
    /* check dimensions */
    for( i = 0; i < RANK2; i++ )
    {
        if (dims_r[i] != dims_e[i]) 
            goto error;
    }
    
    /* read */
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_e) < 0) 
        goto error;

   
    
#if defined (H5_SET_EXTENT_DEBUG)
    printf("\n");
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            printf("%d ", buf_e[i][j]);
        }
        printf("\n");
    }
#endif  

    
    
    
    /* compare the read array with the expanded array */
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            if ( i >= DIM0 || j >= DIM1 ) 
            {
                if(buf_e[i][j] != comp_value) 
                {
                    printf("buf_e[%d][%d] = %d\n", i, j, buf_e[i][j]);
                    printf("value = %d\n", comp_value);
                    goto error;
                } 
            } 
            else 
            {
                if(buf_e[i][j] != buf_o[i][j]) 
                    goto error;
            }
        }
    }
    


     /*-------------------------------------------------------------------------
    * shrink
    *
    * data is now
    *
    *  2 2
    *  2 2
    *
    *-------------------------------------------------------------------------
    */
    
    /* set new dimensions for the array. */
    if (H5Dset_extent(did , dims_s) < 0)
    {
        goto error;
    }
    
    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        goto error;
    }
    
    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0) 
    {
        goto error;
    }
    
    if (H5Sclose(sid) < 0)
    {
        goto error;
    }
    
    /* check dimensions */
    for( i = 0; i < RANK2; i++ )
    {
        if (dims_r[i] != dims_s[i]) 
            goto error;
    }


    
    
    /*-------------------------------------------------------------------------
    * read
    *-------------------------------------------------------------------------
    */
    
    /* read */
    if (H5Dread( did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_s ) < 0) 
    {
        goto error;
    }
    
#if defined (H5_SET_EXTENT_DEBUG)
    printf("\n");
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            printf("%d ", buf_s[i][j]);
        }
        printf("\n");
    }
#endif  


    
    
    /* compare the read array with the shrinked array */
    for( i = 0; i < (int)dims_r[0]; i++ )
    {
        for( j = 0; j < (int)dims_r[1]; j++ )
        {
            if (  buf_s[i][j] != buf_o[i][j] ) 
            {
                printf("buf_s[%d][%d] = %d\n", i, j, buf_s[i][j]);
                printf("buf_o[%d][%d] = %d\n", i, j, buf_o[i][j]);
                goto error;
            } 
        }
    }


    /*-------------------------------------------------------------------------
    * negative test
    * try to extend dimension above maximum
    *-------------------------------------------------------------------------
    */


    dims_e[1] = DIME1;


    H5E_BEGIN_TRY 
    {
        
        
        /* set new dimensions for the array. */
        if (H5Dset_extent(did , dims_e) == SUCCEED)
        {
            goto error;
        }
        
    } H5E_END_TRY;



    /*-------------------------------------------------------------------------
    * close property list
    *-------------------------------------------------------------------------
    */
    
    
    if (H5Pclose(dcpl) < 0) 
    {
        goto error;
    }

    if (H5Fclose( fid ) < 0)
    {
        goto error;
    }

    PASSED();


    return 0;
    
    
    
error:
    
    H5E_BEGIN_TRY 
    {
        H5Dclose( did );
        H5Sclose( sid );
        H5Pclose( dcpl );
        H5Fclose( fid );
    } H5E_END_TRY;
    return -1;
    
}


/*-------------------------------------------------------------------------
 * test usage with layouts compact and contiguous
 *-------------------------------------------------------------------------
 */
static int test_layouts( H5D_layout_t layout, hid_t fapl  )
{

    hid_t   fid=-1;       
    hid_t   did=-1;
    hid_t   sid=-1;
    hid_t   dcpl=-1;
    hsize_t dims_o[RANK2] = {DIM0,DIM1};    /* original dimensions */ 
    hsize_t dims_s[RANK2] = {DIMS0,DIMS1};  /* shrinking dimensions */ 
    hsize_t dims_e[RANK2] = {DIME0,DIME1};  /* extended dimensions */ 
    hsize_t dims_r[RANK2];                  /* read dimensions */ 
    int     buf_o[DIM0][DIM1];
    int     buf_r[DIM0][DIM1];
    int     i, j;
    char    filename[NAME_BUF_SIZE];
   
    for( i = 0; i < DIM0; i++ )
    {
        for( j = 0; j < DIM1; j++ )
        {
            buf_o[i][j] = 2;
        }
    }

  
    /* create a new file */
    h5_fixname(FILENAME[4], fapl, filename, sizeof filename);
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) 
    {
        goto error;
    }

    /* create the data space with unlimited dimensions. */
    if ((sid = H5Screate_simple(RANK2, dims_o, NULL)) < 0) 
    {
        goto error;
    }
    
    /* modify dataset creation properties */
    if ((dcpl = H5Pcreate (H5P_DATASET_CREATE)) < 0)
    {
        goto error;
    }
    
    if (H5Pset_layout (dcpl, layout) < 0)
    {
        goto error;
    }
         
    /* create a dataset */
    if ((did = H5Dcreate(fid , "dset1", H5T_NATIVE_INT, sid, dcpl)) < 0) 
    {
        goto error;
    }
    
    /* write */
    if (H5Dwrite(did , H5T_NATIVE_INT, sid, H5S_ALL, H5P_DEFAULT, buf_o) < 0) 
    {
        goto error;
    }
    
    
#if defined (H5_SET_EXTENT_DEBUG4)
    printf("\n");
    for (i = 0; i < (int)dims_o[0]; i++ )
    {
        for (j = 0; j < (int)dims_o[1]; j++ )
        {
            printf("%d ", buf_o[i][j]);
        }
        printf("\n");
    }
#endif  


    
    if (H5Sclose(sid) < 0)
    {
        goto error;
    }
    
    /*-------------------------------------------------------------------------
    * negative test
    * try to extend dimension 
    *-------------------------------------------------------------------------
    */

    H5E_BEGIN_TRY 
    {
     
        if (H5Dset_extent(did , dims_e) == SUCCEED)
        {
            goto error;
        }
        
    } H5E_END_TRY;

    
  
    
    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        goto error;
    }
    
    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0) 
    {
        goto error;
    }
    
    if (H5Sclose(sid) < 0)
    {
        goto error;
    }
    
    
    /* check dimensions */
    for( i = 0; i < RANK2; i++ )
    {
        if (dims_r[i] != dims_o[i]) 
            goto error;
    }
    
    /* read */
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_r) < 0) 
        goto error;

   
    
#if defined (H5_SET_EXTENT_DEBUG4)
    printf("\n");
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            printf("%d ", buf_r[i][j]);
        }
        printf("\n");
    }
#endif  

     
    
    /*-------------------------------------------------------------------------
    * negative test
    * try to shrink dimension 
    *-------------------------------------------------------------------------
    */

    H5E_BEGIN_TRY 
    {
     
        if (H5Dset_extent(did , dims_s) == SUCCEED)
        {
            goto error;
        }
        
    } H5E_END_TRY;

    
    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        goto error;
    }
    
    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0) 
    {
        goto error;
    }
    
    if (H5Sclose(sid) < 0)
    {
        goto error;
    }
    
    /* check dimensions */
    for( i = 0; i < RANK2; i++ )
    {
        if (dims_r[i] != dims_o[i]) 
            goto error;
    }

    
    /*-------------------------------------------------------------------------
    * read
    *-------------------------------------------------------------------------
    */
    
    /* read */
    if (H5Dread( did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_r ) < 0) 
    {
        goto error;
    }
    
#if defined (H5_SET_EXTENT_DEBUG4)
    printf("\n");
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            printf("%d ", buf_r[i][j]);
        }
        printf("\n");
    }
#endif  


     
    /*-------------------------------------------------------------------------
    * close 
    *-------------------------------------------------------------------------
    */
    
    if (H5Dclose(did) < 0) 
    {
        goto error;
    }
      
    if (H5Pclose(dcpl) < 0) 
    {
        goto error;
    }

    if (H5Fclose( fid ) < 0)
    {
        goto error;
    }


    return 0;
    
    
    
error:
    
    H5E_BEGIN_TRY 
    {
        H5Dclose( did );
        H5Sclose( sid );
        H5Pclose( dcpl  );
        H5Fclose( fid );
    } H5E_END_TRY;
    return -1;
    
}


 

