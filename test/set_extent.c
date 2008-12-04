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

#if 0
#define H5_SET_EXTENT_DEBUG
#endif

#if 0
#define H5_SET_EXTENT_DEBUG2
#endif


#include "hdf5.h"
#include "h5test.h"

/*-------------------------------------------------------------------------
 *
 * Tests the function H5Dset_extent. In the current version of the library
 * the dataset MUST be chunked.
 *
 *-------------------------------------------------------------------------
 */

#define RANK  2
#define DIM0  4
#define DIM1  4
#define DIMS0 2
#define DIMS1 2
#define DIME0 7
#define DIME1 7
#define ISTORE_IK  64


static int test( hbool_t do_compress, hbool_t do_fill_value, hbool_t set_istore_k);


/*-------------------------------------------------------------------------
 * main
 *-------------------------------------------------------------------------
 */


int main( void )
{
    hbool_t do_compress = 0;
    hbool_t do_fillvalue = 0;
    hbool_t set_istore_k = 0;
    
      
    TESTING("with fill value, no compression");

    do_fillvalue = 1;

    if (test( do_compress, do_fillvalue, set_istore_k ) < 0)
    {
        goto error;
    }    

    PASSED();


    TESTING("no fill value, no compression");

    do_fillvalue = 0;

    if (test( do_compress, do_fillvalue, set_istore_k ) < 0)
    {
        goto error;
    }
   

    PASSED();
    
    TESTING("with fill value, with compression");

#ifdef H5_HAVE_FILTER_DEFLATE

    do_compress = 1;

    do_fillvalue = 1;

    if (test( do_compress, do_fillvalue, set_istore_k ) < 0)
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

    if (test( do_compress, do_fillvalue, set_istore_k ) < 0)
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

    if (test( do_compress, do_fillvalue, set_istore_k ) < 0)
    {
        goto error;
    }
    

    PASSED();
   
    
    puts("All set_extent tests passed.");
    return 0;
    
    
error:
    
   
    
    H5_FAILED();
    return 1;
}



/*-------------------------------------------------------------------------
 * test
 *-------------------------------------------------------------------------
 */

static int test( hbool_t do_compress, hbool_t do_fill_value, hbool_t set_istore_k)
{

    hid_t   fid;
    hid_t   did;
    hid_t   sid;
    hid_t   dcpl;
    hid_t   fcpl;
    hsize_t dims_o[RANK] = {DIM0,DIM1};    
    hsize_t dims_s[RANK] = {DIMS0,DIMS1};
    hsize_t dims_e[RANK] = {DIME0,DIME1};
    hsize_t dims_c[RANK] = {2,2};
    hsize_t dims_r[RANK];
    hsize_t maxdims[RANK] = {H5S_UNLIMITED,H5S_UNLIMITED};
    int     buf_o[DIM0][DIM1];
    int     buf_s[DIMS0][DIMS1];
    int     buf_e[DIME0][DIME1];
    int     buf_r[DIM0][DIM1];
    int     i, j;
    int     fillvalue = 1; 
    int     comp_value; 
       
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
    if ((fid = H5Fcreate("set_extent1.h5", H5F_ACC_TRUNC, fcpl, H5P_DEFAULT)) < 0) 
    {
        goto error;
    }

    /* close property list */
    if(H5Pclose(fcpl) < 0) 
    {
        goto error;
    }
    
    /* create the data space with unlimited dimensions. */
    if ((sid = H5Screate_simple(RANK, dims_o, maxdims)) < 0) 
    {
        goto error;
    }
    
    /* modify dataset creation properties, i.e. enable chunking. */
    if ((dcpl = H5Pcreate (H5P_DATASET_CREATE)) < 0)
    {
        goto error;
    }
    if (H5Pset_chunk(dcpl, RANK, dims_c) < 0)
    {
        goto error;
    }
    if ( do_fill_value )
    {
        if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillvalue) < 0)
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
    if(do_compress)
    {
        if(H5Pset_deflate(dcpl, 9) < 0) 
        {
            goto error;
        }
    }
    
    
    
    /*-------------------------------------------------------------------------
    * create and write one dataset
    * data is
    *
    *  2 2 2 2
    *  2 2 2 2
    *  2 2 2 2
    *  2 2 2 2
    *
    *-------------------------------------------------------------------------
    */
    
    /* create a dataset */
    if ((did = H5Dcreate2(fid , "dset1", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) 
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
    for( i = 0; i < RANK; i++ )
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
            if (i >= DIM0 || j >= DIM1) 
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
    * write to the expanded array. fill the space with fill value with other value
    * data is now, extended space was written
    *
    *  2 2 2 2 2 2 2
    *  2 2 2 2 2 2 2
    *  2 2 2 2 2 2 2
    *  2 2 2 2 2 2 2
    *  2 2 2 2 2 2 2
    *  2 2 2 2 2 2 2
    *  2 2 2 2 2 2 2
    *
    *-------------------------------------------------------------------------
    */
    
    for( i = 0; i < DIME0; i++ )
    {
        for( j = 0; j < DIME1; j++ )
        {
            buf_e[i][j] = 2;
        }
    }
    
    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        goto error;
    }
    
    /* write */
    if (H5Dwrite(did , H5T_NATIVE_INT, sid, H5S_ALL, H5P_DEFAULT, buf_e) < 0) 
    {
        goto error;
    }
    
    /* read */
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_e) < 0) 
        goto error;
    
    
    /* close space */
    if (H5Sclose(sid) < 0)
    {
        goto error;
    }
    
    
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
    
    
    
    
    /*-------------------------------------------------------------------------
    * set new dimensions for the array; shrink it
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
    for( i = 0; i < RANK; i++ )
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

        if ((fid = H5Fopen( "set_extent1.h5", H5F_ACC_RDWR, H5P_DEFAULT ))<0) 
        {
            goto error;
        }
        
        if ((did = H5Dopen2( fid , "dset1", H5P_DEFAULT ))<0) 
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
                printf("buf_r[%d][%d] = %d\n", i, j, buf_r[i][j]);
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
    
    /* check dimensions */
    for( i = 0; i < RANK; i++ )
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
    
#if defined (H5_SET_EXTENT_DEBUG)
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
    * close dataset and space
    *-------------------------------------------------------------------------
    */
    
    if (H5Dclose(did) < 0) 
    {
        goto error;
    }
    if (H5Sclose(sid) < 0)
    {
        goto error;
    }
    
    
    
   

     /*-------------------------------------------------------------------------
    * create and write one dataset
    * data is
    *
    *  2 2 2 2
    *  2 2 2 2
    *  2 2 2 2
    *  2 2 2 2
    *
    *-------------------------------------------------------------------------
    */

    /* create the data space with unlimited dimensions. */
    if ((sid = H5Screate_simple(RANK, dims_o, maxdims)) < 0) 
    {
        goto error;
    }
    
    /* create a dataset */
    if ((did = H5Dcreate2(fid , "dset2", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) 
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
    * set new dimensions for the array; shrink it
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
    for( i = 0; i < RANK; i++ )
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
                printf("buf_r[%d][%d] = %d\n", i, j, buf_r[i][j]);
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
    
    /* check dimensions */
    for( i = 0; i < RANK; i++ )
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
    * close dataset and space
    *-------------------------------------------------------------------------
    */
    
    if (H5Dclose(did) < 0) 
    {
        goto error;
    }
    if (H5Sclose(sid) < 0)
    {
        goto error;
    }
    
    
     /*-------------------------------------------------------------------------
    * test a dataset with non initialized chunks
    *-------------------------------------------------------------------------
    */
    
      
    if ((sid = H5Screate_simple(RANK, dims_o, maxdims)) < 0) 
    {
        goto error;
    }
    if ((did = H5Dcreate2(fid , "dset3", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) 
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


 

