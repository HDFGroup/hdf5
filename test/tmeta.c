/****************************************************************************
 * NCSA HDF                                                                 *
 * Software Development Group                                               *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/

#ifdef RCSID
static char RcsId[] = "$Revision$";
#endif

/* $Id$ */

/***********************************************************
*
* Test program:  tmeta
*
* Test the basic meta-data encode/decode macros calls.
*
*************************************************************/

#include "testhdf5.h"

#define TEST_INT16_VALUE    -7641
#define TEST_UINT16_VALUE   45002
#define TEST_INT32_VALUE    -981236
#define TEST_UINT32_VALUE   3476589

uint8 compar_buffer[]={
    /* Little-endian encoded version of the 16-bit signed integer */
    (uint8)((TEST_INT16_VALUE)&0xff), (uint8)((TEST_INT16_VALUE>>8)&0xff), 
    /* Little-endian encoded version of the 16-bit unsigned integer */
    (uint8)((TEST_UINT16_VALUE)&0xff), (uint8)((TEST_UINT16_VALUE>>8)&0xff), 
    /* Little-endian encoded version of the 32-bit signed integer */
    (uint8)((TEST_INT32_VALUE)&0xff), (uint8)((TEST_INT32_VALUE>>8)&0xff), 
    (uint8)((TEST_INT32_VALUE>>16)&0xff), (uint8)((TEST_INT32_VALUE>>24)&0xff), 
    /* Little-endian encoded version of the 32-bit unsigned integer */
    (uint8)((TEST_UINT32_VALUE)&0xff), (uint8)((TEST_UINT32_VALUE>>8)&0xff), 
    (uint8)((TEST_UINT32_VALUE>>16)&0xff), (uint8)((TEST_UINT32_VALUE>>24)&0xff), 
 };

uint8 encode_buffer[sizeof(compar_buffer)];

/****************************************************************
**
**  test_metadata(): Main meta-data encode/decode testing routine.
** 
****************************************************************/
void test_metadata(void)
{
    int16 ei16=TEST_INT16_VALUE;    /* variables to hold the values to encode */
    uint16 eu16=TEST_UINT16_VALUE;
    int32 ei32=TEST_INT32_VALUE;
    uint32 eu32=TEST_UINT32_VALUE;
    int16 di16;    /* variables to hold the decoded values */
    uint16 du16;
    int32 di32;
    uint32 du32;
    uint8 *p;       /* pointer into the buffer being en/de-coded */

    /* Output message about test being performed */
    MESSAGE(5, print_func("Testing Metadata encode/decode code\n"););

    /* Start by encoding the values above */
    p=encode_buffer;
    INT16ENCODE(p,ei16);    /* Encode the int16 value */
    UINT16ENCODE(p,eu16);   /* Encode the uint16 value */
    INT32ENCODE(p,ei32);    /* Encode the int32 value */
    UINT32ENCODE(p,eu32);   /* Encode the uint32 value */

    /* Check if we got what we asked for */
    if(HDmemcmp(encode_buffer,compar_buffer,sizeof(compar_buffer))!=0)
      {
        uintn u;     /* local counting variable */

        for(u=0; u<sizeof(compar_buffer); u++)
          {
            if(compar_buffer[u]!=encode_buffer[u])
              {
                print_func("Error encoding meta-data at offset %u, wanted: %u, got: %u\n",(unsigned)u,(unsigned)compar_buffer[u],(unsigned)encode_buffer[u]);
                num_errs++;
              } /* end if */
          } /* end for */
      } /* end if */

    /* Test decoding macros */
    p=encode_buffer;
    INT16DECODE(p,di16);    /* Decode the int16 value */
    UINT16DECODE(p,du16);   /* Decode the uint16 value */
    INT32DECODE(p,di32);    /* Decode the int32 value */
    UINT32DECODE(p,du32);   /* Decode the uint32 value */

    /* Check the values decoded */
    if(di16!=TEST_INT16_VALUE)
      {
        print_func("Error decoding int16 meta-data wanted: %d, got: %d\n",(int)TEST_INT16_VALUE,(int)di16);
        num_errs++;
      } /* end if */
    if(du16!=TEST_UINT16_VALUE)
      {
        print_func("Error decoding uint16 meta-data wanted: %u, got: %u\n",(unsigned)TEST_UINT16_VALUE,(unsigned)du16);
        num_errs++;
      } /* end if */
    if(di32!=TEST_INT32_VALUE)
      {
        print_func("Error decoding int32 meta-data wanted: %ld, got: %ld\n",(long)TEST_INT32_VALUE,(long)di32);
        num_errs++;
      } /* end if */
    if(du32!=TEST_UINT32_VALUE)
      {
        print_func("Error decoding uint32 meta-data wanted: %lu, got: %lu\n",(unsigned long)TEST_UINT32_VALUE,(unsigned long)du32);
        num_errs++;
      } /* end if */
}   /* test_metadata() */

