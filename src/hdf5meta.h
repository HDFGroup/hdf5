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

/* $Id$ */

/*
 * This file contains macros & information for file meta information
 * (offsets, lengths, etc.)
 */

#ifndef HDF5META_H
#define HDF5META_H

/*-----------------------------------------------------*/
/*     Encode and decode macros for file meta-data     */
/*   (Currently, all file meta-data is little-endian)  */
/*-----------------------------------------------------*/

/* For non-little-endian platforms, encode each byte by itself */
#if ((DF_MT&0xFFF0)!=0x4440)
#   define INT16ENCODE(p, i) \
{ *(p) = (uint8)((uintn)(i) & 0xff); (p)++;  *(p) = (uint8)(((uintn)(i) >> 8) & 0xff); (p)++; }

#   define UINT16ENCODE(p, i) \
{ *(p) = (uint8)((i) & 0xff); (p)++; *(p) = (uint8)(((uintn)(i) >> 8) & 0xff); (p)++; }

#   define INT32ENCODE(p, i) \
{ *(p) = (uint8)((uint32)(i) & 0xff); (p)++; \
    *(p) = (uint8)(((uint32)(i) >> 8) & 0xff); (p)++; \
    *(p) = (uint8)(((uint32)(i) >> 16) & 0xff); (p)++; \
    *(p) = (uint8)(((uint32)(i) >> 24) & 0xff); (p)++; }

#   define UINT32ENCODE(p, i) \
{ *(p) = (uint8)((i) & 0xff); (p)++; \
        *(p) = (uint8)(((i) >> 8) & 0xff); (p)++; \
        *(p) = (uint8)(((i) >> 16) & 0xff); (p)++; \
        *(p) = (uint8)(((i) >> 24) & 0xff); (p)++; }
#ifdef HDF5_HAVE_NATIVE_INT64
#   define INT64ENCODE(p, i) \
{ *(p) = (uint8)((uint64)(i) & 0xff); (p)++; \
    *(p) = (uint8)(((uint64)(i) >> 8) & 0xff); (p)++; \
    *(p) = (uint8)(((uint64)(i) >> 16) & 0xff); (p)++; \
    *(p) = (uint8)(((uint64)(i) >> 24) & 0xff); (p)++; \
    *(p) = (uint8)(((uint64)(i) >> 32) & 0xff); (p)++; \
    *(p) = (uint8)(((uint64)(i) >> 40) & 0xff); (p)++; \
    *(p) = (uint8)(((uint64)(i) >> 48) & 0xff); (p)++; \
    *(p) = (uint8)(((uint64)(i) >> 56) & 0xff); (p)++; }

#   define UINT64ENCODE(p, i) \
{ *(p) = (uint8)((i) & 0xff); (p)++; \
        *(p) = (uint8)(((i) >> 8) & 0xff); (p)++; \
        *(p) = (uint8)(((i) >> 16) & 0xff); (p)++; \
        *(p) = (uint8)(((i) >> 24) & 0xff); (p)++; \
        *(p) = (uint8)(((i) >> 32) & 0xff); (p)++; \
        *(p) = (uint8)(((i) >> 40) & 0xff); (p)++; \
        *(p) = (uint8)(((i) >> 48) & 0xff); (p)++; \
        *(p) = (uint8)(((i) >> 56) & 0xff); (p)++; }
#else /* HDF5_HAVE_NATIVE_INT64 */
#error  "Define int64 on platforms which don't support it"
#endif /* HDF5_HAVE_NATIVE_INT64 */

#   define INT16DECODE(p, i) \
{ (i) = (int16)((*(p) & 0xff)); (p)++; \
    (i) |= (int16)((*(p) & 0xff) << 8); (p)++; }

#   define UINT16DECODE(p, i) \
{ (i) = (uint16)(*(p) & 0xff); (p)++; \
    (i) |= (uint16)((*(p) & 0xff) << 8); (p)++; }

#   define INT32DECODE(p, i) \
{ (i) = (*(p) & 0xff); (p)++; \
    (i) |= ((int32)(*(p) & 0xff) << 8); (p)++; \
    (i) |= ((int32)(*(p) & 0xff) << 16); (p)++; \
    (i) |= ((int32)(*(p) & 0xff) << 24); (p)++; }

#   define UINT32DECODE(p, i) \
{ (i) = (uint32)(*(p) & 0xff); (p)++; \
    (i) |= ((uint32)(*(p) & 0xff) << 8); (p)++; \
    (i) |= ((uint32)(*(p) & 0xff) << 16); (p)++; \
    (i) |= ((uint32)(*(p) & 0xff) << 24); (p)++; }

#ifdef HDF5_HAVE_NATIVE_INT64
#define INT64DECODE(p, i) \
{ (i) = (*(p) & 0xff); (p)++; \
    (i) |= ((int64)(*(p) & 0xff) << 8); (p)++; \
    (i) |= ((int64)(*(p) & 0xff) << 16); (p)++; \
    (i) |= ((int64)(*(p) & 0xff) << 24); (p)++; \
    (i) |= ((int64)(*(p) & 0xff) << 32); (p)++; \
    (i) |= ((int64)(*(p) & 0xff) << 40); (p)++; \
    (i) |= ((int64)(*(p) & 0xff) << 48); (p)++; \
    (i) |= ((int64)(*(p) & 0xff) << 56); (p)++; }

#   define UINT64DECODE(p, i) \
{ (i) = (uint64)(*(p) & 0xff); (p)++; \
    (i) |= ((uint64)(*(p) & 0xff) << 8); (p)++; \
    (i) |= ((uint64)(*(p) & 0xff) << 16); (p)++; \
    (i) |= ((uint64)(*(p) & 0xff) << 24); (p)++; \
    (i) |= ((uint64)(*(p) & 0xff) << 32); (p)++; \
    (i) |= ((uint64)(*(p) & 0xff) << 40); (p)++; \
    (i) |= ((uint64)(*(p) & 0xff) << 48); (p)++; \
    (i) |= ((uint64)(*(p) & 0xff) << 56); (p)++; }
#else /* HDF5_HAVE_NATIVE_INT64 */
#error  "Define int64 on platforms which don't support it"
#endif /* HDF5_HAVE_NATIVE_INT64 */
#else   /* platform has little-endian integers */
/* For little-endian platforms, make the compiler do the work */
#   define INT16ENCODE(p, i) { *((int16 *)(p)) = (int16)(i); (p)+=2; }
#   define UINT16ENCODE(p, i) { *((uint16 *)(p)) = (uint16)(i); (p)+=2; }
#   define INT32ENCODE(p, i) { *((int32 *)(p)) = (int32)(i); (p)+=4; }
#   define UINT32ENCODE(p, i) { *((uint32 *)(p)) = (uint32)(i); (p)+=4; }
#ifdef HDF5_HAVE_NATIVE_INT64
#   define INT64ENCODE(p, i) { *((int64 *)(p)) = (int64)(i); (p)+=8; }
#   define UINT64ENCODE(p, i) { *((uint64 *)(p)) = (uint64)(i); (p)+=8; }
#else /* HDF5_HAVE_NATIVE_INT64 */
#error  "Define int64 on platforms which don't support it"
#endif /* HDF5_HAVE_NATIVE_INT64 */
#   define INT16DECODE(p, i) { (i) = (int16)(*(const int16 *)(p)); (p)+=2; }
#   define UINT16DECODE(p, i) { (i) = (uint16)(*(const uint16 *)(p)); (p)+=2; }
#   define INT32DECODE(p, i) { (i) = (int32)(*(const int32 *)(p)); (p)+=4; }
#   define UINT32DECODE(p, i) { (i) = (uint32)(*(const uint32 *)(p)); (p)+=4; }
#ifdef HDF5_HAVE_NATIVE_INT64
#   define INT64DECODE(p, i) { (i) = (int64)(*(const int64 *)(p)); (p)+=8; }
#   define UINT64DECODE(p, i) { (i) = (uint64)(*(const uint64 *)(p)); (p)+=8; }
#else /* HDF5_HAVE_NATIVE_INT64 */
#error  "Define int64 on platforms which don't support it"
#endif /* HDF5_HAVE_NATIVE_INT64 */
#endif

#   define NBYTEENCODE(d, s, n) {   HDmemcpy(d,s,n); p+=n }

/* Note! the NBYTEDECODE macro is backwards from the memcpy() routine, */
/*      in the spirit of the other DECODE macros */
#   define NBYTEDECODE(s, d, n) {   HDmemcpy(d,s,n); p+=n }

#endif /* HDF5I_H */

