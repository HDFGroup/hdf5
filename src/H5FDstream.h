/*
 * Copyright © 2000 The author.
 * The author prefers this code not be used for military purposes.
 *
 *
 * Author:  Thomas Radke <tradke@aei-potsdam.mpg.de>
 *          Tuesday, September 12, 2000
 *
 * Purpose:	The public header file for the Stream Virtual File Driver.
 */
#ifndef H5FDstream_H
#define H5FDstream_H

#ifdef H5_HAVE_STREAM

#include <H5Ipublic.h>

#ifdef __cplusplus
extern "C" {
#endif

#define H5FD_STREAM	(H5FD_stream_init())

/* prototype for read broadcast callback routine */
typedef int (*H5FD_stream_broadcast_t) (unsigned char **file,
                                        haddr_t *len,
                                        void *arg);

/* Driver-specific file access properties */
typedef struct H5FD_stream_fapl_t
{
  size_t       increment;            /* how much to grow memory in reallocs  */
  int          socket;               /* external socket descriptor           */
  hbool_t      do_socket_io;         /* do I/O on socket                     */
  unsigned int backlog;              /* backlog argument for listen call     */
  H5FD_stream_broadcast_t broadcast_fn; /* READ broadcast callback           */
  void        *broadcast_arg;        /* READ broadcast callback user argument*/
} H5FD_stream_fapl_t;


__DLL__ hid_t  H5FD_stream_init (void);
__DLL__ herr_t H5Pset_fapl_stream (hid_t fapl_id,
                                   H5FD_stream_fapl_t *fapl);
__DLL__ herr_t H5Pget_fapl_stream (hid_t fapl_id,
                                   H5FD_stream_fapl_t *fapl /*out*/ );

#ifdef __cplusplus
}
#endif

#endif /* H5_HAVE_STREAM */

#endif /* H5FDstream_H */
