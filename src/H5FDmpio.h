/*
 * Copyright © 1999 NCSA
 *                  All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Monday, August  2, 1999
 *
 * Purpose:	The public header file for the mpio driver.
 */
#ifndef H5FDmpio_H
#define H5FDmpio_H

#include <H5FDpublic.h>
#include <H5Ipublic.h>

#ifdef HAVE_PARALLEL
#   define H5FD_MPIO	(H5FD_mpio_init())
#else
#   define H5FD_MPIO	(-1)
#endif

/* Type of I/O for data transfer properties */
typedef enum H5FD_mpio_xfer_t {
    H5FD_MPIO_INDEPENDENT = 0, 		/*zero is the default*/
    H5FD_MPIO_COLLECTIVE
} H5FD_mpio_xfer_t;

/* MPIO-specific data transfer properties */
typedef struct H5FD_mpio_dxpl_t {
    H5FD_mpio_xfer_t	xfer_mode;	/*collective or independent I/O	*/
} H5FD_mpio_dxpl_t;
    
/* Function prototypes */
#ifdef HAVE_PARALLEL
hid_t H5FD_mpio_init(void);
herr_t H5Pset_fapl_mpio(hid_t fapl_id, MPI_Comm comm, MPI_Info info);
herr_t H5Pget_fapl_mpio(hid_t fapl_id, MPI_Comm *comm/*out*/,
			MPI_Info *info/*out*/);
herr_t H5Pset_dxpl_mpio(hid_t dxpl_id, H5FD_mpio_xfer_t xfer_mode);
herr_t H5Pget_dxpl_mpio(hid_t dxpl_id, H5FD_mpio_xfer_t *xfer_mode/*out*/);
htri_t H5FD_mpio_tas_allsame(H5FD_t *_file, hbool_t newval);
MPI_Comm H5FD_mpio_communicator(H5FD_t *_file);
herr_t H5FD_mpio_setup(H5FD_t *_file, MPI_Datatype btype, MPI_Datatype ftype,
		       haddr_t disp, hbool_t use_types);
herr_t H5FD_mpio_wait_for_left_neighbor(H5FD_t *file);
herr_t H5FD_mpio_signal_right_neighbor(H5FD_t *file);

#endif /*HAVE_PARALLEL*/

#endif
