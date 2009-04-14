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

#ifndef _H5HLerror_H
#define _H5HLerror_H

/* Private header files needed */

/* When this header is included from a private header, don't make calls to H5open() */
#undef H5OPEN
#ifndef _H5HLprivate2_H
#define H5OPEN          H5HLopen(),
#else   /* _H5private_H */
#define H5OPEN
#endif  /* _H5private_H */

/* High-Level HDF5 API error class */
#define H5HL_ERR_CLS		(H5OPEN H5HL_ERR_CLS_g)
H5_DLLVAR hid_t H5HL_ERR_CLS_g;


/*********************/
/* Major error codes */
/*********************/

#define H5E_LREF          (H5OPEN H5E_LREF_g)
H5_DLLVAR hid_t H5E_LREF_g;     /* Lite References */


/*********************/
/* Minor error codes */
/*********************/

/* <none defined yet> */


/* The following is copied from src/H5Eprivate.h */

/*
 * HERROR macro, used to facilitate error reporting between a FUNC_ENTER()
 * and a FUNC_LEAVE() within a function body.  The arguments are the major
 * error number, the minor error number, and a description of the error.
 */
#define HERROR(maj_id, min_id, str) H5Epush2(H5E_DEFAULT, __FILE__, FUNC, __LINE__, H5HL_ERR_CLS_g, maj_id, min_id, str)

/*
 * HCOMMON_ERROR macro, used by HDONE_ERROR and HGOTO_ERROR
 * (Shouldn't need to be used outside this header file)
 */
#define HCOMMON_ERROR(maj, min, str)  				              \
   HERROR(maj, min, str);						      \
   (void)H5HLE_dump_api_stack((hbool_t)H5_IS_PUB(FUNC));

/*
 * HGOTO_ERROR macro, used to facilitate error reporting between a
 * FUNC_ENTER() and a FUNC_LEAVE() within a function body.  The arguments are
 * the major error number, the minor error number, the return value, and an
 * error string.  The return value is assigned to a variable `ret_value' and
 * control branches to the `done' label.
 */
#define HGOTO_ERROR(maj, min, ret_val, str) {				      \
   HCOMMON_ERROR(maj, min, str);					      \
   HGOTO_DONE(ret_val)						              \
}

/*
 * HGOTO_DONE macro, used to facilitate normal return between a FUNC_ENTER()
 * and a FUNC_LEAVE() within a function body. The argument is the return
 * value which is assigned to the `ret_value' variable.	 Control branches to
 * the `done' label.
 */
#define HGOTO_DONE(ret_val) {ret_value = ret_val; goto done;}

/* Private routines */
herr_t H5HLE_dump_api_stack(hbool_t is_api);

#endif /* _H5HLerror_H */

