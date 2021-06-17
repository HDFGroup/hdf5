/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * H5Idbg.c - Debugging routines for handling IDs
 */

/****************/
/* Module Setup */
/****************/

#include "H5Imodule.h" /* This source code file is part of the H5I module */

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions                        */
#include "H5Dprivate.h"  /* Datasets                                 */
#include "H5Eprivate.h"  /* Error handling                           */
#include "H5Gprivate.h"  /* Groups                                   */
#include "H5Ipkg.h"      /* IDs                                      */
#include "H5RSprivate.h" /* Reference-counted strings                */
#include "H5Tprivate.h"  /* Datatypes                                */

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/

/********************/
/* Package Typedefs */
/********************/

/********************/
/* Local Prototypes */
/********************/

/*********************/
/* Package Variables */
/*********************/

/*****************************/
/* Library Private Variables */
/*****************************/

/*******************/
/* Local Variables */
/*******************/

