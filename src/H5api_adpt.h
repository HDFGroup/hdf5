/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  * Copyright by the Board of Trustees of the University of Illinois.         *
  * All rights reserved.                                                      *
  *                                                                           *
  * This file is part of HDF5.  The full HDF5 copyright notice, including     *
  * terms governing use, modification, and redistribution, is contained in    *
  * the files COPYING and Copyright.html.  COPYING can be found at the root   *
  * of the source code distribution tree; Copyright.html can be found at the  *
  * root level of an installed copy of the electronic HDF5 document set and   *
  * is linked from the top-level documents page.  It can also be found at     *
  * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
  * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
	H5api_adpt.h
	Used for the HDF5 dll project
	Created by Patrick Lu on 1/12/99
*/
#ifndef H5API_ADPT_H
#define H5API_ADPT_H

#if defined(WIN32)

#if defined(_HDF5DLL_)
#pragma warning(disable: 4273)	/* Disable the dll linkage warnings */
#define __DLL__ __declspec(dllexport)
#define __DLLVAR__ __declspec(dllexport)
#elif defined(_HDF5USEDLL_)
#define __DLL__ __declspec(dllimport)
#define __DLLVAR__ __declspec(dllimport)
#else
#define __DLL__
#define __DLLVAR__ extern
#endif /* _HDF5DLL_ */

// Added to export or to import C++ APIs - BMR (01-29-2002)
#if defined(HDF5_CPPDLL_EXPORTS) // this name is generated at creation
#define __DLLCPP__ __declspec(dllexport)
#elif defined(HDF5CPP_USEDLL)
#define __DLLCPP__ __declspec(dllimport)
#else
#define __DLLCPP__
#endif /* HDF5_CPPDLL_EXPORTS */

#else /*WIN32*/
#define __DLL__
#define __DLLVAR__ extern
#define __DLLCPP__
#endif

#endif /* H5API_ADPT_H */
