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

#else /*WIN32*/
#define __DLL__
#define __DLLVAR__ extern
#endif

#endif /* H5API_ADPT_H */
