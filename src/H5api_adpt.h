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
#define H5_DLL __declspec(dllexport)
#define H5_DLLVAR __declspec(dllexport)
#elif defined(_HDF5USEDLL_)
#define H5_DLL __declspec(dllimport)
#define H5_DLLVAR __declspec(dllimport)
#else
#define H5_DLL
#define H5_DLLVAR extern
#endif /* _HDF5DLL_ */

// Added to export or to import C++ APIs - BMR (02-15-2002)
#if defined(HDF5_CPPDLL_EXPORTS) // this name is generated at creation
#define H5_DLLCPP __declspec(dllexport)
#elif defined(HDF5CPP_USEDLL)
#define H5_DLLCPP __declspec(dllimport)
#else
#define H5_DLLCPP
#endif /* HDF5_CPPDLL_EXPORTS */

#else /*WIN32*/
#define H5_DLL
#define H5_DLLVAR extern
#define H5_DLLCPP
#endif

#endif /* H5API_ADPT_H */
