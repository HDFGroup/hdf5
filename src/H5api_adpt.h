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
#define HDF5API __declspec(dllexport)
#define HDF5GLOBAL __declspec(dllexport)
#else
#define HDF5API
#define HDF5GLOBAL extern
#endif /* _HDF5DLL_ */

#else /*WIN32*/
#define HDF5API
#define HDF5GLOBAL extern
#endif

#endif /* H5API_ADPT_H */