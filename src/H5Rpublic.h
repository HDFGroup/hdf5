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
 * This file contains public declarations for the H5R module.
 */
#ifndef H5Rpublic_H
#define H5Rpublic_H

/* Public headers needed by this file */
#include "H5public.h"
#include "H5Gpublic.h"
#include "H5Ipublic.h"

/*****************/
/* Public Macros */
/*****************/

/* Deprecated reference buffer sizes that are kept for backward compatibility */
#define H5R_OBJ_REF_BUF_SIZE      sizeof(haddr_t)
#define H5R_DSET_REG_REF_BUF_SIZE (sizeof(haddr_t) + 4)

/* Default reference buffer size.
 * Note! Be careful with the sizes of the references because they should really
 * depend on the run-time values in the file.
 */
#define H5R_REF_BUF_SIZE (64)

/*******************/
/* Public Typedefs */
/*******************/

/*
 * Reference types allowed.
 * DO NOT CHANGE THE ORDER or VALUES as reference type values are encoded into
 * the datatype message header.
 */
typedef enum {
    H5R_BADTYPE         = (-1), /* Invalid reference type           */
    H5R_OBJECT1         = 0,    /* Backward compatibility (object)  */
    H5R_DATASET_REGION1 = 1,    /* Backward compatibility (region)  */
    H5R_OBJECT2         = 2,    /* Object reference                 */
    H5R_DATASET_REGION2 = 3,    /* Region reference                 */
    H5R_ATTR            = 4,    /* Attribute Reference              */
    H5R_MAXTYPE         = 5     /* Highest type (invalid)           */
} H5R_type_t;

/* Deprecated types are kept for backward compatibility with previous versions */

/**
 * Deprecated object reference type that is used with deprecated reference APIs.
 * Note! This type can only be used with the "native" HDF5 VOL connector.
 */
typedef haddr_t hobj_ref_t;

/**
 * Dataset region reference type that is used with deprecated reference APIs.
 * (Buffer to store heap ID and index)
 * This needs to be large enough to store largest haddr_t in a worst case
 * machine (8 bytes currently) plus an int.
 * Note! This type can only be used with the "native" HDF5 VOL connector.
 */
typedef struct {
    uint8_t __data[H5R_DSET_REG_REF_BUF_SIZE];
} hdset_reg_ref_t;

/**
 * Opaque reference type. The same reference type is used for object,
 * dataset region and attribute references. This is the type that
 * should always be used with the current reference API.
 */
typedef struct {
    union {
        uint8_t __data[H5R_REF_BUF_SIZE]; /* opaque data */
        int64_t align;                    /* ensures alignment */
    } u;
} H5R_ref_t;

/********************/
/* Public Variables */
/********************/

/*********************/
/* Public Prototypes */
/*********************/

#ifdef __cplusplus
extern "C" {
#endif

/* Constructors */
/**
 * --------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Creates an object reference
 *
 * \fgdta_loc_id
 * \param[in] name      Name of object
 * \oapl_id
 * \param[out] ref_ptr  Pointer to reference
 *
 * \return \herr_t
 *
 * \details H5Rcreate_object() creates a reference pointing to the
 *          object named \p name located at \p loc_id. The parameters \p
 *          loc_id and \p name are used to locate the object.
 *
 *          The parameter \p oapl_id is an object access property list
 *          identifier for the referenced object. The access property list
 *          must be of the same type as the object being referenced, that is
 *          a group, dataset or committed datatype property list.
 *
 *          H5R_ref_t is defined in H5Rpublic.h as:   typedef unsigned char
 *          H5R_ref_t[#H5R_REF_BUF_SIZE];
 *
 *          H5Rdestroy() should be used to release the resource from the
 *          reference.
 *
 */
H5_DLL herr_t H5Rcreate_object(hid_t loc_id, const char *name, hid_t oapl_id, H5R_ref_t *ref_ptr);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Creates a region reference
 *
 * \fgdta_loc_id
 * \param[in] name      Name of object
 * \space_id
 * \oapl_id
 * \param[out] ref_ptr  Pointer to reference
 *
 * \return \herr_t
 *
 * \details H5Rcreate_region() creates the reference, \p ref_ptr,
 *          pointing to the region represented by \p space_id within the
 *          object named name located at \p loc_id.
 *
 *          The parameters \p loc_id and \p name are used to locate the
 *          object. The parameter \p space_id identifies the dataset region
 *          that a dataset region reference points to.
 *
 *          The parameter \p oapl_id is an object access property list
 *          identifier for the referenced object. The access property list
 *          must be of the same type as the object being referenced, that is
 *          a dataset property list in this case.
 *
 *          H5R_ref_t is defined in H5Rpublic.h as:   typedef unsigned char
 *          H5R_ref_t[#H5R_REF_BUF_SIZE];
 *
 *          H5Rdestroy() should be used to release the resource from the
 *          reference.
 *
 * \see function_name()
 *
 */
H5_DLL herr_t H5Rcreate_region(hid_t loc_id, const char *name, hid_t space_id, hid_t oapl_id,
                               H5R_ref_t *ref_ptr);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Creates an attribute reference
 *
 * \fgdta_loc_id
 * \param[in] name      Name of object
 * \param[in] attr_name Name of attribute
 * \oapl_id
 * \param[out] ref_ptr  Pointer to reference
 *
 * \return \herr_t
 *
 * \details H5Rcreate_attr() creates the reference, \p ref_ptr, pointing
 *          to the attribute named \p attr_name and attached to the object
 *          named \p name located at \p loc_id.
 *
 *          The parameters \p loc_id and \p name locate the object. The
 *          parameter \p attr_name specifies the attribute within the object.
 *
 *          The parameter \p oapl_id is an object access property list
 *          identifier for the object that the referenced attribute is
 *          attached to. The access property list must be of the same type
 *          as that object, that is a group, dataset or committed datatype
 *          property list.
 *
 *          H5R_ref_t is defined in H5Rpublic.h as:   typedef unsigned char
 *          H5R_ref_t[#H5R_REF_BUF_SIZE];
 *
 *          H5Rdestroy() should be used to release the resource from the
 *          reference.
 *
 */
H5_DLL herr_t H5Rcreate_attr(hid_t loc_id, const char *name, const char *attr_name, hid_t oapl_id,
                             H5R_ref_t *ref_ptr);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Closes a reference
 *
 * \param[in] ref_ptr  Pointer to reference
 *
 * \return \herr_t
 *
 * \details Given a reference, ref_ptr, to an object, region or attribute
 *          attached to an object, H5R_DESTROY releases allocated resources
 *          from a previous create call.
 *
 *          H5R_ref_t is defined in H5Rpublic.h as: typedef unsigned char
 *          H5R_ref_t[#H5R_REF_BUF_SIZE];
 *
 */
H5_DLL herr_t H5Rdestroy(H5R_ref_t *ref_ptr);

/* Info */

/**
 * --------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Retrieves the type of a reference
 *
 * \param[in] ref_ptr  Pointer to reference
 *
 * \return Returns a valid reference type if successful; otherwise returns #H5R_UNKNOWN.
 *
 * \details Given a reference, \p ref_ptr, H5Rget_type() returns the
 *          type of the reference.
 *
 *          Valid returned reference types are:
 *
 *          #H5R_OBJECT2 Object reference version 2
 *          #H5R_DATASET_REGION2 Region reference version 2
 *          #H5R_ATTRIBUTE   Attribute reference
 *
 *          Note that #H5R_OBJECT1 and #H5R_DATASET REGION1 can never be
 *          associated to an H5R_ref_t reference and can therefore never be
 *          returned through that function.
 *
 *          H5R_ref_t is defined in H5Rpublic.h as:   typedef unsigned char
 *          H5R_ref_t[#H5R_REF_BUF_SIZE];
 *
 */
H5_DLL H5R_type_t H5Rget_type(const H5R_ref_t *ref_ptr);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Determines whether two references are equal
 *
 * \param[in]  ref1_ptr  Pointer to reference to compare
 * \param[in]  ref2_ptr  Pointer to reference to compare
 *
 * \return Returns a positive value if the references are equal. Returns
 *          0 if the references are not equal. Returns a negative value when the
 *          function fails.
 *
 * \details H5Requal() determines whether two references point to the
 *          same object, region or attribute.
 *
 *          H5R_ref_t is defined in H5Rpublic.h as: typedef unsigned char
 *          H5R_ref_t[#H5R_REF_BUF_SIZE];
 *
 */
H5_DLL htri_t H5Requal(const H5R_ref_t *ref1_ptr, const H5R_ref_t *ref2_ptr);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Copies an existing reference
 *
 * \param[in]  src_ref_ptr  Pointer to reference to copy
 * \param[out] dst_ref_ptr  Pointer to output reference
 *
 * \return \herr_t
 *
 * \details H5Rcopy() creates a copy of an existing reference.
 *          \p src_ref_ptr points to the reference to copy and \p dst_ref_ptr is the
 *          pointer to the destination reference.
 *
 */
H5_DLL herr_t H5Rcopy(const H5R_ref_t *src_ref_ptr, H5R_ref_t *dst_ref_ptr);

/* Dereference */

/**
 * --------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Opens the HDF5 object referenced
 *
 * \param[in] ref_ptr  Pointer to reference to open
 * \rapl_id
 * \oapl_id
 *
 * \return \hid_t{object}
 *
 * \details Given a reference, \p ref_ptr, to an object, a region in
 *          an object, or an attribute attached to an object, H5Ropen_object()
 *          opens that object and returns an identifier.
 *
 *          The parameter \p oapl_id is an object access property list
 *          identifier for the referenced object. The access property list
 *          must be of the same type as the object being referenced, that is
 *          a group or dataset property list.
 *
 *          H5R_ref_t is defined in H5Rpublic.h as:   typedef unsigned char
 *          H5R_ref_t[#H5R_REF_BUF_SIZE];
 *
 *          The object opened with this function should be closed when it
 *          is no longer needed so that resource leaks will not develop. Use
 *          the appropriate close function such as H5Oclose() or H5Dclose()
 *          for datasets.
 *
 */
H5_DLL hid_t H5Ropen_object(H5R_ref_t *ref_ptr, hid_t rapl_id, hid_t oapl_id);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Asynchronous version of H5Ropen_object()
 *
 * \app_file
 * \app_func
 * \app_line
 * \param[out] ref_ptr  Pointer to reference to open
 * \rapl_id
 * \oapl_id
 * \es_id
 *
 * \return \hid_t{object}
 *
 * \see H5Ropen_object()
 *
 */
H5_DLL hid_t H5Ropen_object_async(const char *app_file, const char *app_func, unsigned app_line,
                                  H5R_ref_t *ref_ptr, hid_t rapl_id, hid_t oapl_id, hid_t es_id);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Sets up a dataspace and selection as specified by a region reference.
 *
 * \param[in] ref_ptr  Pointer to reference to open
 * \rapl_id
 * \oapl_id
 *
 * \return \hid_t{dataspace}
 *
 * \details H5Ropen_region() creates a copy of the dataspace of the
 *          dataset pointed to by a region reference, \p ref_ptr, and defines
 *          a selection matching the selection pointed to by \p ref_ptr within
 *          the dataspace copy.
 *
 *          The parameter \p rapl id is a reference access property list
 *          identifier for the reference. The access property list can
 *          be used to access external files that the reference points to
 *          (through a file access property list).
 *
 *          The parameter \p oapl id is an object access property list
 *          identifier for the referenced object. The access property list
 *          must be of the same type as the object being referenced, that is
 *          a dataset property list in that case.
 *
 *          Use H5Sclose() to release the dataspace identifier returned by
 *          this function when the identifier is no longer needed.
 *
 */
H5_DLL hid_t H5Ropen_region(H5R_ref_t *ref_ptr, hid_t rapl_id, hid_t oapl_id);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Asynchronous version of H5Ropen_region()
 *
 * \app_file
 * \app_func
 * \app_line
 * \param[in] ref_ptr  Pointer to reference to open
 * \rapl_id
 * \oapl_id
 * \es_id
 *
 * \return \hid_t{dataspace}
 *
 * \see H5Ropen_region()
 *
 */
H5_DLL hid_t H5Ropen_region_async(const char *app_file, const char *app_func, unsigned app_line,
                                  H5R_ref_t *ref_ptr, hid_t rapl_id, hid_t oapl_id, hid_t es_id);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Opens the HDF5 attribute referenced
 *
 * \param[in] ref_ptr  Pointer to reference to open
 * \rapl_id
 * \aapl_id
 *
 * \return \hid_t{attribute}
 *
 * \details Given a reference, \p ref_ptr, to an attribute attached to
 *          an object, H5Ropen_attr() opens the attribute attached to that
 *          object and returns an identifier.
 *
 *          The parameter \p rapl id is a reference access property list
 *          identifier for the reference. The access property list can
 *          be used to access external files that the reference points to
 *          (through a file access property list).
 *
 *          The parameter \p aapl_id is an attribute access property list
 *          identifier for the referenced attribute.
 *
 *          The attribute opened with this function should be closed with
 *          H5Aclose() when it is no longer needed.
 *
 */
H5_DLL hid_t H5Ropen_attr(H5R_ref_t *ref_ptr, hid_t rapl_id, hid_t aapl_id);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Asynchronous version of H5Ropen_attr()
 *
 * \app_file
 * \app_func
 * \app_line
 * \param[in] ref_ptr  Pointer to reference to open
 * \rapl_id
 * \aapl_id
 * \es_id
 *
 * \return \hid_t{attribute}
 *
 * \see H5Ropen_attr()
 *
 */
H5_DLL hid_t H5Ropen_attr_async(const char *app_file, const char *app_func, unsigned app_line,
                                H5R_ref_t *ref_ptr, hid_t rapl_id, hid_t aapl_id, hid_t es_id);

/* Get type */

/**
 * --------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Retrieves the type of object that an object reference points to
 *
 * \param[in] ref_ptr  Pointer to reference to query
 * \rapl_id
 * \param[out] obj_type Type of referenced object
 *
 * \return \herr_t
 *
 * \details Given a reference, \p ref_ptr, H5Rget_obj_type3() retrieves
 *          the type of the referenced object in \p obj_type.
 *
 *          The parameter \p rapl id is a reference access property list
 *          identifier for the reference. The access property list can
 *          be used to access external files that the reference points to
 *          (through a file access property list).
 *
 *          Upon success, the function returns in \p obj_type the type of
 *          the object that the reference points to.  Valid values for this
 *          referenced object type are as followed (defined in H5Opublic.h):
 *
 *          H5O_TYPE_GROUP  Object is a group
 *          H5O_TYPE_DATASET    Object is a dataset
 *          H5O_TYPE_NAMED_DATATYPE Object is a named datatype
 *
 */
H5_DLL herr_t H5Rget_obj_type3(H5R_ref_t *ref_ptr, hid_t rapl_id, H5O_type_t *obj_type);

/* Get name */

/**
 * --------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Retrieves the file name for a referenced object
 *
 * \param[in] ref_ptr  Pointer to reference to query
 * \param[in,out] name Buffer to place the file name of the reference
 * \param[in] size     Size of the \p name buffer
 *
 * \return Returns the length of the name if successful, otherwise, a negative value.
 *
 * \details H5Rget_file_name() retrieves the file name for the object,
 *          region or attribute reference pointed to by \p ref_ptr.
 *
 *          Up to \p size characters of the name are returned in \p name;
 *          additional characters, if any, are not returned to the user
 *          application. If the length of the name, which determines
 *          the required value of size, is unknown, a preliminary
 *          H5Rget_file_name() call can be made. The return value of this
 *          call will be the size of the file name. That value can then be
 *          passed in for size in the second call to H5Rget_file_name(),
 *          which will retrieve the actual name.
 *
 */
H5_DLL ssize_t H5Rget_file_name(const H5R_ref_t *ref_ptr, char *name, size_t size);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Retrieves the object name for a referenced object
 *
 * \param[in] ref_ptr  Pointer to reference to query
 * \rapl_id
 * \param[in,out] name Buffer to place the file name of the reference
 * \param[in] size     Size of the \p name buffer
 *
 * \return Returns the length of the name if successful, returning
 *          0 (zero) if no name is associated with the identifier. Otherwise
 *          returns a negative value.
 *
 * \details H5Rget_obj_name() retrieves the object name for the object,
 *          region or attribute reference pointed to by \p ref_ptr.
 *
 *          The parameter \p rapl id is a reference access property list
 *          identifier for the reference. The access property list can
 *          be used to access external files that the reference points to
 *          (through a file access property list).
 *
 *          Up to size characters of the name are returned in name; additional
 *          characters, if any, are not returned to the user application. If
 *          the length of the name, which determines the required value of
 *          \p size, is unknown, a preliminary call to H5Rget_obj_name() call
 *          can be made. The return value of this call will be the size of the
 *          object name. That value can then be passed in for \p size in the
 *          second call to H5Rget_obj_name(), which will retrieve the actual
 *          name. If there is no name associated with the object identifier
 *          or if the name is #NULL, H5Rget_obj_name() returns the size of
 *          the name buffer (the size does not include the #NULL terminator).
 *
 *          If \p ref_ptr is an object reference, \p name will be returned with
 *          a name for the referenced object. If \p ref_ptr is a dataset region
 *          reference, \p name will contain a name for the object containing
 *          the referenced region. If \p ref_ptr is an attribute reference, \p
 *          name will contain a name for the object the attribute is attached
 *          to. Note that an object in an HDF5 file may have multiple paths
 *          if there are multiple links pointing to it. This function may
 *          return any one of these paths.
 *
 */
H5_DLL ssize_t H5Rget_obj_name(H5R_ref_t *ref_ptr, hid_t rapl_id, char *name, size_t size);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Retrieves the attribute name for a referenced object
 *
 * \param[in] ref_ptr  Pointer to reference to query
 * \param[in,out] name Buffer to place the attribute name of the reference
 * \param[in] size     Size of the \p name buffer
 *
 * \return Returns the length of the name if successful, otherwise, a negative value.
 *
 * \details H5Rget_attr_name() retrieves the attribute name for the
 *          attribute reference pointed to by \p ref_ptr.
 *
 *          Up to size characters of the name are returned in \p name;
 *          additional characters, if any, are not returned to the user
 *          application. If the length of the name, which determines
 *          the required value of \p size, is unknown, a preliminary
 *          H5Rget_attr_name() call can be made. The return value of this
 *          call will be the size of the attribute name. That value can then
 *          be passed in for size in the second call to H5Rget_attr_name(),
 *          which will retrieve the actual name.
 *
 */
H5_DLL ssize_t H5Rget_attr_name(const H5R_ref_t *ref_ptr, char *name, size_t size);

/* API Wrappers for async routines */
/* (Must be defined _after_ the function prototype) */
/* (And must only defined when included in application code, not the library) */
#ifndef H5R_MODULE
#define H5Ropen_object_async(...) H5Ropen_object_async(__FILE__, __func__, __LINE__, __VA_ARGS__)
#define H5Ropen_region_async(...) H5Ropen_region_async(__FILE__, __func__, __LINE__, __VA_ARGS__)
#define H5Ropen_attr_async(...)   H5Ropen_attr_async(__FILE__, __func__, __LINE__, __VA_ARGS__)

/* Define "wrapper" versions of function calls, to allow compile-time values to
 * be passed in by language wrapper or library layer on top of HDF5. */
#define H5Ropen_object_async_wrap H5_NO_EXPAND(H5Ropen_object_async)
#define H5Ropen_region_async_wrap H5_NO_EXPAND(H5Ropen_region_async)
#define H5Ropen_attr_async_wrap   H5_NO_EXPAND(H5Ropen_attr_async)
#endif /* H5R_MODULE */

/* Symbols defined for compatibility with previous versions of the HDF5 API.
 *
 * Use of these symbols is or will be deprecated.
 */

/* Macros */

/* Versions for compatibility */
#define H5R_OBJECT         H5R_OBJECT1
#define H5R_DATASET_REGION H5R_DATASET_REGION1

/* Function prototypes */
#ifndef H5_NO_DEPRECATED_SYMBOLS

H5_DLL H5G_obj_t H5Rget_obj_type1(hid_t id, H5R_type_t ref_type, const void *ref);
H5_DLL hid_t     H5Rdereference1(hid_t obj_id, H5R_type_t ref_type, const void *ref);

#endif /* H5_NO_DEPRECATED_SYMBOLS */

H5_DLL herr_t  H5Rcreate(void *ref, hid_t loc_id, const char *name, H5R_type_t ref_type, hid_t space_id);
H5_DLL herr_t  H5Rget_obj_type2(hid_t id, H5R_type_t ref_type, const void *ref, H5O_type_t *obj_type);
H5_DLL hid_t   H5Rdereference2(hid_t obj_id, hid_t oapl_id, H5R_type_t ref_type, const void *ref);
H5_DLL hid_t   H5Rget_region(hid_t dataset, H5R_type_t ref_type, const void *ref);
H5_DLL ssize_t H5Rget_name(hid_t loc_id, H5R_type_t ref_type, const void *ref, char *name, size_t size);

#ifdef __cplusplus
}
#endif

#endif /* H5Rpublic_H */
