/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * This file contains function prototypes for each exported function in
 * the H5I module.
 */
#ifndef _H5Ipublic_H
#define _H5Ipublic_H

/* Public headers needed by this file */
#include "H5public.h"

/**
 * Library type values.
 * \internal Library type values.  Start with `1' instead of `0' because it
 *           makes the tracing output look better when hid_t values are large
 *           numbers. Change the TYPE_BITS in H5I.c if the MAXID gets larger
 *           than 32 (an assertion will fail otherwise).
 *
 *           When adding types here, add a section to the 'misc19' test in
 *           test/tmisc.c to verify that the H5I{inc|dec|get}_ref() routines
 *           work correctly with it. \endinternal
 */
typedef enum H5I_type_t {
    H5I_UNINIT      = (-2),     /**< uninitialized type                        */
    H5I_BADID       = (-1),     /**< invalid Type                              */
    H5I_FILE        = 1,        /**< type ID for File objects                  */
    H5I_GROUP,                  /**< type ID for Group objects                 */
    H5I_DATATYPE,               /**< type ID for Datatype objects              */
    H5I_DATASPACE,              /**< type ID for Dataspace objects             */
    H5I_DATASET,                /**< type ID for Dataset objects               */
    H5I_MAP,                    /**< type ID for Map objects                   */
    H5I_ATTR,                   /**< type ID for Attribute objects             */
    H5I_VFL,                    /**< type ID for virtual file layer            */
    H5I_VOL,                    /**< type ID for virtual object layer          */
    H5I_GENPROP_CLS,            /**< type ID for generic property list classes */
    H5I_GENPROP_LST,            /**< type ID for generic property lists        */
    H5I_ERROR_CLASS,            /**< type ID for error classes                 */
    H5I_ERROR_MSG,              /**< type ID for error messages                */
    H5I_ERROR_STACK,            /**< type ID for error stacks                  */
    H5I_SPACE_SEL_ITER,         /**< type ID for dataspace selection iterator  */
    H5I_NTYPES                  /**< number of library types, MUST BE LAST!    */
} H5I_type_t;

/**
 * Type of atoms to return to users
 */
typedef int64_t hid_t;
/**
 * The size of identifiers
 */
#define H5_SIZEOF_HID_T         H5_SIZEOF_INT64_T

/**
 * An invalid object ID. This is also negative for error return.
 */
#define H5I_INVALID_HID         (-1)

/**
 * A function for freeing objects. This function will be called with an object
 * ID type number and a pointer to the object. The function should free the
 * object and return non-negative to indicate that the object
 * can be removed from the ID type. If the function returns negative
 * (failure) then the object will remain in the ID type.
 */
typedef herr_t (*H5I_free_t)(void *);

/**
 * The type of a function to compare objects & keys
 */
typedef int (*H5I_search_func_t)(void *obj, hid_t id, void *key);

/**
 * The type of H5Iiterate() callback functions
 */
typedef herr_t (*H5I_iterate_func_t)(hid_t id, void *udata);

#ifdef __cplusplus
extern "C" {
#endif

/* Public API functions */

/**
 *-------------------------------------------------------------------------
 * \ingroup H5I
 *
 * \brief Registers an object under a type and returns an ID for it
 *
 * \param[in] type The identifier of the type of the new ID
 * \param[in] object Pointer to object for which a new ID is created
 *
 * \return \hid_t{object}
 *
 * \details H5Iregister() allocates and returns a new ID for an object.
 *
 * \details The \p type parameter is the identifier for the ID type to which
 *          this new ID will belong. This identifier must have been created by
 *          a call to H5Iregister_type().
 *
 * \details The \p object parameter is a pointer to the memory which the new ID
 *          will be a reference to. This pointer will be stored by the library
 *          and returned via a call to H5Iobject_verify().
 *
 *-------------------------------------------------------------------------
 */
H5_DLL hid_t H5Iregister(H5I_type_t type, const void *object);
/**
 *-------------------------------------------------------------------------
 * \ingroup H5I
 *
 * \brief Returns the object referenced by an ID
 *
 * \param[in] id ID to be dereferenced
 * \param[in] id_type The identifier type

 *
 * \return Pointer to the object referenced by \p id on success, NULL on failure.
 *
 * \details H5Iobject_verify() returns a pointer to the memory referenced by id
 *          after verifying that \p id is of type \p id_type. This function is
 *          analogous to dereferencing a pointer in C with type checking.
 *
 * \note H5Iobject_verify() does not change the ID it is called on in any way
 *       (as opposed to H5Iremove_verify(), which removes the ID from its
 *       type’s hash table).
 *
 * \see H5Iregister()
 *
 *-------------------------------------------------------------------------
 */
H5_DLL void *H5Iobject_verify(hid_t id, H5I_type_t id_type);
/**
 *-------------------------------------------------------------------------
 * \ingroup H5I
 *
 * \brief Removes an ID from its type
 *
 * \param[in] id The ID to be removed from its type
 * \param[in] id_type The identifier type

 *
 * \return Returns a pointer to the memory referred to by \p id on success,
 *         NULL on failure.
 *
 * \details H5Iremove_verify() first ensures that \p id belongs to \p id_type.
 *          If so, it removes \p id from its type and returns the pointer
 *          to the memory it referred to. This pointer is the same pointer that
 *          was placed in storage by H5Iregister(). If id does not belong to
 *          \p id_type, then NULL is returned.
 *
 *          The \p id parameter is the ID which is to be removed from its type.
 *
 *          The \p type parameter is the identifier for the ID type which \p id
 *          is supposed to belong to. This identifier must have been created by
 *          a call to H5Iregister_type().
 *
 * \note This function does NOT deallocate the memory that \p id refers to.
 *       The pointer returned by H5Iregister() must be deallocated by the user
 *       to avoid memory leaks.
 *
 *-------------------------------------------------------------------------
 */
H5_DLL void *H5Iremove_verify(hid_t id, H5I_type_t id_type);
/**
 *-------------------------------------------------------------------------
 * \ingroup H5I
 *
 * \brief Retrieves the type of an object
 *
 * \obj_id{id}
 *
 * \return Returns the object type if successful; otherwise #H5I_BADID.
 *
 * \details H5Iget_type() retrieves the type of the object identified by
 *          \p id.
 *
 *          Valid types returned by the function are:
 *          \id_types
 *
 *          If no valid type can be determined or the identifier submitted is
 *          invalid, the function returns #H5I_BADID.
 *
 *          This function is of particular use in determining the type of
 *          object closing function (H5Dclose(), H5Gclose(), etc.) to call
 *          after a call to H5Rdereference().
 *
 * \note Note that this function returns only the type of object that \p id
 *       would identify if it were valid; it does not determine whether \p id
 *       is valid identifier. Validity can be determined with a call to
 *       H5Iis_valid().
 *
 *-------------------------------------------------------------------------
 */
H5_DLL H5I_type_t H5Iget_type(hid_t id);
H5_DLL hid_t H5Iget_file_id(hid_t id);
H5_DLL ssize_t H5Iget_name(hid_t id, char *name/*out*/, size_t size);
H5_DLL int H5Iinc_ref(hid_t id);
H5_DLL int H5Idec_ref(hid_t id);
H5_DLL int H5Iget_ref(hid_t id);
H5_DLL H5I_type_t H5Iregister_type(size_t hash_size, unsigned reserved, H5I_free_t free_func);
H5_DLL herr_t H5Iclear_type(H5I_type_t type, hbool_t force);
H5_DLL herr_t H5Idestroy_type(H5I_type_t type);
H5_DLL int H5Iinc_type_ref(H5I_type_t type);
H5_DLL int H5Idec_type_ref(H5I_type_t type);
H5_DLL int H5Iget_type_ref(H5I_type_t type);
H5_DLL void *H5Isearch(H5I_type_t type, H5I_search_func_t func, void *key);
H5_DLL herr_t H5Iiterate(H5I_type_t type, H5I_iterate_func_t op, void *op_data);
H5_DLL herr_t H5Inmembers(H5I_type_t type, hsize_t *num_members);
H5_DLL htri_t H5Itype_exists(H5I_type_t type);
H5_DLL htri_t H5Iis_valid(hid_t id);

#ifdef __cplusplus
}
#endif
#endif /* _H5Ipublic_H */
