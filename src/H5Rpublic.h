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
 * This file contains public declarations for the H5R module.
 */
#ifndef _H5Rpublic_H
#define _H5Rpublic_H

/* Public headers needed by this file */
#include "H5public.h"
#include "H5Gpublic.h"
#include "H5Ipublic.h"

/*****************/
/* Public Macros */
/*****************/

/* Deprecated reference buffer sizes that are kept for backward compatibility */
#define H5R_OBJ_REF_BUF_SIZE        sizeof(haddr_t)
#define H5R_DSET_REG_REF_BUF_SIZE   (sizeof(haddr_t) + 4)

/* Default reference buffer size.
 * Note! Be careful with the sizes of the references because they should really
 * depend on the run-time values in the file.
 */
#define H5R_REF_BUF_SIZE            (64)

/*******************/
/* Public Typedefs */
/*******************/

/*
 * Reference types allowed.
 * DO NOT CHANGE THE ORDER or VALUES as reference type values are encoded into
 * the datatype message header.
 */
typedef enum {
    H5R_BADTYPE     =   (-1),   /* Invalid reference type           */
    H5R_OBJECT1     =     0,    /* Backward compatibility (object)  */
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
//! [H5R_ref_t_snip]

typedef struct {
    union {
        uint8_t __data[H5R_REF_BUF_SIZE];       /* opaque data */
        int64_t align;                          /* ensures alignment */
    } u;
} H5R_ref_t;

//! [H5R_ref_t_snip]

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
 *-------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Creates an object reference
 *
 * \loc_id
 * \param[in] name Name of object
 * \param[in] oapl_id Valid object access property list identifier
 * \param[out] ref_ptr Pointer to reference
 *
 * \return \herr_t
 *
 * \details H5Rcreate_obj() creates a reference pointing to the object 
 *          named \p name located at \p loc_id. The parameters \p loc_id 
 *          and \p name are used to locate the object.
 *
 *          The parameter \p oapl_id is an object access property list
 *          identifier for the referenced object. The access property
 *          list must be of the same type as the object being referenced,
 *          that is a group, dataset or committed datatype property list.
 *
 *          The parameter \p ref_ptr is a pointer of type 
 *          H5R_ref_t, which is defined in H5Rpublic.h 
 *          as follows:   
 *          \snippet this H5R_ref_t_snip
 *
 * \par Example
 *      An example snippet from examples/h5_ref_extern.c:
 *      \snippet h5_ref_extern.c H5Rcreate_object_snip
 *
 * \todo Check H5R_ref_t is different from portal; 
 *             There is a H5R_ref_t_snip below declaration line??
 *
 * \since 1.12.0 
 *
 */
H5_DLL herr_t   H5Rcreate_object(hid_t loc_id, const char *name, hid_t oapl_id, H5R_ref_t *ref_ptr);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Creates a region reference
 *
 * \loc_id
 * \param[in] name Name of object
 * \param[in] space_id Dataspace identifier
 * \param[in] oapl_id Valid object access property list identifier
 * \param[out] ref_ptr Pointer to reference
 *
 * \return \herr_t
 *
 * \details H5Rcreate_region() creates the reference, \p ref_ptr, 
 *          pointing to the region represented by \p space_id 
 *          within the object named \p name located at \p loc_id.
 *
 *          The parameters \p loc_id and \p name are used to locate the
 *          object. The parameter \p space_id identifies the dataset region
 *          that a dataset region reference points to.
 *
 *          The parameter \p oapl_id is an object access property list
 *          identifier for the referenced object. The access property
 *          list must be of the same type as the object being referenced,
 *          that is a dataset property list in this case.
 *
 *          The parameter \p ref_ptr is a pointer of type 
 *          H5R_ref_t, which is defined in H5Rpublic.h 
 *          as follows:   
 *          \snippet this H5R_ref_t_snip
 *
 * \since 1.12.0 
 *
 */
H5_DLL herr_t   H5Rcreate_region(hid_t loc_id, const char *name, hid_t space_id, hid_t oapl_id, H5R_ref_t *ref_ptr);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Creates an attribute reference
 *
 * \loc_id
 * \param[in] name Name of object
 * \param[in] attr_name Name of attribute
 * \param[in] oapl_id Valid object access property list identifier
 * \param[out] ref_ptr Pointer to reference
 *
 * \return \herr_t
 *
 * \details H5Rcreate_attr() creates the reference, \p ref_ptr, 
 *          pointing to the attribute named \p attr_name and attached 
 *          to the object named \p name located at \p loc_id.
 *
 *          The parameters \p loc_id and \p name are used to locate
 *          the object. The parameter \p attr_name is used to locate 
 *          the attribute within the object.
 *
 *          The parameter \p oapl_id is an object access property list
 *          identifier for the object that the referenced attribute is
 *          attached to. The access property list must be of the same type
 *          as that object, that is a group, dataset or committed datatype
 *          property list.
 *
 *          The parameter \p ref_ptr is a pointer of type 
 *          H5R_ref_t, which is defined in H5Rpublic.h 
 *          as follows:   
 *          \snippet this H5R_ref_t_snip
 *
 * \since 1.12.0 
 *
 */
H5_DLL herr_t   H5Rcreate_attr(hid_t loc_id, const char *name, const char *attr_name, hid_t oapl_id, H5R_ref_t *ref_ptr);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Closes a reference
 *
 * \param[in] ref_ptr Pointer to reference
 *
 * \return \herr_t
 *
 * \details Given a reference, \p ref_ptr, to an object, region or attribute
 *          attached to an object, H5Rdestroy() releases allocated resources
 *          from a previous create call.
 *           
 *          The parameter \p ref_ptr is a pointer of type 
 *          H5R_ref_t, which is defined in H5Rpublic.h 
 *          as follows:   
 *          \snippet this H5R_ref_t_snip
 *
 * \par Example
 *      An example snippet from examples/h5_ref_extern.c:
 *      \snippet h5_ref_extern.c H5Rdestroy_snip
 *
 * \since 1.12.0 
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t   H5Rdestroy(H5R_ref_t *ref_ptr);

/* Info */
/**
 *-------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Retrieves the type of a reference
 *
 * \param[in] ref_ptr Pointer to reference to query
 *
 * \return Returns a valid reference type if successful; otherwise 
 *         returns #H5R_BADTYPE.
 *
 * \details Given a reference, \p ref_ptr, H5Rget_type()
 *          returns the type of the reference.
 *
 *          The parameter \p ref_ptr is a pointer of type 
 *          H5R_ref_t, which is defined in H5Rpublic.h 
 *          as follows:   
 *          \snippet this H5R_ref_t_snip
 *
 *          Valid returned reference types are:
 *           
 *          <table>
 *          <tr>
 *              <th>Reference Type</th>
 *              <th>Description</th>
 *          </tr>
 *          <tr>
 *              <td>#H5R_OBJECT2</td>
 *              <td>Object reference version 2</td>
 *          </tr>
 *          <tr>
 *              <td>#H5R_DATASET_REGION2</td>
 *              <td>Region reference version 2</td>
 *          </tr>
 *          <tr>
 *              <td>#H5R_ATTR</td>
 *              <td>Attribute reference</td>
 *          </tr>
 *          </table>
 *
 *          Note that #H5R_OBJECT1 and #H5R_DATASET_REGION1 can never be
 *          associated to an H5R_ref_t reference and can therefore never
 *          be returned through that function.
 *
 * \todo Check Correction H5R_ATTRIBUTE to H5R_ATTR
 * \todo Check Correction H5R_UNKNOWN to H5R_BADTYPE
 *
 * \since 1.12.0 
 *
 */
H5_DLL H5R_type_t   H5Rget_type(const H5R_ref_t *ref_ptr);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Determines whether two references are equal
 *
 * \param[in] ref1_ptr Pointer to reference to compare
 * \param[in] ref2_ptr Pointer to reference to compare
 *
 * \return Returns a positive value if the references are equal. 
 * \return Returns 0 if the references are not equal. 
 * \return Returns a negative value when the function fails.
 *
 * \details H5Requal() determines whether two references point to the 
 *          same object, region or attribute.
 *
 *          H5R_ref_t is a \c struct defined in H5Rpublic.h 
 *          as follows:   
 *          \snippet this H5R_ref_t_snip
 *
 * \since 1.12.0 
 *
 *-------------------------------------------------------------------------
 */
H5_DLL htri_t   H5Requal(const H5R_ref_t *ref1_ptr, const H5R_ref_t *ref2_ptr);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Copies an existing reference
 *
 * \param[in] src_ref_ptr Pointer to reference to copy
 * \param[out] dst_ref_ptr Pointer to output reference
 *
 * \return \herr_t
 *
 * \details H5Rcopy() creates a copy of an existing reference. 
 *          \p src_ref_ptr points to the reference to copy and 
 *          \p dst_ref_ptr is the pointer to the destination reference.
 *
 *          H5R_ref_t is defined in H5Rpublic.h as follows:   
 *          \snippet this H5R_ref_t_snip
 *
 * \todo Check H5R_ref_t is different from portal; 
 *             Move description of parameter H5R_ref_t to detail;
 *             The H5R_ref_t_snip is strange ????union...
 *
 * \since 1.12.0 
 *
 */
H5_DLL herr_t   H5Rcopy(const H5R_ref_t *src_ref_ptr, H5R_ref_t *dst_ref_ptr);

/* Dereference */
H5_DLL hid_t    H5Ropen_object(H5R_ref_t *ref_ptr, hid_t rapl_id, hid_t oapl_id);
H5_DLL hid_t    H5Ropen_region(H5R_ref_t *ref_ptr, hid_t rapl_id, hid_t oapl_id);
H5_DLL hid_t    H5Ropen_attr(H5R_ref_t *ref_ptr, hid_t rapl_id, hid_t aapl_id);

/* Get type */
/**
 *-------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Retrieves the type of object that an object reference points to
 *
 * \param[in] ref_ptr Pointer to reference to query
 * \param[in] rapl_id Valid reference access property list identifier
 * \param[out] obj_type Type of referenced object
 *
 * \return \herr_t
 *
 * \details Given a reference, \p ref_ptr, H5Rget_obj_type3() retrieves
 *          the type of the referenced object in \p obj_type.
 *
 *          The parameter \p ref_ptr is a pointer of type 
 *          H5R_ref_t, which is defined in H5Rpublic.h 
 *          as follows:   
 *          \snippet this H5R_ref_t_snip
 *
 *          The parameter \p rapl_id is a reference access property list
 *          identifier for the reference. The access property list can
 *          be used to access external files that the reference points to
 *          (through a file access property list).
 *
 *          The referenced object type, or the type of the referenced
 *          object, is the type of the object that the reference points
 *          to. When the function completes successfully, it returns one
 *          of the following valid object type values
 *          (defined in H5Opublic.h):
 *           
 *          <table>
 *          <tr>
 *              <th>Object Type Value</th>
 *              <th>Description</th>
 *          </tr>
 *          <tr>
 *              <td>#H5O_TYPE_GROUP</td>
 *              <td>Object is a group</td>
 *          </tr>
 *          <tr>
 *              <td>#H5O_TYPE_DATASET</td>
 *              <td>Object is a dataset</td>
 *          </tr>
 *          <tr>
 *              <td>#H5O_TYPE_NAMED_DATATYPE</td>
 *              <td>Object is a named datatype</td>
 *          </tr>
 *          </table>
 *
 * \since 1.12.0 
 *
 */
H5_DLL herr_t   H5Rget_obj_type3(H5R_ref_t *ref_ptr, hid_t rapl_id, H5O_type_t *obj_type);

/* Get name */
/**
 *-------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Retrieves the file name for a referenced object
 *
 * \param[in] ref_ptr Pointer to reference to query
 * \param[in,out] buf A buffer to place the file name of the reference
 * \param[in] size The size of the name buffer
 *
 * \return Returns the length of the name if successful. 
 *         Otherwise returns a negative value.
 *
 * \details H5Rget_file_name() retrieves the file name for the object, 
 *          region or attribute reference pointed to by \p ref_ptr. 
 *          Up to \p size characters of the name are returned in 
 *          \p buf; additional characters, if any,
 *          are not returned to the user application. 
 *          If the length of the name, which determines the required 
 *          value of \p size, is unknown, a preliminary 
 *          H5Rget_file_name() call can be made. The return value
 *          of this call will be the size of the file name. That value 
 *          can then be assigned to \p size for a second 
 *          H5Rget_file_name() call, which will retrieve the actual name.
 *
 *          The parameter \p ref_ptr is a pointer of type 
 *          H5R_ref_t, which is defined in H5Rpublic.h 
 *          as follows:   
 *          \snippet this H5R_ref_t_snip
 *
 * \todo Check parameter buffer v.s. name
 *
 * \since 1.12.0 
 *
 *-------------------------------------------------------------------------
 */
H5_DLL ssize_t  H5Rget_file_name(const H5R_ref_t *ref_ptr, char *buf, size_t size);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Retrieves the object name for a referenced object
 *
 * \param[in] ref_ptr Pointer to reference to query
 * \param[in] rapl_id Valid reference access property list identifier
 * \param[in,out] buf A buffer to place the object name of the reference
 * \param[in] size The size of the name buffer
 *
 * \return Returns the length of the name if successful, returning 
 *         \c 0 (zero) if no name is associated with the identifier. 
 *         Otherwise returns a negative value.
 *
 * \details H5Rget_obj_name() retrieves the object name for the object, 
 *          region or attribute reference pointed to by ref_ptr.
 *          The parameter \p ref_ptr is a pointer of type 
 *          H5R_ref_t, which is defined in H5Rpublic.h 
 *          as follows:   
 *          \snippet this H5R_ref_t_snip
 *
 *          The parameter \p rapl_id is a reference access property list
 *          identifier for the reference. The access property list can
 *          be used to access external files that the reference points to
 *          (through a file access property list).
 *
 *          Up to \p size characters of the name are returned 
 *          in \p buf; additional characters, if any, are not returned 
 *          to the user application. If the length of the name, which 
 *          determines the required value of \p size, is unknown, 
 *          a preliminary H5Rget_obj_name() call can be made. 
 *          The return value of this call will be the size of the 
 *          object name. That value can then be assigned to \p size for a 
 *          second H5Rget_obj_name() call, which will retrieve the actual 
 *          name. If there is no name associated with the object identifier 
 *          or if the name is \c NULL, H5Rget_obj_name() returns the size of 
 *          the name buffer (the size does not include the 
 *          \c NULL terminator).
 *
 *          If \p ref_ptr is an object reference, \p buf will be returned 
 *          with a name for the referenced object. If \p ref_ptr is a 
 *          dataset region reference, \p buf will contain a name for the 
 *          object containing the referenced region. If \p ref_ptr is an 
 *          attribute reference, \p buf will contain a name for the object 
 *          the attribute is attached to. Note that an object in an HDF5 
 *          file may have multiple paths if there are multiple links 
 *          pointing to it. This function may return any one of these paths.
 *
 * \todo Check the parameter buffer v.s. name
 *
 * \since 1.12.0 
 *
 */
H5_DLL ssize_t  H5Rget_obj_name(H5R_ref_t *ref_ptr, hid_t rapl_id, char *buf, size_t size);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Retrieves the attribute name for a referenced object
 *
 * \param[in] ref_ptr Pointer to reference to query
 * \param[in,out] buf A buffer to place the attribute name of the reference
 * \param[in] size The size of the name buffer
 *
 * \return Returns the length of the name if successful.
 *         Otherwise returns a negative value.
 *
 * \details H5Rget_attr_name() retrieves the attribute name for the attribute reference
 *          pointed to by \p ref_ptr. Up to \p size characters of the name
 *          are returned in \p buf; additional characters, if any, are not
 *          returned to the user application. If the length of the name,
 *          which determines the required value of \p size, is unknown, a
 *          preliminary H5Rget_attr_name() call can be made. The return value
 *          of this call will be the size of the attribute name. That value
 *          can then be assigned to \p size for a second H5Rget_attr_name()
 *          call, which will retrieve the actual name.
 *
 *          The parameter \p ref_ptr is a pointer of type 
 *          H5R_ref_t, which is defined in H5Rpublic.h 
 *          as follows:   
 *          \snippet this H5R_ref_t_snip
 *
 * \todo Check the parameter buffer v.s. name
 *
 * \since 1.12.0 
 *
 *-------------------------------------------------------------------------
 */
H5_DLL ssize_t  H5Rget_attr_name(const H5R_ref_t *ref_ptr, char *buf, size_t size);

/* Symbols defined for compatibility with previous versions of the HDF5 API.
 *
 * Use of these symbols is or will be deprecated.
 */

/* Macros */

/* Versions for compatibility */
#define H5R_OBJECT          H5R_OBJECT1
#define H5R_DATASET_REGION  H5R_DATASET_REGION1

/* Function prototypes */
#ifndef H5_NO_DEPRECATED_SYMBOLS

H5_DLL H5G_obj_t H5Rget_obj_type1(hid_t id, H5R_type_t ref_type, const void *ref);
H5_DLL hid_t H5Rdereference1(hid_t obj_id, H5R_type_t ref_type, const void *ref);

#endif /* H5_NO_DEPRECATED_SYMBOLS */

/**
 *-------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Creates a reference
 *
 * \param[out] ref Reference created by the function call
 * \param[in] loc_id Location identifier used to locate the 
 *                   object being pointed to
 * \param[in] name Name of object at location \p loc_id
 * \param[in] ref_type Type of reference
 * \param[in] space_id Dataspace identifier with selection
 *
 * \return \herr_t
 *
 * \details H5Rcreate() creates the reference, \p ref, of the type 
 *          specified \p in ref_type, pointing to the object name 
 *          located at \p loc_id.
 *
 *          The HDF5 library maps the void type specified above for 
 *          \p ref to the type specified in \p ref_type, which will be 
 *          one of those appearing in the first column of the following 
 *          table. The second column of the table lists the HDF5 
 *          constant associated with each reference type.
 *
 *          <table>
 *          <tr>
 *              <td>hdset_reg_ref_t</td>
 *              <td>#H5R_DATASET_REGION</td>
 *              <td>Dataset region reference</td>
 *          </tr>
 *          <tr>
 *              <td>#hobj_ref_t</td>
 *              <td>#H5R_OBJECT</td>
 *              <td>Object reference</td>
 *          </tr>
 *          </table>
 *           
 *          The parameters \p loc_id and \p name are used to locate the object.
 *
 *          The parameter \p space_id identifies the dataset region that a
 *          dataset region reference points to. This parameter is used
 *          only with dataset region references and should be set to -1
 *          if the reference is an object reference, #H5R_OBJECT.
 *          Passing \c -1 as \p space_id causes this parameter to be
 *          ignored. Other values besides valid dataspaces result in an error.
 *
 * \par Example
 *      An example snippet from examples/h5_reference_deprec.c:
 *      \snippet h5_reference_deprec.c H5Rcreate_snip
 *
 * \todo Check couldn't find the exact example as in portal;
 *       Move part of parameter description for space_id to detail
 * 
 * \version 1.8.8 Fortran updated to Fortran2003.
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Rcreate(void *ref, hid_t loc_id, const char *name, H5R_type_t ref_type, hid_t space_id);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Retrieves the type of object that an object reference points to
 *
 * \param[in] id The dataset containing the reference object or the 
 *               group containing that dataset
 * \param[in] ref_type Type of reference to query
 * \param[in] ref Reference to query
 * \param[out] obj_type Type of referenced object
 *
 * \return \herr_t
 *
 * \details Given a reference, \p ref, H5Rget_obj_type2() retrieves
 *          the type of the referenced object in \p obj_type.
 *
 *          A <em>reference type</em> is the type of reference, 
 *          either an object reference or a dataset region reference. 
 *          An <em>object reference</em> points to an HDF5 object while 
 *          a <em>dataset region reference</em> points to a defined 
 *          region within a dataset.
 *
 *          The <em>referenced object</em> is the object the reference 
 *          points to. The <em>referenced object type</em>, or the type 
 *          of the referenced object, is the type of the object that the 
 *          reference points to.
 *
 *          The location identifier, \p id, is the identifier for 
 *          either the dataset containing the object reference or the 
 *          group containing that dataset.
 *
 *          Valid reference types, to pass in as \p ref_type, include 
 *          the following:
 *
 *          <table>
 *          <tr>
 *              <td>#H5R_OBJECT</td>
 *              <td>Object reference</td>
 *          </tr>
 *          <tr>
 *              <td>#H5R_DATASET_REGION</td>
 *              <td>Dataset region reference</td>
 *          </tr>
 *          </table>
 *
 *          If the application does not already know the object reference
 *          type, that can be determined with three preliminary calls:
 *          - Call H5Dget_type() on the dataset containing the reference 
 *            to get a datatype identifier for the dataset’s datatype.
 *          - Using that datatype identifier, H5Tget_class() returns a 
 *            datatype class.
 *          - If the datatype class is #H5T_REFERENCE, H5Tequal() can 
 *            then be used to determine whether the reference’s datatype 
 *            is #H5T_STD_REF_OBJ or #H5T_STD_REF_DSETREG:
 *             - If the datatype is #H5T_STD_REF_OBJ, the reference object 
 *               type is #H5R_OBJECT.
 *             - If the datatype is #H5T_STD_REF_DSETREG, the reference 
 *               object type is #H5R_DATASET_REGION.
 *             .
 *          .
 *
 *          When the function completes successfully, it returns one of 
 *          the following valid object type values (defined in H5Opublic.h):
 *
 *          <table>
 *          <tr>
 *              <td>#H5O_TYPE_GROUP</td>
 *              <td>Object is a group</td>
 *          </tr>
 *          <tr>
 *              <td>#H5O_TYPE_DATASET</td>
 *              <td>Object is a dataset</td>
 *          </tr>
 *          <tr>
 *              <td>#H5O_TYPE_NAMED_DATATYPE</td>
 *              <td>Object is a named datatype</td>
 *          </tr>
 *          </table>
 *
 * \par Example
 *      An example snippet from examples/h5_reference_deprec.c:
 *      \snippet h5_reference_deprec.c H5Rget_obj_type2_snip
 *
 * \todo Couldn't find the example as on portal
 *
 * \since 1.8.0 
 *
 */
H5_DLL herr_t H5Rget_obj_type2(hid_t id, H5R_type_t ref_type, const void *ref, H5O_type_t *obj_type);

H5_DLL hid_t H5Rdereference2(hid_t obj_id, hid_t oapl_id, H5R_type_t ref_type, const void *ref);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Sets up a dataspace and selection as specified by a region reference
 *
 * \param[in] id File identifier or identifier for any object 
 *               in the file containing the referenced region
 * \param[in] ref_type Reference type of \p ref, which must be 
 *                     #H5R_DATASET_REGION
 * \param[in] ref Region reference to open
 *
 * \return Returns a valid dataspace identifier if successful; 
 *         otherwise returns a negative value.
 *
 * \details H5Rget_region() creates a copy of the dataspace of the dataset pointed to by
 *          a region reference, \p ref, and defines a selection matching the
 *          selection pointed to by \p ref within the dataspace copy.
 *
 *          \p id is used to identify the file containing the referenced 
 *          region; it can be a file identifier or an identifier for any object in the file.
 *
 *          The parameter \p ref_type specifies the reference type of \p ref 
 *          and must contain the following value:
 *          - #H5R_DATASET_REGION (1)
 *          .
 *
 *          Use H5Sclose() to release the dataspace identifier returned by 
 *          this function when the identifier is no longer needed. 
 *
 * \par Example
 *      An example snippet from examples/h5_ref2reg_deprec.c:
 *      \snippet h5_ref2reg_deprec.c H5Rget_region_snip
 *
 * \todo Check the example is a bit different from portal
 * \todo Change the "dataset" to "id" in the declaration
 *
 */
H5_DLL hid_t H5Rget_region(hid_t id, H5R_type_t ref_type, const void *ref);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Retrieves a name for a referenced object
 *
 * \param[in] loc_id Identifier for the file containing the reference or 
 *                   for any object in that file
 * \param[in] ref_type Type of reference
 * \param[in] ref An object or dataset region reference
 * \param[out] name A buffer to place the name of the referenced object or 
 *                  dataset region. If \c NULL, then this call will return
 *                  the size in bytes of the name.
 * \param[in] size The size of the name buffer. When the size is passed in, 
 *                 the \c NULL terminator needs to be included.
 *
 * \return Returns the length of the name if successful, returning 
 *         \c 0 (zero) if no name is associated with the identifier. 
 *         Otherwise returns a negative value.
 *
 * \details H5Rget_name() retrieves a name for the object identified 
 *          by \p ref.
 *
 *          \p loc_id is used to identify the file containing the 
 *          reference. It can be the file identifier for the file 
 *          containing the reference or an identifier for any object 
 *          in that file.
 *
 *          #H5R_type_t is the reference type of \p ref. 
 *          Valid values include the following:
 *
 *          <table>
 *          <tr>
 *              <td>#H5R_OBJECT</td>
 *              <td>Object reference</td>
 *          </tr>
 *          <tr>
 *              <td>#H5R_DATASET_REGION</td>
 *              <td>Dataset region reference</td>
 *          </tr>
 *          </table>
 *
 *          \p ref is the reference for which the target object’s name
 *          is sought.
 *
 *          If \p ref is an object reference, \p name will be returned with a
 *          name for the referenced object. If \p ref is a dataset region
 *          reference, \p name will contain a name for the object containing
 *          the referenced region.
 *
 *          Up to \p size characters of the name are returned in \p name;
 *          additional characters, if any, are not returned to the user
 *          application.
 *
 *          If the length of the name, which determines the required value
 *          of \p size, is unknown, a preliminary H5Rget_name() call can be
 *          made. The return value of this call will be the size of the
 *          object name. That value can then be assigned to \p size for a
 *          second H5Rget_name() call, which will retrieve the actual name.
 *
 *          If there is no name associated with the object identifier or
 *          if the \p name is \c NULL, H5Rget_name() returns the size of the
 *          name buffer (the size does not include the \c NULL terminator).
 *
 *          Note that an object in an HDF5 file may have multiple paths
 *          if there are multiple links pointing to it. This function may
 *          return any one of these paths.
 *
 * \par Example
 *      An example snippet from examples/h5_ref2reg_deprec.c:
 *      \snippet h5_ref2reg_deprec.c H5Rget_name_snip
 *
 * \version 1.8.8 Fortran updated to Fortran2003.
 *
 * \since 1.8.0
 *
 */
H5_DLL ssize_t H5Rget_name(hid_t loc_id, H5R_type_t ref_type, const void *ref, char *name, size_t size);

#ifdef __cplusplus
}
#endif

#endif  /* _H5Rpublic_H */
