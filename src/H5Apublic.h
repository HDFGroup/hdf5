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
 * This file contains public declarations for the H5A module.
 */
#ifndef H5Apublic_H
#define H5Apublic_H

/* Public headers needed by this file */
#include "H5Ipublic.h" /* IDs			  		*/
#include "H5Opublic.h" /* Object Headers			*/
#include "H5Tpublic.h" /* Datatypes				*/

/* Information struct for attribute (for H5Aget_info/H5Aget_info_by_idx) */
//! [H5A_info_t_snip]
typedef struct {
    hbool_t           corder_valid; /* Indicate if creation order is valid */
    H5O_msg_crt_idx_t corder;       /* Creation order                 */
    H5T_cset_t        cset;         /* Character set of attribute name */
    hsize_t           data_size;    /* Size of raw data		  */
} H5A_info_t;
//! [H5A_info_t_snip]

/* Typedef for H5Aiterate2() callbacks */
typedef herr_t (*H5A_operator2_t)(hid_t location_id /*in*/, const char *attr_name /*in*/,
                                  const H5A_info_t *ainfo /*in*/, void *op_data /*in,out*/);

/********************/
/* Public Variables */
/********************/

/*********************/
/* Public Prototypes */
/*********************/
#ifdef __cplusplus
extern "C" {
#endif

/*-------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Closes the specified attribute
 *
 * \attr_id
 *
 * \return \herr_t
 *
 * \details H5Aclose() terminates access to the attribute specified by
 *          \p attr_id by releasing the identifier.
 *
 * \attention Further use of a released attribute identifier is illegal; a
 *            function using such an identifier will generate an error.
 *
 * \since 1.0.0
 *
 * \see H5Acreate(), H5Aopen()
 */
H5_DLL herr_t H5Aclose(hid_t attr_id);
/* --------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Creates an attribute attached to a specified object
 *
 * \fgdt_loc_id
 * \param[in] attr_name  Name of attribute
 * \param[in] type_id    Attribute datatype identifier
 * \space_id
 * \acpl_id
 * \aapl_id
 *
 * \return \hid_tv{attribute}
 *
 * \details H5Acreate2() creates an attribute, \p attr_name, which is attached
 *          to the object specified by the identifier \p loc_id.
 *
 *          The attribute name, \p attr_name, must be unique for the object.
 *
 *          The attribute is created with the specified datatype and dataspace,
 *          \p type_id and \p space_id, which are created with the H5T and
 *          H5S interfaces, respectively.
 *
 *          If \p type_id is either a fixed-length or variable-length string,
 *          it is important to set the string length when defining the
 *          datatype. String datatypes are derived from #H5T_C_S1 (or
 *          #H5T_FORTRAN_S1 for Fortran), which defaults to 1 character in
 *          size. See H5Tset_size() and Creating variable-length string
 *          datatypes.
 *
 *          The access property list is currently unused, but will be used in
 *          the future. This property list should currently be #H5P_DEFAULT.
 *
 *          The attribute identifier returned by this function must be released
 *          with H5Aclose() resource leaks will develop.
 *
 * \note The \p acpl and \p aapl parameters are currently not used; specify
 *       #H5P_DEFAULT.
 * \note If \p loc_id is a file identifier, the attribute will be attached
 *       that file’s root group.
 *
 * \since 1.8.0
 *
 * \see H5Aclose()
 *
 */
H5_DLL hid_t H5Acreate2(hid_t loc_id, const char *attr_name, hid_t type_id, hid_t space_id, hid_t acpl_id,
                        hid_t aapl_id);
H5_DLL hid_t H5Acreate_async(const char *app_file, const char *app_func, unsigned app_line, hid_t loc_id,
                             const char *attr_name, hid_t type_id, hid_t space_id, hid_t acpl_id,
                             hid_t aapl_id, hid_t es_id);
/*--------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief  Creates an attribute attached to a specified object
 *
 * \fgdt_loc_id
 * \param[in] obj_name  Name, relative to \p loc_id, of object that
 *                      attribute is to be attached to
 * \param[in] attr_name Attribute name
 * \param[in] type_id   Attribute datatype identifier
 * \space_id
 * \acpl_id
 * \aapl_id
 * \lapl_id
 *
 * \return \hid_tv{attribute}
 *
 * \details H5Acreate_by_name() creates an attribute, \p attr_name, which is
 *          attached to the object specified by \p loc_id and \p obj_name.
 *
 *          \p loc_id is a location identifier; \p obj_name is the object
 *          name relative to \p loc_id. If \p loc_id fully specifies the
 *          object to which the attribute is to be attached, \p obj_name
 *          should be '.' (a dot).
 *
 *          The attribute name, \p attr_name, must be unique for the object.
 *
 *          The attribute is created with the specified datatype and
 *          dataspace, \p type_id and \p space_id, which are created with
 *          the H5T and H5S interfaces respectively.
 *
 *          The attribute creation and access property lists are currently
 *          unused, but will be used in the future for optional attribute
 *          creation and access properties. These property lists should
 *          currently be #H5P_DEFAULT.
 *
 *          The link access property list, \p lapl_id, may provide
 *          information regarding the properties of links required to access
 *          the object, \p obj_name.
 *
 *          The attribute identifier returned by this function must be
 *          released with H5close() or resource leaks will develop.
 *
 * \since 1.8.0
 *
 */
H5_DLL hid_t H5Acreate_by_name(hid_t loc_id, const char *obj_name, const char *attr_name, hid_t type_id,
                               hid_t space_id, hid_t acpl_id, hid_t aapl_id, hid_t lapl_id);
/*-------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Deletes an attribute from a specified location
 *
 * \fgdt_loc_id
 * \param[in] attr_name  Name of the attribute to delete
 *
 * \return \herr_t
 *
 * \details H5Adelete() removes the attribute specified by its name,
 *          \p attr_name, from a file, dataset, group, or named datatype.
 *          This function should not be used when attribute identifiers
 *          are open on \p loc_id as it may cause the internal indexes of
 *          the attributes to change and future writes to the open
 *          attributes to produce incorrect results.
 *
 * \since 1.0.0
 *
 */
H5_DLL herr_t H5Adelete(hid_t loc_id, const char *attr_name);
/*-------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Deletes an attribute from an object according to index order
 *
 * \fgdt_loc_id
 * \param[in] obj_name Name of object, relative to location, from which
 *                     attribute is to be removed
 * \param[in] idx_type Type of index
 * \param[in] order    Order in which to iterate over index
 * \param[in] n        Offset within index
 * \lapl_id
 *
 * \return \herr_t
 *
 * \details H5Adelete_by_idx() removes an attribute, specified by its
 *          location in an index, from an object.
 *
 *          The object from which the attribute is to be removed is
 *          specified by a location identifier and name, \p loc_id and
 *          \p obj_name, respectively. If \p loc_id fully specifies the
 *          object from which the attribute is to be removed, \p obj_name
 *          should be '.' (a dot).
 *
 *          The attribute to be removed is specified by a position in an
 *          index, \p n. The type of index is specified by \p idx_type and
 *          may be #H5_INDEX_NAME, for an alpha-numeric index by name, or
 *          #H5_INDEX_CRT_ORDER, for an index by creation order. The order
 *          in which the index is to be traversed is specified by \p order
 *          and may be #H5_ITER_INC (increment) for top-down iteration,
 *          #H5_ITER_DEC (decrement) for bottom-up iteration, or
 *          #H5_ITER_NATIVE, in which case HDF5 will iterate in the
 *          fastest-available order. For example, if \p idx_type, \p order,
 *          and \p n are set to #H5_INDEX_NAME, #H5_ITER_INC, and 5,
 *          respectively, the fifth attribute by alpha-numeric order of
 *          attribute names will be removed.
 *
 *          For a discussion of \p idx_type and \p order, the valid values
 *          of those parameters, and the use of \p n, see the description
 *          of H5Aiterate2().
 *
 *          The link access property list, \p lapl_id, may provide
 *          information regarding the properties of links required to access
 *          the object, \p obj_name.

 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Adelete_by_idx(hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order,
                               hsize_t n, hid_t lapl_id);
/*-------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Removes an attribute from a specified location
 *
 * \fgdt_loc_id
 * \param[in] obj_name  Name of object, relative to location, from which
 *                      attribute is to be removed
 * \param[in] attr_name Name of attribute to delete
 * \lapl_id
 *
 * \return \herr_t
 *
 * \details H5Adelete_by_name() removes the attribute \p attr_name
 *          from an object specified by location and name, \p loc_id and
 *          \p obj_name, respectively.
 *
 *          If \p loc_id fully specifies the object from which the
 *          attribute is to be removed, \p obj_name should be '.' (a dot).
 *
 *          The link access property list, \p lapl_id, may provide
 *          information regarding the properties of links required to
 *          access the object, \p obj_name.
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Adelete_by_name(hid_t loc_id, const char *obj_name, const char *attr_name, hid_t lapl_id);
/*-------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Determines whether an attribute with a given name exists on an
 *        object
 *
 * \fgdt_loc_id{obj_id}
 * \param[in] attr_name  Attribute name
 *
 * \return \htri_t
 *
 * \details H5Aexists() determines whether the attribute \p attr_name
 *          exists on the object specified by \p obj_id.
 *
 * \since 1.8.0
 *
 */
H5_DLL htri_t H5Aexists(hid_t obj_id, const char *attr_name);
/*-------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief  Determines whether an attribute with a given name exists on an
 *         object
 *
 * \fgdt_loc_id{obj_id}
 * \param[in] obj_name  Object name
 * \param[in] attr_name Attribute name
 * \lapl_id
 *
 * \return \htri_t
 *
 * \details H5Aexists_by_name() determines whether the attribute
 *          \p attr_name exists on an object. That object is specified by
 *          its location and name, \p loc_id and \p obj_name, respectively.
 *
 *          \p loc_id specifies a location in the file containing the object.
 *          \p obj_name is the name of the object to which the attribute is
 *          attached and can be a relative name, relative to \p loc_id,
 *          or an absolute name, based in the root group of the file. If
 *          \p loc_id fully specifies the object, \p obj_name should be '.'
 *          (a dot).
 *
 *          The link access property list, \p lapl_id, may provide
 *          information regarding the properties of links required to access
 *          \p obj_name.
 *
 * \since 1.8.0
 *
 */
H5_DLL htri_t H5Aexists_by_name(hid_t obj_id, const char *obj_name, const char *attr_name, hid_t lapl_id);
/*-------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Gets an attribute creation property list identifier
 *
 * \attr_id
 *
 * \return \hid_tv{attribute's creation property list}
 *
 * \details H5Aget_create_plist() returns an identifier for the attribute
 *          creation property list associated with the attribute specified
 *          by \p attr_id.
 *
 *          The creation property list identifier should be released with
 *          H5Pclose().
 *
 * \since 1.8.0
 *
 */
H5_DLL hid_t H5Aget_create_plist(hid_t attr_id);
/*-------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Retrieves attribute information, by attribute identifier
 *
 * \attr_id
 * \param[out]  ainfo   Attribute information struct
 *
 * \return \herr_t
 *
 * \details H5Aget_info() retrieves attribute information, locating the
 *          attribute with an attribute identifier, \p attr_id, which is
 *          the identifier returned by H5Aopen() or H5Aopen_by_idx(). The
 *          attribute information is returned in the \p ainfo struct.
 *
 *          The \p ainfo struct is defined as follows:
 *          \snippet this H5A_info_t_snip
 *
 *          \p corder_valid indicates whether the creation order data is
 *          valid for this attribute. Note that if creation order is not
 *          being tracked, no creation order data will be valid. Valid
 *          values are \c TRUE and \c FALSE.
 *
 *          \p corder is a positive integer containing the creation
 *          order of the attribute. This value is 0-based, so, for
 *          example, the third attribute created will have a \p corder
 *          value of 2.
 *
 *          \p cset indicates the character set used for the attribute’s
 *          name; valid values are defined in H5Tpublic.h and include
 *          the following:
 *          \csets
 *          This value is set with H5Pset_char_encoding().
 *
 *          \p data_size indicates the size, in the number of characters,
 *          of the attribute.
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Aget_info(hid_t attr_id, H5A_info_t *ainfo /*out*/);
/*-------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Retrieves attribute information by attribute index position
 *
 * \fgdt_loc_id
 * \param[in]  obj_name  Name of object to which attribute is attached,
 *                       relative to location
 * \param[in]  idx_type  Type of index
 * \param[in]  order     Index traversal order
 * \param[in]  n         Attribute’s position in index
 * \param[out] ainfo     Struct containing returned attribute information
 * \lapl_id
 *
 * \return \herr_t
 *
 * \details H5Aget_info_by_idx() retrieves information for an attribute
 *          that is attached to an object, which is specified by its
 *          location and name, \p loc_id and \p obj_name, respectively.
 *          The attribute is located by its index position and the attribute
 *          information is returned in the \p ainfo struct.
 *
 *          If \p loc_id fully specifies the object to which the attribute
 *          is attached, \p obj_name should be '.' (a dot).
 *
 *          The attribute is located by means of an index type, an index
 *          traversal order, and a position in the index, \p idx_type,
 *          \p order and \p n, respectively. These parameters and their
 *          valid values are discussed in the description of H5Aiterate2().
 *
 *          The \p ainfo struct, which will contain the returned attribute
 *          information, is described in H5Aget_info().
 *
 *          The link access property list, \p lapl_id, may provide
 *          information regarding the properties of links required to access
 *          the object, \p obj_name.
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Aget_info_by_idx(hid_t loc_id, const char *obj_name, H5_index_t idx_type,
                                 H5_iter_order_t order, hsize_t n, H5A_info_t *ainfo /*out*/, hid_t lapl_id);
/*-------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Retrieves attribute information, by attribute name
 *
 * \fgdt_loc_id
 *
 * \param[in] obj_name   Name of object to which attribute is attached,
 *                       relative to location
 * \param[in] attr_name  Attribute name
 * \param[out] ainfo     Struct containing returned attribute information
 * \lapl_id
 *
 * \return \herr_t
 *
 * \details H5Aget_info_by_name() retrieves information for an attribute,
 *          \p attr_name, that is attached to an object specified by its
 *          location and name, \p loc_id and \p obj_name, respectively.
 *          The attribute information is returned in the \p ainfo struct.
 *
 *          If \p loc_id fully specifies the object to which the attribute
 *          is attached, \p obj_name should be '.' (a dot).
 *
 *          The \p ainfo struct is described in H5Aget_info().
 *
 *          The link access property list, \p lapl_id, may provide
 *          information regarding the properties of links required to
 *          access the object, \p obj_name.
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Aget_info_by_name(hid_t loc_id, const char *obj_name, const char *attr_name,
                                  H5A_info_t *ainfo /*out*/, hid_t lapl_id);
/*-------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Gets an attribute name
 *
 * \attr_id
 * \param[in]  buf_size  The size of the buffer to store the name in
 * \param[out] buf       Buffer to store name in
 *
 * \return  Returns the length of the attribute's name, which may be longer
 *          than \p buf_size, if successful. Otherwise returns a negative
 *          value.
 *
 * \details H5Aget_name() retrieves the name of an attribute specified by
 *          the identifier, \p attr_id. Up to \p buf_size characters are
 *          stored in \p buf followed by a \0 string terminator. If the
 *          name of the attribute is longer than (\p buf_size -1), the
 *          string terminator is stored in the last position of the buffer
 *          to properly terminate the string.
 *
 *          If the user only wants to find out the size of this name, the
 *          values 0 and NULL can be passed in for the parameters
 *          \p bufsize and \p buf.
 *
 * \since 1.0.0
 *
 */
H5_DLL ssize_t H5Aget_name(hid_t attr_id, size_t buf_size, char *buf);
/*-------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Gets an attribute name, by attribute index position
 *
 * \fgdt_loc_id
 * \param[in]  obj_name   Name of object to which attribute is attached,
 *                        relative to location
 * \param[in]  idx_type   Type of index
 * \param[in]  order      Index traversal order
 * \param[in]  n          Attribute’s position in index
 * \param[out] name       Attribute name
 * \param[in]  size       Size, in bytes, of attribute name
 * \lapl_id
 *
 * \return Returns attribute name size, in bytes, if successful;
 *         otherwise returns a negative value.
 *
 * \details H5Aget_name_by_idx() retrieves the name of an attribute that is
 *          attached to an object, which is specified by its location and
 *          name, \p loc_id and \p obj_name, respectively. The attribute is
 *          located by its index position, the size of the name is specified
 *          in \p size, and the attribute name is returned in \p name.
 *
 *          If \p loc_id fully specifies the object to which the attribute
 *          is attached, \p obj_name should be '.' (a dot).
 *
 *          The attribute is located by means of an index type, an index
 *          traversal order, and a position in the index, \p idx_type,
 *          \p order and \p n, respectively. These parameters and their
 *          valid values are discussed in the description of H5Aiterate2().
 *
 *          If the attribute name’s size is unknown, the values 0 and NULL
 *          can be passed in for the parameters \p size and \p name. The
 *          function’s return value will provide the correct value for
 *          \p size.
 *
 *          The link access property list, \p lapl_id, may provide
 *          information regarding the properties of links required to access
 *          the object, \p obj_name.
 *
 * \since 1.8.0
 *
 */
H5_DLL ssize_t H5Aget_name_by_idx(hid_t loc_id, const char *obj_name, H5_index_t idx_type,
                                  H5_iter_order_t order, hsize_t n, char *name /*out*/, size_t size,
                                  hid_t lapl_id);
/*-------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Gets a copy of the dataspace for an attribute
 *
 * \attr_id
 *
 * \return \hid_tv{attribute dataspace}
 *
 * \details  H5Aget_space() retrieves a copy of the dataspace for an
 *           attribute. The dataspace identifier returned from this
 *           function must be released with H5Sclose() or resource leaks
 *           will develop.
 *
 * \since 1.0.0
 *
 */
H5_DLL hid_t H5Aget_space(hid_t attr_id);
/*-------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Returns the amount of storage required for an attribute
 *
 * \attr_id
 *
 * \return Returns the amount of storage size allocated for the attribute;
 *         otherwise returns 0 (zero).
 *
 * \details H5Aget_storage_size() returns the amount of storage that is
 *          required for the specified attribute, \p attr_id.
 *
 * \since 1.6.0
 *
 */
H5_DLL hsize_t H5Aget_storage_size(hid_t attr_id);
/*-------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Gets an attribute datatype
 *
 * \attr_id
 *
 * \return \hid_t{datatype}
 *
 * \details H5Aget_type() retrieves a copy of the datatype for an attribute.
 *          The datatype is reopened if it is a named type before returning
 *          it to the application. The datatypes returned by this function
 *          are always read-only. If an error occurs when atomizing the
 *          return datatype, then the datatype is closed.
 *
 *          The datatype identifier returned from this function must be
 *          released with H5Tclose() or resource leaks will develop.
 *
 * \since 1.0.0
 *
 */
H5_DLL hid_t H5Aget_type(hid_t attr_id);
/*-------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Calls user-defined function for each attribute on an object
 *
 * \fgdt_loc_id
 * \param[in]     idx_type Type of index
 * \param[in]     order    Order in which to iterate over index
 * \param[in,out] idx      Initial and returned offset within index
 * \param[in]     op       User-defined function to pass each attribute to
 * \param[in,out] op_data  User data to pass through to and to be returned
 *                         by iterator operator function
 *
 * \return \herr_t
 *       Further note that this function returns the return value of the
 *       last operator if it was non-zero, which can be a negative value,
 *       zero if all attributes were processed, or a positive value
 *       indicating short-circuit success.
 *
 * \details H5Aiterate2() iterates over the attributes attached to a
 *          dataset, named datatype, or group, as specified by \p loc_id.
 *          For each attribute, user-provided data, \p op_data, with
 *          additional information as defined below, is passed to a
 *          user-defined function, \p op, which operates on that
 *          attribute.
 *
 *          The order of the iteration and the attributes iterated over
 *          are specified by three parameters: the index type,
 *          \p idx_type; the order in which the index is to be traversed,
 *          \p order; and the attribute’s position in the index, \p idx.
 *
 *          The type of index specified by \p idx_type can be one of the
 *          following:
 *
 *          \indexes
 *
 *          The order in which the index is to be traversed, as specified
 *          by \p order, can be one of the following:
 *
 *          \orders
 *
 *          The next attribute to be operated on is specified by \p idx,
 *          a position in the index.
 *
 *          For example, if \p idx_type, \p order, and \p idx are set to
 *          #H5_INDEX_NAME, #H5_ITER_INC, and 5, respectively, the attribute
 *          in question is the fifth attribute from the beginning of the
 *          alpha-numeric index of attribute names. If \p order were set to
 *          #H5_ITER_DEC, it would be the fifth attribute from the end of
 *          the index.
 *
 *          The parameter \p idx is passed in on an H5Aiterate2() call with
 *          one value and may be returned with another value. The value
 *          passed in identifies the parameter to be operated on first;
 *          the value returned identifies the parameter to be operated on
 *          in the next step of the iteration.
 *
 *          The #H5A_operator2_t prototype for the \p op parameter is a
 *          user defined function where:
 *          The operation receives the location identifier for the group or
 *          dataset being iterated over, \p location_id; the name of the
 *          current object attribute, \p attr_name; the attribute’s info
 *          struct, \p ainfo; and a pointer to the operator data passed
 *          into H5Aiterate2(), \p op_data.
 *
 *          Valid return values from an operator and the resulting
 *          H5Aiterate2() and \p op behavior are as follows:
 *
 *          \li Zero causes the iterator to continue, returning zero when
 *              all attributes have been processed.
 *          \li A positive value causes the iterator to immediately return
 *              that positive value, indicating short-circuit success. The
 *              iterator can be restarted at the next attribute, as
 *              indicated by the return value of \p idx.
 *          \li A negative value causes the iterator to immediately return
 *              that value, indicating failure. The iterator can be
 *              restarted at the next attribute, as indicated by the return
 *              value of \p idx.
 *
 * \note This function is also available through the H5Aiterate() macro.
 * \todo Add snippet for H5A_operator2_t
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Aiterate2(hid_t loc_id, H5_index_t idx_type, H5_iter_order_t order, hsize_t *idx,
                          H5A_operator2_t op, void *op_data);
/*--------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Calls user-defined function for each attribute on an object
 *
 * \fgdt_loc_id
 * \param[in] obj_name    Name of object, relative to location
 * \param[in] idx_type    Type of index
 * \param[in] order       Order in which to iterate over index
 * \param[in,out] idx     Initial and returned offset within index
 * \param[in] op          User-defined function to pass each attribute to
 * \param[in,out] op_data User data to pass through to and to be returned
 *                        by iterator operator function
 * \lapl_id
 *
 * \return \herr_t
 *         Further note that this function returns the return value of
 *         the last operator if it was non-zero, which can be a negative
 *         value, zero if all attributes were processed, or a positive value
 *         indicating short-circuit success.
 *
 * \details H5Aiterate_by_name() iterates over the attributes attached
 *          to the dataset or group specified with \p loc_id and \p obj_name.
 *          For each attribute, user-provided data, \p op_data, with
 *          additional information as defined below, is passed to a
 *          user-defined function, \p op, which operates on that attribute.
 *
 *          If \p loc_id fully specifies the object to which these
 *          attributes are attached, \p obj_name should be '.' (a dot).
 *
 *          The order of the iteration and the attributes iterated over
 *          are specified by three parameters: the index type, \p idx_type;
 *          the order in which the index is to be traversed, \p order;
 *          and the attribute’s position in the index, \p idx.
 *
 *          The type of index specified by \p idx_type can be one of the
 *          following:
 *
 *          \indexes
 *
 *          The order in which the index is to be traversed, as specified
 *          by \p order, can be one of the following:
 *
 *          \orders
 *
 *          The next attribute to be operated on is specified by \p idx,
 *          a position in the index.
 *
 *          For example, if \p idx_type, \p order, and \p idx are set to
 *          #H5_INDEX_NAME, #H5_ITER_INC, and 5, respectively, the attribute
 *          in question is the fifth attribute from the beginning of the
 *          alpha-numeric index of attribute names. If \p order were set to
 *          #H5_ITER_DEC, it would be the fifth attribute from the end of
 *          the index.
 *
 *          The parameter \p idx is passed in on an H5Aiterate_by_name()
 *          call with one value and may be returned with another value. The
 *          value passed in identifies the parameter to be operated on first;
 *          the value returned identifies the parameter to be operated on in
 *          the next step of the iteration.
 *
 *          The #H5A_operator2_t prototype for the \p op parameter is a
 *          user defined function where:
 *          The operation receives the location identifier for the group or
 *          dataset being iterated over, \p location_id; the name of the
 *          current object attribute, \p attr_name; the attribute’s info
 *          struct, \p ainfo; and a pointer to the operator data passed
 *          into H5Aiterate_by_name(), \p op_data.
 *
 *          Valid return values from an operator and the resulting
 *          H5Aiterate_by_name() and \p op behavior are as follows:
 *
 *          \li Zero causes the iterator to continue, returning zero when
 *              all attributes have been processed.
 *          \li A positive value causes the iterator to immediately return
 *              that positive value, indicating short-circuit success.
 *              The iterator can be restarted at the next attribute, as
 *              indicated by the return value of \p idx.
 *          \li A negative value causes the iterator to immediately return
 *              that value, indicating failure. The iterator can be
 *              restarted at the next attribute, as indicated by the return
 *              value of \p idx.
 *
 *          The link access property list, \p lapl_id, may provide
 *          information regarding the properties of links required to access
 *          the object, \p obj_name.
 *
 * \todo Add snippet to show H5Aoperator2_t.
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Aiterate_by_name(hid_t loc_id, const char *obj_name, H5_index_t idx_type,
                                 H5_iter_order_t order, hsize_t *idx, H5A_operator2_t op, void *op_data,
                                 hid_t lapl_id);
H5_DLL hid_t  H5Acreate_by_name_async(const char *app_file, const char *app_func, unsigned app_line,
                                      hid_t loc_id, const char *obj_name, const char *attr_name, hid_t type_id,
                                      hid_t space_id, hid_t acpl_id, hid_t aapl_id, hid_t lapl_id,
                                      hid_t es_id);
/*--------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Opens an attribute for an object specified by object identifier and
 *        attribute name
 *
 * \fgdt_loc_id{obj_id}
 * \param[in]  attr_name    Name of attribute to open
 * \aapl_id
 *
 * \return \hid_tv{attribute}
 *
 * \details H5Aopen() opens an existing attribute, \p attr_name, that is
 *          attached to object specified by an object identifier, \p obj_id.
 *
 *          The attribute access property list, \p aapl_id, is currently unused
 *          and should be #H5P_DEFAULT.
 *
 *          This function, H5Aopen_by_idx() or H5Aopen_by_name() must be called
 *          before the attribute can be accessed for any further purpose,
 *          including reading, writing, or any modification.
 *
 *          The attribute identifier returned by this function must be released
 *          with H5Aclose() or resource leaks will develop.
 *
 * \since 1.8.0
 *
 * \see H5Aclose(), H5Acreate()
 */
H5_DLL hid_t H5Aopen(hid_t obj_id, const char *attr_name, hid_t aapl_id);
H5_DLL hid_t H5Aopen_async(const char *app_file, const char *app_func, unsigned app_line, hid_t obj_id,
                           const char *attr_name, hid_t aapl_id, hid_t es_id);
/*--------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Opens the nth attribute attached to an object
 *
 * \loc_id
 * \param[in] obj_name  Name of object to which attribute is attached,
 *                      relative to location
 * \param[in] idx_type  Type of index
 * \param[in] order     Index traversal order
 * \param[in] n         Attribute’s position in index
 * \aapl_id
 * \lapl_id
 *
 * \return \hid_tv{attribute}
 *
 * \details H5Aopen_by_idx() opens an existing attribute that is attached
 *          to an object specified by location and name, \p loc_id and
 *          \p obj_name, respectively. If \p loc_id fully specifies the
 *          object to which the attribute is attached, \p obj_name, should
 *          be  '.'  (a dot).
 *
 *          The attribute is identified by an index type, an index traversal
 *          order, and a position in the index, \p idx_type, \p order and
 *          \p n, respectively. These parameters and their valid values are
 *          discussed in the description of H5Aiterate2().
 *
 *          The attribute access property list, \p aapl_id, is currently
 *          unused and should currently be #H5P_DEFAULT.
 *
 *          The link access property list, \p lapl_id, may provide
 *          information regarding the properties of links required to access
 *          the object, \p obj_name.
 *
 *          This function, H5Aopen(), or H5Aopen_by_name() must be called
 *          before an attribute can be accessed for any further purpose,
 *          including reading, writing, or any modification.
 *
 *          The attribute identifier returned by this function must be
 *          released with H5Aclose() or resource leaks will develop.
 *
 * \since 1.8.0
 *
 */
H5_DLL hid_t H5Aopen_by_idx(hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order,
                            hsize_t n, hid_t aapl_id, hid_t lapl_id);
H5_DLL hid_t H5Aopen_by_idx_async(const char *app_file, const char *app_func, unsigned app_line, hid_t loc_id,
                                  const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t n,
                                  hid_t aapl_id, hid_t lapl_id, hid_t es_id);
/*--------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Opens an attribute for an object by object name and attribute name
 *
 * \fgdt_loc_id
 * \param[in] obj_name   Name of object to which attribute is attached,
 *                       relative to \p loc_id
 * \param[in] attr_name  Name of attribute to open
 * \aapl_id
 * \lapl_id
 *
 * \return \hid_tv{attribute}
 *
 * \details H5Aopen_by_name() opens an existing attribute, \p attr_name,
 *          that is attached to an object specified by location and name,
 *          \p loc_id and \p obj_name, respectively.
 *
 *          \p loc_id specifies a location from which the target object can
 *          be located and \p obj_name is an object name relative to
 *          \p loc_id. If \p loc_id fully specifies the object to which the
 *          attribute is attached, \p obj_name should be '.' (a dot).
 *
 *          The attribute access property list, \p aapl_id, is currently
 *          unused and should currently be #H5P_DEFAULT.
 *
 *          The link access property list, \p lapl_id, may provide
 *          information regarding the properties of links required to access
 *          the object, \p obj_name.
 *
 *          This function, H5Aopen(), or H5Aopen_by_idx() must be called
 *          before an attribute can be accessed for any further purpose,
 *          including reading, writing, or any modification.
 *
 *          The attribute identifier returned by this function must be
 *          released with H5Aclose() or resource leaks will develop.
 *
 * \since 1.8.0
 *
 */
H5_DLL hid_t H5Aopen_by_name(hid_t loc_id, const char *obj_name, const char *attr_name, hid_t aapl_id,
                             hid_t lapl_id);
H5_DLL hid_t H5Aopen_by_name_async(const char *app_file, const char *app_func, unsigned app_line,
                                   hid_t loc_id, const char *obj_name, const char *attr_name, hid_t aapl_id,
                                   hid_t lapl_id, hid_t es_id);
/*-------------------------------------------------------------------------- */
/**
 * \ingroup H5A
 *
 * \brief Reads the value of an attribute
 *
 * \attr_id
 * \mem_type_id{type_id}
 * \param[out] buf        Buffer for data to be read
 *
 * \return \herr_t
 *
 * \details H5Aread() reads an attribute, specified with \p attr_id. The
 *          attribute's in-memory datatype is specified with \p type_id. The
 *          entire attribute is read into \p buf from the file.
 *
 *          Datatype conversion takes place at the time of a read or write and
 *          is automatic.
 *
 * \version 1.8.8  Fortran updated to Fortran2003.
 * \version 1.4.2  The \p dims parameter was added to the Fortran API in this
 *                 release.
 * \since   1.0.0
 *
 * \see H5Awrite()
 *
 */
H5_DLL herr_t H5Aread(hid_t attr_id, hid_t type_id, void *buf);
/*-------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Renames an attribute
 *
 * \fgdt_loc_id
 * \param[in] old_name   Name of the attribute to be changed
 * \param[in] new_name   New name for the attribute
 *
 * \return \herr_t
 *
 * \details H5Arename() changes the name of the attribute located at
 *          \p loc_id.
 *
 *          The old name, \p old_name, is changed to the new name,
 *          \p new_name.
 *
 * \since 1.6.0
 *
 */
H5_DLL herr_t H5Arename(hid_t loc_id, const char *old_name, const char *new_name);
H5_DLL herr_t H5Aread_async(const char *app_file, const char *app_func, unsigned app_line, hid_t attr_id,
                            hid_t dtype_id, void *buf, hid_t es_id);
/*--------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Writes data to an attribute
 *
 * \attr_id
 * \mem_type_id{type_id}
 * \param[out]  buf       Data to be written
 *
 * \return \herr_t
 *
 * \details H5Awrite() writes an attribute, specified with \p attr_id. The
 *          attribute's in-memory datatype is specified with \p type_id.
 *          The entire attribute is written from \p buf to the file.
 *
 *          If \p type_id is either a fixed-length or variable-length string,
 *          it is important to set the string length when defining the datatype.
 *          String datatypes are derived from #H5T_C_S1 (or #H5T_FORTRAN_S1 for
 *          Fortran codes), which defaults to 1 character in size.
 *          See H5Tset_size() and Creating variable-length string datatypes.
 *
 *          Datatype conversion takes place at the time of a read or write and
 *          is automatic.
 *
 * \version 1.8.8   Fortran updated to Fortran2003.
 * \version 1.4.2   Fortran \p dims parameter added in this release
 * \since 1.0.0
 * \see H5Aread()
 *
 */
H5_DLL herr_t H5Awrite(hid_t attr_id, hid_t type_id, const void *buf);
H5_DLL herr_t H5Awrite_async(const char *app_file, const char *app_func, unsigned app_line, hid_t attr_id,
                             hid_t type_id, const void *buf, hid_t es_id);
H5_DLL herr_t H5Arename_async(const char *app_file, const char *app_func, unsigned app_line, hid_t loc_id,
                              const char *old_name, const char *new_name, hid_t es_id);
H5_DLL herr_t H5Arename_by_name_async(const char *app_file, const char *app_func, unsigned app_line,
                                      hid_t loc_id, const char *obj_name, const char *old_attr_name,
                                      const char *new_attr_name, hid_t lapl_id, hid_t es_id);
H5_DLL herr_t H5Aexists_async(const char *app_file, const char *app_func, unsigned app_line, hid_t obj_id,
                              const char *attr_name, hbool_t *exists, hid_t es_id);
H5_DLL herr_t H5Aexists_by_name_async(const char *app_file, const char *app_func, unsigned app_line,
                                      hid_t loc_id, const char *obj_name, const char *attr_name,
                                      hbool_t *exists, hid_t lapl_id, hid_t es_id);
/*-------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \fgdt_loc_id
 * \param[in] obj_name      Name of object, relative to location, whose
 *                          attribute is to be renamed
 * \param[in] old_attr_name Prior attribute name
 * \param[in] new_attr_name New attribute name
 * \lapl_id
 *
 * \details H5Arename_by_name() changes the name of attribute that is
 *          attached to the object specified by \p loc_id and \p obj_name.
 *          The attribute named \p old_attr_name is renamed
 *          \p new_attr_name.
 *
 *          The link access property list, \p lapl_id, may provide
 *          information regarding the properties of links required to
 *          access the object, \p obj_name.
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Arename_by_name(hid_t loc_id, const char *obj_name, const char *old_attr_name,
                                const char *new_attr_name, hid_t lapl_id);
H5_DLL herr_t H5Aclose_async(const char *app_file, const char *app_func, unsigned app_line, hid_t attr_id,
                             hid_t es_id);

/* API Wrappers for async routines */
/* (Must be defined _after_ the function prototype) */
/* (And must only defined when included in application code, not the library) */
#ifndef H5A_MODULE
#define H5Acreate_async(...)         H5Acreate_async(__FILE__, __func__, __LINE__, __VA_ARGS__)
#define H5Acreate_by_name_async(...) H5Acreate_by_name_async(__FILE__, __func__, __LINE__, __VA_ARGS__)
#define H5Aopen_async(...)           H5Aopen_async(__FILE__, __func__, __LINE__, __VA_ARGS__)
#define H5Aopen_by_name_async(...)   H5Aopen_by_name_async(__FILE__, __func__, __LINE__, __VA_ARGS__)
#define H5Aopen_by_idx_async(...)    H5Aopen_by_idx_async(__FILE__, __func__, __LINE__, __VA_ARGS__)
#define H5Awrite_async(...)          H5Awrite_async(__FILE__, __func__, __LINE__, __VA_ARGS__)
#define H5Aread_async(...)           H5Aread_async(__FILE__, __func__, __LINE__, __VA_ARGS__)
#define H5Arename_async(...)         H5Arename_async(__FILE__, __func__, __LINE__, __VA_ARGS__)
#define H5Arename_by_name_async(...) H5Arename_by_name_async(__FILE__, __func__, __LINE__, __VA_ARGS__)
#define H5Aexists_async(...)         H5Aexists_async(__FILE__, __func__, __LINE__, __VA_ARGS__)
#define H5Aexists_by_name_async(...) H5Aexists_by_name_async(__FILE__, __func__, __LINE__, __VA_ARGS__)
#define H5Aclose_async(...)          H5Aclose_async(__FILE__, __func__, __LINE__, __VA_ARGS__)

/* Define "wrapper" versions of function calls, to allow compile-time values to
 *      be passed in by language wrapper or library layer on top of HDF5.
 */
#define H5Acreate_async_wrap         H5_NO_EXPAND(H5Acreate_async)
#define H5Acreate_by_name_async_wrap H5_NO_EXPAND(H5Acreate_by_name_async)
#define H5Aopen_async_wrap           H5_NO_EXPAND(H5Aopen_async)
#define H5Aopen_by_name_async_wrap   H5_NO_EXPAND(H5Aopen_by_name_async)
#define H5Aopen_by_idx_async_wrap    H5_NO_EXPAND(H5Aopen_by_idx_async)
#define H5Awrite_async_wrap          H5_NO_EXPAND(H5Awrite_async)
#define H5Aread_async_wrap           H5_NO_EXPAND(H5Aread_async)
#define H5Arename_async_wrap         H5_NO_EXPAND(H5Arename_async)
#define H5Arename_by_name_async_wrap H5_NO_EXPAND(H5Arename_by_name_async)
#define H5Aexists_async_wrap         H5_NO_EXPAND(H5Aexists_async)
#define H5Aexists_by_name_async_wrap H5_NO_EXPAND(H5Aexists_by_name_async)
#define H5Aclose_async_wrap          H5_NO_EXPAND(H5Aclose_async)
#endif /* H5A_MODULE */

/* Symbols defined for compatibility with previous versions of the HDF5 API.
 *
 * Use of these symbols is deprecated.
 */
#ifndef H5_NO_DEPRECATED_SYMBOLS

/* Macros */

/* Typedefs */

/* Typedef for H5Aiterate1() callbacks */
typedef herr_t (*H5A_operator1_t)(hid_t location_id /*in*/, const char *attr_name /*in*/,
                                  void *operator_data /*in,out*/);

/* Function prototypes */
/* --------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Creates an attribute attached to a specified object
 *
 * \fgdt_loc_id
 * \param[in] name     Name of attribute to locate and open
 * \param[in] type_id  Identifier of attribute datatype
 * \space_id
 * \acpl_id
 *
 * \return \hid_tv{attribute}
 *
 * \note The \p acpl parameters is currently not used; specify #H5P_DEFAULT.
 *
 * \deprecated Deprecated in favor of H5Acreate2()
 *
 * \details H5Acreate1() creates an attribute, \p name, which is attached
 *          to the object specified by the identifier \p loc_id.
 *
 *          The attribute name, \p name, must be unique for the object.
 *
 *          The attribute is created with the specified datatype and dataspace,
 *          \p type_id and \p space_id, which are created with the H5T and
 *          H5S interfaces, respectively.
 *
 *          If \p type_id is either a fixed-length or variable-length string,
 *          it is important to set the string length when defining the
 *          datatype. String datatypes are derived from #H5T_C_S1 (or
 *          #H5T_FORTRAN_S1 for Fortran), which defaults to 1 character in
 *          size. See H5Tset_size() and Creating variable-length string
 *          datatypes.
 *
 *          The attribute identifier returned by this function must be released
 *          with H5Aclose() resource leaks will develop.
 *
 * \since 1.8.0
 *
 * \version 1.8.0 The function H5Acreate() was renamed to H5Acreate1() and
 *          deprecated in this release.
 *
 * \see H5Aclose()
 *
 */
H5_DLL hid_t H5Acreate1(hid_t loc_id, const char *name, hid_t type_id, hid_t space_id, hid_t acpl_id);
/* --------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Determines the number of attributes attached to an object
 *
 * \fgdt_loc_id
 *
 * \return Returns the number of attributes if successful; otherwise returns
 *         a negative value.
 *
 * \deprecated This function is deprecated in favor of the functions
 *             H5Oget_info(), H5Oget_info_by_name(), and H5Oget_info_by_idx().
 *
 * \details H5Aget_num_attrs() returns the number of attributes attached to
 *          the object specified by its identifier, \p loc_id.
 *
 * \since 1.0.0
 *
 */
H5_DLL int H5Aget_num_attrs(hid_t loc_id);
/* --------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Calls a user’s function for each attribute on an object
 *
 * \todo make prototype parameter match function (idx vs attr_num)
 *
 * \loc_id
 * \param[in,out] idx     Starting (in) and ending (out) attribute index
 * \param[in]     op      User's function to pass each attribute to
 * \param[in,out] op_data User's data to pass through to iterator operator
 *                        function
 *
 * \return \herr_t
 *
 * \deprecated This function is deprecated in favor of the function
 *             H5Aiterate2().
 *
 * \details H5Aiterate1() iterates over the attributes of the object
 *          specified by its identifier, \p loc_id. The object can be a
 *          group, dataset, or named datatype. For each attribute of the
 *          object, the \p op_data and some additional information specified
 *          below are passed to the operator function \p op. The iteration
 *          begins with the attribute specified by its index, \p idx; the
 *          index for the next attribute to be processed by the operator,
 *          \p op, is returned in \p idx. If \p idx is the null pointer,
 *          then all attributes are processed.
 *
 *          The prototype for #H5A_operator1_t is a user defined function
 *          where:
 *          The operation receives the identifier for the group, dataset
 *          or named datatype being iterated over, \p loc_id, the name of
 *          the current object attribute, \p attr_name, and the pointer to
 *          the operator data passed in to H5Aiterate1(), \p op_data.
 *
 *          The return values from an operator are:
 *
 *          \li Zero causes the iterator to continue, returning zero when
 *              all attributes have been processed.
 *          \li Positive causes the iterator to immediately return that
 *              positive value, indicating short-circuit success. The
 *              iterator can be restarted at the next attribute.
 *          \li Negative causes the iterator to immediately return that value,
 *              indicating failure. The iterator can be restarted at the next
 *              attribute.
 *
 * \todo Add snippet to show H5A_operator1_t.
 *
 * \version 1.8.0 The function \p H5Aiterate was renamed to H5Aiterate1()
 *                and deprecated in this release.
 *
 * \since 1.0.0
 *
 */
H5_DLL herr_t H5Aiterate1(hid_t loc_id, unsigned *idx, H5A_operator1_t op, void *op_data);
/* --------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Opens the attribute specified by its index
 *
 * \loc_id
 * \param[in] idx Index of the attribute to open
 *
 * \return \hid_tv{attribute}
 *
 * \deprecated This function is deprecated in favor of the function
 *             H5Aopen_by_idx().
 *
 * \details H5Aopen_idx() opens an attribute which is attached to the
 *          object specified with \p loc_id . The location object may be
 *          either a group, dataset, or named datatype, all of which may
 *          have any sort of attribute. The attribute specified by the index,
 *          \p idx , indicates the attribute to access. The value of \p idx
 *          is a 0-based, non-negative integer. The attribute identifier
 *          returned from this function must be released with H5Aclose()
 *          or resource leaks will develop.
 *
 * \since 1.0.0
 *
 */
H5_DLL hid_t H5Aopen_idx(hid_t loc_id, unsigned idx);
/* --------------------------------------------------------------------------*/
/**
 * \ingroup H5A
 *
 * \brief Opens an attribute specified by name
 *
 * \loc_id
 * \param[in] name Attribute name
 *
 * \return \hid_tv{attribute}
 *
 * \deprecated This function is deprecated in favor of the function
 *             H5Aopen_by_name().
 *
 * \details H5Aopen_name() opens an attribute specified by its name,
 *          \p name, which is attached to the object specified with
 *          \p loc_id. The location object may be either a group, dataset,
 *          or named datatype, which may have any sort of attribute. The
 *          attribute identifier returned from this function must be
 *          released with H5Aclose() or resource leaks will develop.
 *
 * \since 1.0.0
 *
 */
H5_DLL hid_t H5Aopen_name(hid_t loc_id, const char *name);

#endif /* H5_NO_DEPRECATED_SYMBOLS */

#ifdef __cplusplus
}
#endif

#endif /* H5Apublic_H */
