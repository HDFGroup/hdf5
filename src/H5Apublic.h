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
typedef struct {
    hbool_t           corder_valid; /* Indicate if creation order is valid */
    H5O_msg_crt_idx_t corder;       /* Creation order                 */
    H5T_cset_t        cset;         /* Character set of attribute name */
    hsize_t           data_size;    /* Size of raw data		  */
} H5A_info_t;

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
H5_DLL herr_t  H5Awrite(hid_t attr_id, hid_t type_id, const void *buf);
H5_DLL hid_t   H5Aget_space(hid_t attr_id);
H5_DLL hid_t   H5Aget_type(hid_t attr_id);
H5_DLL hid_t   H5Aget_create_plist(hid_t attr_id);
H5_DLL ssize_t H5Aget_name(hid_t attr_id, size_t buf_size, char *buf);
H5_DLL ssize_t H5Aget_name_by_idx(hid_t loc_id, const char *obj_name, H5_index_t idx_type,
                                  H5_iter_order_t order, hsize_t n, char *name /*out*/, size_t size,
                                  hid_t lapl_id);
H5_DLL hsize_t H5Aget_storage_size(hid_t attr_id);
H5_DLL herr_t  H5Aget_info(hid_t attr_id, H5A_info_t *ainfo /*out*/);
H5_DLL herr_t  H5Aget_info_by_name(hid_t loc_id, const char *obj_name, const char *attr_name,
                                   H5A_info_t *ainfo /*out*/, hid_t lapl_id);
H5_DLL herr_t  H5Aget_info_by_idx(hid_t loc_id, const char *obj_name, H5_index_t idx_type,
                                  H5_iter_order_t order, hsize_t n, H5A_info_t *ainfo /*out*/, hid_t lapl_id);
H5_DLL herr_t  H5Arename(hid_t loc_id, const char *old_name, const char *new_name);
H5_DLL herr_t  H5Arename_by_name(hid_t loc_id, const char *obj_name, const char *old_attr_name,
                                 const char *new_attr_name, hid_t lapl_id);
H5_DLL herr_t  H5Aiterate2(hid_t loc_id, H5_index_t idx_type, H5_iter_order_t order, hsize_t *idx,
                           H5A_operator2_t op, void *op_data);
H5_DLL herr_t  H5Aiterate_by_name(hid_t loc_id, const char *obj_name, H5_index_t idx_type,
                                  H5_iter_order_t order, hsize_t *idx, H5A_operator2_t op, void *op_data,
                                  hid_t lapd_id);
H5_DLL herr_t  H5Adelete(hid_t loc_id, const char *name);
H5_DLL herr_t  H5Adelete_by_name(hid_t loc_id, const char *obj_name, const char *attr_name, hid_t lapl_id);
H5_DLL herr_t H5Adelete_by_idx(hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order,
                               hsize_t n, hid_t lapl_id);
H5_DLL htri_t H5Aexists(hid_t obj_id, const char *attr_name);
H5_DLL htri_t H5Aexists_by_name(hid_t obj_id, const char *obj_name, const char *attr_name, hid_t lapl_id);

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
H5_DLL hid_t  H5Acreate1(hid_t loc_id, const char *name, hid_t type_id, hid_t space_id, hid_t acpl_id);
H5_DLL hid_t  H5Aopen_name(hid_t loc_id, const char *name);
H5_DLL hid_t  H5Aopen_idx(hid_t loc_id, unsigned idx);
H5_DLL int    H5Aget_num_attrs(hid_t loc_id);
H5_DLL herr_t H5Aiterate1(hid_t loc_id, unsigned *attr_num, H5A_operator1_t op, void *op_data);

#endif /* H5_NO_DEPRECATED_SYMBOLS */

#ifdef __cplusplus
}
#endif

#endif /* H5Apublic_H */
