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
 * This file contains public declarations for the H5S module.
 */
#ifndef _H5Spublic_H
#define _H5Spublic_H

/* Public headers needed by this file */
#include "H5public.h"
#include "H5Ipublic.h"

/* Define atomic datatypes */
#define H5S_ALL         (hid_t)0
#define H5S_UNLIMITED   HSIZE_UNDEF

/** Define user-level maximum number of dimensions */
#define H5S_MAX_RANK    32

/* Flags for selection iterators */
#define H5S_SEL_ITER_GET_SEQ_LIST_SORTED 0x0001 /* Retrieve elements from iterator
                                                 * in increasing offset order, for
                                                 * each call to retrieve sequences.
                                                 * Currently, this only applies to
                                                 * point selections, as hyperslab
                                                 * selections are always returned
                                                 * in increasing offset order.
                                                 *
                                                 * Note that the order is only
                                                 * increasing for each call to
                                                 * get_seq_list, the next set of
                                                 * sequences could start with an
                                                 * earlier offset than the previous
                                                 * one.
                                                 */
#define H5S_SEL_ITER_SHARE_WITH_DATASPACE 0x0002 /* Don't copy the dataspace
                                                 * selection when creating the
                                                 * selection iterator.
                                                 *
                                                 * This can improve performance
                                                 * of creating the iterator, but
                                                 * the dataspace _MUST_NOT_ be
                                                 * modified or closed until the
                                                 * selection iterator is closed
                                                 * or the iterator's behavior
                                                 * will be undefined.
                                                 */

/* Different types of dataspaces */
typedef enum H5S_class_t {
    H5S_NO_CLASS         = -1,  /**< error                                      */
    H5S_SCALAR           = 0,   /**< scalar variable                            */
    H5S_SIMPLE           = 1,   /**< simple dataspace                           */
    H5S_NULL             = 2    /**< null dataspace                             */
} H5S_class_t;

/* Different ways of combining selections */
typedef enum H5S_seloper_t {
    H5S_SELECT_NOOP      = -1,  /**< error                                     */
    H5S_SELECT_SET       = 0,   /**< Select "set" operation 		     */
    H5S_SELECT_OR,              /**< Binary "or" operation for hyperslabs
                                 * (add new selection to existing selection)\n
                                 \verbatim
                                 Original region:  AAAAAAAAAA
                                 New region:             BBBBBBBBBB
                                 A or B:           CCCCCCCCCCCCCCCC
                                 \endverbatim
                                 */
    H5S_SELECT_AND,             /**< Binary "and" operation for hyperslabs
                                 * (only leave overlapped regions in selection)
                                 \verbatim
                                 Original region:  AAAAAAAAAA
                                 New region:             BBBBBBBBBB
                                 A and B:                CCCC
                                 \endverbatim
                                 */
    H5S_SELECT_XOR,             /**< Binary "xor" operation for hyperslabs
                                 * (only leave non-overlapped regions in
                                 * selection)
                                 \verbatim
                                 Original region:  AAAAAAAAAA
                                 New region:             BBBBBBBBBB
                                 A xor B:          CCCCCC    CCCCCC
                                 \endverbatim
                                 */
    H5S_SELECT_NOTB,            /**< Binary "not" operation for hyperslabs
                                 * (only leave non-overlapped regions in
                                 * original selection)\n
                                 \verbatim
                                 Original region:  AAAAAAAAAA
                                 New region:             BBBBBBBBBB
                                 A not B:          CCCCCC
                                 \endverbatim
                                 */
    H5S_SELECT_NOTA,            /**< Binary "not" operation for hyperslabs
                                 * (only leave non-overlapped regions in new
                                 * selection)\n
                                 \verbatim
                                 Original region:  AAAAAAAAAA
                                 New region:             BBBBBBBBBB
                                 B not A:                    CCCCCC
                                 \endverbatim
                                 */
    H5S_SELECT_APPEND,          /**< Append elements to end of point selection */
    H5S_SELECT_PREPEND,         /**< Prepend elements to beginning of point selection */
    H5S_SELECT_INVALID          /**< Invalid upper bound on selection operations */
} H5S_seloper_t;

/* Enumerated type for the type of selection */
typedef enum {
    H5S_SEL_ERROR	= -1, 	 /**< Error	                        */
    H5S_SEL_NONE	= 0,     /**< Nothing selected 	            */
    H5S_SEL_POINTS	= 1,     /**< Points / elements selected	*/
    H5S_SEL_HYPERSLABS  = 2, /**< Hyperslab selected            */
    H5S_SEL_ALL		= 3,     /**< Entire extent selected        */
    H5S_SEL_N			     /**< THIS MUST BE LAST             */
} H5S_sel_type;

#ifdef __cplusplus
extern "C" {
#endif

/* Operations on dataspaces */
/*--------------------------------------------------------------------------*/
/**\ingroup H5S
 *
 * \brief Creates a new dataspace of a specified type
 *
 * \param[in] type   Type of dataspace to be created
 *
 * \return \hid_tv{dataspace}
 *
 * \details H5Screate() creates a new dataspace of a particular type. Currently
 *          supported types are #H5S_SCALAR, #H5S_SIMPLE, and #H5S_NULL.
 *
 *          Further dataspace types may be added later.
 *
 *          A scalar dataspace, #H5S_SCALAR, has a single element, though that
 *          element may be of a complex datatype, such as a compound or array
 *          datatype. By convention, the rank of a scalar dataspace is always \p 0
 *          (zero); think of it geometrically as a single, dimensionless point,
 *          though that point can be complex.
 *
 *          A simple dataspace, #H5S_SIMPLE, consists of a regular array of elements.
 *
 *          A null dataspace, #H5S_NULL, has no data elements.
 *
 *          The dataspace identifier returned by this function can be released with
 *          H5Sclose() so that resource leaks will not occur.
 *
 * \version 1.4.0 Fortran subroutine introduced.
 * \since 1.0.0
 *
 */
H5_DLL hid_t H5Screate(H5S_class_t type);
/*----------------------------------------------------------------------------*/
/**\ingroup H5S
 * \brief Creates a new simple dataspace and opens it for access
 *
 * \param[in] rank          Number of dimensions of dataspace
 * \param[in] current_dims  Array specifying the size of each dimension
 * \param[in] maximum_dims  Array specifying the maximum size of each dimension
 *
 * \return \hid_tv{dataspace}
 *
 * \details  H5Screate_simple() creates a new simple dataspace and opens it
 * for access, returning a dataspace identifier.
 *
 * \p rank is the number of dimensions used in the dataspace.
 *
 * \p current_dims is a one-dimensional array of size rank specifying the
 * size of each dimension of the dataset. \p maximum_dims is an array of the
 * same size specifying the upper limit on the size of each dimension.
 *
 * Any element of \p current_dims can be \p 0 (zero). Note that no data can
 * be written to a dataset if the size of any dimension of its current
 * dataspace is \p 0. This is sometimes a useful initial state for a dataset.
 *
 * \p maximum_dims may be the null pointer, in which case the upper limit is
 * the same as \p current_dims. Otherwise, no element of \p maximum_dims
 * should be smaller than the corresponding element of \p current_dims.
 *
 * If an element of \p maximum_dims is #H5S_UNLIMITED, the maximum size of the
 * corresponding dimension is unlimited.
 *
 * Any dataset with an unlimited dimension must also be chunked; see
 * H5Pset_chunk(). Similarly, a dataset must be chunked if \p current_dims does
 * not equal \p maximum_dims.
 *
 * The dataspace identifier returned from this function must be released with
 * H5Sclose() or resource leaks will occur.
 *
 * \note Once a dataspace has been created, specific regions or elements in
 *       the dataspace can be selected and selections can be removed, as well.
 *       For example, H5Sselect_hyperslab() selects a region in a dataspace and
 *       H5Sselect_elements() selects array elements in a dataspace. These
 *       functions are used for subsetting. H5Sselect_none() removes all
 *       selections from a dataspace and is used in Parallel HDF5 when a process
 *       does not have or need to write data.
 *
 * \version 1.4.0 Fortran subroutine introduced.
 *
 * \since 1.0.0
 *
 */
H5_DLL hid_t H5Screate_simple(int rank, const hsize_t dims[],
    const hsize_t maxdims[]);
/*--------------------------------------------------------------------------*/
/**\ingroup H5S
 *
 * \brief Sets or resets the size of an existing dataspace
 *
 * \space_id
 * \param[in] rank   Rank, or dimensionality, of the dataspace
 * \param[in] dims   Array containing current size of dataspace
 * \param[in] max    Array containing maximum size of dataspace
 *
 * \return \herr_t
 *
 * \details H5Sset_extent_simple() sets or resets the size of an existing
 *          dataspace.
 *
 *          \p rank is the dimensionality, or number of dimensions, of the
 *          dataspace.
 *
 *          \p dims is an array of size \p rank which contains the new size
 *          of each dimension in the dataspace. \p max is an array of size
 *          \p rank which contains the maximum size of each dimension in
 *          the dataspace.
 *
 *          Any previous extent is removed from the dataspace, the dataspace
 *          type is set to #H5S_SIMPLE, and the extent is set as specified.
 *
 *          Note that a dataset must be chunked if \p dims does not equal
 *          \p max.
 *
 *
 * \version 1.4.0 Fortran subroutine was introduced.
 * \since 1.0.0
 *
 */
H5_DLL herr_t H5Sset_extent_simple(hid_t space_id, int rank,
    const hsize_t dims[], const hsize_t max[]);
/*-------------------------------------------------------------------------*/
/**\ingroup H5S
 *
 * \brief Creates an exact copy of a dataspace
 *
 * \space_id
 *
 * \return \hid_tv{dataspace}
 *
 * \details H5Scopy() creates a new dataspace which is an exact copy of the
 *          dataspace identified by \p space_id. The dataspace identifier
 *          returned from this function should be released with H5Sclose()
 *          or resource leaks will occur.
 *
 * \version 1.4.0   Fortran subroutine introduced.
 * \since 1.0.0
 *
 */
H5_DLL hid_t H5Scopy(hid_t space_id);
/*------------------------------------------------------------------------- */
/**\ingroup H5S
 *
 * \brief Releases and terminates access to a dataspace
 *
 * \space_id
 *
 * \return \herr_t
 *
 * \details H5Sclose() releases a dataspace. Further access through the
 *          dataspace identifier is illegal. Failure to release a dataspace with this
 *          call will result in resource leaks.
 *
 * \version 1.4.0 Fortran subroutine introduced in this release.
 * \since 1.0.0
 *
 */
H5_DLL herr_t H5Sclose(hid_t space_id);
/*--------------------------------------------------------------------------*/
/**\ingroup H5S
 *
 * \brief Encodes a data space object description into a binary buffer
 *
 * \space_id{obj_id}
 * \param[in,out] buf      Buffer for the object to be encoded into;
 *                         If the provided buffer is NULL, only the size
 *                         of buffer needed is returned through \p nalloc.
 * \param[in,out] nalloc   The size of the allocated buffer
 * \fapl_id
 *
 * \return \herr_t
 * \details Given the data space identifier \p obj_id, H5Sencode2() converts
 *          a data space description into binary form in a buffer. Using this
 *          binary form in the buffer, a data space object can be
 *          reconstructed with H5Sdecode() to return a new object handle
 *          (\p hid_t) for this data space.
 *
 *          A preliminary H5Sencode2() call can be made to determine the
 *          size of the buffer needed. This value is returned in \p nalloc.
 *          That value can then be assigned to \p nalloc for a second
 *          H5Sencode2() call, which will retrieve the actual encoded object.
 *
 *          If the library determines that \p nalloc is not big enough for the
 *          object, it simply returns the size of the buffer needed through
 *          \p nalloc without encoding the provided buffer.
 *
 *          The file access property list \p fapl_id is used to control the
 *          encoding via the \a libver_bounds property (see
 *          H5Pset_libver_bounds()). If the \a libver_bounds property is missing,
 *          H5Sencode2() proceeds as if the \a libver_bounds property were set to
 *          (#H5F_LIBVER_EARLIEST, #H5F_LIBVER_LATEST). (Functionally,
 *          H5Sencode1() is identical to H5Sencode2() with libver_bounds set to
 *          (#H5F_LIBVER_EARLIEST, #H5F_LIBVER_LATEST).)
 *
 *          The types of data space that are addressed in this function are
 *          null, scalar, and simple space. For a simple data space, the
 *          information on the selection, for example, hyperslab selection,
 *          is also encoded and decoded. A complex data space has not been
 *          implemented in the library.
 *
 * \note Motivation: This function was introduced in HDF5-1.12 as part of the
 *       \a H5Sencode format change to enable 64-bit selection encodings and
 *       a dataspace selection that is tied to a file. See the New Features
 *       in HDF5 Release 1.12 as well as the H5Sencode / H5Sdecode Format Change RFC.
 *
 * \todo Fix the references.
 *
 * \since 1.12.0
 *
 */
H5_DLL herr_t H5Sencode2(hid_t obj_id, void *buf, size_t *nalloc, hid_t fapl);
/*--------------------------------------------------------------------------*/
/**\ingroup H5S
 *
 * \brief Decodes a binary object description of data space and returns a
 *        new object handle
 *
 * \param[in] buf  Buffer for the data space object to be decoded
 *
 * \return \hid_t{dataspace}
 *
 * \details Given an object description of a dataspace in binary in a
 *          buffer, H5Sdecode() reconstructs the HDF5 data type object and
 *          returns a new object handle for it. The binary description of the
 *          object is encoded by H5Sencode(). The user is responsible for
 *          passing in the right buffer. The types of dataspace addressed
 *          in this function are null, scalar, and simple space. For a
 *          simple dataspace, the selection information (for example,
 *          hyperslab selection) is also encoded and decoded. A complex
 *          dataspace has not been implemented in the library.
 *
 * \since 1.8.0
 *
 */
H5_DLL hid_t H5Sdecode(const void *buf);
/*-------------------------------------------------------------------------*/
/**\ingroup H5S
 *
 * \brief Determines the number of elements in a dataspace
 *
 * \space_id
 *
 * \return Returns the number of elements in the dataspace if successful;
 *         otherwise returns a negative value.
 *
 * \details H5Sget_simple_extent_npoints() determines the number of elements
 *          in a dataspace \p space_id. For example, a simple 3-dimensional
 *          dataspace with dimensions 2, 3, and 4 would have 24 elements.
 *
 * \version 1.4.0 Fortran subroutine introduced.
 * \since 1.0.0
 *
 */
H5_DLL hssize_t H5Sget_simple_extent_npoints(hid_t space_id);
/*-------------------------------------------------------------------------*/
/**\ingroup H5S
 *
 * \brief Determines the dimensionality of a dataspace
 *
 * \space_id
 *
 * \return Returns the number of dimensions in the dataspace if successful;
 *         otherwise returns a negative value.
 *
 * \details H5Sget_simple_extent_ndims() determines the dimensionality (or
 *          rank) of a dataspace.
 *
 * \version 1.4.0 Fortran subroutine introduced.
 * \since 1.0.0
 *
 */
H5_DLL int H5Sget_simple_extent_ndims(hid_t space_id);
/*-------------------------------------------------------------------------*/
/**\ingroup H5S
 *
 * \brief Retrieves dataspace dimension size and maximum size
 *
 * \space_id
 * \param[out] dims Pointer to array to store the size of each dimension
 * \param[out] maxdims Pointer to array to store the maximum size of each
 *                     dimension
 *
 * \return Returns the number of dimensions in the dataspace if successful;
 *         otherwise returns a negative value.
 *
 * \details H5Sget_simple_extent_dims() returns the size and maximum sizes
 *          of each dimension of a dataspace \p space_id through the \p dims
 *          and \p maxdims parameters.
 *
 *          Either or both of \p dims and \p maxdims may be NULL.
 *
 *          If a value in the returned array \p maxdims is #H5S_UNLIMITED (-1),
 *          the maximum size of that dimension is unlimited.
 *
 * \version 1.4.0 Fortran subroutine introduced.
 * \since 1.0.0
 *
 */
H5_DLL int H5Sget_simple_extent_dims(hid_t space_id, hsize_t dims[],
    hsize_t maxdims[]);
/*--------------------------------------------------------------------------*/
/**\ingroup H5S
 *
 * \brief Determines whether a dataspace is a simple dataspace
 *
 * \space_id
 *
 * \return \htri_t
 *
 * \details H5Sis_simple() determines whether or not a dataspace is a simple
 *          dataspace. [Currently, all dataspace objects are simple 
 *          dataspaces; complex dataspace support will be added in the 
 *          future.]
 *
 *  \version 1.4.0 Fortran subroutine was introduced.
 *  \since 1.0.0
 *
 */
H5_DLL htri_t H5Sis_simple(hid_t space_id);
/*--------------------------------------------------------------------------*/
/**\ingroup H5S
 *
 * \brief  Determines the current class of a dataspace
 *
 * \space_id{sid}
 *
 * \return Returns a dataspace class name if successful;
 *         otherwise #H5S_NO_CLASS (-1).
 *
 * \details H5Sget_simple_extent_type() determines the current class of a
 *          dataspace \p sid.
 *
 * \version 1.4.0 Fortran subroutine was introduced.
 * \since 1.0.0
 *
 */
H5_DLL H5S_class_t H5Sget_simple_extent_type(hid_t space_id);
/*--------------------------------------------------------------------------*/
/**\ingroup H5S
 *
 * \brief Resets the extent of a dataspace back to "none"
 *
 * \space_id
 *
 * \return  \herr_t
 *
 * \details H5Sset_extent_none() resets the type of a dataspace to
 *          #H5S_NULL with no extent information stored for the dataspace.
 *
 * \version 1.10.7, 1.12.1  The function behavior changed. The previous
 *                          behavior was to set the class to #H5S_NO_CLASS.
 * \version 1.4.0           Fortran subroutine was introduced.
 * \since 1.0.0
 *
 */
H5_DLL herr_t H5Sset_extent_none(hid_t space_id);
/*-------------------------------------------------------------------------*/
/**\ingroup H5S
 *
 * \brief Copies the extent of a dataspace
 *
 * \space_id{dst_id}
 * \space_id{src_id}
 *
 * \return \herr_t
 *
 * \details H5Sextent_copy() copies the extent from \p src_id to \p dst_id.
 *          This action may change the type of the dataspace.
 *
 * \version 1.4.0 Fortran subroutine was introduced.
 * \since 1.0.0
 *
 */
H5_DLL herr_t H5Sextent_copy(hid_t dst_id,hid_t src_id);
/*--------------------------------------------------------------------------*/
/**\ingroup H5S
 *
 * \brief Determines whether two dataspace extents are equal
 *
 * \space_id{space1_id}
 * \space_id{space2_id}
 *
 * \return \htri_t
 *
 * \details H5Sextent_equal() determines whether the dataspace extents of
 *          two dataspaces, \p space1_id and \p space2_id, are equal.
 *
 * \since 1.8.0
 *
 */
H5_DLL htri_t H5Sextent_equal(hid_t sid1, hid_t sid2);

/* Operations on dataspace selections */
H5_DLL H5S_sel_type H5Sget_select_type(hid_t spaceid);
H5_DLL hssize_t H5Sget_select_npoints(hid_t spaceid);
H5_DLL herr_t H5Sselect_copy(hid_t dst_id, hid_t src_id);
H5_DLL htri_t H5Sselect_valid(hid_t spaceid);
H5_DLL herr_t H5Sselect_adjust(hid_t spaceid, const hssize_t *offset);
H5_DLL herr_t H5Sget_select_bounds(hid_t spaceid, hsize_t start[],
    hsize_t end[]);
H5_DLL htri_t H5Sselect_shape_same(hid_t space1_id, hid_t space2_id);
H5_DLL htri_t H5Sselect_intersect_block(hid_t space_id, const hsize_t *start,
    const hsize_t *end);
H5_DLL herr_t H5Soffset_simple(hid_t space_id, const hssize_t *offset);
H5_DLL herr_t H5Sselect_all(hid_t spaceid);
H5_DLL herr_t H5Sselect_none(hid_t spaceid);
H5_DLL herr_t H5Sselect_elements(hid_t space_id, H5S_seloper_t op,
    size_t num_elem, const hsize_t *coord);
H5_DLL hssize_t H5Sget_select_elem_npoints(hid_t spaceid);
H5_DLL herr_t H5Sget_select_elem_pointlist(hid_t spaceid, hsize_t startpoint,
    hsize_t numpoints, hsize_t buf[/*numpoints*/]);
/*--------------------------------------------------------------------------*/
/**\ingroup H5S
 *
 * \brief Selects a hyperslab region to add to the current selected region
 *
 * \space_id
 * \param[in] op     Operation to perform on current selection
 * \param[in] start  Offset of start of hyperslab
 * \param[in] stride Hyperslab stride
 * \param[in] count  Number of blocks included in hyperslab
 * \param[in] block  Size of block in hyperslab
 *
 * \return \herr_t
 *
 * \details H5Sselect_hyperslab() selects a hyperslab region to add to the
 *          current selected region for the dataspace specified by
 *          \p space_id.
 *
 *          The \p start, \p stride, \p count, and \p block arrays must be the
 *          same size as the rank of the dataspace. For example, if the
 *          dataspace is 4-dimensional, each of these parameters must be a
 *          1-dimensional array of size 4.
 *
 *          The selection operator \p op determines how the new selection
 *          is to be combined with the already existing selection for the
 *          dataspace. The following operators are supported:
 *
 *          <table>
 *           <tr>
 *             <td>#H5S_SELECT_SET</td>
 *             <td>Replaces the existing selection with the
 *              parameters from this call. Overlapping blocks
 *             are not supported with this operator.</td>
 *           </tr>
 *           <tr>
 *              <td>#H5S_SELECT_OR</td>
 *              <td>Adds the new selection to the existing selection.
 *              (Binary OR)</td>
 *           </tr>
 *           <tr>
 *             <td>#H5S_SELECT_AND</td>
 *             <td>Retains only the overlapping portions of the
 *                new selection and the existing selection.
 *               (Binary AND)</td>
 *          </tr>
 *          <tr>
 *             <td>#H5S_SELECT_XOR</td>
 *             <td>Retains only the elements that are members of
 *                  the new selection or the existing selection,
 *                  excluding elements that are members of both
 *                  selections. (Binary exclusive-OR, XOR)
 *                 </td>
 *          </tr>
 *          <tr>
 *             <td>#H5S_SELECT_NOTB</td>
 *             <td>Retains only elements of the existing selection
 *               that are not in the new selection.</td>
 *          </tr>
 *          <tr>
 *             <td>#H5S_SELECT_NOTA</td>
 *             <td>Retains only elements of the new selection that
 *              are not in the existing selection.</td>
 *          </tr>
 *          </table>
 *
 *          The \p start array specifies the offset of the starting element
 *          of the specified hyperslab.
 *
 *          The \p stride array chooses array locations from the dataspace with
 *          each value in the \p stride array determining how many elements to
 *          move in each dimension. Setting a value in the \p stride array to
 *          \p 1 moves to each element in that dimension of the dataspace;
 *          setting a value of \p 2 in allocation in the \p stride array moves
 *          to every other element in that dimension of the dataspace. In
 *          other words, the \p stride determines the number of elements to
 *          move from the \p start location in each dimension. Stride values
 *          of \p 0 are not allowed. If the \p stride parameter is NULL, a
 *          contiguous hyperslab is selected (as if each value in the \p stride
 *          array were set to \p 1).
 *
 *          The \p count array determines how many blocks to select from the
 *          dataspace, in each dimension.
 *
 *          The \p block array determines the size of the element block
 *          selected from the dataspace. If the \p block parameter is set to
 *          NULL, the block size defaults to a single element in each dimension
 *          (as if each value in the \p block array were set to \p 1).
 *
 *          For example, consider a 2-dimensional dataspace with hyperslab
 *          selection settings as follows: the \p start offset is specified as
 *          [1,1], \p stride is [4,4], \p count is [3,7], and \p block is [2,2].
 *          In C, these settings will specify a hyperslab consisting of 21
 *          2x2 blocks of array elements starting with location (1,1) with the
 *          selected blocks at locations (1,1), (5,1), (9,1), (1,5), (5,5), etc.;
 *          in Fortran, they will specify a hyperslab consisting of 21 2x2
 *          blocks of array elements starting with location (2,2) with the
 *          selected blocks at locations (2,2), (6,2), (10,2), (2,6), (6,6), etc.
 *
 *          Regions selected with this function call default to C order
 *          iteration when I/O is performed.
 *
 * \version 1.4.0 Fortran subroutine introduced in this release.
 * \since 1.0.0
 *
 */
H5_DLL herr_t H5Sselect_hyperslab(hid_t space_id, H5S_seloper_t op,
    const hsize_t start[], const hsize_t _stride[], const hsize_t count[],
    const hsize_t _block[]);
/*--------------------------------------------------------------------------*/
/**\ingroup H5S
 *
 * \brief Performs an operation on a hyperslab and an existing selection and
 *        returns the resulting selection
 *
 * \space_id
 * \param[in] op      Operation to perform on the current selection
 * \param[in] start   Offset of the start of of the hyperslab
 * \param[in] stride  Hyperslab stride
 * \param[in] count   Number of blocks included in the hyperslab
 * \param[in] block   Size of a block in the hyperslab
 *
 * \return \hid_tv{dataspace}
 *
 * \details H5Scombine_hyperslab() combines a hyperslab selection specified
 *          by \p start, \p stride, \p count and \p block with the current
 *          selection for the dataspace \p space_id, creating a new dataspace
 *          to return the generated selection.  If the current selection is
 *          not a hyperslab, it is freed and the hyperslab parameters passed
 *          in are combined with the H5S_SEL_ALL hyperslab (ie. a selection
 *          composing the entire current extent). If either \p stride or
 *          \p block is NULL, then it will be set to \p 1.
 *
 * \since 1.12.0
 *
 */
H5_DLL hid_t H5Scombine_hyperslab(hid_t space_id, H5S_seloper_t op,
    const hsize_t start[], const hsize_t _stride[], const hsize_t count[],
    const hsize_t _block[]);
/*--------------------------------------------------------------------------*/
/**\ingroup H5S
 *
 * \brief Refines a hyperslab selection with an operation, using a second
 *        hyperslab to modify it
 *
 * \space_id{space1_id}
 * \param[in] op  Selection operator
 * \space_id{space2_id}
 *
 * \return \herr_t
 *
 * \details H5Smodify_select() refines an existing hyperslab selection
 * \p space1_id with an operation \p op, using a second hyperslab
 * \p space2_id. The first selection is modified to contain the result of
 * \p space1_id operated on by \p space2_id.
 *
 * \since 1.12.0
 *
*/
H5_DLL herr_t H5Smodify_select(hid_t space1_id, H5S_seloper_t op, hid_t space2_id);
/*--------------------------------------------------------------------------*/
/**\ingroup H5S
 *
 * \brief Combine two hyperslab selections with an operation, returning a
 *        dataspace with the resulting selection
 *
 * \space_id{space1_id}
 * \param[in] op  Selection operator
 * \space_id{space2_id}
 *
 * \return \hid_t{dataspace}
 *
 * \details H5Scombine_select() combines two hyperslab selections
 *          \p space1_id and \p space2_id with an operation, returning a
 *          new dataspace with the resulting selection. The dataspace extent
 *          from \p space1_id is copied for the dataspace extent of the
 *          newly created dataspace.
 *
 * \since 1.12.0
 *
*/
H5_DLL hid_t H5Scombine_select(hid_t space1_id, H5S_seloper_t op, hid_t space2_id);
/*--------------------------------------------------------------------------*/
/**\ingroup H5S
 *
 * \brief Determines if a hyperslab selection is regular
 *
 * \space_id{spaceid}
 *
 * \return \htri_t
 *
 * \details H5Sis_regular_hyperslab() takes the dataspace identifier,
 *          \p spaceid, and queries the type of the hyperslab selection.
 *
 *          A regular hyperslab selection is a hyperslab selection described
 *          by setting the offset, stride, count, and block parameters for
 *          a single H5Sselect_hyperslab() call. If several calls to
 *          H5Sselect_hyperslab() are needed, then the hyperslab selection
 *          is irregular.
 *
 * \since 1.10.0
 *
 */
H5_DLL htri_t H5Sis_regular_hyperslab(hid_t spaceid);
/*--------------------------------------------------------------------------*/
/**\ingroup H5S
 *
 * \brief Retrieves a regular hyperslab selection
 *
 * \space_id{spaceid}
 * \param[out] start   Offset of the start of the regular hyperslab
 * \param[out] stride  Stride of the regular hyperslab
 * \param[out] count   Number of blocks in the regular hyperslab
 * \param[out] block   Size of a block in the regular hyperslab
 *
 * \return \herr_t
 *
 * \details H5Sget_regular_hyperslab() takes the dataspace identifier,
 * \p spaceid, and retrieves the values of \p start, \p stride, \p count,
 * and \p block for the regular hyperslab selection.
 *
 * A regular hyperslab selection is a hyperslab selection described by
 * setting the \p offset, \p stride, \p count, and \p block parameters to
 * the H5Sselect_hyperslab() call. If several calls to H5Sselect_hyperslab()
 * are needed, the hyperslab selection is irregular.
 *
 * See H5Sselect_hyperslab() for descriptions of \p offset, \p stride,
 * \p count, and \p block.
 *
 * \note If a hyperslab selection is originally regular, then becomes
 *       irregular through selection operations, and then becomes regular
 *       again, the final regular selection may be equivalent but not
 *       identical to the original regular selection.
 *
 * \since 1.10.0
 *
 */
H5_DLL htri_t H5Sget_regular_hyperslab(hid_t spaceid, hsize_t start[],
    hsize_t stride[], hsize_t count[], hsize_t block[]);
H5_DLL hssize_t H5Sget_select_hyper_nblocks(hid_t spaceid);
H5_DLL herr_t H5Sget_select_hyper_blocklist(hid_t spaceid, hsize_t startblock,
    hsize_t numblocks, hsize_t buf[/*numblocks*/]);
H5_DLL hid_t H5Sselect_project_intersection(hid_t src_space_id,
    hid_t dst_space_id, hid_t src_intersect_space_id);

/* Operations on dataspace selection iterators */
H5_DLL hid_t H5Ssel_iter_create(hid_t spaceid, size_t elmt_size, unsigned flags);
H5_DLL herr_t H5Ssel_iter_get_seq_list(hid_t sel_iter_id, size_t maxseq,
    size_t maxbytes, size_t *nseq, size_t *nbytes, hsize_t *off, size_t *len);
H5_DLL herr_t H5Ssel_iter_close(hid_t sel_iter_id);

/* Symbols defined for compatibility with previous versions of the HDF5 API.
 *
 * Use of these symbols is deprecated.
 */
#ifndef H5_NO_DEPRECATED_SYMBOLS
/* Function prototypes */
H5_DLL herr_t H5Sencode1(hid_t obj_id, void *buf, size_t *nalloc);

#endif /* H5_NO_DEPRECATED_SYMBOLS */

#ifdef __cplusplus
}
#endif
#endif /* _H5Spublic_H */
