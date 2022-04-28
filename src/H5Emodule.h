/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
 * Programmer:	Quincey Koziol
 *		Saturday, September 12, 2015
 *
 * Purpose:	This file contains declarations which define macros for the
 *		H5E package.  Including this header means that the source file
 *		is part of the H5E package.
 */
#ifndef H5Emodule_H
#define H5Emodule_H

/* Define the proper control macros for the generic FUNC_ENTER/LEAVE and error
 *      reporting macros.
 */
#define H5E_MODULE
#define H5_MY_PKG      H5E
#define H5_MY_PKG_ERR  H5E_ERROR
#define H5_MY_PKG_INIT YES

/** \page H5E_UG HDF5 Error Handling
 *
 * \section sec_error HDF5 Error Handling
 *
 * Use the functions in this module to manage HDF5 error stacks and error
 * messages.
 *
 * <table>
 * <tr><th>Create</th><th>Read</th></tr>
 * <tr valign="top">
 *   <td>
 *   \snippet{lineno} H5E_examples.c create
 *   </td>
 *   <td>
 *   \snippet{lineno} H5E_examples.c read
 *   </td>
 * <tr><th>Update</th><th>Delete</th></tr>
 * <tr valign="top">
 *   <td>
 *   \snippet{lineno} H5E_examples.c update
 *   </td>
 *   <td>
 *   \snippet{lineno} H5E_examples.c delete
 *   </td>
 * </tr>
 * </table>
 *
 * \subsection subsec_error_intro Introduction
 * When an error occurs deep within the HDF5 library a record is
 *     pushed onto an error stack and that function returns a failure
 *     indication.  Its caller detects the failure, pushes another
 *     record onto the stack, and returns a failure indication.  This
 *     continues until the application-called API function returns a
 *     failure indication (a negative integer or null pointer).  The
 *     next API function which is called (with a few exceptions) resets
 *     the stack.
 *
 * \subsection subsec_error_ops Error Handling Operations
 * In normal circumstances, an error causes the stack to be
 *     printed on the standard error stream.  The first item, number
 *     "#000" is produced by the API function itself and is usually
 *     sufficient to indicate to the application programmer what went
 *     wrong.
 *  <table>
 *     <caption align=top>Example: An Error Message</caption>
 *     <tr>
 *       <td>
 *         <p>If an application calls <code>H5Tclose</code> on a
 *       predefined datatype then the following message is
 *       printed on the standard error stream.  This is a
 *       simple error that has only one component, the API
 *       function; other errors may have many components.
 *
 *         <p><code><pre>
HDF5-DIAG: Error detected in thread 0.  Back trace follows.
  #000: H5T.c line 462 in H5Tclose(): predefined datatype
    major(01): Function argument
    minor(05): Bad value
 *         </pre></code>
 *       </td>
 *     </tr>
 *   </table>
 *
 *   <p>The error stack can also be printed and manipulated by these
 *     functions, but if an application wishes make explicit calls to
 *     <code>H5Eprint()</code> then the automatic printing should be
 *     turned off to prevent error messages from being displayed twice
 *     (see <code>H5Eset_auto()</code> below).
 *
 *   <dl>
 *     <dt><code>@ref H5Eprint2(hid_t err_stack, FILE *stream)</code>
 *     <dd>The error stack is printed on the specified stream. Even if
 *   the error stack is empty a one-line message will be printed:
 *   <code>HDF5-DIAG: Error detected in thread 0.</code>
 *
 *     <dt><code>@ref H5Eclear2(hid_t err_stack)</code>
 *     <dd>The error stack can be explicitly cleared by calling this
 *   function.  The stack is also cleared whenever an API function
 *   is called, with certain exceptions (for instance,
 *   <code>H5Eprint()</code>).
 *   </dl>
 *
 *   <p>Sometimes an application will call a function for the sake of
 *     its return value, fully expecting the function to fail.  Under
 *     these conditions, it would be misleading if an error message
 *     were automatically printed.  Automatic printing of messages is
 *     controlled by the <code>H5Eset_auto()</code> function:
 *
 *   <dl>
 *     <dt><code>@ref H5Eset_auto2(hid_t estack_id, H5E_auto2_t func, void *client_data)</code>
 *     <dd>If <em>func</em> is not a null pointer, then the function to
 *   which it points will be called automatically when an API
 *   function is about to return an indication of failure.  The
 *   function is called with a single argument, the
 *   <em>client_data</em> pointer.  When the library is first
 *   initialized the auto printing function is set to
 *   <code>H5Eprint()</code> (cast appropriately) and
 *   <em>client_data</em> is the standard error stream pointer,
 *   <code>stderr</code>.
 *
 *     <dt><code>@ref H5Eget_auto2(hid_t estack_id, H5E_auto2_t *func, void **client_data))</code>
 *     <dd>This function returns the settings for the automatic error stack
 *          traversal function, \p func, and its data, \p client_data, that are
 *          associated with the error stack specified by \p estack_id.
 *   </dl>
 *
 * \defgroup H5E H5E
 *
 * \internal The \c FUNC_ENTER macro clears the error stack whenever an
 *           interface function is entered. When an error is detected, an entry
 *           is pushed onto the stack. As the functions unwind, additional
 *           entries are pushed onto the stack. The API function will return
 *           some indication that an error occurred and the application can
 *           print the error stack.
 *
 * \internal Certain API functions in the \ref H5E package, such as H5Eprint(),
 *           do not clear the error stack. Otherwise, any function which does
 *           not have an underscore immediately after the package name will
 *           clear the error stack. For instance, H5Fopen() clears the error
 *           stack while \Code{H5F_open} does not.
 *
 * \internal An error stack has a fixed maximum size. If this size is exceeded
 *           then the stack will be truncated and only the inner-most functions
 *           will have entries on the stack. This is expected to be a rare
 *           condition.
 *
 * \internal Each thread has its own error stack, but since multi-threading has
 *           not been added to the library yet, this package maintains a single
 *           error stack. The error stack is statically allocated to reduce the
 *           complexity of handling errors within the \ref H5E package.
 *
 * See \ref sec_error
 *
 */

#endif /* H5Emodule_H */
