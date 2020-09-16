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
 * This file contains public declarations for the H5E module.
 */
#ifndef _H5Epublic_H
#define _H5Epublic_H

#include <stdio.h>              /*FILE arg of H5Eprint()                     */

/* Public headers needed by this file */
#include "H5public.h"
#include "H5Ipublic.h"

/* Value for the default error stack */
#define H5E_DEFAULT             (hid_t)0

/* Different kinds of error information */
typedef enum H5E_type_t {
    H5E_MAJOR,
    H5E_MINOR
} H5E_type_t;

/* Information about an error; element of error stack */
typedef struct H5E_error2_t {
    hid_t       cls_id;         /*class ID                           */
    hid_t       maj_num;	/*major error ID		     */
    hid_t       min_num;	/*minor error number		     */
    unsigned	line;		/*line in file where error occurs    */
    const char	*func_name;   	/*function in which error occurred   */
    const char	*file_name;	/*file in which error occurred       */
    const char	*desc;		/*optional supplied description      */
} H5E_error2_t;

/* When this header is included from a private header, don't make calls to H5open() */
#undef H5OPEN
#ifndef _H5private_H
#define H5OPEN          H5open(),
#else   /* _H5private_H */
#define H5OPEN
#endif  /* _H5private_H */

/* HDF5 error class */
#define H5E_ERR_CLS		(H5OPEN H5E_ERR_CLS_g)
H5_DLLVAR hid_t H5E_ERR_CLS_g;

/* Include the automatically generated public header information */
/* (This includes the list of major and minor error codes for the library) */
#include "H5Epubgen.h"

/*
 * One often needs to temporarily disable automatic error reporting when
 * trying something that's likely or expected to fail.  The code to try can
 * be nested between calls to H5Eget_auto() and H5Eset_auto(), but it's
 * easier just to use this macro like:
 * 	H5E_BEGIN_TRY {
 *	    ...stuff here that's likely to fail...
 *      } H5E_END_TRY;
 *
 * Warning: don't break, return, or longjmp() from the body of the loop or
 *	    the error reporting won't be properly restored!
 *
 * These two macros still use the old API functions for backward compatibility
 * purpose.
 */
#ifndef H5_NO_DEPRECATED_SYMBOLS
#define H5E_BEGIN_TRY {							      \
    unsigned H5E_saved_is_v2;					              \
    union {								      \
        H5E_auto1_t efunc1;						      \
        H5E_auto2_t efunc2;					              \
    } H5E_saved;							      \
    void *H5E_saved_edata;						      \
								    	      \
    (void)H5Eauto_is_v2(H5E_DEFAULT, &H5E_saved_is_v2);		              \
    if(H5E_saved_is_v2) {						      \
        (void)H5Eget_auto2(H5E_DEFAULT, &H5E_saved.efunc2, &H5E_saved_edata); \
        (void)H5Eset_auto2(H5E_DEFAULT, NULL, NULL);		              \
    } else {								      \
        (void)H5Eget_auto1(&H5E_saved.efunc1, &H5E_saved_edata);		      \
        (void)H5Eset_auto1(NULL, NULL);					      \
    }

#define H5E_END_TRY							      \
    if(H5E_saved_is_v2)							      \
        (void)H5Eset_auto2(H5E_DEFAULT, H5E_saved.efunc2, H5E_saved_edata);   \
    else								      \
        (void)H5Eset_auto1(H5E_saved.efunc1, H5E_saved_edata);		      \
}
#else /* H5_NO_DEPRECATED_SYMBOLS */
#define H5E_BEGIN_TRY {							      \
    H5E_auto2_t saved_efunc;						      \
    void *H5E_saved_edata;						      \
								    	      \
    (void)H5Eget_auto2(H5E_DEFAULT, &saved_efunc, &H5E_saved_edata);	      \
    (void)H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

#define H5E_END_TRY							      \
    (void)H5Eset_auto2(H5E_DEFAULT, saved_efunc, H5E_saved_edata);	      \
}
#endif /* H5_NO_DEPRECATED_SYMBOLS */

/*
 * Public API Convenience Macros for Error reporting - Documented
 */
/* Use the Standard C __FILE__ & __LINE__ macros instead of typing them in */
#define H5Epush_sim(func, cls, maj, min, str) H5Epush2(H5E_DEFAULT, __FILE__, func, __LINE__, cls, maj, min, str)

/*
 * Public API Convenience Macros for Error reporting - Undocumented
 */
/* Use the Standard C __FILE__ & __LINE__ macros instead of typing them in */
/*  And return after pushing error onto stack */
#define H5Epush_ret(func, cls, maj, min, str, ret) {			      \
    H5Epush2(H5E_DEFAULT, __FILE__, func, __LINE__, cls, maj, min, str);      \
    return(ret);							      \
}

/* Use the Standard C __FILE__ & __LINE__ macros instead of typing them in
 * And goto a label after pushing error onto stack.
 */
#define H5Epush_goto(func, cls, maj, min, str, label) {			      \
    H5Epush2(H5E_DEFAULT, __FILE__, func, __LINE__, cls, maj, min, str);      \
    goto label;								      \
}

/* Error stack traversal direction */
typedef enum H5E_direction_t {
    H5E_WALK_UPWARD	= 0,		/*begin deep, end at API function    */
    H5E_WALK_DOWNWARD	= 1		/*begin at API function, end deep    */
} H5E_direction_t;


#ifdef __cplusplus
extern "C" {
#endif

/** Error stack traversal callback function pointers */
//![H5E_walk2_t_snip]
typedef herr_t (*H5E_walk2_t)(unsigned n, const H5E_error2_t *err_desc, void *client_data);
//![H5E_walk2_t_snip]

//![H5E_auto2_t_snip]
typedef herr_t (*H5E_auto2_t)(hid_t estack, void *client_data);
//![H5E_auto2_t_snip]

/* Public API functions */
/**
 *-------------------------------------------------------------------------
 * \ingroup H5E
 *
 * \brief Determines type of the error stack
 *
 * \estack_id
 * \param[out] is_stack Flag indicating version the error stack conforms to
 *
 * \return \herr_t
 *
 * \details H5Eauto_is_v2() determines whether the error auto reporting
 *          function for the error stack \p estack_id conforms to the
 *          H5E_auto2_t version or the H5E_auto1_t version.
 *
 *          \p is_stack is set to 1 if the error stack conforms to H5E_auto2_t
 *          and 0 if it conforms to H5E_auto1_t.
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Eauto_is_v2(hid_t estack_id, unsigned *is_stack);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5E
 *
 * \brief Clears the specified error stack or the error stack for the
 *        current thread
 *
 * \estack_id
 *
 * \return \herr_t
 *
 * \details H5Eclear2() clears the error stack specified by \p estack_id,
 *          or, the error stack for the current thread if \p estack_id is
 *          set to #H5E_DEFAULT.
 *
 *          The current error stack is also cleared whenever an API function
 *          is called, with some exceptions, for instance, H5Eprint1() or
 *          H5Eprint2().
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Eclear2(hid_t estack_id);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5E
 *
 * \brief Closes an error message identifier
 *
 * \param[in] err_id Error message identifier
 *
 * \return \herr_t
 *
 * \details H5Eclose_msg() closes an error message identifier, which can be
 *          either a major or minor message.
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Eclose_msg(hid_t err_id);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5E
 *
 * \brief Closes object handle for error stack
 *
 * \estack_id
 *
 * \return \herr_t
 *
 * \details H5Eclose_stack() closes the object handle for an error stack
 *          and releases its resources. #H5E_DEFAULT cannot be closed.
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Eclose_stack(hid_t estack_id);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5E
 *
 * \brief Adds major error message to an error class
 *
 * \param[in] cls      Error class identifier
 * \param[in] msg_type Type of the error message
 * \param[in] msg      Major error message
 *
 * \return Returns a message identifier on success;
 *         otherwise returns a negative value. 
 *
 * \details H5Ecreate_msg() adds an error message to an error class defined
 *          by client library or application program. The error message can
 *          be either major or minor which is indicated by \p msg_type.
 *
 *          Use H5Eclose_msg() to close the message identifier returned by
 *          this function.
 *
 *          Valid values of \p msg_type are #H5E_MAJOR and #H5E_MINOR.
 *
 * \since 1.8.0
 *
 */
H5_DLL hid_t H5Ecreate_msg(hid_t cls, H5E_type_t msg_type, const char *msg);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5E
 *
 * \brief Creates a new empty error stack
 *
 * \return Returns an error stack identifier if successful; otherwise
 *         returns a negative value.
 *
 * \details H5Ecreate_stack() creates a new empty error stack and returns the
 *          new stack’s identifier.
 *
 *          Use H5Eclose_stack() to close the error stack identifier returned
 *          by this function.
 *
 * \since 1.8.0
 *
 */
H5_DLL hid_t  H5Ecreate_stack(void);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5E
 *
 * \brief Returns the settings for the automatic error stack traversal
 *        function and its data
 *
 * \estack_id
 * \param[out] func The function currently set to be called upon an error
 *                  condition
 * \param[out] client_data Data currently set to be passed to the error
 *                  function
 *
 * \return \herr_t
 *
 * \details H5Eget_auto2() returns the settings for the automatic error
 *          stack traversal function, \p func and its data, \p client_data,
 *          that are associated with the error stack specified by \p estack_id.
 *
 *          Either or both of the \p func and \p client_data arguments
 *          may be null, in which case the value is not returned.
 *
 *          The library initializes its default error stack traversal
 *          functions to H5Eprint1() and H5Eprint2(). A call to
 *          H5Eget_auto2() returns H5Eprint2() or the user-defined function
 *          passed in through H5Eset_auto2(). A call to H5Eget_auto1()
 *          returns H5Eprint1() or the user-defined function passed in
 *          through H5Eset_auto1(). However, if the application passes
 *          in a user-defined function through H5Eset_auto1(), it should
 *          call H5Eget_auto1() to query the traversal function. If
 *          the application passes in a user-defined function through
 *          H5Eset_auto2(), it should call H5Eget_auto2() to query the
 *          traversal function.
 *
 *          Mixing the new style and the old style functions will cause a
 *          failure. For example, if the application sets a user-defined
 *          old-style traversal function through H5Eset_auto1(), a call to
 *          H5Eget_auto2() will fail and will indicate that the application
 *          has mixed H5Eset_auto1() and H5Eget_auto2(). On the other hand,
 *          mixing H5Eset_auto2() and H5Eget_auto1() will also cause a
 *          failure. But if the traversal functions are the library’s
 *          default H5Eprint1() or H5Eprint2(), mixing H5Eset_auto1()
 *          and H5Eget_auto2() or mixing H5Eset_auto2() and H5Eget_auto1()
 *          does not fail.
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Eget_auto2(hid_t estack_id, H5E_auto2_t *func, void **client_data);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5E
 *
 * \brief Retrieves error class name
 *
 * \param[in] class_id Error class identifier
 * \param[out] name    Name of the class to be queried
 * \param[in] size     Length of the class name to be returned
 *
 * \return Returns non-negative value on success; otherwise returns
 *         negative value.
 *
 * \details H5Eget_class_name() retrieves the name of the
 *          error class specified by \p class_id. If non-NULL pointer
 *          is passed in for \p name and \p size is greater than zero,
 *          the class name of \p size long is returned. The length of
 *          the error class name is also returned. If NULL is passed in
 *          as \p name, only the length of class name is returned. If
 *          zero is returned, it means no name. User is responsible for
 *          allocated large enough buffer for the name.
 *
 * \since 1.8.0
 *
 */
H5_DLL ssize_t H5Eget_class_name(hid_t class_id, char *name, size_t size);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5E
 *
 * \brief Returns a copy of the current error stack
 *
 * \return Returns an error stack identifier if successful; otherwise
 *         returns a negative value.
 *
 * \details H5Eget_current_stack() copies the current error stack and returns
 *          an error stack identifier for the new copy.
 *
 * \since 1.8.0
 *
 */
H5_DLL hid_t H5Eget_current_stack(void);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5E
 *
 * \brief Retrieves an error message
 *
 * \param[in] msg_id Identifier for error message to be queried
 * \param[out] type   Type of the error message
 * \param[out] msg    Error message buffer
 * \param[in] size   Length of error message to be returned
 *
 * \return Returns the size of the error message in bytes on
 *         success; otherwise returns a negative value.
 *
 * \details H5Eget_msg() retrieves the error message including
 *          its length and type. The error message is specified by \p
 *          msg_id. User is responsible for passing in large enough buffer
 *          for the message. If \p msg is not NULL and \p size is greater
 *          than zero, the error message of \p size long is returned. The
 *          length of the message is also returned. If NULL is passed in as
 *          \p msg, only the length and type of the message is returned. If
 *          the return value is zero, it means no message.
 *
 *          Valid values for \p type are #H5E_MAJOR and #H5E_MINOR.
 *
 * \since 1.8.0
 *
 */
H5_DLL ssize_t H5Eget_msg(hid_t msg_id, H5E_type_t *type, char *msg,
    size_t size);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5E
 *
 * \brief Retrieves the number of error messages in an error stack
 *
 * \estack_id
 *
 * \return Returns the number of error records in the error stack
 *
 * \details H5Eget_num() returns the number of error records in the error stack
 *          specified by \p estack_id (including major, minor messages and
 *          description).
 *
 * \since 1.8.0
 *
 */
H5_DLL ssize_t H5Eget_num(hid_t estack_id);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5E
 *
 * \brief Close  an error message identifier
 *
 * \estack_id
 * \param[in] count The number of error messages to be deleted from the top
 *                  of error stack
 *
 * \return \herr_t
 *
 * \details H5Epop() deletes \p count records from the top of the error stack
 *          \p estack_id (including major, minor messages and description).
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Epop(hid_t estack_id, size_t count);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5E
 *
 * \brief Prints the specified error stack in a default manner
 *
 * \estack_id
 * \param[in] stream File pointer, or stderr if NULL
 *
 * \return \herr_t
 *
 * \details H5Eprint2() prints the error stack specified by
 *          \p estack_id on the specified stream, \p stream. Even if the
 *          error stack is empty, a one-line message of the following form
 *          will be printed:
 *
 *          \code
 *          HDF5-DIAG: Error detected in HDF5 library version: 1.5.62 thread 0.
 *          \endcode
 *
 *          A similar line will appear before the error messages of each
 *          error class stating the library name, library version number,
 *          and thread identifier.
 *          
 *          If \p estack_id is #H5E_DEFAULT, the current error stack will
 *          be printed.
 *
 *          H5Eprint2() is a convenient function for H5Ewalk2 with a
 *          function that prints error messages. Users are encouraged to
 *          write their own more specific error handlers.
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Eprint2(hid_t estack_id, FILE *stream);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5E
 *
 * \brief Pushes new error record onto error stack
 *
 * \estack_id
 * \param[in] file Name of the file in which the error was detected
 * \param[in] func Name of the function in which the error was detected
 * \param[in] line Line number within the file at which the error was detected
 * \param[in] cls_id Error class identifier
 * \param[in] maj_id Major error identifier
 * \param[in] min_id Minor error identifier
 * \param[in] msg Error description string
 *
 * \return \herr_t
 *
 * \details H5Epush2()  pushes a new error record onto the error stack
 *          specified by \p estack_id.
 *
 *          The error record contains the error class identifier \p
 *          class_id, the major and minor message identifiers \p major_id
 *          and \p minor_id, the function name \p func where the error
 *          was detected, the file name \p file and line number \p line
 *          within that file where the error was detected, and an error
 *          description \p msg.
 *
 *          The major and minor errors must be in the same error class.
 *
 *          The function name, filename, and error description strings
 *          must be statically allocated.
 *
 *          \p msg can be a format control string with additional
 *          arguments. This design of appending additional arguments is
 *          similar to the system and C functions printf and fprintf.
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Epush2(hid_t estack_id, const char *file, const char *func, unsigned line, hid_t cls_id, hid_t maj_id, hid_t min_id, const char *msg, ...);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5E
 *
 * \brief Registers a client library or application program to the HDF5
 *        error API
 *
 * \param[in] cls_name Name of the error class
 * \param[in] lib_name Name of the client library or application to which
 *            the error class belongs
 * \param[in] version Version of the client library or application to which
 *            the error class belongs
 *
 * \return Returns a class identifier on success; otherwise, a negative value.
 *
 * \details H5Eregister_class() registers a client library or
 *          application program to the HDF5 error API so that the client
 *          library or application program can report errors together
 *          with the HDF5 library. It receives an identifier for this
 *          error class for further error operations. The library name
 *          and version number will be printed out in the error message
 *          as a preamble.
 *
 *          A NULL can be passed in for \p version.
 *
 * \since 1.8.0
 *
 */
H5_DLL hid_t  H5Eregister_class(const char *cls_name, const char *lib_name,
    const char *version);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5E
 *
 * \brief Turns automatic error printing on or off
 *
 * \estack_id
 * \param[in] func Function to be called upon an error condition
 * \param[in] client_data Data passed to the error function
 *
 * \return \herr_t
 *
 * \details H5Eset_auto2() turns on or off automatic printing
 *          of errors for the error stack specified with \p estack_id. An
 *          \p estack_id value of #H5E_DEFAULT indicates the current stack.
 *
 *          When automatic printing is turned on, by the use of a
 *          non-null func pointer, any API function which returns an error
 *          indication will first call \p func, passing it \p client_data
 *          as an argument.
 *
 *          \p func, a function compliant with the H5E_auto2_t prototype,
 *          is defined in the H5Epublic.h source code file as:
 *          
 * \snippet this H5E_auto2_t_snip
 *          When the library is first initialized, the auto printing
 *          function is set to H5Eprint2() (cast appropriately) and \p
 *          client_data is the standard error stream pointer, \p stderr.
 *
 *          Automatic stack traversal is always in the #H5E_WALK_DOWNWARD
 *          direction.
 *
 *          Automatic error printing is turned off with a H5Eset_auto2()
 *          call with a NULL func pointer.
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Eset_auto2(hid_t estack_id, H5E_auto2_t func, void *client_data);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5E
 *
 * \brief Replaces the current error stack
 *
 * \estack_id
 *
 * \return \herr_t
 *
 * \details H5Eset_current_stack() replaces the content of the current
 *          error stack with a copy of the content of the error stack
 *          specified by \p estack_id, and it closes the error stack
 *          specified by \p estack_id.
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Eset_current_stack(hid_t estack_id);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5E
 *
 * \brief Removes an error class
 *
 * \param[in] class_id Error class identifier
 *
 * \return \herr_t
 *
 * \details H5Eunregister_class() removes the error class specified by
 *          \p class_id. All the major and minor errors in this class will
 *          also be closed.
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Eunregister_class(hid_t class_id);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5E
 *
 * \brief Walks the specified error stack, calling the specified function
 *
 * \estack_id
 * \param[in] direction   Direction in which the error stack is to be walked
 * \param[in] func        Function to be called for each error encountered
 * \param[in] client_data Data to be passed with func
 *
 * \return \herr_t
 *
 * \details H5Ewalk2() walks the error stack specified by \p estack_id
 *          for the current thread and calls the function specified in
 *          \p func for each error along the way.
 *          
 *          If the value of \p estack_id is #H5E_DEFAULT, then H5Ewalk2()
 *          walks the current error stack.
 *
 *          \p direction specifies whether the stack is walked from the
 *          inside out or the outside in. A value of #H5E_WALK_UPWARD means
 *          to begin with the most specific error and end at the API; a
 *          value of #H5E_WALK_DOWNWARD means to start at the API and end
 *          at the innermost function where the error was first detected.
 *
 *          \p func, a function compliant with the \p H5E_walk2_t prototype,
 *          will be called for each error in the error stack. Its
 *          arguments will include an index number \p n (beginning at
 *          zero regardless of stack traversal direction), an error stack
 *          entry \p err_desc, and the \p client_data pointer passed to
 *          H5Eprint(). The H5E_walk2_t prototype is as follows:
 *
 * \snippet this H5E_walk2_t_snip
 *          where the parameters are described in the following table:
 *
 *          <table>
 *          <tr><td>\c n</td>
 *              <td>Indexed position of the error in the stack </td></tr>
 *          <tr><td>\c err_desc</td>
 *              <td>Pointer to a data structure describing the error
 *                  (This structure is currently described only in the source
 *                   code file hdf5/src/H5Epublic.h. That file also contains
 *                   the definition list of major and minor error codes; that
 *                   information will eventually be presented as an appendix to
 *                   this HDF5 Reference Manual.)</td></tr>
 *          <tr><td>\c client_data</td>
 *              <td>Pointer to client data in the format expected by the
 *                  user-defined function</td></tr>
 *          </table>
 *
 * \attention Programming Note for C++ Developers Using C Functions:
 *
 *          If a C routine that takes a function pointer as an argument is
 *          called from within C++ code, the C routine should be returned
 *          from normally.
 *
 *          Examples of this kind of routine include callbacks such as
 *          H5Pset_elink_cb() and H5Pset_type_conv_cb() and functions such
 *          as H5Tconvert() and H5Ewalk2().
 *
 *          Exiting the routine in its normal fashion allows the HDF5
 *          C library to clean up its work properly. In other words, if
 *          the C++ application jumps out of the routine back to the C++
 *          “catch” statement, the library is not given the opportunity
 *          to close any temporary data structures that were set up when
 *          the routine was called. The C++ application should save some
 *          state as the routine is started so that any problem that occurs
 *          might be diagnosed.
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Ewalk2(hid_t estack_id, H5E_direction_t direction, H5E_walk2_t func, void *client_data);


/* Symbols defined for compatibility with previous versions of the HDF5 API.
 *
 * Use of these symbols is deprecated.
 */
#ifndef H5_NO_DEPRECATED_SYMBOLS

/* Typedefs */

/* Alias major & minor error types to hid_t's, for compatibility with new
 *      error API in v1.8
 */
typedef hid_t   H5E_major_t;
typedef hid_t   H5E_minor_t;

/* Information about an error element of error stack. */
typedef struct H5E_error1_t {
    H5E_major_t maj_num;                /*major error number                 */
    H5E_minor_t min_num;                /*minor error number                 */
    const char  *func_name;             /*function in which error occurred   */
    const char  *file_name;             /*file in which error occurred       */
    unsigned    line;                   /*line in file where error occurs    */
    const char  *desc;                  /*optional supplied description      */
} H5E_error1_t;

/** Error stack traversal callback function pointers */
//![H5E_walk1_t_snip]
typedef herr_t (*H5E_walk1_t)(int n, H5E_error1_t *err_desc, void *client_data);
//![H5E_walk1_t_snip]
//![H5E_auto1_t_snip]
typedef herr_t (*H5E_auto1_t)(void *client_data);
//![H5E_auto1_t_snip]

/* Function prototypes */
/**
 *-------------------------------------------------------------------------
 * \ingroup H5E
 *
 * \brief Clears the error stack for the current thread
 *
 * \return \herr_t
 *
 * \deprecated As of HDF5-1.8, this function has been deprecated in favor of
 *             the function H5Eclear2().
 *
 * \details H5Eclear1() clears the error stack for the current thread.
 *
 *          The current error stack is also cleared whenever an API function
 *          is called, with some exceptions, for instance, H5Eprint1().
 *
 * \version 1.8.0 Function H5Eclear() renamed to H5Eclear1() and deprecated
 *          in this release.
 *
 * \since?
 *
 */
H5_DLL herr_t H5Eclear1(void);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5E
 *
 * \brief Returns the settings for the automatic error stack traversal
 *        function and its data
 *
 * \param[out] func The function currently set to be called upon an error
 *                  condition
 * \param[out] client_data Data currently set to be passed to the error
 *                  function
 *
 * \return \herr_t
 *
 * \deprecated As of HDF5-1.8, this function has been deprecated in favor of
 *             the function H5Eget_auto2().
 *
 * \details H5Eget_auto2() returns the settings for the automatic error
 *          stack traversal function, \p func and its data, \p client_data.
 *          Either or both of the \p func and \p client_data arguments
 *          may be null, in which case the value is not returned.
 *
 *          The library initializes its default error stack traversal
 *          functions to H5Eprint1() and H5Eprint2(). A call to
 *          H5Eget_auto2() returns H5Eprint2() or the user-defined function
 *          passed in through H5Eset_auto2(). A call to H5Eget_auto1()
 *          returns H5Eprint1() or the user-defined function passed in
 *          through H5Eset_auto1(). However, if the application passes
 *          in a user-defined function through H5Eset_auto1(), it should
 *          call H5Eget_auto1() to query the traversal function. If
 *          the application passes in a user-defined function through
 *          H5Eset_auto2(), it should call H5Eget_auto2() to query the
 *          traversal function.
 *
 *          Mixing the new style and the old style functions will cause a
 *          failure. For example, if the application sets a user-defined
 *          old-style traversal function through H5Eset_auto1(), a call to
 *          H5Eget_auto2() will fail and will indicate that the application
 *          has mixed H5Eset_auto1() and H5Eget_auto2(). On the other hand,
 *          mixing H5Eset_auto2() and H5Eget_auto1() will also cause a
 *          failure. But if the traversal functions are the library’s
 *          default H5Eprint1() or H5Eprint2(), mixing H5Eset_auto1()
 *          and H5Eget_auto2() or mixing H5Eset_auto2() and H5Eget_auto1()
 *          does not fail.
 *
 * \version 1.8.0 Function H5Eget_auto() renamed to H5Eget_auto1() and
 *          deprecated in this release.
 *
 * \since?
 *
 */
H5_DLL herr_t H5Eget_auto1(H5E_auto1_t *func, void **client_data);

H5_DLL char *H5Eget_major(H5E_major_t maj);
H5_DLL char *H5Eget_minor(H5E_minor_t min);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5E
 *
 * \brief Prints the specified error stack in a default manner
 *
 * \param[in] stream File pointer, or stderr if NULL
 *
 * \return \herr_t
 *
 * \deprecated As of HDF5-1.8, this function has been deprecated in favor of
 *             the function H5Eprint2() or the macro H5Eprint().
 *
 * \details H5Eprint2() prints the error stack specified by
 *          \p estack_id on the specified stream, \p stream. Even if the
 *          error stack is empty, a one-line message of the following form
 *          will be printed:
 *
 *          \code
 *          HDF5-DIAG: Error detected in HDF5 library version: 1.5.62 thread 0.
 *          \endcode
 *
 *          A similar line will appear before the error messages of each
 *          error class stating the library name, library version number,
 *          and thread identifier.
 *          
 *          If \p estack_id is #H5E_DEFAULT, the current error stack will
 *          be printed.
 *
 *          H5Eprint2() is a convenient function for H5Ewalk2 with a
 *          function that prints error messages. Users are encouraged to
 *          write their own more specific error handlers.
 *
 * \version 1.8.0 Function H5Eprint() renamed to H5Eprint1() and deprecated
 *          in this release.
 *
 * \since?
 *
 */
H5_DLL herr_t H5Eprint1(FILE *stream);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5E
 *
 * \brief Pushes new error record onto error stack
 *
 * \param[in] file Name of the file in which the error was detected
 * \param[in] func Name of the function in which the error was detected
 * \param[in] line Line number within the file at which the error was detected
 * \param[in] maj_id Major error identifier
 * \param[in] min_id Minor error identifier
 * \param[in] msg Error description string
 *
 * \return \herr_t
 *
 * \deprecated As of HDF5-1.8, this function has been deprecated in favor of
 *             the function H5Epush2() or the macro H5Epush().
 *
 * \details H5Epush1() pushes a new error record onto the error stack
 *          for the current thread.
 *
 *          The error has major and minor numbers \p maj_id and \p min_id,
 *          the function \p func where the error was detected, the name
 *          of the file \p file where the error was detected, the line \p
 *          line within that file, and an error description string \p msg.
 *
 *          The function name, filename, and error description strings
 *          must be statically allocated.
 *
 *          \p msg can be a format control string with additional
 *          arguments. This design of appending additional arguments is
 *          similar to the system and C functions printf and fprintf.
 *
 * \version 1.8.0 Function H5Epush() renamed to H5Epush1() and deprecated
 *          in this release.
 *
 * \since 1.4.0
 *
 */
H5_DLL herr_t H5Epush1(const char *file, const char *func, unsigned line,
    H5E_major_t maj_id, H5E_minor_t min_id, const char *msg);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5E
 *
 * \brief Turns automatic error printing on or off
 *
 * \param[in] func Function to be called upon an error condition
 * \param[in] client_data Data passed to the error function
 *
 * \return \herr_t
 *
 * \deprecated As of HDF5-1.8, this function has been deprecated in favor of
 *             the function H5Eset_auto2().
 *
 * \details H5Eset_auto2() turns on or off automatic printing of errors. When
 *          turned on, by the use of a non-null func pointer, any API function
 *          which returns an error indication will first call \p func, passing
 *          it \p client_data as an argument.
 *
 *          \p func, a function compliant with the H5E_auto2_t prototype,
 *          is defined in the H5Epublic.h source code file as:
 *          
 * \snippet this H5E_auto2_t_snip
 *          When the library is first initialized, the auto printing
 *          function is set to H5Eprint2() (cast appropriately) and \p
 *          client_data is the standard error stream pointer, \p stderr.
 *
 *          Automatic stack traversal is always in the #H5E_WALK_DOWNWARD
 *          direction.
 *
 *          Automatic error printing is turned off with a H5Eset_auto2()
 *          call with a NULL func pointer.
 *
 * \version 1.8.0 Function H5Eset_auto() renamed to H5Eset_auto1() and
 *          deprecated in this release.
 *
 * \since?
 *
 */
H5_DLL herr_t H5Eset_auto1(H5E_auto1_t func, void *client_data);

/**
 *-------------------------------------------------------------------------
 * \ingroup H5E for H5Ewalk1
 *
 * \brief Walks the specified error stack, calling the specified function
 *
 * \param[in] direction   Direction in which the error stack is to be walked
 * \param[in] func        Function to be called for each error encountered
 * \param[in] client_data Data to be passed with func
 *
 * \return \herr_t
 *
 * \deprecated As of HDF5-1.8, this function has been deprecated in favor of
 *             the function H5Ewalk2() or the macro H5Ewalk().
 *
 * \details H5Ewalk1() walks the error stack
 *          for the current thread and calls the function specified in
 *          \p func for each error along the way.
 *
 *          \p direction specifies whether the stack is walked from the
 *          inside out or the outside in. A value of #H5E_WALK_UPWARD means
 *          to begin with the most specific error and end at the API; a
 *          value of #H5E_WALK_DOWNWARD means to start at the API and end
 *          at the innermost function where the error was first detected.
 *
 *          \p func
 *          will be called for each error in the error stack. Its
 *          arguments will include an index number \p n (beginning at
 *          zero regardless of stack traversal direction), an error stack
 *          entry \p err_desc, and the \p client_data pointer passed to
 *          H5Eprint(). The H5E_walk1_t prototype is as follows:
 *
 * \snippet this H5E_walk1_t_snip
 *          where the parameters are described in the following table:
 *
 *          <table>
 *          <tr><td>\c n</td>
 *              <td>Indexed position of the error in the stack </td></tr>
 *          <tr><td>\c err_desc</td>
 *              <td>Pointer to a data structure describing the error
 *                  (This structure is currently described only in the source
 *                   code file hdf5/src/H5Epublic.h. That file also contains
 *                   the definition list of major and minor error codes; that
 *                   information will eventually be presented as an appendix to
 *                   this HDF5 Reference Manual.)</td></tr>
 *          <tr><td>\c client_data</td>
 *              <td>Pointer to client data in the format expected by the
 *                  user-defined function</td></tr>
 *          </table>
 *
 * \attention Programming Note for C++ Developers Using C Functions:
 *
 *          If a C routine that takes a function pointer as an argument is
 *          called from within C++ code, the C routine should be returned
 *          from normally.
 *
 *          Examples of this kind of routine include callbacks such as
 *          H5Pset_elink_cb() and H5Pset_type_conv_cb() and functions such
 *          as H5Tconvert() and H5Ewalk2().
 *
 *          Exiting the routine in its normal fashion allows the HDF5
 *          C library to clean up its work properly. In other words, if
 *          the C++ application jumps out of the routine back to the C++
 *          “catch” statement, the library is not given the opportunity
 *          to close any temporary data structures that were set up when
 *          the routine was called. The C++ application should save some
 *          state as the routine is started so that any problem that occurs
 *          might be diagnosed.
 *
 * \version 1.8.0 Function H5Ewalk() renamed to H5Ewalk1() and deprecated in
 *          this release.
 *
 * \since?
 *
 */
H5_DLL herr_t H5Ewalk1(H5E_direction_t direction, H5E_walk1_t func,
    void *client_data);
#endif /* H5_NO_DEPRECATED_SYMBOLS */

#ifdef __cplusplus
}
#endif

#endif /* end _H5Epublic_H */

