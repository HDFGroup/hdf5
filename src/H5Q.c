/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose:	Query routines.
 */

/****************/
/* Module Setup */
/****************/

#include "H5Qmodule.h"      /* This source code file is part of the H5Q module */

/***********/
/* Headers */
/***********/
#include "H5private.h"      /* Generic Functions */
#include "H5Qprivate.h"     /* Query */
#include "H5Eprivate.h"     /* Error handling */
#include "H5Iprivate.h"     /* IDs */
#include "H5MMprivate.h"    /* Memory management */
#include "H5Pprivate.h"
#include "H5FLprivate.h"    /* Free lists */
#include "H5Dprivate.h"     /* Datasets */
#include "H5Rprivate.h"     /* References */
#include "H5Aprivate.h"     /* Attributes */
#include "H5FDcore.h"       /* Core driver */


/****************/
/* Local Macros */
/****************/
#define H5Q_APPLY_MATCH_OP(result, op, x, y) \
    switch (op) { \
        case H5Q_MATCH_EQUAL: \
            H5_GCC_DIAG_OFF(float-equal) \
            result = (x == y); \
            H5_GCC_DIAG_ON(float-equal) \
            break; \
        case H5Q_MATCH_NOT_EQUAL: \
            H5_GCC_DIAG_OFF(float-equal) \
            result = (x != y); \
            H5_GCC_DIAG_ON(float-equal) \
            break; \
        case H5Q_MATCH_LESS_THAN: \
            result = (x < y); \
            break; \
        case H5Q_MATCH_GREATER_THAN: \
            result = (x > y); \
            break; \
        default: \
            break; \
    }

#define H5Q_CMP_DATA_ELEM(result, op, type, x, y) \
{ \
    type x_value; \
    type y_value; \
    x_value = *((type *) x); \
    y_value = *((type *) y); \
    \
    H5Q_APPLY_MATCH_OP(result, op, x_value, y_value) \
}

#define H5Q_CMP_TYPE(type1, type2, native_type) \
    (0 == H5T_cmp(type1, native_type, FALSE)) || \
    (0 == H5T_cmp(type2, native_type, FALSE))

/*
 * Singly-linked Tail queue declarations. (from sys/queue.h)
 */
#define H5Q_QUEUE_HEAD(name, type)                                          \
struct name {                                                               \
    struct type *stqh_first;    /* first element */                         \
    struct type **stqh_last;    /* addr of last next element */             \
    size_t n_elem;              /* number of elements */                    \
}

#define H5Q_QUEUE_HEAD_INITIALIZER(head)                                    \
    { NULL, &(head).stqh_first, 0 }

#define H5Q_QUEUE_ENTRY(type)                                               \
struct {                                                                    \
    struct type *stqe_next; /* next element */                              \
}

/*
 * Singly-linked Tail queue functions.
 */
#define H5Q_QUEUE_INIT(head) do {                                           \
    (head)->stqh_first = NULL;                                              \
    (head)->stqh_last = &(head)->stqh_first;                                \
    (head)->n_elem = 0;                                                     \
} while (/*CONSTCOND*/0)

#define H5Q_QUEUE_INSERT_HEAD(head, elm, field) do {                        \
    if (((elm)->field.stqe_next = (head)->stqh_first) == NULL)              \
        (head)->stqh_last = &(elm)->field.stqe_next;                        \
    (head)->stqh_first = (elm);                                             \
    (head)->n_elem++;                                                       \
} while (/*CONSTCOND*/0)

#define H5Q_QUEUE_INSERT_TAIL(head, elm, field) do {                        \
    (elm)->field.stqe_next = NULL;                                          \
    *(head)->stqh_last = (elm);                                             \
    (head)->stqh_last = &(elm)->field.stqe_next;                            \
    (head)->n_elem++;                                                       \
} while (/*CONSTCOND*/0)

#define H5Q_QUEUE_REMOVE_HEAD(head, field) do {                             \
    if (((head)->stqh_first = (head)->stqh_first->field.stqe_next) == NULL) { \
        (head)->stqh_last = &(head)->stqh_first;                            \
        (head)->n_elem--;                                                   \
    }                                                                       \
} while (/*CONSTCOND*/0)

#define H5Q_QUEUE_REMOVE(head, elm, type, field) do {                       \
    if ((head)->stqh_first == (elm)) {                                      \
        H5Q_QUEUE_REMOVE_HEAD((head), field);                               \
    } else {                                                                \
        struct type *curelm = (head)->stqh_first;                           \
        while (curelm->field.stqe_next != (elm))                            \
            curelm = curelm->field.stqe_next;                               \
        if ((curelm->field.stqe_next =                                      \
            curelm->field.stqe_next->field.stqe_next) == NULL)              \
                (head)->stqh_last = &(curelm)->field.stqe_next;             \
        (head)->n_elem--;                                                   \
    }                                                                       \
} while (/*CONSTCOND*/0)

#define H5Q_QUEUE_FOREACH(var, head, field)                                 \
    for ((var) = ((head)->stqh_first);                                      \
        (var);                                                              \
        (var) = ((var)->field.stqe_next))

#define H5Q_QUEUE_CONCAT(head1, head2) do {                                 \
    if (!H5Q_QUEUE_EMPTY((head2))) {                                        \
        *(head1)->stqh_last = (head2)->stqh_first;                          \
        (head1)->stqh_last = (head2)->stqh_last;                            \
        (head1)->n_elem += (head2)->n_elem;                                 \
        H5Q_QUEUE_INIT((head2));                                            \
    }                                                                       \
} while (/*CONSTCOND*/0)

/*
 * Singly-linked Tail queue access methods.
 */
#define H5Q_QUEUE_EMPTY(head)       ((head)->stqh_first == NULL)
#define H5Q_QUEUE_FIRST(head)       ((head)->stqh_first)
#define H5Q_QUEUE_NEXT(elm, field)  ((elm)->field.stqe_next)

#define H5Q_VIEW_INITIALIZER(view) \
    {H5Q_QUEUE_HEAD_INITIALIZER(view.reg_refs), H5Q_QUEUE_HEAD_INITIALIZER(view.obj_refs), H5Q_QUEUE_HEAD_INITIALIZER(view.attr_refs)}

#define H5Q_VIEW_REF_NTYPES      3          /* number of reference types */
#define H5Q_VIEW_CORE_INCREMENT  1024       /* increment for core VFD */

//#define H5Q_DEBUG

#ifdef H5Q_DEBUG
#define H5Q_LOG_DEBUG(...) do {                                 \
      fprintf(stdout, " # %s(): ", __func__);                   \
      fprintf(stdout, __VA_ARGS__);                             \
      fprintf(stdout, "\n");                                    \
      fflush(stdout);                                           \
  } while (0)
#else
#define H5Q_LOG_DEBUG(...) do { \
  } while (0)
#endif

/******************/
/* Local Typedefs */
/******************/
typedef enum H5Q_match_type_t { /* The different kinds of native types we can match */
    H5Q_NATIVE_INT_MATCH_CHAR,
    H5Q_NATIVE_INT_MATCH_SHORT,
    H5Q_NATIVE_INT_MATCH_INT,
    H5Q_NATIVE_INT_MATCH_LONG,
    H5Q_NATIVE_INT_MATCH_LLONG,
    H5Q_NATIVE_FLOAT_MATCH_FLOAT,
    H5Q_NATIVE_FLOAT_MATCH_DOUBLE,
    H5Q_INVALID_MATCH_TYPE
} H5Q_match_type_t;

typedef struct H5Q_ref_entry_t H5Q_ref_entry_t;

struct H5Q_ref_entry_t {
    href_t ref;
    H5Q_QUEUE_ENTRY(H5Q_ref_entry_t) entry;
};

typedef H5Q_QUEUE_HEAD(H5Q_ref_head_t, H5Q_ref_entry_t) H5Q_ref_head_t;

typedef struct {
    H5Q_ref_head_t reg_refs;
    H5Q_ref_head_t obj_refs;
    H5Q_ref_head_t attr_refs;
} H5Q_view_t;

typedef struct {
    const H5Q_t *query;
    unsigned *result;
    H5Q_view_t *view;
} H5Q_apply_arg_t;

typedef struct {
    const char *filename;
    const char *loc_name;
    H5G_loc_t *obj_loc;
    H5Q_apply_arg_t *apply_args;
} H5Q_apply_attr_arg_t;

typedef struct {
    const H5Q_t *query;
    hbool_t *result;
} H5Q_apply_attr_elem_arg_t;

/********************/
/* Local Prototypes */
/********************/
static herr_t H5Q__apply_atom(const H5Q_t *query, hbool_t *result, va_list ap);
static herr_t H5Q__apply_data_elem(const H5Q_t *query, hbool_t *result,
    const H5T_t *type, const void *elem);
static H5T_t *H5Q__promote_type(const H5T_t *type1, const H5T_t *type2,
    H5Q_match_type_t *match_type);
static herr_t H5Q__apply_attr_name(const H5Q_t *query, hbool_t *result,
    const char *name);
static herr_t H5Q__apply_link_name(const H5Q_t *query, hbool_t *result,
    const char *name);

static herr_t H5Q__apply_iterate(hid_t oid, const char *name,
    const H5O_info_t *oinfo, void *udata);
static herr_t H5Q__apply_object(hid_t oid, const char *name,
    const H5O_info_t *oinfo, void *udata);
static herr_t H5Q__apply_object_link(H5G_loc_t *loc, const char *name,
    const H5O_info_t *oinfo, void *udata);
static herr_t H5Q__apply_object_data(H5G_loc_t *loc, const char *name,
    const H5O_info_t *oinfo, void *udata);
static herr_t H5Q__apply_object_attr(H5G_loc_t *loc, const char *name,
    void *udata);
static herr_t H5Q__apply_object_attr_iterate(H5A_t *attr, void *udata);
static herr_t H5Q__apply_object_attr_name(H5A_t *attr, void *udata);
static herr_t H5Q__apply_object_attr_value(H5A_t *attr, void *udata);
static herr_t H5Q__apply_object_attr_value_iterate(void *elem, const H5T_t *type,
    unsigned H5_ATTR_UNUSED ndim, const hsize_t H5_ATTR_UNUSED *point, void *udata);

static herr_t H5Q__view_append(H5Q_view_t *view, H5R_type_t ref_type, void *ref);
static herr_t H5Q__view_combine(H5Q_combine_op_t combine_op, H5Q_view_t *view1, H5Q_view_t *view2,
    unsigned result1, unsigned result2, H5Q_view_t *view, unsigned *result);
static herr_t H5Q__view_write(H5G_t *grp, H5Q_view_t *view);
static herr_t H5Q__view_free(H5Q_view_t *view);


static H5_INLINE void
H5Q__encode_memcpy(unsigned char **buf_ptr, size_t *nalloc, const void *data,
        size_t data_size)
{
    if (*buf_ptr != NULL) {
        HDmemcpy(*buf_ptr, data, data_size);
        *buf_ptr += data_size;
    }
    *nalloc += data_size;
}

static H5_INLINE void
H5Q__decode_memcpy(void *data, size_t data_size, const unsigned char **buf_ptr)
{
    if (*buf_ptr != NULL) {
        HDmemcpy(data, *buf_ptr, data_size);
        *buf_ptr += data_size;
    }
}

/*********************/
/* Package Variables */
/*********************/

/* Package initialization variable */
hbool_t H5_PKG_INIT_VAR = FALSE;

/*****************************/
/* Library Private Variables */
/*****************************/

/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage the H5Q_t struct */
H5FL_DEFINE(H5Q_t);

/* Query ID class */
static const H5I_class_t H5I_QUERY_CLS[1] = {{
    H5I_QUERY,             /* Class ID for the type */
    0,                     /* Class behavior flags */
    0,                     /* Number of reserved IDs for this type */
    (H5I_free_t)H5Q_close  /* Free function for object's of this type */
}};

/* Flag indicating "top" of interface has been initialized */
static hbool_t H5Q_top_package_initialize_s = FALSE;

/*-------------------------------------------------------------------------
 * Function:    H5Q_init
 *
 * Purpose:	Initialize the interface from some other package.
 *
 * Return:  Success:    non-negative
 *          Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Q_init(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)
    /* FUNC_ENTER() does all the work */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q_init() */

/*--------------------------------------------------------------------------
NAME
   H5Q__init_package -- Initialize interface-specific information
USAGE
    herr_t H5Q__init_package()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
herr_t
H5Q__init_package(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_PACKAGE

    /* Initialize the atom group for the QUERY IDs */
    if (FAIL == H5I_register_type(H5I_QUERY_CLS))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTINIT, FAIL, "unable to initialize interface");

    /* Mark "top" of interface as initialized, too */
    H5Q_top_package_initialize_s = TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q__init_package() */

/*--------------------------------------------------------------------------
 NAME
    H5Q_top_term_package
 PURPOSE
    Terminate various H5Q objects
 USAGE
    void H5Q_top_term_package()
 RETURNS
 DESCRIPTION
    Release IDs for the atom group, deferring full interface shutdown
    until later (in H5Q_term_package).
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
int
H5Q_top_term_package(void)
{
    int n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if (H5Q_top_package_initialize_s) {
        if (H5I_nmembers(H5I_QUERY) > 0) {
            (void)H5I_clear_type(H5I_QUERY, FALSE, FALSE);
            n++; /*H5I*/
        } /* end if */

        /* Mark closed */
        if (0 == n)
            H5Q_top_package_initialize_s = FALSE;
    } /* end if */

    FUNC_LEAVE_NOAPI(n)
} /* H5Q_top_term_package() */

/*--------------------------------------------------------------------------
 NAME
    H5Q_term_package
 PURPOSE
    Terminate various H5Q objects
 USAGE
    void H5Q_term_package()
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Release the atom group and any other resources allocated.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
int
H5Q_term_package(void)
{
    int n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if (H5_PKG_INIT_VAR) {
        /* Sanity checks */
        HDassert(0 == H5I_nmembers(H5I_QUERY));

        /* Destroy the query object id group */
        n += (H5I_dec_type_ref(H5I_QUERY) > 0);

        /* Mark closed */
        if (0 == n)
            H5_PKG_INIT_VAR = FALSE;
    } /* end if */

    FUNC_LEAVE_NOAPI(n)
} /* end H5Q_term_package() */

/*-------------------------------------------------------------------------
 * Function:    H5Qcreate
 *
 * Purpose: Create a new query object of query_type type, with match_op
 * determining the query's match condition and additional parameters
 * determined by the type of the query.
 *
 * Return:  Success:    The ID for a new query.
 *          Failure:    FAIL
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Qcreate(H5Q_type_t query_type, H5Q_match_op_t match_op, ...)
{
    H5Q_t *query = NULL;
    va_list ap;
    hid_t ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE2("i", "QtQm", query_type, match_op);

    va_start(ap, match_op);

    switch (query_type) {
        case H5Q_TYPE_DATA_ELEM:
        case H5Q_TYPE_ATTR_VALUE:
        {
            H5T_t *datatype = NULL;
            hid_t datatype_id;
            const void *value;

            /* Get arguments */
            datatype_id = va_arg(ap, hid_t);
            value = va_arg(ap, const void *);

            /* Get type */
            if (NULL == (datatype = (H5T_t *) H5I_object_verify(datatype_id, H5I_DATATYPE)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");

            /* Create a new query object */
            if (NULL == (query = H5Q_create(query_type, match_op, datatype, value)))
                HGOTO_ERROR(H5E_QUERY, H5E_CANTCREATE, FAIL, "unable to create query");
        }
        break;
        case H5Q_TYPE_ATTR_NAME:
        {
            const char *attr_name;

            /* Get arguments */
            attr_name = va_arg(ap, const char *);

            /* Create a new query object */
            if (NULL == (query = H5Q_create(query_type, match_op, attr_name)))
                HGOTO_ERROR(H5E_QUERY, H5E_CANTCREATE, FAIL, "unable to create query");
        }
        break;
        case H5Q_TYPE_LINK_NAME:
        {
            const char *link_name;

            /* Get arguments */
            link_name = va_arg(ap, const char *);

            /* Create a new query object */
            if (NULL == (query = H5Q_create(query_type, match_op, link_name)))
                HGOTO_ERROR(H5E_QUERY, H5E_CANTCREATE, FAIL, "unable to create query");
        }
        break;
        case H5Q_TYPE_MISC:
        default:
        {
            HGOTO_ERROR(H5E_QUERY, H5E_BADTYPE, FAIL, "unsupported/unrecognized query type");
        }
        break;
    }

    /* Register the new query object to get an ID for it */
    if (FAIL == (query->query_id = H5I_register(H5I_QUERY, query, TRUE)))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTREGISTER, FAIL, "can't register query handle");
    ret_value = query->query_id;

done:
    va_end(ap);

    FUNC_LEAVE_API(ret_value)
} /* end H5Qcreate() */

/*-------------------------------------------------------------------------
 * Function:    H5Q_create
 *
 * Purpose: Private function for H5Qcreate
 *
 * Return:  Success:    Pointer to the new query
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
H5Q_t *
H5Q_create(H5Q_type_t query_type, H5Q_match_op_t match_op, ...)
{
    H5Q_t *query = NULL;
    va_list ap;
    H5Q_t *ret_value = NULL; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    va_start(ap, match_op);

    /* Allocate query struct */
    if (NULL == (query = H5FL_CALLOC(H5Q_t)))
        HGOTO_ERROR(H5E_QUERY, H5E_NOSPACE, NULL, "can't allocate query structure");

    query->is_combined = FALSE;
    query->query_id = H5I_UNINIT;
    query->ref_count = 1;
    query->query.select.type = query_type;
    query->query.select.match_op = match_op;
    switch (query_type) {
        case H5Q_TYPE_DATA_ELEM:
        case H5Q_TYPE_ATTR_VALUE:
            {
                H5T_t *datatype = NULL;
                H5T_t *native_datatype = NULL;
                size_t datatype_size;
                const void *value;
                void *value_buf = NULL;

                datatype = va_arg(ap, H5T_t*);
                value = va_arg(ap, const void *);

                /* Only use native type */
                if (NULL == (native_datatype = H5T_get_native_type(datatype, H5T_DIR_DEFAULT, NULL, NULL, NULL)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "cannot retrieve native type");
                query->query.select.elem.data_elem.type = native_datatype;
                if (0 == (datatype_size = H5T_get_size(native_datatype)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a valid size");
                query->query.select.elem.data_elem.type_size = datatype_size;
                if (NULL == (value_buf = H5MM_malloc(datatype_size)))
                    HGOTO_ERROR(H5E_QUERY, H5E_CANTALLOC, NULL,
                            "can't allocate value buffer");
                HDmemcpy(value_buf, value, datatype_size);
                query->query.select.elem.data_elem.value = value_buf;
            }
            break;
        case H5Q_TYPE_ATTR_NAME:
            {
                const char *attr_name;

                attr_name = va_arg(ap, const char *);
                query->query.select.elem.attr_name.name = H5MM_strdup(attr_name);
            }
            break;
        case H5Q_TYPE_LINK_NAME:
            {
                const char *link_name;

                link_name = va_arg(ap, const char *);
                query->query.select.elem.link_name.name = H5MM_strdup(link_name);
            }
            break;
        case H5Q_TYPE_MISC:
        default:
            HGOTO_ERROR(H5E_QUERY, H5E_BADTYPE, NULL, "unsupported/unrecognized query type");
            break;
    } /* end switch */

    /* set return value */
    ret_value = query;

done:
    if (!ret_value && query)
        query = H5FL_FREE(H5Q_t, query);

    va_end(ap);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q_create() */

/*-------------------------------------------------------------------------
 * Function:    H5Qclose
 *
 * Purpose: The H5Qclose routine terminates access to a query object,
 * given by query_id.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Qclose(hid_t query_id)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", query_id);

    /* Check args */
    if (NULL == H5I_object_verify(query_id, H5I_QUERY))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a query ID");

    if (FAIL == H5I_dec_app_ref(query_id))
    	HGOTO_ERROR(H5E_QUERY, H5E_CANTDEC, FAIL, "unable to decrement ref count on query");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Qclose() */

/*-------------------------------------------------------------------------
 * Function:    H5Q_close
 *
 * Purpose: Private function for H5Qclose.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Q_close(H5Q_t *query)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(query);

    if (--query->ref_count)
        HGOTO_DONE(SUCCEED)

    if (query->is_combined) {
        if (FAIL == H5Q_close(query->query.combine.l_query))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTFREE, FAIL, "unable to free query");
            query->query.combine.l_query = NULL;
        if (FAIL == H5Q_close(query->query.combine.r_query))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTFREE, FAIL, "unable to free query");
            query->query.combine.r_query = NULL;
    } else {
        switch (query->query.select.type) {
            case H5Q_TYPE_DATA_ELEM:
            case H5Q_TYPE_ATTR_VALUE:
                if (FAIL == H5T_close(query->query.select.elem.data_elem.type))
                    HGOTO_ERROR(H5E_QUERY, H5E_CANTFREE, FAIL, "unable to free datatype");
                query->query.select.elem.data_elem.type = NULL;
                H5MM_free(query->query.select.elem.data_elem.value);
                query->query.select.elem.data_elem.value = NULL;
                break;
            case H5Q_TYPE_ATTR_NAME:
                H5MM_free(query->query.select.elem.attr_name.name);
                query->query.select.elem.attr_name.name = NULL;
                break;
            case H5Q_TYPE_LINK_NAME:
                H5MM_free(query->query.select.elem.link_name.name);
                query->query.select.elem.link_name.name = NULL;
                break;
            case H5Q_TYPE_MISC:
            default:
                HGOTO_ERROR(H5E_QUERY, H5E_BADTYPE, FAIL, "unsupported/unrecognized query type");
                break;
        }
    }

    /* Free the query struct */
    query = H5FL_FREE(H5Q_t, query);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q_close() */

/*-------------------------------------------------------------------------
 * Function:    H5Qcombine
 *
 * Purpose: The H5Qcombine routine creates a new compound query object by
 * combining two query objects (given by query1 and query2), using the
 * combination operator combine_op. Valid combination operators are:
 *      - H5Q_COMBINE_AND
 *      - H5Q_COMBINE_OR
 *
 * Return:  Success:    The ID for a new combined query.
 *          Failure:    FAIL
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Qcombine(hid_t query1_id, H5Q_combine_op_t combine_op, hid_t query2_id)
{
    H5Q_t *query = NULL, *query1 = NULL, *query2 = NULL;
    hid_t ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("i", "iQci", query1_id, combine_op, query2_id);

    /* Check args and get the query objects */
    if (NULL == (query1 = (H5Q_t *) H5I_object_verify(query1_id, H5I_QUERY)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a query ID");
    if (NULL == (query2 = (H5Q_t *) H5I_object_verify(query2_id, H5I_QUERY)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a query ID");

    /* Combine query objects */
    if (NULL == (query = H5Q_combine(query1, combine_op, query2)))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTCREATE, FAIL, "unable to combine query objects");

    /* Register the new query object to get an ID for it */
    if (FAIL == (query->query_id = H5I_register(H5I_QUERY, query, TRUE)))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTREGISTER, FAIL, "can't register query handle");
    ret_value = query->query_id;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Qcombine() */

/*-------------------------------------------------------------------------
 * Function:    H5Q_combine
 *
 * Purpose: Private function for H5Qcombine.
 *
 * Return:  Success:    Pointer to the new query
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
H5Q_t *
H5Q_combine(H5Q_t *query1, H5Q_combine_op_t combine_op, H5Q_t *query2)
{
    H5Q_t *query = NULL;
    H5Q_type_t type1, type2;
    H5Q_t *ret_value = NULL; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Allocate query struct */
    if (NULL == (query = H5FL_CALLOC(H5Q_t)))
        HGOTO_ERROR(H5E_QUERY, H5E_NOSPACE, NULL, "can't allocate query structure");

    switch (combine_op) {
        case H5Q_COMBINE_AND:
        case H5Q_COMBINE_OR:
            query->query.combine.op = combine_op;
            break;
        case H5Q_SINGLETON:
        default:
            HGOTO_ERROR(H5E_QUERY, H5E_BADTYPE, NULL, "unsupported/unrecognized combine op");
            break;
    }
    query->is_combined = TRUE;
    query->query_id = H5I_UNINIT;
    query->ref_count = 1;
    query->query.combine.l_query = query1;
    query1->ref_count++;
    query->query.combine.r_query = query2;
    query2->ref_count++;
    /* Work out query type of the combined query */
    type1 = (query1->is_combined) ? query1->query.combine.type : query1->query.select.type;
    type2 = (query2->is_combined) ? query2->query.combine.type : query2->query.select.type;
    query->query.combine.type = (type1 == type2) ? type1 : H5Q_TYPE_MISC;

    /* set return value */
    ret_value = query;

done:
    if (!ret_value && query)
        query = H5FL_FREE(H5Q_t, query);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q_combine() */

/*-------------------------------------------------------------------------
 * Function:    H5Qget_type
 *
 * Purpose: The H5Qget_type routine queries a query object,
 * given by query_id, for its type information, originally provided to
 * H5Qcreate or created after a call to H5Qcombine. Information is returned
 * through the query_type parameter. See H5Qcreate for a table listing the
 * complete set of values that may be returned for query_type.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Qget_type(hid_t query_id, H5Q_type_t *query_type)
{
    H5Q_t *query = NULL;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*Qt", query_id, query_type);

    /* Check args and get the query objects */
    if (NULL == (query = (H5Q_t *) H5I_object_verify(query_id, H5I_QUERY)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a query ID");
    if (!query_type)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL pointer to query type");

    /* Get match info */
    if (FAIL == H5Q_get_type(query, query_type))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTGET, FAIL, "unable to get query type");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Qget_type() */

/*-------------------------------------------------------------------------
 * Function:    H5Q_get_type
 *
 * Purpose: Private function for H5Qget_type.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Q_get_type(const H5Q_t *query, H5Q_type_t *query_type)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(query);
    HDassert(query_type);

    *query_type = (query->is_combined) ? query->query.combine.type :
            query->query.select.type;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q_get_type() */


/*-------------------------------------------------------------------------
 * Function:    H5Qget_match_op
 *
 * Purpose: The H5Qget_match_op routine queries a singleton query object,
 * given by query_id, for its op information, originally provided to
 * H5Qcreate. Match information is returned through the
 * match_op parameter. See H5Qcreate for a table listing the complete set of
 * values that may be returned for match_op.
 * It is an error to perform this call on a compound query object (one which
 * was created with H5Qcombine).
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Qget_match_op(hid_t query_id, H5Q_match_op_t *match_op)
{
    H5Q_t *query = NULL;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*Qm", query_id, match_op);

    /* Check args and get the query objects */
    if (NULL == (query = (H5Q_t *) H5I_object_verify(query_id, H5I_QUERY)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a query ID");
    if (!match_op)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL pointer to match op");

    /* Get match info */
    if (FAIL == H5Q_get_match_op(query, match_op))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTGET, FAIL, "unable to get match op");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Qget_match_op() */

/*-------------------------------------------------------------------------
 * Function:    H5Q_get_match_op
 *
 * Purpose: Private function for H5Qget_match_op.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Q_get_match_op(const H5Q_t *query, H5Q_match_op_t *match_op)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(query);
    HDassert(match_op);

    if (query->is_combined)
        HGOTO_ERROR(H5E_QUERY, H5E_CANTGET, FAIL, "cannot retrieve op from combined query");

    *match_op = query->query.select.match_op;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q_get_match_op() */

/*-------------------------------------------------------------------------
 * Function:    H5Qget_components
 *
 * Purpose: The H5Qget_components routine queries a compound query object,
 * given by query_id, for its component queries.  The component queries are
 * returned in sub_query1_id and sub_query2_id, both of which must be closed
 * with H5Qclose.
 * It is an error to apply H5Qget_components to a singleton query object (one
 * which was created with H5Qcreate).
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Qget_components(hid_t query_id, hid_t *sub_query1_id, hid_t *sub_query2_id)
{
    H5Q_t *query = NULL, *sub_query1 = NULL, *sub_query2 = NULL;
    hid_t _sub_query1_id, _sub_query2_id;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "i*i*i", query_id, sub_query1_id, sub_query2_id);

    /* Check args and get the query objects */
    if (NULL == (query = (H5Q_t *) H5I_object_verify(query_id, H5I_QUERY)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a query ID");
    if (!sub_query1_id || !sub_query2_id)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL pointer to query_id");

    /* Get components */
    if (FAIL == H5Q_get_components(query, &sub_query1, &sub_query2))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTGET, FAIL, "unable to get components");

    /* Register the type and return the ID */
    if (FAIL == (_sub_query1_id = H5I_register(H5I_QUERY, sub_query1, TRUE)))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTREGISTER, FAIL, "unable to register query");
    if (FAIL == (_sub_query2_id = H5I_register(H5I_QUERY, sub_query2, TRUE)))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTREGISTER, FAIL, "unable to register query");

    *sub_query1_id = _sub_query1_id;
    *sub_query2_id = _sub_query2_id;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Qget_components() */

/*-------------------------------------------------------------------------
 * Function:    H5Q_get_components
 *
 * Purpose: Private function for H5Qget_components.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Q_get_components(const H5Q_t *query, H5Q_t **sub_query1, H5Q_t **sub_query2)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(query);
    HDassert(sub_query1);
    HDassert(sub_query2);

    if (!query->is_combined)
        HGOTO_ERROR(H5E_QUERY, H5E_CANTGET, FAIL, "not a combined query");

    *sub_query1 = query->query.combine.l_query;
    query->query.combine.l_query->ref_count++;
    *sub_query2 = query->query.combine.r_query;
    query->query.combine.r_query->ref_count++;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q_get_components() */

/*-------------------------------------------------------------------------
 * Function:    H5Qget_combine_op
 *
 * Purpose: The H5Qget_combine_op routine queries a query object, given by
 * query_id, for its operator type. The possible operator types returned are:
 *      - H5Q_SINGLETON
 *      - H5Q_COMBINE_AND
 *      - H5Q_COMBINE_OR
 * H5Q_COMBINE_AND and H5Q_COMBINE_OR are only returned for query objects
 * produced with H5Qcombine and H5Q_SINGLETON is returned for query objects
 * produced with H5Qcreate.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Qget_combine_op(hid_t query_id, H5Q_combine_op_t *op_type)
{
    H5Q_t *query = NULL;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*Qc", query_id, op_type);

    /* Check args and get the query objects */
    if (NULL == (query = (H5Q_t *) H5I_object_verify(query_id, H5I_QUERY)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a query ID");
    if (!op_type)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL pointer to op type");

    /* Get combine op */
    if (FAIL == H5Q_get_combine_op(query, op_type))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTGET, FAIL, "unable to get combine op");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Qget_combine_op() */

/*-------------------------------------------------------------------------
 * Function:    H5Q_get_combine_op
 *
 * Purpose: Private function for H5Qget_combine_op.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Q_get_combine_op(const H5Q_t *query, H5Q_combine_op_t *op_type)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(query);
    HDassert(op_type);

    if (!query->is_combined)
        *op_type = H5Q_SINGLETON;
    else
        *op_type = query->query.combine.op;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q_get_combine_op() */

/*-------------------------------------------------------------------------
 * Function:    H5Qencode
 *
 * Purpose: Given a query ID, serialize the query into buf.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Qencode(hid_t query_id, void *buf, size_t *nalloc)
{
    H5Q_t *query = NULL;
    herr_t ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "i*x*z", query_id, buf, nalloc);

    /* Check argument and retrieve object */
    if (NULL == (query = (H5Q_t *)H5I_object_verify(query_id, H5I_QUERY)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a query ID");
    if (nalloc == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL pointer for buffer size");

    /* Encode the query */
    if (FAIL == (ret_value = H5Q_encode(query, (unsigned char *)buf, nalloc)))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTENCODE, FAIL, "can't encode query");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Qencode() */

/*-------------------------------------------------------------------------
 * Function:    H5Q_encode
 *
 * Purpose: Private function for H5Qencode.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Q_encode(const H5Q_t *query, unsigned char *buf, size_t *nalloc)
{
    size_t buf_size = 0;
    herr_t ret_value = SUCCEED;
    unsigned char *buf_ptr = buf;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(query);
    HDassert(nalloc);

    H5Q__encode_memcpy(&buf_ptr, &buf_size, &query->is_combined, sizeof(hbool_t));
    if (query->is_combined) {
        size_t l_buf_size = 0, r_buf_size = 0;

        H5Q__encode_memcpy(&buf_ptr, &buf_size, &query->query.combine.type, sizeof(H5Q_type_t));
        H5Q__encode_memcpy(&buf_ptr, &buf_size, &query->query.combine.op, sizeof(H5Q_combine_op_t));
        H5Q_encode(query->query.combine.l_query, buf_ptr, &l_buf_size);
        buf_size += l_buf_size;
        if (buf_ptr) buf_ptr += l_buf_size;
        H5Q_encode(query->query.combine.r_query, buf_ptr, &r_buf_size);
        buf_size += r_buf_size;
        if (buf_ptr) buf_ptr += r_buf_size;
    } else {
        H5Q__encode_memcpy(&buf_ptr, &buf_size, &query->query.select.type, sizeof(H5Q_type_t));
        H5Q__encode_memcpy(&buf_ptr, &buf_size, &query->query.select.match_op, sizeof(H5Q_match_op_t));
        switch (query->query.select.type) {
            case H5Q_TYPE_DATA_ELEM:
            case H5Q_TYPE_ATTR_VALUE:
            {
                size_t type_id_nalloc = 0;
                H5T_t *type = query->query.select.elem.data_elem.type;
                size_t type_size = query->query.select.elem.data_elem.type_size;
                void *value_buf = query->query.select.elem.data_elem.value;

                if (FAIL == H5T_encode(type, NULL, &type_id_nalloc))
                    HGOTO_ERROR(H5E_QUERY, H5E_CANTENCODE, FAIL, "can't get encoding size for datatype");
                H5Q__encode_memcpy(&buf_ptr, &buf_size, &type_id_nalloc, sizeof(size_t));
                if (FAIL == H5T_encode(type, buf_ptr, &type_id_nalloc))
                    HGOTO_ERROR(H5E_QUERY, H5E_CANTENCODE, FAIL, "can't encode datatype");
                buf_size += type_id_nalloc;
                if (buf_ptr) buf_ptr += type_id_nalloc;
                H5Q__encode_memcpy(&buf_ptr, &buf_size, &type_size, sizeof(size_t));
                H5Q__encode_memcpy(&buf_ptr, &buf_size, value_buf, type_size);
            }
                break;
            case H5Q_TYPE_ATTR_NAME:
            {
                size_t name_len = HDstrlen(query->query.select.elem.attr_name.name) + 1;
                char *name = query->query.select.elem.attr_name.name;

                H5Q__encode_memcpy(&buf_ptr, &buf_size, &name_len, sizeof(size_t));
                H5Q__encode_memcpy(&buf_ptr, &buf_size, name, name_len);
            }
                break;
            case H5Q_TYPE_LINK_NAME:
            {
                size_t name_len = HDstrlen(query->query.select.elem.link_name.name) + 1;
                char *name = query->query.select.elem.attr_name.name;

                H5Q__encode_memcpy(&buf_ptr, &buf_size, &name_len, sizeof(size_t));
                H5Q__encode_memcpy(&buf_ptr, &buf_size, name, name_len);
            }
                break;
            case H5Q_TYPE_MISC:
            default:
                HGOTO_ERROR(H5E_QUERY, H5E_BADTYPE, FAIL, "unsupported/unrecognized query type");
                break;
        }
    }

    *nalloc = buf_size;

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q_encode() */

/*-------------------------------------------------------------------------
 * Function:    H5Qdecode
 *
 * Purpose: Deserialize the buffer and return a new query handle. The handle
 * must be closed using H5Qclose.
 *
 * Return:  Success:    query ID (non-negative)
 *
 *          Failure:    negative
 *-------------------------------------------------------------------------
 */
hid_t
H5Qdecode(const void *buf)
{
    H5Q_t *query;
    hid_t ret_value;      /* Return value */
    const unsigned char *buf_ptr = (const unsigned char *) buf;

    FUNC_ENTER_API(FAIL)
    H5TRACE1("i", "*x", buf);

    /* Check args */
    if (buf == NULL) HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "empty buffer");

    /* Create datatype by decoding buffer */
    if (NULL == (query = H5Q_decode(&buf_ptr)))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTDECODE, FAIL, "can't decode object");

    /* Register the type and return the ID */
    if (FAIL == (query->query_id = H5I_register(H5I_QUERY, query, TRUE)))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTREGISTER, FAIL, "unable to register query");
    ret_value = query->query_id;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Qdecode() */

/*-------------------------------------------------------------------------
 * Function:    H5Q_decode
 *
 * Purpose: Private function for H5Qdecode.
 *
 * Return:  Success:    Pointer to the decoded query
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
H5Q_t *
H5Q_decode(const unsigned char **buf_ptr)
{
    H5Q_t *ret_value = NULL;
    H5Q_t *query = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(*buf_ptr);

    /* Allocate query struct */
    if (NULL == (query = H5FL_CALLOC(H5Q_t)))
        HGOTO_ERROR(H5E_QUERY, H5E_NOSPACE, NULL, "can't allocate query structure");

    /* Set ref count */
    query->query_id = H5I_UNINIT;
    query->ref_count = 1;

    H5Q__decode_memcpy(&query->is_combined, sizeof(hbool_t), buf_ptr);
    if (query->is_combined) {
        H5Q__decode_memcpy(&query->query.combine.type, sizeof(H5Q_type_t), buf_ptr);
        H5Q__decode_memcpy(&query->query.combine.op, sizeof(H5Q_combine_op_t), buf_ptr);
        query->query.combine.l_query = H5Q_decode(buf_ptr);
        query->query.combine.r_query = H5Q_decode(buf_ptr);
    } else {
        H5Q__decode_memcpy(&query->query.select.type, sizeof(H5Q_type_t), buf_ptr);
        H5Q__decode_memcpy(&query->query.select.match_op, sizeof(H5Q_match_op_t), buf_ptr);
        switch (query->query.select.type) {
            case H5Q_TYPE_DATA_ELEM:
            case H5Q_TYPE_ATTR_VALUE:
            {
                size_t type_id_nalloc = 0;
                H5T_t *type;
                size_t type_size = 0;
                void *value_buf;

                H5Q__decode_memcpy(&type_id_nalloc, sizeof(size_t), buf_ptr);
                if (NULL == (type = H5T_decode(*buf_ptr)))
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, NULL, "can't decode datatype");
                query->query.select.elem.data_elem.type = type;
                *buf_ptr += type_id_nalloc;

                H5Q__decode_memcpy(&type_size, sizeof(size_t), buf_ptr);
                if (!type_size)
                    HGOTO_ERROR(H5E_QUERY, H5E_BADVALUE, NULL, "NULL type size");
                query->query.select.elem.data_elem.type_size = type_size;
                if (NULL == (value_buf = H5MM_malloc(type_size)))
                    HGOTO_ERROR(H5E_QUERY, H5E_CANTALLOC, NULL, "can't allocate value buffer");
                H5Q__decode_memcpy(value_buf, type_size, buf_ptr);
                query->query.select.elem.data_elem.value = value_buf;
            }
            break;
            case H5Q_TYPE_ATTR_NAME:
            {
                size_t name_len = 0;
                char *name;
                
                H5Q__decode_memcpy(&name_len, sizeof(size_t), buf_ptr);
                if (!name_len)
                    HGOTO_ERROR(H5E_QUERY, H5E_BADVALUE, NULL, "NULL name len");
                if (NULL == (name = (char *)H5MM_malloc(name_len)))
                    HGOTO_ERROR(H5E_QUERY, H5E_CANTALLOC, NULL,
                            "can't allocate value buffer");
                H5Q__decode_memcpy(name, name_len, buf_ptr);
                query->query.select.elem.attr_name.name = name;
            }
            break;
            case H5Q_TYPE_LINK_NAME:
            {
                size_t name_len = 0;
                char *name;

                H5Q__decode_memcpy(&name_len, sizeof(size_t), buf_ptr);
                if (!name_len)
                    HGOTO_ERROR(H5E_QUERY, H5E_BADVALUE, NULL, "NULL name len");
                if (NULL == (name = (char *)H5MM_malloc(name_len)))
                    HGOTO_ERROR(H5E_QUERY, H5E_CANTALLOC, NULL,
                            "can't allocate value buffer");
                H5Q__decode_memcpy(name, name_len, buf_ptr);
                query->query.select.elem.link_name.name = name;
            }
            break;
            case H5Q_TYPE_MISC:
            default:
                HGOTO_ERROR(H5E_QUERY, H5E_BADTYPE, NULL, "unsupported/unrecognized query type");
                break;
        }
    }

    ret_value = query;
done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q_decode() */

/*-------------------------------------------------------------------------
 * Function:    H5Qapply_atom
 *
 * Purpose: Apply a query and return the result. Parameters, which the
 * query applies to, are determined by the type of the query.
 * It is an error to apply H5Qapply_atom to a combined query object of
 * different types.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Qapply_atom(hid_t query_id, hbool_t *result, ...)
{
    H5Q_t *query = NULL;
    H5T_t *native_type = NULL;
    herr_t ret_value;
    va_list ap;

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*b", query_id, result);

    /* Check args and get the query objects */
    if (!result)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL pointer for result");
    if (NULL == (query = (H5Q_t *) H5I_object_verify(query_id, H5I_QUERY)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a query ID");

    if (query->is_combined && (query->query.combine.type == H5Q_TYPE_MISC))
        HGOTO_ERROR(H5E_QUERY, H5E_BADTYPE, FAIL, "cannot apply to misc combined query");

    va_start(ap, result);

    switch (query->query.select.type) {
        case H5Q_TYPE_DATA_ELEM:
        case H5Q_TYPE_ATTR_VALUE:
        {
            H5T_t *type = NULL;
            hid_t type_id;
            const void *value;

            /* Get arguments */
            type_id = va_arg(ap, hid_t);
            value = va_arg(ap, const void *);

            /* Get type */
            if (NULL == (type = (H5T_t *) H5I_object_verify(type_id, H5I_DATATYPE)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");

            /* Only use native type */
            if (NULL == (native_type = H5T_get_native_type(type, H5T_DIR_DEFAULT, NULL, NULL, NULL)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "cannot retrieve native type");

            /* Apply query */
            if (FAIL == (ret_value = H5Q_apply_atom(query, result, native_type, value)))
                HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "unable to apply query");
        }
        break;
        case H5Q_TYPE_ATTR_NAME:
        {
            const char *attr_name;

            /* Get arguments */
            attr_name = va_arg(ap, const char *);

            /* Apply query */
            if (FAIL == (ret_value = H5Q_apply_atom(query, result, attr_name)))
                HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "unable to apply query");
        }
        break;
        case H5Q_TYPE_LINK_NAME:
        {
            const char *link_name;

            /* Get arguments */
            link_name = va_arg(ap, const char *);

            /* Apply query */
            if (FAIL == (ret_value = H5Q_apply_atom(query, result, link_name)))
                HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "unable to apply query");
        }
        break;
        case H5Q_TYPE_MISC:
        default:
        {
            HGOTO_ERROR(H5E_QUERY, H5E_BADTYPE, FAIL, "unsupported/unrecognized query type");
        }
        break;
    }

done:
    if (native_type)
        H5T_close(native_type);

    va_end(ap);

    FUNC_LEAVE_API(ret_value)
} /* end H5Qapply_atom() */

/*-------------------------------------------------------------------------
 * Function:    H5Q_apply_atom
 *
 * Purpose: Private function for H5Qapply_atom.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Q_apply_atom(const H5Q_t *query, hbool_t *result, ...)
{
    herr_t ret_value = SUCCEED; /* Return value */
    va_list ap;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(query);
    HDassert(result);

    va_start(ap, result);

    if (FAIL == H5Q__apply_atom(query, result, ap))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "unable to apply query")

done:
    va_end(ap);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q_apply_atom() */

/*-------------------------------------------------------------------------
 * Function:    H5Q__apply_atom
 *
 * Purpose: Private function for H5Q_apply_atom.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Q__apply_atom(const H5Q_t *query, hbool_t *result, va_list ap)
{
    herr_t ret_value = SUCCEED; /* Return value */
    va_list aq;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(query);
    HDassert(result);
    HDassert(query->is_combined == FALSE);

    va_copy(aq, ap);

    if (query->is_combined) {
        hbool_t result1, result2;

        if (FAIL == H5Q__apply_atom(query->query.combine.l_query, &result1, ap))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "unable to apply query");
        if (FAIL == H5Q__apply_atom(query->query.combine.r_query, &result2, ap))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "unable to apply query");

        switch (query->query.combine.op) {
            case H5Q_COMBINE_AND:
                *result = result1 && result2;
                break;
            case H5Q_COMBINE_OR:
                *result = result1 || result2;
                break;
            case H5Q_SINGLETON:
            default:
                HGOTO_ERROR(H5E_QUERY, H5E_BADTYPE, FAIL, "unsupported/unrecognized combine op");
                break;
        }
    } else {
        switch (query->query.select.type) {
            case H5Q_TYPE_DATA_ELEM:
            case H5Q_TYPE_ATTR_VALUE:
            {
                H5T_t *type = va_arg(aq, H5T_t*);
                const void *elem = va_arg(aq, const void *);

                HDassert(type);
                HDassert(elem);

                if (FAIL == (ret_value = H5Q__apply_data_elem(query, result, type, elem)))
                    HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "unable to apply data element query");
                break;
            }
            case H5Q_TYPE_ATTR_NAME:
            {
                const char *attr_name = va_arg(aq, const char *);

                HDassert(attr_name);

                if (FAIL == (ret_value = H5Q__apply_attr_name(query, result, attr_name)))
                    HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "unable to apply attribute name query");
                break;
            }
            case H5Q_TYPE_LINK_NAME:
            {
                const char *link_name = va_arg(aq, const char *);

                HDassert(link_name);

                if (FAIL == (ret_value = H5Q__apply_link_name(query, result, link_name)))
                    HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "unable to apply link name query");
                break;
            }
            case H5Q_TYPE_MISC:
            default:
                HGOTO_ERROR(H5E_QUERY, H5E_BADTYPE, FAIL, "unsupported/unrecognized query type");
                break;
        }
    }

done:
    va_end(aq);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q__apply_atom() */

/*-------------------------------------------------------------------------
 * Function:    H5Q__apply_data_elem
 *
 * Purpose: Private function for H5Q__apply_atom (data element).
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Q__apply_data_elem(const H5Q_t *query, hbool_t *result, const H5T_t *type, const void *value)
{
    herr_t ret_value = SUCCEED; /* Return value */
    void *value_buf = NULL, *query_value_buf = NULL;
    H5T_t *query_type, *promoted_type;
    H5Q_match_type_t match_type = H5Q_INVALID_MATCH_TYPE;
    size_t type_size, query_type_size, promoted_type_size;
    hid_t type_id = FAIL, query_type_id = FAIL, promoted_type_id = FAIL;
    H5T_path_t *tpath;
    H5Q_match_op_t query_op;
    hbool_t query_result = FALSE;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(query);
    HDassert((query->query.select.type == H5Q_TYPE_DATA_ELEM) ||
            (query->query.select.type == H5Q_TYPE_ATTR_VALUE));

    /* Keep a copy of elem to work on */
    if (0 == (type_size = H5T_get_size(type)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a valid size");

    /* Keep a copy of the query value */
    query_type = query->query.select.elem.data_elem.type;
    query_type_size = query->query.select.elem.data_elem.type_size;
    query_op = query->query.select.match_op;

    /* Promote type to compare elements with query */
    promoted_type = H5Q__promote_type(type, query_type, &match_type);
    if (0 == (promoted_type_size = H5T_get_size(promoted_type)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a valid size");

    /* Resize value and query value buf for convert
     * (promoted_type_size is always bigger) */
    if (NULL == (value_buf = H5MM_malloc(promoted_type_size)))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTALLOC, FAIL, "can't allocate value buffer");
    HDmemcpy(value_buf, value, type_size);

    if (NULL == (query_value_buf = H5MM_malloc(promoted_type_size)))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTALLOC, FAIL, "can't allocate value buffer");
    HDmemcpy(query_value_buf, query->query.select.elem.data_elem.value, query_type_size);

    /* Create temporary IDs for H5T_convert */
    if (FAIL == (type_id = H5I_register(H5I_DATATYPE, type, FALSE)))
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register datatype");
    if (FAIL == (query_type_id = H5I_register(H5I_DATATYPE, query_type, FALSE)))
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register datatype");
    if (FAIL == (promoted_type_id = H5I_register(H5I_DATATYPE, promoted_type, FALSE)))
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register datatype");

    /* Find the conversion function */
    if (NULL == (tpath = H5T_path_find(type, promoted_type, NULL, NULL, H5P_LST_DATASET_XFER_ID_g, FALSE)))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTINIT, FAIL, "unable to find type info");
    if (FAIL == (H5T_convert(tpath, type_id, promoted_type_id, 1, (size_t)0, (size_t)0, value_buf, NULL, H5P_LST_DATASET_XFER_ID_g)))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTCONVERT, FAIL, "can't convert value");

    if (NULL == (tpath = H5T_path_find(query_type, promoted_type, NULL, NULL, H5P_LST_DATASET_XFER_ID_g, FALSE)))
          HGOTO_ERROR(H5E_QUERY, H5E_CANTINIT, FAIL, "unable to find type info");
    if (FAIL == (H5T_convert(tpath, query_type_id, promoted_type_id, 1, (size_t)0, (size_t)0, query_value_buf, NULL, H5P_LST_DATASET_XFER_ID_g)))
         HGOTO_ERROR(H5E_QUERY, H5E_CANTCONVERT, FAIL, "can't convert query value");

    /* Could also use BOOST preprocessor for that but not really nice */
    switch (match_type) {
        case H5Q_NATIVE_INT_MATCH_CHAR:
            H5Q_CMP_DATA_ELEM(query_result, query_op, char, value_buf, query_value_buf);
            break;
        case H5Q_NATIVE_INT_MATCH_SHORT:
            H5Q_CMP_DATA_ELEM(query_result, query_op, short, value_buf, query_value_buf);
            break;
        case H5Q_NATIVE_INT_MATCH_INT:
            H5Q_CMP_DATA_ELEM(query_result, query_op, int, value_buf, query_value_buf);
            break;
        case H5Q_NATIVE_INT_MATCH_LONG:
            H5Q_CMP_DATA_ELEM(query_result, query_op, long, value_buf, query_value_buf);
            break;
        case H5Q_NATIVE_INT_MATCH_LLONG:
            H5Q_CMP_DATA_ELEM(query_result, query_op, long long, value_buf, query_value_buf);
            break;
        case H5Q_NATIVE_FLOAT_MATCH_FLOAT:
            H5Q_CMP_DATA_ELEM(query_result, query_op, float, value_buf, query_value_buf);
            break;
        case H5Q_NATIVE_FLOAT_MATCH_DOUBLE:
            H5Q_CMP_DATA_ELEM(query_result, query_op, double, value_buf, query_value_buf);
            break;
        case H5Q_INVALID_MATCH_TYPE:
        default:
            HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "unsupported/unrecognized datatype");
            break;
    }

    *result = query_result;

done:
    H5MM_free(value_buf);
    H5MM_free(query_value_buf);

    /* Free temporary IDs */
    if ((promoted_type_id != FAIL) && !H5I_remove(promoted_type_id))
        HGOTO_ERROR(H5E_ATOM, H5E_CANTFREE, FAIL, "problem freeing id");
    if ((query_type_id != FAIL) && !H5I_remove(query_type_id))
        HGOTO_ERROR(H5E_ATOM, H5E_CANTFREE, FAIL, "problem freeing id");
    if ((type_id != FAIL) && !H5I_remove(type_id))
        HGOTO_ERROR(H5E_ATOM, H5E_CANTFREE, FAIL, "problem freeing id");

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q__apply_data_elem() */

/*-------------------------------------------------------------------------
 * Function:    H5Q__promote_type
 *
 * Purpose: Private function for H5Q__apply_data_elem (promote data element type
 *          so that data element and query element can be compared).
 *
 * Return:  Success:    Pointer to promoted native type
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static H5T_t *
H5Q__promote_type(const H5T_t *type1, const H5T_t *type2, H5Q_match_type_t *match_type)
{
    H5T_t *ret_value = NULL; /* Return value */
    H5T_class_t type1_class, type2_class, promoted_type_class;
    H5T_t *promoted_type;

    FUNC_ENTER_NOAPI_NOINIT

    /* Get class of types */
    if (H5T_NO_CLASS == (type1_class = H5T_get_class(type1, FALSE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a valid class");
    if (H5T_NO_CLASS == (type2_class = H5T_get_class(type2, FALSE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a valid class");

     if ((type1_class == H5T_FLOAT) || (type2_class == H5T_FLOAT)) {
         promoted_type_class = H5T_FLOAT;
     } else {
         promoted_type_class = H5T_INTEGER;
     }

    switch (promoted_type_class) {
         case H5T_INTEGER:
             if (H5Q_CMP_TYPE(type1, type2, (H5T_t *)H5I_object(H5T_NATIVE_LLONG_g))) {
                 *match_type = H5Q_NATIVE_INT_MATCH_LLONG;
                 promoted_type = (H5T_t *)H5I_object(H5T_NATIVE_LLONG_g);
             } else if (H5Q_CMP_TYPE(type1, type2, (H5T_t *)H5I_object(H5T_NATIVE_LONG_g))) {
                 *match_type = H5Q_NATIVE_INT_MATCH_LONG;
                 promoted_type = (H5T_t *)H5I_object(H5T_NATIVE_LONG_g);
             } else if (H5Q_CMP_TYPE(type1, type2, (H5T_t *)H5I_object(H5T_NATIVE_INT_g))) {
                 *match_type = H5Q_NATIVE_INT_MATCH_INT;
                 promoted_type = (H5T_t *)H5I_object(H5T_NATIVE_INT_g);
             } else if (H5Q_CMP_TYPE(type1, type2, (H5T_t *)H5I_object(H5T_NATIVE_SHORT_g))) {
                 *match_type = H5Q_NATIVE_INT_MATCH_SHORT;
                 promoted_type = (H5T_t *)H5I_object(H5T_NATIVE_SHORT_g);
             } else if (H5Q_CMP_TYPE(type1, type2, (H5T_t *)H5I_object(H5T_NATIVE_SCHAR_g))) {
                 *match_type = H5Q_NATIVE_INT_MATCH_CHAR;
                 promoted_type = (H5T_t *)H5I_object(H5T_NATIVE_SCHAR_g);
             } else {
                 HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a valid type");
             }
             break;
         case H5T_FLOAT:
             if (H5Q_CMP_TYPE(type1, type2, (H5T_t *)H5I_object(H5T_NATIVE_DOUBLE_g))) {
                 *match_type = H5Q_NATIVE_FLOAT_MATCH_DOUBLE;
                 promoted_type = (H5T_t *)H5I_object(H5T_NATIVE_DOUBLE_g);
             } else if (H5Q_CMP_TYPE(type1, type2, (H5T_t *)H5I_object(H5T_NATIVE_FLOAT_g))) {
                 *match_type = H5Q_NATIVE_FLOAT_MATCH_FLOAT;
                 promoted_type = (H5T_t *)H5I_object(H5T_NATIVE_FLOAT_g);
             } else {
                 HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a valid type");
             }
             break;
         case H5T_NO_CLASS:
         case H5T_TIME:
         case H5T_STRING:
         case H5T_BITFIELD:
         case H5T_OPAQUE:
         case H5T_COMPOUND:
         case H5T_REFERENCE:
         case H5T_ENUM:
         case H5T_VLEN:
         case H5T_ARRAY:
         case H5T_NCLASSES:
         default:
             HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a valid class");
             break;
     }

    ret_value = promoted_type;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q__promote_type() */

/*-------------------------------------------------------------------------
 * Function:    H5Q__apply_attr_name
 *
 * Purpose: Private function for H5Q__apply_atom (attribute name).
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Q__apply_attr_name(const H5Q_t *query, hbool_t *result, const char *name)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(query);
    HDassert(query->query.select.type == H5Q_TYPE_ATTR_NAME);
    HDassert(result);
    HDassert(name);

    *result = (query->query.select.match_op == H5Q_MATCH_EQUAL) ?
            (0 == HDstrcmp(name, query->query.select.elem.attr_name.name)) :
            (0 != HDstrcmp(name, query->query.select.elem.attr_name.name));

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q__apply_attr_name() */

/*-------------------------------------------------------------------------
 * Function:    H5Q__apply_link_name
 *
 * Purpose: Private function for H5Q__apply_atom (link name).
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Q__apply_link_name(const H5Q_t *query, hbool_t *result, const char *name)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(query);
    HDassert(query->query.select.type == H5Q_TYPE_LINK_NAME);
    HDassert(result);
    HDassert(name);

    *result = (query->query.select.match_op == H5Q_MATCH_EQUAL) ?
            (0 == HDstrcmp(name, query->query.select.elem.link_name.name)) :
            (0 != HDstrcmp(name, query->query.select.elem.link_name.name));

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q__apply_link_name() */

/*-------------------------------------------------------------------------
 * Function:    H5Qapply
 *
 * Purpose: Apply a query and return the result. Parameters, which the
 * query applies to, are determined by the type of the query.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Qapply(hid_t loc_id, hid_t query_id, unsigned *result, hid_t vcpl_id)
{
    H5Q_t *query = NULL;
    H5G_t *ret_grp;
    hid_t ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE4("i", "ii*Iui", loc_id, query_id, result, vcpl_id);

    /* Check args and get the query objects */
    if (NULL == (query = (H5Q_t *) H5I_object_verify(query_id, H5I_QUERY)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a query ID");
    if (!result)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL pointer for result");

    /* Get the default view creation property list if the user didn't provide one */
    /* TODO fix that */
    if (H5P_DEFAULT == vcpl_id)
        vcpl_id = H5P_INDEX_ACCESS_DEFAULT;
    else
        if (TRUE != H5P_isa_class(vcpl_id, H5P_INDEX_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not index access parms");

    /* Apply query */
    if (NULL == (ret_grp = H5Q_apply(loc_id, query, result, vcpl_id)))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "unable to apply query");

    if (FAIL == (ret_value = H5I_register(H5I_GROUP, ret_grp, TRUE)))
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register group");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Qapply() */

/*-------------------------------------------------------------------------
 * Function:    H5Q_apply
 *
 * Purpose: Private function for H5Qapply.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
H5G_t *
H5Q_apply(hid_t loc_id, const H5Q_t *query, unsigned *result,
    hid_t H5_ATTR_UNUSED vcpl_id)
{
    H5Q_apply_arg_t args;
    H5Q_view_t view = H5Q_VIEW_INITIALIZER(view); /* Resulting view */
    H5G_t *ret_grp = NULL; /* New group created */
    H5G_t *ret_value = NULL; /* Return value */
    H5P_genclass_t *pclass = NULL;
    unsigned flags;
    hid_t fapl_id = FAIL;
    H5F_t *new_file = NULL;
    H5G_loc_t file_loc;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(query);
    HDassert(result);

    /* First check and optimize query */
    /* TODO */

    /* Create new view and init args */
    args.query = query;
    args.result = result;
    args.view = &view;

    if (FAIL == H5O_visit(loc_id, ".", H5_INDEX_NAME, H5_ITER_NATIVE, H5Q__apply_iterate,
            &args, H5P_LINK_ACCESS_DEFAULT, H5AC_ind_dxpl_id))
        HGOTO_ERROR(H5E_SYM, H5E_BADITER, NULL, "object visitation failed");

    if (!H5Q_QUEUE_EMPTY(&view.reg_refs))
        H5Q_LOG_DEBUG("Number of reg refs: %zu\n", view.reg_refs.n_elem);
    if (!H5Q_QUEUE_EMPTY(&view.obj_refs))
        H5Q_LOG_DEBUG("Number of obj refs: %zu\n", view.obj_refs.n_elem);
    if (!H5Q_QUEUE_EMPTY(&view.attr_refs))
        H5Q_LOG_DEBUG("Number of attr refs: %zu\n", view.attr_refs.n_elem);

    /* Get property list class */
    if (NULL == (pclass = (H5P_genclass_t *)H5I_object_verify(H5P_FILE_ACCESS, H5I_GENPROP_CLS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a property list class");

    /* Create the new property list */
    if (FAIL == (fapl_id = H5P_create_id(pclass, TRUE)))
        HGOTO_ERROR(H5E_PLIST, H5E_CANTCREATE, NULL, "unable to create property list");

    /* Use the core VFD to store the view */
    if (FAIL == H5Pset_fapl_core(fapl_id, H5Q_VIEW_CORE_INCREMENT, FALSE))
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, NULL, "unable to set property list to core VFD");

    /* Create a new file or truncate an existing file. */
    flags = H5F_ACC_EXCL | H5F_ACC_RDWR | H5F_ACC_CREAT;
    if (NULL == (new_file = H5F_open("view", flags, H5P_FILE_CREATE_DEFAULT, fapl_id, H5AC_dxpl_id)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to create file");

    /* Construct a group location for root group of the file */
    if (FAIL == H5G_root_loc(new_file, &file_loc))
        HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, NULL, "unable to create location for file")

    /* Create the new group & get its ID */
    if (NULL == (ret_grp = H5G_create_anon(&file_loc, H5P_GROUP_CREATE_DEFAULT, H5P_GROUP_ACCESS_DEFAULT)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to create group");

    /* Write view */
    if (FAIL == H5Q__view_write(ret_grp, &view))
        HGOTO_ERROR(H5E_QUERY, H5E_WRITEERROR, NULL, "can't write view");

    ret_value = ret_grp;

done:
    /* Release the group's object header, if it was created */
    if (ret_grp) {
        H5O_loc_t *grp_oloc;         /* Object location for group */

        /* Get the new group's object location */
        if (NULL == (grp_oloc = H5G_oloc(ret_grp)))
            HDONE_ERROR(H5E_SYM, H5E_CANTGET, NULL, "unable to get object location of group");

        /* Decrement refcount on group's object header in memory */
        if (FAIL == H5O_dec_rc_by_loc(grp_oloc, H5AC_dxpl_id))
            HDONE_ERROR(H5E_SYM, H5E_CANTDEC, NULL, "unable to decrement refcount on newly created object");
    } /* end if */

    /* Cleanup on failure */
    if (NULL == ret_value)
        if (ret_grp && (FAIL == H5G_close(ret_grp)))
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, NULL, "unable to release group");

    /* Close the property list */
    if ((fapl_id != FAIL) && (H5I_dec_app_ref(fapl_id) < 0))
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, NULL, "can't close");

    /* Attempt to close the file/mount hierarchy */
    if (new_file && (FAIL == H5F_try_close(new_file)))
        HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, NULL, "can't close file")

    /* Free the view */
    H5Q__view_free(&view);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q_apply() */

/*-------------------------------------------------------------------------
 * Function:    H5Q__apply_iterate
 *
 * Purpose: Private function for H5Q_apply.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Q__apply_iterate(hid_t oid, const char *name, const H5O_info_t *oinfo, void *op_data)
{
    H5Q_apply_arg_t *args = (H5Q_apply_arg_t *) op_data;
    H5Q_type_t query_type;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(args->query);
    HDassert(args->result);
    HDassert(args->view);

    if (FAIL == H5Q_get_type(args->query, &query_type))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTGET, FAIL, "unable to get query type");

    if (query_type != H5Q_TYPE_MISC) {
        if (FAIL == H5Q__apply_object(oid, name, oinfo, args))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "unable to compare query");
    } else {
        H5Q_combine_op_t op_type;
        H5Q_apply_arg_t args1, args2;
        H5Q_view_t view1 = H5Q_VIEW_INITIALIZER(view1), view2 = H5Q_VIEW_INITIALIZER(view2);
        unsigned result1 = 0, result2 = 0;

        if (FAIL == H5Q_get_combine_op(args->query, &op_type))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTGET, FAIL, "unable to get combine op");

        args1.query = args->query->query.combine.l_query;
        args1.result = &result1;
        args1.view = &view1;

        args2.query = args->query->query.combine.r_query;
        args2.result = &result2;
        args2.view = &view2;

        if (FAIL == H5Q__apply_iterate(oid, name, oinfo, &args1))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "unable to apply query");
        if (FAIL == H5Q__apply_iterate(oid, name, oinfo, &args2))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "unable to apply query");

        if (FAIL == H5Q__view_combine(op_type, &view1, &view2, result1, result2,
                args->view, args->result))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTMERGE, FAIL, "unable to merge results");

        if (result1) H5Q__view_free(&view1);
        if (result2) H5Q__view_free(&view2);
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q__apply_iterate */

/*-------------------------------------------------------------------------
 * Function:    H5Q__apply_object
 *
 * Purpose: Private function for H5Q__apply_iterate.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Q__apply_object(hid_t oid, const char *name, const H5O_info_t *oinfo,
        void *udata)
{
    H5G_loc_t loc;
    H5Q_apply_arg_t *args = (H5Q_apply_arg_t *) udata;
    H5Q_type_t query_type;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(name);
    HDassert(oinfo);
    HDassert(args);

    if (FAIL == H5G_loc(oid, &loc))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location");

    if (FAIL == H5Q_get_type(args->query, &query_type))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTGET, FAIL, "unable to get query type");

    switch (query_type) {
        /* If query on a link name, just compare the name of the object */
        case H5Q_TYPE_LINK_NAME:
            if (FAIL == H5Q__apply_object_link(&loc, name, oinfo, udata))
                HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "can't apply link query to object");
            break;
        case H5Q_TYPE_DATA_ELEM:
            if (FAIL == H5Q__apply_object_data(&loc, name, oinfo, udata))
                HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "can't apply data query to object");
            break;
        case H5Q_TYPE_ATTR_NAME:
        case H5Q_TYPE_ATTR_VALUE:
            if (FAIL == H5Q__apply_object_attr(&loc, name, udata))
                HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "can't apply data query to object");
            break;
        case H5Q_TYPE_MISC:
        default:
            HGOTO_ERROR(H5E_QUERY, H5E_BADTYPE, FAIL, "unsupported/unrecognized query type");
            break;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q__apply_object */

/*-------------------------------------------------------------------------
 * Function:    H5Q__apply_object_link
 *
 * Purpose: Private function for H5Q__apply_object.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Q__apply_object_link(H5G_loc_t *loc, const char *name, const H5O_info_t *oinfo,
        void *udata)
{
    href_t ref;
    hbool_t result;
    H5Q_apply_arg_t *args = (H5Q_apply_arg_t *) udata;
    const char *link_name = NULL;
    const char *trimmed_path = NULL;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(loc);
    HDassert(name);
    HDassert(oinfo);
    HDassert(args);

    trimmed_path = HDstrrchr(name, '/');
    link_name = (trimmed_path) ? ++trimmed_path : name;

    if ((oinfo->type != H5O_TYPE_GROUP) && (oinfo->type != H5O_TYPE_DATASET))
        HGOTO_ERROR(H5E_QUERY, H5E_BADTYPE, FAIL, "unsupported/unrecognized object type");

    if (FAIL == H5Q_apply_atom(args->query, &result, link_name))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "can't compare link name");

    if (!result) HGOTO_DONE(SUCCEED);

    *(args->result) = H5Q_REF_OBJ;

    H5Q_LOG_DEBUG("Match link name: %s (%s)\n", link_name, name);

    /* Keep object reference */
    if (NULL == (ref = H5R_create_ext_object(H5F_OPEN_NAME(loc->oloc->file), name)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "can't create object reference");
    if (FAIL == H5Q__view_append(args->view, H5R_EXT_OBJECT, ref))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTAPPEND, FAIL, "can't append object reference to view");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q__apply_object_link */

/*-------------------------------------------------------------------------
 * Function:    H5Q__apply_object_data
 *
 * Purpose: Private function for H5Q__apply_object.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Q__apply_object_data(H5G_loc_t *loc, const char *name, const H5O_info_t *oinfo,
        void *udata)
{
    href_t ref;
    H5Q_apply_arg_t *args = (H5Q_apply_arg_t *) udata;
    hid_t obj_id = FAIL;
    H5S_t *dataspace = NULL;
    H5D_t *dataset = NULL;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(loc);
    HDassert(name);
    HDassert(oinfo);
    HDassert(args);

    /* No data */
    if (oinfo->type == H5O_TYPE_GROUP)
        HGOTO_DONE(SUCCEED);

    if (oinfo->type != H5O_TYPE_DATASET)
        HGOTO_ERROR(H5E_QUERY, H5E_BADTYPE, FAIL, "unsupported/unrecognized object type");

    /* If query on a dataset, open the object and use H5D_query */
    if (FAIL == (obj_id = H5O_open_name(loc, name, H5P_LINK_ACCESS_DEFAULT, FALSE)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, FAIL, "can't open object");

    if (NULL == (dataset = (H5D_t *) H5I_object_verify(obj_id, H5I_DATASET)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");

    /* Query dataset */
    if (NULL == (dataspace = H5D_query(dataset, NULL, args->query, H5P_INDEX_ACCESS_DEFAULT, H5P_INDEX_XFER_DEFAULT)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTSELECT, FAIL, "can't query dataset");

    /* No element matched the query */
    if (H5S_SEL_NONE == H5S_get_select_type(dataspace))
        HGOTO_DONE(SUCCEED);

    *(args->result) = H5Q_REF_REG;

    /* Keep dataset region reference */
    if (NULL == (ref = H5R_create_ext_region(H5F_OPEN_NAME(loc->oloc->file), name, dataspace)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get buffer size for region reference");
    if (FAIL == H5Q__view_append(args->view, H5R_EXT_REGION, ref))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTAPPEND, FAIL, "can't append region reference to view");

done:
    if (dataspace) H5S_close(dataspace);
    if ((obj_id != FAIL) && (FAIL == H5I_dec_app_ref(obj_id)))
        HDONE_ERROR(H5E_OHDR, H5E_CANTRELEASE, FAIL, "unable to close object")
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q__apply_object_data */

/*-------------------------------------------------------------------------
 * Function:    H5Q__apply_object_attr
 *
 * Purpose: Private function for H5Q__apply_object.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Q__apply_object_attr(H5G_loc_t *loc, const char *name, void *udata)
{
    H5Q_apply_attr_arg_t attr_args;
    H5A_attr_iter_op_t attr_op; /* Attribute operator */
    H5Q_apply_arg_t *args = (H5Q_apply_arg_t *) udata;
    hid_t obj_id = FAIL;
    H5G_loc_t obj_loc;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(loc);
    HDassert(name);
    HDassert(args);

    /* Build attribute operator info */
    attr_args.filename = H5F_OPEN_NAME(loc->oloc->file);
    attr_args.loc_name = name;
    attr_args.apply_args = args;
    attr_op.op_type = H5A_ATTR_OP_LIB;
    attr_op.u.lib_op = (H5A_lib_iterate_t) H5Q__apply_object_attr_iterate;

    if (FAIL == (obj_id = H5O_open_name(loc, name, H5P_LINK_ACCESS_DEFAULT, FALSE)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "can't open object");

    /* Keep location of object */
    if (FAIL == H5G_loc(obj_id, &obj_loc))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location");
    attr_args.obj_loc = &obj_loc;

    /* Iterate over attributes */
    if (FAIL == (ret_value = H5O_attr_iterate(obj_id, H5AC_ind_dxpl_id,
            H5_INDEX_NAME, H5_ITER_NATIVE, 0, NULL, &attr_op, &attr_args)))
        HGOTO_ERROR(H5E_ATTR, H5E_BADITER, FAIL, "error iterating over attributes");

done:
    if ((obj_id != FAIL) && (FAIL == H5I_dec_app_ref(obj_id)))
        HDONE_ERROR(H5E_OHDR, H5E_CANTRELEASE, FAIL, "unable to close object")
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q__apply_object_attr */

/*-------------------------------------------------------------------------
 * Function:    H5Q__apply_object_attr_iterate
 *
 * Purpose: Private function for H5Q__apply_iterate.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Q__apply_object_attr_iterate(H5A_t *attr, void *udata)
{
    H5Q_apply_attr_arg_t *args = (H5Q_apply_attr_arg_t *) udata;
    H5Q_type_t query_type;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(attr);
    HDassert(args);

    if (FAIL == H5Q_get_type(args->apply_args->query, &query_type))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTGET, FAIL, "unable to get query type");

    switch (query_type) {
        /* If query on an attribute name, just compare the name of the object */
        case H5Q_TYPE_ATTR_NAME:
            if (FAIL == H5Q__apply_object_attr_name(attr, udata))
                HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "can't apply attr name query to object");
            break;
        case H5Q_TYPE_ATTR_VALUE:
            if (FAIL == H5Q__apply_object_attr_value(attr, udata))
                HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "can't apply attr name query to object");
            break;
        case H5Q_TYPE_LINK_NAME:
        case H5Q_TYPE_DATA_ELEM:
        case H5Q_TYPE_MISC:
        default:
            HGOTO_ERROR(H5E_QUERY, H5E_BADTYPE, FAIL, "unsupported/unrecognized query type");
            break;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q__apply_object_attr_iterate */

/*-------------------------------------------------------------------------
 * Function:    H5Q__apply_object_attr_name
 *
 * Purpose: Private function for H5Q__apply_object_attr_iterate.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Q__apply_object_attr_name(H5A_t *attr, void *udata)
{
    H5Q_apply_attr_arg_t *args = (H5Q_apply_attr_arg_t *) udata;
    char *name = NULL;
    size_t name_len;
    href_t ref;
    hbool_t result = FALSE;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(attr);
    HDassert(args);

    /* Get attribute name */
    if (0 == (name_len = (size_t) H5A_get_name(attr, 0, NULL)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't get size of attribute name");
    if (NULL == (name = (char *) H5MM_malloc(name_len + 1)))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTALLOC, FAIL, "can't allocate buffer for attribute name");
    if (0 == H5A_get_name(attr, name_len + 1, name))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't get attribute name")

    if (FAIL == H5Q_apply_atom(args->apply_args->query, &result, (const char *) name))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "can't compare attr name");

    if (!result) HGOTO_DONE(SUCCEED);

    *(args->apply_args->result) = H5Q_REF_ATTR;

    H5Q_LOG_DEBUG("Match attribute name: %s\n", (const char *) name);

    /* Keep attribute reference */
    if (NULL == (ref = H5R_create_ext_attr(args->filename, args->loc_name, (const char *) name)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get buffer size for attribute reference");
    if (FAIL == H5Q__view_append(args->apply_args->view, H5R_EXT_ATTR, ref))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTAPPEND, FAIL, "can't append attribute reference to view");

done:
    H5MM_free(name);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q__apply_object_attr_name */

/*-------------------------------------------------------------------------
 * Function:    H5Q__apply_object_attr_value
 *
 * Purpose: Private function for H5Q__apply_object_attr_iterate.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Q__apply_object_attr_value(H5A_t *iter_attr, void *udata)
{
    H5Q_apply_attr_arg_t *args = (H5Q_apply_attr_arg_t *) udata;
    char *name = NULL;
    size_t name_len;
    void *buf = NULL;
    size_t buf_size;
    H5A_t *attr = NULL;
    H5T_t *dt = NULL;
    H5S_t *space = NULL;
    size_t nelmts, elmt_size;
    H5S_sel_iter_op_t iter_op;  /* Operator for iteration */
    H5Q_apply_attr_elem_arg_t iter_args;
    href_t ref;
    hbool_t result = FALSE;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(iter_attr);
    HDassert(args);

    /* TODO there may be another way of doing that but the attribute must be
     * opened cleanly and the attribute given is not open
     */

    /* Get attribute name */
    if (0 == (name_len = (size_t) H5A_get_name(iter_attr, 0, NULL)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't get size of attribute name");
    if (NULL == (name = (char *) H5MM_malloc(name_len + 1)))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTALLOC, FAIL, "can't allocate buffer for attribute name");
    if (0 == H5A_get_name(iter_attr, name_len + 1, (char *) name))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't get attribute name")

    /* Finish opening attribute */
    if (NULL == (attr = H5A_open(args->obj_loc, (const char *) name, H5AC_ind_dxpl_id)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "can't open attribute");

    /* Get attribute info */
    if (NULL == (dt = H5A_get_type(attr)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "can't get attribute datatype");
    if (NULL == (space = H5A_get_space(attr)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get attribute dataspace");
    if (0 == (nelmts = (size_t) H5S_get_select_npoints(space)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");
    if (0 == (elmt_size = H5T_get_size(dt)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "invalid size of element");

    /* Allocate buffer to hold data */
    buf_size = nelmts * elmt_size;
    if (NULL == (buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_QUERY, H5E_NOSPACE, FAIL, "can't allocate read buffer");

    /* Read data */
    if (FAIL == H5A_read(attr, dt, buf, H5AC_ind_dxpl_id))
        HGOTO_ERROR(H5E_ATTR, H5E_READERROR, FAIL, "unable to read attribute");

    iter_args.query = args->apply_args->query;
    iter_args.result = &result;

    iter_op.op_type = H5S_SEL_ITER_OP_LIB;
    iter_op.u.lib_op = H5Q__apply_object_attr_value_iterate;

    /* Iterate over attribute elements to compare values */
    if (FAIL == H5S_select_iterate(buf, dt, space, &iter_op, &iter_args))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOMPARE, FAIL, "unable to compare attribute elements");

    if (!result) HGOTO_DONE(SUCCEED);

    *(args->apply_args->result) = H5Q_REF_ATTR;

    H5Q_LOG_DEBUG("Match value of attribute name: %s\n", (const char *) name);

    /* Keep attribute reference */
    if (NULL == (ref = H5R_create_ext_attr(args->filename, args->loc_name, (const char *) name)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get buffer size for attribute reference");
    if (FAIL == H5Q__view_append(args->apply_args->view, H5R_EXT_ATTR, ref))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTAPPEND, FAIL, "can't append attribute reference to view");

done:
    H5MM_free(name);
    H5MM_free(buf);
    if (attr) H5A_close(attr);
    if (dt) H5T_close(dt);
    if (space) H5S_close(space);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q__apply_object_attr_value */

/*-------------------------------------------------------------------------
 * Function:    H5Q__apply_object_attr_value_iterate
 *
 * Purpose: Private function for H5Q__apply_iterate.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Q__apply_object_attr_value_iterate(void *elem, const H5T_t *type,
        unsigned H5_ATTR_UNUSED ndim, const hsize_t H5_ATTR_UNUSED *point, void *udata)
{
    H5Q_apply_attr_elem_arg_t *args = (H5Q_apply_attr_elem_arg_t *) udata;
    hbool_t result;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(elem);
    HDassert(type);

    /* Apply the query */
    if (FAIL == H5Q_apply_atom(args->query, &result, type, elem))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "unable to apply query to data element");

    *(args->result) |= result;

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q__apply_object_attr_value_iterate */

/*-------------------------------------------------------------------------
 * Function:    H5Q__view_append
 *
 * Purpose: Private function for H5Q__apply_iterate.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Q__view_append(H5Q_view_t *view, H5R_type_t ref_type, void *ref)
{
    H5Q_ref_entry_t *ref_entry;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(view);
    HDassert(ref);

    if (NULL == (ref_entry = (H5Q_ref_entry_t *) H5MM_malloc(sizeof(H5Q_ref_entry_t))))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTALLOC, FAIL, "can't allocate ref entry");
    ref_entry->ref = ref;

    switch (ref_type) {
        case H5R_EXT_REGION:
            H5Q_QUEUE_INSERT_TAIL(&view->reg_refs, ref_entry, entry);
            break;
        case H5R_EXT_OBJECT:
            H5Q_QUEUE_INSERT_TAIL(&view->obj_refs, ref_entry, entry);
            break;
        case H5R_EXT_ATTR:
            H5Q_QUEUE_INSERT_TAIL(&view->attr_refs, ref_entry, entry);
            break;
        case H5R_BADTYPE:
        case H5R_MAXTYPE:
        default:
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid ref type");
    }

done:
    if (FAIL == ret_value) H5MM_free(ref_entry);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q__view_append */

/*-------------------------------------------------------------------------
 * Function:    H5Q__view_combine
 *
 * Purpose: Private function for H5Q__apply_iterate.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Q__view_combine(H5Q_combine_op_t combine_op, H5Q_view_t *view1, H5Q_view_t *view2,
        unsigned result1, unsigned result2, H5Q_view_t *view, unsigned *result)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(view);
    HDassert(result);

    if ((combine_op == H5Q_COMBINE_AND) && ((result1 && result2))) {
        unsigned combine_result = (result1 > result2) ? result1 : result2;

        H5Q_LOG_DEBUG("Result 1 (%x), result 2 (%x), combined result (%x)\n",
                result1, result2, combine_result);

        *result = combine_result;

        switch (combine_result) {
            case H5Q_REF_REG:
                /* Combined results are on the same object (here, dataset),
                 * therefore at this point only result1 or result2 has a region
                 * reference */
                if (result1 & H5Q_REF_REG)
                    H5Q_QUEUE_CONCAT(&view->reg_refs, &view1->reg_refs);
                else if (result2 & H5Q_REF_REG)
                    H5Q_QUEUE_CONCAT(&view->reg_refs, &view2->reg_refs);
                break;
            case H5Q_REF_OBJ:
                if (result1 & H5Q_REF_OBJ)
                    H5Q_QUEUE_CONCAT(&view->obj_refs, &view1->obj_refs);
                else if (result2 & H5Q_REF_OBJ)
                    H5Q_QUEUE_CONCAT(&view->obj_refs, &view2->obj_refs);
                break;
            case H5Q_REF_ATTR:
                /* TODO check that references are equal and keep refs equal, discard others*/
                if (result1 & H5Q_REF_ATTR)
                    H5Q_QUEUE_CONCAT(&view->attr_refs, &view1->attr_refs);
                else if (result2 & H5Q_REF_ATTR)
                    H5Q_QUEUE_CONCAT(&view->attr_refs, &view2->attr_refs);
                break;
            default:
                HGOTO_ERROR(H5E_QUERY, H5E_BADTYPE, FAIL, "unsupported/unrecognized query type");
                break;
        }
    } else if ((combine_op == H5Q_COMBINE_OR) && ((result1 || result2))) {
        H5Q_ref_head_t *refs[H5Q_VIEW_REF_NTYPES] = { &view->reg_refs, &view->obj_refs, &view->attr_refs };
        H5Q_ref_head_t *refs1[H5Q_VIEW_REF_NTYPES] = { &view1->reg_refs, &view1->obj_refs, &view1->attr_refs };
        H5Q_ref_head_t *refs2[H5Q_VIEW_REF_NTYPES] = { &view2->reg_refs, &view2->obj_refs, &view2->attr_refs };
        unsigned combine_result = result1 | result2;
        int i;

        H5Q_LOG_DEBUG("Result 1 (%x), result 2 (%x), combined result (%x)\n",
                result1, result2, combine_result);

        *result = combine_result;

        /* Simply concatenate results from sub-views */
        for (i = 0; i < H5Q_VIEW_REF_NTYPES; i++) {
            H5Q_QUEUE_CONCAT(refs[i], refs1[i]);
            H5Q_QUEUE_CONCAT(refs[i], refs2[i]);
        }
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q__view_combine */

/*-------------------------------------------------------------------------
 * Function:    H5Q__view_write
 *
 * Purpose: Private function for H5Q_apply.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Q__view_write(H5G_t *grp, H5Q_view_t *view)
{
    H5G_loc_t loc;
    H5D_t *dset = NULL;
    H5S_t *mem_space = NULL;
    H5S_t *space = NULL;
    H5Q_ref_head_t *refs[H5Q_VIEW_REF_NTYPES] = { &view->reg_refs, &view->obj_refs,
            &view->attr_refs };
    const char *dset_name[H5Q_VIEW_REF_NTYPES] = { H5Q_VIEW_REF_REG_NAME,
            H5Q_VIEW_REF_OBJ_NAME, H5Q_VIEW_REF_ATTR_NAME };
    hid_t ref_types[H5Q_VIEW_REF_NTYPES] = { H5T_STD_REF_EXT_REG,
            H5T_STD_REF_EXT_OBJ, H5T_STD_REF_EXT_ATTR };
    herr_t ret_value = SUCCEED; /* Return value */
    int i;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(grp);

    /* Get location of group */
    loc.oloc = H5G_oloc(grp);
    loc.path = H5G_nameof(grp);

    /* Iterate over reference types and write references if any */
    for (i = 0; i < H5Q_VIEW_REF_NTYPES; i++) {
        H5Q_ref_entry_t *ref_entry = NULL;
        hsize_t n_elem = refs[i]->n_elem;
        hsize_t start = 0;

        if (!n_elem)
            continue;

        /* Create dataspace */
        if (NULL == (space = H5S_create_simple(1, &n_elem, NULL)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCREATE, FAIL, "unable to create dataspace");

        /* Create the new dataset & get its ID */
        H5Q_LOG_DEBUG("Create reference dataset: %s", dset_name[i]);
        if (NULL == (dset = H5D_create_named(&loc, dset_name[i], ref_types[i],
                space, H5P_LINK_CREATE_DEFAULT, H5P_DATASET_CREATE_DEFAULT,
                H5P_DATASET_ACCESS_DEFAULT, H5AC_dxpl_id)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "unable to create dataset");

        /* Iterate over reference entries in view */
        H5Q_QUEUE_FOREACH(ref_entry, refs[i], entry) {
            hsize_t count = 1;

            if (NULL == (mem_space = H5S_create_simple(1, &count, NULL)))
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCREATE, FAIL, "unable to create dataspace");
            if (FAIL == H5S_select_hyperslab(space, H5S_SELECT_SET, &start, NULL, &count, NULL))
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to set hyperslab selection")
            if (FAIL == H5D_write(dset, FALSE, ref_types[i], mem_space, space,
                    H5P_DATASET_XFER_DEFAULT, &ref_entry->ref))
                HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "unable to write dataset");
            if (FAIL == H5S_close(mem_space))
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCLOSEOBJ, FAIL, "unable to close dataspace");
            mem_space = NULL;

            /* Increment reference position in file */
            start++;
        }

        if (FAIL == H5D_close(dset))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to close dataset");
        dset = NULL;
        if (FAIL == H5S_close(space))
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCLOSEOBJ, FAIL, "unable to close dataspace");
        space = NULL;
    }

done:
    if (dset) H5D_close(dset);
    if (space) H5S_close(space);
    if (mem_space) H5S_close(mem_space);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q__view_write */

/*-------------------------------------------------------------------------
 * Function:    H5Q__view_free
 *
 * Purpose: Private function for H5Q_apply.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Q__view_free(H5Q_view_t *view)
{
    H5Q_ref_head_t *refs[H5Q_VIEW_REF_NTYPES] = { &view->reg_refs, &view->obj_refs,
            &view->attr_refs };
    herr_t ret_value = SUCCEED; /* Return value */
    int i;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    for (i = 0; i < H5Q_VIEW_REF_NTYPES; i++) {
        while (!H5Q_QUEUE_EMPTY(refs[i])) {
            H5Q_ref_entry_t *ref_entry = H5Q_QUEUE_FIRST(refs[i]);
            H5Q_QUEUE_REMOVE_HEAD(refs[i], entry);
            /* TODO call H5Rdestroy */
            H5R_destroy(ref_entry->ref);
            H5MM_free(ref_entry);
        }
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q__view_free */

/*-------------------------------------------------------------------------
 * Function:    H5Qapply_multi
 *
 * Purpose: Apply a query on multiple locations and return the result.
 * Parameters, which the query applies to, are determined by the type of the
 * query.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Qapply_multi(size_t loc_count, hid_t *loc_ids, hid_t query_id,
        unsigned *result, hid_t vcpl_id)
{
    H5Q_t *query = NULL;
    H5G_t *ret_grp;
    hid_t ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE5("i", "z*ii*Iui", loc_count, loc_ids, query_id, result, vcpl_id);

    /* Check args and get the query objects */
    if (!loc_count)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "loc_count cannot be NULL");
    if (!loc_ids)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "loc_ids cannot be NULL");
    if (NULL == (query = (H5Q_t *) H5I_object_verify(query_id, H5I_QUERY)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a query ID");
    if (!result)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL pointer for result");

    /* Get the default view creation property list if the user didn't provide one */
    /* TODO fix that */
    if (H5P_DEFAULT == vcpl_id)
        vcpl_id = H5P_INDEX_ACCESS_DEFAULT;
    else
        if (TRUE != H5P_isa_class(vcpl_id, H5P_INDEX_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not index access parms");

    /* Apply query */
    if (NULL == (ret_grp = H5Q_apply_multi(loc_count, loc_ids, query, result, vcpl_id)))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "unable to apply query");

    if (FAIL == (ret_value = H5I_register(H5I_GROUP, ret_grp, TRUE)))
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register group");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Qapply_multi() */

/*-------------------------------------------------------------------------
 * Function:    H5Q_apply_multi
 *
 * Purpose: Private function for H5Qapply_multi.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
H5G_t *
H5Q_apply_multi(size_t loc_count, hid_t *loc_ids, const H5Q_t *query,
    unsigned *result, hid_t H5_ATTR_UNUSED vcpl_id)
{
    H5Q_view_t view = H5Q_VIEW_INITIALIZER(view); /* Resulting view */
    H5Q_ref_head_t *refs[H5Q_VIEW_REF_NTYPES] = { &view.reg_refs, &view.obj_refs, &view.attr_refs };
    unsigned multi_result = 0;
    H5G_t *ret_grp = NULL; /* New group created */
    H5G_t *ret_value = NULL; /* Return value */
    H5P_genclass_t *pclass = NULL;
    unsigned flags;
    hid_t fapl_id = FAIL;
    H5F_t *new_file = NULL;
    H5G_loc_t file_loc;
    size_t i;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(loc_count);
    HDassert(loc_ids);
    HDassert(query);
    HDassert(result);

    /* TODO Serial iteration for now */
    for (i = 0; i < loc_count; i++) {
        H5Q_view_t loc_view = H5Q_VIEW_INITIALIZER(loc_view); /* Resulting view */
        H5Q_ref_head_t *loc_refs[H5Q_VIEW_REF_NTYPES] = { &loc_view.reg_refs, &loc_view.obj_refs, &loc_view.attr_refs };
        unsigned loc_result;
        H5Q_apply_arg_t args;
        int j;

        /* Create new view and init args */
        args.query = query;
        args.result = &loc_result;
        args.view = &loc_view;

        if (FAIL == H5O_visit(loc_ids[i], ".", H5_INDEX_NAME, H5_ITER_NATIVE, H5Q__apply_iterate,
                &args, H5P_LINK_ACCESS_DEFAULT, H5AC_ind_dxpl_id))
            HGOTO_ERROR(H5E_SYM, H5E_BADITER, NULL, "object visitation failed");

        multi_result |= loc_result;
        /* Simply concatenate results from sub-view */
        for (j = 0; j < H5Q_VIEW_REF_NTYPES; j++) {
            H5Q_QUEUE_CONCAT(refs[j], loc_refs[j]);
        }
    }

    if (!H5Q_QUEUE_EMPTY(&view.reg_refs))
        H5Q_LOG_DEBUG("Number of reg refs: %zu\n", view.reg_refs.n_elem);
    if (!H5Q_QUEUE_EMPTY(&view.obj_refs))
        H5Q_LOG_DEBUG("Number of obj refs: %zu\n", view.obj_refs.n_elem);
    if (!H5Q_QUEUE_EMPTY(&view.attr_refs))
        H5Q_LOG_DEBUG("Number of attr refs: %zu\n", view.attr_refs.n_elem);

    /* Get property list class */
    if (NULL == (pclass = (H5P_genclass_t *)H5I_object_verify(H5P_FILE_ACCESS, H5I_GENPROP_CLS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a property list class");

    /* Create the new property list */
    if (FAIL == (fapl_id = H5P_create_id(pclass, TRUE)))
        HGOTO_ERROR(H5E_PLIST, H5E_CANTCREATE, NULL, "unable to create property list");

    /* Use the core VFD to store the view */
    if (FAIL == H5Pset_fapl_core(fapl_id, H5Q_VIEW_CORE_INCREMENT, FALSE))
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, NULL, "unable to set property list to core VFD");

    /* Create a new file or truncate an existing file. */
    flags = H5F_ACC_EXCL | H5F_ACC_RDWR | H5F_ACC_CREAT;
    if (NULL == (new_file = H5F_open("view", flags, H5P_FILE_CREATE_DEFAULT, fapl_id, H5AC_dxpl_id)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to create file");

    /* Construct a group location for root group of the file */
    if (FAIL == H5G_root_loc(new_file, &file_loc))
        HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, NULL, "unable to create location for file")

    /* Create the new group & get its ID */
    if (NULL == (ret_grp = H5G_create_anon(&file_loc, H5P_GROUP_CREATE_DEFAULT, H5P_GROUP_ACCESS_DEFAULT)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to create group");

    /* Write view */
    if (FAIL == H5Q__view_write(ret_grp, &view))
        HGOTO_ERROR(H5E_QUERY, H5E_WRITEERROR, NULL, "can't write view");

    *result = multi_result;
    ret_value = ret_grp;

done:
    /* Release the group's object header, if it was created */
    if (ret_grp) {
        H5O_loc_t *grp_oloc;         /* Object location for group */

        /* Get the new group's object location */
        if (NULL == (grp_oloc = H5G_oloc(ret_grp)))
            HDONE_ERROR(H5E_SYM, H5E_CANTGET, NULL, "unable to get object location of group");

        /* Decrement refcount on group's object header in memory */
        if (FAIL == H5O_dec_rc_by_loc(grp_oloc, H5AC_dxpl_id))
            HDONE_ERROR(H5E_SYM, H5E_CANTDEC, NULL, "unable to decrement refcount on newly created object");
    } /* end if */

    /* Cleanup on failure */
    if (NULL == ret_value)
        if (ret_grp && (FAIL == H5G_close(ret_grp)))
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, NULL, "unable to release group");

    /* Close the property list */
    if ((fapl_id != FAIL) && (H5I_dec_app_ref(fapl_id) < 0))
        HDONE_ERROR(H5E_PLIST, H5E_CANTFREE, NULL, "can't close");

    /* Attempt to close the file/mount hierarchy */
    if (new_file && (FAIL == H5F_try_close(new_file)))
        HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, NULL, "can't close file")

    /* Free the view */
    H5Q__view_free(&view);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Q_apply_multi() */
