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
 * Purpose:	DB index routines.
 */

/****************/
/* Module Setup */
/****************/

/***********/
/* Headers */
/***********/
#include "H5private.h"          /* Generic Functions */
#include "H5Xprivate.h"         /* Index */
#include "H5Eprivate.h"         /* Error handling */
#include "H5Iprivate.h"         /* IDs */
#include "H5MMprivate.h"        /* Memory management */
#include "H5Pprivate.h"
#include "H5Qprivate.h"
#include "H5Sprivate.h"
/* TODO using private headers but could use public ones */

#define H5R_FRIEND
#include "H5Rpkg.h" /* (Tmp) To re-use H5R__get_obj_name */

#ifdef H5_HAVE_DB
#include <db.h>

/****************/
/* Local Macros */
/****************/
#define H5X_DB_DEBUG

#ifdef H5X_DB_DEBUG
#define H5X_DB_LOG_DEBUG(...) do {                              \
      fprintf(stdout, " # %s(): ", __func__);                   \
      fprintf(stdout, __VA_ARGS__);                             \
      fprintf(stdout, "\n");                                    \
      fflush(stdout);                                           \
  } while (0)
#else
#define H5X_DB_LOG_DEBUG(...) do { \
  } while (0)
#endif

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

#define H5X_DB_METADATA_TYPES 3
#define H5X_DB_MAX_NAME_LEN (64 * 1024)

#define H5X_DB_LINK_NAMES_SUFFIX    "__link_names.db"
#define H5X_DB_ATTR_NAMES_SUFFIX    "__attr_names.db"
#define H5X_DB_ATTR_VALUES_SUFFIX   "__attr_values.db"

/******************/
/* Local Typedefs */
/******************/

typedef struct H5X_db_entry_t H5X_db_entry_t;

struct H5X_db_entry_t {
    H5Q_type_t type;            /* Query type */
    href_t ref;                 /* External reference */
    H5Q_QUEUE_ENTRY(H5X_db_entry_t) entry;
};

typedef H5Q_QUEUE_HEAD(H5X_db_head_t, H5X_db_entry_t) H5X_db_head_t;

typedef struct H5X_db_t {
    char *link_names_file;
    char *attr_names_file;
    char *attr_values_file;
    DB *link_names_db;
    DB *attr_names_db;
    DB *attr_values_db;
} H5X_db_t;

typedef struct {
    const char *filename;
    const char *loc_name;
    H5X_db_t *db;
} H5X_db_index_attr_arg_t;

typedef struct {
    H5X_db_index_attr_arg_t *attr_args;
    const char *attr_name;
} H5X_db_index_attr_elem_arg_t;

/********************/
/* Local Prototypes */
/********************/

static herr_t H5X__db_index(hid_t oid, const char *name, const H5O_info_t *oinfo,
    void *udata);
static herr_t H5X__db_index_link_name(H5X_db_t *db, hid_t loc_id,
    const char *name, const H5O_info_t *oinfo);
static herr_t H5X__db_index_attrs(H5X_db_t *db, hid_t loc_id,
    const char *name);
static herr_t H5X__db_index_attr(hid_t loc_id, const char *attr_name,
    const H5A_info_t H5_ATTR_UNUSED *ainfo, void *udata);
static herr_t H5X__db_index_attr_name(const char *attr_name, void *udata);
static herr_t H5X__db_index_attr_value(hid_t loc_id, const char *attr_name,
    void *udata);
static herr_t H5X__db_index_attr_value_iterate(void *elem, hid_t type_id,
    unsigned ndim, const hsize_t *point, void *udata);
static herr_t H5X__db_put(H5X_db_t *db, href_t ref, H5Q_type_t type, ...);
static herr_t H5X__db_push_entry(H5X_db_head_t *head,
    H5X_db_entry_t *entry);
static herr_t H5X__db_remove_entries(H5X_db_head_t *head);
static herr_t H5X__db_gen_file_names(H5X_db_t *db, hid_t loc_id);
static herr_t H5X__db_free_file_names(H5X_db_t *db);
static herr_t H5X__db_create_tables(H5X_db_t *db, hbool_t open, unsigned flags);
static herr_t H5X__db_close_tables(H5X_db_t *db);
static herr_t H5X__db_remove_tables(H5X_db_t *db);
static void H5X__db_stat(H5X_db_t *db);
static herr_t H5X__db_metadata_write(H5X_db_t *db,
    size_t *plugin_metadata_size, void **plugin_metadata);
static herr_t H5X__db_metadata_read(size_t plugin_metadata_size,
    void *plugin_metadata, H5X_db_t *metadata);
static herr_t H5X__db_query_components(H5X_db_t *db, hid_t query_id,
    H5X_db_head_t *result);
static herr_t H5X__db_query_singleton(H5X_db_t *db, hid_t query_id,
    H5X_db_head_t *result);
static herr_t H5X__db_query_combine(H5Q_combine_op_t combine_op,
    H5X_db_head_t *result1, H5X_db_head_t *result2, H5X_db_head_t *result);

static void *H5X__db_create(hid_t loc_id, hid_t xcpl_id, hid_t xapl_id,
    size_t *metadata_size, void **metadata);
static herr_t H5X__db_remove(hid_t loc_id, size_t metadata_size,
    void *metadata);
static void *H5X__db_open(hid_t loc_id, hid_t xapl_id, size_t
    metadata_size, void *metadata);
static herr_t H5X__db_close(void *idx_handle);
static herr_t H5X__db_insert_entry(void *idx_handle, hid_t obj_id,
    H5Q_type_t key_type, H5Q_elem_t *key, hid_t xxpl_id);
static herr_t H5X__db_remove_entry(void *idx_handle, hid_t obj_id,
    H5Q_type_t key_type, H5Q_elem_t *key, hid_t xxpl_id);
static herr_t H5X__db_query(void *idx_handle, hid_t query_id, hid_t xxpl_id,
    size_t *ref_count, href_t *refs[]);

/*********************/
/* Package Variables */
/*********************/

/*****************************/
/* Library Private Variables */
/*****************************/

/*******************/
/* Local Variables */
/*******************/

/* DB index class */
static H5X_idx_class_t idx_class = {.metadata_class = {
    H5X__db_create,         /* create */
    H5X__db_remove,         /* remove */
    H5X__db_open,           /* open */
    H5X__db_close,          /* close */
    H5X__db_insert_entry,   /* insert_entry */
    H5X__db_remove_entry,   /* remove_entry */
    H5X__db_query,          /* query */
    NULL                    /* get_size */
}};

const H5X_class_t H5X_META_DB[1] = {{
    H5X_CLASS_T_VERS,       /* (From the H5Xpublic.h header file) */
    H5X_PLUGIN_META_DB,     /* (Or whatever number is assigned) */
    "DB",                   /* Whatever name desired */
    H5X_TYPE_METADATA,      /* This plugin operates on metadata */
    &idx_class              /* Index class */
}};

#ifdef H5X_DB_DEBUG
static void
print_ref(href_t ref)
{
    char obj_name[H5X_DB_MAX_NAME_LEN];

    H5R__get_obj_name(NULL, H5P_DEFAULT, H5P_DEFAULT, ref, obj_name, H5X_DB_MAX_NAME_LEN);
    if (H5Rget_type(ref) == H5R_EXT_ATTR) {
        char attr_name[H5X_DB_MAX_NAME_LEN];
        H5R__get_attr_name(NULL, ref, attr_name, H5X_DB_MAX_NAME_LEN);
        H5X_DB_LOG_DEBUG("Attribute reference: %s, %s", obj_name, attr_name);
    } else {
        H5X_DB_LOG_DEBUG("Object reference: %s", obj_name);
    }
} /* print_ref */
#endif /* H5X_DB_DEBUG */

static herr_t
H5X__db_index(hid_t oid, const char *name, const H5O_info_t *oinfo, void *udata)
{
    H5X_db_t *db = (H5X_db_t *) udata;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(name);
    HDassert(oinfo);
    HDassert(db);

    /* Index link names */
    if (FAIL == H5X__db_index_link_name(db, oid, name, oinfo))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTAPPEND, FAIL, "can't add link name");

    /* Index attribute names/values */
    if (FAIL == H5X__db_index_attrs(db, oid, name))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "can't apply data query to object");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__db_index */

static herr_t
H5X__db_index_link_name(H5X_db_t *db, hid_t loc_id, const char *name,
    const H5O_info_t *oinfo)
{
    href_t ref = NULL;
    const char *link_name = NULL;
    const char *trimmed_path = NULL;
    char file_name[H5X_DB_MAX_NAME_LEN];
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(db);
    HDassert(name);
    HDassert(oinfo);

    trimmed_path = HDstrrchr(name, '/');
    link_name = (trimmed_path) ? ++trimmed_path : name;

    if ((oinfo->type != H5O_TYPE_GROUP) && (oinfo->type != H5O_TYPE_DATASET))
        HGOTO_ERROR(H5E_QUERY, H5E_BADTYPE, FAIL, "unsupported/unrecognized object type");

    /* Get file name */
    if (H5Fget_name(loc_id, file_name, H5X_DB_MAX_NAME_LEN) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get file name");

    /* Keep object reference */
    if (NULL == (ref = H5R_create_ext_object(file_name, name)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "can't create object reference");
    if (FAIL == H5X__db_put(db, ref, H5Q_TYPE_LINK_NAME, link_name))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTAPPEND, FAIL, "can't append object reference to view");

done:
    if (ref && FAIL == H5Rdestroy(ref))
        HDONE_ERROR(H5E_REFERENCE, H5E_CANTCLOSEOBJ, FAIL, "unable to destroy reference");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__db_index_link_name */

static herr_t
H5X__db_index_attrs(H5X_db_t *db, hid_t loc_id, const char *name)
{
    H5X_db_index_attr_arg_t attr_args;
    hid_t obj_id = FAIL;
    char file_name[H5X_DB_MAX_NAME_LEN];
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(db);
    HDassert(name);

    /* Get file name */
    if (H5Fget_name(loc_id, file_name, H5X_DB_MAX_NAME_LEN) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get file name");

    /* Build attribute operator info */
    attr_args.filename = file_name;
    attr_args.loc_name = name;
    attr_args.db = db;

    if (0 == HDstrcmp(name, ".")) {
        obj_id = loc_id;
    } else {
        if (FAIL == (obj_id = H5Oopen(loc_id, name, H5P_DEFAULT)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, FAIL, "can't open object");
    }

    /* Iterate over attributes */
    if (FAIL == (ret_value = H5Aiterate(obj_id, H5_INDEX_NAME, H5_ITER_NATIVE,
        NULL, H5X__db_index_attr, &attr_args)))
        HGOTO_ERROR(H5E_ATTR, H5E_BADITER, FAIL, "error iterating over attributes");

done:
    if ((obj_id != FAIL) && (obj_id != loc_id) && (FAIL == H5Oclose(obj_id)))
        HDONE_ERROR(H5E_OHDR, H5E_CANTRELEASE, FAIL, "unable to close object")
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__db_index_attrs */

static herr_t
H5X__db_index_attr(hid_t loc_id, const char *attr_name,
    const H5A_info_t H5_ATTR_UNUSED *ainfo, void *udata)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(attr_name);
    HDassert(udata);

    if (FAIL == H5X__db_index_attr_name(attr_name, udata))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "can't apply attr name query to object");

    if (FAIL == H5X__db_index_attr_value(loc_id, attr_name, udata))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "can't apply attr name query to object");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__db_index_attr */

static herr_t
H5X__db_index_attr_name(const char *attr_name, void *udata)
{
    H5X_db_index_attr_arg_t *args = (H5X_db_index_attr_arg_t *) udata;
    href_t ref = NULL;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(attr_name);
    HDassert(args);

    /* Keep attribute reference */
    if (NULL == (ref = H5R_create_ext_attr(args->filename, args->loc_name, attr_name)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "can't create attribute reference");
    if (FAIL == H5X__db_put(args->db, ref, H5Q_TYPE_ATTR_NAME, attr_name))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTAPPEND, FAIL, "can't append attribute reference to view");

done:
    if (ref && FAIL == H5Rdestroy(ref))
        HDONE_ERROR(H5E_REFERENCE, H5E_CANTCLOSEOBJ, FAIL, "unable to destroy reference");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__db_index_attr_name */

static herr_t
H5X__db_index_attr_value(hid_t loc_id, const char *attr_name, void *udata)
{
    H5X_db_index_attr_arg_t *args = (H5X_db_index_attr_arg_t *) udata;
    void *buf = NULL;
    size_t buf_size;
    hid_t attr_id = FAIL;
    hid_t type_id = FAIL;
    hid_t space_id = FAIL;
    size_t nelmts, elmt_size;
    H5X_db_index_attr_elem_arg_t iter_args;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(attr_name);
    HDassert(args);

    /* Open attribute */
    if (FAIL == (attr_id = H5Aopen(loc_id, attr_name, H5P_DEFAULT)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "can't open attribute");

    /* Get attribute info */
    if (FAIL == (type_id = H5Aget_type(attr_id)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "can't get attribute datatype");
    if (FAIL == (space_id = H5Aget_space(attr_id)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get attribute dataspace");
    if (0 == (nelmts = (size_t) H5Sget_select_npoints(space_id)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");
    if (0 == (elmt_size = H5Tget_size(type_id)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "invalid size of element");

    /* Allocate buffer to hold data */
    buf_size = nelmts * elmt_size;
    if (NULL == (buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_QUERY, H5E_NOSPACE, FAIL, "can't allocate read buffer");

    /* Read data */
    if (FAIL == H5Aread(attr_id, type_id, buf))
        HGOTO_ERROR(H5E_ATTR, H5E_READERROR, FAIL, "unable to read attribute");

    iter_args.attr_args = args;
    iter_args.attr_name = attr_name;

    /* Iterate over attribute elements to compare values */
    if (FAIL == H5Diterate(buf, type_id, space_id, H5X__db_index_attr_value_iterate, &iter_args))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOMPARE, FAIL, "unable to compare attribute elements");

done:
    H5MM_free(buf);
    if (attr_id != FAIL) H5Aclose(attr_id);
    if (type_id != FAIL) H5Tclose(type_id);
    if (space_id != FAIL) H5Sclose(space_id);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__db_index_attr_value */

static herr_t
H5X__db_index_attr_value_iterate(void *elem, hid_t type_id,
    unsigned H5_ATTR_UNUSED ndim, const hsize_t H5_ATTR_UNUSED *point, void *udata)
{
    H5X_db_index_attr_elem_arg_t *args = (H5X_db_index_attr_elem_arg_t *) udata;
    href_t ref = NULL;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(elem);
    HDassert(args);

    /* Keep attribute reference */
    if (NULL == (ref = H5R_create_ext_attr(args->attr_args->filename, args->attr_args->loc_name, args->attr_name)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "can't create attribute reference");
    if (FAIL == H5X__db_put(args->attr_args->db, ref, H5Q_TYPE_ATTR_VALUE, type_id, elem))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTAPPEND, FAIL, "can't append attribute reference to view");

done:
    if (ref && FAIL == H5Rdestroy(ref))
        HDONE_ERROR(H5E_REFERENCE, H5E_CANTCLOSEOBJ, FAIL, "unable to destroy reference");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__db_index_attr_value_iterate */

static herr_t
H5X__db_put(H5X_db_t *db, href_t ref, H5Q_type_t type, ...)
{
    va_list ap;
    DBT key;  /* The key to dbcp->put() */
    DBT data; /* The data to dbcp->put() */
    size_t buf_size;
    void *buf = NULL;
    herr_t ret_value = SUCCEED; /* Return value */
    int db_ret;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(db);
    HDassert(ref);

    va_start(ap, type);

    /* Initialize key/data */
    HDmemset(&key, 0, sizeof(DBT));
    HDmemset(&data, 0, sizeof(DBT));

    if (H5R_encode(ref, NULL, &buf_size) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTENCODE, FAIL, "can't encode reference");
    if (NULL == (buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTALLOC, FAIL, "can't allocate buffer for encoding reference");
    if (H5R_encode(ref, buf, &buf_size) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTENCODE, FAIL, "can't encode reference");

    switch (type) {
        case H5Q_TYPE_ATTR_VALUE:
        {
            hid_t datatype_id;
            hid_t native_type_id;
            size_t datatype_size;
            const void *value;
            void *encoded_value = NULL;

            /* Get arguments */
            datatype_id = va_arg(ap, hid_t);
            value = va_arg(ap, const void *);

            HDassert(datatype_id != FAIL);
            HDassert(value);

            if (FAIL == (native_type_id = H5Tget_native_type(datatype_id, H5T_DIR_DEFAULT)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCOPY, FAIL, "can't copy attribute type");
            if (0 == (datatype_size = H5Tget_size(native_type_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a valid size");
            if (NULL == (encoded_value = H5MM_malloc(datatype_size)))
                HGOTO_ERROR(H5E_QUERY, H5E_CANTALLOC, FAIL, "can't allocate value buffer");
            HDmemcpy(encoded_value, value, datatype_size);

            key.data = encoded_value;
            key.size = (u_int32_t)datatype_size;
            data.data = buf;
            data.size = (u_int32_t)buf_size;

            if ((db_ret = db->attr_values_db->put(db->attr_values_db, NULL, &key, &data, 0)) != 0) {
                db->attr_values_db->err(db->attr_values_db, db_ret, "DB->put");
            }
            H5MM_free(encoded_value);
        }
        break;
        case H5Q_TYPE_ATTR_NAME:
        {
            char *attr_name;

            /* Get arguments */
            attr_name = va_arg(ap, char *);

            HDassert(attr_name);

            key.data = attr_name;
            key.size = (u_int32_t)(HDstrlen(attr_name) + 1);
            data.data = buf;
            data.size = (u_int32_t)buf_size;

            if ((db_ret = db->attr_names_db->put(db->attr_names_db, NULL, &key, &data, 0)) != 0) {
                db->attr_names_db->err(db->attr_names_db, db_ret, "DB->put");
            }
        }
        break;
        case H5Q_TYPE_LINK_NAME:
        {
            char *link_name;

            /* Get arguments */
            link_name = va_arg(ap, char *);

            HDassert(link_name);

            key.data = link_name;
            key.size = (u_int32_t)(HDstrlen(link_name) + 1);
            data.data = buf;
            data.size = (u_int32_t)buf_size;

            if ((db_ret = db->link_names_db->put(db->link_names_db, NULL, &key, &data, 0)) != 0) {
                db->link_names_db->err(db->link_names_db, db_ret, "DB->put");
            }
        }
        break;
        case H5Q_TYPE_MISC:
        default:
            HGOTO_ERROR(H5E_QUERY, H5E_BADTYPE, FAIL, "unsupported/unrecognized query type");
            break;
    }

done:
    va_end(ap);

    H5MM_free(buf);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__db_put */

static herr_t
H5X__db_push_entry(H5X_db_head_t *head, H5X_db_entry_t *entry)
{
    H5X_db_entry_t *new_entry;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(head);
    HDassert(entry);

    if (NULL == (new_entry = (H5X_db_entry_t *) H5MM_malloc(sizeof(H5X_db_entry_t))))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTALLOC, FAIL, "can't allocate ref entry");
    new_entry->type = entry->type;
    new_entry->ref = H5Rcopy(entry->ref);
    H5Q_QUEUE_INSERT_TAIL(head, new_entry, entry);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__db_push_entry */

static herr_t
H5X__db_remove_entries(H5X_db_head_t *head)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(head);

    while (!H5Q_QUEUE_EMPTY(head)) {
        H5X_db_entry_t *entry = H5Q_QUEUE_FIRST(head);
        H5Q_QUEUE_REMOVE_HEAD(head, entry);
        if (entry->ref && FAIL == H5Rdestroy(entry->ref))
            HGOTO_ERROR(H5E_REFERENCE, H5E_CANTCLOSEOBJ, FAIL, "unable to destroy reference");
        H5MM_free(entry);
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__db_metadata_remove_entries */

static herr_t
H5X__db_gen_file_names(H5X_db_t *db, hid_t loc_id)
{
    char filename[H5X_DB_MAX_NAME_LEN];
    ssize_t filename_len;
    char *filename_prefix = NULL;
    char **db_filename_ptrs[H5X_DB_METADATA_TYPES] = { &db->link_names_file,
        &db->attr_names_file, &db->attr_values_file };
    const char *db_filename_suffixes[H5X_DB_METADATA_TYPES] = { H5X_DB_LINK_NAMES_SUFFIX,
        H5X_DB_ATTR_NAMES_SUFFIX, H5X_DB_ATTR_VALUES_SUFFIX };
    herr_t ret_value = SUCCEED;
    int i;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(db);

    /* Get filename prefix */
    if ((filename_len = H5Fget_name(loc_id, filename, H5X_DB_MAX_NAME_LEN)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get file name");

    filename_prefix = HDstrtok(filename, ".");
    if (!filename_prefix)
        HGOTO_ERROR(H5E_INDEX, H5E_BADVALUE, FAIL, "invalid file name (no '.' delim)");

    for (i = 0; i < H5X_DB_METADATA_TYPES; i++) {
        char *db_filename;

        /* Generate filename for link names */
        if (NULL == (db_filename = (char *) H5MM_malloc(
            HDstrlen(filename_prefix) + HDstrlen(db_filename_suffixes[i]) + 1)))
            HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate db struct");
        HDstrcpy(db_filename, filename_prefix);
        HDstrcat(db_filename, db_filename_suffixes[i]);
        *db_filename_ptrs[i] = db_filename;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__db_gen_file_names() */

static herr_t
H5X__db_free_file_names(H5X_db_t *db)
{
    char **db_filename_ptrs[H5X_DB_METADATA_TYPES] = { &db->link_names_file,
        &db->attr_names_file, &db->attr_values_file };
    herr_t ret_value = SUCCEED;
    int i;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(db);

    for (i = 0; i < H5X_DB_METADATA_TYPES; i++)
        *db_filename_ptrs[i] = H5MM_xfree(*db_filename_ptrs[i]);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__db_free_file_names() */

static herr_t
H5X__db_create_tables(H5X_db_t *db, hbool_t open, unsigned flags)
{
    herr_t ret_value = SUCCEED; /* Return value */
    DB **db_ptrs[H5X_DB_METADATA_TYPES] = { &db->link_names_db,
        &db->attr_names_db, &db->attr_values_db };
    const char *db_filenames[H5X_DB_METADATA_TYPES] = { db->link_names_file,
        db->attr_names_file, db->attr_values_file };
    int db_ret;
    int i;

    FUNC_ENTER_NOAPI_NOINIT

    for (i = 0; i < H5X_DB_METADATA_TYPES; i++) {
        DB *dbp;

        /* Create the database handles */
        if ((db_ret = db_create(&dbp, NULL, 0)) != 0)
            HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL,
                "can't create database: db_create: %s", db_strerror(db_ret));
        *db_ptrs[i] = dbp;

        if (open) {
            /* Configure the database to use 1KB page sizes */
            if ((db_ret = dbp->set_pagesize(dbp, 1024)) != 0) {
                dbp->err(dbp, db_ret, "set_pagesize");
            }

            /* Allow duplicates */
            if ((db_ret = dbp->set_flags(dbp, DB_DUP)) != 0)
                dbp->err(dbp, db_ret, "set_flags: DB_DUP");

            /* Open and make it a DB_BTREE */
            if ((db_ret = dbp->open(dbp, NULL, db_filenames[i],
                NULL, DB_BTREE, flags, 0664)) != 0) {
                dbp->err(dbp, db_ret, "open: %s", db_filenames[i]);
            }
        }
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__db_create_tables */

static herr_t
H5X__db_close_tables(H5X_db_t *db)
{
    herr_t ret_value = SUCCEED; /* Return value */
    DB **db_ptrs[H5X_DB_METADATA_TYPES] = { &db->link_names_db,
        &db->attr_names_db, &db->attr_values_db };
    int db_ret;
    int i;

    FUNC_ENTER_NOAPI_NOINIT

    for (i = 0; i < H5X_DB_METADATA_TYPES; i++) {
        DB *dbp = *db_ptrs[i];

        if ((db_ret = dbp->close(dbp, 0)) != 0)
            HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "DB->close: %s",
                db_strerror(db_ret));
        *db_ptrs[i] = NULL;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__db_close_tables */

static herr_t
H5X__db_remove_tables(H5X_db_t *db)
{
    herr_t ret_value = SUCCEED; /* Return value */
    DB **db_ptrs[H5X_DB_METADATA_TYPES] = { &db->link_names_db,
        &db->attr_names_db, &db->attr_values_db };
    const char *db_filenames[H5X_DB_METADATA_TYPES] = { db->link_names_file,
        db->attr_names_file, db->attr_values_file };
    int db_ret;
    int i;

    FUNC_ENTER_NOAPI_NOINIT

    for (i = 0; i < H5X_DB_METADATA_TYPES; i++) {
        DB *dbp = *db_ptrs[i];

        /* Remove the database handles */
        if ((db_ret = dbp->remove(dbp, db_filenames[i], NULL, 0)) != 0)
            HGOTO_ERROR(H5E_INDEX, H5E_CANTREMOVE, FAIL,
                "can't remove database: DB->remove: %s", db_strerror(db_ret));
        *db_ptrs[i] = NULL;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__db_remove_tables */

static void
H5X__db_stat(H5X_db_t *db)
{
    const char *db_filenames[H5X_DB_METADATA_TYPES] = { db->link_names_file,
        db->attr_names_file, db->attr_values_file };
    DB *dbs[H5X_DB_METADATA_TYPES] = { db->link_names_db, db->attr_names_db,
        db->attr_values_db };
    const char *db_names[H5X_DB_METADATA_TYPES] = { "link names", "attr names",
        "attr values" };
    int i;

    for (i = 0; i < H5X_DB_METADATA_TYPES; i++) {
        DB_BTREE_STAT *statp; /* The statistic pointer to record the total amount of record number. */
        struct stat st;
        int db_ret;

        /* Flush the database to disk so that we get a valid file size */
        if ((db_ret = dbs[i]->sync(dbs[i], 0)) != 0) {
            dbs[i]->err(dbs[i], db_ret, "DB->sync");
        }

        /* Get the database statistics and print the total number of records. */
        if ((db_ret = dbs[i]->stat(dbs[i], NULL, &statp, 0)) != 0) {
            dbs[i]->err(dbs[i], db_ret, "DB->stat");
        }
//        if ((db_ret = dbs[i]->stat_print(dbs[i], 0)) != 0) {
//            dbs[i]->err(dbs[i], db_ret, "DB->stat_print");
//        }
        HDstat(db_filenames[i], &st);

        H5X_DB_LOG_DEBUG("Database contains %lu records for %s", (u_long)statp->bt_ndata, db_names[i]);
        H5X_DB_LOG_DEBUG("Current database size on disk: %lf MB", (float)st.st_size / (float)(1024 * 1024));

        HDfree(statp);
    }
}

static herr_t
H5X__db_metadata_write(H5X_db_t *db, size_t *plugin_metadata_size,
    void **plugin_metadata)
{
    const char *db_filenames[H5X_DB_METADATA_TYPES] = { db->link_names_file,
        db->attr_names_file, db->attr_values_file };
    herr_t ret_value = SUCCEED; /* Return value */
    size_t buf_size = 0;
    void *buf;
    char *buf_ptr;
    int i;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(db);
    HDassert(plugin_metadata_size);
    HDassert(plugin_metadata);

    for (i = 0; i < H5X_DB_METADATA_TYPES; i++) {
        buf_size += HDstrlen(db_filenames[i]) + 1;
    }
    if (NULL == (buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate plugin metadata");
    buf_ptr = buf;
    for (i = 0; i < H5X_DB_METADATA_TYPES; i++) {
        HDstrcpy(buf_ptr, db_filenames[i]);
        buf_ptr += HDstrlen(db_filenames[i]) + 1;
    }

    *plugin_metadata = buf;
    *plugin_metadata_size = buf_size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__db_metadata_write */

static herr_t
H5X__db_metadata_read(size_t H5_ATTR_UNUSED plugin_metadata_size,
    void *plugin_metadata, H5X_db_t *db)
{
    char **db_filename_ptrs[H5X_DB_METADATA_TYPES] = { &db->link_names_file,
        &db->attr_names_file, &db->attr_values_file };
    herr_t ret_value = SUCCEED; /* Return value */
    char *buf_ptr;
    int i;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(db);
    HDassert(plugin_metadata_size);
    HDassert(plugin_metadata);

    buf_ptr = plugin_metadata;
    for (i = 0; i < H5X_DB_METADATA_TYPES; i++) {
        char *db_filename;

        /* Generate filename for link names */
        if (NULL == (db_filename = (char *) H5MM_malloc(HDstrlen(buf_ptr) + 1)))
            HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate db struct");
        HDstrcpy(db_filename, buf_ptr);
        buf_ptr += HDstrlen(db_filename) + 1;
        *db_filename_ptrs[i] = db_filename;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__db_metadata_read */

/*-------------------------------------------------------------------------
 * Function:    H5X_db_create
 *
 * Purpose: This function creates a new instance of a db plugin index.
 *
 * Return:  Success:    Pointer to the new index
 *          Failure:    NULL
 *
 *------------------------------------------------------------------------
 */
static void *
H5X__db_create(hid_t loc_id, hid_t H5_ATTR_UNUSED xcpl_id,
    hid_t H5_ATTR_UNUSED xapl_id, size_t *metadata_size, void **metadata)
{
    H5X_db_t *db = NULL;
#ifdef H5X_DB_DEBUG
    struct timeval t1, t2;
#endif
    void *ret_value = NULL; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    H5X_DB_LOG_DEBUG("Calling H5X_db_create");

    /* Create new db instance */
    if (NULL == (db = (H5X_db_t *) H5MM_malloc(sizeof(H5X_db_t))))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, NULL, "can't allocate db struct");
    db->link_names_file = NULL;
    db->link_names_db = NULL;
    db->attr_names_file = NULL;
    db->attr_names_db = NULL;
    db->attr_values_file = NULL;
    db->attr_values_db = NULL;

    /* Generate file names */
    if (FAIL == H5X__db_gen_file_names(db, loc_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, NULL, "can't create file names");

    /* Create DBs */
    if (FAIL == H5X__db_create_tables(db, TRUE, DB_CREATE | DB_TRUNCATE))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, NULL, "can't create tables");

    /* Visit file */
#ifdef H5X_DB_DEBUG
    HDgettimeofday(&t1, NULL);
#endif
    if (FAIL == H5Ovisit(loc_id, H5_INDEX_NAME, H5_ITER_NATIVE, H5X__db_index, db))
        HGOTO_ERROR(H5E_SYM, H5E_BADITER, NULL, "object visitation failed");
#ifdef H5X_DB_DEBUG
    HDgettimeofday(&t2, NULL);
#endif

    H5X_DB_LOG_DEBUG("######################################################");
    H5X_DB_LOG_DEBUG("Index build time: %lf ms",
        ((float)(t2.tv_sec - t1.tv_sec)) * 1000.0f
        + ((float)(t2.tv_usec - t1.tv_usec)) / (1000.0f));
    H5X__db_stat(db);
    H5X_DB_LOG_DEBUG("######################################################");

    /* Write index metadata */
    if (FAIL == H5X__db_metadata_write(db, metadata_size, metadata))
        HGOTO_ERROR(H5E_INDEX, H5E_WRITEERROR, NULL, "can't write metadata");

    ret_value = db;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_db_create() */

/*-------------------------------------------------------------------------
 * Function:    H5X_db_remove
 *
 * Purpose: This function removes the db plugin index from the file.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__db_remove(hid_t H5_ATTR_UNUSED file_id, size_t metadata_size,
    void *metadata)
{
    H5X_db_t *db = NULL;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    H5X_DB_LOG_DEBUG("Calling H5X_db_remove");

    if (!metadata_size)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL metadata size");
    if (!metadata)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL metadata");

    if (NULL == (db = (H5X_db_t *) H5MM_malloc(sizeof(H5X_db_t))))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate db struct");

    if (FAIL == H5X__db_metadata_read(metadata_size, metadata, db))
        HGOTO_ERROR(H5E_INDEX, H5E_READERROR, FAIL, "can't read metadata");

    if (FAIL == H5X__db_create_tables(db, FALSE, 0))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create tables");

    if (FAIL == H5X__db_remove_tables(db))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTREMOVE, FAIL, "can't remove tables");

    if (FAIL == H5X__db_free_file_names(db))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTFREE, FAIL, "can't free file names");
    H5MM_free(db);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_db_remove() */

/*-------------------------------------------------------------------------
 * Function:    H5X_db_open
 *
 * Purpose: This function opens an already existing db index from a file.
 *
 * Return:  Success:    Pointer to the index
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5X__db_open(hid_t H5_ATTR_UNUSED loc_id, hid_t H5_ATTR_UNUSED xapl_id,
    size_t metadata_size, void *metadata)
{
    H5X_db_t *db = NULL;
    void *ret_value = NULL; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    H5X_DB_LOG_DEBUG("Calling H5X_db_open");

    if (!metadata_size)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "NULL metadata size");
    if (!metadata)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "NULL metadata");

    if (NULL == (db = (H5X_db_t *) H5MM_malloc(sizeof(H5X_db_t))))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, NULL, "can't allocate db struct");

    if (FAIL == H5X__db_metadata_read(metadata_size, metadata, db))
        HGOTO_ERROR(H5E_INDEX, H5E_READERROR, NULL, "can't read metadata");

    ret_value = db;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_db_open() */

/*-------------------------------------------------------------------------
 * Function:    H5X_db_close
 *
 * Purpose: This function closes a db index.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__db_close(void *idx_handle)
{
    H5X_db_t *db = (H5X_db_t *) idx_handle;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    H5X_DB_LOG_DEBUG("Calling H5X_db_close");

    if (NULL == db)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");

    if (FAIL == H5X__db_close_tables(db))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close tables");

    if (FAIL == H5X__db_free_file_names(db))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTFREE, FAIL, "can't free file names");
    H5MM_free(db);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_db_close() */

/*-------------------------------------------------------------------------
 * Function:    H5X__db_insert_entry
 *
 * Purpose: This function insert a new entry in the db plugin index.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__db_insert_entry(void *idx_handle, hid_t obj_id, H5Q_type_t key_type,
    H5Q_elem_t *key, hid_t H5_ATTR_UNUSED xxpl_id)
{
    H5X_db_t *db = (H5X_db_t *) idx_handle;
    char file_name[H5X_DB_MAX_NAME_LEN];
    char loc_name[H5X_DB_MAX_NAME_LEN];
    href_t ref = NULL;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    H5X_DB_LOG_DEBUG("Calling H5X__db_insert_entry");

    if (NULL == db)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");

    if (H5Fget_name(obj_id, file_name, H5X_DB_MAX_NAME_LEN) < 0)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get file name");
    if (H5Iget_name(obj_id, loc_name, H5X_DB_MAX_NAME_LEN) < 0)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get location name");

    switch (key_type) {
        case H5Q_TYPE_ATTR_VALUE:
        {
            char attr_name[H5X_DB_MAX_NAME_LEN];

            if (H5Aget_name(obj_id, H5X_DB_MAX_NAME_LEN, attr_name) < 0)
                HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get attribute name");

            /* Keep attribute reference */
            if (NULL == (ref = H5R_create_ext_attr(file_name, loc_name, attr_name)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "can't create attribute reference");
            if (FAIL == H5X__db_put(db, ref, H5Q_TYPE_ATTR_VALUE, key->value.type, key->value.value))
                HGOTO_ERROR(H5E_QUERY, H5E_CANTAPPEND, FAIL, "can't append attribute reference to view");
        }
            break;
        case H5Q_TYPE_ATTR_NAME:
        {
            /* Keep attribute reference */
            if (NULL == (ref = H5R_create_ext_attr(file_name, loc_name, key->name)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "can't create attribute reference");
            if (FAIL == H5X__db_put(db, ref, H5Q_TYPE_ATTR_NAME, key->name))
                HGOTO_ERROR(H5E_QUERY, H5E_CANTAPPEND, FAIL, "can't append attribute reference to view");
        }
            break;
        case H5Q_TYPE_LINK_NAME:
        {
            /* Keep object reference */
            if (NULL == (ref = H5R_create_ext_object(file_name, loc_name)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "can't create object reference");
            if (FAIL == H5X__db_put(db, ref, H5Q_TYPE_LINK_NAME, key->name))
                HGOTO_ERROR(H5E_QUERY, H5E_CANTAPPEND, FAIL, "can't append object reference to view");
        }
        break;
        case H5Q_TYPE_MISC:
        default:
            HGOTO_ERROR(H5E_QUERY, H5E_BADTYPE, FAIL, "unsupported/unrecognized query type");
            break;
    }

done:
    if (ref && FAIL == H5Rdestroy(ref))
        HDONE_ERROR(H5E_REFERENCE, H5E_CANTCLOSEOBJ, FAIL, "unable to destroy reference");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__db_insert_entry() */

/*-------------------------------------------------------------------------
 * Function:    H5X__db_remove_entry
 *
 * Purpose: This function removes an entry from the db plugin index.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__db_remove_entry(void H5_ATTR_UNUSED *idx_handle, hid_t H5_ATTR_UNUSED obj_id,
    H5Q_type_t H5_ATTR_UNUSED key_type, H5Q_elem_t H5_ATTR_UNUSED *key,
    hid_t H5_ATTR_UNUSED xxpl_id)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    H5X_DB_LOG_DEBUG("Calling H5X__db_remove_entry");

    /* TODO Does not do anything */
    /*/!\ Calling db->del removes all duplicates that match the same key */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__db_remove_entry() */

/*-------------------------------------------------------------------------
 * Function:    H5X_db_query
 *
 * Purpose: This function queries a db index.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__db_query(void *idx_handle, hid_t query_id, hid_t H5_ATTR_UNUSED xxpl_id,
    size_t *ref_count, href_t *refs[])
{
    H5X_db_t *db = (H5X_db_t *) idx_handle;
    H5X_db_head_t query_result = H5Q_QUEUE_HEAD_INITIALIZER(query_result);
    href_t *ref_buf = NULL;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    H5X_DB_LOG_DEBUG("Calling H5X_db_query");

    if (NULL == db)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");

    /* We assume here that the queries passed only operate on metadata */
    if (FAIL == H5X__db_query_components(db, query_id, &query_result))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCOMPARE, FAIL, "can't query metadata");

    H5X_DB_LOG_DEBUG("###########################");
    if (!H5Q_QUEUE_EMPTY(&query_result)) {
        H5X_db_entry_t *entry;
        H5X_DB_LOG_DEBUG("Query returns %zu references", query_result.n_elem);
        H5Q_QUEUE_FOREACH(entry, &query_result, entry)
            print_ref(entry->ref);
    }
    H5X_DB_LOG_DEBUG("###########################");

    /* Fill result */
    if (!H5Q_QUEUE_EMPTY(&query_result)) {
        H5X_db_entry_t *entry;
        int i = 0;

        if (NULL == (ref_buf = H5MM_malloc(sizeof(href_t) * query_result.n_elem)))
        HGOTO_ERROR(H5E_QUERY, H5E_NOSPACE, FAIL, "can't allocate read buffer");

        H5Q_QUEUE_FOREACH(entry, &query_result, entry) {
            ref_buf[i] = H5Rcopy(entry->ref);
            i++;
        }
    }

    *ref_count = query_result.n_elem;
    *refs = ref_buf;

done:
    if (FAIL == H5X__db_remove_entries(&query_result))
        HDONE_ERROR(H5E_QUERY, H5E_CANTFREE, FAIL, "unable to remove entries");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_db_query() */

static herr_t
H5X__db_query_components(H5X_db_t *db, hid_t query_id, H5X_db_head_t *result)
{
    H5Q_type_t query_type;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if (FAIL == H5Qget_type(query_id, &query_type))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTGET, FAIL, "unable to get query type");

    if (query_type != H5Q_TYPE_MISC) {
        if (FAIL == H5X__db_query_singleton(db, query_id, result))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "unable to compare query");
    } else {
        H5Q_combine_op_t combine_op;
        hid_t sub_query1_id, sub_query2_id;
        H5X_db_head_t result1 = H5Q_QUEUE_HEAD_INITIALIZER(result1);
        H5X_db_head_t result2 = H5Q_QUEUE_HEAD_INITIALIZER(result2);

        if (FAIL == H5Qget_combine_op(query_id, &combine_op))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTGET, FAIL, "unable to get combine op");
        if (FAIL == H5Qget_components(query_id, &sub_query1_id, &sub_query2_id))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTGET, FAIL, "unable to get components");

        if (FAIL == H5X__db_query_components(db, sub_query1_id, &result1))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "unable to apply query");

//        H5X_DB_LOG_DEBUG("###########################");
//        if (!H5Q_QUEUE_EMPTY(&result1)) {
//            H5X_db_entry_t *entry;
//            H5X_DB_LOG_DEBUG("Query returns %zu references", result1.n_elem);
//            H5Q_QUEUE_FOREACH(entry, &result1, entry)
//                print_ref(entry->ref);
//        }
//        H5X_DB_LOG_DEBUG("###########################");

        if (FAIL == H5X__db_query_components(db, sub_query2_id, &result2))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "unable to apply query");

//        H5X_DB_LOG_DEBUG("###########################");
//        if (!H5Q_QUEUE_EMPTY(&result2)) {
//            H5X_db_entry_t *entry;
//            H5X_DB_LOG_DEBUG("Query returns %zu references", result2.n_elem);
//            H5Q_QUEUE_FOREACH(entry, &result2, entry)
//                print_ref(entry->ref);
//        }
//        H5X_DB_LOG_DEBUG("###########################");

        if (FAIL == H5X__db_query_combine(combine_op, &result1, &result2, result))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTMERGE, FAIL, "unable to merge results");

//        H5X_DB_LOG_DEBUG("###########################");
//        if (!H5Q_QUEUE_EMPTY(result)) {
//            H5X_db_entry_t *entry;
//            H5X_DB_LOG_DEBUG("Query returns %zu references", result->n_elem);
//            H5Q_QUEUE_FOREACH(entry, result, entry)
//                print_ref(entry->ref);
//        }
//        H5X_DB_LOG_DEBUG("###########################");

        if (FAIL == H5X__db_remove_entries(&result1))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTFREE, FAIL, "unable to remove entries");
        if (FAIL == H5X__db_remove_entries(&result2))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTFREE, FAIL, "unable to remove entries");
        if (FAIL == H5Qclose(sub_query1_id))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTCLOSEOBJ, FAIL, "unable to close query");
        if (FAIL == H5Qclose(sub_query2_id))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTCLOSEOBJ, FAIL, "unable to close query");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__db_query_components */

static herr_t
H5X__db_query_singleton(H5X_db_t *db, hid_t query_id, H5X_db_head_t *result)
{
    DB *dbp;
    DBC *dbcp = NULL; /* Handle of database cursor used for getting the data. */
    DBT key;  /* The key from dbcp->get() */
    DBT data; /* The data from dbcp->get() */
    int db_ret;
    H5Q_t *query; /* TODO */
    H5Q_type_t query_type;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Initialize key/data */
    HDmemset(&key, 0, sizeof(DBT));
    HDmemset(&data, 0, sizeof(DBT));

    if (FAIL == H5Qget_type(query_id, &query_type))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTGET, FAIL, "unable to get query type");

    /* TODO add public function */
    if (NULL == (query = (H5Q_t *) H5I_object_verify(query_id, H5I_QUERY)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a query ID");

    switch (query_type) {
        case H5Q_TYPE_LINK_NAME:
        {
            /* DB_NEXT moves the cursor to the next record. */
            key.data = query->query.select.elem.link_name.name;
            key.size = (u_int32_t)(HDstrlen(query->query.select.elem.link_name.name) + 1);

            dbp = db->link_names_db;
        }
            break;
        case H5Q_TYPE_ATTR_NAME:
        {
            /* DB_NEXT moves the cursor to the next record. */
            key.data = query->query.select.elem.attr_name.name;
            key.size = (u_int32_t)(HDstrlen(query->query.select.elem.attr_name.name) + 1);

            dbp = db->attr_names_db;
        }
            break;
        case H5Q_TYPE_ATTR_VALUE:
        {
            /* DB_NEXT moves the cursor to the next record. */
            key.data = query->query.select.elem.data_elem.value;
            key.size = (u_int32_t)query->query.select.elem.data_elem.type_size;

            dbp = db->attr_values_db;
        }
            break;
        case H5Q_TYPE_DATA_ELEM:
        case H5Q_TYPE_MISC:
        default:
            HGOTO_ERROR(H5E_QUERY, H5E_BADTYPE, FAIL, "unsupported/unrecognized query type");
            break;
    }

    /* Acquire a cursor for sequential access to the database. */
    if ((db_ret = dbp->cursor(dbp, NULL, &dbcp, 0)) != 0)
        dbp->err(dbp, db_ret, "DB->cursor");

    db_ret = dbcp->get(dbcp, &key, &data, DB_SET);
    while (db_ret != DB_NOTFOUND) {
        H5X_db_entry_t *entry;

//        if (query_type == H5Q_TYPE_ATTR_VALUE)
//            H5X_DB_LOG_DEBUG("key: %d, key size: %u", *((int *)key.data), (unsigned int)key.size);
//        else
//            H5X_DB_LOG_DEBUG("key: %s, key size: %u", (char *)key.data, (unsigned int)key.size);

        if (NULL == (entry = (H5X_db_entry_t *) H5MM_malloc(sizeof(H5X_db_entry_t))))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTALLOC, FAIL, "can't allocate ref entry");
        entry->ref = NULL;
        entry->type = query_type;
        if (NULL == (entry->ref = H5R_decode(data.data)))
            HGOTO_ERROR(H5E_REFERENCE, H5E_CANTDECODE, FAIL, "can't decode reference");

        H5Q_QUEUE_INSERT_TAIL(result, entry, entry);
        db_ret = dbcp->get(dbcp, &key, &data, DB_NEXT_DUP);
    }

done:
    /* Close the cursor */
    if (dbcp)
        dbcp->close(dbcp);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__db_query_singleton */

static herr_t
H5X__db_query_combine(H5Q_combine_op_t combine_op, H5X_db_head_t *result1,
    H5X_db_head_t *result2, H5X_db_head_t *result)
{
    H5X_db_entry_t *entry1;
    H5X_db_entry_t *entry2;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if (combine_op == H5Q_COMBINE_AND) {
        H5Q_QUEUE_FOREACH(entry1, result1, entry) {
            H5R_type_t ref_type1 = H5Rget_type(entry1->ref);
            H5Q_QUEUE_FOREACH(entry2, result2, entry) {
                H5R_type_t ref_type2 = H5Rget_type(entry2->ref);

//                H5X_DB_LOG_DEBUG("Comparing references:");
//                print_ref(entry1->ref);
//                print_ref(entry2->ref);

                if (H5Requal(entry1->ref, entry2->ref)) {
                    if (FAIL == H5X__db_push_entry(result, entry1))
                        HGOTO_ERROR(H5E_INDEX, H5E_CANTMERGE, FAIL, "unable to add result");
                }
                else if (ref_type1 == H5R_EXT_ATTR && ref_type2 == H5R_EXT_OBJECT) {
                    char obj_name1[H5X_DB_MAX_NAME_LEN];
                    char obj_name2[H5X_DB_MAX_NAME_LEN];

                    /* Only combine if obj_names are equal */
                    H5R__get_obj_name(NULL, H5P_DEFAULT, H5P_DEFAULT, entry1->ref,
                        obj_name1, H5X_DB_MAX_NAME_LEN);
                    H5R__get_obj_name(NULL, H5P_DEFAULT, H5P_DEFAULT, entry2->ref,
                        obj_name2, H5X_DB_MAX_NAME_LEN);
                    if (0 == HDstrcmp(obj_name1, obj_name2)) {
                        if (FAIL == H5X__db_push_entry(result, entry2))
                            HGOTO_ERROR(H5E_INDEX, H5E_CANTMERGE, FAIL, "unable to add result");
                    }
                }
                else if (ref_type1 == H5R_EXT_OBJECT && ref_type2 == H5R_EXT_ATTR) {
                    char obj_name1[H5X_DB_MAX_NAME_LEN];
                    char obj_name2[H5X_DB_MAX_NAME_LEN];

                    /* Only combine if obj_names are equal */
                    H5R__get_obj_name(NULL, H5P_DEFAULT, H5P_DEFAULT, entry1->ref,
                        obj_name1, H5X_DB_MAX_NAME_LEN);
                    H5R__get_obj_name(NULL, H5P_DEFAULT, H5P_DEFAULT, entry2->ref,
                        obj_name2, H5X_DB_MAX_NAME_LEN);
                    if (0 == HDstrcmp(obj_name1, obj_name2)) {
                        if (FAIL == H5X__db_push_entry(result, entry1))
                            HGOTO_ERROR(H5E_INDEX, H5E_CANTMERGE, FAIL, "unable to add result");
                    }
                }
            }
        }
    } else if (combine_op == H5Q_COMBINE_OR) {
        /* TODO might want to remove eventual duplicates */
        H5Q_QUEUE_CONCAT(result, result1);
        H5Q_QUEUE_CONCAT(result, result2);
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__db_query_combine */

#endif /* H5_HAVE_DB */
