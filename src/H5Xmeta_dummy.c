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
 * Purpose:	Dummy index routines.
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

//#define H5Q_FRIEND
//#include "H5Qpkg.h" /* To re-use H5Q_QUEUE */

#define H5R_FRIEND
#include "H5Rpkg.h" /* (Tmp) To re-use H5R__get_obj_name */

/****************/
/* Local Macros */
/****************/
//#define H5X_DUMMY_DEBUG

#ifdef H5X_DUMMY_DEBUG
#define H5X_DUMMY_LOG_DEBUG(...) do {                           \
      fprintf(stdout, " # %s(): ", __func__);                   \
      fprintf(stdout, __VA_ARGS__);                             \
      fprintf(stdout, "\n");                                    \
      fflush(stdout);                                           \
  } while (0)
#else
#define H5X_DUMMY_LOG_DEBUG(...) do { \
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

#define H5X_DUMMY_METADATA_TYPES 3
#define H5X_DUMMY_MAX_NAME_LEN (64 * 1024)

/******************/
/* Local Typedefs */
/******************/

typedef struct {
    hid_t type;
    void *value;
} H5X_dummy_elem_t;

typedef struct H5X_dummy_entry_t H5X_dummy_entry_t;

struct H5X_dummy_entry_t {
    H5Q_type_t type;            /* Query type */
    union {                     /* Key */
        H5X_dummy_elem_t elem;
        char *name;
    } key;
    href_t ref;                 /* External reference */
    H5Q_QUEUE_ENTRY(H5X_dummy_entry_t) entry;
};

typedef H5Q_QUEUE_HEAD(H5X_dummy_head_t, H5X_dummy_entry_t) H5X_dummy_head_t;

typedef struct {
    H5X_dummy_head_t attr_values;
    H5X_dummy_head_t attr_names;
    H5X_dummy_head_t link_names;
} H5X_dummy_metadata_t;

typedef struct {
    const char *filename;
    const char *loc_name;
    H5X_dummy_metadata_t *metadata;
} H5X_dummy_index_attr_arg_t;

typedef struct {
    H5X_dummy_index_attr_arg_t *attr_args;
    const char *attr_name;
} H5X_dummy_index_attr_elem_arg_t;

typedef struct H5X_dummy_t {
//    hid_t dataset_id;
//    hid_t idx_anon_id;
//    void *idx_token;
//    size_t idx_token_size;
    H5X_dummy_metadata_t metadata;
} H5X_dummy_t;

/********************/
/* Local Prototypes */
/********************/

static herr_t H5X__dummy_index(hid_t oid, const char *name, const H5O_info_t *oinfo,
    void *udata);
static herr_t H5X__dummy_index_link_name(H5X_dummy_metadata_t *metadata, hid_t loc_id,
    const char *name, const H5O_info_t *oinfo);
static herr_t H5X__dummy_index_attrs(H5X_dummy_metadata_t *metadata, hid_t loc_id,
    const char *name);
static herr_t H5X__dummy_index_attr(hid_t loc_id, const char *attr_name,
    const H5A_info_t H5_ATTR_UNUSED *ainfo, void *udata);
static herr_t H5X__dummy_index_attr_name(const char *attr_name, void *udata);
static herr_t H5X__dummy_index_attr_value(hid_t loc_id, const char *attr_name,
    void *udata);
static herr_t H5X__dummy_index_attr_value_iterate(void *elem, hid_t type_id,
    unsigned ndim, const hsize_t *point, void *udata);
static herr_t H5X__dummy_metadata_add(H5X_dummy_metadata_t *metadata,
    href_t ref, H5Q_type_t type, ...);
static herr_t H5X__dummy_metadata_free(H5X_dummy_metadata_t *metadata);
static herr_t H5X__dummy_metadata_add_entry(H5X_dummy_head_t *head,
    H5X_dummy_entry_t *entry);
static herr_t H5X__dummy_metadata_remove_entries(H5X_dummy_head_t *head);
static herr_t H5X__dummy_metadata_write(H5X_dummy_metadata_t *metadata,
    hid_t loc_id, size_t *plugin_metadata_size, void **plugin_metadata);
static herr_t H5X__dummy_metadata_read(hid_t loc_id,
    size_t plugin_metadata_size, void *plugin_metadata,
    H5X_dummy_metadata_t *metadata);
static herr_t H5X__dummy_serialize_metadata(hid_t dset_ids[], void *buf,
    size_t *buf_size);
static herr_t H5X__dummy_deserialize_metadata(hid_t file_id, void *buf,
    size_t buf_size, hid_t *dset_ids[]);
static herr_t H5X__dummy_index_rebuild(H5X_dummy_metadata_t *metadata,
    void *bufs[], size_t nelmts[], hid_t type_ids[]);
static herr_t H5X__dummy_metadata_query(H5X_dummy_metadata_t *metadata,
    hid_t query_id, H5X_dummy_head_t *result);
static herr_t H5X__dummy_metadata_query_singleton(H5X_dummy_metadata_t *metadata,
    hid_t query_id, H5X_dummy_head_t *result);
static herr_t H5X__dummy_metadata_query_combine(H5Q_combine_op_t combine_op,
    H5X_dummy_head_t *result1, H5X_dummy_head_t *result2,
    H5X_dummy_head_t *result);

static void *H5X__dummy_create(hid_t loc_id, hid_t xcpl_id, hid_t xapl_id,
    size_t *metadata_size, void **metadata);
static herr_t H5X__dummy_remove(hid_t loc_id, size_t metadata_size,
    void *metadata);
static void *H5X__dummy_open(hid_t loc_id, hid_t xapl_id, size_t
    metadata_size, void *metadata);
static herr_t H5X__dummy_close(void *idx_handle);
static herr_t H5X__dummy_insert_entry(void *idx_handle, hid_t obj_id,
    H5Q_type_t key_type, H5Q_elem_t *key, hid_t xxpl_id);
static herr_t H5X__dummy_remove_entry(void *idx_handle, hid_t obj_id,
    H5Q_type_t key_type, H5Q_elem_t *key, hid_t xxpl_id);
static herr_t H5X__dummy_query(void *idx_handle, hid_t query_id, hid_t xxpl_id,
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

/* Dummy index class */
const H5X_class_t H5X_META_DUMMY[1] = {{
    H5X_CLASS_T_VERS,           /* (From the H5Xpublic.h header file) */
    H5X_PLUGIN_META_DUMMY,      /* (Or whatever number is assigned) */
    "dummy index plugin",       /* Whatever name desired */
    H5X_TYPE_METADATA,          /* This plugin operates on metadata */
    {{
    H5X__dummy_create,          /* create */
    H5X__dummy_remove,          /* remove */
    H5X__dummy_open,            /* open */
    H5X__dummy_close,           /* close */
    H5X__dummy_insert_entry,    /* insert_entry */
    H5X__dummy_remove_entry,    /* remove_entry */
    H5X__dummy_query,           /* query */
    NULL                        /* get_size */
    }}
}};

static void
printf_ref(href_t ref)
{
    char obj_name[H5X_DUMMY_MAX_NAME_LEN];

    H5R__get_obj_name(NULL, H5P_DEFAULT, H5P_DEFAULT, ref, obj_name, H5X_DUMMY_MAX_NAME_LEN);
    if (H5Rget_type(ref) == H5R_EXT_ATTR) {
        char attr_name[H5X_DUMMY_MAX_NAME_LEN];
        H5R__get_attr_name(NULL, ref, attr_name, H5X_DUMMY_MAX_NAME_LEN);
        H5X_DUMMY_LOG_DEBUG("Attribute reference: %s, %s", obj_name, attr_name);
    } else {
        H5X_DUMMY_LOG_DEBUG("Object reference: %s", obj_name);
    }
}

static herr_t
H5X__dummy_index(hid_t oid, const char *name, const H5O_info_t *oinfo,
    void *udata)
{
    H5X_dummy_metadata_t *metadata = (H5X_dummy_metadata_t *) udata;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(name);
    HDassert(oinfo);
    HDassert(metadata);

    /* Index link names */
    if (FAIL == H5X__dummy_index_link_name(metadata, oid, name, oinfo))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTAPPEND, FAIL, "can't add link name");

    /* Index attribute names/values */
    if (FAIL == H5X__dummy_index_attrs(metadata, oid, name))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "can't apply data query to object");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__dummy_index */

static herr_t
H5X__dummy_index_link_name(H5X_dummy_metadata_t *metadata, hid_t loc_id,
    const char *name, const H5O_info_t *oinfo)
{
    href_t ref;
    const char *link_name = NULL;
    const char *trimmed_path = NULL;
    char file_name[H5X_DUMMY_MAX_NAME_LEN];
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(metadata);
    HDassert(name);
    HDassert(oinfo);

    trimmed_path = HDstrrchr(name, '/');
    link_name = (trimmed_path) ? ++trimmed_path : name;

    if ((oinfo->type != H5O_TYPE_GROUP) && (oinfo->type != H5O_TYPE_DATASET))
        HGOTO_ERROR(H5E_QUERY, H5E_BADTYPE, FAIL, "unsupported/unrecognized object type");

    /* Get file name */
    if (H5Fget_name(loc_id, file_name, H5X_DUMMY_MAX_NAME_LEN) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get file name");

    /* Keep object reference */
    if (NULL == (ref = H5R_create_ext_object(file_name, name)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "can't create object reference");
    if (FAIL == H5X__dummy_metadata_add(metadata, ref, H5Q_TYPE_LINK_NAME, link_name))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTAPPEND, FAIL, "can't append object reference to view");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__dummy_index_link_name */

static herr_t
H5X__dummy_index_attrs(H5X_dummy_metadata_t *metadata, hid_t loc_id,
    const char *name)
{
    H5X_dummy_index_attr_arg_t attr_args;
    hid_t obj_id = FAIL;
    char file_name[H5X_DUMMY_MAX_NAME_LEN];
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(metadata);
    HDassert(name);

    /* Get file name */
    if (H5Fget_name(loc_id, file_name, H5X_DUMMY_MAX_NAME_LEN) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get file name");

    /* Build attribute operator info */
    attr_args.filename = file_name;
    attr_args.loc_name = name;
    attr_args.metadata = metadata;

    if (0 == HDstrcmp(name, ".")) {
        obj_id = loc_id;
    } else {
        if (FAIL == (obj_id = H5Oopen(loc_id, name, H5P_DEFAULT)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, FAIL, "can't open object");
    }

    /* Iterate over attributes */
    if (FAIL == (ret_value = H5Aiterate(obj_id, H5_INDEX_NAME, H5_ITER_NATIVE,
        NULL, H5X__dummy_index_attr, &attr_args)))
        HGOTO_ERROR(H5E_ATTR, H5E_BADITER, FAIL, "error iterating over attributes");

done:
    if ((obj_id != FAIL) && (obj_id != loc_id) && (FAIL == H5Oclose(obj_id)))
        HDONE_ERROR(H5E_OHDR, H5E_CANTRELEASE, FAIL, "unable to close object")
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__dummy_index_attrs */

static herr_t
H5X__dummy_index_attr(hid_t loc_id, const char *attr_name,
    const H5A_info_t H5_ATTR_UNUSED *ainfo, void *udata)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(attr_name);
    HDassert(udata);

    if (FAIL == H5X__dummy_index_attr_name(attr_name, udata))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "can't apply attr name query to object");

    if (FAIL == H5X__dummy_index_attr_value(loc_id, attr_name, udata))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "can't apply attr name query to object");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__dummy_index_attr */

static herr_t
H5X__dummy_index_attr_name(const char *attr_name, void *udata)
{
    H5X_dummy_index_attr_arg_t *args = (H5X_dummy_index_attr_arg_t *) udata;
    href_t ref;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(attr_name);
    HDassert(args);

    /* Keep attribute reference */
    if (NULL == (ref = H5R_create_ext_attr(args->filename, args->loc_name, attr_name)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get buffer size for attribute reference");
    if (FAIL == H5X__dummy_metadata_add(args->metadata, ref, H5Q_TYPE_ATTR_NAME, attr_name))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTAPPEND, FAIL, "can't append object reference to view");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__dummy_index_attr_name */

static herr_t
H5X__dummy_index_attr_value(hid_t loc_id, const char *attr_name, void *udata)
{
    H5X_dummy_index_attr_arg_t *args = (H5X_dummy_index_attr_arg_t *) udata;
    void *buf = NULL;
    size_t buf_size;
    hid_t attr_id = FAIL;
    hid_t type_id = FAIL;
    hid_t space_id = FAIL;
    size_t nelmts, elmt_size;
    H5X_dummy_index_attr_elem_arg_t iter_args;
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
    if (FAIL == H5Diterate(buf, type_id, space_id, H5X__dummy_index_attr_value_iterate, &iter_args))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOMPARE, FAIL, "unable to compare attribute elements");

done:
    H5MM_free(buf);
    if (attr_id != FAIL) H5Aclose(attr_id);
    if (type_id != FAIL) H5Tclose(type_id);
    if (space_id != FAIL) H5Sclose(space_id);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__dummy_index_attr_value */

static herr_t
H5X__dummy_index_attr_value_iterate(void *elem, hid_t type_id,
    unsigned H5_ATTR_UNUSED ndim, const hsize_t H5_ATTR_UNUSED *point, void *udata)
{
    H5X_dummy_index_attr_elem_arg_t *args = (H5X_dummy_index_attr_elem_arg_t *) udata;
    href_t ref;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(elem);
    HDassert(args);

    /* Keep attribute reference */
    if (NULL == (ref = H5R_create_ext_attr(args->attr_args->filename, args->attr_args->loc_name, args->attr_name)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get buffer size for attribute reference");
    if (FAIL == H5X__dummy_metadata_add(args->attr_args->metadata, ref, H5Q_TYPE_ATTR_VALUE, type_id, elem))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTAPPEND, FAIL, "can't append object reference to view");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__dummy_index_attr_value_iterate */

static herr_t
H5X__dummy_metadata_add(H5X_dummy_metadata_t *metadata, href_t ref, H5Q_type_t type, ...)
{
    va_list ap;
    H5X_dummy_entry_t *entry;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(metadata);
    HDassert(ref);

    va_start(ap, type);

    if (NULL == (entry = (H5X_dummy_entry_t *) H5MM_malloc(sizeof(H5X_dummy_entry_t))))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTALLOC, FAIL, "can't allocate ref entry");
    entry->ref = ref;
    entry->type = type;

    switch (type) {
        case H5Q_TYPE_ATTR_VALUE:
        {
            hid_t datatype_id;
            size_t datatype_size;
            const void *value;

            /* Get arguments */
            datatype_id = va_arg(ap, hid_t);
            value = va_arg(ap, const void *);

            HDassert(datatype_id != FAIL);
            HDassert(value);

            if (FAIL == (entry->key.elem.type = H5Tget_native_type(datatype_id, H5T_DIR_DEFAULT)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCOPY, FAIL, "can't copy attribute type");
            if (0 == (datatype_size = H5Tget_size(entry->key.elem.type)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a valid size");
            if (NULL == (entry->key.elem.value = H5MM_malloc(datatype_size)))
                HGOTO_ERROR(H5E_QUERY, H5E_CANTALLOC, FAIL, "can't allocate value buffer");
            HDmemcpy(entry->key.elem.value, value, datatype_size);
            H5Q_QUEUE_INSERT_TAIL(&metadata->attr_values, entry, entry);
        }
        break;
        case H5Q_TYPE_ATTR_NAME:
        {
            const char *attr_name;

            /* Get arguments */
            attr_name = va_arg(ap, const char *);

            HDassert(attr_name);

            entry->key.name = H5MM_strdup(attr_name);
            H5Q_QUEUE_INSERT_TAIL(&metadata->attr_names, entry, entry);
        }
        break;
        case H5Q_TYPE_LINK_NAME:
        {
            const char *link_name;

            /* Get arguments */
            link_name = va_arg(ap, const char *);

            HDassert(link_name);

            entry->key.name = H5MM_strdup(link_name);
            H5Q_QUEUE_INSERT_TAIL(&metadata->link_names, entry, entry);
        }
        break;
        case H5Q_TYPE_MISC:
        default:
            HGOTO_ERROR(H5E_QUERY, H5E_BADTYPE, FAIL, "unsupported/unrecognized query type");
            break;
    }

done:
    va_end(ap);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__dummy_metadata_add */

static herr_t
H5X__dummy_metadata_free(H5X_dummy_metadata_t *metadata)
{
    H5X_dummy_head_t *entries[H5X_DUMMY_METADATA_TYPES] = { &metadata->attr_names,
        &metadata->attr_values, &metadata->link_names };
    herr_t ret_value = SUCCEED; /* Return value */
    int i;

    FUNC_ENTER_NOAPI_NOINIT

    for (i = 0; i < H5X_DUMMY_METADATA_TYPES; i++) {
        while (!H5Q_QUEUE_EMPTY(entries[i])) {
            H5X_dummy_entry_t *entry = H5Q_QUEUE_FIRST(entries[i]);
            H5Q_QUEUE_REMOVE_HEAD(entries[i], entry);

            if (FAIL == H5Rdestroy(entry->ref))
                HGOTO_ERROR(H5E_REFERENCE, H5E_CANTCLOSEOBJ, FAIL, "unable to destroy reference");
            switch (entry->type) {
                case H5Q_TYPE_ATTR_VALUE:
                    if (FAIL == H5Tclose(entry->key.elem.type))
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCLOSEOBJ, FAIL, "unable to close datatype");
                    entry->key.elem.value = H5MM_xfree(entry->key.elem.value);
                break;
                case H5Q_TYPE_ATTR_NAME:
                case H5Q_TYPE_LINK_NAME:
                    entry->key.name = H5MM_xfree(entry->key.name);
                break;
                case H5Q_TYPE_MISC:
                default:
                    HGOTO_ERROR(H5E_QUERY, H5E_BADTYPE, FAIL, "unsupported/unrecognized query type");
                    break;
            }
            H5MM_free(entry);
        }
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__dummy_metadata_free */

static herr_t
H5X__dummy_metadata_add_entry(H5X_dummy_head_t *head, H5X_dummy_entry_t *entry)
{
    H5X_dummy_entry_t *new_entry;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(head);
    HDassert(entry);

    if (NULL == (new_entry = (H5X_dummy_entry_t *) H5MM_malloc(sizeof(H5X_dummy_entry_t))))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTALLOC, FAIL, "can't allocate ref entry");
    HDmemcpy(new_entry, entry, sizeof(H5X_dummy_entry_t));
    H5Q_QUEUE_INSERT_TAIL(head, new_entry, entry);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__dummy_metadata_add_entry */

static herr_t
H5X__dummy_metadata_remove_entries(H5X_dummy_head_t *head)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(head);

    while (!H5Q_QUEUE_EMPTY(head)) {
        H5X_dummy_entry_t *entry = H5Q_QUEUE_FIRST(head);
        H5Q_QUEUE_REMOVE_HEAD(head, entry);
        H5MM_free(entry);
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__dummy_metadata_remove_entries */

static herr_t
H5X__dummy_metadata_write(H5X_dummy_metadata_t *metadata, hid_t file_id,
    size_t *plugin_metadata_size, void **plugin_metadata)
{
    hid_t dset_ids[H5X_DUMMY_METADATA_TYPES] = {FAIL, FAIL, FAIL};
    hid_t mem_space_id = FAIL;
    hid_t space_id = FAIL;
    H5X_dummy_head_t *entries[H5X_DUMMY_METADATA_TYPES] = { &metadata->attr_names,
        &metadata->attr_values, &metadata->link_names };
    hid_t type_ids[H5X_DUMMY_METADATA_TYPES] = { H5T_STD_REF_EXT_ATTR,
        FAIL, H5T_STD_REF_EXT_OBJ };
    herr_t ret_value = SUCCEED; /* Return value */
    int i;

    FUNC_ENTER_NOAPI_NOINIT

    /* Iterate over reference types and write references if any */
    /* We only need to store attribute/object references and attribute values */
    for (i = 0; i < H5X_DUMMY_METADATA_TYPES; i++) {
        H5X_dummy_entry_t *entry = NULL;
        hsize_t n_elem = entries[i]->n_elem;
        hsize_t start = 0;

        if (!n_elem)
            continue;

        /* Create dataspace */
        if (FAIL == (space_id = H5Screate_simple(1, &n_elem, NULL)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCREATE, FAIL, "unable to create dataspace");

        /* Create the new dataset & get its ID */
        if (H5Q_QUEUE_FIRST(entries[i])->type == H5Q_TYPE_ATTR_VALUE)
            type_ids[i] = H5Q_QUEUE_FIRST(entries[i])->key.elem.type;
        if (FAIL == (dset_ids[i] = H5Dcreate_anon(file_id, type_ids[i],
            space_id, H5P_DEFAULT, H5P_DEFAULT)))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create anonymous dataset");

        /* Increment refcount so that anonymous dataset is persistent */
        if (FAIL == H5Oincr_refcount(dset_ids[i]))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTINC, FAIL, "can't increment dataset refcount");

        /* Iterate over reference entries in view */
        H5Q_QUEUE_FOREACH(entry, entries[i], entry) {
            hsize_t count = 1;
            const void *buf = (entry->type == H5Q_TYPE_ATTR_VALUE) ? entry->key.elem.value : &entry->ref;

            if (FAIL == (mem_space_id = H5Screate_simple(1, &count, NULL)))
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCREATE, FAIL, "unable to create dataspace");
            if (FAIL == H5Sselect_hyperslab(space_id, H5S_SELECT_SET, &start, NULL, &count, NULL))
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to set hyperslab selection")
            if (FAIL == H5Dwrite(dset_ids[i], type_ids[i], mem_space_id,
                space_id, H5P_DEFAULT, buf))
                HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "unable to write dataset");
            if (FAIL == H5Sclose(mem_space_id))
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCLOSEOBJ, FAIL, "unable to close dataspace");
            mem_space_id = FAIL;

            /* Increment reference position in file */
            start++;
        }
        if (FAIL == H5Sclose(space_id))
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCLOSEOBJ, FAIL, "unable to close dataspace");
        space_id = FAIL;
    }

    /* Serialize metadata */
    if (FAIL == H5X__dummy_serialize_metadata(dset_ids, NULL, plugin_metadata_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get plugin metadata size");
    if (NULL == (*plugin_metadata = H5MM_malloc(*plugin_metadata_size)))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate plugin metadata");
    if (FAIL == H5X__dummy_serialize_metadata(dset_ids, *plugin_metadata, plugin_metadata_size))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTENCODE, FAIL, "can't serialize plugin metadata");

    for (i = 0; i < H5X_DUMMY_METADATA_TYPES; i++) {
        if (FAIL == H5Dclose(dset_ids[i]))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to close dataset");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__dummy_metadata_write */


static herr_t
H5X__dummy_metadata_read(hid_t file_id, size_t plugin_metadata_size,
    void *plugin_metadata, H5X_dummy_metadata_t *metadata)
{
    hid_t *dset_ids;
    herr_t ret_value = SUCCEED; /* Return value */
    void *bufs[H5X_DUMMY_METADATA_TYPES];
    size_t nelmts[H5X_DUMMY_METADATA_TYPES];
    hid_t type_ids[H5X_DUMMY_METADATA_TYPES];
    int i;

    FUNC_ENTER_NOAPI_NOINIT

    /* Deserialize metadata and get anon dataset IDs */
    if (FAIL == H5X__dummy_deserialize_metadata(file_id, plugin_metadata, plugin_metadata_size,
        &dset_ids))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTDECODE, FAIL, "can't deserialize plugin metadata");

    /* Iterate over metadata index types */
    for (i = 0; i < H5X_DUMMY_METADATA_TYPES; i++) {
        hid_t type_id = FAIL, space_id = FAIL;
        size_t elmt_size;

        if (FAIL == (type_id = H5Dget_type(dset_ids[i])))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "unable to get datatype");
        if (FAIL == (space_id = H5Dget_space(dset_ids[i])))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "unable to get datatype");
        if (0 == (nelmts[i] = (size_t) H5Sget_select_npoints(space_id)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "invalid number of elements");
        if (0 == (elmt_size = H5Tget_size(type_id)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "invalid size of element");

        /* Allocate buffer to hold data */
        if (NULL == (bufs[i] = H5MM_malloc(nelmts[i] * elmt_size)))
            HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate read buffer");

        /* Read data from dataset */
        if (FAIL == H5Dread(dset_ids[i], type_id, H5S_ALL, space_id,
                H5P_DEFAULT, bufs[i]))
            HGOTO_ERROR(H5E_INDEX, H5E_READERROR, FAIL, "can't read index data");

        if (FAIL == H5Sclose(space_id))
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCLOSEOBJ, FAIL, "unable to close dataspace");
    }

    /* Rebuild metadata index */
    if (FAIL == H5X__dummy_index_rebuild(metadata, bufs, nelmts, type_ids))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTINIT, FAIL, "unable to rebuild metadata index");

    /* Close anon datasets */
    for (i = 0; i < H5X_DUMMY_METADATA_TYPES; i++) {
        H5MM_free(bufs[i]);
        if (FAIL == H5Tclose(type_ids[i]))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCLOSEOBJ, FAIL, "unable to close datatype");
        if (FAIL == H5Dclose(dset_ids[i]))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to close dataset");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__dummy_metadata_read */

static herr_t
H5X__dummy_serialize_metadata(hid_t dset_ids[], void *buf, size_t *buf_size)
{
    uint8_t *p = buf;
    size_t metadata_size = 3 * sizeof(haddr_t);
    int i;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    for (i = 0; i < H5X_DUMMY_METADATA_TYPES; i++) {
        /* Encode metadata token info */
        if (p) {
            H5O_info_t dset_info;

            /* Get addr info for dataset */
            if (FAIL == H5Oget_info(dset_ids[i], &dset_info))
                HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get dataset info");

            /* TODO use H5F_addr_encode instead */
            HDmemcpy(p, &dset_info.addr, sizeof(haddr_t));
            p += sizeof(haddr_t);
        }
    }

    if (buf_size) *buf_size = metadata_size;

 done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__dummy_serialize_metadata */

static herr_t
H5X__dummy_deserialize_metadata(hid_t file_id, void *buf, size_t buf_size,
    hid_t *dset_ids[])
{
    uint8_t *p = buf;
    int i;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(buf);
    HDassert(buf_size);
    HDassert(dset_ids);

    for (i = 0; i < H5X_DUMMY_METADATA_TYPES; i++) {
        /* Decode index token info */
        if (FAIL == (*dset_ids[i] = H5Oopen_by_addr(file_id, *((haddr_t *) p))))
            HGOTO_ERROR(H5E_INDEX, H5E_CANTOPENOBJ, FAIL, "can't open anonymous dataset");
        p += sizeof(haddr_t);
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5X__dummy_deserialize_metadata */

static herr_t
H5X__dummy_index_rebuild(H5X_dummy_metadata_t *metadata, void *bufs[],
    size_t nelmts[], hid_t type_ids[])
{
//    H5X_dummy_head_t *entries[H5X_DUMMY_METADATA_TYPES] = { &metadata->attr_names,
//        &metadata->attr_values, &metadata->link_names };
//    href_t *link_refs;
//    href_t *attr_refs;
//    void **attr_values;
//    hid_t attr_native_id;
//    int i;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(metadata);
    HDassert(bufs);
    HDassert(nelmts);
    HDassert(type_ids);

//    attr_refs = bufs[0];
//    attr_values = bufs[1];
//    link_refs = bufs[2];
//    attr_native_id = H5Tget_native_type(type_ids[1], H5T_DIR_DEFAULT);
//
//    for (i = 0; i < nelmts[0]; i++) {
//        href_t ref;
//        char attr_name[H5X_DUMMY_MAX_NAME_LEN];
//        char filename[H5X_DUMMY_MAX_NAME_LEN];
//        char pathname[H5X_DUMMY_MAX_NAME_LEN];
//
//        H5R__get_attr_name(NULL, attr_refs[i], attr_name, H5X_DUMMY_MAX_NAME_LEN);
//        H5R__get_obj_name(NULL, attr_refs[i], pathname, H5X_DUMMY_MAX_NAME_LEN);
//        H5Rget_file_name(attr_refs[i], attr_name, H5X_DUMMY_MAX_NAME_LEN);
//        ref = H5R_create_ext_attr(filename, pathname, attr_name);
//        if (FAIL == H5X__dummy_metadata_add(metadata, attr_refs[i], H5Q_TYPE_ATTR_NAME, attr_name))
//            HGOTO_ERROR(H5E_QUERY, H5E_CANTAPPEND, FAIL, "can't append object reference to view");
//    }

//done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5X__dummy_index_rebuild */

/*-------------------------------------------------------------------------
 * Function:    H5X_dummy_create
 *
 * Purpose: This function creates a new instance of a dummy plugin index.
 *
 * Return:  Success:    Pointer to the new index
 *          Failure:    NULL
 *
 *------------------------------------------------------------------------
 */
static void *
H5X__dummy_create(hid_t loc_id, hid_t H5_ATTR_UNUSED xcpl_id,
    hid_t H5_ATTR_UNUSED xapl_id, size_t *metadata_size, void **metadata)
{
    H5X_dummy_t *dummy = NULL;
    hid_t file_id;
    void *ret_value = NULL; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    H5X_DUMMY_LOG_DEBUG("Calling H5X_dummy_create");

    /* Create new dummy instance */
    if (NULL == (dummy = (H5X_dummy_t *) H5MM_malloc(sizeof(H5X_dummy_t))))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, NULL, "can't allocate dummy struct");
    H5Q_QUEUE_INIT(&dummy->metadata.attr_names);
    H5Q_QUEUE_INIT(&dummy->metadata.attr_values);
    H5Q_QUEUE_INIT(&dummy->metadata.link_names);

    /* Get file ID */
    file_id = loc_id; /* TODO for now */

    /* Visit file */
    if (FAIL == H5Ovisit(loc_id, H5_INDEX_NAME, H5_ITER_NATIVE, H5X__dummy_index,
        &dummy->metadata))
        HGOTO_ERROR(H5E_SYM, H5E_BADITER, NULL, "object visitation failed");

    H5X_DUMMY_LOG_DEBUG("###########################");
    if (!H5Q_QUEUE_EMPTY(&dummy->metadata.attr_names))
        H5X_DUMMY_LOG_DEBUG("Indexed %zu attributes names", dummy->metadata.attr_names.n_elem);
    if (!H5Q_QUEUE_EMPTY(&dummy->metadata.attr_values))
        H5X_DUMMY_LOG_DEBUG("Indexed %zu attributes values", dummy->metadata.attr_values.n_elem);
    if (!H5Q_QUEUE_EMPTY(&dummy->metadata.link_names))
        H5X_DUMMY_LOG_DEBUG("Indexed %zu link names", dummy->metadata.link_names.n_elem);
    H5X_dummy_entry_t *entry;
    H5Q_QUEUE_FOREACH(entry, &dummy->metadata.attr_names, entry) {
        H5X_DUMMY_LOG_DEBUG("Indexed attribute name: %s", (const char *) entry->key.name);
    }
    H5Q_QUEUE_FOREACH(entry, &dummy->metadata.attr_values, entry) {
        H5X_DUMMY_LOG_DEBUG("Indexed attribute value: %d", *((int *) entry->key.elem.value));
    }
    H5Q_QUEUE_FOREACH(entry, &dummy->metadata.link_names, entry) {
        H5X_DUMMY_LOG_DEBUG("Indexed link name: %s", (const char *) entry->key.name);
    }
    H5X_DUMMY_LOG_DEBUG("###########################");

    /* Write index metadata */
    if (FAIL == H5X__dummy_metadata_write(&dummy->metadata, file_id, metadata_size, metadata))
        HGOTO_ERROR(H5E_INDEX, H5E_WRITEERROR, NULL, "can't write metadata");

    ret_value = dummy;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_dummy_create() */

/*-------------------------------------------------------------------------
 * Function:    H5X_dummy_remove
 *
 * Purpose: This function removes the dummy plugin index from the file.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__dummy_remove(hid_t H5_ATTR_UNUSED file_id, size_t H5_ATTR_UNUSED metadata_size,
        void H5_ATTR_UNUSED *metadata)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    H5X_DUMMY_LOG_DEBUG("Calling H5X_dummy_remove");

    /* TODO Does not do anything */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_dummy_remove() */

/*-------------------------------------------------------------------------
 * Function:    H5X_dummy_open
 *
 * Purpose: This function opens an already existing dummy index from a file.
 *
 * Return:  Success:    Pointer to the index
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5X__dummy_open(hid_t loc_id, hid_t H5_ATTR_UNUSED xapl_id, size_t metadata_size,
    void *metadata)
{
    hid_t file_id;
    H5X_dummy_t *dummy = NULL;
    void *ret_value = NULL; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    H5X_DUMMY_LOG_DEBUG("Calling H5X_dummy_open");

    if (!metadata_size)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "NULL metadata size");
    if (!metadata)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "NULL metadata");

    if (NULL == (dummy = (H5X_dummy_t *) H5MM_malloc(sizeof(H5X_dummy_t))))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, NULL, "can't allocate dummy struct");

    /* Get file ID */
    file_id = loc_id; /* TODO for now */

    if (FAIL == H5X__dummy_metadata_read(file_id, metadata_size, metadata,
        &dummy->metadata))
        HGOTO_ERROR(H5E_INDEX, H5E_READERROR, NULL, "can't read metadata");

    ret_value = dummy;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_dummy_open() */

/*-------------------------------------------------------------------------
 * Function:    H5X_dummy_close
 *
 * Purpose: This function closes a dummy index.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__dummy_close(void *idx_handle)
{
    H5X_dummy_t *dummy = (H5X_dummy_t *) idx_handle;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    H5X_DUMMY_LOG_DEBUG("Calling H5X_dummy_close");

    if (NULL == dummy)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");

    /* Free metadata */
    if (FAIL == H5X__dummy_metadata_free(&dummy->metadata))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTFREE, FAIL, "cannot free metadata");

    H5MM_free(dummy);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_dummy_close() */

/*-------------------------------------------------------------------------
 * Function:    H5X__dummy_insert_entry
 *
 * Purpose: This function insert a new entry in the dummy plugin index.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__dummy_insert_entry(void *idx_handle, hid_t obj_id, H5Q_type_t key_type,
    H5Q_elem_t *key, hid_t H5_ATTR_UNUSED xxpl_id)
{
    H5X_dummy_t *dummy = (H5X_dummy_t *) idx_handle;
    char file_name[H5X_DUMMY_MAX_NAME_LEN];
    char loc_name[H5X_DUMMY_MAX_NAME_LEN];
    href_t ref;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    H5X_DUMMY_LOG_DEBUG("Calling H5X__dummy_insert_entry");

    if (NULL == dummy)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");

    if (H5Fget_name(obj_id, file_name, H5X_DUMMY_MAX_NAME_LEN) < 0)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get file name");
    if (H5Iget_name(obj_id, loc_name, H5X_DUMMY_MAX_NAME_LEN) < 0)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get location name");

    switch (key_type) {
        case H5Q_TYPE_ATTR_VALUE:
        {
            char attr_name[H5X_DUMMY_MAX_NAME_LEN];

            if (H5Aget_name(obj_id, H5X_DUMMY_MAX_NAME_LEN, attr_name) < 0)
                HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get attribute name");

            /* Keep attribute reference */
            if (NULL == (ref = H5R_create_ext_attr(file_name, loc_name, attr_name)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get buffer size for attribute reference");
            if (FAIL == H5X__dummy_metadata_add(&dummy->metadata, ref, H5Q_TYPE_ATTR_VALUE, key->value.type, key->value.value))
                HGOTO_ERROR(H5E_QUERY, H5E_CANTAPPEND, FAIL, "can't append object reference to view");
        }
            break;
        case H5Q_TYPE_ATTR_NAME:
        {
            /* Keep attribute reference */
            if (NULL == (ref = H5R_create_ext_attr(file_name, loc_name, key->name)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get buffer size for attribute reference");
            if (FAIL == H5X__dummy_metadata_add(&dummy->metadata, ref, H5Q_TYPE_ATTR_NAME, key->name))
                HGOTO_ERROR(H5E_QUERY, H5E_CANTAPPEND, FAIL, "can't append object reference to view");
        }
            break;
        case H5Q_TYPE_LINK_NAME:
        {
            /* Keep object reference */
            if (NULL == (ref = H5R_create_ext_object(file_name, loc_name)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "can't create object reference");
            if (FAIL == H5X__dummy_metadata_add(&dummy->metadata, ref, H5Q_TYPE_LINK_NAME, key->name))
                HGOTO_ERROR(H5E_QUERY, H5E_CANTAPPEND, FAIL, "can't append object reference to view");
        }
        break;
        case H5Q_TYPE_MISC:
        default:
            HGOTO_ERROR(H5E_QUERY, H5E_BADTYPE, FAIL, "unsupported/unrecognized query type");
            break;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__dummy_insert_entry() */

/*-------------------------------------------------------------------------
 * Function:    H5X__dummy_remove_entry
 *
 * Purpose: This function removes an entry from the dummy plugin index.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__dummy_remove_entry(void H5_ATTR_UNUSED *idx_handle, hid_t H5_ATTR_UNUSED obj_id,
    H5Q_type_t H5_ATTR_UNUSED key_type, H5Q_elem_t H5_ATTR_UNUSED *key,
    hid_t H5_ATTR_UNUSED xxpl_id)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    H5X_DUMMY_LOG_DEBUG("Calling H5X__dummy_remove_entry");

    /* TODO Does not do anything */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__dummy_insert_entry() */

/*-------------------------------------------------------------------------
 * Function:    H5X_dummy_query
 *
 * Purpose: This function queries a dummy index.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__dummy_query(void *idx_handle, hid_t query_id, hid_t H5_ATTR_UNUSED xxpl_id,
    size_t *ref_count, href_t *refs[])
{
    H5X_dummy_t *dummy = (H5X_dummy_t *) idx_handle;
    H5X_dummy_head_t query_result = H5Q_QUEUE_HEAD_INITIALIZER(query_result);
    href_t *ref_buf = NULL;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    H5X_DUMMY_LOG_DEBUG("Calling H5X_dummy_query");

    if (NULL == dummy)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");

    /* We assume here that the queries passed only operate on metadata */
    if (FAIL == H5X__dummy_metadata_query(&dummy->metadata, query_id, &query_result))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCOMPARE, FAIL, "can't query metadata");

//    H5X_DUMMY_LOG_DEBUG("###########################");
//    if (!H5Q_QUEUE_EMPTY(&query_result)) {
//        H5X_dummy_entry_t *entry;
//        H5X_DUMMY_LOG_DEBUG("Query returns %zu references", query_result.n_elem);
//        H5Q_QUEUE_FOREACH(entry, &query_result, entry)
//            printf_ref(entry->ref);
//    }
//    H5X_DUMMY_LOG_DEBUG("###########################");

    /* Fill result */
    if (!H5Q_QUEUE_EMPTY(&query_result)) {
        H5X_dummy_entry_t *entry;
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
    if (FAIL == H5X__dummy_metadata_remove_entries(&query_result))
        HDONE_ERROR(H5E_QUERY, H5E_CANTFREE, FAIL, "unable to remove entries");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_dummy_query() */

static herr_t
H5X__dummy_metadata_query(H5X_dummy_metadata_t *metadata, hid_t query_id,
    H5X_dummy_head_t *result)
{
    H5Q_type_t query_type;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if (FAIL == H5Qget_type(query_id, &query_type))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTGET, FAIL, "unable to get query type");

    if (query_type != H5Q_TYPE_MISC) {
        if (FAIL == H5X__dummy_metadata_query_singleton(metadata, query_id, result))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "unable to compare query");
    } else {
        H5Q_combine_op_t combine_op;
        hid_t sub_query1_id, sub_query2_id;
        H5X_dummy_head_t result1 = H5Q_QUEUE_HEAD_INITIALIZER(result1);
        H5X_dummy_head_t result2 = H5Q_QUEUE_HEAD_INITIALIZER(result2);

        if (FAIL == H5Qget_combine_op(query_id, &combine_op))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTGET, FAIL, "unable to get combine op");
        if (FAIL == H5Qget_components(query_id, &sub_query1_id, &sub_query2_id))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTGET, FAIL, "unable to get components");

        if (FAIL == H5X__dummy_metadata_query(metadata, sub_query1_id, &result1))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "unable to apply query");

//        H5X_DUMMY_LOG_DEBUG("###########################");
//        if (!H5Q_QUEUE_EMPTY(&result1)) {
//            H5X_dummy_entry_t *entry;
//            H5X_DUMMY_LOG_DEBUG("Query returns %zu references", result1.n_elem);
//            H5Q_QUEUE_FOREACH(entry, &result1, entry)
//                printf_ref(entry->ref);
//        }
//        H5X_DUMMY_LOG_DEBUG("###########################");

        if (FAIL == H5X__dummy_metadata_query(metadata, sub_query2_id, &result2))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "unable to apply query");

//        H5X_DUMMY_LOG_DEBUG("###########################");
//        if (!H5Q_QUEUE_EMPTY(&result2)) {
//            H5X_dummy_entry_t *entry;
//            H5X_DUMMY_LOG_DEBUG("Query returns %zu references", result2.n_elem);
//            H5Q_QUEUE_FOREACH(entry, &result2, entry)
//                printf_ref(entry->ref);
//        }
//        H5X_DUMMY_LOG_DEBUG("###########################");

        if (FAIL == H5X__dummy_metadata_query_combine(combine_op, &result1, &result2, result))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTMERGE, FAIL, "unable to merge results");

//        H5X_DUMMY_LOG_DEBUG("###########################");
//        if (!H5Q_QUEUE_EMPTY(result)) {
//            H5X_dummy_entry_t *entry;
//            H5X_DUMMY_LOG_DEBUG("Query returns %zu references", result->n_elem);
//            H5Q_QUEUE_FOREACH(entry, result, entry)
//                printf_ref(entry->ref);
//        }
//        H5X_DUMMY_LOG_DEBUG("###########################");

        if (FAIL == H5X__dummy_metadata_remove_entries(&result1))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTFREE, FAIL, "unable to remove entries");
        if (FAIL == H5X__dummy_metadata_remove_entries(&result2))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTFREE, FAIL, "unable to remove entries");
        if (FAIL == H5Qclose(sub_query1_id))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTCLOSEOBJ, FAIL, "unable to close query");
        if (FAIL == H5Qclose(sub_query2_id))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTCLOSEOBJ, FAIL, "unable to close query");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__dummy_metadata_query */

static herr_t
H5X__dummy_metadata_query_singleton(H5X_dummy_metadata_t *metadata,
    hid_t query_id, H5X_dummy_head_t *result)
{
    H5X_dummy_entry_t *entry;
    H5Q_type_t query_type;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if (FAIL == H5Qget_type(query_id, &query_type))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTGET, FAIL, "unable to get query type");
    switch (query_type) {
        case H5Q_TYPE_LINK_NAME:
            H5Q_QUEUE_FOREACH(entry, &metadata->link_names, entry) {
                hbool_t apply_result = FALSE;
                if (FAIL == H5Qapply_atom(query_id, &apply_result, entry->key.name))
                    HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "can't compare link name");
                if (!apply_result) continue;

//                H5X_DUMMY_LOG_DEBUG("Match link name: %s", (const char *) entry->key.name);
                if (FAIL == H5X__dummy_metadata_add_entry(result, entry))
                    HGOTO_ERROR(H5E_INDEX, H5E_CANTMERGE, FAIL, "unable to add result");
            }
            break;
        case H5Q_TYPE_ATTR_NAME:
            H5Q_QUEUE_FOREACH(entry, &metadata->attr_names, entry) {
                hbool_t apply_result = FALSE;
                if (FAIL == H5Qapply_atom(query_id, &apply_result, entry->key.name))
                    HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "can't compare attribute name");
                if (!apply_result) continue;

//                H5X_DUMMY_LOG_DEBUG("Match attribute name: %s", (const char *) entry->key.name);
                if (FAIL == H5X__dummy_metadata_add_entry(result, entry))
                    HGOTO_ERROR(H5E_INDEX, H5E_CANTMERGE, FAIL, "unable to add result");
            }
            break;
        case H5Q_TYPE_ATTR_VALUE:
            H5Q_QUEUE_FOREACH(entry, &metadata->attr_values, entry) {
                hbool_t apply_result = FALSE;
                if (FAIL == H5Qapply_atom(query_id, &apply_result, entry->key.elem.type, entry->key.elem.value))
                    HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "can't compare attribute values");
                if (!apply_result) continue;

//                H5X_DUMMY_LOG_DEBUG("Match attribute value: %d", *((int *) entry->key.elem.value));
                if (FAIL == H5X__dummy_metadata_add_entry(result, entry))
                    HGOTO_ERROR(H5E_INDEX, H5E_CANTMERGE, FAIL, "unable to add result");
            }
            break;
        case H5Q_TYPE_DATA_ELEM:
        case H5Q_TYPE_MISC:
        default:
            HGOTO_ERROR(H5E_QUERY, H5E_BADTYPE, FAIL, "unsupported/unrecognized query type");
            break;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__dummy_metadata_query_singleton */

static herr_t
H5X__dummy_metadata_query_combine(H5Q_combine_op_t combine_op,
    H5X_dummy_head_t *result1, H5X_dummy_head_t *result2, H5X_dummy_head_t *result)
{
    H5X_dummy_entry_t *entry1;
    H5X_dummy_entry_t *entry2;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if (combine_op == H5Q_COMBINE_AND) {
        H5Q_QUEUE_FOREACH(entry1, result1, entry) {
            H5R_type_t ref_type1 = H5Rget_type(entry1->ref);
            H5Q_QUEUE_FOREACH(entry2, result2, entry) {
                H5R_type_t ref_type2 = H5Rget_type(entry2->ref);

                if (H5Requal(entry1->ref, entry2->ref)) {
                    if (FAIL == H5X__dummy_metadata_add_entry(result, entry1))
                        HGOTO_ERROR(H5E_INDEX, H5E_CANTMERGE, FAIL, "unable to add result");
                }
                else if (ref_type1 == H5R_EXT_ATTR && ref_type2 == H5R_EXT_OBJECT) {
                    char obj_name1[H5X_DUMMY_MAX_NAME_LEN];
                    char obj_name2[H5X_DUMMY_MAX_NAME_LEN];

                    /* Only combine if obj_names are equal */
                    H5R__get_obj_name(NULL, H5P_DEFAULT, H5P_DEFAULT, entry1->ref,
                        obj_name1, H5X_DUMMY_MAX_NAME_LEN);
                    H5R__get_obj_name(NULL, H5P_DEFAULT, H5P_DEFAULT, entry2->ref,
                        obj_name2, H5X_DUMMY_MAX_NAME_LEN);
                    if (0 == HDstrcmp(obj_name1, obj_name2))
                        if (FAIL == H5X__dummy_metadata_add_entry(result, entry2))
                            HGOTO_ERROR(H5E_INDEX, H5E_CANTMERGE, FAIL, "unable to add result");
                }
                else if (ref_type1 == H5R_EXT_OBJECT && ref_type2 == H5R_EXT_ATTR) {
                    char obj_name1[H5X_DUMMY_MAX_NAME_LEN];
                    char obj_name2[H5X_DUMMY_MAX_NAME_LEN];

                    /* Only combine if obj_names are equal */
                    H5R__get_obj_name(NULL, H5P_DEFAULT, H5P_DEFAULT, entry1->ref,
                        obj_name1, H5X_DUMMY_MAX_NAME_LEN);
                    H5R__get_obj_name(NULL, H5P_DEFAULT, H5P_DEFAULT, entry2->ref,
                        obj_name2, H5X_DUMMY_MAX_NAME_LEN);
                    if (0 == HDstrcmp(obj_name1, obj_name2))
                        if (FAIL == H5X__dummy_metadata_add_entry(result, entry1))
                            HGOTO_ERROR(H5E_INDEX, H5E_CANTMERGE, FAIL, "unable to add result");
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
} /* end H5X__dummy_metadata_query_combine */
