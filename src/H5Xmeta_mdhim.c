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
 * Purpose:	MDHIM index routines.
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

#ifdef H5_HAVE_MDHIM
#include <mdhim.h>

/****************/
/* Local Macros */
/****************/
//#define H5X_MDHIM_DEBUG

#ifdef H5X_MDHIM_DEBUG
#define H5X_MDHIM_LOG_DEBUG(...) do {                              \
      fprintf(stdout, " # %s(): ", __func__);                   \
      fprintf(stdout, __VA_ARGS__);                             \
      fprintf(stdout, "\n");                                    \
      fflush(stdout);                                           \
  } while (0)
#else
#define H5X_MDHIM_LOG_DEBUG(...) do { \
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

#define H5X_MDHIM_NPRIMARIES 2
#define H5X_MDHIM_NSECONDARIES 3
#define H5X_MDHIM_MAX_NAME_LEN (64 * 1024)

#define H5X_MDHIM_MANIFEST_SUFFIX      "__manifest"

#define H5X_MDHIM_OBJ_REFS_SUFFIX      "__obj_refs_ldb"
#define H5X_MDHIM_LINK_NAMES_SUFFIX    "__link_names_ldb"
#define H5X_MDHIM_ATTR_REFS_SUFFIX     "__attr_refs_ldb"
#define H5X_MDHIM_ATTR_NAMES_SUFFIX    "__attr_names_ldb"
#define H5X_MDHIM_ATTR_VALUES_SUFFIX   "__attr_values_ldb"

#define H5X_MDHIM_DB_TYPE   LEVELDB
#define H5X_MDHIM_DB_PATH   "./"


/******************/
/* Local Typedefs */
/******************/

typedef struct H5X_mdhim_entry_t H5X_mdhim_entry_t;

struct H5X_mdhim_entry_t {
    href_t ref;                 /* External reference */
    H5Q_QUEUE_ENTRY(H5X_mdhim_entry_t) entry;
};

typedef H5Q_QUEUE_HEAD(H5X_mdhim_head_t, H5X_mdhim_entry_t) H5X_mdhim_head_t;

typedef struct H5X_mdhim_query_entry_t H5X_mdhim_query_entry_t;

struct H5X_mdhim_query_entry_t {
    H5Q_type_t type;            /* Query type */
    hid_t query_id;             /* Query ID */
    H5Q_QUEUE_ENTRY(H5X_mdhim_query_entry_t) entry;
};

typedef H5Q_QUEUE_HEAD(H5X_mdhim_query_head_t, H5X_mdhim_query_entry_t) H5X_mdhim_query_head_t;

typedef struct H5X_mdhim_t {
    char *manifest_path;

    char *obj_refs_path;        /* Database path for objects */
    char *attr_refs_path;       /* Database path for attributes */
    char *link_names_idx_name;  /* Index name for link names */
    char *attr_values_idx_name; /* Index name for attribute values */
    char *attr_names_idx_name;  /* Index name for attribute names */

    struct mdhim_t *obj_refs_md;  /* Primary database for objects */
    struct mdhim_t *attr_refs_md; /* Primary database for attributes */
    struct index_t *link_names_idx;     /* Secondary index for link names */
    struct index_t *attr_names_idx;     /* Secondary index for attribute names */
    struct index_t *attr_values_idx;    /* Secondary index for attribute values */
} H5X_mdhim_t;

typedef struct H5X_mdhim_obj_t {
    const char *name;
} H5X_mdhim_obj_t;

typedef struct H5X_mdhim_attr_t {
    const char *name;
    const char *link_name;
    void *elmts;
    size_t elmt_size;
    size_t nelmts;
//    href_t obj_ref;
} H5X_mdhim_attr_t;

typedef struct {
    const char *filename;
    const char *loc_name;
    H5X_mdhim_t *db;
} H5X_mdhim_index_attr_arg_t;

typedef struct {
    H5X_mdhim_index_attr_arg_t *attr_args;
    const char *attr_name;
} H5X_mdhim_index_attr_elem_arg_t;

/********************/
/* Local Prototypes */
/********************/

static herr_t H5X__mdhim_index(hid_t oid, const char *name, const H5O_info_t *oinfo,
    void *udata);
static herr_t H5X__mdhim_index_obj(H5X_mdhim_t *db, hid_t loc_id,
    const char *name, const H5O_info_t *oinfo);
static herr_t H5X__mdhim_index_attrs(H5X_mdhim_t *db, hid_t loc_id,
    const char *name);
static herr_t H5X__mdhim_index_attr(hid_t loc_id, const char *attr_name,
    const H5A_info_t H5_ATTR_UNUSED *ainfo, void *udata);
//static herr_t H5X__mdhim_index_attr_value_iterate(void *elem, hid_t type_id,
//    unsigned ndim, const hsize_t *point, void *udata);
static herr_t H5X__mdhim_insert_obj(H5X_mdhim_t *db, href_t ref, H5X_mdhim_obj_t *obj);
static herr_t H5X__mdhim_insert_attr(H5X_mdhim_t *db, href_t ref, H5X_mdhim_attr_t *attr);
#ifndef H5X_MDHIM_USE_JOIN
static herr_t H5X__mdhim_push_entry(H5X_mdhim_head_t *head,
    H5X_mdhim_entry_t *entry);
#endif
static herr_t H5X__mdhim_remove_entries(H5X_mdhim_head_t *head);
static herr_t H5X__mdhim_gen_file_names(H5X_mdhim_t *db, hid_t loc_id);
static herr_t H5X__mdhim_free_file_names(H5X_mdhim_t *db);

static int H5X__mdhim_remove_directory(const char *path);

static herr_t H5X__mdhim_create_tables(H5X_mdhim_t *db, hbool_t open, unsigned flags);
static herr_t H5X__mdhim_close_tables(H5X_mdhim_t *db);
static herr_t H5X__mdhim_remove_tables(H5X_mdhim_t *db);
static void H5X__mdhim_stat(H5X_mdhim_t *db);
static herr_t H5X__mdhim_metadata_write(H5X_mdhim_t *db,
    size_t *plugin_metadata_size, void **plugin_metadata);
static herr_t H5X__mdhim_metadata_read(size_t plugin_metadata_size,
    void *plugin_metadata, H5X_mdhim_t *metadata);
static herr_t H5X__mdhim_query_components(H5X_mdhim_t *db, hid_t query_id,
    H5X_mdhim_head_t *result);
static herr_t H5X__mdhim_query_singleton(H5X_mdhim_t *db, hid_t query_id,
    H5X_mdhim_head_t *result);
#ifndef H5X_MDHIM_USE_JOIN
static herr_t H5X__mdhim_query_combine(H5Q_combine_op_t combine_op,
    H5X_mdhim_head_t *result1, H5X_mdhim_head_t *result2, H5X_mdhim_head_t *result);
#else
static herr_t H5X__mdhim_query_combine(H5X_mdhim_t *db, hid_t query_id,
    H5X_mdhim_head_t *result);
#endif

static void *H5X__mdhim_create(hid_t loc_id, hid_t xcpl_id, hid_t xapl_id,
    size_t *metadata_size, void **metadata);
static herr_t H5X__mdhim_remove(hid_t loc_id, size_t metadata_size,
    void *metadata);
static void *H5X__mdhim_open(hid_t loc_id, hid_t xapl_id, size_t
    metadata_size, void *metadata);
static herr_t H5X__mdhim_close(void *idx_handle);
static herr_t H5X__mdhim_insert_entry(void *idx_handle, hid_t obj_id,
    H5Q_type_t key_type, H5Q_elem_t *key, hid_t xxpl_id);
static herr_t H5X__mdhim_remove_entry(void *idx_handle, hid_t obj_id,
    H5Q_type_t key_type, H5Q_elem_t *key, hid_t xxpl_id);
static herr_t H5X__mdhim_query(void *idx_handle, hid_t query_id, hid_t xxpl_id,
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
    H5X__mdhim_create,         /* create */
    H5X__mdhim_remove,         /* remove */
    H5X__mdhim_open,           /* open */
    H5X__mdhim_close,          /* close */
    H5X__mdhim_insert_entry,   /* insert_entry */
    H5X__mdhim_remove_entry,   /* remove_entry */
    H5X__mdhim_query,          /* query */
    NULL                       /* get_size */
}};

const H5X_class_t H5X_META_MDHIM[1] = {{
    H5X_CLASS_T_VERS,       /* (From the H5Xpublic.h header file) */
    H5X_PLUGIN_META_MDHIM,  /* (Or whatever number is assigned) */
    "MDHIM",                /* Whatever name desired */
    H5X_TYPE_METADATA,      /* This plugin operates on metadata */
    &idx_class              /* Index class */
}};

#ifdef H5X_MDHIM_DEBUG
static void
print_ref(href_t ref)
{
    char obj_name[H5X_MDHIM_MAX_NAME_LEN];

    H5R__get_obj_name(NULL, H5P_DEFAULT, H5P_DEFAULT, ref, obj_name, H5X_MDHIM_MAX_NAME_LEN);
    if (H5Rget_type(ref) == H5R_EXT_ATTR) {
        char attr_name[H5X_MDHIM_MAX_NAME_LEN];
        H5R__get_attr_name(NULL, ref, attr_name, H5X_MDHIM_MAX_NAME_LEN);
        H5X_MDHIM_LOG_DEBUG("Attribute reference: %s, %s", obj_name, attr_name);
    } else {
        H5X_MDHIM_LOG_DEBUG("Object reference: %s", obj_name);
    }
} /* print_ref */
#endif /* H5X_MDHIM_DEBUG */

static herr_t
H5X__mdhim_index(hid_t oid, const char *name, const H5O_info_t *oinfo, void *udata)
{
    H5X_mdhim_t *db = (H5X_mdhim_t *) udata;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(name);
    HDassert(oinfo);
    HDassert(db);

    /* Index link names */
    if (FAIL == H5X__mdhim_index_obj(db, oid, name, oinfo))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTAPPEND, FAIL, "can't add link name");

    /* Index attribute names/values */
    if (FAIL == H5X__mdhim_index_attrs(db, oid, name))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "can't apply data query to object");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__mdhim_index */

static herr_t
H5X__mdhim_index_obj(H5X_mdhim_t *db, hid_t loc_id, const char *name,
    const H5O_info_t *oinfo)
{
    H5X_mdhim_obj_t db_obj;
    href_t ref = NULL;
    const char *link_name = NULL;
    const char *trimmed_path = NULL;
    char file_name[H5X_MDHIM_MAX_NAME_LEN];
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
    if (H5Fget_name(loc_id, file_name, H5X_MDHIM_MAX_NAME_LEN) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get file name");

    /* Keep object reference */
    if (NULL == (ref = H5R_create_ext_object(file_name, name)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "can't create object reference");

    db_obj.name = link_name;
    if (FAIL == H5X__mdhim_insert_obj(db, ref, &db_obj))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTAPPEND, FAIL, "can't append object reference to view");

done:
    if (ref && FAIL == H5Rdestroy(ref))
        HDONE_ERROR(H5E_REFERENCE, H5E_CANTCLOSEOBJ, FAIL, "unable to destroy reference");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__mdhim_index_obj */

static herr_t
H5X__mdhim_insert_obj(H5X_mdhim_t *db, href_t ref, H5X_mdhim_obj_t *obj)
{
    size_t ref_buf_size;
    void *ref_buf = NULL;
    herr_t ret_value = SUCCEED; /* Return value */
    struct mdhim_brm_t *brm = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(db);
    HDassert(ref);

    if (H5Rencode(ref, NULL, &ref_buf_size) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTENCODE, FAIL, "can't encode reference");
    if (NULL == (ref_buf = H5MM_malloc(ref_buf_size)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTALLOC, FAIL, "can't allocate buffer for encoding reference");
    if (H5Rencode(ref, ref_buf, &ref_buf_size) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTENCODE, FAIL, "can't encode reference");

    brm = mdhimPut(db->obj_refs_md, ref_buf, (int)ref_buf_size,
        obj, (int)(HDstrlen(obj->name) + 1), NULL, NULL);
    if (!brm || brm->error)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTINSERT, FAIL, "Error inserting key/value into MDHIM");
    mdhim_full_release_msg(brm);

    brm = mdhimPutSecondary(db->obj_refs_md, db->link_names_idx,
        obj->name, (int)(HDstrlen(obj->name) + 1), ref_buf, (int)ref_buf_size);
    if (!brm || brm->error)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTINSERT, FAIL, "Error inserting key/value into MDHIM");

    H5X_MDHIM_LOG_DEBUG("Inserted obj: %s", obj->name);

done:
    mdhim_full_release_msg(brm);
    H5MM_free(ref_buf);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__mdhim_insert_obj */

static herr_t
H5X__mdhim_index_attrs(H5X_mdhim_t *db, hid_t loc_id, const char *name)
{
    H5X_mdhim_index_attr_arg_t attr_args;
    hid_t obj_id = FAIL;
    char file_name[H5X_MDHIM_MAX_NAME_LEN];
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(db);
    HDassert(name);

    /* Get file name */
    if (H5Fget_name(loc_id, file_name, H5X_MDHIM_MAX_NAME_LEN) < 0)
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
        NULL, H5X__mdhim_index_attr, &attr_args)))
        HGOTO_ERROR(H5E_ATTR, H5E_BADITER, FAIL, "error iterating over attributes");

done:
    if ((obj_id != FAIL) && (obj_id != loc_id) && (FAIL == H5Oclose(obj_id)))
        HDONE_ERROR(H5E_OHDR, H5E_CANTRELEASE, FAIL, "unable to close object")
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__mdhim_index_attrs */

static herr_t
H5X__mdhim_index_attr(hid_t loc_id, const char *attr_name,
    const H5A_info_t H5_ATTR_UNUSED *ainfo, void *udata)
{
    H5X_mdhim_index_attr_arg_t *args = (H5X_mdhim_index_attr_arg_t *) udata;
    void *buf = NULL;
    size_t buf_size;
    hid_t attr_id = FAIL;
    hid_t type_id = FAIL;
    hid_t space_id = FAIL;
    size_t nelmts, elmt_size;
    href_t ref = NULL;
    H5X_mdhim_attr_t db_attr;
    const char *link_name = NULL;
    const char *trimmed_path = NULL;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(attr_name);
    HDassert(udata);

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

    /* Keep attribute reference */
    if (NULL == (ref = H5R_create_ext_attr(args->filename, args->loc_name, attr_name)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "can't create attribute reference");

    trimmed_path = HDstrrchr(args->loc_name, '/');
    link_name = (trimmed_path) ? ++trimmed_path : args->loc_name;

    db_attr.name = attr_name;
    db_attr.link_name = link_name;
    db_attr.elmt_size = elmt_size;
    db_attr.elmts = buf;
    db_attr.nelmts = nelmts;
    //    if (FAIL == (native_type_id = H5Tget_native_type(datatype_id, H5T_DIR_DEFAULT)))
    //        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCOPY, FAIL, "can't copy attribute type");
    //    if (0 == (datatype_size = H5Tget_size(native_type_id)))
    //        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a valid size");

    if (FAIL == H5X__mdhim_insert_attr(args->db, ref, &db_attr))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTAPPEND, FAIL, "can't append object reference to view");

//    H5X_mdhim_index_attr_elem_arg_t iter_args;
    //    iter_args.attr_args = args;
    //    iter_args.attr_name = attr_name;
    /* Iterate over attribute elements to compare values */
//    if (FAIL == H5Diterate(buf, type_id, space_id, H5X__mdhim_index_attr_value_iterate, &iter_args))
//        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOMPARE, FAIL, "unable to compare attribute elements");

done:
    if (ref && FAIL == H5Rdestroy(ref))
        HDONE_ERROR(H5E_REFERENCE, H5E_CANTCLOSEOBJ, FAIL, "unable to destroy reference");
    H5MM_free(buf);
    if (attr_id != FAIL) H5Aclose(attr_id);
    if (type_id != FAIL) H5Tclose(type_id);
    if (space_id != FAIL) H5Sclose(space_id);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__mdhim_index_attr */

//static herr_t
//H5X__mdhim_index_attr_value_iterate(void *elem, hid_t type_id,
//    unsigned H5_ATTR_UNUSED ndim, const hsize_t H5_ATTR_UNUSED *point, void *udata)
//{
//    H5X_mdhim_index_attr_elem_arg_t *args = (H5X_mdhim_index_attr_elem_arg_t *) udata;
//    herr_t ret_value = SUCCEED; /* Return value */
//
//    FUNC_ENTER_NOAPI_NOINIT
//
//    HDassert(elem);
//    HDassert(args);
//
//    /* Keep attribute reference */
//
//
//done:
//
//    FUNC_LEAVE_NOAPI(ret_value)
//} /* end H5X__mdhim_index_attr_value_iterate */

static herr_t
H5X__mdhim_insert_attr(H5X_mdhim_t *db, href_t ref, H5X_mdhim_attr_t *attr)
{
    size_t ref_buf_size;
    void *ref_buf = NULL;
    herr_t ret_value = SUCCEED; /* Return value */
    struct mdhim_brm_t *brm = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(db);
    HDassert(ref);

    if (H5Rencode(ref, NULL, &ref_buf_size) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTENCODE, FAIL, "can't encode reference");
    if (NULL == (ref_buf = H5MM_malloc(ref_buf_size)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTALLOC, FAIL, "can't allocate buffer for encoding reference");
    if (H5Rencode(ref, ref_buf, &ref_buf_size) < 0)
        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTENCODE, FAIL, "can't encode reference");

    brm = mdhimPut(db->attr_refs_md, ref_buf, (int)ref_buf_size,
        attr, (int)(HDstrlen(attr->name) + 1
        + HDstrlen(attr->link_name) + 1 + attr->nelmts * attr->elmt_size
        + sizeof(attr->elmt_size) + sizeof(attr->nelmts)), NULL, NULL);
    if (!brm || brm->error)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTINSERT, FAIL, "Error inserting key/value into MDHIM");
    mdhim_full_release_msg(brm);

    brm = mdhimPutSecondary(db->attr_refs_md, db->attr_names_idx,
        attr->name, (int)(HDstrlen(attr->name) + 1), ref_buf, (int)ref_buf_size);
    if (!brm || brm->error)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTINSERT, FAIL, "Error inserting key/value into MDHIM");
    mdhim_full_release_msg(brm);

    brm = mdhimPutSecondary(db->attr_refs_md, db->attr_values_idx,
        attr->elmts, (int)(attr->elmt_size), ref_buf, (int)ref_buf_size);
    if (!brm || brm->error)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTINSERT, FAIL, "Error inserting key/value into MDHIM");

    H5X_MDHIM_LOG_DEBUG("Inserted attr: %s", attr->name);

done:
    mdhim_full_release_msg(brm);
    H5MM_free(ref_buf);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__mdhim_insert_attr */

//static herr_t
//H5X__mdhim_put(H5X_mdhim_t *db, href_t ref, H5Q_type_t type, ...)
//{
//    va_list ap;
//    DBT key;  /* The key to dbcp->put() */
//    DBT data; /* The data to dbcp->put() */
//    size_t buf_size;
//    void *buf = NULL;
//    herr_t ret_value = SUCCEED; /* Return value */
//    int db_ret;
//
//    FUNC_ENTER_NOAPI_NOINIT
//
//    HDassert(db);
//    HDassert(ref);
//
//    va_start(ap, type);
//
//    /* Initialize key/data */
//    HDmemset(&key, 0, sizeof(DBT));
//    HDmemset(&data, 0, sizeof(DBT));
//
//    if (H5Rencode(ref, NULL, &buf_size) < 0)
//        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTENCODE, FAIL, "can't encode reference");
//    if (NULL == (buf = H5MM_malloc(buf_size)))
//        HGOTO_ERROR(H5E_INDEX, H5E_CANTALLOC, FAIL, "can't allocate buffer for encoding reference");
//    if (H5Rencode(ref, buf, &buf_size) < 0)
//        HGOTO_ERROR(H5E_REFERENCE, H5E_CANTENCODE, FAIL, "can't encode reference");
//
//    switch (type) {
//        case H5Q_TYPE_ATTR_VALUE:
//        {
//            hid_t datatype_id;
//            hid_t native_type_id;
//            size_t datatype_size;
//            const void *value;
//            void *encoded_value = NULL;
//
//            /* Get arguments */
//            datatype_id = va_arg(ap, hid_t);
//            value = va_arg(ap, const void *);
//
//            HDassert(datatype_id != FAIL);
//            HDassert(value);
//
//            if (FAIL == (native_type_id = H5Tget_native_type(datatype_id, H5T_DIR_DEFAULT)))
//                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCOPY, FAIL, "can't copy attribute type");
//            if (0 == (datatype_size = H5Tget_size(native_type_id)))
//                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a valid size");
//            if (NULL == (encoded_value = H5MM_malloc(datatype_size)))
//                HGOTO_ERROR(H5E_QUERY, H5E_CANTALLOC, FAIL, "can't allocate value buffer");
//            HDmemcpy(encoded_value, value, datatype_size);
//
//            key.data = encoded_value;
//            key.size = (u_int32_t)datatype_size;
//            data.data = buf;
//            data.size = (u_int32_t)buf_size;
//
//            if ((db_ret = db->attr_values_db->put(db->attr_values_db, NULL, &key, &data, 0)) != 0) {
//                db->attr_values_db->err(db->attr_values_db, db_ret, "DB->put");
//            }
//            H5MM_free(encoded_value);
//        }
//        break;
//        case H5Q_TYPE_ATTR_NAME:
//        {
//            char *attr_name;
//
//            /* Get arguments */
//            attr_name = va_arg(ap, char *);
//
//            HDassert(attr_name);
//
//            key.data = attr_name;
//            key.size = (u_int32_t)(HDstrlen(attr_name) + 1);
//            data.data = buf;
//            data.size = (u_int32_t)buf_size;
//
//            if ((db_ret = db->attr_names_db->put(db->attr_names_db, NULL, &key, &data, 0)) != 0) {
//                db->attr_names_db->err(db->attr_names_db, db_ret, "DB->put");
//            }
//        }
//        break;
//        case H5Q_TYPE_LINK_NAME:
//        {
//            char *link_name;
//
//            /* Get arguments */
//            link_name = va_arg(ap, char *);
//
//            HDassert(link_name);
//
//            key.data = link_name;
//            key.size = (u_int32_t)(HDstrlen(link_name) + 1);
//            data.data = buf;
//            data.size = (u_int32_t)buf_size;
//
//            if ((db_ret = db->link_names_db->put(db->link_names_db, NULL, &key, &data, 0)) != 0) {
//                db->link_names_db->err(db->link_names_db, db_ret, "DB->put");
//            }
//        }
//        break;
//        case H5Q_TYPE_MISC:
//        default:
//            HGOTO_ERROR(H5E_QUERY, H5E_BADTYPE, FAIL, "unsupported/unrecognized query type");
//            break;
//    }
//
//done:
//    va_end(ap);
//
//    H5MM_free(buf);
//    FUNC_LEAVE_NOAPI(ret_value)
//} /* end H5X__mdhim_put */

#ifndef H5X_MDHIM_USE_JOIN
static herr_t
H5X__mdhim_push_entry(H5X_mdhim_head_t *head, H5X_mdhim_entry_t *entry)
{
    H5X_mdhim_entry_t *new_entry;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(head);
    HDassert(entry);

    if (NULL == (new_entry = (H5X_mdhim_entry_t *) H5MM_malloc(sizeof(H5X_mdhim_entry_t))))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTALLOC, FAIL, "can't allocate ref entry");
    new_entry->ref = H5Rcopy(entry->ref);
    H5Q_QUEUE_INSERT_TAIL(head, new_entry, entry);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__mdhim_push_entry */
#endif

static herr_t
H5X__mdhim_remove_entries(H5X_mdhim_head_t *head)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(head);

    while (!H5Q_QUEUE_EMPTY(head)) {
        H5X_mdhim_entry_t *entry = H5Q_QUEUE_FIRST(head);
        H5Q_QUEUE_REMOVE_HEAD(head, entry);
        if (entry->ref && FAIL == H5Rdestroy(entry->ref))
            HGOTO_ERROR(H5E_REFERENCE, H5E_CANTCLOSEOBJ, FAIL, "unable to destroy reference");
        H5MM_free(entry);
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__mdhim_metadata_remove_entries */

static herr_t
H5X__mdhim_gen_file_names(H5X_mdhim_t *db, hid_t loc_id)
{
    char filename[H5X_MDHIM_MAX_NAME_LEN];
    ssize_t filename_len;
    char *filename_prefix = NULL;
    char **db_filename_ptrs[H5X_MDHIM_NPRIMARIES] = { &db->obj_refs_path,
        &db->attr_refs_path };
    const char *db_filename_suffixes[H5X_MDHIM_NPRIMARIES] = { H5X_MDHIM_OBJ_REFS_SUFFIX,
        H5X_MDHIM_ATTR_REFS_SUFFIX };
    char **sdb_filename_ptrs[H5X_MDHIM_NSECONDARIES] = { &db->link_names_idx_name,
        &db->attr_names_idx_name, &db->attr_values_idx_name };
    const char *sdb_filename_suffixes[H5X_MDHIM_NSECONDARIES] = { H5X_MDHIM_LINK_NAMES_SUFFIX,
        H5X_MDHIM_ATTR_NAMES_SUFFIX, H5X_MDHIM_ATTR_VALUES_SUFFIX };
    const char *manifest_suffix = H5X_MDHIM_MANIFEST_SUFFIX;
    herr_t ret_value = SUCCEED;
    int i;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(db);

    /* Get filename prefix */
    if ((filename_len = H5Fget_name(loc_id, filename, H5X_MDHIM_MAX_NAME_LEN)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get file name");

    filename_prefix = HDstrtok(filename, ".");
    if (!filename_prefix)
        HGOTO_ERROR(H5E_INDEX, H5E_BADVALUE, FAIL, "invalid file name (no '.' delim)");

    if (NULL == (db->manifest_path = (char *) H5MM_malloc(
        HDstrlen(filename_prefix) + HDstrlen(manifest_suffix) + 1)))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate db struct");
    HDstrcpy(db->manifest_path, filename_prefix);
    HDstrcat(db->manifest_path, manifest_suffix);

    /* Create file names for primaries */
    for (i = 0; i < H5X_MDHIM_NPRIMARIES; i++) {
        char *db_filename;

        if (NULL == (db_filename = (char *) H5MM_malloc(
            HDstrlen(filename_prefix) + HDstrlen(db_filename_suffixes[i]) + 1)))
            HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate db struct");
        HDstrcpy(db_filename, filename_prefix);
        HDstrcat(db_filename, db_filename_suffixes[i]);
        *db_filename_ptrs[i] = db_filename;
    }

    /* Create file names for secondaries */
    for (i = 0; i < H5X_MDHIM_NSECONDARIES; i++) {
        char *sdb_filename;

        if (NULL == (sdb_filename = (char *) H5MM_malloc(
            HDstrlen(filename_prefix) + HDstrlen(sdb_filename_suffixes[i]) + 1)))
            HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate db struct");
        HDstrcpy(sdb_filename, filename_prefix);
        HDstrcat(sdb_filename, sdb_filename_suffixes[i]);
        *sdb_filename_ptrs[i] = sdb_filename;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__mdhim_gen_file_names() */

static herr_t
H5X__mdhim_free_file_names(H5X_mdhim_t *db)
{
    char **db_filename_ptrs[H5X_MDHIM_NPRIMARIES] = { &db->obj_refs_path,
        &db->attr_refs_path };
    char **sdb_filename_ptrs[H5X_MDHIM_NSECONDARIES] = { &db->link_names_idx_name,
        &db->attr_names_idx_name, &db->attr_values_idx_name };

    herr_t ret_value = SUCCEED;
    int i;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(db);

    for (i = 0; i < H5X_MDHIM_NPRIMARIES; i++)
        *db_filename_ptrs[i] = H5MM_xfree(*db_filename_ptrs[i]);
    for (i = 0; i < H5X_MDHIM_NSECONDARIES; i++)
        *sdb_filename_ptrs[i] = H5MM_xfree(*sdb_filename_ptrs[i]);
    db->manifest_path = H5MM_xfree(db->manifest_path);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__mdhim_free_file_names() */

static herr_t
H5X__mdhim_create_tables(H5X_mdhim_t *db, hbool_t open, unsigned flags)
{
    herr_t ret_value = SUCCEED; /* Return value */
    mdhim_options_t *mdhim_opts = NULL;
    int mpi_comm = MPI_COMM_WORLD;
    struct stat manifest_stat;

    FUNC_ENTER_NOAPI_NOINIT

    mdhim_opts = mdhim_options_init();
    mdhim_options_set_db_path(mdhim_opts, H5X_MDHIM_DB_PATH);
    mdhim_options_set_db_type(mdhim_opts, H5X_MDHIM_DB_TYPE);
    mdhim_options_set_key_type(mdhim_opts, MDHIM_BYTE_KEY);
    mdhim_options_set_value_append(mdhim_opts, MDHIM_DB_APPEND);
    set_manifest_path(mdhim_opts, db->manifest_path);

    /* Create manifest directory if does not exist */
    if (HDstat(db->manifest_path, &manifest_stat) < 0) {
        mkdir(db->manifest_path, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);
    }

    /* Create primary database for objects */
    mdhim_options_set_db_name(mdhim_opts, db->obj_refs_path);
    if (NULL == (db->obj_refs_md = mdhimInit(&mpi_comm, mdhim_opts)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL,
            "can't create database: %s", db->obj_refs_path);

    db->link_names_idx = create_local_index(db->obj_refs_md, H5X_MDHIM_DB_TYPE,
        MDHIM_BYTE_KEY, db->link_names_idx_name);

    /* Create primary database for attributes */
    mdhim_options_set_db_name(mdhim_opts, db->attr_refs_path);
    if (NULL == (db->attr_refs_md = mdhimInit(&mpi_comm, mdhim_opts)))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL,
            "can't create database: %s", db->attr_refs_path);

    db->attr_names_idx = create_local_index(db->attr_refs_md, H5X_MDHIM_DB_TYPE,
        MDHIM_BYTE_KEY, db->attr_names_idx_name);
    db->attr_values_idx = create_local_index(db->attr_refs_md, H5X_MDHIM_DB_TYPE,
        MDHIM_BYTE_KEY, db->attr_values_idx_name);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__mdhim_create_tables */

static herr_t
H5X__mdhim_close_tables(H5X_mdhim_t *db)
{
    herr_t ret_value = SUCCEED; /* Return value */
    struct mdhim_t **db_ptrs[H5X_MDHIM_NPRIMARIES] = { &db->obj_refs_md,
        &db->attr_refs_md };
//    struct index_t **sdb_ptrs[H5X_MDHIM_NSECONDARIES] = { &db->link_names_idx,
//        &db->attr_names_idx, &db->attr_values_idx };
    int db_ret;
    int i;

    FUNC_ENTER_NOAPI_NOINIT

    /* Close secondaries */
//    for (i = 0; i < H5X_MDHIM_NSECONDARIES; i++) {
//        DB *sdbp = *sdb_ptrs[i];
//
//        if ((db_ret = sdbp->close(sdbp, 0)) != 0)
//            HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "DB->close: %s",
//                db_strerror(db_ret));
//        *sdb_ptrs[i] = NULL;
//    }

    /* Close primaries */
    for (i = 0; i < H5X_MDHIM_NPRIMARIES; i++) {
        struct mdhim_t *dbp = *db_ptrs[i];

        if ((db_ret = mdhimClose(dbp)) != 0)
            HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "mdhimClose() failed");
        *db_ptrs[i] = NULL;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__mdhim_close_tables */

static int
H5X__mdhim_remove_directory(const char *path)
{
    DIR *d = HDopendir(path);
    size_t path_len = HDstrlen(path);
    int r = -1;

    if (d) {
        struct dirent *p;

        r = 0;
        while (!r && (p = HDreaddir(d))) {
            int r2 = -1;
            char *buf;
            size_t len;

            /* Skip the names "." and ".." as we don't want to recurse on them. */
            if (!HDstrcmp(p->d_name, ".") || !HDstrcmp(p->d_name, ".."))
                continue;

            len = path_len + HDstrlen(p->d_name) + 2;
            buf = HDmalloc(len);

            if (buf) {
                struct stat statbuf;

                HDsnprintf(buf, len, "%s/%s", path, p->d_name);

                if (!HDstat(buf, &statbuf)) {
                    if (S_ISDIR(statbuf.st_mode))
                        r2 = H5X__mdhim_remove_directory(buf);
                    else
                        r2 = HDremove(buf);
                }
                HDfree(buf);
            }
            r = r2;
        }
        HDclosedir(d);
    }

    if (!r)
        r = HDremove(path);

    return r;
}

static herr_t
H5X__mdhim_remove_tables(H5X_mdhim_t *db)
{
    herr_t ret_value = SUCCEED; /* Return value */
    struct mdhim_t *db_ptrs[H5X_MDHIM_NPRIMARIES] = { db->obj_refs_md,
        db->attr_refs_md };
    struct index_t *sdb_ptrs[H5X_MDHIM_NSECONDARIES] = { db->link_names_idx,
        db->attr_names_idx, db->attr_values_idx };
    const char *db_filenames[H5X_MDHIM_NPRIMARIES] = { db->obj_refs_path,
        db->attr_refs_path };
    char dirname[H5X_MDHIM_MAX_NAME_LEN];
    char stat_dirname[H5X_MDHIM_MAX_NAME_LEN];

    FUNC_ENTER_NOAPI_NOINIT

    /* Remove files */
    HDsnprintf(dirname, H5X_MDHIM_MAX_NAME_LEN, "%s-%d-%d", db_filenames[0],
        db_ptrs[0]->primary_index->id, db_ptrs[0]->mdhim_rank);
    HDsnprintf(stat_dirname, H5X_MDHIM_MAX_NAME_LEN, "%s_stats", dirname);
    H5X_MDHIM_LOG_DEBUG("Removing dir: %s", dirname);
    H5X_MDHIM_LOG_DEBUG("Removing dir: %s", stat_dirname);
    if (H5X__mdhim_remove_directory(dirname) < 0)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTREMOVE, FAIL, "can't remove %s", dirname);
    if (H5X__mdhim_remove_directory(stat_dirname) < 0)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTREMOVE, FAIL, "can't remove %s", stat_dirname);

    HDsnprintf(dirname, H5X_MDHIM_MAX_NAME_LEN, "%s-%d-%d", db_filenames[0],
        sdb_ptrs[0]->id, db_ptrs[0]->mdhim_rank);
    HDsnprintf(stat_dirname, H5X_MDHIM_MAX_NAME_LEN, "%s_stats", dirname);
    H5X_MDHIM_LOG_DEBUG("Removing dir: %s", dirname);
    H5X_MDHIM_LOG_DEBUG("Removing dir: %s", stat_dirname);
    if (H5X__mdhim_remove_directory(dirname) < 0)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTREMOVE, FAIL, "can't remove %s", dirname);
    if (H5X__mdhim_remove_directory(stat_dirname) < 0)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTREMOVE, FAIL, "can't remove %s", stat_dirname);

    HDsnprintf(dirname, H5X_MDHIM_MAX_NAME_LEN, "%s-%d-%d", db_filenames[1],
        db_ptrs[1]->primary_index->id, db_ptrs[1]->mdhim_rank);
    HDsnprintf(stat_dirname, H5X_MDHIM_MAX_NAME_LEN, "%s_stats", dirname);
    H5X_MDHIM_LOG_DEBUG("Removing dir: %s", dirname);
    H5X_MDHIM_LOG_DEBUG("Removing dir: %s", stat_dirname);
    if (H5X__mdhim_remove_directory(dirname) < 0)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTREMOVE, FAIL, "can't remove %s", dirname);
    if (H5X__mdhim_remove_directory(stat_dirname) < 0)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTREMOVE, FAIL, "can't remove %s", stat_dirname);

    HDsnprintf(dirname, H5X_MDHIM_MAX_NAME_LEN, "%s-%d-%d", db_filenames[1],
        sdb_ptrs[1]->id, db_ptrs[1]->mdhim_rank);
    HDsnprintf(stat_dirname, H5X_MDHIM_MAX_NAME_LEN, "%s_stats", dirname);
    H5X_MDHIM_LOG_DEBUG("Removing dir: %s", dirname);
    H5X_MDHIM_LOG_DEBUG("Removing dir: %s", stat_dirname);
    if (H5X__mdhim_remove_directory(dirname) < 0)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTREMOVE, FAIL, "can't remove %s", dirname);
    if (H5X__mdhim_remove_directory(stat_dirname) < 0)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTREMOVE, FAIL, "can't remove %s", stat_dirname);

    HDsnprintf(dirname, H5X_MDHIM_MAX_NAME_LEN, "%s-%d-%d", db_filenames[1],
        sdb_ptrs[2]->id, db_ptrs[1]->mdhim_rank);
    HDsnprintf(stat_dirname, H5X_MDHIM_MAX_NAME_LEN, "%s_stats", dirname);
    H5X_MDHIM_LOG_DEBUG("Removing dir: %s", dirname);
    H5X_MDHIM_LOG_DEBUG("Removing dir: %s", stat_dirname);
    if (H5X__mdhim_remove_directory(dirname) < 0)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTREMOVE, FAIL, "can't remove %s", dirname);
    if (H5X__mdhim_remove_directory(stat_dirname) < 0)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTREMOVE, FAIL, "can't remove %s", stat_dirname);

    /* Remove manifest dir */
    H5X_MDHIM_LOG_DEBUG("Removing dir: %s", db->manifest_path);
    if (H5X__mdhim_remove_directory(db->manifest_path) < 0)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTREMOVE, FAIL, "can't remove %s", db->manifest_path);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__mdhim_remove_tables */

static void
H5X__mdhim_stat(H5X_mdhim_t *db)
{
    if (mdhimCommit(db->obj_refs_md, db->obj_refs_md->primary_index) != MDHIM_SUCCESS) {
        printf("Error committing MDHIM database\n");
    } else {
        printf("Committed MDHIM database\n");
    }
    if (mdhimCommit(db->attr_refs_md, db->attr_refs_md->primary_index) != MDHIM_SUCCESS) {
        printf("Error committing MDHIM database\n");
    } else {
        printf("Committed MDHIM database\n");
    }
    mdhimStatFlush(db->obj_refs_md, db->link_names_idx);
    mdhimStatFlush(db->attr_refs_md, db->attr_names_idx);
    mdhimStatFlush(db->attr_refs_md, db->attr_values_idx);
}

static herr_t
H5X__mdhim_metadata_write(H5X_mdhim_t *db, size_t *plugin_metadata_size,
    void **plugin_metadata)
{
    const char *db_filenames[H5X_MDHIM_NPRIMARIES] = { db->obj_refs_path,
        db->attr_refs_path };
    const char *sdb_filenames[H5X_MDHIM_NSECONDARIES] = { db->link_names_idx_name,
        db->attr_names_idx_name, db->attr_values_idx_name };
    herr_t ret_value = SUCCEED; /* Return value */
    size_t buf_size = 0;
    void *buf;
    char *buf_ptr;
    int i;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(db);
    HDassert(plugin_metadata_size);
    HDassert(plugin_metadata);

    /* Add directory / index / manifest names */
    for (i = 0; i < H5X_MDHIM_NPRIMARIES; i++)
        buf_size += HDstrlen(db_filenames[i]) + 1;
    for (i = 0; i < H5X_MDHIM_NSECONDARIES; i++)
        buf_size += HDstrlen(sdb_filenames[i]) + 1;
    buf_size += HDstrlen(db->manifest_path) + 1;

    if (NULL == (buf = H5MM_malloc(buf_size)))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate plugin metadata");
    buf_ptr = buf;

    for (i = 0; i < H5X_MDHIM_NPRIMARIES; i++) {
        HDstrcpy(buf_ptr, db_filenames[i]);
        buf_ptr += HDstrlen(db_filenames[i]) + 1;
    }
    for (i = 0; i < H5X_MDHIM_NSECONDARIES; i++) {
        HDstrcpy(buf_ptr, sdb_filenames[i]);
        buf_ptr += HDstrlen(sdb_filenames[i]) + 1;
    }
    HDstrcpy(buf_ptr, db->manifest_path);
    buf_ptr += HDstrlen(db->manifest_path) + 1;

    *plugin_metadata = buf;
    *plugin_metadata_size = buf_size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__mdhim_metadata_write */

static herr_t
H5X__mdhim_metadata_read(size_t H5_ATTR_UNUSED plugin_metadata_size,
    void *plugin_metadata, H5X_mdhim_t *db)
{
    char **db_filename_ptrs[H5X_MDHIM_NPRIMARIES] = { &db->obj_refs_path,
        &db->attr_refs_path };
    char **sdb_filename_ptrs[H5X_MDHIM_NSECONDARIES] = { &db->link_names_idx_name,
        &db->attr_names_idx_name, &db->attr_values_idx_name };
    herr_t ret_value = SUCCEED; /* Return value */
    char *buf_ptr;
    int i;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(db);
    HDassert(plugin_metadata_size);
    HDassert(plugin_metadata);

    buf_ptr = plugin_metadata;

    /* Get file names for primaries */
    for (i = 0; i < H5X_MDHIM_NPRIMARIES; i++) {
        char *db_filename;

        if (NULL == (db_filename = (char *) H5MM_malloc(HDstrlen(buf_ptr) + 1)))
            HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate db struct");
        HDstrcpy(db_filename, buf_ptr);
        buf_ptr += HDstrlen(db_filename) + 1;
        *db_filename_ptrs[i] = db_filename;
    }

    /* Get file names for secondaries */
    for (i = 0; i < H5X_MDHIM_NSECONDARIES; i++) {
        char *sdb_filename;

        if (NULL == (sdb_filename = (char *) H5MM_malloc(HDstrlen(buf_ptr) + 1)))
            HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate db struct");
        HDstrcpy(sdb_filename, buf_ptr);
        buf_ptr += HDstrlen(sdb_filename) + 1;
        *sdb_filename_ptrs[i] = sdb_filename;
    }
    if (NULL == (db->manifest_path = (char *) H5MM_malloc(HDstrlen(buf_ptr) + 1)))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate db struct");
    HDstrcpy(db->manifest_path, buf_ptr);
    buf_ptr += HDstrlen(db->manifest_path) + 1;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__mdhim_metadata_read */

/*-------------------------------------------------------------------------
 * Function:    H5X_mdhim_create
 *
 * Purpose: This function creates a new instance of a db plugin index.
 *
 * Return:  Success:    Pointer to the new index
 *          Failure:    NULL
 *
 *------------------------------------------------------------------------
 */
static void *
H5X__mdhim_create(hid_t loc_id, hid_t H5_ATTR_UNUSED xcpl_id,
    hid_t H5_ATTR_UNUSED xapl_id, size_t *metadata_size, void **metadata)
{
    H5X_mdhim_t *db = NULL;
#ifdef H5X_MDHIM_DEBUG
    struct timeval t1, t2;
#endif
    void *ret_value = NULL; /* Return value */
    int flag;

    FUNC_ENTER_NOAPI_NOINIT

    H5X_MDHIM_LOG_DEBUG("Calling H5X_mdhim_create");

    /* Check for MPI support */
    if (MPI_Initialized(&flag) != MPI_SUCCESS)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, NULL, "MPI_Initialized() failed");
    if (!flag)
        HGOTO_ERROR(H5E_INDEX, H5E_BADVALUE, NULL, "MPI must be initialized");

    /* Create new db instance */
    if (NULL == (db = (H5X_mdhim_t *) H5MM_malloc(sizeof(H5X_mdhim_t))))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, NULL, "can't allocate db struct");
    db->link_names_idx = NULL;
    db->link_names_idx_name = NULL;
    db->attr_names_idx = NULL;
    db->attr_names_idx_name = NULL;
    db->attr_values_idx = NULL;
    db->attr_values_idx_name = NULL;

    /* Generate file names */
    if (FAIL == H5X__mdhim_gen_file_names(db, loc_id))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, NULL, "can't create file names");

    /* Create DBs */
    if (FAIL == H5X__mdhim_create_tables(db, TRUE, 0))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, NULL, "can't create tables");

    /* Visit file */
#ifdef H5X_MDHIM_DEBUG
    HDgettimeofday(&t1, NULL);
#endif
    if (FAIL == H5Ovisit(loc_id, H5_INDEX_NAME, H5_ITER_NATIVE, H5X__mdhim_index, db))
        HGOTO_ERROR(H5E_SYM, H5E_BADITER, NULL, "object visitation failed");
    H5X__mdhim_stat(db);
#ifdef H5X_MDHIM_DEBUG
    HDgettimeofday(&t2, NULL);
#endif

    H5X_MDHIM_LOG_DEBUG("######################################################");
    H5X_MDHIM_LOG_DEBUG("Index build time: %lf ms",
        ((float)(t2.tv_sec - t1.tv_sec)) * 1000.0f
        + ((float)(t2.tv_usec - t1.tv_usec)) / (1000.0f));
    H5X_MDHIM_LOG_DEBUG("######################################################");

    /* Write index metadata */
    if (FAIL == H5X__mdhim_metadata_write(db, metadata_size, metadata))
        HGOTO_ERROR(H5E_INDEX, H5E_WRITEERROR, NULL, "can't write metadata");

    ret_value = db;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_mdhim_create() */

/*-------------------------------------------------------------------------
 * Function:    H5X_mdhim_remove
 *
 * Purpose: This function removes the db plugin index from the file.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__mdhim_remove(hid_t H5_ATTR_UNUSED file_id, size_t metadata_size,
    void *metadata)
{
    H5X_mdhim_t *db = NULL;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    H5X_MDHIM_LOG_DEBUG("Calling H5X_mdhim_remove");

    if (!metadata_size)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL metadata size");
    if (!metadata)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL metadata");

    if (NULL == (db = (H5X_mdhim_t *) H5MM_malloc(sizeof(H5X_mdhim_t))))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, FAIL, "can't allocate db struct");

    if (FAIL == H5X__mdhim_metadata_read(metadata_size, metadata, db))
        HGOTO_ERROR(H5E_INDEX, H5E_READERROR, FAIL, "can't read metadata");

    if (FAIL == H5X__mdhim_create_tables(db, FALSE, 0))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCREATE, FAIL, "can't create tables");

    if (FAIL == H5X__mdhim_remove_tables(db))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTREMOVE, FAIL, "can't remove tables");

    if (FAIL == H5X__mdhim_close_tables(db))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close tables");

    if (FAIL == H5X__mdhim_free_file_names(db))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTFREE, FAIL, "can't free file names");
    H5MM_free(db);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_mdhim_remove() */

/*-------------------------------------------------------------------------
 * Function:    H5X_mdhim_open
 *
 * Purpose: This function opens an already existing db index from a file.
 *
 * Return:  Success:    Pointer to the index
 *          Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5X__mdhim_open(hid_t H5_ATTR_UNUSED loc_id, hid_t H5_ATTR_UNUSED xapl_id,
    size_t metadata_size, void *metadata)
{
    H5X_mdhim_t *db = NULL;
    void *ret_value = NULL; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    H5X_MDHIM_LOG_DEBUG("Calling H5X_mdhim_open");

    if (!metadata_size)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "NULL metadata size");
    if (!metadata)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "NULL metadata");

    if (NULL == (db = (H5X_mdhim_t *) H5MM_malloc(sizeof(H5X_mdhim_t))))
        HGOTO_ERROR(H5E_INDEX, H5E_NOSPACE, NULL, "can't allocate db struct");

    if (FAIL == H5X__mdhim_metadata_read(metadata_size, metadata, db))
        HGOTO_ERROR(H5E_INDEX, H5E_READERROR, NULL, "can't read metadata");

    ret_value = db;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_mdhim_open() */

/*-------------------------------------------------------------------------
 * Function:    H5X_mdhim_close
 *
 * Purpose: This function closes a db index.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__mdhim_close(void *idx_handle)
{
    H5X_mdhim_t *db = (H5X_mdhim_t *) idx_handle;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    H5X_MDHIM_LOG_DEBUG("Calling H5X_mdhim_close");

    if (NULL == db)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");

    if (FAIL == H5X__mdhim_close_tables(db))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCLOSEOBJ, FAIL, "can't close tables");

    if (FAIL == H5X__mdhim_free_file_names(db))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTFREE, FAIL, "can't free file names");
    H5MM_free(db);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_mdhim_close() */

/*-------------------------------------------------------------------------
 * Function:    H5X__mdhim_insert_entry
 *
 * Purpose: This function insert a new entry in the db plugin index.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__mdhim_insert_entry(void *idx_handle, hid_t obj_id, H5Q_type_t key_type,
    H5Q_elem_t *key, hid_t H5_ATTR_UNUSED xxpl_id)
{
    H5X_mdhim_t *db = (H5X_mdhim_t *) idx_handle;
    char file_name[H5X_MDHIM_MAX_NAME_LEN];
    char loc_name[H5X_MDHIM_MAX_NAME_LEN];
    href_t ref = NULL;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    H5X_MDHIM_LOG_DEBUG("Calling H5X__mdhim_insert_entry");

    if (NULL == db)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");

    if (H5Fget_name(obj_id, file_name, H5X_MDHIM_MAX_NAME_LEN) < 0)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get file name");
    if (H5Iget_name(obj_id, loc_name, H5X_MDHIM_MAX_NAME_LEN) < 0)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get location name");

    switch (key_type) {
        case H5Q_TYPE_ATTR_VALUE:
        {
            char attr_name[H5X_MDHIM_MAX_NAME_LEN];

            if (H5Aget_name(obj_id, H5X_MDHIM_MAX_NAME_LEN, attr_name) < 0)
                HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "can't get attribute name");

            /* Keep attribute reference */
            if (NULL == (ref = H5R_create_ext_attr(file_name, loc_name, attr_name)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "can't create attribute reference");
//            if (FAIL == H5X__mdhim_put(db, ref, H5Q_TYPE_ATTR_VALUE, key->value.type, key->value.value))
//                HGOTO_ERROR(H5E_QUERY, H5E_CANTAPPEND, FAIL, "can't append attribute reference to view");
        }
            break;
        case H5Q_TYPE_ATTR_NAME:
        {
            /* Keep attribute reference */
            if (NULL == (ref = H5R_create_ext_attr(file_name, loc_name, key->name)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "can't create attribute reference");
//            if (FAIL == H5X__mdhim_put(db, ref, H5Q_TYPE_ATTR_NAME, key->name))
//                HGOTO_ERROR(H5E_QUERY, H5E_CANTAPPEND, FAIL, "can't append attribute reference to view");
        }
            break;
        case H5Q_TYPE_LINK_NAME:
        {
            /* Keep object reference */
            if (NULL == (ref = H5R_create_ext_object(file_name, loc_name)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "can't create object reference");
//            if (FAIL == H5X__mdhim_put(db, ref, H5Q_TYPE_LINK_NAME, key->name))
//                HGOTO_ERROR(H5E_QUERY, H5E_CANTAPPEND, FAIL, "can't append object reference to view");
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
} /* end H5X__mdhim_insert_entry() */

/*-------------------------------------------------------------------------
 * Function:    H5X__mdhim_remove_entry
 *
 * Purpose: This function removes an entry from the db plugin index.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__mdhim_remove_entry(void H5_ATTR_UNUSED *idx_handle, hid_t H5_ATTR_UNUSED obj_id,
    H5Q_type_t H5_ATTR_UNUSED key_type, H5Q_elem_t H5_ATTR_UNUSED *key,
    hid_t H5_ATTR_UNUSED xxpl_id)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    H5X_MDHIM_LOG_DEBUG("Calling H5X__mdhim_remove_entry");

    /* TODO Does not do anything */
    /*/!\ Calling db->del removes all duplicates that match the same key */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__mdhim_remove_entry() */

/*-------------------------------------------------------------------------
 * Function:    H5X_mdhim_query
 *
 * Purpose: This function queries a db index.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5X__mdhim_query(void *idx_handle, hid_t query_id, hid_t H5_ATTR_UNUSED xxpl_id,
    size_t *ref_count, href_t *refs[])
{
    H5X_mdhim_t *db = (H5X_mdhim_t *) idx_handle;
    H5X_mdhim_head_t query_result = H5Q_QUEUE_HEAD_INITIALIZER(query_result);
    href_t *ref_buf = NULL;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    H5X_MDHIM_LOG_DEBUG("Calling H5X_mdhim_query");

    if (NULL == db)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL index handle");

    /* We assume here that the queries passed only operate on metadata */
    if (FAIL == H5X__mdhim_query_components(db, query_id, &query_result))
        HGOTO_ERROR(H5E_INDEX, H5E_CANTCOMPARE, FAIL, "can't query metadata");

    H5X_MDHIM_LOG_DEBUG("###########################");
#ifdef H5X_MDHIM_DEBUG
    if (!H5Q_QUEUE_EMPTY(&query_result)) {
        H5X_mdhim_entry_t *entry;
        H5X_MDHIM_LOG_DEBUG("Query returns %zu references", query_result.n_elem);
        H5Q_QUEUE_FOREACH(entry, &query_result, entry)
            print_ref(entry->ref);
    }
#endif
    H5X_MDHIM_LOG_DEBUG("###########################");

    /* Fill result */
    if (!H5Q_QUEUE_EMPTY(&query_result)) {
        H5X_mdhim_entry_t *entry;
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
    if (FAIL == H5X__mdhim_remove_entries(&query_result))
        HDONE_ERROR(H5E_QUERY, H5E_CANTFREE, FAIL, "unable to remove entries");
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X_mdhim_query() */

static herr_t
H5X__mdhim_query_components(H5X_mdhim_t *db, hid_t query_id, H5X_mdhim_head_t *result)
{
    H5Q_type_t query_type;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if (FAIL == H5Qget_type(query_id, &query_type))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTGET, FAIL, "unable to get query type");

    if (query_type != H5Q_TYPE_MISC) {
        if (FAIL == H5X__mdhim_query_singleton(db, query_id, result))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "unable to compare query");
    } else {
        H5Q_combine_op_t combine_op;
        hid_t sub_query1_id, sub_query2_id;
        H5X_mdhim_head_t result1 = H5Q_QUEUE_HEAD_INITIALIZER(result1);
        H5X_mdhim_head_t result2 = H5Q_QUEUE_HEAD_INITIALIZER(result2);

        if (FAIL == H5Qget_combine_op(query_id, &combine_op))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTGET, FAIL, "unable to get combine op");
        if (FAIL == H5Qget_components(query_id, &sub_query1_id, &sub_query2_id))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTGET, FAIL, "unable to get components");

        if (FAIL == H5X__mdhim_query_components(db, sub_query1_id, &result1))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "unable to apply query");

//        H5X_MDHIM_LOG_DEBUG("###########################");
//        if (!H5Q_QUEUE_EMPTY(&result1)) {
//            H5X_mdhim_entry_t *entry;
//            H5X_MDHIM_LOG_DEBUG("Query returns %zu references", result1.n_elem);
//            H5Q_QUEUE_FOREACH(entry, &result1, entry)
//                print_ref(entry->ref);
//        }
//        H5X_MDHIM_LOG_DEBUG("###########################");

        if (FAIL == H5X__mdhim_query_components(db, sub_query2_id, &result2))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTCOMPARE, FAIL, "unable to apply query");

//        H5X_MDHIM_LOG_DEBUG("###########################");
//        if (!H5Q_QUEUE_EMPTY(&result2)) {
//            H5X_mdhim_entry_t *entry;
//            H5X_MDHIM_LOG_DEBUG("Query returns %zu references", result2.n_elem);
//            H5Q_QUEUE_FOREACH(entry, &result2, entry)
//                print_ref(entry->ref);
//        }
//        H5X_MDHIM_LOG_DEBUG("###########################");

        if (FAIL == H5X__mdhim_query_combine(combine_op, &result1, &result2, result))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTMERGE, FAIL, "unable to merge results");

//        H5X_MDHIM_LOG_DEBUG("###########################");
//        if (!H5Q_QUEUE_EMPTY(result)) {
//            H5X_mdhim_entry_t *entry;
//            H5X_MDHIM_LOG_DEBUG("Query returns %zu references", result->n_elem);
//            H5Q_QUEUE_FOREACH(entry, result, entry)
//                print_ref(entry->ref);
//        }
//        H5X_MDHIM_LOG_DEBUG("###########################");

        if (FAIL == H5X__mdhim_remove_entries(&result1))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTFREE, FAIL, "unable to remove entries");
        if (FAIL == H5X__mdhim_remove_entries(&result2))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTFREE, FAIL, "unable to remove entries");
        if (FAIL == H5Qclose(sub_query1_id))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTCLOSEOBJ, FAIL, "unable to close query");
        if (FAIL == H5Qclose(sub_query2_id))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTCLOSEOBJ, FAIL, "unable to close query");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__mdhim_query_components */

static herr_t
H5X__mdhim_query_singleton(H5X_mdhim_t *db, hid_t query_id, H5X_mdhim_head_t *result)
{
    void *skey;
    size_t skey_size;
    H5Q_t *query; /* TODO */
    H5Q_type_t query_type;
    struct mdhim_t *mdp = NULL;
    struct index_t *dbp = NULL;
    struct mdhim_bgetrm_t *bgrm = NULL;
    int nbytes_left = 0;
    const unsigned char *buf_ptr = NULL;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if (FAIL == H5Qget_type(query_id, &query_type))
        HGOTO_ERROR(H5E_QUERY, H5E_CANTGET, FAIL, "unable to get query type");

    /* TODO add public function */
    if (NULL == (query = (H5Q_t *) H5I_object_verify(query_id, H5I_QUERY)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a query ID");

    switch (query_type) {
        case H5Q_TYPE_LINK_NAME:
        {
            skey = query->query.select.elem.link_name.name;
            skey_size = HDstrlen(query->query.select.elem.link_name.name) + 1;

            mdp = db->obj_refs_md;
            dbp = db->link_names_idx;
        }
            break;
        case H5Q_TYPE_ATTR_NAME:
        {
            skey = query->query.select.elem.attr_name.name;
            skey_size = HDstrlen(query->query.select.elem.attr_name.name) + 1;

            mdp = db->attr_refs_md;
            dbp = db->attr_names_idx;
        }
            break;
        case H5Q_TYPE_ATTR_VALUE:
        {
            skey = query->query.select.elem.data_elem.value;
            skey_size = query->query.select.elem.data_elem.type_size;

            mdp = db->attr_refs_md;
            dbp = db->attr_values_idx;
        }
            break;
        case H5Q_TYPE_DATA_ELEM:
        case H5Q_TYPE_MISC:
        default:
            HGOTO_ERROR(H5E_QUERY, H5E_BADTYPE, FAIL, "unsupported/unrecognized query type");
            break;
    }

    bgrm = mdhimGet(mdp, dbp, skey, (int)skey_size, MDHIM_GET_PRIMARY_EQ);
    if (!bgrm || bgrm->error)
        HGOTO_ERROR(H5E_INDEX, H5E_CANTGET, FAIL, "Error getting key/value from MDHIM");

    nbytes_left = bgrm->value_lens[0];
    buf_ptr = bgrm->values[0];
    while (nbytes_left) {
        H5X_mdhim_entry_t *entry;
        size_t nbytes;

//        H5X_MDHIM_LOG_DEBUG("Bytes left: %d", nbytes_left);
        if (NULL == (entry = (H5X_mdhim_entry_t *) H5MM_malloc(sizeof(H5X_mdhim_entry_t))))
            HGOTO_ERROR(H5E_QUERY, H5E_CANTALLOC, FAIL, "can't allocate ref entry");
        entry->ref = NULL;
        if (NULL == (entry->ref = H5R_decode(buf_ptr, &nbytes)))
            HGOTO_ERROR(H5E_REFERENCE, H5E_CANTDECODE, FAIL, "can't decode reference");

//        print_ref(entry->ref);
//        H5X_MDHIM_LOG_DEBUG("Nbytes: %d", nbytes);
        H5Q_QUEUE_INSERT_TAIL(result, entry, entry);

        buf_ptr += nbytes;
        nbytes_left -= (int)nbytes;
    }

done:
    mdhim_full_release_msg(bgrm);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5X__mdhim_query_singleton */

static herr_t
H5X__mdhim_query_combine(H5Q_combine_op_t combine_op, H5X_mdhim_head_t *result1,
    H5X_mdhim_head_t *result2, H5X_mdhim_head_t *result)
{
    H5X_mdhim_entry_t *entry1;
    H5X_mdhim_entry_t *entry2;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    if (combine_op == H5Q_COMBINE_AND) {
        H5Q_QUEUE_FOREACH(entry1, result1, entry) {
            H5R_type_t ref_type1 = H5Rget_type(entry1->ref);
            H5Q_QUEUE_FOREACH(entry2, result2, entry) {
                H5R_type_t ref_type2 = H5Rget_type(entry2->ref);

//                H5X_MDHIM_LOG_DEBUG("Comparing references:");
//                print_ref(entry1->ref);
//                print_ref(entry2->ref);

                if (H5Requal(entry1->ref, entry2->ref)) {
                    if (FAIL == H5X__mdhim_push_entry(result, entry1))
                        HGOTO_ERROR(H5E_INDEX, H5E_CANTMERGE, FAIL, "unable to add result");
                }
                else if (ref_type1 == H5R_EXT_ATTR && ref_type2 == H5R_EXT_OBJECT) {
                    char obj_name1[H5X_MDHIM_MAX_NAME_LEN];
                    char obj_name2[H5X_MDHIM_MAX_NAME_LEN];

                    /* Only combine if obj_names are equal */
                    H5R__get_obj_name(NULL, H5P_DEFAULT, H5P_DEFAULT, entry1->ref,
                        obj_name1, H5X_MDHIM_MAX_NAME_LEN);
                    H5R__get_obj_name(NULL, H5P_DEFAULT, H5P_DEFAULT, entry2->ref,
                        obj_name2, H5X_MDHIM_MAX_NAME_LEN);
                    if (0 == HDstrcmp(obj_name1, obj_name2)) {
                        if (FAIL == H5X__mdhim_push_entry(result, entry2))
                            HGOTO_ERROR(H5E_INDEX, H5E_CANTMERGE, FAIL, "unable to add result");
                    }
                }
                else if (ref_type1 == H5R_EXT_OBJECT && ref_type2 == H5R_EXT_ATTR) {
                    char obj_name1[H5X_MDHIM_MAX_NAME_LEN];
                    char obj_name2[H5X_MDHIM_MAX_NAME_LEN];

                    /* Only combine if obj_names are equal */
                    H5R__get_obj_name(NULL, H5P_DEFAULT, H5P_DEFAULT, entry1->ref,
                        obj_name1, H5X_MDHIM_MAX_NAME_LEN);
                    H5R__get_obj_name(NULL, H5P_DEFAULT, H5P_DEFAULT, entry2->ref,
                        obj_name2, H5X_MDHIM_MAX_NAME_LEN);
                    if (0 == HDstrcmp(obj_name1, obj_name2)) {
                        if (FAIL == H5X__mdhim_push_entry(result, entry1))
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
} /* end H5X__mdhim_query_combine */

#endif /* H5_HAVE_MDHIM */
