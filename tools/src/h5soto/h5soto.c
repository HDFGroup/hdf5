/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose: Query onion-backed HDF5 revision history and materialize a chosen
 *          revision into a standalone HDF5 file.
 */

#include "hdf5.h"
#include "h5tools.h"
#include "h5tools_utils.h"

#include <errno.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Name of tool */
#define PROGRAMNAME "h5soto"

#define H5SOTO_TMP_ROOT_NAME "__h5soto_materialized_root__"

#define H5SOTO_FNV_OFFSET 14695981039346656037ULL
#define H5SOTO_FNV_PRIME  1099511628211ULL

#define H5SOTO_HASH_MAX_BUF_BYTES (64ULL * 1024 * 1024) /* max bytes read at once when hashing */

typedef enum h5soto_entry_kind_t {
    H5SOTO_ENTRY_GROUP = 0,
    H5SOTO_ENTRY_DATASET,
    H5SOTO_ENTRY_NAMED_DATATYPE,
    H5SOTO_ENTRY_SOFT_LINK,
    H5SOTO_ENTRY_EXTERNAL_LINK,
    H5SOTO_ENTRY_USER_LINK
} h5soto_entry_kind_t;

typedef struct h5soto_entry_t {
    char               *path;
    h5soto_entry_kind_t kind;
    uint64_t            signature;
} h5soto_entry_t;

typedef struct h5soto_inventory_t {
    h5soto_entry_t *entries;
    size_t          nentries;
    size_t          capacity;
} h5soto_inventory_t;

typedef struct h5soto_token_set_t {
    hid_t        loc_id;
    H5O_token_t *tokens;
    size_t       ntokens;
    size_t       capacity;
} h5soto_token_set_t;

typedef struct h5soto_walk_ctx_t {
    hid_t               file_id;
    h5soto_inventory_t *inventory;
    h5soto_token_set_t  visited_groups;
} h5soto_walk_ctx_t;

typedef struct h5soto_attr_hash_ctx_t {
    uint64_t *hash;
    herr_t    status;
} h5soto_attr_hash_ctx_t;

typedef struct h5soto_attr_copy_ctx_t {
    hid_t  dst_id;
    herr_t status;
} h5soto_attr_copy_ctx_t;

typedef struct h5soto_string_list_t {
    char **items;
    size_t nitems;
    size_t capacity;
} h5soto_string_list_t;

typedef struct h5soto_ref_list_t {
    const h5soto_entry_t **items;
    size_t                 nitems;
    size_t                 capacity;
} h5soto_ref_list_t;

typedef struct h5soto_group_iter_ctx_t {
    const char        *group_path;
    h5soto_walk_ctx_t *walk_ctx;
    herr_t             status;
} h5soto_group_iter_ctx_t;

typedef struct h5soto_child_name_ctx_t {
    h5soto_string_list_t *names;
    herr_t                status;
} h5soto_child_name_ctx_t;

typedef struct h5soto_options_t {
    const char *filename;
    const char *output_filename;
    bool        verbose;
    bool        enable_error_stack;
    bool        materialize;
    uint64_t    materialize_revision; /* H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST = "latest" */
    bool        force;                /* overwrite existing output file */
    bool        list;
    uint64_t    list_revision; /* H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST = "latest" */
    bool        has_from;
    uint64_t    from_revision;
    bool        has_to;
    uint64_t    to_revision;
} h5soto_options_t;

static const char            *s_opts   = "hVm:o:vE*fl:12";
static struct h5_long_options l_opts[] = {{"help", no_arg, 'h'},
                                          {"version", no_arg, 'V'},
                                          {"materialize", require_arg, 'm'},
                                          {"output", require_arg, 'o'},
                                          {"verbose", no_arg, 'v'},
                                          {"enable-error-stack", optional_arg, 'E'},
                                          {"force", no_arg, 'f'},
                                          {"list", require_arg, 'l'},
                                          {"from", require_arg, '1'},
                                          {"to", require_arg, '2'},
                                          {NULL, 0, '\0'}};

static void usage(const char *prog);
static void leave(int ret) H5_ATTR_NORETURN;

static void
h5soto_hash_bytes(uint64_t *hash, const void *buf, size_t len)
{
    const unsigned char *bytes = (const unsigned char *)buf;

    if (!hash || !buf)
        return;

    for (size_t i = 0; i < len; i++) {
        *hash ^= (uint64_t)bytes[i];
        *hash *= H5SOTO_FNV_PRIME;
    }
}

static void
h5soto_hash_u64(uint64_t *hash, uint64_t value)
{
    h5soto_hash_bytes(hash, &value, sizeof(value));
}

static void
h5soto_hash_str(uint64_t *hash, const char *s)
{
    size_t len = 0;

    if (s)
        len = strlen(s);

    h5soto_hash_u64(hash, (uint64_t)len);
    if (len > 0)
        h5soto_hash_bytes(hash, s, len);
}

static int
h5soto_checked_mul_size(size_t a, size_t b, size_t *out)
{
    if (!out)
        return -1;

    if (a > 0 && b > (SIZE_MAX / a))
        return -1;

    *out = a * b;
    return 0;
}

static void
h5soto_inventory_init(h5soto_inventory_t *inventory)
{
    if (inventory)
        memset(inventory, 0, sizeof(*inventory));
}

static void
h5soto_inventory_free(h5soto_inventory_t *inventory)
{
    if (!inventory)
        return;

    for (size_t i = 0; i < inventory->nentries; i++)
        free(inventory->entries[i].path);

    free(inventory->entries);
    memset(inventory, 0, sizeof(*inventory));
}

static herr_t
h5soto_inventory_append(h5soto_inventory_t *inventory, const char *path, h5soto_entry_kind_t kind,
                        uint64_t signature)
{
    h5soto_entry_t *new_entries = NULL;
    size_t          new_cap     = 0;
    char           *path_copy   = NULL;

    if (!inventory || !path)
        return FAIL;

    if (NULL == (path_copy = strdup(path)))
        return FAIL;

    if (inventory->nentries == inventory->capacity) {
        new_cap = (inventory->capacity > 0) ? (inventory->capacity * 2) : 16;
        if (new_cap < inventory->capacity)
            goto error;

        if (NULL ==
            (new_entries = (h5soto_entry_t *)realloc(inventory->entries, new_cap * sizeof(*new_entries))))
            goto error;

        inventory->entries  = new_entries;
        inventory->capacity = new_cap;
    }

    inventory->entries[inventory->nentries].path      = path_copy;
    inventory->entries[inventory->nentries].kind      = kind;
    inventory->entries[inventory->nentries].signature = signature;
    inventory->nentries++;

    return SUCCEED;

error:
    free(path_copy);
    return FAIL;
}

static int
h5soto_entry_path_cmp(const void *_lhs, const void *_rhs)
{
    const h5soto_entry_t *lhs = (const h5soto_entry_t *)_lhs;
    const h5soto_entry_t *rhs = (const h5soto_entry_t *)_rhs;

    return strcmp(lhs->path, rhs->path);
}

static void
h5soto_string_list_init(h5soto_string_list_t *list)
{
    if (list)
        memset(list, 0, sizeof(*list));
}

static void
h5soto_string_list_free(h5soto_string_list_t *list)
{
    if (!list)
        return;

    for (size_t i = 0; i < list->nitems; i++)
        free(list->items[i]);

    free(list->items);
    memset(list, 0, sizeof(*list));
}

static herr_t
h5soto_string_list_append(h5soto_string_list_t *list, const char *value)
{
    char **new_items = NULL;
    size_t new_cap   = 0;
    char  *copy      = NULL;

    if (!list || !value)
        return FAIL;

    if (NULL == (copy = strdup(value)))
        return FAIL;

    if (list->nitems == list->capacity) {
        new_cap = (list->capacity > 0) ? (list->capacity * 2) : 8;
        if (new_cap < list->capacity)
            goto error;

        if (NULL == (new_items = (char **)realloc(list->items, new_cap * sizeof(*new_items))))
            goto error;

        list->items    = new_items;
        list->capacity = new_cap;
    }

    list->items[list->nitems++] = copy;
    return SUCCEED;

error:
    free(copy);
    return FAIL;
}

static void
h5soto_ref_list_init(h5soto_ref_list_t *list)
{
    if (list)
        memset(list, 0, sizeof(*list));
}

static void
h5soto_ref_list_free(h5soto_ref_list_t *list)
{
    if (!list)
        return;

    free(list->items);
    memset(list, 0, sizeof(*list));
}

static herr_t
h5soto_ref_list_append(h5soto_ref_list_t *list, const h5soto_entry_t *entry)
{
    const h5soto_entry_t **new_items = NULL;
    size_t                 new_cap   = 0;

    if (!list || !entry)
        return FAIL;

    if (list->nitems == list->capacity) {
        new_cap = (list->capacity > 0) ? (list->capacity * 2) : 8;
        if (new_cap < list->capacity)
            return FAIL;

        if (NULL == (new_items = (const h5soto_entry_t **)realloc(list->items, new_cap * sizeof(*new_items))))
            return FAIL;

        list->items    = new_items;
        list->capacity = new_cap;
    }

    list->items[list->nitems++] = entry;
    return SUCCEED;
}

static void
h5soto_token_set_init(h5soto_token_set_t *set, hid_t loc_id)
{
    if (!set)
        return;

    memset(set, 0, sizeof(*set));
    set->loc_id = loc_id;
}

static void
h5soto_token_set_free(h5soto_token_set_t *set)
{
    if (!set)
        return;

    free(set->tokens);
    memset(set, 0, sizeof(*set));
}

static bool
h5soto_token_set_contains(const h5soto_token_set_t *set, const H5O_token_t *token)
{
    int cmp = 0;

    if (!set || !token)
        return false;

    for (size_t i = 0; i < set->ntokens; i++) {
        if (H5Otoken_cmp(set->loc_id, token, &set->tokens[i], &cmp) < 0)
            return false;
        if (cmp == 0)
            return true;
    }

    return false;
}

static herr_t
h5soto_token_set_add(h5soto_token_set_t *set, const H5O_token_t *token)
{
    H5O_token_t *new_tokens = NULL;
    size_t       new_cap    = 0;

    if (!set || !token)
        return FAIL;

    if (h5soto_token_set_contains(set, token))
        return SUCCEED;

    if (set->ntokens == set->capacity) {
        new_cap = (set->capacity > 0) ? (set->capacity * 2) : 8;
        if (new_cap < set->capacity)
            return FAIL;

        if (NULL == (new_tokens = (H5O_token_t *)realloc(set->tokens, new_cap * sizeof(*new_tokens))))
            return FAIL;

        set->tokens   = new_tokens;
        set->capacity = new_cap;
    }

    set->tokens[set->ntokens++] = *token;
    return SUCCEED;
}

static const char *
h5soto_kind_name(h5soto_entry_kind_t kind)
{
    switch (kind) {
        case H5SOTO_ENTRY_GROUP:
            return "group";
        case H5SOTO_ENTRY_DATASET:
            return "dataset";
        case H5SOTO_ENTRY_NAMED_DATATYPE:
            return "datatype";
        case H5SOTO_ENTRY_SOFT_LINK:
            return "soft-link";
        case H5SOTO_ENTRY_EXTERNAL_LINK:
            return "external-link";
        case H5SOTO_ENTRY_USER_LINK:
            return "user-link";
        default:
            return "unknown";
    }
}

static h5soto_entry_kind_t
h5soto_kind_from_id_type(H5I_type_t id_type)
{
    switch (id_type) {
        case H5I_GROUP:
            return H5SOTO_ENTRY_GROUP;
        case H5I_DATASET:
            return H5SOTO_ENTRY_DATASET;
        case H5I_DATATYPE:
            return H5SOTO_ENTRY_NAMED_DATATYPE;
        default:
            return H5SOTO_ENTRY_USER_LINK;
    }
}

static h5soto_entry_kind_t
h5soto_kind_from_link_type(H5L_type_t link_type)
{
    switch (link_type) {
        case H5L_TYPE_SOFT:
            return H5SOTO_ENTRY_SOFT_LINK;
        case H5L_TYPE_EXTERNAL:
            return H5SOTO_ENTRY_EXTERNAL_LINK;
        default:
            return H5SOTO_ENTRY_USER_LINK;
    }
}

static char *
h5soto_join_path(const char *parent, const char *name)
{
    char  *result = NULL;
    size_t len    = 0;

    if (!parent || !name)
        return NULL;

    if (!strcmp(parent, "/"))
        len = strlen(name) + 2;
    else
        len = strlen(parent) + strlen(name) + 2;

    if (NULL == (result = (char *)malloc(len)))
        return NULL;

    if (!strcmp(parent, "/"))
        snprintf(result, len, "/%s", name);
    else
        snprintf(result, len, "%s/%s", parent, name);

    return result;
}

static herr_t
h5soto_read_onion_page_size(const char *filename, uint32_t *page_size_out)
{
    static const unsigned char expected_signature[4] = {'O', 'H', 'D', 'H'};
    unsigned char              header[12];
    size_t                     onion_name_len = 0;
    char                      *onion_name     = NULL;
    FILE                      *stream         = NULL;
    uint32_t                   page_size      = 0;
    herr_t                     ret_value      = FAIL;

    if (!filename || !page_size_out)
        return FAIL;

    onion_name_len = strlen(filename) + strlen(".onion") + 1;
    if (NULL == (onion_name = (char *)malloc(onion_name_len)))
        goto done;

    snprintf(onion_name, onion_name_len, "%s.onion", filename);

    if (NULL == (stream = fopen(onion_name, "rb")))
        goto done;

    if (fread(header, 1, sizeof(header), stream) != sizeof(header))
        goto done;

    if (memcmp(header, expected_signature, sizeof(expected_signature)) != 0)
        goto done;
    if (header[4] != 1)
        goto done;

    page_size = (uint32_t)header[8] | ((uint32_t)header[9] << 8) | ((uint32_t)header[10] << 16) |
                ((uint32_t)header[11] << 24);

    if (page_size == 0 || (page_size & (page_size - 1)) != 0)
        goto done;

    *page_size_out = page_size;
    ret_value      = SUCCEED;

done:
    if (stream)
        fclose(stream);
    free(onion_name);
    return ret_value;
}

static bool
h5soto_onion_sidecar_exists(const char *filename)
{
    size_t onion_len  = 0;
    char  *onion_path = NULL;
    FILE  *probe      = NULL;
    bool   exists     = false;

    if (!filename)
        return false;

    onion_len = strlen(filename) + strlen(".onion") + 1;
    if (NULL == (onion_path = (char *)malloc(onion_len)))
        return false;

    snprintf(onion_path, onion_len, "%s.onion", filename);
    probe = fopen(onion_path, "rb");
    if (probe) {
        fclose(probe);
        exists = true;
    }

    free(onion_path);
    return exists;
}

static herr_t
h5soto_hash_encoded_object(uint64_t *hash, herr_t (*encode_fn)(hid_t, void *, size_t *), hid_t obj_id)
{
    unsigned char *buf  = NULL;
    size_t         size = 0;
    herr_t         ret  = FAIL;

    if (!hash || !encode_fn)
        return FAIL;

    if (encode_fn(obj_id, NULL, &size) < 0)
        goto done;

    h5soto_hash_u64(hash, (uint64_t)size);
    if (size == 0) {
        ret = SUCCEED;
        goto done;
    }

    if (NULL == (buf = (unsigned char *)malloc(size)))
        goto done;

    if (encode_fn(obj_id, buf, &size) < 0)
        goto done;

    h5soto_hash_bytes(hash, buf, size);
    ret = SUCCEED;

done:
    free(buf);
    return ret;
}

static herr_t
h5soto_encode_space(hid_t obj_id, void *buf, size_t *nalloc)
{
    return H5Sencode2(obj_id, buf, nalloc, H5P_DEFAULT);
}

static herr_t
h5soto_encode_plist(hid_t obj_id, void *buf, size_t *nalloc)
{
    return H5Pencode2(obj_id, buf, nalloc, H5P_DEFAULT);
}

static herr_t h5soto_hash_type_value(const void *value, hid_t type_id, uint64_t *hash);

static herr_t
h5soto_hash_typed_buffer(const void *buf, size_t npoints, hid_t type_id, uint64_t *hash)
{
    size_t type_size = 0;

    if (!hash)
        return FAIL;
    if (npoints == 0)
        return SUCCEED;
    if (!buf)
        return FAIL;

    type_size = H5Tget_size(type_id);
    if (0 == type_size)
        return FAIL;

    for (size_t i = 0; i < npoints; i++) {
        const unsigned char *elem = (const unsigned char *)buf + (i * type_size);

        if (h5soto_hash_type_value(elem, type_id, hash) < 0)
            return FAIL;
    }

    return SUCCEED;
}

static herr_t
h5soto_hash_type_value(const void *value, hid_t type_id, uint64_t *hash)
{
    H5T_class_t class_id = H5Tget_class(type_id);
    size_t      size     = H5Tget_size(type_id);

    if (!value || !hash || class_id < 0 || 0 == size)
        return FAIL;

    /* Hash atomic values by their stored bytes, but recurse into nested or
     * indirect types so the hash follows logical contents instead of metadata
     * such as heap addresses. */
    switch (class_id) {
        case H5T_INTEGER:
        case H5T_FLOAT:
        case H5T_TIME:
        case H5T_BITFIELD:
        case H5T_OPAQUE:
        case H5T_ENUM:
        case H5T_REFERENCE:
            h5soto_hash_bytes(hash, value, size);
            break;

        case H5T_STRING:
            if (H5Tis_variable_str(type_id) > 0) {
                const char *str = *(const char *const *)value;

                /* Variable-length strings are stored through a heap pointer;
                 * hash the pointed-to bytes, not the pointer value. */
                h5soto_hash_str(hash, str);
            }
            else
                h5soto_hash_bytes(hash, value, size);
            break;

        case H5T_ARRAY: {
            hid_t   base_type = H5I_INVALID_HID;
            int     ndims     = 0;
            hsize_t dims[H5S_MAX_RANK];
            size_t  count     = 1;
            size_t  base_size = 0;

            if ((base_type = H5Tget_super(type_id)) < 0)
                return FAIL;

            if ((ndims = H5Tget_array_ndims(type_id)) < 0 || ndims > H5S_MAX_RANK) {
                H5Tclose(base_type);
                return FAIL;
            }

            if (ndims > 0 && H5Tget_array_dims2(type_id, dims) < 0) {
                H5Tclose(base_type);
                return FAIL;
            }

            for (int i = 0; i < ndims; i++) {
                if (dims[i] > SIZE_MAX || count > (SIZE_MAX / (size_t)dims[i])) {
                    H5Tclose(base_type);
                    return FAIL;
                }
                count *= (size_t)dims[i];
            }

            base_size = H5Tget_size(base_type);
            if (0 == base_size) {
                H5Tclose(base_type);
                return FAIL;
            }

            /* Hash each logical array element with the array's base type so
             * nested arrays/compounds are expanded consistently. */
            for (size_t i = 0; i < count; i++) {
                const unsigned char *elem = (const unsigned char *)value + (i * base_size);

                if (h5soto_hash_type_value(elem, base_type, hash) < 0) {
                    H5Tclose(base_type);
                    return FAIL;
                }
            }

            H5Tclose(base_type);
            break;
        }

        case H5T_VLEN: {
            const hvl_t *vlen      = (const hvl_t *)value;
            hid_t        base_type = H5I_INVALID_HID;
            size_t       base_size = 0;

            if ((base_type = H5Tget_super(type_id)) < 0)
                return FAIL;

            base_size = H5Tget_size(base_type);
            if (0 == base_size) {
                H5Tclose(base_type);
                return FAIL;
            }

            /* Include the sequence length so equal prefixes with different
             * extents do not collide, then hash each element in order. */
            h5soto_hash_u64(hash, (uint64_t)vlen->len);

            for (size_t i = 0; i < vlen->len; i++) {
                const unsigned char *elem = (const unsigned char *)vlen->p + (i * base_size);

                if (h5soto_hash_type_value(elem, base_type, hash) < 0) {
                    H5Tclose(base_type);
                    return FAIL;
                }
            }

            H5Tclose(base_type);
            break;
        }

        case H5T_COMPOUND: {
            int nmembers = H5Tget_nmembers(type_id);

            if (nmembers < 0)
                return FAIL;

            /* Walk members in declaration order and hash each member value at
             * its byte offset, avoiding any dependence on struct padding. */
            for (int i = 0; i < nmembers; i++) {
                hid_t  member_type = H5I_INVALID_HID;
                size_t offset      = 0;

                if ((member_type = H5Tget_member_type(type_id, (unsigned)i)) < 0)
                    return FAIL;

                offset = H5Tget_member_offset(type_id, (unsigned)i);
                if (h5soto_hash_type_value(((const unsigned char *)value) + offset, member_type, hash) < 0) {
                    H5Tclose(member_type);
                    return FAIL;
                }

                H5Tclose(member_type);
            }
            break;
        }

        default:
            h5soto_hash_bytes(hash, value, size);
            break;
    }

    return SUCCEED;
}

static herr_t
h5soto_alloc_typed_buffer(hid_t type_id, hid_t space_id, void **buf_out, size_t *npoints_out,
                          bool *reclaim_out)
{
    H5S_class_t stype      = H5S_NO_CLASS;
    hssize_t    npoints_ss = 0;
    size_t      type_size  = 0;
    size_t      alloc_size = 0;
    htri_t      has_vlen   = 0;
    htri_t      is_vstr    = 0;

    if (!buf_out || !npoints_out || !reclaim_out)
        return FAIL;

    *buf_out     = NULL;
    *npoints_out = 0;
    *reclaim_out = false;

    if ((stype = H5Sget_simple_extent_type(space_id)) == H5S_NO_CLASS)
        return FAIL;
    if (stype == H5S_NULL)
        return SUCCEED;

    if ((npoints_ss = H5Sget_simple_extent_npoints(space_id)) < 0)
        return FAIL;
    if ((uint64_t)npoints_ss > SIZE_MAX)
        return FAIL;

    type_size = H5Tget_size(type_id);
    if (0 == type_size)
        return FAIL;

    if (h5soto_checked_mul_size((size_t)npoints_ss, type_size, &alloc_size) < 0)
        return FAIL;

    if (alloc_size > 0) {
        if (NULL == (*buf_out = calloc(1, alloc_size)))
            return FAIL;
    }

    has_vlen = H5Tdetect_class(type_id, H5T_VLEN);
    is_vstr  = H5Tis_variable_str(type_id);
    if (has_vlen < 0 || is_vstr < 0)
        return FAIL;

    *npoints_out = (size_t)npoints_ss;
    *reclaim_out = ((has_vlen > 0) || (is_vstr > 0));

    return SUCCEED;
}

static herr_t
h5soto_hash_attributes_cb(hid_t obj_id, const char *attr_name, const H5A_info_t *ainfo, void *op_data)
{
    h5soto_attr_hash_ctx_t *ctx       = (h5soto_attr_hash_ctx_t *)op_data;
    hid_t                   attr_id   = H5I_INVALID_HID;
    hid_t                   type_id   = H5I_INVALID_HID;
    hid_t                   space_id  = H5I_INVALID_HID;
    void                   *buf       = NULL;
    bool                    reclaim   = false;
    size_t                  npoints   = 0;
    herr_t                  ret_value = FAIL;

    (void)ainfo;

    if (!ctx || !ctx->hash)
        return FAIL;

    h5soto_hash_str(ctx->hash, attr_name);

    if ((attr_id = H5Aopen(obj_id, attr_name, H5P_DEFAULT)) < 0)
        goto done;
    if ((type_id = H5Aget_type(attr_id)) < 0)
        goto done;
    if ((space_id = H5Aget_space(attr_id)) < 0)
        goto done;

    if (h5soto_hash_encoded_object(ctx->hash, H5Tencode, type_id) < 0)
        goto done;
    if (h5soto_hash_encoded_object(ctx->hash, h5soto_encode_space, space_id) < 0)
        goto done;

    if (h5soto_alloc_typed_buffer(type_id, space_id, &buf, &npoints, &reclaim) < 0)
        goto done;
    if (buf && H5Aread(attr_id, type_id, buf) < 0)
        goto done;
    if (h5soto_hash_typed_buffer(buf, npoints, type_id, ctx->hash) < 0)
        goto done;

    ret_value = SUCCEED;

done:
    if (reclaim && buf && type_id >= 0 && space_id >= 0)
        H5Treclaim(type_id, space_id, H5P_DEFAULT, buf);
    free(buf);

    if (space_id >= 0)
        H5Sclose(space_id);
    if (type_id >= 0)
        H5Tclose(type_id);
    if (attr_id >= 0)
        H5Aclose(attr_id);

    ctx->status = ret_value;
    return ret_value;
}

static herr_t
h5soto_hash_object_attributes(hid_t obj_id, uint64_t *hash)
{
    h5soto_attr_hash_ctx_t ctx;
    hsize_t                idx = 0;

    if (!hash)
        return FAIL;

    ctx.hash   = hash;
    ctx.status = SUCCEED;

    if (H5Aiterate2(obj_id, H5_INDEX_NAME, H5_ITER_INC, &idx, h5soto_hash_attributes_cb, &ctx) < 0)
        return FAIL;

    return ctx.status;
}

static herr_t
h5soto_hash_group_signature(hid_t group_id, uint64_t *signature_out)
{
    hid_t    gcpl_id = H5I_INVALID_HID;
    uint64_t hash    = H5SOTO_FNV_OFFSET;

    if (!signature_out)
        return FAIL;

    h5soto_hash_str(&hash, "group");

    if ((gcpl_id = H5Gget_create_plist(group_id)) < 0)
        goto error;
    if (h5soto_hash_encoded_object(&hash, h5soto_encode_plist, gcpl_id) < 0)
        goto error;
    if (h5soto_hash_object_attributes(group_id, &hash) < 0)
        goto error;

    H5Pclose(gcpl_id);

    *signature_out = hash;
    return SUCCEED;

error:
    if (gcpl_id >= 0)
        H5Pclose(gcpl_id);
    return FAIL;
}

static herr_t
h5soto_hash_named_datatype_signature(hid_t type_id, uint64_t *signature_out)
{
    uint64_t hash = H5SOTO_FNV_OFFSET;

    if (!signature_out)
        return FAIL;

    h5soto_hash_str(&hash, "datatype");
    if (h5soto_hash_encoded_object(&hash, H5Tencode, type_id) < 0)
        return FAIL;
    if (h5soto_hash_object_attributes(type_id, &hash) < 0)
        return FAIL;

    *signature_out = hash;
    return SUCCEED;
}

/* Read a simple dataset in first-dimension slabs to bound peak memory usage. */
static herr_t
h5soto_hash_dataset_data_chunked(hid_t dset_id, hid_t type_id, hid_t space_id, size_t type_size,
                                 uint64_t *hash)
{
    int     ndims = 0;
    hsize_t dims[H5S_MAX_RANK];
    hsize_t start[H5S_MAX_RANK];
    hsize_t count[H5S_MAX_RANK];
    hid_t   fspace_sel = H5I_INVALID_HID;
    void   *buf        = NULL;
    size_t  row_elems  = 1;
    size_t  row_bytes;
    hsize_t rows_per_chunk;
    size_t  chunk_elems;
    size_t  chunk_bytes;
    herr_t  ret_value = FAIL;

    ndims = H5Sget_simple_extent_dims(space_id, dims, NULL);
    if (ndims < 1)
        return FAIL;

    for (int i = 1; i < ndims; i++) {
        if (dims[i] == 0 || row_elems > SIZE_MAX / (size_t)dims[i])
            return FAIL;
        row_elems *= (size_t)dims[i];
    }
    if (row_elems == 0 || dims[0] == 0)
        return SUCCEED;

    row_bytes = row_elems * type_size;
    if (row_bytes == 0)
        return FAIL;

    rows_per_chunk = H5SOTO_HASH_MAX_BUF_BYTES / row_bytes;
    if (rows_per_chunk == 0)
        rows_per_chunk = 1;
    if (rows_per_chunk > dims[0])
        rows_per_chunk = dims[0];

    chunk_elems = (size_t)rows_per_chunk * row_elems;
    chunk_bytes = chunk_elems * type_size;

    if (NULL == (buf = calloc(1, chunk_bytes)))
        return FAIL;

    if ((fspace_sel = H5Scopy(space_id)) < 0)
        goto done;

    memset(start, 0, sizeof(start));
    memcpy(count, dims, sizeof(hsize_t) * (size_t)ndims);

    for (hsize_t row = 0; row < dims[0]; row += rows_per_chunk) {
        hsize_t nrows = rows_per_chunk;
        size_t  sel_elems;
        hsize_t m;
        hid_t   mspace = H5I_INVALID_HID;

        if (row + nrows > dims[0])
            nrows = dims[0] - row;
        sel_elems = (size_t)nrows * row_elems;

        start[0] = row;
        count[0] = nrows;

        if (H5Sselect_hyperslab(fspace_sel, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
            goto done;

        m = (hsize_t)sel_elems;
        if ((mspace = H5Screate_simple(1, &m, NULL)) < 0)
            goto done;

        if (H5Dread(dset_id, type_id, mspace, fspace_sel, H5P_DEFAULT, buf) < 0) {
            H5Sclose(mspace);
            goto done;
        }

        if (h5soto_hash_typed_buffer(buf, sel_elems, type_id, hash) < 0) {
            H5Sclose(mspace);
            goto done;
        }

        H5Sclose(mspace);
        memset(buf, 0, chunk_bytes);
    }

    ret_value = SUCCEED;

done:
    if (fspace_sel >= 0)
        H5Sclose(fspace_sel);
    free(buf);
    return ret_value;
}

static herr_t
h5soto_hash_dataset_signature(hid_t dset_id, uint64_t *signature_out)
{
    hid_t       type_id     = H5I_INVALID_HID;
    hid_t       space_id    = H5I_INVALID_HID;
    hid_t       dcpl_id     = H5I_INVALID_HID;
    void       *buf         = NULL;
    bool        reclaim     = false;
    size_t      npoints     = 0;
    size_t      type_size   = 0;
    size_t      total_bytes = 0;
    H5S_class_t stype       = H5S_NO_CLASS;
    htri_t      has_vlen    = 0;
    htri_t      is_vstr     = 0;
    uint64_t    hash        = H5SOTO_FNV_OFFSET;

    if (!signature_out)
        return FAIL;

    h5soto_hash_str(&hash, "dataset");

    if ((type_id = H5Dget_type(dset_id)) < 0)
        goto error;
    if ((space_id = H5Dget_space(dset_id)) < 0)
        goto error;
    if ((dcpl_id = H5Dget_create_plist(dset_id)) < 0)
        goto error;

    /* Fold in the dataset's declared type, dataspace, creation properties, and
     * attributes before hashing the stored element values. */
    if (h5soto_hash_encoded_object(&hash, H5Tencode, type_id) < 0)
        goto error;
    if (h5soto_hash_encoded_object(&hash, h5soto_encode_space, space_id) < 0)
        goto error;
    if (h5soto_hash_encoded_object(&hash, h5soto_encode_plist, dcpl_id) < 0)
        goto error;
    if (h5soto_hash_object_attributes(dset_id, &hash) < 0)
        goto error;

    /* Determine whether to use chunked or single-shot data reading */
    stype = H5Sget_simple_extent_type(space_id);
    if (stype == H5S_NO_CLASS)
        goto error;

    /* NULL dataspaces have no payload to read; their encoded dataspace above is
     * the complete shape contribution to the signature. */
    if (stype != H5S_NULL) {
        hssize_t npoints_ss = H5Sget_simple_extent_npoints(space_id);
        if (npoints_ss < 0)
            goto error;
        if ((uint64_t)npoints_ss > SIZE_MAX)
            goto error;
        npoints = (size_t)npoints_ss;

        type_size = H5Tget_size(type_id);
        if (0 == type_size)
            goto error;

        has_vlen = H5Tdetect_class(type_id, H5T_VLEN);
        is_vstr  = H5Tis_variable_str(type_id);
        if (has_vlen < 0 || is_vstr < 0)
            goto error;
        reclaim = (has_vlen > 0) || (is_vstr > 0);

        if (h5soto_checked_mul_size(npoints, type_size, &total_bytes) < 0)
            goto error;

        /* Only fixed-size simple datasets can be streamed in slabs. Vlen/vstr
         * reads need a single buffer that matches the full dataspace so
         * H5Treclaim can release the referenced heap data correctly. */
        if (!reclaim && stype == H5S_SIMPLE && total_bytes > H5SOTO_HASH_MAX_BUF_BYTES) {
            /* Large non-vlen dataset: read in slabs to bound memory usage */
            if (h5soto_hash_dataset_data_chunked(dset_id, type_id, space_id, type_size, &hash) < 0)
                goto error;
        }
        else if (total_bytes > 0) {
            if (NULL == (buf = calloc(1, total_bytes)))
                goto error;
            if (H5Dread(dset_id, type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
                goto error;
            if (h5soto_hash_typed_buffer(buf, npoints, type_id, &hash) < 0)
                goto error;
        }
    }

    if (reclaim && buf)
        H5Treclaim(type_id, space_id, H5P_DEFAULT, buf);
    free(buf);
    H5Pclose(dcpl_id);
    H5Sclose(space_id);
    H5Tclose(type_id);

    *signature_out = hash;
    return SUCCEED;

error:
    if (reclaim && buf && type_id >= 0 && space_id >= 0)
        H5Treclaim(type_id, space_id, H5P_DEFAULT, buf);
    free(buf);

    if (dcpl_id >= 0)
        H5Pclose(dcpl_id);
    if (space_id >= 0)
        H5Sclose(space_id);
    if (type_id >= 0)
        H5Tclose(type_id);

    return FAIL;
}

static herr_t
h5soto_hash_link_signature(hid_t parent_id, const char *name, const H5L_info2_t *linfo,
                           uint64_t *signature_out)
{
    unsigned char *buf  = NULL;
    uint64_t       hash = H5SOTO_FNV_OFFSET;

    if (!name || !linfo || !signature_out)
        return FAIL;

    h5soto_hash_str(&hash, "link");
    h5soto_hash_u64(&hash, (uint64_t)linfo->type);

    if (linfo->u.val_size > 0) {
        if (NULL == (buf = (unsigned char *)calloc(1, linfo->u.val_size + 1)))
            return FAIL;

        if (H5Lget_val(parent_id, name, buf, linfo->u.val_size, H5P_DEFAULT) < 0)
            goto error;

        if (linfo->type == H5L_TYPE_EXTERNAL) {
            const char *file_name = NULL;
            const char *obj_name  = NULL;

            if (H5Lunpack_elink_val(buf, linfo->u.val_size, NULL, &file_name, &obj_name) < 0)
                goto error;

            h5soto_hash_str(&hash, file_name);
            h5soto_hash_str(&hash, obj_name);
        }
        else
            h5soto_hash_bytes(&hash, buf, linfo->u.val_size);
    }

    free(buf);
    *signature_out = hash;
    return SUCCEED;

error:
    free(buf);
    return FAIL;
}

static herr_t
h5soto_object_signature(hid_t obj_id, h5soto_entry_kind_t kind, uint64_t *signature_out)
{
    switch (kind) {
        case H5SOTO_ENTRY_GROUP:
            return h5soto_hash_group_signature(obj_id, signature_out);
        case H5SOTO_ENTRY_DATASET:
            return h5soto_hash_dataset_signature(obj_id, signature_out);
        case H5SOTO_ENTRY_NAMED_DATATYPE:
            return h5soto_hash_named_datatype_signature(obj_id, signature_out);
        default:
            return FAIL;
    }
}

static hid_t
h5soto_open_revision(const char *filename, uint64_t revision_num)
{
    hid_t                  backing_fapl_id = H5I_INVALID_HID;
    hid_t                  onion_fapl_id   = H5I_INVALID_HID;
    hid_t                  file_id         = H5I_INVALID_HID;
    uint32_t               page_size       = 0;
    H5FD_onion_fapl_info_t onion_fa        = {H5FD_ONION_FAPL_INFO_VERSION_CURR,
                                              H5I_INVALID_HID,
                                              0,
                                              H5FD_ONION_STORE_TARGET_ONION,
                                              revision_num,
                                              0,
                                              0,
                                              ""};

    if (h5soto_read_onion_page_size(filename, &page_size) < 0)
        goto done;

    onion_fa.page_size = page_size;

    if ((backing_fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto done;
    onion_fa.backing_fapl_id = backing_fapl_id;

    if ((onion_fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto done;
    if (H5Pset_fapl_onion(onion_fapl_id, &onion_fa) < 0)
        goto done;
    if ((file_id = H5Fopen(filename, H5F_ACC_RDONLY, onion_fapl_id)) < 0)
        goto done;

done:
    if (onion_fapl_id >= 0)
        H5Pclose(onion_fapl_id);
    if (backing_fapl_id >= 0)
        H5Pclose(backing_fapl_id);

    return file_id;
}

static herr_t
h5soto_get_latest_revision(const char *filename, uint64_t *latest_revision_out)
{
    hid_t                  backing_fapl_id = H5I_INVALID_HID;
    hid_t                  onion_fapl_id   = H5I_INVALID_HID;
    uint32_t               page_size       = 0;
    H5FD_onion_fapl_info_t onion_fa        = {H5FD_ONION_FAPL_INFO_VERSION_CURR,
                                              H5I_INVALID_HID,
                                              0,
                                              H5FD_ONION_STORE_TARGET_ONION,
                                              H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST,
                                              0,
                                              0,
                                              ""};
    uint64_t               latest_revision = 0;
    herr_t                 ret_value       = FAIL;

    if (!latest_revision_out)
        return FAIL;

    if (h5soto_read_onion_page_size(filename, &page_size) < 0)
        goto done;

    onion_fa.page_size = page_size;

    if ((backing_fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto done;
    onion_fa.backing_fapl_id = backing_fapl_id;

    if ((onion_fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto done;
    if (H5Pset_fapl_onion(onion_fapl_id, &onion_fa) < 0)
        goto done;
    if (H5FDonion_get_revision_count(filename, onion_fapl_id, &latest_revision) < 0)
        goto done;

    *latest_revision_out = latest_revision;
    ret_value            = SUCCEED;

done:
    if (onion_fapl_id >= 0)
        H5Pclose(onion_fapl_id);
    if (backing_fapl_id >= 0)
        H5Pclose(backing_fapl_id);
    return ret_value;
}

static herr_t h5soto_walk_group(hid_t group_id, const char *group_path, h5soto_walk_ctx_t *ctx);

static herr_t
h5soto_walk_group_member(hid_t group_id, const char *group_path, const char *name, const H5L_info2_t *linfo,
                         h5soto_walk_ctx_t *ctx)
{
    hid_t               obj_id  = H5I_INVALID_HID;
    H5I_type_t          id_type = H5I_BADID;
    H5O_info2_t         oinfo;
    h5soto_entry_kind_t kind;
    uint64_t            signature = 0;
    char               *path      = NULL;
    herr_t              ret_value = FAIL;

    if (!group_path || !name || !linfo || !ctx)
        return FAIL;

    memset(&oinfo, 0, sizeof(oinfo));

    if (NULL == (path = h5soto_join_path(group_path, name)))
        goto done;

    if (linfo->type == H5L_TYPE_HARD) {
        if ((obj_id = H5Oopen(group_id, name, H5P_DEFAULT)) < 0)
            goto done;

        id_type = H5Iget_type(obj_id);
        kind    = h5soto_kind_from_id_type(id_type);
        if (kind == H5SOTO_ENTRY_USER_LINK)
            goto done;

        if (h5soto_object_signature(obj_id, kind, &signature) < 0)
            goto done;
        if (h5soto_inventory_append(ctx->inventory, path, kind, signature) < 0)
            goto done;

        if (kind == H5SOTO_ENTRY_GROUP) {
            if (H5Oget_info3(obj_id, &oinfo, H5O_INFO_BASIC) < 0)
                goto done;
            if (!h5soto_token_set_contains(&ctx->visited_groups, &oinfo.token)) {
                if (h5soto_token_set_add(&ctx->visited_groups, &oinfo.token) < 0)
                    goto done;
                if (h5soto_walk_group(obj_id, path, ctx) < 0)
                    goto done;
            }
        }
    }
    else {
        kind = h5soto_kind_from_link_type(linfo->type);
        if (h5soto_hash_link_signature(group_id, name, linfo, &signature) < 0)
            goto done;
        if (h5soto_inventory_append(ctx->inventory, path, kind, signature) < 0)
            goto done;
    }

    ret_value = SUCCEED;

done:
    if (obj_id >= 0)
        H5Oclose(obj_id);
    free(path);
    return ret_value;
}

static herr_t
h5soto_group_iter_cb(hid_t group_id, const char *name, const H5L_info2_t *linfo, void *op_data)
{
    h5soto_group_iter_ctx_t *cb_ctx = (h5soto_group_iter_ctx_t *)op_data;

    if (!cb_ctx || !cb_ctx->group_path || !cb_ctx->walk_ctx)
        return FAIL;

    cb_ctx->status = h5soto_walk_group_member(group_id, cb_ctx->group_path, name, linfo, cb_ctx->walk_ctx);
    return cb_ctx->status;
}

static herr_t
h5soto_walk_group(hid_t group_id, const char *group_path, h5soto_walk_ctx_t *ctx)
{
    hsize_t                 idx = 0;
    h5soto_group_iter_ctx_t iter_ctx;

    iter_ctx.group_path = group_path;
    iter_ctx.walk_ctx   = ctx;
    iter_ctx.status     = SUCCEED;

    if (H5Literate2(group_id, H5_INDEX_NAME, H5_ITER_INC, &idx, h5soto_group_iter_cb, &iter_ctx) < 0)
        return FAIL;

    return iter_ctx.status;
}

static herr_t
h5soto_build_inventory_for_revision(const char *filename, uint64_t revision, h5soto_inventory_t *inventory)
{
    hid_t             file_id = H5I_INVALID_HID;
    hid_t             root_id = H5I_INVALID_HID;
    h5soto_walk_ctx_t walk_ctx;
    H5O_info2_t       root_info;
    uint64_t          signature = 0;
    herr_t            ret_value = FAIL;

    if (!filename || !inventory)
        return FAIL;

    memset(&walk_ctx, 0, sizeof(walk_ctx));
    memset(&root_info, 0, sizeof(root_info));

    if ((file_id = h5soto_open_revision(filename, revision)) < 0)
        goto done;
    if ((root_id = H5Gopen2(file_id, "/", H5P_DEFAULT)) < 0)
        goto done;

    if (h5soto_hash_group_signature(root_id, &signature) < 0)
        goto done;
    if (h5soto_inventory_append(inventory, "/", H5SOTO_ENTRY_GROUP, signature) < 0)
        goto done;

    walk_ctx.file_id   = file_id;
    walk_ctx.inventory = inventory;
    h5soto_token_set_init(&walk_ctx.visited_groups, file_id);

    if (H5Oget_info3(root_id, &root_info, H5O_INFO_BASIC) < 0)
        goto done;
    if (h5soto_token_set_add(&walk_ctx.visited_groups, &root_info.token) < 0)
        goto done;
    if (h5soto_walk_group(root_id, "/", &walk_ctx) < 0)
        goto done;

    qsort(inventory->entries, inventory->nentries, sizeof(*inventory->entries), h5soto_entry_path_cmp);
    ret_value = SUCCEED;

done:
    h5soto_token_set_free(&walk_ctx.visited_groups);
    if (root_id >= 0)
        H5Gclose(root_id);
    if (file_id >= 0)
        H5Fclose(file_id);
    return ret_value;
}

static herr_t
h5soto_copy_attribute_cb(hid_t src_id, const char *attr_name, const H5A_info_t *ainfo, void *op_data)
{
    h5soto_attr_copy_ctx_t *ctx       = (h5soto_attr_copy_ctx_t *)op_data;
    hid_t                   attr_id   = H5I_INVALID_HID;
    hid_t                   type_id   = H5I_INVALID_HID;
    hid_t                   space_id  = H5I_INVALID_HID;
    hid_t                   out_attr  = H5I_INVALID_HID;
    void                   *buf       = NULL;
    bool                    reclaim   = false;
    size_t                  npoints   = 0;
    herr_t                  ret_value = FAIL;

    (void)ainfo;

    if (!ctx)
        return FAIL;

    if ((attr_id = H5Aopen(src_id, attr_name, H5P_DEFAULT)) < 0)
        goto done;
    if ((type_id = H5Aget_type(attr_id)) < 0)
        goto done;
    if ((space_id = H5Aget_space(attr_id)) < 0)
        goto done;

    if (h5soto_alloc_typed_buffer(type_id, space_id, &buf, &npoints, &reclaim) < 0)
        goto done;
    if (buf && H5Aread(attr_id, type_id, buf) < 0)
        goto done;
    if ((out_attr = H5Acreate2(ctx->dst_id, attr_name, type_id, space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto done;
    if (buf && H5Awrite(out_attr, type_id, buf) < 0)
        goto done;

    ret_value = SUCCEED;

done:
    if (reclaim && buf && type_id >= 0 && space_id >= 0)
        H5Treclaim(type_id, space_id, H5P_DEFAULT, buf);
    free(buf);

    if (out_attr >= 0)
        H5Aclose(out_attr);
    if (space_id >= 0)
        H5Sclose(space_id);
    if (type_id >= 0)
        H5Tclose(type_id);
    if (attr_id >= 0)
        H5Aclose(attr_id);

    ctx->status = ret_value;
    return ret_value;
}

static herr_t
h5soto_copy_attributes(hid_t src_id, hid_t dst_id)
{
    hsize_t                idx = 0;
    h5soto_attr_copy_ctx_t ctx;

    ctx.dst_id = dst_id;
    ctx.status = SUCCEED;

    if (H5Aiterate2(src_id, H5_INDEX_NAME, H5_ITER_INC, &idx, h5soto_copy_attribute_cb, &ctx) < 0)
        return FAIL;

    return ctx.status;
}

static herr_t
h5soto_collect_child_name_cb(hid_t group_id, const char *name, const H5L_info2_t *linfo, void *op_data)
{
    h5soto_child_name_ctx_t *ctx = (h5soto_child_name_ctx_t *)op_data;

    (void)group_id;
    (void)linfo;

    if (!ctx || !name)
        return FAIL;

    ctx->status = h5soto_string_list_append(ctx->names, name);
    return ctx->status;
}

static herr_t
h5soto_collect_child_names(hid_t group_id, h5soto_string_list_t *names)
{
    H5G_info_t              ginfo;
    H5_index_t              index = H5_INDEX_NAME;
    h5soto_child_name_ctx_t ctx;
    hsize_t                 idx = 0;

    memset(&ginfo, 0, sizeof(ginfo));

    ctx.names  = names;
    ctx.status = SUCCEED;

    if (H5Gget_info(group_id, &ginfo) < 0)
        return FAIL;

    if (ginfo.max_corder >= 0)
        index = H5_INDEX_CRT_ORDER;

    if (H5Literate2(group_id, index, H5_ITER_INC, &idx, h5soto_collect_child_name_cb, &ctx) < 0) {
        /* Only retry with H5_INDEX_NAME if the callback itself did not fail
         * (i.e., the iteration method was not supported, not an allocation error). */
        if (ctx.status != SUCCEED)
            return FAIL;
        h5soto_string_list_free(names);
        h5soto_string_list_init(names);
        idx        = 0;
        ctx.status = SUCCEED;
        if (H5Literate2(group_id, H5_INDEX_NAME, H5_ITER_INC, &idx, h5soto_collect_child_name_cb, &ctx) < 0)
            return FAIL;
    }

    return ctx.status;
}

static char *
h5soto_default_output_name(const char *input_filename, uint64_t revision)
{
    const char *suffix   = NULL;
    size_t      base_len = 0;
    size_t      out_len  = 0;
    char       *output   = NULL;

    if (!input_filename)
        return NULL;

    suffix = strrchr(input_filename, '.');
    if (suffix && (!strcmp(suffix, ".h5") || !strcmp(suffix, ".hdf5")))
        base_len = (size_t)(suffix - input_filename);
    else
        base_len = strlen(input_filename);

    out_len = base_len + 32;
    if (NULL == (output = (char *)malloc(out_len)))
        return NULL;

    snprintf(output, out_len, "%.*s.revision-%" PRIu64 ".h5", (int)base_len, input_filename, revision);
    return output;
}

static herr_t
h5soto_materialize_revision(const char *filename, uint64_t revision, const char *output_filename, bool force)
{
    hid_t                src_file     = H5I_INVALID_HID;
    hid_t                dst_file     = H5I_INVALID_HID;
    hid_t                dst_fcpl     = H5I_INVALID_HID;
    hid_t                tmp_group_id = H5I_INVALID_HID;
    h5soto_string_list_t child_names;
    bool                 remove_output = false;
    herr_t               ret_value     = FAIL;

    h5soto_string_list_init(&child_names);

    if ((src_file = h5soto_open_revision(filename, revision)) < 0)
        goto done;

    /* Guard against a source root member whose name matches the temp copy target */
    if (H5Lexists(src_file, H5SOTO_TMP_ROOT_NAME, H5P_DEFAULT) > 0) {
        error_msg("source file root group contains a member named '%s', "
                  "which conflicts with an internal temporary name used during materialization\n",
                  H5SOTO_TMP_ROOT_NAME);
        goto done;
    }

    if ((dst_fcpl = H5Fget_create_plist(src_file)) < 0)
        goto done;
    if ((dst_file = H5Fcreate(output_filename, force ? H5F_ACC_TRUNC : H5F_ACC_EXCL, dst_fcpl, H5P_DEFAULT)) <
        0) {
        if (!force)
            error_msg("output file '%s' already exists; use --force to overwrite\n", output_filename);
        goto done;
    }

    remove_output = true;

    if (H5Ocopy(src_file, "/", dst_file, H5SOTO_TMP_ROOT_NAME, H5P_DEFAULT, H5P_DEFAULT) < 0)
        goto done;
    if ((tmp_group_id = H5Gopen2(dst_file, H5SOTO_TMP_ROOT_NAME, H5P_DEFAULT)) < 0)
        goto done;
    if (h5soto_copy_attributes(tmp_group_id, dst_file) < 0)
        goto done;
    if (h5soto_collect_child_names(tmp_group_id, &child_names) < 0)
        goto done;

    for (size_t i = 0; i < child_names.nitems; i++)
        if (H5Lmove(tmp_group_id, child_names.items[i], dst_file, child_names.items[i], H5P_DEFAULT,
                    H5P_DEFAULT) < 0)
            goto done;

    if (H5Ldelete(dst_file, H5SOTO_TMP_ROOT_NAME, H5P_DEFAULT) < 0)
        goto done;

    ret_value     = SUCCEED;
    remove_output = false;

done:
    h5soto_string_list_free(&child_names);

    if (tmp_group_id >= 0)
        H5Gclose(tmp_group_id);
    if (dst_fcpl >= 0)
        H5Pclose(dst_fcpl);
    if (dst_file >= 0)
        H5Fclose(dst_file);
    if (src_file >= 0)
        H5Fclose(src_file);

    if (remove_output)
        remove(output_filename);

    return ret_value;
}

static herr_t
h5soto_print_revision_listing(const char *filename, uint64_t revision, uint64_t latest_revision)
{
    h5soto_inventory_t inventory;
    h5soto_inventory_init(&inventory);

    if (h5soto_build_inventory_for_revision(filename, revision, &inventory) < 0) {
        h5soto_inventory_free(&inventory);
        return FAIL;
    }

    printf("Objects in revision %" PRIu64 " (of %" PRIu64 "):\n", revision, latest_revision + 1);
    for (size_t i = 0; i < inventory.nentries; i++)
        printf("  %s (%s)\n", inventory.entries[i].path, h5soto_kind_name(inventory.entries[i].kind));

    h5soto_inventory_free(&inventory);
    return SUCCEED;
}

static herr_t
h5soto_print_verbose_summary(const char *filename, uint64_t latest_revision, uint64_t from_revision,
                             uint64_t to_revision)
{
    h5soto_inventory_t prev_inventory;
    h5soto_inventory_t curr_inventory;
    herr_t             ret_value = FAIL;

    h5soto_inventory_init(&prev_inventory);
    h5soto_inventory_init(&curr_inventory);

    if (h5soto_build_inventory_for_revision(filename, from_revision, &prev_inventory) < 0)
        goto done;

    printf("File versions: %" PRIu64 " (revisions 0-%" PRIu64 ")\n", latest_revision + 1, latest_revision);

    /* Compare each revision against its immediate predecessor so the summary
     * reports the stepwise changes across the requested range. */
    for (uint64_t revision = from_revision + 1; revision <= to_revision; revision++) {
        size_t            i = 0;
        size_t            j = 0;
        h5soto_ref_list_t added;
        h5soto_ref_list_t removed;
        h5soto_ref_list_t modified;

        h5soto_ref_list_init(&added);
        h5soto_ref_list_init(&removed);
        h5soto_ref_list_init(&modified);

        if (h5soto_build_inventory_for_revision(filename, revision, &curr_inventory) < 0) {
            h5soto_ref_list_free(&modified);
            h5soto_ref_list_free(&removed);
            h5soto_ref_list_free(&added);
            goto done;
        }

        /* Both inventories are kept sorted by object path, so a single merge
         * pass can classify additions, removals, and same-path updates. */
        while (i < prev_inventory.nentries || j < curr_inventory.nentries) {
            if (i == prev_inventory.nentries) {
                if (h5soto_ref_list_append(&added, &curr_inventory.entries[j++]) < 0)
                    goto diff_error;
                continue;
            }

            if (j == curr_inventory.nentries) {
                if (h5soto_ref_list_append(&removed, &prev_inventory.entries[i++]) < 0)
                    goto diff_error;
                continue;
            }

            int cmp = strcmp(prev_inventory.entries[i].path, curr_inventory.entries[j].path);

            if (cmp < 0) {
                if (h5soto_ref_list_append(&removed, &prev_inventory.entries[i++]) < 0)
                    goto diff_error;
            }
            else if (cmp > 0) {
                if (h5soto_ref_list_append(&added, &curr_inventory.entries[j++]) < 0)
                    goto diff_error;
            }
            else {
                /* Matching paths represent the same object identity across
                 * revisions; a kind or signature change makes it "modified". */
                if (prev_inventory.entries[i].kind != curr_inventory.entries[j].kind ||
                    prev_inventory.entries[i].signature != curr_inventory.entries[j].signature) {
                    if (h5soto_ref_list_append(&modified, &curr_inventory.entries[j]) < 0)
                        goto diff_error;
                }
                i++;
                j++;
            }
        }

        printf("Revision %" PRIu64 " -> %" PRIu64 ": %zu added, %zu removed, %zu modified\n", revision - 1,
               revision, added.nitems, removed.nitems, modified.nitems);

        if (0 == added.nitems && 0 == removed.nitems && 0 == modified.nitems)
            printf("  no object-level changes detected\n");

        for (size_t idx = 0; idx < added.nitems; idx++)
            printf("  added: %s (%s)\n", added.items[idx]->path, h5soto_kind_name(added.items[idx]->kind));
        for (size_t idx = 0; idx < removed.nitems; idx++)
            printf("  removed: %s (%s)\n", removed.items[idx]->path,
                   h5soto_kind_name(removed.items[idx]->kind));
        for (size_t idx = 0; idx < modified.nitems; idx++)
            printf("  modified: %s (%s)\n", modified.items[idx]->path,
                   h5soto_kind_name(modified.items[idx]->kind));

        h5soto_ref_list_free(&modified);
        h5soto_ref_list_free(&removed);
        h5soto_ref_list_free(&added);

        /* Reuse the current inventory as the baseline for the next revision
         * comparison and reinitialize curr_inventory for the next load. */
        h5soto_inventory_free(&prev_inventory);
        prev_inventory = curr_inventory;
        h5soto_inventory_init(&curr_inventory);
        continue;

diff_error:
        h5soto_ref_list_free(&modified);
        h5soto_ref_list_free(&removed);
        h5soto_ref_list_free(&added);
        goto done;
    }

    ret_value = SUCCEED;

done:
    h5soto_inventory_free(&curr_inventory);
    h5soto_inventory_free(&prev_inventory);
    return ret_value;
}

static int
h5soto_parse_revision_arg(const char *arg, uint64_t *revision_out)
{
    char              *endptr = NULL;
    unsigned long long value  = 0;

    if (!arg || !*arg || !revision_out)
        return -1;

    errno = 0;
    value = strtoull(arg, &endptr, 10);
    if (errno != 0 || endptr == arg || *endptr != '\0')
        return -1;

    *revision_out = (uint64_t)value;
    return 0;
}

/* Parses a revision number or the literal string "latest".
 * "latest" stores H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST; caller must
 * resolve it to the actual latest revision number before use. */
static int
h5soto_parse_revision_or_latest(const char *arg, uint64_t *revision_out)
{
    if (!arg || !revision_out)
        return -1;
    if (!strcmp(arg, "latest")) {
        *revision_out = H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST;
        return 0;
    }
    return h5soto_parse_revision_arg(arg, revision_out);
}

static int
h5soto_parse_command_line(int argc, const char *const *argv, h5soto_options_t *options)
{
    int opt = 0;

    if (!options)
        return -1;

    /* Start from a fully disabled/default configuration and let option parsing
     * set only the fields explicitly requested on the command line. */
    memset(options, 0, sizeof(*options));

    if (argc == 1) {
        usage(h5tools_getprogname());
        h5tools_setstatus(EXIT_FAILURE);
        return -1;
    }

    /* Parse flags and their raw argument payloads first; cross-option
     * consistency checks happen after the full command line is known. */
    while ((opt = H5_get_option(argc, argv, s_opts, l_opts)) != EOF) {
        switch ((char)opt) {
            case 'h':
                /* Positive return means "handled locally, main should exit
                 * without treating this as an error." */
                usage(h5tools_getprogname());
                h5tools_setstatus(EXIT_SUCCESS);
                return 1;

            case 'V':
                print_version(h5tools_getprogname());
                h5tools_setstatus(EXIT_SUCCESS);
                return 1;

            case 'm':
                options->materialize = true;
                if (h5soto_parse_revision_or_latest(H5_optarg, &options->materialize_revision) < 0) {
                    error_msg("invalid revision for --materialize: '%s'\n", H5_optarg);
                    return -1;
                }
                break;

            case 'o':
                options->output_filename = H5_optarg;
                break;

            case 'v':
                options->verbose = true;
                break;

            case 'E':
                options->enable_error_stack = true;
                break;

            case 'f':
                options->force = true;
                break;

            case 'l':
                options->list = true;
                if (h5soto_parse_revision_or_latest(H5_optarg, &options->list_revision) < 0) {
                    error_msg("invalid revision for --list: '%s'\n", H5_optarg);
                    return -1;
                }
                break;

            case '1':
                options->has_from = true;
                if (h5soto_parse_revision_arg(H5_optarg, &options->from_revision) < 0) {
                    error_msg("invalid revision for --from: '%s'\n", H5_optarg);
                    return -1;
                }
                break;

            case '2':
                options->has_to = true;
                if (h5soto_parse_revision_arg(H5_optarg, &options->to_revision) < 0) {
                    error_msg("invalid revision for --to: '%s'\n", H5_optarg);
                    return -1;
                }
                break;

            default:
                usage(h5tools_getprogname());
                h5tools_setstatus(EXIT_FAILURE);
                return -1;
        }
    }

    /* Exactly one non-option argument is expected: the onion-backed file to
     * inspect or materialize. */
    if (argc <= H5_optind) {
        error_msg("missing file name\n");
        usage(h5tools_getprogname());
        return -1;
    }

    options->filename = argv[H5_optind];

    if ((H5_optind + 1) < argc) {
        error_msg("too many arguments\n");
        usage(h5tools_getprogname());
        return -1;
    }

    /* Cross-option validation */
    {
        /* These modes describe mutually exclusive top-level actions; any
         * remaining options only refine the selected action. */
        int mode_count =
            (options->materialize ? 1 : 0) + (options->verbose ? 1 : 0) + (options->list ? 1 : 0);
        if (mode_count > 1) {
            error_msg("--materialize, --verbose, and --list are mutually exclusive\n");
            return -1;
        }
    }

    if (!options->materialize && options->output_filename) {
        error_msg("--output is only valid with --materialize\n");
        return -1;
    }

    if (!options->materialize && options->force) {
        error_msg("--force is only valid with --materialize\n");
        return -1;
    }

    if ((options->has_from || options->has_to) && !options->verbose) {
        error_msg("--from and --to are only valid with --verbose\n");
        return -1;
    }

    return 0;
}

static void
usage(const char *prog)
{
    fprintf(rawoutstream, "usage: %s [OPTIONS] file_name\n", prog);
    fprintf(rawoutstream, "\n");
    fprintf(rawoutstream, "HDF5 State of the Onion: Inspect an HDF5 file with accompanying onion history.\n");
    fprintf(rawoutstream, "\n");
    fprintf(rawoutstream, "  OPTIONS\n");
    fprintf(rawoutstream, "   -h, --help                   Print a usage message and exit\n");
    fprintf(rawoutstream, "   -V, --version                Print version number and exit\n");
    fprintf(rawoutstream,
            "   -v, --verbose                Print per-revision object-level change summaries\n");
    fprintf(rawoutstream,
            "       --from=REV               With --verbose: start diff summary at revision REV\n");
    fprintf(rawoutstream,
            "       --to=REV                 With --verbose: end diff summary at revision REV\n");
    fprintf(rawoutstream, "   -l, --list=REV               List all objects present in revision REV\n");
    fprintf(rawoutstream, "   -m, --materialize=REV        Create a standalone HDF5 file for revision REV\n");
    fprintf(rawoutstream, "   -o, --output=FILE            Output file path for --materialize\n");
    fprintf(rawoutstream,
            "   -f, --force                  Overwrite existing output file (--materialize only)\n");
    fprintf(rawoutstream,
            "       --enable-error-stack     Print messages from the HDF5 error stack as they occur\n");
    fprintf(rawoutstream, "\n");
    fprintf(rawoutstream, "REV may be a revision number (0-based) or the string \"latest\".\n");
    fprintf(rawoutstream, "\n");
    fprintf(rawoutstream, "Examples:\n");
    fprintf(rawoutstream, "  %s file.h5                                  # print revision count\n", prog);
    fprintf(rawoutstream, "  %s --verbose file.h5                        # show full change history\n", prog);
    fprintf(rawoutstream, "  %s --verbose --from=2 --to=4 file.h5        # show revisions 2-4 only\n", prog);
    fprintf(rawoutstream, "  %s --list=3 file.h5                         # list objects in revision 3\n",
            prog);
    fprintf(rawoutstream, "  %s --list=latest file.h5                    # list objects in latest revision\n",
            prog);
    fprintf(rawoutstream, "  %s --materialize=3 --output=rev3.h5 file.h5 # extract revision 3\n", prog);
    fprintf(rawoutstream,
            "  %s --materialize=latest --force file.h5     # extract latest, overwrite output\n", prog);
}

static void
leave(int ret)
{
    h5tools_close();
    exit(ret);
}

int
main(int argc, char *argv[])
{
    h5soto_options_t options;
    uint64_t         latest_revision = 0;
    char            *default_output  = NULL;
    int              parse_status    = 0;

    /* Initialize the shared h5tools runtime before any parsing or HDF5 work. */
    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);
    h5tools_init();

    /* Parsing can either fail (<0), fully handle the request already (>0, such
     * as --help/--version), or succeed normally (0). */
    parse_status = h5soto_parse_command_line(argc, (const char *const *)argv, &options);
    if (parse_status < 0) {
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }
    if (parse_status > 0)
        goto done;

    if (options.enable_error_stack)
        h5tools_error_report();

    /* Provide a clear error when the .onion sidecar is missing */
    if (!h5soto_onion_sidecar_exists(options.filename)) {
        error_msg("'%s.onion' not found; '%s' does not appear to have onion revision history\n",
                  options.filename, options.filename);
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }

    if (h5soto_get_latest_revision(options.filename, &latest_revision) < 0) {
        error_msg("unable to read onion revision information from '%s'\n", options.filename);
        h5tools_setstatus(EXIT_FAILURE);
        goto done;
    }

    /* Resolve any parsed "latest" sentinel once so the mode-specific paths can
     * work with concrete revision numbers. */
    if (options.materialize && options.materialize_revision == H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST)
        options.materialize_revision = latest_revision;
    if (options.list && options.list_revision == H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST)
        options.list_revision = latest_revision;

    /* Dispatch the single requested top-level action. Command-line validation
     * already guaranteed these modes are mutually exclusive. */
    if (options.materialize) {
        const char *output_filename = options.output_filename;

        if (options.materialize_revision > latest_revision) {
            error_msg("requested revision %" PRIu64 " is out of range; latest revision is %" PRIu64 "\n",
                      options.materialize_revision, latest_revision);
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }

        if (!output_filename) {
            /* Derive a stable default output name only when the caller did not
             * provide one explicitly. */
            if (NULL == (default_output =
                             h5soto_default_output_name(options.filename, options.materialize_revision))) {
                error_msg("unable to allocate a default output file name\n");
                h5tools_setstatus(EXIT_FAILURE);
                goto done;
            }
            output_filename = default_output;
        }

        if (!strcmp(output_filename, options.filename)) {
            error_msg("output file must differ from input file\n");
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }

        if (h5soto_materialize_revision(options.filename, options.materialize_revision, output_filename,
                                        options.force) < 0) {
            error_msg("failed to materialize revision %" PRIu64 " into '%s'\n", options.materialize_revision,
                      output_filename);
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }

        printf("Created %s from revision %" PRIu64 "\n", output_filename, options.materialize_revision);
    }
    else if (options.list) {
        if (options.list_revision > latest_revision) {
            error_msg("requested revision %" PRIu64 " is out of range; latest revision is %" PRIu64 "\n",
                      options.list_revision, latest_revision);
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }

        if (h5soto_print_revision_listing(options.filename, options.list_revision, latest_revision) < 0) {
            error_msg("failed to list objects for revision %" PRIu64 " of '%s'\n", options.list_revision,
                      options.filename);
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
    }
    else if (options.verbose) {
        uint64_t from_rev = options.has_from ? options.from_revision : 0;
        uint64_t to_rev   = options.has_to ? options.to_revision : latest_revision;

        /* Default the verbose range to the full history, then clamp it against
         * the discovered latest revision before diffing inventories. */
        if (from_rev > latest_revision) {
            error_msg("--from %" PRIu64 " exceeds latest revision %" PRIu64 "\n", from_rev, latest_revision);
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
        if (to_rev > latest_revision) {
            error_msg("--to %" PRIu64 " exceeds latest revision %" PRIu64 "\n", to_rev, latest_revision);
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
        if (from_rev > to_rev) {
            error_msg("--from %" PRIu64 " exceeds --to %" PRIu64 "\n", from_rev, to_rev);
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }

        if (h5soto_print_verbose_summary(options.filename, latest_revision, from_rev, to_rev) < 0) {
            error_msg("failed to build revision summary for '%s'\n", options.filename);
            h5tools_setstatus(EXIT_FAILURE);
            goto done;
        }
    }
    else
        printf("%" PRIu64 "\n", latest_revision + 1);

done:
    /* Funnel every exit through one path so transient allocations are released
     * before leave() shuts down h5tools and exits the process. */
    free(default_output);
    leave(h5tools_getstatus());
}
