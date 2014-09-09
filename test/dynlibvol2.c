
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "H5PLextern.h"
#include "hdf5.h"

#define LOG 502

static herr_t H5VL_log_init(hid_t vipl_id);
static herr_t H5VL_log_term(hid_t vtpl_id);

/* Datatype callbacks */
static void *H5VL_log_datatype_commit(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t dxpl_id, void **req);
static void *H5VL_log_datatype_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t tapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_log_datatype_get(void *dt, H5VL_datatype_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_log_datatype_close(void *dt, hid_t dxpl_id, void **req);

/* Dataset callbacks */
static void *H5VL_log_dataset_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dcpl_id, hid_t dapl_id, hid_t dxpl_id, void **req);
static void *H5VL_log_dataset_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_log_dataset_read(void *dset, hid_t mem_type_id, hid_t mem_space_id,
                                    hid_t file_space_id, hid_t plist_id, void *buf, void **req);
static herr_t H5VL_log_dataset_write(void *dset, hid_t mem_type_id, hid_t mem_space_id,
                                     hid_t file_space_id, hid_t plist_id, const void *buf, void **req);
static herr_t H5VL_log_dataset_close(void *dset, hid_t dxpl_id, void **req);

/* File callbacks */
static void *H5VL_log_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t dxpl_id, void **req);
static void *H5VL_log_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_log_file_get(void *file, H5VL_file_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
static herr_t H5VL_log_file_close(void *file, hid_t dxpl_id, void **req);

/* Group callbacks */
static void *H5VL_log_group_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req);
static herr_t H5VL_log_group_close(void *grp, hid_t dxpl_id, void **req);

/* Link callbacks */

/* Object callbacks */
static void *H5VL_log_object_open(void *obj, H5VL_loc_params_t loc_params, H5I_type_t *opened_type, hid_t dxpl_id, void **req);
static herr_t H5VL_log_object_specific(void *obj, H5VL_loc_params_t loc_params, H5VL_object_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);

hid_t native_plugin_id = -1;

static const H5VL_class_t H5VL_log2_g = {
    0,
    LOG,
    "log2",					/* name */
    H5VL_log_init,                              /* initialize */
    H5VL_log_term,                              /* terminate */
    sizeof(hid_t),
    NULL,
    NULL,
    {                                           /* attribute_cls */
        NULL, //H5VL_log_attr_create,                /* create */
        NULL, //H5VL_log_attr_open,                  /* open */
        NULL, //H5VL_log_attr_read,                  /* read */
        NULL, //H5VL_log_attr_write,                 /* write */
        NULL, //H5VL_log_attr_get,                   /* get */
        NULL, //H5VL_log_attr_specific,              /* specific */
        NULL, //H5VL_log_attr_optional,              /* optional */
        NULL  //H5VL_log_attr_close                  /* close */
    },
    {                                           /* dataset_cls */
        H5VL_log_dataset_create,                    /* create */
        H5VL_log_dataset_open,                      /* open */
        H5VL_log_dataset_read,                      /* read */
        H5VL_log_dataset_write,                     /* write */
        NULL, //H5VL_log_dataset_get,               /* get */
        NULL, //H5VL_log_dataset_specific,          /* specific */
        NULL, //H5VL_log_dataset_optional,          /* optional */
        H5VL_log_dataset_close                      /* close */
    },
    {                                               /* datatype_cls */
        H5VL_log_datatype_commit,                   /* commit */
        H5VL_log_datatype_open,                     /* open */
        H5VL_log_datatype_get,                      /* get_size */
        NULL, //H5VL_log_datatype_specific,         /* specific */
        NULL, //H5VL_log_datatype_optional,         /* optional */
        H5VL_log_datatype_close                     /* close */
    },
    {                                           /* file_cls */
        H5VL_log_file_create,                      /* create */
        H5VL_log_file_open,                        /* open */
        H5VL_log_file_get,                         /* get */
        NULL, //H5VL_log_file_specific,            /* specific */
        NULL, //H5VL_log_file_optional,            /* optional */
        H5VL_log_file_close                        /* close */
    },
    {                                           /* group_cls */
        H5VL_log_group_create,                     /* create */
        NULL, //H5VL_log_group_open,               /* open */
        NULL, //H5VL_log_group_get,                /* get */
        NULL, //H5VL_log_group_specific,           /* specific */
        NULL, //H5VL_log_group_optional,           /* optional */
        H5VL_log_group_close                       /* close */
    },
    {                                           /* link_cls */
        NULL, //H5VL_log_link_create,                /* create */
        NULL, //H5VL_log_link_copy,                  /* copy */
        NULL, //H5VL_log_link_move,                  /* move */
        NULL, //H5VL_log_link_get,                   /* get */
        NULL, //H5VL_log_link_specific,              /* specific */
        NULL, //H5VL_log_link_optional,              /* optional */
    },
    {                                           /* object_cls */
        H5VL_log_object_open,                        /* open */
        NULL, //H5VL_log_object_copy,                /* copy */
        NULL, //H5VL_log_object_get,                 /* get */
        H5VL_log_object_specific,                    /* specific */
        NULL, //H5VL_log_object_optional,            /* optional */
    },
    {
        NULL,
        NULL,
        NULL
    },
    NULL
};

typedef struct H5VL_log_t {
    void   *under_object;
} H5VL_log_t;

H5PL_type_t   H5PLget_plugin_type(void) {return H5PL_TYPE_VOL;}
const void    *H5PLget_plugin_info(void) {return &H5VL_log2_g;}

static herr_t H5VL_log_init(hid_t vipl_id)
{
    printf("------- LOG INIT\n");
    return 0;
}

static herr_t H5VL_log_term(hid_t vtpl_id)
{
    printf("------- LOG TERM\n");
    return 0;
}

static void *
H5VL_log_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t dxpl_id, void **req)
{
    hid_t under_fapl;
    H5VL_log_t *file;

    native_plugin_id = H5VLget_plugin_id("native");
    assert(native_plugin_id > 0);

    file = (H5VL_log_t *)calloc(1, sizeof(H5VL_log_t));

    under_fapl = *((hid_t *)H5Pget_vol_info(fapl_id));
    file->under_object = H5VLfile_create(name, flags, fcpl_id, under_fapl, dxpl_id, req);

    printf("------- LOG H5Fcreate\n");
    return (void *)file;
}

static void *
H5VL_log_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t dxpl_id, void **req)
{
    hid_t under_fapl;
    H5VL_log_t *file;

    file = (H5VL_log_t *)calloc(1, sizeof(H5VL_log_t));

    under_fapl = *((hid_t *)H5Pget_vol_info(fapl_id));
    file->under_object = H5VLfile_open(name, flags, under_fapl, dxpl_id, req);

    printf("------- LOG H5Fopen\n");
    return (void *)file;
}

static herr_t 
H5VL_log_file_get(void *file, H5VL_file_get_t get_type, hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_log_t *f = (H5VL_log_t *)file;

    H5VLfile_get(f->under_object, native_plugin_id, get_type, dxpl_id, req, arguments);

    printf("------- LOG H5Fget %d\n", get_type);
    return 1;
}
static herr_t 
H5VL_log_file_close(void *file, hid_t dxpl_id, void **req)
{
    H5VL_log_t *f = (H5VL_log_t *)file;

    H5VLfile_close(f->under_object, native_plugin_id, dxpl_id, req);
    free(f);

    printf("------- LOG H5Fclose\n");
    return 1;
}

static void *
H5VL_log_group_create(void *obj, H5VL_loc_params_t loc_params, const char *name, 
                      hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req)
{
    H5VL_log_t *group;
    H5VL_log_t *o = (H5VL_log_t *)obj;

    group = (H5VL_log_t *)calloc(1, sizeof(H5VL_log_t));

    group->under_object = H5VLgroup_create(o->under_object, loc_params, native_plugin_id, name, gcpl_id,  gapl_id, dxpl_id, req);

    printf("------- LOG H5Gcreate\n");
    return (void *)group;
}

static herr_t 
H5VL_log_group_close(void *grp, hid_t dxpl_id, void **req)
{
    H5VL_log_t *g = (H5VL_log_t *)grp;

    H5VLgroup_close(g->under_object, native_plugin_id, dxpl_id, req);
    free(g);

    printf("------- LOG H5Gclose\n");
    return 1;
}

static void *
H5VL_log_datatype_commit(void *obj, H5VL_loc_params_t loc_params, const char *name, 
                         hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t dxpl_id, void **req)
{
    H5VL_log_t *dt;
    H5VL_log_t *o = (H5VL_log_t *)obj;

    dt = (H5VL_log_t *)calloc(1, sizeof(H5VL_log_t));

    dt->under_object = H5VLdatatype_commit(o->under_object, loc_params, native_plugin_id, name, 
                                           type_id, lcpl_id, tcpl_id, tapl_id, dxpl_id, req);

    printf("------- LOG H5Tcommit\n");
    return dt;
}
static void *
H5VL_log_datatype_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t tapl_id, hid_t dxpl_id, void **req)
{
    H5VL_log_t *dt;
    H5VL_log_t *o = (H5VL_log_t *)obj;  

    dt = (H5VL_log_t *)calloc(1, sizeof(H5VL_log_t));

    dt->under_object = H5VLdatatype_open(o->under_object, loc_params, native_plugin_id, name, tapl_id, dxpl_id, req);

    printf("------- LOG H5Topen\n");
    return (void *)dt;
}

static herr_t 
H5VL_log_datatype_get(void *dt, H5VL_datatype_get_t get_type, hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_log_t *o = (H5VL_log_t *)dt;
    herr_t ret_value;

    ret_value = H5VLdatatype_get(o->under_object, native_plugin_id, get_type, dxpl_id, req, arguments);

    printf("------- LOG datatype get\n");
    return ret_value;
}

static herr_t 
H5VL_log_datatype_close(void *dt, hid_t dxpl_id, void **req)
{
    H5VL_log_t *type = (H5VL_log_t *)dt;

    assert(type->under_object);

    H5VLdatatype_close(type->under_object, native_plugin_id, dxpl_id, req);
    free(type);

    printf("------- LOG H5Tclose\n");
    return 1;
}

static void *
H5VL_log_object_open(void *obj, H5VL_loc_params_t loc_params, H5I_type_t *opened_type, hid_t dxpl_id, void **req)
{
    H5VL_log_t *new_obj;
    H5VL_log_t *o = (H5VL_log_t *)obj;

    new_obj = (H5VL_log_t *)calloc(1, sizeof(H5VL_log_t));
    
    new_obj->under_object = H5VLobject_open(o->under_object, loc_params, native_plugin_id, opened_type, dxpl_id, req);

    printf("------- LOG H5Oopen\n");
    return (void *)new_obj;
}

static herr_t 
H5VL_log_object_specific(void *obj, H5VL_loc_params_t loc_params, H5VL_object_specific_t specific_type, 
                         hid_t dxpl_id, void **req, va_list arguments)
{
    H5VL_log_t *o = (H5VL_log_t *)obj;

    H5VLobject_specific(o->under_object, loc_params, native_plugin_id, specific_type, dxpl_id, req, arguments);

    printf("------- LOG Object specific\n");
    return 1;
}

static void *
H5VL_log_dataset_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dcpl_id, hid_t dapl_id, hid_t dxpl_id, void **req) 
{
    H5VL_log_t *dset;
    H5VL_log_t *o = (H5VL_log_t *)obj;

    dset = (H5VL_log_t *)calloc(1, sizeof(H5VL_log_t));

    dset->under_object = H5VLdataset_create(o->under_object, loc_params, native_plugin_id, name, dcpl_id,  dapl_id, dxpl_id, req);

    printf("------- LOG H5Dcreate\n");
    return (void *)dset;
}

static void *
H5VL_log_dataset_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dapl_id, hid_t dxpl_id, void **req)
{
    H5VL_log_t *dset;
    H5VL_log_t *o = (H5VL_log_t *)obj;

    dset = (H5VL_log_t *)calloc(1, sizeof(H5VL_log_t));

    dset->under_object = H5VLdataset_open(o->under_object, loc_params, native_plugin_id, name, dapl_id, dxpl_id, req);

    printf("------- LOG H5Dopen\n");
    return (void *)dset;
}

static herr_t 
H5VL_log_dataset_read(void *dset, hid_t mem_type_id, hid_t mem_space_id,
                      hid_t file_space_id, hid_t plist_id, void *buf, void **req)
{
    H5VL_log_t *d = (H5VL_log_t *)dset;

    H5VLdataset_read(d->under_object, native_plugin_id, mem_type_id, mem_space_id, file_space_id, 
                     plist_id, buf, req);

    printf("------- LOG H5Dread\n");
    return 1;
}
static herr_t 
H5VL_log_dataset_write(void *dset, hid_t mem_type_id, hid_t mem_space_id,
                       hid_t file_space_id, hid_t plist_id, const void *buf, void **req)
{
    H5VL_log_t *d = (H5VL_log_t *)dset;

    H5VLdataset_write(d->under_object, native_plugin_id, mem_type_id, mem_space_id, file_space_id, 
                     plist_id, buf, req);

    printf("------- LOG H5Dwrite\n");
    return 1;
}
static herr_t 
H5VL_log_dataset_close(void *dset, hid_t dxpl_id, void **req)
{
    H5VL_log_t *d = (H5VL_log_t *)dset;

    H5VLdataset_close(d->under_object, native_plugin_id, dxpl_id, req);
    free(d);

    printf("------- LOG H5Dclose\n");
    return 1;
}
