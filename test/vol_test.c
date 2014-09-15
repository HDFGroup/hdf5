
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "hdf5.h"

hid_t native_plugin_id = -1;

static herr_t
visit_cb(hid_t oid, const char *name,
         const H5O_info_t *oinfo, void *udata)
{
    ssize_t len;
    char n[25];

    if(H5Iget_type(oid) == H5I_GROUP) {
        len = H5VLget_plugin_name(oid, n, 50);
        printf ("Visiting GROUP VOL name = %s  %zd\n", n, len);
    }
    if(H5Iget_type(oid) == H5I_DATASET) 
        printf("visiting dataset\n");
    if(H5Iget_type(oid) == H5I_DATATYPE) 
        printf("visiting datatype\n");

    return 1;
}

int main(int argc, char **argv) {
    const char group_name[]="/Group";
    const char dataset_name[]="Data";
    char fullpath[500];
    hid_t file_id;
    hid_t group_id;
    hid_t dataspaceId;
    hid_t datasetId;
    hid_t acc_tpl;
    hid_t under_fapl;
    hid_t vol_id, vol_id2;
    hid_t int_id;
    hid_t space;
    const unsigned int nelem=60;
    int *data;
    int n;
    unsigned int i;
    hsize_t dims[1];
    ssize_t len;
    char name[25];
    static hsize_t      ds_size[2] = {10, 20};

    for(n=1 ; n<3 ; n++) {
        char pl_name[10];
        char file_name[50];

        sprintf(pl_name, "log%d", n);
        sprintf(file_name, "log_file%d.h5", n);

        vol_id = H5VLregister_by_name (pl_name);
        assert(vol_id > 0);
        assert(H5VLis_registered(pl_name) == 1);

        under_fapl = H5Pcreate (H5P_FILE_ACCESS);
        H5Pset_fapl_native(under_fapl);
        assert(H5VLis_registered("native") == 1);

        vol_id2 = H5VLget_plugin_id(pl_name);
        H5VLinitialize(vol_id2, H5P_DEFAULT);
        H5VLclose(vol_id2);

        native_plugin_id = H5VLget_plugin_id("native");
        assert(native_plugin_id > 0);

        acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
        H5Pset_vol(acc_tpl, vol_id, &under_fapl);

        file_id = H5Fcreate(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, acc_tpl);
        len = H5VLget_plugin_name(file_id, name, 25);
        printf ("FILE VOL name = %s  %zd\n", name, len);

        group_id = H5Gcreate2(file_id, group_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        len = H5VLget_plugin_name(group_id, name, 50);
        printf ("GROUP VOL name = %s  %zd\n", name, len);

        int_id = H5Tcopy(H5T_NATIVE_INT);
        H5Tcommit2(file_id, "int", int_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        len = H5VLget_plugin_name(int_id, name, 50);
        printf ("DT COMMIT name = %s  %zd\n", name, len);
        H5Tclose(int_id);

        int_id = H5Topen2(file_id, "int", H5P_DEFAULT);
        len = H5VLget_plugin_name(int_id, name, 50);
        printf ("DT OPEN name = %s  %zd\n", name, len);
        H5Tclose(int_id);

        int_id = H5Oopen(file_id,"int",H5P_DEFAULT);
        len = H5VLget_plugin_name(int_id, name, 50);
        printf ("DT OOPEN name = %s  %zd\n", name, len);

        len = H5Fget_name(file_id, name, 50);
        printf("name = %zd  %s\n", len, name);

        data = (int *)malloc (sizeof(int)*nelem);
        for(i=0;i<nelem;++i)
            data[i]=i;

        dims [0] = 60;
        dataspaceId = H5Screate_simple(1, dims, NULL); 
        space = H5Screate_simple (2, ds_size, ds_size);

        sprintf(fullpath,"%s/%s",group_name,dataset_name);
        datasetId = H5Dcreate2(file_id,fullpath,H5T_NATIVE_INT,dataspaceId,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
        H5Sclose(dataspaceId);

        len = H5VLget_plugin_name(datasetId, name, 50);
        printf ("DSET name = %s  %zd\n", name, len);

        H5Dwrite(datasetId, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
        H5Dclose(datasetId);

        H5Ovisit(file_id, H5_INDEX_NAME, H5_ITER_NATIVE, visit_cb, NULL);

        free (data);
        H5Oclose(int_id);
        H5Sclose (space);
        H5Gclose(group_id);

        H5Fclose(file_id);
        H5Pclose(acc_tpl);
        H5Pclose(under_fapl);

        H5VLclose(native_plugin_id);
        H5VLterminate(vol_id, H5P_DEFAULT);
        H5VLunregister (vol_id);
        assert(H5VLis_registered(pl_name) == 0);
    }
    return 0;
}
