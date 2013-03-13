
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "mpi.h"
#include "hdf5.h"

static herr_t
link_cb(hid_t gid, const char *name,
         const H5L_info_t *linfo, void *udata)
{
    printf ("LINK VISIT NAME = %s\n", name);
    return 0;
}

static herr_t
visit_cb(hid_t oid, const char *name,
         const H5O_info_t *oinfo, void *udata)
{
    printf ("Object VISIT NAME = %s\n", name);
    return 0;
}

int main(int argc, char **argv) {
        const char file_name[]="test_file.h5";
	const char group_name[]="/Group";
	const char dataset_name[]="Data";
	const char attr_name[]="ATTR";
	char get_name[20];
	char fullpath[500];
	hid_t file_id;
	hid_t gid1, gid2;
	hid_t dataspaceId;
        hid_t datasetId;
        hid_t acc_tpl;
        hid_t fapl;
        hid_t plist_id = -1;
        hid_t vol_id;
        hid_t int_id;
        hid_t attr;
        hid_t space;
	const unsigned int nelem=60;
	int *data = NULL, *r_data = NULL;
	unsigned int i = 0;
	hsize_t dims[1];
        hsize_t n = 1;
        ssize_t len;
        char name[25];
        int my_rank, my_size;
        H5A_info_t ainfo, ainfo1, ainfo2;
        H5G_info_t ginfo, ginfo1, ginfo2;
        H5O_info_t oinfo;
        hobj_ref_t oref;
        hdset_reg_ref_t dref;
        H5O_type_t obj_type;

	MPI_Init(&argc, &argv);
        H5VLeff_init(MPI_COMM_WORLD, MPI_INFO_NULL);

        MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
        MPI_Comm_size(MPI_COMM_WORLD, &my_size);
        printf("APP processes = %d, my rank is %d\n", my_size, my_rank);

        fapl = H5Pcreate (H5P_FILE_ACCESS);
        H5Pset_fapl_iod(fapl, MPI_COMM_WORLD, MPI_INFO_NULL);

        sprintf(fullpath,"%s/%s",group_name,dataset_name);
        data = malloc (sizeof(int)*nelem);
        r_data = malloc (sizeof(int)*nelem);

        for(i=0;i<nelem;++i) {
            r_data[i] = 0;
            data[i]=i;
        }

	file_id = H5Fcreate(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
        H5Fclose(file_id);
        printf("HERE DONE\n");
#if 0
        if(my_rank == 0) {
            printf ("Process %d creating File structure and writing raw data\n", my_rank);

            gid1 = H5Gcreate2(file_id, group_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            {
                unsigned intent;
                char temp_name[50];

                H5Fget_intent(file_id, &intent);
                printf("Intent %d   %d\n", intent, H5F_ACC_RDWR);

                H5Fget_name(gid1, temp_name, 50);
                printf("File name %s   %s\n", temp_name, file_name);

                /*
                if(H5Fmount(gid1, ".", file_id, H5P_DEFAULT) < 0)
                    printf("MOUNT FAILED\n");
                if(H5Funmount(gid1, ".") < 0)
                    printf("UNMOUNT FAILED\n");
                */
            }

            dims [0] = 60;
            dataspaceId = H5Screate_simple(1, dims, NULL);

            {
                hid_t dcpl;
                int value = 12;

                dcpl = H5Pcreate(H5P_DATASET_CREATE);
                H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &value);
                datasetId = H5Dcreate(file_id,fullpath,H5T_NATIVE_INT,dataspaceId,H5P_DEFAULT,dcpl,H5P_DEFAULT);
                H5Pclose(dcpl);
                H5Dread(datasetId, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, r_data);

                printf("FILL values");
                for(i=0;i<nelem;++i)
                    printf("%d ",r_data[i]);
                printf("\n");
            }
            H5Dwrite(datasetId, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
            H5Oset_comment(datasetId, "Comment_1");

#if 0
            if(H5Rcreate(&oref, file_id, fullpath, H5R_OBJECT, -1) < 0) {
                printf("ref create failed\n");
                exit(1);
            }
            H5Rget_obj_type2(datasetId, H5R_OBJECT, &oref, &obj_type);
            assert(H5O_TYPE_DATASET == obj_type);
            {
                hid_t dset;
                dset = H5Rdereference2(datasetId, H5P_DEFAULT, H5R_OBJECT, &oref);
                H5Dclose(dset);
            }
            H5Rcreate(&dref, file_id, fullpath, H5R_DATASET_REGION, dataspaceId);
            H5Rget_obj_type2(datasetId, H5R_DATASET_REGION, &dref, &obj_type);
            assert(H5O_TYPE_DATASET == obj_type);
            {
                hid_t dset, sid;
                int points;

                dset = H5Rdereference2(datasetId, H5P_DEFAULT, H5R_DATASET_REGION, &dref);

                sid = H5Rget_region(datasetId, H5R_DATASET_REGION, &dref);
                points = (int)H5Sget_select_npoints(sid);
                assert(points == 60);

                len = H5Rget_name(datasetId, H5R_DATASET_REGION, &dref, get_name, 12);
                printf("name = %s\n", get_name);

                H5Sclose(sid);
                H5Dclose(dset);
            }
#endif

            int_id = H5Tcopy(H5T_NATIVE_INT);
            H5Tcommit2(gid1, "int", int_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

            attr = H5Acreate2(datasetId, attr_name, int_id, dataspaceId, H5P_DEFAULT, H5P_DEFAULT);
            for(i=0;i<nelem;++i) {
                r_data[i] = 0;
                data[i]=(i+1)*2;
            }
            H5Awrite(attr, int_id, data);

            H5Fflush(file_id, H5F_SCOPE_LOCAL);

            memset(get_name, 0, 5);

            H5Aclose(attr);
            attr = H5Aopen(datasetId,attr_name,H5P_DEFAULT);
            H5Aclose(attr);
            H5Tclose(int_id);
            H5Sclose(dataspaceId);
            H5Dclose(datasetId);
            H5Gclose(gid1);
        }

        H5Fclose(file_id);

        if(H5Fis_accessible(file_name, fapl))
            printf("file IS accessible\n");
        else
            printf("file IS NOT accessible !!!\n");

	file_id = H5Fopen(file_name, H5F_ACC_RDWR, fapl);
        plist_id = H5Fget_access_plist(file_id);
        H5Pclose(plist_id);
        plist_id = H5Fget_create_plist(file_id);
        H5Pclose(plist_id);
        MPI_Barrier(MPI_COMM_WORLD);

        if((my_size > 1 && my_rank == 1) || (my_size == 1 && my_rank == 0)) {
            hid_t dset, dspace;

            for(i=0;i<nelem;++i) {
                data[i]=(i+1)*10;
            }
            dims[0] = 60;
            dspace = H5Screate_simple(1, dims, NULL); 
            dset = H5Dcreate(file_id,"/Group/Data2",H5T_NATIVE_INT,dspace,H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT);
            H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);

            H5Sclose(dspace);
            H5Dclose(dset);

            printf ("Process %d Reading File structure and reading raw data\n", my_rank);

            H5Gget_info_by_name(file_id, group_name, &ginfo1, H5P_DEFAULT);

            //gid1 = H5Gopen(file_id,group_name,H5P_DEFAULT);
            gid1 = H5Oopen(file_id,group_name,H5P_DEFAULT);

            H5Gget_info(gid1, &ginfo);
            assert(ginfo1.nlinks == ginfo.nlinks == 1);
            assert(ginfo1.storage_type == ginfo.storage_type == H5G_STORAGE_TYPE_COMPACT);
            assert(ginfo1.max_corder == ginfo.max_corder);
            assert(!ginfo1.mounted && !ginfo.mounted);

            plist_id = H5Gget_create_plist(gid1);
            H5Pclose(plist_id);

            //datasetId = H5Dopen(file_id,fullpath,H5P_DEFAULT);
            datasetId = H5Oopen(file_id,fullpath,H5P_DEFAULT);
            H5Oget_info(datasetId, &oinfo);

            H5Oget_comment(datasetId, get_name, 10);
            if(0 != strcmp(get_name, "Comment_1")) {
                printf("get comment failed\n");
                exit(0);
            }

            len = H5Aget_name_by_idx(file_id, fullpath, H5_INDEX_CRT_ORDER, H5_ITER_INC, 0, 
                                     get_name, 5, H5P_DEFAULT);
            if(0 != strcmp(get_name, attr_name)) {
                printf("attribute get name failed\n");
                exit(0);
            }
            if(H5Aexists(datasetId, "bla bla")) {
                printf("attribute exists\n");
                exit(0);
            }
            if(!H5Aexists_by_name(file_id, fullpath, attr_name, H5P_DEFAULT)) {
                printf("attribute does not exist\n");
                exit(0);
            }
            H5Aget_info_by_idx(file_id, fullpath, H5_INDEX_CRT_ORDER, H5_ITER_INC, 0, &ainfo1, H5P_DEFAULT);
            assert(ainfo1.data_size == nelem*sizeof(int));
            H5Aget_info_by_name(file_id, fullpath, attr_name, &ainfo2, H5P_DEFAULT);
            assert(ainfo2.data_size == nelem*sizeof(int));

            attr = H5Aopen(datasetId,attr_name,H5P_DEFAULT);
            H5Arename(datasetId, attr_name, "rena");
            len = H5Aget_name(attr, 0, NULL);
            assert(4 == len);
            len = H5Aget_name(attr, 4, get_name);
            printf("ATTR name = %s\n", get_name);
            if(!strcmp(get_name, attr_name)) {
                printf("attribute get name failed\n");
                exit(0);
            }
            H5Arename(datasetId, "rena",attr_name);
            H5Aget_info(attr, &ainfo);
            assert(ainfo.data_size == nelem*sizeof(int));

            H5Gget_info(gid1, &ginfo);
            H5Dread(datasetId, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, r_data);

            printf("Direct ");
            for(i=0;i<nelem;++i)
                printf("%d ",r_data[i]);
            printf("\n");

            dset = H5Dopen(file_id,"/Group/Data2",H5P_DEFAULT);
            H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, r_data);
            printf("Direct2: ");
            for(i=0;i<nelem;++i)
                printf("%d ",r_data[i]);
            printf("\n");
            H5Dclose(dset);

            //int_id = H5Topen(gid1, "int", H5P_DEFAULT);
            int_id = H5Oopen(gid1, "int", H5P_DEFAULT);

            printf("Attr: ");
            H5Aread(attr, int_id, r_data);
            for(i=0;i<nelem;++i)
                printf("%d ",r_data[i]);
            printf("\n");

            H5Tclose(int_id);
            H5Aclose(attr);
            H5Oclose(datasetId);


            H5Lcreate_soft("moved_group", file_id, "soft_name", H5P_DEFAULT, H5P_DEFAULT);
            H5Lcreate_hard(file_id, "Group", H5L_SAME_LOC, "Group_new", H5P_DEFAULT, H5P_DEFAULT);
            H5Lmove(file_id, "Group_new", file_id, "moved_group", H5P_DEFAULT, H5P_DEFAULT);

            if(H5Lexists(file_id, "Group_new", H5P_DEFAULT)) {
                printf("moved link exists\n");
                exit(0);
            }
            if(!H5Lexists(file_id, "moved_group", H5P_DEFAULT)) {
                printf("moved link does not exist\n");
                exit(0);
            }

            datasetId = H5Dopen(file_id,"/moved_group/Data",H5P_DEFAULT);
            for(i=0;i<nelem;++i)
                r_data[i] = 0;
            H5Dread(datasetId, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, r_data);
            printf("Hard moved link: ");
            for(i=0;i<nelem;++i)
                printf("%d ",r_data[i]);
            printf("\n");
            H5Dclose(datasetId);

            datasetId = H5Dopen(file_id,"/soft_name/Data",H5P_DEFAULT);
            for(i=0;i<nelem;++i)
                r_data[i] = 0;
            H5Dread(datasetId, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, r_data);
            printf("Soft link: ");
            for(i=0;i<nelem;++i)
                printf("%d ",r_data[i]);
            printf("\n");
            H5Dclose(datasetId);

            H5Ldelete(file_id, "soft_name", H5P_DEFAULT);

            gid2 = H5Gcreate2(gid1, "group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            H5Lcreate_hard(file_id, "Group", gid2, "Group_new2", H5P_DEFAULT, H5P_DEFAULT);

            H5Ocopy(gid1, "int", gid2, "int2", H5P_DEFAULT, H5P_DEFAULT);

            printf("LINK VISITING:\n");
            H5Lvisit(gid1, H5_INDEX_NAME, H5_ITER_NATIVE, link_cb, NULL);
            printf("LINK ITERATING:\n");
            H5Literate(gid1, H5_INDEX_NAME, H5_ITER_NATIVE, NULL, link_cb, NULL);
            printf("OBJECT VISITING:\n");
            H5Ovisit(gid1, H5_INDEX_NAME, H5_ITER_NATIVE, visit_cb, NULL);

            H5Oclose(gid2);
            H5Gclose(gid1);

            {
                hsize_t fsize;
                hssize_t fspace;
                ssize_t count;
                H5F_mem_t type;
                H5F_sect_info_t sect_info[50];
                H5F_info2_t finfo;
                H5AC_cache_config_t config;
                double hit_rate;
                hid_t file2;
                H5AC_cache_config_t my_cache_config = {
                    H5AC__CURR_CACHE_CONFIG_VERSION,
                    1,
                    0,
                    0,
                    "temp",
                    1,
                    0,
                    ( 2 * 2048 * 1024),
                    0.3f,
                    (64 * 1024 * 1024),
                    (4 * 1024 * 1024),
                    60000,
                    H5C_incr__threshold,
                    0.8f,
                    3.0f,
                    1,
                    (8 * 1024 * 1024),
                    H5C_flash_incr__add_space,
                    2.0f,
                    0.25f,
                    H5C_decr__age_out_with_threshold,
                    0.997f,
                    0.8f,
                    1,
                    (3 * 1024 * 1024),
                    3,
                    0,
                    0.2f,
                    (256 * 2048),
                    H5AC_METADATA_WRITE_STRATEGY__PROCESS_0_ONLY};

                file2 = H5Freopen(file_id);
                count = H5Fget_obj_count(file2, H5F_OBJ_ALL);
                printf("count = %d\n", count);
                H5Fclose(file2);

                //H5Fget_filesize(file_id, &fsize);
                //printf("file size is %llu\n", fsize);

                fspace = H5Fget_freespace(file_id);
                printf("file free space is %llu\n", fspace);

                count = H5Fget_free_sections(file_id, H5FD_MEM_SUPER, 0, NULL);
                printf("free sections is %d\n", count);

                count = H5Fget_free_sections(file_id, H5FD_MEM_SUPER, count, sect_info);
                printf("free sections is %d %llu %llu\n", count, sect_info[0].addr, sect_info[0].size);

                H5Fget_info(file_id, &finfo);
                printf("file info %llu %llu %llu\n", finfo.super.super_size, finfo.free.meta_size,
                       finfo.sohm.hdr_size);

                config.version = H5AC__CURR_CACHE_CONFIG_VERSION;
                H5Fget_mdc_config(file_id, &config);
                printf("mdc_config %d %d \n", config.initial_size, config.min_clean_fraction);
                H5Fset_mdc_config(file_id, &my_cache_config);
                H5Fget_mdc_config(file_id, &config);
                printf("mdc_config %d %d \n", config.initial_size, config.min_clean_fraction);

                H5Fget_mdc_hit_rate(file_id, &hit_rate);
                printf("hit rate %f \n", hit_rate);
                H5Freset_mdc_hit_rate_stats(file_id);
                H5Fget_mdc_hit_rate(file_id, &hit_rate);
                printf("hit rate %d \n", hit_rate);
                H5Fclear_elink_file_cache(file_id);

                count = H5Fget_obj_count(file_id, H5F_OBJ_ALL);
                printf("count = %d\n", count);
            }
        }

        H5Fclose(file_id);
        H5Pclose(fapl);
        H5Pclose(raw_fapl);

        for(i=0;i<nelem;++i) {
            r_data[i] = 0;
        }
        MPI_Barrier(MPI_COMM_WORLD);

        if(my_rank == 0) {
            printf ("Process %d Testing Access with SPLIT VFD\n", my_rank);
            fapl = H5Pcreate(H5P_FILE_ACCESS);
            H5Pset_fapl_split(fapl, ".md", H5P_DEFAULT, ".raw", H5P_DEFAULT);

            file_id = H5Fopen(file_name, H5F_ACC_RDWR, fapl);
            gid1 = H5Gopen(file_id, group_name, H5P_DEFAULT);
            datasetId = H5Dopen(file_id,fullpath,H5P_DEFAULT);
            attr = H5Aopen(datasetId,attr_name,H5P_DEFAULT);

            H5Dread(datasetId, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, r_data);
            for(i=0;i<nelem;++i)
                printf("%d ", r_data[i]);
            printf("\n");

            int_id = H5Topen(gid1, "int", H5P_DEFAULT);

            H5Aread(attr, int_id, r_data);
            for(i=0;i<nelem;++i)
                printf("%d ",r_data[i]);
            printf("\n");

            H5Aclose(attr);
            H5Dclose(datasetId);
            H5Tclose(int_id);
            H5Gclose(gid1);
            H5Fclose(file_id);
            H5Pclose(fapl);
        }
#endif
        free(data);
        free(r_data);

        MPI_Finalize();
	return 0;
}

