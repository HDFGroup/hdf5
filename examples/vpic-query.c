#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#include "mpi.h"
#include "hdf5.h"

#define SHOW_PROGRESS 1

#define MAX_NAME 64

/* Hyperslab layout styles */
#define BYROW                1       /* divide into slabs of rows */
#define BYCOL                2       /* divide into blocks of columns */
#define BYLASTDIM            3       /* For 3D and higher, we get contiguous blocks */
#define ZROW                 4       /* same as BYCOL except process 0 gets 0 rows */
#define ZCOL                 5       /* same as BYCOL except process 0 gets 0 columns */


/* File_Access_type bits */
#define FACC_DEFAULT         0x0     /* default */
#define FACC_MPIO            0x1     /* MPIO */
#define FACC_SPLIT           0x2     /* Split File */

#define DXFER_COLLECTIVE_IO  0x1  /* Collective IO*/
#define DXFER_INDEPENDENT_IO 0x2 /* Independent IO collectively */

#ifndef FALSE
#define FALSE                0x0
#endif
#ifndef TRUE
#define TRUE                 0x1
#endif
#ifndef SUCCEED
#define SUCCEED              0
#endif

#define FUNC __FUNCTION__
/*
 * Print the current location on the standard output stream.
 */
#define AT()     printf ("   at %s:%d in %s()...\n",        \
        __FILE__, __LINE__, FUNC);

#define H5_FAILED()  {puts("*FAILED*");fflush(stdout);}
#define FAIL_STACK_ERROR {H5_FAILED(); AT(); H5Eprint2(H5E_DEFAULT, stdout); goto error;}

/* For debugging */
extern void H5Q_enable_visualize_query(void);


static char *default_temperature_dataset = "WeeklySST";
static char *link_name = "Pressure";
static unsigned plugin = H5X_PLUGIN_FASTBIT;
static int mpi_rank = 0, mpi_size = 1;


static void
display_usage(char *app)
{
    printf("Usage:  %s <hdf5-filename> [dataset-identifier]\n", app);
}

static void
slab_set(int mpi_rank, int mpi_size, int ndims, hsize_t dims[], hsize_t start[], hsize_t count[],
	 hsize_t stride[], hsize_t block[], int mode)
{
    int i, lastdim = ndims-1;
    switch (mode){
    case BYROW:
	/* Each process takes a slabs of rows. */
	block[0] = dims[0]/mpi_size;
	start[0] = mpi_rank*block[0];
        for(i=1; i < ndims; i++) {
            block[i]  = dims[i];
            stride[i] = block[i];
	    count[i]  = 1;
	    start[i]  = 0;
	}
	break;
    case BYCOL:
	/* Each process takes a block of columns. */
        for(i=0; i < ndims; i++) {
            if (i == 1) {
                block[1] = dims[1]/mpi_size;
                start[1] = mpi_rank * block[1];
            } 
            else {
                block[i]  = dims[i];
                start[i]  = 0;
            }
            stride[i] = block[i];
	    count[i]  = 1;
	}
	break;
    case BYLASTDIM:
        for(i=0; i < lastdim; i++) {
            block[i]  = dims[i];
            stride[i] = block[i];
	    count[i]  = 1;
	    start[i]  = 0;
	}
	block[lastdim]  = dims[lastdim]/mpi_size;
	stride[lastdim] = block[lastdim];
	count[lastdim]  = 1;
	start[lastdim]  = mpi_rank*block[lastdim];
        break;	
    case ZROW:
	/* Similar to BYROW except process 0 gets 0 row */
	/* Each process takes a slabs of rows. */
        block[0] = (mpi_rank ? dims[0]/mpi_size  : 0);
	start[0] = (mpi_rank ? mpi_rank*block[0] : 1);
        for(i=1; i < ndims; i++) {
            block[i]  = dims[i];
            stride[i] = block[i];
	    count[i]  = 1;
	    start[i]  = 0;
	}
	break;
    case ZCOL:
	/* Similar to BYCOL except process 0 gets 0 column */
	/* Each process takes a block of columns. */
        for(i=0; i < ndims; i++) {
            if (i == 1) {
                block[1] = (mpi_rank ? dims[1]/mpi_size : 0);
                start[1] = (mpi_rank ? mpi_rank * block[1] : 1);
            } 
            else {
                block[i]  = dims[i];
                start[i]  = 0;
            }
            stride[i] = block[i];
	    count[i]  = 1;
	}
	break;
    default:
	/* Unknown mode.  Set it to cover the whole dataset. */
	printf("unknown slab_set mode (%d)\n", mode);
        for(i=0; i < ndims; i++) {
            block[i]  = dims[i];
            stride[i] = block[i];
	    count[i]  = 1;
	    start[i]  = 0;
	}
	break;
    }
}

/*
 * Create the appropriate File access property list
 * Utility function from HDF parallel testing
 */
hid_t
create_faccess_plist(MPI_Comm comm, MPI_Info info, int l_facc_type)
{
    hid_t ret_pl = -1;
    herr_t ret;

    ret_pl = H5Pcreate (H5P_FILE_ACCESS);
    if (ret_pl < 1) {
      fprintf(stderr, "H5Pcreate(1) failed\n");
      goto error;
    }

    if (l_facc_type == FACC_DEFAULT)
	return (ret_pl);

    if (l_facc_type == FACC_MPIO){
	/* set Parallel access with communicator */
	ret = H5Pset_fapl_mpio(ret_pl, comm, info);
	if (ret < 0) {
            fprintf(stderr, "H5Pset_fapl_mpio failed\n");
            goto error;
	}
#if 0
        ret = H5Pset_all_coll_metadata_ops(ret_pl, TRUE);
	if (ret < 0) {
            fprintf(stderr, "H5Pset_all_coll_metadata_ops failed\n");
            goto error;
	}
        ret = H5Pset_coll_metadata_write(ret_pl, TRUE);
	if (ret < 0) {
            fprintf(stderr, "H5Pset_coll_metadata_write failed\n");
            goto error;
	}
#endif
	return(ret_pl);
    }

    if (l_facc_type == (FACC_MPIO | FACC_SPLIT)){
	hid_t mpio_pl;

	mpio_pl = H5Pcreate (H5P_FILE_ACCESS);
	if (mpio_pl < 0) {
            fprintf(stderr, "H5Pcreate(2) failed\n");
            goto error;
	}
	/* set Parallel access with communicator */
	ret = H5Pset_fapl_mpio(mpio_pl, comm, info);
	if (ret < 0) {
            fprintf(stderr, "H5Pset_fapl_mpio failed\n");
            goto error;
	}
	/* setup file access template */
	ret_pl = H5Pcreate (H5P_FILE_ACCESS);
	if (ret_pl < 0) {
            fprintf(stderr, "H5Pcreate(3) failed\n");
            goto error;
	}
	/* set Parallel access with communicator */
	ret = H5Pset_fapl_split(ret_pl, ".meta", mpio_pl, ".raw", mpio_pl);
	if (ret < 0) {
            fprintf(stderr, "H5Pset_fapl_split failed\n");
            goto error;
	}
	H5Pclose(mpio_pl);
	return(ret_pl);
    }
 error:
    /* unknown file access types */
    return (ret_pl);
}

/* Apply query on data element */
static herr_t
test_query_apply_elem(hid_t query, hbool_t *result, hid_t type_id, const void *value)
{
    H5Q_combine_op_t op_type;
    H5Qget_combine_op(query, &op_type);
    if (op_type == H5Q_SINGLETON) {
        H5Q_type_t query_type;

        if (H5Qget_type(query, &query_type) < 0) FAIL_STACK_ERROR;

        /* Query type should be H5Q_TYPE_DATA_ELEM */
        if (query_type != H5Q_TYPE_DATA_ELEM) FAIL_STACK_ERROR;
        if (H5Qapply_atom(query, result, type_id, value) < 0) FAIL_STACK_ERROR;
    } else {
        hbool_t sub_result1, sub_result2;
        hid_t sub_query1_id, sub_query2_id;

        if (H5Qget_components(query, &sub_query1_id, &sub_query2_id) < 0)
            FAIL_STACK_ERROR;
        if (test_query_apply_elem(sub_query1_id, &sub_result1, type_id, value) < 0)
            FAIL_STACK_ERROR;
        if (test_query_apply_elem(sub_query2_id, &sub_result2, type_id, value) < 0)
            FAIL_STACK_ERROR;

        *result = (op_type == H5Q_COMBINE_AND) ? sub_result1 && sub_result2 :
                sub_result1 || sub_result2;

        if (H5Qclose(sub_query1_id) < 0) FAIL_STACK_ERROR;
        if (H5Qclose(sub_query2_id) < 0) FAIL_STACK_ERROR;
    }

    return 0;

error:
    H5E_BEGIN_TRY {
        /* Nothing */
    } H5E_END_TRY;
    return -1;
}

static herr_t
test_query_read_selection(size_t file_count, const char *filenames[], hid_t *files, hid_t view, H5R_type_t rtype)
{
    hid_t refs = H5I_BADID, ref_type = H5I_BADID, ref_space = H5I_BADID;
    size_t n_refs, ref_size, ref_buf_size;
    void *ref_buf = NULL;
    href_t *ref_ptr = NULL;
    const char *ref_path = NULL;
    hid_t obj = H5I_BADID, type = H5I_BADID, space = H5I_BADID, mem_space = H5I_BADID;
    size_t n_elem, elem_size, buf_size;
    float *buf = NULL;
    unsigned int i;

    if (rtype == H5R_REGION)
        ref_path = H5Q_VIEW_REF_REG_NAME;
    else if (rtype == H5R_OBJECT)
        ref_path = H5Q_VIEW_REF_OBJ_NAME;
    else if (rtype == H5R_ATTR)
        ref_path = H5Q_VIEW_REF_ATTR_NAME;

    /* Get region references from view */
    if ((refs = H5Dopen(view, ref_path, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR;
    if ((ref_type = H5Dget_type(refs)) < 0) FAIL_STACK_ERROR;
    if ((ref_space = H5Dget_space(refs)) < 0) FAIL_STACK_ERROR;
    if (0 == (n_refs = (size_t) H5Sget_select_npoints(ref_space))) FAIL_STACK_ERROR;
        if (mpi_rank == 0)
           printf("Found %zu reference(s)\n", n_refs);
    if (0 == (ref_size = H5Tget_size(ref_type))) FAIL_STACK_ERROR;
//    printf("Reference type size: %zu\n", ref_size);

    /* Allocate buffer to hold data */
    ref_buf_size = n_refs * ref_size;
    if (NULL == (ref_buf = malloc(ref_buf_size))) FAIL_STACK_ERROR;

    if ((H5Dread(refs, ref_type, H5S_ALL, ref_space, H5P_DEFAULT, ref_buf)) < 0) FAIL_STACK_ERROR;

    /* Get dataset / space / type ID for the referenced dataset region */
    ref_ptr = (href_t *) ref_buf;
    for (i = 0; i < n_refs; i++) {
        char obj_path[MAX_NAME];
        char filename[MAX_NAME];
        hid_t loc = H5I_BADID;

        if (H5Rget_file_name(ref_ptr[i], filename, MAX_NAME) < 0) FAIL_STACK_ERROR;
        printf("Found reference from file: %s\n", filename);
        if (file_count > 1) {
            unsigned int j;
            for (j = 0; j < file_count; j++) {
                if (0 == strcmp(filename, filenames[j])) {
                    loc = files[j];
                    break;
                }
            }
        } else {
            if (0 != strcmp(filename, filenames[0])) FAIL_STACK_ERROR;
            loc = files[0];
        }
        if (H5Rget_obj_name(loc, ref_ptr[i], obj_path, MAX_NAME) < 0) FAIL_STACK_ERROR;
        printf("Found reference from object: %s\n", obj_path);
        if ((obj = H5Rget_object(loc, H5P_DEFAULT, ref_ptr[i])) < 0) FAIL_STACK_ERROR;

        if (rtype == H5R_REGION) {
            unsigned int j;
	    int line, k;
            int lastline;
            if ((space = H5Rget_region2(loc, ref_ptr[i])) < 0) FAIL_STACK_ERROR;
            if ((type = H5Dget_type(obj)) < 0) FAIL_STACK_ERROR;
            if (0 == (n_elem = (size_t) H5Sget_select_npoints(space))) FAIL_STACK_ERROR;
            if (0 == (elem_size = H5Tget_size(type))) FAIL_STACK_ERROR;

            /* Get name of dataset */
            printf("Region has %zu elements of size %zu\n", n_elem, elem_size);

            /* Allocate buffer to hold data */
            buf_size = n_elem * elem_size;
            if (NULL == (buf = (float *) malloc(buf_size))) FAIL_STACK_ERROR;

            if ((mem_space = H5Screate_simple(1, (hsize_t *) &n_elem, NULL)) < 0) FAIL_STACK_ERROR;

            if ((H5Dread(obj, type, mem_space, space, H5P_DEFAULT, buf)) < 0) FAIL_STACK_ERROR;

            printf("Elements found are:\n");
#if 1
            lastline = n_elem/10;
            for(line=0; line < lastline; line++) {
                for(k=0; k < 10; k++) printf(" %f", buf[line*10 + k]);
                puts("");
	    }

            for (j = line*10; j < n_elem; j++)
                printf("_%f", buf[j]);
            printf("\n");
#else            
            for (j = 0; j < n_elem; j++)
                printf("%f ", buf[j]);
            printf("\n");
#endif
            if (H5Sclose(mem_space) < 0) FAIL_STACK_ERROR;
            if (H5Sclose(space) < 0) FAIL_STACK_ERROR;
            if (H5Tclose(type) < 0) FAIL_STACK_ERROR;
            free(buf);
            buf = NULL;
        }
        if (rtype == H5R_ATTR) {
            char attr_name[MAX_NAME];

            if (H5Rget_attr_name(obj, ref_ptr[i], attr_name, MAX_NAME) < 0) FAIL_STACK_ERROR;
            printf("Attribute name: %s\n", attr_name);

        }
        if (H5Dclose(obj) < 0) FAIL_STACK_ERROR;
    }

    if ((H5Dref_reclaim(ref_type, ref_space, H5P_DEFAULT, ref_buf)) < 0) FAIL_STACK_ERROR;
    if (H5Sclose(ref_space) < 0) FAIL_STACK_ERROR;
    if (H5Tclose(ref_type) < 0) FAIL_STACK_ERROR;
    if (H5Dclose(refs) < 0) FAIL_STACK_ERROR;
    free(ref_buf);
    ref_buf = NULL;

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Sclose(mem_space);
        H5Dclose(obj);
        H5Sclose(space);
        H5Tclose(type);
        free(buf);
        H5Dclose(refs);
        H5Sclose(ref_space);
        H5Tclose(ref_type);
        free(ref_buf);
    } H5E_END_TRY;

    return -1;
}



/* Create query */
static hid_t
test_query_1(float value1)
{
    hid_t q1 = H5I_BADID;
    hid_t q2 = H5I_BADID;
    hid_t q3 = H5I_BADID;

    /* Create and combine a bunch of queries
     * Query is: (x > value1)
     */
    if ((q1 = H5Qcreate(H5Q_TYPE_DATA_ELEM, H5Q_MATCH_GREATER_THAN, H5T_NATIVE_FLOAT, &value1)) < 0) FAIL_STACK_ERROR;

    /* Select object */
    if ((q2 = H5Qcreate(H5Q_TYPE_LINK_NAME, H5Q_MATCH_EQUAL, link_name)) < 0) FAIL_STACK_ERROR;
    if ((q3 = H5Qcombine(q2, H5Q_COMBINE_AND, q1)) < 0) FAIL_STACK_ERROR;

    /* H5Q_enable_visualize_query(); */
    return q3;


error:
    H5E_BEGIN_TRY {
        if (q1 != H5I_BADID)
            H5Qclose(q1);
        if (q2 != H5I_BADID)
            H5Qclose(q2);
        if (q3 != H5I_BADID)
            H5Qclose(q3);
    } H5E_END_TRY;
    return -1;
}


static int
read_client_data(int grank, int gsize, char *filename, char *datasetID)
{
    int i, errors = 0;
    int ds_ndims = 0;
    int facc_type = FACC_MPIO;
    size_t type_size = 0;
    hsize_t *ds_dimensions = NULL;

    /* for hyperslab settings */
    hsize_t *start = NULL;
    hsize_t *count = NULL;
    hsize_t *stride = NULL;
    hsize_t *block = NULL;
    hsize_t total_elements = 0;
    void *data_array = NULL;

    hid_t acc_tpl = -1;
    hid_t fid = -1;
    hid_t grp = -1;
    hid_t dset = -1;
    hid_t dtype = -1;
    hid_t file_dataspace = -1;
    hid_t mem_dataspace = -1;
    hid_t fapl_id = -1;
    H5T_class_t type_class = H5T_NO_CLASS;

    MPI_Info mpi_info = MPI_INFO_NULL;
    MPI_Comm mpi_comm = MPI_COMM_WORLD;

    double t_start, t_end;
    char *group, *slash;

    H5open();

    if ((group = strchr(datasetID,'/')) != NULL) {
      group++;
      if ((slash = strchr(group,'/')) != NULL) {
	 *slash++ = 0;
	 datasetID = slash;
      }
    }
    

    acc_tpl = create_faccess_plist(mpi_comm, mpi_info, facc_type);
    /* Make the file access collective */
    if (acc_tpl >= 0) H5Pset_fapl_mpio(acc_tpl, mpi_comm, mpi_info );
    fid = H5Fopen(filename, H5F_ACC_RDONLY, acc_tpl);
    if (fid < 0) {
        fprintf(stderr, "Error opening filename: %s\n", filename);
	errors += 1;
	goto done;
    }
    /* Close the access property list */
    if (fid >= 0) H5Pclose(acc_tpl);
    acc_tpl = -1;

    if (group != NULL) {
      grp  = H5Gopen2(fid, group, H5P_DEFAULT);
      dset = H5Dopen2(grp, datasetID, H5P_DEFAULT);
      H5Gclose(grp);
      grp = -1;
    }
    else {
      dset = H5Dopen2(fid, datasetID, H5P_DEFAULT);
    }

    if (dset < 0) {
        fprintf(stderr, "Unable to opening dataset: %s\n", datasetID);
	errors += 1;
	goto done;
    }

    /* Determine the datatype used for this dataset */
    dtype = H5Dget_type(dset);
    if (dtype < 0) {
        fprintf(stderr, 
	"Unable to read the dataset metadata to determine the datatype used\n");
	errors += 1;
	goto done;
    }
    /* Datatype (Continued) */
    type_size  = H5Tget_size(dtype);
    type_class = H5Tget_class(dtype);
    file_dataspace = H5Dget_space(dset);
    if (file_dataspace < 0) {
        fprintf(stderr,
	"There was a problem reading the dataset metadata : %s\n", datasetID);
	errors += 1;
	goto done;
    }
    ds_ndims = H5Sget_simple_extent_ndims(file_dataspace);
    if (ds_ndims <= 0) {
        fprintf(stderr,
	"There was a problem reading the dataset dimensions (ndims = %d)\n", ds_ndims);
	errors += 1;
	goto done;
    }
    else {
        ds_dimensions = (hsize_t *)calloc(ds_ndims, sizeof(hsize_t));
	start = (hsize_t *)calloc(ds_ndims * 4, sizeof(hsize_t));
	if (start) {
            count  = &start[ds_ndims];
            block  = &count[ds_ndims];
	    stride = &block[ds_ndims];
	}
	H5Sget_simple_extent_dims(file_dataspace, ds_dimensions, NULL);
#if defined(SHOW_PROGRESS)
        if (grank == 0) {
            size_t dim;
            printf("Dataset dtype: %llx\n", dtype);
            printf("Dataset class: %x\n", type_class);
            printf("datatype size: %ld\n", type_size);
            printf("Dataset dimensions:\n");
            for (dim=0; dim < ds_ndims; dim++) {
                printf("\t%lld\n", ds_dimensions[dim]);
	    }
        }
#endif
    }
    if (gsize > 1) {
        MPI_Barrier(MPI_COMM_WORLD);
    }

    slab_set(grank, gsize, ds_ndims, ds_dimensions, start, count, stride, block, BYLASTDIM);
    if (H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block) < 0) {
        fprintf(stderr,
	"H5Sselect_hyperslab returned an error\n");
	errors += 1;
	goto done;
    }

    mem_dataspace = H5Screate_simple (ds_ndims, block, NULL);
    if (mem_dataspace < 0) {
        fprintf(stderr,
       	"There was a problem creating an in-memory dataspace\n");
	errors += 1;
	goto done;
    }

    total_elements = block[0];
    for (i=1; i< ds_ndims; i++) 
        total_elements *= block[i];

#if defined(SHOW_PROGRESS) && defined(_SHOW_ELEMENTS_PER_RANK)
    printf("[%d] total_elements this rank = %lld\n", grank, total_elements);
    MPI_Barrier(MPI_COMM_WORLD);
#endif 
    data_array = calloc(total_elements, type_size);
    if (data_array == NULL) {
        fprintf(stderr,
       	"There was a problem allocating memory to read the dataset\n");
	errors += 1;
	goto done;
    }
#if defined(SHOW_PROGRESS)
    if (grank == 0) {
        switch( type_class ) {
        case H5T_INTEGER:
            printf("integer class\n");
            break;
        case H5T_FLOAT:
            printf("float class\n");
            break;
        case H5T_TIME:
            break;
        case H5T_STRING:
            break;
      /* Unsupported for now */
        case H5T_BITFIELD:
        case H5T_OPAQUE:
        case H5T_COMPOUND:
        case H5T_REFERENCE:
        case H5T_ENUM:
        case H5T_VLEN:
        case H5T_ARRAY:
        default:
            break;
        }
    }
#endif    

    /* INTEGER CLASS, type_size == 2 */
    t_start = MPI_Wtime();
    if ( H5Dread(dset, dtype, mem_dataspace, file_dataspace,
		 H5P_DEFAULT, data_array) < 0) {
        fprintf(stderr,
       	"There was a problem reading the dataset\n");
	errors += 1;
	goto done;
    }
    t_end = MPI_Wtime();
#if defined(SHOW_PROGRESS)
    if (grank == 0) {
        float *show_data = (float *)data_array;
        printf("H5Dread dataset took %f seconds to read %lld elements/rank\n", t_end - t_start, total_elements);
	printf("The first 10 elements of the dataset are shown below.  Please confirm the data using h5dump\n");
	for(i=0; i<10; i++) {
	  printf(" %6.5f", show_data[i]);
	}
	puts("");
    }
    MPI_Barrier(MPI_COMM_WORLD);
#endif

 done:

    if (data_array != NULL) free(data_array);
    if (start != NULL) free(start);
    if (ds_dimensions != NULL) free(ds_dimensions);
    if (mem_dataspace >= 0) H5Sclose(mem_dataspace);
    if (file_dataspace >= 0) H5Sclose(file_dataspace);
    if (grp >= 0) H5Gclose(grp);
    if (dset >= 0) H5Dclose(dset);
    if (fid >= 0) H5Fclose(fid);

    H5close();
    return errors;
}

static herr_t
test_create_index(hid_t fid, const char *datasetID)
{
    hid_t dset = H5I_BADID;
    hid_t grp = H5I_BADID;
    hid_t dcpl = H5P_DEFAULT;
    
    char *group, *slash;

#if 0
    /* Add indexing information */
    /* Should this be: H5P_INDEX_CREATE or H5P_DATASET_ACCESS ? */
    if ((dcpl = H5Pcreate(H5P_INDEX_CREATE)) < 0) FAIL_STACK_ERROR;
    if (H5Pset_index_plugin(dcpl, plugin) < 0) FAIL_STACK_ERROR;
#endif
    if ((group = strchr(datasetID,'/')) != NULL) {
      group++;
      if ((slash = strchr(group,'/')) != NULL) {
	 *slash++ = 0;
	 datasetID = slash;
      }
    }
    
    if (group != NULL) {
      if ((grp  = H5Gopen2(fid, group, H5P_DEFAULT)) < 0)
	  FAIL_STACK_ERROR;
      dset = H5Dopen2(grp, datasetID, H5P_DEFAULT);
      H5Gclose(grp);
    }
    else {
      dset = H5Dopen2(fid, datasetID, H5P_DEFAULT);
    }

    if (dset < 0) FAIL_STACK_ERROR;

    /* Add indexing information */
    if (H5Xcreate(dset, plugin, H5P_DEFAULT) < 0) 

    if (H5Dclose(dset) < 0) FAIL_STACK_ERROR;
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
    } H5E_END_TRY;
    return -1;
}


int
main(int argc, char **argv)
{
    int mpi_status = MPI_SUCCESS;
    float limit = 1.7;
    char *temp, *dataset = NULL, *filename = NULL;
    if ((MPI_Init(&argc, &argv) != MPI_SUCCESS) ||
	(MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank) != MPI_SUCCESS) ||
	(MPI_Comm_size(MPI_COMM_WORLD, &mpi_size) != MPI_SUCCESS)) {
        printf("MPI_Init failure, aborting!\n");
        return -1;
    }

    if (argc > 1) {
        temp = strdup(argv[1]);
	if (access(temp, (F_OK|R_OK)) == 0)
            filename = temp;
    }
    if (filename == NULL) {
        if (temp) {
	    printf("ERROR: Unable to access file - %s\n", temp);
	    free(temp);
	}
	else {
	    if (mpi_rank == 0) display_usage(argv[0]);
	}
	goto finalize;
    }
    if (argc > 2) {
        dataset = strdup(argv[2]);
    }

    if (argc > 3) {
        link_name = strdup(argv[3]);
    }

    if (argc > 4) {
        limit = atof(argv[4]);
    }

    if (H5open() != SUCCEED) {
    // if (H5_init_library() != SUCCESS) {
        printf("Error! Unable to initialize the HDF5 library\n");
	return -1;
    }

    if (argc > 3) {
        hid_t fapl = H5I_BADID;
        hid_t query = H5I_BADID;
	hid_t sub_query1_id, sub_query2_id;	
	hid_t fid = H5I_BADID;
        hid_t view = H5I_BADID;
        hbool_t sub_result1, sub_result2;
        unsigned result = 0;


        H5Xinitialize_parallel_query();
	if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0) {
	}
	
        if (H5Pset_fapl_mpio(fapl, MPI_COMM_WORLD, MPI_INFO_NULL) < 0) {
	}

	if ((fid = H5Fopen(filename,H5F_ACC_RDWR,fapl)) < 0) {
	}
	
	if (H5Pclose(fapl) < 0) {
	}

	if (test_create_index(fid, dataset) < 0) {
	}

	query = test_query_1(limit);

        if ((view = H5Qapply(fid, query, &result, H5P_DEFAULT)) < 0) {
            printf("H5Qapply: FAILED\n");
	}

        if (test_query_read_selection(1, &filename, &fid, view, H5R_REGION) < 0) {
	}

	H5Gclose(view);
	H5Fclose(fid);
    }
    else {
        read_client_data(mpi_rank, mpi_size, filename, dataset);
    }

finalize:

    H5close();
    
    // H5_term_library();
    MPI_Finalize();
    return 0;
       
}
