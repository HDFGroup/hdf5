/* -*- c-file-style: "stroustrup" -*- */

#include "hdf5.h"

#include <stdio.h>
#include <stdlib.h>

//! <!-- [H5Dstruct_chunk_iter_cb] -->
int
struct_chunk_cb(const hsize_t *offset, H5D_struct_chunk_info_t *chunk_info, haddr_t *addr,
                hsize_t *chunk_size, void *op_data)
{
    // Print out info for each structured chunk
    printf("offset[0] = %" PRIuHSIZE " offset[1] = %", PRIuHSIZE "\n", offset[0], offset[1]);
    printf("addr = %" PRIuHADDR " chunk_size = %" PRIuHSIZE "\n", *addr, *chunk_size);
    printf("num_sections = %u section_size[0] = %u section_size[1] = %u\n", chunk_info->num_sections,
           chunk_info->section_size[0], chunk_info->section_size[1]);
    return EXIT_SUCCESS;
}
//! <!-- [H5Dstruct_chunk_iter_cb] -->

int
main(void)
{
    int ret_val = EXIT_SUCCESS;

    //! <!-- [get_defined] -->
    {
        __label__ fail_file, fail_space, fail_dcpl, fail_layout;
        __label__ fail_chunk, fail_dcreate, fail_dwrite, fail_defined;

        char    file_name[] = "sparse.h5";
        char    dset_name[] = "sparse_dset";
        hid_t   fid, lcpl, sid, sid1, did;
        hsize_t dim[1]       = {50};
        hsize_t chunk_dim[1] = {5}; /* Chunk size */
        int     wbuf[50];           /* Write buffer */

        // Create a file
        if ((fid = H5Fcreate(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }

        // Create a 1D dataspace
        if ((sid = H5Screate_simple(1, dim, NULL)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_space;
        }

        // Create a dataset creation property list
        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dcpl;
        }

        // Set to sparse chunked dataset
        if (H5Pset_struct_chunk(dcpl, 1, chunk_dim, H5D_SPARSE_CHUNK) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_chunk;
        }

        // Create an integer dataset with sparse chunk layout
        if ((did = H5Dcreate2(fid, dset_name, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) ==
            H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dcreate;
        }

        // Initialize sparse data
        memset(wbuf, 0, sizeof(wbuf));
        wbuf[12] = 12;
        wbuf[13] = 13;
        wbuf[14] = 14;
        wbuf[48] = 48;
        wbuf[49] = 49;

        // Write sparse data to the dataset
        if (H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_dwrite;
        }

        // Get dataspace with the defined elements in the dataset
        if ((sid1 = H5Dget_defined(did, H5S_ALL, H5P_DEFAULT)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_defined;
        }

        // There are 5 defined elements
        if ((npoints = H5Sget_select_npoints(sid1)) != 5)
            ret_val = EXIT_FAILURE;

        H5Sclose(sid1);

fail_defined:
fail_dwrite:
        H5Dclose(did);

fail_dcreate:
fail_layout:
fail_chunk:
        H5Pclose(dcpl);

fail_dcpl:
        H5Sclose(sid);

fail_space:
        H5Fclose(fid);

fail_file:;
    }
    //! <!-- [get_defined] -->

    //! <!-- [erase] -->
    {
        __label__ fail_file, fail_dset, fail_erase, fail_defined;
        hid_t    fid, sid, did;
        hssize_t npoints;

        // Open the file
        if ((fid = H5Fopen(file_name, H5F_ACC_RDWR, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }

        // Open the 1-d dataset with sparse chunk layout
        if ((did = H5Dopen2(fid, dset_name, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dset;
        }

        // Deletes defined elements from the dataset
        if (H5Derase(did, H5S_ALL, H5P_DEFAULT) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_erase;
        }

        // Get dataspace with the defined elements in the dataset
        if ((sid = H5Dget_defined(did, H5S_ALL, H5P_DEFAULT)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_defined;
        }

        // No defined elements
        if ((npoints = H5Sget_select_npoints(sid1)) != 0)
            ret_val = EXIT_FAILURE;

        H5Sclose(sid);

fail_defined:
fail_erase:
        H5Dclose(did);

        fail_dset;
        H5Fclose(fid);

fail_file:;
    }
    //! <!-- [erase] -->

    //! <!-- [direct_chunk_write] -->
    {
        __label__ fail_file, fail_space, fail_dcpl, fail_set_chunk, fail_set_layout;
        __label__ fail_dcreate, fail_direct_write, fail_dread, fail_shyper;
        __label__ fail_calloc0, fail_calloc1, fail_sencode0, fail_encode1;

        hid_t                   fid, sid, did, dcpl;
        char                    file_name[]   = "sparse.h5";
        char                    dset_name[]   = "sparse_direct_dset";
        hsize_t                 dims[2]       = {10, 10};
        hsize_t                 chunk_dims[2] = {5, 5};
        const hsize_t           start[2], count[2], block[2];
        int                     rbuf[10][10];
        size_t                  encode_size;
        hsize_t                 wr_offset[2] = {0, 0};
        H5D_struct_chunk_info_t wr_chk_info;
        uint16_t                wr_filter_mask[2] = {0, 0};
        size_t                  wr_section_size[2];
        void                   *wr_buf[2];
        unsigned char          *wr_buf0;
        int                    *wr_buf1;

        // Open the file
        if ((fid = H5Fopen(file_name, H5F_ACC_RDWR, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }

        // Create a 2D dataspace
        if ((sid = H5Screate_simple(2, dim, NULL)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_space;
        }

        // Create a dataset creation property list
        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dcpl;
        }

        // Set to sparse chunked dataset
        if (H5Pset_struct_chunk(dcpl, 1, chunk_dim, H5D_SPARSE_CHUNK) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_set_chunk;
        }

        // Create an integer 2-d dataset with sparse chunk layout
        if ((did = H5Dcreate2(fid, dset_name, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) ==
            H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dcreate;
        }

        // Select the 2 x 3 block in chunk index 0 for writing
        start[0] = 3;
        start[1] = 2;
        block[0] = 2;
        block[1] = 3;
        count[0] = count[1] = 1;
        if (H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, NULL, count, block) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_shyper;
        }

        // Get the encoded size for the selection
        if (H5Sencode2(sid, NULL, &encode_size, H5P_DEFAULT) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_sencode0;
        }

        // Set up the section size for section 0 and section 1
        wr_section_size[0] = encode_size;
        wr_section_size[1] = block[0] * block[1] * sizeof(int);

        // Allocate buffers for section 0 (encoded selection) and section 1 (data)
        if ((wr_buf0 = (unsigned char *)calloc((size_t)1, encode_size)) == NULL) {
            ret_val = EXIT_FAILURE;
            goto fail_calloc0;
        }
        if ((wr_buf1 = (int *)calloc((size_t)1, wr_section_size[1])) == NULL) {
            ret_val = EXIT_FAILURE;
            goto fail_calloc1;
        }

        // Encode selection into the buffer for section 0
        if (H5Sencode2(sid, wr_buf0, &encode_size, H5P_DEFAULT) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_sencode1;
        }

        // Set up data into the buffer for section 1
        wr_buf1[0] = 32;
        wr_buf1[1] = 33;
        wr_buf1[2] = 34;
        wr_buf1[3] = 42;
        wr_buf1[4] = 43;
        wr_buf1[5] = 44;

        // Set up the buffer for H5D_write_struct_chunk()
        wr_buf[0] = wr_buf0;
        wr_buf[1] = wr_buf1;

        // Set up chunk info
        wr_chk_info.type              = H5D_SPARSE_CHUNK;
        wr_chk_info.num_sections      = 2;
        wr_chk_info.filter_mask       = wr_filter_mask;
        wr_chk_info.section_size      = wr_section_size;
        wr_chk_info.section_orig_size = wr_section_size;

        /* Write the structured chunk at offset [0,0]: chunk index 0 */
        if (H5Dwrite_struct_chunk(did, dxpl, wr_offset, &wr_chk_info, wr_buf) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_direct_write;
        }

        /* Read the whole dataset */
        if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, rbuf) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_dread;
        }

        // Verify elements in rbuf is same as what is written via H5Dwrite_struct_chunk()
        if (rbuf[3][2] != wr_buf[1][0] || rbuf[3][3] != wr_buf[1][1] || rbuf[3][4] != wr_buf[1][2] ||
            rbuf[4][2] != wr_buf[1][3] || rbuf[4][3] != wr_buf[1][4] || rbuf[4][4] != wr_buf[1][5])
            ret_val = EXIT_FAILURE;

fail_direct_write:
fail_dread:
fail_sencode1:
        free(wr_buf1);

fail_calloc1:
        free(wr_buf0);

fail_calloc0:
fail_sencode0:
fail_shyper:
        H5Dclose(did);

fail_dcreate:
fail_set_layout:
fail_set_chunk:
        H5Pclose(dcpl);

fail_dcpl:
        H5Sclose(sid);

fail_space:
        H5Fclose(fid);

fail_file:;
    }
    //! <!-- [direct_chunk_write] -->

    //! <!-- [direct_chunk_read] -->
    {
        __label__ fail_file, fail_dopen, fail_dwrite, fail_direct_read, fail_dspace;
        __label__ fail_shyper, fail_calloc0, fail_calloc1, fail_sencode;

        hid_t                   fid, sid, did, dcpl;
        char                    file_name[] = "sparse.h5";
        char                    dset_name[] = "sparse_direct_dset";
        int                     wbuf[10][10];
        size_t                  encode_size;
        const hsize_t           start[2], count[2], block[2];
        hsize_t                 rd_offset[2] = {5, 5};
        H5D_struct_chunk_info_t rd_chk_info;
        uint16_t                rd_filter_mask[2] = {0, 0};
        size_t                  rd_section_size[2];
        void                   *rd_buf[2];
        unsigned char          *rd_buf0;
        int                    *rd_buf1;

        // Open the file
        if ((fid = H5Fopen(file_name, H5F_ACC_RDWR, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }

        // Open the 2-d dataset with sparse chunk layout
        if ((did = H5Dopen2(fid, dset_name, H5P_DEFAULT)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_dopen;
        } // Initialize sparse data

        memset(wbuf, 0, sizeof(wbuf));
        wbuf[7][6] = 76;
        wbuf[8][6] = 86;

        // Write sparse data to the dataset
        if (H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_dwrite;
        }

        // Retrieve the dataset's dataspace
        if ((sid = H5Dget_space(did)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_dspace;
        }

        // Select the 2 x 1 block in chunk index 3 for reading
        start[0] = 5;
        start[1] = 5;
        block[0] = 2;
        block[1] = 1;
        count[0] = count[1] = 1;
        if (H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, NULL, count, block) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_shyper;
        }

        // Get the encoded size for the selection
        if (H5Sencode2(sid, NULL, &encode_size, H5P_DEFAULT) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_sencode;
        }

        // Set up the section size for section 0 and section 1
        rd_section_size[0] = encode_size;
        rd_section_size[1] = block[0] * block[1] * sizeof(int);

        // Allocate buffers for section 0 (encoded selection) and section 1 (data)
        if ((rd_buf0 = (unsigned char *)calloc((size_t)1, encode_size)) == NULL) {
            ret_val = EXIT_FAILURE;
            goto fail_calloc0;
        }
        if ((rd_buf1 = (int *)calloc((size_t)1, rd_section_size[1])) == NULL) {
            ret_val = EXIT_FAILURE;
            goto fail_calloc1;
        }

        rd_buf[0] = rd_buf0;
        rd_buf[1] = rd_buf1;

        rd_chk_info.type              = H5D_SPARSE_CHUNK;
        rd_chk_info.num_sections      = 2;
        rd_chk_info.filter_mask       = rd_filter_mask;
        rd_chk_info.section_size      = rd_section_size;
        rd_chk_info.section_orig_size = rd_section_size;

        // Read the structured chunk at offset [5,5]: chunk index 3
        if (H5Dread_struct_chunk(did, dxpl, rd_offset, &rd_chk_info, rd_buf) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_direct_read;
        }

        // Verify elements in rd_buf via H5Dread_struct_chunk() is same as wbuf via H5Dwrite()
        if (rd_buf[1][0] != wbuf[7][6] || rd_buf[1][1] != wbuf[8][6])
            ret_val = EXIT_FAILURE;

fail_direct_read:
        free(rd_buf1);

fail_calloc1:
        free(rd_buf0);

fail_calloc0:
        fail_shyper;
        fail_sencode;
        H5Sclose(sid);

fail_dspace:
fail_dwrite:
        H5Dclose(did);

fail_dopen:
        H5Fclose(fid);

fail_file:;
    }
    //! <!-- [direct_chunk_read] -->

    //! <!-- [direct_chunk_iter] -->
    {
        __label__ fail_file, fail_dopen;

        hid_t fid, sid, did, dcpl;
        char  file_name[] = "sparse.h5";
        char  dset_name[] = "sparse_direct_dset";

        // Open the file
        if ((fid = H5Fopen(file_name, H5F_ACC_RDWR, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }

        // Open the 2-d dataset with sparse chunk layout
        if ((did = H5Dopen2(fid, dset_name, H5P_DEFAULT)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_dopen;
        }

        // Iterate over the structured chunks in the dataset
        if (H5Dstruct_chunk_iter(did, H5P_DEFAULT, struct_chunk_cb, NULL) < 0)
            ret_val = EXIT_FAILURE;

        H5Dclose(did);

fail_dopen:
        H5Fclose(fid);

fail_file:;
    }
    //! <!-- [direct_chunk_iter] -->

    //! <!-- [direct_chunk_get_info] -->
    {
        __label__ fail_file, fail_dopen;

        hid_t   fid, sid, did, dcpl;
        char    file_name[] = "sparse.h5";
        char    dset_name[] = "sparse_direct_dset";
        hsize_t offset[2];
        haddr_t addr;
        hsize_t chunk_size;

        // Open the file
        if ((fid = H5Fopen(file_name, H5F_ACC_RDWR, H5P_DEFAULT)) == H5I_INVALID_HID) {
            ret_val = EXIT_FAILURE;
            goto fail_file;
        }

        // Open the 2-d dataset with sparse chunk layout
        if ((did = H5Dopen2(fid, dset_name, H5P_DEFAULT)) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_dopen;
        }

        // Get structured chunk info for chunk index 1 which does not exist
        if (H5Dget_struct_chunk_info(did, H5S_ALL, 1, offset, NULL, &addr, &chunk_size) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_get_info;
        }

        if (chunk_size != 0 && addr != HADDR_UNDEF) {
            ret_val = EXIT_FAILURE;
            goto fail_verify_info;
        }

        // Get structured chunk info for chunk index 3
        if (H5Dget_struct_chunk_info(did, H5S_ALL, 3, offset, NULL, NULL, NULL) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_get_info;
        }

        // Use the offset just retrieved to obtain structured chunk info by coordinates
        if (H5Dget_struct_chunk_info_by_coord(did, offset, NULL, &addr, &chunk_size) < 0) {
            ret_val = EXIT_FAILURE;
            goto fail_get_info;
        }

        if (chunk_size == 0 && addr == HADDR_UNDEF) {
            ret_val = EXIT_FAILURE;
            goto fail_verify_info;
        }

fail_get_info:
fail_verify_info:
fail_get_info:
        H5Dclose(did);

fail_dopen:
        H5Fclose(fid);

fail_file:;
    }
    //! <!-- [direct_chunk_get_info] -->

    return ret_val;
}
