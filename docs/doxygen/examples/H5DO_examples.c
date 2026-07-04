/* -*- c-file-style: "stroustrup" -*- */

//! <!-- [H5DOwrite] -->

#include <zlib.h>
#include <math.h>
#define DEFLATE_SIZE_ADJUST(s) (ceil(((double)(s)) * 1.001) + 12)
                :
                :
size_t       buf_size = CHUNK_NX*CHUNK_NY*sizeof(int);
                const Bytef *z_src = (const Bytef *)(direct_buf);
                Bytef       *z_dst; /* Destination buffer            */
                uLongf       z_dst_nbytes = (uLongf)DEFLATE_SIZE_ADJUST(buf_size);
                uLong        z_src_nbytes = (uLong)buf_size;
                int          aggression   = 9; /* Compression aggression setting */
                uint32_t     filter_mask  = 0;
                size_t       buf_size     = CHUNK_NX * CHUNK_NY * sizeof(int);

                /* Create the data space */
                if ((dataspace = H5Screate_simple(RANK, dims, maxdims)) < 0)
                    goto error;

                /* Create a new file */
                if ((file = H5Fcreate(FILE_NAME5, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
                    goto error;

                /* Modify dataset creation properties, i.e. enable chunking and compression */
                if ((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
                    goto error;

                if ((status = H5Pset_chunk(cparms, RANK, chunk_dims)) < 0)
                    goto error;

                if ((status = H5Pset_deflate(cparms, aggression)) < 0)
                    goto error;

                /* Create a new dataset within the file using cparms creation properties */
                if ((dset_id = H5Dcreate2(file, DATASETNAME, H5T_NATIVE_INT, dataspace, H5P_DEFAULT, cparms,
                                          H5P_DEFAULT)) < 0)
                    goto error;

                /* Initialize data for one chunk */
                for (i = n = 0; i < CHUNK_NX; i++)
                    for (j = 0; j < CHUNK_NY; j++)
                        direct_buf[i][j] = n++;

                /* Allocate output (compressed) buffer */
                outbuf = malloc(z_dst_nbytes);
                z_dst  = (Bytef *)outbuf;

                /* Perform compression from the source to the destination buffer */
                ret = compress2(z_dst, &z_dst_nbytes, z_src, z_src_nbytes, aggression);

                /* Check for various zlib errors */
                if (Z_BUF_ERROR == ret) {
                    fprintf(stderr, "overflow");
                    goto error;
                }
                else if (Z_MEM_ERROR == ret) {
                    fprintf(stderr, "deflate memory error");
                    goto error;
                }
                else if (Z_OK != ret) {
                    fprintf(stderr, "other deflate error");
                    goto error;
                }

                /* Write the compressed chunk data repeatedly to cover all the
                 *  * chunks in the dataset, using the direct write function.     */
                for (i = 0; i < NX / CHUNK_NX; i++) {
                    for (j = 0; j < NY / CHUNK_NY; j++) {
                        status =
                            H5DOwrite_chunk(dset_id, H5P_DEFAULT, filter_mask, offset, z_dst_nbytes, outbuf);
                        offset[1] += CHUNK_NY;
                    }
                    offset[0] += CHUNK_NX;
                    offset[1] = 0;
                }

                /* Overwrite the first chunk with uncompressed data.  Set the filter mask to
                 *  * indicate the compression filter is skipped */
                filter_mask = 0x00000001;
                offset[0] = offset[1] = 0;
                if (H5DOwrite_chunk(dset_id, H5P_DEFAULT, filter_mask, offset, buf_size, direct_buf) < 0)
                    goto error;

                /* Read the entire dataset back for data verification converting ints to longs */
                if (H5Dread(dataset, H5T_NATIVE_LONG, H5S_ALL, H5S_ALL, H5P_DEFAULT, outbuf_long) < 0)
                    goto error;

                /* Data verification here */
            :
            :

            //! <!-- [H5DOwrite] -->

            //! <!-- [H5DOread] -->

#include <zlib.h>
#include <math.h>
#define DEFLATE_SIZE_ADJUST(s) (ceil(((double)(s)) * 1.001) + 12)
                :
                :
size_t       buf_size = CHUNK_NX*CHUNK_NY*sizeof(int);
            const Bytef *z_src = (const Bytef *)(direct_buf);
            Bytef       *z_dst; /* Destination buffer            */
            uLongf       z_dst_nbytes = (uLongf)DEFLATE_SIZE_ADJUST(buf_size);
            uLong        z_src_nbytes = (uLong)buf_size;
            int          aggression   = 9; /* Compression aggression setting */
            uint32_t     filter_mask  = 0;
            size_t       buf_size     = CHUNK_NX * CHUNK_NY * sizeof(int);
            /* For H5DOread_chunk() */
            void        *readbuf = NULL;                   /* Buffer for reading data */
            const Bytef *pt_readbuf;                       /* Point to the buffer for data read */
            hsize_t      read_chunk_nbytes;                /* Size of chunk on disk */
            int          read_dst_buf[CHUNK_NX][CHUNK_NY]; /* Buffer to hold un-compressed data */

            /* Create the data space */
            if ((dataspace = H5Screate_simple(RANK, dims, maxdims)) < 0)
                goto error;

            /* Create a new file */
            if ((file = H5Fcreate(FILE_NAME5, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
                goto error;

            /* Modify dataset creation properties, i.e. enable chunking and compression */
            if ((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
                goto error;

            if ((status = H5Pset_chunk(cparms, RANK, chunk_dims)) < 0)
                goto error;

            if ((status = H5Pset_deflate(cparms, aggression)) < 0)
                goto error;

            /* Create a new dataset within the file using cparms creation properties */
            if ((dset_id = H5Dcreate2(file, DATASETNAME, H5T_NATIVE_INT, dataspace, H5P_DEFAULT, cparms,
                                      H5P_DEFAULT)) < 0)
                goto error;

            /* Initialize data for one chunk */
            for (i = n = 0; i < CHUNK_NX; i++)
                for (j = 0; j < CHUNK_NY; j++)
                    direct_buf[i][j] = n++;

            /* Allocate output (compressed) buffer */
            outbuf = malloc(z_dst_nbytes);
            z_dst  = (Bytef *)outbuf;

            /* Perform compression from the source to the destination buffer */
            ret = compress2(z_dst, &z_dst_nbytes, z_src, z_src_nbytes, aggression);

            /* Check for various zlib errors */
            if (Z_BUF_ERROR == ret) {
                fprintf(stderr, "overflow");
                goto error;
            }
            else if (Z_MEM_ERROR == ret) {
                fprintf(stderr, "deflate memory error");
                goto error;
            }
            else if (Z_OK != ret) {
                fprintf(stderr, "other deflate error");
                goto error;
            }

            /* Write the compressed chunk data repeatedly to cover all the
             *  * chunks in the dataset, using the direct write function.     */
            for (i = 0; i < NX / CHUNK_NX; i++) {
                for (j = 0; j < NY / CHUNK_NY; j++) {
                    status = H5DOwrite_chunk(dset_id, H5P_DEFAULT, filter_mask, offset, z_dst_nbytes, outbuf);
                    offset[1] += CHUNK_NY;
                }
                offset[0] += CHUNK_NX;
                offset[1] = 0;
            }

            if (H5Fflush(dataset, H5F_SCOPE_LOCAL) < 0)
                goto error;

            if (H5Dclose(dataset) < 0)
                goto error;

            if ((dataset = H5Dopen2(file, DATASETNAME1, H5P_DEFAULT)) < 0)
                goto error;

            offset[0] = CHUNK_NX;
            offset[1] = CHUNK_NY;

            /* Get the size of the compressed chunk */
            ret = H5Dget_chunk_storage_size(dataset, offset, &read_chunk_nbytes);

            readbuf    = malloc(read_chunk_nbytes);
            pt_readbuf = (const Bytef *)readbuf;

            /* Use H5DOread_chunk() to read the chunk back */
            if ((status = H5DOread_chunk(dataset, H5P_DEFAULT, offset, &read_filter_mask, readbuf)) < 0)
                goto error;

            ret =
                uncompress((Bytef *)read_dst_buf, (uLongf *)&buf_size, pt_readbuf, (uLong)read_chunk_nbytes);

            /* Check for various zlib errors */
            if (Z_BUF_ERROR == ret) {
                fprintf(stderr, "error: not enough room in output buffer");
                goto error;
            }
            else if (Z_MEM_ERROR == ret) {
                fprintf(stderr, "error: not enough memory");
                goto error;
            }
            else if (Z_OK != ret) {
                fprintf(stderr, "error: corrupted input data");
                goto error;
            }

            /* Data verification here */
            :
            :
//! <!-- [H5DOread] -->
