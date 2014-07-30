/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
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

#include "h5repack.h"
#include "h5tools.h"
#include "h5tools_utils.h"

/*-------------------------------------------------------------------------
 * typedefs
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * globals
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * macros
 *-------------------------------------------------------------------------
 */

/* size of buffer/# of bytes to xfer at a time when copying userblock */
#define USERBLOCK_XFER_SIZE     512     

/* check H5Dread()/H5Dwrite() error, e.g. memory allocation error inside the library. */
#define CHECK_H5DRW_ERROR(_fun, _did, _mtid, _msid, _fsid, _pid, _buf)  {  \
    H5E_BEGIN_TRY {  \
        if(_fun(_did, _mtid, _msid, _fsid, _pid, _buf) < 0) {  \
            hid_t _err_num = 0; \
            char _msg[80]; \
            H5Ewalk2(H5E_DEFAULT, H5E_WALK_DOWNWARD, walk_error_callback, &_err_num); \
            H5Eget_msg(_err_num, NULL, _msg, (size_t)80); \
            error_msg("%s %s -- %s\n", #_fun, "failed", _msg); \
            goto error; \
        } \
    } H5E_END_TRY; \
}

/*-------------------------------------------------------------------------
 * local functions
 *-------------------------------------------------------------------------
 */
static int Get_hyperslab(hid_t dcpl_id, int rank_dset, hsize_t dims_dset[],
		size_t size_datum, hsize_t dims_hslab[], hsize_t * hslab_nbytes_p);
static void print_dataset_info(hid_t dcpl_id, char *objname, double per, int pr);
static int do_copy_objects(hid_t fidin, hid_t fidout, trav_table_t *travt,
		pack_opt_t *options);
static int copy_user_block(const char *infile, const char *outfile,
		hsize_t size);
#if defined (H5REPACK_DEBUG_USER_BLOCK)
static void print_user_block(const char *filename, hid_t fid);
#endif
static herr_t walk_error_callback(unsigned n, const H5E_error2_t *err_desc, void *udata);

/* get the major number from the error stack. */
static herr_t walk_error_callback(UNUSED unsigned n, const H5E_error2_t *err_desc, void *udata) {
	if (err_desc)
		*((int *) udata) = err_desc->maj_num;

	return 0;
}

/*-------------------------------------------------------------------------
 * Function: copy_objects
 *
 * Purpose: duplicate all HDF5 objects in the file
 *
 * Return: 0, ok, -1 no
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October, 23, 2003
 *
 * Modification:
 *   Peter Cao, June 13, 2007
 *   Add "-L, --latest" and other options to pack a file with the latest file format
 *
 *   Peter Cao, September 25, 2007
 *   Copy user block when repacking a file
 *
 *   Pedro Vicente, August 20, 2008
 *   Add a user block to file if requested
 *
 *-------------------------------------------------------------------------
 */

int copy_objects(const char* fnamein, const char* fnameout, pack_opt_t *options) {
	hid_t fidin;
	hid_t fidout = -1;
	trav_table_t *travt = NULL;
	hsize_t ub_size = 0; /* size of user block */
	hid_t fcpl = H5P_DEFAULT; /* file creation property list ID */
	hid_t fapl = H5P_DEFAULT; /* file access property list ID */

	/*-------------------------------------------------------------------------
	 * open input file
	 *-------------------------------------------------------------------------
	 */
	if ((fidin = h5tools_fopen(fnamein, H5F_ACC_RDONLY, H5P_DEFAULT, NULL, NULL,
			(size_t) 0)) < 0) {
		error_msg("<%s>: %s\n", fnamein, H5FOPENERROR);
		goto out;
	}

	/* get user block size and file space strategy/threshold */
	{
		hid_t fcpl_in; /* file creation property list ID for input file */

		if ((fcpl_in = H5Fget_create_plist(fidin)) < 0) {
			error_msg("failed to retrieve file creation property list\n");
			goto out;
		}

		if (H5Pget_userblock(fcpl_in, &ub_size) < 0) {
			error_msg("failed to retrieve userblock size\n");
			goto out;
		}

		if (H5Pclose(fcpl_in) < 0) {
			error_msg("failed to close property list\n");
			goto out;
		}
	}

	/* Check if we need to create a non-default file creation property list */
	if (options->latest || ub_size > 0) {
		/* Create file creation property list */
		if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0) {
			error_msg("fail to create a file creation property list\n");
			goto out;
		}

		if (ub_size > 0) {
			if (H5Pset_userblock(fcpl, ub_size) < 0) {
				error_msg("failed to set non-default userblock size\n");
				goto out;
			}
		}

		if (options->latest) {
			unsigned i = 0, nindex = 0, mesg_type_flags[5], min_mesg_sizes[5];

			/* Adjust group creation parameters for root group */
			/* (So that it is created in "dense storage" form) */
			if (H5Pset_link_phase_change(fcpl, (unsigned) options->grp_compact,
					(unsigned) options->grp_indexed) < 0) {
				error_msg(
						"fail to adjust group creation parameters for root group\n");
				goto out;
			}

			for (i = 0; i < 5; i++) {
				if (options->msg_size[i] > 0) {
					switch (i) {
					case 0:
						mesg_type_flags[nindex] = H5O_SHMESG_SDSPACE_FLAG;
						break;

					case 1:
						mesg_type_flags[nindex] = H5O_SHMESG_DTYPE_FLAG;
						break;

					case 2:
						mesg_type_flags[nindex] = H5O_SHMESG_FILL_FLAG;
						break;

					case 3:
						mesg_type_flags[nindex] = H5O_SHMESG_PLINE_FLAG;
						break;

					case 4:
						mesg_type_flags[nindex] = H5O_SHMESG_ATTR_FLAG;
						break;
					default:
						break;
					} /* end switch */
					min_mesg_sizes[nindex] = (unsigned) options->msg_size[i];

					nindex++;
				} /* end if */
			} /* end for */

			if (nindex > 0) {
				if (H5Pset_shared_mesg_nindexes(fcpl, nindex) < 0) {
					error_msg(
							"fail to set the number of shared object header message indexes\n");
					goto out;
				}

				/* msg_size[0]=dataspace, 1=datatype, 2=file value, 3=filter pipleline, 4=attribute */
				for (i = 0; i < (nindex - 1); i++) {
					if (H5Pset_shared_mesg_index(fcpl, i, mesg_type_flags[i],
							min_mesg_sizes[i]) < 0) {
						error_msg(
								"fail to configure the specified shared object header message index\n");
						goto out;
					} /* end if */
				} /* end for */
			} /* if (nindex>0) */

			/* Create file access property list */
			if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0) {
				error_msg("Could not create file access property list\n");
				goto out;
			} /* end if */

			if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0) {
				error_msg(
						"Could not set property for using latest version of the format\n");
				goto out;
			} /* end if */
		} /* end if */
	} /* end if */
#if defined (H5REPACK_DEBUG_USER_BLOCK)
	print_user_block(fnamein, fidin);
#endif

	/*-------------------------------------------------------------------------
	 * set the new user userblock options in the FCPL (before H5Fcreate )
	 *-------------------------------------------------------------------------
	 */
	if (options->ublock_size > 0) {
		/* either use the FCPL already created or create a new one */
		if (fcpl == H5P_DEFAULT) {
			/* create a file creation property list */
			if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0) {
				error_msg("fail to create a file creation property list\n");
				goto out;
			}
		}

		/* set user block size */
		if (H5Pset_userblock(fcpl, options->ublock_size) < 0) {
			error_msg("failed to set userblock size\n");
			goto out;
		}
	}

	/*-------------------------------------------------------------------------
	 * set alignment options
	 *-------------------------------------------------------------------------
	 */
	if (options->alignment > 0) {
		/* either use the FAPL already created or create a new one */
		if (fapl == H5P_DEFAULT) {
			/* create a file access property list */
			if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0) {
				error_msg("Could not create file access property list\n");
				goto out;
			}
		}

		if (H5Pset_alignment(fapl, options->threshold, options->alignment) < 0) {
			error_msg("failed to set alignment\n");
			goto out;
		}
	}

	/*-------------------------------------------------------------------------
	 * set metadata block size option
	 *-------------------------------------------------------------------------
	 */
	if (options->meta_block_size > 0) {
		/* either use the FAPL already created or create a new one */
		if (fapl == H5P_DEFAULT) {
			/* create a file access property list */
			if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0) {
				error_msg("Could not create file access property list\n");
				goto out;
			}
		}

		if (H5Pset_meta_block_size(fapl, options->meta_block_size) < 0) {
			error_msg("failed to set metadata block size\n");
			goto out;
		}
	}


	/*-------------------------------------------------------------------------
	 * create the output file
	 *-------------------------------------------------------------------------
	 */
	if (options->verbose)
		printf("Making file <%s>...\n", fnameout);

	if ((fidout = H5Fcreate(fnameout, H5F_ACC_TRUNC, fcpl, fapl)) < 0) {
		error_msg("<%s>: Could not create file\n", fnameout);
		goto out;
	}

	/*-------------------------------------------------------------------------
	 * write a new user block if requested
	 *-------------------------------------------------------------------------
	 */
	if (options->ublock_size > 0) {
		if (copy_user_block(options->ublock_filename, fnameout,
				options->ublock_size) < 0) {
			error_msg("Could not copy user block. Exiting...\n");
			goto out;

		}
	}

	/*-------------------------------------------------------------------------
	 * get list of objects
	 *-------------------------------------------------------------------------
	 */

	/* init table */
	trav_table_init(&travt);

	/* get the list of objects in the file */
	if (h5trav_gettable(fidin, travt) < 0)
		goto out;

	/*-------------------------------------------------------------------------
	 * do the copy
	 *-------------------------------------------------------------------------
	 */
	if (do_copy_objects(fidin, fidout, travt, options) < 0) {
		error_msg("<%s>: Could not copy data to: %s\n", fnamein, fnameout);
		goto out;
	} /* end if */

	/*-------------------------------------------------------------------------
	 * do the copy of referenced objects
	 * and create hard links
	 *-------------------------------------------------------------------------
	 */
	if (do_copy_refobjs(fidin, fidout, travt, options) < 0) {
		printf("h5repack: <%s>: Could not copy data to: %s\n", fnamein,
				fnameout);
		goto out;
	}

	/*-------------------------------------------------------------------------
	 * close
	 *-------------------------------------------------------------------------
	 */

	if (fapl > 0)
		H5Pclose(fapl);

	if (fcpl > 0)
		H5Pclose(fcpl);

	H5Fclose(fidin);
	H5Fclose(fidout);

	/* free table */
	trav_table_free(travt);
	travt = NULL;

	/*-------------------------------------------------------------------------
	 * write only the input file user block if there is no user block file input
	 *-------------------------------------------------------------------------
	 */

	if (ub_size > 0 && options->ublock_size == 0) {
		if (copy_user_block(fnamein, fnameout, ub_size) < 0) {
			error_msg("Could not copy user block. Exiting...\n");
			goto out;

		}
	}

	return 0;

	/*-------------------------------------------------------------------------
	 * out
	 *-------------------------------------------------------------------------
	 */

out:
	H5E_BEGIN_TRY
		{
			H5Pclose(fapl);
			H5Pclose(fcpl);
			H5Fclose(fidin);
			H5Fclose(fidout);
		}H5E_END_TRY;
	if (travt)
		trav_table_free(travt);

	return -1;
}

/*-------------------------------------------------------------------------
 * Function: Get_hyperslab
 *
 * Purpose: Calulate a hyperslab from a dataset for higher performance.
 *          The size of hyperslab is limitted by H5TOOLS_BUFSIZE.
 *          Return the hyperslab dimentions and size in byte.
 *
 * Return: 0 - SUCCEED, -1 FAILED
 *
 * Parameters:
 *   dcpl_id : [IN] dataset creation property.
 *   rank_dset : [IN] dataset rank
 *   dims_dset[] : [IN] dataset dimentions
 *   size_datum : [IN] size of a data element in byte
 *   dims_hslab[] : [OUT] calculated hyperslab dimentions
 *   * hslab_nbytes_p : [OUT] total byte of the hyperslab
 *
 * Programmer: Jonathan Kim
 * Date: Feburary, 2012
 * Update:
 *   The hyperslab calucation would be depend on if the dataset is chunked
 *   or not.
 *
 *   There care 3 conditions to cover:
 *   1. If chunked and a chunk fits in buffer, each chunk would be a unit of
 *      collection and the boundary would be dataset's dims.
 *   2. If chunked but a chunk doesn't fit in buffer, each data element would
 *      be a unit of collection and the boundary would be the chunk itself.
 *   3. If not chunked, each data element would be a unit of collection and
 *      the boundary would be dataset's dims.
 *
 *   The calulation starts from the last dimention (h5dump dims output).
 *
 * Note:
 *   Added for JIRA HDFFV-7862.
 *-----------------------------------------*/

int Get_hyperslab(hid_t dcpl_id, int rank_dset, hsize_t dims_dset[],
		size_t size_datum, hsize_t dims_hslab[], hsize_t * hslab_nbytes_p) {
	int status = 0;
	int k;
	H5D_layout_t dset_layout;
	int rank_chunk;
	hsize_t dims_chunk[H5S_MAX_RANK];
	hsize_t size_chunk = 1;
	hsize_t nchunk_fit; /* number of chunks that fits in hyperslab buffer (H5TOOLS_BUFSIZE) */
	hsize_t ndatum_fit; /* number of dataum that fits in hyperslab buffer (H5TOOLS_BUFSIZE) */
	hsize_t chunk_dims_map[H5S_MAX_RANK]; /* mapped chunk dimentions */
	hsize_t hs_dims_map[H5S_MAX_RANK]; /* mapped hyperslab dimentions */
	hsize_t hslab_nbytes; /* size of hyperslab in byte */

	/* init to set as size of a data element */
	hslab_nbytes = size_datum;

	/* get layout of dataset */
	dset_layout = H5Pget_layout(dcpl_id);

	/* if dataset is chunked */
	if (dset_layout == H5D_CHUNKED) {
		/* get chunk dims */
		rank_chunk = H5Pget_chunk(dcpl_id, rank_dset, dims_chunk);
		if (rank_chunk < 0) {
			status = -1;
			goto out;
		}

		for (k = rank_dset; k > 0; --k)
			size_chunk *= dims_chunk[k - 1];

		/* figure out how many chunks can fit in the hyperslab buffer */
		nchunk_fit = (H5TOOLS_BUFSIZE / size_datum) / size_chunk;

		/* 1. if a chunk fit in hyperslab buffer */
		if (nchunk_fit >= 1) {
			/* Calulate a hyperslab that contains as many chunks that can fit
			 * in hyperslab buffer. Hyperslab will be increased starting from
			 * the last dimention of the dataset (see h5dump's dims output).
			 * The calculation boundary is dataset dims.
			 * In the loop, used mapping from a datum to a chunk to figure out
			 * chunk based hyperslab.
			 */
			for (k = rank_dset; k > 0; --k) {
				/* map dataset dimentions with a chunk dims */
				chunk_dims_map[k - 1] = dims_dset[k - 1] / dims_chunk[k - 1];

				/* if reminder exist, increse by 1 to cover partial edge chunks */
				if (dims_dset[k - 1] % dims_chunk[k - 1] > 0)
					chunk_dims_map[k - 1]++;

				/* get mapped hyperslab dims */
				hs_dims_map[k - 1] = MIN (nchunk_fit, chunk_dims_map[k-1]);

				/* prepare next round */
				nchunk_fit = nchunk_fit / chunk_dims_map[k - 1];
				/* if a chunk is bigger than the rest of buffer */
				if (nchunk_fit == 0)
					nchunk_fit = 1;

				/* get hyperslab dimentions as unmapping to actual size */
				dims_hslab[k - 1] =
						MIN( (hs_dims_map[k-1] * dims_chunk[k-1]), dims_dset[k-1]);

				/* calculate total size for the hyperslab */
				hslab_nbytes *= dims_hslab[k - 1];
			}
		}
		/* 2. if a chunk is bigger than hyperslab buffer */
		else {
			/* Calulate a hyperslab that contains as many data elements that
			 * can fit in hyperslab buffer. Hyperslab will be increased
			 * starting from the last dimention of the chunk (see h5dump's dims
			 * output).
			 * The calculation boundary is a chunk dims.
			 */
			for (k = rank_dset; k > 0; --k) {
				ndatum_fit = H5TOOLS_BUFSIZE / hslab_nbytes;

				/* if a datum is bigger than rest of buffer */
				if (ndatum_fit == 0)
					ndatum_fit = 1;
				/* get hyperslab dimentions within a chunk boundary */
				dims_hslab[k - 1] = MIN (dims_chunk[k-1], ndatum_fit);

				/* calculate total size for the hyperslab */
				hslab_nbytes *= dims_hslab[k - 1];

				if (hslab_nbytes <= 0) {
					status = -1;
					goto out;
				}
			}
		}
	}
	/* 3. if dataset is not chunked */
	else {
		/* Calulate a hyperslab that contains as many data elements that can
		 * fit in hyperslab buffer. Hyperslab will be increased starting from
		 * the last dimention of the dataset (see h5dump's dims output).
		 * The calculation boundary is dataset dims.
		 */
		for (k = rank_dset; k > 0; --k) {
			ndatum_fit = H5TOOLS_BUFSIZE / hslab_nbytes;

			/* if a datum is bigger than rest of buffer */
			if (ndatum_fit == 0)
				ndatum_fit = 1;
			/* get hyperslab dimentions within dataset boundary */
			dims_hslab[k - 1] = MIN(dims_dset[k - 1], ndatum_fit);

			/* calculate total size for the hyperslab */
			hslab_nbytes *= dims_hslab[k - 1];

			if (hslab_nbytes <= 0) {
				status = -1;
				goto out;
			}
		}
	}

	/* pass out the hyperslab size*/
	*hslab_nbytes_p = hslab_nbytes;

out:
	return status;
}

/*-------------------------------------------------------------------------
 * Function: do_copy_objects
 *
 * Purpose: duplicate all HDF5 objects in the file
 *
 * Return: 0, ok, -1 no
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October, 23, 2003
 *
 * Modifications:
 *
 *  July 2004:     Introduced the extra EC or NN option for SZIP
 *
 *  December 2004: Added a check for H5Dcreate; if the dataset cannot be created
 *                  with the requested filter, use the input one
 *
 *  October 2006:  Read/write using the file type by default.
 *
 *  October 2006:  Read by hyperslabs for big datasets.
 *
 *  A threshold of H5TOOLS_MALLOCSIZE (128 MB) is the limit upon which I/O hyperslab is done
 *  i.e., if the memory needed to read a dataset is greater than this limit,
 *  then hyperslab I/O is done instead of one operation I/O
 *  For each dataset, the memory needed is calculated according to
 *
 *  memory needed = number of elements * size of each element
 *
 *  if the memory needed is lower than H5TOOLS_MALLOCSIZE, then the following operations
 *  are done
 *
 *  H5Dread( input_dataset1 )
 *  H5Dread( input_dataset2 )
 *
 *  with all elements in the datasets selected. If the memory needed is greater than
 *  H5TOOLS_MALLOCSIZE, then the following operations are done instead:
 *
 *  a strip mine is defined for each dimension k (a strip mine is defined as a
 *  hyperslab whose size is memory manageable) according to the formula
 *
 *  (1) strip_mine_size[k ] = MIN(dimension[k ], H5TOOLS_BUFSIZE / size of memory type)
 *
 *  where H5TOOLS_BUFSIZE is a constant currently defined as 1MB. This formula assures
 *  that for small datasets (small relative to the H5TOOLS_BUFSIZE constant), the strip
 *  mine size k is simply defined as its dimension k, but for larger datasets the
 *  hyperslab size is still memory manageable.
 *  a cycle is done until the number of elements in the dataset is reached. In each
 *  iteration, two parameters are defined for the function H5Sselect_hyperslab,
 *  the start and size of each hyperslab, according to
 *
 *  (2) hyperslab_size [k] = MIN(dimension[k] - hyperslab_offset[k], strip_mine_size [k])
 *
 *  where hyperslab_offset [k] is initially set to zero, and later incremented in
 *  hyperslab_size[k] offsets. The reason for the operation
 *
 *  dimension[k] - hyperslab_offset[k]
 *
 *  in (2) is that, when using the strip mine size, it assures that the "remaining" part
 *  of the dataset that does not fill an entire strip mine is processed.
 *
 *  November 2006:  Use H5Ocopy in the copy of objects. The logic for using
 *   H5Ocopy or not is if a change of filters or layout is requested by the user
 *   then use read/write else use H5Ocopy.
 *
 * May, 1, 2008: Add a printing of the compression ratio of old size / new size
 *
 * Feburary 2012:  improve Read/Write by hyperslabs for big datasets.
 * Programmer: Jonathan Kim
 *
 *  A threshold of H5TOOLS_MALLOCSIZE is the limit upon which I/O hyperslab is done
 *  i.e., if the memory needed to read a dataset is greater than this limit,
 *  then hyperslab I/O is done instead of one operation I/O
 *  For each dataset, the memory needed is calculated according to
 *
 *  memory needed = number of elements * size of each element
 *
 *  if the memory needed is lower than H5TOOLS_MALLOCSIZE, then the following operations
 *  are done
 *
 *  H5Dread( input_dataset )
 *  H5Dwrite( output_dataset )
 *
 *  with all elements in the datasets selected. If the memory needed is greater than
 *  H5TOOLS_MALLOCSIZE, then the following operations are done instead:
 *
 *  1. figure out a hyperslab (dimentions) and size  (refer to Get_hyperslab()).
 *  2. Calculate the hyperslab selections as the selection is moving forward.
 *     Selection would be same as the hyperslab except for the remaining edge portion
 *     of the dataset. The code take care of the remaining portion if exist.
 *
 *-------------------------------------------------------------------------
 */

int do_copy_objects(hid_t fidin, hid_t fidout, trav_table_t *travt,
		pack_opt_t *options) /* repack options */
{
	hid_t grp_in = -1; /* group ID */
	hid_t grp_out = -1; /* group ID */
	hid_t dset_in = -1; /* read dataset ID */
	hid_t dset_out = -1; /* write dataset ID */
	hid_t gcpl_in = -1; /* group creation property list */
	hid_t gcpl_out = -1; /* group creation property list */
	hid_t type_in = -1; /* named type ID */
	hid_t type_out = -1; /* named type ID */
	hid_t dcpl_in = -1; /* dataset creation property list ID */
	hid_t dcpl_out = -1; /* dataset creation property list ID */
	hid_t f_space_id = -1; /* file space ID */
	hid_t ftype_id = -1; /* file type ID */
	hid_t wtype_id = -1; /* read/write type ID */
	named_dt_t *named_dt_head = NULL; /* Pointer to the stack of named datatypes copied */
	size_t msize; /* size of type */
	hsize_t nelmts; /* number of elements in dataset */
	H5D_space_status_t space_status; /* determines whether space has been allocated for the dataset  */
	int rank; /* rank of dataset */
	hsize_t dims[H5S_MAX_RANK];/* dimensions of dataset */
	hsize_t dsize_in; /* input dataset size before filter */
	hsize_t dsize_out; /* output dataset size after filter */
	int apply_s; /* flag for apply filter to small dataset sizes */
	int apply_f; /* flag for apply filter to return error on H5Dcreate */
	void *buf = NULL; /* buffer for raw data */
	void *hslab_buf = NULL; /* hyperslab buffer for raw data */
	int has_filter; /* current object has a filter */
	int req_filter; /* there was a request for a filter */
	int req_obj_layout = 0; /* request layout to current object */
	unsigned crt_order_flags; /* group creation order flag */
	unsigned i;
	unsigned u;
	int is_ref = 0;
	htri_t is_named;
	hbool_t limit_maxdims;
	hsize_t size_dset;

	/*-------------------------------------------------------------------------
	 * copy the suppplied object list
	 *-------------------------------------------------------------------------
	 */

	if (options->verbose) {
		printf("-----------------------------------------\n");
		printf(" Type     Filter (Compression)     Name\n");
		printf("-----------------------------------------\n");
	}

	for (i = 0; i < travt->nobjs; i++) {
		/* init variables per obj */
		buf = NULL;
		limit_maxdims = FALSE;

		switch (travt->objs[i].type) {

		case H5TRAV_TYPE_UNKNOWN:
			HDassert(0);
			break;
			/*-------------------------------------------------------------------------
			 * H5TRAV_TYPE_GROUP
			 *-------------------------------------------------------------------------
			 */
		case H5TRAV_TYPE_GROUP:

			if (options->verbose) {
				printf(FORMAT_OBJ, "group", travt->objs[i].name);
			}

			/* open input group */
			if ((grp_in = H5Gopen2(fidin, travt->objs[i].name, H5P_DEFAULT))
					< 0)
				goto error;

			/* get input group creation property list */
			if ((gcpl_in = H5Gget_create_plist(grp_in)) < 0)
				goto error;

			/* query and set the group creation properties */
			if (H5Pget_link_creation_order(gcpl_in, &crt_order_flags) < 0)
				goto error;

			/* set up group creation property list */
			if ((gcpl_out = H5Pcreate(H5P_GROUP_CREATE)) < 0)
				goto error;

			if (H5Pset_link_creation_order(gcpl_out, crt_order_flags) < 0)
				goto error;

			/*-------------------------------------------------------------------------
			 * the root is a special case, we get an ID for the root group
			 * and copy its attributes using that ID
			 *-------------------------------------------------------------------------
			 */
			if (HDstrcmp(travt->objs[i].name, "/") == 0) {
				if ((grp_out = H5Gopen2(fidout, "/", H5P_DEFAULT)) < 0)
					goto error;
			}
			else {

				if (options->grp_compact > 0 || options->grp_indexed > 0) {
					if (H5Pset_link_phase_change(gcpl_out,
							(unsigned) options->grp_compact,
							(unsigned) options->grp_indexed) < 0)
						goto error;
				}

				if ((grp_out = H5Gcreate2(fidout, travt->objs[i].name,
						H5P_DEFAULT, gcpl_out, H5P_DEFAULT)) < 0)
					goto error;

			}

			/*-------------------------------------------------------------------------
			 * copy attrs
			 *-------------------------------------------------------------------------
			 */
			if (copy_attr(grp_in, grp_out, &named_dt_head, travt, options) < 0)
				goto error;

			if (H5Pclose(gcpl_out) < 0)
				goto error;
			if (H5Pclose(gcpl_in) < 0)
				goto error;
			if (H5Gclose(grp_out) < 0)
				goto error;
			if (H5Gclose(grp_in) < 0)
				goto error;

			break;

			/*-------------------------------------------------------------------------
			 * H5TRAV_TYPE_DATASET
			 *-------------------------------------------------------------------------
			 */
		case H5TRAV_TYPE_DATASET:

			has_filter = 0;
			req_filter = 0;

			/* check if global filters were requested */
			if (options->n_filter_g)
				req_filter = 1;

			/* check if filters were requested for individual objects */
			for (u = 0; u < options->op_tbl->nelems; u++) {

				if (HDstrcmp(travt->objs[i].name, options->op_tbl->objs[u].path) == 0) {
					if (options->op_tbl->objs[u].filter->filtn > 0) {
						req_filter = 1;
					}
				}
			}

			/* check if layout change requested individual object */
			if (options->layout_g != H5D_LAYOUT_ERROR) {
				pack_info_t *pckinfo;
				/* any dataset is specified */
				if (options->op_tbl->nelems > 0) {
					/* check if object exist */
					pckinfo = options_get_object(travt->objs[i].name,
							options->op_tbl);
					if (pckinfo) {
						req_obj_layout = 1;
					}
				}
			}

			/* early detection of references */
			if ((dset_in = H5Dopen2(fidin, travt->objs[i].name, H5P_DEFAULT))
					< 0)
				goto error;
			if ((ftype_id = H5Dget_type(dset_in)) < 0)
				goto error;
			if (H5T_REFERENCE == H5Tget_class(ftype_id))
				is_ref = 1;

			/* Check if the datatype is committed */
			if ((is_named = H5Tcommitted(ftype_id)) < 0)
				goto error;
			if (is_named)
				if ((wtype_id = copy_named_datatype(ftype_id, fidout,
						&named_dt_head, travt, options)) < 0)
					goto error;

			if (H5Tclose(ftype_id) < 0)
				goto error;
			if (H5Dclose(dset_in) < 0)
				goto error;

			/*-------------------------------------------------------------------------
			 * check if we should use H5Ocopy or not
			 * if there is a request for filters/layout, we read/write the object
			 * otherwise we do a copy using H5Ocopy
			 *-------------------------------------------------------------------------
			 */
			if (options->op_tbl->nelems || options->all_filter == 1
					|| options->all_layout == 1 || is_ref || is_named) {

				int j;

				if ((dset_in = H5Dopen2(fidin, travt->objs[i].name, H5P_DEFAULT)) < 0)
					goto error;
				if ((f_space_id = H5Dget_space(dset_in)) < 0)
					goto error;
				if ((ftype_id = H5Dget_type(dset_in)) < 0)
					goto error;
				if ((dcpl_in = H5Dget_create_plist(dset_in)) < 0)
					goto error;
				if ((dcpl_out = H5Pcopy(dcpl_in)) < 0)
					goto error;
				if ((rank = H5Sget_simple_extent_ndims(f_space_id)) < 0)
					goto error;
				HDmemset(dims, 0, sizeof dims);
				if (H5Sget_simple_extent_dims(f_space_id, dims, NULL) < 0)
					goto error;

				if (H5Dget_space_status(dset_in, &space_status) < 0)
					goto error;

				nelmts = 1;
				for (j = 0; j < rank; j++)
					nelmts *= dims[j];

				/* wtype_id will have already been set if using a named dtype */
				if (!is_named) {
					if (options->use_native == 1)
						wtype_id = h5tools_get_native_type(ftype_id);
					else
						wtype_id = H5Tcopy(ftype_id);
				} /* end if */

				if ((msize = H5Tget_size(wtype_id)) == 0)
					goto error;

				/* size of current dset */
				size_dset = nelmts * msize;

				/*-------------------------------------------------------------------------
				 * check if the dataset creation property list has filters that
				 * are not registered in the current configuration
				 * 1) the external filters GZIP and SZIP might not be available
				 * 2) the internal filters might be turned off
				 *-------------------------------------------------------------------------
				 */
				if (h5tools_canreadf((travt->objs[i].name), dcpl_in) == 1) {
					apply_s = 1;
					apply_f = 1;

					/*-------------------------------------------------------------------------
					 * references are a special case
					 * we cannot just copy the buffers, but instead we recreate the reference
					 * in a second traversal of the output file
					 *-------------------------------------------------------------------------
					 */
					if (H5T_REFERENCE != H5Tget_class(wtype_id)) {
						/* get the storage size of the input dataset */
						dsize_in = H5Dget_storage_size(dset_in);

						/* check for small size datasets (less than 1k) except
						 * changing to COMPACT. For the reference, COMPACT is limited
						 * by size 64K by library.
						 */
						if (options->layout_g != H5D_COMPACT) {
							if (size_dset < options->min_comp)
								apply_s = 0;
						}

						/* apply the filter */
						if (apply_s) {
							if (apply_filters(travt->objs[i].name, rank, dims,
									msize, dcpl_out, options, &has_filter) < 0)
								goto error;
						}

						/* only if layout change requested for entire file or
						 * individual obj */
						if (options->all_layout > 0 || req_obj_layout == 1)
							/*-------------------------------------------------
							 * Unset the unlimited max dims if convert to other
							 * than chunk layouts, because unlimited max dims
							 * only can be applied to chunk layout.
							 * Also perform only for targeted dataset
							 * Also check for size limit to convert to compact
							 *-------------------------------------------------*/
							if (options->layout_g != H5D_CHUNKED) {
								/* any dataset is specified */
								if (options->op_tbl->nelems > 0) {
									/* if current obj match specified obj */
									if (options_get_object(travt->objs[i].name,
											options->op_tbl))
										limit_maxdims = TRUE;
								}
								else { /* no dataset is specified */
									limit_maxdims = TRUE;
								}

								/* if convert to COMPACT */
								if (options->layout_g == H5D_COMPACT) {
									/* should be smaller than 64K */
									if (size_dset > MAX_COMPACT_DSIZE)
										limit_maxdims = FALSE;
								}

								/* unset unlimited max dims */
								if (limit_maxdims)
									H5Sset_extent_simple(f_space_id, rank, dims,
											NULL);
							}

						/*-------------------------------------------------------------------------
						 * create the output dataset;
						 * disable error checking in case the dataset cannot be created with the
						 * modified dcpl; in that case use the original instead
						 *-------------------------------------------------------------------------
						 */
						H5E_BEGIN_TRY
							{
								dset_out = H5Dcreate2(fidout,
										travt->objs[i].name, wtype_id,
										f_space_id, H5P_DEFAULT, dcpl_out,
										H5P_DEFAULT);
							}H5E_END_TRY;

						if (dset_out == FAIL) {
							if (options->verbose)
								printf(
										" warning: could not create dataset <%s>. Applying original settings\n",
										travt->objs[i].name);

							if ((dset_out = H5Dcreate2(fidout,
									travt->objs[i].name, wtype_id, f_space_id,
									H5P_DEFAULT, dcpl_in, H5P_DEFAULT)) < 0)
								goto error;
							apply_f = 0;
						}

						/*-------------------------------------------------------------------------
						 * read/write
						 *-------------------------------------------------------------------------
						 */
						if (nelmts > 0 && space_status != H5D_SPACE_STATUS_NOT_ALLOCATED) {
							size_t need = (size_t)(nelmts * msize); /* bytes needed */

							/* have to read the whole dataset if there is only one element in the dataset */
							if (need < H5TOOLS_MALLOCSIZE)
								buf = HDmalloc(need);

							if (buf != NULL) {
								/* read/write: use the macro to check error, e.g. memory allocation error inside the library. */
								CHECK_H5DRW_ERROR(H5Dread, dset_in, wtype_id,
										H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
								CHECK_H5DRW_ERROR(H5Dwrite, dset_out, wtype_id,
										H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);

								/* Check if we have VL data in the dataset's
								 * datatype that must be reclaimed */
								if (TRUE == H5Tdetect_class(wtype_id, H5T_VLEN))
									if (H5Dvlen_reclaim(wtype_id, f_space_id,
											H5P_DEFAULT, buf) < 0)
										goto error;
							}
							else { /* possibly not enough memory, read/write by hyperslabs */
								size_t p_type_nbytes = msize; /*size of memory type */
								hsize_t p_nelmts = nelmts; /*total elements */
								hsize_t elmtno; /*counter  */
								int carry; /*counter carry value */
								unsigned int vl_data = 0; /*contains VL datatypes */

								/* hyperslab info */
								hsize_t hslab_dims[H5S_MAX_RANK]; /*hyperslab dims */
								hsize_t hslab_nbytes; /*bytes per hyperslab */
								hsize_t hslab_nelmts; /*elements per hyperslab*/
								hid_t hslab_space; /*hyperslab data space */

								/* hyperslab selection info */
								hsize_t hs_sel_offset[H5S_MAX_RANK];/* selection offset */
								hsize_t hs_sel_count[H5S_MAX_RANK]; /* selection count */
								hsize_t hs_select_nelmts; /* selected elements */
								hsize_t zero[8]; /*vector of zeros */
								int k;
								H5D_layout_t dset_layout;
								hid_t dcpl_tmp = -1; /* dataset creation property list ID */

								/* check if we have VL data in the dataset's datatype */
								if (H5Tdetect_class(wtype_id, H5T_VLEN) == TRUE)
									vl_data = TRUE;

								/* check first if writing dataset is chunked,
								 * if so use its chunk layout for better performance. */
								dset_layout = H5Pget_layout(dcpl_out);
								if (dset_layout == H5D_CHUNKED)
									dcpl_tmp = dcpl_out; /* writing dataset */
								else { /* if reading dataset is chunked */
									dset_layout = H5Pget_layout(dcpl_in);
									if (dset_layout == H5D_CHUNKED)
										dcpl_tmp = dcpl_in; /* reading dataset */
								}

								/* get hyperslab dims and size in byte */
								if (Get_hyperslab(dcpl_tmp, rank, dims,
										p_type_nbytes, hslab_dims,
										&hslab_nbytes) < 0)
									goto error;

								hslab_buf = HDmalloc((size_t)hslab_nbytes);

								hslab_nelmts = hslab_nbytes / p_type_nbytes;
								hslab_space = H5Screate_simple(1, &hslab_nelmts,
										NULL);

								/* the hyperslab selection loop */
								HDmemset(hs_sel_offset, 0,
										sizeof hs_sel_offset);
								HDmemset(zero, 0, sizeof zero);

								for (elmtno = 0; elmtno < p_nelmts; elmtno +=
										hs_select_nelmts) {
									if (rank > 0) {
										/* calculate the hyperslab selections. The selection would be same as the hyperslab except for remaining edge portion of the dataset which is smaller then the hyperslab.
										 */
										for (k = 0, hs_select_nelmts = 1;
												k < rank; k++) {
											/* MIN() is used to get the remaining edge portion if exist.
											 * "dims[k] - hs_sel_offset[k]" is remaining edge portion that is smaller then the hyperslab.*/
											hs_sel_count[k] =
													MIN(dims[k] - hs_sel_offset[k], hslab_dims[k]);
											hs_select_nelmts *= hs_sel_count[k];
										}

										if (H5Sselect_hyperslab(f_space_id,
												H5S_SELECT_SET, hs_sel_offset,
												NULL, hs_sel_count, NULL) < 0)
											goto error;
										if (H5Sselect_hyperslab(hslab_space,
												H5S_SELECT_SET, zero, NULL,
												&hs_select_nelmts, NULL) < 0)
											goto error;
									}
									else {
										H5Sselect_all(f_space_id);
										H5Sselect_all(hslab_space);
										hs_select_nelmts = 1;
									} /* rank */

									/* read/write: use the macro to check error, e.g. memory allocation error inside the library. */
									CHECK_H5DRW_ERROR(H5Dread, dset_in,
											wtype_id, hslab_space, f_space_id,
											H5P_DEFAULT, hslab_buf);
									CHECK_H5DRW_ERROR(H5Dwrite, dset_out,
											wtype_id, hslab_space, f_space_id,
											H5P_DEFAULT, hslab_buf);

									/* reclaim any VL memory, if necessary */
									if (vl_data)
										H5Dvlen_reclaim(wtype_id, hslab_space,
												H5P_DEFAULT, hslab_buf);

									/* calculate the next hyperslab offset */
									for (k = rank, carry = 1; k > 0 && carry;
											--k) {
										hs_sel_offset[k - 1] += hs_sel_count[k
												- 1];
										/* if reached the end of a dim */
										if (hs_sel_offset[k - 1] == dims[k - 1])
											hs_sel_offset[k - 1] = 0;
										else
											carry = 0;
									} /* k */
								} /* elmtno */

								H5Sclose(hslab_space);
								/* free */
								if (hslab_buf != NULL) {
									HDfree(hslab_buf);
									hslab_buf = NULL;
								}
							} /* hyperslab read */
						} /* if (nelmts>0 && space_status==H5D_SPACE_STATUS_NOT_ALLOCATED) */

						/*-------------------------------------------------------------------------
						 * amount of compression used
						 *-------------------------------------------------------------------------
						 */
						if (options->verbose) {
							double ratio = 0;

							/* only print the compression ration if there was a filter request */
							if (apply_s && apply_f && req_filter) {
								/* get the storage size of the output dataset */
								dsize_out = H5Dget_storage_size(dset_out);

								/* compression ratio = uncompressed size /  compressed size */
								if (dsize_out != 0)
									ratio = (double) dsize_in
											/ (double) dsize_out;

								print_dataset_info(dcpl_out,
										travt->objs[i].name, ratio, 1);
							}
							else
								print_dataset_info(dcpl_in, travt->objs[i].name,
										ratio, 0);

							/* print a message that the filter was not applied
							 (in case there was a filter)
							 */
							if (has_filter && apply_s == 0)
								printf(
										" <warning: filter not applied to %s. dataset smaller than %d bytes>\n",
										travt->objs[i].name,
										(int) options->min_comp);

							if (has_filter && apply_f == 0)
								printf(
										" <warning: could not apply the filter to %s>\n",
										travt->objs[i].name);

						} /* verbose */

						/*-------------------------------------------------------------------------
						 * copy attrs
						 *-------------------------------------------------------------------------
						 */
						if (copy_attr(dset_in, dset_out, &named_dt_head, travt,
								options) < 0)
							goto error;
						/*close */
						if (H5Dclose(dset_out) < 0)
							goto error;

					}/*!H5T_REFERENCE*/
				}/*h5tools_canreadf*/

				/*-------------------------------------------------------------------------
				 * close
				 *-------------------------------------------------------------------------
				 */
				if (H5Tclose(ftype_id) < 0)
					goto error;
				if (H5Tclose(wtype_id) < 0)
					goto error;
				if (H5Pclose(dcpl_in) < 0)
					goto error;
				if (H5Pclose(dcpl_out) < 0)
					goto error;
				if (H5Sclose(f_space_id) < 0)
					goto error;
				if (H5Dclose(dset_in) < 0)
					goto error;

			}
			/*-------------------------------------------------------------------------
			 * we do not have request for filter/chunking use H5Ocopy instead
			 *-------------------------------------------------------------------------
			 */
			else {
				hid_t pid;

				/* create property to pass copy options */
				if ((pid = H5Pcreate(H5P_OBJECT_COPY)) < 0)
					goto error;

				/* set options for object copy */
				if (H5Pset_copy_object(pid, H5O_COPY_WITHOUT_ATTR_FLAG) < 0)
					goto error;

				/*-------------------------------------------------------------------------
				 * do the copy
				 *-------------------------------------------------------------------------
				 */

				if (H5Ocopy(fidin, /* Source file or group identifier */
				travt->objs[i].name, /* Name of the source object to be copied */
				fidout, /* Destination file or group identifier  */
				travt->objs[i].name, /* Name of the destination object  */
				pid, /* Properties which apply to the copy   */
				H5P_DEFAULT) < 0) /* Properties which apply to the new hard link */
					goto error;

				/* close property */
				if (H5Pclose(pid) < 0)
					goto error;

				/*-------------------------------------------------------------------------
				 * copy attrs manually
				 *-------------------------------------------------------------------------
				 */
				if ((dset_in = H5Dopen2(fidin, travt->objs[i].name, H5P_DEFAULT))
						< 0)
					goto error;
				if ((dset_out = H5Dopen2(fidout, travt->objs[i].name,
						H5P_DEFAULT)) < 0)
					goto error;
				if (copy_attr(dset_in, dset_out, &named_dt_head, travt, options)
						< 0)
					goto error;
				if (H5Dclose(dset_in) < 0)
					goto error;
				if (H5Dclose(dset_out) < 0)
					goto error;

				if (options->verbose)
					printf(FORMAT_OBJ, "dset", travt->objs[i].name);

			} /* end do we have request for filter/chunking */

			break;

			/*-------------------------------------------------------------------------
			 * H5TRAV_TYPE_NAMED_DATATYPE
			 *-------------------------------------------------------------------------
			 */
		case H5TRAV_TYPE_NAMED_DATATYPE:

			if (options->verbose)
				printf(FORMAT_OBJ, "type", travt->objs[i].name);

			if ((type_in = H5Topen2(fidin, travt->objs[i].name, H5P_DEFAULT))
					< 0)
				goto error;

			/* Copy the datatype anonymously */
			if ((type_out = copy_named_datatype(type_in, fidout, &named_dt_head,
					travt, options)) < 0)
				goto error;

			/* Link in to group structure */
			if (H5Lcreate_hard(type_out, ".", fidout, travt->objs[i].name,
			H5P_DEFAULT, H5P_DEFAULT) < 0)
				goto error;

			/*-------------------------------------------------------------------------
			 * copy attrs
			 *-------------------------------------------------------------------------
			 */
			if (copy_attr(type_in, type_out, &named_dt_head, travt, options)
					< 0)
				goto error;

			if (H5Tclose(type_in) < 0)
				goto error;
			if (H5Tclose(type_out) < 0)
				goto error;

			break;

			/*-------------------------------------------------------------------------
			 * H5TRAV_TYPE_LINK
			 * H5TRAV_TYPE_UDLINK
			 *
			 * Only handles external links; H5Lcopy will fail for other UD link types
			 * since we don't have creation or copy callbacks for them.
			 *-------------------------------------------------------------------------
			 */

		case H5TRAV_TYPE_LINK:
		case H5TRAV_TYPE_UDLINK: {

			if (options->verbose)
				printf(FORMAT_OBJ, "link", travt->objs[i].name);

			if (H5Lcopy(fidin, travt->objs[i].name, fidout, travt->objs[i].name,
					H5P_DEFAULT, H5P_DEFAULT) < 0)
				goto error;

			if (options->verbose)
				printf(FORMAT_OBJ, "link", travt->objs[i].name);

		}
			break;

		default:
			goto error;
		} /* switch */

		/* free */
		if (buf != NULL) {
			HDfree(buf);
			buf = NULL;
		}

	} /* i */

	/* Finalize (link) the stack of named datatypes (if any) */
	named_datatype_free(&named_dt_head, 0);

	return 0;

error:
	H5E_BEGIN_TRY
		{
			H5Gclose(grp_in);
			H5Gclose(grp_out);
			H5Pclose(dcpl_in);
			H5Pclose(gcpl_in);
			H5Pclose(gcpl_out);
			H5Sclose(f_space_id);
			H5Dclose(dset_in);
			H5Dclose(dset_out);
			H5Tclose(ftype_id);
			H5Tclose(wtype_id);
			H5Tclose(type_in);
			H5Tclose(type_out);
			named_datatype_free(&named_dt_head, 1);
		}H5E_END_TRY;
	/* free */
	if (buf != NULL)
		HDfree(buf);
	if (hslab_buf != NULL)
		HDfree(hslab_buf);
	return -1;
}

/*-------------------------------------------------------------------------
 * Function: print_dataset_info
 *
 * Purpose: print name, filters, percentage compression of a dataset
 *
 *-------------------------------------------------------------------------
 */
static void print_dataset_info(hid_t dcpl_id, char *objname, double ratio,
		int pr) {
	char strfilter[255];
#if defined (PRINT_DEBUG )
	char temp[255];
#endif
	int nfilters; /* number of filters */
	unsigned filt_flags; /* filter flags */
	H5Z_filter_t filtn; /* filter identification number */
	unsigned cd_values[20]; /* filter client data values */
	size_t cd_nelmts; /* filter client number of values */
	char f_objname[256]; /* filter objname */
	int i;

	HDstrcpy(strfilter, "\0");

	/* get information about input filters */
	if ((nfilters = H5Pget_nfilters(dcpl_id)) < 0)
		return;

	for (i = 0; i < nfilters; i++) {
		cd_nelmts = NELMTS(cd_values);

		filtn = H5Pget_filter2(dcpl_id, (unsigned) i, &filt_flags, &cd_nelmts,
				cd_values, sizeof(f_objname), f_objname, NULL);

		switch (filtn) {

		case H5Z_FILTER_NONE:
			HDstrcat(strfilter, "NONE ");
			break;

		case H5Z_FILTER_DEFLATE:
			HDstrcat(strfilter, "GZIP ");

#if defined (PRINT_DEBUG)
			{
				unsigned level=cd_values[0];
				sprintf(temp,"(%d)",level);
				HDstrcat(strfilter,temp);
			}
#endif
			break;

		case H5Z_FILTER_SZIP:
			HDstrcat(strfilter, "SZIP ");

#if defined (PRINT_DEBUG)
			{
				unsigned options_mask=cd_values[0]; /* from dcpl, not filt*/
				unsigned ppb=cd_values[1];
				sprintf(temp,"(%d,",ppb);
				HDstrcat(strfilter,temp);
				if (options_mask & H5_SZIP_EC_OPTION_MASK)
				HDstrcpy(temp,"EC) ");
				else if (options_mask & H5_SZIP_NN_OPTION_MASK)
				HDstrcpy(temp,"NN) ");
			}
			HDstrcat(strfilter,temp);

#endif

			break;

		case H5Z_FILTER_SHUFFLE:
			HDstrcat(strfilter, "SHUF ");
			break;

		case H5Z_FILTER_FLETCHER32:
			HDstrcat(strfilter, "FLET ");
			break;

		case H5Z_FILTER_NBIT:
			HDstrcat(strfilter, "NBIT ");
			break;

		case H5Z_FILTER_SCALEOFFSET:
			HDstrcat(strfilter, "SCALEOFFSET ");
			break;

		default:
			HDstrcat(strfilter, "UD ");
			break;
		} /* switch */
	}/*i*/

	if (!pr)
		printf(FORMAT_OBJ, "dset", objname);
	else {
		char str[255], temp[28];
		HDstrcpy(str, "dset     ");
		HDstrcat(str, strfilter);
		sprintf(temp, "  (%.3f:1)", ratio);
		HDstrcat(str, temp);
		printf(FORMAT_OBJ, str, objname);
	}
}

/*-------------------------------------------------------------------------
 * Function: copy_user_block
 *
 * Purpose: copy user block from one file to another
 *
 * Return: 0, ok, -1 no
 *
 * Programmer: Peter Cao
 *
 * Date: October, 25, 2007
 *
 *-------------------------------------------------------------------------
 */
static int copy_user_block(const char *infile, const char *outfile,
		hsize_t size) {
	int infid = -1, outfid = -1; /* File descriptors */
	int status = 0; /* Return value */

	/* User block must be any power of 2 equal to 512 or greater (512, 1024, 2048, etc.) */
	HDassert(size > 0);

	/* Open files */
	if ((infid = HDopen(infile, O_RDONLY, 0)) < 0) {
		status = -1;
		goto done;
	}
	if ((outfid = HDopen(outfile, O_WRONLY, 0644)) < 0) {
		status = -1;
		goto done;
	}

	/* Copy the userblock from the input file to the output file */
	while (size > 0) {
		ssize_t nread, nbytes; /* # of bytes transfered, etc. */
		char rbuf[USERBLOCK_XFER_SIZE]; /* Buffer for reading */
		const char *wbuf; /* Pointer into buffer, for writing */

		/* Read buffer from source file */
		if (size > USERBLOCK_XFER_SIZE)
			nread = HDread(infid, rbuf, (size_t)USERBLOCK_XFER_SIZE);
		else
			nread = HDread(infid, rbuf, (size_t)size);
		if (nread < 0) {
			status = -1;
			goto done;
		} /* end if */

		/* Write buffer to destination file */
		/* (compensating for interrupted writes & checking for errors, etc.) */
		nbytes = nread;
		wbuf = rbuf;
		while (nbytes > 0) {
			ssize_t nwritten; /* # of bytes written */

			do {
				nwritten = HDwrite(outfid, wbuf, (size_t)nbytes);
			} while (-1 == nwritten && EINTR == errno);
			if (-1 == nwritten) { /* error */
				status = -1;
				goto done;
			} /* end if */
			HDassert(nwritten > 0);
			HDassert(nwritten <= nbytes);

			/* Update # of bytes left & offset in buffer */
			nbytes -= nwritten;
			wbuf += nwritten;
			HDassert(nbytes == 0 || wbuf < (rbuf + USERBLOCK_XFER_SIZE));
		} /* end while */

		/* Update size of userblock left to transfer */
		size = size - (hsize_t) nread;
	} /* end while */

done:
	if (infid > 0)
		HDclose(infid);
	if (outfid > 0)
		HDclose(outfid);

	return status;
}

/*-------------------------------------------------------------------------
 * Function: print_user_block
 *
 * Purpose: print user block
 *
 * Return: 0, ok, -1 no
 *
 * Programmer: Pedro Vicente
 *
 * Date: August, 20, 2008
 *
 *-------------------------------------------------------------------------
 */
#if defined (H5REPACK_DEBUG_USER_BLOCK)
static
void print_user_block(const char *filename, hid_t fid)
{
	int fh; /* file handle  */
	hsize_t ub_size; /* user block size */
	hsize_t size; /* size read */
	hid_t fcpl; /* file creation property list ID for HDF5 file */
	int i;

	/* get user block size */
	if(( fcpl = H5Fget_create_plist(fid)) < 0) {
		error_msg("failed to retrieve file creation property list\n");
		goto done;
	}

	if(H5Pget_userblock(fcpl, &ub_size) < 0) {
		error_msg("failed to retrieve userblock size\n");
		goto done;
	}

	if(H5Pclose(fcpl) < 0) {
		error_msg("failed to close property list\n");
		goto done;
	}

	/* open file */
	if((fh = HDopen(filename, O_RDONLY, 0)) < 0) {
		goto done;
	}

	size = ub_size;

	/* read file */
	while(size > 0) {
		ssize_t nread; /* # of bytes read */
		char rbuf[USERBLOCK_XFER_SIZE]; /* buffer for reading */

		/* read buffer */
		if(size > USERBLOCK_XFER_SIZE)
			nread = HDread(fh, rbuf, (size_t)USERBLOCK_XFER_SIZE);
		else
			nread = HDread(fh, rbuf, (size_t)size);

		for(i = 0; i < nread; i++) {

			printf("%c ", rbuf[i]);

		}
		printf("\n");

		if(nread < 0) {
			goto done;
		}

		/* update size of userblock left to transfer */
		size -= nread;
	}

done:
	if(fh > 0)
		HDclose(fh);

	return;
}
#endif

