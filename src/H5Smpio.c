/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  rky 980813
 *
 * Purpose:	Functions to read/write directly between app buffer and file.
 *
 * 		Beware of the ifdef'ed print statements.
 *		I didn't make them portable.
 */

#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Sprivate.h>

#ifndef HAVE_PARALLEL
/* 
 * The H5S_mpio_xxxx functions are for parallel I/O only and are
 * valid only when HAVE_PARALLEL is #defined.  This empty #ifndef
 * body is used to allow this source file be included in the serial
 * distribution.
 * Some compilers/linkers may complain about "empty" object file.
 * If that happens, uncomment the following statement to pacify
 * them.
 */
/* const hbool_t H5S_mpio_avail = FALSE; */
#else /* HAVE_PARALLEL */
/* Interface initialization */
#define PABLO_MASK      H5S_all_mask
#define INTERFACE_INIT  NULL
static intn             interface_initialize_g = FALSE;

static herr_t
H5S_mpio_all_type( const H5S_t *space, const size_t elmt_size,
		     /* out: */
		     MPI_Datatype *new_type,
		     hsize_t *count,
		     hbool_t *is_derived_type );
static herr_t
H5S_mpio_hyper_type( const H5S_t *space, const size_t elmt_size,
		     /* out: */
		     MPI_Datatype *new_type,
		     hsize_t *count,
		     hbool_t *is_derived_type );
static herr_t
H5S_mpio_space_type( const H5S_t *space, const size_t elmt_size,
		     /* out: */
		     MPI_Datatype *new_type,
		     hsize_t *count,
		     hbool_t *is_derived_type );
static herr_t
H5S_mpio_spaces_xfer (H5F_t *f, const struct H5O_layout_t *layout,
                     const struct H5O_pline_t __unused__ *pline,
                     const struct H5O_efl_t __unused__ *efl, size_t elmt_size,
                     const H5S_t *file_space, const H5S_t *mem_space,
                     const H5D_transfer_t xfer_mode, void *buf /*out*/,
		     hbool_t *must_convert /*out*/,
		     const hbool_t do_write );

/*-------------------------------------------------------------------------
 * Function:	H5S_mpio_all_type
 *
 * Purpose:	Translate an HDF5 "all" selection into an MPI type.
 *
 * Return:	non-negative on success, negative on failure.
 *
 * Outputs:	*new_type	  the MPI type corresponding to the selection
 *		*count		  how many objects of the new_type in selection
 *				  (useful if this is the buffer type for xfer)
 *		*is_derived_type  0 if MPI primitive type, 1 if derived
 *
 * Programmer:	rky 980813
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_mpio_all_type( const H5S_t *space, const size_t elmt_size,
		     /* out: */
		     MPI_Datatype *new_type,
		     hsize_t *count,
		     hbool_t *is_derived_type )
{
    hsize_t	total_bytes;
    int		i;

    FUNC_ENTER (H5S_mpio_all_type, FAIL);

    /* Check args */
    assert (space);

    /* Just treat the entire extent as a block of bytes */
    total_bytes = (hsize_t)elmt_size;
    for (i=0; i<space->extent.u.simple.rank; ++i) {
	total_bytes *= space->extent.u.simple.size[i];
    }

    /* fill in the return values */
    *new_type = MPI_BYTE;
    *count = total_bytes;
    *is_derived_type = 0;

#ifdef H5Smpi_DEBUG
    fprintf(stdout, "Leave %s total_bytes=%lld\n", FUNC, (long long)total_bytes );
#endif
    FUNC_LEAVE (SUCCEED);
} /* H5S_mpio_all_type() */

/*-------------------------------------------------------------------------
 * Function:	H5S_mpio_hyper_type
 *
 * Purpose:	Translate an HDF5 hyperslab selection into an MPI type.
 *
 * Return:	non-negative on success, negative on failure.
 *
 * Outputs:	*new_type	  the MPI type corresponding to the selection
 *		*count		  how many objects of the new_type in selection
 *				  (useful if this is the buffer type for xfer)
 *		*is_derived_type  0 if MPI primitive type, 1 if derived
 *
 * Programmer:	rky 980813
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_mpio_hyper_type( const H5S_t *space, const size_t elmt_size,
		     /* out: */
		     MPI_Datatype *new_type,
		     hsize_t *count,
		     hbool_t *is_derived_type )
{
    struct dim {	/* less hassle than malloc/free & ilk */
	hssize_t start;
	hsize_t strid;
	hsize_t block;
	hsize_t xtent;
	hsize_t count;
    } d[32];

    int			i, err, new_rank, num_to_collapse;
    H5S_hyper_dim_t	*diminfo;		/* [rank] */
    intn		rank;
    MPI_Datatype	inner_type, outer_type;
    MPI_Aint		s[2];  /* array of displacements for struct type */
    MPI_Aint		extent_len, start_Aint;	/* for calculating d[1] */

    FUNC_ENTER (H5S_mpio_hyper_type, FAIL);

    /* Check and abbreviate args */
    assert (space);
    diminfo = space->select.sel_info.hyper.diminfo;
    assert (diminfo);
    rank = space->extent.u.simple.rank;
    assert (rank >= 0);

    /* make a local copy of the dimension info so we can transform them */
    assert(rank<=32);	/* within array bounds */
    for ( i=0; i<rank; ++i) {
	d[i].start = diminfo[i].start;
	d[i].strid = diminfo[i].stride;
	d[i].block = diminfo[i].block;
	d[i].count = diminfo[i].count;
	d[i].xtent = space->extent.u.simple.size[i];
#ifdef H5Smpi_DEBUG
	fprintf(stdout,
	    "hyper_type: start=%lld  stride=%lld  count=%lld  block=%lld  xtent=%lld",
	    (long long)d[i].start, (long long)d[i].strid, (long long)d[i].count, (long long)d[i].block, (long long)d[i].xtent );
	if (i==0) fprintf(stdout, "  rank=%d\n", rank );
	else	  fprintf(stdout, "\n" );
#endif
    }

    /*  Create a type covering the selected hyperslab.
     *  Multidimensional dataspaces are stored in row-major order.
     *  The type is built from the inside out, going from the
     *  fastest-changing (i.e., inner) dimension * to the slowest (outer). */

    /*  Optimization: check for contiguous inner dimensions.
     *  Supposing the dimensions were numbered from 1 to rank, we find that
     *
     *  dim d=rank is contiguous if:
     *                  stride[d] = block[d]
     *   and  count[d] * block[d] = extent.u.simple.size[d]
     *
     *  (i.e., there's no overlap or gaps and the entire extent is filled.)
     *
     * dim d (1<=d<rank) is contiguous if:
     *                    dim d+1 is contiguous
     *   and            stride[d] = block[d]
     *   and  count[d] * block[d] = extent.u.simple.size[d]
     *
     *  There is also a weak sense in which the first noncollapsible dim
     *  is contiguous if it consists of a single unbroken range,
     *  and we also take advantage of that.
     */

    /* figure out how many dimensions we can eliminate */
    /* This loop examines contiguity from the inside out. */
    for ( i=0; i<rank; ++i) {
    	if ((d[rank-i-1].strid != d[rank-i-1].block)
		||
	    (d[rank-i-1].count*d[rank-i-1].block) != space->extent.u.simple.size[rank-i-1]) {
	    break;
	}
    } /* end for */
    num_to_collapse = i;

    num_to_collapse = 0; /* rky 980827 DEBUG Temporary change to prevent
			    collapsing dims until further testing of collapse */

    assert(0<=num_to_collapse && num_to_collapse<rank);
    new_rank = rank - num_to_collapse;
#ifdef H5Smpi_DEBUG
    fprintf(stdout, "hyper_type: new_rank=%d\n", new_rank );
#endif

    /* To collapse dims, just transform dimension info (from inner to outer) */
    for (i=rank-1; i>=new_rank; --i) {
	d[i-1].block *= d[i].strid;
	d[i-1].strid *= d[i].strid;
	d[i-1].xtent *= d[i].strid;
	assert( d[i].start == 0 );
	/* d[i-1].start stays unchanged */
	/* d[i-1].count stays unchanged */
    }

    /* check for possibility to coalesce blocks of the uncoalesced dimensions */
    for (i=0; i<new_rank; ++i) {
        if (d[i].strid == d[i].block) {
	    /* transform smaller blocks to 1 larger block of combined size */
	    d[i].strid = d[i].block *= d[i].count;
	    d[i].count = 1;
	}
    }

    /* initialize induction variables */
    s[0] = 0;			/* stays constant */
    /* create contig type for inner contig dims */
#ifdef H5Smpi_DEBUG
    fprintf(stdout, "hyper_type: Making contig type %d MPI_BYTEs\n", elmt_size );
#endif
    err = MPI_Type_contiguous( (int)elmt_size, MPI_BYTE, &inner_type );
    if (err) {
	HRETURN_ERROR(H5E_DATASPACE, H5E_MPI, FAIL,"couldn't create MPI contiguous type");
    }

    /* construct the type by walking the hyperslab dims from the inside out */
    for ( i=new_rank-1; i>=0; --i) {
#ifdef H5Smpi_DEBUG
	fprintf(stdout, "hyper_type: i=%d Making vector type\n count=%lld block=%lld stride=%lld\n", i, (long long)d[i].count, (long long)d[i].block, (long long)d[i].strid );
#endif
	err = MPI_Type_vector(	(int)(d[i].count),	/* count */
				(int)(d[i].block),	/* blocklength */
				(MPI_Aint)(d[i].strid),	/* stride */
				inner_type,		/* old type */
				&outer_type );		/* new type */
	if (err) {
	    MPI_Type_free( &inner_type );	/* free before abort */
    	    HRETURN_ERROR(H5E_DATASPACE, H5E_MPI, FAIL,"couldn't create MPI vector type");
	}
	/* from here to end of loop, inner_type actually will get the value
	 * of the outermost type: it will be inner for the next iteration  */
	if (0 == d[i].start) {
	    /* don't need to compensate for the start displacement */
	    MPI_Type_free( &inner_type ); /* old inner no longer needed */
	    inner_type = outer_type;	  /* prepare for next iter */
	} else {
	    /* need to compensate for the start displacement */
	    int		 b[2];  /* array of rep counts */
	    MPI_Datatype t[2];  /* array of MPI types */

	    /* fill in the b, d, and t arrays, length is 2 */
	    /* b gives rep count for each type in t */
	    b[0] = 1;
	    b[1] = 1;

	    /* s gives the byte displacement for each "field" by induction:
	     * for 0<=i<rank-1  s[1]_i = start[i]*extent_length[i+1];
	     * with base case   s[1]_(rank-1) = elmt_size    (i decreasing).
	     * (Assuming dimension index increases as we go deeper in.)
	     * Note that in this loop, extent_length[i+1] is the extent length
	     * of the inner type (i.e., the type constructed in previous trip).
	     */
	    err = MPI_Type_extent( inner_type, &extent_len );
	    if (err) {
		MPI_Type_free( &inner_type ); /* free before abort */
		MPI_Type_free( &outer_type ); /* free before abort */
		HRETURN_ERROR(H5E_DATASPACE, H5E_MPI, FAIL,
				"couldn't get extent of MPI type");
	    }
	    start_Aint = (MPI_Aint)(d[i].start);
	    if (start_Aint != d[i].start) {
		MPI_Type_free( &inner_type ); /* free before abort */
		MPI_Type_free( &outer_type ); /* free before abort */
		HRETURN_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL,
				"start value overflows MPI_Aint");
	    }
	    s[1] = start_Aint * extent_len;

	    /* t gives the MPI types for the "fields" */
	    /* I think we could do without the LB type and just have
	     * one "field" consisting of the vector type with displacement,
	     * but I guess there's no harm in doing it this way. */
	    t[0] = MPI_LB;
	    t[1] = outer_type;	/* the just-created vector type */
	    /* Create new struct type to compensate for start displacement.
	     * The struct's first "field" is the displacement,
	     * and its second "field" is the just-created vector type */
#ifdef H5Smpi_DEBUG
	    fprintf(stdout, "hyper_type: i=%d Making struct type\n b[1]=%d d[1]=%lld\n", i, b[1], (long long)s[1] );
#endif
	    err = MPI_Type_struct( 2, b, s, t, &inner_type/*becomes outer*/ );
	    MPI_Type_free( &outer_type );	/* no longer needed */
	    if (err) {
		HRETURN_ERROR(H5E_DATASPACE, H5E_MPI, FAIL,"couldn't create MPI struct type");
	    }
	} /* end else */
	/* at this point, inner_type is actually the outermost type */
    } /* end for */

    /* here inner_type is actually the outermost type, even for 0-trip loop */
    *new_type = inner_type;		/* return the just-constructed type */
    err = MPI_Type_commit( new_type );
    if (err) {
	HRETURN_ERROR(H5E_DATASPACE, H5E_MPI, FAIL,"couldn't commit MPI vector type");
    }

    /* fill in the remaining return values */
    *count = 1;			/* only have to move one of these suckers! */
    *is_derived_type = 1;

#ifdef H5Smpi_DEBUG
    fprintf(stdout, "Leave %s\n", FUNC );
#endif
    FUNC_LEAVE (SUCCEED);
} /* H5S_mpio_hyper_type() */

/*-------------------------------------------------------------------------
 * Function:	H5S_mpio_space_type
 *
 * Purpose:	Translate an HDF5 dataspace selection into an MPI type.
 *		Currently handle only hyperslab and "all" selections.
 *
 * Return:	non-negative on success, negative on failure.
 *
 * Outputs:	*new_type	  the MPI type corresponding to the selection
 *		*count		  how many objects of the new_type in selection
 *				  (useful if this is the buffer type for xfer)
 *		*is_derived_type  0 if MPI primitive type, 1 if derived
 *
 * Programmer:	rky 980813
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_mpio_space_type( const H5S_t *space, const size_t elmt_size,
		     /* out: */
		     MPI_Datatype *new_type,
		     hsize_t *count,
		     hbool_t *is_derived_type )
{
    int		err;
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER (H5S_mpio_space_type, FAIL);

    /* Check args */
    assert (space);

    /* Creat MPI type based on the kind of selection */
    switch (space->extent.type) {
        case H5S_SCALAR:
            /* not yet implemented */
	    ret_value = FAIL;
            break;

        case H5S_SIMPLE:
            switch(space->select.type) {
                case H5S_SEL_NONE:
                case H5S_SEL_ALL:
		    err = H5S_mpio_all_type( space, elmt_size,
				/* out: */ new_type, count, is_derived_type );
		    if (err<0) {
			HRETURN_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL,"couldn't convert \"all\" selection to MPI type");
		    }
                    break;

                case H5S_SEL_POINTS:
		    /* not yet implemented */
		    ret_value = FAIL;
                    break;

                case H5S_SEL_HYPERSLABS:
		    err = H5S_mpio_hyper_type( space, elmt_size,
				/* out: */ new_type, count, is_derived_type );
		    if (err) {
			HRETURN_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL,"couldn't convert \"all\" selection to MPI type");
		    }
                    break;

                default:
                    assert("unknown selection type" && 0);
                    break;
            } /* end switch */
            break;

        case H5S_COMPLEX:
            /* not yet implemented */
	    ret_value = FAIL;
            break;

        default:
            assert("unknown data space type" && 0);
            break;
    }

    FUNC_LEAVE (ret_value);
} /* H5S_mpio_space_type() */

/*-------------------------------------------------------------------------
 * Function:	H5S_mpio_spaces_xfer
 *
 * Purpose:	Use MPI-IO to transfer data efficiently
 *		directly between app buffer and file.
 *
 * Return:	non-negative on success, negative on failure.
 *
 * Programmer:	rky 980813
 *
 * Modifications:
 *
 * rky 980918
 * Added must_convert parameter to let caller know we can't optimize the xfer.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_mpio_spaces_xfer (H5F_t *f, const struct H5O_layout_t *layout,
                     const struct H5O_pline_t __unused__ *pline,
                     const struct H5O_efl_t __unused__ *efl, size_t elmt_size,
                     const H5S_t *file_space, const H5S_t *mem_space,
                     const H5D_transfer_t xfer_mode, void *buf /*out*/,
		     hbool_t *must_convert /*out*/,
		     const hbool_t do_write )
{
    herr_t	 ret_value = SUCCEED;
    int		 err;
    haddr_t	 disp, addr;
    size_t	 mpi_count;
    hsize_t	 mpi_buf_count, mpi_unused_count;
    MPI_Datatype mpi_buf_type, mpi_file_type;
    hbool_t	 mbt_is_derived, mft_is_derived;

    FUNC_ENTER (H5S_mpio_spaces_xfer, FAIL);

    *must_convert = 0;	/* means we at least tried to do optimized xfer */

    /* Check args */
    assert (f);
    assert (layout);
    assert (file_space);
    assert (mem_space);
    assert (buf);
    assert (f->shared->access_parms->driver == H5F_LOW_MPIO);

    /* INCOMPLETE!!!  rky 980816 */
    /* Currently can only handle H5D_CONTIGUOUS layout */
    if (layout->type != H5D_CONTIGUOUS) {
#ifdef H5S_DEBUG
	if (H5DEBUG(S)) {
            fprintf (H5DEBUG(S), "H5S: can only create MPI datatype for hyperslab when layout is contiguous.\n" );
	}
#endif
	*must_convert = 1;      /* can't do optimized xfer; do the old way */
	goto done;
    }

    /* create the MPI buffer type */
    err = H5S_mpio_space_type( mem_space, elmt_size,
			       /* out: */
			       &mpi_buf_type,
			       &mpi_buf_count,
			       &mbt_is_derived );
    if (MPI_SUCCESS != err)
    	HRETURN_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL,"couldn't create MPI buf type");
    /* pass the buf type to low-level write via access_parms */
    f->shared->access_parms->u.mpio.btype = mpi_buf_type;

    /* create the MPI file type */
    err = H5S_mpio_space_type( file_space, elmt_size,
			       /* out: */
			       &mpi_file_type,
			       &mpi_unused_count,
			       &mft_is_derived );
    if (MPI_SUCCESS != err)
    	HRETURN_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL,"couldn't create MPI file type");
    /* pass the file type to low-level write via access_parms */
    f->shared->access_parms->u.mpio.ftype = mpi_file_type;

    /* calculate the absolute base addr (i.e., the file view disp) */
    disp = f->shared->base_addr;
    H5F_addr_add( &disp, &(layout->addr) );
    f->shared->access_parms->u.mpio.disp = disp;
#ifdef H5Smpi_DEBUG
    fprintf(stdout, "spaces_xfer: disp=%lld\n", (long long)disp.offset );
#endif

    /* Effective address determined by base addr and the MPI file type */
    H5F_addr_reset( &addr );	/* set to 0 */

    /* request a dataspace xfer (instead of an elementary byteblock xfer) */
    f->shared->access_parms->u.mpio.use_types = 1;

    /* transfer the data */
    mpi_count = (size_t)mpi_buf_count;
    if (mpi_count != mpi_buf_count)
    	HRETURN_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL,"transfer size overflows size_t");
    if (do_write) {
    	err = H5F_low_write( f->shared->lf, f->shared->access_parms,
			     xfer_mode, &addr, mpi_count, buf );
    	if (err) {
	    HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL,"MPI write failed");
	}
    } else {
    	err = H5F_low_read ( f->shared->lf, f->shared->access_parms,
			     xfer_mode, &addr, mpi_count, buf );
    	if (err) {
	    HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL,"MPI read failed");
	}
    }

    /* free the MPI buf and file types */
    if (mbt_is_derived) {
	err = MPI_Type_free( &mpi_buf_type );
	if (MPI_SUCCESS != err) {
    	    HRETURN_ERROR(H5E_DATASPACE, H5E_MPI, FAIL,"couldn't free MPI file type");
	}
    }
    if (mft_is_derived) {
	err = MPI_Type_free( &mpi_file_type );
	if (MPI_SUCCESS != err) {
    	    HRETURN_ERROR(H5E_DATASPACE, H5E_MPI, FAIL,"couldn't free MPI file type");
	}
    }

    done:
    FUNC_LEAVE (ret_value);
} /* H5S_mpio_spaces_xfer() */

/*-------------------------------------------------------------------------
 * Function:	H5S_mpio_spaces_read
 *
 * Purpose:	MPI-IO function to read directly from app buffer to file.
 *
 * Return:	non-negative on success, negative on failure.
 *
 * Programmer:	rky 980813
 *
 * Modifications:
 *
 * rky 980918
 * Added must_convert parameter to let caller know we can't optimize the xfer.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_mpio_spaces_read (H5F_t *f, const struct H5O_layout_t *layout,
                     const struct H5O_pline_t *pline,
                     const struct H5O_efl_t *efl, size_t elmt_size,
                     const H5S_t *file_space, const H5S_t *mem_space,
                     const H5D_transfer_t xfer_mode, void *buf /*out*/,
		     hbool_t *must_convert /*out*/ )
{
    herr_t ret_value = FAIL;

    FUNC_ENTER (H5S_mpio_spaces_read, FAIL);

    ret_value = H5S_mpio_spaces_xfer( f, layout, pline, efl, elmt_size,
			file_space, mem_space, xfer_mode, (void*)buf,
			must_convert /*out*/, 0 /*read*/ );

    FUNC_LEAVE (ret_value);
} /* H5S_mpio_spaces_read() */

/*-------------------------------------------------------------------------
 * Function:	H5S_mpio_spaces_write
 *
 * Purpose:	MPI-IO function to write directly from app buffer to file.
 *
 * Return:	non-negative on success, negative on failure.
 *
 * Programmer:	rky 980813
 *
 * Modifications:
 *
 * rky 980918
 * Added must_convert parameter to let caller know we can't optimize the xfer.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_mpio_spaces_write(H5F_t *f, const struct H5O_layout_t *layout,
                     const struct H5O_pline_t *pline,
                     const struct H5O_efl_t *efl, size_t elmt_size,
                     const H5S_t *file_space, const H5S_t *mem_space,
                     const H5D_transfer_t xfer_mode, const void *buf,
		     hbool_t *must_convert /*out*/ )
{
    herr_t ret_value = FAIL;

    FUNC_ENTER (H5S_mpio_spaces_write, FAIL);

    ret_value = H5S_mpio_spaces_xfer( f, layout, pline, efl, elmt_size,
			file_space, mem_space, xfer_mode, (void*)buf,
			must_convert /*out*/, 1 /*write*/ );

    FUNC_LEAVE (ret_value);
} /* H5S_mpio_spaces_write() */

#endif  /* HAVE_PARALLEL */
