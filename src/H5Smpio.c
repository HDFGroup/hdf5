/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  rky 980813
 *
 * Purpose:	Functions to read/write directly between app buffer and file.
 *
 * 		Beware of the ifdef'ed print statements.
 *		I didn't make them portable.
 */

#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */
#define H5S_PACKAGE		/*suppress error about including H5Spkg	  */

#include "H5private.h"          /* Internal types, etc. */
#include "H5Eprivate.h"         /* Error reporting */
#include "H5Fpkg.h"             /* Ugly, but necessary for the MPIO I/O accesses */
#include "H5FDmpio.h"		/* MPIO file driver			*/
#include "H5FDprivate.h"        /* Necessary for the H5FD_write & H5FD_read prototypes.. */
#include "H5Iprivate.h"		/* Object IDs */
#include "H5Pprivate.h"		/* Property Lists */
#include "H5Spkg.h"             /* Dataspaces */

#ifndef H5_HAVE_PARALLEL
/* 
 * The H5S_mpio_xxxx functions are for parallel I/O only and are
 * valid only when H5_HAVE_PARALLEL is #defined.  This empty #ifndef
 * body is used to allow this source file be included in the serial
 * distribution.
 * Some compilers/linkers may complain about "empty" object file.
 * If that happens, uncomment the following statement to pacify
 * them.
 */
/* const hbool_t H5S_mpio_avail = FALSE; */
#else /* H5_HAVE_PARALLEL */
/* Interface initialization */
#define PABLO_MASK      H5Sall_mask
#define INTERFACE_INIT  NULL
static int             interface_initialize_g = 0;

static herr_t
H5S_mpio_all_type( const H5S_t *space, size_t elmt_size,
		     /* out: */
		     MPI_Datatype *new_type,
		     size_t *count,
		     hsize_t *extra_offset,
		     hbool_t *is_derived_type );
static herr_t
H5S_mpio_none_type( const H5S_t *space, size_t elmt_size,
		     /* out: */
		     MPI_Datatype *new_type,
		     size_t *count,
		     hsize_t *extra_offset,
		     hbool_t *is_derived_type );
static herr_t
H5S_mpio_hyper_type( const H5S_t *space, size_t elmt_size,
		     /* out: */
		     MPI_Datatype *new_type,
		     size_t *count,
		     hsize_t *extra_offset,
		     hbool_t *is_derived_type );
static herr_t
H5S_mpio_hyper_contig_type( const H5S_t *space, size_t elmt_size,
		     /* out: */
		     MPI_Datatype *new_type,
		     size_t *count,
		     hsize_t *extra_offset,
		     hbool_t *is_derived_type );
static herr_t
H5S_mpio_space_type( const H5S_t *space, size_t elmt_size,
		     /* out: */
		     MPI_Datatype *new_type,
		     size_t *count,
		     hsize_t *extra_offset,
		     hbool_t *is_derived_type );
static herr_t
H5S_mpio_spaces_xfer(H5F_t *f, const H5O_layout_t *layout, size_t elmt_size,
                     const H5S_t *file_space, const H5S_t *mem_space,
                     hid_t dxpl_id, void *buf/*out*/, hbool_t do_write);


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
 *		*extra_offset     Number of bytes of offset within dataset
 *		*is_derived_type  0 if MPI primitive type, 1 if derived
 *
 * Programmer:	rky 980813
 *
 * Modifications:
 *
 *      Quincey Koziol, June 18, 2002
 *      Added 'extra_offset' parameter
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5S_mpio_all_type( const H5S_t *space, size_t elmt_size,
		     /* out: */
		     MPI_Datatype *new_type,
		     size_t *count,
		     hsize_t *extra_offset,
		     hbool_t *is_derived_type )
{
    hsize_t	total_bytes;
    unsigned		u;

    FUNC_ENTER_NOINIT(H5S_mpio_all_type);

    /* Check args */
    assert (space);

    /* Just treat the entire extent as a block of bytes */
    total_bytes = (hsize_t)elmt_size;
    for (u=0; u<space->extent.u.simple.rank; ++u)
        total_bytes *= space->extent.u.simple.size[u];

    /* fill in the return values */
    *new_type = MPI_BYTE;
    H5_ASSIGN_OVERFLOW(*count, total_bytes, hsize_t, size_t);
    *extra_offset = 0;
    *is_derived_type = 0;

    FUNC_LEAVE_NOAPI(SUCCEED);
} /* H5S_mpio_all_type() */


/*-------------------------------------------------------------------------
 * Function:	H5S_mpio_none_type
 *
 * Purpose:	Translate an HDF5 "none" selection into an MPI type.
 *
 * Return:	non-negative on success, negative on failure.
 *
 * Outputs:	*new_type	  the MPI type corresponding to the selection
 *		*count		  how many objects of the new_type in selection
 *				  (useful if this is the buffer type for xfer)
 *		*extra_offset     Number of bytes of offset within dataset
 *		*is_derived_type  0 if MPI primitive type, 1 if derived
 *
 * Programmer:	Quincey Koziol, October 29, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5S_mpio_none_type( const H5S_t UNUSED *space, size_t UNUSED elmt_size,
		     /* out: */
		     MPI_Datatype *new_type,
		     size_t *count,
		     hsize_t *extra_offset,
		     hbool_t *is_derived_type )
{
    FUNC_ENTER_NOINIT(H5S_mpio_none_type);

    /* fill in the return values */
    *new_type = MPI_BYTE;
    *count = 0;
    *extra_offset = 0;
    *is_derived_type = 0;

    FUNC_LEAVE_NOAPI(SUCCEED);
} /* H5S_mpio_none_type() */


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
 *		*extra_offset     Number of bytes of offset within dataset
 *		*is_derived_type  0 if MPI primitive type, 1 if derived
 *
 * Programmer:	rky 980813
 *
 * Modifications:  ppw 990401
 *		rky, ppw 2000-09-26 Freed old type after creating struct type.
 *		rky 2000-10-05 Changed displacements to be MPI_Aint.
 *		rky 2000-10-06 Added code for cases of empty hyperslab.
 *		akc, rky 2000-11-16 Replaced hard coded dimension size with
 *		    H5S_MAX_RANK.
 *
 *      Quincey Koziol, June 18, 2002
 *      Added 'extra_offset' parameter.  Also accomodate
 *      selection offset in MPI type built.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5S_mpio_hyper_type( const H5S_t *space, size_t elmt_size,
		     /* out: */
		     MPI_Datatype *new_type,
		     size_t *count,
		     hsize_t *extra_offset,
		     hbool_t *is_derived_type )
{
    struct dim {	/* less hassle than malloc/free & ilk */
        hssize_t start;
        hsize_t strid;
        hsize_t block;
        hsize_t xtent;
        hsize_t count;
    } d[H5S_MAX_RANK];

    int			i, new_rank, num_to_collapse;
    herr_t		ret_value = SUCCEED;
    int			offset[H5S_MAX_RANK];
    int			max_xtent[H5S_MAX_RANK];
    H5S_hyper_dim_t	*diminfo;		/* [rank] */
    int		rank;
    int			block_length[2];
    MPI_Datatype	inner_type, outer_type, old_type[2];
    MPI_Aint            extent_len, displacement[2];
    int                 mpi_code;               /* MPI return code */

    FUNC_ENTER_NOINIT(H5S_mpio_hyper_type);

    /* Check and abbreviate args */
    assert (space);
    assert(sizeof(MPI_Aint) >= sizeof(elmt_size));
    diminfo = space->select.sel_info.hslab.diminfo;
    assert (diminfo);
    rank = space->extent.u.simple.rank;
    assert (rank >= 0);
    if (0==rank)
        goto empty;
    if (0==elmt_size)
        goto empty;

    /* make a local copy of the dimension info so we can transform them */
    assert(rank<=H5S_MAX_RANK);	/* within array bounds */
    for ( i=0; i<rank; ++i) {
        d[i].start = diminfo[i].start+space->select.offset[i];
        d[i].strid = diminfo[i].stride;
        d[i].block = diminfo[i].block;
        d[i].count = diminfo[i].count;
        d[i].xtent = space->extent.u.simple.size[i];
#ifdef H5Smpi_DEBUG
        HDfprintf(stdout, "hyper_type: start=%Hd  stride=%Hu  count=%Hu  "
            "block=%Hu  xtent=%Hu",
            d[i].start, d[i].strid, d[i].count, d[i].block, d[i].xtent );
        if (i==0)
            HDfprintf(stdout, "  rank=%d\n", rank );
        else
            HDfprintf(stdout, "\n" );
#endif
        if (0==d[i].block)
            goto empty;
        if (0==d[i].count)
            goto empty;
        if (0==d[i].xtent)
            goto empty;
    }
    
/**********************************************************************
    Compute array "offset[rank]" which gives the offsets for a multi-
    dimensional array with dimensions "d[i].xtent" (i=0,1,...,rank-1). 
**********************************************************************/
    offset[rank-1] = 1;
    max_xtent[rank-1] = d[rank-1].xtent;
#ifdef H5Smpi_DEBUG
    i=rank-1;
    HDfprintf(stdout, " offset[%2d]=%d; max_xtent[%2d]=%d\n",
                          i, offset[i], i, max_xtent[i]);
#endif
    for (i=rank-2; i>=0; --i) {
        offset[i] = offset[i+1]*d[i+1].xtent;
        max_xtent[i] = max_xtent[i+1]*d[i].xtent;
#ifdef H5Smpi_DEBUG
        HDfprintf(stdout, " offset[%2d]=%d; max_xtent[%2d]=%d\n",
                          i, offset[i], i, max_xtent[i]);
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
                || (d[rank-i-1].count*d[rank-i-1].block) != space->extent.u.simple.size[rank-i-1])
            break;
    } /* end for */

    num_to_collapse = i;
    if (num_to_collapse == rank)
        num_to_collapse--;

    assert(0<=num_to_collapse && num_to_collapse<rank);
    new_rank = rank - num_to_collapse;
#ifdef H5Smpi_DEBUG
    HDfprintf(stdout, "num_to_collapse=%d\n", num_to_collapse);
    HDfprintf(stdout, "hyper_type: new_rank=%d\n", new_rank );
#endif

    /* To collapse dims, just transform dimension info (from inner to outer) */
    /* must multiply r.h.s. by "count" ---  ppw 03/11/99  */
    for (i=rank-1; i>=new_rank; --i) {
        d[i-1].block *= d[i].count * d[i].block;
        d[i-1].strid *= d[i].count * d[i].block;
        d[i-1].xtent *= d[i].count * d[i].block;
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

/*******************************************************
*  Construct contig type for inner contig dims:
*******************************************************/
#ifdef H5Smpi_DEBUG
    HDfprintf(stdout, "hyper_type: Making contig type %d MPI_BYTEs\n", elmt_size );
    for (i=new_rank-1; i>=0; --i)
        HDfprintf(stdout, "d[%d].xtent=%Hu \n", i, d[i].xtent);
#endif
    if (MPI_SUCCESS != (mpi_code= MPI_Type_contiguous( (int)elmt_size, MPI_BYTE, &inner_type )))
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_contiguous failed", mpi_code);

/*******************************************************
*  Construct the type by walking the hyperslab dims
*  from the inside out:
*******************************************************/
    for ( i=new_rank-1; i>=0; --i) {
#ifdef H5Smpi_DEBUG
        HDfprintf(stdout, "hyper_type: Dimension i=%d \n"
            "count=%Hu block=%Hu stride=%Hu\n",
            i, d[i].count, d[i].block, d[i].strid );
#endif

#ifdef H5Smpi_DEBUG
        HDfprintf(stdout, "hyper_type: i=%d  Making vector-type \n", i);
#endif
       /****************************************
       *  Build vector in current dimension:
       ****************************************/
        mpi_code = MPI_Type_vector ( (int)(d[i].count),        /* count */
                  (int)(d[i].block),        /* blocklength */
				  (int)(d[i].strid),   	    /* stride */
				  inner_type,	            /* old type */
				  &outer_type );            /* new type */

        MPI_Type_free( &inner_type );
        if (mpi_code!=MPI_SUCCESS)
            HMPI_GOTO_ERROR(FAIL, "couldn't create MPI vector type", mpi_code);

        displacement[1] = (MPI_Aint)elmt_size * max_xtent[i];
        if(MPI_SUCCESS != (mpi_code = MPI_Type_extent(outer_type, &extent_len)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Type_extent failed", mpi_code);

       /*************************************************
       *  Restructure this datatype ("outer_type")
       *  so that it still starts at 0, but its extent
       *  is the full extent in this dimension.
       *************************************************/
        if ((int)extent_len < displacement[1]) {

#ifdef H5Smpi_DEBUG
            HDfprintf(stdout, "hyper_type: i=%d Extending struct type\n"
                "***displacements: 0, %d\n", i, displacement[1]);
#endif

#ifdef H5_HAVE_MPI2  /*  have MPI-2  */
            mpi_code = MPI_Type_create_resized
                                  ( outer_type,        /* old type  */
                                    0,                 /* blocklengths */
                                    displacement[1],   /* displacements */
                                    &inner_type);      /* new type */
#else        /*  do not have MPI-2  */
            block_length[0] = 1;
            block_length[1] = 1;
  
            displacement[0] = 0;
  
            old_type[0] = outer_type;
            old_type[1] = MPI_UB;
#ifdef H5Smpi_DEBUG
            HDfprintf(stdout, "hyper_type: i=%d Extending struct type\n"
                "***displacements: 0, %d\n", i, displacement[1]);
#endif
            mpi_code = MPI_Type_struct ( 2,               /* count */
                                    block_length,    /* blocklengths */
                                    displacement,    /* displacements */
                                    old_type,        /* old types */
                                    &inner_type);    /* new type */
#endif
  
            MPI_Type_free (&outer_type);
    	    if (mpi_code!=MPI_SUCCESS)
                HMPI_GOTO_ERROR(FAIL, "couldn't resize MPI vector type", mpi_code);
        }
        else {
            inner_type = outer_type;
        }
    } /* end for */
/***************************
*  End of loop, walking 
*  thru dimensions.
***************************/


    /* At this point inner_type is actually the outermost type, even for 0-trip loop */

/***************************************************************
*  Final task: create a struct which is a "clone" of the
*  current struct, but displaced according to the d[i].start
*  values given in the hyperslab description:
***************************************************************/
    displacement[0] = 0;
    for (i=new_rank-1; i>=0; i--)
        displacement[0] += d[i].start * offset[i];

    if (displacement[0] > 0) {
        displacement[0] *= elmt_size;
        block_length[0] = 1;
        old_type[0] = inner_type;

#ifdef H5Smpi_DEBUG
        HDfprintf(stdout, "hyper_type:  Making final struct\n***count=1:\n");
        HDfprintf(stdout, "\tblocklength[0]=%d; displacement[0]=%d\n",
                     block_length[0], displacement[0]);
#endif

  
        if (MPI_SUCCESS != (mpi_code= MPI_Type_struct( 1,                  /* count */
                     block_length,       /* blocklengths */
                     displacement,       /* displacements */
                     old_type,	         /* old type */
                     new_type ))         /* new type */
                )
            HMPI_GOTO_ERROR(FAIL, "couldn't create MPI struct type", mpi_code);

        if (MPI_SUCCESS != (mpi_code= MPI_Type_free (&old_type[0])))
            HMPI_GOTO_ERROR(FAIL, "MPI_Type_free failed", mpi_code);
    }
    else {
        *new_type = inner_type;
    }

    if (MPI_SUCCESS != (mpi_code= MPI_Type_commit( new_type )))
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_commit failed", mpi_code);

    /* fill in the remaining return values */
    *count = 1;			/* only have to move one of these suckers! */
    *extra_offset = 0;
    *is_derived_type = 1;
    HGOTO_DONE(SUCCEED);

empty:
    /* special case: empty hyperslab */
    *new_type = MPI_BYTE;
    *count = 0;
    *extra_offset = 0;
    *is_derived_type = 0;

done:
#ifdef H5Smpi_DEBUG
    HDfprintf(stdout, "Leave %s, count=%Hu  is_derived_type=%d\n",
		FUNC, *count, *is_derived_type );
#endif
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5S_mpio_hyper_contig_type
 *
 * Purpose:	Translate a contiguous HDF5 "hyperslab" selection into an MPI type.
 *
 * Return:	non-negative on success, negative on failure.
 *
 * Outputs:	*new_type	  the MPI type corresponding to the selection
 *		*count		  how many objects of the new_type in selection
 *				  (useful if this is the buffer type for xfer)
 *		*extra_offset     Number of bytes of offset within dataset
 *		*is_derived_type  0 if MPI primitive type, 1 if derived
 *
 * Programmer:	Quincey Koziol, 2002/06/17
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5S_mpio_hyper_contig_type( const H5S_t *space, size_t elmt_size,
		     /* out: */
		     MPI_Datatype *new_type,
		     size_t *count,
		     hsize_t *extra_offset,
		     hbool_t *is_derived_type )
{
    hsize_t	total_bytes;    /* Number of bytes in selection */
    hssize_t	nelem;          /* Number of elements in selection */
    hsize_t	byte_offset;	/* Byte offset of contiguous region within selection */
    hsize_t	acc;	        /* Accumulator */
    hsize_t	slab[H5O_LAYOUT_NDIMS];         /* Hyperslab size */
    hssize_t    offset[H5O_LAYOUT_NDIMS];       /* Offset in selection */
    int   	ndims;          /* Number of dimensions of dataset */
    int         i;              /* Local index */

    FUNC_ENTER_NOINIT(H5S_mpio_hyper_contig_type);

    /* Check args */
    assert (space);

    /* Get the number of elements in the selection */
    nelem=(*space->select.get_npoints)(space);

    /* Compute the number of bytes in selection */
    total_bytes = (hsize_t)elmt_size*nelem;

    /* Set up convenient alias */
    ndims=space->extent.u.simple.rank;

    /* Initialize row sizes for each dimension */
    for(i=(ndims-1),acc=1; i>=0; i--) {
        slab[i]=acc*elmt_size;
        acc*=space->extent.u.simple.size[i];
    } /* end for */

    /* Get in the selection offset */
    assert(space->select.sel_info.hslab.diminfo);
    for(i=0; i<ndims; i++)
        offset[i] = space->select.sel_info.hslab.diminfo[i].start+space->select.offset[i];

    /* Compute the initial buffer offset */
    for(i=0,byte_offset=0; i<ndims; i++)
        byte_offset+=offset[i]*slab[i];

    /* fill in the return values */
    *new_type = MPI_BYTE;
    H5_ASSIGN_OVERFLOW(*count, total_bytes, hsize_t, size_t);
    *extra_offset = byte_offset;
    *is_derived_type = 0;

    FUNC_LEAVE_NOAPI(SUCCEED);
} /* end H5S_mpio_hyper_contig_type() */


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
 *		*extra_offset     Number of bytes of offset within dataset
 *		*is_derived_type  0 if MPI primitive type, 1 if derived
 *
 * Programmer:	rky 980813
 *
 * Modifications:
 *
 *      Quincey Koziol, June 18, 2002
 *      Added 'extra_offset' parameter
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5S_mpio_space_type( const H5S_t *space, size_t elmt_size,
		     /* out: */
		     MPI_Datatype *new_type,
		     size_t *count,
		     hsize_t *extra_offset,
		     hbool_t *is_derived_type )
{
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOINIT(H5S_mpio_space_type);

    /* Check args */
    assert (space);

    /* Creat MPI type based on the kind of selection */
    switch (space->extent.type) {
        case H5S_SCALAR:
        case H5S_SIMPLE:
            switch(space->select.type) {
                case H5S_SEL_NONE:
                    if ( H5S_mpio_none_type( space, elmt_size,
                        /* out: */ new_type, count, extra_offset, is_derived_type ) <0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL,"couldn't convert \"all\" selection to MPI type");
                    break;

                case H5S_SEL_ALL:
                    if ( H5S_mpio_all_type( space, elmt_size,
                        /* out: */ new_type, count, extra_offset, is_derived_type ) <0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL,"couldn't convert \"all\" selection to MPI type");
                    break;

                case H5S_SEL_POINTS:
                    /* not yet implemented */
                    ret_value = FAIL;
                    break;

                case H5S_SEL_HYPERSLABS:
                    if((*space->select.is_contiguous)(space)) {
                        if(H5S_mpio_hyper_contig_type( space, elmt_size,
                                /* out: */ new_type, count, extra_offset, is_derived_type )<0)
                            HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL,"couldn't convert \"all\" selection to MPI type");
                    } /* end if */
                    else {
                        if(H5S_mpio_hyper_type( space, elmt_size,
                                /* out: */ new_type, count, extra_offset, is_derived_type )<0)
                            HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL,"couldn't convert \"all\" selection to MPI type");
                    } /* end else */
                    break;

                default:
                    assert("unknown selection type" && 0);
                    break;
            } /* end switch */
            break;

        case H5S_COMPLEX:
            /* not yet implemented */
            HGOTO_ERROR(H5E_DATASPACE, H5E_UNSUPPORTED, FAIL, "complex data spaces are not supported yet");
            break;

        default:
            assert("unknown data space type" && 0);
            break;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


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
 * Notes:
 *      For collective data transfer only since this would eventually call
 *      H5FD_mpio_setup to do setup to eveually call MPI_File_set_view in
 *      H5FD_mpio_read or H5FD_mpio_write.  MPI_File_set_view is a collective
 *      call.  Letting independent data transfer use this route would result in
 *      hanging.
 *
 *      The preconditions for calling this routine are located in the
 *      H5S_mpio_opt_possible() routine, which determines whether this routine
 *      can be called for a given dataset transfer.
 *
 * Modifications:
 *	rky 980918
 *	Added must_convert parameter to let caller know we can't optimize
 *	the xfer.
 *
 *	Albert Cheng, 001123
 *	Include the MPI_type freeing as part of cleanup code.
 *
 *      QAK - 2002/04/02
 *      Removed the must_convert parameter and move preconditions to
 *      H5S_mpio_opt_possible() routine
 *
 *      QAK - 2002/06/17
 *      Removed 'disp' parameter from H5FD_mpio_setup routine and use the
 *      address of the dataset in MPI_File_set_view() calls, as necessary.
 *
 *      QAK - 2002/06/18
 *      Removed 'dc_plist' parameter, since it was not used.  Also, switch to
 *      getting the 'extra_offset' setting for each selection.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5S_mpio_spaces_xfer(H5F_t *f, const H5O_layout_t *layout, size_t elmt_size,
                     const H5S_t *file_space, const H5S_t *mem_space,
		     hid_t dxpl_id, void *_buf /*out*/,
		     hbool_t do_write )
{
    haddr_t	 addr;                  /* Address of dataset (or selection) within file */
    size_t	 mpi_buf_count, mpi_file_count;       /* Number of "objects" to transfer */
    hsize_t	 mpi_buf_offset, mpi_file_offset;       /* Offset within dataset where selection (ie. MPI type) begins */
    MPI_Datatype mpi_buf_type, mpi_file_type;   /* MPI types for buffer (memory) and file */
    hbool_t	 mbt_is_derived=0,      /* Whether the buffer (memory) type is derived and needs to be free'd */
		 mft_is_derived=0;      /* Whether the file type is derived and needs to be free'd */
    hbool_t	 plist_is_setup=0;      /* Whether the dxpl has been customized */
    uint8_t	*buf=(uint8_t *)_buf;   /* Alias for pointer arithmetic */
    int         mpi_code;               /* MPI return code */
    herr_t	 ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOINIT(H5S_mpio_spaces_xfer);

    /* Check args */
    assert (f);
    assert (layout);
    assert (file_space);
    assert (mem_space);
    assert (buf);
    assert (IS_H5FD_MPIO(f));
    /* Make certain we have the correct type of property list */
    assert(TRUE==H5P_isa_class(dxpl_id,H5P_DATASET_XFER));

    /* create the MPI buffer type */
    if (H5S_mpio_space_type( mem_space, elmt_size,
			       /* out: */
			       &mpi_buf_type,
			       &mpi_buf_count,
			       &mpi_buf_offset,
			       &mbt_is_derived )<0)
    	HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL,"couldn't create MPI buf type");

    /* create the MPI file type */
    if ( H5S_mpio_space_type( file_space, elmt_size,
			       /* out: */
			       &mpi_file_type,
			       &mpi_file_count,
			       &mpi_file_offset,
			       &mft_is_derived )<0)
    	HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL,"couldn't create MPI file type");

    /* Use the absolute base address of the dataset (or chunk, eventually) as
     * the address to read from.  This should be used as the diplacement for
     * a call to MPI_File_set_view() in the read or write call.
     */
    addr = f->shared->base_addr + layout->addr + mpi_file_offset;
#ifdef H5Smpi_DEBUG
    HDfprintf(stdout, "spaces_xfer: addr=%a\n", addr );
#endif

    /*
     * Pass buf type, file type to the file driver. Request an MPI type
     * transfer (instead of an elementary byteblock transfer).
     */
    if(H5FD_mpio_setup(dxpl_id, mpi_buf_type, mpi_file_type)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set MPI-I/O properties");
    plist_is_setup=1;

    /* Adjust the buffer pointer to the beginning of the selection */
    buf+=mpi_buf_offset;

    /* transfer the data */
    if (do_write) {
    	if (H5FD_write(f->shared->lf, H5FD_MEM_DRAW, dxpl_id, addr, mpi_buf_count, buf) <0)
	    HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL,"MPI write failed");
    } else {
    	if ( H5FD_read (f->shared->lf, H5FD_MEM_DRAW, dxpl_id, addr, mpi_buf_count, buf) <0)
	    HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL,"MPI read failed");
    }

done:
    /* Reset the dxpl settings */
    if(plist_is_setup) {
        if(H5FD_mpio_teardown(dxpl_id)<0)
    	    HDONE_ERROR(H5E_DATASPACE, H5E_CANTFREE, FAIL, "unable to reset dxpl values");
    } /* end if */

    /* free the MPI buf and file types */
    if (mbt_is_derived) {
	if (MPI_SUCCESS != (mpi_code= MPI_Type_free( &mpi_buf_type )))
            HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code);
    }
    if (mft_is_derived) {
	if (MPI_SUCCESS != (mpi_code= MPI_Type_free( &mpi_file_type )))
            HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code);
    }

    FUNC_LEAVE_NOAPI(ret_value);
}


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
 *      QAK - 2002/04/02
 *      Removed the must_convert parameter and move preconditions to
 *      H5S_mpio_opt_possible() routine
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_mpio_spaces_read(H5F_t *f, const H5O_layout_t *layout,
    H5P_genplist_t UNUSED *dc_plist, const H5D_storage_t UNUSED *store, size_t elmt_size,
    const H5S_t *file_space, const H5S_t *mem_space, hid_t dxpl_id,
    void *buf/*out*/)
{
    herr_t ret_value;

    FUNC_ENTER_NOAPI(H5S_mpio_spaces_read, FAIL);

    ret_value = H5S_mpio_spaces_xfer(f, layout, elmt_size,
				     file_space, mem_space, dxpl_id,
				     buf, 0/*read*/);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


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
 *      QAK - 2002/04/02
 *      Removed the must_convert parameter and move preconditions to
 *      H5S_mpio_opt_possible() routine
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_mpio_spaces_write(H5F_t *f, H5O_layout_t *layout,
    H5P_genplist_t UNUSED *dc_plist, const H5D_storage_t UNUSED *store, size_t elmt_size,
    const H5S_t *file_space, const H5S_t *mem_space, hid_t dxpl_id,
    const void *buf)
{
    herr_t ret_value;

    FUNC_ENTER_NOAPI(H5S_mpio_spaces_write, FAIL);

    /*OKAY: CAST DISCARDS CONST QUALIFIER*/
    ret_value = H5S_mpio_spaces_xfer(f, layout, elmt_size,
				     file_space, mem_space, dxpl_id,
				     (void*)buf, 1/*write*/);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5S_mpio_opt_possible
 *
 * Purpose:	Checks if an direct I/O transfer is possible between memory and
 *                  the file.
 *
 * Return:	Success:        Non-negative: TRUE or FALSE
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, April 3, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5S_mpio_opt_possible( const H5S_t *mem_space, const H5S_t *file_space, const unsigned flags)
{
    htri_t c1,c2;               /* Flags whether a selection is optimizable */
    htri_t ret_value=TRUE;

    FUNC_ENTER_NOAPI(H5S_mpio_opt_possible, FAIL);

    /* Check args */
    assert(mem_space);
    assert(file_space);

    /* Check whether these are both simple or scalar dataspaces */
    if (!((H5S_SIMPLE==mem_space->extent.type || H5S_SCALAR==mem_space->extent.type)
         && (H5S_SIMPLE==file_space->extent.type || H5S_SCALAR==file_space->extent.type)))
        HGOTO_DONE(FALSE);

    /* Check whether both selections are "regular" */
    c1=(*file_space->select.is_regular)(file_space);
    c2=(*mem_space->select.is_regular)(mem_space);
    if(c1==FAIL || c2==FAIL)
        HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "invalid check for single selection blocks");
    if(c1==FALSE || c2==FALSE)
        HGOTO_DONE(FALSE);

    /* Can't currently handle point selections */
    if (H5S_SEL_POINTS==mem_space->select.type || H5S_SEL_POINTS==file_space->select.type)
        HGOTO_DONE(FALSE);

    /* Dataset storage must be contiguous currently */
    if ((flags&H5S_CONV_STORAGE_MASK)!=H5S_CONV_STORAGE_CONTIGUOUS)
        HGOTO_DONE(FALSE);

    /* Parallel I/O conversion flag must be set */
    if(!(flags&H5S_CONV_PAR_IO_POSSIBLE))
        HGOTO_DONE(FALSE);

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* H5S_mpio_opt_possible() */

#endif  /* H5_HAVE_PARALLEL */
