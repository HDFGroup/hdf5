/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer: 
 *             January 30, 1998
 *
 * Purpose:    This is the MPI2 I/O subclass of H5Flow.
 *
 * Problems and limitations:
 *
 *	H5F_mpio_access
 *		- Since there is no "access" function for MPI-IO files
 *		  we open (i.e., MPI_File_open) the file to see if it exists
 *		  and to infer the access flags.  If the file is opened,
 *		  we close it without reading or writing it.
 *		- It is not possible within MPI-IO to determine whether or not
 *		  the names "file1" and "file2" refer to the same physical file
 *		  (at least not without writing one and reading the other).
 *		  So we do what H5F_core_open() does: return a bogus device
 *		  number and a unique inode number.
 *		  This has the side effect that calling H5Fopen() twice
 *		  with the same name really does open the file twice
 *		  and the two handles don't communicate with each other,
 *		  resulting in trashing the file.  It also runs the (very
 *		  small) risk of having two unrelated names be seen as the
 *		  same file.
 *
 *	H5F_mpio_open
 *		- "unique" key treated same as in H5F_mpio_access
 *
 *	H5F_mpio_read & H5F_mpio_write
 *		- Eventually these should choose collective or independent i/o
 *		  based on a parameter that is passed down to it from H5Dwrite,
 *		  rather than the access_parms (which are fixed at the open).
 *
 *	H5F_mpio_read
 *		- One implementation of MPI/MPI-IO causes MPI_Get_count
 *		  to return (incorrectly) a negative count.
 *		  I added code to detect this, and a kludge to pretend
 *		  that the number of bytes read is always equal to the number
 *		  requested.  This kluge is activated by #ifdef MPI_KLUGE0202.
 *
 */
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Dprivate.h>
#include <H5MMprivate.h>

#include <sys/types.h>
#include <sys/stat.h>

#ifndef HAVE_PARALLEL
/* 
 * The H5F_mpio_xxxx functions are for parallel I/O only and are
 * valid only when HAVE_PARALLEL is #defined.  This empty #ifndef
 * body is used to allow this source file be included in the serial
 * distribution.
 * Some compilers/linkers may complain about "empty" object file.
 * If that happens, uncomment the following statement to pacify
 * them.
 */
/* const hbool_t H5F_mpio_avail = FALSE; */
#else	/* HAVE_PARALLEL */
#include <mpi.h>
#include <mpio.h>

#define PABLO_MASK      H5F_mpio
static hbool_t          interface_initialize_g = FALSE;	/* rky??? */
#define INTERFACE_INIT  NULL

#define H5F_MPIO_DEV    0xfffe  /*pseudo dev for MPI-IO until we fix things */
				/* Make sure this differs from H5F_CORE_DEV */

#ifdef H5Fmpio_DEBUG
/* Flags to control debug actions in H5Fmpio.
 * Meant to be indexed by characters.
 *
 * 'c' show result of MPI_Get_count after read
 * 'r' show read offset and size
 * 't' trace function entry and exit
 * 'w' show write offset and size
 */
static int H5F_mpio_Debug[256] =
        { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 };
#endif

static hbool_t H5F_mpio_access(const char *name,
			       const H5F_access_t *access_parms, int mode,
			       H5F_search_t *key/*out*/);
static H5F_low_t *H5F_mpio_open(const char *name,
				const H5F_access_t *access_parms, uintn flags,
				H5F_search_t *key/*out*/);
static herr_t H5F_mpio_close(H5F_low_t *lf, const H5F_access_t *access_parms);
static herr_t H5F_mpio_read(H5F_low_t *lf, H5F_access_t *access_parms,
			    const H5D_transfer_t xfer_mode,
			    const haddr_t *addr, size_t size, uint8 *buf/*out*/);
static herr_t H5F_mpio_write(H5F_low_t *lf, H5F_access_t *access_parms,
			     const H5D_transfer_t xfer_mode,
			     const haddr_t *addr, size_t size, const uint8 *buf);
static herr_t H5F_mpio_flush(H5F_low_t *lf, const H5F_access_t *access_parms);
static herr_t H5F_MPIOff_to_haddr(MPI_Offset mpi_off, haddr_t *addr);
static herr_t H5F_haddr_to_MPIOff(haddr_t addr, MPI_Offset *mpi_off);

const H5F_low_class_t	H5F_LOW_MPIO_g[1] = {{
    H5F_mpio_access,		/*access method				*/
    H5F_mpio_open,		/*open method				*/
    H5F_mpio_close,		/*close method				*/

    /* rky 980816
     * this is ugly, but removing the const modifier from access_parms
     * in the parameter list of the write function in H5F_low_class_t
     * would propagate to a lot of functions that don't change that param */
    (int(*)(struct H5F_low_t *lf, const H5F_access_t *access_parms, const H5D_transfer_t xfer_mode, const haddr_t *addr, size_t size, uint8 *buf))
    H5F_mpio_read,		/*read method				*/

    /* rky 980816
     * this is ugly, but removing the const modifier from access_parms
     * in the parameter list of the write function in H5F_low_class_t
     * would propagate to a lot of functions that don't change that param */
    (int(*)(struct H5F_low_t *lf, const H5F_access_t *access_parms, const H5D_transfer_t xfer_mode, const haddr_t *addr, size_t size, const uint8 *buf))
    H5F_mpio_write,		/*write method				*/

    H5F_mpio_flush,		/*flush method				*/
    NULL,			/*extend method				*/
    NULL,			/*alloc method				*/
}};

ino_t mpio_inode_num = 0;      /* fake "inode" number */


/*-------------------------------------------------------------------------
 * Function:    H5F_mpio_access
 *
 * Purpose:     Determines if an MPI-IO file can be accessed in a particular
 *		way.  The access modes for a file are the same as those of
 *		access(2), namely
 *
 *              F_OK:   determines if the MPI-IO file exists
 *			(in fact, we can only determine that the file can be
 *			 opened for reading or writing, or neither)
 *
 *              R_OK:   determines if the MPI-IO file is readable
 *
 *              W_OK:   determines if the MPI-IO file is writable.
 *
 * Warning:	It is not possible within MPI-IO to determine whether or not
 *		the names "file1" and "file2" refer to the same physical fileC
 *		(at least not without writing one and reading the other).
 *		So we do what H5F_core_open() does: return a bogus device number
 *		and a unique inode number.
 *		This has the side effect that calling H5Fopen() twice
 *		with the same name really does open the file twice
 *		and the two handles don't communicate with each other,
 *		resulting in trashing the file.  It also runs the (very small)
 *		risk of having two unrelated names be seen as the same file.
 *
 *		Must call this routine collectively since it collectively
 *		calls MPI_File_open with the communicator in access_parms.
 *
 * Return:      Success:        TRUE or FALSE.  If TRUE, then KEY is
 *                              initialized with data that makes this file
 *                              unique (same value as H5F_low_open).
 *
 *              Failure:        FAIL, KEY is undefined.
 *
 * Programmer:  
 *              January 30, 1998
 *
 * Modifications:
 *
 * 	Robb Matzke, 18 Feb 1998
 *	Added the ACCESS_PARMS argument.
 *
 * 	June 9, 1998	Albert Cheng
 *	Instead of opening the file with COMM_SELF (which results in
 *	racing condition in routine that calls it), open it with the
 *	communicator in access_parms.  (This assumes this access call
 *	must be called collectively.)
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
H5F_mpio_access(const char *name, const H5F_access_t *access_parms, int mode,
		H5F_search_t *key/*out*/)
{
    hbool_t		   ret_val = FALSE;
    MPI_File		   fh;
    int			   mpierr;
    int			   mpi_mode;

    FUNC_ENTER(H5F_mpio_access, FAIL);
#ifdef H5Fmpio_DEBUG
    if (H5F_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Entering H5F_mpio_access name=%s mode=0x%x\n", name, mode );
#endif
    assert(access_parms->driver == H5F_LOW_MPIO);

    /* The only way to get this info in MPI-IO is to try to open the file */
    /* (though particular implementations of MPI-IO may allow other ways) */
    switch (mode) {
	case F_OK: mpi_mode = MPI_MODE_RDONLY;
		   /* to see if it exists, first try to open for read */
		   break;
	case R_OK: mpi_mode = MPI_MODE_RDONLY;
		   break;
	case W_OK: mpi_mode = MPI_MODE_WRONLY;
		   break;
	default:   HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
				 "invalid mode parameter");
    }

    /* (char*) name is okay since MPI_File_open will not change it. */
    mpierr = MPI_File_open(access_parms->u.mpio.comm, (char*) name,
			   mpi_mode, access_parms->u.mpio.info, &fh );
    if (MPI_SUCCESS == mpierr) {
	mpierr = MPI_File_close( &fh );
	if (MPI_SUCCESS != mpierr)
	    HRETURN_ERROR(H5E_IO, H5E_MPI, FAIL, "MPI_File_close failed");
	ret_val = TRUE;
    } else if (mode == F_OK) {
	/* to see if it exists, this time try to open for write */
	mpierr = MPI_File_open(access_parms->u.mpio.comm, (char*)name,
			       MPI_MODE_WRONLY, access_parms->u.mpio.info,
			       &fh );
	if (MPI_SUCCESS == mpierr) {
	    mpierr = MPI_File_close( &fh );
	    if (MPI_SUCCESS != mpierr)
		HRETURN_ERROR(H5E_IO, H5E_MPI, FAIL, "MPI_File_close failed");
	    ret_val = TRUE;
	}
    }

    /* if the file exists, provide its (not really) unique key */
    if ((ret_val==TRUE) && key) {
        key->dev = H5F_MPIO_DEV;
        key->ino = mpio_inode_num++;
    }

#ifdef H5Fmpio_DEBUG
    if (H5F_mpio_Debug[(int)'t']) {
        if (key && (ret_val==TRUE))
    	    fprintf(stdout,
	      "Leaving H5F_mpio_access ret_val=%d key->dev=0x%x key->ino=%d\n",
	      ret_val, key->dev, key->ino );
        else
    	    fprintf(stdout, "Leaving H5F_mpio_access ret_val=%d\n", ret_val );
    }
#endif

    FUNC_LEAVE(ret_val);
}

/*-------------------------------------------------------------------------
 * Function:    H5F_mpio_open
 *
 * Purpose:     Opens a file with name NAME.  The FLAGS are a bit field with
 *              the possible values defined in H5F_low_open().
 *
 * Errors:
 *              IO        CANTOPENFILE  MPI_File_open failed.
 *              IO        CANTOPENFILE  MPI_File_get_size failed.
 *              IO        CANTOPENFILE  MPI_File_set_size failed (for truncate).
 *
 * Return:      Success:        Low-level file pointer
 *
 *              Failure:        NULL
 *
 * Programmer:  
 *              January 30, 1998
 *
 * Modifications:
 *
 * 	Robb Matzke, 18 Feb 1998
 *	Added the ACCESS_PARMS argument.  Moved some error checking here from
 *	elsewhere.
 *
 *      rky, 11 Jun 1998
 *      Added H5F_mpio_Debug debug flags controlled by MPI_Info.
 *
 *-------------------------------------------------------------------------
 */
static H5F_low_t *
H5F_mpio_open(const char *name, const H5F_access_t *access_parms, uintn flags,
	      H5F_search_t *key/*out*/)
{
    H5F_low_t              *lf = NULL;
    MPI_File                fh;
    int                     mpi_amode;
    char                    mpierrmsg[MPI_MAX_ERROR_STRING];
    int                     mpierr, msglen;
    MPI_Offset              size;

    FUNC_ENTER(H5F_mpio_open, NULL);
#ifdef H5Fmpio_DEBUG
    if (H5F_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Entering H5F_mpio_open name=%s flags=0x%x\n", name, flags );
#endif

    /* convert HDF5 flags to MPI-IO flags */
    /* some combinations are illegal; let MPI-IO figure it out */
    mpi_amode  = (flags&H5F_ACC_RDWR) ? MPI_MODE_RDWR : MPI_MODE_RDONLY;
    if (flags&H5F_ACC_CREAT)	mpi_amode |= MPI_MODE_CREATE;
    if (flags&H5F_ACC_EXCL)	mpi_amode |= MPI_MODE_EXCL;

#ifdef H5Fmpio_DEBUG
    /* Check for debug commands in the info parameter */
    {   char debug_str[128];
        int infoerr, flag, i;
        if (access_parms->u.mpio.info) {
            infoerr = MPI_Info_get( access_parms->u.mpio.info,
                                    H5F_MPIO_DEBUG_KEY, 127, debug_str, &flag );
            if (flag) {
                fprintf(stdout, "H5Fmpio debug flags=%s\n", debug_str );
                for (i=0;
                     debug_str[i]/*end of string*/ && i<128/*just in case*/;
                     ++i) {
                    H5F_mpio_Debug[(int)debug_str[i]] = 1;
                }
            }
        }
    }
#endif

    mpierr = MPI_File_open(access_parms->u.mpio.comm, (char*)name, mpi_amode, access_parms->u.mpio.info, &fh);
    if (MPI_SUCCESS != mpierr) {
        MPI_Error_string( mpierr, mpierrmsg, &msglen );
	HRETURN_ERROR(H5E_IO, H5E_CANTOPENFILE, NULL, mpierrmsg );
    }

    /* truncate the file, if requested */
    if (flags&H5F_ACC_TRUNC) {
	mpierr = MPI_File_set_size( fh, (MPI_Offset)0 );
	if (MPI_SUCCESS != mpierr) {
	    MPI_File_close( &fh );
	    HRETURN_ERROR(H5E_IO, H5E_CANTOPENFILE, NULL,
			  "MPI_File_set_size failed trying to truncate file" );
	}
    }

    /* Build the return value */
    if (NULL==(lf = H5MM_calloc(sizeof(H5F_low_t)))) {
	HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,
		       "memory allocation failed");
    }
    lf->u.mpio.f = fh;
    H5F_addr_reset(&(lf->eof));
    mpierr = MPI_File_get_size( fh, &size );
    if (MPI_SUCCESS != mpierr) {
	MPI_File_close( &(lf->u.mpio.f) );
        MPI_Error_string( mpierr, mpierrmsg, &msglen );
	HRETURN_ERROR(H5E_IO, H5E_CANTOPENFILE, NULL, mpierrmsg );
    } else {
        haddr_t	new_eof;
        if (SUCCEED != H5F_MPIOff_to_haddr( size, &new_eof )) {
	    MPI_File_close( &(lf->u.mpio.f) );
	    HRETURN_ERROR(H5E_IO, H5E_CANTOPENFILE, NULL,
			  "couldn't convert size to haddr_t" );
	}
        H5F_low_seteof( lf, &new_eof );
    }

    /* The unique key */
    if (key) {
        key->dev = H5F_MPIO_DEV;
        key->ino = mpio_inode_num++;
    }

#ifdef H5Fmpio_DEBUG
    if (H5F_mpio_Debug[(int)'t']) {
        if (key)
    	    fprintf(stdout, "Leaving H5F_mpio_open key->dev=0x%x key->ino=%d\n",
		    key->dev, key->ino );
        else
    	    fprintf(stdout, "Leaving H5F_mpio_open\n" );
    }
#endif

    FUNC_LEAVE(lf);
}

/*-------------------------------------------------------------------------
 * Function:    H5F_mpio_close
 *
 * Purpose:     Closes a file.
 *
 * Errors:
 *              IO        CLOSEERROR    Fclose failed. 
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  
 *              January 30, 1998
 *
 * Modifications:
 *
 * 	Robb Matzke, 18 Feb 1998
 *	Added the ACCESS_PARMS argument.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_mpio_close(H5F_low_t *lf, const H5F_access_t __unused__ *access_parms)
{
    int                     mpierr;
    char                    mpierrmsg[MPI_MAX_ERROR_STRING];
    int                     msglen;

    FUNC_ENTER(H5F_mpio_close, FAIL);
#ifdef H5Fmpio_DEBUG
    if (H5F_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Entering H5F_mpio_close\n" );
#endif

    mpierr = MPI_File_close( &(lf->u.mpio.f) );
    /* MPI_File_close sets lf->u.mpio.f to MPI_FILE_NULL */

    if (MPI_SUCCESS != mpierr) {
        MPI_Error_string( mpierr, mpierrmsg, &msglen );
	HRETURN_ERROR(H5E_IO, H5E_CLOSEERROR, FAIL, mpierrmsg );
    }

#ifdef H5Fmpio_DEBUG
    if (H5F_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Leaving H5F_mpio_close\n" );
#endif
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5F_mpio_read
 *
 * Purpose:     Depending on a field in access params, either:
 *		- Writes SIZE bytes from the beginning of BUF into file LF
 *                at file address ADDR.
 *		- Reads SIZE bytes beginning at address ADDR in file LF
 *                and places them in buffer BUF.
 *		- Uses the (potentially complex) file and buffer types
 *                to effect the transfer.
 *		  This can allow MPI to coalesce requests from
 *		  different processes (collective or independent).
 *
 *              Reading past the end of the MPI file
 *		returns zeros instead of failing.
 *
 * Errors:
 *              IO        READERROR     MPI_File_read_at failed. 
 *              IO        READERROR     MPI_Get_count failed
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *		(use_types and old_use_types in the access params are altered)
 *
 * Programmer:  rky 980130
 *
 * Modifications:
 *
 * 	Robb Matzke, 18 Feb 1998
 *	Added the ACCESS_PARMS argument.
 *
 * 	rky, 10 Apr 1998
 *	Call independent or collective MPI read, based on ACCESS_PARMS.
 *
 * 	Albert Cheng, June 1, 1998
 *	Added xfer_mode to control independent or collective MPI read.
 *
 * 	rky 980816
 *	Use btype, ftype, and disp from access parms.
 *	The guts of H5F_mpio_read and H5F_mpio_write
 *	should be replaced by a single dual-purpose routine.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_mpio_read(H5F_low_t *lf, H5F_access_t *access_parms,
	      const H5D_transfer_t xfer_mode,
	      const haddr_t *addr, size_t size, uint8 *buf/*out*/)
{
    MPI_Offset  mpi_off, mpi_disp;
    MPI_Status  mpi_stat;
    MPI_Datatype buf_type, file_type;
    int         mpierr, msglen, size_i, bytes_read, n;
    int		use_types_this_time, used_types_last_time;
    char        mpierrmsg[MPI_MAX_ERROR_STRING];

    FUNC_ENTER(H5F_mpio_read, FAIL);
#ifdef H5Fmpio_DEBUG
    if (H5F_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Entering H5F_mpio_read\n" );
#endif

    /* some numeric conversions */
    if (SUCCEED != H5F_haddr_to_MPIOff( *addr, &mpi_off )) {
	HRETURN_ERROR(H5E_IO, H5E_BADTYPE, FAIL,
			"couldn't convert addr to MPIOffset" );
    }
    size_i = (int)size;
    if (size_i != size) {	/* check type conversion */
	HRETURN_ERROR(H5E_IO, H5E_BADTYPE, FAIL,
			"couldn't convert size to int" );
    }

#ifdef H5Fmpio_DEBUG
    if (H5F_mpio_Debug[(int)'r'])
        fprintf(stdout, "in H5F_mpio_read  mpi_off=%lld  size_i=%d\n",
                mpi_off, size_i );
#endif

    /* Set up for a fancy xfer using complex types, or single byte block.
     * We wouldn't need to rely on the use_types field
     * if MPI semantics allowed us to test that btype=ftype=MPI_BYTE
     * (or even MPI_TYPE_NULL, which could mean "use MPI_BYTE" by convention).
     */
    use_types_this_time = access_parms->u.mpio.use_types;
    if (use_types_this_time) {
	/* prepare for a full-blown xfer using btype, ftype, and disp */
	buf_type = access_parms->u.mpio.btype;
	file_type = access_parms->u.mpio.ftype;
	if (SUCCEED !=
	    H5F_haddr_to_MPIOff( access_parms->u.mpio.disp, &mpi_disp)) {
	    HRETURN_ERROR(H5E_IO, H5E_BADTYPE, FAIL, "couldn't convert addr to MPIOffset" );
	}
    } else {
	/* Prepare for a simple xfer of a contiguous block of bytes.
	 * The btype, ftype, and disp fields are not used. */
	buf_type = MPI_BYTE;
	file_type = MPI_BYTE;
	mpi_disp = 0;		/* mpi_off is sufficient */
    }

    /* Don't bother to reset the view if we're not using the types this time,
     * and did we didn't use them last time either. */
    used_types_last_time = access_parms->u.mpio.old_use_types;
    if (used_types_last_time	/* change to new ftype or MPI_BYTE */
    ||  use_types_this_time) 	/* almost certainly a different ftype */ {
	mpierr = MPI_File_set_view( lf->u.mpio.f, mpi_disp,
				    MPI_BYTE, file_type,
				    "native",  access_parms->u.mpio.info );
	if (MPI_SUCCESS != mpierr) {
	    MPI_Error_string( mpierr, mpierrmsg, &msglen );
	    HRETURN_ERROR(H5E_IO, H5E_MPI, FAIL, mpierrmsg );
	}
    }
    /* We always set the use_types flag to 0 because the 
     * default is not to use types next time,
     * unless someone explicitly requests it by setting this flag to !=0. */
    access_parms->u.mpio.old_use_types = use_types_this_time;
    access_parms->u.mpio.use_types = 0;

    /* Read the data.  */
    switch (xfer_mode){
    case H5D_XFER_INDEPENDENT:
    case H5D_XFER_DFLT:
	mpierr = MPI_File_read_at     ( lf->u.mpio.f, mpi_off, (void*) buf,
					size_i, buf_type, &mpi_stat );
	break;
	
    case H5D_XFER_COLLECTIVE:
#ifdef H5Fmpio_DEBUG
	printf("%s: using MPIO collective mode\n", FUNC);
#endif
	mpierr = MPI_File_read_at_all ( lf->u.mpio.f, mpi_off, (void*) buf,
					size_i, buf_type, &mpi_stat );
	break;

    default:
	HRETURN_ERROR(H5E_IO, H5E_BADVALUE, FAIL, "invalid file access mode");
    }
    if (MPI_SUCCESS != mpierr) {
        MPI_Error_string( mpierr, mpierrmsg, &msglen );
	HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL, mpierrmsg );
    }

    /* How many bytes were actually read? */
    mpierr = MPI_Get_count( &mpi_stat, MPI_BYTE, &bytes_read );
#ifdef H5Fmpio_DEBUG
    if (H5F_mpio_Debug[(int)'c'])
    	fprintf(stdout,
	    "In H5F_mpio_read after Get_count size_i=%d bytes_read=%d\n",
	    size_i, bytes_read );
#endif
    if (MPI_SUCCESS != mpierr) {
        MPI_Error_string( mpierr, mpierrmsg, &msglen );
	HRETURN_ERROR(H5E_IO, H5E_MPI, FAIL, mpierrmsg );
    }

#define MPI_KLUGE0202
#ifdef MPI_KLUGE0202
    /* KLUGE rky 980202 MPI_Get_count incorrectly returns negative count;
       fake a complete read */
    bytes_read = size_i;	/* KLUGE rky 980202 */
#endif

    if ((bytes_read<0) || (bytes_read > size_i)) {
	HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL,
			"MPI_Get_count returned invalid count" );
    }

    /* beyond end of file, pretend we're reading zeroes */
    if (use_types_this_time) {
	/* BUG rky 980816  Not implemented yet.  How to do it??? */
	HRETURN_ERROR(H5E_IO, H5E_UNSUPPORTED, FAIL,
		"haven't implemented reading zeroes beyond end of file" );
    } else {
	/* BUG??? rky 980816
	 * This gives us zeroes beyond end of physical MPI file.
	 * What about reading past logical end of HDF5 file??? */
	if ((n=(size_i-bytes_read)) > 0) {
	    HDmemset( buf+bytes_read, 0, (size_t)n );
	}
    }

#ifdef H5Fmpio_DEBUG
    if (H5F_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Leaving H5F_mpio_read\n" );
#endif
    FUNC_LEAVE(SUCCEED);
} /* H5F_mpio_read */

/*-------------------------------------------------------------------------
 * Function:    H5F_mpio_write
 *
 * Purpose:     Depending on a field in access params, either:
 *		- Writes SIZE bytes from the beginning of BUF into file LF
 *                at file address ADDR.
 *		- Uses the (potentially complex) file and buffer types
 *                to effect the transfer.
 *		  This can allow MPI to coalesce requests from
 *		  different processes (collective or independent).
 *
 * Errors:
 *              IO        WRITEERROR    MPI_File_write_at failed. 
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *		(use_types and old_use_types in the access params are altered)
 *
 * Programmer:  
 *              January 30, 1998
 *
 * Modifications:
 *
 * 	Robb Matzke, 18 Feb 1998
 *	Added the ACCESS_PARMS argument.
 *
 * 	rky, 10 Apr 1998
 *	Call independent or collective MPI write, based on ACCESS_PARMS.
 *
 * 	rky, 24 April
 *	Removed redundant write from H5F_Mpio_write.
 *
 * 	Albert Cheng, June 1, 1998
 *	Added xfer_mode to control independent or collective MPI write.
 *
 * 	rky 980816
 *	Use btype, ftype, and disp from access parms.
 *	The guts of H5F_mpio_read and H5F_mpio_write
 *	should be replaced by a single dual-purpose routine.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_mpio_write(H5F_low_t *lf, H5F_access_t *access_parms,
	       const H5D_transfer_t xfer_mode,
	       const haddr_t *addr, size_t size, const uint8 *buf)
{
    MPI_Offset  mpi_off, mpi_disp;
    MPI_Status  mpi_stat;
    MPI_Datatype buf_type, file_type;
    int         mpierr, msglen, size_i, bytes_written;
    int		use_types_this_time, used_types_last_time;
    char        mpierrmsg[MPI_MAX_ERROR_STRING];

    FUNC_ENTER(H5F_mpio_write, FAIL);
#ifdef H5Fmpio_DEBUG
    if (H5F_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Entering H5F_mpio_write\n" );
#endif

    /* some numeric conversions */
    if (SUCCEED != H5F_haddr_to_MPIOff( *addr, &mpi_off )) {
	HRETURN_ERROR(H5E_IO, H5E_BADTYPE, FAIL,
			"couldn't convert addr to MPIOffset" );
    }
    if (SUCCEED!=H5F_haddr_to_MPIOff( access_parms->u.mpio.disp, &mpi_disp)) {
	HRETURN_ERROR(H5E_IO, H5E_BADTYPE, FAIL,
			"couldn't convert addr to MPIOffset" );
    }
    size_i = (int)size;
    if (size_i != size) {	/* check type conversion */
	HRETURN_ERROR(H5E_IO, H5E_BADTYPE, FAIL,
			"couldn't convert size to int" );
    }

#ifdef H5Fmpio_DEBUG
    if (H5F_mpio_Debug[(int)'w'])
        fprintf(stdout, "in H5F_mpio_write  mpi_off=%lld  size_i=%d\n",
                mpi_off, size_i );
#endif

    /* Set up for a fancy xfer using complex types, or single byte block.
     * We wouldn't need to rely on the use_types field
     * if MPI semantics allowed us to test that btype=ftype=MPI_BYTE
     * (or even MPI_TYPE_NULL, which could mean "use MPI_BYTE" by convention).
     */
    use_types_this_time = access_parms->u.mpio.use_types;
    if (use_types_this_time) {
	/* prepare for a full-blown xfer using btype, ftype, and disp */
	buf_type = access_parms->u.mpio.btype;
	file_type = access_parms->u.mpio.ftype;
	if (SUCCEED !=
	    H5F_haddr_to_MPIOff( access_parms->u.mpio.disp, &mpi_disp)) {
	    HRETURN_ERROR(H5E_IO, H5E_BADTYPE, FAIL, "couldn't convert addr to MPIOffset" );
	}
    } else {
	/* Prepare for a simple xfer of a contiguous block of bytes.
	 * The btype, ftype, and disp fields are not used. */
	buf_type = MPI_BYTE;
	file_type = MPI_BYTE;
	mpi_disp = 0;		/* mpi_off is sufficient */
    }

    /* Don't bother to reset the view if we're not using the types this time,
     * and did we didn't use them last time either. */
    used_types_last_time = access_parms->u.mpio.old_use_types;
    if (used_types_last_time	/* change to new ftype or MPI_BYTE */
    ||  use_types_this_time) 	/* almost certainly a different ftype */ {
	mpierr = MPI_File_set_view( lf->u.mpio.f, mpi_disp,
				    MPI_BYTE, file_type,
				    "native",  access_parms->u.mpio.info );
	if (MPI_SUCCESS != mpierr) {
	    MPI_Error_string( mpierr, mpierrmsg, &msglen );
	    HRETURN_ERROR(H5E_IO, H5E_MPI, FAIL, mpierrmsg );
	}
    }
    /* We always set the use_types flag to 0 because the 
     * default is not to use types next time,
     * unless someone explicitly requests it by setting this flag to !=0. */
    access_parms->u.mpio.old_use_types = use_types_this_time;
    access_parms->u.mpio.use_types = 0;

    /* Write the data.  */
    switch (xfer_mode){
    case H5D_XFER_INDEPENDENT:
    case H5D_XFER_DFLT:
	mpierr = MPI_File_write_at    ( lf->u.mpio.f, mpi_off, (void*) buf,
					size_i, buf_type, &mpi_stat );
	break;
	
    case H5D_XFER_COLLECTIVE:
#ifdef H5Fmpio_DEBUG
	printf("%s: using MPIO collective mode\n", FUNC);
#endif
	mpierr = MPI_File_write_at_all( lf->u.mpio.f, mpi_off, (void*) buf,
					size_i, buf_type, &mpi_stat );
	break;

    default:
	HRETURN_ERROR(H5E_IO, H5E_BADVALUE, FAIL, "invalid file access mode");
    }
    if (MPI_SUCCESS != mpierr) {
        MPI_Error_string( mpierr, mpierrmsg, &msglen );
	HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, mpierrmsg );
    }

    /* How many bytes were actually written? */
    mpierr = MPI_Get_count( &mpi_stat, MPI_BYTE, &bytes_written );
#ifdef H5Fmpio_DEBUG
    if (H5F_mpio_Debug[(int)'c'])
    	fprintf(stdout,
	    "In H5F_mpio_write after Get_count size_i=%d bytes_written=%d\n",
	    size_i, bytes_written );
#endif
    if (MPI_SUCCESS != mpierr) {
        MPI_Error_string( mpierr, mpierrmsg, &msglen );
	HRETURN_ERROR(H5E_IO, H5E_MPI, FAIL, mpierrmsg );
    }

#define MPI_KLUGE0202
#ifdef MPI_KLUGE0202
    /* KLUGE rky 980202 MPI_Get_count incorrectly returns negative count;
       fake a complete write */
    bytes_written = size_i;	/* KLUGE rky 980202 */
#endif

    if ((bytes_written<0) || (bytes_written > size_i)) {
	HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL,
			"MPI_Get_count returned invalid count" );
    }

#ifdef H5Fmpio_DEBUG
    if (H5F_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Leaving H5F_mpio_write\n" );
#endif
    FUNC_LEAVE(SUCCEED);
} /* H5F_mpio_write */

/*-------------------------------------------------------------------------
 * Function:    H5F_mpio_flush
 *
 * Purpose:     Makes sure that all data is on disk.
 *
 * Errors:
 *              IO        WRITEERROR    MPI_File_sync failed. 
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  
 *              January 30, 1998
 *
 * Modifications:
 *
 * 	Robb Matzke, 18 Feb 1998
 *	Added the ACCESS_PARMS argument.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_mpio_flush(H5F_low_t *lf, const H5F_access_t __unused__ *access_parms)
{
    int                     mpierr;
    char                    mpierrmsg[MPI_MAX_ERROR_STRING];
    int                     msglen;

    FUNC_ENTER(H5F_mpio_flush, FAIL);
#ifdef H5Fmpio_DEBUG
    if (H5F_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Entering H5F_mpio_flush\n" );
#endif

    mpierr = MPI_File_sync( lf->u.mpio.f );
    if (MPI_SUCCESS != mpierr) {
        MPI_Error_string( mpierr, mpierrmsg, &msglen );
	HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, mpierrmsg );
    }
#ifdef H5Fmpio_DEBUG
    if (H5F_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Leaving H5F_mpio_flush\n" );
#endif
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5F_MPIOff_to_haddr
 *
 * Purpose:     Convert an MPI_Offset value to haddr_t.
 *
 * Problems and limitations:
 *
 * Return:      Success:        return value is SUCCEED
 *				and the haddr_t contains the converted value
 *
 *              Failure:        return value is FAIL, the haddr_t is undefined
 *
 * Programmer:  
 *              January 30, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_MPIOff_to_haddr( MPI_Offset mpi_off, haddr_t *addr )
{
    herr_t ret_val = FAIL;

    addr->offset = (uint64) mpi_off;
    if (addr->offset == mpi_off)
	ret_val = SUCCEED;

    return (ret_val);
}

/*-------------------------------------------------------------------------
 * Function:    H5F_haddr_to_MPIOff
 *
 * Purpose:     Convert an haddr_t value to MPI_Offset.
 *
 * Problems and limitations:
 *
 * Return:      Success:        return value is SUCCEED
 *				and the MPIOffset contains the converted value
 *
 *              Failure:        return value is FAIL, the MPIOffset is undefined
 *
 * Programmer:  
 *              January 30, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_haddr_to_MPIOff( haddr_t addr, MPI_Offset *mpi_off )
{
    herr_t ret_val = FAIL;

    *mpi_off = (MPI_Offset) addr.offset;
    if (*mpi_off ==  addr.offset)
	ret_val = SUCCEED;

    return (ret_val);
}

#endif	/* HAVE_PARALLEL */
