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
 *		  the names "file1" and "file2" refer to the same physical fileC
 *		  (at least not without writing one and reading the other).
 *		  So we do what H5F_core_open() does: return a bogus device
 *		  number and a unique inode number.
 *		  This has the side effect that calling H5Fopen() twice
 *		  with the same name really does open the file twice
 *		  and the two handles don't communicate with each other,
 *		  resulting in trashing the file.  It also runs the (very small)
 *		  risk of having two unrelated names be seen as the same file.
 *
 *	H5F_mpio_open
 *		- should take MPI communicator and MPI info as parameters
 *		  (currently uses MPI_COMM_WORLD and MPI_INFO_NULL)
 *		- "unique" key treated same as in H5F_mpio_access
 *
 *	H5F_mpio_read
 *		- always does independent read (not collective)
 *
 *	H5F_MPIOff_to_haddr and H5F_haddr_to_MPIOff
 *		- For now, we assume that MPI_Offset and haddr_t
 *		  are the same size.  Needs to be generalized.
 */
#include <mpi.h>
#include <mpio.h>
#include <H5private.h>
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Fprivate.h>
#include <H5MMprivate.h>

#include <sys/types.h>
#include <sys/stat.h>

#define PABLO_MASK      H5F_mpio
static hbool_t          interface_initialize_g = FALSE;	/* rky??? */
#define INTERFACE_INIT  NULL

#define H5F_MPIO_DEV    0xfffe  /*pseudo dev for MPI-IO until we fix things */
				/* Make sure this differs from H5F_CORE_DEV */

static hbool_t          H5F_mpio_access(const char *name, int mode,
                                       H5F_search_t *key /*out */ );
static H5F_low_t       *H5F_mpio_open(const char *name, uintn flags,
                                       H5F_search_t *key);
static herr_t           H5F_mpio_close(H5F_low_t *lf);
static herr_t           H5F_mpio_read(H5F_low_t *lf, const haddr_t *addr,
				       size_t size, uint8 *buf);
static herr_t           H5F_mpio_write(H5F_low_t *lf, const haddr_t *addr,
				       size_t size, const uint8 *buf);
static herr_t           H5F_mpio_flush(H5F_low_t *lf);
static haddr_t          H5F_MPIOff_to_haddr( MPI_Offset mpi_off );
static MPI_Offset       H5F_haddr_to_MPIOff( haddr_t addr );

const H5F_low_class_t   H5F_LOW_MPIO[1] =
{
    {
        H5F_mpio_access,       /* access method                        */
        H5F_mpio_open,         /* open method                          */
        H5F_mpio_close,        /* close method                         */
        H5F_mpio_read,         /* read method                          */
        H5F_mpio_write,        /* write method                         */
        H5F_mpio_flush,        /* flush method                         */
        NULL                   /* extend method                        */
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
 *-------------------------------------------------------------------------
 */
static hbool_t
H5F_mpio_access(char *name, int mode, H5F_search_t *key /*out */ )
{
    hbool_t		   ret_val = FALSE;
    MPI_File		   fh;
    int			   mpierr;
    int			   mpi_mode;

    FUNC_ENTER(H5F_mpio_access, FAIL);
#ifdef DEBUG_MPIO
    fprintf(stdout, "Entering H5F_mpio_access name=%s mode=%x\n", name, mode );
#endif

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
	default:   HRETURN_ERROR(H5E_IO, H5E_ARGS, FAIL,
                          "invalid mode parameter");
    }

    mpierr = MPI_File_open( MPI_COMM_SELF, name, mpi_mode, MPI_INFO_NULL, &fh );
    if (mpierr == MPI_SUCCESS) {
	mpierr = MPI_File_close( &fh );
    	if (mpierr != MPI_SUCCESS)
	    HRETURN_ERROR(H5E_IO, H5E_ARGS, FAIL, "MPI_File_close failed");
	ret_val = TRUE;
    } else if (mode == F_OK) {
	/* to see if it exists, this time try to open for write */
	mpierr = MPI_File_open( MPI_COMM_SELF, name, MPI_MODE_WRONLY,
				MPI_INFO_NULL, &fh );
	if (mpierr == MPI_SUCCESS) {
	    mpierr = MPI_File_close( &fh );
	    if (mpierr != MPI_SUCCESS)
		HRETURN_ERROR(H5E_IO, H5E_INTERNAL, FAIL, "MPI_File_close failed");
	    ret_val = TRUE;
	}
    }

    /* if the file exists, provide its (not really) unique key */
    if ((ret_val==TRUE) && key) {
        key->dev = H5F_MPIO_DEV;
        key->ino = mpio_inode_num++;
    }

#ifdef DEBUG_MPIO
    if (key)
    	fprintf(stdout,
	    "Leaving H5F_mpio_access ret_val=%d key->dev=%x key->ino=%d\n",
	    ret_val, key->dev, key->ino );
    else
    	fprintf(stdout,
	    "Leaving H5F_mpio_access ret_val=%d\n", ret_val );
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
 *-------------------------------------------------------------------------
 */
static H5F_low_t       *
H5F_mpio_open(char *name, uintn flags, H5F_search_t *key /*out */ )
{
    H5F_low_t              *lf = NULL;
    MPI_File                fh;
    int                     mpi_amode;
    char                    mpierrmsg[MPI_MAX_ERROR_STRING];
    int                     mpierr, msglen, myid;
    MPI_Offset              size;

    FUNC_ENTER(H5F_mpio_open, NULL);
#ifdef DEBUG_MPIO
    fprintf(stdout, "Entering H5F_mpio_open name=%s flags=%x\n", name, flags );
#endif

    /* convert HDF5 flags to MPI-IO flags */
    /* some combinations are illegal; let MPI-IO figure it out */
    mpi_amode  = (flags&H5F_ACC_WRITE) ? MPI_MODE_RDWR : MPI_MODE_RDONLY;
    if (flags&H5F_ACC_CREAT)	mpi_amode |= MPI_MODE_CREATE;
    if (flags&H5F_ACC_EXCL)	mpi_amode |= MPI_MODE_EXCL;

    mpierr = MPI_File_open(MPI_COMM_WORLD, name, mpi_amode, MPI_INFO_NULL, &fh);
    if (mpierr != MPI_SUCCESS) {
        MPI_Error_string( mpierr, mpierrmsg, &msglen );
	HRETURN_ERROR(H5E_IO, H5E_CANTOPENFILE, NULL, mpierrmsg );
    }

    /* proc 0 truncates the file, if requested */
    MPI_Comm_rank( MPI_COMM_WORLD, &myid );
    if ((myid==0) && (flags&H5F_ACC_TRUNC)) {
	mpierr = MPI_File_set_size( fh, (MPI_Offset)0 );
    }
    MPI_Barrier( MPI_COMM_WORLD );

    /* Build the return value */
    lf = H5MM_xcalloc(1, sizeof(H5F_low_t));
    lf->u.mpio.f = fh;
    H5F_addr_reset(&(lf->eof));
    mpierr = MPI_File_get_size( fh, &size );
    if (mpierr != MPI_SUCCESS) {
	MPI_File_close( &(lf->u.mpio.f) );
        MPI_Error_string( mpierr, mpierrmsg, &msglen );
	HRETURN_ERROR(H5E_IO, H5E_CANTOPENFILE, NULL, mpierrmsg );
    } else {
        haddr_t	new_eof = H5F_MPIOff_to_haddr( size );
        H5F_low_seteof( lf, &new_eof );
    }

    /* The unique key */
    if (key) {
        key->dev = H5F_MPIO_DEV;
        key->ino = mpio_inode_num++;
    }

#ifdef DEBUG_MPIO
    if (key)
    	fprintf(stdout,
	    "Leaving H5F_mpio_open key->dev=%x key->ino=%d\n",
	    key->dev, key->ino );
    else
    	fprintf(stdout,
	    "Leaving H5F_mpio_open\n" );
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
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_mpio_close(H5F_low_t *lf)
{
    int                     mpierr;
    char                    mpierrmsg[MPI_MAX_ERROR_STRING];
    int                     msglen;

    FUNC_ENTER(H5F_mpio_close, FAIL);
#ifdef DEBUG_MPIO
    fprintf(stdout, "Entering H5F_mpio_close\n" );
#endif

    mpierr = MPI_File_close( &(lf->u.mpio.f) );
    /* MPI_File_close sets lf->u.mpio.f to MPI_FILE_NULL */

    if (mpierr != MPI_SUCCESS) {
        MPI_Error_string( mpierr, mpierrmsg, &msglen );
	HRETURN_ERROR(H5E_IO, H5E_CLOSEERROR, FAIL, mpierrmsg );
    }

#ifdef DEBUG_MPIO
    fprintf(stdout, "Leaving H5F_mpio_close\n" );
#endif
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5F_mpio_read
 *
 * Purpose:     Reads SIZE bytes beginning at address ADDR in file LF and
 *              places them in buffer BUF.  Reading past the logical or
 *              physical end of file returns zeros instead of failing.
 *
 * Errors:
 *              IO        READERROR     MPI_File_read_at failed. 
 *              IO        READERROR     MPI_Get_count failed
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
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_mpio_read(H5F_low_t *lf, const haddr_t *addr, size_t size, uint8 *buf)
{
    MPI_Offset              mpi_off, bytes_read;
    MPI_Status              mpi_stat;
    int                     mpierr;
    char                    mpierrmsg[MPI_MAX_ERROR_STRING];
    int                     msglen;
    size_t                  n, br;

    FUNC_ENTER(H5F_mpio_read, FAIL);
#ifdef DEBUG_MPIO
    fprintf(stdout, "Entering H5F_mpio_read\n" );
#endif

    /* Check empty read */
    if (0 == size)
        HRETURN(SUCCEED);

    /* Read the data.  */
    mpi_off = H5F_haddr_to_MPIOff( *addr );
    fprintf(stdout, "In H5F_mpio_read, before calling MPI_File_read_at\n" );
    mpierr = MPI_File_read_at( lf->u.mpio.f, mpi_off, (void*) buf,
				(int) size, MPI_BYTE, &mpi_stat );
    fprintf(stdout, "In H5F_mpio_read, after calling MPI_File_read_at\n" );
    if (mpierr != MPI_SUCCESS) {
        MPI_Error_string( mpierr, mpierrmsg, &msglen );
	HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL, mpierrmsg );
    }

    /* How many bytes were actually read? */
    mpierr = MPI_Get_count( &mpi_stat, MPI_BYTE, &bytes_read );
    if (mpierr != MPI_SUCCESS) {
        MPI_Error_string( mpierr, mpierrmsg, &msglen );
	HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL, mpierrmsg );
    }

    /* read zeroes past the end of the file */
    br = (size_t)bytes_read;
    if ((n=(size-br)) > 0)
        HDmemset( buf+br, 0, n);

#ifdef DEBUG_MPIO
    fprintf(stdout, "Leaving H5F_mpio_read\n" );
#endif
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5F_mpio_write
 *
 * Purpose:     Writes SIZE bytes from the beginning of BUF into file LF at
 *              file address ADDR.
 *
 * Errors:
 *              IO        WRITEERROR    MPI_File_write_at failed. 
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
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_mpio_write(H5F_low_t *lf, const haddr_t *addr, size_t size,
                const uint8 *buf)
{
    MPI_Offset              mpi_off;
    MPI_Status              mpi_stat;
    int                     mpierr;
    char                    mpierrmsg[MPI_MAX_ERROR_STRING];
    int                     msglen;

    FUNC_ENTER(H5F_mpio_write, FAIL);
#ifdef DEBUG_MPIO
    fprintf(stdout, "Entering H5F_mpio_write\n" );
#endif

    /* Check empty write */
    if (0 == size)
        HRETURN(SUCCEED);

    /* Write the data.  */
    mpi_off = H5F_haddr_to_MPIOff( *addr );
    mpierr = MPI_File_write_at( lf->u.mpio.f, mpi_off, (void*) buf,
				(int) size, MPI_BYTE, &mpi_stat );
    if (mpierr != MPI_SUCCESS) {
        MPI_Error_string( mpierr, mpierrmsg, &msglen );
	HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL, mpierrmsg );
    }

#ifdef DEBUG_MPIO
    fprintf(stdout, "Leaving H5F_mpio_write\n" );
#endif
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5F_mpio_flush
 *
 * Purpose:     Makes sure that all data is on disk.
 *
 * Errors:
 *              IO        WRITEERROR    Fflush failed. 
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
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_mpio_flush(H5F_low_t *lf)
{
    int                     mpierr;
    char                    mpierrmsg[MPI_MAX_ERROR_STRING];
    int                     msglen;

    FUNC_ENTER(H5F_mpio_flush, FAIL);
#ifdef DEBUG_MPIO
    fprintf(stdout, "Entering H5F_mpio_flush\n" );
#endif

    mpierr = MPI_File_sync( lf->u.mpio.f );
    if (mpierr != MPI_SUCCESS) {
        MPI_Error_string( mpierr, mpierrmsg, &msglen );
	HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, mpierrmsg );
    }
#ifdef DEBUG_MPIO
    fprintf(stdout, "Leaving H5F_mpio_flush\n" );
#endif
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5F_MPIOff_to_haddr(size) );
 *
 * Purpose:     Convert an MPI_Offset value to haddr_t.
 *
 * Problems and limitations:
 *		For now, we assume that MPI_Offset and haddr_t
 *		are the same size.  Needs to be generalized.
 *
 * Return:      Success:        the offset value, as an hddr_t.
 *
 *              Failure:        0
 *
 * Programmer:  
 *              January 30, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5F_MPIOff_to_haddr( MPI_Offset mpi_off )
{
    haddr_t                 addr;

    addr.offset = (uint64) mpi_off;

    return(addr);
}

/*-------------------------------------------------------------------------
 * Function:    H5F_haddr_to_MPIOff(size) );
 *
 * Purpose:     Convert an haddr_t value to MPI_Offset.
 *
 * Problems and limitations:
 *		For now, we assume that MPI_Offset and haddr_t
 *		are the same size.  Needs to be generalized.
 *
 * Return:      Success:        the offset value, as an MPI_Offset.
 *
 *              Failure:        0
 *
 * Programmer:  
 *              January 30, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static MPI_Offset
H5F_haddr_to_MPIOff( haddr_t addr )
{
    MPI_Offset              mpi_off;

    FUNC_ENTER(H5F_haddr_to_MPIOff, (MPI_Offset)0);

    mpi_off = (MPI_Offset) addr.offset;

    FUNC_LEAVE(mpi_off);
}
