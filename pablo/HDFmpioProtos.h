#define MPI_File_open HDF_MPI_File_open
#define MPI_File_close HDF_MPI_File_close
#define MPI_File_set_size HDF_MPI_File_set_size
#define MPI_File_get_size HDF_MPI_File_get_size
#define MPI_File_set_view HDF_MPI_File_set_view
#define MPI_File_get_view HDF_MPI_File_get_view
#define MPI_File_read_at HDF_MPI_File_read_at
#define MPI_File_read_at_all HDF_MPI_File_read_at_all
#define MPI_File_write_at HDF_MPI_File_write_at
#define MPI_File_write_at_all HDF_MPI_File_write_at_all
#define MPI_File_sync HDF_MPI_File_sync

int HDF_MPI_File_open( MPI_Comm comm, char *filename, int amode, 
			MPI_Info info, MPI_File *fh );
int HDF_MPI_File_close( MPI_File *fh );
int HDF_MPI_File_delete( char *filename, MPI_Info info );
int HDF_MPI_File_set_size( MPI_File fh, MPI_Offset size );
int HDF_MPI_File_get_size( MPI_File fh, MPI_Offset *size );
int HDF_MPI_File_set_view( MPI_File fh, MPI_Offset disp, MPI_Datatype etype, 
			    MPI_Datatype filetype, char *datarep, 
			    MPI_Info info );
int HDF_MPI_File_get_view( MPI_File fh, MPI_Offset *disp, 
			    MPI_Datatype *etype, MPI_Datatype *filetype, 
			    char *datarep );
int HDF_MPI_File_read_at( MPI_File fh, MPI_Offset offset, void *buf, 
			    int count, MPI_Datatype datatype, 
			    MPI_Status *status );
int HDF_MPI_File_read_at_all( MPI_File fh, MPI_Offset offset, void *buf, 
			       int count, MPI_Datatype datatype, 
			       MPI_Status *status );
int HDF_MPI_File_write_at( MPI_File fh, MPI_Offset offset, void *buf, 
                            int count, MPI_Datatype datatype, 
                            MPI_Status *status );
int HDF_MPI_File_write_at_all( MPI_File fh, MPI_Offset offset, void *buf, 
                                int count, MPI_Datatype datatype, 
                                MPI_Status *status );
int HDF_MPI_File_sync( MPI_File fh );
