#define MPI_File_open HDF_MPI_File_open
#define MPI_File_close HDF_MPI_File_close
#define MPI_File_delete HDF_MPI_File_delete
#define MPI_File_set_size HDF_MPI_File_set_size
#define MPI_File_preallocate HDF_MPI_File_preallocate
#define MPI_File_get_size HDF_MPI_File_get_size
#define MPI_File_get_group HDF_MPI_File_get_group
#define MPI_File_get_amode HDF_MPI_File_get_amode
#define MPI_File_set_view HDF_MPI_File_set_view
#define MPI_File_get_view HDF_MPI_File_get_view
#define MPI_File_read_at HDF_MPI_File_read_at
#define MPI_File_read_at_all HDF_MPI_File_read_at_all
#define MPI_File_write_at HDF_MPI_File_write_at
#define MPI_File_write_at_all HDF_MPI_File_write_at_all
#define MPI_File_iread_at HDF_MPI_File_iread_at
#define MPI_File_iwrite_at HDF_MPI_File_iwrite_at
#define MPI_File_read HDF_MPI_File_read
#define MPI_File_read_all HDF_MPI_File_read_all
#define MPI_File_write HDF_MPI_File_write
#define MPI_File_write_all HDF_MPI_File_write_all
#define MPI_File_iread HDF_MPI_File_iread
#define MPI_File_iwrite HDF_MPI_File_iwrite
#define MPI_File_seek HDF_MPI_File_seek
#define MPI_File_get_position HDF_MPI_File_get_position
#define MPI_File_get_byte_offset HDF_MPI_File_get_byte_offset
#define MPI_File_get_type_extent HDF_MPI_File_get_type_extent
#define MPI_File_set_atomicity HDF_MPI_File_set_atomicity
#define MPI_File_get_atomicity HDF_MPI_File_get_atomicity
#define MPI_File_sync HDF_MPI_File_sync

int HDF_MPI_File_open( MPI_Comm comm, char *filename, int amode, 
			MPI_Info info, MPI_File *fh );
int HDF_MPI_File_close( MPI_File *fh );
int HDF_MPI_File_delete( char *filename, MPI_Info info );
int HDF_MPI_File_set_size( MPI_File fh, MPI_Offset size );
int HDF_MPI_File_preallocate( MPI_File fh, MPI_Offset size);
int HDF_MPI_File_get_size( MPI_File fh, MPI_Offset *size );
int HDF_MPI_File_get_group( MPI_File fh, MPI_Group *group );
int HDF_MPI_File_get_amode( MPI_File fh, int *amode );
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

int HDF_MPI_File_iread_at( MPI_File fh, MPI_Offset offset, void *buf, 
                            int count, MPI_Datatype datatype, 
                            MPIO_Request *request );
int HDF_MPI_File_iwrite_at( MPI_File fh, MPI_Offset offset, void *buf, 
                             int count, MPI_Datatype datatype, 
                             MPIO_Request *request );
int HDF_MPI_File_read( MPI_File fh, void *buf, int count, 
                             MPI_Datatype datatype, MPI_Status *status );
int HDF_MPI_File_read_all( MPI_File fh, void *buf, int count, 
                            MPI_Datatype datatype, MPI_Status *status );
int HDF_MPI_File_write( MPI_File fh, void *buf, int count, 
                         MPI_Datatype datatype, MPI_Status *status );
int HDF_MPI_File_write_all( MPI_File fh, void *buf, int count, 
                             MPI_Datatype datatype, MPI_Status *status );
int HDF_MPI_File_iread( MPI_File fh, void *buf, int count, 
			 MPI_Datatype datatype, MPIO_Request *request );
int HDF_MPI_File_iwrite( MPI_File fh, void *buf, int count, 
			  MPI_Datatype datatype, MPIO_Request *request );
int HDF_MPI_File_seek( MPI_File fh, 
		        MPI_Offset offset, int whence ) ;
int HDF_MPI_File_get_position( MPI_File fh, MPI_Offset *offset );
int HDF_MPI_File_get_byte_offset( MPI_File fh, MPI_Offset offset, 
                                   MPI_Offset *disp) ;
int HDF_MPI_File_get_type_extent( MPI_File fh, MPI_Datatype datatype, 
			           MPI_Aint *extent );
int HDF_MPI_File_set_atomicity( MPI_File fh, int flag );
int HDF_MPI_File_get_atomicity( MPI_File fh, int *flag );
int HDF_MPI_File_sync( MPI_File fh );
