/* Open the file with SWMR access and default file access property list */
fid = H5Fopen(FILE, (H5F_ACC_RDONLY | H5F_ACC_SWMR_READ), H5P_DEFAULT);

/* Get the file's file access property list */
file_fapl = H5Fget_access_plist(fid);

/* Retrieve the # of read attempts from the file's file access property list */
H5Pget_metadata_read_attempts(file_fapl, &attempts);

/*
 *  The value returned in "attempts" will be 100 (default for SWMR access).
 */

/* Close the property list */
H5Pclose(file_fapl);

/* Close the file */
H5Fclose(fid);

/* Create a copy of file access property list */
fapl = H5Pcreate(H5P_FILE_ACCESS);

/* Set the # of read attempts */
H5Pset_metadata_read_attempts(fapl, 20);

/* Open the file with SWMR access and the non-default file access property list */
fid = H5Fopen(FILE, (H5F_ACC_RDONLY | H5F_ACC_SWMR_READ), fapl);

/* Get the file's file access property list */
file_fapl = H5Fget_access_plist(fid);

/* Retrieve the # of read attempts from the file's file access property list */
H5Pget_metadata_read_attempts(file_fapl, &attempts);

/*
 *  The value returned in "attempts" will be 20.
 */

/* Close the property lists */
H5Pclose(file_fapl);
H5Pclose(fapl);

/* Close the file */
H5Fclose(fid);
