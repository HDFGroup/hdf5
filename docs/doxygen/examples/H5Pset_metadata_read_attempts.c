//! [SWMR Access]
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
 * The value returned in "attempts" will be 20.
 * The library will use 20 as the number of read attempts
 * when reading checksummed metadata in the file
 */

/* Close the property list */
H5Pclose(fapl);
H5Pclose(file_fapl);

/* Close the file */
H5Fclose(fid);
//! [SWMR Access]

//! [non-SWMR Access]
/* Create a copy of file access property list */
fapl = H5Pcreate(H5P_FILE_ACCESS);

/* Set the # of read attempts */
H5Pset_metadata_read_attempts(fapl, 20);

/* Open the file with SWMR access and the non-default file access property list */
fid = H5Fopen(FILE, H5F_ACC_RDONLY, fapl);

/* Get the file's file access property list */
file_fapl = H5Fget_access_plist(fid);

/* Retrieve the # of read attempts from the file's file access property list */
H5Pget_metadata_read_attempts(file_fapl, &attempts);

/*
 * The value returned in "attempts" will be 1 (default for non-SWMR access).
 * The library will use 1 as the number of read attempts
 * when reading checksummed metadata in the file
 */

/* Close the property lists */
H5Pclose(fapl);
H5Pclose(file_fapl);

/* Close the file */
H5Fclose(fid);
//! [non-SWMR Access]
