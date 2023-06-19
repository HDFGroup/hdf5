/* Get a copy of file access property list */
fapl = H5Pcreate(H5P_FILE_ACCESS);

/* Retrieve the # of read attempts from the file access property list */
H5Pget_metadata_read_attempts(fapl, &attempts);

/*
 *  The value returned in "attempts" will be 1 (default for non-SWMR access).
 */

/* Set the # of read attempts to 20 */
H5Pset_metadata_read_attempts(fapl, 20);

/* Retrieve the # of read attempts from the file access property list */
H5Pget_metadata_read_attempts(fapl, &attempts);

/*
 *  The value returned in "attempts" will be 20 as set.
 */

/* Close the property list */
H5Pclose(fapl);
