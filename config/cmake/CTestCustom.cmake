SET (CTEST_CUSTOM_MAXIMUM_NUMBER_OF_WARNINGS 1500)
 
SET (CTEST_CUSTOM_WARNING_EXCEPTION
    ${CTEST_CUSTOM_WARNING_EXCEPTION}
    "H5detect.c.[0-9]+.[ \t]*:[ \t]*warning C4090:"
    "testhdf5.h.[0-9]+.[ \t]*:[ \t]*warning C4005:"
    "POSIX name for this item is deprecated"
    "disabling jobserver mode"
)
 
SET (CTEST_CUSTOM_MEMCHECK_IGNORE
    ${CTEST_CUSTOM_MEMCHECK_IGNORE}
    h5test-clear-objects
    h5perform-clear-objects
    hl_test-clear-objects
    hl_fortran_test-clear-objects
    H5DIFF-clearall-objects
    H5LS-clearall-objects
    h5repart_20K-clear-objects
    h5repart_5K-clear-objects
    h5repart_sec2-clear-objects
    H5IMPORT-clear-objects
    H5REPACK-clearall-objects
    H5COPY-clearall-objects
    H5STAT-clearall-objects
    H5DUMP-clearall-objects
    H5DUMP-clear-out1
    H5DUMP-clear-out3
    H5DUMP-clear-objects
    H5DUMP_PACKED_BITS-clearall-objects
    H5DUMP-XML-clearall-objects
    H5DUMP*
    H5DIFF*
    H5REPACK*
    H5STAT*
    H5COPY*
    H5LS*
)
