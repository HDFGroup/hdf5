
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################
  
  # --------------------------------------------------------------------
  # Packed Bits
  # --------------------------------------------------------------------
  #-- Copy all the HDF5 files from the test directory into the source directory
  set (HDF5_REFERENCE_PBITS
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tnofilename-with-packed-bits.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsArray.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsCompound.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsIncomplete.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsLengthExceeded.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsCharLengthExceeded.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsIntLengthExceeded.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsLongLengthExceeded.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsLengthPositive.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsMax.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsMaxExceeded.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsOffsetExceeded.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsCharOffsetExceeded.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsIntOffsetExceeded.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsLongOffsetExceeded.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsOffsetNegative.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsOverlapped.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsSigned.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsUnsigned.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsSignedInt.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsUnsignedInt.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsSignedLong.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsUnsignedLong.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsSignedLongLong.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsUnsignedLongLong.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsSignedWhole.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsUnsignedWhole.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsSignedIntWhole.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsUnsignedIntWhole.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsSignedLongWhole.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsUnsignedLongWhole.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsSignedLongLongWhole.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsUnsignedLongLongWhole.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsSignedLongLongWhole1.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsUnsignedLongLongWhole1.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsSignedLongLongWhole63.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsUnsignedLongLongWhole63.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsSigned4.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsUnsigned4.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsSignedInt8.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsUnsignedInt8.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsSignedLong16.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsUnsignedLong16.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsSignedLongLong32.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsUnsignedLongLong32.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsSigned2.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsUnsigned2.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsSignedInt4.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsUnsignedInt4.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsSignedLong8.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsUnsignedLong8.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsSignedLongLong16.ddl
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tpbitsUnsignedLongLong16.ddl
  )
  set (HDF5_REFERENCE_TEST_PBITS
      ${HDF5_TOOLS_SRC_DIR}/testfiles/packedbits.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tarray1.h5
      ${HDF5_TOOLS_SRC_DIR}/testfiles/tcompound.h5
  )
  set (HDF5_ERROR_REFERENCE_PBITS
      ${PROJECT_SOURCE_DIR}/errfiles/tnofilename-with-packed-bits.err
      ${PROJECT_SOURCE_DIR}/errfiles/tpbitsCharLengthExceeded.err
      ${PROJECT_SOURCE_DIR}/errfiles/tpbitsCharOffsetExceeded.err
      ${PROJECT_SOURCE_DIR}/errfiles/tpbitsIncomplete.err
      ${PROJECT_SOURCE_DIR}/errfiles/tpbitsIntLengthExceeded.err
      ${PROJECT_SOURCE_DIR}/errfiles/tpbitsIntOffsetExceeded.err
      ${PROJECT_SOURCE_DIR}/errfiles/tpbitsLengthExceeded.err
      ${PROJECT_SOURCE_DIR}/errfiles/tpbitsLengthPositive.err
      ${PROJECT_SOURCE_DIR}/errfiles/tpbitsLongLengthExceeded.err
      ${PROJECT_SOURCE_DIR}/errfiles/tpbitsLongOffsetExceeded.err
      ${PROJECT_SOURCE_DIR}/errfiles/tpbitsMaxExceeded.err
      ${PROJECT_SOURCE_DIR}/errfiles/tpbitsOffsetExceeded.err
      ${PROJECT_SOURCE_DIR}/errfiles/tpbitsOffsetNegative.err
  )

  foreach (pbits_h5_file ${HDF5_REFERENCE_TEST_PBITS})
    GET_FILENAME_COMPONENT(fname "${pbits_h5_file}" NAME)
    set (dest "${PROJECT_BINARY_DIR}/testfiles/pbits/${fname}")
    #message (STATUS " Copying ${pbits_h5_file}")
    add_custom_command (
        TARGET     h5dump
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${pbits_h5_file} ${dest}
    )
  endforeach (pbits_h5_file ${HDF5_REFERENCE_TEST_PBITS})
  

  foreach (ddl_pbits ${HDF5_REFERENCE_PBITS})
    GET_FILENAME_COMPONENT(fname "${ddl_pbits}" NAME)
    set (ddldest "${PROJECT_BINARY_DIR}/testfiles/pbits/${fname}")
    #message (STATUS " Copying ${ddl_pbits}")
    add_custom_command (
        TARGET     h5dump
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${ddl_pbits} ${ddldest}
    )
  endforeach (ddl_pbits ${HDF5_REFERENCE_PBITS})

  foreach (ddl_pbits ${HDF5_ERROR_REFERENCE_PBITS})
    GET_FILENAME_COMPONENT(fname "${ddl_pbits}" NAME)
    set (ddldest "${PROJECT_BINARY_DIR}/testfiles/pbits/${fname}")
    #message (STATUS " Copying ${ddl_pbits}")
    add_custom_command (
        TARGET     h5dump
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${ddl_pbits} ${ddldest}
    )
  endforeach (ddl_pbits ${HDF5_ERROR_REFERENCE_PBITS})
  
##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  MACRO (ADD_H5_PBITS_TEST resultfile resultcode)
    # If using memchecker add tests without using scripts
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5DUMP-${resultfile} COMMAND $<TARGET_FILE:h5dump> ${ARGN})
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/pbits")
      if (NOT ${resultcode} STREQUAL "0")
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES WILL_FAIL "true")
      endif (NOT ${resultcode} STREQUAL "0")
      if (NOT "${last_pbits_test}" STREQUAL "")
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES DEPENDS ${last_pbits_test})
      endif (NOT "${last_pbits_test}" STREQUAL "")
    else (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5DUMP-${resultfile}-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove ${resultfile}.out ${resultfile}.out.err
      )
      set_tests_properties (H5DUMP-${resultfile}-clear-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/pbits")
      add_test (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/pbits"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES DEPENDS "H5DUMP-${resultfile}-clear-objects")
    endif (HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_H5_PBITS_TEST file)

##############################################################################
##############################################################################
###           T H E   T E S T S                                          HDF5_ENABLE_USING_MEMCHECKER  ###
##############################################################################
##############################################################################

  if (HDF5_ENABLE_USING_MEMCHECKER)
    # Remove any output file left over from previous test run
    add_test (
      NAME H5DUMP_PACKED_BITS-clearall-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          tnofilename-with-packed-bits.out
          tnofilename-with-packed-bits.out.err
          tpbitsArray.out
          tpbitsArray.out.err
          tpbitsCompound.out
          tpbitsCompound.out.err
          tpbitsIncomplete.out
          tpbitsIncomplete.out.err
          tpbitsLengthExceeded.out
          tpbitsLengthExceeded.out.err
          tpbitsCharLengthExceeded.out
          tpbitsCharLengthExceeded.out.err
          tpbitsIntLengthExceeded.out
          tpbitsIntLengthExceeded.out.err
          tpbitsLongLengthExceeded.out
          tpbitsLongLengthExceeded.out.err
          tpbitsLengthPositive.out
          tpbitsLengthPositive.out.err
          tpbitsMax.out
          tpbitsMax.out.err
          tpbitsMaxExceeded.out
          tpbitsMaxExceeded.out.err
          tpbitsOffsetExceeded.out
          tpbitsOffsetExceeded.out.err
          tpbitsCharOffsetExceeded.out
          tpbitsCharOffsetExceeded.out.err
          tpbitsIntOffsetExceeded.out
          tpbitsIntOffsetExceeded.out.err
          tpbitsLongOffsetExceeded.out
          tpbitsLongOffsetExceeded.out.err
          tpbitsOffsetNegative.out
          tpbitsOffsetNegative.out.err
          tpbitsOverlapped.out
          tpbitsOverlapped.out.err
          tpbitsSigned.out
          tpbitsSigned.out.err
          tpbitsUnsigned.out
          tpbitsUnsigned.out.err
          tpbitsSignedInt.out
          tpbitsSignedInt.out.err
          tpbitsUnsignedInt.out
          tpbitsUnsignedInt.out.err
          tpbitsSignedLong.out
          tpbitsSignedLong.out.err
          tpbitsUnsignedLong.out
          tpbitsUnsignedLong.out.err
          tpbitsSignedLongLong.out
          tpbitsSignedLongLong.out.err
          tpbitsUnsignedLongLong.out
          tpbitsUnsignedLongLong.out.err
          tpbitsSignedWhole.out
          tpbitsSignedWhole.out.err
          tpbitsUnsignedWhole.out
          tpbitsUnsignedWhole.out.err
          tpbitsSignedIntWhole.out
          tpbitsSignedIntWhole.out.err
          tpbitsUnsignedIntWhole.out
          tpbitsUnsignedIntWhole.out.err
          tpbitsSignedLongWhole.out
          tpbitsSignedLongWhole.out.err
          tpbitsUnsignedLongWhole.out
          tpbitsUnsignedLongWhole.out.err
          tpbitsSignedLongLongWhole.out
          tpbitsSignedLongLongWhole.out.err
          tpbitsUnsignedLongLongWhole.out
          tpbitsUnsignedLongLongWhole.out.err
          tpbitsSignedLongLongWhole1.out
          tpbitsSignedLongLongWhole1.out.err
          tpbitsUnsignedLongLongWhole1.out
          tpbitsUnsignedLongLongWhole1.out.err
          tpbitsSignedLongLongWhole63.out
          tpbitsSignedLongLongWhole63.out.err
          tpbitsUnsignedLongLongWhole63.out
          tpbitsUnsignedLongLongWhole63.out.err
          tpbitsSigned4.out
          tpbitsSigned4.out.err
          tpbitsUnsigned4.out
          tpbitsUnsigned4.out.err
          tpbitsSignedInt8.out
          tpbitsSignedInt8.out.err
          tpbitsUnsignedInt8.out
          tpbitsUnsignedInt8.out.err
          tpbitsSignedLong16.out
          tpbitsSignedLong16.out.err
          tpbitsUnsignedLong16.out
          tpbitsUnsignedLong16.out.err
          tpbitsSignedLongLong32.out
          tpbitsSignedLongLong32.out.err
          tpbitsUnsignedLongLong32.out
          tpbitsUnsignedLongLong32.out.err
          tpbitsSigned2.out
          tpbitsSigned2.out.err
          tpbitsUnsigned2.out
          tpbitsUnsigned2.out.err
          tpbitsSignedInt4.out
          tpbitsSignedInt4.out.err
          tpbitsUnsignedInt4.out
          tpbitsUnsignedInt4.out.err
          tpbitsSignedLong8.out
          tpbitsSignedLong8.out.err
          tpbitsUnsignedLong8.out
          tpbitsUnsignedLong8.out.err
          tpbitsSignedLongLong16.out
          tpbitsSignedLongLong16.out.err
          tpbitsUnsignedLongLong16.out
          tpbitsUnsignedLongLong16.out.err
    )
    set_tests_properties (H5DUMP_PACKED_BITS-clearall-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/pbits")
    if (NOT "${last_pbits_test}" STREQUAL "")
      set_tests_properties (H5DUMP_PACKED_BITS-clearall-objects PROPERTIES DEPENDS ${last_pbits_test})
    endif (NOT "${last_pbits_test}" STREQUAL "")
    set (last_pbits_test "H5DUMP_PACKED_BITS-clearall-objects")
  endif (HDF5_ENABLE_USING_MEMCHECKER)

  # test failure handling
  # Missing file name
  ADD_H5_PBITS_TEST (tnofilename-with-packed-bits 1 --enable-error-stack)
  # Limits:
  # Maximum number of packed bits is 8 (for now).
  # Maximum integer size is 8*sizeof(long long).
  # Maximun Offset is Maximum size - 1.
  # Maximum Offset+Length is Maximum size.
  # Tests:
  # Normal operation on both signed and unsigned int datasets.
  # Sanity check
  # Their rawdata output should be the same.
  ADD_H5_PBITS_TEST (tpbitsSignedWhole 0 --enable-error-stack -d /DS08BITS -M 0,8 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsUnsignedWhole 0 --enable-error-stack -d /DU08BITS -M 0,8 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsSignedIntWhole 0 --enable-error-stack -d /DS16BITS -M 0,16 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsUnsignedIntWhole 0 --enable-error-stack -d /DU16BITS -M 0,16 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsSignedLongWhole 0 --enable-error-stack -d /DS32BITS -M 0,32 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsUnsignedLongWhole 0 --enable-error-stack -d /DU32BITS -M 0,32 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsSignedLongLongWhole 0 --enable-error-stack -d /DS64BITS -M 0,64 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsUnsignedLongLongWhole 0 --enable-error-stack -d /DU64BITS -M 0,64 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsSignedLongLongWhole63 0 --enable-error-stack -d /DS64BITS -M 0,63 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsUnsignedLongLongWhole63 0 --enable-error-stack -d /DU64BITS -M 0,63 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsSignedLongLongWhole1 0 --enable-error-stack -d /DS64BITS -M 1,63 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsUnsignedLongLongWhole1 0 --enable-error-stack -d /DU64BITS -M 1,63 packedbits.h5)
  # Half sections
  ADD_H5_PBITS_TEST (tpbitsSigned4 0 --enable-error-stack -d /DS08BITS -M 0,4,4,4 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsUnsigned4 0 --enable-error-stack -d /DU08BITS -M 0,4,4,4 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsSignedInt8 0 --enable-error-stack -d /DS16BITS -M 0,8,8,8 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsUnsignedInt8 0 --enable-error-stack -d /DU16BITS -M 0,8,8,8 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsSignedLong16 0 --enable-error-stack -d /DS32BITS -M 0,16,16,16 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsUnsignedLong16 0 --enable-error-stack -d /DU32BITS -M 0,16,16,16 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsSignedLongLong32 0 --enable-error-stack -d /DS64BITS -M 0,32,32,32 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsUnsignedLongLong32 0 --enable-error-stack -d /DU64BITS -M 0,32,32,32 packedbits.h5)
  # Quarter sections
  ADD_H5_PBITS_TEST (tpbitsSigned2 0 --enable-error-stack -d /DS08BITS -M 0,2,2,2,4,2,6,2 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsUnsigned2 0 --enable-error-stack -d /DU08BITS -M 0,2,2,2,4,2,6,2 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsSignedInt4 0 --enable-error-stack -d /DS16BITS -M 0,4,4,4,8,4,12,4 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsUnsignedInt4 0 --enable-error-stack -d /DU16BITS -M 0,4,4,4,8,4,12,4 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsSignedLong8 0 --enable-error-stack -d /DS32BITS -M 0,8,8,8,16,8,24,8 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsUnsignedLong8 0 --enable-error-stack -d /DU32BITS -M 0,8,8,8,16,8,24,8 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsSignedLongLong16 0 --enable-error-stack -d /DS64BITS -M 0,16,16,16,32,16,48,16 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsUnsignedLongLong16 0 --enable-error-stack -d /DU64BITS -M 0,16,16,16,32,16,48,16 packedbits.h5)
  # Begin and End
  ADD_H5_PBITS_TEST (tpbitsSigned 0 --enable-error-stack -d /DS08BITS -M 0,2,2,6 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsUnsigned 0 --enable-error-stack -d /DU08BITS -M 0,2,2,6 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsSignedInt 0 --enable-error-stack -d /DS16BITS -M 0,2,10,6 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsUnsignedInt 0 --enable-error-stack -d /DU16BITS -M 0,2,10,6 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsSignedLong 0 --enable-error-stack -d /DS32BITS -M 0,2,26,6 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsUnsignedLong 0 --enable-error-stack -d /DU32BITS -M 0,2,26,6 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsSignedLongLong 0 --enable-error-stack -d /DS64BITS -M 0,2,58,6 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsUnsignedLongLong 0 --enable-error-stack -d /DU64BITS -M 0,2,58,6 packedbits.h5)
  # Overlapped packed bits.
  ADD_H5_PBITS_TEST (tpbitsOverlapped 0 --enable-error-stack -d /DS08BITS -M 0,1,1,1,2,1,0,3 packedbits.h5)
  # Maximum number of packed bits.
  ADD_H5_PBITS_TEST (tpbitsMax 0 --enable-error-stack -d /DS08BITS -M 0,1,1,1,2,1,3,1,4,1,5,1,6,1,7,1 packedbits.h5)
  # Compound type.
  ADD_H5_PBITS_TEST (tpbitsCompound 0 --enable-error-stack -d /dset1 -M 0,1,1,1 tcompound.h5)
  # Array type.
  ADD_H5_PBITS_TEST (tpbitsArray 0 --enable-error-stack -d /Dataset1 -M 0,1,1,1 tarray1.h5)
  # Test Error handling.
  # Too many packed bits requested. Max is 8 for now.
  ADD_H5_PBITS_TEST (tpbitsMaxExceeded 1 --enable-error-stack -d /DS08BITS -M 0,1,0,1,1,1,2,1,3,1,4,1,5,1,6,1,7,1 packedbits.h5)
  # Offset too large. Max is 8*sizeof(long long.
  ADD_H5_PBITS_TEST (tpbitsOffsetExceeded 1 --enable-error-stack -d /DS08BITS -M 64,1 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsCharOffsetExceeded 0 --enable-error-stack -d /DS08BITS -M 8,1 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsIntOffsetExceeded 0 --enable-error-stack -d /DS16BITS -M 16,1 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsLongOffsetExceeded 0 --enable-error-stack -d /DS32BITS -M 32,1 packedbits.h5)
  # Bad offset, must not be negative.
  ADD_H5_PBITS_TEST (tpbitsOffsetNegative 1 --enable-error-stack -d /DS08BITS -M -1,1 packedbits.h5)
  # Bad length, must not be positive.
  ADD_H5_PBITS_TEST (tpbitsLengthPositive 1 --enable-error-stack -d /DS08BITS -M 4,0 packedbits.h5)
  # Offset+Length is too large. Max is 8*sizeof(long long).
  ADD_H5_PBITS_TEST (tpbitsLengthExceeded 1 --enable-error-stack -d /DS08BITS -M 37,28 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsCharLengthExceeded 0 --enable-error-stack -d /DS08BITS -M 2,7 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsIntLengthExceeded 0 --enable-error-stack -d /DS16BITS -M 10,7 packedbits.h5)
  ADD_H5_PBITS_TEST (tpbitsLongLengthExceeded 0 --enable-error-stack -d /DS32BITS -M 26,7 packedbits.h5)
  # Incomplete pair of packed bits request.
  ADD_H5_PBITS_TEST (tpbitsIncomplete 1 --enable-error-stack -d /DS08BITS -M 0,2,2,1,0,2,2, packedbits.h5)
