#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#

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
      tnofilename-with-packed-bits.ddl
      tpbitsArray.ddl
      tpbitsCompound.ddl
      tpbitsIncomplete.ddl
      tpbitsLengthExceeded.ddl
      tpbitsCharLengthExceeded.ddl
      tpbitsIntLengthExceeded.ddl
      tpbitsLongLengthExceeded.ddl
      tpbitsLengthPositive.ddl
      tpbitsMax.ddl
      tpbitsMaxExceeded.ddl
      tpbitsOffsetExceeded.ddl
      tpbitsCharOffsetExceeded.ddl
      tpbitsIntOffsetExceeded.ddl
      tpbitsLongOffsetExceeded.ddl
      tpbitsOffsetNegative.ddl
      tpbitsOverlapped.ddl
      tpbitsSigned.ddl
      tpbitsUnsigned.ddl
      tpbitsSignedInt.ddl
      tpbitsUnsignedInt.ddl
      tpbitsSignedLong.ddl
      tpbitsUnsignedLong.ddl
      tpbitsSignedLongLong.ddl
      tpbitsUnsignedLongLong.ddl
      tpbitsSignedWhole.ddl
      tpbitsUnsignedWhole.ddl
      tpbitsSignedIntWhole.ddl
      tpbitsUnsignedIntWhole.ddl
      tpbitsSignedLongWhole.ddl
      tpbitsUnsignedLongWhole.ddl
      tpbitsSignedLongLongWhole.ddl
      tpbitsUnsignedLongLongWhole.ddl
      tpbitsSignedLongLongWhole1.ddl
      tpbitsUnsignedLongLongWhole1.ddl
      tpbitsSignedLongLongWhole63.ddl
      tpbitsUnsignedLongLongWhole63.ddl
      tpbitsSigned4.ddl
      tpbitsUnsigned4.ddl
      tpbitsSignedInt8.ddl
      tpbitsUnsignedInt8.ddl
      tpbitsSignedLong16.ddl
      tpbitsUnsignedLong16.ddl
      tpbitsSignedLongLong32.ddl
      tpbitsUnsignedLongLong32.ddl
      tpbitsSigned2.ddl
      tpbitsUnsigned2.ddl
      tpbitsSignedInt4.ddl
      tpbitsUnsignedInt4.ddl
      tpbitsSignedLong8.ddl
      tpbitsUnsignedLong8.ddl
      tpbitsSignedLongLong16.ddl
      tpbitsUnsignedLongLong16.ddl
  )
  set (HDF5_REFERENCE_TEST_PBITS
      ${HDF5_TOOLS_DIR}/testfiles/packedbits.h5
      ${HDF5_TOOLS_DIR}/testfiles/tarray1.h5
      ${HDF5_TOOLS_DIR}/testfiles/tcompound.h5
  )
  set (HDF5_ERROR_REFERENCE_PBITS
      tnofilename-with-packed-bits.err
      tpbitsCharLengthExceeded.err
      tpbitsCharOffsetExceeded.err
      tpbitsIncomplete.err
      tpbitsIntLengthExceeded.err
      tpbitsIntOffsetExceeded.err
      tpbitsLengthExceeded.err
      tpbitsLengthPositive.err
      tpbitsLongLengthExceeded.err
      tpbitsLongOffsetExceeded.err
      tpbitsMaxExceeded.err
      tpbitsOffsetExceeded.err
      tpbitsOffsetNegative.err
  )

  foreach (pbits_h5_file ${HDF5_REFERENCE_TEST_PBITS})
    get_filename_component(fname "${pbits_h5_file}" NAME)
    HDFTEST_COPY_FILE("${pbits_h5_file}" "${PROJECT_BINARY_DIR}/testfiles/pbits/${fname}" "h5dump_pbits_files")
  endforeach ()


  foreach (ddl_pbits ${HDF5_REFERENCE_PBITS})
    get_filename_component(fname "${ddl_pbits}" NAME)
    HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/pbits/${ddl_pbits}" "${PROJECT_BINARY_DIR}/testfiles/pbits/${fname}" "h5dump_pbits_files")
  endforeach ()

  foreach (ddl_pbits ${HDF5_ERROR_REFERENCE_PBITS})
    get_filename_component(fname "${ddl_pbits}" NAME)
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/errfiles/${ddl_pbits}" "${PROJECT_BINARY_DIR}/testfiles/pbits/${fname}" "h5dump_pbits_files")
  endforeach ()
  add_custom_target(h5dump_pbits_files ALL COMMENT "Copying files needed by h5dump_pbits tests" DEPENDS ${h5dump_pbits_files_list})

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  macro (ADD_H5_PBITS_TEST resultfile resultcode)
    # If using memchecker add tests without using scripts
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5DUMP-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5dump${tgt_file_ext}> ${ARGN})
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/pbits")
      if (${resultcode})
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
      if (last_pbits_test)
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES DEPENDS ${last_pbits_test})
      endif ()
    else ()
      add_test (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump${tgt_file_ext}>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/pbits"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
    endif ()
  endmacro ()

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

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
