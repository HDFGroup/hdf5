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
# Copy test files to build directory
# --------------------------------------------------------------------
set (HDF5_REFERENCE_TEST_FILES)
set (HDF5_TOOLS_TEST_H5SIGN_FILES)

# No reference files needed for basic signing tests

# --------------------------------------------------------------------
# Create testfiles directory
# --------------------------------------------------------------------
file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")

# --------------------------------------------------------------------
# Test Macro
# --------------------------------------------------------------------
macro (ADD_H5SIGN_TEST testname resultcode)
  add_test (
    NAME H5SIGN-${testname}
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5sign> ${ARGN}
  )
  set_tests_properties (H5SIGN-${testname} PROPERTIES
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  if (${resultcode} STREQUAL "1")
    set_tests_properties (H5SIGN-${testname} PROPERTIES WILL_FAIL "true")
  endif ()
endmacro ()

# --------------------------------------------------------------------
# Generate test files
# --------------------------------------------------------------------
add_test (
  NAME H5SIGN-gentest
  COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5signgentest>
  WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
)

# --------------------------------------------------------------------
# Generate test RSA key pair
# Note: This requires OpenSSL to be available during testing
# --------------------------------------------------------------------
find_program(OPENSSL_EXECUTABLE openssl)
if (OPENSSL_EXECUTABLE)
  # Generate private key
  add_test (
    NAME H5SIGN-genkey-private
    COMMAND ${OPENSSL_EXECUTABLE} genrsa -out test_private.pem 2048
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-genkey-private PROPERTIES
    DEPENDS H5SIGN-gentest
  )

  # Generate public key
  add_test (
    NAME H5SIGN-genkey-public
    COMMAND ${OPENSSL_EXECUTABLE} rsa -in test_private.pem -pubout -out test_public.pem
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-genkey-public PROPERTIES
    DEPENDS H5SIGN-genkey-private
  )

  # Test 1: Show help
  add_test (
    NAME H5SIGN-h_help
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5sign> -h
  )

  # Test 2: Show version
  add_test (
    NAME H5SIGN-V_version
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5sign> -V
  )

  # Test 3: Sign a small plugin
  add_test (
    NAME H5SIGN-sign_small
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5sign> -p plugin_small.so -k test_private.pem
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-sign_small PROPERTIES
    DEPENDS "H5SIGN-gentest;H5SIGN-genkey-private;H5SIGN-genkey-public"
  )

  # Test 4: Sign a medium plugin with verbose output
  add_test (
    NAME H5SIGN-sign_medium_verbose
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5sign> -p plugin_medium.so -k test_private.pem -v
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-sign_medium_verbose PROPERTIES
    DEPENDS "H5SIGN-gentest;H5SIGN-genkey-private;H5SIGN-genkey-public"
  )

  # Test 5: Sign a large plugin
  add_test (
    NAME H5SIGN-sign_large
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5sign> -p plugin_large.so -k test_private.pem
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-sign_large PROPERTIES
    DEPENDS "H5SIGN-gentest;H5SIGN-genkey-private;H5SIGN-genkey-public"
  )

  # Test 6: Error test - missing plugin file
  add_test (
    NAME H5SIGN-error_no_plugin
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5sign> -k test_private.pem
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-error_no_plugin PROPERTIES
    WILL_FAIL "true"
  )

  # Test 7: Error test - missing key file
  add_test (
    NAME H5SIGN-error_no_key
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5sign> -p plugin_small.so
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-error_no_key PROPERTIES
    WILL_FAIL "true"
  )

  # Test 8: Error test - nonexistent plugin file
  add_test (
    NAME H5SIGN-error_bad_plugin
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5sign> -p nonexistent.so -k test_private.pem
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-error_bad_plugin PROPERTIES
    DEPENDS "H5SIGN-genkey-private;H5SIGN-genkey-public"
    WILL_FAIL "true"
  )

  # Test 9: Error test - nonexistent key file
  add_test (
    NAME H5SIGN-error_bad_key
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5sign> -p plugin_small.so -k nonexistent.pem
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-error_bad_key PROPERTIES
    DEPENDS H5SIGN-gentest
    WILL_FAIL "true"
  )

  # --------------------------------------------------------------------
  # Signature Verification Tests
  # These tests verify that the signature verification and caching work
  # --------------------------------------------------------------------

  # Create keystore directory for verification tests
  add_test (
    NAME H5SIGN-verify-setup-keystore
    COMMAND ${CMAKE_COMMAND} -E make_directory "${PROJECT_BINARY_DIR}/testfiles/test_keystore"
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-verify-setup-keystore PROPERTIES
    DEPENDS H5SIGN-genkey-public
  )

  # Copy public key to keystore directory
  add_test (
    NAME H5SIGN-verify-copy-pubkey
    COMMAND ${CMAKE_COMMAND} -E copy test_public.pem test_keystore/test_public.pem
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-verify-copy-pubkey PROPERTIES
    DEPENDS H5SIGN-verify-setup-keystore
  )

  # Sign test plugins for verification tests
  add_test (
    NAME H5SIGN-verify-sign-plugins
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5sign> -p plugin_small.so -k test_private.pem
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-verify-sign-plugins PROPERTIES
    DEPENDS "H5SIGN-verify-copy-pubkey;H5SIGN-gentest"
  )

  # Rename signed plugin for verification test
  add_test (
    NAME H5SIGN-verify-rename-signed
    COMMAND ${CMAKE_COMMAND} -E copy plugin_small.so plugin_signed.so
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-verify-rename-signed PROPERTIES
    DEPENDS H5SIGN-verify-sign-plugins
  )

  # Create unsigned plugin for negative test
  add_test (
    NAME H5SIGN-verify-copy-unsigned
    COMMAND ${CMAKE_COMMAND} -E copy plugin_medium.so plugin_unsigned.so
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-verify-copy-unsigned PROPERTIES
    DEPENDS H5SIGN-gentest
  )

  # Sign plugin for cache test
  add_test (
    NAME H5SIGN-verify-sign-cache-test
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5sign> -p plugin_large.so -k test_private.pem
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-verify-sign-cache-test PROPERTIES
    DEPENDS "H5SIGN-gentest;H5SIGN-verify-copy-pubkey"
  )

  # Rename signed plugin for cache test
  add_test (
    NAME H5SIGN-verify-rename-cache-test
    COMMAND ${CMAKE_COMMAND} -E copy plugin_large.so plugin_cache_test.so
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-verify-rename-cache-test PROPERTIES
    DEPENDS H5SIGN-verify-sign-cache-test
  )

  # Create tampered plugin (sign then modify)
  add_test (
    NAME H5SIGN-verify-create-tampered
    COMMAND ${CMAKE_COMMAND}
      -DFILE="${PROJECT_BINARY_DIR}/testfiles/plugin_tampered.so"
      -DSOURCE="${PROJECT_BINARY_DIR}/testfiles/plugin_signed.so"
      -P "${HDF5_TOOLS_TEST_H5SIGN_SOURCE_DIR}/CreateTamperedPlugin.cmake"
  )
  set_tests_properties (H5SIGN-verify-create-tampered PROPERTIES
    DEPENDS H5SIGN-verify-rename-signed
  )

  # Run verification tests
  add_test (
    NAME H5SIGN-verify-tests
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5signverifytest>
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-verify-tests PROPERTIES
    DEPENDS "H5SIGN-verify-rename-signed;H5SIGN-verify-copy-unsigned;H5SIGN-verify-rename-cache-test;H5SIGN-verify-create-tampered"
  )

else ()
  message(WARNING "OpenSSL executable not found - h5sign tests will be skipped")
endif ()
