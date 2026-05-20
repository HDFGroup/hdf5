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
set_tests_properties (H5SIGN-gentest PROPERTIES
  FIXTURES_SETUP H5SIGN_testfiles
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
    FIXTURES_REQUIRED H5SIGN_testfiles
    FIXTURES_SETUP H5SIGN_keys
  )

  # Generate public key
  add_test (
    NAME H5SIGN-genkey-public
    COMMAND ${OPENSSL_EXECUTABLE} rsa -in test_private.pem -pubout -out test_public.pem
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-genkey-public PROPERTIES
    DEPENDS H5SIGN-genkey-private
    FIXTURES_REQUIRED H5SIGN_testfiles
    FIXTURES_SETUP H5SIGN_keys
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
  # Note: depends on verify-copy-small-for-signing to ensure the unsigned copy
  # is preserved before this test signs plugin_small.so in-place.
  add_test (
    NAME H5SIGN-sign_small
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5sign> -p plugin_small.so -k test_private.pem
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-sign_small PROPERTIES
    DEPENDS H5SIGN-verify-copy-small-for-signing
    FIXTURES_REQUIRED "H5SIGN_testfiles;H5SIGN_keys"
  )

  # Test 4: Sign a medium plugin with verbose output
  add_test (
    NAME H5SIGN-sign_medium_verbose
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5sign> -p plugin_medium.so -k test_private.pem -v
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-sign_medium_verbose PROPERTIES
    DEPENDS H5SIGN-verify-copy-unsigned
    FIXTURES_REQUIRED "H5SIGN_testfiles;H5SIGN_keys"
  )

  # Test 5: Sign a large plugin
  # Note: depends on verify-copy-large-for-signing to ensure the unsigned copy
  # is preserved before this test signs plugin_large.so in-place.
  add_test (
    NAME H5SIGN-sign_large
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5sign> -p plugin_large.so -k test_private.pem
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-sign_large PROPERTIES
    DEPENDS H5SIGN-verify-copy-large-for-signing
    FIXTURES_REQUIRED "H5SIGN_testfiles;H5SIGN_keys"
  )

  # Test 6: Re-sign an already-signed plugin using --force
  add_test (
    NAME H5SIGN-resign_force
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5sign> -p plugin_small.so -k test_private.pem -f
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-resign_force PROPERTIES
    DEPENDS H5SIGN-sign_small
  )

  # Test 7: Error test - already-signed plugin without --force
  add_test (
    NAME H5SIGN-error_already_signed
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5sign> -p plugin_small.so -k test_private.pem
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-error_already_signed PROPERTIES
    DEPENDS "H5SIGN-sign_small;H5SIGN-resign_force"
    WILL_FAIL "true"
  )

  # Test 9: Error test - missing plugin file
  add_test (
    NAME H5SIGN-error_no_plugin
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5sign> -k test_private.pem
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-error_no_plugin PROPERTIES
    WILL_FAIL "true"
  )

  # Test 10: Error test - missing key file
  add_test (
    NAME H5SIGN-error_no_key
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5sign> -p plugin_small.so
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-error_no_key PROPERTIES
    WILL_FAIL "true"
  )

  # Test 11: Error test - nonexistent plugin file
  add_test (
    NAME H5SIGN-error_bad_plugin
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5sign> -p nonexistent.so -k test_private.pem
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-error_bad_plugin PROPERTIES
    FIXTURES_REQUIRED H5SIGN_keys
    WILL_FAIL "true"
  )

  # Test 12: Error test - nonexistent key file
  add_test (
    NAME H5SIGN-error_bad_key
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5sign> -p plugin_small.so -k nonexistent.pem
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-error_bad_key PROPERTIES
    FIXTURES_REQUIRED H5SIGN_testfiles
    WILL_FAIL "true"
  )

  # --------------------------------------------------------------------
  # Signature Verification Tests
  # These tests verify that the signature verification and caching work
  # --------------------------------------------------------------------

  # ---- Keystore setup (fixture: H5SIGN_keystore) ----

  # Create keystore directory for verification tests
  add_test (
    NAME H5SIGN-verify-setup-keystore
    COMMAND ${CMAKE_COMMAND} -E make_directory "${PROJECT_BINARY_DIR}/testfiles/test_keystore"
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-verify-setup-keystore PROPERTIES
    FIXTURES_REQUIRED H5SIGN_keys
    FIXTURES_SETUP H5SIGN_keystore
  )

  # On Windows, restrict test keystore ACLs so the permission security check passes
  if (WIN32)
    add_test (
      NAME H5SIGN-verify-secure-keystore
      COMMAND powershell -NonInteractive -NoProfile
        -Command "icacls '${PROJECT_BINARY_DIR}/testfiles/test_keystore' /inheritance:r '/grant' ($env:USERNAME + ':(OI)(CI)F') '/grant' 'Administrators:(OI)(CI)F'"
    )
    set_tests_properties (H5SIGN-verify-secure-keystore PROPERTIES
      DEPENDS H5SIGN-verify-setup-keystore
      FIXTURES_SETUP H5SIGN_keystore
    )
  endif ()

  # Copy public key to keystore directory
  add_test (
    NAME H5SIGN-verify-copy-pubkey
    COMMAND ${CMAKE_COMMAND} -E copy test_public.pem test_keystore/test_public.pem
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-verify-copy-pubkey PROPERTIES
    DEPENDS H5SIGN-verify-setup-keystore
    FIXTURES_REQUIRED H5SIGN_keys
    FIXTURES_SETUP H5SIGN_keystore
  )

  # ---- Signed plugin preparation (fixture: H5SIGN_signed_plugins) ----

  # Create unsigned plugin for negative test
  add_test (
    NAME H5SIGN-verify-copy-unsigned
    COMMAND ${CMAKE_COMMAND} -E copy plugin_medium.so plugin_unsigned.so
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-verify-copy-unsigned PROPERTIES
    FIXTURES_REQUIRED H5SIGN_testfiles
    FIXTURES_SETUP H5SIGN_signed_plugins
  )

  # Copy plugin_small.so to a private file for the verification sign test,
  # so it does not conflict with H5SIGN-sign_small which signs the same file.
  add_test (
    NAME H5SIGN-verify-copy-small-for-signing
    COMMAND ${CMAKE_COMMAND} -E copy plugin_small.so plugin_small_to_sign.so
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-verify-copy-small-for-signing PROPERTIES
    FIXTURES_REQUIRED "H5SIGN_testfiles;H5SIGN_keystore"
    FIXTURES_SETUP H5SIGN_signed_plugins
  )

  # Sign the private copy of the small plugin for verification tests
  add_test (
    NAME H5SIGN-verify-sign-plugins
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5sign> -p plugin_small_to_sign.so -k test_private.pem
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-verify-sign-plugins PROPERTIES
    DEPENDS H5SIGN-verify-copy-small-for-signing
    FIXTURES_REQUIRED "H5SIGN_testfiles;H5SIGN_keys"
    FIXTURES_SETUP H5SIGN_signed_plugins
  )

  # Rename signed plugin for verification test
  add_test (
    NAME H5SIGN-verify-rename-signed
    COMMAND ${CMAKE_COMMAND} -E copy plugin_small_to_sign.so plugin_signed.so
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-verify-rename-signed PROPERTIES
    DEPENDS H5SIGN-verify-sign-plugins
    FIXTURES_SETUP H5SIGN_signed_plugins
  )

  # Copy plugin_large.so to a private file for the cache sign test,
  # so it does not conflict with H5SIGN-sign_large which signs the same file.
  add_test (
    NAME H5SIGN-verify-copy-large-for-signing
    COMMAND ${CMAKE_COMMAND} -E copy plugin_large.so plugin_large_to_sign.so
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-verify-copy-large-for-signing PROPERTIES
    FIXTURES_REQUIRED "H5SIGN_testfiles;H5SIGN_keystore"
    FIXTURES_SETUP H5SIGN_signed_plugins
  )

  # Sign the private copy of the large plugin for cache tests
  add_test (
    NAME H5SIGN-verify-sign-cache-test
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5sign> -p plugin_large_to_sign.so -k test_private.pem
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-verify-sign-cache-test PROPERTIES
    DEPENDS H5SIGN-verify-copy-large-for-signing
    FIXTURES_REQUIRED "H5SIGN_testfiles;H5SIGN_keys"
    FIXTURES_SETUP H5SIGN_signed_plugins
  )

  # Rename signed plugin for cache test
  add_test (
    NAME H5SIGN-verify-rename-cache-test
    COMMAND ${CMAKE_COMMAND} -E copy plugin_large_to_sign.so plugin_cache_test.so
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-verify-rename-cache-test PROPERTIES
    DEPENDS H5SIGN-verify-sign-cache-test
    FIXTURES_SETUP H5SIGN_signed_plugins
  )

  # Create tampered plugin (sign then modify)
  add_test (
    NAME H5SIGN-verify-create-tampered
    COMMAND ${CMAKE_COMMAND}
      -DFILE=${PROJECT_BINARY_DIR}/testfiles/plugin_tampered.so
      -DSOURCE=${PROJECT_BINARY_DIR}/testfiles/plugin_signed.so
      -P ${HDF5_TOOLS_TEST_H5SIGN_SOURCE_DIR}/CreateTamperedPlugin.cmake
  )
  set_tests_properties (H5SIGN-verify-create-tampered PROPERTIES
    DEPENDS H5SIGN-verify-rename-signed
    FIXTURES_SETUP H5SIGN_signed_plugins
  )

  # ---- Run verification tests ----
  add_test (
    NAME H5SIGN-verify-tests
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5signverifytest>
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
  )
  set_tests_properties (H5SIGN-verify-tests PROPERTIES
    FIXTURES_REQUIRED "H5SIGN_testfiles;H5SIGN_keys;H5SIGN_keystore;H5SIGN_signed_plugins"
  )

else ()
  message(WARNING "OpenSSL executable not found - h5sign tests will be skipped")
endif ()
