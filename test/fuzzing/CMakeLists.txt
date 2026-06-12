#-----------------------------------------------------------------------------
# libFuzzer harnesses for OSS-Fuzz. Enabled with -DHDF5_ENABLE_FUZZERS=ON.
#
# Each harness links the static HDF5 library plus a fuzzing engine: OSS-Fuzz
# provides one via the LIB_FUZZING_ENGINE environment variable; a standalone
# developer build falls back to -fsanitize=fuzzer. Build them all at once with
# the `oss-fuzz-fuzzers` target.
#-----------------------------------------------------------------------------

# The harnesses are C, but the fuzzing engine is C++ and we link with the C++
# driver; ensure CXX is enabled in this scope so CMAKE_CXX_LINK_EXECUTABLE is set
# (HDF5 only enables CXX inside its c++/ subdirectory).
enable_language (CXX)

set (HDF5_FUZZERS
    h5_read_fuzzer
    h5_extended_fuzzer
)

foreach (fuzzer ${HDF5_FUZZERS})
  add_executable (${fuzzer} "${CMAKE_CURRENT_SOURCE_DIR}/${fuzzer}.c")

  target_include_directories (${fuzzer} PRIVATE
      "${CMAKE_SOURCE_DIR}/src"
      "${CMAKE_BINARY_DIR}/src"
      "${CMAKE_SOURCE_DIR}/src/H5FDsubfiling"
  )

  target_link_libraries (${fuzzer} PRIVATE ${HDF5_LIB_TARGET} z)

  # libFuzzer supplies its own main(); link with the C++ driver.
  set_target_properties (${fuzzer} PROPERTIES LINKER_LANGUAGE CXX)

  if (DEFINED ENV{LIB_FUZZING_ENGINE})
    target_link_libraries (${fuzzer} PRIVATE "$ENV{LIB_FUZZING_ENGINE}")
  else ()
    target_compile_options (${fuzzer} PRIVATE -fsanitize=fuzzer)
    target_link_options (${fuzzer} PRIVATE -fsanitize=fuzzer)
  endif ()
endforeach ()

add_custom_target (oss-fuzz-fuzzers DEPENDS ${HDF5_FUZZERS})
