cmake_minimum_required (VERSION 3.26)
project (HDF5Examples_PYTHON)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
include (Py_sourcefiles.cmake)

if (H5EXAMPLE_BUILD_TESTING)
  set (${EXAMPLE_VARNAME}_PY_CLEANFILES
    compound.h5
    dset.h5
    gzip.h5
    hype.h5
    links.h5
    objref.h5
    regref.h5
    copy1.h5
    copy2.h5
    string.h5
    unlim.h5
    vlstring.h5
  )

   # Remove any output file left over from previous test run
  add_test (
      NAME ${EXAMPLE_VARNAME}_PY-clear-objects
      COMMAND    ${CMAKE_COMMAND} -E remove ${${EXAMPLE_VARNAME}_PY_CLEANFILES}
  )
  set_tests_properties (${EXAMPLE_VARNAME}_PY-clear-objects PROPERTIES
      FIXTURES_SETUP clear_${EXAMPLE_VARNAME}_PY
      WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
  )

  add_custom_target (${EXAMPLE_VARNAME}_PY-copy-objects ALL COMMENT "Copying Python source files")
  foreach (example_name ${examples})
    add_custom_command (
        TARGET     ${EXAMPLE_VARNAME}_PY-copy-objects
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${PROJECT_SOURCE_DIR}/${example_name}.py ${PROJECT_BINARY_DIR}/${example_name}.py
    )
  endforeach ()

  add_test (
      NAME ${EXAMPLE_VARNAME}_PY-clean-objects
      COMMAND    ${CMAKE_COMMAND} -E remove ${${EXAMPLE_VARNAME}_PY_CLEANFILES}
  )
  set_tests_properties (${EXAMPLE_VARNAME}_PY-clean-objects PROPERTIES
      FIXTURES_CLEANUP clear_${EXAMPLE_VARNAME}_PY
      WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
  )

  macro (ADD_H5_TEST testname)
    if (NOT HDF5_USING_ANALYSIS_TOOL)
      add_test (
          NAME ${EXAMPLE_VARNAME}_PY_${testname}
          COMMAND ${Python3_EXECUTABLE} ${testname}.py
          WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
      )
      set_tests_properties (${EXAMPLE_VARNAME}_PY_${testname} PROPERTIES FIXTURES_REQUIRED clear_${EXAMPLE_VARNAME}_PY)
      if (last_test)
        set_tests_properties (${EXAMPLE_VARNAME}_PY_${testname} PROPERTIES DEPENDS ${last_test})
      endif ()
      set (last_test "${EXAMPLE_VARNAME}_PY_${testname}")
    endif ()
  endmacro ()

  foreach (example_name ${examples})
    ADD_H5_TEST (${example_name})
  endforeach ()
endif ()
