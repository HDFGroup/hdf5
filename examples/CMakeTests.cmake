
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################
  file (MAKE_DIRECTORY ${PROJECT_BINARY_DIR}/red ${PROJECT_BINARY_DIR}/blue ${PROJECT_BINARY_DIR}/u2w)
  if (BUILD_SHARED_LIBS)
    file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/H5EX-shared")
    file (MAKE_DIRECTORY ${PROJECT_BINARY_DIR}/H5EX-shared/red ${PROJECT_BINARY_DIR}/H5EX-shared/blue ${PROJECT_BINARY_DIR}/H5EX-shared/u2w)
  endif ()

  # Remove any output file left over from previous test run
  add_test (
      NAME EXAMPLES-clear-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove
          Attributes.h5
          btrees_file.h5
          cmprss.h5
          default_file.h5
          dset.h5
          extend.h5
          extlink_prefix_source.h5
          extlink_source.h5
          extlink_target.h5
          group.h5
          groups.h5
          hard_link.h5
          mount1.h5
          mount2.h5
          one_index_file.h5
          only_dspaces_and_attrs_file.h5
          only_huge_mesgs_file.h5
          REF_REG.h5
          refere.h5
          SDS.h5
          SDScompound.h5
          SDSextendible.h5
          Select.h5
          separate_indexes_file.h5
          small_lists_file.h5
          soft_link.h5
          subset.h5
          unix2win.h5
          blue/prefix_target.h5
          red/prefix_target.h5
          u2w/u2w_target.h5
          vds.h5
          vds-exc.h5
          vds-excalibur.h5
          vds-exclim.h5
          vds-percival.h5
          vds-percival-unlim.h5
          vds-percival-unlim-maxmin.h5
          a.h5
          b.h5
          c.h5
          d.h5
          vds-simpleIO.h5
          vds-eiger.h5
  )
  if (NOT "${last_test}" STREQUAL "")
    set_tests_properties (EXAMPLES-clear-objects PROPERTIES DEPENDS ${last_test})
  endif ()
  set (last_test "EXAMPLES-clear-objects")

  foreach (example ${examples})
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME EXAMPLES-${example} COMMAND $<TARGET_FILE:${example}>)
    else ()
      add_test (NAME EXAMPLES-${example} COMMAND "${CMAKE_COMMAND}"
          -D "TEST_PROGRAM=$<TARGET_FILE:${example}>"
          -D "TEST_ARGS:STRING="
          -D "TEST_EXPECT=0"
          -D "TEST_SKIP_COMPARE=TRUE"
          -D "TEST_OUTPUT=${example}.txt"
          #-D "TEST_REFERENCE=${example}.out"
          -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
          -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
    endif ()
    if (NOT "${last_test}" STREQUAL "")
      set_tests_properties (EXAMPLES-${example} PROPERTIES DEPENDS ${last_test})
    endif ()
    set (last_test "EXAMPLES-${example}")
  endforeach ()

  if (BUILD_SHARED_LIBS)
    # Remove any output file left over from previous test run
    add_test (
        NAME EXAMPLES-shared-clear-objects
        COMMAND    ${CMAKE_COMMAND}
            -E remove
            Attributes.h5
            btrees_file.h5
            cmprss.h5
            default_file.h5
            dset.h5
            extend.h5
            extlink_prefix_source.h5
            extlink_source.h5
            extlink_target.h5
            group.h5
            groups.h5
            hard_link.h5
            mount1.h5
            mount2.h5
            one_index_file.h5
            only_dspaces_and_attrs_file.h5
            only_huge_mesgs_file.h5
            REF_REG.h5
            refere.h5
            SDS.h5
            SDScompound.h5
            SDSextendible.h5
            Select.h5
            separate_indexes_file.h5
            small_lists_file.h5
            soft_link.h5
            subset.h5
            unix2win.h5
            vds.h5
            vds-exc.h5
            vds-excalibur.h5
            vds-exclim.h5
            vds-percival.h5
            vds-percival-unlim.h5
            vds-percival-unlim-maxmin.h5
            a.h5
            b.h5
            c.h5
            d.h5
            vds-simpleIO.h5
            vds-eiger.h5
            blue/prefix_target.h5
            red/prefix_target.h5
            u2w/u2w_target.h5
        WORKING_DIRECTORY
            ${PROJECT_BINARY_DIR}/H5EX-shared
    )
    if (NOT "${last_test}" STREQUAL "")
      set_tests_properties (EXAMPLES-shared-clear-objects PROPERTIES DEPENDS ${last_test})
    endif ()
    set (last_test "EXAMPLES-shared-clear-objects")

    foreach (example ${examples})
      if (HDF5_ENABLE_USING_MEMCHECKER)
        add_test (NAME EXAMPLES-shared-${example} COMMAND $<TARGET_FILE:${example}-shared>)
      else ()
        add_test (NAME EXAMPLES-shared-${example} COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:${example}>"
            -D "TEST_ARGS:STRING="
            -D "TEST_EXPECT=0"
            -D "TEST_SKIP_COMPARE=TRUE"
            -D "TEST_OUTPUT=${example}.txt"
            #-D "TEST_REFERENCE=${example}.out"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/H5EX-shared"
            -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
        )
      endif ()
      set_tests_properties (EXAMPLES-shared-${example} PROPERTIES WORKING_DIRECTORY ${PROJECT_BINARY_DIR}/H5EX-shared)
      if (NOT "${last_test}" STREQUAL "")
        set_tests_properties (EXAMPLES-shared-${example} PROPERTIES DEPENDS ${last_test})
      endif ()
      set (last_test "EXAMPLES-shared-${example}")
    endforeach ()
  endif ()

### Windows pops up a modal permission dialog on this test
  if (H5_HAVE_PARALLEL AND NOT WIN32)
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME EXAMPLES-ph5example COMMAND $<TARGET_FILE:ph5example>)
    else ()
      add_test (NAME EXAMPLES-ph5example COMMAND "${CMAKE_COMMAND}"
          -D "TEST_PROGRAM=$<TARGET_FILE:ph5example>"
          -D "TEST_ARGS:STRING="
          -D "TEST_EXPECT=0"
          -D "TEST_SKIP_COMPARE=TRUE"
          -D "TEST_OUTPUT=ph5example.txt"
          #-D "TEST_REFERENCE=ph5example.out"
          -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
          -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
    endif ()
    if (NOT "${last_test}" STREQUAL "")
      set_tests_properties (EXAMPLES-ph5example PROPERTIES DEPENDS ${last_test})
    endif ()
    set (last_test "EXAMPLES-ph5example")
    if (BUILD_SHARED_LIBS)
      if (HDF5_ENABLE_USING_MEMCHECKER)
        add_test (NAME EXAMPLES-shared-ph5example COMMAND $<TARGET_FILE:ph5example-shared>)
      else ()
        add_test (NAME EXAMPLES-shared-ph5example COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:ph5example-shared>"
            -D "TEST_ARGS:STRING="
            -D "TEST_EXPECT=0"
            -D "TEST_SKIP_COMPARE=TRUE"
            -D "TEST_OUTPUT=ph5example-shared.txt"
            #-D "TEST_REFERENCE=ph5example-shared.out"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/H5EX-shared"
            -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
        )
      endif ()
      set_tests_properties (EXAMPLES-shared-ph5example PROPERTIES WORKING_DIRECTORY ${PROJECT_BINARY_DIR}/H5EX-shared)
      if (NOT "${last_test}" STREQUAL "")
        set_tests_properties (EXAMPLES-shared-ph5example PROPERTIES DEPENDS ${last_test})
      endif ()
      set (last_test "EXAMPLES-shared-ph5example")
    endif ()
  endif ()
