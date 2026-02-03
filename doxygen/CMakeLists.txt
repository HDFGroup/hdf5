cmake_minimum_required (VERSION 3.26)
project (HDF5_DOXYGEN C)

#-----------------------------------------------------------------------------
# Option to build documentation
#-----------------------------------------------------------------------------
string (TIMESTAMP HDF5_PACKAGE_DATE_STRING "%Y-%m-%d")
if (DOXYGEN_FOUND)
  set (DOXYGEN_PACKAGE ${HDF5_PACKAGE_NAME})
  # For release builds use the version from the package
  # set (DOXYGEN_VERSION_STRING ${HDF5_PACKAGE_VERSION_STRING})
  set (DOXYGEN_VERSION_STRING "Last Updated on ${HDF5_PACKAGE_DATE_STRING}")
  set (DOXYGEN_DIR ${HDF5_DOXYGEN_DIR})
  set (DOXYGEN_INCLUDE_ALIASES_PATH ${HDF5_DOXYGEN_DIR})
  set (DOXYGEN_INCLUDE_ALIASES aliases)
  set (DOXYGEN_VERBATIM_VARS DOXYGEN_INCLUDE_ALIASES)
  set (DOXYGEN_PROJECT_LOGO ${HDF5_DOXYGEN_DIR}/img/HDFG-logo.png)
  set (DOXYGEN_PROJECT_BRIEF "The HDF5 Field Guide")
  set (DOXYGEN_INPUT_DIRECTORY "${HDF5_SOURCE_DIR} ${HDF5_DOXYGEN_DIR}/dox ${HDF5_GENERATED_SOURCE_DIR}")
  set (DOXYGEN_OPTIMIZE_OUTPUT_FOR_C YES)
  set (DOXYGEN_MACRO_EXPANSION YES)
  set (DOXYGEN_OUTPUT_DIRECTORY ${HDF5_BINARY_DIR}/hdf5lib_docs)
  set (DOXYGEN_EXAMPLES_DIRECTORY "${HDF5_DOXYGEN_DIR}/dox/cookbook ${HDF5_DOXYGEN_DIR}/examples ${HDF5_SRC_DIR} ${HDF5_SOURCE_DIR}/release_docs ${HDF5_TEST_SRC_DIR}")
  set (DOXYGEN_LAYOUT_FILE ${HDF5_DOXYGEN_DIR}/hdf5doxy_layout.xml)
  set (DOXYGEN_HTML_HEADER ${HDF5_DOXYGEN_DIR}/hdf5_header.html)
  set (DOXYGEN_HTML_FOOTER ${HDF5_DOXYGEN_DIR}/hdf5_footer.html)
  set (DOXYGEN_HTML_EXTRA_STYLESHEET "${HDF5_DOXYGEN_DIR}/hdf5doxy.css ${HDF5_DOXYGEN_DIR}/doxygen-awesome.css")
  set (DOXYGEN_HTML_EXTRA_FILES "${HDF5_DOXYGEN_DIR}/hdf5_navtree_hacks.js ${HDF5_DOXYGEN_DIR}/doxygen-awesome-tabs.js")
  set (DOXYGEN_TAG_FILE ${HDF5_BINARY_DIR}/hdf5.tag)
  set (DOXYGEN_SERVER_BASED_SEARCH NO)
  set (DOXYGEN_EXTERNAL_SEARCH NO)
  set (DOXYGEN_SEARCHENGINE_URL)
  set (DOXYGEN_STRIP_FROM_PATH ${HDF5_SOURCE_DIR})
  set (DOXYGEN_STRIP_FROM_INC_PATH ${HDF5_SOURCE_DIR})
  # Versioned API macros are added to PREDEFINED to give Doxygen a fixed
  # expansion and prevent it from discovering the #defines in H5version.h,
  # which otherwise causes "multiple @param documentation sections" warnings
  # when the macro documentation (\def) and the function documentation both
  # get associated with the same underlying function.
  set (_doxygen_predefined_entries
    H5_HAVE_DIRECT
    H5_HAVE_LIBHDFS
    H5_HAVE_MAP_API
    H5_HAVE_PARALLEL
    H5_HAVE_ROS3_VFD
    H5_DOXYGEN
    H5_HAVE_SUBFILING_VFD
    H5_HAVE_IOC_VFD
    H5_HAVE_MIRROR_VFD
    "H5std_string=std::string"
    "H5G_link_t=H5L_type_t"
    "H5Acreate=H5Acreate2"
    "H5Aiterate=H5Aiterate2"
    "H5Dcreate=H5Dcreate2"
    "H5Dopen=H5Dopen2"
    "H5Dread_chunk=H5Dread_chunk2"
    "H5Eclear=H5Eclear2"
    "H5Eget_auto=H5Eget_auto2"
    "H5Eprint=H5Eprint2"
    "H5Epush=H5Epush2"
    "H5Eset_auto=H5Eset_auto2"
    "H5Ewalk=H5Ewalk2"
    "H5Fget_info=H5Fget_info2"
    "H5Gcreate=H5Gcreate2"
    "H5Gopen=H5Gopen2"
    "H5Iregister_type=H5Iregister_type2"
    "H5Lget_info=H5Lget_info2"
    "H5Lget_info_by_idx=H5Lget_info_by_idx2"
    "H5Literate=H5Literate2"
    "H5Literate_by_name=H5Literate_by_name2"
    "H5Lvisit=H5Lvisit2"
    "H5Lvisit_by_name=H5Lvisit_by_name2"
    "H5Oget_info=H5Oget_info3"
    "H5Oget_info_by_idx=H5Oget_info_by_idx3"
    "H5Oget_info_by_name=H5Oget_info_by_name3"
    "H5Ovisit=H5Ovisit3"
    "H5Ovisit_by_name=H5Ovisit_by_name3"
    "H5Pencode=H5Pencode2"
    "H5Pget_filter=H5Pget_filter2"
    "H5Pget_filter_by_id=H5Pget_filter_by_id2"
    "H5Pinsert=H5Pinsert2"
    "H5Pregister=H5Pregister2"
    "H5Rdereference=H5Rdereference2"
    "H5Rget_obj_type=H5Rget_obj_type2"
    "H5Sencode=H5Sencode2"
    "H5Tarray_create=H5Tarray_create2"
    "H5Tcommit=H5Tcommit2"
    "H5Tdecode=H5Tdecode2"
    "H5Tget_array_dims=H5Tget_array_dims2"
    "H5Topen=H5Topen2"
  )
  list (JOIN _doxygen_predefined_entries " " DOXYGEN_PREDEFINED)
  set (DOXYGEN_WARN_AS_ERROR ${HDF5_DOXY_WARNINGS})

# This configure and individual custom targets work together
  # Replace variables inside @@ with the current values
  configure_file (${HDF5_DOXYGEN_DIR}/Doxyfile.in ${HDF5_BINARY_DIR}/Doxyfile @ONLY)

  install (
      DIRECTORY ${HDF5_BINARY_DIR}/hdf5lib_docs/html
      DESTINATION ${HDF5_INSTALL_DOC_DIR}
      COMPONENT hdfdocuments
  )
  install (
      FILES ${HDF5_BINARY_DIR}/hdf5.tag
      DESTINATION ${HDF5_INSTALL_DOC_DIR}
      COMPONENT hdfdocuments
  )

  if (NOT TARGET doxygen)
    add_custom_target (doxygen)
  endif ()

endif ()
