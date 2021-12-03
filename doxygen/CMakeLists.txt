cmake_minimum_required (VERSION 3.12)
project (HDF5_DOXYGEN C)

#-----------------------------------------------------------------------------
# Option to build documentation
#-----------------------------------------------------------------------------
if (DOXYGEN_FOUND)
  set (DOXYGEN_PACKAGE ${HDF5_PACKAGE_NAME})
  set (DOXYGEN_VERSION_STRING ${HDF5_PACKAGE_VERSION_STRING})
  set (DOXYGEN_INCLUDE_ALIASES_PATH ${HDF5_DOXYGEN_DIR})
  set (DOXYGEN_INCLUDE_ALIASES aliases)
  set (DOXYGEN_VERBATIM_VARS DOXYGEN_INCLUDE_ALIASES)
  set (DOXYGEN_PROJECT_LOGO ${HDF5_DOXYGEN_DIR}/img/HDFG-logo.png)
  set (DOXYGEN_PROJECT_BRIEF "C-API Reference")
  set (DOXYGEN_INPUT_DIRECTORY "${HDF5_SOURCE_DIR} ${HDF5_DOXYGEN_DIR}/dox ${HDF5_GENERATED_SOURCE_DIR}")
  set (DOXYGEN_OPTIMIZE_OUTPUT_FOR_C YES)
  set (DOXYGEN_MACRO_EXPANSION YES)
  set (DOXYGEN_OUTPUT_DIRECTORY ${HDF5_BINARY_DIR}/hdf5lib_docs)
  set (DOXYGEN_EXAMPLES_DIRECTORY "${HDF5_DOXYGEN_DIR}/dox/cookbook ${HDF5_DOXYGEN_DIR}/examples ${HDF5_SRC_DIR} ${HDF5_SOURCE_DIR}/examples ${HDF5_TEST_SRC_DIR}")
  set (DOXYGEN_LAYOUT_FILE ${HDF5_DOXYGEN_DIR}/hdf5doxy_layout.xml)
  set (DOXYGEN_HTML_HEADER ${HDF5_DOXYGEN_DIR}/hdf5_header.html)
  set (DOXYGEN_HTML_FOOTER ${HDF5_DOXYGEN_DIR}/hdf5_footer.html)
  set (DOXYGEN_HTML_EXTRA_STYLESHEET ${HDF5_DOXYGEN_DIR}/hdf5doxy.css)
  set (DOXYGEN_HTML_EXTRA_FILES ${HDF5_DOXYGEN_DIR}/hdf5_navtree_hacks.js ${HDF5_DOXYGEN_DIR}/img/FF-IH_FileGroup.gif ${HDF5_DOXYGEN_DIR}/img/FF-IH_FileObject.gif ${HDF5_DOXYGEN_DIR}/img/FileFormatSpecChunkDiagram.jpg ${HDF5_DOXYGEN_DIR}/img/ftv2node.png ${HDF5_DOXYGEN_DIR}/img/ftv2pnode.png ${HDF5_DOXYGEN_DIR}/img/HDFG-logo.png ${HDF5_DOXYGEN_DIR}/img/IOFlow2.gif ${HDF5_DOXYGEN_DIR}/img/IOFlow3.gif ${HDF5_DOXYGEN_DIR}/img/IOFlow.gif ${HDF5_DOXYGEN_DIR}/img/PaletteExample1.gif ${HDF5_DOXYGEN_DIR}/img/Palettes.fm.anc.gif)
  set (DOXYGEN_TAG_FILE ${HDF5_BINARY_DIR}/hdf5.tag)
  set (DOXYGEN_SERVER_BASED_SEARCH NO)
  set (DOXYGEN_EXTERNAL_SEARCH NO)
  set (DOXYGEN_SEARCHENGINE_URL)
  set (DOXYGEN_STRIP_FROM_PATH ${HDF5_SOURCE_DIR})
  set (DOXYGEN_PREDEFINED "H5_HAVE_DIRECT H5_HAVE_LIBHDFS H5_HAVE_MAP_API H5_HAVE_PARALLEL H5_HAVE_ROS3_VFD")

# This configure and individual custom targets work together
  # Replace variables inside @@ with the current values
  configure_file (${HDF5_DOXYGEN_DIR}/Doxyfile.in ${HDF5_BINARY_DIR}/Doxyfile @ONLY)

  install (
      DIRECTORY ${HDF5_BINARY_DIR}/hdf5lib_docs/html
      DESTINATION ${HDF5_INSTALL_DATA_DIR}
      COMPONENT Documents
  )

  if (NOT TARGET doxygen)
    add_custom_target (doxygen)
  endif ()

endif ()
