cmake_minimum_required (VERSION 3.18)
project (HDF5_HL_TOOLS C)

  #-----------------------------------------------------------------------------
  #-- Option to build the High level GIF Tools
  #-----------------------------------------------------------------------------
  if (EXISTS "${HDF5_HL_SOURCE_DIR}/gif2h5" AND IS_DIRECTORY "${HDF5_HL_SOURCE_DIR}/gif2h5")
    option (HDF5_BUILD_HL_GIF_TOOLS  "Build HDF5 HL GIF Tools" OFF)
    if (HDF5_BUILD_HL_GIF_TOOLS)
      add_subdirectory (gif2h5)
    endif ()
  endif ()

add_subdirectory (h5watch)
