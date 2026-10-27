cmake_minimum_required (VERSION 3.18)

project (HDF5PackageConfigConsumer LANGUAGES C)

if (NOT DEFINED HDF5_CONFIG_LIB_COMPONENT)
  message (FATAL_ERROR "HDF5_CONFIG_LIB_COMPONENT must be set")
endif ()
if (NOT DEFINED HDF5_CONSUMER_COMPONENT)
  message (FATAL_ERROR "HDF5_CONSUMER_COMPONENT must be set")
endif ()
if (NOT HDF5_CONSUMER_MODE STREQUAL "required" AND
    NOT HDF5_CONSUMER_MODE STREQUAL "optional")
  message (FATAL_ERROR "HDF5_CONSUMER_MODE must be 'required' or 'optional'")
endif ()

if (HDF5_CONSUMER_MODE STREQUAL "required")
  find_package (HDF5 CONFIG REQUIRED
      COMPONENTS C ${HDF5_CONFIG_LIB_COMPONENT} ${HDF5_CONSUMER_COMPONENT}
  )
else ()
  find_package (HDF5 CONFIG REQUIRED
      COMPONENTS C ${HDF5_CONFIG_LIB_COMPONENT}
      OPTIONAL_COMPONENTS ${HDF5_CONSUMER_COMPONENT}
  )
  if (HDF5_${HDF5_CONFIG_LIB_COMPONENT}_${HDF5_CONSUMER_COMPONENT}_FOUND)
    message (FATAL_ERROR
        "Optional component ${HDF5_CONSUMER_COMPONENT} unexpectedly found")
  endif ()
endif ()

if (NOT HDF5_${HDF5_CONFIG_LIB_COMPONENT}_C_FOUND)
  message (FATAL_ERROR "Required C component was not found")
endif ()
