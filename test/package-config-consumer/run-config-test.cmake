if (NOT DEFINED HDF5_BINARY_DIR OR NOT DEFINED INSTALL_PREFIX)
  message (FATAL_ERROR "HDF5_BINARY_DIR and INSTALL_PREFIX must be set")
endif ()
if (NOT DEFINED CONSUMER_SOURCE_DIR OR NOT DEFINED CONSUMER_BINARY_DIR)
  message (FATAL_ERROR "CONSUMER_SOURCE_DIR and CONSUMER_BINARY_DIR must be set")
endif ()
if (NOT DEFINED HDF5_CONFIG_LIB_COMPONENT)
  message (FATAL_ERROR "HDF5_CONFIG_LIB_COMPONENT must be set")
endif ()
if (NOT DEFINED HDF5_CONSUMER_COMPONENT OR NOT DEFINED HDF5_CONSUMER_MODE)
  message (FATAL_ERROR "HDF5_CONSUMER_COMPONENT and HDF5_CONSUMER_MODE must be set")
endif ()

execute_process (
    COMMAND "${CMAKE_COMMAND}" --install "${HDF5_BINARY_DIR}" --prefix "${INSTALL_PREFIX}"
    RESULT_VARIABLE _install_result
    OUTPUT_VARIABLE _install_stdout
    ERROR_VARIABLE _install_stderr
)
if (NOT _install_result EQUAL 0)
  message (FATAL_ERROR
      "HDF5 installation failed:\n${_install_stdout}\n${_install_stderr}")
endif ()

file (GLOB_RECURSE _config_files "${INSTALL_PREFIX}/*/hdf5-config.cmake")
list (LENGTH _config_files _config_count)
if (NOT _config_count EQUAL 1)
  message (FATAL_ERROR
      "Expected one installed hdf5-config.cmake, found ${_config_count}: ${_config_files}")
endif ()
list (GET _config_files 0 _config_file)
get_filename_component (_config_dir "${_config_file}" DIRECTORY)

execute_process (
    COMMAND "${CMAKE_COMMAND}"
        -S "${CONSUMER_SOURCE_DIR}"
        -B "${CONSUMER_BINARY_DIR}"
        -D "HDF5_DIR=${_config_dir}"
        -D "HDF5_CONFIG_LIB_COMPONENT=${HDF5_CONFIG_LIB_COMPONENT}"
        -D "HDF5_CONSUMER_COMPONENT=${HDF5_CONSUMER_COMPONENT}"
        -D "HDF5_CONSUMER_MODE=${HDF5_CONSUMER_MODE}"
    RESULT_VARIABLE _configure_result
    OUTPUT_VARIABLE _configure_stdout
    ERROR_VARIABLE _configure_stderr
)
set (_configure_output "${_configure_stdout}\n${_configure_stderr}")

if (HDF5_CONSUMER_MODE STREQUAL "required")
  if (_configure_result EQUAL 0)
    message (FATAL_ERROR
        "Required missing component ${HDF5_CONSUMER_COMPONENT} was accepted:\n${_configure_output}")
  endif ()
  if (NOT _configure_output MATCHES
      "The ${HDF5_CONSUMER_COMPONENT} component is required, but not found")
    message (FATAL_ERROR
        "Configuration failed for an unexpected reason:\n${_configure_output}")
  endif ()
else ()
  if (NOT _configure_result EQUAL 0)
    message (FATAL_ERROR
        "Optional missing component ${HDF5_CONSUMER_COMPONENT} was rejected:\n${_configure_output}")
  endif ()
endif ()
