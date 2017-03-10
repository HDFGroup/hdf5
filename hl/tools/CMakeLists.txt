cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_HL_TOOLS C CXX)

add_subdirectory (${HDF5_HL_TOOLS_SOURCE_DIR}/gif2h5 ${HDF5_HL_TOOLS_BINARY_DIR}/gif2h5)

add_subdirectory (${HDF5_HL_TOOLS_SOURCE_DIR}/h5watch ${HDF5_HL_TOOLS_BINARY_DIR}/h5watch)
