cmake_minimum_required (VERSION 3.10)
project (HDF5_JAVA_HDF_HDF5 Java)

set (CMAKE_VERBOSE_MAKEFILE 1)

set_directory_properties(PROPERTIES INCLUDE_DIRECTORIES "${HDF5_JAVA_HDF_HDF5_SOURCE_DIR};${HDF5_JAVA_HDF_HDF5_BINARY_DIR};${HDF5_JAVA_LIB_DIR};${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/${HDF5_JAVA_HDF5_LIB_CORENAME}.dir/hdf/hdf5lib")

SET_GLOBAL_VARIABLE (HDF5_JAVA_SOURCE_PACKAGES
    "${HDF5_JAVA_SOURCE_PACKAGES};hdf.hdf5lib.callbacks;hdf.hdf5lib.exceptions;hdf.hdf5lib.structs;hdf.hdf5lib"
)

set (HDF5_JAVA_HDF_HDF5_CALLBACKS_SOURCES
    callbacks/H5A_iterate_cb.java
    callbacks/H5A_iterate_t.java
    callbacks/H5D_append_cb.java
    callbacks/H5D_append_t.java
    callbacks/H5D_iterate_cb.java
    callbacks/H5D_iterate_t.java
    callbacks/H5E_walk_cb.java
    callbacks/H5E_walk_t.java
    callbacks/H5L_iterate_cb.java
    callbacks/H5L_iterate_t.java
    callbacks/H5O_iterate_cb.java
    callbacks/H5O_iterate_t.java
    callbacks/H5P_cls_close_func_cb.java
    callbacks/H5P_cls_close_func_t.java
    callbacks/H5P_cls_copy_func_cb.java
    callbacks/H5P_cls_copy_func_t.java
    callbacks/H5P_cls_create_func_cb.java
    callbacks/H5P_cls_create_func_t.java
    callbacks/H5P_prp_close_func_cb.java
    callbacks/H5P_prp_compare_func_cb.java
    callbacks/H5P_prp_copy_func_cb.java
    callbacks/H5P_prp_create_func_cb.java
    callbacks/H5P_prp_delete_func_cb.java
    callbacks/H5P_prp_get_func_cb.java
    callbacks/H5P_prp_set_func_cb.java
    callbacks/H5P_iterate_cb.java
    callbacks/H5P_iterate_t.java
    callbacks/Callbacks.java
)

set (HDF5_JAVA_HDF_HDF5_EXCEPTIONS_SOURCES
    exceptions/HDF5Exception.java
    exceptions/HDF5AtomException.java
    exceptions/HDF5AttributeException.java
    exceptions/HDF5BtreeException.java
    exceptions/HDF5DataFiltersException.java
    exceptions/HDF5DatasetInterfaceException.java
    exceptions/HDF5DataspaceInterfaceException.java
    exceptions/HDF5DataStorageException.java
    exceptions/HDF5DatatypeInterfaceException.java
    exceptions/HDF5ExternalFileListException.java
    exceptions/HDF5FileInterfaceException.java
    exceptions/HDF5FunctionArgumentException.java
    exceptions/HDF5FunctionEntryExitException.java
    exceptions/HDF5HeapException.java
    exceptions/HDF5InternalErrorException.java
    exceptions/HDF5JavaException.java
    exceptions/HDF5LibraryException.java
    exceptions/HDF5LowLevelIOException.java
    exceptions/HDF5MetaDataCacheException.java
    exceptions/HDF5ObjectHeaderException.java
    exceptions/HDF5PropertyListInterfaceException.java
    exceptions/HDF5ReferenceException.java
    exceptions/HDF5ResourceUnavailableException.java
    exceptions/HDF5SymbolTableException.java
)

set (HDF5_JAVA_HDF_HDF5_STRUCTS_SOURCES
    structs/H5_ih_info_t.java
    structs/H5A_info_t.java
    structs/H5AC_cache_config_t.java
    structs/H5E_error2_t.java
    structs/H5F_info2_t.java
    structs/H5FD_ros3_fapl_t.java
    structs/H5FD_hdfs_fapl_t.java
    structs/H5G_info_t.java
    structs/H5L_info_t.java
    structs/H5O_hdr_info_t.java
    structs/H5O_info_t.java
)

set (HDF5_JAVA_HDF_HDF5_SOURCES
    HDFArray.java
    HDF5Constants.java
    HDF5GroupInfo.java
    HDFNativeData.java
    H5.java
)

set (CMAKE_JNI_TARGET TRUE)

file (WRITE ${PROJECT_BINARY_DIR}/Manifest.txt
"
"
)

set (CMAKE_JAVA_INCLUDE_PATH "${HDF5_JAVA_LOGGING_JAR}")

add_jar (${HDF5_JAVA_HDF5_LIB_TARGET} OUTPUT_NAME "${HDF5_JAVA_HDF5_LIB_TARGET}-${HDF5_PACKAGE_VERSION}" OUTPUT_DIR ${CMAKE_JAVA_TARGET_OUTPUT_DIR} MANIFEST ${PROJECT_BINARY_DIR}/Manifest.txt ${HDF5_JAVA_HDF_HDF5_CALLBACKS_SOURCES} ${HDF5_JAVA_HDF_HDF5_EXCEPTIONS_SOURCES} ${HDF5_JAVA_HDF_HDF5_STRUCTS_SOURCES} ${HDF5_JAVA_HDF_HDF5_SOURCES})
install_jar (${HDF5_JAVA_HDF5_LIB_TARGET} LIBRARY DESTINATION ${HDF5_INSTALL_JAR_DIR} COMPONENT libraries)
#if (NOT WIN32)
#  install_jni_symlink (${HDF5_JAVA_HDF5_LIB_TARGET} ${HDF5_INSTALL_JAR_DIR} libraries)
#endif ()

get_target_property (${HDF5_JAVA_HDF5_LIB_TARGET}_JAR_FILE ${HDF5_JAVA_HDF5_LIB_TARGET} JAR_FILE)
SET_GLOBAL_VARIABLE (HDF5_JAVA_JARS_TO_EXPORT "${HDF5_JAVA_JARS_TO_EXPORT};${${HDF5_JAVA_HDF5_LIB_TARGET}_JAR_FILE}")
SET_GLOBAL_VARIABLE (HDF5_JAVA_JARS ${${HDF5_JAVA_HDF5_LIB_TARGET}_JAR_FILE})

add_dependencies (${HDF5_JAVA_HDF5_LIB_TARGET} ${HDF5_JAVA_JNI_LIB_TARGET})
set_target_properties (${HDF5_JAVA_HDF5_LIB_TARGET} PROPERTIES FOLDER libraries/java)

create_javadoc(hdf5_java_doc
    FILES ${HDF5_JAVA_HDF_HDF5_CALLBACKS_SOURCES} ${HDF5_JAVA_HDF_HDF5_EXCEPTIONS_SOURCES} ${HDF5_JAVA_HDF_HDF5_STRUCTS_SOURCES} ${HDF5_JAVA_HDF_HDF5_SOURCES}
    OVERVIEW ${HDF5_JAVA_HDF5_SRC_DIR}/overview.html
    CLASSPATH ${CMAKE_JAVA_INCLUDE_PATH}
    WINDOWTITLE "HDF5 Java"
    DOCTITLE "<h1>HDF5 Java Wrapper</h1>"
    INSTALLPATH ${HDF5_INSTALL_DATA_DIR}
    AUTHOR TRUE
    USE TRUE
    VERSION TRUE
)
