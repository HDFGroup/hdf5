#-----------------------------------------------------------------------------
# Shared Java javadoc generation for hdf.hdf5lib bindings.
# Used by FFM and JNI CMakeLists.txt when HDF5_BUILD_DOC is enabled.
#-----------------------------------------------------------------------------

# Version aligned with java/lib/native-lib-loader-2.5.0.jar and pom.xml.in
set (HDF5_JAVA_NATIVE_LIB_LOADER_JAVADOC_VERSION "2.5.0")
set (HDF5_JAVA_NATIVE_LIB_LOADER_JAVADOC_LINK
  "https://javadoc.io/doc/org.scijava/native-lib-loader/${HDF5_JAVA_NATIVE_LIB_LOADER_JAVADOC_VERSION}/"
)

function (hdf5_java_add_javadoc_target CLASSPATH_JARS)
  if (NOT HDF5_BUILD_DOC)
    return ()
  endif ()
  if (TARGET hdf5_java_doc)
    return ()
  endif ()

  if (NOT Java_JAVADOC_EXECUTABLE)
    find_package (Java COMPONENTS Development)
    if (NOT Java_JAVADOC_EXECUTABLE)
      message (WARNING "Java javadoc executable not found; skipping hdf5_java_doc target")
      return ()
    endif ()
  endif ()

  set (_javadoc_sources
    ${HDF5_JAVADOC_HDF_HDF5_CALLBACKS_SOURCES}
    ${HDF5_JAVADOC_HDF_HDF5_EXCEPTIONS_SOURCES}
    ${HDF5_JAVADOC_HDF_HDF5_STRUCTS_SOURCES}
    ${HDF5_JAVADOC_HDF_HDF5_SOURCES}
  )

  # Callers pass the classpath jars as a CMake list (";"-separated). javadoc
  # expects the platform-native path separator, which is ":" on Unix-like hosts
  # and ";" on Windows. Normalize so the jars resolve on every platform.
  if (CMAKE_HOST_UNIX)
    string (REPLACE ";" ":" _javadoc_classpath "${CLASSPATH_JARS}")
  else ()
    set (_javadoc_classpath "${CLASSPATH_JARS}")
  endif ()

  set (_javadoc_builddir "${CMAKE_CURRENT_BINARY_DIR}/javadoc/hdf5_java_doc")
  set (_javadoc_stamp "${_javadoc_builddir}/.javadoc.stamp")

  set (_javadoc_depends
    ${_javadoc_sources}
    ${HDF5_JAVA_LOGGING_JAR}
    ${HDF5_JAVA_NATIVE_LIB_LOADER_JAR}
  )
  if (HDF5_JAVAHDF5_JARS)
    list (APPEND _javadoc_depends ${HDF5_JAVAHDF5_JARS})
  endif ()

  add_custom_command (
    OUTPUT "${_javadoc_stamp}"
    COMMAND ${CMAKE_COMMAND} -E make_directory "${_javadoc_builddir}"
    COMMAND ${Java_JAVADOC_EXECUTABLE}
      -d "${_javadoc_builddir}"
      -classpath "${_javadoc_classpath}"
      -link "${HDF5_JAVA_NATIVE_LIB_LOADER_JAVADOC_LINK}"
      -tag "defgroup:a:Group:"
      -tag "ingroup:a:Group:"
      -tag "ref:a:See:"
      -tag "note:a:Note:"
      -tag "example:a:Example:"
      -overview "${HDF5_SOURCE_DIR}/java/src-jni/hdf/overview.html"
      -windowtitle "HDF5 Java"
      -doctitle "<h1>HDF5 Java Wrapper</h1>"
      -author
      -use
      -version
      ${_javadoc_sources}
    COMMAND ${CMAKE_COMMAND} -E touch "${_javadoc_stamp}"
    DEPENDS ${_javadoc_depends}
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
    COMMENT "Generating HDF5 Java API documentation with javadoc"
    VERBATIM
  )

  add_custom_target (hdf5_java_doc DEPENDS "${_javadoc_stamp}")
  set_target_properties (hdf5_java_doc PROPERTIES FOLDER documentation)

  install (
    DIRECTORY "${_javadoc_builddir}/"
    DESTINATION "${HDF5_INSTALL_DOC_DIR}/java"
    COMPONENT hdfdocuments
  )
endfunction ()
