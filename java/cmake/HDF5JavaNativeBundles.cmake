#-----------------------------------------------------------------------------
# HDF5JavaNativeBundles.cmake - Package native shared libraries as Maven JARs
#
# Include when HDF5_ENABLE_MAVEN_DEPLOY is ON. Before include, set:
#   HDF5JAVA_MAVEN_NATIVE_JNI - TRUE to add hdf5-jni-native jar (JNI builds only)
#
# Requires: HDF5_MAVEN_PLATFORM, HDF5_MAVEN_ARCHITECTURE, HDF5_JAR_CLASSIFIER,
#           HDF5_PACKAGE_VERSION, HDF5_MAVEN_VERSION_SUFFIX, HDF5_LIBSH_TARGET,
#           HDF5_INSTALL_JAR_DIR, Java_JAR_EXECUTABLE (from FindJava)
# Natives are staged under natives/<platform>/ per SciJava native-lib-loader
# (org.scijava.nativelib.NativeLibraryUtil.Architecture names, lowercased).
#-----------------------------------------------------------------------------

if (NOT DEFINED HDF5JAVA_MAVEN_NATIVE_JNI)
  set (HDF5JAVA_MAVEN_NATIVE_JNI FALSE)
endif ()

if (NOT DEFINED HDF5_MAVEN_PLATFORM OR NOT DEFINED HDF5_MAVEN_ARCHITECTURE)
  message (FATAL_ERROR "HDF5JavaNativeBundles.cmake requires HDF5_MAVEN_PLATFORM and HDF5_MAVEN_ARCHITECTURE")
endif ()

# Map HDF5 Maven classifiers to native-lib-loader platform directory names
if (HDF5_MAVEN_PLATFORM STREQUAL "linux")
  if (HDF5_MAVEN_ARCHITECTURE STREQUAL "aarch64")
    set (HDF5_NATIVE_LOADER_PLATFORM "linux_arm64")
  elseif (HDF5_MAVEN_ARCHITECTURE STREQUAL "x86_64")
    set (HDF5_NATIVE_LOADER_PLATFORM "linux_64")
  elseif (HDF5_MAVEN_ARCHITECTURE STREQUAL "x86")
    set (HDF5_NATIVE_LOADER_PLATFORM "linux_32")
  else ()
    set (HDF5_NATIVE_LOADER_PLATFORM "linux_64")
  endif ()
elseif (HDF5_MAVEN_PLATFORM STREQUAL "windows")
  if (HDF5_MAVEN_ARCHITECTURE STREQUAL "aarch64")
    set (HDF5_NATIVE_LOADER_PLATFORM "windows_arm64")
  else ()
    set (HDF5_NATIVE_LOADER_PLATFORM "windows_64")
  endif ()
elseif (HDF5_MAVEN_PLATFORM STREQUAL "macos")
  if (HDF5_MAVEN_ARCHITECTURE STREQUAL "aarch64")
    set (HDF5_NATIVE_LOADER_PLATFORM "osx_arm64")
  else ()
    set (HDF5_NATIVE_LOADER_PLATFORM "osx_64")
  endif ()
else ()
  message (FATAL_ERROR "HDF5JavaNativeBundles.cmake: unknown HDF5_MAVEN_PLATFORM '${HDF5_MAVEN_PLATFORM}'")
endif ()

set (_HDF5_NATIVE_IMPL_VERSION "${HDF5_PACKAGE_VERSION}${HDF5_MAVEN_VERSION_SUFFIX}")
set (HDF5_MAVEN_ZLIB_NATIVE_PROFILES "")
set (HDF5_MAVEN_SZIP_NATIVE_PROFILES "")

if (NOT Java_JAR_EXECUTABLE AND Java_JAVA_EXECUTABLE)
  get_filename_component (_jbin "${Java_JAVA_EXECUTABLE}" DIRECTORY)
  if (EXISTS "${_jbin}/jar${CMAKE_EXECUTABLE_SUFFIX}")
    set (Java_JAR_EXECUTABLE "${_jbin}/jar${CMAKE_EXECUTABLE_SUFFIX}")
  elseif (EXISTS "${_jbin}/jar.exe")
    set (Java_JAR_EXECUTABLE "${_jbin}/jar.exe")
  endif ()
endif ()

if (NOT Java_JAR_EXECUTABLE)
  message (FATAL_ERROR "Java jar tool not found; cannot build hdf5-native Maven bundles.")
endif ()

# Write a SciJava native-bundle manifest (Implementation-Version drives extraction cache keys).
function (hdf5java_write_native_manifest _manifest_path _bundle_name)
  file (WRITE "${_manifest_path}"
"Manifest-Version: 1.0\nImplementation-Version: ${_HDF5_NATIVE_IMPL_VERSION}\nHDF5-Native-Bundle: ${_bundle_name}\nHDF5-Classifier: ${HDF5_JAR_CLASSIFIER}\nHDF5-Native-Loader-Platform: ${HDF5_NATIVE_LOADER_PLATFORM}\n"
  )
endfunction ()

# Append one OS-activated Maven profile dependency block for a native artifact.
function (hdf5java_append_maven_native_profile _profiles_var _artifact_id _profile_id _family _os_name _arch _classifier _optional)
  if (_optional)
    set (_optional_xml "                    <optional>true</optional>\n")
  else ()
    set (_optional_xml "")
  endif ()
  if (_os_name)
    set (_os_name_xml "<name>${_os_name}</name>")
  else ()
    set (_os_name_xml "")
  endif ()
  set (_chunk "
        <profile>
            <id>${_profile_id}</id>
            <activation>
                <os><family>${_family}</family>${_os_name_xml}<arch>${_arch}</arch></os>
            </activation>
            <dependencies>
                <dependency>
                    <groupId>org.hdfgroup</groupId>
                    <artifactId>${_artifact_id}</artifactId>
                    <version>\${project.version}</version>
                    <classifier>${_classifier}</classifier>
${_optional_xml}                </dependency>
            </dependencies>
        </profile>")
  set (${_profiles_var} "${${_profiles_var}}${_chunk}" PARENT_SCOPE)
endfunction ()

# Generate five platform OS-activated profiles for a classified native artifact.
function (hdf5java_generate_maven_native_profiles _out_var _artifact_id _profile_prefix _comment _optional)
  set (_profiles "${_comment}")
  hdf5java_append_maven_native_profile (_profiles "${_artifact_id}" "${_profile_prefix}-linux-x86_64" "unix" "Linux" "amd64" "linux-x86_64" "${_optional}")
  hdf5java_append_maven_native_profile (_profiles "${_artifact_id}" "${_profile_prefix}-linux-aarch64" "unix" "Linux" "aarch64" "linux-aarch64" "${_optional}")
  hdf5java_append_maven_native_profile (_profiles "${_artifact_id}" "${_profile_prefix}-windows-amd64" "windows" "" "amd64" "windows-x86_64" "${_optional}")
  hdf5java_append_maven_native_profile (_profiles "${_artifact_id}" "${_profile_prefix}-macos-x86_64" "mac" "" "x86_64" "macos-x86_64" "${_optional}")
  hdf5java_append_maven_native_profile (_profiles "${_artifact_id}" "${_profile_prefix}-macos-aarch64" "mac" "" "aarch64" "macos-aarch64" "${_optional}")
  set (${_out_var} "${_profiles}" PARENT_SCOPE)
endfunction ()

# Stage shared library(ies) under natives/<platform>/, jar, install JAR + POM.
function (hdf5java_add_native_maven_jar)
  set (_options "")
  set (_oneValueArgs
    ARTIFACT_ID
    BUNDLE_NAME
    JAR_OUT
    STAGE_DIR
    NATIVES_PREFIX
    MANIFEST_PATH
    POM_TEMPLATE
    POM_OUT
    TARGET_NAME
    COMMENT
  )
  set (_multiValueArgs COPY_COMMANDS DEPENDS)
  cmake_parse_arguments (_args "${_options}" "${_oneValueArgs}" "${_multiValueArgs}" ${ARGN})

  hdf5java_write_native_manifest ("${_args_MANIFEST_PATH}" "${_args_BUNDLE_NAME}")

  add_custom_command (
    OUTPUT "${_args_JAR_OUT}"
    COMMAND ${CMAKE_COMMAND} -E rm -rf "${_args_STAGE_DIR}"
    COMMAND ${CMAKE_COMMAND} -E make_directory "${_args_NATIVES_PREFIX}"
    COMMAND ${CMAKE_COMMAND} -E make_directory "${_args_STAGE_DIR}/META-INF"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${_args_MANIFEST_PATH}" "${_args_STAGE_DIR}/META-INF/MANIFEST.MF"
    ${_args_COPY_COMMANDS}
    COMMAND ${Java_JAR_EXECUTABLE} cfm "${_args_JAR_OUT}" "${_args_STAGE_DIR}/META-INF/MANIFEST.MF" -C "${_args_STAGE_DIR}" .
    DEPENDS ${_args_DEPENDS}
    COMMENT "${_args_COMMENT}"
    VERBATIM
  )

  add_custom_target (${_args_TARGET_NAME} ALL DEPENDS "${_args_JAR_OUT}")
  set_target_properties (${_args_TARGET_NAME} PROPERTIES FOLDER libraries/java)

  install (
    FILES "${_args_JAR_OUT}"
    DESTINATION ${HDF5_INSTALL_JAR_DIR}
    COMPONENT maven
  )

  configure_file (
    ${HDF5_SOURCE_DIR}/java/cmake/${_args_POM_TEMPLATE}
    ${CMAKE_CURRENT_BINARY_DIR}/${_args_POM_OUT}
    @ONLY
  )
  install (
    FILES ${CMAKE_CURRENT_BINARY_DIR}/${_args_POM_OUT}
    DESTINATION ${HDF5_INSTALL_JAR_DIR}
    COMPONENT maven
  )
endfunction ()

# SciJava BaseJniExtractor looks for System.mapLibraryName("hdf5") under natives/<platform>/.
if (HDF5_MAVEN_PLATFORM STREQUAL "linux")
  set (_HDF5_NATIVE_SCIJAVA_MAPPED_NAME "libhdf5.so")
elseif (HDF5_MAVEN_PLATFORM STREQUAL "macos")
  set (_HDF5_NATIVE_SCIJAVA_MAPPED_NAME "libhdf5.dylib")
elseif (HDF5_MAVEN_PLATFORM STREQUAL "windows")
  set (_HDF5_NATIVE_SCIJAVA_MAPPED_NAME "hdf5.dll")
else ()
  set (_HDF5_NATIVE_SCIJAVA_MAPPED_NAME "")
endif ()
if (_HDF5_NATIVE_SCIJAVA_MAPPED_NAME STREQUAL "")
  message (FATAL_ERROR "HDF5JavaNativeBundles.cmake: add SciJava mapped library name for platform '${HDF5_MAVEN_PLATFORM}'")
endif ()

set (_HDF5_NATIVE_MAVEN_JAR
  "${CMAKE_CURRENT_BINARY_DIR}/hdf5-native-${HDF5_PACKAGE_VERSION}${HDF5_MAVEN_VERSION_SUFFIX}-${HDF5_JAR_CLASSIFIER}.jar"
)
set (_HDF5_NATIVE_STAGE "${CMAKE_CURRENT_BINARY_DIR}/native-bundle/hdf5-native")
set (_HDF5_NATIVE_PREFIX "${_HDF5_NATIVE_STAGE}/natives/${HDF5_NATIVE_LOADER_PLATFORM}")
set (_HDF5_NATIVE_MANIFEST "${CMAKE_CURRENT_BINARY_DIR}/native-bundle/META-INF_MANIFEST_NATIVE.mf")

hdf5java_add_native_maven_jar (
  ARTIFACT_ID hdf5-native
  BUNDLE_NAME hdf5-native
  JAR_OUT "${_HDF5_NATIVE_MAVEN_JAR}"
  STAGE_DIR "${_HDF5_NATIVE_STAGE}"
  NATIVES_PREFIX "${_HDF5_NATIVE_PREFIX}"
  MANIFEST_PATH "${_HDF5_NATIVE_MANIFEST}"
  POM_TEMPLATE pom-native.xml.in
  POM_OUT pom-hdf5-native.xml
  TARGET_NAME hdf5_native_maven_jar
  COMMENT "Creating Maven native bundle hdf5-native (${HDF5_JAR_CLASSIFIER}, ${HDF5_NATIVE_LOADER_PLATFORM})"
  COPY_COMMANDS
    COMMAND ${CMAKE_COMMAND} -E copy_if_different $<TARGET_FILE:${HDF5_LIBSH_TARGET}> ${_HDF5_NATIVE_PREFIX}/
    COMMAND ${CMAKE_COMMAND} -E copy_if_different $<TARGET_FILE:${HDF5_LIBSH_TARGET}> ${_HDF5_NATIVE_PREFIX}/${_HDF5_NATIVE_SCIJAVA_MAPPED_NAME}
  DEPENDS ${HDF5_LIBSH_TARGET}
)

# Optional zlib JAR: libhdf5's built-in deflate filter needs libz when linked dynamically.
set (_HDF5_ZLIB_NATIVE_TARGET "")
if (H5_ZLIB_FOUND AND NOT HDF5_USE_ZLIB_STATIC)
  foreach (_zlib_candidate IN ITEMS ${H5_ZLIB_LIBRARY} ZLIB::ZLIB zlib zlib-ng)
    if (TARGET ${_zlib_candidate} AND _HDF5_ZLIB_NATIVE_TARGET STREQUAL "")
      set (_HDF5_ZLIB_NATIVE_TARGET "${_zlib_candidate}")
    endif ()
  endforeach ()
endif ()

if (NOT _HDF5_ZLIB_NATIVE_TARGET STREQUAL "")
  get_target_property (_HDF5_ZLIB_ALIASED_TARGET ${_HDF5_ZLIB_NATIVE_TARGET} ALIASED_TARGET)
  if (_HDF5_ZLIB_ALIASED_TARGET)
    set (_HDF5_ZLIB_NATIVE_TARGET "${_HDF5_ZLIB_ALIASED_TARGET}")
  endif ()

  get_target_property (_HDF5_ZLIB_TARGET_TYPE ${_HDF5_ZLIB_NATIVE_TARGET} TYPE)
  if (NOT _HDF5_ZLIB_TARGET_TYPE STREQUAL "STATIC_LIBRARY")
    if (HDF5_MAVEN_PLATFORM STREQUAL "linux")
      set (_HDF5_ZLIB_NATIVE_SCIJAVA_MAPPED_NAME "libz.so")
    elseif (HDF5_MAVEN_PLATFORM STREQUAL "macos")
      set (_HDF5_ZLIB_NATIVE_SCIJAVA_MAPPED_NAME "libz.dylib")
    elseif (HDF5_MAVEN_PLATFORM STREQUAL "windows")
      set (_HDF5_ZLIB_NATIVE_SCIJAVA_MAPPED_NAME "z.dll")
    else ()
      set (_HDF5_ZLIB_NATIVE_SCIJAVA_MAPPED_NAME "")
    endif ()

    if (_HDF5_ZLIB_NATIVE_SCIJAVA_MAPPED_NAME STREQUAL "")
      message (FATAL_ERROR "HDF5JavaNativeBundles.cmake: add SciJava mapped zlib name for platform '${HDF5_MAVEN_PLATFORM}'")
    endif ()

    set (_HDF5_ZLIB_NATIVE_STAGE "${CMAKE_CURRENT_BINARY_DIR}/native-bundle/hdf5-zlib-native")
    set (_HDF5_ZLIB_NATIVE_PREFIX "${_HDF5_ZLIB_NATIVE_STAGE}/natives/${HDF5_NATIVE_LOADER_PLATFORM}")

    set (_HDF5_ZLIB_NATIVE_EXTRA_ALIASES "")
    if (HDF5_MAVEN_PLATFORM STREQUAL "windows")
      list (APPEND _HDF5_ZLIB_NATIVE_EXTRA_ALIASES "zlib.dll" "zlib1.dll")
    elseif (HDF5_MAVEN_PLATFORM STREQUAL "linux")
      list (APPEND _HDF5_ZLIB_NATIVE_EXTRA_ALIASES "libz.so.1")
    elseif (HDF5_MAVEN_PLATFORM STREQUAL "macos")
      list (APPEND _HDF5_ZLIB_NATIVE_EXTRA_ALIASES "libz.1.dylib")
    endif ()

    set (_HDF5_ZLIB_NATIVE_ALIAS_COMMANDS "")
    foreach (_zlib_alias IN LISTS _HDF5_ZLIB_NATIVE_EXTRA_ALIASES)
      if (NOT _zlib_alias STREQUAL _HDF5_ZLIB_NATIVE_SCIJAVA_MAPPED_NAME)
        list (APPEND _HDF5_ZLIB_NATIVE_ALIAS_COMMANDS
          COMMAND ${CMAKE_COMMAND} -E copy_if_different
            $<TARGET_FILE:${_HDF5_ZLIB_NATIVE_TARGET}>
            ${_HDF5_ZLIB_NATIVE_PREFIX}/${_zlib_alias}
        )
      endif ()
    endforeach ()

    hdf5java_generate_maven_native_profiles (HDF5_MAVEN_ZLIB_NATIVE_PROFILES "hdf5-zlib-native"
      "hdf5-bundled-zlib-native"
      "        <!-- Bundled zlib JARs: libhdf5's deflate/GZIP filter depends on libz when linked dynamically -->" TRUE)

    set (_HDF5_ZLIB_NATIVE_MAVEN_JAR
      "${CMAKE_CURRENT_BINARY_DIR}/hdf5-zlib-native-${HDF5_PACKAGE_VERSION}${HDF5_MAVEN_VERSION_SUFFIX}-${HDF5_JAR_CLASSIFIER}.jar"
    )
    set (_HDF5_ZLIB_NATIVE_MANIFEST "${CMAKE_CURRENT_BINARY_DIR}/native-bundle/META-INF_MANIFEST_ZLIB_NATIVE.mf")

    hdf5java_add_native_maven_jar (
      ARTIFACT_ID hdf5-zlib-native
      BUNDLE_NAME hdf5-zlib-native
      JAR_OUT "${_HDF5_ZLIB_NATIVE_MAVEN_JAR}"
      STAGE_DIR "${_HDF5_ZLIB_NATIVE_STAGE}"
      NATIVES_PREFIX "${_HDF5_ZLIB_NATIVE_PREFIX}"
      MANIFEST_PATH "${_HDF5_ZLIB_NATIVE_MANIFEST}"
      POM_TEMPLATE pom-zlib-native.xml.in
      POM_OUT pom-hdf5-zlib-native.xml
      TARGET_NAME hdf5_zlib_native_maven_jar
      COMMENT "Creating Maven native bundle hdf5-zlib-native (${HDF5_JAR_CLASSIFIER}, ${HDF5_NATIVE_LOADER_PLATFORM})"
      COPY_COMMANDS
        COMMAND ${CMAKE_COMMAND} -E copy_if_different $<TARGET_FILE:${_HDF5_ZLIB_NATIVE_TARGET}> ${_HDF5_ZLIB_NATIVE_PREFIX}/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different $<TARGET_FILE:${_HDF5_ZLIB_NATIVE_TARGET}> ${_HDF5_ZLIB_NATIVE_PREFIX}/${_HDF5_ZLIB_NATIVE_SCIJAVA_MAPPED_NAME}
        ${_HDF5_ZLIB_NATIVE_ALIAS_COMMANDS}
      DEPENDS ${_HDF5_ZLIB_NATIVE_TARGET}
    )
  endif ()
endif ()

# Optional szip JAR: bundle libaec and libsz when libhdf5 uses dynamic SZIP support.
set (_HDF5_SZIP_LIBSZ_TARGET "")
set (_HDF5_SZIP_LIBAEC_TARGET "")
if (H5_SZIP_FOUND AND NOT HDF5_USE_LIBAEC_STATIC)
  foreach (_libsz_candidate IN ITEMS
      ${HDF_PACKAGE_NAMESPACE}sz-shared sz-shared libaec::sz)
    if (TARGET ${_libsz_candidate} AND _HDF5_SZIP_LIBSZ_TARGET STREQUAL "")
      set (_HDF5_SZIP_LIBSZ_TARGET "${_libsz_candidate}")
    endif ()
  endforeach ()
  foreach (_libaec_candidate IN ITEMS
      ${HDF_PACKAGE_NAMESPACE}aec-shared aec-shared libaec::aec)
    if (TARGET ${_libaec_candidate} AND _HDF5_SZIP_LIBAEC_TARGET STREQUAL "")
      set (_HDF5_SZIP_LIBAEC_TARGET "${_libaec_candidate}")
    endif ()
  endforeach ()
endif ()

if (NOT _HDF5_SZIP_LIBSZ_TARGET STREQUAL "" AND NOT _HDF5_SZIP_LIBAEC_TARGET STREQUAL "")
  get_target_property (_HDF5_SZIP_LIBSZ_ALIASED ${_HDF5_SZIP_LIBSZ_TARGET} ALIASED_TARGET)
  if (_HDF5_SZIP_LIBSZ_ALIASED)
    set (_HDF5_SZIP_LIBSZ_TARGET "${_HDF5_SZIP_LIBSZ_ALIASED}")
  endif ()
  get_target_property (_HDF5_SZIP_LIBAEC_ALIASED ${_HDF5_SZIP_LIBAEC_TARGET} ALIASED_TARGET)
  if (_HDF5_SZIP_LIBAEC_ALIASED)
    set (_HDF5_SZIP_LIBAEC_TARGET "${_HDF5_SZIP_LIBAEC_ALIASED}")
  endif ()

  get_target_property (_HDF5_SZIP_LIBSZ_TYPE ${_HDF5_SZIP_LIBSZ_TARGET} TYPE)
  get_target_property (_HDF5_SZIP_LIBAEC_TYPE ${_HDF5_SZIP_LIBAEC_TARGET} TYPE)
  if (NOT _HDF5_SZIP_LIBSZ_TYPE STREQUAL "STATIC_LIBRARY"
      AND NOT _HDF5_SZIP_LIBAEC_TYPE STREQUAL "STATIC_LIBRARY")
    if (HDF5_MAVEN_PLATFORM STREQUAL "linux")
      set (_HDF5_SZIP_NATIVE_SCIJAVA_LIBSZ "libsz.so")
      set (_HDF5_SZIP_NATIVE_SCIJAVA_LIBAEC "libaec.so")
    elseif (HDF5_MAVEN_PLATFORM STREQUAL "macos")
      set (_HDF5_SZIP_NATIVE_SCIJAVA_LIBSZ "libsz.dylib")
      set (_HDF5_SZIP_NATIVE_SCIJAVA_LIBAEC "libaec.dylib")
    elseif (HDF5_MAVEN_PLATFORM STREQUAL "windows")
      set (_HDF5_SZIP_NATIVE_SCIJAVA_LIBSZ "sz.dll")
      set (_HDF5_SZIP_NATIVE_SCIJAVA_LIBAEC "aec.dll")
    else ()
      set (_HDF5_SZIP_NATIVE_SCIJAVA_LIBSZ "")
      set (_HDF5_SZIP_NATIVE_SCIJAVA_LIBAEC "")
    endif ()

    if (_HDF5_SZIP_NATIVE_SCIJAVA_LIBSZ STREQUAL "" OR _HDF5_SZIP_NATIVE_SCIJAVA_LIBAEC STREQUAL "")
      message (FATAL_ERROR "HDF5JavaNativeBundles.cmake: add SciJava mapped libaec/libsz names for platform '${HDF5_MAVEN_PLATFORM}'")
    endif ()

    hdf5java_generate_maven_native_profiles (HDF5_MAVEN_SZIP_NATIVE_PROFILES "hdf5-szip-native"
      "hdf5-bundled-szip-native"
      "        <!-- Bundled libaec (szip) JARs: libhdf5's SZIP filter depends on libsz/libaec when linked dynamically -->" TRUE)

    set (_HDF5_SZIP_NATIVE_MAVEN_JAR
      "${CMAKE_CURRENT_BINARY_DIR}/hdf5-szip-native-${HDF5_PACKAGE_VERSION}${HDF5_MAVEN_VERSION_SUFFIX}-${HDF5_JAR_CLASSIFIER}.jar"
    )
    set (_HDF5_SZIP_NATIVE_STAGE "${CMAKE_CURRENT_BINARY_DIR}/native-bundle/hdf5-szip-native")
    set (_HDF5_SZIP_NATIVE_PREFIX "${_HDF5_SZIP_NATIVE_STAGE}/natives/${HDF5_NATIVE_LOADER_PLATFORM}")
    set (_HDF5_SZIP_NATIVE_MANIFEST "${CMAKE_CURRENT_BINARY_DIR}/native-bundle/META-INF_MANIFEST_SZIP_NATIVE.mf")

    hdf5java_add_native_maven_jar (
      ARTIFACT_ID hdf5-szip-native
      BUNDLE_NAME hdf5-szip-native
      JAR_OUT "${_HDF5_SZIP_NATIVE_MAVEN_JAR}"
      STAGE_DIR "${_HDF5_SZIP_NATIVE_STAGE}"
      NATIVES_PREFIX "${_HDF5_SZIP_NATIVE_PREFIX}"
      MANIFEST_PATH "${_HDF5_SZIP_NATIVE_MANIFEST}"
      POM_TEMPLATE pom-szip-native.xml.in
      POM_OUT pom-hdf5-szip-native.xml
      TARGET_NAME hdf5_szip_native_maven_jar
      COMMENT "Creating Maven native bundle hdf5-szip-native (${HDF5_JAR_CLASSIFIER}, ${HDF5_NATIVE_LOADER_PLATFORM})"
      COPY_COMMANDS
        COMMAND ${CMAKE_COMMAND} -E copy_if_different $<TARGET_FILE:${_HDF5_SZIP_LIBAEC_TARGET}> ${_HDF5_SZIP_NATIVE_PREFIX}/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different $<TARGET_FILE:${_HDF5_SZIP_LIBAEC_TARGET}> ${_HDF5_SZIP_NATIVE_PREFIX}/${_HDF5_SZIP_NATIVE_SCIJAVA_LIBAEC}
        COMMAND ${CMAKE_COMMAND} -E copy_if_different $<TARGET_FILE:${_HDF5_SZIP_LIBSZ_TARGET}> ${_HDF5_SZIP_NATIVE_PREFIX}/
        COMMAND ${CMAKE_COMMAND} -E copy_if_different $<TARGET_FILE:${_HDF5_SZIP_LIBSZ_TARGET}> ${_HDF5_SZIP_NATIVE_PREFIX}/${_HDF5_SZIP_NATIVE_SCIJAVA_LIBSZ}
      DEPENDS ${_HDF5_SZIP_LIBAEC_TARGET} ${_HDF5_SZIP_LIBSZ_TARGET}
    )
  endif ()
endif ()

# JNI bridge JAR (JNI Maven builds only)
if (HDF5JAVA_MAVEN_NATIVE_JNI)
  set (_HDF5_JNI_NATIVE_MAVEN_JAR
    "${CMAKE_CURRENT_BINARY_DIR}/hdf5-jni-native-${HDF5_PACKAGE_VERSION}${HDF5_MAVEN_VERSION_SUFFIX}-${HDF5_JAR_CLASSIFIER}.jar"
  )
  set (_HDF5_JNI_NATIVE_STAGE "${CMAKE_CURRENT_BINARY_DIR}/native-bundle/hdf5-jni-native")
  set (_HDF5_JNI_NATIVE_PREFIX "${_HDF5_JNI_NATIVE_STAGE}/natives/${HDF5_NATIVE_LOADER_PLATFORM}")
  set (_HDF5_JNI_NATIVE_MANIFEST "${CMAKE_CURRENT_BINARY_DIR}/native-bundle/META-INF_MANIFEST_JNI_NATIVE.mf")

  hdf5java_add_native_maven_jar (
    ARTIFACT_ID hdf5-jni-native
    BUNDLE_NAME hdf5-jni-native
    JAR_OUT "${_HDF5_JNI_NATIVE_MAVEN_JAR}"
    STAGE_DIR "${_HDF5_JNI_NATIVE_STAGE}"
    NATIVES_PREFIX "${_HDF5_JNI_NATIVE_PREFIX}"
    MANIFEST_PATH "${_HDF5_JNI_NATIVE_MANIFEST}"
    POM_TEMPLATE pom-jni-native.xml.in
    POM_OUT pom-hdf5-jni-native.xml
    TARGET_NAME hdf5_jni_native_maven_jar
    COMMENT "Creating Maven native bundle hdf5-jni-native (${HDF5_JAR_CLASSIFIER}, ${HDF5_NATIVE_LOADER_PLATFORM})"
    COPY_COMMANDS
      COMMAND ${CMAKE_COMMAND} -E copy_if_different $<TARGET_FILE:${HDF5_JAVA_JNI_LIB_TARGET}> ${_HDF5_JNI_NATIVE_PREFIX}/
    DEPENDS ${HDF5_JAVA_JNI_LIB_TARGET}
  )
endif ()
