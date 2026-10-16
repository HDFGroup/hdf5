#-----------------------------------------------------------------------------
# HDF5JavaNativeBundles.cmake - Package native shared libraries as Maven JARs
#
# Include when HDF5_ENABLE_MAVEN_DEPLOY is ON. Before include, set:
#   HDF5JAVA_MAVEN_NATIVE_JNI - TRUE to add hdf5-jni-native jar (JNI builds only)
#
# Requires: HDF5_SOURCE_DIR, HDF5_MAVEN_PLATFORM, HDF5_MAVEN_ARCHITECTURE,
#           HDF5_JAR_CLASSIFIER, HDF5_PACKAGE_VERSION, HDF5_MAVEN_VERSION_SUFFIX,
#           HDF5_LIBSH_TARGET, HDF5_INSTALL_JAR_DIR, LINK_COMP_LIBS, H5_ZLIB_LIBRARY,
#           Java_JAR_EXECUTABLE (from FindJava), and HDF5_JAVA_JNI_LIB_TARGET when
#           HDF5JAVA_MAVEN_NATIVE_JNI is TRUE
#
# Sets HDF5_MAVEN_NATIVE_PROFILES, the OS-activated <profile> blocks substituted
# into the binding pom.xml.in files. hdf5-native in turn depends on the zlib and
# libaec bundles that libhdf5 links, so adding hdf5-native is enough to load it.
#
# Natives are staged under natives/<platform>/ per SciJava native-lib-loader
# (org.scijava.nativelib.NativeLibraryUtil.Architecture names, lowercased).
# The loader only extracts System.mapLibraryName(name), so each library is
# staged exactly once under that name: the unversioned linker name on Linux and
# macOS (libhdf5.so, libz.dylib, ...), which still carries the SONAME/install
# name that dependent libraries reference, and the DLL file name on Windows,
# which is the name recorded in the PE import table of dependent DLLs.
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
  elseif (HDF5_MAVEN_ARCHITECTURE STREQUAL "x86")
    set (HDF5_NATIVE_LOADER_PLATFORM "linux_32")
  else ()
    set (HDF5_NATIVE_LOADER_PLATFORM "linux_64")
  endif ()
elseif (HDF5_MAVEN_PLATFORM STREQUAL "windows")
  if (HDF5_MAVEN_ARCHITECTURE STREQUAL "aarch64")
    set (HDF5_NATIVE_LOADER_PLATFORM "windows_arm64")
  elseif (HDF5_MAVEN_ARCHITECTURE STREQUAL "x86")
    set (HDF5_NATIVE_LOADER_PLATFORM "windows_32")
  else ()
    set (HDF5_NATIVE_LOADER_PLATFORM "windows_64")
  endif ()
elseif (HDF5_MAVEN_PLATFORM STREQUAL "macos")
  if (HDF5_MAVEN_ARCHITECTURE STREQUAL "aarch64")
    set (HDF5_NATIVE_LOADER_PLATFORM "osx_arm64")
  elseif (HDF5_MAVEN_ARCHITECTURE STREQUAL "x86_64")
    set (HDF5_NATIVE_LOADER_PLATFORM "osx_64")
  else ()
    message (FATAL_ERROR "HDF5JavaNativeBundles.cmake: unsupported macOS architecture '${HDF5_MAVEN_ARCHITECTURE}'")
  endif ()
else ()
  message (FATAL_ERROR "HDF5JavaNativeBundles.cmake: unknown HDF5_MAVEN_PLATFORM '${HDF5_MAVEN_PLATFORM}'")
endif ()

if (NOT Java_JAR_EXECUTABLE AND Java_JAVA_EXECUTABLE)
  get_filename_component (_jbin "${Java_JAVA_EXECUTABLE}" DIRECTORY)
  if (EXISTS "${_jbin}/jar${CMAKE_EXECUTABLE_SUFFIX}")
    set (Java_JAR_EXECUTABLE "${_jbin}/jar${CMAKE_EXECUTABLE_SUFFIX}")
  endif ()
endif ()

if (NOT Java_JAR_EXECUTABLE)
  message (FATAL_ERROR "Java jar tool not found; cannot build hdf5-native Maven bundles.")
endif ()

# Generator expression for the file name SciJava extracts for a shared library target
function (hdf5java_native_stage_name _out_var _target)
  if (WIN32)
    set (${_out_var} "$<TARGET_FILE_NAME:${_target}>" PARENT_SCOPE)
  else ()
    set (${_out_var} "$<TARGET_LINKER_FILE_NAME:${_target}>" PARENT_SCOPE)
  endif ()
endfunction ()

# Append one OS-activated Maven profile that pulls in a classified native artifact.
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

# Append the per-platform profiles for one classified native artifact. Only list
# classifiers that the release matrix (maven-staging.yml) builds: a profile for an
# unpublished classifier makes dependency resolution fail on that platform.
function (hdf5java_append_maven_native_profiles _profiles_var _artifact_id _comment _optional)
  set (_profiles "${${_profiles_var}}\n        <!-- ${_comment} -->")
  hdf5java_append_maven_native_profile (_profiles "${_artifact_id}" "${_artifact_id}-linux-x86_64" "unix" "Linux" "amd64" "linux-x86_64" "${_optional}")
  hdf5java_append_maven_native_profile (_profiles "${_artifact_id}" "${_artifact_id}-windows-amd64" "windows" "" "amd64" "windows-x86_64" "${_optional}")
  hdf5java_append_maven_native_profile (_profiles "${_artifact_id}" "${_artifact_id}-macos-x86_64" "mac" "" "x86_64" "macos-x86_64" "${_optional}")
  hdf5java_append_maven_native_profile (_profiles "${_artifact_id}" "${_artifact_id}-macos-aarch64" "mac" "" "aarch64" "macos-aarch64" "${_optional}")
  set (${_profiles_var} "${_profiles}" PARENT_SCOPE)
endfunction ()

# Stage shared libraries under natives/<platform>/, jar them, and install JAR + POM.
#   ARTIFACT_ID  Maven artifactId (also the jar/POM/stage-dir base name)
#   POM_TEMPLATE template under java/cmake/
#   TARGETS      shared library targets to bundle, one copy each
#   LICENSE_FILE license of the bundled libraries, stored as META-INF/LICENSE (optional)
function (hdf5java_add_native_maven_jar)
  cmake_parse_arguments (_args "" "ARTIFACT_ID;POM_TEMPLATE;LICENSE_FILE" "TARGETS" ${ARGN})

  set (_base "${CMAKE_CURRENT_BINARY_DIR}/native-bundle/${_args_ARTIFACT_ID}")
  set (_stage "${_base}/stage")
  set (_natives "${_stage}/natives/${HDF5_NATIVE_LOADER_PLATFORM}")
  set (_manifest "${_base}/MANIFEST.MF")
  set (_jar "${CMAKE_CURRENT_BINARY_DIR}/${_args_ARTIFACT_ID}-${HDF5_PACKAGE_VERSION}${HDF5_MAVEN_VERSION_SUFFIX}-${HDF5_JAR_CLASSIFIER}.jar")
  set (_target_name "${_args_ARTIFACT_ID}_maven_jar")
  string (REPLACE "-" "_" _target_name "${_target_name}")

  # Implementation-Version records the HDF5 release the libraries were built with
  file (WRITE "${_manifest}"
"Manifest-Version: 1.0\nImplementation-Version: ${HDF5_PACKAGE_VERSION}${HDF5_MAVEN_VERSION_SUFFIX}\nHDF5-Native-Bundle: ${_args_ARTIFACT_ID}\nHDF5-Classifier: ${HDF5_JAR_CLASSIFIER}\nHDF5-Native-Loader-Platform: ${HDF5_NATIVE_LOADER_PLATFORM}\n"
  )

  set (_copy_commands "")
  foreach (_tgt IN LISTS _args_TARGETS)
    hdf5java_native_stage_name (_stage_name ${_tgt})
    list (APPEND _copy_commands
      COMMAND ${CMAKE_COMMAND} -E copy "$<TARGET_FILE:${_tgt}>" "${_natives}/${_stage_name}"
    )
    if (NOT WIN32)
      list (APPEND _copy_commands
        COMMAND ${CMAKE_COMMAND} "-DFILE=${_natives}/${_stage_name}" "-DPLATFORM=${HDF5_MAVEN_PLATFORM}"
                -P "${HDF5_SOURCE_DIR}/java/cmake/HDF5JavaStripRpath.cmake"
      )
    endif ()
  endforeach ()

  set (_license_depends "")
  if (_args_LICENSE_FILE)
    list (APPEND _copy_commands
      COMMAND ${CMAKE_COMMAND} -E copy "${_args_LICENSE_FILE}" "${_stage}/META-INF/LICENSE"
    )
    set (_license_depends "${_args_LICENSE_FILE}")
  else ()
    message (STATUS "No license file found for ${_args_ARTIFACT_ID}; it is not included in the JAR")
  endif ()

  add_custom_command (
    OUTPUT "${_jar}"
    COMMAND ${CMAKE_COMMAND} -E rm -rf "${_stage}"
    COMMAND ${CMAKE_COMMAND} -E make_directory "${_natives}"
    ${_copy_commands}
    COMMAND ${Java_JAR_EXECUTABLE} cfm "${_jar}" "${_manifest}" -C "${_stage}" .
    DEPENDS ${_args_TARGETS} "${_manifest}" "${HDF5_SOURCE_DIR}/java/cmake/HDF5JavaStripRpath.cmake" ${_license_depends}
    COMMENT "Creating Maven native bundle ${_args_ARTIFACT_ID} (${HDF5_JAR_CLASSIFIER}, ${HDF5_NATIVE_LOADER_PLATFORM})"
    VERBATIM
  )

  add_custom_target (${_target_name} ALL DEPENDS "${_jar}")
  set_target_properties (${_target_name} PROPERTIES FOLDER libraries/java)

  configure_file (
    ${HDF5_SOURCE_DIR}/java/cmake/${_args_POM_TEMPLATE}
    ${CMAKE_CURRENT_BINARY_DIR}/pom-${_args_ARTIFACT_ID}.xml
    @ONLY
  )
  install (
    FILES "${_jar}" ${CMAKE_CURRENT_BINARY_DIR}/pom-${_args_ARTIFACT_ID}.xml
    DESTINATION ${HDF5_INSTALL_JAR_DIR}
    COMPONENT maven
  )
endfunction ()

# Find the shared zlib and libaec (libaec + libsz) targets libhdf5 actually
# links. Static or non-target (plain path) dependencies are linked into
# libhdf5 or resolved by the OS, so they are not bundled.
set (_HDF5_ZLIB_NATIVE_TARGET "")
set (_HDF5_AEC_NATIVE_TARGET "")
set (_HDF5_SZ_NATIVE_TARGET "")
foreach (_lib IN LISTS LINK_COMP_LIBS)
  if (NOT TARGET ${_lib})
    continue ()
  endif ()
  set (_tgt ${_lib})
  get_target_property (_aliased ${_tgt} ALIASED_TARGET)
  if (_aliased)
    set (_tgt ${_aliased})
  endif ()
  get_target_property (_type ${_tgt} TYPE)
  if (NOT _type STREQUAL "SHARED_LIBRARY")
    continue ()
  endif ()
  # Classify by the name without its namespace: libaec::sz / libaec::aec from
  # find_package, <namespace>sz-shared / <namespace>aec-shared from FetchContent
  string (REGEX REPLACE "^.*::" "" _base "${_lib}")
  if (_base MATCHES "^sz(ip)?([-_]|$)")
    set (_HDF5_SZ_NATIVE_TARGET ${_tgt})
  elseif (_base MATCHES "^aec([-_]|$)")
    set (_HDF5_AEC_NATIVE_TARGET ${_tgt})
  elseif (_lib MATCHES "^(zlib|ZLIB::|zlib-ng)" OR _lib STREQUAL "${H5_ZLIB_LIBRARY}")
    set (_HDF5_ZLIB_NATIVE_TARGET ${_tgt})
  endif ()
endforeach ()

# Hdf5NativeLoader loads zlib as "z" (plus "zlib"/"zlib1" on Windows); zlib-ng
# without zlib compatibility is named z-ng / zlib-ng2 and would never be loaded
if (_HDF5_ZLIB_NATIVE_TARGET)
  get_target_property (_zlib_output_name ${_HDF5_ZLIB_NATIVE_TARGET} OUTPUT_NAME)
  if (NOT _zlib_output_name)
    get_target_property (_zlib_output_name ${_HDF5_ZLIB_NATIVE_TARGET} NAME)
  endif ()
  if (_zlib_output_name MATCHES "z(lib)?-ng")
    message (WARNING "zlib-ng without zlib compatibility (${_zlib_output_name}) cannot be loaded from hdf5-zlib-native; hdf5-zlib-native not created")
    set (_HDF5_ZLIB_NATIVE_TARGET "")
  endif ()
endif ()

# License file of a dependency built from source with FetchContent, or "" when it
# came from the system (find_package)
function (hdf5java_fetchcontent_license _out_var _content_name)
  include (FetchContent)
  FetchContent_GetProperties (${_content_name} SOURCE_DIR _src)
  set (_license "")
  if (_src)
    file (GLOB _candidates "${_src}/LICENSE*" "${_src}/COPYING*")
    if (_candidates)
      list (SORT _candidates)
      list (GET _candidates 0 _license)
    endif ()
  endif ()
  set (${_out_var} "${_license}" PARENT_SCOPE)
endfunction ()

# Profiles substituted into pom-native.xml.in: libhdf5 cannot load without the
# shared zlib and libaec it links, so hdf5-native requires their bundles
set (_native_dependency_profiles "")

# zlib: libhdf5's built-in deflate filter needs it when linked dynamically
if (_HDF5_ZLIB_NATIVE_TARGET)
  hdf5java_append_maven_native_profiles (_native_dependency_profiles hdf5-zlib-native
    "Bundled zlib that libhdf5 links for the deflate/GZIP filter" FALSE)
  hdf5java_fetchcontent_license (_zlib_license HDF5_ZLIB)
  hdf5java_add_native_maven_jar (
    ARTIFACT_ID hdf5-zlib-native
    POM_TEMPLATE pom-zlib-native.xml.in
    TARGETS ${_HDF5_ZLIB_NATIVE_TARGET}
    LICENSE_FILE "${_zlib_license}"
  )
endif ()

# libaec: libhdf5's built-in SZIP filter needs libsz, which depends on libaec
if (_HDF5_AEC_NATIVE_TARGET AND _HDF5_SZ_NATIVE_TARGET)
  hdf5java_append_maven_native_profiles (_native_dependency_profiles hdf5-szip-native
    "Bundled libaec/libsz that libhdf5 links for the SZIP filter" FALSE)
  hdf5java_fetchcontent_license (_aec_license SZIP)
  hdf5java_add_native_maven_jar (
    ARTIFACT_ID hdf5-szip-native
    POM_TEMPLATE pom-szip-native.xml.in
    TARGETS ${_HDF5_AEC_NATIVE_TARGET} ${_HDF5_SZ_NATIVE_TARGET}
    LICENSE_FILE "${_aec_license}"
  )
elseif (_HDF5_AEC_NATIVE_TARGET OR _HDF5_SZ_NATIVE_TARGET)
  message (WARNING "Only one of libaec/libsz is a shared library target; hdf5-szip-native not created")
endif ()

if (_native_dependency_profiles)
  set (HDF5_NATIVE_DEPENDENCY_PROFILES "\n    <profiles>${_native_dependency_profiles}\n    </profiles>\n")
else ()
  set (HDF5_NATIVE_DEPENDENCY_PROFILES "")
endif ()

hdf5java_add_native_maven_jar (
  ARTIFACT_ID hdf5-native
  POM_TEMPLATE pom-native.xml.in
  TARGETS ${HDF5_LIBSH_TARGET}
  LICENSE_FILE "${HDF5_SOURCE_DIR}/LICENSE"
)

set (HDF5_MAVEN_NATIVE_PROFILES "")

# libhdf5 on the binding POMs. The JNI bridge is built against this libhdf5, so
# hdf5-java-jni requires it; hdf5-java-ffm can also use a system HDF5 install.
if (HDF5JAVA_MAVEN_NATIVE_JNI)
  hdf5java_append_maven_native_profiles (HDF5_MAVEN_NATIVE_PROFILES hdf5-native
    "Bundled libhdf5 that the hdf5_java JNI bridge is built against (required)" FALSE)
else ()
  hdf5java_append_maven_native_profiles (HDF5_MAVEN_NATIVE_PROFILES hdf5-native
    "Bundled libhdf5 (optional: add explicitly, or use a system HDF5 install)" TRUE)
endif ()

# JNI bridge: required by hdf5-java-jni
if (HDF5JAVA_MAVEN_NATIVE_JNI)
  hdf5java_append_maven_native_profiles (HDF5_MAVEN_NATIVE_PROFILES hdf5-jni-native
    "Bundled hdf5_java JNI bridge (required)" FALSE)
  hdf5java_add_native_maven_jar (
    ARTIFACT_ID hdf5-jni-native
    POM_TEMPLATE pom-jni-native.xml.in
    TARGETS ${HDF5_JAVA_JNI_LIB_TARGET}
    LICENSE_FILE "${HDF5_SOURCE_DIR}/LICENSE"
  )
endif ()
