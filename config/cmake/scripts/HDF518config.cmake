#########################################################################
### For Windows ${CTEST_SCRIPT_ARG} is one of                         ###
###    [64-VS2013, 32-VS2013, 64-VS2012, 32-VS2012]                   ###
### ctest -S HDF518config.cmake,32-VS2012 -C Release -V -O hdf518.log ###
#########################################################################

cmake_minimum_required(VERSION 3.1.0 FATAL_ERROR)
set(CTEST_SOURCE_VERSION 1.8.15-pre4)
set(CTEST_SOURCE_NAME hdf5-${CTEST_SOURCE_VERSION})
set(CTEST_BINARY_NAME "build")
set(CTEST_DASHBOARD_ROOT "${CTEST_SCRIPT_DIRECTORY}")
if(WIN32)
  set(CTEST_SOURCE_DIRECTORY "${CTEST_DASHBOARD_ROOT}\\${CTEST_SOURCE_NAME}")
  set(CTEST_BINARY_DIRECTORY "${CTEST_DASHBOARD_ROOT}\\${CTEST_BINARY_NAME}")
else()
  set(CTEST_SOURCE_DIRECTORY "${CTEST_DASHBOARD_ROOT}/${CTEST_SOURCE_NAME}")
  set(CTEST_BINARY_DIRECTORY "${CTEST_DASHBOARD_ROOT}/${CTEST_BINARY_NAME}")
endif()

###################################################################
### Following Line is one of [Release, RelWithDebInfo, Debug] #####
set(CTEST_BUILD_CONFIGURATION "Release")
###################################################################

###################################################################
#########       Following describes compiler           ############
if(WIN32)
  set(SITE_OS_NAME "Windows")
  set(SITE_OS_VERSION "WIN7")
  if(${CTEST_SCRIPT_ARG} STREQUAL "64-VS2013")
    set(CTEST_CMAKE_GENERATOR "Visual Studio 12 2013 Win64")
    set(SITE_OS_BITS "64")
    set(SITE_COMPILER_NAME "vs2013")
    set(SITE_COMPILER_VERSION "12")
  elseif(${CTEST_SCRIPT_ARG} STREQUAL "32-VS2013")
    set(CTEST_CMAKE_GENERATOR "Visual Studio 12 2013")
    set(SITE_OS_BITS "32")
    set(SITE_COMPILER_NAME "vs2013")
    set(SITE_COMPILER_VERSION "12")
  elseif(${CTEST_SCRIPT_ARG} STREQUAL "64-VS2012")
    set(CTEST_CMAKE_GENERATOR "Visual Studio 11 2012 Win64")
    set(SITE_OS_BITS "64")
    set(SITE_COMPILER_NAME "vs2012")
    set(SITE_COMPILER_VERSION "11")
  elseif(${CTEST_SCRIPT_ARG} STREQUAL "32-VS2012")
    set(CTEST_CMAKE_GENERATOR "Visual Studio 11 2012")
    set(SITE_OS_BITS "32")
    set(SITE_COMPILER_NAME "vs2012")
    set(SITE_COMPILER_VERSION "11")
  endif()
##  Set the following to unique id your computer  ##
  set(CTEST_SITE "WIN7${CTEST_SCRIPT_ARG}.XXXX")
else()
  set(CTEST_CMAKE_GENERATOR "Unix Makefiles")
##  Set the following to unique id your computer  ##
  if(APPLE)
    set(CTEST_SITE "MAC.XXXX")
  else()
    set(CTEST_SITE "LINUX.XXXX")
  endif()
endif()
###################################################################

###################################################################
#########       Following is for submission to CDash   ############
###################################################################
set(MODEL "Experimental")
#########       Following describes computer           ############
## following is optional to describe build ##
set(SITE_BUILDNAME_SUFFIX "STATIC")
###################################################################

###################################################################
#####       Following controls CDash submission               #####
#set(LOCAL_SUBMIT "TRUE")
#####       Following controls test process                   #####
#set(LOCAL_SKIP_TEST "TRUE")
#set(LOCAL_MEMCHECK_TEST "TRUE")
#set(LOCAL_COVERAGE_TEST "TRUE")
#####       Following controls cpack command                  #####
#set(LOCAL_NO_PACKAGE "TRUE")
#####       Following controls source update                  #####
#set(LOCAL_UPDATE "TRUE")
set(REPOSITORY_URL "http://svn.hdfgroup.uiuc.edu/hdf5/branches/hdf5_1_8")
#uncomment to use a compressed source file: *.tar on linux or mac *.zip on windows
#set(CTEST_USE_TAR_SOURCE "${CTEST_SOURCE_VERSION}")
###################################################################

###################################################################
####  Change default configuration of options in config/cmake/cacheinit.cmake file ###
####  format: set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DXXX:YY=ZZZZ")

### uncomment/comment and change the following lines for configuration options

### comment the following line or change OFF to ON in order to build shared libraries
set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DBUILD_SHARED_LIBS:BOOL=OFF")

####      ext libraries       ####
### ext libs from tgz
set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_ALLOW_EXTERNAL_SUPPORT:STRING=TGZ -DTGZPATH:PATH=${CTEST_SCRIPT_DIRECTORY}")
### ext libs from svn
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_ALLOW_EXTERNAL_SUPPORT:STRING=SVN")
### ext libs on system
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DZLIB_LIBRARY:FILEPATH=some_location/lib/zlib.lib -DZLIB_INCLUDE_DIR:PATH=some_location/include")
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DSZIP_LIBRARY:FILEPATH=some_location/lib/szlib.lib -DSZIP_INCLUDE_DIR:PATH=some_location/include")
### disable ext libs building
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_ENABLE_Z_LIB_SUPPORT:BOOL=OFF")
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_ENABLE_SZIP_SUPPORT:BOOL=OFF")
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_ENABLE_SZIP_ENCODING:BOOL=OFF")
####      fortran       ####
### enable Fortran 2003 depends on HDF5_BUILD_FORTRAN
set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_ENABLE_F2003:BOOL=ON")
### disable Fortran; change OFF to ON in order to build FORTRAN libraries
set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_BUILD_FORTRAN:BOOL=OFF")

### disable test program builds
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DBUILD_TESTING:BOOL=OFF")

### disable packaging
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_NO_PACKAGES:BOOL=ON")
### Create install package with external libraries (szip, zlib, jpeg)
set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_PACKAGE_EXTLIBS:BOOL=ON")

### change install prefix
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DCMAKE_INSTALL_PREFIX:PATH=install")

###################################################################

if(WIN32)
  include(${CTEST_SCRIPT_DIRECTORY}\\CTestScript.cmake)
  if(EXISTS "${CTEST_BINARY_DIRECTORY}\\HDF5-${CTEST_SOURCE_VERSION}-win${SITE_OS_BITS}.exe")
    file(COPY "${CTEST_BINARY_DIRECTORY}\\HDF5-${CTEST_SOURCE_VERSION}-win${SITE_OS_BITS}.exe" DESTINATION ${CTEST_SCRIPT_DIRECTORY})
  endif()
  if(EXISTS "${CTEST_BINARY_DIRECTORY}\\HDF5-${CTEST_SOURCE_VERSION}-win${SITE_OS_BITS}.msi")
    file(COPY "${CTEST_BINARY_DIRECTORY}\\HDF5-${CTEST_SOURCE_VERSION}-win${SITE_OS_BITS}.msi" DESTINATION  ${CTEST_SCRIPT_DIRECTORY})
  endif()
  if(EXISTS "${CTEST_BINARY_DIRECTORY}\\HDF5-${CTEST_SOURCE_VERSION}-win${SITE_OS_BITS}.zip")
    file(COPY "${CTEST_BINARY_DIRECTORY}\\HDF5-${CTEST_SOURCE_VERSION}-win${SITE_OS_BITS}.zip" DESTINATION  ${CTEST_SCRIPT_DIRECTORY})
  endif()
else()
  include(${CTEST_SCRIPT_DIRECTORY}/CTestScript.cmake)
  if(APPLE)
    if(EXISTS "${CTEST_BINARY_DIRECTORY}\\HDF5-${CTEST_SOURCE_VERSION}-Darwin.dmg")
      file(COPY "${CTEST_BINARY_DIRECTORY}\\HDF5-${CTEST_SOURCE_VERSION}-Darwin.dmg" DESTINATION ${CTEST_SCRIPT_DIRECTORY})
    endif()
    if(EXISTS "${CTEST_BINARY_DIRECTORY}\\HDF5-${CTEST_SOURCE_VERSION}-Darwin.tar.gz")
      file(COPY "${CTEST_BINARY_DIRECTORY}\\HDF5-${CTEST_SOURCE_VERSION}-Darwin.tar.gz" DESTINATION  ${CTEST_SCRIPT_DIRECTORY})
    endif()
    if(EXISTS "${CTEST_BINARY_DIRECTORY}\\HDF5-${CTEST_SOURCE_VERSION}-Darwin.sh")
      file(COPY "${CTEST_BINARY_DIRECTORY}\\HDF5-${CTEST_SOURCE_VERSION}-Darwin.sh" DESTINATION ${CTEST_SCRIPT_DIRECTORY})
    endif()
  else()
    if(EXISTS "${CTEST_BINARY_DIRECTORY}\\HDF5-${CTEST_SOURCE_VERSION}-Linux.sh")
      file(COPY "${CTEST_BINARY_DIRECTORY}\\HDF5-${CTEST_SOURCE_VERSION}-Linux.sh" DESTINATION ${CTEST_SCRIPT_DIRECTORY})
    endif()
    if(EXISTS "${CTEST_BINARY_DIRECTORY}\\HDF5-${CTEST_SOURCE_VERSION}-Linux.tar.gz")
      file(COPY "${CTEST_BINARY_DIRECTORY}\\HDF5-${CTEST_SOURCE_VERSION}-Linux.tar.gz" DESTINATION  ${CTEST_SCRIPT_DIRECTORY})
    endif()
  endif()
endif()
