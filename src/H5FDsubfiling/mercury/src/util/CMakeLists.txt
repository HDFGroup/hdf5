project(MERCURY_UTIL C)

#------------------------------------------------------------------------------
# Setup install directories and cmake module
#------------------------------------------------------------------------------
if(NOT MERCURY_UTIL_INSTALL_BIN_DIR)
  set(MERCURY_UTIL_INSTALL_BIN_DIR ${CMAKE_INSTALL_PREFIX}/bin)
endif()
if(NOT MERCURY_UTIL_INSTALL_LIB_DIR)
  set(MERCURY_UTIL_INSTALL_LIB_DIR ${CMAKE_INSTALL_PREFIX}/lib)
endif()
if(NOT MERCURY_UTIL_INSTALL_INCLUDE_DIR)
  # Interface include will default to prefix/include
  set(MERCURY_UTIL_INSTALL_INCLUDE_INTERFACE include)
  set(MERCURY_UTIL_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_PREFIX}/include)
else()
  set(MERCURY_UTIL_INSTALL_INCLUDE_INTERFACE ${MERCURY_UTIL_INSTALL_INCLUDE_DIR})
endif()
if(NOT MERCURY_UTIL_INSTALL_DATA_DIR)
  set(MERCURY_UTIL_INSTALL_DATA_DIR ${CMAKE_INSTALL_PREFIX}/share)
endif()

#------------------------------------------------------------------------------
# Setup cmake module
#------------------------------------------------------------------------------
#set(MERCURY_UTIL_CMAKE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/CMake")
#set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${MERCURY_UTIL_CMAKE_DIR})

#------------------------------------------------------------------------------
# Version information
#------------------------------------------------------------------------------
# Hard-coded version variables are read-in from a separate file. This makes it
# easier to have a script to update version numbers automatically.
file(STRINGS version.txt version_txt)
extract_version_components("${version_txt}" "${PROJECT_NAME}")
set(MERCURY_UTIL_PACKAGE "mercury-util")
message(STATUS "${MERCURY_UTIL_PACKAGE} v${MERCURY_UTIL_VERSION_FULL}")

#-----------------------------------------------------------------------------
# Targets built within this project are exported at Install time for use
# by other projects.
#-----------------------------------------------------------------------------
if(NOT MERCURY_UTIL_EXPORTED_TARGETS)
  set(MERCURY_UTIL_EXPORTED_TARGETS "${MERCURY_UTIL_PACKAGE}-targets")
endif()

#------------------------------------------------------------------------------
# Include source and build directories
#------------------------------------------------------------------------------
set(MERCURY_UTIL_BUILD_INCLUDE_DEPENDENCIES
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_BINARY_DIR}
)

#------------------------------------------------------------------------------
# External dependencies
#------------------------------------------------------------------------------
include(CheckCSourceCompiles)
include(CheckIncludeFiles)
include(CheckSymbolExists)
include(CheckTypeSize)

# Check for __attribute__((constructor(priority)))
check_c_source_compiles(
  "
  static void test_constructor(void) __attribute__((constructor(101)));
  int main(void) {return 0;}
  "
  HG_UTIL_HAS_ATTR_CONSTRUCTOR_PRIORITY
)

# Threads
set(CMAKE_THREAD_PREFER_PTHREAD TRUE)
set(THREADS_PREFER_PTHREAD_FLAG TRUE)
find_package(Threads REQUIRED)

set(MERCURY_UTIL_EXT_LIB_DEPENDENCIES
  ${MERCURY_UTIL_EXT_LIB_DEPENDENCIES}
  ${CMAKE_THREAD_LIBS_INIT}
)
if(CMAKE_USE_PTHREADS_INIT)
  set(CMAKE_EXTRA_INCLUDE_FILES pthread.h)
  set(CMAKE_REQUIRED_LIBRARIES ${CMAKE_THREAD_LIBS_INIT})

  # Detect pthread_spinlock_t
  check_type_size(pthread_spinlock_t HG_UTIL_HAS_PTHREAD_SPINLOCK_T)

  # Use type size to check enum value
  check_type_size(PTHREAD_MUTEX_ADAPTIVE_NP HG_UTIL_HAS_PTHREAD_MUTEX_ADAPTIVE_NP)

  # Detect pthread_condattr_setclock
  check_symbol_exists(pthread_condattr_setclock pthread.h
    HG_UTIL_HAS_PTHREAD_CONDATTR_SETCLOCK)

  unset(CMAKE_EXTRA_INCLUDE_FILES)
  unset(CMAKE_REQUIRED_LIBRARIES)
endif()

# Rt
if(NOT WIN32 AND NOT APPLE)
  set(MERCURY_UTIL_EXT_LIB_DEPENDENCIES
    ${MERCURY_UTIL_EXT_LIB_DEPENDENCIES}
    -lrt
  )
endif()

# Detect <time.h>
check_include_files("time.h" HG_UTIL_HAS_TIME_H)
if(HG_UTIL_HAS_TIME_H)
  set(CMAKE_EXTRA_INCLUDE_FILES time.h)

  # Detect clock_gettime
  check_symbol_exists(clock_gettime time.h HG_UTIL_HAS_CLOCK_GETTIME)

  # Use type size to check enum value
  check_type_size(CLOCK_MONOTONIC_COARSE HG_UTIL_HAS_CLOCK_MONOTONIC_COARSE)

  unset(CMAKE_EXTRA_INCLUDE_FILES)
endif()

# Debug
if(MERCURY_ENABLE_DEBUG)
  set(HG_UTIL_HAS_DEBUG 1)
else()
  set(HG_UTIL_HAS_DEBUG 0)
endif()

# Detect <sys/time.h>
check_include_files("sys/time.h" HG_UTIL_HAS_SYSTIME_H)

# Detect <sys/epoll.h>
check_include_files("sys/epoll.h" HG_UTIL_HAS_SYSEPOLL_H)

# Detect <sys/eventfd.h>
check_include_files("sys/eventfd.h" HG_UTIL_HAS_SYSEVENTFD_H)
if(HG_UTIL_HAS_SYSEVENTFD_H)
  set(CMAKE_EXTRA_INCLUDE_FILES "sys/eventfd.h")
  check_type_size(eventfd_t HG_UTIL_HAS_EVENTFD_T)
endif()

# Detect <sys/event.h>
check_include_files("sys/event.h" HG_UTIL_HAS_SYSEVENT_H)

# Detect <sys/param.h>
check_include_files("sys/param.h" HG_UTIL_HAS_SYSPARAM_H)

# Atomics
if(NOT WIN32)
  # Detect stdatomic
  check_include_files("stdatomic.h" HG_UTIL_HAS_STDATOMIC_H)
  # Detect size of atomic_long
  set(CMAKE_EXTRA_INCLUDE_FILES stdatomic.h)
  check_type_size(atomic_long HG_UTIL_ATOMIC_LONG_WIDTH)
  unset(CMAKE_EXTRA_INCLUDE_FILES)
endif()

# Colored output
option(MERCURY_ENABLE_LOG_COLOR "Use colored output for log." OFF)
if(MERCURY_ENABLE_LOG_COLOR)
  set(HG_UTIL_HAS_LOG_COLOR 1)
endif()
mark_as_advanced(MERCURY_ENABLE_LOG_COLOR)

#------------------------------------------------------------------------------
# Configure module header files
#------------------------------------------------------------------------------
# Set unique var used in the autogenerated config file (symbol import/export)
if(BUILD_SHARED_LIBS)
  set(HG_UTIL_BUILD_SHARED_LIBS 1)
  set(MERCURY_UTIL_LIBTYPE SHARED)
else()
  set(HG_UTIL_BUILD_SHARED_LIBS 0)
  set(MERCURY_UTIL_LIBTYPE STATIC)
endif()

configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_util_config.h.in
  ${CMAKE_CURRENT_BINARY_DIR}/mercury_util_config.h
)

#------------------------------------------------------------------------------
# Set sources
#------------------------------------------------------------------------------
set(MERCURY_UTIL_SRCS
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_atomic_queue.c
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_dlog.c
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_event.c
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_hash_table.c
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_log.c
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_mem.c
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_mem_pool.c
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_poll.c
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_request.c
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_thread.c
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_thread_condition.c
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_thread_mutex.c
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_thread_pool.c
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_thread_rwlock.c
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_thread_spin.c
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_util.c
)

#------------------------------------------------------------------------------
# Specify project public header files to be installed
#------------------------------------------------------------------------------
set(MERCURY_UTIL_PUBLIC_HEADERS
  ${CMAKE_CURRENT_BINARY_DIR}/mercury_util_config.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_atomic.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_atomic_queue.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_byteswap.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_compiler_attributes.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_dlog.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_event.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_hash_string.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_hash_table.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_inet.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_list.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_log.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_mem.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_mem_pool.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_param.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_poll.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_queue.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_request.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_thread.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_thread_annotation.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_thread_condition.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_thread_mutex.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_thread_pool.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_thread_rwlock.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_thread_spin.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_time.h
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_util.h
)

#------------------------------------------------------------------------------
# Specify project private header files
#------------------------------------------------------------------------------
set(MERCURY_UTIL_PRIVATE_HEADERS
  ${CMAKE_CURRENT_SOURCE_DIR}/mercury_util_error.h
)

#----------------------------------------------------------------------------
# Libraries
#----------------------------------------------------------------------------

# Clean up system include path first
foreach(item ${MERCURY_SYSTEM_INCLUDE_PATH})
  if(MERCURY_UTIL_EXT_INCLUDE_DEPENDENCIES)
    list(REMOVE_ITEM MERCURY_UTIL_EXT_INCLUDE_DEPENDENCIES ${item})
  endif()
endforeach()

# UTIL
add_library(mercury_util ${MERCURY_UTIL_SRCS}
  ${MERCURY_UTIL_PRIVATE_HEADERS} ${MERCURY_UTIL_PUBLIC_HEADERS}
)
if(THREADS_HAVE_PTHREAD_ARG)
  target_compile_options(mercury_util PUBLIC "${CMAKE_THREAD_LIBS_INIT}")
endif()
target_include_directories(mercury_util
  PUBLIC "$<BUILD_INTERFACE:${MERCURY_UTIL_BUILD_INCLUDE_DEPENDENCIES}>"
          $<INSTALL_INTERFACE:${MERCURY_INSTALL_INCLUDE_INTERFACE}>
)
target_include_directories(mercury_util
  SYSTEM PUBLIC ${MERCURY_UTIL_EXT_INCLUDE_DEPENDENCIES}
)
target_link_libraries(mercury_util ${MERCURY_UTIL_EXT_LIB_DEPENDENCIES})
mercury_set_lib_options(mercury_util "mercury_util" ${MERCURY_UTIL_LIBTYPE} ${PROJECT_NAME})
if(MERCURY_ENABLE_COVERAGE)
  set_coverage_flags(mercury_util)
endif()
set_target_properties(mercury_util PROPERTIES
  PUBLIC_HEADER "${MERCURY_UTIL_PUBLIC_HEADERS}"
)

#---------------------------------------------------------------------------
# Add Target(s) to CMake Install
#---------------------------------------------------------------------------
install(
  TARGETS
    mercury_util
  EXPORT
    ${MERCURY_UTIL_EXPORTED_TARGETS}
  LIBRARY DESTINATION ${MERCURY_UTIL_INSTALL_LIB_DIR}
  ARCHIVE DESTINATION ${MERCURY_UTIL_INSTALL_LIB_DIR}
  PUBLIC_HEADER DESTINATION ${MERCURY_UTIL_INSTALL_INCLUDE_DIR}
  RUNTIME DESTINATION ${MERCURY_UTIL_INSTALL_BIN_DIR}
)

#------------------------------------------------------------------------------
# Set variables for parent scope
#------------------------------------------------------------------------------
set(MERCURY_UTIL_EXT_INCLUDE_DEPENDENCIES ${MERCURY_UTIL_EXT_INCLUDE_DEPENDENCIES} PARENT_SCOPE)
set(MERCURY_UTIL_EXT_LIB_DEPENDENCIES ${MERCURY_UTIL_EXT_LIB_DEPENDENCIES} PARENT_SCOPE)
