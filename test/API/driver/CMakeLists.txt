cmake_minimum_required(VERSION 2.8.12.2 FATAL_ERROR)
project(H5VL_TEST_DRIVER CXX)

include(CheckAsan)
include(CheckUbsan)

set(CMAKE_CXX_STANDARD 11)

set(KWSYS_NAMESPACE h5vl_test_sys)
set(KWSYS_USE_SystemTools 1)
set(KWSYS_USE_Process 1)
set(KWSYS_USE_RegularExpression 1)
add_subdirectory(kwsys)
include_directories(${CMAKE_CURRENT_BINARY_DIR}/kwsys)

add_executable(h5vl_test_driver h5vl_test_driver.cxx)
target_link_libraries(h5vl_test_driver h5vl_test_sys)
