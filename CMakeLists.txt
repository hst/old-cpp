cmake_minimum_required(VERSION 2.6)
project(HST)
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

include(AddFileDependencies)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Werror")

# Find required libraries and programs.

message(STATUS "Looking for pandoc")
find_program(PANDOC pandoc)
if(NOT PANDOC)
  message(FATAL_ERROR "Cannot find pandoc")
endif(NOT PANDOC)

message(STATUS "Looking for libjudy")
find_package(Judy REQUIRED)
include_directories(${JUDY_INCLUDE_DIR})

message(STATUS "Looking for bison")
find_package(Bison REQUIRED)
set(BISON_PREFIX_OUTPUTS TRUE)
set(BISON_GENERATE_DEFINES TRUE)

message(STATUS "Looking for Boost")
find_package(Boost)
include_directories(${Boost_INCLUDE_DIRS})

# The judyhash library is currently included directly in the source
# tree.  That means we're responsible for installing it — plus, we
# should also make sure that it's in the include path.

add_subdirectory(libs/judyhash/src_lib)
set(JUDYHASH_DIR ${HST_SOURCE_DIR}/libs/judyhash)
include_directories(${JUDYHASH_DIR}/src_lib)

# Include the rules for building and installing our code targets.

include_directories(${HST_SOURCE_DIR}/include)
link_directories(${HST_BINARY_DIR}/src)

enable_testing()
add_subdirectory(include/hst)
add_subdirectory(src)
add_subdirectory(doc)
add_subdirectory(tests)
