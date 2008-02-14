project(HST)
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

include(AddFileDependencies)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Werror")

# Find required libraries.

find_package(Judy REQUIRED)
find_package(Bison REQUIRED)
include_directories(${JUDY_INCLUDE_DIR})

set(BISON_PREFIX_OUTPUTS TRUE)
set(BISON_GENERATE_DEFINES TRUE)

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
add_subdirectory(tests)
