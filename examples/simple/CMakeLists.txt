cmake_minimum_required(VERSION 3.16)

list(PREPEND CMAKE_PREFIX_PATH ../../cmake)

# set the project name
project(mrc-user VERSION 1.0.0 LANGUAGES CXX)

# Locate the mrc executable and load the mrc_* functions
# This of course only works when you have installed mrc already
find_package(mrc)

# resources depend on C++17 features, like std::filesystem
set(CXX_EXTENSIONS OFF)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Write out an mrsrc.hpp file and make sure the compiler finds it
mrc_write_header(${CMAKE_CURRENT_BINARY_DIR}/mrsrc.hpp)
include_directories(${CMAKE_CURRENT_BINARY_DIR})

# The executable to create
add_executable(mrc-user ${CMAKE_CURRENT_SOURCE_DIR}/mrc-user.cpp)

# # Add the hello.txt file as a resource, more files and/or directories
# # can be specified here
# mrc_target_resources(mrc-user CREATE_ELF_TEMPLATE VERBOSE
# 	RESOURCES ${CMAKE_CURRENT_SOURCE_DIR}/hello.txt)
#
# But you can also create a static library containing the resources
# and then link to that static library:

add_library(rsrc STATIC)
set_target_properties(rsrc PROPERTIES LINKER_LANGUAGE "CXX")
mrc_target_resources(rsrc CREATE_ELF_TEMPLATE VERBOSE
 	RESOURCES ${CMAKE_CURRENT_SOURCE_DIR}/hello.txt)
target_link_libraries(mrc-user PRIVATE rsrc)

