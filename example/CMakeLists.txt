cmake_minimum_required(VERSION 3.16)

# set the project name
project(mrc-user VERSION 1.0.0 LANGUAGES CXX)

# Locate the mrc executable and load the mrc_* functions
find_package(Mrc)

# resources depend on C++17 features, like std::filesystem
set(CXX_EXTENSIONS OFF)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Write out an mrsrc.hpp file and make sure the compiler finds it
mrc_write_header(${CMAKE_CURRENT_BINARY_DIR}/mrsrc.hpp)
include_directories(${CMAKE_CURRENT_BINARY_DIR})

# The executable to create
add_executable(mrc-user ${CMAKE_CURRENT_SOURCE_DIR}/mrc-user.cpp)

# Add the hello.txt file as a resource, more files and/or directories
# can be specified here
mrc_target_resources(mrc-user ${CMAKE_CURRENT_SOURCE_DIR}/hello.txt)
