cmake_minimum_required(VERSION 3.10)

# set the project name
project(mrc VERSION 1.3.0)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

if(MSVC)
    # make msvc standards compliant...
    add_compile_options(/permissive-)

	# Windows is always little endian, right?
	add_compile_definitions(LITTLE_ENDIAN)
endif()

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  if (MSVC)
    set(CMAKE_INSTALL_PREFIX "$ENV{LOCALAPPDATA}/${PROJECT_NAME}" CACHE PATH "..." FORCE)
  endif()
endif()

set(BINARY_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}")

if(MSVC)
	if(${CMAKE_SYSTEM_PROCESSOR} STREQUAL "AMD64")
		set(COFF_TYPE "x64")
	elseif(${CMAKE_SYSTEM_PROCESSOR} STREQUAL "i386")
		set(COFF_TYPE "x86")
	elseif(${CMAKE_SYSTEM_PROCESSOR} STREQUAL "ARM64")
		set(COFF_TYPE "arm64")
	else()
		message(FATAL_ERROR "Unsupported or unknown processor type ${CMAKE_SYSTEM_PROCESSOR}")
	endif()	
endif()

find_package(Boost 1.73.0 REQUIRED COMPONENTS program_options)

include_directories(BEFORE SYSTEM ${Boost_INCLUDE_DIRS})
include_directories(${CMAKE_CURRENT_BINARY_DIR})
link_libraries(${Boost_LIBRARIES})

add_custom_command(OUTPUT mrc-rsrc.obj
	COMMAND $<TARGET_FILE:mrc-bootstrap> -o mrc-rsrc.obj ${PROJECT_SOURCE_DIR}/mrsrc.h --coff=${COFF_TYPE}
	DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/mrsrc.h mrc-bootstrap
)

add_executable(mrc ${PROJECT_SOURCE_DIR}/mrc.cpp mrc-rsrc.obj)
add_executable(mrc-bootstrap ${PROJECT_SOURCE_DIR}/mrc.cpp ${PROJECT_SOURCE_DIR}/dummy.cpp)
add_executable(mrc-unit-test ${PROJECT_SOURCE_DIR}/mrc-unit-test.cpp)

set(PACKAGE_NAME "${PROJECT_NAME}")
set(PACKAGE_VERSION "${PROJECT_VERSION}")
set(PACKAGE_STRING "${PROJECT_NAME} ${PROJECT_VERSION}")
message("The package string is ${PACKAGE_STRING}")
configure_file(${PROJECT_SOURCE_DIR}/mrc.h-cmake.in mrc.h)

install(TARGETS mrc CONFIGURATIONS Release RUNTIME DESTINATION ${BINARY_INSTALL_DIR})