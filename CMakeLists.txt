# Copyright (c) 2021 Maarten L. Hekkelman

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:

# 1. Redistributions of source code must retain the above copyright notice, this
#    list of conditions and the following disclaimer
# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
# ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

cmake_minimum_required(VERSION 3.10)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# set the project name
project(mrc VERSION 1.3.5)

include(Dart)
include(FindFilesystem)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(Filesystem REQUIRED)

if(MSVC)
    # make msvc standards compliant...
    add_compile_options(/permissive-)

	# Windows is always little endian, right?
	add_compile_definitions(LITTLE_ENDIAN)

	# Find out the processor type for the target
	if(${CMAKE_SYSTEM_PROCESSOR} STREQUAL "AMD64")
		set(COFF_TYPE "x64")
	elseif(${CMAKE_SYSTEM_PROCESSOR} STREQUAL "i386")
		set(COFF_TYPE "x86")
	elseif(${CMAKE_SYSTEM_PROCESSOR} STREQUAL "ARM64")
		set(COFF_TYPE "arm64")
	else()
		message(FATAL_ERROR "Unsupported or unknown processor type ${CMAKE_SYSTEM_PROCESSOR}")
	endif()	

	set(COFF_SPEC "--coff=${COFF_TYPE}")
endif()

find_package(Boost 1.70.0 REQUIRED COMPONENTS program_options)

include_directories(SYSTEM ${Boost_INCLUDE_DIRS})
include_directories(${CMAKE_CURRENT_BINARY_DIR})
link_libraries(Boost::program_options std::filesystem)

add_custom_command(OUTPUT mrc-rsrc.obj
	COMMAND $<TARGET_FILE:mrc-bootstrap> -o mrc-rsrc.obj ${PROJECT_SOURCE_DIR}/mrsrc.h ${COFF_SPEC}
	DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/mrsrc.h mrc-bootstrap
)

file(GLOB UNIT_TEST_RSRC LIST_DIRECTORIES true ${PROJECT_SOURCE_DIR}/rsrc/*)

add_custom_command(OUTPUT mrc-unit-test-rsrc.obj
	COMMAND $<TARGET_FILE:mrc-bootstrap> -o mrc-unit-test-rsrc.obj ${UNIT_TEST_RSRC} ${COFF_SPEC}
	DEPENDS ${UNIT_TEST_RSRC} mrc-bootstrap
)

add_executable(mrc ${PROJECT_SOURCE_DIR}/mrc.cpp mrc-rsrc.obj)
add_executable(mrc-bootstrap ${PROJECT_SOURCE_DIR}/mrc.cpp ${PROJECT_SOURCE_DIR}/dummy.cpp)
add_executable(mrc-unit-test ${PROJECT_SOURCE_DIR}/mrc-unit-test.cpp mrc-unit-test-rsrc.obj)

enable_testing()

add_test(NAME unit-test COMMAND $<TARGET_FILE:mrc-unit-test> WORKING_DIRECTORY .)

set(PACKAGE_NAME "${PROJECT_NAME}")
set(PACKAGE_VERSION "${PROJECT_VERSION}")
set(PACKAGE_STRING "${PROJECT_NAME} ${PROJECT_VERSION}")

configure_file(${PROJECT_SOURCE_DIR}/mrc.h.in mrc.h)

install(TARGETS mrc)

if(WIN32)
	install(FILES mrc-config.cmake DESTINATION cmake)
	install(FILES mrc-manual.pdf DESTINATION doc)
else()
	install(FILES mrc-config.cmake DESTINATION share/mrc/cmake)
	install(FILES mrc.1 DESTINATION share/man/man1)
endif()

install(DIRECTORY example DESTINATION share/doc/mrc/)