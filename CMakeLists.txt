# Copyright (c) 2021 Maarten L. Hekkelman

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:

# 1. Redistributions of source code must retain the above copyright notice, this
# list of conditions and the following disclaimer
# 2. Redistributions in binary form must reproduce the above copyright notice,
# this list of conditions and the following disclaimer in the documentation
# and/or other materials provided with the distribution.

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
project(mrc VERSION 1.3.12)

include(CTest)

set(CXX_EXTENSIONS OFF)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

if(MSVC)
	# make msvc standards compliant...
	add_compile_options(/permissive-)
endif()

if(WIN32)
	# Windows is always little endian, right?
	add_compile_definitions(LITTLE_ENDIAN)

	# And we certainly don't need those pesky min/max macro's
	add_compile_definitions(NOMINMAX)

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

	message(STATUS "Using COFF spec: ${COFF_SPEC}")
endif()

find_package(libmcfp 1.2.4 QUIET)

if(NOT libmcfp_FOUND)
	include(FetchContent)

	FetchContent_Declare(
		libmcfp
		GIT_REPOSITORY https://github.com/mhekkel/libmcfp
		GIT_TAG v1.2.4
	)

	FetchContent_MakeAvailable(libmcfp)
endif()

include(VersionString)
write_version_header(${PROJECT_SOURCE_DIR}/src/)

add_custom_command(OUTPUT ${CMAKE_BINARY_DIR}/mrc-rsrc.obj
	COMMAND $<TARGET_FILE:mrc-bootstrap> -o ${CMAKE_BINARY_DIR}/mrc-rsrc.obj ${PROJECT_SOURCE_DIR}/src/mrsrc.h ${COFF_SPEC}
	DEPENDS ${PROJECT_SOURCE_DIR}/src/mrsrc.h mrc-bootstrap
)

add_executable(mrc ${PROJECT_SOURCE_DIR}/src/mrc.cpp ${CMAKE_BINARY_DIR}/mrc-rsrc.obj)
add_executable(mrc-bootstrap ${PROJECT_SOURCE_DIR}/src/mrc.cpp ${PROJECT_SOURCE_DIR}/src/dummy.cpp)

target_link_libraries(mrc libmcfp::libmcfp)
target_link_libraries(mrc-bootstrap libmcfp::libmcfp)

if(BUILD_TESTING)
	find_package(Catch2 3 QUIET)

	if(NOT Catch2_FOUND)
		Include(FetchContent)

		FetchContent_Declare(
			Catch2
			GIT_REPOSITORY https://github.com/catchorg/Catch2.git
			GIT_TAG v3.5.2 # or a later release
		)

		FetchContent_MakeAvailable(Catch2)
	endif()

	file(GLOB UNIT_TEST_RSRC LIST_DIRECTORIES true ${PROJECT_SOURCE_DIR}/rsrc/*)

	add_custom_command(OUTPUT mrc-unit-test-rsrc.obj
		COMMAND $<TARGET_FILE:mrc-bootstrap> -o mrc-unit-test-rsrc.obj ${UNIT_TEST_RSRC} ${COFF_SPEC}
		DEPENDS ${UNIT_TEST_RSRC} mrc-bootstrap
	)

	add_executable(mrc-unit-test ${PROJECT_SOURCE_DIR}/src/mrc-unit-test.cpp ${CMAKE_BINARY_DIR}/mrc-unit-test-rsrc.obj)
	target_link_libraries(mrc-unit-test Catch2::Catch2WithMain)

	add_test(NAME unit-test COMMAND $<TARGET_FILE:mrc-unit-test> WORKING_DIRECTORY .)
endif()

install(TARGETS mrc)

if(WIN32)
	install(FILES cmake/mrc-config.cmake DESTINATION cmake)
	install(FILES doc/mrc-manual.pdf DESTINATION doc)
else()
	install(FILES cmake/mrc-config.cmake DESTINATION share/mrc/cmake)
	install(FILES doc/mrc.1 DESTINATION share/man/man1)
endif()

install(DIRECTORY examples DESTINATION share/doc/mrc/)

# add_subdirectory(examples/simple)
