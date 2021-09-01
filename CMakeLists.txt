cmake_minimum_required(VERSION 3.10)

# set the project name
project(mrc VERSION 1.3.1)

enable_testing()

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

if(MSVC)
    # make msvc standards compliant...
    add_compile_options(/permissive-)

	# Windows is always little endian, right?
	add_compile_definitions(LITTLE_ENDIAN)

	# On Windows, do not install in the system location
	if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
		message(WARNING "The executable will be installed in $ENV{LOCALAPPDATA}/${PROJECT_NAME}")
		set(CMAKE_INSTALL_PREFIX "$ENV{LOCALAPPDATA}/${PROJECT_NAME}" CACHE PATH "..." FORCE)
	endif()

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

if(UNIX AND NOT APPLE)
	# On Linux, install in the $HOME/.local folder by default
	if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
		message(WARNING "The library and auxiliary files will be installed in $ENV{HOME}/.local")
		set(CMAKE_INSTALL_PREFIX "$ENV{HOME}/.local" CACHE PATH "..." FORCE)
	endif()
endif()

find_package(Boost 1.70.0 REQUIRED COMPONENTS program_options)

include_directories(SYSTEM ${Boost_INCLUDE_DIRS})
include_directories(${CMAKE_CURRENT_BINARY_DIR})
link_libraries(${Boost_LIBRARIES})

add_custom_command(OUTPUT mrc-rsrc.obj
	COMMAND $<TARGET_FILE:mrc-bootstrap> -o mrc-rsrc.obj ${PROJECT_SOURCE_DIR}/mrsrc.h ${COFF_SPEC}
	DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/mrsrc.h mrc-bootstrap
)

file(GLOB UNIT_TEST_RSRC LIST_DIRECTORIES true ${PROJECT_SOURCE_DIR}/rsrc/*)
message("Resources: ${UNIT_TEST_RSRC}")

add_custom_command(OUTPUT mrc-unit-test-rsrc.obj
	COMMAND $<TARGET_FILE:mrc-bootstrap> -o mrc-unit-test-rsrc.obj ${UNIT_TEST_RSRC} ${COFF_SPEC}
	DEPENDS ${UNIT_TEST_RSRC} mrc-bootstrap
)

add_executable(mrc ${PROJECT_SOURCE_DIR}/mrc.cpp mrc-rsrc.obj)
add_executable(mrc-bootstrap ${PROJECT_SOURCE_DIR}/mrc.cpp ${PROJECT_SOURCE_DIR}/dummy.cpp)
add_executable(mrc-unit-test ${PROJECT_SOURCE_DIR}/mrc-unit-test.cpp mrc-unit-test-rsrc.obj)

add_test(NAME unit-test COMMAND $<TARGET_FILE:mrc-unit-test> WORKING_DIRECTORY .)

set(PACKAGE_NAME "${PROJECT_NAME}")
set(PACKAGE_VERSION "${PROJECT_VERSION}")
set(PACKAGE_STRING "${PROJECT_NAME} ${PROJECT_VERSION}")

configure_file(${PROJECT_SOURCE_DIR}/mrc.h.in mrc.h)

install(TARGETS mrc CONFIGURATIONS Release RUNTIME DESTINATION ${BINARY_INSTALL_DIR})