# SPDX-License-Identifier: BSD-2-Clause

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

#[=======================================================================[.rst:
FindMrc
-------

The module defines the following variables:

``MRC_EXECUTABLE``
  Path to mrc executable.
``mrc_FOUND``, ``MRC_FOUND``
  True if the mrc executable was found.
``MRC_VERSION_STRING``
  The version of mrc found.

Additionally, the following commands will be added:
  :command:`mrc_write_header`
  :command:`mrc_target_resources`

Example usage:

.. code-block:: cmake

   find_package(Mrc)
   if(mrc_FOUND)
     message("mrc found: ${MRC_EXECUTABLE}")

     mrc_write_header(${CMAKE_CURRENT_BINARY_DIR}/mrsrc.hpp)
     mrc_target_resources(my-app RESOURCES rsrc/hello-world.txt)
   endif()
#]=======================================================================]

find_program(MRC_EXECUTABLE
	NAMES mrc
	DOC "mrc executable"
)

if(CMAKE_HOST_WIN32)
	find_program(MRC_EXECUTABLE
		NAMES mrc
		PATHS $ENV{LOCALAPPDATA}
		PATH_SUFFIXES mrc/bin
		DOC "mrc executable"
	)
endif()

mark_as_advanced(MRC_EXECUTABLE)

# Retrieve the version number
execute_process(COMMAND ${MRC_EXECUTABLE} --version
	OUTPUT_VARIABLE mrc_version
	ERROR_QUIET
	OUTPUT_STRIP_TRAILING_WHITESPACE)

if(mrc_version MATCHES "^mrc version [0-9]")
	string(REPLACE "mrc version " "" MRC_VERSION_STRING "${mrc_version}")
endif()

unset(mrc_version)

find_package(PackageHandleStandardArgs REQUIRED)
find_package_handle_standard_args(Mrc
	REQUIRED_VARS MRC_EXECUTABLE
	VERSION_VAR MRC_VERSION_STRING)

# internal, create an ELF template file based on the current compiler flags and all
function(_mrc_create_elf_template _target)
	try_compile(BUILD_OK
		SOURCE_FROM_CONTENT my_code.cpp [[
extern "C" int my_global_int = 42;
int main() { return 0; }
]]
		NO_CACHE
		NO_LOG
		COPY_FILE "${_target}"
	)

	if(NOT BUILD_OK)
		message(FATAL_ERROR "Failed to create template executable")
	endif()
endfunction()

#[=======================================================================[.rst:
.. command:: mrc_target_resources

  Add resources to a target::

    mrc_target_resources(<target>
                         [RSRC_FILE <file>]
                         [DEPENDS_FILE <file>]
                         [VERBOSE]
                         [COFF_TYPE <AMD64|i386|ARM64>]
                         RESOURCES <file>...
                        )
  
  This command will specify that resources should be added to 
  the target ``<target>``.
  
  ``RSRC_FILE``
    Specify the resource file (object file) to create, default is
    based on ``<target>``.

  ``DEPENDS_FILE``
    Specify the depends file to create, default is based on ``<target>``.

  ``VERBOSE``
    Pass the --verbose to mrc so it will print out the list of files
    added as resource and the path to be used to access them inside the
    executable.

  ``CREATE_ELF_TEMPLATE``
    Create a small executable to be used as template for the ELF object
    file generation. This is executable is built using the same compiler
    settings as the final project and thus contains the correct ELF
    header from which mrc can copy the necessary information.

  ``RESOURCES``
    The files to pack into the resources of the executable. ``<file>``
    here can be a file or a directory. Files will be added to the root
    with their filename. Directories will be added recursively, if the
    directory name ends with a slash character, the directory part of
    the name will be stripped.
#]=======================================================================]
function(mrc_target_resources _target)
	set(flags VERBOSE CREATE_ELF_TEMPLATE)
	set(options COFF_TYPE RSRC_FILE DEPENDS_FILE)
	set(sources RESOURCES)
	cmake_parse_arguments(MRC_OPTION "${flags}" "${options}" "${sources}" ${ARGN})

	if(NOT _target)
		message(FATAL_ERROR "TARGET option is missing")
	endif()

	if("${MRC_OPTION_RESOURCES}" STREQUAL "")
		if("${ARGN}" STREQUAL "")
			message(FATAL_ERROR "no RESOURCES specified")
		else()
			message(DEBUG "no RESOURCES specified, falling back to use ARGN")
			set(MRC_OPTION_RESOURCES ${ARGN})
		endif()
	endif()

	set(_dir "${CMAKE_CURRENT_BINARY_DIR}/mrc-${_target}.dir")
	file(MAKE_DIRECTORY ${_dir})

	if(MRC_OPTION_RSRC_FILE)
		set(RSRC_FILE ${MRC_OPTION_RSRC_FILE})
	else()
		set(RSRC_FILE "${_dir}/rsrc.obj")
	endif()

	if(MRC_OPTION_RSRC_DEP_FILE)
		set(RSRC_DEP_FILE "${MRC_OPTION_DEPENDS_FILE}")
	else()
		set(RSRC_DEP_FILE "${_dir}/rsrc.d")
	endif()

	if(CMAKE_HOST_WIN32)
		if(MRC_OPTION_COFF_TYPE)
			list(APPEND OPTIONS "--coff=${MRC_OPTION_COFF_TYPE}")
		else()
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

			list(APPEND OPTIONS "--coff=${COFF_TYPE}")
		endif()
	endif()

	if(${MRC_OPTION_VERBOSE})
		list(APPEND OPTIONS "--verbose")
	endif()

	if(CMAKE_CROSSCOMPILING OR ${MRC_OPTION_CREATE_ELF_TEMPLATE})
		set(TEMPLATE "${_dir}/template.exe")
		_mrc_create_elf_template(${TEMPLATE})
		list(APPEND OPTIONS "--elf-template=${TEMPLATE}")
	endif()

	cmake_policy(SET CMP0116 NEW)

	# If we can use DEPFILE, use it.
	if(${CMAKE_VERSION} VERSION_GREATER_EQUAL "3.21")
		add_custom_target("mrc-depends-file_${_target}" ALL
			BYPRODUCTS ${RSRC_DEP_FILE}
			COMMAND ${MRC_EXECUTABLE} -o ${RSRC_FILE} -d ${RSRC_DEP_FILE}
			${MRC_OPTION_RESOURCES}
			VERBATIM)

		add_custom_command(OUTPUT ${RSRC_FILE}
			DEPFILE ${RSRC_DEP_FILE}
			COMMAND ${MRC_EXECUTABLE} -o ${RSRC_FILE} ${OPTIONS}
			${MRC_OPTION_RESOURCES}
			VERBATIM)
	else()
		message(STATUS "Not using dependency file since cmake version is too old")

		add_custom_command(OUTPUT ${RSRC_FILE}
			COMMAND ${MRC_EXECUTABLE} -o ${RSRC_FILE} ${OPTIONS}
			${MRC_OPTION_RESOURCES}
			VERBATIM)
	endif()

	target_sources(${_target} PRIVATE ${RSRC_FILE})
endfunction()

#[=======================================================================[.rst:
.. command:: mrc_write_header

  Write out the header file needed to use resources in a C++ application.
  The argument specifies the file to create.
#]=======================================================================]
function(mrc_write_header _header)
	execute_process(
		COMMAND ${MRC_EXECUTABLE} --header --output ${_header}
	)
endfunction()
