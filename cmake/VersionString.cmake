# SPDX-License-Identifier: BSD-2-Clause

# Copyright (c) 2021 NKI/AVL, Netherlands Cancer Institute

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

# This cmake extension writes out a revision.hpp file in a specified directory.
# The file will contain a C++ inline function that can be used to write out
# version information.

cmake_minimum_required(VERSION 3.15)

# Create a revision file, containing the current git version info, if any
function(write_version_header dir)
	# parameter check
	if(NOT IS_DIRECTORY ${dir})
		message(FATAL_ERROR "First parameter to write_version_header should be a directory where the final revision.hpp file will be placed")
	endif()

	# Load git package if needed
	if(NOT GIT_FOUND)
		find_package(Git QUIET)
	endif()

	# If git was found, fetch the git description string
	if(GIT_FOUND)
		execute_process(
			COMMAND "${GIT_EXECUTABLE}" describe --dirty --match=build
			WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
			RESULT_VARIABLE res
			OUTPUT_VARIABLE out
			ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)

		if(res EQUAL 0)
			set(REVISION_STRING "${out}")
		endif()
	endif()

	# Check the revision string, if it matches we fill in the required info
	if(REVISION_STRING MATCHES "build-([0-9]+)-g([0-9a-f]+)(-dirty)?")
		set(REVISION_GIT_TAGREF "${CMAKE_MATCH_2}")
		if(CMAKE_MATCH_3)
			set(REVISION_STRING "${CMAKE_MATCH_1}*")
		else()
			set(REVISION_STRING "${CMAKE_MATCH_1}")
		endif()

		string(TIMESTAMP REVISION_DATE_TIME "%Y-%m-%dT%H:%M:%SZ" UTC)
	else()
		message(STATUS "no git info available, cannot update version string")

		set(REVISION_GIT_TAGREF "")
		set(REVISION_STRING "")
		set(REVISION_DATE_TIME "")
	endif()

	if(ARGC GREATER 1)
		set(VAR_PREFIX "${ARGV1}")
	endif()

	file(WRITE "${PROJECT_BINARY_DIR}/revision.hpp.in" [[// Generated revision file

#pragma once

#include <ostream>

const char k@VAR_PREFIX@ProjectName[] = "@PROJECT_NAME@";
const char k@VAR_PREFIX@VersionNumber[] = "@PROJECT_VERSION@";
const char k@VAR_PREFIX@RevisionGitTag[] = "@REVISION_GIT_TAGREF@";
const char k@VAR_PREFIX@RevisionInfo[] = "@REVISION_STRING@";
const char k@VAR_PREFIX@RevisionDate[] = "@REVISION_DATE_TIME@";

inline void write_version_string(std::ostream &os, bool verbose)
{
	os << k@VAR_PREFIX@ProjectName << " version " << k@VAR_PREFIX@VersionNumber << std::endl;
	if (verbose)
	{
		if (k@VAR_PREFIX@RevisionInfo[0] != 0)
		{
			os << "build: " << k@VAR_PREFIX@RevisionInfo << ' ' << k@VAR_PREFIX@RevisionDate << std::endl;
			if (k@VAR_PREFIX@RevisionGitTag[0] != 0)
				os << "git tag: " << k@VAR_PREFIX@RevisionGitTag << std::endl;
		}
		else
			os << "No revision information available" << std::endl;
	}
}
]])
	configure_file("${PROJECT_BINARY_DIR}/revision.hpp.in" "${dir}/revision.hpp" @ONLY)
endfunction()

