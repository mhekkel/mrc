[![Build Status](https://travis-ci.org/mhekkel/mrc.svg?branch=master)](https://travis-ci.org/mhekkel/mrc)

# mrc
Maartens Resource Compiler
==========================

## Abstract

A long, long time ago there existed an operating system that did things differently. One of the cool features of this OS was that applications were self contained. A single file was everything you needed to copy in order to install a new application instead of having to use installers or install scripts that put dozens of files at the most obscure locations on your disk.

One of the technical features of this OS to make this possible was what they called resources. In fact, resources were stored as some kind of database in one of the two forks of a file, the other fork being the data fork.

Resources were mildly popular, other OS'es copied the concept in one way or another. The company that invented them abandoned the whole concept though, they thought they had something better....

Anyway, I still like being able to provide a single executable to the users of my software. And given the usefulness of resources I decided to create a compiler for them that works with the ELF executable format. Since using resource forks is not an option I decided to store the data in the static data section of an executable. The data is then available through global variables.

New in version 1.4 is support for writing COFF files, so you can now use this type of resources in Windows as well.

## Synopsis

First, create the mrsrc.h file that contains C++ classes to access the resources. This file can be generated by executing:

	mrc --header > mrsrc.h

Then include this file and use it:

	#include "mrsrc.h"
	
	int main()
	{
		mrsrc::rsrc hello("texts/greeting");
		if (hello)
		{
			string s(hello.data(), hello.size());
			cout << s << endl;
		}
		
		return 0;
	}
  
To create a resource file:

	echo "Hello, world!" > greeting
	mrc -o my-rsrc.o --root texts greeting
	c++ -o my-app foo.cpp my-rsrc.o

## CMake integration

mrc comes with a mrc-config.cmake file installed at a suitable location. This means you can include mrc using a `find_package` call. The previous example might have a _CMakeLists.txt_ file containing the following:

```
project(hello VERSION 1.0.0 LANGUAGES CXX)

# Include the mrc package file
find_package(Mrc)

# The MRC_FOUND variable is set if MRC was found
if(NOT MRC_FOUND)
	message(FATAL_ERROR "mrc not found)
endif()

# Write out the mrc header file and add the directory to the include paths
mrc_write_header(${CMAKE_CURRENT_BINARY_DIR}/mrsrc.hpp)
include_directories(${CMAKE_CURRENT_BINARY_DIR})

# The executable to create
add_executable(mrc-user ${CMAKE_CURRENT_SOURCE_DIR}/src/mrc-user.cpp)

# Add the file hello.txt in the directory rsrc as resource 'hello.txt'
mrc_target_resources(mrc-user ${CMAKE_CURRENT_SOURCE_DIR}/rsrc/hello.txt)
```

## Building mrc

To build mrc, you should use [cmake](https://cmake.org), e.g.:
```
git clone https://github.com/mhekkel/mrc.git
cd mrc
mkdir build
cd build
cmake ..
cmake --install
```