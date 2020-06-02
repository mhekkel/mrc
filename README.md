# mrc
Maartens Resource Compiler
==========================

## Abstract

A long, long time ago there existed an operating system that did things differently. One of the cool features of this OS was that applications were self contained. A single file was everything you needed to copy in order to install a new application instead of having to use installers or install scripts that put dozens of files at the most obscure locations on your disk.

One of the technical features of this OS to make this possible was what they called resources. In fact, resources were stored as some kind of database in one of the two forks of a file, the other fork being the data fork.

Resources were mildly popular, other OS'es copied the concept in one way or another. The company that invented them abandoned the whole concept though, they thought they had something better....

Anyway, I still like being able to provide a single executable to the users of my software. And given the usefulness of resources I decided to create a compiler for them that works with the ELF executable format. Since using resource forks is not an option I decided to store the data in the static data section of an executable. The data is then available through global variables.

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
