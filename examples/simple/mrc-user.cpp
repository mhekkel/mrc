/*
	Example of the use of resources created with mrc in a C++ application
*/

#include <iostream>

// Include the generated header file
#include "mrsrc.hpp"

int main()
{
	try
	{
		mrsrc::rsrc res("hello.txt");
		if (not res)
			throw std::runtime_error("Resource not found");
		
		std::cout.write(res.data(), res.size());
		std::cout << std::endl;
	}
	catch(const std::exception& e)
	{
		std::cerr << e.what() << '\n';
		exit(1);
	}
	
	return 0;
}

