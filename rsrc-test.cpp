#include <iostream>
#include <filesystem>

#include "mrsrc.h"

int main()
{
	mrsrc::rsrc eerste("eerste");

	if (eerste)
		std::cout << std::string(eerste.data(), eerste.size()) << std::endl;
	else
		std::cout << "not found" << std::endl;
	
	return 0;
}