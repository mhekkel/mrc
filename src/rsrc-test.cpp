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
	
	mrsrc::rsrc error_rsrc("invalid");
	assert(not error_rsrc);
	assert(error_rsrc.data() == nullptr);
	assert(error_rsrc.size() == 0);

	return 0;
}