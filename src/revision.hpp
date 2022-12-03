// Generated revision file

#pragma once

#include <ostream>

const char kProjectName[] = "mrc";
const char kVersionNumber[] = "1.3.6";
const char kVersionGitTag[] = "9910f94";
const char kBuildInfo[] = "104*";
const char kBuildDate[] = "2022-12-03T10:11:40Z";

inline void write_version_string(std::ostream &os, bool verbose)
{
	os << kProjectName << " version " << kVersionNumber << std::endl;
	if (verbose)
	{
		os << "build: " << kBuildInfo << ' ' << kBuildDate << std::endl;
		if (kVersionGitTag[0] != 0)
			os << "git tag: " << kVersionGitTag << std::endl;
	}
}
