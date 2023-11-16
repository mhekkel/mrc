#include <catch2/catch_test_macros.hpp>

#include "mrsrc.h"

#include <iostream>
#include <set>

TEST_CASE("test_1", "test_1")
{
	mrsrc::rsrc r1("resource-1.txt");

	REQUIRE((bool)r1);

	REQUIRE(r1.data() != nullptr);
	REQUIRE(r1.size() == 50);

	int r = std::memcmp(r1.data(), R"(This is the first line
And this is the second line)", r1.size());

	REQUIRE(r == 0);
}

TEST_CASE("test_2", "test_2")
{
	mrsrc::rsrc r2("resource-2.txt");

	REQUIRE((bool)r2);

	REQUIRE(r2.data() != nullptr);
	REQUIRE(r2.size() == 102);
/*
		const char16_t* t = u"\xfeffThis is the first line\
And this is the second line";
		// t[0] = 0xfeff;

		int r = std::memcmp(r2.data(), (char*)t, r2.size());

		REQUIRE(r == 0);
*/
}

TEST_CASE("test_3", "test_3")
{
	mrsrc::streambuf buf("resource-1.txt");
	std::istream is(&buf);

	std::string line;
	REQUIRE((bool)std::getline(is, line));
	REQUIRE(line == "This is the first line");
	REQUIRE((bool)std::getline(is, line));
	REQUIRE(line == "And this is the second line");
	REQUIRE(not std::getline(is, line));
}

TEST_CASE("test_4", "test_4")
{
	mrsrc::istream is("resource-1.txt");

	std::string line;
	REQUIRE((bool)std::getline(is, line));
	REQUIRE(line == "This is the first line");
	REQUIRE((bool)std::getline(is, line));
	REQUIRE(line == "And this is the second line");
	REQUIRE(not std::getline(is, line));
}

TEST_CASE("test_10", "test_10")
{
	mrsrc::rsrc r0("");

	REQUIRE(std::distance(r0.begin(), r0.end()) == 3);

	std::set<std::string> found;
	for (auto& r1: r0)
		found.insert(r1.name());

	std::set<std::string> kTest{"resource-1.txt", "resource-2.txt", "subdir"};

	REQUIRE(found == kTest);

	if (found != kTest)
	{
		for (auto& f: found)
			std::cout << f << std::endl;
	}
}

TEST_CASE("test_11", "test_11")
{
	mrsrc::rsrc r0("subdir/resource-3.txt");

	REQUIRE((bool)r0);

	mrsrc::istream is(r0);
	std::string line;
	REQUIRE((bool)std::getline(is, line));
	REQUIRE(line == "Dit is resource 3");
}

TEST_CASE("test_12", "test_12")
{
	mrsrc::rsrc r0("subdir/subsubdir/resource-4.txt");

	REQUIRE((bool)r0);

	mrsrc::istream is(r0);
	std::string line;
	REQUIRE((bool)std::getline(is, line));
	REQUIRE(line == "Dit is resource 4");
}

TEST_CASE("test_13", "test_13")
{
	mrsrc::istream ri("subdir/resource-3.txt");

	REQUIRE((bool)ri);
	REQUIRE(ri.eof() == false);

	std::string line;
	REQUIRE((bool)std::getline(ri, line));
	REQUIRE(line == "Dit is resource 3");
	REQUIRE(ri.eof() == true);
}

TEST_CASE("test_14", "test_14")
{
	mrsrc::istream ri("resource-5.txt");

	REQUIRE((bool)ri == false);
	REQUIRE(ri.bad() == true);
}

