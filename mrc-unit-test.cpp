#define BOOST_TEST_MODULE MRC_Test
#include <boost/test/included/unit_test.hpp>

#include "mrsrc.h"

BOOST_AUTO_TEST_CASE(test_1)
{
	mrsrc::rsrc r1("resource-1.txt");

	BOOST_ASSERT((bool)r1);

	BOOST_TEST(r1.data() != nullptr);
	BOOST_TEST(r1.size() == 50);

	int r = std::memcmp(r1.data(), R"(This is the first line
And this is the second line)", r1.size());

	BOOST_TEST(r == 0);
}

BOOST_AUTO_TEST_CASE(test_2)
{
	mrsrc::rsrc r2("resource-2.txt");

	BOOST_ASSERT((bool)r2);

	BOOST_TEST(r2.data() != nullptr);
	BOOST_TEST(r2.size() == 102);

// 		const char16_t* t = u"\xfeffThis is the first line\
// And this is the second line";
// 		// t[0] = 0xfeff;

// 		int r = std::memcmp(r2.data(), (char*)t, r2.size());

// 		BOOST_TEST(r == 0);
}

BOOST_AUTO_TEST_CASE(test_3)
{
	mrsrc::streambuf buf("resource-1.txt");
	std::istream is(&buf);

	std::string line;
	BOOST_TEST((bool)std::getline(is, line));
	BOOST_TEST(line == "This is the first line");
	BOOST_TEST((bool)std::getline(is, line));
	BOOST_TEST(line == "And this is the second line");
	BOOST_TEST(not std::getline(is, line));
}

BOOST_AUTO_TEST_CASE(test_4)
{
	mrsrc::istream is("resource-1.txt");

	std::string line;
	BOOST_TEST((bool)std::getline(is, line));
	BOOST_TEST(line == "This is the first line");
	BOOST_TEST((bool)std::getline(is, line));
	BOOST_TEST(line == "And this is the second line");
	BOOST_TEST(not std::getline(is, line));
}

BOOST_AUTO_TEST_CASE(test_10)
{
	mrsrc::rsrc r0("");

	BOOST_TEST(std::distance(r0.begin(), r0.end()) == 2);

	auto i = r0.begin();
	BOOST_TEST(i->name() == "resource-1.txt");
	++i;
	BOOST_TEST(i->name() == "resource-2.txt");
}

