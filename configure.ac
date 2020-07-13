AC_INIT(mrc, 1.0, [maarten@hekkelman.com])

dnl Switch to a C++ compiler, and check if it works.
AC_LANG(C++)
AX_CXX_COMPILE_STDCXX_17([noext])

AX_BOOST_BASE([1.65.1], [], [AC_MSG_ERROR([Could not find a recent version of boost])])
AX_BOOST_PROGRAM_OPTIONS

AC_CHECK_HEADERS(elf.h filesystem)
AC_SEARCH_LIBS(stdc++fs boost_program_options)

AC_PROG_INSTALL

dnl Process Makefile.in to create Makefile
AC_OUTPUT(GNUmakefile)