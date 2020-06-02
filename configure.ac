AC_INIT(mrc, 1.0, [maarten@hekkelman.com])

dnl Switch to a C++ compiler, and check if it works.
AC_LANG(C++)
AX_CXX_COMPILE_STDCXX_17([noext])

AX_BOOST_BASE([1.67])

AC_CHECK_HEADER(elf.h, [], [AC_MSG_ERROR([Could not find the header file elf.h])])
AC_CHECK_HEADER(filesystem, [], [AC_MSG_ERROR([Could not find the header file filesystem])])
AX_BOOST_PROGRAM_OPTIONS

AC_PROG_INSTALL

dnl Process Makefile.in to create Makefile
AC_OUTPUT(GNUmakefile)
