AC_INIT(mrc, 1.2, [maarten@hekkelman.com])

dnl Switch to a C++ compiler, and check if it works.
AC_LANG(C++)
AX_CXX_COMPILE_STDCXX_17([noext])

AC_CONFIG_SRCDIR([mrc.cpp])
AC_CONFIG_HEADERS([mrc.h])

AX_BOOST_BASE([1.65.1], [], [AC_MSG_ERROR([Could not find a recent version of boost])])
AX_BOOST_PROGRAM_OPTIONS

AC_CHECK_HEADER(elf.h, [], [AC_MSG_ERROR([Could not find the header file elf.h])])
AC_CHECK_HEADER(filesystem, [], [AC_MSG_ERROR([Could not find the header file filesystem])])
AX_BOOST_PROGRAM_OPTIONS

AC_PROG_INSTALL

dnl Process Makefile.in to create Makefile
AC_OUTPUT(GNUmakefile)
