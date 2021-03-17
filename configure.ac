AC_PREREQ([2.69])

m4_define([mrc_version_major],[1])
m4_define([mrc_version_minor],[3])
m4_define([mrc_version_micro],[0])
m4_define([mrc_version_extra],[])
m4_define([mrc_version],[mrc_version_major().mrc_version_minor().mrc_version_micro()mrc_version_extra])

AC_INIT([mrc], [mrc_version], [maarten@hekkelman.com])

dnl Switch to a C++ compiler, and check if it works.
AC_LANG(C++)
AX_CXX_COMPILE_STDCXX_17([noext])

AC_CONFIG_SRCDIR([mrc.cpp])
AC_CONFIG_HEADERS([mrc.h])
AC_CONFIG_AUX_DIR(config)

AC_ARG_VAR([DEBUG], [Build a debug version of the application])

AX_BOOST_BASE([1.65.1], [], [AC_MSG_ERROR([Could not find a recent version of boost])])
dnl somehow this configure script did not find the correct library on an ARM system with buster
if test "x$ax_boost_user_program_options_lib" = "x"; then
	ax_boost_user_program_options_lib=boost_program_options
fi
AX_BOOST_PROGRAM_OPTIONS

AC_CHECK_HEADER(elf.h, [], [AC_MSG_ERROR([Could not find the header file elf.h])])

AC_CHECK_HEADER([filesystem], [], [AC_MSG_ERROR([The file <filesystem> is missing, perhaps you should install a more recent libstdc++ implementation.])])

dnl check if we need stdc++fs as library
AC_CACHE_CHECK([Whether linking to the c++ filesystem library requires -lstdc++fs], ax_cv_need_stdcppfs,
	[
		AC_TRY_LINK(
			[#include <filesystem>],
			[(void)std::filesystem::current_path();],
			[
				ax_cv_need_stdcppfs="no";
			],
			[
				SAVE_LIBS="$LIBS"
				LIBS="$LIBS -lstdc++fs"
				AC_TRY_LINK(
					[#include <filesystem>],
					[(void)std::filesystem::current_path();],
					[
						ax_cv_need_stdcppfs="yes";
					],
					[
						AC_MSG_ERROR([Could not link filesystem])
					]
				)
				LIBS=$SAVE_LIBS
			]
		)
	])
if test "x$ax_cv_need_stdcppfs" = "xyes"; then
	LIBS="$LIBS -lstdc++fs"
fi

AC_PROG_INSTALL

dnl Process Makefile.in to create Makefile
AC_OUTPUT(GNUmakefile)
