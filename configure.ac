AC_INIT(mrc, 1.2.1, [maarten@hekkelman.com])

dnl Switch to a C++ compiler, and check if it works.
AC_LANG(C++)
AX_CXX_COMPILE_STDCXX_17([noext])

AC_CONFIG_SRCDIR([mrc.cpp])
AC_CONFIG_HEADERS([mrc.h])

AC_ARG_VAR([DEBUG], [Build a debug version of the application])

AX_BOOST_BASE([1.65.1], [], [AC_MSG_ERROR([Could not find a recent version of boost])])
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
