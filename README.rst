This is the development source code tree for the Time-Series PROCessor
(TSPROC).

Inital TSPROC development was by John Doherty, Watermark Numerical
Computing.  It was part of the Surface Water Utilities package that was
created to support optimization of model parameters using the Parameter
ESTimation (PEST) suite or programs.  PEST was also developed by John Doherty.

Requirements
============

* The build system is 'cmake' from http://cmake.org.

* A "C" compiler, the tested compilers are:

 + gcc on Linux
 + gcc from Mingw environment on Windows

* A Fortran 90 or better compiler. The tested Fortran compilers are:

 + ifort from Intel on Linux
 + pgfortran from Portland Group on Linux
 + gfortran >= 4.4 (to suport ISO_C_BINDING) from GNU on Linux

Build
=====
Create a build directory outside of the source directory structure and change
directory.

Examples are from Linux, adapt as necessary for Windows...

::

  mkdir /path/to/build/directory
  cd /path/to/build/directory


Then run 'cmake'.  The 'cmake' command draws a lot of the configuration from
the current environment.  For example, running on Linux it will create
'Makefile' files suitable for the Linux 'make' with the Fortran and C
compilers that are set in the respective environment variables (specifically
FC for Fortran and CC for the C Compiler)

::

  cmake -i /path/to/top/of/source/directory


Then answer the questions that 'cmake' asks.  When in doubt, accept the
default.

The run the command suitable for your build environment...

::

  make

There are a couple tests to make sure everything is alright...

::

  make test


Then install...

::

  make install


