This is the development source code tree for the Time-Series PROCessor
(TSPROC).

Inital TSPROC development was by John Doherty, Watermark Numerical
Computing.  It was part of the Surface Water Utilities package that was
created to support optimization of model parameters using the Parameter
ESTimation (PEST) suite or programs.  PEST was also developed by John Doherty.

## Requirements


* The build system is 'CMake' from http://cmake.org.

* A "C" compiler, the tested compilers are:

 + gcc on Linux
 + gcc from Mingw environment on Windows

* A Fortran 90 or better compiler. The tested Fortran compilers are:

 + ifort from Intel on Linux
 + pgfortran from Portland Group on Linux
 + gfortran >= 4.4 (to suport ISO_C_BINDING) from GNU on Linux

## Summary of Build Process

Create a build directory outside of the source directory structure and change
directory. If you have cloned this repository, then you should find several subdirectories within the 'build' subdirectory that contain scripts designed to set environment variables and compilation options before invoking CMake. CMake takes these environment variables, parses the source code, determines dependencies (i.e. which modules are to be built first), and creates a makefile. The makefile can then be invoked by the user to trigger the actual compilation and linking of the various modules.

### Building under Windows with MinGW64

In the `build/win_x64/gfortran` subdirectory is a simple batchfile that sets a number of environment variables before invoking CMake. You may need to alter some of the variable values to work on your system. For example, the batch file assumes that you have the MinGW64 compilers installed at 'c:\MinGW64'; this needs to be changed if your MinGW64 is located elsewhere. The version number environment variable must also match your installed version of gcc.

