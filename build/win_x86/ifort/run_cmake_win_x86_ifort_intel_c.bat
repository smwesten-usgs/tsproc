@echo off
::
:: INSTRUCTIONS ON THE CUSTOMIZATION OF THIS BATCHFILE
::
::  This batchfile is designed to set all important environment variables
::  prior to making the call to CMake. CMake is generally pretty clever
::  about finding libraries and executables if it is given the proper hints.
::
::  THIS SCRIPT ASSUMES THAT YOU ARE RUNNING IT FROM THE build/win_x64/ifort
::  SUBDIRECTORY *AND* THAT YOU HAVE RUN THE INTEL-PROVIDED SCRIPTS NEEDED
::  TO SET CERTAIN ENVIRONMENT VARIABLES.
::
::  On my machine, this script is found here:
::
:: C:\Program Files (x86)\Intel\Composer XE 2011 SP1\bin\ipsxe-comp-vars.bat" intel32 vs2010"
::
:: set CMAKE-related and build-related variables
set CMAKEROOT=C:\Program Files (x86)\CMake 2.8
set R_HOME=C:\Program Files\R\R-2.15.2\bin

:: define where 'make copy' will place executables
set INSTALL_PREFIX=d:\DOS

:: options are "Release" or "Debug"
set BUILD_TYPE="Release"
::
:: Set locations to the Intel Fortran compiler and the Visual
:: Studio C/C++ compiler. Not sure how portable this will be.
set FC=ifort.exe
set CC=icl.exe
set CXX=icl.exe
::
:: define platform and compiler specific compilation flags
set CMAKE_Fortran_FLAGS_DEBUG="/nologo /O /GS /libs:static"
set CMAKE_Fortran_FLAGS_RELEASE="/nologo /O3 /QxHost /libs:static"
::
:: delete existing CMake cache files, previous build and test files
del /F /Q *.cmake
del /F /Q CMakeCache.txt
rmdir /S /Q Testing
rmdir /S /Q tests
rmdir /S /Q src
rmdir /S /Q CMakeFiles
::
::
if "%CMAKE_ADDED_TO_PATH%"=="" set PATH=%PATH%;%CMAKEROOT%;%R_HOME%;%INSTALL_PREFIX%
set CMAKE_ADDED_TO_PATH="TRUE"
::
::
::
set CMAKE_CXX_FLAGS_DEBUG="/nologo /O"
set CMAKE_CXX_FLAGS_RELEASE="/nologo /O3 /QxHost" 
set CMAKE_EXE_LINKER_FLAGS="/SUBSYSTEM:console"

set CMAKE_GENERATOR="NMake Makefiles"

set CTEST_OUTPUT_ON_FAILURE=1

:: add --trace to see copious details re: CMAKE

cmake ..\..\.. -G %CMAKE_GENERATOR% ^
-DCMAKE_BUILD_TYPE=%BUILD_TYPE% ^
-DCMAKE_INSTALL_PREFIX:PATH=%INSTALL_PREFIX% ^
-DCMAKE_CXX_COMPILER:FILEPATH=%CC% ^
-DCMAKE_Fortran_COMPILER:FILEPATH=%FC% ^
-DCMAKE_Fortran_FLAGS_DEBUG=%CMAKE_Fortran_FLAGS_DEBUG% ^
-DCMAKE_Fortran_FLAGS_RELEASE=%CMAKE_Fortran_FLAGS_RELEASE% ^
-DCMAKE_CXX_FLAGS_DEBUG=%CMAKE_CXX_FLAGS_DEBUG% ^
-DCMAKE_CXX_FLAGS_RELEASE=%CMAKE_CXX_FLAGS_RELEASE% ^
-DCMAKE_EXE_LINKER_FLAGS=%CMAKE_EXE_LINKER_FLAGS%

