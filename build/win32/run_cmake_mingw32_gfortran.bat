:: nuke all existing environment variables
for /f "delims==" %%a in ('set') do set %%a=

del /F /Q CMakeCache.txt

set PATH=C:\MinGW32\bin
set PATH=%PATH%;C:\MinGW32\include;C:\MinGW32\lib

:: recreate virgin environment
set PATH=%PATH%;c:\windows;c:\windows\system32;c:\windows\system32\Wbem
set PATH=%PATH%;C:\Program Files (x86)\7-Zip
set PATH=%PATH%;C:\Program Files (x86)\CMake 2.8\bin
set PATH=%PATH%;C:\Program Files (x86)\CMake 2.8\bin
set PATH=%PATH%;C:\Program Files (x86)\CMake 2.8\share
set PATH=%PATH%;c:\Program Files (x86)\Zeus
set PATH=%PATH%;D:\DOS\gnuwin32\bin
set R_HOME=C:\Program Files\R\R-2.15.2\bin
set PATH=%PATH%;d:\saga_2.0.8;d:\saga_2.0.8\modules;d:\saga_2.0.8\dll;.

set FC=c:\MinGW32\bin\gfortran
set CC=c:\MinGW32\bin\gcc
set LIB=c:\MinGW32\lib
set INCLUDE=c:\MinGW32\include
set TEMP=d:\TEMP

:: now set CMAKE-related and build-related variables
set CMAKEROOT=C:/Program Files (x86)/CMake 2.8
set MinGWbase=c:/MinGW32

set INCLUDE=%MinGWbase%\include
set LIB=%MinGWbase%\lib

set TEMP=d:/TEMP
set FC=%MinGWbase%/bin/gfortran.exe
set F90=%MinGWbase%/bin/gfortran.exe
set CC=%MinGWbase%/bin/gcc.exe
set CXX=%MinGWbase%/bin/g++.exe
set AR=%MinGWbase%/bin/ar.exe
set NM=%MinGWbase%/bin/nm.exe
set LD=%MinGWbase%/bin/ld.exe
set STRIP=%MinGWbase%/bin/strip.exe
set CMAKE_RANLIB=%MinGWbase%/bin/ranlib.exe
set LIBRARY_PATH=%MinGWbase%/lib
set LDFLAGS="-flto"
set CPPFLAGS="-DgFortran"

set CMAKE_INCLUDE_PATH=%INCLUDE%
set CMAKE_LIBRARY_PATH=%LIB%
set CTEST_OUTPUT_ON_FAILURE=1

:: add --trace to see copious details re: CMAKE

cmake ..\.. -G "MinGW Makefiles" -DPLATFORM_TYPE="x86" -DCMAKE_BUILD_TYPE="Release" -DCMAKE_INSTALL_PREFIX:PATH="D:\DOS" -DCMAKE_MAKE_PROGRAM="c:\MinGW32\bin\make.exe" -DCMAKE_RANLIB:FILEPATH="c:\MinGW32\bin\ranlib.exe" -DCMAKE_C_COMPILER:FILEPATH="c:\MinGW32\bin\gcc.exe" -DCMAKE_Fortran_COMPILER:FILEPATH="c:\MinGW32\bin\gfortran.exe"
