REM @echo off
:: remove existing Cmake cache and directories
rmdir /S /Q CMakeFiles
rmdir /S /Q src
rmdir /S /Q Testing
rmdir /S /Q tests
del /S /Q *.txt

:: set CMAKE-related and build-related variables
set CMAKEROOT=C:\Program Files (x86)\CMake
set COMPILER_DIR=C:\MinGW32
set COMPILER_VERSION=4.9.2
set COMPILER_TRIPLET=i686-w64-mingw32

set MAKE_EXECUTABLE_NAME=mingw32-make.exe
set Fortran_COMPILER_NAME=gfortran
set R_HOME=C:\Program Files\R\R-3.1.2\bin
set OMP_NUM_THREADS=8

:: define where 'make copy' will place executables
set INSTALL_PREFIX=d:/DOS
set TSPROC_EXECUTABLE=%INSTALL_PREFIX%/tsproc.exe

:: define other variables for use in the CMakeList.txt file
:: options are "Release", "Profile" or "Debug"
set BUILD_TYPE="Release"

:: options are "x86" (32-bit) or "x64" (64-bit)
set OS="win_x86"

:: Define build targets and compilation options
set MAKEFILE_VERBOSE=OFF
set TARGET__TSPROC_EXECUTABLE=ON
set TARGET__TSPROC_LIBRARY=OFF
set OPTION__UNROLL_CONTROL_FILE=OFF

:: define platform and compiler specific compilation flags
set CMAKE_Fortran_FLAGS_DEBUG="-O0 -g -ggdb -fcheck=all -fstack-usage -fexceptions -ffree-line-length-none -static -static-libgcc -static-libgfortran"
set CMAKE_Fortran_FLAGS_RELEASE="-O3 -mtune=core2 -flto -ffree-line-length-none -static -static-libgcc -static-libgfortran"
set CMAKE_Fortran_FLAGS_PROFILE="-O2 -pg -g -fno-omit-frame-pointer -DNDEBUG -fno-inline-functions -fno-inline-functions-called-once -fno-optimize-sibling-calls -ffree-line-length-none -static -static-libgcc -static-libgfortran -DCURL_STATICLIB"
::set CMAKE_Fortran_FLAGS_RELEASE="-O3 -mtune=native -fopenmp -flto -ffree-line-length-none -static-libgcc -static-libgfortran -DCURL_STATICLIB"

:: recreate clean Windows environment
set PATH=c:\windows;c:\windows\system32;c:\windows\system32\Wbem
set PATH=%PATH%;C:\Program Files (x86)\7-Zip
set PATH=%PATH%;%CMAKEROOT%\bin;%CMAKEROOT%\share

:: need to specify path to compiler or gcc will complain about missing dll files
set PATH=%PATH%;C:\MinGW32\bin
set PATH=%PATH%;C:\MinGW32\include;C:\MinGW32\lib

:: set a useful alias for make
echo %COMPILER_DIR%\bin\%MAKE_EXECUTABLE_NAME% %%1 > make.bat

:: not every installation will have these; I (SMW) find them useful
set PATH=%PATH%;D:\DOS\gnuwin32\bin

set CTEST_OUTPUT_ON_FAILURE=1

:: invoke CMake; add --trace to see copious details re: CMAKE
for %%f in ( "CodeBlocks - MinGW Makefiles" "MinGW Makefiles" ) do ^
cmake ..\..\.. -G %%f ^
-DCOMPILER_DIR=%COMPILER_DIR% ^
-DCOMPILER_TRIPLET=%COMPILER_TRIPLET% ^
-DCOMPILER_VERSION=%COMPILER_VERSION% ^
-DFortran_COMPILER_NAME=%Fortran_COMPILER_NAME% ^
-DOS=%OS% ^
-DCMAKE_BUILD_TYPE=%BUILD_TYPE% ^
-DCMAKE_INSTALL_PREFIX:PATH=%INSTALL_PREFIX% ^
-DCMAKE_MAKE_PROGRAM:FILEPATH=%COMPILER_DIR%\bin\%MAKE_EXECUTABLE_NAME% ^
-DCMAKE_C_COMPILER:FILEPATH=%COMPILER_DIR%\bin\%COMPILER_TRIPLET%-gcc.exe ^
-DCMAKE_Fortran_COMPILER:FILEPATH=%COMPILER_DIR%\bin\%COMPILER_TRIPLET%-gfortran.exe ^
-DMAKEFILE_VERBOSE=%MAKEFILE_VERBOSE% ^
-DTARGET__TSPROC_EXECUTABLE=%TARGET__TSPROC_EXECUTABLE% ^
-DTARGET__TSPROC_LIBRARY=%TARGET__TSPROC_LIBRARY% ^
-DOPTION__UNROLL_CONTROL_FILE=%OPTION__UNROLL_CONTROL_FILE% ^
-DCMAKE_Fortran_FLAGS_DEBUG=%CMAKE_Fortran_FLAGS_DEBUG% ^
-DCMAKE_Fortran_FLAGS_RELEASE=%CMAKE_Fortran_FLAGS_RELEASE% ^
-DCMAKE_Fortran_FLAGS_PROFILE=%CMAKE_Fortran_FLAGS_PROFILE%
