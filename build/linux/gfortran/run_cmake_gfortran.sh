#!/bin/bash
#remove existing Cmake cache and directories
# remove existing Cmake cache and directories
rm -fr CMake*
rm -rf Testing
rm -rf src
rm -rf tests
rm -f CPack*
rm -f *.txt

# set CMAKE-related and build-related variables
export GCC=$(which gcc)
export GPP=$(which g++)
export GFORTRAN=$(which gfortran)

export CMAKEROOT=$(which cmake)
export R_SCRIPT=$(which Rscript)
export LIB_GCC=$(gcc -print-file-name=libgcc.a)
export LIB_GFORTRAN=$(gfortran -print-file-name=libgfortran.a)

export PATH=/usr/local:/usr/local/bin:/usr/local/lib:/usr/bin/cmake:$PATH

# define where 'make copy' will place executables
export INSTALL_PREFIX=/usr/local/bin

# define other variables for use in the CMakeList.txt file
# options are "Release" or "Debug"
export BUILD_TYPE="RELEASE"

# define platform and compiler specific compilation flags
export CMAKE_Fortran_FLAGS_DEBUG="-O0 -g -ggdb -Wall -Wuninitialized -fbacktrace -fcheck=all -fexceptions -fsanitize=leak -fmax-errors=6 -fbackslash -ffree-line-length-none"
export CMAKE_Fortran_FLAGS_RELEASE="-O2 -mtune=native -ffree-line-length-512 -fbackslash -ffpe-summary='none'"

# set important environment variables
export FC=$GFORTRAN
export CC=$GCC
export CXX=$GPP

cmake -G "Unix Makefiles"                                    \
-DOPTION__UNROLL_CONTROL_FILE="ON"                           \
-DCMAKE_BUILD_TYPE="$BUILD_TYPE "                            \
-DLIB_GCC="$LIB_GCC "                                        \
-DLIB_GFORTRAN="$LIB_GFORTRAN "                              \
-DR_SCRIPT="$R_SCRIPT "                                      \
-DCMAKE_INSTALL_PREFIX:PATH="$INSTALL_PREFIX "               \
-DCMAKE_Fortran_FLAGS_DEBUG="$CMAKE_Fortran_FLAGS_DEBUG "    \
-DCMAKE_Fortran_FLAGS_RELEASE="$CMAKE_Fortran_FLAGS_RELEASE" \
"../../.."
