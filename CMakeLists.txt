# CMake project for tsproc

# This CMakeLists.txt file is written with the expectation that
# the following configuration variables are set and passed
# during CMake invocation:

# CMAKE_BUILD_TYPE       | possible values: "debug", "release", "profile"
# CMAKE_INSTALL_PREFIX   | ex: "D:/DOS"
# CMAKE_MAKE_PROGRAM     | ex: "c:/MinGW64/bin/mingw32-make.exe"
# Fortran_FLAGS_DEBUG
# Fortran_FLAGS_RELEASE
# Fortran_FLAGS_PROFILE
# PATH_TO_R              | ex: "C:/Program Files/R/R-3.0.1/bin"
# OS                     | possible values: "win_x86", "win_x64", "max_osx", "linux"
# Fortran_COMPILER_NAME  | ex: "gfortran", "ifort"
# COMPILER_DIR           | ex: "c:/MinGW"
# COMPILER_TRIPLET       | ex: "x86_64-w64-mingw32", "x86_64-apple-darwin12.4.0"
# COMPILER_VERSION       | ex: "4.8.2"

message("Processing top-level CMakelists.txt for project tsproc")

# halt if we are attempting to build inside source directory
if(${CMAKE_SOURCE_DIR}/src STREQUAL ${CMAKE_BINARY_DIR})
  message(FATAL_ERROR "Cannot be built in the source directory. Use
  out-of-source build instead.")
  message(FATAL_ERROR "  cd /some/work/dir/build")
  message(FATAL_ERROR "  cmake -i /path/to/source")
endif(${CMAKE_SOURCE_DIR}/src STREQUAL ${CMAKE_BINARY_DIR})

#------------------------------------------------------------------------
# Set basic project settings
#------------------------------------------------------------------------
project (tsproc)
enable_language (Fortran CXX )

SET(CPACK_GENERATOR "TGZ")
include(CPack)

set(PACKAGE_BUGREPORT "smwesten@usgs.gov")
set(PACKAGE_NAME "tsproc")
set(PACKAGE_STRING "tsproc 1.0.4")
set(PACKAGE_TARNAME "tsproc")
set(PACKAGE_VERSION "1.0.4")

set(CMAKE_INCLUDE_CURRENT_DIR ON)
SET( CMAKE_CXX_FLAGS  "${CMAKE_Fortran_FLAGS_RELEASE}")
set( CMAKE_EXE_LINKER_FLAGS="-static" )

cmake_minimum_required(VERSION 3.6)

include( cmake/options_and_targets.cmake )

set( EXTERNAL_LIBS ${EXTERNAL_LIBS} ${LIB_GCC})
set( EXTERNAL_LIBS ${EXTERNAL_LIBS} ${LIB_GFORTRAN})
link_libraries(${EXTERNAL_LIBS})

if (  CMAKE_HOST_WIN32 )
  set ( TSPROC_EXECUTABLE "tsproc.exe" )
else ()
  set ( TSPROC_EXECUTABLE "tsproc" )
endif()

add_custom_target(copy ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/${TSPROC_EXECUTABLE} ${CMAKE_INSTALL_PREFIX}/${TSPROC_EXECUTABLE} )

# include Git commit hash
include( cmake/version_control.cmake )

# print a summary of important details to screen
include( cmake/print_summary.cmake )

add_subdirectory(src)
enable_testing()
add_subdirectory(tests/io)
add_subdirectory(tests/calcs)
