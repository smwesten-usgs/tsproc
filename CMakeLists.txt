# CMake project for swb

# based in part on an example found at:
# http://lagrange.mechse.illinois.edu/mwest/partmc/partmc-1.2.0/CMakeLists.txt

message("Processing top-level CMakelists.txt for project tsproc")


#------------------------------------------------------------------------
# Set basic project settings
#------------------------------------------------------------------------
project (tsproc Fortran CXX)
enable_language (Fortran CXX)

SET(CPACK_GENERATOR "TGZ")
include(CPack)

include(FindBoost)
find_package(Boost)
if(Boost_Found)
    include_directories(${Boost_INCLUDE_DIRS})
endif()

set(PACKAGE_BUGREPORT "smwesten@usgs.gov")
set(PACKAGE_NAME "tsproc")
set(PACKAGE_STRING "tsproc 2.0")
set(PACKAGE_TARNAME "tsproc")
set(PACKAGE_VERSION "2.0")

cmake_minimum_required(VERSION 2.6)
set(CMAKE_SYSTEM_IGNORE_PATH "c:/Python26;c:/Program Files (x86)")

#
option (MAKEFILE_VERBOSE
      "Produce verbose makefiles?" OFF)

option (TARGET__TSPROC_EXECUTABLE
      "Compile the main TSPROC executable?" ON)

option (TARGET__TSPROC_LIBRARY
      "Compile TSPROC as a library?" ON)

#------------------------------------------------------------------------
# Restrict CMAKE_BUILD_TYPE to "Release" or "Debug"
#------------------------------------------------------------------------
set( CMAKE_BUILD_TYPE "Release" CACHE STRING
       "Compile in DEBUG or RELEASE mode" )

#------------------------------------------------------------------------
# What Fortran compiler are we using?
#------------------------------------------------------------------------
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

find_program(UNAME NAMES uname)

#------------------------------------------------------------------------
# Set platform-specific options
#------------------------------------------------------------------------
if(CMAKE_HOST_WIN32)

   if (Fortran_COMPILER_NAME MATCHES "gfortran[.a-z]*")

      message(" <-- setting options for gfortran on MinGW -->")

      set (CMAKE_Fortran_FLAGS_RELEASE "-Ofast -fwhole-file -ffree-line-length-none")
      set (CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -Wall -Wextra -fcheck=all -fexceptions -ffree-line-length-none")

      set_property(CACHE CMAKE_BUILD_TYPE
              PROPERTY STRINGS "Release" "Debug")

      add_custom_target(copy ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/tsproc.exe ${CMAKE_INSTALL_PREFIX}/tsproc.exe )

#     SET(CMAKE_INSTALL_PREFIX "../../../lib/win32" CACHE PATHFILE "Installation directory")

   endif ()

elseif(CMAKE_HOST_UNIX)

   # Accept CMAKE defaults...
   # I think compiler choice is keyed off of the FC environment variable.

else()

   message(FATAL_ERROR "Windows (32/64)/gfortran/MinGW or Linux-64/Intel environments were not detected!")

endif()

link_directories(${PROJECT_SOURCE_DIR}/lib/${PLATFORM_SUBDIR})

get_filename_component(COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

enable_testing()
add_subdirectory(src)
add_subdirectory(tests)

