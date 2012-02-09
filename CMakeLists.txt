# CMake project for swb

# based in part on an example found at:
# http://lagrange.mechse.illinois.edu/mwest/partmc/partmc-1.2.0/CMakeLists.txt

message("Processing top-level CMakelists.txt for project tsproc")

#------------------------------------------------------------------------------------#
#                           Build directory check                                    #
#------------------------------------------------------------------------------------#
if(${CMAKE_SOURCE_DIR}/src STREQUAL ${CMAKE_BINARY_DIR})
  message(FATAL_ERROR "Cannot be built in the source directory. Use
  out-of-source build instead.")
  message(FATAL_ERROR "  cd /some/work/dir/build")
  message(FATAL_ERROR "  cmake -i /path/to/source")
endif(${CMAKE_SOURCE_DIR}/src STREQUAL ${CMAKE_BINARY_DIR})

#------------------------------------------------------------------------
# Set basic project settings
#------------------------------------------------------------------------
project (tsproc Fortran CXX)
enable_language (Fortran CXX)

SET(CPACK_GENERATOR "TGZ")
include(CPack)

set(PACKAGE_BUGREPORT "smwesten@usgs.gov")
set(PACKAGE_NAME "tsproc")
set(PACKAGE_STRING "tsproc 2.0")
set(PACKAGE_TARNAME "tsproc")
set(PACKAGE_VERSION "2.0")

cmake_minimum_required(VERSION 2.6)

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

#------------------------------------------------------------------------
# Set platform-specific options
#------------------------------------------------------------------------
if(CMAKE_HOST_WIN32)

    if (Fortran_COMPILER_NAME MATCHES "gfortran[.a-z]*")

      message(" <-- setting options for gfortran on MinGW -->")

      set_property(CACHE CMAKE_BUILD_TYPE
                PROPERTY STRINGS "Release" "Debug")

      FIND_LIBRARY(GCC_LIB libgcc.a
      HINTS
        C:/MinGW/lib/gcc
        C:/MinGW/lib/gcc/mingw32
        C:/MinGW32/lib/gcc
        C:/MinGW32/lib/gcc/mingw32
      PATH
        C:/MinGW/lib/gcc
        C:/MinGW/lib/gcc/mingw32
        C:/MinGW32/lib/gcc
        C:/MinGW32/lib/gcc/mingw32
      PATH_SUFFIXES
        4.6.2
        4.6.1
        4.5.2
        4.5.1

      )

      FIND_LIBRARY(GFORTRAN_LIB libgfortran.a
        HINTS
          C:/MinGW/lib/gcc
          C:/MinGW/lib/gcc/mingw32
          C:/MinGW32/lib/gcc
          C:/MinGW32/lib/gcc/mingw32
        PATH
          C:/MinGW/lib/gcc
          C:/MinGW/lib/gcc/mingw32
          C:/MinGW32/lib/gcc
          C:/MinGW32/lib/gcc/mingw32
        PATH_SUFFIXES
          4.6.2
          4.6.1
          4.5.2
          4.5.1

      )

      message("  GCC_LIB = " ${GCC_LIB})
      message("  GFORTRAN_LIB = " ${GFORTRAN_LIB})

      set (CMAKE_Fortran_FLAGS_RELEASE "-Ofast -fwhole-file -ffree-line-length-none")

      set (CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -gdb -Wall -Wextra -fcheck=all")
      set (CMAKE_Fortran_FLAGS_DEBUG ${CMAKE_Fortran_FLAGS_DEBUG} "-fexceptions -ffree-line-length-none")
      set (CMAKE_Fortran_FLAGS_DEBUG ${CMAKE_Fortran_FLAGS_DEBUG} "-static -static-libgcc")

      SET(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static -static-libgcc -static-libgfortran")

      set(EXTRA_LIBS ${EXTRA_LIBS} ${GCC_LIB} ${GFORTRAN_LIB} ${STDC++_LIB})

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
