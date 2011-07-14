# CMake project for swb

# based in part on an example found at:
# http://lagrange.mechse.illinois.edu/mwest/partmc/partmc-1.2.0/CMakeLists.txt

message("Processing top-level CMakelists.txt for project libanne")

#------------------------------------------------------------------------
# Set basic project settings
#------------------------------------------------------------------------
project (libanne)
enable_language (Fortran)

set(PACKAGE_BUGREPORT "smwesten@usgs.gov")
set(PACKAGE_NAME "libanne")
set(PACKAGE_STRING "libanne 4.0")
set(PACKAGE_TARNAME "libanne")
set(PACKAGE_VERSION "4.0")

cmake_minimum_required(VERSION 2.6)
set(CMAKE_SYSTEM_IGNORE_PATH "c:/Python26;c:/Program Files (x86)")

#
option (MAKEFILE_VERBOSE
      "Produce verbose makefiles?" OFF)

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

     message(" <-- setting options for gfortran on MinGW (32-bit) -->")

     set (CMAKE_Fortran_FLAGS_RELEASE "-Ofast -fwhole-file -ffree-line-length-none")
     set (CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -Wall -Wextra -fcheck=all -fexceptions -ffree-line-length-none")

     set_property(CACHE CMAKE_BUILD_TYPE
             PROPERTY STRINGS "Release" "Debug")

     set(INCLUDE_DIR ${INCLUDE_DIR} ${PROJECT_SOURCE_DIR}/include/win32/gfortran_mingw )

     set(EXTRA_LIBS ${EXTRA_LIBS} ${GCC_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${GFORTRAN_LIB} )

     SET(CMAKE_INSTALL_PREFIX "d:/dos" CACHE PATHFILE "Installation directory")

   endif ()

elseif(CMAKE_HOST_UNIX)

   if (Fortran_COMPILER_NAME MATCHES "ifort[.a-z]*")
     #------------------------------------------------------------------------
     # Set compilation and link preferences for Intel Fortran on Linux
     #------------------------------------------------------------------------
     set (CMAKE_Fortran_FLAGS_RELEASE "-fast -fPIC")
     set (CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -traceback -fpe0 -check all -debug all -fPIC")
     set(CMAKE_FIND_LIBRARY_PREFIXES lib)
#     set(CMAKE_FIND_LIBRARY_SUFFIXES ".so" "")
     set(CMAKE_FIND_LIBRARY_SUFFIXES ".a" "")

     find_program(CMAKE_MAKE_PROGRAM make
       DOC "Find a suitable make program for building under Linux/IFORT"
       HINTS
       /bin
       /usr/bin)
       message("CMAKE_MAKE_PROGRAM: " ${CMAKE_MAKE_PROGRAM})

     find_program(CMAKE_SH sh
       DOC "Find a suitable shell for building  under Linux/IFORT"
       HINTS
       /bin)
       message("CMAKE_SH: " ${CMAKE_SH})

       PATH_SUFFIXES
       /usr/lib)

     set(EXTRA_LIBS ${EXTRA_LIBS} ${NETCDF_FORTRAN_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${DISLIN_FORTRAN_LIB} )
#     set(EXTRA_LIBS ${EXTRA_LIBS} ${GCC_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${OPEN_MOTIF_LIB} )
     set(EXTRA_LIBS ${EXTRA_LIBS} ${X_LIB} )

     message("libs: " "${EXTRA_LIBS}")

     SET(CMAKE_INSTALL_PREFIX "/usr/local/bin")

   endif (Fortran_COMPILER_NAME MATCHES "ifort[.a-z]*")

else()

  message(FATAL_ERROR "Windows 32/gfortran/MinGW or Linux-64/Intel environments were not detected!")

endif()

#set ( CMAKE_Fortran_FLAGS_DEBUG ${Fortran_FLAGS_DEBUG})
#set ( CMAKE_Fortran_FLAGS_RELEASE ${Fortran_FLAGS_RELEASE})

#if( CMAKE_BUILD_TYPE STREQUAL "Debug")
##  add_definitions(${Fortran_FLAGS_DEBUG})
#else()
#  set(CMAKE_BUILD_TYPE "Release")
##  add_definitions(${Fortran_FLAGS_RELEASE})
#endif()

include_directories(${INCLUDE_DIR} ${INCLUDE_DISLIN})
set(EXTRA_LIBS ${EXTRA_LIBS} ${NETCDF_LIBS} ${DISLIN_LIBS})
#link_libraries( ${EXTRA_LIBS} )

get_filename_component(COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)


add_subdirectory(src)
