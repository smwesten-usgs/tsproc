# CMake project for swb

# based in part on an example found at:
# http://lagrange.mechse.illinois.edu/mwest/partmc/partmc-1.2.0/CMakeLists.txt

message("Processing top-level CMakelists.txt for project tsproc")

#------------------------------------------------------------------------
# Set basic project settings
#------------------------------------------------------------------------
project (tsproc)
enable_language (C Fortran)

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

#option (TARGET__REBUILD_WDM_LIB
#      "Rebuild the WDM library?" OFF)

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

     set (PLATFORM_SUBDIR "win32")
     set (CMAKE_Fortran_FLAGS_RELEASE "-Ofast -fwhole-file -ffree-line-length-none")
     set (CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -Wall -Wextra -fcheck=all -fexceptions -ffree-line-length-none")

     set_property(CACHE CMAKE_BUILD_TYPE
             PROPERTY STRINGS "Release" "Debug")
     set_target_properties(tsproc tsproclib
         PROPERTIES LINKER_LANGUAGE gfortran )
	 
	 include_directories( ${PROJECT_SOURCE_DIR}/include )
	include_directories( C:/mingw/include/c++/4.6.2 )
	include_directories( C:/mingw/include/c++/4.6.2/x86_64-w64-mingw32 )
 
     add_custom_target(copy ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/tsproc.exe ${CMAKE_INSTALL_PREFIX}/tsproc.exe )

#     SET(CMAKE_INSTALL_PREFIX "../../../lib/win32" CACHE PATHFILE "Installation directory")

   endif ()

elseif(CMAKE_HOST_UNIX)

   if (Fortran_COMPILER_NAME MATCHES "ifort[.a-z]*")
     #------------------------------------------------------------------------
     # Set compilation and link preferences for Intel Fortran on Linux
     #------------------------------------------------------------------------
     set (PLATFORM_SUBDIR "linux_x64")
     set (CMAKE_Fortran_FLAGS_RELEASE "-fast -fPIC")
     set (CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -traceback -fpe0 -check all -debug all -fPIC")
     set(CMAKE_FIND_LIBRARY_PREFIXES lib)
#     set(CMAKE_FIND_LIBRARY_SUFFIXES ".so" "")
     set(CMAKE_FIND_LIBRARY_SUFFIXES ".a" "")

     message("libs: " "${EXTRA_LIBS}")

     SET(CMAKE_INSTALL_PREFIX "/usr/local/bin")

   endif (Fortran_COMPILER_NAME MATCHES "ifort[.a-z]*")

else()

  message(FATAL_ERROR "Windows 32/gfortran/MinGW or Linux-64/Intel environments were not detected!")

endif()

#set(INCLUDE_DIR ${INCLUDE_DIR} ${PROJECT_SOURCE_DIR}/include )

include_directories(${INCLUDE_DIR})
link_directories(${PROJECT_SOURCE_DIR}/lib/${PLATFORM_SUBDIR})

get_filename_component(COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

#if(TARGET__REBUILD_WDM_LIB) 
#
#  add_subdirectory(${PROJECT_SOURCE_DIR}/share/libanne4.0/src/wdm)
#  add_subdirectory(${PROJECT_SOURCE_DIR}/share/libanne4.0/src/adwdm)
#  add_subdirectory(${PROJECT_SOURCE_DIR}/share/libanne4.0/src/util)
#
#endif()


if(TARGET__TSPROC_EXECUTABLE)

  add_executable(tsproc
            src/compute_hi.c
            src/tsp_command_processors.F90
            src/tsp_data_structures.F90
            src/tsp_equation_parser.F90
            src/tsp_hydrologic_indices.F90
            src/tsp_input.F90
            src/tsp_main.F90
            src/tsp_main_loop.F90
            src/tsp_output.F90
            src/tsp_time_series_processors.F90
            src/tsp_utilities.F90
            src/wsc_additions.F90 )

  target_link_libraries(tsproc ${PROJECT_SOURCE_DIR}/lib/${PLATFORM_SUBDIR}/libwdm.a
                               ${PROJECT_SOURCE_DIR}/lib/${PLATFORM_SUBDIR}/libadwdm.a
                               ${PROJECT_SOURCE_DIR}/lib/${PLATFORM_SUBDIR}/libutil.a )

endif()

if(TARGET__TSPROC_LIBRARY)

  add_library(tsproclib STATIC
            src/compute_hi.c
            src/tsp_command_processors.F90
            src/tsp_data_structures.F90
            src/tsp_equation_parser.F90
            src/tsp_hydrologic_indices.F90
            src/tsp_input.F90
            src/tsp_main.F90
            src/tsp_main_loop.F90
            src/tsp_output.F90
            src/tsp_time_series_processors.F90
            src/tsp_utilities.F90
            src/wsc_additions.F90 )

  target_link_libraries(tsproclib ${PROJECT_SOURCE_DIR}/lib/${PLATFORM_SUBDIR}/libwdm.a
                               ${PROJECT_SOURCE_DIR}/lib/${PLATFORM_SUBDIR}/libadwdm.a
                               ${PROJECT_SOURCE_DIR}/lib/${PLATFORM_SUBDIR}/libutil.a )

endif()

