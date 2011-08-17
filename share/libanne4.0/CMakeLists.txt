message("Processing top-level CMakelists.txt for WDM libraries")

#------------------------------------------------------------------------
# Set basic project settings
#------------------------------------------------------------------------
project (tsproc)
enable_language (C Fortran)

set(PACKAGE_BUGREPORT "smwesten@usgs.gov")
set(PACKAGE_NAME "wdmlib")


cmake_minimum_required(VERSION 2.6)

if(CMAKE_HOST_WIN32)

   if (Fortran_COMPILER_NAME MATCHES "gfortran[.a-z]*")

     message(" <-- setting options for gfortran on MinGW (32-bit) -->")

     set (PLATFORM_SUBDIR "win32")
     set (CMAKE_Fortran_FLAGS_RELEASE "-Ofast -fwhole-file -ffree-line-length-none")
     set (CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -Wall -Wextra -fcheck=all -fexceptions -ffree-line-length-none")

     set_property(CACHE CMAKE_BUILD_TYPE
             PROPERTY STRINGS "Release" "Debug")
	 
        include_directories( ${PROJECT_SOURCE_DIR}/include )
	include_directories( C:/mingw/include/c++/4.6.2 )
	include_directories( C:/mingw/include/c++/4.6.2/x86_64-w64-mingw32 )


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
     set(CMAKE_FIND_LIBRARY_SUFFIXES ".a" "")

     message("libs: " "${EXTRA_LIBS}")

     SET(CMAKE_INSTALL_PREFIX "/usr/local/bin")

   endif (Fortran_COMPILER_NAME MATCHES "ifort[.a-z]*")

else()

  message(FATAL_ERROR "Windows 32/gfortran/MinGW or Linux-64/Intel environments were not detected!")

endif()


add_subdirectory(src/util)
add_subdirectory(src/adwdm)
add_subdirectory(src/wdm)

add_custom_command(TARGET wdmlib
     POST_BUILD
          COMMAND ${CMAKE_COMMAND} -E copy ${PROJECT_SOURCE_DIR}/build/${PLATFORM_SUBDIR}/adwdm/libadwdm.a ${PROJECT_SOURCE_DIR}/lib/${PLATFORM_SUBDIR}/libadwdm.a 
          COMMAND ${CMAKE_COMMAND} -E copy ${PROJECT_SOURCE_DIR}/build/${PLATFORM_SUBDIR}/wdm/libwdm.a ${PROJECT_SOURCE_DIR}/lib/${PLATFORM_SUBDIR}/libwdm.a 
          COMMAND ${CMAKE_COMMAND} -E copy ${PROJECT_SOURCE_DIR}/build/${PLATFORM_SUBDIR}/util/libutil.a ${PROJECT_SOURCE_DIR}/lib/${PLATFORM_SUBDIR}/libutil.a )

