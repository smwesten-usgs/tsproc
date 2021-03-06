message(" ")
message("--------------------------------------------------------------------")
message("   Summary of CMAKE environment variables")
message("--------------------------------------------------------------------")
message("BUILD TYPE: " ${CMAKE_BUILD_TYPE})
message( " ")
message("CMAKE_HOST_WIN32: " ${CMAKE_HOST_WIN32})
message("CMAKE_HOST_APPLE: " ${CMAKE_HOST_APPLE})
message("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
message("CMAKE Fortran flags (Debug):      " ${CMAKE_Fortran_FLAGS_DEBUG})
message("CMAKE Fortran flags (Profile):      " ${CMAKE_Fortran_FLAGS_PROFILE})
message("CMAKE Fortran flags (Release):      " ${CMAKE_Fortran_FLAGS_RELEASE})
message( " ")
MESSAGE("Install to directory: " ${CMAKE_INSTALL_PREFIX})
message(" ")
message("LIBRARIES:")
message("  GCC          ${LIB_GCC}")
message("  GFORTRAN     ${LIB_GFORTRAN}")
message(" ")
message("EXTERNAL LINK LIBRARIES:")
message("${EXTERNAL_LIBS}")
message(" ")
message("R EXECUTABLE:")
message("${R_SCRIPT}")
message(" ")
message("CMAKE_BINARY_DIR: " ${CMAKE_BINARY_DIR})
message("CMAKE_CURRENT_SOURCE_DIR: " ${CMAKE_CURRENT_SOURCE_DIR})
message("PROJECT_SOURCE_DIR: " ${PROJECT_SOURCE_DIR})
message( " ")
message("--------------------------------------------------------------------")
