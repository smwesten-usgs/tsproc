
option (MAKEFILE_VERBOSE
      "Produce verbose makefiles?" OFF)

option (TARGET__TSPROC_EXECUTABLE
      "Compile the main TSPROC executable?" ON)

option (TARGET__TSPROC_LIBRARY
      "Compile TSPROC as a library?" OFF)

option (OPTION__UNROLL_CONTROL_FILE
      "Compile the code with support for control file unrolling?" OFF)
	  
if(OPTION__UNROLL_CONTROL_FILE)
  add_definitions (-DUNROLL_CONTROL_FILE)
endif()

set( CMAKE_BUILD_TYPE "Release" CACHE STRING
       "Compile in DEBUG or RELEASE mode" )



