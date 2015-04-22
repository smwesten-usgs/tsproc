message("TSPROC LOCATION: " ${CMAKE_INSTALL_PREFIX}/${TSPROC_EXECUTABLE} )

# Need input files over in build environment
file(GLOB infiles
     RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
     *.ssf *.inp *.wdm *.R BEC.* *.tpl *.dat *.csv)

foreach(infilename ${infiles})
    set(infile  ${CMAKE_CURRENT_SOURCE_DIR}/${infilename})
    set(outfile ${CMAKE_CURRENT_BINARY_DIR}/${infilename})
    configure_file(${infile} ${outfile} COPYONLY)
endforeach()

set(R_script_tests
       FLOW_DURATION__compare_to_R_calculation.R
	   SERIES_COMPARE__compare_to_R_calculation.R
	   HYDROLOGICAL_INDICES__compare_to_HIT_calculation.R
   )

if(DEFINED R_SCRIPT)

  foreach(test_filename ${R_script_tests})
     get_filename_component(basename ${test_filename} NAME_WE)

   message("Test string: " ${basename})
   message("   COMMAND " ${R_SCRIPT} " --vanilla "
              ${CMAKE_CURRENT_SOURCE_DIR}/${test_filename} " " ${CMAKE_INSTALL_PREFIX}/${TSPROC_EXECUTABLE} )
   message(" ")

     add_test(${basename}
              ${R_SCRIPT}
              --vanilla
              ${CMAKE_CURRENT_SOURCE_DIR}/${test_filename}
              ${CMAKE_INSTALL_PREFIX}/${TSPROC_EXECUTABLE} )

     # set criteria for success of each test
     set_tests_properties(${basename}
                       PROPERTIES PASS_REGULAR_EXPRESSION "PASS")

     # set criteria for failure of each test
     set_tests_properties(${basename}
                       PROPERTIES FAIL_REGULAR_EXPRESSION "FAIL")

  endforeach()

endif()
