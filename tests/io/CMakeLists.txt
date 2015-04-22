
message("TSPROC LOCATION: " ${CMAKE_INSTALL_PREFIX}/${TSPROC_EXECUTABLE} )

# Need input files over in build environment
file(GLOB infiles
     RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
     *.ssf *.inp *.wdm *.R BEC.* *.tpl *.dat *.ggo *.plt)

foreach(infilename ${infiles})
    set(infile  ${CMAKE_CURRENT_SOURCE_DIR}/${infilename})
    set(outfile ${CMAKE_CURRENT_BINARY_DIR}/${infilename})
    configure_file(${infile} ${outfile} COPYONLY)
endforeach()

set(tests_that_should_pass
       get_mul_series_statvar__basic_read.inp            # requires statvar.dat
       get_mul_series_statvar__test_of_partial_read.inp  # requires statvar.dat
	   get_series_wdm__basic_read.inp	             # requires testdata.wdm
	   get_mul_series_gsflow_gage__basic_read.inp    # requires testdata.ggo
	   GET_MUL_SERIES_PLOTGEN__basic_read.inp        # requires testdata.plt
       )

# process all tests_that_should_pass
foreach(test_filename ${tests_that_should_pass})
   #
   get_filename_component(basename ${test_filename} NAME_WE)

   # ** add_test: Add a test to the project with the specified arguments.
   # **    example: add_test(testname executable_name arg1 arg2 ... )
   #
   add_test(${basename}
            ${CMAKE_INSTALL_PREFIX}/${TSPROC_EXECUTABLE}
            ${test_filename} ${basename}.rec)

   message("Test string: " ${basename})
   message("   COMMAND " ${CMAKE_INSTALL_PREFIX}/${TSPROC_EXECUTABLE} " "
            ${CMAKE_CURRENT_BINARY_DIR}/${test_filename} " " ${CMAKE_CURRENT_BINARY_DIR}/${basename}.rec )
   message(" ")

   # set criteria for success of each test
   set_tests_properties(${basename}
                        PROPERTIES PASS_REGULAR_EXPRESSION "no more blocks to process")

   # set criteria for failure of each test
   set_tests_properties(${basename}
                        PROPERTIES FAIL_REGULAR_EXPRESSION "Error: ")


endforeach()

set(R_script_tests
    GET_MUL_SERIES_SSF__verify_read.R
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
