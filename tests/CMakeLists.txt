
# Need input files over in build environment
file(GLOB infiles
     RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
     *.ssf *.inp *.wdm *.R BEC.* *.tpl *.dat)

foreach(infilename ${infiles})
    set(infile  ${CMAKE_CURRENT_SOURCE_DIR}/${infilename})
    set(outfile ${CMAKE_CURRENT_BINARY_DIR}/${infilename})
    configure_file(${infile} ${outfile} COPYONLY)
endforeach()

# Find the tsproc that this build environment created
get_target_property(tsproclocation tsproc LOCATION)

set(tests_that_should_pass
       test__read_statvar_file.inp    # requires 'statvar.dat'
       hi_test_01.inp
       hi_test_all_classes.inp
       hi_test_inv_indices.inp
    #  hi_test_write_pest.inp   # requires interactive input
       test_hi_01.inp
       tsproc_test.inp
       wdm_test_01.inp
       tsp_test_flow_duration.inp
       )

set(R_script_tests
       tsp_compare_flow_duration_output.R
   )

# process all tests_that_should-pass
foreach(test_filename ${tests_that_should_pass})
   #
   get_filename_component(basename ${test_filename} NAME_WE)

   # ** add_test: Add a test to the project with the specified arguments.
   # **    example: add_test(testname executable_name arg1 arg2 ... )
   #
   add_test(NAME ${basename}
            COMMAND ${tsproclocation}
            ${CMAKE_CURRENT_SOURCE_DIR}/${test_filename} ${CMAKE_CURRENT_BINARY_DIR}/${basename}.rec)

   # set criteria for success of each test
   set_tests_properties(${basename}
                        PROPERTIES PASS_REGULAR_EXPRESSION "no more blocks to process")

   # set criteria for failure of each test
   set_tests_properties(${basename}
                        PROPERTIES FAIL_REGULAR_EXPRESSION "Error: ")

endforeach()

foreach(test_filename ${R_script_tests})
   get_filename_component(basename ${test_filename} NAME_WE)

   add_test(NAME ${basename}
            COMMAND RScript --vanilla
            ${CMAKE_CURRENT_SOURCE_DIR}/${test_filename} ${tsproclocation} )

   # set criteria for success of each test
   set_tests_properties(${basename}
                     PROPERTIES PASS_REGULAR_EXPRESSION "PASS")

   # set criteria for failure of each test
   set_tests_properties(${basename}
                     PROPERTIES FAIL_REGULAR_EXPRESSION "FAIL")

endforeach()
