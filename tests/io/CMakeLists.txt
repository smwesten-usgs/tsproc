
find_program(TSPROC_EXECUTABLE tsproc.exe tsproc
             PATH
			 ${CMAKE_BINARY_DIR}
 			 ${CMAKE_BINARY_DIR}/src
			 ${CMAKE_CURRENT_BINARY_DIR}
)

message("TSPROC LOCATION: " ${TSPROC_EXECUTABLE} )

# Need input files over in build environment
file(GLOB infiles
     RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
     *.ssf *.inp *.wdm *.R BEC.* *.tpl *.dat *.ggo)

foreach(infilename ${infiles})
    set(infile  ${CMAKE_CURRENT_SOURCE_DIR}/${infilename})
    set(outfile ${CMAKE_CURRENT_BINARY_DIR}/${infilename})
    configure_file(${infile} ${outfile} COPYONLY)
endforeach()

set(tests_that_should_pass
       read_statvar_file.inp              # requires 'statvar.dat'
	   read_wdm_file.inp		          # requires testdata.wdm
	   read_gsflow_modflow_ggo_file.inp   # requires testdata.ggo
       )

# process all tests_that_should_pass
foreach(test_filename ${tests_that_should_pass})
   #
   get_filename_component(basename ${test_filename} NAME_WE)

   # ** add_test: Add a test to the project with the specified arguments.
   # **    example: add_test(testname executable_name arg1 arg2 ... )
   #
   add_test(NAME ${basename}
            COMMAND ${TSPROC_EXECUTABLE}
            ${test_filename} ${basename}.rec)

   message("Test string: " ${basename})
   message("   COMMAND " ${TSPROC_EXECUTABLE} " "
            ${CMAKE_CURRENT_BINARY_DIR}/${test_filename} " " ${CMAKE_CURRENT_BINARY_DIR}/${basename}.rec )
   message(" ")

   # set criteria for success of each test
   set_tests_properties(${basename}
                        PROPERTIES PASS_REGULAR_EXPRESSION "no more blocks to process")

   # set criteria for failure of each test
   set_tests_properties(${basename}
                        PROPERTIES FAIL_REGULAR_EXPRESSION "Error: ")


endforeach()
