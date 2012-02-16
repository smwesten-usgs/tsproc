
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

set(tests
    #  BEC_TSPROC_GSFLOW.inp    # requires 'statvar.dat'
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

foreach(testfname ${tests})
   get_filename_component(basename ${testfname} NAME_WE)
   add_test(${basename} ${tsproclocation} ${CMAKE_CURRENT_SOURCE_DIR}/${testfname} ${CMAKE_CURRENT_BINARY_DIR}/${basename}.rec)
   set_tests_properties(${basename} PROPERTIES PASS_REGULAR_EXPRESSION "no more blocks to process")
   set_tests_properties(${basename} PROPERTIES FAIL_REGULAR_EXPRESSION "Error: ")
endforeach()
