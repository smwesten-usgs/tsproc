
add_executable(xcat xcat.F90)

# Need input files over in build environment
file(GLOB infiles
     RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
    +.ssf *.inp *.wdm *.dat)
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
       step4f.inp
       hi_test_all_classes.inp
       hi_test_inv_indices.inp
    #  hi_test_write_pest.inp   # requires interactive input
       test_hi_01.inp
       tsproc_test.inp
       wdm_test_01.inp
       )

foreach(testfname ${tests})
   get_filename_component(basename ${testfname} NAME_WE)
   add_test(${basename} ${tsproclocation} ${CMAKE_CURRENT_SOURCE_DIR}/${testfname} ${CMAKE_CURRENT_BINARY_DIR}/${basename}.rec)
   set_tests_properties(${basename} PROPERTIES PASS_REGULAR_EXPRESSION "no more blocks to process")
   set_tests_properties(${basename} PROPERTIES FAIL_REGULAR_EXPRESSION "Error: ")

endforeach()

set(fname step4p.out)
add_test(${fname} xcat ${fname})
set_tests_properties(${fname} PROPERTIES PASS_REGULAR_EXPRESSION
    "mon_s_orad +01/01/1800 +00:00:00 +140.678")
set_tests_properties(${fname} PROPERTIES PASS_REGULAR_EXPRESSION
    "mon_s_orad +11/01/1800 +00:00:00 +130.507")
set_tests_properties(${fname} PROPERTIES PASS_REGULAR_EXPRESSION
    "s_001_m +10/01/1997 +00:00:00 +538.326")
set_tests_properties(${fname} PROPERTIES PASS_REGULAR_EXPRESSION
    "s_001_max +02/01/1993 +00:00:00 +686.714")
set_tests_properties(${fname} PROPERTIES PASS_REGULAR_EXPRESSION
    "s_001_mbf +11/01/1992 +00:00:00 +572.851")
set_tests_properties(${fname} PROPERTIES PASS_REGULAR_EXPRESSION
    "s_001_hyd +06/08/1993 +00:00:00 +882.512")
set_tests_properties(${fname} PROPERTIES PASS_REGULAR_EXPRESSION
    "mon_s_001 +09/01/1800 +00:00:00 +551.646")
set_tests_properties(${fname} PROPERTIES PASS_REGULAR_EXPRESSION
    "s_002_a +10/01/1986 +00:00:00 +117.158")
set_tests_properties(${fname} PROPERTIES PASS_REGULAR_EXPRESSION
    "s_002_m +07/01/1970 +00:00:00 +88.3858")
set_tests_properties(${fname} PROPERTIES PASS_REGULAR_EXPRESSION
    "s_036_m +07/01/1974 +00:00:00 +36.2482")
set_tests_properties(${fname} PROPERTIES PASS_REGULAR_EXPRESSION
    "mon_s_148 +11/01/1800 +00:00:00 +31.8252")
set_tests_properties(${fname} PROPERTIES PASS_REGULAR_EXPRESSION
    "s_148_hyd +03/18/1990 +00:00:00 +48.8485")

set(fname hi_all_classes_list_output_obs.txt)
add_test(${fname} xcat ${fname})
set_tests_properties(${fname} PROPERTIES PASS_REGULAR_EXPRESSION
   "ML1: Mean minimum monthly flow, January +28.200")
set_tests_properties(${fname} PROPERTIES PASS_REGULAR_EXPRESSION
   "ML13: CV of minimum monthly flows +26.621")
set_tests_properties(${fname} PROPERTIES PASS_REGULAR_EXPRESSION
   "ML16: Median minimum annual flow / median annual flow +0.680")
set_tests_properties(${fname} PROPERTIES PASS_REGULAR_EXPRESSION
   "ML22: Mean annual minimum flow divided by catchment area +0.589")
set_tests_properties(${fname} PROPERTIES PASS_REGULAR_EXPRESSION
   "ML17: 7-day minimum flow / mean annual flow +0.581")

