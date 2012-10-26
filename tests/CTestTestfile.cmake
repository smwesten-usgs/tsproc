# CMake generated Testfile for 
# Source directory: D:/SMWData/Source_Code/tsproc/tests
# Build directory: D:/SMWData/Source_Code/tsproc/build/win32/tests
# 
# This file includes the relevent testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
ADD_TEST(hi_test_01 "D:/SMWData/Source_Code/tsproc/build/win32/src/tsproc.exe" "D:/SMWData/Source_Code/tsproc/tests/hi_test_01.inp" "D:/SMWData/Source_Code/tsproc/build/win32/tests/hi_test_01.rec")
SET_TESTS_PROPERTIES(hi_test_01 PROPERTIES  FAIL_REGULAR_EXPRESSION "Error: " PASS_REGULAR_EXPRESSION "no more blocks to process")
ADD_TEST(hi_test_all_classes "D:/SMWData/Source_Code/tsproc/build/win32/src/tsproc.exe" "D:/SMWData/Source_Code/tsproc/tests/hi_test_all_classes.inp" "D:/SMWData/Source_Code/tsproc/build/win32/tests/hi_test_all_classes.rec")
SET_TESTS_PROPERTIES(hi_test_all_classes PROPERTIES  FAIL_REGULAR_EXPRESSION "Error: " PASS_REGULAR_EXPRESSION "no more blocks to process")
ADD_TEST(hi_test_inv_indices "D:/SMWData/Source_Code/tsproc/build/win32/src/tsproc.exe" "D:/SMWData/Source_Code/tsproc/tests/hi_test_inv_indices.inp" "D:/SMWData/Source_Code/tsproc/build/win32/tests/hi_test_inv_indices.rec")
SET_TESTS_PROPERTIES(hi_test_inv_indices PROPERTIES  FAIL_REGULAR_EXPRESSION "Error: " PASS_REGULAR_EXPRESSION "no more blocks to process")
ADD_TEST(test_hi_01 "D:/SMWData/Source_Code/tsproc/build/win32/src/tsproc.exe" "D:/SMWData/Source_Code/tsproc/tests/test_hi_01.inp" "D:/SMWData/Source_Code/tsproc/build/win32/tests/test_hi_01.rec")
SET_TESTS_PROPERTIES(test_hi_01 PROPERTIES  FAIL_REGULAR_EXPRESSION "Error: " PASS_REGULAR_EXPRESSION "no more blocks to process")
ADD_TEST(tsproc_test "D:/SMWData/Source_Code/tsproc/build/win32/src/tsproc.exe" "D:/SMWData/Source_Code/tsproc/tests/tsproc_test.inp" "D:/SMWData/Source_Code/tsproc/build/win32/tests/tsproc_test.rec")
SET_TESTS_PROPERTIES(tsproc_test PROPERTIES  FAIL_REGULAR_EXPRESSION "Error: " PASS_REGULAR_EXPRESSION "no more blocks to process")
ADD_TEST(wdm_test_01 "D:/SMWData/Source_Code/tsproc/build/win32/src/tsproc.exe" "D:/SMWData/Source_Code/tsproc/tests/wdm_test_01.inp" "D:/SMWData/Source_Code/tsproc/build/win32/tests/wdm_test_01.rec")
SET_TESTS_PROPERTIES(wdm_test_01 PROPERTIES  FAIL_REGULAR_EXPRESSION "Error: " PASS_REGULAR_EXPRESSION "no more blocks to process")
ADD_TEST(tsp_test_flow_duration "D:/SMWData/Source_Code/tsproc/build/win32/src/tsproc.exe" "D:/SMWData/Source_Code/tsproc/tests/tsp_test_flow_duration.inp" "D:/SMWData/Source_Code/tsproc/build/win32/tests/tsp_test_flow_duration.rec")
SET_TESTS_PROPERTIES(tsp_test_flow_duration PROPERTIES  FAIL_REGULAR_EXPRESSION "Error: " PASS_REGULAR_EXPRESSION "no more blocks to process")
