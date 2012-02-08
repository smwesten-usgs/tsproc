# CMake generated Testfile for 
# Source directory: D:/SMWData/Source_Code/tsproc/share/tests
# Build directory: D:/SMWData/Source_Code/tsproc/share/tests
# 
# This file includes the relevent testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
ADD_TEST(hi_test_01 "D:/SMWData/Source_Code/tsproc/src/tsproc.exe" "D:/SMWData/Source_Code/tsproc/share/tests/hi_test_01.inp" "D:/SMWData/Source_Code/tsproc/share/tests/hi_test_01.rec")
SET_TESTS_PROPERTIES(hi_test_01 PROPERTIES  FAIL_REGULAR_EXPRESSION "Error: " PASS_REGULAR_EXPRESSION "no more blocks to process")
ADD_TEST(hi_test_all_classes "D:/SMWData/Source_Code/tsproc/src/tsproc.exe" "D:/SMWData/Source_Code/tsproc/share/tests/hi_test_all_classes.inp" "D:/SMWData/Source_Code/tsproc/share/tests/hi_test_all_classes.rec")
SET_TESTS_PROPERTIES(hi_test_all_classes PROPERTIES  FAIL_REGULAR_EXPRESSION "Error: " PASS_REGULAR_EXPRESSION "no more blocks to process")
ADD_TEST(hi_test_inv_indices "D:/SMWData/Source_Code/tsproc/src/tsproc.exe" "D:/SMWData/Source_Code/tsproc/share/tests/hi_test_inv_indices.inp" "D:/SMWData/Source_Code/tsproc/share/tests/hi_test_inv_indices.rec")
SET_TESTS_PROPERTIES(hi_test_inv_indices PROPERTIES  FAIL_REGULAR_EXPRESSION "Error: " PASS_REGULAR_EXPRESSION "no more blocks to process")
ADD_TEST(test_hi_01 "D:/SMWData/Source_Code/tsproc/src/tsproc.exe" "D:/SMWData/Source_Code/tsproc/share/tests/test_hi_01.inp" "D:/SMWData/Source_Code/tsproc/share/tests/test_hi_01.rec")
SET_TESTS_PROPERTIES(test_hi_01 PROPERTIES  FAIL_REGULAR_EXPRESSION "Error: " PASS_REGULAR_EXPRESSION "no more blocks to process")
ADD_TEST(tsproc_test "D:/SMWData/Source_Code/tsproc/src/tsproc.exe" "D:/SMWData/Source_Code/tsproc/share/tests/tsproc_test.inp" "D:/SMWData/Source_Code/tsproc/share/tests/tsproc_test.rec")
SET_TESTS_PROPERTIES(tsproc_test PROPERTIES  FAIL_REGULAR_EXPRESSION "Error: " PASS_REGULAR_EXPRESSION "no more blocks to process")
ADD_TEST(wdm_test_01 "D:/SMWData/Source_Code/tsproc/src/tsproc.exe" "D:/SMWData/Source_Code/tsproc/share/tests/wdm_test_01.inp" "D:/SMWData/Source_Code/tsproc/share/tests/wdm_test_01.rec")
SET_TESTS_PROPERTIES(wdm_test_01 PROPERTIES  FAIL_REGULAR_EXPRESSION "Error: " PASS_REGULAR_EXPRESSION "no more blocks to process")
