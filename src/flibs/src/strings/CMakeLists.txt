
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod_files)
include_directories(${CMAKE_BINARY_DIR}/mod_files)

add_definitions(-D_VSTRING_ALLOCATABLE
                -D_VSTRINGLIST_ALLOCATABLE)

include_directories(../datastructures)

add_library(flibs_strings

            m_vstring.F90
            m_vstringlist.F90
            tokenize.F90
            tokenlist.F90)

