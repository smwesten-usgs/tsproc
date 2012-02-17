
add_definitions(-D_VSTRING_ALLOCATABLE -D_VSTRINGLIST_ALLOCATABLE
    -D_VSTRING_STATIC_BUFFER)

set(SRCS
    m_vstring.F90
    m_vstringformat.F90
    m_vstringlist.F90
    stringmanip.F90
    stringquery.F90
    textstr.F90
    tokenize.F90
    )

add_library(flibs_strings 
    ${SRCS}
    )

