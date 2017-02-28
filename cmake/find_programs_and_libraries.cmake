
set(CMAKE_FIND_LIBRARY_PREFIXES "lib")
set(CMAKE_FIND_LIBRARY_SUFFIXES ".a")

# MinGW libraries seem to move around with each release. Define the 
# library locations that we have seen to date here.
set(LIB_PATH "${COMPILER_DIR}/lib/gcc/${COMPILER_TRIPLET}/${COMPILER_VERSION}" )
set(LIB_PATH ${LIB_PATH} "${COMPILER_DIR}/${COMPILER_TRIPLET}/lib" )
set(LIB_PATH ${LIB_PATH} "${COMPILER_DIR}/lib" )

find_program( R_SCRIPT Rscript.exe Rscript
    HINTS    
	"c:/Program Files/R"
    ENV R_HOME
	${PATH_TO_R}
    PATHS
    "/usr/bin"
)

find_library(LIBGCC
    NAMES gcc libgcc libgcc.a
    HINTS ${LIB_PATH} )	

find_library(LIBGFORTRAN
    NAMES gfortran libgfortran libgfortran.a
    HINTS ${LIB_PATH} )	

find_library(LIBQUADMATH
    NAMES libquadmath libquadmath.a
    HINTS ${LIB_PATH} )	

if ("${OS}" STREQUAL "win_x64" OR "${OS}" STREQUAL "win_x86")

  find_library(LIBWINPTHREAD
          NAMES libwinpthread.a
          PATHS ${LIB_PATH} )

  find_library(LIBWS2_32
          NAMES ws2_32 libws2_32 libws2_32.a
          PATHS ${LIB_PATH} )

  set( EXTERNAL_LIBS ${EXTERNAL_LIBS} ${LIBWINPTHREAD} ${LIBWS2_32} )
  
endif()  

set( EXTERNAL_LIBS ${EXTERNAL_LIBS} ${LIBGCC} ${LIBQUADMATH} ${LIBGFORTRAN} )
    
# set the path and filename for the tsproc executable
# i.e. if one types 'make copy', where does the resulting executable end up, and what is its name?
if (  CMAKE_HOST_WIN32 )
  set ( TSPROC_EXECUTABLE "tsproc.exe" )
else ()
  set ( TSPROC_EXECUTABLE "tsproc" )
endif()

link_libraries( ${EXTERNAL_LIBS} )
