
set(CMAKE_FIND_LIBRARY_PREFIXES "lib")
set(CMAKE_FIND_LIBRARY_SUFFIXES ".a")

find_program( R_SCRIPT Rscript.exe Rscript
    HINTS
    ENV R_HOME
	${PATH_TO_R}
    PATHS
    "c:/Program Files/R"
    "c:/Program Files/R/R-3.0.1/bin"
    "/usr/bin"
)

find_library(LIBGCC
        NAMES gcc libgcc libgcc.a
        PATHS ${LIB_PATH} )		

find_library(LIBGFORTRAN
        NAMES gfortran libgfortran libgfortran.a
        PATHS ${LIB_PATH} )	
		
set( EXTERNAL_LIBS ${LIBGCC} ${LIBGFORTRAN} )

link_libraries( ${EXTERNAL_LIBS} )
