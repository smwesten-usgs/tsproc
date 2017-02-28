    # set the path and filename for the tsproc executable
# i.e. if one types 'make copy', where does the resulting executable end up, and what is its name?
if (  CMAKE_HOST_WIN32 )
  set ( TSPROC_EXECUTABLE "tsproc.exe" )
else ()
  set ( TSPROC_EXECUTABLE "tsproc" )
endif()
