cmake -G "MinGW Makefiles" -DCMAKE_MAKE_PROGRAM:FILEPATH=c:\mingw32\bin\make.exe ^
  -DCMAKE_C_COMPILER:FILEPATH=c:\mingw32\bin\gcc.exe ^
  -DCMAKE_FORTRAN_COMPILER:FILEPATH=c:\mingw32\bin\gfortran.exe  ..\..
