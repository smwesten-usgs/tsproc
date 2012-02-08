:: cmake -G "MinGW Makefiles" -DCMAKE_MAKE_PROGRAM:FILEPATH=c:/MinGW32/bin/make.exe ^
:: -DCMAKE_C_COMPILER:FILEPATH=c:/MinGW32/bin/gcc.exe ^
:: -DCMAKE_FORTRAN_COMPILER:FILEPATH=c:/MinGW32/bin/gfortran.exe  ..\..
cmake -G "MinGW Makefiles" ..\..