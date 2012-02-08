del /Q /S /F tmp\*.*

set CFLAGS="-O2 -Wall -Wstrict-prototypes"
   f2py --build-dir .\tmp -m pytsproc ^
   -L..\win32\src ^
   --fcompiler=gnu95 ^
   --compiler=mingw32 -c pytsproc.pyf -ltsproc^
   -Ic:\MinGW32\include

::f2py tsp_main_loop.F90 -h pytsproc.pyf -m pytsproc

::f2py -c -m pyblock -Ld:\SMWData\Source_Code\tsproc\branches\experimental ^
::   -ltsproc.a --fcompiler=gnu95 py_block.F90 only: py_block:

::f2py --build-dir .\tmp -m pyblock ^
::   -LD:\SMWData\Source_Code\tsproc\branches\experimental -ltsproc --fcompiler=gnu95 ^
::   --compiler=mingw32 -c pyblock.pyf py_block.F90
::   tsp_data_structures.F90 tsp_utilities.F90 tsp_control_file_ops.F90

::f2py py_block.F90 -h pyblock.pyf -m pyblock
