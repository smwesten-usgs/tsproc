g++ -Ic:\gfortran_32\include -c compute_hi.c
gfortran -c test_compute_hi.F90
g++ compute_hi.o test_compute_hi.o -lgcc -lstdc++ -lgfortran -o test_hi.exe
