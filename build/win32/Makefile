#
# Makefile for compiling under MSYS/gfortran on Windows-32 platform
#
#FFLAGS	= -O2 -x f95-cpp-input -fopenmp
#FFLAGS	= -O2 -cpp
#FFLAGS	= -O2 -cpp -fbounds-check
FFLAGS	= -O3 -cpp -ffree-line-length-none -fwhole-file -fcheck=all -fbounds-check

F95		= gfortran
GCC     = gcc
CXX     = g++
CFLAGS  = -O2
AR		= ar rcs
SRC     = ../../src

LIB		= -lgfortran -lgcc -lstdc++

# -luser32 -lgdi32

all:	main lib

main:	tsp_utilities.o tsp_data_structures.o tsp_equation_parser.o tsp_input.o \
		tsp_output.o tsp_time_series_processors.o tsp_main.o wsc_additions.o \
		tsp_command_processors.o tsp_main_loop.o compute_hi.o
		$(F95) -o tsproc \
		tsp_utilities.o tsp_data_structures.o tsp_equation_parser.o tsp_input.o \
		tsp_output.o tsp_time_series_processors.o tsp_main.o wsc_additions.o \
		tsp_command_processors.o tsp_main_loop.o compute_hi.o \
		$(LIBPATH) $(LIB) $(FFLAGS)

lib:	tsp_utilities.o tsp_data_structures.o tsp_equation_parser.o tsp_input.o \
		tsp_output.o tsp_time_series_processors.o wsc_additions.o \
		tsp_command_processors.o tsp_main_loop.o
		$(AR) libtsproc.a \
		tsp_utilities.o tsp_data_structures.o tsp_equation_parser.o tsp_input.o \
		tsp_output.o tsp_time_series_processors.o wsc_additions.o \
		tsp_command_processors.o tsp_main_loop.o

compute_hi.o: $(SRC)/compute_hi.c
		$(CXX) $(CFLAGS) -c $(SRC)/compute_hi.c -o compute_hi.o

tsp_data_structures.o: $(SRC)/tsp_data_structures.F90
		$(F95) $(FFLAGS) -c $(SRC)/tsp_data_structures.F90 -o tsp_data_structures.o

tsp_utilities.o: $(SRC)/tsp_utilities.F90 tsp_data_structures.o
		$(F95) $(FFLAGS) -c $(SRC)/tsp_utilities.F90 -o tsp_utilities.o

tsp_time_series_processors.o: $(SRC)/tsp_time_series_processors.F90  tsp_data_structures.o tsp_utilities.o
		$(F95) $(FFLAGS) -c $(SRC)/tsp_time_series_processors.F90 -o tsp_time_series_processors.o

tsp_main.o: $(SRC)/tsp_main.F90  tsp_data_structures.o tsp_utilities.o tsp_time_series_processors.o \
		wsc_additions.o tsp_command_processors.o tsp_main_loop.o
		$(F95) $(FFLAGS) -c $(SRC)/tsp_main.F90 -o tsp_main.o

tsp_equation_parser.o: $(SRC)/tsp_equation_parser.F90  tsp_data_structures.o tsp_utilities.o \
		tsp_command_processors.o
		$(F95) $(FFLAGS) -c $(SRC)/tsp_equation_parser.F90 -o tsp_equation_parser.o

tsp_input.o: $(SRC)/tsp_input.F90  tsp_data_structures.o tsp_utilities.o
		$(F95) $(FFLAGS) -c $(SRC)/tsp_input.F90 -o tsp_input.o

tsp_output.o: $(SRC)/tsp_output.F90  tsp_data_structures.o tsp_utilities.o
		$(F95) $(FFLAGS) -c $(SRC)/tsp_output.F90 -o tsp_output.o

tsp_command_processors.o: $(SRC)/tsp_command_processors.F90  tsp_data_structures.o tsp_utilities.o
		$(F95) $(FFLAGS) -c $(SRC)/tsp_command_processors.F90 -o tsp_command_processors.o

wsc_additions.o: $(SRC)/wsc_additions.F90  tsp_data_structures.o tsp_utilities.o tsp_time_series_processors.o
		$(F95) $(FFLAGS) -c $(SRC)/wsc_additions.F90 -o wsc_additions.o

tsp_main_loop.o: $(SRC)/tsp_main_loop.F90  tsp_data_structures.o tsp_utilities.o \
		tsp_time_series_processors.o tsp_equation_parser.o tsp_input.o tsp_output.o \
		wsc_additions.o
		$(F95) $(FFLAGS) -c $(SRC)/tsp_main_loop.F90 -o tsp_main_loop.o

clean:
		rm -f *.o
		rm -f tsproc.exe
		rm -f tsproc
		rm -f *.mod

copy_lin:
		sudo chmod 755 tsproc
		sudo cp tsproc.exe /usr/local/bin/tsproc

copy:
		cp tsproc.exe e:/DOS/tsproc.exe
		cp tsproc.exe d:/DOS/tsproc.exe
