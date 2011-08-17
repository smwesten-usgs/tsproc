
if(TARGET__TSPROC_EXECUTABLE)

  add_executable(tsproc
            compute_hi.c
            tsp_command_processors.F90
            tsp_data_structures.F90
            tsp_equation_parser.F90
            tsp_hydrologic_indices.F90
            tsp_input.F90
            tsp_main.F90
            tsp_main_loop.F90
            tsp_output.F90
            tsp_time_series_processors.F90
            tsp_utilities.F90
            wsc_additions.F90 )

endif()

if(TARGET__TSPROC_LIBRARY)

  add_library(tsproclib STATIC
            compute_hi.c
            tsp_command_processors.F90
            tsp_data_structures.F90
            tsp_equation_parser.F90
            tsp_hydrologic_indices.F90
            tsp_input.F90
            tsp_main.F90
            tsp_main_loop.F90
            tsp_output.F90
            tsp_time_series_processors.F90
            tsp_utilities.F90
            wsc_additions.F90 )

endif()
