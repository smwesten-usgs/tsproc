
if(TARGET__TSPROC_EXECUTABLE)

  add_executable(tsproc

            tokenize.F90

            compute_hi.cpp
            tsp_command_processors.F90
            tsp_data_structures.F90
            tsp_equation_parser.F90
            tsp_input.F90
            tsp_main.F90
            tsp_main_loop.F90
            tsp_output.F90
            tsp_time_series_processors.F90
            tsp_utilities.F90
            wsc_additions.F90

            wdm_support/DTTM90.FOR
            wdm_support/TSBUFR.FOR
            wdm_support/UTCHAR.FOR
            wdm_support/UTCP90.FOR
            wdm_support/UTDATE.FOR
            wdm_support/UTNUMB.FOR
            wdm_support/UTWDMD.FOR
            wdm_support/UTWDMF.FOR
            wdm_support/UTWDT1.FOR
            wdm_support/WDATM1.FOR
            wdm_support/WDATM2.FOR
            wdm_support/WDATRB.FOR
            wdm_support/WDBTCH.FOR
            wdm_support/WDMESS.FOR
            wdm_support/WDMID.FOR
            wdm_support/WDOP.FOR
            wdm_support/WDTMS1.FOR
            wdm_support/WDTMS2.FOR )

   install(TARGETS tsproc
           RUNTIME
           DESTINATION bin
           )
endif()

if(TARGET__TSPROC_LIBRARY)

  add_library(tsproclib STATIC
            tokenize.F90

            compute_hi.cpp
            tsp_command_processors.F90
            tsp_data_structures.F90
            tsp_equation_parser.F90
            tsp_input.F90
            tsp_main.F90
            tsp_main_loop.F90
            tsp_output.F90
            tsp_time_series_processors.F90
            tsp_utilities.F90
            wsc_additions.F90

            wdm_support/DTTM90.FOR
            wdm_support/TSBUFR.FOR
            wdm_support/UTCHAR.FOR
            wdm_support/UTCP90.FOR
            wdm_support/UTDATE.FOR
            wdm_support/UTNUMB.FOR
            wdm_support/UTWDMD.FOR
            wdm_support/UTWDMF.FOR
            wdm_support/UTWDT1.FOR
            wdm_support/WDATM1.FOR
            wdm_support/WDATM2.FOR
            wdm_support/WDATRB.FOR
            wdm_support/WDBTCH.FOR
            wdm_support/WDMESS.FOR
            wdm_support/WDMID.FOR
            wdm_support/WDOP.FOR
            wdm_support/WDTMS1.FOR
            wdm_support/WDTMS2.FOR )

   install(TARGETS tsproclib
           ARCHIVE
           DESTINATION lib )

endif()

set_target_properties(tsproc tsproclib
    PROPERTIES LINKER_LANGUAGE CXX)
