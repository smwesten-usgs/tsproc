module tsp_main_loop

! -- Program TSPROC is a general time-series processor. It can also be used for
!    PEST input file preparation.

! This if the main program block. Options are read here, and subroutines
! are called to carry out the desired time-series processing.


   use tsp_utilities
   use tsp_time_series_processors
   use tsp_command_processors
   use tsp_data_structures
   use tsp_equation_parser
   use tsp_input
   use tsp_output
   use wsc_additions

   implicit none

contains

subroutine main_loop(sFilename, sRecfile)

  !f2py character(*), intent(in) :: sFilename
  !f2py character(*), intent(in) :: sRecFile
  character (len=*) :: sFilename
  character (len=*) :: sRecfile

  integer (kind=T_INT) :: i
  integer (kind=T_INT) :: iblock, ierr , ifail, lastblock
  character (len=256) :: sDateStr, sDateStrPretty



! -- Some variables are initialised

       series_g%active=lFALSE        !series_g is an array
       stable_g%active=lFALSE        !stable_g is an array
       vtable_g%active=lFALSE        !vtable_g is an array
       dtable_g%active=lFALSE        !dtable_g is an array
       ctable_g%active=lFALSE        !ctable_g is an array
       tempseries_g%active=lFALSE

       tempdtable_g%active=lTRUE
       allocate(tempdtable_g%flow(MAXTEMPDURFLOW),   &
                tempdtable_g%time(MAXTEMPDURFLOW),   &
                tempdtable_g%tdelay(MAXTEMPDURFLOW),stat=ierr)

       call Assert(ierr==0, &
         "Cannot allocate sufficient memory to store temporary E_TABLE.", &
         TRIM(__FILE__),__LINE__)

       LU_TSPROC_CONTROL=nextunit()

       sInfile_g = sFilename

       open(unit=LU_TSPROC_CONTROL,file=TRIM(ADJUSTL(sInfile_g)),status='old',iostat=ierr)
       call Assert(ierr==0,"Could not open file '"//TRIM(ADJUSTL(sInfile_g))//"'")

       LU_REC=nextunit()

       open(unit=LU_REC,file=TRIM(ADJUSTL(sRecfile)),status='replace',iostat=ierr)
       call Assert(ierr==0,"Could not open file '"//TRIM(ADJUSTL(sRecFile))//"'")

! -- More variables are initialised.

       imessage=0
       NumProcBloc_g=0
       ILine_g=0
       IProcSetting_g=0
       Context_g=' '
       tempseries_g%nterm=0
       call GetSysTimeDate(sDateStr,sDateStrPretty)
       call addquote(sInfile_g,sString_g)
       write(*,110) trim(sDateStrPretty),trim(sString_g)
       write(LU_REC,110) trim(sDateStrPretty),trim(sString_g)
110    format(/,a,': processing information contained in TSPROC input file ',a,'....')


! -- The TSPROC input file is now read, looking for Blocks.

!120    continue

       do

         call get_next_block(ifail,iblock)
!         if(ifail.ne.0) go to 9900
         if(ifail /= 0) exit

         ! settings
         if(iblock == iGET_SETTINGS) then
           call process_settings(ifail)

        ! get series from WDM file
       else if(iblock == iGET_WDM_SERIES) then
!         call get_wdm_series(ifail)

         ! get series_g from site sample file
         else if(iblock == iGET_SSF_SERIES) then
           call get_ssf_series(ifail)

         ! get series_g from PLOTGEN file
         else if(iblock == iGET_PLT_SERIES) then
           call get_plt_series(ifail)

         ! get series_g from TETRAD output file
         else if(iblock == iGET_MUL_SERIES_TETRAD) then
           call get_mul_series_tetrad(ifail)

         ! get multiple series_g from site sample file
         else if(iblock == iGET_MUL_SERIES_SSF) then
           call get_mul_series_ssf(ifail)

         ! get series_g from UFORE-HYDRO file
         else if(iblock == iGET_UFORE_SERIES) then
           call get_ufore_series(ifail)

         ! get multiple series_g from a GSFLOW gage file
         else if(iblock == iGET_MUL_SERIES_GSFLOW_GAGE) then
           call get_mul_series_gsflow_gage(ifail)

         ! get multiple series_g from a MMS/GSFLOW STATVAR file
         else if(iblock == iGET_MUL_SERIES_STATVAR) then
           call get_mul_series_statvar(ifail)

         ! write list output file
         else if(iblock == iWRITE_LIST_OUTPUT) then
           call write_list_output(ifail)

         ! erase entity from memory
         else if(iblock == iERASE_ENTITY) then
           call erase_entity(ifail)

         ! reduce time_span of series_g
         else if(iblock == iREDUCE_SPAN) then
           call reduce_span(ifail)

         ! calculate series_g statistics
         else if(iblock == iSERIES_STATISTICS) then
           call statistics(ifail)

         ! series_g comparison statistics
         else if(iblock == iSERIES_COMPARE) then
           call compare_series(ifail)

         ! change time_base
         else if(iblock == iNEW_TIME_BASE) then
           call time_base(ifail)

         ! volume calculation
         else if(iblock == iVOLUME_CALCULATION) then
           call volume(ifail)

         ! exceedence time
         else if(iblock == iEXCEEDANCE_TIME) then
           call time_duration(ifail)

         ! series_g equation
         else if(iblock == iSERIES_EQUATION) then
           call equation(ifail)

         ! series_g displace
         else if(iblock == iSERIES_DISPLACE) then
           call displace(ifail)

         ! series_g clean
         else if(iblock == iSERIES_CLEAN) then
           call series_clean(ifail)

         ! digital filter
         else if(iblock == iDIGITAL_FILTER) then
           call bfilter(ifail)

         ! series_g base level
         else if(iblock == iSERIES_BASE_LEVEL) then
           call series_base_level(ifail)

         ! volume to series_g
         else if(iblock == iVOL_TABLE_TO_SERIES) then
           call vol_to_series(ifail)

         ! moving minimum
         else if(iblock == iMOVING_MINIMUM) then
           call moving_window(ifail)

         ! new uniform series_g
         else if(iblock == iNEW_SERIES_UNIFORM) then
           call new_series_uniform(ifail)

         ! series_g difference
         else if(iblock == iSERIES_DIFFERENCE) then
           call series_difference(ifail)

         ! period statistics - monthly & annual stats calculations
         else if(iblock == iPERIOD_STATISTICS) then
           call period_stats(ifail)

         ! hydro_peaks - find and compare peak values within a time series_g
         else if(iblock == iHYDRO_PEAKS) then
           call hydro_peaks(ifail)

         ! usgs_hysep - run USGS HYSEP routines on time series_g values
         else if(iblock == iUSGS_HYSEP) then
           call usgs_hysep(ifail)

         ! hydro_events - extract time series for a window of time surrounding a peak
         else if(iblock == iHYDRO_EVENTS) then
           call hydro_events(ifail)

         ! hydrologic_indices
         else if(iblock == iHYDROLOGIC_INDICES) then
!           call calc_hydrologic_indices(ifail)

         ! write pest files
         else if(iblock == iWRITE_PEST_FILES) then
           call pest_files(ifail,lastblock)

         end if
         if(ifail.ne.0) then
!         go to 9900
           call close_files
           call Assert(lFALSE,"Problem processing TSPROC block", &
             TRIM(__FILE__),__LINE__)
         end if
         lastblock=iblock

       end do
!       go to 120

!       call write_message(leadspace='yes')
       call close_files

       do i=1,MAXSERIES
         if(series_g(i)%active)then
           deallocate(series_g(i)%days,series_g(i)%secs,series_g(i)%val,stat=ierr)
           if(associated(series_g(i)%days)) nullify(series_g(i)%days)
           if(associated(series_g(i)%secs)) nullify(series_g(i)%secs)
           if(associated(series_g(i)%val))  nullify(series_g(i)%val)
         end if
       end do
       if(tempseries_g%active)then
         deallocate(tempseries_g%days,tempseries_g%secs,tempseries_g%val,stat=ierr)
         if(associated(tempseries_g%days)) nullify(tempseries_g%days)
         if(associated(tempseries_g%secs)) nullify(tempseries_g%secs)
         if(associated(tempseries_g%val)) nullify(tempseries_g%val)
       end if
       do i=1,MAXVTABLE
         if(vtable_g(i)%active)then
           deallocate(vtable_g(i)%days1,vtable_g(i)%days2,vtable_g(i)%secs1,  &
           vtable_g(i)%secs2,vtable_g(i)%vol,stat=ierr)
           if(associated(vtable_g(i)%days1)) nullify(vtable_g(i)%days1)
           if(associated(vtable_g(i)%days2)) nullify(vtable_g(i)%days2)
           if(associated(vtable_g(i)%secs1)) nullify(vtable_g(i)%secs1)
           if(associated(vtable_g(i)%secs2)) nullify(vtable_g(i)%secs2)
           if(associated(vtable_g(i)%vol)) nullify(vtable_g(i)%vol)
         end if
       end do
       do i=1,MAXDTABLE
         if(dtable_g(i)%active)then
           deallocate(dtable_g(i)%time,dtable_g(i)%flow,  &
                      dtable_g(i)%tdelay,stat=ierr)
           if(associated(dtable_g(i)%time))   nullify(dtable_g(i)%time)
           if(associated(dtable_g(i)%flow))   nullify(dtable_g(i)%flow)
           if(associated(dtable_g(i)%tdelay)) nullify(dtable_g(i)%tdelay)
         end if
       end do
       deallocate(tempdtable_g%time,tempdtable_g%flow,tempdtable_g%tdelay,stat=ierr)
       nullify (tempdtable_g%time,tempdtable_g%flow,tempdtable_g%tdelay)

end subroutine main_loop


end module tsp_main_loop
