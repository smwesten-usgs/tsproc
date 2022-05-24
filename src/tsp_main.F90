program tsp_main

    use tsp_data_structures
    use tsp_main_loop
    use version_control, only: GIT_COMMIT_HASH_STRING, GIT_BRANCH_STRING, COMPILE_DATE, COMPILE_TIME
    use ISO_FORTRAN_ENV
    implicit none

    character(len=256) :: sFilename
    integer(kind=T_INT) :: iReturnCode

    character(len=256) :: sInputFile
    character(len=256) :: sRecFile
    character(len=1024) :: sCompilerFlags
    character(len=256) :: sCompilerVersion

    ! default behavior is to query the user for the control file and record files
    logical :: lInteractive = lTRUE
    integer :: iNumArgs
    integer(kind=T_INT) :: ifail

    ! ensure all tables and series are inactive to start with
    gtable_g%active = lFALSE
    dtable_g%active = lFALSE
    stable_g%active = lFALSE
    ctable_g%active = lFALSE
    vtable_g%active = lFALSE
    series_g%active = lFALSE

    sContextOverride_g = ""
    iReturnCode = -1

    ! get number of command-line arguments
    iNumArgs = COMMAND_ARGUMENT_COUNT()

    if (iNumArgs == 2) then

        lInteractive = lFALSE
        ! get actual values of the command-line arguments
        call GET_COMMAND_ARGUMENT(1, sInputFile)
        call GET_COMMAND_ARGUMENT(2, sRecFile)

    elseif (iNumArgs == 3) then

        lInteractive = lFALSE
        ! get actual values of the command-line arguments
        call GET_COMMAND_ARGUMENT(1, sInputFile)
        call GET_COMMAND_ARGUMENT(2, sRecFile)
        call GET_COMMAND_ARGUMENT(3, sContextOverride_g)

    end if

    write (unit=LU_STD_OUT, fmt="(/,a,/,a,/,a,/,a)") &
        ' Program TSPROC is a general time-series processor. It can ', &
        ' also be used for PEST input file preparation where time series data, ', &
        ' or processed time series data, comprises at least part of the observation ', &
        ' dataset.'

    write (unit=LU_STD_OUT, fmt="(/,a)") " TSPROC -- "//TRIM(sVersionString)
    write (unit=LU_STD_OUT, fmt="(/,a)") "  Compiled on: "// &
        TRIM(COMPILE_DATE)//" "//TRIM(COMPILE_TIME)
    write (unit=LU_STD_OUT, fmt="(a,/)") "  Git commit hash: "//TRIM(GIT_COMMIT_HASH_STRING) &
        //" ("//TRIM(GIT_BRANCH_STRING)//" )"

#ifdef __GFORTRAN__
    sCompilerFlags = COMPILER_OPTIONS()
    sCompilerVersion = COMPILER_VERSION()
    write (UNIT=*, FMT="(a,/)") "Compiled with: gfortran ("//TRIM(sCompilerVersion)//")"
    write (UNIT=*, FMT="(a)") "Compiler flags:"
    write (UNIT=*, FMT="(a)") "-------------------------------"
    write (UNIT=*, FMT="(a,/)") TRIM(sCompilerFlags)
#endif

#ifdef __INTEL_COMPILER
    write (UNIT=*, FMT="(/,a,/)") "Compiled with Intel Fortran version " &
        //TRIM(int2char(__INTEL_COMPILER))
#endif

#ifdef __G95__
    write (UNIT=*, FMT="(/,a,/)") "Compiled with G95 minor version " &
        //TRIM(int2char(__G95_MINOR__))
#endif

    if (lInteractive) then
        write (6, FMT="('Enter name of TSPROC input file: ')", advance='no')
        read (5, '(a)') sFilename
    else
        sFilename = sInputFile
    end if

    if (lInteractive) then
        write (6, FMT="('Enter name of TSPROC record file: ')", advance='no')
        read (5, '(a)') sRecFile
    end if

    call openControlfile(sFilename, sRecfile)

    do

        call get_next_block(ifail)
        if (ifail /= 0) exit

        call processBlock()

    end do

!       call write_message(leadspace='yes')
    call close_files

!   do i=1,MAXSERIES
!     if(series_g(i)%active)then
!       deallocate(series_g(i)%days,series_g(i)%secs,series_g(i)%val,stat=ierr)
!       if(associated(series_g(i)%days)) nullify(series_g(i)%days)
!       if(associated(series_g(i)%secs)) nullify(series_g(i)%secs)
!       if(associated(series_g(i)%val))  nullify(series_g(i)%val)
!     end if
!   end do
!   if(tempseries_g%active)then
!     deallocate(tempseries_g%days,tempseries_g%secs,tempseries_g%val,stat=ierr)
!     if(associated(tempseries_g%days)) nullify(tempseries_g%days)
!     if(associated(tempseries_g%secs)) nullify(tempseries_g%secs)
!     if(associated(tempseries_g%val)) nullify(tempseries_g%val)
!   end if
!   do i=1,MAXVTABLE
!     if(vtable_g(i)%active)then
!       deallocate(vtable_g(i)%days1,vtable_g(i)%days2,vtable_g(i)%secs1,  &
!       vtable_g(i)%secs2,vtable_g(i)%vol,stat=ierr)
!       if(associated(vtable_g(i)%days1)) nullify(vtable_g(i)%days1)
!       if(associated(vtable_g(i)%days2)) nullify(vtable_g(i)%days2)
!       if(associated(vtable_g(i)%secs1)) nullify(vtable_g(i)%secs1)
!       if(associated(vtable_g(i)%secs2)) nullify(vtable_g(i)%secs2)
!       if(associated(vtable_g(i)%vol)) nullify(vtable_g(i)%vol)
!     end if
!   end do
!   do i=1,MAXDTABLE
!     if(dtable_g(i)%active)then
!       deallocate(dtable_g(i)%time,dtable_g(i)%flow,  &
!                  dtable_g(i)%tdelay,stat=ierr)
!       if(associated(dtable_g(i)%time))   nullify(dtable_g(i)%time)
!       if(associated(dtable_g(i)%flow))   nullify(dtable_g(i)%flow)
!       if(associated(dtable_g(i)%tdelay)) nullify(dtable_g(i)%tdelay)
!     end if
!   end do
!   deallocate(tempdtable_g%time,tempdtable_g%flow,tempdtable_g%tdelay,stat=ierr)
!   nullify (tempdtable_g%time,tempdtable_g%flow,tempdtable_g%tdelay)

end program tsp_main
