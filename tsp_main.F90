program test

  use tsp_data_structures
  use tsp_main_loop
  implicit none

  character(len=256) :: sFilename
  character (len=256) :: sBlockname
  integer (kind=T_INT) :: iReturnCode

  character (len=32) :: sContext
  character (len=32) :: sDateFormat

  character (len=256) :: sInputFile
  character (len=256) :: sRecFile
  character (len=256) :: sDateStr
  character (len=256) :: sDateStrPretty

  logical :: lInteractive = lTRUE
  integer  :: iNumArgs

  iReturnCode = -1

  ! get number of command-line arguments
  iNumArgs = COMMAND_ARGUMENT_COUNT()

  if(iNumArgs==2) then

    lInteractive = lFALSE
    ! get actual values of the command-line arguments
    call GET_COMMAND_ARGUMENT(1,sInputFile)
    call GET_COMMAND_ARGUMENT(2,sRecFile)

  end if

   write(unit=LU_STD_OUT,fmt="(/,a,/)")  " TSPROC -- compiled on: "  // &
      TRIM(__DATE__) //" "// TRIM(__TIME__)

   write(unit=LU_STD_OUT,fmt="(a,/,a,/,a,/,a)") &
     ' Program TSPROC is a general time-series processor. It can ', &
     ' also be used for PEST input file preparation where time series data, ', &
     ' or processed time series data, comprises at least part of the observation ',&
     ' dataset.'

#ifdef __GFORTRAN__
    write(UNIT=*,FMT="(/,a,/)") " compiled with GNU gfortran version " &
      //TRIM(__VERSION__)
#endif

#ifdef __INTEL_COMPILER
    write(UNIT=*,FMT="(/,a,/)") " compiled with Intel Fortran version " &
      //TRIM(int2char(__INTEL_COMPILER))
#endif

#ifdef __G95__
    write(UNIT=*,FMT="(/,a,/)") " compiled with G95 minor version " &
      //TRIM(int2char(__G95_MINOR__))
#endif

  if(lInteractive) then
    write(6,FMT="('Enter name of TSPROC input file: ')",advance='no')
    read(5,'(a)') sFilename
  else
    sFilename=sInputFile
  end if

  if(lInteractive) then
    write(6,FMT="('Enter name of TSPROC record file: ')",advance='no')
    read(5,'(a)') sRecFile
  end if

  call main_loop(sFilename, sRecfile)

end program test
