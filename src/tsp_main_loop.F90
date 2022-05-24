module tsp_main_loop

! -- Program TSPROC is a general time - series processor. It can also be used for
!    PEST input file preparation.

! This if the main program block. Options are read here, and subroutines
    ! are called to carry out the desired time - series processing.

    use tsp_utilities
    use tsp_time_series_processors
    use tsp_command_processors
    use tsp_data_structures
    use tsp_equation_parser
    use tsp_input
    use tsp_output
    use wsc_additions

    implicit none

    integer(kind=T_INT) :: lastblock

contains

#ifdef UNROLL_CONTROL_FILE

    subroutine openControlfile(sFilename, sRecfile)

        use tokenize

        !f2py character( *), intent(in) :: sFilename
        !f2py character( *), intent(in) :: sRecFile
        character(len=*) :: sFilename
        character(len=*) :: sRecfile

        integer(kind=T_INT) :: ierr
        character(len=256) :: sDateStr, sDateStrPretty

        integer :: blocktype
        integer :: bpunit
        integer :: kline
        integer :: tokenlen
        type(tokenizer) :: token
        integer :: maxtokencnt
        character(len=40) :: wordone
        character(len=40) :: wordinnerone
        character(len=120) :: word
        character(len=120) :: lastword
        !-- TODO the allowable line length is referenced somewhere and should be used
        !here.
        character(len=400) :: nline
        character(len=400) :: xline
        character(len=400) :: wordinner
        integer :: wordcount
        integer :: wordloopcnt
        integer :: innerloopcnt
        integer :: tmpfnamecounter
        character(len=40) :: tmpfname
        character(len=40) :: lucfname
        logical :: exists

        tempdtable_g%active = lTRUE
        allocate (tempdtable_g%flow(MAXTEMPDURFLOW), &
                  tempdtable_g%time(MAXTEMPDURFLOW), &
                  tempdtable_g%tdelay(MAXTEMPDURFLOW), stat=ierr)

        call Assert(ierr == 0, &
                    "Cannot allocate sufficient memory to store temporary D_TABLE.", &
                    TRIM(__FILE__), __LINE__)

        sInfile_g = sFilename

        bpunit = nextunit()
        open (UNIT=bpunit, FILE=TRIM(ADJUSTL(sInfile_g)), STATUS="OLD", ACCESS='SEQUENTIAL', IOSTAT=ierr)
        call Assert(ierr == 0, "Could not open file '"//TRIM(ADJUSTL(sInfile_g))//"'")

        tmpunit = nextunit()
        open (unit=tmpunit, file="temporary."//TRIM(int2char(tmpunit)), status='REPLACE', &
              form='FORMATTED', access='SEQUENTIAL')
!       OPEN (UNIT = tmpunit, STATUS = 'SCRATCH', ACCESS = 'SEQUENTIAL')

        LU_TSPROC_CONTROL = nextunit()
!       OPEN (UNIT = LU_TSPROC_CONTROL, STATUS = 'SCRATCH', ACCESS = 'SEQUENTIAL')
! **64 - bit Windows version of gfortran tries to place the scratch
! file into the c: \Windows directory. On most machines this is
! locked down tight, and so the program crashes.
!> NOTE: NOT TRUE!! This indicates that the TEMP environment variable is
!> undefined. If undefined, it appears to default to c:\Windows

        open (unit=LU_TSPROC_CONTROL, file='tsproc_unrolled.inp', &
              status='REPLACE', form='FORMATTED')
        !      blocktype = 1 inside a block to NOT loop, 2 inside a block to loop
        blocktype = 0
        kline = 0
        do
            kline = kline + 1
            read (bpunit, '(A)', end=1200) cline

            ! -- Get rid of blank lines and comments
            if (LEN_TRIM(cline) == 0) then
                cycle
            end if
            if (cline(1:1) == '#') then
                cycle
            end if

            call set_tokenizer(token, ' ,', token_empty, token_quotes)
            wordone = first_token(token, cline, tokenlen)
            ! -- Identify the type of block - whether it can be unrolled or not
            if (TRIM(wordone) == 'START') then
                maxtokencnt = 0

      !! NOTE **It might be better to list the blocks in which
      !! unrolling is ALLOWED rather than the reverse

                word = next_token(token, cline, tokenlen)
                if ((TRIM(word) == 'SERIES_COMPARE') .OR. &
                    (TRIM(word) == 'SERIES_EQUATION') .OR. &
                    (TRIM(word) == 'SETTINGS') .OR. &
                    (TRIM(word) == 'HYDROLOGIC_INDICES') .OR. &
                    (TRIM(word) == 'FLOW_DURATION') .OR. &
                    (TRIM(word) == 'WRITE_PEST_FILES')) then

                    blocktype = 1

                else

                    blocktype = 2

                end if

            end if

            ! -- Handle the blocks that shouldn't be unrolled.
            if (blocktype == 1) then

                write (LU_TSPROC_CONTROL, '(A)') TRIM(cline)

            end if

            ! -- Blocks that can be unrolled.
            if (blocktype == 2) then

                ! -- Store contents of block unchanged into tmpunit scratch file,-
                ! -- Determine maximum number of tokens in the block (maxtokencnt)
                wordone = first_token(token, cline, tokenlen)
                nline = TRIM(wordone)
                wordcount = 1
                do
                    nline = TRIM(nline)//' '//TRIM(next_token(token, cline, tokenlen))
                    if (tokenlen == -1) then
                        exit
                    end if
                    wordcount = wordcount + 1
                end do

                write (tmpunit, '(A)') nline
                if (wordcount > maxtokencnt) then
                    maxtokencnt = wordcount
                end if

                ! -- The following error check only works with blocktype == 2, TODO move.
                if (((TRIM(wordone) == 'START') .OR. (TRIM(wordone) == 'END')) .AND. (wordcount /= 2)) then
                    write (*, *) 'Something is wrong with the START or END keywords'
                    write (*, *) 'at line: ', TRIM(cline)
                    write (*, *) 'at line number: ', kline
                    stop 1
                end if

                ! -- This is the END of a block to unroll.  Unroll from tmpunit and write to LU_TSPROC_CONTROL.
                if (TRIM(wordone) == 'END') then
                    do wordloopcnt = 2, maxtokencnt
                        rewind (tmpunit)
                        do
                            lastword = ' '
                            read (tmpunit, '(A)', end=1205) xline
                            wordinnerone = first_token(token, xline, tokenlen)
                            do innerloopcnt = 2, wordloopcnt
                                wordinner = TRIM(next_token(token, xline, tokenlen))

                                if (tokenlen == -1) then
                                    wordinner = lastword
                                    exit
                                else
                                    lastword = wordinner
                                end if

                            end do

                            if (wordinnerone == 'START') then
                                write (LU_TSPROC_CONTROL, '(A)') TRIM(wordinnerone)//' '//TRIM(wordinner)
                            elseif (wordinnerone == 'END') then
                                write (LU_TSPROC_CONTROL, '(A, / )') TRIM(wordinnerone)//' '//TRIM(wordinner)
                            else
                                write (LU_TSPROC_CONTROL, '(A)') '  '//TRIM(wordinnerone)//' '//TRIM(wordinner)
                            end if

                        end do
1205                    continue
                    end do
                    ! -- Have to close and reopen tmpunit to be ready for next block.
                    close (tmpunit)
                    open (unit=tmpunit, file="temporary.xxx", status='REPLACE', &
                          form='FORMATTED', access='SEQUENTIAL')
                    maxtokencnt = 0
                end if
            end if
        end do

1200    continue

        ! -- "Unrolled" tsproc control file is now in scratch file LU_TSPROC_CONTROL.
        ! -- REWIND to make ready for rest of PEST to read.
        rewind (LU_TSPROC_CONTROL)

        LU_REC = nextunit()
        open (unit=LU_REC, file=TRIM(ADJUSTL(sRecfile)), status='replace', iostat=ierr)
        call Assert(ierr == 0, "Could not open file '"//TRIM(ADJUSTL(sRecFile))//"'")

        ! -- Cleanup
        close (tmpunit, status='DELETE')
        close (bpunit, status='KEEP')

        ! -- More variables are initialised.

        imessage = 0
        NumProcBloc_g = 0
        ILine_g = 0
        IProcSetting_g = 0

        Context_g = ' '

        tempseries_g%nterm = 0
        call GetSysTimeDate(sDateStr, sDateStrPretty)
        call addquote(sInfile_g, sString_g)
        write (*, 110) TRIM(sDateStrPretty), TRIM(sString_g)
        write (LU_REC, 110) TRIM(sDateStrPretty), TRIM(sString_g)
110     format(/, a, ': processing information contained in TSPROC input file ', a, '....')

    end subroutine openControlfile

#else

    subroutine openControlfile(sFilename, sRecfile)

        !f2py character(*), intent(in) :: sFilename
        !f2py character(*), intent(in) :: sRecFile
        character(len=*) :: sFilename
        character(len=*) :: sRecfile

        integer(kind=T_INT) :: ierr
        character(len=256) :: sDateStr, sDateStrPretty

        tempdtable_g%active = lTRUE
        allocate (tempdtable_g%flow(MAXTEMPDURFLOW), &
                  tempdtable_g%time(MAXTEMPDURFLOW), &
                  tempdtable_g%tdelay(MAXTEMPDURFLOW), stat=ierr)

        call Assert(ierr == 0, &
                    "Cannot allocate sufficient memory to store temporary D_TABLE.", &
                    TRIM(__FILE__), __LINE__)

        LU_TSPROC_CONTROL = nextunit()

        sInfile_g = sFilename

        open (unit=LU_TSPROC_CONTROL, file=TRIM(ADJUSTL(sInfile_g)), status='old', iostat=ierr)
        call Assert(ierr == 0, "Could not open file '"//TRIM(ADJUSTL(sInfile_g))//"'")

        LU_REC = nextunit()

        open (unit=LU_REC, file=TRIM(ADJUSTL(sRecfile)), status='replace', iostat=ierr)
        call Assert(ierr == 0, "Could not open file '"//TRIM(ADJUSTL(sRecFile))//"'")

        ! -- More variables are initialised.

        imessage = 0
        NumProcBloc_g = 0
        ILine_g = 0
        IProcSetting_g = 0
        Context_g = ' '
        tempseries_g%nterm = 0
        call GetSysTimeDate(sDateStr, sDateStrPretty)
        call addquote(sInfile_g, sString_g)
        write (*, 110) TRIM(sDateStrPretty), TRIM(sString_g)
        write (LU_REC, 110) TRIM(sDateStrPretty), TRIM(sString_g)
110     format(/, a, ': processing information contained in TSPROC input file ', a, '....')

    end subroutine openControlfile

#endif

    !------------------------------------------------------------------------------

    subroutine closeControlfile()

        close (LU_TSPROC_CONTROL)

    end subroutine closeControlfile

    !------------------------------------------------------------------------------

    subroutine processBlock()

        integer(kind=T_INT) :: ifail

        ! settings
        if (iBlockNumber == iGET_SETTINGS) then
            call process_settings(ifail)

            ! get series from WDM file
        else if (iBlockNumber == iGET_WDM_SERIES) then
            call get_wdm_series(ifail)

            ! get series_g from site sample file
        else if (iBlockNumber == iGET_SSF_SERIES) then
            call get_ssf_series(ifail)

            ! get series_g from PLOTGEN file
        else if (iBlockNumber == iGET_PLT_SERIES) then
            call get_plt_series(ifail)

            ! get series_g from TETRAD output file
        else if (iBlockNumber == iGET_MUL_SERIES_TETRAD) then
            call get_mul_series_tetrad(ifail)

            ! get multiple series_g from site sample file
        else if (iBlockNumber == iGET_MUL_SERIES_SSF) then
            call get_mul_series_ssf(ifail)

            ! get series_g from UFORE-HYDRO file
        else if (iBlockNumber == iGET_UFORE_SERIES) then
            call get_ufore_series(ifail)

            ! get multiple series_g from a GSFLOW gage file
        else if (iBlockNumber == iGET_MUL_SERIES_GSFLOW_GAGE) then
            call get_mul_series_gsflow_gage(ifail)

            ! get multiple series_g from a MMS/GSFLOW STATVAR file
        else if (iBlockNumber == iGET_MUL_SERIES_STATVAR) then
            call get_mul_series_statvar(ifail)

            ! write list output file
        else if (iBlockNumber == iWRITE_LIST_OUTPUT) then
            call write_list_output(ifail)

            ! erase entity from memory
        else if (iBlockNumber == iERASE_ENTITY) then
            call erase_entity(ifail)

            ! reduce time_span of series_g
        else if (iBlockNumber == iREDUCE_SPAN) then
            call reduce_span(ifail)

            ! calculate series_g statistics
        else if (iBlockNumber == iSERIES_STATISTICS) then
            call statistics(ifail)

            ! series_g comparison statistics
        else if (iBlockNumber == iSERIES_COMPARE) then
            call compare_series(ifail)

            ! change time_base
        else if (iBlockNumber == iNEW_TIME_BASE) then
            call time_base(ifail)

            ! volume calculation
        else if (iBlockNumber == iVOLUME_CALCULATION) then
            call volume(ifail)

            ! exceedance time
        else if (iBlockNumber == iEXCEEDANCE_TIME) then
            call time_duration(ifail)

            ! flow duration
        else if (iBlockNumber == iFLOW_DURATION) then
            call flow_duration(ifail)

            ! series_g equation
        else if (iBlockNumber == iSERIES_EQUATION) then
            call equation(ifail)

            ! series_g displace
        else if (iBlockNumber == iSERIES_DISPLACE) then
            call displace(ifail)

            ! series_g clean
        else if (iBlockNumber == iSERIES_CLEAN) then
            call series_clean(ifail)

            ! digital filter
        else if (iBlockNumber == iDIGITAL_FILTER) then
            call bfilter(ifail)

            ! series_g base level
        else if (iBlockNumber == iSERIES_BASE_LEVEL) then
            call series_base_level(ifail)

            ! volume to series_g
        else if (iBlockNumber == iVOL_TABLE_TO_SERIES) then
            call vol_to_series(ifail)

            ! moving minimum
        else if (iBlockNumber == iMOVING_MINIMUM) then
            call moving_window(ifail)

            ! new uniform series_g
        else if (iBlockNumber == iNEW_SERIES_UNIFORM) then
            call new_series_uniform(ifail)

            ! series_g difference
        else if (iBlockNumber == iSERIES_DIFFERENCE) then
            call series_difference(ifail)

            ! period statistics - monthly & annual stats calculations
        else if (iBlockNumber == iPERIOD_STATISTICS) then
            call period_stats(ifail)

            ! hydro_peaks - find and compare peak values within a time series_g
        else if (iBlockNumber == iHYDRO_PEAKS) then
            call hydro_peaks(ifail)

            ! usgs_hysep - run USGS HYSEP routines on time series_g values
        else if (iBlockNumber == iUSGS_HYSEP) then
            call usgs_hysep(ifail)

            ! hydro_events - extract time series for a window of time surrounding a peak
        else if (iBlockNumber == iHYDRO_EVENTS) then
            call hydro_events(ifail)

            ! hydrologic_indices
        else if (iBlockNumber == iHYDROLOGIC_INDICES) then
            call compute_hydrologic_indices(ifail)

            ! write pest files
        else if (iBlockNumber == iWRITE_PEST_FILES) then
            call pest_files(ifail, lastblock)

        end if

        if (ifail /= 0) then
            call close_files
            call Assert(lFALSE, "Problem processing TSPROC block")
        end if

        lastblock = iBlockNumber

    end subroutine processBlock

    !------------------------------------------------------------------------------

    subroutine getNextBlock(sBlockName)

        !f2py character*30, intent(out) :: sBlockName
        character(len=30), intent(out) :: sBlockName

        ! [ LOCALS ]
        integer(kind=T_INT) :: ifail

        call get_next_block(ifail)
        sBlockName = sCurrentBlockName

    end subroutine getNextBlock

    !------------------------------------------------------------------------------

end module tsp_main_loop
