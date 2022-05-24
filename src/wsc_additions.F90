module wsc_additions

    use tsp_data_structures
    use tsp_utilities
    use tsp_command_processors
    implicit none

contains

    subroutine hydro_events(ifail)

! -- Subroutine hydro_events finds events for a hydrograph time series.

        implicit none

        integer, intent(out) :: ifail

        integer dd1, mm1, yy1, hh1, nn1, ss1, dd2, mm2, yy2, hh2, nn2, ss2, ierr, &
            icontext, i, begdays, begsecs, enddays, endsecs, iseries, &
            j, ibterm, ieterm, iterm, iiterm, itemp, ixcon, &
            ndays, nsecs, npts, nmax, tlast, vlast, &
            DA(12), INDEX(10000), npeak
        real twindow, rtemp, tmin, slope1, slope2, rise_lag, fall_lag
        real, dimension(:), allocatable :: tdate, tval !rank 1
        character(10) aname
        character(15) aline
        character(25) aoption
        character(25) acontext(MAXCONTEXT)
        data DA/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/

        ifail = 0
        CurrentBlock_g = 'HYDRO_EVENTS'

        write (*, 10) TRIM(CurrentBlock_g)
        write (LU_REC, 10) TRIM(CurrentBlock_g)
10      format(/, ' Processing ', a, ' block....')

        icontext = 0
        iseries = 0
        yy1 = -9999
        hh1 = -9999
        yy2 = -9999
        hh2 = -9999
        twindow = 1
        tmin = 0
        aname = ' '
        ixcon = 0

! -- The HYDRO_EVENTS block is first parsed.

        do
            ILine_g = ILine_g + 1
            read (LU_TSPROC_CONTROL, '(a)', err=9000, end=9100) cline
            if (cline == ' ') cycle
            if (cline(1:1) == '#') cycle
            call linesplit(ierr, 2)
            if (ierr /= 0) then
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 20) TRIM(aline), TRIM(sString_g)
20              format('there should be 2 entries on line ', a, ' of file ', a)
                go to 9800
            end if
            aoption = cline(left_word(1):right_word(1))
            call casetrans(aoption, 'hi')
            if (aoption /= 'CONTEXT') then
                call test_context(ierr, icontext, acontext)
                if (ierr == -1) then
                    call find_end(ifail)
                    if (ifail == 1) go to 9800
                    return
                else if (ierr == 1) then
                    go to 9800
                end if
                ixcon = 1
            end if
            if (aoption == 'DATE_1') then
                call get_date(ierr, dd1, mm1, yy1, 'DATE_1')
                if (ierr /= 0) go to 9800
            else if (aoption == 'DATE_2') then
                call get_date(ierr, dd2, mm2, yy2, 'DATE_2')
                if (ierr /= 0) go to 9800
            else if (aoption == 'TIME_1') then
                call get_time(ierr, hh1, nn1, ss1, 'TIME_1')
                if (ierr /= 0) go to 9800
            else if (aoption == 'TIME_2') then
                call get_time(ierr, hh2, nn2, ss2, 'TIME_2')
                if (ierr /= 0) go to 9800
            else if (aoption == 'NEW_SERIES_NAME') then
                call get_new_series_name(ierr, aname)
                if (ierr /= 0) go to 9800
            else if (aoption == 'SERIES_NAME') then
                call get_series_name(ierr, iseries, 'SERIES_NAME')
                if (ierr /= 0) go to 9800
            else if (aoption == 'CONTEXT') then
                if (ixcon /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 41) TRIM(aline), TRIM(sString_g)
41                  format('CONTEXT keyword in incorrect location at line ', a, ' of file ', a)
                    go to 9800
                end if
                call get_context(ierr, icontext, acontext)
                if (ierr /= 0) go to 9800
            else if (aoption == 'WINDOW') then
                call get_keyword_value(ierr, 2, itemp, twindow, 'WINDOW')
                if (ierr /= 0) go to 9800
                if (twindow <= 0.0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 42) TRIM(aline), TRIM(sString_g)
42                  format('WINDOW must be greater than zero at line ', a, ' of file ', a)
                    go to 9800
                end if
            else if (aoption == 'MIN_PEAK') then
                call get_keyword_value(ierr, 2, itemp, tmin, 'MIN_PEAK')
                if (ierr /= 0) go to 9800
                if (tMIN < 0.0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 43) TRIM(aline), TRIM(sString_g)
43                  format('MIN_PEAK must not zero at line ', a, ' of file ', a)
                    go to 9800
                end if
            else if (aoption == 'RISE_LAG') then
                call get_keyword_value(ierr, 2, itemp, rise_lag, 'RISE_LAG')
                if (ierr /= 0) go to 9800
                if (rise_lag <= 0.0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 44) TRIM(aline), TRIM(sString_g)
44                  format('RISE_LAG must be greater than zero at line ', a, ' of file ', a)
                    go to 9800
                end if
            else if (aoption == 'FALL_LAG') then
                call get_keyword_value(ierr, 2, itemp, fall_lag, 'FALL_LAG')
                if (ierr /= 0) go to 9800
                if (fall_lag <= 0.0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 45) TRIM(aline), TRIM(sString_g)
45                  format('FALL_LAG must be greater than zero at line ', a, ' of file ', a)
                    go to 9800
                end if
            else if (aoption == 'END') then
                go to 200
            else
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 90) TRIM(aoption), TRIM(CurrentBlock_g), TRIM(aline), TRIM(sString_g)
90              format('unexpected keyword - "', a, '" in ', a, ' block at line ', a, &
                       ' of file ', a)
                go to 9800
            end if
        end do

! -- The block has been read; now it is checked for correctness.

200     continue
        if (iseries == 0) then
            write (amessage, 210) TRIM(CurrentBlock_g)
210         format('no SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (aname == ' ') then
            write (amessage, 230) TRIM(CurrentBlock_g)
230         format('no NEW_SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (icontext == 0) then
            write (amessage, 220) TRIM(CurrentBlock_g)
220         format('no CONTEXT keyword(s) provided in ', a, ' block.')
            go to 9800
        end if
        call date_check(ierr, yy1, mm1, dd1, hh1, nn1, ss1, yy2, mm2, dd2, hh2, nn2, ss2, &
                        begdays, begsecs, enddays, endsecs)
        if (ierr /= 0) go to 9800
        call beg_end_check(ierr, iseries, begdays, begsecs, enddays, endsecs)
        if (ierr /= 0) go to 9800

! -- All is well with the block. The requested events are found.

        call numterms(iterm, ibterm, ieterm, begdays, begsecs, enddays, endsecs, iseries)
        if (iterm == 0) then
            write (amessage, 270) TRIM(series_g(iseries)%name)
270         format('there are no terms in time series_g "', a, '" between the provided ', &
                   'dates and times.')
            go to 9800
        end if

        if ((begdays < series_g(iseries)%days(1)) .OR. &
            ((begdays == series_g(iseries)%days(1)) .AND. &
             (begsecs < series_g(iseries)%secs(1)))) then
            begdays = series_g(iseries)%days(1)
            begsecs = series_g(iseries)%secs(1)
        end if
        iiterm = series_g(iseries)%nterm
        if ((enddays > series_g(iseries)%days(iiterm)) .OR. &
            ((enddays == series_g(iseries)%days(iiterm)) .AND. &
             (endsecs > series_g(iseries)%secs(iiterm)))) then
            enddays = series_g(iseries)%days(iiterm)
            endsecs = series_g(iseries)%secs(iiterm)
        end if

        nmax = ieterm - ibterm + 1
        allocate (tdate(nmax), tval(nmax), stat=ierr)
        if (ierr /= 0) then
            write (amessage, 280)
280         format('cannot allocate temporary time series')
            goto 9800
        end if
        npts = 0
        do j = ibterm, ieterm
            ndays = series_g(iseries)%days(j)
            nsecs = series_g(iseries)%secs(j)
            rtemp = series_g(iseries)%val(j)
            npts = npts + 1
            tdate(npts) = ndays + nsecs / 86400
            tval(npts) = rtemp
        end do
        call alloc_tempseries(ierr, iiterm)
        if (ierr /= 0) then
            write (amessage, 280)
            goto 9800
        end if
        iterm = 0
        tlast = tdate(1)
        vlast = tval(1)
        npeak = 0
        do j = 2, npts - 2
            slope1 = (tval(j) - tval(j - 1)) / (tdate(j) - tdate(j - 1))
            slope2 = (tval(j + 1) - tval(j)) / (tdate(j + 1) - tdate(j))
!         print *, j, slope1, slope2, tdate(j), tlast, twindow, tval(j), tmin, vlast
            if (slope1 > 0.0) then
                if (slope2 <= 0.0) then
                    if ((tdate(j) - tlast) > twindow) then
                        if (tval(j) > tmin) then
                            npeak = npeak + 1
                            INDEX(npeak) = j
                            tlast = tdate(j)
                            vlast = tval(j)
                        end if
                    else
                        if (tval(j) > vlast) then
                            if (npeak < 1) npeak = 1
                            INDEX(npeak) = j
                            tlast = tdate(j)
                            vlast = tval(j)
                        end if
                    end if
                end if
            end if
        end do
        do i = 1, npeak
            call get_event(tdate, tval, npts, INDEX(i), rise_lag, fall_lag, iterm)
        end do
! --  space is allocated for the new time series.

        do i = 1, MAXSERIES
            if (.NOT. series_g(i)%active) go to 380
        end do
        write (amessage, 370)
370     format('no more time series available for data storage - increase MAXSERIES and ', &
               'recompile program.')
        go to 9800

380     allocate (series_g(i)%days(iterm), series_g(i)%secs(iterm), &
                  series_g(i)%val(iterm), stat=ierr)
        if (ierr /= 0) then
            write (amessage, 390)
390         format('cannot allocate memory for another time series.')
            go to 9800
        end if

        series_g(i)%active = .TRUE.
        series_g(i)%name = aname
        series_g(i)%nterm = iterm
        series_g(i)%type = 'ts'

        do j = 1, iterm
            series_g(i)%days(j) = tempseries_g%days(j)
            series_g(i)%secs(j) = tempseries_g%secs(j)
            series_g(i)%val(j) = tempseries_g%val(j)
        end do

        write (6, 400) TRIM(series_g(iseries)%name), TRIM(aname)
        write (LU_REC, 400) TRIM(series_g(iseries)%name), TRIM(aname)
400     format(t5, 'Hydrograph events for time series "', a, '" stored in ', &
               'time series "', a, '".')
        return

9000    call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9010) TRIM(aline), TRIM(sString_g)
9010    format('cannot read line ', a, ' of TSPROC input file ', a)
        go to 9800
9100    continue
        call addquote(sInfile_g, sString_g)
        write (amessage, 9110) TRIM(sString_g), TRIM(CurrentBlock_g)
9110    format('unexpected end encountered to TSPROC input file ', a, ' while ', &
               ' reading ', a, ' block.')
        go to 9800

9800    call write_message(leadspace='yes', error='yes')
        call write_message(iunit=LU_REC, leadspace='yes')
        ifail = 1

        return

    end subroutine hydro_events

    subroutine get_event(tdate, tval, npts, index, rise_lag, fall_lag, iterm)

! -- Subroutine get_event finds events for a specified peak.

        implicit none

        integer j, j1, j2, iterm, &
            ndays, nsecs, npts, index
        real rise_lag, fall_lag, tbeg, tend
        real tdate(*), tval(*)

        tbeg = tdate(index) - rise_lag
        if (tbeg < tdate(1)) tbeg = tdate(1)
        tend = tdate(index) + fall_lag
        if (tend > tdate(npts)) tend = tdate(npts)
        j1 = index
        j2 = index
        do j = 1, npts - 1
            if (tdate(j) <= tbeg .AND. tdate(j + 1) > tbeg) then
                j1 = j
                exit
            end if
        end do
        do j = 1, npts - 1
            if (tdate(j) <= tend .AND. tdate(j + 1) > tend) then
                j2 = j
                exit
            end if
        end do
        do j = j1, j2
            iterm = iterm + 1
            ndays = INT(tdate(j))
            nsecs = 86400 * (tdate(j) - ndays)
            tempseries_g%days(iterm) = ndays
            tempseries_g%secs(iterm) = nsecs
            tempseries_g%val(iterm) = tval(j)
        end do

    end subroutine get_event

! -- Subroutine period_stats calculates period statistics for a time series.
    subroutine period_stats(ifail)
        implicit none

        integer, intent(out) :: ifail

        integer dd1, mm1, yy1, hh1, nn1, ss1, dd2, mm2, yy2, hh2, nn2, ss2, ierr, &
            icontext, i, begdays, begsecs, enddays, endsecs, iseries, jtrans, &
            j, ibterm, ieterm, iterm, iiterm, itemp, ixcon, ndays, nvals, days, &
            dd, mm, yy, cdd, cmm, cyy, cwy, wy, &
            DA(12), nvals_mon(12), nsecs
        real tpower, rtemp, rvalue, tstat, tmax, tmin, tsum, tsum2, rmean
        real tmax_mon(12), tmin_mon(12), tsum_mon(12), tsum2_mon(12)
        character(3) aaa
        character(len=iTSNAMELENGTH) :: aname, abscissa, statistic, period, year_type
        character(15) aline
        character(25) aoption
        character(25) acontext(MAXCONTEXT)
        data DA/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/

        ifail = 0
        CurrentBlock_g = 'PERIOD_STATISTICS'

        write (*, 10) TRIM(CurrentBlock_g)
        write (LU_REC, 10) TRIM(CurrentBlock_g)
10      format(/, ' Processing ', a, ' block....')

        icontext = 0
        iseries = 0
        yy1 = -9999
        hh1 = -9999
        yy2 = -9999
        hh2 = -9999
        year_type = 'water_high'
        period = ' '
        statistic = ' '
        abscissa = ' '
        aname = ' '
        jtrans = 0
        tpower = -1.0E35
        ixcon = 0

! -- The SERIES_STATISTICS block is first parsed.

        do
            ILine_g = ILine_g + 1
            read (LU_TSPROC_CONTROL, '(a)', err=9000, end=9100) cline
            if (cline == ' ') cycle
            if (cline(1:1) == '#') cycle
            call linesplit(ierr, 2)
            if (ierr /= 0) then
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 20) TRIM(aline), TRIM(sString_g)
20              format('there should be 2 entries on line ', a, ' of file ', a)
                go to 9800
            end if
            aoption = cline(left_word(1):right_word(1))
            call casetrans(aoption, 'hi')
            if (aoption /= 'CONTEXT') then
                call test_context(ierr, icontext, acontext)
                if (ierr == -1) then
                    call find_end(ifail)
                    if (ifail == 1) go to 9800
                    return
                else if (ierr == 1) then
                    go to 9800
                end if
                ixcon = 1
            end if
            if (aoption == 'DATE_1') then
                call get_date(ierr, dd1, mm1, yy1, 'DATE_1')
                if (ierr /= 0) go to 9800
            else if (aoption == 'DATE_2') then
                call get_date(ierr, dd2, mm2, yy2, 'DATE_2')
                if (ierr /= 0) go to 9800
            else if (aoption == 'TIME_1') then
                call get_time(ierr, hh1, nn1, ss1, 'TIME_1')
                if (ierr /= 0) go to 9800
            else if (aoption == 'TIME_2') then
                call get_time(ierr, hh2, nn2, ss2, 'TIME_2')
                if (ierr /= 0) go to 9800
            else if (aoption == 'NEW_SERIES_NAME') then
                call get_new_series_name(ierr, aname)
                if (ierr /= 0) go to 9800
            else if (aoption == 'SERIES_NAME') then
                call get_series_name(ierr, iseries, 'SERIES_NAME')
                if (ierr /= 0) go to 9800
            else if (aoption == 'CONTEXT') then
                if (ixcon /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 41) TRIM(aline), TRIM(sString_g)
41                  format('CONTEXT keyword in incorrect location at line ', a, ' of file ', a)
                    go to 9800
                end if
                call get_context(ierr, icontext, acontext)
                if (ierr /= 0) go to 9800
            else if (aoption == 'POWER') then
                call get_keyword_value(ierr, 2, itemp, tpower, 'POWER')
                if (ierr /= 0) go to 9800
                if (tpower == 0.0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 125) TRIM(aline), TRIM(sString_g)
125                 format('POWER must not be zero at line ', a, ' of file ', a)
                    go to 9800
                end if
            else if (aoption == 'LOG') then
                call get_yes_no(ierr, jtrans)
                if (ierr /= 0) go to 9800
                if (jtrans == 1) then
                    aaa = 'yes'
                else
                    aaa = 'no'
                end if
                write (*, 127) TRIM(aaa)
                write (LU_REC, 126) TRIM(aaa)
126             format(t5, 'LOG ', a)
            else if (aoption == 'STATISTIC') then
                call getfile(ierr, cline, statistic, left_word(2), right_word(2))
                if (ierr /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 42) TRIM(aline), TRIM(sString_g)
42                  format('cannot read statistic from line ', a, ' of file ', a)
                    go to 9800
                end if
                call casetrans(statistic, 'lo')
                if ((statistic /= 'sum') .AND. &
                    (statistic /= 'mean') .AND. &
                    (statistic /= 'std_dev') .AND. &
                    (statistic /= 'maximum') .AND. &
                    (statistic /= 'minimum') .AND. &
                    (statistic /= 'median') .AND. &
                    (statistic /= 'range')) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 43) TRIM(aline), TRIM(sString_g)
43                  format('statistic must be "mean", "median", "std_dev", "sum", ', &
                           '"maximum", "minimum" or "range" at line ', a, ' of file ', a)
                    go to 9800
                end if
                write (*, 127) TRIM(statistic)
                write (LU_REC, 127) TRIM(statistic)
127             format(t5, 'STATISTIC ', a)
            else if (aoption == 'PERIOD') then
                call getfile(ierr, cline, period, left_word(2), right_word(2))
                if (ierr /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 44) TRIM(aline), TRIM(sString_g)
44                  format('cannot read period from line ', a, ' of file ', a)
                    go to 9800
                end if
                call casetrans(period, 'lo')
                if ((period /= 'month_one') .AND. &
                    (period /= 'month_many') .AND. &
                    (period /= 'daily') .AND. &
                    (period /= 'year')) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 45) TRIM(aline), TRIM(sString_g)
45                  format('period must be "month_one", "month_many" or "year" ', &
                           'at line ', a, ' of file ', a)
                    go to 9800
                end if
                write (*, 128) TRIM(period)
                write (LU_REC, 128) TRIM(period)
128             format(t5, 'PERIOD ', a)
            else if (aoption == 'YEAR_TYPE') then
                call getfile(ierr, cline, year_type, left_word(2), right_word(2))
                if (ierr /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 46) TRIM(aline), TRIM(sString_g)
46                  format('cannot read year_type from line ', a, ' of file ', a)
                    go to 9800
                end if
                call casetrans(year_type, 'lo')
                if ((year_type /= 'water_high') .AND. &
                    (year_type /= 'water_low') .AND. &
                    (year_type /= 'calendar')) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 47) TRIM(aline), TRIM(sString_g)
47                  format('year_type must be "water_high", "water_low" ', &
                           'or "calendar" at line ', a, ' of file ', a)
                    go to 9800
                end if
                write (*, 129) TRIM(year_type)
                write (LU_REC, 129) TRIM(year_type)
129             format(t5, 'YEAR_TYPE ', a)
            else if (aoption == 'TIME_ABSCISSA') then
                call getfile(ierr, cline, abscissa, left_word(2), right_word(2))
                if (ierr /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 48) TRIM(aline), TRIM(sString_g)
48                  format('cannot read time abscissa from line ', a, ' of file ', a)
                    go to 9800
                end if
                call casetrans(abscissa, 'lo')
                if (abscissa == 'center') abscissa = 'centre'
                if ((abscissa /= 'start') .AND. &
                    (abscissa /= 'centre') .AND. &
                    (abscissa /= 'end')) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 49) TRIM(aline), TRIM(sString_g)
49                  format('time abscissa must be "start", "center", "centre" or "end" ', &
                           'at line ', a, ' of file ', a)
                    go to 9800
                end if
                write (*, 130) TRIM(abscissa)
                write (LU_REC, 130) TRIM(abscissa)
130             format(t5, 'TIME_ABSCISSA ', a)
            else if (aoption == 'END') then
                go to 200
            else
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 90) TRIM(aoption), TRIM(CurrentBlock_g), TRIM(aline), TRIM(sString_g)
90              format('unexpected keyword - "', a, '" in ', a, ' block at line ', a, &
                       ' of file ', a)
                go to 9800
            end if
        end do

! -- The block has been read; now it is checked for correctness.

200     continue
        if (iseries == 0) then
            write (amessage, 210) TRIM(CurrentBlock_g)
210         format('no SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (aname == ' ') then
            write (amessage, 230) TRIM(CurrentBlock_g)
230         format('no NEW_SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (icontext == 0) then
            write (amessage, 220) TRIM(CurrentBlock_g)
220         format('no CONTEXT keyword(s) provided in ', a, ' block.')
            go to 9800
        end if
        call date_check(ierr, yy1, mm1, dd1, hh1, nn1, ss1, yy2, mm2, dd2, hh2, nn2, ss2, &
                        begdays, begsecs, enddays, endsecs)
        if (ierr /= 0) go to 9800
        call beg_end_check(ierr, iseries, begdays, begsecs, enddays, endsecs)
        if (ierr /= 0) go to 9800
        if (period == ' ') then
            write (amessage, 240) TRIM(CurrentBlock_g)
240         format('the period keyword must be supplied within a ', &
                   a, ' block.')
            go to 9800
        end if
        if (statistic == ' ') then
            write (amessage, 244) TRIM(CurrentBlock_g)
244         format('the statistic keyword must be supplied within a ', &
                   a, ' block.')
            go to 9800
        end if
        if ((jtrans == 1) .AND. (tpower > -1.0E30)) then
            write (amessage, 245) TRIM(CurrentBlock_g)
245         format('either the LOG or POWER keywords can be supplied ', &
                   'in a ', a, ' block, but not both.')
            go to 9800
        end if
        if (abscissa == ' ') then
            write (amessage, 246) TRIM(CurrentBlock_g)
246         format('the time_abscissa keyword must be supplied in ', &
                   'a ', a, ' block.')
            go to 9800
        end if

! -- All is well with the block. The requested statistics are computed.

        call numterms(iterm, ibterm, ieterm, begdays, begsecs, enddays, endsecs, iseries)
        if (iterm == 0) then
            write (amessage, 270) TRIM(series_g(iseries)%name)
270         format('there are no terms in time series "', a, '" between the provided ', &
                   'dates and times.')
            go to 9800
        end if

        if ((begdays < series_g(iseries)%days(1)) .OR. &
            ((begdays == series_g(iseries)%days(1)) .AND. &
             (begsecs < series_g(iseries)%secs(1)))) then
            begdays = series_g(iseries)%days(1)
            begsecs = series_g(iseries)%secs(1)
        end if
        iiterm = series_g(iseries)%nterm
        if ((enddays > series_g(iseries)%days(iiterm)) .OR. &
            ((enddays == series_g(iseries)%days(iiterm)) .AND. &
             (endsecs > series_g(iseries)%secs(iiterm)))) then
            enddays = series_g(iseries)%days(iiterm)
            endsecs = series_g(iseries)%secs(iiterm)
        end if

        call alloc_tempseries(ierr, iterm)
        if (ierr /= 0) then
            write (amessage, 280)
280         format('cannot allocate temporary time series')
            goto 9800
        end if

        ndays = series_g(iseries)%days(ibterm)
!       call newdate(ndays,1,1,1970,cdd,cmm,cyy)
        call gregorian_date(iJD=ndays, &
                            iMonth=cmm, &
                            iDay=cdd, &
                            iYear=cyy)
        if (period == 'year') then
            cwy = cyy
            selectcase (year_type)
            case ('water_high')
                if (cmm >= 10) cwy = cyy + 1
            case ('water_low')
                if (cmm >= 4) cwy = cyy + 1
            end select
        end if

        iterm = 0
        nvals = 0
        tmax = -1.0E30
        tmin = 1.0E30
        tsum = 0
        tsum2 = 0
        do i = 1, 12
            nvals_mon(i) = 0
            tmax_mon(i) = -1.0E30
            tmin_mon(i) = 1.0E30
            tsum_mon(i) = 0
            tsum2_mon(i) = 0
        end do

        if (tpower < -1.0E30) tpower = 0.0

        do j = ibterm, ieterm
            ndays = series_g(iseries)%days(j)
!         call newdate(ndays,1,1,1970,dd,mm,yy)
            call gregorian_date(iJD=ndays, &
                                iMonth=mm, &
                                iDay=dd, &
                                iYear=yy)

            rtemp = series_g(iseries)%val(j)
            rvalue = rtemp
            if (jtrans == 1) then
                if (rtemp <= 0.0) then
                    write (amessage, 350) TRIM(series_g(iseries)%name)
350                 format('cannot compute statistics on basis of log transform of terms ', &
                           'in series "', a, '" as there are zero or negative terms in this series.')
                    goto 9800
                end if
                rtemp = LOG10(rtemp)
            else
                if (tpower /= 0.0) then
                    if ((tpower < 0.0) .AND. (rtemp == 0.0)) then
                        write (amessage, 355) TRIM(series_g(iseries)%name)
355                     format('cannot compute statistics based on a negative POWER because ', &
                               'at least one of the terms of series "', a, '" is zero.')
                        goto 9800
                    end if
                    if ((ABS(tpower) < 1.0) .AND. (rtemp < 0.0)) then
                        write (amessage, 360) TRIM(series_g(iseries)%name)
360                     format('cannot compute statistics based on a POWER with absolute value ', &
                               'less than one because ', &
                               'at least one of the terms of series "', a, '" is negative.')
                        goto 9800
                    end if
                    rtemp = rtemp**tpower
                end if
            end if
            selectcase (period)
            case ('year')
                wy = yy
                selectcase (year_type)
                case ('water_high')
                    if (mm >= 10) wy = yy + 1
                case ('water_low')
                    if (mm >= 4) wy = yy + 1
                end select
                if (wy == cwy) then
                    nvals = nvals + 1
                    tsum = tsum + rtemp
                    tsum2 = tsum2 + rtemp * rtemp
                    if (rtemp > tmax) tmax = rtemp
                    if (rtemp < tmin) tmin = rtemp
                else
                    if (nvals > 0) then
                        selectcase (statistic)
                        case ('sum')
                            tstat = tsum
                        case ('mean')
                            tstat = tsum / nvals
                        case ('std_dev')
                            if (nvals == 1) then
                                tstat = 0
                            else
                                rmean = tsum / nvals
                                tstat = SQRT((tsum2 - nvals * rmean * rmean) / (nvals - 1))
                            end if
                        case ('maximum')
                            tstat = tmax
                        case ('minimum')
                            tstat = tmin
                        case ('range')
                            tstat = tmax - tmin
                        end select
                        selectcase (abscissa)
                        case ('start')
                            selectcase (year_type)
                            case ('water_high')
!                         ndays=numdays(1,1,1970,1,10,cwy-1)
                                ndays = julian_day(iMonth=10, iDay=1, iYear=cwy - 1)
                            case ('water_low')
!                       ndays=numdays(1,1,1970,1,4,cwy-1)
                                ndays = julian_day(iMonth=4, iDay=1, iYear=cwy - 1)
                            case ('calendar')
!                       ndays=numdays(1,1,1970,1,1,cwy)
                                ndays = julian_day(iMonth=1, iDay=1, iYear=cwy)
                            end select
                        case ('centre')
                            selectcase (year_type)
                            case ('water_high')
!                         ndays=numdays(1,1,1970,1,4,cwy)
                                ndays = julian_day(iMonth=4, iDay=1, iYear=cwy)
                            case ('water_low')
!                         ndays=numdays(1,1,1970,1,10,cwy)
                                ndays = julian_day(iMonth=10, iDay=1, iYear=cwy)
                            case ('calendar')
!                         ndays=numdays(1,1,1970,1,7,cwy)
                                ndays = julian_day(iMonth=7, iDay=1, iYear=cwy)
                            end select
                        case ('end')
                            selectcase (year_type)
                            case ('water_high')
!                         ndays=numdays(1,1,1970,30,9,cwy)
                                ndays = julian_day(iMonth=9, iDay=30, iYear=cwy)
                            case ('water_low')
!                         ndays=numdays(1,1,1970,31,3,cwy)
                                ndays = julian_day(iMonth=3, iDay=31, iYear=cwy)
                            case ('calendar')
!                         ndays=numdays(1,1,1970,31,12,cwy)
                                ndays = julian_day(iMonth=12, iDay=31, iYear=cwy)
                            end select
                        end select
                        iterm = iterm + 1
                        tempseries_g%days(iterm) = ndays
                        tempseries_g%secs(iterm) = 0
                        tempseries_g%val(iterm) = tstat
                    end if
                    cwy = wy
                    nvals = 1
                    tsum = rtemp
                    tsum2 = rtemp * rtemp
                    tmax = rvalue
                    tmin = rvalue
                end if
            case ('month_many')
                if (mm == cmm) then
                    nvals = nvals + 1
                    tsum = tsum + rtemp
                    tsum2 = tsum2 + rtemp * rtemp
                    if (rtemp > tmax) tmax = rtemp
                    if (rtemp < tmin) tmin = rtemp
                else
                    if (nvals > 0) then
                        selectcase (statistic)
                        case ('sum')
                            tstat = tsum
                        case ('mean')
                            tstat = tsum / float(nvals)
                        case ('std_dev')
                            if (nvals == 1) then
                                tstat = 0
                            else
                                rmean = tsum / nvals
                                tstat = SQRT((tsum2 - nvals * rmean * rmean) / (nvals - 1))
                            end if
                        case ('maximum')
                            tstat = tmax
                        case ('minimum')
                            tstat = tmin
                        case ('range')
                            tstat = tmax - tmin
                        end select
                        selectcase (abscissa)
                        case ('start')
!                     ndays=numdays(1,1,1970,1,cmm,cyy)
                            ndays = julian_day(iMonth=cmm, iDay=1, iYear=cyy)
                        case ('centre')
!                     ndays=numdays(1,1,1970,15,cmm,cyy)
                            ndays = julian_day(iMonth=cmm, iDay=15, iYear=cyy)
                        case ('end')
                            days = DA(cmm)
                            if ((cmm == 2) .AND. (leap(cyy))) days = days + 1
!                     ndays=numdays(1,1,1970,days,cmm,cyy)
                            ndays = julian_day(iMonth=cmm, iDay=days, iYear=cyy)
                        end select
                        iterm = iterm + 1
                        tempseries_g%days(iterm) = ndays
                        tempseries_g%secs(iterm) = 0
                        tempseries_g%val(iterm) = tstat
                    end if
                    cmm = mm
                    cyy = yy
                    nvals = 1
                    tsum = rtemp
                    tsum2 = rtemp * rtemp
                    tmax = rvalue
                    tmin = rvalue
                end if
            case ('month_one')
                nvals_mon(mm) = nvals_mon(mm) + 1
                tsum_mon(mm) = tsum_mon(mm) + rtemp
                tsum2_mon(mm) = tsum2_mon(mm) + rtemp * rtemp
                if (rtemp > tmax_mon(mm)) tmax_mon(mm) = rtemp
                if (rtemp < tmin_mon(mm)) tmin_mon(mm) = rtemp
            case ('daily')
                if (dd == cdd) then
                    nvals = nvals + 1
                    tsum = tsum + rtemp
                    tsum2 = tsum2 + rtemp * rtemp
                    if (rtemp > tmax) tmax = rtemp
                    if (rtemp < tmin) tmin = rtemp
                else
                    if (nvals > 0) then
                        selectcase (statistic)
                        case ('sum')
                            tstat = tsum
                        case ('mean')
                            tstat = tsum / float(nvals)
                        case ('std_dev')
                            if (nvals == 1) then
                                tstat = 0
                            else
                                rmean = tsum / nvals
                                tstat = SQRT((tsum2 - nvals * rmean * rmean) / (nvals - 1))
                            end if
                        case ('maximum')
                            tstat = tmax
                        case ('minimum')
                            tstat = tmin
                        case ('range')
                            tstat = tmax - tmin
                        end select
                        selectcase (abscissa)
                        case ('start')
!                     ndays=numdays(1,1,1970,1,cmm,cyy)
                            ndays = julian_day(iMonth=cmm, iDay=cdd, iYear=cyy)
                            nsecs = 0
                        case ('centre')
!                     ndays=numdays(1,1,1970,15,cmm,cyy)
                            ndays = julian_day(iMonth=cmm, iDay=cdd, iYear=cyy)
                            nsecs = 43200
                        case ('end')
!                     ndays=numdays(1,1,1970,days,cmm,cyy)
                            ndays = julian_day(iMonth=cmm, iDay=cdd, iYear=cyy)
                            nsecs = 86399
                        end select
                        iterm = iterm + 1
                        tempseries_g%days(iterm) = ndays
                        tempseries_g%secs(iterm) = nsecs
                        tempseries_g%val(iterm) = tstat
                    end if
                    cdd = dd
                    cmm = mm
                    cyy = yy
                    nvals = 1
                    tsum = rtemp
                    tsum2 = rtemp * rtemp
                    tmax = rvalue
                    tmin = rvalue
                end if
            end select
        end do

! --  write the last value

        selectcase (period)
        case ('year')
            wy = yy
            selectcase (year_type)
            case ('water_high')
                if (mm >= 10) wy = yy + 1
            case ('water_low')
                if (mm >= 4) wy = yy + 1
            end select
            if (nvals > 0) then
                selectcase (statistic)
                case ('sum')
                    tstat = tsum
                case ('mean')
                    tstat = tsum / nvals
                case ('std_dev')
                    if (nvals == 1) then
                        tstat = 0
                    else
                        rmean = tsum / nvals
                        tstat = SQRT((tsum2 - nvals * rmean * rmean) / (nvals - 1))
                    end if
                case ('maximum')
                    tstat = tmax
                case ('minimum')
                    tstat = tmin
                case ('range')
                    tstat = tmax - tmin
                end select
                selectcase (abscissa)
                case ('start')
                    selectcase (year_type)
                    case ('water_high')
!                     ndays=numdays(1,1,1970,1,10,cwy-1)
                        ndays = julian_day(iMonth=10, iDay=1, iYear=cwy - 1)
                    case ('water_low')
!                     ndays=numdays(1,1,1970,1,4,cwy-1)
                        ndays = julian_day(iMonth=4, iDay=1, iYear=cwy - 1)
                    case ('calendar')
!                     ndays=numdays(1,1,1970,1,1,cwy)
                        ndays = julian_day(iMonth=1, iDay=1, iYear=cwy)
                    end select
                case ('centre')
                    selectcase (year_type)
                    case ('water_high')
!                     ndays=numdays(1,1,1970,1,4,cwy)
                        ndays = julian_day(iMonth=4, iDay=1, iYear=cwy)
                    case ('water_low')
!                     ndays=numdays(1,1,1970,1,10,cwy)
                        ndays = julian_day(iMonth=10, iDay=1, iYear=cwy)
                    case ('calendar')
!                     ndays=numdays(1,1,1970,1,7,cwy)
                        ndays = julian_day(iMonth=7, iDay=1, iYear=cwy)
                    end select
                case ('end')
                    selectcase (year_type)
                    case ('water_high')
!                     ndays=numdays(1,1,1970,30,9,cwy)
                        ndays = julian_day(iMonth=9, iDay=30, iYear=cwy)
                    case ('water_low')
!                     ndays=numdays(1,1,1970,31,3,cwy)
                        ndays = julian_day(iMonth=3, iDay=31, iYear=cwy)
                    case ('calendar')
!                     ndays=numdays(1,1,1970,31,12,cwy)
                        ndays = julian_day(iMonth=12, iDay=31, iYear=cwy)
                    end select
                end select
                iterm = iterm + 1
                tempseries_g%days(iterm) = ndays
                tempseries_g%secs(iterm) = 0
                tempseries_g%val(iterm) = tstat
            end if
        case ('month_many')
            if (nvals > 0) then
                selectcase (statistic)
                case ('sum')
                    tstat = tsum
                case ('mean')
                    tstat = tsum / float(nvals)
                case ('std_dev')
                    if (nvals == 1) then
                        tstat = 0
                    else
                        rmean = tsum / nvals
                        tstat = SQRT((tsum2 - nvals * rmean * rmean) / (nvals - 1))
                    end if
                case ('maximum')
                    tstat = tmax
                case ('minimum')
                    tstat = tmin
                case ('range')
                    tstat = tmax - tmin
                end select
                selectcase (abscissa)
                case ('start')
!                 ndays=numdays(1,1,1970,1,cmm,cyy)
                    ndays = julian_day(iMonth=cmm, iDay=1, iYear=cyy)
                case ('centre')
!                 ndays=numdays(1,1,1970,15,cmm,cyy)
                    ndays = julian_day(iMonth=cmm, iDay=15, iYear=cyy)
                case ('end')
                    days = DA(cmm)
                    if ((cmm == 2) .AND. (leap(cyy))) days = days + 1
!                 ndays=numdays(1,1,1970,days,cmm,cyy)
                    ndays = julian_day(iMonth=cmm, iDay=days, iYear=cyy)
                end select
                iterm = iterm + 1
                tempseries_g%days(iterm) = ndays
                tempseries_g%secs(iterm) = 0
                tempseries_g%val(iterm) = tstat
            end if
        case ('month_one')
            do i = 1, 12
                nvals = nvals_mon(i)
                tmin = tmin_mon(i)
                tmax = tmax_mon(i)
                tsum = tsum_mon(i)
                tsum2 = tsum2_mon(i)
                if (nvals > 0) then
                    selectcase (statistic)
                    case ('sum')
                        tstat = tsum
                    case ('mean')
                        tstat = tsum / float(nvals)
                    case ('std_dev')
                        if (nvals == 1) then
                            tstat = 0
                        else
                            rmean = tsum / nvals
                            tstat = SQRT((tsum2 - nvals * rmean * rmean) / (nvals - 1))
                        end if
                    case ('maximum')
                        tstat = tmax
                    case ('minimum')
                        tstat = tmin
                    case ('range')
                        tstat = tmax - tmin
                    end select
                    selectcase (abscissa)
                    case ('start')
!                   ndays=numdays(1,1,1970,1,i,1800)
                        ndays = julian_day(iMonth=i, iDay=1, iYear=1800)
                    case ('centre')
!                   ndays=numdays(1,1,1970,15,i,1800)
                        ndays = julian_day(iMonth=i, iDay=15, iYear=1800)
                    case ('end')
                        days = DA(i)
!                   ndays=numdays(1,1,1970,days,i,1800)
                        ndays = julian_day(iMonth=i, iDay=days, iYear=1800)
                    end select
                    iterm = iterm + 1
                    tempseries_g%days(iterm) = ndays
                    tempseries_g%secs(iterm) = 0
                    tempseries_g%val(iterm) = tstat
                end if
            end do
        case ('daily')
            if (nvals > 0) then
                selectcase (statistic)
                case ('sum')
                    tstat = tsum
                case ('mean')
                    tstat = tsum / float(nvals)
                case ('std_dev')
                    if (nvals == 1) then
                        tstat = 0
                    else
                        rmean = tsum / nvals
                        tstat = SQRT((tsum2 - nvals * rmean * rmean) / (nvals - 1))
                    end if
                case ('maximum')
                    tstat = tmax
                case ('minimum')
                    tstat = tmin
                case ('range')
                    tstat = tmax - tmin
                end select
                selectcase (abscissa)
                case ('start')
!                 ndays=numdays(1,1,1970,1,cmm,cyy)
                    ndays = julian_day(iMonth=cmm, iDay=cdd, iYear=cyy)
                    nsecs = 0
                case ('centre')
!                 ndays=numdays(1,1,1970,15,cmm,cyy)
                    ndays = julian_day(iMonth=cmm, iDay=cdd, iYear=cyy)
                    nsecs = 43200
                case ('end')
!                 ndays=numdays(1,1,1970,days,cmm,cyy)
                    ndays = julian_day(iMonth=cmm, iDay=cdd, iYear=cyy)
                    nsecs = 86399
                end select
                iterm = iterm + 1
                tempseries_g%days(iterm) = ndays
                tempseries_g%secs(iterm) = nsecs
                tempseries_g%val(iterm) = tstat
            end if
        end select
! --  space is allocated for the new time series.

        do i = 1, MAXSERIES
            if (.NOT. series_g(i)%active) go to 380
        end do
        write (amessage, 370)
370     format('no more time series available for data storage - increase MAXSERIES and ', &
               'recompile program.')
        go to 9800

380     allocate (series_g(i)%days(iterm), series_g(i)%secs(iterm), &
                  series_g(i)%val(iterm), stat=ierr)
        if (ierr /= 0) then
            write (amessage, 390)
390         format('cannot allocate memory for another time series.')
            go to 9800
        end if

        series_g(i)%active = .TRUE.
        series_g(i)%name = aname
        series_g(i)%nterm = iterm
        series_g(i)%type = 'ts'

        do j = 1, iterm
            series_g(i)%days(j) = tempseries_g%days(j)
            series_g(i)%secs(j) = tempseries_g%secs(j)
            series_g(i)%val(j) = tempseries_g%val(j)
        end do

        write (6, 400) TRIM(series_g(iseries)%name), TRIM(aname)
        write (LU_REC, 400) TRIM(series_g(iseries)%name), TRIM(aname)
400     format(t5, 'Period statistics for time series "', a, '" stored in ', &
               'time series "', a, '".')
        return

9000    call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9010) TRIM(aline), TRIM(sString_g)
9010    format('cannot read line ', a, ' of TSPROC input file ', a)
        go to 9800
9100    continue
        call addquote(sInfile_g, sString_g)
        write (amessage, 9110) TRIM(sString_g), TRIM(CurrentBlock_g)
9110    format('unexpected end encountered to TSPROC input file ', a, ' while ', &
               ' reading ', a, ' block.')
        go to 9800

9800    call write_message(leadspace='yes', error='yes')
        call write_message(iunit=LU_REC, leadspace='yes')
        ifail = 1

        return

    end subroutine period_stats

    subroutine usgs_hysep(ifail)

! -- Subroutine usgs_hysep performs baseflow separation for a hydrograph time series_g.

        implicit none

        integer, intent(out) :: ifail

        integer dd1, mm1, yy1, hh1, nn1, ss1, dd2, mm2, yy2, hh2, nn2, ss2, ierr, &
            icontext, i, begdays, begsecs, enddays, endsecs, iseries_g, &
            j, ibterm, ieterm, iterm, iiterm, itemp, ixcon, nmax, &
            ndays, nsecs, npts, intrvl, &
            DA(12)
        real twindow, rtemp, tarea, rintr
        real, dimension(:), allocatable :: tdate, tval, tbflow !rank 1
        character(len=iTSNAMELENGTH) :: aname
        character(15) aline
        character(25) aoption, hysep_type
        character(25) acontext(MAXCONTEXT)
        data DA/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/

        ifail = 0
        CurrentBlock_g = 'USGS_HYSEP'

        write (*, 10) TRIM(CurrentBlock_g)
        write (LU_REC, 10) TRIM(CurrentBlock_g)
10      format(/, ' Processing ', a, ' block....')

        icontext = 0
        iseries_g = 0
        yy1 = -9999
        hh1 = -9999
        yy2 = -9999
        hh2 = -9999
        twindow = 1
        aname = ' '
        ixcon = 0

! -- The USGS_HYSEP block is first parsed.

        do
            ILine_g = ILine_g + 1
            read (LU_TSPROC_CONTROL, '(a)', err=9000, end=9100) cline
            if (cline == ' ') cycle
            if (cline(1:1) == '#') cycle
            call linesplit(ierr, 2)
            if (ierr /= 0) then
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 20) TRIM(aline), TRIM(sString_g)
20              format('there should be 2 entries on line ', a, ' of file ', a)
                go to 9800
            end if
            aoption = cline(left_word(1):right_word(1))
            call casetrans(aoption, 'hi')
            if (aoption /= 'CONTEXT') then
                call test_context(ierr, icontext, acontext)
                if (ierr == -1) then
                    call find_end(ifail)
                    if (ifail == 1) go to 9800
                    return
                else if (ierr == 1) then
                    go to 9800
                end if
                ixcon = 1
            end if
            if (aoption == 'DATE_1') then
                call get_date(ierr, dd1, mm1, yy1, 'DATE_1')
                if (ierr /= 0) go to 9800
            else if (aoption == 'DATE_2') then
                call get_date(ierr, dd2, mm2, yy2, 'DATE_2')
                if (ierr /= 0) go to 9800
            else if (aoption == 'TIME_1') then
                call get_time(ierr, hh1, nn1, ss1, 'TIME_1')
                if (ierr /= 0) go to 9800
            else if (aoption == 'TIME_2') then
                call get_time(ierr, hh2, nn2, ss2, 'TIME_2')
                if (ierr /= 0) go to 9800
            else if (aoption == 'NEW_SERIES_NAME') then
                call get_new_series_name(ierr, aname)
                if (ierr /= 0) go to 9800
            else if (aoption == 'SERIES_NAME') then
                call get_series_name(ierr, iseries_g, 'SERIES_NAME')
                if (ierr /= 0) go to 9800
            else if (aoption == 'CONTEXT') then
                if (ixcon /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 41) TRIM(aline), TRIM(sString_g)
41                  format('CONTEXT keyword in incorrect location at line ', a, ' of file ', a)
                    go to 9800
                end if
                call get_context(ierr, icontext, acontext)
                if (ierr /= 0) go to 9800
            else if (aoption == 'HYSEP_TYPE') then
                call getfile(ierr, cline, hysep_type, left_word(2), right_word(2))
                if (ierr /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 42) TRIM(aline), TRIM(sString_g)
42                  format('cannot read hysep_type from line ', a, ' of file ', a)
                    go to 9800
                end if
                call casetrans(hysep_type, 'lo')
                if ((hysep_type /= 'fixed_interval') .AND. &
                    (hysep_type /= 'sliding_interval') .AND. &
                    (hysep_type /= 'local_minimum')) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 43) TRIM(aline), TRIM(sString_g)
43                  format('hysep_type must be "fixed_interval", "sliding_interval" or "local_mimimum" ', &
                           'at line ', a, ' of file ', a)
                    go to 9800
                end if
            else if (aoption == 'TIME_INTERVAL') then
                call get_keyword_value(ierr, 1, intrvl, rtemp, 'TIME_INTERVAL')
                if (ierr /= 0) go to 9800
                if (intrvl <= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 44) TRIM(aline), TRIM(sString_g)
44                  format('TIME_INTERVAL must be greater than zero at line ', a, ' of file ', a)
                    go to 9800
                end if
                if (MOD(intrvl, 2) == 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 45) TRIM(aline), TRIM(sString_g)
45                  format('TIME_INTERVAL must be an odd integer at line ', a, ' of file ', a)
                    go to 9800
                end if
            else if (aoption == 'DRAINAGE_AREA') then
                call get_keyword_value(ierr, 2, itemp, tarea, 'DRAINAGE_AREA')
                if (ierr /= 0) go to 9800
                if (tarea <= 0.0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 46) TRIM(aline), TRIM(sString_g)
46                  format('DRAINAGE_AREA must be greater than zero at line ', a, ' of file ', a)
                    go to 9800
                end if
            else if (aoption == 'END') then
                go to 200
            else
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 90) TRIM(aoption), TRIM(CurrentBlock_g), TRIM(aline), TRIM(sString_g)
90              format('unexpected keyword - "', a, '" in ', a, ' block at line ', a, &
                       ' of file ', a)
                go to 9800
            end if
        end do

! -- The block has been read; now it is checked for correctness.

200     continue
        if (iseries_g == 0) then
            write (amessage, 210) TRIM(CurrentBlock_g)
210         format('no SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (aname == ' ') then
            write (amessage, 230) TRIM(CurrentBlock_g)
230         format('no NEW_SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (icontext == 0) then
            write (amessage, 220) TRIM(CurrentBlock_g)
220         format('no CONTEXT keyword(s) provided in ', a, ' block.')
            go to 9800
        end if
        call date_check(ierr, yy1, mm1, dd1, hh1, nn1, ss1, yy2, mm2, dd2, hh2, nn2, ss2, &
                        begdays, begsecs, enddays, endsecs)
        if (ierr /= 0) go to 9800
        call beg_end_check(ierr, iseries_g, begdays, begsecs, enddays, endsecs)
        if (ierr /= 0) go to 9800

! -- All is well with the block. The requested peaks are found.

        call numterms(iterm, ibterm, ieterm, begdays, begsecs, enddays, endsecs, iseries_g)
        if (iterm == 0) then
            write (amessage, 270) TRIM(series_g(iseries_g)%name)
270         format('there are no terms in time series "', a, '" between the provided ', &
                   'dates and times.')
            go to 9800
        end if

        if ((begdays < series_g(iseries_g)%days(1)) .OR. &
            ((begdays == series_g(iseries_g)%days(1)) .AND. &
             (begsecs < series_g(iseries_g)%secs(1)))) then
            begdays = series_g(iseries_g)%days(1)
            begsecs = series_g(iseries_g)%secs(1)
        end if
        iiterm = series_g(iseries_g)%nterm
        if ((enddays > series_g(iseries_g)%days(iiterm)) .OR. &
            ((enddays == series_g(iseries_g)%days(iiterm)) .AND. &
             (endsecs > series_g(iseries_g)%secs(iiterm)))) then
            enddays = series_g(iseries_g)%days(iiterm)
            endsecs = series_g(iseries_g)%secs(iiterm)
        end if

        nmax = ieterm - ibterm + 1
        allocate (tdate(nmax), tval(nmax), tbflow(nmax), stat=ierr)
        if (ierr /= 0) then
            write (amessage, 280)
280         format('cannot allocate temporary time series')
            goto 9800
        end if
        npts = 0
        do j = ibterm, ieterm
            ndays = series_g(iseries_g)%days(j)
            nsecs = series_g(iseries_g)%secs(j)
            rtemp = series_g(iseries_g)%val(j)
            npts = npts + 1
            tdate(npts) = ndays + nsecs / 86400
            tval(npts) = rtemp
        end do

        if (tarea > rNEAR_ZERO) then
            rintr = 2.0 * (tarea**0.2)
            if (rintr <= 4.0) then
                intrvl = 3
            else if (rintr <= 6.0 .AND. rintr > 4.0) then
                intrvl = 5
            else if (rintr <= 8.0 .AND. rintr > 6.0) then
                intrvl = 7
            else if (rintr <= 10. .AND. rintr > 8.0) then
                intrvl = 9
            else
                intrvl = 11
            end if
        end if

        selectcase (hysep_type)
        case ('fixed_interval')
            call fixed(npts, tval, intrvl, 1, 0, tbflow)
        case ('sliding_interval')
            call slide(npts, tval, intrvl, tbflow)
        case ('local_minimum')
            call locmin(npts, tval, intrvl, tbflow)
        end select

! --  space is allocated for the new time series_g.

        do i = 1, MAXSERIES
            if (.NOT. series_g(i)%active) go to 380
        end do
        write (amessage, 370)
370     format('no more time series available for data storage - increase MAXSERIES and ', &
               'recompile program.')
        go to 9800

380     allocate (series_g(i)%days(npts), series_g(i)%secs(npts), &
                  series_g(i)%val(npts), stat=ierr)
        if (ierr /= 0) then
            write (amessage, 390)
390         format('cannot allocate memory for another time series.')
            go to 9800
        end if

        series_g(i)%active = .TRUE.
        series_g(i)%name = aname
        series_g(i)%nterm = npts
        series_g(i)%type = 'ts'

        do j = 1, npts
            ndays = INT(tdate(j))
            nsecs = 86400 * (tdate(j) - ndays)
            series_g(i)%days(j) = ndays
            series_g(i)%secs(j) = nsecs
            series_g(i)%val(j) = tbflow(j)
        end do

        write (6, 400) TRIM(series_g(iseries_g)%name), TRIM(aname)
        write (LU_REC, 400) TRIM(series_g(iseries_g)%name), TRIM(aname)
400     format(t5, 'Baseflow values for time series "', a, '" stored in ', &
               'time series "', a, '".')
        return

9000    call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9010) TRIM(aline), TRIM(sString_g)
9010    format('cannot read line ', a, ' of TSPROC input file ', a)
        go to 9800
9100    continue
        call addquote(sInfile_g, sString_g)
        write (amessage, 9110) TRIM(sString_g), TRIM(CurrentBlock_g)
9110    format('unexpected end encountered to TSPROC input file ', a, ' while ', &
               ' reading ', a, ' block.')
        go to 9800

9800    call write_message(leadspace='yes', error='yes')
        call write_message(iunit=LU_REC, leadspace='yes')
        ifail = 1

        return

    end subroutine usgs_hysep

    subroutine FIXED(NDAYS, DIS, INTRVL, LSTFLG, START, GDIS)
!
!     + + + PURPOSE + + +
!     Hydrograph separation by the fixed interval method
!     adapted from Pettyjohn and Henning (1979)
!     by R. A. Sloto
!     04/01/88   3.2
!
!     Code modified by AML for cases of missing data at beginning of
!     record and transition between years.  Missing data values are
!     defined by negative values.  9/16/91
!
!     + + + DUMMY ARGUMENTS + + +
        integer NDAYS, INTRVL, START, LSTFLG
        real DIS(*), GDIS(*)
!
!     + + + ARGUMENT DEFINITION + + +
!     NDAYS  - number of days for processing
!     DIS    - daily streamflow
!     INTRVL - time period in days for finding minimum
!     LSTFLG - flag set equal to 1 if last year being processed
!     START  - position (day) in DIS after which initial interval should begin
!              (index of last processed value/last bad value)
!     GDIS   - computed daily baseflow
!
!     + + + LOCAL VARIABLES + + +
        integer I, J, K, L1, L2, LD, LA, M, GOODV, FXDAYS
        real PMIN
!
!     + + + END SPECIFICATIONS + + +
!
!     initialize variables and find start of good data--
!     initialize GDIS to 0.0 for days of good data and set equal
!     to DIS for days of negative data
        GOODV = 0
        do I = 1, NDAYS
            if (DIS(I) >= 0.0) then
                GDIS(I) = 0.0
                GOODV = 1
            else
                GDIS(I) = DIS(I)
!         reset START only if a good value hasn't been found after initial START
                if (GOODV == 0 .AND. I > START) START = I
            end if
        end do
!
        FXDAYS = NDAYS
        if (LSTFLG == 1) then
!       last year to process; reduce total days for processing by the
!       number of missing values within extra 11 days at end
            do I = NDAYS, NDAYS - 10, -1
                if (DIS(I) < 0) then
                    FXDAYS = FXDAYS - 1
                end if
            end do
        end if
!
        K = (FXDAYS - START) / INTRVL
!     for beginning of period
        if (K >= 1) then
            do I = 1, K
                PMIN = 100000.
                L1 = ((I - 1) * INTRVL) + START + 1
                L2 = I * INTRVL + START
                do J = L1, L2
                    if (DIS(J) < PMIN) PMIN = DIS(J)
                end do
                do J = L1, L2
                    GDIS(J) = PMIN
                end do
            end do
        end if
!
        if (LSTFLG /= 1) then
!       set START for next year so that first few days of next year
!       use some of last few days of this year to determine base flow
!
!       find last day processed
            LD = START + K * INTRVL
!       find last day of year in array
            LA = NDAYS - 11
!       move START back in INTRVL increments
            M = LD
70          continue
            M = M - INTRVL
            if (M > LA) goto 70
            START = 11 - (LA - M)
        else
!       last year to process
            if (L2 < FXDAYS) then
!         extra days left over after processing K intervals;
!         do base-flow separation on those days by themselves
                PMIN = 100000.
                do J = L2 + 1, FXDAYS
                    if (DIS(J) < PMIN) PMIN = DIS(J)
                end do
                do J = L2 + 1, FXDAYS
                    GDIS(J) = PMIN
                end do
            end if
        end if
!
        return
    end subroutine FIXED

    subroutine SLIDE(NDAYS, DIS, INTRVL, GDIS)
!
!     + + + PURPOSE + + +
!     Hydrograph separation by the sliding interval method
!     adapted from Pettyjohn and Henning (1979)
!     by R. A. Sloto
!     04/05/88   3.1
!
!     Code modified by AML for cases of missing data.  Missing data values
!     are identified by negative values.  Code also converted to
!     IF-THEN-ELSE structure.  9/16/91
!
!     + + + DUMMY ARGUMENTS + + +
        integer NDAYS, INTRVL
        real DIS(*), GDIS(*)
!
!     + + + ARGUMENT DEFINITION + + +
!     NDAYS  - number of days for processing
!     DIS    - daily streamflow
!     GDIS   - computed daily baseflow
!     INTRVL - time period in days for finding minimum
!
!
!     + + + LOCAL VARIABLES + + +
        integer I, J, INT, K1, K2, GOODV, START, S1, DAY
        real PMIN
!
!     + + + END SPECIFICATIONS + + +
!
!     initialize variables and find start of good data--
!     initialize GDIS to 0.0 for days of good data and set equal
!     to DIS for days of negative data
        START = 0
        GOODV = 0
        do I = 1, NDAYS
            if (DIS(I) >= 0.0) then
                GDIS(I) = 0.0
                GOODV = 1
            else
                GDIS(I) = DIS(I)
!         reset START only if a good value hasn't been found yet
                if (GOODV == 0) START = I
            end if
        end do
!
        INT = (INTRVL - 1) / 2
        S1 = START + 1
        do I = S1, NDAYS
            if (DIS(I) >= 0.0) then
!         set DAY equal to index of current day in year
                DAY = I - START
                if (DAY <= INT) then
!           when day near beginning
                    PMIN = 100000.
                    K2 = I + INT
                    do J = S1, K2
                        if (DIS(J) < PMIN) PMIN = DIS(J)
                    end do
                    GDIS(I) = PMIN
                else if (NDAYS - I <= INT) then
!           when day near end
                    PMIN = 100000.
                    K1 = I - INT
                    do J = K1, NDAYS
                        if (DIS(J) < PMIN) PMIN = DIS(J)
                    end do
                    GDIS(I) = PMIN
                else
!           when day not near beginning or end
                    PMIN = 100000.
                    K1 = I - INT
                    K2 = I + INT
                    do J = K1, K2
                        if (DIS(J) < PMIN) PMIN = DIS(J)
                    end do
                    GDIS(I) = PMIN
                end if
            end if
        end do
!
        return
    end subroutine SLIDE

    subroutine LOCMIN(NDAYS, DIS, INTRVL, GDIS)
!
!     + + + PURPOSE + + +
!     Hydrograph separation by the local minimum method
!     adapted from Pettyjohn and Henning (1979)
!     by R. A. Sloto
!     04/05/88   2.1
!
!     Code modified by AML for cases of missing data.  Missing data values
!     are identified by negative values.  Code also converted to
!     IF-THEN-ELSE structure.  9/16/91
!
!     + + + DUMMY ARGUMENTS + + +
        integer NDAYS, INTRVL
        real, dimension(:) :: DIS, GDIS
!
!     + + + ARGUMENT DEFINITION + + +
!     NDAYS  - Number of days in the year
!     DIS    - streamflow
!     GDIS   - baseflow
!     INTRVL - interval
!
!     + + + LOCAL VARIABLES + + +

        integer, allocatable, dimension(:) :: IPOINT
        integer I, J, K, NUMPT, L, ID, PFLAG, end, &
            IJ, IP1, IP2, ISTART, IEND, IJK, S, GOODV, START
        real X, Y
!
!     + + + INTRINSCIS + + +
        intrinsic ALOG10
!
!     + + + END SPECIFICATIONS + + +
!
        allocate (IPOINT(SIZE(DIS)))

        ID = 0
10      continue
!       loop for periods of good data
        NUMPT = 0
        GOODV = 0
        PFLAG = 0
20      continue
!         find start and end of good values
        ID = ID + 1
        if (DIS(ID) >= 0.0) then
!           good value
            GDIS(ID) = 0.0
            if (GOODV == 0) START = ID
            GOODV = GOODV + 1
            end = ID
        else
!           bad value
            if (GOODV == 0) then
!             no good values yet
                GDIS(ID) = DIS(ID)
            else if (GOODV < INTRVL) then
!             not enough good values to process
                do J = 1, GOODV
                    K = ID - J
                    GDIS(K) = -999.0
                end do
                GOODV = 0
            else
!             found good period to process
                PFLAG = 1
                GDIS(ID) = -999.0
            end if
        end if
        if (ID < NDAYS .AND. PFLAG == 0) GO TO 20
!
        if (GOODV >= INTRVL) then
!         have good period to process
            if (INTRVL == 3) then
                L = end - 1
                S = START + 1
                do I = S, L
                    if (DIS(I) <= DIS(I + 1) .AND. DIS(I) <= DIS(I - 1)) then
                        NUMPT = NUMPT + 1
                        IPOINT(NUMPT) = I
                    end if
                end do
            else if (INTRVL == 5) then
                L = end - 2
                S = START + 2
                do I = S, L
                    if (DIS(I) <= DIS(I + 1) .AND. DIS(I) <= DIS(I - 1) .AND. &
                        DIS(I) <= DIS(I + 2) .AND. DIS(I) <= DIS(I - 2)) then
                        NUMPT = NUMPT + 1
                        IPOINT(NUMPT) = I
                    end if
                end do
            else if (INTRVL == 7) then
                L = end - 3
                S = START + 3
                do I = S, L
                    if (DIS(I) <= DIS(I + 1) .AND. DIS(I) <= DIS(I + 2) .AND. &
                        DIS(I) <= DIS(I + 3) .AND. DIS(I) <= DIS(I - 1) .AND. &
                        DIS(I) <= DIS(I - 2) .AND. DIS(I) <= DIS(I - 3)) then
                        NUMPT = NUMPT + 1
                        IPOINT(NUMPT) = I
                    end if
                end do
            else if (INTRVL == 9) then
                L = end - 4
                S = START + 4
                do I = S, L
                    if (DIS(I) <= DIS(I + 1) .AND. DIS(I) <= DIS(I + 2) .AND. &
                        DIS(I) <= DIS(I + 3) .AND. DIS(I) <= DIS(I - 1) .AND. &
                        DIS(I) <= DIS(I - 2) .AND. DIS(I) <= DIS(I - 3) .AND. &
                        DIS(I) <= DIS(I + 4) .AND. DIS(I) <= DIS(I - 4)) then
                        NUMPT = NUMPT + 1
                        IPOINT(NUMPT) = I
                    end if
                end do
            else if (INTRVL >= 11) then
                L = end - 5
                S = START + 5
                do I = S, L
                    if (DIS(I) <= DIS(I + 1) .AND. DIS(I) <= DIS(I + 2) .AND. &
                        DIS(I) <= DIS(I + 3) .AND. DIS(I) <= DIS(I - 1) .AND. &
                        DIS(I) <= DIS(I - 2) .AND. DIS(I) <= DIS(I - 3) .AND. &
                        DIS(I) <= DIS(I + 4) .AND. DIS(I) <= DIS(I - 4) .AND. &
                        DIS(I) <= DIS(I + 5) .AND. DIS(I) <= DIS(I - 5)) then
                        NUMPT = NUMPT + 1
                        IPOINT(NUMPT) = I
                    end if
                end do
            end if
!
            if (NUMPT > 0) then
!           at least one local minimum found in good period being analyzed
                K = NUMPT - 1
                J = IPOINT(1)
                L = IPOINT(NUMPT)
!           set beginning values to first local minimum
                do IJ = START, J
                    GDIS(IJ) = DIS(J)
                end do
!           set ending values to last local minimum
                do IJ = L, end
                    GDIS(IJ) = DIS(L)
                end do
!           set all the values in the middle
                do I = 1, K
                    IP1 = IPOINT(I)
                    IP2 = IPOINT(I + 1)
                    GDIS(IP1) = DIS(IP1)
                    GDIS(IP2) = DIS(IP2)
                    ISTART = IP1
                    IEND = IP2
                    do J = ISTART, IEND
                        X = J - IP1
                        Y = IP2 - IP1
                        if (GDIS(IP1) <= 0.0) GDIS(IP1) = 0.01
                        if (GDIS(IP2) <= 0.0) GDIS(IP2) = 0.01
                        GDIS(J) = 10.**((X / Y) * (ALOG10(GDIS(IP2)) - &
                                                   ALOG10(GDIS(IP1))) + ALOG10(GDIS(IP1)))
                    end do
                end do
            else
!           no local minimum found in period analyzed
                do I = START, end
                    GDIS(I) = -999.0
                end do
            end if
        end if
        if (end < NDAYS .AND. ID < NDAYS) GO TO 10
!
        do IJK = 1, NDAYS
            if (GDIS(IJK) > DIS(IJK)) GDIS(IJK) = DIS(IJK)
        end do
!
        return
    end subroutine LOCMIN

    subroutine hydro_peaks(ifail)

! -- Subroutine hydro_peaks finds peaks for a hydrograph time series.

        implicit none

        integer, intent(out) :: ifail

        integer dd1, mm1, yy1, hh1, nn1, ss1, dd2, mm2, yy2, hh2, nn2, ss2, ierr, &
            icontext, i, begdays, begsecs, enddays, endsecs, iseries, &
            j, ibterm, ieterm, iterm, iiterm, itemp, ixcon, &
            ndays, nsecs, npts, nmax, tlast, vlast, &
            DA(12)
        real twindow, rtemp, tmin, slope1, slope2
        real, dimension(:), allocatable :: tdate, tval !rank 1
        character(len=iTSNAMELENGTH) :: aname
        character(15) aline
        character(25) aoption
        character(25) acontext(MAXCONTEXT)
        data DA/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/

        ifail = 0
        CurrentBlock_g = 'HYDRO_PEAKS'

        write (*, 10) TRIM(CurrentBlock_g)
        write (LU_REC, 10) TRIM(CurrentBlock_g)
10      format(/, ' Processing ', a, ' block....')

        icontext = 0
        iseries = 0
        yy1 = -9999
        hh1 = -9999
        yy2 = -9999
        hh2 = -9999
        twindow = 1
        tmin = 0
        aname = ' '
        ixcon = 0

! -- The HYDRO_PEAKS block is first parsed.

        do
            ILine_g = ILine_g + 1
            read (LU_TSPROC_CONTROL, '(a)', err=9000, end=9100) cline
            if (cline == ' ') cycle
            if (cline(1:1) == '#') cycle
            call linesplit(ierr, 2)
            if (ierr /= 0) then
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 20) TRIM(aline), TRIM(sString_g)
20              format('there should be 2 entries on line ', a, ' of file ', a)
                go to 9800
            end if
            aoption = cline(left_word(1):right_word(1))
            call casetrans(aoption, 'hi')
            if (aoption /= 'CONTEXT') then
                call test_context(ierr, icontext, acontext)
                if (ierr == -1) then
                    call find_end(ifail)
                    if (ifail == 1) go to 9800
                    return
                else if (ierr == 1) then
                    go to 9800
                end if
                ixcon = 1
            end if
            if (aoption == 'DATE_1') then
                call get_date(ierr, dd1, mm1, yy1, 'DATE_1')
                if (ierr /= 0) go to 9800
            else if (aoption == 'DATE_2') then
                call get_date(ierr, dd2, mm2, yy2, 'DATE_2')
                if (ierr /= 0) go to 9800
            else if (aoption == 'TIME_1') then
                call get_time(ierr, hh1, nn1, ss1, 'TIME_1')
                if (ierr /= 0) go to 9800
            else if (aoption == 'TIME_2') then
                call get_time(ierr, hh2, nn2, ss2, 'TIME_2')
                if (ierr /= 0) go to 9800
            else if (aoption == 'NEW_SERIES_NAME') then
                call get_new_series_name(ierr, aname)
                if (ierr /= 0) go to 9800
            else if (aoption == 'SERIES_NAME') then
                call get_series_name(ierr, iseries, 'SERIES_NAME')
                if (ierr /= 0) go to 9800
            else if (aoption == 'CONTEXT') then
                if (ixcon /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 41) TRIM(aline), TRIM(sString_g)
41                  format('Context keyword in incorrect location at line ', a, ' of file ', a)
                    go to 9800
                end if
                call get_context(ierr, icontext, acontext)
                if (ierr /= 0) go to 9800
            else if (aoption == 'WINDOW') then
                call get_keyword_value(ierr, 2, itemp, twindow, 'WINDOW')
                if (ierr /= 0) go to 9800
                if (twindow <= 0.0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 42) TRIM(aline), TRIM(sString_g)
42                  format('WINDOW must be greater than zero at line ', a, ' of file ', a)
                    go to 9800
                end if
            else if (aoption == 'MIN_PEAK') then
                call get_keyword_value(ierr, 2, itemp, tmin, 'MIN_PEAK')
                if (ierr /= 0) go to 9800
                if (tMIN < 0.0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 43) TRIM(aline), TRIM(sString_g)
43                  format('MIN_PEAK must not zero at line ', a, ' of file ', a)
                    go to 9800
                end if
            else if (aoption == 'END') then
                go to 200
            else
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 90) TRIM(aoption), TRIM(CurrentBlock_g), TRIM(aline), TRIM(sString_g)
90              format('unexpected keyword - "', a, '" in ', a, ' block at line ', a, &
                       ' of file ', a)
                go to 9800
            end if
        end do

! -- The block has been read; now it is checked for correctness.

200     continue
        if (iseries == 0) then
            write (amessage, 210) TRIM(CurrentBlock_g)
210         format('no SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (aname == ' ') then
            write (amessage, 230) TRIM(CurrentBlock_g)
230         format('no NEW_SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (icontext == 0) then
            write (amessage, 220) TRIM(CurrentBlock_g)
220         format('no Context_g keyword(s) provided in ', a, ' block.')
            go to 9800
        end if
        call date_check(ierr, yy1, mm1, dd1, hh1, nn1, ss1, yy2, mm2, dd2, hh2, nn2, ss2, &
                        begdays, begsecs, enddays, endsecs)
        if (ierr /= 0) go to 9800
        call beg_end_check(ierr, iseries, begdays, begsecs, enddays, endsecs)
        if (ierr /= 0) go to 9800

! -- All is well with the block. The requested peaks are found.

        call numterms(iterm, ibterm, ieterm, begdays, begsecs, enddays, endsecs, iseries)
        if (iterm == 0) then
            write (amessage, 270) TRIM(series_g(iseries)%name)
270         format('there are no terms in time series "', a, '" between the provided ', &
                   'dates and times.')
            go to 9800
        end if

        if ((begdays < series_g(iseries)%days(1)) .OR. &
            ((begdays == series_g(iseries)%days(1)) .AND. &
             (begsecs < series_g(iseries)%secs(1)))) then
            begdays = series_g(iseries)%days(1)
            begsecs = series_g(iseries)%secs(1)
        end if
        iiterm = series_g(iseries)%nterm
        if ((enddays > series_g(iseries)%days(iiterm)) .OR. &
            ((enddays == series_g(iseries)%days(iiterm)) .AND. &
             (endsecs > series_g(iseries)%secs(iiterm)))) then
            enddays = series_g(iseries)%days(iiterm)
            endsecs = series_g(iseries)%secs(iiterm)
        end if

        nmax = ieterm - ibterm + 1
        allocate (tdate(nmax), tval(nmax), stat=ierr)
        if (ierr /= 0) then
            write (amessage, 280)
280         format('cannot allocate temporary time series')
            goto 9800
        end if
        npts = 0
        do j = ibterm, ieterm
            ndays = series_g(iseries)%days(j)
            nsecs = series_g(iseries)%secs(j)
            rtemp = series_g(iseries)%val(j)
            npts = npts + 1
            tdate(npts) = ndays + nsecs / 86400
            tval(npts) = rtemp
        end do
        call alloc_tempseries(ierr, iiterm)
        if (ierr /= 0) then
            write (amessage, 280)
            goto 9800
        end if
        iterm = 0
        tlast = tdate(1)
        vlast = tval(1)
        do j = 2, npts - 2
            slope1 = (tval(j) - tval(j - 1)) / (tdate(j) - tdate(j - 1))
            slope2 = (tval(j + 1) - tval(j)) / (tdate(j + 1) - tdate(j))
            if (slope1 > 0.0) then
                if (slope2 <= 0.0) then
                    if ((tdate(j) - tlast) > twindow) then
                        if (tval(j) > tmin) then
                            iterm = iterm + 1
                            ndays = INT(tdate(j))
                            nsecs = 86400 * (tdate(j) - ndays)
                            tempseries_g%days(iterm) = ndays
                            tempseries_g%secs(iterm) = nsecs
                            tempseries_g%val(iterm) = tval(j)
                            tlast = tdate(j)
                            vlast = tval(j)
                        end if
                    else
                        if (tval(j) > vlast) then
                            if (iterm < 1) iterm = 1
                            ndays = INT(tdate(j))
                            nsecs = 86400 * (tdate(j) - ndays)
                            tempseries_g%days(iterm) = ndays
                            tempseries_g%secs(iterm) = nsecs
                            tempseries_g%val(iterm) = tval(j)
                            tlast = tdate(j)
                            vlast = tval(j)
                        end if
                    end if
                end if
            end if
        end do

! --  space is allocated for the new time series.

        do i = 1, MAXSERIES
            if (.NOT. series_g(i)%active) go to 380
        end do
        write (amessage, 370)
370     format('no more time series available for data storage - increase MAXSERIES and ', &
               'recompile program.')
        go to 9800

380     allocate (series_g(i)%days(iterm), series_g(i)%secs(iterm), &
                  series_g(i)%val(iterm), stat=ierr)
        if (ierr /= 0) then
            write (amessage, 390)
390         format('cannot allocate memory for another time series.')
            go to 9800
        end if

        series_g(i)%active = .TRUE.
        series_g(i)%name = aname
        series_g(i)%nterm = iterm
        series_g(i)%type = 'ts'

        do j = 1, iterm
            series_g(i)%days(j) = tempseries_g%days(j)
            series_g(i)%secs(j) = tempseries_g%secs(j)
            series_g(i)%val(j) = tempseries_g%val(j)
        end do

        write (6, 400) TRIM(series_g(iseries)%name), TRIM(aname)
        write (LU_REC, 400) TRIM(series_g(iseries)%name), TRIM(aname)
400     format(t5, 'Hydrograph peaks for time series "', a, '" stored in ', &
               'time series "', a, '".')
        return

9000    call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9010) TRIM(aline), TRIM(sString_g)
9010    format('cannot read line ', a, ' of TSPROC input file ', a)
        go to 9800
9100    continue
        call addquote(sInfile_g, sString_g)
        write (amessage, 9110) TRIM(sString_g), TRIM(CurrentBlock_g)
9110    format('unexpected end encountered to TSPROC input file ', a, ' while ', &
               ' reading ', a, ' block.')
        go to 9800

9800    call write_message(leadspace='yes', error='yes')
        call write_message(iunit=LU_REC, leadspace='yes')
        ifail = 1

        return

    end subroutine hydro_peaks

end module wsc_additions
