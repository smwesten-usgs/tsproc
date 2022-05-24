module tsp_command_processors

    interface get_keyword_value
        module procedure get_keyword_value_single
        module procedure get_keyword_value_double
    end interface

contains

!     Last change:  JD   22 Sep 2001    9:36 pm
    subroutine get_new_series_name(ifail, aname)

! -- Subroutine get_new_series_name retreives a new series name from a block in
!    the TSPROC input file.

        use tsp_data_structures
        use tsp_utilities

        implicit none

        integer, intent(out) :: ifail
        character(*), intent(out) :: aname
        integer ierr, nn, i
        character(20) atemp, aline

        ifail = 0
        call getfile(ierr, cline, atemp, left_word(2), right_word(2))
        if (ierr /= 0) then
            call num2char(ILine_g, aline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 71) TRIM(aline), TRIM(sString_g)
71          format('cannot read NEW_SERIES_NAME from line ', a, ' of file ', a)
            go to 9800
        end if
        nn = LEN_TRIM(atemp)
        if (nn > iTSNAMELENGTH) then
            call num2char(ILine_g, aline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 77) TRIM(atemp), iTSNAMELENGTH, TRIM(aline), TRIM(sString_g)
77          format('series name "', a, '" greater than ', i3, ' characters at line ', a, &
                   ' of file ', a)
            go to 9800
        end if
!       aname=atemp(1:10)
        aname = TRIM(ADJUSTL(atemp))
        if (isspace(aname)) then
            call num2char(ILine_g, aline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 81) TRIM(aname), TRIM(aline), TRIM(sString_g)
81          format('space character in NEW_SERIES_NAME "', a, '" at line ', a, ' of file ', a)
            go to 9800
        end if
        call casetrans(aname, 'lo')
        write (*, 74) TRIM(aname)
        write (LU_REC, 74) TRIM(aname)
74      format(t5, 'NEW_SERIES_NAME ', a)
        do i = 1, MAXSERIES
            if (series_g(i)%active) then
                if (str_compare(series_g(i)%name, aname)) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 68) TRIM(aname), TRIM(aline), TRIM(sString_g)
68                  format('the name "', a, '" at line ', a, ' of file ', a, ' is already used by ', &
                           'another active series.')
                    go to 9800
                end if
            end if
        end do

        do i = 1, MAXSTABLE
            if (stable_g(i)%active) then
                if (str_compare(stable_g(i)%name, aname)) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 61) TRIM(aname), TRIM(aline), TRIM(sString_g)
61                  format('the name "', a, '" at line ', a, ' of file ', a, ' is already used by ', &
                           'an active s_table.')
                    go to 9800
                end if
            end if
        end do

        do i = 1, MAXGTABLE
            if (gtable_g(i)%active) then
                if (str_compare(gtable_g(i)%name, aname)) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 79) TRIM(aname), TRIM(aline), TRIM(sString_g)
79                  format('the name "', a, '" at line ', a, ' of file ', a, ' is already used by ', &
                           'an active g_table.')
                    go to 9800
                end if
            end if
        end do

        do i = 1, MAXVTABLE
            if (vtable_g(i)%active) then
                if (str_compare(vtable_g(i)%name, aname)) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 69) TRIM(aname), TRIM(aline), TRIM(sString_g)
69                  format('the name "', a, '" at line ', a, ' of file ', a, ' is already used by ', &
                           'an active v_table.')
                    go to 9800
                end if
            end if
        end do

        do i = 1, MAXDTABLE
            if (dtable_g(i)%active) then
                if (str_compare(dtable_g(i)%name, aname)) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 66) TRIM(aname), TRIM(aline), TRIM(sString_g)
66                  format('the name "', a, '" at line ', a, ' of file ', a, ' is already used by ', &
                           'an active e_table.')
                    go to 9800
                end if
            end if
        end do

        return

9800    ifail = 1
        return

    end subroutine get_new_series_name

    subroutine get_file_name(ifail, afile)

! -- Subroutine READFILE retreives a file name from a data block on
!    the TSPROC input file.

        use tsp_data_structures
        use tsp_utilities

        implicit none

        integer, intent(out) :: ifail
        character(*), intent(out) :: afile
        integer ierr
        character(20) aline

        ifail = 0
        call getfile(ierr, cline, afile, left_word(2), right_word(2))
        if (ierr /= 0) then
            call num2char(ILine_g, aline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 30) TRIM(aline), TRIM(sString_g)
30          format('cannot read filename from line ', a, ' of file ', a)
            go to 9800
        end if
        call addquote(afile, sString_g)
        write (*, 40) TRIM(sString_g)
        write (LU_REC, 40) TRIM(sString_g)
40      format(t5, 'FILE ', a)
        return

9800    ifail = 1
        return

    end subroutine get_file_name

    subroutine get_date(ifail, dd1, mm1, yy1, alabel)

! -- Subroutine get_date reads a date from a block of the TSPROC input file.

        use tsp_data_structures
        use tsp_utilities

        implicit none

        integer, intent(out) :: ifail
        integer, intent(inout) :: dd1, mm1, yy1
        character(*), intent(in) :: alabel

        integer ierr
        character(20) aline, adate

        ifail = 0
        adate = cline(left_word(2):right_word(2))
        call char2date(ierr, adate, dd1, mm1, yy1)
        if (ierr /= 0) then
            call num2char(ILine_g, aline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 60) TRIM(aline), TRIM(sString_g)
60          format('illegal date at line ', a, ' of TSPROC input file ', a)
            go to 9800
        end if
        write (*, 65) TRIM(alabel), TRIM(adate)
        write (LU_REC, 65) TRIM(alabel), TRIM(adate)
65      format(t5, a, ' ', a)
        return

9800    ifail = 1
        return

    end subroutine get_date

    subroutine get_time(ifail, hh1, nn1, ss1, alabel)

! -- Subroutine get_time reads a time from a block of the TSPROC input file.

        use tsp_data_structures
        use tsp_utilities

        implicit none

        integer, intent(out) :: ifail
        integer, intent(inout) :: hh1, nn1, ss1
        character*(*), intent(in) :: alabel

        integer ierr
        character(20) aline, atime

        ifail = 0
        atime = cline(left_word(2):right_word(2))
        call char2time(ierr, atime, hh1, nn1, ss1, ignore_24=1)
        if (ierr /= 0) then
            call num2char(ILine_g, aline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 70) TRIM(aline), TRIM(sString_g)
70          format('illegal time at line ', a, ' of TSPROC input file ', a)
            go to 9800
        end if
        write (*, 75) TRIM(alabel), TRIM(atime)
        write (LU_REC, 75) TRIM(alabel), TRIM(atime)
75      format(t5, a, ' ', a)
        return

9800    ifail = 1
        return

    end subroutine get_time

    subroutine get_context(ifail, icontext, acontext)

! -- Subroutine get_context reads a Context_g string from a block of the
!    TSPROC input file.

        use tsp_data_structures
        use tsp_utilities

        implicit none

        integer, intent(out) :: ifail
        integer, intent(inout) :: icontext
        character*(*), intent(inout) :: acontext(MAXCONTEXT)

        integer ierr
        character(20) aline, aaline

        ifail = 0

        icontext = icontext + 1
        if (icontext > MAXCONTEXT) then
            call num2char(MAXCONTEXT, aline)
            call num2char(ILine_g, aaline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 69) TRIM(aline), TRIM(aaline), TRIM(sString_g)
69          format('maximum of ', a, ' CONTEXTs can be supplied in a ', &
                   'TSPROC block. Violation at line ', a, ' of file ', a)
            go to 9800
        end if
        call getfile(ierr, cline, acontext(icontext), left_word(2), right_word(2))
        if (ierr /= 0) then
            call num2char(ILine_g, aline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 78) TRIM(aline), TRIM(sString_g)
78          format('cannot read context from line ', a, ' of file ', a)
            go to 9800
        end if
        if (isspace(acontext(icontext))) then
            call num2char(ILine_g, aline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 82) TRIM(acontext(icontext)), TRIM(aline), TRIM(sString_g)
82          format('space character in context string "', a, '" at line ', a, ' of file ', a)
            go to 9800
        end if
        call casetrans(acontext(icontext), 'lo')
        write (*, 79) TRIM(acontext(icontext))
        write (LU_REC, 79) TRIM(acontext(icontext))
79      format(t5, 'context ', a)
        return

9800    ifail = 1
        return

    end subroutine get_context

    subroutine get_series_name(ifail, iseries, aword)

! -- Subroutine get_series_name reads a series name from a block of the
!    TSPROC input file. It is assumed that the series name represents an already
!    active time series.

        use tsp_data_structures
        use tsp_utilities

        implicit none

        integer, intent(out) :: ifail
        integer, intent(inout) :: iseries

        integer ierr, nn, i
        character(len=iTSNAMELENGTH) :: aname
        character(20) aline, atemp
        character * (*) aword

        ifail = 0

        call getfile(ierr, cline, atemp, left_word(2), right_word(2))
        if (ierr /= 0) then
            call num2char(ILine_g, aline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 110) TRIM(aline), TRIM(sString_g)
110         format('cannot read series name from line ', a, ' of file ', a)
            go to 9800
        end if
        nn = LEN_TRIM(atemp)
        if (nn > iTSNAMELENGTH) then
            call num2char(ILine_g, aline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 120) TRIM(atemp), iTSNAMELENGTH, TRIM(aline), TRIM(sString_g)
120         format('series name "', a, '" greater than ', i3, ' characters at line ', a, &
                   ' of file ', a)
            go to 9800
        end if
!       aname=atemp(1:10)
        aname = TRIM(ADJUSTL(atemp))
        if (isspace(aname)) then
            call num2char(ILine_g, aline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 121) TRIM(aname), TRIM(aline), TRIM(sString_g)
121         format('space character in series name "', a, '" at line ', a, ' of file ', a)
            go to 9800
        end if
        call casetrans(aname, 'lo')
        write (*, 140) TRIM(aword), TRIM(aname)
        write (LU_REC, 140) TRIM(aword), TRIM(aname)
140     format(t5, a, ' ', a)
        do i = 1, MAXSERIES
            if (.NOT. series_g(i)%active) cycle

            if (str_compare(series_g(i)%name, aname)) then
                iseries = i
                go to 130
            end if
        end do
        call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 160) TRIM(aname), TRIM(aline), TRIM(sString_g)
160     format('series name "', a, '" at line ', a, ' of TSPROC input file ', a, &
               ' has not been read or calculated, or has been erased.')
        go to 9800
130     continue
        return

9800    ifail = 1
        return

    end subroutine get_series_name

    subroutine get_table_name(ifail, itable, jtype)

! -- Subroutine get_table_name reads a table name from a block of the
!    TSPROC input file. It is assumed that the table name represents an already
!    active table.

        use tsp_data_structures
        use tsp_utilities

        implicit none

        integer, intent(out) :: ifail
        integer, intent(inout) :: itable
        integer, intent(in) :: jtype

        integer ierr, nn, i, itype
        character(len=iTSNAMELENGTH) :: aname
        character(20) aline, atemp

        ifail = 0
        itype = jtype

        do
            if (itype < 10) exit
            itype = itype - 10
        end do

        call getfile(ierr, cline, atemp, left_word(2), right_word(2))
        if (ierr /= 0) then
            call num2char(ILine_g, aline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 110) TRIM(aline), TRIM(sString_g)
110         format('cannot read table name from line ', a, ' of file ', a)
            go to 9800
        end if
        nn = LEN_TRIM(atemp)
        if (nn > iTSNAMELENGTH) then
            call num2char(ILine_g, aline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 120) TRIM(atemp), iTSNAMELENGTH, TRIM(aline), TRIM(sString_g)
120         format('table name "', a, '" greater than ', i3, ' characters at line ', a, &
                   ' of file ', a)
            go to 9800
        end if
!       aname=atemp(1:10)
        aname = TRIM(ADJUSTL(atemp))
        if (isspace(aname)) then
            call num2char(ILine_g, aline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 121) TRIM(aname), TRIM(aline), TRIM(sString_g)
121         format('space character in table name "', a, '" at line ', a, ' of file ', a)
            go to 9800
        end if
        call casetrans(aname, 'lo')
        if (jtype == 1) then
            write (*, 140) TRIM(aname)
            write (LU_REC, 140) TRIM(aname)
140         format(t5, 'S_TABLE_NAME ', a)

        else if (jtype == 2) then
            write (*, 141) TRIM(aname)
            write (LU_REC, 141) TRIM(aname)
141         format(t5, 'V_TABLE_NAME ', a)

        else if (jtype == 3) then
            write (*, 142) TRIM(aname)
            write (LU_REC, 142) TRIM(aname)
142         format(t5, 'E_TABLE_NAME ', a)

        else if (jtype == 4) then
            write (*, 151) TRIM(aname)
            write (LU_REC, 151) TRIM(aname)
151         format(t5, 'C_TABLE_NAME ', a)

        else if (jtype == iG_TABLE) then
            write (*, 157) TRIM(aname)
            write (LU_REC, 157) TRIM(aname)
157         format(t5, 'G_TABLE_NAME ', a)

        else if (jtype == 11) then
            write (*, 143) TRIM(aname)
            write (LU_REC, 143) TRIM(aname)
143         format(t5, 'OBSERVATION_S_TABLE_NAME ', a)

        else if (jtype == 12) then
            write (*, 144) TRIM(aname)
            write (LU_REC, 144) TRIM(aname)
144         format(t5, 'OBSERVATION_V_TABLE_NAME ', a)

        else if (jtype == 13) then
            write (*, 145) TRIM(aname)
            write (LU_REC, 145) TRIM(aname)
145         format(t5, 'OBSERVATION_E_TABLE_NAME ', a)

        else if (jtype == 15) then
            write (*, 146) TRIM(aname)
            write (LU_REC, 146) TRIM(aname)
146         format(t5, 'OBSERVATION_G_TABLE_NAME ', a)

        else if (jtype == 21) then
            write (*, 147) TRIM(aname)
            write (LU_REC, 147) TRIM(aname)
147         format(t5, 'MODEL_S_TABLE_NAME ', a)

        else if (jtype == 22) then
            write (*, 148) TRIM(aname)
            write (LU_REC, 148) TRIM(aname)
148         format(t5, 'MODEL_V_TABLE_NAME ', a)

        else if (jtype == 23) then
            write (*, 149) TRIM(aname)
            write (LU_REC, 149) TRIM(aname)
149         format(t5, 'MODEL_E_TABLE_NAME ', a)

        else if (jtype == 25) then
            write (*, 150) TRIM(aname)
            write (LU_REC, 150) TRIM(aname)
150         format(t5, 'MODEL_G_TABLE_NAME ', a)

        end if
        if (itype == 1) then
            do i = 1, MAXSTABLE
                if (.NOT. stable_g(i)%active) cycle
                if (str_compare(stable_g(i)%name, aname)) then
                    itable = i
                    go to 130
                end if
            end do
            call num2char(ILine_g, aline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 160) TRIM(aname), TRIM(aline), TRIM(sString_g)
160         format('S_TABLE "', a, '" cited at line ', a, ' of file ', a, &
                   ' has not been read or calculated, or has been erased.')
            go to 9800
130         continue

        else if (itype == 2) then
            do i = 1, MAXVTABLE
                if (.NOT. vtable_g(i)%active) cycle
                if (str_compare(vtable_g(i)%name, aname)) then
                    itable = i
                    go to 131
                end if
            end do
            call num2char(ILine_g, aline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 161) TRIM(aname), TRIM(aline), TRIM(sString_g)
161         format('V_TABLE "', a, '" cited at line ', a, ' of file ', a, &
                   ' has not been read or calculated, or has been erased.')
            go to 9800
131         continue

        else if (itype == 3) then
            do i = 1, MAXDTABLE
                if (.NOT. dtable_g(i)%active) cycle
                if (str_compare(dtable_g(i)%name, aname)) then
                    itable = i
                    go to 132
                end if
            end do
            call num2char(ILine_g, aline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 162) TRIM(aname), TRIM(aline), TRIM(sString_g)
162         format('E_TABLE "', a, '" cited at line ', a, ' of file ', a, &
                   ' has not been read or calculated, or has been erased.')
            go to 9800
132         continue

        else if (itype == 4) then
            do i = 1, MAXCTABLE
                if (.NOT. ctable_g(i)%active) cycle
                if (str_compare(ctable_g(i)%name, aname)) then
                    itable = i
                    go to 172
                end if
            end do
            call num2char(ILine_g, aline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 171) TRIM(aname), TRIM(aline), TRIM(sString_g)
171         format('C_TABLE "', a, '" cited at line ', a, ' of file ', a, &
                   ' has not been read or calculated, or has been erased.')
            go to 9800
172         continue

        else if (itype == iG_TABLE) then
            do i = 1, MAXGTABLE
                if (.NOT. gtable_g(i)%active) cycle
                if (str_compare(gtable_g(i)%name, aname)) then
                    itable = i
                    go to 192
                end if
            end do
            call num2char(ILine_g, aline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 191) TRIM(aname), TRIM(aline), TRIM(sString_g)
191         format('G_TABLE "', a, '" cited at line ', a, ' of file ', a, &
                   ' has not been read or calculated, or has been erased.')
            go to 9800
192         continue

        end if
        return

9800    ifail = 1
        return

    end subroutine get_table_name

    subroutine date_check(ifail, yy1, mm1, dd1, hh1, nn1, ss1, yy2, mm2, dd2, hh2, nn2, ss2, &
                          begdays, begsecs, enddays, endsecs)

! -- Subroutine DATE_CHECK checks the integrity of date and time information
!    supplied through a TSPROC data block.

        use tsp_data_structures
        use tsp_utilities

        implicit none

        integer, intent(out) :: ifail
        integer, intent(inout) :: yy1, mm1, dd1, hh1, nn1, ss1, &
                                  yy2, mm2, dd2, hh2, nn2, ss2
        integer, intent(out) :: begdays, begsecs, enddays, endsecs

        integer nn, ss

        ifail = 0

        if (yy1 == -9999) then
            if (hh1 /= -9999) then
                write (amessage, 530) TRIM(CurrentBlock_g)
530             format('TIME_1 specifier provided, but no DATE_1 specifier in ', a, ' block.')
                go to 9800
            end if
        end if
        if (yy2 == -9999) then
            if (hh2 /= -9999) then
                write (amessage, 540) TRIM(CurrentBlock_g)
540             format('TIME_2 specifier provided, but no DATE_2 specifier in ', a, ' block.')
                go to 9800
            end if
        end if
        if (hh1 == -9999) then
!         hh1=0
            hh1 = 24
            nn1 = 0
            ss1 = 0
        end if
        if (hh2 == -9999) then
!         hh2=0
            hh2 = 24
            nn2 = 0
            ss2 = 0
        end if
        if ((yy1 /= -9999) .AND. (yy2 /= -9999)) then
            nn = numdays(dd1, mm1, yy1, dd2, mm2, yy2)
            ss = numsecs(hh1, nn1, ss1, hh2, nn2, ss2)
            if ((nn < 0) .OR. ((nn == 0) .AND. (ss <= 0))) then
                write (amessage, 570) TRIM(CurrentBlock_g)
570             format('first date/time does not precede second date/time in ', a, ' block.')
                go to 9800
            end if
        end if
        if (yy1 /= -9999) then
            begdays = julian_day(iYear=yy1, &
                                 iMonth=mm1, &
                                 iDay=dd1)
!         begdays=numdays(1,1,1970,dd1,mm1,yy1)
        else
            begdays = -99999999
        end if
        begsecs = numsecs(0, 0, 0, hh1, nn1, ss1)
571     if (begsecs >= 86400) then
            begsecs = begsecs - 86400
            begdays = begdays + 1
            go to 571
        end if
        if (yy2 /= -9999) then
!         enddays=numdays(1,1,1970,dd2,mm2,yy2)

            enddays = julian_day(iYear=yy2, &
                                 iMonth=mm2, &
                                 iDay=dd2)

        else
            enddays = 99999999
        end if
        endsecs = numsecs(0, 0, 0, hh2, nn2, ss2)
572     if (endsecs >= 86400) then
            endsecs = endsecs - 86400
            enddays = enddays + 1
            go to 572
        end if
        if ((begdays == enddays) .AND. (begsecs == endsecs)) then
            write (amessage, 570) TRIM(CurrentBlock_g)
            go to 9800
        end if

        return

9800    ifail = 1
        return

    end subroutine date_check

    subroutine test_context(ifail, icontext, acontext)

! -- Subroutine TEST_CONTEXT checks whether the context is such that processing
!    should continue.

        use tsp_data_structures
        use tsp_utilities

        implicit none

        integer, intent(out) :: ifail
        integer, intent(in) :: icontext
        character*(*), intent(in) :: acontext(icontext)

        integer i
        character(len=iTSNAMELENGTH) :: aline

        ifail = 0
        if (icontext == 0) then
            call num2char(ILine_g, aline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 10) TRIM(aline), TRIM(sString_g)
10          format('one of more CONTEXT keywords should lead any TSPROC block at ', &
                   'line ', a, ' of file ', a)
            ifail = 1
            return
        end if
        do i = 1, icontext
            if (acontext(i) == context_g) go to 174
            if (acontext(i) == 'all') go to 174
        end do
        write (*, 175)
        write (LU_REC, 175)
175     format(t5, 'Requested actions not undertaken because no CONTEXT option in the')
        write (*, 176)
        write (LU_REC, 176)
176     format(t5, ' block coincides with the current run context.')
        ifail = -1
        return

174     continue
        return

    end subroutine test_context

    subroutine alloc_tempseries(ifail, iterm)

! -- Subroutine ALLOC_TEMPSERIES allocates (or re-allocates) memory for the temporary
!    time series.

        use tsp_data_structures
        use tsp_utilities

        implicit none

        integer, intent(out) :: ifail
        integer, intent(in) :: iterm

        integer ierr

        ifail = 0

        if (.NOT. tempseries_g%active) then
            allocate (tempseries_g%days(iterm), tempseries_g%secs(iterm), &
                      tempseries_g%val(iterm), stat=ierr)
            if (ierr /= 0) then
                write (amessage, 340)
340             format('cannot allocate sufficient memory to continue execution.')
                go to 9800
            end if
            tempseries_g%nterm = iterm
            tempseries_g%active = .TRUE.
        else
            if (iterm /= tempseries_g%nterm) then
                deallocate (tempseries_g%days, tempseries_g%secs, tempseries_g%val, stat=ierr)
                if (ierr /= 0) then
                    write (amessage, 355)
355                 format('error #1 in expanding memory allocation for temporary time series.')
                    go to 9800
                end if
!           nullify(tempseries_g%days,tempseries_g%secs,tempseries_g%val)
                allocate (tempseries_g%days(iterm), tempseries_g%secs(iterm), &
                          tempseries_g%val(iterm), stat=ierr)
                if (ierr /= 0) then
                    write (amessage, 360)
360                 format('cannot expand memory allocation for temporary time series')
                    go to 9800
                end if
                tempseries_g%nterm = iterm
            end if
        end if
        return

9800    ifail = 1
        return

    end subroutine alloc_tempseries

    subroutine get_yes_no(ifail, iyesno)

! -- Subroutine GET_YES_NO reads "yes" or "no" from a TSPROC input file.

        use tsp_data_structures
        use tsp_utilities

        implicit none

        integer, intent(out) :: ifail
        integer, intent(inout) :: iyesno

        integer ierr
        character(len=iTSNAMELENGTH) :: atemp, aline
        character(3) ayesno

        ifail = 0
        call getfile(ierr, cline, atemp, left_word(2), right_word(2))
        if (ierr /= 0) go to 9000
        if (LEN_TRIM(atemp) > 3) go to 9000
        ayesno = atemp(1:3)
        call casetrans(ayesno, 'lo')
        if (ayesno == 'yes') then
            iyesno = 1
        else if (ayesno == 'no') then
            iyesno = 0
        else
            go to 9000
        end if
        return

9000    ifail = 1
        call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9010) TRIM(aline), TRIM(sString_g)
9010    format('"yes" or "no" expected as the second entry at line ', a, ' of file ', a)
        return

    end subroutine

    subroutine beg_end_check(ifail, iseries, begdays, begsecs, enddays, endsecs)

! -- Subroutine BEG_END_CHECK checks that DATE_1, DATE_2, TIME_1 and TIME_2
!    are compatible with a given time series.

        use tsp_data_structures
        use tsp_utilities

        implicit none

        integer, intent(out) :: ifail
        integer, intent(in) :: iseries, begdays, begsecs, enddays, endsecs

        integer jterm

        ifail = 0

        jterm = series_g(iseries)%nterm
        if ((begdays > series_g(iseries)%days(jterm)) .OR. &
            ((begdays == series_g(iseries)%days(jterm)) .AND. &
             (begsecs > series_g(iseries)%secs(jterm)))) then
            write (amessage, 240) TRIM(series_g(iseries)%name)
240         format('DATE_1 and TIME_1 postdate the end of time series "', a, '".')
            go to 9800
        end if
        if ((enddays < series_g(iseries)%days(1)) .OR. &
            ((enddays == series_g(iseries)%days(1)) .AND. &
             (endsecs < series_g(iseries)%secs(1)))) then
            write (amessage, 250) TRIM(series_g(iseries)%name)
250         format('DATE_2 and TIME_2 precede the start of time series "', a, '".')
            go to 9800
        end if

        return

9800    ifail = 1
        return

    end subroutine beg_end_check

    subroutine numterms(iterm, ibterm, ieterm, begdays, begsecs, enddays, endsecs, iseries)

! -- Subroutine NUMTERMS counts the number of terms in a series between two dates
!    and times. {Also: returns ibterm and ieterm}

!
! begdays, begsecs serve as means to subset the data
! enddays, endsecs serve as means to subset the data
!
! if no DATE_1, DATE_2 values are supplied by the user, the default
! begdays = -99999998, and enddays = 100000000
!

        use tsp_data_structures
        use tsp_utilities

        implicit none

        integer, intent(in) :: iseries, begdays, begsecs, enddays, endsecs
        integer, intent(out) :: iterm, ibterm, ieterm

        integer i, jterm

        ibterm = 0
        ieterm = 0
        iterm = 0
        jterm = series_g(iseries)%nterm
        do i = 1, jterm
            if ((begdays > series_g(iseries)%days(i)) .OR. &
                ((begdays == series_g(iseries)%days(i)) .AND. &
                 (begsecs > series_g(iseries)%secs(i)))) cycle
            if (ibterm == 0) ibterm = i
            if ((enddays < series_g(iseries)%days(i)) .OR. &
                ((enddays == series_g(iseries)%days(i)) .AND. &
                 (endsecs < series_g(iseries)%secs(i)))) go to 300
            iterm = iterm + 1
        end do
        ieterm = jterm
        go to 310
300     ieterm = i - 1
310     continue

        return

    end subroutine numterms

    subroutine time_interp_s(ifail, nbore, ndays, nsecs, value, intday, intsec, &
                             rnear, rconst, valinterp, extrap, direction, startindex)

! -- Subroutine time_interp_s interpolates an array of times and values to a
!    user-supplied time. It is slightly modified from time_interp, the only
!    change being that the value array is now single precision, as is the
!    interpolated value.

! -- Arguments are as follows:-
!       ifail:     returned as zero unless an error condition arises
!       nbore:     number of times and corresponding values to be interpolated
!       ndays:     elapsed days corresponding to each value
!       nsecs:     elpased seconds corresponding to each value
!       value:     array of time-based values to be interpolated
!       intday:    the day to be interpolated to, expressed as elapsed days
!       intsec:    the time to be intepolated to, expressed as elapsed seconds
!       rnear:     maximum permitted days to nearest sample
!       rconst:    maximum days to nearest sample if interpolation cannot take
!                  place
!       valinterp: interpolated value
!       extrap:    'yes' if use linear extrapolation to (within rconst) if
!                  interpolation cannot take place
!       direction: 'lo' if extrapolation from two previous points,
!                  'hi' if extrapolation from two following points,
!                  'med' if interpolation if possible, otherwise extrapolation
!                  (note: 'med' is the default)
!       startindex: index of bore index at which to start the search through the
!                   table

        use tsp_data_structures
        use tsp_utilities

        implicit none

        integer, intent(out) :: ifail
        integer, intent(in) :: nbore
        integer, intent(in), dimension(nbore) :: ndays, nsecs
        real, intent(in), dimension(nbore) :: value
        integer, intent(in) :: intday, intsec
        real, intent(in) :: rnear, rconst
        real, intent(out) :: valinterp
        character(len=*), intent(in), optional :: extrap
        character(len=*), intent(in), optional :: direction
        integer, intent(inout), optional :: startindex

        integer :: i, ie, id, istart, is
        double precision :: secfac, diff, diff1, dentime
        character(len=3) :: atemp

        ie = 0
        if (PRESENT(extrap)) then
            atemp = extrap
            call casetrans(atemp, 'lo')
            if (atemp == 'yes') then
                ie = 1
            else if (atemp == 'no') then
                ie = 0
            else
                call sub_error('TIME_INTERP_S')
            end if
        end if

        id = 0
        if (PRESENT(direction)) then
            atemp = direction
            call casetrans(atemp, 'lo')
            if (atemp == 'lo') then
                id = -1
            else if (atemp == 'hi') then
                id = 1
            else if (atemp == 'med') then
                id = 0
            else
                call sub_error('TIME_INTERP')
            end if
        end if

        if ((id /= 0) .AND. (ie == 0)) then
            call sub_error('TIME_INTERP')
        end if

        if (PRESENT(startindex)) then
            istart = startindex
            is = 1
        else
            istart = 1
            is = 0
        end if
        if (istart < 1) istart = 1
        if (istart > nbore - 1) istart = nbore - 1

        ifail = 0
        secfac = 1.0D0 / 86400.0D0
        if (nbore == 1) then
            diff = DBLE(intday - ndays(1)) + DBLE(intsec - nsecs(1)) * secfac
            if (ABS(diff) <= rconst) then
                valinterp = value(1)
            else
                if (diff > 0) then
                    valinterp = -9.1E37
                else
                    valinterp = -8.1E37
                end if
            end if
            return
        end if

!  do i=1,nbore-1
!    if((ndays(i).gt.ndays(i+1)).or. &
!      ((ndays(i).eq.ndays(i+1)).and.(nsecs(i).ge.nsecs(i+1))))then
!      ifail=1
!      return
!    end if
!  end do

        do i = istart, nbore
            diff = DBLE(ndays(i) - intday) + DBLE(nsecs(i) - intsec) * secfac
            if (diff >= 0) then
                if (i == 1) then
                    if (diff <= rconst) then
                        if (ie == 1) then
                            if ((value(1) < -1.0E38) .OR. (value(2) < -1.0E38)) then
                                valinterp = value(1)
                            else
                                dentime = DBLE(ndays(i + 1) - ndays(i)) + &
                                          DBLE(nsecs(i + 1) - nsecs(i)) * secfac
                                if (dentime <= 0) then
                                    ifail = 1
                                    go to 9000
                                else
                                    valinterp = value(i) - (value(i + 1) - value(i)) * diff / dentime
                                end if
                            end if
                        else
                            valinterp = value(1)
                        end if
                    else
                        valinterp = -8.1E37
                    end if
                    go to 9000
                end if

                if (id == -1) then
                    if (i == 2) then
                        diff1 = DBLE(intday - ndays(1)) + DBLE(intsec - nsecs(1)) * secfac
                        if (diff1 > rnear) then !note - not rconst
                            valinterp = -7.1E37
                        else
                            valinterp = value(1)
                        end if
                        if (value(1) < -1.0E38) valinterp = -1.1E38
                    else
                        dentime = DBLE(ndays(i - 1) - ndays(i - 2)) + &
                                  DBLE(nsecs(i - 1) - nsecs(i - 2)) * secfac
                        if (dentime < 0.0D0) then
                            ifail = 1
                            go to 9000
                        else
                            diff1 = DBLE(intday - ndays(i - 1)) + &
                                    DBLE(intsec - nsecs(i - 1)) * secfac
                            if (diff1 > rnear) then
                                valinterp = -7.1E37
                            else
                                if (value(i - 1) < -1.0E38) then
                                    valinterp = -1.1E38
                                else if (value(i - 2) < -1.0E38) then
                                    valinterp = value(i - 1)
                                else
                                    valinterp = value(i - 1) + &
                                                (value(i - 1) - value(i - 2)) / dentime * diff1
                                end if
                            end if
                        end if
                    end if
                    go to 9000
                else if (id == 1) then
                    if (i == nbore) then
                        if (diff > rnear) then
                            valinterp = -7.1E37
                        else
                            valinterp = value(i)
                        end if
                        if (value(i) < -1.0E38) valinterp = -1.1E38
                    else
                        dentime = DBLE(ndays(i + 1) - ndays(i)) + &
                                  DBLE(nsecs(i + 1) - nsecs(i)) * secfac
                        if (dentime <= 0) then
                            ifail = 1
                            go to 9000
                        else
                            if (diff > rnear) then
                                valinterp = -7.1E37
                            else
                                if (value(i) < -1.0E38) then
                                    valinterp = -1.1E38
                                else if (value(i + 1) < -1.0E38) then
                                    valinterp = value(i)
                                else
                                    valinterp = value(i) - &
                                                (value(i + 1) - value(i)) / dentime * diff
                                end if
                            end if
                        end if
                    end if
                    go to 9000
                else

                    dentime = DBLE(ndays(i) - ndays(i - 1)) + &
                              DBLE(nsecs(i) - nsecs(i - 1)) * secfac
                    if (dentime <= 0) then
                        ifail = 1
                        go to 9000
                    else
                        diff1 = dentime - diff
                        if ((diff1 > rnear) .AND. (diff > rnear)) then
                            valinterp = -7.1E37
                        else
                            valinterp = value(i - 1) + (value(i) - value(i - 1)) / dentime * diff1
                        end if
                        if (value(i) < -1.0E38) then
                            if (diff1 <= rconst) then
                                valinterp = value(i - 1)
                            else
                                valinterp = -1.1E38
                            end if
                        else if (value(i - 1) < -1.0E38) then
                            if (diff <= rconst) then
                                valinterp = value(i)
                            else
                                valinterp = -1.1E38
                            end if
                        end if
                    end if
                    go to 9000
                end if
            end if
        end do

        diff1 = DBLE(intday - ndays(nbore)) + DBLE(intsec - nsecs(nbore)) * secfac
        if (diff1 <= rconst) then
            if (ie == 1) then
                if ((value(nbore) < -1.0E38) .OR. (value(nbore - 1) < -1.0E38)) then
                    valinterp = value(nbore)
                else
                    dentime = DBLE(ndays(nbore) - ndays(nbore - 1)) &
                              + DBLE(nsecs(nbore) - nsecs(nbore - 1)) * secfac
                    if (dentime <= 0) then
                        ifail = 1
                        go to 9000
                    else
                        valinterp = value(nbore) + (value(nbore) - value(nbore - 1)) * &
                                    diff1 / dentime
                    end if
                end if
            else
                valinterp = value(nbore)
            end if
        else
            valinterp = -9.1E37
        end if

9000    continue
        if (is == 1) then
            if (ifail == 0) then
                startindex = i
            else
                startindex = 0
            end if
        end if

        return
    end subroutine time_interp_s

    subroutine get_new_table_name(ifail, itype, aname)

! -- Subroutine READ_NEW_TABLE_NAME reads the name of a new table.

        use tsp_data_structures
        use tsp_utilities

        implicit none

        integer, intent(out) :: ifail
        integer, intent(in) :: itype
        character*(*), intent(out) :: aname

        integer ierr, nn, i
        character(20) aline, atemp

        ifail = 0

        call getfile(ierr, cline, atemp, left_word(2), right_word(2))
        if (ierr /= 0) then
            call num2char(ILine_g, aline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 71) TRIM(aline), TRIM(sString_g)
71          format('cannot read new table name from line ', a, ' of file ', a)
            go to 9800
        end if
        nn = LEN_TRIM(atemp)
        if (nn > iTSNAMELENGTH) then
            call num2char(ILine_g, aline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 77) TRIM(atemp), iTSNAMELENGTH, TRIM(aline), TRIM(sString_g)
77          format('table name "', a, '" greater than ', i3, ' characters at line ', a, &
                   ' of file ', a)
            go to 9800
        end if
!       aname=atemp(1:10)
        aname = TRIM(ADJUSTL(atemp))
        if (isspace(aname)) then
            call num2char(ILine_g, aline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 81) TRIM(aname), TRIM(aline), TRIM(sString_g)
81          format('space character in new table name "', a, '" at line ', a, ' of file ', a)
            go to 9800
        end if
        call casetrans(aname, 'lo')

        if (itype == iS_TABLE) then

            write (*, 74) TRIM(aname)
            write (LU_REC, 74) TRIM(aname)
74          format(t5, 'NEW_S_TABLE_NAME ', a)

        else if (itype == iV_TABLE) then

            write (*, 75) TRIM(aname)
            write (LU_REC, 75) TRIM(aname)
75          format(t5, 'NEW_V_TABLE_NAME ', a)

        else if (itype == iE_TABLE) then

            write (*, 76) TRIM(aname)
            write (LU_REC, 76) TRIM(aname)
76          format(t5, 'NEW_E_TABLE_NAME ', a)

        else if (itype == iC_TABLE) then

            write (*, 78) TRIM(aname)
            write (LU_REC, 78) TRIM(aname)
78          format(t5, 'NEW_C_TABLE_NAME ', a)

        else if (itype == iG_TABLE) then

            write (*, 80) TRIM(aname)
            write (LU_REC, 80) TRIM(aname)
80          format(t5, 'NEW_G_TABLE_NAME ', a)

        end if

!       if(itype.eq.1)then
        do i = 1, MAXSTABLE
            if (stable_g(i)%active) then
                if (str_compare(stable_g(i)%name, aname)) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 68) TRIM(aname), TRIM(aline), TRIM(sString_g)
68                  format('the name "', a, '" at line ', a, ' of file ', a, ' is already used ', &
                           'by an active s_table.')
                    go to 9800
                end if
            end if
        end do
!       else if(itype.eq.2)then
        do i = 1, MAXVTABLE
            if (vtable_g(i)%active) then
                if (str_compare(vtable_g(i)%name, aname)) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 69) TRIM(aname), TRIM(aline), TRIM(sString_g)
69                  format('the name "', a, '" at line ', a, ' of file ', a, ' is already used ', &
                           'by an active v_table.')
                    go to 9800
                end if
            end if
        end do
!       else if(itype.eq.3)then
        do i = 1, MAXDTABLE
            if (dtable_g(i)%active) then
                if (str_compare(dtable_g(i)%name, aname)) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 66) TRIM(aname), TRIM(aline), TRIM(sString_g)
66                  format('the name "', a, '" at line ', a, ' of file ', a, ' is already used ', &
                           'by an active e_table.')
                    go to 9800
                end if
            end if
        end do
!       else if(itype.eq.4)then
        do i = 1, MAXCTABLE
            if (ctable_g(i)%active) then
                if (str_compare(ctable_g(i)%name, aname)) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 67) TRIM(aname), TRIM(aline), TRIM(sString_g)
67                  format('the name "', a, '" at line ', a, ' of file ', a, ' is already used ', &
                           'by an active c_table.')
                    go to 9800
                end if
            end if
        end do
!       else if(itype.eq.5)then
        do i = 1, MAXGTABLE
            if (gtable_g(i)%active) then
                if (str_compare(gtable_g(i)%name, aname)) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 97) TRIM(aname), TRIM(aline), TRIM(sString_g)
97                  format('the name "', a, '" at line ', a, ' of file ', a, ' is already used ', &
                           'by an active g_table.')
                    go to 9800
                end if
            end if
        end do

        do i = 1, MAXSERIES
            if (series_g(i)%active) then
                if (str_compare(series_g(i)%name, aname)) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 63) TRIM(aname), TRIM(aline), TRIM(sString_g)
63                  format('the name "', a, '" at line ', a, ' of file ', a, ' is already used ', &
                           'by an active series.')
                    go to 9800
                end if
            end if
        end do

        return

9800    ifail = 1
        return

    end subroutine get_new_table_name

    subroutine get_time_units(ifail, itunit, itype)

! -- Subroutine get_time_UNITS reads time units from a TSPROC input file.

        use tsp_data_structures
        use tsp_utilities

        implicit none

        integer, intent(out) :: ifail
        integer, intent(out) :: itunit
        integer, intent(in) :: itype

        integer ierr
        character(len=iTSNAMELENGTH) :: aunit
        character(20) aline, atemp

        ifail = 0

        call getfile(ierr, cline, atemp, left_word(2), right_word(2))
        if (ierr /= 0) go to 9000
        call casetrans(atemp, 'lo')
        if (atemp(1:4) == 'year') then
            itunit = 6
            aunit = 'years'
        else if (atemp(1:5) == 'month') then
            itunit = 5
            aunit = 'months'
        else if (atemp(1:3) == 'day') then
            itunit = 4
            aunit = 'days'
        else if (atemp(1:4) == 'hour') then
            itunit = 3
            aunit = 'hours'
        else if (atemp(1:3) == 'min') then
            itunit = 2
            aunit = 'minutes'
        else if (atemp(1:3) == 'sec') then
            itunit = 1
            aunit = 'seconds'
        else
            go to 9000
        end if
        if (itype == 1) then
            write (*, 120) TRIM(aunit)
            write (LU_REC, 120) TRIM(aunit)
120         format(t5, 'FLOW_TIME_UNITS ', a)
        else if (itype == 2) then
            write (*, 121) TRIM(aunit)
            write (LU_REC, 120) TRIM(aunit)
121         format(t5, 'EXCEEDANCE_TIME_UNITS ', a)
        end if
        return

9000    call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9010) TRIM(aline), TRIM(sString_g)
9010    format('cannot read time units from line ', a, ' of file ', a)
        go to 9800

9800    ifail = 1
        return

    end subroutine get_time_units

    subroutine volume_interp_s(ifail, num, days, secs, flows, bdays, bsecs, fdays, fsecs, vol, fac)
! -- Subroutine volume_interp calculates the extracted volume between two dates
!    on the basis of flow rates recorded at certain times. It is a modified version
!    of volume_interp such that the input time series does not need to be double precision.

! -- Arguments are as follows:-
!      num:    number of flow rate samples
!      days:   julian date pertaining to each flow sample
!      secs:   seconds since midnight pertaining to each flow sample
!      bdays:  julian date at which volume begins accumulating
!      bsecs:  seconds since midnight at which volume begins accumulating
!      fdays:  julian date at which volume ceases accumulating
!      fsecs:  seconds since midnight at which volume ceases accumulating
!      vol:    accumulated volume
!      fac:    factor by which to multiply volumes

        use tsp_data_structures
        use tsp_utilities

        implicit none

        integer, intent(out) :: ifail
        integer, intent(in) :: num
        integer, intent(in) :: days(num), secs(num)
        real, intent(in) :: flows(num)
        integer, intent(in) :: bdays, bsecs, fdays, fsecs
        real(kind=T_DBL), intent(out) :: vol
        real(kind=T_DBL), intent(in) :: fac

        integer i, ndd, j, ndt
        double precision tdd, tdt, m, volb, volm, volf

        ifail = 0

! It is assumed that the following checks have already been made.
!       if(bdays.lt.days(1)) go to 9000
!       if(bdays.eq.days(1))then
!         if(bsecs.lt.secs(1)) go to 9000
!       end if
!       if(bdays.gt.days(num)) go to 9100
!       if(bdays.eq.days(num))then
!         if(bsecs.gt.secs(num)) go to 9100
!       end if
!       if(fdays.gt.days(num)) go to 9200
!       if(fdays.eq.days(num))then
!         if(fsecs.gt.secs(num)) go to 9200
!       end if

! -- The volume in the first interval is calculated.

        do i = 2, num
            if ((days(i) > bdays) .OR. &
                ((days(i) == bdays) .AND. (secs(i) >= bsecs))) go to 50
        end do
        go to 9100
50      ndd = days(i) - days(i - 1)
        ndt = days(i) - bdays
        tdd = DBLE(ndd) * 86400.0D0 + DBLE(secs(i) - secs(i - 1))
        tdt = DBLE(ndt) * 86400.0D0 + DBLE(secs(i) - bsecs)
        if (tdd <= 0.0D0) go to 9400
        m = (flows(i) - flows(i - 1)) / tdd
        volb = tdt * (flows(i) - m * 0.5D0 * tdt)

! -- Now we traverse sample intervals until we find the last interval

        volm = 0.0D0
        j = i
        do i = j, num

            ! Have we reached the end of the interval of interest? If so, GOTO 100
            if ((days(i) > fdays) .OR. &
                ((days(i) == fdays) .AND. (secs(i) >= fsecs))) go to 100
            if (i /= j) then
                ndd = days(i) - days(i - 1)
                tdd = DBLE(ndd) * 86400.0D0 + DBLE(secs(i) - secs(i - 1))
                volm = volm + 0.5 * (flows(i) + flows(i - 1)) * tdd
            end if
        end do
        go to 9200
100     ndd = days(i) - days(i - 1)
        ndt = fdays - days(i - 1)
        tdd = DBLE(ndd) * 86400.0D0 + DBLE(secs(i) - secs(i - 1))
        tdt = DBLE(ndt) * 86400.0D0 + DBLE(fsecs - secs(i - 1))
        if (tdd == 0.0D0) go to 9400
        m = (flows(i) - flows(i - 1)) / tdd
        volf = tdt * (flows(i - 1) + m * 0.5D0 * tdt)
        if (i == j) then
            ndd = days(i) - days(i - 1)
            tdd = DBLE(ndd) * 86400.0D0 + DBLE(secs(i) - secs(i - 1))
            volm = -0.5 * (flows(i) + flows(i - 1)) * tdd
        end if

        vol = (volb + volm + volf) * fac / 86400.0D0
        return

!9000   vol=-5.1e37
!       go to 9999
9100    vol = -4.1E37
        go to 9999
9200    vol = -3.1E37
        go to 9999
9400    ifail = 1
        go to 9999

9999    return

    end subroutine volume_interp_s

    subroutine get_keyword_value_single(ifail, itype, ival, rval, aword)

! -- Subroutine GET_KEYWORD_VALUE retreives a keyword value from a TSPROC input file.

        use tsp_data_structures
        use tsp_utilities

        implicit none

        integer, intent(out) :: ifail
        integer, intent(in) :: itype
        integer, intent(inout) :: ival
        real, intent(inout) :: rval
        character*(*), intent(in) :: aword

        integer ierr
        character(20) aline

        ifail = 0
        if (itype == 1) then
            call char2num(ierr, cline(left_word(2):right_word(2)), ival)
        else
            call char2num(ierr, cline(left_word(2):right_word(2)), rval)
        end if
        if (ierr /= 0) then
            call num2char(ILine_g, aline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 120) TRIM(aword), TRIM(aline), TRIM(sString_g)
120         format('cannot read ', a, ' from line ', a, ' of file ', a)
            go to 9800
        end if
        if (itype == 1) then
            call num2char(ival, aline)
        else
            call num2char(rval, aline, 9)
        end if
        write (*, 130) TRIM(aword), TRIM(aline)
        write (LU_REC, 130) TRIM(aword), TRIM(aline)
130     format(t5, a, ' ', a)
        return

9800    ifail = 1
        return

    end subroutine get_keyword_value_single

    subroutine get_keyword_value_double(ifail, itype, ival, rval, aword)

! -- Subroutine GET_KEYWORD_VALUE_DOUBLE retreives a keyword value from a TSPROC input file.
!    It differs from GET_KEYWORD_VALUE in that the value read from the input file
!    can be double precision. This is a little rough. I should have modified
!    GET_KEYWORD_VALUE and put in an optional argumment. But it is quick.

        use tsp_data_structures
        use tsp_utilities

        implicit none

        integer, intent(out) :: ifail
        integer, intent(in) :: itype
        integer, intent(inout) :: ival
        double precision, intent(inout) :: rval
        character*(*), intent(in) :: aword

        integer ierr
        character(20) aline

        ifail = 0
        if (itype == 1) then
            call char2num(ierr, cline(left_word(2):right_word(2)), ival)
        else
            call char2num(ierr, cline(left_word(2):right_word(2)), rval)
        end if
        if (ierr /= 0) then
            call num2char(ILine_g, aline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 120) TRIM(aword), TRIM(aline), TRIM(sString_g)
120         format('cannot read ', a, ' from line ', a, ' of file ', a)
            go to 9800
        end if
        if (itype == 1) then
            call num2char(ival, aline)
        else
            call num2char(rval, aline, 9)
        end if
        write (*, 130) TRIM(aword), TRIM(aline)
        write (LU_REC, 130) TRIM(aword), TRIM(aline)
130     format(t5, a, ' ', a)
        return

9800    ifail = 1
        return

    end subroutine get_keyword_value_double

    subroutine get_equation(ifail, eqntext, atext)

        use tsp_data_structures
        use tsp_utilities

        implicit none

        integer, intent(out) :: ifail
        character*(*), intent(out) :: eqntext
        character*(*), intent(in) :: atext

        integer ncl, lnxx, idx, lnx, ixcount
        character(1) aa
        character(20) aline

        ifail = 0
        aa = cline(left_word(2):left_word(2))
        if ((aa == '"') .OR. (aa == '''')) then
            cline(left_word(2):left_word(2)) = ' '
            ncl = LEN_TRIM(cline)
            aa = cline(ncl:ncl)
            if ((aa /= '"') .AND. (aa /= '''')) go to 9200
!         call getfile(ierr,cline,eqntext,left_word(2),ncl)
!         if(ierr.ne.0) go to 9200
            cline(ncl:ncl) = ' '
            eqntext = cline(left_word(2) + 1:)
            eqntext = ADJUSTL(eqntext)
        else
            eqntext = cline(left_word(2):)
!         ncl=len_trim(eqntext)
!         aa=eqntext(ncl:ncl)
!         if((aa.eq.'''').or.(aa.eq.'"')) go to 9200
        end if
        call casetrans(eqntext, 'lo')
        write (*, 40) TRIM(atext), TRIM(eqntext)
        write (LU_REC, 40) TRIM(atext), TRIM(eqntext)
40      format(t5, a, ' "', a, '"')

! -- Before control is returned, "~" is substituted for "/" in the
!    @_days_"dd/mm/yyyy_hh:mm:ss" function.

        lnxx = LEN_TRIM(eqntext)
        idx = 1
241     continue
        lnx = INDEX(eqntext(idx:), '@_days_ ')
        if (lnx /= 0) go to 9500
        lnx = INDEX(eqntext(idx:), '@_days_')
        if (lnx == 0) go to 249
        idx = idx + lnx - 1
        idx = idx + 7
        if (eqntext(idx:idx + 9) == 'start_year') then
            go to 241
        else if ((eqntext(idx:idx) == '"') .OR. (eqntext(idx:idx) == '''')) then
            ixcount = 0
243         idx = idx + 1
            if (idx > lnxx) go to 9500
            if ((eqntext(idx:idx) == '"') .OR. (eqntext(idx:idx) == '''')) then
                if (ixcount /= 2) go to 9500
                go to 241
            end if
            if (eqntext(idx:idx) == '/') then
                ixcount = ixcount + 1
                if (ixcount > 2) go to 9500
                eqntext(idx:idx) = CHAR(196)
            end if
            go to 243
        else
            go to 9500
        end if
249     continue

        return

9200    call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9210) TRIM(aline), TRIM(sString_g)
9210    format('cannot read equation from line ', a, ' of TSPROC input file ', a)
        ifail = 1
        return

9500    write (amessage, 9510)
9510    format('illegal "@_days_" function in weights equation.')
        ifail = 1
        return

    end subroutine get_equation

    subroutine get_two_numbers(ifail, rnum1, rnum2, atext)

        use tsp_data_structures
        use tsp_utilities

        implicit none

        integer, intent(out) :: ifail
        real, intent(out) :: rnum1, rnum2
        character*(*), intent(in) :: atext

        integer ierr
        character(13) anum1, anum2

        ifail = 0
        cline = cline(left_word(2):)
        call linesplit(ierr, 2)
        if (ierr /= 0) go to 9000
        call char2num(ierr, cline(left_word(1):right_word(1)), rnum1)
        if (ierr /= 0) go to 9000
        call char2num(ierr, cline(left_word(2):right_word(2)), rnum2)
        if (ierr /= 0) go to 9000
        call num2char(rnum1, anum1, 9)
        call num2char(rnum2, anum2, 9)
        write (*, 40) TRIM(atext), TRIM(anum1), TRIM(anum2)
        write (LU_REC, 40) TRIM(atext), TRIM(anum1), TRIM(anum2)
40      format(t5, a, ' ', a, '   ', a)
        return

9000    continue
        call num2char(ILine_g, anum1)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9010) TRIM(atext), TRIM(anum1), TRIM(sString_g)
9010    format('cannot read two numbers following ', a, ' keyword on line ', a, ' of file ', a)
        ifail = 1
        return

    end subroutine get_two_numbers

    subroutine check_weight_order(ifail, rmin, rmax)

        use tsp_data_structures
        use tsp_utilities

        implicit none

        integer, intent(out) :: ifail
        real, intent(in) :: rmin, rmax
        character(15) aline

        ifail = 0

        if (rmax <= rmin) then
            call num2char(ILine_g, aline)
            call addquote(sInfile_g, sString_g)
            write (amessage, 10) TRIM(aline), TRIM(sString_g)
10          format('minimum exceeds maximum at line ', a, ' of file ', a)
            ifail = 1
        end if

        return

    end subroutine check_weight_order

    subroutine remchar(sString_g, ach)

        implicit none

        character*(*), intent(inout) :: sString_g
        character*(*), intent(in) :: ach

        integer ll, ii, icount

        icount = 0
        ll = LEN_TRIM(ach)

10      ii = INDEX(sString_g, ach)
        if (ii == 0) then
            if (icount == 0) return
            go to 20
        end if
        icount = icount + 1
        sString_g(ii:ii - 1 + ll) = ' '
        go to 10

20      sString_g = ADJUSTL(sString_g)
        return

    end subroutine remchar

    subroutine make_basename(ifail, iout, nsterm, aname, basename)

! -- Subroutine MAKE_BASENAME formulates the basename of observation names pertaining
!    to a SERIES, V_TABLE or E_TABLE.

        use tsp_data_structures
        use tsp_utilities

        implicit none

        integer, intent(out) :: ifail
        integer, intent(in) :: iout, nsterm
        character*(*), intent(in) :: aname
        character*(*), intent(inout) :: basename(MAXSERIES + MAXVTABLE + MAXDTABLE)

        integer ndig, baselen, itemp, j
        character(len=iTSNAMELENGTH) :: atemp

        ifail = 0

        call num2char(nsterm, atemp)
        ndig = LEN_TRIM(atemp)
        baselen = 19 - ndig
        itemp = LEN_TRIM(aname)
        if (itemp < baselen) baselen = itemp
        basename(iout) = aname(1:baselen)//OBSCHAR
        if (iout > 1) then
            do j = 1, iout - 1
                if (basename(iout) == basename(j)) then
                    write (amessage, 10)
10                  format('TSPROC cannot generate unique observation names from the ', &
                           'names of the model SERIES, V_TABLES, and E_TABLES involved in the ', &
                           'calibration process. Alter the first few letters of the longest ', &
                           'of these names, or of those SERIES with the most terms.')
                    go to 9800
                end if
            end do
        end if
        return

9800    ifail = 1
        return

    end subroutine make_basename

    subroutine find_end(ifail)

! -- Subroutine FIND_END locates the end of a block when it is out of context.

        use tsp_data_structures
        use tsp_utilities

        implicit none

        integer, intent(out) :: ifail
        integer ierr
        character(15) aline
        character(30) aoption

        ifail = 0
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
            if (aoption == 'END') return
        end do

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

9800    ifail = 1
        return

    end subroutine find_end

    subroutine get_next_block(ifail)

! -- Subroutine get_next_block obtains the header to the next section of
!    the TSPROC input file.

        use tsp_data_structures
        use tsp_utilities

        implicit none

        integer, intent(out) :: ifail

        integer ierr
        character(15) aline

        ifail = 0
        call addquote(sInfile_g, sString_g)
        do
            ILine_g = ILine_g + 1
            read (LU_TSPROC_CONTROL, '(a)', err=9000, end=500) cline
            if (cline == ' ') cycle
            if (cline(1:1) == '#') cycle
            cline = ADJUSTL(cline)

            call linesplit(ierr, 2)
            if (ierr /= 0) then
                call num2char(ILine_g, aline)
                write (amessage, 5) TRIM(aline), TRIM(sString_g)
5               format('two entries expected on line ', a, ' of file ', a)
                go to 9800
            end if

            sCurrentBlockName = cline(left_word(1):right_word(1))
            call casetrans(sCurrentBlockName, 'hi')

            if (sCurrentBlockName /= 'START') then
                call num2char(ILine_g, aline)
                write (amessage, 7) TRIM(aline), TRIM(sString_g)
7               format('first item on line ', a, ' of file ', a, ' expected to be START.')

                go to 9800
            end if
            sCurrentBlockName = cline(left_word(2):right_word(2))
            call casetrans(sCurrentBlockName, 'hi')

            if (sCurrentBlockName == 'SETTINGS') then
                iBlockNumber = iGET_SETTINGS

            else if (sCurrentBlockName == 'GET_SERIES_WDM') then
                iBlockNumber = iGET_WDM_SERIES

            else if (sCurrentBlockName == 'GET_SERIES_SSF') then
                iBlockNumber = iGET_SSF_SERIES

            else if (sCurrentBlockName == 'GET_SERIES_PLOTGEN' &
                     .OR. sCurrentBlockName == 'GET_MUL_SERIES_PLOTGEN') then
                iBlockNumber = iGET_PLT_SERIES

            else if (sCurrentBlockName == 'GET_SERIES_TETRAD') then
                iBlockNumber = iGET_MUL_SERIES_TETRAD

            else if (sCurrentBlockName == 'GET_MUL_SERIES_SSF') then
                iBlockNumber = iGET_MUL_SERIES_SSF

            else if (sCurrentBlockName == 'GET_SERIES_UFORE_HYDRO') then
                iBlockNumber = iGET_UFORE_SERIES

            else if (sCurrentBlockName == 'GET_MUL_SERIES_GSFLOW_GAGE' &
                     .OR. sCurrentBlockName == 'GET_SERIES_GSFLOW_GAGE') then
                iBlockNumber = iGET_MUL_SERIES_GSFLOW_GAGE

            else if (sCurrentBlockName == 'GET_MUL_SERIES_STATVAR' &
                     .OR. sCurrentBlockName == 'GET_SERIES_STATVAR') then
                iBlockNumber = iGET_MUL_SERIES_STATVAR

            else if (sCurrentBlockName == 'LIST_OUTPUT') then
                iBlockNumber = iWRITE_LIST_OUTPUT

            else if (sCurrentBlockName == 'ERASE_ENTITY') then
                iBlockNumber = iERASE_ENTITY

            else if (sCurrentBlockName == 'REDUCE_TIME_SPAN') then
                iBlockNumber = iREDUCE_SPAN

            else if (sCurrentBlockName == 'SERIES_STATISTICS') then
                iBlockNumber = iSERIES_STATISTICS

            else if (sCurrentBlockName == 'SERIES_COMPARE') then
                iBlockNumber = iSERIES_COMPARE

            else if (sCurrentBlockName == 'NEW_TIME_BASE') then
                iBlockNumber = iNEW_TIME_BASE

            else if (sCurrentBlockName == 'VOLUME_CALCULATION') then
                iBlockNumber = iVOLUME_CALCULATION

            else if (sCurrentBlockName == 'EXCEEDENCE_TIME') then
                iBlockNumber = iEXCEEDANCE_TIME

            else if (sCurrentBlockName == 'EXCEEDANCE_TIME') then
                iBlockNumber = iEXCEEDANCE_TIME

            else if (sCurrentBlockName == 'FLOW_DURATION') then
                iBlockNumber = iFLOW_DURATION

            else if (sCurrentBlockName == 'SERIES_EQUATION') then
                iBlockNumber = iSERIES_EQUATION

            else if (sCurrentBlockName == 'SERIES_DISPLACE') then
                iBlockNumber = iSERIES_DISPLACE

            else if (sCurrentBlockName == 'SERIES_CLEAN') then
                iBlockNumber = iSERIES_CLEAN

            else if (sCurrentBlockName == 'DIGITAL_FILTER') then
                iBlockNumber = iDIGITAL_FILTER

            else if (sCurrentBlockName == 'SERIES_BASE_LEVEL') then
                iBlockNumber = iSERIES_BASE_LEVEL

            else if (sCurrentBlockName == 'V_TABLE_TO_SERIES') then
                iBlockNumber = iVOL_TABLE_TO_SERIES

            else if (sCurrentBlockName == 'MOVING_MINIMUM') then
                iBlockNumber = iMOVING_MINIMUM

            else if (sCurrentBlockName == 'NEW_SERIES_UNIFORM') then
                iBlockNumber = iNEW_SERIES_UNIFORM

            else if (sCurrentBlockName == 'SERIES_DIFFERENCE') then
                iBlockNumber = iSERIES_DIFFERENCE

            else if (sCurrentBlockName == 'PERIOD_STATISTICS') then
                iBlockNumber = iPERIOD_STATISTICS

            else if (sCurrentBlockName == 'USGS_HYSEP') then
                iBlockNumber = iUSGS_HYSEP

            else if (sCurrentBlockName == 'HYDRO_PEAKS') then
                iBlockNumber = iHYDRO_PEAKS

            else if (sCurrentBlockName == 'HYDRO_EVENTS') then
                iBlockNumber = iHYDRO_EVENTS

            else if (sCurrentBlockName == 'HYDROLOGIC_INDICES') then
                iBlockNumber = iHYDROLOGIC_INDICES

            else if (sCurrentBlockName == 'WRITE_PEST_FILES') then
                iBlockNumber = iWRITE_PEST_FILES

            else
                call num2char(ILine_g, aline)
                write (amessage, 10) TRIM(sCurrentBlockName), TRIM(aline), TRIM(sString_g)
10              format(' Unrecognised block title "', a, '" at line ', a, &
                       ' of TSPROC input file ', a)
                call write_message(leadspace='yes')
                call write_message(iunit=LU_REC, leadspace='yes')
                ifail = 1
                return
            end if
!         go to 400
            exit
        end do

!400    continue
        NumProcBloc_g = NumProcBloc_g + 1
        if (iBlockNumber == iGET_SETTINGS) then
            if (IProcSetting_g /= 0) then
                write (amessage, 525) TRIM(sString_g)
525             format('file ', a, ' contains two SETTINGS blocks.')
                go to 9800
            else if (numprocBloc_g /= 1) then
                write (amessage, 520)
520             format('SETTINGS block must be the first block in a TSPROC input file.')
                go to 9800
            end if
        else
            if (IProcSetting_g /= 1) then
                write (amessage, 530) TRIM(sString_g)
530             format('a SETTINGS block must lead all other blocks in TSPROC ', &
                       'input file ', a)
                go to 9800
            end if
        end if

        return

500     continue
        if (numprocbloc_g == 0) then
            write (amessage, 550) TRIM(sString_g)
550         format(' No blocks found in TSPROC input file ', a)
        else
            write (amessage, 540) TRIM(sString_g)
540         format(' End of TSPROC input file ', A)
            write (amessage, *) '- no more blocks to process.'
        end if
        call write_message(leadspace='yes')
        call write_message(iunit=LU_REC, leadspace='yes')
        ifail = -1
        return

9000    call num2char(ILine_g, aline)
        write (amessage, 9010) TRIM(aline), TRIM(sString_g)
9010    format('unable to read line ', a, ' of file ', a)
        go to 9800

9800    call write_message(leadspace='yes', error='yes')
        call write_message(iunit=LU_REC, leadspace='yes')
        ifail = 1

        return
    end subroutine get_next_block

    subroutine Process_Settings(ifail)

! -- Subroutine ProcessSettings reads a SETTINGS segment from a TSPROC input file.

        use tsp_data_structures
        use tsp_utilities

        implicit none

        integer, intent(out) :: ifail

        integer ierr
        character(15) aline
        character(25) aoption, datestr

        ifail = 0

        write (*, 10)
        write (LU_REC, 10)
10      format(/' Processing SETTINGS block....')

        datestr = ' '
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

            if ((aoption == 'CONTEXT')) then
                if (context_g /= ' ' .AND. (LEN_TRIM(sContextOverride_g) == 0)) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 12) TRIM(aline), TRIM(sString_g)
12                  format('only one context keyword is allowed in a SETTINGS block. ', &
                           'The second at line ', a, ' of file ', a, ' is illegal.')
                    go to 9800
                end if
                call getfile(ierr, cline, Context_g, left_word(2), right_word(2))
                if (ierr /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 23) TRIM(aline), TRIM(sString_g)
23                  format('cannot read context from line ', a, ' of file ', a)
                    go to 9800
                end if
                if (isspace(Context_g)) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 21) TRIM(Context_g), TRIM(aline), TRIM(sString_g)
21                  format('space character in context name "', a, '" at line ', a, ' of file ', a)
                    go to 9800
                end if
                call casetrans(Context_g, 'lo')
                write (*, 25) TRIM(Context_g)
                write (LU_REC, 25) TRIM(Context_g)
25              format(t5, 'context ', a)

            else if (aoption == 'DATE_FORMAT') then
                call getfile(ierr, cline, datestr, left_word(2), right_word(2))
                if (ierr /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 140) TRIM(aline), TRIM(sString_g)
140                 format('cannot read date format string from line ', a, ' of file ', a)
                    go to 9800
                end if
                call casetrans(datestr, 'lo')
                if ((datestr(1:2) == 'dd') .AND. (datestr(4:5) == 'mm')) then
                    datespec = 1
                else if ((datestr(1:2) == 'mm') .AND. (datestr(4:5) == 'dd')) then
                    datespec = 2
                else
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 140) TRIM(aline), TRIM(sString_g)
                    go to 9800
                end if
                if (datespec == 1) then
                    write (*, 141)
                    write (LU_REC, 141)
141                 format(t5, 'DATE_FORMAT dd/mm/yyyy')
                else
                    write (*, 142)
                    write (LU_REC, 142)
142                 format(t5, 'DATE_FORMAT mm/dd/yyyy')
                end if

            else if (aoption == 'END') then
                go to 100
            else
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 30) TRIM(aoption), TRIM(aline), TRIM(sString_g)
30              format('unexpected keyword - "', a, '" in SETTINGS block at line ', a, &
                       ' of file ', a)
                go to 9800
            end if
        end do

100     continue

        if ((context_g == ' ') .AND. (LEN_TRIM(sContextOverride_g) == 0)) then
            call addquote(sInfile_g, sString_g)
            write (amessage, 19) TRIM(sString_g)
19          format('the SETTINGS block in file ', a, ' does not contain ', &
                   'a Context keyword, and no context was provided at the command line.')
            go to 9800
        end if

        if (datestr == ' ') then
            call addquote(sInfile_g, sString_g)
            write (amessage, 18) TRIM(sString_g)
18          format('the SETTINGS block in file ', a, ' does not contain a ', &
                   'DATE_FORMAT keyword.')
            go to 9800
        end if

        if (LEN_TRIM(sContextOverride_g) > 0) then
            context_g = TRIM(sContextOverride_g)
            if (isspace(Context_g)) then
                write (amessage, 41) TRIM(Context_g)
41              format('space character in context name "', a, '"; context entered at the command line')
                go to 9800
            end if

            write (*, 42) TRIM(context_g)
            write (LU_REC, 42) TRIM(context_g)
42          format(/, t5, '*** Context has been overridden from the command line; CONTEXT = "', a, '" ***',/)

        end if

        IProcSetting_g = 1
        write (*, 120)
120     format(t5, 'Processing of SETTINGS block complete.')
        write (LU_REC, 120)
        return

9000    continue
        call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9010) TRIM(aline), TRIM(sString_g)
9010    format('cannot read line ', a, ' of TSPROC input file ', a)
        go to 9800
9100    continue
        call addquote(sInfile_g, sString_g)
        write (amessage, 9110) TRIM(sString_g)
9110    format('unexpected end encountered to TSPROC input file ', a, &
               ' while reading SETTINGS block.')
        go to 9800

9800    call write_message(leadspace='yes', error='yes')
        call write_message(iunit=LU_REC, leadspace='yes')
        ifail = 1
        return

    end subroutine process_settings

end module tsp_command_processors
