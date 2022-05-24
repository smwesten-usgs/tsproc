module tsp_equation_parser

    use tsp_data_structures
    use tsp_utilities
    use tsp_command_processors

    implicit none

contains

!     Last change:  JD    4 Sep 2001    3:59 pm
    subroutine equation(ifail)

! -- Subroutine EQUATION evaluates a relationship between time series.
        implicit none

        integer, intent(out) :: ifail

        integer icontext, ierr, ieqn, ncl, nterm, i, nser, isnum, j, iser, k, nsterm, iterm, nnterm, &
            nn, ixcon, ddx, mmx, yyx, hhx, nnx, ssx, idx, nex, sex, lnx, lnxx, ixcount
        integer serno(maxeqnser)
        real rtime
        double precision dval, dtempx
        character(1) aa
        character(len=iTSNAMELENGTH) :: aname
        character(15) aline
        character(25) aoption, adate_atime
        character(300) eqntext
        character(25) acontext(MAXCONTEXT)

        ifail = 0
        CurrentBlock_g = 'SERIES_EQUATION'

        write (*, 10) TRIM(CurrentBlock_g)
        write (LU_REC, 10) TRIM(CurrentBlock_g)
10      format(/, ' Processing ', a, ' block....')

        icontext = 0
        aname = ' '
        ieqn = 0
        ixcon = 0

! -- The SERIES_EQUATION block is first parsed.

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
            if (aoption == 'NEW_SERIES_NAME') then
                call get_new_series_name(ierr, aname)
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
            else if (aoption == 'END') then
                go to 200
            else if (aoption == 'EQUATION') then
                ieqn = ieqn + 1
                if (ieqn > 1) then
                    write (amessage, 30) TRIM(CurrentBlock_g)
30                  format('only one EQUATION keyword can appear in a ', a, ' block.')
                    go to 9800
                end if
                aa = cline(left_word(2):left_word(2))
                if ((aa == '"') .OR. (aa == '''')) then
                    cline(left_word(2):left_word(2)) = ' '
                    ncl = LEN_TRIM(cline)
                    aa = cline(ncl:ncl)
                    if ((aa /= '"') .AND. (aa /= '''')) go to 9200
                    cline(ncl:ncl) = ' '
                    eqntext = cline(left_word(2) + 1:)
                    eqntext = ADJUSTL(eqntext)
                else
                    eqntext = cline(left_word(2):)
!             ncl=len_trim(eqntext)
!             aa=eqntext(ncl:ncl)
!             if((aa.eq.'''').or.(aa.eq.'"')) go to 9200
                end if
                call casetrans(eqntext, 'lo')
                write (*, 40) TRIM(eqntext)
                write (LU_REC, 40) TRIM(eqntext)
40              format(t5, 'EQUATION "', a, '"')
            else
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 90) TRIM(aoption), TRIM(CurrentBlock_g), TRIM(aline), TRIM(sString_g)
90              format('unexpected keyword - "', a, '" in ', a, ' block at line ', a, &
                       ' of file ', a)
                go to 9800
            end if
        end do
200     continue

! The data supplied in the block is now checked for absences.

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
        if (ieqn == 0) then
            write (amessage, 240) TRIM(CurrentBlock_g)
240         format('no EQUATION keyword provided in ', a, ' block.')
            go to 9800
        end if

! -- The equation is now analysed for correctness. First it is parsed.
! -- But first the @_days_"dd/mm/yy_hh/nn/ss" function is given special treatment.

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
                eqntext(idx:idx) = '~'
            end if
            go to 243
        else
            go to 9500
        end if

249     continue
        call parse(ierr, maxterm, nterm, noper, eqntext, aterm, &
                   bterm, nfunct, funct, operat, rterm, 0)
        if (ierr /= 0) go to 9800
        call series_sub(ierr, NTERM, 0)
        if (ierr /= 0) go to 9800

! -- The series cited in the equation are now collected.

        nser = 0
        do i = 1, nterm
            if (aterm(i) (1:3) == '$~$') then
                call char2num(ierr, aterm(i) (4:), isnum)
                if (ierr /= 0) then
                    write (amessage, 250)
250                 format('internal error - contact programmer.')
                    go to 9800
                end if
                if (nser == 0) then
                    nser = nser + 1
                    serno(nser) = isnum
                else
                    do j = 1, nser - 1
                        if (serno(j) == isnum) go to 270
                    end do
                    nser = nser + 1
                    if (nser > maxeqnser) then
                        call num2char(maxeqnser, aline)
                        write (amessage, 260) TRIM(aline)
260                     format('maximum of ', a, ' different series names can be cited in a ', &
                               'series equation.')
                        go to 9800
                    end if
                    serno(nser) = isnum
270                 continue
                end if
            end if
        end do
        if (nser == 0) then
            write (amessage, 280)
280         format('at least one series name should be cited in a series equation.')
            go to 9800
        end if

        ! -- Now they are checked for time consitency.

        if (nser == 1) go to 350
        do iser = 2, nser
            i = serno(iser)
            j = serno(iser - 1)
            if (series_g(i)%nterm /= series_g(j)%nterm) go to 9300
            do k = 1, series_g(i)%nterm
                if (series_g(i)%days(k) /= (series_g(j)%days(k))) go to 9300
                if (series_g(i)%secs(k) /= (series_g(j)%secs(k))) go to 9300
            end do
        end do

350     continue

! -- Space is allocated for the new series.

        do iser = 1, MAXSERIES
            if (.NOT. series_g(iser)%active) go to 370
        end do
        write (amessage, 360)
360     format('no more time series available for data storage - increase MAXSERIES and ', &
               'recompile program.')
        go to 9800

370     continue
        k = serno(nser)
        nsterm = series_g(k)%nterm
        allocate (series_g(iser)%days(nsterm), series_g(iser)%secs(nsterm), &
                  series_g(iser)%val(nsterm), stat=ierr)
        if (ierr /= 0) then
            write (amessage, 380)
380         format('cannot allocate memory for another time series.')
            go to 9800
        end if
        series_g(iser)%active = .TRUE.
        series_g(iser)%name = aname
        series_g(iser)%nterm = nsterm
        series_g(iser)%type = 'ts'
        do j = 1, nsterm
            series_g(iser)%days(j) = series_g(k)%days(j)
        end do
        do j = 1, nsterm
            series_g(iser)%secs(j) = series_g(k)%secs(j)
        end do

! -- Numbers are identified and copied to the rterm array.

        do iterm = 1, nterm
            aa = aterm(iterm) (1:1)
            if ((aa /= '(') .AND. (aa /= ')') .AND. (aa /= '+') .AND. (aa /= '-') .AND. &
                (aa /= '*') .AND. (aa /= '/') .AND. (aa /= '^') .AND. &
                (aterm(iterm) (1:6) /= '~#str_') .AND. &
                (aterm(iterm) (1:6) /= '~#fin_') .AND. &
                (aterm(iterm) (1:3) /= '$~$') .AND. &
                (aterm(iterm) (1:2) /= '@_')) then
                call char2num(ierr, aterm(iterm), rterm(iterm))
                if (ierr /= 0) then
                    write (amessage, 395) TRIM(aterm(iterm))
395                 format('the term "', a, '" in the series equation cannot be interpreted ', &
                           'as a number, function or operator.')
                    go to 9800
                end if
                aterm(iterm) = '~!~'
            end if
        end do

! -- We now check for intrinsic functions.

        do iterm = 1, nterm
            if (aterm(iterm) (1:2) == '@_') then
                if (aterm(iterm) (3:) == 'days_start_year') then
                    aterm(iterm) (3:) = '1'
                else if (aterm(iterm) (3:) == 'abs_val') then
                    write (amessage, 409) TRIM(aterm(iterm))
409                 format('intrinsic function "', a, '" cannot be used in a series equation.')
                    go to 9800
                else if (aterm(iterm) (3:7) == 'days_') then
                    lnx = LEN(aterm(iterm))
                    do idx = 1, lnx
                        if (aterm(iterm) (idx:idx) == '~') aterm(iterm) (idx:idx) = '/'
                    end do
                    call getfile(ierr, aterm(iterm), adate_atime, 8, lnx)
                    if (ierr /= 0) go to 9400
                    idx = INDEX(adate_atime, '_')
                    if (idx == 0) go to 9400
                    call char2date(ierr, adate_atime(1:idx - 1), ddx, mmx, yyx)
                    if (ierr /= 0) go to 9400
                    call char2time(ierr, adate_atime(idx + 1:), hhx, nnx, ssx, ignore_24=1)
                    if (ierr /= 0) go to 9400
!             nex=numdays(1,1,1970,ddx,mmx,yyx)
                    nex = julian_day(iMonth=mmx, iDay=ddx, iYear=yyx)
                    sex = numsecs(0, 0, 0, hhx, nnx, ssx)
                    dtempx = DBLE(nex) + DBLE(sex) / 86400.0D0
                    aterm(iterm) (3:4) = '3_'
                    write (aterm(iterm) (5:), '(1pd22.14)') dtempx
                else
                    go to 9400
                end if
            end if
        end do

! -- The series equation is now evaluated for each term in the series.

        nnterm = nterm
        do iterm = 1, nterm
            cterm(iterm) = aterm(iterm)
        end do
        do iterm = 1, nterm
            qterm(iterm) = rterm(iterm)
        end do
        do j = 1, nsterm
            nterm = nnterm
            do iterm = 1, nterm
                aterm(iterm) = cterm(iterm)
            end do
            do iterm = 1, nterm
                rterm(iterm) = qterm(iterm)
            end do

! -- First the series numbers in the equation terms are replaced by series values.

            do iterm = 1, nterm
                if (aterm(iterm) (1:3) == '$~$') then
                    call char2num(ierr, aterm(iterm) (4:), isnum)
                    rterm(iterm) = series_g(isnum)%val(j)
                    aterm(iterm) = '~!~'
!             write(aterm(iterm),'(e25.14e3)')series_g(isnum)%val(j)
                end if
            end do

! -- Any instinsic function evaluations are carried out.

            do iterm = 1, nterm
                if (aterm(iterm) (1:3) == '@_1') then
!             call newdate(series_g(isnum)%days(j),1,1,1970,dd,mm,yy)
!             call gregorian_date(iJD=series_g(isnum)%days(j), &
!                                 iMonth=mm, &
!                                 iDay=dd, &
!                                 iYear=yy)
                    nn = day_of_year(series_g(isnum)%days(j))
                    rtime = float(nn) + float(series_g(isnum)%secs(j)) / 86400.0
                    rterm(iterm) = rtime
                    aterm(iterm) = '~!~'

                else if (aterm(iterm) (1:3) == '@_3') then !
                    call char2num(ierr, aterm(iterm) (5:), dtempx)
                    rterm(iterm) = DBLE(series_g(isnum)%days(j)) + &
                                   DBLE(series_g(isnum)%secs(j)) / 86400.0D0 - dtempx
                    aterm(iterm) = '~!~'
                end if
            end do

            call EVALUATE(ierr, MAXTERM, NTERM, NOPER, NFUNCT, ATERM, BTERM, &
                          OPERAT, FUNCT, IORDER, DVAL, rterm)
            if (ierr /= 0) go to 9800
            series_g(iser)%val(j) = dval
        end do
        write (*, 580) TRIM(aname)
        write (LU_REC, 580) TRIM(aname)
580     format(t5, 'Series "', a, '" successfully calculated using series equation.')

!       close(unit=99)                !debug
        return

9000    call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9010) TRIM(aline), TRIM(sString_g)
9010    format('cannot read line ', a, ' of TSPROC input file ', a)
        go to 9800
9100    continue
        call addquote(sInfile_g, sString_g)
        write (amessage, 9110) TRIM(sString_g), TRIM(CurrentBlock_g)
9110    format('unexpected end encountered to TSPROC input file ', a, &
               ' while reading ', a, ' block.')
        go to 9800
9200    call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9210) TRIM(aline), TRIM(sString_g)
9210    format('cannot read equation from line ', a, ' of TSPROC input file ', a)
        go to 9800
9300    write (amessage, 9310) TRIM(series_g(i)%name), TRIM(series_g(j)%name)
9310    format('all series cited in the series equation must have an identical time-base ', &
               '(ie. the same number of terms, with all dates and times coincident). Series ', &
               '"', a, '" and "', a, '" have different time bases.')
        go to 9800
9400    write (amessage, 9410) TRIM(aterm(iterm))
9410    format('illegal intrinsic function "', a, '" in series equation.')
        go to 9800
9500    write (amessage, 9510)
9510    format('illegal "@_days_" function in series equation.')
        go to 9800

9800    call write_message(leadspace='yes', error='yes')
        call write_message(iunit=LU_REC, leadspace='yes')
        ifail = 1

        return

    end subroutine equation

    subroutine PARSE(IFAIL, MAXTERM, NTERM, NOPER, eqntext, ATERM, &
                     BTERM, NFUNCT, FUNCT, OPERAT, rterm, itype)

! -- Subroutine PARSE breaks an expression up into elements.
        implicit none

        integer IFAIL, MAXTERM, NTERM, NFUNCT, NOPER, ierr, itype
        integer ICOUNT, NB, I, JFAIL, ITERM, J, IIFUN, NEG
        double precision rterm(maxterm)
        character(30) ATEMP
        character(1) AA
        character(6) AATERM
        character(10) BB
        character * (*) ATERM(MAXTERM), BTERM(MAXTERM)
        character * (*) eqntext
        character * (*) FUNCT(NFUNCT)
        character * (*) OPERAT(NOPER)

        IFAIL = 0
        NTERM = 0

! -- First a check is made to see if brackets are balanced.

        if (eqntext == ' ') GO TO 9000

        ICOUNT = 0
        NB = LEN_TRIM(eqntext)
        do I = 1, NB
            if (eqntext(I:I) == '(') then
                ICOUNT = ICOUNT + 1
            else if (eqntext(I:I) == ')') then
                ICOUNT = ICOUNT - 1
            end if
        end do
        if (ICOUNT /= 0) then
            if (itype == 0) then
                write (amessage, 4)
4               format('unbalanced parentheses in series equation.')
            else if (itype == 1) then
                write (amessage, 2)
2               format('unbalanced parentheses in weights equation.')
            end if
            GO TO 9020
        end if

        if (INDEX(eqntext, '=') /= 0) GO TO 9000
5       continue
        call GETNEXT(JFAIL, NOPER, eqntext, ATEMP, OPERAT)
        if (JFAIL < 0) then
            GO TO 50
        else
            NTERM = NTERM + 1
            if (NTERM > MAXTERM) GO TO 9100
            ATERM(NTERM) = ATEMP
            GO TO 5
        end if
50      continue

! -- Functions are now dealt with.

        IIFUN = 0
        if (NTERM <= 2) GO TO 400
        do ITERM = 2, NTERM
            if (ATERM(ITERM) (1:1) == '(') then
                AA = ATERM(ITERM - 1) (1:1)
                if ((AA == '+') .OR. (AA == '-') .OR. (AA == '*') .OR. &
                    (AA == '/') .OR. (AA == '^') .OR. (AA == '(')) cycle
                AATERM = ATERM(ITERM - 1) (1:6)
                do J = 1, NFUNCT
                    if (AATERM == FUNCT(J)) GO TO 80
                end do
                if (itype == 0) then
                    write (amessage, 75) TRIM(ATERM(ITERM - 1))
75                  format('illegal function name  "', A, '" in series equation.')
                else
                    write (amessage, 76) TRIM(ATERM(ITERM - 1))
76                  format('illegal function name  "', A, '" in weights equation.')
                end if
                GO TO 9020
80              continue
                call num2char(j, bb)
!          CALL WRTINT(BB,J)
                ATERM(ITERM - 1) = '~#str_'//TRIM(BB)
                IIFUN = IIFUN + 1
            end if
        end do
        if (IIFUN == 0) GO TO 400

        do ITERM = 1, NTERM
            if (ATERM(ITERM) (1:6) == '~#str_') then
                ATERM(ITERM + 1) (1:1) = CHAR(220)
                ICOUNT = 1
                do J = ITERM + 1, NTERM
                    if (ATERM(J) (1:1) == '(') then
                        ICOUNT = ICOUNT + 1
                    else if (ATERM(J) (1:1) == ')') then
                        ICOUNT = ICOUNT - 1
                        if (ICOUNT == 0) then
                            ATERM(J) = '~#fin_'
                            GO TO 300
                        end if
                    end if
                end do
            end if
300         continue
        end do

        call COMPRESS(MAXTERM, NTERM, ATERM, BTERM, rterm)

400     continue

! -- If the last item is an operator then the expression is invalid.

        AA = ATERM(NTERM) (1:1)
        if ((AA == '+') .OR. (AA == '-') .OR. (AA == '/') .OR. (AA == '*') .OR. &
            (AA == '^')) GO TO 9000

! -- The "-" and the "+" signs are expanded as a function if appropriate.

490     continue
        do ITERM = 1, NTERM
            if ((ATERM(ITERM) (1:1) == '-') .OR. &
                (ATERM(ITERM) (1:1) == '+')) then
                if (ATERM(ITERM) (1:1) == '+') then
                    NEG = 0
                else
                    NEG = 1
                end if
                if (ITERM == 1) then
                    if (NTERM == MAXTERM) GO TO 9100
                    call EXPNEG(ierr, MAXTERM, NTERM, ITERM, ATERM, NEG)
                    if (ierr /= 0) GO TO 9000
                    GO TO 490
                else if (ATERM(ITERM - 1) (1:6) == '~#str_') then
                    if (NTERM == MAXTERM) GO TO 9100
                    call EXPNEG(ierr, MAXTERM, NTERM, ITERM, ATERM, NEG)
                    if (ierr /= 0) GO TO 9000
                    GO TO 490
                else
                    AA = ATERM(ITERM - 1) (1:1)
                    if ((AA == '(') .OR. (AA == '+') .OR. (AA == '-') .OR. &
                        (AA == '*') .OR. (AA == '/') .OR. (AA == '^')) then
                        if (NTERM == MAXTERM) GO TO 9100
                        call EXPNEG(ierr, MAXTERM, NTERM, ITERM, ATERM, NEG)
                        if (ierr /= 0) GO TO 9000
                        GO TO 490
                    end if
                end if
            end if
        end do

        return

9000    continue
        if (itype == 0) then
            write (amessage, 9010)
9010        format('illegal series equation.')
        else if (itype == 1) then
            write (amessage, 9011)
9011        format('illegal weights equation.')
        end if
        go to 9020
9100    continue
        if (itype == 0) then
            write (amessage, 9110)
9110        format('too many terms in series equation.')
        else if (itype == 1) then
            write (amessage, 9111)
9111        format('too many terms in weights equation.')
        end if
        GO TO 9020

9020    IFAIL = 1

        return
    end subroutine PARSE

    subroutine EXPNEG(IFAIL, MAXTERM, NTERM, ITERM, ATERM, NEG)

! -- Subroutine EXPNEG expands a "-" sign into a function.

        integer MAXTERM, NTERM, ITERM, IFAIL, ICOUNT, JTERM, I, NEG
        character(*) ATERM(MAXTERM)

        IFAIL = 0
        if (NEG == 1) then
            ATERM(ITERM) = '~#str_15'
        else
            ATERM(ITERM) = '~#str_16'
        end if
        ICOUNT = 0
        do JTERM = ITERM + 1, NTERM
            if ((ATERM(JTERM) (1:1) == '-') .OR. &
                (ATERM(JTERM) (1:1) == '+')) cycle
            if (ATERM(JTERM) (1:1) == '(') then
                ICOUNT = ICOUNT + 1
            else if (ATERM(JTERM) (1:1) == ')') then
                ICOUNT = ICOUNT - 1
            else if (ATERM(JTERM) (1:6) == '~#str_') then
                ICOUNT = ICOUNT + 1
            else if (ATERM(JTERM) (1:6) == '~#fin_') then
                ICOUNT = ICOUNT - 1
            end if
            if (ICOUNT < 0) then
                IFAIL = 1
                return
            end if
            if (ICOUNT == 0) then
                if (JTERM < NTERM) then
                    do I = NTERM, JTERM + 1, -1
                        ATERM(I + 1) = ATERM(I)
                    end do
                end if
                ATERM(JTERM + 1) = '~#fin_'
                NTERM = NTERM + 1
                return
            end if
        end do

        return
    end subroutine EXPNEG

    subroutine GETNEXT(IFAIL, NOPER, CLINE, ATERM, OPERAT)

! -- Subroutine GETNEXT splits off the next term of an expression.

        implicit none
        integer IFAIL, I, J, NB, NOPER, L, K, IERR
        double precision DVAL
        character(len=iTSNAMELENGTH) :: AFMT
        character * (*) CLINE
        character * (*) ATERM
        character * (*) OPERAT(NOPER)

        ATERM = ' '
        IFAIL = 0
        if (CLINE == ' ') then
            IFAIL = -1
            return
        end if

        do I = 1, NOPER
            if (CLINE(1:1) == OPERAT(I)) then
                ATERM(1:1) = OPERAT(I)
                CLINE = CLINE(2:)
                cline = ADJUSTL(cline)
                GO TO 20
            end if
        end do
        GO TO 50

20      if (ATERM(1:1) == '*') then
            if (CLINE(1:1) == '*') then
                ATERM(1:1) = '^'
                CLINE = CLINE(2:)
                cline = ADJUSTL(cline)
            end if
        end if
        return

50      continue
        NB = LEN_TRIM(CLINE)
        do I = 2, NB
            do J = 1, NOPER
                if (CLINE(I:I) == OPERAT(J)) then
                    if (I <= 2) go to 120
                    if (I == NB) go to 120
                    if ((J /= 4) .AND. (J /= 5)) go to 120
                    if ((CLINE(I - 1:I - 1) /= 'E') .AND. (CLINE(I - 1:I - 1) /= 'e') .AND. &
                        (CLINE(I - 1:I - 1) /= 'D') .AND. (CLINE(I - 1:I - 1) /= 'd')) go to 120
                    do K = I + 1, NB
                        do L = 1, NOPER
                            if (CLINE(K:K) == OPERAT(L)) go to 200
                        end do
                    end do
                    K = NB + 1
200                 K = K - 1
                    afmt = '(f    .0)'
                    write (afmt(3:6), '(i4)') k
                    read (cline(1:k), afmt, iostat=ierr) dval
                    if (IERR /= 0) go to 120
                    cycle
                end if
            end do
        end do
        ATERM = CLINE(1:MIN(28, NB))
        CLINE = ' '
        return

120     ATERM = CLINE(1:I - 1)
        CLINE = CLINE(I:)
        return

    end subroutine GETNEXT

    subroutine COMPRESS(MAXTERM, NTERM, ATERM, BTERM, rterm)

! -- Subroutine COMPRESS removes "dead terms" from the expression.

        implicit none
        integer MAXTERM, NTERM, I, JTERM
        double precision rterm(maxterm)
        character * (*) ATERM(MAXTERM), BTERM(MAXTERM)

        do I = 1, NTERM
            BTERM(I) = ATERM(I)
        end do
        JTERM = 0
        do I = 1, NTERM
            if (BTERM(I) (1:1) /= CHAR(220)) then
                JTERM = JTERM + 1
                ATERM(JTERM) = BTERM(I)
                rterm(jterm) = rterm(i)
            end if
        end do
        NTERM = JTERM

        return
    end subroutine COMPRESS

    SUBROUTINE series_sub(IFAIL,NTERM,itype)
! -- Subroutine SERIES_SUB replaces series names with their numbers.

    implicit none
    INTEGER IFAIL,NTERM,ITERM,J,JERR,NB,is,itype
    DOUBLE PRECISION DTEMP
    character (len=iTSNAMELENGTH) :: as
    CHARACTER(25)AAPAR

    IFAIL=0
    DO 200 ITERM=1,NTERM
       if(aterm(iterm)(1:2).eq.'@_') go to 200
       IF(ATERM(ITERM)(1:2).EQ.'~#') GO TO 200
       DO 20 J=1,NOPER
         IF(ATERM(ITERM)(1:1).EQ.OPERAT(J)) GO TO 200
20       CONTINUE
       AAPAR=ATERM(ITERM)
       NB=len_trim(AAPAR)
       IF(INDEX(AAPAR(1:NB),' ').NE.0)THEN
         if(itype.eq.0)then
           WRITE(amessage,30) AAPAR(1:NB)
30           FORMAT('series name "',A,'" in series equation cannot include a ',  &
           'blank character.')
         else if(itype.eq.1)then
           WRITE(amessage,31) AAPAR(1:NB)
31           FORMAT('series name "',A,'" in weights equation cannot include a ',  &
           'blank character.')
         endif
         IFAIL=1
         RETURN
       END IF
       call char2num(jerr,aapar,dtemp)
!         CALL RLREAD(JERR,AAPAR,DTEMP)
       IF(JERR.EQ.0) GO TO 200
       do is=1,MAXSERIES
         if(.not.series_g(is)%active) cycle
         if(series_g(is)%name.ne.aapar)cycle
         call num2char(is,as)
         aterm(iterm)='$~$'//trim(as)
         go to 200
       end do
       if(itype.eq.0)then
         write(amessage,40) trim(aapar)
40         format('series "',a,'" appearing in the series equation is either ', &
         'undefined or has been erased.')
       else if(itype.eq.1)then
         write(amessage,41) trim(aapar)
41         format('series "',a,'" appearing in a weights equation is either ', &
         'undefined or has been erased.')
       end if
       ifail=1
       return
200   CONTINUE

    RETURN
    END SUBROUTINE series_sub






    subroutine EVALUATE(IFAIL, MAXTERM, NTERM, NOPER, NFUNCT, ATERM, BTERM, &
                        OPERAT, FUNCT, IORDER, DVAL, rterm)

        implicit none
        integer NTERM, NOPER, NFUNCT, MAXTERM, ITERM, JERR, MAXORD, ICOUNT, I, &
            IOPER, IFAIL
        integer IORDER(MAXTERM)
        double precision rterm(maxterm)
        double precision DVAL, DTEMP1, DTEMP2
        character(1) AA
        character(6) AFUNCT
        character * (*) ATERM(MAXTERM), BTERM(MAXTERM)
        character * (*) OPERAT(NOPER), FUNCT(NFUNCT)

        IFAIL = 0

! -- IF THERE IS ONLY ONE TERM LEFT, THE EXPRESSION HAS BEEN EVALUATED.

100     continue
!      write(99,*) (trim(aterm(iterm)),iterm=1,nterm)     !debug

        if (NTERM == 1) then
            dval = rterm(1)
!        CALL RLREAD(JERR,ATERM(1),DVAL)
!        IF(JERR.NE.0)THEN
!          WRITE(amessage,110)
!110       FORMAT('cannot evaluate series equation using current parameter values.')
!          go to 9999
!        END IF
            return
        end if

! -- IF THERE ARE ANY NUMBERS SURROUNDED BY BRACKETS, THEN THE BRACKETS ARE
!    REMOVED

        if (NTERM >= 3) then
            do ITERM = 1, NTERM - 2
                if (ATERM(ITERM) (1:1) == '(') then
                    if (ATERM(ITERM + 2) (1:1) == ')') then
                        ATERM(ITERM) (1:1) = CHAR(220)
                        ATERM(ITERM + 2) (1:1) = CHAR(220)
                        call COMPRESS(MAXTERM, NTERM, ATERM, BTERM, rterm)
                        GO TO 100
                    end if
                end if
            end do
        end if

! -- CAN ANY FUNCTION EVALUATIONS NOW BE DONE?

        if (NTERM >= 3) then
            do ITERM = 1, NTERM - 2
                if (ATERM(ITERM) (1:6) == '~#str_') then
                    if (ATERM(ITERM + 2) (1:6) == '~#fin_') then
                        call FUNCEVAL(JERR, ATERM(ITERM), rterm(iterm + 1), DVAL)
                        if (JERR /= 0) then
                            AFUNCT = FUNCT(JERR)
                            write (amessage, 170) TRIM(AFUNCT)
170                         format('cannot evaluate "', A, '" function in ', &
                                   'series or weights equation because function argument is out of range ', &
                                   'for at least one term in the series time_span.')
                            go to 9999
                        end if
                        ATERM(ITERM) (1:1) = CHAR(220)
                        rterm(iterm + 1) = dval
                        aterm(iterm + 1) = '~!~'
                        ATERM(ITERM + 2) (1:1) = CHAR(220)
                        call COMPRESS(MAXTERM, NTERM, ATERM, BTERM, rterm)
                        GO TO 100
                    end if
                end if
            end do
        end if

! -- The operators are now ranked by their level of nesting.

        MAXORD = 0
        do ITERM = 1, NTERM
            IORDER(ITERM) = 0
        end do
        ICOUNT = 1
        do ITERM = 1, NTERM
            AA = ATERM(ITERM) (1:1)
            if (AA == '(') then
                ICOUNT = ICOUNT + 1
            else if (AA == ')') then
                ICOUNT = ICOUNT - 1
            else if (ATERM(ITERM) (1:6) == '~#str_') then
                ICOUNT = ICOUNT + 1
            else if (ATERM(ITERM) (1:6) == '~#fin_') then
                ICOUNT = ICOUNT - 1
            else if ((AA == '+') .OR. (AA == '-') .OR. (AA == '*') .OR. &
                     (AA == '/') .OR. (AA == '^')) then
                IORDER(ITERM) = ICOUNT
                if (ICOUNT > MAXORD) MAXORD = ICOUNT
            else
                IORDER(ITERM) = -1 ! It must be a number.
            end if
        end do

! -- We now look for a calculation to do, starting at the highest level.

        if (NTERM >= 3) then
            do I = MAXORD, 1, -1
                do IOPER = 1, 5
                    do ITERM = 2, NTERM - 1
                        if (IORDER(ITERM) == I) then !It is an operator
                            if (ATERM(ITERM) (1:1) == OPERAT(IOPER)) then
                                if ((IORDER(ITERM - 1) < 0) .AND. &
                                    (IORDER(ITERM + 1) < 0)) then !numbers either side
                                    dtemp1 = rterm(iterm - 1)
                                    dtemp2 = rterm(iterm + 1)
!                    CALL RLREAD(IFAIL,ATERM(ITERM-1),DTEMP1)
!                    CALL RLREAD(IFAIL,ATERM(ITERM+1),DTEMP2)
                                    if (IOPER == 1) then
                                        if (DTEMP1 < 0.0) then
                                            if (DTEMP2 /= FLOAT(NINT(DTEMP2))) then
                                                write (amessage, 384)
384                                             format('negative number raised to fractional power in series ', &
                                                       'or weights equation.')
                                                go to 9999
                                            end if
                                        end if
                                        DVAL = DTEMP1**DTEMP2
                                    else if (IOPER == 3) then
                                        DVAL = DTEMP1 * DTEMP2
                                    else if (IOPER == 2) then
                                        if (DTEMP2 == 0.0D0) then
                                            write (amessage, 385)
385                                         format('divide by zero in series or weights equation.')
                                            go to 9999
                                        end if
                                        DVAL = DTEMP1 / DTEMP2
                                    else if (IOPER == 5) then
                                        DVAL = DTEMP1 + DTEMP2
                                    else if (IOPER == 4) then
                                        DVAL = DTEMP1 - DTEMP2
                                    end if
                                    rterm(iterm) = dval
                                    aterm(iterm) = '~!~'
                                    ATERM(ITERM - 1) (1:1) = CHAR(220)
                                    ATERM(ITERM + 1) (1:1) = CHAR(220)
                                    call COMPRESS(MAXTERM, NTERM, ATERM, BTERM, rterm)
                                    GO TO 100
                                end if
                            end if
                        end if
                    end do
                end do
            end do
        end if
        write (amessage, 410)
410     format('cannot evaluate series equation.')
        go to 9999

9999    IFAIL = 1
        return
    end subroutine EVALUATE

    subroutine FUNCEVAL(JERR, ATERM1, rterm, DVAL)

! -- Subroutine FUNCEVAL evaluates a function.

        implicit none
        integer JERR, IFN, IFAIL
        double precision DVAL, DTEMP, rterm
        character * (*) ATERM1

! -- First we find out which function we are evaluating.

        JERR = 0
        ATERM1(1:6) = ' '
        aterm1 = ADJUSTL(aterm1)
        call char2num(ifail, aterm1, ifn)
        dtemp = rterm
!      CALL INREAD(IFAIL,ATERM1,IFN)
!      CALL RLREAD(IFAIL,ATERM2,DTEMP)
!      IF(IFAIL.NE.0) GO TO 9000
        if (IFN == 1) then
            DVAL = ABS(DTEMP)
        else if (IFN == 2) then
            if ((DTEMP > 1.0D0) .OR. (DTEMP < -1.0D0)) GO TO 9000
            DVAL = ACOS(DTEMP)
        else if (IFN == 3) then
            if ((DTEMP > 1.0D0) .OR. (DTEMP < -1.0D0)) GO TO 9000
            DVAL = ASIN(DTEMP)
        else if (IFN == 4) then
            DVAL = ATAN(DTEMP)
        else if (IFN == 5) then
            if ((DTEMP > 1.0D10) .OR. (DTEMP < -1.0D10)) GO TO 9000
            DVAL = COS(DTEMP)
        else if (IFN == 6) then
            DVAL = COSH(DTEMP)
        else if (IFN == 7) then
            if (DTEMP > 500.0D0) GO TO 9000
            DVAL = EXP(DTEMP)
        else if (IFN == 8) then
            if (DTEMP <= 0.0D0) GO TO 9000
            DVAL = LOG(DTEMP)
        else if (IFN == 9) then
            if (DTEMP <= 0.0D0) GO TO 9000
            DVAL = LOG10(DTEMP)
        else if (IFN == 10) then
            if ((DTEMP > 1.0D10) .OR. (DTEMP < -1.0D10)) GO TO 9000
            DVAL = SIN(DTEMP)
        else if (IFN == 11) then
            DVAL = SINH(DTEMP)
        else if (IFN == 12) then
            if (DTEMP < 0.0D0) GO TO 9000
            DVAL = SQRT(DTEMP)
        else if (IFN == 13) then
            if ((DTEMP > 1.0E10) .OR. (DTEMP < -1.0E10)) GO TO 9000
            DVAL = TAN(DTEMP)
        else if (IFN == 14) then
            DVAL = TANH(DTEMP)
        else if (IFN == 15) then
            DVAL = -DTEMP
        else if (IFN == 16) then
            DVAL = DTEMP
        end if

        return

! -- An error condition has occurred.

9000    JERR = IFN
        return

    end subroutine FUNCEVAL

    subroutine prepare_eqn(ifail, nterm, eqntext, iseries)

! -- Subroutine PREPARE_EQN prepares a weights equation for use in weights calculation.

        implicit none
        integer, intent(out) :: ifail
        integer, intent(out) :: nterm
        character*(*), intent(inout) :: eqntext
        integer, intent(in) :: iseries

        integer ierr, iterm, k, isnum, i, ddx, mmx, yyx, hhx, nnx, ssx, idx, nex, sex, lnx
        double precision dtempx
        character(1) aa
        character(25) adate_atime

        ifail = 0
        call parse(ierr, MAXTERM, nterm, noper, eqntext, aterm, bterm, nfunct, funct, &
                   operat, rterm, 1)
        if (ierr /= 0) then
            ifail = 1
            return
        end if

! -- Series are identified in the weights equation.

        call series_sub(ierr, NTERM, 1)
        if (ierr /= 0) then
            ifail = 1
            return
        end if

        do i = 1, nterm
            if (aterm(i) (1:3) == '$~$') then
                if (iseries == 0) then
                    write (amessage, 5)
5                   format('a series name cannot be cited in the weights equation for ', &
                           'an s_table, v_table or e_table.')
                    ifail = 1
                    return
                end if
                call char2num(ierr, aterm(i) (4:), isnum)
                if (ierr /= 0) then
                    write (amessage, 250)
250                 format('internal error - contact programmer.')
                    ifail = 1
                    return
                end if
                if (series_g(iseries)%nterm /= series_g(isnum)%nterm) go to 9300
                do k = 1, series_g(iseries)%nterm
                    if (series_g(iseries)%days(k) /= (series_g(isnum)%days(k))) go to 9300
                    if (series_g(iseries)%secs(k) /= (series_g(isnum)%secs(k))) go to 9300
                end do
            end if
        end do

! -- Numbers are identified and copied to the rterm array.

        do iterm = 1, nterm
            aa = aterm(iterm) (1:1)
            if ((aa /= '(') .AND. (aa /= ')') .AND. (aa /= '+') .AND. (aa /= '-') .AND. &
                (aa /= '*') .AND. (aa /= '/') .AND. (aa /= '^') .AND. &
                (aterm(iterm) (1:6) /= '~#str_') .AND. &
                (aterm(iterm) (1:6) /= '~#fin_') .AND. &
                (aterm(iterm) (1:3) /= '$~$') .AND. &
                (aterm(iterm) (1:2) /= '@_')) then
                call char2num(ierr, aterm(iterm), rterm(iterm))
                if (ierr /= 0) then
                    write (amessage, 1870) TRIM(aterm(iterm))
1870                format('the term "', a, '" in a weights equation cannot be interpreted ', &
                           'as a number, function or operator.')
                    ifail = 1
                    return
                end if
                aterm(iterm) = '~!~'
            end if
        end do

! -- We now check for intrinsic functions.

        do iterm = 1, nterm
            if (aterm(iterm) (1:2) == '@_') then
                if (aterm(iterm) (3:) == 'abs_value') then
                    aterm(iterm) (3:) = '2'
                elseif (aterm(iterm) (3:) == 'min') then ! ** SMW Addition
                    aterm(iterm) (3:) = '4' ! ** SMW Addition
                elseif (aterm(iterm) (3:) == 'max') then ! ** SMW Addition
                    aterm(iterm) (3:) = '5' ! ** SMW Addition
                elseif (aterm(iterm) (3:) == 'obj_fun_value') then ! ** SMW Addition
                    aterm(iterm) (3:) = '6' ! ** SMW Addition
                elseif (aterm(iterm) (3:) == 'count') then ! ** SMW Addition
                    aterm(iterm) (3:) = '7' ! ** SMW Addition
                elseif (aterm(iterm) (3:) == 'mean') then ! ** SMW Addition
                    aterm(iterm) (3:) = '8' ! ** SMW Addition
                elseif (aterm(iterm) (3:) == 'stddev') then ! ** SMW Addition
                    aterm(iterm) (3:) = '9' ! ** SMW Addition

                else if (aterm(iterm) (3:) == 'days_start_year') then
                    if (iseries /= 0) then
                        aterm(iterm) (3:) = '1'
                    else
                        write (amessage, 1875) TRIM(aterm(iterm))
1875                    format('intrinsic function "', a, '" cannot be used in the ', &
                               'weights equation for an s_table, e_table or v_table.')
                        ifail = 1
                        return
                    end if
                else if (aterm(iterm) (3:7) == 'days_') then
                    lnx = LEN(aterm(iterm))
                    do idx = 1, lnx
                        if (aterm(iterm) (idx:idx) == CHAR(196)) aterm(iterm) (idx:idx) = '/'
                    end do
                    if (iseries == 0) then
                        write (amessage, 1875) TRIM(aterm(iterm))
                        ifail = 1
                        return
                    else
                        call getfile(ierr, aterm(iterm), adate_atime, 8, lnx)
                        if (ierr /= 0) go to 9400
                        idx = INDEX(adate_atime, '_')
                        if (idx == 0) go to 9400
                        call char2date(ierr, adate_atime(1:idx - 1), ddx, mmx, yyx)
                        if (ierr /= 0) go to 9400
                        call char2time(ierr, adate_atime(idx + 1:), hhx, nnx, ssx, ignore_24=1)
                        if (ierr /= 0) go to 9400
!               nex=numdays(1,1,1970,ddx,mmx,yyx)
                        nex = julian_day(iMonth=mmx, iDay=ddx, iYear=yyx)
                        sex = numsecs(0, 0, 0, hhx, nnx, ssx)
                        dtempx = DBLE(nex) + DBLE(sex) / 86400.0D0
                        aterm(iterm) (3:4) = '3_'
                        write (aterm(iterm) (5:), '(1pd22.14)') dtempx
                    end if
                else
                    go to 9400
                end if
            end if
        end do

        return

9300    write (amessage, 9310)
9310    format('any series cited in a weights equation must have an identical time-base ', &
               '(ie. the same number of terms, with all dates and times coincident) as the ', &
               'observation time series.')
        ifail = 1
        return
9400    write (amessage, 9410) TRIM(aterm(iterm))
9410    format('illegal intrinsic function "', a, '" in weights equation.')
        ifail = 1
        return

    end subroutine prepare_eqn

end module tsp_equation_parser
