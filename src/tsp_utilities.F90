module tsp_utilities

#ifdef __INTEL_COMPILER
    use ifport
#endif

    use tsp_data_structures
    implicit none

    ! declare generic interfaces

    interface chomp
        module procedure chomp_delim_sub
        module procedure chomp_default_sub
    end interface

    interface char2num
        module procedure a2i
        module procedure a2r
        module procedure a2d
    end interface

    interface num2char
        module procedure i2a
        module procedure r2a
        module procedure d2a
    end interface

    interface asChar
        module procedure int2char
        module procedure real2char
        module procedure double2char
    end interface

    interface pos_test
        module procedure pos_i_test
        module procedure pos_r_test
        module procedure pos_d_test
    end interface

    interface nneg_test
        module procedure nneg_i_test
        module procedure nneg_r_test
        module procedure nneg_d_test
    end interface

    interface key_read
        module procedure int_key_read
        module procedure real_key_read
        module procedure double_key_read
    end interface

    interface equals
        module procedure equals_int
        module procedure equals_real
        module procedure equals_dbl
    end interface

    interface uppercase
!    module procedure uppercase_sub
        module procedure uppercase_fn
    end interface

    character(len=1), parameter :: sTAB = ACHAR(9)
    character(len=2), parameter :: sWHITESPACE = ACHAR(9)//" "
    character(len=1), parameter :: sBACKSLASH = ACHAR(92)
    character(len=1), parameter :: sFORWARDSLASH = ACHAR(47)

contains

!     Last change:  JD   28 Dec 2000    9:08 pm

    subroutine addquote(afile, aqfile)

! -- Subroutine ADDQUOTE adds quotes to a filename if it has a space in it.

! -- Arguments are as follows:-
!        afile:       the name of the file
!        aqfile:      the name of the file with quotes added

        character(len=*), intent(in) :: afile
        character(len=*), intent(out) :: aqfile
        integer nbb

        if (INDEX(TRIM(afile), ' ') == 0) then
            aqfile = afile
        else
            aqfile(1:1) = '"'
            aqfile(2:) = TRIM(afile)
            nbb = LEN_TRIM(aqfile) + 1
            aqfile(nbb:nbb) = '"'
        end if

        return
    end subroutine addquote

    function str_compare(sString1, sString2) result(lBool)

        character(len=*) :: sString1
        character(len=*) :: sString2
        logical(kind=T_LOGICAL) :: lBool

        if (TRIM(ADJUSTL(uppercase(sString1))) == TRIM(ADJUSTL(uppercase(sString2)))) then
            lBool = lTRUE
        else
            lBool = lFALSE
        end if

    end function str_compare

    subroutine spacesub(sString_g)

        integer i, j, k, n
        character(1) bb
        character * (*) sString_g

        bb = CHAR(211)
        n = LEN_TRIM(sString_g)
        k = 1
10      continue
        if (k > n) go to 100
        do i = k, n
            if ((sString_g(i:i) == '''') .OR. (sString_g(i:i) == '"')) then
                sString_g(i:i) = ' '
                do j = i + 1, n
                    if ((sString_g(j:j) == '''') .OR. (sString_g(j:j) == '"')) then
                        sString_g(j:j) = ' '
                        k = j + 1
                        go to 10
                    end if
                    if (sString_g(j:j) == ' ') sString_g(j:j) = bb
                end do
                go to 100
            end if
        end do

100     continue
        return

    end subroutine spacesub

    !     Last change:  JD   24 Aug 2001    4:15 pm
    subroutine char2time(ifail, atime, hh, mm, ss, ignore_24)

! -- Subroutine CHAR2TIME extracts the time from a string.

! -- Arguments are as follows:-
!       ifail:     indicates failure if returned as non-zero
!       atime:     a string containing the time in ASCII format
!       hh,mm,ss   hours, minutes and seconds extracted from the atime string.

! -- Revision history:-
!       June-November, 1995: version 1.

        integer, intent(out) :: ifail
        character(len=*), intent(in) :: atime
        integer, intent(out) :: hh, mm, ss
        integer, optional, intent(in) :: ignore_24
        integer :: lentime, i, j, ig_24
        character(len=2) :: asep
        character(len=20) :: btime

        ifail = 0
        if (.NOT. PRESENT(ignore_24)) then
            ig_24 = 0
        else
            ig_24 = ignore_24
        end if

        asep = ':.'
        if (atime == ' ') go to 9000
        btime = ADJUSTL(atime)
        lentime = LEN_TRIM(btime)
        if (lentime < 5) go to 9000

        do i = 1, lentime
            if (INDEX(asep, btime(i:i)) /= 0) go to 20
        end do
        go to 9000

! -- The first integer is extracted from the string. This represents hours.

20      if (i == 1) go to 9000
        call char2num(ifail, btime(1:i - 1), hh)
        if (ifail /= 0) go to 9000
        if (ig_24 == 0) then
            if ((hh < 0) .OR. (hh > 23)) go to 9000
        else
            if ((hh < 0) .OR. (hh > 24)) go to 9000
        end if

        i = i + 1
        if (lentime - i < 2) go to 9000
        do j = i, lentime
            if (INDEX(asep, btime(j:j)) /= 0) go to 40
        end do
        go to 9000

! -- The second integer (representing minutes) is extracted from the string.

40      if (j == i) go to 9000
        call char2num(ifail, btime(i:j - 1), mm)
        if (ifail /= 0) go to 9000
        if ((mm < 0) .OR. (mm > 59)) go to 9000

! -- The third integer (representing seconds) is extracted from the string.

        j = j + 1
        if (lentime - j < 0) go to 9000
        call char2num(ifail, btime(j:lentime), ss)
        if (ifail /= 0) go to 9000
        if ((ss < 0) .OR. (ss > 59)) go to 9000

        if (ig_24 /= 0) then
            if (hh == 24) then
                if ((mm /= 0) .OR. (ss /= 0)) go to 9000
            end if
        end if
        ifail = 0
        return

9000    ifail = 1
        return

    end subroutine char2time

    subroutine char2date(ifail, adate, dd, mm, yy)

! -- Subroutine CHAR2DATE extracts the date from a string.

! -- Arguments are as follows:-
!      ifail:      returns a non-zero value if an error condition is encountered
!      adate:      the string containing the date
!      dd,mm,yy    the day, month and year read from the date string

! --  Revision history:-
!       June-November, 1995: version 1.

        integer, intent(out) :: ifail
        character(len=*), intent(in) :: adate
        integer, intent(out) :: dd, mm, yy
        integer :: lendate, i, j
        character(len=2) :: asep
        character(len=20) :: bdate

        ifail = 0
        asep = ':/'
        if (adate == ' ') go to 9000
        bdate = ADJUSTL(adate)
        lendate = LEN_TRIM(bdate)
        if (lendate < 8) go to 9000

        do i = 1, lendate
            if (INDEX(asep, bdate(i:i)) /= 0) go to 20
        end do
        go to 9000

! -- The first integer is extracted from the date string. This is either days
!    or months depending on the contents of file settings.fig.

20      if (i == 1) go to 9000
        if (datespec /= 1) then
            call char2num(ifail, bdate(1:i - 1), mm)
        else
            call char2num(ifail, bdate(1:i - 1), dd)
        end if
        if (ifail /= 0) go to 9000

        i = i + 1
        if (lendate - i < 5) go to 9000
        do j = i, lendate
            if (INDEX(asep, bdate(j:j)) /= 0) go to 40
        end do
        go to 9000

! -- The second integer is extracted from the date string. This is either months
!    or days depending on the contents of file settings.fig.

40      if (j == i) go to 9000
        if (datespec /= 1) then
            call char2num(ifail, bdate(i:j - 1), dd)
        else
            call char2num(ifail, bdate(i:j - 1), mm)
        end if
        if (ifail /= 0) go to 9000
        if ((dd <= 0) .OR. (dd > 31)) go to 9000
        if ((mm <= 0) .OR. (mm > 12)) go to 9000
        if (dd == 31) then
            if ((mm == 2) .OR. (mm == 4) .OR. (mm == 6) .OR. (mm == 9) .OR. &
                (mm == 11)) go to 9000
        end if
        if ((mm == 2) .AND. (dd == 30)) go to 9000

! -- The third integer is extracted from the date string. This is years.

        j = j + 1
        if (lendate - j /= 3) go to 9000
        call char2num(ifail, bdate(j:lendate), yy)
        if (ifail /= 0) go to 9000
        if (.NOT. leap(yy)) then
            if ((mm == 2) .AND. (dd == 29)) go to 9000
        end if
        ifail = 0
        return

9000    ifail = 1
        return

    end subroutine char2date

!*****************************************************************************
! subroutines comprising the generic subroutine CHAR2NUM ------->
!*****************************************************************************

! -- The subroutines comprising char2num convert a string to either an integer,
!    a real number, or a double precision number.

! -- Arguments are as follows:-
!      ifail:   indicates failure if returned as non-zero
!      string:  a character string containing a number
!      num:     an integer (for a2i), real (for a2r), or double precision (for
!               a2d) number extracted from the string.

! -- Revision history:-
!       June-November, 1995: version 1.

    subroutine a2i(ifail, string, num)

        integer, intent(out) :: ifail
        character(len=*), intent(in) :: string
        integer, intent(out) :: num
        character(len=10) :: afmt

        if (string == ' ') go to 10
        ifail = 0
        afmt = '(i    )'
        write (afmt(3:6), '(i4)') LEN(string)
        read (string, afmt, err=10) num
        return

10      ifail = 1
        return

    end subroutine a2i

    subroutine a2r(ifail, string, num)

        integer, intent(out) :: ifail
        character(len=*), intent(in) :: string
        real, intent(out) :: num
        character(len=10) :: afmt

        if (string == ' ') go to 10
        ifail = 0
        afmt = '(f    .0)'
        write (afmt(3:6), '(i4)') LEN(string)
        read (string, afmt, err=10) num
        return

10      ifail = 1
        return

    end subroutine a2r

    subroutine a2d(ifail, string, num)

        integer, intent(out) :: ifail
        character(len=*), intent(in) :: string
        double precision, intent(out) :: num
        character(len=10) :: afmt

        if (string == ' ') go to 10
        ifail = 0
        afmt = '(f    .0)'
        write (afmt(3:6), '(i4)') LEN(string)
        read (string, afmt, err=10) num
        return

10      ifail = 1
        return

    end subroutine a2d

    subroutine casetrans(string, hi_or_lo)

! -- Subroutine casetrans converts a string to upper or lower case.

! -- Arguments are as follows:-
!      string:     contains the string whose case must be changed
!      hi_or_lo:  must be either 'lo' or 'hi' to indicate
!                 change of case direction.

! -- Revision history:-
!       June-November, 1995: version 1.

        character(len=*), intent(inout) :: string
        character(len=*), intent(in) :: hi_or_lo
        character :: alo, ahi
        integer :: inc, i

        if (hi_or_lo == 'lo') then
            alo = 'A'; ahi = 'Z'; inc = IACHAR('a') - IACHAR('A')
        else if (hi_or_lo == 'hi') then
            alo = 'a'; ahi = 'z'; inc = IACHAR('A') - IACHAR('a')
        else
            call sub_error('CASETRANS')
        end if

        do i = 1, LEN_TRIM(string)
            if ((string(i:i) >= alo) .AND. (string(i:i) <= ahi)) &
                string(i:i) = ACHAR(IACHAR(string(i:i)) + inc)
        end do

        return

    end subroutine casetrans

!     Last change:  JD   31 Jul 2001   10:05 am
    function isspace(sString_g)

! -- Subroutine ISSPACE checks whether there is a space within a string.

! -- Arguments are as follows:-
!        sString_g:     the name of the string to be checked.

        character(len=*), intent(in) :: sString_g
        logical :: isspace

        if (INDEX(TRIM(sString_g), ' ') == 0) then
            isspace = .FALSE.
        else
            isspace = .TRUE.
        end if

        return
    end function isspace

    function leap(year)

! -- Function LEAP returns .true. if a year is a leap year.

! -- Revision history:-
!       June-November, 1995: version 1.

        integer, intent(in) :: year
        logical :: leap

        leap = (MOD(year, 4) == 0 .AND. MOD(year, 100) /= 0) .OR. &
               (MOD(year, 400) == 0 .AND. year /= 0)

        return
    end function leap

!*****************************************************************************
! subprograms for reading and parsing data (mainly from files) ------->
!*****************************************************************************

    subroutine linesplit(ifail, num)

! -- subroutine linesplit splits a line into whitespace-delimited words

! -- Arguments are as follows:-
!       ifail:   returned as -1 if line is blank
!                returned as  1 if less than num segments
!       num:     number of words to be extracted

!    Revision history:-
!       June-November, 1995: version 1.

        integer, intent(out) :: ifail
        integer, intent(in) :: num
        integer :: nblc, j, i, nw
        character(len=3) :: aspace

        ifail = 0; nw = 0; j = 0
        aspace = ' ,'//ACHAR(9)
        if (num > NUM_WORD_DIM) call sub_error('LINESPLIT')
        nblc = LEN_TRIM(cline)
        if (nblc == 0) then
            ifail = -1
            return
        end if

5       if (nw == num) return

        ! scan through string; jump to line 20 if space, comma or tab found
        do i = j + 1, nblc
            if (INDEX(aspace, cline(i:i)) == 0) go to 20
        end do
        ifail = 1
        return

        ! scan for beginning (LEFT) character of current word
20      nw = nw + 1
        left_word(nw) = i
        do i = left_word(nw) + 1, nblc
            if (INDEX(aspace, cline(i:i)) /= 0) go to 40
        end do
        right_word(nw) = nblc
        if (nw < num) ifail = 1
        return

        ! record ending character (RIGHT) of current word
40      right_word(nw) = i - 1
        j = right_word(nw)

        go to 5

    end subroutine linesplit

    integer function char2int(ifail, num)

! -- Function char2int extracts an integer from a word demarcated by subroutine
!    linesplit.

! -- Arguments are as follows:-
!       ifail:    returned as zero unless an error condition arises
!       num:      the number of the word previously extracted by linesplit
!       returns   value of integer read from word

! -- Revision history:-
!       June-November, 1995: version 1.

        integer, intent(in) :: num
        integer, intent(out) :: ifail
        character(len=8) :: afmt

        if (num > NUM_WORD_DIM) call sub_error('CHAR2INT')
        if ((right_word(num) < left_word(num)) .OR. &
            (left_word(num) <= 0)) call sub_error('CHAR2INT')

        ifail = 0
        afmt = '(i   )'
        write (afmt(3:5), '(i3)') right_word(num) - left_word(num) + 1
        read (cline(left_word(num):right_word(num)), afmt, err=100) char2int
        return

100     ifail = 1
        return

    end function char2int

    real function char2real(ifail, num)

! -- Function char2real extracts a real number from a word demarcated by
!    subroutine linesplit.

! -- Arguments are as follows:-
!       ifail:    returned as zero unless an error condition arises
!       num:      the number of the word previously extracted by linesplit
!       returns   value of real number read from word

! -- Revision history:-
!       June-November, 1995: version 1.

        integer, intent(in) :: num
        integer, intent(out) :: ifail
        integer :: ierr
        character(len=10) :: afmt

        if (num > NUM_WORD_DIM) call sub_error('CHAR2REAL')
        if ((right_word(num) < left_word(num)) .OR. &
            (left_word(num) <= 0)) call sub_error('CHAR2REAL')

        ifail = 0
        afmt = '(f   .0)'
        write (afmt(3:5), '(i3)') right_word(num) - left_word(num) + 1
        read (cline(left_word(num):right_word(num)), afmt, iostat=ierr) char2real
        if (ierr /= 0) go to 110
        return

110     ifail = 1
        return

    end function char2real

    double precision function char2double(ifail, num)

! -- Function char2double extracts a double precision number from a word
!    demarcated by subroutine linesplit.

! -- Arguments are as follows:-
!       ifail:    returned as zero unless an error condition arises
!       num:      the number of the word previously extracted by linesplit
!       returns   value of double precision number read from word

! -- Revision history:-
!       June-November, 1995: version 1.

        integer, intent(in) :: num
        integer, intent(out) :: ifail
        integer :: ierr
        character(len=10) :: afmt

        if (num > NUM_WORD_DIM) call sub_error('CHAR2DOUBLE')
        if ((right_word(num) < left_word(num)) .OR. &
            (left_word(num) <= 0)) call sub_error('CHAR2DOUBLE')

        ifail = 0
        afmt = '(f   .0)'
        write (afmt(3:5), '(i3)') right_word(num) - left_word(num) + 1
        read (cline(left_word(num):right_word(num)), afmt, iostat=ierr) char2double
        if (ierr /= 0) go to 110
        return

110     ifail = 1
        return

    end function char2double

    subroutine newdate(ndays, day1, mon1, year1, day2, mon2, year2)

! -- Subroutine NEWDATE evaluates the date after NDAYS days have elapsed from
!    a provided date. NDAYS may be negative.

! -- Arguments are as follows:-
!       ndays:            elapsed number of days
!       day1,mon1,year1:  days, month and year of first date
!       day2,mon2,year2:  days, month and year of second date

! -- Revision history:-
!       June-November, 1995: version 1.

        implicit none

        integer, intent(in) :: ndays, day1, mon1, year1
        integer, intent(out) :: day2, mon2, year2

        ! [ LOCALS ]
        integer :: iJulianDate1

        ! determine Julian Date for provided date
        iJulianDate1 = julian_day(iYear=year1, &
                                  iMonth=mon1, &
                                  iDay=day1)

        ! add ndays to JD
        iJulianDate1 = iJulianDate1 + ndays

        ! back calculate gregorian date
        call gregorian_date(iJD=iJulianDate1, &
                            iYear=year2, &
                            iMonth=mon2, &
                            iDay=day2)

!    integer  :: yearref,newdays,idays,iyear,jdays,i
!    integer, dimension(12) :: monthdays
!
!    data monthdays /31,28,31,30,31,30,31,31,30,31,30,31/
!
! ! -- First a reference date is chosen. This is the beginning of the first
! ! -- year. Alternatively the reference date is the beginning of a year prior
! ! -- to the likely calculated date if NDAYS is negative.
!
!    if(ndays.ge.0) then
!      yearref=year1
!    else
!      yearref=year1-abs(ndays)/365-1
!    end if
!    newdays=numdays(31,12,yearref-1,day1,mon1,year1)
!    newdays=ndays+newdays
!    if(newdays.lt.0) call sub_error('NEWDATE')
!
! ! -- Next days are counted, starting at the new reference date.
!
!    idays=0
!    iyear=yearref
!    do
!      jdays=idays+365
!      if(leap(iyear)) jdays=jdays+1
!      if(jdays.ge.newdays) go to 20
!      iyear=iyear+1
!      idays=jdays
!    end do
!    call sub_error('NEWDATE')
! 20      year2=iyear
!
!    do i=1,12
!      jdays=idays+monthdays(i)
!      if((i.eq.2).and.(leap(year2))) jdays=jdays+1
!      if(jdays.ge.newdays) go to 40
!      idays=jdays
!    end do
!    call sub_error('NEWDATE')
! 40      mon2=i
!    day2=newdays-idays
! !   if((day2.le.0).or.(mon2.le.0).or.(year2.le.0)) call sub_error('NEWDATE')
!    if((day2.le.0).or.(mon2.le.0)) call sub_error('NEWDATE')
!    return

    end subroutine newdate

    integer function nextunit()

! -- Function nextunit determines the lowest unit number available for
! -- opening.

! -- Revision history:-
!       June-November, 1995: version 1.

        logical :: lopen

        do nextunit = 20, 100
            inquire (unit=nextunit, opened=lopen)
            if (.NOT. lopen) return
        end do
        write (6, 10)
10      format(' *** No more unit numbers to open files ***')
        stop

    end function nextunit

!*****************************************************************************
! subroutines comprising the generic subroutine NUM2CHAR ------->
!*****************************************************************************

! -- Subroutine num2char writes the character equivalent of a number.

! -- Arguments are as follows:-
!       value:   the number to be expressed in character form
!       string:  the number expressed in character form
!       nchar:   the maximum number of characters in which to express number

! -- Revision history:-
!       June-November, 1995: version 1.

    subroutine i2a(value, string, nchar)

        integer, intent(in) :: value
        character(len=*), intent(out) :: string
        integer, intent(in), optional :: nchar
        character(len=12) :: afmt
        integer :: llen

        string = ' '
        afmt = '(i    )'
        llen = MIN(30, LEN(string))
        if (PRESENT(nchar)) llen = MIN(llen, nchar)
        write (afmt(3:6), '(i4)') llen
        write (string(1:llen), afmt, err=100) value
        string = ADJUSTL(string)
        if (string(1:1) == '*') go to 100
        return

100     string(1:llen) = REPEAT('#', llen)
        return

    end subroutine i2a

    subroutine d2a(value, string, nchar)

        double precision, intent(in) :: value
        character(len=*), intent(out) :: string
        integer, intent(in), optional :: nchar
        integer :: llen, ifail
        double precision :: value_check
        character(len=32) :: word

        string = ' '
        llen = MIN(29, LEN(string))
        if (PRESENT(nchar)) llen = MIN(llen, nchar)
        call wrtsig(ifail, value, word, llen, 1, value_check, 0)
        if (ifail < 0) then
            call sub_error('D2A')
        else if (ifail > 0) then
            string(1:llen) = REPEAT('#', llen)
        else
            string = ADJUSTL(word)
        end if
        return

    end subroutine d2a

    subroutine r2a(value, string, nchar)

        real, intent(in) :: value
        character(len=*), intent(out) :: string
        integer, intent(in), optional :: nchar
        integer :: llen, ifail
        double precision :: dvalue, dvalue_check
        character(len=32) :: word

        string = ' '
        llen = MIN(29, LEN(string))
        if (PRESENT(nchar)) llen = MIN(llen, nchar)
        dvalue = value
        call wrtsig(ifail, dvalue, word, llen, 0, dvalue_check, 0)
        if (ifail < 0) then
            call sub_error('R2A')
        else if (ifail > 0) then
            string(1:llen) = REPEAT('#', llen)
        else
            string = ADJUSTL(word)
        end if
        return

    end subroutine r2a

!> Convert an integer value into a formatted character string
    function int2char(iValue) result(sBuf)

        integer(kind=T_INT) :: iValue
        character(len=256) :: sBuf

        write (UNIT=sBuf, FMT="(i14)") iValue
        sBuf = ADJUSTL(sBuf)

        return

    end function int2char

!--------------------------------------------------------------------------

!> Convert a real value into a formatted character string
    function real2char(rValue, iDec, iWidth) result(sBuf)

        real(kind=T_SGL) :: rValue
        integer(kind=T_INT), optional :: iDec
        integer(kind=T_INT), optional :: iWidth

        ![ LOCALS ]
        character(len=256) :: sBuf, sFmt
        integer(kind=T_INT) :: iD, iW

        if (PRESENT(iDec)) then
            iD = iDec
        else
            iD = 4
        end if

        if (PRESENT(iWidth)) then
            iW = iWidth
        else
            iW = 16
        end if

        if (ABS(rValue) < rNEAR_ZERO) then
            sBuf = "0."
        else
            sFmt = "(G"//TRIM(int2char(iW))//"."//TRIM(int2char(iD))//")"
            write (UNIT=sBuf, FMT=TRIM(sFmt)) rValue
            sBuf = ADJUSTL(sBuf)
        end if

    end function real2char

!--------------------------------------------------------------------------

!> Convert a double value into a formatted character string
    function double2char(rValue, iDec, iWidth) result(sBuf)

        real(kind=T_DBL) :: rValue
        integer(kind=T_INT), optional :: iDec
        integer(kind=T_INT), optional :: iWidth

        ![ LOCALS ]
        character(len=256) :: sBuf, sFmt
        integer(kind=T_INT) :: iD, iW

        if (PRESENT(iDec)) then
            iD = iDec
        else
            iD = 12
        end if

        if (PRESENT(iWidth)) then
            iW = iWidth
        else
            iW = 16
        end if

        sFmt = "(G"//TRIM(int2char(iW))//"."//TRIM(int2char(iD))//")"
        write (UNIT=sBuf, FMT=TRIM(sFmt)) rValue
        sBuf = ADJUSTL(sBuf)

    end function double2char

!--------------------------------------------------------------------------

    subroutine WRTSIG(IFAIL, VAL, WORD, NW, PRECIS, TVAL, NOPNT)
! --
! -- SUBROUTINE WRTSIG WRITES A NUMBER INTO A CONFINED SPACE WITH MAXIMUM
! -- PRECISION
! --

! -- Revision history:-
!       July, 1993: version 1.
!       August 1994: modified for unix version (#ifdef's added)
!       August, 1995: #ifdefs commented out for inclusion in Groundwater
!                     Data Utilities

!       failure criteria:
!           ifail= 1 ...... number too large or small for single precision type
!           ifail= 2 ...... number too large or small for double precision type
!           ifail= 3 ...... field width too small to represent number
!           ifail=-1 ...... internal error type 1
!           ifail=-2 ...... internal error type 2
!           ifail=-3 ...... internal error type 3

        integer PRECIS, LW, POS, INC, D, P, W, J, JJ, K, JEXP, N, JFAIL, NW, &
            EPOS, PP, NOPNT, KEXP, IFLAG, LEXP
        integer IFAIL
        double precision VAL, TVAL
        character(29) TWORD, TTWORD, FMT(14)
        character * (*) WORD

        LEXP = 0
        IFLAG = 0
        WORD = ' '
        POS = 1
        if (VAL < 0.0D0) POS = 0
!#ifdef USE_D_FORMAT
!        WRITE(TWORD,'(1PD23.15D3)') VAL
!#else
        write (TWORD, '(1PE23.15E3)') VAL
!#endif
        read (TWORD(20:23), '(I4)') JEXP
        EPOS = 1
        if (JEXP < 0) EPOS = 0

        JFAIL = 0
        IFAIL = 0
        if (PRECIS == 0) then
            LW = MIN(15, NW)
        else
            LW = MIN(23, NW)
        end if

        N = 0
        if (NOPNT == 1) N = N + 1
        if (POS == 1) N = N + 1
        if (PRECIS == 0) then
            if (ABS(JEXP) > 38) then
                IFAIL = 1
                return
            end if
            if (POS == 1) then
                if (LW >= 13) then
                    write (WORD, '(1PE13.7)', ERR=80) VAL
                    GO TO 200
                end if
            else
                if (LW >= 14) then
                    write (WORD, '(1PE14.7)', ERR=80) VAL
                    GO TO 200
                end if
            end if
            if (LW >= 14 - N) then
                LW = 14 - N
                GO TO 80
            end if
        else
            if (ABS(JEXP) > 275) then
                IFAIL = 2
                return
            end if
            if (POS == 1) then
                if (LW >= 22) then
!#ifdef USE_D_FORMAT
!              WRITE(WORD,'(1PD22.15D3)',ERR=80) VAL
!#else
                    write (WORD, '(1PE22.15E3)', ERR=80) VAL
!#endif
                    GO TO 200
                end if
            else
                if (LW >= 23) then
!#ifdef USE_D_FORMAT
!              WRITE(WORD,'(1PD23.15D3)',ERR=80) VAL
!#else
                    write (WORD, '(1PE23.15E3)', ERR=80) VAL
!#endif
                    GO TO 200
                end if
            end if
            if (LW >= 23 - N) then
                LW = 23 - N
                GO TO 80
            end if
        end if

        if (NOPNT == 1) then
            if ((JEXP == LW - 2 + POS) .OR. (JEXP == LW - 3 + POS)) then
                write (FMT, 15) LW + 1
15              format('(F', I2, '.0)')
                write (WORD, FMT, ERR=19) VAL
                if (INDEX(WORD, '*') /= 0) GO TO 19
                if (WORD(1:1) == ' ') GO TO 19
                WORD(LW + 1:LW + 1) = ' '
                GO TO 200
            end if
        end if
19      D = MIN(LW - 2 + POS, LW - JEXP - 3 + POS)
20      if (D < 0) GO TO 80
        write (FMT, 30) LW, D
30      format('(F', I2, '.', I2, ')')
        write (WORD, FMT, ERR=80) VAL
        if (INDEX(WORD, '*') /= 0) then
            D = D - 1
            GO TO 20
        end if
        K = INDEX(WORD, '.')
        if (K == 0) then
            IFAIL = -1
            return
        end if
        if ((K == 1) .OR. ((POS == 0) .AND. (K == 2))) then
            do J = 1, 3
                if (K + J > LW) GO TO 75
                if (WORD(K + J:K + J) /= '0') GO TO 200
            end do
            GO TO 80
75          IFAIL = 3
            return
        end if
        GO TO 200

80      WORD = ' '
        if (NOPNT == 0) then
            D = LW - 7
            if (POS == 1) D = D + 1
            if (EPOS == 1) D = D + 1
            if (ABS(JEXP) < 100) D = D + 1
            if (ABS(JEXP) < 10) D = D + 1
            if ((JEXP >= 100) .AND. (JEXP - (D - 1) < 100)) then
                P = 1 + (JEXP - 99)
                D = D + 1
                LEXP = 99
            else if ((JEXP >= 10) .AND. (JEXP - (D - 1) < 10)) then
                P = 1 + (JEXP - 9)
                D = D + 1
                LEXP = 9
            else if ((JEXP == -10) .OR. (JEXP == -100)) then
                IFLAG = 1
                D = D + 1
            else
                P = 1
            end if
            INC = 0
85          if (D <= 0) GO TO 300
            if (IFLAG == 0) then
                write (FMT, 100, ERR=300) P, D + 7, D - 1
            else
                write (FMT, 100, ERR=300) 0, D + 8, D
            end if
            write (TWORD, FMT) VAL
            if (IFLAG == 1) GO TO 87
            read (TWORD(D + 4:D + 7), '(I4)', ERR=500) KEXP
            if (((KEXP == 10) .AND. ((JEXP == 9) .OR. (LEXP == 9))) .OR. &
                ((KEXP == 100) .AND. ((JEXP == 99) .OR. LEXP == 99))) then
                if (INC == 0) then
                    if (LEXP == 0) then
                        if (D - 1 == 0) then
                            D = D - 1
                        else
                            P = P + 1
                        end if
                    else if (LEXP == 9) then
                        if (JEXP - (D - 2) < 10) then
                            P = P + 1
                        else
                            D = D - 1
                        end if
                    else if (LEXP == 99) then
                        if (JEXP - (D - 2) < 100) then
                            P = P + 1
                        else
                            D = D - 1
                        end if
                    end if
                    INC = INC + 1
                    GO TO 85
                end if
            end if
!#ifdef USE_D_FORMAT
!87        J=INDEX(TWORD,'D')
!#else
87          J = INDEX(TWORD, 'E')
!#endif
            GO TO 151
        end if
        INC = 0
        P = LW - 2
        PP = JEXP - (P - 1)
        if (PP >= 10) then
            P = P - 1
            if (PP >= 100) P = P - 1
        else if (PP < 0) then
            P = P - 1
            if (PP <= -10) then
                P = P - 1
                if (PP <= -100) P = P - 1
            end if
        end if
        if (POS == 0) P = P - 1
90      continue
        D = P - 1
        W = D + 8
        write (FMT, 100) P, W, D
        if (D < 0) then
            if (JFAIL == 1) GO TO 300
            JFAIL = 1
            P = P + 1
            GO TO 90
        end if
!#ifdef USE_D_FORMAT
!100     FORMAT('(',I2,'pD',I2,'.',I2,'D3)')
!#else
100     format('(', I2, 'pE', I2, '.', I2, 'E3)')
!#endif
        write (TWORD, FMT) VAL
!#ifdef USE_D_FORMAT
!        J=INDEX(TWORD,'D')
!#else
        J = INDEX(TWORD, 'E')
!#endif
        if (TWORD(J - 1:J - 1) /= '.') then
            IFAIL = -1
            return
        end if
        N = 1
        if (TWORD(J + 1:J + 1) == '-') N = N + 1
        if (TWORD(J + 2:J + 2) /= '0') then
            N = N + 2
            GO TO 120
        end if
        if (TWORD(J + 3:J + 3) /= '0') N = N + 1
120     N = N + 1
        if (J + N - 2 - POS < LW) then
            if (INC == -1) GO TO 150
            TTWORD = TWORD
            P = P + 1
            INC = 1
            GO TO 90
        else if (J + N - 2 - POS == LW) then
            GO TO 150
        else
            if (INC == 1) then
                TWORD = TTWORD
                GO TO 150
            end if
            if (JFAIL == 1) GO TO 300
            P = P - 1
            INC = -1
            GO TO 90
        end if

150     J = INDEX(TWORD, '.')
151     if (POS == 0) then
            K = 1
        else
            K = 2
        end if
        WORD(1:J - K) = TWORD(K:J - 1)
        JJ = J
        J = J - K + 1
        if (PRECIS == 0) then
            WORD(J:J) = 'E'
        else
            WORD(J:J) = 'D'
        end if
        JJ = JJ + 2
        if (NOPNT == 0) JJ = JJ - 1
        if (TWORD(JJ:JJ) == '-') then
            J = J + 1
            WORD(J:J) = '-'
        end if
        if (TWORD(JJ + 1:JJ + 1) /= '0') then
            J = J + 2
            WORD(J - 1:J) = TWORD(JJ + 1:JJ + 2)
            GO TO 180
        end if
        if (TWORD(JJ + 2:JJ + 2) /= '0') then
            J = J + 1
            WORD(J:J) = TWORD(JJ + 2:JJ + 2)
        end if
180     J = J + 1
        WORD(J:J) = TWORD(JJ + 3:JJ + 3)
        if (IFLAG == 1) then
            if (POS == 1) then
                JJ = 1
            else
                JJ = 2
            end if
            N = LEN_TRIM(WORD)
            do J = JJ, N - 1
                WORD(J:J) = WORD(J + 1:J + 1)
            end do
            WORD(N:N) = ' '
        end if

200     if (LEN_TRIM(WORD) > LW) then
            IFAIL = -2
            return
        end if
        write (FMT, 30) LW, 0
        read (WORD, FMT, ERR=400) TVAL
        return
300     IFAIL = 3
        return
400     IFAIL = -3
        return
500     IFAIL = -2
        return
    end subroutine WRTSIG

    integer function numdays(DR, MR, YR, D, M, Y)

! -- Function numdays calculates the number of days between dates
!    D-M-Y and DR-MR-YR. If the former preceeds the latter the answer is
!    negative.

! -- Arguments are as follows:-
!       dr,mr,yr:     days, months and years of first date
!       d,m,y:        days, months and years of second date
!       numdays returns the number of elapsed days

! -- Revision history:-
!       22 July 1994:  version 1
!       13 September 1995:  modified for Groundwater Data Utilities

        integer, intent(in) :: dr, mr, yr, d, m, y

        ! [ LOCALS ]
        integer :: iJulianDate1, iJulianDate2

        iJulianDate1 = julian_day(iYear=yr, &
                                  iMonth=mr, &
                                  iDay=dr)

        iJulianDate2 = julian_day(iYear=y, &
                                  iMonth=m, &
                                  iDay=d)

        numdays = iJulianDate2 - iJulianDate1

!   INTEGER FLAG,I,J,DA(12),YE,ME,DE,YL,ML,DL
!
!    DATA DA /31,28,31,30,31,30,31,31,30,31,30,31/
!
! ! --    THE SMALLER OF THE TWO DATES IS NOW CHOSEN TO DO THE COUNTING FROM.
!
!    IF(Y.LT.YR)GO TO 10
!    IF((Y.EQ.YR).AND.(M.LT.MR)) GO TO 10
!    IF((Y.EQ.YR).AND.(M.EQ.MR).AND.(D.LT.DR)) GO TO 10
!    FLAG=0
!    YE=YR
!    ME=MR
!    DE=DR
!    YL=Y
!    ML=M
!    DL=D
!    GO TO 20
! 10      FLAG=1
!    YE=Y
!    ME=M
!    DE=D
!    YL=YR
!    ML=MR
!    DL=DR
!
! ! --    IN THE ABOVE THE POSTSCRIPT "E" STANDS FOR EARLIER DATE, WHILE
! !       "L" STANDS FOR THE LATER DATE.
!
! 20      numdays=0
!    IF((ME.EQ.ML).AND.(YL.EQ.YE))THEN
!    numdays=DL-DE
!    IF(FLAG.EQ.1) numdays=-numdays
!    RETURN
!    END IF
!
!    DO 30 J=ME,12
!    IF((ML.EQ.J).AND.(YE.EQ.YL))GOTO 40
!    numdays=numdays+DA(J)
!    IF((J.EQ.2).AND.(leap(ye)))numdays=numdays+1
! 30      CONTINUE
!    GO TO 50
! 40      numdays=numdays+DL-DE
!    IF(FLAG.EQ.1)numdays=-numdays
!    RETURN
!
! 50      DO 60 I=YE+1,YL
!    DO 70 J=1,12
!    IF((YL.EQ.I).AND.(ML.EQ.J))GO TO 80
!    numdays=numdays+DA(J)
!    IF((J.EQ.2).AND.(leap(i))) numdays=numdays+1
! 70      CONTINUE
! 60      CONTINUE
!    call sub_error('NUMDAYS')
!    RETURN
!
! 80      numdays=numdays+DL-DE
!    IF(FLAG.EQ.1) numdays=-numdays
!
!    RETURN
    end function numdays

    integer function numsecs(h1, m1, s1, h2, m2, s2)

! -- Subroutine NUMSECS calculates the number of seconds between two times.

! -- Arguments are as follows:-
!       h1,m1,s1:   hours, minutes seconds of first time
!       h2,m2,y2:   hours, minutes seconds of second time

! -- Revision history:-
!       June-November 1995: version 1.

        integer, intent(in) :: h1, m1, s1, h2, m2, s2

        numsecs = (h2 - h1) * 3600 + (m2 - m1) * 60 + s2 - s1

    end function numsecs

!=======================================================================

!=======================================================================
!     Last change:  JD   29 Jun 2001    4:34 pm
    subroutine read_rest_of_sample_line(ifail, cols, ndays, nsecs, value, ILine_g, sampfile)

! -- Subroutine read_rest_of_sample_line reads the date, time, value and
!    optional fifth column from a line of a site sample file.

! -- Arguments are as follows:-
!       ifail:     returned as zero unless an error condition is encountered
!       cols:      number of data columns in the line
!       ndays:     number of days from 1/1/1970 until sample date
!       nsecs:     number of seconds from midnight until sample time
!       value:     sample value
!       ILine_g:     current line number of site sample file
!       sampfile:  name of site sample file

! -- Revision history:-
!       June-November, 1995: version 1.

        integer, intent(out) :: ifail
        integer, intent(in) :: cols
        integer, intent(out) :: ndays, nsecs
        double precision, intent(out) :: value
        integer, intent(in) :: ILine_g
        character(len=*), intent(in) :: sampfile
        integer :: dd, mm, yy, hhh, mmm, sss
        character(len=15) :: aline
        character(len=2) :: aa

        ifail = 0
        call char2date(ifail, cline(left_word(2):right_word(2)), dd, mm, yy)
        if (ifail /= 0) then
            call num2char(ILine_g, aline)
            write (amessage, 150) TRIM(aline), TRIM(sampfile)
150         format('illegal date at line ', a, ' of site sample file ', a)
            call write_message(error='yes', leadspace='yes')
            go to 9800
        end if
!   ndays=numdays(1,1,1970,dd,mm,yy)
        ndays = julian_day(iMonth=mm, iDay=dd, iYear=yy)

        call char2time(ifail, cline(left_word(3):right_word(3)), hhh, mmm, sss)
        if (ifail /= 0) then
            call num2char(ILine_g, aline)
            write (amessage, 160) TRIM(aline), TRIM(sampfile)
160         format('illegal time at line ', a, ' of site sample file ', a)
            call write_message(error='yes', leadspace='yes')
            go to 9800
        end if
        nsecs = numsecs(0, 0, 0, hhh, mmm, sss)

        value = char2double(ifail, 4)
        if (ifail /= 0) then
            call num2char(ILine_g, aline)
            write (amessage, 180) TRIM(aline), TRIM(sampfile)
180         format('cannot read sample value at line ', a, ' of site sample file ', a)
            call write_message(error='yes', leadspace='yes')
            go to 9800
        end if
        if (value < -1.0E37) then
            call num2char(ILine_g, aline)
            write (amessage, 190) TRIM(aline), TRIM(sampfile)
190         format('illegal sample value at line ', a, ' of site sample file ', a, &
                   '; lower limit is -1.0E37.')
            call write_message(error='yes', leadspace='yes')
            go to 9800
        end if
        if (cols == 5) then
            aa = cline(left_word(5):right_word(5))
            call casetrans(aa, 'lo')
            if (aa == 'x ') then
                value = -1.1E38
            else
                call num2char(ILine_g, aline)
                write (amessage, 210) TRIM(aline), TRIM(sampfile)
210             format('illegal optional fifth item on line ', a, ' of site sample ', &
                       'file ', a, '; item must be "x" if present.')
                call write_message(error='yes', leadspace='yes')
                go to 9800
            end if
        end if
        return

9800    ifail = 1
        return

    end subroutine read_rest_of_sample_line

    subroutine sub_error(subname)

! -- Subroutine sub_error names the subroutine causing a run-time error.

! -- Arguments are as follows:-
!       subname:  name of offending subroutine

! -- Revision history:-
!       June-November, 1995: version 1.

        character(len=*) :: subname

        write (6, 10) TRIM(subname)
10      format(/, ' *** PROGRAMMING ERROR CALLING SUBROUTINE ', a, ' ***')
        stop

    end subroutine sub_error

    subroutine write_message(increment, iunit, error, leadspace, endspace)

! -- Subroutine write_message formats and writes a message.

! -- Arguments are as follows:-
!       increment:  the increment to the message counter
!       iunit:      the unit number to which the message is written
!       error:      if "yes" precede message with "Error"
!       leadspace   if "yes" precede message with blank line
!       endspace    if "yes" follow message by blank line

! -- Revision history:-
!       June-November, 1995: version 1.

        integer, intent(in), optional :: increment, iunit
        integer :: jend, i, nblc, junit, leadblank
        integer :: itake, j
        character(len=*), intent(in), optional :: error, leadspace, endspace
        character(len=20) ablank

        ablank = ' '
        itake = 0
        j = 0
        if (PRESENT(increment)) imessage = imessage + increment
        if (PRESENT(iunit)) then
            junit = iunit
        else
            junit = 6
        end if
        if (PRESENT(leadspace)) then
            if (leadspace == 'yes') write (junit, *)
        end if
        if (PRESENT(error)) then
            if (INDEX(error, 'yes') /= 0) then
                nblc = LEN_TRIM(amessage)
                amessage = ADJUSTR(amessage(1:nblc + 8))
                if (nblc + 8 < LEN(amessage)) amessage(nblc + 9:) = ' '
                amessage(1:8) = ' Error: '
            end if
        end if

        do i = 1, 20
            if (amessage(i:i) /= ' ') exit
        end do
        leadblank = i - 1
        nblc = LEN_TRIM(amessage)
5       jend = j + 78 - itake
        if (jend >= nblc) go to 100
        do i = jend, j + 1, -1
        if (amessage(i:i) == ' ') then
            if (itake == 0) then
                write (junit, '(a)', err=200) amessage(j + 1:i)
                itake = 2 + leadblank
            else
                write (junit, '(a)', err=200) ablank(1:leadblank + 2)//amessage(j + 1:i)
            end if
            j = i
            go to 5
        end if
        end do
        if (itake == 0) then
            write (junit, '(a)', err=200) amessage(j + 1:jend)
            itake = 2 + leadblank
        else
            write (junit, '(a)', err=200) ablank(1:leadblank + 2)//amessage(j + 1:jend)
        end if
        j = jend
        go to 5
100     jend = nblc
        if (itake == 0) then
            write (junit, '(a)', err=200) amessage(j + 1:jend)
        else
            write (junit, '(a)', err=200) ablank(1:leadblank + 2)//amessage(j + 1:jend)
        end if
        if (PRESENT(endspace)) then
            if (endspace == 'yes') write (junit, *)
        end if
        return

200     stop 100

    end subroutine write_message

    subroutine close_files
        use tsp_data_structures, only: LU_TSPROC_CONTROL
! -- Subroutine close_files closes all open files.

! -- Revision history:-
!       June-November, 1995: version 1.

        integer :: i, ierr

#ifdef UNROLL_CONTROL_FILE
        close (LU_TSPROC_CONTROL, STATUS='DELETE')
#else
        close (LU_TSPROC_CONTROL, STATUS='KEEP')
#endif

        do i = 7, 1000
            close (unit=i, iostat=ierr)
        end do
        return

    end subroutine close_files

!     Last change:  JD   31 Jul 2001   11:51 pm

    subroutine getfile(ifail, cline, filename, ibeg, iend)

! Subroutine getfile extracts a filename from a string.

! -- Arguments are as follows:-
!       ifail: returned as zero if filename successfully read
!       cline: a character string containing the file name
!       filename: the name of the file read from the string
!       ibeg: character position at which to begin search for filename
!       iend: on input  - character position at which to end search for filename
!             on output - character postion at which filename ends

        integer, intent(out) :: ifail
        integer, intent(in) :: ibeg
        integer, intent(inout) :: iend
        character(len=*), intent(in) :: cline
        character(len=*), intent(out) :: filename

        integer :: i, j, k
        character(len=1) :: aa

        ifail = 0
        do i = ibeg, iend
            aa = cline(i:i)
            if ((aa /= ' ') .AND. (aa /= ',') .AND. (aa /= CHAR(9))) go to 50
        end do
        ifail = 1
        return

50      if ((aa == '"') .OR. (aa == '''')) then
!          do j=i+1,iend
            do j = i + 1, LEN_TRIM(cline) !note
                if (cline(j:j) == aa) go to 60
            end do
            ifail = 1
            return
60          iend = j
            if (i + 1 > j - 1) then
                ifail = 1
                return
            else
                filename = cline(i + 1:j - 1)
            end if
        else
            do j = i + 1, iend
                if ((cline(j:j) == ' ') .OR. (cline(j:j) == ',') .OR. (cline(j:j) == CHAR(9))) then
                    k = j - 1
                    go to 100
                end if
            end do
            k = iend
100         filename = cline(i:k)
            if (cline(k:k) == '"') then
                ifail = 1
                return
            else if (cline(k:k) == '''') then
                ifail = 1
                return
            end if

            iend = k
        end if
        filename = ADJUSTL(filename)
        return

    end subroutine getfile

!*****************************************************************************
! functions comprising the generic function NNEG_TEST
!*****************************************************************************

! -- Function nnegtest tests that a number is not negative.

! -- Arguments are as follows:-
!       value:   the number to be tested
!       string:  part of displayed error message if number is -ve.
!       returns  0 if not negative; 1 otherwise

! -- Revision history:-
!       June-November, 1995: version 1.

    integer function nneg_i_test(value, string)

        integer, intent(in) :: value
        character(len=*), intent(in) :: string

        nneg_i_test = 0
        if (value < 0) then
            write (amessage, 10) TRIM(string)
10          format(' Error: ', a, ' must not be negative  - try again.')
            call write_message(0, 6, 'no', 'no', 'no')
            nneg_i_test = 1
        end if
        return

    end function nneg_i_test

    integer function nneg_r_test(value, string)

        real, intent(in) :: value
        character(len=*), intent(in) :: string

        nneg_r_test = 0
        if (value < 0.0) then
            write (amessage, 10) TRIM(string)
10          format(' Error: ', a, ' must not be negative  - try again.')
            call write_message(0, 6, 'no', 'no', 'no')
            nneg_r_test = 1
        end if
        return

    end function nneg_r_test

    integer function nneg_d_test(value, string)

        double precision, intent(in) :: value
        character(len=*), intent(in) :: string

        nneg_d_test = 0
        if (value < 0.0D0) then
            write (amessage, 10) TRIM(string)
10          format(' Error: ', a, ' must not be negative  - try again.')
            call write_message(0, 6, 'no', 'no', 'no')
            nneg_d_test = 1
        end if
        return

    end function nneg_d_test

!*****************************************************************************
! functions comprising the generic function POS_TEST
!*****************************************************************************

! -- Function pos_test tests whether a number is positive.

! -- Arguments are as follows:-
!       value:    the number to be tested
!       string:   part of the displayed error message if string is not +ve
!       returns   0 if positive; 1 otherwise

! -- Revision history:-
!       June-November, 1995: version 1.

    integer function pos_i_test(value, string)

        integer, intent(in) :: value
        character(len=*), intent(in) :: string

        pos_i_test = 0
        if (value <= 0) then
            write (amessage, 10) TRIM(string)
10          format(' Error: ', a, ' must be positive  - try again.')
            call write_message(0, 6, 'no', 'no', 'no')
            pos_i_test = 1
        end if
        return

    end function pos_i_test

    integer function pos_r_test(value, string)

        real, intent(in) :: value
        character(len=*), intent(in) :: string

        pos_r_test = 0
        if (value <= 0.0) then
            write (amessage, 10) TRIM(string)
10          format(' Error: ', a, ' must be positive  - try again.')
            call write_message(0, 6, 'no', 'no', 'no')
            pos_r_test = 1
        end if
        return

    end function pos_r_test

    integer function pos_d_test(value, string)

        double precision, intent(in) :: value
        character(len=*), intent(in) :: string

        pos_d_test = 0
        if (value <= 0.0D0) then
            write (amessage, 10) TRIM(string)
10          format(' Error: ', a, ' must be positive  - try again.')
            call write_message(0, 6, 'no', 'no', 'no')
            pos_d_test = 1
        end if
        return

    end function pos_d_test

!*****************************************************************************
! subroutines comprising the generic subroutine EQUALS ------->
!*****************************************************************************

! -- Subroutine equals compares two numbers of the same kind.

! -- Arguments are as follows:-
!       r1:   the first number
!       r2:   the second number

! -- It returns .TRUE. or .FALSE.

    logical function equals_int(r1, r2)

        integer, intent(in) :: r1
        integer, intent(in) :: r2

        equals_int = (r1 == r2)

    end function equals_int

    logical function equals_real(r1, r2)

        real, intent(in) :: r1
        real, intent(in) :: r2

        real :: rtemp

        rtemp = ABS(3.0 * SPACING(r1))
        if (ABS(r1 - r2) < rtemp) then
            equals_real = .TRUE.
        else
            equals_real = .FALSE.
        end if

    end function equals_real

    logical function equals_dbl(r1, r2)

        real(KIND(1.0D0)), intent(in) :: r1
        real(KIND(1.0D0)), intent(in) :: r2

        real(KIND(1.0D0)) :: rtemp

        rtemp = ABS(3.0 * SPACING(r1))
        if (ABS(r1 - r2) < rtemp) then
            equals_dbl = .TRUE.
        else
            equals_dbl = .FALSE.
        end if

    end function equals_dbl

!*****************************************************************************
! functions comprising the generic function KEY_READ
!*****************************************************************************

! -- Function key_read reads and checks a number from the keyboard.

! -- Arguments are as follows:-
!       value:   value of the number read
!       returns  0 unless an error condition arises

! -- Revision history:-
!       June-November, 1995: version 1.

    integer function int_key_read(value)

        integer, intent(out) :: value
        integer :: ifail
        character(len=50) :: atemp

        int_key_read = 0
        read (5, '(a)') atemp
        if (atemp /= ' ') atemp = ADJUSTL(atemp) ! lf90 bug
        if (atemp == ' ') then
            int_key_read = -1
        else if (INDEX(eschar, atemp(1:2)) /= 0) then
            escset = 1
        else
            call a2i(ifail, atemp, value)
            if (ifail /= 0) int_key_read = 1
        end if
        return

    end function int_key_read

    integer function real_key_read(value)

        real, intent(out) :: value
        integer :: ifail
        character(len=50) :: atemp

        real_key_read = 0
        read (5, '(a)') atemp
        if (atemp /= ' ') atemp = ADJUSTL(atemp) ! lf90 bug
        if (atemp == ' ') then
            real_key_read = -1
        else if (INDEX(eschar, atemp(1:2)) /= 0) then
            escset = 1
        else
            call a2r(ifail, atemp, value)
            if (ifail /= 0) real_key_read = 1
        end if
        return

    end function real_key_read

    integer function double_key_read(value)

        double precision, intent(out) :: value
        integer :: ifail
        character(len=50) :: atemp

        double_key_read = 0
        read (5, '(a)') atemp
        if (atemp /= ' ') atemp = ADJUSTL(atemp) ! lf90 bug
        if (atemp == ' ') then
            double_key_read = -1
        else if (INDEX(eschar, atemp(1:2)) /= 0) then
            escset = 1
        else
            call a2d(ifail, atemp, value)
            if (ifail /= 0) double_key_read = 1
        end if
        return

    end function double_key_read

!> \brief Return the number of days in the given year.
!!
!! This function simply returns the number of days given the current year.
    function day_of_year(iJulianDay) result(iDOY)

        integer(kind=T_INT), intent(in) :: iJulianDay

        ! [ LOCALS ]
        integer(kind=T_INT) :: iFirstDay, iDOY
        integer(kind=T_INT) :: iYear, iMonth, iDay

        ! first get the value for the current year
        call gregorian_date(iJulianDay, iYear, iMonth, iDay)

        ! now calculate the Julian day for the first of the year
        iFirstDay = julian_day(iYear, 1, 1)

        ! return the current day of the year
        iDOY = iJulianDay - iFirstDay + 1

    end function day_of_year

!--------------------------------------------------------------------------
!!****f* types/julian_day
! NAME
!   julian_day - Convert from a Gregorian calendar date to a Julian day number.
!
! SYNOPSIS
!   Conversion from a Gregorian calendar date to a Julian day number.
!   Valid for any Gregorian calendar date producing a Julian day
!   greater than zero.
!
! INPUTS
!   iYear   4-digit year
!   iMonth  2-digit month (1-12)
!   iDay    2-digit day (1-31)
!
! OUTPUTS
!   iJD     integer number of days that have elapsed since noon
!           Greenwich Mean Time (UT or TT) Monday, January 1, 4713 BC
!
! SOURCE

    function julian_day(iYear, iMonth, iDay, iOrigin) result(iJD)

        ! [ ARGUMENTS ]
        integer(kind=T_INT), intent(in) :: iYear, iMonth, iDay
        integer(kind=T_INT), optional :: iOrigin

        ! [ LOCALS ]
        integer(kind=T_INT) i, j, k
        integer(kind=T_INT) :: iOffset

        ! [ RETURN VALUE ]
        integer(kind=T_INT) :: iJD

        i = iYear
        j = iMonth
        k = iDay

        if (PRESENT(iOrigin)) then
            iOffset = iOrigin
        else
            iOffset = 0
        end if

        iJD = (k - 32075_T_INT + 1461_T_INT * (i + 4800_T_INT + (j - 14_T_INT) / 12_T_INT) &
               / 4_T_INT + 367_T_INT * (j - 2_T_INT - (j - 14_T_INT) / 12_T_INT * 12_T_INT) &
               / 12_T_INT - 3_T_INT * ((i + 4900_T_INT + (j - 14_T_INT) &
                                        / 12_T_INT) / 100_T_INT) / 4_T_INT) - iOffset

        return

    end function julian_day

!--------------------------------------------------------------------------
!!****f* types/solstice
! NAME
!   solstice - Returns 0 normally, or a value >0 during solstice or equinox.
!
! SYNOPSIS
!    Returns the following:
!      0: non-solstice and non-equinox day
!      1: Vernal equinox
!      2: Summer Solstice
!      3: Autumnal equinox
!      4: Winter solstice
!
! INPUTS
!   iJD     Julian day value
!
! OUTPUTS
!   iSol    Code as described above
!
! SOURCE

    function solstice(iJD) result(iSol)

        ! [ ARGUMENTS ]
        integer(kind=T_INT), intent(in) :: iJD

        ! [ LOCALS ]
        integer(kind=T_INT) iMonth, iDay, iYear

        ! [ RETURN VALUE ]
        integer(kind=T_INT) :: iSol

        call gregorian_date(iJD, iYear, iMonth, iDay)

        if (iMonth == 3 .AND. iDay == 20) then
            iSol = 1
        elseif (iMonth == 6 .AND. iDay == 21) then
            iSol = 2
        elseif (iMonth == 9 .AND. iDay == 22) then
            iSol = 3
        elseif (iMonth == 12 .AND. iDay == 21) then
            iSol = 4
        else
            iSol = 0
        end if

        return

    end function solstice

!!***

!--------------------------------------------------------------------------
!!****f* types/num_days_in_year
! NAME
!   num_days_in_year - Return the number of days in the given year.
!
! SYNOPSIS
!   This function simply returns the number of days given the current year.
!
! INPUTS
!   iYear   4-digit year
!
! OUTPUTS
!   iNumDaysInYear - integer number of days that have elapsed between
!                    January 1 and December 31 of the current year.
!
! SOURCE

    function num_days_in_year(iYear) result(iNumDaysInYear)

        integer(kind=T_INT), intent(in) :: iYear

        ! [ LOCALS ]
        integer(kind=T_INT) :: iFirstDay, iLastDay, iNumDaysInYear

        iFirstDay = julian_day(iYear, 1, 1)
        iLastDay = julian_day(iYear, 12, 31)
        iNumDaysInYear = iLastDay - iFirstDay + 1

    end function num_days_in_year

!--------------------------------------------------------------------------

    function num_days_in_month(iMonth, iYear) result(iNumDaysInMonth)

        integer(kind=T_INT), intent(in) :: iMonth
        integer(kind=T_INT), intent(in) :: iYear
        integer(kind=T_INT) :: iNumDaysInMonth

        call assert(iMonth <= UBOUND(MONTH, 1), "Illegal month number supplied" &
                    //TRIM(asChar(iMonth)), TRIM(__FILE__), __LINE__)

        if (iMonth /= 2) then
            iNumDaysInMonth = MONTH(iMonth)%iNumDays
        elseif (iMonth == 2 .AND. leap(iYear)) then
            iNumDaysInMonth = 29
        elseif (iMonth == 2) then
            iNumDaysInMonth = 28
        end if

    end function num_days_in_month
!--------------------------------------------------------------------------

    subroutine make_date_list(iSampleDates, iFromDates, iToDates, sListType)

        integer, dimension(:) :: iSampleDates
        integer, dimension(:), allocatable, intent(out) :: iFromDates
        integer, dimension(:), allocatable, intent(out) :: iToDates
        character(len=*) :: sListType

        ! [ LOCALS ]
        integer :: iIndex
        integer :: iMyMonth, iMyYear
        integer :: iUBound
        integer :: iCount
        integer :: iStartMM, iStartDD, iStartYYYY
        integer :: iEndMM, iEndDD, iEndYYYY
        integer, dimension(SIZE(iSampleDates, 1)) :: iSampleYYYY, iSampleMM, iSampleDD

        integer, dimension(:), allocatable :: iTempFromDates
        integer, dimension(:), allocatable :: iTempToDates

        iUBound = UBOUND(iSampleDates, 1)

        call gregorian_date(iJD=iSampleDates(1), &
                            iMonth=iStartMM, &
                            iDay=iStartDD, &
                            iYear=iStartYYYY)

        call gregorian_date(iJD=iSampleDates(iUBound), &
                            iMonth=iEndMM, &
                            iDay=iEndDD, &
                            iYear=iEndYYYY)

        do iIndex = 1, iUBound
            call gregorian_date(iJD=iSampleDates(iIndex), &
                                iMonth=iSampleMM(iIndex), &
                                iDay=iSampleDD(iIndex), &
                                iYear=iSampleYYYY(iIndex))
        end do

        if (ALLOCATED(iTempFromDates)) deallocate (iTempFromDates)
        if (ALLOCATED(iTempToDates)) deallocate (iTempToDates)

        if (ALLOCATED(iFromDates)) deallocate (iFromDates)
        if (ALLOCATED(iToDates)) deallocate (iToDates)

        if (TRIM(sListType) == "ANNUAL") then

            allocate (iTempFromDates(iUBound))
            allocate (iTempToDates(iUBound))

            iIndex = 0

            do iMyYear = iStartYYYY, iEndYYYY

                iCount = COUNT(iSampleYYYY == iMyYear)

                !> set arbitrary minimum number of days that must be present
                !> in order to include this year as a date range in our list
                if (iCount < 350) cycle

                iIndex = iIndex + 1

                iTempFromDates(iIndex) = MINVAL(iSampleDates, dim=1, &
                                                mask=(iSampleYYYY == iMyYear .AND. iSampleMM == 1))

                iTempToDates(iIndex) = MAXVAL(iSampleDates, 1, &
                                              iSampleYYYY == iMyYear .AND. iSampleMM == 12)

            end do

        elseif (TRIM(sListType) == "MONTHLY") then

            allocate (iTempFromDates(iUBound * 12))
            allocate (iTempToDates(iUBound * 12))

            iIndex = 0

            do iMyYear = iStartYYYY, iEndYYYY
                do iMyMonth = 1, 12

                    iCount = COUNT(iSampleYYYY == iMyYear .AND. iSampleMM == iMyMonth)

                    !> set arbitrary minimum number of days that must be present
                    !> in order to include this month and year as a date range in our list
                    if (iCount < 25) cycle

                    iIndex = iIndex + 1

                    iTempFromDates(iIndex) = MINVAL(iSampleDates, dim=1, &
                                                    mask=(iSampleYYYY == iMyYear .AND. iSampleMM == iMyMonth))

                    iTempToDates(iIndex) = MAXVAL(iSampleDates, dim=1, &
                                                  mask=(iSampleYYYY == iMyYear .AND. iSampleMM == iMyMonth))

                end do
            end do

        else

            call assert(lFALSE, "INTERNAL PROGRAMMING ERROR: unhandled sListType", &
                        TRIM(__FILE__), __LINE__)

        end if

        allocate (iFromDates(iIndex))
        allocate (iToDates(iIndex))

        iFromDates = iTempFromDates(1:iIndex)
        iToDates = iTempToDates(1:iIndex)

    end subroutine make_date_list

!!***

!--------------------------------------------------------------------------
!!****f* types/gregorian_date
! NAME
!   gregorian_date - Convert from a Julian day number to a Gregorian date.
!
! SYNOPSIS
!   Conversion to a Gregorian calendar date from a Julian date.
!   Valid for any Gregorian calendar date producing a Julian day number
!   greater than zero.
!
! INPUTS
!   iJD     integer number of days that have elapsed since noon
!           Greenwich Mean Time (UT or TT) Monday, January 1, 4713 BC
! OUTPUTS
!   iYear   4-digit year
!   iMonth  2-digit month (1-12)
!   iDay    2-digit day (1-31)
!
! NOTES
!   Reference: Fliegel, H. F. and van Flandern, T. C. (1968).
!   Communications of the ACM, Vol. 11, No. 10 (October, 1968).
!   Modified from code found at:
!       http://aa.usno.navy.mil/faq/docs/JD_Formula.html
!
! SOURCE

    subroutine gregorian_date(iJD, iYear, iMonth, iDay, iOrigin)

!! COMPUTES THE GREGORIAN CALENDAR DATE (YEAR,MONTH,DAY)
!! GIVEN THE JULIAN DATE (JD).

        ! [ ARGUMENTS ]
        integer(kind=T_INT) :: iJD
        integer(kind=T_INT), intent(inout) :: iYear, iMonth, iDay
        integer(kind=T_INT), optional :: iOrigin
        ! [ LOCALS ]
        integer(kind=T_INT) iI, iJ, iK, iL, iN
        integer(kind=T_INT) :: iOffset

        if (PRESENT(iOrigin)) then
            iOffset = iOrigin
        else
            iOffset = 0
        end if

        ! allow for an alternate "origin" to be specified... technically,
        ! this is no longer a "Julian" day, but alas... This modification
        ! was required in order to process the "time" variables from global
        ! climate models, which seem to be defined as something like this:
        ! time:units = "days since 1960-01-01 00:00:00"
        !
        ! for the above example, JD = 2436935 on the first day; the NetCDF "time"
        ! variable will be equal to 0.  Thus, in order to get the conversion
        ! right, we must add 0 + 2436935 to yield a true Julian Day.

        iJD = iJD + iOffset

        iL = iJD + 68569_T_INT
        iN = 4 * iL / 146097_T_INT
        iL = iL - (146097_T_INT * iN + 3_T_INT) / 4_T_INT
        iI = 4000_T_INT * (iL + 1_T_INT) / 1461001_T_INT
        iL = iL - 1461_T_INT * iI / 4_T_INT + 31_T_INT
        iJ = 80_T_INT * iL / 2447_T_INT
        iK = iL - 2447_T_INT * iJ / 80_T_INT
        iL = iJ / 11_T_INT
        iJ = iJ + 2_T_INT - 12_T_INT * iL
        iI = 100_T_INT * (iN - 49_T_INT) + iI + iL

        iYear = iI
        iMonth = iJ
        iDay = iK

        return

    end subroutine gregorian_date

!--------------------------------------------------------------------------

    subroutine water_year_and_day(iJD, iWY, iDayOfWY)

        integer, intent(in) :: iJD
        integer, intent(out) :: iWY
        integer, intent(out) :: iDayOfWY ! returns value between 0 and 365

        ! [ LOCALS ]
        integer :: iMM, iDD, iYYYY
        integer :: iOrigin

        call gregorian_date(iJD, iYYYY, iMM, iDD)

        if (iMM >= 10) then
            iWY = iYYYY + 1
            iOrigin = julian_day(iYYYY, 10, 1)
        else
            iWY = iYYYY
            iOrigin = julian_day(iYYYY - 1, 10, 1)
        end if

        if (iMM == 2 .AND. iDD == 29) then
            iDayOfWY = 151
        else
            iDayOfWY = julian_day(iYYYY, iMM, iDD, iOrigin)
            if (iDayOfWY >= 151 .AND. (.NOT. leap(iYYYY))) iDayOfWY = iDayOfWY + 1
        end if

    end subroutine water_year_and_day

!--------------------------------------------------------------------------
!!****s* types/Assert
! NAME
!   Assert - General-purpose error-checking routine.
!
! SYNOPSIS
!   General-purpose error-checking routine. If lCondition is .false.,
!   prints the error message and stops!
!
! INPUTS
!   lCondition - statement that evaluates to a logical .true. or .false value.
!   sErrorMessage - accompanying error message to print if lCondition is .false.
!   sFilename - name of the offending file; populate with the C compiler
!                 preprocessor macro __FILE__
!   sLineNo - line number of error; populate with preprocessor macro __LINE__
!
! OUTPUTS
!   NONE
!
! SOURCE

    subroutine Assert(lCondition, sErrorMessage, sFilename, iLineNo)

        ! ARGUMENTS
        logical(kind=T_LOGICAL), intent(in) :: lCondition
        character(len=*), intent(in) :: sErrorMessage
        character(len=*), optional :: sFilename
        integer(kind=T_INT), optional :: iLineNo
        logical :: lFileOpen

        if (.NOT. lCondition) then
            print *, 'FATAL ERROR - HALTING TSPROC'
            print *, TRIM(sErrorMessage)
            print *, " "
            if (PRESENT(sFilename)) print *, "module: ", TRIM(sFilename)
            if (PRESENT(iLineNo)) print *, "line no.: ", iLineNo

            ! echo error condition to the log file ONLY if it is open!
            inquire (unit=LU_REC, opened=lFileOpen)
            if (lFileOpen) then

                write (UNIT=LU_REC, FMT=*) 'FATAL ERROR - HALTING TSPROC'
                write (UNIT=LU_REC, FMT=*) TRIM(sErrorMessage)
                write (UNIT=LU_REC, FMT=*) " "
                if (PRESENT(sFilename)) write (UNIT=LU_REC, FMT=*) "module: ", &
                    TRIM(sFilename)
                if (PRESENT(iLineNo)) write (UNIT=LU_REC, FMT=*) "line no.: ", iLineNo

            end if
            stop
        end if

        return
    end subroutine Assert

    subroutine Chomp_delim_sub(sRecord, sItem, sDelimiters)

        ! ARGUMENTS
        character(len=*), intent(inout) :: sRecord
        character(len=256), intent(out) :: sItem
        character(len=*), intent(in) :: sDelimiters
        ! LOCALS
        integer(kind=T_INT) :: iR ! Index in sRecord
        integer(kind=T_INT) :: iB !
        integer(kind=T_INT) :: iLen

        iB = 0

#ifdef DEBUG_PRINT
        write (*, fmt="(/,a)") TRIM(__FILE__)//":"//TRIM(int2char(__LINE__))
        write (*, fmt="(a)") "Incoming sRecord: "//dquote(sRecord)
#endif

        ! eliminate any leading spaces
        sRecord = ADJUSTL(sRecord)
        ! find the end position of 'sRecord'
        iLen = LEN_TRIM(sRecord)

        ! find the POSITION of the first delimiter found
        iR = SCAN(TRIM(sRecord), sDelimiters)

        if (iR == 0) then
            sItem = TRIM(sRecord) ! no delimiters found; return entirety of sRecord
            sRecord = "" ! as sItem
        else
            sItem = TRIM(sRecord(1:iR - 1))
            sRecord = TRIM(ADJUSTL(sRecord(iR + 1:)))
        end if

#ifdef DEBUG_PRINT
        write (*, fmt="(a)") "Exit sRecord: "//dquote(sRecord)
        write (*, fmt="(a)") "Exit sItem: "//dquote(sItem)
        write (*, fmt="(a,i3)") " iR: ", iR
        write (*, fmt="(a,i3)") " iB: ", iB
#endif

    end subroutine Chomp_delim_sub

!------------------------------------------------------------------------------

    subroutine Chomp_default_sub(sRecord, sItem)

        ! ARGUMENTS
        character(len=*), intent(inout) :: sRecord
        character(len=256), intent(out) :: sItem

        ! LOCALS
        integer(kind=T_INT) :: iR ! Index in sRecord

#ifdef DEBUG_PRINT
        write (*, fmt="(/,a)") TRIM(__FILE__)//":"//TRIM(int2char(__LINE__))
        write (*, fmt="(a)") "Incoming sRecord: "//dquote(sRecord)
#endif

        ! eliminate any leading spaces
        sRecord = ADJUSTL(sRecord)
        ! find the end position of 'sRecord'
! iLen = len_trim(sRecord)

        ! find the POSITION of the first delimiter found
        iR = SCAN(TRIM(sRecord), sWHITESPACE)

        if (iR == 0) then
            sItem = TRIM(sRecord) ! no delimiters found; return entirety of sRecord
            sRecord = "" ! as sItem
        else
            sItem = TRIM(ADJUSTL(sRecord(1:iR - 1)))
            sRecord = TRIM(ADJUSTL(sRecord(iR + 1:)))
        end if

#ifdef DEBUG_PRINT
        write (*, fmt="(a)") "Exit sRecord: "//dquote(sRecord)
        write (*, fmt="(a)") "Exit sItem: "//dquote(sItem)
        write (*, fmt="(a,i3)") " iR: ", iR
#endif

    end subroutine Chomp_default_sub

!--------------------------------------------------------------------------

    function count_fields(sRecord) result(iNumFields)

        character(len=*), intent(inout) :: sRecord

        character(len=256) :: sItem
        integer(kind=T_INT) :: iNumFields, i

        i = 1

        do
            call chomp(sRecord, sItem, sWHITESPACE)
            if (LEN_TRIM(sRecord) == 0) exit
            i = i + 1
        end do

        iNumFields = i

    end function count_fields

!--------------------------------------------------------------------------

    function day_of_week(yr, mn, dy)
        ! Algorithm from wikipedia
        ! http://en.wikipedia.org/wiki/Determination_of_the_day_of_the_week
        character(LEN=3) day_of_week
        character(LEN=3), parameter :: day_names(0:6) = &
                                       (/'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'/)
        integer yr, mn, dy
        integer c, y, m, d, w
        y = yr
        m = mn
        d = dy
        if (mn < 3) then
            y = y - 1
        end if

        c = INT(y / 100)
        y = y - c * 100

        m = MOD((m + 9), 12) + 1

        w = INT(MODULO((d + (2.6 * m - 0.2) + y + y / 4 + c / 4 - 2 * c), 7.0))
        day_of_week = day_names(w)

    end function day_of_week

!--------------------------------------------------------------------------

    function ctime_subst()
        character(24) :: ctime_subst

        character(8) :: date
        character(10) :: time
        character(5) :: zone
        integer, dimension(8) :: values

        character(LEN=3), parameter :: months(1:12) = &
                                       (/'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', &
                                         'Sep', 'Oct', 'Nov', 'Dec'/)

        call DATE_AND_TIME(date, time, zone, values)

        ctime_subst = day_of_week(values(1), values(2), values(3))//' '// &
                      months(values(2))//' '//date(7:8)//' '//time(1:2)// &
                      ':'//time(3:4)//':'//time(5:6)//' '//date(1:4)

    end function ctime_subst

!--------------------------------------------------------------------------

    subroutine GetSysTimeDate(sDateStr, sDateStrPretty)

        character(len=256), intent(out) :: sDateStr, sDateStrPretty

        character(len=256) :: sRecord
        character(len=256) :: sDay
        character(len=256) :: sMon
        character(len=256) :: sDD
        character(len=8) :: sHH
        character(len=8) :: sMM
        character(len=8) :: sSS
        character(len=256) :: sTime
        character(len=256) :: sYear

        sRecord = ctime_subst()

        call chomp(sRecord, sDay)
        call chomp(sRecord, sMon)
        call chomp(sRecord, sDD)
        call chomp(sRecord, sTime)
        call chomp(sRecord, sYear)

        sHH = sTime(1:2)
        sMM = sTime(4:5)
        sSS = sTime(7:8)

        sDateStr = TRIM(sDD)//"_"//TRIM(sMon)//"_"//TRIM(sYear)//"__"// &
                   TRIM(sHH)//"_"//TRIM(sMM)
        sDateStrPretty = &
            TRIM(sDay)//" "//TRIM(sMon)//" "//TRIM(sDD)//" "//TRIM(sYear)//" " &
            //TRIM(sHH)//":"//TRIM(sMM)

        return

    end subroutine GetSysTimeDate

!------------------------------------------------------------------------------------------------------------------------------------

    recursive subroutine quick_sort(list, order)

! Quick sort routine from:
! Brainerd, W.S., Goldberg, C.H. & Adams, J.C. (1990) "Programmer's Guide to
! Fortran 90", McGraw-Hill  ISBN 0-07-000248-7, pages 149-150.
! Modified by Alan Miller to include an associated integer array which gives
! the positions of the elements in the original order.

        implicit none
        real, dimension(:), intent(IN OUT) :: list
        integer, dimension(:), intent(OUT) :: order

! Local variable
        integer :: i

! "order " is the ORIGINAL order in which the value are supplied
        do i = 1, SIZE(list)
            order(i) = i
        end do

        call quick_sort_1(1, SIZE(list))

    contains

        recursive subroutine quick_sort_1(left_end, right_end)

            integer, intent(IN) :: left_end, right_end

!     Local variables
            integer :: i, j, itemp
            real :: reference, temp
            integer, parameter :: max_simple_sort_size = 6

            if (right_end < left_end + max_simple_sort_size) then
                ! Use interchange sort for small lists
                call interchange_sort(left_end, right_end)

            else
                ! Use partition ("quick") sort
                reference = list((left_end + right_end) / 2)
                i = left_end - 1; j = right_end + 1

                do
                    ! Scan list from left end until element >= reference is found
                    do
                        i = i + 1
                        if (list(i) >= reference) exit
                    end do
                    ! Scan list from right end until element <= reference is found
                    do
                        j = j - 1
                        if (list(j) <= reference) exit
                    end do

                    if (i < j) then
                        ! Swap two out-of-order elements
                        temp = list(i); list(i) = list(j); list(j) = temp
                        itemp = order(i); order(i) = order(j); order(j) = itemp
                    else if (i == j) then
                        i = i + 1
                        exit
                    else
                        exit
                    end if
                end do

                if (left_end < j) call quick_sort_1(left_end, j)
                if (i < right_end) call quick_sort_1(i, right_end)
            end if

        end subroutine quick_sort_1

        subroutine interchange_sort(left_end, right_end)

            integer, intent(IN) :: left_end, right_end

!     Local variables
            integer :: i, j, itemp
            real :: temp

            do i = left_end, right_end - 1
                do j = i + 1, right_end
                    if (list(i) > list(j)) then
                        temp = list(i); list(i) = list(j); list(j) = temp
                        itemp = order(i); order(i) = order(j); order(j) = itemp
                    end if
                end do
            end do

        end subroutine interchange_sort

    end subroutine quick_sort

!-------------------------------------------------------------------------------------------

    function median(rData) result(rMedian)
!  *                                           *
!  *********************************************
!  * Returns the median value of the elements in
!  * the vector rData(:)
!  *********************************************

        real, intent(in) :: rData(:)
        real :: rMedian

        real :: rDatacp(SIZE(rData))
        integer :: Ns, Nsd2
        integer(kind=T_INT), dimension(SIZE(rData)) :: iOriginalOrder

        rDatacp = rData
        Ns = SIZE(rData)
        Nsd2 = INT(Ns / 2)

        if (Ns > 1) then

            call quick_sort(rDatacp, iOriginalOrder)
            if (MOD(Ns, 2) == 1) then
                rMedian = rDatacp(Nsd2 + 1)
            else
                rMedian = (rDatacp(Nsd2) + rDatacp(Nsd2 + 1)) / 2.0_T_SGL
            end if

        else

            rMedian = rDatacp(1)

        end if

        return

    end function median

    function quantile_scalar(rQuantile, rData) result(rValue)

        real, intent(in) :: rQuantile
        real, dimension(:), intent(in) :: rData
        real :: rValue

        ! [ LOCALS ]
        integer(kind=T_INT) :: iNumRecords
        integer(kind=T_INT) :: iInitialIndex
        real(kind=T_DBL) :: rRealIndex
        real(kind=T_DBL) :: rFractionalIndex
        real, dimension(SIZE(rData)) :: rDatacp
        integer(kind=T_INT), dimension(SIZE(rData)) :: iOriginalOrder

        rDatacp = rData

        call quick_sort(rDatacp, iOriginalOrder)

        rValue = rZERO

        iNumRecords = SIZE(rDatacp)

        if (iNumRecords > 0) then
            rRealIndex = rQuantile * real(iNumRecords, kind=T_DBL)
            iInitialIndex = MAX(INT(rRealIndex, kind=T_INT), 1)
            rFractionalIndex = rRealIndex - real(iInitialIndex, kind=T_DBL)
            if (iInitialIndex < iNumRecords) then
                rValue = rDatacp(iInitialIndex) &
                         + (rDatacp(iInitialIndex + 1) - rDatacp(iInitialIndex)) &
                         * rFractionalIndex
            else
                rValue = rDatacp(iNumRecords)
            end if
        end if

        return

    end function quantile_scalar

    function quantile_vector(rQuantile, rData) result(rValue)

        real, dimension(:), intent(in) :: rQuantile
        real, dimension(:), intent(in) :: rData
        real, dimension(SIZE(rQuantile, 1)) :: rValue

        ! [ LOCALS ]
        integer(kind=T_INT) :: iNumRecords
        integer(kind=T_INT) :: iInitialIndex
        real(kind=T_DBL) :: rRealIndex
        real(kind=T_DBL) :: rFractionalIndex
        real, dimension(SIZE(rData)) :: rDatacp
        integer(kind=T_INT), dimension(SIZE(rData)) :: iOriginalOrder
        integer(kind=T_INT) :: iIndex

        rDatacp = rData

        call quick_sort(rDatacp, iOriginalOrder)

        rValue = rZERO

        iNumRecords = SIZE(rDatacp)

        if (iNumRecords > 0) then
            do iIndex = 1, SIZE(rQuantile, 1)
                rRealIndex = rQuantile(iIndex) * real(iNumRecords + 1, kind=T_DBL)
                iInitialIndex = MAX(INT(rRealIndex, kind=T_INT), 1)
                rFractionalIndex = rRealIndex - real(iInitialIndex, kind=T_DBL)
                if (iInitialIndex < iNumRecords) then
                    rValue(iIndex) = rDatacp(iInitialIndex) &
                                     + (rDatacp(iInitialIndex + 1) - rDatacp(iInitialIndex)) &
                                     * rFractionalIndex
                else
                    rValue(iIndex) = rDatacp(iNumRecords)
                end if
            end do
        end if

        return

    end function quantile_vector

    function mean(rData) result(rMean)

        real, dimension(:), intent(in) :: rData
        real :: rMean

        rMean = SUM(rData) / real(SIZE(rData), kind=T_SGL)

    end function mean

    function variance(rData) result(rVariance)

        real, dimension(:), intent(in) :: rData
        real(kind=T_DBL) :: rVariance

        ! [ LOCALS ]
        real(kind=T_DBL) :: rSum
        real(kind=T_DBL) :: rMean

        rMean = SUM(rData) / real(SIZE(rData), kind=T_DBL)

        rSum = SUM((rData - rMean)**2)

        rVariance = rSum / real(SIZE(rData) - 1, kind=T_DBL)

    end function variance

    function stddev(rData) result(rStdDev)

        real, dimension(:), intent(in) :: rData
        real(kind=T_DBL) :: rStdDev
        real(kind=T_DBL) :: rVariance

        ! [ LOCALS ]

        rVariance = variance(rData)
        rStdDev = SQRT(rVariance)

    end function stddev

    subroutine uppercase_sub(s)

        ! ARGUMENTS
        character(len=*), intent(inout) :: s
        ! LOCALS
        integer(kind=T_INT) :: i ! do loop index
        ! CONSTANTS
        integer(kind=T_INT) :: LOWER_TO_UPPER = ICHAR("A") - ICHAR("a")

        do i = 1, LEN_TRIM(s)
            if (ICHAR(s(i:i)) >= ICHAR("a") .AND. ICHAR(s) <= ICHAR("z")) then
                s(i:i) = CHAR(ICHAR(s(i:i)) + LOWER_TO_UPPER)
            end if
        end do

        return
    end subroutine uppercase_sub

    function uppercase_fn(s) result(sOut)

        ! ARGUMENTS
        character(len=*), intent(in) :: s
        character(len=LEN(s)) :: sOut
        ! LOCALS
        integer(kind=T_INT) :: i ! do loop index
        ! CONSTANTS
        integer(kind=T_INT) :: LOWER_TO_UPPER = ICHAR("A") - ICHAR("a")

        sOut = s

        do i = 1, LEN_TRIM(sOut)
            if (ICHAR(sOut(i:i)) >= ICHAR("a") .AND. ICHAR(sOut) <= ICHAR("z")) then
                sOut(i:i) = CHAR(ICHAR(sOut(i:i)) + LOWER_TO_UPPER)
            end if
        end do

        return
    end function uppercase_fn

!------------------------------------------------------------------------------

    function quote(sString) result(sQuotedString)

        character(len=*), intent(in) :: sString
        character(len=LEN_TRIM(ADJUSTL(sString)) + 2) :: sQuotedString

        sQuotedString = '"'//TRIM(ADJUSTL(sString))//'"'

    end function quote

!------------------------------------------------------------------------------

end module tsp_utilities
