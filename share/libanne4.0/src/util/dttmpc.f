C
C
C
      SUBROUTINE   WDSYSD
     O                   (IDATE)
C
C     + + + PURPOSE + + +
C     Fetch system date and time for DSN creation/modification
C     attributes.  Assumes that sydatm returns a 2 digit year
C     and that the year is 2000 or later.
C     *** FORTRAN 77 ONLY ***
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IDATE(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IDATE  - integer array containing character representation
C              of date and time
C              (1) - 4-digit year
C              (2) - 2-digit month and 2-digit day
C              (3) - 2-digit hour and 2-digit month
C              (4) - 2-digit second and 2 blanks
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      YR,MO,DY,HR,MN,SC
      CHARACTER*16 DATE
C
C     + + + EXTERNALS + + +
      EXTERNAL     SYDATM
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A4,5A2,2X)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (I4,5I2,2X)
C
C     + + + END SPECIFICATIONS + + +
C
c     Modified 9/20/2010 by SMW - modified code does indeed return a 4-digit year;
c     therefore 2000 should *not* be added to the year term!
      CALL SYDATM (YR,MO,DY,HR,MN,SC)
c      YR = YR + 2000
      WRITE (DATE,2000) YR,MO,DY,HR,MN,SC
      READ (DATE,1000) IDATE
C
      RETURN
      END
C
C
C
      SUBROUTINE   SYDATM
     O                   ( YR, MO, DY, HR, MN, SC )
C
C     + + + PURPOSE + + +
C     Returns the current date and time.  Calls the system dependent
C     subroutines SYDATE for the date and SYTIME for the time.
Cy2k  Note:  Returns a 2-digit year for backwords compatability
Cy2k  with older code.  Assumes that the DATE will continue to
Cy2k  return a 2-digit year in positions 7 and 8.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   YR, MO, DY, HR, MN, SC
C
C     + + + ARGUMENT DEFINITIONS
C     YR     - year
C     MO     - month
C     DA     - day
C     HR     - hour
C     MN     - minute
C     SC     - second
C
      character*8 sDATE
      character*10 sTIME
      character*5 sZONE
      integer, dimension(8) ::  iVALUE

      CALL DATE_AND_TIME(sDATE, sTIME, sZONE, iVALUE)

      YR = iVALUE(1)
      MO = iVALUE(2)
      DA = iVALUE(3)

      HR = iVALUE(5)
      MN = iVALUE(6)
      SC = iVALUE(7)

C
      RETURN
      END
C
C
C
      SUBROUTINE   SYDATE
     O                   ( YR, MO, DA )
C
C     + + + PURPOSE + + +
C     This subroutine is used to retrieve the system date.
C     This version of SYDATE calls the Lahey Fortran system
C     routine DATE.
Cy2k  Note:  Returns a 2-digit year for backwords compatability
Cy2k  with older code.  Assumes that the DATE will continue to
Cy2k  return a 2-digit year in positions 7 and 8.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   YR, MO, DA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YR     - year
C     MO     - month
C     DA     - day
C
C      + + + LOCAL VARIABLES + + +
       CHARACTER*8 CDATE
C
      integer, dimension(8) ::  iVALUE
      character*8 sDATE
      character*10 sTIME
      character*5 sZONE

      CALL DATE_AND_TIME(sDATE, sTIME, sZONE, iVALUE)

      YR = iVALUE(1)
      MO = iVALUE(2)
      DA = iVALUE(3)

      RETURN
      END
C
C
C
      SUBROUTINE   SYTIME
     O                  ( HR, MN, SC )
C
C     + + + PURPOSE + + +
C     This subroutine is used to retrieve the system time.
C     This version of SYTIME calls the Lahey Fortran system
C     routine TIME.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   HR, MN, SC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     HR     - Number of hours since midnight
C     MN     - Number of minutes since hour
C     SC     - Number of seconds since minute
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*11 CTIME
      integer, dimension(8) ::  iVALUE
      character*8 sDATE
      character*10 sTIME
      character*5 sZONE

      CALL DATE_AND_TIME(sDATE, sTIME, sZONE, iVALUE)

      HR = iVALUE(5)
      MN = iVALUE(6)
      SC = iVALUE(7)

      RETURN
      END
