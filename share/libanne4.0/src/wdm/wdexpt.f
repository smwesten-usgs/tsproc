      subroutine sub_error(subname)

! -- Subroutine sub_error names the subroutine causing a run-time error.

! -- Arguments are as follows:-
!       subname:  name of offending subroutine

! -- Revision history:-
!       June-November, 1995: version 1.

    	character (len=*)               ::subname

	    write(6,10) trim(subname)
10      format(/,' *** PROGRAMMING ERROR CALLING SUBROUTINE ',a,' ***')
    	stop

      end subroutine sub_error

c-----------------------------------------------------------------------------

      subroutine newtime(nsecs,sec,min,hour)

! -- Subroutine SECTIME expresses elapsed time since midnight as hours,
!    minutes and seconds.
!    NSECS must be positive.
!    If the resulting hours are greater than 24, no correction is made.

! -- Arguments are as follows:-
!	nsecs:                 elapsed number of seconds
!       sec,min,hour:          seconds, minutes, hours of time of day

! -- Revision history:-
!	April, 1997: version 1.

        implicit none

        integer, intent(in)   :: nsecs
        integer, intent(out)  :: sec,min,hour

        integer               :: tsecs

        if(nsecs.lt.0) call sub_error('NEWTIME')

        hour=nsecs/3600
        tsecs=nsecs-hour*3600
        min=tsecs/60
        sec=tsecs-min*60

        return

      end subroutine newtime

c-----------------------------------------------------------------------------

      subroutine newdate(ndays,day1,mon1,year1,day2,mon2,year2)

! -- Subroutine NEWDATE evaluates the date after NDAYS days have elapsed from
!    a provided date. NDAYS may be negative.

! -- Arguments are as follows:-
!       ndays:            elapsed number of days
!       day1,mon1,year1:  days, month and year of first date
!       day2,mon2,year2:  days, month and year of second date

! -- Revision history:-
!       June-November, 1995: version 1.

    	implicit none

    	integer, intent(in)     :: ndays,day1,mon1,year1
	    integer, intent(out)    :: day2,mon2,year2

    	integer  :: yearref,newdays,idays,iyear,jdays,i
	    integer, dimension(12) :: monthdays

        integer numdays
        logical leap

    	data monthdays /31,28,31,30,31,30,31,31,30,31,30,31/

! -- First a reference date is chosen. This is the beginning of the first
! -- year. Alternatively the reference date is the beginning of a year prior
! -- to the likely calculated date if NDAYS is negative.

	    if(ndays.ge.0) then
    	  yearref=year1
	    else
    	  yearref=year1-abs(ndays)/365-1
	    end if
    	newdays=numdays(31,12,yearref-1,day1,mon1,year1)
	    newdays=ndays+newdays
    	if(newdays.lt.0) call sub_error('NEWDATE')

! -- Next days are counted, starting at the new reference date.

	    idays=0
    	iyear=yearref
    	do
    	  jdays=idays+365
    	  if(leap(iyear)) jdays=jdays+1
	      if(jdays.ge.newdays) go to 20
    	  iyear=iyear+1
	      idays=jdays
    	end do
    	call sub_error('NEWDATE')
20      year2=iyear

     	do i=1,12
            jdays=idays+monthdays(i)
            if((i.eq.2).and.(leap(year2))) jdays=jdays+1
            if(jdays.ge.newdays) go to 40
	      idays=jdays
     	end do
        call sub_error('NEWDATE')
40      mon2=i
        day2=newdays-idays
	    if((day2.le.0).or.(mon2.le.0)) call sub_error('NEWDATE')
        return

      end subroutine newdate

      logical function leap(year)

! -- Function LEAP returns .true. if a year is a leap year.

! -- Revision history:-
!       June-November, 1995: version 1.

       integer, intent(in)     :: year

         leap = ( mod(year,4).eq.0 .and. mod(year,100).ne.0 ) .or.
     1            ( mod(year,400).eq.0 .and. year.ne.0 )

       return
      end function leap

c-----------------------------------------------------------------------------

      integer function numsecs(h1,m1,s1,h2,m2,s2)

! -- Subroutine NUMSECS calculates the number of seconds between two times.

! -- Arguments are as follows:-
!       h1,m1,s1:   hours, minutes seconds of first time
!       h2,m2,y2:   hours, minutes seconds of second time

! -- Revision history:-
!       June-November 1995: version 1.

        integer, intent(in)             :: h1,m1,s1,h2,m2,s2

        numsecs=(h2-h1)*3600+(m2-m1)*60+s2-s1

      end function numsecs

c-------------------------------------------------------------------------

      subroutine update_time(curtun,curtst,days,secs,dd,mm,yy,hh,nn,ss)

! -- Subroutine UPDATE_TIME is called by a modified version of one of the WDM
!    utilities. Its role is to update the elapased time on the basis of time units
!    and time step length. Note that it only actually uses and updates dates (ie.
!    the last six of the above subroutine arguments), when the time step is in
!    months.

       implicit none

       integer, intent(in)              :: curtun,curtst
       integer, intent(inout)           :: days,secs,dd,mm,yy,hh,nn,ss
       integer tdd

       ! curtun :: current time units
       ! curtst :: current time steps, in units of curtun

       ! [ LOCALS ]
       integer, parameter :: SECOND = 1
       integer, parameter :: MINUTE = 2
       integer, parameter :: HOUR = 3
       integer, parameter :: DAY = 4
       integer, parameter :: MONTH = 5
       integer, parameter :: YEAR = 6
       logical leap
       integer numdays, numsecs
       integer dd_, mm_, yy_

       select case(curtun)

       case(SECOND)
         secs=secs+1*curtst

       case(MINUTE)
         secs=secs+60*curtst

       case(HOUR)
         secs=secs+3600*curtst

       case(DAY)
         days=days+1*curtst

       case(MONTH)
         mm_ = mm + 1*curtst
!         mm=mm+1*curtst
10       if(mm_.gt.12)then
           yy_=yy+1
           mm_=mm_-12
           go to 10
         end if
         tdd=dd
         if(dd.eq.31)then
           if((mm_.eq.9).or.(mm_.eq.4).or.(mm_.eq.6).or.(mm_.eq.11))then
             tdd=30
           end if
         end if
         if((mm_.eq.2).and.(tdd.gt.28))then
           if(leap(yy_) .eqv. .true.)then
             tdd=29
           else
             tdd=28
           end if
         end if
         days=numdays(1,1,1970,tdd,mm_,yy_)
         secs=numsecs(0,0,0,hh,mm,ss)

       case(YEAR)
         yy_=yy+1
         tdd=dd
         if((mm.eq.2).and.(dd.gt.28))then
           if(leap(yy_))then
             tdd=29
           else
             tdd=28
           end if
         end if
         days=numdays(1,1,1970,tdd,mm,yy_)
         secs=numsecs(0,0,0,hh,mm,ss)

       case default

         call sub_error('UPDATE_TIME')

       end select

20     if(secs.ge.86400)then
         secs=secs-86400
         days=days+1
         go to 20
       end if

       call newtime(secs,ss, mm, hh)
       call newdate(days,1,1,1970,dd,mm,yy)

       return

      end subroutine update_time

c-------------------------------------------------------------------------

      integer function numdays(DR,MR,YR,D,M,Y)

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


      integer, intent(in)     :: dr,mr,yr,d,m,y

      INTEGER FLAG,I,J,DA(12),YE,ME,DE,YL,ML,DL
      logical leap

      DATA DA /31,28,31,30,31,30,31,31,30,31,30,31/

! --    THE SMALLER OF THE TWO DATES IS NOW CHOSEN TO DO THE COUNTING FROM.

      IF(Y.LT.YR)GO TO 10
      IF((Y.EQ.YR).AND.(M.LT.MR)) GO TO 10
      IF((Y.EQ.YR).AND.(M.EQ.MR).AND.(D.LT.DR)) GO TO 10
      FLAG=0
      YE=YR
      ME=MR
      DE=DR
      YL=Y
      ML=M
      DL=D
      GO TO 20
10      FLAG=1
      YE=Y
      ME=M
      DE=D
      YL=YR
      ML=MR
      DL=DR

! --    IN THE ABOVE THE POSTSCRIPT "E" STANDS FOR EARLIER DATE, WHILE
!       "L" STANDS FOR THE LATER DATE.

20      numdays=0
      IF((ME.EQ.ML).AND.(YL.EQ.YE))THEN
      numdays=DL-DE
      IF(FLAG.EQ.1) numdays=-numdays
      RETURN
      END IF

      DO 30 J=ME,12
      IF((ML.EQ.J).AND.(YE.EQ.YL))GOTO 40
      numdays=numdays+DA(J)
      IF((J.EQ.2).AND.(leap(ye)))numdays=numdays+1
30      CONTINUE
      GO TO 50
40      numdays=numdays+DL-DE
      IF(FLAG.EQ.1)numdays=-numdays
      RETURN

50      DO 60 I=YE+1,YL
      DO 70 J=1,12
      IF((YL.EQ.I).AND.(ML.EQ.J))GO TO 80
      numdays=numdays+DA(J)
      IF((J.EQ.2).AND.(leap(i))) numdays=numdays+1
70      CONTINUE
60      CONTINUE
      call sub_error('NUMDAYS')
      RETURN

80      numdays=numdays+DL-DE
      IF(FLAG.EQ.1) numdays=-numdays

      RETURN
      end function numdays

c********************************************************************

      SUBROUTINE   PRWMTE
     I                    (WDMSFL,DSN,LSDAT,LEDAT,
     +                     iCount, iMM, iDD, iYY, iHour, iMin,
     +                     iSec, rValue)
C
C     + + + PURPOSE + + +
C     Export timeseries data.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL,DSN,LSDAT(6),LEDAT(6)
      integer :: iCount
      integer, intent(out) :: iMM(iCount), iDD(iCount), iYY(iCount)
      integer, intent(out) :: iHour(iCount), iMin(iCount), iSec(iCount)
      real, intent(out) :: rValue(iCount)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     SUCIFL - Fortran unit number of sequential file
C     DSN    - dataset containing timeseries data to export
C     LSDAT  - export start date
C     LEDAT  - export end date
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    J,I1,GPFLG,DSFREC,RETCOD,RIND,J1,J2,DONFG,
     1           TUNIT,TSTEP,TDAT(6),BADJFG,ADDAFG,LGRPFG,
     2           TGROUP,LTSPT,GPIND,GPOSEN,GPSDAT(6),GPEDAT(6),
     3           CURREC,CURBKS,CURPOS,CURTST,CURTUN,CURCMP,
     4           CURQUA,CURDAT(6),EGPFG,BLSDAT(6),BLEDAT(6),NUMSKP,
     5           OLDNOV
      INTEGER*4  NVAL,CURNOV,CURCNT,TMPNOV
      REAL       LTSFIL,LTOLR,CURVAL,PREVAL
C
      integer yy_,mm_,dd_,ss_,nn_,hh_,ddd_,mmm_,yyy_,i_,j_   ! jd
      integer days(iCount), secs(iCount)
      integer tdays


C     + + + FUNCTIONS + + +
      INTEGER    WDRCGO, TIMCHK
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCGO, TIMCHK, TIMADD, WTFNDG, WTSKVX
      EXTERNAL   TIMDFX, TIMDIF, WBCWSP, WDATCP, WDSKBK, WTEGRP
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('  DATA       STARTS: ',I4,5(1X,I11),/,
     1        '             ENDS: ',I4,5(1X,I11))
c2010 FORMAT (4X,I4,5I3,3X,3I3,I6,I3,7X,2(1PG12.5))
c2020 FORMAT (3X,6(1PG12.5))
c2010 FORMAT (4X,I4,5I3,3X,3I3,I6,I3,7X,2(1PG12.6))
C2020 FORMAT (3X,6(1PG12.6))
 2010 FORMAT (4X,I4,5I3,3X,3I3,I6,I3,7X, 2G12.6 )
 2020 FORMAT (3X, 6G12.6 )
 2030 FORMAT (2X,'END DATA')
C
C     + + + END SPECIFICATIONS + + +

      iYY = - huge(iYY)
      iMM = - huge(iYY)
      iDD = - huge(iYY)
      iHour = - huge(iYY)
      iMin = - huge(iYY)
      iSec = - huge(iYY)
      rValue = -huge(rValue)
C
      iterm=0                          !jd

      I1    = 1
      GPFLG = 1
      ADDAFG= 0
      BADJFG= 0
      LGRPFG= 0
C
C     output data start/end record with date
c      WRITE (*,2000) LSDAT,LEDAT
C
C     get dummy nval, units and timestep
      CALL TIMDFX (LSDAT,LEDAT,
     O             NVAL,TUNIT,TSTEP)
c      print *, LSDAT,LEDAT, NVAL,TUNIT,TSTEP
C
C     get info about timser dsn
      CALL WTFNDG (WDMSFL,DSN,GPFLG,LSDAT,TSTEP,TUNIT,NVAL,
     O             DSFREC,LTSFIL,TGROUP,LTOLR,LTSPT,
     O             GPIND,GPOSEN,GPSDAT,TDAT,RETCOD)

c      print *, WDMSFL,DSN,GPFLG,LSDAT,TSTEP,TUNIT,NVAL,
c     O             DSFREC,LTSFIL,TGROUP,LTOLR,LTSPT,
c     O             GPIND,GPOSEN,GPSDAT,TDAT,RETCOD

C
      CALL WDATCP (LSDAT,CURDAT)
      CALL WDATCP (LSDAT,TDAT)
      GPIND= GPIND- 1
 10   CONTINUE
C       group loop
        GPIND= GPIND+ 1
c        WRITE(*,*) 'group loop',GPIND
C       find out the end of the group
        CALL WTEGRP (GPSDAT,TGROUP,
     O               GPEDAT)
        IF (GPIND.EQ.GPOSEN) THEN
C         last group, dont dump too far
          CALL WDATCP (LEDAT,GPEDAT)
          LGRPFG= 1
c          WRITE(*,*) 'last group',LEDAT
        END IF
C       skip as required within group, dont need VBTIME because
C       ADDAFG is 0
        CALL WTSKVX (WDMSFL,GPIND,GPSDAT,TDAT,
     I               DSFREC,LTSFIL,TGROUP,BADJFG,ADDAFG,ADDAFG,
     O               CURREC,CURBKS,CURPOS,CURNOV,CURVAL,PREVAL,
     O               CURTST,CURTUN,CURCMP,CURQUA,CURCNT,CURDAT,
     O               RETCOD,BLSDAT,BLEDAT)
        IF (RETCOD.EQ.-10) THEN
C         missing group in span of data, write missing val
          CURQUA= 31
c          WRITE (SUCIFL,2010) GPSDAT,TGROUP,I1,CURQUA,I1,I1,LTSFIL
          CALL WDATCP (GPEDAT,GPSDAT)
          CALL WDATCP (GPEDAT,TDAT)
        ELSE
C         be sure wdmsfl record in memory
          RIND= WDRCGO(WDMSFL,CURREC)
C
C         loop to write out requested values
 20       CONTINUE
C
            EGPFG= TIMCHK(BLEDAT,GPEDAT)
C
            IF (LGRPFG.EQ.1 .AND. EGPFG.LE.0) THEN
C             end of last group, recalc number of values
              OLDNOV = CURNOV
              CALL TIMDIF (CURDAT,GPEDAT,CURTUN,CURTST,J)
              CURNOV= J+ CURCNT- 1

c              IF (OLDNOV .NE. CURNOV) THEN
c                WRITE(*,*) 'adjust curnov',OLDNOV
c                WRITE(*,*) '           to',CURNOV
c              END IF
            END IF
C
            TMPNOV= CURNOV- CURCNT+ 1
C
            IF (TMPNOV.GT.0) THEN
C             values to write, do it

              yy_=curdat(1)                                     !jd
              mm_=curdat(2)                                     !jd
              dd_=curdat(3)                                     !jd
              hh_=curdat(4)                                     !jd
              nn_=curdat(5)                                     !jd
              ss_=curdat(6)                                     !jd
!              print *, "init", yy_, mm_, dd_, hh_, nn_, ss_
              tdays=numdays(1,1,1970,dd_,mm_,yy_)               !jd
850           if(hh_.ge.24)then                                 !jd
                hh_=hh_-24                                      !jd
                tdays=tdays+1                                   !jd
                call newdate(1,dd_,mm_,yy_,ddd_,mmm_,yyy_)      !jd
                dd_=ddd_                                        !jd
                mm_=mmm_                                        !jd
                yy_=yyy_                                        !jd
                go to 850                                       !jd
              end if                                            !jd
              tsecs=numsecs(0,0,0,hh_,nn_,ss_)                  !jd
!              print *, "init_tdays, curbks", tdays, curbks

              j_=curcnt                                         !jd
              do i_=1,tmpnov                                    !jd
                iterm=iterm+1                                   !jd
                rValue(iterm)=wrbuff(curbks+j_,rind)               !jd

                iYY(iterm) = yy_
                iMM(iterm) = mm_
                iDD(iterm) = dd_
                iHour(iterm) = hh_
                iMin(iterm) = nn_
                iSec(iterm) = ss_

                days(iterm)=tdays                               !jd
                secs(iterm)=tsecs                               !jd
!                print *
!                print *, j_, i_, curtun,curtst,tdays,tsecs,     !jd
!      +          dd_,mm_,yy_,hh_,nn_,ss_
!                 print *
                call update_time(curtun,curtst,tdays,tsecs,     !jd
     +          dd_,mm_,yy_,hh_,nn_,ss_)                        !jd
!                 print *, j_, i_, curtun,curtst,tdays,tsecs,     !jd
!     +          dd_,mm_,yy_,hh_,nn_,ss_

                j_=j_+1                                         !jd

              end do                                            !jd

            ENDIF

            CALL WDATCP (BLEDAT,CURDAT)
            IF (EGPFG.GT.0) THEN
C             get next block
              NUMSKP= 2
              IF (CURCMP.EQ.0) NUMSKP= CURNOV+ 1
              CALL WDSKBK (WDMSFL,NUMSKP,
     O                     CURREC,CURBKS)
              CURPOS= CURBKS+ 1
C             be sure record is in buffer
              RIND= WDRCGO (WDMSFL,CURREC)
C             split up bcw
              CALL WBCWSP (WIBUFF(CURBKS,RIND),
     O                     CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
C             calc new end of block
              CALL TIMADD (CURDAT,CURTUN,CURTST,CURNOV,
     O                     BLEDAT)
              CURCNT= 1
            ELSE
C             we are at end of group
              CALL WDATCP (GPEDAT,GPSDAT)
              CALL WDATCP (GPEDAT,TDAT)
            END IF
          IF (EGPFG.GT.0) GO TO 20
        END IF
      IF (GPIND.LT.GPOSEN) GO TO 10
C
C     output end record
c      WRITE (*,2030)                         !jd
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWMXE
     I                   (MESSFL,WDMSFL,SUCIFL,DSN)
C
C     + + + PURPOSE + + +
C     Export table datasets.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,WDMSFL,SUCIFL,DSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     SUCIFL - Fortran unit number of sequential file
C     DSN    - dataset containing timeseries data to export
C
C     + + + PARAMETERS + + +
      INTEGER      MXTROW,   MXTLEN
      PARAMETER   (MXTROW=300,MXTLEN=MXTROW*20)
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctblab.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,I1,I3,I80,ITBL,TABIND,NROW,NCOL,NEXT,RETCOD,
     1             IEXIST,DATFLG,MXPOS,BFLDS,LEN,BCWORD,
     2             TCLU,TGRP,TFLDS,TLEN(30),TCOL(30),TSPA,TNUM(4),
     3             ACLU,AGRP,AFLDS,ALEN(30),ACOL(30),ASPA,ANUM(4),
     4             BSPA,IPOS,IFLD,FFLD,FSPA,XNUM(4),ID,DREC,DPOS,
     5             MLEN,GLEN,OLEN,FLEN,CONT,LMESFL,LCLU,LGRP
      REAL         RBUFF(MXTLEN)
      CHARACTER*1  TTYP(30),ATYP(30),MFID(2),TBUFF(80,MXTROW),BLNK(1),
     1             CSTAR(3),BK3(3)
      CHARACTER*16 MTBNAM,CTBNAM
C
C     + + + FUNCTIONS + + +
      INTEGER      LENSTR, STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL     LENSTR, STRFND, WDTBSU, WTBDSP, WTBISP, WMSQCK
      EXTERNAL     WDTBSP, WTBGET, WTBDCD, WTBCLN, WTBSPA, WMSIDP
      EXTERNAL     WDPRPS, WDNXDV, WMSGTE, WMSBCS, CHRCHR, CHRINS
      EXTERNAL     CARVAR, ZIPC, WDTBCG
C
C     + + + DATA INITIALIZATIONS + + +
      DATA BK3,CSTAR/' ',' ',' ','*','*','*'/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('  DATA  name: ',16A1,'  ind ',I4,'  mid  ',2A1,
     1        '  clu ',I4,'  grp ',I4,'  nrw ',I4)
 2010 FORMAT ('    EXTENSION DATA')
 2015 FORMAT ('    END EXTENSION DATA')
 2020 FORMAT ('    MAIN DATA')
 2025 FORMAT ('    END MAIN DATA')
 2030 FORMAT (80A1)
 2040 FORMAT ('  END DATA')
 2050 FORMAT ('Table template missing (CLU=',I5,' GRP=',I5,
     1        ' for table index',I5)
 2060 FORMAT ('Label name ',A16,' template name ',A16,
     1        ' mismatch, index',I5)
 2070 FORMAT ('For table index',I5,' field',I3,
     1        'could not be completely stored (length set to 80.')
 2080 FORMAT ('Problem summarizing data-set number',I5,
     1        ' Return code',I5)
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
      I3 = 3
      I80= 80
      BLNK(1)= ' '
C
C     fill label common block
      I= 1
      CALL WDTBSU (WDMSFL,DSN,TABMX,I,
     O             TABCNT,TABNAM,TABID,TABDIM,PDATVL,
     O             RETCOD)
C
      IF (RETCOD.GE.0) THEN
C       export all tables in this data set
        ITBL= 0
 10     CONTINUE
C         export next table
          ITBL= ITBL+ 1
          IF (ITBL.LE.TABCNT) THEN
C           export next table, determine index, rows, col space, ext space
            CALL WTBDSP (TABDIM(ITBL),
     O                   TABIND,NROW,NCOL,NEXT)
            IF (TABIND.GT.0) THEN
C             table exists, split out template cluster/group
              CALL WTBISP (TABID(ITBL),
     O                     MFID,TCLU,TGRP)
C             get actual location of table data set template
              CALL WDTBCG (MESSFL,WDMSFL,TCLU,TGRP,
     O                     LMESFL,LCLU,LGRP,RETCOD)
              IF (RETCOD.NE.0) THEN
C               table exists, but cant find template
                RETCOD= -29
                IEXIST= 0
                WRITE (99,2050) TCLU,TGRP,TABIND
              ELSE
                IEXIST= 1
              END IF
            ELSE
C             table does not exist
              IEXIST= 0
            END IF
          END IF
          IF (IEXIST.GT.0) THEN
C           table exists to export, save current table name
            I= 16
            CALL CARVAR (I,TABNAM(1,ITBL),I,CTBNAM)
C           get screen template info
            CALL WDTBSP (LMESFL,LCLU,LGRP,
     O                   MTBNAM,TFLDS,TTYP,TLEN,TCOL,TSPA,TNUM,
     O                   ACLU,AGRP,AFLDS,ATYP,ALEN,ACOL,ASPA,ANUM,
     O                   RETCOD)
            IF (CTBNAM.EQ.MTBNAM) THEN
C             names match, export name, index, other info header
              WRITE (SUCIFL,2000) (TABNAM(I,ITBL),I=1,16),TABIND,MFID,
     1                             LCLU,LGRP,NROW
              IF (AGRP.GT.0) THEN
C               associated table data exists
                DATFLG= 2
C               may need to adjust column numbers
                CALL WTBCLN (AFLDS,ALEN,
     M                       ACOL)
C               export associated data
                WRITE (SUCIFL,2010)
C               80 char buffers at a time
                IFLD = 1
                MXPOS= 80
                FFLD = 1
                FSPA = 1
 100            CONTINUE
C                 see if this field will fit in buffer
                  IPOS= ACOL(IFLD)+ ALEN(IFLD)- 1
                  IF (IPOS.GT.MXPOS .OR. IFLD.EQ.AFLDS) THEN
C                   not enough room in buffer or end of fields,
C                   get a block of associated data
                    IF (IPOS.GT.MXPOS .AND. IFLD.GT.FFLD) THEN
C                     dont include this fields space
                      IFLD= IFLD- 1
                    ELSE IF (IPOS.GT.MXPOS) THEN
C                     field will not fit in buffer
                      WRITE (99,2070) CTBNAM,TABIND,IFLD
                      ALEN(IFLD)= 80
                    END IF
                    BFLDS= IFLD- FFLD+ 1
C                   determine space for this buffer of data
                    CALL WTBSPA (BFLDS,ATYP(FFLD),ALEN(FFLD),
     O                           BSPA,XNUM)
C                   get this buffer of data
                    CALL WTBGET (WDMSFL,DSN,CTBNAM,TABIND,DATFLG,
     I                           I1,I1,FSPA,BSPA,
     O                           RBUFF,RETCOD)
C                   clear write buffer
                    CALL ZIPC (I80,BLNK,TBUFF)
C                   decode real buffer into text buffer
                    CALL WTBDCD (BFLDS,I1,BSPA,ALEN(FFLD),ATYP(FFLD),
     I                           ACOL(FFLD),RBUFF,MXTLEN,
     O                           TBUFF,RETCOD)
C                   write out extension data to sequential file
                    LEN= LENSTR(I80,TBUFF)
                    WRITE (SUCIFL,2030) (TBUFF(I,1),I=1,LEN)
                    IF (IPOS.GT.MXPOS) THEN
C                     more table to process, update positions in table
                      FFLD = IFLD+ 1
                      FSPA = FSPA+ BSPA
                      MXPOS= ACOL(FFLD)+ 80- 1
                    END IF
                  END IF
C                 increment field number
                  IFLD= IFLD+ 1
                IF (IFLD.LE.AFLDS) GO TO 100
C               end of extension data
                WRITE (SUCIFL,2015)
              END IF
C             now do main table
              DATFLG= 1
C             may need to adjust column numbers
              CALL WTBCLN (TFLDS,TLEN,
     M                     TCOL)
C             export main data
              WRITE (SUCIFL,2020)
C             for main table, output headers
              ID= 16
              CALL WMSIDP (LMESFL,LCLU,LGRP,ID,
     O                     DREC,DPOS)
C             back up one position
              CALL WDPRPS (LMESFL,
     M                     DREC,DPOS,
     O                     I)
C             now get block control word from start of data
              CALL WDNXDV (LMESFL,
     M                     DREC,DPOS,
     O                     BCWORD)
C             split block control word to determine total length
              CALL WMSBCS (BCWORD,
     O                     I,I,I,FLEN)
              MLEN= 0
              GLEN= 0
 150          CONTINUE
C               get a record of text from WDM file
                CALL WMSGTE (LMESFL,FLEN,I80,
     M                       DREC,DPOS,GLEN,MLEN,
     O                       OLEN,TBUFF,CONT)
C               insert '***' string to indicate comment line
                IPOS= STRFND(I80,TBUFF,I3,BK3)
                IF (IPOS.EQ.0) THEN
C                 blank space not found, create them
                  DO 175 I=1,I3
                    CALL CHRINS (I80,I1,CSTAR(I),TBUFF)
 175              CONTINUE
                ELSE
C                 insert '***' at available blank space
                  CALL CHRCHR (I3,CSTAR,TBUFF(IPOS,1))
                END IF
                IF (OLEN.LE.77) THEN
C                 increase output length to adjust for '***' added
                  OLEN= OLEN+ 3
                ELSE
C                 max of 80 for length
                  OLEN= 80
                END IF
C               output to file
                WRITE (SUCIFL,2030) (TBUFF(I,1),I=1,OLEN)
              IF (CONT.EQ.1) GO TO 150
C             80 char buffers at a time
              IFLD = 1
              MXPOS= 80
              FFLD = 1
              FSPA = 1
 200          CONTINUE
C               see if this field will fit in buffer
                IPOS= TCOL(IFLD)+ TLEN(IFLD)- 1
                IF (IPOS.GT.MXPOS .OR. IFLD.EQ.TFLDS) THEN
C                 not enough room in buffer or end of fields,
C                 get a block of main data
                  IF (IPOS.GT.MXPOS .AND. IFLD.GT.FFLD) THEN
C                   dont include this fields space
                    IFLD= IFLD- 1
                  ELSE IF (IPOS.GT.MXPOS) THEN
C                   field will not fit in buffer
                    WRITE (99,2070) CTBNAM,TABIND,IFLD
                    TLEN(IFLD)= 80
                  END IF
                  BFLDS= IFLD- FFLD+ 1
C                 determine space for this buffer of data
                  CALL WTBSPA (BFLDS,TTYP(FFLD),TLEN(FFLD),
     O                         BSPA,XNUM)
C                 get this buffer of data
                  CALL WTBGET (WDMSFL,DSN,CTBNAM,TABIND,DATFLG,
     I                         I1,NROW,FSPA,BSPA,
     O                         RBUFF,RETCOD)
C                 clear write buffer
                  CALL ZIPC (I80*NROW,BLNK,TBUFF)
C                 decode real buffer into text buffer
                  IF (FFLD.GT.1) THEN
C                   info beyond 1st 80 chars, need to adjust starting columns
                    DO 225 J= BFLDS,1,-1
C                     subtract starting column of 1st field from all fields
                      TCOL(FFLD+J-1)= TCOL(FFLD+J-1)- TCOL(FFLD)+ 1
 225                CONTINUE
                  END IF
                  CALL WTBDCD (BFLDS,NROW,BSPA,TLEN(FFLD),TTYP(FFLD),
     I                         TCOL(FFLD),RBUFF,MXTLEN,
     O                         TBUFF,RETCOD)
C                 write out main data to sequential file
                  DO 250 J= 1,NROW
                    LEN= LENSTR(I80,TBUFF(1,J))
                    WRITE (SUCIFL,2030) (TBUFF(I,J),I=1,LEN)
 250              CONTINUE
                  IF (IPOS.GT.MXPOS) THEN
C                   more table to process, update positions in table
                    FFLD = IFLD+ 1
                    FSPA = FSPA+ BSPA
                    MXPOS= TCOL(FFLD)+ 80- 1
                  END IF
                END IF
C               increment field number
                IFLD= IFLD+ 1
              IF (IFLD.LE.TFLDS) GO TO 200
C             end of main data
              WRITE (SUCIFL,2025)
C             end of all data for table
              WRITE (SUCIFL,2040)
            ELSE
C             names don't match, problem
              RETCOD= -33
              WRITE (99,2060) CTBNAM,MTBNAM,TABIND
            END IF
          END IF
        IF (ITBL.LT.TABCNT) GO TO 10
      ELSE
C       summary problems
        WRITE (SUCIFL,2080) DSN,RETCOD
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWMDE
     I                   (WDMSFL,SUCIFL,DSN)
C
C     + + + PURPOSE + + +
C     export DLG type datasets
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,SUCIFL,DSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     SUCIFL - Fortran unit number for sequential export file
C     DSN    - dataset number on WDM file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I20,J,ITYPE(20),ATT1(20),ATT2(20),ITMP,
     1            ILEN,ID,IPOS,NPTS,LOLEN,BOLEN,LRTCOD,BRTCOD
      REAL        DLGBUF(2400)
      CHARACTER*1 CTYPE(15)
      CHARACTER*4 GRNAME(15)
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDLGET, WDLLSU
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CTYPE/'L','I','N','E',' ','A','R','E','A',' ',
     1           'N','O','D','E',' '/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A4)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('  DATA')
 2005 FORMAT ('GROUP  TYPE ',5A1,'   MAJ ATTR',I5,'   MIN ATTR',I5)
 2010 FORMAT (I3,I5,I6,2F12.2,/,15A4)
 2020 FORMAT (6F12.2)
 2030 FORMAT ('  END DATA')
C
C     + + + END SPECIFICATIONS + + +
C
      I20= 20
C
C     always export all DLG groups
      WRITE (SUCIFL,2000)
 10   CONTINUE
C       get summary of label
        CALL WDLLSU (WDMSFL,DSN,I20,
     O               LOLEN,ITYPE,ATT1,ATT2,LRTCOD)
        IF (LRTCOD.GE.0) THEN
C         get data for LOLEN groups
          ILEN= 2400
          DO 30 I= 1,LOLEN
C           export all groups
C           header for this group
            IPOS= 5* (ITYPE(I)-1)+ 1
            WRITE (SUCIFL,2005) (CTYPE(J),J=IPOS,IPOS+4),
     1                           ATT1(I),ATT2(I)
 20         CONTINUE
C             get data until BRTCOD= 0
              ID= 0
              CALL WDLGET (WDMSFL,DSN,ITYPE(I),ATT1(I),ATT2(I),ILEN,
     M                     ID,
     O                     BOLEN,DLGBUF,BRTCOD)
              IF (ID.EQ.1) THEN
C               output internal number, coordinates, and name
                ITMP= DLGBUF(1)
                IF (BOLEN.GT.3) THEN
C                 read name from buffer
                  DO 22 J= 1,15
                    GRNAME(J)= ' '
 22               CONTINUE
                  DO 25 J= 4,BOLEN
                    WRITE (GRNAME(J-3),1000) DLGBUF(J)
 25               CONTINUE
C                 output internal number, coords, and name
                  WRITE (SUCIFL,2010) ID,BOLEN,ITMP,DLGBUF(2),
     1                         DLGBUF(3),(GRNAME(J),J=1,BOLEN-3)
                ELSE IF (BOLEN.GT.1) THEN
C                 output internal number and coords
                  WRITE (SUCIFL,2010) ID,BOLEN,ITMP,
     1                                DLGBUF(2),DLGBUF(3)
                ELSE
C                 no coordinates or name
                  WRITE (SUCIFL,2010) ID,BOLEN,ITMP
                END IF
              ELSE IF (ID.EQ.2) THEN
C               output number of points and coordinates
                NPTS= BOLEN/2
                WRITE (SUCIFL,2010) ID,BOLEN
                WRITE (SUCIFL,2020) (DLGBUF(J),
     1                               DLGBUF(J+NPTS),J=1,NPTS)
              END IF
            IF (BRTCOD.NE.2) GO TO 20
 30       CONTINUE
        END IF
      IF (LRTCOD.EQ.1) GO TO 10
C     all done, end data
      WRITE (SUCIFL,2030)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWMAE
     I                   (WDMSFL,SUCIFL,DSN)
C
C     + + + PURPOSE + + +
C     export attribute data
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,SUCIFL,DSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     SUCIFL - Fortran unit number for output file
C     DSN    - dataset number on WDM file being exported
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,I0,I10,GRCNT,ATIND,ATCNT,RETCOD,
     1              IPOS,DREC,DPOS,DSTYP,ILEN,ATTYP,
     2              ATLEN,ATUSWD,ATUSE(10),ATUPD,LATIND,DPTR
      CHARACTER*1   CDSTYP(10,10),ATNAM(6)
      CHARACTER*110 OBUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE  (OBUF1,OBUFF)
      CHARACTER*1   OBUF1(110)
C
C     + + + FUNCTIONS + + +
      INTEGER       LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL      LENSTR, CHRCHR, WDSCHK, WADQCK, WADGTL
      EXTERNAL      PRMSAE, ZIPI,   WATTUS, WATWDS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CDSTYP/'T','I','M','E','S','E','R','I','E','S',
     1            'T','A','B','L','E',' ',' ',' ',' ',' ',
     2            'S','C','H','E','M','A','T','I','C',' ',
     3            'P','R','O','J','E','C','T',' ',' ',' ',
     4            'V','E','C','T','O','R',' ',' ',' ',' ',
     5            'R','A','S','T','E','R',' ',' ',' ',' ',
     6            'S','P','A','C','E','-','T','I','M','E',
     7            'A','T','T','R','I','B','U','T','E',' ',
     8            'M','E','S','S','A','G','E',' ',' ',' ',
     9            ' ',' ',' ',' ',' ',' ',' ',' ',' ',' '/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('  DATA   DSN ',I5)
 2010 FORMAT ('  END DATA')
 2020 FORMAT ('#ATTRIBUTE ',6A1,'   INDEX ',I5)
 2030 FORMAT ('$TYPE  INTEGER')
 2031 FORMAT ('$TYPE  REAL')
 2032 FORMAT ('$TYPE  CHARACTER')
 2033 FORMAT ('$TYPE  DOUBLE PRECISION')
 2040 FORMAT ('$LENGTH ',I4)
 2050 FORMAT (110A1)
 2060 FORMAT ('$UPDATE')
 2070 FORMAT ('Unable to export dataset ',I4)
C
C     + + + END SPECIFICATIONS + + +
C
      I0    = 0
      I10   = 10
      LATIND= 0
C
C     check dataset existence, get total number of attribute groups
      DSTYP= 8
      CALL WDSCHK (WDMSFL,DSN,DSTYP,
     O             I,GRCNT,RETCOD)
C
      IF (RETCOD.EQ.0) THEN
C       ok to export, write beginning data header
        WRITE (SUCIFL,2000) DSN
        ATIND= 0
        ATCNT= 0
 5      CONTINUE
C         loop through cluster until exported all questions
          CALL ZIPI (I10,I0,ATUSE)
          LATIND= LATIND+ 1
C         check attribute existence
          CALL WADQCK (WDMSFL,DSN,LATIND,
     O                 ATIND)
          IF (ATIND.GT.0) THEN
C           group exists, export it
            ATCNT= ATCNT+ 1
C           get all of label info
            CALL WADGTL (WDMSFL,DSN,ATIND,
     O                   ATNAM,DPTR,ATTYP,ATLEN,ATUSWD,ATUPD)
            WRITE (SUCIFL,2020) ATNAM,ATIND
            IF (ATTYP.EQ.1) THEN
              WRITE (SUCIFL,2030)
            ELSE IF (ATTYP.EQ.2) THEN
              WRITE (SUCIFL,2031)
            ELSE IF (ATTYP.EQ.3) THEN
              WRITE (SUCIFL,2032)
            ELSE IF (ATTYP.EQ.4) THEN
              WRITE (SUCIFL,2033)
            END IF
            WRITE (SUCIFL,2040) ATLEN
            IF (ATUSWD.GT.0) THEN
C             determine required and optional dataset usage
              CALL WATTUS (ATUSWD,
     O                     ATUSE)
C             first do required datasets
              OBUFF= '$REQUIRED'
              IPOS = 9
              DO 10 I= 1,10
                IF (ATUSE(I).EQ.2) THEN
                  ILEN= LENSTR(I10,CDSTYP(1,I))
                  IPOS= IPOS+ 2
                  CALL CHRCHR (ILEN,CDSTYP(1,I),OBUF1(IPOS))
                  IPOS= IPOS+ ILEN
                  OBUF1(IPOS)= ','
                END IF
 10           CONTINUE
              IF (IPOS.GT.9) THEN
C               this attribute required for some datasets
                OBUF1(IPOS)= ' '
                WRITE (SUCIFL,2050) (OBUF1(I),I=1,IPOS-1)
              END IF
C             now do optional datasets
              OBUFF= '$OPTIONAL'
              IPOS = 9
              DO 20 I= 1,10
                IF (ATUSE(I).EQ.1) THEN
                  ILEN= LENSTR(I10,CDSTYP(1,I))
                  IPOS= IPOS+ 2
                  CALL CHRCHR (ILEN,CDSTYP(1,I),OBUF1(IPOS))
                  IPOS= IPOS+ ILEN
                  OBUF1(IPOS)= ','
                END IF
 20           CONTINUE
              IF (IPOS.GT.9) THEN
C               this attribute optional for some datasets
                OBUF1(IPOS)= ' '
                WRITE (SUCIFL,2050) (OBUF1(I),I=1,IPOS-1)
              END IF
            END IF
            IF (ATUPD.EQ.1) WRITE (SUCIFL,2060)
C           export rest of info from data record
            CALL WATWDS (DPTR,
     O                   DREC,DPOS)
            CALL PRMSAE (WDMSFL,SUCIFL,ATTYP,
     M                   DREC,DPOS)
          END IF
        IF (ATCNT.LT.GRCNT) GO TO 5
C
C       write end data
        WRITE(SUCIFL,2010)
      ELSE
C       problems with this dataset
        WRITE(99,2070) DSN
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRMSAE
     I                   (WDMSFL,SUCIFL,ATTYP,
     M                    DREC,DPOS)
C
C     + + + PURPOSE + + +
C     export attribute type question
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,SUCIFL,ATTYP,DREC,DPOS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     SUCIFL - Fortran unit number for export file
C     ATTYP  - attribute type
C     DREC   - record on WDM file
C     DPOS   - position on record
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,BCWORD,IVAL(2),ID,TLEN,ILEN
      CHARACTER*1  CDESC(5)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (RVAL,IVAL)
      REAL         RVAL(2)
C
C     + + + EXTERNALS + + +
      EXTERNAL     WDPRPS, WDNXDV, WATWDS, WMSGTO, PRMSTA
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CDESC/'$','D','E','S','C'/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('$RANGE ',I10,' : ',I10)
 2001 FORMAT ('$RANGE ',F10.3,' : ',F10.3)
 2010 FORMAT ('$DEFAULT ',I10)
 2011 FORMAT ('$DEFAULT ',F10.3)
 2020 FORMAT ('$HELP')
 2030 FORMAT ('$VALID')
C
C     + + + END SPECIFICATIONS + + +
C
C     back up one data value, then get the next one to get first value
      CALL WDPRPS (WDMSFL,
     M             DREC,DPOS,
     O             I)
      CALL WDNXDV (WDMSFL,
     M             DREC,DPOS,
     O             BCWORD)
      CALL WATWDS (BCWORD,
     O             ID,TLEN)
C
 5    CONTINUE
C       loop to export blocks
        IF (ID.GE.3 .AND. ID.LE.7) THEN
C         valid info id
          ID= ID- 2
C
          GO TO (30,40,50,60,70), ID
C
 30       CONTINUE
C           attribute range
            DO 35 I= 1,2
C             read min and max
              CALL WDNXDV (WDMSFL,
     M                     DREC,DPOS,
     O                     IVAL(I))
  35        CONTINUE
            IF (ATTYP.EQ.1) THEN
C             integer format
              WRITE (SUCIFL,2000) IVAL
            ELSE
C             real format
              WRITE (SUCIFL,2001) RVAL
            END IF
            GO TO 200
C
 40       CONTINUE
C           valid attribute responses
            WRITE (SUCIFL,2030)
            CALL WMSGTO (WDMSFL,SUCIFL,TLEN,
     M                   DREC,DPOS)
            GO TO 200
C
 50       CONTINUE
C           default value for attribute, get data value
            CALL WDNXDV (WDMSFL,
     M                   DREC,DPOS,
     O                   IVAL(1))
            IF (ATTYP.EQ.1) THEN
C             integer format
              WRITE (SUCIFL,2010) IVAL(1)
            ELSE
C             real format
              WRITE (SUCIFL,2011) RVAL(1)
            END IF
            GO TO 200
C
 60       CONTINUE
C           description of attribute
            ILEN= 5
            CALL PRMSTA (WDMSFL,SUCIFL,ILEN,CDESC,TLEN,
     M                   DREC,DPOS)
            GO TO 200
C
 70       CONTINUE
C           help for attribute
            WRITE (SUCIFL,2020)
            CALL WMSGTO (WDMSFL,SUCIFL,TLEN,
     M                   DREC,DPOS)
            GO TO 200
C
 200      CONTINUE
        END IF
C
C       get next block control word
        CALL WDNXDV (WDMSFL,
     M               DREC,DPOS,
     O               BCWORD)
        IF (BCWORD.GT.0) THEN
C         more info to come, split block control word
          CALL WATWDS (BCWORD,
     O                 ID,TLEN)
        ELSE
          ID= 0
        END IF
      IF (ID.GT.0) GO TO 5
C
      RETURN
      END
