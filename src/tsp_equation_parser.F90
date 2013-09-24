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

       integer, intent(out)   :: ifail

       integer icontext,ierr,ieqn,ncl,nterm,i,nser,isnum,j,iser,k,nsterm,iterm,nnterm, &
               nn,ixcon,ddx,mmx,yyx,hhx,nnx,ssx,idx,nex,sex,lnx,lnxx,ixcount
       integer serno(maxeqnser)
       real rtime
       double precision dval,dtempx
       character(1)  aa
       character (len=iTSNAMELENGTH) ::  aname
       character(15) aline
       character(25) aoption,adate_atime
       character(300)eqntext
       character(25) acontext(MAXCONTEXT)

       ifail=0
       CurrentBlock_g='SERIES_EQUATION'

       write(*,10) trim(CurrentBlock_g)
       write(LU_REC,10)trim(CurrentBlock_g)
10     format(/,' Processing ',a,' block....')

       icontext=0
       aname=' '
       ieqn=0
       ixcon=0

! -- The SERIES_EQUATION block is first parsed.

       do
         ILine_g=ILine_g+1
         read(LU_TSPROC_CONTROL,'(a)',err=9000,end=9100) cline
         if(cline.eq.' ') cycle
         if(cline(1:1).eq.'#') cycle
         call linesplit(ierr,2)
         if(ierr.ne.0)then
           call num2char(ILine_g,aline)
           call addquote(sInfile_g,sString_g)
           write(amessage,20) trim(aline),trim(sString_g)
20         format('there should be 2 entries on line ',a,' of file ',a)
           go to 9800
         end if
         aoption=cline(left_word(1):right_word(1))
         call casetrans(aoption,'hi')
         if(aoption.ne.'CONTEXT')then
           call test_context(ierr,icontext,acontext)
           if(ierr.eq.-1)then
             call find_end(ifail)
             if(ifail.eq.1) go to 9800
             return
           else if(ierr.eq.1) then
             go to 9800
           end if
           ixcon=1
         end if
         if(aoption.eq.'NEW_SERIES_NAME')then
           call get_new_series_name(ierr,aname)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'CONTEXT')then
           if(ixcon.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,41) trim(aline),trim(sString_g)
41           format('CONTEXT keyword in incorrect location at line ',a,' of file ',a)
             go to 9800
           end if
           call get_context(ierr,icontext,acontext)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'END')then
           go to 200
         else if(aoption.eq.'EQUATION')then
           ieqn=ieqn+1
           if(ieqn.gt.1)then
             write(amessage,30) trim(CurrentBlock_g)
30           format('only one EQUATION keyword can appear in a ',a,' block.')
             go to 9800
           end if
           aa=cline(left_word(2):left_word(2))
           if((aa.eq.'"').or.(aa.eq.'''')) then
             cline(left_word(2):left_word(2))=' '
             ncl=len_trim(cline)
             aa=cline(ncl:ncl)
             if((aa.ne.'"').and.(aa.ne.''''))go to 9200
             cline(ncl:ncl)=' '
             eqntext=cline(left_word(2)+1:)
             eqntext=adjustl(eqntext)
           else
             eqntext=cline(left_word(2):)
!             ncl=len_trim(eqntext)
!             aa=eqntext(ncl:ncl)
!             if((aa.eq.'''').or.(aa.eq.'"')) go to 9200
           end if
           call casetrans(eqntext,'lo')
           write(*,40) trim(eqntext)
           write(LU_REC,40) trim(eqntext)
40         format(t5,'EQUATION "',a,'"')
         else
           call num2char(ILine_g,aline)
           call addquote(sInfile_g,sString_g)
           write(amessage,90) trim(aoption),trim(CurrentBlock_g),trim(aline),trim(sString_g)
90         format('unexpected keyword - "',a,'" in ',a,' block at line ',a, &
           ' of file ',a)
           go to 9800
         end if
       end do
200    continue

! The data supplied in the block is now checked for absences.

       if(aname.eq.' ')then
         write(amessage,230) trim(CurrentBlock_g)
230      format('no NEW_SERIES_NAME keyword provided in ',a,' block.')
         go to 9800
       end if
       if(icontext.eq.0)then
         write(amessage,220) trim(CurrentBlock_g)
220      format('no CONTEXT keyword(s) provided in ',a,' block.')
         go to 9800
       end if
       if(ieqn.eq.0)then
         write(amessage,240) trim(CurrentBlock_g)
240      format('no EQUATION keyword provided in ',a,' block.')
         go to 9800
       end if

! -- The equation is now analysed for correctness. First it is parsed.
! -- But first the @_days_"dd/mm/yy_hh/nn/ss" function is given special treatment.

       lnxx=len_trim(eqntext)
       idx=1
241    continue
       lnx=index(eqntext(idx:),'@_days_ ')
       if(lnx.ne.0) go to 9500
       lnx=index(eqntext(idx:),'@_days_')
       if(lnx.eq.0) go to 249
       idx=idx+lnx-1
       idx=idx+7
       if(eqntext(idx:idx+9).eq.'start_year') then
         go to 241
       else if((eqntext(idx:idx).eq.'"').or.(eqntext(idx:idx).eq.''''))then
         ixcount=0
243      idx=idx+1
         if(idx.gt.lnxx) go to 9500
         if((eqntext(idx:idx).eq.'"').or.(eqntext(idx:idx).eq.'''')) then
           if(ixcount.ne.2) go to 9500
           go to 241
         end if
         if(eqntext(idx:idx).eq.'/')then
           ixcount=ixcount+1
           if(ixcount.gt.2)go to 9500
           eqntext(idx:idx)='~'
         end if
         go to 243
       else
         go to 9500
       end if

249    continue
       call parse(ierr,maxterm,nterm,noper,eqntext,aterm,   &
       bterm,nfunct,funct,operat,rterm,0)
       if(ierr.ne.0) go to 9800
       call series_sub(ierr,NTERM,0)
       if(ierr.ne.0) go to 9800

! -- The series cited in the equation are now collected.

       nser=0
       do i=1,nterm
         if(aterm(i)(1:3).eq.'$~$')then
           call char2num(ierr,aterm(i)(4:),isnum)
           if(ierr.ne.0)then
             write(amessage,250)
250          format('internal error - contact programmer.')
             go to 9800
           end if
           if(nser.eq.0)then
             nser=nser+1
             serno(nser)=isnum
           else
             do j=1,nser-1
               if(serno(j).eq.isnum) go to 270
             end do
             nser=nser+1
             if(nser.gt.maxeqnser)then
               call num2char(maxeqnser,aline)
               write(amessage,260) trim(aline)
260            format('maximum of ',a,' different series names can be cited in a ', &
               'series equation.')
               go to 9800
             end if
             serno(nser)=isnum
270          continue
           end if
         end if
       end do
       if(nser.eq.0)then
         write(amessage,280)
280      format('at least one series name should be cited in a series equation.')
         go to 9800
       end if

 ! -- Now they are checked for time consitency.

       if(nser.eq.1) go to 350
       do iser =2,nser
         i=serno(iser)
         j=serno(iser-1)
         if(series_g(i)%nterm.ne.series_g(j)%nterm) go to 9300
         do k=1,series_g(i)%nterm
           if(series_g(i)%days(k).ne.(series_g(j)%days(k))) go to 9300
           if(series_g(i)%secs(k).ne.(series_g(j)%secs(k))) go to 9300
         end do
       end do

350    continue

! -- Space is allocated for the new series.

       do iser=1,MAXSERIES
         if(.not.series_g(iser)%active) go to 370
       end do
       write(amessage,360)
360    format('no more time series available for data storage - increase MAXSERIES and ', &
       'recompile program.')
       go to 9800

370    continue
       k=serno(nser)
       nsterm=series_g(k)%nterm
       allocate(series_g(iser)%days(nsterm),series_g(iser)%secs(nsterm),  &
       series_g(iser)%val(nsterm),stat=ierr)
       if(ierr.ne.0)then
         write(amessage,380)
380      format('cannot allocate memory for another time series.')
         go to 9800
       end if
       series_g(iser)%active=.true.
       series_g(iser)%name=aname
       series_g(iser)%nterm=nsterm
       series_g(iser)%type='ts'
       do j=1,nsterm
         series_g(iser)%days(j)=series_g(k)%days(j)
       end do
       do j=1,nsterm
         series_g(iser)%secs(j)=series_g(k)%secs(j)
       end do

! -- Numbers are identified and copied to the rterm array.

       do iterm=1,nterm
         aa=aterm(iterm)(1:1)
         if((aa.ne.'(').and.(aa.ne.')').and.(aa.ne.'+').and.(aa.ne.'-').and.   &
            (aa.ne.'*').and.(aa.ne.'/').and.(aa.ne.'^').and.                   &
            (aterm(iterm)(1:6).ne.'~#str_').and.                               &
            (aterm(iterm)(1:6).ne.'~#fin_').and.                               &
            (aterm(iterm)(1:3).ne.'$~$').and.                                  &
            (aterm(iterm)(1:2).ne.'@_'))then
              call char2num(ierr,aterm(iterm),rterm(iterm))
              if(ierr.ne.0)then
                write(amessage,395) trim(aterm(iterm))
395             format('the term "',a,'" in the series equation cannot be interpreted ', &
                'as a number, function or operator.')
                go to 9800
              end if
              aterm(iterm)='~!~'
         end if
       end do

! -- We now check for intrinsic functions.

       do iterm=1,nterm
         if(aterm(iterm)(1:2).eq.'@_')then
           if(aterm(iterm)(3:).eq.'days_start_year')then
             aterm(iterm)(3:)='1'
           else if(aterm(iterm)(3:).eq.'abs_val')then
             write(amessage,409) trim(aterm(iterm))
409          format('intrinsic function "',a,'" cannot be used in a series equation.')
             go to 9800
           else if(aterm(iterm)(3:7).eq.'days_')then
             lnx=len(aterm(iterm))
             do idx=1,lnx
               if(aterm(iterm)(idx:idx).eq.'~')aterm(iterm)(idx:idx)='/'
             end do
             call getfile(ierr,aterm(iterm),adate_atime,8,lnx)
             if(ierr.ne.0) go to 9400
             idx=index(adate_atime,'_')
             if(idx.eq.0) go to 9400
             call char2date(ierr,adate_atime(1:idx-1),ddx,mmx,yyx)
             if(ierr.ne.0) go to 9400
             call char2time(ierr,adate_atime(idx+1:),hhx,nnx,ssx,ignore_24=1)
             if(ierr.ne.0) go to 9400
!             nex=numdays(1,1,1970,ddx,mmx,yyx)
             nex = julian_day(iMonth=mmx, iDay=ddx, iYear=yyx)
             sex=numsecs(0,0,0,hhx,nnx,ssx)
             dtempx=dble(nex)+dble(sex)/86400.0d0
             aterm(iterm)(3:4)='3_'
             write(aterm(iterm)(5:),'(1pd22.14)') dtempx
           else
             go to 9400
           end if
         endif
       end do

! -- The series equation is now evaluated for each term in the series.

       nnterm=nterm
       do iterm=1,nterm
         cterm(iterm)=aterm(iterm)
       end do
       do iterm=1,nterm
         qterm(iterm)=rterm(iterm)
       end do
       do j=1,nsterm
         nterm=nnterm
         do iterm=1,nterm
           aterm(iterm)=cterm(iterm)
         end do
         do iterm=1,nterm
           rterm(iterm)=qterm(iterm)
         end do

! -- First the series numbers in the equation terms are replaced by series values.

         do iterm =1,nterm
           if(aterm(iterm)(1:3).eq.'$~$') then
             call char2num(ierr,aterm(iterm)(4:),isnum)
             rterm(iterm)=series_g(isnum)%val(j)
             aterm(iterm)='~!~'
!             write(aterm(iterm),'(e25.14e3)')series_g(isnum)%val(j)
           end if
         end do

! -- Any instinsic function evaluations are carried out.

         do iterm =1,nterm
           if(aterm(iterm)(1:3).eq.'@_1') then
!             call newdate(series_g(isnum)%days(j),1,1,1970,dd,mm,yy)
!             call gregorian_date(iJD=series_g(isnum)%days(j), &
!                                 iMonth=mm, &
!                                 iDay=dd, &
!                                 iYear=yy)
             nn=day_of_year(series_g(isnum)%days(j))
             rtime=float(nn)+float(series_g(isnum)%secs(j))/86400.0
             rterm(iterm)=rtime
             aterm(iterm)='~!~'


           else if(aterm(iterm)(1:3).eq.'@_3')then   !
             call char2num(ierr,aterm(iterm)(5:),dtempx)
             rterm(iterm)=dble(series_g(isnum)%days(j))+     &
                          dble(series_g(isnum)%secs(j))/86400.0d0-dtempx
             aterm(iterm)='~!~'
           end if
         end do

         call EVALUATE(ierr,MAXTERM,NTERM,NOPER,NFUNCT,ATERM,BTERM,   &
         OPERAT,FUNCT,IORDER,DVAL,rterm)
         if(ierr.ne.0) go to 9800
         series_g(iser)%val(j)=dval
       end do
       write(*,580) trim(aname)
       write(LU_REC,580) trim(aname)
580    format(t5,'Series "',a,'" successfully calculated using series equation.')

!       close(unit=99)                !debug
       return


9000   call num2char(ILine_g,aline)
       call addquote(sInfile_g,sString_g)
       write(amessage,9010) trim(aline),trim(sString_g)
9010   format('cannot read line ',a,' of TSPROC input file ',a)
       go to 9800
9100   continue
       call addquote(sInfile_g,sString_g)
       write(amessage,9110) trim(sString_g),trim(CurrentBlock_g)
9110   format('unexpected end encountered to TSPROC input file ',a,  &
       ' while reading ',a,' block.')
       go to 9800
9200   call num2char(ILine_g,aline)
       call addquote(sInfile_g,sString_g)
       write(amessage,9210) trim(aline),trim(sString_g)
9210   format('cannot read equation from line ',a,' of TSPROC input file ',a)
       go to 9800
9300   write(amessage,9310) trim(series_g(i)%name), trim(series_g(j)%name)
9310   format('all series cited in the series equation must have an identical time-base ', &
       '(ie. the same number of terms, with all dates and times coincident). Series ', &
       '"',a,'" and "',a,'" have different time bases.')
       go to 9800
9400   write(amessage,9410) trim(aterm(iterm))
9410   format('illegal intrinsic function "',a,'" in series equation.')
       go to 9800
9500   write(amessage,9510)
9510   format('illegal "@_days_" function in series equation.')
       go to 9800

9800   call write_message(leadspace='yes',error='yes')
       call write_message(iunit=LU_REC,leadspace='yes')
       ifail=1

       return

end subroutine equation


      SUBROUTINE PARSE(IFAIL,MAXTERM,NTERM,NOPER,eqntext,ATERM,   &
      BTERM,NFUNCT,FUNCT,OPERAT,rterm,itype)

! -- Subroutine PARSE breaks an expression up into elements.
       implicit none

      INTEGER IFAIL,MAXTERM,NTERM,NFUNCT,NOPER,ierr,itype
      INTEGER ICOUNT,NB,I,JFAIL,ITERM,J,IIFUN,NEG
      double precision rterm(maxterm)
      CHARACTER(30)ATEMP
      CHARACTER(1)AA
      CHARACTER(6)AATERM
      CHARACTER(10)BB
      CHARACTER*(*) ATERM(MAXTERM),BTERM(MAXTERM)
      CHARACTER*(*) eqntext
      CHARACTER*(*) FUNCT(NFUNCT)
      CHARACTER*(*) OPERAT(NOPER)

      IFAIL=0
      NTERM=0

! -- First a check is made to see if brackets are balanced.

      IF(eqntext.EQ.' ')GO TO 9000

      ICOUNT=0
      NB=len_trim(eqntext)
      DO 3 I=1,NB
        IF(eqntext(I:I).EQ.'(')THEN
          ICOUNT=ICOUNT+1
        ELSE IF(eqntext(I:I).EQ.')')THEN
          ICOUNT=ICOUNT-1
        END IF
3     CONTINUE
      IF(ICOUNT.NE.0)THEN
        if(itype.eq.0)then
          WRITE(amessage,4)
4         FORMAT('unbalanced parentheses in series equation.')
        else if(itype.eq.1)then
          write(amessage,2)
2         format('unbalanced parentheses in weights equation.')
        end if
        GO TO 9020
      END IF

      IF(INDEX(eqntext,'=').NE.0)GO TO 9000
5     CONTINUE
      CALL GETNEXT(JFAIL,NOPER,eqntext,ATEMP,OPERAT)
      IF(JFAIL.LT.0)THEN
        GO TO 50
      ELSE
        NTERM=NTERM+1
        IF(NTERM.GT.MAXTERM)GO TO 9100
        ATERM(NTERM)=ATEMP
        GO TO 5
      END IF
50    CONTINUE

! -- Functions are now dealt with.

      IIFUN=0
      IF(NTERM.LE.2) GO TO 400
      DO 200 ITERM=2,NTERM
        IF(ATERM(ITERM)(1:1).EQ.'(')THEN
          AA=ATERM(ITERM-1)(1:1)
          IF((AA.EQ.'+').OR.(AA.EQ.'-').OR.(AA.EQ.'*').OR.        &
             (AA.EQ.'/').OR.(AA.EQ.'^').OR.(AA.EQ.'(')) GO TO 200
          AATERM=ATERM(ITERM-1)(1:6)
          DO 70 J=1,NFUNCT
            IF(AATERM.EQ.FUNCT(J)) GO TO 80
70        CONTINUE
          if(itype.eq.0)then
            WRITE(amessage,75) trim(ATERM(ITERM-1))
75          FORMAT('illegal function name  "',A,'" in series equation.')
          else
            WRITE(amessage,76) trim(ATERM(ITERM-1))
76          FORMAT('illegal function name  "',A,'" in weights equation.')
          end if
          GO TO 9020
80        continue
          call num2char(j,bb)
!          CALL WRTINT(BB,J)
          ATERM(ITERM-1)='~#str_'//trim(BB)
          IIFUN=IIFUN+1
        END IF
200   CONTINUE
      IF(IIFUN.EQ.0) GO TO 400

      DO 300 ITERM=1,NTERM
        IF(ATERM(ITERM)(1:6).EQ.'~#str_') THEN
          ATERM(ITERM+1)(1:1)=CHAR(220)
          ICOUNT=1
          DO 280 J=ITERM+1,NTERM
            IF(ATERM(J)(1:1).EQ.'(')THEN
              ICOUNT=ICOUNT+1
            ELSE IF(ATERM(J)(1:1).EQ.')')THEN
              ICOUNT=ICOUNT-1
              IF(ICOUNT.EQ.0)THEN
                ATERM(J)='~#fin_'
                GO TO 300
              END IF
            END IF
280       CONTINUE
        END IF
300   CONTINUE

      CALL COMPRESS(MAXTERM,NTERM,ATERM,BTERM,rterm)

400   CONTINUE

! -- If the last item is an operator then the expression is invalid.

      AA=ATERM(NTERM)(1:1)
      IF((AA.EQ.'+').OR.(AA.EQ.'-').OR.(AA.EQ.'/').OR.(AA.EQ.'*').OR.   &
      (AA.EQ.'^')) GO TO 9000


! -- The "-" and the "+" signs are expanded as a function if appropriate.

490   CONTINUE
      DO 500 ITERM=1,NTERM
        IF((ATERM(ITERM)(1:1).EQ.'-').OR.            &
           (ATERM(ITERM)(1:1).EQ.'+'))THEN
          IF(ATERM(ITERM)(1:1).EQ.'+')THEN
            NEG=0
          ELSE
            NEG=1
          END IF
          IF(ITERM.EQ.1) THEN
            IF(NTERM.EQ.MAXTERM) GO TO 9100
            CALL EXPNEG(ierr,MAXTERM,NTERM,ITERM,ATERM,NEG)
            IF(ierr.NE.0) GO TO 9000
            GO TO 490
          ELSE IF(ATERM(ITERM-1)(1:6).EQ.'~#str_')THEN
            IF(NTERM.EQ.MAXTERM) GO TO 9100
            CALL EXPNEG(ierr,MAXTERM,NTERM,ITERM,ATERM,NEG)
            IF(ierr.NE.0) GO TO 9000
            GO TO 490
          ELSE
            AA=ATERM(ITERM-1)(1:1)
            IF((AA.EQ.'(').OR.(AA.EQ.'+').OR.(AA.EQ.'-').OR.  &
               (AA.EQ.'*').OR.(AA.EQ.'/').OR.(AA.EQ.'^'))THEN
               IF(NTERM.EQ.MAXTERM) GO TO 9100
               CALL EXPNEG(ierr,MAXTERM,NTERM,ITERM,ATERM,NEG)
               IF(ierr.NE.0) GO TO 9000
               GO TO 490
            END IF
          END IF
        END IF
500   CONTINUE

      RETURN

9000  continue
      if(itype.eq.0)then
        WRITE(amessage,9010)
9010    FORMAT('illegal series equation.')
      else if(itype.eq.1)then
        write(amessage,9011)
9011    format('illegal weights equation.')
      end if
      go to 9020
9100  continue
      if(itype.eq.0)then
        WRITE(amessage,9110)
9110    FORMAT('too many terms in series equation.')
      else if(itype.eq.1)then
        write(amessage,9111)
9111    format('too many terms in weights equation.')
      end if
      GO TO 9020

9020  IFAIL=1

      RETURN
      END SUBROUTINE PARSE


      SUBROUTINE EXPNEG(IFAIL,MAXTERM,NTERM,ITERM,ATERM,NEG)

! -- Subroutine EXPNEG expands a "-" sign into a function.

      INTEGER MAXTERM,NTERM,ITERM,IFAIL,ICOUNT,JTERM,I,NEG
      CHARACTER(*) ATERM(MAXTERM)

      IFAIL=0
      IF(NEG.EQ.1)THEN
        ATERM(ITERM)='~#str_15'
      ELSE
        ATERM(ITERM)='~#str_16'
      END IF
      ICOUNT=0
      DO 100 JTERM=ITERM+1,NTERM
        IF((ATERM(JTERM)(1:1).EQ.'-').OR.             &
           (ATERM(JTERM)(1:1).EQ.'+'))GO TO 100
        IF(ATERM(JTERM)(1:1).EQ.'(')THEN
          ICOUNT=ICOUNT+1
        ELSE IF(ATERM(JTERM)(1:1).EQ.')')THEN
          ICOUNT=ICOUNT-1
        ELSE IF(ATERM(JTERM)(1:6).EQ.'~#str_')THEN
          ICOUNT=ICOUNT+1
        ELSE IF(ATERM(JTERM)(1:6).EQ.'~#fin_')THEN
          ICOUNT=ICOUNT-1
        END IF
        IF(ICOUNT.LT.0)THEN
          IFAIL=1
          RETURN
        END IF
        IF(ICOUNT.EQ.0)THEN
          IF(JTERM.LT.NTERM)THEN
            DO 40 I=NTERM,JTERM+1,-1
              ATERM(I+1)=ATERM(I)
40          CONTINUE
          END IF
          ATERM(JTERM+1)='~#fin_'
          NTERM=NTERM+1
          RETURN
        END IF
100   CONTINUE

      RETURN
      END SUBROUTINE EXPNEG



      SUBROUTINE GETNEXT(IFAIL,NOPER,CLINE,ATERM,OPERAT)

! -- Subroutine GETNEXT splits off the next term of an expression.

      implicit none
      INTEGER IFAIL,I,J,NB,NOPER,L,K,IERR
      double precision DVAL
      character (len=iTSNAMELENGTH) :: AFMT
      CHARACTER*(*) CLINE
      CHARACTER*(*) ATERM
      CHARACTER*(*) OPERAT(NOPER)

      ATERM=' '
      IFAIL=0
      IF(CLINE.EQ.' ')THEN
        IFAIL=-1
        RETURN
      END IF

      DO 10 I=1,NOPER
        IF(CLINE(1:1).EQ.OPERAT(I))THEN
          ATERM(1:1)=OPERAT(I)
          CLINE=CLINE(2:)
          cline=adjustl(cline)
          GO TO 20
        END IF
10    CONTINUE
      GO TO 50

20    IF(ATERM(1:1).EQ.'*')THEN
        IF(CLINE(1:1).EQ.'*')THEN
          ATERM(1:1)='^'
          CLINE=CLINE(2:)
          cline=adjustl(cline)
        END IF
      END IF
      RETURN

50    CONTINUE
      NB=len_trim(CLINE)
      DO 100 I=2,NB
        DO 90 J=1,NOPER
          IF(CLINE(I:I).EQ.OPERAT(J)) THEN
            if(I.LE.2) go to 120
            if(I.EQ.NB) go to 120
            IF((J.NE.4).AND.(J.NE.5))go to 120
            if((CLINE(I-1:I-1).NE.'E').AND.(CLINE(I-1:I-1).NE.'e').AND.   &
               (CLINE(I-1:I-1).NE.'D').AND.(CLINE(I-1:I-1).NE.'d'))go to 120
            DO 190 K=I+1,NB
              DO 180 L=1,NOPER
                if(CLINE(K:K).EQ.OPERAT(L))go to 200
180           CONTINUE
190         CONTINUE
            K=NB+1
200         K=K-1
            afmt='(f    .0)'
            write(afmt(3:6),'(i4)')k
            read(cline(1:k),afmt,iostat=ierr) dval
            if(IERR.NE.0) go to 120
            go to 100
          end IF
90      CONTINUE
100   CONTINUE
      ATERM=CLINE(1:MIN(28,NB))
      CLINE=' '
      RETURN

120   ATERM=CLINE(1:I-1)
      CLINE=CLINE(I:)
      RETURN

      END SUBROUTINE GETNEXT


      SUBROUTINE COMPRESS(MAXTERM,NTERM,ATERM,BTERM,rterm)

! -- Subroutine COMPRESS removes "dead terms" from the expression.

      implicit none
      INTEGER MAXTERM,NTERM,I,JTERM
      double precision rterm(maxterm)
      CHARACTER*(*) ATERM(MAXTERM),BTERM(MAXTERM)

      DO 100 I=1,NTERM
        BTERM(I)=ATERM(I)
100   CONTINUE
      JTERM=0
      DO 200 I=1,NTERM
        IF(BTERM(I)(1:1).NE.CHAR(220))THEN
          JTERM=JTERM+1
          ATERM(JTERM)=BTERM(I)
          rterm(jterm)=rterm(i)
        END IF
200   CONTINUE
      NTERM=JTERM

      RETURN
      END SUBROUTINE COMPRESS


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



      SUBROUTINE EVALUATE(IFAIL,MAXTERM,NTERM,NOPER,NFUNCT,ATERM,BTERM,   &
      OPERAT,FUNCT,IORDER,DVAL,rterm)

      implicit none
      INTEGER NTERM,NOPER,NFUNCT,MAXTERM,ITERM,JERR,MAXORD,ICOUNT,I,      &
      IOPER,IFAIL
      INTEGER IORDER(MAXTERM)
      double precision rterm(maxterm)
      DOUBLE PRECISION DVAL,DTEMP1,DTEMP2
      CHARACTER(1)AA
      CHARACTER(6)AFUNCT
      CHARACTER*(*) ATERM(MAXTERM),BTERM(MAXTERM)
      CHARACTER*(*) OPERAT(NOPER),FUNCT(NFUNCT)

      IFAIL=0

! -- IF THERE IS ONLY ONE TERM LEFT, THE EXPRESSION HAS BEEN EVALUATED.

100   CONTINUE
!      write(99,*) (trim(aterm(iterm)),iterm=1,nterm)     !debug

      IF(NTERM.EQ.1)THEN
        dval=rterm(1)
!        CALL RLREAD(JERR,ATERM(1),DVAL)
!        IF(JERR.NE.0)THEN
!          WRITE(amessage,110)
!110       FORMAT('cannot evaluate series equation using current parameter values.')
!          go to 9999
!        END IF
        RETURN
      END IF

! -- IF THERE ARE ANY NUMBERS SURROUNDED BY BRACKETS, THEN THE BRACKETS ARE
!    REMOVED

      IF(NTERM.GE.3)THEN
        DO 150 ITERM=1,NTERM-2
          IF(ATERM(ITERM)(1:1).EQ.'(') THEN
            IF(ATERM(ITERM+2)(1:1).EQ.')')THEN
              ATERM(ITERM)(1:1)=CHAR(220)
              ATERM(ITERM+2)(1:1)=CHAR(220)
              CALL COMPRESS(MAXTERM,NTERM,ATERM,BTERM,rterm)
              GO TO 100
            END IF
          END IF
150     CONTINUE
      END IF

! -- CAN ANY FUNCTION EVALUATIONS NOW BE DONE?

      IF(NTERM.GE.3)THEN
        DO 300 ITERM=1,NTERM-2
          IF(ATERM(ITERM)(1:6).EQ.'~#str_')THEN
            IF(ATERM(ITERM+2)(1:6).EQ.'~#fin_')THEN
              CALL FUNCEVAL(JERR,ATERM(ITERM),rterm(iterm+1),DVAL)
              IF(JERR.NE.0)THEN
                AFUNCT=FUNCT(JERR)
                WRITE(amessage,170) trim(AFUNCT)
170             FORMAT('cannot evaluate "',A,'" function in ',    &
                'series or weights equation because function argument is out of range ', &
                'for at least one term in the series time_span.')
                go to 9999
              END IF
              ATERM(ITERM)(1:1)=CHAR(220)
              rterm(iterm+1)=dval
              aterm(iterm+1)='~!~'
              ATERM(ITERM+2)(1:1)=CHAR(220)
              CALL COMPRESS(MAXTERM,NTERM,ATERM,BTERM,rterm)
              GO TO 100
            END IF
          END IF
300     CONTINUE
      END IF

! -- The operators are now ranked by their level of nesting.

      MAXORD=0
      DO 320 ITERM=1,NTERM
        IORDER(ITERM)=0
320   CONTINUE
      ICOUNT=1
      DO 350 ITERM=1,NTERM
        AA=ATERM(ITERM)(1:1)
        IF(AA.EQ.'(')THEN
          ICOUNT=ICOUNT+1
        ELSE IF(AA.EQ.')')THEN
          ICOUNT=ICOUNT-1
        ELSE IF(ATERM(ITERM)(1:6).EQ.'~#str_')THEN
          ICOUNT=ICOUNT+1
        ELSE IF(ATERM(ITERM)(1:6).EQ.'~#fin_')THEN
          ICOUNT=ICOUNT-1
        ELSE IF((AA.EQ.'+').OR.(AA.EQ.'-').OR.(AA.EQ.'*').OR.             &
        (AA.EQ.'/').OR.(AA.EQ.'^'))THEN
          IORDER(ITERM)=ICOUNT
          IF(ICOUNT.GT.MAXORD)MAXORD=ICOUNT
        ELSE
          IORDER(ITERM)=-1            ! It must be a number.
        END IF
350   CONTINUE

! -- We now look for a calculation to do, starting at the highest level.

      IF(NTERM.GE.3)THEN
        DO 400 I=MAXORD,1,-1
          DO 390 IOPER=1,5
            DO 380 ITERM=2,NTERM-1
              IF(IORDER(ITERM).EQ.I)THEN   !It is an operator
                IF(ATERM(ITERM)(1:1).EQ.OPERAT(IOPER))THEN
                  IF((IORDER(ITERM-1).LT.0).AND.                          &
                     (IORDER(ITERM+1).LT.0))THEN    !numbers either side
                     dtemp1=rterm(iterm-1)
                     dtemp2=rterm(iterm+1)
!                    CALL RLREAD(IFAIL,ATERM(ITERM-1),DTEMP1)
!                    CALL RLREAD(IFAIL,ATERM(ITERM+1),DTEMP2)
                    IF(IOPER.EQ.1)THEN
                      IF(DTEMP1.LT.0.0)THEN
                        IF(DTEMP2.NE.FLOAT(NINT(DTEMP2)))THEN
                          write(amessage,384)
384                       format('negative number raised to fractional power in series ', &
                          'or weights equation.')
                          go to 9999
                        end IF
                      end IF
                      DVAL=DTEMP1**DTEMP2
                    ELSE IF(IOPER.EQ.3)THEN
                      DVAL=DTEMP1*DTEMP2
                    ELSE IF(IOPER.EQ.2)THEN
                      IF(DTEMP2.EQ.0.0D0) THEN
                        WRITE(amessage,385)
385                     FORMAT('divide by zero in series or weights equation.')
                        go to 9999
                      END IF
                      DVAL=DTEMP1/DTEMP2
                    ELSE IF(IOPER.EQ.5)THEN
                      DVAL=DTEMP1+DTEMP2
                    ELSE IF(IOPER.EQ.4)THEN
                      DVAL=DTEMP1-DTEMP2
                    END IF
                    rterm(iterm)=dval
                    aterm(iterm)='~!~'
                    ATERM(ITERM-1)(1:1)=CHAR(220)
                    ATERM(ITERM+1)(1:1)=CHAR(220)
                    CALL COMPRESS(MAXTERM,NTERM,ATERM,BTERM,rterm)
                    GO TO 100
                  END IF
                END IF
              END IF
380         CONTINUE
390       CONTINUE
400     CONTINUE
      END IF
      WRITE(amessage,410)
410   FORMAT('cannot evaluate series equation.')
      go to 9999

9999  IFAIL=1
      RETURN
      END SUBROUTINE EVALUATE




      SUBROUTINE FUNCEVAL(JERR,ATERM1,rterm,DVAL)

! -- Subroutine FUNCEVAL evaluates a function.


      implicit none
      INTEGER JERR,IFN,IFAIL
      DOUBLE PRECISION DVAL,DTEMP,rterm
      CHARACTER*(*) ATERM1

! -- First we find out which function we are evaluating.

      JERR=0
      ATERM1(1:6)=' '
      aterm1=adjustl(aterm1)
      call char2num(ifail,aterm1,ifn)
      dtemp=rterm
!      CALL INREAD(IFAIL,ATERM1,IFN)
!      CALL RLREAD(IFAIL,ATERM2,DTEMP)
!      IF(IFAIL.NE.0) GO TO 9000
      IF(IFN.EQ.1)THEN
        DVAL=ABS(DTEMP)
      ELSE IF(IFN.EQ.2)THEN
        IF((DTEMP.GT.1.0D0).OR.(DTEMP.LT.-1.0D0))GO TO 9000
        DVAL=ACOS(DTEMP)
      ELSE IF(IFN.EQ.3)THEN
        IF((DTEMP.GT.1.0D0).OR.(DTEMP.LT.-1.0D0))GO TO 9000
        DVAL=ASIN(DTEMP)
      ELSE IF(IFN.EQ.4)THEN
        DVAL=ATAN(DTEMP)
      ELSE IF(IFN.EQ.5)THEN
        IF((DTEMP.GT.1.0D10).OR.(DTEMP.LT.-1.0D10))GO TO 9000
        DVAL=COS(DTEMP)
      ELSE IF(IFN.EQ.6)THEN
        DVAL=COSH(DTEMP)
      ELSE IF(IFN.EQ.7)THEN
        IF(DTEMP.GT.500.0D0) GO TO 9000
        DVAL=EXP(DTEMP)
      ELSE IF(IFN.EQ.8)THEN
        IF(DTEMP.LE.0.0D0) GO TO 9000
        DVAL=LOG(DTEMP)
      ELSE IF(IFN.EQ.9)THEN
        IF(DTEMP.LE.0.0D0) GO TO 9000
        DVAL=LOG10(DTEMP)
      ELSE IF(IFN.EQ.10)THEN
        IF((DTEMP.GT.1.0D10).OR.(DTEMP.LT.-1.0D10))GO TO 9000
        DVAL=SIN(DTEMP)
      ELSE IF(IFN.EQ.11)THEN
        DVAL=SINH(DTEMP)
      ELSE IF(IFN.EQ.12)THEN
        IF(DTEMP.LT.0.0D0) GO TO 9000
        DVAL=SQRT(DTEMP)
      ELSE IF(IFN.EQ.13)THEN
        IF((DTEMP.GT.1.0E10).OR.(DTEMP.LT.-1.0E10))GO TO 9000
        DVAL=TAN(DTEMP)
      ELSE IF(IFN.EQ.14)THEN
        DVAL=TANH(DTEMP)
      ELSE IF(IFN.EQ.15)THEN
        DVAL=-DTEMP
      ELSE IF(IFN.EQ.16)THEN
        DVAL=DTEMP
      END IF

      RETURN

! -- An error condition has occurred.

9000  JERR=IFN
      RETURN

      end SUBROUTINE FUNCEVAL

subroutine prepare_eqn(ifail,nterm,eqntext,iseries)

! -- Subroutine PREPARE_EQN prepares a weights equation for use in weights calculation.

       implicit none
       integer, intent(out)         :: ifail
       integer, intent(out)         :: nterm
       character*(*), intent(inout) :: eqntext
       integer, intent(in)          :: iseries

       integer ierr,iterm,k,isnum,i,ddx,mmx,yyx,hhx,nnx,ssx,idx,nex,sex,lnx
       double precision dtempx
       character(1)aa
       character(25)adate_atime

       ifail=0
       call parse(ierr,MAXTERM,nterm,noper,eqntext,aterm,bterm,nfunct,funct,  &
       operat,rterm,1)
       if(ierr.ne.0) then
         ifail=1
         return
       end if

! -- Series are identified in the weights equation.

       call series_sub(ierr,NTERM,1)
       if(ierr.ne.0) then
         ifail=1
         return
       end if

       do i=1,nterm
         if(aterm(i)(1:3).eq.'$~$')then
           if(iseries.eq.0)then
             write(amessage,5)
5            format('a series name cannot be cited in the weights equation for ', &
             'an s_table, v_table or e_table.')
             ifail=1
             return
           end if
           call char2num(ierr,aterm(i)(4:),isnum)
           if(ierr.ne.0)then
             write(amessage,250)
250          format('internal error - contact programmer.')
             ifail=1
             return
           end if
           if(series_g(iseries)%nterm.ne.series_g(isnum)%nterm) go to 9300
           do k=1,series_g(iseries)%nterm
             if(series_g(iseries)%days(k).ne.(series_g(isnum)%days(k))) go to 9300
             if(series_g(iseries)%secs(k).ne.(series_g(isnum)%secs(k))) go to 9300
           end do
         end if
       end do

! -- Numbers are identified and copied to the rterm array.

       do iterm=1,nterm
         aa=aterm(iterm)(1:1)
         if((aa.ne.'(').and.(aa.ne.')').and.(aa.ne.'+').and.(aa.ne.'-').and.   &
            (aa.ne.'*').and.(aa.ne.'/').and.(aa.ne.'^').and.                   &
            (aterm(iterm)(1:6).ne.'~#str_').and.                               &
            (aterm(iterm)(1:6).ne.'~#fin_').and.                               &
            (aterm(iterm)(1:3).ne.'$~$').and.                                  &
            (aterm(iterm)(1:2).ne.'@_'))then
            call char2num(ierr,aterm(iterm),rterm(iterm))
            if(ierr.ne.0)then
              write(amessage,1870) trim(aterm(iterm))
1870          format('the term "',a,'" in a weights equation cannot be interpreted ', &
              'as a number, function or operator.')
              ifail=1
              return
            end if
            aterm(iterm)='~!~'
         end if
       end do

! -- We now check for intrinsic functions.

       do iterm=1,nterm
         if(aterm(iterm)(1:2).eq.'@_')then
           if(aterm(iterm)(3:).eq.'abs_value')then
             aterm(iterm)(3:)='2'
           elseif(aterm(iterm)(3:).eq.'min')then     ! ** SMW Addition
             aterm(iterm)(3:)='4'                    ! ** SMW Addition
           elseif(aterm(iterm)(3:).eq.'max')then     ! ** SMW Addition
             aterm(iterm)(3:)='5'                    ! ** SMW Addition
           elseif(aterm(iterm)(3:).eq.'obj_fun_value')then     ! ** SMW Addition
             aterm(iterm)(3:)='6'                    ! ** SMW Addition
           elseif(aterm(iterm)(3:).eq.'count')then   ! ** SMW Addition
             aterm(iterm)(3:)='7'                    ! ** SMW Addition
           elseif(aterm(iterm)(3:).eq.'mean')then    ! ** SMW Addition
             aterm(iterm)(3:)='8'                    ! ** SMW Addition
           elseif(aterm(iterm)(3:).eq.'stddev')then  ! ** SMW Addition
             aterm(iterm)(3:)='9'                    ! ** SMW Addition


           else if(aterm(iterm)(3:).eq.'days_start_year')then
             if(iseries.ne.0)then
               aterm(iterm)(3:)='1'
             else
               write(amessage,1875) trim(aterm(iterm))
1875           format('intrinsic function "',a,'" cannot be used in the ',  &
               'weights equation for an s_table, e_table or v_table.')
               ifail=1
               return
             end if
           else if(aterm(iterm)(3:7).eq.'days_')then
             lnx=len(aterm(iterm))
             do idx=1,lnx
               if(aterm(iterm)(idx:idx).eq.char(196))aterm(iterm)(idx:idx)='/'
             end do
             if(iseries.eq.0)then
               write(amessage,1875) trim(aterm(iterm))
               ifail=1
               return
             else
               call getfile(ierr,aterm(iterm),adate_atime,8,lnx)
               if(ierr.ne.0) go to 9400
               idx=index(adate_atime,'_')
               if(idx.eq.0) go to 9400
               call char2date(ierr,adate_atime(1:idx-1),ddx,mmx,yyx)
               if(ierr.ne.0) go to 9400
               call char2time(ierr,adate_atime(idx+1:),hhx,nnx,ssx,ignore_24=1)
               if(ierr.ne.0) go to 9400
!               nex=numdays(1,1,1970,ddx,mmx,yyx)
               nex = julian_day(iMonth=mmx, iDay=ddx, iYear=yyx)
               sex=numsecs(0,0,0,hhx,nnx,ssx)
               dtempx=dble(nex)+dble(sex)/86400.0d0
               aterm(iterm)(3:4)='3_'
               write(aterm(iterm)(5:),'(1pd22.14)') dtempx
             end if
           else
             go to 9400
           end if
         endif
       end do

       return

9300   write(amessage,9310)
9310   format('any series cited in a weights equation must have an identical time-base ', &
       '(ie. the same number of terms, with all dates and times coincident) as the ', &
       'observation time series.')
       ifail=1
       return
9400   write(amessage,9410) trim(aterm(iterm))
9410   format('illegal intrinsic function "',a,'" in weights equation.')
       ifail=1
       return

end subroutine prepare_eqn


end module tsp_equation_parser
