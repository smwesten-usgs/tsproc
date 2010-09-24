module tsp_time_series_processors

contains

subroutine erase_entity(ifail)

! -- Subroutine ERASE_ENTITY removes a TSPROC entity from memory.

       use tsp_data_structures
       use tsp_utilities
       use tsp_command_processors

       implicit none

       integer, intent(out)   :: ifail

       integer iseries,icontext,ierr,istable,ivtable,idtable,j,is,ixcon, &
       ictable,ic
       integer eseries(MAXSERIES),evtable(MAXVTABLE),estable(MAXSTABLE), &
       edtable(MAXDTABLE),ectable(MAXCTABLE)
       character*20 aline
       character*25 aoption
       character*25 acontext(MAXCONTEXT)

       ifail=0
       icontext=0
       iseries=0
       istable=0
       ivtable=0
       idtable=0
       ictable=0
       CurrentBlock_g='ERASE_ENTITY'
       ixcon=0

       write(*,10) trim(CurrentBlock_g)
       write(LU_REC,10)trim(CurrentBlock_g)
10     format(/,' Processing ',a,' block....')

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
         if(aoption.eq.'SERIES_NAME')then
           iseries=iseries+1
           if(iseries.gt.MAXSERIES)then
             call num2char(MAXSERIES,aline)
             write(amessage,100) trim(aline)
100          format('a maximum of ',a,' series can be cited in an ERASE_ENTITY block.')
             go to 9800
           end if
           call get_series_name(ierr,eseries(iseries),'SERIES_NAME')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'S_TABLE_NAME')then
           istable=istable+1
           if(istable.gt.MAXSTABLE)then
             call num2char(MAXSTABLE,aline)
             write(amessage,102) trim(aline)
102          format('a maximum of ',a,' s_tables can be cited in an ERASE_ENTITY block.')
             go to 9800
           end if
           call get_table_name(ierr,estable(istable),1)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'C_TABLE_NAME')then
           ictable=ictable+1
           if(ictable.gt.MAXCTABLE)then
             call num2char(MAXCTABLE,aline)
             write(amessage,110) trim(aline)
110          format('a maximum of ',a,' c_tables can be cited in an ERASE_ENTITY block.')
             go to 9800
           end if
           call get_table_name(ierr,ectable(ictable),4)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'V_TABLE_NAME')then
           ivtable=ivtable+1
           if(ivtable.gt.MAXVTABLE)then
             call num2char(MAXVTABLE,aline)
             write(amessage,103) trim(aline)
103          format('a maximum of ',a,' v_tables can be cited in an ERASE_ENTITY block.')
             go to 9800
           end if
           call get_table_name(ierr,evtable(ivtable),2)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'E_TABLE_NAME')then
           idtable=idtable+1
           if(idtable.gt.MAXDTABLE)then
             call num2char(MAXDTABLE,aline)
             write(amessage,104) trim(aline)
104          format('a maximum of ',a,' e_tables can be cited in an ERASE_ENTITY block.')
             go to 9800
           end if
           call get_table_name(ierr,edtable(idtable),3)
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
         else
           call num2char(ILine_g,aline)
           call addquote(sInfile_g,sString_g)
           write(amessage,180) trim(aoption),trim(aline),trim(sString_g)
180        format('unexpected keyword - "',a,'" in ERASE_ENTITY block at line ',a, &
           ' of file ',a)
           go to 9800
         end if
       end do

! -- The block has been read; now it is checked for correctness.

200    continue
       if((iseries.eq.0).and.(istable.eq.0).and.(ivtable.eq.0).and.   &
          (idtable.eq.0).and.(ictable.eq.0))then
         write(amessage,210)
210      format('no series or tables have been named for deletion in ERASE_ENTITY block.')
         go to 9800
       end if
       if(icontext.eq.0)then
         write(amessage,220)
220      format('no CONTEXT keyword(s) provided in ERASE_ENTITY block.')
         go to 9800
       end if

       if(iseries.eq.0) go to 300
       do j=1,iseries
         is=eseries(j)
         deallocate(series_g(is)%days,series_g(is)%secs,series_g(is)%val,stat=ierr)
         if(ierr.ne.0)then
           write(amessage,230)
230        format('cannot de-allocate memory previously allocated to erased time series.')
           go to 9800
         end if
         nullify(series_g(is)%days,series_g(is)%secs,series_g(is)%val)
         series_g(is)%active=.false.
         series_g(is)%nterm=0
         series_g(is)%type=' '
         write(*,250) trim(series_g(is)%name)
         write(LU_REC,250) trim(series_g(is)%name)
250      format(t5,'Series "',a,'" erased.')
         series_g(is)%name=' '
       end do

300    continue
       if(istable.eq.0) go to 350
       do j=1,istable
         is=estable(j)
         stable_g(is)%active=.false.
         write(*,320) trim(stable_g(is)%name)
         write(LU_REC,320) trim(stable_g(is)%name)
320      format(t5,'s_table "',a,'" erased.')
         stable_g(is)%name=' '
       end do

350    continue
       if(ictable.eq.0) go to 400
       do j=1,ictable
         ic=ectable(j)
         ctable_g(ic)%active=.false.
         write(*,321) trim(ctable_g(ic)%name)
         write(LU_REC,321) trim(ctable_g(ic)%name)
321      format(t5,'c_table "',a,'" erased.')
         ctable_g(ic)%name=' '
       end do

400    continue
       if(ivtable.eq.0) go to 500
       do j=1,ivtable
         is=evtable(j)
         vtable_g(is)%active=.false.
         deallocate(vtable_g(is)%days1,vtable_g(is)%days2,vtable_g(is)%secs1,  &
                    vtable_g(is)%secs2,vtable_g(is)%vol,stat=ierr)
         if(ierr.ne.0)then
           write(amessage,420)
420        format('cannot de-allocate memory previously allocated to erased V_TABLE.')
           go to 9800
         end if
         nullify(vtable_g(is)%days1,vtable_g(is)%days2,vtable_g(is)%secs1,   &
                 vtable_g(is)%secs2,vtable_g(is)%vol)
         vtable_g(is)%nterm=0
         vtable_g(is)%series_name=' '
         write(*,430) trim(vtable_g(is)%name)
         write(LU_REC,430) trim(vtable_g(is)%name)
430      format(t5,'v_table "',a,'" erased.')
         vtable_g(is)%name=' '
       end do

500    continue
       if(idtable.eq.0) go to 600
       do j=1,idtable
         is=edtable(j)
         dtable_g(is)%active=.false.
         deallocate(dtable_g(is)%flow,dtable_g(is)%time,dtable_g(is)%tdelay,stat=ierr)
         if(ierr.ne.0)then
           write(amessage,520)
520        format('cannot de-allocate memory previously allocated to erased E_TABLE.')
           go to 9800
         end if
         nullify(dtable_g(is)%time,dtable_g(is)%flow,dtable_g(is)%tdelay)
         dtable_g(is)%nterm=0
         dtable_g(is)%series_name=' '
         write(*,521) trim(dtable_g(is)%name)
         write(LU_REC,521) trim(dtable_g(is)%name)
521      format(t5,'e_table "',a,'" erased.')
         dtable_g(is)%name=' '
       end do
600    continue
       return

9000   call num2char(ILine_g,aline)
       call addquote(sInfile_g,sString_g)
       write(amessage,9010) trim(aline), trim(sString_g)
9010   format('cannot read line ',a,' of TSPROC input file ',a)
       go to 9800
9100   continue
       call addquote(sInfile_g,sString_g)
       write(amessage,9110) trim(sString_g),trim(CurrentBlock_g)
9110   format('unexpected end encountered to TSPROC input file ',a,' while ', &
       ' reading ',a,' block.')
       go to 9800

9800   call write_message(leadspace='yes',error='yes')
       call write_message(iunit=LU_REC,leadspace='yes')
       ifail=1

       return

end subroutine erase_entity

subroutine moving_window(ifail)

! -- Subroutine MOVING is still quick and dirty. It calculates the minimum sample
!    value within a window consisting of an odd number of terms.

       use tsp_data_structures
       use tsp_utilities
       use tsp_command_processors

       implicit none

       integer, intent(out)   :: ifail

       integer icontext,iseries,ixcon,ierr,itemp,iterm,i,j,wt2,l,winterms,imode,  &
       icount,is,ie,iiterm,k
       real rtemp,first_value,last_value
       character (len=iTSNAMELENGTH) :: aname,atemp
       character*15 aline,amode
       character*25 aoption
       character*25 acontext(MAXCONTEXT)

       ifail=0
       CurrentBlock_g='MOVING_MINIMUM'

       write(*,10) trim(CurrentBlock_g)
       write(LU_REC,10) trim(CurrentBlock_g)
10     format(/,' Processing ',a,' block....')

       icontext=0
       iseries=0
       aname=' '
       winterms=-99999999
       ixcon=0
       amode=' '
       first_value=-1.1e30
       last_value=-1.1e30

! -- The MOVING_MINIMUM block is first parsed.

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
         if(aoption.eq.'CONTEXT')then
           if(ixcon.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,41) trim(aline),trim(sString_g)
41           format('CONTEXT keyword in incorrect location at line ',a,' of file ',a)
             go to 9800
           end if
           call get_context(ierr,icontext,acontext)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'TERMS_IN_WINDOW')then
           call get_keyword_value(ierr,1,winterms,rtemp,aoption)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'FIRST_VALUE')then
           call get_keyword_value(ierr,2,itemp,first_value,aoption)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'LAST_VALUE')then
           call get_keyword_value(ierr,2,itemp,last_value,aoption)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'NEW_SERIES_NAME')then
           call get_new_series_name(ierr,aname)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'SERIES_NAME')then
           call get_series_name(ierr,iseries,'SERIES_NAME')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'MODE')then
           amode=cline(left_word(2):right_word(2))
           call casetrans(amode,'lo')
           write(*,89) trim(amode)
           write(LU_REC,89) trim(amode)
89         format(t5,'MODE ',a)
         else if(aoption.eq.'END')then
           go to 200
         else
           call num2char(ILine_g,aline)
           call addquote(sInfile_g,sString_g)
           write(amessage,90) trim(aoption),trim(CurrentBlock_g),trim(aline),trim(sString_g)
90         format('unexpected keyword - "',a,'" in ',a,' block at line ',a, &
           ' of file ',a)
           go to 9800
         end if
       end do

! -- The block has been read; now it is checked for correctness.

200    continue
       if(iseries.eq.0)then
         write(amessage,210) trim(CurrentBlock_g)
210      format('no SERIES_NAME keyword provided in ',a,' block.')
         go to 9800
       end if
       if(aname.eq.' ')then
         write(amessage,230) trim(CurrentBlock_g)
230      format('no NEW_SERIES_NAME provided in ',a,' block.')
         go to 9800
       end if
       if(amode.eq.' ')then
         write(amessage,231) trim(CurrentBlock_g)
231      format('no MODE keyword provided in ',a,' block.')
         go to 9800
       end if
       if(icontext.eq.0)then
         write(amessage,220) trim(CurrentBlock_g)
220      format('no CONTEXT keyword(s) provided in ',a,' block.')
         go to 9800
       end if
       if(winterms.eq.-99999999)then
         write(amessage,225) trim(CurrentBlock_g)
225      format('no TERMS_IN_WINDOW keyword(s)provided in ',a,' block.')
         go to 9800
       else
         if(winterms.le.0)then
           write(amessage,226) trim(CurrentBlock_g)
226        format('value for TERMS_IN_WINDOW must be positve in ',a,' block.')
           go to 9800
         end if
         if((winterms/2)*2.eq.winterms)then
           write(amessage,227) trim(CurrentBlock_g)
227        format('TERMS_IN_WINDOW must be an odd number in ',a,' block.')
           go to 9800
         end if
       end if
       if(amode.eq.'continuous')then
         imode=1
       else if(amode.eq.'discrete')then
         imode=2
       else
         write(amessage,228) trim(CurrentBlock_g)
228      format('MODE must be "discrete" or "continuous" in ',a,' block.')
         go to 9800
       end if
       if(imode.eq.2)then
         if(first_value.lt.-1.0e30)then
           write(amessage,340) trim(CurrentBlock_g)
340        format('no FIRST_VALUE keyword supplied in ',a,' block.')
           go to 9800
         end if
         if(last_value.lt.-1.0e30)then
           write(amessage,341) trim (CurrentBlock_g)
341        format('no LAST_VALUE keyword supplied in ',a,' block.')
           go to 9800
         end if
       end if

! -- The new series is now written.

       if(imode.eq.2) go to 900
       iterm=series_g(iseries)%nterm
       do i=1,MAXSERIES
         if(.not.series_g(i)%active) go to 515
       end do
       write(amessage,510)
510    format('no more time series available for data storage - increase MAXSERIES and ', &
       'recompile program.')
       go to 9800

515    allocate(series_g(i)%days(iterm),series_g(i)%secs(iterm),  &
       series_g(i)%val(iterm),stat=ierr)
       if(ierr.ne.0)then
         write(amessage,550)
550      format('cannot allocate memory for another time series.')
         go to 9800
       end if
       series_g(i)%active=.true.
       series_g(i)%name=aname
       series_g(i)%nterm=iterm
       series_g(i)%type='ts'
       do j=1,iterm
         series_g(i)%days(j)=series_g(iseries)%days(j)
       end do
       do j=1,iterm
         series_g(i)%secs(j)=series_g(iseries)%secs(j)
       end do
       wt2=winterms/2
       if(iterm.le.wt2*2)then
         do j=1,iterm
           series_g(i)%val(j)=series_g(iseries)%val(j)
         end do
       else
         do j=1,wt2
           series_g(i)%val(j)=series_g(iseries)%val(j)
         end do
         itemp=iterm-wt2+1
         do j=itemp,iterm
           series_g(i)%val(j)=series_g(iseries)%val(j)
         end do
         do j=wt2+1,itemp-1
           rtemp=1e30
           do l=j-wt2,j+wt2
             if(series_g(iseries)%val(l).lt.rtemp)rtemp=series_g(iseries)%val(l)
           end do
           series_g(i)%val(j)=rtemp
         end do
       end if

       write(*,590) trim(aname)
       write(LU_REC,590) trim(aname)
590    format(t5,'Series "',a,'" successfully calculated.')
       return

900    continue

! -- The following refers to discrete mode.
!    First we find out how many terms will be required.

       icount=0
       iterm=series_g(iseries)%nterm
       wt2=winterms/2
       is=wt2+1
       ie=iterm-wt2
       do j=is,ie
         rtemp=series_g(iseries)%val(j)
         do k=j-wt2,j+wt2
            if(j.eq.k) cycle
           if(rtemp.ge.series_g(iseries)%val(k)) go to 930
         end do
         icount=icount+1
930      continue
       end do

       do i=1,MAXSERIES
         if(.not.series_g(i)%active) go to 1515
       end do
       write(amessage,510)
       go to 9800
1515   continue
       iiterm=icount+2
       allocate(series_g(i)%days(iiterm),series_g(i)%secs(iiterm),  &
       series_g(i)%val(iiterm),stat=ierr)
       if(ierr.ne.0)then
         write(amessage,550)
         go to 9800
       end if
       series_g(i)%active=.true.
       series_g(i)%name=aname
       series_g(i)%nterm=iiterm
       series_g(i)%type='ts'
       series_g(i)%val(1)=first_value
       series_g(i)%days(1)=series_g(iseries)%days(1)
       series_g(i)%secs(1)=series_g(iseries)%secs(1)
       series_g(i)%val(iiterm)=last_value
       series_g(i)%days(iiterm)=series_g(iseries)%days(iterm)
       series_g(i)%secs(iiterm)=series_g(iseries)%secs(iterm)
       icount=1
       do j=is,ie
         rtemp=series_g(iseries)%val(j)
         do k=j-wt2,j+wt2
           if(j.eq.k) cycle
           if(rtemp.ge.series_g(iseries)%val(k)) go to 950
         end do
         icount=icount+1
         series_g(i)%val(icount)=series_g(iseries)%val(j)
         series_g(i)%days(icount)=series_g(iseries)%days(j)
         series_g(i)%secs(icount)=series_g(iseries)%secs(j)
950      continue
       end do

       return

9000   call num2char(ILine_g,aline)
       call addquote(sInfile_g,sString_g)
       write(amessage,9010) trim(aline), trim(sString_g)
9010   format('cannot read line ',a,' of TSPROC input file ',a)
       go to 9800
9100   continue
       call addquote(sInfile_g,sString_g)
       write(amessage,9110) trim(sString_g), trim(CurrentBlock_g)
9110   format('unexpected end encountered to TSPROC input file ',a,' while ', &
       ' reading ',a,' block.')
       go to 9800

9800   call write_message(leadspace='yes',error='yes')
       call write_message(iunit=LU_REC,leadspace='yes')
       ifail=1

       return

end subroutine moving_window


subroutine new_series_uniform(ifail)

! -- Subroutine NEW_SERIES_UNIFORM generates a uniform, equispaced, time series.

       use tsp_data_structures
       use tsp_utilities
       use tsp_command_processors

       implicit none

       integer, intent(out)   :: ifail

       integer dd1,mm1,yy1,hh1,nn1,ss1,dd2,mm2,yy2,hh2,nn2,ss2,ierr, &
       icontext,i,begdays,begsecs,enddays,endsecs,ixcon,iterm,ifac
       integer time_interval,time_unit,ival,tterm,iseries,dd,mm,yy,itemp
       real rtemp,rval
       double precision timediff,timeinc
       character (len=iTSNAMELENGTH) :: aname
       character*15 aline
       character*25 aoption
       character*30 atemp
       character*25 acontext(MAXCONTEXT)

       ifail=0
       CurrentBlock_g='NEW_SERIES_UNIFORM'

       write(*,10) trim(CurrentBlock_g)
       write(LU_REC,10) trim(CurrentBlock_g)
10     format(/,' Processing ',a,' block....')

       icontext=0
       ixcon=0
       yy1=-9999
       hh1=-9999
       yy2=-9999
       hh2=-9999
       time_unit=-9999
       time_interval=-9999
       ival=-9999
       aname=' '

! -- The NEW_SERIES_UNIFORM block is first parsed.

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
         if(aoption.eq.'DATE_1')then
           call get_date(ierr,dd1,mm1,yy1,'DATE_1')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'DATE_2')then
           call get_date(ierr,dd2,mm2,yy2,'DATE_2')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'TIME_1')then
           call get_time(ierr,hh1,nn1,ss1,'TIME_1')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'TIME_2')then
           call get_time(ierr,hh2,nn2,ss2,'TIME_2')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'TIME_INTERVAL')then
           call get_keyword_value(ierr,1,time_interval,rtemp,'TIME_INTERVAL')
           if(ierr.ne.0) go to 9800
           if(time_interval.le.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,65) trim(aline),trim(sString_g)
65           format('time interval must be positive at line ',a,' of file ',a)
             go to 9800
           end if
         else if(aoption.eq.'TIME_UNIT')then
           call getfile(ierr,cline,atemp,left_word(2),right_word(2))
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,50) trim(aline),trim(sString_g)
50           format('cannot read time unit from line ',a,' of file ',a)
             go to 9800
           end if
           call casetrans(atemp,'lo')
           if(atemp(1:3).eq.'sec') then
             time_unit=1
             atemp='seconds'
           else if(atemp(1:3).eq.'min')then
             time_unit=2
             atemp='minutes'
           else if(atemp(1:3).eq.'hou')then
             time_unit=3
             atemp='hours'
           else if(atemp(1:3).eq.'day')then
             time_unit=4
             atemp='days'
           else if(atemp(1:3).eq.'mon')then
             time_unit=5
             atemp='months'
           else if(atemp(1:3).eq.'yea')then
             time_unit=6
             atemp='years'
           else
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,51) trim(aline),trim(sString_g)
51           format('illegal time unit at line ',a,' of file ',a)
             go to 9800
           end if
           write(*,54) trim(atemp)
           write(LU_REC,54) trim(atemp)
54         format(t5,'TIME UNIT ',a)
         else if(aoption.eq.'NEW_SERIES_NAME')then
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
         else if(aoption.eq.'NEW_SERIES_VALUE')then
           call get_keyword_value(ierr,2,itemp,rval,'NEW_SERIES_VALUE')
           if(ierr.ne.0) go to 9800
           ival=1
         else if(aoption.eq.'END')then
           go to 100
         else
           call num2char(ILine_g,aline)
           call addquote(sInfile_g,sString_g)
           write(amessage,80) trim(aoption),trim(CurrentBlock_g),trim(aline),trim(sString_g)
80         format('unexpected keyword - "',a,'" in ',a,' block at line ',a, &
           ' of file ',a)
           go to 9800
         end if
       end do

! -- If there are any absences in the GET_MUL_SERIES_SSF block, these are now reported.

100    continue
       if(icontext.eq.0)then
         call addquote(sInfile_g,sString_g)
         write(amessage,122) trim(CurrentBlock_g),trim(sString_g)
122      format('no CONTEXT keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       if(ival.ne.1)then
         write(amessage,123) trim(CurrentBlock_g),trim(sString_g)
123      format('no NEW_SERIES_VALUE keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       if(time_unit.eq.-9999)then
         write(amessage,124) trim(CurrentBlock_g),trim(sString_g)
124      format('no TIME_UNIT keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       if(aname.eq.' ')then
         write(amessage,129) trim(CurrentBlock_g),trim(sString_g)
129      format('no NEW_SERIES_NAME keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       if(time_interval.eq.-9999)then
         write(amessage,125) trim(CurrentBlock_g),trim(sString_g)
125      format('no TIME_INTERVAL keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       if((yy1.eq.-9999).or.(hh1.eq.-9999).or.(yy2.eq.-9999).or.(hh2.eq.-9999))then
         write(amessage,126) trim(CurrentBlock_g),trim(sString_g)
126      format('all of DATE_1, TIME_1, DATE_2, TIME_2 keywords must be provided in ',a,  &
         ' block in file ',a)
         go to 9800
       end if
       call date_check(ierr,yy1,mm1,dd1,hh1,nn1,ss1,yy2,mm2,dd2,hh2,nn2,ss2,  &
       begdays,begsecs,enddays,endsecs)
       if(ierr.ne.0) go to 9800
       if(begsecs.ge.86400)then
         begsecs=begsecs-86400
         begdays=begdays+1
       end if
       if(endsecs.ge.86400)then
         endsecs=endsecs-86400
         enddays=enddays+1
       end if

! -- Some errors are checked for.

       if(time_unit.eq.5)then
         if(dd1.gt.28)then
           write(amessage,82)
82         format('if TIME_UNIT is set to "month" then the day in DATE_1 must ',  &
           'not be greater than 28.')
           go to 9800
         end if
       end if
       if(time_unit.eq.6)then
         if((dd1.gt.28).and.(mm1.eq.2))then
           write(amessage,83)
83         format('if TIME_UNIT is set to "year" then DATE_1 must not be ',   &
           '28th or 29th February.')
           go to 9800
         end if
       end if

! -- Roughly the number of terms in the series is now evaluated so that memory can
!    be allocated for the temporary series.

       timediff=dble(enddays-begdays)*86400.0d0+dble(endsecs-begsecs)
       timeinc=dble(time_interval)
       if(time_unit.eq.1)then
         timeinc=timeinc*1.0d0
       else if(time_unit.eq.2)then
         timeinc=timeinc*60.0d0
       else if(time_unit.eq.3)then
         timeinc=timeinc*3600.0d0
       else if(time_unit.eq.4)then
         timeinc=timeinc*86400.0d0
       else if(time_unit.eq.5)then
         timeinc=timeinc*28.0d0*86400.0d0
       else if(time_unit.eq.6)then
         timeinc=timeinc*365.0d0*86400.0d0
       end if
       tterm=ceiling(timediff/timeinc)+20

! -- The temporary series is allocated.

       call alloc_tempseries(ierr,tterm)
       if(ierr.ne.0) go to 9800

! -- Terms of the temporary series are now created.

       if(time_unit.le.3)then
         ifac=1
         if(time_unit.eq.2)then
           ifac=60
         else if(time_unit.eq.3)then
           ifac=3600
         end if
         tempseries_g%days(1)=begdays
         tempseries_g%secs(1)=begsecs
         i=1
         do
           i=i+1
           tempseries_g%days(i)=tempseries_g%days(i-1)
           tempseries_g%secs(i)=tempseries_g%secs(i-1)+time_interval*ifac
120        continue
           if(tempseries_g%secs(i).ge.86400)then
             tempseries_g%secs(i)=tempseries_g%secs(i)-86400
             tempseries_g%days(i)=tempseries_g%days(i)+1
             go to 120
           end if
           if((tempseries_g%days(i).gt.enddays).or.                     &
             ((tempseries_g%days(i).eq.enddays).and.(tempseries_g%secs(i).gt.endsecs)))then
               iterm=i-1
               go to 500
           end if
         end do
       else if(time_unit.eq.4)then
         tempseries_g%days(1)=begdays
         tempseries_g%secs(1)=begsecs
         i=1
         do
           i=i+1
           tempseries_g%days(i)=tempseries_g%days(i-1)+time_interval
           tempseries_g%secs(i)=tempseries_g%secs(i-1)
           if((tempseries_g%days(i).gt.enddays).or.                     &
             ((tempseries_g%days(i).eq.enddays).and.(tempseries_g%secs(i).gt.endsecs)))then
             iterm=i-1
             go to 500
           end if
         end do
       else if(time_unit.eq.5)then
         tempseries_g%days(1)=begdays
         tempseries_g%secs(1)=begsecs
         dd=dd1
         mm=mm1
         yy=yy1
         i=1
         do
           i=i+1
           mm=mm+time_interval
180        continue
           if(mm.gt.12)then
             mm=mm-12
             yy=yy+1
             go to 180
           end if
           tempseries_g%days(i)=numdays(1,1,1970,dd,mm,yy)
           tempseries_g%secs(i)=tempseries_g%secs(i-1)
           if((tempseries_g%days(i).gt.enddays).or.                     &
             ((tempseries_g%days(i).eq.enddays).and.(tempseries_g%secs(i).gt.endsecs)))then
               iterm=i-1
               go to 500
           end if
         end do
       else if(time_unit.eq.6)then
         tempseries_g%days(1)=begdays
         tempseries_g%secs(1)=begsecs
         dd=dd1
         mm=mm1
         yy=yy1
         i=1
         do
           i=i+1
           yy=yy+time_interval
           tempseries_g%days(i)=numdays(1,1,1970,dd,mm,yy)
           tempseries_g%secs(i)=tempseries_g%secs(i-1)
           if((tempseries_g%days(i).gt.enddays).or.                     &
             ((tempseries_g%days(i).eq.enddays).and.(tempseries_g%secs(i).gt.endsecs)))then
               iterm=i-1
               go to 500
           end if
         end do
       end if

! -- The series has been generated and is now copied from the temporary series.

500    continue

       do iseries=1,MAXSERIES
         if(series_g(iseries)%active) cycle
         go to 510
       end do
510    continue
       allocate(series_g(iseries)%days(iterm),series_g(iseries)%secs(iterm),  &
       series_g(iseries)%val(iterm),stat=ierr)
       if(ierr.ne.0)then
         write(amessage,550)
550      format('cannot allocate memory for another time series.')
         go to 9800
       end if
       series_g(iseries)%active=.true.
       series_g(iseries)%name=aname
       series_g(iseries)%type='ts'
       series_g(iseries)%nterm=iterm
       do i=1,iterm
         series_g(iseries)%days(i)=tempseries_g%days(i)
         series_g(iseries)%secs(i)=tempseries_g%secs(i)
         series_g(iseries)%val(i)=rval
       end do

       write(*,580) trim(aname)
       write(LU_REC,580) trim(aname)
580    format(t5,'Series "',a,'" successfully created.')

       go to 9900

9000   call num2char(ILine_g,aline)
       call addquote(sInfile_g,sString_g)
       write(amessage,9010) trim(aline), trim(sString_g)
9010   format('cannot read line ',a,' of TSPROC input file ',a)
       go to 9800
9100   continue
       call addquote(sInfile_g,sString_g)
       write(amessage,9110) trim(sString_g),trim(CurrentBlock_g)
9110   format('unexpected end encountered to TSPROC input file ',a,' while ', &
       ' reading ',a,' block.')
       go to 9800

9800   call write_message(leadspace='yes',error='yes')
       call write_message(iunit=LU_REC,leadspace='yes')
       ifail=1

9900   continue
       return


end subroutine new_series_uniform

!--------------------------------------------------------------------------------------------

subroutine series_difference(ifail)

! -- Subroutine SERIES_DIFFERENCE computes a new series based on differences of consecutive
!    terms in an existing time series.

       use tsp_data_structures
       use tsp_utilities
       use tsp_command_processors

       implicit none

       integer, intent(out)   :: ifail

       integer icontext,iseries,ixcon,ierr,iterm,i,j
       character (len=iTSNAMELENGTH) :: aname
       character*15 aline
       character*25 aoption
       character*25 acontext(MAXCONTEXT)

       ifail=0
       CurrentBlock_g='SERIES_DIFFERENCE'

       write(*,10) trim(CurrentBlock_g)
       write(LU_REC,10) trim(CurrentBlock_g)
10     format(/,' Processing ',a,' block....')

       icontext=0
       iseries=0
       aname=' '
       ixcon=0

! -- The CLEAN_SERIES block is first parsed.

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
         if(aoption.eq.'CONTEXT')then
           if(ixcon.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,41) trim(aline),trim(sString_g)
41           format('CONTEXT keyword in incorrect location at line ',a,' of file ',a)
             go to 9800
           end if
           call get_context(ierr,icontext,acontext)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'NEW_SERIES_NAME')then
           call get_new_series_name(ierr,aname)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'SERIES_NAME')then
           call get_series_name(ierr,iseries,'SERIES_NAME')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'END')then
           go to 200
         else
           call num2char(ILine_g,aline)
           call addquote(sInfile_g,sString_g)
           write(amessage,90) trim(aoption),trim(CurrentBlock_g),trim(aline),trim(sString_g)
90         format('unexpected keyword - "',a,'" in ',a,' block at line ',a, &
           ' of file ',a)
           go to 9800
         end if
       end do

! -- The block has been read; now it is checked for correctness.

200    continue
       if(iseries.eq.0)then
         write(amessage,210) trim(CurrentBlock_g)
210      format('no SERIES_NAME keyword provided in ',a,' block.')
         go to 9800
       end if
       if(icontext.eq.0)then
         write(amessage,220) trim(CurrentBlock_g)
220      format('no CONTEXT keyword(s) provided in ',a,' block.')
         go to 9800
       end if
       if(series_g(iseries)%nterm.eq.1)then
         write(amessage,213) trim(series_g(iseries)%name),trim(CurrentBlock_g)
213      format('specified series "',a,'" must have more than one term in ',a,' block.')
         go to 9800
       end if
       if(aname.eq.' ')then
         write(amessage,214) trim(CurrentBlock_g)
214      format('no NEW_SERIES_NAME keyword provided in ',a,' block.')
         go to 9800
       end if

! -- Space is allocated for the new time series.

       iterm=series_g(iseries)%nterm-1
       do i=1,MAXSERIES
         if(.not.series_g(i)%active) go to 515
       end do
       write(amessage,510)
510    format('no more time series available for data storage - increase MAXSERIES and ', &
       'recompile program.')
       go to 9800

515    allocate(series_g(i)%days(iterm),series_g(i)%secs(iterm),  &
       series_g(i)%val(iterm),stat=ierr)
       if(ierr.ne.0)then
         write(amessage,550)
550      format('cannot allocate memory for another time series.')
         go to 9800
       end if
       series_g(i)%active=.true.
       series_g(i)%name=aname
       series_g(i)%nterm=iterm
       series_g(i)%type='ts'
       do j=2,series_g(iseries)%nterm
         series_g(i)%days(j-1)=series_g(iseries)%days(j)
         series_g(i)%secs(j-1)=series_g(iseries)%secs(j)
         series_g(i)%val(j-1)=series_g(iseries)%val(j)-series_g(iseries)%val(j-1)
       end do

       write(*,590) trim(aname)
       write(LU_REC,590) trim(aname)
590    format(t5,'Series "',a,'" successfully calculated.')
       return

9000   call num2char(ILine_g,aline)
       call addquote(sInfile_g,sString_g)
       write(amessage,9010) trim(aline), trim(sString_g)
9010   format('cannot read line ',a,' of TSPROC input file ',a)
       go to 9800
9100   continue
       call addquote(sInfile_g,sString_g)
       write(amessage,9110) trim(sString_g), trim(CurrentBlock_g)
9110   format('unexpected end encountered to TSPROC input file ',a,' while ', &
       ' reading ',a,' block.')
       go to 9800

9800   call write_message(leadspace='yes',error='yes')
       call write_message(iunit=LU_REC,leadspace='yes')
       ifail=1

       return

end subroutine series_difference

subroutine series_base_level(ifail)

! -- Subroutine SERIES_BASE_LEVEL subtracts a constant amount from a series, this
!    amount being an element of the same or another series.

       use tsp_data_structures
       use tsp_utilities
       use tsp_command_processors

       implicit none

       integer, intent(out)   :: ifail

       integer icontext,iseries,ixcon,ierr,isub,jseries,bseries,ddb,mmb,yyb, &
       hhb,nnb,ssb,daysb,secsb,iterm,i,j,ineg
       real rbase
       character (len=iTSNAMELENGTH) :: aname,atemp
       character*15 aline
       character*25 aoption
       character*25 acontext(MAXCONTEXT)

       ifail=0
       CurrentBlock_g='SERIES_BASE_LEVEL'

       write(*,10) trim(CurrentBlock_g)
       write(LU_REC,10) trim(CurrentBlock_g)
10     format(/,' Processing ',a,' block....')

       icontext=0
       iseries=0
       aname=' '
       ixcon=0
       isub=-9999
       jseries=-9999
       bseries=-9999
       ddb=-9999
       hhb=-9999
       ineg=0

! -- The SERIES_BASE_LEVEL block is first parsed.

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
         if(aoption.eq.'CONTEXT')then
           if(ixcon.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,41) trim(aline),trim(sString_g)
41           format('CONTEXT keyword in incorrect location at line ',a,' of file ',a)
             go to 9800
           end if
           call get_context(ierr,icontext,acontext)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'SUBSTITUTE')then
           call getfile(ierr,cline,atemp,left_word(2),right_word(2))
           if(atemp.eq.'yes')then
             isub=1
           else if(atemp.eq.'no')then
             isub=0
           else
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,42) trim(aline),trim(sString_g)
42           format('"yes" or "no" should follow the SUBSTITUTE ', &
             'keyword at line ',a,' of file ',a)
             go to 9800
           end if
           write(*,44) trim(aoption),trim(atemp)
           write(LU_REC,44) trim(aoption),trim(atemp)
44         format(t5,a,1x,a)
         else if(aoption.eq.'NEW_SERIES_NAME')then
           call get_new_series_name(ierr,aname)
           if(ierr.ne.0) go to 9800
           jseries=1
         else if(aoption.eq.'SERIES_NAME')then
           call get_series_name(ierr,iseries,'SERIES_NAME')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'BASE_LEVEL_SERIES_NAME')then
           call get_series_name(ierr,bseries,'BASE_LEVEL_SERIES_NAME')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'BASE_LEVEL_DATE')then
           call get_date(ierr,ddb,mmb,yyb,'BASE_LEVEL_DATE')
           if(ierr.ne.0) go to 9800
           daysb=numdays(1,1,1970,ddb,mmb,yyb)
         else if(aoption.eq.'BASE_LEVEL_TIME')then
           call get_time(ierr,hhb,nnb,ssb,'BASE_LEVEL_TIME')
           if(ierr.ne.0) go to 9800
           secsb=hhb*3600+nnb*60+ssb
         else if(aoption.eq.'NEGATE')then
           call getfile(ierr,cline,atemp,left_word(2),right_word(2))
           if(atemp.eq.'yes')then
             ineg=1
           else if(atemp.eq.'no')then
             ineg=0
           else
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,49) trim(aline),trim(sString_g)
49           format('"yes" or "no" should follow the NEGATE ', &
             'keyword at line ',a,' of file ',a)
             go to 9800
           end if
           write(*,44) trim(aoption),trim(atemp)
           write(LU_REC,44) trim(aoption),trim(atemp)
         else if(aoption.eq.'END')then
           go to 200
         else
           call num2char(ILine_g,aline)
           call addquote(sInfile_g,sString_g)
           write(amessage,90) trim(aoption),trim(CurrentBlock_g),trim(aline),trim(sString_g)
90         format('unexpected keyword - "',a,'" in ',a,' block at line ',a, &
           ' of file ',a)
           go to 9800
         end if
       end do

! -- The block has been read; now it is checked for correctness.

200    continue
       if(iseries.eq.0)then
         write(amessage,210) trim(CurrentBlock_g)
210      format('no SERIES_NAME keyword provided in ',a,' block.')
         go to 9800
       end if
       if(isub.eq.-9999)then
         write(amessage,211) trim(CurrentBlock_g)
211      format('no SUBSTITUTE keyword supplied in ',a,' block.')
         go to 9800
       end if
       if((isub.eq.1).and.(jseries.ne.-9999))then
         write(amessage,220) trim(CurrentBlock_g)
220      format('if SUBSTITUTE is set to "yes" then a NEW_SERIES_NAME ', &
         'keyword must not be supplied in ',a,' block.')
         go to 9800
       end if
       if((isub.eq.0).and.(jseries.eq.-9999))then
         write(amessage,222) trim(CurrentBlock_g)
222      format('if SUBSTITUTE is set to "no" then a NEW_SERIES_NAME ', &
         'keyword must be supplied in ',a,' block.')
         go to 9800
       end if
       if(bseries.eq.-9999)then
         write(amessage,230) trim(CurrentBlock_g)
230      format('no BASE_LEVEL_SERIES_NAME keyword supplied in ',a,' block.')
         go to 9800
       end if
       if(ddb.eq.-9999)then
         write(amessage,240) trim(CurrentBlock_g)
240      format('no BASE_LEVEL_DATE keyword supplied in ',a,' block.')
         go to 9800
       end if
       if(hhb.eq.-9999)then
         write(amessage,250) trim(CurrentBlock_g)
250      format('no BASE_LEVEL_TIME keyword supplied in ',a,' block.')
         go to 9800
       end if
       if(secsb.ge.86400)then
         daysb=daysb+1
         secsb=secsb-86400
       end if
       if(icontext.eq.0)then
         write(amessage,260) trim(CurrentBlock_g)
260      format('no CONTEXT keyword(s) provided in ',a,' block.')
         go to 9800
       end if

! -- The new series base level is now determined.

       do i=1,series_g(bseries)%nterm
         if((series_g(bseries)%days(i).eq.daysb).and.  &
            (series_g(bseries)%secs(i).eq.secsb))then
            rbase=series_g(bseries)%val(i)
            go to 300
         end if
       end do
       write(amessage,280) trim(series_g(bseries)%name),trim(CurrentBlock_g)
280    format('no member of BASE_LEVEL_SERIES "',a,'" has a date and time ', &
       'corresponding to those supplied with the BASE_LEVEL_DATE and ', &
       'BASE_LEVEL_TIME keywords in ',a,' block.')
       go to 9800
300    continue

! -- If no new series is required, the base level change is now undertaken.

       if(jseries.eq.-9999)then
         if(ineg.eq.0)then
           do i=1,series_g(iseries)%nterm
             series_g(iseries)%val(i)=series_g(iseries)%val(i)-rbase
           end do
         else
           do i=1,series_g(iseries)%nterm
             series_g(iseries)%val(i)=rbase-series_g(iseries)%val(i)
           end do
         end if
         go to 900
       end if

! -- If a new time series is warranted, then space is allocated for it.

       if(jseries.ne.-9999)then
         do i=1,MAXSERIES
           if(.not.series_g(i)%active) go to 515
         end do
         write(amessage,510)
510      format('no more time series available for data storage - increase MAXSERIES and ', &
         'recompile program.')
         go to 9800

515      continue
         iterm=series_g(iseries)%nterm
         allocate(series_g(i)%days(iterm),series_g(i)%secs(iterm),  &
         series_g(i)%val(iterm),stat=ierr)
         if(ierr.ne.0)then
           write(amessage,550)
550        format('cannot allocate memory for another time series.')
           go to 9800
         end if
         series_g(i)%active=.true.
         series_g(i)%name=aname
         series_g(i)%nterm=iterm
         series_g(i)%type='ts'
         do j=1,series_g(iseries)%nterm
           series_g(i)%days(j)=series_g(iseries)%days(j)
         end do
         do j=1,series_g(iseries)%nterm
           series_g(i)%secs(j)=series_g(iseries)%secs(j)
         end do
         if(ineg.eq.0)then
           do j=1,series_g(iseries)%nterm
             series_g(i)%val(j)=series_g(iseries)%val(j)-rbase
           end do
         else
           do j=1,series_g(iseries)%nterm
             series_g(i)%val(j)=rbase-series_g(iseries)%val(j)
           end do
         end if
       end if

900    continue
       if(jseries.eq.-9999)then
         write(*,910) trim(series_g(iseries)%name)
         write(LU_REC,910) trim(series_g(iseries)%name)
910      format(t5,'New base level applied to series "',a,'".')
       else
         write(*,920) trim(series_g(i)%name)
         write(LU_REC,920) trim(series_g(i)%name)
920      format(t5,'New series "',a,'" successfully calculated.')
       end if
       return

9000   call num2char(ILine_g,aline)
       call addquote(sInfile_g,sString_g)
       write(amessage,9010) trim(aline), trim(sString_g)
9010   format('cannot read line ',a,' of TSPROC input file ',a)
       go to 9800
9100   continue
       call addquote(sInfile_g,sString_g)
       write(amessage,9110) trim(sString_g), trim(CurrentBlock_g)
9110   format('unexpected end encountered to TSPROC input file ',a,' while ', &
       ' reading ',a,' block.')
       go to 9800

9800   call write_message(leadspace='yes',error='yes')
       call write_message(iunit=LU_REC,leadspace='yes')
       ifail=1

       return

end subroutine series_base_level



subroutine vol_to_series(ifail)

! -- Subroutine VOL_TO_SERIES stores a V_TABLE as an S_TABLE.

       use tsp_data_structures
       use tsp_utilities
       use tsp_command_processors

       implicit none

       integer, intent(out)   :: ifail

       integer ivtable,nsterm,idiff,ihalf,isecs,icontext,ixcon,ierr,iser, &
       j,idays
       character (len=iTSNAMELENGTH) :: aname,abscissa
       character*15 aline
       character*25 aoption
       character*25 acontext(MAXCONTEXT)

       ifail=0
       CurrentBlock_g='V_TABLE_TO_SERIES'

       write(*,10) trim(CurrentBlock_g)
       write(LU_REC,10) trim(CurrentBlock_g)
10     format(/,' Processing ',a,' block....')

       ivtable=0
       icontext=0
       abscissa=' '
       aname=' '
       ixcon=0

! -- The V_TABLE_TO_SERIES block is first parsed.

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
         if(aoption.eq.'V_TABLE_NAME')then
           call get_table_name(ierr,ivtable,2)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'NEW_SERIES_NAME')then
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
         else if(aoption.eq.'TIME_ABSCISSA')then
           call getfile(ierr,cline,abscissa,left_word(2),right_word(2))
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,49) trim(aline),trim(sString_g)
49           format('cannot read time abscissa from line ',a,' of file ',a)
             go to 9800
           end if
           call casetrans(abscissa,'lo')
           if(abscissa.eq.'center')abscissa='centre'
           if((abscissa.ne.'start').and.   &
              (abscissa.ne.'centre').and.  &
              (abscissa.ne.'end'))then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,55) trim(aline),trim(sString_g)
55           format('time abscissa must be "start", "centre" or "end" ', &
             'at line ',a,' of file ',a)
             go to 9800
           end if
           write(*,60) trim(abscissa)
           write(LU_REC,60) trim(abscissa)
60         format(t5,'TIME_ABSCISSA ',a)
         else if(aoption.eq.'END')then
           go to 200
         else
           call num2char(ILine_g,aline)
           call addquote(sInfile_g,sString_g)
           write(amessage,90) trim(aoption),trim(CurrentBlock_g),trim(aline),trim(sString_g)
90         format('unexpected keyword - "',a,'" in ',a,' block at line ',a, &
           ' of file ',a)
           go to 9800
         end if
       end do

! -- The block has been read; now it is checked for correctness.

200    continue
       if(ivtable.eq.0)then
         write(amessage,210) trim(CurrentBlock_g)
210      format('no V_TABLE_NAME keyword provided in ',a,' block.')
         go to 9800
       end if
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
       if(abscissa.eq.' ')then
         write(amessage,235) trim(CurrentBlock_g)
235      format('no TIME_ABSCISSA keyword provided in ',a,' block.')
         go to 9800
       end if

! -- The new time series is now written.

       do iser=1,MAXSERIES
         if(.not.series_g(iser)%active) go to 370
       end do
       write(amessage,360)
360    format('no more time series available for data storage - increase MAXSERIES and ', &
       'recompile program.')
       go to 9800

370    continue
       nsterm=vtable_g(ivtable)%nterm
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
       if(abscissa.eq.'start')then
         do j=1,nsterm
           series_g(iser)%days(j)=vtable_g(ivtable)%days1(j)
         end do
         do j=1,nsterm
           series_g(iser)%secs(j)=vtable_g(ivtable)%secs1(j)
         end do
       else if(abscissa.eq.'end')then
         do j=1,nsterm
           series_g(iser)%days(j)=vtable_g(ivtable)%days2(j)
         end do
         do j=1,nsterm
           series_g(iser)%secs(j)=vtable_g(ivtable)%secs2(j)
         end do
       else
         do j=1,nsterm
           idiff=vtable_g(ivtable)%days2(j)-vtable_g(ivtable)%days1(j)
           ihalf=idiff/2
           idays=vtable_g(ivtable)%days1(j)+ihalf
           if(ihalf*2.eq.idiff)then
             isecs=(vtable_g(ivtable)%secs1(j)+vtable_g(ivtable)%secs2(j))/2
           else
             isecs=(vtable_g(ivtable)%secs1(j)+vtable_g(ivtable)%secs2(j))/2+43200
           end if
385        if(isecs.ge.86400)then
             isecs=isecs-86400
             idays=idays+1
             go to 385
           end if
           series_g(iser)%days(j)=idays
           series_g(iser)%secs(j)=isecs
         end do
       end if
       do j=1,nsterm
         series_g(iser)%val(j)=vtable_g(ivtable)%vol(j)
       end do

       write(6,390) trim(aname)
       write(LU_REC,390) trim(aname)
390    format(t5,'Data from v_table stored in series "',a,'".')
       return

9000   call num2char(ILine_g,aline)
       call addquote(sInfile_g,sString_g)
       write(amessage,9010) trim(aline), trim(sString_g)
9010   format('cannot read line ',a,' of TSPROC input file ',a)
       go to 9800
9100   continue
       call addquote(sInfile_g,sString_g)
       write(amessage,9110) trim(sString_g),trim(CurrentBlock_g)
9110   format('unexpected end encountered to TSPROC input file ',a,' while ', &
       ' reading ',a,' block.')
       go to 9800

9800   call write_message(leadspace='yes',error='yes')
       call write_message(iunit=LU_REC,leadspace='yes')
       ifail=1

       return

end subroutine vol_to_series

!     Last change:  JD    4 Sep 2001    7:08 pm
subroutine series_clean(ifail)

! -- Subroutine CLEAN_SERIES removes or replaces terms between two user-supplied
!    thresholds.

       use tsp_data_structures
       use tsp_utilities
       use tsp_command_processors

       implicit none

       integer, intent(out)   :: ifail

       integer icontext,iseries,ixcon,ierr,idelete,ilthresh,iuthresh,itemp,iterm,i,k,j, &
       isub
       real lthresh,uthresh,svalue,rtemp
       character (len=iTSNAMELENGTH) :: aname,atemp
       character*15 aline
       character*25 aoption
       character*25 acontext(MAXCONTEXT)

       ifail=0
       CurrentBlock_g='SERIES_CLEAN'

       write(*,10) trim(CurrentBlock_g)
       write(LU_REC,10) trim(CurrentBlock_g)
10     format(/,' Processing ',a,' block....')

       icontext=0
       iseries=0
       aname=' '
       ixcon=0
       lthresh=1.1e37
       uthresh=-1.1e37
       ilthresh=0
       iuthresh=0
       idelete=0
       isub=0


! -- The CLEAN_SERIES block is first parsed.

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
         if(aoption.eq.'CONTEXT')then
           if(ixcon.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,41) trim(aline),trim(sString_g)
41           format('CONTEXT keyword in incorrect location at line ',a,' of file ',a)
             go to 9800
           end if
           call get_context(ierr,icontext,acontext)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'LOWER_ERASE_BOUNDARY')then
           call get_keyword_value(ierr,2,itemp,lthresh,aoption)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'UPPER_ERASE_BOUNDARY')then
           call get_keyword_value(ierr,2,itemp,uthresh,aoption)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'SUBSTITUTE_VALUE')then
           isub=1
           call get_keyword_value(ierr,2,itemp,svalue,'SUBSTITUTE_VALUE')
           if(ierr.ne.0) then
             call casetrans(cline(left_word(2):right_word(2)),'lo')
             call getfile(ierr,cline,atemp,left_word(2),right_word(2))
             if(atemp.eq.'delete')then
               idelete=1
               write(*,44) trim(aoption)
               write(LU_REC,44) trim(aoption)
44             format(t5,a,1x,'delete')
             else
               call num2char(ILine_g,aline)
               call addquote(sInfile_g,sString_g)
               write(amessage,50) trim(aline),trim(sString_g)
50             format('a real number or "delete" must be supplied with the ',  &
               'SUBSTITUTE_VALUE keyword at line ',a,' of file ',a)
               go to 9800
             end if
           end if
         else if(aoption.eq.'NEW_SERIES_NAME')then
           call get_new_series_name(ierr,aname)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'SERIES_NAME')then
           call get_series_name(ierr,iseries,'SERIES_NAME')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'END')then
           go to 200
         else
           call num2char(ILine_g,aline)
           call addquote(sInfile_g,sString_g)
           write(amessage,90) trim(aoption),trim(CurrentBlock_g),trim(aline),trim(sString_g)
90         format('unexpected keyword - "',a,'" in ',a,' block at line ',a, &
           ' of file ',a)
           go to 9800
         end if
       end do

! -- The block has been read; now it is checked for correctness.

200    continue
       if(iseries.eq.0)then
         write(amessage,210) trim(CurrentBlock_g)
210      format('no SERIES_NAME keyword provided in ',a,' block.')
         go to 9800
       end if
       if(isub.eq.0)then
         write(amessage,211) trim(CurrentBlock_g)
211      format('no SUBSTITUTE_VALUE keyword supplied in the ',a,' block.')
         go to 9800
       end if
       if((aname.eq.' ').and.(idelete.eq.1))then
         write(amessage,230) trim(CurrentBlock_g)
230      format('if SUBSTITUTE_VALUE is supplied as "delete" then a ', &
         'NEW_SERIES_NAME must be supplied in the ',a,' block.')
         go to 9800
       end if
       if(icontext.eq.0)then
         write(amessage,220) trim(CurrentBlock_g)
220      format('no CONTEXT keyword(s) provided in ',a,' block.')
         go to 9800
       end if
       if((lthresh.gt.1.0e37).and.(uthresh.lt.-1.0e37))then
         write(amessage,225) trim(CurrentBlock_g)
225      format('neither an UPPER_ERASE_BOUNDARY nor a LOWER_ERASE_BOUNDARY ', &
         'has been supplied in the ',a,' block.')
         go to 9800
       else
         if(lthresh.lt.1.0e37)ilthresh=1
         if(uthresh.gt.-1.0e37)iuthresh=1
       end if
       if((ilthresh.eq.1).and.(iuthresh.eq.1))then
         if(lthresh.ge.uthresh)then
           write(amessage,235)
235        format('the upper erase boundary must be greater than the lower ', &
           'erase boundary.')
           go to 9800
         end if
       end if

! -- The new series is now written. But first the number of terms in the new series
!    is counted.

       if(idelete.eq.0)then
         iterm=series_g(iseries)%nterm
       else
         iterm=0
         if((ilthresh.eq.1).and.(iuthresh.eq.1))then
           do j=1,series_g(iseries)%nterm
             if((series_g(iseries)%val(j).lt.lthresh).or.      &
                (series_g(iseries)%val(j).gt.uthresh)) iterm=iterm+1
           end do
         else if(ilthresh.eq.1)then
           do j=1,series_g(iseries)%nterm
             if(series_g(iseries)%val(j).lt.lthresh) iterm=iterm+1
           end do
         else
           do j=1,series_g(iseries)%nterm
             if(series_g(iseries)%val(j).gt.uthresh) iterm=iterm+1
           end do
         end if
         if(iterm.eq.0)then
           write(amessage,240) trim(series_g(iseries)%name), trim(aname)
240        format('all terms in series "',a,'" have been erased, so the new ', &
           'series "',a,'" has no terms.')
           go to 9800
         end if
       end if

! -- If a new time series is warranted, then space is allocated for it.

       if(aname.ne.' ')then
         do i=1,MAXSERIES
           if(.not.series_g(i)%active) go to 515
         end do
         write(amessage,510)
510      format('no more time series available for data storage - increase MAXSERIES and ', &
         'recompile program.')
         go to 9800

515      allocate(series_g(i)%days(iterm),series_g(i)%secs(iterm),  &
         series_g(i)%val(iterm),stat=ierr)
         if(ierr.ne.0)then
           write(amessage,550)
550        format('cannot allocate memory for another time series.')
           go to 9800
         end if
         series_g(i)%active=.true.
         series_g(i)%name=aname
         series_g(i)%nterm=iterm
         series_g(i)%type='ts'
         k=0
         do j=1,series_g(iseries)%nterm
           if((ilthresh.eq.1).and.(iuthresh.eq.1))then
             if((series_g(iseries)%val(j).lt.lthresh).or.   &
                (series_g(iseries)%val(j).gt.uthresh)) go to 570
           else if(ilthresh.eq.1)then
             if(series_g(iseries)%val(j).lt.lthresh) go to 570
           else
             if(series_g(iseries)%val(j).gt.uthresh) go to 570
           end if
           if(idelete.eq.1) go to 580
           rtemp=svalue
           go to 575
570        continue
           rtemp=series_g(iseries)%val(j)
575        continue
           k=k+1
           series_g(i)%days(k)=series_g(iseries)%days(j)
           series_g(i)%secs(k)=series_g(iseries)%secs(j)
           series_g(i)%val(k)=rtemp
580        continue
         end do
       else
         do j=1,series_g(iseries)%nterm
           if((ilthresh.eq.1).and.(iuthresh.eq.1))then
             if((series_g(iseries)%val(j).ge.lthresh).and.  &
                (series_g(iseries)%val(j).le.uthresh)) &
                series_g(iseries)%val(j)=svalue
           else if(ilthresh.eq.1)then
             if(series_g(iseries)%val(j).ge.lthresh)series_g(iseries)%val(j)=svalue
           else
             if(series_g(iseries)%val(j).le.uthresh)series_g(iseries)%val(j)=svalue
           end if
         end do
       end if

       if(aname.ne.' ')then
         write(*,590) trim(aname)
         write(LU_REC,590) trim(aname)
590      format(t5,'Series "',a,'" successfully calculated.')
       else
         write(*,600) trim(series_g(iseries)%name)
         write(LU_REC,600) trim(series_g(iseries)%name)
600      format(t5,'Series "',a,'" successfully cleaned.')
       end if
       return

9000   call num2char(ILine_g,aline)
       call addquote(sInfile_g,sString_g)
       write(amessage,9010) trim(aline), trim(sString_g)
9010   format('cannot read line ',a,' of TSPROC input file ',a)
       go to 9800
9100   continue
       call addquote(sInfile_g,sString_g)
       write(amessage,9110) trim(sString_g), trim(CurrentBlock_g)
9110   format('unexpected end encountered to TSPROC input file ',a,' while ', &
       ' reading ',a,' block.')
       go to 9800

9800   call write_message(leadspace='yes',error='yes')
       call write_message(iunit=LU_REC,leadspace='yes')
       ifail=1

       return

end subroutine series_clean

subroutine bfilter(ifail)

! -- Subroutine BFILTER calculates Butterworth filter coefficients and
!    carries out low, high or band pass filtering operations. It also
!    carries out "baseflow filtering" as described in Nathan and McMahon (1990).

       use tsp_data_structures
       use tsp_utilities
       use tsp_command_processors

       implicit none

       integer, intent(out)   :: ifail

       integer ierr,icontext,iseries,itemp,nsterm,ilags,j,nsecs,ndays, &
       dd,mm,yy,hh,nn,ss,i,ixcon,ns,k,jclipzero,jclipinput,jfreq1,jfreq2, &
       jfreq,jns,jfilpass,jpass,jalpha,ipass,ip
       integer jrevstage2,jj
       real rtemp,freq,freq1,freq2,af,bf,cf,df,ef,tdelt,alpha,alpha1,fk_1, &
       yk1,yk,yk_1
       real a(3),b(3),c(3),d(3),e(3),rval(-3:5),gval(-3:5)
       character*3 aaa
       character (len=iTSNAMELENGTH) :: aname,filpass
       character*20 aline,filtype
       character*25 aoption
       character*25 acontext(MAXCONTEXT)

       ifail=0
       CurrentBlock_g='DIGITAL_FILTER'

       write(*,10) trim(CurrentBlock_g)
       write(LU_REC,10) trim(CurrentBlock_g)
10     format(/,' Processing ',a,' block....')

       icontext=0
       iseries=0
       aname=' '
       ixcon=0
       filtype=' '
       filpass=' '
       jfilpass=0
       ns=1
       jns=0
       freq=-1.0e35
       freq1=-1.0e35
       freq2=-1.0e35
       jfreq=0
       jfreq1=0
       jfreq2=0
       jclipzero=0
       jclipinput=0
       jalpha=0
       jpass=0
       jrevstage2=0

! -- The DIGITAL_FILTER block is first parsed.

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
         else if(aoption.eq.'SERIES_NAME')then
           call get_series_name(ierr,iseries,'SERIES_NAME')
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
         else if(aoption.eq.'FILTER_TYPE')then
           call getfile(ierr,cline,filtype,left_word(2),right_word(2))
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,49) trim(aline),trim(sString_g)
49           format('cannot read filter type from line ',a,' of file ',a)
             go to 9800
           end if
           call casetrans(filtype,'lo')
           if((filtype.ne.'butterworth').and.   &
              (filtype.ne.'baseflow_separation'))then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,55) trim(aline),trim(sString_g)
55           format('filter type must be "butterworth" or "baseflow_separation" ', &
             'at line ',a,' of file ',a)
             go to 9800
           end if
           write(*,60) trim(filtype)
           write(LU_REC,60) trim(filtype)
60         format(t5,'FILTER_TYPE ',a)
         else if(aoption.eq.'FILTER_PASS')then
           call getfile(ierr,cline,filpass,left_word(2),right_word(2))
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,50) trim(aline),trim(sString_g)
50           format('cannot read filter pass band type from line ',a,' of file ',a)
             go to 9800
           end if
           jfilpass=1
           call casetrans(filpass,'lo')
           if((filpass.ne.'high').and.(filpass.ne.'low').and.  &
              (filpass.ne.'band'))then
              call num2char(ILine_g,aline)
              call addquote(sInfile_g,sString_g)
              write(amessage,53) trim(aline),trim(sString_g)
53            format('filter pass band type must be "high", "low" or "band" at line ',  &
              a,' of file ',a)
              go to 9800
           end if
           write(*,54) trim(filpass)
           write(LU_REC,54) trim(filpass)
54         format(t5,'FILTER_PASS ',a)
         else if(aoption.eq.'STAGES')then
           call get_keyword_value(ierr,1,ns,rtemp,'STAGES')
           if(ierr.ne.0) go to 9800
           jns=1
         else if(aoption.eq.'CUTOFF_FREQUENCY')then
           call get_keyword_value(ierr,2,itemp,freq,'CUTOFF_FREQUENCY')
           if(ierr.ne.0) go to 9800
           if(freq.le.0.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,65) trim(aline),trim(sString_g)
65           format('frequency must be positive at line ',a,' of file ',a)
             go to 9800
           end if
           jfreq=1
         else if(aoption.eq.'CUTOFF_FREQUENCY_1')then
           call get_keyword_value(ierr,2,itemp,freq1,'CUTOFF_FREQUENCY_1')
           if(ierr.ne.0) go to 9800
           if(freq1.le.0.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,65) trim(aline),trim(sString_g)
             go to 9800
           end if
           jfreq1=1
         else if(aoption.eq.'CUTOFF_FREQUENCY_2')then
           call get_keyword_value(ierr,2,itemp,freq2,'CUTOFF_FREQUENCY_2')
           if(ierr.ne.0) go to 9800
           if(freq2.le.0.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,65) trim(aline),trim(sString_g)
             go to 9800
           end if
           jfreq2=1
         else if(aoption.eq.'CLIP_ZERO')then
           call get_yes_no(ierr,jclipzero)
           if(ierr.ne.0) go to 9800
           if(jclipzero.eq.1)then
             aaa='yes'
           else
             aaa='no'
           end if
           write(*,70) trim(aaa)
           write(LU_REC,70) trim(aaa)
70         format(t5,'CLIP_ZERO ',a)
         else if(aoption.eq.'CLIP_INPUT')then
           call get_yes_no(ierr,jclipinput)
           if(ierr.ne.0) go to 9800
           if(jclipinput.eq.1)then
             aaa='yes'
           else
             aaa='no'
           end if
           write(*,71) trim(aaa)
           write(LU_REC,71) trim(aaa)
71         format(t5,'CLIP_INPUT ',a)
         else if(aoption.eq.'ALPHA')then
           call get_keyword_value(ierr,2,itemp,alpha,'ALPHA')
           if(ierr.ne.0) go to 9800
           if(alpha.le.0.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,81) trim(aline),trim(sString_g)
81           format('alpha must be positive at line ',a,' of file ',a)
             go to 9800
           end if
           jalpha=1
         else if(aoption.eq.'REVERSE_SECOND_STAGE')then
           call get_yes_no(ierr,jrevstage2)
           if(ierr.ne.0) go to 9800
           if(jrevstage2.eq.1)then
             aaa='yes'
           else
             aaa='no'
           end if
           write(*,72) trim(aaa)
           write(LU_REC,72) trim(aaa)
72         format(t5,'REVERSE_SECOND_STAGE ',a)
         else if(aoption.eq.'PASSES')then
           call get_keyword_value(ierr,1,ipass,rtemp,'PASSES')
           if(ierr.ne.0) go to 9800
           if((ipass.ne.1).and.(ipass.ne.3))then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,82) trim(aline),trim(sString_g)
82           format('number of passes must be 1 or 3 at line ',a,' of file ',a)
             go to 9800
           end if
           jpass=1
         else if(aoption.eq.'END')then
           go to 200
         else
           call num2char(ILine_g,aline)
           call addquote(sInfile_g,sString_g)
           write(amessage,90) trim(aoption),trim(CurrentBlock_g),trim(aline),trim(sString_g)
90         format('unexpected keyword - "',a,'" in ',a,' block at line ',a, &
           ' of file ',a)
           go to 9800
         end if
       end do

! -- The block has been read; now it is checked for correctness.

200    continue
       if(iseries.eq.0)then
         write(amessage,210) trim(CurrentBlock_g)
210      format('no SERIES_NAME keyword provided in ',a,' block.')
         go to 9800
       end if
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
       if(filtype.eq.' ')then
         write(amessage,222) trim(CurrentBlock_g)
222      format('no FILTER_TYPE keyword has been provided in ',a,' block.')
         go to 9800
       end if
       if(filtype(1:4).eq.'base')then
         if(jns.eq.1) go to 9300
         if(jfilpass.eq.1) go to 9300
         if((jfreq.eq.1).or.(jfreq1.eq.1).or.(jfreq2.eq.1)) go to 9300
       else if(filtype(1:3).eq.'but')then
         if(jpass.eq.1) go to 9350
         if(jalpha.eq.1) go to 9350
         if((jclipzero.eq.1).or.(jclipinput.eq.1)) go to 9370
       end if
       if(filtype(1:3).eq.'but')then
         if((ns.lt.1).or.(ns.gt.3))then
           write(amessage,245) trim(CurrentBlock_g)
245        format('number of filter stages must be 1, 2, or 3 in ',a,' block.')
           go to 9800
         end if
         if((filpass.eq.'low').or.(filpass.eq.'high'))then
           if((freq1.gt.0.0).or.(freq2.gt.0.0))then
             write(amessage,250) trim(CurrentBlock_g)
250          format('values for CUTOFF_FREQUENCY_1 and CUTOFF_FREQUENCY_2 ',  &
             'should be supplied only for band pass filter in ',a,' block.')
             go to 9800
           end if
           if(freq.lt.0.0)then
             write(amessage,255) trim(CurrentBlock_g)
255          format('no value supplied for CUTOFF_FREQUENCY in ',a,' block.')
             go to 9800
           end if
         else if(filpass.eq.'band')then
           if((freq1.lt.0.0).or.(freq2.lt.0.0))then
             write(amessage,256) trim(CurrentBlock_g)
256          format('values for both CUTOFF_FREQUENCY_1 and CUTOFF_FREQUENCY_2 ',  &
             'should be supplied for band pass filter in ',a,' block.')
             go to 9800
           end if
           if(freq.gt.0.0)then
             write(amessage,257) trim(CurrentBlock_g)
257          format('no value should be supplied for CUTOFF_FREQUENCY for ', &
             'band pass filter in ',a,' block - only for CUTOFF_FREQUENCY_1 ',  &
             'and CUTOFF_FREQUENCY_2.')
             go to 9800
           end if
           if(freq2.le.freq1)then
             write(amessage,258) trim(CurrentBlock_g)
258          format('CUTOFF_FREQUENCY_2 should be greater than CUTOFF_FREQUENCY_1 ', &
             'in ',a,' block.')
             go to 9800
           end if
         else if(filpass.eq.' ')then
           write(amessage,259) trim(CurrentBlock_g)
259        format('no FILTER_PASS keyword provided in ',a,' block.')
           go to 9800
         end if
       else if(filtype(1:4).eq.'base')then
         if(jpass.eq.0)ipass=1
         if(jalpha.eq.0)then
           write(amessage,270) trim(CurrentBlock_g)
270        format('no ALPHA keyword provided in ',a,' block.')
           go to 9800
         end if
       end if
       if(jrevstage2.eq.1)then
         if((filtype(1:3).ne.'but').or.(filpass.ne.'low').or.(ns.ne.2))then
           write(amessage,299)
299        format('REVERSE_SECOND_STAGE can only be set to "yes" when (a) butterworth ', &
           'filter is chosen, (b) number or stages is 2 and (c) the FILTER_PASS is "low".')
           go to 9800
         end if
       end if

! -- Filtering can only be performed if the input time series has equal
!    increments. This is now tested.

       nsterm=series_g(iseries)%nterm
       if(nsterm.lt.20)then
         call num2char(nsterm,aline)
         write(amessage,300) trim(series_g(iseries)%name),trim(aline)
300      format('series "',a,'" has only ',a,' terms. This is insufficient to perform ', &
         'the requested filtering operation.')
         go to 9800
       end if
       if(nsterm.gt.2)then
         ilags=(series_g(iseries)%days(2)-series_g(iseries)%days(1))*86400+   &
                series_g(iseries)%secs(2)-series_g(iseries)%secs(1)
         do j=2,nsterm-1
           nsecs=series_g(iseries)%secs(j)+ilags
           ndays=series_g(iseries)%days(j)
260        if(nsecs.ge.86400)then
             ndays=ndays+1
             nsecs=nsecs-86400
             go to 260
           end if
           if((nsecs.ne.series_g(iseries)%secs(j+1)).or.   &
              (ndays.ne.series_g(iseries)%days(j+1)))then
               call newdate(series_g(iseries)%days(j),1,1,1970,dd,mm,yy)
               nsecs=series_g(iseries)%secs(j)
               hh=nsecs/3600
               nn=(nsecs-hh*3600)/60
               ss=nsecs-hh*3600-nn*60
               if(datespec.eq.1) then
                 write(amessage,280) trim(series_g(iseries)%name),dd,mm,yy,hh,nn,ss
               else
                 write(amessage,280) trim(series_g(iseries)%name),mm,dd,yy,hh,nn,ss
               end if
280            format('time interval between terms in time series "',a,'" is not ', &
               'constant. The first discrepancy occurs following the sample taken on ',  &
               i2.2,'/',i2.2,'/',i4,' at ',i2.2,':',i2.2,':',i2.2)
               go to 9800
           end if
         end do
       end if

! -- The filter coefficients are now calculated (butterworth filter).

       if(filtype(1:3).eq.'but')then
         tdelt=float(ilags)/86400.00
         if(filpass.eq.'low')then
           if(freq.ge.0.5/tdelt) go to 9200
           call lpdes(freq,tdelt,ns,a,b,c)
         else if(filpass.eq.'high')then
           if(freq.ge.0.5/tdelt) go to 9200
           call hpdes(freq,tdelt,ns,a,b,c)
         else
           if((freq1.ge.0.5/tdelt).or.(freq2.ge.0.5/tdelt)) go to 9200
           call bpdes(freq1,freq2,tdelt,ns,a,b,c,d,e)
         end if
       end if

! -- Space for a new series is allocated.

       do i=1,MAXSERIES
         if(.not.series_g(i)%active) go to 515
       end do
       write(amessage,510)
510    format('no more time series available for data storage - increase MAXSERIES and ', &
       'recompile program, or erase a series using an ERASE_SERIES block.')
       go to 9800

515    continue
       allocate(series_g(i)%days(nsterm),series_g(i)%secs(nsterm),  &
       series_g(i)%val(nsterm),stat=ierr)
       if(ierr.ne.0)then
         write(amessage,550)
550      format('cannot allocate memory for another time series.')
         go to 9800
       end if
       series_g(i)%active=.true.
       series_g(i)%name=aname
       series_g(i)%nterm=nsterm
       series_g(i)%type='ts'
       do j=1,nsterm
         series_g(i)%days(j)=series_g(iseries)%days(j)
       end do
       do j=1,nsterm
         series_g(i)%secs(j)=series_g(iseries)%secs(j)
       end do

! -- Now the butterworth filtering is carried out. But first a temporary time series is
!    allocated if needed.

       if(filtype(1:4).eq.'base') go to 700
       if(ns.gt.1)then
         call alloc_tempseries(ierr,nsterm)
         if(ierr.ne.0) go to 9800
       end if

       do k=1,ns
         af=a(k)
         bf=b(k)
         cf=c(k)
         if((k.eq.2).and.(jrevstage2.eq.1))then
           af=a(1)
           bf=b(1)
           cf=c(1)
         end if
         if(filpass.eq.'band')then
           df=d(k)
           ef=e(k)
         end if
         if(k.eq.1)then
           do j=1,5
             rval(j)=series_g(iseries)%val(j)
           end do
         else
           do j=1,5
             rval(j)=tempseries_g%val(j)
           end do
         end if
!         rval(0)=rval(1)-(rval(2)-rval(1))
!         rval(-1)=rval(0)-(rval(1)-rval(0))
!         rval(-2)=rval(-1)-(rval(0)-rval(-1))
!         rval(-3)=rval(-2)-(rval(-1)-rval(-2))
!         gval(-3)=rval(-3)
!         gval(-2)=rval(-2)
!         gval(-1)=rval(-1)
!         gval(0)=rval(0)
         rval(0)=rval(1)
         rval(-1)=rval(0)
         rval(-2)=rval(-1)
         rval(-3)=rval(-2)
         if(filpass.eq.'low')then
           gval(-3)=rval(-3)
           gval(-2)=rval(-2)
           gval(-1)=rval(-1)
           gval(0)=rval(0)
         else
           gval(-3)=0.0
           gval(-2)=0.0
           gval(-1)=0.0
           gval(0)=0.0
         end if
         if(filpass.eq.'low')then
           do j=1,5
             gval(j)=af*(rval(j)+2.0*rval(j-1)+rval(j-2))-  &
             bf*gval(j-1)-cf*gval(j-2)
           end do
         else if(filpass.eq.'high')then
           do j=1,5
             gval(j)=af*(rval(j)-2.0*rval(j-1)+rval(j-2))-  &
             bf*gval(j-1)-cf*gval(j-2)
           end do
         else
           do j=1,5
             gval(j)=af*(rval(j)-2.0*rval(j-2)+rval(j-4))-bf*gval(j-1)-  &
             cf*gval(j-2)-df*gval(j-3)-ef*gval(j-4)
           end do
         end if
         do j=1,5
           series_g(i)%val(j)=gval(j)
         end do
         if(filpass.eq.'low')then
           if(k.ne.1)then
             do j=6,nsterm
               series_g(i)%val(j)=af*(tempseries_g%val(j)+    &
               2.0*tempseries_g%val(j-1)+tempseries_g%val(j-2))-  &
               bf*series_g(i)%val(j-1)-cf*series_g(i)%val(j-2)
             end do
           else
             do j=6,nsterm
               series_g(i)%val(j)=af*(series_g(iseries)%val(j)+    &
               2.0*series_g(iseries)%val(j-1)+series_g(iseries)%val(j-2))-  &
               bf*series_g(i)%val(j-1)-cf*series_g(i)%val(j-2)
             end do
           end if
         else if(filpass.eq.'high')then
           if(k.ne.1)then
             do j=6,nsterm
               series_g(i)%val(j)=af*(tempseries_g%val(j)-     &
               2.0*tempseries_g%val(j-1)+tempseries_g%val(j-2))-  &
               bf*series_g(i)%val(j-1)-cf*series_g(i)%val(j-2)
             end do
           else
             do j=6,nsterm
               series_g(i)%val(j)=af*(series_g(iseries)%val(j)-     &
               2.0*series_g(iseries)%val(j-1)+series_g(iseries)%val(j-2))-  &
               bf*series_g(i)%val(j-1)-cf*series_g(i)%val(j-2)
             end do
           end if
         else
           if(k.ne.1)then
             do j=6,nsterm
               series_g(i)%val(j)=af*(tempseries_g%val(j)-    &
               2.0*tempseries_g%val(j-2)+tempseries_g%val(j-4))-  &
               bf*series_g(i)%val(j-1)-cf*series_g(i)%val(j-2)-    &
               df*series_g(i)%val(j-3)-ef*series_g(i)%val(j-4)
               if(abs(series_g(i)%val(j)).gt.1.0e30) go to 9400
             end do
           else
             do j=6,nsterm
               series_g(i)%val(j)=af*(series_g(iseries)%val(j)-    &
               2.0*series_g(iseries)%val(j-2)+series_g(iseries)%val(j-4))-  &
               bf*series_g(i)%val(j-1)-cf*series_g(i)%val(j-2)-    &
               df*series_g(i)%val(j-3)-ef*series_g(i)%val(j-4)
               if(abs(series_g(i)%val(j)).gt.1.0e30) go to 9400
             end do
           end if
         end if
         if(k.ne.ns)then
           if((k.eq.1).and.(jrevstage2.eq.1))then
             do j=1,nsterm
               tempseries_g%val(j)=series_g(i)%val(nsterm-j+1)
             end do
           else
             do j=1,nsterm
               tempseries_g%val(j)=series_g(i)%val(j)
             end do
           end if
         else
           if(jrevstage2.eq.1)then
             do j=1,nsterm/2
               jj=nsterm-j+1
               rtemp=series_g(i)%val(j)
               series_g(i)%val(j)=series_g(i)%val(jj)
               series_g(i)%val(jj)=rtemp
             end do
           end if
         end if
       end do
       go to 900

! -- Baseflow separation filtering is carried out.

700    continue

       call alloc_tempseries(ierr,nsterm)
       if(ierr.ne.0) go to 9800

       alpha1=(1.0+alpha)*0.5
       do ip=1,ipass
         if(ip.eq.1)then
           do j=1,nsterm
             tempseries_g%val(j)=series_g(iseries)%val(j)
           end do
         else if((ip.eq.2).or.(ip.eq.3))then
           do j=1,nsterm
             tempseries_g%val(j)=series_g(i)%val(nsterm+1-j)
           end do
         end if
         yk=tempseries_g%val(1)
         yk1=tempseries_g%val(2)
         yk_1=yk-(yk1-yk)
         fk_1=yk_1
         series_g(i)%val(1)=alpha*fk_1+alpha1*(yk-yk_1)
         do j=2,nsterm
           series_g(i)%val(j)=alpha*series_g(i)%val(j-1) + alpha1*  &
           (tempseries_g%val(j)-tempseries_g%val(j-1))
         end do
       end do

! -- The following applies to both types of filtering.

900    continue
       if(jclipzero.ne.0)then
         do j=1,nsterm
           if(series_g(i)%val(j).lt.0.0)series_g(i)%val(j)=0.0
         end do
       end if
       if(jclipinput.ne.0)then
         do j=1,nsterm
           if(series_g(i)%val(j).gt.series_g(iseries)%val(j))  &
              series_g(i)%val(j) =  series_g(iseries)%val(j)
         end do
       end if

       write(*,580) trim(aname)
       write(LU_REC,580) trim(aname)
580    format(t5,'Series "',a,'" successfully calculated.')
       return

9000   call num2char(ILine_g,aline)
       call addquote(sInfile_g,sString_g)
       write(amessage,9010) trim(aline), trim(sString_g)
9010   format('cannot read line ',a,' of TSPROC input file ',a)
       go to 9800
9100   continue
       call addquote(sInfile_g,sString_g)
       write(amessage,9110) trim(sString_g),trim(CurrentBlock_g)
9110   format('unexpected end encountered to TSPROC input file ',a,' while ', &
       ' reading ',a,' block.')
       go to 9800
9200   write(amessage,9210) trim(CurrentBlock_g)
9210   format('filter cutoff frequency must be less than half of the ',  &
       'series sampling frequency in ',a,' block.')
       go to 9800
9300   write(amessage,9310)
9310   format('if FILTER_TYPE is "baseflow_separation" then none of the STAGES ',   &
       'CUTOFF_FREQUENCY, CUTOFF_FREQUENCY_1, CUTOFF_FREQUENCY_2 ', &
       'or FILTER_PASS keywords must be provided.')
       go to 9800
9350   write(amessage,9360)
9360   format('if FILTER_TYPE is "butterworth" then neither of the PASSES, ', &
       'ALPHA, CLIP_ZERO nor CLIP_INPUT keywords must be provided.')
       go to 9800
9370   write(amessage,9380)
9380   format('if FILTER_TYPE is "butterworth" then CLIP_ZERO and CLIP_INPUT ',  &
       'should be omitted or set to "no".')
       go to 9800
9400   write(amessage,9410)
9410   format('bandpass filter is numerically unstable - consider using ', &
       'wider pass band.')
       go to 9800

9800   call write_message(leadspace='yes',error='yes')
       call write_message(iunit=LU_REC,leadspace='yes')
       ifail=1

       return

end subroutine bfilter


subroutine lpdes(fc,t,ns,a,b,c)

! -- Subroutine LPDES evaluates the coefficients for a low pass filter.

       implicit none
       integer, intent(in) :: ns
       real, intent(in)    :: fc,t
       real, intent(out)   :: a(ns),b(ns),c(ns)

       integer k
       real pi,wcp,cs,x

       pi=3.1415926536
       wcp=sin(fc*pi*t)/cos(fc*pi*t)
       do 120 k=1,ns
         cs=cos(float(2*(k+ns)-1)*pi/float(4*ns))
         x=1.0/(1.0+wcp*wcp-2.0*wcp*cs)
         a(k)=wcp*wcp*x
         b(k)=2.0*(wcp*wcp-1.0)*x
         c(k)=(1.0+wcp*wcp+2.0*wcp*cs)*x
120    continue
       return

end subroutine lpdes


subroutine hpdes(fc,t,ns,a,b,c)

! -- Subroutine HPDES evaluates the coefficients for a high pass filter.

       implicit none
       integer, intent(in) :: ns
       real, intent(in)    :: fc,t
       real, intent(out)   :: a(ns),b(ns),c(ns)

       integer k
       real pi,wcp,cs

       pi=3.1415926536
       wcp=sin(fc*pi*t)/cos(fc*pi*t)
       do 120 k=1,ns
         cs=cos(float(2*(k+ns)-1)*pi/float(4*ns))
         a(k)=1.0/(1.0+wcp*wcp-2.0*wcp*cs)
         b(k)=2.0*(wcp*wcp-1.0)*a(k)
         c(k)=(1.0+wcp*wcp+2.0*wcp*cs)*a(k)
120    continue
       return

end subroutine hpdes


subroutine bpdes(f1,f2,t,ns,a,b,c,d,e)

! -- Subroutine BPDES evaluates the coefficients for a band pass filter.

       implicit none
       integer, intent(in) :: ns
       real, intent(in)    :: f1,f2,t
       real, intent(out)   :: a(ns),b(ns),c(ns),d(ns),e(ns)

       integer k
       real pi,w1,w2,wc,q,s,cs,p,r,x

       pi=3.1415926536
       w1=sin(f1*pi*t)/cos(f1*pi*t)
       w2=sin(f2*pi*t)/cos(f2*pi*t)
       wc=w2-w1
       q=wc*wc+2.0*w1*w2
       s=w1*w1*w2*w2
       do k=1,ns
         cs=cos(float(2*(k+ns)-1)*pi/float(4*ns))
         p=-2.0*wc*cs
         r=p*w1*w2
         x=1.0+p+q+r+s
         a(k)=wc*wc/x
         b(k)=(-4.0-2.0*p+2.0*r+4.0*s)/x
         c(k)=(6.0-2.0*q+6.0*s)/x
         d(k)=(-4.0+2.0*p-2.0*r+4.0*s)/x
         e(k)=(1.0-p+q-r+s)/x
       end do

       return

end subroutine bpdes




subroutine compare_series(ifail)

! -- Subroutine COMPARE_SERIES calculates comparison statistics between time series.

       use tsp_data_structures
       use tsp_utilities
       use tsp_command_processors

       implicit none

       integer, intent(out)   :: ifail

       integer isseries,ioseries,jbias,jse,jrbias,jrse,jns,jce,jia,  &
       ibseries,ibbterm,ibeterm,ibterm,exponent,l
       integer dd1,mm1,yy1,hh1,nn1,ss1,dd2,mm2,yy2,hh2,nn2,ss2,ierr, &
       icontext,i,begdays,begsecs,enddays,endsecs, &
       j,isbterm,iobterm,iseterm,ioeterm,iiterm,ixcon,isterm,ioterm,k
       real rtemp,rtemp1,tsum1,tsum2,tsum3,tsum4
       character*3 aaa
       character (len=iTSNAMELENGTH) :: aname
       character*15 aline
       character*25 aoption
       character*25 acontext(MAXCONTEXT)

       ifail=0
       CurrentBlock_g='SERIES_COMPARE'

       write(*,10) trim(CurrentBlock_g)
       write(LU_REC,10) trim(CurrentBlock_g)
10     format(/,' Processing ',a,' block....')

       icontext=0
       isseries=0
       ioseries=0
       ibseries=0
       jbias=0
       jse=0
       jrbias=0
       jrse=0
       jns=0
       jce=0
       jia=0
       exponent=-9999
       yy1=-9999
       hh1=-9999
       yy2=-9999
       hh2=-9999
       aname=' '
       ixcon=0

! -- The COMPARE_SERIES block is first parsed.

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
         if(aoption.eq.'NEW_C_TABLE_NAME')then
           call get_new_table_name(ierr,4,aname)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'DATE_1')then
           call get_date(ierr,dd1,mm1,yy1,'DATE_1')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'DATE_2')then
           call get_date(ierr,dd2,mm2,yy2,'DATE_2')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'TIME_1')then
           call get_time(ierr,hh1,nn1,ss1,'TIME_1')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'TIME_2')then
           call get_time(ierr,hh2,nn2,ss2,'TIME_2')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'SERIES_NAME_SIM')then
           if(isseries.ne.0)then
             write(amessage,26)
26           format('more than one SERIES_NAME_SIM entry in SERIES_COMPARE block.')
             go to 9800
           end if
           call get_series_name(ierr,isseries,'SERIES_NAME_SIM')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'SERIES_NAME_OBS')then
           if(ioseries.ne.0)then
             write(amessage,27)
27           format('more than one SERIES_NAME_OBS entry in SERIES_COMPARE block.')
             go to 9800
           end if
           call get_series_name(ierr,ioseries,'SERIES_NAME_OBS')
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
         else if(aoption.eq.'BIAS')then
           call get_yes_no(ierr,jbias)
           if(ierr.ne.0) go to 9800
           if(jbias.eq.1)then
             aaa='yes'
           else
             aaa='no'
           end if
           write(*,128) trim(aaa)
           write(LU_REC,128) trim(aaa)
128        format(t5,'BIAS ',a)
         else if(aoption.eq.'STANDARD_ERROR')then
           call get_yes_no(ierr,jse)
           if(ierr.ne.0) go to 9800
           if(jse.eq.1)then
             aaa='yes'
           else
             aaa='no'
           end if
           write(*,129) trim(aaa)
           write(LU_REC,129) trim(aaa)
129        format(t5,'STANDARD_ERROR ',a)
         else if(aoption.eq.'RELATIVE_BIAS')then
           call get_yes_no(ierr,jrbias)
           if(ierr.ne.0) go to 9800
           if(jrbias.eq.1)then
             aaa='yes'
           else
             aaa='no'
           end if
           write(*,132) trim(aaa)
           write(LU_REC,132) trim(aaa)
132        format(t5,'RELATIVE_BIAS ',a)
         else if(aoption.eq.'RELATIVE_STANDARD_ERROR')then
           call get_yes_no(ierr,jrse)
           if(ierr.ne.0) go to 9800
           if(jrse.eq.1)then
             aaa='yes'
           else
             aaa='no'
           end if
           write(*,133) trim(aaa)
           write(LU_REC,133) trim(aaa)
133        format(t5,'RELATIVE_STANDARD_ERROR ',a)
         else if(aoption.eq.'NASH_SUTCLIFFE')then
           call get_yes_no(ierr,jns)
           if(ierr.ne.0) go to 9800
           if(jns.eq.1)then
             aaa='yes'
           else
             aaa='no'
           end if
           write(*,134) trim(aaa)
           write(LU_REC,134) trim(aaa)
134        format(t5,'NASH_SUTCLIFFE ',a)
         else if(aoption.eq.'COEFFICIENT_OF_EFFICIENCY')then
           call get_yes_no(ierr,jce)
           if(ierr.ne.0) go to 9800
           if(jce.eq.1)then
             aaa='yes'
           else
             aaa='no'
           end if
           write(*,135) trim(aaa)
           write(LU_REC,135) trim(aaa)
135        format(t5,'COEFFICIENT_OF_EFFICIENCY ',a)
         else if(aoption.eq.'INDEX_OF_AGREEMENT')then
           call get_yes_no(ierr,jia)
           if(ierr.ne.0) go to 9800
           if(jia.eq.1)then
             aaa='yes'
           else
             aaa='no'
           end if
           write(*,136) trim(aaa)
           write(LU_REC,136) trim(aaa)
136        format(t5,'INDEX_OF_AGREEMENT ',a)
         else if(aoption.eq.'EXPONENT')then
           call get_keyword_value(ierr,1,exponent,rtemp,'EXPONENT')
           if(ierr.ne.0) go to 9800
           if((exponent.ne.1).and.(exponent.ne.2))then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,138) trim(aline),trim(sString_g)
138          format('exponent must be 1 or 2 at line ',a,  &
             ' of file ',a)
             go to 9800
           end if
         else if(aoption.eq.'SERIES_NAME_BASE')then
           call get_series_name(ierr,ibseries,'SERIES_NAME_BASE')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'END')then
           go to 200
         else
           call num2char(ILine_g,aline)
           call addquote(sInfile_g,sString_g)
           write(amessage,90) trim(aoption),trim(CurrentBlock_g),trim(aline),trim(sString_g)
90         format('unexpected keyword - "',a,'" in ',a,' block at line ',a, &
           ' of file ',a)
           go to 9800
         end if
       end do

! -- The block has been read; now it is checked for correctness.

200    continue
       if(isseries.eq.0)then
         write(amessage,210) trim(CurrentBlock_g)
210      format('no SERIES_NAME_SIM keyword provided in ',a,' block.')
         go to 9800
       end if
       if(ioseries.eq.0)then
         write(amessage,211) trim(CurrentBlock_g)
211      format('no SERIES_NAME_OBS keyword provided in ',a,' block.')
         go to 9800
       end if

       if(aname.eq.' ')then
         write(amessage,230) trim(CurrentBlock_g)
230      format('no NEW_C_TABLE keyword provided in ',a,' block.')
         go to 9800
       end if
       if(icontext.eq.0)then
         write(amessage,220) trim(CurrentBlock_g)
220      format('no CONTEXT keyword(s) provided in ',a,' block.')
         go to 9800
       end if
       call date_check(ierr,yy1,mm1,dd1,hh1,nn1,ss1,yy2,mm2,dd2,hh2,nn2,ss2,  &
       begdays,begsecs,enddays,endsecs)
       if(ierr.ne.0) go to 9800
       call beg_end_check(ierr,isseries,begdays,begsecs,enddays,endsecs)
       if(ierr.ne.0) go to 9800
       call beg_end_check(ierr,ioseries,begdays,begsecs,enddays,endsecs)
       if(ierr.ne.0) go to 9800
       if((jbias.eq.0).and.(jse.eq.0).and.(jrbias.eq.0)  &
         .and.(jrse.eq.0).and.(jns.eq.0).and.(jce.eq.0).and.(jia.eq.0))then
         write(amessage,240) trim(CurrentBlock_g)
240      format('at least one of the BIAS, STANDARD_ERROR, RELATIVE_BIAS, ',     &
         'RELATIVE_STANDARD_ERROR, NASH_SUTCLIFFE, ', &
         'COEFFICIENT_OF_EFFICIENCY or INDEX_OF_AGREEMENT keywords must ', &
         'be supplied within a ',a,' block.')
         go to 9800
       end if
       if(ibseries.ne.0)then
         if((jce.eq.0).and.(jia.eq.0))then
           write(amessage,245) trim(CurrentBlock_g)
245        format('a SERIES_NAME_BASE keyword can only be supplied ', &
           'if a COEFFICIENT_OF_EFFICIENCY and/or INDEX_OF_AGREEMENT ',   &
           'keyword is supplied in a ',a,' block.')
           go to 9800
         end if
       end if
       if((jia.ne.0).or.(jce.ne.0))then
         if(exponent.eq.-9999)then
           write(amessage,250) trim(CurrentBlock_g)
250        format('if a COEFFICIENT_OF_EFFICIENCY or INDEX_OF_AGREEMENT ',  &
           'keyword is supplied, an EXPONENT keyword must be supplied in ', &
           a,' block.')
           go to 9800
         end if
       end if
       if(exponent.ne.-9999)then
         if((jia.eq.0).and.(jce.eq.0))then
           write(amessage,251) trim(CurrentBlock_g)
251        format('if an EXPONENT keyword is supplied, then a ',  &
           'COEFFICIENT_OF_EFFICIENCY or INDEX_OF_AGREEMENT keyword must ', &
           'also be supplied in a ',a,' block.')
           go to 9800
         end if
       end if

! -- The two (maybe three) time series are checked for time consitency.

       call numterms(isterm,isbterm,iseterm,begdays,begsecs,enddays,endsecs,isseries)
       if(isterm.eq.0)then
         write(amessage,270) trim(series_g(isseries)%name)
270      format('there are no terms in time series "',a,'" between the provided ', &
         'dates and times.')
         go to 9800
       end if
       call numterms(ioterm,iobterm,ioeterm,begdays,begsecs,enddays,endsecs,ioseries)
       if(ioterm.eq.0)then
         write(amessage,270) trim(series_g(ioseries)%name)
         go to 9800
       end if
       if(isterm.ne.ioterm) go to 9300
       if(isterm.le.2) then
         write(amessage,271)
271      format('there must be at least two terms in the comparison time ', &
         'window of the nominated series.')
         go to 9800
       end if

       i=iobterm-1
       do j=isbterm,iseterm
         i=i+1
         if(series_g(isseries)%days(j).ne.(series_g(ioseries)%days(i))) go to 9300
         if(series_g(isseries)%secs(j).ne.(series_g(ioseries)%secs(i))) go to 9300
       end do

       if(ibseries.ne.0)then
         call numterms(ibterm,ibbterm,ibeterm,begdays,begsecs,enddays,endsecs,ibseries)
         if(ibterm.eq.0)then
           write(amessage,270) trim(series_g(ibseries)%name)
           go to 9800
         end if
         if(ibterm.ne.ioterm) go to 9400
         i=iobterm-1
         do j=ibbterm,ibeterm
           i=i+1
           if(series_g(ibseries)%days(j).ne.(series_g(ioseries)%days(i))) go to 9400
           if(series_g(ibseries)%secs(j).ne.(series_g(ioseries)%secs(i))) go to 9400
         end do
       end if

! The new c_table is initialized.

       do i=1,MAXCTABLE
         if(.not.ctable_g(i)%active) go to 300
       end do
       write(amessage,310)
310    format('no more C_TABLE''s available for data storage - increase MAXCTABLE and ', &
       'recompile program.')
       go to 9800
300    continue

       ctable_g(i)%active=.true.
       ctable_g(i)%name=aname
       ctable_g(i)%rec_icount=isterm
       ctable_g(i)%series_name_sim=series_g(isseries)%name
       ctable_g(i)%series_name_obs=series_g(ioseries)%name
       if(begdays.le.-99999990)then
         ctable_g(i)%rec_begdays=series_g(isseries)%days(1)
         ctable_g(i)%rec_begsecs=series_g(isseries)%secs(1)
       else
         ctable_g(i)%rec_begdays=begdays
         ctable_g(i)%rec_begsecs=begsecs
       end if
       if(enddays.ge.99999990)then
         iiterm=series_g(isseries)%nterm
         ctable_g(i)%rec_enddays=series_g(isseries)%days(iiterm)
         ctable_g(i)%rec_endsecs=series_g(isseries)%secs(iiterm)
       else
         ctable_g(i)%rec_enddays=enddays
         ctable_g(i)%rec_endsecs=endsecs
       end if

! -- The comparison statistics are now calculated.

       tsum1=0.0
       tsum2=0.0
       tsum3=0.0
       tsum4=0.0
       k=iobterm-1
       do j=isbterm,iseterm
         k=k+1
         rtemp=series_g(ioseries)%val(k)
!         if((jrbias.ne.0).or.(jrse.ne.0).or.(jns.ne.0))then
!           if(rtemp.le.0.0)then
!             write(amessage,280)
!280          format('RELATIVE_BIAS, RELATIVE_STANDARD_ERROR or NASH_SUTCLIFFE ', &
!             'coefficient cannot be calculated because at least one term in the ', &
!             'observation time series has a value equal to, or less than, zero.')
!             go to 9800
!           end if
!         end if
         rtemp1=series_g(isseries)%val(j)-rtemp
         tsum1=tsum1+rtemp1
         tsum2=tsum2+rtemp1*rtemp1
         tsum3=tsum3+rtemp
         if((jia.ne.0).or.(jce.ne.0))then
           tsum4=tsum4+abs(rtemp1)**exponent
         end if
       end do
       tsum3=tsum3/isterm
       if(jbias.ne.0)then
         ctable_g(i)%bias=tsum1/isterm
       else
         ctable_g(i)%bias=-1.0e37
       end if
       if(jse.ne.0)then
         ctable_g(i)%se=sqrt(tsum2/(isterm-1))
       else
         ctable_g(i)%se=-1.0e37
       end if
       if(jrbias.ne.0)then
         if(tsum3.eq.0.0)then
           ctable_g(i)%rbias=1.0e30
         else
           ctable_g(i)%rbias=tsum1/isterm/tsum3
         end if
       else
         ctable_g(i)%rbias=-1.0e37
       end if
       if((jrse.ne.0).or.(jns.ne.0))then
         tsum1=0.0
         k=iobterm-1
         do j=isbterm,iseterm
           k=k+1
           rtemp1=series_g(ioseries)%val(k)-tsum3
           tsum1=tsum1+rtemp1*rtemp1
         end do
         if(tsum1.le.0.0)then
           write(amessage,390) trim(series_g(ioseries)%name)
390        format('cannot compute RELATIVE_STANDARD_ERROR or NASH_SUTCLIFFE ', &
           'coefficient because observation time series "',a,'" is uniform ', &
           'in observation time window.')
           go to 9800
         end if
         if(jrse.ne.0)then
           ctable_g(i)%rse=sqrt(tsum2/(isterm-1))/sqrt(tsum1/(isterm-1))
         else
           ctable_g(i)%rse=-1.0e37
         end if
         if(jns.ne.0)then
           ctable_g(i)%ns=1.0-tsum2/tsum1
         else
           ctable_g(i)%ns=-1.0e37
         end if
       else
           ctable_g(i)%rse=-1.0e37
           ctable_g(i)%ns=-1.0e37
       end if
       if(jce.ne.0)then
         tsum1=0.0
         k=iobterm-1
         if(ibseries.eq.0)then
           do j=isbterm,iseterm
             k=k+1
             rtemp1=(abs(series_g(ioseries)%val(k)-tsum3))**exponent
             tsum1=tsum1+rtemp1
           end do
           if(tsum1.le.0.0)then
             write(amessage,410) trim(series_g(ioseries)%name)
410          format('cannot compute COEFFICIENT_OF_EFFICIENCY ', &
             'because observation time series "',a,'" is uniform ', &
             'in observation time window.')
             go to 9800
           end if
         else
           do j=ibbterm,ibeterm
             k=k+1
             rtemp1=(abs(series_g(ioseries)%val(k)-series_g(ibseries)%val(j)))**exponent
             tsum1=tsum1+rtemp1
           end do
           if(tsum1.le.0.0)then
             write(amessage,420) trim(series_g(ioseries)%name),  &
                                 trim(series_g(ibseries)%name)
420          format('cannot compute COEFFICIENT_OF_EFFICIENCY ', &
             'because observation time series "',a,'" is equal to ', &
             'baseline time series "',a,'" in observation time window.')
             go to 9800
           end if
         end if
         ctable_g(i)%ce=1.0-tsum4/tsum1
       else
         ctable_g(i)%ce=-1.0e37
       end if
       if(jia.ne.0)then
         tsum1=0.0
         k=iobterm-1
         l=isbterm-1
         if(ibseries.eq.0)then
           do j=isbterm,iseterm
             k=k+1
             rtemp1=(abs(series_g(ioseries)%val(k)-tsum3)+   &
                     abs(series_g(isseries)%val(j)-tsum3))**exponent
             tsum1=tsum1+rtemp1
           end do
           if(tsum1.le.0.0)then
             write(amessage,430) trim(series_g(ioseries)%name), &
                                 trim(series_g(isseries)%name)
430          format('cannot compute INDEX_OF_AGREEMENT ', &
             'because observation time series "',a,'" and simulation ', &
             'time series "',a,'" are uniform and equal ', &
             'in observation time window.')
             go to 9800
           end if
         else
           do j=ibbterm,ibeterm
             k=k+1
             l=l+1
             rtemp1=(abs(series_g(ioseries)%val(k)-series_g(ibseries)%val(j))+  &
                     abs(series_g(isseries)%val(l)-series_g(ibseries)%val(j)))  &
                     **exponent
             tsum1=tsum1+rtemp1
           end do
           if(tsum1.le.0.0)then
             write(amessage,440)
440          format('cannot compute INDEX_OF_AGREEMENT ', &
             'because observation time series, simulation time series ', &
             'and baseline time series are all equal in observation time ', &
             'window.')
             go to 9800
           end if
         end if
         ctable_g(i)%ia=1.0-tsum4/tsum1
       else
         ctable_g(i)%ia=-1.0e37
       end if

       write(6,380) trim(aname)
       write(LU_REC,380) trim(aname)
380    format(t5,'Comparison statistics stored in C_TABLE "',a,'".')
       return

9000   call num2char(ILine_g,aline)
       call addquote(sInfile_g,sString_g)
       write(amessage,9010) trim(aline), trim(sString_g)
9010   format('cannot read line ',a,' of TSPROC input file ',a)
       go to 9800
9100   continue
       call addquote(sInfile_g,sString_g)
       write(amessage,9110) trim(sString_g),trim(CurrentBlock_g)
9110   format('unexpected end encountered to TSPROC input file ',a,' while ', &
       ' reading ',a,' block.')
       go to 9800
9300   write(amessage,9310)
9310   format('the two series cited in the COMPARE_SERIES block must have ',   &
       'identical sample dates and times within the comparison time ',  &
       'window. Maybe the use of a NEW_TIME_BASE block will rectify the problem.')
       go to 9800
9400   write(amessage,9410)
9410   format('the baseline series cited in the COMPARE_SERIES block must ',   &
       'have identical sample dates and times within the comparison time ',    &
       'window to the simulated and observed series. Maybe the use of a ',     &
       'NEW_TIME_BASE block will rectify the problem.')
       go to 9800

9800   call write_message(leadspace='yes',error='yes')
       call write_message(iunit=LU_REC,leadspace='yes')
       ifail=1

       return

end subroutine compare_series

!     Last change:  J    10 Sep 2004    0:00 am
subroutine reduce_span(ifail)

! -- Subroutine REDUCE_SPAN shortens the time-span of a time series.

       use tsp_data_structures
       use tsp_utilities
       use tsp_command_processors

       implicit none

       integer, intent(out)   :: ifail

       integer dd1,mm1,yy1,hh1,nn1,ss1,dd2,mm2,yy2,hh2,nn2,ss2,ierr, &
       icontext,i,begdays,begsecs,enddays,endsecs,iterm,j,           &
       iseries,k,ibterm,ieterm,ixcon
       character (len=iTSNAMELENGTH) :: aname
       character*15 aline
       character*25 aoption
       character*25 acontext(MAXCONTEXT)

       ifail=0
       CurrentBlock_g='REDUCE_TIME_SPAN'

       write(*,10) trim(CurrentBlock_g)
       write(LU_REC,10) trim(CurrentBlock_g)
10     format(/,' Processing ',a,' block....')

       icontext=0
       iseries=0
       yy1=-9999
       hh1=-9999
       yy2=-9999
       hh2=-9999
       aname=' '
       ixcon=0

! -- The REDUCE_TIME_SPAN block is first parsed.

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
         if(aoption.eq.'DATE_1')then
           call get_date(ierr,dd1,mm1,yy1,'DATE_1')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'DATE_2')then
           call get_date(ierr,dd2,mm2,yy2,'DATE_2')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'TIME_1')then
           call get_time(ierr,hh1,nn1,ss1,'TIME_1')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'TIME_2')then
           call get_time(ierr,hh2,nn2,ss2,'TIME_2')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'NEW_SERIES_NAME')then
           call get_new_series_name(ierr,aname)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'SERIES_NAME')then
           call get_series_name(ierr,iseries,'SERIES_NAME')
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
         else
           call num2char(ILine_g,aline)
           call addquote(sInfile_g,sString_g)
           write(amessage,90) trim(aoption),trim(CurrentBlock_g),trim(aline),trim(sString_g)
90         format('unexpected keyword - "',a,'" in ',a,' block at line ',a, &
           ' of file ',a)
           go to 9800
         end if
       end do

! -- The block has been read; now it is checked for correctness.

200    continue
       if(iseries.eq.0)then
         write(amessage,210) trim(CurrentBlock_g)
210      format('no SERIES_NAME keyword provided in ',a,' block.')
         go to 9800
       end if
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
       if((yy1.eq.-9999).and.(yy2.eq.-9999))then
         write(amessage,235) trim(CurrentBlock_g)
235      format('neither a DATE_1 keyword nor a DATE_2 keyword provided in ',a,' block')
         go to 9800
       end if
       call date_check(ierr,yy1,mm1,dd1,hh1,nn1,ss1,yy2,mm2,dd2,hh2,nn2,ss2,  &
       begdays,begsecs,enddays,endsecs)
       if(ierr.ne.0) go to 9800
       call beg_end_check(ierr,iseries,begdays,begsecs,enddays,endsecs)
       if(ierr.ne.0) go to 9800

! -- The new series is now written. But first the number of terms in the new series
!    is counted.

       call numterms(iterm,ibterm,ieterm,begdays,begsecs,enddays,endsecs,iseries)
       if(iterm.le.0)then
         write(amessage,315)
315      format('there are no terms in the reduced-time-span series.')
         go to 9800
       end if

       do i=1,MAXSERIES
         if(.not.series_g(i)%active) go to 515
       end do
       write(amessage,510)
510    format('no more time series available for data storage - increase MAXSERIES and ', &
       'recompile program.')
       go to 9800

515    allocate(series_g(i)%days(iterm),series_g(i)%secs(iterm),  &
       series_g(i)%val(iterm),stat=ierr)
       if(ierr.ne.0)then
         write(amessage,550)
550      format('cannot allocate memory for another time series.')
         go to 9800
       end if
       series_g(i)%active=.true.
       series_g(i)%name=aname
       series_g(i)%nterm=iterm
       series_g(i)%type='ts'
       k=0
       do j=ibterm,ieterm
         k=k+1
         series_g(i)%days(k)=series_g(iseries)%days(j)
       end do
       k=0
       do j=ibterm,ieterm
         k=k+1
         series_g(i)%secs(k)=series_g(iseries)%secs(j)
       end do
       k=0
       do j=ibterm,ieterm
         k=k+1
         series_g(i)%val(k)=series_g(iseries)%val(j)
       end do
       write(*,580) trim(aname)
       write(LU_REC,580) trim(aname)
580    format(t5,'Series "',a,'" successfully calculated.')
       return

9000   call num2char(ILine_g,aline)
       call addquote(sInfile_g,sString_g)
       write(amessage,9010) trim(aline), trim(sString_g)
9010   format('cannot read line ',a,' of TSPROC input file ',a)
       go to 9800
9100   continue
       call addquote(sInfile_g,sString_g)
       write(amessage,9110) trim(sString_g), trim(CurrentBlock_g)
9110   format('unexpected end encountered to TSPROC input file ',a,' while ', &
       ' reading ',a,' block.')
       go to 9800

9800   call write_message(leadspace='yes',error='yes')
       call write_message(iunit=LU_REC,leadspace='yes')
       ifail=1

       return

end subroutine reduce_span




subroutine statistics(ifail)

! -- Subroutine STATISTICS calculates summary statistics for a time series.

       use tsp_data_structures
       use tsp_utilities
       use tsp_command_processors

       implicit none

       integer, intent(out)   :: ifail

       integer dd1,mm1,yy1,hh1,nn1,ss1,dd2,mm2,yy2,hh2,nn2,ss2,ierr, &
       icontext,i,begdays,begsecs,enddays,endsecs,iseries,jtrans,javerage, &
       jstddev,jmaximum,jminimum,jsum,j,ibterm,ieterm,iterm,iiterm,itemp,ixcon, &
       iitemp,jj,minaverage,maxaverage,ii,nnterm,jrange
       real tpower,tsum,tmin,tmax,rtemp,raverage,localsum,tminmean,tmaxmean
       character*3 aaa
       character (len=iTSNAMELENGTH) :: aname,atemp
       character*15 aline
       character*25 aoption
       character*25 acontext(MAXCONTEXT)

       ifail=0
       CurrentBlock_g='SERIES_STATISTICS'

       write(*,10) trim(CurrentBlock_g)
       write(LU_REC,10) trim(CurrentBlock_g)
10     format(/,' Processing ',a,' block....')

       icontext=0
       iseries=0
       yy1=-9999
       hh1=-9999
       yy2=-9999
       hh2=-9999
       jtrans=0
       javerage=0
       jstddev=0
       jmaximum=0
       jminimum=0
       jrange=0
       jsum=0
       aname=' '
       tpower=-1.0e35
       ixcon=0
       minaverage=0
       maxaverage=0

! -- The SERIES_STATISTICS block is first parsed.

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
         if(aoption.eq.'DATE_1')then
           call get_date(ierr,dd1,mm1,yy1,'DATE_1')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'DATE_2')then
           call get_date(ierr,dd2,mm2,yy2,'DATE_2')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'TIME_1')then
           call get_time(ierr,hh1,nn1,ss1,'TIME_1')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'TIME_2')then
           call get_time(ierr,hh2,nn2,ss2,'TIME_2')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'NEW_S_TABLE_NAME')then
           call get_new_table_name(ierr,1,aname)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'SERIES_NAME')then
           call get_series_name(ierr,iseries,'SERIES_NAME')
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
         else if(aoption.eq.'POWER')then
           call get_keyword_value(ierr,2,itemp,tpower,'POWER')
           if(ierr.ne.0) go to 9800
           if(tpower.eq.0.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,125) trim(aline),trim(sString_g)
125          format('POWER must not be zero at line ',a,' of file ',a)
             go to 9800
           end if
         else if(aoption.eq.'LOG')then
           call get_yes_no(ierr,jtrans)
           if(ierr.ne.0) go to 9800
           if(jtrans.eq.1)then
             aaa='yes'
           else
             aaa='no'
           end if
           write(*,127) trim(aaa)
           write(LU_REC,127) trim(aaa)
127        format(t5,'LOG ',a)
         else if(aoption.eq.'MEAN')then
           call get_yes_no(ierr,javerage)
           if(ierr.ne.0) go to 9800
           if(javerage.eq.1)then
             aaa='yes'
           else
             aaa='no'
           end if
           write(*,128) trim(aaa)
           write(LU_REC,128) trim(aaa)
128        format(t5,'MEAN ',a)
         else if(aoption(1:8).eq.'MINMEAN_')then
           call get_yes_no(ierr,iitemp)
           if(ierr.ne.0) go to 9800
           if(iitemp.eq.1)then
             aaa='yes'
           else
             aaa='no'
             go to 178
           end if
           if(minaverage.ne.0)then
             write(amessage,156) trim(CurrentBlock_g)
156          format('only one MINMEAN_* keyword is allowed in each ',a,' block.')
             go to 9800
           else
             minaverage=iitemp
           end if
           atemp=aoption(9:)
           call char2num(ierr,atemp,iitemp)
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,154) trim(aline),trim(sString_g)
154          format('cannot read averaging count for MINMEAN_* keyword at line ',  &
             a,' of file ',a)
             go to 9800
           end if
           if(iitemp.le.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,153) trim(aline),trim(sString_g)
153          format('illegal averaging count for MINMEAN_* keyword at line ',  &
             a,' of file ',a)
             go to 9800
           end if
           minaverage=iitemp
178        continue
           write(*,151) trim(aoption),trim(aaa)
           write(LU_REC,151) trim(aoption),trim(aaa)
151        format(t5,a,1x,a)
         else if(aoption(1:8).eq.'MAXMEAN_')then
           call get_yes_no(ierr,iitemp)
           if(ierr.ne.0) go to 9800
           if(iitemp.eq.1)then
             aaa='yes'
           else
             aaa='no'
             go to 179
           end if
           if(maxaverage.ne.0)then
             write(amessage,138) trim(CurrentBlock_g)
138          format('only one MAXMEAN_* keyword is allowed in each ',a,' block.')
             go to 9800
           end if
           maxaverage=iitemp
           atemp=aoption(9:)
           call char2num(ierr,atemp,iitemp)
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,149) trim(aline),trim(sString_g)
149          format('cannot read averaging count for MAXMEAN_* keyword at line ',  &
             a,' of file ',a)
             go to 9800
           end if
           if(iitemp.le.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,139) trim(aline),trim(sString_g)
139          format('illegal averaging count for MAXMEAN_* keyword at line ',  &
             a,' of file ',a)
             go to 9800
           end if
           maxaverage=iitemp
179        continue
           write(*,151) trim(aoption),trim(aaa)
           write(LU_REC,151) trim(aoption),trim(aaa)
         else if((aoption.eq.'STD_DEV').or.(aoption.eq.'STANDARD_DEVIATION'))then
           call get_yes_no(ierr,jstddev)
           if(ierr.ne.0) go to 9800
           if(jstddev.eq.1)then
             aaa='yes'
           else
             aaa='no'
           end if
           write(*,129) trim(aaa)
           write(LU_REC,129) trim(aaa)
129        format(t5,'STD_DEV ',a)
         else if(aoption.eq.'MAXIMUM')then
           call get_yes_no(ierr,jmaximum)
           if(ierr.ne.0) go to 9800
           if(jmaximum.eq.1)then
             aaa='yes'
           else
             aaa='no'
           end if
           write(*,132) trim(aaa)
           write(LU_REC,132) trim(aaa)
132        format(t5,'MAXIMUM ',a)
         else if(aoption.eq.'MINIMUM')then
           call get_yes_no(ierr,jminimum)
           if(ierr.ne.0) go to 9800
           if(jminimum.eq.1)then
             aaa='yes'
           else
             aaa='no'
           end if
           write(*,133) trim(aaa)
           write(LU_REC,133) trim(aaa)
133        format(t5,'MINIMUM ',a)
         else if(aoption.eq.'RANGE')then
           call get_yes_no(ierr,jrange)
           if(ierr.ne.0) go to 9800
           if(jrange.eq.1)then
             aaa='yes'
           else
             aaa='no'
           end if
           write(*,136) trim(aaa)
           write(LU_REC,136) trim(aaa)
136        format(t5,'RANGE ',a)
         else if(aoption.eq.'SUM')then
           call get_yes_no(ierr,jsum)
           if(ierr.ne.0) go to 9800
           if(jsum.eq.1)then
             aaa='yes'
           else
             aaa='no'
           end if
           write(*,134) trim(aaa)
           write(LU_REC,134) trim(aaa)
134        format(t5,'SUM ',a)
         else if(aoption.eq.'END')then
           go to 200
         else
           call num2char(ILine_g,aline)
           call addquote(sInfile_g,sString_g)
           write(amessage,90) trim(aoption),trim(CurrentBlock_g),trim(aline),trim(sString_g)
90         format('unexpected keyword - "',a,'" in ',a,' block at line ',a, &
           ' of file ',a)
           go to 9800
         end if
       end do

! -- The block has been read; now it is checked for correctness.

200    continue
       if(iseries.eq.0)then
         write(amessage,210) trim(CurrentBlock_g)
210      format('no SERIES_NAME keyword provided in ',a,' block.')
         go to 9800
       end if
       if(aname.eq.' ')then
         write(amessage,230) trim(CurrentBlock_g)
230      format('no NEW_S_TABLE keyword provided in ',a,' block.')
         go to 9800
       end if
       if(icontext.eq.0)then
         write(amessage,220) trim(CurrentBlock_g)
220      format('no CONTEXT keyword(s) provided in ',a,' block.')
         go to 9800
       end if
       call date_check(ierr,yy1,mm1,dd1,hh1,nn1,ss1,yy2,mm2,dd2,hh2,nn2,ss2,  &
       begdays,begsecs,enddays,endsecs)
       if(ierr.ne.0) go to 9800
       call beg_end_check(ierr,iseries,begdays,begsecs,enddays,endsecs)
       if(ierr.ne.0) go to 9800
       if((javerage.eq.0).and.(jstddev.eq.0).and.(jmaximum.eq.0)  &
         .and.(jminimum.eq.0).and.(jsum.eq.0).and.(maxaverage.eq.0)  &
         .and.(minaverage.eq.0).and.(jrange.eq.0))then
         write(amessage,240) trim(CurrentBlock_g)
240      format('at least one of the MEAN, STD_DEV, MAXIMUM, MINIMUM, ', &
         'RANGE, SUM, MINMEAN_* or MAXMEAN_*  keywords must be supplied within a ',  &
         a,' block.')
         go to 9800
       end if
       if((jtrans.eq.1).and.(tpower.gt.-1.0e30))then
         write(amessage,245) trim(CurrentBlock_g)
245      format('either the LOG or POWER keywords can be supplied ', &
         'in a ',a,' block, but not both.')
         go to 9800
       end if
       if((minaverage.ne.0).or.(maxaverage.ne.0))then
         if((jtrans.eq.1).or.(tpower.gt.-1.0d30))then
           write(amessage,246) trim(CurrentBlock_g)
246        format('if a MINMEAN_* or MAXMEAN_* keyword is supplied in ',a,' block, ', &
           'then neither the LOG or POWER keywords can be supplied in the same block.')
           go to 9800
         end if
       end if
       if(minaverage.ne.0)then
         if(maxaverage.ne.0)then
           if(minaverage.ne.maxaverage)then
             write(amessage,247) trim(CurrentBlock_g)
247          format('if both a MINMEAN_* and a MAXMEAN_* keyword are supplied ',   &
             'in a ',a,' block, then the averaging count must be the same for both.')
             go to 9800
           end if
         end if
       end if

! -- All is well with the block. The STABLE is filled with requested statistics.

       call numterms(iterm,ibterm,ieterm,begdays,begsecs,enddays,endsecs,iseries)
       if(iterm.eq.0)then
         write(amessage,270) trim(series_g(iseries)%name)
270      format('there are no terms in time series "',a,'" between the provided ', &
         'dates and times.')
         go to 9800
       end if
       if((minaverage.gt.iterm).or.(maxaverage.gt.iterm))then
         write(amessage,271)
271      format('the averaging count provided with the MINMEAN_* and/or ', &
         'MAXMEAN_* keyword is greater than the number of terms in the block.')
         go to 9800
       end if


       do i=1,MAXSTABLE
         if(.not.stable_g(i)%active) go to 300
       end do
       write(amessage,310)
310    format('no more S_TABLE''s available for data storage - increase MAXSTABLE and ', &
       'recompile program.')
       go to 9800
300    continue

       if((begdays.lt.series_g(iseries)%days(1)).or.  &
         ((begdays.eq.series_g(iseries)%days(1)).and. &
          (begsecs.lt.series_g(iseries)%secs(1))))then
         begdays=series_g(iseries)%days(1)
         begsecs=series_g(iseries)%secs(1)
       end if
       iiterm=series_g(iseries)%nterm
       if((enddays.gt.series_g(iseries)%days(iiterm)).or.  &
         ((enddays.eq.series_g(iseries)%days(iiterm)).and. &
          (endsecs.gt.series_g(iseries)%secs(iiterm))))then
         enddays=series_g(iseries)%days(iiterm)
         endsecs=series_g(iseries)%secs(iiterm)
       end if

       if(tpower.lt.-1.0e30)tpower=0.0
       stable_g(i)%active=.true.
       stable_g(i)%name=aname
       stable_g(i)%rec_icount=iterm
       stable_g(i)%series_name=series_g(iseries)%name
       stable_g(i)%rec_itrans=jtrans
       if(begdays.eq.-99999999)then
         stable_g(i)%rec_begdays=series_g(iseries)%days(1)
         stable_g(i)%rec_begsecs=series_g(iseries)%secs(1)
       else
         stable_g(i)%rec_begdays=begdays
         stable_g(i)%rec_begsecs=begsecs
       end if
       if(enddays.eq.99999999)then
         stable_g(i)%rec_enddays=series_g(iseries)%days(iiterm)
         stable_g(i)%rec_endsecs=series_g(iseries)%secs(iiterm)
       else
         stable_g(i)%rec_enddays=enddays
         stable_g(i)%rec_endsecs=endsecs
       end if
       stable_g(i)%rec_power=tpower

       tsum=0.0
       tmin=1.0e30
       tmax=-1.0e30
       tminmean=1.0e30
       tmaxmean=-1.0e30
       if(jtrans.eq.1)then
         do j=ibterm,ieterm
           rtemp=series_g(iseries)%val(j)
           if(rtemp.le.0.0)then
             write(amessage,350) trim(series_g(iseries)%name)
350          format('cannot compute statistics on basis of log transform of terms ', &
             'in series "',a,'" as there are zero or negative terms in this series.')
             go to 9800
           end if
           rtemp=log10(rtemp)
           tsum=tsum+rtemp
           if(rtemp.lt.tmin)tmin=rtemp
           if(rtemp.gt.tmax)tmax=rtemp
         end do
       else
         if(tpower.eq.0.0)then
           do j=ibterm,ieterm
             rtemp=series_g(iseries)%val(j)
             tsum=tsum+rtemp
             if(rtemp.lt.tmin)tmin=rtemp
             if(rtemp.gt.tmax)tmax=rtemp
             if((maxaverage.gt.0).or.(minaverage.gt.0))then
               localsum=0
               nnterm=max(minaverage,maxaverage)
               do ii=1,nnterm
                 jj=j+ii-1
                 if(jj.gt.ieterm) go to 359
                 localsum=localsum+series_g(iseries)%val(jj)
               end do
               localsum=localsum/nnterm
               if(maxaverage.gt.0)then
                 if(localsum.gt.tmaxmean)tmaxmean=localsum
               end if
               if(minaverage.gt.0)then
                 if(localsum.lt.tminmean)tminmean=localsum
               end if
359            continue
             end if
           end do
         else
           do j=ibterm,ieterm
             rtemp=series_g(iseries)%val(j)
             if((tpower.lt.0.0).and.(rtemp.eq.0.0))then
               write(amessage,355) trim(series_g(iseries)%name)
355            format('cannot compute statistics based on a negative POWER because ', &
               'at least one of the terms of series "',a,'" is zero.')
               go to 9800
             end if
             if((abs(tpower).lt.1.0).and.(rtemp.lt.0.0))then
               write(amessage,360) trim(series_g(iseries)%name)
360            format('cannot compute statistics based on a POWER with absolute value ', &
               'less than one because ', &
               'at least one of the terms of series "',a,'" is negative.')
               go to 9800
             end if
             rtemp=rtemp**tpower
             tsum=tsum+rtemp
             if(rtemp.lt.tmin)tmin=rtemp
             if(rtemp.gt.tmax)tmax=rtemp
           end do
         end if
       end if
       raverage=tsum/iterm
       if(jmaximum.eq.1)then
         stable_g(i)%maximum=tmax
       else
         stable_g(i)%maximum=-1.0e37
       end if
       if(jminimum.eq.1)then
         stable_g(i)%minimum=tmin
       else
         stable_g(i)%minimum=-1.0e37
       end if
       if(jrange.eq.1)then
         stable_g(i)%range=tmax-tmin
       else
         stable_g(i)%range=-1.0e37
       end if
       if(javerage.eq.1)then
         stable_g(i)%mean=raverage
       else
         stable_g(i)%mean=-1.0e37
       end if
       if(jsum.eq.1)then
         stable_g(i)%total=tsum
       else
         stable_g(i)%total=-1.0e37
       end if
       if(jstddev.eq.0)then
         stable_g(i)%stddev=-1.0e37
       else
         tsum=0
         if(jtrans.eq.1)then
           do j=ibterm,ieterm
             rtemp=series_g(iseries)%val(j)
             rtemp=log10(rtemp)-raverage
             tsum=tsum+rtemp*rtemp
           end do
         else
           if(tpower.eq.0.0)then
             do j=ibterm,ieterm
               rtemp=series_g(iseries)%val(j)
               rtemp=rtemp-raverage
               tsum=tsum+rtemp*rtemp
             end do
           else
             do j=ibterm,ieterm
               rtemp=series_g(iseries)%val(j)
               rtemp=rtemp**tpower-raverage
               tsum=tsum+rtemp*rtemp
             end do
           end if
         end if
         if(iterm.eq.1)then
!           tsum=sqrt(tsum)
           tsum=0.0
         else
           tsum=sqrt(tsum/(iterm-1))
         end if
         stable_g(i)%stddev=tsum
       end if
       stable_g(i)%avetime=0
       if(maxaverage.eq.0)then
         stable_g(i)%maxmean=-1.0e37
       else
         stable_g(i)%maxmean=tmaxmean
         stable_g(i)%avetime=nnterm
       end if
       if(minaverage.eq.0)then
         stable_g(i)%minmean=-1.0e37
       else
         stable_g(i)%minmean=tminmean
         stable_g(i)%avetime=nnterm
       end if

       write(6,380) trim(series_g(iseries)%name),trim(aname)
       write(LU_REC,380) trim(series_g(iseries)%name),trim(aname)
380    format(t5,'Statistics for time series "',a,'" stored in ', &
       'S_TABLE "',a,'".')
       return


9000   call num2char(ILine_g,aline)
       call addquote(sInfile_g,sString_g)
       write(amessage,9010) trim(aline), trim(sString_g)
9010   format('cannot read line ',a,' of TSPROC input file ',a)
       go to 9800
9100   continue
       call addquote(sInfile_g,sString_g)
       write(amessage,9110) trim(sString_g),trim(CurrentBlock_g)
9110   format('unexpected end encountered to TSPROC input file ',a,' while ', &
       ' reading ',a,' block.')
       go to 9800

9800   call write_message(leadspace='yes',error='yes')
       call write_message(iunit=LU_REC,leadspace='yes')
       ifail=1

       return

end subroutine statistics


subroutine time_base(ifail)

! -- Subroutine TIME_BASE spatially interpolates one time series to the sample dates/times
!    of another.

       use tsp_data_structures
       use tsp_utilities
       use tsp_command_processors

       implicit none

       integer, intent(out)   :: ifail

       integer ierr,icontext,i,iseries,j,itbseries,ntermtb,ndaysbtb, &
       nsecsbtb,ndaysftb,nsecsftb,ntermos,ndaysbos,nsecsbos,ndaysfos,nsecsfos,istart, &
       intday,intsec,ixcon
       real valinterp
       character (len=iTSNAMELENGTH) :: aname
       character*15 aline
       character*25 aoption
       character*25 acontext(MAXCONTEXT)

!interface
!	subroutine time_interp_s(ifail,nbore,ndays,nsecs,value,intday, &
!	intsec,rnear,rconst,valinterp,extrap,direction,startindex)
!          integer, intent(out)                    :: ifail
!          integer, intent(in)                     :: nbore
!          integer, intent(in), dimension(nbore)   :: ndays,nsecs
!          real, intent(in), dimension(nbore)      :: value
!          integer, intent(in)                     :: intday,intsec
!	  real, intent(in)			  :: rnear,rconst
!          real, intent(out)                       :: valinterp
!	  character (len=*), intent(in),optional  :: extrap
!	  character (len=*), intent(in),optional  :: direction
!          integer, intent(inout), optional        :: startindex
!	end subroutine time_interp_s
!end interface


       ifail=0
       CurrentBlock_g='NEW_TIME_BASE'

       write(*,10) trim(CurrentBlock_g)
       write(LU_REC,10) trim(CurrentBlock_g)
10     format(/,' Processing ',a,' block....')

       itbseries=0
       icontext=0
       iseries=0
       aname=' '
       ixcon=0

! -- The NEW_TIME_BASE block is first parsed.

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
         else if(aoption.eq.'SERIES_NAME')then
           call get_series_name(ierr,iseries,'SERIES_NAME')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'TB_SERIES_NAME')then
           call get_series_name(ierr,itbseries,'TB_SERIES_NAME')
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
         else
           call num2char(ILine_g,aline)
           call addquote(sInfile_g,sString_g)
           write(amessage,90) trim(aoption),trim(CurrentBlock_g),trim(aline),trim(sString_g)
90         format('unexpected keyword - "',a,'" in ',a,' block at line ',a, &
           ' of file ',a)
           go to 9800
         end if
       end do

! -- The block has been read; now it is checked for correctness.

200    continue
       if(iseries.eq.0)then
         write(amessage,210) trim(CurrentBlock_g)
210      format('no SERIES_NAME keyword provided in ',a,' block.')
         go to 9800
       end if
       if(aname.eq.' ')then
         write(amessage,230) trim(CurrentBlock_g)
230      format('no NEW_SERIES_NAME keyword provided in ',a,' block.')
         go to 9800
       end if
       if(itbseries.eq.0)then
         write(amessage,218) trim(CurrentBlock_g)
218      format('no TB_SERIES_NAME keyword provided in ',a,' block.')
         go to 9800
       end if
       if(icontext.eq.0)then
         write(amessage,220) trim(CurrentBlock_g)
220      format('no CONTEXT keyword(s) provided in ',a,' block.')
         go to 9800
       end if

       ntermtb=series_g(itbseries)%nterm
       ndaysbtb=series_g(itbseries)%days(1)
       nsecsbtb=series_g(itbseries)%secs(1)
       ndaysftb=series_g(itbseries)%days(ntermtb)
       nsecsftb=series_g(itbseries)%secs(ntermtb)
       ntermos=series_g(iseries)%nterm
       ndaysbos=series_g(iseries)%days(1)
       nsecsbos=series_g(iseries)%secs(1)
       ndaysfos=series_g(iseries)%days(ntermos)
       nsecsfos=series_g(iseries)%secs(ntermos)
       if((ndaysbtb.lt.ndaysbos).or.         &
         ((ndaysbtb.eq.ndaysbos).and.(nsecsbtb.lt.nsecsbos)))go to 9200
       if((ndaysftb.gt.ndaysfos).or.         &
         ((ndaysftb.eq.ndaysfos).and.(nsecsftb.gt.nsecsfos)))go to 9200

! -- Memory is now allocated for the new series prior to its being filled.

       do i=1,MAXSERIES
         if(.not.series_g(i)%active) go to 250
       end do
       write(amessage,240)
240    format('no more time series available for data storage - increase MAXSERIES and ', &
       'recompile program.')
       go to 9800
250    allocate(series_g(i)%days(ntermtb),series_g(i)%secs(ntermtb),  &
       series_g(i)%val(ntermtb),stat=ierr)
       if(ierr.ne.0)then
         write(amessage,560)
560      format('cannot allocate memory for another time series.')
         go to 9800
       end if
       series_g(i)%active=.true.
       series_g(i)%name=aname
       series_g(i)%nterm=ntermtb
       series_g(i)%type='ts'
       do j=1,ntermtb
         series_g(i)%days(j)=series_g(itbseries)%days(j)
       end do
       do j=1,ntermtb
         series_g(i)%secs(j)=series_g(itbseries)%secs(j)
       end do

! -- Temporal interpolation is now undertaken.

       istart=0
       do j=1,ntermtb
         intday=series_g(i)%days(j)
         intsec=series_g(i)%secs(j)
         call time_interp_s(ierr,ntermos,series_g(iseries)%days,series_g(iseries)%secs, &
         series_g(iseries)%val,intday,intsec,1.0e20,0.0,valinterp,startindex=istart)
         series_g(i)%val(j)=valinterp
       end do

       write(*,580) trim(aname)
       write(LU_REC,580) trim(aname)
580    format(t5,'New series "',a,'" successfully calculated.')
       return

9000   call num2char(ILine_g,aline)
       call addquote(sInfile_g,sString_g)
       write(amessage,9010) trim(aline), trim(sString_g)
9010   format('cannot read line ',a,' of TSPROC input file ',a)
       go to 9800
9100   continue
       call addquote(sInfile_g,sString_g)
       write(amessage,9110) trim(sString_g)
9110   format('unexpected end encountered to TSPROC input file ',a,' while ', &
       ' reading REDUCE_TIME_SPAN block.')
       go to 9800
9200   write(amessage,9210)
9210   format('the time span of the time base series is greater than that of the ', &
       'series to be interpolated. Reduce the time span of the time base series to ', &
       'that of the series to be interpolated using a REDUCE_SPAN block.')
       go to 9800

9800   call write_message(leadspace='yes',error='yes')
       call write_message(iunit=LU_REC,leadspace='yes')
       ifail=1

       return

end subroutine time_base



subroutine volume(ifail)

! -- Subroutine VOLUME accumulates volumes between user-specified dates and times.

       use tsp_data_structures
       use tsp_utilities
       use tsp_command_processors

       implicit none

       integer, intent(out)   :: ifail

       integer ierr,icontext,iseries,itunit,iunit,jline,ndate,iv,nsterm,nsdays1, &
       nssecs1,nsdays2,nssecs2,dd,mm,yy,hh,nn,ss,ndays1,nsecs1,ndays2,nsecs2,itemp,ixcon
       real factor,fac,volcalc
       character (len=iTSNAMELENGTH) :: aname
       character*15 aline
       character*25 aoption
       character*120 datefile
       character*25 acontext(MAXCONTEXT)

       ifail=0
       CurrentBlock_g='VOLUME_CALCULATION'

       write(*,10) trim(CurrentBlock_g)
       write(LU_REC,10) trim(CurrentBlock_g)
10     format(/,' Processing ',a,' block....')

       icontext=0
       iseries=0
       aname=' '
       factor=1.0
       itunit=0
       datefile=' '
       ixcon=0
       iunit=0

! -- The VOLUME_CALCULATION block is first parsed.

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
         if(aoption.eq.'SERIES_NAME')then
           call get_series_name(ierr,iseries,'SERIES_NAME')
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
         else if(aoption.eq.'NEW_V_TABLE_NAME')then
           call get_new_table_name(ierr,2,aname)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'FLOW_TIME_UNITS')then
           call get_time_units(ierr,itunit,1)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'FACTOR')then
           call get_keyword_value(ierr,2,itemp,factor,'FACTOR')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'DATE_FILE')then
           call getfile(ierr,cline,datefile,left_word(2),right_word(2))
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,140) trim(aline),trim(sString_g)
140          format('cannot read date file name from line ',a,' of file ',a)
             go to 9800
           end if
           call addquote(datefile,sString_g)
           write(*,145) trim(sString_g)
           write(LU_REC,145) trim(sString_g)
145        format(t5,'DATE_FILE ',a)
         else if(aoption.eq.'END')then
           go to 200
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

! -- The block has been read; now it is checked for absences.

       if(iseries.eq.0)then
         write(amessage,210) trim(CurrentBlock_g)
210      format('no SERIES_NAME keyword provided in ',a,' block.')
         go to 9800
       end if
       if(aname.eq.' ')then
         write(amessage,230) trim(CurrentBlock_g)
230      format('no NEW_V_TABLE_NAME keyword provided in ',a,' block.')
         go to 9800
       end if
       if(itunit.eq.0)then
         write(amessage,218) trim(CurrentBlock_g)
218      format('no FLOW_TIME_UNITS keyword provided in ',a,' block.')
         go to 9800
       end if
       if(icontext.eq.0)then
         write(amessage,220) trim(CurrentBlock_g)
220      format('no CONTEXT keyword(s) provided in ',a,' block.')
         go to 9800
       end if
       if(datefile.eq.' ')then
         write(amessage,240) trim(CurrentBlock_g)
240      format('no DATE_FILE keyword provided in ',a,' block.')
         go to 9800
       end if

! -- The date file is now opened and the number of lines within it read.

       iunit=nextunit()
       call addquote(datefile,sString_g)
       open(unit=iunit,file=datefile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,300) trim(sString_g)
300      format('cannot open dates file ',a)
         go to 9800
       end if
       write(6,305) trim(sString_g)
       write(LU_REC,305) trim(sString_g)
305    format(t5,'Reading dates file ',a,'....')
       jline=0
       ndate=0
       do
         jline=jline+1
         read(iunit,'(a)',end=350) cline
         if(cline.eq.' ') cycle
         if(cline(1:1).eq.'#') cycle
         ndate=ndate+1
       end do
350    continue
       if(ndate.eq.0)then
         write(amessage,360) trim(sString_g)
360      format('no dates found in dates file ',a)
         go to 9800
       end if
       rewind(unit=iunit,iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,370) trim(sString_g)
370      format('cannot rewind dates file ',a)
         go to 9800
       end if

! -- Memory is now allocated for the v_table.

       do iv=1,MAXVTABLE
         if(.not.vtable_g(iv)%active) go to 380
       end do
       write(amessage,390)
390    format('no more v_tables available for data storage - increase MAXVTABLE and ', &
       'recompile program.')
       go to 9800
380    continue
       vtable_g(iv)%active=.true.
       vtable_g(iv)%name=aname
       vtable_g(iv)%series_name=series_g(iseries)%name
       allocate(vtable_g(iv)%days1(ndate),vtable_g(iv)%secs1(ndate),vtable_g(iv)%days2(ndate), &
       vtable_g(iv)%secs2(ndate),vtable_g(iv)%vol(ndate),stat=ierr)
       if(ierr.ne.0)then
         write(amessage,395)
395      format('cannot allocate memory for storage of v_table data.')
         go to 9800
       end if

       if(itunit.eq.1)then
         fac=86400.0
       else if(itunit.eq.2)then
         fac=1440.0
       else if(itunit.eq.3)then
         fac=24.0
       else if(itunit.eq.4)then
         fac=1.0
       else if(itunit.eq.5)then
         fac=1.0*12/365.25
       else
         fac=1.0/365.25
       end if

! -- The date file is now re-read and volumes calculated.

       jline=0
       ndate=0
       nsterm=series_g(iseries)%nterm
       nsdays1=series_g(iseries)%days(1)
       nssecs1=series_g(iseries)%secs(1)
       nsdays2=series_g(iseries)%days(nsterm)
       nssecs2=series_g(iseries)%secs(nsterm)
       do
         jline=jline+1
         read(iunit,'(a)',end=500) cline
         if(cline.eq.' ') cycle
         if(cline(1:1).eq.'#') cycle
         ndate=ndate+1
         call linesplit(ierr,4)
         if(ierr.ne.0)then
           call num2char(jline,aline)
           write(amessage,410) trim(aline),trim(sString_g)
410        format('four entries expected on line ',a,' of dates file ',a)
           go to 9800
         end if
         call char2date(ierr,cline(left_word(1):right_word(1)),dd,mm,yy)
         if(ierr.ne.0) go to 9200
         ndays1=numdays(1,1,1970,dd,mm,yy)
         call char2time(ierr,cline(left_word(2):right_word(2)),hh,nn,ss,ignore_24=1)
         if(ierr.ne.0) go to 9200
         nsecs1=numsecs(0,0,0,hh,nn,ss)
         if(nsecs1.ge.86400)then
           nsecs1=nsecs1-86400
           ndays1=ndays1+1
         end if
         call char2date(ierr,cline(left_word(3):right_word(3)),dd,mm,yy)
         if(ierr.ne.0) go to 9200
         ndays2=numdays(1,1,1970,dd,mm,yy)
         call char2time(ierr,cline(left_word(4):right_word(4)),hh,nn,ss,ignore_24=1)
         if(ierr.ne.0) go to 9200
         nsecs2=numsecs(0,0,0,hh,nn,ss)
         if(nsecs2.ge.86400)then
           nsecs2=nsecs2-86400
           ndays2=ndays2+1
         end if
         if((ndays1.gt.ndays2).or.         &
           ((ndays1.eq.ndays2).and.(nsecs1.ge.nsecs2)))then
           call num2char(jline,aline)
           write(amessage,420) trim(aline),trim(sString_g)
420        format('first date/time must precede second date/time at line ',a,  &
           ' of file ',a)
           go to 9800
         end if
         if((ndays1.lt.nsdays1).or.        &
           ((ndays1.eq.nsdays1).and.(nsecs1.lt.nssecs1)))then
           call num2char(jline,aline)
           write(amessage,425) trim(aline),trim(sString_g),trim(series_g(iseries)%name)
425        format('the first date/time on line ',a,' of file ',a,' predates the ', &
           'commencement of time series "',a,'".')
           go to 9800
         end if
         if((ndays2.gt.nsdays2).or.         &
           ((ndays2.eq.nsdays2).and.(nsecs2.gt.nssecs2)))then
           call num2char(jline,aline)
           write(amessage,426) trim(aline),trim(sString_g),trim(series_g(iseries)%name)
426        format('the second date/time on line ',a,' of file ',a,' postdates the ', &
           'end of time series "',a,'".')
           go to 9800
         end if
         vtable_g(iv)%days1(ndate)=ndays1
         vtable_g(iv)%secs1(ndate)=nsecs1
         vtable_g(iv)%days2(ndate)=ndays2
         vtable_g(iv)%secs2(ndate)=nsecs2
         call volume_interp_s(ierr,nsterm,series_g(iseries)%days,series_g(iseries)%secs,  &
         series_g(iseries)%val,ndays1,nsecs1,ndays2,nsecs2,volcalc,fac)
         vtable_g(iv)%vol(ndate)=volcalc*factor
       end do
500    continue
       vtable_g(iv)%nterm=ndate
       close(unit=iunit)
       write(*,430) trim(sString_g)
       write(LU_REC,430) trim(sString_g)
430    format(t5,'File ',a,' read ok.')
       write(*,440) trim(aname)
       write(LU_REC,440) trim(aname)
440    format(t5,'Volumes calculated and stored in v_table "',a,'".')
       return

9000   call num2char(ILine_g,aline)
       call addquote(sInfile_g,sString_g)
       write(amessage,9010) trim(aline), trim(sString_g)
9010   format('cannot read line ',a,' of TSPROC input file ',a)
       go to 9800
9100   continue
       call addquote(sInfile_g,sString_g)
       write(amessage,9110) trim(sString_g),trim(CurrentBlock_g)
9110   format('unexpected end encountered to TSPROC input file ',a,' while ', &
       ' reading ',a,' block.')
       go to 9800
9200   continue
       call num2char(jline,aline)
       write(amessage,9210) trim(aline),trim(sString_g)
9210   format('erroneous date or time at line ',a,' of file ',a)
       go to 9800
9800   call write_message(leadspace='yes',error='yes')
       call write_message(iunit=LU_REC,leadspace='yes')
       ifail=1
       if(iunit.ne.0)close(unit=iunit,iostat=ierr)

       return

end subroutine volume

subroutine time_duration(ifail)

! -- Subroutine TIME_DURATION calculations exceedence durations for certain flows.

       use tsp_data_structures
       use tsp_utilities
       use tsp_command_processors

       implicit none

       integer, intent(out)   :: ifail

       logical on,oldon
       integer ierr,icontext,iseries,itunit,iflow,id,i,ndays,nsecs,j,oldndays,oldnsecs, &
               nnterm,ixcon,iuo
       real rtemp,fac,duration,fflow,vval,oldvval,timediff,accumulation,timedelay
       character (len=iTSNAMELENGTH) :: aname,atemp
       character*15 aline
       character*25 aoption
       character*25 acontext(MAXCONTEXT)

       ifail=0
       CurrentBlock_g='EXCEEDENCE_TIME'


       write(*,10) trim(CurrentBlock_g)
       write(LU_REC,10) trim(CurrentBlock_g)
10     format(/,' Processing ',a,' block....')

       icontext=0
       iseries=0
       aname=' '
       itunit=0
       iflow=0
       ixcon=0
       iuo=-999
       do i=1,MAXTEMPDURFLOW
         tempdtable_g%tdelay(i)=-1.1e36
       end do

! -- The EXCEEDENCE-TIME block is first parsed.

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
         if(aoption.eq.'SERIES_NAME')then
           call get_series_name(ierr,iseries,'SERIES_NAME')
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
         else if(aoption.eq.'NEW_E_TABLE_NAME')then
           call get_new_table_name(ierr,3,aname)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'EXCEEDENCE_TIME_UNITS')then
           call get_time_units(ierr,itunit,2)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'UNDER_OVER')then
           call getfile(ierr,cline,atemp,left_word(2),right_word(2))
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,57) trim(aline),trim(sString_g)
57           format('cannot read UNDER_OVER from line ',a,' of file ',a)
             go to 9800
           end if
           call casetrans(atemp,'lo')
           if(atemp(1:5).eq.'under')then
             iuo=0
           else if(atemp(1:5).eq.'over')then
             iuo=1
           else
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,58) trim(aline),trim(sString_g)
58           format('UNDER_OVER must be "under" or "over" at line ',a,' of file ',a)
             go to 9800
           end if
           call addquote(atemp,sString_g)
           write(*,59) trim(sString_g)
           write(LU_REC,59) trim(sString_g)
59         format(t5,'UNDER_OVER ',a)
         else if(aoption.eq.'FLOW')then
           iflow=iflow+1
           if(iflow.gt.MAXTEMPDURFLOW)then
             call num2char(MAXTEMPDURFLOW,aline)
             write(amessage,30) trim(aline), trim(CurrentBlock_g)
30           format('a maximum of ',a,' FLOWs are allowed in an ',a,' block.')
             go to 9800
           end if
           call char2num(ierr,cline(left_word(2):right_word(2)),rtemp)
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,120) trim(aline),trim(sString_g)
120          format('cannot read flow from line ',a,' of file ',a)
             go to 9800
           end if
           write(*,130) cline(left_word(2):right_word(2))
           write(LU_REC,130) cline(left_word(2):right_word(2))
130        format(t5,'FLOW ',a)
           tempdtable_g%flow(iflow)=rtemp
         else if(aoption.eq.'DELAY')then
           if(iflow.eq.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,320) trim(aline),trim(sString_g)
320          format('DELAY not preceeded by FLOW at line ',a,' of file ',a)
             go to 9800
           end if
           if(tempdtable_g%tdelay(iflow).gt.-1.0e36)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,330) trim(aline),trim(sString_g)
330          format('more than one DELAY associated with FLOW at line ',a,' of file ',a)
             go to 9800
           end if
           call char2num(ierr,cline(left_word(2):right_word(2)),rtemp)
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,335) trim(aline),trim(sString_g)
335          format('cannot read time delay from line ',a,' of file ',a)
             go to 9800
           end if
           if(rtemp.lt.0.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,336) trim(aline),trim(sString_g)
336          format('time delay cannot be negative at line ',a,' of file ',a)
             go to 9800
           end if
           write(*,340) cline(left_word(2):right_word(2))
           write(LU_REC,340) cline(left_word(2):right_word(2))
340        format(t5,'DELAY ',a)
           tempdtable_g%tdelay(iflow)=rtemp
         else if(aoption.eq.'END')then
           go to 200
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

! -- The block has been read; now it is checked for absences.

       if(iseries.eq.0)then
         write(amessage,210) trim(CurrentBlock_g)
210      format('no SERIES_NAME keyword provided in ',a,' block.')
         go to 9800
       end if
       if(aname.eq.' ')then
         write(amessage,230) trim(CurrentBlock_g)
230      format('no NEW_E_TABLE_NAME keyword provided in ',a,' block.')
         go to 9800
       end if
       if(itunit.eq.0)then
         write(amessage,218) trim(CurrentBlock_g)
218      format('no EXCEEDENCE_TIME_UNITS keyword provided in ',a,' block.')
         go to 9800
       end if
       if(icontext.eq.0)then
         write(amessage,220) trim(CurrentBlock_g)
220      format('no CONTEXT keyword(s) provided in ',a,' block.')
         go to 9800
       end if
       if(iflow.eq.0)then
         write(amessage,225) trim(CurrentBlock_g)
225      format('no FLOW keywords provided in ',a,' block.')
         go to 9800
       end if
       do i=1,iflow
         if (tempdtable_g%tdelay(i).gt.-1.0e36) go to 360
       end do
       go to 400
360    do i=1,iflow
         if(tempdtable_g%tdelay(i).lt.-1.0e36)then
           write(amessage,370) trim(CurrentBlock_g)
370        format('if any FLOW is associated with a DELAY, than all flows must be associated ',  &
           'with a DELAY in ',a,' block')
           go to 9800
         end if
       end do
400    continue
       if(series_g(iseries)%nterm.eq.1)then
         write(amessage,250) trim(series_g(iseries)%name)
250      format('cannot calculate exceedence times because time series "',a,   &
         '" has only one term.')
         go to 9800
       end if

! -- Space is now allocated in a non-temporary E_TABLE.

       do id=1,MAXDTABLE
         if(.not.dtable_g(id)%active) go to 380
       end do
       write(amessage,390)
390    format('no more e_tables available for data storage - increase MAXDTABLE and ', &
       'recompile program.')
       go to 9800
380    continue
       dtable_g(id)%active=.true.
       dtable_g(id)%name=aname
       dtable_g(id)%series_name=series_g(iseries)%name
       dtable_g(id)%nterm=iflow
       allocate(dtable_g(id)%flow(iflow),dtable_g(id)%time(iflow),  &
                dtable_g(id)%tdelay(iflow),stat=ierr)
       if(ierr.ne.0)then
         write(amessage,395)
395      format('cannot allocate memory for storage of e_table data.')
         go to 9800
       end if
       do i=1,iflow
         dtable_g(id)%flow(i)=tempdtable_g%flow(i)
         if(tempdtable_g%tdelay(i).lt.-1.0e36)then
           dtable_g(id)%tdelay(i)=0.0
         else
           dtable_g(id)%tdelay(i)=tempdtable_g%tdelay(i)
         end if
       end do

       if(itunit.eq.1)then
         fac=1.0
         dtable_g(id)%time_units='secs'
       else if(itunit.eq.2)then
         fac=1.0/60.0
         dtable_g(id)%time_units='mins'
       else if(itunit.eq.3)then
         fac=1.0/3600.0
         dtable_g(id)%time_units='hrs'
       else if(itunit.eq.4)then
         fac=1.0/86400.0
         dtable_g(id)%time_units='days'
       else if(itunit.eq.5)then
         fac=1.0/86400.0/(356.25/12.0)
         dtable_g(id)%time_units='mths'
       else
         fac=1.0/86400.0/365.25
         dtable_g(id)%time_units='yrs'
       end if
       if((iuo.eq.-999).or.(iuo.eq.1))then
         dtable_g(id)%under_over=1
         iuo=1
       else
         dtable_g(id)%under_over=0
         iuo=0
       end if

! -- Durations are now calculated for each flow.

       nnterm=series_g(iseries)%nterm
       do i=1,iflow
         timedelay=dtable_g(id)%tdelay(i)/fac
         duration=0.0
         accumulation=0.0
         fflow=dtable_g(id)%flow(i)
         vval=series_g(iseries)%val(1)
         ndays=series_g(iseries)%days(1)
         nsecs=series_g(iseries)%secs(1)
         if(iuo.eq.1)then
           if(vval.ge.fflow)then
             on=.true.
           else
             on=.false.
           end if
         else
           if(vval.le.fflow)then
             on=.true.
           else
             on=.false.
           end if
         end if
         do j=2,nnterm
           oldon=on
           oldvval=vval
           oldndays=ndays
           oldnsecs=nsecs
           vval=series_g(iseries)%val(j)
           ndays=series_g(iseries)%days(j)
           nsecs=series_g(iseries)%secs(j)
           if(iuo.eq.1)then
             if(vval.ge.fflow)then
               on=.true.
             else
               on=.false.
             end if
           else
             if(vval.le.fflow)then
               on=.true.
             else
               on=.false.
             end if
           end if
           if((on).and.(oldon))then
!             duration=duration+timediff
             timediff=float(ndays-oldndays)*86400.0+float(nsecs-oldnsecs)
             accumulation=accumulation+timediff
           else if((on).and.(.not.oldon))then
             timediff=float(ndays-oldndays)*86400.0+float(nsecs-oldnsecs)
             accumulation=timediff*(vval-fflow)/(vval-oldvval)
!             duration=duration+timediff*(vval-fflow)/(vval-oldvval)
           else if((oldon).and.(.not.on))then
             timediff=float(ndays-oldndays)*86400.0+float(nsecs-oldnsecs)
             accumulation=accumulation+timediff*(oldvval-fflow)/(oldvval-vval)
             duration=duration+max(0.0,accumulation-timedelay)
             accumulation=0.0
!             duration=duration+timediff*(oldvval-fflow)/(oldvval-vval)
           end if
         end do
         duration=duration+max(0.0,accumulation-timedelay)
         dtable_g(id)%time(i)=duration*fac
       end do

! -- The total time encompassed by the time series is now calculated.

       oldndays=series_g(iseries)%days(1)
       oldnsecs=series_g(iseries)%secs(1)
       ndays=series_g(iseries)%days(nnterm)
       nsecs=series_g(iseries)%secs(nnterm)
       dtable_g(id)%total_time=(float(ndays-oldndays)*86400.0+float(nsecs-oldnsecs))*fac

       write(*,440) trim(aname)
       write(LU_REC,440) trim(aname)
440    format(t5,'Exceedence times calculated and stored in e_table "',a,'".')
       return

9000   call num2char(ILine_g,aline)
       call addquote(sInfile_g,sString_g)
       write(amessage,9010) trim(aline), trim(sString_g)
9010   format('cannot read line ',a,' of TSPROC input file ',a)
       go to 9800
9100   continue
       call addquote(sInfile_g,sString_g)
       write(amessage,9110) trim(sString_g),trim(CurrentBlock_g)
9110   format('unexpected end encountered to TSPROC input file ',a,' while ', &
       ' reading ',a,' block.')
       go to 9800
9800   call write_message(leadspace='yes',error='yes')
       call write_message(iunit=LU_REC,leadspace='yes')
       ifail=1

       return

end subroutine time_duration



subroutine displace(ifail)

! -- Subroutine DISPLACE moves a time series by a user-supplied number of time increments.

       use tsp_data_structures
       use tsp_utilities
       use tsp_command_processors

       implicit none

       integer, intent(out)   :: ifail

       integer ierr,icontext,iseries,lag,itemp,nsterm,ilags,j,nsecs,ndays, &
       dd,mm,yy,hh,nn,ss,i,ixcon
       real fill,rtemp
       character (len=iTSNAMELENGTH) :: aname
       character*15 aline
       character*25 aoption
       character*25 acontext(MAXCONTEXT)

       ifail=0
       CurrentBlock_g='SERIES_DISPLACE'

       write(*,10) trim(CurrentBlock_g)
       write(LU_REC,10) trim(CurrentBlock_g)
10     format(/,' Processing ',a,' block....')

       icontext=0
       iseries=0
       aname=' '
       lag=-9999
       fill=-1.1e36
       ixcon=0

! -- The SERIES_DISPLACE block is first parsed.

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
         else if(aoption.eq.'SERIES_NAME')then
           call get_series_name(ierr,iseries,'SERIES_NAME')
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
         else if(aoption.eq.'LAG_INCREMENT')then
           call get_keyword_value(ierr,1,lag,rtemp,'LAG_INCREMENT')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'FILL_VALUE')then
           call get_keyword_value(ierr,2,itemp,fill,'FILL_VALUE')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'END')then
           go to 200
         else
           call num2char(ILine_g,aline)
           call addquote(sInfile_g,sString_g)
           write(amessage,90) trim(aoption),trim(CurrentBlock_g),trim(aline),trim(sString_g)
90         format('unexpected keyword - "',a,'" in ',a,' block at line ',a, &
           ' of file ',a)
           go to 9800
         end if
       end do

! -- The block has been read; now it is checked for correctness.

200    continue
       if(iseries.eq.0)then
         write(amessage,210) trim(CurrentBlock_g)
210      format('no SERIES_NAME keyword provided in ',a,' block.')
         go to 9800
       end if
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
       if(lag.eq.-9999)then
         write(amessage,225) trim(CurrentBlock_g)
225      format('no LAG_INCREMENT keyword provided in ',a,' block.')
         go to 9800
       end if
       if(fill.lt.-1.0e36)then
         write(amessage,240) trim(CurrentBlock_g)
240      format('no FILL_VALUE keyword provided in ',a,' block.')
         go to 9800
       end if

! -- The DISPLACE operation can only be performed if the input time series has equal
!    increments. This is now tested.

       nsterm=series_g(iseries)%nterm
       if(nsterm.lt.abs(lag)+1)then
         call num2char(nsterm,aline)
         write(amessage,250) trim(series_g(iseries)%name),trim(aline)
250      format('series "',a,'" has only ',a,' terms. This is insufficient to perform ', &
         'the requested displacement operation.')
         go to 9800
       end if
       if(nsterm.gt.2)then
         ilags=(series_g(iseries)%days(2)-series_g(iseries)%days(1))*86400+   &
                series_g(iseries)%secs(2)-series_g(iseries)%secs(1)
         do j=2,nsterm-1
           nsecs=series_g(iseries)%secs(j)+ilags
           ndays=series_g(iseries)%days(j)
260        if(nsecs.ge.86400)then
             ndays=ndays+1
             nsecs=nsecs-86400
             go to 260
           end if
           if((nsecs.ne.series_g(iseries)%secs(j+1)).or.   &
              (ndays.ne.series_g(iseries)%days(j+1)))then
               call newdate(series_g(iseries)%days(j),1,1,1970,dd,mm,yy)
               nsecs=series_g(iseries)%secs(j)
               hh=nsecs/3600
               nn=(nsecs-hh*3600)/60
               ss=nsecs-hh*3600-nn*60
               if(datespec.eq.1) then
                 write(amessage,280) trim(series_g(iseries)%name),dd,mm,yy,hh,nn,ss
               else
                 write(amessage,280) trim(series_g(iseries)%name),mm,dd,yy,hh,nn,ss
               end if
280            format('time interval between terms in time series "',a,'" is not ', &
               'constant. The first discrepancy occurs following the sample taken on ',  &
               i2.2,'/',i2.2,'/',i4,' at ',i2.2,':',i2.2,':',i2.2)
               go to 9800
           end if
         end do
       end if

! -- Space for a new series is allocated.

       do i=1,MAXSERIES
         if(.not.series_g(i)%active) go to 515
       end do
       write(amessage,510)
510    format('no more time series available for data storage - increase MAXSERIES and ', &
       'recompile program, or erase a series using an ERASE_SERIES block.')
       go to 9800

515    continue
       allocate(series_g(i)%days(nsterm),series_g(i)%secs(nsterm),  &
       series_g(i)%val(nsterm),stat=ierr)
       if(ierr.ne.0)then
         write(amessage,550)
550      format('cannot allocate memory for another time series.')
         go to 9800
       end if
       series_g(i)%active=.true.
       series_g(i)%name=aname
       series_g(i)%nterm=nsterm
       series_g(i)%type='ts'
       do j=1,nsterm
         series_g(i)%days(j)=series_g(iseries)%days(j)
       end do
       do j=1,nsterm
         series_g(i)%secs(j)=series_g(iseries)%secs(j)
       end do
       if(lag.eq.0)then
         do j=1,nsterm
           series_g(i)%val(j)=series_g(iseries)%val(j)
         end do
       else if(lag.gt.0)then
         do j=1+lag,nsterm
           series_g(i)%val(j)=series_g(iseries)%val(j-lag)
         end do
         do j=1,lag
           series_g(i)%val(j)=fill
         end do
       else if(lag.lt.0)then
         lag=-lag
         do j=1,nsterm-lag
           series_g(i)%val(j)=series_g(iseries)%val(j+lag)
         end do
         do j=nsterm-lag+1,nsterm
           series_g(i)%val(j)=fill
         end do
       end if

       write(*,580) trim(aname)
       write(LU_REC,580) trim(aname)
580    format(t5,'Series "',a,'" successfully calculated.')
       return

9000   call num2char(ILine_g,aline)
       call addquote(sInfile_g,sString_g)
       write(amessage,9010) trim(aline), trim(sString_g)
9010   format('cannot read line ',a,' of TSPROC input file ',a)
       go to 9800
9100   continue
       call addquote(sInfile_g,sString_g)
       write(amessage,9110) trim(sString_g),trim(CurrentBlock_g)
9110   format('unexpected end encountered to TSPROC input file ',a,' while ', &
       ' reading ',a,' block.')
       go to 9800

9800   call write_message(leadspace='yes',error='yes')
       call write_message(iunit=LU_REC,leadspace='yes')
       ifail=1

       return

end subroutine displace

end module tsp_time_series_processors
