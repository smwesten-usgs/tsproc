module tsp_input

  use tsp_data_structures
  use tsp_utilities
  use tsp_command_processors

  implicit none

contains

      subroutine get_mul_series_gsflow_gage(ifail)

! -- Subroutine GET_MUL_SERIES_GSFLOW_GAGE reads multiple series fron a GSFLOW gage file.
       integer, intent(out)   :: ifail

       integer dd1,mm1,yy1,hh1,nn1,ss1,dd2,mm2,yy2,hh2,nn2,ss2,ierr, &
       icontext,i,iunit,begdays,begsecs,enddays,endsecs,jline,j, &
       jseries,kseries,ixcon,k,isite,iseriesname,iterm
       integer ddr,mmr,yyr,hhr,nnr,ssr,refdays,refsecs,itemp,jfail, &
       ncol,icount,jcount,timecol,maxcol,ddays,dsecs
       integer jjseries(MAXSERIESREAD)
       integer datcol(MAXSERIESREAD)
       double precision dtime, time_per_day
       character*15 aline
       character*25 aoption
       character*200 afile
       character*30 atemp
       character*30 site(MAXSERIESREAD)
       character*25 acontext(MAXCONTEXT)
       character (len=iTSNAMELENGTH) :: aname(MAXSERIESREAD)

!       make sure that i have done the right thing changing MAXSERIES to MAXSERIESREAD
!       if so, do it in other subroutines as well that read multiple series.

       ifail=0
       CurrentBlock_g='GET_MUL_SERIES_GSFLOW_GAGE'

       write(*,10) trim(CurrentBlock_g)
       write(LU_REC,10) trim(CurrentBlock_g)
10     format(/,' Processing ',a,' block....')

       afile=' '
       icontext=0
       ixcon=0
       yy1=-9999
       hh1=-9999
       yy2=-9999
       hh2=-9999
       yyr=-9999
       hhr=-9999
       isite=1
       iseriesname=0
       jseries=0
       kseries=0
       time_per_day=1.0d0

! -- The GET_MUL_SERIES_GSFLOW_GAGE block is first parsed.

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
         if(aoption.eq.'FILE')then
           call get_file_name(ierr,afile)
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
         else if(aoption.eq.'MODEL_REFERENCE_DATE')then
           call get_date(ierr,ddr,mmr,yyr,'MODEL_REFERENCE_DATE')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'MODEL_REFERENCE_TIME')then
           call get_time(ierr,hhr,nnr,ssr,'MODEL_REFERENCE_TIME')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'TIME_UNITS_PER_DAY')then
           call get_keyword_value_double(ierr,2,itemp,time_per_day,'TIME_UNITS_PER_DAY')
           if(ierr.ne.0) go to 9800
           if(time_per_day.le.0.0d0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,65) trim(aline),trim(sString_g)
65           format('time units per day must be positive at line ',a,' of file ',a)
             go to 9800
           end if
         else if(aoption.eq.'DATA_TYPE')then
           if(isite.eq.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,42) trim(CurrentBlock_g),trim(aline),trim(sString_g)
42           format('DATA_TYPE keyword in wrong position in ',a,' block at line ',a, &
             ' of file ',a)
             go to 9800
           end if
           jseries=jseries+1
45         kseries=kseries+1
           if(kseries.gt.MAXSERIES)then
             write(amessage,44) trim(CurrentBlock_g)
44           format('too many new series cited in ',a,' block. Increase MAXSERIES ', &
             'and re-compile program.')
             go to 9800
           end if
           if(series_g(kseries)%active) go to 45
           jjseries(jseries)=kseries
           call getfile(ierr,cline,site(jseries),left_word(2),right_word(2))
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,57) trim(aline),trim(sString_g)
57           format('cannot read DATA_TYPE string from line ',a,' of file ',a)
             go to 9800
           end if
           call casetrans(site(jseries),'lo')
           call addquote(site(jseries),sString_g)
           write(*,46) trim(sString_g)
           write(LU_REC,46) trim(sString_g)
46         format(t5,'DATA_TYPE ',a)
           isite=0
           iseriesname=1
           aname(jseries)=' '
         else if(aoption.eq.'NEW_SERIES_NAME')then
           if(iseriesname.eq.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,43) trim(CurrentBlock_g),trim(aline),trim(sString_g)
43           format('NEW_SERIES_NAME keyword can only follow a DATA_TYPE ',  &
             'keyword in ',a,' block at line ',a,' of file ',a)
             go to 9800
           end if
           call get_new_series_name(ierr,aname(jseries))
           if(ierr.ne.0) go to 9800
           if(jseries.gt.1)then
             do j=1,jseries-1
               if(aname(jseries).eq.aname(j))then
                 write(amessage,146) trim(aname(jseries)),trim(CurrentBlock_g)
146              format('SERIES_NAME "',a,'" used more than once in ',a,' block.')
                 go to 9800
               end if
             end do
           end if
           iseriesname=0
           isite=1
         else if(aoption.eq.'CONTEXT')then
           if(ixcon.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,41) trim(aline),trim(sString_g)
41           format('Context keyword in incorrect location at line ',a,' of file ',a)
             go to 9800
           end if
           call get_context(ierr,icontext,acontext)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'END')then
           if(iseriesname.eq.1)then
             write(amessage,48) trim(CurrentBlock_g)
48           format(a,' block END encountered before finding ', &
             'expected NEW_SERIES_NAME keyword.')
             go to 9800
           end if
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
       if(afile.eq.' ')then
         call addquote(sInfile_g,sString_g)
         write(amessage,110) trim(CurrentBlock_g),trim(sString_g)
110      format('no FILE keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       if(icontext.eq.0)then
         call addquote(sInfile_g,sString_g)
         write(amessage,122) trim(CurrentBlock_g),trim(sString_g)
122      format('no CONTEXT keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       if(jseries.eq.0)then
         call addquote(sInfile_g,sString_g)
         write(amessage,125) trim(CurrentBlock_g),trim(sString_g)
125      format('no DATA_TYPE keyword provided in ',a,' block in file ',a)
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
       if(jseries.gt.1)then
         do j=2,jseries
           do i=1,j-1
             if(site(j).eq.site(i))then
               call addquote(sInfile_g,sString_g)
               write(amessage,401) trim(CurrentBlock_g),trim(sString_g)
401            format('two series possess the same DATA_TYPE name ',  &
               'in ',a,' block of file ',a)
               go to 9800
             end if
           end do
         end do
       end if
       if(yyr.eq.-9999)then
         call addquote(sInfile_g,sString_g)
         write(amessage,402) trim(CurrentBlock_g),trim(sString_g)
402      format('no MODEL_REFERENCE_DATE keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       if(hhr.eq.-9999)then
         call addquote(sInfile_g,sString_g)
         write(amessage,403) trim(CurrentBlock_g),trim(sString_g)
403      format('no MODEL_REFERENCE_TIME keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       refdays=numdays(1,1,1970,ddr,mmr,yyr)
       refsecs=numsecs(0,0,0,hhr,nnr,ssr)
404    continue
       if(refsecs.ge.86400)then
         refsecs=refsecs-86400
         refdays=refdays+1
         go to 404
       end if

! -- There appear to be no errors in the block, so now it is processed.

       call addquote(afile,sString_g)
       write(*,179) trim(sString_g)
       write(LU_REC,179) trim(sString_g)
179    format(t5,'Reading GSFLOW gage file ',a,'....')
       iunit=nextunit()
       open(unit=iunit,file=afile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,180) trim(sString_g),trim(CurrentBlock_g)
180      format('cannot open file ',a,' cited in ',a,' block.')
         go to 9800
       end if

! -- The file is perused a first time to find out the storage requirements of the
!    time series.

       jline=1
       read(iunit,'(a)',err=9200,end=9200) cline
       jline=jline+1
       read(iunit,'(a)',err=9200,end=9200) cline
       call remchar(cline,'"')
       call casetrans(cline,'lo')
       cline=adjustl(cline)
       if(cline(1:5).ne.'data:') go to 9300
       cline=cline(6:)
! -- Establish the exact number of columns.
       do i=1,NUM_WORD_DIM
         call linesplit(jfail,i)
         if(jfail.ne.0) go to 380
       end do
       write(amessage,371) trim(sString_g)
371    format('too many data columns in file ',a,'. Increase NUM_WORD_DIM and ',  &
       're-compile program.')
       go to 9800
380    ncol=i-1
! -- Read the column headers and identify which columns we need to read.
       datcol=0               ! an array
       icount=0
       do i=1,ncol
         atemp=cline(left_word(i):right_word(i))
         do j=1,jseries
           if(site(j).eq.atemp)then
             datcol(j)=i
             icount=icount+1
             if(icount.lt.jseries)then
               go to 390
             else
               go to 400
             end if
           end if
         end do
390      continue
       end do
400    continue
       if(icount.lt.jseries)then
         do i=1,jseries
           if(datcol(i).eq.0)then
             write(amessage,410) trim(site(i)),trim(sString_g)
410          format('data type column header "',a,'" not found in file ',a,'.')
             go to 9800
           end if
         end do
       end if
       do i=1,ncol
         atemp=cline(left_word(i):right_word(i))
         if(atemp.eq.'time')then
           timecol=i
           go to 421
         end if
       end do
       write(amessage,420) trim(sString_g)
420    format('no "time" data header found in file ',a,'.')
       go to 9800
421    continue

! -- The file is now read a first time in order to establish memory requirements.

       icount=0
       jcount=0
       maxcol=0
       do i=1,jseries
         if(datcol(i).gt.maxcol)maxcol=datcol(i)
       end do
       do
         jline=jline+1
         read(iunit,'(a)',err=9200,end=450) cline
         if(cline.eq.' ') cycle
         call linesplit(jfail,timecol)
         if(jfail.ne.0)then
           call num2char(jline,aline)
           write(amessage,422) trim(aline),trim(sString_g)
422        format('insufficient entries on line ',a,' of file ',a)
           go to 9800
         end if
         dtime=char2double(jfail,timecol)
         if(jfail.ne.0)then
           call num2char(jline,aline)
           write(amessage,430) trim(aline),trim(sString_g)
430        format('cannot read time from line ',a,' of file ',a)
           go to 9800
         end if
         jcount=jcount+1
         dtime=dtime/time_per_day
         ddays=floor(dtime)
         dsecs=nint((dtime-dble(ddays))*86400.0d0)
         ddays=ddays+refdays
         dsecs=dsecs+refsecs
440      continue
         if(dsecs.ge.86400)then
           ddays=ddays+1
           dsecs=dsecs-86400
           go to 440
         end if
         if((ddays.lt.begdays).or.((ddays.eq.begdays).and.(dsecs.lt.begsecs))) cycle
         if((ddays.gt.enddays).or.((ddays.eq.enddays).and.(dsecs.gt.endsecs))) cycle
         icount=icount+1
       end do
450    continue
       if(jcount.eq.0)then
         write(amessage,460) trim(sString_g)
460      format('no data is present within file ',a)
         go to 9800
       end if
       if(icount.eq.0)then
         write(amessage,470) trim(sString_g)
470      format('no data is present within file ',a,' within requested date/time limits.')
         go to 9800
       end if
       iterm=icount

! -- Space is now allocated for the new series.

       do j=1,jseries
         k=jjseries(j)
         allocate(series_g(k)%days(iterm),series_g(k)%secs(iterm),  &
         series_g(k)%val(iterm),stat=ierr)
         if(ierr.ne.0)then
           write(amessage,550)
550        format('cannot allocate memory for another time series.')
           go to 9800
         end if
         series_g(k)%active=.true.
         series_g(k)%name=aname(j)
         series_g(k)%type='ts'
         series_g(k)%nterm=iterm
       end do

! -- The file is now read a second time and the data is imported.

       rewind(unit=iunit,iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,370) trim(sString_g)
370      format('cannot re-wind GSFLOW output file ',a)
         go to 9800
       end if
       jline=1
       read(iunit,'(a)',err=9200,end=9200) cline
       jline=jline+1
       read(iunit,'(a)',err=9200,end=9200) cline

       icount=0
       do
         jline=jline+1
         read(iunit,'(a)',err=9200,end=700) cline
         if(cline.eq.' ') cycle
         call linesplit(ifail,maxcol)
         if(ifail.ne.0)then
           call num2char(jline,aline)
           write(amessage,372) trim(aline),trim(sString_g)
372        format('insufficient entries on line ',a,' of file ',a)
           go to 9800
         end if
         dtime=char2double(ifail,timecol)
         dtime=dtime/time_per_day
         ddays=floor(dtime)
         dsecs=nint((dtime-dble(ddays))*86400.0d0)
         ddays=ddays+refdays
         dsecs=dsecs+refsecs
640      continue
         if(dsecs.ge.86400)then
           ddays=ddays+1
           dsecs=dsecs-86400
           go to 640
         end if
         if((ddays.lt.begdays).or.((ddays.eq.begdays).and.(dsecs.lt.begsecs))) cycle
         if((ddays.gt.enddays).or.((ddays.eq.enddays).and.(dsecs.gt.endsecs))) go to 700
         icount=icount+1
         do i=1,jseries
           k=jjseries(i)
           series_g(k)%days(icount)=ddays
           series_g(k)%secs(icount)=dsecs
           j=datcol(i)
           series_g(k)%val(icount)=char2real(jfail,j)
           if(jfail.ne.0)then
             call num2char(jline,aline)
             write(amessage,650) trim(site(i)),trim(aline),trim(sString_g)
650          format('cannot read data value for data type "',a,'" from line ',a,  &
             ' of file ',a)
             go to 9800
           end if
         end do
       end do
700    continue

       do j=1,jseries
         write(*,860) trim(aname(j)),trim(sString_g)
         write(LU_REC,860) trim(aname(j)),trim(sString_g)
860      format(t5,'Series "',a,'" successfully imported from file ',a)
       end do

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
9200   call num2char(jline,aline)
       call addquote(afile,sString_g)
       write(amessage,9210) trim(aline),trim(sString_g)
9210   format('unable to read line ',a,' of file ',a)
       go to 9800
9300   call addquote(afile,sString_g)
       write(amessage,9310) trim(sString_g)
9310   format('unexpected components in header lines to GSFLOW gage file ',a,'.')
       go to 9800

9800   call write_message(leadspace='yes',error='yes')
       call write_message(iunit=LU_REC,leadspace='yes')
       ifail=1

9900   close(unit=iunit,iostat=ierr)
       return


end subroutine get_mul_series_gsflow_gage






subroutine get_mul_series_statvar(ifail)

       implicit none
! -- Subroutine GET_MUL_SERIES_STATVAR reads multiple series from an MMS/GSFLOW statvar file.
       integer, intent(out)   :: ifail

       integer dd1,mm1,yy1,hh1,nn1,ss1,dd2,mm2,yy2,hh2,nn2,ss2,ierr, &
       icontext,i,iunit,begdays,begsecs,enddays,endsecs,jline,j, &
       jseries,kseries,ixcon,k,isite,iseriesname,iterm
       integer itemp,jfail,icount,jcount,ddays,dsecs,ilocid,nstatseries,   &
       ibeg,iend
       integer modday,yys,mms,dds,hhs,nns,sss
       integer jjseries(MAXSERIESREAD)
       integer datcol(MAXSERIESREAD),locid(MAXSERIESREAD)
       real, allocatable :: rval(:)
       real rtemp
       character*15 aline,aadate,aatime
       character*25 aoption
       character*100 varname
       character*200 afile
       character*30 atemp
       character*50 site(MAXSERIESREAD)
       character*25 acontext(MAXCONTEXT)
       character (len=iTSNAMELENGTH) :: aname(MAXSERIESREAD)

!       make sure that i have done the right thing changing MAXSERIES to MAXSERIESREAD
!       if so, do it in other subroutines as well that read multiple series.

       ifail=0
       CurrentBlock_g='GET_MUL_SERIES_STATVAR'

       write(*,10) trim(CurrentBlock_g)
       write(LU_REC,10) trim(CurrentBlock_g)
10     format(/,' Processing ',a,' block....')

       afile=' '
       icontext=0
       ixcon=0
       yy1=-9999
       hh1=-9999
       yy2=-9999
       hh2=-9999
       ilocid=0
       isite=1
       iseriesname=0
       jseries=0
       kseries=0

! -- The GET_MUL_SERIES_STATVAR block is first parsed.

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
         if(aoption.eq.'FILE')then
           call get_file_name(ierr,afile)
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
         else if(aoption.eq.'VARIABLE_NAME')then
           if(isite.eq.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,42) trim(CurrentBlock_g),trim(aline),trim(sString_g)
42           format('VARIABLE_NAME keyword in wrong position in ',a,' block at line ',a, &
             ' of file ',a)
             go to 9800
           end if
           jseries=jseries+1
45         kseries=kseries+1
           if(kseries.gt.MAXSERIES)then
             write(amessage,44) trim(CurrentBlock_g)
44           format('too many new series cited in ',a,' block. Increase MAXSERIES ', &
             'and re-compile program.')
             go to 9800
           end if
           if(jseries.gt.MAXSERIESREAD)then
             write(amessage,461) trim(CurrentBlock_g)
461          format('too many new series cited in ',a,' block. Increase MAXSERIESREAD ', &
             'and re-compile program.')
             go to 9800
           end if

           if(series_g(kseries)%active) go to 45
           jjseries(jseries)=kseries
           call getfile(ierr,cline,site(jseries),left_word(2),right_word(2))
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,57) trim(aline),trim(sString_g)
57           format('cannot read VARIABLE_NAME string from line ',a,' of file ',a)
             go to 9800
           end if
           call casetrans(site(jseries),'lo')
           call addquote(site(jseries),sString_g)
           write(*,46) trim(sString_g)
           write(LU_REC,46) trim(sString_g)
46         format(t5,'VARIABLE_NAME ',a)
           isite=0
           ilocid=1
         else if(aoption.eq.'LOCATION_ID')then
           if(ilocid.eq.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,441) trim(CurrentBlock_g),trim(aline),trim(sString_g)
441          format('LOCATION_ID keyword in wrong position in ',a,' block at line ',a, &
             ' of file ',a)
             go to 9800
           end if
           call get_keyword_value(ierr,1,locid(jseries),rtemp,'LOCATION_ID')
           if(ierr.ne.0) go to 9800
           iseriesname=1
           aname(jseries)=' '
           ilocid=0
         else if(aoption.eq.'NEW_SERIES_NAME')then
           if(iseriesname.eq.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,43) trim(CurrentBlock_g),trim(aline),trim(sString_g)
43           format('NEW_SERIES_NAME keyword can only follow a LOCATION_ID ',  &
             'keyword in ',a,' block at line ',a,' of file ',a)
             go to 9800
           end if
           call get_new_series_name(ierr,aname(jseries))
           if(ierr.ne.0) go to 9800
           if(jseries.gt.1)then
             do j=1,jseries-1
               if(aname(jseries).eq.aname(j))then
                 write(amessage,146) trim(aname(jseries)),trim(CurrentBlock_g)
146              format('SERIES_NAME "',a,'" used more than once in ',a,' block.')
                 go to 9800
               end if
             end do
           end if
           iseriesname=0
           isite=1
         else if(aoption.eq.'CONTEXT')then
           if(ixcon.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,41) trim(aline),trim(sString_g)
41           format('Context keyword in incorrect location at line ',a,' of file ',a)
             go to 9800
           end if
           call get_context(ierr,icontext,acontext)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'END')then
           if(iseriesname.eq.1)then
             write(amessage,48) trim(CurrentBlock_g)
48           format(a,' block END encountered before finding ', &
             'expected NEW_SERIES_NAME keyword.')
             go to 9800
           end if
           if(ilocid.eq.1)then
             write(amessage,49) trim(CurrentBlock_g)
49           format(a,' block END encountered before finding ', &
             'expected LOCATION_ID keyword.')
             go to 9800
           end if
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
       if(afile.eq.' ')then
         call addquote(sInfile_g,sString_g)
         write(amessage,110) trim(CurrentBlock_g),trim(sString_g)
110      format('no FILE keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       if(icontext.eq.0)then
         call addquote(sInfile_g,sString_g)
         write(amessage,122) trim(CurrentBlock_g),trim(sString_g)
122      format('no CONTEXT keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       if(jseries.eq.0)then
         call addquote(sInfile_g,sString_g)
         write(amessage,125) trim(CurrentBlock_g),trim(sString_g)
125      format('no VARIABLE_NAME keyword provided in ',a,' block in file ',a)
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
       if(jseries.gt.1)then
         do j=2,jseries
           do i=1,j-1
             if((site(j).eq.site(i)).and.(locid(j).eq.locid(i)))then
               call addquote(sInfile_g,sString_g)
               write(amessage,401) trim(CurrentBlock_g),trim(sString_g)
401            format('two series possess the same VARIABLE_NAME ',  &
               'and LOCATION_ID in ',a,' block of file ',a)
               go to 9800
             end if
           end do
         end do
       end if

! -- There appear to be no errors in the block, so now it is processed.

       call addquote(afile,sString_g)
       write(*,179) trim(sString_g)
       write(LU_REC,179) trim(sString_g)
179    format(t5,'Reading STATVAR file ',a,'....')
       iunit=nextunit()
       open(unit=iunit,file=afile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,180) trim(sString_g),trim(CurrentBlock_g)
180      format('cannot open file ',a,' cited in ',a,' block.')
         go to 9800
       end if

! -- The header to the file is perused in its entirety.

       jline=1
       read(iunit,'(a)',err=9200,end=9200) cline
       if(cline.eq.' ') go to 9350
       call linesplit(ifail,1)
       nstatseries=char2int(jfail,1)
       if(jfail.ne.0) go to 9350
       if(nstatseries.le.0) go to 9350
       datcol=0               ! an array
       icount=0
       do i=1,nstatseries
         jline=jline+1
         read(iunit,'(a)',err=9200,end=9200) cline
         ibeg=1
         iend=len_trim(cline)
         call getfile(jfail,cline,varname,ibeg,iend)
         if(jfail.ne.0)then
           call num2char(jline,aline)
           write(amessage,370) trim(aline),trim(sString_g)
370        format('cannot read varable name from line ',a,' of file ',a)
           go to 9800
         end if
         call casetrans(varname,'lo')
         ibeg=iend+1
         cline=cline(ibeg:)
         call linesplit(jfail,1)
         if(jfail.ne.0) then
           call num2char(jline,aline)
           write(amessage,380) trim(aline),trim(sString_g)
380        format('cannot read location id from line ',a,' of file ',a,'.')
           go to 9800
         end if
         itemp=char2int(jfail,1)
         if(jfail.ne.0)then
           call num2char(jline,aline)
           write(amessage,380) trim(aline),trim(sString_g)
           go to 9800
         end if
         do j=1,jseries
           if((site(j).eq.varname).and.(itemp.eq.locid(j))) then
             datcol(j)=i
             icount=icount+1
             if(icount.lt.jseries)then
               go to 390
             else
               go to 400
             end if
           end if
         end do
390      continue
       end do
400    continue
       if(icount.lt.jseries)then
         do i=1,jseries
           if(datcol(i).eq.0)then
             call num2char(locid(i),atemp)
             write(amessage,410) trim(site(i)),trim(atemp),trim(sString_g)
410          format('VARIABLE_NAME "',a,'", LOCATION_ID ',a,' not found in file ',a,'.')
             go to 9800
           end if
         end do
       end if

! -- The file is now read a first time in order to establish memory requirements.
       allocate(rval(nstatseries),stat=ierr)
       if(ierr.ne.0)then
         write(amessage,420)
420      format('cannot allocate memory for temporary array storage.')
         go to 9800
       end if
       icount=0
       jcount=0
       do
         read(iunit,*,err=9270,end=450) modday,yys,mms,dds,hhs,nns,sss,(rval(i),i=1,nstatseries)
         jcount=jcount+1
         ddays=numdays(1,1,1970,dds,mms,yys)
         dsecs=numsecs(0,0,0,hhs,nns,sss)
440      continue
         if(dsecs.ge.86400)then
           ddays=ddays+1
           dsecs=dsecs-86400
           go to 440
         end if
         if((ddays.lt.begdays).or.((ddays.eq.begdays).and.(dsecs.lt.begsecs))) cycle
         if((ddays.gt.enddays).or.((ddays.eq.enddays).and.(dsecs.gt.endsecs))) cycle
         icount=icount+1
       end do
450    continue
       if(jcount.eq.0)then
         write(amessage,460) trim(sString_g)
460      format('no data is present within file ',a)
         go to 9800
       end if
       if(icount.eq.0)then
         write(amessage,470) trim(sString_g)
470      format('no data is present within file ',a,' within requested date/time limits.')
         go to 9800
       end if
       iterm=icount

! -- Space is now allocated for the new series.

       do j=1,jseries
         k=jjseries(j)
         allocate(series_g(k)%days(iterm),series_g(k)%secs(iterm),  &
         series_g(k)%val(iterm),stat=ierr)
         if(ierr.ne.0)then
           write(amessage,550)
550        format('cannot allocate memory for another time series.')
           go to 9800
         end if
         series_g(k)%active=.true.
         series_g(k)%name=aname(j)
         series_g(k)%type='ts'
         series_g(k)%nterm=iterm
       end do

! -- The file is now read a second time and the data is imported.

       rewind(unit=iunit,iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,371) trim(sString_g)
371      format('cannot re-wind STATVAR file ',a)
         go to 9800
       end if
       do i=1,nstatseries+1
         read(iunit,'(a)') cline
       end do
       icount=0
       jcount=0
       do
         read(iunit,*,err=9270,end=700) modday,yys,mms,dds,hhs,nns,sss,(rval(i),i=1,nstatseries)
         jcount=jcount+1
         ddays=numdays(1,1,1970,dds,mms,yys)
         dsecs=numsecs(0,0,0,hhs,nns,sss)
442      continue
         if(dsecs.ge.86400)then
           ddays=ddays+1
           dsecs=dsecs-86400
           go to 442
         end if
         if((ddays.lt.begdays).or.((ddays.eq.begdays).and.(dsecs.lt.begsecs))) cycle
         if((ddays.gt.enddays).or.((ddays.eq.enddays).and.(dsecs.gt.endsecs))) go to 700
         icount=icount+1
         do i=1,jseries
           k=jjseries(i)
           series_g(k)%days(icount)=ddays
           series_g(k)%secs(icount)=dsecs
           j=datcol(i)
           series_g(k)%val(icount)=rval(j)
         end do
       end do
700    continue

       do j=1,jseries
         write(*,860) trim(aname(j)),trim(sString_g)
         write(LU_REC,860) trim(aname(j)),trim(sString_g)
860      format(t5,'Series "',a,'" successfully imported from file ',a)
       end do

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
9200   call num2char(jline,aline)
       call addquote(afile,sString_g)
       write(amessage,9210) trim(aline),trim(sString_g)
9210   format('unable to read line ',a,' of STATVAR file ',a)
       go to 9800
9270   if(jcount.eq.0)then
         call addquote(afile,sString_g)
         write(amessage,9280) trim(sString_g)
9280     format('cannot read first data line from STATVAR file ',a,'.')
         go to 9800
       else
         if(datespec.eq.1)then
           write(aadate,9281) dds,mms,yys
9281       format(i2.2,'/',i2.2,'/',i4)
         else
           write(aadate,9281) mms,dds,yys
         end if
         write(aatime,9282) hhs,nns,sss
9282     format(i2.2,':',i2.2,':',i2.2)
         call addquote(afile,sString_g)
         write(amessage,9283) trim(sString_g),trim(aadate),trim(aatime)
9283     format('error reading STATVAR file ',a,'. Error occured for date=',a,', ', &
         'time=',a,' or for the entry after that.')
         go to 9800
       end if
9300   call addquote(afile,sString_g)
       write(amessage,9310) trim(sString_g)
9310   format('unexpected components in header lines to GSFLOW gage file ',a,'.')
       go to 9800
9350   call addquote(afile,sString_g)
       write(amessage,9360) trim(sString_g)
9360   format('positive integer expected on first line of STATVAR file ',a)
       go to 9800

9800   call write_message(leadspace='yes',error='yes')
       call write_message(iunit=LU_REC,leadspace='yes')
       ifail=1

9900   close(unit=iunit,iostat=ierr)
       if(allocated(rval)) deallocate(rval,stat=ierr)
       return


end subroutine get_mul_series_statvar


!     Last change:  JD   24 Aug 2003    9:11 am
subroutine get_mul_series_tetrad(ifail)

! -- Subroutine GET_MUL_SERIES_TETRAD reads multiple series fron a TETRAD
!    PLT file.
       integer, intent(out)   :: ifail

       integer dd1,mm1,yy1,hh1,nn1,ss1,dd2,mm2,yy2,hh2,nn2,ss2,ierr, &
       icontext,i,iunit,begdays,begsecs,enddays,endsecs,jline,j, &
       jseries,kseries,iwellname,ivarname,ddr,mmr,yyr,hhr,ssr,nnr,rdays,  &
       rsecs,nplot,ixcon,mdays,msecs,k,iterm,isplit,iseriesname,jj,kk
       integer jjseries(MAXSERIES),iname(MAXSERIES),iiterm(MAXSERIES)
       real rtemp,rtime
       character*12 atemp
       character*15 aline
       character*25 aoption
       character*120 afile
       character*12 wellname(MAXSERIES),varname(MAXSERIES)
       character*25 acontext(MAXCONTEXT)
       character (len=iTSNAMELENGTH) :: aname(MAXSERIES)

       ifail=0
       CurrentBlock_g='GET_SERIES_TETRAD'

       write(*,10) trim(CurrentBlock_g)
       write(LU_REC,10) trim(CurrentBlock_g)
10     format(/,' Processing ',a,' block....')

       afile=' '
       icontext=0
       ixcon=0
       yy1=-9999
       hh1=-9999
       yy2=-9999
       hh2=-9999
       yyr=-9999
       hhr=-9999
       iwellname=1
       iseriesname=0
       ivarname=0
       jseries=0
       kseries=0
       iiterm=0               ! iiterm is a series.
       iunit=0

! -- The GET_SERIES_TETRAD block is first parsed.

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
         if(aoption.eq.'FILE')then
           call get_file_name(ierr,afile)
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
         else if(aoption.eq.'WELL_NAME')then
           if(iwellname.eq.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,42) trim(CurrentBlock_g),trim(aline),trim(sString_g)
42           format('WELL_NAME keyword in wrong position in ',a,' block at line ',a, &
             ' of file ',a)
             go to 9800
           end if
           jseries=jseries+1
45         kseries=kseries+1
           if(kseries.gt.MAXSERIES)then
             write(amessage,44) trim(CurrentBlock_g)
44           format('too many new series cited in ',a,' block. Increase MAXSERIES ', &
             'and re-compile program.')
             go to 9800
           end if
           if(series_g(kseries)%active) go to 45
           jjseries(jseries)=kseries
           call getfile(ierr,cline,wellname(jseries),left_word(2),right_word(2))
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,57) trim(aline),trim(sString_g)
57           format('cannot read WELL_NAME from line ',a,' of file ',a)
             go to 9800
           end if
           call casetrans(wellname(jseries),'lo')
           call addquote(wellname(jseries),sString_g)
           write(*,46) trim(sString_g)
           write(LU_REC,46) trim(sString_g)
46         format(t5,'WELL_NAME ',a)
           iwellname=0
           ivarname=1
         else if(aoption.eq.'OBJECT_NAME')then
           if(ivarname.eq.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,47) trim(CurrentBlock_g),trim(aline),trim(sString_g)
47           format('OBJECT_NAME keyword can only follow a WELL_NAME keyword in ',  &
             a,' block at line ',a,' of file ',a)
             go to 9800
           end if
           call getfile(ierr,cline,varname(jseries),left_word(2),right_word(2))
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,58) trim(aline),trim(sString_g)
58           format('cannot read OBJECT_NAME from line ',a,' of file ',a)
             go to 9800
           end if
           call casetrans(varname(jseries),'lo')
           call addquote(varname(jseries),sString_g)
           write(*,51) trim(sString_g)
           write(LU_REC,51) trim(sString_g)
51         format(t5,'OBJECT_NAME ',a)
           ivarname=0
           iseriesname=1
           aname(jseries)=' '
         else if(aoption.eq.'NEW_SERIES_NAME')then
           if(iseriesname.eq.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,43) trim(CurrentBlock_g),trim(aline),trim(sString_g)
43           format('NEW_SERIES_NAME keyword can only follow a OBJECT_NAME ',  &
             'keyword in ',a,' block at line ',a,' of file ',a)
             go to 9800
           end if
           call get_new_series_name(ierr,aname(jseries))
           if(ierr.ne.0) go to 9800
           if(jseries.gt.1)then
             do j=1,jseries-1
               if(aname(jseries).eq.aname(j))then
                 write(amessage,146) trim(aname(jseries)),trim(CurrentBlock_g)
146               format('SERIES_NAME "',a,'" used more than once in ',a,' block.')
                 go to 9800
               end if
             end do
           end if
           iseriesname=0
           iwellname=1
         else if(aoption.eq.'CONTEXT')then
           if(ixcon.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,41) trim(aline),trim(sString_g)
41           format('Context keyword in incorrect location at line ',a,' of file ',a)
             go to 9800
           end if
           call get_context(ierr,icontext,acontext)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'MODEL_REFERENCE_DATE')then
           call get_date(ierr,ddr,mmr,yyr,'MODEL_REFERENCE_DATE')
           if(ierr.ne.0) go to 9800
           rdays=numdays(1,1,1970,ddr,mmr,yyr)
         else if(aoption.eq.'MODEL_REFERENCE_TIME')then
           call get_time(ierr,hhr,nnr,ssr,'MODEL_REFERENCE_TIME')
           if(ierr.ne.0) go to 9800
           rsecs=hhr*3600+nnr*60+ssr
         else if(aoption.eq.'END')then
           if(iseriesname.eq.1)then
             write(amessage,48) trim(CurrentBlock_g)
48           format(a,' block END encountered before finding ', &
             'expected NEW_SERIES_NAME keyword.')
             go to 9800
           end if
           if(ivarname.eq.1)then
             write(amessage,56) trim(CurrentBlock_g)
56           format(a,' block END encountered before finding ', &
             'expected NEW_OBJECT_NAME keyword.')
             go to 9800
           end if
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

! -- If there are any absences in the GET_SERIES_TETRAD block, these are now reported.

100    continue
       if(afile.eq.' ')then
         call addquote(sInfile_g,sString_g)
         write(amessage,110) trim(CurrentBlock_g),trim(sString_g)
110      format('no FILE keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       if(icontext.eq.0)then
         call addquote(sInfile_g,sString_g)
         write(amessage,122) trim(CurrentBlock_g),trim(sString_g)
122      format('no CONTEXT keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       if(jseries.eq.0)then
         call addquote(sInfile_g,sString_g)
         write(amessage,125) trim(CurrentBlock_g),trim(sString_g)
125      format('no WELL_NAME keyword provided in ',a,' block in file ',a)
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
       if(yyr.eq.-9999)then
         call addquote(sInfile_g,sString_g)
         write(amessage,126) trim(CurrentBlock_g),trim(sString_g)
126      format('no MODEL_REFERENCE_DATE keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       if(hhr.eq.-9999)then
         call addquote(sInfile_g,sString_g)
         write(amessage,127) trim(CurrentBlock_g),trim(sString_g)
127      format('no MODEL_REFERENCE_TIME keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       if(jseries.gt.1)then
         do j=2,jseries
           do i=1,j-1
             if((wellname(j).eq.wellname(i)).and.(varname(j).eq.varname(i)))then
               call addquote(sInfile_g,sString_g)
               write(amessage,401) trim(CurrentBlock_g),trim(sString_g)
401            format('two series possess the same WELL NAME and OBJECT NAME ',  &
               'in ',a,' block of file ',a)
               go to 9800
             end if
           end do
         end do
       end if

! -- There appear to be no errors in the block, so now it is processed.

       call addquote(afile,sString_g)
       write(*,179) trim(sString_g)
       write(LU_REC,179) trim(sString_g)
179    format(t5,'Reading TETRAD output file ',a,'....')
       iunit=nextunit()
       open(unit=iunit,file=afile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,180) trim(sString_g),trim(CurrentBlock_g)
180      format('cannot open file ',a,' cited in ',a,' block.')
         go to 9800
       end if

! -- The file is perused a first time to find out the storage requirements of the
!    time series.

       jline=0
190    continue
       do
         jline=jline+1
         read(iunit,'(a)',end=400) cline
         if(index(cline,'NPLOT').ne.0) exit
       end do
191    jline=jline+1
       read(iunit,'(a)',end=9300) cline
       call linesplit(ierr,3)
       if(ierr.ne.0)then
         call num2char(jline,aline)
         write(amessage,200) trim(aline),trim(sString_g)
200      format('there should be 3 entries on line ',a,' of file ',a)
         go to 9800
       end if
       call char2num(ierr,cline(left_word(1):right_word(1)),nplot)
       if(ierr.ne.0)then
         call num2char(jline,aline)
         write(amessage,220) trim(aline),trim(sString_g)
220      format('cannot read NPLOT from line ',a,' of file ',a)
         go to 9800
       end if
       if(nplot.gt.NUM_WORD_DIM)then
         call num2char(jline,aline)
         write(amessage,221) trim(aline),trim(sString_g)
221      format('NPLOT too large at line ',a,' of file ',a,'. Increase ',  &
         'NUM_WORD_DIM and re-compile program.')
         go to 9800
       end if
       call char2num(ierr,cline(left_word(2):right_word(2)),rtime)
       if(ierr.ne.0)then
         call num2char(jline,aline)
         write(amessage,223) trim(aline),trim(sString_g)
223      format('cannot read TIME from line ',a,' of file ',a)
         go to 9800
       end if
       mdays=rtime
       msecs=(rtime-mdays)*86400
       mdays=mdays+rdays
       msecs=msecs+rsecs
224    if(msecs.ge.86400)then
         msecs=msecs-86400
         mdays=mdays+1
         go to 224
       end if
       if((mdays.lt.begdays).or.((mdays.eq.begdays).and.(msecs.lt.begsecs)))then
         do
           jline=jline+1
           read(iunit,'(a)',end=400) cline
           if((index(cline,'NPLOT ').ne.0).and.(index(cline,' TIME').ne.0)) go to 191
         end do
       end if
       if((mdays.gt.enddays).or.                                  &
         ((mdays.eq.enddays).and.(msecs.gt.endsecs))) go to 400
       jline=jline+1
       read(iunit,'(a)',end=9300) cline
       jline=jline+1
       read(iunit,'(a)',end=9300) cline
       call linesplit(ierr,1)
       if(ierr.ne.0)then
         call num2char(jline,aline)
         write(amessage,230) trim(aline),trim(sString_g)
         go to 9800
       end if
       atemp=cline(left_word(1):right_word(1))
       call casetrans(atemp,'hi')
       if(atemp(1:5).ne.'NAMEW')then
         call num2char(jline,aline)
         write(amessage,230) trim(aline),trim(sString_g)
230      format('"NAMEW" string expected as first entry in line ',a,' of file ',a)
         go to 9800
       end if
       cline=cline(right_word(1)+1:)
       cline=adjustl(cline)
       iname=0                        ! iname is an array
       call linesplit(ierr,nplot)
       if(ierr.ne.0)then
         call num2char(jline,aline)
         write(amessage,240) trim(aline),trim(sString_g)
240      format('insufficient entries on line ',a,' of file ',a)
         go to 9800
       end if
       do i=1,nplot
         atemp=cline(left_word(i):right_word(i))
         call casetrans(atemp,'lo')
         do j=1,jseries
           if(varname(j).eq.atemp)then
             if(iname(j).ne.0)then
               call num2char(jline,aline)
               write(amessage,245) trim(aline),trim(sString_g)
245            format('object name mentioned twice on line ',a,' of file ',a)
               go to 9800
             end if
             iname(j)=i
           end if
         end do
       end do
       do
         jline=jline+1
         read(iunit,'(a)',end=400) cline
         if(cline.eq.' ') cycle
         if((index(cline,'NPLOT ').ne.0).and.(index(cline,' TIME').ne.0)) go to 191
         call linesplit(ierr,1)
         atemp=cline(left_word(1):right_word(1))
         call casetrans(atemp,'lo')
         do j=1,jseries
           if((wellname(j).eq.atemp).and.(iname(j).ne.0))iiterm(j)=iiterm(j)+1
         end do
       end do

400    continue

! -- Space is now allocated for the new series.

       do j=1,jseries
         k=jjseries(j)
         iterm=iiterm(j)
         if(iterm.eq.0)then
           write(amessage,405) trim(wellname(j)),trim(varname(j)),trim(sString_g)
405        format('no data can be assigned to the series pertaining to WELL NAME "', &
           a,'" and OBJECT NAME "',a,'" from file ',a)
           go to 9800
         end if
         allocate(series_g(k)%days(iterm),series_g(k)%secs(iterm),  &
         series_g(k)%val(iterm),stat=ierr)
         if(ierr.ne.0)then
           write(amessage,550)
550        format('cannot allocate memory for another time series.')
           go to 9800
         end if
         series_g(k)%active=.true.
         series_g(k)%name=aname(j)
         series_g(k)%type='ts'
         series_g(k)%nterm=iiterm(j)
       end do

! -- The TETRAD output file is now re-read and the time-series are imported.

       rewind(unit=iunit,iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,460) trim(sString_g)
460      format('cannot rewind file ',a,' to import time series data.')
         go to 9800
       end if

       iiterm=0                 !iiterm is an array
       jline=0
490    continue
       do
         jline=jline+1
         read(iunit,'(a)',end=800) cline
         if(index(cline,'NPLOT').ne.0) exit
       end do
491    jline=jline+1
       read(iunit,'(a)',end=9300) cline
       call linesplit(ierr,3)
       call char2num(ierr,cline(left_word(1):right_word(1)),nplot)
       call char2num(ierr,cline(left_word(2):right_word(2)),rtime)
       mdays=rtime
       msecs=(rtime-mdays)*86400
       mdays=mdays+rdays
       msecs=msecs+rsecs
424    if(msecs.ge.86400)then
         msecs=msecs-86400
         mdays=mdays+1
         go to 424
       end if
       if((mdays.lt.begdays).or.((mdays.eq.begdays).and.(msecs.lt.begsecs)))then
         do
           jline=jline+1
           read(iunit,'(a)',end=400) cline
           if((index(cline,'NPLOT ').ne.0).and.(index(cline,' TIME').ne.0)) go to 491
         end do
       end if
       if((mdays.gt.enddays).or.                              &
         ((mdays.eq.enddays).and.(msecs.gt.endsecs))) go to 800
       jline=jline+1
       read(iunit,'(a)',end=9300) cline
       jline=jline+1
       read(iunit,'(a)',end=9300) cline
       call linesplit(ierr,1)
       cline=cline(right_word(1)+1:)
       cline=adjustl(cline)
       iname=0                        ! iname is an array
       call linesplit(ierr,nplot)
       do i=1,nplot
         atemp=cline(left_word(i):right_word(i))
         call casetrans(atemp,'lo')
         do j=1,jseries
           if(varname(j).eq.atemp)then
             iname(j)=i
           end if
         end do
       end do
       do
         jline=jline+1
         read(iunit,'(a)',end=800) cline
         if(cline.eq.' ') cycle
         if((index(cline,'NPLOT ').ne.0).and.(index(cline,' TIME').ne.0)) go to 491
         call linesplit(ierr,1)
         isplit=0
         atemp=cline(left_word(1):right_word(1))
         call casetrans(atemp,'lo')
         do j=1,jseries
           if(wellname(j).eq.atemp)then
             if(iname(j).ne.0)then
               if(isplit.eq.0)then
                 isplit=1
                 cline=cline(right_word(1)+1:)
                 call linesplit(ierr,nplot)
               end if
               jj=iname(j)
               call char2num(ierr,cline(left_word(jj):right_word(jj)),rtemp)
               if(ierr.ne.0)then
                 call num2char(jline,aline)
                 write(amessage,520) trim(varname(j)),trim(aline),trim(sString_g)
520              format('cannot read "',a,'" object from line ',a,' of file ',a)
                 go to 9800
               end if
               k=jjseries(j)
               iiterm(j)=iiterm(j)+1
               kk=iiterm(j)
               series_g(k)%val(kk)=rtemp
               series_g(k)%days(kk)=mdays
               series_g(k)%secs(kk)=msecs
               if(kk.gt.1)then
                 if((series_g(k)%days(kk).eq.series_g(k)%days(kk-1)).and.   &
                    (series_g(k)%secs(kk).eq.series_g(k)%secs(kk-1)))then
                    call num2char(jline,aline)
                    write(amessage,551) trim(wellname(j)),trim(aline),trim(sString_g)
551                 format('well "',a,'" appears twice in one block at line ',a,  &
                    ' of file ',a)
                    go to 9800
                 end if
               end if
             end if
           end if
         end do
465      continue
       end do

800    continue
       do j=1,jseries
         write(*,860) trim(aname(j)),trim(sString_g)
         write(LU_REC,860) trim(aname(j)),trim(sString_g)
860      format(t5,'Series "',a,'" successfully imported from file ',a)
       end do

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
9300   continue
       write(amessage,9310) trim(sString_g)
9310   format('premature end encountered to TETRAD output file ',a)
       go to 9800

9800   call write_message(leadspace='yes',error='yes')
       call write_message(iunit=LU_REC,leadspace='yes')
       ifail=1

9900   if(iunit.ne.0)close(unit=iunit,iostat=ierr)
       return


end subroutine get_mul_series_tetrad



subroutine get_mul_series_ssf(ifail)

! -- Subroutine GET_MUL_SERIES_SSF reads multiple series fron a site
!    sample file.
       integer, intent(out)   :: ifail

       integer dd1,mm1,yy1,hh1,nn1,ss1,dd2,mm2,yy2,hh2,nn2,ss2,ierr, &
       icontext,i,iunit,begdays,begsecs,enddays,endsecs,jline,j, &
       jseries,kseries,ixcon,k,isite,iseriesname,jj,iactive,nn,ss,iterm
       integer jjseries(MAXSERIES),iiterm(MAXSERIES)
       double precision dvalue
       character (len=iTSNAMELENGTH) :: bsite,lastsite
       character*15 aline
       character*25 aoption
       character*120 afile
       character (len=iTSNAMELENGTH) :: site(MAXSERIES)
       character*25 acontext(MAXCONTEXT)
       character (len=iTSNAMELENGTH) :: aname(MAXSERIES)

       ifail=0
       CurrentBlock_g='GET_MUL_SERIES_SSF'

       write(*,10) trim(CurrentBlock_g)
       write(LU_REC,10) trim(CurrentBlock_g)
10     format(/,' Processing ',a,' block....')

       afile=' '
       icontext=0
       ixcon=0
       yy1=-9999
       hh1=-9999
       yy2=-9999
       hh2=-9999
       isite=1
       iseriesname=0
       jseries=0
       kseries=0
       iiterm=0               ! iiterm is a series.

! -- The GET_MUL_SERIES_SSF block is first parsed.

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
         if(aoption.eq.'FILE')then
           call get_file_name(ierr,afile)
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
         else if(aoption.eq.'SITE')then
           if(isite.eq.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,42) trim(CurrentBlock_g),trim(aline),trim(sString_g)
42           format('SITE keyword in wrong position in ',a,' block at line ',a, &
             ' of file ',a)
             go to 9800
           end if
           jseries=jseries+1
45         kseries=kseries+1
           if(kseries.gt.MAXSERIES)then
             write(amessage,44) trim(CurrentBlock_g)
44           format('too many new series cited in ',a,' block. Increase MAXSERIES ', &
             'and re-compile program.')
             go to 9800
           end if
           if(series_g(kseries)%active) go to 45
           jjseries(jseries)=kseries
           call getfile(ierr,cline,site(jseries),left_word(2),right_word(2))
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,57) trim(aline),trim(sString_g)
57           format('cannot read SITE name from line ',a,' of file ',a)
             go to 9800
           end if
           call casetrans(site(jseries),'lo')
           call addquote(site(jseries),sString_g)
           write(*,46) trim(sString_g)
           write(LU_REC,46) trim(sString_g)
46         format(t5,'SITE ',a)
           isite=0
           iseriesname=1
           aname(jseries)=' '
         else if(aoption.eq.'NEW_SERIES_NAME')then
           if(iseriesname.eq.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,43) trim(CurrentBlock_g),trim(aline),trim(sString_g)
43           format('NEW_SERIES_NAME keyword can only follow a SITE ',  &
             'keyword in ',a,' block at line ',a,' of file ',a)
             go to 9800
           end if
           call get_new_series_name(ierr,aname(jseries))
           if(ierr.ne.0) go to 9800
           if(jseries.gt.1)then
             do j=1,jseries-1
               if(aname(jseries).eq.aname(j))then
                 write(amessage,146) trim(aname(jseries)),trim(CurrentBlock_g)
146               format('SERIES_NAME "',a,'" used more than once in ',a,' block.')
                 go to 9800
               end if
             end do
           end if
           iseriesname=0
           isite=1
         else if(aoption.eq.'CONTEXT')then
           if(ixcon.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,41) trim(aline),trim(sString_g)
41           format('Context keyword in incorrect location at line ',a,' of file ',a)
             go to 9800
           end if
           call get_context(ierr,icontext,acontext)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'END')then
           if(iseriesname.eq.1)then
             write(amessage,48) trim(CurrentBlock_g)
48           format(a,' block END encountered before finding ', &
             'expected NEW_SERIES_NAME keyword.')
             go to 9800
           end if
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
       if(afile.eq.' ')then
         call addquote(sInfile_g,sString_g)
         write(amessage,110) trim(CurrentBlock_g),trim(sString_g)
110      format('no FILE keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       if(icontext.eq.0)then
         call addquote(sInfile_g,sString_g)
         write(amessage,122) trim(CurrentBlock_g),trim(sString_g)
122      format('no CONTEXT keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       if(jseries.eq.0)then
         call addquote(sInfile_g,sString_g)
         write(amessage,125) trim(CurrentBlock_g),trim(sString_g)
125      format('no SITE keyword provided in ',a,' block in file ',a)
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
       if(jseries.gt.1)then
         do j=2,jseries
           do i=1,j-1
             if(site(j).eq.site(i))then
               call addquote(sInfile_g,sString_g)
               write(amessage,401) trim(CurrentBlock_g),trim(sString_g)
401            format('two series possess the same SITE name ',  &
               'in ',a,' block of file ',a)
               go to 9800
             end if
           end do
         end do
       end if

! -- There appear to be no errors in the block, so now it is processed.

       call addquote(afile,sString_g)
       write(*,179) trim(sString_g)
       write(LU_REC,179) trim(sString_g)
179    format(t5,'Reading site sample file ',a,'....')
       iunit=nextunit()
       open(unit=iunit,file=afile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,180) trim(sString_g),trim(CurrentBlock_g)
180      format('cannot open file ',a,' cited in ',a,' block.')
         go to 9800
       end if

! -- The file is perused a first time to find out the storage requirements of the
!    time series.

       iiterm=0          ! iiterm is an array
       jline=0
       lastsite=' '
       iactive=0
       do
         jline=jline+1
         read(iunit,'(a)',err=9200,end=500)cline
         call linesplit(ierr,4)
         if(ierr.lt.0) then
           cycle
         else if(ierr.gt.0)then
           call num2char(jline,aline)
           write(amessage,375) trim(aline),trim(sString_g)
375        format('four entries expected on line ',a,' of site sample file ',a)
           go to 9800
         end if
         bsite=cline(left_word(1):right_word(1))
         call casetrans(bsite,'lo')
         if(bsite.ne.lastsite)then
           lastsite=bsite
           do j=1,jseries
             if(bsite.eq.site(j))then
               iactive=1
               go to 376
             end if
           end do
           iactive=0
376        continue
         end if
         if(iactive.eq.0) cycle
         if(cline(right_word(4):).ne.' ')then
           do k=right_word(4)+1,len_trim(cline)
             if(cline(k:k).ne.' ')then
               if(cline(k:k).eq.'x') go to 379
               go to 378
             end if
           end do
         end if
378      continue
         call read_rest_of_sample_line(ierr,4,nn,ss,dvalue,jline,afile)
         if(ierr.ne.0)then
           call write_message(iunit=LU_REC,leadspace='yes',error='yes')
           ifail=1
           return
         end if
         if(ss.ge.86400)then
           ss=ss-86400
           nn=nn+1
         end if
         if(iiterm(j).eq.0)then
           if((nn.lt.begdays).or.((nn.eq.begdays).and.(ss.lt.begsecs))) &
           cycle
         end if
         if((nn.gt.enddays).or.((nn.eq.enddays).and.(ss.gt.endsecs))) then
           iactive=0
           go to 379
         end if
         iiterm(j)=iiterm(j)+1
379      continue
       end do

500    continue

! -- Space is now allocated for the new series.

       do j=1,jseries
         k=jjseries(j)
         iterm=iiterm(j)
         if(iterm.eq.0)then
           write(amessage,405) trim(site(j)),trim(sString_g)
405        format('no data can be assigned to the series pertaining to SITE "', &
           a,'" from file ',a)
           go to 9800
         end if
         allocate(series_g(k)%days(iterm),series_g(k)%secs(iterm),  &
         series_g(k)%val(iterm),stat=ierr)
         if(ierr.ne.0)then
           write(amessage,550)
550        format('cannot allocate memory for another time series.')
           go to 9800
         end if
         series_g(k)%active=.true.
         series_g(k)%name=aname(j)
         series_g(k)%type='ts'
         series_g(k)%nterm=iiterm(j)
       end do

! -- The site sample file is now read a second time and the data is imported.

       rewind(unit=iunit,iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,370) trim(sString_g)
370      format('cannot re-wind site sample file ',a)
         go to 9800
       end if

       iiterm=0          ! iiterm is an array
       jline=0
       lastsite=' '
       iactive=0
       do
         jline=jline+1
         read(iunit,'(a)',err=9200,end=800)cline
         call linesplit(ierr,4)
         if(ierr.lt.0) cycle
         bsite=cline(left_word(1):right_word(1))
         call casetrans(bsite,'lo')
         if(bsite.ne.lastsite)then
           lastsite=bsite
           do j=1,jseries
             if(bsite.eq.site(j))then
               iactive=1
               jj=jjseries(j)
               go to 576
             end if
           end do
           iactive=0
576        continue
         end if
         if(iactive.eq.0) cycle
         if(cline(right_word(4):).ne.' ')then
           do k=right_word(4)+1,len_trim(cline)
             if(cline(k:k).ne.' ')then
               if(cline(k:k).eq.'x') go to 579
               go to 578
             end if
           end do
         end if
578      continue
         call read_rest_of_sample_line(ierr,4,nn,ss,dvalue,jline,afile)
         if(ss.ge.86400)then
           ss=ss-86400
           nn=nn+1
         end if
         if(iiterm(j).eq.0)then
           if((nn.lt.begdays).or.((nn.eq.begdays).and.(ss.lt.begsecs))) &
           cycle
         end if
         if((nn.gt.enddays).or.((nn.eq.enddays).and.(ss.gt.endsecs))) then
           iactive=0
           go to 579
         end if
         iiterm(j)=iiterm(j)+1
         iterm=iiterm(j)
         series_g(jj)%days(iterm)=nn
         series_g(jj)%secs(iterm)=ss
         series_g(jj)%val(iterm)=dvalue
579      continue
       end do

800    continue
       do j=1,jseries
         write(*,860) trim(aname(j)),trim(sString_g)
         write(LU_REC,860) trim(aname(j)),trim(sString_g)
860      format(t5,'Series "',a,'" successfully imported from file ',a)
       end do

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
9200   call num2char(jline,aline)
       call addquote(afile,sString_g)
       write(amessage,9210) trim(aline),trim(sString_g)
9210   format('unable to read line ',a,' of file ',a)
       go to 9800

9800   call write_message(leadspace='yes',error='yes')
       call write_message(iunit=LU_REC,leadspace='yes')
       ifail=1

9900   close(unit=iunit,iostat=ierr)
       return


end subroutine get_mul_series_ssf

subroutine get_plt_series(ifail)

! -- Subroutine get_plt_series reads one or a number of time series from a HSPF
!    PLTGEN file.
       integer, intent(out)   :: ifail

       integer dd1,mm1,yy1,hh1,nn1,ss1,dd2,mm2,yy2,hh2,nn2,ss2,ierr, &
       icontext,i,iunit,begdays,begsecs,enddays,endsecs,jline,j, &
       ilabel,iname,jseries,nseries,ii,npltseries,ipyear,ipmonth,ipday,iphour, &
       ipmin,idata,iterm,jdatstart,ndays,nsecs,ixcon
       integer icurve(MAXSERIES),lw(MAXSERIES),rw(MAXSERIES),iiterm(MAXSERIES), &
       jjseries(MAXSERIES)
       real threshold,rtemp
       character*15 aline
       character*25 aoption
       character*120 afile
       character*25 acontext(MAXCONTEXT)
       character (len=iTSNAMELENGTH) :: aname(MAXSERIES)
       character*20 aalabel,alabel(MAXSERIES)

       ifail=0
       CurrentBlock_g='GET_SERIES_PLOTGEN'

       write(*,10) trim(CurrentBlock_g)
       write(LU_REC,10) trim(CurrentBlock_g)
10     format(/,' Processing ',a,' block....')

       afile=' '
       icontext=0
       ixcon=0
       yy1=-9999
       hh1=-9999
       yy2=-9999
       hh2=-9999
       ilabel=1
       iname=0
       jseries=0
       nseries=0
       do i=1,MAXSERIES
         if(series_g(i)%active) nseries=nseries+1
       end do
       icurve=0         !icurve is an array
       iunit=0

! -- The GET_SERIES_PLOTGEN block is first parsed.

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
         if(aoption.eq.'FILE')then
           call get_file_name(ierr,afile)
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
         else if(aoption.eq.'LABEL')then
           if(ilabel.eq.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,42) trim(CurrentBlock_g),trim(aline),trim(sString_g)
42           format('LABEL keyword in wrong position in ',a,' block at line ',a, &
             ' of file ',a)
             go to 9800
           end if
           jseries=jseries+1
           if(jseries.gt.MAXSERIES)then
             call num2char(MAXSERIES,aline)
             write(amessage,44)trim(aline)
44           format('maximum of ',a,' LABELs can be cited in SET_SERIES_PLOTGEN block.')
             go to 9800
           end if
           call getfile(ierr,cline,alabel(jseries),left_word(2),right_word(2))
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,57) trim(aline),trim(sString_g)
57           format('cannot read LABEL from line ',a,' of file ',a)
             go to 9800
           end if
           call casetrans(alabel(jseries),'lo')
           call addquote(alabel(jseries),sString_g)
           write(*,46) trim(sString_g)
           write(LU_REC,46) trim(sString_g)
46         format(t5,'LABEL ',a)
           ilabel=0
           iname=1
           aname(jseries)=' '
         else if(aoption.eq.'NEW_SERIES_NAME')then
           if(iname.eq.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,43) trim(CurrentBlock_g),trim(aline),trim(sString_g)
43           format('"NEW_SERIES_NAME" keyword must follow a LABEL keyword in ',a, &
             ' block at line ',a,' of file ',a)
             go to 9800
           end if
           call get_new_series_name(ierr,aname(jseries))
           if(ierr.ne.0) go to 9800
           if(jseries.gt.1)then
             do j=1,jseries-1
               if(aname(jseries).eq.aname(j))then
                 write(amessage,146) trim(aname(jseries)),trim(CurrentBlock_g)
146               format('NEW_SERIES_NAME "',a,'" used more than once in ',a,' block.')
                 go to 9800
               end if
             end do
           end if
           iname=0
           ilabel=1
         else if(aoption.eq.'CONTEXT')then
           if(ixcon.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,41) trim(aline),trim(sString_g)
41           format('Context keyword in incorrect location at line ',a,' of file ',a)
             go to 9800
           end if
           call get_context(ierr,icontext,acontext)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'END')then
           if(iname.eq.1)then
             write(amessage,48) trim(CurrentBlock_g)
48           format(a,' block END encountered before finding ', &
             'expected NEW_SERIES_NAME.')
             go to 9800
           end if
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

! -- If there are any absences in the GETSERIES block, this is now reported.

100    continue
       if(afile.eq.' ')then
         call addquote(sInfile_g,sString_g)
         write(amessage,110) trim(CurrentBlock_g),trim(sString_g)
110      format('no FILE keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       if(icontext.eq.0)then
         call addquote(sInfile_g,sString_g)
         write(amessage,122) trim(CurrentBlock_g),trim(sString_g)
122      format('no CONTEXT keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       if(jseries.eq.0)then
         call addquote(sInfile_g,sString_g)
         write(amessage,125) trim(CurrentBlock_g),trim(sString_g)
125      format('no LABEL keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       call date_check(ierr,yy1,mm1,dd1,hh1,nn1,ss1,yy2,mm2,dd2,hh2,nn2,ss2,  &
       begdays,begsecs,enddays,endsecs)
       if(ierr.ne.0) go to 9800
       if(nseries+jseries.gt.MAXSERIES)then
         call num2char(MAXSERIES,aline)
         write(amessage,132) trim(aline)
132      format('the time-series storage capabilities of TSPROC have been exceeded. ',&
         'You must increase MAXSERIES and recompile TSPROC.')
         go to 9800
       end if


! -- There appear to be no errors in the block, so now it is processed.

       call addquote(afile,sString_g)
       write(*,179) trim(sString_g)
       write(LU_REC,179) trim(sString_g)
179    format(t5,'Reading HSPF PLOTGEN file ',a,'....')
       iunit=nextunit()
       open(unit=iunit,file=afile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,180) trim(sString_g),trim(CurrentBlock_g)
180      format('cannot open file ',a,' cited in ',a,' block.')
         go to 9800
       end if

! -- The file is perused a first time to find out the storage requirements of the
!    time series.

       iterm=0
       jline=0
       do i=1,2
         jline=jline+1
         read(iunit,'(a)',err=9200,end=9300) cline
       end do
       jline=jline+1
       read(iunit,'(a)',err=9200,end=9300) cline
       call casetrans(cline,'lo')
       ii=index(cline,'total')
       if(ii.eq.0)then
         call num2char(jline,aline)
         write(amessage,210) trim(aline),trim(sString_g)
210      format('string "total" expected on line ',a,' of file ',a)
         go to 9800
       end if
       cline=cline(ii+5:)
       call linesplit(ierr,1)
       if(ierr.ne.0)then
         call num2char(jline,aline)
         write(amessage,220) trim(aline),trim(sString_g)
         go to 9800
       end if
       call char2num(ierr,cline(left_word(1):right_word(1)),npltseries)
       if(ierr.ne.0)then
         call num2char(jline,aline)
         write(amessage,220) trim(aline),trim(sString_g)
220      format('cannot read total curves from line ',a,' of file ',a)
         go to 9800
       end if
       do
         jline=jline+1
         read(iunit,'(a)',err=9200,end=9320) cline
         ii=index(cline,'reshold:')
         if(ii.ne.0) exit
       end do
       cline=cline(ii+8:)
       call linesplit(ierr,1)
       if(ierr.ne.0)then
         call num2char(jline,aline)
         write(amessage,225) trim(aline),trim(sString_g)
         go to 9800
       end if
       call char2num(ierr,cline(left_word(1):right_word(1)),threshold)
       if(ierr.ne.0)then
         call num2char(jline,aline)
         write(amessage,225) trim(aline),trim(sString_g)
225      format('cannot read threshold value from line ',a,' of file ',a)
         go to 9800
       end if
       do
         jline=jline+1
         read(iunit,'(a)',err=9200,end=9350) cline
         ii=index(cline,'for each curve')
         if(ii.ne.0) exit
       end do
       jline=jline+1
       read(iunit,'(a)',err=9200,end=9350) cline
       call casetrans(cline,'lo')
       if(index(cline,'abel').eq.0) go to 9350
       do i=1,npltseries
         jline=jline+1
         read(iunit,'(a)',err=9200,end=9400) cline
         aalabel=cline(6:25)
         aalabel=adjustl(aalabel)
         call casetrans(aalabel,'lo')
         do j=1,jseries
           if(alabel(j).eq.aalabel)then
             icurve(j)=i
             go to 230
           end if
         end do
230      continue
       end do
       do i=1,jseries
         if(icurve(i).eq.0)then
           write(amessage,240) trim(alabel(i)),trim(CurrentBlock_g)
240        format('no curve in HSPF PLOTGEN file corresponding to label "',a,  &
           '" cited in ',a,' block.')
           go to 9800
         end if
         lw(i)=23+(icurve(i)-1)*14
         rw(i)=lw(i)+13
       end do
       do
         jline=jline+1
         read(iunit,'(a)',err=9200,end=9450) cline
         ii=index(cline,'ate/time')
         if(ii.ne.0) exit
       end do
       jline=jline+1
       read(iunit,'(a)',err=9200,end=9450) cline
       idata=0
       iterm=0
       jdatstart=jline
       do
         jline=jline+1
         read(iunit,'(a)',err=9200,end=450) cline
         call char2num(ierr,cline(7:10),ipyear)
         if(ierr.ne.0) go to 9500
         call char2num(ierr,cline(12:13),ipmonth)
         if(ierr.ne.0) go to 9500
         call char2num(ierr,cline(15:16),ipday)
         if(ierr.ne.0) go to 9500
         call char2num(ierr,cline(18:19),iphour)
         if(ierr.ne.0) go to 9500
         call char2num(ierr,cline(21:22),ipmin)
         if(ierr.ne.0) go to 9500
         ndays=numdays(1,1,1970,ipday,ipmonth,ipyear)
260      if(iphour.ge.24)then
           iphour=iphour-24
           ndays=ndays+1
           go to 260
         end if
         nsecs=numsecs(0,0,0,iphour,ipmin,0)
         if(idata.eq.0)then
           if(begdays.eq.-99999999)then
             begdays=ndays
             begsecs=nsecs
           end if
           idata=1
         end if
         if((ndays.lt.begdays).or.((ndays.eq.begdays).and.(nsecs.lt.begsecs))) go to 400
         if((ndays.gt.enddays).or.((ndays.eq.enddays).and.(nsecs.gt.endsecs))) go to 450
         iterm=iterm+1
400      continue
       end do

450    continue
       if(iterm.eq.0)then
         write(amessage,460) trim(sString_g),trim(CurrentBlock_g)
460      format('no time series data can be imported from file ',a,'. Check contents ', &
         'of file, as well as DATE_1 and DATE_2 keywords in ',a,' block.')
         go to 9800
       end if

! -- Now that data storage requirements have been ascertained, space is allocated in the
!    time series.

       do j=1,jseries
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
         series_g(i)%name=aname(j)
         series_g(i)%type='ts'
         jjseries(j)=i
       end do

! -- The PLOTGEN file is now re-read and the time-series are imported.

       iiterm=0           ! iiterm is an array
       rewind(unit=iunit,iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,560) trim(sString_g)
560      format('cannot rewind file ',a,' to import time series data.')
         go to 9800
       end if
       jline=0
       do i=1,jdatstart
         jline=jline+1
         read(iunit,'(a)',err=9200,end=9300) cline
       end do
       do
         jline=jline+1
         read(iunit,'(a)',err=9200,end=650) cline
         call char2num(ierr,cline(7:10),ipyear)
         call char2num(ierr,cline(12:13),ipmonth)
         call char2num(ierr,cline(15:16),ipday)
         call char2num(ierr,cline(18:19),iphour)
         call char2num(ierr,cline(21:22),ipmin)
         ndays=numdays(1,1,1970,ipday,ipmonth,ipyear)
570      if(iphour.ge.24)then
           iphour=iphour-24
           ndays=ndays+1
           go to 570
         end if
         nsecs=numsecs(0,0,0,iphour,ipmin,0)
         if((ndays.lt.begdays).or.((ndays.eq.begdays).and.(nsecs.lt.begsecs))) go to 640
         if((ndays.gt.enddays).or.((ndays.eq.enddays).and.(nsecs.gt.endsecs))) go to 650
         do j=1,jseries
           call char2num(ierr,cline(lw(j):rw(j)),rtemp)
           if(ierr.ne.0)then
             call num2char(jline,aline)
             write(amessage,580) trim(aline),trim(sString_g)
580          format('cannot read time series value from line ',a,' of HSPF ', &
             'PLOTGEN file ',a)
             go to 9800
           end if
           if(rtemp.gt.threshold+3*spacing(threshold))then
             iiterm(j)=iiterm(j)+1
             series_g(jjseries(j))%val(iiterm(j))=rtemp
             series_g(jjseries(j))%days(iiterm(j))=ndays
             series_g(jjseries(j))%secs(iiterm(j))=nsecs
           end if
         end do
640      continue
       end do
650    continue
       do j=1,jseries
         if(iiterm(j).eq.0)then
           write(amessage,630) trim(aname(j)),trim(sString_g)
630        format('no terms can be imported into series ',a, &
           ' from HSPF PLOTGEN file ',a)
           go to 9800
         end if
         series_g(jjseries(j))%nterm=iiterm(j)
         write(*,660) trim(aname(j)),trim(sString_g)
         write(LU_REC,660) trim(aname(j)),trim(sString_g)
660      format(t5,'Series "',a,'" successfully imported from file ',a)
       end do
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
9200   call num2char(jline,aline)
       write(amessage,9210) trim(aline),trim(sString_g)
9210   format('unable to read line ',a,' of file ',a)
       go to 9800
9300   continue
       write(amessage,9310) trim(sString_g)
9310   format('premature end encountered to HSPF PLOTGEN file ',a)
       go to 9800
9320   continue
       write(amessage,9330) trim(sString_g)
9330   format('cannot locate "Threshold" value in HSPF PLOTGEN file ',a)
       go to 9800
9350   continue
       write(amessage,9360) trim(sString_g)
9360   format('cannot locate label list in HSPF PLOTGEN file ',a)
       go to 9800
9400   continue
       write(amessage,9410) trim(sString_g)
9410   format('unexpected end encountered to HSPF PLOTGEN file ',a,' while reading ', &
       'list of curve labels.')
       go to 9800
9450   continue
       write(amessage,9460) trim(sString_g)
9460   format('unexpected end encountered to HSPF PLOTGEN file ',a,' while looking ', &
       'for curve data.')
       go to 9800
9500   call num2char(jline,aline)
       write(amessage,9510) trim(aline),trim(sString_g)
9510   format('cannot read date/time from line ',a,' of HSPF PLOTGEN file ',a)
       go to 9800

9800   call write_message(leadspace='yes',error='yes')
       call write_message(iunit=LU_REC,leadspace='yes')
       ifail=1

9900   if(iunit.ne.0)close(unit=iunit,iostat=ierr)
       return

end subroutine get_plt_series

subroutine get_ssf_series(ifail)

! -- Subroutine get_ssf_series reads a time series from a site sample file.
       integer, intent(out)   :: ifail

       integer dd1,mm1,yy1,hh1,nn1,ss1,dd2,mm2,yy2,hh2,nn2,ss2,ierr,k,ixcon, &
       icontext,nn,ss,i,iunit,begdays,begsecs,enddays,endsecs,iterm,jline,j
       double precision dvalue
       character (len=iTSNAMELENGTH) :: asite,aname,bsite
       character*15 aline
       character*20 atemp
       character*25 aoption
       character*120 afile
       character*25 acontext(MAXCONTEXT)

       ifail=0
       CurrentBlock_g='GET_SERIES_SSF'

       write(*,10) trim(CurrentBlock_g)
       write(LU_REC,10) trim(CurrentBlock_g)
10     format(/,' Processing ',a,' block....')

       afile=' '
       asite=' '
       acontext(1)=' '
       aname=' '
       icontext=0
       yy1=-9999
       hh1=-9999
       yy2=-9999
       hh2=-9999
       ixcon=0
       iunit=0

! -- The GET_SERIES_SSF block is first parsed.

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
         if(aoption.eq.'FILE')then
           call get_file_name(ierr,afile)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'SITE')then
           if(asite.ne.' ')then
             write(amessage,44)
44           format('only one site name can be provided in a ',  &
             'GET_SERIES_SSF block; use a GET_MUL_SERIES_SSF block ', &
             'to read multiple series using one block.')
             go to 9800
           end if
           call getfile(ierr,cline,atemp,left_word(2),right_word(2))
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,45) trim(aline),trim(sString_g)
45           format('cannot read SITE name from line ',a,' of file ',a)
             go to 9800
           end if
           nn=len_trim(atemp)
           if(nn.gt.10)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,50) trim(aline),trim(sString_g)
50           format('site identifier must be 10 characters or less at line ', &
             a,' of file ',a)
             go to 9800
           end if
           asite=atemp(1:10)
           if(isspace(asite))then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,51) trim(asite),trim(aline),trim(sString_g)
51           format('space character in SITE name "',a,'" at line ',a,' of file ',a)
             go to 9800
           end if
           call casetrans(asite,'lo')
           write(*,55) trim(asite)
           write(LU_REC,55) trim(asite)
55         format(t5,'SITE ',a)
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
         else if(aoption.eq.'NEW_SERIES_NAME')then
           call get_new_series_name(ierr,aname)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'CONTEXT')then
           if(ixcon.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,41) trim(aline),trim(sString_g)
41           format('Context keyword in incorrect location at line ',a,' of file ',a)
             go to 9800
           end if
           call get_context(ierr,icontext,acontext)
           if(ierr.ne.0) go to 9800
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

! -- If there are any absences in the GETSERIES block, this is now reported.

100    continue
       if(afile.eq.' ')then
         call addquote(sInfile_g,sString_g)
         write(amessage,110) trim(CurrentBlock_g),trim(sString_g)
110      format('no FILE keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       if(asite.eq.' ')then
         call addquote(sInfile_g,sString_g)
         write(amessage,120) trim(CurrentBlock_g),trim(sString_g)
120      format('no SITE keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       if(icontext.eq.0)then
         call addquote(sInfile_g,sString_g)
         write(amessage,122) trim(CurrentBlock_g),trim(sString_g)
122      format('no CONTEXT keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       if(aname.eq.' ')then
         call addquote(sInfile_g,sString_g)
         write(amessage,125) trim(CurrentBlock_g),trim(sString_g)
125      format('no NEW_SERIES_NAME keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       call date_check(ierr,yy1,mm1,dd1,hh1,nn1,ss1,yy2,mm2,dd2,hh2,nn2,ss2,  &
       begdays,begsecs,enddays,endsecs)
       if(ierr.ne.0) go to 9800

! -- There appear to be no errors in the block, so now it is processed.

       call addquote(afile,sString_g)
       write(*,179) trim(sString_g)
       write(LU_REC,179) trim(sString_g)
179    format(t5,'Reading site sample file ',a,'....')
       iunit=nextunit()
       open(unit=iunit,file=afile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,180) trim(sString_g),trim(CurrentBlock_g)
180      format('cannot open site sample file ',a,' cited in ',a,' block.')
         go to 9800
       end if

! -- The file is perused a first time to find out the storage requirements of the
!    time series (actually, the approximate storage requirements, because we don't
!    want to waste too much time processing this file on the first pass through it).

       iterm=0
       jline=0
       do
         jline=jline+1
         read(iunit,'(a)',err=9200,end=300) cline
         call linesplit(ierr,1)
         if(ierr.lt.0) cycle
         bsite=cline(left_word(1):right_word(1))
         call casetrans(bsite,'lo')
         if(bsite.eq.asite)then
           iterm=iterm+1
         else
           if(iterm.gt.0) go to 300
         end if
       end do

300    continue
       if(iterm.eq.0)then
         write(amessage,310) trim(asite),trim(sString_g)
310      format('site "',a,'" not found in site sample file ',a)
         go to 9800
       end if

! -- Samples pertaining to the site are now read into the temporary time series
!    structure. If the structure is not big enough, it is re-dimensioned appropriately.

       call alloc_tempseries(ierr,iterm)
       if(ierr.ne.0) go to 9800

! -- The site sample file is now re-read and only the necessary data read in.

       rewind(unit=iunit,iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,370) trim(sString_g)
370      format('cannot re-wind site sample file ',a)
         go to 9800
       end if
       iterm=0
       jline=0
       do
         jline=jline+1
         read(iunit,'(a)',err=9200,end=500)cline
         call linesplit(ierr,4)
         if(ierr.lt.0) then
           cycle
         else if(ierr.gt.0)then
           call num2char(jline,aline)
           write(amessage,375) trim(aline),trim(sString_g)
375        format('four entries expected on line ',a,' of site sample file ',a)
           go to 9800
         end if
         bsite=cline(left_word(1):right_word(1))
         call casetrans(bsite,'lo')
         if(bsite.ne.asite)cycle
         if(cline(right_word(4):).ne.' ')then
           do k=right_word(4)+1,len_trim(cline)
             if(cline(k:k).ne.' ')then
               if(cline(k:k).eq.'x') go to 379
               go to 376
             end if
           end do
         end if
376      continue
         call read_rest_of_sample_line(ierr,4,nn,ss,dvalue,jline,afile)
         if(ierr.ne.0)then
           call write_message(iunit=LU_REC,leadspace='yes',error='yes')
           ifail=1
           return
         end if
         if(iterm.eq.0)then
           if((nn.lt.begdays).or.((nn.eq.begdays).and.(ss.lt.begsecs))) &
           cycle
         end if
         if((nn.gt.enddays).or.((nn.eq.enddays).and.(ss.gt.endsecs))) &
         go to 500
         iterm=iterm+1
         tempseries_g%days(iterm)=nn
         tempseries_g%secs(iterm)=ss
         tempseries_g%val(iterm)=dvalue
379      continue
       end do

500    continue
       if(iterm.eq.0)then
         write(amessage,505)
505      format('no terms of the series can be imported. Check the date settings.')
         go to 9800
       end if

! -- The time series is now copied to a real time series.

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
         series_g(i)%days(j)=tempseries_g%days(j)
       end do
       do j=1,iterm
         series_g(i)%secs(j)=tempseries_g%secs(j)
       end do
       do j=1,iterm
         series_g(i)%val(j)=tempseries_g%val(j)
       end do
       call addquote(afile,sString_g)
       write(*,580) trim(aname),trim(sString_g)
       write(LU_REC,580) trim(aname),trim(sString_g)
580    format(t5,'Series "',a,'" successfully imported from file ',a)
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

9200   call num2char(jline,aline)
       call addquote(afile,sString_g)
       write(amessage,9210) trim(aline),trim(sString_g)
9210   format('unable to read line ',a,' of file ',a)
       go to 9800

9800   call write_message(leadspace='yes',error='yes')
       call write_message(iunit=LU_REC,leadspace='yes')
       ifail=1

9900   if(iunit.ne.0)close(unit=iunit,iostat=ierr)
       return

end subroutine get_ssf_series

subroutine get_ufore_series(ifail)

! -- Subroutine get_ufore_series reads a time series from a UFORE-HYDRO file.
       integer, intent(out)   :: ifail

       integer dd1,mm1,yy1,hh1,nn1,ss1,dd2,mm2,yy2,hh2,nn2,ss2,ierr,k,ixcon, &
       icontext,i,iunit,begdays,begsecs,enddays,endsecs,iterm,jline,j, &
       dds,mms,yys,hhs,nns,sss,deltat,refdays,refsecs,nterm,tt1secs,tt2secs,  &
       totsecs,secs,days,jj
       real rtemp
       character (len=iTSNAMELENGTH) :: aname
       character*15 aline
       character*20 atemp
       character*25 aoption
       character*120 afile,bstring
       character*25 acontext(MAXCONTEXT)

       ifail=0
       CurrentBlock_g='GET_SERIES_UFORE_HYDRO'

       write(*,10) trim(CurrentBlock_g)
       write(LU_REC,10) trim(CurrentBlock_g)
10     format(/,' Processing ',a,' block....')

       afile=' '
       acontext(1)=' '
       aname=' '
       icontext=0
       yy1=-9999
       hh1=-9999
       yy2=-9999
       hh2=-9999
       yys=-9999
       hhs=-9999
       deltat=-9999
       ixcon=0
       iunit=0

! -- The GET_SERIES_UFORE_HYDRO block is first parsed.

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
         if(aoption.eq.'FILE')then
           call get_file_name(ierr,afile)
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
         else if(aoption.eq.'NEW_SERIES_NAME')then
           call get_new_series_name(ierr,aname)
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'MODEL_REFERENCE_DATE')then
           call get_date(ierr,dds,mms,yys,'MODEL_REFERENCE_DATE')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'MODEL_REFERENCE_TIME')then
           call get_time(ierr,hhs,nns,sss,'MODEL_REFERENCE_TIME')
           if(ierr.ne.0) go to 9800
         else if(aoption.eq.'TIME_INCREMENT')then
           call get_keyword_value(ierr,1,deltat,rtemp,'TIME_INCREMENT')
           if(ierr.ne.0) go to 9800
           if(deltat.le.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,30) trim(aline),trim(sString_g)
30           format('time increment must be positive at line ',a,' of file ',a)
             go to 9800
           end if
         else if(aoption.eq.'CONTEXT')then
           if(ixcon.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,41) trim(aline),trim(sString_g)
41           format('Context keyword in incorrect location at line ',a,' of file ',a)
             go to 9800
           end if
           call get_context(ierr,icontext,acontext)
           if(ierr.ne.0) go to 9800
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

! -- If there are any absences in the GETSERIES block, this is now reported.

100    continue
       if(afile.eq.' ')then
         call addquote(sInfile_g,sString_g)
         write(amessage,110) trim(CurrentBlock_g),trim(sString_g)
110      format('no FILE keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       if(icontext.eq.0)then
         call addquote(sInfile_g,sString_g)
         write(amessage,122) trim(CurrentBlock_g),trim(sString_g)
122      format('no CONTEXT keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       if(aname.eq.' ')then
         call addquote(sInfile_g,sString_g)
         write(amessage,125) trim(CurrentBlock_g),trim(sString_g)
125      format('no NEW_SERIES_NAME keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       if(yys.eq.-9999)then
         call addquote(sInfile_g,sString_g)
         write(amessage,126) trim(CurrentBlock_g),trim(sString_g)
126      format('no MODEL_REFERENCE_DATE keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       if(hhs.eq.-9999)then
         call addquote(sInfile_g,sString_g)
         write(amessage,127) trim(CurrentBlock_g),trim(sString_g)
127      format('no MODEL_REFERENCE_TIME keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       if(deltat.eq.-9999)then
         call addquote(sInfile_g,sString_g)
         write(amessage,128) trim(CurrentBlock_g),trim(sString_g)
128      format('no TIME_INCREMENT keyword provided in ',a,' block in file ',a)
         go to 9800
       end if
       call date_check(ierr,yy1,mm1,dd1,hh1,nn1,ss1,yy2,mm2,dd2,hh2,nn2,ss2,  &
       begdays,begsecs,enddays,endsecs)
       if(ierr.ne.0) go to 9800
       refdays=numdays(1,1,1970,dds,mms,yys)
       refsecs=numsecs(0,0,0,hhs,nns,sss)
       if(refsecs.ge.86400)then
         refsecs=refsecs-86400
         refdays=refdays+1
       end if
       if(yy1.ne.-9999)then
         if(((begdays.eq.refdays).and.(begsecs.lt.refsecs)).or.    &
            (begdays.lt.refdays))then
            write(amessage,130) trim(CurrentBlock_g),trim(sString_g)
130         format('DATE_1 and TIME_1 keywords provide a date and time that ', &
            'precedes the model reference date and time in ',a,' block of ',   &
            'file ',a)
            go to 9800
         end if
       end if

! -- There appear to be no errors in the block, so now it is processed.

       call addquote(afile,sString_g)
       write(*,179) trim(sString_g)
       write(LU_REC,179) trim(sString_g)
179    format(t5,'Reading UFORE-HYDRO file ',a,'....')
       iunit=nextunit()
       open(unit=iunit,file=afile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,180) trim(sString_g),trim(CurrentBlock_g)
180      format('cannot open UFORE-HYDRO file ',a,' cited in ',a,' block.')
         go to 9800
       end if

! -- The first line on the file is read.

       jline=1
       read(iunit,*,err=9200,end=9300) nterm
       if(nterm.le.0)then
         call addquote(afile,sString_g)
         write(amessage,190) trim(sString_g)
190      format('number-of-entries header cannot be zero or negative at ',  &
         'first line of UFORE-HYDRO file ',a)
         go to 9800
       end if

! -- If the date and time corresponding to DATE_2 and TIME_2 postdates
!    the end of the series, this is now evaluated.

       if(yy2.ne.-9999)then
         totsecs=nterm*deltat+refdays*86400+refsecs
         tt2secs=enddays*86400+endsecs
         if(tt2secs.gt.totsecs)then
           call addquote(sInfile_g,sString_g)
           call addquote(afile,bstring)
           write(amessage,200) trim(CurrentBlock_g),trim(sString_g),trim(bstring)
200        format('date and time corresponding to DATE_2 and TIME_2 in ',   &
           a,' block of file ',a,' postdates end of time series contained ',  &
           'in UFORE-HYDRO file ',a)
           go to 9800
         end if
       else
         tt2secs=huge(i)
       end if

! -- The total number of terms in the new series is evaluated given the
!    entries in the DATE_1, TIME_1, DATE_2 and TIME_2 strings.

        if((yy1.eq.-9999).and.(yy2.eq.-9999))then
          iterm=nterm
        else
          secs=refdays*86400+refsecs
          if(yy1.eq.-9999)then
            tt1secs=-9999
          else
            tt1secs=begdays*86400+begsecs
          end if
          iterm=0
          do i=1,nterm
            secs=secs+deltat
            if(secs.ge.tt1secs)then
              if(secs.le.tt2secs)then
                iterm=iterm+1
              else
                go to 220
              end if
            end if
          end do
220       continue
        end if
        if(iterm.eq.0)then
          call addquote(afile,sString_g)
          write(amessage,222) trim(sString_g)
222       format('time series of zero length is requested from UFORE-HYDRO ',  &
          'file ',a,'. Alter DATE_1, TIME_1, DATE_2, TIME_2 settings.')
          go to 9800
        end if

! -- Storage for the new series is now allocated.

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

! -- The series is now read in.

       days=refdays
       secs=refsecs
       if((yy1.eq.-9999).and.(yy2.eq.-9999))then
         do j=1,nterm
           jline=jline+1
           read(iunit,*,err=9200,end=9300) series_g(i)%val(j)
           secs=secs+deltat
551        continue
           if(secs.ge.86400)then
             secs=secs-86400
             days=days+1
             go to 551
           end if
           series_g(i)%days(j)=days
           series_g(i)%secs(j)=secs
         end do
       else
         if(yy1.eq.-9999)begdays=-9999
         if(yy2.eq.-9999)enddays=huge(i)
         jj=0
         do j=1,nterm
           jline=jline+1
           read(iunit,*,err=9200,end=9300) rtemp
           secs=secs+deltat
           if(secs.ge.86400)then
             secs=secs-86400
             days=days+1
           end if
           if((days.gt.begdays).or.                          &
              ((days.eq.begdays).and.(secs.ge.begsecs)))then
              if((days.lt.enddays).or.                        &
                 ((days.eq.enddays).and.(secs.le.endsecs)))then
                 jj=jj+1
                 series_g(i)%val(jj)=rtemp
                 series_g(i)%days(jj)=days
                 series_g(i)%secs(jj)=secs
              else
                go to 300
              end if
           end if
         end do
300      continue
       end if

       call addquote(afile,sString_g)
       write(*,580) trim(aname),trim(sString_g)
       write(LU_REC,580) trim(aname),trim(sString_g)
580    format(t5,'Series "',a,'" successfully imported from file ',a)
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

9200   call num2char(jline,aline)
       call addquote(afile,sString_g)
       write(amessage,9210) trim(aline),trim(sString_g)
9210   format('unable to read line ',a,' of UFORE-HYDRO file ',a)
       go to 9800
9300   call addquote(afile,sString_g)
       write(amessage,9310) trim(sString_g)
9310   format('unexpected end encountered to UFORE-HYDRO file ',a)
       go to 9800

9800   call write_message(leadspace='yes',error='yes')
       call write_message(iunit=LU_REC,leadspace='yes')
       ifail=1

9900   if(iunit.ne.0)close(unit=iunit,iostat=ierr)
       return

end subroutine get_ufore_series

! subroutine get_wdm_series(ifail)
!
! ! -- Subroutine get_wdm_series reads a time series from a WDM file.
!
!        implicit none
!
!        integer, intent(out)   :: ifail
!
!        integer dd1,mm1,yy1,hh1,nn1,ss1,dd2,mm2,yy2,hh2,nn2,ss2,ierr, &
!        icontext,nn,ss,i,wdmunit,iterm,dsn,jterm,j,hhd,nnd,ssd,secsd, &
!        retcode,yyy1,yyy2,mmm1,mmm2,ddd1,ddd2,hhh1,hhh2,nnn1,nnn2,sss1,sss2, &
!        begdays,begsecs,enddays,endsecs,ixcon,yy1flag,dd1a,mm1a,yy1a,hh1a,nn1a, &
!        ss1a,yy2flag,dd2a,mm2a,yy2a,hh2a,nn2a,ss2a,ibd,ibs,itsecs,iacount
!        integer lsdat(6),ledat(6),llsdat(6),lledat(6),tstep,tcode
!        real filter,fspace,fval
!        character*10 aname
!        character*15 aline
!        character*25 aoption
!        character*120 afile
!        character*25 acontext(MAXCONTEXT)
!
!        include 'cfbuff90.inc'
! !       include 'CDRLOC90.INC'
! !       include 'CTBLAB90.INC'
! !       include 'CTSBUF90.INC'
! !       include 'CWDMID90.INC'
! !       include 'CWTSDS90.INC'
! !       include 'inc_init.inc'
!
!        ifail=0
!        currentblock_g='GET_SERIES_WDM'
!
!        write(*,10) trim(currentblock_g)
!        write(LU_REC,10) trim(currentblock_g)
! 10     format(/,' Processing ',a,' block....')
!
!        afile=' '
!        aname=' '
!        icontext=0
!        yy1=-9999
!        hh1=-9999
!        yy2=-9999
!        hh2=-9999
!        hhd=-9999
!        dsn=-99999999
!        filter=-1.0e37
!        ixcon=0
!
! ! -- The GET_SERIES_WDM block is first parsed.
!
!        do
!          iLine_g=iLine_g+1
!          read(LU_TSPROC_CONTROL,'(a)',err=9000,end=9100) cline
!          if(cline.eq.' ') cycle
!          if(cline(1:1).eq.'#') cycle
!          call linesplit(ierr,2)
!          if(ierr.ne.0)then
!            call num2char(iLine_g,aline)
!            call addquote(sInfile_g,sString_g)
!            write(amessage,20) trim(aline),trim(sString_g)
! 20         format('there should be 2 entries on line ',a,' of file ',a)
!            go to 9800
!          end if
!          aoption=cline(left_word(1):right_word(1))
!          call casetrans(aoption,'hi')
!          if(aoption.ne.'CONTEXT')then
!            call test_context(ierr,icontext,acontext)
!            if(ierr.eq.-1)then
!              call find_end(ifail)
!              if(ifail.eq.1) go to 9800
!              return
!            else if(ierr.eq.1) then
!              go to 9800
!            end if
!            ixcon=1
!          end if
!          if(aoption.eq.'FILE')then
!            call get_file_name(ierr,afile)
!            if(ierr.ne.0) go to 9800
!          else if(aoption.eq.'DSN')then
!            call char2num(ierr,cline(left_word(2):right_word(2)),dsn)
!            if(ierr.ne.0)then
!              call num2char(iLine_g,aline)
!              call addquote(sInfile_g,sString_g)
!              write(amessage,45) trim(aline),trim(sString_g)
! 45           format('cannot read DSN from line ',a,' of file ',a)
!              go to 9800
!            end if
!            call num2char(dsn,aline)
!            write(*,55) trim(aline)
!            write(LU_REC,55) trim(aline)
! 55         format(t5,'DSN ',a)
!          else if(aoption.eq.'DATE_1')then
!            call get_date(ierr,dd1,mm1,yy1,'DATE_1')
!            if(ierr.ne.0) go to 9800
!          else if(aoption.eq.'DATE_2')then
!            call get_date(ierr,dd2,mm2,yy2,'DATE_2')
!            if(ierr.ne.0) go to 9800
!          else if(aoption.eq.'TIME_1')then
!            call get_time(ierr,hh1,nn1,ss1,'TIME_1')
!            if(ierr.ne.0) go to 9800
!          else if(aoption.eq.'TIME_2')then
!            call get_time(ierr,hh2,nn2,ss2,'TIME_2')
!            if(ierr.ne.0) go to 9800
!          else if(aoption.eq.'DEF_TIME')then
!            call get_time(ierr,hhd,nnd,ssd,'DEF_TIME')
!            if(ierr.ne.0) go to 9800
!          else if(aoption.eq.'NEW_SERIES_NAME')then
!            call get_new_series_name(ierr,aname)
!            if(ierr.ne.0) go to 9800
!          else if(aoption.eq.'CONTEXT')then
!            if(ixcon.ne.0)then
!              call num2char(iLine_g,aline)
!              call addquote(sInfile_g,sString_g)
!              write(amessage,41) trim(aline),trim(sString_g)
! 41           format('CONTEXT keyword in incorrect location at line ',a,' of file ',a)
!              go to 9800
!            end if
!            call get_context(ierr,icontext,acontext)
!            if(ierr.ne.0) go to 9800
!          else if(aoption.eq.'FILTER')then
!            call char2num(ierr,cline(left_word(2):right_word(2)),filter)
!            if(ierr.ne.0)then
!              call num2char(iLine_g,aline)
!              call addquote(sInfile_g,sString_g)
!              write(amessage,89) trim(aline),trim(sString_g)
! 89           format('cannot read filter from line ',a,' of file ',a)
!              go to 9800
!            end if
!            call num2char(filter,aline)
!            write(*,86) cline(left_word(2):right_word(2))
!            write(LU_REC,86) cline(left_word(2):right_word(2))
! 86         format(t5,'FILTER ',a)
!          else if(aoption.eq.'END')then
!            go to 100
!          else
!            call num2char(iLine_g,aline)
!            call addquote(sInfile_g,sString_g)
!            write(amessage,80) trim(aoption),trim(currentblock_g),trim(aline),trim(sString_g)
! 80         format('unexpected keyword - "',a,'" in ',a,' block at line ',a, &
!            ' of file ',a)
!            go to 9800
!          end if
!        end do
!
! ! -- If there are any absences in the GETSERIES block, this is now reported.
! ! -- The DEF_TIME keyword must NOT act on samples unless the time series is a day or greater!!!!!!
!
!
! 100    continue
!        if(afile.eq.' ')then
!          call addquote(sInfile_g,sString_g)
!          write(amessage,110) trim(currentblock_g),trim(sString_g)
! 110      format('no FILE keyword provided in ',a,' block in file ',a)
!          go to 9800
!        end if
!        if(dsn.eq.-99999999)then
!          call addquote(sInfile_g,sString_g)
!          write(amessage,120) trim(currentblock_g),trim(sString_g)
! 120      format('no DSN keyword provided in ',a,' block in file ',a)
!          go to 9800
!        end if
!        if(icontext.eq.0)then
!          call addquote(sInfile_g,sString_g)
!          write(amessage,122) trim(currentblock_g),trim(sString_g)
! 122      format('no CONTEXT keyword provided in ',a,' block in file ',a)
!          go to 9800
!        end if
!        if(aname.eq.' ')then
!          call addquote(sInfile_g,sString_g)
!          write(amessage,125) trim(currentblock_g),trim(sString_g)
! 125      format('no NEW_SERIES_NAME keyword provided in ',a,' block in file ',a)
!          go to 9800
!        end if
!        call date_check(ierr,yy1,mm1,dd1,hh1,nn1,ss1,yy2,mm2,dd2,hh2,nn2,ss2,  &
!        begdays,begsecs,enddays,endsecs)
!        if(ierr.ne.0) go to 9800
!
!
! ! -- There appear to be no errors in the block, so now it is processed.
!
!        call addquote(afile,sString_g)
!        write(*,179) trim(sString_g)
!        write(LU_REC,179) trim(sString_g)
! 179    format(t5,'Reading WDM file ',a,'....')
!
!        call nextwdmunit(ierr,wdmunit,afile)
!        if(ierr.ne.0) go to 9800
!        open(unit=99,file='dummy.dat')
!        if(wdmunit.lt.0)then
!          wdmunit=-wdmunit
!        else
!          call wdbopn(wdmunit,afile,1,retcode)
!          if(retcode.ne.0)then
!            write(amessage,210) trim(sString_g)
! 210        format('unable to open WDM file ',a,' for data retreival.')
!            go to 9800
!          end if
!        end if
!
! ! -- Next we ensure that our dates are no wider than those of the actual time series.
!
!        call wdatim(wdmunit,dsn,llsdat,lledat,tstep,tcode,retcode)
!        if(retcode.eq.-6)then
!          write(amessage,220) trim(sString_g)
! 220      format('there is no data pertaining to the nominated DSN in WDM file ',a)
!          go to 9800
!        else if(retcode.eq.-81) then
!          write(amessage,230) trim(sString_g)
! 230      format('the nominated data set does not exist in WDM file ',a)
!          go to 9800
!        else if(retcode.eq.-82)then
!          write(amessage,240) trim(sString_g)
! 240      format('the nominated data set is not a time-series data set in file ',a)
!          go to 9800
!        else if(retcode.ne.0)then
!          write(amessage,250) trim(sString_g)
! 250      format('cannot retrieve data for nominated data set from file ',a)
!          go to 9800
!        end if
!        yyy1=llsdat(1)
!        mmm1=llsdat(2)
!        ddd1=llsdat(3)
!        hhh1=llsdat(4)
!        nnn1=llsdat(5)
!        sss1=llsdat(6)
!        yyy2=lledat(1)
!        mmm2=lledat(2)
!        ddd2=lledat(3)
!        hhh2=lledat(4)
!        nnn2=lledat(5)
!        sss2=lledat(6)
! !       write(6,244) ddd1,mmm1,yyy1,hhh1,nnn1,sss1
! !244    format(' real start date  ',i2.2,'/',i2.2,'/',i4)
! !       write(6,245) ddd2,mmm2,yyy2,hhh2,nnn2,sss2
! !245    format(' real end   date  ',i2.2,'/',i2.2,'/',i4)
!
!        yy1flag=0
!        if(yy1.ne.-9999)then
!          yy1flag=1
!          dd1a=dd1
!          mm1a=mm1
!          yy1a=yy1
!          hh1a=hh1
!          nn1a=nn1
!          ss1a=ss1
!          nn=numdays(ddd1,mmm1,yyy1,dd1,mm1,yy1)
!          ss=numsecs(hhh1,nnn1,sss1,hh1,nn1,ss1)
!          if(ss.ge.86400)then
!            ss=ss-86400
!            nn=nn+1
!          else if(ss.le.-86400)then
!            ss=ss+86400
!            nn=nn-1
!          end if
!          if((nn.lt.0).or.((nn.eq.0).and.(ss.le.0)))then
!            dd1=ddd1
!            mm1=mmm1
!            yy1=yyy1
!            hh1=hhh1
!            nn1=nnn1
!            ss1=sss1
!          end if
!        else
!          dd1=ddd1
!          mm1=mmm1
!          yy1=yyy1
!          hh1=hhh1
!          nn1=nnn1
!          ss1=sss1
!        end if
!        yy2flag=0
!        if(yy2.ne.-9999)then
!          yy2flag=1
!          dd2a=dd2
!          mm2a=mm2
!          yy2a=yy2
!          hh2a=hh2
!          nn2a=nn2
!          ss2a=ss2
!          nn=numdays(dd2,mm2,yy2,ddd2,mmm2,yyy2)
!          ss=numsecs(hh2,nn2,ss2,hhh2,nnn2,sss2)
!          if(ss.ge.86400)then
!            ss=ss-86400
!            nn=nn+1
!          else if(ss.le.-86400)then
!            ss=ss+86400
!            nn=nn-1
!          end if
!          if((nn.lt.0).or.((nn.eq.0).and.(ss.le.0)))then
!            dd2=ddd2
!            mm2=mmm2
!            yy2=yyy2
!            hh2=hhh2
!            nn2=nnn2
!            ss2=sss2
!          end if
!        else
!          dd2=ddd2
!          mm2=mmm2
!          yy2=yyy2
!          hh2=hhh2
!          nn2=nnn2
!          ss2=sss2
!        end if
!
! ! -- The following lines is added to compensate for an idiosyncracy in the WDM
! !    library. A check is made later that the DATE_2 and TIME_2 specs are exactly
! !    obeyed.
!        dd2=dd2+1
!        if(dd2.gt.ddd2)dd2=ddd2
!
!        lsdat(1)=yy1
!        lsdat(2)=mm1
!        lsdat(3)=dd1
!        lsdat(4)=hh1
!        lsdat(5)=nn1
!        lsdat(6)=ss1
!        ledat(1)=yy2
!        ledat(2)=mm2
!        ledat(3)=dd2
!        ledat(4)=hh2
!        ledat(5)=nn2
!        ledat(6)=ss2
!
! ! -- Next space is allocated in the temporary time series to receive the data.
!
!        nn=numdays(dd1,mm1,yy1,dd2,mm2,yy2) + 2
!        if(tcode.eq.4)then
!          iterm=nn
!        else if(tcode.eq.3)then
!          iterm=nn*24
!        else if(tcode.eq.2)then
!          iterm=nn*24*60
!        else if(tcode.eq.1)then
!          iterm=nn*86400
!        else if(tcode.eq.5)then
!          iterm=nn/12+2
!        else
!          iterm=nn/365.25 +2
!        end if
!        if(tstep.ne.0) then
!          iterm=iterm/tstep
!        end if
!        iterm=iterm+1
!        call alloc_tempseries(ierr,iterm)
!        if(ierr.ne.0) go to 9800
!
!        call prwmte(wdmunit,20,dsn,lsdat,ledat,iterm,tempseries_g%days, &
!                    tempseries_g%secs,tempseries_g%val,jterm)              !change unit numbers
!        iterm=jterm
!
! ! -- The time series is now copied to a real time series.
!
!        do i=1,MAXSERIES
!          if(.not.series_g(i)%active) go to 515
!        end do
!        write(amessage,510)
! 510    format('no more time series available for data storage - increase MAXSERIES and ', &
!        'recompile program.')
!        go to 9800
!
! 515    allocate(series_g(i)%days(iterm),series_g(i)%secs(iterm),  &
!        series_g(i)%val(iterm),stat=ierr)
!        if(ierr.ne.0)then
!          write(amessage,550)
! 550      format('cannot allocate memory for another time series.')
!          go to 9800
!        end if
!
! ! -- But first we make doubly sure that no terms of the series predate or postdate
! !    DATE_1, TIME_1 or DATE_2, TIME_2 taking into account DEF_TIME.
!
!        iacount=0
!        secsd=-9999
!        if(tcode.ge.4)then
!          if(hhd.ne.-9999)then
!            secsd=numsecs(0,0,0,hhd,nnd,ssd)
!          end if
!        end if
!        if(yy1flag.eq.1)then
!          ibd=numdays(1,1,1970,dd1a,mm1a,yy1a)
!          ibs=numsecs(0,0,0,hh1a,nn1a,ss1a)
! 555      continue
!          if(ibs.ge.86400)then
!            ibs=ibs-86400
!            ibd=ibd+1
!            go to 555
!          end if
!          do j=1,iterm
!            if(secsd.eq.-9999)then
!               itsecs=tempseries_g%secs(j)
!            else
!              itsecs=secsd
!            end if
!            if((tempseries_g%days(j).lt.ibd).or.  &
!               (tempseries_g%days(j).eq.ibd).and.(itsecs.lt.ibs))then
!               tempseries_g%days(j)=-99999999
!               iacount=iacount+1
!            else
!              go to 556
!            end if
!          end do
!        end if
! 556    continue
!        if(yy2flag.eq.1)then
!          ibd=numdays(1,1,1970,dd2a,mm2a,yy2a)
!          ibs=numsecs(0,0,0,hh2a,nn2a,ss2a)
! 557      continue
!          if(ibs.ge.86400)then
!            ibs=ibs-86400
!            ibd=ibd+1
!            go to 557
!          end if
!          do j=iterm,1,-1
!            if(secsd.eq.-9999)then
!               itsecs=tempseries_g%secs(j)
!            else
!              itsecs=secsd
!            end if
!            if((tempseries_g%days(j).gt.ibd).or.  &
!               (tempseries_g%days(j).eq.ibd).and.(itsecs.gt.ibs))then
!               tempseries_g%days(j)=-99999999
!               iacount=iacount+1
!            else
!              go to 558
!            end if
!          end do
!        end if
! 558    continue
!        series_g(i)%active=.true.
!        series_g(i)%name=aname
!        series_g(i)%nterm=iterm-iacount
!        series_g(i)%type='ts'
!        jterm=0
!        if(iacount.eq.0)then
!          if(filter.lt.-1.0e35)then
!            do j=1,iterm
!              series_g(i)%val(j)=tempseries_g%val(j)
!            end do
!            do j=1,iterm
!              series_g(i)%days(j)=tempseries_g%days(j)
!            end do
!            if(secsd.eq.-9999)then
!              do j=1,iterm
!                series_g(i)%secs(j)=tempseries_g%secs(j)
!              end do
!            else
!              do j=1,iterm
!                series_g(i)%secs(j)=secsd
!              end do
!            end if
!          else
!            fspace=abs(3.0*spacing(filter))
!            do j=1,iterm
!              fval=tempseries_g%val(j)
!              if(abs(fval-filter).lt.fspace) go to 620
!              jterm=jterm+1
!              series_g(i)%val(jterm)=fval
!              series_g(i)%days(jterm)=tempseries_g%days(j)
!              if(secsd.eq.-9999)then
!                series_g(i)%secs(jterm)=tempseries_g%secs(j)
!              else
!                series_g(i)%secs(jterm)=secsd
!              end if
! 620          continue
!            end do
!            series_g(i)%nterm=jterm
!          end if
!        else
!          if(filter.lt.-1.0e35)then
!            do j=1,iterm
!              if(tempseries_g%days(j).ne.-99999999)then
!                jterm=jterm+1
!                series_g(i)%val(jterm)=tempseries_g%val(j)
!                series_g(i)%days(jterm)=tempseries_g%days(j)
!                if(secsd.eq.-9999)then
!                  series_g(i)%secs(jterm)=tempseries_g%secs(j)
!                else
!                  series_g(i)%secs(jterm)=secsd
!                end if
!              end if
!            end do
!          else
!            fspace=abs(3.0*spacing(filter))
!            do j=1,iterm
!              fval=tempseries_g%val(j)
!              if(abs(fval-filter).lt.fspace) go to 621
!              if(tempseries_g%days(j).eq.-99999999) go to 621
!              jterm=jterm+1
!              series_g(i)%val(jterm)=fval
!              series_g(i)%days(jterm)=tempseries_g%days(j)
!              if(secsd.eq.-9999)then
!                series_g(i)%secs(jterm)=tempseries_g%secs(j)
!              else
!                series_g(i)%secs(jterm)=secsd
!              end if
! 621          continue
!            end do
!            series_g(i)%nterm=jterm
!          end if
!        end if
!        if(secsd.eq.86400)then
!          do j=1,series_g(i)%nterm
!            series_g(i)%days(j)=series_g(i)%days(j)+1
!            series_g(i)%secs(j)=0
!          end do
!        end if
!        call addquote(afile,sString_g)
!        write(*,580) trim(aname),trim(sString_g)
!        write(LU_REC,580) trim(aname),trim(sString_g)
! 580    format(t5,'Series "',a,'" successfully imported from file ',a)
!        go to 9900
!
!
! 9000   call num2char(iLine_g,aline)
!        call addquote(sInfile_g,sString_g)
!        write(amessage,9010) trim(aline), trim(sString_g)
! 9010   format('cannot read line ',a,' of TSPROC input file ',a)
!        go to 9800
! 9100   continue
!        call addquote(sInfile_g,sString_g)
!        write(amessage,9110) trim(sString_g),trim(currentblock_g)
! 9110   format('unexpected end encountered to TSPROC input file ',a,' while ', &
!        ' reading ',a,' block.')
!        go to 9800
!
! 9800   call write_message(leadspace='yes',error='yes')
!        call write_message(iunit=LU_REC,leadspace='yes',error='yes')
!        ifail=1
!
! 9900   continue
! !       close(unit=wdmunit,iostat=ierr)         !wdm
!        close(unit=99,iostat=ierr)
!        return
!
! end subroutine get_wdm_series
!

end module tsp_input
