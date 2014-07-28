module tsp_output

  use tsp_data_structures
  use tsp_command_processors
  use tsp_utilities

  use tsp_equation_parser
  implicit none


contains

!     Last change:  J     9 Sep 2004   10:39 pm
subroutine pest_files(ifail,lastblock)

! -- Subroutine PEST_FILES generates a PEST input dataset.

       integer, intent(out)   :: ifail
       integer, intent(in)    :: lastblock

! -- General parameters
       logical lexist
       integer ierr,icontext,itempfile,ioseries,iostable,iovtable,iodtable,i,iunit,j, &
       iogtable, iomgtable, &
       jline,numtempfile,ii1,ll,jj1,jj,kk,io,im,noterm,nmterm,iomseries,iomstable,  &
       iomvtable,iomdtable,iout,nsterm,iterm,il,siout,nobs,nobsgp,ieqnerr,nterm,nnterm, &
       isnum,dd,nn,yy,mm,k,ixcon,auiyesno,itempunit,isvd,iaui,itemp
       real rotemp,rmtemp,rprecis,weightmin,weightmax,totim,rtime,eigthresh
       integer obsseries(MAXSERIES),obsstable(MAXSTABLE),obsvtable(MAXVTABLE), &
       obsdtable(MAXDTABLE),obsgtable(MAXGTABLE),modseries(MAXSERIES),modstable(MAXSTABLE), &
       modvtable(MAXVTABLE),moddtable(MAXDTABLE),modgtable(MAXGTABLE)
       real sweightmin(MAXSERIES),sweightmax(MAXSERIES),stweightmin(MAXSTABLE), &
       stweightmax(MAXSTABLE),vtweightmin(MAXVTABLE),vtweightmax(MAXVTABLE), &
       dtweightmin(MAXDTABLE),dtweightmax(MAXDTABLE),gtweightmin(MAXGTABLE),gtweightmax(MAXGTABLE)
       double precision dval,dtempx, sse, obj_fun_value, delta2
       real (kind=T_DBL), dimension(:), allocatable :: tempobsvals, tempsimvals
       character(1)aa
       character(3)auiaa
       character(len=iTSNAMELENGTH) :: aoname,amname,anum,atrans
       character(15)aline,avariable
       character(30)aoption,correct_keyword,last_keyword,atemp,otherblock,aname
       character(120)pardatfile,pestctlfile,instructfile,modcomline,bstring,cstring, &
       micactlfile,pest2micacom
       character(25)acontext(MAXCONTEXT)
       character(12)basename(MAXSERIES+MAXVTABLE+MAXDTABLE+MAXGTABLE),sbasename(MAXSTABLE), &
                    obgnme(MAXSERIES+MAXSTABLE+MAXVTABLE+MAXDTABLE+MAXGTABLE)
       character(120)tempfile(MAXTEMPFILE),modfile(MAXTEMPFILE)
       character(150)sequation(MAXSERIES),stequation(MAXSTABLE),vtequation(MAXVTABLE), &
                     dtequation(MAXDTABLE),gtequation(MAXGTABLE),eqntext

! -- Variable used for dealing with parameter groups.

       integer                   :: igp,f_numpargp,npargp
       real,         allocatable :: f_derinc(:),f_derinclb(:),f_derincmul(:), derinc(:), &
                                    derinclb(:),derincmul(:)
       character(14)             :: apargp
       character(120)            :: pargroupfile
       character(14), allocatable :: f_pargpnme(:),f_inctyp(:),f_forcen(:),f_dermthd(:), &
                                    forcen(:),dermthd(:),pargpnme(:),inctyp(:)

! -- Variable used for dealing with parameter data.

       integer                   :: ipar,f_numpar,npar,tempunit,nnpar
       real, allocatable         :: f_parval1(:),f_parlbnd(:),f_parubnd(:),f_scale(:), &
                                    f_offset(:),parval1(:),parlbnd(:),parubnd(:),      &
                                    scale(:),offset(:)
       character(1)              :: pardelim
       character(12)             :: aapar
       character(12)             :: apar(MAXPAR)
       character(14), allocatable :: f_parnme(:),f_parchglim(:),f_pargp(:), &
                                    parchglim(:),pargp(:)
       character(19), allocatable :: f_partrans(:),partrans(:)

! -- Variables for use in calculating series stats as componenta of a weight equation

       double precision :: dpMin, dpMax, dpSum, dpCount, dpMean, dpVariance
       double precision :: delta, m2, tempmean
       integer :: lc  ! counter, disposable

       ifail=0
       CurrentBlock_g='WRITE_PEST_FILES'
       ieqnerr=0

       write(*,10) trim(CurrentBlock_g)
       write(LU_REC,10) trim(CurrentBlock_g)
10     format(/,' Processing ',a,' block....')
       if(lastblock.ne.201)then
         write(amessage,15)
15       format('a WRITE_PEST_FILES block must immediately follow a LIST_OUTPUT block ', &
         'in a TSPROC input file.')
         go to 9800
       end if

! -- Initialisation

       isvd=0
       iaui=0
       auiyesno=0
       icontext=0
       itempfile=0
       tempfile=' '             ! tempfile is an array
       modfile=' '              ! modfile is an array

       sequation=' '            ! sequation is an array
       stequation=' '           ! stequation is an array
       vtequation=' '           ! vtequation is an array
       dtequation=' '           ! dtequation is an array
       gtequation=' '           ! gtequation is an array

       ioseries=0
       iostable=0
       iovtable=0
       iodtable=0
       iogtable = 0
       iomseries=0
       iomstable=0
       iomvtable=0
       iomdtable=0
       iomgtable = 0

       pardatfile=' '
       pargroupfile=' '
       pestctlfile=' '
       instructfile=' '
       modcomline=' '
       micactlfile=' '
       pest2micacom=' '

       sweightmin=-1.0e36              !sweightmin is an array
       sweightmax= 1.0e36              !sweightman is an array
       stweightmin=-1.0e36             !stweightmin is an array
       stweightmax= 1.0e36             !stweightmax is an array
       vtweightmin=-1.0e36              !vtweightmin is an array
       vtweightmax= 1.0e36             !vtweightmax is an array
       dtweightmin=-1.0e36             !dtweightmin is an array
       dtweightmax= 1.0e36             !dtweightmax is an array
       gtweightmin=-1.0e36             !dtweightmin is an array
       gtweightmax= 1.0e36             !dtweightmax is an array


       f_numpargp=0
       f_numpar=0
       ixcon=0
       iunit=0

! -- The PEST_FILES block is first parsed.

       do
         ILine_g=ILine_g+1
         read(LU_TSPROC_CONTROL,'(a)',err=9000,end=9100) cline
         if( len_trim(cline) == 0 ) cycle
         if(cline(1:1).eq.'#') cycle
         call linesplit(ierr,2)
         if(ierr.ne.0)then
           call num2char(ILine_g,aline)
           call addquote(sInfile_g,sString_g)
           write(amessage,20) trim(aline),trim(sString_g)
20         format('insufficient entries on line ',a,' of file ',a)
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

         if(aoption.eq.'TEMPLATE_FILE')then
           itempfile=itempfile+1
           if(itempfile.gt.MAXTEMPFILE)then
             call num2char(MAXTEMPFILE,aline)
             write (amessage,30) trim(aline),trim(CurrentBlock_g)
30           format('only ',a,' template files can be cited in a ',a,' block.')
             go to 9800
           end if
           call getfile(ierr,cline,tempfile(itempfile),left_word(2),right_word(2))
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,35) trim(aline),trim(sString_g)
35           format('cannot read template file name from line ',a,' of file ',a)
             go to 9800
           end if
           write(*,37) trim(aoption),trim(tempfile(itempfile))
           write(LU_REC,37) trim(aoption),trim(tempfile(itempfile))
37         format(t5,a,' ',a)

         else if(aoption.eq.'MODEL_INPUT_FILE')then
           correct_keyword='TEMPLATE_FILE'
           if(last_keyword.ne.correct_keyword)go to 9300
           call getfile(ierr,cline,modfile(itempfile),left_word(2),right_word(2))
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,40) trim(aline),trim(sString_g)
40           format('cannot read model input file name from line ',a,' of file ',a)
             go to 9800
           end if
           write(*,37) trim(aoption),trim(modfile(itempfile))
           write(LU_REC,37) trim(aoption),trim(modfile(itempfile))

         else if(aoption.eq.'PARAMETER_DATA_FILE')then
           call getfile(ierr,cline,pardatfile,left_word(2),right_word(2))
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,50) trim(aline),trim(sString_g)
50           format('cannot read parameter data file name from line ',a,' of file ',a)
             go to 9800
           end if
           write(*,37) trim(aoption),trim(pardatfile)
           write(LU_REC,37) trim(aoption),trim(pardatfile)

         else if(aoption.eq.'PARAMETER_GROUP_FILE')then
           call getfile(ierr,cline,pargroupfile,left_word(2),right_word(2))
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,52) trim(aline),trim(sString_g)
52           format('cannot read parameter group file name from line ',a,' of file ',a)
             go to 9800
           end if
           write(*,37) trim(aoption),trim(pargroupfile)
           write(LU_REC,37) trim(aoption),trim(pargroupfile)

         else if(aoption.eq.'AUTOMATIC_USER_INTERVENTION')then
           call get_yes_no(ierr,auiyesno)
           if(ierr.ne.0) go to 9800
           if(auiyesno.eq.1)then
             auiaa='yes'
           else
             auiaa='no'
           end if
           iaui=1
           write(*,37) trim(aoption),trim(auiaa)
           write(LU_REC,37) trim(aoption),trim(auiaa)

         else if(aoption.eq.'TRUNCATED_SVD')then
           call get_keyword_value(ierr,2,itemp,eigthresh,aoption)
           if(ierr.ne.0) go to 9800
           if(eigthresh.le.0.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,53) trim(aline),trim(sString_g)
53           format('SVD truncation limit must be positive at line ',a,' of file ',a)
             go to 9800
           end if
           isvd=1

         else if(aoption.eq.'NEW_PEST_CONTROL_FILE')then
           call getfile(ierr,cline,pestctlfile,left_word(2),right_word(2))
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,55) trim(aline),trim(sString_g)
55           format('cannot read pest control file name from line ',a,' of file ',a)
             go to 9800
           end if
           write(*,37) trim(aoption),trim(pestctlfile)
           write(LU_REC,37) trim(aoption),trim(pestctlfile)

         else if(aoption.eq.'NEW_MICA_CONTROL_FILE')then
           call getfile(ierr,cline,micactlfile,left_word(2),right_word(2))
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,155) trim(aline),trim(sString_g)
155          format('cannot read mica control file name from line ',a,' of file ',a)
             go to 9800
           end if
           write(*,37) trim(aoption),trim(micactlfile)
           write(LU_REC,37) trim(aoption),trim(micactlfile)

         else if(aoption.eq.'PEST2MICA_COMMAND')then
           call getfile(ierr,cline,pest2micacom,left_word(2),right_word(2))
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,156) trim(aline),trim(sString_g)
156          format('cannot read PEST2MICA command from line ',a,' of file ',a)
             go to 9800
           end if
           write(*,37) trim(aoption),trim(pest2micacom)
           write(LU_REC,37) trim(aoption),trim(pest2micacom)

         else if(aoption.eq.'NEW_INSTRUCTION_FILE')then
           call getfile(ierr,cline,instructfile,left_word(2),right_word(2))
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,56) trim(aline),trim(sString_g)
56           format('cannot read instruction file name from line ',a,' of file ',a)
             go to 9800
           end if
           write(*,37) trim(aoption),trim(instructfile)
           write(LU_REC,37) trim(aoption),trim(instructfile)

         else if(aoption.eq.'MODEL_COMMAND_LINE')then
           call getfile(ierr,cline,modcomline,left_word(2),right_word(2))
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,57) trim(aline),trim(sString_g)
57           format('cannot read model command line from line ',a,' of file ',a)
             go to 9800
           end if
           write(*,37) trim(aoption),trim(modcomline)
           write(LU_REC,37) trim(aoption),trim(modcomline)

         else if(aoption.eq.'OBSERVATION_SERIES_NAME')then
           ioseries=ioseries+1
           if(ioseries.gt.MAXSERIES)then
             call num2char(MAXSERIES,aline)
             write(amessage,100) trim(aline),trim(CurrentBlock_g)
100          format('a maximum of ',a,' series can be cited in a ',a,' block.')
             go to 9800
           end if
           call get_series_name(ierr,obsseries(ioseries),'OBSERVATION_SERIES_NAME')
           if(ierr.ne.0) go to 9800

         else if(aoption.eq.'OBSERVATION_S_TABLE_NAME')then
           iostable=iostable+1
           if(iostable.gt.MAXSTABLE)then
             call num2char(MAXSTABLE,aline)
             write(amessage,102) trim(aline),trim(CurrentBlock_g)
102          format('a maximum of ',a,' s_tables can be cited in a ',a,' block.')
             go to 9800
           end if
           call get_table_name(ierr,obsstable(iostable),11)
           if(ierr.ne.0) go to 9800

         else if(aoption.eq.'OBSERVATION_C_TABLE_NAME')then
           write(amessage,109)
109        format('current version of TSPROC does not allow C_TABLES to be ', &
           'used for parameter estimation.')
           go to 9800

         else if(aoption.eq.'OBSERVATION_V_TABLE_NAME')then
           iovtable=iovtable+1
           if(iovtable.gt.MAXVTABLE)then
             call num2char(MAXVTABLE,aline)
             write(amessage,103) trim(aline),trim(CurrentBlock_g)
103          format('a maximum of ',a,' v_tables can be cited in a ',a,' block.')
             go to 9800
           end if
           call get_table_name(ierr,obsvtable(iovtable),12)
           if(ierr.ne.0) go to 9800

         else if(aoption.eq.'OBSERVATION_E_TABLE_NAME')then
           iodtable=iodtable+1
           if(iodtable.gt.MAXDTABLE)then
             call num2char(MAXDTABLE,aline)
             write(amessage,104) trim(aline),trim(CurrentBlock_g)
104          format('a maximum of ',a,' e_tables can be cited in a ',a,' block.')
             go to 9800
           end if
           call get_table_name(ierr,obsdtable(iodtable),13)
           if(ierr.ne.0) go to 9800

         else if(aoption.eq.'OBSERVATION_G_TABLE_NAME')then
           iogtable=iogtable+1
           if(iogtable .gt. MAXGTABLE)then
             call num2char(MAXGTABLE,aline)
             write(amessage,804) trim(aline),trim(CurrentBlock_g)
804          format('a maximum of ',a,' g_tables can be cited in a ',a,' block.')
             go to 9800
           end if
           ! after call below, obsgtable(iotable) should contain the
           ! index of the gtable with the user supplied name
           call get_table_name(ierr,obsgtable(iogtable),15)
           if(ierr.ne.0) go to 9800

         else if(aoption.eq.'MODEL_SERIES_NAME')then
           correct_keyword='OBSERVATION_SERIES_NAME'
           if(last_keyword.ne.correct_keyword) go to 9300
           iomseries=iomseries+1
           call get_series_name(ierr,modseries(iomseries),'MODEL_SERIES_NAME')
           if(ierr.ne.0) go to 9800
           if(iomseries.gt.1)then
             do k=1,iomseries-1
               if(modseries(k).eq.modseries(iomseries))then
                 write(amessage,105) trim(series_g(modseries(iomseries))%name)
105              format('time series "',a,'" has been provided as more than one ', &
                 'MODEL_SERIES_NAME.')
                 go to 9800
               end if
             end do
           end if

         else if(aoption.eq.'MODEL_S_TABLE_NAME')then
           correct_keyword='OBSERVATION_S_TABLE_NAME'
           if(last_keyword.ne.correct_keyword) go to 9300
           iomstable=iomstable+1
           call get_table_name(ierr,modstable(iomstable),21)
           if(ierr.ne.0) go to 9800
           if(iomstable.gt.1)then
             do k=1,iomstable-1
               if(modstable(k).eq.modstable(iomstable))then
                 write(amessage,106) trim(stable_g(modstable(iomstable))%name)
106              format('s_table "',a,'" has been provided as more than one ', &
                 'MODEL_S_TABLE_NAME.')
                 go to 9800
               end if
             end do
           end if

         else if(aoption.eq.'MODEL_C_TABLE_NAME')then
           write(amessage,109)
           go to 9800

         else if(aoption.eq.'MODEL_V_TABLE_NAME')then
           correct_keyword='OBSERVATION_V_TABLE_NAME'
           if(last_keyword.ne.correct_keyword) go to 9300
           iomvtable=iomvtable+1
           call get_table_name(ierr,modvtable(iomvtable),22)
           if(ierr.ne.0) go to 9800
           if(iomvtable.gt.1)then
             do k=1,iomvtable-1
               if(modvtable(k).eq.modvtable(iomvtable))then
                 write(amessage,107) trim(vtable_g(modvtable(iomvtable))%name)
107              format('v_table "',a,'" has been provided as more than one ', &
                 'MODEL_V_TABLE_NAME.')
                 go to 9800
               end if
             end do
           end if

         else if(aoption.eq.'MODEL_E_TABLE_NAME')then
           correct_keyword='OBSERVATION_E_TABLE_NAME'
           if(last_keyword.ne.correct_keyword) go to 9300
           iomdtable=iomdtable+1
           call get_table_name(ierr,moddtable(iomdtable),23)
           if(ierr.ne.0) go to 9800
           if(iomdtable.gt.1)then
             do k=1,iomdtable-1
               if(moddtable(k).eq.moddtable(iomdtable))then
                 write(amessage,108) trim(dtable_g(moddtable(iomdtable))%name)
108              format('e_table "',a,'" has been provided as more than one ', &
                 'MODEL_E_TABLE_NAME.')
                 go to 9800
               end if
             end do
           end if

         else if(aoption.eq.'MODEL_G_TABLE_NAME')then
           correct_keyword='OBSERVATION_G_TABLE_NAME'
           if(last_keyword.ne.correct_keyword) go to 9300
           iomgtable=iomgtable+1
           call get_table_name(ierr,modgtable(iomgtable),25)
           if(ierr.ne.0) go to 9800
           if(iomgtable.gt.1)then
             do k=1,iomgtable-1
               if(modgtable(k).eq.modgtable(iomgtable))then
                 write(amessage,308) trim(gtable_g(modgtable(iomgtable))%name)
308              format('g_table "',a,'" has been provided as more than one ', &
                 'MODEL_G_TABLE_NAME.')
                 go to 9800
               end if
             end do
           end if

         else if(aoption.eq.'SERIES_WEIGHTS_EQUATION')then
           correct_keyword='MODEL_SERIES_NAME'
           if(last_keyword.ne.correct_keyword) go to 9300
           call get_equation(ierr,sequation(ioseries),aoption)
           if(ierr.ne.0) go to 9800

         else if(aoption.eq.'S_TABLE_WEIGHTS_EQUATION')then
           correct_keyword='MODEL_S_TABLE_NAME'
           if(last_keyword.ne.correct_keyword) go to 9300
           call get_equation(ierr,stequation(iostable),aoption)
           if(ierr.ne.0) go to 9800

         else if(aoption.eq.'V_TABLE_WEIGHTS_EQUATION')then
           correct_keyword='MODEL_V_TABLE_NAME'
           if(last_keyword.ne.correct_keyword) go to 9300
           call get_equation(ierr,vtequation(iovtable),aoption)
           if(ierr.ne.0) go to 9800

         else if(aoption.eq.'E_TABLE_WEIGHTS_EQUATION')then
           correct_keyword='MODEL_E_TABLE_NAME'
           if(last_keyword.ne.correct_keyword) go to 9300
           call get_equation(ierr,dtequation(iodtable),aoption)
           if(ierr.ne.0) go to 9800

         else if(aoption.eq.'G_TABLE_WEIGHTS_EQUATION')then
           correct_keyword='MODEL_G_TABLE_NAME'
           if(last_keyword.ne.correct_keyword) go to 9300
           call get_equation(ierr,gtequation(iogtable),aoption)
           if(ierr.ne.0) go to 9800

         else if(aoption.eq.'SERIES_WEIGHTS_MIN_MAX')then
           correct_keyword='SERIES_WEIGHTS_EQUATION'
           if(last_keyword.ne.correct_keyword) go to 9300
           call get_two_numbers(ierr,sweightmin(ioseries),sweightmax(ioseries),aoption)
           if(ierr.ne.0) go to 9800
           call check_weight_order(ierr,sweightmin(ioseries),sweightmax(ioseries))
           if(ierr.ne.0) go to 9800

         else if(aoption.eq.'S_TABLE_WEIGHTS_MIN_MAX')then
           correct_keyword='S_TABLE_WEIGHTS_EQUATION'
           if(last_keyword.ne.correct_keyword) go to 9300
           call get_two_numbers(ierr,stweightmin(iostable),stweightmax(iostable),aoption)
           if(ierr.ne.0) go to 9800
           call check_weight_order(ierr,stweightmin(iostable),stweightmax(iostable))
           if(ierr.ne.0) go to 9800

         else if(aoption.eq.'V_TABLE_WEIGHTS_MIN_MAX')then
           correct_keyword='V_TABLE_WEIGHTS_EQUATION'
           if(last_keyword.ne.correct_keyword) go to 9300
           call get_two_numbers(ierr,vtweightmin(iovtable),vtweightmax(iovtable),aoption)
           if(ierr.ne.0) go to 9800
           call check_weight_order(ierr,vtweightmin(iovtable),vtweightmax(iovtable))
           if(ierr.ne.0) go to 9800

         else if(aoption.eq.'E_TABLE_WEIGHTS_MIN_MAX')then
           correct_keyword='E_TABLE_WEIGHTS_EQUATION'
           if(last_keyword.ne.correct_keyword) go to 9300
           call get_two_numbers(ierr,dtweightmin(iodtable),dtweightmax(iodtable),aoption)
           if(ierr.ne.0) go to 9800
           call check_weight_order(ierr,dtweightmin(iodtable),dtweightmax(iodtable))
           if(ierr.ne.0) go to 9800

         else if(aoption.eq.'G_TABLE_WEIGHTS_MIN_MAX')then
           correct_keyword='G_TABLE_WEIGHTS_EQUATION'
           if(last_keyword.ne.correct_keyword) go to 9300
           call get_two_numbers(ierr,gtweightmin(iogtable),gtweightmax(iogtable),aoption)
           if(ierr.ne.0) go to 9800
           call check_weight_order(ierr,gtweightmin(iogtable),gtweightmax(iogtable))
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

         last_keyword=aoption

       end do

200    continue

! -- Any absenses in the block are now looked for.

       if((ioseries.eq.0).and.(iostable.eq.0).and.(iovtable.eq.0).and.   &
          (iodtable.eq.0) .and. (iogtable == 0) )then
         write(amessage,210) trim(CurrentBlock_g)
210      format('no observation series or table names have been cited in ',a,' block.')
         go to 9800
       end if
       if(itempfile.eq.0)then
         write(amessage,220) trim(CurrentBlock_g)
220      format('at least one TEMPLATE_FILE keyword must be provided in ',&
         'a ',a,' block.')
         go to 9800
       end if
       if(pestctlfile.eq.' ')then
         write(amessage,230) trim(CurrentBlock_g)
230      format('a NEW_PEST_CONTROL_FILE keyword must be provided in a ',a,' block.')
         go to 9800
       end if
       if(instructfile.eq.' ')then
         write(amessage,240) trim(CurrentBlock_g)
240      format('NEW_INSTRUCTION_FILE keyword is missing from the ',a,' block.')
         go to 9800
       end if
       if(ioseries.ne.0)then
         if(iomseries.ne.ioseries)then
           write(amessage,241) trim(CurrentBlock_g)
241        format('a MODEL_SERIES_NAME keyword has not been provided for each ', &
             'OBSERVATION_SERIES_NAME cited in the ',a,' block.')
           go to 9800
         end if
         do i=1,ioseries
           if(sequation(i).eq.' ')then
             write(amessage,250) trim(CurrentBlock_g)
250          format('a SERIES_WEIGHTS_EQUATION keyword has not been provided for each ', &
             'series cited in the ',a,' block.')
             go to 9800
           end if
         end do
       end if
       if(iostable.ne.0)then
         if(iomstable.ne.iostable)then
           write(amessage,251) trim(CurrentBlock_g)
251        format('a MODEL_S_TABLE_NAME keyword has not been provided for each ', &
             'OBSERVATION_S_TABLE_NAME cited in the ',a,' block.')
           go to 9800
         end if
         do i=1,iostable
           if(stequation(i).eq.' ')then
             write(amessage,260) trim(CurrentBlock_g)
260          format('an S_TABLE_WEIGHTS_EQUATION keyword has not been provided for each ', &
             's_table cited in the ',a,' block.')
             go to 9800
           end if
         end do
       end if
       if(iovtable.ne.0)then
         if(iomvtable.ne.iovtable)then
           write(amessage,261) trim(CurrentBlock_g)
261        format('a MODEL_V_TABLE_NAME keyword has not been provided for each ', &
             'OBSERVATION_V_TABLE_NAME cited in the ',a,' block.')
           go to 9800
         end if
         do i=1,iovtable
           if(vtequation(i).eq.' ')then
             write(amessage,270) trim(CurrentBlock_g)
270          format('a V_TABLE_WEIGHTS_EQUATION keyword has not been provided for each ', &
             'v_table cited in the ',a,' block.')
             go to 9800
           end if
         end do
       end if

       if(iodtable.ne.0)then
         if(iomdtable.ne.iodtable)then
           write(amessage,271) trim(CurrentBlock_g)
271        format('a MODEL_E_TABLE_NAME keyword has not been provided for each ', &
             'OBSERVATION_E_TABLE_NAME cited in the ',a,' block.')
           go to 9800
         end if

         do i=1,iodtable
           if(dtequation(i).eq.' ')then
             write(amessage,280) trim(CurrentBlock_g)
280          format('a E_TABLE_WEIGHTS_EQUATION keyword has not been provided for each ', &
             'e_table cited in the ',a,'block.')
             go to 9800
           end if
         end do
       end if

       if(iogtable.ne.0)then
         if(iomgtable.ne.iogtable)then
           write(amessage,671) trim(CurrentBlock_g)
671        format('a MODEL_G_TABLE_NAME keyword has not been provided for each ', &
             'OBSERVATION_G_TABLE_NAME cited in the ',a,' block.')
           go to 9800
         end if

         do i=1,iogtable
           if(gtequation(i).eq.' ')then
             write(amessage,680) trim(CurrentBlock_g)
680          format('a G_TABLE_WEIGHTS_EQUATION keyword has not been provided for each ', &
             'g_table cited in the ',a,'block.')
             go to 9800
           end if
         end do
       end if

       if(icontext.eq.0)then
         write(amessage,290) trim(CurrentBlock_g)
290      format('no Context keyword(s) provided in ',a,' block.')
         go to 9800
       end if
       if((micactlfile.ne.' ').and.(pest2micacom.eq.' '))then
         write(amessage,291)
291      format('if a NEW_MICA_CONTROL_FILE keyword is supplied, a PEST2MICA_COMMAND ', &
         'keyword must also be supplied.')
         go to 9800
       end if
       if((isvd.eq.1).and.(iaui.eq.1))then
         write(amessage,292) trim(CurrentBlock_g)
292      format('only one of the TRUNCATED_SVD or AUTOMATIC_USER_INTERVENTION keywords ', &
         'must be supplied in ',a,' block.')
         go to 9800
       end if

! -- Before any processing is done, a check is made that the observation series and
!    tables correspond to the series and tables requested for output in the last
!    LIST_OUTPUT block.

       otherblock='LIST_OUTPUT'
       if((ioseries.ne.iMseries_g).or.(iostable.ne.iMstable_g).or.(iovtable.ne.iMvtable_g).or. &
          (iodtable.ne.iMdtable_g) .or. (iogtable /= iMgtable_g) )then
          write(amessage,1010) trim(CurrentBlock_g),trim(otherblock)
1010      format('the number of series, s_tables, e_tables, g_tables and v_tables cited in the ', &
          a,' block does not correspond exactly to the number of these entities cited in ', &
          'the immediately-preceding ',a,' block.')
          go to 9800
       end if
       if ( iMctable_g .ne. 0 ) then
         write(amessage,1011)
1011     format('a c_table features in the LIST_OUTPUT block preceding ', &
         'the WRITE_PEST_FILES block. The present version of TSPROC does not ', &
         'support the use of c_tables in the calibration process.')
         go to 9800
       end if
       if(ioseries.ne.0)then
         do i=1,ioseries
           io=obsseries(i)
           im=modseries(i)
           aoname=series_g(io)%name
           amname=series_g(im)%name
           if(io.eq.im) go to 1029
           noterm=series_g(io)%nterm
           nmterm=series_g(im)%nterm
           if(noterm.ne.nmterm)then
             write(amessage,1020) trim(aoname),trim(amname)
1020         format('OBSERVATION_SERIES "',a,'" has been matched to ', &
             'MODEL_SERIES "',a,'". However these series have different ', &
             'numbers of terms.')
             go to 9800
           end if
           do j=1,noterm
             if((series_g(io)%days(j).ne.series_g(im)%days(j)).or.   &
                (series_g(io)%secs(j).ne.series_g(im)%secs(j)))then
               write(amessage,1030) trim(aoname),trim(amname)
1030           format('OBSERVATION_SERIES "',a,'" has been matched to ', &
               'MODEL_SERIES "',a,'". However the dates and times in ', &
               'these SERIES do not correspond.')
               go to 9800
             end if
           end do
1029       continue
           do j=1,ioseries
             if(im.eq.iOutseries_g(j)) go to 1035
           end do
           write(amessage,1032) trim(amname),trim(CurrentBlock_g)
1032       format('MODEL__SERIES "',a,'" is not listed in the ', &
           'LIST_OUTPUT block immediately preceding the ',a,' block.')
           go to 9800
1035       continue
         end do
       end if

       if(iostable.ne.0)then
         do i=1,iostable
           io=obsstable(i)
           im=modstable(i)
           aoname=stable_g(io)%name
           amname=stable_g(im)%name
           if(io.eq.im) go to 1039
           if(((stable_g(io)%maximum.lt.-1.0e36).and.(stable_g(im)%maximum.gt.-1.0e36)).or.  &
              ((stable_g(io)%maximum.gt.-1.0e36).and.(stable_g(im)%maximum.lt.-1.0e36)))then
             avariable='MAXIMUM'
             go to 9600
           end if
           if(((stable_g(io)%minimum.lt.-1.0e36).and.(stable_g(im)%minimum.gt.-1.0e36)).or.  &
              ((stable_g(io)%minimum.gt.-1.0e36).and.(stable_g(im)%minimum.lt.-1.0e36)))then
             avariable='MINIMUM'
             go to 9600
           end if
           if(((stable_g(io)%range.lt.-1.0e36).and.(stable_g(im)%range.gt.-1.0e36)).or.  &
              ((stable_g(io)%range.gt.-1.0e36).and.(stable_g(im)%range.lt.-1.0e36)))then
             avariable='RANGE'
             go to 9600
           end if
           if(((stable_g(io)%mean.lt.-1.0e36).and.(stable_g(im)%mean.gt.-1.0e36)).or.   &
              ((stable_g(io)%mean.gt.-1.0e36).and.(stable_g(im)%mean.lt.-1.0e36)))then
             avariable='MEAN'
             go to 9600
           end if
           if(((stable_g(io)%median.lt.-1.0e36).and.(stable_g(im)%median.gt.-1.0e36)).or.   &
              ((stable_g(io)%median.gt.-1.0e36).and.(stable_g(im)%median.lt.-1.0e36)))then
             avariable='MEDIAN'
             go to 9600
           end if
           if(((stable_g(io)%stddev.lt.-1.0e36).and.(stable_g(im)%stddev.gt.-1.0e36)).or.  &
              ((stable_g(io)%stddev.gt.-1.0e36).and.(stable_g(im)%stddev.lt.-1.0e36)))then
             avariable='STD_DEV'
             go to 9600
           end if
           if(((stable_g(io)%total.lt.-1.0e36).and.(stable_g(im)%total.gt.-1.0e36)).or.   &
              ((stable_g(io)%total.gt.-1.0e36).and.(stable_g(im)%total.lt.-1.0e36)))then
             avariable='SUM'
             go to 9600
           end if
           if(((stable_g(io)%minmean.lt.-1.0e36).and.(stable_g(im)%minmean.gt.-1.0e36)).or.   &
              ((stable_g(io)%minmean.gt.-1.0e36).and.(stable_g(im)%minmean.lt.-1.0e36)))then
             avariable='MINMEAN_*'
             go to 9600
           end if
           if(((stable_g(io)%maxmean.lt.-1.0e36).and.(stable_g(im)%maxmean.gt.-1.0e36)).or.   &
              ((stable_g(io)%maxmean.gt.-1.0e36).and.(stable_g(im)%maxmean.lt.-1.0e36)))then
             avariable='MAXMEAN_*'
             go to 9600
           end if
           if((stable_g(io)%maxmean.gt.-1.0e36).or.(stable_g(io)%minmean.gt.-1.0e36))then
             write(amessage,1023)
1023         format('The present version of TSPROC does not support the use of ', &
             'S_TABLE minimum or maximum sample count averages in the calibration process.')
             go to 9800
           end if
1039       continue
           do j=1,iostable
             if(im.eq.iOutStable_g(j)) go to 1038
           end do
           write(amessage,1037) 'S',trim(amname),trim(CurrentBlock_g)
1037       format('MODEL_',a,'_TABLE "',a,'" is not listed in the ', &
           'LIST_OUTPUT block immediately preceding the ',a,' block.')
           go to 9800
1038       continue
         end do
       end if

       if(iovtable.ne.0)then
         do i=1,iovtable
           io=obsvtable(i)
           im=modvtable(i)
           aoname=vtable_g(io)%name
           amname=vtable_g(im)%name
           if(io.eq.im) go to 1047
           noterm=vtable_g(io)%nterm
           nmterm=vtable_g(im)%nterm
           if(noterm.ne.nmterm)then
             write(amessage,1040) trim(aoname),trim(amname)
1040         format('OBSERVATION_V_TABLE "',a,'" has been matched to ', &
             'MODEL_V_TABLE "',a,'". However these V_TABLES ', &
             'have different numbers of integration times.')
             go to 9800
           end if
           do j=1,noterm
             if((vtable_g(io)%days1(j).ne.vtable_g(im)%days1(j)).or.   &
                (vtable_g(io)%days2(j).ne.vtable_g(im)%days2(j)).or.   &
                (vtable_g(io)%secs1(j).ne.vtable_g(im)%secs1(j)).or.   &
                (vtable_g(io)%secs2(j).ne.vtable_g(im)%secs2(j)))then
               write(amessage,1050) trim(aoname),trim(amname)
1050           format('OBSERVATION_V_TABLE "',a,'" has been matched to ', &
               'MODEL_V_TABLE "',a,'". However the integration dates and ', &
               'times in these V_TABLES do not correspond.')
               go to 9800
             end if
           end do
1047       continue
           do j=1,iovtable
             if(im.eq.iOutVtable_g(j)) go to 1048
           end do
           write(amessage,1037) 'V',trim(amname),trim(CurrentBlock_g)
           go to 9800
1048       continue
         end do
       end if

       if(iodtable.ne.0)then
         do i=1,iodtable
           io=obsdtable(i)
           im=moddtable(i)
           aoname=dtable_g(io)%name
           amname=dtable_g(im)%name
           if(io.eq.im) go to 1066  ! was 1079 previously SMW
           noterm=dtable_g(io)%nterm
           nmterm=dtable_g(im)%nterm
           if(noterm.ne.nmterm)then
             write(amessage,1060) trim(aoname),trim(amname)
1060         format('OBSERVATION_E_TABLE "',a,'"  has been matched to ', &
             'MODEL E_TABLE "',a,'". However these E_TABLES ', &
             'have different numbers of flows.')
             go to 9800
           end if
           if(dtable_g(io)%under_over.ne.dtable_g(im)%under_over)then
             write(amessage,1061) trim(aoname),trim(amname)
1061         format('OBSERVATION_E_TABLE "',a,'"  has been matched to ', &
             'MODEL E_TABLE "',a,'". However these E_TABLES ', &
             'have different UNDER_OVER specifications.')
             go to 9800
           end if
           ! check that the sequence and content of each item in the table
           ! is identical between observation and model table
1066       do j=1,noterm
             rotemp=dtable_g(io)%flow(j)
             rmtemp=dtable_g(im)%flow(j)
             rprecis=5*spacing(rmtemp)
             if((rotemp.lt.rmtemp-rprecis).or.(rotemp.gt.rmtemp+rprecis))then
               write(amessage,1070) trim(aoname),trim(amname)
1070           format('OBSERVATION_E_TABLE "',a,'"  has been matched to ', &
               'MODEL E_TABLE "',a,'". However the flows in ', &
               'these E_TABLES do not correspond.')
               go to 9800
             end if
           end do
           do j=1,noterm
             rotemp=dtable_g(io)%tdelay(j)
             rmtemp=dtable_g(im)%tdelay(j)
             rprecis=5*spacing(rmtemp)
             if((rotemp.lt.rmtemp-rprecis).or.(rotemp.gt.rmtemp+rprecis))then
               write(amessage,1071) trim(aoname),trim(amname)
1071           format('OBSERVATION_E_TABLE "',a,'"  has been matched to ', &
               'MODEL E_TABLE "',a,'". However the time delays in ', &
               'these E_TABLES do not correspond.')
               go to 9800
             end if
           end do
! 1079       continue
           do j=1,iodtable
             if(im.eq.iOutDtable_g(j)) go to 1078
           end do
           write(amessage,1037) 'D',trim(amname),trim(CurrentBlock_g)
!1037       format('MODEL_',a,'_TABLE "',a,'" is not listed in the ', &
!           'LIST_OUTPUT block immediately preceding the ',a,' block.')
           go to 9800
1078       continue
         end do
       end if

       ! check for a one-to-one correspondence in the items contained
       ! within each G_TABLE
       if(iogtable.ne.0)then
         do i=1,iogtable
           io=obsgtable(i)
           im=modgtable(i)
           aoname=gtable_g(io)%name
           amname=gtable_g(im)%name
           if(io.eq.im) go to 3066  ! was 3079
           noterm = ubound(gtable_g(io)%sDescription, 1)
           nmterm = ubound(gtable_g(im)%sDescription, 1)
           if(noterm.ne.nmterm)then
             write(amessage,3060) trim(aoname),trim(amname)
3060         format('OBSERVATION_G_TABLE "',a,'"  has been matched to ', &
             'MODEL G_TABLE "',a,'". However these G_TABLES ', &
             'have different numbers of entries.')
             go to 9800
           end if
3066       do j=1,noterm
             if(.not. str_compare(gtable_g(io)%sDescription(j),gtable_g(im)%sDescription(j)) )then
               write(amessage,3070) trim(aoname),trim(amname)
3070           format('OBSERVATION_G_TABLE "',a,'"  has been matched to ', &
               'MODEL G_TABLE "',a,'". However the items in ', &
               'these G_TABLES do not correspond.')
               go to 9800
             end if
           end do
! 3079       continue
           do j=1,iogtable
             if(im.eq.iOutGtable_g(j)) go to 3078
           end do
           write(amessage,1037) 'G',trim(amname),trim(CurrentBlock_g)
!1037       format('MODEL_',a,'_TABLE "',a,'" is not listed in the ', &
!           'LIST_OUTPUT block immediately preceding the ',a,' block.')
           go to 9800
3078       continue
         end do
       end if

! -- If present, the parameter group file is read.

       if(pargroupfile.eq.' ') go to 500
       call addquote(pargroupfile,sString_g)
       write(*,300) trim(sString_g)
       write(LU_REC,300) trim(sString_g)
300    format(t5,'Reading parameter group file ',a,' ....')
       iunit=nextunit()
       open(unit=iunit,file=pargroupfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,310) trim(sString_g)
310      format('cannot open parameter group file ',a)
         go to 9800
       end if

! -- The file is read a first time to find out the number of groups

       jline=0
       f_numpargp=0
       do
         jline=jline+1
         read(iunit,'(a)',err=9400,end=320) cline
         if( len_trim(cline) == 0 ) cycle
         if(cline(1:1).eq.'#') cycle
         f_numpargp=f_numpargp+1
       end do
320    continue
       if(f_numpargp.eq.0)then
         write(amessage,322) trim(sString_g)
322      format('file ',a,' appears to contain no data.')
         go to 9800
       end if
       allocate(f_pargpnme(f_numpargp),f_inctyp(f_numpargp),f_derinc(f_numpargp), &
                f_derinclb(f_numpargp),f_forcen(f_numpargp),f_derincmul(f_numpargp), &
                f_dermthd(f_numpargp),stat=ierr)
       if(ierr.ne.0) go to 9200
       rewind(unit=iunit,iostat=ierr)
       if(ierr.ne.0) go to 9350

! -- Now it is read a second time to obtain the data.

       jline=0
       igp=0
325    jline=jline+1
       call num2char(jline,aline)
       READ(iunit,'(A)',ERR=9400,END=480) cline
       if( len_trim(cline) == 0 ) go to 325
       if(cline(1:1).eq.'#') go to 325
       call casetrans(cline,'lo')
       call linesplit(ierr,7)
       if(ierr.ne.0) go to 9450
       atemp=cline(left_word(1):right_word(1))
       call remchar(atemp,'"')
       IF(len_trim(atemp).GT.12)THEN
         write(amessage,330) trim(atemp),trim(aline),trim(sString_g)
330      format('parameter group name "',a,'" greater than 12 characters ', &
         'at line ',a,' of file ',a)
         go to 9800
       end if
       igp=igp+1
       f_pargpnme(igp)=atemp
       IF(f_pargpnme(igp).EQ.'none') THEN
         write(amessage,340) trim(sString_g)
340      FORMAT('parameter group name "none" in file ',a,' is a reserved ', &
         'name, used for some fixed and tied parameters.')
         go to 9800
       END IF
       f_inctyp(igp)=cline(left_word(2):right_word(2))
       call remchar(f_inctyp(igp),'"')
       if((f_inctyp(igp).ne.'relative').and.(f_inctyp(igp).ne.'absolute').and.  &
          (f_inctyp(igp).ne.'rel_to_max'))then
          write(amessage,350) trim(aline),trim(sString_g)
350       format('INCTYP on line ',a,' of file ',a,' must be ',  &
          '"relative", "absolute" or "rel_to_max".')
          go to 9800
       end if
       call char2num(ierr,cline(left_word(3):right_word(3)),f_derinc(igp))
       if(ierr.ne.0)then
         write(amessage,590) 'DERINC',trim(aline),trim(sString_g)
         go to 9800
       end if
       if(f_derinc(igp).le.0.0)then
         write(amessage,370) 'DERINC',trim(aline),trim(sString_g)
370      format('value for ',a,' on line ',a,' of file ',a,' must be positive.')
         go to 9800
       end if
       call char2num(ierr,cline(left_word(4):right_word(4)),f_derinclb(igp))
       if(ierr.ne.0)then
         write(amessage,590) 'DERINCLB',trim(aline),trim(sString_g)
         go to 9800
       end if
       if(f_derinclb(igp).lt.0.0)then
         write(amessage,390) 'DERINCLB',trim(aline),trim(sString_g)
390      format('value for ',a,' on line ',a,' of file ',a,' must not be negative.')
         go to 9800
       end if
       f_forcen(igp)=cline(left_word(5):right_word(5))
       call remchar(f_forcen(igp),'"')
       if((f_forcen(igp).ne.'switch').and.(f_forcen(igp).ne.'always_2').and.  &
          (f_forcen(igp).ne.'always_3'))then
          write(amessage,400) trim(aline),trim(sString_g)
400       format('FORCEN must be "switch", "always_2" or "always_3" at line ',a,  &
          ' of file ',a)
          go to 9800
       end if
       call char2num(ierr,cline(left_word(6):right_word(6)),f_derincmul(igp))
       if(ierr.ne.0)then
         write(amessage,590) 'DERINCMUL',trim(aline),trim(sString_g)
         go to 9800
       end if
       if(f_derincmul(igp).le.0.0)then
         write(amessage,370) 'DERINCMUL',trim(aline),trim(sString_g)
         go to 9800
       end if
       f_dermthd(igp)=cline(left_word(7):right_word(7))
       call remchar(f_dermthd(igp),'"')
       if((f_dermthd(igp).ne.'parabolic').and.(f_dermthd(igp).ne.'best_fit')  &
          .and.(f_dermthd(igp).ne.'outside_pts'))then
          write(amessage,420) trim(aline),trim(sString_g)
420       format('DERMTHD must be "parabolic", "best_fit" or "outside_pts"',  &
          ' on line ',a,' of file ',a)
          go to 9800
       end if
       go to 325

480    continue

       if(f_numpargp.gt.1)then
         do i=1,f_numpargp-1
           do j=i+1,f_numpargp
             if(f_pargpnme(i).eq.f_pargpnme(j))then
               write(amessage,430) trim(sString_g)
430            format('2 parameter groups have the same name in file ',a)
               go to 9800
             end if
           end do
         end do
       end if

       call num2char(f_numpargp,aline)
       write(*,450) trim(aline),trim(sString_g)
       write(LU_REC,450) trim(aline),trim(sString_g)
450    format(t5,'- data for ',a,' parameter groups read from file ',a)
       close(unit=iunit)

500    continue

! -- If present, the parameter data file is read.

       if(pardatfile.eq.' ') go to 700
       call addquote(pardatfile,sString_g)
       write(*,510) trim(sString_g)
       write(LU_REC,510) trim(sString_g)
510    format(t5,'Reading parameter data file ',a,' ....')
       iunit=nextunit()
       open(unit=iunit,file=pardatfile,status='old',iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,520) trim(sString_g)
520      format('cannot open parameter data file ',a)
         go to 9800
       end if

! -- The file is read a first time to obtain the number of parameters.

       jline=0
       f_numpar=0
       do
         jline=jline+1
         read(iunit,'(a)',err=9400,end=550) cline
         if( len_trim(cline) == 0 ) cycle
         if(cline(1:1).eq.'#') cycle
         f_numpar=f_numpar+1
       end do
550    continue
       if(f_numpar.eq.0)then
         write(amessage,322) trim(sString_g)
         go to 9800
       end if
       allocate(f_parnme(f_numpar),f_partrans(f_numpar),f_parchglim(f_numpar), &
                f_parval1(f_numpar),f_parlbnd(f_numpar),f_parubnd(f_numpar), &
                f_pargp(f_numpar),f_scale(f_numpar),f_offset(f_numpar),stat=ierr)
       if(ierr.ne.0) go to 9200
       rewind(unit=iunit,iostat=ierr)
       if(ierr.ne.0) go to 9350

! -- Now it is read a second time to obtain the data.

       jline=0
       ipar=0
560    jline=jline+1
       call num2char(jline,aline)
       read(iunit,'(A)',ERR=9400,END=620) cline
       if(len_trim(cline) == 0) go to 560
       if(cline(1:1).eq.'#') go to 560
       call casetrans(cline,'lo')
       call linesplit(ierr,9)
       if(ierr.ne.0) go to 9450
       atemp=cline(left_word(1):right_word(1))
       call remchar(atemp,'"')
       IF(len_trim(atemp).GT.12)THEN
         write(amessage,565) trim(atemp),trim(aline),trim(sString_g)
565      format('parameter name "',a,'" greater than 12 characters in length ', &
         'at line ',a,' of file ',a)
         go to 9800
       end if
       ipar=ipar+1
       f_parnme(ipar)=atemp
       f_partrans(ipar)=cline(left_word(2):right_word(2))
       call remchar(f_partrans(ipar),'"')
       if((f_partrans(ipar).ne.'log').and.(f_partrans(ipar).ne.'none').and.  &
          (f_partrans(ipar)(1:4).ne.'tied').and.(f_partrans(ipar).ne.'fixed'))then
          write(amessage,570) trim(aline),trim(sString_g)
570       format('PARTRANS on line ',a,' of file ',a,' must be ',  &
          '"none", "log", "fixed" or "tied_(parameter name)".')
          go to 9800
       end if
       if((f_partrans(ipar).eq.'tied').or.(f_partrans(ipar).eq.'tied_'))then
         write(amessage,572) trim(aline),trim(sString_g)
572      format('the parent parameter name must follow the "tied_" string at line ',a,  &
         ' of file ',a)
         go to 9800
       end if
       f_parchglim(ipar)=cline(left_word(3):right_word(3))
       call remchar(f_parchglim(ipar),'"')
       if((f_parchglim(ipar).ne.'relative').and.(f_parchglim(ipar).ne.'factor'))then
          write(amessage,580) trim(aline),trim(sString_g)
580       format('PARCHGLIM on line ',a,' of file ',a,' must be ',  &
          '"relative" or "factor".')
          go to 9800
       end if
       call char2num(ierr,cline(left_word(4):right_word(4)),f_parval1(ipar))
       if(ierr.ne.0)then
         write(amessage,590) 'PARVAL1',trim(aline),trim(sString_g)
590      format('cannot read value for ',a,' on line ',a,' of file ',a)
         go to 9800
       end if
       call char2num(ierr,cline(left_word(5):right_word(5)),f_parlbnd(ipar))
       if(ierr.ne.0)then
         write(amessage,590) 'PARLBND',trim(aline),trim(sString_g)
         go to 9800
       end if
       call char2num(ierr,cline(left_word(6):right_word(6)),f_parubnd(ipar))
       if(ierr.ne.0)then
         write(amessage,590) 'PARUBND',trim(aline),trim(sString_g)
         go to 9800
       end if
       atemp=cline(left_word(7):right_word(7))
       call remchar(atemp,'"')
       if(len_trim(atemp).GT.12)then
         write(amessage,330) trim(atemp),trim(aline),trim(sString_g)
         go to 9800
       end if
       f_pargp(ipar)=atemp
       call char2num(ierr,cline(left_word(8):right_word(8)),f_scale(ipar))
       if(ierr.ne.0)then
         write(amessage,590) 'SCALE',trim(aline),trim(sString_g)
         go to 9800
       end if
       call char2num(ierr,cline(left_word(9):right_word(9)),f_offset(ipar))
       if(ierr.ne.0)then
         write(amessage,590) 'OFFSET',trim(aline),trim(sString_g)
         go to 9800
       end if
       go to 560

620    continue

! -- Some checks are made of the parameter data.

       if(f_numpar.gt.1)then
         do i=1,f_numpar-1
           do j=i+1,f_numpar
             if(f_parnme(i).eq.f_parnme(j))then
               write(amessage,630) trim(sString_g)
630            format('2 parameters have the same name in file ',a)
               go to 9800
             end if
           end do
         end do
       end if

! -- If any parameters are tied, parameter linkages are now read.

       do ipar=1,f_numpar
         if(f_partrans(ipar)(1:4).eq.'tied')then
           atemp=f_partrans(ipar)(6:)
           if(atemp.eq.f_parnme(ipar))then
             write(amessage,635) trim(atemp),trim(sString_g)
635          format('parameter "',a,'" is tied to itself in file ',a)
             go to 9800
           end if
         end if
       end do

       call num2char(f_numpar,aline)
       write(*,640) trim(aline),trim(sString_g)
       write(LU_REC,640) trim(aline),trim(sString_g)
640    format(t5,'- data for ',a,' parameters read from file ',a)
       close(unit=iunit)

700    continue

! -- Next the names of all parameters are ascertained by reading template files.

        numtempfile=itempfile
   npar=0
   read_template_file: do itempfile=1,numtempfile
     nnpar=0
          tempunit=nextunit()
          call addquote(tempfile(itempfile),sString_g)
          write(*,710) trim(sString_g)
          write(LU_REC,710) trim(sString_g)
710       format(t5,'Reading template file ',a,' ....')
          open(unit=tempunit,file=tempfile(itempfile),status='old',iostat=ierr)
          if(ierr.ne.0)then
            write(amessage,720) trim(sString_g)
720         format('cannot open template file ',a)
            go to 9800
          end if
          jline=1
     read(tempunit,'(a)',err=9400,end=800) cline
          call casetrans(cline,'lo')
     if(cline(1:3).ne.'ptf')then
       write(amessage,730) trim(sString_g)
730       format('"ptf" header missing from first line of file ',a)
            go to 9800
     end if
     pardelim=cline(5:5)
     if((pardelim.eq.' ').or.   &
        (index('1234567890,;:',pardelim).ne.0).or.    &
        (index('abcdefghijklmnopqrstuvwxyz',pardelim).ne.0))then
        write(amessage,740) trim(sString_g)
740        format('invalid parameter delimeter on line 1 of file ',a)
             go to 9800
     end if
     read_a_line: do
       ii1=1
       jline=jline+1
       read(tempunit,'(a)',err=9400,end=800) cline
       ll=len(cline)
745       j=index(cline(ii1:),pardelim)
       if(j.eq.0) cycle read_a_line
       if(j.gt.ll) cycle read_a_line
       ii1=ii1+j-1
       j=0
       if(ii1.le.ll)j=index(cline(ii1+1:),pardelim)
       if(j.eq.0)then
         call num2char(jline,aline)
         write(amessage,750) trim(aline),trim(sString_g)
750         format('unbalanced parameter delimiters on line ',a,  &
         ' of template file ',a)
         go to 9800
       end if
       jj1=ii1+j
       ii1=ii1+1
       jj1=jj1-1
       if(jj1-ii1+1.le.0)then
         call num2char(jline,aline)
         write(amessage,760) trim(aline),trim(sString_g)
760         format('parameter space has zero width at line ',a,   &
         ' of template file ',a)
         go to 9800
       end if
       do jj=ii1,jj1
         if(cline(jj:jj).ne.' ') then
           do kk=jj,jj1
             if(cline(kk:kk).eq.' ') go to 765
           end do
           kk=jj1+1
765           kk=kk-1
           go to 767
         end if
       end do
       call num2char(jline,aline)
       write(amessage,766) trim(aline), trim(sString_g)
766       format('blank parameter space at line ',a,' of template ', &
       'file ',a)
       go to 9800
767       continue
       if(kk-jj+1.gt.12)then
         call num2char(jline,aline)
         write(amessage,768) trim(aline),trim(sString_g)
768         format('parameter name greater than 12 characters in ',  &
         'line ',a,' of template file ',a)
         go to 9800
       end if
            if(cline(kk+1:jj1).ne.' ')then
              call num2char(jline,aline)
              write(amessage,769) trim(aline),trim(sString_g)
769           format('parameter name includes a space character at line ',a,  &
              ' of file ',a)
              go to 9800
            end if
       aapar=cline(jj:kk)
       aapar=adjustl(aapar)
       call casetrans(aapar,'lo')
       if(npar.ne.0)then
         do ipar=1,npar
           if(aapar.eq.apar(ipar)) go to 785
         end do
         npar=npar+1
         nnpar=nnpar+1
         if(npar.gt.MAXPAR)then
                call num2char(MAXPAR,aline)
           write(amessage,780) trim(aline)
780           format('number of parameters cited in template files is limited ', &
                'to ',a,'. Increase MAXPAR and re-compile program.')
           go to 9800
         end if
         apar(npar)=aapar
       else
         npar=1
         nnpar=1
         apar(npar)=aapar
       end if
785       ii1=jj1+2
       go to 745
     end do read_a_line
800     continue
     call num2char(nnpar,aline)
          if(itempfile.eq.1)then
       write(*,795) trim(aline),trim(sString_g)
            write(LU_REC,795) trim(aline),trim(sString_g)
795       format(t5,'- ',a,' parameter names read from file ',a)
          else
       write(*,796) trim(aline),trim(sString_g)
            write(LU_REC,796) trim(aline),trim(sString_g)
796       format(t5,'- ',a,' more parameter names read from file ',a)
          end if
     close(unit=tempunit,err=9500)
   end do read_template_file

! -- Observations are named and the instruction file is now written.

       nobs=0
       nobsgp=0

       iunit=nextunit()
       call addquote(instructfile,sString_g)
       write(*,810) trim(sString_g)
       write(LU_REC,810) trim(sString_g)
810    format(t5,'Writing instruction file ',a,' ....')
       inquire(file=instructfile,exist=lexist)
       if(lexist)then
         write(6,*)
813      write(*,815,advance='no') trim(sString_g)
815      format(' File ',a,' already exists. Overwrite it? [y/n]: ')
         read(5,'(a)') aa
         call casetrans(aa,'lo')
         if((aa.ne.'y').and.(aa.ne.'n')) go to 813
         if(aa.eq.'n')then
           write(*,820)
           write(LU_REC,820)
820        format(/,' Execution terminated so file not overwritten.')
           ifail=1
           return
         end if
       end if
       open(unit=iunit,file=instructfile,iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,830) trim(sString_g)
830      format('cannot open file ',a,' for output.')
         go to 9800
       end if
       write(iunit,840)
840    format('pif $')

! -- First the time series instructions are written.

       iout=0
       if(ioseries.eq.0) go to 1100
       do i=1,ioseries
         iout=iout+1
         im=iOutseries_g(i)
         do j=1,ioseries
           if(im.eq.modseries(j)) go to 860
         end do
         write(amessage,850) trim(series_g(im)%name)
850      format('time series "',a,'" cited in the LIST_OUTPUT block immediately ', &
         'preceding the WRITE_PEST_FILES block is not cited as a ', &
         'MODEL_SERIES_NAME in the latter block.')
         go to 9800
860      io=obsseries(j)
         nsterm=series_g(io)%nterm
         aname=series_g(im)%name
         nobsgp=nobsgp+1
         obgnme(nobsgp)=aname
         call make_basename(ierr,iout,nsterm,aname,basename)
         if(ierr.ne.0) go to 9800
         atemp=basename(iout)
         do iterm=1,nsterm
           call num2char(iterm,anum)
           if(trim(sSeriesFormat_g) == "ssf" &
             .or. trim(sSeriesFormat_g) == "long" )then
             aname='['//trim(atemp)//trim(anum)//']42:65'
           else
             aname='['//trim(atemp)//trim(anum)//']2:25'
           end if
           if(iterm.eq.1 .and. trim(sSeriesFormat_g) /= "ssf" )then
             write(iunit,"('l3',t6,a)") trim(aname)
           else
             write(iunit,"('l1',t6,a)") trim(aname)
           end if
           nobs=nobs+1
         end do
       end do

! -- Next the S_TABLE instructions are written.

1100   continue
       if(iostable.eq.0) go to 1200
       siout=0
       do i=1,iostable
         il=0
         siout=siout+1
         im=iOutStable_g(i)
         do j=1,iostable
           if(im.eq.modstable(j)) go to 1120
         end do
         write(amessage,1110) 's',trim(stable_g(im)%name),'S'
1110     format(a,'_table "',a,'" cited in the LIST_OUTPUT block immediately ', &
         'preceding the WRITE_PEST_FILES block is not cited as a ', &
         'MODEL_',a,'_TABLE_NAME in the latter block.')
         go to 9800
1120     io=obsstable(j)
         aname=stable_g(im)%name
         nobsgp=nobsgp+1
         obgnme(nobsgp)=aname
         sbasename(siout)=aname(1:12)
         if(siout.gt.1)then
           do j=1,siout-1
             if(sbasename(j).eq.sbasename(siout))then
               write(amessage,1130)
1130           format('TSPROC cannot generate unique observation names from the ',  &
               'names of the MODEL_S_TABLES involved in the ', &
               'calibration process. Alter the first twelve letters of at least one ', &
               'of the model S_TABLE names.')
               go to 9800
             end if
           end do
         end if
         if(stable_g(io)%maximum.gt.-1.0e36)then
           il=il+1
           aname='['//trim(sbasename(siout))//OBSCHAR//'max]51:69'
           if(il.eq.1)then
             write(iunit,1140) trim(aname)
1140         format('l11',t6,a)
           else
             write(iunit,1150) trim(aname)
1150         format('l1',t6,a)
           end if
           nobs=nobs+1
         end if
         if(stable_g(io)%minimum.gt.-1.0e36)then
           il=il+1
           aname='['//trim(sbasename(siout))//OBSCHAR//'min]51:69'
           if(il.eq.1)then
             write(iunit,1140) trim(aname)
           else
             write(iunit,1150) trim(aname)
           end if
           nobs=nobs+1
         end if
         if(stable_g(io)%range.gt.-1.0e36)then
           il=il+1
           aname='['//trim(sbasename(siout))//OBSCHAR//'range]51:69'
           if(il.eq.1)then
             write(iunit,1140) trim(aname)
           else
             write(iunit,1150) trim(aname)
           end if
           nobs=nobs+1
         end if

         if(stable_g(io)%total.gt.-1.0e36)then
           il=il+1
           aname='['//trim(sbasename(siout))//OBSCHAR//'sum]51:69'
           if(il.eq.1)then
             write(iunit,1140) trim(aname)
           else
             write(iunit,1150) trim(aname)
           end if
           nobs=nobs+1
         end if
         if(stable_g(io)%mean.gt.-1.0e36)then
           il=il+1
           aname='['//trim(sbasename(siout))//OBSCHAR//'mean]51:69'
           if(il.eq.1)then
             write(iunit,1140) trim(aname)
           else
             write(iunit,1150) trim(aname)
           end if
           nobs=nobs+1
         end if
         if(stable_g(io)%median.gt.-1.0e36)then
           il=il+1
           aname='['//trim(sbasename(siout))//OBSCHAR//'median]51:69'
           if(il.eq.1)then
             write(iunit,1140) trim(aname)
           else
             write(iunit,1150) trim(aname)
           end if
           nobs=nobs+1
         end if
         if(stable_g(io)%stddev.gt.-1.0e36)then
           il=il+1
           aname='['//trim(sbasename(siout))//OBSCHAR//'sd]51:69'
           if(il.eq.1)then
             write(iunit,1140) trim(aname)
           else
             write(iunit,1150) trim(aname)
           end if
           nobs=nobs+1
         end if
       end do

! -- Next the V_TABLE instructions are written.

1200   continue

       if(iovtable.eq.0) go to 1300
       do i=1,iovtable
         iout=iout+1
         im=iOutVtable_g(i)
         do j=1,iovtable
           if(im.eq.modvtable(j)) go to 1220
         end do
         write(amessage,1110) 'v',trim(vtable_g(im)%name),'V'
         go to 9800
1220     io=obsvtable(j)
         nsterm=vtable_g(io)%nterm
         aname=vtable_g(im)%name
         nobsgp=nobsgp+1
         obgnme(nobsgp)=aname
         call make_basename(ierr,iout,nsterm,aname,basename)
         if(ierr.ne.0) go to 9800
         atemp=basename(iout)
         do iterm=1,nsterm
           call num2char(iterm,anum)
           aname='['//trim(atemp)//trim(anum)//']62:81'
           if(iterm.eq.1)then
             write(iunit,1230) trim(aname)
1230         format('l4',t6,a)
           else
             write(iunit,1240) trim(aname)
1240         format('l1',t6,a)
           end if
           nobs=nobs+1
         end do
       end do

! -- Next the E_TABLE instructions are written.

1300   continue

       if(iodtable.eq.0) go to 4300
       do i=1,iodtable
         iout=iout+1
         im=iOutDtable_g(i)
         do j=1,iodtable
           if(im.eq.moddtable(j)) go to 1320
         end do
         write(amessage,1110) 'e',trim(vtable_g(im)%name),'E'
         go to 9800
1320     io=obsdtable(j)
         nsterm=dtable_g(io)%nterm
         aname=dtable_g(im)%name
         nobsgp=nobsgp+1
         obgnme(nobsgp)=aname                                  !!!*** is this correct??
         call make_basename(ierr,iout,nsterm,aname,basename)
         if(ierr.ne.0) go to 9800
         atemp=basename(iout)
         do iterm=1,nsterm
           call num2char(iterm,anum)
           aname='['//trim(atemp)//trim(anum)//']59:78'
           if(iterm.eq.1)then
             write(iunit,1230) trim(aname)
           else
             write(iunit,1240) trim(aname)
           end if
           nobs=nobs+1
         end do
       end do

! -- Next the G_TABLE instructions are written.

4300   continue

       if(iogtable.eq.0) go to 1400
       do i=1,iogtable
         iout=iout+1
         im=iOutGtable_g(i)
         do j=1,iogtable
           if(im.eq.modgtable(j)) go to 4320
         end do
         write(amessage,1110) 'g',trim(gtable_g(im)%name),'G'
         go to 9800
4320     io=obsgtable(j)
         nsterm=ubound(gtable_g(io)%sDescription, 1)
         aname=gtable_g(im)%name
         nobsgp=nobsgp+1
         obgnme(nobsgp)=aname                                  !!!*** is this correct??
         call make_basename(ierr,iout,nsterm,aname,basename)
         if(ierr.ne.0) go to 9800
         atemp=basename(iout)
         do iterm=1,nsterm
           call num2char(iterm,anum)
           aname='['//trim(atemp)//trim(anum)//']82:96'
           if(iterm.eq.1)then
             write(iunit,1230) trim(aname)
           else
             write(iunit,1240) trim(aname)
           end if
           nobs=nobs+1
         end do
       end do

1400   continue
       close(unit=iunit)
       write(*,1410) trim(sString_g)
       write(LU_REC,1410) trim(sString_g)
1410   format(t5,'- file ',a,' written ok.')


! -- Parameter and parameter group data are now assimilated on the basis of information
!    read from the parameter data file, the parameter group file and the template files.

       allocate(partrans(npar),parchglim(npar),parval1(npar),parlbnd(npar),  &
                parubnd(npar),pargp(npar),scale(npar),offset(npar), stat=ierr)
       if(ierr.ne.0) go to 9200

       allocate(pargpnme(npar),inctyp(npar),derinc(npar),derinclb(npar),forcen(npar), &
       derincmul(npar),dermthd(npar), stat=ierr)
       if(ierr.ne.0) go to 9200

       do ipar=1,npar
         aapar=apar(ipar)
         if(f_numpar.ne.0)then
           do j=1,f_numpar
             if(aapar.eq.f_parnme(j))then
               partrans(ipar)=f_partrans(j)
               parchglim(ipar)=f_parchglim(j)
               parval1(ipar)=f_parval1(j)
               parlbnd(ipar)=f_parlbnd(j)
               parubnd(ipar)=f_parubnd(j)
               pargp(ipar)=f_pargp(j)
               scale(ipar)=f_scale(j)
               offset(ipar)=f_offset(j)
               go to 1450
             end if
           end do
         end if
         write(amessage,1449)trim(aapar)
1449     format('Parameter "', A, '" was found in the template file ',      &
            'but was not in the parameter file.')
         goto 9800
1450     continue
       end do

! -- If any parameters are tied to a parameter which does not exist, this is now
!    rectified.

       do ipar=1,npar
         if(partrans(ipar)(1:4).eq.'tied') then
           aapar=partrans(ipar)(6:)
           do i=1,npar
             if(aapar.eq.apar(i)) go to 1470
           end do
           partrans(ipar)='none'
1470       continue
         end if
       end do

! -- Parameter groups are now organised.

       npargp=0
       do ipar=1,npar
         apargp=pargp(ipar)
         if(apargp.eq.'none') then
           if((partrans(ipar).ne.'tied').and.(partrans(ipar).ne.'fixed'))then
             call addquote(pardatfile,sString_g)
              write(amessage,1471)trim(apar(ipar)),trim(sString_g)
1471         format('parameter "',a,'" has been assigned to parameter group "none" ', &
             'in file ',a,' but is not tied or fixed.')
             go to 9800
           else
             go to 1500
           end if
         end if
         if(ipar.ne.1)then
           do i=1,ipar-1
             if(pargp(i).eq.apargp) go to 1500
           end do
         end if
         if(f_numpargp.ne.0)then
           do i=1,f_numpargp
             if(apargp.eq.f_pargpnme(i))then
               npargp=npargp+1
               pargpnme(npargp)=f_pargpnme(i)
               inctyp(npargp)=f_inctyp(i)
               derinc(npargp)=f_derinc(i)
               derinclb(npargp)=f_derinclb(i)
               forcen(npargp)=f_forcen(i)
               derincmul(npargp)=f_derincmul(i)
               dermthd(npargp)=f_dermthd(i)
               go to 1500
             end if
           end do
         end if
         npargp=npargp+1
         pargpnme(npargp)=apargp
         inctyp(npargp)='relative'
         derinc(npargp)=0.01
         derinclb(npargp)=0.00
         forcen(npargp)='switch'
         derincmul(npargp)=2.0
         dermthd(npargp)='parabolic'
1500     continue
       end do

! -- The "* control data" section of the PEST control file is now written.

       iunit=nextunit()
       call addquote(pestctlfile,sString_g)
       write(*,1510) trim(sString_g)
       write(LU_REC,1510) trim(sString_g)
1510   format(t5,'Writing PEST control file ',a,' ....')
       inquire(file=pestctlfile,exist=lexist)
       if(lexist)then
         write(6,*)
1520     write(*,815,advance='no') trim(sString_g)
         read(5,'(a)') aa
         call casetrans(aa,'lo')
         if((aa.ne.'y').and.(aa.ne.'n')) go to 1520
         if(aa.eq.'n')then
           write(*,820)
           write(LU_REC,820)
           ifail=1
           return
         end if
       end if
       open(unit=iunit,file=pestctlfile,iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,830) trim(sString_g)
         go to 9800
       end if
       write(iunit,1530)
1530   format('pcf')
       write(iunit,1540)
1540   format('* control data')
       write(iunit,1550)
1550   format('restart estimation')
       write(iunit,1560) npar,nobs,npargp,0,nobsgp
1560   format(5i7)
       write(iunit,1570) numtempfile,1
1570   format(2i6,'   single   point   1   0   0')
       if(isvd.eq.0)then
         write(iunit,1580)
1580     format('10.0   2.0    0.3    0.03    10  999')
       else
         write(iunit,1581)
1581     format('10.0  -3.0    0.3    0.03     1  999')
       end if
       write(iunit,1590)
1590   format('5.0   5.0   1.0e-3')
       if(auiyesno.eq.0)then
         write(iunit,1600)
1600     format('0.1  noaui')
       else
         write(iunit,1601)
1601   format('0.1   aui')
       end if
       write(iunit,1610)
1610   format('30   .005  4   4  .005   4')
       write(iunit,1620)
1620   format('1    1    1')
       if(isvd.eq.1)then
         write(iunit,1621)
1621     format('* singular value decomposition')
         write(iunit,1622)
1622     format('1')
         write(iunit,1633) npar,eigthresh
1633     format(i6,2x,1pg13.7)
         write(iunit,1634)
1634     format('1')
       end if

! -- The "* parameter groups" section of the PEST control file is now written.

       write(iunit,1630)
1630   format('* parameter groups')
       do igp=1,npargp
         write(iunit,1640)trim(pargpnme(igp)),trim(inctyp(igp)),derinc(igp), &
         derinclb(igp),trim(forcen(igp)),derincmul(igp), trim(dermthd(igp))
1640     format(a,t14,a,t27,1pg12.5,t41,1pg12.5,t55,a,t66,1pg12.5,2x,a)
       end do

! -- The "* parameter data" section of the PEST control file is now written.

       write(iunit,1650)
1650   format('* parameter data')
       do ipar=1,npar
         if(partrans(ipar)(1:4).eq.'tied')then
           atrans='tied'
         else
           atrans=partrans(ipar)
         end if
         write(iunit,1660) trim(apar(ipar)),trim(atrans),   &
         trim(parchglim(ipar)),parval1(ipar), &
         parlbnd(ipar),parubnd(ipar),trim(pargp(ipar)),scale(ipar),offset(ipar)
1660     format(a,t14,a,t21,a,t33,1pg12.5,t47,1pg12.5,t61,1pg12.5,t75,a,t89,1pg12.5,  &
         t103,1pg12.5,t117,'  1')
       end do
       do ipar=1,npar
         if(partrans(ipar)(1:4).eq.'tied')then
           write(iunit,1670) trim(apar(ipar)),trim(partrans(ipar)(6:))
1670       format(a,t14,a)
         end if
       end do

! -- The "* observation groups" section of the PEST control file is now written.

       write(iunit,1690)
1690   format('* observation groups')
       do i=1,nobsgp
         write(iunit,1700) trim(obgnme(i))
1700     format(a)
       end do

! -- The "* observation data" section of the PEST control file is now written.
! -- First the time series observations are dealt with.

       write(iunit,1705)
1705   format('* observation data')

       iout=0
       ieqnerr=0
       if(ioseries.eq.0) go to 2100
       do i=1,ioseries
         iout=iout+1
         im=iOutseries_g(i)
         do j=1,ioseries
           if(im.eq.modseries(j)) go to 1860
         end do
         write(amessage,850)
         go to 9800
1860     io=obsseries(j)
         nsterm=series_g(io)%nterm
         aname=series_g(im)%name

         allocate(tempobsvals(nsterm))
         allocate(tempsimvals(nsterm))

         if (series_g(io)%lIsSinglePrecision) then
           tempobsvals = real(series_g(io)%val, kind=T_DBL)
           tempsimvals = real(series_g(im)%val, kind=T_DBL)
         else
           tempobsvals = series_g(io)%dpval
           tempsimvals = series_g(im)%dpval
         endif

         tempmean = 0.
         m2 = 0.
         sse = 0.

         do lc = 1,nsterm
           delta = tempobsvals(lc) - tempmean
           delta2 = tempobsvals(lc) - tempsimvals(lc)
           tempmean = tempmean + delta / real(lc, kind=8)
           sse = sse + delta2**2
           m2 = m2 + delta * ( tempobsvals(lc) - tempmean )
         enddo

         obj_fun_value = sqrt(sse)

         ! SMW additions August 2013
         dpCount = real(nsterm, kind=8)
         dpSum = sum( tempobsvals )
         dpMin = minval( tempobsvals )
         dpMax = maxval( tempobsvals )
         dpMean = tempmean
         dpVariance = m2 / (nsterm -1)

         call make_basename(ierr,iout,nsterm,aname,basename)
         atemp=basename(iout)
         weightmin=max(sweightmin(j),0.0)
         weightmax=min(sweightmax(j),1.0e36)

! -- The pertinent equation is parsed and prepared.

         eqntext=sequation(j)
         call prepare_eqn(ierr,nterm,sequation(j),io)
         if(ierr.ne.0) then
           ieqnerr=1
           go to 9800
         end if
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
           call num2char(j,anum)
           aname=trim(atemp)//trim(anum)

! -- First the series numbers in the equation terms are replaced by series values.

           do iterm =1,nterm
             if(aterm(iterm)(1:3).eq.'$~$') then
               call char2num(ierr,aterm(iterm)(4:),isnum)
               rterm(iterm)=tempobsvals(j)
               aterm(iterm)='~!~'
             end if
           end do

! -- The weights equation instinsic function evaluations is carried out if necessary.

           do iterm =1,nterm
             if(aterm(iterm)(1:3).eq.'@_2') then
               rterm(iterm)=abs(tempobsvals(j))
               aterm(iterm)='~!~'

             elseif(aterm(iterm)(1:3).eq.'@_4') then       ! min
               rterm(iterm)=dpMin
               aterm(iterm)='~!~'

             elseif(aterm(iterm)(1:3).eq.'@_5') then       ! max
               rterm(iterm)=dpMax
               aterm(iterm)='~!~'

             elseif(aterm(iterm)(1:3).eq.'@_6') then       ! obj fun value
               rterm(iterm)=obj_fun_value
               aterm(iterm)='~!~'

             elseif(aterm(iterm)(1:3).eq.'@_7') then       ! count
               rterm(iterm)=dpCount
               aterm(iterm)='~!~'

             elseif(aterm(iterm)(1:3).eq.'@_8') then       ! mean
               rterm(iterm)=dpMean
               aterm(iterm)='~!~'

             elseif(aterm(iterm)(1:3).eq.'@_9') then       ! variance
               rterm(iterm)=dpVariance
               aterm(iterm)='~!~'


             else if(aterm(iterm)(1:3).eq.'@_1') then
!               call newdate(series_g(io)%days(j),1,1,1970,dd,mm,yy)
               call gregorian_date(iJD=series_g(io)%days(j), &
                               iMonth=mm, &
                               iDay=dd, &
                               iYear=yy)
               nn=numdays(1,1,yy,dd,mm,yy)
               rtime=float(nn)+float(series_g(io)%secs(j))/86400.0
               rterm(iterm)=rtime
               aterm(iterm)='~!~'
             else if(aterm(iterm)(1:3).eq.'@_3') then
               call char2num(ierr,aterm(iterm)(5:),dtempx)
               rterm(iterm)=dble(series_g(io)%days(j))+     &
                            dble(series_g(io)%secs(j))/86400.0d0-dtempx
               aterm(iterm)='~!~'
             end if
           end do

           call EVALUATE(ierr,MAXTERM,NTERM,NOPER,NFUNCT,ATERM,BTERM,   &
           OPERAT,FUNCT,IORDER,DVAL,rterm)
           if(ierr.ne.0) go to 9800
           if(dval.lt.weightmin)dval=weightmin
           if(dval.gt.weightmax)dval=weightmax
           write(iunit,1900) trim(aname),tempobsvals(j),dval,trim(series_g(im)%name)
1900       format(a,t22,1pg14.7,t40,1pg12.6,2x,a)
         end do

         if (allocated(tempobsvals)) deallocate(tempobsvals)
         if (allocated(tempsimvals)) deallocate(tempsimvals)

       end do

! -- Now we handle S_TABLE observations.

2100   continue
       if(iostable.eq.0) go to 2200
       siout=0
       do i=1,iostable
         siout=siout+1
         im=iOutStable_g(i)
         do j=1,iostable
           if(im.eq.modstable(j)) go to 2120
         end do
         write(amessage,1110) 's',trim(stable_g(im)%name),'S'
         go to 9800
2120     io=obsstable(j)
         aname=stable_g(im)%name
         sbasename(siout)=aname(1:12)
         weightmin=max(stweightmin(j),0.0)
         weightmax=min(stweightmax(j),1.0e36)
         eqntext=stequation(j)
         call prepare_eqn(ierr,nterm,stequation(j),0)
         if(ierr.ne.0) then
           ieqnerr=1
           go to 9800
         end if
         nnterm=nterm
         do iterm=1,nterm
           cterm(iterm)=aterm(iterm)
         end do
         do iterm=1,nterm
           qterm(iterm)=rterm(iterm)
         end do

         if(stable_g(io)%maximum.gt.-1.0e36)then
           aname=trim(sbasename(siout))//OBSCHAR//'max'
           nterm=nnterm
           do iterm=1,nterm
             aterm(iterm)=cterm(iterm)
           end do
           do iterm=1,nterm
             rterm(iterm)=qterm(iterm)
           end do
           do iterm =1,nterm
             if(aterm(iterm)(1:3).eq.'@_2') then
               rterm(iterm)=abs(stable_g(io)%maximum)
               aterm(iterm)='~!~'
             end if
           end do
           call EVALUATE(ierr,MAXTERM,NTERM,NOPER,NFUNCT,ATERM,BTERM,   &
           OPERAT,FUNCT,IORDER,DVAL,rterm)
           if(dval.lt.weightmin)dval=weightmin
           if(dval.gt.weightmax)dval=weightmax
           write(iunit,1900) trim(aname),stable_g(io)%maximum,dval,trim(stable_g(im)%name)
         end if

         if(stable_g(io)%minimum.gt.-1.0e36)then
           aname=trim(sbasename(siout))//OBSCHAR//'min'
           nterm=nnterm
           do iterm=1,nterm
             aterm(iterm)=cterm(iterm)
           end do
           do iterm=1,nterm
             rterm(iterm)=qterm(iterm)
           end do
           do iterm =1,nterm
             if(aterm(iterm)(1:3).eq.'@_2') then
               rterm(iterm)=abs(stable_g(io)%minimum)
               aterm(iterm)='~!~'
             end if
           end do
           call EVALUATE(ierr,MAXTERM,NTERM,NOPER,NFUNCT,ATERM,BTERM,   &
           OPERAT,FUNCT,IORDER,DVAL,rterm)
           if(dval.lt.weightmin)dval=weightmin
           if(dval.gt.weightmax)dval=weightmax
           write(iunit,1900) trim(aname),stable_g(io)%minimum,dval,trim(stable_g(im)%name)
         end if

         if(stable_g(io)%range.gt.-1.0e36)then
           aname=trim(sbasename(siout))//OBSCHAR//'range'
           nterm=nnterm
           do iterm=1,nterm
             aterm(iterm)=cterm(iterm)
           end do
           do iterm=1,nterm
             rterm(iterm)=qterm(iterm)
           end do
           do iterm =1,nterm
             if(aterm(iterm)(1:3).eq.'@_2') then
               rterm(iterm)=abs(stable_g(io)%range)
               aterm(iterm)='~!~'
             end if
           end do
           call EVALUATE(ierr,MAXTERM,NTERM,NOPER,NFUNCT,ATERM,BTERM,   &
           OPERAT,FUNCT,IORDER,DVAL,rterm)
           if(dval.lt.weightmin)dval=weightmin
           if(dval.gt.weightmax)dval=weightmax
           write(iunit,1900) trim(aname),stable_g(io)%range,dval,trim(stable_g(im)%name)
         end if

         if(stable_g(io)%total.gt.-1.0e36)then
           aname=trim(sbasename(siout))//OBSCHAR//'sum'
           nterm=nnterm
           do iterm=1,nterm
             aterm(iterm)=cterm(iterm)
           end do
           do iterm=1,nterm
             rterm(iterm)=qterm(iterm)
           end do
           do iterm =1,nterm
             if(aterm(iterm)(1:3).eq.'@_2') then
               rterm(iterm)=abs(stable_g(io)%total)
               aterm(iterm)='~!~'
             end if
           end do
           call EVALUATE(ierr,MAXTERM,NTERM,NOPER,NFUNCT,ATERM,BTERM,   &
           OPERAT,FUNCT,IORDER,DVAL,rterm)
           if(dval.lt.weightmin)dval=weightmin
           if(dval.gt.weightmax)dval=weightmax
           write(iunit,1900) trim(aname),stable_g(io)%total,dval,trim(stable_g(im)%name)
         end if

         if(stable_g(io)%mean.gt.-1.0e36)then
           aname=trim(sbasename(siout))//OBSCHAR//'mean'
           nterm=nnterm
           do iterm=1,nterm
             aterm(iterm)=cterm(iterm)
           end do
           do iterm=1,nterm
             rterm(iterm)=qterm(iterm)
           end do
           do iterm =1,nterm
             if(aterm(iterm)(1:3).eq.'@_2') then
               rterm(iterm)=abs(stable_g(io)%mean)
               aterm(iterm)='~!~'
             end if
           end do
           call EVALUATE(ierr,MAXTERM,NTERM,NOPER,NFUNCT,ATERM,BTERM,   &
           OPERAT,FUNCT,IORDER,DVAL,rterm)
           if(dval.lt.weightmin)dval=weightmin
           if(dval.gt.weightmax)dval=weightmax
           write(iunit,1900) trim(aname),stable_g(io)%mean,dval,trim(stable_g(im)%name)
         end if

         if(stable_g(io)%median.gt.-1.0e36)then
           aname=trim(sbasename(siout))//OBSCHAR//'median'
           nterm=nnterm
           do iterm=1,nterm
             aterm(iterm)=cterm(iterm)
           end do
           do iterm=1,nterm
             rterm(iterm)=qterm(iterm)
           end do
           do iterm =1,nterm
             if(aterm(iterm)(1:3).eq.'@_2') then
               rterm(iterm)=abs(stable_g(io)%median)
               aterm(iterm)='~!~'
             end if
           end do
           call EVALUATE(ierr,MAXTERM,NTERM,NOPER,NFUNCT,ATERM,BTERM,   &
           OPERAT,FUNCT,IORDER,DVAL,rterm)
           if(dval.lt.weightmin)dval=weightmin
           if(dval.gt.weightmax)dval=weightmax
           write(iunit,1900) trim(aname),stable_g(io)%median,dval,trim(stable_g(im)%name)
         end if

         if(stable_g(io)%stddev.gt.-1.0e36)then
           aname=trim(sbasename(siout))//OBSCHAR//'sd'
           nterm=nnterm
           do iterm=1,nterm
             aterm(iterm)=cterm(iterm)
           end do
           do iterm=1,nterm
             rterm(iterm)=qterm(iterm)
           end do
           do iterm =1,nterm
             if(aterm(iterm)(1:3).eq.'@_2') then
               rterm(iterm)=abs(stable_g(io)%stddev)
               aterm(iterm)='~!~'
             end if
           end do
           call EVALUATE(ierr,MAXTERM,NTERM,NOPER,NFUNCT,ATERM,BTERM,   &
           OPERAT,FUNCT,IORDER,DVAL,rterm)
           if(dval.lt.weightmin)dval=weightmin
           if(dval.gt.weightmax)dval=weightmax
           write(iunit,1900) trim(aname),stable_g(io)%stddev,dval,trim(stable_g(im)%name)
         end if

       end do

! -- Next the V_TABLE observations are handled.

2200   continue
       if(iovtable.eq.0) go to 2300
       do i=1,iovtable
         iout=iout+1
         im=iOutVtable_g(i)
         do j=1,iovtable
           if(im.eq.modvtable(j)) go to 2220
         end do
         write(amessage,1110) 'v',trim(vtable_g(im)%name),'V'
         go to 9800
2220     io=obsvtable(j)
         nsterm=vtable_g(io)%nterm
         aname=vtable_g(im)%name
         call make_basename(ierr,iout,nsterm,aname,basename)
         if(ierr.ne.0) go to 9800
         atemp=basename(iout)
         weightmin=max(vtweightmin(j),0.0)
         weightmax=min(vtweightmax(j),1.0e36)
         eqntext=vtequation(j)
         call prepare_eqn(ierr,nterm,vtequation(j),0)
         if(ierr.ne.0) then
           ieqnerr=1
           go to 9800
         end if
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
           call num2char(j,anum)
           aname=trim(atemp)//trim(anum)
           do iterm =1,nterm
             if(aterm(iterm)(1:3).eq.'@_2') then
               rterm(iterm)=abs(vtable_g(io)%vol(j))
               aterm(iterm)='~!~'
             end if
           end do
           call EVALUATE(ierr,MAXTERM,NTERM,NOPER,NFUNCT,ATERM,BTERM,   &
           OPERAT,FUNCT,IORDER,DVAL,rterm)
           if(dval.lt.weightmin)dval=weightmin
           if(dval.gt.weightmax)dval=weightmax
           write(iunit,1900) trim(aname),vtable_g(io)%vol(j),dval,trim(vtable_g(im)%name)
         end do
       end do

! -- Next the E_TABLE observations are handled.

2300   continue
       if(iodtable.eq.0) go to 7300
       do i=1,iodtable
         iout=iout+1
         im=iOutDtable_g(i)
         do j=1,iodtable
           if(im.eq.moddtable(j)) go to 2320
         end do
         write(amessage,1110) 'e',trim(vtable_g(im)%name),'E'
         go to 9800
2320     io=obsdtable(j)
         totim=dtable_g(io)%total_time
         nsterm=dtable_g(io)%nterm
         aname=dtable_g(im)%name
         call make_basename(ierr,iout,nsterm,aname,basename)
         if(ierr.ne.0) go to 9800
         atemp=basename(iout)
         weightmin=max(dtweightmin(j),0.0)        !chek
         weightmax=min(dtweightmax(j),1.0e36)
         eqntext=dtequation(j)
         call prepare_eqn(ierr,nterm,dtequation(j),0)
         if(ierr.ne.0) then
           ieqnerr=1
           go to 9800
         end if
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
           call num2char(j,anum)
           aname=trim(atemp)//trim(anum)
           do iterm =1,nterm
             if(aterm(iterm)(1:3).eq.'@_2') then
               rterm(iterm)=abs(dtable_g(io)%time(j)/totim)
               aterm(iterm)='~!~'
             end if
           end do
           call EVALUATE(ierr,MAXTERM,NTERM,NOPER,NFUNCT,ATERM,BTERM,   &
           OPERAT,FUNCT,IORDER,DVAL,rterm)
           if(dval.lt.weightmin)dval=weightmin
           if(dval.gt.weightmax)dval=weightmax
           write(iunit,1900) trim(aname),dtable_g(io)%time(j)/totim,dval,trim(dtable_g(im)%name)
         end do
       end do


! -- Next the G_TABLE observations are handled.

7300   continue
       if(iogtable.eq.0) go to 2400
       do i=1,iogtable
         iout=iout+1
         im=iOutGtable_g(i)
         do j=1,iogtable
           if(im .eq. modgtable(j)) go to 7320
         end do
         write(amessage,1110) 'g',trim(gtable_g(im)%name),'G'
         go to 9800
7320     io=obsgtable(j)
         nsterm=ubound(gtable_g(io)%sDescription, 1)
         aname=gtable_g(im)%name
         call make_basename(ierr,iout,nsterm,aname,basename)
         if(ierr.ne.0) go to 9800
         atemp=basename(iout)
         weightmin=max(gtweightmin(j),0.0)        !chek
         weightmax=min(gtweightmax(j),1.0e36)
         eqntext=gtequation(j)
         call prepare_eqn(ierr,nterm,gtequation(j),0)
         if(ierr.ne.0) then
           ieqnerr=1
           go to 9800
         end if
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
           call num2char(j,anum)
           aname=trim(atemp)//trim(anum)
           do iterm =1,nterm
             if(aterm(iterm)(1:3).eq.'@_2') then
               rterm(iterm)=abs( gtable_g(io)%rValue(j) )
               aterm(iterm)='~!~'
             end if
           end do
           call EVALUATE(ierr,MAXTERM,NTERM,NOPER,NFUNCT,ATERM,BTERM,   &
           OPERAT,FUNCT,IORDER,DVAL,rterm)
           if(dval.lt.weightmin) dval=weightmin
           if(dval.gt.weightmax) dval=weightmax

           write(iunit,1900) trim(aname),gtable_g(io)%rValue(j),dval,trim(gtable_g(im)%name)
         end do
       end do


! -- The "* model command line" section of the PEST control file is written.

2400   continue

       write(iunit,2410)
2410   format('* model command line')
       if(modcomline.eq.' ')modcomline='model'
!       call addquote(modcomline,bstring)
!       write(iunit,2420) trim(bstring)
       write(iunit,2420) trim(modcomline)
2420   format(a)

! -- The "* model input/output" section of the PEST control file is written.

       write(iunit,2430)
2430   format('* model input/output')
       do i=1,numtempfile
         if(modfile(i).eq.' ')then
           call num2char(i,anum)
           modfile(i)='model'//trim(anum)//'.in'
         end if
         call addquote(tempfile(i),bstring)
         call addquote(modfile(i),cstring)
         write(iunit,2440) trim(bstring),trim(cstring)
2440     format(a,3x,a)
       end do
       call addquote(instructfile,bstring)
       call addquote(sListOutputFile_g,cstring)
       write(iunit,2440) trim(bstring),trim(cstring)
       close(unit=iunit)


       write(*,2460) trim(sString_g)
       write(LU_REC,2460) trim(sString_g)
2460   format(t5,'- file ',a,' written ok.')

! -- If a MICA control file was requested, it is now written.

       if(micactlfile.ne.' ')then
         call addquote(micactlfile,sString_g)
         write(*,2470) trim(sString_g)
         write(LU_REC,2470) trim(sString_g)
2470     format(t5,'Writing MICA control file ',a,' ....')
         inquire(file=micactlfile,exist=lexist)
         if(lexist)then
2471       write(6,*)
           write(*,815,advance='no') trim(sString_g)
           read(5,'(a)') aa
           call casetrans(aa,'lo')
           if((aa.ne.'y').and.(aa.ne.'n')) go to 2471
           if(aa.eq.'n') go to 2495
         end if
         itempunit=nextunit()
         open(unit=itempunit,file=micactlfile,status='old',iostat=ierr)
         if(ierr.eq.0)then
           close(unit=itempunit,status='delete')
         end if
         itempunit=nextunit()
         open(unit=itempunit,file='t###.###')
         call addquote(pestctlfile,cstring)
         write(itempunit,'(a)') trim(cstring)
         write(itempunit,'(a)') '1'
         write(itempunit,'(a)') trim(sString_g)
         close(unit=itempunit)
         call execute_command_line (trim(pest2micacom)//' < t###.### > nul')
         inquire(file=micactlfile,exist=lexist)
         if(.not.lexist)then
           write(amessage,2480)
2480       format('could not write MICA control file - check PEST2MICA command.')
           go to 9800
         else
           write(*,2460) trim(sString_g)
           write(LU_REC,2460) trim(sString_g)
         end if
       end if
2495   continue

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
9200   write(amessage,9210)
9210   format('cannot allocate sufficient memory to continue execution.')
       go to 9800
9300   call num2char(ILine_g,aline)
       call addquote(sInfile_g,sString_g)
       write(amessage,9310) trim(aoption),trim(aline),trim(sString_g),trim(correct_keyword)
9310   format(a,' keyword at line ',a,' of TSPROC input file ',a,' should immediately ', &
       'follow ',a,' keyword.')
       go to 9800
9350   write(amessage,9360) trim(sString_g)
9360   format('cannot rewind file ',a)
       go to 9800
9400   call num2char(jline,aline)
       write(amessage,9410) trim(aline),trim(sString_g)
9410   format('cannot read line ',a,' of file ',a)
       go to 9800
9450   call num2char(jline,aline)
       write(amessage,9460) trim(aline),trim(sString_g)
9460   format('insufficient entries on line ',a,' of file ',a)
       go to 9800
9500   write(amessage,9510) trim(sString_g)
9510   format('cannot close file ',a)
       go to 9800
9600   write(amessage,9610) trim(aoname),trim(amname),trim(avariable)
9610   format('OBSERVATION_S_TABLE "',a,'"  has been matched to ', &
       'MODEL_S_TABLE "',a,'". However the ',a,' has been computed ', &
       'for one and not for the other.')
       go to 9800


9800   call write_message(leadspace='yes',error='yes')
       call write_message(iunit=LU_REC,leadspace='yes')
       if(ieqnerr.ne.0)then
         write(amessage,9810)
9810     format(' Offending equation follows:-')
         call write_message()
         call write_message(iunit=LU_REC)
         do i=1,len_trim(eqntext)
           if(eqntext(i:i).eq.char(196)) eqntext(i:i)='/'
         end do
         write(*,9820) trim(eqntext)
         write(LU_REC,9820) trim(eqntext)
9820     format(' "',a,'"')
       end if
       ifail=1
       if(iunit.ne.0)close(unit=iunit,iostat=ierr)

9900   deallocate(f_pargpnme,f_inctyp,f_derinc,f_derinclb,f_forcen,f_derincmul, &
                  f_dermthd,stat=ierr)
       deallocate(f_parnme,f_partrans,f_parchglim,f_parval1,f_parlbnd,f_parubnd, &
                  f_pargp,f_scale,f_offset,stat=ierr)
       deallocate(partrans,parchglim,parval1,parlbnd,parubnd,pargp,scale,offset, stat=ierr)
       deallocate(pargpnme,inctyp,derinc,derinclb,forcen,derincmul,dermthd,stat=ierr)


       return

end subroutine pest_files

subroutine write_list_output(ifail)

! -- Subroutine Write_List_Output writes TSPROC entities to an ASCII output file.

       integer, intent(out) :: ifail

       integer icontext,ierr,i,dd,mm,yy,ss,hhh,mmm,sss,nn,iterm,j, &
       nnn,dds1,mms1,yys1,dds2,mms2,yys2,hhs1,nns1,sss1,ixcon, &
       hhs2,nns2,sss2,jstable,jvtable,jdtable,iseries,idtable,istable,ivtable, &
       ictable,jctable,igtable,jgtable
       real totim
       character(3)aaa
       character (len=iTSNAMELENGTH) :: aname,sformat,atemp
       character(15)aline
       character(25)aoption
       character(25)acontext(MAXCONTEXT)
       character (len=10) :: sDateStr

       ifail=0
       CurrentBlock_g='LIST_OUTPUT'

       write(*,10)
       write(LU_REC,10)
10     format(/,' Processing LIST_OUTPUT block....')

       ixcon=0
       icontext=0
       iOutseries_g=0          !iOutseries_g is an array
       iseries=0
       istable=0
       ictable=0
       ivtable=0
       idtable=0
       igtable = 0
       sOutfile_g=' '
       sformat=' '

! -- Options for the LIST_OUTPUT block are first read.

       do
         ILine_g=ILine_g+1
         read(LU_TSPROC_CONTROL,'(a)',err=9000,end=9100) cline
         if( len_trim(cline) == 0 ) cycle
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
           call get_file_name(ierr,sOutfile_g)
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

         else if(aoption.eq.'SERIES_NAME')then
           iseries=iseries+1
           if(iseries.gt.MAXSERIES)then
             call num2char(MAXSERIES,aline)
             write(amessage,100) trim(aline)
100          format('a maximum of ',a,' series can be cited in a LIST_OUTPUT block.')
             go to 9800
           end if
           call get_series_name(ierr,iOutseries_g(iseries),'SERIES_NAME')
           if(ierr.ne.0) go to 9800

         else if(aoption.eq.'S_TABLE_NAME')then
           istable=istable+1
           if(istable.gt.MAXSTABLE)then
             call num2char(MAXSTABLE,aline)
             write(amessage,102) trim(aline)
102          format('a maximum of ',a,' s_tables can be cited in a LIST_OUTPUT block.')
             go to 9800
           end if
           call get_table_name(ierr,iOutStable_g(istable),1)
           if(ierr.ne.0) go to 9800

         else if(aoption.eq.'C_TABLE_NAME')then
           ictable=ictable+1
           if(ictable.gt.MAXCTABLE)then
             call num2char(MAXCTABLE,aline)
             write(amessage,109) trim(aline)
109          format('a maximum of ',a,' c_tables can be cited in a LIST_OUTPUT block.')
             go to 9800
           end if
           call get_table_name(ierr,iOutCtable_g(ictable),4)
           if(ierr.ne.0) go to 9800

         else if(aoption.eq.'V_TABLE_NAME')then
           ivtable=ivtable+1
           if(ivtable.gt.MAXVTABLE)then
             call num2char(MAXVTABLE,aline)
             write(amessage,103) trim(aline)
103          format('a maximum of ',a,' v_tables can be cited in a LIST_OUTPUT block.')
             go to 9800
           end if
           call get_table_name(ierr,iOutVtable_g(ivtable),2)
           if(ierr.ne.0) go to 9800

         else if(aoption.eq.'E_TABLE_NAME')then
           idtable=idtable+1
           if(idtable.gt.MAXDTABLE)then
             call num2char(MAXDTABLE,aline)
             write(amessage,104) trim(aline)
104          format('a maximum of ',a,' E_TABLES can be cited in a LIST_OUTPUT block.')
             go to 9800
           end if
           call get_table_name(ierr,iOutDtable_g(idtable),3)
           if(ierr.ne.0) go to 9800

         else if(aoption.eq.'G_TABLE_NAME')then
           igtable=igtable+1
           if(igtable > MAXGTABLE)then
             call num2char(MAXGTABLE,aline)
             write(amessage,204) trim(aline)
204          format('a maximum of ',a,' G_TABLES can be cited in a LIST_OUTPUT block.')
             go to 9800
           end if
           call get_table_name(ierr,iOutGtable_g(igtable),iG_TABLE)
           if(ierr.ne.0) go to 9800

         else if(aoption.eq.'END')then
           go to 200

         else if(aoption.eq.'SERIES_FORMAT')then
           call getfile(ierr,cline,sformat,left_word(2),right_word(2))
           if(ierr.ne.0)then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,152) trim(aline),trim(sString_g)
152          format('cannot read SERIES_FORMAT from line ',a,' of file ',a)
             go to 9800
           end if
           call casetrans(sformat,'lo')
           if((sformat.ne.'short').and.(sformat.ne.'long') &
             .and. ( sformat /= 'ssf' ))then
             call num2char(ILine_g,aline)
             call addquote(sInfile_g,sString_g)
             write(amessage,155) trim(aline),trim(sString_g)
155          format('SERIES_FORMAT must be "long", "short", or "ssf" at line ',a, &
             ' of TSPROC input file ',a)
             go to 9800
           end if
           write(*,157) trim(sformat)
           write(LU_REC,157) trim(sformat)
157        format(t5,'SERIES_FORMAT ',a)

         else
           call num2char(ILine_g,aline)
           call addquote(sInfile_g,sString_g)
           write(amessage,180) trim(aoption),trim(aline),trim(sString_g)
180        format('unexpected keyword - "',a,'" in LIST_OUTPUT block at line ',a, &
           ' of file ',a)
           go to 9800
         end if

       end do

! -- The block has been read; now it is checked for correctness.

200    continue
       if((iseries.eq.0).and.(istable.eq.0).and.(ivtable.eq.0).and.   &
          (idtable.eq.0).and.(ictable.eq.0) .and. (igtable == 0) )then
         write(amessage,210)
210      format('no series or tables have been named for output in LIST_OUTPUT block.')
         go to 9800
       end if
       if((iseries.ne.0).and.(sformat.eq.' '))then
         write(amessage,215)
215      format('if a time series is specified for output then the SERIES_FORMAT ', &
         'specifier must also be set in a LIST_OUTPUT block.')
         go to 9800
       end if
       if ( sOutfile_g .eq. ' ' ) then
         write(amessage,230)
230      format('no FILE name provided in LIST_OUTPUT block.')
         go to 9800
       end if
       if(icontext.eq.0)then
         write(amessage,220)
220      format('no Context keyword(s) provided in LIST_OUTPUT block.')
         go to 9800
       end if

! -- All is well with the LIST_OUTPUT block so the output file is written.

       sSeriesFormat_g=sformat
       sListOutputFile_g=sOutfile_g
       call addquote(sOutfile_g,sString_g)
       write(*,260) trim(sString_g)
       write(LU_REC,260) trim(sString_g)
260    format(t5,'Writing output file ',a,'....')
       LU_OUT=nextunit()
       open(unit=LU_OUT,file=sOutfile_g,iostat=ierr)
       if(ierr.ne.0)then
         write(amessage,270) trim(sString_g)
270      format('cannot open file ',a,' for output.')
         go to 9800
       end if

! -- All of the requested time series are first written.

       iMseries_g=iseries
       iMdtable_g=idtable
       iMvtable_g=ivtable
       iMstable_g=istable
       iMctable_g=ictable
       iMgtable_g = igtable

       if(iseries.eq.0) go to 500
       do i=1,iseries
         j=iOutseries_g(i)
         aname=series_g(j)%name
         if( trim(sformat) /= 'ssf' ) then               ! suggested JFW changes to enable SSF output
           write(LU_OUT,271) trim(series_g(j)%name)
271        format(/,' TIME_SERIES "',a,'" ---->')
         endif
         if(series_g(j)%type.eq.'ts')then
           do iterm=1,series_g(j)%nterm
             if(sformat.eq.'long' .or. sformat == 'ssf' )then
               nn=series_g(j)%days(iterm)
               ss=series_g(j)%secs(iterm)
!               call newdate(nn,1,1,1970,dd,mm,yy)
               call gregorian_date(iJD=nn, &
                               iMonth=mm, &
                               iDay=dd, &
                               iYear=yy)
               hhh=ss/3600
               mmm=(ss-hhh*3600)/60
               sss=ss-hhh*3600-mmm*60

               if(datespec == 1) then
                 write(sDateStr, fmt="(i2.2,'/',i2.2,'/',i4.4)") dd, mm, yy
               else
                 write(sDateStr, fmt="(i2.2,'/',i2.2,'/',i4.4)") mm, dd, yy
               endif

               if (series_g(j)%lIsSinglePrecision) then
                 write(LU_OUT,fmt="(1x,a,t20,a10,3x,i2.2,':',i2.2,':',   &
                    & i2.2,3x,g16.9)")  &
                    trim(aname),sDateStr,hhh,mmm, sss,series_g(j)%val(iterm)
               else
                 write(LU_OUT,fmt="(1x,a,t20,a10,3x,i2.2,':',i2.2,':',   &
                    & i2.2,3x,g18.13)")  &
                    trim(aname),sDateStr,hhh,mmm, sss,series_g(j)%dpval(iterm)
               endif

             else

               if (series_g(j)%lIsSinglePrecision) then
                 write(LU_OUT,fmt="(4x,g16.9)") series_g(j)%val(iterm)
               else
                 write(LU_OUT,fmt="(4x,g18.13)") series_g(j)%dpval(iterm)
               endif

             end if

           end do
         end if
       end do

! -- If any S_TABLEs were requested, they are now written.

500    if(istable.eq.0) go to 1200
       do i=1,istable
          jstable=iOutStable_g(i)
          write(LU_OUT,510) trim(stable_g(jstable)%name)
510       format(/,' S_TABLE "',a,'" ---->')
          write(LU_OUT,515) trim(stable_g(jstable)%series_name)
515       format(t5,'Series for which data calculated:',t55,'"',a,'"')
          nnn=stable_g(jstable)%rec_begdays
          sss=stable_g(jstable)%rec_begsecs
!          call newdate(nnn,1,1,1970,dds1,mms1,yys1)
          call gregorian_date(iJD=nnn, &
                          iMonth=mms1, &
                          iDay=dds1, &
                          iYear=yys1)

          hhs1=sss/3600
          nns1=(sss-hhs1*3600)/60
          sss1=sss-hhs1*3600-nns1*60
          nnn=stable_g(jstable)%rec_enddays
          sss=stable_g(jstable)%rec_endsecs
!          call newdate(nnn,1,1,1970,dds2,mms2,yys2)
          call gregorian_date(iJD=nnn, &
                          iMonth=mms2, &
                          iDay=dds2, &
                          iYear=yys2)

          hhs2=sss/3600
          nns2=(sss-hhs2*3600)/60
          sss2=sss-hhs2*3600-nns2*60
          if(datespec.eq.1)then
            write(LU_OUT,520) dds1,mms1,yys1
          else
            write(LU_OUT,520) mms1,dds1,yys1
          end if
520       format(t5,'Starting date for data accumulation:',t55,i2.2,'/',i2.2,'/',i4)
          write(LU_OUT,530) hhs1,nns1,sss1
530       format(t5,'Starting time for data accumulation:',t55,i2.2,':',i2.2,':',i2.2)
          if(datespec.eq.1)then
            write(LU_OUT,540) dds2,mms2,yys2
          else
            write(LU_OUT,540) mms2,dds2,yys2
          end if
540       format(t5,'Ending date for data accumulation:',t55,i2.2,'/',i2.2,'/',i4)
          write(LU_OUT,550) hhs2,nns2,sss2
550       format(t5,'Ending time for data accumulation:',t55,i2.2,':',i2.2,':',i2.2)
          call num2char(stable_g(jstable)%rec_icount,aline)
          write(LU_OUT,555) trim(aline)
555       format(t5,'Number of series terms in this interval:',t55,a)
          if(stable_g(jstable)%rec_itrans.eq.0)then
            aaa='no'
          else
            aaa='yes'
          end if
          write(LU_OUT,560) trim(aaa)
560       format(t5,'Logarithmic transformation of series?',t55,a)
!          if(stable_g(jstable)%rec_power.eq.0.0)then
!            aaa='no'
!          else
!            aaa='yes'
!          end if
!          write(LU_OUT,570) trim(aaa)
!570       format(t5,'Power transformation of series?',t55,a)
          if(stable_g(jstable)%rec_power.eq.0.0)then
            aline='na'
          else
            call num2char(stable_g(jstable)%rec_power,aline)
          end if
          write(LU_OUT,580) trim(aline)
580       format(t5,'Exponent in power transformation:',t55,a)
          if(stable_g(jstable)%maximum.gt.-1.0e35)then
            write(LU_OUT,590) stable_g(jstable)%maximum
590         format(t5,'Maximum value:',t55,1pg14.7)
          end if
          if(stable_g(jstable)%minimum.gt.-1.0e35)then
            write(LU_OUT,600) stable_g(jstable)%minimum
600         format(t5,'Minimum value:',t55,1pg14.7)
          end if
          if(stable_g(jstable)%range.gt.-1.0e35)then
            write(LU_OUT,601) stable_g(jstable)%range
601         format(t5,'Range:',t55,1pg14.7)
          end if
          if(stable_g(jstable)%total.gt.-1.0e35)then
            write(LU_OUT,605) stable_g(jstable)%total
605         format(t5,'Sum of values:',t55,1pg14.7)
          end if
          if(stable_g(jstable)%mean.gt.-1.0e35)then
            write(LU_OUT,610) stable_g(jstable)%mean
610         format(t5,'Mean value:',t55,1pg14.7)
          end if
          if(stable_g(jstable)%stddev.gt.-1.0e35)then
            write(LU_OUT,620) stable_g(jstable)%stddev
620         format(t5,'Standard deviation:',t55,1pg14.7)
          end if
          if(stable_g(jstable)%minmean.gt.-1.0e35)then
            call num2char(stable_g(jstable)%avetime,atemp)
            write(LU_OUT,630) trim(atemp),stable_g(jstable)%minmean
630         format(t5,'Minimum ',a,'-sample mean',t55,1pg14.7)
          end if
          if(stable_g(jstable)%maxmean.gt.-1.0e35)then
            call num2char(stable_g(jstable)%avetime,atemp)
            write(LU_OUT,640) trim(atemp),stable_g(jstable)%maxmean
640         format(t5,'Maximum ',a,'-sample mean',t55,1pg14.7)
          end if
          if(stable_g(jstable)%median.gt.-1.0e35)then
            write(LU_OUT,650) stable_g(jstable)%median
650         format(t5,'Median value:',t55,1pg14.7)
          end if
       end do

! -- If any C_TABLEs were requested, they are now written.

1200   if(ictable.eq.0) go to 700
       do i=1,ictable
          jctable=iOutCtable_g(i)
          write(LU_OUT,1210) trim(ctable_g(jctable)%name)
1210      format(/,' C_TABLE "',a,'" ---->')
          write(LU_OUT,1215) trim(ctable_g(jctable)%series_name_obs)
1215      format(t5,'Observation time series name:',t55,'"',a,'"')
          write(LU_OUT,1216) trim(ctable_g(jctable)%series_name_sim)
1216      format(t5,'Simulation time series name:',t55,'"',a,'"')
          nnn=ctable_g(jctable)%rec_begdays
          sss=ctable_g(jctable)%rec_begsecs
!          call newdate(nnn,1,1,1970,dds1,mms1,yys1)

          call gregorian_date(iJD=nnn, &
                          iMonth=mms1, &
                          iDay=dds1, &
                          iYear=yys1)

          hhs1=sss/3600
          nns1=(sss-hhs1*3600)/60
          sss1=sss-hhs1*3600-nns1*60
          nnn=ctable_g(jctable)%rec_enddays
          sss=ctable_g(jctable)%rec_endsecs
!          call newdate(nnn,1,1,1970,dds2,mms2,yys2)
          call gregorian_date(iJD=nnn, &
                          iMonth=mms2, &
                          iDay=dds2, &
                          iYear=yys2)

          hhs2=sss/3600
          nns2=(sss-hhs2*3600)/60
          sss2=sss-hhs2*3600-nns2*60
          if(datespec.eq.1)then
            write(LU_OUT,521) dds1,mms1,yys1
521         format(t5,'Beginning date of series comparison:',   &
            t55,i2.2,'/',i2.2,'/',i4)
          else
            write(LU_OUT,521) mms1,dds1,yys1
          end if
          write(LU_OUT,531) hhs1,nns1,sss1
531       format(t5,'Beginning time of series comparison:',   &
          t55,i2.2,':',i2.2,':',i2.2)
          if(datespec.eq.1)then
            write(LU_OUT,541) dds2,mms2,yys2
541         format(t5,'Finishing date of series comparison:',  &
            t55,i2.2,'/',i2.2,'/',i4)
          else
            write(LU_OUT,541) mms2,dds2,yys2
          end if
          write(LU_OUT,551) hhs2,nns2,sss2
551       format(t5,'Finishing time of series comparison:',  &
          t55,i2.2,':',i2.2,':',i2.2)
          call num2char(ctable_g(jctable)%rec_icount,aline)
          write(LU_OUT,555) trim(aline)
          if(ctable_g(jctable)%bias.gt.-1.0e35)then
            write(LU_OUT,1290) ctable_g(jctable)%bias
1290        format(t5,'Bias:',t55,1pg14.7)
          end if
          if(ctable_g(jctable)%se.gt.-1.0e35)then
            write(LU_OUT,1300) ctable_g(jctable)%se
1300        format(t5,'Standard error:',t55,1pg14.7)
          end if
          if(ctable_g(jctable)%rbias.gt.-1.0e35)then
            write(LU_OUT,1305) ctable_g(jctable)%rbias
1305        format(t5,'Relative bias:',t55,1pg14.7)
          end if
          if(ctable_g(jctable)%rse.gt.-1.0e35)then
            write(LU_OUT,1310) ctable_g(jctable)%rse
1310         format(t5,'Relative standard error:',t55,1pg14.7)
          end if
          if(ctable_g(jctable)%ns.gt.-1.0e35)then
            write(LU_OUT,1320) ctable_g(jctable)%ns
1320         format(t5,'Nash-Sutcliffe coefficient:',t55,1pg14.7)
          end if
          if(ctable_g(jctable)%ce.gt.-1.0e35)then
            write(LU_OUT,1330) ctable_g(jctable)%ce
1330        format(t5,'Coefficient of efficiency:',t55,1pg14.7)
          end if
          if(ctable_g(jctable)%ia.gt.-1.0e35)then
            write(LU_OUT,1340) ctable_g(jctable)%ia
1340        format(t5,'Index of agreement:',t55,1pg14.7)
          endif

          if (ctable_g(jctable)%ve .gt. -1.0e35) then
            write(LU_OUT,1350) ctable_g(jctable)%ve
1350        format(t5,'Volumetric efficiency:',t55,1pg14.7)
          end if
       end do

! -- If any V_TABLES were requested, they are now written.

700    if(ivtable.eq.0) go to 900
       do i=1,ivtable
          jvtable=iOutVtable_g(i)
          write(LU_OUT,710) trim(vtable_g(jvtable)%name)
710       format(/,' V_TABLE "',a,'" ---->')
          write(LU_OUT,715) trim(vtable_g(jvtable)%series_name)
715       format(t5,'Volumes calculated from series "',a,'" are as follows:-')
          do j=1,vtable_g(jvtable)%nterm
!            call newdate(vtable_g(jvtable)%days1(j),1,1,1970,dds1,mms1,yys1)

            call gregorian_date(iJD=vtable_g(jvtable)%days1(j), &
                            iMonth=mms1, &
                            iDay=dds1, &
                            iYear=yys1)

            sss=vtable_g(jvtable)%secs1(j)
            hhs1=sss/3600
            nns1=(sss-hhs1*3600)/60
            sss1=sss-hhs1*3600-nns1*60
!            call newdate(vtable_g(jvtable)%days2(j),1,1,1970,dds2,mms2,yys2)

            call gregorian_date(iJD=vtable_g(jvtable)%days2(j), &
                            iMonth=mms2, &
                            iDay=dds2, &
                            iYear=yys2)

            sss=vtable_g(jvtable)%secs2(j)
            hhs2=sss/3600
            nns2=(sss-hhs2*3600)/60
            sss2=sss-hhs2*3600-nns2*60
            if(datespec.eq.1)then
              write(LU_OUT,720) dds1,mms1,yys1,hhs1,nns1,sss1,  &
                                 dds2,mms2,yys2,hhs2,nns2,sss2,vtable_g(jvtable)%vol(j)
            else
              write(LU_OUT,720) mms1,dds1,yys1,hhs1,nns1,sss1,  &
                                 mms2,dds2,yys2,hhs2,nns2,sss2,vtable_g(jvtable)%vol(j)
            end if
720         format(t5,'From ',i2.2,'/',i2.2,'/',i4,' ',i2.2,':',i2.2,':',i2.2,  &
                      ' to ',i2.2,'/',i2.2,'/',i4,' ',i2.2,':',i2.2,':',i2.2,  &
                      '  volume = ',g18.12)
          end do
       end do

! -- If any E_TABLES were requested, they are now written.

900    continue
       if(idtable.eq.0) go to 1100
       do i=1,idtable
          jdtable=iOutDtable_g(i)
          totim=dtable_g(jdtable)%total_time
          write(LU_OUT,910) trim(dtable_g(jdtable)%name)
910       format(/,' E_TABLE "',a,'" ---->')
          if(dtable_g(jdtable)%under_over.eq.1)then
            write(LU_OUT,915) trim(dtable_g(jdtable)%time_units), &
            trim(dtable_g(jdtable)%time_units)
915         format(t4,'Flow',t19,'Time delay (',a,')',t40,'Time above (',a,')',  &
            t60,'Fraction of time above threshold')
          else
            write(LU_OUT,916) trim(dtable_g(jdtable)%time_units), &
            trim(dtable_g(jdtable)%time_units)
916         format(t4,'Flow',t19,'Time delay (',a,')',t40,'Time under (',a,')',  &
            t60,'Fraction of time below threshold')
          end if
          do j=1,dtable_g(jdtable)%nterm
            write(LU_OUT,920) dtable_g(jdtable)%flow(j),  &
            dtable_g(jdtable)%tdelay(j), dtable_g(jdtable)%time(j), &
            dtable_g(jdtable)%time(j)/totim
920         format(t2,g14.7,t20,g14.7,t40,g14.7,t63,g14.7)
          end do
       end do
1100   continue


! -- If any G_TABLES were requested, they are now written.

       if(igtable.eq.0) go to 3100
       do i=1,igtable
         jgtable=iOutGtable_g(i)
         write(LU_OUT,2910) trim(gtable_g(jgtable)%name)
2910     format(/,' G_TABLE "',a,'" ---->')
         write(LU_OUT,2915) gtable_g(jgtable)%g_table_header
2915     format(t4,a75,t90,'Value')
         do j=1,ubound(gtable_g(jgtable)%sDescription, 1 )
           write(LU_OUT,fmt="(t4,a,t82,g14.7)") gtable_g(jgtable)%sDescription(j), &
              gtable_g(jgtable)%rValue(j)
         end do
       end do
3100   continue

       write(*,320) trim(sString_g)
       write(LU_REC,320) trim(sString_g)
320    format(t5,'File ',a,' written ok.')
       flush(unit=LU_OUT)
       close(unit=LU_OUT)

       return

9000   call num2char(ILine_g,aline)
       call addquote(sInfile_g,sString_g)
       write(amessage,9010) trim(aline),trim(sString_g)
9010   format('cannot read line ',a,' of TSPROC input file ',a)
       go to 9800
9100   continue
       call addquote(sInfile_g,sString_g)
       write(amessage,9110) trim(sString_g)
9110   format('unexpected end encountered to TSPROC input file ',a,  &
       ' while reading LIST_OUTPUT block.')
       go to 9800
9800   call write_message(leadspace='yes',error='yes')
       call write_message(iunit=LU_REC,leadspace='yes')
       ifail=1

       close(unit=LU_REC,iostat=ierr)
       return


end subroutine write_list_output



end module tsp_output
