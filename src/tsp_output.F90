module TSP_OUTPUT

    use TSP_DATA_STRUCTURES
    use TSP_COMMAND_PROCESSORS
    use TSP_UTILITIES
    use TSP_EQUATION_PARSER

    use m_vstringlist, only: vstrlist_new, vstrlist_append, vstrlist_sort,   &
                             vstrlist_index, vstrlist_free, t_vstringlist,   &
                             vstrlist_length, vstrlist_remove,               &
                             vstrlist_search, vstrlist_concat
    use m_vstring, only: vstring_equals, vstring_cast
    use tokenlists

    implicit none

contains

    subroutine write_vstrlist(eq_sorted, wordcnt, lu_rec)
        type(t_vstringlist), intent(in) :: eq_sorted
        integer, intent(in) :: wordcnt, lu_rec

        integer :: ipar
        character(12) :: aapar

        do ipar = 1, vstrlist_length(eq_sorted)
            call vstring_cast(vstrlist_index(eq_sorted, ipar), aapar)
            write (*, "(A13)", advance="no") aapar
            write (lu_rec, "(A13)", advance="no") aapar
            if (MOD(ipar, wordcnt) == 0) then
                write (*, *)
                write (lu_rec, *)
            end if
        end do
    end subroutine write_vstrlist

    subroutine PEST_FILES(Ifail, Lastblock)
!    -- Subroutine PEST_FILES generates a PEST input dataset.

        implicit none
!
! Dummy arguments
!
        integer :: Ifail, Lastblock
        intent(in) Lastblock
        intent(out) Ifail
!
! Local variables
!
        character(1) :: aa
        character(25), dimension(maxcontext) :: acontext
        character(15) :: aline, avariable
        real :: eigthresh, rmtemp, rotemp, rprecis, rtime, totim, weightmax,      &
                weightmin
        character(itsnamelength) :: amname, anum, aoname, atrans
        character(30) :: aname, aoption, atemp, correct_keyword, last_keyword,    &
                         otherblock
        character(3) :: auiaa
        integer :: auiyesno, dd, i, iaui, icontext, ieqnerr, ierr, ii1, il,       &
                   iterm, im, io, iodtable, iogtable, iomdtable, iomgtable,       &
                   iomseries, iomstable, iomvtable, ioseries, iostable, iout,     &
                   iovtable, isnum, isvd, itemp, itempfile, itempunit, iunit,     &
                   ixcon, j, jj, jj1, jline, k, kk, ll, mm, nmterm, nn, nnterm,   &
                   nobs, nobsgp, noterm, nsterm, nterm, numtempfile, siout, yy
        character(12), dimension(maxseries + maxvtable + maxdtable + maxgtable)   &
                     :: basename
        character(120) :: bstring, cstring, instructfile, micactlfile, modcomline,&
                          pardatfile, pest2micacom, pestctlfile
        real(8) :: delta2, dtempx, dval, obj_fun_value, sse
        character(150), dimension(maxdtable) :: dtequation
        real, dimension(maxdtable) :: dtweightmax, dtweightmin
        character(150) :: eqntext
        character(150), dimension(maxgtable) :: gtequation
        real, dimension(maxgtable) :: gtweightmax, gtweightmin
        logical :: lexist
        integer, dimension(maxdtable) :: moddtable, obsdtable
        character(120), dimension(maxtempfile) :: modfile, tempfile
        integer, dimension(maxgtable) :: modgtable, obsgtable
        integer, dimension(maxseries) :: modseries, obsseries
        integer, dimension(maxstable) :: modstable, obsstable
        integer, dimension(maxvtable) :: modvtable, obsvtable
        character(12), dimension(maxseries + maxstable + maxvtable + maxdtable +  &
                     maxgtable) :: obgnme
        character(12), dimension(maxstable) :: sbasename
        character(150), dimension(maxseries) :: sequation
        character(150), dimension(maxstable) :: stequation
        real, dimension(maxstable) :: stweightmax, stweightmin
        real, dimension(maxseries) :: sweightmax, sweightmin
        real(t_dbl), allocatable, dimension(:) :: tempobsvals, tempsimvals
        character(150), dimension(maxvtable) :: vtequation
        real, dimension(maxvtable) :: vtweightmax, vtweightmin

! -- Variables used for dealing with parameter groups.
        integer :: f_numpargp, igp, npargp
        real, allocatable, dimension(:) :: f_derinc, f_derinclb, f_derincmul
        character(12) :: apargp
        character(12), allocatable, dimension(:) :: f_dermthd, f_forcen,          &
                                                    f_inctyp, f_pargpnme
        character(120) :: pargroupfile

! -- Variables used for dealing with parameter data.
        integer :: f_numpar, ipar, npar, nnpar, tempunit
        real, allocatable, dimension(:) :: f_offset, f_parlbnd, f_parubnd,        &
                                           f_parval1, f_scale
        character(1) :: pardelim
        character(12) :: aapar
        character(12), dimension(maxpar) :: apar
        character(12), allocatable, dimension(:) :: f_parchglim, f_pargp
        character(12), allocatable, dimension(:) :: f_parnme
        character(19), allocatable, dimension(:) :: f_partrans

! -- Variables to work with secondary parameters.
        integer :: f_nequation, f_nparsec, iequation
        character(12), allocatable, dimension(:) :: f_parsecnme
        character(240), allocatable, dimension(:) :: f_equation

! -- Variables for use in calculating series stats as components of a weight
!    equation.
        integer :: lc
        real(8) :: delta, dpcount, dpmax, dpmean, dpmin, dpsum, dpvariance,       &
                        m2, tempmean

        integer :: nnnpar

        type(t_vstringlist) :: groups_dat, dat_sorted, params_par_dat,            &
                               params_eq_dat, template_params,                    &
                               tpl_sorted, eq_sorted, params_grp_dat
        type(tokenlist) :: tlist

        Ifail = 0 ! counter, disposable
        currentblock_g = 'WRITE_PEST_FILES'
        ieqnerr = 0

        write (*, 9122) TRIM(currentblock_g)
        write (lu_rec, 9122) TRIM(currentblock_g)
        if (Lastblock /= 201) then
            write (amessage, 9001)
9001        format('a WRITE_PEST_FILES block must immediately follow a LIST_OUTPUT ',     &
                   'block in a TSPROC input file.')
            goto 2400
        end if

!    -- Initialisation
        isvd = 0
        iaui = 0
        auiyesno = 0
        icontext = 0
        itempfile = 0
        tempfile = ' ' ! tempfile is an array
        modfile = ' ' ! modfile is an array

        sequation = ' ' ! sequation is an array
        stequation = ' ' ! stequation is an array
        vtequation = ' ' ! vtequation is an array
        dtequation = ' ' ! dtequation is an array
        gtequation = ' ' ! gtequation is an array

        ioseries = 0
        iostable = 0
        iovtable = 0
        iodtable = 0
        iogtable = 0
        iomseries = 0
        iomstable = 0
        iomvtable = 0
        iomdtable = 0
        iomgtable = 0

        pardatfile = ' '
        pargroupfile = ' '
        pestctlfile = ' '
        instructfile = ' '
        modcomline = ' '
        micactlfile = ' '
        pest2micacom = ' '

        sweightmin = -1.0E36 !sweightmin is an array
        sweightmax = 1.0E36 !sweightman is an array
        stweightmin = -1.0E36 !stweightmin is an array
        stweightmax = 1.0E36 !stweightmax is an array
        vtweightmin = -1.0E36 !vtweightmin is an array
        vtweightmax = 1.0E36 !vtweightmax is an array
        dtweightmin = -1.0E36 !dtweightmin is an array
        dtweightmax = 1.0E36 !dtweightmax is an array
        gtweightmin = -1.0E36 !dtweightmin is an array
        gtweightmax = 1.0E36 !dtweightmax is an array

        f_numpar = 0
        ixcon = 0
        iunit = 0

!    -- The PEST_FILES block is first parsed.
        do
            iline_g = iline_g + 1
            read (lu_tsproc_control, '(a)', err=1500, end=1600) cline
            if (LEN_TRIM(cline) == 0) cycle
            if (cline(1:1) == '#') cycle
            call LINESPLIT(ierr, 2)
            if (ierr /= 0) then
                call NUM2CHAR(iline_g, aline)
                call ADDQUOTE(sinfile_g, sstring_g)
                write (amessage, 9002) TRIM(aline), TRIM(sstring_g)
9002            format('insufficient entries on line ', a, ' of file ', a)
                goto 2400
            end if
            aoption = cline(LEFT_WORD(1):RIGHT_WORD(1))
            call CASETRANS(aoption, 'hi')
            if (aoption /= 'CONTEXT') then
                call TEST_CONTEXT(ierr, icontext, acontext)
                if (ierr == -1) then
                    call FIND_END(Ifail)
                    if (Ifail == 1) goto 2400
                    return
                elseif (ierr == 1) then
                    goto 2400
                end if
                ixcon = 1
            end if

            if (aoption == 'TEMPLATE_FILE') then
                itempfile = itempfile + 1
                if (itempfile > maxtempfile) then
                    call NUM2CHAR(maxtempfile, aline)
                    write (amessage, 9003) TRIM(aline), TRIM(currentblock_g)
9003                format('only ', a, ' template files can be cited in a ', a,   &
                           ' block.')
                    goto 2400
                end if
                call GETFILE(ierr, cline, tempfile(itempfile), LEFT_WORD(2),      &
                             RIGHT_WORD(2))
                if (ierr /= 0) then
                    call NUM2CHAR(iline_g, aline)
                    call ADDQUOTE(sinfile_g, sstring_g)
                    write (amessage, 9004) TRIM(aline), TRIM(sstring_g)
9004                format('cannot read template file name from line ', a,        &
                           ' of file ', a)
                    goto 2400
                end if
                write (*, 9123) TRIM(aoption), TRIM(tempfile(itempfile))
                write (lu_rec, 9123) TRIM(aoption), TRIM(tempfile(itempfile))

            elseif (aoption == 'MODEL_INPUT_FILE') then
                correct_keyword = 'TEMPLATE_FILE'
                if (last_keyword /= correct_keyword) goto 1800
                call GETFILE(ierr, cline, modfile(itempfile), LEFT_WORD(2),       &
                             RIGHT_WORD(2))
                if (ierr /= 0) then
                    call NUM2CHAR(iline_g, aline)
                    call ADDQUOTE(sinfile_g, sstring_g)
                    write (amessage, 9005) TRIM(aline), TRIM(sstring_g)
9005                format('cannot read model input file name from line ', a,     &
                           ' of file ', a)
                    goto 2400
                end if
                write (*, 9123) TRIM(aoption), TRIM(modfile(itempfile))
                write (lu_rec, 9123) TRIM(aoption), TRIM(modfile(itempfile))

            elseif (aoption == 'PARAMETER_DATA_FILE') then
                call GETFILE(ierr, cline, pardatfile, LEFT_WORD(2), RIGHT_WORD(2))
                if (ierr /= 0) then
                    call NUM2CHAR(iline_g, aline)
                    call ADDQUOTE(sinfile_g, sstring_g)
                    write (amessage, 9006) TRIM(aline), TRIM(sstring_g)
9006                format('cannot read parameter data file name from line ', a,  &
                           ' of file ', a)
                    goto 2400
                end if
                write (*, 9123) TRIM(aoption), TRIM(pardatfile)
                write (lu_rec, 9123) TRIM(aoption), TRIM(pardatfile)

            elseif (aoption == 'PARAMETER_GROUP_FILE') then
                call GETFILE(ierr, cline, pargroupfile, LEFT_WORD(2),             &
                             RIGHT_WORD(2))
                if (ierr /= 0) then
                    call NUM2CHAR(iline_g, aline)
                    call ADDQUOTE(sinfile_g, sstring_g)
                    write (amessage, 9007) TRIM(aline), TRIM(sstring_g)
9007                format('cannot read parameter group file name from line ', a, &
                           ' of file ', a)
                    goto 2400
                end if
                write (*, 9123) TRIM(aoption), TRIM(pargroupfile)
                write (lu_rec, 9123) TRIM(aoption), TRIM(pargroupfile)

            elseif (aoption == 'AUTOMATIC_USER_INTERVENTION') then
                call GET_YES_NO(ierr, auiyesno)
                if (ierr /= 0) goto 2400
                if (auiyesno == 1) then
                    auiaa = 'yes'
                else
                    auiaa = 'no'
                end if
                iaui = 1
                write (*, 9123) TRIM(aoption), TRIM(auiaa)
                write (lu_rec, 9123) TRIM(aoption), TRIM(auiaa)

            elseif (aoption == 'TRUNCATED_SVD') then
                call GET_KEYWORD_VALUE(ierr, 2, itemp, eigthresh, aoption)
                if (ierr /= 0) goto 2400
                if (eigthresh <= 0.0) then
                    call NUM2CHAR(iline_g, aline)
                    call ADDQUOTE(sinfile_g, sstring_g)
                    write (amessage, 9008) TRIM(aline), TRIM(sstring_g)
9008                format('SVD truncation limit must be positive at line ', a,   &
                           ' of file ', a)
                    goto 2400
                end if
                isvd = 1

            elseif (aoption == 'NEW_PEST_CONTROL_FILE') then
                call GETFILE(ierr, cline, pestctlfile, LEFT_WORD(2), RIGHT_WORD(2)&
                             )
                if (ierr /= 0) then
                    call NUM2CHAR(iline_g, aline)
                    call ADDQUOTE(sinfile_g, sstring_g)
                    write (amessage, 9009) TRIM(aline), TRIM(sstring_g)
9009                format('cannot read pest control file name from line ', a,    &
                           ' of file ', a)
                    goto 2400
                end if
                write (*, 9123) TRIM(aoption), TRIM(pestctlfile)
                write (lu_rec, 9123) TRIM(aoption), TRIM(pestctlfile)

            elseif (aoption == 'NEW_MICA_CONTROL_FILE') then
                call GETFILE(ierr, cline, micactlfile, LEFT_WORD(2), RIGHT_WORD(2)&
                             )
                if (ierr /= 0) then
                    call NUM2CHAR(iline_g, aline)
                    call ADDQUOTE(sinfile_g, sstring_g)
                    write (amessage, 9010) TRIM(aline), TRIM(sstring_g)
9010                format('cannot read mica control file name from line ', a,    &
                           ' of file ', a)
                    goto 2400
                end if
                write (*, 9123) TRIM(aoption), TRIM(micactlfile)
                write (lu_rec, 9123) TRIM(aoption), TRIM(micactlfile)

            elseif (aoption == 'PEST2MICA_COMMAND') then
                call GETFILE(ierr, cline, pest2micacom, LEFT_WORD(2),             &
                             RIGHT_WORD(2))
                if (ierr /= 0) then
                    call NUM2CHAR(iline_g, aline)
                    call ADDQUOTE(sinfile_g, sstring_g)
                    write (amessage, 9011) TRIM(aline), TRIM(sstring_g)
9011                format('cannot read PEST2MICA command from line ', a,         &
                           ' of file ', a)
                    goto 2400
                end if
                write (*, 9123) TRIM(aoption), TRIM(pest2micacom)
                write (lu_rec, 9123) TRIM(aoption), TRIM(pest2micacom)

            elseif (aoption == 'NEW_INSTRUCTION_FILE') then
                call GETFILE(ierr, cline, instructfile, LEFT_WORD(2),             &
                             RIGHT_WORD(2))
                if (ierr /= 0) then
                    call NUM2CHAR(iline_g, aline)
                    call ADDQUOTE(sinfile_g, sstring_g)
                    write (amessage, 9012) TRIM(aline), TRIM(sstring_g)
9012                format('cannot read instruction file name from line ', a,     &
                           ' of file ', a)
                    goto 2400
                end if
                write (*, 9123) TRIM(aoption), TRIM(instructfile)
                write (lu_rec, 9123) TRIM(aoption), TRIM(instructfile)

            elseif (aoption == 'MODEL_COMMAND_LINE') then
                call GETFILE(ierr, cline, modcomline, LEFT_WORD(2), RIGHT_WORD(2))
                if (ierr /= 0) then
                    call NUM2CHAR(iline_g, aline)
                    call ADDQUOTE(sinfile_g, sstring_g)
                    write (amessage, 9013) TRIM(aline), TRIM(sstring_g)
9013                format('cannot read model command line from line ', a,        &
                           ' of file ', a)
                    goto 2400
                end if
                write (*, 9123) TRIM(aoption), TRIM(modcomline)
                write (lu_rec, 9123) TRIM(aoption), TRIM(modcomline)

            elseif (aoption == 'OBSERVATION_SERIES_NAME') then
                ioseries = ioseries + 1
                if (ioseries > maxseries) then
                    call NUM2CHAR(maxseries, aline)
                    write (amessage, 9014) TRIM(aline), TRIM(currentblock_g)
9014                format('a maximum of ', a, ' series can be cited in a ', a,   &
                           ' block.')
                    goto 2400
                end if
                call GET_SERIES_NAME(ierr, obsseries(ioseries),                   &
                                     'OBSERVATION_SERIES_NAME')
                if (ierr /= 0) goto 2400

            elseif (aoption == 'OBSERVATION_S_TABLE_NAME') then
                iostable = iostable + 1
                if (iostable > maxstable) then
                    call NUM2CHAR(maxstable, aline)
                    write (amessage, 9015) TRIM(aline), TRIM(currentblock_g)
9015                format('a maximum of ', a, ' s_tables can be cited in a ', a, &
                                                                                              ' block.')
                    goto 2400
                end if
                call GET_TABLE_NAME(ierr, obsstable(iostable), 11)
                if (ierr /= 0) goto 2400

            elseif (aoption == 'OBSERVATION_C_TABLE_NAME') then
                write (amessage, 9124)
                goto 2400

            elseif (aoption == 'OBSERVATION_V_TABLE_NAME') then
                iovtable = iovtable + 1
                if (iovtable > maxvtable) then
                    call NUM2CHAR(maxvtable, aline)
                    write (amessage, 9016) TRIM(aline), TRIM(currentblock_g)
9016                format('a maximum of ', a, ' v_tables can be cited in a ', a, &
                           ' block.')
                    goto 2400
                end if
                call GET_TABLE_NAME(ierr, obsvtable(iovtable), 12)
                if (ierr /= 0) goto 2400

            elseif (aoption == 'OBSERVATION_E_TABLE_NAME') then
                iodtable = iodtable + 1
                if (iodtable > maxdtable) then
                    call NUM2CHAR(maxdtable, aline)
                    write (amessage, 9017) TRIM(aline), TRIM(currentblock_g)
9017                format('a maximum of ', a, ' e_tables can be cited in a ', a, &
                           ' block.')
                    goto 2400
                end if
                call GET_TABLE_NAME(ierr, obsdtable(iodtable), 13)
                if (ierr /= 0) goto 2400

            elseif (aoption == 'OBSERVATION_G_TABLE_NAME') then
                iogtable = iogtable + 1
                if (iogtable > maxgtable) then
                    call NUM2CHAR(maxgtable, aline)
                    write (amessage, 9018) TRIM(aline), TRIM(currentblock_g)
9018                format('a maximum of ', a, ' g_tables can be cited in a ', a, &
                           ' block.')
                    goto 2400
                end if
                ! after call below, obsgtable(iotable) should contain the
                ! index of the gtable with the user supplied name
                call GET_TABLE_NAME(ierr, obsgtable(iogtable), 15)
                if (ierr /= 0) goto 2400

            elseif (aoption == 'MODEL_SERIES_NAME') then
                correct_keyword = 'OBSERVATION_SERIES_NAME'
                if (last_keyword /= correct_keyword) goto 1800
                iomseries = iomseries + 1
                call GET_SERIES_NAME(ierr, modseries(iomseries),                  &
                                     'MODEL_SERIES_NAME')
                if (ierr /= 0) goto 2400
                if (iomseries > 1) then
                    do k = 1, iomseries - 1
                        if (modseries(k) == modseries(iomseries)) then
                            write (amessage, 9019)                             &
                                  TRIM(SERIES_G(modseries(iomseries))%NAME)
9019                        format('time series "', a,                         &
                                   '" has been provided as more than one ',    &
                                   'MODEL_SERIES_NAME.')
                            goto 2400
                        end if
                    end do
                end if

            elseif (aoption == 'MODEL_S_TABLE_NAME') then
                correct_keyword = 'OBSERVATION_S_TABLE_NAME'
                if (last_keyword /= correct_keyword) goto 1800
                iomstable = iomstable + 1
                call GET_TABLE_NAME(ierr, modstable(iomstable), 21)
                if (ierr /= 0) goto 2400
                if (iomstable > 1) then
                    do k = 1, iomstable - 1
                        if (modstable(k) == modstable(iomstable)) then
                            write (amessage, 9020)                             &
                                  TRIM(STABLE_G(modstable(iomstable))%NAME)
9020                        format('s_table "', a,                             &
                                   '" has been provided as more than one ',    &
                                   'MODEL_S_TABLE_NAME.')
                            goto 2400
                        end if
                    end do
                end if

            elseif (aoption == 'MODEL_C_TABLE_NAME') then
                write (amessage, 9124)
                goto 2400

            elseif (aoption == 'MODEL_V_TABLE_NAME') then
                correct_keyword = 'OBSERVATION_V_TABLE_NAME'
                if (last_keyword /= correct_keyword) goto 1800
                iomvtable = iomvtable + 1
                call GET_TABLE_NAME(ierr, modvtable(iomvtable), 22)
                if (ierr /= 0) goto 2400
                if (iomvtable > 1) then
                    do k = 1, iomvtable - 1
                        if (modvtable(k) == modvtable(iomvtable)) then
                            write (amessage, 9021)                             &
                                  TRIM(VTABLE_G(modvtable(iomvtable))%NAME)
9021                        format('v_table "', a,                             &
                                   '" has been provided as more than one ',    &
                                   'MODEL_V_TABLE_NAME.')
                            goto 2400
                        end if
                    end do
                end if

            elseif (aoption == 'MODEL_E_TABLE_NAME') then
                correct_keyword = 'OBSERVATION_E_TABLE_NAME'
                if (last_keyword /= correct_keyword) goto 1800
                iomdtable = iomdtable + 1
                call GET_TABLE_NAME(ierr, moddtable(iomdtable), 23)
                if (ierr /= 0) goto 2400
                if (iomdtable > 1) then
                    do k = 1, iomdtable - 1
                        if (moddtable(k) == moddtable(iomdtable)) then
                            write (amessage, 9022)                             &
                                  TRIM(DTABLE_G(moddtable(iomdtable))%NAME)
9022                        format('e_table "', a,                             &
                                   '" has been provided as more than one ',    &
                                   'MODEL_E_TABLE_NAME.')
                            goto 2400
                        end if
                    end do
                end if

            elseif (aoption == 'MODEL_G_TABLE_NAME') then
                correct_keyword = 'OBSERVATION_G_TABLE_NAME'
                if (last_keyword /= correct_keyword) goto 1800
                iomgtable = iomgtable + 1
                call GET_TABLE_NAME(ierr, modgtable(iomgtable), 25)
                if (ierr /= 0) goto 2400
                if (iomgtable > 1) then
                    do k = 1, iomgtable - 1
                        if (modgtable(k) == modgtable(iomgtable)) then
                            write (amessage, 9023)                             &
                                  TRIM(GTABLE_G(modgtable(iomgtable))%NAME)
9023                        format('g_table "', a,                             &
                                   '" has been provided as more than one ',    &
                                   'MODEL_G_TABLE_NAME.')
                            goto 2400
                        end if
                    end do
                end if

            elseif (aoption == 'SERIES_WEIGHTS_EQUATION') then
                correct_keyword = 'MODEL_SERIES_NAME'
                if (last_keyword /= correct_keyword) goto 1800
                call GET_EQUATION(ierr, sequation(ioseries), aoption)
                if (ierr /= 0) goto 2400

            elseif (aoption == 'S_TABLE_WEIGHTS_EQUATION') then
                correct_keyword = 'MODEL_S_TABLE_NAME'
                if (last_keyword /= correct_keyword) goto 1800
                call GET_EQUATION(ierr, stequation(iostable), aoption)
                if (ierr /= 0) goto 2400

            elseif (aoption == 'V_TABLE_WEIGHTS_EQUATION') then
                correct_keyword = 'MODEL_V_TABLE_NAME'
                if (last_keyword /= correct_keyword) goto 1800
                call GET_EQUATION(ierr, vtequation(iovtable), aoption)
                if (ierr /= 0) goto 2400

            elseif (aoption == 'E_TABLE_WEIGHTS_EQUATION') then
                correct_keyword = 'MODEL_E_TABLE_NAME'
                if (last_keyword /= correct_keyword) goto 1800
                call GET_EQUATION(ierr, dtequation(iodtable), aoption)
                if (ierr /= 0) goto 2400

            elseif (aoption == 'G_TABLE_WEIGHTS_EQUATION') then
                correct_keyword = 'MODEL_G_TABLE_NAME'
                if (last_keyword /= correct_keyword) goto 1800
                call GET_EQUATION(ierr, gtequation(iogtable), aoption)
                if (ierr /= 0) goto 2400

            elseif (aoption == 'SERIES_WEIGHTS_MIN_MAX') then
                correct_keyword = 'SERIES_WEIGHTS_EQUATION'
                if (last_keyword /= correct_keyword) goto 1800
                call GET_TWO_NUMBERS(ierr, sweightmin(ioseries),                  &
                                     sweightmax(ioseries), aoption)
                if (ierr /= 0) goto 2400
                call CHECK_WEIGHT_ORDER(ierr, sweightmin(ioseries),               &
                                        sweightmax(ioseries))
                if (ierr /= 0) goto 2400

            elseif (aoption == 'S_TABLE_WEIGHTS_MIN_MAX') then
                correct_keyword = 'S_TABLE_WEIGHTS_EQUATION'
                if (last_keyword /= correct_keyword) goto 1800
                call GET_TWO_NUMBERS(ierr, stweightmin(iostable),                 &
                                     stweightmax(iostable), aoption)
                if (ierr /= 0) goto 2400
                call CHECK_WEIGHT_ORDER(ierr, stweightmin(iostable),              &
                                        stweightmax(iostable))
                if (ierr /= 0) goto 2400

            elseif (aoption == 'V_TABLE_WEIGHTS_MIN_MAX') then
                correct_keyword = 'V_TABLE_WEIGHTS_EQUATION'
                if (last_keyword /= correct_keyword) goto 1800
                call GET_TWO_NUMBERS(ierr, vtweightmin(iovtable),                 &
                                     vtweightmax(iovtable), aoption)
                if (ierr /= 0) goto 2400
                call CHECK_WEIGHT_ORDER(ierr, vtweightmin(iovtable),              &
                                        vtweightmax(iovtable))
                if (ierr /= 0) goto 2400

            elseif (aoption == 'E_TABLE_WEIGHTS_MIN_MAX') then
                correct_keyword = 'E_TABLE_WEIGHTS_EQUATION'
                if (last_keyword /= correct_keyword) goto 1800
                call GET_TWO_NUMBERS(ierr, dtweightmin(iodtable),                 &
                                     dtweightmax(iodtable), aoption)
                if (ierr /= 0) goto 2400
                call CHECK_WEIGHT_ORDER(ierr, dtweightmin(iodtable),              &
                                        dtweightmax(iodtable))
                if (ierr /= 0) goto 2400

            elseif (aoption == 'G_TABLE_WEIGHTS_MIN_MAX') then
                correct_keyword = 'G_TABLE_WEIGHTS_EQUATION'
                if (last_keyword /= correct_keyword) goto 1800
                call GET_TWO_NUMBERS(ierr, gtweightmin(iogtable),                 &
                                     gtweightmax(iogtable), aoption)
                if (ierr /= 0) goto 2400
                call CHECK_WEIGHT_ORDER(ierr, gtweightmin(iogtable),              &
                                        gtweightmax(iogtable))
                if (ierr /= 0) goto 2400

            elseif (aoption == 'CONTEXT') then
                if (ixcon /= 0) then
                    call NUM2CHAR(iline_g, aline)
                    call ADDQUOTE(sinfile_g, sstring_g)
                    write (amessage, 9024) TRIM(aline), TRIM(sstring_g)
9024                format('CONTEXT keyword in incorrect location at line ', a,   &
                           ' of file ', a)
                    goto 2400
                end if
                call GET_CONTEXT(ierr, icontext, acontext)
                if (ierr /= 0) goto 2400

            elseif (aoption == 'END') then
                exit

            else
                call NUM2CHAR(iline_g, aline)
                call ADDQUOTE(sinfile_g, sstring_g)
                write (amessage, 9025) TRIM(aoption), TRIM(currentblock_g),         &
                                     TRIM(aline), TRIM(sstring_g)
9025            format('unexpected keyword - "', a, '" in ', a, ' block at line ',&
                       a, ' of file ', a)
                goto 2400
            end if

            last_keyword = aoption

        end do

!    -- Any absences in the block are now looked for.
        if ((ioseries == 0) .AND. (iostable == 0) .AND. (iovtable == 0) .AND.            &
           (iodtable == 0) .AND. (iogtable == 0)) then
            write (amessage, 9026) TRIM(currentblock_g)
9026        format('no observation series or table names have been cited in ', a, &
                                                ' block.')
            goto 2400
        end if
        if (itempfile == 0) then
            write (amessage, 9027) TRIM(currentblock_g)
9027        format('at least one TEMPLATE_FILE keyword must be provided in a ',   &
                                                a, ' block.')
            goto 2400
        end if
        if (pestctlfile == ' ') then
            write (amessage, 9028) TRIM(currentblock_g)
9028        format('a NEW_PEST_CONTROL_FILE keyword must be provided in a ', a,   &
                                                ' block.')
            goto 2400
        end if
        if (instructfile == ' ') then
            write (amessage, 9029) TRIM(currentblock_g)
9029        format('NEW_INSTRUCTION_FILE keyword is missing from the ', a,        &
                                                ' block.')
            goto 2400
        end if
        if (ioseries /= 0) then
            if (iomseries /= ioseries) then
                write (amessage, 9030) TRIM(currentblock_g)
9030            format('a MODEL_SERIES_NAME keyword has not been provided for each ', &
                       'OBSERVATION_SERIES_NAME cited in the ', a, ' block.')
                goto 2400
            end if
            do i = 1, ioseries
                if (sequation(i) == ' ') then
                    write (amessage, 9031) TRIM(currentblock_g)
9031                format('a SERIES_WEIGHTS_EQUATION keyword has not been provided for each ', &
                           'series cited in the ', a, ' block.')
                    goto 2400
                end if
            end do
        end if
        if (iostable /= 0) then
            if (iomstable /= iostable) then
                write (amessage, 9032) TRIM(currentblock_g)
9032            format('a MODEL_S_TABLE_NAME keyword has not been provided for each ', &
                       'OBSERVATION_S_TABLE_NAME cited in the ', a, ' block.')
                goto 2400
            end if
            do i = 1, iostable
                if (stequation(i) == ' ') then
                    write (amessage, 9033) TRIM(currentblock_g)
9033                format('an S_TABLE_WEIGHTS_EQUATION keyword has not been provided for each ', &
                           's_table cited in the ', a, ' block.')
                    goto 2400
                end if
            end do
        end if
        if (iovtable /= 0) then
            if (iomvtable /= iovtable) then
                write (amessage, 9034) TRIM(currentblock_g)
9034            format('a MODEL_V_TABLE_NAME keyword has not been provided for each ', &
                       'OBSERVATION_V_TABLE_NAME cited in the ', a, ' block.')
                goto 2400
            end if
            do i = 1, iovtable
                if (vtequation(i) == ' ') then
                    write (amessage, 9035) TRIM(currentblock_g)
9035                format('a V_TABLE_WEIGHTS_EQUATION keyword has not been provided for each ', &
                           'v_table cited in the ', a, ' block.')
                    goto 2400
                end if
            end do
        end if

        if (iodtable /= 0) then
            if (iomdtable /= iodtable) then
                write (amessage, 9036) TRIM(currentblock_g)
9036            format('a MODEL_E_TABLE_NAME keyword has not been provided for each ', &
                       'OBSERVATION_E_TABLE_NAME cited in the ', a, ' block.')
                goto 2400
            end if

            do i = 1, iodtable
                if (dtequation(i) == ' ') then
                    write (amessage, 9037) TRIM(currentblock_g)
9037                format('a E_TABLE_WEIGHTS_EQUATION keyword has not been provided for each ', &
                           'e_table cited in the ', a, 'block.')
                    goto 2400
                end if
            end do
        end if

        if (iogtable /= 0) then
            if (iomgtable /= iogtable) then
                write (amessage, 9038) TRIM(currentblock_g)
9038            format('a MODEL_G_TABLE_NAME keyword has not been provided for each ', &
                       'OBSERVATION_G_TABLE_NAME cited in the ', a, ' block.')
                goto 2400
            end if

            do i = 1, iogtable
                if (gtequation(i) == ' ') then
                    write (amessage, 9039) TRIM(currentblock_g)
9039                format('a G_TABLE_WEIGHTS_EQUATION keyword has not been provided for each ', &
                           'g_table cited in the ', a, 'block.')
                    goto 2400
                end if
            end do
        end if

        if (icontext == 0) then
            write (amessage, 9040) TRIM(currentblock_g)
9040        format('no Context keyword(s) provided in ', a, ' block.')
            goto 2400
        end if
        if ((micactlfile /= ' ') .AND. (pest2micacom == ' ')) then
            write (amessage, 9041)
9041        format('if a NEW_MICA_CONTROL_FILE keyword is supplied, a PEST2MICA_COMMAND ', &
                   'keyword must also be supplied.')
            goto 2400
        end if
        if ((isvd == 1) .AND. (iaui == 1)) then
            write (amessage, 9042) TRIM(currentblock_g)
9042        format('only one of the TRUNCATED_SVD or AUTOMATIC_USER_INTERVENTION keywords ', &
                   'must be supplied in ', a, ' block.')
            goto 2400
        end if

!    -- Before any processing is done, a check is made that the observation
!       series and tables correspond to the series and tables requested for
!       output in the last LIST_OUTPUT block.

        otherblock = 'LIST_OUTPUT'
        if ((ioseries /= imseries_g) .OR. (iostable /= imstable_g) .OR.                &
           (iovtable /= imvtable_g) .OR. (iodtable /= imdtable_g) .OR.                &
           (iogtable /= imgtable_g)) then
            write (amessage, 9043) TRIM(currentblock_g), TRIM(otherblock)
9043        format('the number of series, s_tables, e_tables, g_tables and ',     &
                   'v_tables cited in the ', a, ' block does not correspond ',    &
                   'exactly to the number of these entities cited in the ',       &
                   'immediately-preceding ', a, ' block.')
            goto 2400
        end if
        if (imctable_g /= 0) then
            write (amessage, 9044)
9044        format('a c_table features in the LIST_OUTPUT block preceding ',      &
                   'the WRITE_PEST_FILES block. The present version of TSPROC ',  &
                   'does not support the use of c_tables in the calibration ',    &
                   'process.')
            goto 2400
        end if
        if (ioseries /= 0) then
            do i = 1, ioseries
                io = obsseries(i)
                im = modseries(i)
                aoname = SERIES_G(io)%NAME
                amname = SERIES_G(im)%NAME
                if (io /= im) then
                    noterm = SERIES_G(io)%nterm
                    nmterm = SERIES_G(im)%nterm
                    if (noterm /= nmterm) then
                        write (amessage, 9045) TRIM(aoname), TRIM(amname)
9045                    format('OBSERVATION_SERIES "', a,                         &
                               '" has been matched to ', 'MODEL_SERIES "', a,     &
                               '". However these series have different ',         &
                               'numbers of terms.')
                        goto 2400
                    end if
                    do j = 1, noterm
                        if ((SERIES_G(io)%DAYS(j) /= SERIES_G(im)%DAYS(j)) .OR.      &
                           (SERIES_G(io)%SECS(j) /= SERIES_G(im)%SECS(j))) then
                            write (amessage, 9046) TRIM(aoname), TRIM(amname)
9046                        format('OBSERVATION_SERIES "', a,                  &
                                   '" has been matched to ', 'MODEL_SERIES "', a, &
                                   '". However the dates and times in ',          &
                                   'these SERIES do not correspond.')
                            goto 2400
                        end if
                    end do
                end if
                do j = 1, ioseries
                    if (im == IOUTSERIES_G(j)) goto 50
                end do
                write (amessage, 9047) TRIM(amname), TRIM(currentblock_g)
9047            format('MODEL__SERIES "', a, '" is not listed in the ',        &
                       'LIST_OUTPUT block immediately preceding the ', a,      &
                       ' block.')
                goto 2400
50          end do
        end if

        if (iostable /= 0) then
            do i = 1, iostable
                io = obsstable(i)
                im = modstable(i)
                aoname = STABLE_G(io)%NAME
                amname = STABLE_G(im)%NAME
                if (io /= im) then
                    if (((STABLE_G(io)%MAXIMUM < -1.0E36) .AND.                    &
                       (STABLE_G(im)%MAXIMUM > -1.0E36)) .OR.                     &
                       ((STABLE_G(io)%MAXIMUM > -1.0E36) .AND.                    &
                       (STABLE_G(im)%MAXIMUM < -1.0E36))) then
                        avariable = 'MAXIMUM'
                        goto 2300
                    end if
                    if (((STABLE_G(io)%MINIMUM < -1.0E36) .AND.                    &
                       (STABLE_G(im)%MINIMUM > -1.0E36)) .OR.                     &
                       ((STABLE_G(io)%MINIMUM > -1.0E36) .AND.                    &
                       (STABLE_G(im)%MINIMUM < -1.0E36))) then
                        avariable = 'MINIMUM'
                        goto 2300
                    end if
                    if (((STABLE_G(io)%RANGE < -1.0E36) .AND. (STABLE_G(im)%RANGE > &
                       -1.0E36)) .OR.                                            &
                       ((STABLE_G(io)%RANGE > -1.0E36) .AND. (STABLE_G(im)        &
                       %RANGE < -1.0E36))) then
                        avariable = 'RANGE'
                        goto 2300
                    end if
                    if (((STABLE_G(io)%MEAN < -1.0E36) .AND. (STABLE_G(im)%MEAN > - &
                       1.0E36)) .OR.                                              &
                       ((STABLE_G(io)%MEAN > -1.0E36) .AND. (STABLE_G(im)         &
                       %MEAN < -1.0E36))) then
                        avariable = 'MEAN'
                        goto 2300
                    end if
                    if (((STABLE_G(io)%MEDIAN < -1.0E36) .AND. (STABLE_G(im)%MEDIAN&
                       > -1.0E36)) .OR.                                          &
                       ((STABLE_G(io)%MEDIAN > -1.0E36) .AND. (STABLE_G(im)       &
                       %MEDIAN < -1.0E36))) then
                        avariable = 'MEDIAN'
                        goto 2300
                    end if
                    if (((STABLE_G(io)%STDDEV < -1.0E36) .AND. (STABLE_G(im)%STDDEV&
                       > -1.0E36)) .OR.                                          &
                       ((STABLE_G(io)%STDDEV > -1.0E36) .AND. (STABLE_G(im)       &
                       %STDDEV < -1.0E36))) then
                        avariable = 'STD_DEV'
                        goto 2300
                    end if
                    if (((STABLE_G(io)%TOTAL < -1.0E36) .AND. (STABLE_G(im)%TOTAL > &
                       -1.0E36)) .OR.                                            &
                       ((STABLE_G(io)%TOTAL > -1.0E36) .AND. (STABLE_G(im)        &
                       %TOTAL < -1.0E36))) then
                        avariable = 'SUM'
                        goto 2300
                    end if
                    if (((STABLE_G(io)%MINMEAN < -1.0E36) .AND.                    &
                       (STABLE_G(im)%MINMEAN > -1.0E36)) .OR.                     &
                       ((STABLE_G(io)%MINMEAN > -1.0E36) .AND.                    &
                       (STABLE_G(im)%MINMEAN < -1.0E36))) then
                        avariable = 'MINMEAN_*'
                        goto 2300
                    end if
                    if (((STABLE_G(io)%MAXMEAN < -1.0E36) .AND.                    &
                       (STABLE_G(im)%MAXMEAN > -1.0E36)) .OR.                     &
                       ((STABLE_G(io)%MAXMEAN > -1.0E36) .AND.                    &
                       (STABLE_G(im)%MAXMEAN < -1.0E36))) then
                        avariable = 'MAXMEAN_*'
                        goto 2300
                    end if
                    if ((STABLE_G(io)%MAXMEAN > -1.0E36) .OR.                      &
                       (STABLE_G(io)%MINMEAN > -1.0E36)) then
                        write (amessage, 9048)
9048                    format('The present version of TSPROC does not support ', &
                               'the use of S_TABLE minimum or maximum sample ',   &
                               'count averages in the calibration process.')
                        goto 2400
                    end if
                end if
                do j = 1, iostable
                    if (im == IOUTSTABLE_G(j)) goto 100
                end do
                write (amessage, 9125) 'S', TRIM(amname), TRIM(currentblock_g)
                goto 2400
100         end do
        end if

        if (iovtable /= 0) then
            do i = 1, iovtable
                io = obsvtable(i)
                im = modvtable(i)
                aoname = VTABLE_G(io)%NAME
                amname = VTABLE_G(im)%NAME
                if (io /= im) then
                    noterm = VTABLE_G(io)%nterm
                    nmterm = VTABLE_G(im)%nterm
                    if (noterm /= nmterm) then
                        write (amessage, 9049) TRIM(aoname), TRIM(amname)
9049                    format('OBSERVATION_V_TABLE "', a,                     &
                               '" has been matched to ', 'MODEL_V_TABLE "', a, &
                               '". However these V_TABLES ',                   &
                               'have different numbers of integration times.')
                        goto 2400
                    end if
                    do j = 1, noterm
                        if ((VTABLE_G(io)%DAYS1(j) /= VTABLE_G(im)%DAYS1(j)) .OR.    &
                           (VTABLE_G(io)%DAYS2(j) /= VTABLE_G(im)%DAYS2(j)) .OR.    &
                           (VTABLE_G(io)%SECS1(j) /= VTABLE_G(im)%SECS1(j)) .OR.    &
                           (VTABLE_G(io)%SECS2(j) /= VTABLE_G(im)%SECS2(j))) then
                            write (amessage, 9050) TRIM(aoname), TRIM(amname)
9050                        format('OBSERVATION_V_TABLE "', a,                    &
                                   '" has been matched to ', 'MODEL_V_TABLE "', a,&
                                   '". However the integration dates and ',       &
                                   'times in these V_TABLES do not correspond.')
                            goto 2400
                        end if
                    end do
                end if
                do j = 1, iovtable
                    if (im == IOUTVTABLE_G(j)) goto 150
                end do
                write (amessage, 9125) 'V', TRIM(amname), TRIM(currentblock_g)
                goto 2400
150         end do
        end if

        if (iodtable /= 0) then
            do i = 1, iodtable
                io = obsdtable(i)
                im = moddtable(i)
                aoname = DTABLE_G(io)%NAME
                amname = DTABLE_G(im)%NAME
                if (io /= im) then ! was 1079 previously SMW
                    noterm = DTABLE_G(io)%nterm
                    nmterm = DTABLE_G(im)%nterm
                    if (noterm /= nmterm) then
                        write (amessage, 9051) TRIM(aoname), TRIM(amname)
9051                    format('OBSERVATION_E_TABLE "', a,                       &
                               '"  has been matched to ', 'MODEL E_TABLE "', a,  &
                               '". However these E_TABLES ',                     &
                               'have different numbers of flows.')
                        goto 2400
                    end if
                    if (DTABLE_G(io)%UNDER_OVER /= DTABLE_G(im)%UNDER_OVER) then
                        write (amessage, 9052) TRIM(aoname), TRIM(amname)
9052                    format('OBSERVATION_E_TABLE "', a,                       &
                               '"  has been matched to ', 'MODEL E_TABLE "', a,  &
                               '". However these E_TABLES ',                     &
                               'have different UNDER_OVER specifications.')
                        goto 2400
                    end if
                end if
                ! check that the sequence and content of each item in the table
                ! is identical between observation and model table
                do j = 1, noterm
                    rotemp = DTABLE_G(io)%FLOW(j)
                    rmtemp = DTABLE_G(im)%FLOW(j)
                    rprecis = 5 * SPACING(rmtemp)
                    if ((rotemp < rmtemp - rprecis) .OR. (rotemp > rmtemp + rprecis))  &
                       then
                        write (amessage, 9053) TRIM(aoname), TRIM(amname)
9053                    format('OBSERVATION_E_TABLE "', a,                       &
                               '"  has been matched to ', 'MODEL E_TABLE "', a,  &
                               '". However the flows in ',                       &
                               'these E_TABLES do not correspond.')
                        goto 2400
                    end if
                end do
                do j = 1, noterm
                    rotemp = DTABLE_G(io)%TDELAY(j)
                    rmtemp = DTABLE_G(im)%TDELAY(j)
                    rprecis = 5 * SPACING(rmtemp)
                    if ((rotemp < rmtemp - rprecis) .OR. (rotemp > rmtemp + rprecis))  &
                       then
                        write (amessage, 9054) TRIM(aoname), TRIM(amname)
9054                    format('OBSERVATION_E_TABLE "', a,                       &
                               '"  has been matched to ', 'MODEL E_TABLE "', a,  &
                               '". However the time delays in ',                 &
                               'these E_TABLES do not correspond.')
                        goto 2400
                    end if
                end do
!            1079       continue
                do j = 1, iodtable
                    if (im == IOUTDTABLE_G(j)) goto 200
                end do
                write (amessage, 9125) 'D', TRIM(amname), TRIM(currentblock_g)
                goto 2400
200         end do
        end if

        ! check for a one-to-one correspondence in the items contained
        ! within each G_TABLE
        if (iogtable /= 0) then
            do i = 1, iogtable
                io = obsgtable(i)
                im = modgtable(i)
                aoname = GTABLE_G(io)%NAME
                amname = GTABLE_G(im)%NAME
                if (io /= im) then ! was 3079
                    noterm = UBOUND(GTABLE_G(io)%SDESCRIPTION, 1)
                    nmterm = UBOUND(GTABLE_G(im)%SDESCRIPTION, 1)
                    if (noterm /= nmterm) then
                        write (amessage, 9055) TRIM(aoname), TRIM(amname)
9055                    format('OBSERVATION_G_TABLE "', a,                       &
                               '"  has been matched to ', 'MODEL G_TABLE "', a,  &
                               '". However these G_TABLES ',                     &
                               'have different numbers of entries.')
                        goto 2400
                    end if
                end if
                do j = 1, noterm
                    if (.NOT. STR_COMPARE(GTABLE_G(io)%SDESCRIPTION(j), GTABLE_G(im)&
                       %SDESCRIPTION(j))) then
                        write (amessage, 9056) TRIM(aoname), TRIM(amname)
9056                    format('OBSERVATION_G_TABLE "', a,                       &
                               '"  has been matched to ', 'MODEL G_TABLE "', a,  &
                               '". However the items in ',                       &
                               'these G_TABLES do not correspond.')
                        goto 2400
                    end if
                end do
!            3079       continue
                do j = 1, iogtable
                    if (im == IOUTGTABLE_G(j)) goto 250
                end do
                write (amessage, 9125) 'G', TRIM(amname), TRIM(currentblock_g)
                goto 2400
250         end do
        end if

!    -- If present, the parameter group file is read.
        if (pargroupfile == ' ') goto 500
        call ADDQUOTE(pargroupfile, sstring_g)
        write (*, 9126) TRIM(sstring_g)
        write (lu_rec, 9126) TRIM(sstring_g)
        iunit = NEXTUNIT()
        open (unit=iunit, file=pargroupfile, status='old', iostat=ierr)
        if (ierr /= 0) then
            write (amessage, 9057) TRIM(sstring_g)
9057        format('cannot open parameter group file ', a)
            goto 2400
        end if

!    -- The file is read a first time to find out the number of groups
        f_numpargp = 0
        do
            read (iunit, '(a)', err=2000, end=300) cline
            if (LEN_TRIM(cline) == 0) cycle
            if (cline(1:1) == '#') cycle
            f_numpargp = f_numpargp + 1
        end do

300     if (f_numpargp == 0) then
            write (amessage, 9127) TRIM(sstring_g)
            goto 2400
        end if

        allocate (f_pargpnme(f_numpargp), f_inctyp(f_numpargp),                  &
                 f_derinc(f_numpargp), f_derinclb(f_numpargp),                   &
                 f_forcen(f_numpargp), f_derincmul(f_numpargp),                  &
                 f_dermthd(f_numpargp), STAT=ierr)
        if (ierr /= 0) goto 1700
        rewind (unit=iunit, iostat=ierr)
        if (ierr /= 0) goto 1900

        call vstrlist_new(groups_dat)
        call tlist%set_tokenizer(token_whitespace, token_empty, token_empty)

        jline = 0
        igp = 0
        do
            jline = jline + 1
            call NUM2CHAR(jline, aline)
            read (iunit, '(a)', err=2000, end=400) cline
            if (LEN_TRIM(cline) == 0) cycle
            if (cline(1:1) == '#') cycle

            igp = igp + 1
            call CASETRANS(cline, 'lo')

            call tlist%tokenize(cline)

            if (tlist%number() /= 7) then
                write (amessage, 9902) TRIM(aline), TRIM(sstring_g)
9902            format('require 7 items on line ', a, ' in file "', a, '"')
                goto 2400
            end if

            f_pargpnme(igp) = tlist%token(1)
            if (LEN_TRIM(f_pargpnme(igp)) > 12) then
                write (amessage, 9128) TRIM(f_pargpnme(igp)), TRIM(aline),       &
                      TRIM(sstring_g)
                goto 2400
            end if
            if (f_pargpnme(igp) == 'none') then
                write (amessage, 9058) TRIM(sstring_g)
9058            format('parameter group name "none" in file ', a,                &
                       ' is a reserved ',                                        &
                       'name, used for some fixed and tied parameters.')
                goto 2400
            end if

            call vstrlist_append(groups_dat, TRIM(f_pargpnme(igp)))

            f_inctyp(igp) = tlist%token(2)
            if ((f_inctyp(igp) /= 'relative') .AND.                              &
               (f_inctyp(igp) /= 'absolute') .AND.                               &
               (f_inctyp(igp) /= 'rel_to_max')) then
                write (amessage, 9059) TRIM(aline), TRIM(sstring_g)
9059            format('INCTYP on line ', a, ' of file ', a, ' must be ',        &
                       '"relative", "absolute" or "rel_to_max".')
                goto 2400
            end if

            call CHAR2NUM(ierr, tlist%token(3), f_derinc(igp))
            if (ierr /= 0) then
                write (amessage, 9132) 'DERINC', TRIM(aline), TRIM(sstring_g)
                goto 2400
            end if
            if (f_derinc(igp) <= 0.0) then
                write (amessage, 9129) 'DERINC', TRIM(aline), TRIM(sstring_g)
                goto 2400
            end if

            call CHAR2NUM(ierr, tlist%token(4), f_derinclb(igp))
            if (ierr /= 0) then
                write (amessage, 9132) 'DERINCLB', TRIM(aline), TRIM(sstring_g)
                goto 2400
            end if
            if (f_derinclb(igp) < 0.0) then
                write (amessage, 9060) 'DERINCLB', TRIM(aline), TRIM(sstring_g)
9060            format('value for ', a, ' on line ', a, ' of file ', a,          &
                       ' must not be negative.')
                goto 2400
            end if

            f_forcen(igp) = tlist%token(5)
            if ((f_forcen(igp) /= 'switch') .AND. (f_forcen(igp) /= 'always_2')  &
               .AND. (f_forcen(igp) /= 'always_3')) then
                write (amessage, 9061) TRIM(aline), TRIM(sstring_g)
9061            format('FORCEN must be "switch", "always_2" or ',                &
                       '"always_3" at line ', a, ' of file ', a)
                goto 2400
            end if

            call CHAR2NUM(ierr, tlist%token(6), f_derincmul(igp))
            if (ierr /= 0) then
                write (amessage, 9132) 'DERINCMUL', TRIM(aline), TRIM(sstring_g)
                goto 2400
            end if
            if (f_derincmul(igp) <= 0.0) then
                write (amessage, 9129) 'DERINCMUL', TRIM(aline), TRIM(sstring_g)
                goto 2400
            end if

            f_dermthd(igp) = tlist%token(7)
            if ((f_dermthd(igp) /= 'parabolic') .AND.                            &
               (f_dermthd(igp) /= 'best_fit') .AND.                              &
               (f_dermthd(igp) /= 'outside_pts')) then
                write (amessage, 9062) TRIM(aline), TRIM(sstring_g)
9062            format('DERMTHD must be "parabolic", "best_fit" or ',            &
                       '"outside_pts" on line ', a, ' of file ', a)
                goto 2400
            end if
        end do
400     continue

! -- Test for duplicate group names
        dat_sorted = vstrlist_sort(groups_dat, unique=.TRUE.)

        if (vstrlist_length(groups_dat) /= vstrlist_length(dat_sorted)) then
            write (amessage, 9063) TRIM(sstring_g)
9063        format('2 parameter groups have the same name in file ', a)
            goto 2400
        end if

        call NUM2CHAR(f_numpargp, aline)
        write (*, 9130) TRIM(aline), TRIM(sstring_g)
        write (lu_rec, 9130) TRIM(aline), TRIM(sstring_g)
        close (unit=iunit)

!    -- If present, the parameter data file is read.
500     if (pardatfile == ' ') goto 800
        call ADDQUOTE(pardatfile, sstring_g)
        write (*, 9131) TRIM(sstring_g)
        write (lu_rec, 9131) TRIM(sstring_g)
        iunit = NEXTUNIT()
        open (unit=iunit, file=pardatfile, status='old', iostat=ierr)
        if (ierr /= 0) then
            write (amessage, 9064) TRIM(sstring_g)
9064        format('cannot open parameter data file ', a)
            goto 2400
        end if

!    -- The file is read a first time to obtain the number of parameters.
        jline = 0
        f_numpar = 0 ! number of parameters
        f_nequation = 0 ! number of equations
        f_nparsec = 0 ! number of secondary parameters =
        ! number of unique equation parameter names on LHS
        ! of all equations
        iequation = 0
        do
            jline = jline + 1
            read (iunit, '(a)', err=2000, end=600) cline
            if (LEN_TRIM(cline) == 0) cycle
            if (cline(1:1) == '#') cycle
            if (INDEX(cline, '=') > 0) then
                f_nequation = f_nequation + 1
            else
                f_numpar = f_numpar + 1
            end if
        end do
600     if (f_numpar == 0) then
            write (amessage, 9127) TRIM(sstring_g)
            goto 2400
        end if
        allocate (f_parnme(f_numpar), f_partrans(f_numpar), f_parchglim(f_numpar), &
                 f_parval1(f_numpar), f_parlbnd(f_numpar), f_parubnd(f_numpar),   &
                 f_pargp(f_numpar), f_scale(f_numpar), f_offset(f_numpar),        &
                 STAT=ierr)
        if (ierr /= 0) goto 1700
        allocate (f_equation(f_nequation), f_parsecnme(f_nequation), STAT=ierr)
        if (ierr /= 0) goto 1700

        rewind (unit=iunit, iostat=ierr)
        if (ierr /= 0) goto 1900

        call vstrlist_new(params_par_dat)
        call vstrlist_new(params_eq_dat)
        call vstrlist_new(params_grp_dat)
!    -- Now it is read a second time to obtain the data.
        jline = 0
        ipar = 0
        do
            jline = jline + 1
            call NUM2CHAR(jline, aline)
            read (iunit, '(A)', err=2000, end=700) cline
            if (LEN_TRIM(cline) /= 0) then
                if (cline(1:1) /= '#') then
                    call CASETRANS(cline, 'lo')
                    call tlist%tokenize(cline)
                    if (INDEX(cline, '=') > 0) then
                        ! equations will only have 3 tokens on the line
                        if (tlist%number() < 3) goto 2100
                    else
                        if (tlist%number() < 9) goto 2100
                    end if

                    if (LEN_TRIM(tlist%token(1)) > 12) then
                        write (amessage, 9065) TRIM(tlist%token(1)), TRIM(aline),   &
                              TRIM(sstring_g)
9065                    format('parameter name "', a,                             &
                               '" greater than 12 characters in length ',         &
                               'at line ', a, ' of file ', a)
                        goto 2400
                    end if

                    if (INDEX(cline, '=') > 0) then
                        iequation = iequation + 1

                        f_parsecnme(iequation) = tlist%token(1)
                        call vstrlist_append(params_eq_dat,                       &
                                             TRIM(f_parsecnme(iequation)))

                        f_equation(iequation) = tlist%token(3)
                    else
                        ipar = ipar + 1

                        f_parnme(ipar) = tlist%token(1)

                        call vstrlist_append(params_par_dat,                      &
                                             TRIM(f_parnme(ipar)))

                        f_partrans(ipar) = tlist%token(2)
                        if ((f_partrans(ipar) /= 'log') .AND.                  &
                           (f_partrans(ipar) /= 'none') .AND.                  &
                           (f_partrans(ipar) (1:4) /= 'tied') .AND.            &
                           (f_partrans(ipar) /= 'fixed')) then
                            write (amessage, 9066) TRIM(aline), TRIM(sstring_g)
9066                        format('PARTRANS on line ', a, ' of file ', a,     &
                                   ' must be "none", "log", "fixed" or ',      &
                                   '"tied_(parameter name)".')
                            goto 2400
                        end if
                        if ((f_partrans(ipar) == 'tied') .OR.                  &
                           (f_partrans(ipar) == 'tied_')) then
                            write (amessage, 9067) TRIM(aline), TRIM(sstring_g)
9067                        format('the parent parameter name must follow the ',  &
                                   '"tied_" string at line ', a, ' of file ', a)
                            goto 2400
                        end if

                        f_parchglim(ipar) = tlist%token(3)
                        if ((f_parchglim(ipar) /= 'relative') .AND.            &
                           (f_parchglim(ipar) /= 'factor')) then
                            write (amessage, 9068) TRIM(aline), TRIM(sstring_g)
9068                        format('PARCHGLIM on line ', a, ' of file ', a,    &
                                   ' must be ', '"relative" or "factor".')
                            goto 2400
                        end if

                        call CHAR2NUM(ierr, tlist%token(4), f_parval1(ipar))
                        if (ierr /= 0) then
                            write (amessage, 9132) 'PARVAL1', TRIM(aline),     &
                                  TRIM(sstring_g)
                            goto 2400
                        end if

                        call CHAR2NUM(ierr, tlist%token(5), f_parlbnd(ipar))
                        if (ierr /= 0) then
                            write (amessage, 9132) 'PARLBND', TRIM(aline),     &
                                  TRIM(sstring_g)
                            goto 2400
                        end if

                        call CHAR2NUM(ierr, tlist%token(6), f_parubnd(ipar))
                        if (ierr /= 0) then
                            write (amessage, 9132) 'PARUBND', TRIM(aline),     &
                                  TRIM(sstring_g)
                            goto 2400
                        end if

                        f_pargp(ipar) = tlist%token(7)
                        if (LEN_TRIM(f_pargp(ipar)) > 12) then
                            write (amessage, 9128) TRIM(f_pargp(ipar)),        &
                                  TRIM(aline), TRIM(sstring_g)
                            goto 2400
                        end if

                        i = vstrlist_search(groups_dat, f_pargp(ipar))
                        if (i > 0) then
                            write (amessage, 9910) TRIM(f_pargp(ipar)),        &
                                  TRIM(aline), TRIM(sstring_g)
9910                        format('Group "', a, '" at line ', a, ' in file "',&
                                    a, '" is not in the group file')
                            goto 2400
                        end if
                        call vstrlist_append(params_grp_dat, TRIM(f_pargp(ipar)))

                        call CHAR2NUM(ierr, tlist%token(8), f_scale(ipar))
                        if (ierr /= 0) then
                            write (amessage, 9132) 'SCALE', TRIM(aline),       &
                                  TRIM(sstring_g)
                            goto 2400
                        end if

                        call CHAR2NUM(ierr, tlist%token(9), f_offset(ipar))
                        if (ierr /= 0) then
                            write (amessage, 9132) 'OFFSET', TRIM(aline),      &
                                  TRIM(sstring_g)
                            goto 2400
                        end if
                    end if
                end if
            end if
        end do
700     continue

!    -- Some checks are made of the parameter data.
! -- Test for duplicate parameter names
        dat_sorted = vstrlist_sort(params_par_dat, unique=.TRUE.)

        if (vstrlist_length(params_par_dat) /= vstrlist_length(dat_sorted)) then
            write (amessage, 9963) TRIM(sstring_g)
9963        format('2 parameter names have the same name in file ', a)
            goto 2400
        end if

!    -- Primary and secondary parameters can't have the same name.
        do i = 1, f_numpar
            do j = 1, f_nequation
                if (f_parnme(i) == f_parsecnme(j)) then
                    write (amessage, 9901) TRIM(f_parnme(i))
9901                format('A secondary parameter (equation) has the same name '  &
                           'as a primary parameter: ', a)
                    goto 2400
                end if
            end do
        end do

!    -- Find out how many secondary parameters are there.
        f_nparsec = vstrlist_length(vstrlist_sort(params_eq_dat, unique=.TRUE.))

!    -- If any parameters are tied, parameter linkages are now read.
        do ipar = 1, f_numpar
            if (f_partrans(ipar) (1:4) == 'tied') then
                atemp = f_partrans(ipar) (6:)
                if (atemp == f_parnme(ipar)) then
                    write (amessage, 9070) TRIM(atemp), TRIM(sstring_g)
9070                format('parameter "', a, '" is tied to itself in file ', a)
                    goto 2400
                end if
            end if
        end do

!    -- If any parameters are tied to a parameter which does not exist, this
!       is now rectified.  The transformation is set to 'none', but might
!       should be an error.
        do ipar = 1, f_numpar
            if (f_partrans(ipar) (1:4) == 'tied') then
                aapar = f_partrans(ipar) (6:)
                do i = 1, f_numpar
                    if (aapar == f_parnme(i)) goto 1200
                end do
                f_partrans(ipar) = 'none'
            end if
1200    end do

        call NUM2CHAR(f_numpar, aline)
        write (*, 9133) TRIM(aline), TRIM(sstring_g)
        write (lu_rec, 9133) TRIM(aline), TRIM(sstring_g)
        close (unit=iunit)

!    -- Next the names of all parameters are ascertained by reading template
!       files.
800     numtempfile = itempfile
        call vstrlist_new(template_params)
        npar = 0
        read_template_file: do itempfile = 1, numtempfile
            nnpar = 0
            tempunit = NEXTUNIT()
            call ADDQUOTE(tempfile(itempfile), sstring_g)
            write (*, 9134) TRIM(sstring_g)
            write (lu_rec, 9134) TRIM(sstring_g)
            open (unit=tempunit, file=tempfile(itempfile), status='old',       &
                 iostat=ierr)
            if (ierr /= 0) then
                write (amessage, 9071) TRIM(sstring_g)
9071            format('cannot open template file ', a)
                goto 2400
            end if
            jline = 1
            read (tempunit, '(a)', err=2000, end=900) cline
            call CASETRANS(cline, 'lo')
            if (cline(1:3) /= 'ptf') then
                write (amessage, 9072) TRIM(sstring_g)
9072            format('"ptf" header missing from first line of file ', a)
                goto 2400
            end if
            pardelim = cline(5:5)
            if ((pardelim == ' ') .OR. (INDEX('1234567890,;:', pardelim) /= 0) .OR.    &
               (INDEX('abcdefghijklmnopqrstuvwxyz', pardelim) /= 0)) then
                write (amessage, 9073) TRIM(sstring_g)
9073            format('invalid parameter delimeter on line 1 of file ', a)
                goto 2400
            end if
            read_a_line: do
                ii1 = 1
                jline = jline + 1
                read (tempunit, '(a)', err=2000, end=900) cline
                ll = LEN(cline)
820             j = INDEX(cline(ii1:), pardelim)
                if (j == 0) cycle read_a_line
                if (j > ll) cycle read_a_line
                ii1 = ii1 + j - 1
                j = 0
                if (ii1 <= ll) j = INDEX(cline(ii1 + 1:), pardelim)
                if (j == 0) then
                    call NUM2CHAR(jline, aline)
                    write (amessage, 9074) TRIM(aline), TRIM(sstring_g)
9074                format('unbalanced parameter delimiters on line ', a,      &
                           ' of template file ', a)
                    goto 2400
                end if
                jj1 = ii1 + j
                ii1 = ii1 + 1
                jj1 = jj1 - 1
                if (jj1 - ii1 + 1 <= 0) then
                    call NUM2CHAR(jline, aline)
                    write (amessage, 9075) TRIM(aline), TRIM(sstring_g)
9075                format('parameter space has zero width at line ', a,       &
                           ' of template file ', a)
                    goto 2400
                end if
                do jj = ii1, jj1
                    if (cline(jj:jj) /= ' ') then
                        do kk = jj, jj1
                            if (cline(kk:kk) == ' ') goto 825
                        end do
                        kk = jj1 + 1
825                     kk = kk - 1
                        goto 840
                    end if
                end do
                call NUM2CHAR(jline, aline)
                write (amessage, 9076) TRIM(aline), TRIM(sstring_g)
9076            format('blank parameter space at line ', a, ' of template ',   &
                       'file ', a)
                goto 2400
840             if (kk - jj + 1 > 12) then
                    call NUM2CHAR(jline, aline)
                    write (amessage, 9077) TRIM(aline), TRIM(sstring_g)
9077                format('parameter name greater than 12 characters in ',    &
                           'line ', a, ' of template file ', a)
                    goto 2400
                end if
                if (cline(kk + 1:jj1) /= ' ') then
                    call NUM2CHAR(jline, aline)
                    write (amessage, 9078) TRIM(aline), TRIM(sstring_g)
9078                format('parameter name includes a space character at line ',  &
                           a, ' of file ', a)
                    goto 2400
                end if
                aapar = cline(jj:kk)
                aapar = ADJUSTL(aapar)
                call CASETRANS(aapar, 'lo')
                if (npar /= 0) then
                    do ipar = 1, npar
                        if (aapar == apar(ipar)) goto 860
                    end do
                    npar = npar + 1
                    nnpar = nnpar + 1
                    if (npar > maxpar) then
                        call NUM2CHAR(maxpar, aline)
                        write (amessage, 9079) TRIM(aline)
9079                    format('number of parameters cited in template files ',&
                               'is limited to ', a, '. Increase MAXPAR and ',  &
                               're-compile program.')
                        goto 2400
                    end if
                else
                    npar = 1
                    nnpar = 1
                end if
                apar(npar) = aapar
                call vstrlist_append(template_params, TRIM(apar(npar)))
860             ii1 = jj1 + 2
                goto 820
            end do read_a_line
900         call NUM2CHAR(nnpar, aline)
            if (itempfile == 1) then
                write (*, 9135) TRIM(aline), TRIM(sstring_g)
                write (lu_rec, 9135) TRIM(aline), TRIM(sstring_g)
            else
                write (*, 9136) TRIM(aline), TRIM(sstring_g)
                write (lu_rec, 9136) TRIM(aline), TRIM(sstring_g)
            end if
            close (unit=tempunit, err=2200)
        end do read_template_file

! -- Get unique list of parameter names in all templates.
        template_params = vstrlist_sort(template_params, unique=.TRUE.)

!    -- Observations are named and the instruction file is now written.
        nobs = 0
        nobsgp = 0

        iunit = NEXTUNIT()
        call ADDQUOTE(instructfile, sstring_g)
        write (*, 9137) TRIM(sstring_g)
        write (lu_rec, 9137) TRIM(sstring_g)
        inquire (file=instructfile, exist=lexist)
        if (lexist) then
            write (6, *)
            do
                write (*, 9138, advance='no') TRIM(sstring_g)
                read (5, '(a)') aa
                call CASETRANS(aa, 'lo')
                if ((aa == 'y') .OR. (aa == 'n')) then
                    if (aa == 'n') then
                        write (*, 9139)
                        write (lu_rec, 9139)
                        Ifail = 1
                        return
                    end if
                    exit
                end if
            end do
        end if
        open (unit=iunit, file=instructfile, iostat=ierr)
        if (ierr /= 0) then
            write (amessage, 9140) TRIM(sstring_g)
            goto 2400
        end if
        write (iunit, 9080)
9080    format('pif $')

!    -- First the time series instructions are written.
        iout = 0
        if (ioseries /= 0) then
            do i = 1, ioseries
                iout = iout + 1
                im = IOUTSERIES_G(i)
                do j = 1, ioseries
                    if (im == modseries(j)) goto 920
                end do
                write (amessage, 9141) TRIM(SERIES_G(im)%NAME)
                goto 2400
920             io = obsseries(j)
                nsterm = SERIES_G(io)%nterm
                aname = SERIES_G(im)%NAME
                nobsgp = nobsgp + 1
                obgnme(nobsgp) = aname
                call MAKE_BASENAME(ierr, iout, nsterm, aname, basename)
                if (ierr /= 0) goto 2400
                atemp = basename(iout)
                do iterm = 1, nsterm
                    call NUM2CHAR(iterm, anum)
                    if (TRIM(sseriesformat_g) == "ssf" .OR. &
                        TRIM(sseriesformat_g) == "long") then
                        aname = '['//TRIM(atemp)//TRIM(anum)//']42:65'
                    else
                        aname = '['//TRIM(atemp)//TRIM(anum)//']2:25'
                    end if
                    if (iterm == 1 .AND. TRIM(sseriesformat_g) /= "ssf") then
                        write (iunit, "('l3',t6,a)") TRIM(aname)
                    else
                        write (iunit, "('l1',t6,a)") TRIM(aname)
                    end if
                    nobs = nobs + 1
                end do
            end do
        end if

!    -- Next the S_TABLE instructions are written.
        if (iostable /= 0) then
            siout = 0
            do i = 1, iostable
                il = 0
                siout = siout + 1
                im = IOUTSTABLE_G(i)
                do j = 1, iostable
                    if (im == modstable(j)) goto 940
                end do
                write (amessage, 9142) 's', TRIM(STABLE_G(im)%NAME), 'S'
                goto 2400
940             io = obsstable(j)
                aname = STABLE_G(im)%NAME
                nobsgp = nobsgp + 1
                obgnme(nobsgp) = aname
                sbasename(siout) = aname(1:12)
                if (siout > 1) then
                    do j = 1, siout - 1
                        if (sbasename(j) == sbasename(siout)) then
                            write (amessage, 9081)
9081                        format('TSPROC cannot generate unique observation ',  &
                                   'names from the names of the MODEL_S_TABLES ', &
                                   'involved in the calibration process. Alter ', &
                                   'the first twelve letters of at least one ',   &
                                   'of the model S_TABLE names.')
                            goto 2400
                        end if
                    end do
                end if
                if (STABLE_G(io)%MAXIMUM > -1.0E36) then
                    il = il + 1
                    aname = '['//TRIM(sbasename(siout))//obschar//'max]51:69'
                    if (il == 1) then
                        write (iunit, 9143) TRIM(aname)
                    else
                        write (iunit, 9144) TRIM(aname)
                    end if
                    nobs = nobs + 1
                end if
                if (STABLE_G(io)%MINIMUM > -1.0E36) then
                    il = il + 1
                    aname = '['//TRIM(sbasename(siout))//obschar//'min]51:69'
                    if (il == 1) then
                        write (iunit, 9143) TRIM(aname)
                    else
                        write (iunit, 9144) TRIM(aname)
                    end if
                    nobs = nobs + 1
                end if
                if (STABLE_G(io)%RANGE > -1.0E36) then
                    il = il + 1
                    aname = '['//TRIM(sbasename(siout))//obschar//'range]51:69'
                    if (il == 1) then
                        write (iunit, 9143) TRIM(aname)
                    else
                        write (iunit, 9144) TRIM(aname)
                    end if
                    nobs = nobs + 1
                end if

                if (STABLE_G(io)%TOTAL > -1.0E36) then
                    il = il + 1
                    aname = '['//TRIM(sbasename(siout))//obschar//'sum]51:69'
                    if (il == 1) then
                        write (iunit, 9143) TRIM(aname)
                    else
                        write (iunit, 9144) TRIM(aname)
                    end if
                    nobs = nobs + 1
                end if
                if (STABLE_G(io)%MEAN > -1.0E36) then
                    il = il + 1
                    aname = '['//TRIM(sbasename(siout))//obschar//'mean]51:69'
                    if (il == 1) then
                        write (iunit, 9143) TRIM(aname)
                    else
                        write (iunit, 9144) TRIM(aname)
                    end if
                    nobs = nobs + 1
                end if
                if (STABLE_G(io)%MEDIAN > -1.0E36) then
                    il = il + 1
                    aname = '['//TRIM(sbasename(siout))//obschar//'median]51:69'
                    if (il == 1) then
                        write (iunit, 9143) TRIM(aname)
                    else
                        write (iunit, 9144) TRIM(aname)
                    end if
                    nobs = nobs + 1
                end if
                if (STABLE_G(io)%STDDEV > -1.0E36) then
                    il = il + 1
                    aname = '['//TRIM(sbasename(siout))//obschar//'sd]51:69'
                    if (il == 1) then
                        write (iunit, 9143) TRIM(aname)
                    else
                        write (iunit, 9144) TRIM(aname)
                    end if
                    nobs = nobs + 1
                end if
            end do
        end if

!    -- Next the V_TABLE instructions are written.
        if (iovtable /= 0) then
            do i = 1, iovtable
                iout = iout + 1
                im = IOUTVTABLE_G(i)
                do j = 1, iovtable
                    if (im == modvtable(j)) goto 960
                end do
                write (amessage, 9142) 'v', TRIM(VTABLE_G(im)%NAME), 'V'
                goto 2400
960             io = obsvtable(j)
                nsterm = VTABLE_G(io)%nterm
                aname = VTABLE_G(im)%NAME
                nobsgp = nobsgp + 1
                obgnme(nobsgp) = aname
                call MAKE_BASENAME(ierr, iout, nsterm, aname, basename)
                if (ierr /= 0) goto 2400
                atemp = basename(iout)
                do iterm = 1, nsterm
                    call NUM2CHAR(iterm, anum)
                    aname = '['//TRIM(atemp)//TRIM(anum)//']62:81'
                    if (iterm == 1) then
                        write (iunit, 9145) TRIM(aname)
                    else
                        write (iunit, 9146) TRIM(aname)
                    end if
                    nobs = nobs + 1
                end do
            end do
        end if

!    -- Next the E_TABLE instructions are written.
        if (iodtable /= 0) then
            do i = 1, iodtable
                iout = iout + 1
                im = IOUTDTABLE_G(i)
                do j = 1, iodtable
                    if (im == moddtable(j)) goto 980
                end do
                write (amessage, 9142) 'e', TRIM(VTABLE_G(im)%NAME), 'E'
                goto 2400
980             io = obsdtable(j)
                nsterm = DTABLE_G(io)%nterm
                aname = DTABLE_G(im)%NAME
                nobsgp = nobsgp + 1
                obgnme(nobsgp) = aname !!!*** is this correct??
                call MAKE_BASENAME(ierr, iout, nsterm, aname, basename)
                if (ierr /= 0) goto 2400
                atemp = basename(iout)
                do iterm = 1, nsterm
                    call NUM2CHAR(iterm, anum)
                    aname = '['//TRIM(atemp)//TRIM(anum)//']59:78'
                    if (iterm == 1) then
                        write (iunit, 9145) TRIM(aname)
                    else
                        write (iunit, 9146) TRIM(aname)
                    end if
                    nobs = nobs + 1
                end do
            end do
        end if

!    -- Next the G_TABLE instructions are written.
        if (iogtable /= 0) then
            do i = 1, iogtable
                iout = iout + 1
                im = IOUTGTABLE_G(i)
                do j = 1, iogtable
                    if (im == modgtable(j)) goto 1000
                end do
                write (amessage, 9142) 'g', TRIM(GTABLE_G(im)%NAME), 'G'
                goto 2400
1000            io = obsgtable(j)
                nsterm = UBOUND(GTABLE_G(io)%SDESCRIPTION, 1)
                aname = GTABLE_G(im)%NAME
                nobsgp = nobsgp + 1
                obgnme(nobsgp) = aname !!!*** is this correct??
                call MAKE_BASENAME(ierr, iout, nsterm, aname, basename)
                if (ierr /= 0) goto 2400
                atemp = basename(iout)
                do iterm = 1, nsterm
                    call NUM2CHAR(iterm, anum)
                    aname = '['//TRIM(atemp)//TRIM(anum)//']82:96'
                    if (iterm == 1) then
                        write (iunit, 9145) TRIM(aname)
                    else
                        write (iunit, 9146) TRIM(aname)
                    end if
                    nobs = nobs + 1
                end do
            end do
        end if

        close (unit=iunit)
        write (*, 9147) TRIM(sstring_g)
        write (lu_rec, 9147) TRIM(sstring_g)

! -- Find out parameters in parameter file but not in any template file.
        dat_sorted = params_par_dat
        tpl_sorted = template_params
        do ipar = 1, vstrlist_length(params_par_dat)
            do j = 1, vstrlist_length(template_params)
                if (vstring_equals(vstrlist_index(params_par_dat, ipar),       &
                                  vstrlist_index(template_params, j))) then
                    i = vstrlist_search(dat_sorted,                            &
                                        vstrlist_index(params_par_dat, ipar))
                    if (i > 0) then
                        call vstrlist_remove(dat_sorted, i)
                    end if
                    i = vstrlist_search(tpl_sorted,                            &
                                        vstrlist_index(template_params, j))
                    if (i > 0) then
                        call vstrlist_remove(tpl_sorted, i)
                    end if
                end if
            end do
        end do

! -- Find out secondary parameters in parameter file but not in any template file.
        eq_sorted = params_eq_dat
        do ipar = 1, vstrlist_length(params_eq_dat)
            ! -- This inner do loop is not efficient since tpl_sorted has already
            ! been condensed, however, I didn't see an easy way to fix.
            do j = 1, vstrlist_length(template_params)
                if (vstring_equals(vstrlist_index(params_eq_dat, ipar),            &
                                  vstrlist_index(template_params, j))) then
                    i = vstrlist_search(eq_sorted, vstrlist_index(params_eq_dat, ipar))
                    if (i > 0) then
                        call vstrlist_remove(eq_sorted, i)
                    end if
                    i = vstrlist_search(tpl_sorted, vstrlist_index(template_params, j))
                    if (i > 0) then
                        call vstrlist_remove(tpl_sorted, i)
                    end if
                end if
            end do
        end do

! -- Give ERROR about parameters in templates but not in parameters.
        if (vstrlist_length(tpl_sorted) > 0) then
            do j = 1, vstrlist_length(tpl_sorted)
                call vstring_cast(vstrlist_index(tpl_sorted, j), aapar)
                write (amessage, 9082) TRIM(aapar)
9082            format('Parameter "', a, '" was found in the template file ',     &
                       'but was not in the parameter file.')
            end do
            goto 2400
        end if

        ! -- If equations then give WARNING about parameters not in any template,
        ! -- however give an error if f_nequation==0
        if (f_nequation > 0 .AND. vstrlist_length(dat_sorted) > 0) then
            write (*, 9981)
            write (*, 9982)
9981        format(/, /, "The following parameters are not in any template.")
9982        format(/, "This could be fine since they might be used in an",        &
                   /, "equation, however listed here in case there might",        &
                   /, "be a mistake and they should in fact be in a template",    &
                   /, "file.",/)

            write (lu_rec, 9981)
            write (lu_rec, 9982)
            call write_vstrlist(dat_sorted, 5, lu_rec)
            write (*, *)
            write (*, *)
            write (lu_rec, *)
            write (lu_rec, *)
        elseif (f_nequation == 0 .AND. vstrlist_length(dat_sorted) > 0) then
            write (*, 9992)
9992        format(/, /, 'The following parameters are not in any template file.',&
                                                /)
            call write_vstrlist(dat_sorted, 5, lu_rec)
            goto 2400
        end if

! -- Give WARNING about secondary parameters not in any template.
        if (vstrlist_length(eq_sorted) > 0) then
            write (*, 9983)
            write (*, 9982)
9983        format(/, /, "The following secondary parameters are not in any "     &
                                                "template.")
            write (lu_rec, 9983)
            write (lu_rec, 9982)
            call write_vstrlist(eq_sorted, 5, lu_rec)
            write (*, *)
            write (*, *)
            write (lu_rec, *)
            write (lu_rec, *)
        end if

!    -- Parameter and parameter group data are now assimilated on the basis of
!    information read from the parameter data file, the parameter group file
!    and the template files.

!    -- Parameter groups are now organised.
        npargp = 0
        do ipar = 1, f_numpar
            apargp = f_pargp(ipar)
            if (apargp == 'none') then
                if ((f_partrans(ipar) == 'tied') .OR. (f_partrans(ipar) == 'fixed')) cycle
                call ADDQUOTE(pardatfile, sstring_g)
                write (amessage, 9083) TRIM(apar(ipar)), TRIM(sstring_g)
9083            format('parameter "', a,                                          &
                       '" has been assigned to parameter group "none" ',          &
                       'in file ', a, ' but is not tied or fixed.')
                goto 2400
            end if
        end do

!    -- The "* control data" section of the PEST control file is now written.
        iunit = NEXTUNIT()
        call ADDQUOTE(pestctlfile, sstring_g)
        write (*, 9148) TRIM(sstring_g)
        write (lu_rec, 9148) TRIM(sstring_g)
        inquire (file=pestctlfile, exist=lexist)
        if (lexist) then
            write (6, *)
            do
                write (*, 9138, advance='no') TRIM(sstring_g)
                read (5, '(a)') aa
                call CASETRANS(aa, 'lo')
                if ((aa == 'y') .OR. (aa == 'n')) then
                    if (aa == 'n') then
                        write (*, 9139)
                        write (lu_rec, 9139)
                        Ifail = 1
                        return
                    end if
                    exit
                end if
            end do
        end if

        nnnpar = f_numpar
        open (unit=iunit, file=pestctlfile, iostat=ierr)
        if (ierr /= 0) then
            write (amessage, 9140) TRIM(sstring_g)
            goto 2400
        end if
        write (iunit, 9084)
9084    format('pcf')
        write (iunit, 9085)
9085    format('* control data')
        write (iunit, 9086)
9086    format('restart estimation')
        write (iunit, 9087) nnnpar, nobs, f_numpargp, 0, nobsgp, f_nparsec, f_nequation
9087    format(5I8, 1X, 'nparsec=', I0, 1X, 'nequation=', I0)
        write (iunit, 9088) numtempfile, 1
9088    format(2I6, '   single   point   1   0   0')
        if (isvd == 0) then
            write (iunit, 9089)
9089        format('10.0   2.0    0.3    0.03    10  999')
        else
            write (iunit, 9090)
9090        format('10.0  -3.0    0.3    0.03     1  999')
        end if
        write (iunit, 9091)
9091    format('5.0   5.0   1.0e-3')
        if (auiyesno == 0) then
            write (iunit, 9092)
9092        format('0.1  noaui')
        else
            write (iunit, 9093)
9093        format('0.1   aui')
        end if
        write (iunit, 9094)
9094    format('30   .005  4   4  .005   4')
        write (iunit, 9095)
9095    format('1    1    1')
        if (isvd == 1) then
            write (iunit, 9096)
9096        format('* singular value decomposition')
            write (iunit, 9097)
9097        format('1')
            write (iunit, 9098) nnnpar, eigthresh
9098        format(i6, 2X, 1PG13.7)
            write (iunit, 9099)
9099        format('1')
        end if

! -- The "* parameter groups" section of the PEST control file is now
!    written.
        write (iunit, 9100)
9100    format('* parameter groups')
        do igp = 1, f_numpargp
            write (iunit, 9101) TRIM(f_pargpnme(igp)), TRIM(f_inctyp(igp)),         &
                              f_derinc(igp), f_derinclb(igp), TRIM(f_forcen(igp)),&
                              f_derincmul(igp), TRIM(f_dermthd(igp))
9101        format(a, t14, a, t27, 1PG12.5, t41, 1PG12.5, t55, a, t66, 1PG12.5,   &
                                                2X, a)
        end do

!    -- The "* parameter data" section of the PEST control file is now written.
        write (iunit, 9102)
9102    format('* parameter data')
        do ipar = 1, SIZE(f_parnme)
            if (f_partrans(ipar) (1:4) == 'tied') then
                atrans = 'tied'
            else
                atrans = f_partrans(ipar)
            end if
            write (iunit, 9103) TRIM(f_parnme(ipar)), TRIM(atrans),                 &
                              TRIM(f_parchglim(ipar)), f_parval1(ipar),           &
                              f_parlbnd(ipar), f_parubnd(ipar),                   &
                              TRIM(f_pargp(ipar)), f_scale(ipar), f_offset(ipar)
9103        format(a, t14, a, t21, a, t33, 1PG12.5, t47, 1PG12.5, t61, 1PG12.5,   &
                                                t75, a, t89, 1PG12.5, t103, 1PG12.5, t117, '  1')
        end do
        do ipar = 1, f_nequation
            write (iunit, 9900) TRIM(f_parsecnme(ipar)), TRIM(f_equation(ipar))
9900        format(a, t14, ' = ', a)
        end do
        do ipar = 1, SIZE(f_parnme)
            if (f_partrans(ipar) (1:4) == 'tied') then
                write (iunit, 9104) TRIM(f_parnme(ipar)), TRIM(f_partrans(ipar) (6:))
9104            format(a, t14, a)
            end if
        end do

! -- The "* observation groups" section of the PEST control file is now
!    written.
        write (iunit, 9105)
9105    format('* observation groups')
        do i = 1, nobsgp
            write (iunit, 9106) TRIM(obgnme(i))
9106        format(a)
        end do

! -- The "* observation data" section of the PEST control file is now
!    written.
! -- First the time series observations are dealt with.
        write (iunit, 9107)
9107    format('* observation data')

        iout = 0
        ieqnerr = 0
        if (ioseries /= 0) then
            do i = 1, ioseries
                iout = iout + 1
                im = IOUTSERIES_G(i)
                do j = 1, ioseries
                    if (im == modseries(j)) goto 1320
                end do
                write (amessage, 9141)
                goto 2400
1320            io = obsseries(j)
                nsterm = SERIES_G(io)%nterm
                aname = SERIES_G(im)%NAME

                allocate (tempobsvals(nsterm))
                allocate (tempsimvals(nsterm))

                if (SERIES_G(io)%LISSINGLEPRECISION) then
                    tempobsvals = real(SERIES_G(io)%VAL, KIND=t_dbl)
                    tempsimvals = real(SERIES_G(im)%VAL, KIND=t_dbl)
                else
                    tempobsvals = SERIES_G(io)%DPVAL
                    tempsimvals = SERIES_G(im)%DPVAL
                end if

                tempmean = 0.
                m2 = 0.
                sse = 0.

                do lc = 1, nsterm
                    delta = tempobsvals(lc) - tempmean
                    delta2 = tempobsvals(lc) - tempsimvals(lc)
                    tempmean = tempmean + delta / real(lc, KIND=8)
                    sse = sse + delta2**2
                    m2 = m2 + delta * (tempobsvals(lc) - tempmean)
                end do

                obj_fun_value = SQRT(sse)

                dpcount = real(nsterm, KIND=8)
                dpsum = SUM(tempobsvals)
                dpmin = MINVAL(tempobsvals)
                dpmax = MAXVAL(tempobsvals)
                dpmean = tempmean
                dpvariance = m2 / (nsterm - 1)

                call MAKE_BASENAME(ierr, iout, nsterm, aname, basename)
                atemp = basename(iout)
                weightmin = MAX(sweightmin(j), 0.0)
                weightmax = MIN(sweightmax(j), 1.0E36)

!            -- The pertinent equation is parsed and prepared.

                eqntext = sequation(j)
                call PREPARE_EQN(ierr, nterm, sequation(j), io)
                if (ierr /= 0) then
                    ieqnerr = 1
                    goto 2400
                end if
                nnterm = nterm
                do iterm = 1, nterm
                    CTERM(iterm) = ATERM(iterm)
                end do
                do iterm = 1, nterm
                    QTERM(iterm) = RTERM(iterm)
                end do
                do j = 1, nsterm
                    nterm = nnterm
                    do iterm = 1, nterm
                        ATERM(iterm) = CTERM(iterm)
                    end do
                    do iterm = 1, nterm
                        RTERM(iterm) = QTERM(iterm)
                    end do
                    call NUM2CHAR(j, anum)
                    aname = TRIM(atemp)//TRIM(anum)

!             -- First the series numbers in the equation terms are
!                replaced by series values.
                    do iterm = 1, nterm
                        if (ATERM(iterm) (1:3) == '$~$') then
                            call CHAR2NUM(ierr, ATERM(iterm) (4:), isnum)
                            RTERM(iterm) = tempobsvals(j)
                            ATERM(iterm) = '~!~'
                        end if
                    end do

!             -- The weights equation intrinsic function evaluations is
!                carried out if necessary.
                    do iterm = 1, nterm
                        if (ATERM(iterm) (1:3) == '@_2') then
                            RTERM(iterm) = ABS(tempobsvals(j))
                            ATERM(iterm) = '~!~'

                        elseif (ATERM(iterm) (1:3) == '@_4') then ! min
                            RTERM(iterm) = dpmin
                            ATERM(iterm) = '~!~'

                        elseif (ATERM(iterm) (1:3) == '@_5') then ! max
                            RTERM(iterm) = dpmax
                            ATERM(iterm) = '~!~'

                        elseif (ATERM(iterm) (1:3) == '@_6') then ! obj fun value
                            RTERM(iterm) = obj_fun_value
                            ATERM(iterm) = '~!~'

                        elseif (ATERM(iterm) (1:3) == '@_7') then ! count
                            RTERM(iterm) = dpcount
                            ATERM(iterm) = '~!~'

                        elseif (ATERM(iterm) (1:3) == '@_8') then ! mean
                            RTERM(iterm) = dpmean
                            ATERM(iterm) = '~!~'

                        elseif (ATERM(iterm) (1:3) == '@_9') then ! variance
                            RTERM(iterm) = dpvariance
                            ATERM(iterm) = '~!~'

                        elseif (ATERM(iterm) (1:3) == '@_1') then
                            call GREGORIAN_DATE(IJD=SERIES_G(io)%DAYS(j),       &
                                IMONTH=mm, IDAY=dd, IYEAR=yy)
                            nn = NUMDAYS(1, 1, yy, dd, mm, yy)
                            rtime = FLOAT(nn) + FLOAT(SERIES_G(io)%SECS(j))       &
                                    / 86400.0
                            RTERM(iterm) = rtime
                            ATERM(iterm) = '~!~'
                        elseif (ATERM(iterm) (1:3) == '@_3') then
                            call CHAR2NUM(ierr, ATERM(iterm) (5:), dtempx)
                            RTERM(iterm) = DBLE(SERIES_G(io)%DAYS(j))             &
                                           + DBLE(SERIES_G(io)%SECS(j))           &
                                           / 86400.0D0 - dtempx
                            ATERM(iterm) = '~!~'
                        end if
                    end do

                    call EVALUATE(ierr, maxterm, nterm, noper, nfunct, ATERM,     &
                                  bterm, operat, funct, iorder, dval, RTERM)
                    if (ierr /= 0) goto 2400
                    if (dval < weightmin) dval = weightmin
                    if (dval > weightmax) dval = weightmax
                    write (iunit, 9149) TRIM(aname), tempobsvals(j), dval,          &
                                      TRIM(SERIES_G(im)%NAME)
                end do

                if (ALLOCATED(tempobsvals)) deallocate (tempobsvals)
                if (ALLOCATED(tempsimvals)) deallocate (tempsimvals)

            end do
        end if

!    -- Now we handle S_TABLE observations.
        if (iostable /= 0) then
            siout = 0
            do i = 1, iostable
                siout = siout + 1
                im = IOUTSTABLE_G(i)
                do j = 1, iostable
                    if (im == modstable(j)) goto 1340
                end do
                write (amessage, 9142) 's', TRIM(STABLE_G(im)%NAME), 'S'
                goto 2400
1340            io = obsstable(j)
                aname = STABLE_G(im)%NAME
                sbasename(siout) = aname(1:12)
                weightmin = MAX(stweightmin(j), 0.0)
                weightmax = MIN(stweightmax(j), 1.0E36)
                eqntext = stequation(j)
                call PREPARE_EQN(ierr, nterm, stequation(j), 0)
                if (ierr /= 0) then
                    ieqnerr = 1
                    goto 2400
                end if
                nnterm = nterm
                do iterm = 1, nterm
                    CTERM(iterm) = ATERM(iterm)
                end do
                do iterm = 1, nterm
                    QTERM(iterm) = RTERM(iterm)
                end do

                if (STABLE_G(io)%MAXIMUM > -1.0E36) then
                    aname = TRIM(sbasename(siout))//obschar//'max'
                    nterm = nnterm
                    do iterm = 1, nterm
                        ATERM(iterm) = CTERM(iterm)
                    end do
                    do iterm = 1, nterm
                        RTERM(iterm) = QTERM(iterm)
                    end do
                    do iterm = 1, nterm
                        if (ATERM(iterm) (1:3) == '@_2') then
                            RTERM(iterm) = ABS(STABLE_G(io)%MAXIMUM)
                            ATERM(iterm) = '~!~'
                        end if
                    end do
                    call EVALUATE(ierr, maxterm, nterm, noper, nfunct, ATERM,     &
                                  bterm, operat, funct, iorder, dval, RTERM)
                    if (dval < weightmin) dval = weightmin
                    if (dval > weightmax) dval = weightmax
                    write (iunit, 9149) TRIM(aname), STABLE_G(io)%MAXIMUM, dval,    &
                                      TRIM(STABLE_G(im)%NAME)
                end if

                if (STABLE_G(io)%MINIMUM > -1.0E36) then
                    aname = TRIM(sbasename(siout))//obschar//'min'
                    nterm = nnterm
                    do iterm = 1, nterm
                        ATERM(iterm) = CTERM(iterm)
                    end do
                    do iterm = 1, nterm
                        RTERM(iterm) = QTERM(iterm)
                    end do
                    do iterm = 1, nterm
                        if (ATERM(iterm) (1:3) == '@_2') then
                            RTERM(iterm) = ABS(STABLE_G(io)%MINIMUM)
                            ATERM(iterm) = '~!~'
                        end if
                    end do
                    call EVALUATE(ierr, maxterm, nterm, noper, nfunct, ATERM,     &
                                  bterm, operat, funct, iorder, dval, RTERM)
                    if (dval < weightmin) dval = weightmin
                    if (dval > weightmax) dval = weightmax
                    write (iunit, 9149) TRIM(aname), STABLE_G(io)%MINIMUM, dval,    &
                                      TRIM(STABLE_G(im)%NAME)
                end if

                if (STABLE_G(io)%RANGE > -1.0E36) then
                    aname = TRIM(sbasename(siout))//obschar//'range'
                    nterm = nnterm
                    do iterm = 1, nterm
                        ATERM(iterm) = CTERM(iterm)
                    end do
                    do iterm = 1, nterm
                        RTERM(iterm) = QTERM(iterm)
                    end do
                    do iterm = 1, nterm
                        if (ATERM(iterm) (1:3) == '@_2') then
                            RTERM(iterm) = ABS(STABLE_G(io)%RANGE)
                            ATERM(iterm) = '~!~'
                        end if
                    end do
                    call EVALUATE(ierr, maxterm, nterm, noper, nfunct, ATERM,     &
                                  bterm, operat, funct, iorder, dval, RTERM)
                    if (dval < weightmin) dval = weightmin
                    if (dval > weightmax) dval = weightmax
                    write (iunit, 9149) TRIM(aname), STABLE_G(io)%RANGE, dval,      &
                                      TRIM(STABLE_G(im)%NAME)
                end if

                if (STABLE_G(io)%TOTAL > -1.0E36) then
                    aname = TRIM(sbasename(siout))//obschar//'sum'
                    nterm = nnterm
                    do iterm = 1, nterm
                        ATERM(iterm) = CTERM(iterm)
                    end do
                    do iterm = 1, nterm
                        RTERM(iterm) = QTERM(iterm)
                    end do
                    do iterm = 1, nterm
                        if (ATERM(iterm) (1:3) == '@_2') then
                            RTERM(iterm) = ABS(STABLE_G(io)%TOTAL)
                            ATERM(iterm) = '~!~'
                        end if
                    end do
                    call EVALUATE(ierr, maxterm, nterm, noper, nfunct, ATERM,     &
                                  bterm, operat, funct, iorder, dval, RTERM)
                    if (dval < weightmin) dval = weightmin
                    if (dval > weightmax) dval = weightmax
                    write (iunit, 9149) TRIM(aname), STABLE_G(io)%TOTAL, dval,      &
                                      TRIM(STABLE_G(im)%NAME)
                end if

                if (STABLE_G(io)%MEAN > -1.0E36) then
                    aname = TRIM(sbasename(siout))//obschar//'mean'
                    nterm = nnterm
                    do iterm = 1, nterm
                        ATERM(iterm) = CTERM(iterm)
                    end do
                    do iterm = 1, nterm
                        RTERM(iterm) = QTERM(iterm)
                    end do
                    do iterm = 1, nterm
                        if (ATERM(iterm) (1:3) == '@_2') then
                            RTERM(iterm) = ABS(STABLE_G(io)%MEAN)
                            ATERM(iterm) = '~!~'
                        end if
                    end do
                    call EVALUATE(ierr, maxterm, nterm, noper, nfunct, ATERM,     &
                                  bterm, operat, funct, iorder, dval, RTERM)
                    if (dval < weightmin) dval = weightmin
                    if (dval > weightmax) dval = weightmax
                    write (iunit, 9149) TRIM(aname), STABLE_G(io)%MEAN, dval,       &
                                      TRIM(STABLE_G(im)%NAME)
                end if

                if (STABLE_G(io)%MEDIAN > -1.0E36) then
                    aname = TRIM(sbasename(siout))//obschar//'median'
                    nterm = nnterm
                    do iterm = 1, nterm
                        ATERM(iterm) = CTERM(iterm)
                    end do
                    do iterm = 1, nterm
                        RTERM(iterm) = QTERM(iterm)
                    end do
                    do iterm = 1, nterm
                        if (ATERM(iterm) (1:3) == '@_2') then
                            RTERM(iterm) = ABS(STABLE_G(io)%MEDIAN)
                            ATERM(iterm) = '~!~'
                        end if
                    end do
                    call EVALUATE(ierr, maxterm, nterm, noper, nfunct, ATERM,     &
                                  bterm, operat, funct, iorder, dval, RTERM)
                    if (dval < weightmin) dval = weightmin
                    if (dval > weightmax) dval = weightmax
                    write (iunit, 9149) TRIM(aname), STABLE_G(io)%MEDIAN, dval,     &
                                      TRIM(STABLE_G(im)%NAME)
                end if

                if (STABLE_G(io)%STDDEV > -1.0E36) then
                    aname = TRIM(sbasename(siout))//obschar//'sd'
                    nterm = nnterm
                    do iterm = 1, nterm
                        ATERM(iterm) = CTERM(iterm)
                    end do
                    do iterm = 1, nterm
                        RTERM(iterm) = QTERM(iterm)
                    end do
                    do iterm = 1, nterm
                        if (ATERM(iterm) (1:3) == '@_2') then
                            RTERM(iterm) = ABS(STABLE_G(io)%STDDEV)
                            ATERM(iterm) = '~!~'
                        end if
                    end do
                    call EVALUATE(ierr, maxterm, nterm, noper, nfunct, ATERM,     &
                                  bterm, operat, funct, iorder, dval, RTERM)
                    if (dval < weightmin) dval = weightmin
                    if (dval > weightmax) dval = weightmax
                    write (iunit, 9149) TRIM(aname), STABLE_G(io)%STDDEV, dval,     &
                                      TRIM(STABLE_G(im)%NAME)
                end if

            end do
        end if

!    -- Next the V_TABLE observations are handled.
        if (iovtable /= 0) then
            do i = 1, iovtable
                iout = iout + 1
                im = IOUTVTABLE_G(i)
                do j = 1, iovtable
                    if (im == modvtable(j)) goto 1360
                end do
                write (amessage, 9142) 'v', TRIM(VTABLE_G(im)%NAME), 'V'
                goto 2400
1360            io = obsvtable(j)
                nsterm = VTABLE_G(io)%nterm
                aname = VTABLE_G(im)%NAME
                call MAKE_BASENAME(ierr, iout, nsterm, aname, basename)
                if (ierr /= 0) goto 2400
                atemp = basename(iout)
                weightmin = MAX(vtweightmin(j), 0.0)
                weightmax = MIN(vtweightmax(j), 1.0E36)
                eqntext = vtequation(j)
                call PREPARE_EQN(ierr, nterm, vtequation(j), 0)
                if (ierr /= 0) then
                    ieqnerr = 1
                    goto 2400
                end if
                nnterm = nterm
                do iterm = 1, nterm
                    CTERM(iterm) = ATERM(iterm)
                end do
                do iterm = 1, nterm
                    QTERM(iterm) = RTERM(iterm)
                end do
                do j = 1, nsterm
                    nterm = nnterm
                    do iterm = 1, nterm
                        ATERM(iterm) = CTERM(iterm)
                    end do
                    do iterm = 1, nterm
                        RTERM(iterm) = QTERM(iterm)
                    end do
                    call NUM2CHAR(j, anum)
                    aname = TRIM(atemp)//TRIM(anum)
                    do iterm = 1, nterm
                        if (ATERM(iterm) (1:3) == '@_2') then
                            RTERM(iterm) = ABS(VTABLE_G(io)%VOL(j))
                            ATERM(iterm) = '~!~'
                        end if
                    end do
                    call EVALUATE(ierr, maxterm, nterm, noper, nfunct, ATERM,     &
                                  bterm, operat, funct, iorder, dval, RTERM)
                    if (dval < weightmin) dval = weightmin
                    if (dval > weightmax) dval = weightmax
                    write (iunit, 9149) TRIM(aname), VTABLE_G(io)%VOL(j), dval,     &
                                      TRIM(VTABLE_G(im)%NAME)
                end do
            end do
        end if

!    -- Next the E_TABLE observations are handled.
        if (iodtable /= 0) then
            do i = 1, iodtable
                iout = iout + 1
                im = IOUTDTABLE_G(i)
                do j = 1, iodtable
                    if (im == moddtable(j)) goto 1380
                end do
                write (amessage, 9142) 'e', TRIM(VTABLE_G(im)%NAME), 'E'
                goto 2400
1380            io = obsdtable(j)
                totim = DTABLE_G(io)%TOTAL_TIME
                nsterm = DTABLE_G(io)%nterm
                aname = DTABLE_G(im)%NAME
                call MAKE_BASENAME(ierr, iout, nsterm, aname, basename)
                if (ierr /= 0) goto 2400
                atemp = basename(iout)
                weightmin = MAX(dtweightmin(j), 0.0) !chek
                weightmax = MIN(dtweightmax(j), 1.0E36)
                eqntext = dtequation(j)
                call PREPARE_EQN(ierr, nterm, dtequation(j), 0)
                if (ierr /= 0) then
                    ieqnerr = 1
                    goto 2400
                end if
                nnterm = nterm
                do iterm = 1, nterm
                    CTERM(iterm) = ATERM(iterm)
                end do
                do iterm = 1, nterm
                    QTERM(iterm) = RTERM(iterm)
                end do
                do j = 1, nsterm
                    nterm = nnterm
                    do iterm = 1, nterm
                        ATERM(iterm) = CTERM(iterm)
                    end do
                    do iterm = 1, nterm
                        RTERM(iterm) = QTERM(iterm)
                    end do
                    call NUM2CHAR(j, anum)
                    aname = TRIM(atemp)//TRIM(anum)
                    do iterm = 1, nterm
                        if (ATERM(iterm) (1:3) == '@_2') then
                            RTERM(iterm) = ABS(DTABLE_G(io)%TIME(j) / totim)
                            ATERM(iterm) = '~!~'
                        end if
                    end do
                    call EVALUATE(ierr, maxterm, nterm, noper, nfunct, ATERM,     &
                                  bterm, operat, funct, iorder, dval, RTERM)
                    if (dval < weightmin) dval = weightmin
                    if (dval > weightmax) dval = weightmax
                    write (iunit, 9149) TRIM(aname), DTABLE_G(io)%TIME(j) / totim,    &
                                      dval, TRIM(DTABLE_G(im)%NAME)
                end do
            end do
        end if

!    -- Next the G_TABLE observations are handled.
        if (iogtable /= 0) then
            do i = 1, iogtable
                iout = iout + 1
                im = IOUTGTABLE_G(i)
                do j = 1, iogtable
                    if (im == modgtable(j)) goto 1400
                end do
                write (amessage, 9142) 'g', TRIM(GTABLE_G(im)%NAME), 'G'
                goto 2400
1400            io = obsgtable(j)
                nsterm = UBOUND(GTABLE_G(io)%SDESCRIPTION, 1)
                aname = GTABLE_G(im)%NAME
                call MAKE_BASENAME(ierr, iout, nsterm, aname, basename)
                if (ierr /= 0) goto 2400
                atemp = basename(iout)
                weightmin = MAX(gtweightmin(j), 0.0) !chek
                weightmax = MIN(gtweightmax(j), 1.0E36)
                eqntext = gtequation(j)
                call PREPARE_EQN(ierr, nterm, gtequation(j), 0)
                if (ierr /= 0) then
                    ieqnerr = 1
                    goto 2400
                end if
                nnterm = nterm
                do iterm = 1, nterm
                    CTERM(iterm) = ATERM(iterm)
                end do
                do iterm = 1, nterm
                    QTERM(iterm) = RTERM(iterm)
                end do
                do j = 1, nsterm
                    nterm = nnterm
                    do iterm = 1, nterm
                        ATERM(iterm) = CTERM(iterm)
                    end do
                    do iterm = 1, nterm
                        RTERM(iterm) = QTERM(iterm)
                    end do
                    call NUM2CHAR(j, anum)
                    aname = TRIM(atemp)//TRIM(anum)
                    do iterm = 1, nterm
                        if (ATERM(iterm) (1:3) == '@_2') then
                            RTERM(iterm) = ABS(GTABLE_G(io)%RVALUE(j))
                            ATERM(iterm) = '~!~'
                        end if
                    end do
                    call EVALUATE(ierr, maxterm, nterm, noper, nfunct, ATERM,    &
                                  bterm, operat, funct, iorder, dval, RTERM)
                    if (dval < weightmin) dval = weightmin
                    if (dval > weightmax) dval = weightmax

                    write (iunit, 9149) TRIM(aname), GTABLE_G(io)%RVALUE(j), dval,  &
                                      TRIM(GTABLE_G(im)%NAME)
                end do
            end do
        end if

!    -- The "* model command line" section of the PEST control file is written.
        write (iunit, 9108)
9108    format('* model command line')
        if (modcomline == ' ') modcomline = 'model'
        write (iunit, 9109) TRIM(modcomline)
9109    format(a)

!    -- The "* model input/output" section of the PEST control file is written.
        write (iunit, 9110)
9110    format('* model input/output')
        do i = 1, numtempfile
            if (modfile(i) == ' ') then
                call NUM2CHAR(i, anum)
                modfile(i) = 'model'//TRIM(anum)//'.in'
            end if
            call ADDQUOTE(tempfile(i), bstring)
            call ADDQUOTE(modfile(i), cstring)
            write (iunit, 9150) TRIM(bstring), TRIM(cstring)
        end do
        call ADDQUOTE(instructfile, bstring)
        call ADDQUOTE(slistoutputfile_g, cstring)
        write (iunit, 9150) TRIM(bstring), TRIM(cstring)
        close (unit=iunit)

        write (*, 9151) TRIM(sstring_g)
        write (lu_rec, 9151) TRIM(sstring_g)

!    -- If a MICA control file was requested, it is now written.
        if (micactlfile /= ' ') then
            call ADDQUOTE(micactlfile, sstring_g)
            write (*, 9152) TRIM(sstring_g)
            write (lu_rec, 9152) TRIM(sstring_g)
            inquire (file=micactlfile, exist=lexist)
            if (lexist) then
                do
                    write (6, *)
                    write (*, 9138, advance='no') TRIM(sstring_g)
                    read (5, '(a)') aa
                    call CASETRANS(aa, 'lo')
                    if ((aa == 'y') .OR. (aa == 'n')) then
                        if (aa /= 'n') exit
                        goto 2500
                    end if
                end do
            end if
            itempunit = NEXTUNIT()
            open (unit=itempunit, file=micactlfile, status='old',                &
                  iostat=ierr)
            if (ierr == 0) close (unit=itempunit, status='delete')
            itempunit = NEXTUNIT()
            open (unit=itempunit, file='t###.###')
            call ADDQUOTE(pestctlfile, cstring)
            write (itempunit, '(a)') TRIM(cstring)
            write (itempunit, '(a)') '1'
            write (itempunit, '(a)') TRIM(sstring_g)
            close (unit=itempunit)
            call EXECUTE_COMMAND_LINE(TRIM(pest2micacom)//' < t###.### > nul')
            inquire (file=micactlfile, exist=lexist)
            if (.NOT. lexist) then
                write (amessage, 9111)
9111            format('could not write MICA control file - check PEST2MICA ',    &
                       'command.')
                goto 2400
            else
                write (*, 9151) TRIM(sstring_g)
                write (lu_rec, 9151) TRIM(sstring_g)
            end if
        end if

        goto 2500

1500    call NUM2CHAR(iline_g, aline)
        call ADDQUOTE(sinfile_g, sstring_g)
        write (amessage, 9112) TRIM(aline), TRIM(sstring_g)
9112    format('cannot read line ', a, ' of TSPROC input file ', a)
        goto 2400
1600    call ADDQUOTE(sinfile_g, sstring_g)
        write (amessage, 9113) TRIM(sstring_g), TRIM(currentblock_g)
9113    format('unexpected end encountered to TSPROC input file ', a, ' while ', &
               ' reading ', a, ' block.')
        goto 2400
1700    write (amessage, 9114)
9114    format('cannot allocate sufficient memory to continue execution.')
        goto 2400
1800    call NUM2CHAR(iline_g, aline)
        call ADDQUOTE(sinfile_g, sstring_g)
        write (amessage, 9115) TRIM(aoption), TRIM(aline), TRIM(sstring_g),      &
                             TRIM(correct_keyword)
9115    format(a, ' keyword at line ', a, ' of TSPROC input file ', a,           &
                          ' should immediately ', 'follow ', a, ' keyword.')
        goto 2400
1900    write (amessage, 9116) TRIM(sstring_g)
9116    format('cannot rewind file ', a)
        goto 2400
2000    call NUM2CHAR(jline, aline)
        write (amessage, 9117) TRIM(aline), TRIM(sstring_g)
9117    format('cannot read line ', a, ' of file ', a)
        goto 2400
2100    call NUM2CHAR(jline, aline)
        write (amessage, 9118) TRIM(aline), TRIM(sstring_g)
9118    format('insufficient entries on line ', a, ' of file ', a)
        goto 2400
2200    write (amessage, 9119) TRIM(sstring_g)
9119    format('cannot close file ', a)
        goto 2400
2300    write (amessage, 9120) TRIM(aoname), TRIM(amname), TRIM(avariable)
9120    format('OBSERVATION_S_TABLE "', a, '"  has been matched to ',            &
               'MODEL_S_TABLE "', a, '". However the ', a, ' has been computed ',&
               'for one and not for the other.')

2400    call WRITE_MESSAGE(LEADSPACE='yes', ERROR='yes')
        call WRITE_MESSAGE(iunit=lu_rec, LEADSPACE='yes')
        if (ieqnerr /= 0) then
            write (amessage, 9121)
9121        format(' Offending equation follows:-')
            call WRITE_MESSAGE()
            call WRITE_MESSAGE(iunit=lu_rec)
            do i = 1, LEN_TRIM(eqntext)
                if (eqntext(i:i) == CHAR(196)) eqntext(i:i) = '/'
            end do
            write (*, 9153) TRIM(eqntext)
            write (lu_rec, 9153) TRIM(eqntext)
        end if
        Ifail = 1
        if (iunit /= 0) close (unit=iunit, iostat=ierr)

2500    deallocate (f_pargpnme, f_inctyp, f_derinc, f_derinclb, f_forcen,        &
                              f_derincmul, f_dermthd, STAT=ierr)
        deallocate (f_parnme, f_partrans, f_parchglim, f_parval1, f_parlbnd,     &
                   f_parubnd, f_pargp, f_scale, f_offset, STAT=ierr)
9122    format(/, ' Processing ', a, ' block....')
9123    format(t5, a, ' ', a)
9124    format('current version of TSPROC does not allow C_TABLES to be ',       &
               'used for parameter estimation.')
9125    format('MODEL_', a, '_TABLE "', a, '" is not listed in the ',            &
               'LIST_OUTPUT block immediately preceding the ', a, ' block.')
9126    format(t5, 'Reading parameter group file ', a, ' ....')
9127    format('file ', a, ' appears to contain no data.')
9128    format('parameter group name "', a, '" greater than 12 characters ',     &
               'at line ', a, ' of file ', a)
9129    format('value for ', a, ' on line ', a, ' of file ', a,                  &
               ' must be positive.')
9130    format(t5, '- data for ', a, ' parameter groups read from file ', a)
9131    format(t5, 'Reading parameter data file ', a, ' ....')
9132    format('cannot read value for ', a, ' on line ', a, ' of file ', a)
9133    format(t5, '- data for ', a, ' parameters read from file ', a)
9134    format(t5, 'Reading template file ', a, ' ....')
9135    format(t5, '- ', a, ' parameter names read from file ', a)
9136    format(t5, '- ', a, ' more parameter names read from file ', a)
9137    format(t5, 'Writing instruction file ', a, ' ....')
9138    format(' File ', a, ' already exists. Overwrite it? [y/n]: ')
9139    format(/, ' Execution terminated so file not overwritten.')
9140    format('cannot open file ', a, ' for output.')
9141    format('time series "', a,                                               &
               '" cited in the LIST_OUTPUT block immediately ',                  &
               'preceding the WRITE_PEST_FILES block is not cited as a ',        &
               'MODEL_SERIES_NAME in the latter block.')
9142    format(a, '_table "', a, '" cited in the LIST_OUTPUT block immediately ',&
               'preceding the WRITE_PEST_FILES block is not cited as a ',        &
               'MODEL_', a, '_TABLE_NAME in the latter block.')
9143    format('l11', t6, a)
9144    format('l1', t6, a)
9145    format('l4', t6, a)
9146    format('l1', t6, a)
9147    format(t5, '- file ', a, ' written ok.')
9148    format(t5, 'Writing PEST control file ', a, ' ....')
9149    format(a, t22, 1PG14.7, t40, 1PG12.6, 2X, a)
9150    format(a, 3X, a)
9151    format(t5, '- file ', a, ' written ok.')
9152    format(t5, 'Writing MICA control file ', a, ' ....')
9153    format(' "', a, '"')

    end subroutine PEST_FILES

    subroutine WRITE_LIST_OUTPUT(Ifail)
!    -- Subroutine Write_List_Output writes TSPROC entities to an ASCII output
!       file.

        implicit none
!
! Dummy arguments
!
        integer :: Ifail
        intent(out) Ifail
!
! Local variables
!
        character(3) :: aaa
        character(25), dimension(maxcontext) :: acontext
        character(15) :: aline
        real :: totim
        character(itsnamelength) :: aname, atemp, sformat
        character(25) :: aoption
        integer :: dd, dds1, dds2, hhh, hhs1, hhs2, i, icontext, ictable, idtable,&
                   ierr, igtable, iseries, istable, iterm, ivtable, ixcon, j,     &
                   jctable, jdtable, jgtable, jstable, jvtable, mm, mmm, mms1,    &
                   mms2, nn, nnn, nns1, nns2, ss, sss, sss1, sss2, yy, yys1, yys2
        character(10) :: sdatestr

        Ifail = 0
        currentblock_g = 'LIST_OUTPUT'

        write (*, 9056)
        write (lu_rec, 9056)

        ixcon = 0
        icontext = 0
        ioutseries_g = 0 !iOutseries_g is an array
        iseries = 0
        istable = 0
        ictable = 0
        ivtable = 0
        idtable = 0
        igtable = 0
        soutfile_g = ' '
        sformat = ' '

!    -- Options for the LIST_OUTPUT block are first read.
        do
            iline_g = iline_g + 1
            read (lu_tsproc_control, '(a)', err=100, end=200) cline
            if (LEN_TRIM(cline) == 0) cycle
            if (cline(1:1) == '#') cycle
            call LINESPLIT(ierr, 2)
            if (ierr /= 0) then
                call NUM2CHAR(iline_g, aline)
                call ADDQUOTE(sinfile_g, sstring_g)
                write (amessage, 9001) TRIM(aline), TRIM(sstring_g)
9001            format('there should be 2 entries on line ', a, ' of file ', a)
                goto 300
            end if
            aoption = cline(LEFT_WORD(1):RIGHT_WORD(1))
            call CASETRANS(aoption, 'hi')
            if (aoption /= 'CONTEXT') then
                call TEST_CONTEXT(ierr, icontext, acontext)
                if (ierr == -1) then
                    call FIND_END(Ifail)
                    if (Ifail == 1) goto 300
                    return
                elseif (ierr == 1) then
                    goto 300
                end if
                ixcon = 1
            end if

            if (aoption == 'FILE') then
                call GET_FILE_NAME(ierr, soutfile_g)
                if (ierr /= 0) goto 300

            elseif (aoption == 'CONTEXT') then
                if (ixcon /= 0) then
                    call NUM2CHAR(iline_g, aline)
                    call ADDQUOTE(sinfile_g, sstring_g)
                    write (amessage, 9002) TRIM(aline), TRIM(sstring_g)
9002                format('CONTEXT keyword in incorrect location at line ', a,  &
                           ' of file ', a)
                    goto 300
                end if
                call GET_CONTEXT(ierr, icontext, acontext)
                if (ierr /= 0) goto 300

            elseif (aoption == 'SERIES_NAME') then
                iseries = iseries + 1
                if (iseries > maxseries) then
                    call NUM2CHAR(maxseries, aline)
                    write (amessage, 9003) TRIM(aline)
9003                format('a maximum of ', a,                                   &
                           ' series can be cited in a LIST_OUTPUT block.')
                    goto 300
                end if
                call GET_SERIES_NAME(ierr, ioutseries_g(iseries), 'SERIES_NAME')
                if (ierr /= 0) goto 300

            elseif (aoption == 'S_TABLE_NAME') then
                istable = istable + 1
                if (istable > maxstable) then
                    call NUM2CHAR(maxstable, aline)
                    write (amessage, 9004) TRIM(aline)
9004                format('a maximum of ', a,                                   &
                           ' s_tables can be cited in a LIST_OUTPUT block.')
                    goto 300
                end if
                call GET_TABLE_NAME(ierr, IOUTSTABLE_G(istable), 1)
                if (ierr /= 0) goto 300

            elseif (aoption == 'C_TABLE_NAME') then
                ictable = ictable + 1
                if (ictable > maxctable) then
                    call NUM2CHAR(maxctable, aline)
                    write (amessage, 9005) TRIM(aline)
9005                format('a maximum of ', a,                                   &
                           ' c_tables can be cited in a LIST_OUTPUT block.')
                    goto 300
                end if
                call GET_TABLE_NAME(ierr, IOUTCTABLE_G(ictable), 4)
                if (ierr /= 0) goto 300

            elseif (aoption == 'V_TABLE_NAME') then
                ivtable = ivtable + 1
                if (ivtable > maxvtable) then
                    call NUM2CHAR(maxvtable, aline)
                    write (amessage, 9006) TRIM(aline)
9006                format('a maximum of ', a,                                   &
                           ' v_tables can be cited in a LIST_OUTPUT block.')
                    goto 300
                end if
                call GET_TABLE_NAME(ierr, IOUTVTABLE_G(ivtable), 2)
                if (ierr /= 0) goto 300

            elseif (aoption == 'E_TABLE_NAME') then
                idtable = idtable + 1
                if (idtable > maxdtable) then
                    call NUM2CHAR(maxdtable, aline)
                    write (amessage, 9007) TRIM(aline)
9007                format('a maximum of ', a,                                   &
                           ' E_TABLES can be cited in a LIST_OUTPUT block.')
                    goto 300
                end if
                call GET_TABLE_NAME(ierr, IOUTDTABLE_G(idtable), 3)
                if (ierr /= 0) goto 300

            elseif (aoption == 'G_TABLE_NAME') then
                igtable = igtable + 1
                if (igtable > maxgtable) then
                    call NUM2CHAR(maxgtable, aline)
                    write (amessage, 9008) TRIM(aline)
9008                format('a maximum of ', a,                                   &
                           ' G_TABLES can be cited in a LIST_OUTPUT block.')
                    goto 300
                end if
                call GET_TABLE_NAME(ierr, IOUTGTABLE_G(igtable), ig_table)
                if (ierr /= 0) goto 300

            elseif (aoption == 'END') then
                exit

            elseif (aoption == 'SERIES_FORMAT') then
                call GETFILE(ierr, cline, sformat, LEFT_WORD(2), RIGHT_WORD(2))
                if (ierr /= 0) then
                    call NUM2CHAR(iline_g, aline)
                    call ADDQUOTE(sinfile_g, sstring_g)
                    write (amessage, 9009) TRIM(aline), TRIM(sstring_g)
9009                format('cannot read SERIES_FORMAT from line ', a,            &
                           ' of file ', a)
                    goto 300
                end if
                call CASETRANS(sformat, 'lo')
                if ((sformat /= 'short') .AND. (sformat /= 'long') .AND.         &
                   (sformat /= 'ssf')) then
                    call NUM2CHAR(iline_g, aline)
                    call ADDQUOTE(sinfile_g, sstring_g)
                    write (amessage, 9010) TRIM(aline), TRIM(sstring_g)
9010                format('SERIES_FORMAT must be "long", "short", or "ssf" ',   &
                           'at line ', a, ' of TSPROC input file ', a)
                    goto 300
                end if
                write (*, 9057) TRIM(sformat)
                write (lu_rec, 9057) TRIM(sformat)

            else
                call NUM2CHAR(iline_g, aline)
                call ADDQUOTE(sinfile_g, sstring_g)
                write (amessage, 9011) TRIM(aoption), TRIM(aline), TRIM(sstring_g)
9011            format('unexpected keyword - "', a,                              &
                       '" in LIST_OUTPUT block at line ', a, ' of file ', a)
                goto 300
            end if

        end do

!    -- The block has been read; now it is checked for correctness.
        if ((iseries == 0) .AND. (istable == 0) .AND. (ivtable == 0) .AND. (idtable == 0)  &
           .AND. (ictable == 0) .AND. (igtable == 0)) then
            write (amessage, 9012)
9012        format('no series or tables have been named for output in ',         &
                   'LIST_OUTPUT block.')
            goto 300
        end if
        if ((iseries /= 0) .AND. (sformat == ' ')) then
            write (amessage, 9013)
9013        format('if a time series is specified for output then the ',         &
                   'SERIES_FORMAT specifier must also be set in a ',             &
                   'LIST_OUTPUT block.')
            goto 300
        end if
        if (soutfile_g == ' ') then
            write (amessage, 9014)
9014        format('no FILE name provided in LIST_OUTPUT block.')
            goto 300
        end if
        if (icontext == 0) then
            write (amessage, 9015)
9015        format('no Context keyword(s) provided in LIST_OUTPUT block.')
            goto 300
        end if

!    -- All is well with the LIST_OUTPUT block so the output file is written.
        sseriesformat_g = sformat
        slistoutputfile_g = soutfile_g
        call ADDQUOTE(soutfile_g, sstring_g)
        write (*, 9058) TRIM(sstring_g)
        write (lu_rec, 9058) TRIM(sstring_g)
        lu_out = NEXTUNIT()
        open (unit=lu_out, file=soutfile_g, iostat=ierr)
        if (ierr /= 0) then
            write (amessage, 9016) TRIM(sstring_g)
9016        format('cannot open file ', a, ' for output.')
            goto 300
        end if

!    -- All of the requested time series are first written.
        imseries_g = iseries
        imdtable_g = idtable
        imvtable_g = ivtable
        imstable_g = istable
        imctable_g = ictable
        imgtable_g = igtable

        if (iseries /= 0) then
            do i = 1, iseries
                j = ioutseries_g(i)
                aname = SERIES_G(j)%NAME
                if (TRIM(sformat) /= 'ssf') then
                    write (lu_out, 9017) TRIM(SERIES_G(j)%NAME)
9017                format(/, ' TIME_SERIES "', a, '" ---->')
                end if
                if (SERIES_G(j)%type == 'ts') then
                    do iterm = 1, SERIES_G(j)%NTERM
                        if (sformat == 'long' .OR. sformat == 'ssf') then
                            nn = SERIES_G(j)%DAYS(iterm)
                            ss = SERIES_G(j)%SECS(iterm)
                            call GREGORIAN_DATE(IJD=nn, IMONTH=mm, IDAY=dd,      &
                                IYEAR=yy)
                            hhh = ss / 3600
                            mmm = (ss - hhh * 3600) / 60
                            sss = ss - hhh * 3600 - mmm * 60

                            if (datespec == 1) then
                                write (sdatestr, fmt="(i2.2,'/',i2.2,'/',i4.4)") &
                                      dd, mm, yy
                            else
                                write (sdatestr, fmt="(i2.2,'/',i2.2,'/',i4.4)") &
                                      mm, dd, yy
                            end if

                            if (SERIES_G(j)%LISSINGLEPRECISION) then
                                write (lu_out, fmt=                              &
                           "(1x,a,t20,a10,3x,i2.2,':',i2.2,':',    i2.2,3x,g16.9)"&
                           ) TRIM(aname), sdatestr, hhh, mmm, sss, SERIES_G(j)   &
                            %VAL(iterm)
                            else
                                write (lu_out, fmt=                              &
                          "(1x,a,t20,a10,3x,i2.2,':',i2.2,':',    i2.2,3x,g18.13)"&
                          ) TRIM(aname), sdatestr, hhh, mmm, sss, SERIES_G(j)    &
                           %DPVAL(iterm)
                            end if

                        elseif (SERIES_G(j)%LISSINGLEPRECISION) then
                            write (lu_out, fmt="(4x,g16.9)") SERIES_G(j)         &
                                  %VAL(iterm)
                        else
                            write (lu_out, fmt="(4x,g18.13)") SERIES_G(j)        &
                                  %DPVAL(iterm)

                        end if

                    end do
                end if
            end do
        end if

!    -- If any S_TABLEs were requested, they are now written.
        if (istable /= 0) then
            do i = 1, istable
                jstable = IOUTSTABLE_G(i)
                write (lu_out, 9018) TRIM(STABLE_G(jstable)%NAME)
9018            format(/, ' S_TABLE "', a, '" ---->')
                write (lu_out, 9019) TRIM(STABLE_G(jstable)%SERIES_NAME)
9019            format(t5, 'Series for which data calculated:', t55, '"', a, '"')
                nnn = STABLE_G(jstable)%REC_BEGDAYS
                sss = STABLE_G(jstable)%REC_BEGSECS
!            call newdate(nnn,1,1,1970,dds1,mms1,yys1)
                call GREGORIAN_DATE(IJD=nnn, IMONTH=mms1, IDAY=dds1, IYEAR=yys1)

                hhs1 = sss / 3600
                nns1 = (sss - hhs1 * 3600) / 60
                sss1 = sss - hhs1 * 3600 - nns1 * 60
                nnn = STABLE_G(jstable)%REC_ENDDAYS
                sss = STABLE_G(jstable)%REC_ENDSECS
!            call newdate(nnn,1,1,1970,dds2,mms2,yys2)
                call GREGORIAN_DATE(IJD=nnn, IMONTH=mms2, IDAY=dds2, IYEAR=yys2)

                hhs2 = sss / 3600
                nns2 = (sss - hhs2 * 3600) / 60
                sss2 = sss - hhs2 * 3600 - nns2 * 60
                if (datespec == 1) then
                    write (lu_out, 9059) dds1, mms1, yys1
                else
                    write (lu_out, 9059) mms1, dds1, yys1
                end if
                write (lu_out, 9020) hhs1, nns1, sss1
9020            format(t5, 'Starting time for data accumulation:', t55, i2.2, ':',&
                       i2.2, ':', i2.2)
                if (datespec == 1) then
                    write (lu_out, 9060) dds2, mms2, yys2
                else
                    write (lu_out, 9060) mms2, dds2, yys2
                end if
                write (lu_out, 9021) hhs2, nns2, sss2
9021            format(t5, 'Ending time for data accumulation:', t55, i2.2, ':', &
                       i2.2, ':', i2.2)
                call NUM2CHAR(STABLE_G(jstable)%REC_ICOUNT, aline)
                write (lu_out, 9061) TRIM(aline)
                if (STABLE_G(jstable)%REC_ITRANS == 0) then
                    aaa = 'no'
                else
                    aaa = 'yes'
                end if
                write (lu_out, 9022) TRIM(aaa)
9022            format(t5, 'Logarithmic transformation of series?', t55, a)
                if (STABLE_G(jstable)%REC_POWER == 0.0) then
                    aline = 'na'
                else
                    call NUM2CHAR(STABLE_G(jstable)%REC_POWER, aline)
                end if
                write (lu_out, 9023) TRIM(aline)
9023            format(t5, 'Exponent in power transformation:', t55, a)
                if (STABLE_G(jstable)%MAXIMUM > -1.0E35) then
                    write (lu_out, 9024) STABLE_G(jstable)%MAXIMUM
9024                format(t5, 'Maximum value:', t55, 1PG14.7)
                end if
                if (STABLE_G(jstable)%MINIMUM > -1.0E35) then
                    write (lu_out, 9025) STABLE_G(jstable)%MINIMUM
9025                format(t5, 'Minimum value:', t55, 1PG14.7)
                end if
                if (STABLE_G(jstable)%RANGE > -1.0E35) then
                    write (lu_out, 9026) STABLE_G(jstable)%RANGE
9026                format(t5, 'Range:', t55, 1PG14.7)
                end if
                if (STABLE_G(jstable)%TOTAL > -1.0E35) then
                    write (lu_out, 9027) STABLE_G(jstable)%TOTAL
9027                format(t5, 'Sum of values:', t55, 1PG14.7)
                end if
                if (STABLE_G(jstable)%MEAN > -1.0E35) then
                    write (lu_out, 9028) STABLE_G(jstable)%MEAN
9028                format(t5, 'Mean value:', t55, 1PG14.7)
                end if
                if (STABLE_G(jstable)%STDDEV > -1.0E35) then
                    write (lu_out, 9029) STABLE_G(jstable)%STDDEV
9029                format(t5, 'Standard deviation:', t55, 1PG14.7)
                end if
                if (STABLE_G(jstable)%MINMEAN > -1.0E35) then
                    call NUM2CHAR(STABLE_G(jstable)%AVETIME, atemp)
                    write (lu_out, 9030) TRIM(atemp), STABLE_G(jstable)%MINMEAN
9030                format(t5, 'Minimum ', a, '-sample mean', t55, 1PG14.7)
                end if
                if (STABLE_G(jstable)%MAXMEAN > -1.0E35) then
                    call NUM2CHAR(STABLE_G(jstable)%AVETIME, atemp)
                    write (lu_out, 9031) TRIM(atemp), STABLE_G(jstable)%MAXMEAN
9031                format(t5, 'Maximum ', a, '-sample mean', t55, 1PG14.7)
                end if
                if (STABLE_G(jstable)%MEDIAN > -1.0E35) then
                    write (lu_out, 9032) STABLE_G(jstable)%MEDIAN
9032                format(t5, 'Median value:', t55, 1PG14.7)
                end if
            end do
        end if

!    -- If any C_TABLEs were requested, they are now written.
        if (ictable /= 0) then
            do i = 1, ictable
                jctable = IOUTCTABLE_G(i)
                write (lu_out, 9033) TRIM(CTABLE_G(jctable)%NAME)
9033            format(/, ' C_TABLE "', a, '" ---->')
                write (lu_out, 9034) TRIM(CTABLE_G(jctable)%SERIES_NAME_OBS)
9034            format(t5, 'Observation time series name:', t55, '"', a, '"')
                write (lu_out, 9035) TRIM(CTABLE_G(jctable)%SERIES_NAME_SIM)
9035            format(t5, 'Simulation time series name:', t55, '"', a, '"')
                nnn = CTABLE_G(jctable)%REC_BEGDAYS
                sss = CTABLE_G(jctable)%REC_BEGSECS
                call GREGORIAN_DATE(IJD=nnn, IMONTH=mms1, IDAY=dds1,             &
                                    IYEAR=yys1)

                hhs1 = sss / 3600
                nns1 = (sss - hhs1 * 3600) / 60
                sss1 = sss - hhs1 * 3600 - nns1 * 60
                nnn = CTABLE_G(jctable)%REC_ENDDAYS
                sss = CTABLE_G(jctable)%REC_ENDSECS
                call GREGORIAN_DATE(IJD=nnn, IMONTH=mms2, IDAY=dds2,             &
                                    IYEAR=yys2)

                hhs2 = sss / 3600
                nns2 = (sss - hhs2 * 3600) / 60
                sss2 = sss - hhs2 * 3600 - nns2 * 60
                if (datespec == 1) then
                    write (lu_out, 9062) dds1, mms1, yys1
                else
                    write (lu_out, 9062) mms1, dds1, yys1
                end if
                write (lu_out, 9036) hhs1, nns1, sss1
9036            format(t5, 'Beginning time of series comparison:', t55, i2.2, ':',&
                       i2.2, ':', i2.2)
                if (datespec == 1) then
                    write (lu_out, 9063) dds2, mms2, yys2
                else
                    write (lu_out, 9063) mms2, dds2, yys2
                end if
                write (lu_out, 9037) hhs2, nns2, sss2
9037            format(t5, 'Finishing time of series comparison:', t55, i2.2, ':',&
                       i2.2, ':', i2.2)
                call NUM2CHAR(CTABLE_G(jctable)%REC_ICOUNT, aline)
                write (lu_out, 9061) TRIM(aline)
                if (CTABLE_G(jctable)%BIAS > -1.0E35) then
                    write (lu_out, 9038) CTABLE_G(jctable)%BIAS
9038                format(t5, 'Bias:', t55, 1PG14.7)
                end if
                if (CTABLE_G(jctable)%SE > -1.0E35) then
                    write (lu_out, 9039) CTABLE_G(jctable)%SE
9039                format(t5, 'Standard error:', t55, 1PG14.7)
                end if
                if (CTABLE_G(jctable)%RBIAS > -1.0E35) then
                    write (lu_out, 9040) CTABLE_G(jctable)%RBIAS
9040                format(t5, 'Relative bias:', t55, 1PG14.7)
                end if
                if (CTABLE_G(jctable)%RSE > -1.0E35) then
                    write (lu_out, 9041) CTABLE_G(jctable)%RSE
9041                format(t5, 'Relative standard error:', t55, 1PG14.7)
                end if
                if (CTABLE_G(jctable)%NS > -1.0E35) then
                    write (lu_out, 9042) CTABLE_G(jctable)%NS
9042                format(t5, 'Nash-Sutcliffe coefficient:', t55, 1PG14.7)
                end if
                if (CTABLE_G(jctable)%CE > -1.0E35) then
                    write (lu_out, 9043) CTABLE_G(jctable)%CE
9043                format(t5, 'Coefficient of efficiency:', t55, 1PG14.7)
                end if
                if (CTABLE_G(jctable)%KGE > -1.0E35) then
                    write (lu_out, 1321) CTABLE_G(jctable)%KGE
1321                format(t5, 'Kling-Gupta coefficient:', t55, 1PG14.7)
                end if
                if (CTABLE_G(jctable)%IA > -1.0E35) then
                    write (lu_out, 9044) CTABLE_G(jctable)%IA
9044                format(t5, 'Index of agreement:', t55, 1PG14.7)
                end if
                if (CTABLE_G(jctable)%VE > -1.0E35) then
                    write (lu_out, 9045) CTABLE_G(jctable)%VE
9045                format(t5, 'Volumetric efficiency:', t55, 1PG14.7)
                end if
            end do
        end if

!    -- If any V_TABLES were requested, they are now written.

        if (ivtable /= 0) then
            do i = 1, ivtable
                jvtable = IOUTVTABLE_G(i)
                write (lu_out, 9046) TRIM(VTABLE_G(jvtable)%NAME)
9046            format(/, ' V_TABLE "', a, '" ---->')
                write (lu_out, 9047) TRIM(VTABLE_G(jvtable)%SERIES_NAME)
9047            format(t5, 'Volumes calculated from series "', a,                &
                       '" are as follows:-')
                do j = 1, VTABLE_G(jvtable)%NTERM
                    call GREGORIAN_DATE(IJD=VTABLE_G(jvtable)%DAYS1(j),          &
                                        IMONTH=mms1, IDAY=dds1, IYEAR=yys1)

                    sss = VTABLE_G(jvtable)%SECS1(j)
                    hhs1 = sss / 3600
                    nns1 = (sss - hhs1 * 3600) / 60
                    sss1 = sss - hhs1 * 3600 - nns1 * 60
                    call GREGORIAN_DATE(IJD=VTABLE_G(jvtable)%DAYS2(j),          &
                                        IMONTH=mms2, IDAY=dds2, IYEAR=yys2)

                    sss = VTABLE_G(jvtable)%SECS2(j)
                    hhs2 = sss / 3600
                    nns2 = (sss - hhs2 * 3600) / 60
                    sss2 = sss - hhs2 * 3600 - nns2 * 60
                    if (datespec == 1) then
                        write (lu_out, 9064) dds1, mms1, yys1, hhs1, nns1, sss1, &
                                           dds2, mms2, yys2, hhs2, nns2, sss2,   &
                                           VTABLE_G(jvtable)%VOL(j)
                    else
                        write (lu_out, 9064) mms1, dds1, yys1, hhs1, nns1, sss1, &
                                           mms2, dds2, yys2, hhs2, nns2, sss2,   &
                                           VTABLE_G(jvtable)%VOL(j)
                    end if
                end do
            end do
        end if

!    -- If any E_TABLES were requested, they are now written.
        if (idtable /= 0) then
            do i = 1, idtable
                jdtable = IOUTDTABLE_G(i)
                totim = DTABLE_G(jdtable)%TOTAL_TIME
                write (lu_out, 9048) TRIM(DTABLE_G(jdtable)%NAME)
9048            format(/, ' E_TABLE "', a, '" ---->')
                if (DTABLE_G(jdtable)%UNDER_OVER == 1) then
                    write (lu_out, 9049) TRIM(DTABLE_G(jdtable)%TIME_UNITS),     &
                                       TRIM(DTABLE_G(jdtable)%TIME_UNITS)
9049                format(t4, 'Flow', t19, 'Time delay (', a, ')', t40,         &
                           'Time above (', a, ')', t60,                          &
                           'Fraction of time above threshold')
                else
                    write (lu_out, 9050) TRIM(DTABLE_G(jdtable)%TIME_UNITS),     &
                                       TRIM(DTABLE_G(jdtable)%TIME_UNITS)
9050                format(t4, 'Flow', t19, 'Time delay (', a, ')', t40,         &
                           'Time under (', a, ')', t60,                          &
                           'Fraction of time below threshold')
                end if
                do j = 1, DTABLE_G(jdtable)%NTERM
                    write (lu_out, 9051) DTABLE_G(jdtable)%FLOW(j),              &
                                       DTABLE_G(jdtable)%TDELAY(j),              &
                                       DTABLE_G(jdtable)%TIME(j),                &
                                       DTABLE_G(jdtable)%TIME(j) / totim
9051                format(t2, g14.7, t20, g14.7, t40, g14.7, t63, g14.7)
                end do
            end do
        end if

!    -- If any G_TABLES were requested, they are now written.
        if (igtable /= 0) then
            do i = 1, igtable
                jgtable = IOUTGTABLE_G(i)
                write (lu_out, 9052) TRIM(GTABLE_G(jgtable)%NAME)
9052            format(/, ' G_TABLE "', a, '" ---->')
                write (lu_out, 9053) GTABLE_G(jgtable)%G_TABLE_HEADER
9053            format(t4, a75, t90, 'Value')
                do j = 1, UBOUND(GTABLE_G(jgtable)%SDESCRIPTION, 1)
                    write (lu_out, fmt="(t4,a,t82,g14.7)") GTABLE_G(jgtable)     &
                          %SDESCRIPTION(j), GTABLE_G(jgtable)%RVALUE(j)
                end do
            end do
        end if

        write (*, 9065) TRIM(sstring_g)
        write (lu_rec, 9065) TRIM(sstring_g)
        FLUSH (UNIT=lu_out)
        close (unit=lu_out)

        return

100     call NUM2CHAR(iline_g, aline)
        call ADDQUOTE(sinfile_g, sstring_g)
        write (amessage, 9054) TRIM(aline), TRIM(sstring_g)
9054    format('cannot read line ', a, ' of TSPROC input file ', a)
        goto 300
200     call ADDQUOTE(sinfile_g, sstring_g)
        write (amessage, 9055) TRIM(sstring_g)
9055    format('unexpected end encountered to TSPROC input file ', a,            &
                          ' while reading LIST_OUTPUT block.')
300     call WRITE_MESSAGE(LEADSPACE='yes', ERROR='yes')
        call WRITE_MESSAGE(IUNIT=lu_rec, LEADSPACE='yes')
        Ifail = 1

        close (unit=lu_rec, iostat=ierr)
9056    format(/, ' Processing LIST_OUTPUT block....')
9057    format(t5, 'SERIES_FORMAT ', a)
9058    format(t5, 'Writing output file ', a, '....')
9059    format(t5, 'Starting date for data accumulation:', t55, i2.2, '/', i2.2, &
                          '/', i4)
9060    format(t5, 'Ending date for data accumulation:', t55, i2.2, '/', i2.2,   &
                          '/', i4)
9061    format(t5, 'Number of series terms in this interval:', t55, a)
9062    format(t5, 'Beginning date of series comparison:', t55, i2.2, '/', i2.2, &
                          '/', i4)
9063    format(t5, 'Finishing date of series comparison:', t55, i2.2, '/', i2.2, &
                          '/', i4)
9064    format(t5, 'From ', i2.2, '/', i2.2, '/', i4, ' ', i2.2, ':', i2.2, ':', &
                          i2.2, ' to ', i2.2, '/', i2.2, '/', i4, ' ', i2.2, ':', i2.2, ':',&
                          i2.2, '  volume = ', g18.12)
9065    format(t5, 'File ', a, ' written ok.')

    end subroutine WRITE_LIST_OUTPUT

end module TSP_OUTPUT
