!     Last change:  J     9 Sep 2004    5:38 pm
module tsp_data_structures

  use ISO_C_BINDING
  implicit none

  character (len=256) :: sVersionString = "version 1.0.4"

  integer, parameter    :: MAXSERIES=50000
  integer, parameter    :: MAXSERIESREAD=50000
  integer, parameter    :: MAXSTABLE=5000
  integer, parameter    :: MAXGTABLE=5000
  integer, parameter    :: MAXCTABLE=500
  integer, parameter    :: MAXCONTEXT=5
  integer, parameter    :: MAXVTABLE=5000
  integer, parameter    :: MAXDTABLE=5000
  integer, parameter    :: MAXTEMPDURFLOW=100         ! this used to be 50
  integer, parameter    :: MAXTEMPFILE=200
  integer, parameter    :: MAXPAR=8500
  character, parameter  :: OBSCHAR='_'
  integer, parameter    :: iTSNAMELENGTH = 18         ! this used to be limited to 10

  integer, parameter :: iS_TABLE = 1
  integer, parameter :: iV_TABLE = 2
  integer, parameter :: iE_TABLE = 3
  integer, parameter :: iC_TABLE = 4
  integer, parameter :: iG_TABLE = 5

  ! Define the sizes of base types used in the model
  integer(2), public, parameter :: T_LOGICAL = 4
  integer(2), public, parameter :: T_INT = 4
  integer(2), public, parameter :: T_BYTE = 1
  integer(2), public, parameter :: T_SHORT = 2
  integer(2), public, parameter :: T_REAL = SELECTED_REAL_KIND(p=6,r=37)

! Define machine-independent sizes for base types
  integer(2), public, parameter :: T_SGL = SELECTED_REAL_KIND(p=6,r=37)
!  integer(2), public, parameter :: T_SGL = SELECTED_REAL_KIND(p=13,r=307)
  integer(2), public, parameter :: T_DBL = SELECTED_REAL_KIND(p=13,r=200)
  integer(2), public, parameter :: T_CPLX_SGL = KIND((T_SGL, T_SGL))
  integer(2), public, parameter :: T_CPLX_DBL = KIND((T_DBL, T_DBL))

  ! Some useful typed constants (to ensure accurate computations)
  real (kind=T_SGL), public, parameter :: rZERO = 0.0_T_SGL
  real (kind=T_DBL), public, parameter :: rD_ZERO = 0.0_T_DBL
  real (kind=T_SGL), public, parameter :: rNEAR_ZERO = 1E-8_T_SGL
  logical (kind=T_LOGICAL), public, parameter :: lTRUE = .true._T_LOGICAL
  logical (kind=T_LOGICAL), public, parameter :: lFALSE = .false._T_LOGICAL
  real (kind=T_SGL), public, parameter :: rNODATA = -99999_T_SGL
  real (kind=T_SGL), public, parameter :: rHUGE = HUGE(rNODATA)
  real (kind=T_SGL), public, parameter :: rNEARHUGE = HUGE(rNODATA) - 100.
  real (kind=T_SGL), public, parameter :: rTINY = -HUGE(rNODATA)

  real (kind=C_DOUBLE), public, parameter :: cdNODATA = -99999_C_DOUBLE
  real (kind=C_DOUBLE), public, parameter :: cdHUGE = HUGE(cdNODATA)
  real (kind=C_DOUBLE), public, parameter :: cdNEARHUGE = HUGE(cdNODATA) - 100.


  type T_MONTH
    character (len=3) :: sAbbreviation
    character (len=12) :: sName
    integer :: iNumDays
  end type T_MONTH

  type T_USGS_NWIS_DAILY
    integer (kind=T_INT) :: iWaterYear
    integer (kind=T_INT) :: iYear
    integer (kind=T_INT) :: iMonth
    integer (kind=T_INT) :: iDay
    integer (kind=T_INT) :: iJulianDay
    real (kind=T_SGL) :: rMeanDischarge
    character (len=10) :: sDataFlag
  end type T_USGS_NWIS_DAILY

  type T_USGS_NWIS_GAGE
    character (len=256) :: sAgencyCode
    character (len=256) :: sSiteNumber
    character (len=256) :: sDescription
    type (T_USGS_NWIS_DAILY), dimension(:), pointer :: pGageData
  end type T_USGS_NWIS_GAGE

  type time_series
    logical active
    integer nterm
    character(2)type
    character (len=iTSNAMELENGTH) :: name
!    integer, dimension(:), pointer :: days
!    integer, dimension(:), pointer :: secs
!    real,    dimension(:), pointer :: val
    integer, dimension(:), allocatable :: days
    integer, dimension(:), allocatable :: secs
    logical :: lIsSinglePrecision = lTRUE
    real (kind=T_SGL), dimension(:), allocatable :: val
    real (kind=T_DBL), dimension(:), allocatable :: dpval
  end type time_series

  ! general-purpose table for arbitrary stats output
  type g_table
    logical :: active
    character (len=iTSNAMELENGTH) :: name
    character (len=iTSNAMELENGTH) :: series_name
    character (len=80) :: g_table_header
    integer  rec_begdays
    integer  rec_begsecs
    integer  rec_enddays
    integer  rec_endsecs

    character (len=80), dimension(:), allocatable  :: sDescription
    real (kind=T_DBL), dimension(:), allocatable   :: rValue
  end type g_table

  ! STATISTICS TABLE
  type s_table
    logical :: active
    character (len=iTSNAMELENGTH) :: name
    character (len=iTSNAMELENGTH) :: series_name
    real     maximum
    real     minimum
    real     range
    real     mean
    real     median
    real     stddev
    real     total
    real     minmean
    real     maxmean
    real     rec_power
    integer  rec_icount
    integer  rec_itrans
    integer  rec_begdays
    integer  rec_begsecs
    integer  rec_enddays
    integer  rec_endsecs
    integer  avetime
  end type s_table

  ! COMPARE SERIES table
  type c_table
    logical :: active
    character (len=iTSNAMELENGTH) :: name
    character (len=iTSNAMELENGTH) :: series_name_obs
    character (len=iTSNAMELENGTH) :: series_name_sim
    real     bias
    real     se
    real     rbias
    real     rse
    real     ns
    real     kge
    real     ce
    real     ia
    real     ve  ! volumetric efficiency
    integer  rec_icount
    integer  rec_begdays
    integer  rec_begsecs
    integer  rec_enddays
    integer  rec_endsecs
  end type c_table

  ! VOLUME table
  type v_table
    logical :: active
    character (len=iTSNAMELENGTH) :: name
    character (len=iTSNAMELENGTH) :: series_name
    integer nterm
    integer, dimension(:), pointer :: days1
    integer, dimension(:), pointer :: secs1
    integer, dimension(:), pointer :: days2
    integer, dimension(:), pointer :: secs2
    real (kind=T_DBL), dimension(:), pointer    :: vol
  end type v_table

  !FLOW-DURATION table (exceedance table)
  type d_table
    logical :: active
    character (len=iTSNAMELENGTH) :: name
    character (len=iTSNAMELENGTH) :: series_name
    character(7)time_units
    integer under_over
    integer nterm
    real total_time
    real, dimension(:), pointer     :: flow
    real, dimension(:), pointer     :: tdelay
    real, dimension(:), pointer     :: time
  end type d_table

  save series_g, tempseries_g
  type (time_series) tempseries_g
  type (time_series) series_g(MAXSERIES)
  type (s_table) stable_g(MAXSTABLE)
  type (g_table) gtable_g(MAXGTABLE)
  type (c_table) ctable_g(MAXCTABLE)
  type (v_table) vtable_g(MAXVTABLE)
  type (d_table) tempdtable_g
  type (d_table) dtable_g(MAXDTABLE)

  integer LU_TSPROC_CONTROL,LU_OUT
  integer NumProcBloc_g,ILine_g,IProcSetting_g
  character(25)Context_g
  character(25)sContextOverride_g
  character(40)CurrentBlock_g
  character(120)sInfile_g,sRecfile_g,sOutfile_g,sString_g
  integer :: tmpunit

! -- The following variables are global because they are used to exchange information
!    between the LIST_OUTPUT block and the WRITE_PEST_FILES block.

  integer iMseries_g
  integer iMstable_g
  integer iMctable_g
  integer iMvtable_g
  integer iMdtable_g
  integer iMgtable_g
  integer iOutseries_g(MAXSERIES),iOutStable_g(MAXSTABLE),iOutVtable_g(MAXVTABLE), &
          iOutDtable_g(MAXDTABLE),iOutCtable_g(MAXCTABLE), iOutGtable_g(MAXGTABLE)
  character (len=iTSNAMELENGTH) :: sSeriesFormat_g
  character(120)sListOutputFile_g

! -- Following are some parameter definitions related to equations.

! -- Maximum terms in any mathematical expression:-
  integer MAXTERM
  parameter(MAXTERM=200)
! -- Maximum number of function types:-
  integer NFUNCT
  parameter(NFUNCT=16)
! -- Maximum number of operators:-
  integer NOPER
  parameter(NOPER=7)
! -- Maximum number of series_g names in a series_g equation:-
  integer MAXEQNSER
  parameter (MAXEQNSER=25)

  integer iorder(MAXTERM)
  character(1)  operat(7)
  character(6)  funct(NFUNCT)
  character(28) aterm(MAXTERM),bterm(MAXTERM),cterm(MAXTERM)
  double precision rterm(MAXTERM), qterm(MAXTERM)
  data funct /'abs   ','acos  ','asin  ','atan  ','cos   ','cosh  ',  &
    'exp   ','log   ','log10 ','sin   ','sinh  ','sqrt  ','tan   ',   &
    'tanh  ','neg   ','pos   '/
  data operat /'^','/','*','-','+','(',')'/

!****************************************************************************
! defined types
!****************************************************************************

  type modelgrid
    integer                         :: nrow,ncol
    double precision                :: east_corner,north_corner,rotation
    real                            :: cosang,sinang
    real, dimension(:), pointer     :: delr,delc
    integer                         :: specunit,specline
    character (len=80)              :: specfile
  end type modelgrid

!****************************************************************************
!global variables
!****************************************************************************

!variables for reading a file ------->

  integer, parameter                :: NUM_WORD_DIM=100
  integer, dimension(NUM_WORD_DIM)        :: left_word,right_word
  character (len=400)               :: cline

!variables for writing a message ------->

  integer                 :: imessage=0
  character (len=500)     :: amessage= ' '
  character (len=200)     :: initial_message=' '
  ! from Jupiter API....
  CHARACTER(LEN=80)       :: ERRSUB  = ' ' ! Character string for error header

!escape variables ------->

  integer                 :: escset=0
  character (len=5)       :: eschar = 'E ~e '

!variables in bore data manipulation ------->

  integer                         :: num_bore_coord, num_bore_list
  character (len=120)             :: bore_coord_file, bore_list_file
  integer, dimension(:), pointer      :: bore_coord_layer
  double precision, dimension(:), pointer         :: bore_coord_east, &
                 bore_coord_north
  character (len=10), dimension(:), pointer       :: bore_coord_id, &
                                                           bore_list_id

!variables recording data settings ------->

  integer        :: datespec

! parameters defining valid program options

   integer, parameter :: iGET_WDM_SERIES               = 101
   integer, parameter :: iGET_SSF_SERIES               = 102
   integer, parameter :: iGET_PLT_SERIES               = 103
   integer, parameter :: iGET_MUL_SERIES_TETRAD        = 104
   integer, parameter :: iGET_MUL_SERIES_SSF           = 105
   integer, parameter :: iGET_UFORE_SERIES             = 106
   integer, parameter :: iGET_MUL_SERIES_GSFLOW_GAGE   = 107
   integer, parameter :: iGET_MUL_SERIES_STATVAR       = 108

   integer, parameter :: iWRITE_LIST_OUTPUT            = 201

   integer, parameter :: iERASE_ENTITY                 = 301
   integer, parameter :: iREDUCE_SPAN                  = 302
   integer, parameter :: iSERIES_STATISTICS            = 303
   integer, parameter :: iSERIES_COMPARE               = 304
   integer, parameter :: iNEW_TIME_BASE                = 305
   integer, parameter :: iVOLUME_CALCULATION           = 306
   integer, parameter :: iEXCEEDANCE_TIME              = 308
   integer, parameter :: iFLOW_DURATION                = 309
   integer, parameter :: iSERIES_EQUATION              = 310
   integer, parameter :: iSERIES_DISPLACE              = 311
   integer, parameter :: iSERIES_CLEAN                 = 312
   integer, parameter :: iDIGITAL_FILTER               = 313
   integer, parameter :: iSERIES_BASE_LEVEL            = 314
   integer, parameter :: iVOL_TABLE_TO_SERIES          = 315
   integer, parameter :: iMOVING_MINIMUM               = 316
   integer, parameter :: iNEW_SERIES_UNIFORM           = 317
   integer, parameter :: iSERIES_DIFFERENCE            = 318
   integer, parameter :: iPERIOD_STATISTICS            = 319
   integer, parameter :: iHYDRO_PEAKS                  = 320
   integer, parameter :: iUSGS_HYSEP                   = 321
   integer, parameter :: iHYDROLOGIC_INDICES           = 322
   integer, parameter :: iHYDRO_EVENTS                 = 323

   integer, parameter :: iWRITE_PEST_FILES             = 401

   integer, parameter :: iGET_SETTINGS                 = 1

   character(30)sCurrentBlockName
   integer (kind=T_INT) :: iBlockNumber

! global parameter for rec file output unit
   integer, public :: LU_REC
   integer, public :: LU_STD_OUT = 6

   type (T_MONTH),dimension(12) :: MONTH = (/ &
     T_MONTH('JAN', 'January', 31), &
     T_MONTH('FEB', 'February', 28), &
     T_MONTH('MAR', 'March', 31), &
     T_MONTH('APR', 'April', 30), &
     T_MONTH('MAY', 'May', 31), &
     T_MONTH('JUN', 'June', 30), &
     T_MONTH('JUL', 'July', 31), &
     T_MONTH('AUG', 'August', 31), &
     T_MONTH('SEP', 'September', 30), &
     T_MONTH('OCT', 'October', 31), &
     T_MONTH('NOV', 'November', 30), &
     T_MONTH('DEC', 'December', 31) &
     /)

end module tsp_data_structures
