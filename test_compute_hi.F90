program test_hi

  use ISO_C_BINDING
  implicit none

  type T_HI
    integer :: iUnits
    integer :: iTemporalAspect
    character (len=80)   :: sHydrologicIndex
    real   :: rValue
  end type T_HI

  integer, parameter :: rZERO = 0
  integer, parameter :: iDAILY = 0
  integer, parameter :: iMONTHLY = 1
  integer, parameter :: iANNUAL = 2

!void compute_hi(int datatype, float carea,
!		float m_lp, float m_up, int yr[150], float q[150][366])

  interface
    subroutine compute_hi(lUseMedian, rCarea, &
      rLowerPercentile, rUpperPercentile, iYr, rQ, &
      rMA, rLMA, rUMA, &
      rML, rLML, rUML, &
      rMH, rLMH, rUMH, &
      rFH, rLFH, rUFH, &
      rDL, rLDL, rUDL, &
      rDH, rLDH, rUDH, &
      rRA, rLRA, rURA) BIND(C)
      use ISO_C_BINDING
      logical(  C_BOOL), value :: lUseMedian
      real(C_FLOAT), value :: rCarea
      real(C_FLOAT), value :: rLowerPercentile
      real(C_FLOAT), value :: rUpperPercentile
      integer(C_INT), dimension(150) :: iYr
      real(C_FLOAT), dimension(0:365,0:149) :: rQ
      real(C_FLOAT), dimension(0:45) :: rMA
      real(C_FLOAT), dimension(0:45) :: rLMA
      real(C_FLOAT), dimension(0:45) :: rUMA
      real(C_FLOAT), dimension(0:22) :: rML
      real(C_FLOAT), dimension(0:22) :: rLML
      real(C_FLOAT), dimension(0:22) :: rUML
      real(C_FLOAT), dimension(0:28) :: rMH
      real(C_FLOAT), dimension(0:28) :: rLMH
      real(C_FLOAT), dimension(0:28) :: rUMH
      real(C_FLOAT), dimension(0:11) :: rFH
      real(C_FLOAT), dimension(0:11) :: rLFH
      real(C_FLOAT), dimension(0:11) :: rUFH
      real(C_FLOAT), dimension(0:20) :: rDL
      real(C_FLOAT), dimension(0:20) :: rLDL
      real(C_FLOAT), dimension(0:20) :: rUDL
      real(C_FLOAT), dimension(0:24) :: rDH
      real(C_FLOAT), dimension(0:24) :: rLDH
      real(C_FLOAT), dimension(0:24) :: rUDH

      real(C_FLOAT), dimension(0:3) :: rUTA
      real(C_FLOAT), dimension(0:3) :: rTA
      real(C_FLOAT), dimension(0:3) :: rLTA

      real(C_FLOAT), dimension(0:3) :: rUTL
      real(C_FLOAT), dimension(0:3) :: rTL
      real(C_FLOAT), dimension(0:3) :: rLTL

      real(C_FLOAT), dimension(0:3) :: rUTH
      real(C_FLOAT), dimension(0:3) :: rTH
      real(C_FLOAT), dimension(0:3) :: rLTH

      real(C_FLOAT), dimension(0:9) :: rRA
      real(C_FLOAT), dimension(0:9) :: rLRA
      real(C_FLOAT), dimension(0:9) :: rURA
    end subroutine compute_hi
  end interface

  integer(C_INT), dimension(0:149) :: iYr
  real(C_FLOAT), dimension(0:365,0:149) :: rQ
  real(C_FLOAT), dimension(0:45) :: rMA
  real(C_FLOAT), dimension(0:45) :: rLMA
  real(C_FLOAT), dimension(0:45) :: rUMA
  real(C_FLOAT), dimension(0:22) :: rML
  real(C_FLOAT), dimension(0:22) :: rLML
  real(C_FLOAT), dimension(0:22) :: rUML
  real(C_FLOAT), dimension(0:28) :: rMH
  real(C_FLOAT), dimension(0:28) :: rLMH
  real(C_FLOAT), dimension(0:28) :: rUMH
  real(C_FLOAT), dimension(0:11) :: rFH
  real(C_FLOAT), dimension(0:11) :: rLFH
  real(C_FLOAT), dimension(0:11) :: rUFH
  real(C_FLOAT), dimension(0:20) :: rDL
  real(C_FLOAT), dimension(0:20) :: rLDL
  real(C_FLOAT), dimension(0:20) :: rUDL
  real(C_FLOAT), dimension(0:24) :: rDH
  real(C_FLOAT), dimension(0:24) :: rLDH
  real(C_FLOAT), dimension(0:24) :: rUDH
  real(C_FLOAT), dimension(0:9) :: rRA
  real(C_FLOAT), dimension(0:9) :: rLRA
  real(C_FLOAT), dimension(0:9) :: rURA

  integer :: i

  type (T_HI), dimension(45) :: MA
  type (T_HI), dimension(22) :: ML
  type (T_HI), dimension(27) :: MH


   MA = [ &
     T_HI( 1,iDAILY,'Mean, all daily flows',rZERO), &
     T_HI( 1,iDAILY,'Median, all daily flows',rZERO), &
     T_HI( 6,iDAILY,'CV, all daily flows',rZERO), &
     T_HI( 6,iDAILY,'CV, log of all daily flows',rZERO), &
     T_HI( 6,iDAILY,'Mean daily flow / median daily flow',rZERO), &
     T_HI( 6,iDAILY,'Ratio, Q10 / Q90 for all daily flows',rZERO), &
     T_HI( 6,iDAILY,'Ratio, Q20 / Q80 for all daily flows',rZERO), &
     T_HI( 6,iDAILY,'Ratio, Q25 / Q75 for all daily flows',rZERO), &
     T_HI( 2,iDAILY,'(Q10 - Q90) / median daily flow',rZERO), &
     T_HI( 2,iDAILY,'(Q20 - Q80) / median daily flow',rZERO), &
     T_HI( 2,iDAILY,'(Q25 - Q75) / median daily flow',rZERO), &
     T_HI( 2,iMONTHLY,'Mean monthly flow, January',rZERO), &
     T_HI( 2,iMONTHLY,'Mean monthly flow, February',rZERO), &
     T_HI( 2,iMONTHLY,'Mean monthly flow, March',rZERO), &
     T_HI( 2,iMONTHLY,'Mean monthly flow, April',rZERO), &
     T_HI( 2,iMONTHLY,'Mean monthly flow, May',rZERO), &
     T_HI( 2,iMONTHLY,'Mean monthly flow, June',rZERO), &
     T_HI( 2,iMONTHLY,'Mean monthly flow, July',rZERO), &
     T_HI( 2,iMONTHLY,'Mean monthly flow, August',rZERO), &
     T_HI( 2,iMONTHLY,'Mean monthly flow, September',rZERO), &
     T_HI( 2,iMONTHLY,'Mean monthly flow, October',rZERO), &
     T_HI( 2,iMONTHLY,'Mean monthly flow, November',rZERO), &
     T_HI( 2,iMONTHLY,'Mean monthly flow, December',rZERO), &
     T_HI( 2,iMONTHLY,'CV of monthly flow, January',rZERO), &
     T_HI( 2,iMONTHLY,'CV of monthly flow, February',rZERO), &
     T_HI( 2,iMONTHLY,'CV of monthly flow, March',rZERO), &
     T_HI( 2,iMONTHLY,'CV of monthly flow, April',rZERO), &
     T_HI( 2,iMONTHLY,'CV of monthly flow, May',rZERO), &
     T_HI( 2,iMONTHLY,'CV of monthly flow, June',rZERO), &
     T_HI( 2,iMONTHLY,'CV of monthly flow, July',rZERO), &
     T_HI( 2,iMONTHLY,'CV of monthly flow, August',rZERO), &
     T_HI( 2,iMONTHLY,'CV of monthly flow, September',rZERO), &
     T_HI( 2,iMONTHLY,'CV of monthly flow, October',rZERO), &
     T_HI( 2,iMONTHLY,'CV of monthly flow, November',rZERO), &
     T_HI( 2,iMONTHLY,'CV of monthly flow, December',rZERO), &
     T_HI( 2,iMONTHLY,'Range mean monthly / median monthly flow',rZERO), &
     T_HI( 2,iMONTHLY,'IQR mean monthly / median monthly flow',rZERO), &
     T_HI( 2,iMONTHLY,'(Q10 - Q90)[monthly] / median monthly flow',rZERO), &
     T_HI( 2,iMONTHLY,'CV, monthly mean flows',rZERO), &
     T_HI( 2,iMONTHLY,'Skewness in monthly flows',rZERO), &
     T_HI( 2,iANNUAL,'Mean annual runoff',rZERO), &
     T_HI( 2,iANNUAL,'Range mean annual / median annual flow',rZERO), &
     T_HI( 2,iANNUAL,'IQR mean annual / median annual flow',rZERO), &
     T_HI( 2,iANNUAL,'(Q10 - Q90)[annual] / median annual flow',rZERO), &
     T_HI( 2,iANNUAL,'Skewness in annual flows',rZERO) &
     ]

     ML = [ &
     T_HI( 1,iMONTHLY,'Mean minimum monthly flow, January',rZERO), &
     T_HI( 1,iMONTHLY,'Mean minimum monthly flow, February',rZERO), &
     T_HI( 1,iMONTHLY,'Mean minimum monthly flow, March',rZERO), &
     T_HI( 1,iMONTHLY,'Mean minimum monthly flow, April',rZERO), &
     T_HI( 1,iMONTHLY,'Mean minimum monthly flow, May',rZERO), &
     T_HI( 1,iMONTHLY,'Mean minimum monthly flow, June',rZERO), &
     T_HI( 1,iMONTHLY,'Mean minimum monthly flow, July',rZERO), &
     T_HI( 1,iMONTHLY,'Mean minimum monthly flow, August',rZERO), &
     T_HI( 1,iMONTHLY,'Mean minimum monthly flow, September',rZERO), &
     T_HI( 1,iMONTHLY,'Mean minimum monthly flow, October',rZERO), &
     T_HI( 1,iMONTHLY,'Mean minimum monthly flow, November',rZERO), &
     T_HI( 1,iMONTHLY,'Mean minimum monthly flow, December',rZERO), &
     T_HI( 6,iMONTHLY,'CV of minimum monthly flows',rZERO), &
     T_HI( 6,iMONTHLY,'Mean minimum daily flow / mean median annual flow',rZERO), &
     T_HI( 6,iANNUAL,'Mean minimum annual flow / mean annual flow',rZERO), &
     T_HI( 6,iANNUAL,'Median minimum annual flow / median annual flow',rZERO), &
     T_HI( 6,iANNUAL,'7-day minimum flow / mean annual flow',rZERO), &
     T_HI( 6,iANNUAL,'CV of ( mean minimum annual flow / mean annual flow )',rZERO), &
     T_HI( 6,iANNUAL,'Mean of (minimum annual flow / mean annual flow ) * 100',rZERO), &
     T_HI( 6,iANNUAL,'Ratio of baseflow volume to total flow volume',rZERO), &
     T_HI( 6,iANNUAL,'CV of annual minimum flows',rZERO), &
     T_HI( 6,iANNUAL,'Mean annual minimum flow divided by catchment area',rZERO) &
     ]

     MH = [ &
     T_HI( 1,iMONTHLY,'Mean maximum monthly flow, January',rZERO), &
     T_HI( 1,iMONTHLY,'Mean maximum monthly flow, February',rZERO), &
     T_HI( 1,iMONTHLY,'Mean maximum monthly flow, March',rZERO), &
     T_HI( 1,iMONTHLY,'Mean maximum monthly flow, April',rZERO), &
     T_HI( 1,iMONTHLY,'Mean maximum monthly flow, May',rZERO), &
     T_HI( 1,iMONTHLY,'Mean maximum monthly flow, June',rZERO), &
     T_HI( 1,iMONTHLY,'Mean maximum monthly flow, July',rZERO), &
     T_HI( 1,iMONTHLY,'Mean maximum monthly flow, August',rZERO), &
     T_HI( 1,iMONTHLY,'Mean maximum monthly flow, September',rZERO), &
     T_HI( 1,iMONTHLY,'Mean maximum monthly flow, October',rZERO), &
     T_HI( 1,iMONTHLY,'Mean maximum monthly flow, November',rZERO), &
     T_HI( 1,iMONTHLY,'Mean maximum monthly flow, December',rZERO), &
     T_HI( 6,iMONTHLY,'CV of maximum monthly flows',rZERO), &
     T_HI( 6,iANNUAL,'Median maximum annual flow / median annual flow',rZERO), &
     T_HI( 6,iANNUAL,'Mean of Q1 values / median daily flow across all years',rZERO), &
     T_HI( 6,iANNUAL,'Mean of Q10 values / median daily flow across all years',rZERO), &
     T_HI( 6,iANNUAL,'Mean of Q25 values / median daily flow across all years',rZERO), &
     T_HI( 6,iANNUAL,'CV of logarithmic annual maximum flows',rZERO), &
     T_HI( 6,iANNUAL,'Skewness in annual maximum flows',rZERO), &
     T_HI( 6,iANNUAL,'Mean annual maximum flow / catchment area',rZERO), &
     T_HI( 6,iANNUAL,'High-flow volume (thresh = 1 * median annual flow)',rZERO), &
     T_HI( 6,iANNUAL,'High-flow volume (thresh= 3 * median annual flow)',rZERO), &
     T_HI( 6,iANNUAL,'High-flow volume (thresh = 7 * median annual flow)',rZERO), &
     T_HI( 6,iANNUAL,'Maximum peak flow / median flow (thresh = 1 * median annual flow)',rZERO), &
     T_HI( 6,iANNUAL,'Maximum peak flow / median flow (thresh = 3 * median annual flow)',rZERO), &
     T_HI( 6,iANNUAL,'Maximum peak flow / median flow (thresh = 7 * median annual flow)',rZERO), &
     T_HI( 6,iANNUAL,'Maximum peak flow / median flow (threshold = Q25)',rZERO) &
     ]

  ! to work with the C++ code, non-leap years must have a -9999 (or missing value code)
  ! inserted in the space reserved for February 29.
  !
  ! data must be arranged by water year as well. (i.e. day 0 = 10/1/YYYY)

  rQ = 999999.

  rQ(:,0) = [27,25,25,25,24,23,22,24,24,23,25,23,23,23,24,28,24,23,&
    22,25,25,24,24,25,25,24,24,23,23,24,24,32,46,41,34,32,29,27,26, &
    27,26,25,30,29,27,25,24,23,23,23,59,128,75,81,60,53,51,44,40,38, &
    37,36,35,33,32,31,31,31,31,31,31,31,31,31,30,44,79,57,47,42,36,&
    34,33,32,31,31,31,30,30,35,43,42,33,31,33,61,36,33,32,31,30,29,&
    29,30,30,29,29,28,28,27,27,27,33,40,42,38,30,28,28,28,26,26,29,&
    32,31,32,41,48,46,31,29,28,29,27,27,27,26,25,25,23,24,24,24,25,&
    25,25,25,25,24,24,25,-9999,25,26,30,34,41,58,91,122,90,60,40,32,30,27,&
    26,121,83,39,34,28,30,34,65,151,225,239,147,158,173,113,208,119,&
    79,70,70,68,64,66,102,81,67,68,64,59,59,171,154,101,84,106,196,&
    115,89,78,74,68,65,64,68,64,61,60,75,77,77,68,63,61,75,65,60,59,&
    58,56,56,55,53,53,55,54,54,52,52,54,56,51,50,50,49,47,55,54,50,49,&
    51,49,49,46,141,137,77,64,57,54,52,77,60,56,65,100,72,68,61,57,54,&
    54,67,56,53,51,50,61,55,54,52,51,228,733,165,161,511,194,173,128,&
    113,111,96,88,107,107,91,81,76,73,71,70,201,102,87,137,83,75,72,&
    70,68,66,64,64,66,63,61,66,65,61,59,58,57,224,136,83,73,71,67,64,&
    62,79,80,64,61,59,57,59,75,68,60,58,57,56,57,57,55,55,54,53,53,&
    55,64,129,91,75,67,62,59,58,58,58,56,54,58,73,62,58,55,54]


  rQ(:,1) = [54,52,52,51,51,51,51,51,58,52,51,51,50,50,51,51,51,50,50,50,53,51,51,50,50,49,49,50,49,48,48,48,48,49,50,50,49, &
   48,48,47,47,47,47,53,50,53,50,49,48,49,47,47,46,46,46,50,63,55,51,49,47,47,51,50,49,49,50,47,47,47,47,46,46,45, &
   46,46,45,46,47,46,45,44,43,43,42,42,40,40,39,39,38,39,40,39,39,39,39,39,39,39,39,39,39,39,39,38,37,37,37,37,37, &
   37,37,37,38,38,38,38,39,39,39,38,38,38,38,38,38,38,38,37,37,36,37,37,37,38,39,39,39,40,42,189,460,105,70,58,54,51, &
   49,47,47,-9999,46,45,46,52,124,165,98,68,58,54,51,52,52,52,54,50,48,46,44,48,57,53,51,49,47,46,47,46,44,43,42,43,45,43, &
   43,42,40,39,39,39,38,38,44,47,43,47,43,40,39,38,37,36,36,35,35,40,40,37,36,36,39,45,43,40,38,38,37,40,38,37,37, &
   38,37,36,38,39,36,35,34,35,35,33,32,33,35,35,34,32,32,31,31,31,31,30,30,30,31,31,31,30,30,30,29,28,30,30,28,28, &
   27,27,27,29,29,29,46,62,43,69,48,46,44,42,39,39,38,200,60,39,37,45,38,35,33,32,32,36,34,33,35,34,34,44,40,39,39, &
   37,38,37,37,37,37,37,37,38,38,42,46,39,39,37,37,36,45,80,51,47,42,39,38,37,39,39,39,38,37,37,37,37,38,38,37,37, &
   38,39,38,38,39,39,39,39,38,38,39,44,41,39,40,60,48,56,45,42,39,38,37,36,46,44,48,58,54,47,42,40]

  rQ(:,2) = [38,37,36,33,33,33,34,34,32,31,30,30,30,30,30,30,31,32,31,31,31,32,32,30,31,31,30,30,30,30,30,31,30,31,33,35,43,&
   37,36,37,35,34,34,35,39,35,32,32,32,31,33,43,39,37,36,36,33,54,69,49,43,40,39,38,38,39,40,40,38,37,35,34,33,32,&
   31,32,33,34,34,33,33,33,34,34,35,35,34,35,38,37,36,35,34,33,32,31,30,30,30,30,30,29,30,33,33,34,34,33,34,34,34,&
   33,33,33,33,31,30,29,30,31,32,32,32,33,33,32,32,30,30,29,29,30,30,29,29,29,29,30,29,29,30,39,55,43,37,37,35,33,&
   32,31,32,-9999,31,29,29,28,29,29,29,28,28,30,77,58,49,43,41,39,37,36,37,53,54,45,42,38,36,36,58,58,52,46,43,41,40,38,&
   37,35,36,37,52,51,47,68,79,59,50,45,43,43,65,60,50,54,50,44,43,42,43,72,56,49,47,43,42,41,40,39,38,37,44,54,71,&
   58,48,48,47,41,39,38,36,35,34,33,33,36,36,36,35,56,144,69,53,47,45,45,44,42,42,44,45,47,44,44,43,42,41,40,40,39,&
   39,38,38,36,35,36,35,35,35,37,36,36,34,34,32,32,32,35,54,42,37,35,35,34,34,34,35,35,37,42,35,34,34,35,34,33,34,&
   36,37,38,37,40,37,36,40,43,40,37,36,34,34,42,47,45,40,37,36,35,34,33,50,104,55,86,63,46,40,38,35,34,32,31,36,48,&
   39,35,34,33,33,32,30,31,32,32,31,30,30,29,30,29,29,29,30,29,34,35,32,31,29,29,29,29,29,28,30,30]


  iYr = 0
  iYr(0) = 1993
  iYr(1) = 1994
  iYr(2) = 1995

  ! make the actual call to the C++ routine provided by
  call compute_hi(lUseMedian=.FALSE._C_BOOL, rCarea=1024., &
      rLowerPercentile=25., rUpperPercentile=75., iYr=iYr, rQ=rQ, rMA=rMA, &
      rLMA = rLMA, rUMA = rUMA, &
      rML = rML, rLML = rLML, rUML = rUML, &
      rMH = rMH, rLMH = rLMH, rUMH = rUMH, &
      rFH = rFH, rLFH = rLFH, rUFH = rUFH, &
      rDL = rDL, rLDL = rLDL, rUDL = rUDL, &
      rDH = rDH, rLDH = rLDH, rUDH = rUDH, &
      rRA = rRA, rLRA = rLRA, rURA = rURA )

  print *, "Done!!"

  MA(1:45)%rValue = rMA(1:45)
  ML(1:22)%rValue = rML(1:22)
  MH(1:27)%rValue = rMH(1:27)

  do i=1,45
    write(*,fmt="('MA(',i2') ',a,t65,3(f12.3,2x))") i,trim(MA(i)%sHydrologicIndex), &
      rLMA(i), MA(i)%rValue, rUMA(i)
  enddo

  do i=1,22
    write(*,fmt="('ML(',i2') ',a,t65,3(f12.3,2x))") i,trim(ML(i)%sHydrologicIndex), &
      rLML(i), rML(i), rUML(i)
  enddo

  do i=1,27
    write(*,fmt="('MH(',i2') ',a,t65,3(f12.3,2x))") i,trim(MH(i)%sHydrologicIndex), &
      rLMH(i), rMH(i), rUMH(i)
  enddo

  do i=1,11
    write(*,fmt="('FH(',i2') ',a,t50,3(f14.3,2x))") i,"  ", &
      rLFH(i), rFH(i), rUFH(i)
  enddo

  do i=1,20
    write(*,fmt="('DL(',i2') ',a,t50,3(f14.3,2x))") i,"  ", &
      rLDL(i), rDL(i), rUDL(i)
  enddo

  do i=1,24
    write(*,fmt="('DH(',i2') ',a,t50,3(f14.3,2x))") i,"  ", &
      rLDH(i), rDH(i), rUDH(i)
  enddo

  do i=1,9
    write(*,fmt="('RA(',i2') ',a,t50,3(f14.3,2x))") i,"  ", &
      rLRA(i), rRA(i), rURA(i)
  enddo


end program test_hi
