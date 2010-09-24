module tsp_hydrologic_indices

   use tsp_data_structures
   use tsp_utilities
   use tsp_command_processors
   implicit none

   ! Define enumerated items for PERCENTILE indices
   enum, bind(c)
     enumerator :: P01 = 1
     enumerator P05, P10, P15, P20, P25, P30, P35, P40, P45, P50, P55, P60, P65, &
                P70, P75, P80, P85, P90, P95, P99
   end enum

   ! Enumerated constants for DAY AVERAGING indices
   enum, bind(c)
     enumerator :: D1 = 1
     enumerator D3, D7, D30, D90
   end enum

   ! Enumerated constants for MONTH indices
   enum, bind(c)
     enumerator :: JAN = 1
     enumerator FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC
   end enum

   type T_AVERAGING_PERIOD
     integer (kind=T_INT) :: iDaysInPeriod
     character (len=256) :: sDescription
   end type T_AVERAGING_PERIOD

   type T_HI
     integer (kind=T_INT) :: iUnits
     character (len=80)   :: sHydrologicIndex
     real (kind=T_SGL)    :: rValue
   end type T_HI

   integer (kind=T_INT), parameter :: iNUM_QUANTILES = 21
   integer (kind=T_INT), parameter :: iNUM_PERIODS = 5

   type T_HI_STATS
      integer (kind=T_INT) :: iCount = 0
      real (kind=T_SGL) :: rMean = rZERO
      real (kind=T_SGL) :: rMedian = rZERO
      real (kind=T_SGL) :: rVariance = rZERO
      real (kind=T_SGL) :: rStddev = rZERO
      real (kind=T_SGL) :: rCV = rZERO
      real (kind=T_SGL), dimension(iNUM_QUANTILES) :: rQuantile = rZERO
      real (kind=T_SGL), dimension(iNUM_QUANTILES) :: rExceedance = rZERO
      real (kind=T_SGL) :: rMin = rZERO
      real (kind=T_SGL) :: rMax = rZERO
      integer (kind=T_INT) :: iDayOfYearMin
      integer (kind=T_INT) :: iDayOfYearMax
      real (kind=T_SGL), dimension(iNUM_PERIODS) :: rPeriodMin = rZERO
      real (kind=T_SGL), dimension(iNUM_PERIODS) :: rPeriodMax = rZERO
      logical (kind=T_LOGICAL) :: lValid = lFALSE
   end type T_HI_STATS

   type T_STATS_COLLECTION
     type (T_HI_STATS), dimension(:), pointer :: pByYear
     type (T_HI_STATS), dimension(:), pointer :: pByMonth
     type (T_HI_STATS), dimension(:,:), pointer :: pByYearAndMonth
     type (T_HI_STATS), pointer :: pAllRecords
   end type T_STATS_COLLECTION

   real(kind=T_SGL), dimension(iNUM_QUANTILES), parameter :: rSTD_PROBABILITIES = (/ &
     0.01_T_SGL, 0.05_T_SGL, 0.1_T_SGL, 0.15_T_SGL, 0.2_T_SGL, 0.25_T_SGL, &
     0.3_T_SGL, 0.35_T_SGL, 0.4_T_SGL, 0.45_T_SGL, 0.5_T_SGL, 0.55_T_SGL, &
     0.6_T_SGL, 0.65_T_SGL, 0.7_T_SGL, 0.75_T_SGL, 0.8_T_SGL, 0.85_T_SGL, &
     0.9_T_SGL, 0.95_T_SGL, 0.99_T_SGL /)

   type (T_AVERAGING_PERIOD), dimension(iNUM_PERIODS) :: AVERAGING_PERIOD = (/ &
     T_AVERAGING_PERIOD(1, '1-day'), &
     T_AVERAGING_PERIOD(3, '3-day'), &
     T_AVERAGING_PERIOD(7, '7-day'), &
     T_AVERAGING_PERIOD(30, '30-day'), &
     T_AVERAGING_PERIOD(90, '90-day') &
     /)

   contains

function read_USGS_NWIS(sFilename)   result(pGage)

  character(len=*) :: sFilename
  type(T_USGS_NWIS_GAGE),dimension(:), pointer :: pGage
  type(T_USGS_NWIS_Daily),dimension(:), allocatable :: pTempDaily


  ! [ LOCALS ]
  integer (kind=T_INT) :: iMM, iDD, iYYYY
  integer (kind=T_INT) :: iStat
  integer (kind=T_INT) :: iLineNum = 0
  integer (kind=T_INT) :: iGageNum = 0
  character (len=256)   :: sTempAgencyCode
  character (len=256)   :: sTempSiteNumber

  integer (kind=T_INT) :: iMonth, iDay, iYear, iJulianDay, iWaterYear

  integer (kind=T_INT) :: iCount = 0
  integer (kind=T_INT) :: iNumNonBlankValues
  integer (kind=T_INT) :: i
  integer (kind=T_INT),dimension(:),allocatable  :: iTotalNumLines
  integer (kind=T_INT) :: iTotalNumGages = 0
  integer (kind=T_INT), parameter :: LU_NWIS = 101

  character (len=256) sRecord, sItem

  open(unit=LU_NWIS,file=TRIM(ADJUSTL(sFilename)),status='OLD',iostat=iStat)
  call Assert(iStat==0,'Error opening file '//TRIM(sFilename))

  ! make first pass through the NWIS file to determine what it contains
  do
    read(unit=LU_NWIS, fmt="(a)",iostat=iStat) sRecord
    if(iStat /= 0) exit
    if(sRecord(1:24) .eq. '# Data for the following') then  ! next lines describe the gages
      do
        read(unit=LU_NWIS, fmt="(a)",iostat=iStat) sRecord
        if(sRecord(1:5) .eq. '# ---') exit
        iTotalNumGages = iTotalNumGages + 1
      end do
      allocate(iTotalNumLines(iTotalNumGages))
      iTotalNumLines = 0.
      cycle
    else if(sRecord(1:9) .eq. 'agency_cd') then
      read(unit=LU_NWIS, fmt="(a)",iostat=iStat) sRecord
      iGageNum = iGageNum + 1
      cycle
    else if(sRecord(1:1) .eq. "#") then
      cycle
    end if
    call Chomp_tab(sRecord,sItem)  ! agency cd
    call Chomp_tab(sRecord,sItem)  ! USGS gage ID
    call Chomp_tab(sRecord,sItem)  ! date
    call Chomp_tab(sRecord,sItem)  ! discharge and data flag

    if(len_trim( sItem ) > 0 ) &
      iTotalNumLines(iGageNum) = iTotalNumLines(iGageNum) + 1
  end do

!  print *, "Allocated the following array sizes:"
!  print *
!  do iGageNum=1,iTotalNumGages
!    print *, iGageNum, iTotalNumLines(iGageNum)
!  end do


  rewind(unit=LU_NWIS, iostat=iStat)
  call Assert(iStat==0,"Problem rewinding NWIS data file")

  ! ALLOCATE memory for the GAGE object (collection of gages)
  allocate(pGage(iTotalNumGages),stat=iStat)
  call Assert(iStat==0,"Problem allocating memory for NWIS gage data structure", &
     TRIM(__FILE__),__LINE__)

  ! ALLOCATE memory for the time-series data associated with each gage
  do iGageNum = 1,iTotalNumGages
    allocate(pGage(iGageNum)%pGageData(iTotalNumLines(iGageNum)),stat=iStat)
  call Assert(iStat==0,"Problem allocating memory for NWIS daily data structure", &
     TRIM(__FILE__),__LINE__)
  end do

  iLineNum = 0
  iGageNum = 0

  ! now make second pass through the NWIS file, commiting values to memory
  do
    read(unit=LU_NWIS, fmt="(a)",iostat=iStat) sRecord
    if(iStat /= 0) then
      exit
    end if

    if(sRecord(1:24) .eq. '# Data for the following') then  ! next lines describe the gages
      iCount = 0
      do
        read(unit=LU_NWIS, fmt="(a)",iostat=iStat) sRecord
        if(sRecord(1:5) .eq. '# ---') exit
        iCount = iCount + 1
        call Chomp(sRecord, sItem)
        pGage(iCount)%sDescription = TRIM(ADJUSTL(sRecord))
        write(*,fmt="(' ** READ IN DATA FOR:',/,7x,i6,') ',a)") &
           iCount,pGage(iCount)%sDescription
      end do
      cycle
    else if(sRecord(1:9) .eq. 'agency_cd') then
      read(unit=LU_NWIS, fmt="(a)",iostat=iStat) sRecord
      iGageNum = iGageNum + 1
      iLineNum = 0
      cycle
    else if(sRecord(1:1) .eq. "#") then
      cycle
    else

      if(iLineNum == 0) then
        call Chomp_tab(sRecord, sItem)
        pGage(iGageNum)%sAgencyCode = TRIM(ADJUSTL(sItem))
        call Chomp_tab(sRecord, sItem)
        pGage(iGageNum)%sSiteNumber = TRIM(ADJUSTL(sItem))
      else

        ! skip over first two items
        call Chomp_tab(sRecord, sTempAgencyCode)
        call Chomp_tab(sRecord, sTempSiteNumber)
        ! check to make sure the NWIS file isn't somehow corrupted
        call Assert(TRIM(sTempAgencyCode) .eq. &
           TRIM(pGage(iGageNum)%sAgencyCode), &
           "Error in reading NWIS file - mismatched agency codes", &
           TRIM(__FILE__), __LINE__)
        call Assert(TRIM(sTempSiteNumber) .eq. &
           TRIM(pGage(iGageNum)%sSiteNumber), &
           "Error in reading NWIS file - mismatched agency codes", &
           TRIM(__FILE__), __LINE__)
      end if

      ! obtain a value for the DATE field
      call Chomp_tab(sRecord, sItem)

      read(sItem(1:4),fmt=*) iYear
      read(sItem(6:7),fmt=*) iMonth
      read(sItem(9:10),fmt=*) iDay

      ! obtain a value for the DISCHARGE and DATA FLAG fields
      call Chomp_tab(sRecord, sItem)

      if(len_trim(sItem)>0) then
        iLineNum = iLineNum + 1
        read(sItem,fmt=*) pGage(iGageNum)%pGageData(iLineNum)%rMeanDischarge
        call Chomp_tab(sRecord, sItem)
        pGage(iGageNum)%pGageData(iLineNum)%sDataFlag = TRIM(sItem)
      else
        cycle
      end if

      ! we have valid data; record the date
      pGage(iGageNum)%pGageData(iLineNum)%iYear = iYear
      pGage(iGageNum)%pGageData(iLineNum)%iMonth = iMonth
      pGage(iGageNum)%pGageData(iLineNum)%iDay = iDay

      ! calculate the Julian day for this date
      pGage(iGageNum)%pGageData(iLineNum)%iJulianDay = julian_day ( &
          pGage(iGageNum)%pGageData(iLineNum)%iYear, &
          pGage(iGageNum)%pGageData(iLineNum)%iMonth, &
          pGage(iGageNum)%pGageData(iLineNum)%iDay)

      ! calculate a value for the WATER YEAR field
      if(pGage(iGageNum)%pGageData(iLineNum)%iMonth > 9) then
         pGage(iGageNum)%pGageData(iLineNum)%iWaterYear = &
            pGage(iGageNum)%pGageData(iLineNum)%iYear + 1
      else
         pGage(iGageNum)%pGageData(iLineNum)%iWaterYear = &
            pGage(iGageNum)%pGageData(iLineNum)%iYear
      end if


    end if

  end do

  print *, "read_USGS_NWIS: "//TRIM(pGage(1)%sDescription)
  print *, "read_USGS_NWIS: "//TRIM(pGage(2)%sDescription)
  print *, "read_USGS_NWIS: "//TRIM(pGage(3)%sDescription)


  return

end function read_USGS_NWIS

function create_stats_object(rData, iMonth, iYear, iJulianDay)  result(pStats)

   real (kind=T_SGL), dimension(:) :: rData
   integer (kind=T_INT), dimension(:) :: iMonth
   integer (kind=T_INT), dimension(:) :: iYear
   integer (kind=T_INT), dimension(:) :: iJulianDay
   type (T_STATS_COLLECTION), pointer :: pStats

   ! [ LOCALS ]
   integer (kind=T_INT) :: iNumRecs
   integer (kind=T_INT) :: iStat
   integer (kind=T_INT) :: iFirstYear, iLastYear
   integer (kind=T_INT) :: i, j
   real (kind=T_SGL), dimension(:), allocatable :: rSubset
   integer (kind=T_INT), dimension(:), allocatable :: iJD

   iNumRecs = size(rData)
   iFirstYear = MINVAL(iYear)
   iLastYear = MAXVAL(iYear)

   allocate(pStats, stat=iStat)
   call Assert( iStat == 0, &
     "Could not allocate memory for statistics collections data object")

   allocate(pStats%pByYear(iFirstYear:iLastYear), stat=iStat)
   call Assert( iStat == 0, &
     "Could not allocate memory for 'ByYear' statistics data object")

   allocate(pStats%pByMonth(12), stat=iStat)
   call Assert( iStat == 0, &
     "Could not allocate memory for 'ByMonth' statistics data object")

   do i=1,12
     allocate(rSubset(count(iMonth==i)))
     allocate(iJD(count(iMonth==i)))

     ! create a subset of the data for a given month
     rSubset = PACK(rData,iMonth==i)
     iJD = PACK(iJulianDay,iMonth==i)

     if(size(rSubset) < 30) then
       pStats%pByMonth(i)%lValid = lFALSE
     else
       pStats%pByMonth(i) = calc_base_stats(rSubset, iJD)
     endif

     deallocate(rSubset)
     deallocate(iJD)
   enddo

   allocate(pStats%pByYearAndMonth(iFirstYear:iLastYear,12), stat=iStat)
   call Assert( iStat == 0, &
     "Could not allocate memory for 'ByYearAndMonth' statistics data object")

   do i=iFirstYear,iLastYear

     allocate(rSubset(count( iYear==i )))
     allocate(iJD(count( iYear==i )))

     ! create a subset of the data for a given month/year combination
     rSubset = PACK(rData, iYear==i)
     iJD = PACK(iJulianDay, iYear==i)

     if(size(rSubset) < 350) then
         pStats%pByYear(i)%lValid = lFALSE
       else
         pStats%pByYear(i) = calc_base_stats(rSubset, iJD)
     endif

     deallocate(rSubset)
     deallocate(iJD)

     do j=1,12

       allocate(rSubset(count(iMonth==j .and. iYear==i )))
       allocate(iJD(count(iMonth==j .and. iYear==i )))

       ! create a subset of the data for a given month/year combination
       rSubset = PACK(rData,iMonth==j .and. iYear==i)
       iJD = PACK(iJulianDay,iMonth==j .and. iYear==i)

       if(size(rSubset) < 30) then
         pStats%pByYearAndMonth(i,j)%lValid = lFALSE
       else
         pStats%pByYearAndMonth(i,j) = calc_base_stats(rSubset, iJD)
       endif

       deallocate(rSubset)
       deallocate(iJD)
     enddo
   enddo

   allocate(pStats%pAllRecords, stat=iStat)
   call Assert( iStat == 0, &
     "Could not allocate memory for 'AllRecords' statistics data object")

   pStats%pAllRecords = calc_base_stats(rData, iJulianDay)

   return

end function create_stats_object


function calc_base_stats(rData, iJulianDay)   result(pBaseStats)

   real (kind=T_SGL), dimension(:) :: rData
   integer (kind=T_INT), dimension(:) :: iJulianDay
   type (T_HI_STATS) :: pBaseStats

   ! [ LOCALS ]
   integer (kind=T_INT) :: i,j,k
   integer (kind=T_INT), dimension(1) :: iLocMin, iLocMax
   real (kind=T_SGL) :: rMean
   integer (kind=T_INT) :: iMaxPeriodIndex
   integer (kind=T_INT) ,dimension(size(rData)):: iOriginalOrder
   real (kind=T_SGL), dimension(size(rData)) :: rSortedData

   rSortedData = rData

   call quick_sort(rSortedData, iOriginalOrder)

   pBaseStats%iCount = size(rData)

!   call stats(rData,pStats%rMean, pStats%rMedian, pStats%rStddev)

!   pStats%rMean = SUM(rData) / real(size(rData))
   pBaseStats%rMedian = median(rData)

!   call online_mean(rData, pStats%rMean, pStats%rVariance)

   pBaseStats%rMean = mean(rData)
   pBaseStats%rVariance = variance(rData)

   pBaseStats%rStddev = sqrt(pBaseStats%rVariance)
   if(pBaseStats%rMean /= 0.) pBaseStats%rCV = pBaseStats%rStddev / pBaseStats%rMean

   do i=1,iNUM_QUANTILES
     pBaseStats%rQuantile(i) = quantile(rSTD_PROBABILITIES(i), rSortedData)
     pBaseStats%rExceedance(iNUM_QUANTILES - i + 1) = pBaseStats%rQuantile(i)
   end do

   pBaseStats%rMin = MINVAL(rData)
   pBaseStats%rMax = MAXVAL(rData)
   iLocMin = MINLOC(rData)
   iLocMax = MAXLOC(rData)

   pBaseStats%iDayOfYearMin = day_of_year(iJulianDay(iLocMin(1)))
   pBaseStats%iDayOfYearMax = day_of_year(iJulianDay(iLocMax(1)))

   pBaseStats%rPeriodMax = 1.e-20
   pBaseStats%rPeriodMin = 1.e+20

   if(size(rData) >= 365) then
     iMaxPeriodIndex = D90
   else if(size(rData) >= 30) then
     iMaxPeriodIndex = D30
   else
     iMaxPeriodIndex = D1
   endif

   do i=1,iMaxPeriodIndex
     do j=1,size(rData) - AVERAGING_PERIOD(i)%iDaysInPeriod + 1
       k = j + AVERAGING_PERIOD(i)%iDaysInPeriod - 1
       rMean = SUM(rData(j:k)) / AVERAGING_PERIOD(i)%iDaysInPeriod
       if (rMean > pBaseStats%rPeriodMax(i)) pBaseStats%rPeriodMax(i) = rMean
       if (rMean < pBaseStats%rPeriodMin(i)) pBaseStats%rPeriodMin(i) = rMean
     end do
   end do

   pBaseStats%lValid = lTRUE

   return

end function calc_base_stats

function compute_hyd_indices_MA(pStats)  result(MA)

  type (T_STATS_COLLECTION), pointer :: pStats
  type(T_HI), dimension(:), pointer :: MA

   ! [ LOCALS ]
   integer(kind=T_INT) :: i
   real (kind=T_SGL) :: rTempVal
   real (kind=T_SGL), dimension(iNUM_QUANTILES) :: &
          rQuantilesOfMonthlyMeanFlow, rQuantilesOfAnnualMeanFlow
   real (kind=T_SGL), dimension(:), allocatable :: rMeanMonthlyFlowTS
   real (kind=T_SGL), dimension(:), allocatable :: rMeanAnnualFlowTS

   allocate(MA(45) )

   MA = (/ &
     T_HI( 1,'Mean, all daily flows',rZERO), &
     T_HI( 1,'Median, all daily flows',rZERO), &
     T_HI( 6,'CV, all daily flows',rZERO), &
     T_HI( 6,'CV, log of all daily flows',rZERO), &
     T_HI( 6,'Mean daily flow / median daily flow',rZERO), &
     T_HI( 6,'Ratio, Q10 / Q90 for all daily flows',rZERO), &
     T_HI( 6,'Ratio, Q20 / Q80 for all daily flows',rZERO), &
     T_HI( 6,'Ratio, Q25 / Q75 for all daily flows',rZERO), &
     T_HI( 2,'(Q10 - Q90) / median daily flow',rZERO), &
     T_HI( 2,'(Q20 - Q80) / median daily flow',rZERO), &
     T_HI( 2,'(Q25 - Q75) / median daily flow',rZERO), &
     T_HI( 2,'Mean monthly flow, January',rZERO), &
     T_HI( 2,'Mean monthly flow, February',rZERO), &
     T_HI( 2,'Mean monthly flow, March',rZERO), &
     T_HI( 2,'Mean monthly flow, April',rZERO), &
     T_HI( 2,'Mean monthly flow, May',rZERO), &
     T_HI( 2,'Mean monthly flow, June',rZERO), &
     T_HI( 2,'Mean monthly flow, July',rZERO), &
     T_HI( 2,'Mean monthly flow, August',rZERO), &
     T_HI( 2,'Mean monthly flow, September',rZERO), &
     T_HI( 2,'Mean monthly flow, October',rZERO), &
     T_HI( 2,'Mean monthly flow, November',rZERO), &
     T_HI( 2,'Mean monthly flow, December',rZERO), &
     T_HI( 2,'CV of monthly flow, January',rZERO), &
     T_HI( 2,'CV of monthly flow, February',rZERO), &
     T_HI( 2,'CV of monthly flow, March',rZERO), &
     T_HI( 2,'CV of monthly flow, April',rZERO), &
     T_HI( 2,'CV of monthly flow, May',rZERO), &
     T_HI( 2,'CV of monthly flow, June',rZERO), &
     T_HI( 2,'CV of monthly flow, July',rZERO), &
     T_HI( 2,'CV of monthly flow, August',rZERO), &
     T_HI( 2,'CV of monthly flow, September',rZERO), &
     T_HI( 2,'CV of monthly flow, October',rZERO), &
     T_HI( 2,'CV of monthly flow, November',rZERO), &
     T_HI( 2,'CV of monthly flow, December',rZERO), &
     T_HI( 2,'Range mean monthly / median monthly flow',rZERO), &
     T_HI( 2,'IQR mean monthly / median monthly flow',rZERO), &
     T_HI( 2,'(Q10 - Q90)[monthly] / median monthly flow',rZERO), &
     T_HI( 2,'CV, monthly mean flows',rZERO), &
     T_HI( 2,'Skewness in monthly flows',rZERO), &
     T_HI( 2,'Mean annual runoff',rZERO), &
     T_HI( 2,'Range mean annual / median annual flow',rZERO), &
     T_HI( 2,'IQR mean annual / median annual flow',rZERO), &
     T_HI( 2,'(Q10 - Q90)[annual] / median annual flow',rZERO), &
     T_HI( 2,'Skewness in annual flows',rZERO) &
     /)

   MA(1)%rValue = pStats%pAllRecords%rMean
   MA(2)%rValue = pStats%pAllRecords%rMedian

   if(pStats%pAllRecords%rMean /= 0.) &
        MA(3)%rValue = mean(pStats%pByYear%rStdDev) / pStats%pAllRecords%rMean * 100.


   if(MA(2)%rValue /=0. ) &
        MA(5)%rValue = pStats%pAllRecords%rMean / pStats%pAllRecords%rMedian

   rTempVal = mean(pStats%pAllRecords%rQuantile(P05:P95))
   if(rTempVal /= 0.) &
        MA(4)%rValue = stddev(pStats%pAllRecords%rQuantile(P05:P95)) / rTempVal * 100.

   MA(6)%rValue = pStats%pAllRecords%rExceedance(P10) / pStats%pAllRecords%rExceedance(P90)
   MA(7)%rValue = pStats%pAllRecords%rExceedance(p20) / pStats%pAllRecords%rExceedance(P80)
   MA(8)%rValue = pStats%pAllRecords%rExceedance(p25) / pStats%pAllRecords%rExceedance(p75)

   if(MA(2)%rValue > 0.) then
     MA(9)%rValue  = (pStats%pAllRecords%rExceedance(P10) - pStats%pAllRecords%rExceedance(P90)) / MA(2)%rValue
     MA(10)%rValue = (pStats%pAllRecords%rExceedance(P20) - pStats%pAllRecords%rExceedance(P80)) / MA(2)%rValue
     MA(11)%rValue = (pStats%pAllRecords%rExceedance(P25) - pStats%pAllRecords%rExceedance(P75)) / MA(2)%rValue
   end if

   do i=1,12
     MA(11+i)%rValue = mean(PACK(pStats%pByYearAndMonth(:,i)%rMean, &
                              pStats%pByYearAndMonth(:,i)%lValid))

     MA(23+i)%rValue = mean(PACK(pStats%pByYearAndMonth(:,i)%rCV * 100., &
                              pStats%pByYearAndMonth(:,i)%lValid))
   end do

   allocate(rMeanMonthlyFlowTS(COUNT(pStats%pByYearAndMonth(:,:)%lValid)))

   rMeanMonthlyFlowTS = PACK(pStats%pByYearAndMonth(:,:)%rMean, &
                               pStats%pByYearAndMonth(:,:)%lValid)

   allocate(rMeanAnnualFlowTS(COUNT(pStats%pByYear%lValid)))

   rMeanAnnualFlowTS = PACK(pStats%pByYear(:)%rMean, &
                               pStats%pByYear(:)%lValid)


   write(*,*) "      PROB  Q(mon mean TS) Q(ann mean TS)"
   write (*,*)"---------------------------------------------"

   do i=1,iNUM_QUANTILES
     rQuantilesOfMonthlyMeanFlow(i) = quantile(rSTD_PROBABILITIES(i), rMeanMonthlyFlowTS)
     rQuantilesOfAnnualMeanFlow(i) = quantile(rSTD_PROBABILITIES(i), rMeanAnnualFlowTS)
     write(*,fmt="(i3,') ',f6.2,2f12.3)")  i,rSTD_PROBABILITIES(i), &
         rQuantilesOfMonthlyMeanFlow(i),rQuantilesOfAnnualMeanFlow(i)
   end do


   MA(36)%rValue = (MAXVAL(pStats%pByYearAndMonth(:,:)%rMean) &
                     - MINVAL(pStats%pByYearAndMonth(:,:)%rMean )) &
                     / rQuantilesOfMonthlyMeanFlow(P50)

   MA(37)%rValue = (rQuantilesOfMonthlyMeanFlow(P75) &
                     - rQuantilesOfMonthlyMeanFlow(P25)) &
                     / rQuantilesOfMonthlyMeanFlow(P50)

   MA(38)%rValue = (rQuantilesOfMonthlyMeanFlow(P90) &
                     - rQuantilesOfMonthlyMeanFlow(P10)) &
                     / rQuantilesOfMonthlyMeanFlow(P50)


   MA(39)%rValue = stddev(rMeanMonthlyFlowTS) / mean(rMeanMonthlyFlowTS) * 100.

   MA(40)%rValue =  (mean(rMeanMonthlyFlowTS) -  median(rMeanMonthlyFlowTS)) &
                      / median(rMeanMonthlyFlowTS)

   MA(42)%rValue = (MAXVAL(pStats%pByYear%rMean) &
                     - MINVAL(pStats%pByYear%rMean )) &
                     / rQuantilesOfAnnualMeanFlow(P50)

   MA(43)%rValue = ( rQuantilesOfAnnualMeanFlow(P75) &
                     - rQuantilesOfAnnualMeanFlow(P25)) &
                     / rQuantilesOfAnnualMeanFlow(P50)

   MA(44)%rValue = ( rQuantilesOfAnnualMeanFlow(P90) &
                     - rQuantilesOfAnnualMeanFlow(P10)) &
                     / rQuantilesOfAnnualMeanFlow(P50)

   MA(45)%rValue =  (mean(rMeanAnnualFlowTS) -  median(rMeanAnnualFlowTS)) &
                      / median(rMeanAnnualFlowTS)


   deallocate(rMeanMonthlyFlowTS)
   deallocate(rMeanAnnualFlowTS)


   return

end function compute_hyd_indices_MA

subroutine write_base_stats(pBaseStats, sDescription)

  type (T_HI_STATS) :: pBaseStats
  character (len=*), optional :: sDescription

  if(present(sDescription)) then
    write(*,fmt="(/,a,/,a,/,a,/)") repeat("-",80),TRIM(sDescription),repeat("-",80)
  end if

  if(pBaseStats%lValid) then

    write(*,fmt="('  count:',t15,i8)") pBaseStats%iCount
    write(*,fmt="('  mean:',t15,f12.3)") pBaseStats%rMean
    write(*,fmt="('  median:',t15,f12.3)") pBaseStats%rMedian
    write(*,fmt="('  variance:',t15,f12.3)") pBaseStats%rVariance
    write(*,fmt="('  min:',t15,f12.3)") pBaseStats%rMin
    write(*,fmt="('  max:',t15,f12.3)") pBaseStats%rMax
    write(*,fmt="('  DOY, minimum:',t15,i8)") pBaseStats%iDayOfYearMin
    write(*,fmt="('  DOY, maximum:',t15,i8)") pBaseStats%iDayOfYearMax
    write(*,fmt="(/)")

  else

      write(*,fmt="('  ===> NOT ENOUGH VALID DATA FOR THIS TIME SUBSET... NO STATS CALCULATED <===',/)")

  endif

  ! [ LOCALS ]

!      integer (kind=T_INT) :: iCount = 0
!      real (kind=T_SGL) :: rMean = rZERO
!      real (kind=T_SGL) :: rMedian = rZERO
!      real (kind=T_SGL) :: rVariance = rZERO
!      real (kind=T_SGL) :: rStddev = rZERO
!      real (kind=T_SGL) :: rCV = rZERO
!      real (kind=T_SGL), dimension(iNUM_QUANTILES) :: rQuantile = rZERO
!      real (kind=T_SGL), dimension(iNUM_QUANTILES) :: rExceedance = rZERO
!      real (kind=T_SGL) :: rMin = rZERO
!      real (kind=T_SGL) :: rMax = rZERO
!      integer (kind=T_INT) :: iDayOfYearMin
!      integer (kind=T_INT) :: iDayOfYearMax
!      real (kind=T_SGL), dimension(iNUM_PERIODS) :: rPeriodMin = rZERO
!      real (kind=T_SGL), dimension(iNUM_PERIODS) :: rPeriodMax = rZERO

  return

end subroutine write_base_stats

end module tsp_hydrologic_indices
