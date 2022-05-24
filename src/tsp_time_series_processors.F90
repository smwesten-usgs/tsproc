module tsp_time_series_processors

    use ISO_C_BINDING
    use tsp_data_structures
    implicit none

    type T_HI
        integer(kind=T_INT) :: iUnits
        integer(kind=T_INT) :: iTemporalAspect
        character(len=80) :: sHydrologicIndex
        real(kind=T_DBL) :: rValue
        logical(kind=T_LOGICAL) :: lInclude = lFALSE
        integer(kind=T_INT) :: iMask = 0
    end type T_HI

    integer, parameter :: iDAILY = 0
    integer, parameter :: iMONTHLY = 1
    integer, parameter :: iANNUAL = 2

!void compute_hi(int datatype, float carea,
!                float m_lp, float m_up, int yr[150], float q[150][366])

    interface
        subroutine compute_hi(lUseMedian, rCarea, rNearHuge, &
                              rLowerPercentile, rUpperPercentile, iYr, rQ, &
                              rMA, rLMA, rUMA, &
                              rML, rLML, rUML, &
                              rMH, rLMH, rUMH, &
                              rFL, rLFL, rUFL, &
                              rFH, rLFH, rUFH, &
                              rDL, rLDL, rUDL, &
                              rDH, rLDH, rUDH, &
                              rTA, rLTA, rUTA, &
                              rTL, rLTL, rUTL, &
                              rTH, rLTH, rUTH, &
                              rRA, rLRA, rURA) bind(C)

            use ISO_C_BINDING

            logical(C_BOOL), value :: lUseMedian
            real(C_DOUBLE), value :: rCarea
            real(C_DOUBLE), value :: rNearHuge
            real(C_DOUBLE), value :: rLowerPercentile
            real(C_DOUBLE), value :: rUpperPercentile
            integer(C_INT), dimension(150) :: iYr
            real(C_DOUBLE), dimension(0:365, 0:149) :: rQ
            real(C_DOUBLE), dimension(0:45) :: rMA
            real(C_DOUBLE), dimension(0:45) :: rLMA
            real(C_DOUBLE), dimension(0:45) :: rUMA
            real(C_DOUBLE), dimension(0:22) :: rML
            real(C_DOUBLE), dimension(0:22) :: rLML
            real(C_DOUBLE), dimension(0:22) :: rUML
            real(C_DOUBLE), dimension(0:28) :: rMH
            real(C_DOUBLE), dimension(0:28) :: rLMH
            real(C_DOUBLE), dimension(0:28) :: rUMH
            real(C_DOUBLE), dimension(0:4) :: rFL
            real(C_DOUBLE), dimension(0:4) :: rLFL
            real(C_DOUBLE), dimension(0:4) :: rUFL
            real(C_DOUBLE), dimension(0:11) :: rFH
            real(C_DOUBLE), dimension(0:11) :: rLFH
            real(C_DOUBLE), dimension(0:11) :: rUFH
            real(C_DOUBLE), dimension(0:20) :: rDL
            real(C_DOUBLE), dimension(0:20) :: rLDL
            real(C_DOUBLE), dimension(0:20) :: rUDL
            real(C_DOUBLE), dimension(0:24) :: rDH
            real(C_DOUBLE), dimension(0:24) :: rLDH
            real(C_DOUBLE), dimension(0:24) :: rUDH

            real(C_DOUBLE), dimension(0:3) :: rUTA
            real(C_DOUBLE), dimension(0:3) :: rTA
            real(C_DOUBLE), dimension(0:3) :: rLTA

            real(C_DOUBLE), dimension(0:4) :: rUTL
            real(C_DOUBLE), dimension(0:4) :: rTL
            real(C_DOUBLE), dimension(0:4) :: rLTL

            real(C_DOUBLE), dimension(0:3) :: rUTH
            real(C_DOUBLE), dimension(0:3) :: rTH
            real(C_DOUBLE), dimension(0:3) :: rLTH

            real(C_DOUBLE), dimension(0:9) :: rRA
            real(C_DOUBLE), dimension(0:9) :: rLRA
            real(C_DOUBLE), dimension(0:9) :: rURA
        end subroutine compute_hi
    end interface

    type(T_HI), dimension(45) :: MA = [ &
      T_HI(1, iDAILY, 'Mean, all daily flows', rZERO, lFALSE, int(B'00000001')), & ! 1*
      T_HI(1, iDAILY, 'Median, all daily flows', rZERO, lFALSE, int(B'00000001')), & ! 2*
      T_HI(6, iDAILY, 'CV, all daily flows', rZERO, lFALSE, int(B'00011011')), & ! 3*
      T_HI(6, iDAILY, 'CV, log of all daily flows', rZERO, lFALSE, int(B'00000001')), & ! 4*
      T_HI(6, iDAILY, 'Mean daily flow / median daily flow', rZERO, lFALSE, int(B'00000011')), & ! 5*
      T_HI(6, iDAILY, 'Ratio, Q10 / Q90 for all daily flows', rZERO, lFALSE, int(B'00000001')), & ! 6*
      T_HI(6, iDAILY, 'Ratio, Q20 / Q80 for all daily flows', rZERO, lFALSE, int(B'00000001')), & ! 7*
      T_HI(6, iDAILY, 'Ratio, Q25 / Q75 for all daily flows', rZERO, lFALSE, int(B'00001001')), & ! 8*
      T_HI(2, iDAILY, '(Q10 - Q90) / median daily flow', rZERO, lFALSE, int(B'01000001')), & ! 9*
      T_HI(2, iDAILY, '(Q20 - Q80) / median daily flow', rZERO, lFALSE, int(B'00000101')), & ! 10
      T_HI(2, iDAILY, '(Q25 - Q75) / median daily flow', rZERO, lFALSE, int(B'00000011')), & ! 11
      T_HI(1, iMONTHLY, 'Mean monthly flow, January', rZERO, lFALSE, int(B'00000001')), & ! 12
      T_HI(1, iMONTHLY, 'Mean monthly flow, February', rZERO, lFALSE, int(B'00000001')), & ! 13
      T_HI(1, iMONTHLY, 'Mean monthly flow, March', rZERO, lFALSE, int(B'00000001')), & ! 14
      T_HI(1, iMONTHLY, 'Mean monthly flow, April', rZERO, lFALSE, int(B'00000001')), & ! 15
      T_HI(1, iMONTHLY, 'Mean monthly flow, May', rZERO, lFALSE, int(B'10000001')), & ! 16
      T_HI(1, iMONTHLY, 'Mean monthly flow, June', rZERO, lFALSE, int(B'00000001')), & ! 17
      T_HI(1, iMONTHLY, 'Mean monthly flow, July', rZERO, lFALSE, int(B'01000001')), & ! 18
      T_HI(1, iMONTHLY, 'Mean monthly flow, August', rZERO, lFALSE, int(B'00000001')), & ! 19
      T_HI(1, iMONTHLY, 'Mean monthly flow, September', rZERO, lFALSE, int(B'00000001')), & ! 20
      T_HI(1, iMONTHLY, 'Mean monthly flow, October', rZERO, lFALSE, int(B'01000001')), & ! 21
      T_HI(1, iMONTHLY, 'Mean monthly flow, November', rZERO, lFALSE, int(B'10000001')), & ! 22
      T_HI(1, iMONTHLY, 'Mean monthly flow, December', rZERO, lFALSE, int(B'00000001')), & ! 23
      T_HI(6, iMONTHLY, 'CV of monthly flow, January', rZERO, lFALSE, int(B'00000001')), & ! 24
      T_HI(6, iMONTHLY, 'CV of monthly flow, February', rZERO, lFALSE, int(B'00000001')), & ! 25
      T_HI(6, iMONTHLY, 'CV of monthly flow, March', rZERO, lFALSE, int(B'00000101')), & ! 26
      T_HI(6, iMONTHLY, 'CV of monthly flow, April', rZERO, lFALSE, int(B'00000001')), & ! 27
      T_HI(6, iMONTHLY, 'CV of monthly flow, May', rZERO, lFALSE, int(B'00000001')), & ! 28
      T_HI(6, iMONTHLY, 'CV of monthly flow, June', rZERO, lFALSE, int(B'00100001')), & ! 29
      T_HI(6, iMONTHLY, 'CV of monthly flow, July', rZERO, lFALSE, int(B'00000001')), & ! 30
      T_HI(6, iMONTHLY, 'CV of monthly flow, August', rZERO, lFALSE, int(B'00000001')), & ! 31
      T_HI(6, iMONTHLY, 'CV of monthly flow, September', rZERO, lFALSE, int(B'00000001')), & ! 32
      T_HI(6, iMONTHLY, 'CV of monthly flow, October', rZERO, lFALSE, int(B'00000001')), & ! 33
      T_HI(6, iMONTHLY, 'CV of monthly flow, November', rZERO, lFALSE, int(B'10000001')), & ! 34
      T_HI(6, iMONTHLY, 'CV of monthly flow, December', rZERO, lFALSE, int(B'00000001')), & ! 35
      T_HI(6, iMONTHLY, 'Range mean monthly / median monthly flow', rZERO, lFALSE, int(B'00000001')), & ! 36
      T_HI(6, iMONTHLY, 'IQR mean monthly / median monthly flow', rZERO, lFALSE, int(B'01000001')), & ! 37
      T_HI(6, iMONTHLY, '(Q10 - Q90)[monthly] / median monthly flow', rZERO, lFALSE, int(B'00000001')), & ! 38
      T_HI(6, iMONTHLY, 'CV, monthly mean flows', rZERO, lFALSE, int(B'00000001')), & ! 39
      T_HI(6, iMONTHLY, 'Skewness in monthly flows', rZERO, lFALSE, int(B'00100001')), & ! 40
      T_HI(3, iANNUAL, 'Mean annual runoff', rZERO, lFALSE, int(B'00001111')), & ! 41
      T_HI(6, iANNUAL, 'Range mean annual / median annual flow', rZERO, lFALSE, int(B'00000001')), & ! 42
      T_HI(6, iANNUAL, 'IQR mean annual / median annual flow', rZERO, lFALSE, int(B'00000001')), & ! 43
      T_HI(6, iANNUAL, '(Q10 - Q90)[annual] / median annual flow', rZERO, lFALSE, int(B'00010001')), & ! 44
      T_HI(6, iANNUAL, 'Skewness in annual flows', rZERO, lFALSE, int(B'00000001')) & ! 45
      ]

    type(T_HI), dimension(22) :: ML = [ &
      T_HI(1, iMONTHLY, 'Mean minimum monthly flow, January', rZERO, lFALSE, int(B'10000001')), & ! 1
      T_HI(1, iMONTHLY, 'Mean minimum monthly flow, February', rZERO, lFALSE, int(B'00000001')), & ! 2
      T_HI(1, iMONTHLY, 'Mean minimum monthly flow, March', rZERO, lFALSE, int(B'00000001')), & ! 3
      T_HI(1, iMONTHLY, 'Mean minimum monthly flow, April', rZERO, lFALSE, int(B'00000011')), & ! 4
      T_HI(1, iMONTHLY, 'Mean minimum monthly flow, May', rZERO, lFALSE, int(B'00000001')), & ! 5
      T_HI(1, iMONTHLY, 'Mean minimum monthly flow, June', rZERO, lFALSE, int(B'01000001')), & ! 6
      T_HI(1, iMONTHLY, 'Mean minimum monthly flow, July', rZERO, lFALSE, int(B'00000001')), & ! 7
      T_HI(1, iMONTHLY, 'Mean minimum monthly flow, August', rZERO, lFALSE, int(B'00000001')), & ! 8
      T_HI(1, iMONTHLY, 'Mean minimum monthly flow, September', rZERO, lFALSE, int(B'00000001')), & ! 9
      T_HI(1, iMONTHLY, 'Mean minimum monthly flow, October', rZERO, lFALSE, int(B'00000001')), & ! 10
      T_HI(1, iMONTHLY, 'Mean minimum monthly flow, November', rZERO, lFALSE, int(B'00000001')), & ! 11
      T_HI(1, iMONTHLY, 'Mean minimum monthly flow, December', rZERO, lFALSE, int(B'00000001')), & ! 12
      T_HI(6, iMONTHLY, 'CV of minimum monthly flows', rZERO, lFALSE, int(B'10110001')), & ! 13
      T_HI(6, iMONTHLY, 'Mean minimum daily flow / mean median annual flow', rZERO, lFALSE, int(B'00011101')), & ! 14
      T_HI(6, iANNUAL, 'Mean minimum annual flow / mean annual flow', rZERO, lFALSE, int(B'11000001')), & ! 15
      T_HI(6, iANNUAL, 'Median minimum annual flow / median annual flow', rZERO, lFALSE, int(B'01001101')), & ! 16
      T_HI(6, iANNUAL, '7-day minimum flow / mean annual flow', rZERO, lFALSE, int(B'00000111')), & ! 17
      T_HI(6, iANNUAL, 'CV of ( 7-day minimum flow / mean annual flow )', rZERO, lFALSE, int(B'00001011')), & ! 18
      T_HI(6, iANNUAL, 'Mean of (minimum annual flow / mean annual flow ) * 100', rZERO, lFALSE, int(B'00000001')), & ! 19
      T_HI(6, iANNUAL, 'Ratio of baseflow volume to total flow volume', rZERO, lFALSE, int(B'00000001')), & ! 20
      T_HI(6, iANNUAL, 'CV of annual minimum flows', rZERO, lFALSE, int(B'00000011')), & ! 21
      T_HI(3, iANNUAL, 'Mean annual minimum flow divided by catchment area', rZERO, lFALSE, int(B'01100001')) & ! 22
      ]

    type(T_HI), dimension(27) :: MH = [ &
      T_HI(1, iMONTHLY, 'Mean maximum monthly flow, January', rZERO, lFALSE, int(B'00100001')), & ! 1
      T_HI(1, iMONTHLY, 'Mean maximum monthly flow, February', rZERO, lFALSE, int(B'00000001')), & ! 2
      T_HI(1, iMONTHLY, 'Mean maximum monthly flow, March', rZERO, lFALSE, int(B'00000001')), & ! 3
      T_HI(1, iMONTHLY, 'Mean maximum monthly flow, April', rZERO, lFALSE, int(B'01000001')), & ! 4
      T_HI(1, iMONTHLY, 'Mean maximum monthly flow, May', rZERO, lFALSE, int(B'00000001')), & ! 5
      T_HI(1, iMONTHLY, 'Mean maximum monthly flow, June', rZERO, lFALSE, int(B'00000001')), & ! 6
      T_HI(1, iMONTHLY, 'Mean maximum monthly flow, July', rZERO, lFALSE, int(B'01000001')), & ! 7
      T_HI(1, iMONTHLY, 'Mean maximum monthly flow, August', rZERO, lFALSE, int(B'00000111')), & ! 8
      T_HI(1, iMONTHLY, 'Mean maximum monthly flow, September', rZERO, lFALSE, int(B'10000001')), & ! 9
      T_HI(1, iMONTHLY, 'Mean maximum monthly flow, October', rZERO, lFALSE, int(B'00001011')), & ! 10
      T_HI(1, iMONTHLY, 'Mean maximum monthly flow, November', rZERO, lFALSE, int(B'00000001')), & ! 11
      T_HI(1, iMONTHLY, 'Mean maximum monthly flow, December', rZERO, lFALSE, int(B'00000001')), & ! 12
      T_HI(6, iMONTHLY, 'CV of maximum monthly flows', rZERO, lFALSE, int(B'00000001')), & ! 13
      T_HI(6, iANNUAL, 'Median maximum annual flow / median annual flow', rZERO, lFALSE, int(B'11000111')), & ! 14
      T_HI(6, iANNUAL, 'Mean of Q1 values / median daily flow across all years', rZERO, lFALSE, int(B'00000001')), & ! 15
      T_HI(6, iANNUAL, 'Mean of Q10 values / median daily flow across all years', rZERO, lFALSE, int(B'00000011')), & ! 16
      T_HI(6, iANNUAL, 'Mean of Q25 values / median daily flow across all years', rZERO, lFALSE, int(B'00011001')), & ! 17
      T_HI(6, iANNUAL, 'CV of logarithmic annual maximum flows', rZERO, lFALSE, int(B'00000001')), & ! 18
      T_HI(6, iANNUAL, 'Skewness in annual maximum flows', rZERO, lFALSE, int(B'00001001')), & ! 19
      T_HI(6, iANNUAL, 'Mean annual maximum flow / catchment area', rZERO, lFALSE, int(B'00110001')), & ! 20
      T_HI(6, iANNUAL, 'High-flow volume (thresh = 1 * median annual flow)', rZERO, lFALSE, int(B'00000001')), & ! 21
      T_HI(6, iANNUAL, 'High-flow volume (thresh= 3 * median annual flow)', rZERO, lFALSE, int(B'00000001')), & ! 22
      T_HI(6, iANNUAL, 'High-flow volume (thresh = 7 * median annual flow)', rZERO, lFALSE, int(B'11000101')), & ! 23
      T_HI(6, iANNUAL, 'Maximum peak flow / median flow (thresh = 1 * median annual flow)', rZERO, lFALSE, int(B'00000001')), & ! 24
      T_HI(6, iANNUAL, 'Maximum peak flow / median flow (thresh = 3 * median annual flow)', rZERO, lFALSE, int(B'00000001')), & ! 25
      T_HI(6, iANNUAL, 'Maximum peak flow / median flow (thresh = 7 * median annual flow)', rZERO, lFALSE, int(B'00000001')), & ! 26
      T_HI(6, iANNUAL, 'Maximum peak flow / median flow (threshold = Q25)', rZERO, lFALSE, int(B'00000001')) & ! 27
      ]

    type(T_HI), dimension(3) :: FL = [ &
      T_HI(5, iANNUAL, 'Annual low flow pulse count; number of periods < 25th percentile', rZERO, lFALSE, int(B'11001011')), & ! 1
      T_HI(6, iANNUAL, 'CV of low flow pulse count', rZERO, lFALSE, int(B'11111111')), & ! 2
      T_HI(5, iANNUAL, 'Num. low flow spells (< 5% of mean flow) / record length (yrs)', rZERO, lFALSE, int(B'11111111')) & ! 3
      ]

    type(T_HI), dimension(11) :: FH = [ &
      T_HI(5, iANNUAL, 'Annual high flow pulse count; number of periods > 75th percentile', rZERO, lFALSE, int(B'00000001')), & ! 1
      T_HI(6, iANNUAL, 'CV of high flow pulse count', rZERO, lFALSE, int(B'11000011')), & ! 2
      T_HI(5, iANNUAL, 'Num. high flow spells (>3 * median annual flow)', rZERO, lFALSE, int(B'01011011')), & ! 3
      T_HI(5, iANNUAL, 'Num. high flow spells (>7 * median annual flow)', rZERO, lFALSE, int(B'00000101')), & ! 4
      T_HI(5, iANNUAL, 'Num. high flow spells (>1 * median annual flow) / record length (yrs)', rZERO, lFALSE, int(B'10010001')), & ! 5
      T_HI(5, iANNUAL, 'Num. high flow spells (>3 * median annual flow) / record length (yrs)', rZERO, lFALSE, int(B'00001111')), & ! 6
      T_HI(5, iANNUAL, 'Num. high flow spells (>7 * median annual flow) / record length (yrs)', rZERO, lFALSE, int(B'11000111')), & ! 7
      T_HI(5, iANNUAL, 'Num. high flow spells (>25th percentile flow) / record length (yrs)', rZERO, lFALSE, int(B'00100001')), & ! 8
      T_HI(5, iANNUAL, 'Num. high flow spells (>75th percentile flow) / record length (yrs)', rZERO, lFALSE, int(B'00000001')), & ! 9
      T_HI(5, iANNUAL, 'Num. high flow spells (>median of annual minima) / record length (yrs)', rZERO, lFALSE, int(B'01000001')), & ! 10
      T_HI(5, iANNUAL, 'Mean number of discrete flood events per year', rZERO, lFALSE, int(B'00101001')) & ! 11
      ]

    type(T_HI), dimension(20) :: DL = [ &
      T_HI(1, iDAILY, 'Annual minimum of 1-day mean of flow', rZERO, lFALSE, int(B'11000001')), & ! 1
      T_HI(1, iDAILY, 'Annual minimum of 3-day mean of flow', rZERO, lFALSE, int(B'10000001')), & ! 2
      T_HI(1, iDAILY, 'Annual minimum of 7-day mean of flow', rZERO, lFALSE, int(B'00000001')), & ! 3
      T_HI(1, iDAILY, 'Annual minimum of 30-day mean of flow', rZERO, lFALSE, int(B'00000001')), & ! 4
      T_HI(1, iDAILY, 'Annual minimum of 90-day mean of flow', rZERO, lFALSE, int(B'00100001')), & ! 5
      T_HI(6, iDAILY, 'CV, annual minimum of 1-day mean of flow', rZERO, lFALSE, int(B'00010101')), & ! 6
      T_HI(6, iDAILY, 'CV, annual minimum of 3-day mean of flow', rZERO, lFALSE, int(B'00000001')), & ! 7
      T_HI(6, iDAILY, 'CV, annual minimum of 7-day mean of flow', rZERO, lFALSE, int(B'00000001')), & ! 8
      T_HI(6, iDAILY, 'CV, annual minimum of 30-day mean of flow', rZERO, lFALSE, int(B'00001001')), & ! 9
      T_HI(6, iDAILY, 'CV, annual minimum of 90-day mean of flow', rZERO, lFALSE, int(B'00000101')), & ! 10
      T_HI(6, iDAILY, 'Mean of 1-day minimum of flow', rZERO, lFALSE, int(B'00001001')), & ! 11
      T_HI(6, iDAILY, 'Mean of 7-day minimum of flow', rZERO, lFALSE, int(B'00000001')), & ! 12
      T_HI(6, iDAILY, 'Mean of 30-day minimum of flow', rZERO, lFALSE, int(B'11010011')), & ! 13
      T_HI(6, iANNUAL, 'Mean of Q75 values / median daily flow across all years', rZERO, lFALSE, int(B'00000001')), & ! 14
      T_HI(6, iANNUAL, 'Mean of Q90 values / median daily flow across all years', rZERO, lFALSE, int(B'00000001')), & ! 15
      T_HI(4, iANNUAL, 'Low flow pulse duration (Mean duration of FL1)', rZERO, lFALSE, int(B'01101011')), & ! 16
      T_HI(6, iANNUAL, 'CV, low flow pulse duration (DL16)', rZERO, lFALSE, int(B'00000111')), & ! 17
      T_HI(5, iANNUAL, 'Mean annual number of zero-flow days', rZERO, lFALSE, int(B'01000011')), & ! 18
      T_HI(6, iANNUAL, 'CV, mean annual number of zero-flow days', rZERO, lFALSE, int(B'00000001')), & ! 19
      T_HI(6, iANNUAL, 'Percentage of all months with zero flow', rZERO, lFALSE, int(B'00000001')) & ! 20
      ]

    type(T_HI), dimension(24) :: DH = [ &
      T_HI(1, iDAILY, 'Annual maximum of 1-day mean of flow', rZERO, lFALSE, int(B'00000001')), & ! 1
      T_HI(1, iDAILY, 'Annual maximum of 3-day mean of flow', rZERO, lFALSE, int(B'00000001')), & ! 2
      T_HI(1, iDAILY, 'Annual maximum of 7-day mean of flow', rZERO, lFALSE, int(B'00000001')), & ! 3
      T_HI(1, iDAILY, 'Annual maximum of 30-day mean of flow', rZERO, lFALSE, int(B'00000001')), & ! 4
      T_HI(1, iDAILY, 'Annual maximum of 90-day mean of flow', rZERO, lFALSE, int(B'10000001')), & ! 5
      T_HI(6, iDAILY, 'CV, annual maximum of 1-day mean of flow', rZERO, lFALSE, int(B'00000001')), & ! 6
      T_HI(6, iDAILY, 'CV, annual maximum of 3-day mean of flow', rZERO, lFALSE, int(B'00000001')), & ! 7
      T_HI(6, iDAILY, 'CV, annual maximum of 7-day mean of flow', rZERO, lFALSE, int(B'00000001')), & ! 8
      T_HI(6, iDAILY, 'CV, annual maximum of 30-day mean of flow', rZERO, lFALSE, int(B'00000001')), & ! 9
      T_HI(6, iDAILY, 'CV, annual maximum of 90-day mean of flow', rZERO, lFALSE, int(B'10000001')), & ! 10
      T_HI(6, iDAILY, 'Mean of 1-day maximum of flow', rZERO, lFALSE, int(B'00001001')), & ! 11
      T_HI(6, iDAILY, 'Mean of 7-day maximum of flow', rZERO, lFALSE, int(B'01010001')), & ! 12
      T_HI(6, iDAILY, 'Mean of 30-day maximum of flow', rZERO, lFALSE, int(B'01000111')), & ! 13
      T_HI(6, iANNUAL, 'Q95 value / mean monthly flow across all years', rZERO, lFALSE, int(B'00000001')), & ! 14
      T_HI(6, iANNUAL, 'Mean duration of flood pulses > 75th percentile flow', rZERO, lFALSE, int(B'01001011')), & ! 15
      T_HI(6, iANNUAL, 'CV, mean duration of high flow pulse (FH1)', rZERO, lFALSE, int(B'00100111')), & ! 16
      T_HI(6, iANNUAL, 'Mean duration of flood pulses > 1 * median flow', rZERO, lFALSE, int(B'00000001')), & ! 17
      T_HI(6, iANNUAL, 'Mean duration of flood pulses > 3 * median flow', rZERO, lFALSE, int(B'00000001')), & ! 18
      T_HI(6, iANNUAL, 'Mean duration of flood pulses > 7 * median flow', rZERO, lFALSE, int(B'00100001')), & ! 19
      T_HI(6, iANNUAL, 'Mean duration of flood pulses > 25th percentile of median flow', rZERO, lFALSE, int(B'00001011')), & ! 20
      T_HI(6, iANNUAL, 'Mean duration of flood pulses > 75th percentile of median flow', rZERO, lFALSE, int(B'00000001')), & ! 21
      T_HI(6, iANNUAL, 'Mean annual median interval in days between floods over all years', rZERO, lFALSE, int(B'10000001')), & ! 22
      T_HI(6, iANNUAL, 'Mean annual number of days that flows > threshold over all years', rZERO, lFALSE, int(B'01000001')), & ! 23
      T_HI(6, iANNUAL, 'Mean annual maximum number of 365-day periods in which no floods occur', rZERO, lFALSE, int(B'00010101')) & ! 24
                                 ]

    type(T_HI), dimension(3) :: TA = [ &
      T_HI(6, iDAILY, 'Constancy (see Colwell, 1974)', rZERO, lFALSE, int(B'01111111')), & ! 1
      T_HI(6, iDAILY, 'Predictability of flow', rZERO, lFALSE, int(B'01000001')), & ! 2
      T_HI(6, iDAILY, 'Seasonal predictability of flooding', rZERO, lFALSE, int(B'00100101')) & ! 3
      ]

    type(T_HI), dimension(4) :: TL = [ &
      T_HI(6, iDAILY, 'Mean day-of-year of annual minimum', rZERO, lFALSE, int(B'01010001')), & ! 1
      T_HI(6, iDAILY, 'CV, day-of-year of annual minimum', rZERO, lFALSE, int(B'10001011')), & ! 2
      T_HI(6, iDAILY, 'Seasonal predictibility of low flow', rZERO, lFALSE, int(B'00000001')), & ! 3
      T_HI(6, iDAILY, 'Seasonal predictibility of non-low flow', rZERO, lFALSE, int(B'00000001')) & ! 4
      ]

    type(T_HI), dimension(3) :: TH = [ &
      T_HI(6, iDAILY, 'Mean day-of-year of annual maximum', rZERO, lFALSE, int(B'10001001')), & ! 1
      T_HI(6, iDAILY, 'CV, day-of-year of annual maximum', rZERO, lFALSE, int(B'10000001')), & ! 2
      T_HI(6, iDAILY, 'Seasonal predictibility of non-flooding', rZERO, lFALSE, int(B'01000111')) & ! 3
      ]

    type(T_HI), dimension(9) :: RA = [ &
      T_HI(7, iDAILY, 'Mean of positive changes from one day to next (rise rate)', rZERO, lFALSE, int(B'10100001')), & ! 1
      T_HI(6, iDAILY, 'CV, mean of positive changes from one day to next (rise rate)', rZERO, lFALSE, int(B'00000001')), & ! 2
      T_HI(7, iDAILY, 'Mean of negative changes from one day to next (fall rate)', rZERO, lFALSE, int(B'00000001')), & ! 3
      T_HI(6, iDAILY, 'CV, mean of negative changes from one day to next (fall rate)', rZERO, lFALSE, int(B'10000001')), & ! 4
      T_HI(6, iDAILY, 'Ratio of days that are higher than previous day', rZERO, lFALSE, int(B'11001011')), & ! 5
      T_HI(1, iDAILY, 'Median of difference in log of flows over two consecutive days of rising flow', rZERO, lFALSE, int(B'01000111')), & ! 6
      T_HI(1, iDAILY, 'Median of difference in log of flows over two consecutive days of falling flow', rZERO, lFALSE, int(B'01000101')), & ! 7
      T_HI(1, iDAILY, 'Number of flow reversals from one day to the next', rZERO, lFALSE, int(B'00111011')), & ! 8
      T_HI(1, iDAILY, 'CV, number of flow reversals from one day to the next', rZERO, lFALSE, int(B'01011111')) & ! 9
      ]

    character(len=23), dimension(0:7), parameter :: STREAM_CLASSIFICATIONS = [ &
                                                    'All indices            ', & ! 0
                                                    'All streams            ', & ! 1
                                                    'Flashy perennial       ', & ! 2
                                                    'Groundwater perennial  ', & ! 3
                                                    'Snow and rain perennial', & ! 4
                                                    'Snowmelt perennial     ', & ! 5
                                                    'Flashy intermittent    ', & ! 6
                                                    'Harsh intermittent     ' & ! 7
                                                    ]

contains

    subroutine erase_entity(ifail)

! -- Subroutine ERASE_ENTITY removes a TSPROC entity from memory.

        use tsp_data_structures
        use tsp_utilities
        use tsp_command_processors

        implicit none

        integer, intent(out) :: ifail

        integer iseries, icontext, ierr, istable, ivtable, idtable, j, is, ixcon, &
            ictable, ic
        integer eseries(MAXSERIES), evtable(MAXVTABLE), estable(MAXSTABLE), &
            edtable(MAXDTABLE), ectable(MAXCTABLE)
        character(20) aline
        character(25) aoption
        character(25) acontext(MAXCONTEXT)

        ifail = 0
        icontext = 0
        iseries = 0
        istable = 0
        ivtable = 0
        idtable = 0
        ictable = 0
        CurrentBlock_g = 'ERASE_ENTITY'
        ixcon = 0

        write (*, 10) TRIM(CurrentBlock_g)
        write (LU_REC, 10) TRIM(CurrentBlock_g)
10      format(/, ' Processing ', a, ' block....')

        do
            ILine_g = ILine_g + 1
            read (LU_TSPROC_CONTROL, '(a)', err=9000, end=9100) cline
            if (cline == ' ') cycle
            if (cline(1:1) == '#') cycle
            call linesplit(ierr, 2)
            if (ierr /= 0) then
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 20) TRIM(aline), TRIM(sString_g)
20              format('there should be 2 entries on line ', a, ' of file ', a)
                go to 9800
            end if
            aoption = cline(left_word(1):right_word(1))
            call casetrans(aoption, 'hi')
            if (aoption /= 'CONTEXT') then
                call test_context(ierr, icontext, acontext)
                if (ierr == -1) then
                    call find_end(ifail)
                    if (ifail == 1) go to 9800
                    return
                else if (ierr == 1) then
                    go to 9800
                end if
                ixcon = 1
            end if
            if (aoption == 'SERIES_NAME') then
                iseries = iseries + 1
                if (iseries > MAXSERIES) then
                    call num2char(MAXSERIES, aline)
                    write (amessage, 100) TRIM(aline)
100                 format('a maximum of ', a, ' series can be cited in an ERASE_ENTITY block.')
                    go to 9800
                end if
                call get_series_name(ierr, eseries(iseries), 'SERIES_NAME')
                if (ierr /= 0) go to 9800
            else if (aoption == 'S_TABLE_NAME') then
                istable = istable + 1
                if (istable > MAXSTABLE) then
                    call num2char(MAXSTABLE, aline)
                    write (amessage, 102) TRIM(aline)
102                 format('a maximum of ', a, ' s_tables can be cited in an ERASE_ENTITY block.')
                    go to 9800
                end if
                call get_table_name(ierr, estable(istable), 1)
                if (ierr /= 0) go to 9800
            else if (aoption == 'C_TABLE_NAME') then
                ictable = ictable + 1
                if (ictable > MAXCTABLE) then
                    call num2char(MAXCTABLE, aline)
                    write (amessage, 110) TRIM(aline)
110                 format('a maximum of ', a, ' c_tables can be cited in an ERASE_ENTITY block.')
                    go to 9800
                end if
                call get_table_name(ierr, ectable(ictable), 4)
                if (ierr /= 0) go to 9800
            else if (aoption == 'V_TABLE_NAME') then
                ivtable = ivtable + 1
                if (ivtable > MAXVTABLE) then
                    call num2char(MAXVTABLE, aline)
                    write (amessage, 103) TRIM(aline)
103                 format('a maximum of ', a, ' v_tables can be cited in an ERASE_ENTITY block.')
                    go to 9800
                end if
                call get_table_name(ierr, evtable(ivtable), 2)
                if (ierr /= 0) go to 9800
            else if (aoption == 'E_TABLE_NAME') then
                idtable = idtable + 1
                if (idtable > MAXDTABLE) then
                    call num2char(MAXDTABLE, aline)
                    write (amessage, 104) TRIM(aline)
104                 format('a maximum of ', a, ' e_tables can be cited in an ERASE_ENTITY block.')
                    go to 9800
                end if
                call get_table_name(ierr, edtable(idtable), 3)
                if (ierr /= 0) go to 9800
            else if (aoption == 'CONTEXT') then
                if (ixcon /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 41) TRIM(aline), TRIM(sString_g)
41                  format('CONTEXT keyword in incorrect location at line ', a, ' of file ', a)
                    go to 9800
                end if
                call get_context(ierr, icontext, acontext)
                if (ierr /= 0) go to 9800
            else if (aoption == 'END') then
                go to 200
            else
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 180) TRIM(aoption), TRIM(aline), TRIM(sString_g)
180             format('unexpected keyword - "', a, '" in ERASE_ENTITY block at line ', a, &
                       ' of file ', a)
                go to 9800
            end if
        end do

! -- The block has been read; now it is checked for correctness.

200     continue
        if ((iseries == 0) .AND. (istable == 0) .AND. (ivtable == 0) .AND. &
            (idtable == 0) .AND. (ictable == 0)) then
            write (amessage, 210)
210         format('no series or tables have been named for deletion in ERASE_ENTITY block.')
            go to 9800
        end if
        if (icontext == 0) then
            write (amessage, 220)
220         format('no CONTEXT keyword(s) provided in ERASE_ENTITY block.')
            go to 9800
        end if

        if (iseries == 0) go to 300
        do j = 1, iseries
            is = eseries(j)
            deallocate (series_g(is)%days, series_g(is)%secs, series_g(is)%val, stat=ierr)
            if (ierr /= 0) then
                write (amessage, 230)
230             format('cannot de-allocate memory previously allocated to erased time series.')
                go to 9800
            end if
!         nullify(series_g(is)%days,series_g(is)%secs,series_g(is)%val)
            series_g(is)%active = .FALSE.
            series_g(is)%nterm = 0
            series_g(is)%type = ' '
            write (*, 250) TRIM(series_g(is)%name)
            write (LU_REC, 250) TRIM(series_g(is)%name)
250         format(t5, 'Series "', a, '" erased.')
            series_g(is)%name = ' '
        end do

300     continue
        if (istable == 0) go to 350
        do j = 1, istable
            is = estable(j)
            stable_g(is)%active = .FALSE.
            write (*, 320) TRIM(stable_g(is)%name)
            write (LU_REC, 320) TRIM(stable_g(is)%name)
320         format(t5, 's_table "', a, '" erased.')
            stable_g(is)%name = ' '
        end do

350     continue
        if (ictable == 0) go to 400
        do j = 1, ictable
            ic = ectable(j)
            ctable_g(ic)%active = .FALSE.
            write (*, 321) TRIM(ctable_g(ic)%name)
            write (LU_REC, 321) TRIM(ctable_g(ic)%name)
321         format(t5, 'c_table "', a, '" erased.')
            ctable_g(ic)%name = ' '
        end do

400     continue
        if (ivtable == 0) go to 500
        do j = 1, ivtable
            is = evtable(j)
            vtable_g(is)%active = .FALSE.
            deallocate (vtable_g(is)%days1, vtable_g(is)%days2, vtable_g(is)%secs1, &
                        vtable_g(is)%secs2, vtable_g(is)%vol, stat=ierr)
            if (ierr /= 0) then
                write (amessage, 420)
420             format('cannot de-allocate memory previously allocated to erased V_TABLE.')
                go to 9800
            end if
            nullify (vtable_g(is)%days1, vtable_g(is)%days2, vtable_g(is)%secs1, &
                     vtable_g(is)%secs2, vtable_g(is)%vol)
            vtable_g(is)%nterm = 0
            vtable_g(is)%series_name = ' '
            write (*, 430) TRIM(vtable_g(is)%name)
            write (LU_REC, 430) TRIM(vtable_g(is)%name)
430         format(t5, 'v_table "', a, '" erased.')
            vtable_g(is)%name = ' '
        end do

500     continue
        if (idtable == 0) go to 600
        do j = 1, idtable
            is = edtable(j)
            dtable_g(is)%active = .FALSE.
            deallocate (dtable_g(is)%flow, dtable_g(is)%time, dtable_g(is)%tdelay, stat=ierr)
            if (ierr /= 0) then
                write (amessage, 520)
520             format('cannot de-allocate memory previously allocated to erased E_TABLE.')
                go to 9800
            end if
            nullify (dtable_g(is)%time, dtable_g(is)%flow, dtable_g(is)%tdelay)
            dtable_g(is)%nterm = 0
            dtable_g(is)%series_name = ' '
            write (*, 521) TRIM(dtable_g(is)%name)
            write (LU_REC, 521) TRIM(dtable_g(is)%name)
521         format(t5, 'e_table "', a, '" erased.')
            dtable_g(is)%name = ' '
        end do
600     continue
        return

9000    call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9010) TRIM(aline), TRIM(sString_g)
9010    format('cannot read line ', a, ' of TSPROC input file ', a)
        go to 9800
9100    continue
        call addquote(sInfile_g, sString_g)
        write (amessage, 9110) TRIM(sString_g), TRIM(CurrentBlock_g)
9110    format('unexpected end encountered to TSPROC input file ', a, ' while ', &
               ' reading ', a, ' block.')
        go to 9800

9800    call write_message(leadspace='yes', error='yes')
        call write_message(iunit=LU_REC, leadspace='yes')
        ifail = 1

        return

    end subroutine erase_entity

    subroutine moving_window(ifail)

! -- Subroutine MOVING is still quick and dirty. It calculates the minimum sample
!    value within a window consisting of an odd number of terms.

        use tsp_data_structures
        use tsp_utilities
        use tsp_command_processors

        implicit none

        integer, intent(out) :: ifail

        integer icontext, iseries, ixcon, ierr, itemp, iterm, i, j, wt2, l, winterms, imode, &
            icount, is, ie, iiterm, k
        real rtemp, first_value, last_value
        character(len=iTSNAMELENGTH) :: aname
        character(15) aline, amode
        character(25) aoption
        character(25) acontext(MAXCONTEXT)

        ifail = 0
        CurrentBlock_g = 'MOVING_MINIMUM'

        write (*, 10) TRIM(CurrentBlock_g)
        write (LU_REC, 10) TRIM(CurrentBlock_g)
10      format(/, ' Processing ', a, ' block....')

        icontext = 0
        iseries = 0
        aname = ' '
        winterms = -99999999
        ixcon = 0
        amode = ' '
        first_value = -1.1E30
        last_value = -1.1E30

! -- The MOVING_MINIMUM block is first parsed.

        do
            ILine_g = ILine_g + 1
            read (LU_TSPROC_CONTROL, '(a)', err=9000, end=9100) cline
            if (cline == ' ') cycle
            if (cline(1:1) == '#') cycle
            call linesplit(ierr, 2)
            if (ierr /= 0) then
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 20) TRIM(aline), TRIM(sString_g)
20              format('there should be 2 entries on line ', a, ' of file ', a)
                go to 9800
            end if
            aoption = cline(left_word(1):right_word(1))
            call casetrans(aoption, 'hi')
            if (aoption /= 'CONTEXT') then
                call test_context(ierr, icontext, acontext)
                if (ierr == -1) then
                    call find_end(ifail)
                    if (ifail == 1) go to 9800
                    return
                else if (ierr == 1) then
                    go to 9800
                end if
                ixcon = 1
            end if
            if (aoption == 'CONTEXT') then
                if (ixcon /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 41) TRIM(aline), TRIM(sString_g)
41                  format('CONTEXT keyword in incorrect location at line ', a, ' of file ', a)
                    go to 9800
                end if
                call get_context(ierr, icontext, acontext)
                if (ierr /= 0) go to 9800
            else if (aoption == 'TERMS_IN_WINDOW') then
                call get_keyword_value(ierr, 1, winterms, rtemp, aoption)
                if (ierr /= 0) go to 9800
            else if (aoption == 'FIRST_VALUE') then
                call get_keyword_value(ierr, 2, itemp, first_value, aoption)
                if (ierr /= 0) go to 9800
            else if (aoption == 'LAST_VALUE') then
                call get_keyword_value(ierr, 2, itemp, last_value, aoption)
                if (ierr /= 0) go to 9800
            else if (aoption == 'NEW_SERIES_NAME') then
                call get_new_series_name(ierr, aname)
                if (ierr /= 0) go to 9800
            else if (aoption == 'SERIES_NAME') then
                call get_series_name(ierr, iseries, 'SERIES_NAME')
                if (ierr /= 0) go to 9800
            else if (aoption == 'MODE') then
                amode = cline(left_word(2):right_word(2))
                call casetrans(amode, 'lo')
                write (*, 89) TRIM(amode)
                write (LU_REC, 89) TRIM(amode)
89              format(t5, 'MODE ', a)
            else if (aoption == 'END') then
                go to 200
            else
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 90) TRIM(aoption), TRIM(CurrentBlock_g), TRIM(aline), TRIM(sString_g)
90              format('unexpected keyword - "', a, '" in ', a, ' block at line ', a, &
                       ' of file ', a)
                go to 9800
            end if
        end do

! -- The block has been read; now it is checked for correctness.

200     continue
        if (iseries == 0) then
            write (amessage, 210) TRIM(CurrentBlock_g)
210         format('no SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (aname == ' ') then
            write (amessage, 230) TRIM(CurrentBlock_g)
230         format('no NEW_SERIES_NAME provided in ', a, ' block.')
            go to 9800
        end if
        if (amode == ' ') then
            write (amessage, 231) TRIM(CurrentBlock_g)
231         format('no MODE keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (icontext == 0) then
            write (amessage, 220) TRIM(CurrentBlock_g)
220         format('no CONTEXT keyword(s) provided in ', a, ' block.')
            go to 9800
        end if
        if (winterms == -99999999) then
            write (amessage, 225) TRIM(CurrentBlock_g)
225         format('no TERMS_IN_WINDOW keyword(s)provided in ', a, ' block.')
            go to 9800
        else
            if (winterms <= 0) then
                write (amessage, 226) TRIM(CurrentBlock_g)
226             format('value for TERMS_IN_WINDOW must be positve in ', a, ' block.')
                go to 9800
            end if
            if ((winterms / 2) * 2 == winterms) then
                write (amessage, 227) TRIM(CurrentBlock_g)
227             format('TERMS_IN_WINDOW must be an odd number in ', a, ' block.')
                go to 9800
            end if
        end if
        if (amode == 'continuous') then
            imode = 1
        else if (amode == 'discrete') then
            imode = 2
        else
            write (amessage, 228) TRIM(CurrentBlock_g)
228         format('MODE must be "discrete" or "continuous" in ', a, ' block.')
            go to 9800
        end if
        if (imode == 2) then
            if (first_value < -1.0E30) then
                write (amessage, 340) TRIM(CurrentBlock_g)
340             format('no FIRST_VALUE keyword supplied in ', a, ' block.')
                go to 9800
            end if
            if (last_value < -1.0E30) then
                write (amessage, 341) TRIM(CurrentBlock_g)
341             format('no LAST_VALUE keyword supplied in ', a, ' block.')
                go to 9800
            end if
        end if

! -- The new series is now written.

        if (imode == 2) go to 900
        iterm = series_g(iseries)%nterm
        do i = 1, MAXSERIES
            if (.NOT. series_g(i)%active) go to 515
        end do
        write (amessage, 510)
510     format('no more time series available for data storage - increase MAXSERIES and ', &
               'recompile program.')
        go to 9800

515     allocate (series_g(i)%days(iterm), series_g(i)%secs(iterm), &
                  series_g(i)%val(iterm), stat=ierr)
        if (ierr /= 0) then
            write (amessage, 550)
550         format('cannot allocate memory for another time series.')
            go to 9800
        end if
        series_g(i)%active = .TRUE.
        series_g(i)%name = aname
        series_g(i)%nterm = iterm
        series_g(i)%type = 'ts'
        do j = 1, iterm
            series_g(i)%days(j) = series_g(iseries)%days(j)
        end do
        do j = 1, iterm
            series_g(i)%secs(j) = series_g(iseries)%secs(j)
        end do
        wt2 = winterms / 2
        if (iterm <= wt2 * 2) then
            do j = 1, iterm
                series_g(i)%val(j) = series_g(iseries)%val(j)
            end do
        else
            do j = 1, wt2
                series_g(i)%val(j) = series_g(iseries)%val(j)
            end do
            itemp = iterm - wt2 + 1
            do j = itemp, iterm
                series_g(i)%val(j) = series_g(iseries)%val(j)
            end do
            do j = wt2 + 1, itemp - 1
                rtemp = 1E30
                do l = j - wt2, j + wt2
                    if (series_g(iseries)%val(l) < rtemp) rtemp = series_g(iseries)%val(l)
                end do
                series_g(i)%val(j) = rtemp
            end do
        end if

        write (*, 590) TRIM(aname)
        write (LU_REC, 590) TRIM(aname)
590     format(t5, 'Series "', a, '" successfully calculated.')
        return

900     continue

! -- The following refers to discrete mode.
!    First we find out how many terms will be required.

        icount = 0
        iterm = series_g(iseries)%nterm
        wt2 = winterms / 2
        is = wt2 + 1
        ie = iterm - wt2
        do j = is, ie
            rtemp = series_g(iseries)%val(j)
            do k = j - wt2, j + wt2
                if (j == k) cycle
                if (rtemp >= series_g(iseries)%val(k)) go to 930
            end do
            icount = icount + 1
930         continue
        end do

        do i = 1, MAXSERIES
            if (.NOT. series_g(i)%active) go to 1515
        end do
        write (amessage, 510)
        go to 9800
1515    continue
        iiterm = icount + 2
        allocate (series_g(i)%days(iiterm), series_g(i)%secs(iiterm), &
                  series_g(i)%val(iiterm), stat=ierr)
        if (ierr /= 0) then
            write (amessage, 550)
            go to 9800
        end if
        series_g(i)%active = .TRUE.
        series_g(i)%name = aname
        series_g(i)%nterm = iiterm
        series_g(i)%type = 'ts'
        series_g(i)%val(1) = first_value
        series_g(i)%days(1) = series_g(iseries)%days(1)
        series_g(i)%secs(1) = series_g(iseries)%secs(1)
        series_g(i)%val(iiterm) = last_value
        series_g(i)%days(iiterm) = series_g(iseries)%days(iterm)
        series_g(i)%secs(iiterm) = series_g(iseries)%secs(iterm)
        icount = 1
        do j = is, ie
            rtemp = series_g(iseries)%val(j)
            do k = j - wt2, j + wt2
                if (j == k) cycle
                if (rtemp >= series_g(iseries)%val(k)) go to 950
            end do
            icount = icount + 1
            series_g(i)%val(icount) = series_g(iseries)%val(j)
            series_g(i)%days(icount) = series_g(iseries)%days(j)
            series_g(i)%secs(icount) = series_g(iseries)%secs(j)
950         continue
        end do

        return

9000    call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9010) TRIM(aline), TRIM(sString_g)
9010    format('cannot read line ', a, ' of TSPROC input file ', a)
        go to 9800
9100    continue
        call addquote(sInfile_g, sString_g)
        write (amessage, 9110) TRIM(sString_g), TRIM(CurrentBlock_g)
9110    format('unexpected end encountered to TSPROC input file ', a, ' while ', &
               ' reading ', a, ' block.')
        go to 9800

9800    call write_message(leadspace='yes', error='yes')
        call write_message(iunit=LU_REC, leadspace='yes')
        ifail = 1

        return

    end subroutine moving_window

    subroutine new_series_uniform(ifail)

! -- Subroutine NEW_SERIES_UNIFORM generates a uniform, equispaced, time series.

        use tsp_data_structures
        use tsp_utilities
        use tsp_command_processors

        implicit none

        integer, intent(out) :: ifail

        integer dd1, mm1, yy1, hh1, nn1, ss1, dd2, mm2, yy2, hh2, nn2, ss2, ierr, &
            icontext, i, begdays, begsecs, enddays, endsecs, ixcon, iterm, ifac
        integer time_interval, time_unit, ival, tterm, iseries, dd, mm, yy, itemp
        real rtemp, rval
        double precision timediff, timeinc
        character(len=iTSNAMELENGTH) :: aname
        character(15) aline
        character(25) aoption
        character(30) atemp
        character(25) acontext(MAXCONTEXT)

        ifail = 0
        CurrentBlock_g = 'NEW_SERIES_UNIFORM'

        write (*, 10) TRIM(CurrentBlock_g)
        write (LU_REC, 10) TRIM(CurrentBlock_g)
10      format(/, ' Processing ', a, ' block....')

        icontext = 0
        ixcon = 0
        yy1 = -9999
        hh1 = -9999
        yy2 = -9999
        hh2 = -9999
        time_unit = -9999
        time_interval = -9999
        ival = -9999
        aname = ' '

! -- The NEW_SERIES_UNIFORM block is first parsed.

        do
            ILine_g = ILine_g + 1
            read (LU_TSPROC_CONTROL, '(a)', err=9000, end=9100) cline
            if (cline == ' ') cycle
            if (cline(1:1) == '#') cycle
            call linesplit(ierr, 2)
            if (ierr /= 0) then
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 20) TRIM(aline), TRIM(sString_g)
20              format('there should be 2 entries on line ', a, ' of file ', a)
                go to 9800
            end if
            aoption = cline(left_word(1):right_word(1))
            call casetrans(aoption, 'hi')
            if (aoption /= 'CONTEXT') then
                call test_context(ierr, icontext, acontext)
                if (ierr == -1) then
                    call find_end(ifail)
                    if (ifail == 1) go to 9800
                    return
                else if (ierr == 1) then
                    go to 9800
                end if
                ixcon = 1
            end if
            if (aoption == 'DATE_1') then
                call get_date(ierr, dd1, mm1, yy1, 'DATE_1')
                if (ierr /= 0) go to 9800
            else if (aoption == 'DATE_2') then
                call get_date(ierr, dd2, mm2, yy2, 'DATE_2')
                if (ierr /= 0) go to 9800
            else if (aoption == 'TIME_1') then
                call get_time(ierr, hh1, nn1, ss1, 'TIME_1')
                if (ierr /= 0) go to 9800
            else if (aoption == 'TIME_2') then
                call get_time(ierr, hh2, nn2, ss2, 'TIME_2')
                if (ierr /= 0) go to 9800
            else if (aoption == 'TIME_INTERVAL') then
                call get_keyword_value(ierr, 1, time_interval, rtemp, 'TIME_INTERVAL')
                if (ierr /= 0) go to 9800
                if (time_interval <= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 65) TRIM(aline), TRIM(sString_g)
65                  format('time interval must be positive at line ', a, ' of file ', a)
                    go to 9800
                end if
            else if (aoption == 'TIME_UNIT') then
                call getfile(ierr, cline, atemp, left_word(2), right_word(2))
                if (ierr /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 50) TRIM(aline), TRIM(sString_g)
50                  format('cannot read time unit from line ', a, ' of file ', a)
                    go to 9800
                end if
                call casetrans(atemp, 'lo')
                if (atemp(1:3) == 'sec') then
                    time_unit = 1
                    atemp = 'seconds'
                else if (atemp(1:3) == 'min') then
                    time_unit = 2
                    atemp = 'minutes'
                else if (atemp(1:3) == 'hou') then
                    time_unit = 3
                    atemp = 'hours'
                else if (atemp(1:3) == 'day') then
                    time_unit = 4
                    atemp = 'days'
                else if (atemp(1:3) == 'mon') then
                    time_unit = 5
                    atemp = 'months'
                else if (atemp(1:3) == 'yea') then
                    time_unit = 6
                    atemp = 'years'
                else
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 51) TRIM(aline), TRIM(sString_g)
51                  format('illegal time unit at line ', a, ' of file ', a)
                    go to 9800
                end if
                write (*, 54) TRIM(atemp)
                write (LU_REC, 54) TRIM(atemp)
54              format(t5, 'TIME UNIT ', a)
            else if (aoption == 'NEW_SERIES_NAME') then
                call get_new_series_name(ierr, aname)
                if (ierr /= 0) go to 9800
            else if (aoption == 'CONTEXT') then
                if (ixcon /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 41) TRIM(aline), TRIM(sString_g)
41                  format('CONTEXT keyword in incorrect location at line ', a, ' of file ', a)
                    go to 9800
                end if
                call get_context(ierr, icontext, acontext)
                if (ierr /= 0) go to 9800
            else if (aoption == 'NEW_SERIES_VALUE') then
                call get_keyword_value(ierr, 2, itemp, rval, 'NEW_SERIES_VALUE')
                if (ierr /= 0) go to 9800
                ival = 1
            else if (aoption == 'END') then
                go to 100
            else
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 80) TRIM(aoption), TRIM(CurrentBlock_g), TRIM(aline), TRIM(sString_g)
80              format('unexpected keyword - "', a, '" in ', a, ' block at line ', a, &
                       ' of file ', a)
                go to 9800
            end if
        end do

! -- If there are any absences in the GET_MUL_SERIES_SSF block, these are now reported.

100     continue
        if (icontext == 0) then
            call addquote(sInfile_g, sString_g)
            write (amessage, 122) TRIM(CurrentBlock_g), TRIM(sString_g)
122         format('no CONTEXT keyword provided in ', a, ' block in file ', a)
            go to 9800
        end if
        if (ival /= 1) then
            write (amessage, 123) TRIM(CurrentBlock_g), TRIM(sString_g)
123         format('no NEW_SERIES_VALUE keyword provided in ', a, ' block in file ', a)
            go to 9800
        end if
        if (time_unit == -9999) then
            write (amessage, 124) TRIM(CurrentBlock_g), TRIM(sString_g)
124         format('no TIME_UNIT keyword provided in ', a, ' block in file ', a)
            go to 9800
        end if
        if (aname == ' ') then
            write (amessage, 129) TRIM(CurrentBlock_g), TRIM(sString_g)
129         format('no NEW_SERIES_NAME keyword provided in ', a, ' block in file ', a)
            go to 9800
        end if
        if (time_interval == -9999) then
            write (amessage, 125) TRIM(CurrentBlock_g), TRIM(sString_g)
125         format('no TIME_INTERVAL keyword provided in ', a, ' block in file ', a)
            go to 9800
        end if
        if ((yy1 == -9999) .OR. (hh1 == -9999) .OR. (yy2 == -9999) .OR. (hh2 == -9999)) then
            write (amessage, 126) TRIM(CurrentBlock_g), TRIM(sString_g)
126         format('all of DATE_1, TIME_1, DATE_2, TIME_2 keywords must be provided in ', a, &
                   ' block in file ', a)
            go to 9800
        end if
        call date_check(ierr, yy1, mm1, dd1, hh1, nn1, ss1, yy2, mm2, dd2, hh2, nn2, ss2, &
                        begdays, begsecs, enddays, endsecs)
        if (ierr /= 0) go to 9800
        if (begsecs >= 86400) then
            begsecs = begsecs - 86400
            begdays = begdays + 1
        end if
        if (endsecs >= 86400) then
            endsecs = endsecs - 86400
            enddays = enddays + 1
        end if

! -- Some errors are checked for.

        if (time_unit == 5) then
            if (dd1 > 28) then
                write (amessage, 82)
82              format('if TIME_UNIT is set to "month" then the day in DATE_1 must ', &
                       'not be greater than 28.')
                go to 9800
            end if
        end if
        if (time_unit == 6) then
            if ((dd1 > 28) .AND. (mm1 == 2)) then
                write (amessage, 83)
83              format('if TIME_UNIT is set to "year" then DATE_1 must not be ', &
                       '28th or 29th February.')
                go to 9800
            end if
        end if

! -- Roughly the number of terms in the series is now evaluated so that memory can
!    be allocated for the temporary series.

        timediff = DBLE(enddays - begdays) * 86400.0D0 + DBLE(endsecs - begsecs)
        timeinc = DBLE(time_interval)
        if (time_unit == 1) then
            timeinc = timeinc * 1.0D0
        else if (time_unit == 2) then
            timeinc = timeinc * 60.0D0
        else if (time_unit == 3) then
            timeinc = timeinc * 3600.0D0
        else if (time_unit == 4) then
            timeinc = timeinc * 86400.0D0
        else if (time_unit == 5) then
            timeinc = timeinc * 28.0D0 * 86400.0D0
        else if (time_unit == 6) then
            timeinc = timeinc * 365.0D0 * 86400.0D0
        end if
        tterm = CEILING(timediff / timeinc) + 20

! -- The temporary series is allocated.

        call alloc_tempseries(ierr, tterm)
        if (ierr /= 0) go to 9800

! -- Terms of the temporary series are now created.

        if (time_unit <= 3) then
            ifac = 1
            if (time_unit == 2) then
                ifac = 60
            else if (time_unit == 3) then
                ifac = 3600
            end if
            tempseries_g%days(1) = begdays
            tempseries_g%secs(1) = begsecs
            i = 1
            do
                i = i + 1
                tempseries_g%days(i) = tempseries_g%days(i - 1)
                tempseries_g%secs(i) = tempseries_g%secs(i - 1) + time_interval * ifac
120             continue
                if (tempseries_g%secs(i) >= 86400) then
                    tempseries_g%secs(i) = tempseries_g%secs(i) - 86400
                    tempseries_g%days(i) = tempseries_g%days(i) + 1
                    go to 120
                end if
                if ((tempseries_g%days(i) > enddays) .OR. &
                    ((tempseries_g%days(i) == enddays) .AND. (tempseries_g%secs(i) > endsecs))) then
                    iterm = i - 1
                    go to 500
                end if
            end do
        else if (time_unit == 4) then
            tempseries_g%days(1) = begdays
            tempseries_g%secs(1) = begsecs
            i = 1
            do
                i = i + 1
                tempseries_g%days(i) = tempseries_g%days(i - 1) + time_interval
                tempseries_g%secs(i) = tempseries_g%secs(i - 1)
                if ((tempseries_g%days(i) > enddays) .OR. &
                    ((tempseries_g%days(i) == enddays) .AND. (tempseries_g%secs(i) > endsecs))) then
                    iterm = i - 1
                    go to 500
                end if
            end do
        else if (time_unit == 5) then
            tempseries_g%days(1) = begdays
            tempseries_g%secs(1) = begsecs
            dd = dd1
            mm = mm1
            yy = yy1
            i = 1
            do
                i = i + 1
                mm = mm + time_interval
180             continue
                if (mm > 12) then
                    mm = mm - 12
                    yy = yy + 1
                    go to 180
                end if
!           tempseries_g%days(i)=numdays(1,1,1970,dd,mm,yy)
                tempseries_g%days(i) = julian_day(iMonth=mm, iDay=dd, iYear=yy)
                tempseries_g%secs(i) = tempseries_g%secs(i - 1)
                if ((tempseries_g%days(i) > enddays) .OR. &
                    ((tempseries_g%days(i) == enddays) .AND. (tempseries_g%secs(i) > endsecs))) then
                    iterm = i - 1
                    go to 500
                end if
            end do
        else if (time_unit == 6) then
            tempseries_g%days(1) = begdays
            tempseries_g%secs(1) = begsecs
            dd = dd1
            mm = mm1
            yy = yy1
            i = 1
            do
                i = i + 1
                yy = yy + time_interval
!           tempseries_g%days(i)=numdays(1,1,1970,dd,mm,yy)
                tempseries_g%days(i) = julian_day(iMonth=mm, iDay=dd, iYear=yy)
                tempseries_g%secs(i) = tempseries_g%secs(i - 1)
                if ((tempseries_g%days(i) > enddays) .OR. &
                    ((tempseries_g%days(i) == enddays) .AND. (tempseries_g%secs(i) > endsecs))) then
                    iterm = i - 1
                    go to 500
                end if
            end do
        end if

! -- The series has been generated and is now copied from the temporary series.

500     continue

        do iseries = 1, MAXSERIES
            if (series_g(iseries)%active) cycle
            go to 510
        end do
510     continue
        allocate (series_g(iseries)%days(iterm), series_g(iseries)%secs(iterm), &
                  series_g(iseries)%val(iterm), stat=ierr)
        if (ierr /= 0) then
            write (amessage, 550)
550         format('cannot allocate memory for another time series.')
            go to 9800
        end if
        series_g(iseries)%active = .TRUE.
        series_g(iseries)%name = aname
        series_g(iseries)%type = 'ts'
        series_g(iseries)%nterm = iterm
        do i = 1, iterm
            series_g(iseries)%days(i) = tempseries_g%days(i)
            series_g(iseries)%secs(i) = tempseries_g%secs(i)
            series_g(iseries)%val(i) = rval
        end do

        write (*, 580) TRIM(aname)
        write (LU_REC, 580) TRIM(aname)
580     format(t5, 'Series "', a, '" successfully created.')

        go to 9900

9000    call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9010) TRIM(aline), TRIM(sString_g)
9010    format('cannot read line ', a, ' of TSPROC input file ', a)
        go to 9800
9100    continue
        call addquote(sInfile_g, sString_g)
        write (amessage, 9110) TRIM(sString_g), TRIM(CurrentBlock_g)
9110    format('unexpected end encountered to TSPROC input file ', a, ' while ', &
               ' reading ', a, ' block.')
        go to 9800

9800    call write_message(leadspace='yes', error='yes')
        call write_message(iunit=LU_REC, leadspace='yes')
        ifail = 1

9900    continue
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

        integer, intent(out) :: ifail

        integer icontext, iseries, ixcon, ierr, iterm, i, j
        character(len=iTSNAMELENGTH) :: aname
        character(15) aline
        character(25) aoption
        character(25) acontext(MAXCONTEXT)

        ifail = 0
        CurrentBlock_g = 'SERIES_DIFFERENCE'

        write (*, 10) TRIM(CurrentBlock_g)
        write (LU_REC, 10) TRIM(CurrentBlock_g)
10      format(/, ' Processing ', a, ' block....')

        icontext = 0
        iseries = 0
        aname = ' '
        ixcon = 0

! -- The CLEAN_SERIES block is first parsed.

        do
            ILine_g = ILine_g + 1
            read (LU_TSPROC_CONTROL, '(a)', err=9000, end=9100) cline
            if (cline == ' ') cycle
            if (cline(1:1) == '#') cycle
            call linesplit(ierr, 2)
            if (ierr /= 0) then
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 20) TRIM(aline), TRIM(sString_g)
20              format('there should be 2 entries on line ', a, ' of file ', a)
                go to 9800
            end if
            aoption = cline(left_word(1):right_word(1))
            call casetrans(aoption, 'hi')
            if (aoption /= 'CONTEXT') then
                call test_context(ierr, icontext, acontext)
                if (ierr == -1) then
                    call find_end(ifail)
                    if (ifail == 1) go to 9800
                    return
                else if (ierr == 1) then
                    go to 9800
                end if
                ixcon = 1
            end if
            if (aoption == 'CONTEXT') then
                if (ixcon /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 41) TRIM(aline), TRIM(sString_g)
41                  format('CONTEXT keyword in incorrect location at line ', a, ' of file ', a)
                    go to 9800
                end if
                call get_context(ierr, icontext, acontext)
                if (ierr /= 0) go to 9800
            else if (aoption == 'NEW_SERIES_NAME') then
                call get_new_series_name(ierr, aname)
                if (ierr /= 0) go to 9800
            else if (aoption == 'SERIES_NAME') then
                call get_series_name(ierr, iseries, 'SERIES_NAME')
                if (ierr /= 0) go to 9800
            else if (aoption == 'END') then
                go to 200
            else
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 90) TRIM(aoption), TRIM(CurrentBlock_g), TRIM(aline), TRIM(sString_g)
90              format('unexpected keyword - "', a, '" in ', a, ' block at line ', a, &
                       ' of file ', a)
                go to 9800
            end if
        end do

! -- The block has been read; now it is checked for correctness.

200     continue
        if (iseries == 0) then
            write (amessage, 210) TRIM(CurrentBlock_g)
210         format('no SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (icontext == 0) then
            write (amessage, 220) TRIM(CurrentBlock_g)
220         format('no CONTEXT keyword(s) provided in ', a, ' block.')
            go to 9800
        end if
        if (series_g(iseries)%nterm == 1) then
            write (amessage, 213) TRIM(series_g(iseries)%name), TRIM(CurrentBlock_g)
213         format('specified series "', a, '" must have more than one term in ', a, ' block.')
            go to 9800
        end if
        if (aname == ' ') then
            write (amessage, 214) TRIM(CurrentBlock_g)
214         format('no NEW_SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if

! -- Space is allocated for the new time series.

        iterm = series_g(iseries)%nterm - 1
        do i = 1, MAXSERIES
            if (.NOT. series_g(i)%active) go to 515
        end do
        write (amessage, 510)
510     format('no more time series available for data storage - increase MAXSERIES and ', &
               'recompile program.')
        go to 9800

515     allocate (series_g(i)%days(iterm), series_g(i)%secs(iterm), &
                  series_g(i)%val(iterm), stat=ierr)
        if (ierr /= 0) then
            write (amessage, 550)
550         format('cannot allocate memory for another time series.')
            go to 9800
        end if
        series_g(i)%active = .TRUE.
        series_g(i)%name = aname
        series_g(i)%nterm = iterm
        series_g(i)%type = 'ts'
        do j = 2, series_g(iseries)%nterm
            series_g(i)%days(j - 1) = series_g(iseries)%days(j)
            series_g(i)%secs(j - 1) = series_g(iseries)%secs(j)
            series_g(i)%val(j - 1) = series_g(iseries)%val(j) - series_g(iseries)%val(j - 1)
        end do

        write (*, 590) TRIM(aname)
        write (LU_REC, 590) TRIM(aname)
590     format(t5, 'Series "', a, '" successfully calculated.')
        return

9000    call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9010) TRIM(aline), TRIM(sString_g)
9010    format('cannot read line ', a, ' of TSPROC input file ', a)
        go to 9800
9100    continue
        call addquote(sInfile_g, sString_g)
        write (amessage, 9110) TRIM(sString_g), TRIM(CurrentBlock_g)
9110    format('unexpected end encountered to TSPROC input file ', a, ' while ', &
               ' reading ', a, ' block.')
        go to 9800

9800    call write_message(leadspace='yes', error='yes')
        call write_message(iunit=LU_REC, leadspace='yes')
        ifail = 1

        return

    end subroutine series_difference

    subroutine series_base_level(ifail)

! -- Subroutine SERIES_BASE_LEVEL subtracts a constant amount from a series, this
!    amount being an element of the same or another series.

        use tsp_data_structures
        use tsp_utilities
        use tsp_command_processors

        implicit none

        integer, intent(out) :: ifail

        integer icontext, iseries, ixcon, ierr, isub, jseries, bseries, ddb, mmb, yyb, &
            hhb, nnb, ssb, daysb, secsb, iterm, i, j, ineg
        real rbase
        character(len=iTSNAMELENGTH) :: aname, atemp
        character(15) aline
        character(25) aoption
        character(25) acontext(MAXCONTEXT)

        ifail = 0
        CurrentBlock_g = 'SERIES_BASE_LEVEL'

        write (*, 10) TRIM(CurrentBlock_g)
        write (LU_REC, 10) TRIM(CurrentBlock_g)
10      format(/, ' Processing ', a, ' block....')

        icontext = 0
        iseries = 0
        aname = ' '
        ixcon = 0
        isub = -9999
        jseries = -9999
        bseries = -9999
        ddb = -9999
        hhb = -9999
        ineg = 0

! -- The SERIES_BASE_LEVEL block is first parsed.

        do
            ILine_g = ILine_g + 1
            read (LU_TSPROC_CONTROL, '(a)', err=9000, end=9100) cline
            if (cline == ' ') cycle
            if (cline(1:1) == '#') cycle
            call linesplit(ierr, 2)
            if (ierr /= 0) then
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 20) TRIM(aline), TRIM(sString_g)
20              format('there should be 2 entries on line ', a, ' of file ', a)
                go to 9800
            end if
            aoption = cline(left_word(1):right_word(1))
            call casetrans(aoption, 'hi')
            if (aoption /= 'CONTEXT') then
                call test_context(ierr, icontext, acontext)
                if (ierr == -1) then
                    call find_end(ifail)
                    if (ifail == 1) go to 9800
                    return
                else if (ierr == 1) then
                    go to 9800
                end if
                ixcon = 1
            end if
            if (aoption == 'CONTEXT') then
                if (ixcon /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 41) TRIM(aline), TRIM(sString_g)
41                  format('CONTEXT keyword in incorrect location at line ', a, ' of file ', a)
                    go to 9800
                end if
                call get_context(ierr, icontext, acontext)
                if (ierr /= 0) go to 9800
            else if (aoption == 'SUBSTITUTE') then
                call getfile(ierr, cline, atemp, left_word(2), right_word(2))
                if (atemp == 'yes') then
                    isub = 1
                else if (atemp == 'no') then
                    isub = 0
                else
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 42) TRIM(aline), TRIM(sString_g)
42                  format('"yes" or "no" should follow the SUBSTITUTE ', &
                           'keyword at line ', a, ' of file ', a)
                    go to 9800
                end if
                write (*, 44) TRIM(aoption), TRIM(atemp)
                write (LU_REC, 44) TRIM(aoption), TRIM(atemp)
44              format(t5, a, 1X, a)
            else if (aoption == 'NEW_SERIES_NAME') then
                call get_new_series_name(ierr, aname)
                if (ierr /= 0) go to 9800
                jseries = 1
            else if (aoption == 'SERIES_NAME') then
                call get_series_name(ierr, iseries, 'SERIES_NAME')
                if (ierr /= 0) go to 9800
            else if (aoption == 'BASE_LEVEL_SERIES_NAME') then
                call get_series_name(ierr, bseries, 'BASE_LEVEL_SERIES_NAME')
                if (ierr /= 0) go to 9800
            else if (aoption == 'BASE_LEVEL_DATE') then
                call get_date(ierr, ddb, mmb, yyb, 'BASE_LEVEL_DATE')
                if (ierr /= 0) go to 9800
!           daysb=numdays(1,1,1970,ddb,mmb,yyb)
                daysb = julian_day(iMonth=mmb, iDay=ddb, iYear=yyb)
            else if (aoption == 'BASE_LEVEL_TIME') then
                call get_time(ierr, hhb, nnb, ssb, 'BASE_LEVEL_TIME')
                if (ierr /= 0) go to 9800
                secsb = hhb * 3600 + nnb * 60 + ssb
            else if (aoption == 'NEGATE') then
                call getfile(ierr, cline, atemp, left_word(2), right_word(2))
                if (atemp == 'yes') then
                    ineg = 1
                else if (atemp == 'no') then
                    ineg = 0
                else
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 49) TRIM(aline), TRIM(sString_g)
49                  format('"yes" or "no" should follow the NEGATE ', &
                           'keyword at line ', a, ' of file ', a)
                    go to 9800
                end if
                write (*, 44) TRIM(aoption), TRIM(atemp)
                write (LU_REC, 44) TRIM(aoption), TRIM(atemp)
            else if (aoption == 'END') then
                go to 200
            else
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 90) TRIM(aoption), TRIM(CurrentBlock_g), TRIM(aline), TRIM(sString_g)
90              format('unexpected keyword - "', a, '" in ', a, ' block at line ', a, &
                       ' of file ', a)
                go to 9800
            end if
        end do

! -- The block has been read; now it is checked for correctness.

200     continue
        if (iseries == 0) then
            write (amessage, 210) TRIM(CurrentBlock_g)
210         format('no SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (isub == -9999) then
            write (amessage, 211) TRIM(CurrentBlock_g)
211         format('no SUBSTITUTE keyword supplied in ', a, ' block.')
            go to 9800
        end if
        if ((isub == 1) .AND. (jseries /= -9999)) then
            write (amessage, 220) TRIM(CurrentBlock_g)
220         format('if SUBSTITUTE is set to "yes" then a NEW_SERIES_NAME ', &
                   'keyword must not be supplied in ', a, ' block.')
            go to 9800
        end if
        if ((isub == 0) .AND. (jseries == -9999)) then
            write (amessage, 222) TRIM(CurrentBlock_g)
222         format('if SUBSTITUTE is set to "no" then a NEW_SERIES_NAME ', &
                   'keyword must be supplied in ', a, ' block.')
            go to 9800
        end if
        if (bseries == -9999) then
            write (amessage, 230) TRIM(CurrentBlock_g)
230         format('no BASE_LEVEL_SERIES_NAME keyword supplied in ', a, ' block.')
            go to 9800
        end if
        if (ddb == -9999) then
            write (amessage, 240) TRIM(CurrentBlock_g)
240         format('no BASE_LEVEL_DATE keyword supplied in ', a, ' block.')
            go to 9800
        end if
        if (hhb == -9999) then
            write (amessage, 250) TRIM(CurrentBlock_g)
250         format('no BASE_LEVEL_TIME keyword supplied in ', a, ' block.')
            go to 9800
        end if
        if (secsb >= 86400) then
            daysb = daysb + 1
            secsb = secsb - 86400
        end if
        if (icontext == 0) then
            write (amessage, 260) TRIM(CurrentBlock_g)
260         format('no CONTEXT keyword(s) provided in ', a, ' block.')
            go to 9800
        end if

! -- The new series base level is now determined.

        do i = 1, series_g(bseries)%nterm
            if ((series_g(bseries)%days(i) == daysb) .AND. &
                (series_g(bseries)%secs(i) == secsb)) then
                rbase = series_g(bseries)%val(i)
                go to 300
            end if
        end do
        write (amessage, 280) TRIM(series_g(bseries)%name), TRIM(CurrentBlock_g)
280     format('no member of BASE_LEVEL_SERIES "', a, '" has a date and time ', &
               'corresponding to those supplied with the BASE_LEVEL_DATE and ', &
               'BASE_LEVEL_TIME keywords in ', a, ' block.')
        go to 9800
300     continue

! -- If no new series is required, the base level change is now undertaken.

        if (jseries == -9999) then
            if (ineg == 0) then
                do i = 1, series_g(iseries)%nterm
                    series_g(iseries)%val(i) = series_g(iseries)%val(i) - rbase
                end do
            else
                do i = 1, series_g(iseries)%nterm
                    series_g(iseries)%val(i) = rbase - series_g(iseries)%val(i)
                end do
            end if
            go to 900
        end if

! -- If a new time series is warranted, then space is allocated for it.

        if (jseries /= -9999) then
            do i = 1, MAXSERIES
                if (.NOT. series_g(i)%active) go to 515
            end do
            write (amessage, 510)
510         format('no more time series available for data storage - increase MAXSERIES and ', &
                   'recompile program.')
            go to 9800

515         continue
            iterm = series_g(iseries)%nterm
            allocate (series_g(i)%days(iterm), series_g(i)%secs(iterm), &
                      series_g(i)%val(iterm), stat=ierr)
            if (ierr /= 0) then
                write (amessage, 550)
550             format('cannot allocate memory for another time series.')
                go to 9800
            end if
            series_g(i)%active = .TRUE.
            series_g(i)%name = aname
            series_g(i)%nterm = iterm
            series_g(i)%type = 'ts'
            do j = 1, series_g(iseries)%nterm
                series_g(i)%days(j) = series_g(iseries)%days(j)
            end do
            do j = 1, series_g(iseries)%nterm
                series_g(i)%secs(j) = series_g(iseries)%secs(j)
            end do
            if (ineg == 0) then
                do j = 1, series_g(iseries)%nterm
                    series_g(i)%val(j) = series_g(iseries)%val(j) - rbase
                end do
            else
                do j = 1, series_g(iseries)%nterm
                    series_g(i)%val(j) = rbase - series_g(iseries)%val(j)
                end do
            end if
        end if

900     continue
        if (jseries == -9999) then
            write (*, 910) TRIM(series_g(iseries)%name)
            write (LU_REC, 910) TRIM(series_g(iseries)%name)
910         format(t5, 'New base level applied to series "', a, '".')
        else
            write (*, 920) TRIM(series_g(i)%name)
            write (LU_REC, 920) TRIM(series_g(i)%name)
920         format(t5, 'New series "', a, '" successfully calculated.')
        end if
        return

9000    call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9010) TRIM(aline), TRIM(sString_g)
9010    format('cannot read line ', a, ' of TSPROC input file ', a)
        go to 9800
9100    continue
        call addquote(sInfile_g, sString_g)
        write (amessage, 9110) TRIM(sString_g), TRIM(CurrentBlock_g)
9110    format('unexpected end encountered to TSPROC input file ', a, ' while ', &
               ' reading ', a, ' block.')
        go to 9800

9800    call write_message(leadspace='yes', error='yes')
        call write_message(iunit=LU_REC, leadspace='yes')
        ifail = 1

        return

    end subroutine series_base_level

    subroutine vol_to_series(ifail)

! -- Subroutine VOL_TO_SERIES stores a V_TABLE as an S_TABLE.

        use tsp_data_structures
        use tsp_utilities
        use tsp_command_processors

        implicit none

        integer, intent(out) :: ifail

        integer ivtable, nsterm, idiff, ihalf, isecs, icontext, ixcon, ierr, iser, &
            j, idays, itemp
        character(len=iTSNAMELENGTH) :: aname, abscissa
        character(15) aline
        character(25) aoption
        character(25) acontext(MAXCONTEXT)
        logical(kind=T_LOGICAL) :: lCumulativeValues
        real(kind=T_DBL) :: dpCumulativeVolume
        real :: factor

        lCumulativeValues = lFALSE
        dpCumulativeVolume = 0_T_DBL
        factor = 1.0
        ifail = 0
        CurrentBlock_g = 'V_TABLE_TO_SERIES'

        write (*, 10) TRIM(CurrentBlock_g)
        write (LU_REC, 10) TRIM(CurrentBlock_g)
10      format(/, ' Processing ', a, ' block....')

        ivtable = 0
        icontext = 0
        abscissa = ' '
        aname = ' '
        ixcon = 0

! -- The V_TABLE_TO_SERIES block is first parsed.

        do
            ILine_g = ILine_g + 1
            read (LU_TSPROC_CONTROL, '(a)', err=9000, end=9100) cline
            if (cline == ' ') cycle
            if (cline(1:1) == '#') cycle
            call linesplit(ierr, 2)
!         if(ierr.ne.0)then
!           call num2char(ILine_g,aline)
!           call addquote(sInfile_g,sString_g)
!           write(amessage,20) trim(aline),trim(sString_g)
!20         format('there should be 2 entries on line ',a,' of file ',a)
!           go to 9800
!         end if
            aoption = cline(left_word(1):right_word(1))
            call casetrans(aoption, 'hi')
            if (aoption /= 'CONTEXT') then
                call test_context(ierr, icontext, acontext)
                if (ierr == -1) then
                    call find_end(ifail)
                    if (ifail == 1) go to 9800
                    return
                else if (ierr == 1) then
                    go to 9800
                end if
                ixcon = 1
            end if
            if (aoption == 'V_TABLE_NAME') then
                call get_table_name(ierr, ivtable, 2)
                if (ierr /= 0) go to 9800

            else if (aoption == 'NEW_SERIES_NAME') then
                call get_new_series_name(ierr, aname)
                if (ierr /= 0) go to 9800

            else if (aoption == 'FACTOR') then
                call get_keyword_value(ierr, 2, itemp, factor, 'FACTOR')
                if (ierr /= 0) go to 9800

            else if (aoption == 'CUMULATIVE') then
                lCumulativeValues = lTRUE

            else if (aoption == 'CONTEXT') then
                if (ixcon /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 41) TRIM(aline), TRIM(sString_g)
41                  format('CONTEXT keyword in incorrect location at line ', a, ' of file ', a)
                    go to 9800
                end if
                call get_context(ierr, icontext, acontext)
                if (ierr /= 0) go to 9800

            else if (aoption == 'TIME_ABSCISSA') then
                call getfile(ierr, cline, abscissa, left_word(2), right_word(2))
                if (ierr /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 49) TRIM(aline), TRIM(sString_g)
49                  format('cannot read time abscissa from line ', a, ' of file ', a)
                    go to 9800
                end if
                call casetrans(abscissa, 'lo')
                if (abscissa == 'center') abscissa = 'centre'
                if ((abscissa /= 'start') .AND. &
                    (abscissa /= 'centre') .AND. &
                    (abscissa /= 'end')) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 55) TRIM(aline), TRIM(sString_g)
55                  format('time abscissa must be "start", "centre" or "end" ', &
                           'at line ', a, ' of file ', a)
                    go to 9800
                end if
                write (*, 60) TRIM(abscissa)
                write (LU_REC, 60) TRIM(abscissa)
60              format(t5, 'TIME_ABSCISSA ', a)

            else if (aoption == 'END') then
                go to 200
            else
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 90) TRIM(aoption), TRIM(CurrentBlock_g), TRIM(aline), TRIM(sString_g)
90              format('unexpected keyword - "', a, '" in ', a, ' block at line ', a, &
                       ' of file ', a)
                go to 9800
            end if
        end do

! -- The block has been read; now it is checked for correctness.

200     continue
        if (ivtable == 0) then
            write (amessage, 210) TRIM(CurrentBlock_g)
210         format('no V_TABLE_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (aname == ' ') then
            write (amessage, 230) TRIM(CurrentBlock_g)
230         format('no NEW_SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (icontext == 0) then
            write (amessage, 220) TRIM(CurrentBlock_g)
220         format('no CONTEXT keyword(s) provided in ', a, ' block.')
            go to 9800
        end if
        if (abscissa == ' ') then
            write (amessage, 235) TRIM(CurrentBlock_g)
235         format('no TIME_ABSCISSA keyword provided in ', a, ' block.')
            go to 9800
        end if

! -- The new time series is now written.

        do iser = 1, MAXSERIES
            if (.NOT. series_g(iser)%active) go to 370
        end do
        write (amessage, 360)
360     format('no more time series available for data storage - increase MAXSERIES and ', &
               'recompile program.')
        go to 9800

370     continue
        nsterm = vtable_g(ivtable)%nterm
        allocate (series_g(iser)%days(nsterm), series_g(iser)%secs(nsterm), &
                  series_g(iser)%dpval(nsterm), series_g(iser)%val(nsterm), stat=ierr)

        series_g(iser)%lIsSinglePrecision = lFALSE

        if (ierr /= 0) then
            write (amessage, 380)
380         format('cannot allocate memory for another time series.')
            go to 9800
        end if
        series_g(iser)%active = .TRUE.
        series_g(iser)%name = aname
        series_g(iser)%nterm = nsterm
        series_g(iser)%type = 'ts'
        if (abscissa == 'start') then
            do j = 1, nsterm
                series_g(iser)%days(j) = vtable_g(ivtable)%days1(j)
            end do
            do j = 1, nsterm
                series_g(iser)%secs(j) = vtable_g(ivtable)%secs1(j)
            end do
        else if (abscissa == 'end') then
            do j = 1, nsterm
                series_g(iser)%days(j) = vtable_g(ivtable)%days2(j)
            end do
            do j = 1, nsterm
                series_g(iser)%secs(j) = vtable_g(ivtable)%secs2(j)
            end do
        else
            do j = 1, nsterm
                idiff = vtable_g(ivtable)%days2(j) - vtable_g(ivtable)%days1(j)
                ihalf = idiff / 2
                idays = vtable_g(ivtable)%days1(j) + ihalf
                if (ihalf * 2 == idiff) then
                    isecs = (vtable_g(ivtable)%secs1(j) + vtable_g(ivtable)%secs2(j)) / 2
                else
                    isecs = (vtable_g(ivtable)%secs1(j) + vtable_g(ivtable)%secs2(j)) / 2 + 43200
                end if
385             if (isecs >= 86400) then
                    isecs = isecs - 86400
                    idays = idays + 1
                    go to 385
                end if
                series_g(iser)%days(j) = idays
                series_g(iser)%secs(j) = isecs
            end do
        end if

        ! assign volumes to time series elements
        if (lCumulativeValues) then
            do j = 1, nsterm
                dpCumulativeVolume = dpCumulativeVolume + vtable_g(ivtable)%vol(j) * factor
                series_g(iser)%dpval(j) = dpCumulativeVolume
                series_g(iser)%val(j) = dpCumulativeVolume
            end do
        else
            do j = 1, nsterm
                series_g(iser)%dpval(j) = vtable_g(ivtable)%vol(j) * factor
                series_g(iser)%val(j) = vtable_g(ivtable)%vol(j) * factor
            end do
        end if

        write (6, 390) TRIM(aname)
        write (LU_REC, 390) TRIM(aname)
390     format(t5, 'Data from v_table stored in series "', a, '".')
        return

9000    call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9010) TRIM(aline), TRIM(sString_g)
9010    format('cannot read line ', a, ' of TSPROC input file ', a)
        go to 9800
9100    continue
        call addquote(sInfile_g, sString_g)
        write (amessage, 9110) TRIM(sString_g), TRIM(CurrentBlock_g)
9110    format('unexpected end encountered to TSPROC input file ', a, ' while ', &
               ' reading ', a, ' block.')
        go to 9800

9800    call write_message(leadspace='yes', error='yes')
        call write_message(iunit=LU_REC, leadspace='yes')
        ifail = 1

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

        integer, intent(out) :: ifail

        integer icontext, iseries, ixcon, ierr, idelete, ilthresh, iuthresh, itemp, iterm, i, k, j, &
            isub
        real(kind=T_DBL) :: lthresh, uthresh, svalue, rtemp
        character(len=iTSNAMELENGTH) :: aname, atemp
        character(15) aline
        character(25) aoption
        character(25) acontext(MAXCONTEXT)

        ifail = 0
        CurrentBlock_g = 'SERIES_CLEAN'

        write (*, 10) TRIM(CurrentBlock_g)
        write (LU_REC, 10) TRIM(CurrentBlock_g)
10      format(/, ' Processing ', a, ' block....')

        icontext = 0
        iseries = 0
        aname = ' '
        ixcon = 0
        lthresh = 1.1E37
        uthresh = -1.1E37
        ilthresh = 0
        iuthresh = 0
        idelete = 0
        isub = 0

! -- The CLEAN_SERIES block is first parsed.

        do
            ILine_g = ILine_g + 1
            read (LU_TSPROC_CONTROL, '(a)', err=9000, end=9100) cline
            if (cline == ' ') cycle
            if (cline(1:1) == '#') cycle
            call linesplit(ierr, 2)
            if (ierr /= 0) then
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 20) TRIM(aline), TRIM(sString_g)
20              format('there should be 2 entries on line ', a, ' of file ', a)
                go to 9800
            end if
            aoption = cline(left_word(1):right_word(1))
            call casetrans(aoption, 'hi')
            if (aoption /= 'CONTEXT') then
                call test_context(ierr, icontext, acontext)
                if (ierr == -1) then
                    call find_end(ifail)
                    if (ifail == 1) go to 9800
                    return
                else if (ierr == 1) then
                    go to 9800
                end if
                ixcon = 1
            end if
            if (aoption == 'CONTEXT') then
                if (ixcon /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 41) TRIM(aline), TRIM(sString_g)
41                  format('CONTEXT keyword in incorrect location at line ', a, ' of file ', a)
                    go to 9800
                end if
                call get_context(ierr, icontext, acontext)
                if (ierr /= 0) go to 9800
            else if (aoption == 'LOWER_ERASE_BOUNDARY') then
                call get_keyword_value(ierr, 2, itemp, lthresh, aoption)
                if (ierr /= 0) go to 9800
            else if (aoption == 'UPPER_ERASE_BOUNDARY') then
                call get_keyword_value(ierr, 2, itemp, uthresh, aoption)
                if (ierr /= 0) go to 9800
            else if (aoption == 'SUBSTITUTE_VALUE') then
                isub = 1
                call get_keyword_value(ierr, 2, itemp, svalue, 'SUBSTITUTE_VALUE')
                if (ierr /= 0) then
                    call casetrans(cline(left_word(2):right_word(2)), 'lo')
                    call getfile(ierr, cline, atemp, left_word(2), right_word(2))
                    if (atemp == 'delete') then
                        idelete = 1
                        write (*, 44) TRIM(aoption)
                        write (LU_REC, 44) TRIM(aoption)
44                      format(t5, a, 1X, 'delete')
                    else
                        call num2char(ILine_g, aline)
                        call addquote(sInfile_g, sString_g)
                        write (amessage, 50) TRIM(aline), TRIM(sString_g)
50                      format('a real number or "delete" must be supplied with the ', &
                               'SUBSTITUTE_VALUE keyword at line ', a, ' of file ', a)
                        go to 9800
                    end if
                end if
            else if (aoption == 'NEW_SERIES_NAME') then
                call get_new_series_name(ierr, aname)
                if (ierr /= 0) go to 9800
            else if (aoption == 'SERIES_NAME') then
                call get_series_name(ierr, iseries, 'SERIES_NAME')
                if (ierr /= 0) go to 9800
            else if (aoption == 'END') then
                go to 200
            else
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 90) TRIM(aoption), TRIM(CurrentBlock_g), TRIM(aline), TRIM(sString_g)
90              format('unexpected keyword - "', a, '" in ', a, ' block at line ', a, &
                       ' of file ', a)
                go to 9800
            end if
        end do

! -- The block has been read; now it is checked for correctness.

200     continue
        if (iseries == 0) then
            write (amessage, 210) TRIM(CurrentBlock_g)
210         format('no SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (isub == 0) then
            write (amessage, 211) TRIM(CurrentBlock_g)
211         format('no SUBSTITUTE_VALUE keyword supplied in the ', a, ' block.')
            go to 9800
        end if
        if ((aname == ' ') .AND. (idelete == 1)) then
            write (amessage, 230) TRIM(CurrentBlock_g)
230         format('if SUBSTITUTE_VALUE is supplied as "delete" then a ', &
                   'NEW_SERIES_NAME must be supplied in the ', a, ' block.')
            go to 9800
        end if
        if (icontext == 0) then
            write (amessage, 220) TRIM(CurrentBlock_g)
220         format('no CONTEXT keyword(s) provided in ', a, ' block.')
            go to 9800
        end if
        if ((lthresh > 1.0E37) .AND. (uthresh < -1.0E37)) then
            write (amessage, 225) TRIM(CurrentBlock_g)
225         format('neither an UPPER_ERASE_BOUNDARY nor a LOWER_ERASE_BOUNDARY ', &
                   'has been supplied in the ', a, ' block.')
            go to 9800
        else
            if (lthresh < 1.0E37) ilthresh = 1
            if (uthresh > -1.0E37) iuthresh = 1
        end if
        if ((ilthresh == 1) .AND. (iuthresh == 1)) then
            if (lthresh >= uthresh) then
                write (amessage, 235)
235             format('the upper erase boundary must be greater than the lower ', &
                       'erase boundary.')
                go to 9800
            end if
        end if

! -- The new series is now written. But first the number of terms in the new series
!    is counted.

        if (idelete == 0) then
            iterm = series_g(iseries)%nterm
        else
            iterm = 0
            if ((ilthresh == 1) .AND. (iuthresh == 1)) then
                do j = 1, series_g(iseries)%nterm
                    if ((series_g(iseries)%val(j) < lthresh) .OR. &
                        (series_g(iseries)%val(j) > uthresh)) iterm = iterm + 1
                end do
            else if (ilthresh == 1) then
                do j = 1, series_g(iseries)%nterm
                    if (series_g(iseries)%val(j) < lthresh) iterm = iterm + 1
                end do
            else
                do j = 1, series_g(iseries)%nterm
                    if (series_g(iseries)%val(j) > uthresh) iterm = iterm + 1
                end do
            end if
            if (iterm == 0) then
                write (amessage, 240) TRIM(series_g(iseries)%name), TRIM(aname)
240             format('all terms in series "', a, '" have been erased, so the new ', &
                       'series "', a, '" has no terms.')
                go to 9800
            end if
        end if

! -- If a new time series is warranted, then space is allocated for it.

        if (aname /= ' ') then
            do i = 1, MAXSERIES
                if (.NOT. series_g(i)%active) go to 515
            end do
            write (amessage, 510)
510         format('no more time series available for data storage - increase MAXSERIES and ', &
                   'recompile program.')
            go to 9800

515         allocate (series_g(i)%days(iterm), series_g(i)%secs(iterm), &
                      series_g(i)%val(iterm), stat=ierr)
            if (ierr /= 0) then
                write (amessage, 550)
550             format('cannot allocate memory for another time series.')
                go to 9800
            end if
            series_g(i)%active = .TRUE.
            series_g(i)%name = aname
            series_g(i)%nterm = iterm
            series_g(i)%type = 'ts'
            k = 0
            do j = 1, series_g(iseries)%nterm
                if ((ilthresh == 1) .AND. (iuthresh == 1)) then
                    if ((series_g(iseries)%val(j) < lthresh) .OR. &
                        (series_g(iseries)%val(j) > uthresh)) go to 570
                else if (ilthresh == 1) then
                    if (series_g(iseries)%val(j) < lthresh) go to 570
                else
                    if (series_g(iseries)%val(j) > uthresh) go to 570
                end if
                if (idelete == 1) go to 580
                rtemp = svalue
                go to 575
570             continue
                rtemp = series_g(iseries)%val(j)
575             continue
                k = k + 1
                series_g(i)%days(k) = series_g(iseries)%days(j)
                series_g(i)%secs(k) = series_g(iseries)%secs(j)
                series_g(i)%val(k) = rtemp
580             continue
            end do
        else
            do j = 1, series_g(iseries)%nterm
                if ((ilthresh == 1) .AND. (iuthresh == 1)) then
                    if ((series_g(iseries)%val(j) >= lthresh) .AND. &
                        (series_g(iseries)%val(j) <= uthresh)) &
                        series_g(iseries)%val(j) = svalue
                else if (ilthresh == 1) then
                    if (series_g(iseries)%val(j) >= lthresh) series_g(iseries)%val(j) = svalue
                else
                    if (series_g(iseries)%val(j) <= uthresh) series_g(iseries)%val(j) = svalue
                end if
            end do
        end if

        if (aname /= ' ') then
            write (*, 590) TRIM(aname)
            write (LU_REC, 590) TRIM(aname)
590         format(t5, 'Series "', a, '" successfully calculated.')
        else
            write (*, 600) TRIM(series_g(iseries)%name)
            write (LU_REC, 600) TRIM(series_g(iseries)%name)
600         format(t5, 'Series "', a, '" successfully cleaned.')
        end if
        return

9000    call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9010) TRIM(aline), TRIM(sString_g)
9010    format('cannot read line ', a, ' of TSPROC input file ', a)
        go to 9800
9100    continue
        call addquote(sInfile_g, sString_g)
        write (amessage, 9110) TRIM(sString_g), TRIM(CurrentBlock_g)
9110    format('unexpected end encountered to TSPROC input file ', a, ' while ', &
               ' reading ', a, ' block.')
        go to 9800

9800    call write_message(leadspace='yes', error='yes')
        call write_message(iunit=LU_REC, leadspace='yes')
        ifail = 1

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

        integer, intent(out) :: ifail

        integer ierr, icontext, iseries, itemp, nsterm, ilags, j, nsecs, ndays, &
            dd, mm, yy, hh, nn, ss, i, ixcon, ns, k, jclipzero, jclipinput, jfreq1, jfreq2, &
            jfreq, jns, jfilpass, jpass, jalpha, ipass, ip
        integer jrevstage2, jj
        real rtemp, freq, freq1, freq2, af, bf, cf, df, ef, tdelt, alpha, alpha1, fk_1, &
            yk1, yk, yk_1
        real a(3), b(3), c(3), d(3), e(3), rval(-3:5), gval(-3:5)
        character(3) aaa
        character(len=iTSNAMELENGTH) :: aname, filpass
        character(20) aline, filtype
        character(25) aoption
        character(25) acontext(MAXCONTEXT)

        ifail = 0
        CurrentBlock_g = 'DIGITAL_FILTER'

        write (*, 10) TRIM(CurrentBlock_g)
        write (LU_REC, 10) TRIM(CurrentBlock_g)
10      format(/, ' Processing ', a, ' block....')

        icontext = 0
        iseries = 0
        aname = ' '
        ixcon = 0
        filtype = ' '
        filpass = ' '
        jfilpass = 0
        ns = 1
        jns = 0
        freq = -1.0E35
        freq1 = -1.0E35
        freq2 = -1.0E35
        jfreq = 0
        jfreq1 = 0
        jfreq2 = 0
        jclipzero = 0
        jclipinput = 0
        jalpha = 0
        jpass = 0
        jrevstage2 = 0

! -- The DIGITAL_FILTER block is first parsed.

        do
            ILine_g = ILine_g + 1
            read (LU_TSPROC_CONTROL, '(a)', err=9000, end=9100) cline
            if (cline == ' ') cycle
            if (cline(1:1) == '#') cycle
            call linesplit(ierr, 2)
            if (ierr /= 0) then
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 20) TRIM(aline), TRIM(sString_g)
20              format('there should be 2 entries on line ', a, ' of file ', a)
                go to 9800
            end if
            aoption = cline(left_word(1):right_word(1))
            call casetrans(aoption, 'hi')
            if (aoption /= 'CONTEXT') then
                call test_context(ierr, icontext, acontext)
                if (ierr == -1) then
                    call find_end(ifail)
                    if (ifail == 1) go to 9800
                    return
                else if (ierr == 1) then
                    go to 9800
                end if
                ixcon = 1
            end if
            if (aoption == 'NEW_SERIES_NAME') then
                call get_new_series_name(ierr, aname)
                if (ierr /= 0) go to 9800
            else if (aoption == 'SERIES_NAME') then
                call get_series_name(ierr, iseries, 'SERIES_NAME')
                if (ierr /= 0) go to 9800
            else if (aoption == 'CONTEXT') then
                if (ixcon /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 41) TRIM(aline), TRIM(sString_g)
41                  format('CONTEXT keyword in incorrect location at line ', a, ' of file ', a)
                    go to 9800
                end if
                call get_context(ierr, icontext, acontext)
                if (ierr /= 0) go to 9800
            else if (aoption == 'FILTER_TYPE') then
                call getfile(ierr, cline, filtype, left_word(2), right_word(2))
                if (ierr /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 49) TRIM(aline), TRIM(sString_g)
49                  format('cannot read filter type from line ', a, ' of file ', a)
                    go to 9800
                end if
                call casetrans(filtype, 'lo')
                if ((filtype /= 'butterworth') .AND. &
                    (filtype /= 'baseflow_separation')) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 55) TRIM(aline), TRIM(sString_g)
55                  format('filter type must be "butterworth" or "baseflow_separation" ', &
                           'at line ', a, ' of file ', a)
                    go to 9800
                end if
                write (*, 60) TRIM(filtype)
                write (LU_REC, 60) TRIM(filtype)
60              format(t5, 'FILTER_TYPE ', a)
            else if (aoption == 'FILTER_PASS') then
                call getfile(ierr, cline, filpass, left_word(2), right_word(2))
                if (ierr /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 50) TRIM(aline), TRIM(sString_g)
50                  format('cannot read filter pass band type from line ', a, ' of file ', a)
                    go to 9800
                end if
                jfilpass = 1
                call casetrans(filpass, 'lo')
                if ((filpass /= 'high') .AND. (filpass /= 'low') .AND. &
                    (filpass /= 'band')) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 53) TRIM(aline), TRIM(sString_g)
53                  format('filter pass band type must be "high", "low" or "band" at line ', &
                           a, ' of file ', a)
                    go to 9800
                end if
                write (*, 54) TRIM(filpass)
                write (LU_REC, 54) TRIM(filpass)
54              format(t5, 'FILTER_PASS ', a)
            else if (aoption == 'STAGES') then
                call get_keyword_value(ierr, 1, ns, rtemp, 'STAGES')
                if (ierr /= 0) go to 9800
                jns = 1
            else if (aoption == 'CUTOFF_FREQUENCY') then
                call get_keyword_value(ierr, 2, itemp, freq, 'CUTOFF_FREQUENCY')
                if (ierr /= 0) go to 9800
                if (freq <= 0.0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 65) TRIM(aline), TRIM(sString_g)
65                  format('frequency must be positive at line ', a, ' of file ', a)
                    go to 9800
                end if
                jfreq = 1
            else if (aoption == 'CUTOFF_FREQUENCY_1') then
                call get_keyword_value(ierr, 2, itemp, freq1, 'CUTOFF_FREQUENCY_1')
                if (ierr /= 0) go to 9800
                if (freq1 <= 0.0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 65) TRIM(aline), TRIM(sString_g)
                    go to 9800
                end if
                jfreq1 = 1
            else if (aoption == 'CUTOFF_FREQUENCY_2') then
                call get_keyword_value(ierr, 2, itemp, freq2, 'CUTOFF_FREQUENCY_2')
                if (ierr /= 0) go to 9800
                if (freq2 <= 0.0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 65) TRIM(aline), TRIM(sString_g)
                    go to 9800
                end if
                jfreq2 = 1
            else if (aoption == 'CLIP_ZERO') then
                call get_yes_no(ierr, jclipzero)
                if (ierr /= 0) go to 9800
                if (jclipzero == 1) then
                    aaa = 'yes'
                else
                    aaa = 'no'
                end if
                write (*, 70) TRIM(aaa)
                write (LU_REC, 70) TRIM(aaa)
70              format(t5, 'CLIP_ZERO ', a)
            else if (aoption == 'CLIP_INPUT') then
                call get_yes_no(ierr, jclipinput)
                if (ierr /= 0) go to 9800
                if (jclipinput == 1) then
                    aaa = 'yes'
                else
                    aaa = 'no'
                end if
                write (*, 71) TRIM(aaa)
                write (LU_REC, 71) TRIM(aaa)
71              format(t5, 'CLIP_INPUT ', a)
            else if (aoption == 'ALPHA') then
                call get_keyword_value(ierr, 2, itemp, alpha, 'ALPHA')
                if (ierr /= 0) go to 9800
                if (alpha <= 0.0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 81) TRIM(aline), TRIM(sString_g)
81                  format('alpha must be positive at line ', a, ' of file ', a)
                    go to 9800
                end if
                jalpha = 1
            else if (aoption == 'REVERSE_SECOND_STAGE') then
                call get_yes_no(ierr, jrevstage2)
                if (ierr /= 0) go to 9800
                if (jrevstage2 == 1) then
                    aaa = 'yes'
                else
                    aaa = 'no'
                end if
                write (*, 72) TRIM(aaa)
                write (LU_REC, 72) TRIM(aaa)
72              format(t5, 'REVERSE_SECOND_STAGE ', a)
            else if (aoption == 'PASSES') then
                call get_keyword_value(ierr, 1, ipass, rtemp, 'PASSES')
                if (ierr /= 0) go to 9800
                if ((ipass /= 1) .AND. (ipass /= 3)) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 82) TRIM(aline), TRIM(sString_g)
82                  format('number of passes must be 1 or 3 at line ', a, ' of file ', a)
                    go to 9800
                end if
                jpass = 1
            else if (aoption == 'END') then
                go to 200
            else
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 90) TRIM(aoption), TRIM(CurrentBlock_g), TRIM(aline), TRIM(sString_g)
90              format('unexpected keyword - "', a, '" in ', a, ' block at line ', a, &
                       ' of file ', a)
                go to 9800
            end if
        end do

! -- The block has been read; now it is checked for correctness.

200     continue
        if (iseries == 0) then
            write (amessage, 210) TRIM(CurrentBlock_g)
210         format('no SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (aname == ' ') then
            write (amessage, 230) TRIM(CurrentBlock_g)
230         format('no NEW_SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (icontext == 0) then
            write (amessage, 220) TRIM(CurrentBlock_g)
220         format('no CONTEXT keyword(s) provided in ', a, ' block.')
            go to 9800
        end if
        if (filtype == ' ') then
            write (amessage, 222) TRIM(CurrentBlock_g)
222         format('no FILTER_TYPE keyword has been provided in ', a, ' block.')
            go to 9800
        end if
        if (filtype(1:4) == 'base') then
            if (jns == 1) go to 9300
            if (jfilpass == 1) go to 9300
            if ((jfreq == 1) .OR. (jfreq1 == 1) .OR. (jfreq2 == 1)) go to 9300
        else if (filtype(1:3) == 'but') then
            if (jpass == 1) go to 9350
            if (jalpha == 1) go to 9350
            if ((jclipzero == 1) .OR. (jclipinput == 1)) go to 9370
        end if
        if (filtype(1:3) == 'but') then
            if ((ns < 1) .OR. (ns > 3)) then
                write (amessage, 245) TRIM(CurrentBlock_g)
245             format('number of filter stages must be 1, 2, or 3 in ', a, ' block.')
                go to 9800
            end if
            if ((filpass == 'low') .OR. (filpass == 'high')) then
                if ((freq1 > 0.0) .OR. (freq2 > 0.0)) then
                    write (amessage, 250) TRIM(CurrentBlock_g)
250                 format('values for CUTOFF_FREQUENCY_1 and CUTOFF_FREQUENCY_2 ', &
                           'should be supplied only for band pass filter in ', a, ' block.')
                    go to 9800
                end if
                if (freq < 0.0) then
                    write (amessage, 255) TRIM(CurrentBlock_g)
255                 format('no value supplied for CUTOFF_FREQUENCY in ', a, ' block.')
                    go to 9800
                end if
            else if (filpass == 'band') then
                if ((freq1 < 0.0) .OR. (freq2 < 0.0)) then
                    write (amessage, 256) TRIM(CurrentBlock_g)
256                 format('values for both CUTOFF_FREQUENCY_1 and CUTOFF_FREQUENCY_2 ', &
                           'should be supplied for band pass filter in ', a, ' block.')
                    go to 9800
                end if
                if (freq > 0.0) then
                    write (amessage, 257) TRIM(CurrentBlock_g)
257                 format('no value should be supplied for CUTOFF_FREQUENCY for ', &
                           'band pass filter in ', a, ' block - only for CUTOFF_FREQUENCY_1 ', &
                           'and CUTOFF_FREQUENCY_2.')
                    go to 9800
                end if
                if (freq2 <= freq1) then
                    write (amessage, 258) TRIM(CurrentBlock_g)
258                 format('CUTOFF_FREQUENCY_2 should be greater than CUTOFF_FREQUENCY_1 ', &
                           'in ', a, ' block.')
                    go to 9800
                end if
            else if (filpass == ' ') then
                write (amessage, 259) TRIM(CurrentBlock_g)
259             format('no FILTER_PASS keyword provided in ', a, ' block.')
                go to 9800
            end if
        else if (filtype(1:4) == 'base') then
            if (jpass == 0) ipass = 1
            if (jalpha == 0) then
                write (amessage, 270) TRIM(CurrentBlock_g)
270             format('no ALPHA keyword provided in ', a, ' block.')
                go to 9800
            end if
        end if
        if (jrevstage2 == 1) then
            if ((filtype(1:3) /= 'but') .OR. (filpass /= 'low') .OR. (ns /= 2)) then
                write (amessage, 299)
299             format('REVERSE_SECOND_STAGE can only be set to "yes" when (a) butterworth ', &
                       'filter is chosen, (b) number or stages is 2 and (c) the FILTER_PASS is "low".')
                go to 9800
            end if
        end if

! -- Filtering can only be performed if the input time series has equal
!    increments. This is now tested.

        nsterm = series_g(iseries)%nterm
        if (nsterm < 20) then
            call num2char(nsterm, aline)
            write (amessage, 300) TRIM(series_g(iseries)%name), TRIM(aline)
300         format('series "', a, '" has only ', a, ' terms. This is insufficient to perform ', &
                   'the requested filtering operation.')
            go to 9800
        end if
        if (nsterm > 2) then
            ilags = (series_g(iseries)%days(2) - series_g(iseries)%days(1)) * 86400 + &
                    series_g(iseries)%secs(2) - series_g(iseries)%secs(1)
            do j = 2, nsterm - 1
                nsecs = series_g(iseries)%secs(j) + ilags
                ndays = series_g(iseries)%days(j)
260             if (nsecs >= 86400) then
                    ndays = ndays + 1
                    nsecs = nsecs - 86400
                    go to 260
                end if
                if ((nsecs /= series_g(iseries)%secs(j + 1)) .OR. &
                    (ndays /= series_g(iseries)%days(j + 1))) then
!               call newdate(series_g(iseries)%days(j),1,1,1970,dd,mm,yy)
                    call gregorian_date(iJD=series_g(iseries)%days(j), &
                                        iMonth=mm, &
                                        iDay=dd, &
                                        iYear=yy)
                    nsecs = series_g(iseries)%secs(j)
                    hh = nsecs / 3600
                    nn = (nsecs - hh * 3600) / 60
                    ss = nsecs - hh * 3600 - nn * 60
                    if (datespec == 1) then
                        write (amessage, 280) TRIM(series_g(iseries)%name), dd, mm, yy, hh, nn, ss
                    else
                        write (amessage, 280) TRIM(series_g(iseries)%name), mm, dd, yy, hh, nn, ss
                    end if
280                 format('time interval between terms in time series "', a, '" is not ', &
                           'constant. The first discrepancy occurs following the sample taken on ', &
                           i2.2, '/', i2.2, '/', i4, ' at ', i2.2, ':', i2.2, ':', i2.2)
                    go to 9800
                end if
            end do
        end if

! -- The filter coefficients are now calculated (butterworth filter).

        if (filtype(1:3) == 'but') then
            tdelt = float(ilags) / 86400.00
            if (filpass == 'low') then
                if (freq >= 0.5 / tdelt) go to 9200
                call lpdes(freq, tdelt, ns, a, b, c)
            else if (filpass == 'high') then
                if (freq >= 0.5 / tdelt) go to 9200
                call hpdes(freq, tdelt, ns, a, b, c)
            else
                if ((freq1 >= 0.5 / tdelt) .OR. (freq2 >= 0.5 / tdelt)) go to 9200
                call bpdes(freq1, freq2, tdelt, ns, a, b, c, d, e)
            end if
        end if

! -- Space for a new series is allocated.

        do i = 1, MAXSERIES
            if (.NOT. series_g(i)%active) go to 515
        end do
        write (amessage, 510)
510     format('no more time series available for data storage - increase MAXSERIES and ', &
               'recompile program, or erase a series using an ERASE_SERIES block.')
        go to 9800

515     continue
        allocate (series_g(i)%days(nsterm), series_g(i)%secs(nsterm), &
                  series_g(i)%val(nsterm), stat=ierr)
        if (ierr /= 0) then
            write (amessage, 550)
550         format('cannot allocate memory for another time series.')
            go to 9800
        end if
        series_g(i)%active = .TRUE.
        series_g(i)%name = aname
        series_g(i)%nterm = nsterm
        series_g(i)%type = 'ts'
        do j = 1, nsterm
            series_g(i)%days(j) = series_g(iseries)%days(j)
        end do
        do j = 1, nsterm
            series_g(i)%secs(j) = series_g(iseries)%secs(j)
        end do

! -- Now the butterworth filtering is carried out. But first a temporary time series is
!    allocated if needed.

        if (filtype(1:4) == 'base') go to 700
        if (ns > 1) then
            call alloc_tempseries(ierr, nsterm)
            if (ierr /= 0) go to 9800
        end if

        do k = 1, ns
            af = a(k)
            bf = b(k)
            cf = c(k)
            if ((k == 2) .AND. (jrevstage2 == 1)) then
                af = a(1)
                bf = b(1)
                cf = c(1)
            end if
            if (filpass == 'band') then
                df = d(k)
                ef = e(k)
            end if
            if (k == 1) then
                do j = 1, 5
                    rval(j) = series_g(iseries)%val(j)
                end do
            else
                do j = 1, 5
                    rval(j) = tempseries_g%val(j)
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
            rval(0) = rval(1)
            rval(-1) = rval(0)
            rval(-2) = rval(-1)
            rval(-3) = rval(-2)
            if (filpass == 'low') then
                gval(-3) = rval(-3)
                gval(-2) = rval(-2)
                gval(-1) = rval(-1)
                gval(0) = rval(0)
            else
                gval(-3) = 0.0
                gval(-2) = 0.0
                gval(-1) = 0.0
                gval(0) = 0.0
            end if
            if (filpass == 'low') then
                do j = 1, 5
                    gval(j) = af * (rval(j) + 2.0 * rval(j - 1) + rval(j - 2)) - &
                              bf * gval(j - 1) - cf * gval(j - 2)
                end do
            else if (filpass == 'high') then
                do j = 1, 5
                    gval(j) = af * (rval(j) - 2.0 * rval(j - 1) + rval(j - 2)) - &
                              bf * gval(j - 1) - cf * gval(j - 2)
                end do
            else
                do j = 1, 5
                    gval(j) = af * (rval(j) - 2.0 * rval(j - 2) + rval(j - 4)) - bf * gval(j - 1) - &
                              cf * gval(j - 2) - df * gval(j - 3) - ef * gval(j - 4)
                end do
            end if
            do j = 1, 5
                series_g(i)%val(j) = gval(j)
            end do
            if (filpass == 'low') then
                if (k /= 1) then
                    do j = 6, nsterm
                        series_g(i)%val(j) = af * (tempseries_g%val(j) + &
                                                   2.0 * tempseries_g%val(j - 1) + tempseries_g%val(j - 2)) - &
                                             bf * series_g(i)%val(j - 1) - cf * series_g(i)%val(j - 2)
                    end do
                else
                    do j = 6, nsterm
                        series_g(i)%val(j) = af * (series_g(iseries)%val(j) + &
                                                   2.0 * series_g(iseries)%val(j - 1) + series_g(iseries)%val(j - 2)) - &
                                             bf * series_g(i)%val(j - 1) - cf * series_g(i)%val(j - 2)
                    end do
                end if
            else if (filpass == 'high') then
                if (k /= 1) then
                    do j = 6, nsterm
                        series_g(i)%val(j) = af * (tempseries_g%val(j) - &
                                                   2.0 * tempseries_g%val(j - 1) + tempseries_g%val(j - 2)) - &
                                             bf * series_g(i)%val(j - 1) - cf * series_g(i)%val(j - 2)
                    end do
                else
                    do j = 6, nsterm
                        series_g(i)%val(j) = af * (series_g(iseries)%val(j) - &
                                                   2.0 * series_g(iseries)%val(j - 1) + series_g(iseries)%val(j - 2)) - &
                                             bf * series_g(i)%val(j - 1) - cf * series_g(i)%val(j - 2)
                    end do
                end if
            else
                if (k /= 1) then
                    do j = 6, nsterm
                        series_g(i)%val(j) = af * (tempseries_g%val(j) - &
                                                   2.0 * tempseries_g%val(j - 2) + tempseries_g%val(j - 4)) - &
                                             bf * series_g(i)%val(j - 1) - cf * series_g(i)%val(j - 2) - &
                                             df * series_g(i)%val(j - 3) - ef * series_g(i)%val(j - 4)
                        if (ABS(series_g(i)%val(j)) > 1.0E30) go to 9400
                    end do
                else
                    do j = 6, nsterm
                        series_g(i)%val(j) = af * (series_g(iseries)%val(j) - &
                                                   2.0 * series_g(iseries)%val(j - 2) + series_g(iseries)%val(j - 4)) - &
                                             bf * series_g(i)%val(j - 1) - cf * series_g(i)%val(j - 2) - &
                                             df * series_g(i)%val(j - 3) - ef * series_g(i)%val(j - 4)
                        if (ABS(series_g(i)%val(j)) > 1.0E30) go to 9400
                    end do
                end if
            end if
            if (k /= ns) then
                if ((k == 1) .AND. (jrevstage2 == 1)) then
                    do j = 1, nsterm
                        tempseries_g%val(j) = series_g(i)%val(nsterm - j + 1)
                    end do
                else
                    do j = 1, nsterm
                        tempseries_g%val(j) = series_g(i)%val(j)
                    end do
                end if
            else
                if (jrevstage2 == 1) then
                    do j = 1, nsterm / 2
                        jj = nsterm - j + 1
                        rtemp = series_g(i)%val(j)
                        series_g(i)%val(j) = series_g(i)%val(jj)
                        series_g(i)%val(jj) = rtemp
                    end do
                end if
            end if
        end do
        go to 900

! -- Baseflow separation filtering is carried out.

700     continue

        call alloc_tempseries(ierr, nsterm)
        if (ierr /= 0) go to 9800

        alpha1 = (1.0 + alpha) * 0.5
        do ip = 1, ipass
            if (ip == 1) then
                do j = 1, nsterm
                    tempseries_g%val(j) = series_g(iseries)%val(j)
                end do
            else if ((ip == 2) .OR. (ip == 3)) then
                do j = 1, nsterm
                    tempseries_g%val(j) = series_g(i)%val(nsterm + 1 - j)
                end do
            end if
            yk = tempseries_g%val(1)
            yk1 = tempseries_g%val(2)
            yk_1 = yk - (yk1 - yk)
            fk_1 = yk_1
            series_g(i)%val(1) = alpha * fk_1 + alpha1 * (yk - yk_1)
            do j = 2, nsterm
                series_g(i)%val(j) = alpha * series_g(i)%val(j - 1) + alpha1 * &
                                     (tempseries_g%val(j) - tempseries_g%val(j - 1))
            end do
        end do

! -- The following applies to both types of filtering.

900     continue
        if (jclipzero /= 0) then
            do j = 1, nsterm
                if (series_g(i)%val(j) < 0.0) series_g(i)%val(j) = 0.0
            end do
        end if
        if (jclipinput /= 0) then
            do j = 1, nsterm
                if (series_g(i)%val(j) > series_g(iseries)%val(j)) &
                    series_g(i)%val(j) = series_g(iseries)%val(j)
            end do
        end if

        write (*, 580) TRIM(aname)
        write (LU_REC, 580) TRIM(aname)
580     format(t5, 'Series "', a, '" successfully calculated.')
        return

9000    call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9010) TRIM(aline), TRIM(sString_g)
9010    format('cannot read line ', a, ' of TSPROC input file ', a)
        go to 9800
9100    continue
        call addquote(sInfile_g, sString_g)
        write (amessage, 9110) TRIM(sString_g), TRIM(CurrentBlock_g)
9110    format('unexpected end encountered to TSPROC input file ', a, ' while ', &
               ' reading ', a, ' block.')
        go to 9800
9200    write (amessage, 9210) TRIM(CurrentBlock_g)
9210    format('filter cutoff frequency must be less than half of the ', &
               'series sampling frequency in ', a, ' block.')
        go to 9800
9300    write (amessage, 9310)
9310    format('if FILTER_TYPE is "baseflow_separation" then none of the STAGES ', &
               'CUTOFF_FREQUENCY, CUTOFF_FREQUENCY_1, CUTOFF_FREQUENCY_2 ', &
               'or FILTER_PASS keywords must be provided.')
        go to 9800
9350    write (amessage, 9360)
9360    format('if FILTER_TYPE is "butterworth" then neither of the PASSES, ', &
               'ALPHA, CLIP_ZERO nor CLIP_INPUT keywords must be provided.')
        go to 9800
9370    write (amessage, 9380)
9380    format('if FILTER_TYPE is "butterworth" then CLIP_ZERO and CLIP_INPUT ', &
               'should be omitted or set to "no".')
        go to 9800
9400    write (amessage, 9410)
9410    format('bandpass filter is numerically unstable - consider using ', &
               'wider pass band.')
        go to 9800

9800    call write_message(leadspace='yes', error='yes')
        call write_message(iunit=LU_REC, leadspace='yes')
        ifail = 1

        return

    end subroutine bfilter

    subroutine lpdes(fc, t, ns, a, b, c)

! -- Subroutine LPDES evaluates the coefficients for a low pass filter.

        implicit none
        integer, intent(in) :: ns
        real, intent(in) :: fc, t
        real, intent(out) :: a(ns), b(ns), c(ns)

        integer k
        real pi, wcp, cs, x

        pi = 3.1415926536
        wcp = SIN(fc * pi * t) / COS(fc * pi * t)
        do k = 1, ns
            cs = COS(float(2 * (k + ns) - 1) * pi / float(4 * ns))
            x = 1.0 / (1.0 + wcp * wcp - 2.0 * wcp * cs)
            a(k) = wcp * wcp * x
            b(k) = 2.0 * (wcp * wcp - 1.0) * x
            c(k) = (1.0 + wcp * wcp + 2.0 * wcp * cs) * x
        end do
        return

    end subroutine lpdes

    subroutine hpdes(fc, t, ns, a, b, c)

! -- Subroutine HPDES evaluates the coefficients for a high pass filter.

        implicit none
        integer, intent(in) :: ns
        real, intent(in) :: fc, t
        real, intent(out) :: a(ns), b(ns), c(ns)

        integer k
        real pi, wcp, cs

        pi = 3.1415926536
        wcp = SIN(fc * pi * t) / COS(fc * pi * t)
        do k = 1, ns
            cs = COS(float(2 * (k + ns) - 1) * pi / float(4 * ns))
            a(k) = 1.0 / (1.0 + wcp * wcp - 2.0 * wcp * cs)
            b(k) = 2.0 * (wcp * wcp - 1.0) * a(k)
            c(k) = (1.0 + wcp * wcp + 2.0 * wcp * cs) * a(k)
        end do
        return

    end subroutine hpdes

    subroutine bpdes(f1, f2, t, ns, a, b, c, d, e)

! -- Subroutine BPDES evaluates the coefficients for a band pass filter.

        implicit none
        integer, intent(in) :: ns
        real, intent(in) :: f1, f2, t
        real, intent(out) :: a(ns), b(ns), c(ns), d(ns), e(ns)

        integer k
        real pi, w1, w2, wc, q, s, cs, p, r, x

        pi = 3.1415926536
        w1 = SIN(f1 * pi * t) / COS(f1 * pi * t)
        w2 = SIN(f2 * pi * t) / COS(f2 * pi * t)
        wc = w2 - w1
        q = wc * wc + 2.0 * w1 * w2
        s = w1 * w1 * w2 * w2
        do k = 1, ns
            cs = COS(float(2 * (k + ns) - 1) * pi / float(4 * ns))
            p = -2.0 * wc * cs
            r = p * w1 * w2
            x = 1.0 + p + q + r + s
            a(k) = wc * wc / x
            b(k) = (-4.0 - 2.0 * p + 2.0 * r + 4.0 * s) / x
            c(k) = (6.0 - 2.0 * q + 6.0 * s) / x
            d(k) = (-4.0 + 2.0 * p - 2.0 * r + 4.0 * s) / x
            e(k) = (1.0 - p + q - r + s) / x
        end do

        return

    end subroutine bpdes

    subroutine compare_series(ifail)

! -- Subroutine COMPARE_SERIES calculates comparison statistics between time series.

        use tsp_data_structures
        use tsp_utilities
        use tsp_command_processors

        implicit none

        integer, intent(out) :: ifail

        integer isseries, ioseries, jbias, jse, jrbias, jrse, jns, jce, jia, jve, &
            ibseries, ibbterm, ibeterm, ibterm, exponent, l, jkg
        integer dd1, mm1, yy1, hh1, nn1, ss1, dd2, mm2, yy2, hh2, nn2, ss2, ierr, &
            icontext, i, begdays, begsecs, enddays, endsecs, &
            j, isbterm, iobterm, iseterm, ioeterm, iiterm, ixcon, isterm, ioterm, k
        real(kind=T_DBL) :: rtemp, rtemp1, tsum1, tsum2, tsum3, tsum4, tsum5, mean3
        real(kind=T_DBL) :: kgetcov, kgestds, kgestdo
        real(kind=T_DBL) :: kgetmpo, kgetmps
        character(3) aaa
        character(len=iTSNAMELENGTH) :: aname
        character(15) aline
        character(25) aoption
        character(25) acontext(MAXCONTEXT)

        ifail = 0
        CurrentBlock_g = 'SERIES_COMPARE'

        write (*, 10) TRIM(CurrentBlock_g)
        write (LU_REC, 10) TRIM(CurrentBlock_g)
10      format(/, ' Processing ', a, ' block....')

        icontext = 0
        isseries = 0
        ioseries = 0
        ibseries = 0
        jbias = 0
        jse = 0
        jrbias = 0
        jrse = 0
        jns = 0
        jkg = 0
        jce = 0
        jia = 0
        jve = 0
        exponent = -9999
        yy1 = -9999
        hh1 = -9999
        yy2 = -9999
        hh2 = -9999
        aname = ' '
        ixcon = 0

! -- The COMPARE_SERIES block is first parsed.

        do
            ILine_g = ILine_g + 1
            read (LU_TSPROC_CONTROL, '(a)', err=9000, end=9100) cline
            if (cline == ' ') cycle
            if (cline(1:1) == '#') cycle
            call linesplit(ierr, 2)
            if (ierr /= 0) then
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 20) TRIM(aline), TRIM(sString_g)
20              format('there should be 2 entries on line ', a, ' of file ', a)
                go to 9800
            end if
            aoption = cline(left_word(1):right_word(1))
            call casetrans(aoption, 'hi')
            if (aoption /= 'CONTEXT') then
                call test_context(ierr, icontext, acontext)
                if (ierr == -1) then
                    call find_end(ifail)
                    if (ifail == 1) go to 9800
                    return
                else if (ierr == 1) then
                    go to 9800
                end if
                ixcon = 1
            end if
            if (aoption == 'NEW_C_TABLE_NAME') then
                call get_new_table_name(ierr, 4, aname)
                if (ierr /= 0) go to 9800

            else if (aoption == 'DATE_1') then
                call get_date(ierr, dd1, mm1, yy1, 'DATE_1')
                if (ierr /= 0) go to 9800

            else if (aoption == 'DATE_2') then
                call get_date(ierr, dd2, mm2, yy2, 'DATE_2')
                if (ierr /= 0) go to 9800

            else if (aoption == 'TIME_1') then
                call get_time(ierr, hh1, nn1, ss1, 'TIME_1')
                if (ierr /= 0) go to 9800

            else if (aoption == 'TIME_2') then
                call get_time(ierr, hh2, nn2, ss2, 'TIME_2')
                if (ierr /= 0) go to 9800

            else if (aoption == 'SERIES_NAME_SIM') then
                if (isseries /= 0) then
                    write (amessage, 26)
26                  format('more than one SERIES_NAME_SIM entry in SERIES_COMPARE block.')
                    go to 9800
                end if
                call get_series_name(ierr, isseries, 'SERIES_NAME_SIM')
                if (ierr /= 0) go to 9800
            else if (aoption == 'SERIES_NAME_OBS') then
                if (ioseries /= 0) then
                    write (amessage, 27)
27                  format('more than one SERIES_NAME_OBS entry in SERIES_COMPARE block.')
                    go to 9800
                end if
                call get_series_name(ierr, ioseries, 'SERIES_NAME_OBS')
                if (ierr /= 0) go to 9800

            else if (aoption == 'CONTEXT') then
                if (ixcon /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 41) TRIM(aline), TRIM(sString_g)
41                  format('CONTEXT keyword in incorrect location at line ', a, ' of file ', a)
                    go to 9800
                end if
                call get_context(ierr, icontext, acontext)
                if (ierr /= 0) go to 9800

            else if (aoption == 'BIAS') then
                call get_yes_no(ierr, jbias)
                if (ierr /= 0) go to 9800
                if (jbias == 1) then
                    aaa = 'yes'
                else
                    aaa = 'no'
                end if
                write (*, 128) TRIM(aaa)
                write (LU_REC, 128) TRIM(aaa)
128             format(t5, 'BIAS ', a)

            else if (aoption == 'STANDARD_ERROR') then
                call get_yes_no(ierr, jse)
                if (ierr /= 0) go to 9800
                if (jse == 1) then
                    aaa = 'yes'
                else
                    aaa = 'no'
                end if
                write (*, 129) TRIM(aaa)
                write (LU_REC, 129) TRIM(aaa)
129             format(t5, 'STANDARD_ERROR ', a)

            else if (aoption == 'RELATIVE_BIAS') then
                call get_yes_no(ierr, jrbias)
                if (ierr /= 0) go to 9800
                if (jrbias == 1) then
                    aaa = 'yes'
                else
                    aaa = 'no'
                end if
                write (*, 132) TRIM(aaa)
                write (LU_REC, 132) TRIM(aaa)
132             format(t5, 'RELATIVE_BIAS ', a)

            else if (aoption == 'RELATIVE_STANDARD_ERROR') then
                call get_yes_no(ierr, jrse)
                if (ierr /= 0) go to 9800
                if (jrse == 1) then
                    aaa = 'yes'
                else
                    aaa = 'no'
                end if
                write (*, 133) TRIM(aaa)
                write (LU_REC, 133) TRIM(aaa)
133             format(t5, 'RELATIVE_STANDARD_ERROR ', a)

            else if (aoption == 'NASH_SUTCLIFFE') then
                call get_yes_no(ierr, jns)
                if (ierr /= 0) go to 9800
                if (jns == 1) then
                    aaa = 'yes'
                else
                    aaa = 'no'
                end if
                write (*, 134) TRIM(aaa)
                write (LU_REC, 134) TRIM(aaa)
134             format(t5, 'NASH_SUTCLIFFE ', a)

            else if (aoption == 'KLING_GUPTA') then
                call get_yes_no(ierr, jkg)
                if (ierr /= 0) go to 9800
                if (jkg == 1) then
                    aaa = 'yes'
                else
                    aaa = 'no'
                end if
                write (*, 137) TRIM(aaa)
                write (LU_REC, 137) TRIM(aaa)
137             format(t5, 'KLING_GUPTA ', a)

            else if (aoption == 'VOLUMETRIC_EFFICIENCY') then
                call get_yes_no(ierr, jve)
                if (ierr /= 0) go to 9800
                if (jve == 1) then
                    aaa = 'yes'
                else
                    aaa = 'no'
                end if
                write (*, 140) TRIM(aaa)
                write (LU_REC, 140) TRIM(aaa)
140             format(t5, 'VOLUMETRIC_EFFICIENCY ', a)

            else if (aoption == 'COEFFICIENT_OF_EFFICIENCY') then
                call get_yes_no(ierr, jce)
                if (ierr /= 0) go to 9800
                if (jce == 1) then
                    aaa = 'yes'
                else
                    aaa = 'no'
                end if
                write (*, 135) TRIM(aaa)
                write (LU_REC, 135) TRIM(aaa)
135             format(t5, 'COEFFICIENT_OF_EFFICIENCY ', a)

            else if (aoption == 'INDEX_OF_AGREEMENT') then
                call get_yes_no(ierr, jia)
                if (ierr /= 0) go to 9800
                if (jia == 1) then
                    aaa = 'yes'
                else
                    aaa = 'no'
                end if
                write (*, 136) TRIM(aaa)
                write (LU_REC, 136) TRIM(aaa)
136             format(t5, 'INDEX_OF_AGREEMENT ', a)

            else if (aoption == 'EXPONENT') then
                call get_keyword_value(ierr, 1, exponent, rtemp, 'EXPONENT')
                if (ierr /= 0) go to 9800
                if ((exponent /= 1) .AND. (exponent /= 2)) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 138) TRIM(aline), TRIM(sString_g)
138                 format('exponent must be 1 or 2 at line ', a, &
                           ' of file ', a)
                    go to 9800
                end if

            else if (aoption == 'SERIES_NAME_BASE') then
                call get_series_name(ierr, ibseries, 'SERIES_NAME_BASE')
                if (ierr /= 0) go to 9800

            else if (aoption == 'END') then
                go to 200

            else
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 90) TRIM(aoption), TRIM(CurrentBlock_g), TRIM(aline), TRIM(sString_g)
90              format('unexpected keyword - "', a, '" in ', a, ' block at line ', a, &
                       ' of file ', a)
                go to 9800
            end if

        end do

! -- The block has been read; now it is checked for correctness.

200     continue
        if (isseries == 0) then
            write (amessage, 210) TRIM(CurrentBlock_g)
210         format('no SERIES_NAME_SIM keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (ioseries == 0) then
            write (amessage, 211) TRIM(CurrentBlock_g)
211         format('no SERIES_NAME_OBS keyword provided in ', a, ' block.')
            go to 9800
        end if

        if (aname == ' ') then
            write (amessage, 230) TRIM(CurrentBlock_g)
230         format('no NEW_C_TABLE keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (icontext == 0) then
            write (amessage, 220) TRIM(CurrentBlock_g)
220         format('no CONTEXT keyword(s) provided in ', a, ' block.')
            go to 9800
        end if
        call date_check(ierr, yy1, mm1, dd1, hh1, nn1, ss1, yy2, mm2, dd2, hh2, nn2, ss2, &
                        begdays, begsecs, enddays, endsecs)
        if (ierr /= 0) go to 9800
        call beg_end_check(ierr, isseries, begdays, begsecs, enddays, endsecs)
        if (ierr /= 0) go to 9800
        call beg_end_check(ierr, ioseries, begdays, begsecs, enddays, endsecs)
        if (ierr /= 0) go to 9800

        if ((jbias == 0) .AND. (jse == 0) .AND. (jrbias == 0) &
            .AND. (jrse == 0) .AND. (jns == 0) .AND. (jce == 0) .AND. (jia == 0) &
            .AND. jve == 0 .AND. (jkg == 0)) then
            write (amessage, 240) TRIM(CurrentBlock_g)
240         format('at least one of the BIAS, STANDARD_ERROR, RELATIVE_BIAS, ', &
                   'RELATIVE_STANDARD_ERROR, NASH_SUTCLIFFE, KLING_GUPTA, ', &
                   'VOLUMETRIC_EFFICIENCY, COEFFICIENT_OF_EFFICIENCY or INDEX_OF_AGREEMENT keywords must ', &
                   'be supplied within a ', a, ' block.')
            go to 9800
        end if
        if (ibseries /= 0) then
            if ((jce == 0) .AND. (jia == 0)) then
                write (amessage, 245) TRIM(CurrentBlock_g)
245             format('a SERIES_NAME_BASE keyword can only be supplied ', &
                       'if a COEFFICIENT_OF_EFFICIENCY and/or INDEX_OF_AGREEMENT ', &
                       'keyword is supplied in a ', a, ' block.')
                go to 9800
            end if
        end if
        if ((jia /= 0) .OR. (jce /= 0)) then
            if (exponent == -9999) then
                write (amessage, 250) TRIM(CurrentBlock_g)
250             format('if a COEFFICIENT_OF_EFFICIENCY or INDEX_OF_AGREEMENT ', &
                       'keyword is supplied, an EXPONENT keyword must be supplied in ', &
                       a, ' block.')
                go to 9800
            end if
        end if
        if (exponent /= -9999) then
            if ((jia == 0) .AND. (jce == 0)) then
                write (amessage, 251) TRIM(CurrentBlock_g)
251             format('if an EXPONENT keyword is supplied, then a ', &
                       'COEFFICIENT_OF_EFFICIENCY or INDEX_OF_AGREEMENT keyword must ', &
                       'also be supplied in a ', a, ' block.')
                go to 9800
            end if
        end if

! -- The two (maybe three) time series are checked for time consitency.

        call numterms(isterm, isbterm, iseterm, begdays, begsecs, enddays, endsecs, isseries)
        if (isterm == 0) then
            write (amessage, 270) TRIM(series_g(isseries)%name)
270         format('there are no terms in time series "', a, '" between the provided ', &
                   'dates and times.')
            go to 9800
        end if
        call numterms(ioterm, iobterm, ioeterm, begdays, begsecs, enddays, endsecs, ioseries)
        if (ioterm == 0) then
            write (amessage, 270) TRIM(series_g(ioseries)%name)
            go to 9800
        end if
        if (isterm /= ioterm) go to 9300
        if (isterm <= 2) then
            write (amessage, 271)
271         format('there must be at least two terms in the comparison time ', &
                   'window of the nominated series.')
            go to 9800
        end if

        i = iobterm - 1
        do j = isbterm, iseterm
            i = i + 1
            if (series_g(isseries)%days(j) /= (series_g(ioseries)%days(i))) go to 9300
            if (series_g(isseries)%secs(j) /= (series_g(ioseries)%secs(i))) go to 9300
        end do

        if (ibseries /= 0) then
            call numterms(ibterm, ibbterm, ibeterm, begdays, begsecs, enddays, endsecs, ibseries)
            if (ibterm == 0) then
                write (amessage, 270) TRIM(series_g(ibseries)%name)
                go to 9800
            end if
            if (ibterm /= ioterm) go to 9400
            i = iobterm - 1
            do j = ibbterm, ibeterm
                i = i + 1
                if (series_g(ibseries)%days(j) /= (series_g(ioseries)%days(i))) go to 9400
                if (series_g(ibseries)%secs(j) /= (series_g(ioseries)%secs(i))) go to 9400
            end do
        end if

! The new c_table is initialized.

        do i = 1, MAXCTABLE
            if (.NOT. ctable_g(i)%active) go to 300
        end do
        write (amessage, 310)
310     format('no more C_TABLE''s available for data storage - increase MAXCTABLE and ', &
               'recompile program.')
        go to 9800
300     continue

        ctable_g(i)%active = .TRUE.
        ctable_g(i)%name = aname
        ctable_g(i)%rec_icount = isterm
        ctable_g(i)%series_name_sim = series_g(isseries)%name
        ctable_g(i)%series_name_obs = series_g(ioseries)%name
        if (begdays <= -99999990) then
            ctable_g(i)%rec_begdays = series_g(isseries)%days(1)
            ctable_g(i)%rec_begsecs = series_g(isseries)%secs(1)
        else
            ctable_g(i)%rec_begdays = begdays
            ctable_g(i)%rec_begsecs = begsecs
        end if
        if (enddays >= 99999990) then
            iiterm = series_g(isseries)%nterm
            ctable_g(i)%rec_enddays = series_g(isseries)%days(iiterm)
            ctable_g(i)%rec_endsecs = series_g(isseries)%secs(iiterm)
        else
            ctable_g(i)%rec_enddays = enddays
            ctable_g(i)%rec_endsecs = endsecs
        end if

! -- The comparison statistics are now calculated.

        tsum1 = 0.0
        tsum2 = 0.0
        tsum3 = 0.0
        tsum4 = 0.0
        tsum5 = 0.0

        k = iobterm - 1
        do j = isbterm, iseterm
            k = k + 1

            rtemp = series_g(ioseries)%val(k)
!         if((jrbias.ne.0).or.(jrse.ne.0).or.(jns.ne.0))then
!           if(rtemp.le.0.0)then
!             write(amessage,280)
!280          format('RELATIVE_BIAS, RELATIVE_STANDARD_ERROR or NASH_SUTCLIFFE ', &
!             'coefficient cannot be calculated because at least one term in the ', &
!             'observation time series has a value equal to, or less than, zero.')
!             go to 9800
!           end if
!         end if

            ! difference between simulated and observed (Vsim - Vobs)
            rtemp1 = series_g(isseries)%val(j) - rtemp

            ! sum of differences
            tsum1 = tsum1 + rtemp1

            ! sum of the square of the differences
            tsum2 = tsum2 + (rtemp1 * rtemp1)

            ! sum of observed values
            tsum3 = tsum3 + rtemp

            ! calculate this if we need INDEX of AGREEMENT or COEFFICIENT of EFFICIENCY
            ! sum of absolute value of difference raised to a power
            if ((jia /= 0) .OR. (jce /= 0)) then
                tsum4 = tsum4 + ABS(rtemp1)**exponent
            end if

            ! sum of the absolute value of the difference between
            ! simulated and observed
            tsum5 = tsum5 + ABS(rtemp1)
        end do

        ! this was formerly reassigned to tsum3
        ! mean3 is the mean of OBSERVED values
        mean3 = tsum3 / real(isterm, kind=T_DBL)

        ! VOLUMETRIC EFFICIENCY
        if (jve /= 0) then
            ctable_g(i)%ve = 1.0_T_DBL - (tsum5 / tsum3)
        else
            ctable_g(i)%ve = -1.0E37
        end if

        ! BIAS
        if (jbias /= 0) then
            ctable_g(i)%bias = tsum1 / real(isterm, kind=T_DBL)
        else
            ctable_g(i)%bias = -1.0E37
        end if

        ! STANDARD ERROR
        if (jse /= 0) then
            ctable_g(i)%se = SQRT(tsum2 / real(isterm - 1, kind=T_DBL))
        else
            ctable_g(i)%se = -1.0E37
        end if

        ! RELATIVE BIAS
        if (jrbias /= 0) then
            if (mean3 == 0.0) then
                ctable_g(i)%rbias = 1.0E30
            else
                ctable_g(i)%rbias = tsum1 / real(isterm, kind=T_DBL) / mean3
            end if
        else
            ctable_g(i)%rbias = -1.0E37
        end if

        ! RELATIVE STANDARD ERROR; NASH-SUTCLIFFE
        if ((jrse /= 0) .OR. (jns /= 0)) then
            tsum1 = 0.0
            k = iobterm - 1
            do j = isbterm, iseterm
                k = k + 1
                rtemp1 = series_g(ioseries)%val(k) - mean3
                tsum1 = tsum1 + (rtemp1 * rtemp1)
            end do
            if (tsum1 <= 0.0) then
                write (amessage, 390) TRIM(series_g(ioseries)%name)
390             format('cannot compute RELATIVE_STANDARD_ERROR or NASH_SUTCLIFFE ', &
                       'coefficient because observation time series "', a, '" is uniform ', &
                       'in observation time window.')
                go to 9800
            end if
            if (jrse /= 0) then
                ctable_g(i)%rse = SQRT(tsum2 / real(isterm - 1, kind=T_DBL)) &
                                  / SQRT(tsum1 / real(isterm - 1, kind=T_DBL))
            else
                ctable_g(i)%rse = -1.0E37
            end if
            if (jns /= 0) then
                ctable_g(i)%ns = 1.0 - (tsum2 / tsum1)
            else
                ctable_g(i)%ns = -1.0E37
            end if
        else
            ctable_g(i)%rse = -1.0E37
            ctable_g(i)%ns = -1.0E37
        end if

        ! KLING_GUPTA KGE
        if ((jkg /= 0)) then
            tsum1 = 0.0
            do j = isbterm, iseterm
                tsum1 = tsum1 + series_g(isseries)%val(j)
            end do
            ! mean of simulated
            tsum1 = tsum1 / real(isterm, kind=T_DBL)

            kgestdo = 0.0
            kgestds = 0.0
            kgetcov = 0.0
            k = iobterm - 1
            do j = isbterm, iseterm
                k = k + 1
                kgetmpo = series_g(ioseries)%val(k) - mean3
                kgetmps = series_g(isseries)%val(j) - tsum1
                kgestdo = kgestdo + kgetmpo**2
                kgestds = kgestds + kgetmps**2
                kgetcov = kgetcov + (kgetmpo * kgetmps)
            end do
            kgestdo = (kgestdo / (iseterm - isbterm))**0.5
            kgestds = (kgestds / (iseterm - isbterm))**0.5
            kgetcov = kgetcov / (iseterm - isbterm)

            ctable_g(i)%kge = 1.0 - ((kgetcov / (kgestdo * kgestds) - 1)**2 + &
                                     ((kgestds / kgestdo) - 1)**2 + &
                                     ((tsum1 / mean3) - 1)**2)**0.5
        else
            ctable_g(i)%kge = -1.0E37
        end if

        ! calculate COEFFICIENT of EFFICIENCY
        if (jce /= 0) then
            tsum1 = 0.0
            k = iobterm - 1
            if (ibseries == 0) then
                do j = isbterm, iseterm
                    k = k + 1
                    rtemp1 = (ABS(series_g(ioseries)%val(k) - mean3))**exponent
                    tsum1 = tsum1 + rtemp1
                end do
                if (tsum1 <= 0.0) then
                    write (amessage, 410) TRIM(series_g(ioseries)%name)
410                 format('cannot compute COEFFICIENT_OF_EFFICIENCY ', &
                           'because observation time series "', a, '" is uniform ', &
                           'in observation time window.')
                    go to 9800
                end if
            else
                do j = ibbterm, ibeterm
                    k = k + 1
                    rtemp1 = (ABS(series_g(ioseries)%val(k) - series_g(ibseries)%val(j)))**exponent
                    tsum1 = tsum1 + rtemp1
                end do
                if (tsum1 <= 0.0) then
                    write (amessage, 420) TRIM(series_g(ioseries)%name), &
                        TRIM(series_g(ibseries)%name)
420                 format('cannot compute COEFFICIENT_OF_EFFICIENCY ', &
                           'because observation time series "', a, '" is equal to ', &
                           'baseline time series "', a, '" in observation time window.')
                    go to 9800
                end if
            end if
            ctable_g(i)%ce = 1.0 - (tsum4 / tsum1)
        else
            ctable_g(i)%ce = -1.0E37
        end if

        ! calculate INDEX of AGREEMENT
        if (jia /= 0) then
            tsum1 = 0.0
            k = iobterm - 1
            l = isbterm - 1
            if (ibseries == 0) then
                do j = isbterm, iseterm
                    k = k + 1
                    rtemp1 = (ABS(series_g(ioseries)%val(k) - mean3) + &
                              ABS(series_g(isseries)%val(j) - mean3))**exponent
                    tsum1 = tsum1 + rtemp1
                end do
                if (tsum1 <= 0.0) then
                    write (amessage, 430) TRIM(series_g(ioseries)%name), &
                        TRIM(series_g(isseries)%name)
430                 format('cannot compute INDEX_OF_AGREEMENT ', &
                           'because observation time series "', a, '" and simulation ', &
                           'time series "', a, '" are uniform and equal ', &
                           'in observation time window.')
                    go to 9800
                end if
            else
                do j = ibbterm, ibeterm
                    k = k + 1
                    l = l + 1
                    rtemp1 = (ABS(series_g(ioseries)%val(k) - series_g(ibseries)%val(j)) + &
                              ABS(series_g(isseries)%val(l) - series_g(ibseries)%val(j))) &
                             **exponent
                    tsum1 = tsum1 + rtemp1
                end do
                if (tsum1 <= 0.0) then
                    write (amessage, 440)
440                 format('cannot compute INDEX_OF_AGREEMENT ', &
                           'because observation time series, simulation time series ', &
                           'and baseline time series are all equal in observation time ', &
                           'window.')
                    go to 9800
                end if
            end if
            ctable_g(i)%ia = 1.0 - tsum4 / tsum1
        else
            ctable_g(i)%ia = -1.0E37
        end if

        write (6, 380) TRIM(aname)
        write (LU_REC, 380) TRIM(aname)
380     format(t5, 'Comparison statistics stored in C_TABLE "', a, '".')
        return

9000    call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9010) TRIM(aline), TRIM(sString_g)
9010    format('cannot read line ', a, ' of TSPROC input file ', a)
        go to 9800
9100    continue
        call addquote(sInfile_g, sString_g)
        write (amessage, 9110) TRIM(sString_g), TRIM(CurrentBlock_g)
9110    format('unexpected end encountered to TSPROC input file ', a, ' while ', &
               ' reading ', a, ' block.')
        go to 9800
9300    write (amessage, 9310)
9310    format('the two series cited in the COMPARE_SERIES block must have ', &
               'identical sample dates and times within the comparison time ', &
               'window. Maybe the use of a NEW_TIME_BASE block will rectify the problem.')
        go to 9800
9400    write (amessage, 9410)
9410    format('the baseline series cited in the COMPARE_SERIES block must ', &
               'have identical sample dates and times within the comparison time ', &
               'window to the simulated and observed series. Maybe the use of a ', &
               'NEW_TIME_BASE block will rectify the problem.')
        go to 9800

9800    call write_message(leadspace='yes', error='yes')
        call write_message(iunit=LU_REC, leadspace='yes')
        ifail = 1

        return

    end subroutine compare_series

!     Last change:  J    10 Sep 2004    0:00 am
    subroutine reduce_span(ifail)

! -- Subroutine REDUCE_SPAN shortens the time-span of a time series.

        use tsp_data_structures
        use tsp_utilities
        use tsp_command_processors

        implicit none

        integer, intent(out) :: ifail

        integer dd1, mm1, yy1, hh1, nn1, ss1, dd2, mm2, yy2, hh2, nn2, ss2, ierr, &
            icontext, i, begdays, begsecs, enddays, endsecs, iterm, j, &
            iseries, k, ibterm, ieterm, ixcon
        character(len=iTSNAMELENGTH) :: aname
        character(15) aline
        character(25) aoption
        character(25) acontext(MAXCONTEXT)

        ifail = 0
        CurrentBlock_g = 'REDUCE_TIME_SPAN'

        write (*, 10) TRIM(CurrentBlock_g)
        write (LU_REC, 10) TRIM(CurrentBlock_g)
10      format(/, ' Processing ', a, ' block....')

        icontext = 0
        iseries = 0
        yy1 = -9999
        hh1 = -9999
        yy2 = -9999
        hh2 = -9999
        aname = ' '
        ixcon = 0

! -- The REDUCE_TIME_SPAN block is first parsed.

        do
            ILine_g = ILine_g + 1
            read (LU_TSPROC_CONTROL, '(a)', err=9000, end=9100) cline
            if (cline == ' ') cycle
            if (cline(1:1) == '#') cycle
            call linesplit(ierr, 2)
            if (ierr /= 0) then
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 20) TRIM(aline), TRIM(sString_g)
20              format('there should be 2 entries on line ', a, ' of file ', a)
                go to 9800
            end if
            aoption = cline(left_word(1):right_word(1))
            call casetrans(aoption, 'hi')
            if (aoption /= 'CONTEXT') then
                call test_context(ierr, icontext, acontext)
                if (ierr == -1) then
                    call find_end(ifail)
                    if (ifail == 1) go to 9800
                    return
                else if (ierr == 1) then
                    go to 9800
                end if
                ixcon = 1
            end if
            if (aoption == 'DATE_1') then
                call get_date(ierr, dd1, mm1, yy1, 'DATE_1')
                if (ierr /= 0) go to 9800
            else if (aoption == 'DATE_2') then
                call get_date(ierr, dd2, mm2, yy2, 'DATE_2')
                if (ierr /= 0) go to 9800
            else if (aoption == 'TIME_1') then
                call get_time(ierr, hh1, nn1, ss1, 'TIME_1')
                if (ierr /= 0) go to 9800
            else if (aoption == 'TIME_2') then
                call get_time(ierr, hh2, nn2, ss2, 'TIME_2')
                if (ierr /= 0) go to 9800
            else if (aoption == 'NEW_SERIES_NAME') then
                call get_new_series_name(ierr, aname)
                if (ierr /= 0) go to 9800
            else if (aoption == 'SERIES_NAME') then
                call get_series_name(ierr, iseries, 'SERIES_NAME')
                if (ierr /= 0) go to 9800
            else if (aoption == 'CONTEXT') then
                if (ixcon /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 41) TRIM(aline), TRIM(sString_g)
41                  format('CONTEXT keyword in incorrect location at line ', a, ' of file ', a)
                    go to 9800
                end if
                call get_context(ierr, icontext, acontext)
                if (ierr /= 0) go to 9800
            else if (aoption == 'END') then
                go to 200
            else
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 90) TRIM(aoption), TRIM(CurrentBlock_g), TRIM(aline), TRIM(sString_g)
90              format('unexpected keyword - "', a, '" in ', a, ' block at line ', a, &
                       ' of file ', a)
                go to 9800
            end if
        end do

! -- The block has been read; now it is checked for correctness.

200     continue
        if (iseries == 0) then
            write (amessage, 210) TRIM(CurrentBlock_g)
210         format('no SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (aname == ' ') then
            write (amessage, 230) TRIM(CurrentBlock_g)
230         format('no NEW_SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (icontext == 0) then
            write (amessage, 220) TRIM(CurrentBlock_g)
220         format('no CONTEXT keyword(s) provided in ', a, ' block.')
            go to 9800
        end if
        if ((yy1 == -9999) .AND. (yy2 == -9999)) then
            write (amessage, 235) TRIM(CurrentBlock_g)
235         format('neither a DATE_1 keyword nor a DATE_2 keyword provided in ', a, ' block')
            go to 9800
        end if
        call date_check(ierr, yy1, mm1, dd1, hh1, nn1, ss1, yy2, mm2, dd2, hh2, nn2, ss2, &
                        begdays, begsecs, enddays, endsecs)
        if (ierr /= 0) go to 9800
        call beg_end_check(ierr, iseries, begdays, begsecs, enddays, endsecs)
        if (ierr /= 0) go to 9800

! -- The new series is now written. But first the number of terms in the new series
!    is counted.

        call numterms(iterm, ibterm, ieterm, begdays, begsecs, enddays, endsecs, iseries)
        if (iterm <= 0) then
            write (amessage, 315)
315         format('there are no terms in the reduced-time-span series.')
            go to 9800
        end if

        do i = 1, MAXSERIES
            if (.NOT. series_g(i)%active) go to 515
        end do
        write (amessage, 510)
510     format('no more time series available for data storage - increase MAXSERIES and ', &
               'recompile program.')
        go to 9800

515     allocate (series_g(i)%days(iterm), series_g(i)%secs(iterm), &
                  series_g(i)%val(iterm), stat=ierr)
        if (ierr /= 0) then
            write (amessage, 550)
550         format('cannot allocate memory for another time series.')
            go to 9800
        end if
        series_g(i)%active = .TRUE.
        series_g(i)%name = aname
        series_g(i)%nterm = iterm
        series_g(i)%type = 'ts'
        k = 0
        do j = ibterm, ieterm
            k = k + 1
            series_g(i)%days(k) = series_g(iseries)%days(j)
        end do
        k = 0
        do j = ibterm, ieterm
            k = k + 1
            series_g(i)%secs(k) = series_g(iseries)%secs(j)
        end do
        k = 0
        do j = ibterm, ieterm
            k = k + 1
            series_g(i)%val(k) = series_g(iseries)%val(j)
        end do
        write (*, 580) TRIM(aname)
        write (LU_REC, 580) TRIM(aname)
580     format(t5, 'Series "', a, '" successfully calculated.')
        return

9000    call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9010) TRIM(aline), TRIM(sString_g)
9010    format('cannot read line ', a, ' of TSPROC input file ', a)
        go to 9800
9100    continue
        call addquote(sInfile_g, sString_g)
        write (amessage, 9110) TRIM(sString_g), TRIM(CurrentBlock_g)
9110    format('unexpected end encountered to TSPROC input file ', a, ' while ', &
               ' reading ', a, ' block.')
        go to 9800

9800    call write_message(leadspace='yes', error='yes')
        call write_message(iunit=LU_REC, leadspace='yes')
        ifail = 1

        return

    end subroutine reduce_span

    subroutine statistics(ifail)

! -- Subroutine STATISTICS calculates summary statistics for a time series.

        use tsp_data_structures
        use tsp_utilities
        use tsp_command_processors

        implicit none

        integer, intent(out) :: ifail

        integer dd1, mm1, yy1, hh1, nn1, ss1, dd2, mm2, yy2, hh2, nn2, ss2, ierr, &
            icontext, i, begdays, begsecs, enddays, endsecs, iseries, jtrans, javerage, &
            jstddev, jmaximum, jminimum, jsum, j, ibterm, ieterm, iterm, iiterm, itemp, ixcon, &
            iitemp, jj, minaverage, maxaverage, ii, nnterm, jrange, jmed
        real tpower, tsum, tmin, tmax, rtemp, raverage, localsum, tminmean, tmaxmean
        character(3) aaa
        character(len=iTSNAMELENGTH) :: aname, atemp
        character(15) aline
        character(25) aoption
        character(25) acontext(MAXCONTEXT)

        ifail = 0
        CurrentBlock_g = 'SERIES_STATISTICS'

        write (*, 10) TRIM(CurrentBlock_g)
        write (LU_REC, 10) TRIM(CurrentBlock_g)
10      format(/, ' Processing ', a, ' block....')

        icontext = 0
        iseries = 0
        yy1 = -9999
        hh1 = -9999
        yy2 = -9999
        hh2 = -9999
        jtrans = 0
        javerage = 0
        jstddev = 0
        jmaximum = 0
        jminimum = 0
        jrange = 0
        jsum = 0
        aname = ' '
        tpower = -1.0E35
        ixcon = 0
        minaverage = 0
        maxaverage = 0
        jmed = 0

! -- The SERIES_STATISTICS block is first parsed.

        do
            ILine_g = ILine_g + 1
            read (LU_TSPROC_CONTROL, '(a)', err=9000, end=9100) cline
            if (cline == ' ') cycle
            if (cline(1:1) == '#') cycle
            call linesplit(ierr, 2)
            if (ierr /= 0) then
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 20) TRIM(aline), TRIM(sString_g)
20              format('there should be 2 entries on line ', a, ' of file ', a)
                go to 9800
            end if
            aoption = cline(left_word(1):right_word(1))
            call casetrans(aoption, 'hi')
            if (aoption /= 'CONTEXT') then
                call test_context(ierr, icontext, acontext)
                if (ierr == -1) then
                    call find_end(ifail)
                    if (ifail == 1) go to 9800
                    return
                else if (ierr == 1) then
                    go to 9800
                end if
                ixcon = 1
            end if
            if (aoption == 'DATE_1') then
                call get_date(ierr, dd1, mm1, yy1, 'DATE_1')
                if (ierr /= 0) go to 9800
            else if (aoption == 'DATE_2') then
                call get_date(ierr, dd2, mm2, yy2, 'DATE_2')
                if (ierr /= 0) go to 9800
            else if (aoption == 'TIME_1') then
                call get_time(ierr, hh1, nn1, ss1, 'TIME_1')
                if (ierr /= 0) go to 9800
            else if (aoption == 'TIME_2') then
                call get_time(ierr, hh2, nn2, ss2, 'TIME_2')
                if (ierr /= 0) go to 9800
            else if (aoption == 'NEW_S_TABLE_NAME') then
                call get_new_table_name(ierr, 1, aname)
                if (ierr /= 0) go to 9800
            else if (aoption == 'SERIES_NAME') then
                call get_series_name(ierr, iseries, 'SERIES_NAME')
                if (ierr /= 0) go to 9800
            else if (aoption == 'CONTEXT') then
                if (ixcon /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 41) TRIM(aline), TRIM(sString_g)
41                  format('CONTEXT keyword in incorrect location at line ', a, ' of file ', a)
                    go to 9800
                end if
                call get_context(ierr, icontext, acontext)
                if (ierr /= 0) go to 9800
            else if (aoption == 'POWER') then
                call get_keyword_value(ierr, 2, itemp, tpower, 'POWER')
                if (ierr /= 0) go to 9800
                if (tpower == 0.0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 125) TRIM(aline), TRIM(sString_g)
125                 format('POWER must not be zero at line ', a, ' of file ', a)
                    go to 9800
                end if
            else if (aoption == 'LOG') then
                call get_yes_no(ierr, jtrans)
                if (ierr /= 0) go to 9800
                if (jtrans == 1) then
                    aaa = 'yes'
                else
                    aaa = 'no'
                end if
                write (*, 127) TRIM(aaa)
                write (LU_REC, 127) TRIM(aaa)
127             format(t5, 'LOG ', a)
            else if (aoption == 'MEAN') then
                call get_yes_no(ierr, javerage)
                if (ierr /= 0) go to 9800
                if (javerage == 1) then
                    aaa = 'yes'
                else
                    aaa = 'no'
                end if
                write (*, 128) TRIM(aaa)
                write (LU_REC, 128) TRIM(aaa)
128             format(t5, 'MEAN ', a)
            else if (aoption(1:8) == 'MINMEAN_') then
                call get_yes_no(ierr, iitemp)
                if (ierr /= 0) go to 9800
                if (iitemp == 1) then
                    aaa = 'yes'
                else
                    aaa = 'no'
                    go to 178
                end if
                if (minaverage /= 0) then
                    write (amessage, 156) TRIM(CurrentBlock_g)
156                 format('only one MINMEAN_* keyword is allowed in each ', a, ' block.')
                    go to 9800
                else
                    minaverage = iitemp
                end if
                atemp = aoption(9:)
                call char2num(ierr, atemp, iitemp)
                if (ierr /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 154) TRIM(aline), TRIM(sString_g)
154                 format('cannot read averaging count for MINMEAN_* keyword at line ', &
                           a, ' of file ', a)
                    go to 9800
                end if
                if (iitemp <= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 153) TRIM(aline), TRIM(sString_g)
153                 format('illegal averaging count for MINMEAN_* keyword at line ', &
                           a, ' of file ', a)
                    go to 9800
                end if
                minaverage = iitemp
178             continue
                write (*, 151) TRIM(aoption), TRIM(aaa)
                write (LU_REC, 151) TRIM(aoption), TRIM(aaa)
151             format(t5, a, 1X, a)
            else if (aoption(1:8) == 'MAXMEAN_') then
                call get_yes_no(ierr, iitemp)
                if (ierr /= 0) go to 9800
                if (iitemp == 1) then
                    aaa = 'yes'
                else
                    aaa = 'no'
                    go to 179
                end if
                if (maxaverage /= 0) then
                    write (amessage, 138) TRIM(CurrentBlock_g)
138                 format('only one MAXMEAN_* keyword is allowed in each ', a, ' block.')
                    go to 9800
                end if
                maxaverage = iitemp
                atemp = aoption(9:)
                call char2num(ierr, atemp, iitemp)
                if (ierr /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 149) TRIM(aline), TRIM(sString_g)
149                 format('cannot read averaging count for MAXMEAN_* keyword at line ', &
                           a, ' of file ', a)
                    go to 9800
                end if
                if (iitemp <= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 139) TRIM(aline), TRIM(sString_g)
139                 format('illegal averaging count for MAXMEAN_* keyword at line ', &
                           a, ' of file ', a)
                    go to 9800
                end if
                maxaverage = iitemp
179             continue
                write (*, 151) TRIM(aoption), TRIM(aaa)
                write (LU_REC, 151) TRIM(aoption), TRIM(aaa)
            else if ((aoption == 'STD_DEV') .OR. (aoption == 'STANDARD_DEVIATION')) then
                call get_yes_no(ierr, jstddev)
                if (ierr /= 0) go to 9800
                if (jstddev == 1) then
                    aaa = 'yes'
                else
                    aaa = 'no'
                end if
                write (*, 129) TRIM(aaa)
                write (LU_REC, 129) TRIM(aaa)
129             format(t5, 'STD_DEV ', a)
            else if (aoption == 'MAXIMUM') then
                call get_yes_no(ierr, jmaximum)
                if (ierr /= 0) go to 9800
                if (jmaximum == 1) then
                    aaa = 'yes'
                else
                    aaa = 'no'
                end if
                write (*, 132) TRIM(aaa)
                write (LU_REC, 132) TRIM(aaa)
132             format(t5, 'MAXIMUM ', a)
            else if (aoption == 'MINIMUM') then
                call get_yes_no(ierr, jminimum)
                if (ierr /= 0) go to 9800
                if (jminimum == 1) then
                    aaa = 'yes'
                else
                    aaa = 'no'
                end if
                write (*, 133) TRIM(aaa)
                write (LU_REC, 133) TRIM(aaa)
133             format(t5, 'MINIMUM ', a)
            else if (aoption == 'RANGE') then
                call get_yes_no(ierr, jrange)
                if (ierr /= 0) go to 9800
                if (jrange == 1) then
                    aaa = 'yes'
                else
                    aaa = 'no'
                end if
                write (*, 136) TRIM(aaa)
                write (LU_REC, 136) TRIM(aaa)
136             format(t5, 'RANGE ', a)
            else if (aoption == 'SUM') then
                call get_yes_no(ierr, jsum)
                if (ierr /= 0) go to 9800
                if (jsum == 1) then
                    aaa = 'yes'
                else
                    aaa = 'no'
                end if
                write (*, 134) TRIM(aaa)
                write (LU_REC, 134) TRIM(aaa)
134             format(t5, 'SUM ', a)
            else if (aoption == 'MEDIAN') then
                call get_yes_no(ierr, jmed)
                if (ierr /= 0) go to 9800
                if (jmed == 1) then
                    aaa = 'yes'
                else
                    aaa = 'no'
                end if
                write (*, 435) TRIM(aaa)
                write (LU_REC, 435) TRIM(aaa)
435             format(t5, 'MEDIAN ', a)
            else if (aoption == 'END') then
                go to 200
            else
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 90) TRIM(aoption), TRIM(CurrentBlock_g), TRIM(aline), TRIM(sString_g)
90              format('unexpected keyword - "', a, '" in ', a, ' block at line ', a, &
                       ' of file ', a)
                go to 9800
            end if
        end do

! -- The block has been read; now it is checked for correctness.

200     continue
        if (iseries == 0) then
            write (amessage, 210) TRIM(CurrentBlock_g)
210         format('no SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (aname == ' ') then
            write (amessage, 230) TRIM(CurrentBlock_g)
230         format('no NEW_S_TABLE keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (icontext == 0) then
            write (amessage, 220) TRIM(CurrentBlock_g)
220         format('no CONTEXT keyword(s) provided in ', a, ' block.')
            go to 9800
        end if
        call date_check(ierr, yy1, mm1, dd1, hh1, nn1, ss1, yy2, mm2, dd2, hh2, nn2, ss2, &
                        begdays, begsecs, enddays, endsecs)
        if (ierr /= 0) go to 9800
        call beg_end_check(ierr, iseries, begdays, begsecs, enddays, endsecs)
        if (ierr /= 0) go to 9800
        if ((javerage == 0) .AND. (jstddev == 0) .AND. (jmaximum == 0) &
            .AND. (jminimum == 0) .AND. (jsum == 0) .AND. (maxaverage == 0) &
            .AND. (minaverage == 0) .AND. (jrange == 0) .AND. (jmed == 0)) then
            write (amessage, 240) TRIM(CurrentBlock_g)
240         format('at least one of the MEAN, STD_DEV, MAXIMUM, MINIMUM, ', &
                   'RANGE, SUM, MINMEAN_* or MAXMEAN_*  keywords must be supplied within a ', &
                   a, ' block.')
            go to 9800
        end if
        if ((jtrans == 1) .AND. (tpower > -1.0E30)) then
            write (amessage, 245) TRIM(CurrentBlock_g)
245         format('either the LOG or POWER keywords can be supplied ', &
                   'in a ', a, ' block, but not both.')
            go to 9800
        end if
        if ((minaverage /= 0) .OR. (maxaverage /= 0)) then
            if ((jtrans == 1) .OR. (tpower > -1.0D30)) then
                write (amessage, 246) TRIM(CurrentBlock_g)
246             format('if a MINMEAN_* or MAXMEAN_* keyword is supplied in ', a, ' block, ', &
                       'then neither the LOG or POWER keywords can be supplied in the same block.')
                go to 9800
            end if
        end if
        if (minaverage /= 0) then
            if (maxaverage /= 0) then
                if (minaverage /= maxaverage) then
                    write (amessage, 247) TRIM(CurrentBlock_g)
247                 format('if both a MINMEAN_* and a MAXMEAN_* keyword are supplied ', &
                           'in a ', a, ' block, then the averaging count must be the same for both.')
                    go to 9800
                end if
            end if
        end if

! -- All is well with the block. The STABLE is filled with requested statistics.

        call numterms(iterm, ibterm, ieterm, begdays, begsecs, enddays, endsecs, iseries)
        if (iterm == 0) then
            write (amessage, 270) TRIM(series_g(iseries)%name)
270         format('there are no terms in time series "', a, '" between the provided ', &
                   'dates and times.')
            go to 9800
        end if
        if ((minaverage > iterm) .OR. (maxaverage > iterm)) then
            write (amessage, 271)
271         format('the averaging count provided with the MINMEAN_* and/or ', &
                   'MAXMEAN_* keyword is greater than the number of terms in the block.')
            go to 9800
        end if

        do i = 1, MAXSTABLE
            if (.NOT. stable_g(i)%active) go to 300
        end do
        write (amessage, 310)
310     format('no more S_TABLE''s available for data storage - increase MAXSTABLE and ', &
               'recompile program.')
        go to 9800
300     continue

        if ((begdays < series_g(iseries)%days(1)) .OR. &
            ((begdays == series_g(iseries)%days(1)) .AND. &
             (begsecs < series_g(iseries)%secs(1)))) then
            begdays = series_g(iseries)%days(1)
            begsecs = series_g(iseries)%secs(1)
        end if
        iiterm = series_g(iseries)%nterm
        if ((enddays > series_g(iseries)%days(iiterm)) .OR. &
            ((enddays == series_g(iseries)%days(iiterm)) .AND. &
             (endsecs > series_g(iseries)%secs(iiterm)))) then
            enddays = series_g(iseries)%days(iiterm)
            endsecs = series_g(iseries)%secs(iiterm)
        end if

        if (tpower < -1.0E30) tpower = 0.0
        stable_g(i)%active = .TRUE.
        stable_g(i)%name = aname
        stable_g(i)%rec_icount = iterm
        stable_g(i)%series_name = series_g(iseries)%name
        stable_g(i)%rec_itrans = jtrans
        if (begdays == -99999999) then
            stable_g(i)%rec_begdays = series_g(iseries)%days(1)
            stable_g(i)%rec_begsecs = series_g(iseries)%secs(1)
        else
            stable_g(i)%rec_begdays = begdays
            stable_g(i)%rec_begsecs = begsecs
        end if
        if (enddays == 99999999) then
            stable_g(i)%rec_enddays = series_g(iseries)%days(iiterm)
            stable_g(i)%rec_endsecs = series_g(iseries)%secs(iiterm)
        else
            stable_g(i)%rec_enddays = enddays
            stable_g(i)%rec_endsecs = endsecs
        end if
        stable_g(i)%rec_power = tpower

        tsum = 0.0
        tmin = 1.0E30
        tmax = -1.0E30
        tminmean = 1.0E30
        tmaxmean = -1.0E30
        if (jtrans == 1) then
            do j = ibterm, ieterm
                rtemp = series_g(iseries)%val(j)
                if (rtemp <= 0.0) then
                    write (amessage, 350) TRIM(series_g(iseries)%name)
350                 format('cannot compute statistics on basis of log transform of terms ', &
                           'in series "', a, '" as there are zero or negative terms in this series.')
                    go to 9800
                end if
                rtemp = LOG10(rtemp)
                tsum = tsum + rtemp
                if (rtemp < tmin) tmin = rtemp
                if (rtemp > tmax) tmax = rtemp
            end do
        else
            if (tpower == 0.0) then
                do j = ibterm, ieterm
                    rtemp = series_g(iseries)%val(j)
                    tsum = tsum + rtemp
                    if (rtemp < tmin) tmin = rtemp
                    if (rtemp > tmax) tmax = rtemp
                    if ((maxaverage > 0) .OR. (minaverage > 0)) then
                        localsum = 0
                        nnterm = MAX(minaverage, maxaverage)
                        do ii = 1, nnterm
                            jj = j + ii - 1
                            if (jj > ieterm) go to 359
                            localsum = localsum + series_g(iseries)%val(jj)
                        end do
                        localsum = localsum / nnterm
                        if (maxaverage > 0) then
                            if (localsum > tmaxmean) tmaxmean = localsum
                        end if
                        if (minaverage > 0) then
                            if (localsum < tminmean) tminmean = localsum
                        end if
359                     continue
                    end if
                end do
            else
                do j = ibterm, ieterm
                    rtemp = series_g(iseries)%val(j)
                    if ((tpower < 0.0) .AND. (rtemp == 0.0)) then
                        write (amessage, 355) TRIM(series_g(iseries)%name)
355                     format('cannot compute statistics based on a negative POWER because ', &
                               'at least one of the terms of series "', a, '" is zero.')
                        go to 9800
                    end if
                    if ((ABS(tpower) < 1.0) .AND. (rtemp < 0.0)) then
                        write (amessage, 360) TRIM(series_g(iseries)%name)
360                     format('cannot compute statistics based on a POWER with absolute value ', &
                               'less than one because ', &
                               'at least one of the terms of series "', a, '" is negative.')
                        go to 9800
                    end if
                    rtemp = rtemp**tpower
                    tsum = tsum + rtemp
                    if (rtemp < tmin) tmin = rtemp
                    if (rtemp > tmax) tmax = rtemp
                end do
            end if
        end if
        raverage = tsum / iterm
        if (jmaximum == 1) then
            stable_g(i)%maximum = tmax
        else
            stable_g(i)%maximum = -1.0E37
        end if
        if (jminimum == 1) then
            stable_g(i)%minimum = tmin
        else
            stable_g(i)%minimum = -1.0E37
        end if
        if (jrange == 1) then
            stable_g(i)%range = tmax - tmin
        else
            stable_g(i)%range = -1.0E37
        end if
        if (javerage == 1) then
            stable_g(i)%mean = raverage
        else
            stable_g(i)%mean = -1.0E37
        end if
        if (jsum == 1) then
            stable_g(i)%total = tsum
        else
            stable_g(i)%total = -1.0E37
        end if
        if (jmed == 1) then
            stable_g(i)%median = median(series_g(iseries)%val)
        else
            stable_g(i)%median = -1.0E37
        end if
        if (jstddev == 0) then
            stable_g(i)%stddev = -1.0E37
        else
            tsum = 0
            if (jtrans == 1) then
                do j = ibterm, ieterm
                    rtemp = series_g(iseries)%val(j)
                    rtemp = LOG10(rtemp) - raverage
                    tsum = tsum + rtemp * rtemp
                end do
            else
                if (tpower == 0.0) then
                    do j = ibterm, ieterm
                        rtemp = series_g(iseries)%val(j)
                        rtemp = rtemp - raverage
                        tsum = tsum + rtemp * rtemp
                    end do
                else
                    do j = ibterm, ieterm
                        rtemp = series_g(iseries)%val(j)
                        rtemp = rtemp**tpower - raverage
                        tsum = tsum + rtemp * rtemp
                    end do
                end if
            end if
            if (iterm == 1) then
!           tsum=sqrt(tsum)
                tsum = 0.0
            else
                tsum = SQRT(tsum / (iterm - 1))
            end if
            stable_g(i)%stddev = tsum
        end if
        stable_g(i)%avetime = 0
        if (maxaverage == 0) then
            stable_g(i)%maxmean = -1.0E37
        else
            stable_g(i)%maxmean = tmaxmean
            stable_g(i)%avetime = nnterm
        end if
        if (minaverage == 0) then
            stable_g(i)%minmean = -1.0E37
        else
            stable_g(i)%minmean = tminmean
            stable_g(i)%avetime = nnterm
        end if

        write (6, 380) TRIM(series_g(iseries)%name), TRIM(aname)
        write (LU_REC, 380) TRIM(series_g(iseries)%name), TRIM(aname)
380     format(t5, 'Statistics for time series "', a, '" stored in ', &
               'S_TABLE "', a, '".')
        return

9000    call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9010) TRIM(aline), TRIM(sString_g)
9010    format('cannot read line ', a, ' of TSPROC input file ', a)
        go to 9800
9100    continue
        call addquote(sInfile_g, sString_g)
        write (amessage, 9110) TRIM(sString_g), TRIM(CurrentBlock_g)
9110    format('unexpected end encountered to TSPROC input file ', a, ' while ', &
               ' reading ', a, ' block.')
        go to 9800

9800    call write_message(leadspace='yes', error='yes')
        call write_message(iunit=LU_REC, leadspace='yes')
        ifail = 1

        return

    end subroutine statistics

    subroutine compute_hydrologic_indices(ifail)

! -- This subroutine makes a call to the computation engine pulled from HIT --
! -- the USGS Hydrologic Index Tool -- in order to provide user-specified
! -- combinations of the 171 hydrologic indices evaluated in Olden and Poff (2003)

        use tsp_data_structures
        use tsp_utilities
        use tsp_command_processors

        implicit none

        integer, intent(out) :: ifail

        integer dd1, mm1, yy1, hh1, nn1, ss1, dd2, mm2, yy2, hh2, nn2, ss2, ierr, &
            icontext, begdays, begsecs, enddays, endsecs, iseries, &
            iterm, ibterm, ieterm, iiterm, itemp, ixcon, &
            j, ig
        integer :: iCount, iStat, iIndex, iIndex2, iIndex3
        integer :: iNumberOfKeywords
        character(3) aaa
        character(len=iTSNAMELENGTH) :: aname
        character(15) aline
        character(25) aoption
        character(len=25) :: sStreamClass
        character(len=25) :: sFlowComponent
        character(25) acontext(MAXCONTEXT)

        integer(C_INT), dimension(0:149) :: iYr
        real(C_DOUBLE), dimension(0:365, 0:149) :: rQ
        real(C_DOUBLE), dimension(0:45) :: rMA
        real(C_DOUBLE), dimension(0:45) :: rLMA
        real(C_DOUBLE), dimension(0:45) :: rUMA
        real(C_DOUBLE), dimension(0:22) :: rML
        real(C_DOUBLE), dimension(0:22) :: rLML
        real(C_DOUBLE), dimension(0:22) :: rUML
        real(C_DOUBLE), dimension(0:28) :: rMH
        real(C_DOUBLE), dimension(0:28) :: rLMH
        real(C_DOUBLE), dimension(0:28) :: rUMH
        real(C_DOUBLE), dimension(0:4) :: rFL
        real(C_DOUBLE), dimension(0:4) :: rLFL
        real(C_DOUBLE), dimension(0:4) :: rUFL
        real(C_DOUBLE), dimension(0:11) :: rFH
        real(C_DOUBLE), dimension(0:11) :: rLFH
        real(C_DOUBLE), dimension(0:11) :: rUFH
        real(C_DOUBLE), dimension(0:20) :: rDL
        real(C_DOUBLE), dimension(0:20) :: rLDL
        real(C_DOUBLE), dimension(0:20) :: rUDL
        real(C_DOUBLE), dimension(0:24) :: rDH
        real(C_DOUBLE), dimension(0:24) :: rLDH
        real(C_DOUBLE), dimension(0:24) :: rUDH

        real(C_DOUBLE), dimension(0:3) :: rUTA
        real(C_DOUBLE), dimension(0:3) :: rTA
        real(C_DOUBLE), dimension(0:3) :: rLTA

        real(C_DOUBLE), dimension(0:4) :: rUTL
        real(C_DOUBLE), dimension(0:4) :: rTL
        real(C_DOUBLE), dimension(0:4) :: rLTL

        real(C_DOUBLE), dimension(0:3) :: rUTH
        real(C_DOUBLE), dimension(0:3) :: rTH
        real(C_DOUBLE), dimension(0:3) :: rLTH

        real(C_DOUBLE), dimension(0:9) :: rRA
        real(C_DOUBLE), dimension(0:9) :: rLRA
        real(C_DOUBLE), dimension(0:9) :: rURA

!       integer :: iOrigin
        integer :: iStartJD, iStartMM, iStartDD, iStartYYYY
        integer :: iEndJD, iEndMM, iEndDD, iEndYYYY
        integer :: iBaseWY, iDayOfWY, iWY, iCurrWY
        integer(kind=T_INT) :: iStreamClass
        logical, dimension(1:9) :: lFlowComponent
        logical(kind=T_LOGICAL) :: lFlowComponentFirstCall
        logical(kind=C_BOOL) :: lUseMedian
        real :: rTemp

        real(C_DOUBLE) :: rCarea

        rCarea = 100.
        lUseMedian = .FALSE._C_BOOL

        lFlowComponent = lFALSE
        lFlowComponentFirstCall = lTRUE

        iStreamClass = 0

        rQ = rTINY
        iYr = 0

        ifail = 0
        CurrentBlock_g = 'HYDROLOGIC_INDICES'

        MA%lInclude = lFALSE
        ML%lInclude = lFALSE
        MH%lInclude = lFALSE
        FL%lInclude = lFALSE
        FH%lInclude = lFALSE
        DL%lInclude = lFALSE
        DH%lInclude = lFALSE
        TA%lInclude = lFALSE
        TL%lInclude = lFALSE
        TH%lInclude = lFALSE
        RA%lInclude = lFALSE

        iNumberOfKeywords = 0

        write (*, 10) TRIM(CurrentBlock_g)
        write (LU_REC, 10) TRIM(CurrentBlock_g)
10      format(/, ' Processing ', a, ' block....')

        icontext = 0
        iseries = 0
        yy1 = -9999
        hh1 = -9999
        yy2 = -9999
        hh2 = -9999
        aname = ' '
        ixcon = 0

! -- The HYDROLOGIC_INDICES block is first parsed.

        do
            ILine_g = ILine_g + 1
            read (LU_TSPROC_CONTROL, '(a)', err=9000, end=9100) cline
            if (cline == ' ') cycle
            if (cline(1:1) == '#') cycle
            if (.NOT. str_compare(cline, "CURRENT_DEFINITIONS") &
                .AND. .NOT. str_compare(cline, "USE_MEDIAN")) then
                call linesplit(ierr, 2)
                if (ierr /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 20) TRIM(aline), TRIM(sString_g)
20                  format('there should be 2 entries on line ', a, ' of file ', a)
                    go to 9800
                end if
            end if
            aoption = cline(left_word(1):right_word(1))
            call casetrans(aoption, 'hi')
            if (aoption /= 'CONTEXT') then
                call test_context(ierr, icontext, acontext)
                if (ierr == -1) then
                    call find_end(ifail)
                    if (ifail == 1) go to 9800
                    return
                else if (ierr == 1) then
                    go to 9800
                end if
                ixcon = 1
            end if

            if (aoption == 'STREAM_CLASSIFICATION') then
                iNumberOfKeywords = iNumberOfKeywords + 1
                call getfile(ierr, cline, sStreamClass, left_word(2), right_word(2))

                select case (TRIM(ADJUSTL(uppercase(sStreamClass))))

                case ("HARSH_INTERMITTENT")
                    iStreamClass = 7
                case ("FLASHY_INTERMITTENT")
                    iStreamClass = 6
                case ("SNOWMELT_PERENNIAL")
                    iStreamClass = 5
                case ("SNOW_RAIN_PERENNIAL")
                    iStreamClass = 4
                case ("GROUNDWATER_PERENNIAL")
                    iStreamClass = 3
                case ("FLASHY_PERENNIAL")
                    iStreamClass = 2
                case ("ALL_STREAMS")
                    iStreamClass = 1
                case default
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 34) TRIM(aline), TRIM(sString_g)
34                  format('Unknown STREAM_CLASSIFICATION specified at line ', a, ' of file ', a)
                    go to 9800
                end select

                write (*, 54) TRIM(sStreamClass)
                write (LU_REC, 54) TRIM(sStreamClass)
54              format(t5, 'STREAM_CLASSIFICATION ', a)

            elseif (aoption == 'FLOW_COMPONENT') then
                iNumberOfKeywords = iNumberOfKeywords + 1
                call getfile(ierr, cline, sFlowComponent, left_word(2), right_word(2))

                ! user is specifying components of flow; need to clear out this
                ! set of flags so that the user can pick his/her own flow components

                select case (TRIM(ADJUSTL(uppercase(sFlowComponent))))

                case ("AVERAGE_MAGNITUDE")
                    lFlowComponent(1) = lTRUE
                case ("LOW_FLOW_MAGNITUDE")
                    lFlowComponent(2) = lTRUE
                case ("HIGH_FLOW_MAGNITUDE")
                    lFlowComponent(3) = lTRUE
                case ("LOW_FLOW_FREQUENCY")
                    lFlowComponent(4) = lTRUE
                case ("HIGH_FLOW_FREQUENCY")
                    lFlowComponent(5) = lTRUE
                case ("LOW_FLOW_DURATION")
                    lFlowComponent(6) = lTRUE
                case ("HIGH_FLOW_DURATION")
                    lFlowComponent(7) = lTRUE
                case ("TIMING")
                    lFlowComponent(8) = lTRUE
                case ("RATE_OF_CHANGE")
                    lFlowComponent(9) = lTRUE
                case default
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 32) TRIM(aline), TRIM(sString_g)
32                  format('Unknown FLOW_COMPONENT specified at line ', a, ' of file ', a)
                    go to 9800
                end select

                write (*, 56) TRIM(sFlowComponent)
                write (LU_REC, 56) TRIM(sFlowComponent)
56              format(t5, 'FLOW_COMPONENT ', a)

            elseif (aoption == 'USE_MEDIAN') then
                lUseMedian = .TRUE._C_BOOL

                ! as a help to the user, print out all of the indices calculated
                ! for a given stream classification (screen output only)
            else if (str_compare(cline, "CURRENT_DEFINITIONS")) then
                do iIndex2 = 0, 6
                    write (*, fmt="(/,/,'Indices calculated for streams in classification ',a,':',/)") &
                        quote(STREAM_CLASSIFICATIONS(iIndex2))
                    do iIndex3 = 1, UBOUND(MA, 1)
                        if (BTEST(MA(iIndex3)%iMask, iIndex2)) then
                            write (*, fmt="(a)") "  MA("//TRIM(int2char(iIndex3)) &
                                //"):: "//TRIM(MA(iIndex3)%sHydrologicIndex)
                        end if
                    end do
                    write (*, fmt="(/)")
                    do iIndex3 = 1, UBOUND(ML, 1)
                        if (BTEST(ML(iIndex3)%iMask, iIndex2)) then
                            write (*, fmt="(a)") "  ML("//TRIM(int2char(iIndex3)) &
                                //"):: "//TRIM(ML(iIndex3)%sHydrologicIndex)
                        end if
                    end do
                    write (*, fmt="(/)")
                    do iIndex3 = 1, UBOUND(MH, 1)
                        if (BTEST(MH(iIndex3)%iMask, iIndex2)) then
                            write (*, fmt="(a)") "  MH("//TRIM(int2char(iIndex3)) &
                                //"):: "//TRIM(MH(iIndex3)%sHydrologicIndex)
                        end if
                    end do
                    write (*, fmt="(/)")
                    do iIndex3 = 1, UBOUND(FL, 1)
                        if (BTEST(FL(iIndex3)%iMask, iIndex2)) then
                            write (*, fmt="(a)") "  FL("//TRIM(int2char(iIndex3)) &
                                //"):: "//TRIM(FL(iIndex3)%sHydrologicIndex)
                        end if
                    end do
                    write (*, fmt="(/)")
                    do iIndex3 = 1, UBOUND(FH, 1)
                        if (BTEST(FH(iIndex3)%iMask, iIndex2)) then
                            write (*, fmt="(a)") "  FH("//TRIM(int2char(iIndex3)) &
                                //"):: "//TRIM(FH(iIndex3)%sHydrologicIndex)
                        end if
                    end do
                    write (*, fmt="(/)")
                    do iIndex3 = 1, UBOUND(DL, 1)
                        if (BTEST(DL(iIndex3)%iMask, iIndex2)) then
                            write (*, fmt="(a)") "  DL("//TRIM(int2char(iIndex3)) &
                                //"):: "//TRIM(DL(iIndex3)%sHydrologicIndex)
                        end if
                    end do
                    write (*, fmt="(/)")
                    do iIndex3 = 1, UBOUND(DH, 1)
                        if (BTEST(DH(iIndex3)%iMask, iIndex2)) then
                            write (*, fmt="(a)") "  DH("//TRIM(int2char(iIndex3)) &
                                //"):: "//TRIM(DH(iIndex3)%sHydrologicIndex)
                        end if
                    end do

                    write (*, fmt="(/)")
                    do iIndex3 = 1, UBOUND(TA, 1)
                        if (BTEST(TA(iIndex3)%iMask, iIndex2)) then
                            write (*, fmt="(a)") "  TA("//TRIM(int2char(iIndex3)) &
                                //"):: "//TRIM(TA(iIndex3)%sHydrologicIndex)
                        end if
                    end do
                    write (*, fmt="(/)")
                    do iIndex3 = 1, UBOUND(TL, 1)
                        if (BTEST(TL(iIndex3)%iMask, iIndex2)) then
                            write (*, fmt="(a)") "  TL("//TRIM(int2char(iIndex3)) &
                                //"):: "//TRIM(TL(iIndex3)%sHydrologicIndex)
                        end if
                    end do
                    write (*, fmt="(/)")
                    do iIndex3 = 1, UBOUND(TH, 1)
                        if (BTEST(TH(iIndex3)%iMask, iIndex2)) then
                            write (*, fmt="(a)") "  TH("//TRIM(int2char(iIndex3)) &
                                //"):: "//TRIM(TH(iIndex3)%sHydrologicIndex)
                        end if
                    end do
                    write (*, fmt="(/)")
                    do iIndex3 = 1, UBOUND(RA, 1)
                        if (BTEST(RA(iIndex3)%iMask, iIndex2)) then
                            write (*, fmt="(a)") "  RA("//TRIM(int2char(iIndex3)) &
                                //"):: "//TRIM(RA(iIndex3)%sHydrologicIndex)
                        end if
                    end do
                end do

            else if (aoption == 'DRAINAGE_AREA') then
                call get_keyword_value(ierr, 2, itemp, rCarea, 'DRAINAGE_AREA')
                if (ierr /= 0) go to 9800
                if (rCarea <= 0.0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 46) TRIM(aline), TRIM(sString_g)
46                  format('DRAINAGE_AREA must be greater than zero at line ', a, ' of file ', a)
                    go to 9800
                end if

            else if (aoption == 'MA') then
                iNumberOfKeywords = iNumberOfKeywords + 1
                call get_keyword_value(ierr, 1, itemp, rTemp, 'MA')
                if (ierr /= 0) go to 9800
                iIndex = itemp
                if (iIndex < LBOUND(MA, dim=1) .OR. iIndex > UBOUND(MA, dim=1)) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 48) LBOUND(MA), UBOUND(MA), TRIM(aline), TRIM(sString_g)
48                  format('Average magnitude (MA) indices must be in the range ', &
                           i3, ' to ', i3, ': line ', a, ' of file ', a)
                    go to 9800
                end if
                MA(iIndex)%lInclude = lTRUE

            else if (aoption == 'MH') then
                iNumberOfKeywords = iNumberOfKeywords + 1
                call get_keyword_value(ierr, 1, itemp, rTemp, 'MH')
                if (ierr /= 0) go to 9800
                iIndex = itemp
                if (iIndex < LBOUND(MH, dim=1) .OR. iIndex > UBOUND(MH, dim=1)) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 60) LBOUND(MH), UBOUND(MH), TRIM(aline), TRIM(sString_g)
60                  format('High-flow Magnitude (MH) indices must be in the range ', &
                           i3, ' to ', i3, ': line ', a, ' of file ', a)
                    go to 9800
                end if
                MH(iIndex)%lInclude = lTRUE

            else if (aoption == 'ML') then
                iNumberOfKeywords = iNumberOfKeywords + 1
                call get_keyword_value(ierr, 1, itemp, rTemp, 'ML')
                if (ierr /= 0) go to 9800
                iIndex = itemp
                if (iIndex < LBOUND(ML, dim=1) .OR. iIndex > UBOUND(ML, dim=1)) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 62) LBOUND(ML), UBOUND(ML), TRIM(aline), TRIM(sString_g)
62                  format('Low-flow Magnitude (ML) indices must be in the range ', &
                           i3, ' to ', i3, ': line ', a, ' of file ', a)
                    go to 9800
                end if
                ML(iIndex)%lInclude = lTRUE

            else if (aoption == 'FL') then
                iNumberOfKeywords = iNumberOfKeywords + 1
                call get_keyword_value(ierr, 1, itemp, rTemp, 'FL')
                if (ierr /= 0) go to 9800
                iIndex = itemp
                if (iIndex < LBOUND(FL, dim=1) .OR. iIndex > UBOUND(FL, dim=1)) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 64) LBOUND(FL), UBOUND(FL), TRIM(aline), TRIM(sString_g)
64                  format('Low-flow Frequency (FL) indices must be in the range ', &
                           i3, ' to ', i3, ': line ', a, ' of file ', a)
                    go to 9800
                end if
                FL(iIndex)%lInclude = lTRUE

            else if (aoption == 'FH') then
                iNumberOfKeywords = iNumberOfKeywords + 1
                call get_keyword_value(ierr, 1, itemp, rTemp, 'FH')
                if (ierr /= 0) go to 9800
                iIndex = itemp
                if (iIndex < LBOUND(FH, dim=1) .OR. iIndex > UBOUND(FH, dim=1)) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 66) LBOUND(FH), UBOUND(FH), TRIM(aline), TRIM(sString_g)
66                  format('High-flow Frequency (FH) indices must be in the range ', &
                           i3, ' to ', i3, ': line ', a, ' of file ', a)
                    go to 9800
                end if
                FH(iIndex)%lInclude = lTRUE

            else if (aoption == 'DL') then
                iNumberOfKeywords = iNumberOfKeywords + 1
                call get_keyword_value(ierr, 1, itemp, rTemp, 'DL')
                if (ierr /= 0) go to 9800
                iIndex = itemp
                if (iIndex < LBOUND(DL, dim=1) .OR. iIndex > UBOUND(DL, dim=1)) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 68) LBOUND(DL), UBOUND(DL), TRIM(aline), TRIM(sString_g)
68                  format('Low-flow Duration (DL) indices must be in the range ', &
                           i3, ' to ', i3, ': line ', a, ' of file ', a)
                    go to 9800
                end if
                DL(iIndex)%lInclude = lTRUE

            else if (aoption == 'DH') then
                iNumberOfKeywords = iNumberOfKeywords + 1
                call get_keyword_value(ierr, 1, itemp, rTemp, 'DH')
                if (ierr /= 0) go to 9800
                iIndex = itemp
                if (iIndex < LBOUND(DH, dim=1) .OR. iIndex > UBOUND(DH, dim=1)) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 70) LBOUND(DH), UBOUND(DH), TRIM(aline), TRIM(sString_g)
70                  format('High-flow Duration (DH) indices must be in the range ', &
                           i3, ' to ', i3, ': line ', a, ' of file ', a)
                    go to 9800
                end if
                DH(iIndex)%lInclude = lTRUE

            else if (aoption == 'TH') then
                iNumberOfKeywords = iNumberOfKeywords + 1
                call get_keyword_value(ierr, 1, itemp, rTemp, 'TH')
                if (ierr /= 0) go to 9800
                iIndex = itemp
                if (iIndex < LBOUND(TH, dim=1) .OR. iIndex > UBOUND(TH, dim=1)) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 72) LBOUND(TH), UBOUND(TH), TRIM(aline), TRIM(sString_g)
72                  format('Timing of high-flow events (TH) indices must be in the range ', &
                           i3, ' to ', i3, ': line ', a, ' of file ', a)
                    go to 9800
                end if
                TH(iIndex)%lInclude = lTRUE

            else if (aoption == 'TL') then
                iNumberOfKeywords = iNumberOfKeywords + 1
                call get_keyword_value(ierr, 1, itemp, rTemp, 'TL')
                if (ierr /= 0) go to 9800
                iIndex = itemp
                if (iIndex < LBOUND(TL, dim=1) .OR. iIndex > UBOUND(TL, dim=1)) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 74) LBOUND(TL), UBOUND(TL), TRIM(aline), TRIM(sString_g)
74                  format('Timing of low-flow events (TL) indices must be in the range ', &
                           i3, ' to ', i3, ': line ', a, ' of file ', a)
                    go to 9800
                end if
                TL(iIndex)%lInclude = lTRUE

            else if (aoption == 'TA') then
                iNumberOfKeywords = iNumberOfKeywords + 1
                call get_keyword_value(ierr, 1, itemp, rTemp, 'TA')
                if (ierr /= 0) go to 9800
                iIndex = itemp
                if (iIndex < LBOUND(TA, dim=1) .OR. iIndex > UBOUND(TA, dim=1)) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 76) LBOUND(TA), UBOUND(TA), TRIM(aline), TRIM(sString_g)
76                  format('Timing of average-flow events (TA) indices must be in the range ', &
                           i3, ' to ', i3, ': line ', a, ' of file ', a)
                    go to 9800
                end if
                TA(iIndex)%lInclude = lTRUE

            else if (aoption == 'RA') then
                iNumberOfKeywords = iNumberOfKeywords + 1
                call get_keyword_value(ierr, 1, itemp, rTemp, 'RA')
                if (ierr /= 0) go to 9800
                iIndex = itemp
                if (iIndex < LBOUND(RA, dim=1) .OR. iIndex > UBOUND(RA, dim=1)) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 78) LBOUND(RA), UBOUND(RA), TRIM(aline), TRIM(sString_g)
78                  format('Rate of change (RA) indices must be in the range ', &
                           i3, ' to ', i3, ': line ', a, ' of file ', a)
                    go to 9800
                end if
                RA(iIndex)%lInclude = lTRUE

            elseif (aoption == 'DATE_1') then
                call get_date(ierr, dd1, mm1, yy1, 'DATE_1')
                if (ierr /= 0) go to 9800
            else if (aoption == 'DATE_2') then
                call get_date(ierr, dd2, mm2, yy2, 'DATE_2')
                if (ierr /= 0) go to 9800
            else if (aoption == 'TIME_1') then
                call get_time(ierr, hh1, nn1, ss1, 'TIME_1')
                if (ierr /= 0) go to 9800
            else if (aoption == 'TIME_2') then
                call get_time(ierr, hh2, nn2, ss2, 'TIME_2')
                if (ierr /= 0) go to 9800
            else if (aoption == 'NEW_G_TABLE_NAME') then
                call get_new_table_name(ierr, iG_TABLE, aname)
                if (ierr /= 0) go to 9800

            else if (aoption == 'SERIES_NAME') then
                call get_series_name(ierr, iseries, 'SERIES_NAME')
                if (ierr /= 0) go to 9800

            else if (aoption == 'CONTEXT') then
                if (ixcon /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 41) TRIM(aline), TRIM(sString_g)
41                  format('CONTEXT keyword in incorrect location at line ', a, ' of file ', a)
                    go to 9800
                end if
                call get_context(ierr, icontext, acontext)
                if (ierr /= 0) go to 9800

            else if (aoption == 'END') then
                exit
            else
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 90) TRIM(aoption), TRIM(CurrentBlock_g), TRIM(aline), TRIM(sString_g)
90              format('unexpected keyword - "', a, '" in ', a, ' block at line ', a, &
                       ' of file ', a)
                go to 9800
            end if
        end do

! -- The block has been read; now it is checked for correctness.

        if (iseries == 0) then
            write (amessage, 210) TRIM(CurrentBlock_g)
210         format('no SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (LEN_TRIM(aname) == 0) then
            write (amessage, 230) TRIM(CurrentBlock_g)
230         format('no NEW_G_TABLE keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (icontext == 0) then
            write (amessage, 220) TRIM(CurrentBlock_g)
220         format('no CONTEXT keyword(s) provided in ', a, ' block.')
            go to 9800
        end if

        call date_check(ierr, yy1, mm1, dd1, hh1, nn1, ss1, yy2, mm2, dd2, hh2, nn2, ss2, &
                        begdays, begsecs, enddays, endsecs)
        if (ierr /= 0) go to 9800
        call beg_end_check(ierr, iseries, begdays, begsecs, enddays, endsecs)
        if (ierr /= 0) go to 9800

        ! now set the status flags for calculation and reporting of the hydrologic indices
        if (iNumberOfKeywords == 0) then

            ! default condition: calculate ALL indices
            MA%lInclude = lTRUE
            ML%lInclude = lTRUE
            MH%lInclude = lTRUE
            FL%lInclude = lTRUE
            FH%lInclude = lTRUE
            DL%lInclude = lTRUE
            DH%lInclude = lTRUE
            TA%lInclude = lTRUE
            TL%lInclude = lTRUE
            TH%lInclude = lTRUE
            RA%lInclude = lTRUE

        else

            ! i.e. if the STREAM_CLASSIFICATION is given as "FLASHY_INTERMITTENT",
            ! iStreamClass is given the value of 6, and the code below extracts the
            ! binary value of bit 6 of the mask value.

            if (lFlowComponent(1)) &
                MA%lInclude = MA%lInclude .OR. BTEST(MA%iMask, iStreamClass)

            if (lFlowComponent(2)) &
                ML%lInclude = ML%lInclude .OR. BTEST(ML%iMask, iStreamClass)

            if (lFlowComponent(3)) &
                MH%lInclude = MH%lInclude .OR. BTEST(MH%iMask, iStreamClass)

            if (lFlowComponent(4)) &
                FL%lInclude = FL%lInclude .OR. BTEST(FL%iMask, iStreamClass)

            if (lFlowComponent(5)) &
                FH%lInclude = FH%lInclude .OR. BTEST(FH%iMask, iStreamClass)

            if (lFlowComponent(6)) &
                DL%lInclude = DL%lInclude .OR. BTEST(DL%iMask, iStreamClass)

            if (lFlowComponent(7)) &
                DH%lInclude = DH%lInclude .OR. BTEST(DH%iMask, iStreamClass)

            if (lFlowComponent(8)) then
                TA%lInclude = TA%lInclude .OR. BTEST(TA%iMask, iStreamClass)
                TL%lInclude = TL%lInclude .OR. BTEST(TL%iMask, iStreamClass)
                TH%lInclude = TH%lInclude .OR. BTEST(TH%iMask, iStreamClass)
            end if

            if (lFlowComponent(9)) &
                RA%lInclude = RA%lInclude .OR. BTEST(RA%iMask, iStreamClass)

        end if

        ! inactivate the indices which require peak flow values to calculate
        FH(11)%lInclude = lFALSE
        DH(22)%lInclude = lFALSE
        DH(23)%lInclude = lFALSE
        DH(24)%lInclude = lFALSE
        TA(3)%lInclude = lFALSE
        TL(3)%lInclude = lFALSE
        TL(4)%lInclude = lFALSE
        TH(3)%lInclude = lFALSE

! -- All is well with the block. The GTABLE is filled with requested statistics.

        call numterms(iterm, ibterm, ieterm, begdays, begsecs, enddays, endsecs, iseries)
        if (iterm == 0) then
            write (amessage, 270) TRIM(series_g(iseries)%name)
270         format('there are no terms in time series "', a, '" between the provided ', &
                   'dates and times.')
            go to 9800
        end if

        do ig = 1, MAXGTABLE
            if (.NOT. gtable_g(ig)%active) go to 300
        end do
        write (amessage, 310)
310     format('no more G_TABLE''s available for data storage - increase MAXGTABLE and ', &
               'recompile program.')
        go to 9800
300     continue

        if ((begdays < series_g(iseries)%days(1)) .OR. &
            ((begdays == series_g(iseries)%days(1)) .AND. &
             (begsecs < series_g(iseries)%secs(1)))) then
            begdays = series_g(iseries)%days(1)
            begsecs = series_g(iseries)%secs(1)
        end if
        iiterm = series_g(iseries)%nterm
        if ((enddays > series_g(iseries)%days(iiterm)) .OR. &
            ((enddays == series_g(iseries)%days(iiterm)) .AND. &
             (endsecs > series_g(iseries)%secs(iiterm)))) then
            enddays = series_g(iseries)%days(iiterm)
            endsecs = series_g(iseries)%secs(iiterm)
        end if

        ! get the Julian date associated with John's "origin" term
!       iOrigin = julian_day(1970, 1, 1)

        gtable_g(ig)%active = lTRUE
        gtable_g(ig)%name = aname
        gtable_g(ig)%series_name = series_g(iseries)%name
        gtable_g(ig)%g_table_header = &
            'Hydrologic Index and description (Olden and Poff, 2003)'

        if (begdays <= -9999999) then
            gtable_g(ig)%rec_begdays = series_g(iseries)%days(1)
            gtable_g(ig)%rec_begsecs = series_g(iseries)%secs(1)
        else
            gtable_g(ig)%rec_begdays = begdays
            gtable_g(ig)%rec_begsecs = begsecs
        end if
        if (enddays >= 9999999) then
            gtable_g(ig)%rec_enddays = series_g(iseries)%days(iiterm)
            gtable_g(ig)%rec_endsecs = series_g(iseries)%secs(iiterm)
        else
            gtable_g(ig)%rec_enddays = enddays
            gtable_g(ig)%rec_endsecs = endsecs
        end if

        iStartJD = gtable_g(ig)%rec_begdays ! + iOrigin
        iEndJD = gtable_g(ig)%rec_enddays ! + iOrigin

        call gregorian_date(iStartJD, iStartYYYY, iStartMM, iStartDD)
        call gregorian_date(iEndJD, iEndYYYY, iEndMM, iEndDD)

!       write(*,fmt="('a ',4(i8,2x))") iStartJD, iStartYYYY, iStartMM, iStartDD
!       write(*,fmt="('b ',4(i8,2x))") iEndJD, iEndYYYY, iEndMM, iEndDD

        ! determine the number of entries that will be in the GTABLE
        iCount = COUNT(MA%lInclude) + COUNT(ML%lInclude) &
                 + COUNT(MH%lInclude) + COUNT(FL%lInclude) &
                 + COUNT(FH%lInclude) + COUNT(DL%lInclude) &
                 + COUNT(DH%lInclude) + COUNT(TL%lInclude) &
                 + COUNT(TH%lInclude) + COUNT(RA%lInclude) &
                 + COUNT(TA%lInclude)

        allocate (gtable_g(ig)%rValue(iCount), stat=iStat)
        allocate (gtable_g(ig)%sDescription(iCount), stat=iStat)

        ! establish first water year
        call water_year_and_day(iStartJD, iBaseWY, iDayOfWY)
!       write(*,fmt="('c ',4(i8,2x))") iStartJD, iBaseWY, iDayOfWY

        iCurrWY = 0

        ! The c++ code we're calling assumes that the data are organized
        ! by WATER YEAR, not by CALENDAR year. Therefore, we must reformat
        ! the time series to meet this assumption.
        iCount = 0
        do j = 1, iiterm
!         call water_year_and_day(series_g(iseries)%days(j) + iOrigin, iWY, iDayOfWY)
            call water_year_and_day(series_g(iseries)%days(j), iWY, iDayOfWY)
            if (iCurrWY /= iWY) then
                iCurrWY = iWY
                iYr(iCount) = iWY
                iCount = iCount + 1
            end if
!         write(*,fmt="('d ',4(i8,2x))") series_g(iseries)%days(j) + iOrigin, iWY, iDayOfWY
            iIndex = iWY - iBaseWY
!         write(*,fmt="('e ',i8,2x,i8,2x,f12.3)") iIndex, iDayOfWY, series_g(iseries)%val(j)
            rQ(iDayOfWY, iIndex) = series_g(iseries)%val(j)
        end do

        ! make the actual call to the C++ routine provided by
        call compute_hi(lUseMedian=lUseMedian, rCarea=rCarea, rNearHuge=cdNEARHUGE, &
                        rLowerPercentile=25._C_DOUBLE, rUpperPercentile=75._C_DOUBLE, iYr=iYr, rQ=rQ, rMA=rMA, &
                        rLMA=rLMA, rUMA=rUMA, &
                        rML=rML, rLML=rLML, rUML=rUML, &
                        rMH=rMH, rLMH=rLMH, rUMH=rUMH, &
                        rFL=rFL, rLFL=rLFL, rUFL=rUFL, &
                        rFH=rFH, rLFH=rLFH, rUFH=rUFH, &
                        rDL=rDL, rLDL=rLDL, rUDL=rUDL, &
                        rDH=rDH, rLDH=rLDH, rUDH=rUDH, &
                        rTA=rTA, rLTA=rLTA, rUTA=rUTA, &
                        rTL=rTL, rLTL=rLTL, rUTL=rUTL, &
                        rTH=rTH, rLTH=rLTH, rUTH=rUTH, &
                        rRA=rRA, rLRA=rLRA, rURA=rURA)

        MA(1:45)%rValue = rMA(1:45)
        ML(1:22)%rValue = rML(1:22)
        MH(1:27)%rValue = rMH(1:27)
        FL(1:3)%rValue = rFL(1:3)
        FH(1:11)%rValue = rFH(1:11)
        DL(1:20)%rValue = rDL(1:20)
        DH(1:24)%rValue = rDH(1:24)
        TA(1:3)%rValue = rTA(1:3)
        TL(1:4)%rValue = rTL(1:4)
        TH(1:3)%rValue = rTH(1:3)
        RA(1:9)%rValue = rRA(1:9)

        iCount = 1
        do j = 1, SIZE(MA%lInclude)
            if (MA(j)%lInclude) then
                gtable_g(ig)%rValue(iCount) = MA(j)%rValue
                write (aaa, fmt="(i3)") j
                gtable_g(ig)%sDescription(iCount) = "MA"//TRIM(ADJUSTL(aaa))//": " &
                                                    //TRIM(MA(j)%sHydrologicIndex)
!           write(*,fmt="(a,t75,3g14.3)") gtable_g(ig)%sDescription(iCount),rLMA(j),MA(j)%rValue,rUMA(j)
                iCount = iCount + 1
            end if
        end do

        do j = 1, SIZE(ML%lInclude)
            if (ML(j)%lInclude) then
                gtable_g(ig)%rValue(iCount) = ML(j)%rValue
                write (aaa, fmt="(i3)") j
                gtable_g(ig)%sDescription(iCount) = "ML"//TRIM(ADJUSTL(aaa))//": " &
                                                    //TRIM(ML(j)%sHydrologicIndex)
!           write(*,fmt="(a,t75,3g14.3)") gtable_g(ig)%sDescription(iCount),rLML(j),ML(j)%rValue,rUML(j)
                iCount = iCount + 1
            end if
        end do

        do j = 1, SIZE(MH%lInclude)
            if (MH(j)%lInclude) then
                gtable_g(ig)%rValue(iCount) = MH(j)%rValue
                write (aaa, fmt="(i3)") j
                gtable_g(ig)%sDescription(iCount) = "MH"//TRIM(ADJUSTL(aaa))//": " &
                                                    //TRIM(MH(j)%sHydrologicIndex)
!           write(*,fmt="(a,t75,3g14.3)") gtable_g(ig)%sDescription(iCount),rLMH(j),MH(j)%rValue,rUMH(j)
                iCount = iCount + 1
            end if
        end do

        do j = 1, SIZE(FL%lInclude)
            if (FL(j)%lInclude) then
                gtable_g(ig)%rValue(iCount) = FL(j)%rValue
                write (aaa, fmt="(i3)") j
                gtable_g(ig)%sDescription(iCount) = "FL"//TRIM(ADJUSTL(aaa))//": " &
                                                    //TRIM(FL(j)%sHydrologicIndex)
!           write(*,fmt="(a,t75,3g14.3)") gtable_g(ig)%sDescription(iCount),rLFL(j),FL(j)%rValue,rUFL(j)
                iCount = iCount + 1
            end if
        end do

        do j = 1, SIZE(FH%lInclude)
            if (FH(j)%lInclude) then
                gtable_g(ig)%rValue(iCount) = FH(j)%rValue
                write (aaa, fmt="(i3)") j
                gtable_g(ig)%sDescription(iCount) = "FH"//TRIM(ADJUSTL(aaa))//": " &
                                                    //TRIM(FH(j)%sHydrologicIndex)
!           write(*,fmt="(a,t75,3g14.3)") gtable_g(ig)%sDescription(iCount),rLFH(j),FH(j)%rValue,rUFH(j)
                iCount = iCount + 1
            end if
        end do

        do j = 1, SIZE(DL%lInclude)
            if (DL(j)%lInclude) then
                gtable_g(ig)%rValue(iCount) = DL(j)%rValue
                write (aaa, fmt="(i3)") j
                gtable_g(ig)%sDescription(iCount) = "DL"//TRIM(ADJUSTL(aaa))//": " &
                                                    //TRIM(DL(j)%sHydrologicIndex)
!           write(*,fmt="(a,t75,3g14.3)") gtable_g(ig)%sDescription(iCount),rLDL(j),DL(j)%rValue,rUDL(j)
                iCount = iCount + 1
            end if
        end do

        do j = 1, SIZE(DH%lInclude)
            if (DH(j)%lInclude) then
                gtable_g(ig)%rValue(iCount) = DH(j)%rValue
                write (aaa, fmt="(i3)") j
                gtable_g(ig)%sDescription(iCount) = "DH"//TRIM(ADJUSTL(aaa))//": " &
                                                    //TRIM(DH(j)%sHydrologicIndex)
!           write(*,fmt="(a,t75,3g14.3)") gtable_g(ig)%sDescription(iCount),rLDH(j),DH(j)%rValue,rUDH(j)
                iCount = iCount + 1
            end if
        end do

        do j = 1, SIZE(TA%lInclude)
            if (TA(j)%lInclude) then
                gtable_g(ig)%rValue(iCount) = TA(j)%rValue
                write (aaa, fmt="(i3)") j
                gtable_g(ig)%sDescription(iCount) = "TA"//TRIM(ADJUSTL(aaa))//": " &
                                                    //TRIM(TA(j)%sHydrologicIndex)
!           write(*,fmt="(a,t75,3g14.3)") gtable_g(ig)%sDescription(iCount),rLTA(j),TA(j)%rValue,rUTA(j)
                iCount = iCount + 1
            end if
        end do

        do j = 1, SIZE(TL%lInclude)
            if (TL(j)%lInclude) then
                gtable_g(ig)%rValue(iCount) = TL(j)%rValue
                write (aaa, fmt="(i3)") j
                gtable_g(ig)%sDescription(iCount) = "TL"//TRIM(ADJUSTL(aaa))//": " &
                                                    //TRIM(TL(j)%sHydrologicIndex)
!           write(*,fmt="(a,t75,3g14.3)") gtable_g(ig)%sDescription(iCount),rLTL(j),TL(j)%rValue,rUTL(j)
                iCount = iCount + 1
            end if
        end do

        do j = 1, SIZE(TH%lInclude)
            if (TH(j)%lInclude) then
                gtable_g(ig)%rValue(iCount) = TH(j)%rValue
                write (aaa, fmt="(i3)") j
                gtable_g(ig)%sDescription(iCount) = "TH"//TRIM(ADJUSTL(aaa))//": " &
                                                    //TRIM(TH(j)%sHydrologicIndex)
!           write(*,fmt="(a,t75,3g14.3)") gtable_g(ig)%sDescription(iCount),rLTH(j),TH(j)%rValue,rUTH(j)
                iCount = iCount + 1
            end if
        end do

        do j = 1, SIZE(RA%lInclude)
            if (RA(j)%lInclude) then
                gtable_g(ig)%rValue(iCount) = RA(j)%rValue
                write (aaa, fmt="(i3)") j
                gtable_g(ig)%sDescription(iCount) = "RA"//TRIM(ADJUSTL(aaa))//": " &
                                                    //TRIM(RA(j)%sHydrologicIndex)
!           write(*,fmt="(a,t75,3g14.3)") gtable_g(ig)%sDescription(iCount),rLRA(j),gtable_g(ig)%rValue(iCount),rURA(j)
                iCount = iCount + 1
            end if
        end do

        write (6, 380) TRIM(series_g(iseries)%name), TRIM(gtable_g(ig)%name)
        write (LU_REC, 380) TRIM(series_g(iseries)%name), TRIM(gtable_g(ig)%name)
380     format(/, t5, 'Hydrologic indices for time series "', a, '" stored in ', &
                'G_TABLE "', a, '".')
        return

9000    call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9010) TRIM(aline), TRIM(sString_g)
9010    format('cannot read line ', a, ' of TSPROC input file ', a)
        go to 9800
9100    continue
        call addquote(sInfile_g, sString_g)
        write (amessage, 9110) TRIM(sString_g), TRIM(CurrentBlock_g)
9110    format('unexpected end encountered to TSPROC input file ', a, ' while ', &
               ' reading ', a, ' block.')
        go to 9800

9800    call write_message(leadspace='yes', error='yes')
        call write_message(iunit=LU_REC, leadspace='yes')
        ifail = 1

        return

    end subroutine compute_hydrologic_indices

    subroutine time_base(ifail)

! -- Subroutine TIME_BASE spatially interpolates one time series to the sample dates/times
!    of another.

        use tsp_data_structures
        use tsp_utilities
        use tsp_command_processors

        implicit none

        integer, intent(out) :: ifail

        integer ierr, icontext, i, iseries, j, itbseries, ntermtb, ndaysbtb, &
            nsecsbtb, ndaysftb, nsecsftb, ntermos, ndaysbos, nsecsbos, ndaysfos, nsecsfos, istart, &
            intday, intsec, ixcon
        real :: valinterp
        character(len=iTSNAMELENGTH) :: aname
        character(15) aline
        character(25) aoption
        character(25) acontext(MAXCONTEXT)
        logical(kind=T_LOGICAL) :: lDatesAreOK

!interface
!        subroutine time_interp_s(ifail,nbore,ndays,nsecs,value,intday, &
!        intsec,rnear,rconst,valinterp,extrap,direction,startindex)
!          integer, intent(out)                    :: ifail
!          integer, intent(in)                     :: nbore
!          integer, intent(in), dimension(nbore)   :: ndays,nsecs
!          real, intent(in), dimension(nbore)      :: value
!          integer, intent(in)                     :: intday,intsec
!          real, intent(in)                          :: rnear,rconst
!          real, intent(out)                       :: valinterp
!          character (len=*), intent(in),optional  :: extrap
!          character (len=*), intent(in),optional  :: direction
!          integer, intent(inout), optional        :: startindex
!        end subroutine time_interp_s
!end interface

        ifail = 0
        CurrentBlock_g = 'NEW_TIME_BASE'

        write (*, 10) TRIM(CurrentBlock_g)
        write (LU_REC, 10) TRIM(CurrentBlock_g)
10      format(/, ' Processing ', a, ' block....')

        itbseries = 0
        icontext = 0
        iseries = 0
        aname = ' '
        ixcon = 0

! -- The NEW_TIME_BASE block is first parsed.

        do
            ILine_g = ILine_g + 1
            read (LU_TSPROC_CONTROL, '(a)', err=9000, end=9100) cline
            if (cline == ' ') cycle
            if (cline(1:1) == '#') cycle
            call linesplit(ierr, 2)
            if (ierr /= 0) then
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 20) TRIM(aline), TRIM(sString_g)
20              format('there should be 2 entries on line ', a, ' of file ', a)
                go to 9800
            end if
            aoption = cline(left_word(1):right_word(1))
            call casetrans(aoption, 'hi')
            if (aoption /= 'CONTEXT') then
                call test_context(ierr, icontext, acontext)
                if (ierr == -1) then
                    call find_end(ifail)
                    if (ifail == 1) go to 9800
                    return
                else if (ierr == 1) then
                    go to 9800
                end if
                ixcon = 1
            end if
            if (aoption == 'NEW_SERIES_NAME') then
                call get_new_series_name(ierr, aname)
                if (ierr /= 0) go to 9800
            else if (aoption == 'SERIES_NAME') then
                call get_series_name(ierr, iseries, 'SERIES_NAME')
                if (ierr /= 0) go to 9800
            else if (aoption == 'TB_SERIES_NAME') then
                call get_series_name(ierr, itbseries, 'TB_SERIES_NAME')
                if (ierr /= 0) go to 9800
            else if (aoption == 'CONTEXT') then
                if (ixcon /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 41) TRIM(aline), TRIM(sString_g)
41                  format('CONTEXT keyword in incorrect location at line ', a, ' of file ', a)
                    go to 9800
                end if
                call get_context(ierr, icontext, acontext)
                if (ierr /= 0) go to 9800
            else if (aoption == 'END') then
                go to 200
            else
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 90) TRIM(aoption), TRIM(CurrentBlock_g), TRIM(aline), TRIM(sString_g)
90              format('unexpected keyword - "', a, '" in ', a, ' block at line ', a, &
                       ' of file ', a)
                go to 9800
            end if
        end do

! -- The block has been read; now it is checked for correctness.

200     continue
        if (iseries == 0) then
            write (amessage, 210) TRIM(CurrentBlock_g)
210         format('no SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (aname == ' ') then
            write (amessage, 230) TRIM(CurrentBlock_g)
230         format('no NEW_SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (itbseries == 0) then
            write (amessage, 218) TRIM(CurrentBlock_g)
218         format('no TB_SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (icontext == 0) then
            write (amessage, 220) TRIM(CurrentBlock_g)
220         format('no CONTEXT keyword(s) provided in ', a, ' block.')
            go to 9800
        end if

        lDatesAreOK = lTRUE

        ! time and date parameters for BASE SERIES
        ntermtb = series_g(itbseries)%nterm
        ndaysbtb = series_g(itbseries)%days(1)
        nsecsbtb = series_g(itbseries)%secs(1)
        ndaysftb = series_g(itbseries)%days(ntermtb)
        nsecsftb = series_g(itbseries)%secs(ntermtb)

        ! time and date parameters for the OTHER SERIES
        ntermos = series_g(iseries)%nterm
        ndaysbos = series_g(iseries)%days(1)
        nsecsbos = series_g(iseries)%secs(1)
        ndaysfos = series_g(iseries)%days(ntermos)
        nsecsfos = series_g(iseries)%secs(ntermos)

        ! check to see that the base series timespan matches that of the
        ! series to be interpolated
        if ((ndaysbtb < ndaysbos) .OR. &
            ((ndaysbtb == ndaysbos) .AND. (nsecsbtb < nsecsbos))) lDatesAreOK = lFALSE
        if ((ndaysftb > ndaysfos) .OR. &
            ((ndaysftb == ndaysfos) .AND. (nsecsftb > nsecsfos))) lDatesAreOK = lFALSE

        if (.NOT. lDatesAreOK) then

            write (*, fmt="(/,/,'*** incomparable time series ***')")
            write (*, fmt="(/,t25,'TB SERIES',t45,'SERIES')")
            write (*, fmt="(a,t25,i8,t45,i8)") "Number of terms:", ntermtb, ntermos
            write (*, fmt="(a,t25,i8,t45,i8)") "First day:", ndaysbtb, ndaysbos
            write (*, fmt="(a,t25,i8,t45,i8)") "First time:", nsecsbtb, nsecsbos
            write (*, fmt="(a,t25,i8,t45,i8)") "Last day:", ndaysftb, ndaysfos
            write (*, fmt="(a,t25,i8,t45,i8)") "Last time:", nsecsftb, nsecsfos

            write (amessage, fmt= &
             "('the time span of the time base series is greater than that of the ', &
             'series to be interpolated. Reduce the time span of the time base series to ', &
             'that of the series to be interpolated using a REDUCE_SPAN block.')")

            goto 9800
        end if

! -- Memory is now allocated for the new series prior to its being filled.

        do i = 1, MAXSERIES
            if (.NOT. series_g(i)%active) go to 250
        end do
        write (amessage, 240)
240     format('no more time series available for data storage - increase MAXSERIES and ', &
               'recompile program.')
        go to 9800
250     allocate (series_g(i)%days(ntermtb), series_g(i)%secs(ntermtb), &
                  series_g(i)%val(ntermtb), stat=ierr)
        if (ierr /= 0) then
            write (amessage, 560)
560         format('cannot allocate memory for another time series.')
            go to 9800
        end if
        series_g(i)%active = .TRUE.
        series_g(i)%name = aname
        series_g(i)%nterm = ntermtb
        series_g(i)%type = 'ts'
        do j = 1, ntermtb
            series_g(i)%days(j) = series_g(itbseries)%days(j)
        end do
        do j = 1, ntermtb
            series_g(i)%secs(j) = series_g(itbseries)%secs(j)
        end do

! -- Temporal interpolation is now undertaken.

        istart = 0
        do j = 1, ntermtb
            intday = series_g(i)%days(j)
            intsec = series_g(i)%secs(j)
            call time_interp_s(ierr, ntermos, series_g(iseries)%days, series_g(iseries)%secs, &
                               series_g(iseries)%val, intday, intsec, 1.0E20, 0.0, valinterp, startindex=istart)
            series_g(i)%val(j) = valinterp
        end do

        write (*, 580) TRIM(aname)
        write (LU_REC, 580) TRIM(aname)
580     format(t5, 'New series "', a, '" successfully calculated.')
        return

9000    call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9010) TRIM(aline), TRIM(sString_g)
9010    format('cannot read line ', a, ' of TSPROC input file ', a)
        go to 9800
9100    continue
        call addquote(sInfile_g, sString_g)
        write (amessage, 9110) TRIM(sString_g)
9110    format('unexpected end encountered to TSPROC input file ', a, ' while ', &
               ' reading TIME_BASE block.')
        go to 9800

9800    call write_message(leadspace='yes', error='yes')
        call write_message(iunit=LU_REC, leadspace='yes')
        ifail = 1

        return

    end subroutine time_base

    subroutine volume(ifail)

! -- Subroutine VOLUME accumulates volumes between user-specified dates and times.

        use tsp_data_structures
        use tsp_utilities
        use tsp_command_processors

        implicit none

        integer, intent(out) :: ifail

        integer ierr, icontext, iseries, itunit, iunit, jline, ndate, iv, nsterm, nsdays1, &
            nssecs1, nsdays2, nssecs2, dd, mm, yy, hh, nn, ss, ndays1, nsecs1, ndays2, nsecs2, itemp, ixcon
        real(kind=T_DBL) :: factor, fac, volcalc
        character(len=iTSNAMELENGTH) :: aname
        character(15) aline
        character(25) aoption
        character(25) sKeyword
        character(120) datefile
        character(25) acontext(MAXCONTEXT)
        character(len=iTSNAMELENGTH) :: sCurrentSeriesName
        logical :: lAutoDateAnnual
        logical :: lAutoDateMonthly
        logical :: lDatesFromFile

        integer, dimension(:), allocatable :: iFromDates, iToDates

        ifail = 0
        CurrentBlock_g = 'VOLUME_CALCULATION'

        write (*, 10) TRIM(CurrentBlock_g)
        write (LU_REC, 10) TRIM(CurrentBlock_g)
10      format(/, ' Processing ', a, ' block....')

        icontext = 0
        iseries = 0
        aname = ' '
        factor = 1.0
        itunit = 0
        datefile = ' '
        ixcon = 0
        iunit = 0

        lDatesFromFile = lTRUE
        lAutoDateMonthly = lFALSE
        lAutoDateAnnual = lFALSE

! -- The VOLUME_CALCULATION block is first parsed.

        do
            ILine_g = ILine_g + 1
            read (LU_TSPROC_CONTROL, '(a)', err=9000, end=9100) cline
            if (cline == ' ') cycle
            if (cline(1:1) == '#') cycle
            call linesplit(ierr, 2)
            if (ierr /= 0) then
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 20) TRIM(aline), TRIM(sString_g)
20              format('there should be 2 entries on line ', a, ' of file ', a)
                go to 9800
            end if
            aoption = cline(left_word(1):right_word(1))
            call casetrans(aoption, 'hi')

            sKeyword = cline(left_word(2):right_word(2))
            call casetrans(sKeyword, 'hi')

            if (aoption /= 'CONTEXT') then
                call test_context(ierr, icontext, acontext)
                if (ierr == -1) then
                    call find_end(ifail)
                    if (ifail == 1) go to 9800
                    return
                else if (ierr == 1) then
                    go to 9800
                end if
                ixcon = 1
            end if

            if (aoption == 'SERIES_NAME') then
                call get_series_name(ierr, iseries, 'SERIES_NAME')
                if (ierr /= 0) go to 9800
                sCurrentSeriesName = series_g(iseries)%name
            else if (aoption == 'CONTEXT') then
                if (ixcon /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 41) TRIM(aline), TRIM(sString_g)
41                  format('CONTEXT keyword in incorrect location at line ', a, ' of file ', a)
                    go to 9800
                end if
                call get_context(ierr, icontext, acontext)
                if (ierr /= 0) go to 9800
            else if (aoption == 'NEW_V_TABLE_NAME') then
                call get_new_table_name(ierr, 2, aname)
                if (ierr /= 0) go to 9800
            else if (aoption == 'FLOW_TIME_UNITS') then
                call get_time_units(ierr, itunit, 1)
                if (ierr /= 0) go to 9800
            else if (aoption == 'FACTOR') then
                call get_keyword_value(ierr, 2, itemp, factor, 'FACTOR')
                if (ierr /= 0) go to 9800
            else if (aoption == 'DATE_FILE') then
                call getfile(ierr, cline, datefile, left_word(2), right_word(2))
                if (ierr /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 140) TRIM(aline), TRIM(sString_g)
140                 format('cannot read date file name from line ', a, ' of file ', a)
                    go to 9800
                end if
                call addquote(datefile, sString_g)
                write (*, 145) TRIM(sString_g)
                write (LU_REC, 145) TRIM(sString_g)
145             format(t5, 'DATE_FILE ', a)

            else if (aoption == 'AUTOMATIC_DATES') then

                if (TRIM(sKeyword) == 'ANNUAL') then
                    datefile = "{auto generated: ANNUAL}"
                    sString_g = TRIM(datefile)
                    write (*, 147) TRIM(sString_g)
                    write (LU_REC, 147) TRIM(sString_g)
147                 format(t5, 'DATE_FILE ', a)
                    lAutoDateAnnual = lTRUE
                    lDatesFromFile = lFALSE
                elseif (TRIM(sKeyword) == 'MONTHLY') then
                    datefile = "{auto generated: MONTHLY}"
                    sString_g = TRIM(datefile)
                    write (*, 149) TRIM(sString_g)
                    write (LU_REC, 149) TRIM(sString_g)
149                 format(t5, 'DATE_FILE ', a)
                    lAutoDateMonthly = lTRUE
                    lDatesFromFile = lFALSE
                else
                    call num2char(ILine_g, aline)
                    amessage = 'Unhandled keyword "'//TRIM(sKeyword)//'" in ' &
                               //TRIM(CurrentBlock_g)//' at line '//TRIM(aline)
                    goto 9800
                end if

            else if (aoption == 'END') then
                go to 200
            else
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 90) TRIM(aoption), TRIM(CurrentBlock_g), TRIM(aline), TRIM(sString_g)
90              format('unexpected keyword - "', a, '" in ', a, ' block at line ', a, &
                       ' of file ', a)
                go to 9800
            end if
        end do

200     continue

! -- The block has been read; now it is checked for absences.

        if (iseries == 0) then
            write (amessage, 210) TRIM(CurrentBlock_g)
210         format('no SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (aname == ' ') then
            write (amessage, 230) TRIM(CurrentBlock_g)
230         format('no NEW_V_TABLE_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (itunit == 0) then
            write (amessage, 218) TRIM(CurrentBlock_g)
218         format('no FLOW_TIME_UNITS keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (icontext == 0) then
            write (amessage, 220) TRIM(CurrentBlock_g)
220         format('no CONTEXT keyword(s) provided in ', a, ' block.')
            go to 9800
        end if
        if (datefile == ' ' .AND. lDatesFromFile) then
            write (amessage, 240) TRIM(CurrentBlock_g)
240         format('no DATE_FILE or AUTOMATIC_DATES keywords provided in ', a, ' block.')
            go to 9800
        end if

        ! define the time bounds of the V_TABLE on the basis of the
        ! time bounds found in the underlying series
        nsterm = series_g(iseries)%nterm
        nsdays1 = series_g(iseries)%days(1)
        nssecs1 = series_g(iseries)%secs(1)
        nsdays2 = series_g(iseries)%days(nsterm)
        nssecs2 = series_g(iseries)%secs(nsterm)

! -- The date file is now opened and the number of lines within it read.
        if (lDatesFromFile) then

            iunit = nextunit()
            call addquote(datefile, sString_g)
            open (unit=iunit, file=datefile, status='old', iostat=ierr)
            if (ierr /= 0) then
                write (amessage, 300) TRIM(sString_g)
300             format('cannot open dates file ', a)
                go to 9800
            end if
            write (6, 305) TRIM(sString_g)
            write (LU_REC, 305) TRIM(sString_g)
305         format(t5, 'Reading dates file ', a, '....')
            jline = 0
            ndate = 0
            do
                jline = jline + 1
                read (iunit, '(a)', end=350) cline
                if (cline == ' ') cycle
                if (cline(1:1) == '#') cycle
                ndate = ndate + 1
            end do
350         continue
            if (ndate == 0) then
                write (amessage, 360) TRIM(sString_g)
360             format('no dates found in dates file ', a)
                go to 9800
            end if
            rewind (unit=iunit, iostat=ierr)
            if (ierr /= 0) then
                write (amessage, 370) TRIM(sString_g)
370             format('cannot rewind dates file ', a)
                go to 9800
            end if

        else ! dates are automatically calculated

            if (lAutoDateAnnual) then

                call make_date_list(iSampleDates=series_g(iseries)%days, &
                                    iFromDates=iFromDates, &
                                    iToDates=iToDates, &
                                    sListType="ANNUAL")

            else

                call make_date_list(iSampleDates=series_g(iseries)%days, &
                                    iFromDates=iFromDates, &
                                    iToDates=iToDates, &
                                    sListType="MONTHLY")

            end if

            ndate = SIZE(iFromDates, 1)

        end if

! -- Memory is now allocated for the v_table.

        do iv = 1, MAXVTABLE
            if (.NOT. vtable_g(iv)%active) go to 380
        end do
        write (amessage, 390)
390     format('no more v_tables available for data storage - increase MAXVTABLE and ', &
               'recompile program.')
        go to 9800
380     continue
        vtable_g(iv)%active = .TRUE.
        vtable_g(iv)%name = aname
        vtable_g(iv)%series_name = series_g(iseries)%name
        allocate (vtable_g(iv)%days1(ndate), vtable_g(iv)%secs1(ndate), vtable_g(iv)%days2(ndate), &
                  vtable_g(iv)%secs2(ndate), vtable_g(iv)%vol(ndate), stat=ierr)
        if (ierr /= 0) then
            write (amessage, 395)
395         format('cannot allocate memory for storage of v_table data.')
            go to 9800
        end if

        if (itunit == 1) then
            fac = 86400.0
        else if (itunit == 2) then
            fac = 1440.0
        else if (itunit == 3) then
            fac = 24.0
        else if (itunit == 4) then
            fac = 1.0
        else if (itunit == 5) then
            fac = 1.0 * 12 / 365.25
        else
            fac = 1.0 / 365.25
        end if

! -- The date file is now re-read and volumes calculated.

        jline = 0
        ndate = 0
        volclc: do

            if (lDatesFromFile) then
                jline = jline + 1

                ! read in a line from date file
                read (iunit, '(a)', end=500) cline
                if (cline == ' ') cycle
                if (cline(1:1) == '#') cycle
                ndate = ndate + 1
                call linesplit(ierr, 4)
                if (ierr /= 0) then
                    call num2char(jline, aline)
                    write (amessage, 410) TRIM(aline), TRIM(sString_g)
410                 format('four entries expected on line ', a, ' of dates file ', a)
                    go to 9800
                end if
                call char2date(ierr, cline(left_word(1):right_word(1)), dd, mm, yy)
                if (ierr /= 0) go to 9200

                ! calculate start date and time for current line of datefile
!           ndays1=numdays(1,1,1970,dd,mm,yy)
                ndays1 = julian_day(iMonth=mm, iDay=dd, iYear=yy)
                call char2time(ierr, cline(left_word(2):right_word(2)), hh, nn, ss, ignore_24=1)
                if (ierr /= 0) go to 9200
                nsecs1 = numsecs(0, 0, 0, hh, nn, ss)
                if (nsecs1 >= 86400) then
                    nsecs1 = nsecs1 - 86400
                    ndays1 = ndays1 + 1
                end if
                call char2date(ierr, cline(left_word(3):right_word(3)), dd, mm, yy)
                if (ierr /= 0) go to 9200

                ! calculate end date and time for current line of datefile
!           ndays2=numdays(1,1,1970,dd,mm,yy)
                ndays2 = julian_day(iMonth=mm, iDay=dd, iYear=yy)
                call char2time(ierr, cline(left_word(4):right_word(4)), hh, nn, ss, ignore_24=1)
                if (ierr /= 0) go to 9200
                nsecs2 = numsecs(0, 0, 0, hh, nn, ss)
                if (nsecs2 >= 86400) then
                    nsecs2 = nsecs2 - 86400
                    ndays2 = ndays2 + 1
                end if

                ! perform sanity checks on start and end date from datefile
                if ((ndays1 > ndays2) .OR. &
                    ((ndays1 == ndays2) .AND. (nsecs1 >= nsecs2))) then
                    call num2char(jline, aline)
                    write (amessage, 420) TRIM(aline), TRIM(sString_g)
420                 format('first date/time must precede second date/time at line ', a, &
                           ' of file ', a)
                    go to 9800
                end if
                if ((ndays1 < nsdays1) .OR. &
                    ((ndays1 == nsdays1) .AND. (nsecs1 < nssecs1))) then
                    call num2char(jline, aline)
                    write (amessage, 425) TRIM(aline), TRIM(sString_g), TRIM(series_g(iseries)%name)
425                 format('the first date/time on line ', a, ' of file ', a, ' predates the ', &
                           'commencement of time series "', a, '".')
                    go to 9800
                end if
                if ((ndays2 > nsdays2) .OR. &
                    ((ndays2 == nsdays2) .AND. (nsecs2 > nssecs2))) then
                    call num2char(jline, aline)
                    write (amessage, 426) TRIM(aline), TRIM(sString_g), TRIM(series_g(iseries)%name)
426                 format('the second date/time on line ', a, ' of file ', a, ' postdates the ', &
                           'end of time series "', a, '".')
                    go to 9800
                end if

            else ! dates "automatically" calculated

                ndate = ndate + 1

                if (ndate > UBOUND(iFromDates, 1) &
                    .OR. ndate > UBOUND(iToDates, 1)) then
                    ndate = ndate - 1
                    exit volclc
                end if

                ndays1 = iFromDates(ndate)
                ndays2 = iToDates(ndate)

!           nsecs1=numsecs(0,0,0,hh,nn,ss)
                nsecs1 = numsecs(0, 0, 0, 0, 0, 0)
                nsecs2 = numsecs(0, 0, 0, 23, 59, 59)

            end if

            ! assign starting/ending date and time to V_TABLE entry
            vtable_g(iv)%days1(ndate) = ndays1
            vtable_g(iv)%secs1(ndate) = nsecs1
            vtable_g(iv)%days2(ndate) = ndays2
            vtable_g(iv)%secs2(ndate) = nsecs2

            ! make the call to calculate volume within given date/time range
            call volume_interp_s(ierr, nsterm, series_g(iseries)%days, series_g(iseries)%secs, &
                                 series_g(iseries)%val, ndays1, nsecs1, ndays2, nsecs2, volcalc, fac)

            ! assign volume to the associated date/time range
            vtable_g(iv)%vol(ndate) = volcalc * factor

        end do volclc

500     continue

        vtable_g(iv)%nterm = ndate

        if (lDatesFromFile) then
            close (unit=iunit)
            write (*, 430) TRIM(sString_g)
            write (LU_REC, 430) TRIM(sString_g)
430         format(t5, 'File ', a, ' read ok.')
        end if

        write (*, 440) TRIM(aname)
        write (LU_REC, 440) TRIM(aname)
440     format(t5, 'Volumes calculated and stored in v_table "', a, '".')
        return

9000    call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9010) TRIM(aline), TRIM(sString_g)
9010    format('cannot read line ', a, ' of TSPROC input file ', a)
        go to 9800
9100    continue
        call addquote(sInfile_g, sString_g)
        write (amessage, 9110) TRIM(sString_g), TRIM(CurrentBlock_g)
9110    format('unexpected end encountered to TSPROC input file ', a, ' while ', &
               ' reading ', a, ' block.')
        go to 9800
9200    continue
        call num2char(jline, aline)
        write (amessage, 9210) TRIM(aline), TRIM(sString_g)
9210    format('erroneous date or time at line ', a, ' of file ', a)
        go to 9800
9800    call write_message(leadspace='yes', error='yes')
        call write_message(iunit=LU_REC, leadspace='yes')
        ifail = 1
        if (iunit /= 0) close (unit=iunit, iostat=ierr)

    end subroutine volume

    subroutine time_duration(ifail)

! -- Subroutine TIME_DURATION calculates exceedance durations for certain flows.

        use tsp_data_structures
        use tsp_utilities
        use tsp_command_processors

        implicit none

        integer, intent(out) :: ifail

        logical on, oldon
        integer ierr, icontext, iseries, itunit, iflow, id, i, ndays, nsecs, j, oldndays, oldnsecs, &
            nnterm, ixcon, iuo
        real rtemp, fac, duration, fflow, vval, oldvval, timediff, accumulation, timedelay
        character(len=iTSNAMELENGTH) :: aname, atemp
        character(15) aline
        character(25) aoption
        character(25) acontext(MAXCONTEXT)

        ifail = 0
        CurrentBlock_g = 'EXCEEDANCE_TIME'

        write (*, 10) TRIM(CurrentBlock_g)
        write (LU_REC, 10) TRIM(CurrentBlock_g)
10      format(/, ' Processing ', a, ' block....')

        icontext = 0
        iseries = 0
        aname = ' '
        itunit = 0
        iflow = 0
        ixcon = 0
        iuo = -999
        do i = 1, MAXTEMPDURFLOW
            tempdtable_g%tdelay(i) = -1.1E36
        end do

! -- The EXCEEDANCE-TIME block is first parsed.

        do
            ILine_g = ILine_g + 1
            read (LU_TSPROC_CONTROL, '(a)', err=9000, end=9100) cline
            if (cline == ' ') cycle
            if (cline(1:1) == '#') cycle
            call linesplit(ierr, 2)
            if (ierr /= 0) then
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 20) TRIM(aline), TRIM(sString_g)
20              format('there should be 2 entries on line ', a, ' of file ', a)
                go to 9800
            end if
            aoption = cline(left_word(1):right_word(1))
            call casetrans(aoption, 'hi')

            if (aoption /= 'CONTEXT') then
                call test_context(ierr, icontext, acontext)
                if (ierr == -1) then
                    call find_end(ifail)
                    if (ifail == 1) go to 9800
                    return
                else if (ierr == 1) then
                    go to 9800
                end if
                ixcon = 1
            end if

            if (aoption == 'SERIES_NAME') then
                call get_series_name(ierr, iseries, 'SERIES_NAME')
                if (ierr /= 0) go to 9800
            else if (aoption == 'CONTEXT') then
                if (ixcon /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 41) TRIM(aline), TRIM(sString_g)
41                  format('CONTEXT keyword in incorrect location at line ', a, ' of file ', a)
                    go to 9800
                end if
                call get_context(ierr, icontext, acontext)
                if (ierr /= 0) go to 9800

            else if (aoption == 'NEW_E_TABLE_NAME') then
                call get_new_table_name(ierr, 3, aname)
                if (ierr /= 0) go to 9800

            else if (aoption == 'EXCEEDENCE_TIME_UNITS') then
                call get_time_units(ierr, itunit, 2)
                if (ierr /= 0) go to 9800

            else if (aoption == 'EXCEEDANCE_TIME_UNITS') then
                call get_time_units(ierr, itunit, 2)
                if (ierr /= 0) go to 9800

            else if (aoption == 'UNDER_OVER') then
                call getfile(ierr, cline, atemp, left_word(2), right_word(2))
                if (ierr /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 57) TRIM(aline), TRIM(sString_g)
57                  format('cannot read UNDER_OVER from line ', a, ' of file ', a)
                    go to 9800
                end if
                call casetrans(atemp, 'lo')
                if (atemp(1:5) == 'under') then
                    iuo = 0
                else if (atemp(1:5) == 'over') then
                    iuo = 1
                else
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 58) TRIM(aline), TRIM(sString_g)
58                  format('UNDER_OVER must be "under" or "over" at line ', a, ' of file ', a)
                    go to 9800
                end if
                call addquote(atemp, sString_g)
                write (*, 59) TRIM(sString_g)
                write (LU_REC, 59) TRIM(sString_g)
59              format(t5, 'UNDER_OVER ', a)

            else if (aoption == 'FLOW') then
                iflow = iflow + 1
                if (iflow > MAXTEMPDURFLOW) then
                    call num2char(MAXTEMPDURFLOW, aline)
                    write (amessage, 30) TRIM(aline), TRIM(CurrentBlock_g)
30                  format('a maximum of ', a, ' FLOWs are allowed in an ', a, ' block.')
                    go to 9800
                end if
                call char2num(ierr, cline(left_word(2):right_word(2)), rtemp)
                if (ierr /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 120) TRIM(aline), TRIM(sString_g)
120                 format('cannot read flow from line ', a, ' of file ', a)
                    go to 9800
                end if
                write (*, 130) cline(left_word(2):right_word(2))
                write (LU_REC, 130) cline(left_word(2):right_word(2))
130             format(t5, 'FLOW ', a)
                tempdtable_g%flow(iflow) = rtemp

            else if (aoption == 'DELAY') then
                if (iflow == 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 320) TRIM(aline), TRIM(sString_g)
320                 format('DELAY not preceeded by FLOW at line ', a, ' of file ', a)
                    go to 9800
                end if
                if (tempdtable_g%tdelay(iflow) > -1.0E36) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 330) TRIM(aline), TRIM(sString_g)
330                 format('more than one DELAY associated with FLOW at line ', a, ' of file ', a)
                    go to 9800
                end if
                call char2num(ierr, cline(left_word(2):right_word(2)), rtemp)
                if (ierr /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 335) TRIM(aline), TRIM(sString_g)
335                 format('cannot read time delay from line ', a, ' of file ', a)
                    go to 9800
                end if
                if (rtemp < 0.0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 336) TRIM(aline), TRIM(sString_g)
336                 format('time delay cannot be negative at line ', a, ' of file ', a)
                    go to 9800
                end if
                write (*, 340) cline(left_word(2):right_word(2))
                write (LU_REC, 340) cline(left_word(2):right_word(2))
340             format(t5, 'DELAY ', a)
                tempdtable_g%tdelay(iflow) = rtemp

            else if (aoption == 'END') then
                go to 200

            else
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 90) TRIM(aoption), TRIM(CurrentBlock_g), TRIM(aline), TRIM(sString_g)
90              format('unexpected keyword - "', a, '" in ', a, ' block at line ', a, &
                       ' of file ', a)
                go to 9800
            end if
        end do

200     continue

! -- The block has been read; now it is checked for absences.

        if (iseries == 0) then
            write (amessage, 210) TRIM(CurrentBlock_g)
210         format('no SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (aname == ' ') then
            write (amessage, 230) TRIM(CurrentBlock_g)
230         format('no NEW_E_TABLE_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (itunit == 0) then
            write (amessage, 218) TRIM(CurrentBlock_g)
218         format('no EXCEEDENCE_TIME_UNITS or EXEEDANCE_TIME_UNITS keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (icontext == 0) then
            write (amessage, 220) TRIM(CurrentBlock_g)
220         format('no CONTEXT keyword(s) provided in ', a, ' block.')
            go to 9800
        end if
        if (iflow == 0) then
            write (amessage, 225) TRIM(CurrentBlock_g)
225         format('no FLOW keywords provided in ', a, ' block.')
            go to 9800
        end if
        do i = 1, iflow
            if (tempdtable_g%tdelay(i) > -1.0E36) go to 360
        end do
        go to 400
360     do i = 1, iflow
            if (tempdtable_g%tdelay(i) < -1.0E36) then
                write (amessage, 370) TRIM(CurrentBlock_g)
370             format('if any FLOW is associated with a DELAY, than all flows must be associated ', &
                       'with a DELAY in ', a, ' block')
                go to 9800
            end if
        end do
400     continue
        if (series_g(iseries)%nterm == 1) then
            write (amessage, 250) TRIM(series_g(iseries)%name)
250         format('cannot calculate exceedance times because time series "', a, &
                   '" has only one term.')
            go to 9800
        end if

! -- Space is now allocated in a non-temporary E_TABLE.

        do id = 1, MAXDTABLE
            if (.NOT. dtable_g(id)%active) go to 380
        end do
        write (amessage, 390)
390     format('no more e_tables available for data storage - increase MAXDTABLE and ', &
               'recompile program.')
        go to 9800
380     continue
        dtable_g(id)%active = .TRUE.
        dtable_g(id)%name = aname
        dtable_g(id)%series_name = series_g(iseries)%name
        dtable_g(id)%nterm = iflow
        allocate (dtable_g(id)%flow(iflow), dtable_g(id)%time(iflow), &
                  dtable_g(id)%tdelay(iflow), stat=ierr)
        if (ierr /= 0) then
            write (amessage, 395)
395         format('cannot allocate memory for storage of e_table data.')
            go to 9800
        end if
        do i = 1, iflow
            dtable_g(id)%flow(i) = tempdtable_g%flow(i)
            if (tempdtable_g%tdelay(i) < -1.0E36) then
                dtable_g(id)%tdelay(i) = 0.0
            else
                dtable_g(id)%tdelay(i) = tempdtable_g%tdelay(i)
            end if
        end do

        if (itunit == 1) then
            fac = 1.0
            dtable_g(id)%time_units = 'secs'
        else if (itunit == 2) then
            fac = 1.0 / 60.0
            dtable_g(id)%time_units = 'mins'
        else if (itunit == 3) then
            fac = 1.0 / 3600.0
            dtable_g(id)%time_units = 'hrs'
        else if (itunit == 4) then
            fac = 1.0 / 86400.0
            dtable_g(id)%time_units = 'days'
        else if (itunit == 5) then
            fac = 1.0 / 86400.0 / (356.25 / 12.0)
            dtable_g(id)%time_units = 'mths'
        else
            fac = 1.0 / 86400.0 / 365.25
            dtable_g(id)%time_units = 'yrs'
        end if
        if ((iuo == -999) .OR. (iuo == 1)) then
            dtable_g(id)%under_over = 1
            iuo = 1
        else
            dtable_g(id)%under_over = 0
            iuo = 0
        end if

! -- Durations are now calculated for each flow.

        nnterm = series_g(iseries)%nterm
        do i = 1, iflow
            timedelay = dtable_g(id)%tdelay(i) / fac
            duration = 0.0
            accumulation = 0.0
            fflow = dtable_g(id)%flow(i)
            vval = series_g(iseries)%val(1)
            ndays = series_g(iseries)%days(1)
            nsecs = series_g(iseries)%secs(1)
            if (iuo == 1) then
                if (vval >= fflow) then
                    on = .TRUE.
                else
                    on = .FALSE.
                end if
            else
                if (vval <= fflow) then
                    on = .TRUE.
                else
                    on = .FALSE.
                end if
            end if
            do j = 2, nnterm
                oldon = on
                oldvval = vval
                oldndays = ndays
                oldnsecs = nsecs
                vval = series_g(iseries)%val(j)
                ndays = series_g(iseries)%days(j)
                nsecs = series_g(iseries)%secs(j)
                if (iuo == 1) then
                    if (vval >= fflow) then
                        on = .TRUE.
                    else
                        on = .FALSE.
                    end if
                else
                    if (vval <= fflow) then
                        on = .TRUE.
                    else
                        on = .FALSE.
                    end if
                end if
                if ((on) .AND. (oldon)) then
!             duration=duration+timediff
                    timediff = float(ndays - oldndays) * 86400.0 + float(nsecs - oldnsecs)
                    accumulation = accumulation + timediff
                else if ((on) .AND. (.NOT. oldon)) then
                    timediff = float(ndays - oldndays) * 86400.0 + float(nsecs - oldnsecs)
                    accumulation = timediff * (vval - fflow) / (vval - oldvval)
!             duration=duration+timediff*(vval-fflow)/(vval-oldvval)
                else if ((oldon) .AND. (.NOT. on)) then
                    timediff = float(ndays - oldndays) * 86400.0 + float(nsecs - oldnsecs)
                    accumulation = accumulation + timediff * (oldvval - fflow) / (oldvval - vval)
                    duration = duration + MAX(0.0, accumulation - timedelay)
                    accumulation = 0.0
!             duration=duration+timediff*(oldvval-fflow)/(oldvval-vval)
                end if
            end do
            duration = duration + MAX(0.0, accumulation - timedelay)
            dtable_g(id)%time(i) = duration * fac
        end do

! -- The total time encompassed by the time series is now calculated.

        oldndays = series_g(iseries)%days(1)
        oldnsecs = series_g(iseries)%secs(1)
        ndays = series_g(iseries)%days(nnterm)
        nsecs = series_g(iseries)%secs(nnterm)
        dtable_g(id)%total_time = (float(ndays - oldndays) * 86400.0 + float(nsecs - oldnsecs)) * fac

        write (*, 440) TRIM(aname)
        write (LU_REC, 440) TRIM(aname)
440     format(t5, 'Exceedance times calculated and stored in e_table "', a, '".')
        return

9000    call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9010) TRIM(aline), TRIM(sString_g)
9010    format('cannot read line ', a, ' of TSPROC input file ', a)
        go to 9800
9100    continue
        call addquote(sInfile_g, sString_g)
        write (amessage, 9110) TRIM(sString_g), TRIM(CurrentBlock_g)
9110    format('unexpected end encountered to TSPROC input file ', a, ' while ', &
               ' reading ', a, ' block.')
        go to 9800
9800    call write_message(leadspace='yes', error='yes')
        call write_message(iunit=LU_REC, leadspace='yes')
        ifail = 1

        return

    end subroutine time_duration

    subroutine flow_duration(ifail)

! -- Subroutine FLOW_DURATION calculates exceedance flows for a set of
! -- user-specified exceedance quantiles.

        use tsp_data_structures
        use tsp_utilities
        use tsp_command_processors

        implicit none

        integer, intent(out) :: ifail

        logical(kind=T_LOGICAL) :: lUseDefaultProbabilities
        integer ierr, icontext, iseries, itunit, iPercentExceeded, &
            ixcon, iuo, iCount, iOrigin, iStat, iIndex
        integer dd1, mm1, yy1, hh1, nn1, ss1, dd2, mm2, yy2, hh2, nn2, ss2, &
            begdays, begsecs, enddays, endsecs, iiterm, ig
        character(len=iTSNAMELENGTH) :: aname
        real, dimension(:), allocatable :: rResultVector
        real, dimension(13), parameter :: &
            rDefaultExceedanceProbabilities = [ &
            99.5, 99., 98., 95., 90., 75., 50., 25., 10., 5., 2., 1., 0.5 &
            ]
        real, dimension(:), allocatable :: rCustomExceedanceProbabilities
        integer(kind=T_INT), dimension(:), allocatable :: iSortOrder

        character(15) aline
        character(25) aoption
        character(25) acontext(MAXCONTEXT)
        character(len=256) :: sRecord, sItem

        integer :: iStartJD, iStartMM, iStartDD, iStartYYYY
        integer :: iEndJD, iEndMM, iEndDD, iEndYYYY

        ifail = 0
        CurrentBlock_g = 'FLOW_DURATION'

        write (*, 10) TRIM(CurrentBlock_g)
        write (LU_REC, 10) TRIM(CurrentBlock_g)
10      format(/, ' Processing ', a, ' block....')

        lUseDefaultProbabilities = lTRUE
        icontext = 0
        iseries = 0
        aname = ' '
        itunit = 0
        iPercentExceeded = 0
        ixcon = 0
        iuo = -999
        yy1 = -9999
        hh1 = -9999
        yy2 = -9999
        hh2 = -9999

! -- The FLOW_DURATION block is first parsed.

        do
            ILine_g = ILine_g + 1
            read (LU_TSPROC_CONTROL, fmt='(a256)', err=9000, end=9100) sRecord
            if (LEN_TRIM(sRecord) == 0) cycle
            if (sRecord(1:1) == '#') cycle
            cline = REPEAT(" ", 400)
            cline = ADJUSTL(sRecord)
            call linesplit(ierr, 2)
            if (ierr /= 0) then
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 20) TRIM(aline), TRIM(sString_g)
20              format('there should be 2 entries on line ', a, ' of file ', a)
                go to 9800
            end if
            aoption = cline(left_word(1):right_word(1))
            call casetrans(aoption, 'hi')
            if (aoption /= 'CONTEXT') then
                call test_context(ierr, icontext, acontext)
                if (ierr == -1) then
                    call find_end(ifail)
                    if (ifail == 1) go to 9800
                    return
                else if (ierr == 1) then
                    go to 9800
                end if
                ixcon = 1
            end if

            if (aoption == 'SERIES_NAME') then
                call get_series_name(ierr, iseries, 'SERIES_NAME')
                if (ierr /= 0) go to 9800

            else if (aoption == 'CONTEXT') then
                if (ixcon /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 41) TRIM(aline), TRIM(sString_g)
41                  format('CONTEXT keyword in incorrect location at line ', a, ' of file ', a)
                    go to 9800
                end if
                call get_context(ierr, icontext, acontext)
                if (ierr /= 0) go to 9800

            else if (aoption == 'NEW_G_TABLE_NAME' &
                     .OR. aoption == 'NEW_TABLE_NAME') then

                call get_new_table_name(ierr, iG_TABLE, aname)
                if (ierr /= 0) go to 9800

            else if (aoption == 'EXCEEDANCE_PROBABILITIES') then
                lUseDefaultProbabilities = lFALSE

                iCount = count_fields(cline) - 1
                allocate (rCustomExceedanceProbabilities(iCount), stat=iStat)
                call assert(iStat == 0, &
                            "Problem allocating memory for storing custom exceedance probabilities", &
                            TRIM(__FILE__), __LINE__)
                allocate (iSortOrder(iCount), stat=iStat)
                call assert(iStat == 0, &
                            "Problem allocating memory for storing sort order for exceedance probabilities", &
                            TRIM(__FILE__), __LINE__)

                sRecord = ADJUSTL(sRecord)
                ! read and throw away first value
                call chomp(sRecord, sItem, " ")
                ! now read in each of the probabilities
                do iIndex = 1, iCount
                    call chomp(sRecord, sItem)
                    read (sItem, *) rCustomExceedanceProbabilities(iIndex)
                end do

                call quick_sort(rCustomExceedanceProbabilities, iSortOrder)
                ! sort returns values in ascending order

                write (*, fmt="(a)") TRIM(cline)
                write (LU_REC, fmt="(a)") TRIM(cline)

            elseif (aoption == 'DATE_1') then
                call get_date(ierr, dd1, mm1, yy1, 'DATE_1')
                if (ierr /= 0) go to 9800

            else if (aoption == 'DATE_2') then
                call get_date(ierr, dd2, mm2, yy2, 'DATE_2')
                if (ierr /= 0) go to 9800

            else if (aoption == 'TIME_1') then
                call get_time(ierr, hh1, nn1, ss1, 'TIME_1')
                if (ierr /= 0) go to 9800

            else if (aoption == 'TIME_2') then
                call get_time(ierr, hh2, nn2, ss2, 'TIME_2')
                if (ierr /= 0) go to 9800

            else if (aoption == 'END') then
                go to 200
            else
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 90) TRIM(aoption), TRIM(CurrentBlock_g), TRIM(aline), TRIM(sString_g)
90              format('unexpected keyword - "', a, '" in ', a, ' block at line ', a, &
                       ' of file ', a)
                go to 9800
            end if
        end do

200     continue

        call date_check(ierr, yy1, mm1, dd1, hh1, nn1, ss1, yy2, mm2, dd2, hh2, nn2, ss2, &
                        begdays, begsecs, enddays, endsecs)
        if (ierr /= 0) go to 9800
        call beg_end_check(ierr, iseries, begdays, begsecs, enddays, endsecs)
        if (ierr /= 0) go to 9800

! -- The block has been read; now it is checked for absences.

        ! has user provided a SERIES_NAME?
        if (iseries == 0) then
            write (amessage, 210) TRIM(CurrentBlock_g)
210         format('no SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if

        ! has user specified a new TABLE NAME?
        if (LEN_TRIM(aname) == 0) then
            write (amessage, 230) TRIM(CurrentBlock_g)
230         format('no NEW_G_TABLE_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if

        ! is a CONTEXT provided?
        if (icontext == 0) then
            write (amessage, 220) TRIM(CurrentBlock_g)
220         format('no CONTEXT keyword(s) provided in ', a, ' block.')
            go to 9800
        end if

        ! does the specified time series have enough data points
        ! to bother with?
        if (series_g(iseries)%nterm < 10) then
            write (amessage, 250) TRIM(series_g(iseries)%name)
250         format('cannot calculate exceedance times because time series "', a, &
                   '" has less than 10 values.')
            go to 9800
        end if

! -- Find an inactive G_TABLE; activate.
        do ig = 1, MAXGTABLE
            if (.NOT. gtable_g(ig)%active) go to 300
        end do
        write (amessage, 310)
310     format('no more G_TABLE''s available for data storage - increase MAXGTABLE and ', &
               'recompile program.')
        go to 9800
300     continue

        if ((begdays < series_g(iseries)%days(1)) .OR. &
            ((begdays == series_g(iseries)%days(1)) .AND. &
             (begsecs < series_g(iseries)%secs(1)))) then
            begdays = series_g(iseries)%days(1)
            begsecs = series_g(iseries)%secs(1)
        end if
        iiterm = series_g(iseries)%nterm
        if ((enddays > series_g(iseries)%days(iiterm)) .OR. &
            ((enddays == series_g(iseries)%days(iiterm)) .AND. &
             (endsecs > series_g(iseries)%secs(iiterm)))) then
            enddays = series_g(iseries)%days(iiterm)
            endsecs = series_g(iseries)%secs(iiterm)
        end if

        ! get the Julian date associated with John's "origin" term
        iOrigin = julian_day(1970, 1, 1)

        if (begdays < -9999999) then
            gtable_g(ig)%rec_begdays = series_g(iseries)%days(1)
            gtable_g(ig)%rec_begsecs = series_g(iseries)%secs(1)
        else
            gtable_g(ig)%rec_begdays = begdays
            gtable_g(ig)%rec_begsecs = begsecs
        end if
        if (enddays > 9999999) then
            gtable_g(ig)%rec_enddays = series_g(iseries)%days(iiterm)
            gtable_g(ig)%rec_endsecs = series_g(iseries)%secs(iiterm)
        else
            gtable_g(ig)%rec_enddays = enddays
            gtable_g(ig)%rec_endsecs = endsecs
        end if

        iStartJD = gtable_g(ig)%rec_begdays + iOrigin
        iEndJD = gtable_g(ig)%rec_enddays + iOrigin

        call gregorian_date(iStartJD, iStartYYYY, iStartMM, iStartDD)
        call gregorian_date(iEndJD, iEndYYYY, iEndMM, iEndDD)

        gtable_g(ig)%active = lTRUE
        gtable_g(ig)%name = aname
        gtable_g(ig)%series_name = series_g(iseries)%name
        write (gtable_g(ig)%g_table_header, &
               fmt="(a,a,' (',i2.2,'/',i2.2,'/',i4.4,' to ',i2.2,'/',i2.2,'/',i4.4,')')") &
            'Flow-duration curve for series ', &
            quote(series_g(iseries)%name), &
            iStartMM, iStartDD, iStartYYYY, &
            iEndMM, iEndDD, iEndYYYY

        if (lUseDefaultProbabilities) then

            iCount = SIZE(rDefaultExceedanceProbabilities, 1)
            allocate (rResultVector(iCount), stat=iStat)
            rResultVector = quantile_vector(rData=series_g(iseries)%val, &
                                            rQuantile=(1.0 - (rDefaultExceedanceProbabilities / 100.)))
        else

            iCount = SIZE(rCustomExceedanceProbabilities, 1)
            allocate (rResultVector(iCount), stat=iStat)
            rResultVector = quantile_vector(rData=series_g(iseries)%val, &
                                            rQuantile=(1.0 - (rCustomExceedanceProbabilities / 100.)))
        end if

        allocate (gtable_g(ig)%rValue(iCount), stat=iStat)
        allocate (gtable_g(ig)%sDescription(iCount), stat=iStat)

        if (lUseDefaultProbabilities) then

            do iIndex = 1, iCount
                gtable_g(ig)%rValue(iIndex) = rResultVector(iIndex)
                gtable_g(ig)%sDescription(iIndex) = &
                    TRIM(asChar(rDefaultExceedanceProbabilities(iIndex)))//"% of flows" &
                    //" exceed: "
            end do
        else
            do iIndex = 1, iCount
                gtable_g(ig)%rValue(iIndex) = rResultVector(iCount - iIndex + 1)
                gtable_g(ig)%sDescription(iIndex) = &
                    TRIM(asChar(rCustomExceedanceProbabilities(iCount - iIndex + 1))) &
                    //"% of flows exceed: "
            end do
        end if

        write (6, 380) TRIM(series_g(iseries)%name), TRIM(gtable_g(ig)%name)
        write (LU_REC, 380) TRIM(series_g(iseries)%name), TRIM(gtable_g(ig)%name)
380     format(/, t5, 'Flow duration for time series "', a, '" stored in ', &
                'G_TABLE "', a, '".')
        return

9000    call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9010) TRIM(aline), TRIM(sString_g)
9010    format('cannot read line ', a, ' of TSPROC input file ', a)
        go to 9800
9100    continue
        call addquote(sInfile_g, sString_g)
        write (amessage, 9110) TRIM(sString_g), TRIM(CurrentBlock_g)
9110    format('unexpected end encountered to TSPROC input file ', a, ' while ', &
               ' reading ', a, ' block.')
        go to 9800

9800    call write_message(leadspace='yes', error='yes')
        call write_message(iunit=LU_REC, leadspace='yes')
        ifail = 1
        return

    end subroutine flow_duration

    subroutine displace(ifail)

! -- Subroutine DISPLACE moves a time series by a user-supplied number of time increments.

        use tsp_data_structures
        use tsp_utilities
        use tsp_command_processors

        implicit none

        integer, intent(out) :: ifail

        integer ierr, icontext, iseries, lag, itemp, nsterm, ilags, j, nsecs, ndays, &
            dd, mm, yy, hh, nn, ss, i, ixcon
        real fill, rtemp
        character(len=iTSNAMELENGTH) :: aname
        character(15) aline
        character(25) aoption
        character(25) acontext(MAXCONTEXT)

        ifail = 0
        CurrentBlock_g = 'SERIES_DISPLACE'

        write (*, 10) TRIM(CurrentBlock_g)
        write (LU_REC, 10) TRIM(CurrentBlock_g)
10      format(/, ' Processing ', a, ' block....')

        icontext = 0
        iseries = 0
        aname = ' '
        lag = -9999
        fill = -1.1E36
        ixcon = 0

! -- The SERIES_DISPLACE block is first parsed.

        do
            ILine_g = ILine_g + 1
            read (LU_TSPROC_CONTROL, '(a)', err=9000, end=9100) cline
            if (cline == ' ') cycle
            if (cline(1:1) == '#') cycle
            call linesplit(ierr, 2)
            if (ierr /= 0) then
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 20) TRIM(aline), TRIM(sString_g)
20              format('there should be 2 entries on line ', a, ' of file ', a)
                go to 9800
            end if
            aoption = cline(left_word(1):right_word(1))
            call casetrans(aoption, 'hi')
            if (aoption /= 'CONTEXT') then
                call test_context(ierr, icontext, acontext)
                if (ierr == -1) then
                    call find_end(ifail)
                    if (ifail == 1) go to 9800
                    return
                else if (ierr == 1) then
                    go to 9800
                end if
                ixcon = 1
            end if
            if (aoption == 'NEW_SERIES_NAME') then
                call get_new_series_name(ierr, aname)
                if (ierr /= 0) go to 9800
            else if (aoption == 'SERIES_NAME') then
                call get_series_name(ierr, iseries, 'SERIES_NAME')
                if (ierr /= 0) go to 9800
            else if (aoption == 'CONTEXT') then
                if (ixcon /= 0) then
                    call num2char(ILine_g, aline)
                    call addquote(sInfile_g, sString_g)
                    write (amessage, 41) TRIM(aline), TRIM(sString_g)
41                  format('CONTEXT keyword in incorrect location at line ', a, ' of file ', a)
                    go to 9800
                end if
                call get_context(ierr, icontext, acontext)
                if (ierr /= 0) go to 9800
            else if (aoption == 'LAG_INCREMENT') then
                call get_keyword_value(ierr, 1, lag, rtemp, 'LAG_INCREMENT')
                if (ierr /= 0) go to 9800
            else if (aoption == 'FILL_VALUE') then
                call get_keyword_value(ierr, 2, itemp, fill, 'FILL_VALUE')
                if (ierr /= 0) go to 9800
            else if (aoption == 'END') then
                go to 200
            else
                call num2char(ILine_g, aline)
                call addquote(sInfile_g, sString_g)
                write (amessage, 90) TRIM(aoption), TRIM(CurrentBlock_g), TRIM(aline), TRIM(sString_g)
90              format('unexpected keyword - "', a, '" in ', a, ' block at line ', a, &
                       ' of file ', a)
                go to 9800
            end if
        end do

! -- The block has been read; now it is checked for correctness.

200     continue
        if (iseries == 0) then
            write (amessage, 210) TRIM(CurrentBlock_g)
210         format('no SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (aname == ' ') then
            write (amessage, 230) TRIM(CurrentBlock_g)
230         format('no NEW_SERIES_NAME keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (icontext == 0) then
            write (amessage, 220) TRIM(CurrentBlock_g)
220         format('no CONTEXT keyword(s) provided in ', a, ' block.')
            go to 9800
        end if
        if (lag == -9999) then
            write (amessage, 225) TRIM(CurrentBlock_g)
225         format('no LAG_INCREMENT keyword provided in ', a, ' block.')
            go to 9800
        end if
        if (fill < -1.0E36) then
            write (amessage, 240) TRIM(CurrentBlock_g)
240         format('no FILL_VALUE keyword provided in ', a, ' block.')
            go to 9800
        end if

! -- The DISPLACE operation can only be performed if the input time series has equal
!    increments. This is now tested.

        nsterm = series_g(iseries)%nterm
        if (nsterm < ABS(lag) + 1) then
            call num2char(nsterm, aline)
            write (amessage, 250) TRIM(series_g(iseries)%name), TRIM(aline)
250         format('series "', a, '" has only ', a, ' terms. This is insufficient to perform ', &
                   'the requested displacement operation.')
            go to 9800
        end if
        if (nsterm > 2) then
            ilags = (series_g(iseries)%days(2) - series_g(iseries)%days(1)) * 86400 + &
                    series_g(iseries)%secs(2) - series_g(iseries)%secs(1)
            do j = 2, nsterm - 1
                nsecs = series_g(iseries)%secs(j) + ilags
                ndays = series_g(iseries)%days(j)
260             if (nsecs >= 86400) then
                    ndays = ndays + 1
                    nsecs = nsecs - 86400
                    go to 260
                end if
                if ((nsecs /= series_g(iseries)%secs(j + 1)) .OR. &
                    (ndays /= series_g(iseries)%days(j + 1))) then
!               call newdate(series_g(iseries)%days(j),1,1,1970,dd,mm,yy)
                    call gregorian_date(iJD=series_g(iseries)%days(j), &
                                        iMonth=mm, &
                                        iDay=dd, &
                                        iYear=yy)

                    nsecs = series_g(iseries)%secs(j)
                    hh = nsecs / 3600
                    nn = (nsecs - hh * 3600) / 60
                    ss = nsecs - hh * 3600 - nn * 60
                    if (datespec == 1) then
                        write (amessage, 280) TRIM(series_g(iseries)%name), dd, mm, yy, hh, nn, ss
                    else
                        write (amessage, 280) TRIM(series_g(iseries)%name), mm, dd, yy, hh, nn, ss
                    end if
280                 format('time interval between terms in time series "', a, '" is not ', &
                           'constant. The first discrepancy occurs following the sample taken on ', &
                           i2.2, '/', i2.2, '/', i4, ' at ', i2.2, ':', i2.2, ':', i2.2)
                    go to 9800
                end if
            end do
        end if

! -- Space for a new series is allocated.

        do i = 1, MAXSERIES
            if (.NOT. series_g(i)%active) go to 515
        end do
        write (amessage, 510)
510     format('no more time series available for data storage - increase MAXSERIES and ', &
               'recompile program, or erase a series using an ERASE_SERIES block.')
        go to 9800

515     continue
        allocate (series_g(i)%days(nsterm), series_g(i)%secs(nsterm), &
                  series_g(i)%val(nsterm), stat=ierr)
        if (ierr /= 0) then
            write (amessage, 550)
550         format('cannot allocate memory for another time series.')
            go to 9800
        end if
        series_g(i)%active = .TRUE.
        series_g(i)%name = aname
        series_g(i)%nterm = nsterm
        series_g(i)%type = 'ts'
        do j = 1, nsterm
            series_g(i)%days(j) = series_g(iseries)%days(j)
        end do
        do j = 1, nsterm
            series_g(i)%secs(j) = series_g(iseries)%secs(j)
        end do
        if (lag == 0) then
            do j = 1, nsterm
                series_g(i)%val(j) = series_g(iseries)%val(j)
            end do
        else if (lag > 0) then
            do j = 1 + lag, nsterm
                series_g(i)%val(j) = series_g(iseries)%val(j - lag)
            end do
            do j = 1, lag
                series_g(i)%val(j) = fill
            end do
        else if (lag < 0) then
            lag = -lag
            do j = 1, nsterm - lag
                series_g(i)%val(j) = series_g(iseries)%val(j + lag)
            end do
            do j = nsterm - lag + 1, nsterm
                series_g(i)%val(j) = fill
            end do
        end if

        write (*, 580) TRIM(aname)
        write (LU_REC, 580) TRIM(aname)
580     format(t5, 'Series "', a, '" successfully calculated.')
        return

9000    call num2char(ILine_g, aline)
        call addquote(sInfile_g, sString_g)
        write (amessage, 9010) TRIM(aline), TRIM(sString_g)
9010    format('cannot read line ', a, ' of TSPROC input file ', a)
        go to 9800
9100    continue
        call addquote(sInfile_g, sString_g)
        write (amessage, 9110) TRIM(sString_g), TRIM(CurrentBlock_g)
9110    format('unexpected end encountered to TSPROC input file ', a, ' while ', &
               ' reading ', a, ' block.')
        go to 9800

9800    call write_message(leadspace='yes', error='yes')
        call write_message(iunit=LU_REC, leadspace='yes')
        ifail = 1

        return

    end subroutine displace

end module tsp_time_series_processors
