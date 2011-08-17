!
!     This common block contains buffers for in memory storage of
!     records from multiple watershed files.
!
!     + + + PARAMETERS + + +
      INTEGER     CONREC,MXWDM
      PARAMETER   (CONREC=10,MXWDM=5)

      COMMON /CFBUFF/ WIBUFF,RECNO,WDMFUN,NXTPOS,PREPOS,FREPOS, &
                      WDMCNT,WDMOPN,MAXREC
      INTEGER     RECNO(CONREC),WDMFUN(CONREC),NXTPOS(CONREC),  &
                  PREPOS(CONREC),FREPOS,                        &
                  WDMCNT,WDMOPN(MXWDM),MAXREC(MXWDM)
      INTEGER*4   WIBUFF(512,CONREC)

      SAVE   /CFBUFF/

      EQUIVALENCE (WIBUFF,WRBUFF)
      REAL        WRBUFF(512,CONREC)

