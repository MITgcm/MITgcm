#include "CPP_OPTIONS.h"

C--  File remove_mean.F:
C--   Contents
C--   o REMOVE_MEAN_RL
C--   o REMOVE_MEAN_RS

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: REMOVE_MEAN_RL
C     !INTERFACE:
      SUBROUTINE REMOVE_MEAN_RL(
     I                myNr,
     U                arrFld,
     I                arrhFac, arrMask, arrArea, arrDr,
     I                arrName, myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     Correct for global-mean imbalance of global "_RL" array "arrFld"
C      (i.e., output arrFld has zero mean):
C     Apply either (if arrDr(1) > 0) uniform correction
C      or (if arrDr(1) < 0) a correction scaled by "arrhFac" weight.
C     The global mean imbalance is computed as area (arrArea), mask
C      (arrMask) and thickness (arrDr) weighted, as well as thickness
C      factor (arrhFac) weighted (1rst case) or not (2nd case).
C     Comment: currently this S/R is only applied to 2-D field.
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myNr      :: third dimension of field to correct
C     arrFld    :: field array to correct for imbalance
C     arrhFac   :: same size as arrFld ; thickness factor (1rst case)
C               :: or correction scaling factor (2nd case)
C     arrMask   :: 2-D mask for global mean calculation
C     arrArea   :: 2-D grid-cell area for global mean calculation
C     arrDr     :: myNr long vector (level thickness)
C     arrName   :: name of field to correct
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number in simulation
C     myThid    :: my Thread ID number
      INTEGER myNr
      _RL arrFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrhFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS arrArea(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS arrDr(myNr)
      CHARACTER*(*) arrName
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
      INTEGER bi, bj, i, j, k
      _RL volTile(nSx,nSy), sumTile(nSx,nSy)
      _RL tmpVol, volGlob, sumGlob, theMean
      _RL meanCorr
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        volTile(bi,bj) = 0. _d 0
        sumTile(bi,bj) = 0. _d 0
        IF ( arrDr(1).GE.zeroRS ) THEN
C--   do (arrhFac) weighted average to apply uniform correction
         DO k=1,myNr
          DO j=1,sNy
           DO i=1,sNx
            tmpVol = arrArea(i,j,bi,bj)*arrMask(i,j,bi,bj)
     &             * arrhFac(i,j,k,bi,bj)*arrDr(k)
            volTile(bi,bj) = volTile(bi,bj) + tmpVol
            sumTile(bi,bj) = sumTile(bi,bj) + tmpVol*arrFld(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ELSE
C-    do simple average to apply (arrhFac) weighted correction
         DO k=1,myNr
          DO j=1,sNy
           DO i=1,sNx
            tmpVol = arrArea(i,j,bi,bj)*arrMask(i,j,bi,bj)
     &             * ABS(arrDr(k))
            volTile(bi,bj) = volTile(bi,bj)
     &                     + tmpVol*arrhFac(i,j,k,bi,bj)
            sumTile(bi,bj) = sumTile(bi,bj)
     &                     + tmpVol*arrFld(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDDO

      CALL GLOBAL_SUM_TILE_RL( volTile, volGlob, myThid )
      CALL GLOBAL_SUM_TILE_RL( sumTile, sumGlob, myThid )

      IF ( volGlob.GT.zeroRL ) THEN
       meanCorr = sumGlob/volGlob
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         IF ( arrDr(1).GE.zeroRS ) THEN
C--   apply uniform correction
          DO k=1,myNr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             IF ( arrhFac(i,j,k,bi,bj).NE.zeroRS ) THEN
              arrFld(i,j,k,bi,bj) = arrFld(i,j,k,bi,bj) - meanCorr
             ENDIF
            ENDDO
           ENDDO
          ENDDO
         ELSE
C-    apply (arrhFac) weighted correction
          DO k=1,myNr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             IF ( arrMask(i,j,bi,bj).NE.zeroRS ) THEN
              arrFld(i,j,k,bi,bj) = arrFld(i,j,k,bi,bj)
     &                            - meanCorr*arrhFac(i,j,k,bi,bj)
             ENDIF
            ENDDO
           ENDDO
          ENDDO
C-    end type correction selection
         ENDIF
        ENDDO
       ENDDO
      ELSE
       meanCorr = 0. _d 0
      ENDIF

C     Print the global mean to standard output, this is a measure
C     of the magnitude of the correction to array arrFld
      IF ( balancePrintMean ) THEN
       _BEGIN_MASTER( myThid )
       IF ( arrDr(1).GE.zeroRS ) THEN
        theMean = meanCorr
        WRITE(msgBuf,'(3A,1PE21.14,A,I10)')
     &        'REMOVE_MEAN_RL: Global mean of ', arrName, ' = ',
     &        theMean, '  @ it=', myIter
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ELSE
        theMean = 0. _d 0
        IF ( globalArea.GT.zeroRL ) theMean = sumGlob/globalArea
        WRITE(msgBuf,'(3A,2(1PE21.14,A),I10)')
     &        'REMOVE_MEAN_RL: ', arrName, ' Global mean= ',
     &        theMean, ', remove: ', meanCorr, ' @ it=', myIter
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
       _END_MASTER( myThid )
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: REMOVE_MEAN_RS
C     !INTERFACE:
      SUBROUTINE REMOVE_MEAN_RS(
     I                myNr,
     U                arrFld,
     I                arrhFac, arrMask, arrArea, arrDr,
     I                arrName, myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     Correct for global-mean imbalance of global "_RS" array "arrFld"
C      (i.e., output arrFld has zero mean):
C     Apply either (if arrDr(1) > 0) uniform correction
C      or (if arrDr(1) < 0) a correction scaled by "arrhFac" weight.
C     The global mean imbalance is computed as area (arrArea), mask
C      (arrMask) and thickness (arrDr) weighted, as well as thickness
C      factor (arrhFac) weighted (1rst case) or not (2nd case).
C     Comment: currently this S/R is only applied to 2-D field.
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myNr      :: third dimension of field to correct
C     arrFld    :: field array to correct for imbalance
C     arrhFac   :: same size as arrFld ; thickness factor (1rst case)
C               :: or correction scaling factor (2nd case)
C     arrMask   :: 2-D mask for global mean calculation
C     arrArea   :: 2-D grid-cell area for global mean calculation
C     arrDr     :: myNr long vector (level thickness)
C     arrName   :: name of field to correct
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number in simulation
C     myThid    :: my Thread ID number
      INTEGER myNr
      _RS arrFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrhFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS arrArea(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS arrDr(myNr)
      CHARACTER*(*) arrName
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
      INTEGER bi, bj, i, j, k
      _RL volTile(nSx,nSy), sumTile(nSx,nSy)
      _RL tmpVol, volGlob, sumGlob, theMean
      _RS meanCorr
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        volTile(bi,bj) = 0. _d 0
        sumTile(bi,bj) = 0. _d 0
        IF ( arrDr(1).GE.zeroRS ) THEN
C--   do (arrhFac) weighted average to apply uniform correction
         DO k=1,myNr
          DO j=1,sNy
           DO i=1,sNx
            tmpVol = arrArea(i,j,bi,bj)*arrMask(i,j,bi,bj)
     &             * arrhFac(i,j,k,bi,bj)*arrDr(k)
            volTile(bi,bj) = volTile(bi,bj) + tmpVol
            sumTile(bi,bj) = sumTile(bi,bj) + tmpVol*arrFld(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ELSE
C-    do simple average to apply (arrhFac) weighted correction
         DO k=1,myNr
          DO j=1,sNy
           DO i=1,sNx
            tmpVol = arrArea(i,j,bi,bj)*arrMask(i,j,bi,bj)
     &             * ABS(arrDr(k))
            volTile(bi,bj) = volTile(bi,bj)
     &                     + tmpVol*arrhFac(i,j,k,bi,bj)
            sumTile(bi,bj) = sumTile(bi,bj)
     &                     + tmpVol*arrFld(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDDO

      CALL GLOBAL_SUM_TILE_RL( volTile, volGlob, myThid )
      CALL GLOBAL_SUM_TILE_RL( sumTile, sumGlob, myThid )

      IF ( volGlob.GT.zeroRL ) THEN
       meanCorr = sumGlob/volGlob
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         IF ( arrDr(1).GE.zeroRS ) THEN
C--   apply uniform correction
          DO k=1,myNr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             IF ( arrhFac(i,j,k,bi,bj).NE.zeroRS ) THEN
              arrFld(i,j,k,bi,bj) = arrFld(i,j,k,bi,bj) - meanCorr
             ENDIF
            ENDDO
           ENDDO
          ENDDO
         ELSE
C-    apply (arrhFac) weighted correction
          DO k=1,myNr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             IF ( arrMask(i,j,bi,bj).NE.zeroRS ) THEN
              arrFld(i,j,k,bi,bj) = arrFld(i,j,k,bi,bj)
     &                            - meanCorr*arrhFac(i,j,k,bi,bj)
             ENDIF
            ENDDO
           ENDDO
          ENDDO
C-    end type correction selection
         ENDIF
        ENDDO
       ENDDO
      ELSE
       meanCorr = 0. _d 0
      ENDIF

C     Print the global mean to standard output, this is a measure
C     of the magnitude of the correction to array arrFld
      IF ( balancePrintMean ) THEN
       _BEGIN_MASTER( myThid )
       IF ( arrDr(1).GE.zeroRS ) THEN
        theMean = meanCorr
        WRITE(msgBuf,'(3A,1PE21.14,A,I10)')
     &        'REMOVE_MEAN_RS: Global mean of ', arrName, ' = ',
     &        theMean, '  @ it=', myIter
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ELSE
        theMean = 0. _d 0
        IF ( globalArea.GT.zeroRL ) theMean = sumGlob/globalArea
        WRITE(msgBuf,'(3A,2(1PE21.14,A),I10)')
     &        'REMOVE_MEAN_RS: ', arrName, ' Global mean= ',
     &        theMean, ', remove: ', meanCorr, ' @ it=', myIter
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
       _END_MASTER( myThid )
      ENDIF

      RETURN
      END
