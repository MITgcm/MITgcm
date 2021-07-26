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
C     *==========================================================*
C     | SUBROUTINE REMOVE_MEAN_RL                                |
C     | o Calculate mean of global "_RL" array "arrFld" and      |
C     |   substract it from the same array                       |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
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
      _RL tmpVol, volGlob, sumGlob
      _RL theMean
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        volTile(bi,bj) = 0. _d 0
        sumTile(bi,bj) = 0. _d 0
        DO k=1,myNr
         DO j=1,sNy
          DO i=1,sNx
           tmpVol = arrArea(i,j,bi,bj)*arrMask(i,j,bi,bj)
     &            * arrhFac(i,j,k,bi,bj)*arrDr(k)
           volTile(bi,bj) = volTile(bi,bj) + tmpVol
           sumTile(bi,bj) = sumTile(bi,bj) + tmpVol*arrFld(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL GLOBAL_SUM_TILE_RL( volTile, volGlob, myThid )
      CALL GLOBAL_SUM_TILE_RL( sumTile, sumGlob, myThid )

      IF ( volGlob.GT.zeroRL ) THEN
       theMean = sumGlob/volGlob
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO k=1,myNr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            IF ( arrhFac(i,j,k,bi,bj).NE.zeroRS ) THEN
             arrFld(i,j,k,bi,bj) = arrFld(i,j,k,bi,bj) - theMean
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       theMean = 0. _d 0
      ENDIF

C     Print the global mean to standard output, this is a measure
C     of the magnitude of the correction to array arrFld
      IF ( balancePrintMean ) THEN
       _BEGIN_MASTER( myThid )
       WRITE(msgBuf,'(3A,1PE21.14,A,I10)')
     &      'REMOVE_MEAN_RL: Global mean of ',
     &      arrName, ' = ', theMean, '  @ it=', myIter
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
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
C     *==========================================================*
C     | SUBROUTINE REMOVE_MEAN_RS                                |
C     | o Calculate mean of global "_RS" array "arrFld" and      |
C     |   substract it from the same array                       |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER myNr
      _RS arrFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
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
      _RL tmpVol, volGlob, sumGlob
      _RS theMean
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        volTile(bi,bj) = 0. _d 0
        sumTile(bi,bj) = 0. _d 0
        DO k=1,myNr
         DO j=1,sNy
          DO i=1,sNx
           tmpVol = arrArea(i,j,bi,bj)*arrMask(i,j,bi,bj)
     &            * arrhFac(i,j,k,bi,bj)*arrDr(k)
           volTile(bi,bj) = volTile(bi,bj) + tmpVol
           sumTile(bi,bj) = sumTile(bi,bj) + tmpVol*arrFld(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL GLOBAL_SUM_TILE_RL( volTile, volGlob, myThid )
      CALL GLOBAL_SUM_TILE_RL( sumTile, sumGlob, myThid )

      IF ( volGlob.GT.zeroRL ) THEN
       theMean = sumGlob/volGlob
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO k=1,myNr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            IF ( arrhFac(i,j,k,bi,bj).NE.zeroRS ) THEN
             arrFld(i,j,k,bi,bj) = arrFld(i,j,k,bi,bj) - theMean
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       theMean = 0. _d 0
      ENDIF

C     Print the global mean to standard output, this is a measure
C     of the magnitude of the correction to array arrFld
      IF ( balancePrintMean ) THEN
       _BEGIN_MASTER( myThid )
       WRITE(msgBuf,'(3A,1PE21.14,A,I10)')
     &      'REMOVE_MEAN_RS: Global mean of ',
     &      arrName, ' = ', theMean, '  @ it=', myIter
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       _END_MASTER( myThid )
      ENDIF

      RETURN
      END
