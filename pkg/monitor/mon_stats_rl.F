C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_stats_rl.F,v 1.11 2005/01/27 16:36:24 jmc Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MON_STATS_RL

C     !INTERFACE:
      SUBROUTINE MON_STATS_RL(
     I     myNr, arr, arrMask,arrhFac, arrArea, arrDr,
     O     theMin,theMax,theMean,theSD,theDel2,theVol,
     I     myThid )

C     Calculate bare statistics of global array ``\_RL arr''.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
      INTEGER myNr
      _RL arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrhFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrArea(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS arrDr(myNr)
      _RL theMin, theMax, theMean, theSD, theDel2, theVol
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER bi,bj,I,J,K
      INTEGER numPnts
      LOGICAL noPnts
      _RL tmpVal,rNumPnts
      _RL theVar,theVarTile
      _RL tmpVol
      _RL theMeanTile, theSDTile, theDel2Tile, theVolTile

      theMin=0.
      theMax=0.
      theMean=0.
      theSD=0.
      theVar=0.
      theDel2=0.
      theVol=0.
      numPnts=0
      noPnts=.TRUE.

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        theDel2Tile = 0.
        theVolTile = 0.
        theMeanTile = 0.
        theVarTile = 0.
        DO K=1,myNr
         DO J=1,sNy
          DO I=1,sNx
           tmpVal=arr(I,J,K,bi,bj)
           IF (arrMask(I,J,K,bi,bj).NE.0. .AND. noPnts) THEN
            theMin=tmpVal
            theMax=tmpVal
            noPnts=.FALSE.
           ENDIF
           IF (arrMask(I,J,K,bi,bj).NE.0.) THEN
            theMin=min(theMin,tmpVal)
            theMax=max(theMax,tmpVal)
            theDel2Tile = theDel2Tile+0.25*ABS(
     &         (arr(I+1,J,K,bi,bj)-tmpVal)*arrMask(I+1,J,K,bi,bj)
     &        +(arr(I-1,J,K,bi,bj)-tmpVal)*arrMask(I-1,J,K,bi,bj)
     &        +(arr(I,J+1,K,bi,bj)-tmpVal)*arrMask(I,J+1,K,bi,bj)
     &        +(arr(I,J-1,K,bi,bj)-tmpVal)*arrMask(I,J-1,K,bi,bj)
     &                               )
            numPnts=numPnts+1
            tmpVol = arrArea(I,J,bi,bj)*arrhFac(I,J,K,bi,bj)*arrDr(K)
     &                                 *arrMask(I,J,K,bi,bj)
            theVolTile = theVolTile   + tmpVol
            theMeanTile = theMeanTile + tmpVol*tmpVal
            theVarTile = theVarTile   + tmpVol*tmpVal*tmpVal
           ENDIF
          ENDDO
         ENDDO
        ENDDO
        theDel2 = theDel2 + theDel2Tile
        theVol = theVol + theVolTile
        theMean = theMean + theMeanTile
        theVar = theVar + theVarTile
       ENDDO
      ENDDO

      _GLOBAL_SUM_R8(theDel2,myThid)
      _GLOBAL_SUM_R8(theVol,myThid)
      _GLOBAL_SUM_R8(theMean,myThid)
      _GLOBAL_SUM_R8(theVar,myThid)
      tmpVal=FLOAT(numPnts)
      _GLOBAL_SUM_R8(tmpVal,myThid)
      numPnts=NINT(tmpVal)

      IF (tmpVal.GT.0.) THEN
       rNumPnts=1. _d 0/tmpVal
       theDel2=theDel2*rNumPnts
      ENDIF

      IF (theVol.GT.0.) THEN
       theMean=theMean/theVol
       theVar=theVar/theVol
       IF ( noPnts ) theMin = theMean
       theMin=-theMin
       _GLOBAL_MAX_R8(theMin,myThid)
       theMin=-theMin
       IF ( noPnts ) theMax = theMean
       _GLOBAL_MAX_R8(theMax,myThid)

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         theSDtile=0.
         DO K=1,myNr
          DO J=1,sNy
           DO I=1,sNx
            tmpVal=arr(I,J,K,bi,bj)
c           IF (tmpVal.NE.0.) THEN
            IF (arrMask(I,J,K,bi,bj).NE.0.) THEN
             tmpVol=arrArea(I,J,bi,bj)*arrhFac(I,J,K,bi,bj)*arrDr(K)
     &                                *arrMask(I,J,K,bi,bj)
             theSDtile = theSDtile + tmpVol*(tmpVal-theMean)**2
            ENDIF
           ENDDO
          ENDDO
         ENDDO
         theSD = theSD + theSDtile
        ENDDO
       ENDDO

       _GLOBAL_SUM_R8(theSD,myThid)

       theSD=sqrt(theSD/theVol)
c      theSD=sqrt(theVar-theMean**2)
      ENDIF

      RETURN
      END
