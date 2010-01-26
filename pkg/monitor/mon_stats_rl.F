C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_stats_rl.F,v 1.15 2010/01/26 01:09:02 jmc Exp $
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
      _RL theVar
      _RL tmpVol
      _RL tileMean(nSx,nSy)
      _RL tileVar (nSx,nSy)
      _RL tileSD  (nSx,nSy)
      _RL tileDel2(nSx,nSy)
      _RL tileVol (nSx,nSy)

C     Since 2009/12/21 MON_CALC_STATS_RL replaces MON_STATS_RL
C     which is now disabled
      STOP 'ABNORMAL END: S/R MON_STATS_RL no longer maintained'

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
        tileDel2(bi,bj) = 0.
        tileVol (bi,bj) = 0.
        tileMean(bi,bj) = 0.
        tileVar (bi,bj) = 0.
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
            tileDel2(bi,bj) = tileDel2(bi,bj)
     &      + 0.25*ABS(
     &         (arr(I+1,J,K,bi,bj)-tmpVal)*arrMask(I+1,J,K,bi,bj)
     &        +(arr(I-1,J,K,bi,bj)-tmpVal)*arrMask(I-1,J,K,bi,bj)
     &        +(arr(I,J+1,K,bi,bj)-tmpVal)*arrMask(I,J+1,K,bi,bj)
     &        +(arr(I,J-1,K,bi,bj)-tmpVal)*arrMask(I,J-1,K,bi,bj)
     &                )
            numPnts=numPnts+1
            tmpVol = arrArea(I,J,bi,bj)*arrhFac(I,J,K,bi,bj)*arrDr(K)
     &                                 *arrMask(I,J,K,bi,bj)
            tileVol (bi,bj) = tileVol (bi,bj) + tmpVol
            tileMean(bi,bj) = tileMean(bi,bj) + tmpVol*tmpVal
            tileVar (bi,bj) = tileVar (bi,bj) + tmpVol*tmpVal*tmpVal
           ENDIF
          ENDDO
         ENDDO
        ENDDO
c       theDel2 = theDel2 + tileDel2(bi,bj)
c       theVol  = theVol  + tileVol(bi,bj)
c       theMean = theMean + tileMean(bi,bj)
c       theVar  = theVar  + tileVar (bi,bj)
       ENDDO
      ENDDO

c     _GLOBAL_SUM_RL(theDel2,myThid)
c     _GLOBAL_SUM_RL(theVol,myThid)
c     _GLOBAL_SUM_RL(theMean,myThid)
c     _GLOBAL_SUM_RL(theVar,myThid)
      CALL GLOBAL_SUM_TILE_RL( tileDel2, theDel2, myThid )
      CALL GLOBAL_SUM_TILE_RL( tileVol , theVol , myThid )
      CALL GLOBAL_SUM_TILE_RL( tileMean, theMean, myThid )
      CALL GLOBAL_SUM_TILE_RL( tileVar , theVar , myThid )
      tmpVal=FLOAT(numPnts)
      _GLOBAL_SUM_RL(tmpVal,myThid)
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
       _GLOBAL_MAX_RL(theMin,myThid)
       theMin=-theMin
       IF ( noPnts ) theMax = theMean
       _GLOBAL_MAX_RL(theMax,myThid)

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         tileSD(bi,bj)=0.
         DO K=1,myNr
          DO J=1,sNy
           DO I=1,sNx
            tmpVal=arr(I,J,K,bi,bj)
            IF (arrMask(I,J,K,bi,bj).NE.0.) THEN
             tmpVol=arrArea(I,J,bi,bj)*arrhFac(I,J,K,bi,bj)*arrDr(K)
     &                                *arrMask(I,J,K,bi,bj)
             tileSD(bi,bj) = tileSD(bi,bj)
     &                     + tmpVol*(tmpVal-theMean)*(tmpVal-theMean)
            ENDIF
           ENDDO
          ENDDO
         ENDDO
c        theSD = theSD + tileSD(bi,bj)
        ENDDO
       ENDDO

c      _GLOBAL_SUM_RL(theSD,myThid)
       CALL GLOBAL_SUM_TILE_RL( tileSD, theSD, myThid )

       theSD = SQRT(theSD/theVol)
c      theSD = SQRT(theVar-theMean*theMean)
      ENDIF

      RETURN
      END
