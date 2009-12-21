C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_calc_stats_rl.F,v 1.1 2009/12/21 00:03:40 jmc Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MON_CALC_STATS_RL

C     !INTERFACE:
      SUBROUTINE MON_CALC_STATS_RL(
     I               myNr, arr, arrhFac, arrMask, arrArea, arrDr,
     O               theMin, theMax, theMean, theSD, theDel2, theVol,
     I               myThid )

C     Calculate statistics of global array ``\_RL arr''.
C     account for volume and mask

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER myNr
      _RL arr    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrhFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS arrArea(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS arrDr(myNr)
      _RL theMin, theMax, theMean, theSD, theDel2, theVol
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER bi,bj,i,j,k
      INTEGER numPnts
      LOGICAL noPnts
      _RL tmpVal
      _RL tmpMask
      _RL tmpVol
      _RL ddx, ddy
      _RL theVar
      _RL tileMean(nSx,nSy)
      _RL tileVar (nSx,nSy)
      _RL tileSD  (nSx,nSy)
      _RL tileDel2(nSx,nSy)
      _RL tileVol (nSx,nSy)

      theMin = 0.
      theMax = 0.
      theMean= 0.
      theSD  = 0.
      theVar = 0.
      theDel2= 0.
      theVol = 0.
      numPnts= 0
      noPnts = .TRUE.

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        tileDel2(bi,bj) = 0.
        tileVol (bi,bj) = 0.
        tileMean(bi,bj) = 0.
        tileVar (bi,bj) = 0.
        DO k=1,myNr
         DO j=1,sNy
          DO i=1,sNx
           tmpVal  = arr(i,j,k,bi,bj)
           tmpMask = arrMask(i,j,bi,bj)*arrhFac(i,j,k,bi,bj)
           IF ( tmpMask.GT.0. _d 0 .AND. noPnts ) THEN
            theMin=tmpVal
            theMax=tmpVal
            noPnts=.FALSE.
           ENDIF
           IF ( tmpMask.GT.0. _d 0 ) THEN
            theMin = MIN(theMin,tmpVal)
            theMax = MAX(theMax,tmpVal)
C--   like old code (but using hFac instead of mask): identical if no partial cell
c           tileDel2(bi,bj) = tileDel2(bi,bj)
c    &       + 0.25*ABS(
c    &          (arr(i+1,j,k,bi,bj)-tmpVal)*arrhFac(i+1,j,k,bi,bj)
c    &         +(arr(i-1,j,k,bi,bj)-tmpVal)*arrhFac(i-1,j,k,bi,bj)
c    &         +(arr(i,j+1,k,bi,bj)-tmpVal)*arrhFac(i,j+1,k,bi,bj)
c    &         +(arr(i,j-1,k,bi,bj)-tmpVal)*arrhFac(i,j-1,k,bi,bj)
c    &                 )
C--   New form:
            ddx = arrhFac(i+1,j,k,bi,bj)*arrhFac(i-1,j,k,bi,bj)
            IF ( ddx.GT.0. _d 0 ) THEN
             ddx = (arr(i+1,j,k,bi,bj)-tmpVal)
     &           + (arr(i-1,j,k,bi,bj)-tmpVal)
            ENDIF
            ddy = arrhFac(i,j+1,k,bi,bj)*arrhFac(i,j-1,k,bi,bj)
            IF ( ddy.GT.0. _d 0 ) THEN
             ddy = (arr(i,j+1,k,bi,bj)-tmpVal)
     &           + (arr(i,j-1,k,bi,bj)-tmpVal)
            ENDIF
            tileDel2(bi,bj) = tileDel2(bi,bj) + ddx*ddx + ddy*ddy

            numPnts=numPnts+1
            tmpVol = arrArea(i,j,bi,bj)*arrDr(k)*tmpMask
            tileVol (bi,bj) = tileVol (bi,bj) + tmpVol
            tileMean(bi,bj) = tileMean(bi,bj) + tmpVol*tmpVal
            tileVar (bi,bj) = tileVar (bi,bj) + tmpVol*tmpVal*tmpVal
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL GLOBAL_SUM_TILE_RL( tileDel2, theDel2, myThid )
      CALL GLOBAL_SUM_TILE_RL( tileVol , theVol , myThid )
      CALL GLOBAL_SUM_TILE_RL( tileMean, theMean, myThid )
c     CALL GLOBAL_SUM_TILE_RL( tileVar , theVar , myThid )

      CALL GLOBAL_SUM_INT( numPnts, myThid )

      IF ( numPnts.GT.0 ) THEN
       tmpVal   = FLOAT(numPnts)
c      theDel2  = theDel2/tmpVal
       theDel2  = SQRT(theDel2)/tmpVal
      ENDIF

      IF ( theVol.GT.0. _d 0 ) THEN
       theMean= theMean/theVol
       theVar = theVar/theVol
       IF ( noPnts ) theMin = theMean
       theMin = -theMin
       _GLOBAL_MAX_RL(theMin,myThid)
       theMin = -theMin
       IF ( noPnts ) theMax = theMean
       _GLOBAL_MAX_RL(theMax,myThid)

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         tileSD(bi,bj)=0.
         DO k=1,myNr
          DO j=1,sNy
           DO i=1,sNx
            tmpVal=arr(i,j,k,bi,bj)
            tmpMask = arrMask(i,j,bi,bj)*arrhFac(i,j,k,bi,bj)
            IF ( tmpMask.GT.0. _d 0 ) THEN
             tmpVol = arrArea(i,j,bi,bj)*arrDr(k)*tmpMask
             tileSD(bi,bj) = tileSD(bi,bj)
     &                     + tmpVol*(tmpVal-theMean)*(tmpVal-theMean)
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       CALL GLOBAL_SUM_TILE_RL( tileSD, theSD, myThid )

       theSD = SQRT(theSD/theVol)
c      theSD = SQRT(theVar-theMean*theMean)
      ENDIF

      RETURN
      END
