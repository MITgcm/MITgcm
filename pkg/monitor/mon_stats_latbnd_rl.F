C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_stats_latbnd_rl.F,v 1.1 2003/11/10 22:58:53 jmc Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"

      SUBROUTINE MON_STATS_LATBND_RL(
     I                myNr, nSepBnd, ySepBnd,
     I                arr, arrMask, arrhFac, arrArea, arrY, arrDr,
     O                theMin,theMax,theMean,theVar,theVol,
     I                myThid )
C     *==========================================================*
C     | SUBROUTINE MON_STATS_LATBND_RL
C     | o Calculate bare statistics of global array "_RL arr"
C     |   on each Latitude band (given by ySepBnd)
C     *==========================================================*
C     *==========================================================*
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     === Routine arguments ===
C     nSepBnd :: Number of latitude bands
C     ySepBnd :: latitude band egdes (from 1 to nSepBnd-1)
      INTEGER myNr
      INTEGER nSepBnd
      _RS ySepBnd(nSepBnd)
      _RL arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrhFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrArea(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS arrY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS arrDr(myNr)
      _RL theMin(nSepBnd)
      _RL theMax(nSepBnd)
      _RL theMean(nSepBnd)
      _RL theVar(nSepBnd)
      _RL theVol(nSepBnd)
      INTEGER myThid

C     === Functions ====
      INTEGER  NLATBND
      EXTERNAL NLATBND

C     === Local variables ====
      INTEGER bi,bj,i,j,k,n
      INTEGER numPnts
      LOGICAL noPnts(Ny)
      _RL tmpVal,rNumPnts
      _RL tmpVol

      DO n=1,nSepBnd
       noPnts(n)=.TRUE.
       theMin(n)=0.
       theMax(n)=0.
       theMean(n)=0.
       theVar(n)=0.
       theVol(n)=0.
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k=1,myNr
         DO j=1,sNy
          DO i=1,sNx
           n = NLATBND(nSepBnd, ySepBnd, arrY(i,j,bi,bj) )
           tmpVal=arr(i,j,k,bi,bj)
           IF (arrMask(i,j,k,bi,bj).NE.0. .AND. noPnts(n)) THEN
            theMin(n)=tmpVal
            theMax(n)=tmpVal
            noPnts(n)=.FALSE.
           ENDIF
           IF (arrMask(i,j,k,bi,bj).NE.0.) THEN
            theMin(n)=min(theMin(n),tmpVal)
            theMax(n)=max(theMax(n),tmpVal)
            tmpVol = arrArea(i,j,bi,bj)*arrhFac(i,j,k,bi,bj)*arrDr(k)
     &                                 *arrMask(i,j,k,bi,bj)
            theVol(n) = theVol(n) + tmpVol
            theMean(n)=theMean(n) + tmpVol*tmpVal
            theVar(n) = theVar(n) + tmpVol*tmpVal*tmpVal
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO n=1,nSepBnd
       theMin(n)=-theMin(n)
       _GLOBAL_MAX_R8(theMin(n), myThid)
       theMin(n)=-theMin(n)
       _GLOBAL_MAX_R8(theMax(n), myThid)
       _GLOBAL_SUM_R8(theVol(n), myThid)
       _GLOBAL_SUM_R8(theMean(n),myThid)
       _GLOBAL_SUM_R8(theVar(n), myThid)
      ENDDO

      DO n=1,nSepBnd
       IF (theVol(n).GT.0.) THEN
        theMean(n)= theMean(n)/theVol(n)
        theVar(n) = theVar(n) /theVol(n)
       ENDIF
      ENDDO

      RETURN
      END
      
CBOP
C     !ROUTINE: NLATBND

C     !INTERFACE:
      INTEGER FUNCTION NLATBND( nBnd, yBnd, yLoc )
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================*
C     | FUNCTION NLATBND
C     | o Find the latidude band of yLoc in nSep strip
C     *==========================================================*
C
C     !INPUT PARAMETERS:
C     nBnd :: Nb of latitude band
C     yBnd :: latitude of band boundaries
C     yLoc :: current latitude
      INTEGER nBnd
      _RS yBnd(nBnd) 
      _RS yLoc

C     !LOCAL VARIABLES:
      INTEGER n

      NLATBND = nBnd
      DO n=1,nBnd-1
       IF (yLoc .LE. yBnd(n)) NLATBND = n
      ENDDO

      RETURN
      END
