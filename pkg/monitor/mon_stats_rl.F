C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_stats_rl.F,v 1.3 2001/11/08 16:47:34 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MON_STATS_RL(
     I                myNr, arr, arrMask,arrhFac, arrArea, arrDr,
     O                theMin,theMax,theMean,theSD,theDel2,
     I                myThid )
C     /==========================================================\
C     | SUBROUTINE MON_STATS_RL                                  |
C     | o Calculate bare statistics of global array "_RL arr"    |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     === Routine arguments ===
      INTEGER myNr
      _RL arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrhFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RS arrArea(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS arrDr(myNr)
      _RL theMin
      _RL theMax
      _RL theMean
      _RL theSD
      _RL theDel2
      INTEGER myThid

C     === Local variables ====
      INTEGER bi,bj,I,J,K
      INTEGER numPnts
      LOGICAL noPnts
      _RL tmpVal,rNumPnts
      _RL theVar
      _RL theVol, tmpVol

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
        DO K=1,myNr
         DO J=1,sNy
          DO I=1,sNx
           tmpVal=arr(I,J,K,bi,bj)
c          IF (tmpVal.NE.0. .AND. noPnts) THEN
           IF (arrMask(I,J,K,bi,bj).NE.0. .AND. noPnts) THEN
            theMin=tmpVal
            theMax=tmpVal
            noPnts=.FALSE.
           ENDIF
c          IF (tmpVal.NE.0.) THEN
           IF (arrMask(I,J,K,bi,bj).NE.0.) THEN
            theMin=min(theMin,tmpVal)
            theMax=max(theMax,tmpVal)
            theDel2 = theDel2+0.25*ABS(
     &         (arr(I+1,J,K,bi,bj)-tmpVal)*arrMask(I+1,J,K,bi,bj)
     &        +(arr(I-1,J,K,bi,bj)-tmpVal)*arrMask(I-1,J,K,bi,bj)
     &        +(arr(I,J+1,K,bi,bj)-tmpVal)*arrMask(I,J+1,K,bi,bj)
     &        +(arr(I,J-1,K,bi,bj)-tmpVal)*arrMask(I,J-1,K,bi,bj)
     &                               )
            numPnts=numPnts+1
            tmpVol = arrArea(I,J,bi,bj)*arrhFac(I,J,K,bi,bj)*arrDr(K)
            theVol = theVol   + tmpVol
            theMean = theMean + tmpVol*tmpVal
            theVar = theVar   + tmpVol*tmpVal**2
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      theMin=-theMin
      _GLOBAL_MAX_R8(theMin,myThid)
      theMin=-theMin
      _GLOBAL_MAX_R8(theMax,myThid)
      _GLOBAL_SUM_R8(theDel2,myThid)
      _GLOBAL_SUM_R8(theVol,myThid)
      _GLOBAL_SUM_R8(theMean,myThid)
      _GLOBAL_SUM_R8(theVar,myThid)
      tmpVal=FLOAT(numPnts)
      _GLOBAL_SUM_R8(tmpVal,myThid)
      numPnts=INT(tmpVal+0.5)

      IF (tmpVal.GT.0.) THEN
       rNumPnts=1./tmpVal
       theDel2=theDel2*rNumPnts
      ENDIF

      IF (theVol.GT.0.) THEN
       theMean=theMean/theVol
       theVar=theVar/theVol

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO K=1,myNr
          DO J=1,sNy
           DO I=1,sNx
            tmpVal=arr(I,J,K,bi,bj)
c           IF (tmpVal.NE.0.) THEN
            IF (arrMask(I,J,K,bi,bj).NE.0.) THEN
             tmpVol=arrArea(I,J,bi,bj)*arrhFac(I,J,K,bi,bj)*arrDr(K)
             theSD = theSD + tmpVol*(tmpVal-theMean)**2
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       _GLOBAL_SUM_R8(theSD,myThid)

       theSD=sqrt(theSD/theVol)
c      theSD=sqrt(theVar-theMean**2)
      ENDIF

      RETURN
      END
