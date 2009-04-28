C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_stats_rs.F,v 1.9 2009/04/28 18:16:53 jmc Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MON_STATS_RS

C     !INTERFACE:
      SUBROUTINE MON_STATS_RS(
     I     myNr, arr,
     O     theMin,theMax,theMean,theSD,
     I     myThid )

C     !DESCRIPTION:
C     Calculate bare statistics of global array ``\_RS arr''.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
      INTEGER myNr
      _RS arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNr,nSx,nSy)
      _RL theMin, theMax, theMean, theSD
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER bi,bj,I,J,K
      INTEGER numPnts
      LOGICAL noPnts
      _RL tmpVal,rNumPnts
      _RL theVar
      _RL tileMean(nSx,nSy)
      _RL tileVar (nSx,nSy)
      _RL tileSD  (nSx,nSy)

      theMin = 0.
      theMax = 0.
      theMean= 0.
      theSD  = 0.
      theVar = 0.
      numPnts= 0
      noPnts = .TRUE.

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        tileMean(bi,bj) = 0.
        tileVar (bi,bj) = 0.
        DO K=1,myNr
         DO J=1,sNy
          DO I=1,sNx
           tmpVal=arr(I,J,K,bi,bj)
c          IF (tmpVal.NE.0. .AND. noPnts) THEN
           IF ( noPnts ) THEN
            theMin = tmpVal
            theMax = tmpVal
            noPnts = .FALSE.
           ENDIF
c          IF (tmpVal.NE.0.) THEN
            theMin = MIN(theMin,tmpVal)
            theMax = MAX(theMax,tmpVal)
            tileMean(bi,bj)=tileMean(bi,bj)+tmpVal
            tileVar (bi,bj)=tileVar (bi,bj)+tmpVal*tmpVal
            numPnts=numPnts+1
c          ENDIF
          ENDDO
         ENDDO
        ENDDO
c       theMean=theMean+tileMean(bi,bj)
c       theVar =theVar +tileVar (bi,bj)
       ENDDO
      ENDDO

c     _GLOBAL_SUM_RL(theMean,myThid)
c     _GLOBAL_SUM_RL(theVar,myThid)
      CALL GLOBAL_SUM_TILE_RL( tileMean, theMean, myThid )
      CALL GLOBAL_SUM_TILE_RL( tileVar , theVar , myThid )
      tmpVal=FLOAT(numPnts)
      _GLOBAL_SUM_RL(tmpVal,myThid)
      numPnts=NINT(tmpVal)

      IF (tmpVal.GT.0.) THEN
       rNumPnts=1. _d 0/tmpVal
       theMean=theMean*rNumPnts
       theVar=theVar*rNumPnts
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
c           IF (tmpVal.NE.0.) THEN
             tileSD(bi,bj) = tileSD(bi,bj)
     &                     + (tmpVal-theMean)*(tmpVal-theMean)
c           ENDIF
           ENDDO
          ENDDO
         ENDDO
c        theSD = theSD + tileSD(bi,bj)
        ENDDO
       ENDDO

c      _GLOBAL_SUM_RL(theSD,myThid)
       CALL GLOBAL_SUM_TILE_RL( tileSD, theSD, myThid )

       theSD = SQRT(theSD*rNumPnts)
c      theSD = SQRT(theVar-theMean*theMean)
      ENDIF

      RETURN
      END
