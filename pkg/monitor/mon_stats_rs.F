C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_stats_rs.F,v 1.7 2005/01/27 16:36:24 jmc Exp $
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
      _RL theVar,theVarTile
      _RL theMeanTile,theSDTile

      theMin=0.
      theMax=0.
      theMean=0.
      theSD=0.
      theVar=0.
      numPnts=0
      noPnts=.TRUE.

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        theVarTile=0.
        theMeanTile=0.
        DO K=1,myNr
         DO J=1,sNy
          DO I=1,sNx
           tmpVal=arr(I,J,K,bi,bj)
           IF (tmpVal.NE.0. .AND. noPnts) THEN
            theMin=tmpVal
            theMax=tmpVal
            noPnts=.FALSE.
           ENDIF
           IF (tmpVal.NE.0.) THEN
            theMin=min(theMin,tmpVal)
            theMax=max(theMax,tmpVal)
            theMeanTile=theMeanTile+tmpVal
            theVarTile=theVarTile+tmpVal*tmpVal
            numPnts=numPnts+1
           ENDIF
          ENDDO
         ENDDO
        ENDDO
        theMean=theMean+theMeanTile
        theVar=theVar+theVarTile
       ENDDO
      ENDDO

      _GLOBAL_SUM_R8(theMean,myThid)
      _GLOBAL_SUM_R8(theVar,myThid)
      tmpVal=FLOAT(numPnts)
      _GLOBAL_SUM_R8(tmpVal,myThid)
      numPnts=NINT(tmpVal)

      IF (tmpVal.GT.0.) THEN
       rNumPnts=1. _d 0/tmpVal
       theMean=theMean*rNumPnts
       theVar=theVar*rNumPnts
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
            IF (tmpVal.NE.0.) THEN
             theSDtile=theSDtile+(tmpVal-theMean)**2
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        theSD=theSD+theSDtile
        ENDDO
       ENDDO

       _GLOBAL_SUM_R8(theSD,myThid)

       theSD=sqrt(theSD*rNumPnts)
c      theSD=sqrt(theVar-theMean**2)
      ENDIF

      RETURN
      END
