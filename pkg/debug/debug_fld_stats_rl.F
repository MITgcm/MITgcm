C $Header: /u/gcmpack/MITgcm/pkg/debug/debug_fld_stats_rl.F,v 1.3 2009/04/28 18:10:47 jmc Exp $
C $Name:  $

#include "DEBUG_OPTIONS.h"

      SUBROUTINE DEBUG_FLD_STATS_RL(
     I                myNr, arr,
     O                theMin,theMax,theMean,theSD,
     I                myThid )
C     /==========================================================\
C     | SUBROUTINE DEBUG_FLD_STATS_RL                                  |
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
      _RL theMin
      _RL theMax
      _RL theMean
      _RL theSD
      INTEGER myThid

C     === Local variables ====
      INTEGER bi,bj,I,J,K
      INTEGER numPnts
      LOGICAL noPnts
      _RL tmpVal,rNumPnts
      _RL theVar

      theMin=0.
      theMax=0.
      theMean=0.
      theSD=0.
      theVar=0.
      numPnts=0
      noPnts=.TRUE.

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
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
            theMean=theMean+tmpVal
            theVar=theVar+tmpVal**2
            numPnts=numPnts+1
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      theMin=-theMin
      _GLOBAL_MAX_RL(theMin,myThid)
      theMin=-theMin
      _GLOBAL_MAX_RL(theMax,myThid)
      _GLOBAL_SUM_RL(theMean,myThid)
      _GLOBAL_SUM_RL(theVar,myThid)
      tmpVal=FLOAT(numPnts)
      _GLOBAL_SUM_RL(tmpVal,myThid)
      numPnts=INT(tmpVal+0.5)

      IF (tmpVal.GT.0.) THEN
       rNumPnts=1./tmpVal
       theMean=theMean*rNumPnts
       theVar=theVar*rNumPnts

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO K=1,myNr
          DO J=1,sNy
           DO I=1,sNx
            tmpVal=arr(I,J,K,bi,bj)
            IF (tmpVal.NE.0.) THEN
             theSD=theSD+(tmpVal-theMean)**2
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       _GLOBAL_SUM_RL(theSD,myThid)

       theSD=sqrt(theSD*rNumPnts)
c      theSD=sqrt(theVar-theMean**2)
      ENDIF

      RETURN
      END
