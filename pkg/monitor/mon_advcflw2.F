C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_advcflw2.F,v 1.1 2001/07/03 21:34:52 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MON_ADVCFLW2(
     I                label, W, rHFac, rDrF, dT,
     I                myThid )
C     /==========================================================\
C     | SUBROUTINE MON_ADVCFLW2                                  |
C     | o Calculates maximum CFL number in vertical              |
C     |   relevant for tracer Adv. Pb. with Partial Cell         |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "MONITOR.h"

C     === Routine arguments ===
      CHARACTER*(*) label
      _RL     W(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS rHFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy) 
      _RS rDrF(Nr)
      _RL dT
      INTEGER myThid

C     === Local variables ====
      INTEGER bi,bj,I,J,K
      _RL tmpVal,theMax

      theMax=0.

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO K=2,Nr
         DO J=1,sNy
          DO I=1,sNx
           tmpVal=abs(W(I,J,K,bi,bj))*dT
     &           *max(  rDrF(K)*rHfac(I,J,K,bi,bj),
     &                rDrF(K-1)*rHfac(I,J,K-1,bi,bj) )
           theMax=max(theMax,tmpVal)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      _GLOBAL_MAX_R8(theMax,myThid)

      CALL MON_OUT_RL( label, theMax, mon_foot_max ,myThid)

      RETURN
      END
