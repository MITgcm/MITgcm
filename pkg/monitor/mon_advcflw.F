C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_advcflw.F,v 1.4 2001/06/25 20:35:23 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MON_ADVCFLW(
     I                label, W, rDz, dT,
     I                myThid )
C     /==========================================================\
C     | SUBROUTINE MON_ADVCFLW                                   |
C     | o Calculates maximum CFL number in vertical              |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "MONITOR.h"

C     === Routine arguments ===
      CHARACTER*(*) label
      _RL W(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS rDz(Nr)
      _RL dT
      INTEGER myThid

C     === Local variables ====
      INTEGER bi,bj,I,J,K
      _RL tmpVal,theMax

      theMax=0.

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO K=1,Nr
         DO J=1,sNy
          DO I=1,sNx
           tmpVal=abs(W(I,J,K,bi,bj))*rDz(K)*dT
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
