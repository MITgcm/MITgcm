C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_advcfl.F,v 1.4 2001/06/25 20:35:23 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE MON_ADVCFL(
     I                label, U, rDx, dT,
     I                myThid )
C     /==========================================================\
C     | SUBROUTINE MON_ADVCFL                                    |
C     | o Calculates maximum CFL number                          |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "MONITOR.h"

C     === Routine arguments ===
      CHARACTER*(*) label
      _RL U(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS rDx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
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
           tmpVal=abs(U(I,J,K,bi,bj))*rDx(I,J,BI,BJ)*dT
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
