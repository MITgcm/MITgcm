C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_advcflw.F,v 1.7 2009/04/28 18:16:52 jmc Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: MON_ADVCFLW

C     !INTERFACE:
      SUBROUTINE MON_ADVCFLW(
     I     label, W, rDz, dT,
     I     myThid )

C     !DESCRIPTION:
C     Calculates maximum CFL number in the vertical direction.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "MONITOR.h"

C     !INPUT PARAMETERS:
      CHARACTER*(*) label
      _RL W(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS rDz(Nr)
      _RL dT
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
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
      _GLOBAL_MAX_RL(theMax,myThid)

      CALL MON_OUT_RL( label, theMax, mon_foot_max ,myThid)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
