C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_advcfl.F,v 1.7 2009/04/28 18:16:52 jmc Exp $
C $Name:  $

#include "MONITOR_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: MON_ADVCFL

C     !INTERFACE:
      SUBROUTINE MON_ADVCFL(
     I     label, U, rDx, dT,
     I     myThid )

C     !DESCRIPTION:
C     Calculates maximum CFL number

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "MONITOR.h"

C     !INPUT PARAMETERS:
      CHARACTER*(*) label
      _RL U(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS rDx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
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
           tmpVal=abs(U(I,J,K,bi,bj))*rDx(I,J,BI,BJ)*dT
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
