C $Header: /u/gcmpack/MITgcm/pkg/monitor/mon_advcflw.F,v 1.1 2001/05/30 19:33:18 adcroft Exp $
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

      _BEGIN_MASTER( myThid )
      WRITE(*,'(A,A20,A,1PE22.14)')
     &      'MON_ADVCFLW: ',label,'  max=',theMax
      _END_MASTER( )

      RETURN
      END
