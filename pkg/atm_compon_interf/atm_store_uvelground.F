C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/Attic/atm_store_uvelground.F,v 1.1 2003/12/15 02:44:48 jmc Exp $
C $Name:  $  

#include "AIM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_STORE_UVELGROUND( bi,bj,
     I                           myCurrentTime, 
     I                           myCurrentIter, 
     I                           myThid )
C     /==========================================================\
C     | SUBROUTINE ATM_STORE_UVELGROUND                          |
C     | o Routine for saving atmos. lower boundary wind speed    |
C     |   for export to coupling layer.                          |
C     |==========================================================|
C     | This version interfaces to the MITgcm AIMPHYS package.   |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
#include "DYNVARS.h"
C     == Global variables for coupling interface ==
#include "ATMCPL.h"

C     == Routine arguments ==
C     bi,bj         - Tile index
C     myThid        - Thread number for this instance of the routine
C     myCurrentIter - Current timestep number
C     myCurrentTime - Current model time
      INTEGER bi, bj
      _RL     myCurrentTime
      INTEGER myCurrentIter
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     I,J,K - Loop counters
      INTEGER I,J,K
      _RL cplTimeFraction

C     o Accumulate zonal wind speed that will be exported to the 
C       coupling layer. Speed is in m/s with positive 
C       to the east. Wind speed is in COMMON /DYNVARS_R/,
C       DYNVARS.h variable uVel using a south-west-c-grid staggering.
      cplTimeFraction = 1. _d 0 / DFLOAT(cplSendFrq_iter)
c     DO bj=myByLo(myThid),myByHi(myThid)
c      DO bi=myBxLo(myThid),myBxHi(myThid)
        uVelGroundTime(bi,bj) = uVelGroundTime(bi,bj) + cplTimeFraction
        DO J=1,sNy
         DO I=1,sNx
          uVelGround(I,J,bi,bj) = 
     &    uVelGround(I,J,bi,bj) + uVel(I,J,1,bi,bj)*cplTimeFraction
         ENDDO
        ENDDO
c      ENDDO
c     ENDDO

      RETURN
      END
