C $Header: /u/gcmpack/MITgcm/pkg/aim_compon_interf/Attic/atm_store_runoff.F,v 1.1 2003/12/15 02:44:48 jmc Exp $
C $Name:  $  

#include "LAND_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_STORE_RUNOFF( bi,bj,
     I                             myCurrentTime, 
     I                             myCurrentIter, 
     I                             myThid )
C     /==========================================================\
C     | SUBROUTINE ATM_STORE_RUNOFF                              |
C     | o Routine for saving atmos. RunOff from land bucket      |
C     |   for export to coupling layer.                          |
C     |==========================================================|
C     | This version interfaces to the LAND package.             |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "LAND_SIZE.h"

#include "EEPARAMS.h"
#include "PARAMS.h"
c #include "LAND_PARAMS.h"
#include "CPL_PARAMS.h"
#include "LAND_VARS.h"
C     == Global variables for coupling interface ==
#include "ATMCPL.h"
C     == AIMPHYS specific global data ==


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

#ifdef ALLOW_LAND
C     == Local variables ==
C     I,J,K,II - Loop counters
C     conv_precip  :: conversion factor for precip: from g.m-2.s-1 to m/s
      INTEGER I,J,K
      _RL cplTimeFraction

C     o Accumulate RunOff from land bucket that will be exported to the 
C       coupling layer. RunOff is per surface unit, in m/s. 
      cplTimeFraction = 1. _d 0 / DFLOAT(cplSendFrq_iter)
c     DO bj=myByLo(myThid),myByHi(myThid)
c      DO bi=myBxLo(myThid),myBxHi(myThid)
        RunOffTime(bi,bj) = RunOffTime(bi,bj) + cplTimeFraction
        DO J=1,sNy
         DO I=1,sNx
          RunOffFlux(I,J,bi,bj) = RunOffFlux(I,J,bi,bj) 
     &                 + land_runOff(I,J,bi,bj)*cplTimeFraction
         ENDDO
        ENDDO
c      ENDDO
c     ENDDO

#endif /* ALLOW_LAND */

      RETURN
      END
