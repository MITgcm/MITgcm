C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/Attic/atm_store_runoff.F,v 1.2 2004/05/21 20:00:48 jmc Exp $
C $Name:  $  

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_STORE_RUNOFF( bi,bj,
     I                             myTime, 
     I                             myIter, 
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
#ifdef ALLOW_LAND
#include "LAND_SIZE.h"
#else
#include "SIZE.h"
#endif

#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"

#ifdef ALLOW_LAND
c #include "LAND_PARAMS.h"
#include "LAND_VARS.h"
#endif
C     == Global variables for coupling interface ==
#include "ATMCPL.h"


C     == Routine arguments ==
C     bi,bj  - Tile index
C     myThid - Thread number for this instance of the routine
C     myIter - Current timestep number
C     myTime - Current model time
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter
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
       RunOffTime(bi,bj) = RunOffTime(bi,bj) + cplTimeFraction
       DO J=1,sNy
         DO I=1,sNx
          RunOffFlux(I,J,bi,bj) = RunOffFlux(I,J,bi,bj) 
     &                 + land_runOff(I,J,bi,bj)*cplTimeFraction
         ENDDO
       ENDDO

#endif /* ALLOW_LAND */

      RETURN
      END
