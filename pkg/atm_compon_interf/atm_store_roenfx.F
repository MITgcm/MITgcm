C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/Attic/atm_store_roenfx.F,v 1.1 2004/05/21 19:59:38 jmc Exp $
C $Name:  $  

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_STORE_ROENFX( bi,bj,
     I                             myTime, 
     I                             myIter, 
     I                             myThid )
C     /==========================================================\
C     | SUBROUTINE ATM_STORE_ROENFX                              |
C     | o Routine for saving atmos. RunOff Energy Flux from land |
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
C     i,j     :: Loop counters
      INTEGER i,j
      _RL cplTimeFraction

C     o Accumulate RunOff Energy from land bucket that will be exported
C       to the coupling layer. RunOff Energy is per surface unit, in W/m2. 
       cplTimeFraction = 1. _d 0 / DFLOAT(cplSendFrq_iter)
       ROEnFxTime(bi,bj) = ROEnFxTime(bi,bj) + cplTimeFraction
       DO j=1,sNy
         DO i=1,sNx
          RunOffEnFx(i,j,bi,bj) = RunOffEnFx(i,j,bi,bj) 
     &                 + land_enRnOf(i,j,bi,bj)*cplTimeFraction
         ENDDO
       ENDDO

#endif /* ALLOW_LAND */

      RETURN
      END
