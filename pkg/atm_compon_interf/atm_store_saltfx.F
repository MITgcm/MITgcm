C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/Attic/atm_store_saltfx.F,v 1.1 2004/05/21 19:59:38 jmc Exp $
C $Name:  $  

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_STORE_SALTFX( bi,bj,
     I                             myTime, 
     I                             myIter, 
     I                             myThid )
C     /==========================================================\
C     | SUBROUTINE ATM_STORE_SALTFX                              |
C     | o Routine for saving atmos. salt flux from sea-ice       |
C     |   for export to coupling layer.                          |
C     |==========================================================|
C     | This version interfaces to the main model                |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"

#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
C     == Global variables (main model)
#include "FFIELDS.h"
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

#ifdef COMPONENT_MODULE
C     == Local variables ==
C     i,j - Loop counters
      INTEGER i,j
      _RL cplTimeFraction

C     o Accumulate SaltFlux from sea-ice that will be exported
C       to the coupling layer. saltflux is in psu.kg/m^2/s (+=upward)
       cplTimeFraction = 1. _d 0 / DFLOAT(cplSendFrq_iter)
       saltFxTime(bi,bj) = saltFxTime(bi,bj) + cplTimeFraction
       DO j=1,sNy
         DO i=1,sNx
          iceSaltFlx(i,j,bi,bj) = iceSaltFlx(i,j,bi,bj) 
     &                 + saltFlux(i,j,bi,bj)*cplTimeFraction
         ENDDO
       ENDDO

#endif /* COMPONENT_MODULE */

      RETURN
      END
