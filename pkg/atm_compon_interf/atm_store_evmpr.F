C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/Attic/atm_store_evmpr.F,v 1.2 2004/05/21 20:00:48 jmc Exp $
C $Name:  $  

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_STORE_EVMPR( bi,bj,
     I                             myTime, 
     I                             myIter, 
     I                             myThid )
C     /==========================================================\
C     | SUBROUTINE ATM_STORE_EVMPR                               |
C     | o Routine for saving atmos. fresh water flux             |
C     |   (=Evap-Precip) for export to coupling layer.           |
C     |==========================================================|
C     | This version interfaces to the MITgcm AIMPHYS package.   |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "AIM_SIZE.h"
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

C     o Accumulate fresh water flux that will be exported to the 
C       coupling layer. Fresh-water flux is in units of g/m^2/s. 
C       To convert to meters per second divide by density in 
C       g/m^3. Positive flux is into the atmosphere (E-P).

       cplTimeFraction = 1. _d 0 / DFLOAT(cplSendFrq_iter)
       EvMPrTime(bi,bj) = EvMPrTime(bi,bj) + cplTimeFraction
       DO j=1,sNy
         DO i=1,sNx
          EvMPrFlux(i,j,bi,bj) = EvMPrFlux(i,j,bi,bj) 
     &                         + EmPmR(i,j,bi,bj)*cplTimeFraction
         ENDDO
       ENDDO

#endif /* COMPONENT_MODULE */

      RETURN
      END
