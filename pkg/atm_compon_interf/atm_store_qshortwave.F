C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/Attic/atm_store_qshortwave.F,v 1.3 2004/05/21 20:00:48 jmc Exp $
C $Name:  $  

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_STORE_QSHORTWAVE(bi,bj,
     I                                myTime, 
     I                                myIter, 
     I                                myThid )
C     /==========================================================\
C     | SUBROUTINE ATM_STORE_QSHORTWAVE                          |
C     | o Routine for saving atmos. lower bdy shortwave heat flux|
C     |   export to coupling layer.                              |
C     |==========================================================|
C     | This version interfaces to the MITgcm AIMPHYS package.   |
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

C     o Accumulate heat flux that will be exported to the coupling layer.
C     +ve into atmos., W/m2.

       cplTimeFraction = 1. _d 0 / DFLOAT(cplSendFrq_iter)
       qShortWaveTime(bi,bj) = qShortWaveTime(bi,bj) + cplTimeFraction
       DO j=1,sNy
         DO i=1,sNx
          qShortWave(i,j,bi,bj) = qShortWave(i,j,bi,bj)
     &                          + Qsw(i,j,bi,bj)*cplTimeFraction
         ENDDO
       ENDDO

#endif /* COMPONENT_MODULE */

      RETURN
      END
