C $Header: /u/gcmpack/MITgcm/pkg/aim_compon_interf/Attic/atm_store_seaice.F,v 1.1 2004/05/21 19:59:38 jmc Exp $
C $Name:  $  

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_STORE_SEAICE( bi,bj,
     I                             myTime, 
     I                             myIter, 
     I                             myThid )
C     /==========================================================\
C     | SUBROUTINE ATM_STORE_SEAICE                              |
C     | o Routine for saving atmos. Sea-Ice Mass from thSIce pkg |
C     |   for export to coupling layer.                          |
C     |==========================================================|
C     | This version interfaces to the LAND package.             |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"

#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
#ifdef ALLOW_THSICE
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"
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

#ifdef ALLOW_THSICE
C     == Local variables ==
C     i,j - Loop counters
      INTEGER i,j
      _RL cplTimeFraction

C     o Accumulate Sea-Ice Mass from thSIce pkg that will be exported
C       to the coupling layer. seaIceMass is per surface unit, in kg/m2. 
       cplTimeFraction = 1. _d 0 / DFLOAT(cplSendFrq_iter)
       seaIceTime(bi,bj) = seaIceTime(bi,bj) + cplTimeFraction
C-     Needs really to store the last time-step value and not the average
       cplTimeFraction = 1. _d 0
       seaIceTime(bi,bj) = cplTimeFraction
       DO j=1,sNy
         DO i=1,sNx
c         seaIceMass(i,j,bi,bj) = seaIceMass(i,j,bi,bj) +
          seaIceMass(i,j,bi,bj) = 
     &                   ( snowHeight(i,j,bi,bj)*rhos 
     &                    + iceHeight(i,j,bi,bj)*rhoi 
     &                   )*iceMask(i,j,bi,bj)*cplTimeFraction
         ENDDO
       ENDDO

#endif /* ALLOW_THSICE */

      RETURN
      END
