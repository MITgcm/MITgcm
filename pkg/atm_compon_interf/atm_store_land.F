C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/atm_store_land.F,v 1.1 2013/09/11 20:17:27 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: ATM_STORE_LAND
C     !INTERFACE:
      SUBROUTINE ATM_STORE_LAND(
     I                     bi, bj,
     I                     myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE ATM_STORE_LAND
C     | o Routine for saving Land fluxes from LAND pkg
C     |   for export to coupling layer.
C     *==========================================================*
C     | This version interfaces to the LAND package.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#ifdef ALLOW_LAND
# include "LAND_SIZE.h"
#else
# include "SIZE.h"
#endif

#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
#ifdef ALLOW_LAND
c #include "LAND_PARAMS.h"
# include "LAND_VARS.h"
#endif
C     == Global variables for coupling interface ==
#include "ATMCPL.h"

C     !INPUT/OUTPUT PARAMETERS:
C     bi, bj    :: Tile indices
C     myTime    :: Current model time
C     myIter    :: Current timestep number
C     myThid    :: my Thread Id number
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_LAND
C     !LOCAL VARIABLES:
C     i, j      :: Loop counters
      INTEGER i, j
      _RL cplTimeFraction

C     o Accumulate RunOff from land bucket that will be exported to the
C       coupling layer. RunOff is per surface unit, in kg/m2/s
       cplTimeFraction = 1. _d 0 / DFLOAT(cplSendFrq_iter)
       RunOffTime(bi,bj) = RunOffTime(bi,bj) + cplTimeFraction
       DO j=1,sNy
        DO i=1,sNx
          RunOffFlux(i,j,bi,bj) = RunOffFlux(i,j,bi,bj)
     &                 + land_runOff(i,j,bi,bj)*cplTimeFraction
        ENDDO
       ENDDO

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
