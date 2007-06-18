C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/Attic/atm_store_wspeed.F,v 1.1 2007/06/18 21:28:52 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: ATM_STORE_WSPEED
C     !INTERFACE:
      SUBROUTINE ATM_STORE_WSPEED( bi,bj,
     I                             myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE ATM_STORE_WSPEED
C     | o Routine for saving atmos. surface wind-speed from AIM
C     |   pkg for export to coupling layer.
C     *==========================================================*
C     | This version interfaces to the AIM package.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#ifdef ALLOW_AIM
# include "AIM_SIZE.h"
#else
# include "SIZE.h"
#endif

#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
#ifdef ALLOW_AIM
# include "com_physvar.h"
#endif
C     == Global variables for coupling interface ==
#include "ATMCPL.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi,bj  :: Tile indices
C     myTime :: Current model time
C     myIter :: Current timestep number
C     myThid :: my Thread Id number
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_AIM
C     == Local variables ==
C     i,j :: Loop counters
      INTEGER i,j, ij
      _RL cplTimeFraction

C     o Accumulate Sea-Ice Mass from thSIce pkg that will be exported
C       to the coupling layer. seaIceMass is per surface unit, in kg/m2.
       cplTimeFraction = 1. _d 0 / DFLOAT(cplSendFrq_iter)
       sWSpeedTime(bi,bj) = sWSpeedTime(bi,bj) + cplTimeFraction
       DO j=1,sNy
         DO i=1,sNx
          ij = i + (j-1)*sNx
          sWSpeed(i,j,bi,bj) = sWSpeed(i,j,bi,bj)
     &                       + SPEED0(ij,myThid)*cplTimeFraction
         ENDDO
       ENDDO

#endif /* ALLOW_AIM */

      RETURN
      END
