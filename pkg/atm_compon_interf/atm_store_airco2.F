C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/Attic/atm_store_airco2.F,v 1.1 2007/06/18 21:28:52 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: ATM_STORE_AIRCO2
C     !INTERFACE:
      SUBROUTINE ATM_STORE_AIRCO2( bi,bj,
     I                             myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE ATM_STORE_AIRCO2
C     | o Routine for saving atmos. CO2 concentration from ??? pkg
C     |   for export to coupling layer.
C     *==========================================================*
C     | This version interfaces to the ??? package.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"

#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
c#ifdef ALLOW_AIM
c#include "AIM_SIZE.h"
c#include "com_physvar.h"
c#endif
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
       airCO2Time(bi,bj) = airCO2Time(bi,bj) + cplTimeFraction
       DO j=1,sNy
         DO i=1,sNx
          ij = i + (j-1)*sNx
          airCO2(i,j,bi,bj) = airCO2(i,j,bi,bj)
c    &                      + aim_CO2(ij,myThid)*cplTimeFraction
         ENDDO
       ENDDO

#endif /* ALLOW_AIM */

      RETURN
      END
