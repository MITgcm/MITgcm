C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/Attic/atm_store_airco2.F,v 1.2 2009/01/05 15:19:17 dfer Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_AIM
# include "AIM_OPTIONS.h"
#endif

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
#ifdef ALLOW_AIM
# include "AIM_SIZE.h"
#else
# include "SIZE.h"
#endif

#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
#ifdef ALLOW_AIM
#include "AIM_CO2.h"
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
#ifdef ALLOW_AIM_CO2
C     == Local variables ==
C     i,j :: Loop counters
      INTEGER i,j, ij
      _RL cplTimeFraction

C     o Accumulate atmospheric CO2 from Aim pkg that will be exported
C       to the coupling layer.
       cplTimeFraction = 1. _d 0 / DFLOAT(cplSendFrq_iter)
       airCO2Time(bi,bj) = airCO2Time(bi,bj) + cplTimeFraction
       DO j=1,sNy
         DO i=1,sNx
          ij = i + (j-1)*sNx
          airCO2(i,j,bi,bj) = airCO2(i,j,bi,bj)
     &                      + aim_CO2(ij,myThid)*cplTimeFraction
         ENDDO
       ENDDO

#endif
#endif /* ALLOW_AIM */

      RETURN
      END
