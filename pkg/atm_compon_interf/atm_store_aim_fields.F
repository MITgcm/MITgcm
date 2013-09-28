C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/atm_store_aim_fields.F,v 1.2 2013/09/28 00:15:18 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_AIM
# include "AIM_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: ATM_STORE_AIM_FIELDS
C     !INTERFACE:
      SUBROUTINE ATM_STORE_AIM_FIELDS(
     I                     bi, bj,
     I                     myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE ATM_STORE_AIM_FIELDS
C     | o Routine for saving AIM fields (e.g., surface wind
C     |   speed) for export to coupling layer.
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
c#include "ATMIDS.h"
#include "CPL_PARAMS.h"
#ifdef ALLOW_AIM
# include "AIM2DYN.h"
# include "AIM_CO2.h"
#endif
C     == Global variables for coupling interface ==
#include "ATMCPL.h"

C     !INPUT/OUTPUT PARAMETERS:
C     bi, bj    :: Tile indices
C     myTime    :: Current time in simulation (s)
C     myIter    :: Current iteration number
C     myThid    :: My Thread Id. number
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_AIM
C     !LOCAL VARIABLES:
C     i, j      :: Loop counters
      INTEGER i,j
      _RL cplTimeFraction

      cplTimeFraction = 1. _d 0 / DFLOAT(cplSendFrq_iter)

C     o Accumulate atmospheric surface wind speed (m/s) from AIM pkg
C       that will be exported to the coupling layer.
       sWSpeedTime(bi,bj) = sWSpeedTime(bi,bj) + cplTimeFraction
       DO j=1,sNy
        DO i=1,sNx
          sWSpeed(i,j,bi,bj) = sWSpeed(i,j,bi,bj)
     &                       + aim_surfWind(i,j,bi,bj)*cplTimeFraction
        ENDDO
       ENDDO

#ifdef ALLOW_AIM_CO2
c     IF ( atmCpl_exchange_DIC ) THEN
C     o Accumulate atmospheric CO2 from Aim pkg that will be exported
C       to the coupling layer.
       airCO2Time(bi,bj) = airCO2Time(bi,bj) + cplTimeFraction
       DO j=1,sNy
        DO i=1,sNx
          airCO2(i,j,bi,bj) = airCO2(i,j,bi,bj)
     &                      + aim_CO2(i,j,bi,bj)*cplTimeFraction
        ENDDO
       ENDDO
c     ENDIF
#endif /* ALLOW_AIM_CO2 */

#endif /* ALLOW_AIM */

      RETURN
      END
