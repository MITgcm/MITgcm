C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/atm_store_surfflux.F,v 1.1 2013/09/11 20:17:27 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: ATM_STORE_SURFFLUX
C     !INTERFACE:
      SUBROUTINE ATM_STORE_SURFFLUX(
     I                     bi, bj,
     I                     myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE ATM_STORE_SURFFLUX
C     | o Routine for saving surface flux fields (in FFIELDS.h)
C     |   for export to coupling layer.
C     *==========================================================*
C     | This version interfaces to the main model
C     *==========================================================*
C     \ev

C     !USES:
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

C     !INPUT/OUTPUT PARAMETERS:
C     bi, bj    :: Tile indices
C     myTime    :: Current time in simulation (s)
C     myIter    :: Current iteration number
C     myThid    :: My Thread Id. number
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     i, j      :: Loop counters
      INTEGER i,j
      _RL cplTimeFraction
CEOP

       cplTimeFraction = 1. _d 0 / DFLOAT(cplSendFrq_iter)

C     o Accumulate net surface heat flux (Qnet, +=upward, W/m2)
C       that will be exported to the coupling layer.
       HeatFluxTime(bi,bj) = HeatFluxTime(bi,bj) + cplTimeFraction
       DO j=1,sNy
        DO i=1,sNx
          HeatFlux(i,j,bi,bj) = HeatFlux(i,j,bi,bj)
     &                        + Qnet(i,j,bi,bj)*cplTimeFraction
        ENDDO
       ENDDO

C     o Accumulate net surface shortwave heat flux (Qsw, +=upward, W/m2)
C       that will be exported to the coupling layer.
       qShortWaveTime(bi,bj) = qShortWaveTime(bi,bj) + cplTimeFraction
       DO j=1,sNy
        DO i=1,sNx
          qShortWave(i,j,bi,bj) = qShortWave(i,j,bi,bj)
     &                          + Qsw(i,j,bi,bj)*cplTimeFraction
        ENDDO
       ENDDO

C     o Accumulate fresh water flux ( E-P, +=upward, kg/m^2/s)
C       that will be exported to the coupling layer.
       EvMPrTime(bi,bj) = EvMPrTime(bi,bj) + cplTimeFraction
       DO j=1,sNy
        DO i=1,sNx
          EvMPrFlux(i,j,bi,bj) = EvMPrFlux(i,j,bi,bj)
     &                         + EmPmR(i,j,bi,bj)*cplTimeFraction
        ENDDO
       ENDDO

      IF ( useThSIce ) THEN
C     o Accumulate SaltFlux from sea-ice (saltFlux, +=upward, g/m^2/s)
C       that will be exported to the coupling layer.
       saltFxTime(bi,bj) = saltFxTime(bi,bj) + cplTimeFraction
       DO j=1,sNy
        DO i=1,sNx
          iceSaltFlx(i,j,bi,bj) = iceSaltFlx(i,j,bi,bj)
     &                 + saltFlux(i,j,bi,bj)*cplTimeFraction
        ENDDO
       ENDDO
      ENDIF

      IF ( useAtm_Phys ) THEN
C     o Accumulate surface wind-stress
C       that will be exported to the coupling layer.
       tauXTime(bi,bj) = tauXTime(bi,bj) + cplTimeFraction
       tauYTime(bi,bj) = tauYTime(bi,bj) + cplTimeFraction
       DO j=1,sNy
        DO i=1,sNx
          tauX(i,j,bi,bj) = tauX(i,j,bi,bj)
     &                    + fu(i,j,bi,bj)*cplTimeFraction
          tauY(i,j,bi,bj) = tauY(i,j,bi,bj)
     &                    + fv(i,j,bi,bj)*cplTimeFraction
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
