#include "SALT_PLUME_OPTIONS.h"

CBOP
C     !ROUTINE: SALT_PLUME_FORCING_SURF
C     !INTERFACE:
      SUBROUTINE SALT_PLUME_FORCING_SURF(
     I                            bi, bj, iMin, iMax, jMin, jMax,
     I                            myTime,myIter,myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SALT_PLUME_FORCING_SURF
C     | o saltPlume is the amount of salt rejected by ice while freezing;
C     |   it is here subtracted from surfaceForcingS and will be redistributed
C     |   to multiple vertical levels later on as per Duffy et al. (GRL 1999)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "SALT_PLUME.h"

C     !INPUT PARAMETERS:
C     bi,bj                :: tile indices
C     myTime               :: model time
C     myIter               :: time-step number
C     myThid               :: thread number
      INTEGER bi, bj, iMin, iMax, jMin, jMax
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_SALT_PLUME
C     !LOCAL VARIABLES:
C     i,j                  :: loop indices
C     ks                   :: surface level index
      INTEGER i, j, ks
      IF ( usingPCoords ) THEN
         ks = Nr
      ELSE
         ks = 1
      ENDIF

      DO j = jMin, jMax
       DO i = iMin, iMax
#ifdef SALT_PLUME_VOLUME
        SPforcS1(i,j,bi,bj)=SPbrineVolFlux(i,j,bi,bj)
     &   *SPbrineSconst*rhoConst
        SPforcT1(i,j,bi,bj)=SPbrineVolFlux(i,j,bi,bj)
     &   *theta(i,j,1,bi,bj)*rhoConst
     &   *HeatCapacity_Cp

Cunits: surfaceForcingT [Kelvin.m/s], surfaceForcingS [g/kg.m/s]
C SPforcS1 has same unit as saltPlumeFlux [g/m2/s]=[g/kg kg/m2/s]
C SPforcT1: [W/m2]
        surfaceForcingS(i,j,bi,bj) = surfaceForcingS(i,j,bi,bj)
     &      - SPforcS1(i,j,bi,bj) * mass2rUnit
        surfaceForcingT(i,j,bi,bj) = surfaceForcingT(i,j,bi,bj)
     &      - SPforcT1(i,j,bi,bj) * mass2rUnit / HeatCapacity_Cp
#else /* SALT_PLUME_VOLUME */
        surfaceForcingS(i,j,bi,bj) = surfaceForcingS(i,j,bi,bj)
     &       - saltPlumeFlux(i,j,bi,bj) * mass2rUnit
#endif /* SALT_PLUME_VOLUME */
       ENDDO
      ENDDO
#endif /* ALLOW_SALT_PLUME */

      RETURN
      END
