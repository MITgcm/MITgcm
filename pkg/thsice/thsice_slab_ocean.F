#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_SLAB_OCEAN
C     !INTERFACE:
      SUBROUTINE THSICE_SLAB_OCEAN(
     I                      aim_sWght0, aim_sWght1,
     O                      dTsurf,
     I                      bi, bj, myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R  THSICE_SLAB_OCEAN
C     | o Slab ocean for atmosphere (and sea-ice) model
C     *==========================================================*
C     | o add ocean-surface fluxes + restoring term
C     |   and step forward ocean mixed-layer Temp. & Salinity
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
C-- MITgcm
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"

C-- Sea-Ice package
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"
#include "THSICE_TAVE.h"

C-- Physics package
#ifdef ALLOW_AIM
#include "AIM_FFIELDS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     aim_sWght0 :: weight for time interpolation of surface BC
C     aim_sWght1 :: 0/1 = time period before/after the current time
C     dTsurf     :: diagnostics of slab-ocean temperature change [K/iter]
C     bi,bj      :: tile indices
C     myTime     :: Current time of simulation ( s )
C     myIter     :: Current iteration number in simulation
C     myThid     :: my Thread number Id.
      _RL     aim_sWght0, aim_sWght1
      _RL     dTsurf(sNx,sNy)
      _RL     myTime
      INTEGER bi,bj
      INTEGER myIter, myThid
CEOP

#ifdef ALLOW_THSICE

C     == Local variables ==
C     i,j          :: Loop counters
      _RL dtFac, fwFac, heatFac
#ifdef ALLOW_AIM
      _RL oceTfreez, locTemp, locQflux, dtFacR
#endif
      INTEGER i,j

cph the following structure is not supported by TAF
cph      IF ( .NOT.stepFwd_oceMxL ) RETURN
      IF ( stepFwd_oceMxL ) THEN

C--    add heat flux and fresh-water + salt flux :
       dtFac   = ocean_deltaT/rhosw
       fwFac   = ocean_deltaT*sMxL_default/rhosw
       heatFac = ocean_deltaT/(cpwater*rhosw)
       DO j=1,sNy
        DO i=1,sNx
         IF ( hOceMxL(i,j,bi,bj).NE.0. _d 0 ) THEN
          dTsurf(i,j) = tOceMxL(i,j,bi,bj)
          tOceMxL(i,j,bi,bj) = tOceMxL(i,j,bi,bj)
     &       - heatFac*Qnet(i,j,bi,bj) / hOceMxL(i,j,bi,bj)
          sOceMxL(i,j,bi,bj) = sOceMxL(i,j,bi,bj)
     &       + (fwFac*EmPmR(i,j,bi,bj) - dtFac*saltFlux(i,j,bi,bj))
     &                                 / hOceMxL(i,j,bi,bj)
         ENDIF
        ENDDO
       ENDDO

#ifdef ALLOW_AIM
       IF ( tauRelax_MxL_salt .GT. 0. _d 0 ) THEN
C--    add restoring (backward) toward climatological fixed Salinity
        dtFac   = ocean_deltaT/tauRelax_MxL_salt
        dtFacR  = 1. _d 0 /(1. _d 0 + dtFac)
        DO j=1,sNy
         DO i=1,sNx
          IF ( hOceMxL(i,j,bi,bj).NE.0. _d 0 ) THEN
           sOceMxL(i,j,bi,bj) =
     &         (sOceMxL(i,j,bi,bj) + dtFac*sMxL_default)*dtFacR
          ENDIF
         ENDDO
        ENDDO
       ENDIF
       IF ( tauRelax_MxL .GT. 0. _d 0 ) THEN
C--    add restoring (backward) toward climatological Temp.
        dtFac   = ocean_deltaT/tauRelax_MxL
        dtFacR  = 1. _d 0 /(1. _d 0 + dtFac)
        oceTfreez = - 1.9 _d 0
        DO j=1,sNy
         DO i=1,sNx
          IF ( hOceMxL(i,j,bi,bj).NE.0. _d 0 ) THEN
           oceTfreez = -mu_Tf*sOceMxL(i,j,bi,bj)
           locTemp = ( aim_sWght0*aim_sst0(i,j,bi,bj)
     &               + aim_sWght1*aim_sst1(i,j,bi,bj)
     &               ) - celsius2K
           locTemp = MAX( locTemp , oceTfreez )
           tOceMxL(i,j,bi,bj) =
     &         (tOceMxL(i,j,bi,bj) + dtFac*locTemp)*dtFacR
          ENDIF
         ENDDO
        ENDDO
       ENDIF
       DO j=1,sNy
        DO i=1,sNx
          IF ( hOceMxL(i,j,bi,bj).NE.0. _d 0 ) THEN
           locQflux = ( aim_sWght0*aim_qfx0(i,j,bi,bj)
     &                + aim_sWght1*aim_qfx1(i,j,bi,bj)
     &                )
           tOceMxL(i,j,bi,bj) = tOceMxL(i,j,bi,bj)
     &            + heatFac*locQflux / hOceMxL(i,j,bi,bj)
          ENDIF
        ENDDO
       ENDDO
#endif /* ALLOW_AIM */

C-    Diagnose surf. temp. change
       DO j=1,sNy
        DO i=1,sNx
         IF ( hOceMxL(i,j,bi,bj).NE.0. _d 0 ) THEN
          dTsurf(i,j) = tOceMxL(i,j,bi,bj) - dTsurf(i,j)
         ENDIF
        ENDDO
       ENDDO

#ifdef ALLOW_TIMEAVE
C-- average of Ocean Mixed-Layer temp. & salinity
       IF ( thSIce_taveFreq .GT. 0. _d 0 ) THEN
        CALL TIMEAVE_CUMULATE( ice_tMxL_Ave, tOceMxL,
     &                         1, thSIce_deltaT, bi, bj, myThid )
        CALL TIMEAVE_CUMULATE( ice_sMxL_Ave, sOceMxL,
     &                         1, thSIce_deltaT, bi, bj, myThid )
       ENDIF
#endif /* ALLOW_TIMEAVE */

c-- End of IF ( stepFwd_oceMxL ) THEN
      ENDIF

C--   Cumulate time-averaged fields and also fill-up flux diagnostics
C     (if not done in THSICE_DO_ADVECT call)
      IF ( thSIceAdvScheme.LE.0 ) THEN
         CALL THSICE_AVE(
     I                    bi, bj, myTime, myIter, myThid )
      ENDIF

#endif  /* ALLOW_THSICE */

      RETURN
      END
