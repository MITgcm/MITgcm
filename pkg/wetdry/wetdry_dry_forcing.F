#include "WETDRY_OPTIONS.h"
#ifdef ALLOW_PTRACERS
# include "PTRACERS_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: WETDRY_DRY_FORCING

C !INTERFACE:
      SUBROUTINE WETDRY_DRY_FORCING( bi, bj, myThid )

C !DESCRIPTION:
C     No surface forcing on dry columns: the surface heat, salt and
C     fresh-water forcing (surfaceForcingT/S), the penetrating short-wave
C     (Qsw) and the passive-tracer surface forcing (surfaceForcingPTr)
C     are scaled by a column factor
C       fD = MIN(1, MAX(0, (D - wdCritDepth)/wdCritDepth))
C     (0 on dry columns, 1 once D >= 2*wdCritDepth). On a dry flat these
C     fluxes would otherwise act on the few cm of the film (salinity,
C     heat or brine spikes); physically they go into the exposed bed.
C     EmPmR is not changed (it moves volume with useRealFreshWaterFlux).
C     With a non-linear free surface and useRealFreshWaterFlux, the
C     tracer content of that volume, PmEpR*(xx_EvPrRn - surface tracer),
C     is part of surfaceForcingT/S/PTr; it is left unscaled, as the volume
C     it belongs to (scaled, the fresh water would bring in the surface
C     salinity: a salt source wherever fD < 1).
C     Called at the end of S/R EXTERNAL_FORCING_SURF.

C !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "SURFACE.h"
#include "WETDRY.h"
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "PTRACERS_PARAMS.h"
# include "PTRACERS_START.h"
# include "PTRACERS_FIELDS.h"
#endif

C !INPUT PARAMETERS:
      INTEGER bi, bj
      INTEGER myThid
CEOP

#ifdef ALLOW_WETDRY
C !LOCAL VARIABLES:
      INTEGER i, j, ks
#ifdef ALLOW_PTRACERS
      INTEGER iTr
#endif
      LOGICAL realFW
      _RL fwT, fwS
      _RL fD(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      IF ( .NOT.( useWETDRY .AND. wdDryForcing ) ) RETURN

      IF ( usingPCoords ) THEN
       ks = Nr
      ELSE
       ks = 1
      ENDIF
C-    as in EXTERNAL_FORCING_SURF: the fresh-water tracer-content term
      realFW = ( nonlinFreeSurf.GT.0 .OR. usingPCoords )
     &         .AND. useRealFreshWaterFlux
      fwT = 0. _d 0
      fwS = 0. _d 0

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        fD(i,j) = MIN( oneRL, MAX( zeroRL,
     &     ( etaH(i,j,bi,bj) + Ro_surf(i,j,bi,bj) - R_low(i,j,bi,bj)
     &       - wdCritDepth )/wdCritDepth ) )
        IF ( realFW ) THEN
         IF ( temp_EvPrRn.NE.UNSET_RL ) fwT = PmEpR(i,j,bi,bj)
     &      *( temp_EvPrRn - theta(i,j,ks,bi,bj) )*mass2rUnit
         IF ( salt_EvPrRn.NE.UNSET_RL ) fwS = PmEpR(i,j,bi,bj)
     &      *( salt_EvPrRn - salt(i,j,ks,bi,bj) )*mass2rUnit
        ENDIF
        surfaceForcingT(i,j,bi,bj) =
     &     ( surfaceForcingT(i,j,bi,bj) - fwT )*fD(i,j) + fwT
        surfaceForcingS(i,j,bi,bj) =
     &     ( surfaceForcingS(i,j,bi,bj) - fwS )*fD(i,j) + fwS
C-    Qsw is not reloaded every step (only with EXF or periodic forcing):
C     if it still holds our last output, scale the last input again
        IF ( Qsw(i,j,bi,bj).NE.wdQswOut(i,j,bi,bj) )
     &    wdQswIn(i,j,bi,bj) = Qsw(i,j,bi,bj)
        Qsw(i,j,bi,bj) = wdQswIn(i,j,bi,bj)*fD(i,j)
        wdQswOut(i,j,bi,bj) = Qsw(i,j,bi,bj)
       ENDDO
      ENDDO
#ifdef ALLOW_PTRACERS
      IF ( usePTRACERS ) THEN
       DO iTr=1,PTRACERS_numInUse
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          fwS = 0. _d 0
          IF ( realFW .AND. PTRACERS_StepFwd(iTr) .AND.
     &         PTRACERS_EvPrRn(iTr).NE.UNSET_RL ) fwS = PmEpR(i,j,bi,bj)
     &      *( PTRACERS_EvPrRn(iTr) - pTracer(i,j,ks,bi,bj,iTr) )
     &      *mass2rUnit
          surfaceForcingPTr(i,j,bi,bj,iTr) =
     &       ( surfaceForcingPTr(i,j,bi,bj,iTr) - fwS )*fD(i,j) + fwS
         ENDDO
        ENDDO
       ENDDO
      ENDIF
#endif /* ALLOW_PTRACERS */
#endif /* ALLOW_WETDRY */

      RETURN
      END
