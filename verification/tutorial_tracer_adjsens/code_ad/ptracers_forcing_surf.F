#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_FORCING_SURF

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_FORCING_SURF(
     I                            relaxForcingS,
     I                            bi, bj, iMin, iMax, jMin, jMax,
     I                            myTime,myIter,myThid )

C !DESCRIPTION:
C     Precomputes surface forcing term for pkg/ptracers.
C     Precomputation is needed because of non-local KPP transport term,
C     routine KPP_TRANSPORT_PTR.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_START.h"
#include "PTRACERS_FIELDS.h"

C !INPUT PARAMETERS: ===================================================
C  relaxForcingS        :: Salt forcing due to surface relaxation
C  bi,bj                :: tile indices
C  myTime               :: model time
C  myIter               :: time-step number
C  myThid               :: thread number
      _RL relaxForcingS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER bi, bj, iMin, iMax, jMin, jMax
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  iTrc                 :: tracer index
C  ks                   :: surface level index
      INTEGER i, j
      INTEGER iTrc, ks
      _RL add2EmP(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL epsil, cutoff, tmpVar
CEOP

      IF ( usingPCoords ) THEN
        ks = Nr
      ELSE
        ks = 1
      ENDIF

C Example of how to add forcing at the surface
      DO iTrc=1,PTRACERS_numInUse
c       IF ( PTRACERS_StepFwd(iTrc) ) THEN
          DO j = jMin, jMax
           DO i = iMin, iMax
             surfaceForcingPTr(i,j,bi,bj,iTrc) = 0. _d 0
     &                        + surfaceForcingS(i,j,bi,bj)
           ENDDO
          ENDDO
c       ENDIF
      ENDDO

C--   Option to convert Salt-relaxation into additional EmP contribution
      IF ( PTRACERS_addSrelax2EmP ) THEN
C-    here we assume that salt_EvPrRn = 0
C     set cutoff value to prevent too large additional EmP:
C       current limit is set to 0.1 CFL
        epsil = 1. _d -10
        cutoff = 0.1 _d 0 *drF(ks)/PTRACERS_dTLev(ks)
        IF ( ( (nonlinFreeSurf.GT.0 .OR. usingPCoords)
     &         .AND. useRealFreshWaterFlux )
     &     .OR.convertFW2Salt .EQ. -1. ) THEN
         DO j = jMin, jMax
          DO i = iMin, iMax
            tmpVar = MAX( salt(i,j,ks,bi,bj), epsil )
            add2EmP(i,j) = relaxForcingS(i,j)/tmpVar
            add2EmP(i,j) = rUnit2mass
     &                  *MAX( -cutoff, MIN( add2EmP(i,j), cutoff ) )
          ENDDO
         ENDDO
        ELSE
         DO j = jMin, jMax
          DO i = iMin, iMax
            add2EmP(i,j) = relaxForcingS(i,j)/convertFW2Salt
            add2EmP(i,j) = rUnit2mass
     &                  *MAX( -cutoff, MIN( add2EmP(i,j), cutoff ) )
          ENDDO
         ENDDO
        ENDIF
#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         CALL DIAGNOSTICS_FILL(add2EmP,'Add2EmP ',0,1,2,bi,bj,myThid)
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */
      ELSE
        DO j = jMin, jMax
          DO i = iMin, iMax
            add2EmP(i,j) = 0. _d 0
          ENDDO
        ENDDO
      ENDIF
C-- end of "addEmP" setting

#ifdef EXACT_CONSERV
      IF ( (nonlinFreeSurf.GT.0 .OR. usingPCoords)
     &     .AND. useRealFreshWaterFlux ) THEN

       DO iTrc=1,PTRACERS_numInUse

c-  NonLin_FrSurf and RealFreshWaterFlux : PmEpR effectively changes
c   the water column height ; temp., salt, (tracer) flux associated
c   with this input/output of water is added here to the surface tendency.
c
         IF ( PTRACERS_StepFwd(iTrc) .AND.
     &        PTRACERS_EvPrRn(iTrc).NE.UNSET_RL ) THEN
          DO j = jMin, jMax
           DO i = iMin, iMax
             surfaceForcingPTr(i,j,bi,bj,iTrc) =
     &          surfaceForcingPTr(i,j,bi,bj,iTrc)
     &        + ( PmEpR(i,j,bi,bj) - add2EmP(i,j) )
     &          *( PTRACERS_EvPrRn(iTrc) - pTracer(i,j,ks,bi,bj,iTrc) )
     &          *mass2rUnit
           ENDDO
          ENDDO
         ENDIF

       ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ELSE
#else /* EXACT_CONSERV */
      IF (.TRUE.) THEN
#endif /* EXACT_CONSERV */

C--   EmPmR does not really affect the water column height (for tracer budget)
C     and is converted to a salt tendency.

       IF (convertFW2Salt .EQ. -1.) THEN
C-    use local surface tracer field to calculate forcing term:

        DO iTrc=1,PTRACERS_numInUse

         IF ( PTRACERS_StepFwd(iTrc) .AND.
     &        PTRACERS_EvPrRn(iTrc).NE.UNSET_RL ) THEN
C        account for Rain/Evap tracer content (PTRACERS_EvPrRn) using
C        local surface tracer
          DO j = jMin, jMax
           DO i = iMin, iMax
            surfaceForcingPTr(i,j,bi,bj,iTrc) =
     &          surfaceForcingPTr(i,j,bi,bj,iTrc)
     &        + ( EmPmR(i,j,bi,bj) + add2EmP(i,j) )
     &          *( pTracer(i,j,ks,bi,bj,iTrc) - PTRACERS_EvPrRn(iTrc) )
     &          *mass2rUnit
           ENDDO
          ENDDO
         ENDIF

        ENDDO

       ELSE
C-    use uniform tracer value to calculate forcing term:

        DO iTrc=1,PTRACERS_numInUse

         IF ( PTRACERS_StepFwd(iTrc) .AND.
     &        PTRACERS_EvPrRn(iTrc).NE.UNSET_RL ) THEN
C     account for Rain/Evap tracer content (PTRACERS_EvPrRn) assuming uniform
C     surface tracer (=PTRACERS_ref)
          DO j = jMin, jMax
           DO i = iMin, iMax
            surfaceForcingPTr(i,j,bi,bj,iTrc) =
     &          surfaceForcingPTr(i,j,bi,bj,iTrc)
     &        + ( EmPmR(i,j,bi,bj) + add2EmP(i,j) )
     &            *( PTRACERS_ref(ks,iTrc) - PTRACERS_EvPrRn(iTrc) )
     &            *mass2rUnit
           ENDDO
          ENDDO
         ENDIF

        ENDDO

C-    end local-surface-tracer / uniform-value distinction
       ENDIF

      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_PTRACERS */

      RETURN
      END
