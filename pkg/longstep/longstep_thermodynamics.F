#include "LONGSTEP_OPTIONS.h"

CBOP
C     !ROUTINE: LONGSTEP_THERMODYNAMICS
C     !INTERFACE:
      SUBROUTINE LONGSTEP_THERMODYNAMICS( myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE LONGSTEP_THERMODYNAMICS
C     | o Controlling routine for the prognostics of passive tracers
C     |   with longer time step.
C     *===========================================================
C     | This is a copy of THERMODYNAMICS, but only with the
C     | parts relevant to ptracers, and dynamics fields replaced
C     | by their longstep averages.
C     | When THERMODYNAMICS is changed, this routine probably has
C     | to be changed too :(
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "RESTART.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SURFACE.h"
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD.h"
#endif
#include "LONGSTEP_PARAMS.h"
#include "LONGSTEP.h"
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "PTRACERS_PARAMS.h"
# include "PTRACERS_FIELDS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_LONGSTEP
C     !LOCAL VARIABLES:
C     == Local variables
C     uFld,vFld,wFld :: Local copy of velocity field (3 components)
C     kappaRk        :: Total diffusion in vertical, all levels, 1 tracer
C     useVariableK   :: T when vertical diffusion is not constant
C     iMin, iMax     :: Ranges and sub-block indices on which calculations
C     jMin, jMax        are applied.
C     bi, bj         :: Tile indices
C     i, j, k        :: loop indices
      _RL uFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL vFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL wFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL kappaRk (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RS recip_hFacNew(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      INTEGER bi, bj
      INTEGER i, j, k
CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('LONGSTEP_THERMODYNAMICS',myThid)
#endif

C     time for a ptracer time step?
      IF ( LS_doTimeStep ) THEN

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C--   Set up work arrays with valid (i.e. not NaN) values
C     These inital values do not alter the numerical results. They
C     just ensure that all memory references are to valid floating
C     point numbers. This prevents spurious hardware signals due to
C     uninitialised but inert locations.

        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
C This is currently also used by IVDC and Diagnostics
           kappaRk(i,j,k)    = 0. _d 0
          ENDDO
         ENDDO
        ENDDO

C--     Compute new reciprocal hFac for implicit calculation
#ifdef NONLIN_FRSURF
        IF ( nonlinFreeSurf.GT.0 ) THEN
         IF ( select_rStar.GT.0 ) THEN
# ifndef DISABLE_RSTAR_CODE
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             recip_hFacNew(i,j,k) = recip_hFacC(i,j,k,bi,bj)
     &                            / rStarExpC(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDDO
# endif /* DISABLE_RSTAR_CODE */
         ELSEIF ( selectSigmaCoord.NE.0 ) THEN
# ifndef DISABLE_SIGMA_CODE
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             recip_hFacNew(i,j,k) = recip_hFacC(i,j,k,bi,bj)
     &        /( 1. _d 0 + dEtaHdt(i,j,bi,bj)*deltaTFreeSurf
     &                    *dBHybSigF(k)*recip_drF(k)
     &                    *recip_hFacC(i,j,k,bi,bj)
     &         )
            ENDDO
           ENDDO
          ENDDO
# endif /* DISABLE_RSTAR_CODE */
         ELSE
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             IF ( k.EQ.kSurfC(i,j,bi,bj) ) THEN
              recip_hFacNew(i,j,k) = 1. _d 0 / hFac_surfC(i,j,bi,bj)
             ELSE
              recip_hFacNew(i,j,k) = recip_hFacC(i,j,k,bi,bj)
             ENDIF
            ENDDO
           ENDDO
          ENDDO
         ENDIF
        ELSE
#endif /* NONLIN_FRSURF */
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             recip_hFacNew(i,j,k) = _recip_hFacC(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
#ifdef NONLIN_FRSURF
        ENDIF
#endif /* NONLIN_FRSURF */

        iMin = 1-OLx
        iMax = sNx+OLx
        jMin = 1-OLy
        jMax = sNy+OLy

C     need to recompute surfaceForcingPtr using LS_fwFlux
        CALL LONGSTEP_FORCING_SURF(
     I        bi, bj, iMin, iMax, jMin, jMax,
     I        myTime,myIter,myThid )

C--   Set up 3-D velocity field that we use to advect tracers:
C-    just do a local copy:
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uFld(i,j,k) = LS_uVel(i,j,k,bi,bj)
           vFld(i,j,k) = LS_vVel(i,j,k,bi,bj)
           wFld(i,j,k) = LS_wVel(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
#ifdef ALLOW_GMREDI
C-    add Bolus velocity to Eulerian-mean velocity:
        IF (useGMRedi) THEN
          CALL  LONGSTEP_RESIDUAL_FLOW(
     U                  uFld, vFld, wFld,
     I                  bi, bj, myIter, myThid )
        ENDIF
#endif /* ALLOW_GMREDI */

#ifdef ALLOW_PTRACERS
C--     Calculate passive tracer tendencies
C       and step forward, storing result in gPtr
C       Also apply open boundary conditions for each passive tracer
        IF ( usePTRACERS ) THEN
#ifdef ALLOW_DEBUG
           IF (debugMode) CALL DEBUG_CALL('PTRACERS_INTEGRATE',myThid)
#endif
           CALL PTRACERS_INTEGRATE(
     I          bi, bj, recip_hFacNew,
     I          uFld, vFld, wFld,
     U          kappaRk,
     I          myTime, myIter, myThid )
        ENDIF
#endif /* ALLOW_PTRACERS */

C--   end bi,bj loops.
       ENDDO
      ENDDO

#ifdef ALLOW_DEBUG
      IF ( debugLevel.GE.debLevD ) THEN
       CALL DEBUG_STATS_RL(Nr,LS_uVel,'LS_Uvel (THERMODYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,LS_vVel,'LS_Vvel (THERMODYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,LS_wVel,'LS_Wvel (THERMODYNAMICS)',myThid)
       CALL DEBUG_STATS_RL(Nr,LS_theta,'LS_Theta (THERMODYNAMICS)',
     &                     myThid)
       CALL DEBUG_STATS_RL(Nr,LS_salt,'LS_Salt (THERMODYNAMICS)',myThid)
#ifdef ALLOW_PTRACERS
       IF ( usePTRACERS ) THEN
         CALL PTRACERS_DEBUG(myThid)
       ENDIF
#endif /* ALLOW_PTRACERS */
      ENDIF
#endif /* ALLOW_DEBUG */

C     LS_doTimeStep
      ENDIF

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('LONGSTEP_THERMODYNAMICS',myThid)
#endif

#endif /* ALLOW_LONGSTEP */

      RETURN
      END
