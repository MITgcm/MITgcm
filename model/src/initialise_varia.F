C $Header: /u/gcmpack/MITgcm/model/src/initialise_varia.F,v 1.52 2007/01/09 20:08:03 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INITIALISE_VARIA
C     !INTERFACE:
      SUBROUTINE INITIALISE_VARIA(myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INITIALISE_VARIA                               
C     | o Set the initial conditions for dynamics variables       
C     |   and time dependent arrays                               
C     *==========================================================*
C     | This routine reads/writes  data from an input file and    
C     | from various binary files.                                
C     | Each thread invokes an instance of this routine as does   
C     | each process in a multi-process parallel environment like 
C     | MPI.                                                      
C     *==========================================================*
C     \ev

C     !CALLING SEQUENCE:
C     INITIALISE_VARIA
C       |
C #ifdef ALLOW_AUTODIFF_TAMC
C       |-- INI_LINEAR_PHISURF
C       |
C       |-- INI_CORI
C       |
C       |-- INI_CG2D
C       |
C       |-- INI_CG3D
C #endif
C       |-- INI_MIXING
C       |
C       |-- INI_DYNVARS
C       |-- INI_NH_VARS
C       |
C       |-- INI_FIELDS
C       |
C       |-- INI_AUTODIFF
C       |
C       |-- PACKAGES_INIT_VARIABLES
C       |
C       |-- THE_CORRECTION_STEP (if restart from old pickup files)
C       |
C       |-- CALL CONVECTIVE_ADJUSTMENT_INI
C       |
C       |-- CALC_SURF_DR
C       |
C       |-- UPDATE_SURF_DR
C       |
C       |-- UPDATE_CG2D
C       |
C       |-- INTEGR_CONTINUITY
C       |
C       |-- TIMEAVE_STATVARS
C       |
C       |-- PTRACERS_STATVARS
C       |
C       |-- STATE_SUMMARY

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "SURFACE.h"
#ifdef ALLOW_SHAP_FILT
# include "SHAP_FILT.h"
#endif
#ifdef ALLOW_ZONAL_FILT
# include "ZONAL_FILT.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj,iMin,iMax,jMin,jMax
CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('INITIALISE_VARIA',myThid)
#endif

#ifdef ALLOW_AUTODIFF_TAMC

      nIter0 = NINT( (startTime-baseTime)/deltaTClock )

C--   Intialize the hFacC for TAF/TAMC 
      CALL INI_HFAC( myThid )
      CALL INI_DEPTHS( myThid )
      CALL INI_MASKS_ETC( myThid )

C--   Set Bo_surf => define the Linear Relation: Phi_surf(eta)
# ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_LINEAR_PHISURF',myThid)
# endif
      CALL INI_LINEAR_PHISURF( myThid )

C--   Set coriolis operators
# ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_CORI',myThid)
# endif
      CALL INI_CORI( myThid )

C--   Set laplace operators for use in 2D conjugate gradient solver.
# ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_CG2D',myThid)
# endif
      CALL INI_CG2D( myThid )

#ifdef ALLOW_DEPTH_CONTROL
C--   get control parameter depth
      CALL CTRL_DEPTH_INI( myThid )
C--   Re-calculate hFacS/W and some other parameters from hFacC
      CALL UPDATE_MASKS_ETC( myThid )
C--   Update laplace operators for use in 2D conjugate gradient solver.
      CALL UPDATE_CG2D( startTime, nIter0, myThid )
CML      CALL UPDATE_CG2D( myThid )
#endif /* ALLOW_DEPTH_CONTROL */

# ifdef ALLOW_NONHYDROSTATIC
C--   Set laplace operators for use in 3D conjugate gradient solver.
ceh3 should add an IF ( useNONHYDROSTATIC ) THEN
#  ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_CG3D',myThid)
#  endif
      CALL INI_CG3D( myThid )
# endif

#endif /* ALLOW_AUTODIFF_TAMC */
      _BARRIER

C--   Initialise 3-dim. diffusivities
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_MIXING',myThid)
#endif
      CALL INI_MIXING( myThid )
      _BARRIER

#ifdef ALLOW_TAU_EDDY
C--  Initialise eddy diffusivities
      CALL TAUEDDY_INIT_VARIA( myThid )
#endif

C--   Initialize DYNVARS arrays (state fields + G terms: Gu,Gv,...) to zero [always]
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_DYNVARS',myThid)
#endif
      CALL INI_DYNVARS( myThid )

C--   Initialize NH_VARS arrays to zero [always]
#ifdef ALLOW_NONHYDROSTATIC
      CALL INI_NH_VARS( myThid )
#endif

C--   Initialise model fields.
C     Starting values of U, V, W, temp., salt. and tendency terms
C     are set here. Fields are either set to default or read from
C     stored files.
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INI_FIELDS',myThid)
#endif
      CALL INI_FIELDS( myThid )
      _BARRIER

#ifdef ALLOW_AUTODIFF_TAMC
C--   Initialise active fields to help TAMC
      CALL INI_AUTODIFF( myThid )
      _BARRIER
#endif

C--   Initialize variable data for packages
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('PACKAGES_INIT_VARIABLES',myThid)
#endif
      CALL PACKAGES_INIT_VARIABLES( myThid )

c#ifndef ALLOW_AUTODIFF_TAMC
c     IF ( usePickupBeforeC35 .AND. startTime .NE. baseTime ) THEN
C-- IMPORTANT : Need to activate the following call to restart from a pickup
C     file written by MITgcmUV_checkpoint34 (Feb-08, 2001) or earlier.
C-  Disable this option on Jan-09, 2007.
c      CALL THE_CORRECTION_STEP(startTime, nIter0, myThid)
c     ENDIF
c#endif

C--   Initial conditions are convectively adjusted (for historical reasons)
      IF ( startTime .EQ. baseTime .AND. cAdjFreq .NE. 0. ) THEN
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('CONVECTIVE_ADJUSTMENT_INI',myThid)
#endif
CADJ loop = parallel 
        DO bj = myByLo(myThid), myByHi(myThid)
CADJ loop = parallel
         DO bi = myBxLo(myThid), myBxHi(myThid)
           iMin=1-Olx
           iMax=sNx+Olx
           jMin=1-Oly
           jMax=sNy+Oly
           CALL CONVECTIVE_ADJUSTMENT_INI(
     I       bi, bj, iMin, iMax, jMin, jMax,
     I       startTime, nIter0, myThid )
         ENDDO
        ENDDO
        _BARRIER
      ENDIF

#ifdef NONLIN_FRSURF
C--   Compute the surface level thickness <-- function of etaH(n)
C     and modify hFac(C,W,S) accordingly :
      CALL INI_SURF_DR( myThid )
      CALL INI_R_STAR( myThid )
# ifndef DISABLE_RSTAR_CODE
      IF ( select_rStar.NE.0 )
     &  CALL CALC_R_STAR(etaH, startTime, -1 , myThid )
# endif /* DISABLE_RSTAR_CODE */
      IF (nonlinFreeSurf.GT.0) THEN
       IF ( select_rStar.GT.0 ) THEN
# ifndef DISABLE_RSTAR_CODE
        CALL UPDATE_R_STAR( startTime, nIter0, myThid )
# endif /* DISABLE_RSTAR_CODE */
       ELSE
        CALL CALC_SURF_DR(etaH, startTime, -1 , myThid )
        CALL UPDATE_SURF_DR( startTime, nIter0, myThid )
       ENDIF
      ENDIF
C-    update also CG2D matrix (and preconditioner)
      IF ( nonlinFreeSurf.GT.2) THEN
        CALL UPDATE_CG2D( startTime, nIter0, myThid )
      ENDIF
#endif /* NONLIN_FRSURF */

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('INTEGR_CONTINUITY',myThid)
#endif
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C--   Integrate continuity vertically for vertical velocity
        CALL INTEGR_CONTINUITY( bi, bj, uVel, vVel,
     I                          startTime, nIter0, myThid )

       ENDDO
      ENDDO

#ifdef EXACT_CONSERV
      IF ( exactConserv ) THEN
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('UPDATE_ETAH',myThid)
#endif
C--   Update etaH(n) :
         CALL UPDATE_ETAH( startTime, nIter0, myThid )
      ENDIF
#endif /* EXACT_CONSERV */

#ifdef NONLIN_FRSURF
      IF ( select_rStar.NE.0 ) THEN
#ifndef ALLOW_AUTODIFF_TAMC
C--   r* : compute the future level thickness according to etaH(n+1)
          CALL CALC_R_STAR(etaH, startTime, nIter0, myThid )
#endif
      ELSEIF ( nonlinFreeSurf.GT.0) THEN
C--   compute the future surface level thickness according to etaH(n+1)
          CALL CALC_SURF_DR(etaH, startTime, nIter0, myThid )
      ENDIF
#endif /* NONLIN_FRSURF */

c     IF ( nIter0.EQ.0 .AND. staggerTimeStep ) THEN
C--    Filter initial T & S fields if staggerTimeStep 
C       (only for backward compatibility ; to be removed later)
#ifdef ALLOW_SHAP_FILT
c      IF ( useSHAP_FILT .AND. shap_filt_TrStagg ) THEN
c       CALL SHAP_FILT_APPLY_TS(theta,salt,startTime,nIter0,myThid)
c      ENDIF
#endif
#ifdef ALLOW_ZONAL_FILT
c      IF ( useZONAL_FILT .AND. zonal_filt_TrStagg ) THEN
c       CALL ZONAL_FILT_APPLY_TS( theta, salt, myThid )
c      ENDIF
#endif
c     ENDIF

#ifdef ALLOW_TIMEAVE
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('TIMEAVE_STATVARS',myThid)
#endif
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C--   initialise time-average arrays with initial state values
        IF (taveFreq.GT.0.) THEN
           CALL TIMEAVE_STATVARS(startTime, nIter0, bi, bj, myThid)
        ENDIF

#ifdef ALLOW_PTRACERS
        IF ( usePTRACERS ) THEN
#ifdef ALLOW_DEBUG
          IF (debugMode) CALL DEBUG_CALL('PTRACERS_STATVARS',myThid)
#endif
           CALL PTRACERS_STATVARS(startTime, nIter0, bi, bj, myThid)
        ENDIF
#endif /* ALLOW_PTRACERS */

C--   end bi,bj loop.
       ENDDO
      ENDDO
#endif /* ALLOW_TIMEAVE */

#ifdef ALLOW_GRIDALT
        if (useGRIDALT) then
         CALL TIMER_START('GRIDALT_UPDATE  [INITIALISE_VARIA]',mythid)
         CALL GRIDALT_UPDATE(myThid)
         CALL TIMER_STOP ('GRIDALT_UPDATE  [INITIALISE_VARIA]',mythid)
        endif
#endif

C--   Fill in overlap regions for wVel :
      _EXCH_XYZ_R8(wVel,myThid)

C--   Finally summarise the model state
#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('STATE_SUMMARY',myThid)
#endif
      CALL STATE_SUMMARY( myThid )

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('INITIALISE_VARIA',myThid)
#endif

C--   Check barrier synchronization:
      CALL BAR_CHECK( 4, myThid )

      RETURN
      END
