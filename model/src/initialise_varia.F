C $Header: /u/gcmpack/MITgcm/model/src/initialise_varia.F,v 1.20 2001/09/26 18:09:15 cnh Exp $
C $Name:  $

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
C       |-- INI_LINEAR_PHISURF
C       |
C       |-- INI_CORI
C       |
C       |-- INI_CG2D
C       |
C       |-- INI_CG3D
C       |
C       |-- INI_MIXING
C       |
C       |-- INI_DYNVARS
C       |
C       |-- INI_FIELDS
C       |
C       |-- INI_TR1
C       |
C       |-- THE_CORRECTION_STEP
C       |
C       |-- CALL CONVECTIVE_ADJUSTMENT_INI
C       |
C       |-- PACKAGES_INIT_VARIABLES
C       |
C       |-- CALC_SURF_DR
C       |
C       |-- UPDATE_SURF_DR
C       |
C       |-- UPDATE_CG2D
C       |
C       |-- STATE_SUMMARY
C       |
C       |-- TIMEAVE_STATVARS

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj,K,iMin,iMax,jMin,jMax
CEOP

#ifdef ALLOW_TAMC_CHECKPOINTING

      nIter0 = INT( startTime/deltaTClock )

C--   Set Bo_surf => define the Linear Relation: Phi_surf(eta)
      CALL INI_LINEAR_PHISURF( myThid )

C--   Set coriolis operators
      CALL INI_CORI( myThid )

C--   Set laplace operators for use in 2D conjugate gradient solver.
      CALL INI_CG2D( myThid )

#ifdef ALLOW_NONHYDROSTATIC
C--   Set laplace operators for use in 3D conjugate gradient solver.
      CALL INI_CG3D( myThid )
#endif

#endif /* ALLOW_TAMC_CHECKPOINTING */
      _BARRIER

C--   Initialise 3-dim. diffusivities
      CALL INI_MIXING( myThid )
      _BARRIER

C--   Initialize DYNVARS arrays (state fields + G terms: Gu,Gv,...) to zero [always]
      CALL INI_DYNVARS( myThid )

C--   Initialise model fields.
C     Starting values of U, V, W, temp., salt. and tendency terms
C     are set here. Fields are either set to default or read from
C     stored files.
      CALL INI_FIELDS( myThid )
      _BARRIER

#ifdef ALLOW_PASSIVE_TRACER
C--   Initialise passive tracer(s)
      CALL INI_TR1( myThid )
      _BARRIER
#endif

#ifndef ALLOW_AUTODIFF_TAMC
      IF ( usePickupBeforeC35 ) THEN
C-- IMPORTANT : Need to activate the following call to restart from 
C     a pickup file written by MITgcmUV_checkpoint34 or earlier.
      IF ( startTime .NE. 0. ) THEN
       CALL THE_CORRECTION_STEP(startTime, nIter0, myThid)
      ENDIF
      ENDIF
#endif

C--   Initial conditions are convectively adjusted (for historical reasons)
      IF ( startTime .EQ. 0. ) THEN
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
      END IF

C--   Initialize variable data for packages
      CALL PACKAGES_INIT_VARIABLES( myThid )

#ifdef NONLIN_FRSURF
C--   Compute the surface level thickness <-- function of etaH(n)
C     and modify hFac(C,W,S) accordingly :
      IF (nonlinFreeSurf.GT.0) THEN
        CALL CALC_SURF_DR(etaH, startTime, nIter0, myThid )
        CALL UPDATE_SURF_DR( startTime, nIter0, myThid )
      ENDIF
C-    update also CG2D matrix (and preconditioner)
      IF ( nonlinFreeSurf.GT.2) THEN
        CALL UPDATE_CG2D( startTime, nIter0, myThid )
      ENDIF
#endif

C--   Finally summarise the model state
      CALL STATE_SUMMARY( myThid )

#ifdef ALLOW_TIMEAVE
C--   initialise time-average arrays with initial state values
      IF (taveFreq.GT.0.) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
           CALL TIMEAVE_STATVARS(startTime, nIter0, bi, bj, myThid)
         ENDDO
        ENDDO
      ENDIF
#endif /* ALLOW_TIMEAVE */

      RETURN
      END
