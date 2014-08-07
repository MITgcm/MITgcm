C $Header: /u/gcmpack/MITgcm/pkg/longstep/longstep_correction_step.F,v 1.3 2014/08/07 17:37:32 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: LONGSTEP_CORRECTION_STEP
C     !INTERFACE:
      SUBROUTINE LONGSTEP_CORRECTION_STEP(myTime, myIter, myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE LONGSTEP_CORRECTION_STEP
C     *==========================================================*
C     | part of tracer_correction_step that affect ptracers
C     | (except convective adjustment): -> empty
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "LONGSTEP.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_GENERIC_ADVDIFF
C     !LOCAL VARIABLES:
C     == Local variables
c     INTEGER bi,bj
CEOP

#ifdef ALLOW_LONGSTEP
C     only cycle if we have computed gPtr in this time step
      IF ( LS_doTimeStep ) THEN
#else
      IF ( .TRUE. ) THEN
#endif

C     endif LS_doTimeStep
      ENDIF

#endif /* ALLOW_GENERIC_ADVDIFF */

      RETURN
      END
