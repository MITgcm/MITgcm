#include "TAPENADE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: DUMMY_IN_STEPPING_TAP
C     !INTERFACE:
      SUBROUTINE DUMMY_IN_STEPPING_TAP( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DUMMY_IN_STEPPING_TAP
C     *==========================================================*
C     Tapenade counterpart of the DUMMY_IN_STEPPING hook, called
C     beside it from FORWARD_STEP when the tapenade package is
C     enabled. A no-op in the forward model, like the TAF hook.
C
C     TAF forces its hand-written adjoint of DUMMY_IN_STEPPING into
C     the reverse sweep with a flow directive. Tapenade has no such
C     directive, so this routine is differentiated by Tapenade (it
C     is listed in tapenade_ad_diff.list) and consists of one call
C     per field to an external hook whose field argument
C     tools/TAP_support/flow_tap declares read-then-written.
C     Tapenade therefore generates, in the adjoint of this routine,
C     a call to the hand-written adjoint of each hook
C     (pkg/tapenade/dummy_tap.F), with the adjoint of the field as
C     an extra argument; that adjoint folds the tile halos and
C     writes the ADJ* output exactly as ADDUMMY_IN_STEPPING does.
C     The tangent-linear build gets the G_J* output of
C     G_DUMMY_IN_STEPPING the same way.
C
C     One field per call keeps the argument list of every generated
C     call fixed: Tapenade generates the derivative call of an
C     external only when one of its inputs is active, and a hook
C     carrying several fields, some of which exist or are active
C     only in some configurations, would be called with a
C     configuration-dependent argument list. With one field per
C     call, a field that is passive in a given configuration loses
C     its call entirely rather than an argument in a shared list.
C     The field set and its guards mirror
C     ADDUMMY_IN_STEPPING (pkg/autodiff/addummy_in_stepping.F); the
C     package-specific dumps of that routine (seaice, ptracers,
C     shelfice, ggl90, ...) are not included. Adding a field is one
C     guarded call here and, for a new field shape, one hook family
C     in dummy_tap.F. Tapenade stores each field once per time step
C     around its hook call, whether this routine is checkpointed
C     (the default) or differentiated in split mode.
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "FFIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myTime :: time counter for this thread
C     myIter :: iteration counter for this thread
C     myThid :: Thread number for this instance of the routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_AUTODIFF_MONITOR

C-----------------------------------------------------------------------
C--- State variables
C-----------------------------------------------------------------------
      CALL DUMMY_IN_STEPPING_XYZ_RL( theta, 'ADJtheta', 'ADJtheta.',
     &                               myTime, myIter, myThid )
      CALL DUMMY_IN_STEPPING_XYZ_RL( salt,  'ADJsalt ', 'ADJsalt.',
     &                               myTime, myIter, myThid )
      CALL DUMMY_IN_STEPPING_XYZ_RL( wVel,  'ADJwvel ', 'ADJwvel.',
     &                               myTime, myIter, myThid )
      CALL DUMMY_IN_STEPPING_UV_XYZ_RL( uVel, vVel,
     &                               'ADJuvel ', 'ADJuvel.',
     &                               'ADJvvel ', 'ADJvvel.',
     &                               myTime, myIter, myThid )

C-----------------------------------------------------------------------
C--- Surface forcing
C-----------------------------------------------------------------------
      IF ( .NOT. useSEAICE .AND. .NOT. useEXF ) THEN
        CALL DUMMY_IN_STEPPING_UV_XY_RS( fu, fv,
     &                               'ADJtaux ', 'ADJtaux.',
     &                               'ADJtauy ', 'ADJtauy.',
     &                               myTime, myIter, myThid )
        CALL DUMMY_IN_STEPPING_XY_RS( Qnet,  'ADJqnet ', 'ADJqnet.',
     &                               myTime, myIter, myThid )
        CALL DUMMY_IN_STEPPING_XY_RS( EmPmR, 'ADJempmr', 'ADJempr.',
     &                               myTime, myIter, myThid )
# ifdef SHORTWAVE_HEATING
        CALL DUMMY_IN_STEPPING_XY_RS( Qsw,   'ADJqsw  ', 'ADJqsw.',
     &                               myTime, myIter, myThid )
# endif
      ENDIF

C-----------------------------------------------------------------------
C--- Parameters
C-----------------------------------------------------------------------
# ifdef ALLOW_DIFFKR_CONTROL
      CALL DUMMY_IN_STEPPING_XYZ_RL( diffKr, 'ADJdifkr', 'ADJdiffkr.',
     &                               myTime, myIter, myThid )
# endif

#endif /* ALLOW_AUTODIFF_MONITOR */

      RETURN
      END
