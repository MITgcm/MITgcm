C $Header: /u/gcmpack/MITgcm/model/src/do_stagger_fields_exchanges.F,v 1.3 2004/04/22 21:38:52 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: DO_STAGGER_FIELDS_EXCHANGES
C     !INTERFACE:
      SUBROUTINE DO_STAGGER_FIELDS_EXCHANGES(myTime, myIter, myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DO_STAGGER_FIELDS_EXCHANGES
C     | o Exchange edge info of Active tracers fields (T,S)
C     |   (needed when using stagger time step)
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number in simulation
C     myThid  :: Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid   
CEOP

      IF ( staggerTimeStep ) THEN
        _EXCH_XYZ_R8( gT , myThid )
        _EXCH_XYZ_R8( gS , myThid )
c       _EXCH_XYZ_R8( theta , myThid )
c       _EXCH_XYZ_R8( salt , myThid )
      ENDIF

#ifdef ALLOW_AIM
      IF ( useAIM ) THEN
       CALL AIM_AIM2DYN_EXCHANGES( myTime, myIter, myThid )
      ENDIF
#endif /* ALLOW_AIM */

      RETURN
      END
