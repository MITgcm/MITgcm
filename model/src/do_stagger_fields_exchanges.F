C $Header: /u/gcmpack/MITgcm/model/src/do_stagger_fields_exchanges.F,v 1.7 2009/04/28 18:01:14 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: DO_STAGGER_FIELDS_EXCHANGES
C     !INTERFACE:
      SUBROUTINE DO_STAGGER_FIELDS_EXCHANGES(myTime, myIter, myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DO_STAGGER_FIELDS_EXCHANGES
C     | o Exchange edge info of Active tracers fields (U,V)
C     |   (needed when using stagger time Step + multiDimAdvec)
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

      IF ( staggerTimeStep .AND. useMultiDimAdvec) THEN
        CALL EXCH_UV_XYZ_RL(uVel,vVel,.TRUE.,myThid)
        IF ( .NOT.implicitIntGravWave )
     &  _EXCH_XYZ_RL( wVel , myThid )
      ENDIF
c #ifdef ALLOW_NONHYDROSTATIC
      IF ( implicitIntGravWave ) THEN
        _EXCH_XYZ_RL( gT , myThid )
        _EXCH_XYZ_RL( gS , myThid )
      ENDIF
c #endif

      RETURN
      END
