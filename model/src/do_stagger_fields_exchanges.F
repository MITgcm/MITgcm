C $Header: /u/gcmpack/MITgcm/model/src/do_stagger_fields_exchanges.F,v 1.11 2014/08/07 17:32:03 jmc Exp $
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

      IF ( .NOT.useOffLine ) THEN
C-    Apply Exchanges on dynamics state variable, except in Off-Line mode

C- note: use less common EXCH S/R here (instead of usual ones) to
C        force TAF to also generate AD version for these rare ones
      IF ( staggerTimeStep ) THEN
c       CALL EXCH_UV_XYZ_RL( uVel,vVel, .TRUE., myThid )
        CALL EXCH_UV_3D_RL( uVel, vVel, .TRUE., Nr, myThid )
c       IF ( .NOT.implicitIntGravWave )
c    &  _EXCH_XYZ_RL( wVel, myThid )
        IF ( .NOT.implicitIntGravWave )
     &   CALL EXCH_3D_RL( wVel, Nr, myThid )
      ENDIF
c #ifdef ALLOW_NONHYDROSTATIC
      IF ( implicitIntGravWave ) THEN
c       _EXCH_XYZ_RL( theta, myThid )
c       _EXCH_XYZ_RL( salt , myThid )
        CALL EXCH_SM_3D_RL( theta, .FALSE., Nr, myThid )
        CALL EXCH_SM_3D_RL( salt , .FALSE., Nr, myThid )
      ENDIF
c #endif

C-    if not useOffLine: end
      ENDIF

      RETURN
      END
