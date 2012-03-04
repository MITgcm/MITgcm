C $Header: /u/gcmpack/MITgcm/pkg/frazil/Attic/frazil_diagnostics_state.F,v 1.3 2012/03/04 23:11:20 dimitri Exp $
C $Name:  $

#include "FRAZIL_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: FRAZIL_DIAGNOSTICS_STATE

C !INTERFACE:
      SUBROUTINE FRAZIL_DIAGNOSTICS_STATE(
     I                     myTime, myIter, myThid )

C     !DESCRIPTION:
C     Fill-in diagnostics array for FRAZIL state variables

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_FRAZIL
# include "FRAZIL.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myTime :: current time of simulation
C     myIter :: current iteration number in simulation
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_FRAZIL

# ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
         CALL DIAGNOSTICS_FILL( FrazilForcingT, 'FrzForcT',
     &                          0,Nr, 0, 1, 1, myThid )
      ENDIF
# endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_FRAZIL */

      RETURN
      END
