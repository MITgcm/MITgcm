C $Header: /u/gcmpack/MITgcm/pkg/bbl/bbl_diagnostics_state.F,v 1.2 2011/08/07 07:08:15 dimitri Exp $
C $Name:  $

#include "BBL_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: BBL_DIAGNOSTICS_STATE

C !INTERFACE:
      SUBROUTINE BBL_DIAGNOSTICS_STATE(
     I                     myTime, myIter, myThid )

C     !DESCRIPTION:
C     Fill-in diagnostics array for BBL state variables

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "BBL.h"

C     !INPUT PARAMETERS:
C     myTime   :: Current time of simulation ( s )
C     myIter   :: Current iteration number in simulation
C     myThid   :: my Thread Id number
      _RL     myTime
      INTEGER myIter, myThid
CEOP

#ifdef ALLOW_BBL
C     !LOCAL VARIABLES:

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN

         CALL DIAGNOSTICS_FILL( bbl_theta    , 'BBLtheta',
     &                          0, 1, 0, 1, 1, myThid )
         CALL DIAGNOSTICS_FILL( bbl_salt     , 'BBLsalt ',
     &                          0, 1, 0, 1, 1, myThid )
         CALL DIAGNOSTICS_FILL( bbl_eta      , 'BBLeta  ',
     &                          0, 1, 0, 1, 1, myThid )
         CALL DIAGNOSTICS_FILL( bbl_TendTheta, 'BBLtendT',
     &                          0, 1, 0, 1, 1, myThid )
         CALL DIAGNOSTICS_FILL( bbl_TendSalt , 'BBLtendS',
     &                          0, 1, 0, 1, 1, myThid )

      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_BBL */

      RETURN
      END
