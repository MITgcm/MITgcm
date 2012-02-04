C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/salt_plume_do_exch.F,v 1.1 2012/02/04 14:50:24 jmc Exp $
C $Name:  $

#include "SALT_PLUME_OPTIONS.h"

CBOP
C     !ROUTINE: SALT_PLUME_DO_EXCH
C     !INTERFACE:
      SUBROUTINE SALT_PLUME_DO_EXCH(
     I                           myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SALT_PLUME_DO_EXCH
C     | o Apply Exchanges to SALT_PLUME variables when necessary
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SALT_PLUME.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: my Thread Id. number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:

      IF ( useSALT_PLUME ) THEN
       _EXCH_XY_RL( saltPlumeFlux, myThid )
      ENDIF

      RETURN
      END
