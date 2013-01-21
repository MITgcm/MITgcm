C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_do_exch.F,v 1.1 2013/01/21 22:40:15 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_DO_EXCH
C     !INTERFACE:
      SUBROUTINE THSICE_DO_EXCH( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE THSICE_DO_EXCH
C     | o Apply Exchanges to THSICE variables when necessary
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE
C     !LOCAL VARIABLES:

      CALL EXCH_XY_RL( iceMask, myThid )
      IF ( useSEAICE .OR. thSIceAdvScheme.GT.0 ) THEN
C--   Exchange fields that are advected by seaice dynamics
        CALL EXCH_XY_RL( iceHeight, myThid )
        CALL EXCH_XY_RL( snowHeight, myThid )
        CALL EXCH_XY_RL( Qice1, myThid )
        CALL EXCH_XY_RL( Qice2, myThid )
      ENDIF

#endif /* ALLOW_THSICE */

      RETURN
      END
