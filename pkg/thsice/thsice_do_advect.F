C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_do_advect.F,v 1.1 2007/04/04 02:40:42 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_DO_ADVECT
C     !INTERFACE:
      SUBROUTINE THSICE_DO_ADVECT(
     I                     bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE THSICE_DO_ADVECT
C     | o wraper for pkg/thSIce advection-diffusion calls
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "THSICE_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     bi,bj     :: Tile indices
C     myTime    :: Current time in simulation (s)
C     myIter    :: Current iteration number
C     myThid    :: My Thread Id. number
      INTEGER bi,bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===
C     uIce/vIce :: ice velocity on C-grid [m/s]
      _RL  uIce(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  vIce(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      IF ( thSIceAdvScheme.GT.0 ) THEN
         CALL THSICE_GET_VELOCITY(
     O                        uIce, vIce,
     I                        bi,bj, myTime, myIter, myThid )
         CALL THSICE_ADVDIFF(
     U                        uIce, vIce,
     I                        bi,bj, myTime, myIter, myThid )
      ENDIF

      RETURN
      END
