C $Header: /u/gcmpack/MITgcm/pkg/aim_compon_interf/Attic/atm_export_atmconfig.F,v 1.1 2003/12/15 02:44:48 jmc Exp $
C $Name:  $  

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_EXPORT_ATMCONFIG( myThid )
C     /==========================================================\
C     | SUBROUTINE ATM_EXPORT_ATMCONFIG                          |
C     | o Routine for exporting atmos. config to coupling level. |
C     |==========================================================|
C     | This version talks to the MIT Coupler. It uses the MIT   |
C     | Coupler "checkpoint1" library calls.                     |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "ATMIDS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine
      INTEGER myThid
CEndOfInterface

C     == Local variables ==

C     Send my configuration information to the coupler
C     o Send atmos. model orography map
      CALL COMPSEND_R8TILES( atmDepthName,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy, Ro_surf )

      RETURN
      END
