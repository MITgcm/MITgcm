C $Header: /u/gcmpack/MITgcm/pkg/aim_compon_interf/Attic/atm_import_ocnconfig.F,v 1.1 2003/12/15 02:44:48 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_IMPORT_OCNCONFIG( myThid )
C     /==========================================================\
C     | SUBROUTINE ATM_IMPORT_OCNCONFIG                          |
C     | o Routine for importing ocean config into atmos.         |
C     |   component.                                             |
C     |==========================================================|
C     | This version talks to the MIT Coupler. It uses the       |
C     | MIT Coupler "checkpoint 1" library calls.                |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "OCNIDS.h"
#include "ATMCPL.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine
      INTEGER myThid
CEndOfInterface

C     Receive ocean model configuration info.
C     o Import ocean model depths
      CALL COMPRECV_R8TILES( ocnBathyName,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O     Hocn )

      RETURN
      END
