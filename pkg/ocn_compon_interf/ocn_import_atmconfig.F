C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/ocn_import_atmconfig.F,v 1.1 2003/12/15 02:49:09 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE OCN_IMPORT_ATMCONFIG( myThid )
C     /==========================================================\
C     | SUBROUTINE OCN_IMPORT_ATMCONFIG                          |
C     | o Routine for importing atmos. config into ocean         |
C     |   component.                                             |
C     |==========================================================|
C     | This version talks to the MIT Coupler. It uses the       |
C     | MIT Coupler "checkpoint 1" library calls.                |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "ATMIDS.h"
#include "OCNCPL.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine
      INTEGER myThid
CEndOfInterface

C     Receive atmos. model configuration info.
C     o Import atmosphere model orography
      CALL COMPRECV_R8TILES( atmDepthName,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O     Hatm )

      RETURN
      END
