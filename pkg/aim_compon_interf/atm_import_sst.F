C $Header: /u/gcmpack/MITgcm/pkg/aim_compon_interf/Attic/atm_import_sst.F,v 1.1 2003/12/15 02:44:48 jmc Exp $
C $Name:  $  

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_IMPORT_SST( myThid )
C     /==========================================================\
C     | SUBROUTINE ATM_IMPORT_SST                                |
C     | o Routine for importing ocean surface temperature        |
C     |   from coupling layer.                                   |
C     |==========================================================|
C     | This version talks to the MIT Coupler. It uses the MIT   |
C     | Coupler "checkpoint1" library calls.                     |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "ATMCPL.h"
#include "ATMIDS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine
      INTEGER myThid
CEndOfInterface

C     == Local variables ==

C     o Receive ocean surface temperatures from coupling layer.
      CALL COMPRECV_R8TILES( atmSSTName,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy, SSTocn )

      RETURN
      END
