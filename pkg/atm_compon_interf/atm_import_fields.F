C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/atm_import_fields.F,v 1.1 2004/05/21 19:59:38 jmc Exp $
C $Name:  $  

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ATM_IMPORT_FIELDS( myThid )
C     /==========================================================\
C     | SUBROUTINE ATM_IMPORT_SST                                |
C     | o Routine for importing ocean surface coupling fields    |
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

#ifdef COMPONENT_MODULE
C     == Local variables ==

C     Fetch data from coupling layer. Note MIT Coupler checkpoint1
C     does not allow asynchronous extraction of data, so ordering
C     has to be consistent with ordering coupling layer.

C-    Receive ocean mixed-layer thickness from coupling layer.
      CALL COMPRECV_R8TILES( atmMxlDName,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy, ocMxlD )

C-    Receive ocean surface temperatures from coupling layer.
      CALL COMPRECV_R8TILES( atmSSTName,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy, SSTocn )

C-    Receive ocean surface salinity from coupling layer.
      CALL COMPRECV_R8TILES( atmSSSName,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy, SSSocn )

C-    Receive ocean surface velocity square from coupling layer.
      CALL COMPRECV_R8TILES( atmSSVsqName,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy, vSqocn )

#endif /* COMPONENT_MODULE */

      RETURN
      END
