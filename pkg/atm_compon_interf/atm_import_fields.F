C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/atm_import_fields.F,v 1.4 2015/11/12 01:03:32 jmc Exp $
C $Name:  $

#include "ATM_CPL_OPTIONS.h"

CBOP
C     !ROUTINE: ATM_IMPORT_FIELDS
C     !INTERFACE:
      SUBROUTINE ATM_IMPORT_FIELDS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE ATM_IMPORT_FIELDS
C     | o Routine for importing ocean surface coupling fields
C     |   from coupling layer.
C     *==========================================================*
C     | This version talks to the MIT Coupler. It uses the MIT
C     | Coupler "checkpoint1" library calls.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "CPL_PARAMS.h"
#include "ATMCPL.h"
#include "ATMIDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: my Thread Id number
      INTEGER myThid
CEOP

#ifdef COMPONENT_MODULE
C     !LOCAL VARIABLES:
C     == Local variables ==

C     Fetch data from coupling layer. Note MIT Coupler checkpoint1
C     does not allow asynchronous extraction of data, so ordering
C     has to be consistent with ordering coupling layer.

      _BARRIER
      _BEGIN_MASTER( myThid )

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

C-    Receive air-sea flux of CO2
      IF ( atm_cplExch_DIC ) THEN
        CALL COMPRECV_R8TILES( atmCO2FluxName,
     I       sNx, OLx, sNy, OLy, 1, nSx, nSy, flxCO2ocn )
      ENDIF

      _END_MASTER( myThid )
      _BARRIER

#endif /* COMPONENT_MODULE */

      RETURN
      END
