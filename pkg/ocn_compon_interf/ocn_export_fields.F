C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/ocn_export_fields.F,v 1.3 2009/12/25 19:45:13 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP 0
C !ROUTINE: OCN_EXPORT_FIELDS

C !INTERFACE:
      SUBROUTINE OCN_EXPORT_FIELDS( myThid )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE OCN_EXPORT_FIELDS
C     | o Routine for exporting oceanic fields
C     |   to the coupling layer.
C     *==========================================================*
C     | This version talks to the MIT Coupler. It uses the MIT
C     | Coupler "checkpoint1" library calls.
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "OCNIDS.h"
#include "OCNCPL.h"

C !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: Thread number for this instance of the routine
      INTEGER myThid
CEOP

#ifdef COMPONENT_MODULE
C !LOCAL VARIABLES:
C     == Local variables ==

C-    Send Oceanic fields to coupling layer
C  Note: 1) Information is sent on the ocean model grid.
C        2) MIT Coupler checkpoint1 does not allow asynchronous posting of
C          data, so ordering has to be consistent with coupling layer ordering

      _BARRIER
      _BEGIN_MASTER( myThid )

C-    Send Ocean mixed-layer depth to coupling layer
      CALL COMPSEND_R8TILES( ocnMxlDName,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy, ocMxlD2cpl )

C-    Send sea-surface temperature to coupling layer
      CALL COMPSEND_R8TILES( ocnSSTName,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy, SSTocn2cpl )

C-    Send sea-surface salinity to coupling layer
      CALL COMPSEND_R8TILES( ocnSSSName,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy, SSSocn2cpl )

C-    Send ocean surface velocity square to coupling layer
      CALL COMPSEND_R8TILES( ocnSSVsqName,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy, vSqocn2cpl )

      IF ( ocnCpl_exchange_DIC ) THEN
C-    Send ocean CO2 flux to coupling layer
        CALL COMPSEND_R8TILES( ocnFCO2Name,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy, fluxCO2cpl )
      ENDIF

      _END_MASTER( myThid )
      _BARRIER

#endif /* COMPONENT_MODULE */

      RETURN
      END
