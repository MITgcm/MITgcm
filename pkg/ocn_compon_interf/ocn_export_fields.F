C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/ocn_export_fields.F,v 1.2 2007/05/10 21:15:52 jscott Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE OCN_EXPORT_FIELDS( myThid )
C     /==========================================================\
C     | SUBROUTINE OCN_EXPORT_FIELDS                             |
C     | o Routine for exporting oceanic fields                   |
C     |   to the coupling layer.                                 |
C     |==========================================================|
C     | This version talks to the MIT Coupler. It uses the MIT   |
C     | Coupler "checkpoint1" library calls.                     |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "OCNIDS.h"
#include "OCNCPL.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine
      INTEGER myThid
CEndOfInterface

C     == Local variables ==

C-    Send Oceanic fields to coupling layer
C  Note: 1) Information is sent on the ocean model grid.
C        2) MIT Coupler checkpoint1 does not allow asynchronous posting of
C          data, so ordering has to be consistent with coupling layer ordering

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

      RETURN
      END
