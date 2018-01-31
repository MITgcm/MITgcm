#include "OCN_CPL_OPTIONS.h"

CBOP 0
C !ROUTINE: OCN_EXPORT_FIELDS

C !INTERFACE:
      SUBROUTINE OCN_EXPORT_FIELDS( myIter, myThid )

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
#include "CPL_PARAMS.h"
#include "OCNIDS.h"
#include "OCNCPL.h"

C !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myIter  :: Current timestep number.
C     myThid  :: Thread number for this instance of the routine
      INTEGER myIter
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

#ifdef ALLOW_DIC
      IF ( ocn_cplExch_DIC ) THEN
C-    Send ocean CO2 flux to coupling layer
        CALL COMPSEND_R8TILES( ocnCO2FluxName,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy, fluxCO2cpl )
      ENDIF
#endif /* ALLOW_DIC */
#ifdef ALLOW_THSICE
      IF ( ocn_cplExch2W_sIce ) THEN
C-    Send thSIce state-vars to coupling layer
        CALL COMPSEND_R8TILES( ocnSIceFracName,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy, sIceFrac_cpl )
        CALL COMPSEND_R8TILES( ocnSIceThickName,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy, sIceThick_cpl )
        CALL COMPSEND_R8TILES( ocnSIceSnowName,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy, sIceSnowH_cpl )
        CALL COMPSEND_R8TILES( ocnSIceQ1Name,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy, sIceQ1_cpl )
        CALL COMPSEND_R8TILES( ocnSIceQ2Name,
     I     sNx, OLx, sNy, OLy, 1, nSx, nSy, sIceQ2_cpl )
      ENDIF
#endif /* ALLOW_THSICE */

      _END_MASTER( myThid )
      _BARRIER

#endif /* COMPONENT_MODULE */

      RETURN
      END
