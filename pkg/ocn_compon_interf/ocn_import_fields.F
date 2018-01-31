#include "OCN_CPL_OPTIONS.h"

CBOP 0
C !ROUTINE: OCN_IMPORT_FIELDS

C !INTERFACE:
      SUBROUTINE OCN_IMPORT_FIELDS( myIter, myThid )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE OCN_IMPORT_FIELDS
C     | o Routine for importing fields (e.g. surface fluxes)
C     |   from coupling layer.
C     *==========================================================*
C     | This version talks to the MIT Coupler. It uses the
C     | MIT Coupler "checkpoint 1" library calls.
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

C  Note: 1) all fields will have been gridded onto ocean grid.
C        2) MIT Coupler checkpoint1 does not allow asynchronous posting of
C          data, so ordering has to be consistent with coupling layer ordering

      _BARRIER
      _BEGIN_MASTER( myThid )

C-    Receive Atmospheric Sea-Level pressure from coupling layer.
      CALL COMPRECV_R8TILES( ocnSLPrName,
     I                       sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O                       atmSLPr )

C-    Receive surface heatflux from coupling layer.
      CALL COMPRECV_R8TILES( ocnHeatFluxName,
     I                       sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O                       HeatFlux )

C-    Receive net shortwave radiation from coupling layer.
      CALL COMPRECV_R8TILES( ocnQshortWaveName,
     I                       sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O                       qShortWave )

C-    Receive Zonal surface wind-stress from coupling layer.
      CALL COMPRECV_R8TILES( ocnTauXName,
     I                       sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O                       tauX )

C-    Receive Meridional surface wind-stress from coupling layer.
      CALL COMPRECV_R8TILES( ocnTauYName,
     I                       sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O                       tauY )

C-    Receive fresh water flux from coupling layer.
      CALL COMPRECV_R8TILES( ocnFWFluxName,
     I                       sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O                       FWFlux )

      IF ( ocn_cplExch1W_sIce ) THEN
C-    Receive salt flux (sea-ice model) from coupling layer.
        CALL COMPRECV_R8TILES( ocnSaltFxName,
     I                       sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O                       iceSaltFlx )

C-    Receive sea-ice mass (sea-ice model) from coupling layer.
        CALL COMPRECV_R8TILES( ocnSIceMassName,
     I                       sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O                       seaIceMass )
      ENDIF

#ifdef ALLOW_SALT_PLUME
      IF ( ocn_cplExch_SaltPl ) THEN
C-      Receive salt-plume flux (for pkg/salt_plume) from coupling layer.
        CALL COMPRECV_R8TILES( ocnSaltPlmFlxName,
     I                       sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O                       saltPlmFlx_cpl )
      ENDIF
#endif
#ifdef ALLOW_DIC
      IF ( ocn_cplExch_RunOff ) THEN
C-      Receive Run-Off from coupling layer.
        CALL COMPRECV_R8TILES( ocnRunOffName,
     I                       sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O                       RunOff_cpl )
      ENDIF
      IF ( ocn_cplExch_DIC ) THEN
C-      Receive atmos CO2 level from coupling layer.
        CALL COMPRECV_R8TILES( ocnAirCO2Name,
     I                       sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O                       airCO2 )

C-      Receive surface windspeed from coupling layer.
        CALL COMPRECV_R8TILES( ocnWindSpdName,
     I                       sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O                       surfWSpeed )
      ENDIF
#endif /* ALLOW_DIC */
#if (defined ALLOW_DIC) || (defined ALLOW_THSICE)
      IF ( ( ocn_cplExch1W_sIce.AND.ocn_cplExch_DIC )
     &  .OR. ocn_cplExch2W_sIce ) THEN
C-      Receive seaice fraction from coupling layer.
        CALL COMPRECV_R8TILES( ocnSIceFracName,
     I                       sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O                       sIceFrac_cpl )
      ENDIF
#endif /* ALLOW_DIC or ALLOW_THSICE */
#ifdef ALLOW_THSICE
      IF ( ocn_cplExch2W_sIce ) THEN
C-      Receive thSIce state-vars from coupling layer.
        CALL COMPRECV_R8TILES( ocnSIceThickName,
     I                       sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O                       sIceThick_cpl )
        CALL COMPRECV_R8TILES( ocnSIceSnowName,
     I                       sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O                       sIceSnowH_cpl )
        CALL COMPRECV_R8TILES( ocnSIceQ1Name,
     I                       sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O                       sIceQ1_cpl )
        CALL COMPRECV_R8TILES( ocnSIceQ2Name,
     I                       sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O                       sIceQ2_cpl )
      ENDIF
#endif /* ALLOW_THSICE */

      _END_MASTER( myThid )
      _BARRIER

#endif /* COMPONENT_MODULE */

      RETURN
      END
