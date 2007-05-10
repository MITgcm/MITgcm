C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/ocn_import_fields.F,v 1.2 2007/05/10 21:15:52 jscott Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE OCN_IMPORT_FIELDS( myThid )
C     /==========================================================\
C     | SUBROUTINE OCN_IMPORT_FIELDS                             |
C     | o Routine for importing fields (e.g. surface fluxes)     |
C     |   from coupling layer.                                   |
C     |==========================================================|
C     | This version talks to the MIT Coupler. It uses the       |
C     | MIT Coupler "checkpoint 1" library calls.                |
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

C  Note: 1) all fields will have been gridded onto ocean grid.
C        2) MIT Coupler checkpoint1 does not allow asynchronous posting of 
C          data, so ordering has to be consistent with coupling layer ordering

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

C-    Receive salt flux (sea-ice model) from coupling layer.
      CALL COMPRECV_R8TILES( ocnSaltFxName,
     I                       sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O                       iceSaltFlx )

C-    Receive sea-ice mass (sea-ice model) from coupling layer.
      CALL COMPRECV_R8TILES( ocnSeaIceName,
     I                       sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O                       seaIceMass )

      IF ( ocnCpl_exchange_DIC ) THEN
C-      Receive atmos CO2 level from coupling layer.
        CALL COMPRECV_R8TILES( ocnAirCO2Name,
     I                       sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O                       airCO2 )

C-      Receive ground-level windspeed from coupling layer.
        CALL COMPRECV_R8TILES( ocnWSpdName,
     I                       sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O                       surfWSpeed )

C-      Receive seaice fraction from coupling layer.
        CALL COMPRECV_R8TILES( ocnFIceName,
     I                       sNx, OLx, sNy, OLy, 1, nSx, nSy,
     O                       fracIce )
      ENDIF

      RETURN
      END
