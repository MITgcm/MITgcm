C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_diagnostics_init.F,v 1.11 2010/10/26 20:59:53 dfer Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CBOP
C     !ROUTINE: AIM_DIAGNOSTICS_INIT
C     !INTERFACE:
      SUBROUTINE AIM_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE AIM_DIAGNOSTICS_INIT
C     | o Routine to initialize list of all available diagnostics
C     |   for AIM package
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
C     !LOCAL VARIABLES:
C     === Local variables ===
C     diagNum   :: diagnostics number in the (long) list of available diag.
C     diagMate  :: diag. mate number in the (long) list of available diag.
C     diagName  :: local short name (8c) of a diagnostics
C     diagCode  :: local parser field with characteristics of the diagnostics
C              cf head of S/R DIAGNOSTICS_INIT_EARLY or DIAGNOSTICS_MAIN_INIT
C     diagUnits :: local string (16c): physical units of a diagnostic field
C     diagTitle :: local string (80c): description of field in diagnostic
      INTEGER       diagNum
      INTEGER       diagMate
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      diagName  = 'DIABT   '
      diagTitle =
     &  'Pot. Temp.  Tendency (Mass-Weighted) from Diabatic Processes'
      diagUnits = 'K/s             '
      diagCode  = 'SMr     MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'DIABQ   '
      diagTitle =
     &  'Spec.Humid. Tendency (Mass-Weighted) from Diabatic Processes'
      diagUnits = 'g/kg/s          '
      diagCode  = 'SMr     MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'RADSW   '
      diagTitle =
     &  'Temperature Tendency due to Shortwave Radiation (TT_RSW)'
      diagUnits = 'K/s             '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'RADLW   '
      diagTitle =
     &  'Temperature Tendency due to Longwave  Radiation (TT_RLW)'
      diagUnits = 'K/s             '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'DTCONV  '
      diagTitle = 'Temperature Tendency due to Convection (TT_CNV)'
      diagUnits = 'K/s             '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'TURBT   '
      diagTitle =
     &  'Temperature Tendency due to Turbulence in PBL (TT_PBL)'
      diagUnits = 'K/s             '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'DTLS    '
      diagTitle =
     &  'Temperature Tendency due to Large-scale condens. (TT_LSC)'
      diagUnits = 'K/s             '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'DQCONV  '
      diagTitle = 'Spec. Humidity Tendency due to Convection (QT_CNV)'
      diagUnits = 'g/kg/s          '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'TURBQ   '
      diagTitle =
     &  'Spec. Humidity Tendency due to Turbulence in PBL (QT_PBL)'
      diagUnits = 'g/kg/s          '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'DQLS    '
      diagTitle =
     &  'Spec. Humidity Tendency due to Large-Scale Condens. (QT_LSC)'
      diagUnits = 'g/kg/s          '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'RELHUM  '
      diagTitle = 'Relative Humidity (fraction)'
      diagUnits = '0-1             '
      diagCode  = 'SMR     MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'TSR     '
      diagTitle = 'Top-of-atm. net Shortwave Radiation (+=dw)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM P    U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'OLR     '
      diagTitle = 'Outgoing Longwave  Radiation (+=up)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM P    U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'RADSWG  '
      diagTitle = 'Net Shortwave Radiation at the Ground (+=dw)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'RADLWG  '
      diagTitle = 'Net Longwave  Radiation at the Ground (+=up)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'HFLUX   '
      diagTitle = 'Sensible Heat Flux (+=up)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'EVAP    '
      diagTitle = 'Surface Evaporation (g/m2/s)'
      diagUnits = 'g/m^2/s         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'PRECON  '
      diagTitle = 'Convective  Precipitation (g/m2/s)'
      diagUnits = 'g/m^2/s         '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'PRECLS  '
      diagTitle = 'Large Scale Precipitation (g/m2/s)'
      diagUnits = 'g/m^2/s         '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#ifdef ALLOW_CLOUD_3D
      diagName  = 'CLDCLW  '
      diagTitle = 'Local Cloud Fraction (0-1)'
      diagUnits = '0-1             '
      diagCode  = 'SM P    ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif

      diagName  = 'CLDFRC  '
      diagTitle = 'Total Cloud Fraction (0-1)'
      diagUnits = '0-1             '
      diagCode  = 'SM P    M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'CLDPRS  '
      diagTitle = 'Cloud Top Pressure (normalized)'
      diagUnits = '0-1             '
      diagCode  = 'SM PC   M1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'CLDMAS  '
      diagTitle = 'Cloud-base Mass Flux  (kg/m^2/s)'
      diagUnits = 'kg/m^2/s        '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'DRAG    '
      diagTitle = 'Surface Drag Coefficient (kg/m^2/s)'
      diagUnits = 'kg/m^2/s        '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'WINDS   '
      diagTitle = 'Surface Wind Speed  (m/s)'
      diagUnits = 'm/s             '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'TS      '
      diagTitle = 'near Surface Air Temperature  (K)'
      diagUnits = 'K               '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'QS      '
      diagTitle = 'near Surface Specific Humidity  (g/kg)'
      diagUnits = 'g/kg            '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'ENPREC  '
      diagTitle ='Energy flux associated with precip. (snow, rain Temp)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'ALBVISDF'
      diagTitle = 'Surface Albedo (Visible band) (0-1)'
      diagUnits = '0-1             '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'DWNLWG  '
      diagTitle =
     &   'Downward Component of Longwave Flux at the Ground (+=dw)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'UPSWG   '
      diagTitle =
     &   'Upward Shortwave Flux at the Ground (+=up)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SWCLR   '
      diagTitle = 'Clear Sky Temp. Tendency due to Shortwave Radiation'
      diagUnits = 'K/s             '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'LWCLR   '
      diagTitle = 'Clear Sky Temp. Tendency due to Longwave  Radiation'
      diagUnits = 'K/s             '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'TSRCLR  '
      diagTitle = 'Clear Sky Top-of-atm. net Shortwave Radiation (+=dw)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM P    U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'OLRCLR  '
      diagTitle = 'Clear Sky Outgoing Longwave  Radiation  (+=up)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM P    U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SWGCLR  '
      diagTitle =
     &    'Clear Sky Net Shortwave Radiation at the Ground (+=dw)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'LWGCLR  '
      diagTitle =
     &    'Clear Sky Net Longwave  Radiation at the Ground (+=up)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'UPSWGclr'
      diagTitle =
     &   'Upward Shortwave Flux at the Ground (+=up)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'UFLUX   '
      diagTitle = 'Zonal Wind Surface Stress  (N/m^2)'
      diagUnits = 'N/m^2           '
      diagCode  = 'UU      L1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'VFLUX   '
      diagTitle = 'Meridional Wind Surface Stress  (N/m^2)'
      diagUnits = 'N/m^2           '
      diagCode  = 'VV      L1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'dKE_Ustr'
      diagTitle =
     &    'KE sink due to Zonal Surf. Stress and Strat. drag (<0)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SU      L1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'dKE_Vstr'
      diagTitle =
     &    'KE sink due to Merid. Surf. Stress and Strat. drag (<0)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SV      L1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'DTSIMPL '
      diagTitle = 'Surf. Temp Change after 1 implicit time step'
      diagUnits = 'K               '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#ifdef ALLOW_AIM_CO2
      diagName  = 'aim_pCO2'
      diagTitle = 'Atmospheric CO2'
      diagUnits = 'ppm             '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif

cdfer
      diagName  = 'FSOL    '
      diagTitle = 'Incoming Solar radiation'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I            diagName, diagCode, diagUnits, diagTitle, 0, myThid )
cdfer

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
