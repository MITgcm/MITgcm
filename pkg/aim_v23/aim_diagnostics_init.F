C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_diagnostics_init.F,v 1.6 2007/02/21 21:32:09 jmc Exp $
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
C     diagName  :: local short name (8c) of a diagnostics
C     diagCode  :: local parser field with characteristics of the diagnostics
C              cf head of S/R DIAGNOSTICS_INIT_EARLY or DIAGNOSTICS_MAIN_INIT
C     diagUnits :: local string (16c): physical units of a diagnostic field
C     diagTitle :: local string (80c): description of field in diagnostic
      INTEGER       diagNum
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      diagName  = 'DIABT   '
      diagTitle =
     &  'Pot. Temp.  Tendency (Mass-Weighted) from Diabatic Processes'
      diagUnits = 'K/s             '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DIABQ   '
      diagTitle =
     &  'Spec.Humid. Tendency (Mass-Weighted) from Diabatic Processes'
      diagUnits = 'g/kg/s          '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'RADSW   '
      diagTitle =
     &  'Temperature Tendency due to Shortwave Radiation (TT_RSW)'
      diagUnits = 'K/s             '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'RADLW   '
      diagTitle =
     &  'Temperature Tendency due to Longwave  Radiation (TT_RLW)'
      diagUnits = 'K/s             '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DTCONV  '
      diagTitle = 'Temperature Tendency due to Convection (TT_CNV)'
      diagUnits = 'K/s             '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TURBT   '
      diagTitle =
     &  'Temperature Tendency due to Turbulence in PBL (TT_PBL)'
      diagUnits = 'K/s             '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DTLS    '
      diagTitle =
     &  'Temperature Tendency due to Large-scale condens. (TT_LSC)'
      diagUnits = 'K/s             '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DQCONV  '
      diagTitle = 'Spec. Humidity Tendency due to Convection (QT_CNV)'
      diagUnits = 'g/kg/s          '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TURBQ   '
      diagTitle =
     &  'Spec. Humidity Tendency due to Turbulence in PBL (QT_PBL)'
      diagUnits = 'g/kg/s          '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DQLS    '
      diagTitle =
     &  'Spec. Humidity Tendency due to Large-Scale Condens. (QT_LSC)'
      diagUnits = 'g/kg/s          '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'RELHUM  '
      diagTitle = 'Relative Humidity (fraction)'
      diagUnits = '0-1             '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TSR     '
      diagTitle = 'Top-of-atm. net Shortwave Radiation (+=dw)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM P    U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'OLR     '
      diagTitle = 'Outgoing Longwave  Radiation (+=up)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM P    U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'RADSWG  '
      diagTitle = 'Net Shortwave Radiation at the Ground (+=dw)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'RADLWG  '
      diagTitle = 'Net Longwave  Radiation at the Ground (+=up)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'HFLUX   '
      diagTitle = 'Sensible Heat Flux (+=up)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'EVAP    '
      diagTitle = 'Surface Evaporation (g/m2/s)'
      diagUnits = 'g/m^2/s         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'PRECON  '
      diagTitle = 'Convective  Precipitation (g/m2/s)'
      diagUnits = 'g/m^2/s         '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'PRECLS  '
      diagTitle = 'Large Scale Precipitation (g/m2/s)'
      diagUnits = 'g/m^2/s         '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'CLDFRC  '
      diagTitle = 'Total Cloud Fraction (0-1)'
      diagUnits = '0-1             '
      diagCode  = 'SM P    M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'CLDPRS  '
      diagTitle = 'Cloud Top Pressure (normalized)'
      diagUnits = '0-1             '
      WRITE(diagCode,'(A,I3.3,A)') 'SM PC',diagNum,'M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'CLDMAS  '
      diagTitle = 'Cloud-base Mass Flux  (kg/m^2/s)'
      diagUnits = 'kg/m^2/s        '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DRAG    '
      diagTitle = 'Surface Drag Coefficient (kg/m^2/s)'
      diagUnits = 'kg/m^2/s        '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'WINDS   '
      diagTitle = 'Surface Wind Speed  (m/s)'
      diagUnits = 'm/s             '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TS      '
      diagTitle = 'near Surface Air Temperature  (K)'
      diagUnits = 'K               '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'QS      '
      diagTitle = 'near Surface Specific Humidity  (g/kg)'
      diagUnits = 'g/kg            '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'ENPREC  '
      diagTitle ='Energy flux associated with precip. (snow, rain Temp)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'ALBVISDF'
      diagTitle = 'Surface Albedo (Visible band) (0-1)'
      diagUnits = '0-1             '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DWNLWG  '
      diagTitle =
     &   'Downward Component of Longwave Flux at the Ground (+=dw)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'SWCLR   '
      diagTitle = 'Clear Sky Temp. Tendency due to Shortwave Radiation'
      diagUnits = 'K/s             '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'LWCLR   '
      diagTitle = 'Clear Sky Temp. Tendency due to Longwave  Radiation'
      diagUnits = 'K/s             '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TSRCLR  '
      diagTitle = 'Clear Sky Top-of-atm. net Shortwave Radiation (+=dw)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM P    U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'OLRCLR  '
      diagTitle = 'Clear Sky Outgoing Longwave  Radiation  (+=up)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM P    U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'SWGCLR  '
      diagTitle =
     &    'Clear Sky Net Shortwave Radiation at the Ground (+=dw)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'LWGCLR  '
      diagTitle =
     &    'Clear Sky Net Longwave  Radiation at the Ground (+=up)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'UFLUX   '
      diagTitle = 'Zonal Wind Surface Stress  (N/m^2)'
      diagUnits = 'N/m^2           '
      WRITE(diagCode,'(A,I3.3,A)') 'UU   ',diagNum+2,'L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'VFLUX   '
      diagTitle = 'Meridional Wind Surface Stress  (N/m^2)'
      diagUnits = 'N/m^2           '
      WRITE(diagCode,'(A,I3.3,A)') 'VV   ', diagNum ,'L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'dKE_Ustr'
      diagTitle = 
     &    'KE sink due to Zonal Surf. Stress and Strat. drag (<0)'
      diagUnits = 'W/m^2           '
      WRITE(diagCode,'(A,I3.3,A)') 'SU   ',diagNum+2,'L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'dKE_Vstr'
      diagTitle = 
     &    'KE sink due to Meridional Surf. Stress and Strat. drag (<0)'
      diagUnits = 'W/m^2           '
      WRITE(diagCode,'(A,I3.3,A)') 'SV   ', diagNum ,'L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DTSIMPL '
      diagTitle = 'Surf. Temp Change after 1 implicit time step'
      diagUnits = 'K               '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
