C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_diagnostics_init.F,v 1.2 2005/07/10 00:47:42 jmc Exp $
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
     &  'Temperature Tendency due to Diabatic Forcing  (K/day)'
      diagUnits = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DIABQ   '
      diagTitle =
     &  'Specific Humidity Tendency due to Diabatic Forcing  (g/kg/day)'
      diagUnits = 'gram/kg/day     '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'RADLW   '
      diagTitle =
     &  'Temperature Tendency due to Longwave Radiation  (K/day)'
      diagUnits = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'RADSW   '
      diagTitle =
     &  'Temperature Tendency due to Shortwave Radiation  (K/day)'
      diagUnits = 'K/day           '
      diagCode  = 'SM P    ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DTCONV  '
      diagTitle = 'Temp Change due to Convection (K/sec)'
      diagUnits = 'K/s             '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TURBT   '
      diagTitle =
     &  'Temperature Tendency from Turbulence  (K/day)'
      diagUnits = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DTLS    '
      diagTitle =
     &  'Temperature Tendency due to Stratiform Processes  (K/day)'
      diagUnits = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DQCONV  '
      diagTitle = 'Humidity Change due to Convection (g/kg/sec)'
      diagUnits = 'g/kg/s          '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TURBQ   '
      diagTitle =
     &  'Specific Humidity Tendency from Turbulence  (g/kg/day)'
      diagUnits = 'gram/kg/day     '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DQLS    '
      diagTitle =
     & 'Specific Humidity Tendency from Stratiform Processes (g/kg/day)'
      diagUnits = 'gram/kg/day     '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'RELHUM  '
      diagTitle = 'Relative Humidity (fraction)'
      diagUnits = 'Fraction        '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'OSR     '
      diagTitle = 'Outgoing Shortwave Radiation  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode  = 'SM P    U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'OLR     '
      diagTitle = 'Outgoing Longwave Radiation   (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'RADSWG  '
      diagTitle = 'Net Downward Shortwave Flux at the Ground  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'RADLWG  '
      diagTitle = 'Net Upward   Longwave Flux at the Ground  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'HFLUX   '
      diagTitle = 'Sensible Heat Flux (pos.upwrd)  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'EVAP    '
      diagTitle = 'Surface Evaporation  (mm/day)'
      diagUnits = 'mm/day          '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'PRECON  '
      diagTitle = 'Convective Precipitation  (mm/day)'
      diagUnits = 'mm/day          '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'PRECLS  '
      diagTitle = 'Large Scale Precipitation (g/m2/sec)'
      diagUnits = 'g/m^2/s         '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'CLDFRC  '
      diagTitle = '2-D Total Cloud Fraction  (0-1)'
      diagUnits = '0-1             '
      diagCode  = 'SM P    M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'CLDPRS  '
      diagTitle = 'Cloud Top Pressure (when cloudy)  (hPa)'
      diagUnits = 'hPa             '
      WRITE(diagCode,'(A,I3.3,A)') 'SM PC',diagNum+2,'M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'CTPCNT  '
      diagTitle = 'Cloud Top Pressure Counter'
      diagUnits = 'count           '
      diagCode  = 'SM  D   M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'CLDMAS  '
      diagTitle = 'Cloud Mass Flux  (g/m^2/sec)'
      diagUnits = 'g/m2/sec        '
      diagCode  = 'SM P    LL      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'KM      '
      diagTitle = 'Eddy Diffusivity Coefficient for Momentum  (m^2/s)'
      diagUnits = 'm2/sec          '
      diagCode  = 'SM      LL      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'WINDS   '
      diagTitle = 'Surface Wind Speed  (m/s)'
      diagUnits = 'm/sec           '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TS      '
      diagTitle = 'Surface Air Temperature  (deg K)'
      diagUnits = 'K               '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'QS      '
      diagTitle = 'Saturation Surface Specific Humidity  (g/kg)'
      diagUnits = 'gram/kg         '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'ENPREC  '
      diagTitle = 'Energy of Precipitation (snow, rain Temp) in (J/g)'
      diagUnits = 'J/g             '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'ALBVISDF'
      diagTitle = 'Diffuse Beam VIS Surface Albedo (0-1)   '
      diagUnits = '0-1             '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'SWCLR   '
      diagTitle = 'Clear Sky Shortwave Heating Rates  (K/day)'
      diagUnits = 'K/day           '
      diagCode  = 'SM P    ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'LWCLR   '
      diagTitle = 'Clear Sky Longwave Radiation Heating Rates  (K/day)'
      diagUnits = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'OSRCLR  '
      diagTitle = 'Clear Sky Outgoing Shortwave Radiation  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode  = 'SM P    U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'OLRCLR  '
      diagTitle = 'Clear Sky Outgoing Longwave Radiation  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'SWGCLR  '
      diagTitle =
     &  'Clear Sky Net Downward SW Radiation at the Ground  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'LWGCLR  '
      diagTitle = 'Clear Sky Net Longwave Flux at the Ground  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'UFLUX   '
      diagTitle = 'Zonal Wind Surface Stress  (N/m^2)'
      diagUnits = 'N/m2            '
      WRITE(diagCode,'(A,I3.3,A)') 'UM   ',diagNum+2,'L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'VFLUX   '
      diagTitle = 'Meridional Wind Surface Stress  (N/m^2)'
      diagUnits = 'N/m2            '
      WRITE(diagCode,'(A,I3.3,A)') 'VM   ', diagNum ,'L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DTSIMPL '
      diagTitle = 'Surf. Temp Change after 1 implicit time step'
      diagUnits = 'Deg C           '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
