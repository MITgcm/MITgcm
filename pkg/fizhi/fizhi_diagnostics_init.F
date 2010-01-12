C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_diagnostics_init.F,v 1.13 2010/01/12 21:31:15 jmc Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"

CBOP
C     !ROUTINE: FIZHI_DIAGNOSTICS_INIT
C     !INTERFACE:
      SUBROUTINE FIZHI_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE FIZHI_DIAGNOSTICS_INIT
C     | o Routine to initialize list of all available diagnostics
C     |   for FIZHI package
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

      diagName  = 'HFLUX   '
      diagTitle = 'Sensible Heat Flux (pos.upwrd)  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'EFLUX   '
      diagTitle = 'Latent   Heat Flux (pos.upwrd)  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'UFLUX   '
      diagTitle = 'Zonal Wind Surface Stress  (N/m^2)'
      diagUnits = 'N/m2            '
      diagCode  = 'UM      L1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'VFLUX   '
      diagTitle = 'Meridional Wind Surface Stress  (N/m^2)'
      diagUnits = 'N/m2            '
      diagCode  = 'VM      L1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'QICE    '
      diagTitle = 'Heat Conduction through Sea-Ice  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'RADLWG  '
      diagTitle = 'Net Upward   Longwave Flux at the Ground  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'RADSWG  '
      diagTitle = 'Net Downward Shortwave Flux at the Ground  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'RI      '
      diagTitle = 'Richardson Number  (non-dimensional)'
      diagUnits = 'non-dimensional '
      diagCode  = 'SM      LL      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'CT      '
      diagTitle = 'Surface Drag Coef for Scalars (non-dimensional)'
      diagUnits = 'non-dimensional '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'CU      '
      diagTitle = 'Surface Drag Coef for Winds (non-dimensional)'
      diagUnits = 'non-dimensional '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'KM      '
      diagTitle = 'Eddy Diffusivity Coefficient for Momentum  (m^2/s)'
      diagUnits = 'm2/sec          '
      diagCode  = 'SM      LL      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'KH      '
      diagTitle = 'Eddy Diffusivity Coefficient for Scalars  (m^2/s)'
      diagUnits = 'm2/sec          '
      diagCode  = 'SM      LL      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'TURBU   '
      diagTitle = 'Zonal Wind Tendency from Turbulence  (m/s/day)'
      diagUnits = 'm/s/day         '
      diagCode  = 'UM      ML      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'TURBV   '
      diagTitle = 'Meridional Wind Tendency from Turbulence  (m/s/day)'
      diagUnits = 'm/s/day         '
      diagCode  = 'VM      ML      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'TURBT   '
      diagTitle =
     &  'Temperature Tendency from Turbulence  (K/day)'
      diagUnits = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'TURBQ   '
      diagTitle =
     &  'Specific Humidity Tendency from Turbulence  (g/kg/day)'
      diagUnits = 'gram/kg/day     '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'MOISTU  '
      diagTitle = 'Zonal Wind Tendency from Moist Proc  (m/s/day)'
      diagUnits = 'm/s/day         '
      diagCode  = 'UM      ML      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'MOISTV  '
      diagTitle = 'Meridional Wind Tendency from Moist Proc  (m/s/day)'
      diagUnits = 'm/s/day         '
      diagCode  = 'VM      ML      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'MOISTT  '
      diagTitle =
     &  'Temperature Tendency due to Moist Processes  (K/day)'
      diagUnits = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'MOISTQ  '
      diagTitle =
     &  'Specific Humidity Tendency due to Moist Processes  (g/kg/day)'
      diagUnits = 'gram/kg/day     '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'RADLW   '
      diagTitle =
     &  'Temperature Tendency due to Longwave Radiation  (K/day)'
      diagUnits = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'RADSW   '
      diagTitle =
     &  'Temperature Tendency due to Shortwave Radiation  (K/day)'
      diagUnits = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'PREACC  '
      diagTitle = 'Total Precipitation  (mm/day)'
      diagUnits = 'mm/day          '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'PRECON  '
      diagTitle = 'Convective Precipitation  (mm/day)'
      diagUnits = 'mm/day          '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'TUFLUX  '
      diagTitle = 'Turbulent Flux of Zonal Momentum  (N/m^2)'
      diagUnits = 'N/m2            '
      diagCode  = 'UM      LL      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'TVFLUX  '
      diagTitle = 'Turbulent Flux of Meridional Momentum  (N/m^2)'
      diagUnits = 'N/m2            '
      diagCode  = 'VM      LL      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'TTFLUX  '
      diagTitle = 'Turbulent Flux of Sensible Heat  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode  = 'SM      LL      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'TQFLUX  '
      diagTitle = 'Turbulent Flux of Latent Heat  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode  = 'SM      LL      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'CN      '
      diagTitle = 'Neutral Drag Coefficient  (non-dimensional)'
      diagUnits = 'non-dimensional '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'WINDS   '
      diagTitle = 'Surface Wind Speed  (m/s)'
      diagUnits = 'm/sec           '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'DTSRF   '
      diagTitle = 'Air/Surface Virtual Temperature Difference  (deg K)'
      diagUnits = 'K               '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'TGROUND '
      diagTitle = 'Ground Temperature  (deg K)'
      diagUnits = 'K               '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'TS      '
      diagTitle = 'Surface Air Temperature  (deg K)'
      diagUnits = 'K               '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'DTG     '
      diagTitle = 'Ground Temperature Adjustment (deg K)'
      diagUnits = 'K               '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'QG      '
      diagTitle = 'Ground Specific Humidity  (g/kg)'
      diagUnits = 'gram/kg         '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'QS      '
      diagTitle = 'Saturation Surface Specific Humidity  (g/kg)'
      diagUnits = 'gram/kg         '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'TGRLW   '
      diagTitle = 'Ground Temperature used by LW Radiation  (deg K)'
      diagUnits = 'K               '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'LWGUP   '
      diagTitle = 'Upward Longwave Radiation at the Ground  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'OLR     '
      diagTitle = 'Outgoing Longwave Radiation   (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'OLRCLR  '
      diagTitle = 'Clear Sky Outgoing Longwave Radiation  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'LWGCLR  '
      diagTitle = 'Clear Sky Net Longwave Flux at the Ground  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'LWCLR   '
      diagTitle = 'Clear Sky Longwave Radiation Heating Rates  (K/day)'
      diagUnits = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'TLW     '
      diagTitle = 'Temperature used by Radiation  (deg K)'
      diagUnits = 'K               '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SHRAD   '
      diagTitle = 'Specific Humidity used by Radiation  (g/kg)'
      diagUnits = 'gram/kg         '
      diagCode  = 'SM P    ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'OZLW    '
      diagTitle =
     & 'Ozone mixing ratio used by Longwave Radiation (non-dimensional)'
      diagUnits = 'non-dimensional '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'CLDRAS  '
      diagTitle = 'Convective Cloud Fraction  (0-1)'
      diagUnits = '0-1             '
      diagCode  = 'SM P    ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'CLDTOT  '
      diagTitle = '3-D Total Cloud Fraction  (0-1)'
      diagUnits = '0-1             '
      diagCode  = 'SM P    ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'LWGDOWN '
      diagTitle = 'Downward Longwave Radiation at the Ground  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'GWDT    '
      diagTitle =
     &  'Temperature Tendency due to Gravity Wave Drag  (K/day)'
      diagUnits = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'RADSWT  '
      diagTitle = 'Incident Shortwave Radiation at TOA (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'TAUCLD  '
      diagTitle =
     &  'Counted Cloud Optical Depth (non-dimensional) per 100 mb'
      diagUnits = 'non-dimensional '
      diagCode  = 'SM  C   ML      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'TAUCLDC '
      diagTitle = 'Cloud Optical Depth Counter'
      diagUnits = 'count           '
      diagCode  = 'SM  D   ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'CLDLOW  '
      diagTitle = 'Low-Level ( 1000-700 hPa) Cloud Fraction  (0-1)'
      diagUnits = '0-1             '
      diagCode  = 'SM P    M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'EVAP    '
      diagTitle = 'Surface Evaporation  (mm/day)'
      diagUnits = 'mm/day          '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'DPDT    '
      diagTitle = 'Surface Pressure Tendency  (hPa/day)'
      diagUnits = 'hPa/day         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'UAVE    '
      diagTitle = 'Average Zonal Wind (m/s)'
      diagUnits = 'm/s             '
      diagCode  = 'UM      ML      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'VAVE    '
      diagTitle = 'Average Meridional Wind (m/s)'
      diagUnits = 'm/s             '
      diagCode  = 'VM      ML      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'TAVE    '
      diagTitle = 'Average Potential Temperature (K)'
      diagUnits = 'K               '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'QAVE    '
      diagTitle =
     &  'Average Specific Humidity (g/kg)'
      diagUnits = 'gram/kg         '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'OMEGA   '
      diagTitle = 'Vertical Velocity  (hPa/day)'
      diagUnits = 'hPa/day         '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'DUDT    '
      diagTitle = 'Total Zonal Wind Tendency  (m/s/day)'
      diagUnits = 'm/s/day         '
      diagCode  = 'UM      ML      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'DVDT    '
      diagTitle = 'Total Meridional Wind Tendency  (m/s/day)'
      diagUnits = 'm/s/day         '
      diagCode  = 'VM      ML      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'DTDT    '
      diagTitle = 'Total Temperature Tendency  (K/day)'
      diagUnits = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'DQDT    '
      diagTitle = 'Total Specific Humidity Tendency  (g/kg/day)'
      diagUnits = 'gram/kg/day     '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'VORT    '
      diagTitle = 'Relative Vorticity  (x10^-4  sec^-1)'
      diagUnits = '(e-4 sec-1)     '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'DTLS    '
      diagTitle =
     &  'Temperature Tendency due to Stratiform Processes  (K/day)'
      diagUnits = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'DQLS    '
      diagTitle =
     & 'Specific Humidity Tendency from Stratiform Processes (g/kg/day)'
      diagUnits = 'gram/kg/day     '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'USTAR   '
      diagTitle = 'Friction Velocity  (m/s)'
      diagUnits = 'm/sec           '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'Z0      '
      diagTitle = 'Surface Roughness Z0  (m)'
      diagUnits = 'm               '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'FRQTRB  '
      diagTitle = 'Frequency of Turbulence  (0-1)'
      diagUnits = '0-1             '
      diagCode  = 'SM P    MM      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'PBL     '
      diagTitle = 'Estimated PBL Depth  (hPa)'
      diagUnits = 'hPa             '
      diagCode  = 'SM P    M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SWCLR   '
      diagTitle = 'Clear Sky Shortwave Heating Rates  (K/day)'
      diagUnits = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'OSR     '
      diagTitle = 'Outgoing Shortwave Radiation  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'OSRCLR  '
      diagTitle = 'Clear Sky Outgoing Shortwave Radiation  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'CLDMAS  '
      diagTitle = 'Cloud Mass Flux  (g/m^2/sec)'
      diagUnits = 'g/m2/sec        '
      diagCode  = 'SM P    LL      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'UWND    '
      diagTitle = 'Time-Averaged Zonal Wind  (m/s)'
      diagUnits = 'm/sec           '
      diagCode  = 'UM      ML      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'VWND    '
      diagTitle = 'Time-Averaged Meridional Wind  (m/s)'
      diagUnits = 'm/sec           '
      diagCode  = 'VM      ML      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'TMPU    '
      diagTitle = 'Time-Averaged Temperature  (deg K)'
      diagUnits = 'K               '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SPHU    '
      diagTitle = 'Time-Averaged Specific Humidity  (g/kg)'
      diagUnits = 'gram/kg         '
      diagCode  = 'SM P    ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'RFT     '
      diagTitle =
     &  'Temperature Tendency due to Rayleigh Friction (K/day)'
      diagUnits = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'PS      '
      diagTitle = 'Time-Averaged Surface Pressure  (hPa)'
      diagUnits = 'hPa             '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SLP     '
      diagTitle = 'Sea Level Pressure (mb)'
      diagUnits = 'mb              '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'TKE     '
      diagTitle = 'Time-Averaged Turbulent Kinetic Energy  (m/s)^2'
      diagUnits = '(m/sec)2        '
      diagCode  = 'SM P    LL      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SWGCLR  '
      diagTitle =
     &  'Clear Sky Net Downward SW Radiation at the Ground  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'PAVE    '
      diagTitle = 'Average Surface Pressure (hPa)'
      diagUnits = 'hPa/day         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'DIABU   '
      diagTitle =
     &  'Zonal Wind Tendency due to Diabatic Forcing  (m/s/day)'
      diagUnits = 'm/s/day         '
      diagCode  = 'UM      ML      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'DIABV   '
      diagTitle =
     &  'Meridional Wind Tendency due to Diabatic Forcing  (m/s/day)'
      diagUnits = 'm/s/day         '
      diagCode  = 'VM      ML      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'DIABT   '
      diagTitle =
     &  'Temperature Tendency due to Diabatic Forcing  (K/day)'
      diagUnits = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'DIABQ   '
      diagTitle =
     &  'Specific Humidity Tendency due to Diabatic Forcing  (g/kg/day)'
      diagUnits = 'gram/kg/day     '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'RFU     '
      diagTitle =
     &  'Zonal Wind Tendency due to Rayleigh Friction  (m/s/day)'
      diagUnits = 'm/s/day         '
      diagCode  = 'UM      ML      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'RFV     '
      diagTitle =
     &  'Meridional Wind Tendency due to Rayleigh Friction  (m/s/day)'
      diagUnits = 'm/s/day         '
      diagCode  = 'VM      ML      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'GWDU    '
      diagTitle =
     &  'Zonal Wind Tendency due to Gravity Wave Drag  (m/s/day)'
      diagUnits = 'm/s/day         '
      diagCode  = 'UM      ML      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'GWDV    '
      diagTitle =
     &  'Meridional Wind Tendency due to Gravity Wave Drag  (m/s/day)'
      diagUnits = 'm/s/day         '
      diagCode  = 'VM      ML      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'GWDUS   '
      diagTitle = 'Zonal Wind Gravity Wave Surface Stress (N/m^2)'
      diagUnits = 'N/m2            '
      diagCode  = 'UM      L1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'GWDVS   '
      diagTitle = 'Meridional Wind Gravity Wave Surface Stress (N/m^2)'
      diagUnits = 'N/m2            '
      diagCode  = 'VM      L1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'GWDUT   '
      diagTitle = 'Zonal Wind Gravity Wave Stress at Model Top (N/m^2)'
      diagUnits = 'N/m2            '
      diagCode  = 'UM      U1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'GWDVT   '
      diagTitle =
     &  'Meridional Wind Gravity Wave Stress at Model Top (N/m^2)'
      diagUnits = 'N/m2            '
      diagCode  = 'VM      U1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName = 'LZRAD   '
      diagTitle =
     &  'Estimated Cloud Liquid Water used in Radiation (mg/kg)'
      diagUnits = 'mg/kg           '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'VAVEUQ  '
      diagTitle = 'Vertically Averaged UWND*SPHU  (m/s g/kg)'
      diagUnits = 'm/s gram/kg     '
      diagCode  = 'UM      M1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName = 'VAVEVQ  '
      diagTitle = 'Vertically Averaged VWND*SPHU  (m/s g/kg)'
      diagUnits = 'm/s gram/kg     '
      diagCode  = 'VM      M1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName = 'VAVEUT  '
      diagTitle = 'Vertically Averaged UWND*TMPU  (m/s deg K)'
      diagUnits = 'm/s K           '
      diagCode  = 'UM      M1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName = 'VAVEVT  '
      diagTitle = 'Vertically Averaged VWND*TMPU  (m/s deg K)'
      diagUnits = 'm/s K           '
      diagCode  = 'VM      M1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName = 'CLDFRC  '
      diagTitle = '2-D Total Cloud Fraction  (0-1)'
      diagUnits = '0-1             '
      diagCode = 'SM P    M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'TPW     '
      diagTitle = 'Total Precipitable Water  (g/cm^2)'
      diagUnits = 'gram/cm2        '
      diagCode = 'SM P    M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'U2M     '
      diagTitle = 'Zonal Wind Interpolated to 2 Meters (m/s)'
      diagUnits = 'm/sec           '
      diagCode  = 'UM      M1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName = 'V2M     '
      diagTitle = 'Meridional Wind Interpolated to 2 Meters (m/s)'
      diagUnits = 'm/sec           '
      diagCode  = 'VM      M1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName = 'T2M     '
      diagTitle = 'Temperature Interpolated to 2 Meters (deg K)'
      diagUnits = 'K               '
      diagCode = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'Q2M     '
      diagTitle = 'Specific Humidity Interpolated to 2 Meters (g/kg)'
      diagUnits = 'gram/kg         '
      diagCode = 'SM P    M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'U10M    '
      diagTitle = 'Zonal Wind Interpolated to 10 Meters (m/s)'
      diagUnits = 'm/sec           '
      diagCode  = 'UM      M1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName = 'V10M    '
      diagTitle = 'Meridional Wind Interpolated to 10 Meters (m/s)'
      diagUnits = 'm/sec           '
      diagCode  = 'VM      M1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName = 'T10M    '
      diagTitle = 'Temperature Interpolated to 10 Meters (deg K)'
      diagUnits = 'K               '
      diagCode = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'Q10M    '
      diagTitle = 'Specific Humidity Interpolated to 10 Meters (g/kg)'
      diagUnits = 'gram/kg         '
      diagCode = 'SM P    M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'DTRAIN  '
      diagTitle = 'Detrainment Cloud Mass Flux  (g/m^2/sec)'
      diagUnits = 'g/m2/sec        '
      diagCode = 'SM P    LL      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'QFILL   '
      diagTitle = 'Filling of Negative Specific Humidity (g/kg/day)'
      diagUnits = 'gram/kg/day     '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'VAVEQAVE'
      diagTitle = 'Vertically Averaged QAVE  (mm/day)'
      diagUnits = 'mm/day          '
      diagCode = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'VAVEQFIL'
      diagTitle = 'Vertically Averaged QFILL  (mm/day)'
      diagUnits = 'mm/day          '
      diagCode = 'SM P    L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'CLDMID  '
      diagTitle = 'Mid-Level (700-400 hPa) Cloud Fraction  (0-1)'
      diagUnits = '0-1             '
      diagCode = 'SM P    M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'CLDHI   '
      diagTitle = 'High-Level (above 400 hPa) Cloud Fraction  (0-1)'
      diagUnits = '0-1             '
      diagCode = 'SM P    M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'TAULOW  '
      diagTitle = 'Low-Level (1000-700 hPa) Optical Depth'
      diagUnits = 'non-dimensional '
      diagCode  = 'SM PC   M1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName = 'TAULOWC '
      diagTitle = 'Low-Level Optical Depth Counter'
      diagUnits = 'count           '
      diagCode = 'SM  D   M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'TAUMID  '
      diagTitle = 'Mid-Level (700-400 hPa) Optical Depth'
      diagUnits = 'non-dimensional '
      diagCode  = 'SM PC   M1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName = 'TAUMIDC '
      diagTitle = 'Mid-Level Optical Depth Counter'
      diagUnits = 'count           '
      diagCode = 'SM  D   M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'TAUHI   '
      diagTitle = 'High-Level (above 400 hPa) Optical Depth'
      diagUnits = 'non-dimensional '
      diagCode  = 'SM PC   M1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName = 'TAUHIC  '
      diagTitle = 'High-Level Optical Depth Counter'
      diagUnits = 'count           '
      diagCode = 'SM  D   M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'CLDNP   '
      diagTitle = 'Non-Precipitating Cloud Fraction  (0-1)'
      diagUnits = '0-1             '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'CLDPRS  '
      diagTitle = 'Cloud Top Pressure (when cloudy)  (hPa)'
      diagUnits = 'hPa             '
      diagCode  = 'SM PC   M1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName = 'CTPCNT  '
      diagTitle = 'Cloud Top Pressure Counter'
      diagUnits = 'count           '
      diagCode = 'SM  D   M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'CLDTMP  '
      diagTitle = 'Cloud Top Temperature (when cloudy)  (deg K)'
      diagUnits = 'K               '
      diagCode  = 'SM PC   M1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName = 'CTTCNT  '
      diagTitle = 'Cloud Top Temperature Counter'
      diagUnits = 'count           '
      diagCode = 'SM  D   M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'TCANOPY '
      diagTitle =
     &  'Skin Temperature of the Canopy/Soil (SST over water)  (deg K)'
      diagUnits = 'K               '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'TDEEP   '
      diagTitle = 'Deep Soil Temperature  (deg K)'
      diagUnits = 'K               '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'QCANOPY '
      diagTitle = 'Specific Humidity of the Canopy  (g/kg)'
      diagUnits = 'gram/kg         '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'SMSHAL  '
      diagTitle =
     &  'Soil Moisture Field Capacity Fraction in Shallow (5 mm) Layer'
      diagUnits = '0-1             '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'SMROOT  '
      diagTitle = 'Soil Moisture Field Capacity Fraction in Root Zone'
      diagUnits = '0-1             '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'SMDEEP  '
      diagTitle =
     &  'Soil Moisture Field Capacity Fraction in Recharge Layer '
      diagUnits = '0-1             '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'CAPACITY'
      diagTitle = 'Canopy Interception Reservoir (mm)'
      diagUnits = 'mm              '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'SNOW    '
      diagTitle = 'Snow Depth (mm water equivalent)'
      diagUnits = 'mm water equiv  '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'RAINCON '
      diagTitle = 'Convective Rainfall (liquid precipitate)  (mm/day)'
      diagUnits = 'mm/day          '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'RAINLSP '
      diagTitle = 'Large-scale Rainfall (liquid precipitate)  (mm/day)'
      diagUnits = 'mm/day          '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'SNOWFALL'
      diagTitle = 'Total Snowfall (solid precipitate)  (mm/day)'
      diagUnits = 'mm/day          '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'RUNOFF  '
      diagTitle =
     &  'Water from precipitation not infiltrated into soil  (mm/s)'
      diagUnits = 'mm/sec          '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'FWSOIL  '
      diagTitle = 'Rainwater Infiltration into top soil layer  (mm/s)'
      diagUnits = 'mm/sec          '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'GDRAIN  '
      diagTitle =
     &  'Diffusion of Moisture across bottom of root zone  (mm/s)'
      diagUnits = 'mm/sec          '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'SNOWMELT'
      diagTitle = 'Rate of Snow Melt  (mm/s)'
      diagUnits = 'mm/sec          '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'ERESV   '
      diagTitle =
     &  'Latent Heat Flux from Canopy Interception Reservoir  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'ESOIL   '
      diagTitle = 'Latent Heat Flux from Bare Soil  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'EVEG    '
      diagTitle =
     & 'Latent Heat Flux (Transpiration) from Vegetated Surface (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'ESNOW   '
      diagTitle = 'Latent Heat Flux from Snow Pack  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'PARDF   '
      diagTitle =
     &  'Diffuse-beam Photosynthetically Active Radiation  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode = 'SM P    L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'PARDR   '
      diagTitle =
     &  'Direct-beam Photosynthetically Active Radiation  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode = 'SM P    L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'LAI     '
      diagTitle = 'Leaf Area Index  (%)'
      diagUnits = 'percent         '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'GREEN   '
      diagTitle = 'Green-ness Index  (%)'
      diagUnits = 'percent         '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'DLWDTC  '
      diagTitle =
     &  'Derivative of Upward LW Radiation wrt TCANOPY (W/m^2 /deg K)'
      diagUnits = 'watt/m2/K       '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'DHDTC   '
      diagTitle =
     &  'Derivative of Sensible Heat Flux wrt TCANOPY (W/m^2 /deg K)'
      diagUnits = 'watt/m2/K       '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'DEDTC   '
      diagTitle =
     &  'Derivative of Latent Heat Flux wrt TCANOPY (W/m^2 /deg K)'
      diagUnits = 'watt/m2/K       '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'VDTMOIST'
      diagTitle = 'Vertically Averaged MOISTT  (K/day)'
      diagUnits = 'K/day           '
      diagCode = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'VDTTURB '
      diagTitle = 'Vertically Averaged TURBT  (K/day)'
      diagUnits = 'K/day           '
      diagCode = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'VDTLWRAD'
      diagTitle = 'Vertically Averaged RADLW  (K/day)'
      diagUnits = 'K/day           '
      diagCode = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'VDTSWRAD'
      diagTitle = 'Vertically Averaged RADSW  (K/day)'
      diagUnits = 'K/day           '
      diagCode = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'VAVETAVE'
      diagTitle = 'Vertically Averaged TAVE  (K/day)'
      diagUnits = 'K/day           '
      diagCode = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'PSUBCLD '
      diagTitle = 'Pressure Thickness of the RAS Subcloud Layer (mb)'
      diagUnits = 'hPa             '
      diagCode  = 'SM PC   M1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName = 'PSUBCLDC'
      diagTitle =
     &  'Pressure Thickness of the RAS Subcloud Layer Counter'
      diagUnits = 'count           '
      diagCode = 'SM PD   M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'LCL     '
      diagTitle = 'Lifting Condensation Level (mb)                 '
      diagUnits = 'hPa             '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'EVPOT   '
      diagTitle = 'Potential Evapotranspiration (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'RHCHECK '
      diagTitle =
     & 'Net precip due to supersaturation above critical RH  (mm/day)'
      diagUnits = 'mm/day          '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'DHDQA   '
      diagTitle =
     &  'Derivative of Sensible Heat wrt Canopy Humidity (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'DEDQA   '
      diagTitle =
     &  'Derivative of Latent Heat wrt Canopy Humidity (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'DTC     '
      diagTitle = 'Total Change in Canopy Temperature (deg/sec)'
      diagUnits = 'K/sec           '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'DQC     '
      diagTitle = 'Change in Canopy Humidity (kg/kg/sec)'
      diagUnits = 'sec-1           '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'TCDTC   '
      diagTitle =
     &  'Change in Canopy Temperature - Energy Term (deg/sec)'
      diagUnits = 'K/sec           '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'RADDTC  '
      diagTitle =
     &  'Change in Canopy Temperature - Net Radiation Term (deg/sec)'
      diagUnits = 'K/sec           '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'SENDDTC '
      diagTitle =
     &  'Change in Canopy Temperature - Sensible Heat Term (deg/sec)'
      diagUnits = 'K/sec           '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'LATDTC  '
      diagTitle =
     &  'Change in Canopy Temperature - Latent Heat Term (deg/sec)'
      diagUnits = 'K/sec           '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'TDDTC   '
      diagTitle =
     &  'Change in Canopy Temperature - Ground Temp Term (deg/sec)'
      diagUnits = 'K/sec           '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'QCDTC   '
      diagTitle =
     &  'Change in Canopy Temperature - Humidity Term (deg/sec)'
      diagUnits = 'K/sec           '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'TRBQLIQ '
      diagTitle = 'Cloud Liquid Water Generated in Turbulence (mg/kg)'
      diagUnits = 'mg/kg           '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'TRBFCC  '
      diagTitle = 'Cloud Fraction Generated in Turbulence (fraction)'
      diagUnits = '0-1             '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'ALBEDO  '
      diagTitle = 'Mean Surface Albedo (SW_Up/SW_Down)  0-1 '
      diagUnits = '0-1             '
      diagCode  = 'SM  C   L1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName = 'ALBEDOC '
      diagTitle = 'Surface Albedo Counter'
      diagUnits = 'count           '
      diagCode = 'SM  D   L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'ALBVISDR'
      diagTitle = 'Direct  Beam VIS Surface Albedo (0-1)   '
      diagUnits = '0-1             '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'ALBVISDF'
      diagTitle = 'Diffuse Beam VIS Surface Albedo (0-1)   '
      diagUnits = '0-1             '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'ALBNIRDR'
      diagTitle = 'Direct  Beam NIR Surface Albedo (0-1)   '
      diagUnits = '0-1             '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'ALBNIRDF'
      diagTitle = 'Diffuse Beam NIR Surface Albedo (0-1)   '
      diagUnits = '0-1             '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'TAUAVE  '
      diagTitle =
     &  'Averaged Cloud Optical Depth (non-dimensional) per 100 mb'
      diagUnits = 'non-dimensional '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'RHCHECKT'
      diagTitle =
     & 'Temperature Change due to humidity above critical RH  (K/day)'
      diagUnits = 'K/day           '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'RHCHECKQ'
      diagTitle =
     & 'Spec.Humid. Change due to humidity above critical RH (g/kg/day)'
      diagUnits = 'g/kg/day        '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'CLDLSP  '
      diagTitle = 'Large-Scale Cloud Fraction  (0-1)'
      diagUnits = '0-1             '
      diagCode = 'SM P    ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'LZLOW   '
      diagTitle =
     &  'Vert integrated low-level (below 700mb) cloud water (mg/cm**2)'
      diagUnits = 'mg/cm2          '
      diagCode = 'SM P    L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'LZMID   '
      diagTitle =
     &  'Vert integrated mid-level (700-400mb) cloud water (mg/cm**2)'
      diagUnits = 'mg/cm2          '
      diagCode = 'SM P    L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'LZHI    '
      diagTitle =
     & 'Vert integrated high-level (above 400mb) cloud water (mg/cm**2)'
      diagUnits = 'mg/cm2          '
      diagCode = 'SM P    L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'LZTOT   '
      diagTitle =
     &  'Vertically integrated total suspended cloud water (mg/cm**2)'
      diagUnits = 'mg/cm2          '
      diagCode = 'SM P    L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'CAPE    '
      diagTitle = 'Convective Available Potential Energy'
      diagUnits = 'mb m2/sec2      '
      diagCode = 'SM P    L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'HSUBCL  '
      diagTitle = 'Moist Static Energy in RAS subcloud layer'
      diagUnits = 'm2/sec2         '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'HSTAR   '
      diagTitle = 'Saturation Moist Static Energy'
      diagUnits = 'm2/sec2         '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'CONT    '
      diagTitle =
     &  'Temperature Tendency due to Convective  Precipitation (K/day)'
      diagUnits = 'K/day           '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'CONQ    '
      diagTitle =
     &  'Moisture Tendency due to Convective Precipitation (g/kg/day)'
      diagUnits = 'g/kg/day        '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'LST     '
      diagTitle =
     &  'Temperature Tendency due to Large Scale Precipitation (K/day)'
      diagUnits = 'K/day           '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'LSQ     '
      diagTitle =
     &  'Moisture Tendency due to Large Scale Precipitation (g/kg/day)'
      diagUnits = 'g/kg/day        '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'CONEVPT '
      diagTitle =
     &  'Temperature Tendency due to Conv Rain Re-evaporation (K/day)'
      diagUnits = 'K/day           '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'CONEVPQ '
      diagTitle =
     &  'Moisture Tendency due to Conv Rain Re-evaporation (g/kg/day)'
      diagUnits = 'g/kg/day        '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'LSEVPT  '
      diagTitle =
     &  'Temperature Tendency due to Large Scale Rain Re-evap (K/day)'
      diagUnits = 'K/day           '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'LSEVPQ  '
      diagTitle =
     &  'Moisture Tendency due to Large Scale Rain Re-evap (g/kg/day)'
      diagUnits = 'g/kg/day        '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'GWCUMID '
      diagTitle =
     &  'Mid-Lev Mean U-Wind for Gravity Wave Drag w/Clouds (m/sec)'
      diagUnits = 'm/sec           '
      diagCode  = 'UM      L1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName = 'GWCVMID '
      diagTitle =
     &  'Mid-Lev Mean V-Wind for Gravity Wave Drag w/Clouds (m/sec)'
      diagUnits = 'm/sec           '
      diagCode  = 'VM      L1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName = 'CLDSTD  '
      diagTitle =
     & 'Mid-Lev Cloud Perturb Length for Gravity Wave Drag w/Clouds (m)'
      diagUnits = 'm               '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'GWCUBAR '
      diagTitle =
     & 'Mid-Lev Perturb U-Wind for Gravity Wave Drag w/Clouds (m/sec)'
      diagUnits = 'm/sec           '
      diagCode  = 'UM      L1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName = 'GWCVBAR '
      diagTitle =
     & 'Mid-Lev Perturb V-Wind for Gravity Wave Drag w/Clouds (m/sec)'
      diagUnits = 'm/sec           '
      diagCode  = 'VM      L1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName = 'GWCUS   '
      diagTitle =
     & 'Mid-Lev U-Wind Stress for Gravity Wave Drag w/Clouds (N/m^2)'
      diagUnits = 'N/m2            '
      diagCode  = 'UM      L1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName = 'GWCVS   '
      diagTitle =
     & 'Mid-Lev V-Wind Stress for Gravity Wave Drag w/Clouds (N/m^2)'
      diagUnits = 'N/m2            '
      diagCode  = 'VM      L1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName = 'GWCU    '
      diagTitle =
     & 'U-Wind Tendency due to Gravity Wave Drag w/Clouds (m/s/day)'
      diagUnits = 'm/sec/day       '
      diagCode  = 'UM      ML      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName = 'GWCV    '
      diagTitle =
     & 'V-Wind Tendency due to Gravity Wave Drag w/Clouds (m/s/day)'
      diagUnits = 'm/sec/day       '
      diagCode  = 'VM      ML      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName = 'PLALBEDO'
      diagTitle = 'Planetary Albedo (Fraction)'
      diagUnits = 'Fraction        '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName = 'FIZPRES '
      diagTitle = 'Pressures for Fizhi (mb)'
      diagUnits = 'mb  (hPa)       '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'CORRDU  '
      diagTitle =
     &  'Zonal Wind Tendency Gridalt Correction Term (m/s/day)'
      diagUnits = 'm/s/day         '
      diagCode  = 'UM      ML      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'CORRDV  '
      diagTitle =
     &  'Meridional Wind Tendency Gridalt Correction Term (m/s/day)'
      diagUnits = 'm/s/day         '
      diagCode  = 'VM      ML      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'CORRDT  '
      diagTitle =
     &  'Temperature Tendency Gridalt Correction Term (K/day)'
      diagUnits = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'CORRDQ  '
      diagTitle =
     &  'Specific Humidity Tendency Gridalt Correction Term (g/kg/day)'
      diagUnits = 'gram/kg/day     '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'DIABUDYN'
      diagTitle =
     &  'Zonal Wind Tend due to Diab Forcing - Dyn Grid (m/s/day)'
      diagUnits = 'm/s/day         '
      diagCode  = 'UM      MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'DIABVDYN'
      diagTitle =
     &  'Meridional Wind Tend due to Diab Forcing - Dyn Grid (m/s/day)'
      diagUnits = 'm/s/day         '
      diagCode  = 'VM      MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'DIABTDYN'
      diagTitle =
     &  'Temperature Tend due to Diab Forcing - Dyn Grid (K/day)'
      diagUnits = 'K/day           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'DIABQDYN'
      diagTitle =
     &  'Specific Hum Tend due to Diab Forcing - Dyn Grid (g/kg/day)'
      diagUnits = 'gram/kg/day     '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'TENDUFIZ'
      diagTitle =
     &  'Total Zonal Wind Tend - Fizhi Grid (m/s/day)'
      diagUnits = 'm/s/day         '
      diagCode  = 'UM      ML      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'TENDVFIZ'
      diagTitle =
     &  'Total Meridional Wind Tend - Fizhi Grid (m/s/day)'
      diagUnits = 'm/s/day         '
      diagCode  = 'VM      ML      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'TENDTFIZ'
      diagTitle =
     &  'Total Temperature Tend - Fizhi Grid (K/day)'
      diagUnits = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'TENDQFIZ'
      diagTitle =
     &  'Total Specific Hum Tend - Fizhi Grid (g/kg/day)'
      diagUnits = 'gram/kg/day     '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
