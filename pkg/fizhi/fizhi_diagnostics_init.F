C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_diagnostics_init.F,v 1.2 2005/07/28 00:09:21 molod Exp $
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

      diagName  = 'UFLUX   '
      diagTitle  = 'Zonal Wind Surface Stress  (N/m^2)'
      diagUnits  = 'N/m2            '
      WRITE(diagCode,'(A,I3.3,A)') 'UM   ',diagNum+2,'L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'VFLUX   '
      diagTitle  = 'Meridional Wind Surface Stress  (N/m^2)'
      diagUnits  = 'N/m2            '
      WRITE(diagCode,'(A,I3.3,A)') 'VM   ', diagNum ,'L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'HFLUX   '
      diagTitle  = 'Sensible Heat Flux (pos.upwrd)  (W/m^2)'
      diagUnits  = 'watt/m2         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'EFLUX   '
      diagTitle  = 'Latent   Heat Flux (pos.upwrd)  (W/m^2)'
      diagUnits  = 'watt/m2         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'QICE    '
      diagTitle  = 'Heat Conduction through Sea-Ice  (W/m^2)'
      diagUnits  = 'watt/m2         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'RADLWG  '
      diagTitle  = 'Net Upward   Longwave Flux at the Ground  (W/m^2)'
      diagUnits  = 'watt/m2         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'RADSWG  '
      diagTitle  = 'Net Downward Shortwave Flux at the Ground  (W/m^2)'
      diagUnits  = 'watt/m2         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'RI      '
      diagTitle  = 'Richardson Number  (non-dimensional)'
      diagUnits  = 'non-dimensional '
      diagCode  = 'SM      LL      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'CT      '
      diagTitle  = 'Surface Drag Coef for Scalars (non-dimensional)'
      diagUnits  = 'non-dimensional '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'CU      '
      diagTitle  = 'Surface Drag Coef for Winds (non-dimensional)'
      diagUnits  = 'non-dimensional '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'KM      '
      diagTitle  = 'Eddy Diffusivity Coefficient for Momentum  (m^2/s)'
      diagUnits  = 'm2/sec          '
      diagCode  = 'SM      LL      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'KH      '
      diagTitle  = 'Eddy Diffusivity Coefficient for Scalars  (m^2/s)'
      diagUnits  = 'm2/sec          '
      diagCode  = 'SM      LL      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TURBU   '
      diagTitle  = 'Zonal Wind Tendency from Turbulence  (m/s/day)'
      diagUnits  = 'm/s/day         '
      WRITE(diagCode,'(A,I3.3,A)') 'UM   ',diagNum+2,'ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TURBV   '
      diagTitle  = 'Meridional Wind Tendency from Turbulence  (m/s/day)'
      diagUnits  = 'm/s/day         '
      WRITE(diagCode,'(A,I3.3,A)') 'VM   ', diagNum ,'ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TURBT   '
      diagTitle  =
     .  'Temperature Tendency from Turbulence  (K/day)'
      diagUnits  = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TURBQ   '
      diagTitle  =
     .  'Specific Humidity Tendency from Turbulence  (g/kg/day)'
      diagUnits  = 'gram/kg/day     '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'MOISTT  '
      diagTitle  =
     .  'Temperature Tendency due to Moist Processes  (K/day)'
      diagUnits  = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'MOISTQ  '
      diagTitle  =
     .  'Specific Humidity Tendency due to Moist Processes  (g/kg/day)'
      diagUnits  = 'gram/kg/day     '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'RADLW   '
      diagTitle  =
     .  'Temperature Tendency due to Longwave Radiation  (K/day)'
      diagUnits  = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'RADSW   '
      diagTitle  =
     .  'Temperature Tendency due to Shortwave Radiation  (K/day)'
      diagUnits  = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'PREACC  '
      diagTitle  = 'Total Precipitation  (mm/day)'
      diagUnits  = 'mm/day          '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'PRECON  '
      diagTitle  = 'Convective Precipitation  (mm/day)'
      diagUnits  = 'mm/day          '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TUFLUX  '
      diagTitle  = 'Turbulent Flux of Zonal Momentum  (N/m^2)'
      diagUnits  = 'N/m2            '
      WRITE(diagCode,'(A,I3.3,A)') 'UM   ',diagNum+2,'LL      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TVFLUX  '
      diagTitle  = 'Turbulent Flux of Meridional Momentum  (N/m^2)'
      diagUnits  = 'N/m2            '
      WRITE(diagCode,'(A,I3.3,A)') 'VM   ', diagNum ,'LL      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TTFLUX  '
      diagTitle  = 'Turbulent Flux of Sensible Heat  (W/m^2)'
      diagUnits  = 'watt/m2         '
      diagCode  = 'SM      LL      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TQFLUX  '
      diagTitle  = 'Turbulent Flux of Latent Heat  (W/m^2)'
      diagUnits  = 'watt/m2         '
      diagCode  = 'SM      LL      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'CN      '
      diagTitle  = 'Neutral Drag Coefficient  (non-dimensional)'
      diagUnits  = 'non-dimensional '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'WINDS   '
      diagTitle  = 'Surface Wind Speed  (m/s)'
      diagUnits  = 'm/sec           '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DTSRF   '
      diagTitle  = 'Air/Surface Virtual Temperature Difference  (deg K)'
      diagUnits  = 'K               '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TGROUND '
      diagTitle  = 'Ground Temperature  (deg K)'
      diagUnits  = 'K               '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TS      '
      diagTitle  = 'Surface Air Temperature  (deg K)'
      diagUnits  = 'K               '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DTG     '
      diagTitle  = 'Ground Temperature Adjustment (deg K)'
      diagUnits  = 'K               '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'QG      '
      diagTitle  = 'Ground Specific Humidity  (g/kg)'
      diagUnits  = 'gram/kg         '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'QS      '
      diagTitle  = 'Saturation Surface Specific Humidity  (g/kg)'
      diagUnits  = 'gram/kg         '
      diagCode  = 'SM P    L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TGRLW   '
      diagTitle  = 'Ground Temperature used by LW Radiation  (deg K)'
      diagUnits  = 'K               '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'LWGUP   '
      diagTitle  = 'Upward Longwave Radiation at the Ground  (W/m^2)'
      diagUnits  = 'watt/m2         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'OLR     '
      diagTitle  = 'Outgoing Longwave Radiation   (W/m^2)'
      diagUnits  = 'watt/m2         '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'OLRCLR  '
      diagTitle  = 'Clear Sky Outgoing Longwave Radiation  (W/m^2)'
      diagUnits  = 'watt/m2         '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'LWGCLR  '
      diagTitle  = 'Clear Sky Net Longwave Flux at the Ground  (W/m^2)'
      diagUnits  = 'watt/m2         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'LWCLR   '
      diagTitle  = 'Clear Sky Longwave Radiation Heating Rates  (K/day)'
      diagUnits  = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TLW     '
      diagTitle  = 'Temperature used by Radiation  (deg K)'
      diagUnits  = 'K               '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'SHRAD   '
      diagTitle  = 'Specific Humidity used by Radiation  (g/kg)'
      diagUnits  = 'gram/kg         '
      diagCode  = 'SM P    ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'OZLW    '
      diagTitle  =
     . 'Ozone mixing ratio used by Longwave Radiation (non-dimensional)'
      diagUnits  = 'non-dimensional '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'CLDRAS  '
      diagTitle  = 'Convective Cloud Fraction  (0-1)'
      diagUnits  = '0-1             '
      diagCode  = 'SM P    ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'CLDTOT  '
      diagTitle  = '3-D Total Cloud Fraction  (0-1)'
      diagUnits  = '0-1             '
      diagCode  = 'SM P    ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'LWGDOWN '
      diagTitle  = 'Downward Longwave Radiation at the Ground  (W/m^2)'
      diagUnits  = 'watt/m2         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'GWDT    '
      diagTitle  =
     .  'Temperature Tendency due to Gravity Wave Drag  (K/day)'
      diagUnits  = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'RADSWT  '
      diagTitle  = 'Incident Shortwave Radiation at TOA (W/m^2)'
      diagUnits  = 'watt/m2         '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TAUCLD  '
      diagTitle  =
     .  'Counted Cloud Optical Depth (non-dimensional) per 100 mb'
      diagUnits  = 'non-dimensional '
      WRITE(diagCode,'(A,I3.3,A)') 'SM  C',diagNum+2,'ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TAUCLDC '
      diagTitle  = 'Cloud Optical Depth Counter'
      diagUnits  = 'count           '
      diagCode  = 'SM  D   ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'CLDLOW  '
      diagTitle  = 'Low-Level ( 1000-700 hPa) Cloud Fraction  (0-1)'
      diagUnits  = '0-1             '
      diagCode  = 'SM P    M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'EVAP    '
      diagTitle  = 'Surface Evaporation  (mm/day)'
      diagUnits  = 'mm/day          '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DPDT    '
      diagTitle  = 'Surface Pressure Tendency  (hPa/day)'
      diagUnits  = 'hPa/day         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'UAVE    '
      diagTitle  = 'Average Zonal Wind (m/s)'
      diagUnits  = 'm/s             '
      WRITE(diagCode,'(A,I3.3,A)') 'UM   ',diagNum+2,'MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'VAVE    '
      diagTitle  = 'Average Meridional Wind (m/s)'
      diagUnits  = 'm/s             '
      WRITE(diagCode,'(A,I3.3,A)') 'VM   ', diagNum ,'MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TAVE    '
      diagTitle  = 'Average Potential Temperature (K)'
      diagUnits  = 'K               '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'QAVE    '
      diagTitle  =
     .  'Average Specific Humidity (g/kg)'
      diagUnits  = 'gram/kg         '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'OMEGA   '
      diagTitle  = 'Vertical Velocity  (hPa/day)'
      diagUnits  = 'hPa/day         '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DUDT    '
      diagTitle  = 'Total Zonal Wind Tendency  (m/s/day)'
      diagUnits  = 'm/s/day         '
      WRITE(diagCode,'(A,I3.3,A)') 'UM   ',diagNum+2,'ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DVDT    '
      diagTitle  = 'Total Meridional Wind Tendency  (m/s/day)'
      diagUnits  = 'm/s/day         '
      WRITE(diagCode,'(A,I3.3,A)') 'VM   ', diagNum ,'ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DTDT    '
      diagTitle  = 'Total Temperature Tendency  (K/day)'
      diagUnits  = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DQDT    '
      diagTitle  = 'Total Specific Humidity Tendency  (g/kg/day)'
      diagUnits  = 'gram/kg/day     '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'VORT    '
      diagTitle  = 'Relative Vorticity  (x10^-4  sec^-1)'
      diagUnits  = '(e-4 sec-1)     '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'PSIGDOT '
      diagTitle  = 'Pi-Weighted Vertical Sigma Velocity  (hPa/day)'
      diagUnits  = 'hPa/day         '
      diagCode  = 'SM      LL      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DTLS    '
      diagTitle  =
     .  'Temperature Tendency due to Stratiform Processes  (K/day)'
      diagUnits  = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DQLS    '
      diagTitle  =
     . 'Specific Humidity Tendency from Stratiform Processes (g/kg/day)'
      diagUnits  = 'gram/kg/day     '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'USTAR   '
      diagTitle  = 'Friction Velocity  (m/s)'
      diagUnits  = 'm/sec           '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'Z0      '
      diagTitle  = 'Surface Roughness Z0  (m)'
      diagUnits  = 'm               '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'FRQTRB  '
      diagTitle  = 'Frequency of Turbulence  (0-1)'
      diagUnits  = '0-1             '
      diagCode  = 'SM P    MM      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'PBL     '
      diagTitle  = 'Estimated PBL Depth  (hPa)'
      diagUnits  = 'hPa             '
      diagCode  = 'SM P    M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'SWCLR   '
      diagTitle  = 'Clear Sky Shortwave Heating Rates  (K/day)'
      diagUnits  = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'OSR     '
      diagTitle  = 'Outgoing Shortwave Radiation  (W/m^2)'
      diagUnits  = 'watt/m2         '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'OSRCLR  '
      diagTitle  = 'Clear Sky Outgoing Shortwave Radiation  (W/m^2)'
      diagUnits  = 'watt/m2         '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'CLDMAS  '
      diagTitle  = 'Cloud Mass Flux  (g/m^2/sec)'
      diagUnits  = 'g/m2/sec        '
      diagCode  = 'SM P    LL      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'UWND    '
      diagTitle  = 'Time-Averaged Zonal Wind  (m/s)'
      diagUnits  = 'm/sec           '
      WRITE(diagCode,'(A,I3.3,A)') 'UM   ',diagNum+2,'ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'VWND    '
      diagTitle  = 'Time-Averaged Meridional Wind  (m/s)'
      diagUnits  = 'm/sec           '
      WRITE(diagCode,'(A,I3.3,A)') 'VM   ', diagNum ,'ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TMPU    '
      diagTitle  = 'Time-Averaged Temperature  (deg K)'
      diagUnits  = 'K               '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'SPHU    '
      diagTitle  = 'Time-Averaged Specific Humidity  (g/kg)'
      diagUnits  = 'gram/kg         '
      diagCode  = 'SM P    ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'RFT     '
      diagTitle  =
     .  'Temperature Tendency due to Rayleigh Friction (K/day)'
      diagUnits  = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'PS      '
      diagTitle  = 'Time-Averaged Surface Pressure  (hPa)'
      diagUnits  = 'hPa             '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TKE     '
      diagTitle  = 'Time-Averaged Turbulent Kinetic Energy  (m/s)^2'
      diagUnits  = '(m/sec)2        '
      diagCode  = 'SM P    LL      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'SWGCLR  '
      diagTitle  =
     .  'Clear Sky Net Downward SW Radiation at the Ground  (W/m^2)'
      diagUnits  = 'watt/m2         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'PAVE    '
      diagTitle  = 'Average Surface Pressure (hPa)'
      diagUnits  = 'hPa/day         '
      diagCode  = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DIABU   '
      diagTitle  =
     .  'Zonal Wind Tendency due to Diabatic Forcing  (m/s/day)'
      diagUnits  = 'm/s/day         '
      WRITE(diagCode,'(A,I3.3,A)') 'UM   ',diagNum+2,'ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DIABV   '
      diagTitle  =
     .  'Meridional Wind Tendency due to Diabatic Forcing  (m/s/day)'
      diagUnits  = 'm/s/day         '
      WRITE(diagCode,'(A,I3.3,A)') 'VM   ', diagNum ,'ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DIABT   '
      diagTitle  =
     .  'Temperature Tendency due to Diabatic Forcing  (K/day)'
      diagUnits  = 'K/day           '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DIABQ   '
      diagTitle  =
     .  'Specific Humidity Tendency due to Diabatic Forcing  (g/kg/day)'
      diagUnits  = 'gram/kg/day     '
      diagCode  = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'RFU     '
      diagTitle  =
     .  'Zonal Wind Tendency due to Rayleigh Friction  (m/s/day)'
      diagUnits  = 'm/s/day         '
      WRITE(diagCode,'(A,I3.3,A)') 'UM   ',diagNum+2,'ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'RFV     '
      diagTitle  =
     .  'Meridional Wind Tendency due to Rayleigh Friction  (m/s/day)'
      diagUnits  = 'm/s/day         '
      WRITE(diagCode,'(A,I3.3,A)') 'VM   ', diagNum ,'ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'GWDU    '
      diagTitle  =
     .  'Zonal Wind Tendency due to Gravity Wave Drag  (m/s/day)'
      diagUnits  = 'm/s/day         '
      WRITE(diagCode,'(A,I3.3,A)') 'UM   ',diagNum+2,'ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'GWDV    '
      diagTitle  =
     .  'Meridional Wind Tendency due to Gravity Wave Drag  (m/s/day)'
      diagUnits  = 'm/s/day         '
      WRITE(diagCode,'(A,I3.3,A)') 'VM   ', diagNum ,'ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'GWDUS   '
      diagTitle  = 'Zonal Wind Gravity Wave Surface Stress (N/m^2)'
      diagUnits  = 'N/m2            '
      WRITE(diagCode,'(A,I3.3,A)') 'UM   ',diagNum+2,'L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'GWDVS   '
      diagTitle  = 'Meridional Wind Gravity Wave Surface Stress (N/m^2)'
      diagUnits  = 'N/m2            '
      WRITE(diagCode,'(A,I3.3,A)') 'VM   ', diagNum ,'L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'GWDUT   '
      diagTitle  = 'Zonal Wind Gravity Wave Stress at Model Top (N/m^2)'
      diagUnits  = 'N/m2            '
      WRITE(diagCode,'(A,I3.3,A)') 'UM   ',diagNum+2,'U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'GWDVT   '
      diagTitle  =
     .  'Meridional Wind Gravity Wave Stress at Model Top (N/m^2)'
      diagUnits  = 'N/m2            '
      WRITE(diagCode,'(A,I3.3,A)') 'VM   ', diagNum ,'U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'LZRAD   '
      diagTitle =
     .  'Estimated Cloud Liquid Water used in Radiation (mg/kg)'
      diagUnits = 'mg/kg           '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'SLP     '
      diagTitle = 'Time-Averaged Sea-Level Pressure  (mb)'
      diagUnits = 'hPa             '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'VAVEUQ  '
      diagTitle = 'Vertically Averaged UWND*SPHU  (m/s g/kg)'
      diagUnits = 'm/s gram/kg     '
      WRITE(diagCode,'(A,I3.3,A)') 'UM   ',diagNum+2,'M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'VAVEVQ  '
      diagTitle = 'Vertically Averaged VWND*SPHU  (m/s g/kg)'
      diagUnits = 'm/s gram/kg     '
      WRITE(diagCode,'(A,I3.3,A)') 'VM   ', diagNum ,'M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'VAVEUT  '
      diagTitle = 'Vertically Averaged UWND*TMPU  (m/s deg K)'
      diagUnits = 'm/s K           '
      WRITE(diagCode,'(A,I3.3,A)') 'UM   ',diagNum+2,'M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'VAVEVT  '
      diagTitle = 'Vertically Averaged VWND*TMPU  (m/s deg K)'
      diagUnits = 'm/s K           '
      WRITE(diagCode,'(A,I3.3,A)') 'VM   ', diagNum ,'M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'CLDFRC  '
      diagTitle = '2-D Total Cloud Fraction  (0-1)'
      diagUnits = '0-1             '
      diagCode = 'SM P    M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'TPW     '
      diagTitle = 'Total Precipitable Water  (g/cm^2)'
      diagUnits = 'gram/cm2        '
      diagCode = 'SM P    M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'U2M     '
      diagTitle = 'Zonal Wind Interpolated to 2 Meters (m/s)'
      diagUnits = 'm/sec           '
      WRITE(diagCode,'(A,I3.3,A)') 'UM   ',diagNum+2,'M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'V2M     '
      diagTitle = 'Meridional Wind Interpolated to 2 Meters (m/s)'
      diagUnits = 'm/sec           '
      WRITE(diagCode,'(A,I3.3,A)') 'VM   ', diagNum ,'M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'T2M     '
      diagTitle = 'Temperature Interpolated to 2 Meters (deg K)'
      diagUnits = 'K               '
      diagCode = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'Q2M     '
      diagTitle = 'Specific Humidity Interpolated to 2 Meters (g/kg)'
      diagUnits = 'gram/kg         '
      diagCode = 'SM P    M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'U10M    '
      diagTitle = 'Zonal Wind Interpolated to 10 Meters (m/s)'
      diagUnits = 'm/sec           '
      WRITE(diagCode,'(A,I3.3,A)') 'UM   ',diagNum+2,'M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'V10M    '
      diagTitle = 'Meridional Wind Interpolated to 10 Meters (m/s)'
      diagUnits = 'm/sec           '
      WRITE(diagCode,'(A,I3.3,A)') 'VM   ', diagNum ,'M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'T10M    '
      diagTitle = 'Temperature Interpolated to 10 Meters (deg K)'
      diagUnits = 'K               '
      diagCode = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'Q10M    '
      diagTitle = 'Specific Humidity Interpolated to 10 Meters (g/kg)'
      diagUnits = 'gram/kg         '
      diagCode = 'SM P    M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'DTRAIN  '
      diagTitle = 'Detrainment Cloud Mass Flux  (g/m^2/sec)'
      diagUnits = 'g/m2/sec        '
      diagCode = 'SM P    LL      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'QFILL   '
      diagTitle = 'Filling of Negative Specific Humidity (g/kg/day)'
      diagUnits = 'gram/kg/day     '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'VAVEQAVE'
      diagTitle = 'Vertically Averaged QAVE  (mm/day)'
      diagUnits = 'mm/day          '
      diagCode = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'VAVEQFIL'
      diagTitle = 'Vertically Averaged QFILL  (mm/day)'
      diagUnits = 'mm/day          '
      diagCode = 'SM P    L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'CLDMID  '
      diagTitle = 'Mid-Level (700-400 hPa) Cloud Fraction  (0-1)'
      diagUnits = '0-1             '
      diagCode = 'SM P    M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'CLDHI   '
      diagTitle = 'High-Level (above 400 hPa) Cloud Fraction  (0-1)'
      diagUnits = '0-1             '
      diagCode = 'SM P    M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'TAULOW  '
      diagTitle = 'Low-Level (1000-700 hPa) Optical Depth'
      diagUnits = 'non-dimensional '
      WRITE(diagCode,'(A,I3.3,A)') 'SM PC',diagNum+2,'M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'TAULOWC '
      diagTitle = 'Low-Level Optical Depth Counter'
      diagUnits = 'count           '
      diagCode = 'SM  D   M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'TAUMID  '
      diagTitle = 'Mid-Level (700-400 hPa) Optical Depth'
      diagUnits = 'non-dimensional '
      WRITE(diagCode,'(A,I3.3,A)') 'SM PC',diagNum+2,'M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'TAUMIDC '
      diagTitle = 'Mid-Level Optical Depth Counter'
      diagUnits = 'count           '
      diagCode = 'SM  D   M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'TAUHI   '
      diagTitle = 'High-Level (above 400 hPa) Optical Depth'
      diagUnits = 'non-dimensional '
      WRITE(diagCode,'(A,I3.3,A)') 'SM PC',diagNum+2,'M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'TAUHIC  '
      diagTitle = 'High-Level Optical Depth Counter'
      diagUnits = 'count           '
      diagCode = 'SM  D   M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'CLDNP   '
      diagTitle = 'Non-Precipitating Cloud Fraction  (0-1)'
      diagUnits = '0-1             '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'CLDPRS  '
      diagTitle = 'Cloud Top Pressure (when cloudy)  (hPa)'
      diagUnits = 'hPa             '
      WRITE(diagCode,'(A,I3.3,A)') 'SM PC',diagNum+2,'M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'CTPCNT  '
      diagTitle = 'Cloud Top Pressure Counter'
      diagUnits = 'count           '
      diagCode = 'SM  D   M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'CLDTMP  '
      diagTitle = 'Cloud Top Temperature (when cloudy)  (deg K)'
      diagUnits = 'K               '
      WRITE(diagCode,'(A,I3.3,A)') 'SM PC',diagNum+2,'M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'CTTCNT  '
      diagTitle = 'Cloud Top Temperature Counter'
      diagUnits = 'count           '
      diagCode = 'SM  D   M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'TCANOPY '
      diagTitle =
     .  'Skin Temperature of the Canopy/Soil (SST over water)  (deg K)'
      diagUnits = 'K               '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'TDEEP   '
      diagTitle = 'Deep Soil Temperature  (deg K)'
      diagUnits = 'K               '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'QCANOPY '
      diagTitle = 'Specific Humidity of the Canopy  (g/kg)'
      diagUnits = 'gram/kg         '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'SMSHAL  '
      diagTitle =
     .  'Soil Moisture Field Capacity Fraction in Shallow (5 mm) Layer'
      diagUnits = '0-1             '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'SMROOT  '
      diagTitle = 'Soil Moisture Field Capacity Fraction in Root Zone'
      diagUnits = '0-1             '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'SMDEEP  '
      diagTitle =
     .  'Soil Moisture Field Capacity Fraction in Recharge Layer '
      diagUnits = '0-1             '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'CAPACITY'
      diagTitle = 'Canopy Interception Reservoir (mm)'
      diagUnits = 'mm              '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'SNOW    '
      diagTitle = 'Snow Depth (mm water equivalent)'
      diagUnits = 'mm water equiv  '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'RAINCON '
      diagTitle = 'Convective Rainfall (liquid precipitate)  (mm/day)'
      diagUnits = 'mm/day          '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'RAINLSP '
      diagTitle = 'Large-scale Rainfall (liquid precipitate)  (mm/day)'
      diagUnits = 'mm/day          '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'SNOWFALL'
      diagTitle = 'Total Snowfall (solid precipitate)  (mm/day)'
      diagUnits = 'mm/day          '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'RUNOFF  '
      diagTitle =
     .  'Water from precipitation not infiltrated into soil  (mm/s)'
      diagUnits = 'mm/sec          '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'FWSOIL  '
      diagTitle = 'Rainwater Infiltration into top soil layer  (mm/s)'
      diagUnits = 'mm/sec          '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'GDRAIN  '
      diagTitle =
     .  'Diffusion of Moisture across bottom of root zone  (mm/s)'
      diagUnits = 'mm/sec          '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'SNOWMELT'
      diagTitle = 'Rate of Snow Melt  (mm/s)'
      diagUnits = 'mm/sec          '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'ERESV   '
      diagTitle =
     .  'Latent Heat Flux from Canopy Interception Reservoir  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'ESOIL   '
      diagTitle = 'Latent Heat Flux from Bare Soil  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'EVEG    '
      diagTitle =
     . 'Latent Heat Flux (Transpiration) from Vegetated Surface (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'ESNOW   '
      diagTitle = 'Latent Heat Flux from Snow Pack  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'PARDF   '
      diagTitle =
     .  'Diffuse-beam Photosynthetically Active Radiation  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode = 'SM P    L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'PARDR   '
      diagTitle =
     .  'Direct-beam Photosynthetically Active Radiation  (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode = 'SM P    L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'LAI     '
      diagTitle = 'Leaf Area Index  (%)'
      diagUnits = 'percent         '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'GREEN   '
      diagTitle = 'Green-ness Index  (%)'
      diagUnits = 'percent         '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'DLWDTC  '
      diagTitle =
     .  'Derivative of Upward LW Radiation wrt TCANOPY (W/m^2 /deg K)'
      diagUnits = 'watt/m2/K       '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'DHDTC   '
      diagTitle =
     .  'Derivative of Sensible Heat Flux wrt TCANOPY (W/m^2 /deg K)'
      diagUnits = 'watt/m2/K       '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'DEDTC   '
      diagTitle =
     .  'Derivative of Latent Heat Flux wrt TCANOPY (W/m^2 /deg K)'
      diagUnits = 'watt/m2/K       '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'VDTMOIST'
      diagTitle = 'Vertically Averaged MOISTT  (K/day)'
      diagUnits = 'K/day           '
      diagCode = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'VDTTURB '
      diagTitle = 'Vertically Averaged TURBT  (K/day)'
      diagUnits = 'K/day           '
      diagCode = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'VDTLWRAD'
      diagTitle = 'Vertically Averaged RADLW  (K/day)'
      diagUnits = 'K/day           '
      diagCode = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'VDTSWRAD'
      diagTitle = 'Vertically Averaged RADSW  (K/day)'
      diagUnits = 'K/day           '
      diagCode = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'VAVETAVE'
      diagTitle = 'Vertically Averaged TAVE  (K/day)'
      diagUnits = 'K/day           '
      diagCode = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'PSUBCLD '
      diagTitle = 'Pressure Thickness of the RAS Subcloud Layer (mb)'
      diagUnits = 'hPa             '
      WRITE(diagCode,'(A,I3.3,A)') 'SM PC',diagNum+2,'M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'PSUBCLDC'
      diagTitle =
     .  'Pressure Thickness of the RAS Subcloud Layer Counter'
      diagUnits = 'count           '
      diagCode = 'SM PD   M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'LCL     '
      diagTitle = 'Lifting Condensation Level (mb)                 '
      diagUnits = 'hPa             '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'EVPOT   '
      diagTitle = 'Potential Evapotranspiration (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'RHCHECK '
      diagTitle =
     . 'Net precip due to supersaturation above critical RH  (mm/day)'
      diagUnits = 'mm/day          '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'DHDQA   '
      diagTitle =
     .  'Derivative of Sensible Heat wrt Canopy Humidity (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'DEDQA   '
      diagTitle =
     .  'Derivative of Latent Heat wrt Canopy Humidity (W/m^2)'
      diagUnits = 'watt/m2         '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'DTC     '
      diagTitle = 'Total Change in Canopy Temperature (deg/sec)'
      diagUnits = 'K/sec           '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'DQC     '
      diagTitle = 'Change in Canopy Humidity (kg/kg/sec)'
      diagUnits = 'sec-1           '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'TCDTC   '
      diagTitle =
     .  'Change in Canopy Temperature - Energy Term (deg/sec)'
      diagUnits = 'K/sec           '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'RADDTC  '
      diagTitle =
     .  'Change in Canopy Temperature - Net Radiation Term (deg/sec)'
      diagUnits = 'K/sec           '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'SENDDTC '
      diagTitle =
     .  'Change in Canopy Temperature - Sensible Heat Term (deg/sec)'
      diagUnits = 'K/sec           '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'LATDTC  '
      diagTitle =
     .  'Change in Canopy Temperature - Latent Heat Term (deg/sec)'
      diagUnits = 'K/sec           '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'TDDTC   '
      diagTitle =
     .  'Change in Canopy Temperature - Ground Temp Term (deg/sec)'
      diagUnits = 'K/sec           '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'QCDTC   '
      diagTitle =
     .  'Change in Canopy Temperature - Humidity Term (deg/sec)'
      diagUnits = 'K/sec           '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'TRBQLIQ '
      diagTitle = 'Cloud Liquid Water Generated in Turbulence (mg/kg)'
      diagUnits = 'mg/kg           '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'TRBFCC  '
      diagTitle = 'Cloud Fraction Generated in Turbulence (fraction)'
      diagUnits = '0-1             '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'ALBEDO  '
      diagTitle = 'Mean Surface Albedo (SW_Up/SW_Down)  0-1 '
      diagUnits = '0-1             '
      WRITE(diagCode,'(A,I3.3,A)') 'SM  C',diagNum+2,'L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'ALBEDOC '
      diagTitle = 'Surface Albedo Counter'
      diagUnits = 'count           '
      diagCode = 'SM  D   L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'ALBVISDR'
      diagTitle = 'Direct  Beam VIS Surface Albedo (0-1)   '
      diagUnits = '0-1             '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'ALBVISDF'
      diagTitle = 'Diffuse Beam VIS Surface Albedo (0-1)   '
      diagUnits = '0-1             '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'ALBNIRDR'
      diagTitle = 'Direct  Beam NIR Surface Albedo (0-1)   '
      diagUnits = '0-1             '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'ALBNIRDF'
      diagTitle = 'Diffuse Beam NIR Surface Albedo (0-1)   '
      diagUnits = '0-1             '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'TAUAVE  '
      diagTitle =
     .  'Averaged Cloud Optical Depth (non-dimensional) per 100 mb'
      diagUnits = 'non-dimensional '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'RHCHECKT'
      diagTitle =
     . 'Temperature Change due to humidity above critical RH  (K/day)'
      diagUnits = 'K/day           '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'RHCHECKQ'
      diagTitle =
     . 'Spec.Humid. Change due to humidity above critical RH (g/kg/day)'
      diagUnits = 'g/kg/day        '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'CLDLSP  '
      diagTitle = 'Large-Scale Cloud Fraction  (0-1)'
      diagUnits = '0-1             '
      diagCode = 'SM P    ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'LZLOW   '
      diagTitle =
     .  'Vert integrated low-level (below 700mb) cloud water (mg/cm**2)'
      diagUnits = 'mg/cm2          '
      diagCode = 'SM P    L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'LZMID   '
      diagTitle =
     .  'Vert integrated mid-level (700-400mb) cloud water (mg/cm**2)'
      diagUnits = 'mg/cm2          '
      diagCode = 'SM P    L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'LZHI    '
      diagTitle =
     . 'Vert integrated high-level (above 400mb) cloud water (mg/cm**2)'
      diagUnits = 'mg/cm2          '
      diagCode = 'SM P    L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'LZTOT   '
      diagTitle =
     .  'Vertically integrated total suspended cloud water (mg/cm**2)'
      diagUnits = 'mg/cm2          '
      diagCode = 'SM P    L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'CAPE    '
      diagTitle = 'Convective Available Potential Energy'
      diagUnits = 'mb m2/sec2      '
      diagCode = 'SM P    L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'HSUBCL  '
      diagTitle = 'Moist Static Energy in RAS subcloud layer'
      diagUnits = 'm2/sec2         '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'HSTAR   '
      diagTitle = 'Saturation Moist Static Energy'
      diagUnits = 'm2/sec2         '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'CONT    '
      diagTitle =
     .  'Temperature Tendency due to Convective  Precipitation (K/day)'
      diagUnits = 'K/day           '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'CONQ    '
      diagTitle =
     .  'Moisture Tendency due to Convective Precipitation (g/kg/day)'
      diagUnits = 'g/kg/day        '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'LST     '
      diagTitle =
     .  'Temperature Tendency due to Large Scale Precipitation (K/day)'
      diagUnits = 'K/day           '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'LSQ     '
      diagTitle =
     .  'Moisture Tendency due to Large Scale Precipitation (g/kg/day)'
      diagUnits = 'g/kg/day        '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'CONEVPT '
      diagTitle =
     .  'Temperature Tendency due to Conv Rain Re-evaporation (K/day)'
      diagUnits = 'K/day           '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'CONEVPQ '
      diagTitle =
     .  'Moisture Tendency due to Conv Rain Re-evaporation (g/kg/day)'
      diagUnits = 'g/kg/day        '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'LSEVPT  '
      diagTitle =
     .  'Temperature Tendency due to Large Scale Rain Re-evap (K/day)'
      diagUnits = 'K/day           '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'LSEVPQ  '
      diagTitle =
     .  'Moisture Tendency due to Large Scale Rain Re-evap (g/kg/day)'
      diagUnits = 'g/kg/day        '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'GWCUMID '
      diagTitle =
     .  'Mid-Lev Mean U-Wind for Gravity Wave Drag w/Clouds (m/sec)'
      diagUnits = 'm/sec           '
      WRITE(diagCode,'(A,I3.3,A)') 'UM   ',diagNum+2,'L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'GWCVMID '
      diagTitle =
     .  'Mid-Lev Mean V-Wind for Gravity Wave Drag w/Clouds (m/sec)'
      diagUnits = 'm/sec           '
      WRITE(diagCode,'(A,I3.3,A)') 'VM   ', diagNum ,'L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'CLDSTD  '
      diagTitle =
     . 'Mid-Lev Cloud Perturb Length for Gravity Wave Drag w/Clouds (m)'
      diagUnits = 'm               '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'GWCUBAR '
      diagTitle =
     . 'Mid-Lev Perturb U-Wind for Gravity Wave Drag w/Clouds (m/sec)'
      diagUnits = 'm/sec           '
      WRITE(diagCode,'(A,I3.3,A)') 'UM   ',diagNum+2,'L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'GWCVBAR '
      diagTitle =
     . 'Mid-Lev Perturb V-Wind for Gravity Wave Drag w/Clouds (m/sec)'
      diagUnits = 'm/sec           '
      WRITE(diagCode,'(A,I3.3,A)') 'VM   ', diagNum ,'L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'GWCUS   '
      diagTitle =
     . 'Mid-Lev U-Wind Stress for Gravity Wave Drag w/Clouds (N/m^2)'
      diagUnits = 'N/m2            '
      WRITE(diagCode,'(A,I3.3,A)') 'UM   ',diagNum+2,'L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'GWCVS   '
      diagTitle =
     . 'Mid-Lev V-Wind Stress for Gravity Wave Drag w/Clouds (N/m^2)'
      diagUnits = 'N/m2            '
      WRITE(diagCode,'(A,I3.3,A)') 'VM   ', diagNum ,'L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'GWCU    '
      diagTitle =
     . 'U-Wind Tendency due to Gravity Wave Drag w/Clouds (m/s/day)'
      diagUnits = 'm/sec/day       '
      WRITE(diagCode,'(A,I3.3,A)') 'UM   ',diagNum+2,'ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'GWCV    '
      diagTitle =
     . 'V-Wind Tendency due to Gravity Wave Drag w/Clouds (m/s/day)'
      diagUnits = 'm/sec/day       '
      WRITE(diagCode,'(A,I3.3,A)') 'VM   ', diagNum ,'ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'PLALBEDO'
      diagTitle = 'Planetary Albedo (Fraction)'
      diagUnits = 'Fraction        '
      diagCode = 'SM      L1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName = 'FIZPRES '
      diagTitle = 'Pressures for Fizhi (mb)'
      diagUnits = 'mb  (hPa)       '
      diagCode = 'SM      ML      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'RELHUM  '
      diagTitle = 'Relative Humidity (fraction)'
      diagUnits = 'Fraction        '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
