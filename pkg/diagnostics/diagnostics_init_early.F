C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_init_early.F,v 1.8 2005/06/15 12:58:03 molod Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_INIT_EARLY

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_INIT_EARLY( myThid )

C     !DESCRIPTION:
C     Initialize available diagnostics list: set the following attributes:
C     name (=cdiag), parsing code (=gdiag), units (=udiag), and title (=tdiag)
C     Note: 1) diagnostics defined here also appear in equivalence statement
C           in "DIAGNOSTICS.h" ; this allow a more direct access than 
C           calling S/R FILL_DIAGNOSTICS.
C           2) GDIAG is defined as character*16 and can be to character*1
C           parse(16) with the following codes currently defined:

C     \begin{center}
C       \begin{tabular}[h]{|c|c|}\hline
C         \textbf{Positions}  &  \textbf{Characters}
C         &  \textbf{Meanings} \\\hline
C         parse(1)  &  S  &  scalar \\
C                   &  U  &  vector component in X direction \\
C                   &  V  &  vector component in Y direction \\
C                   &  W  &  vector component in vertical direction \\
C         parse(2)  &  U  &  C-grid U-Point  \\
C                   &  V  &  C-grid V-Point  \\
C                   &  M  &  C-grid Mass Point  \\
C                   &  Z  &  C-grid Corner Point  \\
C         parse(3)  &  --  &  --Unused--  \\
C         parse(4)  &  P  &  positive definite  \\
C         parse(5 ) &  C  &  with counter array  \\
C                   &  D  &  disable an array for output  \\
C         parse(6--8) & '123'  &  3-digit mate number \\
C         parse(9)  &  U  &  model-level plus 1/2  \\
C                   &  M  &  model-level middle  \\
C                   &  L  &  model-level minus 1/2  \\
C         parse(10) &  0  &  levels = 0  \\
C                   &  1  &  levels = 1  \\
C                   &  R  &  levels = Nr  \\
C                   &  L  &  levels = MAX(Nr,NrPhys)  \\
C                   &  M  &  levels = MAX(Nr,NrPhys) - 1  \\
C                   &  G  &  levels = Ground_level Number \\
C                   &  I  &  levels = sea-Ice_level Number \\
C       \end{tabular}
C     \end{center}

C     !USES:
      implicit none
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
      integer myThid
CEOP

      integer n

      do n=1,ndiagMax
        kdiag(n) = 0
        cdiag(n) = '        '
        gdiag(n) = '                '
        udiag(n) = '                '
        tdiag(n) = ' '
      enddo

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     For each output variable,
C     specify Name (cdiag, 8c), Descriptions (tdiag, *c), Units (udiag, 16c) 
C         and parsing code (location on C grid, 2D/3D, ...) (gdiag, 16c)
C----------------------------------------------------------------------
      cdiag( 1)  = 'UFLUX   '
      tdiag( 1)  = 'Zonal Wind Surface Stress  (N/m^2)'
      udiag( 1)  = 'N/m2            '
      gdiag( 1)  = 'UM   002L1      '

      cdiag( 2)  = 'VFLUX   '
      tdiag( 2)  = 'Meridional Wind Surface Stress  (N/m^2)'
      udiag( 2)  = 'N/m2            '
      gdiag( 2)  = 'VM   001L1      '

      cdiag( 3)  = 'HFLUX   '
      tdiag( 3)  = 'Sensible Heat Flux (pos.upwrd)  (W/m^2)'
      udiag( 3)  = 'watt/m2         '
      gdiag( 3)  = 'SM      L1      '

      cdiag( 4)  = 'EFLUX   '
      tdiag( 4)  = 'Latent   Heat Flux (pos.upwrd)  (W/m^2)'
      udiag( 4)  = 'watt/m2         '
      gdiag( 4)  = 'SM      L1      '

      cdiag( 5)  = 'QICE    '
      tdiag( 5)  = 'Heat Conduction through Sea-Ice  (W/m^2)'
      udiag( 5)  = 'watt/m2         '
      gdiag( 5)  = 'SM      L1      '

      cdiag( 6)  = 'RADLWG  '
      tdiag( 6)  = 'Net Upward   Longwave Flux at the Ground  (W/m^2)'
      udiag( 6)  = 'watt/m2         '
      gdiag( 6)  = 'SM      L1      '

      cdiag( 7)  = 'RADSWG  '
      tdiag( 7)  = 'Net Downward Shortwave Flux at the Ground  (W/m^2)'
      udiag( 7)  = 'watt/m2         '
      gdiag( 7)  = 'SM P    L1      '

      cdiag( 8)  = 'RI      '
      tdiag( 8)  = 'Richardson Number  (non-dimensional)'
      udiag( 8)  = 'non-dimensional '
      gdiag( 8)  = 'SM      LL      '

      cdiag( 9)  = 'CT      '
      tdiag( 9)  = 'Surface Drag Coef for Scalars (non-dimensional)'
      udiag( 9)  = 'non-dimensional '
      gdiag( 9)  = 'SM      L1      '

      cdiag(10)  = 'CU      '
      tdiag(10)  = 'Surface Drag Coef for Winds (non-dimensional)'
      udiag(10)  = 'non-dimensional '
      gdiag(10)  = 'SM      L1      '

      cdiag(11)  = 'KM      '
      tdiag(11)  = 'Eddy Diffusivity Coefficient for Momentum  (m^2/s)'
      udiag(11)  = 'm2/sec          '
      gdiag(11)  = 'SM      LL      '

      cdiag(12)  = 'KH      '
      tdiag(12)  = 'Eddy Diffusivity Coefficient for Scalars  (m^2/s)'
      udiag(12)  = 'm2/sec          '
      gdiag(12)  = 'SM      LL      '

      cdiag(13)  = 'TURBU   '
      tdiag(13)  = 'Zonal Wind Tendency from Turbulence  (m/s/day)'
      udiag(13)  = 'm/s/day         '
      gdiag(13)  = 'UM   014ML      '

      cdiag(14)  = 'TURBV   '
      tdiag(14)  = 'Meridional Wind Tendency from Turbulence  (m/s/day)'
      udiag(14)  = 'm/s/day         '
      gdiag(14)  = 'VM   013ML      '

      cdiag(15)  = 'TURBT   '
      tdiag(15)  =
     .  'Temperature Tendency from Turbulence  (K/day)'
      udiag(15)  = 'K/day           '
      gdiag(15)  = 'SM      ML      '

      cdiag(16)  = 'TURBQ   '
      tdiag(16)  =
     .  'Specific Humidity Tendency from Turbulence  (g/kg/day)'
      udiag(16)  = 'gram/kg/day     '
      gdiag(16)  = 'SM      ML      '

      cdiag(17)  = 'MOISTT  '
      tdiag(17)  =
     .  'Temperature Tendency due to Moist Processes  (K/day)'
      udiag(17)  = 'K/day           '
      gdiag(17)  = 'SM      ML      '

      cdiag(18)  = 'MOISTQ  '
      tdiag(18)  =
     .  'Specific Humidity Tendency due to Moist Processes  (g/kg/day)'
      udiag(18)  = 'gram/kg/day     '
      gdiag(18)  = 'SM      ML      '

      cdiag(19)  = 'RADLW   '
      tdiag(19)  =
     .  'Temperature Tendency due to Longwave Radiation  (K/day)'
      udiag(19)  = 'K/day           '
      gdiag(19)  = 'SM      ML      '

      cdiag(20)  = 'RADSW   '
      tdiag(20)  =
     .  'Temperature Tendency due to Shortwave Radiation  (K/day)'
      udiag(20)  = 'K/day           '
      gdiag(20)  = 'SM P    ML      '

      cdiag(21)  = 'PREACC  '
      tdiag(21)  = 'Total Precipitation  (mm/day)'
      udiag(21)  = 'mm/day          '
      gdiag(21)  = 'SM P    L1      '

      cdiag(22)  = 'PRECON  '
      tdiag(22)  = 'Convective Precipitation  (mm/day)'
      udiag(22)  = 'mm/day          '
      gdiag(22)  = 'SM P    L1      '

      cdiag(23)  = 'TUFLUX  '
      tdiag(23)  = 'Turbulent Flux of Zonal Momentum  (N/m^2)'
      udiag(23)  = 'N/m2            '
      gdiag(23)  = 'UM   024LL      '

      cdiag(24)  = 'TVFLUX  '
      tdiag(24)  = 'Turbulent Flux of Meridional Momentum  (N/m^2)'
      udiag(24)  = 'N/m2            '
      gdiag(24)  = 'VM   023LL      '

      cdiag(25)  = 'TTFLUX  '
      tdiag(25)  = 'Turbulent Flux of Sensible Heat  (W/m^2)'
      udiag(25)  = 'watt/m2         '
      gdiag(25)  = 'SM      LL      '

      cdiag(26)  = 'TQFLUX  '
      tdiag(26)  = 'Turbulent Flux of Latent Heat  (W/m^2)'
      udiag(26)  = 'watt/m2         '
      gdiag(26)  = 'SM      LL      '

      cdiag(27)  = 'CN      '
      tdiag(27)  = 'Neutral Drag Coefficient  (non-dimensional)'
      udiag(27)  = 'non-dimensional '
      gdiag(27)  = 'SM      L1      '

      cdiag(28)  = 'WINDS   '
      tdiag(28)  = 'Surface Wind Speed  (m/s)'
      udiag(28)  = 'm/sec           '
      gdiag(28)  = 'SM      L1      '

      cdiag(29)  = 'DTSRF   '
      tdiag(29)  = 'Air/Surface Virtual Temperature Difference  (deg K)'
      udiag(29)  = 'K               '
      gdiag(29)  = 'SM      L1      '

      cdiag(30)  = 'TGROUND '
      tdiag(30)  = 'Ground Temperature  (deg K)'
      udiag(30)  = 'K               '
      gdiag(30)  = 'SM      L1      '

      cdiag(31)  = 'TS      '
      tdiag(31)  = 'Surface Air Temperature  (deg K)'
      udiag(31)  = 'K               '
      gdiag(31)  = 'SM      L1      '

      cdiag(32)  = 'DTG     '
      tdiag(32)  = 'Ground Temperature Adjustment (deg K)'
      udiag(32)  = 'K               '
      gdiag(32)  = 'SM      L1      '

      cdiag(33)  = 'QG      '
      tdiag(33)  = 'Ground Specific Humidity  (g/kg)'
      udiag(33)  = 'gram/kg         '
      gdiag(33)  = 'SM P    L1      '

      cdiag(34)  = 'QS      '
      tdiag(34)  = 'Saturation Surface Specific Humidity  (g/kg)'
      udiag(34)  = 'gram/kg         '
      gdiag(34)  = 'SM P    L1      '

      cdiag(35)  = 'TGRLW   '
      tdiag(35)  = 'Ground Temperature used by LW Radiation  (deg K)'
      udiag(35)  = 'K               '
      gdiag(35)  = 'SM      L1      '

      cdiag(36)  = 'LWGUP   '
      tdiag(36)  = 'Upward Longwave Radiation at the Ground  (W/m^2)'
      udiag(36)  = 'watt/m2         '
      gdiag(36)  = 'SM      L1      '

      cdiag(37)  = 'OLR     '
      tdiag(37)  = 'Outgoing Longwave Radiation   (W/m^2)'
      udiag(37)  = 'watt/m2         '
      gdiag(37)  = 'SM      U1      '

      cdiag(38)  = 'OLRCLR  '
      tdiag(38)  = 'Clear Sky Outgoing Longwave Radiation  (W/m^2)'
      udiag(38)  = 'watt/m2         '
      gdiag(38)  = 'SM      U1      '

      cdiag(39)  = 'LWGCLR  '
      tdiag(39)  = 'Clear Sky Net Longwave Flux at the Ground  (W/m^2)'
      udiag(39)  = 'watt/m2         '
      gdiag(39)  = 'SM      L1      '

      cdiag(40)  = 'LWCLR   '
      tdiag(40)  = 'Clear Sky Longwave Radiation Heating Rates  (K/day)'
      udiag(40)  = 'K/day           '
      gdiag(40)  = 'SM      ML      '

      cdiag(41)  = 'TLW     '
      tdiag(41)  = 'Temperature used by Radiation  (deg K)'
      udiag(41)  = 'K               '
      gdiag(41)  = 'SM      ML      '

      cdiag(42)  = 'SHRAD   '
      tdiag(42)  = 'Specific Humidity used by Radiation  (g/kg)'
      udiag(42)  = 'gram/kg         '
      gdiag(42)  = 'SM P    ML      '

      cdiag(43)  = 'OZLW    '
      tdiag(43)  =
     . 'Ozone mixing ratio used by Longwave Radiation (non-dimensional)'
      udiag(43)  = 'non-dimensional '
      gdiag(43)  = 'SM      ML      '

      cdiag(44)  = 'CLDRAS  '
      tdiag(44)  = 'Convective Cloud Fraction  (0-1)'
      udiag(44)  = '0-1             '
      gdiag(44)  = 'SM P    ML      '

      cdiag(45)  = 'CLDTOT  '
      tdiag(45)  = '3-D Total Cloud Fraction  (0-1)'
      udiag(45)  = '0-1             '
      gdiag(45)  = 'SM P    ML      '

      cdiag(46)  = 'LWGDOWN '
      tdiag(46)  = 'Downward Longwave Radiation at the Ground  (W/m^2)'
      udiag(46)  = 'watt/m2         '
      gdiag(46)  = 'SM      L1      '

      cdiag(47)  = 'GWDT    '
      tdiag(47)  =
     .  'Temperature Tendency due to Gravity Wave Drag  (K/day)'
      udiag(47)  = 'K/day           '
      gdiag(47)  = 'SM      ML      '

      cdiag(48)  = 'RADSWT  '
      tdiag(48)  = 'Incident Shortwave Radiation at TOA (W/m^2)'
      udiag(48)  = 'watt/m2         '
      gdiag(48)  = 'SM P    U1      '

      cdiag(49)  = 'TAUCLD  '
      tdiag(49)  =
     .  'Counted Cloud Optical Depth (non-dimensional) per 100 mb'
      udiag(49)  = 'non-dimensional '
      gdiag(49)  = 'SM  C050ML      '

      cdiag(50)  = 'TAUCLDC '
      tdiag(50)  = 'Cloud Optical Depth Counter'
      udiag(50)  = 'count           '
      gdiag(50)  = 'SM  D   ML      '

      cdiag(51)  = 'CLDLOW  '
      tdiag(51)  = 'Low-Level ( 1000-700 hPa) Cloud Fraction  (0-1)'
      udiag(51)  = '0-1             '
      gdiag(51)  = 'SM P    M1      '

      cdiag(52)  = 'EVAP    '
      tdiag(52)  = 'Surface Evaporation  (mm/day)'
      udiag(52)  = 'mm/day          '
      gdiag(52)  = 'SM      L1      '

      cdiag(53)  = 'DPDT    '
      tdiag(53)  = 'Surface Pressure Tendency  (hPa/day)'
      udiag(53)  = 'hPa/day         '
      gdiag(53)  = 'SM      L1      '

      cdiag(54)  = 'UAVE    '
      tdiag(54)  = 'Average Zonal Wind (m/s)'
      udiag(54)  = 'm/s             '
      gdiag(54)  = 'UU   055MR      '

      cdiag(55)  = 'VAVE    '
      tdiag(55)  = 'Average Meridional Wind (m/s)'
      udiag(55)  = 'm/s             '
      gdiag(55)  = 'VV   054MR      '

      cdiag(56)  = 'TAVE    '
      tdiag(56)  = 'Average Potential Temperature (K)'
      udiag(56)  = 'K               '
      gdiag(56)  = 'SM      MR      '

      cdiag(57)  = 'QAVE    '
      tdiag(57)  =
     .  'Average Specific Humidity (g/kg)'
      udiag(57)  = 'gram/kg         '
      gdiag(57)  = 'SM      MR      '

      cdiag(58)  = 'OMEGA   '
      tdiag(58)  = 'Vertical Velocity  (hPa/day)'
      udiag(58)  = 'hPa/day         '
      gdiag(58)  = 'SM      ML      '

      cdiag(59)  = 'DUDT    '
      tdiag(59)  = 'Total Zonal Wind Tendency  (m/s/day)'
      udiag(59)  = 'm/s/day         '
      gdiag(59)  = 'UU   060ML      '

      cdiag(60)  = 'DVDT    '
      tdiag(60)  = 'Total Meridional Wind Tendency  (m/s/day)'
      udiag(60)  = 'm/s/day         '
      gdiag(60)  = 'VV   059ML      '

      cdiag(61)  = 'DTDT    '
      tdiag(61)  = 'Total Temperature Tendency  (K/day)'
      udiag(61)  = 'K/day           '
      gdiag(61)  = 'SM      ML      '

      cdiag(62)  = 'DQDT    '
      tdiag(62)  = 'Total Specific Humidity Tendency  (g/kg/day)'
      udiag(62)  = 'gram/kg/day     '
      gdiag(62)  = 'SM      ML      '

      cdiag(63)  = 'VORT    '
      tdiag(63)  = 'Relative Vorticity  (x10^-4  sec^-1)'
      udiag(63)  = '(e-4 sec-1)     '
      gdiag(63)  = 'SM      ML      '

      cdiag(64)  = 'PSIGDOT '
      tdiag(64)  = 'Pi-Weighted Vertical Sigma Velocity  (hPa/day)'
      udiag(64)  = 'hPa/day         '
      gdiag(64)  = 'SM      LL      '

      cdiag(65)  = 'DTLS    '
      tdiag(65)  =
     .  'Temperature Tendency due to Stratiform Processes  (K/day)'
      udiag(65)  = 'K/day           '
      gdiag(65)  = 'SM      ML      '

      cdiag(66)  = 'DQLS    '
      tdiag(66)  =
     . 'Specific Humidity Tendency from Stratiform Processes (g/kg/day)'
      udiag(66)  = 'gram/kg/day     '
      gdiag(66)  = 'SM      ML      '

      cdiag(67)  = 'USTAR   '
      tdiag(67)  = 'Friction Velocity  (m/s)'
      udiag(67)  = 'm/sec           '
      gdiag(67)  = 'SM      L1      '

      cdiag(68)  = 'Z0      '
      tdiag(68)  = 'Surface Roughness Z0  (m)'
      udiag(68)  = 'm               '
      gdiag(68)  = 'SM      L1      '

      cdiag(69)  = 'FRQTRB  '
      tdiag(69)  = 'Frequency of Turbulence  (0-1)'
      udiag(69)  = '0-1             '
      gdiag(69)  = 'SM P    MM      '

      cdiag(70)  = 'PBL     '
      tdiag(70)  = 'Estimated PBL Depth  (hPa)'
      udiag(70)  = 'hPa             '
      gdiag(70)  = 'SM P    M1      '

      cdiag(71)  = 'SWCLR   '
      tdiag(71)  = 'Clear Sky Shortwave Heating Rates  (K/day)'
      udiag(71)  = 'K/day           '
      gdiag(71)  = 'SM P    ML      '

      cdiag(72)  = 'OSR     '
      tdiag(72)  = 'Outgoing Shortwave Radiation  (W/m^2)'
      udiag(72)  = 'watt/m2         '
      gdiag(72)  = 'SM P    U1      '

      cdiag(73)  = 'OSRCLR  '
      tdiag(73)  = 'Clear Sky Outgoing Shortwave Radiation  (W/m^2)'
      udiag(73)  = 'watt/m2         '
      gdiag(73)  = 'SM P    U1      '

      cdiag(74)  = 'CLDMAS  '
      tdiag(74)  = 'Cloud Mass Flux  (g/m^2/sec)'
      udiag(74)  = 'g/m2/sec        '
      gdiag(74)  = 'SM P    LL      '

      cdiag(75)  = 'UWND    '
      tdiag(75)  = 'Time-Averaged Zonal Wind  (m/s)'
      udiag(75)  = 'm/sec           '
      gdiag(75)  = 'UU   076ML      '

      cdiag(76)  = 'VWND    '
      tdiag(76)  = 'Time-Averaged Meridional Wind  (m/s)'
      udiag(76)  = 'm/sec           '
      gdiag(76)  = 'VV   075ML      '

      cdiag(77)  = 'TMPU    '
      tdiag(77)  = 'Time-Averaged Temperature  (deg K)'
      udiag(77)  = 'K               '
      gdiag(77)  = 'SM      ML      '

      cdiag(78)  = 'SPHU    '
      tdiag(78)  = 'Time-Averaged Specific Humidity  (g/kg)'
      udiag(78)  = 'gram/kg         '
      gdiag(78)  = 'SM P    ML      '

      cdiag(79)  = 'RFT     '
      tdiag(79)  =
     .  'Temperature Tendency due to Rayleigh Friction (K/day)'
      udiag(79)  = 'K/day           '
      gdiag(79)  = 'SM      ML      '

      cdiag(80)  = 'PS      '
      tdiag(80)  = 'Time-Averaged Surface Pressure  (hPa)'
      udiag(80)  = 'hPa             '
      gdiag(80)  = 'SM      L1      '

      cdiag(81)  = 'TKE     '
      tdiag(81)  = 'Time-Averaged Turbulent Kinetic Energy  (m/s)^2'
      udiag(81)  = '(m/sec)2        '
      gdiag(81)  = 'SM P    LL      '

      cdiag(82)  = 'SWGCLR  '
      tdiag(82)  =
     .  'Clear Sky Net Downward SW Radiation at the Ground  (W/m^2)'
      udiag(82)  = 'watt/m2         '
      gdiag(82)  = 'SM P    L1      '

      cdiag(83)  = 'PAVE    '
      tdiag(83)  = 'Average Surface Pressure (hPa)'
      udiag(83)  = 'hPa/day         '
      gdiag(83)  = 'SM      L1      '

      cdiag(84)  = 'SDIAG1  '
      tdiag(84)  = 'User-Defined Surface   Diagnostic #1'
      udiag(84)  = 'user-defined    '
      gdiag(84)  = 'SM      L1      '

      cdiag(85)  = 'SDIAG2  '
      tdiag(85)  = 'User-Defined Surface   Diagnostic #2'
      udiag(85)  = 'user-defined    '
      gdiag(85)  = 'SM      L1      '

      cdiag(86)  = 'UDIAG1  '
      tdiag(86)  = 'User-Defined Upper-Air Diagnostic #1'
      udiag(86)  = 'user-defined    '
      gdiag(86)  = 'SM      ML      '

      cdiag(87)  = 'UDIAG2  '
      tdiag(87)  = 'User-Defined Upper-Air Diagnostic #2'
      udiag(87)  = 'user-defined    '
      gdiag(87)  = 'SM      ML      '

      cdiag(88)  = 'DIABU   '
      tdiag(88)  =
     .  'Zonal Wind Tendency due to Diabatic Forcing  (m/s/day)'
      udiag(88)  = 'm/s/day         '
      gdiag(88)  = 'UU   089ML      '

      cdiag(89)  = 'DIABV   '
      tdiag(89)  =
     .  'Meridional Wind Tendency due to Diabatic Forcing  (m/s/day)'
      udiag(89)  = 'm/s/day         '
      gdiag(89)  = 'VV   088ML      '

      cdiag(90)  = 'DIABT   '
      tdiag(90)  =
     .  'Temperature Tendency due to Diabatic Forcing  (K/day)'
      udiag(90)  = 'K/day           '
      gdiag(90)  = 'SM      ML      '

      cdiag(91)  = 'DIABQ   '
      tdiag(91)  =
     .  'Specific Humidity Tendency due to Diabatic Forcing  (g/kg/day)'
      udiag(91)  = 'gram/kg/day     '
      gdiag(91)  = 'SM      ML      '

      cdiag(92)  = 'RFU     '
      tdiag(92)  =
     .  'Zonal Wind Tendency due to Rayleigh Friction  (m/s/day)'
      udiag(92)  = 'm/s/day         '
      gdiag(92)  = 'UU   093ML      '

      cdiag(93)  = 'RFV     '
      tdiag(93)  =
     .  'Meridional Wind Tendency due to Rayleigh Friction  (m/s/day)'
      udiag(93)  = 'm/s/day         '
      gdiag(93)  = 'VV   092ML      '

      cdiag(94)  = 'GWDU    '
      tdiag(94)  =
     .  'Zonal Wind Tendency due to Gravity Wave Drag  (m/s/day)'
      udiag(94)  = 'm/s/day         '
      gdiag(94)  = 'UM   095ML      '

      cdiag(95)  = 'GWDV    '
      tdiag(95)  =
     .  'Meridional Wind Tendency due to Gravity Wave Drag  (m/s/day)'
      udiag(95)  = 'm/s/day         '
      gdiag(95)  = 'VM   094ML      '

      cdiag(96)  = 'GWDUS   '
      tdiag(96)  = 'Zonal Wind Gravity Wave Surface Stress (N/m^2)'
      udiag(96)  = 'N/m2            '
      gdiag(96)  = 'UM   097L1      '

      cdiag(97)  = 'GWDVS   '
      tdiag(97)  = 'Meridional Wind Gravity Wave Surface Stress (N/m^2)'
      udiag(97)  = 'N/m2            '
      gdiag(97)  = 'VM   096L1      '

      cdiag(98)  = 'GWDUT   '
      tdiag(98)  = 'Zonal Wind Gravity Wave Stress at Model Top (N/m^2)'
      udiag(98)  = 'N/m2            '
      gdiag(98)  = 'UM   099U1      '

      cdiag(99)  = 'GWDVT   '
      tdiag(99)  =
     .  'Meridional Wind Gravity Wave Stress at Model Top (N/m^2)'
      udiag(99)  = 'N/m2            '
      gdiag(99)  = 'VM   098U1      '

      cdiag(100) = 'LZRAD   '
      tdiag(100) =
     .  'Estimated Cloud Liquid Water used in Radiation (mg/kg)'
      udiag(100) = 'mg/kg           '
      gdiag(100) = 'SM      ML      '

      cdiag(101) = 'SLP     '
      tdiag(101) = 'Time-Averaged Sea-Level Pressure  (mb)'
      udiag(101) = 'hPa             '
      gdiag(101) = 'SM      L1      '

      cdiag(102) = 'VAVEUQ  '
      tdiag(102) = 'Vertically Averaged UWND*SPHU  (m/s g/kg)'
      udiag(102) = 'm/s gram/kg     '
      gdiag(102) = 'UM   103M1      '

      cdiag(103) = 'VAVEVQ  '
      tdiag(103) = 'Vertically Averaged VWND*SPHU  (m/s g/kg)'
      udiag(103) = 'm/s gram/kg     '
      gdiag(103) = 'VM   102M1      '

      cdiag(104) = 'VAVEUT  '
      tdiag(104) = 'Vertically Averaged UWND*TMPU  (m/s deg K)'
      udiag(104) = 'm/s K           '
      gdiag(104) = 'UM   105M1      '

      cdiag(105) = 'VAVEVT  '
      tdiag(105) = 'Vertically Averaged VWND*TMPU  (m/s deg K)'
      udiag(105) = 'm/s K           '
      gdiag(105) = 'VM   104M1      '

      cdiag(106) = 'CLDFRC  '
      tdiag(106) = '2-D Total Cloud Fraction  (0-1)'
      udiag(106) = '0-1             '
      gdiag(106) = 'SM P    M1      '

      cdiag(107) = 'TPW     '
      tdiag(107) = 'Total Precipitable Water  (g/cm^2)'
      udiag(107) = 'gram/cm2        '
      gdiag(107) = 'SM P    M1      '

      cdiag(108) = 'U2M     '
      tdiag(108) = 'Zonal Wind Interpolated to 2 Meters (m/s)'
      udiag(108) = 'm/sec           '
      gdiag(108) = 'UM   109M1      '

      cdiag(109) = 'V2M     '
      tdiag(109) = 'Meridional Wind Interpolated to 2 Meters (m/s)'
      udiag(109) = 'm/sec           '
      gdiag(109) = 'VM   108M1      '

      cdiag(110) = 'T2M     '
      tdiag(110) = 'Temperature Interpolated to 2 Meters (deg K)'
      udiag(110) = 'K               '
      gdiag(110) = 'SM      M1      '

      cdiag(111) = 'Q2M     '
      tdiag(111) = 'Specific Humidity Interpolated to 2 Meters (g/kg)'
      udiag(111) = 'gram/kg         '
      gdiag(111) = 'SM P    M1      '

      cdiag(112) = 'U10M    '
      tdiag(112) = 'Zonal Wind Interpolated to 10 Meters (m/s)'
      udiag(112) = 'm/sec           '
      gdiag(112) = 'UM   113M1      '

      cdiag(113) = 'V10M    '
      tdiag(113) = 'Meridional Wind Interpolated to 10 Meters (m/s)'
      udiag(113) = 'm/sec           '
      gdiag(113) = 'VM   112M1      '

      cdiag(114) = 'T10M    '
      tdiag(114) = 'Temperature Interpolated to 10 Meters (deg K)'
      udiag(114) = 'K               '
      gdiag(114) = 'SM      M1      '

      cdiag(115) = 'Q10M    '
      tdiag(115) = 'Specific Humidity Interpolated to 10 Meters (g/kg)'
      udiag(115) = 'gram/kg         '
      gdiag(115) = 'SM P    M1      '

      cdiag(116) = 'DTRAIN  '
      tdiag(116) = 'Detrainment Cloud Mass Flux  (g/m^2/sec)'
      udiag(116) = 'g/m2/sec        '
      gdiag(116) = 'SM P    LL      '

      cdiag(117) = 'QFILL   '
      tdiag(117) = 'Filling of Negative Specific Humidity (g/kg/day)'
      udiag(117) = 'gram/kg/day     '
      gdiag(117) = 'SM      ML      '

      cdiag(118) = 'VAVEQAVE'
      tdiag(118) = 'Vertically Averaged QAVE  (mm/day)'
      udiag(118) = 'mm/day          '
      gdiag(118) = 'SM      M1      '

      cdiag(119) = 'VAVEQFIL'
      tdiag(119) = 'Vertically Averaged QFILL  (mm/day)'
      udiag(119) = 'mm/day          '
      gdiag(119) = 'SM P    L1      '

c     cdiag(120) = 'SHAPU   '
c     tdiag(120) =
c    .  'Zonal Wind Tendency due to Shapiro Filter  (m/s/day)'
c     udiag(120) = 'm/s/day         '
c     gdiag(120) = 'UU   121ML      '

c     cdiag(121) = 'SHAPV   '
c     tdiag(121) =
c    .  'Meridional Wind Tendency due to Shapiro Filter  (m/s/day)'
c     udiag(121) = 'm/s/day         '
c     gdiag(121) = 'VV   120ML      '

c     cdiag(122) = 'SHAPT   '
c     tdiag(122) = 'Temperature Tendency due to Shapiro Filter  (K/day)'
c     udiag(122) = 'K/day           '
c     gdiag(122) = 'SM      ML      '

c     cdiag(123) = 'SHAPQ   '
c     tdiag(123) =
c    .  'Specific Humidity Tendency due to Shapiro Filter  (g/kg/day)'
c     udiag(123) = 'gram/kg/day     '
c     gdiag(123) = 'SM      ML      '

      cdiag(124) = 'SDIAG3  '
      tdiag(124) = 'User-Defined Surface   Diagnostic #3    '
      udiag(124) = 'user-defined    '
      gdiag(124) = 'SM      L1      '

      cdiag(125) = 'SDIAG4  '
      tdiag(125) = 'User-Defined Surface   Diagnostic #4    '
      udiag(125) = 'user-defined    '
      gdiag(125) = 'SM      L1      '

      cdiag(126) = 'SDIAG5  '
      tdiag(126) = 'User-Defined Surface   Diagnostic #5    '
      udiag(126) = 'user-defined    '
      gdiag(126) = 'SM      L1      '

      cdiag(127) = 'SDIAG6  '
      tdiag(127) = 'User-Defined Surface   Diagnostic #6    '
      udiag(127) = 'user-defined    '
      gdiag(127) = 'SM      L1      '

      cdiag(128) = 'SDIAG7  '
      tdiag(128) = 'User-Defined Surface   Diagnostic #7    '
      udiag(128) = 'user-defined    '
      gdiag(128) = 'SM      L1      '

      cdiag(129) = 'SDIAG8  '
      tdiag(129) = 'User-Defined Surface   Diagnostic #8    '
      udiag(129) = 'user-defined    '
      gdiag(129) = 'SM      L1      '

      cdiag(130) = 'SDIAG9  '
      tdiag(130) = 'User-Defined Surface   Diagnostic #9    '
      udiag(130) = 'user-defined    '
      gdiag(130) = 'SM      L1      '

      cdiag(131) = 'SDIAG10 '
      tdiag(131) = 'User-Defined Surface   Diagnostic #10   '
      udiag(131) = 'user-defined    '
      gdiag(131) = 'SM      L1      '

      cdiag(132) = 'UDIAG3  '
      tdiag(132) = 'User-Defined Upper-Air Diagnostic #3    '
      udiag(132) = 'user-defined    '
      gdiag(132) = 'SM      ML      '

      cdiag(133) = 'UDIAG4  '
      tdiag(133) = 'User-Defined Upper-Air Diagnostic #4    '
      udiag(133) = 'user-defined    '
      gdiag(133) = 'SM      ML      '

      cdiag(134) = 'UDIAG5  '
      tdiag(134) = 'User-Defined Upper-Air Diagnostic #5    '
      udiag(134) = 'user-defined    '
      gdiag(134) = 'SM      ML      '

      cdiag(135) = 'UDIAG6  '
      tdiag(135) = 'User-Defined Upper-Air Diagnostic #6    '
      udiag(135) = 'user-defined    '
      gdiag(135) = 'SM      ML      '

      cdiag(136) = 'UDIAG7  '
      tdiag(136) = 'User-Defined Upper-Air Diagnostic #7    '
      udiag(136) = 'user-defined    '
      gdiag(136) = 'SM      ML      '

      cdiag(137) = 'UDIAG8  '
      tdiag(137) = 'User-Defined Upper-Air Diagnostic #8    '
      udiag(137) = 'user-defined    '
      gdiag(137) = 'SM      ML      '

      cdiag(138) = 'UDIAG9  '
      tdiag(138) = 'User-Defined Upper-Air Diagnostic #9    '
      udiag(138) = 'user-defined    '
      gdiag(138) = 'SM      ML      '

      cdiag(139) = 'UDIAG10 '
      tdiag(139) = 'User-Defined Upper-Air Diagnostic #10   '
      udiag(139) = 'user-defined    '
      gdiag(139) = 'SM      ML      '

      cdiag(140) = 'CLDMID  '
      tdiag(140) = 'Mid-Level (700-400 hPa) Cloud Fraction  (0-1)'
      udiag(140) = '0-1             '
      gdiag(140) = 'SM P    M1      '

      cdiag(141) = 'CLDHI   '
      tdiag(141) = 'High-Level (above 400 hPa) Cloud Fraction  (0-1)'
      udiag(141) = '0-1             '
      gdiag(141) = 'SM P    M1      '

      cdiag(142) = 'TAULOW  '
      tdiag(142) = 'Low-Level (1000-700 hPa) Optical Depth'
      udiag(142) = 'non-dimensional '
      gdiag(142) = 'SM PC150M1      '

      cdiag(143) = 'TAUMID  '
      tdiag(143) = 'Mid-Level (700-400 hPa) Optical Depth'
      udiag(143) = 'non-dimensional '
      gdiag(143) = 'SM PC151M1      '

      cdiag(144) = 'TAUHI   '
      tdiag(144) = 'High-Level (above 400 hPa) Optical Depth'
      udiag(144) = 'non-dimensional '
      gdiag(144) = 'SM PC152M1      '

      cdiag(145) = 'CLDNP   '
      tdiag(145) = 'Non-Precipitating Cloud Fraction  (0-1)'
      udiag(145) = '0-1             '
      gdiag(145) = 'SM      ML      '

      cdiag(146) = 'CLDPRS  '
      tdiag(146) = 'Cloud Top Pressure (when cloudy)  (hPa)'
      udiag(146) = 'hPa             '
      gdiag(146) = 'SM PC147M1      '

      cdiag(147) = 'CTPCNT  '
      tdiag(147) = 'Cloud Top Pressure Counter'
      udiag(147) = 'count           '
      gdiag(147) = 'SM  D   M1      '

      cdiag(148) = 'CLDTMP  '
      tdiag(148) = 'Cloud Top Temperature (when cloudy)  (deg K)'
      udiag(148) = 'K               '
      gdiag(148) = 'SM PC149M1      '

      cdiag(149) = 'CTTCNT  '
      tdiag(149) = 'Cloud Top Temperature Counter'
      udiag(149) = 'count           '
      gdiag(149) = 'SM  D   M1      '

      cdiag(150) = 'TAULOWC '
      tdiag(150) = 'Low-Level Optical Depth Counter'
      udiag(150) = 'count           '
      gdiag(150) = 'SM  D   M1      '

      cdiag(151) = 'TAUMIDC '
      tdiag(151) = 'Mid-Level Optical Depth Counter'
      udiag(151) = 'count           '
      gdiag(151) = 'SM  D   M1      '

      cdiag(152) = 'TAUHIC  '
      tdiag(152) = 'High-Level Optical Depth Counter'
      udiag(152) = 'count           '
      gdiag(152) = 'SM  D   M1      '

      cdiag(153) = 'TCANOPY '
      tdiag(153) =
     .  'Skin Temperature of the Canopy/Soil (SST over water)  (deg K)'
      udiag(153) = 'K               '
      gdiag(153) = 'SM      L1      '

      cdiag(154) = 'TDEEP   '
      tdiag(154) = 'Deep Soil Temperature  (deg K)'
      udiag(154) = 'K               '
      gdiag(154) = 'SM      L1      '

      cdiag(155) = 'QCANOPY '
      tdiag(155) = 'Specific Humidity of the Canopy  (g/kg)'
      udiag(155) = 'gram/kg         '
      gdiag(155) = 'SM      L1      '

      cdiag(156) = 'SMSHAL  '
      tdiag(156) =
     .  'Soil Moisture Field Capacity Fraction in Shallow (5 mm) Layer'
      udiag(156) = '0-1             '
      gdiag(156) = 'SM      L1      '

      cdiag(157) = 'SMROOT  '
      tdiag(157) = 'Soil Moisture Field Capacity Fraction in Root Zone'
      udiag(157) = '0-1             '
      gdiag(157) = 'SM      L1      '

      cdiag(158) = 'SMDEEP  '
      tdiag(158) =
     .  'Soil Moisture Field Capacity Fraction in Recharge Layer '
      udiag(158) = '0-1             '
      gdiag(158) = 'SM      L1      '

      cdiag(159) = 'CAPACITY'
      tdiag(159) = 'Canopy Interception Reservoir (mm)'
      udiag(159) = 'mm              '
      gdiag(159) = 'SM      L1      '

      cdiag(160) = 'SNOW    '
      tdiag(160) = 'Snow Depth (mm water equivalent)'
      udiag(160) = 'mm water equiv  '
      gdiag(160) = 'SM      L1      '

      cdiag(161) = 'RAINCON '
      tdiag(161) = 'Convective Rainfall (liquid precipitate)  (mm/day)'
      udiag(161) = 'mm/day          '
      gdiag(161) = 'SM      L1      '

      cdiag(162) = 'RAINLSP '
      tdiag(162) = 'Large-scale Rainfall (liquid precipitate)  (mm/day)'
      udiag(162) = 'mm/day          '
      gdiag(162) = 'SM      L1      '

      cdiag(163) = 'SNOWFALL'
      tdiag(163) = 'Total Snowfall (solid precipitate)  (mm/day)'
      udiag(163) = 'mm/day          '
      gdiag(163) = 'SM P    L1      '

      cdiag(164) = 'RUNOFF  '
      tdiag(164) =
     .  'Water from precipitation not infiltrated into soil  (mm/s)'
      udiag(164) = 'mm/sec          '
      gdiag(164) = 'SM      L1      '

      cdiag(165) = 'FWSOIL  '
      tdiag(165) = 'Rainwater Infiltration into top soil layer  (mm/s)'
      udiag(165) = 'mm/sec          '
      gdiag(165) = 'SM      L1      '

      cdiag(166) = 'GDRAIN  '
      tdiag(166) =
     .  'Diffusion of Moisture across bottom of root zone  (mm/s)'
      udiag(166) = 'mm/sec          '
      gdiag(166) = 'SM      L1      '

      cdiag(167) = 'SNOWMELT'
      tdiag(167) = 'Rate of Snow Melt  (mm/s)'
      udiag(167) = 'mm/sec          '
      gdiag(167) = 'SM      L1      '

      cdiag(168) = 'ERESV   '
      tdiag(168) =
     .  'Latent Heat Flux from Canopy Interception Reservoir  (W/m^2)'
      udiag(168) = 'watt/m2         '
      gdiag(168) = 'SM      L1      '

      cdiag(169) = 'ESOIL   '
      tdiag(169) = 'Latent Heat Flux from Bare Soil  (W/m^2)'
      udiag(169) = 'watt/m2         '
      gdiag(169) = 'SM      L1      '

      cdiag(170) = 'EVEG    '
      tdiag(170) =
     . 'Latent Heat Flux (Transpiration) from Vegetated Surface (W/m^2)'
      udiag(170) = 'watt/m2         '
      gdiag(170) = 'SM      L1      '

      cdiag(171) = 'ESNOW   '
      tdiag(171) = 'Latent Heat Flux from Snow Pack  (W/m^2)'
      udiag(171) = 'watt/m2         '
      gdiag(171) = 'SM      L1      '

      cdiag(172) = 'PARDF   '
      tdiag(172) =
     .  'Diffuse-beam Photosynthetically Active Radiation  (W/m^2)'
      udiag(172) = 'watt/m2         '
      gdiag(172) = 'SM P    L1      '

      cdiag(173) = 'PARDR   '
      tdiag(173) =
     .  'Direct-beam Photosynthetically Active Radiation  (W/m^2)'
      udiag(173) = 'watt/m2         '
      gdiag(173) = 'SM P    L1      '

      cdiag(174) = 'LAI     '
      tdiag(174) = 'Leaf Area Index  (%)'
      udiag(174) = 'percent         '
      gdiag(174) = 'SM      L1      '

      cdiag(175) = 'GREEN   '
      tdiag(175) = 'Green-ness Index  (%)'
      udiag(175) = 'percent         '
      gdiag(175) = 'SM      L1      '

      cdiag(176) = 'DLWDTC  '
      tdiag(176) =
     .  'Derivative of Upward LW Radiation wrt TCANOPY (W/m^2 /deg K)'
      udiag(176) = 'watt/m2/K       '
      gdiag(176) = 'SM      L1      '

      cdiag(177) = 'DHDTC   '
      tdiag(177) =
     .  'Derivative of Sensible Heat Flux wrt TCANOPY (W/m^2 /deg K)'
      udiag(177) = 'watt/m2/K       '
      gdiag(177) = 'SM      L1      '

      cdiag(178) = 'DEDTC   '
      tdiag(178) =
     .  'Derivative of Latent Heat Flux wrt TCANOPY (W/m^2 /deg K)'
      udiag(178) = 'watt/m2/K       '
      gdiag(178) = 'SM      L1      '

      cdiag(179) = 'VDTMOIST'
      tdiag(179) = 'Vertically Averaged MOISTT  (K/day)'
      udiag(179) = 'K/day           '
      gdiag(179) = 'SM      M1      '

      cdiag(180) = 'VDTTURB '
      tdiag(180) = 'Vertically Averaged TURBT  (K/day)'
      udiag(180) = 'K/day           '
      gdiag(180) = 'SM      M1      '

      cdiag(181) = 'VDTLWRAD'
      tdiag(181) = 'Vertically Averaged RADLW  (K/day)'
      udiag(181) = 'K/day           '
      gdiag(181) = 'SM      M1      '

      cdiag(182) = 'VDTSWRAD'
      tdiag(182) = 'Vertically Averaged RADSW  (K/day)'
      udiag(182) = 'K/day           '
      gdiag(182) = 'SM P    M1      '

      cdiag(183) = 'VAVETAVE'
      tdiag(183) = 'Vertically Averaged TAVE  (K/day)'
      udiag(183) = 'K/day           '
      gdiag(183) = 'SM      M1      '

      cdiag(184) = 'PSUBCLD '
      tdiag(184) = 'Pressure Thickness of the RAS Subcloud Layer (mb)'
      udiag(184) = 'hPa             '
      gdiag(184) = 'SM PC185M1      '

      cdiag(185) = 'PSUBCLDC'
      tdiag(185) =
     .  'Pressure Thickness of the RAS Subcloud Layer Counter'
      udiag(185) = 'count           '
      gdiag(185) = 'SM PD   M1      '

      cdiag(186) = 'LCL     '
      tdiag(186) = 'Lifting Condensation Level (mb)                 '
      udiag(186) = 'hPa             '
      gdiag(186) = 'SM      L1      '

      cdiag(187) = 'SDIAGC  '
      tdiag(187) = 'User-Defined Counted Surface Diagnostic         '
      udiag(187) = 'user-defined    '
      gdiag(187) = 'SM  C188L1      '

      cdiag(188) = 'SDIAGCC '
      tdiag(188) = 'User-Defined Counted Surface Diagnostic Counter'
      udiag(188) = 'count           '
      gdiag(188) = 'SM  D   L1      '

      cdiag(189) = '        '
      tdiag(189) = '                         '
      udiag(189) = '                '
      gdiag(189) = '         0      '

      cdiag(190) = 'EVPOT   '
      tdiag(190) = 'Potential Evapotranspiration (W/m^2)'
      udiag(190) = 'watt/m2         '
      gdiag(190) = 'SM      L1      '

      cdiag(191) = 'RHCHECK '
      tdiag(191) =
     . 'Net precip due to supersaturation above critical RH  (mm/day)'
      udiag(191) = 'mm/day          '
      gdiag(191) = 'SM P    L1      '

      cdiag(192) = 'DHDQA   '
      tdiag(192) =
     .  'Derivative of Sensible Heat wrt Canopy Humidity (W/m^2)'
      udiag(192) = 'watt/m2         '
      gdiag(192) = 'SM      L1      '

      cdiag(193) = 'DEDQA   '
      tdiag(193) =
     .  'Derivative of Latent Heat wrt Canopy Humidity (W/m^2)'
      udiag(193) = 'watt/m2         '
      gdiag(193) = 'SM      L1      '

      cdiag(194) = 'DTC     '
      tdiag(194) = 'Total Change in Canopy Temperature (deg/sec)'
      udiag(194) = 'K/sec           '
      gdiag(194) = 'SM      L1      '

      cdiag(195) = 'DQC     '
      tdiag(195) = 'Change in Canopy Humidity (kg/kg/sec)'
      udiag(195) = 'sec-1           '
      gdiag(195) = 'SM      L1      '

      cdiag(196) = 'TCDTC   '
      tdiag(196) =
     .  'Change in Canopy Temperature - Energy Term (deg/sec)'
      udiag(196) = 'K/sec           '
      gdiag(196) = 'SM      L1      '

      cdiag(197) = 'RADDTC  '
      tdiag(197) =
     .  'Change in Canopy Temperature - Net Radiation Term (deg/sec)'
      udiag(197) = 'K/sec           '
      gdiag(197) = 'SM      L1      '

      cdiag(198) = 'SENDDTC '
      tdiag(198) =
     .  'Change in Canopy Temperature - Sensible Heat Term (deg/sec)'
      udiag(198) = 'K/sec           '
      gdiag(198) = 'SM      L1      '

      cdiag(199) = 'LATDTC  '
      tdiag(199) =
     .  'Change in Canopy Temperature - Latent Heat Term (deg/sec)'
      udiag(199) = 'K/sec           '
      gdiag(199) = 'SM      L1      '

      cdiag(200) = 'TDDTC   '
      tdiag(200) =
     .  'Change in Canopy Temperature - Ground Temp Term (deg/sec)'
      udiag(200) = 'K/sec           '
      gdiag(200) = 'SM      L1      '

      cdiag(201) = 'QCDTC   '
      tdiag(201) =
     .  'Change in Canopy Temperature - Humidity Term (deg/sec)'
      udiag(201) = 'K/sec           '
      gdiag(201) = 'SM      L1      '

      cdiag(202) = 'TRBQLIQ '
      tdiag(202) = 'Cloud Liquid Water Generated in Turbulence (mg/kg)'
      udiag(202) = 'mg/kg           '
      gdiag(202) = 'SM      ML      '

      cdiag(203) = 'TRBFCC  '
      tdiag(203) = 'Cloud Fraction Generated in Turbulence (fraction)'
      udiag(203) = '0-1             '
      gdiag(203) = 'SM      ML      '

      cdiag(204) = 'ALBEDO  '
      tdiag(204) = 'Mean Surface Albedo (SW_Up/SW_Down)  0-1 '
      udiag(204) = '0-1             '
      gdiag(204) = 'SM  C210L1      '

      cdiag(205) = 'ALBVISDR'
      tdiag(205) = 'Direct  Beam VIS Surface Albedo (0-1)   '
      udiag(205) = '0-1             '
      gdiag(205) = 'SM      L1      '

      cdiag(206) = 'ALBVISDF'
      tdiag(206) = 'Diffuse Beam VIS Surface Albedo (0-1)   '
      udiag(206) = '0-1             '
      gdiag(206) = 'SM      L1      '

      cdiag(207) = 'ALBNIRDR'
      tdiag(207) = 'Direct  Beam NIR Surface Albedo (0-1)   '
      udiag(207) = '0-1             '
      gdiag(207) = 'SM      L1      '

      cdiag(208) = 'ALBNIRDF'
      tdiag(208) = 'Diffuse Beam NIR Surface Albedo (0-1)   '
      udiag(208) = '0-1             '
      gdiag(208) = 'SM      L1      '

      cdiag(209) = 'TAUAVE  '
      tdiag(209) =
     .  'Averaged Cloud Optical Depth (non-dimensional) per 100 mb'
      udiag(209) = 'non-dimensional '
      gdiag(209) = 'SM      ML      '

      cdiag(210) = 'ALBEDOC '
      tdiag(210) = 'Surface Albedo Counter'
      udiag(210) = 'count           '
      gdiag(210) = 'SM  D   L1      '

      cdiag(211) = 'RHCHECKT'
      tdiag(211) =
     . 'Temperature Change due to humidity above critical RH  (K/day)'
      udiag(211) = 'K/day           '
      gdiag(211) = 'SM      ML      '

      cdiag(212) = 'RHCHECKQ'
      tdiag(212) =
     . 'Spec.Humid. Change due to humidity above critical RH (g/kg/day)'
      udiag(212) = 'g/kg/day        '
      gdiag(212) = 'SM      ML      '

      cdiag(213) = 'CLDLSP  '
      tdiag(213) = 'Large-Scale Cloud Fraction  (0-1)'
      udiag(213) = '0-1             '
      gdiag(213) = 'SM P    ML      '

      cdiag(214) = 'LZLOW   '
      tdiag(214) =
     .  'Vert integrated low-level (below 700mb) cloud water (mg/cm**2)'
      udiag(214) = 'mg/cm2          '
      gdiag(214) = 'SM P    L1      '

      cdiag(215) = 'LZMID   '
      tdiag(215) =
     .  'Vert integrated mid-level (700-400mb) cloud water (mg/cm**2)'
      udiag(215) = 'mg/cm2          '
      gdiag(215) = 'SM P    L1      '

      cdiag(216) = 'LZHI    '
      tdiag(216) =
     . 'Vert integrated high-level (above 400mb) cloud water (mg/cm**2)'
      udiag(216) = 'mg/cm2          '
      gdiag(216) = 'SM P    L1      '

      cdiag(217) = 'LZTOT   '
      tdiag(217) =
     .  'Vertically integrated total suspended cloud water (mg/cm**2)'
      udiag(217) = 'mg/cm2          '
      gdiag(217) = 'SM P    L1      '

      cdiag(218) = 'CAPE    '
      tdiag(218) = 'Convective Available Potential Energy'
      udiag(218) = 'mb m2/sec2      '
      gdiag(218) = 'SM P    L1      '

      cdiag(219) = 'HSUBCL  '
      tdiag(219) = 'Moist Static Energy in RAS subcloud layer'
      udiag(219) = 'm2/sec2         '
      gdiag(219) = 'SM P    L1      '

      cdiag(220) = 'HSTAR   '
      tdiag(220) = 'Saturation Moist Static Energy'
      udiag(220) = 'm2/sec2         '
      gdiag(220) = 'SM P    ML      '

      cdiag(221) = 'CONT    '
      tdiag(221) =
     .  'Temperature Tendency due to Convective  Precipitation (K/day)'
      udiag(221) = 'K/day           '
      gdiag(221) = 'SM      ML      '

      cdiag(222) = 'CONQ    '
      tdiag(222) =
     .  'Moisture Tendency due to Convective Precipitation (g/kg/day)'
      udiag(222) = 'g/kg/day        '
      gdiag(222) = 'SM      ML      '

      cdiag(223) = 'LST     '
      tdiag(223) =
     .  'Temperature Tendency due to Large Scale Precipitation (K/day)'
      udiag(223) = 'K/day           '
      gdiag(223) = 'SM      ML      '

      cdiag(224) = 'LSQ     '
      tdiag(224) =
     .  'Moisture Tendency due to Large Scale Precipitation (g/kg/day)'
      udiag(224) = 'g/kg/day        '
      gdiag(224) = 'SM      ML      '

      cdiag(225) = 'CONEVPT '
      tdiag(225) =
     .  'Temperature Tendency due to Conv Rain Re-evaporation (K/day)'
      udiag(225) = 'K/day           '
      gdiag(225) = 'SM      ML      '

      cdiag(226) = 'CONEVPQ '
      tdiag(226) =
     .  'Moisture Tendency due to Conv Rain Re-evaporation (g/kg/day)'
      udiag(226) = 'g/kg/day        '
      gdiag(226) = 'SM      ML      '

      cdiag(227) = 'LSEVPT  '
      tdiag(227) =
     .  'Temperature Tendency due to Large Scale Rain Re-evap (K/day)'
      udiag(227) = 'K/day           '
      gdiag(227) = 'SM      ML      '

      cdiag(228) = 'LSEVPQ  '
      tdiag(228) =
     .  'Moisture Tendency due to Large Scale Rain Re-evap (g/kg/day)'
      udiag(228) = 'g/kg/day        '
      gdiag(228) = 'SM      ML      '

      cdiag(229) = 'GWCUMID '
      tdiag(229) =
     .  'Mid-Lev Mean U-Wind for Gravity Wave Drag w/Clouds (m/sec)'
      udiag(229) = 'm/sec           '
      gdiag(229) = 'UM   230L1      '

      cdiag(230) = 'GWCVMID '
      tdiag(230) =
     .  'Mid-Lev Mean V-Wind for Gravity Wave Drag w/Clouds (m/sec)'
      udiag(230) = 'm/sec           '
      gdiag(230) = 'VM   229L1      '

      cdiag(231) = 'CLDSTD  '
      tdiag(231) =
     . 'Mid-Lev Cloud Perturb Length for Gravity Wave Drag w/Clouds (m)'
      udiag(231) = 'm               '
      gdiag(231) = 'SM      L1      '

      cdiag(232) = 'GWCUBAR '
      tdiag(232) =
     . 'Mid-Lev Perturb U-Wind for Gravity Wave Drag w/Clouds (m/sec)'
      udiag(232) = 'm/sec           '
      gdiag(232) = 'UM   233L1      '

      cdiag(233) = 'GWCVBAR '
      tdiag(233) =
     . 'Mid-Lev Perturb V-Wind for Gravity Wave Drag w/Clouds (m/sec)'
      udiag(233) = 'm/sec           '
      gdiag(233) = 'VM   232L1      '

      cdiag(234) = 'GWCUS   '
      tdiag(234) =
     . 'Mid-Lev U-Wind Stress for Gravity Wave Drag w/Clouds (N/m^2)'
      udiag(234) = 'N/m2            '
      gdiag(234) = 'UM   235L1      '

      cdiag(235) = 'GWCVS   '
      tdiag(235) =
     . 'Mid-Lev V-Wind Stress for Gravity Wave Drag w/Clouds (N/m^2)'
      udiag(235) = 'N/m2            '
      gdiag(235) = 'VM   234L1      '

      cdiag(236) = 'GWCU    '
      tdiag(236) =
     . 'U-Wind Tendency due to Gravity Wave Drag w/Clouds (m/s/day)'
      udiag(236) = 'm/sec/day       '
      gdiag(236) = 'UM   237ML      '

      cdiag(237) = 'GWCV    '
      tdiag(237) =
     . 'V-Wind Tendency due to Gravity Wave Drag w/Clouds (m/s/day)'
      udiag(237) = 'm/sec/day       '
      gdiag(237) = 'VM   236ML      '

      cdiag(238) = 'PLALBEDO'
      tdiag(238) ='Planetary Albedo (Fraction)'
      udiag(238) = 'Fraction        '
      gdiag(238) = 'SM      L1      '

c-    set the total number of available diagnostics 
      ndiagt = 238

c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
