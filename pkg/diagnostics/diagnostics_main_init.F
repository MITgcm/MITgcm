C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_main_init.F,v 1.20 2005/08/16 21:29:38 molod Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_MAIN_INIT

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_MAIN_INIT( myThid )

C     !DESCRIPTION:
C     Initialize available diagnostics list for variables of the main code
C     (not part of a package): set the following attributes:
C     name (=cdiag), parsing code (=gdiag), units (=udiag), and title (=tdiag)
C     Notes: 1) diagnostics defined here do not require any EQUIVALENCE
C            since they get filled-in with S/R FILL_DIAGNOSTICS
C            2) GDIAG is defined as character*16 and can be to character*1
C            parse(16) with the following codes currently defined:

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
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER       diagNum
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle
      CHARACTER*2   rUnit2c
      CHARACTER*4   tUnit4c, sUnit4c

      CHARACTER*(16) DIAGS_MK_UNITS
      EXTERNAL DIAGS_MK_UNITS

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     For each output variable,
C     specify Name (cdiag, 8c), Descriptions (tdiag, *c), Units (udiag, 16c) 
C         and Type/Parms (location on C grid, 2D/3D, ...) (gdiag, 16c)
C----------------------------------------------------------------------

      IF ( usingPCoords ) THEN
        rUnit2c= 'Pa'
      ELSE
        rUnit2c= 'm '
      ENDIF
      IF ( fluidIsAir ) THEN
        tUnit4c= 'K   '
        sUnit4c= 'g/kg'
#ifdef ALLOW_FIZHI
      IF(useFIZHI)THEN
        sUnit4c= 'kg/kg'
      ENDIF
#endif /* ALLOW_FIZHI */
      ELSE
        tUnit4c= 'degC'
        sUnit4c= 'psu '
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-    state variables of the main code (and related quadratic var):

      diagName  = 'ETAN    '
      diagTitle = 'Perturbation of Surface (pressure, height) (Pa,m)'
      diagUnits = DIAGS_MK_UNITS( rUnit2c, myThid )
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'ETANSQ  '
      diagTitle = 'Square of Perturbation of Sfc (Pa^2,m^2)'
      diagUnits = DIAGS_MK_UNITS( rUnit2c//'^2', myThid )
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DETADT2 '
      diagTitle = 'Square of Eta (Surf.P,SSH) Tendency ((r_unit/s)^2)'
      diagUnits = DIAGS_MK_UNITS( rUnit2c//'^2/s^2', myThid )
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'THETA   '
      diagTitle = 'Potential Temperature (degC,K)'
      diagUnits = DIAGS_MK_UNITS( tUnit4c, myThid )
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'SST     '
      diagTitle = 'Sea Surface Temperature (degC,K)'
      diagUnits = DIAGS_MK_UNITS( tUnit4c, myThid )
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'SALT    '
      diagTitle = 'Salt or Water Vapor mixing ratio'
      diagUnits = DIAGS_MK_UNITS( sUnit4c, myThid )
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'RELHUM  '
      diagTitle = 'Relative Humidity               '
      diagUnits = 'percent         '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'SSS     '
      diagTitle = 'Sea Surface Salinity '
      diagUnits = DIAGS_MK_UNITS( sUnit4c, myThid )
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'SALTanom'
      diagTitle = 'Salt anomaly (=SALT-35)'
      diagUnits = DIAGS_MK_UNITS( sUnit4c, myThid )
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'UVEL    '
      diagTitle = 'Zonal Component of Velocity (m/s)'
      diagUnits = 'm/s             '
      WRITE(diagCode,'(A,I3.3,A)') 'UU   ',diagNum+2,'MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'VVEL    '
      diagTitle = 'Meridional Component of Velocity (m/s)'
      diagUnits = 'm/s             '
      WRITE(diagCode,'(A,I3.3,A)') 'VV   ', diagNum ,'MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'UVEL_k2 '
      diagTitle = 'Zonal Component of Velocity at level 2 (m/s)'
      diagUnits = 'm/s             '
      WRITE(diagCode,'(A,I3.3,A)') 'UU   ',diagNum+2,'M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'VVEL_k2 '
      diagTitle = 'Meridional Component of Velocity at level 2 (m/s)'
      diagUnits = 'm/s             '
      WRITE(diagCode,'(A,I3.3,A)') 'VV   ', diagNum ,'M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'WVEL    '
      diagTitle = 'Vertical Component of Velocity (r_units/s)'
      diagUnits = DIAGS_MK_UNITS( rUnit2c//'/s', myThid )
      diagCode  = 'WM      LR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'THETASQ '
      diagTitle = 'Square of Potential Temperature (K^2)'
      diagUnits = DIAGS_MK_UNITS( tUnit4c//'^2', myThid )
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'SALTSQ  '
      diagTitle = 'Square of Salt or Water Vapor mix rat (g^2/kg^2)'
      diagUnits = DIAGS_MK_UNITS( '('//sUnit4c//')^2', myThid )
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'SALTSQan'
      diagTitle = 'Square of Salt anomaly (=(SALT-35)^2 (g^2/kg^2)'
      diagUnits = DIAGS_MK_UNITS( '('//sUnit4c//')^2', myThid )
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'UVELSQ  '
      diagTitle = 'Square of Zonal Comp of Velocity (m^2/s^2)'
      diagUnits = 'm^2/s^2         '
      WRITE(diagCode,'(A,I3.3,A)') 'UU   ',diagNum+2,'MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'VVELSQ  '
      diagTitle = 'Square of Meridional Comp of Velocity (m^2/s^2)'
      diagUnits = 'm^2/s^2         '
      WRITE(diagCode,'(A,I3.3,A)') 'VV   ', diagNum ,'MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'WVELSQ  '
      diagTitle = 'Square of Vertical Comp of Velocity (m^2/s^2)'
      diagUnits = DIAGS_MK_UNITS( rUnit2c//'^2/s^2', myThid )
      diagCode  = 'WM      LR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'UV_VEL_C'
      diagTitle ='Product of horizontal Comp of velocity (cell center)'
      diagUnits = 'm^2/s^2         '
      WRITE(diagCode,'(A,I3.3,A)') 'UM   ', diagNum+1 ,'MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'UV_VEL_Z'
      diagTitle = 'Meridional Transport of Zonal Momentum (m^2/s^2)'
      diagUnits = 'm^2/s^2         '
      WRITE(diagCode,'(A,I3.3,A)') 'UZ   ', diagNum+1 ,'MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'WU_VEL  '
      diagTitle = 'Vertical Transport of Zonal Momentum (m^2/s^2)'
      diagUnits = DIAGS_MK_UNITS( 'm.'//rUnit2c//'/s^2', myThid )
      diagCode  = 'WU      LR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'WV_VEL  '
      diagTitle ='Vertical Transport of Meridional Momentum (m^2/s^2)'
      diagUnits = DIAGS_MK_UNITS( 'm.'//rUnit2c//'/s^2', myThid )
      diagCode  = 'WV      LR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'UVELMASS'
      diagTitle = 'Zonal Mass-Weighted Comp of Velocity (m/s)'
      diagUnits = 'm/s             '
      WRITE(diagCode,'(A,I3.3,A)') 'UU   ',diagNum+2,'MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'VVELMASS'
      diagTitle = 'Meridional Mass-Weighted Comp of Velocity (m/s)'
      diagUnits = 'm/s             '
      WRITE(diagCode,'(A,I3.3,A)') 'VV   ', diagNum ,'MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'WVELMASS'
      diagTitle = 'Vertical Mass-Weighted Comp of Velocity (m/s)'
      diagUnits = DIAGS_MK_UNITS( rUnit2c//'/s', myThid )
      diagCode  = 'WM      LR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'UTHMASS '
      diagTitle = 'Zonal Mass-Weight Transp of Pot Temp (K.m/s)'
      diagUnits = DIAGS_MK_UNITS( tUnit4c//'.m/s', myThid )
      WRITE(diagCode,'(A,I3.3,A)') 'UU   ',diagNum+2,'MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'VTHMASS '
      diagTitle = 'Meridional Mass-Weight Transp of Pot Temp (K.m/s)'
      diagUnits = DIAGS_MK_UNITS( tUnit4c//'.m/s', myThid )
      WRITE(diagCode,'(A,I3.3,A)') 'VV   ', diagNum ,'MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'WTHMASS '
      diagTitle = 'Vertical Mass-Weight Transp of Pot Temp (K.m/s)'
      diagUnits = DIAGS_MK_UNITS(tUnit4c//'.'//rUnit2c//'/s', myThid )
      diagCode  = 'WM      LR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'USLTMASS'
      diagTitle = 'Zonal Mass-Weight Transp of Salt '
      diagUnits = DIAGS_MK_UNITS(sUnit4c//'.m/s', myThid )
      WRITE(diagCode,'(A,I3.3,A)') 'UU   ',diagNum+2,'MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'VSLTMASS'
      diagTitle = 'Meridional Mass-Weight Transp of Salt'
      diagUnits = DIAGS_MK_UNITS(sUnit4c//'.m/s', myThid )
      WRITE(diagCode,'(A,I3.3,A)') 'VV   ', diagNum ,'MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'WSLTMASS'
      diagTitle = 'Vertical Mass-Weight Transp of Salt '
      diagUnits = DIAGS_MK_UNITS(sUnit4c//'.'//rUnit2c//'/s', myThid )
      diagCode  = 'WM      LR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'UVELTH  '
      diagTitle = 'Zonal Transp of Pot Temp (K.m/s)'
      diagUnits = DIAGS_MK_UNITS( tUnit4c//'.m/s', myThid )
      WRITE(diagCode,'(A,I3.3,A)') 'UU   ',diagNum+2,'MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'VVELTH  '
      diagTitle = 'Meridional Transp of Pot Temp (K.m/s)'
      diagUnits = DIAGS_MK_UNITS( tUnit4c//'.m/s', myThid )
      WRITE(diagCode,'(A,I3.3,A)') 'VV   ', diagNum ,'MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'WVELTH  '
      diagTitle = 'Vertical Transp of Pot Temp (K.m/s)'
      diagUnits = DIAGS_MK_UNITS(tUnit4c//'.'//rUnit2c//'/s', myThid )
      diagCode  = 'WM      LR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'UVELSLT '
      diagTitle = 'Zonal Transp of Salt '
      diagUnits = DIAGS_MK_UNITS( sUnit4c//'.m/s', myThid )
      WRITE(diagCode,'(A,I3.3,A)') 'UU   ',diagNum+2,'MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'VVELSLT '
      diagTitle = 'Meridional Transp of Salt '
      diagUnits = DIAGS_MK_UNITS( sUnit4c//'.m/s', myThid )
      WRITE(diagCode,'(A,I3.3,A)') 'VV   ', diagNum ,'MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'WVELSLT '
      diagTitle = 'Vertical Transp of Salt '
      diagUnits = DIAGS_MK_UNITS(sUnit4c//'.'//rUnit2c//'/s', myThid )
      diagCode  = 'WM      LR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      diagName  = 'RHOAnoma'
      diagTitle = 'Density Anomaly (=Rho-rhoConst)'
      diagUnits = 'kg/m^3          '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'RHOANOSQ'
      diagTitle = 'Square of Density Anomaly (=(Rho-rhoConst)^2)'
      diagUnits = 'kg^2/m^6          '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'URHOMASS'
      diagTitle = 'Zonal Transport of Density'
      diagUnits = 'kg/m^2/s        '
      WRITE(diagCode,'(A,I3.3,A)') 'UU   ',diagNum+2,'MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'VRHOMASS'
      diagTitle = 'Meridional Transport of Density'
      diagUnits = 'kg/m^2/s        '
      WRITE(diagCode,'(A,I3.3,A)') 'VV   ',diagNum,'MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'WRHOMASS'
      diagTitle = 'Vertical Transport of Potential Density'
      diagUnits = 'kg/m^2/s        '
      diagCode  = 'WM      LR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'PHIHYD  '
      diagTitle = 'Hydrostatic (ocean) pressure / (atmos) geo-Potential'
      diagUnits = 'm^2/s^2         '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'PHIHYDSQ'
      diagTitle = 'Square of Hyd. (ocean) press / (atmos) geoPotential'
      diagUnits = 'm^4/s^4         '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'PHIBOT  '
      diagTitle = 'ocean bottom pressure / top. atmos geo-Potential'
      diagUnits = 'm^2/s^2         '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'PHIBOTSQ'
      diagTitle = 'Square of ocean bottom pressure / top. geo-Potential'
      diagUnits = 'm^4/s^4         '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'DRHODR  '
      diagTitle = 'Stratification: d.Sigma/dr (kg/m3/r_unit)'
      diagUnits = 'kg/m^4          '
      IF ( usingPCoords ) diagUnits = 's^2/m^2         '
      diagCode  = 'SM      LR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

C--   Momentum terms:
      diagName  = 'VISCA4  '
      diagTitle = 'Biharmonic Viscosity Coefficient in (m4/s)        '
      diagUnits = 'm^4/s           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'VISCAH  '
      diagTitle = 'Harmonic Viscosity Coefficient in (m2/s)        '
      diagUnits = 'm^2/s           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

C--   surface fluxes:
      diagName  = 'TAUX    '
      diagTitle = 'zonal surface wind stress, >0 increases uVel    '
      diagUnits = 'N/m^2           '
      diagCode  = 'SU      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TAUY    '
      diagTitle = 'meridional surf. wind stress, >0 increases vVel '
      diagUnits = 'N/m^2           '
      diagCode  = 'SV      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TFLUX   '
      diagTitle = 'net surface heat flux, >0 increases theta       '
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TRELAX  '
      diagTitle = 'surface temperature relaxation, >0 increases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'TICE    '
      diagTitle = 'heat from melt/freeze of sea-ice, >0 increases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'SFLUX   '
      diagTitle = 'net surface salt flux, >0 increases salt        '
      diagUnits = 'g/m^2/s         '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'SRELAX  '
      diagTitle = 'surface salinity relaxation, >0 increases salt  '
      diagUnits = 'g/m^2/s         '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

#ifdef SHORTWAVE_HEATING
      diagName  = 'SWFLUX  '
      diagTitle = 'net upward SW radiation, >0 increases theta     '
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      diagName  = 'PRESSURE'
      diagTitle = 'Atmospheric Pressure (Pa)     '
      diagUnits = DIAGS_MK_UNITS( rUnit2c, myThid )
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )

      diagName  = 'RSURF   '
      diagTitle = 'Surface (Pressure, Height) (Pa,m)'
      diagUnits = DIAGS_MK_UNITS( rUnit2c, myThid )
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, myThid )


      RETURN
      END
