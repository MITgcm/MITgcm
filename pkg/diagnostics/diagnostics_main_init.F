C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_main_init.F,v 1.35 2008/11/15 19:52:32 jmc Exp $
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
C     rTitle     :: r-coordinate title
C     eTitle     :: free-surface title
C     fTitle     :: fixed boundary title
C     pTitle     :: "Phi"  title
C     sTitle     :: "salt" title
      INTEGER        diagNum
      INTEGER        diagMate
      CHARACTER*8    diagName
      CHARACTER*16   diagCode
      CHARACTER*16   diagUnits
      CHARACTER*(80) diagTitle
      CHARACTER*2    rUnit2c
      CHARACTER*4    tUnit4c, sUnit4c
      CHARACTER*(10) rTitle, eTitle, fTitle
      CHARACTER*(20) pTitle, sTitle

      CHARACTER*(16) DIAGS_MK_UNITS
      EXTERNAL DIAGS_MK_UNITS
      CHARACTER*(80) DIAGS_MK_TITLE
      EXTERNAL DIAGS_MK_TITLE

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     For each output variable,
C     specify Name (cdiag, 8c), Descriptions (tdiag, *c), Units (udiag, 16c)
C         and Type/Parms (location on C grid, 2D/3D, ...) (gdiag, 16c)
C----------------------------------------------------------------------

      IF ( usingPCoords ) THEN
        rUnit2c= 'Pa'
        rTitle = ' Pressure '
        pTitle = ' Geopotential       '
      ELSE
        rUnit2c= 'm '
        rTitle = ' Height   '
        pTitle = 'Pressure Pot.(p/rho)'
      ENDIF
      IF ( fluidIsAir ) THEN
        tUnit4c= 'K   '
        sUnit4c= 'g/kg'
        sTitle = ' Specific Humidity  '
#ifdef ALLOW_FIZHI
        IF (useFIZHI) sUnit4c= 'kg/kg'
#endif /* ALLOW_FIZHI */
      ELSE
        tUnit4c= 'degC'
        sUnit4c= 'psu '
        sTitle = ' Salinity           '
      ENDIF
C-    free-surface (eTitle) and fixed-boundary (fTitle) position:
      IF ( fluidIsAir ) THEN
        eTitle = ' Surface  '
        fTitle = ' Top      '
      ELSEIF ( usingPCoords ) THEN
        eTitle = ' Bottom   '
        fTitle = ' Surface  '
      ELSE
        eTitle = ' Surface  '
        fTitle = ' Bottom   '
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-    state variables of the main code (and related quadratic var):


      diagName  = 'ETAN    '
      diagTitle = DIAGS_MK_TITLE( eTitle//rTitle//' Anomaly', myThid )
c     IF ( fluidIsWater .AND. usingZCoords )
c    &diagTitle = 'Sea Surface Elevation'
      diagUnits = DIAGS_MK_UNITS( rUnit2c, myThid )
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'ETANSQ  '
      diagTitle = DIAGS_MK_TITLE( 'Square of '//eTitle//rTitle
     I                          //' Anomaly', myThid )
      diagUnits = DIAGS_MK_UNITS( rUnit2c//'^2', myThid )
      diagCode  = 'SM P    M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'DETADT2 '
      diagTitle = DIAGS_MK_TITLE( 'Square of '//eTitle//rTitle
     I                          //' Anomaly Tendency', myThid )
      diagUnits = DIAGS_MK_UNITS( rUnit2c//'^2/s^2', myThid )
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'THETA   '
      diagTitle = 'Potential Temperature'
      diagUnits = DIAGS_MK_UNITS( tUnit4c, myThid )
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

c     diagName  = 'SST     '
c     diagTitle = 'Sea Surface Temperature (degC,K)'
c     diagUnits = DIAGS_MK_UNITS( tUnit4c, myThid )
c     diagCode  = 'SM      M1      '
c     CALL DIAGNOSTICS_ADDTOLIST( diagNum,
c    I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SALT    '
      diagTitle = DIAGS_MK_TITLE( sTitle,  myThid )
      diagUnits = DIAGS_MK_UNITS( sUnit4c, myThid )
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'RELHUM  '
      diagTitle = 'Relative Humidity'
      diagUnits = 'percent         '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

c     diagName  = 'SSS     '
c     diagTitle = 'Sea Surface Salinity '
c     diagUnits = DIAGS_MK_UNITS( sUnit4c, myThid )
c     diagCode  = 'SM      M1      '
c     CALL DIAGNOSTICS_ADDTOLIST( diagNum,
c    I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      IF ( fluidIsWater ) THEN
      diagName  = 'SALTanom'
      diagTitle = 'Salt anomaly (=SALT-35; g/kg)'
      diagUnits = DIAGS_MK_UNITS( sUnit4c, myThid )
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
      ENDIF

      diagName  = 'UVEL    '
      diagTitle = 'Zonal Component of Velocity (m/s)'
      diagUnits = 'm/s             '
      diagCode  = 'UU      MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'VVEL    '
      diagTitle = 'Meridional Component of Velocity (m/s)'
      diagUnits = 'm/s             '
      diagCode  = 'VV      MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'WVEL    '
      diagTitle = 'Vertical Component of Velocity (r_units/s)'
      diagUnits = DIAGS_MK_UNITS( rUnit2c//'/s', myThid )
      diagCode  = 'WM      LR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'THETASQ '
      diagTitle = 'Square of Potential Temperature'
      diagUnits = DIAGS_MK_UNITS( tUnit4c//'^2', myThid )
      diagCode  = 'SM P    MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SALTSQ  '
      diagTitle = DIAGS_MK_TITLE( 'Square of '//sTitle, myThid )
      diagUnits = DIAGS_MK_UNITS( '('//sUnit4c//')^2', myThid )
      diagCode  = 'SM P    MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      IF ( fluidIsWater ) THEN
      diagName  = 'SALTSQan'
      diagTitle = 'Square of Salt anomaly (=(SALT-35)^2 (g^2/kg^2)'
      diagUnits = DIAGS_MK_UNITS( '('//sUnit4c//')^2', myThid )
      diagCode  = 'SM P    MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
      ENDIF

      diagName  = 'UVELSQ  '
      diagTitle = 'Square of Zonal Comp of Velocity (m^2/s^2)'
      diagUnits = 'm^2/s^2         '
      diagCode  = 'UU P    MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'VVELSQ  '
      diagTitle = 'Square of Meridional Comp of Velocity (m^2/s^2)'
      diagUnits = 'm^2/s^2         '
      diagCode  = 'VV P    MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'WVELSQ  '
      diagTitle = 'Square of Vertical Comp of Velocity'
      diagUnits = DIAGS_MK_UNITS( rUnit2c//'^2/s^2', myThid )
      diagCode  = 'WM P    LR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'UE_VEL_C'
      diagTitle = 'Eastward Velocity (m/s) (cell center)'
      diagUnits = 'm/s             '
      diagCode  = 'UM      MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'VN_VEL_C'
      diagTitle = 'Northward Velocity (m/s) (cell center)'
      diagUnits = 'm/s             '
      diagCode  = 'VM      MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'UV_VEL_C'
      diagTitle ='Product of horizontal Comp of velocity (cell center)'
      diagUnits = 'm^2/s^2         '
      diagCode  = 'UM      MR      '
      diagMate  = diagNum + 1
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'UV_VEL_Z'
      diagTitle = 'Meridional Transport of Zonal Momentum (m^2/s^2)'
      diagUnits = 'm^2/s^2         '
      diagCode  = 'UZ      MR      '
      diagMate  = diagNum + 1
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'WU_VEL  '
      diagTitle = 'Vertical Transport of Zonal Momentum'
      diagUnits = DIAGS_MK_UNITS( 'm.'//rUnit2c//'/s^2', myThid )
      diagCode  = 'WU      LR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'WV_VEL  '
      diagTitle ='Vertical Transport of Meridional Momentum'
      diagUnits = DIAGS_MK_UNITS( 'm.'//rUnit2c//'/s^2', myThid )
      diagCode  = 'WV      LR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'UVELMASS'
      diagTitle = 'Zonal Mass-Weighted Comp of Velocity (m/s)'
      diagUnits = 'm/s             '
      diagCode  = 'UU      MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'VVELMASS'
      diagTitle = 'Meridional Mass-Weighted Comp of Velocity (m/s)'
      diagUnits = 'm/s             '
      diagCode  = 'VV      MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'WVELMASS'
      diagTitle = 'Vertical Mass-Weighted Comp of Velocity'
      diagUnits = DIAGS_MK_UNITS( rUnit2c//'/s', myThid )
      diagCode  = 'WM      LR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'UTHMASS '
      diagTitle = 'Zonal Mass-Weight Transp of Pot Temp'
      diagUnits = DIAGS_MK_UNITS( tUnit4c//'.m/s', myThid )
      diagCode  = 'UU      MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'VTHMASS '
      diagTitle = 'Meridional Mass-Weight Transp of Pot Temp'
      diagUnits = DIAGS_MK_UNITS( tUnit4c//'.m/s', myThid )
      diagCode  = 'VV      MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'WTHMASS '
      diagTitle = 'Vertical Mass-Weight Transp of Pot Temp (K.m/s)'
      diagUnits = DIAGS_MK_UNITS(tUnit4c//'.'//rUnit2c//'/s', myThid )
      diagCode  = 'WM      LR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'USLTMASS'
      diagTitle = DIAGS_MK_TITLE( 'Zonal Mass-Weight Transp of '
     I                           //sTitle, myThid )
      diagUnits = DIAGS_MK_UNITS(sUnit4c//'.m/s', myThid )
      diagCode  = 'UU      MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'VSLTMASS'
      diagTitle = DIAGS_MK_TITLE( 'Meridional Mass-Weight Transp of '
     I                           //sTitle, myThid )
      diagUnits = DIAGS_MK_UNITS(sUnit4c//'.m/s', myThid )
      diagCode  = 'VV      MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'WSLTMASS'
      diagTitle = DIAGS_MK_TITLE( 'Vertical Mass-Weight Transp of '
     I                           //sTitle, myThid )
      diagUnits = DIAGS_MK_UNITS(sUnit4c//'.'//rUnit2c//'/s', myThid )
      diagCode  = 'WM      LR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'UVELTH  '
      diagTitle = 'Zonal Transport of Pot Temp'
      diagUnits = DIAGS_MK_UNITS( tUnit4c//'.m/s', myThid )
      diagCode  = 'UU      MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'VVELTH  '
      diagTitle = 'Meridional Transport of Pot Temp'
      diagUnits = DIAGS_MK_UNITS( tUnit4c//'.m/s', myThid )
      diagCode  = 'VV      MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'WVELTH  '
      diagTitle = 'Vertical Transport of Pot Temp'
      diagUnits = DIAGS_MK_UNITS(tUnit4c//'.'//rUnit2c//'/s', myThid )
      diagCode  = 'WM      LR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'UVELSLT '
      diagTitle = DIAGS_MK_TITLE( 'Zonal Transport of '
     I                          //sTitle, myThid )
      diagUnits = DIAGS_MK_UNITS( sUnit4c//'.m/s', myThid )
      diagCode  = 'UU      MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'VVELSLT '
      diagTitle = DIAGS_MK_TITLE( 'Meridional Transport of '
     I                          //sTitle, myThid )
      diagUnits = DIAGS_MK_UNITS( sUnit4c//'.m/s', myThid )
      diagCode  = 'VV      MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'WVELSLT '
      diagTitle = DIAGS_MK_TITLE( 'Vertical Transport of '
     I                          //sTitle, myThid )
      diagUnits = DIAGS_MK_UNITS(sUnit4c//'.'//rUnit2c//'/s', myThid )
      diagCode  = 'WM      LR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'UVELPHI '
      diagTitle = DIAGS_MK_TITLE( 'Zonal Mass-Weight Transp of '
     I                 //pTitle//' Anomaly', myThid )
      diagUnits = 'm^3/s^3         '
      diagCode  = 'UU      MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'VVELPHI '
      diagTitle = DIAGS_MK_TITLE( 'Meridional Mass-Weight Transp of '
     I                 //pTitle//' Anomaly', myThid )
      diagUnits = 'm^3/s^3         '
      diagCode  = 'VV      MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      diagName  = 'RHOAnoma'
      diagTitle = 'Density Anomaly (=Rho-rhoConst)'
      diagUnits = 'kg/m^3          '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'RHOANOSQ'
      diagTitle = 'Square of Density Anomaly (=(Rho-rhoConst)^2)'
      diagUnits = 'kg^2/m^6          '
      diagCode  = 'SM P    MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'URHOMASS'
      diagTitle = 'Zonal Transport of Density'
      diagUnits = 'kg/m^2/s        '
      diagCode  = 'UU      MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'VRHOMASS'
      diagTitle = 'Meridional Transport of Density'
      diagUnits = 'kg/m^2/s        '
      diagCode  = 'VV      MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'WRHOMASS'
      diagTitle = 'Vertical Transport of Potential Density'
      diagUnits = 'kg/m^2/s        '
      diagCode  = 'WM      LR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'PHIHYD  '
      diagTitle = DIAGS_MK_TITLE( 'Hydrostatic '
     I                           //pTitle//' Anomaly', myThid )
      diagUnits = 'm^2/s^2         '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'PHIHYDSQ'
      diagTitle = DIAGS_MK_TITLE( 'Square of Hyd. '
     I                           //pTitle//' Anomaly', myThid )
      diagUnits = 'm^4/s^4         '
      diagCode  = 'SM P    MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'PHIBOT  '
c     diagTitle = 'ocean bottom pressure / top. atmos geo-Potential'
      diagTitle = DIAGS_MK_TITLE( fTitle
     I                           //pTitle//' Anomaly', myThid )
      diagUnits = 'm^2/s^2         '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'PHIBOTSQ'
c     diagTitle = 'Square of ocean bottom pressure / top. geo-Potential'
      diagTitle = DIAGS_MK_TITLE( 'Square of '//fTitle
     I                           //pTitle//' Anomaly', myThid )
      diagUnits = 'm^4/s^4         '
      diagCode  = 'SM P    M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#ifdef ALLOW_NONHYDROSTATIC
      diagName  = 'PHI_NH  '
      diagTitle = DIAGS_MK_TITLE( 'Non-Hydrostatic '//pTitle, myThid )
      diagUnits = 'm^2/s^2         '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif /* ALLOW_NONHYDROSTATIC */

      diagName  = 'MXLDEPTH'
      diagTitle = 'Mixed-Layer Depth (>0)'
      diagUnits = 'm               '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'DRHODR  '
      diagTitle = 'Stratification: d.Sigma/dr (kg/m3/r_unit)'
      diagUnits = 'kg/m^4          '
      IF ( usingPCoords ) diagUnits = 's^2/m^2         '
      diagCode  = 'SM      LR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'CONVADJ '
      diagTitle = 'Convective Adjustment Index [0-1] '
      diagUnits = 'fraction        '
      diagCode  = 'SM      LR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

C--   surface fluxes:
      diagName  = 'oceTAUX '
      diagTitle = 'zonal surface wind stress, >0 increases uVel'
      diagUnits = 'N/m^2           '
      diagCode  = 'UU      U1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'oceTAUY '
      diagTitle = 'meridional surf. wind stress, >0 increases vVel'
      diagUnits = 'N/m^2           '
      diagCode  = 'VV      U1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'atmPload'
      diagTitle = 'Atmospheric pressure loading'
      diagUnits = 'Pa              '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'sIceLoad'
      diagTitle = 'sea-ice loading (in Mass of ice+snow / area unit)'
      diagUnits = 'kg/m^2          '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'oceFWflx'
      diagTitle = 'net surface Fresh-Water flux into the ocean'
     &          //' (+=down), >0 decreases salinity'
      diagUnits = 'kg/m^2/s        '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'oceSflux'
      diagTitle = 'net surface Salt flux into the ocean (+=down),'
     &          //' >0 increases salinity'
      diagUnits = 'g/m^2/s         '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'oceQnet '
      diagTitle = 'net surface heat flux into the ocean (+=down),'
     &          //' >0 increases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'oceQsw  '
      diagTitle = 'net Short-Wave radiation (+=down),'
     &          //' >0 increases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'oceFreez'
      diagTitle = 'heating from freezing of sea-water (allowFreezing=T)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'TRELAX  '
      diagTitle = 'surface temperature relaxation, >0 increases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SRELAX  '
      diagTitle = 'surface salinity relaxation, >0 increases salt'
      diagUnits = 'g/m^2/s         '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'surForcT'
      diagTitle = 'model surface forcing for Temperature,'
     &          //' >0 increases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'surForcS'
      diagTitle = 'model surface forcing for Salinity,'
     &          //' >0 increases salinity'
      diagUnits = 'g/m^2/s         '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'TFLUX   '
      diagTitle = 'total heat flux (match heat-content variations),'
     &          //' >0 increases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SFLUX   '
      diagTitle = 'total salt flux (match salt-content variations),'
     &          //' >0 increases salt'
      diagUnits = 'g/m^2/s         '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      diagName  = 'RCENTER '
c     diagTitle = 'Cell-Center r-Position (Pressure, Height) (Pa,m)'
      diagTitle = DIAGS_MK_TITLE( 'Cell-Center '//rTitle, myThid )
      diagUnits = DIAGS_MK_UNITS( rUnit2c, myThid )
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'RSURF   '
c     diagTitle = 'Free-Surface r-Position (Pressure, Height) (Pa,m)'
      diagTitle = DIAGS_MK_TITLE( eTitle//rTitle, myThid )
      diagUnits = DIAGS_MK_UNITS( rUnit2c, myThid )
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'TOTUTEND'
      diagTitle = 'Tendency of Zonal Component of Velocity'
      diagUnits = 'm/s/day         '
      diagCode  = 'UU      MR      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'TOTVTEND'
      diagTitle = 'Tendency of Meridional Component of Velocity'
      diagUnits = 'm/s/day         '
      diagCode  = 'VV      MR      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I   diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'TOTTTEND'
      diagTitle = 'Tendency of Potential Temperature'
      diagUnits = DIAGS_MK_UNITS( tUnit4c//'/day', myThid )
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'TOTSTEND'
      diagTitle = DIAGS_MK_TITLE('Tendency of '//sTitle, myThid )
      diagUnits = DIAGS_MK_UNITS( sUnit4c//'/day', myThid )
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )


      RETURN
      END
