C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/Attic/CPL_DIAGS.h,v 1.4 2007/05/10 21:15:51 jscott Exp $
C $Name:  $

#ifdef COMPONENT_MODULE

C     /==========================================================\
C     | CPL_DIAGS.h                                              |
C     | o Header for CPL diagnostic output                       |
C     |==========================================================|
C     | Declares global arrays used for holding/accumulating     |
C     | diagnostic output from CPL.                              |
C     \==========================================================/

#ifdef ALLOW_TIMEAVE

C     Timer for CPL diags
      _RL CPL_TimeAve(Nr,nSx,nSy)
      COMMON /CPL_TAVE/ CPL_TimeAve

C     SLPtave - Atmospheric Sea-Level pressure (Pa=N/m2)
C     HFtave  - Net surface heat-flux   (W/m2, +=upward)
C     QSWtave - Net shortwave heat flux (W/m2, +=upward)
C     QLTtave - latent heat flux        (W/m2, +=upward)
C     QSNtave - sensible heat flux      (W/m2, +=upward)
C     QLWtave - net longwave heat flux  (W/m2, +=upward)
C     UGtave  - wind speed @ ground, zonal  component (m/s)
C     VGtave  - wind speed @ ground, merid. component (m/s)
C     TXtave  - surface stress (Pa=N/m2), zonal compon.
C     TYtave  - surface stress (Pa=N/m2), merid compon.
C     FWtave  - Net fresh water flux (=E-P-R)    (m/s, +=upward)
C     SFxtave - salt flux (from sea-ice) (psu.kg/m2/s, +=upward)
C     SICtave - sea-ice mass (kg/m2)
C     MXLtave - Ocean mixed-layer depth   (m)
C     SSTtave - ocean surface temperature (oC)
C     SSStave - ocean surface salinity    (psu)
C     vSqtave - ocean surface velocity square (m2/s2)
C     aC02tave- CO2 level in atm (parts by volume)
C     sWSpdtave-surface wind speed (m/s)
C     iceftave- fraction of ocean covered by seaice
C     fCO2tave- flux of CO2 from ocean->atm (mol/m2/s)

C
      COMMON /CPL_TAVE/
     &                   SLPtave, HFtave, QSWtave, 
c    &                   QLTtave, QSNtave, QLWtave,
c    &                   UGtave, VGtave,
     &                   TXtave, TYtave, 
     &                   FWtave, SFxtave, SICtave,
     &                   MXLtave, SSTtave, SSStave, vSqtave,
     &                   aCO2tave, sWSpdtave,
     &                   iceftave, fCO2tave

      _RL  SLPtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  HFtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  QSWtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL  QLTtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL  QSNtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL  QLWtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL  UGtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL  VGtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  TXtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  TYtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  FWtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  SFxtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  SICtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  MXLtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  SSTtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  SSStave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  vSqtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  aCO2tave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  sWSpdtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  iceftave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  fCO2tave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#endif /* ALLOW_TIMEAVE */


#endif /* COMPONENT_MODULE */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
