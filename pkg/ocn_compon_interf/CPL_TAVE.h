C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/CPL_TAVE.h,v 1.1 2010/01/03 19:26:54 jmc Exp $
C $Name:  $

CBOP
C     !ROUTINE: LAND_TAVE.h
C     !INTERFACE:
C     include "LAND_TAVE.h"
C     !DESCRIPTION:
C     \bv
C     *==========================================================*
C     | CPL_TAVE.h
C     | o Header for CPL time-average diagnostics
C     *==========================================================*
C     | Declares global arrays used for holding/accumulating
C     | diagnostic output from CPL.
C     *==========================================================*
C     \ev
CEOP

#ifdef COMPONENT_MODULE
#ifdef ALLOW_TIMEAVE

C--   COMMON /CPL_TAVE_VARS/ Time average CPL-variables
C     CPL_timeAve :: Cumulated time [s]
C     SLPtave     :: Atmospheric Sea-Level pressure [Pa=N/m2]
C     HFtave      :: Net surface heat-flux   [W/m2, +=upward]
C     QSWtave     :: Net shortwave heat flux [W/m2, +=upward]
C     QLTtave     :: Latent heat flux        [W/m2, +=upward]
C     QSNtave     :: Sensible heat flux      [W/m2, +=upward]
C     QLWtave     :: Net longwave heat flux  [W/m2, +=upward]
C     UGtave      :: Wind speed @ ground, zonal  component [m/s]
C     VGtave      :: Wind speed @ ground, merid. component [m/s]
C     TXtave      :: Surface stress [Pa=N/m2], zonal compon.
C     TYtave      :: Surface stress [Pa=N/m2], merid compon.
C     FWtave      :: Net fresh water flux (=E-P-R) [kg/m2/s, +=upward]
C     SFxtave     :: Salt flux (from sea-ice)  [psu.kg/m2/s, +=upward]
C     SICtave     :: Sea-ice mass [kg/m2]
C     MXLtave     :: Ocean mixed-layer depth   [m]
C     SSTtave     :: Ocean surface temperature [oC]
C     SSStave     :: Ocean surface salinity    [psu]
C     vSqtave     :: Ocean surface velocity square [m2/s2]
C     aC02tave    :: CO2 level in atm [parts by volume]
C     sWSpdtave   :: Surface wind speed [m/s]
C     iceftave    :: Fraction of ocean covered by seaice
C     fCO2tave    :: Flux of CO2 from ocean->atm [mol/m2/s]
      COMMON /CPL_TAVE_VARS/
     &                   CPL_timeAve,
     &                   SLPtave, HFtave, QSWtave,
c    &                   QLTtave, QSNtave, QLWtave,
c    &                   UGtave, VGtave,
     &                   TXtave, TYtave,
     &                   FWtave, SFxtave, SICtave,
     &                   MXLtave, SSTtave, SSStave, vSqtave,
     &                   aCO2tave, sWSpdtave,
     &                   iceftave, fCO2tave

      _RL  CPL_timeAve(nSx,nSy)
      _RL  SLPtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  HFtave    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  QSWtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL  QLTtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL  QSNtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL  QLWtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL  UGtave    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL  VGtave    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  TXtave    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  TYtave    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  FWtave    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  SFxtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  SICtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  MXLtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  SSTtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  SSStave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  vSqtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  aCO2tave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  sWSpdtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  iceftave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  fCO2tave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#endif /* ALLOW_TIMEAVE */
#endif /* COMPONENT_MODULE */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
