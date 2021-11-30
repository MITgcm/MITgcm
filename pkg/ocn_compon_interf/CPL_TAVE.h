CBOP
C     !ROUTINE: CPL_TAVE.h
C     !INTERFACE:
C     include "CPL_TAVE.h"
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
C     TXtave      :: Surface stress [Pa=N/m2], zonal compon.
C     TYtave      :: Surface stress [Pa=N/m2], merid compon.
C     FWtave      :: Net fresh water flux (=E-P-R) [kg/m2/s, +=upward]
C     SFxtave     :: Salt flux (from sea-ice)       [g/m2/s, +=upward]
C     SICtave     :: Sea-ice mass [kg/m2]
C     MXLtave     :: Ocean mixed-layer depth   [m]
C     SSTtave     :: Ocean surface temperature [oC]
C     SSStave     :: Ocean surface salinity    [g/kg]
C     vSqtave     :: Ocean surface velocity square [m2/s2]
C     aC02tave    :: CO2 level in atm [parts by volume]
C     sWSpdtave   :: Surface wind speed [m/s]
C     iceftave    :: Fraction of ocean covered by seaice
C     fCO2tave    :: Flux of CO2 from ocean->atm [mol/m2/s]
      COMMON /CPL_TAVE_VARS/
     &                   CPL_timeAve,
     &                   SLPtave, HFtave, QSWtave,
     &                   TXtave, TYtave,
     &                   FWtave, SFxtave, SICtave,
     &                   MXLtave, SSTtave, SSStave, vSqtave
#ifdef ALLOW_DIC
     &                 , aCO2tave, sWSpdtave,
     &                   iceftave, fCO2tave
#endif /* ALLOW_DIC */

      _RL  CPL_timeAve(nSx,nSy)
      _RL  SLPtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  HFtave    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  QSWtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  TXtave    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  TYtave    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  FWtave    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  SFxtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  SICtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  MXLtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  SSTtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  SSStave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  vSqtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef ALLOW_DIC
      _RL  aCO2tave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  sWSpdtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  iceftave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  fCO2tave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* ALLOW_DIC */

#endif /* ALLOW_TIMEAVE */
#endif /* COMPONENT_MODULE */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
