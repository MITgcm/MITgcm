C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/Attic/CPL_DIAGS.h,v 1.1 2003/12/15 02:49:09 jmc Exp $
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

C     HFtave  - Net heat-flux (w/m^2, +ve into atmos.)
C     TXtave  - surface stress (Pa=N/m2), zonal compon.
C     TYtave  - surface stress (Pa=N/m2), merid compon.
C     FWtave  - fresh water (=E-P, m/s)
C     UGtave  - wind speed @ ground, zonal  component (m/s)
C     VGtave  - wind speed @ ground, merid. component (m/s)
C     QLTtave  - latent heat flux (W/m2)
C     QSNtave  - sensible heat flux (W/m2)
C     QLWtave  - longwave heat flux (W/m2)
C     QSWtave  - shortwave heat flux (W/m2)
C     SSTtave  - ocean temperature   (oC)
C
      COMMON /CPL_TAVE/
     &                   HFtave, TXtave, TYtave, FWtave,
     &                   UGtave, VGtave,
     &                   QLTtave, QSNtave, QLWtave, QSWtave,
     &                   SSTtave
      _RL  HFtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  TXtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  TYtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  FWtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  UGtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  VGtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  QLTtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  QSNtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  QLWtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  QSWtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  SSTtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#endif /* ALLOW_TIMEAVE */


#endif /* COMPONENT_MODULE */

