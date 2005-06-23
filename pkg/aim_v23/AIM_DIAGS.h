C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/Attic/AIM_DIAGS.h,v 1.5 2005/06/23 20:02:50 edhill Exp $
C $Name:  $

#ifdef ALLOW_AIM

C     /==========================================================\
C     | AIM_DIAGS.h                                              |
C     | o Header for AIM diagnostic output                       |
C     |==========================================================|
C     | Declares global arrays used for holding/accumulating     |
C     | diagnostic output from AIM.                              |
C     \==========================================================/

#ifdef ALLOW_AIM_TAVE

C     Timer for AIM diags
      _RL aim_timeAve(Nr,nSx,nSy)
      COMMON /AIM_TAVE/ aim_timeAve

C     USTRtave  - u-stress (Pa)
C     VSTRtave  - v-stress (Pa)
C     TSRtave   - top-of-atm. shortwave radiation (W/m2)
C     OLRtave   - outgoing longwave radiation (W/m2)
C     SSRtave   - surface shortwave radiation (W/m2)
C     SLRtave   - surface longwave radiation (W/m2)
C     SHFtave   - sensible heat flux (W/m2)
C     EVAPtave  - evaporation [g/ (m2 s)]
C     PRECNVtave- convective precipitation [g/ (m2 s)]
C     PRECLStave- large-scale precipitation [g/ (m2 s)]
C     CLOUDCtave- total cloud cover (fraction)
C     CLTOPtave - normalized pressure at cloud top
C     CBMFtave  - cloud-base mass flux
C     DRAGtave  - surface Drag term (= Cd*Rho*|V|) (land+sea combined)
C     aimV0tave - surface wind speed [m/s]
C     aimT0tave - surface air absolute temp. [K]
C     aimQ0tave - surface air spec. humidity [g/kg]
C     EnFxPrtave- energy flux associated with precip. (snow, rain temp) [W/m^2]
C     albedotave- surface albedo [0-1]
C     dTsurftave- surf. Temp change from 1 iter to the next one (>0) [K]
C     aimRHtave - Relative Humidity [0-1]
C
      COMMON /AIM_TAVE_A/
     &                   USTRTave, VSTRtave,
     &                   TSRtave, OLRtave, SSRtave, SLRtave, SHFtave,
     &                   EVAPtave, PRECNVtave, PRECLStave,
     &                   CLOUDCtave, CLTOPtave, CBMFtave, DRAGtave,
     &                   aimV0tave, aimT0tave, aimQ0tave, 
     &                   EnFxPrtave, albedotave, dTsurftave,
     &                   aimRHtave
      _RL  USTRtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  VSTRtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  TSRtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  OLRtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  SSRtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  SLRtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  SHFtave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  EVAPtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  PRECNVtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  PRECLStave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  CLOUDCtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  CLTOPtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  CBMFtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  DRAGtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  aimV0tave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  aimT0tave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  aimQ0tave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  EnFxPrtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  albedotave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  dTsurftave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  aimRHtave (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

#endif /* ALLOW_AIM_TAVE */


#endif /* ALLOW_AIM */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
