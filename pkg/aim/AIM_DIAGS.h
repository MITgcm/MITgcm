C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/AIM_DIAGS.h,v 1.3 2001/03/06 17:50:32 jmc Exp $
C $Name:  $

#ifdef ALLOW_AIM

C     /==========================================================\
C     | AIM_DIAGS.h                                              |
C     | o Header for AIM diagnostic output                       |
C     |==========================================================|
C     | Declares global arrays used for holding/accumulating     |
C     | diagnostic output from AIM.                              |
C     \==========================================================/

#ifdef ALLOW_TIMEAVE

C     Timer for AIM diags
      _RL AIM_TimeAve(Nr,nSx,nSy)
      COMMON /AIM_TAVE/ AIM_TimeAve

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
C
      COMMON /AIM_TAVE_A/
     &                   USTRTave,VSTRtave,
     &                   TSRtave,OLRtave,SSRtave,SLRtave,SHFtave,
     &                   EVAPtave,PRECNVtave,PRECLStave,CLOUDCtave
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

#endif /* ALLOW_TIMEAVE */


#endif /* ALLOW_AIM */
