C $Header: /u/gcmpack/MITgcm/pkg/therm_seaice/Attic/ICE_DIAGS.h,v 1.1 2002/11/21 19:11:42 cheisey Exp $
C $Name:  $
cswdice -- change to keep ice fields --

C     /==========================================================\
C     | ICE_DIAGS.h                                               |
C     | o Header for GM?Redi diagnostic output                   |
C     \==========================================================/

#ifdef ALLOW_THERM_SEAICE

#ifdef ALLOW_TIMEAVE

C     Keep track of time
      _RL ICE_TimeAve(nSx,nSy)
      COMMON /ICE_TAVE/ ICE_TimeAve


C     Storage arrays for time-averages
      _RL ICE_icemask_AVE     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ICE_iceheight_AVE   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ICE_snowheight_AVE  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ICE_Tsrf_AVE        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ICE_Tice1_AVE       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ICE_Tice2_AVE       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ICE_snow_AVE        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ICE_qleft_AVE       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ICE_fresh_AVE        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)


      COMMON /ICE_TAVE_ARRAYS/
     &                       Ice_icemask_AVE, ICE_iceheight_AVE,
     &                       ICE_snowheight_AVE, ICE_Tsrf_AVE,
     &                       ICE_Tice1_AVE, ICE_Tice2_AVE, ICE_snow_AVE,
     &                       ICE_qleft_AVE, ICE_fresh_AVE

#endif /* ALLOW_TIMEAVE */

#endif /* ALLOW_THERM_SEAICE */
