C $Header: /u/gcmpack/MITgcm/pkg/thsice/Attic/THSICE_DIAGS.h,v 1.2 2003/12/31 17:44:32 jmc Exp $
C $Name:  $

C     /==========================================================\
C     | THSICE_DIAGS.h
C     | o Header for Therm-SeaICE time-average diagnostic
C     \==========================================================/


#ifdef ALLOW_THSICE

#ifdef ALLOW_TIMEAVE

C     Keep track of time
      _RL ICE_TimeAve(Nr, nSx,nSy)

C     Storage arrays for time-averages
      _RL ICE_iceMask_AVE(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ICE_iceH_AVE   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ICE_snowH_AVE  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ICE_Tsrf_AVE   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ICE_Tice1_AVE  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ICE_Tice2_AVE  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ICE_snow_AVE   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ICE_qleft_AVE  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ICE_fresh_AVE  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ICE_salFx_AVE  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ICE_Qnet_AVE   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ICE_FWfx_AVE   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL ICE_albedo_AVE (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      COMMON /THSICE_TAVE_ARRAYS/ ICE_TimeAve,
     &                 ICE_iceMask_AVE, ICE_iceH_AVE,
     &                 ICE_snowH_AVE, ICE_Tsrf_AVE,
     &                 ICE_Tice1_AVE, ICE_Tice2_AVE, ICE_snow_AVE,
     &                 ICE_qleft_AVE, ICE_fresh_AVE, ICE_salFx_AVE,
     &                 ICE_Qnet_AVE, ICE_FWfx_AVE, ICE_albedo_AVE

#endif /* ALLOW_TIMEAVE */

#endif /* ALLOW_THSICE */
