C $Header: /u/gcmpack/MITgcm/pkg/bulk_force/Attic/BULKF_DIAG.h,v 1.4 2002/12/11 14:23:35 cheisey Exp $
C $Name:  $
cswdblk -- change to keep bulk formula fields --

C     /==========================================================\
C     | BULKF_DIAG.h                                             |
C     | o Header for Bulk formula diagnostic output              |
C     \==========================================================/

#ifdef ALLOW_BULK_FORCE

#ifdef ALLOW_TIMEAVE

C     Keep track of time
      _RL BULKF_TimeAve(Nr, nSx,nSy)
      COMMON /BULKF_TAVE/ BULKF_TimeAve

C     Storage arrays for time-averages
      _RL BULK_Qnet_AVE    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BULK_EmPmR_AVE   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BULK_fu_AVE  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BULK_fv_AVE      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BULK_latent_AVE  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BULK_sensible_AVE(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BULK_evap_AVE     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BULK_flwup_AVE    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BULK_flwupnet_AVE (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BULK_solar_AVE    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BULK_ssq_AVE      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)


      COMMON /BULKF_TAVE_ARRAYS/
     &                       BULK_Qnet_AVE, BULK_EmPmR_AVE, 
     &                       BULK_fu_AVE, BULK_fv_AVE,
     &                       BULK_sensible_AVE, BULK_latent_AVE,
     &                       BULK_evap_AVE, BULK_flwup_AVE,
     &                       BULK_solar_AVE, BULK_ssq_AVE,
     &                       BULK_flwupnet_AVE



#endif /* ALLOW_TIMEAVE */

#endif /* ALLOW_BULK_FORCE */
