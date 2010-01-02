C $Header: /u/gcmpack/MITgcm/pkg/bulk_force/BULKF_TAVE.h,v 1.2 2010/01/02 22:46:29 jmc Exp $
C $Name:  $

CBOP
C     !ROUTINE: BULKF_TAVE.h
C     !INTERFACE:
C     include "BULKF_TAVE.h"
C     !DESCRIPTION:
C     \bv
C     *==========================================================*
C     | BULKF_TAVE.h
C     | o Header for Bulk formula time-average output
C     *==========================================================*
C     \ev
CEOP

#ifdef ALLOW_BULK_FORCE
#ifdef ALLOW_TIMEAVE

C--   COMMON /BULKF_TAVE_VARS/  Time average Bulk-Formula variables
      _RL BULK_timeAve     (nSx,nSy)
      _RL BULK_Qnet_Ave    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BULK_EmPmR_Ave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BULK_fu_Ave      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BULK_fv_Ave      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BULK_latent_Ave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BULK_sensible_Ave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BULK_evap_Ave    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BULK_flwup_Ave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BULK_flwupnet_Ave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BULK_solar_Ave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL BULK_ssq_Ave     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)


      COMMON /BULKF_TAVE_VARS/
     &         BULK_timeAve, BULK_Qnet_Ave, BULK_EmPmR_Ave,
     &                       BULK_fu_Ave, BULK_fv_Ave,
     &                       BULK_sensible_Ave, BULK_latent_Ave,
     &                       BULK_evap_Ave, BULK_flwup_Ave,
     &                       BULK_solar_Ave, BULK_ssq_Ave,
     &                       BULK_flwupnet_Ave


#endif /* ALLOW_TIMEAVE */
#endif /* ALLOW_BULK_FORCE */
