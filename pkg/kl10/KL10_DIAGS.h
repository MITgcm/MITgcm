C $Header: /u/gcmpack/MITgcm/pkg/kl10/KL10_DIAGS.h,v 1.1 2014/07/30 03:28:05 jmc Exp $
C $Name:  $

#ifdef ALLOW_KL10

C     /==========================================================\
C     | KL10_DIAGS.h                                             |
C     | o Header for KL10 diagnostic output                      |
C     \==========================================================/

C----------------------------------------------------------------
C     kl_drctrec     - next record to dump for KL files
C----------------------------------------------------------------
      INTEGER kl_drctrec
      COMMON /KL10_RECORDNUM1/ kl_drctrec

#ifdef ALLOW_TIMEAVE
C----------------------------------------------------------------
C     kl_drctrecTave - next record to dump for KL averaging files
C----------------------------------------------------------------

      INTEGER kl_drctrecTave
      COMMON /KL10_RECORDNUM2/ kl_drctrecTave

C----------------------------------------------------------------
C     kl_TimeAve - time of temporal integration (s) for each thread
C----------------------------------------------------------------

      _RL kl_TimeAve(Nr,nSx,nSy)
      COMMON /KL10_TAVE/ kl_TimeAve

C----------------------------------------------------------------
C     KL*tave    - Time-averaging KL variables
C----------------------------------------------------------------

      _RL KLviscArtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL KLdiffKrtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL KLepstave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /KL10_TAVE_DIAGS/  KLviscArtave, KLdiffKrtave, KLepstave

#endif /* ALLOW_TIMEAVE */

#endif /* ALLOW_KL10 */
