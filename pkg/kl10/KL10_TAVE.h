C $Header: /u/gcmpack/MITgcm/pkg/kl10/KL10_TAVE.h,v 1.1 2014/07/30 03:28:05 jmc Exp $
C $Name:  $

#ifdef ALLOW_KL10

C     *==========================================================*
C     | KL10_TAVE.h
C     | o Header for KL10 time-average output
C     *==========================================================*

#ifdef ALLOW_TIMEAVE
C----------------------------------------------------------------
C     KL_timeAve - time of temporal integration (s) for each thread
C----------------------------------------------------------------

      _RL KL_timeAve(nSx,nSy)
      COMMON /KL10_TAVE_COUNT/ KL_timeAve

C----------------------------------------------------------------
C     KL*tave    - Time-averaging KL variables
C----------------------------------------------------------------

      _RL KLviscArtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL KLdiffKrtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /KL10_TAVE_FIELDS/
     &        KLviscArtave, KLdiffKrtave

#endif /* ALLOW_TIMEAVE */

#endif /* ALLOW_KL10 */
