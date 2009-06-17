C $Header: /u/gcmpack/MITgcm/pkg/ggl90/GGL90_TAVE.h,v 1.1 2009/06/17 14:15:34 jmc Exp $
C $Name:  $

#ifdef ALLOW_GGL90

C     *==========================================================*
C     | GGL90_TAVE.h
C     | o Header for GGL90 time-average output
C     *==========================================================*

C----------------------------------------------------------------
C     my_drctrec     - next record to dump for GGL90 files
C----------------------------------------------------------------
      INTEGER ggl90_drctrec
      COMMON /GGL90_RECORDNUM1/ ggl90_drctrec

#ifdef ALLOW_TIMEAVE
C----------------------------------------------------------------
C     ggl90_drctrecTave - next record to dump for GGL90 averaging files
C----------------------------------------------------------------

      INTEGER ggl90_drctrecTave
      COMMON /GGL90_RECORDNUM2/ ggl90_drctrecTave

C----------------------------------------------------------------
C     ggl90_TimeAve - time of temporal integration (s) for each thread
C----------------------------------------------------------------

      _RL ggl90_TimeAve(Nr,nSx,nSy)
      COMMON /GGL90_TAVE/ ggl90_TimeAve

C----------------------------------------------------------------
C     GGL90*tave    - Time-averaging GGL90 variables
C----------------------------------------------------------------

      _RL GGL90TKEtave     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL GGL90viscArtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL GGL90diffKrtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /GGL90_TAVE_DIAGS/
     &        GGL90TKEtave, GGL90viscArtave, GGL90diffKrtave

#endif /* ALLOW_TIMEAVE */

#endif /* ALLOW_GGL90 */
