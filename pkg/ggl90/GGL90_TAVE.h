C $Header: /u/gcmpack/MITgcm/pkg/ggl90/GGL90_TAVE.h,v 1.2 2010/01/03 18:55:06 jmc Exp $
C $Name:  $

#ifdef ALLOW_GGL90

C     *==========================================================*
C     | GGL90_TAVE.h
C     | o Header for GGL90 time-average output
C     *==========================================================*

#ifdef ALLOW_TIMEAVE

C----------------------------------------------------------------
C     GGL90_timeAve - time of temporal integration (s) for each thread
C----------------------------------------------------------------

      _RL GGL90_timeAve(nSx,nSy)
      COMMON /GGL90_TAVE_COUNT/ GGL90_timeAve

C----------------------------------------------------------------
C     GGL90*tave    - Time-averaging GGL90 variables
C----------------------------------------------------------------

      _RL GGL90TKEtave     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL GGL90viscArtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL GGL90diffKrtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /GGL90_TAVE_FIELDS/
     &        GGL90TKEtave, GGL90viscArtave, GGL90diffKrtave

#endif /* ALLOW_TIMEAVE */

#endif /* ALLOW_GGL90 */
