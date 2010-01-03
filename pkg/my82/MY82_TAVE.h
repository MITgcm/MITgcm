C $Header: /u/gcmpack/MITgcm/pkg/my82/MY82_TAVE.h,v 1.2 2010/01/03 19:10:46 jmc Exp $
C $Name:  $

#ifdef ALLOW_MY82

C     *==========================================================*
C     | MY82_TAVE.h
C     | o Header for MY82 time-average output
C     *==========================================================*

#ifdef ALLOW_TIMEAVE
C----------------------------------------------------------------
C     MY_timeAve - time of temporal integration (s) for each thread
C----------------------------------------------------------------

      _RL MY_timeAve(nSx,nSy)
      COMMON /MY_TAVE_COUNT/ MY_timeAve

C----------------------------------------------------------------
C     MY*tave    - Time-averaging MY variables
C----------------------------------------------------------------

      _RL MYhbltave   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL MYviscArtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL MYdiffKrtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /MY_TAVE_FIELDS/
     &        MYhbltave, MYviscArtave, MYdiffKrtave

#endif /* ALLOW_TIMEAVE */
#endif /* ALLOW_MY82 */
