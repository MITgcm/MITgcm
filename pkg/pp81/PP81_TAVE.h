C $Header: /u/gcmpack/MITgcm/pkg/pp81/PP81_TAVE.h,v 1.2 2010/01/03 19:19:59 jmc Exp $
C $Name:  $

#ifdef ALLOW_PP81

C     *==========================================================*
C     | PP81_TAVE.h
C     | o Header for PP81 time-average output
C     *==========================================================*

#ifdef ALLOW_TIMEAVE
C----------------------------------------------------------------
C     PP_timeAve - time of temporal integration (s) for each thread
C----------------------------------------------------------------

      _RL PP_timeAve(nSx,nSy)
      COMMON /PP81_TAVE_COUNT/ PP_timeAve

C----------------------------------------------------------------
C     PP*tave    - Time-averaging PP variables
C----------------------------------------------------------------

      _RL PPviscArtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL PPdiffKrtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /PP81_TAVE_FIELDS/
     &        PPviscArtave, PPdiffKrtave

#endif /* ALLOW_TIMEAVE */

#endif /* ALLOW_PP81 */
