C $Header: /u/gcmpack/MITgcm/pkg/pp81/Attic/PP81_DIAGS.h,v 1.1 2004/09/02 09:11:54 mlosch Exp $
C $Name:  $

#ifdef ALLOW_PP81

C     /==========================================================\
C     | PP81_DIAGS.h                                             |
C     | o Header for PP81 diagnostic output                      |
C     \==========================================================/

C----------------------------------------------------------------
C     pp_drctrec     - next record to dump for PP files
C----------------------------------------------------------------
      INTEGER pp_drctrec
      COMMON /PP81_RECORDNUM1/ pp_drctrec

#ifdef ALLOW_TIMEAVE
C----------------------------------------------------------------
C     pp_drctrecTave - next record to dump for PP averaging files
C----------------------------------------------------------------

      INTEGER pp_drctrecTave
      COMMON /PP81_RECORDNUM2/ pp_drctrecTave

C----------------------------------------------------------------
C     pp_TimeAve - time of temporal integration (s) for each thread
C----------------------------------------------------------------

      _RL pp_TimeAve(Nr,nSx,nSy)
      COMMON /PP81_TAVE/ pp_TimeAve

C----------------------------------------------------------------
C     PP*tave    - Time-averaging PP variables
C----------------------------------------------------------------

      _RL PPviscArtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL PPdiffKrtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /PP81_TAVE_DIAGS/
     &        PPviscArtave, PPdiffKrtave

#endif /* ALLOW_TIMEAVE */

#endif /* ALLOW_PP81 */
