C $Header: /u/gcmpack/MITgcm/pkg/my82/Attic/MY82_DIAGS.h,v 1.1 2004/09/02 09:11:54 mlosch Exp $
C $Name:  $

#ifdef ALLOW_MY82

C     /==========================================================\
C     | MY82_DIAGS.h                                              |
C     | o Header for MY82 diagnostic output                       |
C     \==========================================================/

C----------------------------------------------------------------
C     my_drctrec     - next record to dump for MY82 files
C----------------------------------------------------------------
      INTEGER my_drctrec
      COMMON /MY_RECORDNUM1/ my_drctrec

#ifdef ALLOW_TIMEAVE
C----------------------------------------------------------------
C     my_drctrecTave - next record to dump for MY82 averaging files
C----------------------------------------------------------------

      INTEGER my_drctrecTave
      COMMON /MY_RECORDNUM2/ my_drctrecTave

C----------------------------------------------------------------
C     my_TimeAve - time of temporal integration (s) for each thread
C----------------------------------------------------------------

      _RL my_TimeAve(Nr,nSx,nSy)
      COMMON /MY_TAVE/ my_TimeAve

C----------------------------------------------------------------
C     MY*tave    - Time-averaging MY variables
C----------------------------------------------------------------

      _RL MYhbltave     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL MYviscArtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL MYdiffKrtave  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /MY_TAVE_DIAGS/
     &        MYhbltave, MYviscArtave, MYdiffKrtave

#endif

#endif
