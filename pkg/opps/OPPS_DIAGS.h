C $Header: /u/gcmpack/MITgcm/pkg/opps/Attic/OPPS_DIAGS.h,v 1.1 2004/09/16 11:28:16 mlosch Exp $
C $Name:  $

#ifdef ALLOW_OPPS

C     /==========================================================\
C     | OPPS_DIAGS.h                                             |
C     | o Header for OPPS diagnostic output                      |
C     \==========================================================/

C----------------------------------------------------------------
C     opps_drctrec     - next record to dump for PP files
C----------------------------------------------------------------
      INTEGER opps_drctrec
      COMMON /OPPS_RECORDNUM1/ opps_drctrec

#ifdef ALLOW_TIMEAVE
C----------------------------------------------------------------
C     opps_drctrecTave - next record to dump for PP averaging files
C----------------------------------------------------------------

      INTEGER opps_drctrecTave
      COMMON /OPPS_RECORDNUM2/ opps_drctrecTave

C----------------------------------------------------------------
C     opps_TimeAve - time of temporal integration (s) for each thread
C----------------------------------------------------------------

      _RL opps_TimeAve(Nr,nSx,nSy)
      COMMON /OPPS_TAVE/ opps_TimeAve

C----------------------------------------------------------------
C     OPPS*tave    - Time-averaging OPPS variables
C----------------------------------------------------------------

      _RL OPPSconvectCountTave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                         Nr,nSx,nSy)
      COMMON /OPPS_TAVE_DIAGS/
     &     OPPSconvectCountTave

#endif /* ALLOW_TIMEAVE */

#endif /* ALLOW_OPPS */
