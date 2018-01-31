#ifdef ALLOW_OPPS

C     *==========================================================*
C     | OPPS_TAVE.h
C     | o Header for OPPS time-average output
C     *==========================================================*

#ifdef ALLOW_TIMEAVE
C----------------------------------------------------------------
C     OPPS_timeAve - time of temporal integration (s) for each thread
C----------------------------------------------------------------

      _RL OPPS_timeAve(nSx,nSy)
      COMMON /OPPS_TAVE_COUNT/ OPPS_timeAve

C----------------------------------------------------------------
C     OPPS*tave    - Time-averaging OPPS variables
C----------------------------------------------------------------

      _RL OPPSconvCountTave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /OPPS_TAVE_FIELDS/
     &     OPPSconvCountTave

#endif /* ALLOW_TIMEAVE */

#endif /* ALLOW_OPPS */
