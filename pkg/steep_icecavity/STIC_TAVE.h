C     *==========================================================*
C     | STIC_TAVE.h
C     | o Header for STIC time-average output
C     *==========================================================*

#ifdef ALLOW_TIMEAVE

C     Keep track of time
      _RL STIC_timeAve(nSx,nSy)
      COMMON /STIC_TAVE_COUNT/ STIC_timeAve

C     Storage arrays for time-averages
      _RL shelfice_heatFluxtave
     &     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      _RL shelfice_frshFluxtave
     &     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,nSx,nSy)
      COMMON /STIC_TAVE_VARS/
     &     shelfice_heatFluxtave,
     &     shelfice_frshFluxtave

#endif /* ALLOW_TIMEAVE */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
