cswdptr --- added routine ----

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_TIMEAVE

CBOP
C     !ROUTINE: PTRACERS_STATV.h
C     !INTERFACE:
C     *================================================================*
C     | PTRACERS_STATV.h
C     | o Time averages of model state-variables 
C     *================================================================*
C     | Time average of state variables is (generally) centered on the
C     |  middle of the time step (time average interval = TimeAve_half)
C     | Time average of intermediate and tandancy variables is centered 
C     |  on the time step (time average interval=TimeAve_full)
C     *================================================================*
C     \ev
CEOP

C     ptracertave    :: tracer values

      COMMON /TAVE_PTRACER/ 
     &                  ptracertave, ptracer_half
      _RL  ptracertave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,
     &              PTRACERS_num)
      _RL ptracer_half(Nr,nSx,nSy)

#endif /* ALLOW_TIMEAVE */ 
#endif
