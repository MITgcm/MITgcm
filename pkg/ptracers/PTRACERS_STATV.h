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

C     ptracerFluxtave: surface ptracer flux (mol/m2/s, >0 for increase in ptracer)
C     ptracertave    : tracer values (mol/m-3)

      COMMON /TAVE_PTRACER/ 
     &                  ptracerFluxtave, ptracertave,
     &                  ptracer_half   , ptracer_full
      _RL ptracerFluxtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,   nSx,nSy,
     &                                                     PTRACERS_num)
      _RL ptracertave    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,
     &                                                     PTRACERS_num)
      _RL ptracer_half(Nr,nSx,nSy), ptracer_full(Nr,nSx,nSy)

#endif /* ALLOW_TIMEAVE */ 
#endif
