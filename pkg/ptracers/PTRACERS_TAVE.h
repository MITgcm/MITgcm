C $Header: /u/gcmpack/MITgcm/pkg/ptracers/PTRACERS_TAVE.h,v 1.1 2010/01/02 23:45:46 jmc Exp $
C $Name:  $

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_TIMEAVE

CBOP
C     !ROUTINE: PTRACERS_TAVE.h
C     !INTERFACE:
C     include "PTRACERS_TAVE.h"
C     !DESCRIPTION:
C     \bv
C     *================================================================*
C     | PTRACERS_TAVE.h
C     | o Time averages of pTracers variables
C     *================================================================*
C     | Time average of state variables is (generally) centered on the
C     |  middle of the time step (time average interval = TimeAve_half)
C     | Time average of intermediate and tandancy variables is centered
C     |  on the time step (time average interval=TimeAve_full)
C     *================================================================*
C     \ev
CEOP

C--   COMMON /PTRACER_TAVE_VARS/ time-averaged variables
C     ptracerFluxtave :: surface ptracer flux (mol/m2/s, >0 for increase in ptracer)
C     ptracertave     :: tracer values (mol/m3)
C     ptracer_half    :: cumulated time
C     ptracer_full    :: cumulated time
      COMMON /PTRACER_TAVE_VARS/
     &                  ptracerFluxtave, ptracertave,
     &                  ptracer_half   , ptracer_full

      _RL ptracerFluxtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,   nSx,nSy,
     &                                                     PTRACERS_num)
      _RL ptracertave    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,
     &                                                     PTRACERS_num)
      _RL ptracer_half(nSx,nSy), ptracer_full(nSx,nSy)

#endif /* ALLOW_TIMEAVE */
#endif /* ALLOW_PTRACERS */


CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
