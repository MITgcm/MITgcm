C $Header: /u/gcmpack/MITgcm/pkg/frazil/FRAZIL.h,v 1.2 2012/03/03 16:04:38 dimitri Exp $
C $Name:  $

#ifdef ALLOW_FRAZIL

C     FrazilForcingT : frazil temperature forcing, > 0 increases theta [W/m^2]
      _RL FrazilForcingT(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /FRAZIL_FORCING/ FrazilForcingT

#endif /* ALLOW_FRAZIL */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
