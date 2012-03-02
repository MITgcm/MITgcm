C $Header: /u/gcmpack/MITgcm/pkg/frazil/FRAZIL.h,v 1.1 2012/03/02 01:45:22 dimitri Exp $
C $Name:  $

#ifdef ALLOW_FRAZIL

C     frazil_TendT           - temperature tendency (Kelvin/s)
      _RL frazil_TendT(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /FRAZIL_TENDENCY/ frazil_TendT

#endif /* ALLOW_FRAZIL */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
