C $Header: /u/gcmpack/MITgcm/pkg/mom_fluxform/MOM_FLUXFORM.h,v 1.4 2014/04/04 19:55:51 jmc Exp $
C $Name:  $

#ifdef NONLIN_FRSURF
# ifndef DISABLE_RSTAR_CODE

CBOP
C     !ROUTINE: MOM_FLUXFORM.h
C     !INTERFACE:
C     #include MOM_FLUXFORM.h
C     !DESCRIPTION:
C     Header file for pkg mom_fluxform subroutines
CEOP

C--   COMMON /LOCAL_MOM_CALC_RTRANS/
C       was part of mom_calc_rtrans.F, and moved later
C       to a separated header file for the adjoint.
C     == Local variables in common block ==
C     dWtransC :: vertical transp. difference between r & r* coordinates
C     dWtransU :: same but above u.point location (West  face)
C     dWtransV :: same but above v.point location (South face)
#  ifndef ALLOW_OPENAD
      COMMON /LOCAL_MOM_CALC_RTRANS/
     &       dWtransC, dWtransU, dWtransV
#  endif
      _RL dWtransC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL dWtransU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL dWtransV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
# endif
#endif /* NONLIN_FRSURF */

