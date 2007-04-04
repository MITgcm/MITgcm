C $Header: /u/gcmpack/MITgcm/pkg/thsice/Attic/THSICE_2DYN.h,v 1.1 2007/04/04 02:40:42 jmc Exp $
C $Name:  $

#ifdef ALLOW_THSICE

C     !ROUTINE: THSICE_2DYN.h
C -------------------------------
C   THSICE_2DYN.h
C  variable for thermodynamics sea-ice interface with dynamics
C -------------------------------

C-- COMMON /THSICE_DYN_R/  interface real variables :
C   uc/vc     :: current ice velocity on C-grid [m/s]
C   oceFWfx   :: fresh water flux to the ocean  [kg/m^2/s]
C   oceSflx   :: salt flux to the ocean         [psu.kg/m^2/s] (~g/m^2/s)
C   oceQnet   :: heat flux to the ocean         [W/m^2]
C---
C    Note :: when ice volume is too small to be kept, ice & snow is melt
C        and fresh water, salt and heat are returned to the ocean.
C        For now, those fluxes are stored separately and will try to find
C        out how to incorporate them more naturally in the usual forcing.
C---
      COMMON /THSICE_DYN_R/
c    &       uc, vc,
     &       oceFWfx, oceSflx, oceQnet

c     _RL uc     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL vc     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL oceFWfx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL oceSflx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL oceQnet(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#endif /* ALLOW_THSICE */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
