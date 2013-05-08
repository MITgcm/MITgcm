C $Header: /u/gcmpack/MITgcm/pkg/atm_phys/ATM_PHYS_VARS.h,v 1.1 2013/05/08 22:14:14 jmc Exp $
C $Name:  $

#ifdef ALLOW_ATM_PHYS

C-    AtmPhys 2-dim. fields
      _RL atmPhys_SST (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL atmPhys_Qflx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /ATMPHYS_2D_VARS/
     &    atmPhys_SST,
     &    atmPhys_Qflx

C-    AtmPhys 3-dim. fields
      _RL atmPhys_dT(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL atmPhys_dQ(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL atmPhys_dU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL atmPhys_dV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /ATMPHYS_3D_VARS/
     &    atmPhys_dT, atmPhys_dQ,
     &    atmPhys_dU, atmPhys_dV

#endif /* ALLOW_ATM_PHYS */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
