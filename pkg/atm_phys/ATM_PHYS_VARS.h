#ifdef ALLOW_ATM_PHYS

C-    AtmPhys 2-dim. fields
      _RL atmPhys_SST (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL atmPhys_Qflx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL atmPhys_MxLD(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL atmPhys_Albedo(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /ATMPHYS_2D_VARS/
     &    atmPhys_SST,
     &    atmPhys_Qflx,
     &    atmPhys_MxLD,
     &    atmPhys_Albedo

C-    AtmPhys 3-dim. fields
C     atmPhys_Ozone :: ozone concentration [volume mixing ratio <-> mol/mol]
      _RL atmPhys_dT(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL atmPhys_dQ(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL atmPhys_dU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL atmPhys_dV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL atmPhys_Ozone(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /ATMPHYS_3D_VARS/
     &    atmPhys_dT, atmPhys_dQ,
     &    atmPhys_dU, atmPhys_dV,
     &    atmPhys_Ozone

#endif /* ALLOW_ATM_PHYS */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
