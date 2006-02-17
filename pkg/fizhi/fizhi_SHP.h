C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_SHP.h,v 1.1 2006/02/17 16:04:00 molod Exp $
C $Name:  $


C The physics state uses the dynamics dimensions in the horizontal
C     and the land dimensions in the horizontal for turbulence variables
c
c Secret Hiding Place - Use to compute gridalt correction term diagnostics
c ------------------------------------------------------------------------
      common /fizhi_SHP/ ubef,vbef,thbef,sbef,
     .                   udynbef,vdynbef,thdynbef,sdynbef

      _RL ubef(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,Nsx,Nsy)
      _RL vbef(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,Nsx,Nsy)
      _RL thbef(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,Nsx,Nsy)
      _RL sbef(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nrphys,Nsx,Nsy)
      _RL udynbef(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,Nsx,Nsy)
      _RL vdynbef(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,Nsx,Nsy)
      _RL thdynbef(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,Nsx,Nsy)
      _RL sdynbef(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,Nsx,Nsy)
