C $Header: /u/gcmpack/MITgcm/pkg/mom_common/MOM_VISC.h,v 1.1 2009/09/17 16:57:13 dfer Exp $
C $Name:  $

C- Common file for length scales 

#ifdef ALLOW_MOM_COMMON

      COMMON /MOM_VISC_L/ L2_D, L2_Z,
     &                    L3_D, L3_Z,
     &                    L4rdt_D, L4rdt_Z,
     &                    recip_dt
      _RL L2_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL L2_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL L3_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL L3_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL L4rdt_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL L4rdt_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL recip_dt

#endif /* ALLOW_MOM_COMMON */
