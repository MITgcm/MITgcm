C $Header: /u/gcmpack/MITgcm/pkg/mom_common/MOM_VISC.h,v 1.2 2010/02/17 23:39:37 gforget Exp $
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

#ifdef ALLOW_3D_VISCAH
C     viscAhDfld, viscAhZfld :: full 3D specification of Laplacian Viscosity
C               coeff. for mixing of momentum horizontally ( units of r^2/s )
      COMMON /DYNVARS_3D_VISCAH/
     &                  viscAhDfld, viscAhZfld
      _RL  viscAhDfld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  viscAhZfld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

#ifdef ALLOW_3D_VISCA4
C     viscA4Dfld, viscA4Zfld :: full 3D specification of Bi-harmonic Viscosity
C               coeff. for mixing of momentum horizontally ( units of r^2/s )
      COMMON /DYNVARS_3D_VISCA4/
     &                  viscA4Dfld, viscA4Zfld
      _RL  viscA4Dfld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  viscA4Zfld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

#endif /* ALLOW_MOM_COMMON */
