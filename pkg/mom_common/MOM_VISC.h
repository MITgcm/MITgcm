C $Header: /u/gcmpack/MITgcm/pkg/mom_common/MOM_VISC.h,v 1.4 2013/07/28 21:02:33 jmc Exp $
C $Name:  $

C- Common file for length scales

#ifdef ALLOW_MOM_COMMON

C--   COMMON /MOM_VISC_PAR_L/ logical-type parameters for Momemtum viscosity
C     useHarmonicVisc   :: harmonic   horizontal viscosity is used
C     useBiharmonicVisc :: biharmonic horizontal viscosity is used
C     useVariableVisc   :: variable (in space or time) viscosity is used
      COMMON /MOM_VISC_PAR_L/
     &        useHarmonicVisc, useBiharmonicVisc, useVariableVisc
      LOGICAL useHarmonicVisc, useBiharmonicVisc, useVariableVisc

      COMMON /MOM_VISC_LENGTH/ L2_D, L2_Z,
     &                         L3_D, L3_Z,
     &                         L4rdt_D, L4rdt_Z
      _RL L2_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL L2_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL L3_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL L3_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL L4rdt_D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL L4rdt_Z(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_3D_VISCAH
C     viscAhDfld, viscAhZfld :: full 3D specification of Laplacian Viscosity
C               coeff. for mixing of momentum horizontally ( units of m^2/s )
      COMMON /MOM_VISC_3D_VISCAH/
     &                  viscAhDfld, viscAhZfld
      _RL  viscAhDfld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  viscAhZfld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

#ifdef ALLOW_3D_VISCA4
C     viscA4Dfld, viscA4Zfld :: full 3D specification of Bi-harmonic Viscosity
C               coeff. for mixing of momentum horizontally ( units of m^4/s )
      COMMON /MOM_VISC_3D_VISCA4/
     &                  viscA4Dfld, viscA4Zfld
      _RL  viscA4Dfld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  viscA4Zfld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

#ifdef ALLOW_NONHYDROSTATIC
C     viscAh_W  :: Horizontal  harmonic  viscosity for vertical momentum
C     viscA4_W  :: Horizontal biharmonic viscosity for vertical momentum
      COMMON /MOM_VISC_NH/
     &                  viscAh_W, viscA4_W
      _RL  viscAh_W(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  viscA4_W(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif /* ALLOW_NONHYDROSTATIC */

#endif /* ALLOW_MOM_COMMON */
