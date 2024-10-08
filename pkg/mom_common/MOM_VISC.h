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

C--   COMMON /MOM_FLUXFORM_GRID/ just to hold a copy of deepFacC
C     deepFacAdv :: copy of deepFacC or just Nr-length vector of 1 when
C                   MOM_USE_OLD_DEEP_VERT_ADV is defined or useNHMTerms=F
      COMMON /MOM_GRID_COPY/ deepFacAdv
      _RL deepFacAdv(Nr)

#ifdef ALLOW_SMAG_3D
C     smag3D_hLsC :: horiz. grid length scale (power 2/3) at grid cell center
C     smag3D_hLsW :: horiz. grid length scale (power 2/3) at western  edge
C     smag3D_hLsS :: horiz. grid length scale (power 2/3) at southern egde
C     smag3D_hLsZ :: horiz. grid length scale (power 2/3) at grid cell corner
      COMMON /MOM_SMAG_3D_LENGTH/
     &        smag3D_hLsC, smag3D_hLsW,
     &        smag3D_hLsS, smag3D_hLsZ
      _RS smag3D_hLsC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS smag3D_hLsW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS smag3D_hLsS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS smag3D_hLsZ(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* ALLOW_SMAG_3D */

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

#ifdef ALLOW_BOTTOMDRAG_ROUGHNESS
C--   bottom drag coefficents as a function of grid cell thickness
C     and roughness length
      COMMON /MOM_DRAGCOEFFS_RS/
     &     bottomDragCoeffW, bottomDragCoeffS
      _RS bottomDragCoeffW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS bottomDragCoeffS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

#endif /* ALLOW_MOM_COMMON */
