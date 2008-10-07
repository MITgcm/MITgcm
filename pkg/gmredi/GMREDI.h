C $Header: /u/gcmpack/MITgcm/pkg/gmredi/GMREDI.h,v 1.15 2008/10/07 20:15:27 jmc Exp $
C $Name:  $

#ifdef ALLOW_GMREDI

C---  GM/Redi package parameters

C--   Numerical Constant
      _RL op5
      _RL op25
      PARAMETER( op5 = 0.5D0 )
      PARAMETER( op25 = 0.25D0 )

C--   GM/Redi Logical-type parameters
      LOGICAL GM_AdvForm
      LOGICAL GM_AdvSeparate
      LOGICAL GM_ExtraDiag
      LOGICAL GM_InMomAsStress
      LOGICAL GM_MNC
      LOGICAL GM_MDSIO
      COMMON /GM_PARAMS_L/
     &                   GM_AdvForm, GM_AdvSeparate,
     &                   GM_ExtraDiag, GM_MNC, GM_MDSIO,
     &                   GM_InMomAsStress

C--   GM/Redi Character-type parameters
C     GM_taper_scheme  :: select which tapering/clipping scheme to use
      CHARACTER*(40) GM_taper_scheme
      COMMON /GM_PARAMS_C/
     &                   GM_taper_scheme

C--   GM/Redi real-type parameters
C     GM_isopycK       :: Isopycnal diffusivity [m^2/s] (Redi-tensor)
C     GM_background_K  :: Thickness diffusivity [m^2/s] (GM bolus transport)
C     GM_maxSlope      :: maximum slope (tapering/clipping) [-]
C     GM_Kmin_horiz    :: minimum horizontal diffusivity [m^2/s]
C     GM_Small_Number  :: epsilon used in computing the slope
C     GM_slopeSqCutoff :: slope^2 cut-off value
C-    transition layer thickness definition:
C     GM_facTrL2dz   :: minimum Trans. Layer Thick. as a factor of local dz
C     GM_facTrL2ML   :: maximum Trans. Layer Thick. as a factor of Mix-Layer Depth
C     GM_maxTransLay :: maximum Trans. Layer Thick. [m]
      _RL GM_isopycK
      _RL GM_background_K
      _RL GM_maxSlope
      _RL GM_Kmin_horiz
      _RL GM_Small_Number
      _RL GM_slopeSqCutoff
      _RL GM_Visbeck_alpha
      _RL GM_Visbeck_length
      _RL GM_Visbeck_depth
      _RL GM_Visbeck_maxval_K
      _RL GM_facTrL2dz
      _RL GM_facTrL2ML
      _RL GM_maxTransLay
      _RL GM_Scrit
      _RL GM_Sd
      COMMON /GM_PARAMS_R/
     &                   GM_isopycK, GM_background_K,
     &                   GM_maxSlope,
     &                   GM_Kmin_horiz,
     &                   GM_Small_Number, GM_slopeSqCutoff,
     &                   GM_Visbeck_alpha,
     &                   GM_Visbeck_length,
     &                   GM_Visbeck_depth,
     &                   GM_Visbeck_maxval_K,
     &                   GM_facTrL2dz, GM_facTrL2ML, GM_maxTransLay,
     &                   GM_Scrit, GM_Sd

C--   More GM/Redi parameters derived from previous block
C     (not directly user configured)
      _RL GM_rMaxSlope
      _RL GM_skewflx
      _RL GM_advect
      COMMON /GM_DERIVED_PAR/
     &                   GM_rMaxSlope,
     &                   GM_skewflx, GM_advect

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C---  GM/Redi tensor elements

C     Bottom row of tensor corresponds to W points
C     Kwx is K_31 element, X direction at W point
C     Kwy is K_32 element, Y direction at W point
C     Kwz is K_33 element, Z direction at W point
      _RL Kwx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL Kwy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL Kwz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /GM_Wtensor/ Kwx,Kwy,Kwz

#ifdef GM_NON_UNITY_DIAGONAL
C     Horizontal part of the tensor
C     Kux is K_11 element, X direction at U point
C     Kvy is K_22 element, Y direction at V point
      _RL Kux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL Kvy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /GM_HorTensor/ Kux,Kvy
#else
      _RL Kux,Kvy
      PARAMETER(Kux=1.,Kvy=1.)
#endif

#ifdef GM_EXTRA_DIAGONAL
C     First/second rows of tensor corresponds to U/V points
C     Kuz is K_13 element, Z direction at U point
C     Kvz is K_23 element, Z direction at V point
      _RL Kuz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL Kvz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /GM_UVtensor/ Kuz,Kvz
#else
      _RL Kuz,Kvz
      PARAMETER(Kuz=1.,Kvz=1.)
#endif

#ifdef GM_BOLUS_ADVEC
C     GM advection formulation: bolus velocities are derived from 2
C        streamfunctions PsiX and PsiY :
      _RL GM_PsiX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL GM_PsiY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /GM_BOLUS/ GM_PsiX,GM_PsiY
#endif

#ifdef GM_VISBECK_VARIABLE_K
C     GM mixing/stirring coefficient (spatially variable in horizontal
C     for Visbeck et al. parameterization)
      _RL VisbeckK(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /GM_Visbeck/ VisbeckK
#endif

#endif /* ALLOW_GMREDI */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
