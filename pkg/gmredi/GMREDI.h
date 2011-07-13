C $Header: /u/gcmpack/MITgcm/pkg/gmredi/GMREDI.h,v 1.19 2011/07/13 22:59:53 jmc Exp $
C $Name:  $

#ifdef ALLOW_GMREDI

C---  GM/Redi package parameters

C--   Numerical Constant
      _RL op5
      _RL op25
      PARAMETER( op5  = 0.5 _d 0 )
      PARAMETER( op25 = 0.25 _d 0 )

C--   COMMON /GM_PARAMS_L/ GM/Redi Logical-type parameters
C     GM_AdvForm       :: use Advective Form (instead of Skew-Flux form)
C     GM_AdvSeparate   :: do separately advection by Eulerian and Bolus velocity
C     GM_useBVP        :: use Boundary-Value-Problem method for Bolus transport
C     GM_useSubMeso    :: use parameterization of mixed layer (Sub-Mesoscale) eddies
C     GM_ExtraDiag     :: select extra diagnostics
C     GM_InMomAsStress :: apply GM as a stress in momentum Eq.
C     GM_MNC           ::
C     GM_MDSIO         ::
      LOGICAL GM_AdvForm
      LOGICAL GM_AdvSeparate
      LOGICAL GM_useBVP
      LOGICAL GM_useSubMeso
      LOGICAL GM_ExtraDiag
      LOGICAL GM_InMomAsStress
      LOGICAL GM_MNC
      LOGICAL GM_MDSIO
      COMMON /GM_PARAMS_L/
     &                   GM_AdvForm, GM_AdvSeparate,
     &                   GM_useBVP,  GM_useSubMeso,
     &                   GM_ExtraDiag, GM_MNC, GM_MDSIO,
     &                   GM_InMomAsStress

C--   GM/Redi Integer-type parameters
C     GM_BVP_modeNumber :: vertical mode number used for speed "c" in BVP transport
      INTEGER GM_BVP_modeNumber
      COMMON /GM_PARAMS_I/
     &                   GM_BVP_modeNumber

C--   COMMON /GM_PARAMS_C/ GM/Redi Character-type parameters
C     GM_taper_scheme :: select which tapering/clipping scheme to use
C     GM_iso2dFile :: input file for 2.D horiz scaling of Isopycnal diffusivity
C     GM_iso1dFile :: input file for 1.D vert. scaling of Isopycnal diffusivity
C     GM_bol2dFile :: input file for 2.D horiz scaling of Thickness diffusivity
C     GM_bol1dFile :: input file for 1.D vert. scaling of Thickness diffusivity
      CHARACTER*(40) GM_taper_scheme
      CHARACTER*(MAX_LEN_FNAM) GM_iso2dFile
      CHARACTER*(MAX_LEN_FNAM) GM_iso1dFile
      CHARACTER*(MAX_LEN_FNAM) GM_bol2dFile
      CHARACTER*(MAX_LEN_FNAM) GM_bol1dFile
      COMMON /GM_PARAMS_C/
     &                   GM_taper_scheme,
     &                   GM_iso2dFile, GM_iso1dFile,
     &                   GM_bol2dFile, GM_bol1dFile

C--   COMMON /GM_PARAMS_R/ GM/Redi real-type parameters
C     GM_isopycK       :: Isopycnal diffusivity [m^2/s] (Redi-tensor)
C     GM_background_K  :: Thickness diffusivity [m^2/s] (GM bolus transport)
C     GM_maxSlope      :: maximum slope (tapering/clipping) [-]
C     GM_Kmin_horiz    :: minimum horizontal diffusivity [m^2/s]
C     GM_Small_Number  :: epsilon used in computing the slope
C     GM_slopeSqCutoff :: slope^2 cut-off value
C-    Transition layer thickness definition:
C     GM_facTrL2dz   :: minimum Trans. Layer Thick. as a factor of local dz
C     GM_facTrL2ML   :: maximum Trans. Layer Thick. as a factor of Mix-Layer Depth
C     GM_maxTransLay :: maximum Trans. Layer Thick. [m]
C-    Boundary-Value-Problem method parameters:
C     GM_BVP_cMin    :: minimum value for wave speed parameter "c" in BVP [m/s]
C-    mixed layer (Sub-Mesoscale) eddies parameterization:
C     subMeso_Ceff   :: efficiency coefficient of Mixed-Layer Eddies [-]
C     subMeso_invTau :: inverse of mixing time-scale in sub-meso parameteriz. [s^-1]
C     subMeso_LfMin  :: minimum value for length-scale "Lf" [m]
C     subMeso_Lmax   :: maximum horizontal grid-scale length [m]

      _RL GM_isopycK
      _RL GM_background_K
      _RL GM_maxSlope
      _RL GM_Kmin_horiz
      _RL GM_Small_Number
      _RL GM_slopeSqCutoff
      _RL GM_Visbeck_alpha
      _RL GM_Visbeck_length
      _RL GM_Visbeck_depth
      _RL GM_Visbeck_minDepth
      _RL GM_Visbeck_maxSlope
      _RL GM_Visbeck_minVal_K
      _RL GM_Visbeck_maxVal_K
      _RL GM_facTrL2dz
      _RL GM_facTrL2ML
      _RL GM_maxTransLay
      _RL GM_Scrit
      _RL GM_Sd
      _RL GM_BVP_cMin
      _RL subMeso_Ceff
      _RL subMeso_invTau
      _RL subMeso_LfMin
      _RS subMeso_Lmax
      COMMON /GM_PARAMS_RL/
     &                   GM_isopycK, GM_background_K,
     &                   GM_maxSlope,
     &                   GM_Kmin_horiz,
     &                   GM_Small_Number, GM_slopeSqCutoff,
     &                   GM_Visbeck_alpha, GM_Visbeck_length,
     &                   GM_Visbeck_depth,
     &                   GM_Visbeck_minDepth, GM_Visbeck_maxSlope,
     &                   GM_Visbeck_minVal_K, GM_Visbeck_maxVal_K,
     &                   GM_facTrL2dz, GM_facTrL2ML, GM_maxTransLay,
     &                   GM_Scrit, GM_Sd, GM_BVP_cMin,
     &                   subMeso_Ceff, subMeso_invTau, subMeso_LfMin
      COMMON /GM_PARAMS_RS/
     &                   subMeso_Lmax

C--   COMMON /GM_DERIVED_PAR/ other GM/Redi parameters
C     (derived from previous block and not directly user configured)
      _RL GM_rMaxSlope
      _RL GM_skewflx
      _RL GM_advect
      _RL GM_BVP_rModeNumber
      _RL GM_BVP_cHat2Min
      COMMON /GM_DERIVED_PAR/
     &                   GM_rMaxSlope,
     &                   GM_skewflx, GM_advect,
     &                   GM_BVP_rModeNumber, GM_BVP_cHat2Min


C--   COMMON /GM_COEFFICIENTS/ GM/Redi scaling coefficients
C     defined at grid-cell center (tracer location)
C     GM_isoFac2d  :: 2.D horiz scaling factor [-] of Isopycnal diffusivity
C     GM_bolFac2d  :: 2.D horiz scaling factor [-] of Thickness diffusivity
C     GM_isoFac1d  :: 1.D vert. scaling factor [-] of Isopycnal diffusivity
C     GM_bolFac1d  :: 1.D vert. scaling factor [-] of Thickness diffusivity
      _RS GM_isoFac2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS GM_bolFac2d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS GM_isoFac1d(Nr)
      _RS GM_bolFac1d(Nr)
      COMMON /GM_COEFFICIENTS/
     &  GM_isoFac2d, GM_bolFac2d, GM_isoFac1d, GM_bolFac1d

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C---  GM/Redi tensor elements

C     Bottom row of tensor corresponds to W points
C     Kwx :: K_31 element of GM/Redi tensor, X direction at W point
C     Kwy :: K_32 element of GM/Redi tensor, Y direction at W point
C     Kwz :: K_33 element of GM/Redi tensor, Z direction at W point
      _RL Kwx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL Kwy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL Kwz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /GM_Wtensor/ Kwx,Kwy,Kwz

#ifdef GM_NON_UNITY_DIAGONAL
C     Horizontal part of the tensor
C     Kux :: K_11 element of GM/Redi tensor, X direction at U point
C     Kvy :: K_22 element of GM/Redi tensor, Y direction at V point
      _RL Kux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL Kvy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /GM_HorTensor/ Kux,Kvy
#else
      _RL Kux,Kvy
      PARAMETER(Kux=1.,Kvy=1.)
#endif

#ifdef GM_EXTRA_DIAGONAL
C     First/second rows of tensor corresponds to U/V points
C     Kuz :: K_13 element of GM/Redi tensor, Z direction at U point
C     Kvz :: K_23 element of GM/Redi tensor, Z direction at V point
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
