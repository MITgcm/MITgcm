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
C     GM_useK3D        :: use the 3 dimensional calculation for K
C     GM_K3D_beta_eq_0 :: Ignores the beta term when calculating grad(q)
C     GM_K3D_ThickSheet:: Use a thick PV sheet
C     GM_K3D_surfK     :: Imposes a constant K in the surface layer
C     GM_K3D_constRedi :: Imposes a constant K for the Redi isoneutral diffusivity
C     GM_K3D_use_constK:: Imposes a constant K for the eddy transport
C     GM_K3D_smooth    :: Expand PV closure in terms of baroclinic modes (=.FALSE. for debugging only!)
C     GM_useLeithQG    :: add Leith QG viscosity to GMRedi tensor
      LOGICAL GM_AdvForm
      LOGICAL GM_AdvSeparate
      LOGICAL GM_useBVP
      LOGICAL GM_useSubMeso
      LOGICAL GM_ExtraDiag
      LOGICAL GM_InMomAsStress
      LOGICAL GM_MNC
      LOGICAL GM_MDSIO
      LOGICAL GM_useK3D
      LOGICAL GM_K3D_ThickSheet
      LOGICAL GM_K3D_surfK
      LOGICAL GM_K3D_constRedi
      LOGICAL GM_K3D_use_constK
      LOGICAL GM_K3D_beta_eq_0
      LOGICAL GM_K3D_smooth
      LOGICAL GM_useLeithQG
      COMMON /GM_PARAMS_L/
     &                   GM_AdvForm, GM_AdvSeparate,
     &                   GM_useBVP,  GM_useSubMeso,
     &                   GM_ExtraDiag, GM_MNC, GM_MDSIO,
     &                   GM_InMomAsStress,
     &                   GM_useK3D, GM_K3D_smooth, GM_K3D_use_constK,
     &                   GM_K3D_beta_eq_0, GM_K3D_ThickSheet,
     &                   GM_K3D_surfK, GM_K3D_constRedi, GM_useLeithQG

C--   GM/Redi Integer-type parameters
C     GM_BVP_modeNumber :: vertical mode number used for speed "c" in BVP transport
C     GM_K3D_NModes :: number of vertical modes used for calculating Xi in K3D
      INTEGER GM_BVP_modeNumber
      INTEGER GM_K3D_NModes
      PARAMETER (GM_K3D_NModes=6)
      COMMON /GM_PARAMS_I/
     &                   GM_BVP_modeNumber

C--   COMMON /GM_PARAMS_C/ GM/Redi Character-type parameters
C     GM_taper_scheme :: select which tapering/clipping scheme to use
C     GM_iso2dFile :: input file for 2.D horiz scaling of Isopycnal diffusivity
C     GM_iso1dFile :: input file for 1.D vert. scaling of Isopycnal diffusivity
C     GM_bol2dFile :: input file for 2.D horiz scaling of Thickness diffusivity
C     GM_bol1dFile :: input file for 1.D vert. scaling of Thickness diffusivity
C     GM_isopycK3dFile :: input file for 3.D GM_isopycK
C     GM_background_K3dFile :: input file for 3.D GM_background_K

      CHARACTER*(40) GM_taper_scheme
      CHARACTER*(MAX_LEN_FNAM) GM_iso2dFile
      CHARACTER*(MAX_LEN_FNAM) GM_iso1dFile
      CHARACTER*(MAX_LEN_FNAM) GM_bol2dFile
      CHARACTER*(MAX_LEN_FNAM) GM_bol1dFile
      CHARACTER*(MAX_LEN_FNAM) GM_isopycK3dFile
      CHARACTER*(MAX_LEN_FNAM) GM_background_K3dFile
      COMMON /GM_PARAMS_C/
     &                   GM_taper_scheme,
     &                   GM_iso2dFile, GM_iso1dFile,
     &                   GM_bol2dFile, GM_bol1dFile,
     &                   GM_isopycK3dFile, GM_background_K3dFile

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
C-    Variable K with PV diffusion parameters:
C     GM_K3D_gamma   :: mixing efficiency for 3D eddy diffusivity [-]
C     GM_K3D_b1      :: an empirically determined constant of O(1)
C     GM_K3D_EadyMinDepth :: upper depth for Eady calculation
C     GM_K3D_EadyMaxDepth :: lower depth for Eady calculation
C     GM_maxK3D      :: Upper bound on the diffusivity
C     GM_K3D_constK  :: Constant diffusivity to use when GM_useK3D=.TRUE. and
C                       GM_K3D_use_constK=.TRUE. and/or GM_K3D_constRedi=.TRUE.
C     GM_K3D_Rmax    :: Upper bound on the length scale used for calculating urms
C     GM_K3D_Rmin    :: Lower bound on the length scale used for calculating the eddy radius
C     GM_K3D_minCori :: minimum value for f (to stop things blowing up near the equator)
C     GM_K3D_minN2   :: minimum value for the square of the buoyancy frequency
C     GM_K3D_surfMinDepth :: minimum value for the depth of the surface layer
C     GM_K3D_vecFreq :: Frequency at which to update the baroclinic modes
C     GM_K3D_minRenorm:: minimum value for the renormalisation factor
C     GM_K3D_maxRenorm:: maximum value for the renormalisation factor

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
      _RL GM_K3D_gamma
      _RL GM_K3D_b1
      _RL GM_K3D_EadyMinDepth
      _RL GM_K3D_EadyMaxDepth
      _RL GM_K3D_Lambda
      _RL GM_K3D_smallK
      _RL GM_K3D_maxC
      _RL GM_maxK3D
      _RL GM_K3D_constK
      _RL GM_K3D_Rmax
      _RL GM_K3D_Rmin
      _RL GM_K3D_minCori
      _RL GM_K3D_minN2
      _RL GM_K3D_surfMinDepth
      _RL GM_K3D_vecFreq
      _RL GM_K3D_minRenorm
      _RL GM_K3D_maxRenorm
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
     &                   GM_K3D_gamma, GM_K3D_b1, GM_K3D_EadyMinDepth,
     &                   GM_K3D_EadyMaxDepth, GM_K3D_Lambda,
     &                   GM_K3D_smallK, GM_K3D_maxC,
     &                   GM_maxK3D, GM_K3D_minCori, GM_K3D_minN2,
     &                   GM_K3D_surfMinDepth, GM_K3D_Rmax, GM_K3D_Rmin,
     &                   GM_K3D_constK, GM_K3D_vecFreq,
     &                   GM_K3D_minRenorm, GM_K3D_maxRenorm,
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
      COMMON /GM_Wtensor/ Kwx, Kwy, Kwz

C     Horizontal part of the tensor
C     Kux :: K_11 element of GM/Redi tensor, X direction at U point
C     Kvy :: K_22 element of GM/Redi tensor, Y direction at V point
      _RL Kux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL Kvy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /GM_HorTensor/ Kux, Kvy

#ifdef GM_EXTRA_DIAGONAL
C     First/second rows of tensor corresponds to U/V points
C     Kuz :: K_13 element of GM/Redi tensor, Z direction at U point
C     Kvz :: K_23 element of GM/Redi tensor, Z direction at V point
      _RL Kuz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL Kvz(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /GM_UVtensor/ Kuz, Kvz
#else
      _RL Kuz, Kvz
      PARAMETER( Kuz=1., Kvz=1. )
#endif

#ifdef GM_BOLUS_ADVEC
C     GM advection formulation: bolus velocities are derived from 2
C        streamfunctions PsiX and PsiY :
      _RL GM_PsiX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL GM_PsiY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /GM_BOLUS/ GM_PsiX, GM_PsiY
#endif

#ifdef GM_VISBECK_VARIABLE_K
C     GM mixing/stirring coefficient (spatially variable in horizontal
C     for Visbeck et al. parameterization)
      _RL VisbeckK(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /GM_Visbeck/ VisbeckK
#endif

#ifdef GM_K3D
C     K3D          :: The three dimensional eddy mixing coeffixint [m**2/s]
C     modesC       :: First baroclinic mode at the centre of a tracer cell [-]
C     modesW       :: First N baroclinic mode at the western face of a tracer cell [-]
C     modesS       :: First N baroclinic mode at the southern face of a tracer cell [-]
C     Rdef         :: Deformation radius [m]
C     gradf        :: gradient of the Coriolis paramater at a cell centre, 1/(m*s)

      _RL K3D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      _RL modesC(1,1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      _RL modesW(GM_K3D_NModes,1-OLx:sNx+OLx,
     &     1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      _RL modesS(GM_K3D_NModes,1-OLx:sNx+OLx,
     &     1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      _RL Rdef(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gradf(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      COMMON /GM_K3D/ K3D, modesC, modesW, modesS, Rdef, gradf
#endif

#ifdef ALLOW_GM_LEITH_QG
C     GM_LeithQG_K :: Horizontal LeithQG viscosity, to add to GM coefficient
      _RL GM_LeithQG_K(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /GM_LEITH_QG/ GM_LeithQG_K
#endif /* ALLOW_GM_LEITH_QG */

#endif /* ALLOW_GMREDI */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
