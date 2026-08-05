#ifdef ALLOW_GMREDI
CBOP
C     !ROUTINE: GMREDI.h
C     !INTERFACE:
C     #include "GMREDI.h"

C     !DESCRIPTION:
C     *================================================================*
C     | GMREDI.h
C     | o Header file defining GM/Redi parameters and variables
C     *================================================================*
CEOP

C--   Numerical Constant
      _RL op5
      _RL op25
      PARAMETER( op5  = 0.5 _d 0 )
      PARAMETER( op25 = 0.25 _d 0 )

C--   COMMON /GM_PARAMS_L/ GM/Redi Logical-type parameters
C     GM_AdvForm       :: use Advective Form (instead of Skew-Flux form)
C     GM_AdvSeparate   :: do separately advection by Eulerian and Bolus velocity
C     GM_useBVP        :: use Boundary-Value-Problem method for Bolus transport
C     GM_useSubMeso    :: use parameterization of mixed layer (Sub-Mesoscale)
C                         eddies
C     GM_ExtraDiag     :: use Extra Off-Diagonal tensor elements (Kuz,Kvz)
C     GM_InMomAsStress :: apply GM as a stress in momentum Eq.
C     GM_MNC           ::
C     GM_MDSIO         ::
C     GM_useBatesK3d     :: use Bates etal (2014) calculation for 3-d K
C     GM_Bates_beta_eq_0 :: Ignores the beta term when calculating grad(q)
C     GM_Bates_ThickSheet:: Use a thick PV sheet
C     GM_Bates_surfK     :: Imposes a constant K in the surface layer
C     GM_Bates_constRedi :: Imposes a constant K for the Redi diffusivity
C     GM_Bates_use_constK:: Imposes a constant K for the eddy transport
C     GM_Bates_smooth    :: Expand PV closure in terms of baroclinic modes
C                           (=.FALSE. for debugging only!)
C     GM_useLeithQG    :: add Leith QG viscosity to GMRedi tensor
C     GM_useGEOM       :: use the GEOME formulation to calculate kgm
C     GEOM_vert_struc  :: allow for N2 structure function

      LOGICAL GM_AdvForm
      LOGICAL GM_AdvSeparate
      LOGICAL GM_useBVP
      LOGICAL GM_useSubMeso
      LOGICAL GM_ExtraDiag
      LOGICAL GM_InMomAsStress
      LOGICAL GM_MNC
      LOGICAL GM_MDSIO
      LOGICAL GM_useBatesK3d
      LOGICAL GM_Bates_ThickSheet
      LOGICAL GM_Bates_surfK
      LOGICAL GM_Bates_constRedi
      LOGICAL GM_Bates_use_constK
      LOGICAL GM_Bates_beta_eq_0
      LOGICAL GM_Bates_smooth
      LOGICAL GM_useLeithQG
      LOGICAL GM_useGEOM
      LOGICAL GEOM_vert_struc
      COMMON /GM_PARAMS_L/
     &                   GM_AdvForm, GM_AdvSeparate,
     &                   GM_useBVP,  GM_useSubMeso,
     &                   GM_ExtraDiag, GM_MNC, GM_MDSIO,
     &                   GM_InMomAsStress,
     &                   GM_useBatesK3d, GM_Bates_smooth,
     &                   GM_Bates_use_constK, GM_Bates_beta_eq_0,
     &                   GM_Bates_ThickSheet, GM_Bates_surfK,
     &                   GM_Bates_constRedi,
     &                   GM_useLeithQG,
     &                   GM_useGEOM, GEOM_vert_struc

C--   GM/Redi Integer-type parameters
C     GM_BVP_modeNumber :: vertical mode number used for speed "c" in BVP transport
C     GM_Bates_NModes :: number of vertical modes used for calculating Xi in GM_BatesK3d
      INTEGER GM_BVP_modeNumber
      INTEGER GM_Bates_NModes
      PARAMETER (GM_Bates_NModes=6)
      COMMON /GM_PARAMS_I/
     &                   GM_BVP_modeNumber

C--   COMMON /GM_PARAMS_C/ GM/Redi Character-type parameters
C     GM_taper_scheme :: select which tapering/clipping scheme to use
C     GM_iso2dFile :: input file for 2.D horiz scaling of Isopycnal diffusivity
C     GM_iso1dFile :: input file for 1.D vert. scaling of Isopycnal diffusivity
C     GM_bol2dFile :: input file for 2.D horiz scaling of Thickness diffusivity
C     GM_bol1dFile :: input file for 1.D vert. scaling of Thickness diffusivity
C     GM_K3dRediFile :: input file for background 3.D Isopycal(Redi) diffusivity
C     GM_K3dGMFile   :: input file for background 3.D Thickness (GM) diffusivity

      CHARACTER*(40) GM_taper_scheme
      CHARACTER*(MAX_LEN_FNAM) GM_iso2dFile
      CHARACTER*(MAX_LEN_FNAM) GM_iso1dFile
      CHARACTER*(MAX_LEN_FNAM) GM_bol2dFile
      CHARACTER*(MAX_LEN_FNAM) GM_bol1dFile
      CHARACTER*(MAX_LEN_FNAM) GM_K3dRediFile
      CHARACTER*(MAX_LEN_FNAM) GM_K3dGMFile
      COMMON /GM_PARAMS_C/
     &                   GM_taper_scheme,
     &                   GM_iso2dFile, GM_iso1dFile,
     &                   GM_bol2dFile, GM_bol1dFile,
     &                   GM_K3dRediFile, GM_K3dGMFile

C--   COMMON /GM_PARAMS_R/ GM/Redi real-type parameters
C     GM_isopycK       :: Isopycnal diffusivity [m^2/s] (Redi-tensor)
C     GM_background_K  :: Thickness diffusivity [m^2/s] (GM bolus transport)
C     GM_maxSlope      :: maximum slope (tapering/clipping) [-]
C     GM_Kmin_horiz    :: minimum horizontal diffusivity [m^2/s]
C     GM_Small_Number  :: epsilon used in computing the slope
C     GM_slopeSqCutoff :: slope^2 cut-off value
C     GM_Scrit, GM_Sd  :: parameter for 'dm95' & 'ldd97' tapering fct
C     GM_isoFac_calcK  :: add fraction of  dynamically computed variable K,
C                         e.g. Visbeck, also to Redi tensor (default = 1.)
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
C-    Variable K parameters for Visbeck etal (1997) scheme:
C-    Variable K parameters for Marshall etal (2012) GEOM scheme:
C     GEOM_alpha       :: non-dim eddy efficiency param (=<1 in QG)
C     GEOM_lmbda       :: lin eddy energy dissipation rate
C     GEOM_ini_EKE     :: initial value for depth-int eddy kinetic energy
C     GEOM_diffKh_EKE  :: depth-int param eddy energy diffusion coeff
C     GEOM_minval_K    :: lower bound on diffusivity
C     GEOM_maxval_K    :: upper bound on diffusivity
C     GEOM_vert_struc_min   :: lower bound on N2/Nref vertical structure func
C     GEOM_vert_struc_max   :: upper bound on N2/Nref vertical structure func
C-    Variable K parameters for PV diffusion based, Bates etal (2014) scheme:
C     GM_Bates_gamma   :: mixing efficiency for 3D eddy diffusivity [-]
C     GM_Bates_b1      :: an empirically determined constant of O(1)
C     GM_Bates_EadyMinDepth :: upper depth for Eady calculation
C     GM_Bates_EadyMaxDepth :: lower depth for Eady calculation
C     GM_Bates_Lambda  ::
C     GM_Bates_smallK  ::
C     GM_Bates_maxK    :: Upper bound on the diffusivity
C     GM_Bates_constK  :: Constant diffusivity to use when GM_useBatesK3d=T and
C                         GM_Bates_use_constK=T and/or GM_Bates_constRedi=T
C     GM_Bates_maxC    ::
C     GM_Bates_Rmax    :: Length scale upper bound used for calculating urms
C     GM_Bates_Rmin    :: Length scale lower bound for calc. the eddy radius
C     GM_Bates_minCori :: minimum value for f (prevents Pb near the equator)
C     GM_Bates_minN2   :: minimum value for the square of the buoyancy frequency
C     GM_Bates_surfMinDepth :: minimum value for the depth of the surface layer
C     GM_Bates_vecFreq :: Frequency at which to update the baroclinic modes
C     GM_Bates_minRenorm :: minimum value for the renormalisation factor
C     GM_Bates_maxRenorm :: maximum value for the renormalisation factor
      _RL GM_isopycK
      _RL GM_background_K
      _RL GM_maxSlope
      _RL GM_Kmin_horiz
      _RL GM_Small_Number
      _RL GM_slopeSqCutoff
      _RL GM_Scrit, GM_Sd
      _RL GM_isoFac_calcK
      _RL GM_facTrL2dz
      _RL GM_facTrL2ML
      _RL GM_maxTransLay
      _RL GM_BVP_cMin
      _RL subMeso_Ceff
      _RL subMeso_invTau
      _RL subMeso_LfMin
      _RS subMeso_Lmax
      _RL GM_Visbeck_alpha
      _RL GM_Visbeck_length
      _RL GM_Visbeck_depth
      _RL GM_Visbeck_minDepth
      _RL GM_Visbeck_maxSlope
      _RL GM_Visbeck_minVal_K
      _RL GM_Visbeck_maxVal_K
      _RL GEOM_alpha
      _RL GEOM_lmbda
      _RL GEOM_ini_EKE
      _RL GEOM_diffKh_EKE
      _RL GEOM_minval_K
      _RL GEOM_maxval_K
      _RL GEOM_vert_struc_min
      _RL GEOM_vert_struc_max
      _RL GM_Bates_gamma
      _RL GM_Bates_b1
      _RL GM_Bates_EadyMinDepth
      _RL GM_Bates_EadyMaxDepth
      _RL GM_Bates_Lambda
      _RL GM_Bates_smallK
      _RL GM_Bates_maxK
      _RL GM_Bates_constK
      _RL GM_Bates_maxC
      _RL GM_Bates_Rmax
      _RL GM_Bates_Rmin
      _RL GM_Bates_minCori
      _RL GM_Bates_minN2
      _RL GM_Bates_surfMinDepth
      _RL GM_Bates_vecFreq
      _RL GM_Bates_minRenorm
      _RL GM_Bates_maxRenorm
      COMMON /GM_PARAMS_RL/
     &                 GM_isopycK, GM_background_K,
     &                 GM_maxSlope,
     &                 GM_Kmin_horiz,
     &                 GM_Small_Number, GM_slopeSqCutoff,
     &                 GM_Scrit, GM_Sd, GM_isoFac_calcK,
     &                 GM_facTrL2dz, GM_facTrL2ML, GM_maxTransLay,
     &                 GM_BVP_cMin,
     &                 subMeso_Ceff, subMeso_invTau, subMeso_LfMin,
     &                 GM_Visbeck_alpha, GM_Visbeck_length,
     &                 GM_Visbeck_depth,
     &                 GM_Visbeck_minDepth, GM_Visbeck_maxSlope,
     &                 GM_Visbeck_minVal_K, GM_Visbeck_maxVal_K,
     &                 GEOM_alpha, GEOM_lmbda,
     &                 GEOM_ini_EKE, GEOM_diffKh_EKE,
     &                 GEOM_minval_K, GEOM_maxval_K,
     &                 GEOM_vert_struc_min, GEOM_vert_struc_max,
     &                 GM_Bates_gamma, GM_Bates_b1,
     &                 GM_Bates_EadyMinDepth, GM_Bates_EadyMaxDepth,
     &                 GM_Bates_Lambda, GM_Bates_smallK, GM_Bates_maxK,
     &                 GM_Bates_constK, GM_Bates_maxC,
     &                 GM_Bates_Rmax, GM_Bates_Rmin,
     &                 GM_Bates_minCori, GM_Bates_minN2,
     &                 GM_Bates_surfMinDepth, GM_Bates_vecFreq,
     &                 GM_Bates_minRenorm, GM_Bates_maxRenorm

      COMMON /GM_PARAMS_RS/
     &                   subMeso_Lmax

C--   COMMON /GM_DERIVED_PAR/ other GM/Redi parameters
C     (derived from previous block and not directly user configured)
      _RL GM_rMaxSlope
      _RL GM_skewflx
      _RL GM_BVP_rModeNumber
      _RL GM_BVP_cHat2Min
      COMMON /GM_DERIVED_PAR/
     &                   GM_rMaxSlope,
     &                   GM_skewflx,
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

#ifdef GM_READ_K3D_REDI
C--   COMMON /GM_INP_K3D_REDI/ 3.D background isopycnal (Redi) diffusiv. [m^2/s]
      COMMON /GM_INP_K3D_REDI/ GM_inpK3dRedi
      _RL GM_inpK3dRedi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif
#ifdef GM_READ_K3D_GM
C--   COMMON /GM_INP_K3D_GM/   3.D background thickness (GM) diffusivity [m^2/s]
      COMMON /GM_INP_K3D_GM/   GM_inpK3dGM
      _RL GM_inpK3dGM  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

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
C     GM mixing/stirring coefficient (spatially variable in horizontal)
C     for Visbeck et al. parameterization
      _RL VisbeckK(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /GM_Visbeck/ VisbeckK
#endif

#ifdef GM_BATES_K3D
C     GM_BatesK3d :: The 3-d eddy mixing coefficient from Bates etal [m**2/s]
C     modesC      :: First baroclinic mode at the centre of a tracer cell [-]
C     modesW      :: First N baroclinic mode at the western face of a cell [-]
C     modesS      :: First N baroclinic mode at the southern face of a cell [-]
C     Rdef        :: Deformation radius [m]
C     gradf       :: gradient of Coriolis paramater at a cell centre, 1/(m*s)

      _RL GM_BatesK3d(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL modesC   (1,1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL modesW(GM_Bates_NModes,
     &                1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL modesS(GM_Bates_NModes,
     &                1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL Rdef       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gradf      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      COMMON /GM_BATES_K/
     &                 GM_BatesK3d,
     &                 modesC, modesW, modesS,
     &                 Rdef, gradf
#endif

#ifdef GM_GEOM_VARIABLE_K
C     GEOM_K3d      :: mixing/stirring coefficient (spatially variable in
C                      horizontal for Marshall et al. (2012) parameterization);
C                      defined at cell interfaces (W-points)
C     GEOM_EKE      :: parameterised total eddy energy in GEOMETRIC;
C                      used to update GEOM_K3d; 2D field
C     GEOM_gEKE_Nm1 :: 2D eddy energy tendency for AB time-stepping
C     GEOM_startAB  :: controls time-stepping routine of parameterised
C                      eddy energy in gmredi_calc_geom.F (=0/1)
C     GEOM_taper    :: reduce GEOM_K3d based on some conditions (currently
C                      depth); 2D field

      _RL GEOM_K3d     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL GEOM_EKE     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL GEOM_gEKE_Nm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL GEOM_taper   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER GEOM_startAB

      COMMON /GM_GEOM/ GEOM_K3d,
     &                 GEOM_EKE,
     &                 GEOM_gEKE_Nm1,
     &                 GEOM_taper
      COMMON /GM_GEOM_I/ GEOM_startAB
#endif /* GM_GEOM_VARIABLE_K */

#ifdef ALLOW_GM_LEITH_QG
C     GM_LeithQG_K :: Horizontal LeithQG viscosity, to add to GM coefficient
      _RL GM_LeithQG_K(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /GM_LEITH_QG/ GM_LeithQG_K
#endif /* ALLOW_GM_LEITH_QG */

#endif /* ALLOW_GMREDI */
