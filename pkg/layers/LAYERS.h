C $Header: /u/gcmpack/MITgcm/pkg/layers/LAYERS.h,v 1.14 2014/06/04 14:48:32 rpa Exp $
C $Name:  $

#ifdef ALLOW_LAYERS

C--   Header for LAYERS package. By Ryan Abernathey.
C--   For computing volume fluxes in isopyncal layers

C --  Parms
      INTEGER LAYER_nb, layers_kref
      INTEGER layers_num(layers_maxNum), layers_krho(layers_maxNum)
      COMMON /LAYERS_PARM_I/ layers_num,layers_krho,
     &                       LAYER_nb, layers_kref

      CHARACTER*(3) layers_name(layers_maxNum)
      COMMON /LAYERS_PARM_C/ layers_name

      _RL layers_taveFreq, layers_diagFreq
      COMMON /LAYERS_PARM_RL/ layers_taveFreq, layers_diagFreq

      LOGICAL layers_MNC, layers_MDSIO, useBOLUS
      LOGICAL layers_bolus(layers_maxNum)
      COMMON /LAYERS_PARM_L/ layers_MNC, layers_MDSIO,
     & useBOLUS, layers_bolus

C --  Isopycnal grid parameters:
C      layers_bounds :: boundaries of tracer layers
C      layers_G :: boundaries of tracer layers (retired)
C      dZZf     :: height of fine grid cells
C      NZZ      :: the number of levels to use in the fine grid
C      MapIndex :: indices for mapping ZZ to Z
C      MapFact  :: factors for interpolating T(Z) to T(ZZ)

      _RL layers_G(nLayers+1)
      _RL layers_bounds(nLayers+1,layers_maxNum)
      _RL dZZf(FineGridMax)
      INTEGER MapIndex(FineGridMax), CellIndex(FineGridMax)
      _RL MapFact(FineGridMax)
      INTEGER NZZ
      COMMON /LAYERS_VERT_GRID_I/
     &      NZZ, MapIndex, CellIndex
      COMMON /LAYERS_VERT_GRID_R/
     &      layers_G, MapFact, dZZf, layers_bounds

C --  3D Layers fields. The vertical dimension in these fields is nLayers,
C     i.e. the isopycnal coordinate.
C
C      layers_UH :: U integrated over layer (m^2/s)
C      layers_VH :: V integrated over layer (m^2/s)
C      layers_Hw    :: Layer thickness at the U point (m)
C      layers_Hs    :: Layer thickness at the V point (m)
C      layers_PIw   :: 1 if layer exists, 0 otherwise
C      layers_PIs   :: 1 if layer exists, 0 otherwise
C      layers_U     :: mean zonal velocity in layer (only if layer exists) (m/s)
C      layers_V     :: mean meridional velocity in layer (only if layer exists) (m/s)

#ifdef LAYERS_UFLUX
      COMMON /LAYERS_VAR_UFLUX/ layers_UH
      _RL layers_UH (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
# ifdef LAYERS_THICKNESS
      COMMON /LAYERS_VAR_UTHICKNESS/
     &    layers_Hw, layers_PIw, layers_U
      _RL layers_Hw (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL layers_PIw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL layers_U  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
# endif /* LAYERS_THICKNESS */
#endif /* LAYERS_UFLUX */

#ifdef LAYERS_VFLUX
      COMMON /LAYERS_VAR_VFLUX/ layers_VH
      _RL layers_VH (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
     &
# ifdef LAYERS_THICKNESS
      COMMON /LAYERS_VAR_VTHICKNESS/
     &    layers_Hs, layers_PIs, layers_V
      _RL layers_Hs (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL layers_PIs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
      _RL layers_V  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
# endif /* LAYERS_THICKNESS */
#endif /* LAYERS_VFLUX */

#ifdef LAYERS_PRHO_REF
      COMMON /LAYERS_PRHO/ prho
      _RL prho(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

C -- Thermodynamics fields
C    Right-hand-side tendency terms times thickness
C
C      layers_TtendSurf  :: Temperature tendency from surface forcing (m deg/s)
C      layers_TtendDiffh :: Temperature tendency from horizontal mixing (m deg/s)
C      layers_TtendDiffr :: Temperature tendency from vertical mixing (m deg/s)
C      layers_StendSurf  :: Salinity tendency from surface forcing (m PSU/s)
C      layers_StendDiffh :: Salinity tendency from horizontal mixing (m PSU/s)
C      layers_StendDiffr :: Salinity tendency from vertical mixing (m PSU/s)
C  -- The following are temporary arrays that need to be stored.
C  -- They are in regular vertical coordinates.
C  -- The fourth index is tracer id: 1 for T and 2 for S
C      layers_surfflux   :: surface temperature flux (same as diagnostics TFLUX and SFLUX)
C      layers_dfx        :: zonal diffusive flux of T / S
C      layers_dfy        :: meridional diffusive flux of T / S
C      layers_dfr        :: vertical diffusive flux of T / S
C  -- to save memory, the same arrays are converted in place to divergences

# ifdef LAYERS_THERMODYNAMICS
      COMMON /LAYERS_VAR_THERMODYNAMICS/
     &    layers_TtendSurf, layers_TtendDiffh, layers_TtendDiffr,
     &    layers_StendSurf, layers_StendDiffh, layers_StendDiffr,
     &    layers_Hc, layers_PIc,
     &    layers_surfflux, layers_dfx, layers_dfy, layers_dfr
      _RL layers_TtendSurf (1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                                         Nlayers,nSx,nSy)
      _RL layers_TtendDiffh(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                                         Nlayers,nSx,nSy)
      _RL layers_TtendDiffr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                                         Nlayers,nSx,nSy)
      _RL layers_StendSurf (1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                                         Nlayers,nSx,nSy)
      _RL layers_StendDiffh(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                                         Nlayers,nSx,nSy)
      _RL layers_StendDiffr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                                         Nlayers,nSx,nSy)      
      _RL layers_Hc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                                         Nlayers,nSx,nSy)      
      _RL layers_PIc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                                         Nlayers,nSx,nSy)      
      _RL layers_surfflux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,2,nSx,nSy)
      _RL layers_dfx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,2,nSx,nSy)
      _RL layers_dfy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,2,nSx,nSy)
      _RL layers_dfr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,2,nSx,nSy)
# endif /* LAYERS_THERMODYAMICS */

#ifdef ALLOW_TIMEAVE
C-- The same variables, time-averaged

C     Keep track of time
      _RL layers_TimeAve(nSx,nSy)
      COMMON /LAYERS_TAVE/ layers_TimeAve

#ifdef LAYERS_UFLUX
      _RL layers_UH_T(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,
     &                   nSx,nSy)
#ifdef LAYERS_THICKNESS
      _RL layers_Hw_T(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,
     &                   nSx,nSy)
      COMMON /LAYERS_U_TAVE/ layers_UH_T,
     &    layers_Hw_T
#else  /* LAYERS_THICKNESS */
      COMMON /LAYERS_U_TAVE/ layers_UH_T
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_UFLUX */

#ifdef LAYERS_VFLUX
      _RL layers_VH_T(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,
     &                   nSx,nSy)
#ifdef LAYERS_THICKNESS
      _RL layers_Hs_T(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,
     &                   nSx,nSy)
      COMMON /LAYERS_V_TAVE/ layers_VH_T,
     &    layers_Hs_T
#else  /* LAYERS_THICKNESS */
      COMMON /LAYERS_V_TAVE/ layers_VH_T
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_VFLUX */

#ifdef LAYERS_PRHO_REF
      _RL prho_tave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /LAYERS_RPHO_TAVE/ prho_tave
#endif

#endif /* ALLOW_TIMEAVE */

#endif /* ALLOW_LAYERS */
