#ifdef ALLOW_LAYERS

C--   Header for LAYERS package. By Ryan Abernathey.
C--   For computing volume fluxes in isopyncal layers

C --  Parameters:
C     layers_num      ::
C     layers_krho     ::
C     layers_name     ::
C     layers_bolus    ::
C     layers_MNC      ::
C     layers_MDSIO    ::
C     layers_diagFreq ::

      INTEGER layers_num(layers_maxNum), layers_krho(layers_maxNum)
      COMMON /LAYERS_PARM_I/ layers_num, layers_krho

      CHARACTER*(3) layers_name(layers_maxNum)
      COMMON /LAYERS_PARM_C/ layers_name

      LOGICAL layers_MNC, layers_MDSIO
      LOGICAL layers_bolus(layers_maxNum)
      COMMON /LAYERS_PARM_L/ layers_MNC, layers_MDSIO,
     &                       layers_bolus

      _RL layers_diagFreq
      COMMON /LAYERS_PARM_RL/ layers_diagFreq

C --  Isopycnal grid parameters:
C      layers_bounds :: boundaries of tracer layers
C      dZZf     :: height of fine grid cells
C      NZZ      :: the number of levels to use in the fine grid
C      MapIndex :: indices for mapping ZZ to Z
C      MapFact  :: factors for interpolating T(Z) to T(ZZ)

      _RL layers_bounds(Nlayers+1,layers_maxNum)
      _RL dZZf(FineGridMax)
      INTEGER MapIndex(FineGridMax), CellIndex(FineGridMax)
      _RL MapFact(FineGridMax)
      INTEGER NZZ
      COMMON /LAYERS_VERT_GRID_I/
     &      NZZ, MapIndex, CellIndex
      COMMON /LAYERS_VERT_GRID_R/
     &      MapFact, dZZf, layers_bounds

C -- Thermodynamics fields
C    Right-hand-side tendency terms times thickness
C
C      layers_TtendSurf  :: Layer thickness tendency due to THETA surf. forc. (m/s)
C      layers_TtendDiffh :: Layer thickness tendency due to THETA horiz. diff. (m/s)
C      layers_TtendDiffr :: Layer thickness tendency due to THETA vert. diff. (m/s)
C      layers_TtendAdvh  :: Layer thickness tendency due to THETA horiz. adv. (m/s)
C      layers_TtendAdvr  :: Layer thickness tendency due to THETA vert. adv. (m/s)
C      layers_StendSurf  :: Layer thickness tendency due to SALT surf. forc. (m/s)
C      layers_StendDiffh :: Layer thickness tendency due to SALT horiz. diff. (m/s)
C      layers_StendDiffr :: Layer thickness tendency due to SALT vert. diff. (m/s)
C      layers_StendAdvh  :: Layer thickness tendency due to SALT horiz. adv. (m/s)
C      layers_StendAdvr  :: Layer thickness tendency due to SALT vert. adv. (m/s)
C  -- The following are temporary arrays that need to be stored.
C  -- They are in regular vertical coordinates.
C  -- The fourth index is tracer id: 1 for T and 2 for S
C      layers_surfflux   :: surface temperature flux (same as diagnostics TFLUX and SFLUX)
C      layers_dfx        :: zonal diffusive flux of T / S
C      layers_dfy        :: meridional diffusive flux of T / S
C      layers_dfr        :: vertical diffusive flux of T / S
C      layers_afx        :: zonal advective flux of T / S
C      layers_afy        :: meridional advective flux of T / S
C      layers_afr        :: vertical advective flux of T / S
C  -- to save memory, the same arrays are converted in place to divergences
C
C  -- We also need the thermal / saline expansion coefficients for diapycnal fluxes
C      layers_alpha      :: alpha factor for density eqn (-drhodT/rho)
C      layers_beta       :: alpha factor for density eqn (-drhodS/rho)

# ifdef LAYERS_THERMODYNAMICS
      COMMON /LAYERS_VAR_THERMODYNAMICS/
     &    layers_bounds_w, layers_recip_delta,
     &    layers_TtendSurf, layers_TtendDiffh, layers_TtendDiffr,
     &    layers_TtendAdvh, layers_TtendAdvr, layers_Ttendtot,
     &    layers_StendSurf, layers_StendDiffh, layers_StendDiffr,
     &    layers_StendAdvh, layers_StendAdvr, layers_Stendtot,
     &    layers_Hc, layers_PIc,
     &    layers_Hcw,
     &    layers_surfflux, layers_dfx, layers_dfy, layers_dfr,
     &    layers_afx, layers_afy, layers_afr, layers_tottend
      _RL layers_bounds_w(Nlayers, layers_maxNum)
      _RL layers_recip_delta(Nlayers-1, layers_maxNum)
      _RL layers_TtendSurf (1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                                         Nlayers-1,nSx,nSy)
      _RL layers_Ttendtot(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                                         Nlayers-1,nSx,nSy)
      _RL layers_TtendDiffh(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                                         Nlayers-1,nSx,nSy)
      _RL layers_TtendDiffr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                                         Nlayers-1,nSx,nSy)
      _RL layers_TtendAdvh(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                                         Nlayers-1,nSx,nSy)
      _RL layers_TtendAdvr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                                         Nlayers-1,nSx,nSy)
      _RL layers_StendSurf (1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                                         Nlayers-1,nSx,nSy)
      _RL layers_Stendtot(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                                         Nlayers-1,nSx,nSy)
      _RL layers_StendDiffh(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                                         Nlayers-1,nSx,nSy)
      _RL layers_StendDiffr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                                         Nlayers-1,nSx,nSy)
      _RL layers_StendAdvh(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                                         Nlayers-1,nSx,nSy)
      _RL layers_StendAdvr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                                         Nlayers-1,nSx,nSy)
      _RL layers_Hcw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                                         Nlayers-1,nSx,nSy)
      _RL layers_Hc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                                         Nlayers,nSx,nSy)
      _RL layers_PIc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,
     &                                         Nlayers,nSx,nSy)
      _RL layers_surfflux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,2,nSx,nSy)
      _RL layers_dfx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,2,nSx,nSy)
      _RL layers_dfy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,2,nSx,nSy)
      _RL layers_dfr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,2,nSx,nSy)
      _RL layers_afx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,2,nSx,nSy)
      _RL layers_afy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,2,nSx,nSy)
      _RL layers_afr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,2,nSx,nSy)
      _RL layers_tottend(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,2,nSx,nSy)

#ifdef SHORTWAVE_HEATING
      COMMON /LAYERS_SW/ layers_sw
      _RL layers_sw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,1,nSx,nSy)
#endif /* LAYERS_SHORTWAVE */

#ifdef LAYERS_PRHO_REF
      COMMON /LAYERS_VAR_THERMODYNAMICS_PRHO/
     & layers_alpha, layers_beta
      _RL layers_alpha(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL layers_beta(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

# endif /* LAYERS_THERMODYAMICS */

#endif /* ALLOW_LAYERS */
