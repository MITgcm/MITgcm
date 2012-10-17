C $Header: /u/gcmpack/MITgcm/pkg/layers/LAYERS.h,v 1.10 2012/10/17 17:45:35 rpa Exp $
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


C     3D Layers fields. The vertical dimension in these fields is nLayers,
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
      _RL layers_UH(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,
     &                   nSx,nSy,layers_maxNum)
# ifdef LAYERS_THICKNESS
      _RL layers_Hw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,
     &                   nSx,nSy,layers_maxNum)
      _RL layers_PIw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,
     &                   nSx,nSy,layers_maxNum)
      _RL layers_U(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,
     &                   nSx,nSy,layers_maxNum)
# else
      _RL layers_Hw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,
     &                   nSx,nSy,layers_maxNum)
      _RL layers_PIw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,
     &                   nSx,nSy,layers_maxNum)
      _RL layers_U(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,
     &                   nSx,nSy,layers_maxNum)
# endif /* LAYERS_THICKNESS */
#else
      _RL layers_UH(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,
     &                   nSx,nSy,layers_maxNum)
      _RL layers_Hw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,
     &                   nSx,nSy,layers_maxNum)
      _RL layers_PIw(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,
     &                   nSx,nSy,layers_maxNum)
      _RL layers_U(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,
     &                   nSx,nSy,layers_maxNum)
#endif

      COMMON /LAYERS_U/ layers_UH, layers_Hw,
     &    layers_PIw, layers_U
     
#ifdef LAYERS_VFLUX
      _RL layers_VH(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,
     &                   nSx,nSy,layers_maxNum)
# ifdef LAYERS_THICKNESS
      _RL layers_Hs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,
     &                   nSx,nSy,layers_maxNum)
      _RL layers_PIs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,
     &                   nSx,nSy,layers_maxNum)
      _RL layers_V(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,
     &                   nSx,nSy,layers_maxNum)
# else
      _RL layers_Hs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,
     &                   nSx,nSy,layers_maxNum)
      _RL layers_PIs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,
     &                   nSx,nSy,layers_maxNum)
      _RL layers_V(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,
     &                   nSx,nSy,layers_maxNum)     
# endif /* LAYERS_THICKNESS */
#else
      _RL layers_VH(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,
     &                   nSx,nSy,layers_maxNum)
      _RL layers_Hs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,
     &                   nSx,nSy,layers_maxNum)
      _RL layers_PIs(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,
     &                   nSx,nSy,layers_maxNum)
      _RL layers_V(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,
     &                   nSx,nSy,layers_maxNum)
#endif /* LAYERS_VFLUX */

      COMMON /LAYERS_V/ layers_VH, layers_Hs,
     &    layers_PIs, layers_V



#ifdef LAYERS_PRHO_REF
      _RL prho(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,
     &                   nSx,nSy,layers_maxNum)
#else
      _RL prho(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1,
     &                   nSx,nSy,layers_maxNum)
#endif

      COMMON /LAYERS_PRHO/ prho

#ifdef ALLOW_LAYERS_OUTPUT
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
      _RL layers_PIw_T(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,
     &                   nSx,nSy)
      _RL layers_U_T(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,
     &                   nSx,nSy)
#endif /* LAYERS_THICKNESS */
      COMMON /LAYERS_U_TAVE/ layers_UH_T
#ifdef LAYERS_THICKNESS
     &  , layers_Hw_T, layers_PIw_T, layers_U_T
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_UFLUX */

#ifdef LAYERS_VFLUX
      _RL layers_VH_T(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,
     &                   nSx,nSy)
#ifdef LAYERS_THICKNESS
      _RL layers_Hs_T(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,
     &                   nSx,nSy)
      _RL layers_PIs_T(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,
     &                   nSx,nSy)
      _RL layers_V_T(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,
     &                   nSx,nSy)
#endif /* LAYERS_THICKNESS */
      COMMON /LAYERS_V_TAVE/ layers_VH_T
#ifdef LAYERS_THICKNESS
     &  , layers_Hs_T, layers_PIs_T, layers_V_T
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_VFLUX */

#ifdef LAYERS_PRHO_REF
      _RL prho_tave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      COMMON /LAYERS_RPHO_TAVE/ prho_tave
#endif

#endif /* ALLOW_TIMEAVE */
#endif /* ALLOW_LAYERS_OUTPUT */

C     Isopycnal grid parameters:
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


#endif /* ALLOW_LAYERS */
