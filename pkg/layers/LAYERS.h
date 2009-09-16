C $Header: /u/gcmpack/MITgcm/pkg/layers/LAYERS.h,v 1.1 2009/09/16 21:25:47 rpa Exp $
C $Name:  $

#ifdef ALLOW_LAYERS

C--   Header for LAYERS package. By Ryan Abernathey.
C--   For computing volume fluxes in isopyncal layers

C --  Parms
      LOGICAL layers_MNC, layers_MDSIO
      COMMON /LAYERS_PARMS/ layers_MNC, layers_MDSIO

C     3D Layers fields. The vertical dimension in these fields is nLayers,
C     i.e. the isopycnal coordinate.
C
C      layers_UFlux :: U integrated over layer (m^2/s)
C      layers_VFlux :: V integrated over layer (m^2/s)
C      layers_HU    :: Layer thickness at the U point (m)
C      layers_HV    :: Layer thickness at the V point (m)

#ifdef LAYERS_UFLUX
      _RL layers_UFlux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
#ifdef LAYERS_THICKNESS
      _RL layers_HU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
#endif /* LAYERS_THICKNESS */
      COMMON /LAYERS_U/ layers_UFlux
#ifdef LAYERS_THICKNESS
     &  , layers_HU
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_UFLUX */

#ifdef LAYERS_VFLUX
      _RL layers_VFlux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
#ifdef LAYERS_THICKNESS
      _RL layers_HV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
#endif /* LAYERS_THICKNESS */
      COMMON /LAYERS_V/ layers_VFlux
#ifdef LAYERS_THICKNESS
     &  , layers_HV
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_VFLUX */

C-- The same variables, time-averaged

#ifdef ALLOW_TIMEAVE

C     Keep track of time
      _RL layers_TimeAve(Nlayers,nSx,nSy)
      COMMON /LAYERS_TAVE/ layers_TimeAve

#ifdef LAYERS_UFLUX
      _RL layers_UFlux_T(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
#ifdef LAYERS_THICKNESS
      _RL layers_HU_T(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
#endif /* LAYERS_THICKNESS */
      COMMON /LAYERS_U_TAVE/ layers_UFlux_T
#ifdef LAYERS_THICKNESS
     &  , layers_HU_T
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_UFLUX */

#ifdef LAYERS_VFLUX
      _RL layers_VFlux_T(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
#ifdef LAYERS_THICKNESS
      _RL layers_HV_T(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nlayers,nSx,nSy)
#endif /* LAYERS_THICKNESS */
      COMMON /LAYERS_V_TAVE/ layers_VFlux_T
#ifdef LAYERS_THICKNESS
     &  , layers_HV_T
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_VFLUX */

#endif /* ALLOW_TIMEAVE */


C     Isopycnal grid parameters:
C      layers_G :: boundaries of isopycnal layers
C      dZZ      :: the vertical grid spacing
C      NZZ      :: the number of levels to use in the fine grid
C      MapIndex :: indices for mapping ZZ to Z
C      MapFact  :: factors for interpolating T(Z) to T(ZZ)

      _RL layers_G(nLayers+1)
      _RL dZZ
      INTEGER MapIndex(FineGridMax), CellIndex(FineGridMax)
      _RL MapFact(FineGridMax)
      INTEGER NZZ
      COMMON /LAYERS_VERT_GRID_I/
     &      NZZ, MapIndex, CellIndex
      COMMON /LAYERS_VERT_GRID_R/
     &      layers_G, dZZ, MapFact


#endif /* ALLOW_LAYERS */
