C $Header: /u/gcmpack/MITgcm/pkg/thsice/THSICE_VARS.h,v 1.13 2013/05/02 20:03:12 jmc Exp $
C $Name:  $

#ifdef ALLOW_THSICE

C     !ROUTINE: THSICE_VARS.h
C -------------------------------
C   THSICE_VARS.h
C  variable for thermodynamics - Sea-Ice model
C -------------------------------

C-- COMMON /THSICE_VARS/  state variables of sea-ice model :
C   iceMask   :: sea-ice fraction: no ice=0, grid all ice 1  []
C   iceHeight :: depth of ice layer        [m]
C   snowHeight:: depth of snow layer       [m]
C   Tsrf      :: temperature at surface     [oC]
C   Tice1     :: temperature of ice layer 1 [oC]
C   Tice2     :: temperature of ice layer 2 [oC]
C   Qice1     :: enthalpy of ice layer 1  [J/kg]
C   Qice2     :: enthalpy of ice layer 2  [J/kg]
C   snowAge   :: snow age                   [s]
      COMMON /THSICE_VARS/
     &       iceMask, iceHeight, snowHeight,
     &       Tsrf, Tice1, Tice2,
     &       Qice1, Qice2, snowAge

      _RL iceMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL iceHeight (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL snowHeight(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Tsrf   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Tice1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Tice2  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Qice1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Qice2  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL snowAge(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C-- COMMON /THSICE_FLUX/ hold fluxes needed for thsice model
C   sHeating :: surf heating left to melt snow or ice (= Atmos-Conduction)
C   flxCndBt :: heat flux conducted through the ice to bottom surface
C   siceAlb  :: area weighted sea-ice albedo           [0-1]
C   icAlbNIR :: seaice(+snow) surface Near Infra-Red Albedo
C atmospheric fluxes (change along the time-stepping):
C   icFlxSW  :: short-wave heat flux (+=down) over sea-ice
C               (downward SW / net SW @ surface / net SW below sea-ice)
C   icFlxAtm :: Atmospheric surf. heat flux over sea-ice [W/m2] (+=down)
C               (over sea-ice only / weighted by ice-fraction)
C   icFrwAtm :: fresh-water flux (E-P) from the atmosphere [kg/m2/s] (+=up)
C               ( ice Evap only / ice E-P / ice - ocean weighted E-P )
C   oceQnet  :: net heat flux  to the ocean         (+=down) [W/m2]
C   oceQsw   :: net short-wave that enter the ocean (+=down) [W/m2]
C   oceFWfx  :: net fresh water flux to the ocean   (+=down) [kg/m2]
C   oceSflx  :: net salt flux to the ocean      (+=down) [psu.kg/m2]
C   adjustFrW :: global adjustment to surface fresh-water flux [kg/m2/s]
      COMMON / THSICE_FLUX /
c    &       oceQsw, oceQnet, oceFWfx, oceSflx,
     &       sHeating, flxCndBt,
     &       siceAlb, icAlbNIR,
     &       icFlxSW, icFlxAtm, icFrwAtm,
     &       adjustFrW

c     _RL oceQnet(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL oceQsw (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL oceFWfx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL oceSflx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL sHeating(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL flxCndBt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL siceAlb (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL icAlbNIR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL icFlxSW (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL icFlxAtm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL icFrwAtm(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL adjustFrW

C-- COMMON /THSICE_DYN_R/  variables used with sea-ice advection/diffusion
C   oceFWfx   :: fresh water flux to the ocean  [kg/m^2/s]
C   oceSflx   :: salt flux to the ocean         [psu.kg/m^2/s] (~g/m^2/s)
C   oceQnet   :: heat flux to the ocean         [W/m^2]
C---
C    Note :: when ice volume is too small to be kept, ice & snow is melt
C        and fresh water, salt and heat are returned to the ocean.
C        For now, those fluxes are stored separately and will try to find
C        out how to incorporate them more naturally in the usual forcing.
C---
      COMMON /THSICE_DYN_R/
     &       oceFWfx, oceSflx, oceQnet

      _RL oceFWfx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL oceSflx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL oceQnet(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

C-- COMMON / THSICE_OCEMXLAYER / oceanic mixed layer state
C   hOceMxL :: thickness   of the ocean mixed layer [m]
C   tOceMxL :: temperature in the ocean mixed layer [oC]
C   sOceMxL :: salinity    in the ocean mixed layer [psu]
C   v2ocMxL :: velocity (square) in the mixed layer [m2/s2]
      COMMON / THSICE_OCEMXLAYER /
     &       hOceMxL, tOceMxL, sOceMxL, v2ocMxL

      _RL hOceMxL(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tOceMxL(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL sOceMxL(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL v2ocMxL(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#endif /* ALLOW_THSICE */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
