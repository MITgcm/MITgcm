C $Header: /u/gcmpack/MITgcm/pkg/thsice/THSICE_VARS.h,v 1.3 2005/06/24 04:36:54 edhill Exp $
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
C   Qice1     :: enthalphy of ice layer 1  [J/m3]
C   Qice2     :: enthalphy of ice layer 2  [J/m3]
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
C   snowPrc  :: snow precipitation                        [kg/m2/s]
C   siceAlb  :: area weighted sea-ice albedo           [0-1]
C   dFdT     :: heat deriveative for coupled model
C   oceQnet  :: net heat flux  to the ocean         (+=down) [W/m2]
C   oceQsw   :: net short-wave that enter the ocean (+=down) [W/m2]
C   oceFWfx  :: net fresh water flux to the ocean   (+=down) [kg/m2]
C   oceSflx  :: net salt flux to the ocean      (+=down) [psu.kg/m2]
      COMMON / THSICE_FLUX /
c    &       oceQsw, oceQnet, oceFWfx, oceSflx,
     &       sHeating, flxCndBt,
     &       snowPrc, siceAlb
#ifdef COUPLE_MODEL
     &     , dFdT 
#endif

c     _RL oceQnet(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL oceQsw (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL oceFWfx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
c     _RL oceSflx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL sHeating(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL flxCndBt(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL snowPrc (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL siceAlb (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#ifdef COUPLE_MODEL
      _RL dFdT    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

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
