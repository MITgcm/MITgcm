#ifdef ALLOW_THERM_SEAICE
cswdice
c     !ROUTINE: ICE.h
c -------------------------------
c   ICE.h
C  variable for ice model
c -------------------------------
c   OUTPUT VARAIBLES
c  icemask  - no ice=0, grid all ice 1
c  iceheight - depth of ice layer
C  snowheight- depth of snow layer (in equivalent liquid water)
c  snow      - rate of snow accumulation
c  Tsrf      - temperature at surface
c  Tice1     - temperature of ice layer 1
c  Tice2     - temperature of ice layer 2
c  Qice1     - enthalphy of ice layer 1
c  Qice2     - enthalphy of ice layer 2
c  hicemin     - minimum ice height
c  sage      - snow age
cswdcou -- dFdT - heat deriveative for coupled model ---

      COMMON /ICE_PARMS/
     &       iceMask, iceHeight, snowHeight,
     &       Tsrf, Tice1, Tice2, Snow,
     &       Qice1, Qice2,sage,hicemin
#ifdef COUPLE_MODEL
     &      ,dFdT
#endif


      _RL iceMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL iceHeight(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL snowHeight(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Tsrf(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Tice1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Tice2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Qice1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Qice2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL Snow(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL sage(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL hicemin
#ifdef COUPLE_MODEL
      _RL dFdT(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

c      parameter (hicemin=0.d0)

c
c  startIceModel - start ice model at nIter0=1,
c                  use pickup files =0
c  relaxlat      - surface forcing only in lower latitudes

      COMMON /ICE_FFIELDS_PARMS/
     &       relaxlat,startIceModel

      INTEGER startIceModel
      _RL  relaxlat
#endif
