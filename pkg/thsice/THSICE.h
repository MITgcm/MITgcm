C $Header: /u/gcmpack/MITgcm/pkg/thsice/Attic/THSICE.h,v 1.1 2003/11/23 01:20:13 jmc Exp $
C $Name:  $

#ifdef ALLOW_THSICE

C     !ROUTINE: THSICE.h
C -------------------------------
C   THSICE.h
C  variable for thermodynamics - Sea-Ice model
C -------------------------------

C-- COMMON /THSICE_VARS/
C  iceMask  - no ice=0, grid all ice 1
C  iceHeight - depth of ice layer
C  snowHeight- depth of snow layer (in equivalent liquid water)
C  snow      - rate of snow accumulation
C  Tsrf      - temperature at surface
C  Tice1     - temperature of ice layer 1
C  Tice2     - temperature of ice layer 2
C  Qice1     - enthalphy of ice layer 1
C  Qice2     - enthalphy of ice layer 2
C  sage      - snow age
Cswdcou -- dFdT - heat deriveative for coupled model ---

      COMMON /THSICE_VARS/
     &       iceMask, iceHeight, snowHeight,
     &       Tsrf, Tice1, Tice2, Snow,
     &       Qice1, Qice2,sage
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
#ifdef COUPLE_MODEL
      _RL dFdT(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

#endif /* ALLOW_THSICE */
