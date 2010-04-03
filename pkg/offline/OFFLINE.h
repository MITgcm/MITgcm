C $Header: /u/gcmpack/MITgcm/pkg/offline/OFFLINE.h,v 1.9 2010/04/03 22:34:26 jmc Exp $
C $Name:  $

#ifdef  ALLOW_OFFLINE
c     !ROUTINE: OFFLINE.h
c -------------------------------
c   OFFLINE.h
C  variable for forcing offline tracer
c -------------------------------

c   Forcing files
      COMMON /OFFLINE_COMMON_R/
     &       ConvectCount, ICEM,
     &       deltaToffline,
     &       offlineForcingPeriod, offlineForcingCycle,
     &       offlineLoadPrec
      _RL ICEM(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS ConvectCount(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
      _RL deltaToffline
      _RL offlineForcingPeriod
      _RL offlineForcingCycle
      INTEGER offlineLoadPrec

      COMMON /OFFLINE_COMMON_C/
     &       UvelFile, VvelFile, WvelFile, ThetFile, Saltfile,
     &       ConvFile, GMwxFile, GMwyFile, GMwzFile,
     &       HfluxFile, SfluxFile, ICEFile,
     &       KPP_DiffSFile, KPP_ghatKFile
      CHARACTER*(MAX_LEN_FNAM) UvelFile
      CHARACTER*(MAX_LEN_FNAM) VvelFile
      CHARACTER*(MAX_LEN_FNAM) WvelFile
      CHARACTER*(MAX_LEN_FNAM) ConvFile
      CHARACTER*(MAX_LEN_FNAM) ThetFile
      CHARACTER*(MAX_LEN_FNAM) SaltFile
      CHARACTER*(MAX_LEN_FNAM) GMwxFile
      CHARACTER*(MAX_LEN_FNAM) GMwyFile
      CHARACTER*(MAX_LEN_FNAM) GMwzFile
      CHARACTER*(MAX_LEN_FNAM) HFluxFile
      CHARACTER*(MAX_LEN_FNAM) SFluxFile
      CHARACTER*(MAX_LEN_FNAM) ICEFile
      CHARACTER*(MAX_LEN_FNAM) KPP_DiffSFile
      CHARACTER*(MAX_LEN_FNAM) KPP_ghatKFile

      COMMON /OFFLINE_COMMON_I/
     &       offlineIter0, offlineOffsetIter
      INTEGER offlineIter0
      INTEGER offlineOffsetIter

C     uvel[01]  :: Temp. for u
C     vvel[01]  :: Temp. for v
C     wvel[01]  :: Temp. for w
C     conv[01]  :: Temp for Convection Count
C     [01]      :: End points for interpolation
C     Above use static heap storage to allow exchange.
C     aWght, bWght :: Interpolation weights
      COMMON /OFFLINE_FFIELDS_R/
     &                 uvel0, vvel0, wvel0, tave0, save0,
     &                 conv0, gmkx0, gmky0, gmkz0, hflx0,
     &                 sflx0, kdfs0, kght0, icem0,
     &                 uvel1, vvel1, wvel1, tave1, save1,
     &                 conv1, gmkx1, gmky1, gmkz1, hflx1,
     &                 sflx1, kdfs1, kght1, icem1
      _RS  uvel0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  uvel1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  vvel0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  vvel1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  wvel0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  wvel1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  tave0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  tave1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  save0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  save1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  conv0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  conv1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  gmkx0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  gmkx1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  gmky0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  gmky1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  gmkz0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  gmkz1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  hflx0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  hflx1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  sflx0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  sflx1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  icem0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  icem1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  kdfs0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  kdfs1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  kght0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  kght1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

#endif /* ALLOW_OFFLINE*/


