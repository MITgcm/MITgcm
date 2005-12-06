#ifdef  ALLOW_OFFLINE   
c     !ROUTINE: OFFLINE.h
c -------------------------------
c   OFFLINE.h
C  variable for forcing offline tracer
c -------------------------------

c   Forcing files
      COMMON /OFFLINE_FFIELDS/
     &       ConvectCount,
     &       UvelFile, VvelFile, WvelFile, ThetFile, Saltfile,
     &       ConvFile, GMwxFile, GMwyFile, GMwzFile, 
     &       HfluxFile, SfluxFile,
     &       KPP_DiffSFile, KPP_ghatFile,
     &       deltaToffline,
     &       offlineIter0, offlineOffsetIter,
     &       offlineForcingPeriod, offlineForcingCycle   
      _RS  ConvectCount(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
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
      CHARACTER*(MAX_LEN_FNAM) KPP_DiffSFile
      CHARACTER*(MAX_LEN_FNAM) KPP_ghatFile
      INTEGER offlineIter0
      INTEGER offlineOffsetIter
      _RL deltaToffline
      _RL offlineForcingPeriod
      _RL offlineForcingCycle

#endif /* ALLOW_OFFLINE*/


