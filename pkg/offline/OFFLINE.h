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
     &       ConvFile, GMwxFile, GMwyFile, GMwzFile, deltaToffline,
     &       offlineIter0, offlineForcingPeriod, offlineForcingCycle   
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
      INTEGER offlineIter0
      _RL deltaToffline
      _RL offlineForcingPeriod
      _RL offlineForcingCycle

#endif /* ALLOW_OFFLINE*/


