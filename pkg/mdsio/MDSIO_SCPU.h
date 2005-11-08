C $Header: /u/gcmpack/MITgcm/pkg/mdsio/Attic/MDSIO_SCPU.h,v 1.1 2005/11/08 23:03:17 cnh Exp $
C $Name:  $
C
C Buffers for single CPU IO that allow it to work in multi-threaded code
C
C     sharedLocalBuf - Heap storage buffer to which master thread copies 
C                      data (during read) as part of a scatter/gather and 
C                      from which all threads read data (during read).
      COMMON /MDSIO_SCPU_R/ sharedLocalBuf
      _RL sharedLocalBuf(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
