C $Header: /u/gcmpack/MITgcm/pkg/mdsio/Attic/MDSIO_SCPU.h,v 1.2 2007/07/19 15:34:50 dimitri Exp $
C $Name:  $
C
C Buffers for single CPU IO that allow it to work in multi-threaded code
C
C     sharedLocalBuf - Heap storage buffer to which master thread copies 
C                      data (during read) as part of a scatter/gather and 
C                      from which all threads read data (during read).
      COMMON /MDSIO_SCPU_R/ sharedLocalBuf
      _RL sharedLocalBuf(1:sNx,1:sNy,nSx,nSy)
