C $Header: /u/gcmpack/MITgcm/pkg/mdsio/Attic/MDSIO_SCPU.h,v 1.4 2009/05/06 02:41:01 jmc Exp $
C $Name:  $
C
C Buffers for single CPU IO that allow it to work in multi-threaded code
C
C     sharedLocalBuf :: Heap storage buffer to which master thread copies
C                       data (during read) as part of a scatter/gather and
C                       from which all threads read data (during read).
      INTEGER xyBuffer_size
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
      PARAMETER ( xyBuffer_size = W2_ioBufferSize )
#else
      PARAMETER ( xyBuffer_size = Nx*Ny )
#endif
      COMMON /MDSIO_SCPU_R/ sharedLocalBuf
      _RL sharedLocalBuf(1:sNx,1:sNy,nSx,nSy)

      COMMON /MDSIO_BUFFERS/ globalBuf, xy_buffer_r8, xy_buffer_r4
      Real*8 globalBuf(Nx,Ny)
      Real*8 xy_buffer_r8(xyBuffer_size)
      Real*4 xy_buffer_r4(xyBuffer_size)
