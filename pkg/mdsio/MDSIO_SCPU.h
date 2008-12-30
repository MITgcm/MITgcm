C $Header: /u/gcmpack/MITgcm/pkg/mdsio/Attic/MDSIO_SCPU.h,v 1.3 2008/12/30 00:13:35 jahn Exp $
C $Name:  $
C
C Buffers for single CPU IO that allow it to work in multi-threaded code
C
C     sharedLocalBuf - Heap storage buffer to which master thread copies 
C                      data (during read) as part of a scatter/gather and 
C                      from which all threads read data (during read).
      INTEGER x_size,y_size
#if defined(ALLOW_EXCH2) && !defined(MISSING_TILE_IO)
      PARAMETER ( x_size = exch2_domain_nxt * sNx )
      PARAMETER ( y_size = exch2_domain_nyt * sNy )
#else
      PARAMETER ( x_size = Nx )
      PARAMETER ( y_size = Ny )
#endif
      COMMON /MDSIO_SCPU_R/ sharedLocalBuf
      _RL sharedLocalBuf(1:sNx,1:sNy,nSx,nSy)

      COMMON /MDSIO_BUFFERS/ xy_buffer_r4, xy_buffer_r8, globalBuf 
      Real*4 xy_buffer_r4(x_size,y_size)
      Real*8 xy_buffer_r8(x_size,y_size)
      Real*8 globalBuf(Nx,Ny)
