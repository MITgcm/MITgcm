C $Header: /u/gcmpack/MITgcm/eesupp/inc/EEBUFF_SCPU.h,v 1.1 2009/05/16 13:33:33 jmc Exp $
C $Name:  $

CBOP
C     !ROUTINE: EEBUFF_SCPU.h
C     !INTERFACE:
C     include "EEBUFF_SCPU.h"
C
C     !DESCRIPTION:
C     *==========================================================*
C     | EEBUFF_SCPU.h
C     | o Buffers used by S/R gather_2d and scatter_2d,
C     |   in mapping 2-D local array from all processes to/from
C     |   2-D Global (X,Y) array from Master-Proc (SingleCPU)
C     | o Contain both 2-D Global (X,Y) buffers and
C     |   Shared Local Buffer (for multi-threaded).
C     *==========================================================*
C     | presently used with:
C     | - SingleCpu IO
C     | - global-sum SingleCpu
C     *==========================================================*
CEOP

      INTEGER xyBuffer_size
#ifdef ALLOW_EXCH2
      PARAMETER ( xyBuffer_size = W2_ioBufferSize )
#else  /* ALLOW_EXCH2 */
      PARAMETER ( xyBuffer_size = Nx*Ny )
#endif /* ALLOW_EXCH2 */

C--   COMMON /EE_BUFFERS_GLOBAL/  2-D Global Buffers
C     Those buffers are in common block to save some memory
C     xy_buffer_r8 :: 2-D global Real*8 buffer.
C     xy_buffer_r4 :: 2-D global Real*4 buffer.
      COMMON /EE_BUFFERS_GLOBAL/ xy_buffer_r8, xy_buffer_r4
      Real*8 xy_buffer_r8(xyBuffer_size)
      Real*4 xy_buffer_r4(xyBuffer_size)

C--   COMMON /EE_BUFFERS_LOCAL/  2-D Shared Local Buffers
C     Those buffers have be in common block to be shared by all threads
C sharedLocBuf_rx :: Heap storage buffer to which master thread copies
C                    data (during read) as part of a scatter/gather and
C                    from which all threads read data (during read).
      COMMON /EE_BUFFERS_LOCAL/ sharedLocBuf_r8, sharedLocBuf_r4
      Real*8 sharedLocBuf_r8(1:sNx,1:sNy,nSx,nSy)
      Real*4 sharedLocBuf_r4(1:sNx,1:sNy,nSx,nSy)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
