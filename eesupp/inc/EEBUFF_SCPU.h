!BOP
! !ROUTINE: EEBUFF_SCPU.h
! !INTERFACE:
! include "EEBUFF_SCPU.h"
!
! !DESCRIPTION:
! *==========================================================*
! | EEBUFF_SCPU.h
! | o Buffers used by S/R gather_2d and scatter_2d,
! |   in mapping 2-D local array from all processes to/from
! |   2-D Global (X,Y) array from Master-Proc (SingleCPU)
! | o Contain both 2-D Global (X,Y) buffers and
! |   Shared Local Buffer (for multi-threaded).
! *==========================================================*
! | presently used with:
! | - SingleCpu IO
! | - global-sum SingleCpu
! *==========================================================*
!EOP

      INTEGER :: xyBuffer_size
#ifdef ALLOW_EXCH2
      PARAMETER ( xyBuffer_size = W2_ioBufferSize )
#else  /* ALLOW_EXCH2 */
      PARAMETER ( xyBuffer_size = Nx*Ny )
#endif /* ALLOW_EXCH2 */

!--   COMMON /EE_BUFFERS_GLOBAL/  2-D Global Buffers
! Those buffers are in common block to save some memory
! xy_buffer_r8 :: 2-D global Real*8 buffer.
! xy_buffer_r4 :: 2-D global Real*4 buffer.
      COMMON /EE_BUFFERS_GLOBAL/ xy_buffer_r8, xy_buffer_r4
      Real(kind=8) :: xy_buffer_r8(xyBuffer_size)
      Real(kind=4) :: xy_buffer_r4(xyBuffer_size)

!--   COMMON /EE_BUFFERS_LOCAL/  2-D Shared Local Buffers
! Those buffers have be in common block to be shared by all threads
! sharedLocBuf_rx :: Heap storage buffer to which master thread copies
!                data (during read) as part of a scatter/gather and
!                from which all threads read data (during read).
      COMMON /EE_BUFFERS_LOCAL/ sharedLocBuf_r8, sharedLocBuf_r4
      Real(kind=8) :: sharedLocBuf_r8(1:sNx,1:sNy,nSx,nSy)
      Real(kind=4) :: sharedLocBuf_r4(1:sNx,1:sNy,nSx,nSy)

!---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
