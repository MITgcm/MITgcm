!BOP
! !ROUTINE: GLOBAL_SUM.h
! !INTERFACE:
! include "GLOBAL_SUM.h"
! !DESCRIPTION:
! *==========================================================*
! | GLOBAL\_SUM.h
! | o Globals used by Fortran global sum routine.
! *==========================================================*
! | The global sum shared memory scheme uses global heap data|
! | structures (.i.e COMMON blocks ). Each thread writes to  |
! | an its own element of the shared memory array and then   |
! | one thread reads all the entries and sums them. The sum  |
! | result is then read by all threads.                      |
! | Remember - you are working with regions of memory that   |
! | are being updated concurrently by different threads.     |
! | What happens, when it happens and who gets to see what   |
! | happens at what stage depends on the computer systems    |
! | memory model. Every computer has a different memory model|
! | and they are never simple. In all current platforms it is|
! | possible for one thread to see events happening in a     |
! | different order from the order they are written in the   |
! | code.                                                    |
! | Unless you understand this it is not a good idea to      |
! | make modifications te way these header files are setup or|
! | the way the global sum routines work.                    |
! *==========================================================*
!EOP

      COMMON / GSUM_COMMON_R8 / phiGSR8, shareBufGSR8
      Real(kind=8) :: phiGSR8 (lShare8, 0:MAX_NO_THREADS )
      Real(kind=8) :: shareBufGSR8 ( nSx, nSy )

      COMMON / GSUM_COMMON_R4 / phiGSR4
      Real(kind=4) :: phiGSR4 (lShare4, 0:MAX_NO_THREADS )

      COMMON / GSUM_COMMON_I  / phiGSI
      INTEGER :: phiGSI  (lShare4, 0:MAX_NO_THREADS )

      COMMON / GLBSUM_VECTOR_RL / shareBufGSVec, shareGSVector
      Real(kind=8) :: shareBufGSVec( nSx, nSy, GSVec_size )
      Real(kind=8) :: shareGSVector( GSVec_size )

      COMMON / GLBSUM_VECTOR_I / shareBufGSVecI, shareGSVectInt
      INTEGER :: shareBufGSVecI( nSx, nSy, GSVec_size )
      INTEGER :: shareGSVectInt( GSVec_size )
!---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
