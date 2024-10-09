CBOP
C     !ROUTINE: GLOBAL_SUM.h
C     !INTERFACE:
C     include "GLOBAL_SUM.h"
C     !DESCRIPTION:
C     *==========================================================*
C     | GLOBAL\_SUM.h
C     | o Globals used by Fortran global sum routine.
C     *==========================================================*
C     | The global sum shared memory scheme uses global heap data|
C     | structures (.i.e COMMON blocks ). Each thread writes to  |
C     | an its own element of the shared memory array and then   |
C     | one thread reads all the entries and sums them. The sum  |
C     | result is then read by all threads.                      |
C     | Remember - you are working with regions of memory that   |
C     | are being updated concurrently by different threads.     |
C     | What happens, when it happens and who gets to see what   |
C     | happens at what stage depends on the computer systems    |
C     | memory model. Every computer has a different memory model|
C     | and they are never simple. In all current platforms it is|
C     | possible for one thread to see events happening in a     |
C     | different order from the order they are written in the   |
C     | code.                                                    |
C     | Unless you understand this it is not a good idea to      |
C     | make modifications te way these header files are setup or|
C     | the way the global sum routines work.                    |
C     *==========================================================*
CEOP

      COMMON / GSUM_COMMON_R8 / phiGSR8, shareBufGSR8
      Real*8  phiGSR8 (lShare8, 0:MAX_NO_THREADS )
      Real*8  shareBufGSR8 ( nSx, nSy )

      COMMON / GSUM_COMMON_R4 / phiGSR4
      Real*4  phiGSR4 (lShare4, 0:MAX_NO_THREADS )

      COMMON / GSUM_COMMON_I  / phiGSI
      INTEGER phiGSI  (lShare4, 0:MAX_NO_THREADS )

      COMMON / GLBSUM_VECTOR_RL / shareBufGSVec, shareGSVector
      Real*8  shareBufGSVec( nSx, nSy, GSVec_size )
      Real*8  shareGSVector( GSVec_size )

      COMMON / GLBSUM_VECTOR_I / shareBufGSVecI, shareGSVectInt
      INTEGER shareBufGSVecI( nSx, nSy, GSVec_size )
      INTEGER shareGSVectInt( GSVec_size )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
