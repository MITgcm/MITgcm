!BOP
! !ROUTINE: GLOBAL_MAX.h
! !INTERFACE:
! include "GLOBAL_MAX.h"
! !DESCRIPTION:
! *==========================================================*
! | GLOBAL\_MAX.h
! | o Globals used by Fortran global max routine.
! *==========================================================*
! | The global max shared memory scheme uses global heap data
! | structures (.i.e COMMON blocks ). Each thread writes to
! | an its own element of the shared memory array and then
! | one thread reads all the entries and maxs them. The max
! | result is then read by all threads.
! | Remember - you are working with regions of memory that
! | are being updated concurrently by different threads.
! | What happens, when it happens and who gets to see what
! | happens at what stage depends on the computer systems
! | memory model. Every computer has a different memory model
! | and they are never simple. In all current platforms it is
! | possible for one thread to see events happening in a
! | different order from the order they are written in the
! | code.
! | Unless you understand this it is not a good idea to
! | make modifications te way these header files are setup or
! | the way the global max routines work.
! *==========================================================*
!EOP
      COMMON / GMAX_COMMON_R8 / phiGMR8
      Real(kind=8) :: phiGMR8(lShare8, 0:MAX_NO_THREADS )

      COMMON / GMAX_COMMON_R4 / phiGMR4
      Real(kind=4) :: phiGMR4(lShare4, 0:MAX_NO_THREADS )

      COMMON / GMAX_COMMON_I  / phiGMI
      INTEGER :: phiGMI (lShare4, 0:MAX_NO_THREADS )
