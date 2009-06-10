C $Header: /u/gcmpack/MITgcm/eesupp/inc/GLOBAL_MAX.h,v 1.5 2009/06/10 03:45:11 jmc Exp $
C $Name:  $
CBOP
C     !ROUTINE: GLOBAL_MAX.h
C     !INTERFACE:
C     include "GLOBAL_MAX.h"
C     !DESCRIPTION:
C     *==========================================================*
C     | GLOBAL\_MAX.h
C     | o Globals used by Fortran global max routine.
C     *==========================================================*
C     | The global max shared memory scheme uses global heap data
C     | structures (.i.e COMMON blocks ). Each thread writes to
C     | an its own element of the shared memory array and then
C     | one thread reads all the entries and maxs them. The max
C     | result is then read by all threads.
C     | Remember - you are working with regions of memory that
C     | are being updated concurrently by different threads.
C     | What happens, when it happens and who gets to see what
C     | happens at what stage depends on the computer systems
C     | memory model. Every computer has a different memory model
C     | and they are never simple. In all current platforms it is
C     | possible for one thread to see events happening in a
C     | different order from the order they are written in the
C     | code.
C     | Unless you understand this it is not a good idea to
C     | make modifications te way these header files are setup or
C     | the way the global max routines work.
C     *==========================================================*
CEOP
      COMMON / GMAX_COMMON_R8 / phiGMR8
      Real*8  phiGMR8(lShare8, 0:MAX_NO_THREADS )

      COMMON / GMAX_COMMON_R4 / phiGMR4
      Real*4  phiGMR4(lShare4, 0:MAX_NO_THREADS )

      COMMON / GMAX_COMMON_I  / phiGMI
      INTEGER phiGMI (lShare4, 0:MAX_NO_THREADS )
