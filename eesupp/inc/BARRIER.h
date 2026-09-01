!BOP
! !ROUTINE: BARRIER.h
! !INTERFACE:
! include "BARRIER.h"
! !DESCRIPTION:
! *==========================================================*
! | BARRIER.h
! | o Globals used by Fortran barrier routine.
! *==========================================================*
! | These variables support a simple Fortran shared memory
! | barrier routine. They do busy waiting, that is the
! | thread that is waiting sits in a tight loop reading
! | some memory location and waiting for all other threads.
! | On some systems this is sometimes a good method to use.
! | On the T3E memory is not shared so the routine should
! | use the T3E "eureka" barriers. On CRAY and NEC there are
! | hardware barriers that are accessed through a compiler
! | directives. Finally proper multi-threading compilers
! | support barrier compile directives - sometimes these
! | are good, sometimes they are lousy.
! |  The barrier mechanism is used as follows
! |  1. In the single-threaded part of the code
! |     CALL FBAR\_INIT
! |     on CRAY, NEC this routine does nothing
! |     on T3E there is no single-threaded code
! |        but there may be barrier initialisation -
! |        need to check.
! |  2. When we need to synchronize everybody just
! |     CALL FBAR( myThid )
! |     where myThid is myThreadId
! |     on CRAY, NEC FBAR will just do C\$DIR BARRIER
! |     or the like.
! |     on T3E FBAR does CALL BARRIER(...) or something
! |     need to check this.
! *==========================================================*
!EOP
      COMMON / BARRIER_COMMON / key1,  key2,  key3,                               &
     &      door1, door2, door3,                                                  &
     &      bCount, masterSet
      INTEGER :: key1(lShare4,MAX_NO_THREADS)
      INTEGER :: key2(lShare4,MAX_NO_THREADS)
      INTEGER :: key3(lShare4,MAX_NO_THREADS)
      INTEGER :: door1
      INTEGER :: door2
      INTEGER :: door3
      INTEGER :: VALID
      PARAMETER ( VALID = 1 )
      INTEGER :: INVALID
      PARAMETER ( INVALID = 0 )
      INTEGER :: OPEN
      PARAMETER ( OPEN = 1 )
      INTEGER :: SHUT
      PARAMETER ( SHUT = 0 )
      INTEGER :: bCount(MAX_NO_THREADS)
      INTEGER :: masterSet(MAX_NO_THREADS)
