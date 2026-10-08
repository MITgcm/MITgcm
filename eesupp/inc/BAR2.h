!
!BOP
! !ROUTINE: BAR2.h
! !INTERFACE:
! include "BAR2.h"
! !DESCRIPTION:
! *==========================================================*
! | BAR2.h
! | o Globals used by BAR2 Fortran barrier routine.
! |==========================================================*
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
! |     CALL BAR2\_INIT
! |     on CRAY, NEC this routine does nothing
! |     on T3E there is no single-threaded code
! |        but there may be barrier initialisation -
! |        need to check.
! |  2. When we need to synchronize everybody just
! |     CALL BAR2( myThid )
! |     where myThid is myThreadId
! |     on CRAY, NEC FBAR will just do C\$DIR BARRIER
! |     or CALL BARRIER or the like.
! |     on T3E FBAR does CALL BARRIER(...) or something
! |     need to check this.
! *==========================================================*
!EOP
      COMMON /BAR2_BUFFER_I/                                                      &
     &      BAR2_level,                                                           &
     &      BAR2_barrierCount,                                                    &
     &      BAR2_spinsCount, BAR2_spinsMax, BAR2_spinsMin
      INTEGER :: BAR2_level(cacheLineSize/4,MAX_NO_THREADS)
      INTEGER :: BAR2_barrierCount(cacheLineSize/4,MAX_NO_THREADS)
      INTEGER :: BAR2_spinsCount(cacheLineSize/4,MAX_NO_THREADS)
      INTEGER :: BAR2_spinsMax(cacheLineSize/4,MAX_NO_THREADS)
      INTEGER :: BAR2_spinsMin(cacheLineSize/4,MAX_NO_THREADS)

      COMMON /BAR2_L/ bar2CollectStatistics
      LOGICAL :: bar2CollectStatistics
