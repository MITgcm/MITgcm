C $Header: /u/gcmpack/MITgcm/eesupp/src/exch_cycle_ebl.F,v 1.2 2001/02/04 14:38:43 cnh Exp $
C $Name:  $

      SUBROUTINE EXCH_CYCLE_EBL( myThid )
C     /==========================================================\
C     | SUBROUTINE EXCH_CYCLE_EBL                                |
C     | o Cycle exchange buffer level.                           |
C     |==========================================================|
C     | Step the "exchange buffer level" to next value.          |
C     | The buffer level is used to prevent consecutive messages |
C     | overwriting one another. Because we know that every tile |
C     | will participate in an exchange we can be sure that every|
C     | tile will cycle through the buffers with the same        |
C     | sequence. Also we know that a tile cannot start a new    |
C     | exchange before it has completed its end of the previous |
C     | exchange. Thus by cycling through three buffer levels we |
C     | can be sure that the buffer we write to will be free.    |
C     |  That is by the time we are participating in the third   |
C     | exchange transaction we know that both ends of the first |
C     | transaction must have completed.                         |
C     | Note --                                                  |
C     | 1. We allow more than three levels below although this   |
C     | is not necessary.                                        |
C     \==========================================================/
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EXCH.h"

C     == Routine arguments ==
C     myThid - Thread number of this instance of the routine
      INTEGER myThid

C     == Local variables ==
C     bi, bj - Loop counters
      INTEGER bi, bj

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C--     Use next level exchange buffer in cycle.
        exchangeBufLevel(1,bi,bj) = exchangeBufLevel(1,bi,bj)+1
        IF ( exchangeBufLevel(1,bi,bj) .GT. NUMBER_OF_BUFFER_LEVELS )
     &   exchangeBufLevel(1,bi,bj) = 1
       ENDDO
      ENDDO

      RETURN
      END
