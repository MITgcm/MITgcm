C $Header: /u/gcmpack/MITgcm/eesupp/src/exch_init.F,v 1.3 2001/02/04 14:38:43 cnh Exp $
C $Name:  $
      SUBROUTINE EXCH_INIT
C     /==========================================================\
C     | SUBROUTINE EXCH_INIT                                     |
C     | o Initialise model exchange data structures.             |
C     \==========================================================/
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EXCH.h"

C     == Routine arguments ==

C     == Local variables ==
C     bi, bj - Loop counters
      INTEGER bi, bj

      DO bj=1,nSy
       DO bi=1,nSx
        exchangeBufLevel(1,bi,bj)   = 1
        exchRecvXSpinCount(1,bi,bj) = 0
        exchRecvXSpinMax  (1,bi,bj) = 0
        exchRecvXSpinMin  (1,bi,bj) = 1000000000
        exchRecvYSpinCount(1,bi,bj) = 0
        exchRecvYSpinMax  (1,bi,bj) = 0
        exchRecvYSpinMin  (1,bi,bj) = 1000000000
       ENDDO
      ENDDO

      exchCollectStatistics = .TRUE.

      RETURN
      END
