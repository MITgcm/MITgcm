C $Header: /u/gcmpack/MITgcm/eesupp/src/memsync.F,v 1.3 1999/05/18 17:39:21 adcroft Exp $

#include "CPP_EEOPTIONS.h"

      SUBROUTINE MEMSYNC

C     /==========================================================\
C     | SUBROUTINE MEMSYNC                                       |
C     | o Place holder for memory sync routine                   |
C     |==========================================================|
C     | This routine is called wherever the code requires        |
C     | that writes performed by one thread or process must      |
C     | be guaranteed visible to other processes and or threads. |
C     |  On SPARC this routine would implement a                 |
C     |  asm("membar #LoadStore|#StoreStore");                   |
C     |  On ALPHA                                                |
C     |  asm("mb");                                              |
C     |  Note: I have yet to show this is necessary although     |
C     |        the Memory Channel writes must need something like|
C     |        this.                                             |
C     \==========================================================/
      IMPLICIT NONE

      END
