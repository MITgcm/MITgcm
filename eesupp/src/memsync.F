C $Header: /u/gcmpack/MITgcm/eesupp/src/memsync.F,v 1.4 2001/02/04 14:38:44 cnh Exp $
C $Name:  $

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
