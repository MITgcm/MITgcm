C $Header: /u/gcmpack/MITgcm/eesupp/src/comm_stats.F,v 1.4 2001/02/04 14:38:42 cnh Exp $
C $Name:  $
#include "CPP_EEOPTIONS.h"

      SUBROUTINE COMM_STATS
C     /==========================================================\
C     | SUBROUTINE COMM_STATS                                    |
C     | o Print out statistics from communication routines.      |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EXCH.h"
#include "BAR2.h"

C     == Local variables ==
      INTEGER bi, bj, I
      CHARACTER*(MAX_LEN_MBUF) msgBuf
    
C      o For each tile print its excange statistics
       WRITE(msgBuf,'(A)')
     & '// ======================================================'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &   SQUEEZE_RIGHT , 1)
       WRITE(msgBuf,'(A)') '// Tile <-> Tile communication statistics'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &   SQUEEZE_RIGHT , 1)
       WRITE(msgBuf,'(A)')
     & '// ======================================================'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &   SQUEEZE_RIGHT , 1)
      
      IF ( exchCollectStatistics ) THEN
       DO bj=1,nSy
        DO bi=1,nSx
         WRITE(msgBuf,'(A,A,I6.6)')
     &    '//',' o Tile number: ',tileNo(bi,bj)
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                      SQUEEZE_RIGHT , 1)
C        o X exchange stats.
         WRITE(msgBuf,'(A,A,I15)')
     &    '//','         No. X exchanges =',exchRecvXExchCount(1,bi,bj)
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)
         WRITE(msgBuf,'(A,A,I15)')
     &    '//','            Max. X spins =',exchRecvXSpinMax  (1,bi,bj)
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)
         WRITE(msgBuf,'(A,A,I15)')
     &    '//','            Min. X spins =',exchRecvXSpinMin  (1,bi,bj)
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)
         WRITE(msgBuf,'(A,A,I15)')
     &    '//','          Total. X spins =',exchRecvXSpinCount(1,bi,bj)
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)
         IF ( FLOAT(exchRecvXExchCount(1,bi,bj)) .NE. 0. ) THEN
          WRITE(msgBuf,'(A,A,1PE15.2)')
     &    '//','            Avg. X spins =',
     &    FLOAT(exchRecvXSpinCount(1,bi,bj))/
     &    FLOAT(exchRecvXExchCount(1,bi,bj))
         ELSE
          WRITE(msgBuf,'(A,A,1PE15.2)')
     &    '//','            Avg. X spins =',
     &    0.
         ENDIF
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)
C        o Y exchange stats.
         WRITE(msgBuf,'(A,A,I15)')
     &    '//','         No. Y exchanges =',exchRecvYExchCount(1,bi,bj)
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)
         WRITE(msgBuf,'(A,A,I15)')
     &    '//','            Max. Y spins =',exchRecvYSpinMax  (1,bi,bj)
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)
         WRITE(msgBuf,'(A,A,I15)')
     &    '//','            Min. Y spins =',exchRecvYSpinMin  (1,bi,bj)
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)
         WRITE(msgBuf,'(A,A,I15)')
     &    '//','          Total. Y spins =',exchRecvYSpinCount(1,bi,bj)
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)
         IF ( FLOAT(exchRecvYExchCount(1,bi,bj)) .NE. 0. ) THEN
          WRITE(msgBuf,'(A,A,1PE15.2)')
     &    '//','            Avg. Y spins =',
     &    FLOAT(exchRecvYSpinCount(1,bi,bj))/
     &    FLOAT(exchRecvYExchCount(1,bi,bj))
         ELSE
          WRITE(msgBuf,'(A,A,1PE15.2)')
     &    '//','            Avg. Y spins =',
     &    0.
         ENDIF
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)
       
        ENDDO
       ENDDO
      ENDIF
 
      IF ( bar2CollectStatistics ) THEN
       DO I=1,nThreads
         WRITE(msgBuf,'(A,A,I6.6)')
     &    '//',' o Thread number: ',I
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                      SQUEEZE_RIGHT , 1)
         WRITE(msgBuf,'(A,A,I15)')
     &    '//','            No. barriers =',BAR2_barrierCount(1,I)
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)
         WRITE(msgBuf,'(A,A,I15)')
     &    '//','      Max. barrier spins =',BAR2_spinsMax(1,I)
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)
         WRITE(msgBuf,'(A,A,I15)')
     &    '//','      Min. barrier spins =',BAR2_spinsMin(1,I)
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)
         WRITE(msgBuf,'(A,A,I15)')
     &    '//','     Total barrier spins =',BAR2_spinsCount(1,I)
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)
         IF ( FLOAT(BAR2_spinsCount(1,I)) .NE. 0. ) THEN
          WRITE(msgBuf,'(A,A,1PE15.2)')
     &    '//','      Avg. barrier spins =',
     &    FLOAT(BAR2_spinsCount(1,I))/FLOAT(BAR2_barrierCount(1,I))
         ELSE
          WRITE(msgBuf,'(A,A,1PE15.2)')
     &    '//','      Avg. barrier spins =',
     &    0.
         ENDIF
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)
       ENDDO
      ENDIF

C
      RETURN
      END

