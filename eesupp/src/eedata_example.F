C $Header: /u/gcmpack/MITgcm/eesupp/src/eedata_example.F,v 1.4 2001/02/04 14:38:42 cnh Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

      SUBROUTINE EEDATA_EXAMPLE
C     /==========================================================\
C     | S/R EEDATA_EXAMPLE                                       |
C     | o Write example eedata file                              |
C     |==========================================================|
C     | Notes                                                    |
C     | =====                                                    |
C     | Some systems require & as the namelist terminator.       |
C     | Other systems use a / character.                         |
C     \==========================================================/
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"

C     -- Local variables --
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      
      WRITE(msgBuf,'(A)') '// Shown below is an example "eedata" file.'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '// To use this example copy and paste the  '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '// ">" lines. Then remove the text up to' 
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1) 
      WRITE(msgBuf,'(A)') '// and including the ">".'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '># Example "eedata" file'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '># Lines beginning "#" are comments'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '># nTx - No. threads per process in X'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '># nTy - No. threads per process in Y'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>&EEPARMS              '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>nTx=1,nTy=1           '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '>/                     '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '># Note: Some systems use & as the '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '># namelist terminator. Other systems'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') '># use a / character (as shown here).'
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT,1)
C
      RETURN
      END

