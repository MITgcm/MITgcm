C $Header: /u/gcmpack/MITgcm/eesupp/src/eeintro_msg.F,v 1.7 2001/09/27 18:13:40 adcroft Exp $

#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: EEINTRO_MSG

C     !INTERFACE:  
      SUBROUTINE EEINTRO_MSG
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================
C     | SUBROUTINE EEINTRO_MSG                                   
C     | o Write basic WRAPPER introductory message.                      
C     *==========================================================
C     | This routine could be customised for different platforms.
C     | Output includes which checkpoint of the code is running.
C     | The routine doesnt currently report the user id or      
C     | machine name correctly.                                  
C     *==========================================================

C     !USES:
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     !LOCAL VARIABLES:
C     === Local variables ===
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP
      

      WRITE(msgBuf,'(A)') '                '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') 
     &'// ======================================================'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '//                      MITgcm UV'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '//                      ========='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') 
     &'// ======================================================'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '// execution environment starting up...'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '// Run starting Wed Apr  1 14:19:48 1998'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '// on melville.lcs.mit.edu'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '// account cnh'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') 
     &'// MITgcm UV Release: $Name:  $'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '   '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
C
      RETURN
      END
