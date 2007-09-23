C $Header: /u/gcmpack/MITgcm/eesupp/src/eeintro_msg.F,v 1.11 2007/09/23 22:48:04 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: EEINTRO_MSG

C     !INTERFACE:
      SUBROUTINE EEINTRO_MSG
      IMPLICIT NONE

C     !DESCRIPTION:
C     Write basic WRAPPER introductory message.  This routine could be
C     customised for different platforms.  Output includes which
C     checkpoint of the code is running.  The routine doesnt currently
C     report the user id or machine name correctly.

C     !USES:
#include "BUILD_INFO.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      WRITE(msgBuf,'(A)') '                '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)')
     &     '// ======================================================'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '//                      MITgcm UV'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '//                      ========='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)')
     &     '// ======================================================'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '// execution environment starting up...'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '   '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)

#ifdef THISVER
      WRITE(msgBuf,'(2A)') '// MITgcmUV version:  ',
     &     THISVER
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)
#endif
#ifdef THISUSER
      WRITE(msgBuf,'(2A)') '// Build user:        ',
     &     THISUSER
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)
#endif
#ifdef THISHOST
      WRITE(msgBuf,'(2A)') '// Build host:        ',
     &     THISHOST
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)
#endif
#ifdef THISDATE
      WRITE(msgBuf,'(2A)') '// Build date:        ',
     &     THISDATE
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)
#endif

      WRITE(msgBuf,'(A)') '   '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)
C
      RETURN
      END
