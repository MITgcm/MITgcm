C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_check.F,v 1.5 2005/06/26 16:51:49 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_CHECK

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_CHECK(myThid)

C     !DESCRIPTION:
C     Check option and parameter consistency
      
C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER k,l,n,m

      _BEGIN_MASTER(myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Check diagnostics parameter consistency

#ifdef DIAGNOSTICS_HAS_PICKUP
      IF ( diag_pickup_read ) THEN
        WRITE(msgBuf,'(2A)') '**CAUTION** (DIAGNOSTICS_CHECK): ',
     &   'reading diagnostics previous state'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &       SQUEEZE_RIGHT , myThid)
        WRITE(msgBuf,'(2A)') '**CAUTION** ',
     &   ' from a pickup file can only work if data.diagnostics'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &       SQUEEZE_RIGHT , myThid)
        WRITE(msgBuf,'(2A)') '**CAUTION** ',
     &   ' is not changed (<= further checking not yet implemented)'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &       SQUEEZE_RIGHT , myThid)
      ENDIF
#else /* undef DIAGNOSTICS_HAS_PICKUP */
C-    stop if trying to use part of the code that is not compiled:
      IF ( diag_pickup_read  ) THEN
        WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_CHECK: ',
     &   'diag_pickup_read  is TRUE ',
     &   'but DIAGNOSTICS_HAS_PICKUP is "#undef"'
        CALL PRINT_ERROR( msgBuf , myThid)
      ENDIF
      IF ( diag_pickup_write ) THEN
        WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_CHECK: ',
     &   'diag_pickup_write is TRUE ',
     &   'but DIAGNOSTICS_HAS_PICKUP is "#undef"'
        CALL PRINT_ERROR( msgBuf , myThid)
      ENDIF
      IF ( diag_pickup_read .OR. diag_pickup_write ) THEN
        STOP 'ABNORMAL END: S/R DIAGNOSTICS_CHECK'
      ENDIF
#endif /* DIAGNOSTICS_HAS_PICKUP */
      
C-    File names:
      DO n = 2,nlists
       DO m = 1,n-1
        IF ( fnames(n).EQ.fnames(m) ) THEN
         WRITE(msgBuf,'(2A)') 'DIAGNOSTICS_CHECK: ',
     &            'found 2 identical filenames:'
         CALL PRINT_ERROR( msgBuf , myThid )
         WRITE(msgBuf,'(2A,I3,2A)') 'DIAGNOSTICS_CHECK: ',
     &    '1rst (m=', m, ' ): ', fnames(m)
         CALL PRINT_ERROR( msgBuf , myThid )
         WRITE(msgBuf,'(2A,I3,2A)') 'DIAGNOSTICS_CHECK: ',
     &    ' 2nd (n=', n, ' ): ', fnames(n)
         CALL PRINT_ERROR( msgBuf , myThid )
         STOP 'ABNORMAL END: S/R DIAGNOSTICS_CHECK'
        ENDIF
       ENDDO
      ENDDO

      DO n = 2,diagSt_nbLists
       DO m = 1,n-1
        IF ( diagSt_Fname(n).EQ.diagSt_Fname(m) ) THEN
         WRITE(msgBuf,'(2A)') 'DIAGNOSTICS_CHECK: ',
     &            'found 2 identical stat_fname:'
         CALL PRINT_ERROR( msgBuf , myThid )
         WRITE(msgBuf,'(2A,I3,2A)') 'DIAGNOSTICS_CHECK: ',
     &    '1rst (m=', m, ' ): ', diagSt_Fname(m)
         CALL PRINT_ERROR( msgBuf , myThid )
         WRITE(msgBuf,'(2A,I3,2A)') 'DIAGNOSTICS_CHECK: ',
     &    ' 2nd (n=', n, ' ): ', diagSt_Fname(n)
         CALL PRINT_ERROR( msgBuf , myThid )
         STOP 'ABNORMAL END: S/R DIAGNOSTICS_CHECK'
        ENDIF
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-    Check for field that appears 2 times (or more) with differents frequency:
C     disable this checking since now diagnostics pkg can handle this case.

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _END_MASTER(myThid)

      RETURN
      END
