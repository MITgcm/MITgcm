C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_check.F,v 1.4 2005/05/20 07:28:49 jmc Exp $
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

      DO n = 2,nlists
       DO m = 1,n-1
        IF ( freq(m).NE.freq(n) .OR. phase(m).NE.phase(n) ) THEN
C--     once the SWITCH_ONOFF is changed to only turns ON diag with <0 freq
C                 and CLRDIAG is changed to turns OFF diag with <0 freq,
C       then we can allow 1 diag to be used with 2 differents <0 freq.
C       and this would become:
c       IF (  ( freq(m).GT.0. .OR. freq(n).GT.0. )
c    &   .AND.( freq(m).NE.freq(n) .OR. phase(m).NE.phase(n) )
c    &     ) THEN
         DO k = 1,nActive(n)
          DO l = 1,nActive(m)
           IF ( flds(k,n).EQ.flds(l,m) ) THEN
            WRITE(msgBuf,'(4A)') 'DIAGNOSTICS_CHECK: ',
     &       'field : ',flds(k,n),' use 2 different freq. :'
            CALL PRINT_ERROR( msgBuf , myThid )
            WRITE(msgBuf,'(2A,I3,A,2F17.6,2A)') 'DIAGNOSTICS_CHECK: ',
     &       '1rst (m=', m, ' ) freq,phase=', freq(m),phase(m),
     &       ' file:',fnames(m)
            CALL PRINT_ERROR( msgBuf , myThid )
            WRITE(msgBuf,'(2A,I3,A,2F17.6,2A)') 'DIAGNOSTICS_CHECK: ',
     &       ' 2nd (n=', n, ' ) freq,phase=', freq(n),phase(n),
     &       ' file:',fnames(n)
            CALL PRINT_ERROR( msgBuf , myThid )
            STOP 'ABNORMAL END: S/R DIAGNOSTICS_CHECK'
           ENDIF
          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDDO

      DO n = 2,diagSt_nbLists
       DO m = 1,n-1
        IF    ( diagSt_freq(m) .NE. diagSt_freq(n) .OR.
     &          diagSt_phase(m).NE.diagSt_phase(n) ) THEN
c       IF (  ( diagSt_freq(m).GT.0. .OR. diagSt_freq(n).GT.0. )
c    &   .AND.( diagSt_freq(m) .NE. diagSt_freq(n) .OR.
c    &          diagSt_phase(m).NE.diagSt_phase(n) )
c    &     ) THEN
         DO k = 1,diagSt_nbActv(n)
          DO l = 1,diagSt_nbActv(m)
           IF ( diagSt_Flds(k,n).EQ.diagSt_Flds(l,m) ) THEN
            WRITE(msgBuf,'(4A)') 'DIAGNOSTICS_CHECK: ',
     &       'field : ',diagSt_Flds(k,n),' use 2 different stat_freq.:'
            CALL PRINT_ERROR( msgBuf , myThid )
            WRITE(msgBuf,'(2A,I3,A,2F17.6,2A)') 'DIAGNOSTICS_CHECK: ',
     &       '1rst (m=', m, ' ) freq,phase=', diagSt_freq(m),
     &       diagSt_phase(m), ' file:', diagSt_Fname(m)
            CALL PRINT_ERROR( msgBuf , myThid )
            WRITE(msgBuf,'(2A,I3,A,2F17.6,2A)') 'DIAGNOSTICS_CHECK: ',
     &       ' 2nd (n=', n, ' ) freq,phase=', diagSt_freq(n),
     &       diagSt_phase(n), ' file:', diagSt_Fname(n)
            CALL PRINT_ERROR( msgBuf , myThid )
            STOP 'ABNORMAL END: S/R DIAGNOSTICS_CHECK'
           ENDIF
          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _END_MASTER(myThid)

      RETURN
      END
