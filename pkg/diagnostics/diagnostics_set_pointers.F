C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_set_pointers.F,v 1.2 2004/12/15 00:18:39 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

CBOP
C     !ROUTINE: DIAGNOSTICS_SET_POINTERS
C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_SET_POINTERS( myThid )

C     !DESCRIPTION: \bv
C     *==================================================================
C     | S/R DIAGNOSTICS_SET_POINTERS
C     | o set pointers for active diagnostics
C     *==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER ndiagcount
      INTEGER m,mm,n
      INTEGER mate, nActiveMax
      LOGICAL found
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C--   Calculate pointers for diagnostics set to non-zero frequency

      _BEGIN_MASTER( myThid)

      ndiagcount = 0
      nActiveMax = 0
      DO n=1,nlists
       nActive(n) = nfields(n)
       DO m=1,nfields(n)

         found = .FALSE.
C        Search all possible model diagnostics
         DO mm=1,ndiagt
          IF ( flds(m,n).EQ.cdiag(mm) ) THEN
            CALL DIAGNOSTICS_SETDIAG (mate,ndiagcount,mm,myThid)
            found = .TRUE.
            jdiag(m,n) = mm
          ENDIF
         ENDDO
         IF ( .NOT.found ) THEN
           WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_SET_POINTERS: ', 
     &                      flds(m,n),' is not a Diagnostic'
           CALL PRINT_ERROR( msgBuf , myThid )
           STOP 'ABNORMAL END: S/R DIAGNOSTICS_SET_POINTERS'
         ENDIF
         IF ( found .AND. mate.GE.1 ) THEN
            nActive(n) = nActive(n) + 1
            IF ( nActive(n).LE.numperlist ) THEN
             jdiag(nActive(n),n) = mate
             flds( nActive(n),n) = cdiag(mate)
           ENDIF
         ENDIF

       ENDDO
       nActiveMax = MAX(nActive(n),nActiveMax)
      ENDDO

      IF (  ndiagcount.LE.numdiags .AND.
     &      nActiveMax.LE.numperlist ) THEN
        WRITE(msgBuf,'(A)') 'DIAGNOSTICS_SET_POINTERS: done'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)
        WRITE(msgBuf,'(A,I6,A)')
     &    '  space allocated for all diagnostics:', 
     &    ndiagcount, ' levels'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)
        WRITE(msgBuf,'(2A)')
     &   '------------------------------------------------------------'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)
      ELSE
       IF ( ndiagcount.GT.numdiags ) THEN
         WRITE(msgBuf,'(2A)')
     &    'DIAGNOSTICS_SET_POINTERS: Not enough space',
     &    ' for all active diagnostics (from data.diagnostics)'
         CALL PRINT_ERROR( msgBuf , myThid )
         WRITE(msgBuf,'(A,I6,A,I6)')
     &    'DIAGNOSTICS_SET_POINTERS: numdiags=', numdiags,
     &    ' but needs at least', ndiagcount
         CALL PRINT_ERROR( msgBuf , myThid )
       ENDIF
       IF ( nActiveMax.GT.numperlist ) THEN
         WRITE(msgBuf,'(2A)')
     &    'DIAGNOSTICS_SET_POINTERS: Not enough space',
     &    ' for all active diagnostics (from data.diagnostics)'
         CALL PRINT_ERROR( msgBuf , myThid )
         WRITE(msgBuf,'(A,I6,A,I6)')
     &    'DIAGNOSTICS_SET_POINTERS: numperlist=', numperlist,
     &    ' but needs at least', nActiveMax
         CALL PRINT_ERROR( msgBuf , myThid )
       ENDIF
       STOP 'ABNORMAL END: S/R DIAGNOSTICS_SET_POINTERS'
      ENDIF

      _END_MASTER( myThid )

      RETURN
      END
