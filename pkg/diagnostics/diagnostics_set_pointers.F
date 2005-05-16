C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_set_pointers.F,v 1.4 2005/05/16 15:07:45 jmc Exp $
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
      INTEGER l, k, kLev
      LOGICAL found
      CHARACTER*(MAX_LEN_MBUF) msgBuf


      _BEGIN_MASTER( myThid)

C--   Initialize pointer arrays to zero:
      DO n=1,ndiagMax
        idiag(n) = 0
      ENDDO

C--   Calculate pointers for diagnostics set to non-zero frequency

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
        WRITE(msgBuf,'(A,I6,A)')
     &    '  space allocated for all diagnostics:', 
     &    ndiagcount, ' levels'
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

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Set list of levels to write (if not specified in data.diagnostics)

      DO n=1,nlists
        IF ( nlevels(n).EQ.-1 ) THEN
C-      set Nb of levels to the minimum size of all diag of this list:
          kLev = numLevels
          DO m=1,nfields(n)
            mm = jdiag(m,n)
            kLev = MIN(kdiag(mm),kLev)
          ENDDO
          IF ( kLev.LE.0 ) THEN
            WRITE(msgBuf,'(2A,I4,2A)')
     &      'DIAGNOSTICS_SET_POINTERS: kLev < 1 in ',
     &      ' setting levs of list n=',n,', fnames: ', fnames(n)
            CALL PRINT_ERROR( msgBuf , myThid )
            STOP 'ABNORMAL END: S/R DIAGNOSTICS_SET_POINTERS'
          ENDIF
          nlevels(n) = kLev
          DO k=1,kLev
           levs(k,n) = k
          ENDDO
          WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_SET_POINTERS: ',
     &      'Set levels for Outp.Stream: ',fnames(n)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid)
          DO l=1,nlevels(n),20
            m = MIN(nlevels(n),l+19)
            WRITE(msgBuf,'(A,20F5.0)')' Levels:    ',(levs(k,n),k=l,m)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid)
          ENDDO
        ELSE
C-      Check for levels out of range ( > kdiag)
          kLev = 0
          DO k=1,nlevels(n)
            kLev = MAX(NINT(levs(k,n)),kLev)
          ENDDO
          DO m=1,nfields(n)
            mm = jdiag(m,n)
            IF ( kLev.GT.kdiag(mm) ) THEN
C- Note: diagnostics_out take care (in some way) of this case 
C        so that it does not cause "index out-off bounds" error.
C        However, the output file looks strange.
C- For now, choose to stop, but could change it to just a warning
             WRITE(msgBuf,'(A,I3,A,I3,2A)')
     &       'DIAGNOSTICS_SET_POINTERS: Ask for level=',kLev,
     &         ' in list n=', n, ', filename: ', fnames(n)
             CALL PRINT_ERROR( msgBuf , myThid )
             WRITE(msgBuf,'(2A,I3,A,I3,2A)')
     &       'DIAGNOSTICS_SET_POINTERS: ==> exceed Max.Nb of lev.',
     &       '(=',kdiag(mm),') for Diag. #', mm, ' : ',cdiag(mm)
             CALL PRINT_ERROR( msgBuf , myThid )
             WRITE(msgBuf,'(4A)') 'DIAGNOSTICS_SET_POINTERS: ',
     &       ' parsing code >>',gdiag(mm),'<<'
             CALL PRINT_ERROR( msgBuf , myThid )
             STOP 'ABNORMAL END: S/R DIAGNOSTICS_SET_POINTERS'
            ENDIF
          ENDDO
        ENDIF
      ENDDO

        WRITE(msgBuf,'(A)') 'DIAGNOSTICS_SET_POINTERS: done'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)
        WRITE(msgBuf,'(2A)')
     &   '------------------------------------------------------------'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)

      _END_MASTER( myThid )

      RETURN
      END
