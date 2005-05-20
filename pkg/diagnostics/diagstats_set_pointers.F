C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagstats_set_pointers.F,v 1.1 2005/05/20 07:28:52 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

CBOP
C     !ROUTINE: DIAGSTATS_SET_POINTERS
C     !INTERFACE:
      SUBROUTINE DIAGSTATS_SET_POINTERS( myThid )

C     !DESCRIPTION: \bv
C     *==================================================================
C     | S/R DIAGSTATS_SET_POINTERS
C     | o set pointers for active statistics diagnostics
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
      INTEGER i,j,l
      LOGICAL found, addMate2List, inList
      CHARACTER*(MAX_LEN_MBUF) msgBuf


      _BEGIN_MASTER( myThid)

C--   Initialize pointer arrays to zero:
      DO n=1,ndiagMax
        iSdiag(n) = 0
      ENDDO

C--   Calculate pointers for diagnostics set to non-zero frequency

      ndiagcount = 0
      nActiveMax = 0
      DO n=1,diagSt_nbLists
       diagSt_nbActv(n) = diagSt_nbFlds(n)
       DO m=1,diagSt_nbFlds(n)

         found = .FALSE.
C        Search all possible model diagnostics
         DO mm=1,ndiagt
          IF ( diagSt_Flds(m,n).EQ.cdiag(mm) ) THEN
            CALL DIAGSTATS_SETDIAG (mate,ndiagcount,mm,myThid)
            found = .TRUE.
            jSdiag(m,n) = mm
          ENDIF
         ENDDO
         IF ( .NOT.found ) THEN
           WRITE(msgBuf,'(3A)') 'DIAGSTATS_SET_POINTERS: ', 
     &                      diagSt_Flds(m,n),' is not a Diagnostic'
           CALL PRINT_ERROR( msgBuf , myThid )
           STOP 'ABNORMAL END: S/R DIAGSTATS_SET_POINTERS'
         ENDIF
         IF ( found .AND. mate.LE.-1 ) THEN
C-       add this fields to the active list in case regions are differents:
           mate = -mate
           addMate2List = .FALSE.
           DO l=1,n-1
            inList = .FALSE.
            DO i=1,diagSt_nbActv(l)
              IF ( diagSt_Flds(i,l).EQ.cdiag(mate) ) inList=.TRUE.
            ENDDO
            IF ( inList ) THEN
             DO j=0,nRegions
              addMate2List = addMate2List
     &                 .OR. (diagSt_region(j,l).LT.diagSt_region(j,n))
             ENDDO
            ENDIF
           ENDDO
           IF ( .NOT.addMate2List ) mate = 0
         ENDIF
         IF ( found .AND. mate.GE.1 ) THEN
            diagSt_nbActv(n) = diagSt_nbActv(n) + 1
            IF ( diagSt_nbActv(n).LE.numperlist ) THEN
             jSdiag( diagSt_nbActv(n), n ) = mate
             diagSt_Flds( diagSt_nbActv(n), n ) = cdiag(mate)
           ENDIF
         ENDIF

       ENDDO
       nActiveMax = MAX(diagSt_nbActv(n),nActiveMax)
      ENDDO

      IF (  ndiagcount.LE.diagSt_size .AND.
     &      nActiveMax.LE.numperlist ) THEN
        WRITE(msgBuf,'(A,I6,A)')
     &    '  space allocated for all stats-diags:', 
     &    ndiagcount, ' levels'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)
      ELSE
       IF ( ndiagcount.GT.diagSt_size ) THEN
         WRITE(msgBuf,'(2A)')
     &    'DIAGSTATS_SET_POINTERS: Not enough space',
     &    ' for all active stats-diags (from data.diagnostics)'
         CALL PRINT_ERROR( msgBuf , myThid )
         WRITE(msgBuf,'(A,I6,A,I6)')
     &    'DIAGSTATS_SET_POINTERS: diagSt_size=', diagSt_size,
     &    ' but needs at least', ndiagcount
         CALL PRINT_ERROR( msgBuf , myThid )
       ENDIF
       IF ( nActiveMax.GT.numperlist ) THEN
         WRITE(msgBuf,'(2A)')
     &    'DIAGSTATS_SET_POINTERS: Not enough space',
     &    ' for all active stats-diags (from data.diagnostics)'
         CALL PRINT_ERROR( msgBuf , myThid )
         WRITE(msgBuf,'(A,I6,A,I6)')
     &    'DIAGSTATS_SET_POINTERS: numperlist=', numperlist,
     &    ' but needs at least', nActiveMax
         CALL PRINT_ERROR( msgBuf , myThid )
       ENDIF
       STOP 'ABNORMAL END: S/R DIAGSTATS_SET_POINTERS'
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Set list of regions to write
C-    Need to check that all selected regions are actually defined

        WRITE(msgBuf,'(A)') 'DIAGSTATS_SET_POINTERS: done'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)
        WRITE(msgBuf,'(2A)')
     &   '------------------------------------------------------------'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)

      _END_MASTER( myThid )

      RETURN
      END
