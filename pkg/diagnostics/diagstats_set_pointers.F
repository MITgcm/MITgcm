C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagstats_set_pointers.F,v 1.7 2008/10/30 18:52:54 dfer Exp $
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
#include "DIAGSTATS_REGIONS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER ndiagcount, ndCount
      INTEGER md,ld,nd
      INTEGER mm, mate, nActiveMax
      INTEGER j, k, l
      LOGICAL found, addMate2List, inList, regListPb
      CHARACTER*(MAX_LEN_MBUF) msgBuf


      _BEGIN_MASTER( myThid)

C--   Initialize pointer arrays to zero:
      DO ld=1,numlists
       DO md=1,numperlist
        iSdiag(md,ld) = 0
        jSdiag(md,ld) = 0
        mSdiag(md,ld) = 0
       ENDDO
      ENDDO

C--   Calculate pointers for diagnostics set to non-zero frequency

      ndiagcount = 0
      nActiveMax = 0
      DO ld=1,diagSt_nbLists
       diagSt_nbActv(ld) = diagSt_nbFlds(ld)
       DO md=1,diagSt_nbFlds(ld)

         found = .FALSE.
C        Search all possible model diagnostics
         DO nd=1,ndiagt
          IF ( diagSt_Flds(md,ld).EQ.cdiag(nd) ) THEN
            CALL DIAGSTATS_SETDIAG(mate,ndiagcount,md,ld,nd,myThid)
            found = .TRUE.
            jSdiag(md,ld) = nd
          ENDIF
         ENDDO
         IF ( .NOT.found ) THEN
           CALL DIAGNOSTICS_LIST_CHECK(
     O                      ndCount,
     I                      ld,md, diagSt_nbLists,
     I                      diagSt_nbFlds,diagSt_Flds,myThid)
           IF ( ndCount.EQ.0 ) THEN
             WRITE(msgBuf,'(3A)') 'DIAGSTATS_SET_POINTERS: ',
     &                     diagSt_Flds(md,ld),' is not a Diagnostic'
             CALL PRINT_ERROR( msgBuf , myThid )
           ENDIF
           STOP 'ABNORMAL END: S/R DIAGSTATS_SET_POINTERS'
         ENDIF
         IF ( found .AND. mate.LE.-1 ) THEN
C-       add this fields to the active list in case regions are differents:
           addMate2List = .FALSE.
           DO l=1,ld-1
            inList = .FALSE.
            DO k=1,diagSt_nbActv(l)
              IF ( diagSt_Flds(k,l).EQ.cdiag(-mate) ) inList=.TRUE.
            ENDDO
            IF ( inList ) THEN
             DO j=0,nRegions
              addMate2List = addMate2List
     &                 .OR. (diagSt_region(j,l).LT.diagSt_region(j,ld))
             ENDDO
            ENDIF
           ENDDO
           IF ( .NOT.addMate2List ) mate = 0
         ENDIF
         IF ( found .AND. mate.NE.0 ) THEN
            mm = diagSt_nbActv(ld) + 1
            IF ( mm.LE.numperlist ) THEN
             iSdiag(mm,ld) = SIGN(mSdiag(md,ld),mate)
             mate = ABS(mate)
             jSdiag(mm,ld) = mate
             diagSt_Flds(mm,ld) = cdiag(mate)
            ENDIF
            diagSt_nbActv(ld) = mm
         ENDIF

       ENDDO
       nActiveMax = MAX(diagSt_nbActv(ld),nActiveMax)
      ENDDO

      IF (  ndiagcount.LE.diagSt_size .AND.
     &      nActiveMax.LE.numperlist ) THEN
        WRITE(msgBuf,'(A,I8,A)')
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
         WRITE(msgBuf,'(A,I8,A,I8)')
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
C-    check that all selected regions are actually defined
      regListPb = .FALSE.
      DO l=1,diagSt_nbLists
       DO j=1,nRegions
        IF ( diagSt_region(j,l).NE.0 ) THEN
          IF ( diagSt_kRegMsk(j).LT.1 .OR.
     &         diagSt_kRegMsk(j).GT.nSetRegMask ) THEN
            WRITE(msgBuf,'(A,3(A,I5))') 'DIAGSTATS_SET_POINTERS:',
     &       ' region', j, ' undefined (k=', diagSt_kRegMsk(j),
     &       ') in list l=', l
            CALL PRINT_ERROR( msgBuf , myThid )
            regListPb = .TRUE.
          ENDIF
        ENDIF
       ENDDO
      ENDDO
      IF ( regListPb ) THEN
        WRITE(msgBuf,'(2A)') 'DIAGSTATS_SET_POINTERS:',
     &                       ' Cannot select undefined regions'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R DIAGSTATS_SET_POINTERS'
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

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
