C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_set_pointers.F,v 1.7 2006/11/19 21:59:56 jmc Exp $
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
      INTEGER ndiagcount, ndCount
      INTEGER md,ld,nd
      INTEGER mate, nActiveMax
      INTEGER i, j, k, k1, k2, mm, kLev
      LOGICAL found
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*3 mate_index


      _BEGIN_MASTER( myThid)

C--   Initialize pointer arrays to zero:
      DO ld=1,numlists
       DO md=1,numperlist
        idiag(md,ld) = 0
        jdiag(md,ld) = 0
        mdiag(md,ld) = 0
       ENDDO
      ENDDO

C--   Calculate pointers for diagnostics in active output-stream
C                                   (i.e., with defined filename)

      ndiagcount = 0
      nActiveMax = 0
      DO ld=1,nlists
       nActive(ld) = nfields(ld)
       DO md=1,nfields(ld)

         found = .FALSE.
C        Search all possible model diagnostics
         DO nd=1,ndiagt
          IF ( flds(md,ld).EQ.cdiag(nd) ) THEN
            CALL DIAGNOSTICS_SETDIAG(mate,ndiagcount,md,ld,nd,myThid)
            found = .TRUE.
            jdiag(md,ld) = nd
          ENDIF
         ENDDO
         IF ( .NOT.found ) THEN
           CALL DIAGNOSTICS_LIST_CHECK(
     O                      ndCount,
     I                      ld, md, nfields, flds, myThid )
           IF ( ndCount.EQ.0 ) THEN
             WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_SET_POINTERS: ',
     &                      flds(md,ld),' is not a Diagnostic'
             CALL PRINT_ERROR( msgBuf , myThid )
           ENDIF
           STOP 'ABNORMAL END: S/R DIAGNOSTICS_SET_POINTERS'
         ENDIF
         IF ( found .AND. mate.GE.1 ) THEN
           mm = nActive(ld) + 1
           IF ( mm.LE.numperlist ) THEN
             jdiag(mm,ld) = mate
             idiag(mm,ld) = mdiag(md,ld)
             flds (mm,ld) = cdiag(mate)
           ENDIF
           nActive(ld) = mm
         ENDIF

       ENDDO
       nActiveMax = MAX(nActive(ld),nActiveMax)
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
C--   Set pointer for mate (e.g.vector component mate) if not already done
C     and if it exists. Note: for now, only used to print message.
      DO ld=1,nlists
       DO md=1,nActive(ld)
        IF (mdiag(md,ld).EQ.0 ) THEN

         nd = jdiag(md,ld)
         mate_index = gdiag(nd)(6:8)
         IF ( mate_index.NE.'   ' ) THEN
          READ(mate_index,'(I3)') mate
          DO j=1,nlists
           DO i=1,nActive(j)
            IF ( mdiag(md,ld).EQ.0 .AND. jdiag(i,j).EQ.mate ) THEN
              mdiag(md,ld) = ABS(idiag(i,j))
            ENDIF
           ENDDO
          ENDDO
         ENDIF
         IF ( mdiag(md,ld).NE.0 ) THEN
          WRITE(msgBuf,'(A,I4,4A)') '  set mate pointer for diag #',
     &         nd, '  ', cdiag(nd), ' , Parms: ', gdiag(nd)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT , myThid)
         ENDIF

        ENDIF
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Set list of levels to write (if not specified in data.diagnostics)

      DO ld=1,nlists
        IF ( nlevels(ld).EQ.-1 ) THEN
C-      set Nb of levels to the minimum size of all diag of this list:
          kLev = numLevels
          DO md=1,nfields(ld)
            nd = jdiag(md,ld)
            kLev = MIN(kdiag(nd),kLev)
          ENDDO
          IF ( kLev.LE.0 ) THEN
            WRITE(msgBuf,'(2A,I4,2A)')
     &      'DIAGNOSTICS_SET_POINTERS: kLev < 1 in ',
     &      ' setting levs of list l=',ld,', fnames: ', fnames(ld)
            CALL PRINT_ERROR( msgBuf , myThid )
            STOP 'ABNORMAL END: S/R DIAGNOSTICS_SET_POINTERS'
          ENDIF
          nlevels(ld) = kLev
          DO k=1,kLev
           levs(k,ld) = k
          ENDDO
          WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_SET_POINTERS: ',
     &      'Set levels for Outp.Stream: ',fnames(ld)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid)
          DO k1=1,nlevels(ld),20
            k2 = MIN(nlevels(ld),k1+19)
            WRITE(msgBuf,'(A,20F5.0)')
     &         ' Levels:    ', (levs(k,ld),k=k1,k2)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid)
          ENDDO
        ELSE
C-      Check for levels out of range ( > kdiag)
          kLev = 0
          DO k=1,nlevels(ld)
            kLev = MAX(NINT(levs(k,ld)),kLev)
          ENDDO
          DO md=1,nfields(ld)
            nd = jdiag(md,ld)
            IF ( kLev.GT.kdiag(nd) ) THEN
C- Note: diagnostics_out take care (in some way) of this case
C        so that it does not cause "index out-off bounds" error.
C        However, the output file looks strange.
C- For now, choose to stop, but could change it to just a warning
             WRITE(msgBuf,'(A,I3,A,I3,2A)')
     &       'DIAGNOSTICS_SET_POINTERS: Ask for level=',kLev,
     &         ' in list l=', ld, ', filename: ', fnames(ld)
             CALL PRINT_ERROR( msgBuf , myThid )
             WRITE(msgBuf,'(2A,I3,A,I3,2A)')
     &       'DIAGNOSTICS_SET_POINTERS: ==> exceed Max.Nb of lev.',
     &       '(=',kdiag(nd),') for Diag. #', nd, ' : ',cdiag(nd)
             CALL PRINT_ERROR( msgBuf , myThid )
             WRITE(msgBuf,'(4A)') 'DIAGNOSTICS_SET_POINTERS: ',
     &       ' parsing code >>',gdiag(nd),'<<'
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
