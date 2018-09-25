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
C     myThid :: my Thread Id. number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER ndiagcount, ndCount
      INTEGER md,ld,nd
      INTEGER mate, nActiveMax
      INTEGER i, j, k, k1, k2, kLev
      LOGICAL found
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*12 suffix

      _BEGIN_MASTER( myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   In case an output file contains 2 post-processed diags which are computed
C     together (mate of 2nd PP-diag one is 1rst PP-diag), move these 2 diags
C     next to each other (to only computate them once): 1rst one then 2nd one.
      DO ld=1,nlists
       found = .FALSE.
       DO md=1,nfields(ld)
C        Search all possible model diagnostics
         nd = 0
         DO i=1,ndiagt
          IF ( nd.EQ.0 .AND. flds(md,ld).EQ.cdiag(i) ) nd = i
         ENDDO
         j  = 0
         IF ( nd.GE.1 ) THEN
           IF ( gdiag(nd)(5:5).EQ.'P' ) THEN
             mate = hdiag(nd)
             IF ( gdiag(mate)(5:5).EQ.'P' ) THEN
C        Mate of Post-Processed diag "nd" is also Post-Processed
               DO i=1,nfields(ld)
                 IF ( j.EQ.0 .AND. flds(i,ld).EQ.cdiag(mate) ) j = i
               ENDDO
             ENDIF
           ENDIF
         ENDIF
C        And is found in the same output stream "ld" (at rank "j")
         IF ( j.GE.1 .AND. j.NE.md-1 ) THEN
           IF ( .NOT.found ) THEN
             WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_SET_POINTERS: ',
     &             'Re-Order Diags in Outp.Stream: ',fnames(ld)
             CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                           SQUEEZE_RIGHT, myThid )
           ENDIF
           found  = .TRUE.
           IF ( j.LT.md-1 ) THEN
             WRITE(msgBuf,'(2A,2(A,I4),2A)')
     &         ' move ',flds(j,ld),' from ',j,' down to',md-1,
     &         ' just before ',flds(md,ld)
             CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                           SQUEEZE_RIGHT, myThid )
             DO i=j,md-2
               flds(i,ld) = flds(i+1,ld)
             ENDDO
             flds(md-1,ld) = cdiag(mate)
           ELSEIF ( j.GT.md ) THEN
             WRITE(msgBuf,'(2A,2(A,I4),2A)')
     &         ' move ',flds(j,ld),' from ',j,'  up to ',md,
     &         ' just before ',flds(md,ld)
             CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                           SQUEEZE_RIGHT, myThid )
             DO i=j,md+1,-1
               flds(i,ld) = flds(i-1,ld)
             ENDDO
             flds(md,ld) = cdiag(mate)
           ENDIF
         ENDIF
       ENDDO
       IF ( found ) THEN
         WRITE(msgBuf,'(2A,I4,A)') 'DIAGNOSTICS_SET_POINTERS: ',
     &             'Updated list in Outp.Stream #', ld, ' :'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         DO md = 1,nfields(ld),10
           j = MIN(nfields(ld),md+9)
           WRITE(msgBuf,'(21A)') ' Fields:   ',(' ',flds(i,ld),i=md,j)
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                         SQUEEZE_RIGHT, myThid )
         ENDDO
       ENDIF
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Initialize pointer arrays to zero:
      DO ld=1,numLists
       DO md=1,numperList
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
          ENDIF
         ENDDO
         IF ( .NOT.found ) THEN
           CALL DIAGNOSTICS_LIST_CHECK(
     O                      ndCount,
     I                      ld, md, nlists, nfields, flds, myThid )
           IF ( ndCount.EQ.0 ) THEN
             WRITE(msgBuf,'(3A)') 'DIAGNOSTICS_SET_POINTERS: ',
     &                      flds(md,ld),' is not a Diagnostic'
             CALL PRINT_ERROR( msgBuf , myThid )
           ENDIF
           STOP 'ABNORMAL END: S/R DIAGNOSTICS_SET_POINTERS'
         ENDIF

       ENDDO
       nActiveMax = MAX(nActive(ld),nActiveMax)
      ENDDO

      IF (  ndiagcount.LE.numDiags .AND.
     &      nActiveMax.LE.numperList ) THEN
        WRITE(msgBuf,'(A,I8,A)')
     &    '  space allocated for all diagnostics:',
     &    ndiagcount, ' levels'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ELSE
       IF ( ndiagcount.GT.numDiags ) THEN
         WRITE(msgBuf,'(2A)')
     &    'DIAGNOSTICS_SET_POINTERS: Not enough space',
     &    ' for all active diagnostics (from data.diagnostics)'
         CALL PRINT_ERROR( msgBuf , myThid )
         WRITE(msgBuf,'(A,I8,A,I8)')
     &    'DIAGNOSTICS_SET_POINTERS: numDiags=', numDiags,
     &    ' but needs at least', ndiagcount
         CALL PRINT_ERROR( msgBuf , myThid )
       ENDIF
       IF ( nActiveMax.GT.numperList ) THEN
         WRITE(msgBuf,'(2A)')
     &    'DIAGNOSTICS_SET_POINTERS: Not enough space',
     &    ' for all active diagnostics (from data.diagnostics)'
         CALL PRINT_ERROR( msgBuf , myThid )
         WRITE(msgBuf,'(A,I6,A,I6)')
     &    'DIAGNOSTICS_SET_POINTERS: numperList=', numperList,
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

         k = SIGN(1,jdiag(md,ld))
         nd = ABS(jdiag(md,ld))
         mate = hdiag(nd)
         IF ( mate.GT.0 ) THEN
          DO j=1,nlists
           DO i=1,nActive(j)
            IF ( mdiag(md,ld).EQ.0 .AND. (k*jdiag(i,j)).EQ.mate ) THEN
             IF ( freq(j).EQ.freq(ld) .AND. phase(j).EQ.phase(ld)
     &           .AND. averageFreq(j) .EQ.averageFreq(ld)
     &           .AND. averagePhase(j).EQ.averagePhase(ld)
     &           .AND. averageCycle(j).EQ.averageCycle(ld) )
     &          mdiag(md,ld) = ABS(idiag(i,j))
            ENDIF
           ENDDO
          ENDDO
         ENDIF
         IF ( mdiag(md,ld).NE.0 ) THEN
          WRITE(msgBuf,'(A,I6,5A,I6)') '  set mate pointer for diag #',
     &         nd, '  ', cdiag(nd), ' , Parms: ', gdiag(nd)(1:10),
     &             ' , mate:', hdiag(nd)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
         ENDIF

        ENDIF
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Set internal parameter "useDiag4AdjOutp" if Adj Diagnostics are found
      DO ld=1,nlists
       DO md=1,nfields(ld)
c      DO md=1,nActive(ld)
         nd = ABS(jdiag(md,ld))
         useDiag4AdjOutp = useDiag4AdjOutp
     &                .OR. ( gdiag(nd)(4:4).EQ.'A' )
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Set list of levels to write (if not specified in data.diagnostics)

      DO ld=1,nlists
        IF ( nlevels(ld).EQ.-1 ) THEN
C-      set Nb of levels to the minimum size of all diag of this list:
          kLev = numLevels*10
          DO md=1,nfields(ld)
            nd = ABS(jdiag(md,ld))
            kLev = MIN(kdiag(nd),kLev)
          ENDDO
          IF ( kLev.LE.0 ) THEN
            WRITE(msgBuf,'(2A,I4,2A)')
     &      'DIAGNOSTICS_SET_POINTERS: kLev < 1 in',
     &      ' setting levs of list l=',ld,', fnames=', fnames(ld)
            CALL PRINT_ERROR( msgBuf , myThid )
            STOP 'ABNORMAL END: S/R DIAGNOSTICS_SET_POINTERS'
          ELSEIF ( kLev.GT.numLevels ) THEN
            WRITE(msgBuf,'(A,2(I6,A))')
     &      'DIAGNOSTICS_SET_POINTERS: kLev=', kLev,
     &                  ' >', numLevels, ' =numLevels'
            CALL PRINT_ERROR( msgBuf , myThid )
            WRITE(msgBuf,'(2A,I4,2A)') 'DIAGNOSTICS_SET_POINTERS: in',
     &      ' setting levs of list l=',ld,', fnames=', fnames(ld)
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
          suffix = ' Levels:    '
          IF ( fflags(ld)(2:2).EQ.'I' ) suffix = ' Sum Levels:'
          DO k1=1,nlevels(ld),20
            k2 = MIN(nlevels(ld),k1+19)
            WRITE(msgBuf,'(A,20F5.0)') suffix, (levs(k,ld),k=k1,k2)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid)
          ENDDO
        ELSEIF ( fflags(ld)(2:2).NE.'P' ) THEN
C-      if no Vert.Interpolation, check for levels out of range ( > kdiag):
          kLev = 0
          DO k=1,nlevels(ld)
            kLev = MAX(NINT(levs(k,ld)),kLev)
          ENDDO
          DO md=1,nfields(ld)
            nd = ABS(jdiag(md,ld))
            IF ( kLev.GT.kdiag(nd) ) THEN
C- Note: diagnostics_out take care (in some way) of this case
C        so that it does not cause "index out-off bounds" error.
C        However, the output file looks strange.
C- For now, choose to stop, but could change it to just a warning
             WRITE(msgBuf,'(A,I4,A,I6,2A)')
     &       'DIAGNOSTICS_SET_POINTERS: Ask for level=',kLev,
     &         ' in list l=', ld, ', filename: ', fnames(ld)
             CALL PRINT_ERROR( msgBuf , myThid )
             WRITE(msgBuf,'(2A,I4,A,I6,2A)')
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
