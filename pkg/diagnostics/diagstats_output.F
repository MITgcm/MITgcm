C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagstats_output.F,v 1.4 2005/07/06 13:52:31 edhill Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGSTATS_OUTPUT

C     !INTERFACE:
      SUBROUTINE  DIAGSTATS_OUTPUT(
     I     listId,
     I     myIter, myTime, myThid )

C     !DESCRIPTION:
C     Write output for diagnostics fields.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

      INTEGER nLev
#ifdef ALLOW_FIZHI
#include "fizhi_SIZE.h"
      PARAMETER (nLev = Nr+Nrphys)
#else
      PARAMETER (nLev = Nr)
#endif

C     !INPUT PARAMETERS:
C     listId  :: Diagnostics list number being written
C     myIter  :: current iteration number
C     myTime  :: Current time of simulation (s)
C     myThid  :: my Thread Id number
      INTEGER listId, myIter, myTime, myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER j, m, ndId, iSp, iSm
      CHARACTER*8 parms1
      CHARACTER*3 mate_index
      INTEGER mate
      _RL statGlob(0:nStats,0:nLev,0:nRegions)
      _RL tmp_Glob(0:nStats,0:nLev)
      _RL undef, getcon
      EXTERNAL getcon
c     INTEGER ILNBLNK
c     EXTERNAL ILNBLNK

      INTEGER ioUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      ioUnit= standardMessageUnit
      undef = getcon('UNDEF')

      DO m = 1,diagSt_nbFlds(listId)
       ndId = jSdiag(m,listId)
       parms1 = gdiag(ndId)(1:8)
       IF ( iSdiag(m,listId).NE.0 .AND. parms1(5:5).NE.'D' ) THEN
C--    Start processing 1 Fld :

         IF ( parms1(5:5).EQ.'C' ) THEN
C          Check for Mate of a Counter Diagnostic
C          --------------------------------------
           mate_index = parms1(6:8)
           READ (mate_index,'(I3)') mate
         ELSE
           mate = 0
         ENDIF

         DO j=0,nRegions
          IF ( diagSt_region(j,listId).GT.0 ) THEN
            iSp = ABS(iSdiag(m,listId))
            iSm = mSdiag(m,listId)
            CALL DIAGSTATS_GLOBAL(
     O                       statGlob(0,0,j), tmp_Glob,
     I                       undef, nLev, j,
     I                       ndId, mate, iSp, iSm, myThid )

C-          Check for empty Diag (= not filled or using empty mask)
            IF ( statGlob(0,0,j).EQ.0. ) THEN
             _BEGIN_MASTER( myThid )
             WRITE(msgBuf,'(A,I10,A,I3)')
     &        '- WARNING - from DIAGSTATS_OUTPUT at iter=', myIter,
     &        ' , region:', j
             CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                           SQUEEZE_RIGHT, myThid)
             WRITE(msgBuf,'(A,I4,3A,I3,2A)')
     &       '- WARNING - diagSt.#',ndId, ' : ',diagSt_Flds(m,listId),
     &       ' (#',m,' ) in outp.Stream: ',diagSt_Fname(listId)
             CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                           SQUEEZE_RIGHT, myThid)
             WRITE(msgBuf,'(2A)') '- WARNING - has not been filled,',
     &                            ' OR using empty mask/region'
             CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                           SQUEEZE_RIGHT, myThid)
             WRITE(msgBuf,'(A)')
     &       'WARNING DIAGSTATS_OUTPUT  => write UNDEF instead'
             CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                           SQUEEZE_RIGHT, myThid)
             _END_MASTER( myThid )
            ENDIF

            IF ( debugLevel .GE. debLevA ) THEN
             _BEGIN_MASTER( myThid )
              WRITE(ioUnit,'(A,I3,3A,I3,A,1PE10.3,2A)')
     &         ' Compute Stats, Diag. # ',ndId, '  ', cdiag(ndId),
     &         '  vol(',j,' ):', statGlob(0,0,j),'  Parms: ',gdiag(ndId)
             IF ( mate.GT.0 ) THEN
              WRITE(ioUnit,'(A,I3,3A,I3,2(A,1PE10.3))')
     &         '    use Counter Mate  # ', mate,'  ',cdiag(mate),
     &         '  vol(',j,' ):',tmp_Glob(0,0), ' integral',tmp_Glob(1,0)
             ENDIF
             _END_MASTER( myThid )
            ENDIF
          ENDIF
         ENDDO

C--      Write to ASCII file:
         IF (diagSt_Ascii) THEN
           CALL DIAGSTATS_ASCII_OUT( statGlob, nLev, ndId,
     &                               m, listId, myIter, myThid )
         ENDIF

         IF (diagSt_mnc) THEN
           CALL DIAGSTATS_MNC_OUT( 
     &          statGlob, nLev, ndId,
     &          m, listId, myIter, myTime, myThid )
         ENDIF

C--    end of Processing Fld # m
       ENDIF
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
