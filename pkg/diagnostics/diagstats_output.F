C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagstats_output.F,v 1.11 2011/06/06 15:42:58 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGSTATS_OUTPUT

C     !INTERFACE:
      SUBROUTINE DIAGSTATS_OUTPUT(
     I     listId,
     I     myTime, myIter, myThid )

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
      _RL     myTime
      INTEGER listId, myIter, myThid
CEOP

C     !FUNCTIONS:
c     INTEGER ILNBLNK
c     EXTERNAL ILNBLNK
#ifdef ALLOW_FIZHI
      _RL   getcon
      EXTERNAL getcon
#endif

C     !LOCAL VARIABLES:
      INTEGER j, m, ndId, iSp, iSm
      CHARACTER*10 gcode
      INTEGER mate
      _RL statGlob(0:nStats,0:nLev,0:nRegions)
      _RL tmp_Glob(0:nStats,0:nLev)
      _RL undef

      INTEGER ioUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      ioUnit= standardMessageUnit
      undef = UNSET_RL
#ifdef ALLOW_FIZHI
      IF ( useFIZHI ) undef = getcon('UNDEF')
#endif

      DO m = 1,diagSt_nbFlds(listId)
       ndId = jSdiag(m,listId)
       gcode = gdiag(ndId)(1:10)
       IF ( iSdiag(m,listId).NE.0 .AND. gcode(5:5).NE.'D' ) THEN
C--    Start processing 1 Fld :

         IF ( gcode(5:5).EQ.'C' ) THEN
C          Check for Mate of a Counter Diagnostic
C          --------------------------------------
           mate = hdiag(ndId)
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
            IF ( tmp_Glob(0,0).EQ.0. ) THEN
             _BEGIN_MASTER( myThid )
             WRITE(msgBuf,'(A,I10,A,I4)')
     &        '- WARNING - from DIAGSTATS_OUTPUT at iter=', myIter,
     &        ' , region:', j
             CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                           SQUEEZE_RIGHT, myThid)
             WRITE(msgBuf,'(A,I6,3A,I4,2A)')
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

            IF ( debugLevel .GE. debLevB ) THEN
             _BEGIN_MASTER( myThid )
              WRITE(ioUnit,'(A,I6,3A,I4,A,1PE10.3,2A)')
     &         ' Compute Stats, Diag. # ',ndId, '  ', cdiag(ndId),
     &         '  vol(',j,' ):', statGlob(0,0,j),'  Parms: ',gdiag(ndId)
             IF ( mate.GT.0 ) THEN
              WRITE(ioUnit,'(A,I6,3A,I4,2(A,1PE10.3))')
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

#ifdef ALLOW_MNC
         IF (diagSt_mnc) THEN
           CALL DIAGSTATS_MNC_OUT(
     &          statGlob, nLev, ndId,
     &          m, listId, myTime, myIter, myThid )
         ENDIF
#endif

C--    end of Processing Fld # m
       ENDIF
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
