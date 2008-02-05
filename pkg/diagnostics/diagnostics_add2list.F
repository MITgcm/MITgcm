C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/Attic/diagnostics_add2list.F,v 1.6 2008/02/05 15:13:01 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_ADD2LIST

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_ADD2LIST (
     O     diagNum,
     I     diagName, diagCode, diagUnits, diagTitle,
     I     myThid )

C     !DESCRIPTION:
C     old version of this subroutine (new version= DIAGNOSTICS_ADDTOLIST)
C     the diagnostic-mate number, which is missing from the argument list,
C     is read from the parsing code before calling DIAGNOSTICS_ADDTOLIST ;
C     (see description of DIAGNOSTICS_ADDTOLIST for more details).
C     Note: needs to be called after DIAGNOSTICS_INIT_EARLY
C                         and before DIAGNOSTICS_INIT_FIXED

C     !USES:
       IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
C     diagName  :: diagnostic name to declare
C     diagCode  :: parser code for this diagnostic
C     diagUnits :: field units for this diagnostic
C     diagTitle :: field description for this diagnostic
C     myThid    :: my Thread Id number
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(*) diagTitle
      INTEGER myThid

C     !OUTPUT PARAMETERS:
C     numDiag   :: diagnostic number in the list of available diagnostics
      INTEGER diagNum
CEOP

C     !LOCAL VARIABLES:
C     msgBuf    :: Informational/error meesage buffer
C     diagMate  :: diagnostic mate number
C     errIO     :: IO error flag
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER diagMate
      INTEGER errIO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( diagCode(6:8) .EQ. '   ' ) THEN
        diagMate = 0
      ELSEIF ( diagCode(5:5).EQ.'C' ) THEN
        READ(diagCode(6:8),'(I3)',IOSTAT=errIO) diagMate
        IF ( errIO.NE.0 ) THEN
          WRITE(msgBuf,'(6A)') 'DIAGNOSTICS_ADD2LIST: ',
     &     'adding diagnostic: "',diagName,
     &     '" with diagcode="',diagCode,'"'
          CALL PRINT_ERROR( msgBuf , myThid)
          WRITE(msgBuf,'(A,I6,A)') 'DIAGNOSTICS_ADD2LIST: Error (=',
     &      errIO,') reading mate number from diagcode => STOP'
          CALL PRINT_ERROR( msgBuf , myThid)
          STOP 'ABNORMAL END: S/R DIAGNOSTICS_ADD2LIST'
        ENDIF
      ELSE
        READ(diagCode(6:8),'(I3)',IOSTAT=errIO) diagMate
        IF ( errIO.NE.0 ) THEN
          WRITE(msgBuf,'(4A)') 'WARNING: DIAGNOSTICS_ADD2LIST: ',
     &    'mate number not readable from diagCode="',diagCode,'"'
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT , myThid)
          WRITE(msgBuf,'(4A)') 'WARNING: DIAGNOSTICS_ADD2LIST: ',
     &    'mate number of diag="',diagName,'" is reset to zero'
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT , myThid)
          diagMate = 0
        ENDIF
      ENDIF

      CALL DIAGNOSTICS_ADDTOLIST (
     O     diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate,
     I     myThid )

      RETURN
      END
