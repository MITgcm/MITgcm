C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/Attic/diagnostics_add2list.F,v 1.4 2005/02/13 23:22:56 jmc Exp $
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
C     routine to add 1 diagnostics to the list of available diagnostics:
C     set the attributes:
C     name (=cdiag), parsing code (=gdiag), units (=udiag), and title (=tdiag)
C     of the new diagnostics and update the total number of available diagnostic
C     Note: needs to be called after DIAGNOSTICS_INIT_EARLY 
C                         and before DIAGNOSTICS_INIT_FIXED
      
C     !USES:
       IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(*) diagTitle
      INTEGER myThid

C     !OUTPUT PARAMETERS:
C     numDiag :: diagnostic number in the list of available diagnostics
      INTEGER diagNum
CEOP

C     !LOCAL VARIABLES:
C     msgBuf :: Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER n


C--   Search for "diagName" in the list of available diagnostics:
      diagNum = 0
      DO n=1,ndiagt
       IF ( cdiag(n).EQ.diagName ) THEN
         diagNum = n
        _BEGIN_MASTER( myThid)
         IF ( gdiag(n).EQ.diagCode ) THEN
C-    diagnostics is already defined and has the same characteristics
          WRITE(msgBuf,'(3A,I4,A)') 'DIAGNOSTICS_ADD2LIST: diag=',
     &     diagName,' is already defined (# ',n,' )'
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT , myThid)
          WRITE(msgBuf,'(2A)') 'DIAGNOSTICS_ADD2LIST:',
     &     ' with same parser => update Title & Units '
          CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT , myThid)
          udiag(diagNum) = diagUnits
          tdiag(diagNum) = diagTitle
         ELSE
C-    diagnostics is already defined but with different characteristics
          WRITE(msgBuf,'(3A,I4,A)') 'DIAGNOSTICS_ADD2LIST: diag=',
     &     diagName,' is already defined (# ',n,' )'
          CALL PRINT_ERROR( msgBuf , myThid)
          WRITE(msgBuf,'(4A)') 'DIAGNOSTICS_ADD2LIST:',
     &     ' parser=',gdiag(n),'< cannot be changed'
          CALL PRINT_ERROR( msgBuf , myThid)
          WRITE(msgBuf,'(4A)') 'DIAGNOSTICS_ADD2LIST:',
     &     '  to  ->',diagCode,'< ; => STOP'
          CALL PRINT_ERROR( msgBuf , myThid)
          STOP 'ABNORMAL END: S/R DIAGNOSTICS_ADD2LIST'
         ENDIF
        _END_MASTER( myThid )
       ENDIF
      ENDDO
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( diagNum.EQ.0 ) THEN
C--   Add one diagnostic to the list of available diagnostics:
       diagNum = ndiagt + 1

      _BEGIN_MASTER( myThid)

       IF ( diagNum .LE. ndiagMax ) THEN
        cdiag(diagNum) = diagName
        gdiag(diagNum) = diagCode
        udiag(diagNum) = diagUnits
        tdiag(diagNum) = diagTitle
        ndiagt = diagNum
       ELSE
        WRITE(msgBuf,'(2A,I4)') 'DIAGNOSTICS_ADD2LIST:',
     &   ' Exceed Max.Number of diagnostics ndiagMax=', ndiagMax 
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(2A)')
     &   'DIAGNOSTICS_ADD2LIST: when setting diagnostic: ',diagName
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R DIAGNOSTICS_ADD2LIST'
       ENDIF
      
      _END_MASTER( myThid )

      ENDIF

      RETURN 
      END
